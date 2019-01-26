#!/usr/bin/env python

import sys
import os
import shutil
import imp
import unittest
import tempfile
import subprocess

pj = os.path.join

HOMECTL_ROOT = os.path.realpath(os.path.dirname(sys.argv[0]))
HOMECTL_BIN = pj(HOMECTL_ROOT, 'homectl.hcpkg', 'python', 'homectl.py')
TESTDATA_DIR = pj(HOMECTL_ROOT, "testdata")

# Since the hc command isn't really intended to be used as a module, we need to
# fake it.  This is a glorified "import hc", if hc were in fact importable.
hc = imp.load_source('homectl', HOMECTL_BIN)



# Since git doesn't allow you to store empty directories in version control, we
# have to create them here, so they exist in our test data set.  (XXX Since a
# user's setup is stored in git, probably we should just make homectl ignore
# them entirely, but that's a project for another day.)
for d in [
    ("package-empty.hcpkg",),
    ("package-full.hcpkg", "Darwin", "lib"),
    ("package-full.hcpkg", "Linux", "lib"),
]:
    hc.mkdirp(pj(TESTDATA_DIR, *d))



def fileset(base, *paths):
    return set([(f, pj(base, f)) for f in paths])

def file_contents(path):
    with open(path, 'r') as f:
        return f.read()



class HomectlTest(unittest.TestCase):
    # Base class for other homectl tests; handles creating and tearing down a
    # homectl environment in the filesystem.

    # Useful packages in testdata
    EMPTY = pj(TESTDATA_DIR, 'package-empty.hcpkg')
    FULL = pj(TESTDATA_DIR, 'package-full.hcpkg')

    def setUp(self):
        self.dir = os.path.realpath(tempfile.mkdtemp(prefix='homectl-selftest-'))
        self.old_pwd = os.getcwd()
        self.old_env = dict(os.environ)
        os.environ.clear()
        os.environ.update({
            'SHELL': '/bin/sh',
            'PATH': '/usr/bin:/bin',
            'TERM': self.old_env['TERM'],
            'HOME': self.dir,
            'LOGNAME': self.old_env['LOGNAME'],
            'USER': self.old_env['USER'],
            'LANG': self.old_env['LANG'],
        })
        os.chdir(self.dir)

    def tearDown(self):
        shutil.rmtree(self.dir)
        os.environ.clear()
        os.environ.update(self.old_env)
        os.chdir(self.old_pwd)



class SilentSystem(hc.System):
    def __init__(self):
        super(SilentSystem, self).__init__(pretend=False)
        self.__log = []

    def log(self, msg):
        self.__log.append(msg)



def with_system(fn):
    def decorator(self):
        s = SilentSystem()
        fn(self, s)
    return decorator



def with_deployment(fn):
    def decorator(self):
        s = SilentSystem()
        d = hc.Deployment(s, os.getcwd())
        fn(self, d)
    return decorator



class UtilTest(HomectlTest):
    # Small tests for utility functions.

    DATA = pj(TESTDATA_DIR, 'util')

    def test_mkdirp(self):
        hc.mkdirp('') # should do nothing
        hc.mkdirp('.') # should do nothing
        hc.mkdirp('..') # should do nothing

        hc.mkdirp('foo')
        self.assertEqual(os.path.isdir('foo'), True)

        hc.mkdirp(pj('outer', 'inner'))
        self.assertEqual(os.path.isdir('outer'), True)
        self.assertEqual(os.path.isdir(pj('outer', 'inner')), True)

    def test_visible_dirs(self):
        self.assertEqual(
            set(hc.visible_dirs(self.DATA)),
            fileset(self.DATA,
                'visible-dir',
                'visible-link-to-dir',
            ))

    def test_visible_links(self):
        self.assertEqual(
            set(hc.visible_links(self.DATA)),
            fileset(self.DATA,
                'visible-link',
                'visible-link-to-dir',
            ))

    def test_fs_tree(self):
        self.assertEqual(
            set(hc.fs_tree(self.DATA)),
            fileset(self.DATA,
                ".invisible",
                ".invisible-dir",
                ".invisible-dir/foo",
                ".invisible-link",
                "visible",
                "visible-dir",
                "visible-dir/bar",
                "visible-dir/foo",
                "visible-link",
                "visible-link-to-dir",
            ))

    def test_fs_files_in(self):
        self.assertEqual(
            set(hc.fs_files_in(self.DATA)),
            fileset(self.DATA,
                ".invisible",
                ".invisible-dir/foo",
                ".invisible-link",
                "visible",
                "visible-dir/bar",
                "visible-dir/foo",
                "visible-link",
                "visible-link-to-dir",
            ))

    def test_sh_quote(self):
        for t, a in [
            ("foo", "foo"),
            ("foo\"", "foo\\\""),
            ("foo bar", "\"foo bar\""),
            ("foo\nbar", "foo\\nbar"),
            ("foo\rbar", "foo\\rbar"),
            ("foo\tbar", "foo\\tbar"),
            ("foo\0bar", "foo\\0bar"),
            ("foo\\bar", "foo\\\\bar"),
        ]:
            self.assertEqual(hc.sh_quote(t), a)



class PackageTest(HomectlTest):
    # Tests for the public Package class API.

    def test_systems(self):
        empty = hc.Package(self.EMPTY)
        p = hc.Package(self.FULL)
        self.assertEqual(set(empty.systems), set(['common']))
        self.assertEqual(set(p.systems),
                         set(['common', 'Linux', 'Darwin', 'Windows']))

    def test_hooks_in_system(self):
        empty = hc.Package(self.EMPTY)
        p = hc.Package(self.FULL)
        self.assertEqual(set(empty.hooks_in_system('common')), set())
        self.assertEqual(set(empty.hooks_in_system('Linux')), set())
        self.assertEqual(set(p.hooks_in_system('common')), set(['bin', 'share']))
        self.assertEqual(set(p.hooks_in_system('Linux')), set(['bin', 'lib']))
        self.assertEqual(set(p.hooks_in_system('Plan9')), set())

    def test_files_in_sys_hook(self):
        empty = hc.Package(self.EMPTY)
        p = hc.Package(self.FULL)
        self.assertEqual(set(empty.files_in_sys_hook('common', 'bin')), set())
        self.assertEqual(set(empty.files_in_sys_hook('Linux', 'bin')), set())
        self.assertEqual(set(p.files_in_sys_hook('common', 'bin')),
                         fileset(pj(self.FULL, 'bin'), 'hello'))
        self.assertEqual(set(p.files_in_sys_hook('Linux', 'bin')),
                         fileset(pj(self.FULL, 'Linux', 'bin'), 'bye'))
        self.assertEqual(set(p.files_in_sys_hook('Linux', 'share')), set())
        self.assertEqual(set(p.files_in_sys_hook('Plan9', 'bin')), set())

    def test_file_map(self):
        def filemap(*l):
            return set([
                ((s, h, f),
                 pj(self.FULL, s, h, f) if s != 'common' \
                     else pj(self.FULL, h, f))
                for s, h, f in l])

        p = hc.Package(self.FULL)

        self.assertEqual(set(p.file_map()), filemap(
            ('common', 'bin', 'hello'),
            ('common', 'share', 'package/README.txt'),
            ('Darwin', 'bin', 'bye'),
            ('Linux', 'bin', 'bye'),
            ('Windows', 'bin', 'bye'),
            ('Windows', 'dlls', 'not-a-dll'),
        ))

        self.assertEqual(set(p.file_map(systems=['common', 'Linux'])), filemap(
            ('common', 'bin', 'hello'),
            ('common', 'share', 'package/README.txt'),
            ('Linux', 'bin', 'bye'),
        ))

        self.assertEqual(set(p.file_map(hooks=['bin'])), filemap(
            ('common', 'bin', 'hello'),
            ('Darwin', 'bin', 'bye'),
            ('Linux', 'bin', 'bye'),
            ('Windows', 'bin', 'bye'),
        ))

        self.assertEqual(
            set(p.file_map(systems=['common', 'Linux'], hooks=['bin'])),
            filemap(
                ('common', 'bin', 'hello'),
                ('Linux', 'bin', 'bye'),
            ))



class SystemTest(HomectlTest):
    # Tests for the System class API.  Sadly not everything here is very
    # testable, since it's just a thin wrapper around the Python/OS interface.
    # I'm not going to bother testing trivial functionality.

    @with_system
    def test_run_and_readlines(self, s):
        self.assertEqual(
            list(s.run_and_readlines("echo", "Hello")),
            [b"Hello"])
        self.assertEqual(list(s.run_and_readlines("true")), [])
        with self.assertRaises(subprocess.CalledProcessError):
            for l in s.run_and_readlines("false"): pass

    @with_system
    def test_update_file(self, s):
        s.update_file("foo", "bar")
        self.assertEqual(file_contents("foo"), "bar")
        s.update_file("foo", "other")
        self.assertEqual(file_contents("foo"), "other")

    @with_system
    def test_update_link(self, s):
        s.update_link(pj("foo", "bar"), "f")
        self.assertEqual(os.readlink("f"), pj("foo", "bar"))

        # XXX Use pj() here, if we want this to work on Windows ever...
        s.update_link("/dev/null", "f")
        self.assertEqual(os.readlink("f"), "/dev/null")

    @with_system
    def test_rm_link(self, s):
        s.update_link("f", "f")
        s.rm_link("f")
        self.assertEqual(os.path.exists("f"), False)



class DeploymentTest(HomectlTest):
    # Tests for the Deployment class API, except for upgrades (handled in a
    # separate test suite below).

    #
    # Utility functions
    #

    def enabled_list(self, d):
        with open(d.enabled_list, 'r') as f:
            return set([p.rstrip() for p in f.readlines()])

    def check_links(self, *lmap, **kw):
        for src, lnk in lmap:
            src_f = pj(self.dir, *src.split('/'))
            src_dir = os.path.dirname(src_f)
            if kw.get('testdata', None):
                lnk_path = os.path.realpath(pj(TESTDATA_DIR, *lnk.split('/')))
            else:
                lnk_path = os.path.realpath(pj(*lnk.split('/')))
            self.assertEqual(os.path.realpath(src_f), lnk_path)

    def check_absence(self, *path):
        for p in path:
            self.assertFalse(os.path.exists(pj(self.dir, *p.split('/'))))

    def mk_files(self, name, *files):
        for f in files:
            path = pj(name, *f.split('/'))
            hc.mkdirp(os.path.dirname(path))
            open(path, 'w').close()

    #
    # Test cases
    #

    @with_deployment
    def test_cfg_vars(self, d):
        self.assertEqual(d.homedir, self.dir)
        self.assertEqual(d.cfgdir, pj(self.dir, hc.CFG_DIR))
        self.assertEqual(d.enabled_list, pj(d.cfgdir, hc.ENABLED_LIST))

    @with_deployment
    def test_add_package_updates_enabled_list(self, d):
        self.assertEqual(d.packages, set())

        d.packages = set([hc.Package(self.EMPTY)])
        self.assertEqual(self.enabled_list(d),
            set([os.path.relpath(p, self.dir) for p in [self.EMPTY]]))
        self.assertEqual(d.packages, set([hc.Package(self.EMPTY)]))

        d.packages = d.packages.union([hc.Package(self.FULL)])
        self.assertEqual(self.enabled_list(d),
            set([os.path.relpath(p, self.dir) for p in [self.EMPTY, self.FULL]]))
        self.assertEqual(d.packages,
            set([hc.Package(self.EMPTY), hc.Package(self.FULL)]))

    @with_deployment
    def test_add_package_creates_homectl_links(self, d):
        self.assertEqual(d.packages, set())

        d.packages = set([hc.Package(self.FULL)])

        # Just spot-check a few things
        self.check_links(
            ('.homectl/common/bin/hello', 'package-full.hcpkg/bin/hello'),
            ('.homectl/Linux/bin/bye', 'package-full.hcpkg/Linux/bin/bye'),
            ('.homectl/common/share/package/README.txt', 'package-full.hcpkg/share/package/README.txt'),
            testdata=True,
        )

        # Platform-specific stuff should never show up in common
        self.check_absence(
            '.homectl/common/bin/bye',
            '.homectl/common/lib'
        )

    @with_deployment
    def test_rm_package_deletes_homectl_links(self, d):
        d.packages = set([hc.Package(self.FULL)])

        self.check_links(
            ('.homectl/common/bin/hello', 'package-full.hcpkg/bin/hello'),
            ('.homectl/Linux/bin/bye', 'package-full.hcpkg/Linux/bin/bye'),
            ('.homectl/common/share/package/README.txt', 'package-full.hcpkg/share/package/README.txt'),
            testdata=True,
        )

        d.packages = set()

        self.check_absence(
            '.homectl/common/bin/hello',
            '.homectl/Linux/bin/bye',
            '.homectl/common/share/package/README.txt',
        )

    @with_deployment
    def test_refresh_creates_links(self, d):
        self.mk_files('small.hcpkg', 'bin/foo', 'Linux/bin/bar')
        d.packages = set([hc.Package('small.hcpkg')])
        self.check_links(
            ('.homectl/common/bin/foo', 'small.hcpkg/bin/foo'),
            ('.homectl/Linux/bin/bar', 'small.hcpkg/Linux/bin/bar'),
        )
        self.check_absence(
            '.homectl/Linux/bin/foo',
            '.homectl/common/bin/bar',
        )

        self.mk_files('small.hcpkg', 'bin/new')
        d.refresh()
        self.check_links(
            ('.homectl/common/bin/new', 'small.hcpkg/bin/new'),
        )

    @with_deployment
    def test_refresh_deletes_links(self, d):
        self.mk_files('small.hcpkg', 'bin/foo', 'Linux/bin/bar')
        d.packages = set([hc.Package('small.hcpkg')])
        self.check_links(
            ('.homectl/common/bin/foo', 'small.hcpkg/bin/foo'),
            ('.homectl/Linux/bin/bar', 'small.hcpkg/Linux/bin/bar'),
        )
        self.check_absence(
            'small.hcpkg/Linux/bin/foo',
            'small.hcpkg/bin/bar',
        )

        os.unlink(pj('small.hcpkg', 'bin', 'foo'))
        d.refresh()
        self.check_absence('.homectl/common/bin/foo')

    @with_deployment
    def test_overlay_create_links(self, d):
        self.mk_files('overlay.hcpkg', 'overlay/.mycfg', 'overlay/.config/my')
        d.packages = set([hc.Package('overlay.hcpkg')])
        self.check_links(
            ('.homectl/common/overlay/.mycfg', 'overlay.hcpkg/overlay/.mycfg'),
            ('.homectl/common/overlay/.config/my', 'overlay.hcpkg/overlay/.config/my'),
            ('.mycfg', 'overlay.hcpkg/overlay/.mycfg'),
            ('.config/my', 'overlay.hcpkg/overlay/.config/my'),
        )

    @with_deployment
    def test_overlay_delete_links_on_refresh(self, d):
        self.mk_files('overlay.hcpkg', 'overlay/.mycfg', 'overlay/.config/my')
        d.packages = set([hc.Package('overlay.hcpkg')])
        self.check_links(
            ('.homectl/common/overlay/.mycfg', 'overlay.hcpkg/overlay/.mycfg'),
            ('.homectl/common/overlay/.config/my', 'overlay.hcpkg/overlay/.config/my'),
            ('.mycfg', 'overlay.hcpkg/overlay/.mycfg'),
            ('.config/my', 'overlay.hcpkg/overlay/.config/my'),
        )

        os.unlink(os.path.join('overlay.hcpkg', 'overlay', '.mycfg'))
        d.refresh()
        self.check_absence(
            '.homectl/common/overlay/.mycfg',
            '.mycfg',
        )

    @with_deployment
    def test_overlay_doesnt_touch_user_files(self, d):
        self.mk_files('overlay.hcpkg', 'overlay/.mycfg')
        self.mk_files('.', '.mycfg')

        d.packages = set([hc.Package('overlay.hcpkg')])
        self.assertTrue(os.path.exists('.mycfg'))
        self.assertFalse(os.path.islink('.mycfg'))

        d.packages = set()
        self.assertTrue(os.path.exists('.mycfg'))

    @with_deployment
    def test_trigger_pwd(self, d):
        os.mkdir('trigger.hcpkg')
        # XXX This is a UNIX-ism
        with open(pj('trigger.hcpkg', '_trigger'), 'w') as f:
            f.write('#!/bin/sh\npwd > %s\n' % pj(self.dir, 'triggered'))
        os.chmod(pj('trigger.hcpkg', '_trigger'), 0o755)

        pkg = hc.Package('trigger.hcpkg')
        d.packages = set([pkg])

        with open(pj(self.dir, 'triggered')) as f:
            self.assertEqual(pkg.path, f.readline().strip())

    @with_deployment
    def test_refresh_trigger(self, d):
        os.mkdir('trigger.hcpkg')
        # XXX This is a UNIX-ism
        with open(pj('trigger.hcpkg', '_trigger'), 'w') as f:
            f.write('#!/bin/sh\n[ "$1" = refresh ] && touch triggered\n')
        os.chmod(pj('trigger.hcpkg', '_trigger'), 0o755)

        pkg = hc.Package('trigger.hcpkg')
        sentinel = pj(pkg.path, 'triggered')

        d.packages = set([pkg])

        # Trigger should have run
        self.assertTrue(os.path.exists(sentinel))

        # Triggers should not be linked into ~/.homectl
        self.check_absence(
            '.homectl/_trigger',
            '.homectl/common/_trigger',
        )

    @with_deployment
    def test_disable_trigger(self, d):
        os.mkdir('trigger.hcpkg')
        # XXX This is a UNIX-ism
        with open(pj('trigger.hcpkg', '_trigger'), 'w') as f:
            f.write('#!/bin/sh\n[ "$1" = disable ] && touch triggered\n')
        os.chmod(pj('trigger.hcpkg', '_trigger'), 0o755)

        pkg = hc.Package('trigger.hcpkg')
        sentinel = pj(pkg.path, 'triggered')

        d.packages = set([pkg])

        # Shouldn't have run yet
        self.assertFalse(os.path.exists(sentinel))

        # Triggers should not be linked into ~/.homectl
        self.check_absence(
            '.homectl/_trigger',
            '.homectl/common/_trigger',
        )

        d.packages = set()

        # Trigger should have run
        self.assertTrue(os.path.exists(sentinel))

    @with_deployment
    def test_hook_tree(self, d):
        plat = os.uname()[0]
        d.packages = set([hc.Package(self.FULL)])

        self.assertEqual(
            set(d.hook_tree('bin', 'h*')),
            set([os.path.abspath(f) for f in [
                pj('.homectl', 'common', 'bin', 'hello')
            ]]))
        self.assertEqual(
            set(d.hook_tree('bin', 'b*')),
            set([os.path.abspath(f) for f in [
                pj('.homectl', plat, 'bin', 'bye')
            ]]))



class UpgradeTest(HomectlTest):
    # Tests related to upgrading pre-0.2 homectl setups to the latest format.

    # XXX I'm not really sure how to verify that a newly-created or upgraded
    # homectl setup is "good", since that's highly dependent on what the
    # individual user had in their old setup, or wants to put in their new
    # setup.

    def test_upgrade_is_needed(self):
        cfg = pj(self.dir, 'cfg-that-needs-upgrade')

        # Pre-Python homectls would just symlink ~/.homectl to somewhere; newer
        # homectls expect this to be a directory.
        os.symlink('/', cfg)

        d = hc.Deployment(SilentSystem(), self.dir, cfg)
        self.assertEqual(d.needs_upgrade, True)
        with self.assertRaises(hc.NeedsUpgrade): d.packages
        with self.assertRaises(hc.NeedsUpgrade): d.refresh()
        with self.assertRaises(hc.NeedsUpgrade): d.enable('foo.hcpkg')
        with self.assertRaises(hc.NeedsUpgrade): d.disable('foo.hcpkg')
        with self.assertRaises(hc.NeedsUpgrade): d.uninstall()

    @with_deployment
    def test_upgrade_not_needed(self, d):
        cfg = pj(self.dir, hc.CFG_DIR)
        hc.mkdirp(cfg)
        self.assertEqual(os.path.isdir(cfg), True)
        self.assertEqual(d.needs_upgrade, False)

    @with_deployment
    def test_upgrade_when_not_needed(self, d):
        d.upgrade() # Should be a no-op

    @with_deployment
    def test_upgrade(self, d):
        pkgs = ('one.hcpkg', 'two.hcpkg')
        enable_d_dir = pj(self.dir, 'enable.d')

        # First, create things that look like old homectl packages
        for i, p in zip(range(len(pkgs)), pkgs):
            hc.mkdirp(pj(self.dir, p, 'bin'))
            open(pj(self.dir, p, 'bin', str(i)), 'w').close()

        # Then create something that looks like hc<0.2's enable.d dir, with a
        # couple enabled packages inside
        hc.mkdirp(enable_d_dir)
        for p in pkgs: os.symlink(pj(self.dir, p), pj(enable_d_dir, p))

        # Then put the ~/.homectl symlink in place
        new_cfg = pj(self.dir, hc.CFG_DIR)
        os.symlink(enable_d_dir, new_cfg)

        # Finally call upgrade()
        d.upgrade()

        self.assertEqual(d.needs_upgrade, False)
        self.assertEqual(os.path.isdir(new_cfg), True)
        self.assertEqual(os.path.islink(new_cfg), False)
        self.assertEqual(set(hc.fs_tree(new_cfg)),
                         fileset(new_cfg,
                                 'enabled-pkgs',
                                 'common',
                                 'common/bin',
                                 'common/bin/0',
                                 'common/bin/1'))
        self.assertEqual(d.packages,
                         set([hc.Package(pj(self.dir, p)) for p in pkgs]))



class InitTest(HomectlTest):
    # Tests for creating new homectl deployments.
    pass



class CLITest(HomectlTest):
    # System-level tests for CLI commands themselves.
    pass



if __name__ == '__main__': # pragma: no branch
    unittest.main()
