#!/usr/bin/env python

import sys
import os
import shutil
import imp
import unittest
import tempfile

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



class HomectlTest(unittest.TestCase):
    # Base class for other homectl tests; handles creating and tearing down a
    # homectl environment in the filesystem.

    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix='homectl-selftest-')
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



class UtilTest(HomectlTest):
    # Small tests for utility functions.

    DATA = pj(TESTDATA_DIR, 'util')

    def test_mkdirp(self):
        hc.mkdirp('') # should do nothing
        hc.mkdirp('.') # should do nothing
        hc.mkdirp('..') # should do nothing

        hc.mkdirp('foo')
        self.assertEquals(os.path.isdir('foo'), True)

        hc.mkdirp(pj('outer', 'inner'))
        self.assertEquals(os.path.isdir('outer'), True)
        self.assertEquals(os.path.isdir(pj('outer', 'inner')), True)

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

    EMPTY = pj(TESTDATA_DIR, 'package-empty.hcpkg')
    FULL = pj(TESTDATA_DIR, 'package-full.hcpkg')

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

    def test_trigger_path(self):
        p = hc.Package(self.FULL)
        self.assertEqual(p.trigger_path('exec'), pj(self.FULL, 'exec.trigger'))
        self.assertEqual(p.trigger_path('not-exec'), None)
        self.assertEqual(p.trigger_path('missing'), None)

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

    # XXX
    pass



class DeploymentTest(HomectlTest):
    # Tests for the Deployment class API.
    # XXX
    pass



class InitUpgradeTest(HomectlTest):
    # Tests for the init and upgrade CLI commands, which live outside the
    # classes and provide some substantial functionality.

    # XXX
    pass



class CLITest(HomectlTest):
    # Tests for other CLI commands which provide significant functionality.

    # XXX
    pass



if __name__ == '__main__':
    unittest.main()
