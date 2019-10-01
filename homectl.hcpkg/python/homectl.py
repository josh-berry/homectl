#!/usr/bin/env python

#
# WARNING: This code is carefully written to work in both Python 2 AND Python 3,
# since on some systems, "python" means the former, while on others, it means
# the latter.  (Grumble, grumble, backward compatibility, etc.)  You may see
# some rather strange idioms here for that reason.
#

import os
import sys
import re
import subprocess
import collections
from optparse import OptionParser, OptionGroup
import fnmatch

# If we are imported as a module, this is our API.
__all__ = [
    'HOME', 'HOMECTL_DIR', 'DEFAULT_HOMECTL_URL',
    'System', 'ConsoleSystem',
    'Deployment', 'Package',
    'main',

    'HomectlError', 'PackageError',
]


VERSION = '0.3'

# System defaults that are hard to change.
CMD_NAME = 'hc'
HOME = os.environ['HOME']
CFG_DIR = os.environ.get('HOMECTL_DIR', os.path.join(HOME, '.homectl'))
DEFAULT_HOMECTL_URL = "https://github.com/josh-berry/homectl.git"
DEFAULT_HOMECTL_PKGS = ['homectl.hcpkg',
                        'loader-bash.hcpkg', 'loader-zsh.hcpkg']

ENABLED_LIST = 'enabled-pkgs'

# Errors
class HomectlError(Exception): pass
class PackageError(HomectlError): pass

# Util functions

if sys.version_info[0] == 2: # pragma: no cover
    def iteritems(coll): return coll.iteritems()
elif sys.version_info[0] == 3:
    def iteritems(coll): return coll.items()
else: # pragma: no cover
    raise RuntimeError("Don't know what version of Python this is!")

def mkdirp(path):
    if path == '': return
    if not os.path.isdir(path):
        os.makedirs(path)

def visible_dirs(path):
    # Lists all the visible directories, or symlinks to directories, within
    # /path/.  Yields a series of ("name", "path/name") tuples.
    for d in os.listdir(path):
        if d[0] == '.': continue
        p = os.path.join(path, d)
        if os.path.isdir(p):
            yield d, p

def visible_links(path):
    # Lists all the visible symlinks within /path/.  Yields a series of
    # ("name", "path/name") tuples.
    for d in os.listdir(path):
        if d[0] == '.': continue
        p = os.path.join(path, d)
        if os.path.islink(p):
            yield d, p

def fs_tree(path):
    # List all the files, directories, etc. in /path/, recursively.
    #
    # As a convenience, for each entry, yields a tuple of two paths:
    #
    # (relative_to_/path/, abs_path)

    if not os.path.exists(path):
        return

    for ent in os.listdir(path):
        entp = os.path.join(path, ent)

        yield ent, entp

        # We treat symlinks as regular files to avoid directory loops, and
        # because that's generally what we want in homectl packages
        if os.path.isdir(entp) and not os.path.islink(entp):
            for relp, absp in fs_tree(entp):
                yield os.path.join(ent, relp), absp

def fs_files_in(path):
    # List all the files in /path/.  /path/ is expected to be a directory; if it
    # is not, returns no values.  Assumes that all symlinks are files.
    return ((relp, absp) for relp, absp in fs_tree(path)
            if os.path.islink(absp) or not os.path.isdir(absp))

def sh_quote(text):
    # Quotes /text/ so it will be interpreted by shells as a single argument.
    # Takes a paranoid approach -- things that are not known to be safe are
    # escaped.

    bad_chars = re.compile("[^A-Za-z0-9_ .,/+-]")

    def escape_char(c):
        if c == "\n": return 'n'
        if c == "\r": return 'r'
        if c == "\t": return 't'
        if c == "\0": return '0'
        if ord(c) < 128: return c
        raise ValueError('Invalid character %d' % ord(c))

    text = bad_chars.sub((lambda m: "\\" + escape_char(m.group(0))), text)
    if ' ' in text:
        return "\"%s\"" % text
    else:
        return text



class Package(object):
    # A Package represents a single homectl package.  Packages contain files for
    # specific "systems" and "hooks".  homectl packages are stored as
    # directories in the filesystem with a ".hcpkg" extension, organized as
    # described below.
    #
    # A "system" is analagous to a type of computer system, e.g. a Linux system,
    # an Linux-x86_64 system, or a machine with a particular hostname.  The
    # special "common" system will be available on all machines, regardless of
    # their type.  Except for the special "common" system, all system names must
    # begin with an uppercase letter [A-Z].
    #
    # A "hook" is a special directory where files from many different packages
    # reside together in a common location.  One example of a hook is "bin" --
    # executable programs from many different packages may be placed in a common
    # "bin" directory (such as /usr/bin or ~/bin), so they can be found in a
    # single, well-known place.  All hook names must begin with a lowercase
    # letter [a-z].
    #
    # Files in a homectl package are typically organized by system, then hook.
    # For example, a binary which runs on Linux x86_64 systems will be placed in
    # the foo.hcpkg/Linux-x86_64/bin subdirectory.
    #
    # The special "common" system does not have its own directory; hooks for the
    # "common" system are placed directly inside the package (e.g. shell scripts
    # would go under foo.hcpkg/bin).
    #
    # In the "common" system only, there is a special "overlay" hook which
    # contains files that are linked directly into the user's home directory
    # (for example, a .emacs or a .vimrc).  Like other "common" hooks, these
    # files go directly under foo.hcpkg/overlay.

    def __init__(self, path):
        self.path = os.path.abspath(path)

        if not self.path.endswith('.hcpkg'):
            raise PackageError("%s: Not a homectl package" % self.path)

    def __eq__(self, other):
        return os.path.realpath(self.path) == os.path.realpath(other.path)

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(os.path.realpath(self.path))

    @property
    def systems(self):
        yield 'common'
        for name, path in visible_dirs(self.path):
            if re.match('^[A-Z].*', name):
                yield name

    def _system_dir(self, system):
        if system == 'common':
            return self.path
        else:
            return os.path.join(self.path, system)

    def _hook_dir(self, system, hook):
        return os.path.join(self._system_dir(system), hook)

    def hooks_in_system(self, system):
        # Yields a list of hooks present in the specified system.  If the
        # requested system doesn't exist, doesn't yield anything.
        d = self._system_dir(system)
        if not os.path.isdir(d): return

        for name, path in visible_dirs(d):
            if not re.match('^[A-Z_].*', name):
                yield name

    def files_in_sys_hook(self, system, hook):
        # Yields a list of files present in the specified system/hook,
        # recursively, in the style of fs_files_in().  If the requested system
        # and/or hook doesn't exist, doesn't yield anything.
        d = self._hook_dir(system, hook)
        if not os.path.isdir(d): return []
        return fs_files_in(d)

    #
    # Derived Functionality
    #

    def file_map(self, systems=None, hooks=None):
        # For all hooks and systems in this package, return a list of tuples:
        #
        # ( (system, hook, hook_file), "/full/path/to/file/in/pkg" )
        #
        # for each file that is present in this package.

        for s in self.systems:
            if systems and s not in systems: continue

            for h in self.hooks_in_system(s):
                if hooks and h not in hooks: continue

                for f, fpath in self.files_in_sys_hook(s, h):
                    yield (s, h, f), fpath



class System(object):
    # System serves a dual purpose -- it provides an independent interface for
    # homectl to interact with the user and the OS, and it provides information
    # on what capabilities the current machine/environment supports.
    #
    # Capability information is provided as a set of system names, such as
    # "common", "Linux", "Linux-x86_64", "Host-mymachine", etc.  These names
    # correspond to the systems in homectl Package objects.  homectl packages
    # provide functionality for specific systems (or all systems, via the
    # "commoon" system), and only those systems supported by the current machine
    # are reported by the System.names property.
    #
    # The System class also provides an interface to the host machine (and thus
    # the user) with a few utility functions for logging (log() and log_*()),
    # and executing programs and logging/returning the results (run()).

    def __init__(self, pretend=False):
        self.pretend = pretend

    @property
    def names(self):
        system, node, rel, ver, machine = os.uname()
        return ['common',
                system,
                '%s-%s' % (system, machine),
                '%s-%s' % (system, rel),
                '%s-%s-%s' % (system, rel, machine)]

    def log_cmd(self, *args):
        self.log('$ %s' % ' '.join([sh_quote(a) for a in args]))

    def log_output(self, text):
        self.log('  ... %s' % text)

    def log_warn(self, msg):
        self.log('!!! %s' % msg)

    def log_err(self, msg):
        self.log('!!! %s' % msg)

    def run_and_readlines(self, *args, **opts):
        # Runs the specified command; yields lines of output from the command's
        # stdout/stderr (with trailing whitespace stripped, for convenience).
        #
        # On an error, or if the process exits with a non-zero status, raises
        # subprocess.CalledProcessError.

        self.log_cmd(*args)
        proc = subprocess.Popen(args, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, close_fds=True,
                                **opts)
        with proc.stdout:
            while True:
                line = proc.stdout.readline()
                if not line: break
                self.log_output(line)
                yield line.rstrip()

        rc = proc.wait()
        if rc != 0:
            raise subprocess.CalledProcessError(rc, args)

    def run(self, *args, **opts):
        for l in self.run_and_readlines(*args, **opts): pass

    def update_file(self, path, contents):
        self.log_cmd('update', path)
        if self.pretend: return

        mkdirp(os.path.dirname(path))
        with open(path + '.tmp', 'w') as f:
            f.write(contents)
            f.flush()
            os.fsync(f)
        os.rename(path + '.tmp', path)
        # XXX Should make sure permissions, etc. match the old file

    def update_link(self, src, target):
        tgt_dir = os.path.dirname(target)
        link_text = os.path.relpath(src, tgt_dir)

        if os.path.islink(target):
            if os.readlink(target) == src:
                # Link already in place; nothing to do
                return
            self.rm_link(target)

        elif os.path.exists(target):
            self.log_warn("Won't touch existing file: %s" % target)
            return

        self.log_cmd('ln', '-s', src, target)
        if not self.pretend:
            mkdirp(tgt_dir)
            os.symlink(src, target)

    def rm_link(self, path):
        if os.path.islink(path):
            self.log_cmd("rm", path)
            if not self.pretend:
                os.unlink(path)
        else:
            self.log_warn("Can't unlink %s: Not a symbolic link" % path)



class Deployment(object):
    # A homectl Deployment is the set of "enabled" or activated packages, as
    # deployed into the user's home directory (via symlinks, etc.).
    #
    # Packages may be enabled/disabled (analagous to installing/uninstalling) by
    # modifying the Deployment.packages attribute, or calling enable() or
    # disable().  Deployment.packages is always a list of Package objects,
    # representing the packages enabled in the user's deployment.
    #
    # When Deployment.packages is modified (directly or via enable()/disable()),
    # the user's deployment is automatically refresh()ed.  This updates all the
    # symlinks put in place to the user's various packages, removing old ones,
    # placing new ones, etc.  refresh() may also be called any time a
    # previously-enabled package is modified, to ensure the deployment reflects
    # all the changes.
    #
    # The contents of enabled packages are symlinked in the config dir (see
    # CFG_DIR).  A directory tree under $CFG_DIR is created for all the
    # systems and hooks in all the user's enabled packages.  Links are
    # maintained in the form:
    #
    #     $CFG_DIR/<system>/<hook>/<path_to_file_in_pkg>
    #
    # Additionally, any files in the "common/overlay" system/hook are linked
    # directly underneath the user's home directory.  This "overlay" hook is
    # used for dot-files and other config files.
    #
    # The contents of all enabled packages may be queried using the hook_dirs()
    # and hook_tree() calls.  The former lists all directories for a hook which
    # apply to the current system.  The latter enumerates all the files within a
    # particular hook which apply to the current system.

    def __init__(self, system, homedir=HOME, cfgdir=CFG_DIR):
        self.sys = system

        self.homedir = os.path.realpath(homedir)
        self.cfgdir = os.path.realpath(cfgdir)
        self.enabled_list = PackageListFile(
            system, os.path.join(self.cfgdir, ENABLED_LIST), self.homedir)

    @property
    def packages(self):
        return self.enabled_list.packages

    @packages.setter
    def packages(self, pkgs):
        # Run pre-removal triggers
        removing_pkgs = set(self.packages) - set(pkgs)
        for pkg in removing_pkgs: self.run_pkg_trigger(pkg, "disable")

        self.enabled_list.packages = pkgs
        self.refresh()

        # Run post-removal triggers
        for pkg in removing_pkgs: self.run_pkg_trigger(pkg, "clean")

    def hook_dirs(self, hook):
        for s in self.sys.names:
            p = os.path.join(self.cfgdir, s, hook)
            if os.path.isdir(p):
                yield p

    def hook_tree(self, hook, glob=None):
        for d in self.hook_dirs(hook):
            for r, a in fs_tree(d):
                if not glob or fnmatch.fnmatch(r, glob):
                    yield a

    def run_pkg_trigger(self, pkg, trigger):
        tpath = os.path.join(pkg.path, '_trigger')
        if os.path.isfile(tpath) and os.access(tpath, os.X_OK):
            try:
                self.sys.run(tpath, trigger, cwd=pkg.path)
                return 0
            except subprocess.CalledProcessError as e:
                return e.returncode

    def refresh(self):
        # Run pre-refresh triggers so packages can create things in
        # homectl-visible directories if necessary.
        for p in self.packages:
            self.run_pkg_trigger(p, "build")

        link_map = {} # link_path_in_cfgdir: link_text
        overlay_links = set() # relative paths under $cfgdir/common/overlay
        overlay_rel = os.path.join('common', 'overlay')
        overlay_path = os.path.join(self.cfgdir, overlay_rel)

        # First, build the set of links we know should exist for each package.
        for p in self.packages:
            for (s, h, f), path in p.file_map():
                cfgrel = os.path.join(s, h, f)
                cfgabsdir = os.path.join(self.cfgdir, os.path.dirname(cfgrel))
                if not path.startswith(self.homedir):
                    link_text = path # use the abs path since it's outside ~
                else:
                    link_text = os.path.relpath(path, cfgabsdir)

                link_map[cfgrel] = link_text
                if s == 'common' and h == 'overlay':
                    overlay_links.add(f)

        # Remove any overlay links in ~ that don't match what we expect.  We
        # discover overlay links by scanning $cfgdir/common/overlay (before
        # removing anything from it), so we don't have to do a recursive scan of
        # ~.
        for rel, path in fs_files_in(overlay_path):
            home_path = os.path.join(self.homedir, rel)

            # For each link in $cfgdir/common/overlay, we expect there to be a
            # corresponding link from ~/... to $cfgdir/common/overlay/...

            # If this isn't true, leave the link in ~ alone.
            if not os.path.islink(home_path):
                if os.path.exists(home_path):
                    self.sys.log_warn("Won't touch existing file: %s"
                                      % home_path)
                continue

            if rel not in overlay_links:
                # This is a stale overlay link; remove it
                self.sys.rm_link(home_path)

        # Remove any link in the cfgdir that doesn't match what we expect.
        for rel, path in fs_files_in(self.cfgdir):
            if not os.path.islink(path): continue
            if rel not in link_map:
                self.sys.rm_link(path)

        # Update all the links in $cfgdir.
        for rel, text in iteritems(link_map):
            # Create in $cfgdir
            lnk = os.path.join(self.cfgdir, rel)
            self.sys.update_link(text, lnk)

        # Now update any missing/old links from ~ to $cfgdir.
        for rel in overlay_links:
            home_path = os.path.join(self.homedir, rel)
            cfg_path = os.path.join(overlay_path, rel)
            link_text = os.path.relpath(cfg_path, os.path.dirname(home_path))

            self.sys.update_link(link_text, home_path)

        # XXX cleanup empty dirs in $cfgdir

        # Run post-refresh triggers so packages can ensure everything is okay
        for p in self.packages:
            self.run_pkg_trigger(p, "refresh")

    #
    # Derived methods (implemented only in terms of the above)
    #

    def enable(self, pkg):
        self.packages = list(self.packages) + [pkg]

    def disable(self, pkg):
        self.packages = [
            p for p in self.packages
            if os.path.realpath(p.path) != os.path.realpath(pkg.path)]

    def uninstall(self):
        self.packages = []
        self.sys.run('rm', '-rf', self.cfgdir)

        self.sys.log('')
        self.sys.log('homectl has been uninstalled.')
        self.sys.log('')



class PackageListFile(object):
    # A file containing a list of packages.  The packages themselves are
    # accessible through the .packages property, which is expected to be a set
    # of Packages.  When .packages is set, the file is updated incrementally (to
    # preserve comments and be friendly to version control) and written to disk.

    def __init__(self, sys, path, relative_to=None):
        self.sys = sys
        self.path = path
        self.relative_to = relative_to if relative_to \
            else os.path.dirname(self.path)

        self.__pkgs = set((p for p in self._read_pkg_lines()
                           if isinstance(p, Package)))

    def _read_pkg_lines(self):
        # Reads the package list line by line, replacing recognized package
        # paths with package objects, and yielding the remaining lines
        # (comments, etc.) as strings.
        try:
            with open(self.path, 'r') as f:
                for l in self.read_pkg_lines_from_fd(f, self.relative_to):
                    yield l
        except IOError: pass

    @staticmethod
    def read_pkg_lines_from_fd(fd, relative_to):
        for l in fd.readlines():
            if l.rstrip() == '' or l.startswith('#'):
                yield l.rstrip()
                continue
            yield Package(os.path.join(relative_to, l.strip()))

    @property
    def packages(self):
        return set(self.__pkgs)

    @packages.setter
    def packages(self, pkgs):
        pkgs = set(pkgs)

        for p in pkgs:
            if not isinstance(p, Package):
                raise TypeError('%r: Expected a Package' % (p,))

        out = []
        unrecorded = set(pkgs)

        # Read the package file, preserve comments and remove old packages as
        # needed.
        for line_or_pkg in self._read_pkg_lines():
            if isinstance(line_or_pkg, Package):
                if line_or_pkg not in pkgs:
                    # It was removed from the list; skip it
                    continue
                else:
                    # It's already in the file; keep it
                    unrecorded.remove(line_or_pkg)
                    out.append(os.path.relpath(line_or_pkg.path, self.relative_to))
            else:
                out.append(line_or_pkg)

        # Append anything new that wasn't mentioned in the file before.
        out += sorted(set([os.path.relpath(p.path, self.relative_to)
                           for p in unrecorded]))

        # Write the file.
        if out:
            self.sys.update_file(self.path, "\n".join(out) + "\n")
        else:
            self.sys.update_file(self.path, '')

        self.__pkgs = set(pkgs)



class ConsoleSystem(System):
    def log(self, msg):
        print(msg.rstrip())



#
# User-facing homectl commands
#

commands = {}

def cmd_help(d, args):
    print("""Usage: %(cmd)s CMD OPTIONS ...

For help on individual commands, run "cmd --help".

    init

    refresh
    uninstall

    list
    enable
    disable
    set-enabled

    path
    find
""" % {'cmd': CMD_NAME})

commands['help'] = cmd_help

def cmd_init(d, argv):
    if len(argv) < 2 or argv[1].startswith('-'):
        print("""Usage: %s init PATH-TO-YOUR-GIT-REPO [URL]

Create a new homectl setup.

Creates a new Git repository at the path you specify, and populates
it with a copy of homectl and a helper script for deploying your
setup on new machines.

You can optionally specify a URL to a homectl Git repo, and 'init'
will use that repo instead of the default.  This is only useful if
you work on homectl itself.  Note that if you specify a local
filesystem path for the URL, it must be an absolute path.
""" % CMD_NAME)
        return

    hc_url = DEFAULT_HOMECTL_URL
    if len(argv) > 2:
        hc_url = argv[2]

    # Stuff to create or update
    gitrepo = argv[1]
    readme = os.path.join(gitrepo, 'README.md')
    deploy_sh = os.path.join(gitrepo, 'deploy.sh')

    # Figure out what packages to enable by default
    cmd_tmpl = '$%(cmd)s enable %(pkg)s\n'

    enable_cmds = ['# Default homectl packages\n']
    enable_cmds += [cmd_tmpl % {'cmd': CMD_NAME,
                                'pkg': os.path.join('homectl', p)}
                    for p in DEFAULT_HOMECTL_PKGS]

    enable_cmds += ['\n# Your custom packages\n']
    enable_cmd_str = ''.join(enable_cmds)

    # Setup the git repo
    if not os.path.isdir(gitrepo) or \
       not os.path.isdir(os.path.join(gitrepo, '.git')):
        d.sys.run('git', 'init', gitrepo)

    if not os.path.isdir(os.path.join(gitrepo, 'homectl')):
        d.sys.run('git', 'submodule', 'add', hc_url, 'homectl',
                  cwd=gitrepo)

    d.sys.update_file(readme, """%(user)s's homectl setup
================================================================================

This is a Git repository which contains a homectl setup.  homectl is a program
which manages all the scripts, dot-files, vendor packages, etc. that you tend to
accumulate over time in your home directory.  Feel free to dump all of these
things here (or in git submodules linked here).

Quick Deployment
----------------

All you have to do is:

    $ ./deploy.sh

And all your homectl packages, as they are stored here, will be reconsituted.

WARNING: Once you have deployed, do not rename or move this Git repository.  You
will break your homectl installation if you do.  (If you have to move it,
uninstall first and then re-deploy.)

Uninstallation
--------------

    $ %(cmd)s uninstall

Your Git repository will be left alone but all homectl-created symlinks in your
home directory will be removed.

Additional Notes
----------------

NOTE: Be sure to check out the homectl documentation for tips on how to create
your own homectl packages.

A few files and directories have been included here to start you off.

- `README.md`: This file.

- `homectl/`: A git submodule containing homectl itself.  It includes the
  `%(cmd)s` command and its accompaniing documentation.

- `deploy.sh`: A handy, one-step script to run on new machines to instantly
  deploy your painstakingly-crafted homectl configuration.

NOTE: Make sure the `homectl.hcpkg` package itself remains enabled, or you won't
be able to use the `hc` command to manage your setup.
""" % {'user': os.environ['LOGNAME'], 'cmd': CMD_NAME})

    d.sys.run('git', 'add', os.path.basename(readme), cwd=gitrepo)

    d.sys.update_file(deploy_sh, """#!/bin/bash

# This script deploys your homectl setup into your home directory, by enabling
# a default set of homectl packages.

cd "$(dirname "$0")"
%(cmd)s=./homectl/homectl.hcpkg/bin/%(cmd)s

set -e

git submodule update --init --recursive

%(enable_cmds)s
""" % {'cmd': CMD_NAME, 'enable_cmds': enable_cmd_str})

    d.sys.run('chmod', 'a+x', deploy_sh)
    d.sys.run('git', 'add', os.path.basename(deploy_sh), cwd=gitrepo)

    d.sys.run('git', 'commit', '--author', 'homectl <homectl@(none)>',
              '-m', 'New homectl setup', cwd=gitrepo)

    d.sys.log('')
    d.sys.log('If you want to start using this homectl setup,')
    d.sys.log('you should now run:')
    d.sys.log('')
    d.sys.log('    %s' % deploy_sh)
    d.sys.log('')

commands['init'] = cmd_init

def cmd_refresh(d, argv):
    if len(argv) > 1:
        print("""Usage: %s refresh

Scans for any changes in your homectl packages, and ensures those
changes are reflected in your home directory (e.g. creates/removes
symlinks so that scripts and binaries appear in your path).
""" % CMD_NAME)
        return

    d.refresh()
commands['refresh'] = cmd_refresh
commands['ref'] = cmd_refresh

def cmd_uninstall(d, argv):
    if len(argv) > 1:
        print("""Usage: %s uninstall

Completely remove homectl from your home directory.  Leaves your
homectl setup (git repository) intact.

If you accidentally uninstall, you can run your deploy.sh script
again to get your homectl deployment back.
""" % CMD_NAME)
        return

    d.uninstall()
commands['uninstall'] = cmd_uninstall

def cmd_list(d, argv):
    parser = OptionParser(
        usage="""Usage: %s list [options]

List all enabled packages.""" % CMD_NAME)
    parser.add_option('-r', '--relative', dest='relative', default=False,
                      action='store_true',
                      help="Print paths relative to the current directory")
    options, args = parser.parse_args(argv)

    pkgs = [p.path for p in d.packages]
    if options.relative:
        pkgs = [os.path.relpath(p) for p in pkgs]

    for p in sorted(pkgs):
        print(p)
commands['list'] = cmd_list
commands['ls'] = cmd_list

def cmd_set_enabled(d, argv):
    parser = OptionParser(
        usage="""Usage: %(cmd)s set-enabled

Changes the entire set of enabled packages to be the list on stdin, one
package per line (ignoring #-comments and blank lines).  Packages which
are currently enabled and do not appear in the list will be disabled;
packages which appear in the list and are not enabled will be enabled.

This command is intended for script-friendly use to bring your system
into a known configuration; for example, assume the following is passed
to stdin:

    # Built-in packages
    homectl/homectl.hcpkg

    my-package.hcpkg
    another.package.hcpkg
    ...

Running `hc set-enabled` would set the entire set of enabled packages to
the list above.  Any package NOT in the list would be disabled.

A refresh is done automatically as part of the enable/disable process."""
        % {'cmd': CMD_NAME})
    options, args = parser.parse_args(argv)

    if len(args) != 1:
        parser.print_usage()
        sys.exit(1)

    pkgs = set((p
                for p in PackageListFile.read_pkg_lines_from_fd(
                        sys.stdin, os.getcwd())
                if isinstance(p, Package)))

    d.packages = pkgs
commands['set-enabled'] = cmd_set_enabled

def cmd_enable(d, argv):
    if len(argv) <= 1 or argv[1].startswith('-'):
        print("""Usage: %s enable PKG [PKG ...]

Enables one or more packages, linking their contents into
your home directory.

If the package contains new shell aliases, changes to $PATH,
etc., you will have to restart any affected programs to pick
up the new features.
""" % CMD_NAME)
        return

    d.packages = d.packages.union([Package(path) for path in argv[1:]])

commands['enable'] = cmd_enable
commands['en'] = cmd_enable

def cmd_disable(d, argv):
    if len(argv) <= 1 or argv[1].startswith('-'):
        print("""Usage: %s disable PKG [PKG ...]

Undoes the effect of the 'enable' command.  Unlinks the enabled
package from your home directory, so it won't be loaded by default.

You may have to restart programs this package uses after it is
disabled.
""" % CMD_NAME)
        return

    d.packages = d.packages.difference([Package(path) for path in argv[1:]])

commands['disable'] = cmd_disable
commands['dis'] = cmd_disable

def cmd_path(d, argv):
    parser = OptionParser(
        usage="""Usage: %s path [options] [HOOK] [ENV_VAR]

The 'path' command generates a list of directories (or other items), combining
several sources as described in the options.  Once the list is generated, 'path'
will remove duplicates from the list, keeping the item that appears earliest.

The HOOK and ENV_VAR parameters are deprecated; they are the same as the -H and
-E options, respectively.""" % CMD_NAME)

    delim = OptionGroup(parser, "Delimiters")
    delim.add_option('-d', '--delimiter', dest='delimiter', default=':',
                     help="Separate items in the list with DELIMITER "
                        + "(default: '%default')")
    delim.add_option('-n', '--newlines', dest='delimiter',
                     action='store_const', const="\n",
                     help="Separate items in the list with newlines "
                        + "(conflicts with -d).")
    delim.add_option('-i', '--in-delimiter', dest='in_delimiter', default=':',
                     help="When parsing environment variables, split them "
                        + "using DELIMITER (default: '%default')")
    parser.add_option_group(delim)

    actions = OptionGroup(parser, "Adding Items to a Path",
"""Options are handled in the order listed below; that is, all -H items are
added before all -E items.  You may pass each option multiple times; within a
particular option type, items are added in the order presented.""")
    actions.add_option('-P', '--prepend', dest='prepend', metavar='ITEM',
                       action='append',
                       help='Prepend a single item to the list')
    actions.add_option('-H', '--hook', dest='hook', metavar='NAME',
                       action='append',
                       help=
"""Include directories for a homectl hook.  For example, if "bin" is used,
'path' will return a list of every directory in which "bin" files may be found
for this system.""")
    actions.add_option('-E', '--env', dest='env', metavar='VAR',
                       action='append',
                       help=
"""Include an environment variable.  Items are extracted from the variable by
splitting it using the delimiter specified with -i (or ':' if -i isn't
specified).""")
    actions.add_option('-A', '--append', dest='append', metavar='ITEM',
                       action='append',
                       help='Append a single item to the list')
    parser.add_option_group(actions)

    options, args = parser.parse_args(argv)

    if not options.hook: options.hook = []
    if not options.env: options.env = []
    if not options.append: options.append = []
    if not options.prepend: options.prepend = []

    if len(args) > 1:
        options.hook.append(args[1])
    if len(args) > 2:
        options.env.append(args[2])

    dirs = options.prepend
    for hook in options.hook:
        dirs += d.hook_dirs(hook)
    for var in options.env:
        dirs += os.environ.get(var, "").split(options.in_delimiter)
    dirs += options.append

    uniq_dirs = []
    for d in dirs:
        if d not in uniq_dirs:
            uniq_dirs.append(d)

    print(options.delimiter.join(uniq_dirs))

commands['path'] = cmd_path

def cmd_tree(d, argv):
    parser = OptionParser(
        usage="""Usage: %s tree [options] HOOK [GLOB [GLOB ...]]

The 'tree' command searches through the specified HOOK for files and directories
that match GLOBs (which may also be a path).

This is equivalent to (but more convenient/faster than) using "hc path" and
searching each returned path with "find -path GLOB".

GLOBs are searched in the order presented; for example, if both '*.sh' and
'*.zsh' are specified, all files ending in '.sh' will be returned first,
followed by all files ending in '*.zsh'.  If this isn't the behavior you want,
consider piping the output through `sort`, `uniq`, or similar.

""" % CMD_NAME)
    parser.add_option('-d', '--delimiter', dest='delimiter', default=' ',
                      help="Separate items with DELIMITER (default: '%default')")
    parser.add_option('-n', '--newlines', dest='delimiter',
                      action='store_const', const="\n",
                      help='Separate items with newlines.')
    options, args = parser.parse_args(argv)

    if len(args) < 2:
        parser.print_usage()
        sys.exit(1)

    hook = args[1]
    files = []

    if len(args) >= 3:
        for glob in args[2:]:
            files.extend([a for a in d.hook_tree(hook, glob)])
    else:
        files.extend([a for a in d.hook_tree(hook)])

    if files:
        print(options.delimiter.join(files))

commands['tree'] = cmd_tree

def cmd_find(d, argv):
    if len(argv) <= 1 or argv[1].startswith('-'):
        print("""Usage: %s find FILE

This command is deprecated.  Use "path", "tree", or "files" instead.
""" % CMD_NAME)
        return

    # XXX This is for compatibility with homectl <= 0.2
    for p in sorted(d.packages):
        for f in os.listdir(p.path):
            if f == argv[1]:
                print(os.path.join(p.path, f))
commands['find'] = cmd_find

def main(argv):
    show_help = len(argv) < 2 or argv[1] == 'help' or argv[1] == '--help'

    if show_help:
        cmd = commands['help']
    else:
        try:
            cmd = commands[argv[1]]
        except KeyError:
            raise

    system = ConsoleSystem(pretend=False)

    try:
        d = Deployment(system)
        cmd(d, argv[1:])

    except KeyboardInterrupt: return 1
    except SystemExit: raise
    except (HomectlError, EnvironmentError) as e:
        system.log_err('[%s] %s' % (type(e).__name__, str(e)))
        return 1

    return 0



if __name__ == '__main__': # pragma: no cover
    sys.exit(main(sys.argv))
