#!/usr/bin/env python3
import logging
from tempfile import gettempdir
import os
from os.path import abspath, join
import sys
import shutil
import subprocess
import difflib
import datetime
import time

log = logging.getLogger('test')

prefixes_to_ignore = ['Wall-clock', 'CPU time', 
    'Version', 'Platform', 'Timer', 'Build-date',
    'Warning', 'https', 'www', 'Peter', 'Daniel', 'Dylan',
    'time taken for']

test_categories = ['short', 'cx', 'long', 'geminal', 'relativistic']

def is_junk(line):
    return any(map(line.startswith, prefixes_to_ignore))


def isclose(a, b, abs_tol=0.0, rel_tol=0.0):
    return (abs(a-b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol))

def get_lines(filename):
    """Read all lines from a file, returning a
    list of line contents

    Arguments:
    filename -- name/path of the file to read
    """
    lines = []
    with open(filename) as f:
        lines = []
        junk_lines = []
        for i, line in enumerate(f):
            if not is_junk(line.strip()):
                lines.append(line)
            else:
                junk_lines.append(i)
    log.debug('Ignored junk lines: %s', junk_lines)
    return lines


def is_float(s):
    """Check if a string may be converted into a float

    Arguments:
    s -- string to be converted"""
    try:
        float(s)
        return True
    except ValueError:
        return False

def equivalent(s1, s2, **kwargs):
    if is_float(s1) and is_float(s2):
        return isclose(float(s1), float(s2), **kwargs)
    else:
        return s1 == s2
    

def check_numbers(line1, line2, **kwargs):
    tokens1 = line1.strip('+- ').split()
    tokens2 = line2.strip('+- ').split()
    if len(tokens1) != len(tokens2):
        return False

    else:
        return all(
                equivalent(t1, t2, **kwargs) 
                for t1, t2 in zip(tokens1, tokens2))


def diff_sbf(file1, file2, args): 
    """Find the differences between 2 cxs files using sbftool
    """
    verbosity = 1
    retcode = subprocess.check_call([args.sbftool, '-vc', file1, file2])
    log.debug('sbftool returned: %s', retcode)
    return (retcode == 0)


def is_sbf(filename):
    """Check if a file is a SBF by reading the header"""
    with open(filename, 'rb') as f:
        if f.read(3) == b'SBF':
            return True
    return False

def diff_files(file1, file2, args, print_diffs=True, **kwargs):
    """Find the differences between two output files, delegating
    to sbftool for sbf files"""
    if is_sbf(file1) and is_sbf(file2):
        log.debug('Diffing with sbftool')
        return diff_sbf(file1, file2, args)
    lines1 = get_lines(file1)
    lines2 = get_lines(file2)
    diff = list(difflib.ndiff(lines1, lines2))
    del1 = [x for x in diff if x.startswith('-')]
    del2 = [x for x in diff if x.startswith('+')]
    diffs = [check_numbers(l1, l2, **kwargs) for l1, l2 in zip(del1, del2)]
    correct = all(diffs)

    if print_diffs:
        log.info('Diff:\n%s', ''.join(a + b for a, b in zip(del1,del2)))

    if not correct:
        log.debug('Found differences in %s and %s', file1, file2)
    return correct


class working_directory:
    """ Context manager for temporarily changing the current working directory. """
    old_directory = None

    def __init__(self, directory, create=False):
        if directory:
            self.directory = os.path.expanduser(directory)
            if not os.path.exists(self.directory) and create:
                os.makedirs(self.directory)
        else:
            self.directory = None


    def __enter__(self):
        self.old_directory = os.getcwd()
        if self.directory:
            log.debug('cwd: %s', self.directory)
            os.chdir(self.directory)

    def __exit__(self, exc_type, exc_val, exc_tb):
        os.chdir(self.old_directory)

def temp_test_dir(testname, subdir='tonto-tests'):
    d = join(gettempdir(), subdir)
    timestamp = datetime.datetime.now().strftime('-%y.%m.%d_%H:%M')
    name = join(d, testname + timestamp)
    log.debug('temp_test_dir = %s', name)
    return name

def parse_IO_file(path):
    io_files = {
        'input': set(['stdin']),
        'output': set(['stdout']),
        'delete': set(),
    }

    if os.path.exists(path):
        with open(path) as f:
            for line in f:
                tokens = line.split(':')
                io_files[tokens[0].strip()].add(tokens[1].strip())
    return io_files


def compare_outputs(f1, f2, args):
    if args.compare_program:
        return (subprocess.check_call([args.compare_program, f1, f2]) == 0)
    else:
        log.debug('Using builtin diffing or sbftool')
        d = diff_files(f1, f2, args, rel_tol=args.rel_tol, abs_tol=args.abs_tol)
        log.debug('diff_files returned: %s', d)
        return d


def run_test(args, test_dir, io_files):
    env = dict(os.environ)
    env['TONTO_BASIS_SET_DIRECTORY'] = args.basis_sets
    kwargs = {
        'shell': False,
        'universal_newlines': True,
        'env': env,
    }
    if args.mpi:
        prog = ['mpirun', '-np', '4', args.program]
    else:
        prog = [args.program]

    timings = {}
    exec_dir = temp_test_dir(os.path.basename(test_dir.rstrip('/')))
    timings['start'] = time.time() 
    with working_directory(exec_dir, create=True):
        for path in io_files['input']:
            shutil.copy(abspath(join(test_dir, path)), '.')
        timings['cp_input'] = time.time() - timings['start']

        log.debug('Running program %s', ' '.join(prog))
        retcode = subprocess.check_call(prog, **kwargs)
        completed = (retcode == 0)

        timings['tonto'] = time.time() - sum(t for t in timings.values())
        files_equivalent = []

        if completed:
            log.debug('Outputs to check %s', io_files['output'])

            for path in io_files['output']:
                canonical = abspath(join(test_dir, path))
                log.debug('Comparing %s to %s', path, canonical)
                d = compare_outputs(canonical, path, args)
                log.debug('Same file: %s', d)
                files_equivalent.append(d)
        timings['diffs'] = time.time() - sum(t for t in timings.values())
        success = completed and all(files_equivalent)

        for path, equivalent in zip(io_files['output'], files_equivalent):
            log.debug('%s: %s', path, 'GOOD' if equivalent else 'BAD')
            if not equivalent:
                shutil.copy(abspath(join('.', path)),
                        abspath(join(test_dir, path + '.bad')))
        timings['cp_output'] = time.time() - sum(t for t in timings.values())
        log.debug('Time spent:')
        for k, v in timings.items():
            if k != 'start':
                log.debug('%s: \t %f s', k, v)
    return success

def main():
    """Show the differences between two test files
    """
    import argparse
    import os
    parser = argparse.ArgumentParser()
    parser.add_argument('--program', '-p', default='./tonto',
                        help='Program to use to run the test jobs i.e. tonto')
    parser.add_argument('--test-directory', '-t', default='.',
                        help='Directory in which tests are located')
    parser.add_argument('--compare-program', '-c', default=None,
                        help='diff style program to compare outputs')
    parser.add_argument('--log-level', default='ERROR',
                        help='Log level for running tests')
    parser.add_argument('--basis-sets', '-b', default='.',
                        help='Basis sets directory')
    parser.add_argument('--sbftool', default='../../external/sbf/src/sbftool',
                        help='Location of sbftool')
    parser.add_argument('--mpi', '-m', default=False, action='store_true',
                        help='Test with mpirun')
    parser.add_argument('--abs-tol', type=float, default=1e-7,
                        help='Absolute tolerance for numerical differences')
    parser.add_argument('--rel-tol', type=float, default=1e-3,
                        help='Relative tolerance for numerical differences')
    args = parser.parse_args()
    args.sbftool = os.path.abspath(args.sbftool)
    logging.basicConfig(level=args.log_level)
    io_files = parse_IO_file(join(args.test_directory,'IO'))
    if run_test(args, args.test_directory, io_files):
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == '__main__':
    main()
