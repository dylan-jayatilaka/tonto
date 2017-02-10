#!/usr/bin/env python3
from pathlib import Path
import logging
from tempfile import gettempdir
import os
import sys
import shutil
import subprocess
import difflib
import time
from math import isclose

log = logging.getLogger('test')
prefixes_to_ignore = ['Wall-clock', 'CPU time', 
    ' Version', ' Platform', 'Timer', ' Build-date']

test_categories = ['short', 'cx', 'long', 'geminal', 'relativistic']

def check_line(line):
    return not any(map(line.startswith, prefixes_to_ignore))


def get_lines(filename):
    """Read all lines from a file, returning a
    list of line contents

    Arguments:
    filename -- name/path of the file to read
    """
    lines = []
    with Path(filename).open() as f:
        for line in f:
            if check_line(line):
                lines.append(line)
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
    return all(
            equivalent(t1, t2, **kwargs) 
            for t1, t2 in zip(line1.strip('+- ').split(), line2.strip('+- ').split()))


def diff_files(file1, file2, print_diffs=False, **kwargs):
    lines1 = get_lines(file1)
    lines2 = get_lines(file2)
    diff = list(difflib.ndiff(lines1, lines2))
    #print(''.join(diff))
    del1 = [x for x in diff if x.startswith('-')]
    del2 = [x for x in diff if x.startswith('+')]
    diffs = [check_numbers(l1, l2, **kwargs) for l1, l2 in zip(del1, del2)]
    if all(diffs):
        return None
    else:
        return [''.join((a, b)) for a, b, d in zip(del1, del2, diffs) if not d]


class working_directory:
    """ Context manager for temporarily changing the current working directory. """
    old_directory = None

    def __init__(self, directory, create=False):
        if directory:
            self.directory = os.path.expanduser(directory)
            if not os.path.exists(self.directory) and create:
                os.mkdir(self.directory)
        else:
            self.directory = None


    def __enter__(self):
        self.old_directory = os.getcwd()
        if self.directory:
            log.debug('cwd: %s', self.directory)
            os.chdir(self.directory)

    def __exit__(self, exc_type, exc_val, exc_tb):
        os.chdir(self.old_directory)

def temp_test_dir(testname):
    name = gettempdir() + '/' + testname
    log.debug('temp_test_dir = %s', name)
    return name

def parse_IO_file(path):
    io_files = {
        'input': set(['stdin']),
        'output': set(['stdout']),
        'delete': set(),
    }

    if path.exists():
        with path.open() as f:
            for line in f:
                tokens = line.split(':')
                io_files[tokens[0].strip()].add(tokens[1].strip())
    return io_files


def compare_outputs(f1, f2, cmd=None):
    if cmd:
        args = [cmd, f1, f2]
        return subprocess.run(args, check=True)
    else:
        log.debug('Using builtin diff')
        d = diff_files(f1, f2)
        return d is None


def run_program(command, test_dir, compare_prog, io_files, basis_sets):
    kwargs = {
        'shell': True,
        'universal_newlines': True,
        'check': True,
    }

    timings = {}
    exec_dir = temp_test_dir(test_dir.name)
    timings['start'] = time.time() 
    with working_directory(exec_dir, create=True):
        for path in io_files['input']:
            shutil.copy(Path(test_dir, path).absolute(), '.')
        timings['cp_input'] = time.time() - timings['start']
        completed = subprocess.run([command, '-b', basis_sets], **kwargs)
        timings['tonto'] = time.time() - sum(t for t in timings.values())
        files_equivalent = []
        if completed:
            log.debug('Outputs to check %s', io_files['output'])
            for path in io_files['output']:
                canonical = str(Path(test_dir, path).absolute())
                log.debug('Comparing %s to %s', path, canonical)
                d = compare_outputs(canonical, path,
                                    cmd=compare_prog)
                log.debug('Same file: %s', d)
                files_equivalent.append(d)
        timings['diffs'] = time.time() - sum(t for t in timings.values())
        success = completed and all(files_equivalent)
        for path in io_files['output']:
            shutil.copy(Path('.', path).absolute(), 
                        Path(test_dir, path + '.bad').absolute())
        timings['cp_output'] = time.time() - sum(t for t in timings.values())
        log.debug('Time spent:')
        for k, v in timings.items():
            if k != 'start':
                log.debug('%s: \t %f s', k, v)
    return completed

def main():
    """Show the differences between two test files
    """
    import argparse
    import os
    parser = argparse.ArgumentParser()
    parser.add_argument('--program', '-p', default='tonto',
                        help='Program to use to run the test jobs i.e. tonto')
    parser.add_argument('--test-directory', '-t', default='.',
                        help='Directory in which tests are located')
    parser.add_argument('--compare-program', '-c', default=None,
                        help='diff style program to compare outputs')
    parser.add_argument('--log-level', default='ERROR',
                        help='Log level for running tests')
    parser.add_argument('--basis-sets', default='.',
                        help='Basis sets directory')
 

    args = parser.parse_args()
    logging.basicConfig(level=args.log_level)
    test_dir = Path(args.test_directory)
    io_files = parse_IO_file(test_dir / 'IO')
    if run_program(args.program, test_dir, args.compare_program, io_files,
                   args.basis_sets):
        sys.exit(0)
    else:
        sys.exit(1)

if __name__ == '__main__':
    main()
