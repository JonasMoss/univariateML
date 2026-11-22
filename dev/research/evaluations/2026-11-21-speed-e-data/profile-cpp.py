"""Inventory CTest registrations and optionally time every target and doctest case."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import csv
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time


def timed(command, cwd):
    started = time.monotonic()
    with tempfile.TemporaryFile() as output:
        child = subprocess.Popen(command, cwd=cwd, stdout=output, stderr=subprocess.STDOUT)
        _, status, usage = os.wait4(child.pid, 0)
        child.returncode = os.waitstatus_to_exitcode(status)
        wall = time.monotonic() - started
        output.seek(0)
        text = output.read().decode(errors='replace')
    if child.returncode:
        raise RuntimeError(f'{command} returned {child.returncode}\n{text}')
    return usage.ru_utime + usage.ru_stime, wall, text


def cases(test):
    command = test['command']
    properties = {p['name']: p['value'] for p in test['properties']}
    cwd = properties.get('WORKING_DIRECTORY', str(Path(command[0]).parent))
    labels = properties.get('LABELS', [])
    if test['name'] == 'diagnostics_tests':
        return command, cwd, labels, []
    text = subprocess.check_output([*command, '--list-test-cases', '--no-colors',
                                    '--order-by=file'], cwd=cwd, text=True)
    lines = text.splitlines()
    bar = '=' * 79
    first = lines.index(bar)
    last = lines.index(bar, first + 1)
    assert int(re.search(r'unskipped test cases passing the current filters: (\d+)', text)[1]) == last - first - 1
    return command, cwd, labels, lines[first + 1:last]


def timing(test, command, cwd, number=None, case=None):
    args = [] if not number else [f'--first={number}', f'--last={number}']
    if test != 'diagnostics_tests':
        args += ['-d', '--no-colors', '--order-by=file']
    cpu, wall, text = timed([*command, *args], cwd)
    assertions = re.search(r'assertions:\s*(\d+)', text)
    if number:
        assert re.findall(r'^TEST CASE:  (.*)$', text, re.M) == [case], text
    return round(cpu, 6), round(wall, 6), int(assertions[1]) if assertions else ''


def write(path, header, rows):
    with path.open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(header)
        writer.writerows(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('prefix', type=Path)
    parser.add_argument('--jobs', type=int, default=4)
    parser.add_argument('--time', action='store_true')
    args = parser.parse_args()
    tests = json.loads(subprocess.check_output(
        ['ctest', '--test-dir', str(args.build), '--show-only=json-v1']))['tests']
    inventory = []
    targets = []
    singles = []
    for test in tests:
        name = test['name']
        command, cwd, labels, selected = cases(test)
        inventory.extend((name, ';'.join(labels), case) for case in selected or [''])
        targets.append((name, command, cwd))
        singles.extend((name, command, cwd, i, case) for i, case in enumerate(selected, 1))
    write(args.prefix.with_suffix('.inventory.csv'), ['target', 'labels', 'case'], inventory)
    if args.time:
        def target(row):
            name, command, cwd = row
            return name, *timing(name, command, cwd)

        def single(row):
            name, command, cwd, number, case = row
            return name, case, *timing(name, command, cwd, number, case)

        history = args.build/'Testing/Temporary/CTestCostData.txt'
        costs = {}
        if history.is_file():
            for line in history.read_text().splitlines():
                if line == '---':
                    break
                name, _, cost = line.split()
                costs[name] = float(cost)
        singles.sort(key=lambda row: -costs.get(row[0], 0))
        targets.sort(key=lambda row: -costs.get(row[0], 0))
        with ThreadPoolExecutor(args.jobs) as pool:
            write(args.prefix.with_suffix('.cases.csv'), ['target', 'case', 'cpu', 'wall', 'assertions'],
                  pool.map(single, singles))
            write(args.prefix.with_suffix('.targets.csv'), ['target', 'cpu', 'wall', 'assertions'],
                  pool.map(target, targets))


if __name__ == '__main__':
    main()
