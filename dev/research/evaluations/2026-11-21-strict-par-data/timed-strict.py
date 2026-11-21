import pathlib
import re
import sys

source, dest, logs, profile, records = map(pathlib.Path, sys.argv[1:6])
uncached = sys.argv[6:] == ['--no-cache']
recipe = source.read_text()
start = recipe.index('\nstrict ')
end = recipe.index('\n\n', start)
section = recipe[start:end]

def timed(m):
    script, args = m.groups()
    command = f'python3 $v/{script}'
    if uncached and script in ('cert-replay.py', 'strict-seam.py'):
        args += ' --no-cache'
    if script == 'cert-replay.py':
        args = f'{args} --records "{records}"'
        if str(profile) != '-':
            command = f'python3 "{profile}" $v/{script}'
    return (f'    /usr/bin/time -o "{logs}/{script}.time" -f "%e %U %S %x" '
            f'{command}{args} || status=1')

section, count = re.subn(r'^    python3 \$v/(\S+)(.*) \|\| status=1$', timed, section, flags=re.M)
assert count == 6, count
# Keep justfile_directory() pointing at the measured tree.
recipe = recipe.replace('repo_root := justfile_directory()', f'repo_root := "{source.parent}"', 1)
start = recipe.index('\nstrict ')
end = recipe.index('\n\n', start)
dest.write_text(recipe[:start] + section + recipe[end:])
