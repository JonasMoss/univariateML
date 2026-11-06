"""Restore only standalone headers from the tested base, using read-only Git."""
from pathlib import Path
import subprocess
revision = '164d41784756606a48ddc0cedc67752acab4050b'
root = Path('univariateML-cpp/include')
destination = Path('build/scratch/st-baseline-include')
paths = subprocess.check_output(['git', 'ls-tree', '-r', '--name-only', revision, str(root)], text=True).splitlines()
for name in paths:
    if not name.endswith('.hpp'):
        continue
    target = destination / Path(name).relative_to(root)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(subprocess.check_output(['git', 'show', revision + ':' + name]))
print(destination)
