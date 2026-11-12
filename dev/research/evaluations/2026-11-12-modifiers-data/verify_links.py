"""Distinguish new broken Markdown destinations from baseline link debt.

Run from the worktree root. Uses only read-only Git operations.
"""
import importlib.util
from pathlib import Path
import subprocess

root = Path.cwd()
spec = importlib.util.spec_from_file_location('links', root/'dev/tools/check_links.py')
links = importlib.util.module_from_spec(spec)
spec.loader.exec_module(links)
paths = sorted(root.glob('*.md'))
for folder in ('dev', 'bindings', 'univariateML-cpp'):
    paths += sorted((root/folder).rglob('*.md'))
    paths += sorted((root/folder).rglob('*.Rmd'))
new, inherited = [], []
for path in paths:
    if 'build' in path.relative_to(root).parts or '__pycache__' in path.parts:
        continue
    baseline = None
    for _, url in links.destinations(path.read_text()):
        if url.startswith(('#', '//')) or links.urlsplit(url).scheme:
            continue
        destination = links.unquote(links.urlsplit(url).path)
        target = root/destination.lstrip('/') if destination.startswith('/') else path.parent/destination
        if not destination or target.exists():
            continue
        if baseline is None:
            old = subprocess.run(['git', 'show', 'bb460787:'+str(path.relative_to(root))],
                                 capture_output=True, text=True)
            baseline = {u for _, u in links.destinations(old.stdout)} if old.returncode == 0 else set()
        (inherited if url in baseline else new).append((str(path.relative_to(root)), url))
print(f'Inherited broken destinations: {len(inherited)} (also present in bb460787)')
print(f'New broken destinations: {len(new)}')
for item in new:
    print(item)
assert not new
