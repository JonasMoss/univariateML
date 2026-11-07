"""Fingerprint the tested core, bindings, dossiers, goldens and check sources.
Run from the repository root; --check verifies the saved frozen state.
"""
import hashlib, pathlib, sys
root=pathlib.Path.cwd()
folder=root/'dev/research/evaluations/2026-11-07-skew-impl-data'
paths=set()
for directory in ('univariateML-cpp/include','univariateML-cpp/tests','bindings/common'):
    for p in (root/directory).rglob('*'):
        if p.is_file() and (p.suffix in ('.hpp','.cpp') or p.name in ('README.md','golden.json')):
            paths.add(p)
for family in ('sstd','st'):
    paths.update((root/'bindings/r/distributions'/family).glob('*'))
for suffix in ('*.cpp','*.py','*.sh'):
    paths.update(folder.glob(suffix))
text=''.join(f'{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.relative_to(root)}\n' for p in sorted(paths) if p.is_file())
target=folder/'source-manifest.sha256'
if '--check' in sys.argv:
    assert target.read_text()==text, 'tested source state changed'
else:
    target.write_text(text)
print(f'{len(text.splitlines())} frozen source files; manifest sha256 {hashlib.sha256(text.encode()).hexdigest()}')
