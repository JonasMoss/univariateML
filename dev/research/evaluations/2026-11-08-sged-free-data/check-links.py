"""Check the two Markdown files changed by TASK-116.44 with the repo parser."""
from pathlib import Path
from urllib.parse import unquote, urlsplit
import importlib.util

root=Path(__file__).resolve().parents[4]
spec=importlib.util.spec_from_file_location('repo_links',root/'dev/tools/check_links.py')
parser=importlib.util.module_from_spec(spec)
spec.loader.exec_module(parser)
files=[root/'dev/research/evaluations/2026-11-08-sged-free-report.md',
       root/'univariateML-cpp/include/cppunivariate/distributions/sged/README.md']
checked=0
errors=[]
for file in files:
    text=file.read_text()
    for offset,url in parser.destinations(text):
        if url.startswith(('#','//')) or urlsplit(url).scheme:
            continue
        path=unquote(urlsplit(url).path)
        if not path:
            continue
        checked+=1
        target=root/path.lstrip('/') if path.startswith('/') else file.parent/path
        if not target.exists():
            errors.append(f'{file.relative_to(root)}:{text[:offset].count(chr(10))+1}: {url}')
print(f'{len(files)} changed Markdown files; {checked} local destinations; {len(errors)} broken')
for error in errors:
    print(error)
raise SystemExit(bool(errors))
