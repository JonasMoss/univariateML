"""Classify the optional global link-check findings without modifying old files.
Run under /tmp/umvl-cpu/run after dev/tools/check_links.py; the raw output is
read from build/scratch/links.log. Git operations are read-only.
"""
from pathlib import Path
import hashlib,json,re,subprocess
root=Path.cwd();out=Path(__file__).parent;baseline='af824a98d78f2f2325c7a675c20c84c45e6bb826'
raw=(root/'build/scratch/links.log').read_text();findings=[]
for line in raw.splitlines():
 m=re.fullmatch(r'(.+?):(\d+): (.+)',line)
 if m:findings.append(dict(path=m[1],line=int(m[2]),target=m[3]))
files=[]
for name in sorted({r['path'] for r in findings}):
 current=(root/name).read_bytes();old=subprocess.check_output(['git','show',baseline+':'+name])
 files.append(dict(path=name,baseline_identical=current==old,sha256=hashlib.sha256(current).hexdigest()))
result=dict(baseline=baseline,findings=len(findings),all_source_files_unchanged=all(f['baseline_identical'] for f in files),files=files,entries=findings)
(out/'link-baseline-check.json').write_text(json.dumps(result,indent=2)+'\n')
(out/'baseline-links.txt').write_text(raw)
print(json.dumps({k:v for k,v in result.items() if k not in ['files','entries']},indent=2))
if not result['all_source_files_unchanged']:raise SystemExit(1)
