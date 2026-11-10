"""Separate unchanged global link findings from this job's authored documents."""
import hashlib,importlib.util,json,pathlib,re,subprocess,sys
HERE=pathlib.Path(__file__).resolve().parent
ROOT=HERE.parents[3]
spec=importlib.util.spec_from_file_location('links',ROOT/'dev/tools/check_links.py')
links=importlib.util.module_from_spec(spec);spec.loader.exec_module(links)
log=pathlib.Path(sys.argv[1]) if len(sys.argv)>1 else ROOT/'build/scratch/audit-support-links.log'
errors=[]
for line in log.read_text().splitlines():
 m=re.match(r'^(.*?):(\d+): (.*)$',line)
 if m:errors.append(dict(path=m[1],line=int(m[2]),destination=m[3]))
unchanged=[]
for path in sorted({r['path'] for r in errors}):
 original=subprocess.check_output(['git','show','594c491dd495892b9cffd8977fcc9809fcbec151:'+path],cwd=ROOT)
 current=(ROOT/path).read_bytes()
 assert original==current,('new/modified file has a broken link',path)
 unchanged.append(dict(path=path,sha256=hashlib.sha256(current).hexdigest()))
files=[ROOT/'univariateML-cpp/include/cppunivariate/distributions'/f/'README.md' for f in ['emg','triang','gev']]
report=HERE.parent/'2026-11-09-audit-support-report.md'
if report.exists():files.append(report)
checked=0
for file in files:
 for offset,url in links.destinations(file.read_text()):
  if url.startswith(('#','//')) or links.urlsplit(url).scheme:continue
  path=links.unquote(links.urlsplit(url).path)
  if not path:continue
  target=ROOT/path.lstrip('/') if path.startswith('/') else file.parent/path
  assert target.exists(),(file,url)
  checked+=1
result=dict(job_documents='PASS',job_files=[str(f.relative_to(ROOT)) for f in files],local_destinations=checked,report_checked=report.exists(),global_existing_findings=len(errors),unchanged_global_files=unchanged,baseline='594c491dd495892b9cffd8977fcc9809fcbec151')
(HERE/'job-links-validation.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(result,indent=2))
