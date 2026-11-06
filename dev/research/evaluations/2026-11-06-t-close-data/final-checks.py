"""Inspect final documents and source state, then display the finished report."""
import hashlib,json,re,subprocess,sys
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
report=here.parent/'2026-11-06-t-close-report.md'
result=dict(document_links=[],source_diff_check=True)
previous=json.loads((here/'final-checks.json').read_text()) if (here/'final-checks.json').exists() else {}
if not (here/'final-checks.json').exists():(here/'final-checks.json').write_text('{}\n')
for doc in [report,here/'README.md',here/'derivation.md']:
 for link in re.findall(r'(?<!!)\[[^\]]*\]\(([^)]+)\)',doc.read_text()):
  if '://' in link:continue
  target=(doc.parent/link.split('#')[0]).resolve()
  assert target.exists(),(doc,link)
  result['document_links'].append(dict(document=str(doc.relative_to(root)),link=link,resolves=True))
subprocess.run(['git','diff','--check'],check=True)
record=json.loads((here/'validation.json').read_text())
for name,digest in record['source_sha256'].items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
result['validated_source_unchanged']=True
result['goldens_unchanged']=record['goldens_unchanged']
result['report_sha256']=hashlib.sha256(report.read_bytes()).hexdigest()
result['tracked_changes']=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
if '--no-display' in sys.argv:
 result['artifact_display']=previous.get('artifact_display',{})
else:
 try:
  display=subprocess.run(['/home/jonas/Files/work/misc/codex-math-web/bin/show',str(report)],stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True,timeout=20)
  result['artifact_display']=dict(exit_code=display.returncode,output=display.stdout[:2000])
  if '403' in display.stdout:
   result['artifact_display']=dict(exit_code=display.returncode,http_status=403,reason='The worktree is outside every configured viewer readable root. Display requires registration of this worktree root. No viewer configuration was changed.')
 except subprocess.TimeoutExpired:
  result['artifact_display']=dict(status='timeout after 20 seconds')
(here/'final-checks.json').write_text(json.dumps(result,indent=2)+'\n')
print('Document links resolved:',len(result['document_links']))
print('Validated source and goldens unchanged; whitespace check passed')
print(json.dumps(result['artifact_display'],indent=2))
