"""Check final deliverables, source hashes and completed local test logs."""
import csv, hashlib, json, re, subprocess
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4]
OUT=Path(__file__).resolve().parent
report=ROOT/'dev/research/evaluations/2026-11-04-dqm-a-report.md'
revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
assert revision=='b92c6189497f00b5917c8da8c29d07cc494f896d'
text=report.read_text(); assert revision in text
checks=json.loads((OUT/'checks.json').read_text())
assert checks['metadata_rows']==31 and len(checks['quadratures'])==11
for p,expected in checks['source_sha256'].items():
    assert hashlib.sha256((ROOT/p).read_bytes()).hexdigest()==expected,p
rows=list(csv.DictReader((OUT/'classification.csv').open()))
for r in rows:
    assert f"| {r['id']} | {r['free']} |" in text
    assert r['argument'] in text
cpp=(ROOT/'build/scratch/dqm-a-cpp-dev.log').read_text()
rlog=(ROOT/'build/scratch/dqm-a-r-dev.log').read_text()
assert '100% tests passed, 0 tests failed out of 106' in cpp
rsummary=re.findall(r'\[ FAIL 0 \| WARN 0 \| SKIP 154 \| PASS 20612 \]',rlog)
assert rsummary
assert 'requires full test tier (154)' in rlog
status=subprocess.check_output(['git','status','--short','--untracked-files=all'],cwd=ROOT,text=True)
for line in status.splitlines():
    path=line[3:]
    assert line.startswith('?? ') and (path=='dev/research/evaluations/2026-11-04-dqm-a-report.md' or path.startswith('dev/research/evaluations/2026-11-04-dqm-a-data/')),line
result=dict(source_revision=revision,machine='local workstation; no simbox',slots='/tmp/umvl-cpu/run or run4; wrappers apply nice -n 10 and BLAS/OpenMP thread caps',cpp_dev=dict(command='/tmp/umvl-cpu/run4 just cpp-test',exit_status=0,passed_shards=106,failed_shards=0,ctest_elapsed_seconds=64.65),r_dev=dict(command='/tmp/umvl-cpu/run4 just test',exit_status=0,fail=0,warn=0,skip=154,passed=20612,skip_reason='requires full test tier',runner_elapsed_seconds=150.109),research_checks=dict(command='/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-04-dqm-a-data/check_classification.py',exit_status=0,rows=31,quadratures=11,symbolic_identities=3),full_tier='not run: research-only change; no fitting, dpqr or test infrastructure changes',artifact_audit='PASS: rows match report; source hashes unchanged; dev logs green; git status contains only requested research deliverables',git_writes=False,report_sha256=hashlib.sha256(report.read_bytes()).hexdigest(),csv_sha256=hashlib.sha256((OUT/'classification.csv').read_bytes()).hexdigest())
# Create the link target before checking all Markdown references.
(OUT/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
links=subprocess.run(['python3','dev/tools/check_links.py'],cwd=ROOT,capture_output=True,text=True)
expected_sources={'dev/research/evaluations/2026-10-31-inference-6-report.md','dev/research/evaluations/2026-11-03-boundary-lit-report.md','univariateML-cpp/notes/inference.md'}
failures=links.stdout.splitlines()[1:]
assert links.returncode==1 and len(failures)==5 and {line.split(':')[0] for line in failures}==expected_sources,links.stdout
for path in expected_sources:
    assert subprocess.check_output(['git','show',f'HEAD:{path}'],cwd=ROOT)==(ROOT/path).read_bytes()
# Validate only authored Markdown links, including CSV/evidence targets.
import importlib.util
spec=importlib.util.spec_from_file_location('local_links',ROOT/'dev/tools/check_links.py')
mod=importlib.util.module_from_spec(spec);spec.loader.exec_module(mod)
from urllib.parse import urlsplit,unquote
count=0
for offset,url in mod.destinations(text):
    if url.startswith(('#','//')) or urlsplit(url).scheme:continue
    target=unquote(urlsplit(url).path)
    assert (ROOT/target.lstrip('/') if target.startswith('/') else report.parent/target).exists(),url
    count+=1
result['local_link_audit']=dict(command='/tmp/umvl-cpu/run python3 dev/tools/check_links.py (executed inside artifact audit slot)',exit_status=links.returncode,result=links.stdout.strip(),classification='five pre-existing broken links; all three source files byte-identical to HEAD; out of scope')
result['authored_report_link_audit']=dict(result='PASS',local_destinations=count)
result['artifact_pane_display']=dict(command='/home/jonas/Files/work/misc/codex-math-web/bin/show dev/research/evaluations/2026-11-04-dqm-a-report.md',result='viewer HTTP 403: this worktree is outside every configured readable root; no file copied outside worktree')
(OUT/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print('PASS: final artifact, source-hash, scope, test-log and authored-link audit')
print(f'Authored report: {count} local links, zero broken')
print('Global link inventory: five pre-existing broken links recorded in validation.json')
