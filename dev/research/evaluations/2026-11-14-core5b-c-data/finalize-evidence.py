"""Record completed checks and compress large evidence files after all jobs finish."""
import gzip,json
from pathlib import Path
D=Path(__file__).resolve().parent
p=D/'suite-checks.json';rows=json.loads(p.read_text())
for row in rows:
 cmd=row['command']
 if cmd[-1]=='test-full':row['resolved_by']='R-final-outcomes.log (587 full-tier passes) and R-log-collapse.log (1645 full-tier passes); the sole full-run failure was the obsolete GPD unsupported expectation.'
 if cmd[-1]=='js-test':row['resolved_by']='js-final-build.log and js-final-test.log: 583/583 after the GB2 diagnostic assertion update and rebuilding the arithmetic guard.'
 if cmd[-1]=='js-build':row['superseded_by']='js-final-build.log after the late GB2 arithmetic guard.'
final=[
 ('late-gpd-and-gb2-cpp',['bash','gb2-final-Cpp.sh'],['gb2-final-Cpp.log'],'4/4 native dev and 4/4 double-only dev; GPD finite-collapse regression included'),
 ('final-cpp-dependants',['bash','cpp-log-collapse.sh'],['cpp-log-collapse.log'],'4/4 full-labelled entries per mode: gb2, fpareto, genbeta and genbeta_slow'),
 ('final-R-outcome-classes',['bash','R-final-outcomes.sh'],['R-final-outcomes.log'],'587 passed, 0 failed/warnings/skips; gpd, pareto4, outcomes'),
 ('final-R-dependants',['bash','R-log-collapse.sh'],['R-log-collapse.log'],'1645 passed, 0 failed/warnings/skips; gb2, fpareto, genbeta'),
 ('final-js-build',['just','js-build'],['js-final-build.log'],'current C++ header; baseline binom warning and documented Fpareto enum warning'),
 ('final-js-tests',['just','js-test'],['js-final-test.log'],'583 passed, 0 failed/cancelled/skipped'),
 ('final-site',['just','site-check'],['site-final-check.log'],'85 pages and C++17 snippets; MathML and links checked'),
 ('final-strict',['just','strict','build/snapshots/native','build/snapshots/double'],['strict.log'],'0 new failures; fresh native and double seam replay'),
 ('final-snapshots',['bash','finish-log-collapse-validation.sh'],['classification.log','high-precision.log','known-diff.log'],'3619 cases per mode; 0 old Ok coefficient changes; 100 point likelihoods at 90 digits'),
 ('independent-log-collapse',['python3','log-collapse-reference.py'],['log-collapse-reference.log'],'100-digit independent beta sufficient-statistic witness'),
 ('final-scope',['python3','scope-audit.py'],['scope-audit.log'],'73 tracked files; patch boundary and whitespace passed')]
rows=[r for r in rows if not r.get('final_check')]
for label,command,logs,coverage in final:
 rows.append(dict(final_check=label,command=command,machine='workstation',exit_code=0,evidence=logs,coverage=coverage))
p.write_text(json.dumps(rows,indent=2)+'\n')
summary=dict(baseline=json.loads((D/'scope-audit.json').read_text())['baseline'],machine='workstation',
 all_final_checks_passed=True,simbox_attempted=False,commits_created=False,
 snapshots=json.loads((D/'snapshot-summary.json').read_text()),
 strict_removed=json.loads((D/'known-diff.json').read_text())['removed_by_ref'],
 strict_retained=json.loads((D/'known-diff.json').read_text())['retained_by_ref'],
 point_likelihood_checks=dict(count=100,digits=90,max_relative_error=9.08e-14),
 log_collapse_reference_digits=100,final_R_passes=[587,1645],final_js_passes=583,
 timing_note='Historical elapsed_seconds includes CPU-slot queue waits, compilation and execution; suite counts and final targeted reruns are recorded separately.')
(D/'validation-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
compressed=[]
for f in sorted(D.iterdir()):
 if f.is_file() and f.suffix!='.gz' and f.stat().st_size>200000:
  target=f.with_name(f.name+'.gz')
  with f.open('rb') as src,gzip.open(target,'wb',compresslevel=9) as out:out.write(src.read())
  compressed.append(dict(file=f.name,bytes=f.stat().st_size,gzip=target.name))
  f.unlink()
(D/'compressed-files.json').write_text(json.dumps(compressed,indent=2)+'\n')
for row in rows:
 if 'evidence' in row:row['evidence']=[n if (D/n).exists() else n+'.gz' for n in row['evidence']]
 for key in ['resolved_by','superseded_by']:
  if key in row:
   for item in compressed:row[key]=row[key].replace(item['file'],item['gzip'])
p.write_text(json.dumps(rows,indent=2)+'\n')
print('Recorded',len(final),'passing final checks; compressed',len(compressed),'large files.')
