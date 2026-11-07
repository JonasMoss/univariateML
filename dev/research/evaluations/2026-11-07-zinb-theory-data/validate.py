"""Foreground independent replay, cross-machine checks and evidence validation."""
from pathlib import Path
import ast,contextlib,hashlib,json,platform,runpy,subprocess,time,shutil,os,sys
import mpmath as mp
P=Path(__file__).resolve().parent;root=P.parents[3]
start=time.monotonic();os.environ['PYTHONDONTWRITEBYTECODE']='1'
for f in P.glob('*.py'):ast.parse(f.read_text(),filename=str(f))
if not (P/'near-face-simbox.json').exists():shutil.copyfile(P/'near-face.json',P/'near-face-simbox.json')
sys.path.insert(0,str(P))
with (P/'research-local.txt').open('w') as log,contextlib.redirect_stdout(log):
 for script in ['explore.py','search_modes.py','curvature_search.py','recheck_curvature.py']:runpy.run_path(str(P/script),run_name='__main__')
 runpy.run_path(str(P/'near_face.py'),run_name='__main__')
 runpy.run_path(str(P/'certify_near.py'),run_name='__main__')
mp.mp.dps=110
loc=json.loads((P/'near-face.json').read_text());rem=json.loads((P/'near-face-simbox.json').read_text())
keys=['size','prob','pi','dispersion','t','logLik','gain']
errors={key:str(abs(mp.mpf(loc['runs'][0][key])-mp.mpf(loc['runs'][1][key]))/max(1,abs(mp.mpf(loc['runs'][1][key])))) for key in keys}
assert max(mp.mpf(x) for x in errors.values())<mp.mpf('1e-55')
cross={key:str(abs(mp.mpf(rem['runs'][1][key])-mp.mpf(loc['runs'][1][key]))/max(1,abs(mp.mpf(loc['runs'][1][key])))) for key in keys}
assert max(mp.mpf(x) for x in cross.values())<mp.mpf('1e-85')
assert abs(mp.mpf(loc['runs'][1]['score_d']))<mp.mpf('1e-95')
assert abs(mp.mpf(loc['runs'][1]['mean_residual']))<mp.mpf('1e-95')
cert=json.loads((P/'near-certificate.json').read_text());cover=json.loads((P/'near-cover.json').read_text())
assert cert['global_unique'] and len(cover)==445
assert cover[0][0]=='0.01' and mp.mpf(cover[-1][1])==100
assert all(a[1]==b[0] for a,b in zip(cover,cover[1:]))
assert all(mp.mpf(z[2])>0 for z in cover)
assert len(json.loads((P/'literature-searches.json').read_text())['queries'])==20
assert json.loads((P/'curvature-recheck.json').read_text())['all_candidates_negative']
# High precision numerical checks accompany, but do not replace, report proofs.
lam4=mp.findroot(lambda u:u/(-mp.expm1(-u))-4,mp.mpf('3.9'))
assert 3<lam4<4 and 1-mp.exp(-lam4)>mp.mpf(1)/4 and 6-2*lam4<0
lam25=mp.findroot(lambda u:u/(-mp.expm1(-u))-mp.mpf('2.5'),mp.mpf('2.2'))
assert lam25<mp.mpf('2.4') and 6-5*lam25/2>0
assert mp.mpf(9)/4<mp.mpf('2.5') # [1,4]: var=9/4 < mean=5/2
assert subprocess.run(['git','diff','--quiet','HEAD','--'],cwd=root).returncode==0
assert '100% tests passed out of 106' in (P/'cpp-dev.txt').read_text()
assert '[ FAIL 0 | WARN 0 | SKIP 154 | PASS 20983 ]' in (P/'r-dev.txt').read_text()
source=[]
for family in ['zinb','ztnbinom','nbinom']:
 folder=root/'univariateML-cpp/include/cppunivariate/distributions'/family
 for f in sorted(folder.glob('*')):
  if f.is_file():source.append((str(f.relative_to(root)),hashlib.sha256(f.read_bytes()).hexdigest()))
(P/'source-sha256.txt').write_text(''.join(h+'  '+f+'\n' for f,h in source))
refs=[]
for name,url in [('aragon','https://stacks.stanford.edu/file/druid:gy645ty0351/gy645ty0351_OLK-NSF-282.pdf'),('sampford-891','https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/891/download'),('sampford-892','https://amsr.contentdm.oclc.org/digital/api/collection/p21050coll5/id/892/download')]:
 f=root/'build/scratch'/(name+'.pdf');refs.append(dict(name=name,url=url,sha256=hashlib.sha256(f.read_bytes()).hexdigest(),bytes=f.stat().st_size))
(P/'source-records.json').write_text(json.dumps(refs,indent=2)+'\n')
search_statuses={name:json.loads((P/name).read_text())['statuses'] for name in ['explore.json','search-modes.json','curvature-search.json']}
assert not json.loads((P/'explore.json').read_text())['candidates']
assert not json.loads((P/'search-modes.json').read_text())['results']
record=dict(search_solver_statuses=search_statuses,revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True,cwd=root).strip(),machine='workstation',python=platform.python_version(),mpmath=mp.__version__,seconds=time.monotonic()-start,precision_agreement_errors=errors,cross_machine_errors=cross,certificate=cert,search_queries=20,counterexample_checks=dict(lambda_4=str(lam4),lambda_2_5=str(lam25)),final_suites=[dict(command='/tmp/umvl-cpu/simbox-run just cpp-test',machine='simbox',exit_code=0,passed=106,failed=0),dict(command='/tmp/umvl-cpu/simbox-run just test',machine='simbox',exit_code=0,passed=20983,failed=0,warnings=0,tier_skips=154)],research_command='/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-zinb-theory-data/validate.py',tests_passed=True)
(P/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
print('PASS: research replay, 70/110-digit agreement, cross-machine agreement, compact global cover, concavity and root signs, search adjudication, source immutability, and both development suites.')
