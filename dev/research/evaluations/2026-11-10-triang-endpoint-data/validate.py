import csv,json,pathlib,math,hashlib,platform,subprocess
import mpmath as mp
mp.mp.dps=400
HERE=pathlib.Path(__file__).resolve().parent
cases={c['index']:c for c in json.loads((HERE/'cases.json').read_text())}
refs={c['index']:c for c in json.loads((HERE/'reference.json').read_text())}
summary={}
for mode,tol in [('',mp.mpf('1e-12')),('-contract',mp.mpf('1e-10'))]:
 rows={}
 for phase in ['before','after']:
  rows[phase]={int(r[0]):r for r in csv.reader((HERE/(phase+mode+'.csv')).open())}
  exhaustive={int(r[0]):r for r in csv.reader((HERE/(phase+mode+'-exhaustive.csv')).open())}
  # Pruning may save work; statistical results must still agree exactly.
  assert {i:r[:7]+r[8:] for i,r in rows[phase].items()}=={i:r[:7]+r[8:] for i,r in exhaustive.items()}
  assert len(rows[phase])==168
  for id,r in rows[phase].items():
   if int(r[2]):continue
   data=list(map(mp.mpf,cases[id]['data']));a,b,c=map(lambda v:mp.mpf(float(v)),r[4:7])
   assert all(a<=v<=b for v in data)
   total=mp.fsum(mp.log(2)-mp.log(b-a)+(mp.log(v-a)-mp.log(c-a) if v<c else 0)+(mp.log(b-v)-mp.log(b-c) if v>c else 0) for v in data)
   assert abs(total-mp.mpf(float(r[3])))<mp.mpf('1e-11')*(1+abs(total))
 for id,ref in refs.items():
  r=rows['after'][id]
  assert abs(mp.mpf(ref['continuous_upper'])-mp.mpf(ref['continuous_loglik']))<mp.mpf('1e-110')
  assert list(map(float,r[4:7]))==ref['best_public_point']
  assert max(map(mp.mpf,ref['coefficient_relative_errors']))<mp.mpf('1e-10')
  # Reference justified stops: even the best binary64 point is outside
  # requested likelihood tolerance. All other modes have smaller upper bounds.
  expected_ok=mp.mpf(ref['relative_loglik_loss'])<=tol
  assert (int(r[2])==0)==expected_ok,(id,mode,r[2])
 changes=[dict(index=id,before=int(rows['before'][id][2]),after=int(r[2])) for id,r in rows['after'].items() if r!=rows['before'][id]]
 assert {r['index'] for r in changes}==({8168,8213} if not mode else {8165,8168,8172,8213})
 assert all(rows['before'][id][3:]==rows['after'][id][3:] for id in rows['before'])
 summary[mode or 'defaults']=dict(cases=168,before_ok=sum(r[2]=='0' for r in rows['before'].values()),after_ok=sum(r[2]=='0' for r in rows['after'].values()),changes=changes,pruning_statistical_payloads_equal=True,all_numeric_payloads_unchanged=True)
summary['environment']=dict(platform=platform.platform(),python=platform.python_version(),mpmath=mp.__version__,compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],R=subprocess.check_output(['Rscript','-e','cat(R.version.string, " BH ",as.character(packageVersion("BH")))'],text=True),base=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip())
summary['source_sha256']={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in [pathlib.Path('univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp'),HERE/'cases.json',HERE/'reference.py']}
(HERE/'validation.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
