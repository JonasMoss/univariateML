"""Collect completed tests and benchmark records; fail if final sources drift."""
import csv,gzip,hashlib,json,math,re,statistics,subprocess
from pathlib import Path
root=Path.cwd();out=Path(__file__).resolve().parent
base='52820d8c9094a256aefd2d00223e9419350b22ed'
assert subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()==base
log=(root/'build/scratch/ged-perf-final-cpp.log').read_text()+(root/'build/scratch/ged-perf-final-suites.log').read_text()+(root/'build/scratch/ged-perf-site.log').read_text()
commands=['just cpp-test','env UMVL_TEST_TIER=full just cpp-test','just cpp-test-no-long-double','just test','just test-full','just site-check']
for c in commands:assert 'GREEN '+c+'\n' in log,c
files=['univariateML-cpp/include/cppunivariate/distributions/ged/'+f for f in ['mle.hpp','shape_certificate.hpp','free_shape_bounds.hpp','test.cpp','README.md','golden.json']]
hashes={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in files}
for name in ['mle.hpp','shape_certificate.hpp','free_shape_bounds.hpp']:
 assert (root/'build/scratch/ged-perf/after/include/cppunivariate/distributions/ged'/name).read_bytes()==(root/'univariateML-cpp/include/cppunivariate/distributions/ged'/name).read_bytes(),('benchmark source drift',name)
for p,h in hashes.items():assert log.count(h+'  '+p)==4,('final suite source drift',p)
changed=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
assert set(changed)<=set(files),changed
for p in files:
 if p.endswith('golden.json'):assert (root/p).read_bytes()==subprocess.check_output(['git','show',base+':'+p])
test=files[3];assert (root/test).read_bytes().startswith(subprocess.check_output(['git','show',base+':'+test]))
cpp_counts=list(map(int,re.findall(r'100% tests passed.*?out of (\d+)',log)))
assert len(cpp_counts)==3,cpp_counts
r_counts=[dict(zip(['fail','warn','skip','pass'],map(int,row))) for row in re.findall(r'^\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]$',log,re.M)]
assert len(r_counts)==2 and all(r['fail']==r['warn']==0 for r in r_counts),r_counts
site=[s for s in log.splitlines() if s.startswith('PASS')]
assert any('99 family pages' in s for s in site),site
checks=(root/'build/scratch/ged-perf-checks-final.log').read_text()
assert '5764 | 5764 passed | 0 failed' in checks
assert checks.count('"violations": 0')==3
bounds=json.JSONDecoder().raw_decode((root/'build/scratch/ged-perf-bounds.log').read_text())[0]
assert bounds==dict(precision=80,rows=108,enclosure_checks=756,violations=0)
comparison=json.loads((out/'comparison.json').read_text())
assert not comparison['stop_changes'] and not comparison['coefficient_errors']
timings=[];profiles=[];fits={};scope={};counter={};cells={}
for variant in ['before','after']:
 rows=[s.split() for s in (out/(variant+'-time.txt')).read_text().splitlines()]
 assert len(rows)==24
 for r in rows:
  assert r[4:6]==['0','1'],r
  fits.setdefault((int(r[1]),int(r[2]),variant),[]).append(r)
 for line in (out/(variant+'-profile.txt')).read_text().splitlines():
  r=line.split();key=(int(r[1]),int(r[2]),variant)
  if r[0]=='profile':scope.setdefault(key,{})[r[3]]=dict(calls=int(r[4]),ms=float(r[5]))
  elif r[0]=='counter':counter.setdefault(key,{})[r[3]]=int(r[4])
  elif r[0]=='cell':cells.setdefault(key,[]).append(tuple(map(float,r[3:5])))
labels=['mean,sd,nu','sd,nu','mean,nu','nu']
for n in [500,10000]:
 for mask in range(4):
  b=fits[n,mask,'before'];a=fits[n,mask,'after']
  assert all(r[4:10]==a[0][4:10] for r in a+b),('coefficients/likelihood changed',n,mask)
  bm=statistics.median(float(r[-1]) for r in b);am=statistics.median(float(r[-1]) for r in a)
  timings.append(dict(n=n,free=labels[mask],before_ms=bm,after_ms=am,speedup=bm/am,before_evaluations=int(b[0][-2]),after_evaluations=int(a[0][-2]),coefficients_and_loglik_bit_identical=True))
  for variant in ['before','after']:
   key=n,mask,variant;v=scope[key];cc=cells[key];width=max(b for a,b in cc)-min(a for a,b in cc)
   profiles.append(dict(n=n,free=labels[mask],variant=variant,shape_cells=len(cc),maximum_subdivision_depth=round(max(math.log2(width/(b-a)) for a,b in cc)),score_boxes=v['score_box']['calls'],tail_calls=v['tail']['calls'],tail_ms=v['tail']['ms'],location_enclosure_ms=v['locations']['ms'],location_score_calls=v.get('location_score',dict(calls=0))['calls'],score_moment_ms=v['score_box']['ms']-v['locations']['ms'],conditional_ms=v['conditional']['ms'],interval_ms=v['interval']['ms'],**{k:counter.get(key,{}).get(k,0) for k in ['conditional_location_steps','location_hessian_passes','moment_location_steps']}))
for filename,rows in [('timings.tsv',timings),('profiles.tsv',profiles)]:
 with (out/filename).open('w') as f:
  w=csv.DictWriter(f,fieldnames=list(dict.fromkeys(k for row in rows for k in row)),delimiter='\t');w.writeheader();w.writerows(rows)
cpp_seconds=list(map(float,re.findall(r'Total Test time \(real\) =\s*([\d.]+) sec',log)))
elapsed=list(map(float,re.findall(r'^real ([\d.]+)$',log,re.M)))
assert len(elapsed)==6,elapsed
simbox_attempts={p:(root/'build/scratch'/p).read_text().strip() for p in ['ged-perf-checks.log','ged-perf-simbox-no-long-double.log','ged-perf-simbox-connect.log']}
record=dict(base_commit=base,machine='workstation GCC 15.2.0',simbox_attempts=simbox_attempts,source_sha256=hashes,changed_files=changed,golden_bytes_unchanged=True,test_cases_append_only=True,benchmark=dict(compiler_flags='-std=c++17 -O2',tol=1e-10,iterlim=200,repetitions=3,summary=timings),profiles=profiles,comparison=comparison,inherited_moment_bounds=bounds,targeted=dict(cases=42,assertions=5764,precision=70,enclosure_checks_each=2746,enclosure_probes=2,independent_golden_roots=4,violations=0),final_commands=[dict(command=c,machine=('simbox' if c in ['just test','just test-full'] else 'workstation'),seconds=s,exit_code=0) for c,s in zip(commands,elapsed)],cpp_shards=cpp_counts,cpp_test_seconds=cpp_seconds,r_counts=r_counts,site_checks=site)
(out/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
for src,dest in [('ged-perf-final-cpp.log','final-cpp.log.gz'),('ged-perf-final-suites.log','final-suites.log.gz'),('ged-perf-site.log','site.txt'),('ged-perf-checks-final.log','checks.txt'),('ged-perf-bounds.log','bounds.txt')]:
 data=(root/'build/scratch'/src).read_bytes()
 if dest.endswith('.gz'):
  with gzip.GzipFile(filename=str(out/dest),mode='wb',mtime=0) as f:f.write(data)
 else:(out/dest).write_bytes(data)
print(json.dumps(record,indent=2))
remote=(root/'build/scratch/ged-perf-simbox-final-no-long-double.log').read_text()
assert re.search(r'100% tests passed.*out of 129',remote)
record['simbox_no_long_double']=dict(passed=129,seconds=float(re.findall(r'Total Test time \(real\) =\s*([\d.]+)',remote)[-1]),machine='simbox GCC 13',exit_code=0)
(out/'validation.json').write_text(json.dumps(record,indent=2)+'\n')
with gzip.GzipFile(filename=str(out/'simbox-no-long-double.log.gz'),mode='wb',mtime=0) as f:f.write(remote.encode())
