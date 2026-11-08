"""Summarize retained numerical evidence; run through a CPU slot."""
import hashlib,json,pathlib,subprocess
root=pathlib.Path.cwd()
record=root/'dev/research/evaluations/2026-11-08-gb2-cert-data'
rows=[line.split() for line in (record/'after.txt').read_text().splitlines() if line.split()[0] not in {'large','shifted'}]
ref=json.loads((record/'high-precision.json').read_text())
errors=[]
for row,oracle in zip(rows,ref):
 inverse=bool(int(row[1]))
 shape=float(row[5] if inverse else row[3]);scale=float(row[6]);ll=float(row[7])
 errors.append(dict(power=float(row[0]),inverse=inverse,
  shape_relative_error=abs(shape/float(oracle['shape'])-1),
  scale_relative_error=abs(scale/float(oracle['scale'])-1),
  loglik_absolute_error=abs(ll-float(oracle['logLik']))))
assert max(r['shape_relative_error'] for r in errors)<1e-6
assert max(r['scale_relative_error'] for r in errors)<1e-6
(record/'accuracy.json').write_text(json.dumps(errors,indent=2)+'\n')
integrity={}
for family in ['gb2','fpareto']:
 for name in ['golden.json','dpqr.hpp','truth.py','truth.json']:
  path=f'univariateML-cpp/include/cppunivariate/distributions/{family}/{name}'
  old=subprocess.check_output(['git','show','f204b94e:'+path]);current=(root/path).read_bytes()
  assert current==old,path
  integrity[path]=hashlib.sha256(current).hexdigest()
(record/'settled-file-sha256.json').write_text(json.dumps(integrity,indent=2)+'\n')
versions={}
for label,cmd in [('compiler',['c++','--version']),('R',['R','--version']),('python',['python3','--version']),('node',['node','--version']),('BH',['Rscript','-e','cat(as.character(packageVersion("BH")))'])]:
 versions[label]=subprocess.check_output(cmd,text=True).splitlines()[0]
(record/'environment-local.json').write_text(json.dumps(versions,indent=2)+'\n')
print(json.dumps({'max_shape_relative_error':max(r['shape_relative_error'] for r in errors),'max_scale_relative_error':max(r['scale_relative_error'] for r in errors),'max_loglik_absolute_error':max(r['loglik_absolute_error'] for r in errors),'settled_files_unchanged':len(integrity)}))
