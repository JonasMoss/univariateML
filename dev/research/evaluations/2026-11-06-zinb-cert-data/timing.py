import json,os,subprocess
from pathlib import Path
base=Path(__file__).resolve().parent;root=base.parents[3];os.chdir(root)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['ccache','c++','-std=c++17','-O2','-IunivariateML-cpp/include','-I'+boost,str(base/'timing.cpp'),'-o','build/scratch/zinb-timing'],check=True)
g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/zinb/golden.json').read_text());e=g['estimates']
line=' '.join(map(str,[len(g['data']),e['size'],e['prob'],e['pi']]+g['data']))+'\n'
a=subprocess.run(['build/scratch/zinb-timing'],input=line,text=True,capture_output=True,check=True)
(base/'timing-raw.txt').write_text(a.stdout+a.stderr)
rows=[dict(mask=int(v[0]),method='certificate' if int(v[1]) else 'legacy-proposer',median_us=float(v[2])) for v in map(str.split,a.stdout.splitlines())]
(base/'timing.json').write_text(json.dumps(dict(batches=3,repetitions=100,n=len(g['data']),rows=rows),indent=2)+'\n')
print(a.stdout)
