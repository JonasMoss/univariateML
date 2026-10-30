"""Run under the CPU slot lock after activating emsdk; no linking required."""
from pathlib import Path
import subprocess, json, os
root=Path(__file__).resolve().parents[2]
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True)
out=root/'build/scratch/inventory';out.mkdir(exist_ok=True)
rows=[]
for family in sorted((root/'univariateML-cpp/include/cppunivariate/distributions').iterdir()):
 if not family.is_dir():continue
 row={'family':family.name}
 for header in ['mle.hpp','dpqr.hpp']:
  src=out/'probe.cpp';src.write_text(f'#include "cppunivariate/distributions/{family.name}/{header}"\n')
  p=subprocess.run(['em++','-std=c++17','-fsyntax-only','-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src)],capture_output=True,text=True)
  (out/f'{family.name}-{header}.log').write_text(p.stderr)
  row[header]='compiles' if p.returncode==0 else next((s.strip() for s in p.stderr.splitlines() if 'error:' in s),'unknown error')
 rows.append(row)
(out/'results.json').write_text(json.dumps(rows,indent=2))
print(f'{len(rows)} families; '+str(sum(all(r[h]=='compiles' for h in ['mle.hpp','dpqr.hpp']) for r in rows))+' compile both headers')
