"""Native public default golden fits before/after, with unchanged frozen data."""
import json,os,subprocess
from pathlib import Path
p=Path(__file__).resolve().parent;root=p.parents[3];scratch=root/'build/scratch/fpareto-archive'
os.environ.update(TMPDIR=str(root/'build/tmp'),JUST_TEMPDIR=str(root/'build/tmp'),XDG_RUNTIME_DIR=str(root/'build/tmp'))
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
executables=[]
for baseline in [True,False]:
 exe=scratch/('golden-before' if baseline else 'golden-after')
 flags=['-DFPAR_HEADER="'+str(scratch/'baseline-fpareto.hpp')+'"'] if baseline else []
 subprocess.run(['ccache','g++','-std=c++17','-O2',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(p/'golden.cpp'),'-o',str(exe)],check=True)
 executables.append(exe)
rows=[]
for family_id,family in enumerate(['fpareto','gb2']):
 g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions'/family/'golden.json').read_text());line=' '.join(map(str,[family_id,len(g['data']),*g['data']]))
 values=[subprocess.check_output([str(exe)],input=line,text=True).strip() for exe in executables]
 row=dict(family=family,before=values[0],after=values[1],text_identical=values[0]==values[1]);rows.append(row)
(p/'golden-check.json').write_text(json.dumps(rows,indent=2)+'\n');print(json.dumps(rows,indent=2));assert all(r['text_identical'] for r in rows)
