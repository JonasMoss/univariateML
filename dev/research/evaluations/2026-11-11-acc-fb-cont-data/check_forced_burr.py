from pathlib import Path
import subprocess,json,os
P=Path(__file__).resolve().parent;ROOT=P.parents[3];boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip();out={}
for mode in ['native','double']:
 exe=ROOT/('build/scratch/forced-burr-'+mode+'-'+str(os.getpid()))
 subprocess.run(['g++','-O2','-std=c++17',*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []),'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(P/'forced_burr.cpp'),'-o',str(exe)],check=True)
 out[mode]=json.loads(subprocess.check_output([str(exe)],text=True));print('forced Burr',mode,out[mode],flush=True);assert out[mode]['box_below_minimum']
temporary=P/('forced-burr-evidence.tmp-'+str(os.getpid()));temporary.write_text(json.dumps(out,indent=2)+'\n');temporary.replace(P/'forced-burr-evidence.json')
