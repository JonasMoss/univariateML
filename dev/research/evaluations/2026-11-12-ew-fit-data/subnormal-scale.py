"""Run through /tmp/umvl-cpu/run from the repository root.

The exact fixed-shape scale is exp(log(mean(exp(b*log(x))))/b).
At b=double denorm_min its log differs from mean(log(x)) by at most
b*(log(8))**2/8, far below double precision; sqrt(2) is the oracle.
The parent Weibull result is recorded, not asserted: this job changes EW only.
"""
from pathlib import Path
import subprocess,json,os

root=Path.cwd();here=Path(__file__).resolve().parent;rows=[]
tmp=root/'build/tmp';tmp.mkdir(parents=True,exist_ok=True)
os.environ.update(TMPDIR=str(tmp),JUST_TEMPDIR=str(tmp),XDG_RUNTIME_DIR=str(tmp))
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True)
for mode,flags in [('native',[]),('double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
    exe=root/'build/scratch'/('subnormal-scale-'+mode)
    subprocess.run(['c++','-O2','-std=c++17',*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(here/'subnormal-scale.cpp'),'-o',str(exe)],check=True)
    output=subprocess.check_output([str(exe)],text=True).strip()
    words=output.split();status=int(words[3]);scale=float(words[4]);oracle=float(words[-1])
    assert status==0 and abs(scale/oracle-1)<1e-10,output
    rows.append(dict(mode=mode,output=output,ew_relative_error=abs(scale/oracle-1)))
(here/'subnormal-scale.json').write_text(json.dumps(rows,indent=2)+'\n')
print(json.dumps(rows,indent=2))
