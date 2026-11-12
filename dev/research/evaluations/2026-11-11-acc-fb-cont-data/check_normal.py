"""Independent Fraction oracle for exact binary-product normal checks."""
from pathlib import Path
from fractions import Fraction as F
import random,math,subprocess,json,os,sys
P=Path(__file__).resolve().parent;ROOT=P.parents[3];work=ROOT/'build/scratch/exact-normal'/str(os.getpid());work.mkdir(parents=True,exist_ok=True)
rng=random.Random(9021053);cases=[]
exponents=[-1073,-1000,-500,-1,0,1,500,1000,1023,1024]
def number():return math.ldexp(.5+rng.getrandbits(52)*2**-53,rng.choice(exponents))*rng.choice([-1,1])
for _ in range(300):cases.append(dict(data=[number() for _ in range(rng.randrange(2,11))],center=number(),scale=abs(number())))
for e in [-1073,-1000,-500,0,500,1023,1024]:
 v=math.ldexp(.5,e)
 for sigma in [math.ldexp(1.,-1074),1.,v]:
  cases += [dict(data=[v]*7,center=0.,scale=sigma),dict(data=[-v,v],center=0.,scale=sigma),dict(data=[-v,v],center=math.ldexp(1.,-1000),scale=sigma)]
cases += [dict(data=[-3.,-1.,0.,1.,3.],center=0.,scale=2.),dict(data=[-2.,-1.,1.,2.],center=0.,scale=math.sqrt(2.5))]
def sign(v):return (v>0)-(v<0)
def encode(v):
 m,e=math.frexp(abs(v));return f'{int(m*2**53)} {e-53} {int(v<0)}'
lines=[];expected=[]
for c in cases:
 x=list(map(F,c['data']));mu,s=F(c['center']),F(c['scale']);n=len(x);B=sum(x)-n*mu;A=sum((v-mu)**2 for v in x)-n*s*s
 expected.append([int(B==0 and A==0),sign(A),sign(n*A-B*B),int(x[0]+x[-1]==2*mu)])
 lines.append(str(n)+' '+encode(c['center'])+' '+encode(c['scale'])+' '+' '.join(map(encode,c['data']))+'\n')
input_text=''.join(lines);results={};source=str(P/'check_normal.cpp');inc='-I'+str(ROOT/'univariateML-cpp/include')
for mode in ['native','double','wasm']:
 exe=work/('check-'+mode+('.cjs' if mode=='wasm' else ''))
 if mode=='wasm':
  env=dict(os.environ,EM_CACHE=str(ROOT/'build/em-cache'),EMCC_CORES='1')
  subprocess.run(['bash','-c','source "$HOME/emsdk/emsdk_env.sh" >/dev/null && export EM_CACHE="$1" EMCC_CORES=1 && shift && em++ "$@"','bash',str(ROOT/'build/em-cache'),source,inc,'-std=c++17','-O2','-sEXIT_RUNTIME=1','-sENVIRONMENT=node','-o',str(exe)],env=env,check=True)
  command=['node',str(exe)]
 else:
  subprocess.run(['g++',source,inc,'-std=c++17','-O2',*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if mode=='double' else []),'-o',str(exe)],check=True);command=[str(exe)]
 actual=[list(map(int,line.split())) for line in subprocess.check_output(command,input=input_text,text=True).splitlines()]
 mismatches=[i for i,(a,b) in enumerate(zip(actual,expected)) if a!=b]
 assert len(actual)==len(expected),(mode,len(actual));results[mode]=dict(cases=len(cases),mismatches=mismatches)
 print('exact normal',mode,results[mode],flush=True)
for filename,value in [('exact-normal-inputs.json',cases),('exact-normal-checks.json',results)]:
 temporary=P/(filename+'.tmp-'+str(os.getpid()));temporary.write_text(json.dumps(value,indent=2)+'\n');temporary.replace(P/filename)
assert not any(r['mismatches'] for r in results.values())
