"""Single-core WASM portability probe while the full-suite group is occupied."""
import study,subprocess,os,json
from pathlib import Path
body=''
for f in study.F:
 pars=','.join('p['+str(i)+']' for i in range(3 if f in ('ged','std') else 4))
 body+=f'if(f=="{f}"){{if(op==0)a=cppunivariate::distributions::d{f}(x,{pars},lg);if(op==1)a=cppunivariate::distributions::p{f}(x,{pars},lo,lg);if(op==2)a=cppunivariate::distributions::q{f}(x,{pars},lo,lg);}}'
includes=''.join('#include "cppunivariate/distributions/'+f+'/dpqr.hpp"\n' for f in study.F)
source='#include <emscripten/bind.h>\n#include <vector>\n#include <string>\n'+includes
source+='double probe(std::string f,int op,double x,emscripten::val args,bool lo,bool lg){std::vector<double>p;for(unsigned i=0;i<args["length"].as<unsigned>();++i)p.push_back(args[i].as<double>());double a=NAN;'+body+'return a;}\nEMSCRIPTEN_BINDINGS(dpq_skew_probe){emscripten::function("probe",&probe);}\n'
(study.W/'lean.cpp').write_text(source)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
cpu=str(sorted(os.sched_getaffinity(0))[0]);env=os.environ.copy();env.update(EM_CACHE=str(study.ROOT/'build/em-cache'),EMCC_CORES='1',BINARYEN_CORES='1')
# Source the installed SDK in the compiler child; no install or source mutation.
sdk=os.environ.get('EMSDK',str(Path.home()/'emsdk'))
compiler=['taskset','-c',cpu,'em++',str(study.W/'lean.cpp'),'-std=c++17','-O2','-I'+str(study.ROOT/'univariateML-cpp/include'),'-I'+boost,'--bind','-sMODULARIZE','-sEXPORT_ES6','-sALLOW_MEMORY_GROWTH','-sENVIRONMENT=node','-o',str(study.W/'lean.mjs')]
subprocess.run(['bash','-e','-c','source "$1/emsdk_env.sh" >/dev/null 2>&1; export EM_CACHE="$2"; shift 2; exec "$@"','lean-wasm',sdk,env['EM_CACHE'],*compiler],check=True,env=env)
s=(study.H/'js-consumer.mjs').read_text().replace("import create from '../../../../build/js/univariateML.mjs';","import create from './lean.mjs';").replace("new URL('consumer-jobs.json',import.meta.url)",json.dumps(str(study.H/'consumer-jobs.json'))).replace("new URL('final-js.json',import.meta.url)",json.dumps(str(study.H/'lean-js.json')))
s=s.replace("m[['d','p','q'][op]+f](new Float64Array([x]),p,!!lo,!!lg)[0]","m.probe(f,op,x,p,!!lo,!!lg)")
(study.W/'lean-check.mjs').write_text(s)
subprocess.run(['node','--v8-pool-size=1',str(study.W/'lean-check.mjs')],check=True)
