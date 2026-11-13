#!/usr/bin/env python3
"""Resumable Monte Carlo for implemented numeric-inference family scopes.

Generates one standalone C++ probe per family, using existing binding-neutral
fitters and core samplers. Each replication has an independent deterministic
seed and a flushed checkpoint. A one-replication CPU-time probe marks fitters
slower than 0.25 seconds per n=500 trial as skipped; the aggregate projected
budget is 3,200 CPU seconds. No timed-out or failed fit is returned as an
estimate. This model-only sanity check is not a coverage or robustness proof.
"""
import hashlib
import json
import math
import os
from pathlib import Path
import re
import statistics
import subprocess
import shutil
import sys
import yaml

ROOT=Path(__file__).resolve().parents[2]
os.environ.setdefault('CCACHE_DIR',str(Path.home()/'.cache/ccache/univariateML'))
os.environ.setdefault('CCACHE_BASEDIR',str(ROOT))
os.environ.setdefault('CCACHE_NOHASHDIR','1')
COMPILER=([shutil.which('ccache')] if shutil.which('ccache') else [])+[os.environ.get('CXX','c++')]

INC=ROOT/'univariateML-cpp/include/cppunivariate'
DIST=INC/'distributions'
COMMON=ROOT/'bindings/common'
registry=(INC/'core/inference_numeric_generated.hpp').read_text()
generated=re.findall(r'#include "\.\./distributions/([^/]+)/dpqr.hpp"',registry)
base='norm gamma beta pois exp geom lnorm logitnorm lgamma rayleigh binom'.split()
special='laplace gev gpd ged sged snorm sstd cmb cmp'.split()
families=sorted(set(base+generated+special))
selected=sys.argv[1:] or families
if any(f not in families for f in selected): raise SystemExit('unknown or unavailable family requested')
h=hashlib.sha256(Path(__file__).read_bytes())
for directory in (INC,COMMON):
    for path in sorted(directory.rglob('*.hpp')):
        h.update(str(path.relative_to(ROOT)).encode());h.update(path.read_bytes())
for family in families:
    path=DIST/family/'README.md'
    h.update(str(path.relative_to(ROOT)).encode());h.update(path.read_bytes())
identity=h.hexdigest()
# Optional overrides for larger runs (defaults reproduce the pilot):
# UMVL_MC_REPS (<= 1000: seeds are 1000 apart per family), UMVL_MC_SLOW_LIMIT
# (CPU seconds per probe), UMVL_MC_BUDGET (CPU seconds), UMVL_MC_OUT (directory).
REPS=int(os.environ.get('UMVL_MC_REPS','200'))
SAMPLE_N=int(os.environ.get('UMVL_MC_N','500'))
if not 2<=REPS<=1000: raise SystemExit('UMVL_MC_REPS must be in [2, 1000]')
SLOW_LIMIT=float(os.environ.get('UMVL_MC_SLOW_LIMIT','0.25'))
BUDGET=float(os.environ.get('UMVL_MC_BUDGET','3200'))
OUT=Path(os.environ['UMVL_MC_OUT']) if os.environ.get('UMVL_MC_OUT') else ROOT/'build/scratch/inference-numeric-mc'/identity[:16]
OUT.mkdir(parents=True,exist_ok=True)
cache=ROOT/'univariateML-cpp/build/CMakeCache.txt'
boost=os.environ.get('BOOST_INCLUDE_DIR','')
if not boost and cache.exists():
    boost=next((line.split('=',1)[1] for line in cache.read_text().splitlines() if line.startswith('Boost_INCLUDE_DIR:')),'')

def cpp_number(x):
    v=float(x)
    if math.isnan(v): raise ValueError('NaN simulation coefficient')
    if math.isinf(v): return ('-' if v<0 else '')+'std::numeric_limits<double>::infinity()'
    return format(v,'.17g')

def setup(family):
    meta=yaml.safe_load((DIST/family/'README.md').read_text().split('---',2)[1])
    params=meta['params'];names=[p['name'] for p in params]
    truth=[float(p['simulate']) for p in params]
    held=[p.get('kind','regular')!='regular' for p in params]
    for i,p in enumerate(params):
        domain=str(p['domain']).replace(' ','')
        if not held[i] and domain in ('(0,1)','(0,1]','[0,1]','[0,1)') and not 0<truth[i]<1: truth[i]=.4
        elif not held[i] and domain in ('(0,inf)','[0,inf)') and not truth[i]>0: truth[i]=1.
        elif not held[i] and domain in ('(1,inf)','[1,inf)') and not truth[i]>1: truth[i]=2.
        elif not held[i] and domain=='(2,inf)' and not truth[i]>2: truth[i]=5.
    def override(name,value,fix=False):
        i=names.index(name);truth[i]=value;held[i]=held[i] or fix
    if family=='dlaplace': override('location',0,True)
    if family=='laplace': override('mu',0,True)
    if family=='gev': override('xi',0,True)
    if family=='gpd': override('xi',.2)
    if family in ('ged','sged'): override('nu',3)
    if family in ('snorm','sstd'): override('xi',1,True)
    if family=='sn': override('alpha',.5)
    if family=='foldnorm': override('mu',1)
    if family=='bhatt': override('a',.7)
    if family=='sgomp': override('eta',1)
    if family in ('bbinom','zib','ztbinom','zmbinom','cmb','zipf'):
        name='N' if family=='zipf' else 'size'
        if truth[names.index(name)]<2: override(name,10,True)
    # UMVL_MC_TRUTH="name=value,..." overrides truth values (free coefficients stay free).
    for item in filter(None,os.environ.get('UMVL_MC_TRUTH','').split(',')):
        key,value=item.split('=');override(key.strip(),float(value))
    fitfile=COMMON/'distributions'/family/'fit.hpp'
    fittext=fitfile.read_text()
    hit=re.search(r'inline\s+FitResult\s+fit_'+re.escape(family)+r'\s*\(\s*cppunivariate::(ispan|span)\s+\w+\s*,?([^)]*)',fittext,re.S)
    if not hit: raise ValueError('fit signature unavailable')
    fitnames=re.findall(r'\bHeld\s+(\w+)_arg\b',hit.group(2))
    if fitnames[:len(names)]!=names: raise ValueError(f'fit coefficient order mismatch: {fitnames} vs {names}')
    free=[i for i in range(len(names)) if not held[i]]
    if not free: raise ValueError('empty free vector')
    return meta,names,truth,held,free,hit.group(1)

def make_source(family,names,truth,held,free,span_type,index):
    q=', '.join(cpp_number(v) for v in truth)
    heldargs=', '.join('umvl_binding::Held{true,truth['+str(i)+'],1}' if held[i] else 'umvl_binding::Held{}' for i in range(len(names)))
    sampler=', '.join('truth['+str(i)+']' for i in range(len(names)))
    mask=sum(1<<i for i in free)
    ptr='counts.data()' if span_type=='ispan' else 'data.data()'
    integer_check='''if(!std::isfinite(v) || v!=std::floor(v) || v<std::numeric_limits<int>::min() || v>std::numeric_limits<int>::max()) { fail("sample_failure"); return 0; }
    counts.push_back(static_cast<int>(v));''' if span_type=='ispan' else 'if(!std::isfinite(v)) { fail("sample_failure"); return 0; }'
    assign='\n'.join('  bool found'+str(i)+'=false; for(const auto& kv:fit.estimates) if(kv.first=="'+name+'") {est['+str(i)+']=kv.second;found'+str(i)+'=true;}\n  if(!found'+str(i)+') {fail("missing_estimate","'+name+'");return 0;}' for i,name in enumerate(names))
    return f'''// Generated scratch probe, model point {family}.
#include <array>
#include <cmath>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>
#include "cppunivariate/core/inference_families.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "distributions/{family}/fit.hpp"
int main(int argc,char** argv) {{
  if(argc!=2) return 2; const int rep=std::stoi(argv[1]);const std::clock_t start=std::clock();
  const auto elapsed=[&]() {{return static_cast<double>(std::clock()-start)/CLOCKS_PER_SEC;}};
  const auto fail=[&](const std::string& why,const std::string& detail=std::string{{}}) {{std::cout<<rep<<" "<<why<<" "<<elapsed()<<" "<<std::quoted(detail)<<"\\n";}};
  std::cout<<std::setprecision(17);cppunivariate::distributions::StdRng<> rng(20261101ULL+1000ULL*{index}+rep);
  const std::array<double,{len(names)}> truth={{{q}}}; std::vector<double> data;std::vector<int> counts;
  data.reserve({SAMPLE_N});counts.reserve({SAMPLE_N});
  for(int i=0;i<{SAMPLE_N};++i) {{
    const double v=cppunivariate::distributions::r{family}(rng,{sampler});
    {integer_check}
    data.push_back(v);
  }}
  const auto fit=umvl_binding::fit_{family}(cppunivariate::{span_type}{{{ptr},data.size()}},{heldargs});
  if(!fit.error.empty()) {{fail("fit_failure",fit.error);return 0;}}
  if(!fit.restricted.empty()) {{std::ostringstream detail;for(const auto& a:fit.restricted)detail<<a.first<<":"<<a.second<<";";fail("restricted_fit",detail.str());return 0;}}
  std::array<double,cppunivariate::inference_max_parameters> est{{}};
{assign}
  const auto info=cppunivariate::family_inference(cppunivariate::span{{data.data(),data.size()}},"{family}",est,{mask});
  const auto model=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Observed);
  const auto sandwich=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Sandwich);
  if(info.status!=cppunivariate::InferenceStatus::Ok || model.status!=cppunivariate::InferenceStatus::Ok || sandwich.status!=cppunivariate::InferenceStatus::Ok) {{std::ostringstream detail;detail<<"inference="<<static_cast<int>(info.status)<<", model="<<static_cast<int>(model.status)<<", sandwich="<<static_cast<int>(sandwich.status)<<": "<<info.validity_conditions;fail("inference_failure",detail.str());return 0;}}
  std::cout<<rep<<" ok "<<elapsed();
  for(std::size_t j=0;j<info.dimension;++j) std::cout<<" "<<est[info.free_indices[j]]<<" "<<std::sqrt(model.matrix[j*info.dimension+j])<<" "<<std::sqrt(sandwich.matrix[j*info.dimension+j]);
  std::cout<<"\\n";
}}
'''

results={}
summary=OUT/'summary.json'
projected=0.
used_cpu=0.
if summary.exists():
    prior_summary=json.loads(summary.read_text())
    results=prior_summary['families']
    projected=prior_summary.get('projected_cpu_seconds',0.)
    used_cpu=prior_summary.get('actual_simulation_cpu_seconds',0.)
for index,family in enumerate(families):
    if family not in selected: continue
    prior=results.get(family,{})
    if prior.get('status') in ('complete','skipped_slow','skipped_budget','open_setup','open_compile'): continue
    try:
        meta,names,truth,held,free,span_type=setup(family)
        source=make_source(family,names,truth,held,free,span_type,index)
    except Exception as exc:
        results[family]={'status':'open_setup','reason':f'{type(exc).__name__}: {exc}'}
        summary.write_text(json.dumps({'source_hash':identity,'families':results},indent=2)+'\n');continue
    path=OUT/(family+'.cpp');binary=OUT/(family+'.bin');path.write_text(source)
    compile_result=subprocess.run(COMPILER+['-std=c++17','-O2','-I'+str(COMMON),'-I'+boost,
                 '-I'+str(INC.parent),str(path),'-o',str(binary)],text=True,capture_output=True)
    if compile_result.returncode:
        (OUT/(family+'.compile.log')).write_text(compile_result.stderr)
        results[family]={'status':'open_compile','reason':'see '+family+'.compile.log'}
        summary.write_text(json.dumps({'source_hash':identity,'families':results},indent=2)+'\n');print(family,'compile failure',flush=True);continue
    checkpoint=OUT/(family+'.txt')
    lines=checkpoint.read_text().splitlines() if checkpoint.exists() else []
    assert all(int(line.split()[0])==i for i,line in enumerate(lines)),(family,'invalid checkpoint')
    used_cpu+=sum(float(line.split()[2]) for line in lines)
    if not lines:
        first=subprocess.run([str(binary),'0'],text=True,capture_output=True,check=True).stdout.strip()
        assert first.split()[0]=='0',(family,first)
        lines.append(first);checkpoint.write_text(first+'\n')
        used_cpu+=float(first.split()[2])
    probe_cpu=float(lines[0].split()[2])
    item={'truth':{k:(v if math.isfinite(v) else ('Inf' if v>0 else '-Inf')) for k,v in zip(names,truth)},'held':[names[i] for i in range(len(names)) if held[i]],
          'free':[names[i] for i in free],'probe_cpu_seconds':probe_cpu}
    if probe_cpu>SLOW_LIMIT:
        item.update(status='skipped_slow',reason=f'one n=500 sampling, fit and inference probe exceeded {SLOW_LIMIT} CPU seconds')
    elif used_cpu>=BUDGET or projected+probe_cpu*REPS>BUDGET:
        item.update(status='skipped_budget',reason=f'projected simulation exceeds the {BUDGET} CPU-second budget')
    else:
        projected+=probe_cpu*REPS
        with checkpoint.open('a') as dest:
            for rep in range(len(lines),REPS):
                line=subprocess.run([str(binary),str(rep)],text=True,capture_output=True,check=True).stdout.strip()
                assert int(line.split()[0])==rep,(family,line)
                lines.append(line);dest.write(line+'\n');dest.flush()
                used_cpu+=float(line.split()[2])
                if used_cpu>=BUDGET: break
        if len(lines)<REPS:
            item.update(status='skipped_budget',reason=f'actual simulation CPU use reached {BUDGET} seconds',replications=len(lines))
            results[family]=item
            summary.write_text(json.dumps({'source_hash':identity,'actual_simulation_cpu_seconds':used_cpu,
                'projected_cpu_seconds':projected,'families':results},indent=2)+'\n')
            print(family,item['status'],flush=True)
            continue
        good=[[float(v) for v in line.split()[3:]] for line in lines if line.split()[1]=='ok']
        failures=[line for line in lines if line.split()[1]!='ok']
        item.update(status='complete',replications=len(lines),successes=len(good),failures=failures,coordinates={})
        if len(good)>=2:
            for j in range(len(free)):
                estimate=[v[3*j] for v in good];model=[v[3*j+1] for v in good];sandwich=[v[3*j+2] for v in good]
                if not all(math.isfinite(v) and v>=0 for v in model+sandwich):
                    item['status']='open_nonfinite_se';continue
                n=len(good);mean=statistics.mean(estimate);sd=statistics.stdev(estimate)
                if not sd>0: item['status']='open_zero_empirical_sd';continue
                influence=[((v-mean)**2-sd**2)/(2*sd) for v in estimate]
                m4=statistics.mean((v-mean)**4 for v in estimate)
                coord={'empirical_sd':sd,'empirical_sd_mcse':math.sqrt(max(0,m4-((n-3)/(n-1))*sd**4)/n)/(2*sd)}
                for label,values in [('model',model),('sandwich',sandwich)]:
                    avg=statistics.mean(values);mcse=statistics.stdev(values)/math.sqrt(n)
                    diff_mcse=statistics.stdev(a-b for a,b in zip(values,influence))/math.sqrt(n)
                    coord[label]={'mean_se':avg,'mean_se_mcse':mcse,'difference':avg-sd,
                                  'difference_mcse_paired':diff_mcse,'flag_2mcse':abs(avg-sd)>2*diff_mcse}
                item['coordinates'][names[free[j]]]=coord
    results[family]=item
    summary.write_text(json.dumps({'revision':(subprocess.run(['git','rev-parse','HEAD'],cwd=ROOT,text=True,capture_output=True).stdout.strip() or os.environ.get('UMVL_REVISION','unknown')),
                       'source_hash':identity,'sample_size':SAMPLE_N,'replications':REPS,'seed':'20261101 + 1000*family_index + replication',
                       'family_order':families,'projected_cpu_seconds':projected,
                       'actual_simulation_cpu_seconds':used_cpu,'families':results},indent=2)+'\n')
    print(family,item['status'],item.get('successes',''),flush=True)
print('results:',summary,flush=True)
if any(v.get('status','').startswith('open') or v.get('failures') for v in results.values()):
    raise SystemExit('simulation has open families or failed replications; see summary')
