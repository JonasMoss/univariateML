#!/usr/bin/env python3
"""Reproduce TASK-116 accuracy and before/after timing checks.
Run under /tmp/umvl-cpu/run from the repository root; no installations.
Writes only build/scratch/lomax-cert; --output selects the retained JSON.
"""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import random
import re
import subprocess
import platform
import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
FAMILY = ROOT/'univariateML-cpp/include/cppunivariate/distributions/lomax'
BASE = '54b0d06a3e447f944940fc0dd4b5b87235d2acc2'
DRIVER = r'''
#include <iostream>
#include <iomanip>
#include <chrono>
#include <string>
#include <vector>
#include "before.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
int main() {
  std::string name; int n,reps;
  std::cout << std::setprecision(17);
  while(std::cin>>name>>n>>reps) {
    std::vector<double> x(n); for(double& v:x)std::cin>>v;
    const cppunivariate::span sample{x.data(),x.size()};
    cppunivariate::LomaxResult now;
    cppunivariate::BaselineLomaxResult before;
    auto t=std::chrono::steady_clock::now();
    for(int j=0;j<reps;++j)before=cppunivariate::baseline_lomax_mle(sample,1.220703e-4,100);
    const double oldtime=std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()/reps;
    t=std::chrono::steady_clock::now();
    for(int j=0;j<reps;++j)now=cppunivariate::lomax_mle(sample,1.220703e-4,100);
    const double newtime=std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()/reps;
    std::cout<<name<<' '<<int(before.status)<<' '<<before.lambda<<' '<<before.kappa<<' '<<oldtime<<' '
      <<int(now.status)<<' '<<now.lambda<<' '<<now.kappa<<' '<<now.logLik<<' '<<newtime<<' '<<now.iterations<<' '<<int(now.certificate_failure)<<'\n';
  }
}
'''

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    os.chdir(ROOT)
    scratch = ROOT/'build/scratch/lomax-cert'
    scratch.mkdir(parents=True, exist_ok=True)
    baseline = subprocess.check_output(['git','show',f'{BASE}:univariateML-cpp/include/cppunivariate/distributions/lomax/mle.hpp'],text=True)
    baseline = baseline.replace('../../core/', 'cppunivariate/core/')
    baseline = re.sub(r'\bLomax', 'BaselineLomax', baseline)
    baseline = re.sub(r'\blomax_', 'baseline_lomax_', baseline)
    (scratch/'before.hpp').write_text(baseline)
    (scratch/'driver.cpp').write_text(DRIVER)
    command = ['c++','-std=c++17','-O2','-I'+str(ROOT/'univariateML-cpp/include'),str(scratch/'driver.cpp'),'-o',str(scratch/'driver')]
    subprocess.run(command,check=True)
    golden=json.loads((FAMILY/'golden.json').read_text())
    cases={'golden':golden['data'],'nonlocal':[1,100],'small_min':[.001,4,6],
           'near_crossing':[1,29],'negative_boundary':[1,1,40,59],
           'moment_equality':[1,1,40,60], 'shallow':[1,1,1,12,12], 'replicated':[1,29]*3000,
           'concentrated':[.9,.95,1,1.05,1.1], 'escape':[1,28], 'constant':[1]*5}
    rng=random.Random(116)
    cases['n10000']=[math.expm1(-math.log1p(-rng.random())/2) for _ in range(10000)]
    cases['shallow_subnormal_rate']=[v*1e307 for v in cases['shallow']]
    cases['shallow_small']=[v*1e-200 for v in cases['shallow']]
    cases['shallow_large']=[v*1e200 for v in cases['shallow']]
    cases['scale_small']=[v*1e-200 for v in golden['data']]
    cases['scale_large']=[v*1e200 for v in golden['data']]
    payload=''.join(f'{name} {len(x)} 5 '+' '.join(map(repr,x))+'\n' for name,x in cases.items())
    lines=subprocess.check_output([str(scratch/'driver')],input=payload,text=True).splitlines()
    mp.mp.dps=80
    records=[]
    for line in lines:
        name,oldstatus,oldlambda,oldkappa,oldtime,status,lam,kappa,ll,newtime,work,reason=line.split()
        row=dict(name=name,n=len(cases[name]),old_status=int(oldstatus),status=int(status),
                 old_seconds=float(oldtime),seconds=float(newtime),subdivision_evaluations=int(work),reason=int(reason))
        if int(status)==0:
            lm=float(lam);k=float(kappa);x=cases[name]
            # Use exact binary64 inputs, and independently solve the scalar score.
            xx=list(map(mp.mpf,x)); mean=mp.fsum(xx)/len(x)
            y=[v/mean for v in xx]
            def quantities(z):
                q=mp.exp(z)
                aa=mp.fsum(mp.log1p(q*v) for v in y)/len(y)
                bb=mp.fsum(q*v/(1+q*v) for v in y)/len(y)
                return aa,1-bb/aa-bb
            z0=mp.log(mp.mpf(lm)*mean)
            z=mp.findroot(lambda z:quantities(z)[1],(z0-mp.mpf('.0001'),z0+mp.mpf('.0001')),tol=mp.mpf('1e-65'))
            aa,score=quantities(z)
            oracle_l=mp.exp(z)/mean;oracle_k=1/aa
            le=float(abs(mp.mpf(lm)/oracle_l-1));ke=float(abs(mp.mpf(k)/oracle_k-1))
            assert max(le,ke)<5e-7,(name,le,ke)
            oracle_ll=len(x)*(z-mp.log(aa)-aa-mp.log(mean)-1)
            assert abs(float(ll)-float(oracle_ll))<1e-8*len(x),(name,ll,oracle_ll)
            row.update(lambda_=lm,kappa=k,logLik=float(ll),lambda_relative_error=le,kappa_relative_error=ke,
                       oracle_lambda=str(oracle_l),oracle_kappa=str(oracle_k),oracle_score=str(score))
            if name=='golden':
                assert abs(lm/golden['estimates']['lambda']-1)<1e-10
                assert abs(k/golden['estimates']['kappa']-1)<1e-10
        else:
            assert int(status)==1,(name,line)
        records.append(row)
    result={'baseline_revision':BASE,'source_revision':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
            'machine':'local workstation','repetitions':5,'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
            'mpmath_precision':80,'mpmath_version':mp.__version__,
            'python_version':platform.python_version(),'platform':platform.platform(),'cases':records,
            'source_sha256':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in [FAMILY/'mle.hpp',FAMILY/'certificate.hpp',Path(__file__)]}}
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(result,indent=2,allow_nan=False)+'\n')
    print(json.dumps({'checks':'passed','cases':len(records),'output':str(args.output)}))

if __name__=='__main__':
    main()
