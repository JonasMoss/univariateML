#!/usr/bin/env python3
"""Independent stationary-root checks and before/after GPD timings.
Run through /tmp/umvl-cpu/run, from the worktree root. No installations.
"""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import platform
import random
import shutil
import subprocess
import mpmath as mp

ROOT = Path(__file__).resolve().parents[4]
BASE = "fe4573e9b02a950a1440039a8395c30ff2f25926"
FAMILY = Path("univariateML-cpp/include/cppunivariate/distributions/gpd")
SCRATCH = ROOT / "build/scratch/gpd-cert"

def prepare():
    SCRATCH.mkdir(parents=True, exist_ok=True)
    baseline = subprocess.check_output(["git", "show", f"{BASE}:{FAMILY}/mle.hpp"], cwd=ROOT, text=True)
    baseline = baseline.replace('"../../core/', '"cppunivariate/core/').replace('"../gev/', '"cppunivariate/distributions/gev/')
    baseline = baseline.replace("namespace cppunivariate {", "namespace baseline {\nusing cppunivariate::span;\nnamespace extreme_detail=cppunivariate::extreme_detail;")
    baseline = baseline.replace("gpd_", "baseline_gpd_")
    (SCRATCH / "baseline.hpp").write_text(baseline)
    source = r'''
#include "cppunivariate/distributions/gpd/mle.hpp"
#include "baseline.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <string>
int main() {
  int n,repeats;double sigma,xi;std::string s,k;
  std::cout << std::setprecision(17);
  while(std::cin>>n>>repeats>>s>>k) {
    sigma=std::stod(s);xi=std::stod(k);
    std::vector<double> x(n);for(auto& v:x)std::cin>>v;
    const cppunivariate::span data{x.data(),x.size()};
    const auto before=std::chrono::steady_clock::now();
    baseline::GpdResult old{};
    for(int j=0;j<repeats;++j)old=baseline::baseline_gpd_mle(data,1e-12,200,0,xi,sigma);
    const auto middle=std::chrono::steady_clock::now();
    cppunivariate::GpdResult fit{};
    for(int j=0;j<repeats;++j)fit=cppunivariate::gpd_mle(data,1e-12,200,0,xi,sigma);
    const auto end=std::chrono::steady_clock::now();
    std::cout << static_cast<int>(fit.status)<<' '<<static_cast<int>(fit.certificate_failure)<<' '
      <<fit.sigma<<' '<<fit.xi<<' '<<fit.logLik<<' '<<fit.iterations<<' '
      <<old.sigma<<' '<<old.xi<<' '<<old.logLik<<' '
      <<std::chrono::duration<double,std::milli>(middle-before).count()/repeats<<' '
      <<std::chrono::duration<double,std::milli>(end-middle).count()/repeats<<'\n';
  }
}
'''
    (SCRATCH / "runner.cpp").write_text(source)
    cmd = ["c++", "-std=c++17", "-O2", "-I", str(ROOT / "univariateML-cpp/include"), str(SCRATCH / "runner.cpp"), "-o", str(SCRATCH / "runner")]
    if shutil.which("ccache"):
        cmd.insert(0,"ccache")
        os.environ.setdefault("CCACHE_DIR",str(Path.home()/".cache/ccache/univariateML"))
        os.environ.setdefault("CCACHE_BASEDIR",str(ROOT))
        os.environ.setdefault("CCACHE_NOHASHDIR","1")
    subprocess.run(cmd, check=True)
    return cmd

def workloads():
    golden = json.loads((ROOT / FAMILY / "golden.json").read_text())
    result = [("golden", golden["data"], math.nan, math.nan)]
    for shape in [-.9, -.6, -.1, 0., .3, 2.]:
        x = [math.expm1(-shape*math.log1p(-(i+.5)/400))/shape if shape else -math.log1p(-(i+.5)/400) for i in range(400)]
        result.append((f"quantiles-{shape}", x, math.nan, math.nan))
    def seam_sample(power):
        return [(-math.log((i+.5)/400))**power for i in range(400)]
    low,high=.9,1.1
    for _ in range(50):
        middle=(low+high)/2;x=seam_sample(middle);mean=math.fsum(x)/len(x)
        if math.fsum(z*z for z in x)/len(x)<2*mean*mean:low=middle
        else:high=middle
    for delta in [-1e-6,0.,1e-6]:
        result.append((f"exponential-seam-{delta}",seam_sample((low+high)/2+delta),math.nan,math.nan))
    rng = random.Random(1162)
    x = [math.expm1(-.3*math.log1p(-rng.random()))/.3 for _ in range(10000)]
    result += [("seed1162-n10000", x, math.nan, math.nan),
               ("held-scale", [.1,.3,.5,1.,2.,4.], 1., math.nan),
               ("held-scale-ties", [0.,0.,.1,.3,1.,2.,4.], 1., math.nan),
               ("held-scale-uniform", [0.,.1,1.,2.], 3., math.nan),
               ("uniform", [1.,2.,3.,4.], math.nan, math.nan),
               ("constant", [2.]*4, math.nan, math.nan)]
    for factor in [1e-200, 1e200]:
        result.append((f"rescale-{factor}", [factor*v for v in golden["data"]], math.nan, math.nan))
    for shape in [-.8, 0., .3]:
        result.append((f"held-shape-{shape}", golden["data"], math.nan, shape))
    result.append(('held-shape-zero-subnormal',[math.ulp(0.),3*math.ulp(0.)],math.nan,0.))
    return result, golden

def refine(f,x):
    # Stop on the residual before proposing another step. Secant refinement
    # in this mpmath/Python combination can jump away after an already
    # converged iterate when two residuals round to the same tiny value.
    for _ in range(24):
        value=f(x)
        if abs(value)<mp.mpf('1e-55'):
            return x
        derivative=mp.diff(f,x)
        assert derivative != 0
        x-=value/derivative
    raise AssertionError("independent Newton residual did not converge")

def independent(x, scale, shape, sigma, xi):
    values = list(map(mp.mpf, x)); M=max(values); n=len(values)
    def loglik(s,k):
        if k == -1:return -n*mp.log(s)
        if k == 0:return -n*mp.log(s)-mp.fsum(values)/s
        return -n*mp.log(s)-(1+1/k)*mp.fsum(mp.log1p(k*v/s) for v in values)
    if math.isfinite(xi):
        k=mp.mpf(xi)
        if k==-1:s=M
        elif k==0:s=mp.fsum(values)/n
        else:
            score=lambda s:mp.fsum((1+k)*v/(s+k*v) for v in values)/n-1
            s=refine(score,mp.mpf(scale))
    elif math.isfinite(sigma):
        s=mp.mpf(sigma)
        if s>=M:k=mp.mpf(-1)
        else:
            def score(k):
                if abs(k)<mp.mpf('1e-50'):
                    return mp.fsum((v/s)**2/2-v/s for v in values)
                return mp.fsum(mp.log1p(k*v/s)/k**2-(1+1/k)*(v/s)/(1+k*v/s) for v in values)
            k=refine(score,mp.mpf(shape))
    elif shape==-1:
        s=M;k=mp.mpf(-1)
    else:
        v=[y/M for y in values]
        t0=mp.log1p(mp.mpf(shape)*M/mp.mpf(scale))
        moments=None
        if abs(t0)<mp.mpf('.001'):
            moments=[mp.fsum(z**j for z in v)/n for j in range(28)]
        def values_at(b):
            if moments is not None and abs(b)<mp.mpf('.001'):
                D=mp.fsum((-b)**j*moments[j+1]/(j+1) for j in range(25))
                P=mp.fsum((-b)**j*(j+1)*moments[j+2]/(j+2) for j in range(25))
            else:
                D=mp.fsum(mp.log1p(b*z)/b for z in v)/n
                P=mp.fsum((mp.log1p(b*z)-b*z/(1+b*z))/b**2 for z in v)/n
            return D,P
        def profile_score(t):
            b=mp.expm1(t);D,P=values_at(b)
            return mp.exp(t)*(P/D-D+b*P)
        t=refine(profile_score,t0)
        b=mp.expm1(t);D,_=values_at(b)
        s=M*D;k=b*D
    return s,k,loglik(s,k)

def main():
    parser=argparse.ArgumentParser();parser.add_argument('--output',type=Path,required=True);args=parser.parse_args()
    print("Compiling comparison driver", flush=True)
    compile_cmd=prepare();jobs,golden=workloads()
    print("Running baseline and certified workloads", flush=True)
    payload=''.join(f"{len(x)} 5 {sigma} {xi} "+' '.join(map(repr,x))+'\n' for _,x,sigma,xi in jobs)
    run=subprocess.run([str(SCRATCH/'runner')],input=payload,text=True,capture_output=True,check=True)
    print("Checking independent 80-digit roots", flush=True)
    mp.mp.dps=80
    records=[]
    for (name,x,sigma,xi),line in zip(jobs,run.stdout.splitlines(),strict=True):
        print(f"Checking {name}", flush=True)
        a=list(map(float,line.split()));status,reason=map(int,a[:2]);scale,shape,ll=a[2:5]
        record=dict(name=name,n=len(x),status=status,reason=reason,sigma=scale,xi=shape,loglik=ll,work=int(a[5]),baseline_ms=a[9],certified_ms=a[10])
        print(line,flush=True)
        assert status==0,(name,line)
        s,k,l=independent(x,scale,shape,sigma,xi)
        ds=float(abs(mp.mpf(scale)/s-1));dk=float(abs(mp.mpf(shape)-k)/max(1,abs(k)));dl=float(abs(mp.mpf(ll)-l)/len(x))
        record.update(scale_relative_error=ds,shape_scaled_error=dk,mean_loglik_error=dl)
        assert ds<1e-6 and dk<1e-6 and dl<1e-8,(name,record)
        if name=='golden':
            assert abs(scale/golden['estimates']['sigma']-1)<1e-12
            assert abs(shape/golden['estimates']['xi']-1)<1e-12
            assert abs(ll/golden['logLik']-1)<1e-12
        records.append(record)
    hashes={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [ROOT/FAMILY/'golden.json',ROOT/FAMILY/'mle.hpp',ROOT/FAMILY/'certificate.hpp',ROOT/FAMILY/'test.cpp',ROOT/FAMILY/'README.md',ROOT/'bindings/common/distributions/gpd/fit.hpp',ROOT/'bindings/r/distributions/gpd/test.R',Path(__file__).with_name('check_uniqueness.py'),Path(__file__).resolve()]}
    output=dict(base=BASE,environment=platform.platform(),compile_command=compile_cmd,mpmath=mp.__version__,hashes=hashes,records=records)
    args.output.write_text(json.dumps(output,indent=2,allow_nan=False)+'\n')
    print(json.dumps(records,indent=2))
if __name__=='__main__':main()
