"""Check all six actual-hold goldens at default budget, independently of R."""
import json,os,subprocess
from pathlib import Path
root=Path.cwd();D=root/'dev/research/evaluations/2026-11-08-std-free-data'
scratch=root/'build/scratch';parts=['#include "cppunivariate/distributions/std/mle.hpp"\n#include <chrono>\n#include <iomanip>\n#include <iostream>\nint main(){\nstd::cout<<std::setprecision(17)<<"family,held_mask,status,certified,mu,scale,shape,loglik,maximum_relative_golden_error,seconds\\n";\n']
for family,names in [('std',('mean','sd','nu')),('lst',('mu','sigma','df'))]:
    g=json.loads((root/'univariateML-cpp/include/cppunivariate/distributions'/family/'golden.json').read_text())
    values=','.join(map(repr,g['data']));m,s,v=(g['estimates'][k] for k in names)
    fun='student_t_mle' if family=='std' else 'lst_mle';shape='nu' if family=='std' else 'df'
    parts.append(' {const std::vector<double> x{'+values+'};\n')
    parts.append(f'''for(int mask:{{1,2,3}}){{
const auto start=std::chrono::steady_clock::now();
const auto r=cppunivariate::{fun}({{x.data(),x.size()}},1e-10,200,mask&1?{m!r}:NAN,mask&2?{s!r}:NAN,NAN);
const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
const double error=std::max({{std::abs(r.mu/{m!r}-1),std::abs(r.sigma/{s!r}-1),std::abs(r.{shape}/{v!r}-1)}});
if(static_cast<int>(r.status)!=0||r.certified!=(mask==3)||!std::isfinite(error)||error>3e-5||r.logLik<{g['logLik']!r}-1e-6*x.size())return 2;
std::cout<<"{family},"<<mask<<","<<static_cast<int>(r.status)<<","<<r.certified<<","<<r.mu<<","<<r.sigma<<","<<r.{shape}<<","<<r.logLik<<","<<error<<","<<seconds<<"\\n";
}} }}\n''')
parts.append('return std::cout?0:3;\n}\n')
source=scratch/'golden-default-native.cpp';source.write_text(''.join(parts))
env=os.environ.copy();env.update(TMPDIR=str(root/'build/tmp'),JUST_TEMPDIR=str(root/'build/tmp'),XDG_RUNTIME_DIR=str(root/'build/tmp'),CCACHE_DIR='/home/jonas/.cache/ccache',CCACHE_BASEDIR=str(root))
exe=scratch/'golden-default-native'
subprocess.run(['/tmp/umvl-cpu/run','ccache','c++','-O2','-std=c++17','-IunivariateML-cpp/include','-I/home/jonas/R/x86_64-pc-linux-gnu-library/4.6/BH/include',str(source),'-o',str(exe)],check=True,env=env)
subprocess.run(['/tmp/umvl-cpu/run',str(exe)],check=True,env=env)
