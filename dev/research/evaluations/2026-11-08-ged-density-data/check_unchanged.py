#!/usr/bin/env python3
"""Compile old/new evaluators together and require bit-identical ordinary values.
Run inside one CPU-slot reservation; arguments: scratch-directory Boost-include.
"""
from pathlib import Path
import subprocess
import sys
scratch=Path(sys.argv[1]).resolve()
scratch.mkdir(parents=True,exist_ok=True)
root=Path.cwd()
base='cfb7c4b3'
parts=['#include <cppunivariate/distributions/sged/dpqr.hpp>']
for family in ('ged','sged'):
    path=f'univariateML-cpp/include/cppunivariate/distributions/{family}/dpqr.hpp'
    old=subprocess.check_output(['git','show',f'{base}:{path}'],text=True)
    old='\n'.join(line for line in old.splitlines() if not line.startswith(('#include','#pragma')))
    old=old.replace('namespace distributions {','namespace baseline {\nnamespace fs = cppunivariate::distributions::fs;')
    parts.append(old)
parts.append(r'''
#include <cstring>
#include <iomanip>
#include <iostream>
int main() {
  int checks=0;
  const auto same=[&](double a,double b) {
    ++checks;
    if(std::memcmp(&a,&b,sizeof(double))!=0) {
      std::cerr<<"ordinary value changed at check "<<checks<<'\n'; return false;
    }
    return true;
  };
  for(double x:{-10.,-1.,0.,.1,3.}) for(double mean:{-.3,0.,1.7})
  for(double sd:{.1,1.,100.}) for(double nu:{.02,.5,1.,2.,10.}) {
    for(bool log:{false,true}) {
      if(!same(cppunivariate::distributions::dged(x,mean,sd,nu,log),
               cppunivariate::baseline::dged(x,mean,sd,nu,log)))return 1;
      for(bool lower:{false,true})
        if(!same(cppunivariate::distributions::pged(x,mean,sd,nu,lower,log),
                 cppunivariate::baseline::pged(x,mean,sd,nu,lower,log)))return 1;
      for(double xi:{.5,1.,1.3}) {
        if(!same(cppunivariate::distributions::dsged(x,mean,sd,nu,xi,log),
                 cppunivariate::baseline::dsged(x,mean,sd,nu,xi,log)))return 1;
        for(bool lower:{false,true})
          if(!same(cppunivariate::distributions::psged(x,mean,sd,nu,xi,lower,log),
                   cppunivariate::baseline::psged(x,mean,sd,nu,xi,lower,log)))return 1;
      }
    }
  }
  for(double p:{.01,.1,.5,.9,.99}) for(double mean:{-.3,0.,1.7})
  for(double sd:{.1,1.,100.}) for(double nu:{.02,.5,1.,2.,10.})
  for(double xi:{.5,1.,1.3}) for(bool lower:{false,true}) for(bool log:{false,true}) {
    const double prob=log?std::log(p):p;
    if(!same(cppunivariate::distributions::qsged(prob,mean,sd,nu,xi,lower,log),
             cppunivariate::baseline::qsged(prob,mean,sd,nu,xi,lower,log)))return 1;
  }
  std::cout<<std::setprecision(17);
  std::cout<<"GED residual old="<<cppunivariate::baseline::dged(1e-100,0,1e300,.02,true)
           <<" new="<<cppunivariate::distributions::dged(1e-100,0,1e300,.02,true)<<'\n';
  std::cout<<"GED negative log-CDF old="<<cppunivariate::baseline::pged(-1e-100,0,1e300,.0005,true,true)
           <<" new="<<cppunivariate::distributions::pged(-1e-100,0,1e300,.0005,true,true)<<'\n';
  std::cout<<"SGED shift old="<<cppunivariate::baseline::dsged(0,0,1e300,.0003,1.3,true)
           <<" new="<<cppunivariate::distributions::dsged(0,0,1e300,.0003,1.3,true)<<'\n';
  std::cout<<"SGED quantile old="<<cppunivariate::baseline::qsged(.1,0,1e300,.0003,1.3,true,false)
           <<" new="<<cppunivariate::distributions::qsged(.1,0,1e300,.0003,1.3,true,false)<<'\n';
  std::cout<<checks<<" ordinary density/CDF/quantile values are bit-identical to cfb7c4b3\n";
}
''')
source=scratch/'unchanged.cpp'
source.write_text('\n'.join(parts))
exe=scratch/'unchanged'
subprocess.run(['g++','-std=c++17','-O2',f'-I{root}/univariateML-cpp/include',f'-I{sys.argv[2]}',str(source),'-o',str(exe)],check=True)
subprocess.run([str(exe)],check=True)
