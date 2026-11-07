from pathlib import Path
import re,yaml
root=Path.cwd();base=Path(__file__).resolve().parent
families='bbinom betapr bnbinom burr cauchy cmb dgamma foldnorm fpareto gb2 gpd invburr invparalogis invpareto kumar lomax lst nbinom paralogis pareto2 pareto4 slash skellam sn std trgamma invtrgamma zinb zmnbinom ztnbinom zib ztbinom'.split()
out=['#include "json_loader.hpp"','#include <iostream>','#include <iomanip>','#include <chrono>','#include <random>']
for f in families:out += [f'#include "common/distributions/{f}/fit.hpp"']
out+=['int main(int argc,char** argv){std::string family=argv[1];int mode=std::stoi(argv[2]);std::mt19937 rng(143);std::cout<<std::setprecision(17);']
for f in families:
 text=(root/f'bindings/common/distributions/{f}/fit.hpp').read_text();sig=re.search(r'inline FitResult fit_'+f+r'\(([^\n]+)\)\s*\{',text)[1];names=re.findall(r'Held (\w+)_arg',sig)
 meta=yaml.safe_load((root/f'univariateML-cpp/include/cppunivariate/distributions/{f}/README.md').read_text().split('---')[1]);required={p['name'] for p in meta['params'] if p.get('kind') in ['threshold','design']}
 args=[]
 for name in names:
  held=str(name in required).lower()
  value=f'g.estimates.at("{name}")'
  if f in ['bbinom','cmb'] and name=='size':held=f'({held} && mode!=3)'
  special=(name=='scale' and f in ['burr','invburr','paralogis','invparalogis','cauchy','betapr','gb2','fpareto','pareto4'])
  special=special or (f=='foldnorm' and name=='mu') or (f=='cmb' and name=='nu') or (f=='bbinom' and name=='alpha')
  special=special or (f=='sn' and name=='omega') or (f in ['nbinom','zib','ztbinom'] and name=='prob') or (f=='zib' and name=='pi')
  special=special or (f=='kumar' and name=='b') or (f in ['gb2','fpareto'] and name=='shape3') or (f=='betapr' and name=='shape1')
  if special:held=f'({held} || mode==3)'
  if f in ['gb2','fpareto'] and name=='shape3' or f=='betapr' and name=='shape1':value=f'(mode==3?1.:{value})'
  if f in ['zib','ztbinom'] and name=='prob':value=f'(mode==3?1e-6:{value})'
  args.append(f'umvl_binding::Held{{{held},{value}}}')
 out += [f'if(family=="{f}"){{',f'auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/{f}/golden.json");',
 'std::vector<double> data;for(int i=0;i<100000;++i){double v=g.data[i%g.data.size()];',
 'if(mode==1)v*=1+1e-4*std::normal_distribution<double>()(rng);',
 'if(mode==2)v=(i%10==0?1.:1e-8)*v;',
 'data.push_back(v);}']
 if f in ['ztnbinom','zmnbinom','zinb']:
  out += ['if(mode==2){for(int i=0;i<100000;++i){int j=i%10000;data[i]=j<3958?1:j<8021?2:4;}}']
  if f!='ztnbinom':out += ['if(mode==2){for(int i=0;i<40000;++i)data[i]=0;}']
 if f=='skellam':out+=['for(double& v:data)v=std::round(v);']
 if f=='dgamma':out+=['for(double& v:data)v=std::max(0.,std::round(v));']
 if 'cppunivariate::ispan' in sig:
  if f in ['ztnbinom','ztbinom']:out+=['for(double& v:data)v=std::max(1.,std::round(v));']
  out+=['std::vector<int> integer;for(double v:data)integer.push_back(std::max(0,int(std::round(v))));cppunivariate::ispan x{integer.data(),integer.size()};']
 else:out+=['cppunivariate::span x{data.data(),data.size()};']
 out+=['auto start=std::chrono::steady_clock::now();',f'const auto fit=umvl_binding::fit_{f}(x,'+', '.join(args)+');',
 'std::cout<<family<<"\\t"<<mode<<"\\t"<<data.size()<<"\\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\\t"<<fit.error_kind<<"\\t"<<fit.error<<std::endl;return 0;}']
 out+=[]
out+=['return 2;}'];(base/'stress.cpp').write_text('\n'.join(out)+'\n')
