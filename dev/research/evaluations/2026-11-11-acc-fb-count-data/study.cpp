#include "bindings/common/distributions/bbinom/fit.hpp"
#include "bindings/common/distributions/hyper/fit.hpp"
#include "bindings/common/distributions/nhyper/fit.hpp"
#include "bindings/common/distributions/tbinom/fit.hpp"
#include "bindings/common/distributions/ztbinom/fit.hpp"
#include "bindings/common/distributions/zib/fit.hpp"
#include "bindings/common/distributions/zmbinom/fit.hpp"
#include "bindings/common/distributions/bnbinom/fit.hpp"
#include "bindings/common/distributions/skellam/fit.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::string family;int id,n;double size,alpha,beta,m,N,k,r,p,pi,p0,a,b;int maxit;double tol,mu1,mu2;
 while(std::cin>>id>>family>>n){
  std::vector<double>x(n);std::vector<int>ix(n);for(int j=0;j<n;++j){std::cin>>x[j];ix[j]=int(x[j]);}
  const auto read=[](){std::string t;std::cin>>t;return t=="NA"?NAN:std::stod(t);};
  size=read();alpha=read();beta=read();m=read();N=read();k=read();r=read();p=read();pi=read();p0=read();a=read();b=read();mu1=read();mu2=read();std::cin>>maxit>>tol;
  const auto held=[](double v){return umvl_binding::Held{!std::isnan(v),v};};
  umvl_binding::FitResult f;
  const cppunivariate::ispan ints{ix.data(),ix.size()};const cppunivariate::span reals{x.data(),x.size()};
  if(family=="bbinom")f=umvl_binding::fit_bbinom(ints,held(size),held(alpha),held(beta),tol,maxit);
  if(family=="hyper")f=umvl_binding::fit_hyper(reals,held(m),held(N),held(k));
  if(family=="nhyper")f=umvl_binding::fit_nhyper(reals,held(m),held(N),held(r));
  if(family=="tbinom")f=umvl_binding::fit_tbinom(ints,held(p),held(a),held(b),held(size));
  if(family=="ztbinom")f=umvl_binding::fit_ztbinom(ints,held(size),held(p),tol,maxit);
  if(family=="zib")f=umvl_binding::fit_zib(ints,held(size),held(p),held(pi),tol,maxit);
  if(family=="zmbinom")f=umvl_binding::fit_zmbinom(ints,held(size),held(p),held(p0),tol,maxit);
  if(family=="bnbinom")f=umvl_binding::fit_bnbinom(ints,held(size),held(alpha),held(beta),tol,maxit);
  if(family=="skellam")f=umvl_binding::fit_skellam(reals,held(mu1),held(mu2),tol,maxit);
  int cert=family=="skellam"?0:1;for(const auto& e:f.extra)if(e.first=="certified")cert=e.second;
  std::cout<<id<<'\t'<<family<<'\t'<<(f.error.empty()?(family=="skellam"?"unflagged":(cert?"certified":"uncertified")):("stop:"+(f.error_kind.empty()?std::string("input-or-search"):f.error_kind)))<<'\t'<<std::setprecision(17)<<f.logLik;
  for(const auto& e:f.estimates)std::cout<<'\t'<<e.first<<'='<<e.second;
  std::cout<<'\t'<<f.error<<std::endl;
 }
}
