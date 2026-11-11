#include "common/distributions/gb2/fit.hpp"
#include "common/distributions/sged/fit.hpp"
#include "common/distributions/invparalogis/fit.hpp"
#include "common/distributions/invpareto/fit.hpp"
#include "common/distributions/pareto2/fit.hpp"
#include "common/distributions/pareto3/fit.hpp"
#include "common/distributions/trgamma/fit.hpp"
#include "common/distributions/fpareto/fit.hpp"
#include "common/distributions/burr/fit.hpp"
#include "common/distributions/invburr/fit.hpp"
#include "common/distributions/pareto4/fit.hpp"
#include "common/distributions/paralogis/fit.hpp"
#include "common/distributions/betapr/fit.hpp"
#include "common/distributions/bhatt/fit.hpp"
#include "common/distributions/ged/fit.hpp"
#include <iostream>
#include <iomanip>
#include <map>
int main() {
 std::string family,id;int n,h,it;double tol;
 while(std::cin>>family>>id>>n>>h>>it>>tol) {
  std::vector<double>x(n);for(double&v:x)std::cin>>v;
  std::map<std::string,umvl_binding::Held> held;
  for(int j=0;j<h;++j){std::string k;double v;std::cin>>k>>v;held[k]={true,v};}
  cppunivariate::span data{x.data(),x.size()};umvl_binding::FitResult r;
  if(family=="burr")r=umvl_binding::fit_burr(data,held["shape1"],held["shape2"],held["scale"],tol,it);
  if(family=="invburr")r=umvl_binding::fit_invburr(data,held["shape1"],held["shape2"],held["scale"],tol,it);
  if(family=="pareto4")r=umvl_binding::fit_pareto4(data,held["shape1"],held["shape2"],held["scale"],held["min"],tol,it);
  if(family=="paralogis")r=umvl_binding::fit_paralogis(data,held["shape"],held["scale"],{},tol,it);
  if(family=="betapr")r=umvl_binding::fit_betapr(data,held["shape1"],held["shape2"],held["scale"],tol,it);
  if(family=="bhatt")r=umvl_binding::fit_bhatt(data,held["mu"],held["sigma"],held["a"],tol,it);
  if(family=="ged")r=umvl_binding::fit_ged(data,held["mean"],held["sd"],held["nu"],tol,it);
  if(family=="gb2")r=umvl_binding::fit_gb2(data,held["shape1"],held["shape2"],held["shape3"],held["scale"],tol,it);
  if(family=="sged")r=umvl_binding::fit_sged(data,held["mean"],held["sd"],held["nu"],held["xi"],tol,it);
  if(family=="invparalogis")r=umvl_binding::fit_invparalogis(data,held["shape"],held["scale"],{},tol,it);
  if(family=="invpareto")r=umvl_binding::fit_invpareto(data,held["shape"],held["scale"],{},tol,it);
  if(family=="pareto2")r=umvl_binding::fit_pareto2(data,held["shape"],held["scale"],held["min"],tol,it);
  if(family=="pareto3")r=umvl_binding::fit_pareto3(data,held["shape"],held["scale"],held["min"],tol,it);
  if(family=="trgamma")r=umvl_binding::fit_trgamma(data,held["shape1"],held["shape2"],held["scale"],tol,it);
  if(family=="fpareto")r=umvl_binding::fit_fpareto(data,held["shape1"],held["shape2"],held["shape3"],held["scale"],held["min"],tol,it);
  bool certified=true;for(const auto& e:r.extra)if(e.first=="certified")certified=e.second!=0;
  std::cout<<id<<'\t'<<family<<'\t'<<(r.error.empty()?(certified?"certified":"uncertified"):"stop:"+(r.error_kind.empty()?std::string("input-or-unclassified"):r.error_kind))<<'\t'<<std::setprecision(17)<<r.logLik;
  for(const auto&e:r.estimates)std::cout<<'\t'<<e.first<<'='<<e.second;
  if(!r.error.empty())std::cout<<"\tmessage="<<r.error;
  std::cout<<'\n'<<std::flush;
 }
}
