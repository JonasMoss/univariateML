// Compare generated independent references with native or double-only fitters.
// Compile with -IunivariateML-cpp/include -IunivariateML-cpp/tests.
#include "censored_reference.hpp"
#include "cppunivariate/distributions/exp/censored.hpp"
#include "cppunivariate/distributions/weibull/censored.hpp"
#include "cppunivariate/distributions/rayleigh/censored.hpp"
#include "cppunivariate/distributions/pareto/censored.hpp"
#include "cppunivariate/distributions/lomax/censored.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
  std::cout << "family,label,held,relative_error,absolute_logLik_error,exact_relative_difference\n" << std::setprecision(17);
  for(const auto& c:censored_reference::cases()) {
    std::vector<double> u=c.lower;
    for(std::size_t i=0;i<u.size();++i)if(!c.exact[i])u[i]=INFINITY;
    const cppunivariate::CensoredData x{{c.lower.data(),c.lower.size()},{u.data(),u.size()}};
    double coef=NAN,ll=NAN;bool ok=false;
    const std::string family=c.family;
    if(family=="exp") {const auto r=cppunivariate::exp_censored_mle(x);coef=r.rate;ll=r.logLik;ok=r.status==cppunivariate::ExpCensoredStatus::Ok;}
    if(family=="weibull") {const auto r=cppunivariate::weibull_censored_mle(x,c.held);coef=r.scale;ll=r.logLik;ok=r.status==cppunivariate::WeibullCensoredStatus::Ok;}
    if(family=="rayleigh") {const auto r=cppunivariate::rayleigh_censored_mle(x);coef=r.sigma;ll=r.logLik;ok=r.status==cppunivariate::RayleighCensoredStatus::Ok;}
    if(family=="pareto") {const auto r=cppunivariate::pareto_censored_mle(x,NAN,c.held);coef=r.shape;ll=r.logLik;ok=r.status==cppunivariate::ParetoCensoredStatus::Ok;}
    if(family=="lomax") {const auto r=cppunivariate::lomax_censored_mle(x,c.held);coef=r.kappa;ll=r.logLik;ok=r.status==cppunivariate::LomaxCensoredStatus::Ok;}
    if(!ok || !(std::abs(coef/c.coefficient-1)<1e-10))return 1;
    double exact_difference=0;
    if(std::string(c.label)=="0pct" || std::string(c.label)=="single") {
      double old=NAN;
      if(family=="exp")old=cppunivariate::exp_mle(x.lower).rate;
      if(family=="weibull")old=cppunivariate::weibull_mle(x.lower,1e-10,1000,NAN,c.held,NAN).scale;
      if(family=="rayleigh")old=cppunivariate::rayleigh_mle(x.lower).sigma;
      if(family=="pareto")old=cppunivariate::pareto_mle(x.lower,NAN,c.held).shape;
      if(family=="lomax")old=cppunivariate::lomax_mle(x.lower,1e-10,1000,NAN,c.held,NAN).kappa;
      exact_difference=std::abs(coef/old-1);
    }
    std::cout<<family<<','<<c.label<<','<<c.held<<','<<std::abs(coef/c.coefficient-1)<<','<<std::abs(ll-c.logLik)<<','<<exact_difference<<'\n';
  }
}
