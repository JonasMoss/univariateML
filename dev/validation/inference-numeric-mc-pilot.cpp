// Standard-family pilot only. One independently seeded row per replication;
// caller persists rows as they are produced. n=500, no installed R required.
#include "cppunivariate/core/inference_families.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/geom/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include "cppunivariate/distributions/lgamma/mle.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include "cppunivariate/distributions/binom/mle.hpp"
#include <iostream>
#include <iomanip>
#include <random>
#include <vector>
#include <string>
#include <cmath>

int main(int argc,char** argv) {
  if(argc!=4) return 2;
  const std::string family=argv[1]; const int first=std::stoi(argv[2]),tag=std::stoi(argv[3]);
  std::cout<<std::setprecision(17);
  for(int rep=first;rep<200;++rep) {
    std::mt19937_64 gen(20261101ULL+1000ULL*tag+rep);
    std::normal_distribution<double> normal(0,1);
    std::gamma_distribution<double> gamma2(2,1),gamma3(3,1);
    std::poisson_distribution<int> poisson(2);
    std::geometric_distribution<int> geometric(.4);
    std::binomial_distribution<int> binomial(10,.4);
    std::uniform_real_distribution<double> uniform(0,1);
    std::vector<double> x(500);std::vector<int> counts(500);
    for(std::size_t i=0;i<x.size();++i) {
      if(family=="norm") x[i]=normal(gen);
      else if(family=="gamma") x[i]=gamma2(gen);
      else if(family=="beta") {const double a=gamma2(gen),b=gamma3(gen);x[i]=a/(a+b);}
      else if(family=="pois") x[i]=counts[i]=poisson(gen);
      else if(family=="exp") x[i]=-std::log1p(-uniform(gen));
      else if(family=="geom") x[i]=counts[i]=geometric(gen);
      else if(family=="lnorm") x[i]=std::exp(normal(gen));
      else if(family=="logitnorm") {const double z=normal(gen);x[i]=1/(1+std::exp(-z));}
      else if(family=="lgamma") x[i]=std::exp(gamma2(gen));
      else if(family=="rayleigh") x[i]=std::sqrt(-2*std::log1p(-uniform(gen)));
      else if(family=="binom") x[i]=counts[i]=binomial(gen);
      else return 3;
    }
    const cppunivariate::span data{x.data(),x.size()};
    const cppunivariate::ispan integers{counts.data(),counts.size()};
    std::array<double,cppunivariate::inference_max_parameters> p{};
    unsigned mask=1;bool ok=false;
    if(family=="norm") {const auto r=cppunivariate::norm_mle(data);p[0]=r.mean;p[1]=r.sd;mask=3;ok=r.status==cppunivariate::NormStatus::Ok;}
    else if(family=="gamma") {const auto r=cppunivariate::gamma_mle(data,1e-10,1000);p[0]=r.shape;p[1]=r.scale;mask=3;ok=r.status==cppunivariate::GammaStatus::Ok;}
    else if(family=="beta") {const auto r=cppunivariate::beta_mle(data,1e-10,1000);p[0]=r.alpha;p[1]=r.beta;mask=3;ok=r.status==cppunivariate::BetaStatus::Ok;}
    else if(family=="pois") {const auto r=cppunivariate::pois_mle(integers);p[0]=r.lambda;ok=r.status==cppunivariate::PoisStatus::Ok;}
    else if(family=="exp") {const auto r=cppunivariate::exp_mle(data);p[0]=r.rate;ok=r.status==cppunivariate::ExpStatus::Ok;}
    else if(family=="geom") {const auto r=cppunivariate::geom_mle(integers);p[0]=r.prob;ok=r.status==cppunivariate::GeomStatus::Ok;}
    else if(family=="lnorm") {const auto r=cppunivariate::lognormal_mle(data);p[0]=r.meanlog;p[1]=r.sdlog;mask=3;ok=r.status==cppunivariate::LognormalStatus::Ok;}
    else if(family=="logitnorm") {const auto r=cppunivariate::logitnorm_mle(data);p[0]=r.mu;p[1]=r.sigma;mask=3;ok=r.status==cppunivariate::LogitnormStatus::Ok;}
    else if(family=="lgamma") {const auto r=cppunivariate::lgamma_mle(data,1e-10,1000);p[0]=r.shapelog;p[1]=r.ratelog;mask=3;ok=r.status==cppunivariate::LgammaStatus::Ok;}
    else if(family=="rayleigh") {const auto r=cppunivariate::rayleigh_mle(data);p[0]=r.sigma;ok=r.status==cppunivariate::RayleighStatus::Ok;}
    else if(family=="binom") {const auto r=cppunivariate::binom_mle(integers,10,1e-10,1000);p[0]=r.size;p[1]=r.prob;mask=2;ok=r.status==cppunivariate::BinomStatus::Ok;}
    if(!ok) {std::cout<<rep<<" fit_failure\n"<<std::flush;continue;}
    const auto info=cppunivariate::family_inference(data,family,p,mask);
    const auto model=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Observed);
    const auto robust=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Sandwich);
    if(info.status!=cppunivariate::InferenceStatus::Ok || model.status!=cppunivariate::InferenceStatus::Ok || robust.status!=cppunivariate::InferenceStatus::Ok) {
      std::cout<<rep<<" inference_failure\n"<<std::flush;continue;
    }
    std::cout<<rep<<" ok";
    for(std::size_t j=0;j<info.dimension;++j)
      std::cout<<" "<<p[info.free_indices[j]]<<" "<<std::sqrt(model.matrix[j*info.dimension+j])<<" "<<std::sqrt(robust.matrix[j*info.dimension+j]);
    std::cout<<"\n"<<std::flush;
  }
}
