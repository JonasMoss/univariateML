// Streaming bridge for the seeded calibration; no R linkage.
#include <iostream>
#include <iomanip>
#include <cppunivariate/distributions/norm/mle.hpp>
#include <cppunivariate/distributions/gamma/mle.hpp>
#include <cppunivariate/distributions/beta/mle.hpp>
#include <cppunivariate/distributions/norm/score.hpp>
#include <cppunivariate/distributions/gamma/score.hpp>
#include <cppunivariate/distributions/beta/score.hpp>
#include <cppunivariate/distributions/pois/score.hpp>
int main() {
  std::cout << std::setprecision(17);
  std::string family,action; std::size_t n; double a,b;
  while(std::cin>>family>>action>>a>>b>>n) {
    std::vector<double> x(n);for(auto& v:x) std::cin>>v;
    cppunivariate::span data{x.data(),n}; int status=0;double ll=0;
    if(action=="targetmean" || action=="targetshape") {
      cppunivariate::TargetInferenceResult t;
      if(family=="norm") t=cppunivariate::norm_mean_inference(data,a);
      if(family=="gamma") t=cppunivariate::gamma_target_inference(data,action=="targetshape"?cppunivariate::GammaTarget::Shape:cppunivariate::GammaTarget::Mean,a);
      if(family=="beta") t=cppunivariate::beta_mean_inference(data,a);
      if(family=="pois") t=cppunivariate::pois_rate_inference(data,a);
      std::cout<<static_cast<int>(t.status)<<' '<<t.estimate<<' '<<static_cast<int>(t.uncertainty.status)<<' '<<t.uncertainty.variance;
      for(auto test:{t.wald,t.score,t.likelihood_ratio}) std::cout<<' '<<static_cast<int>(test.status)<<' '<<test.statistic;
      std::cout<<std::endl;continue;
    }
    if(action=="interval") {
      cppunivariate::InferenceInterval ci;
      if(family=="norm") ci=cppunivariate::norm_mean_lr_interval(data,b);
      if(family=="gamma") ci=cppunivariate::gamma_target_lr_interval(data,a==1?cppunivariate::GammaTarget::Shape:cppunivariate::GammaTarget::Mean,b);
      if(family=="beta") ci=cppunivariate::beta_mean_lr_interval(data,b);
      if(family=="pois") ci=cppunivariate::pois_rate_lr_interval(data,b);
      std::cout<<static_cast<int>(ci.status)<<' '<<ci.lower<<' '<<ci.upper<<std::endl;continue;
    }
    if(action=="exact") {
      const auto ci=family=="norm"?cppunivariate::norm_mean_exact_interval(data):cppunivariate::pois_rate_exact_interval(data);
      std::cout<<static_cast<int>(ci.status)<<' '<<ci.lower<<' '<<ci.upper<<std::endl;continue;
    }
    if(action=="fit") {
      if(family=="gamma") {const auto f=cppunivariate::gamma_mle(data,1e-11,2000);a=f.shape;b=f.scale;ll=f.logLik;status=static_cast<int>(f.status);}
      if(family=="beta") {const auto f=cppunivariate::beta_mle(data,1e-11,2000);a=f.alpha;b=f.beta;ll=f.logLik;status=static_cast<int>(f.status);}
      if(family=="norm") {const auto f=cppunivariate::norm_mle(data);a=f.mean;b=f.sd;ll=f.logLik;status=static_cast<int>(f.status);}
      if(family=="pois") a=cppunivariate::mean(data);
    }
    if(action=="shape") {const auto f=cppunivariate::gamma_mle(data,1e-11,2000,a,NAN);a=f.shape;b=f.scale;ll=f.logLik;status=static_cast<int>(f.status);}
    cppunivariate::InferenceResult r;
    if(family=="norm") r=cppunivariate::norm_inference(data,a,b);
    if(family=="gamma") r=cppunivariate::gamma_inference(data,a,b);
    if(family=="beta") r=cppunivariate::beta_inference(data,a,b);
    if(family=="pois") r=cppunivariate::pois_inference(data,a);
    std::cout<<status<<' '<<a<<' '<<b<<' '<<ll<<' '<<static_cast<int>(r.status);
    for(auto kind:{cppunivariate::CovarianceKind::Fisher,cppunivariate::CovarianceKind::Observed,cppunivariate::CovarianceKind::Sandwich}) {
      const auto c=cppunivariate::inference_covariance(r,kind);
      std::cout<<' '<<static_cast<int>(c.status);
      for(std::size_t i=0;i<r.dimension*r.dimension;++i) std::cout<<' '<<c.matrix[i];
    }
    const auto score=cppunivariate::inference_rao(r);
    std::cout<<' '<<static_cast<int>(score.status)<<' '<<score.statistic<<std::endl;
  }
}
