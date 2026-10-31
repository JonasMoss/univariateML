// Streaming production bridge following inference-4-probe.cpp. No R linkage.
#include <iostream>
#include <iomanip>
#include <cppunivariate/distributions/laplace/inference.hpp>
#include <cppunivariate/distributions/pareto/inference.hpp>
#include <cppunivariate/distributions/exp/inference.hpp>
#include <cppunivariate/distributions/unif/inference.hpp>
#include <cppunivariate/distributions/norm/inference.hpp>
#include <cppunivariate/distributions/triang/inference.hpp>
int main() {
  std::cout<<std::setprecision(17);
  std::string family; std::size_t n;double parameter;
  while(std::cin>>family>>parameter>>n) {
    std::vector<double> x(n);for(auto& v:x) std::cin>>v;
    const cppunivariate::span data{x.data(),n};
    std::vector<cppunivariate::InferenceInterval> cis;
    if(family=="laplace") cis={cppunivariate::laplace_location_wald_interval(data),cppunivariate::median_sandwich_interval(data),cppunivariate::median_sign_interval(data)};
    if(family=="pareto") cis={cppunivariate::pareto_shape_exact_interval(data),cppunivariate::pareto_shape_exact_interval(data,.05,parameter)};
    if(family=="exp") cis={cppunivariate::exp_rate_exact_interval(data)};
    if(family=="norm") cis={cppunivariate::norm_variance_exact_interval(data),cppunivariate::norm_variance_exact_interval(data,.05,parameter)};
    if(family=="unif") cis={cppunivariate::unif_exact_interval(data,cppunivariate::UniformTarget::Minimum),cppunivariate::unif_exact_interval(data,cppunivariate::UniformTarget::Maximum),cppunivariate::unif_exact_interval(data,cppunivariate::UniformTarget::Width),cppunivariate::unif_endpoint_known_opposite_interval(data,true,0),cppunivariate::unif_endpoint_known_opposite_interval(data,false,1)};
    if(family=="triang") {
      cis={cppunivariate::triang_mode_wald_interval(data,0,1)};
      const auto t=cppunivariate::triang_mode_tests(data,0,1,parameter);
      for(auto r:{t.score,t.likelihood_ratio}) std::cout<<static_cast<int>(r.status)<<' '<<r.statistic<<' ';
    }
    for(const auto& r:cis) std::cout<<static_cast<int>(r.status)<<' '<<r.lower<<' '<<r.upper<<' ';
    std::cout<<std::endl;
  }
}
