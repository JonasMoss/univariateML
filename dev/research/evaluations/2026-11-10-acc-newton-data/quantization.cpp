#include "cppunivariate/distributions/gumbel/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
#include "cppunivariate/distributions/pig/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);const double u=std::numeric_limits<double>::denorm_min();
 const std::vector<double> x{0,u},w{u,2*u};
 const auto g=cppunivariate::fit_gumbel_newton({x.data(),x.size()},1e-10,200);
 const auto v=cppunivariate::weibull_mle({w.data(),w.size()},1e-10,200);
 const auto f=cppunivariate::foldnorm_mle({x.data(),x.size()},1e-10,200);
 const double mu=1e-158;const std::vector<int> count{0,2};
 const auto p=cppunivariate::pig_mle({count.data(),count.size()},1e-10,200,mu,NAN);
 const long double ideal=static_cast<long double>(mu)*mu;
 std::cout<<"{\"id\":\"gumbel\",\"status\":"<<int(g.status)<<",\"sigma\":"<<g.sigma<<",\"mu\":"<<g.mu<<"}\n";
 std::cout<<"{\"id\":\"weibull\",\"status\":"<<int(v.status)<<",\"scale_in_ulps\":"<<v.scale/u<<",\"shape\":"<<v.shape<<"}\n";
 std::cout<<"{\"id\":\"foldnorm\",\"status\":"<<int(f.status)<<",\"sigma_in_ulps\":"<<f.sigma/u<<",\"mu_in_ulps\":"<<f.mu/u<<"}\n";
 std::cout<<"{\"id\":\"pig\",\"status\":"<<int(p.status)<<",\"shape\":"<<p.shape<<",\"relative_square_error\":"<<static_cast<double>(std::abs(static_cast<long double>(p.shape)/ideal-1))<<"}\n";
}
