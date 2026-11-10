#include "cppunivariate/distributions/gumbel/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);
 const std::vector<double> x{2.4349403590372,2.434940359039635,2.43494035904207,2.434940359044505,2.434940359049375};
 const auto w=cppunivariate::weibull_mle({x.data(),x.size()},1e-10,100,NAN,NAN,x[0]);
 std::cout<<"{\"id\":\"weibull-held-scale\",\"shape\":"<<w.shape<<",\"status\":"<<int(w.status)<<"}\n";
 const std::vector<double> y{1,2,3};
 const auto l=cppunivariate::lomax_mle({y.data(),y.size()},1e-10,100,NAN,NAN,1e-14);
 std::cout<<"{\"id\":\"lomax-held-kappa\",\"lambda\":"<<l.lambda<<",\"status\":"<<int(l.status)<<"}\n";
 const double u=std::numeric_limits<double>::denorm_min();const std::vector<double> z{0,u};
 const auto g=cppunivariate::fit_gumbel_held({z.data(),z.size()},1e-10,100,NAN,u);
 std::cout<<"{\"id\":\"gumbel-held-scale\",\"mu_in_ulps\":"<<g.mu/u<<",\"status\":"<<int(g.status)<<"}\n";
 const std::vector<double> wdata{u,2*u};
 const auto wq=cppunivariate::weibull_mle({wdata.data(),wdata.size()},1e-10,100,NAN,1,NAN);
 std::cout<<"{\"id\":\"weibull-held-shape\",\"scale_in_ulps\":"<<(std::isfinite(wq.scale)?wq.scale/u:0)<<",\"status\":"<<int(wq.status)<<"}\n";
 struct Case {std::string id;std::vector<double> data;double kappa;};
 for(const auto& c:std::vector<Case>{{"lomax-wide-held-kappa",{1e-200,1e200},1},
     {"lomax-weighted-held-kappa",{1e-200,1e200,1e200,1e200},1./3},
     {"lomax-subnormal-held-kappa",{1e158,2e158},1e158}}) {
  const auto r=cppunivariate::lomax_mle({c.data.data(),c.data.size()},1e-10,100,NAN,NAN,c.kappa);
  std::cout<<"{\"id\":"<<std::quoted(c.id)<<",\"lambda\":"<<(std::isfinite(r.lambda)?r.lambda:0)<<",\"status\":"<<int(r.status)<<"}\n";
 }
}
