#include <iostream>
#include <iomanip>
#include <vector>
#include "cppunivariate/distributions/skellam/mle.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
#include "cppunivariate/distributions/ht/mle.hpp"
int main(){
 std::cout<<std::setprecision(17);
 std::vector<double> x(10000000,2000000001.);double naive=0;for(double v:x)naive+=v;
 auto s=cppunivariate::skellam_mle({x.data(),x.size()});
 std::cout<<"{\"id\":\"skellam\",\"n\":"<<x.size()<<",\"naive_mean\":"<<naive/x.size()<<",\"mean\":"<<s.mu1<<",\"reference\":2000000001,\"status\":"<<int(s.status)<<"}\n";
 x.assign(10000000,.1);x.back()=1;naive=0;for(double v:x)naive+=v;
 auto g=cppunivariate::sgomp_mle({x.data(),x.size()},1e-10,200,NAN,0);
 const long double reference=x.size()/((x.size()-1)*static_cast<long double>(.1)+1);
 std::cout<<"{\"id\":\"sgomp\",\"n\":"<<x.size()<<",\"naive_rate\":"<<x.size()/naive<<",\"rate\":"<<g.b<<",\"reference\":"<<double(reference)<<",\"relative_error\":"<<double(std::abs(g.b/reference-1))<<",\"status\":"<<int(g.status)<<"}\n";
 const double d=std::numeric_limits<double>::denorm_min();x={0,d,2*d};auto h=cppunivariate::ht_mle({x.data(),x.size()},0,1,NAN);
 std::cout<<"{\"id\":\"ht\",\"scale_over_denorm\":"<<h.sigma/d<<",\"exact_scale_over_denorm\":"<<std::sqrt((std::sqrt(73.)-5)/6)<<",\"status\":"<<int(h.status)<<"}\n";
 return s.status==cppunivariate::SkellamStatus::Ok&&s.mu1==2000000001.&&s.mu2==0&&g.status==cppunivariate::SgompStatus::Ok&&std::abs(g.b/reference-1)<1e-10L&&h.status==cppunivariate::HtStatus::AccuracyFailure?0:1;
}
