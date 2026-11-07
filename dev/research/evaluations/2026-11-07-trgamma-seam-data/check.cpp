// Standalone numerical evidence driver; stdin: inverse scale a b maxit n x...
#include <cppunivariate/distributions/invtrgamma/mle.hpp>
#include <cppunivariate/distributions/trgamma/dpqr.hpp>
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <string>
int main() {
  int inverse,maxit,n;double scale,a,b;
  std::string ss,sa,sb;
  std::cin>>inverse>>ss>>sa>>sb>>maxit>>n;
  scale=std::strtod(ss.c_str(),nullptr);a=std::strtod(sa.c_str(),nullptr);b=std::strtod(sb.c_str(),nullptr);
  std::vector<double>x(n);for(double&v:x){std::string token;std::cin>>token;v=std::strtod(token.c_str(),nullptr);}
  if(a<0)a=NAN;if(b<0)b=NAN;
  if(inverse==2) {
    std::cout<<std::setprecision(17)<<0<<" "<<a<<" "<<b<<" "<<scale<<" "<<cppunivariate::distributions::dtrgamma(x[0],a,b,scale,true)<<" 0 0\n";
    return 0;
  }
  double k,p,s,ll;int status,it;bool budget;
  if(inverse) {
    const auto r=scale>0 ? cppunivariate::invtrgamma_mle_fixed_scale({x.data(),x.size()},scale,a,b,1e-10,maxit) : cppunivariate::invtrgamma_mle({x.data(),x.size()},1e-10,maxit,a,b);
    k=r.shape1;p=r.shape2;s=r.scale;ll=r.logLik;status=static_cast<int>(r.status);it=r.iterations;budget=r.stop_detail.budget_exhausted;
  } else {
    const auto r=scale>0 ? cppunivariate::trgamma_mle_fixed_scale({x.data(),x.size()},scale,a,b,1e-10,maxit) : cppunivariate::trgamma_mle({x.data(),x.size()},1e-10,maxit,a,b);
    k=r.shape1;p=r.shape2;s=r.scale;ll=r.logLik;status=static_cast<int>(r.status);it=r.iterations;budget=r.stop_detail.budget_exhausted;
  }
  std::cout<<std::setprecision(17)<<status<<" "<<k<<" "<<p<<" "<<s<<" "<<ll<<" "<<it<<" "<<budget<<"\n";
}
