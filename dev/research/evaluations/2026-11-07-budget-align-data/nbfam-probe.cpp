#include "cppunivariate/distributions/ztnbinom/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main() {
 int n,maxit;double r,p;while(std::cin>>n>>r>>p>>maxit){std::vector<int>x(n);for(auto&v:x)std::cin>>v;
 const auto start=std::chrono::steady_clock::now();
 const auto old=cppunivariate::ztnbinom_candidate_mle({x.data(),x.size()},1e-10,maxit,r<0?NAN:r,p<0?NAN:p);
 const auto middle=std::chrono::steady_clock::now();
 const auto f=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,maxit,r<0?NAN:r,p<0?NAN:p);
 std::cout<<std::setprecision(17)<<int(f.status)<<' '<<f.size<<' '<<f.prob<<' '<<f.logLik<<' '<<f.iterations<<' '<<int(old.status)<<' '<<old.size<<' '<<old.prob<<' '<<old.logLik<<' '<<std::chrono::duration<double>(middle-start).count()<<' '<<std::chrono::duration<double>(std::chrono::steady_clock::now()-middle).count()<<' '<<int(f.budget_exhausted)<<' '<<int(f.certificate_failure)<<'\n';
 const cppunivariate::ztnbinom_cert_detail::Data d({x.data(),x.size()});
 for(double a:{.001,.05,.8,2.,50.,500.}) {
 const double b=1.05*a;
 const auto ta=cppunivariate::ztnbinom_cert_detail::theta(a,d.m),tb=cppunivariate::ztnbinom_cert_detail::theta(b,d.m);
 const auto s=cppunivariate::ztnbinom_cert_detail::score(d,a,b,ta,tb),der=cppunivariate::ztnbinom_cert_detail::derivative(d,a,b,ta,tb);
 std::cout<<"B "<<a<<' '<<b<<' '<<s.lo<<' '<<s.hi<<' '<<der.lo<<' '<<der.hi<<'\n';
 }
 }
}
