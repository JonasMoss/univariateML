#include "cppunivariate/distributions/frechet/mle.hpp"
#include <iostream>
int main(){
 int failed=0;auto check=[&](bool ok,const char* label){std::cout<<(ok?"PASS ":"FAIL ")<<label<<"\n";failed+=!ok;};
 const std::vector<double> x{0,.2,.5,1,3,20};
 const auto a=cppunivariate::frechet_mle({x.data(),x.size()},1e-10,1,NAN,2);
 check(a.status==cppunivariate::FrechetStatus::IterationFailure && !a.stop_detail.candidates.empty(),"Frechet solver cap retains witness");
 const std::vector<double> large{-1e308,-.9e308,0};
 const auto b=cppunivariate::frechet_mle({large.data(),large.size()},1e-10,1,NAN,2);
 check(b.status==cppunivariate::FrechetStatus::NumericalFailure && b.stop_detail.candidates.empty(),"Frechet removes unrepresentable witness");
 const double origin=1e300,next=std::nextafter(origin,INFINITY),last=std::nextafter(next,INFINITY);
 const std::vector<double> shifted{origin,next,last};
 const auto c=cppunivariate::gev_mle({shifted.data(),shifted.size()},1e-10,1,-.3,NAN,next-origin);
 bool valid=c.status==cppunivariate::GevStatus::IterationFailure && c.stop_detail.candidates.size()==1;
 if(valid){const auto& w=c.stop_detail.candidates[0];double ll=cppunivariate::gev_loglik({shifted.data(),shifted.size()},w.coef[0],w.coef[1],w.coef[2]);valid=std::isfinite(ll) && w.loglik_lower<=ll && w.loglik_upper>=ll;}
 check(valid,"GEV restored witness likelihood enclosure");
 const std::vector<double> one{origin};
 const auto d=cppunivariate::gev_mle({one.data(),one.size()},1e-10,100,-.3,NAN,1);
 check(d.status==cppunivariate::GevStatus::NumericalFailure,"GEV unrepresentable maximum");
 const std::vector<double> ratio{0,1,2,3};
 const auto e=cppunivariate::frechet_mle({ratio.data(),ratio.size()},1e-10,100,NAN,std::nextafter(1./3.,1.));
 check(e.status!=cppunivariate::FrechetStatus::InvalidData && e.status!=cppunivariate::FrechetStatus::NoMLE,"positive exact collapse exponent does not become no-MLE");
 const std::vector<double> extremes{-1e308,1e308};
 const auto face=cppunivariate::frechet_mle({extremes.data(),extremes.size()},1e-10,100,NAN,1);
 check(face.status==cppunivariate::FrechetStatus::InvalidData && std::isfinite(face.stop_detail.loglik_upper) && std::abs(face.stop_detail.loglik_upper-(-2-2*std::log(1e308)))<1e-9,"finite supremum with overflowing range");
 const auto overflow=cppunivariate::frechet_mle({extremes.data(),extremes.size()},1e-10,100,NAN,2);
 check(overflow.status==cppunivariate::FrechetStatus::NumericalFailure,"positive-exponent range overflow is numerical");
 const auto stiff=cppunivariate::frechet_mle({x.data(),x.size()},1e-10,100,NAN,1e100);
 check(stiff.status==cppunivariate::FrechetStatus::NumericalFailure,"enormous held shape stops on precision loss");
 return failed ? 1 : 0;
}
