#include "cppunivariate/distributions/std/mle.hpp"
#include <cassert>
#include <iostream>
int main(){
  const std::vector<double> x{-100,-2,-.01,0,.01,2,100};
  const cppunivariate::span data{x.data(),x.size()};
  const auto shape=cppunivariate::t_location_detail::decreasing_shape(data,1,200);
  std::cout<<"shape proof "<<shape.result<<" evaluations "<<shape.evaluations<<std::endl;
  assert(shape.result==1);
  const auto fit=cppunivariate::lst_mle(data,1e-7,200,NAN,1,NAN);
  std::cout<<"fit status "<<int(fit.status)<<" mu "<<fit.mu<<" scale "<<fit.sigma<<" df "<<fit.df<<" evaluations "<<fit.iterations<<std::endl;
  assert(fit.status==cppunivariate::LstStatus::Ok);
  assert(fit.restricted==16u&&fit.df==1&&std::abs(fit.mu)<1e-8);
  const auto limited=cppunivariate::lst_mle(data,1e-7,1,NAN,1,NAN);
  assert(limited.status!=cppunivariate::LstStatus::Ok);
  const double center=1e7;
  const std::vector<double> shifted{center,center,std::nextafter(center,INFINITY),center+1};
  const auto translated=cppunivariate::t_location_detail::solve({shifted.data(),shifted.size()},1e-4,3,500);
  std::cout<<"translated status "<<translated.failure<<std::endl;
  assert(translated.failure==0);
  for(const auto& candidate:translated.detail.candidates){
    const double p[2]={candidate.coef[0],std::log(candidate.coef[1])};
    const double ll=-cppunivariate::LstCost{{shifted.data(),shifted.size()},3,true}(p);
    assert(ll>=candidate.loglik_lower&&ll<=candidate.loglik_upper);
  }
  const double third=1./3;
  const std::vector<double> critical{0,1,2,3};
  assert(std::fma(third,3.,-1.)<0);
  assert(cppunivariate::lst_mle({critical.data(),critical.size()},1e-7,200,0,NAN,third).status==cppunivariate::LstStatus::Unbounded);
  const double a=1e7,bb=std::nextafter(a,INFINITY),cc=std::nextafter(bb,INFINITY),dd=std::nextafter(cc,INFINITY);
  const std::vector<double> adjacent{a,bb,cc,dd};
  const auto stopped=cppunivariate::lst_mle_fixed_df({adjacent.data(),adjacent.size()},3,1e-7,500);
  std::cout<<"translated joint status "<<int(stopped.status)<<std::endl;
  assert(stopped.status==cppunivariate::LstStatus::NumericalFailure);
  assert(!stopped.stop_detail.candidates.empty());
  for(const auto& candidate:stopped.stop_detail.candidates){
    const double p[2]={candidate.coef[0],std::log(candidate.coef[1])};
    const double ll=-cppunivariate::LstCost{{adjacent.data(),adjacent.size()},3,true}(p);
    assert(ll>=candidate.loglik_lower&&ll<=candidate.loglik_upper);
  }
  const double endpoint=std::nextafter(a,INFINITY);
  const std::vector<double> balanced{a,a,endpoint,endpoint};
  const auto rounded=cppunivariate::lst_mle_fixed_df({balanced.data(),balanced.size()},1,1e-7,200);
  std::cout<<"rounded Cauchy status "<<int(rounded.status)<<std::endl;
  assert(rounded.status==cppunivariate::LstStatus::NumericalFailure);
  const std::vector<double> rescued{a,a,dd,dd};
  const auto curve=cppunivariate::lst_mle_fixed_df({rescued.data(),rescued.size()},1,1e-7,200);
  std::cout<<"Cauchy curve status "<<int(curve.status)<<std::endl;
  assert(curve.status==cppunivariate::LstStatus::Ok);
  assert(curve.mu>a&&curve.mu<dd);
  assert(std::abs(curve.sigma/(bb-a)-std::sqrt(2.))<1e-6);
  const double near_two=std::nextafter(2.,INFINITY);
  const std::vector<double> enormous{-1e301,1e301};
  for(double held_mu:std::vector<double>{NAN,0.}) {
    const auto transport=cppunivariate::student_t_mle({enormous.data(),enormous.size()},1e-7,500,held_mu,NAN,near_two);
    std::cout<<"std transport status "<<int(transport.status)<<std::endl;
    assert(transport.status==cppunivariate::StudentTStatus::NumericalFailure);
    assert(std::isfinite(transport.stop_detail.loglik_lower));
    assert(!transport.stop_detail.candidates.empty());
  }
  const std::vector<double> finite_sd{-1e300,1e300};
  const auto large_finite=cppunivariate::student_t_mle({finite_sd.data(),finite_sd.size()},1e-7,500,0,NAN,near_two);
  assert(large_finite.status==cppunivariate::StudentTStatus::Ok&&std::isfinite(large_finite.sigma));
  const std::vector<double> capped{-3,-1,.2,1,8};
  const auto cap=cppunivariate::lst_mle_fixed_df({capped.data(),capped.size()},3,1e-7,1);
  assert(cap.status==cppunivariate::LstStatus::IterationFailure);
  assert(std::isfinite(cap.stop_detail.loglik_lower));
  assert(cap.stop_detail.loglik_upper==INFINITY);
  const std::vector<double> spread{-2,-1,0,1,3};
  const auto wide=cppunivariate::lst_mle({spread.data(),spread.size()},1e-7,500,NAN,1e100,3);
  std::cout<<"wide-scale location "<<wide.mu<<" status "<<int(wide.status)<<std::endl;
  assert(wide.status==cppunivariate::LstStatus::Ok&&std::abs(wide.mu-.2)<1e-7);
  const auto wide_std=cppunivariate::student_t_mle({spread.data(),spread.size()},1e-7,500,NAN,1e100,3);
  assert(wide_std.status==cppunivariate::StudentTStatus::Ok&&std::abs(wide_std.mu-.2)<1e-7);
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> microscopic{tiny,2*tiny},symmetric{-2*tiny,2*tiny};
  const auto microscopic_fit=cppunivariate::lst_mle({microscopic.data(),microscopic.size()},1e-7,500,0,NAN,.001);
  assert(microscopic_fit.status==cppunivariate::LstStatus::NumericalFailure);
  const auto symmetric_fit=cppunivariate::student_t_mle({symmetric.data(),symmetric.size()},1e-7,500,0,NAN,3);
  assert(symmetric_fit.status==cppunivariate::StudentTStatus::NumericalFailure);
  const std::vector<double> zero{0,0};
  const auto exact=cppunivariate::student_t_mle({zero.data(),zero.size()},1e-7,500,NAN,3e-316,near_two);
  assert(exact.status==cppunivariate::StudentTStatus::Ok&&std::isfinite(exact.logLik)&&exact.mu==0);

}
