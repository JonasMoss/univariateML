// Research-only global likelihood enclosure. It never returns an MLE.
#include "cppunivariate/distributions/emg/mle.hpp"
#include "cppunivariate/distributions/emg/fixed_shape_certificate.hpp"
#include "json_loader.hpp"
#include <queue>
#include <iostream>
#include <iomanip>
struct Segment {double a,b,left,right,upper;};
struct Compare {bool operator()(const Segment& a,const Segment& b)const{return a.upper<b.upper;}};
int main() {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  const cppunivariate::span x{g.data.data(),g.data.size()};
  const int iterlim=100;
  const auto candidate=cppunivariate::emg_mle(x,1e-10,500);
  if(candidate.status!=cppunivariate::EmgStatus::Ok)return 2;
  double lower=candidate.logLik-256*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(candidate.logLik));
  const double a=1./x.size();
  const double small=cppunivariate::emg_detail::small_shape_upper(x,a);
  double b=4,large=INFINITY;
  for(int it=0;it<cppunivariate::solver_budget(iterlim);++it) {
    large=cppunivariate::emg_detail::large_shape_upper(x,b,lower,iterlim);
    if(large<=lower)break;
    b*=2;
  }
  if(!(small<lower&&large<=lower))return 3;
  const auto first=cppunivariate::emg_detail::fixed_shape_certificate(x,a,1e-7,iterlim);
  const auto last=cppunivariate::emg_detail::fixed_shape_certificate(x,b,1e-7,iterlim);
  if(!(first.ok&&last.ok))return 4;
  std::priority_queue<Segment,std::vector<Segment>,Compare> queue;
  auto push=[&](double a,double b,double left,double right) {
    queue.push({a,b,left,right,cppunivariate::emg_detail::shape_chord_upper(left,right,x.size(),a,b)});
  };
  push(a,b,first.loglik_upper,last.loglik_upper);
  std::int64_t evaluations=2;
  std::cout<<std::setprecision(17)<<"target,status,evaluations,loglik_lower,loglik_upper,unresolved_width,segments\n";
  for(double target:{1e-6,1e-12}) {
    const char* status="objective-enclosed";
    while(queue.top().upper-lower>target) {
      const Segment top=queue.top();
      if(evaluations>=cppunivariate::certificate_budget(iterlim)) {status="budget-stop";break;}
      if(top.b-top.a<=std::sqrt(std::numeric_limits<double>::epsilon())*top.a) {status="objective-resolution-stop";break;}
      const double mid=std::sqrt(top.a)*std::sqrt(top.b);
      const auto point=cppunivariate::emg_detail::fixed_shape_certificate(x,mid,1e-7,iterlim);
      ++evaluations;
      if(!point.ok) {status="inner-numerical-stop";break;}
      lower=std::max(lower,point.loglik_lower);
      queue.pop();push(top.a,mid,top.left,point.loglik_upper);push(mid,top.b,point.loglik_upper,top.right);
    }
    const auto top=queue.top();
    std::cout<<target<<','<<status<<','<<evaluations<<','<<lower<<','<<std::max({lower,small,large,top.upper})<<','<<top.b-top.a<<','<<queue.size()<<'\n';
  }
}
