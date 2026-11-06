// The log-rho score is strictly decreasing; unique counts cache repeated work.
#pragma once
#include "../../core/discrete_stop.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate {
enum class YulesimonStatus { Ok, PointMass, InvalidInput, IterationFailure };
struct YulesimonResult { double rho, logLik; int iterations; YulesimonStatus status;   StopDetail stop_detail{};
};
namespace yulesimon_detail {
inline double score_term(int k,double rho) {
  if (k<=32) {
    double sum=0;
    for (int j=1;j<=k;++j) sum+=rho/(rho+j);
    return sum;
  }
  return rho*(special::digamma(rho+k+1)-special::digamma(rho+1));
}
}
inline YulesimonResult yulesimon_mle(ispan x,double tol=1e-10,int maxit=200, double held=std::numeric_limits<double>::quiet_NaN()) {
  auto stopped = [&]() -> YulesimonResult {
  const double nan=special::kNaN;
  if (!x.size() || !(tol>0) || !std::isfinite(tol) || maxit<1)
    return {nan,nan,0,YulesimonStatus::InvalidInput};
  std::vector<int> values;
  for (int k:x) { if (k<1) return {nan,nan,0,YulesimonStatus::InvalidInput}; values.push_back(k); }
  std::sort(values.begin(),values.end());
  if (!std::isnan(held)) {
    if (!(held>0) || !std::isfinite(held)) return {nan,nan,0,YulesimonStatus::InvalidInput};
    double ll=0;
    for (int k:x) ll+=distributions::dyulesimon(k,held,true);
    return {held,ll,0,YulesimonStatus::Ok};
  }
  if (values.back()==1) return {special::kPosInf,0,0,YulesimonStatus::PointMass};
  std::vector<int> keys,counts;
  for (int k:values) {
    if (keys.empty() || keys.back()!=k) { keys.push_back(k); counts.push_back(1); }
    else ++counts.back();
  }
  const auto score=[&](double rho) {
    double out=1;
    for (std::size_t j=0;j<keys.size();++j)
      out-=static_cast<double>(counts[j])/x.size()*yulesimon_detail::score_term(keys[j],rho);
    return out;
  };
  double lo=-1,hi=1;
  while (score(std::exp(lo))<0) lo*=2;
  while (score(std::exp(hi))>0) hi*=2;
  double rho=1;
  for (int it=1;it<=maxit;++it) {
    const double mid=lo+(hi-lo)/2;
    rho=std::exp(mid);
    const double residual=score(rho);
    if (residual>0) lo=mid; else hi=mid;
    if (hi-lo<=tol) {
      double ll=0;
      for (std::size_t j=0;j<keys.size();++j)
        ll+=counts[j]*distributions::dyulesimon(keys[j],rho,true);
      return {rho,ll,it,YulesimonStatus::Ok};
    }
  }
  return {rho,nan,maxit,YulesimonStatus::IterationFailure};

  }();
  if(stopped.status==YulesimonStatus::PointMass) stopped.stop_detail=discrete_point(x[0]);
  return stopped;
}
}
