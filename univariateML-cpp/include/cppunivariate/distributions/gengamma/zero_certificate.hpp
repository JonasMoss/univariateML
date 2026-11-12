// A sufficient certificate for the attained Q=0 member.
#pragma once
#include "face_certificate.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

// In the safe normal range, fma supplies the exact rounding residual of a
// product of two binary64 inputs. Identical product pairs prove that the
// original observations have exact multiplicative symmetry, rather than
// merely having a small computed third moment after taking logarithms.
inline bool multiplicatively_symmetric(span x) {
  if (x.size()<2) return false;
  std::vector<double> values(x.begin(),x.end());
  for(double v:values)if(!(v>0) || !std::isfinite(v))return false;
  std::sort(values.begin(),values.end());
  double product=0,residual=0;
  for(std::size_t i=0;i<(values.size()+1)/2;++i) {
    const double a=values[i],b=values[values.size()-1-i];
    const double p=a*b;
    // The exponent guard also keeps any nonzero product residual well above
    // the binary64 underflow range, so equality of both parts is exact.
    if (!(p>std::ldexp(1.,-900) && p<std::ldexp(1.,900)))return false;
    const double e=std::fma(a,b,-p);
    if (i==0) {product=p;residual=e;}
    else if (p!=product || e!=residual) return false;
  }
  return true;
}

struct ZeroCertificate {bool certified=false;int evaluations=0;};

inline ZeroCertificate certify_zero(span x, span y, int maxit, double data_error=0) {
  ZeroCertificate out;
  if (x.size()!=y.size() || maxit<1 || !(data_error>=0) || !std::isfinite(data_error) ||
      !multiplicatively_symmetric(x)) return out;
  std::vector<double> reflected;reflected.reserve(y.size());
  for(std::size_t i=0;i<y.size();++i){
    if(!std::isfinite(y[i]) || std::log(x[i])!=y[i])return out;
    reflected.push_back(-y[i]);
  }
  trgamma_detail::LogData positive,negative;
  if(!trgamma_detail::standardize(y,positive) ||
     !trgamma_detail::standardize({reflected.data(),reflected.size()},negative))return out;
  const double scale=std::min({positive.sd,positive.sd*positive.umax,
                               negative.sd,negative.sd*negative.umax});
  if (!(scale>0) || !std::isfinite(scale)) return out;
  const double plus=trgamma_detail::power_delta(positive),minus=trgamma_detail::power_delta(negative);
  const double pad=(256*std::numeric_limits<double>::epsilon()*(1+y.size())+16*data_error/scale)*
      (1+std::abs(plus)+std::abs(minus));
  if (!(std::isfinite(pad) && plus < -pad && minus < -pad)) return out;
  // Exact multiplicative symmetry gives zero third central log moment even
  // when separately rounded logarithms leave a small computed residual.
  positive.kappa3=0;negative.kappa3=0;
  const std::int64_t half=certificate_budget(maxit)/2;
  int e1=0,e2=0;
  const bool a=side_below_face(positive,0,maxit,half,e1,data_error,true);
  const bool b=a && side_below_face(negative,0,maxit,half,e2,data_error,true);
  out.evaluations=e1+e2;out.certified=a && b;
  return out;
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
