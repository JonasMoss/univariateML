#pragma once
#include "../../special/tails.hpp"
#include "../nbinom/dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline double dzinb(double x, double size, double prob, double pi, bool log) {
  if (!(size >= 0) || !std::isfinite(size) || !(prob > 0 && prob <= 1) || !(pi >= 0 && pi <= 1))
    return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (x < 0 || !std::isfinite(x) || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  if (size == 0 || pi == 1 || prob == 1) return x == 0 ? (log ? 0 : 1) :
      (log ? -std::numeric_limits<double>::infinity() : 0);
  double l = std::log1p(-pi) + cppunivariate::distributions::dnbinom(x, size, prob, true);
  if (x == 0 && pi > 0) {
    const double a = std::log(pi), m = std::max(a, l);
    l = m + std::log1p(std::exp(std::min(a, l) - m));
  }
  return log ? l : std::exp(l);
}
inline special::LogTails zinb_tails(double q,double size,double prob,double pi) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1) || !(pi>=0 && pi<=1) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  if(q<0) return special::tails_from_log(special::kNegInf,true);
  if(size==0 || pi==1 || prob==1) return special::tails_from_log(special::kNegInf,false);
  const auto t=nbinom_tails(q,size,prob);
  const double lo=special::log_sum_exp(std::log(pi),std::log1p(-pi)+t.lower);
  const double up=std::log1p(-pi)+t.upper;
  return lo<up?special::tails_from_log(lo,true):special::tails_from_log(up,false);
}
inline double pzinb(double q,double size,double prob,double pi,bool lower_tail,bool log_p) {
  return special::tail_probability(zinb_tails(q,size,prob,pi),lower_tail,log_p);
}
inline double qzinb(double p,double size,double prob,double pi,bool lower_tail,bool log_p) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1) || !(pi>=0 && pi<=1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,
      size==0||pi==1||prob==1?0.0:special::kPosInf,[=](double x){return zinb_tails(x,size,prob,pi);});
}
template <typename Rng>
inline double rzinb(Rng &rng, double size, double prob, double pi) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1) || !(pi>=0 && pi<=1)) return special::kNaN;
  if (size == 0 || pi == 1 || prob == 1 || rng.runif(0, 1) < pi) return 0;
  return cppunivariate::distributions::rnbinom(rng, size, prob);
}
} // namespace distributions
} // namespace cppunivariate
