// Exponential-family traits for closed-window truncation.
#pragma once
#include "../../core/truncation.hpp"
#include "../../special/special.hpp"
#include "ztnbinom_mle.hpp"
namespace cppunivariate {
struct NbinomTruncationTraits {
  double size=1;
  double lower() const {return 0.;}
  double upper() const {return size==0?0:INFINITY;}
  bool valid() const {return size>=0 && std::isfinite(size);}
  bool valid_parameter(double p) const {return p>0 && p<=1 && std::isfinite(p);}
  double natural(double p) const {return std::log1p(-p);}
  double coefficient(double e) const {return -std::expm1(e);}
  double mode(double e) const {
    if (size<=1) return 0;
    // At zero tilt the positive-size weights increase up to the cut.
    if (e==0) return INFINITY;
    return (size-1)*std::exp(e)/(-std::expm1(e));
  }
  double kernel(double k,double e) const {return k*e+special::lgamma(k+size)-special::lgamma(k+1);}
  double ratio(double k,double e) const {return std::exp(e)*(k+size)/(k+1);}
  double tail_bound(double r,double e,int direction) const {return direction>0?std::max(r,std::exp(e)):r;}
  double atom(double p) const {return size==0||p==1?0:NAN;}
  double start(double m) const {return std::log(m/(m+size));}
  double eta_lower() const {return -744.;}
  double eta_upper() const {return -1e-16;}
  double lower_member() const {return 1.;}
  double upper_member() const {return NAN;}
  bool positive_valid() const {return true;}
  double boundary_mean(double lo,double hi) const {return (size==1?lo+(hi-lo)/2:lo==0?hi*(size/(1+size)):NAN);}
  double boundary_log_ratio(double k) const {
    return k == 0 ? std::log(size) : std::log1p((size-1)/(k+1));
  }
  // S(j)=sum_{k=0}^j (size)_k/k!=(size+1)_j/j!. Evaluate its
  // difference relative to w(hi) when enumeration is too wide.
  double boundary_loglik(ispan x, double lo, double hi) const {
    if (lo > 0 && size <= 0x1p-56/(1+std::log(hi))) {
      // On positive cells, w(k)/size differs from 1/k by a relative
      // factor bounded by exp(size*(1+log(hi))). This avoids subtracting
      // two subnormal prefix logs before the common size cancels.
      double sum = 0, m = lo-1;
      for (; m < std::min(16.,hi); ++m) sum += 1/(m+1);
      if (hi > m) {
        const double inv_hi = 1/hi, inv_lo = 1/m;
        const double d1 = -((hi-m)/hi)/m, d2 = d1*(inv_hi+inv_lo);
        const double x2 = inv_hi*inv_hi, y2 = inv_lo*inv_lo;
        const double d4 = d2*(x2+y2), d6 = d2*(x2*x2+x2*y2+y2*y2);
        const double d8 = d4*(x2*x2+y2*y2);
        sum += std::log1p((hi-m)/m)+d1/2-d2/12+d4/120-d6/252+d8/240;
      }
      const double logz = std::log(sum);
      double ll = 0;
      for (int k : x) ll -= std::log(double(k))+logz;
      return ll;
    }
    const auto prefix = [&](double j) {
      if (size > j+1)
        return special::log_gamma_ratio(size,j+1,1)-special::lgamma(j+1);
      return special::log_gamma_ratio(j+1,size,0)-special::lgamma(size+1);
    };
    const auto relative = [&](double j) {
      if (size > hi+1)
        return special::log_gamma_ratio(size,j,hi) +
               special::log_gamma_ratio(j+1,hi-j,0);
      if (j == 0)
        return special::lgamma(size)-special::log_gamma_ratio(hi,size,1);
      return special::log_gamma_ratio(j,size,1)-special::log_gamma_ratio(hi,size,1);
    };
    const double prefix_ratio = lo == 0 ? -INFINITY : prefix(lo-1)-prefix(hi);
    const double logz = special::softplus(std::log(hi)-std::log(size)) +
        std::log(-std::expm1(prefix_ratio));
    double result = 0, correction = 0;
    for (int k : x) {
      const double value = (relative(k)-logz)-correction;
      const double next = result+value;
      correction = (next-result)-value;
      result = next;
    }
    return std::isfinite(result) && result <= 0 ? result : NAN;
  }
  double positive_member() const {return 1.;}
  bool positive_special() const {return size==0;}
  TruncationResult positive_fit(ispan x,double held,double tol,int maxit) const {
    const auto r=ztnbinom_mle(x,tol,maxit,size,held);
    return {r.prob,r.logLik,r.iterations,r.status==ZtnbinomStatus::Ok?TruncationStatus::Ok:
      r.status==ZtnbinomStatus::IterationFailure?TruncationStatus::NumericalFailure:TruncationStatus::InvalidInput};
  }
  double reference() const {return .5;}
};
}
