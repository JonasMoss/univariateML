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
  double mode(double e) const {return size>1? (size-1)*std::exp(e)/(-std::expm1(e)):0.;}
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
  double boundary_mean(double lo,double hi) const {return (size==1?lo+(hi-lo)/2:lo==0?hi/(1+1/size):NAN);}
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
