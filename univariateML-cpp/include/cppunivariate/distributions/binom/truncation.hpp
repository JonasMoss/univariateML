// Exponential-family traits for closed-window truncation.
#pragma once
#include "../../core/truncation.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
struct BinomTruncationTraits {
  double size=1;
  double lower() const {return 0.;}
  double upper() const {return size;}
  bool valid() const {return size>=0 && std::isfinite(size) && size==std::floor(size);}
  bool valid_parameter(double p) const {return p>=0 && p<=1 && std::isfinite(p);}
  double natural(double p) const {return std::log(p)-std::log1p(-p);}
  double coefficient(double e) const {return 1/(1+std::exp(-e));}
  double mode(double e) const {return (size+1)/(1+std::exp(-e));}
  double kernel(double k,double e) const {return k*e-special::lgamma(k+1)-special::lgamma(size-k+1);}
  double ratio(double k,double e) const {return std::exp(e)*(size-k)/(k+1);}
  double tail_bound(double r,double e,int direction) const {return direction>0?std::max(r,r):r;}
  double atom(double p) const {return p==0?0:p==1?size:NAN;}
  double start(double m) const {return std::log(m/(size-m));}
  double eta_lower() const {return -744.;}
  double eta_upper() const {return 709.;}
  double lower_member() const {return 0.;}
  double upper_member() const {return 1.;}
  bool positive_valid() const {return size>=1;}
  double boundary_mean(double ,double ) const {return NAN;}
  double positive_member() const {return 0.;}
  bool positive_special() const {return false;}
  TruncationResult positive_fit(ispan,double,double,int) const {return {NAN,NAN,0,TruncationStatus::NotImplemented};}
  double reference() const {return .5;}
};
}
