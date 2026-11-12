// Exponential-family traits for closed-window truncation.
#pragma once
#include "../../core/truncation.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
struct PoisTruncationTraits {
  double size=0;
  double lower() const {return 0.;}
  double upper() const {return INFINITY;}
  bool valid() const {return true;}
  bool valid_parameter(double p) const {return p>=0 && std::isfinite(p);}
  double natural(double p) const {return std::log(p);}
  double coefficient(double e) const {return std::exp(e);}
  double mode(double e) const {return std::exp(e);}
  double kernel(double k,double e) const {return k*e-special::lgamma(k+1);}
  double ratio(double k,double e) const {return std::exp(e)/(k+1);}
  double tail_bound(double r,double e,int direction) const {return direction>0?std::max(r,r):r;}
  double atom(double p) const {return p==0?0:NAN;}
  double start(double m) const {return std::log(m);}
  double eta_lower() const {return -744.;}
  double eta_upper() const {return 709.;}
  double lower_member() const {return 0.;}
  double upper_member() const {return NAN;}
  bool positive_valid() const {return true;}
  double boundary_mean(double ,double ) const {return NAN;}
  double positive_member() const {return 0.;}
  bool positive_special() const {return false;}
  TruncationResult positive_fit(ispan,double,double,int) const {return {NAN,NAN,0,TruncationStatus::NotImplemented};}
  double reference() const {return 1.;}
};
}
