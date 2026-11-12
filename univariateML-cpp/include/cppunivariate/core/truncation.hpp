// Conditional exponential-family likelihood. Fixed windows preserve concavity:
// d log Z / d eta = E[X], d2 log Z / d eta2 = Var[X] > 0.
#pragma once
#include "span.hpp"
#include "work_budget.hpp"
#include "precision.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
enum class TruncationStatus { Ok, InvalidInput, NotImplemented, LowerLimit, UpperLimit, NumericalFailure };
struct TruncationResult {
  double parameter=NAN, logLik=NAN;
  int iterations=0;
  TruncationStatus status=TruncationStatus::InvalidInput;
};
// Traits supply support, natural domain, log kernel, adjacent mass ratio and
// natural/coefficient maps. No distribution-specific switch in the solver.
template<class Traits> struct truncated {
  struct Mass { double logz, mode; CPPUNIV_EXTENDED_REAL offset; bool ok; };
  static Mass mass(const Traits& t,double eta,double lo,double hi,int maxit) {
    const double mode=std::floor(std::max(lo,std::min(hi,t.mode(eta))));
    if(!std::isfinite(mode))return {NAN,NAN,0,false};
    CPPUNIV_EXTENDED_REAL total=1,offset=0;
    const auto cap=enumeration_budget(maxit);
    for(int direction : {-1,1}) {
      double k=mode; CPPUNIV_EXTENDED_REAL w=1;
      bool done=false;
      for(std::size_t j=0;j<cap;++j) {
        if((direction<0 && k<=lo)||(direction>0 && k>=hi)){done=true;break;}
        const double ratio=direction>0?t.ratio(k,eta):1/t.ratio(k-1,eta);
        w*=ratio;k+=direction;total+=w;offset+=(k-mode)*w;
        // Log-concavity bounds the remaining geometric tail. Include its
        // first moment, since a mass-only cutoff does not bound the score.
        const double bound=t.tail_bound(ratio,eta,direction);
        if(bound<1 && w*bound/(1-bound)*(1+std::abs(k-mode)+1/(1-bound))<1e-18L*std::max(static_cast<CPPUNIV_EXTENDED_REAL>(1e-300L),std::abs(offset))){done=true;break;}
        if(!std::isfinite(static_cast<double>(total)))return {NAN,NAN,0,false};
      }
      if(!done)return {NAN,NAN,0,false};
    }
    // Relative normalizer: retaining the large absolute log kernel would
    // lose the conditional likelihood through cancellation (notably huge NB size).
    const double z=std::log(total);
    return {z,mode,offset/total,std::isfinite(z)};
  }
  static TruncationResult fit(ispan x,double a,double b,Traits t={},double held=NAN,double tol=1e-10,int maxit=200) {
    TruncationResult r;
    maxit=static_cast<int>(solver_budget(maxit));
    if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1)return r;
    double xmin=x[0],xmax=x[0];
    for(int v:x){xmin=std::min(xmin,double(v));xmax=std::max(xmax,double(v));}
    if(std::isnan(a))a=xmin;if(std::isnan(b))b=xmax;
    // Retain the attained positive-truncation extensions of the legacy
    // families, including NB size zero (the logarithmic conditional law).
    if(a==1 && b==INFINITY && t.valid() && t.positive_valid()) {
      if(t.positive_special())return t.positive_fit(x,held,tol,maxit);
      if(xmin==1 && xmax==1 && (std::isnan(held)||held==t.positive_member()))
        return {t.positive_member(),0,0,TruncationStatus::Ok};
    }
    const double lo=std::max(t.lower(),std::ceil(a)),hi=std::min(t.upper(),std::floor(b));
    if(!(lo<=hi) || !std::isfinite(lo) || xmin<lo || xmax>hi || !t.valid())return r;
    CPPUNIV_EXTENDED_REAL centred=0;
    for(int v:x)centred+=static_cast<double>(v)-xmin;
    const auto observed_offset=centred/x.size();
    const double target=xmin+static_cast<double>(observed_offset);
    const auto below=[&](const Mass& m){return m.offset < (xmin-m.mode)+observed_offset;};
    const auto above=[&](const Mass& m){return m.offset > (xmin-m.mode)+observed_offset;};
    if(lo==hi){if(std::isnan(held))held=t.reference();}
    double eta=std::isnan(held)?0:t.natural(held);
    if(!std::isnan(held)) {
      if(!t.valid_parameter(held))return r;
      // Attained parent atoms, only when the conditioning event contains it.
      const double atom=t.atom(held);
      if(std::isfinite(atom)) {
        if(xmin!=atom || xmax!=atom)return r;
        return {held,0,0,TruncationStatus::Ok};
      }
    } else {
      if(xmin==lo && xmax==lo) {
        const double p=t.lower_member();
        if(t.atom(p)==target)return {p,0,0,TruncationStatus::Ok};
        return {NAN,NAN,0,TruncationStatus::LowerLimit};
      }
      if(xmin==hi && xmax==hi) {
        const double p=t.upper_member();
        if(t.atom(p)==target)return {p,0,0,TruncationStatus::Ok};
        return {NAN,NAN,0,TruncationStatus::UpperLimit};
      }
      if(t.eta_upper()<0 && std::isfinite(hi)) {
        // On a finite window eta=0 is a proper conditional boundary law,
        // although prob=0 is excluded from the parent domain. Its mean
        // separates an interior root from a proved unattained supremum.
        const double known=t.boundary_mean(lo,hi);
        if(std::isfinite(known)) {
          if(observed_offset>=known-xmin)return {NAN,NAN,0,TruncationStatus::UpperLimit};
        } else {
          const auto boundary=mass(t,0,lo,hi,maxit);
          if(boundary.ok && !above(boundary))return {NAN,NAN,0,TruncationStatus::UpperLimit};
          // An unfinished optional face calculation does not preclude an
          // interior root: a successful sign bracket is its own certificate.
        }
      }
      if(tol<32*std::numeric_limits<double>::epsilon())
        return {NAN,NAN,0,TruncationStatus::NumericalFailure};
      const double centre=t.start(target);
      double width=1,left=std::max(t.eta_lower(),centre-width),right=centre+width>=t.eta_upper() && t.eta_upper()<0 ? centre/2 : std::min(t.eta_upper(),centre+width);
      bool bracket=false;
      for(int i=0;i<maxit;++i) {
        const auto l=mass(t,left,lo,hi,maxit),h=mass(t,right,lo,hi,maxit);
        if(!l.ok||!h.ok)return {NAN,NAN,0,TruncationStatus::NumericalFailure};
        if(below(l) && above(h)){bracket=true;break;}
        if(!below(l) && left==t.eta_lower())return {NAN,NAN,0,TruncationStatus::LowerLimit};
        if(!above(h) && right==t.eta_upper())return {NAN,NAN,0,TruncationStatus::UpperLimit};
        width*=2;
        if(!below(l))left=std::max(t.eta_lower(),centre-width);
        if(!above(h))right=centre+width>=t.eta_upper() && t.eta_upper()<0 ? centre/2 : std::min(t.eta_upper(),centre+width);
      }
      if(!bracket)return {NAN,NAN,0,TruncationStatus::NumericalFailure};
      bool closed=false;
      for(int i=0;i<maxit;++i) {
        eta=left+(right-left)/2;auto z=mass(t,eta,lo,hi,maxit);r.iterations=i+1;
        if(!z.ok)return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
        if(below(z))left=eta;else right=eta;
        if(right-left<.05*tol*(t.eta_upper()<0?std::min(1.,std::abs(eta)):1.)){closed=true;break;}
      }
      if(!closed)return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
      eta=left+(right-left)/2;
    }
    r.parameter=std::isnan(held)?t.coefficient(eta):held;
    const double atom=t.atom(r.parameter);
    if(std::isfinite(atom) && (xmin!=atom || xmax!=atom))
      return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
    // Evaluate the law of the coefficient actually returned after rounding.
    eta=t.natural(r.parameter);
    auto z=mass(t,eta,lo,hi,maxit);
    if(!z.ok)return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
    r.logLik=0;
    std::int64_t work=0;const auto cap=enumeration_budget(maxit);
    for(int v:x) {
      double relative=0;
      if(v>z.mode)for(double k=z.mode;k<v;++k) {
        if(++work>cap)return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
        relative+=std::log(t.ratio(k,eta));
      }
      else for(double k=v;k<z.mode;++k) {
        if(++work>cap)return {NAN,NAN,r.iterations,TruncationStatus::NumericalFailure};
        relative-=std::log(t.ratio(k,eta));
      }
      r.logLik+=relative-z.logz;
    }
    r.status=std::isfinite(r.logLik)?TruncationStatus::Ok:TruncationStatus::NumericalFailure;return r;
  }
};
}
