#pragma once
#include "../../core/work_budget.hpp"
#include <array>
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
// Shared by the three J6 exponential-family fitters. Likelihoods and scores
// are per observation. This checks global likelihood error, not just stationarity.
namespace concave_accept_detail {
struct Evaluation { double value=NAN, g1=NAN, g2=0; bool ok=false; };
struct Bound {
  bool ok=false; double upper=INFINITY; int evaluations=0;
  double lo1=NAN,hi1=NAN,lo2=NAN,hi2=NAN;
};
// Concavity gives f(z)<=f(y)+g(y).(z-y). Positive weights cancelling
// the free-coordinate gradients therefore give a global constant upper
// bound. In two dimensions use three surrounding scores; in one use two.
// Also, gradient monotonicity at the maximizer z* implies
// g(y).(z*-y)>=0. When the scores positively span the plane, their three
// half-spaces have a bounded triangular intersection containing z*. Its
// coordinate extrema give coefficient enclosures, independent of flatness.
// These assertions require concavity on a convex domain, existence of a
// maximizer, and exact evaluations. Each caller establishes its existence
// or face classification before calling this check.
// Numerical contract: per-observation upper padding
// 1e-9*(1+abs(candidate)+abs(unpadded_upper)), acceptance gap
// <=1e-8*(1+abs(candidate)); coordinate extrema are padded by
// 128*epsilon*(1+max(abs(low),abs(high))). Coefficient tolerances and maps
// are supplied by each family. These are practical floating-point margins,
// conditional on computed moments/scores/comparisons, not interval arithmetic.
// The refinement loop is a solver loop (maxit passes, 2 or 3 evaluations
// per pass, hence also below the 128*maxit certificate-evaluation allowance).
// A valid passing proposal is retained; this helper never refits it.
// For a constrained second coordinate z2>=0, boundary=true permits a
// nonpositive combined second score at y2=0 (the KKT dual inequality).
// The caller must establish monotonicity of that boundary score along the
// first coordinate. Both bracket endpoint scores must be strictly negative,
// certifying the KKT sign at the enclosed scalar root as well.
// The inverse information maps a small score displacement to coordinates.
// Failed evaluations/comparisons are terminal; the proposer is never changed.
template<class Evaluate,class Accurate>
inline Bound certify(double p1,double p2,double f,double a,double b,double c,
                     int dim,bool boundary,int maxit,const Evaluate& evaluate,const Accurate& accurate) {
  Bound out;
  if(!std::isfinite(f) || !(a>0) || (dim==2 && !(a*c-b*b>0)))return out;
  double h=1e-4;
  for(std::int64_t it=0;it<solver_budget(maxit);++it,h*=0.25) {
    std::array<double,3> d1{},d2{};
    if(dim==1) {d1={-h*a,h*a,0};}
    else {
      d1={-h*(a+b),h*(a-b),h*b};
      d2={-h*(b+c),h*(b-c),h*c};
    }
    std::array<Evaluation,3> v;
    const int count=dim==1?2:3;
    bool valid=true;
    for(int j=0;j<count;++j) {
      v[j]=evaluate(p1+d1[j],p2+d2[j]);++out.evaluations;
      valid=valid && v[j].ok && std::isfinite(v[j].value) &&
        std::isfinite(v[j].g1) && std::isfinite(v[j].g2);
    }
    if(!valid)continue;
    std::array<double,3> w{};
    if(dim==1) {
      if(!(v[0].g1>0 && v[1].g1<0))return out;
      w[0]=-v[1].g1/(v[0].g1-v[1].g1);w[1]=1-w[0];
    } else {
      const double x=v[0].g1-v[2].g1,y=v[1].g1-v[2].g1;
      const double u=v[0].g2-v[2].g2,z=v[1].g2-v[2].g2;
      const double det=x*z-y*u;
      if(!std::isfinite(det) || det==0)return out;
      w[0]=(-v[2].g1*z+y*v[2].g2)/det;
      w[1]=(-x*v[2].g2+v[2].g1*u)/det;w[2]=1-w[0]-w[1];
    }
    double upper=0,g2=0;
    for(int j=0;j<count;++j) {
      if(!(w[j]>1e-6 && w[j]<1))return out;
      upper+=w[j]*(v[j].value-v[j].g1*d1[j]-v[j].g2*d2[j]);
      g2+=w[j]*v[j].g2;
    }
    if(boundary && (!(v[0].g2 < -1e-12*(1+std::abs(v[0].g2))) ||
                    !(v[1].g2 < -1e-12*(1+std::abs(v[1].g2)))))continue;
    if(boundary && !(g2 < -1e-12*(1+std::abs(g2))))continue;
    // Practical floating-point allowance; computed gradients and moments
    // are conditional comparisons, not outward-rounded interval arithmetic.
    upper+=1e-9*(1+std::abs(f)+std::abs(upper));
    if(!std::isfinite(upper) || upper<f-1e-9*(1+std::abs(f)))return out;
    out.upper=upper;
    out.lo1=out.lo2=INFINITY;out.hi1=out.hi2=-INFINITY;
    if(dim==1) {
      out.lo1=p1+d1[0];out.hi1=p1+d1[1];out.lo2=out.hi2=p2;
    } else {
      // Monotonicity of the gradient at a stationary maximum gives
      // g(y).(z*-y)>=0. Intersect the three half-planes. Positive score
      // weights above ensure that the intersection is bounded.
      for(int j=0;j<3;++j) {
        const int k=(j+1)%3;
        const double r=v[j].g1*d1[j]+v[j].g2*d2[j];
        const double t=v[k].g1*d1[k]+v[k].g2*d2[k];
        const double det=v[j].g1*v[k].g2-v[k].g1*v[j].g2;
        if(det==0 || !std::isfinite(det))return out;
        const double u=(r*v[k].g2-t*v[j].g2)/det;
        const double z=(v[j].g1*t-v[k].g1*r)/det;
        if(!std::isfinite(u) || !std::isfinite(z))return out;
        out.lo1=std::min(out.lo1,p1+u);out.hi1=std::max(out.hi1,p1+u);
        out.lo2=std::min(out.lo2,p2+z);out.hi2=std::max(out.hi2,p2+z);
      }
    }
    const double pad1=128*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(out.lo1),std::abs(out.hi1)));
    const double pad2=dim==2?128*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(out.lo2),std::abs(out.hi2))):0;
    out.lo1-=pad1;out.hi1+=pad1;out.lo2-=pad2;out.hi2+=pad2;
    if(upper-f<=1e-8*(1+std::abs(f)) && accurate(out)) {out.ok=true;return out;}
  }
  return out;
}
}
}
