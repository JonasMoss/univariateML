// Exact finite mode enumeration with concave endpoint solves and interval bounds.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/span.hpp"
#include "../../core/stop_detail.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <utility>
#include <vector>

namespace cppunivariate {
enum class TriangStatus { Ok, InvalidInput, Incompatible, Unbounded, IterationFailure, NumericalFailure };
struct TriangModeCandidate { double a, b, c, logLik; };
struct TriangResult {
  double a, b, c, logLik;
  int iterations;
  TriangStatus status;
  bool not_unique;
  // Proved tie: the sample is symmetric about its midrange and the returned
  // (a, b, c) is not its own reflection (s - b, s - a, s - c), s = min + max;
  // the reflection maps maximizers to maximizers with equal likelihood.
  bool reflection_tie=false;
  StopDetail stop_detail{};
  // Other modes whose likelihood comparison remains unresolved.
  std::vector<TriangModeCandidate> unresolved_modes{};
};
namespace triang_detail {
struct Profile { double a, b, value, upper; int iterations; bool converged; };
// Lexicographic order of the rounded sum and its exact residual is the
// order of the exact sums, provided neither rounded sum overflows.
inline std::pair<double,double> sum_parts(double x, double y) {
  const double hi=x+y, z=hi-x;
  return {hi,(x-(hi-z))+(y-z)};
}

// Compare fixed-endpoint likelihoods by enclosing their ratio. The common
// density height cancels. A zero return includes exact equality, overlap
// of the enclosures, and ratios outside the usable floating-point range.
inline int compare_modes(const std::vector<double>& x, double a, double b,
                         double first, double second, bool balanced_pair) {
  if(balanced_pair) {
    // At the two observed modes the likelihoods have the order of
    // (b-y)/(b-x) and (x-a)/(y-a). Cross multiplication reduces the
    // first minus the second to (y-x)*(a+b-x-y).
    const auto endpoints=sum_parts(a,b);
    const auto sample=sum_parts(first,second);
    if(std::isfinite(endpoints.first)&&std::isfinite(endpoints.second)&&
       std::isfinite(sample.first)&&std::isfinite(sample.second)) {
      if(endpoints==sample) return 0;
      return (endpoints>sample)==(first<second) ? 1 : -1;
    }
  }
  struct Bounds { double lo, hi; };
  const auto down=[](double v) { return std::nextafter(v,0.0); };
  const auto up=[](double v) { return std::nextafter(v,INFINITY); };
  const auto gap=[&](double hi,double lo) {
    const double v=hi-lo;
    return Bounds{down(v),up(v)};
  };
  const auto factor=[&](double z,double mode) {
    if(z==mode) return Bounds{1,1};
    const Bounds numerator=z<mode ? gap(z,a) : gap(b,z);
    const Bounds denominator=z<mode ? gap(mode,a) : gap(b,mode);
    return Bounds{down(numerator.lo/denominator.hi),up(numerator.hi/denominator.lo)};
  };
  Bounds ratio{1,1};
  for(double z:x) {
    const auto p=factor(z,first), q=factor(z,second);
    if(!(q.lo>0 && std::isfinite(p.hi) && std::isfinite(q.hi))) return 0;
    ratio={down(ratio.lo*down(p.lo/q.hi)),up(ratio.hi*up(p.hi/q.lo))};
    if(!(ratio.lo>0 && std::isfinite(ratio.hi))) return 0;
  }
  if(ratio.lo>1) return 1;
  if(ratio.hi<1) return -1;
  return 0;
}

// For modes in [l,u], replace all middle factors by one, and use l on
// the left, u on the right. This dominates every candidate pointwise.
// In p=L/(l-a), q=R/(b-u), the log envelope is strictly concave:
// n log(2/(u-l+L/p+R/q)) + sum log(1-p*d/L) + sum log(1-q*e/R).
// Its tangent over [0,1]^2 supplies a certified optimization upper bound,
// including when Newton has not converged. Fixed coordinates are omitted.
inline Profile profile(const std::vector<double>& x, double l, double u,
                       double a, double b, double tol, int maxit, double tilt=0) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bool fa = !std::isnan(a), fb = !std::isnan(b);
  if (tilt == 0 && l == u && x.front() == x.back() && x.front() != l) {
    const double z = x.front(), n = static_cast<double>(x.size());
    if (z > l) {
      if (!fa) a = l;
      if (!fb) b = z + std::sqrt((z-a)*(z-l));
      const double ll = n*(std::log(2.0) + std::log(b-z) - std::log(b-a) - std::log(b-l));
      return {a,b,ll,ll,0,true};
    }
    if (!fb) b = l;
    if (!fa) a = z - std::sqrt((b-z)*(l-z));
    const double ll = n*(std::log(2.0) + std::log(z-a) - std::log(b-a) - std::log(l-a));
    return {a,b,ll,ll,0,true};
  }

  const double L = fa ? l-a : std::max(0.0,l-x.front());
  const double R = fb ? b-u : std::max(0.0,x.back()-u);
  if (L<0 || R<0) return {0,0,-INFINITY,-INFINITY,0,true};
  const bool pa = !fa && L>0, pb = !fb && R>0;
  double p = pa ? 0.5 : 1, q = pb ? 0.5 : 1;
  const double n = static_cast<double>(x.size());
  const double nl=std::lower_bound(x.begin(),x.end(),l)-x.begin();
  const double nr=n-nl;
  auto evaluate = [&](double pp, double qq, std::vector<double>& v) {
    const double w=u-l+L/pp+R/qq;
    v.assign(6,0);
    if (!(w>0)) { v[0]=-INFINITY; return; }
    v[0]=n*std::log(2/w);
    const double A=L/(pp*pp), B=R/(qq*qq);
    v[1]=n*A/w; v[2]=n*B/w;
    v[3]=n*(A*A/(w*w)-2*A/(pp*w));
    v[4]=n*A*B/(w*w);
    v[5]=n*(B*B/(w*w)-2*B/(qq*w));
    if(tilt!=0) {
      if(L>0) {v[0]-=tilt*nl*pp/L;v[1]-=tilt*nl/L;}
      if(R>0) {v[0]+=tilt*nr*qq/R;v[2]+=tilt*nr/R;}
    }
    for (double z:x) {
      if (z<l) {
        const double d=(l-z)/L, t=fa?(z-a)/L:1-pp*d;
        if (!(t>0)) {v[0]=-INFINITY; return;}
        v[0]+=std::log(t); v[1]-=d/t; v[3]-=d*d/(t*t);
      } else if (z>u) {
        const double d=(z-u)/R, t=fb?(b-z)/R:1-qq*d;
        if (!(t>0)) {v[0]=-INFINITY; return;}
        v[0]+=std::log(t); v[2]-=d/t; v[5]-=d*d/(t*t);
      }
    }
  };
  std::vector<double> v, trial;
  int it=0; bool done=false;
  double previous=INFINITY;
  evaluate(p,q,v);
  for (;it<maxit;++it) {
    if (!std::isfinite(v[0])) break;
    double dp=0,dq=0;
    if (pa && pb) {
      const double det=v[3]*v[5]-v[4]*v[4];
      if (!(v[3]<0 && v[5]<0 && det>0)) break;
      dp=(-v[5]*v[1]+v[4]*v[2])/det;
      dq=(v[4]*v[1]-v[3]*v[2])/det;
    } else if (pa) {
      if (!(v[3]<0)) break;
      dp=-v[1]/v[3];
    } else if (pb) {
      if (!(v[5]<0)) break;
      dq=-v[2]/v[5];
    }
    else {done=true; break;}
    // Transform the information correction through the reciprocal
    // distances to measure error in each endpoint's natural scale.
    const double delta_a=pa ? std::abs(L*dp/(p*p)) : 0;
    const double delta_b=pb ? std::abs(R*dq/(q*q)) : 0;
    const double width=u-l+L/p+R/q;
    const double error=std::max(delta_a,delta_b)/width;
    if (error<=tol/4 && (previous<=tol/4 ||
        (error<=previous/4 && std::isfinite(previous) &&
         error<=std::max(16*std::numeric_limits<double>::epsilon(),16*previous*previous)))) {
      done=true;
      break;
    }
    double step=1;
    if (dp<0) step=std::min(step,-0.99*p/dp);
    if (dp>0) step=std::min(step,0.99*(1-p)/dp);
    if (dq<0) step=std::min(step,-0.99*q/dq);
    if (dq>0) step=std::min(step,0.99*(1-q)/dq);
    bool accepted=false;
    for (int j=0;j<maxit;++j) {
      evaluate(p+step*dp,q+step*dq,trial);
      if (trial[0]>=v[0]+1e-4*step*(v[1]*dp+v[2]*dq)-1e-13*n) {
        accepted=true;break;
      }
      step*=0.5;
    }
    if(!accepted) break;
    p+=step*dp; q+=step*dq;
    v.swap(trial);
    previous=step==1 ? error : INFINITY;
  }
  double upper=v[0];
  if(pa) upper+=v[1]*(v[1]>0 ? 1-p : -p);
  if(pb) upper+=v[2]*(v[2]>0 ? 1-q : -q);
  // Roundoff allowance only enlarges the pruning bound.
  upper+=1e-11*n*(1+std::abs(v[0])/n);
  return {fa?a:l-L/p,fb?b:u+R/q,v[0],upper,it,done};
}
// For an observation endpoint mode only one endpoint remains to optimize.
// In reciprocal distance its objective is strictly concave. Signed scores at
// the two public neighbours bracket its root; log1p differences then identify
// the best public point without subtracting nearly equal log likelihoods.
inline bool rounded_endpoint(span x, double a, double b, double c,
                             bool fa, bool fb) {
  const double minimum=*std::min_element(x.begin(),x.end());
  const double maximum=*std::max_element(x.begin(),x.end());
  const bool reflected=c==maximum;
  if(reflected ? (fa || (!fb && b!=c)) :
                 (c!=minimum || fb || (!fa && a!=c))) return false;
  const double endpoint=reflected?a:b;
  const double fixed=reflected?b:a;
  const double distance=reflected?c-endpoint:endpoint-c;
  const double offset=reflected?fixed-c:c-fixed;
  auto score=[&](double e) -> double {
    const double w=reflected?c-e:e-c;
    // This is w times the derivative in w. Its sign is opposite to
    // the derivative in reciprocal distance.
    double value=-static_cast<double>(x.size())*w/(w+offset);
    for(double z:x) if(z!=c) {
      const double d=reflected?c-z:z-c;
      if(!(w>d)) return INFINITY;
      value+=d/(w-d);
    }
    return value;
  };
  const double inward=std::nextafter(endpoint,c);
  const double outward=std::nextafter(endpoint,reflected?-INFINITY:INFINITY);
  if(!std::isfinite(outward) || !(distance>0) || !(offset>=0)) return false;
  if(!(score(inward)>=0 && score(outward)<=0)) return false;
  auto difference=[&](double e) -> double {
    const double delta=reflected?endpoint-e:e-endpoint;
    double value=-static_cast<double>(x.size())*std::log1p(delta/(distance+offset));
    for(double z:x) if(z!=c) {
      const double gap=reflected?z-endpoint:endpoint-z;
      if(!(gap+delta>0)) return -INFINITY;
      value+=std::log1p(delta/gap)-std::log1p(delta/distance);
    }
    return value;
  };
  // A sign comparison margin in likelihood-difference units, not in the
  // much larger untranslated log likelihood.
  const double margin=64*std::numeric_limits<double>::epsilon()*x.size()*
      std::abs(outward-endpoint)/distance;
  return difference(inward)<=margin && difference(outward)<=margin;
}
} // namespace triang_detail

// NaN means free. prune=false is the exhaustive reference path.
inline TriangResult triang_mle(span input, double tol=1e-10, int maxit=100,
    double a=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN(),
    double c=std::numeric_limits<double>::quiet_NaN(), bool prune=true) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TriangResult out{nan,nan,nan,-INFINITY,0,TriangStatus::InvalidInput,false};
  if(input.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1) return out;
  const double requested_tol=tol;
  constexpr double method_floor=4*std::numeric_limits<double>::epsilon();
  tol=std::max(tol,method_floor);
  for(double z:input) if(!std::isfinite(z)) return out;
  for(double z:{a,b,c}) if(!std::isnan(z) && !std::isfinite(z)) return out;
  const double original_a=a, original_b=b, original_c=c;
  const bool fa=!std::isnan(a), fb=!std::isnan(b), fc=!std::isnan(c);
  if((fa&&fb&&a>=b)||(fa&&fc&&a>c)||(fb&&fc&&c>b)) return out;
  std::vector<double> x(input.begin(),input.end());
  std::sort(x.begin(),x.end());
  const auto observations=x;
  const double origin=x.front();
  if((fa&&a>origin)||(fb&&b<x.back())) {out.status=TriangStatus::Incompatible;return out;}
  double scale=x.back()-origin;
  for(double z:{a,b,c}) if(std::isfinite(z)) scale=std::max(scale,std::abs(z-origin));
  if(scale==0) {out.status=TriangStatus::Unbounded;return out;}
  if(!std::isfinite(scale)) return out;
  for(double& z:x) z=(z-origin)/scale;
  if(fa) a=(a-origin)/scale;
  if(fb) b=(b-origin)/scale;
  if(fc) c=(c-origin)/scale;
  std::vector<double> modes;
  const bool forced_a=fa&&a==x.front(), forced_b=fb&&b==x.back();
  if((forced_a&&forced_b)||(fc&&forced_a&&c!=a)||(fc&&forced_b&&c!=b)) {
    out.status=TriangStatus::Incompatible;return out;
  }
  if(fc) modes.push_back(c);
  else if(forced_a) modes.push_back(a);
  else if(forced_b) modes.push_back(b);
  else for(double z:x) if((modes.empty()||z!=modes.back())&&(!fa||z>=a)&&(!fb||z<=b)) modes.push_back(z);
  // A constant sample can still collapse unless a held value prevents it.
  if(x.back()==0 && (!fa||a==0)&&(!fb||b==0)&&(!fc||c==0)) {
    out.status=TriangStatus::Unbounded;return out;
  }
  const double tie=1e-10*x.size();
  double best=-INFINITY;
  std::size_t chosen=0;
  std::vector<triang_detail::Profile> profiles(modes.size(), {0,0,-INFINITY,-INFINITY,0,false});
  std::vector<double> public_modes;
  if(fc) public_modes.push_back(original_c);
  else {
    std::size_t i=0;
    for(double mode:modes) {
      while(x[i]<mode) ++i;
      double representative=observations[i];
      if(i+1<x.size() && x[i+1]==mode && observations[i+1]!=representative)
        for(double z : input) if((z-origin)/scale==mode) { representative=z; break; }
      public_modes.push_back(representative);
    }
  }
  const bool balanced_pair=fa&&fb&&modes.size()==2&&x.size()%2==0&&
      observations[x.size()/2-1]==observations.front()&&
      observations[x.size()/2]==observations.back();
  auto comparison_budget=certificate_budget(maxit);
  const auto compare=[&](std::size_t i,std::size_t j) {
    const auto& p=profiles[i];
    const auto& q=profiles[j];
    if(std::abs(p.value-q.value)>tie) return p.value>q.value ? 1 : -1;
    if(fa&&fb) {
      // One exact sign comparison for a balanced pair, or one factor
      // enclosure per observation for a general fixed-endpoint pair.
      if(!balanced_pair && x.size()>static_cast<std::size_t>(comparison_budget)) return 0;
      if(!balanced_pair) comparison_budget-=static_cast<std::int64_t>(x.size());
      return triang_detail::compare_modes(observations,original_a,original_b,
                                          public_modes[i],public_modes[j],balanced_pair);
    }
    // Compare the feasible profile values with their tangent upper bounds.
    // The practical margin covers rounding in the normalized evaluations.
    const double margin=256*std::numeric_limits<double>::epsilon()*
        (x.size()+std::abs(p.value)+std::abs(q.value));
    if(p.value>q.upper+margin) return 1;
    if(q.value>p.upper+margin) return -1;
    return 0;
  };
  bool success=true;
  double certificate_upper=-INFINITY;
  std::vector<double> known_ll;
  if(fa&&fb) {
    std::vector<double> prefix(x.size()+1,0), suffix(x.size()+1,0);
    for(std::size_t i=0;i<x.size();++i)
      prefix[i+1]=prefix[i]+(x[i]>a?std::log(x[i]-a):0);
    for(std::size_t i=x.size();i>0;--i)
      suffix[i-1]=suffix[i]+(x[i-1]<b?std::log(b-x[i-1]):0);
    std::size_t left=0,right=0;
    for(double m:modes) {
      while(left<x.size()&&x[left]<m) ++left;
      while(right<x.size()&&x[right]<=m) ++right;
      double ll=x.size()*std::log(2/(b-a));
      if(m<a||m>b||(x.front()==a&&m!=a)||(x.back()==b&&m!=b)) ll=-INFINITY;
      else {
        if(left) ll+=prefix[left]-left*std::log(m-a);
        if(right<x.size()) ll+=suffix[right]-(x.size()-right)*std::log(b-m);
      }
      known_ll.push_back(ll);
    }
  }
  std::vector<bool> visited(modes.size(),false);
  auto candidate=[&](std::size_t k) {
    if(visited[k]) return;
    visited[k]=true;
    auto p=fa&&fb ? triang_detail::Profile{a,b,known_ll[k],known_ll[k],0,true}
                        : triang_detail::profile(x,modes[k],modes[k],a,b,tol,maxit);
    out.iterations+=p.iterations;
    // Strip the deliberately generous pruning allowance; public acceptance
    // below supplies its own operand-scaled floating-point margin.
    certificate_upper=std::max(certificate_upper,p.upper-
        ((fa&&fb)?0:1e-11*x.size()*(1+std::abs(p.value)/x.size())));
    if(!std::isfinite(p.value)) return;
    success=success&&p.converged;
    auto& saved=profiles[k];
    saved=p;
    saved.upper=std::max(p.value,p.upper-
        ((fa&&fb)?0:1e-11*x.size()*(1+std::abs(p.value)/x.size())));
    const int order=std::isfinite(best) ? compare(k,chosen) : 1;
    // Keep the existing representative convention when the sign is open.
    if(order>0 || (order==0 && (p.value>best+1e-13*x.size() ||
        (std::abs(p.value-best)<=1e-13*x.size()&&modes[k]<modes[chosen])))) {
      chosen=k;
      best=p.value;
    }
  };
  if(!modes.empty()) {
    candidate(0);
    if(modes.size()>1) candidate(modes.size()-1);
    struct Range {std::size_t lo,hi;};
    std::vector<Range> stack;
    if(modes.size()>2) stack.push_back({1,modes.size()-2});
    while(!stack.empty()) {
      const auto r=stack.back();stack.pop_back();
      if(prune&&!(fa&&fb)&&r.lo<r.hi) {
        const double l=modes[r.lo], u=modes[r.hi], mid=(l+u)/2;
        const double dl=l-(fa?a:x.front()), dr=(fb?b:x.back())-u;
        if(dl>0&&dr>0) {
          // For ANY endpoints, log L(c) has downward score jumps and
          // second derivative <= B=n/min(l-a,b-u)^2. Its tangent at mid
          // plus B*(c-mid)^2/2 is an upper bound. The midpoint score
          // -nl/(mid-a)+nr/(b-mid) is linear in reciprocal distances.
          // Maximize the two tilted concave endpoint objectives (t=+-h),
          // then add B*h^2/2. Supremum over endpoints preserves the bound.
          const double h=(u-l)/2;
          const auto left=triang_detail::profile(x,mid,mid,a,b,tol,maxit,-h);
          const auto right=triang_detail::profile(x,mid,mid,a,b,tol,maxit,h);
          out.iterations+=left.iterations+right.iterations;
          const double upper=std::max(left.upper,right.upper)+
            x.size()*h*h/(2*std::pow(std::min(dl,dr),2));
          if(upper<best-tie) {certificate_upper=std::max(certificate_upper,upper);continue;}
        }
      }
      const auto mid=r.lo+(r.hi-r.lo)/2;
      candidate(mid);
      if(mid>r.lo) stack.push_back({r.lo,mid-1});
      if(mid<r.hi) stack.push_back({mid+1,r.hi});
    }
  }
  if(!std::isfinite(best)) {out.status=TriangStatus::Incompatible;return out;}
  const double minimum=observations.front(), maximum=observations.back();
  const auto coefficients=[&](std::size_t i) {
    const auto& p=profiles[i];
    const double mode=public_modes[i];
    double lower=fa?original_a:(p.a==modes[i]?mode:origin+scale*p.a);
    double upper=fb?original_b:(p.b==modes[i]?mode:origin+scale*p.b);
    // Rounding a free support endpoint inward must never discard data.
    if(!fa && (lower>minimum || (lower==minimum && mode!=minimum)))
      lower=std::nextafter(minimum,-INFINITY);
    if(!fb && (upper<maximum || (upper==maximum && mode!=maximum)))
      upper=std::nextafter(maximum,INFINITY);
    return std::array<double,3>{lower,upper,mode};
  };
  const auto selected=coefficients(chosen);
  out.a=selected[0]; out.b=selected[1]; out.c=selected[2];
  if(!(std::isfinite(out.a)&&std::isfinite(out.b)&&std::isfinite(out.c)&&
       out.a<out.b&&out.a<=out.c&&out.c<=out.b)) {
    out.status=TriangStatus::NumericalFailure;return out;
  }
  const double predicted=best-input.size()*std::log(scale);
  double actual=0,magnitude=0;
  for(double z:input) {
    const double value=distributions::dtriang(z,out.a,out.b,out.c,true);
    actual+=value;magnitude+=std::abs(value);
  }
  out.logLik=actual;
  // The local endpoint solve cannot certify a different represented point.
  // This practical margin covers summation and affine evaluation, not a
  // finite likelihood loss caused by coarse public endpoint spacing.
  const double margin=256*std::numeric_limits<double>::epsilon()*
      (input.size()+magnitude+std::abs(predicted));
  const bool same_point=std::isfinite(actual) && std::abs(actual-predicted)<=margin;
  // Rounding can lose more than evaluation roundoff (a steep maximizer: the
  // continuous endpoint lies between two public numbers). Accept the public
  // point when it is the best public endpoint and the global bound certifies
  // the continuous optimum it was rounded from; logLik stays the value at the
  // returned coefficients, below the supremum by the rounding loss.
  const double continuous_gap=certificate_upper-best;
  const bool rounded=!same_point && success && std::isfinite(actual) &&
      triang_detail::rounded_endpoint(input,out.a,out.b,out.c,fa,fb) &&
      continuous_gap<=tol*std::max(1.0,std::abs(predicted))+margin;
  const bool represented=same_point || rounded;
  out.status=!represented?TriangStatus::NumericalFailure:
      (success?TriangStatus::Ok:TriangStatus::IterationFailure);
  if (out.status==TriangStatus::Ok && requested_tol<method_floor &&
      ((!fa && profiles[chosen].a!=modes[chosen]) || (!fb && profiles[chosen].b!=modes[chosen]))) {
    out.status=TriangStatus::IterationFailure;
    out.stop_detail.accuracy_stop=AccuracyStop::Tolerance;
    out.stop_detail.attainable_tol=method_floor;
    const double width=out.b-out.a;
    record_accuracy(out,requested_tol,{out.a,out.b,out.c},
        {fa?0:std::max(width,std::abs(out.a)),fb?0:std::max(width,std::abs(out.b)),0});
  }
  if(out.status==TriangStatus::Ok) {
    const auto center=triang_detail::sum_parts(minimum,maximum);
    if(fa==fb&&!fc) {
      bool symmetric=std::isfinite(center.first) && std::isfinite(center.second) &&
          (!fa || triang_detail::sum_parts(original_a,original_b)==center);
      for(std::size_t i=0;symmetric&&i<observations.size()/2+observations.size()%2;++i)
        symmetric=triang_detail::sum_parts(observations[i],observations[observations.size()-1-i])==center;
      out.reflection_tie=symmetric && triang_detail::sum_parts(out.c,out.c)!=center;
    }
    for(std::size_t i=0;i<profiles.size();++i) {
      if(i==chosen || !std::isfinite(profiles[i].value)) continue;
      if(out.reflection_tie && triang_detail::sum_parts(out.c,public_modes[i])==center) continue;
      if(compare(chosen,i)>0) continue;
      const auto rival=coefficients(i);
      double logLik=0;
      for(double z:input) logLik+=distributions::dtriang(z,rival[0],rival[1],rival[2],true);
      out.unresolved_modes.push_back({rival[0],rival[1],rival[2],logLik});
    }
    out.not_unique=out.reflection_tie || !out.unresolved_modes.empty();
  }
  return out;
}
} // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const TriangResult& r) {
  if (r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)
    return FitOutcome::failure(OutcomeReason::Tolerance);
  switch (r.status) {
    case TriangStatus::Ok: return FitOutcome::estimate(r.unresolved_modes.empty(),!r.not_unique);
    case TriangStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TriangStatus::Incompatible: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TriangStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case TriangStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TriangStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
