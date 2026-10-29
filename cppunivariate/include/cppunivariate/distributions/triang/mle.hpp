// Exact finite mode enumeration with concave endpoint solves and interval bounds.
#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
enum class TriangStatus { Ok, InvalidInput, Incompatible, Unbounded, IterationFailure };
struct TriangResult {
  double a, b, c, logLik;
  int iterations;
  TriangStatus status;
  bool not_unique;
};
namespace triang_detail {
struct Profile { double a, b, value, upper; int iterations; bool converged; };
// For modes in [l,u], replace all middle factors by one, and use l on
// the left, u on the right. This dominates every candidate pointwise.
// In p=L/(l-a), q=R/(b-u), the log envelope is strictly concave:
// n log(2/(u-l+L/p+R/q)) + sum log(1-p*d/L) + sum log(1-q*e/R).
// Its tangent over [0,1]^2 supplies a certified optimization upper bound,
// including when Newton has not converged. Fixed coordinates are omitted.
inline Profile profile(const std::vector<double>& x, double l, double u,
                       double a, double b, double tol, int maxit, double tilt=0) {
  const bool fa = !std::isnan(a), fb = !std::isnan(b);
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
        const double d=(l-z)/L, t=1-pp*d;
        if (!(t>0)) {v[0]=-INFINITY; return;}
        v[0]+=std::log1p(-pp*d); v[1]-=d/t; v[3]-=d*d/(t*t);
      } else if (z>u) {
        const double d=(z-u)/R, t=1-qq*d;
        if (!(t>0)) {v[0]=-INFINITY; return;}
        v[0]+=std::log1p(-qq*d); v[2]-=d/t; v[5]-=d*d/(t*t);
      }
    }
  };
  std::vector<double> v, trial;
  int it=0; bool done=false;
  evaluate(p,q,v);
  for (;it<maxit;++it) {
    if (!std::isfinite(v[0])) break;
    double dp=0,dq=0;
    if (pa && pb) {
      const double det=v[3]*v[5]-v[4]*v[4];
      dp=(-v[5]*v[1]+v[4]*v[2])/det;
      dq=(v[4]*v[1]-v[3]*v[2])/det;
    } else if (pa) dp=-v[1]/v[3];
    else if (pb) dq=-v[2]/v[5];
    else {done=true; break;}
    if (std::max(std::abs(dp),std::abs(dq)) < tol) {done=true; break;}
    double step=1;
    if (dp<0) step=std::min(step,-0.99*p/dp);
    if (dp>0) step=std::min(step,0.99*(1-p)/dp);
    if (dq<0) step=std::min(step,-0.99*q/dq);
    if (dq>0) step=std::min(step,0.99*(1-q)/dq);
    bool accepted=false;
    for (int j=0;j<60;++j) {
      evaluate(p+step*dp,q+step*dq,trial);
      if (trial[0]>=v[0]+1e-4*step*(v[1]*dp+v[2]*dq)-1e-13*n) {
        accepted=true;break;
      }
      step*=0.5;
    }
    if(!accepted) break;
    p+=step*dp; q+=step*dq;
    v.swap(trial);
  }
  double upper=v[0];
  if(pa) upper+=v[1]*(v[1]>0 ? 1-p : -p);
  if(pb) upper+=v[2]*(v[2]>0 ? 1-q : -q);
  // Roundoff allowance only enlarges the pruning bound.
  upper+=1e-11*n*(1+std::abs(v[0])/n);
  return {fa?a:l-L/p,fb?b:u+R/q,v[0],upper,it,done};
}
} // namespace triang_detail

// NaN means free. prune=false is the exhaustive reference path.
inline TriangResult triang_mle(span input, double tol=1e-12, int maxit=100,
    double a=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN(),
    double c=std::numeric_limits<double>::quiet_NaN(), bool prune=true) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TriangResult out{nan,nan,nan,-INFINITY,0,TriangStatus::InvalidInput,false};
  if(input.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1) return out;
  for(double z:input) if(!std::isfinite(z)) return out;
  for(double z:{a,b,c}) if(!std::isnan(z) && !std::isfinite(z)) return out;
  const double original_a=a, original_b=b, original_c=c;
  const bool fa=!std::isnan(a), fb=!std::isnan(b), fc=!std::isnan(c);
  if((fa&&fb&&a>=b)||(fa&&fc&&a>c)||(fb&&fc&&c>b)) return out;
  std::vector<double> x(input.begin(),input.end());
  std::sort(x.begin(),x.end());
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
  double best=-INFINITY, second=-INFINITY, ba=0,bb=0,bc=0;
  bool success=true;
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
    if(!std::isfinite(p.value)) return;
    success=success&&p.converged;
    // Ties at floating-point precision choose the smallest mode.
    if(p.value>best+1e-13*x.size() || (std::abs(p.value-best)<=1e-13*x.size()&&modes[k]<bc)) {
      second=std::max(second,best);best=p.value;ba=p.a;bb=p.b;bc=modes[k];
    } else second=std::max(second,p.value);
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
          if(upper<best-tie) continue;
        }
      }
      const auto mid=r.lo+(r.hi-r.lo)/2;
      candidate(mid);
      if(mid>r.lo) stack.push_back({r.lo,mid-1});
      if(mid<r.hi) stack.push_back({mid+1,r.hi});
    }
  }
  if(!std::isfinite(best)) {out.status=TriangStatus::Incompatible;return out;}
  out.a=fa?original_a:origin+scale*ba;
  out.b=fb?original_b:origin+scale*bb;
  // Preserve exact endpoint-mode equality across the affine round trip.
  if(fc && !fa && ba==bc) out.a=original_c;
  if(fc && !fb && bb==bc) out.b=original_c;
  out.c=fc?original_c:origin+scale*bc;
  if(!(std::isfinite(out.a)&&std::isfinite(out.b)&&std::isfinite(out.c)&&
       out.a<out.b&&out.a<=out.c&&out.c<=out.b)) {
    out.status=TriangStatus::IterationFailure;return out;
  }
  out.logLik=best-input.size()*std::log(scale);
  out.status=success?TriangStatus::Ok:TriangStatus::IterationFailure;
  out.not_unique=second>=best-tie;
  return out;
}
} // namespace cppunivariate
