// Deterministic multistart likelihood search; no unimodality assumption.
// NB and Poisson faces are fitted separately, never by huge BNB shapes.
#pragma once
#include "../../core/precision.hpp"
#include "dpqr.hpp"
#include "../nbinom/mle.hpp"
#include "../pois/mle.hpp"
#include "../../core/span.hpp"
#include <map>
#include <vector>

namespace cppunivariate {
enum class BnbinomStatus { Ok, InvalidInput, NegativeBinomialLimit, PoissonLimit,
                           PointMass, IterationFailure };
struct BnbinomResult {
  double size, alpha, beta, logLik;
  int iterations;
  BnbinomStatus status;
};
namespace bnbinom_detail {
struct Data {
  std::vector<int> x;
  std::vector<double> count;
  double n=0;
};
inline double likelihood(const Data &d, const std::vector<double> &p) {
  CPPUNIV_EXTENDED_REAL ll=d.n*zero(p[0],p[1],p[2]);
  const CPPUNIV_EXTENDED_REAL t=static_cast<CPPUNIV_EXTENDED_REAL>(p[0])+p[1]+p[2];
  for (std::size_t i=0;i<d.x.size();++i) {
    const int x=d.x[i];
    ll+=d.count[i]*(increment(p[0],x)+increment(p[2],x)-increment(t,x)-special::lgamma(x+1.0));
  }
  return static_cast<double>(ll);
}
struct Cost {
  const Data &d;
  std::vector<double> fixed;
  std::vector<int> free;
  std::vector<double> decode(const std::vector<double> &u) const {
    std::vector<double> p=fixed;
    for (std::size_t j=0;j<free.size();++j) p[free[j]]=std::exp(u[j]);
    return p;
  }
  double value(const std::vector<double> &u) const {
    for (double v:u) if (!std::isfinite(v) || v < -28 || v > 28)
      return std::numeric_limits<double>::infinity();
    return -likelihood(d,decode(u))/d.n;
  }
  std::vector<double> gradient(std::vector<double> u) const {
    std::vector<double> g(u.size());
    for (std::size_t j=0;j<u.size();++j) {
      const double v=u[j], h=1e-4;
      u[j]=v+2*h; const double pp=value(u);
      u[j]=v+h; const double p=value(u);
      u[j]=v-h; const double m=value(u);
      u[j]=v-2*h; const double mm=value(u);
      u[j]=v;
      g[j]=(mm-8*m+8*p-pp)/(12*h);
    }
    return g;
  }
};
// Small vector BFGS, to retain the core's no-raw-pointer interface.
inline std::vector<double> optimise(const Cost &c, std::vector<double> u,
                                    double tol, int maxit, int &iterations) {
  const std::size_t k=u.size();
  std::vector<double> H(k*k,0), g=c.gradient(u);
  for (std::size_t j=0;j<k;++j) H[j*k+j]=1;
  double f=c.value(u);
  int flat=0;
  for (int it=0;it<maxit;++it) {
    ++iterations;
    double norm=0; for (double v:g) norm+=v*v;
    if (norm<tol*tol) break;
    std::vector<double> v(k,0);
    for (std::size_t i=0;i<k;++i) for (std::size_t j=0;j<k;++j) v[i]-=H[i*k+j]*g[j];
    double slope=0; for (std::size_t j=0;j<k;++j) slope+=v[j]*g[j];
    if (!(slope<0)) {
      for (std::size_t j=0;j<k;++j) v[j]=-g[j];
      slope=-norm;
    }
    double step=1, fn=f;
    std::vector<double> next(k);
    bool accepted=false;
    for (int ls=0;ls<45;++ls) {
      for (std::size_t j=0;j<k;++j) next[j]=u[j]+step*v[j];
      fn=c.value(next);
      if (fn<=f+1e-4*step*slope) { accepted=true; break; }
      step*=.5;
    }
    if (!accepted) break;
    const std::vector<double> gn=c.gradient(next);
    std::vector<double> s(k), y(k), Hy(k,0);
    double sy=0;
    for (std::size_t j=0;j<k;++j) { s[j]=next[j]-u[j]; y[j]=gn[j]-g[j]; sy+=s[j]*y[j]; }
    if (sy>1e-20) {
      for (std::size_t i=0;i<k;++i) for (std::size_t j=0;j<k;++j) Hy[i]+=H[i*k+j]*y[j];
      double yHy=0; for (std::size_t j=0;j<k;++j) yHy+=y[j]*Hy[j];
      for (std::size_t i=0;i<k;++i) for (std::size_t j=0;j<k;++j)
        H[i*k+j]+=(1+yHy/sy)*s[i]*s[j]/sy-(s[i]*Hy[j]+Hy[i]*s[j])/sy;
    }
    flat = f-fn < 1e-14*(1+std::abs(f)) ? flat+1 : 0;
    u=next; g=gn; f=fn;
    if (flat>=3) break;
  }
  return u;
}
}
inline BnbinomResult bnbinom_mle(ispan x, double tol=1e-8, int maxit=600,
    double size_fixed=std::numeric_limits<double>::quiet_NaN(),
    double alpha_fixed=std::numeric_limits<double>::quiet_NaN(),
    double beta_fixed=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  BnbinomResult out{nan,nan,nan,nan,0,BnbinomStatus::InvalidInput};
  if (x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1) return out;
  bnbinom_detail::Data d;
  d.n=static_cast<double>(x.size());
  std::map<int,double> counts;
  double sum=0;
  for (int v:x) { if (v<0) return out; counts[v]+=1; sum+=v; }
  for (const auto &kv:counts) { d.x.push_back(kv.first); d.count.push_back(kv.second); }
  bnbinom_detail::Cost cost{d,{size_fixed,alpha_fixed,beta_fixed},{}};
  for (int j=0;j<3;++j) {
    if (std::isnan(cost.fixed[j])) cost.free.push_back(j);
    else if (!(cost.fixed[j]>0) || !std::isfinite(cost.fixed[j])) return out;
  }
  if (cost.free.empty()) return {size_fixed,alpha_fixed,beta_fixed,
      bnbinom_detail::likelihood(d,cost.fixed),0,BnbinomStatus::Ok};
  if (sum==0) { out.logLik=0; out.status=BnbinomStatus::PointMass; return out; }
  const bool fr=std::isnan(size_fixed), fa=std::isnan(alpha_fixed), fb=std::isnan(beta_fixed);
  double face=-std::numeric_limits<double>::infinity();
  BnbinomStatus face_status=BnbinomStatus::IterationFailure;
  double poisson_face=-std::numeric_limits<double>::infinity();
  if (fa && (fr || fb)) {
    if (fr && fb) {
      const auto nb=nbinom_mle(x,1e-10,200);
      if (nb.status==NbinomStatus::Ok && !std::isfinite(nb.logLik)) {
        out.status=BnbinomStatus::IterationFailure; return out;
      }
      if (nb.status==NbinomStatus::Ok && std::isfinite(nb.logLik)) {
        face=nb.logLik; face_status=BnbinomStatus::NegativeBinomialLimit;
      }
      const auto po=pois_mle(x);
      poisson_face=po.logLik;
      if (po.logLik>=face) { face=po.logLik; face_status=BnbinomStatus::PoissonLimit; }
    } else {
      const double r=fr ? beta_fixed : size_fixed;
      face=nbinom_mle(x,1e-10,200,r).logLik;
      if (!std::isfinite(face)) { out.status=BnbinomStatus::IterationFailure; return out; }
      face_status=BnbinomStatus::NegativeBinomialLimit;
    }
  }
  double attempted=-std::numeric_limits<double>::infinity();
  double best=-std::numeric_limits<double>::infinity();
  std::vector<double> best_u;
  // Include the diagonal and asymmetric starts; mirror-equivalent starts
  // are omitted only when both interchangeable shapes are free.
  for (double r:{.15,1.,6.,40.}) for (double b:{.15,1.,6.,40.}) {
    if ((!fr && r!=.15) || (!fb && b!=.15) || (fr && fb && r>b)) continue;
    for (double a:{.3,2.,12.}) {
      if (!fa && a!=.3) continue;
      std::vector<double> p{fr?r:size_fixed,fa?a:alpha_fixed,fb?b:beta_fixed};
      if (fa) p[1]=a*(1+p[0]*p[2]/(1+sum/d.n));
      std::vector<double> u;
      for (int j:cost.free) u.push_back(std::log(p[j]));
      u=bnbinom_detail::optimise(cost,u,tol,maxit,out.iterations);
      const double value=-d.n*cost.value(u);
      attempted=std::max(attempted,value);
      const auto g=cost.gradient(u);
      double norm=0; for(double v:g) norm=std::hypot(norm,v);
      bool in_range=true; for(double v:u) in_range=in_range && std::isfinite(v) && std::abs(v)<=27;
      if (in_range && std::isfinite(norm) && norm<=std::max(1e-6,10*tol) && value>best) {
        best=value; best_u=u;
      }
    }
  }
  out.logLik=std::isfinite(best)?best:attempted;
  if (std::isfinite(face) && face>=attempted-1e-8*d.n) {
    out.logLik=face; out.status=face_status;
    if (poisson_face>=attempted-1e-8*d.n && poisson_face>=face-1e-8*d.n) {
      out.logLik=poisson_face; out.status=BnbinomStatus::PoissonLimit;
    }
    return out;
  }
  if (best_u.empty()) { out.status=BnbinomStatus::IterationFailure; return out; }
  auto p=cost.decode(best_u);
  if (fr && fb && p[0]>p[2]) std::swap(p[0],p[2]);
  out.size=p[0]; out.alpha=p[1]; out.beta=p[2]; out.status=BnbinomStatus::Ok;
  return out;
}
}
