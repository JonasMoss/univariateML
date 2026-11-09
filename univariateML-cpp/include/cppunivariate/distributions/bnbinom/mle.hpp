// Global scalar, pair and finite all-free covers. General winning all-free
// NB faces retain the legacy comparison when unproved. Faces are fitted separately.
#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/precision.hpp"
#include "dpqr.hpp"
#include "../nbinom/mle.hpp"
#include "../pois/mle.hpp"
#include "../../core/span.hpp"
#include <array>
#include <map>
#include <vector>

namespace cppunivariate {
enum class BnbinomStatus { Ok, InvalidInput, NegativeBinomialLimit, PoissonLimit,
                           PointMass, IterationFailure, NumericalFailure, ComparisonTie };
struct BnbinomResult {
  double size, alpha, beta, logLik;
  int iterations;
  BnbinomStatus status;
  StopDetail stop_detail{};
  std::array<double,2> global_interval{{NAN,NAN}};
  std::array<double,2> profile_alpha{{NAN,NAN}};
  std::array<double,4> profile_mode{{NAN,NAN,NAN,NAN}};
  bool certified=false;
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
  maxit = static_cast<int>(solver_budget(maxit));
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
    for (int ls=0;ls<maxit;++ls) {
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
// Scalar score differences use a conservative practical cancellation margin.
// The single-crossing proof and floating-point contract are in README.md.
struct ScalarScore { double value, margin; };
inline ScalarScore scalar_score(const Data &d, const std::vector<double> &p, int j) {
  const double r=p[0], a=p[1], b=p[2];
  const double first=special::digamma(j==1?a+r:(j==0?a+r:a+b));
  const double second=special::digamma(j==1?a+b:(j==0?r:b));
  CPPUNIV_EXTENDED_REAL sum=0, magnitude=0;
  for (std::size_t i=0;i<d.x.size();++i) {
    const double third=special::digamma(j==1?a:(j==0?r:b)+d.x[i]);
    const double fourth=special::digamma(a+r+b+d.x[i]);
    const double term=j==1?first+second-third-fourth:first-second+third-fourth;
    sum+=d.count[i]*term;
    magnitude+=d.count[i]*(std::abs(first)+std::abs(second)+std::abs(third)+std::abs(fourth));
  }
  return {static_cast<double>(sum/d.n),
    128*std::numeric_limits<double>::epsilon()*static_cast<double>(magnitude/d.n)};
}
inline BnbinomResult scalar_fit(const Cost &c, double tol, int maxit) {
  BnbinomResult out{NAN,NAN,NAN,NAN,0,BnbinomStatus::NumericalFailure};
  const int j=c.free[0];
  const int steps=std::min(maxit,100000);
  const long long cap=128LL*steps;
  long long evaluations=0;
  std::vector<double> best;
  double best_ll=-INFINITY;
  auto evaluate=[&](double u) -> int {
    if (++evaluations>cap) return 2;
    auto p=c.fixed; p[j]=std::exp(u);
    if (!(p[j]>0) || !std::isfinite(p[j])) return 2;
    const auto score=scalar_score(c.d,p,j);
    const double ll=likelihood(c.d,p);
    if (std::isfinite(ll) && ll>best_ll) { best=p; best_ll=ll; }
    if (!std::isfinite(score.value) || !std::isfinite(score.margin) || !std::isfinite(ll)) return 2;
    return score.value>score.margin?1:(score.value < -score.margin?-1:0);
  };
  auto stop=[&](bool budget) -> BnbinomResult {
    out.status=budget?BnbinomStatus::IterationFailure:BnbinomStatus::NumericalFailure;
    out.stop_detail.budget_exhausted=budget;
    if (!best.empty()) {
      const double margin=1e-10*c.d.n*(1+std::abs(best_ll/c.d.n));
      out.logLik=best_ll;
      out.stop_detail.candidates.push_back({best,best_ll-margin,best_ll+margin,StopModel::None});
      out.stop_detail.loglik_lower=best_ll-margin;
    }
    out.stop_detail.loglik_upper=INFINITY;
    return out;
  };
  double lo=0, hi=0, radius=1;
  bool left=false, right=false;
  // Expansion and refinement count certificate evaluations; each can be raised
  // through maxit. No exp/log range endpoint can be returned as a fit.
  while (!left || !right) {
    if (evaluations+2>cap) return stop(true);
    if (!left) { const int sign=evaluate(-radius); if(sign==2)return stop(false); if(sign==1){lo=-radius;left=true;} }
    if (!right) { const int sign=evaluate(radius); if(sign==2)return stop(false); if(sign==-1){hi=radius;right=true;} }
    radius*=2;
  }
  const double width=std::min(tol,1e-8);
  for (int it=0;it<steps;++it) {
    ++out.iterations;
    if (hi-lo<=width) {
      auto p=c.fixed; p[j]=std::exp(lo+(hi-lo)/2);
      const double ll=likelihood(c.d,p);
      if(!std::isfinite(ll))return stop(false);
      out.size=p[0];out.alpha=p[1];out.beta=p[2];out.logLik=ll;
      out.status=BnbinomStatus::Ok;return out;
    }
    if (evaluations+3>cap) return stop(true);
    const double mid=lo+(hi-lo)/2;
    if(mid==lo || mid==hi)return stop(false);
    const int sign=evaluate(mid);
    if(sign==2)return stop(false);
    if(sign==1)lo=mid;
    else if(sign==-1)hi=mid;
    else {
      // A small score alone never certifies coefficient accuracy. Preserve
      // signed endpoints and try both quarter points instead.
      const double l=lo+(hi-lo)/4, h=hi-(hi-lo)/4;
      const int sl=evaluate(l), sh=evaluate(h);
      if(sl==2 || sh==2)return stop(false);
      bool progress=false;
      if(sl==1){lo=l;progress=true;} else if(sl==-1){hi=l;progress=true;}
      if(sh==-1 && h<hi){hi=h;progress=true;} else if(sh==1 && h>lo){lo=h;progress=true;}
      if(!progress || !(lo<hi))return stop(false);
    }
  }
  return stop(true);
}
// Alpha-held certificates use single-crossing coordinate scores to enclose
// every stationary maximum, then cover that rectangle. See README.md.
struct AlphaBox { double rl, rh, bl, bh; };
struct AlphaRoot { AlphaBox box; std::vector<double> p; double ll, error; };
struct AlphaLikelihood { double value, margin; };
inline AlphaLikelihood alpha_likelihood_bound(const Data &d,double a,const AlphaBox &q) {
  CPPUNIV_EXTENDED_REAL value=d.n*zero(q.rl,a,q.bl),magnitude=std::abs(value);
  const CPPUNIV_EXTENDED_REAL t=static_cast<CPPUNIV_EXTENDED_REAL>(a)+q.rh+q.bh;
  for(std::size_t i=0;i<d.x.size();++i) {
    const int x=d.x[i];
    const CPPUNIV_EXTENDED_REAL r=increment(q.rh,x),b=increment(q.bh,x),
      denominator=increment(t,x),factorial=special::lgamma(x+1.);
    value+=d.count[i]*(r+b-denominator-factorial);
    magnitude+=d.count[i]*(std::abs(r)+std::abs(b)+std::abs(denominator)+std::abs(factorial));
  }
  const double ll=static_cast<double>(value);
  return {ll,1e-10*d.n*(1+std::abs(ll/d.n))+
    512*std::numeric_limits<double>::epsilon()*static_cast<double>(magnitude)};
}
struct AlphaHessian { double rr, rb, bb; };
inline AlphaHessian alpha_hessian(const Data &d, double a, double r, double b) {
  double rr=special::trigamma(a+r)-special::trigamma(r);
  double bb=special::trigamma(a+b)-special::trigamma(b), rb=0;
  for(std::size_t i=0;i<d.x.size();++i) {
    const double w=d.count[i]/d.n, t=special::trigamma(a+r+b+d.x[i]);
    rr+=w*(special::trigamma(r+d.x[i])-t);
    bb+=w*(special::trigamma(b+d.x[i])-t);rb-=w*t;
  }
  return {rr,rb,bb};
}
// Componentwise interval Hessian, using decreasing trigamma. All intervals
// receive a practical cancellation margin; these are not outward roundings.
inline std::array<AlphaHessian,2> alpha_hessian_bounds(const Data &d,double a,const AlphaBox &q) {
  AlphaHessian lo{special::trigamma(a+q.rh)-special::trigamma(q.rl),0,
                  special::trigamma(a+q.bh)-special::trigamma(q.bl)};
  AlphaHessian hi{special::trigamma(a+q.rl)-special::trigamma(q.rh),0,
                  special::trigamma(a+q.bl)-special::trigamma(q.bh)};
  double mag=special::trigamma(a+q.rh)+special::trigamma(q.rl)+
    special::trigamma(a+q.rl)+special::trigamma(q.rh)+
    special::trigamma(a+q.bh)+special::trigamma(q.bl)+
    special::trigamma(a+q.bl)+special::trigamma(q.bh);
  for(std::size_t i=0;i<d.x.size();++i) {
    const double w=d.count[i]/d.n, x=d.x[i];
    const double tl=special::trigamma(a+q.rh+q.bh+x), th=special::trigamma(a+q.rl+q.bl+x);
    lo.rr+=w*(special::trigamma(q.rh+x)-th);
    hi.rr+=w*(special::trigamma(q.rl+x)-tl);
    lo.bb+=w*(special::trigamma(q.bh+x)-th);
    hi.bb+=w*(special::trigamma(q.bl+x)-tl);
    lo.rb-=w*th;hi.rb-=w*tl;
    mag+=w*(special::trigamma(q.rl+x)+special::trigamma(q.bl+x)+2*th);
  }
  const double e=512*std::numeric_limits<double>::epsilon()*mag;
  lo.rr-=e;lo.rb-=e;lo.bb-=e;hi.rr+=e;hi.rb+=e;hi.bb+=e;
  return {lo,hi};
}
inline double alpha_root_error(const Data &d,const std::vector<double> &p,double tol) {
  const double radius=std::min(tol,1e-8)*.25,dr=p[0]*radius,db=p[2]*radius;
  const AlphaBox small{p[0]-dr,p[0]+dr,p[2]-db,p[2]+db};
  const auto h=alpha_hessian_bounds(d,p[1],small);
  const auto r=scalar_score(d,p,0),b=scalar_score(d,p,2);
  const double rr=std::max(std::abs(h[0].rr),std::abs(h[1].rr));
  const double rb=std::max(std::abs(h[0].rb),std::abs(h[1].rb));
  const double bb=std::max(std::abs(h[0].bb),std::abs(h[1].bb));
  // Taylor's theorem transfers the evaluated-center likelihood enclosure to
  // the enclosed stationary point, including coefficient-isolation error.
  return alpha_likelihood_bound(d,p[1],{p[0],p[0],p[2],p[2]}).margin+
    d.n*((std::abs(r.value)+r.margin)*dr+(std::abs(b.value)+b.margin)*db+
         .5*(rr*dr*dr+2*rb*dr*db+bb*db*db));
}
inline bool alpha_isolate(const Cost &c, std::vector<double> &p, AlphaBox &box,
                          double tol,int maxit,int &iterations) {
  // Newton only proposes a center. The interval contraction below must prove
  // existence, uniqueness and coefficient accuracy independently of this loop.
  for(int it=0;it<maxit;++it) {
    if(iterations<std::numeric_limits<int>::max())++iterations;
    const auto g=scalar_score(c.d,p,0), h=scalar_score(c.d,p,2);
    const auto H=alpha_hessian(c.d,p[1],p[0],p[2]);
    const double det=H.rr*H.bb-H.rb*H.rb;
    if(!(H.rr<0 && H.bb<0 && det>0) || !std::isfinite(det))return false;
    const double dr=(H.bb*g.value-H.rb*h.value)/det;
    const double db=(H.rr*h.value-H.rb*g.value)/det;
    if(!std::isfinite(dr) || !std::isfinite(db))return false;
    if(std::max(std::abs(dr)/p[0],std::abs(db)/p[2])<std::min(tol,1e-8)*.01)break;
    if(std::abs(dr)>p[0]*.25 || std::abs(db)>p[2]*.25)return false;
    p[0]-=dr;p[2]-=db;
  }
  if(p[0]>p[2])std::swap(p[0],p[2]);
  const auto g=scalar_score(c.d,p,0), h=scalar_score(c.d,p,2);
  const auto H=alpha_hessian(c.d,p[1],p[0],p[2]);
  const double det=H.rr*H.bb-H.rb*H.rb;
  if(!(H.rr<0 && H.bb<0 && det>0))return false;
  const double m00=H.bb/det,m01=-H.rb/det,m11=H.rr/det;
  const auto contraction=[&](double radius,AlphaBox &q) {
    q={p[0]*(1-radius),p[0]*(1+radius),p[2]*(1-radius),p[2]*(1+radius)};
    const auto bounds=alpha_hessian_bounds(c.d,p[1],q);
    const auto lo=bounds[0],hi=bounds[1];
    // Strict negative definiteness throughout the box makes its stationary
    // point the unique box maximum, including its evaluated center.
    if(!(hi.rr<0 && hi.bb<0 && hi.rr*hi.bb>std::max(lo.rb*lo.rb,hi.rb*hi.rb)))return false;
    const auto product=[](double m,double l,double u) {return m>=0?std::array<double,2>{m*l,m*u}:std::array<double,2>{m*u,m*l};};
    const auto bound=[&](double a0,double a1,double l0,double u0,double l1,double u1,double identity) {
      const auto v=product(a0,l0,u0),w=product(a1,l1,u1);
      return std::max(std::abs(identity-v[0]-w[0]),std::abs(identity-v[1]-w[1]));
    };
    const double e00=bound(m00,m01,lo.rr,hi.rr,lo.rb,hi.rb,1);
    const double e01=bound(m00,m01,lo.rb,hi.rb,lo.bb,hi.bb,0);
    const double e10=bound(m01,m11,lo.rr,hi.rr,lo.rb,hi.rb,0);
    const double e11=bound(m01,m11,lo.rb,hi.rb,lo.bb,hi.bb,1);
    const double er=std::abs(m00*g.value+m01*h.value)+std::abs(m00)*g.margin+std::abs(m01)*h.margin;
    const double eb=std::abs(m01*g.value+m11*h.value)+std::abs(m01)*g.margin+std::abs(m11)*h.margin;
    // The weighted infinity norm is used both for the mapping and contraction.
    const double cr=e00+e01*p[2]/p[0], cb=e11+e10*p[0]/p[2];
    return std::isfinite(cr+cb+er+eb) && cr<1 && cb<1 &&
      er/p[0]+cr*radius<radius && eb/p[2]+cb*radius<radius;
  };
  AlphaBox small;
  if(!contraction(std::min(tol,1e-8)*.25,small))return false;
  double radius=.01;
  for(int it=0;it<maxit;++it) {
    if(contraction(radius,box))return std::isfinite(likelihood(c.d,p)) &&
      std::isfinite(alpha_root_error(c.d,p,tol));
    radius*=.5;
    if(radius<std::min(tol,1e-8)*.25)break;
  }
  return false;
}
inline BnbinomResult alpha_fit(const Cost &c,double tol,int maxit) {
  BnbinomResult out{NAN,NAN,NAN,NAN,0,BnbinomStatus::NumericalFailure};
  const double a=c.fixed[1];
  const long long cap=certificate_budget(maxit);
  long long evaluations=0;
  const auto margin=[&](const std::vector<double> &p) {
    return alpha_likelihood_bound(c.d,a,{p[0],p[0],p[2],p[2]}).margin;
  };
  std::vector<double> best{1,a,1}, diagonal=best;
  double best_ll=likelihood(c.d,best),diagonal_ll=best_ll;
  std::vector<AlphaRoot> roots;
  const auto known_root=[&](const std::vector<double> &p) {
    const double width=std::min(tol,1e-8)*.25;
    for(const auto &root:roots)
      if(p[0]*(1-width)>root.box.rl && p[0]*(1+width)<root.box.rh &&
         p[2]*(1-width)>root.box.bl && p[2]*(1+width)<root.box.bh)return true;
    return false;
  };
  double global_upper=INFINITY;
  const auto stop=[&](bool budget,bool tie=false) {
    out.status=budget?BnbinomStatus::IterationFailure:(tie?BnbinomStatus::ComparisonTie:BnbinomStatus::NumericalFailure);
    out.stop_detail.budget_exhausted=budget;out.stop_detail.comparison_tie=tie;
    out.logLik=best_ll;
    out.stop_detail.candidates.clear();
    const auto retain=[&](const std::vector<double> &p,double ll) {
      const double error=margin(p);
      if(!std::isfinite(ll-error) || !std::isfinite(ll+error))return;
      out.stop_detail.candidates.push_back({p,ll-error,ll+error,StopModel::None});
    };
    retain(best,best_ll);
    if(best[0]!=best[2])retain({best[2],a,best[0]},best_ll);
    retain(diagonal,diagonal_ll);
    for(const auto &root:roots) {
      out.stop_detail.candidates.push_back({root.p,root.ll-root.error,root.ll+root.error,StopModel::None});
      if(root.p[0]!=root.p[2])out.stop_detail.candidates.push_back(
        {{root.p[2],a,root.p[0]},root.ll-root.error,root.ll+root.error,StopModel::None});
    }
    out.stop_detail.loglik_lower=best_ll-margin(best);
    out.stop_detail.loglik_upper=global_upper;
    return out;
  };
  // Proposals are not evidence: no window from Cost constrains the certificate.
  // The diagonal proposal is retained even when it is a saddle.
  for(double r:{.15,1.,6.,40.})for(double b:{.15,1.,6.,40.}) {
    if(r>b)continue;
    auto u=optimise(c,{std::log(r),std::log(b)},tol,maxit,out.iterations);
    auto p=c.decode(u);
    if(p[0]>p[2])std::swap(p[0],p[2]);
    const double ll=likelihood(c.d,p);
    if(std::isfinite(ll) && ll>best_ll){best=p;best_ll=ll;}
    if(r==b && std::abs(p[0]-p[2])<1e-8*(1+p[0])) {
      const double mid=(p[0]+p[2])/2;
      const double dll=likelihood(c.d,{mid,a,mid});
      if(std::isfinite(dll) && dll>diagonal_ll){diagonal={mid,a,mid};diagonal_ll=dll;}
    }
    if(++evaluations>cap)return stop(true);
    AlphaBox box;
    if(alpha_isolate(c,p,box,tol,maxit,out.iterations)) {
      if(!known_root(p))roots.push_back({box,p,likelihood(c.d,p),alpha_root_error(c.d,p,tol)});
    }
  }
  if(!std::isfinite(best_ll))return stop(false);
  // Small-shape exclusion: p_x <= min(1,(r)_x/x!) uniformly in beta.
  double lower=1,small_upper=0;
  while(true) {
    if(++evaluations>cap)return stop(true);
    CPPUNIV_EXTENDED_REAL ll=0;
    double small_magnitude=0;
    for(std::size_t i=0;i<c.d.x.size();++i)if(c.d.x[i]>0) {
      const double numerator=static_cast<double>(increment(lower,c.d.x[i]));
      const double denominator=special::lgamma(c.d.x[i]+1.);
      ll+=c.d.count[i]*std::min(0.,numerator-denominator);
      small_magnitude+=c.d.count[i]*(std::abs(numerator)+std::abs(denominator));
    }
    small_upper=static_cast<double>(ll);
    if(std::isfinite(small_upper) && small_upper+1e-10*c.d.n*(1+std::abs(small_upper/c.d.n))+512*std::numeric_limits<double>::epsilon()*small_magnitude<best_ll-margin(best))break;
    lower*=.5;
    if(!(lower>0))return stop(false);
  }
  // Since the r-score is strictly decreasing in beta, its single-crossing
  // scalar theorem makes a negative score at (upper,lower) exclude every
  // stationary maximum with r>=upper,beta>=lower. Symmetry closes beta too.
  double upper=std::max(1.,2*lower);
  while(true) {
    if(++evaluations>cap)return stop(true);
    const auto score=scalar_score(c.d,{upper,a,lower},0);
    if(!std::isfinite(score.value+score.margin))return stop(false);
    if(score.value < -score.margin)break;
    upper*=2;
    if(!std::isfinite(upper))return stop(false);
  }
  // The diagonal score has a single crossing too: increasing both shapes
  // preserves the scalar sign after its root. This isolates a seam witness,
  // without assuming it maximizes the two-dimensional likelihood.
  double dl=std::log(lower),dh=std::log(upper);
  for(int it=0;it<maxit;++it) {
    if(++evaluations>cap)return stop(true);
    const double mid=std::exp(dl+(dh-dl)/2);
    const auto score=scalar_score(c.d,{mid,a,mid},0);
    const double ll=likelihood(c.d,{mid,a,mid});
    if(std::isfinite(ll) && ll>diagonal_ll){diagonal={mid,a,mid};diagonal_ll=ll;}
    if(score.value>score.margin)dl=std::log(mid);
    else if(score.value < -score.margin)dh=std::log(mid);
    else break;
    if(dh-dl<std::min(tol,1e-8))break;
  }
  {
    if(++evaluations>cap)return stop(true);
    auto p=diagonal;AlphaBox box;
    if(alpha_isolate(c,p,box,tol,maxit,out.iterations)) {
      if(!known_root(p))roots.push_back({box,p,likelihood(c.d,p),alpha_root_error(c.d,p,tol)});
    }
  }
  // Every exterior point is below the witness or is dominated by clipping
  // its large coordinates to upper. Zero is a universal log-likelihood UB.
  global_upper=0;
  std::vector<AlphaBox> queue{{lower,upper,lower,upper}};
  const auto covered=[&](const AlphaBox &q,const AlphaBox &r) {
    return q.rl>=r.rl && q.rh<=r.rh && q.bl>=r.bl && q.bh<=r.bh;
  };
  while(!queue.empty()) {
    if(++evaluations>cap)return stop(true);
    const AlphaBox q=queue.back();queue.pop_back();
    // Work in the identified half; boxes crossing the diagonal stay intact.
    if(q.rl>q.bh)continue;
    bool isolated=false;
    for(const auto &root:roots)if(covered(q,root.box)){isolated=true;break;}
    if(isolated)continue;
    // Monotone mass-ratio enclosure, valid including on the singular diagonal.
    const auto enclosure=alpha_likelihood_bound(c.d,a,q);
    if(!std::isfinite(enclosure.value+enclosure.margin))return stop(false);
    if(enclosure.value+enclosure.margin<best_ll-margin(best))continue;
    // Coordinate profiles need no numerical root at each box: the scalar
    // theorem and negative cross partial propagate corner score signs.
    bool excluded=false;
    for(int j:{0,2}) {
      const auto lo=scalar_score(c.d,{q.rl,a,q.bl},j),hi=scalar_score(c.d,{q.rh,a,q.bh},j);
      if(!std::isfinite(lo.value+lo.margin+hi.value+hi.margin))return stop(false);
      if(lo.value < -lo.margin || hi.value>hi.margin){excluded=true;break;}
    }
    if(excluded)continue;
    // Discover any root missed by proposals, once its box is local enough.
    const double wr=std::log(q.rh/q.rl),wb=std::log(q.bh/q.bl);
    if(std::max(wr,wb)<.02) {
      std::vector<double> p{std::sqrt(q.rl)*std::sqrt(q.rh),a,std::sqrt(q.bl)*std::sqrt(q.bh)};
      if(p[0]>p[2])std::swap(p[0],p[2]);
      AlphaBox box;
      if(alpha_isolate(c,p,box,tol,maxit,out.iterations)) {
        if(!known_root(p))roots.push_back({box,p,likelihood(c.d,p),alpha_root_error(c.d,p,tol)});
        if(covered(q,box))continue;
      }
    }
    if(wr>=wb) {
      const double mid=std::sqrt(q.rl)*std::sqrt(q.rh);
      if(!(q.rl<mid && mid<q.rh))return stop(false);
      queue.push_back({mid,q.rh,q.bl,q.bh});queue.push_back({q.rl,mid,q.bl,q.bh});
    } else {
      const double mid=std::sqrt(q.bl)*std::sqrt(q.bh);
      if(!(q.bl<mid && mid<q.bh))return stop(false);
      queue.push_back({q.rl,q.rh,mid,q.bh});queue.push_back({q.rl,q.rh,q.bl,mid});
    }
  }
  // Full coverage proves every global maximum is one of these isolated roots.
  if(roots.empty())return stop(false);
  std::size_t winner=0;
  for(std::size_t i=1;i<roots.size();++i)if(roots[i].ll>roots[winner].ll)winner=i;
  global_upper=-INFINITY;
  for(const auto &root:roots)global_upper=std::max(global_upper,root.ll+root.error);
  for(std::size_t i=0;i<roots.size();++i)if(i!=winner &&
      roots[i].ll+roots[i].error>=roots[winner].ll-roots[winner].error)return stop(false,true);
  const auto &root=roots[winner];
  out.size=root.p[0];out.alpha=a;out.beta=root.p[2];out.logLik=root.ll;out.status=BnbinomStatus::Ok;
  // Both labeled maxima and the singular-diagonal witness remain inspectable.
  stop(false);
  out.status=BnbinomStatus::Ok;out.logLik=root.ll;
  out.stop_detail.loglik_lower=root.ll-root.error;
  return out;
}

}
} // namespace cppunivariate
#include "pair_certificate.hpp"
#include "alpha_interval_certificate.hpp"
#include "joint_certificate.hpp"
namespace cppunivariate {
inline BnbinomResult bnbinom_mle(ispan x, double tol=1e-8, int maxit=600,
    double size_fixed=std::numeric_limits<double>::quiet_NaN(),
    double alpha_fixed=std::numeric_limits<double>::quiet_NaN(),
    double beta_fixed=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> BnbinomResult {
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
  if (cost.free.size()==1) return bnbinom_detail::scalar_fit(cost,tol,maxit);
  if (cost.free.size()==2 && std::isfinite(alpha_fixed))
    return bnbinom_detail::alpha_fit(cost,tol,maxit);
  const bool fr=std::isnan(size_fixed), fa=std::isnan(alpha_fixed), fb=std::isnan(beta_fixed);
  StopDetail nb_detail, po_detail;
  double face=-std::numeric_limits<double>::infinity();
  BnbinomStatus face_status=BnbinomStatus::IterationFailure;
  double poisson_face=-std::numeric_limits<double>::infinity();
  const auto nb_failure=[&](const NbinomResult &nb) {
    out.status=nb.stop_detail.budget_exhausted?BnbinomStatus::IterationFailure:BnbinomStatus::NumericalFailure;
    out.stop_detail=nb.stop_detail;
    for(auto &candidate:out.stop_detail.candidates)
      if(candidate.model==StopModel::None && candidate.coef.size()==2)candidate.model=StopModel::Nbinom;
    out.stop_detail.loglik_upper=0;return out;
  };
  if (fa && (fr || fb)) {
    if (fr && fb) {
      const auto nb=nbinom_mle(x,1e-10,maxit);
      if(nb.status==NbinomStatus::PrecisionFailure)return nb_failure(nb);
      if (nb.status==NbinomStatus::Ok && !std::isfinite(nb.logLik)) {
        out.status=BnbinomStatus::NumericalFailure;out.stop_detail.loglik_upper=0;return out;
      }
      if (nb.status==NbinomStatus::Ok && std::isfinite(nb.logLik)) {
        nb_detail=discrete_face(StopModel::Nbinom,{nb.size,nb.prob},nb.logLik);
        face=nb.logLik; face_status=BnbinomStatus::NegativeBinomialLimit;
      }
      const auto po=pois_mle(x);
      poisson_face=po.logLik; po_detail=discrete_face(StopModel::Pois,{po.lambda},po.logLik);
      if (po.logLik>=face) { face=po.logLik; face_status=BnbinomStatus::PoissonLimit; }
    } else {
      const double r=fr ? beta_fixed : size_fixed;
      const auto nb=nbinom_mle(x,1e-10,maxit,r);
      if(nb.status!=NbinomStatus::Ok)return nb_failure(nb);
      face=nb.logLik;nb_detail=discrete_face(StopModel::Nbinom,{nb.size,nb.prob},nb.logLik);
      if (!std::isfinite(face)) {out.status=BnbinomStatus::NumericalFailure;out.stop_detail.loglik_upper=0;return out;}
      face_status=BnbinomStatus::NegativeBinomialLimit;
    }
  }
  if(cost.free.size()==2 && fa) {
    const bool mirror=fr;
    bnbinom_detail::Cost canonical{d,{mirror?beta_fixed:size_fixed,NAN,NAN},{1,2}};
    auto fitted=bnbinom_detail::pair_fit(canonical,tol,maxit,face,nb_detail.limit_coef[1]);
    if(mirror) {
      std::swap(fitted.size,fitted.beta);
      for(auto &candidate:fitted.stop_detail.candidates)
        if(candidate.model==StopModel::None && candidate.coef.size()==3)
          std::swap(candidate.coef[0],candidate.coef[2]);
    }
    return fitted;
  }
  if(fr && fa && fb && (bnbinom_detail::compound_poisson_uniform_four_face(d) ||
      bnbinom_detail::mixture_face(d,0,true,maxit))) {
    out.logLik=poisson_face;out.status=BnbinomStatus::PoissonLimit;out.stop_detail=po_detail;
    out.certified=true;
    return out;
  }
  double attempted=-std::numeric_limits<double>::infinity();
  double best=-std::numeric_limits<double>::infinity();
  std::vector<double> best_u,diagonal,attempted_point;
  double diagonal_ll=-INFINITY;
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
      const auto proposed=cost.decode(u);
      if(value>attempted){attempted=value;attempted_point=proposed;}
      if(fr && fb && proposed[0]==proposed[2] && value>diagonal_ll) {
        diagonal=proposed;diagonal_ll=value;
      }
      const auto g=cost.gradient(u);
      double norm=0; for(double v:g) norm=std::hypot(norm,v);
      bool in_range=true; for(double v:u) in_range=in_range && std::isfinite(v) && std::abs(v)<=27;
      if (in_range && std::isfinite(norm) && norm<=std::max(1e-6,10*tol) && value>best) {
        best=value; best_u=u;
      }
    }
  }
  out.logLik=std::isfinite(best)?best:attempted;
  const auto unresolved=[&](bool budget,bool tie=false) {
    out.status=budget?BnbinomStatus::IterationFailure:
      (tie?BnbinomStatus::ComparisonTie:BnbinomStatus::NumericalFailure);
    out.size=NAN;out.alpha=NAN;out.beta=NAN;
    out.stop_detail.budget_exhausted=budget;out.stop_detail.comparison_tie=tie;
    const auto &preferred=face_status==BnbinomStatus::NegativeBinomialLimit?nb_detail:po_detail;
    out.stop_detail.limit_model=preferred.limit_model;
    out.stop_detail.limit_coef=preferred.limit_coef;
    out.stop_detail.limit_loglik=preferred.limit_loglik;
    for(const auto &detail:{nb_detail,po_detail})if(detail.limit_model!=StopModel::None) {
      const double error=1e-9*d.n*(1+std::abs(detail.limit_loglik/d.n));
      out.stop_detail.candidates.push_back({detail.limit_coef,detail.limit_loglik-error,detail.limit_loglik+error,detail.limit_model});
    }
    const auto retain_point=[&](std::vector<double> p,double ll) {
      if(p[0]>p[2])std::swap(p[0],p[2]);
      const double error=bnbinom_detail::alpha_likelihood_bound(d,p[1],{p[0],p[0],p[2],p[2]}).margin;
      if(!std::isfinite(ll-error) || !std::isfinite(ll+error))return;
      for(const auto &c:out.stop_detail.candidates)if(c.model==StopModel::None && c.coef==p)return;
      out.stop_detail.candidates.push_back({p,ll-error,ll+error,StopModel::None});
      if(p[0]!=p[2])out.stop_detail.candidates.push_back({{p[2],p[1],p[0]},ll-error,ll+error,StopModel::None});
    };
    if(!best_u.empty())retain_point(cost.decode(best_u),best);
    if(!diagonal.empty())retain_point(diagonal,diagonal_ll);
    if(!attempted_point.empty())retain_point(attempted_point,attempted);
    out.stop_detail.loglik_lower=-INFINITY;
    for(const auto &candidate:out.stop_detail.candidates)
      out.stop_detail.loglik_lower=std::max(out.stop_detail.loglik_lower,candidate.loglik_lower);
    out.stop_detail.loglik_upper=0;
    return out;
  };
  // Preserve the legacy comparison unless a proved inequality rules it out.
  // Certification failure alone does not turn a successful search into failure.
  bool legacy_face=std::isfinite(face) && face>=attempted-1e-8*d.n;
  auto legacy_status=face_status;
  auto legacy_detail=face_status==BnbinomStatus::PoissonLimit?po_detail:nb_detail;
  double legacy_ll=face;
  if(legacy_face && poisson_face>=attempted-1e-8*d.n && poisson_face>=face-1e-8*d.n) {
    legacy_status=BnbinomStatus::PoissonLimit;legacy_detail=po_detail;legacy_ll=poisson_face;
  }
  const auto face_bound=[&](const StopDetail &detail) {
    if(detail.limit_model==StopModel::Nbinom && detail.limit_coef.size()==2)
      return bnbinom_detail::joint_nb_profile_bound(d,detail.limit_coef[0]);
    const double ll=detail.limit_loglik;
    return bnbinom_detail::AlphaLikelihood{ll,bnbinom_detail::joint_error(d,ll,std::abs(ll))+
      1e-9*d.n*(1+std::abs(ll/d.n))};
  };
  if(legacy_face && legacy_status==BnbinomStatus::PoissonLimit && nb_detail.limit_model==StopModel::Nbinom) {
    const auto nb=face_bound(nb_detail),po=face_bound(po_detail);
    if(std::isfinite(nb.value-nb.margin) && nb.value-nb.margin>po.value+po.margin) {
      legacy_status=BnbinomStatus::NegativeBinomialLimit;legacy_detail=nb_detail;legacy_ll=nb_detail.limit_loglik;
    }
  }
  bool rejected_face=false;
  if(legacy_face && !attempted_point.empty()) {
    const auto &v=attempted_point;
    const double lower=attempted-bnbinom_detail::alpha_likelihood_bound(d,v[1],{v[0],v[0],v[2],v[2]}).margin;
    const auto bound=face_bound(legacy_detail);
    if(std::isfinite(lower) && std::isfinite(bound.value+bound.margin) && lower>bound.value+bound.margin) {
      legacy_face=false;rejected_face=true;
    }
  }
  const auto legacy_fallback=[&]() {
    auto result=unresolved(false);
    result.certified=false;result.global_interval={{NAN,NAN}};
    result.profile_alpha={{NAN,NAN}};result.profile_mode={{NAN,NAN,NAN,NAN}};
    if(legacy_face) {
      result.status=legacy_status;result.logLik=legacy_ll;
      result.stop_detail.limit_model=legacy_detail.limit_model;
      result.stop_detail.limit_coef=legacy_detail.limit_coef;
      result.stop_detail.limit_loglik=legacy_detail.limit_loglik;
      return result;
    }
    if(best_u.empty())return result;
    auto v=cost.decode(best_u);if(v[0]>v[2])std::swap(v[0],v[2]);
    if(rejected_face) {
      const auto bound=face_bound(legacy_detail);
      const double lower=best-bnbinom_detail::alpha_likelihood_bound(d,v[1],{v[0],v[0],v[2],v[2]}).margin;
      if(!(lower>bound.value+bound.margin))return result;
    }
    result.status=BnbinomStatus::Ok;result.size=v[0];result.alpha=v[1];result.beta=v[2];result.logLik=best;
    return result;
  };
  if(best_u.empty())return legacy_fallback();
  auto p=cost.decode(best_u);
  if (fr && fb && p[0]>p[2]) std::swap(p[0],p[2]);
  out.size=p[0]; out.alpha=p[1]; out.beta=p[2]; out.status=BnbinomStatus::Ok;
  // These all-free candidates are numerical witnesses, not a global cover.
  const auto retain=[&](const std::vector<double> &v,double ll) {
    const double error=bnbinom_detail::alpha_likelihood_bound(d,v[1],{v[0],v[0],v[2],v[2]}).margin;
    if(std::isfinite(ll-error) && std::isfinite(ll+error))
      out.stop_detail.candidates.push_back({v,ll-error,ll+error,StopModel::None});
  };
  retain(p,out.logLik);if(fr && fb && p[0]!=p[2])retain({p[2],p[1],p[0]},out.logLik);
  if(!diagonal.empty())retain(diagonal,diagonal_ll);
  out.stop_detail.loglik_lower=out.logLik-bnbinom_detail::alpha_likelihood_bound(d,p[1],{p[0],p[0],p[2],p[2]}).margin;
  out.stop_detail.loglik_upper=0;
  const auto global=bnbinom_detail::joint_cover(d,p,face,poisson_face,tol,maxit,
    nb_detail.limit_coef.empty()?NAN:nb_detail.limit_coef[0]);
  out.iterations=static_cast<int>(std::min(static_cast<long long>(std::numeric_limits<int>::max()),
    static_cast<long long>(out.iterations)+global.iterations));
  if(!global.proved) {
    // A completed finite/face inequality remains proved even if the full
    // coefficient cover did not finish. Never reinstate that defeated face.
    if(legacy_face || rejected_face) {
      const auto bound=face_bound(legacy_detail);
      if(std::isfinite(global.loglik_lower) && global.loglik_lower>bound.value+bound.margin) {
        legacy_face=false;rejected_face=true;
        const auto &v=global.witness_coef;
        const double witness_ll=bnbinom_detail::likelihood(d,{v[0],v[1],v[2]});
        retain({v[0],v[1],v[2]},witness_ll);
        if(v[0]!=v[2])retain({v[2],v[1],v[0]},witness_ll);
        std::vector<double> u;bool in_range=true;
        for(double value:v) {
          const double log_value=std::log(value);u.push_back(log_value);
          in_range=in_range && std::isfinite(log_value) && std::abs(log_value)<=27;
        }
        if(in_range) {
          double norm=0;for(double value:cost.gradient(u))norm=std::hypot(norm,value);
          const double value=witness_ll;
          const double lower=value-bnbinom_detail::alpha_likelihood_bound(d,v[1],{v[0],v[0],v[2],v[2]}).margin;
          const auto previous=cost.decode(best_u);
          const double previous_lower=best-bnbinom_detail::alpha_likelihood_bound(d,previous[1],{previous[0],previous[0],previous[2],previous[2]}).margin;
          if(std::isfinite(norm) && norm<=std::max(1e-6,10*tol) && lower>bound.value+bound.margin &&
              (value>best || !(previous_lower>bound.value+bound.margin))) {
            best=value;best_u=u;retain({v[0],v[1],v[2]},value);
            if(v[0]!=v[2])retain({v[2],v[1],v[0]},value);
          }
        }
      }
    }
    return legacy_fallback();
  }
  p={global.coef[0],global.coef[1],global.coef[2]};
  out.size=p[0];out.alpha=p[1];out.beta=p[2];out.logLik=bnbinom_detail::likelihood(d,p);
  retain(p,out.logLik);if(p[0]!=p[2])retain({p[2],p[1],p[0]},out.logLik);
  out.global_interval={{global.loglik_lower,global.loglik_upper}};
  out.certified=true;
  out.stop_detail.loglik_lower=global.loglik_lower;
  // Certify the global inner profile uniformly near this alpha proposal.
  // Retain a parameter-uniform inner enclosure in addition to the full cover.
  // General winning-NB-face decisions remain separate and unresolved.
  const double width=std::min(1e-6,std::sqrt(tol));
  const auto profile=bnbinom_detail::alpha_interval_profile(d,p,p[1]*(1-width),p[1]*(1+width),tol,maxit);
  out.iterations=static_cast<int>(std::min(static_cast<long long>(std::numeric_limits<int>::max()),
    static_cast<long long>(out.iterations)+profile.iterations));
  if(!profile.proved) {
    for(const auto &candidate:profile.stop_detail.candidates)
      out.stop_detail.candidates.push_back(candidate);
    // The completed joint cover already proves the finite answer. A failed
    // optional inner enclosure cannot invalidate that stronger certificate.
    return out;
  }
  out.profile_alpha={{profile.lower,profile.upper}};
  out.profile_mode={{profile.mode.rl,profile.mode.rh,profile.mode.bl,profile.mode.bh}};
  return out;

  }();
  if(stopped.status==BnbinomStatus::PointMass) {
    stopped.stop_detail=discrete_face(StopModel::Pois,{0},0);stopped.certified=true;
  }
  const int free_count=static_cast<int>(std::isnan(size_fixed))+static_cast<int>(std::isnan(alpha_fixed))+static_cast<int>(std::isnan(beta_fixed));
  if(free_count>0 && free_count<3 && (stopped.status==BnbinomStatus::Ok ||
      stopped.status==BnbinomStatus::NegativeBinomialLimit || stopped.status==BnbinomStatus::PoissonLimit))stopped.certified=true;
  return stopped;
}
}
