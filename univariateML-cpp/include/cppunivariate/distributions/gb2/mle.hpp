// GB2 in actuar order (q,a,p,b). All optimisation uses standardized logs.
#pragma once
#include "local_accuracy.hpp"
#include "../beta/mle.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
#include "../trgamma/mle.hpp"
#include "../burr/mle.hpp"
#include "../invburr/mle.hpp"
#include "../betapr/mle.hpp"
#include "../lnorm/mle.hpp"
#include <array>

namespace cppunivariate {
enum class Gb2Status { Ok, TrGammaBoundary, InvTrGammaBoundary,
  LognormalBoundary, ParetoBoundary, PowerBoundary, DoubleParetoBoundary,
  InvalidData, InvalidFixed, SearchUnresolved, ScaleOutOfRange,
  WeibullBoundary, InvWeibullBoundary, CertificateTie, CertificateBudget, NumericalFailure };
struct Gb2Result {
  double shape1, shape2, shape3, scale, logLik;
  int iterations;
  Gb2Status status;
  StopDetail stop_detail{};
  bool certified=true;
  bool accuracy_failure=false;
};
} // namespace cppunivariate
#include "power_certificate.hpp"
#include "endpoint_certificate.hpp"
namespace cppunivariate {
namespace gb2_detail {
// Shift both shapes to >= 10, then use a cancellation-resistant Stirling
// expression. The recurrence is exact; only the Stirling remainder is numeric.
inline double logbeta(double p,double q) {
  double correction=0;
  while(p<10){correction+=std::log1p(q/p);p+=1;}
  while(q<10){correction+=std::log1p(p/q);q+=1;}
  const double s=p+q;
  return correction-(p-.5)*std::log1p(q/p)-(q-.5)*std::log1p(p/q)
    -.5*std::log(s)+trgamma_detail::kHalfLog2Pi+
    trgamma_detail::stirling_remainder(p)+trgamma_detail::stirling_remainder(q)-
    trgamma_detail::stirling_remainder(s);
}
inline double softplus(double t) { return std::max(t,0.0)+std::log1p(std::exp(-std::abs(t))); }
struct Point {
  double A=1, c=0, p=1, q=1, ll=-std::numeric_limits<double>::infinity();
  std::array<double,2> g{{0,0}};
  std::array<double,4> h{{0,0,0,0}};
  bool ok=false;
  bool outer_done=false;
};
struct Inner {
  double ll, ga, gc, haa, hac, hcc, lp, lq, u, vu;
};
inline Inner evaluate(const trgamma_detail::LogData &d, double A, double c,
                       double p, double q, bool heldb, double offset) {
  CPPUNIV_EXTENDED_REAL ll=0, ga=0, gc=0, haa=0, hac=0, hcc=0, lp=0, lq=0, su=0, svu=0;
  for (double z : d.u) {
    const double v=heldb ? z+offset : z;
    const double t=A*v-c;
    const double e=std::exp(-std::abs(t));
    const double u=t>=0 ? 1/(1+e) : e/(1+e);
    const double w=e/((1+e)*(1+e));
    // softplus(-t) and softplus(t) both reduce to max(.,0)+log1p(exp(-|t|));
    // the exp(-|t|) and log1p(.) terms are shared with `e` above instead of
    // being recomputed inside two separate softplus() calls (same bits:
    // std::abs(-t)==std::abs(t), so softplus(-t)'s internal exp(-|-t|) is
    // exactly `e`).
    const double log1pe = std::log1p(e);
    const double lpu=-(std::max(-t,0.0)+log1pe), lqu=-(std::max(t,0.0)+log1pe);
    const double vbar=t>=0 ? e/(1+e) : 1/(1+e);
    const double r=p*vbar-q*u;
    ll+=p*lpu+q*lqu; lp+=lpu; lq+=lqu;
    ga+=v*r; gc-=r;su+=u;svu+=v*u;
    haa-=(p+q)*v*v*w; hac+=(p+q)*v*w; hcc-=(p+q)*w;
  }
  const double n=static_cast<double>(d.n);
  return {n*(std::log(A)-logbeta(p,q))+static_cast<double>(ll),
    n/A+static_cast<double>(ga), static_cast<double>(gc),
    -n/(A*A)+static_cast<double>(haa),static_cast<double>(hac),
    static_cast<double>(hcc),static_cast<double>(lp)/n,static_cast<double>(lq)/n,static_cast<double>(su)/n,static_cast<double>(svu)/n};
}
// Strict concavity in (A,c); a held scale restricts c to a line.
inline Point profile(const trgamma_detail::LogData &d,double p,double q,
                     double a,double b,int maxit=300) {
  maxit = static_cast<int>(solver_budget(maxit));
  const bool ha=!std::isnan(a), hb=!std::isnan(b);
  const double offset=hb ? (d.mean-std::log(b))/d.sd : 0;
  double A=ha ? a*d.sd : std::sqrt(special::trigamma(p)+special::trigamma(q));
  double c=hb ? 0 : special::digamma(q)-special::digamma(p);
  Inner e=evaluate(d,A,c,p,q,hb,offset);
  bool ok=false;
  for(int it=0;it<maxit;++it) {
    double da=0,dc=0;
    if(!ha && !hb) {
      const double det=e.haa*e.hcc-e.hac*e.hac;
      if(!(det>0)) break;
      da=(-e.hcc*e.ga+e.hac*e.gc)/det;
      dc=(e.hac*e.ga-e.haa*e.gc)/det;
    } else if(!ha) da=-e.ga/e.haa;
    else if(!hb) dc=-e.gc/e.hcc;
    else {ok=true;break;}
    const double residual=std::max(ha?0:std::abs(e.ga*A),hb?0:std::abs(e.gc));
    if(residual<1e-9*d.n) {ok=true;break;}
    double step=1;
    bool accepted=false;
    for(int ls=0;ls<maxit;++ls) {
      if(A+step*da==A && c+step*dc==c)break;
      if(A+step*da>0) {
        const Inner next=evaluate(d,A+step*da,c+step*dc,p,q,hb,offset);
        if(next.ll>=e.ll-1e-12*d.n) {
          A+=step*da;c+=step*dc;e=next;accepted=true;break;
        }
      }
      step*=0.5;
    }
    if(!accepted) break;
  }
  const double dp=std::log1p(q/p)+trgamma_detail::log_minus_digamma(p)-trgamma_detail::log_minus_digamma(p+q);
  const double dq=std::log1p(p/q)+trgamma_detail::log_minus_digamma(q)-trgamma_detail::log_minus_digamma(p+q);
  const std::array<double,2> g{{p*(e.lp+dp),q*(e.lq+dq)}};
  const double tri=special::trigamma(p+q);
  std::array<double,4> h{{-special::trigamma(p)+tri,tri,tri,-special::trigamma(q)+tri}};
  const std::array<double,2> ca{{(hb?offset:0)-e.vu,-e.vu}};
  const std::array<double,2> cc{{e.u-1,e.u}};
  const double n=static_cast<double>(d.n);
  for(int i=0;i<2;++i)for(int j=0;j<2;++j) {
    double schur=0;
    if(!ha&&!hb) {
      const double det=e.haa*e.hcc-e.hac*e.hac;
      schur=n*(ca[i]*(e.hcc*ca[j]-e.hac*cc[j])+cc[i]*(e.haa*cc[j]-e.hac*ca[j]))/det;
    } else if(!ha) schur=n*ca[i]*ca[j]/e.haa;
    else if(!hb) schur=n*cc[i]*cc[j]/e.hcc;
    h[2*i+j]=(h[2*i+j]-schur)*(i==0?p:q)*(j==0?p:q)+(i==j?g[i]:0);
  }
  return {A,hb ? A*(std::log(b)-d.mean)/d.sd : c,p,q,
    e.ll-d.n*std::log(d.sd)-d.sum,g,h,ok};
}
// Exact kink profile, including both one-sided endpoint suprema.
inline double double_pareto(const trgamma_detail::LogData &d,double b) {
  std::vector<double> y=d.u;
  std::sort(y.begin(),y.end());
  const double n=static_cast<double>(d.n);
  CPPUNIV_EXTENDED_REAL total=0; for(double z:y) total+=z;
  double best=-std::numeric_limits<double>::infinity();
  auto at=[&](double m,CPPUNIV_EXTENDED_REAL left,std::size_t k) {
    const double L=std::max(0.0,static_cast<double>(k*m-left));
    const double R=std::max(0.0,static_cast<double>(total-left-(d.n-k)*m));
    const double s=std::sqrt(L)+std::sqrt(R);
    if(s>0) best=std::max(best,n*std::log(n)-2*n*std::log(s)-n-n*std::log(d.sd)-d.sum);
  };
  CPPUNIV_EXTENDED_REAL prefix=0;
  if(std::isnan(b)) {
    for(std::size_t k=0;k<y.size();++k) {at(y[k],prefix,k);prefix+=y[k];}
  } else {
    const double m=(std::log(b)-d.mean)/d.sd;
    std::size_t k=0; for(double z:y) if(z<m){prefix+=z;++k;}
    at(m,prefix,k);
  }
  return best;
}
// A small dimension BFGS with analytic envelope scores; no pointer API.
inline Point refine(const trgamma_detail::LogData &d,std::array<double,2> v,
                    double p,double q,double a,double b,double box,int maxit,int &evals) {
  maxit = static_cast<int>(solver_budget(maxit));
  const std::array<bool,2> free{{std::isnan(p),std::isnan(q)}};
  auto get=[&](std::array<double,2> w) {
    ++evals;
    if((free[0] && std::abs(w[0])>box)||(free[1] && std::abs(w[1])>box)) return Point{};
    return profile(d,free[0]?std::exp(w[0]):p,free[1]?std::exp(w[1]):q,a,b,maxit);
  };
  Point cur=get(v);
  std::array<double,4> H{{1,0,0,1}};
  for(int it=0;it<maxit;++it) {
    // The existing completion rule already treats an outward box edge as
    // a completed proposal. It cannot be returned as a fitted maximum.
    const bool outward=(free[0]&&std::abs(v[0])>box-0.2&&v[0]*cur.g[0]>0)||
                       (free[1]&&std::abs(v[1])>box-0.2&&v[1]*cur.g[1]>0);
    if(outward)break;
    std::array<double,2> g{{free[0]?cur.g[0]:0,free[1]?cur.g[1]:0}};
    if(std::hypot(g[0],g[1])<1e-8) break;
    std::array<double,2> s{{H[0]*g[0]+H[1]*g[1],H[2]*g[0]+H[3]*g[1]}};
    // Exact envelope Hessian: H_shapes - H_shapes,inner H_inner^-1 H_inner,shapes.
    // Newton is used in a concave basin; BFGS supplies ascent elsewhere.
    const double det=cur.h[0]*cur.h[3]-cur.h[1]*cur.h[2];
    if(free[0]&&free[1]&&cur.h[0]<0&&cur.h[3]<0&&det>1e-18)
      s={{(-cur.h[3]*g[0]+cur.h[1]*g[1])/det,(cur.h[2]*g[0]-cur.h[0]*g[1])/det}};
    else if(free[0]&&!free[1]&&cur.h[0]<0)s={{-g[0]/cur.h[0],0}};
    else if(free[1]&&!free[0]&&cur.h[3]<0)s={{0,-g[1]/cur.h[3]}};
    if(!free[0])s[0]=0;
    if(!free[1])s[1]=0;
    if(s[0]*g[0]+s[1]*g[1]<=0){H={{1,0,0,1}};s=g;}
    const double norm=std::max(std::abs(s[0]),std::abs(s[1]));
    if(norm>2){s[0]*=2/norm;s[1]*=2/norm;}
    double step=1; Point next; std::array<double,2> w;
    bool accepted=false;
    for(int ls=0;ls<maxit;++ls){
      w={{v[0]+step*s[0],v[1]+step*s[1]}};
      if(w==v)break;
      next=get(w);
      if(next.ok && next.ll>=cur.ll+1e-4*d.n*step*(s[0]*g[0]+s[1]*g[1])){accepted=true;break;}
      step*=0.5;
    }
    if(!accepted)break;
    std::array<double,2> dx{{step*s[0],step*s[1]}};
    std::array<double,2> y{{free[0]?g[0]-next.g[0]:0,free[1]?g[1]-next.g[1]:0}};
    const double sy=dx[0]*y[0]+dx[1]*y[1];
    if(sy>1e-16){
      const std::array<double,2> hy{{H[0]*y[0]+H[1]*y[1],H[2]*y[0]+H[3]*y[1]}};
      const double f=(1+(y[0]*hy[0]+y[1]*hy[1])/sy)/sy;
      for(int i=0;i<2;++i)for(int j=0;j<2;++j)
        H[2*i+j]+=f*dx[i]*dx[j]-(dx[i]*hy[j]+hy[i]*dx[j])/sy;
    }
    v=w;cur=next;
  }
  const double residual=std::hypot(free[0]?cur.g[0]:0,free[1]?cur.g[1]:0);
  const bool outward=(free[0]&&std::abs(std::log(cur.p))>box-0.2&&std::log(cur.p)*cur.g[0]>0)||
                     (free[1]&&std::abs(std::log(cur.q))>box-0.2&&std::log(cur.q)*cur.g[1]>0);
  cur.outer_done=residual<1e-7||outward;
  return cur;
}
} // namespace gb2_detail

} // namespace cppunivariate
#include "concave_certificate.hpp"
#include "shape_power_certificate.hpp"
namespace cppunivariate {
inline Gb2Result gb2_mle(span x,double tol=1e-10,int maxit=300,
    double q=std::numeric_limits<double>::quiet_NaN(),
    double a=std::numeric_limits<double>::quiet_NaN(),
    double p=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  Gb2Result bad{q,a,p,b,nan,0,Gb2Status::InvalidData};
  for(double v:{q,a,p,b}) if(!std::isnan(v) && (!(v>0)||!std::isfinite(v))){bad.status=Gb2Status::InvalidFixed;return bad;}
  if(!(tol>0)||!std::isfinite(tol)||maxit<1||x.empty())return bad;
  std::vector<double> logs(x.size());
  for(std::size_t i=0;i<x.size();++i){if(!(x[i]>0)||!std::isfinite(x[i]))return bad;logs[i]=std::log(x[i]);}
  if(!logs.empty()&&std::isnan(a)) {
    bool original_constant=true,log_constant=true;
    for(std::size_t i=1;i<x.size();++i) {
      original_constant=original_constant&&x[i]==x[0];
      log_constant=log_constant&&logs[i]==logs[0];
    }
    if(log_constant&&!original_constant){bad.status=Gb2Status::NumericalFailure;return bad;}
  }
  // Exact held unit beta-shape submodels. Never use these as certificates
  // when that beta shape is free: the constrained and joint maxima differ.
  if(p==1 || q==1) {
    Gb2Result out=bad;
    if(p==1) {
      const auto r=burr_mle_holds(x,q,a,b,tol,maxit);
      out={r.shape1,r.shape2,1,r.scale,r.logLik,r.iterations,Gb2Status::SearchUnresolved,r.stop_detail};
      out.certified=r.certified;
      if(r.status==BurrStatus::Ok)out.status=Gb2Status::Ok;
      if(r.status==BurrStatus::InvalidInit || r.status==BurrStatus::Unbounded)out.status=Gb2Status::InvalidData;
      if(r.status==BurrStatus::CertificateTie)out.status=Gb2Status::CertificateTie;
      if(r.status==BurrStatus::CertificateBudget)out.status=Gb2Status::CertificateBudget;
      if(r.status==BurrStatus::NumericalFailure)out.status=Gb2Status::NumericalFailure;
      if(r.status==BurrStatus::ParetoBoundary || r.status==BurrStatus::AllGeOne)out.status=Gb2Status::ParetoBoundary;
      if(r.status==BurrStatus::WeibullBoundary)out.status=Gb2Status::WeibullBoundary;
      for(auto& c:out.stop_detail.candidates)if(c.model==StopModel::None && c.coef.size()==3)c.coef.insert(c.coef.begin()+2,1);
    } else {
      const auto r=invburr_mle_holds(x,p,a,b,tol,maxit);
      out={1,r.shape2,r.shape1,r.scale,r.logLik,r.iterations,Gb2Status::SearchUnresolved,r.stop_detail};
      out.certified=r.certified;
      if(r.status==InvBurrStatus::Ok)out.status=Gb2Status::Ok;
      if(r.status==InvBurrStatus::InvalidInit || r.status==InvBurrStatus::Unbounded)out.status=Gb2Status::InvalidData;
      if(r.status==InvBurrStatus::CertificateTie)out.status=Gb2Status::CertificateTie;
      if(r.status==InvBurrStatus::CertificateBudget)out.status=Gb2Status::CertificateBudget;
      if(r.status==InvBurrStatus::NumericalFailure)out.status=Gb2Status::NumericalFailure;
      if(r.status==InvBurrStatus::PowerBoundary || r.status==InvBurrStatus::AllLeOne)out.status=Gb2Status::PowerBoundary;
      if(r.status==InvBurrStatus::InvWeibullBoundary)out.status=Gb2Status::InvWeibullBoundary;
      for(auto& c:out.stop_detail.candidates)if(c.model==StopModel::None && c.coef.size()==3)c.coef={1,c.coef[1],c.coef[0],c.coef[2]};
    }
    return out;
  }
  if(!std::isnan(a) && std::isnan(b) && (std::isnan(p)!=std::isnan(q)))
    return gb2_power_detail::fit(x,q,a,p,tol,maxit);
  if(std::isnan(a)&&!std::isnan(b)&&(std::isnan(p)!=std::isnan(q)))
    return gb2_shape_power_detail::fit(x,span{logs.data(),logs.size()},q,p,b,tol,maxit);
  const auto solve_root=[&](auto score){return burr_hold_detail::root(score,maxit);};
  // Constant samples still have a finite constrained maximum when only
  // one beta shape, the power, or location is free.
  bool constant=!logs.empty();for(double v:logs)constant=constant&&v==logs[0];
  if(constant){
    if(!std::isnan(p)&&!std::isnan(q)&&(std::isnan(a)!=std::isnan(b))) {
      if(std::isnan(a)&&x[0]==b)return bad;
      trgamma_detail::LogData cd;
      cd.n=logs.size();cd.mean=logs[0];cd.sd=1;cd.sum=logs.size()*logs[0];
      cd.u.assign(logs.size(),0);
      return gb2_concave_detail::fit(cd,q,a,p,b,tol,maxit);
    }
    if(!std::isnan(a)&&!std::isnan(b)&&(!std::isnan(p)||!std::isnan(q))){
      const double u=a*(logs[0]-std::log(b));
      if(std::isnan(p))p=std::exp(solve_root([&](double z){double v=std::exp(z);return special::digamma(v+q)-special::digamma(v)-gb2_detail::softplus(-u);}));
      if(!std::isfinite(p)){bad.status=Gb2Status::SearchUnresolved;return bad;}
      if(std::isnan(q))q=std::exp(solve_root([&](double z){double v=std::exp(z);return special::digamma(p+v)-special::digamma(v)-gb2_detail::softplus(u);}));
      if(!std::isfinite(q)){bad.status=Gb2Status::SearchUnresolved;return bad;}
    } else if(!std::isnan(p)&&!std::isnan(q)&&!std::isnan(b)&&std::isnan(a)){
      if(x[0]==b)return bad;
      const double w=logs[0]-std::log(b);
      a=std::exp(solve_root([&](double z){double u=std::exp(z)*w;return 1+u*(p-(p+q)*burr_hold_detail::sigmoid(u));}));
      if(!std::isfinite(a)){bad.status=Gb2Status::SearchUnresolved;return bad;}
    }
    if(!std::isnan(a)&&!std::isnan(p)&&!std::isnan(q)){
      if(std::isnan(b))b=std::exp(logs[0]-(std::log(p)-std::log(q))/a);
      const double u=a*(logs[0]-std::log(b));
      double ll=logs.size()*(std::log(a)-logs[0]-gb2_detail::logbeta(p,q)-p*gb2_detail::softplus(-u)-q*gb2_detail::softplus(u));
      return {q,a,p,b,ll,0,std::isfinite(ll)?Gb2Status::Ok:Gb2Status::SearchUnresolved};
    }
    return bad;
  }
  trgamma_detail::LogData d;
  if(!trgamma_detail::standardize(span{logs.data(),logs.size()},d)) {
    if(!logs.empty()&&!std::isnan(q)&&!std::isnan(a)&&!std::isnan(p)) {
      if(std::isnan(b)) b=std::exp(logs[0]-(std::log(p)-std::log(q))/a);
      if(!(b>0)||!std::isfinite(b)){bad.status=Gb2Status::ScaleOutOfRange;return bad;}
      CPPUNIV_EXTENDED_REAL ll=0;
      for(double y:logs) {
        const double t=a*(y-std::log(b));
        ll+=std::log(a)-y-gb2_detail::logbeta(p,q)-p*gb2_detail::softplus(-t)-q*gb2_detail::softplus(t);
      }
      return {q,a,p,b,static_cast<double>(ll),0,Gb2Status::Ok};
    }
    return bad;
  }
  if(!std::isnan(p)&&!std::isnan(q)&&(std::isnan(a)||std::isnan(b)))
    return gb2_concave_detail::fit(d,q,a,p,b,tol,maxit);
  double face=-std::numeric_limits<double>::infinity();
  Gb2Status fs=Gb2Status::SearchUnresolved;
  bool face_failure=false;
  auto consider=[&](double ll,Gb2Status s) {
    if(ll>face || (fs==Gb2Status::DoubleParetoBoundary &&
        (s==Gb2Status::ParetoBoundary||s==Gb2Status::PowerBoundary) &&
        ll>=face-1e-11*d.n)) {face=ll;fs=s;}
  };
  if(std::isnan(b)) {
    if(std::isnan(q)) {
      const auto gg=trgamma_fit_log(span{logs.data(),logs.size()},1e-10,maxit,p,a);
      if(gg.stop_detail.budget_exhausted) {
        Gb2Result out{nan,nan,nan,nan,gg.logLik,gg.iterations,Gb2Status::CertificateBudget,gg.stop_detail};
        for(auto& c:out.stop_detail.candidates)if(c.model==StopModel::None)c.model=StopModel::Trgamma;
        return out;
      }
      face_failure=face_failure||gg.status==TrGammaStatus::IterationFailure;
      consider(gg.logLik,gg.status==TrGammaStatus::LognormalBoundary?Gb2Status::LognormalBoundary:
        gg.status==TrGammaStatus::PowerBoundary?Gb2Status::PowerBoundary:Gb2Status::TrGammaBoundary);
    }
    if(std::isnan(p)) {
      std::vector<double> neg=logs;for(double &v:neg)v=-v;
      const auto gg=trgamma_fit_log(span{neg.data(),neg.size()},1e-10,maxit,q,a);
      if(gg.stop_detail.budget_exhausted) {
        const double jac=-2*d.sum;
        Gb2Result out{nan,nan,nan,nan,gg.logLik+jac,gg.iterations,Gb2Status::CertificateBudget,gg.stop_detail};
        out.stop_detail.loglik_lower+=jac;out.stop_detail.loglik_upper+=jac;
        out.stop_detail.limit_loglik+=jac;
        for(auto& c:out.stop_detail.candidates) {
          if(c.model==StopModel::None && c.coef.size()==3){c.model=StopModel::InvTrgamma;c.coef[2]=1/c.coef[2];}
          c.loglik_lower+=jac;c.loglik_upper+=jac;
        }
        if(out.stop_detail.limit_model==StopModel::Lnorm && out.stop_detail.limit_coef.size()==2)out.stop_detail.limit_coef[0]=-out.stop_detail.limit_coef[0];
        if(out.stop_detail.limit_model==StopModel::Power && out.stop_detail.limit_coef.size()==2){out.stop_detail.limit_model=StopModel::Pareto;out.stop_detail.limit_coef={out.stop_detail.limit_coef[1],1/out.stop_detail.limit_coef[0]};}
        return out;
      }
      face_failure=face_failure||gg.status==TrGammaStatus::IterationFailure;
      consider(gg.logLik-2*d.sum,gg.status==TrGammaStatus::LognormalBoundary?Gb2Status::LognormalBoundary:
        gg.status==TrGammaStatus::PowerBoundary?Gb2Status::ParetoBoundary:Gb2Status::InvTrGammaBoundary);
    }
  }
  if(std::isnan(a)&&std::isnan(p)&&std::isnan(q)) {
    consider(lognormal_mle(x).logLik,Gb2Status::LognormalBoundary);
    consider(gb2_detail::double_pareto(d,b),Gb2Status::DoubleParetoBoundary);
  }
  // Endpoint faces also apply with a positive held opposite beta shape.
  if(std::isnan(a)&&std::isnan(b)) {
    if(std::isnan(p))consider(power_mle(x,0).logLik,Gb2Status::PowerBoundary);
    if(std::isnan(q))consider(pareto_mle(x).logLik,Gb2Status::ParetoBoundary);
  }
  if(std::isnan(a)&&!std::isnan(b)) {
    const double gap=std::log(b)-d.mean;
    // A held opposite shape costs shape*log(2) for an observation exactly
    // at the held endpoint. With both shapes free that penalty vanishes.
    if(std::isnan(p)&&gap>=d.sd*d.umax) {
      const double penalty=!std::isnan(q) ? q*std::log(2.0)*std::count(logs.begin(),logs.end(),std::log(b)) : 0;
      consider(d.n*(-std::log(gap)-1)-d.sum-penalty,Gb2Status::PowerBoundary);
    }
    if(std::isnan(q)&&gap<=d.sd*d.umin) {
      const double penalty=!std::isnan(p) ? p*std::log(2.0)*std::count(logs.begin(),logs.end(),std::log(b)) : 0;
      consider(d.n*(-std::log(-gap)-1)-d.sum-penalty,Gb2Status::ParetoBoundary);
    }
  }
  std::vector<std::array<double,2>> starts;
  for(double lp:{-4.0,-1.0,0.0,2.0,5.0})for(double lq:{-4.0,-1.0,0.0,2.0,5.0})starts.push_back({{lp,lq}});
  if(std::isnan(a)&&std::isnan(b)&&std::isnan(p)&&std::isnan(q)) {
    // Only member seeds use a bounded deterministic subsample.
    // All face comparisons and final profile/score gates retain the full data.
    std::vector<double> seed_data;
    if(x.size()>1000) {
      // One distinct index per block avoids a modular stride aliasing n.
      // Quotient/remainder construction also avoids multiplying n by j.
      const std::size_t width=x.size()/1000,remainder=x.size()%1000;
      for(std::size_t j=0;j<1000;++j) {
        const std::size_t begin=j*width+j*remainder/1000;
        const std::size_t end=(j+1)*width+(j+1)*remainder/1000;
        seed_data.push_back(x[begin+(j*104729u)%(end-begin)]);
      }
    }
    const span seed=seed_data.empty()?x:span{seed_data.data(),seed_data.size()};
    const auto bu=burr_mle_legacy(seed,1e-8,maxit);
    if(bu.status==BurrStatus::Ok) starts.push_back({{0,std::log(bu.shape1)}});
    const auto ib=invburr_mle(seed,1e-8,maxit);
    if(ib.status==InvBurrStatus::Ok) starts.push_back({{std::log(ib.shape1),0}});
    const auto bp=betapr_mle(seed,1e-6,maxit);
    if(bp.status==BetaPrimeStatus::Ok) starts.push_back({{std::log(bp.alpha),std::log(bp.beta)}});
  }
  if(!std::isnan(p)||!std::isnan(q)) {
    starts.clear();
    for(double v:{-4.0,-1.0,0.0,2.0,5.0})
      starts.push_back({{std::isnan(p)?v:std::log(p),std::isnan(q)?v:std::log(q)}});
    if(!std::isnan(p)&&!std::isnan(q))starts={{{std::log(p),std::log(q)}}};
  }
  gb2_detail::Point best;
  int evals=0;double box=8, searched_box=8;
  bool explored=false;
  for(int expansion=0;expansion<maxit;++expansion) {
    searched_box=box;
    // Give every grid/member/face seed a short exploration, then spend the
    // full budget on distinct promising basins rather than repeatedly
    // following the same long, flat escape from many seeds.
    std::vector<gb2_detail::Point> basins;
    for(auto v:starts) {
      const auto candidate=gb2_detail::refine(d,v,p,q,a,b,box,maxit,evals);
      if(candidate.ok) {
        basins.push_back(candidate);
        if(candidate.ll>best.ll)best=candidate;
      }
    }
    std::sort(basins.begin(),basins.end(),[](const gb2_detail::Point &l,const gb2_detail::Point &r){return l.ll>r.ll;});
    std::vector<std::array<double,2>> selected;
    for(const auto &seed:basins) {
      const std::array<double,2> v{{std::log(seed.p),std::log(seed.q)}};
      bool distinct=true;
      for(const auto &w:selected) {
        if(std::hypot(std::isnan(p)?v[0]-w[0]:0,std::isnan(q)?v[1]-w[1]:0)<0.5)distinct=false;
      }
      if(!distinct)continue;
      selected.push_back(v);
      const auto candidate=gb2_detail::refine(d,v,p,q,a,b,box,maxit,evals);
      explored=explored||(candidate.ok&&candidate.outer_done);
      if(candidate.ok&&candidate.ll>best.ll)best=candidate;
      if(selected.size()>=8)break;
    }
    const bool edge=(std::isnan(p)&&std::abs(std::log(best.p))>box-0.2)||
                    (std::isnan(q)&&std::abs(std::log(best.q))>box-0.2);
    if(!edge || basins.empty())break;
    // Once an explored outward proposal is dominated by a resolved face,
    // the existing final face comparison can decide without more boxes.
    if(explored && !face_failure && face>=best.ll-1e-8*d.n)break;
    // Beyond this representable domain exp(box) cannot supply a finite
    // proposal. The final edge gate still returns SearchUnresolved.
    if(box>std::log(std::numeric_limits<double>::max())-3)break;
    starts={{{std::log(best.p),std::log(best.q)}},{{-box,-box}},{{box,box}},{{-box,box}},{{box,-box}}};
    box+=3;
  }
  if(best.ok && explored && !face_failure && face_numerical_tie(face,best.ll,1e-8*d.n)) return {nan,nan,nan,nan,face,evals,Gb2Status::SearchUnresolved};
  if(best.ok && explored && face>=best.ll-1e-8*d.n && !face_failure) return {nan,nan,nan,nan,face,evals,fs};
  bool local_done=false;
  if(best.ok && std::isnan(q)&&!std::isnan(a)&&std::isnan(p)&&!std::isnan(b)) {
    // With power and scale held, the logit transform is exactly beta. Its
    // mean/precision solve avoids the singular cancellation of two large
    // shape scores. Keep endpoint distances on the better represented side.
    double left=0,right=0;for(double y:logs){double t=a*(y-std::log(b));left=std::min(left,t);right=std::max(right,t);}
    const bool reflect=right>-left;std::vector<double> probability,uncertainty;bool represented=true;
    for(std::size_t i=0;i<logs.size();++i){double t=(reflect?-1:1)*a*gb2_accuracy::log_ratio(x[i],b);double e=std::exp(-std::abs(t));double u=t>=0?1/(1+e):e/(1+e);represented=represented&&u>0&&u<1;probability.push_back(u);
      const double lr=gb2_accuracy::log_ratio(x[i],b),logb=std::log(b);
      const double error_lr=x[i]>=b*.5&&x[i]<=b*2?gb2_accuracy::spacing((x[i]-b)/b)/(1+(x[i]-b)/b)+gb2_accuracy::spacing(lr):gb2_accuracy::spacing(logs[i])+gb2_accuracy::spacing(logb)+gb2_accuracy::spacing(lr);
      const double error_t=a*error_lr+gb2_accuracy::spacing(t),den=1+e;
      uncertainty.push_back((e*error_t+gb2_accuracy::spacing(e)+(t>=0?1:e)*gb2_accuracy::spacing(den))/(den*den)+gb2_accuracy::spacing(u));
    }
    if(represented&&tol>=8*std::numeric_limits<double>::epsilon()) {
      const auto beta=beta_mle(span{probability.data(),probability.size()},std::min(tol,1e-10)*.125,maxit);
      if(beta.status==BetaStatus::Ok&&gb2_accuracy::input_error(span{probability.data(),probability.size()},span{uncertainty.data(),uncertainty.size()},beta.alpha,beta.beta)<=tol*.5) {
        best.p=reflect?beta.beta:beta.alpha;best.q=reflect?beta.alpha:beta.beta;
        best.ll=beta.logLik;for(std::size_t i=0;i<logs.size();++i){double t=a*gb2_accuracy::log_ratio(x[i],b);best.ll+=std::log(a)-logs[i]-gb2_detail::softplus(-t)-gb2_detail::softplus(t);}
        local_done=true;
      } else if(beta.status==BetaStatus::Degenerate||(beta.status==BetaStatus::Ok&&std::min(beta.alpha,beta.beta)*std::numeric_limits<double>::epsilon()>tol*.125)) {
        bad.status=Gb2Status::NumericalFailure;bad.accuracy_failure=true;return bad;
      }
    }
  }
  const double proposal_score=std::hypot(std::isnan(p)?best.g[0]:0,std::isnan(q)?best.g[1]:0);
  if(!local_done && best.ok && proposal_score<1e-3/d.n && face<best.ll-1e-8*d.n) {
    // The shared numerical route (including its held subsets) gets a joint
    // correction. Earlier held-subset certificate routes are unchanged.
    const std::array<bool,4> free{{std::isnan(q),std::isnan(a),std::isnan(p),std::isnan(b)}};
    const bool shifted=best.q>=16&&best.p/best.q<.125;
    const std::array<double,4> start{{std::log(best.q),std::log(best.A),std::log(best.p),best.c-(shifted?std::log(best.q):0)}};
    std::array<int,4> index{{-1,-1,-1,-1}};std::array<double,4> v{};int dim=0;
    for(int j=0;j<4;++j)if(free[j]){index[j]=dim;v[dim++]=start[j];}
    const auto unpack=[&](const std::array<local_accuracy::Jet,4>& z){
      const auto Q=free[0]?local_accuracy::exp(z[index[0]]):local_accuracy::Jet(q);
      const auto A=free[1]?local_accuracy::exp(z[index[1]]):local_accuracy::Jet(a*d.sd);
      const auto P=free[2]?local_accuracy::exp(z[index[2]]):local_accuracy::Jet(p);
      const auto C=free[3]?(z[index[3]]+(shifted?local_accuracy::log(Q):local_accuracy::Jet(0))):A*((std::log(b)-d.mean)/d.sd);
      return std::array<local_accuracy::Jet,4>{{Q,A,P,C}};
    };
    const auto objective=[&](const std::array<local_accuracy::Jet,4>& z){
      const auto t=unpack(z);const auto Q=t[0],A=t[1],P=t[2],C=t[3];
      if(P.v>=16&&Q.v>=16){
        const auto S=P+Q,lp=local_accuracy::log(P),lq=local_accuracy::log(Q);
        local_accuracy::JetSum f(d.n*(.91893853320467274178-.5*(lp+lq-local_accuracy::log(S))+local_accuracy::stirling(P)+local_accuracy::stirling(Q)-local_accuracy::stirling(S)-local_accuracy::log(A)));
        for(double u:d.u){const auto delta=shifted&&free[3]?A*u-z[index[3]]-lp:A*u-C-lp+lq;
          f.add(gb2_accuracy::centered_kernel(P,Q,delta));}
        return f.value();
      }
      if(shifted&&Q.v>=16&&P.v/Q.v<.125) {
        const auto D=free[3]?z[index[3]]:C-local_accuracy::log(Q);
        local_accuracy::JetSum f(d.n*(gb2_accuracy::rare_normalizer(P,Q)-local_accuracy::log(A)));
        for(double u:d.u){auto h=A*u-D;if(h.v>std::log(std::numeric_limits<double>::max()))return local_accuracy::Jet(INFINITY);auto w=local_accuracy::exp(h),r=w/Q;
          f.add(-P*h+(r.v<.125?w+P*local_accuracy::log1p(r)+Q*gb2_accuracy::log1pmx(r):(P+Q)*local_accuracy::log1p(r)));}
        return f.value();
      }
      local_accuracy::JetSum f(d.n*(local_accuracy::logbeta(P,Q)-local_accuracy::log(A)));
      for(double u:d.u){auto h=A*u-C;f.add(P*local_accuracy::softplus(-h)+Q*local_accuracy::softplus(h));}return f.value();
    };
    const auto error=[&](const std::array<double,4>& z,const std::array<double,4>& step){
      const auto t=unpack(local_accuracy::variables(z));double e=0;
      for(int j=0;j<3;++j)if(free[j])e=std::max(e,std::abs(step[index[j]]));
      if(free[3])e=std::max(e,std::abs(d.sd/t[1].v*(step[index[3]]+(shifted&&free[0]?step[index[0]]:0)-t[3].v*(free[1]?step[index[1]]:0))));
      return e;
    };
    if(dim>0&&!local_accuracy::polish(v,dim,tol,maxit,objective,error)){bad.status=Gb2Status::NumericalFailure;bad.accuracy_failure=true;return bad;}
    const auto t=unpack(local_accuracy::variables(v));
    local_done=true;best.q=t[0].v;best.A=t[1].v;best.p=t[2].v;best.c=t[3].v;
    best.ll=-objective(local_accuracy::variables(v)).v-d.n*std::log(d.sd)-d.sum;
  }
  const double logb=d.mean+d.sd*best.c/best.A;
  const bool edge=(std::isnan(p)&&std::abs(std::log(best.p))>searched_box-0.3)||
                  (std::isnan(q)&&std::abs(std::log(best.q))>searched_box-0.3);
  const double score=std::hypot(std::isnan(p)?best.g[0]:0,std::isnan(q)?best.g[1]:0);
  Gb2Status status=best.ok&&!edge&&(local_done||score<tol)&&!face_failure?Gb2Status::Ok:Gb2Status::SearchUnresolved;
  if(status==Gb2Status::Ok&&(!std::isfinite(std::exp(logb))||std::exp(logb)==0))status=Gb2Status::ScaleOutOfRange;
  Gb2Result out{std::isnan(q)?best.q:q,std::isnan(a)?best.A/d.sd:a,
    std::isnan(p)?best.p:p,std::isnan(b)?std::exp(logb):b,best.ll,evals,status};
  if(out.status==Gb2Status::Ok&&std::isnan(b)&&!local_accuracy::represented(out.scale,out.scale,tol)){out.status=Gb2Status::NumericalFailure;out.accuracy_failure=true;}
  return out;
}
} // namespace cppunivariate
