// GB2 in actuar order (q,a,p,b). All optimisation uses standardized logs.
#pragma once
#include "../trgamma/mle.hpp"
#include "../burr/mle.hpp"
#include "../invburr/mle.hpp"
#include "../betapr/mle.hpp"
#include "../lnorm/mle.hpp"
#include <array>

namespace cppunivariate {
enum class Gb2Status { Ok, TrGammaBoundary, InvTrGammaBoundary,
  LognormalBoundary, ParetoBoundary, PowerBoundary, DoubleParetoBoundary,
  InvalidData, InvalidFixed, SearchUnresolved, ScaleOutOfRange };
struct Gb2Result {
  double shape1, shape2, shape3, scale, logLik;
  int iterations;
  Gb2Status status;
};
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
  long double ll=0, ga=0, gc=0, haa=0, hac=0, hcc=0, lp=0, lq=0, su=0, svu=0;
  for (double z : d.u) {
    const double v=heldb ? z+offset : z;
    const double t=A*v-c;
    const double e=std::exp(-std::abs(t));
    const double u=t>=0 ? 1/(1+e) : e/(1+e);
    const double w=e/((1+e)*(1+e));
    const double lpu=-softplus(-t), lqu=-softplus(t);
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
                     double a,double b) {
  const bool ha=!std::isnan(a), hb=!std::isnan(b);
  const double offset=hb ? (d.mean-std::log(b))/d.sd : 0;
  double A=ha ? a*d.sd : std::sqrt(special::trigamma(p)+special::trigamma(q));
  double c=hb ? 0 : special::digamma(q)-special::digamma(p);
  Inner e=evaluate(d,A,c,p,q,hb,offset);
  bool ok=false;
  for(int it=0;it<100;++it) {
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
    for(int ls=0;ls<45;++ls) {
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
  long double total=0; for(double z:y) total+=z;
  double best=-std::numeric_limits<double>::infinity();
  auto at=[&](double m,long double left,std::size_t k) {
    const double L=std::max(0.0,static_cast<double>(k*m-left));
    const double R=std::max(0.0,static_cast<double>(total-left-(d.n-k)*m));
    const double s=std::sqrt(L)+std::sqrt(R);
    if(s>0) best=std::max(best,n*std::log(n)-2*n*std::log(s)-n-n*std::log(d.sd)-d.sum);
  };
  long double prefix=0;
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
  const std::array<bool,2> free{{std::isnan(p),std::isnan(q)}};
  auto get=[&](std::array<double,2> w) {
    ++evals;
    if((free[0] && std::abs(w[0])>box)||(free[1] && std::abs(w[1])>box)) return Point{};
    return profile(d,free[0]?std::exp(w[0]):p,free[1]?std::exp(w[1]):q,a,b);
  };
  Point cur=get(v);
  std::array<double,4> H{{1,0,0,1}};
  for(int it=0;it<maxit;++it) {
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
    for(int ls=0;ls<35;++ls){
      w={{v[0]+step*s[0],v[1]+step*s[1]}};next=get(w);
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

inline Gb2Result gb2_mle(span x,double tol=1e-7,int maxit=300,
    double q=std::numeric_limits<double>::quiet_NaN(),
    double a=std::numeric_limits<double>::quiet_NaN(),
    double p=std::numeric_limits<double>::quiet_NaN(),
    double b=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  Gb2Result bad{q,a,p,b,nan,0,Gb2Status::InvalidData};
  for(double v:{q,a,p,b}) if(!std::isnan(v) && (!(v>0)||!std::isfinite(v))){bad.status=Gb2Status::InvalidFixed;return bad;}
  if(!(tol>0)||!std::isfinite(tol)||maxit<1)return bad;
  std::vector<double> logs(x.size());
  for(std::size_t i=0;i<x.size();++i){if(!(x[i]>0)||!std::isfinite(x[i]))return bad;logs[i]=std::log(x[i]);}
  const auto solve_root=[&](auto score){return burr_hold_detail::root(score,maxit);};
  // Constant samples still have a finite constrained maximum when only
  // one beta shape, the power, or location is free.
  bool constant=!logs.empty();for(double v:logs)constant=constant&&v==logs[0];
  if(constant){
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
      long double ll=0;
      for(double y:logs) {
        const double t=a*(y-std::log(b));
        ll+=std::log(a)-y-gb2_detail::logbeta(p,q)-p*gb2_detail::softplus(-t)-q*gb2_detail::softplus(t);
      }
      return {q,a,p,b,static_cast<double>(ll),0,Gb2Status::Ok};
    }
    return bad;
  }
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
      const auto gg=trgamma_fit_log(span{logs.data(),logs.size()},1e-10,100,p,a);
      face_failure=face_failure||gg.status==TrGammaStatus::IterationFailure;
      consider(gg.logLik,gg.status==TrGammaStatus::LognormalBoundary?Gb2Status::LognormalBoundary:
        gg.status==TrGammaStatus::PowerBoundary?Gb2Status::PowerBoundary:Gb2Status::TrGammaBoundary);
    }
    if(std::isnan(p)) {
      std::vector<double> neg=logs;for(double &v:neg)v=-v;
      const auto gg=trgamma_fit_log(span{neg.data(),neg.size()},1e-10,100,q,a);
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
    const auto bu=burr_mle(x,1e-8,300);
    if(bu.status==BurrStatus::Ok) starts.push_back({{0,std::log(bu.shape1)}});
    const auto ib=invburr_mle(x,1e-8,300);
    if(ib.status==InvBurrStatus::Ok) starts.push_back({{std::log(ib.shape1),0}});
    const auto bp=betapr_mle(x);
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
  for(int expansion=0;expansion<3;++expansion) {
    searched_box=box;
    // Give every grid/member/face seed a short exploration, then spend the
    // full budget on distinct promising basins rather than repeatedly
    // following the same long, flat escape from many seeds.
    std::vector<gb2_detail::Point> basins;
    for(auto v:starts) {
      const auto candidate=gb2_detail::refine(d,v,p,q,a,b,box,std::min(20,maxit),evals);
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
    if(!edge)break;
    starts={{{std::log(best.p),std::log(best.q)}},{{-box,-box}},{{box,box}},{{-box,box}},{{box,-box}}};
    box+=3;
  }
  if(best.ok && explored && face>=best.ll-1e-8*d.n && !face_failure) return {nan,nan,nan,nan,face,evals,fs};
  const double logb=d.mean+d.sd*best.c/best.A;
  const bool edge=(std::isnan(p)&&std::abs(std::log(best.p))>searched_box-0.3)||
                  (std::isnan(q)&&std::abs(std::log(best.q))>searched_box-0.3);
  const double score=std::hypot(std::isnan(p)?best.g[0]:0,std::isnan(q)?best.g[1]:0);
  Gb2Status status=best.ok&&!edge&&score<tol&&!face_failure?Gb2Status::Ok:Gb2Status::SearchUnresolved;
  if(status==Gb2Status::Ok&&(!std::isfinite(std::exp(logb))||std::exp(logb)==0))status=Gb2Status::ScaleOutOfRange;
  return {std::isnan(q)?best.q:q,std::isnan(a)?best.A/d.sd:a,
    std::isnan(p)?best.p:p,std::isnan(b)?std::exp(logb):b,best.ll,evals,status};
}
} // namespace cppunivariate
