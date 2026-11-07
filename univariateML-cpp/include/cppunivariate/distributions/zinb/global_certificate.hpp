#pragma once
#include "../../core/work_budget.hpp"
#include "certificate.hpp"
#include "../zip/certificate.hpp"
#include "../ztnbinom/mle.hpp"

namespace cppunivariate {
namespace zinb_global_detail {
struct Sum {
  double value=0,correction=0;
  void add(double x){const double y=x-correction,t=value+y;correction=(t-value)-y;value=t;}
};
struct Interval {double lo,hi;};
struct Point {double ll,error,ga,gb,ge,aa,ab,bb;};
struct Curvature {Interval aa,ab,bb;};
// Bernoulli series remainder at .01 is below 1e-27.
inline double jminusone(double u) {
  const double u2=u*u;
  return -u/2+u2*(1./12+u2*(-1./720+u2*(1./30240+u2*(-1./1209600+u2/47900160))));
}
inline double J(double u) {
  if(u<.01)return 1+jminusone(u);
  const double e=std::exp(-u);return u*e/(-std::expm1(-u));
}
inline double h(double u) {return u<.01?u+jminusone(u):u/(-std::expm1(-u))-1;}
inline double V(double u) {return J(u)*h(u);}
inline double K(double u) {const double j=J(u);return j*(u+j);}
inline double logratio(double u) {
  if(u<.01) {const double u2=u*u;return -u/2+u2*(1./24+u2*(-1./2880+u2*(1./181440-u2/9676800)));}
  return std::log(-std::expm1(-u))-std::log(u);
}
struct Data {
  std::map<int,double> counts;
  double n=0,k=0,S=0,n0=0,z=0,A=0,factorial=0,pi=NAN;
  int M=0;
  double pad(double scale) const {return 4096*std::numeric_limits<double>::epsilon()*(1+scale)*(1+M*std::numeric_limits<double>::epsilon());}
  double inflation(double t) const {
    if(!std::isnan(pi))return pi;
    const double den=-std::expm1(-t),eta=k/n;return den>eta?1-eta/den:0;
  }
  double weight(double t) const {
    if(std::isnan(pi))return -std::expm1(-t)>k/n?k/(-std::expm1(-t)):n;
    const double f=std::exp(-t),q=pi==0?1:(1-pi)*f/(pi+(1-pi)*f);
    return k+n0*q;
  }
  Point at(double a,double b) const {
    const double r=std::exp(a),t=std::exp(b),u=t/r;
    Sum rise_sum,deficit_sum,d_sum;
    for(const auto& kv:counts)for(int j=1;j<kv.first;++j) {
      const double q=j/r;
      rise_sum.add(kv.second*std::log1p(q));
      deficit_sum.add(kv.second*q/(1+q));
      d_sum.add(kv.second*q/((1+q)*(1+q)));
    }
    const double rise=rise_sum.value,deficit=deficit_sum.value,D=d_sum.value;
    const double zero=inflation(t),j=J(u),v=V(u),W=weight(t);
    double ll=-k*t+S*b+factorial+rise+S*logratio(u),extra=0,C=0;
    if(std::isnan(pi) && zero>0) {
      extra=n0*std::log1p(-k/n)+k*std::log(k/n)-k*std::log(-std::expm1(-t));
      C=k*K(t);
    } else {
      const double zero_ll=zinb_cert_detail::zero_logmass(-t,zero);
      extra=n0*zero_ll+k*std::log1p(-zero);
      if(!std::isnan(pi)) {
        const double f=std::exp(-t),q=pi==0?1:(1-pi)*f/(pi+(1-pi)*f);
        C=n0*t*t*q*(1-q);
      }
    }
    ll+=extra;
    const double error=pad(std::abs(k*t)+std::abs(S*b)+std::abs(factorial)+rise+
      std::abs(S*logratio(u))+std::abs(extra));
    const double ga=S*(u<.01?-jminusone(u):1-j)-deficit,gb=S*j-t*W;
    return {ll,error,ga,gb,pad(S+t*W+deficit),D-S*v,S*v,-t*W-S*v+C};
  }
  Point zip_at(double b) const {
    const double t=std::exp(b),p=inflation(t),W=weight(t);
    double ll=-k*t+S*b+factorial,extra=0,C=0;
    if(std::isnan(pi) && p>0) {
      extra=n0*std::log1p(-k/n)+k*std::log(k/n)-k*std::log(-std::expm1(-t));C=k*K(t);
    } else {
      const double zero_ll=zinb_cert_detail::zero_logmass(-t,p);
      extra=n0*zero_ll+k*std::log1p(-p);
      if(!std::isnan(pi)){const double f=std::exp(-t),q=pi==0?1:(1-pi)*f/(pi+(1-pi)*f);C=n0*t*t*q*(1-q);}
    }
    ll+=extra;
    return {ll,pad(k*t+std::abs(S*b)+std::abs(factorial)+std::abs(extra)),0,S-t*W,pad(S+t*W),0,0,-t*W+C};
  }
  Curvature curvature(double al,double ah,double bl,double bh) const {
    const double rl=std::exp(al),rh=std::exp(ah),tl=std::exp(bl),th=std::exp(bh);
    const double ul=tl/rh,uh=th/rl;
    // J decreases and h increases: their product bounds cover V.
    const double vl=J(uh)*h(ul),vh=J(ul)*h(uh);
    Sum dl_sum,dh_sum;
    for(const auto& kv:counts)for(int j=1;j<kv.first;++j) {
      const double l=rl*j/((rl+j)*(rl+j)),h0=rh*j/((rh+j)*(rh+j));
      dl_sum.add(kv.second*std::min(l,h0));
      dh_sum.add(kv.second*((rl<=j && rh>=j)?.25:std::max(l,h0)));
    }
    const double dl=dl_sum.value,dh=dh_sum.value;
    double wl=0,wh=0,cl=0,ch=0;
    if(!std::isnan(pi)) {
      auto q_at=[&](double t){const double f=std::exp(-t);return pi==0?1:(1-pi)*f/(pi+(1-pi)*f);};
      const double ql=q_at(th),qh=q_at(tl);
      wl=tl*(k+n0*ql);wh=th*(k+n0*qh);
      cl=n0*tl*tl*std::min(ql*(1-ql),qh*(1-qh));
      ch=n0*th*th*((ql<=.5 && qh>=.5)?.25:std::max(ql*(1-ql),qh*(1-qh)));
    } else {
      // The join is C1. Union Hessian bounds also cover cells crossing it.
      const double join=n0>0?-std::log1p(-k/n):INFINITY;
      if(th<=join){wl=n*tl;wh=n*th;}
      else if(tl>=join){wl=k*(tl+J(tl));wh=k*(th+J(th));cl=k*K(th);ch=k*K(tl);}
      else {wl=std::min(n*tl,k*(join+J(join)));wh=std::max(n*join,k*(th+J(th)));cl=0;ch=k*K(join);}
    }
    const double e=pad(dl+dh+S*vh+wh+ch);
    return {{dl-S*vh-e,dh-S*vl+e},{std::max(0.,S*vl-e),S*vh+e},
      {-wh-S*vh+cl-e,-wl-S*vl+ch+e}};
  }
};
struct RatePeak {double lo,hi,lower,upper;};
struct Face {
  double lambda=NAN,pi=NAN,ll=NAN,lower=NAN,upper=NAN;bool ok=false;
  double lo=NAN,hi=NAN;std::vector<RatePeak> peaks{};
};
inline Face zip_upper(const Data& d,int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(!std::isnan(d.pi) && d.pi>0 && d.n0>0) {
    const auto cert=zip_certificate_detail::held_rate(d.n,d.k,d.S,d.pi,maxit,
      [&](double rate){return d.zip_at(std::log(rate)).ll;},
      [&](double scale){return d.pad(scale);});
    if(cert.status!=zip_certificate_detail::Status::Ok &&
       cert.status!=zip_certificate_detail::Status::Tie)return {};
    const auto& peak=cert.peaks[cert.best];
    Face out{peak.lo+(peak.hi-peak.lo)/2,d.pi,peak.ll,peak.lower,cert.upper,true};
    out.lo=peak.lo;out.hi=peak.hi;
    for(const auto& p:cert.peaks)out.peaks.push_back({p.lo,p.hi,p.lower,p.upper});
    return out;
  }

  const double mean=d.S/d.n,positive=d.S/d.k;
  double lo=mean,hi=mean;
  if(std::isnan(d.pi) && -std::expm1(-mean)>d.k/d.n) {
    lo=0;hi=positive;
    for(int iter=0;iter<maxit;++iter) {
      const double mid=lo+(hi-lo)/2;
      const double mu=mid/(-std::expm1(-mid)),e=d.pad(mu+positive);
      if(mu>positive+e)hi=mid;
      else if(mu<positive-e)lo=mid;
      else {if(lo>0 && (hi-lo)/lo<1e-7)break;return {};}
      if(lo>0 && (hi-lo)/lo<1e-10)break;
    }
    if(!(lo>0) || (hi-lo)/lo>1e-7)return {};
  }
  const double lambda=lo+(hi-lo)/2,den=-std::expm1(-lambda),eta=d.k/d.n;
  const double pi=std::isnan(d.pi)?(den>eta?1-eta/den:0):d.pi;
  if(std::isnan(d.pi) && pi>0 && std::abs((1-pi)*den-eta)>1e-7*eta)return {};
  double ll=-d.k*lambda+d.S*std::log(lambda)+d.factorial;
  if(!std::isnan(d.pi))ll+=d.k*std::log1p(-pi)-d.n0*lambda;
  else if(pi>0)ll+=d.n0*std::log1p(-d.k/d.n)+d.k*std::log(d.k/d.n)-d.k*std::log(-std::expm1(-lambda));
  else ll-=d.n0*lambda;
  const double arithmetic=d.pad(d.k*lambda+d.S*std::abs(std::log(lambda))+std::abs(d.factorial)+std::abs(ll));
  const double e=(hi-lo)*(d.n+d.S/lo)+arithmetic;
  if(!std::isfinite(ll)||!std::isfinite(e))return {};
  Face out{lambda,pi,ll,ll-arithmetic,ll+e,true};out.lo=lo;out.hi=hi;out.peaks.push_back({lo,hi,ll-arithmetic,ll+e});return out;
}
} // namespace zinb_global_detail

// Global two-coordinate finite-maximizer certificate. The unrestricted ZIP
// upper bound is also valid with inflation held. An unclosed tail stops.
inline ZinbCertificateResult zinb_joint_certificate(ispan x,double fixed_pi=NAN,
    double tol=1e-10,int maxit=200,double start_size=NAN,double start_prob=NAN) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  ZinbCertificateResult out{{nan,nan,nan,nan,0,ZinbStatus::IterationFailure},{}};
  auto fail=[&](ZinbCertificateFailure reason) {
    out.failure=reason;out.fit.status=ZinbStatus::IterationFailure;
    out.stop_detail.budget_exhausted=reason==ZinbCertificateFailure::Budget;
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  // Validate via an evaluated held-size fit, without using its optimum.
  const auto valid=zinb_fixed_size_mle(x,1,nan,fixed_pi,tol,maxit);
  if(valid.fit.status!=ZinbStatus::Ok)return valid;
  zinb_global_detail::Data d;d.pi=fixed_pi;d.n=double(x.size());
  for(int v:x){++d.counts[v];d.S+=v;d.k+=v>0;d.M=std::max(d.M,v);}
  if(d.k==0){out.fit=zinb_candidate_mle(x,tol,maxit,nan,nan,fixed_pi);return out;}
  if(d.M>100000 || d.S>4503599627370496.)return fail(ZinbCertificateFailure::Workload);
  d.n0=d.n-d.k;d.z=d.n0/d.n;
  zinb_global_detail::Sum factorial_sum,a_sum;
  for(const auto& kv:d.counts){factorial_sum.add(-kv.second*special::lgamma(kv.first+1.));a_sum.add(kv.second*kv.first*(kv.first-1)/2);}
  d.factorial=factorial_sum.value;d.A=a_sum.value;
  const auto face=zinb_global_detail::zip_upper(d,maxit);
  if(!face.ok)return fail(ZinbCertificateFailure::Arithmetic);
  out.stop_detail.candidates.push_back({{face.lambda,face.pi},face.lower,face.upper,StopModel::Zip});
  auto candidate=std::isnan(start_size)?
    zinb_candidate_mle(x,std::max(1e-12,std::min(tol,1e-10)),maxit,nan,nan,fixed_pi):
    ZinbResult{start_size,start_prob,nan,nan,0,ZinbStatus::IterationFailure};
  if(!(candidate.size>0 && candidate.prob>0 && candidate.prob<1 && std::isfinite(candidate.size)))return fail(ZinbCertificateFailure::Arithmetic);
  double a=std::log(candidate.size),b=std::log(-candidate.size*std::log(candidate.prob));
  auto point=d.at(a,b);
  // Refine only as a proposal; the global cover proves the result.
  auto eigen_lower=[](const zinb_global_detail::Curvature& c) {
    const double det=c.aa.hi*c.bb.hi-c.ab.hi*c.ab.hi;
    const double value=det/(-c.aa.lo-c.bb.lo);
    return c.aa.hi<0 && c.bb.hi<0 && value>0 && std::isfinite(value)?value:0.;
  };
  double radius=.02,lambda=0;
  for(int iter=0;iter<maxit;++iter) {
    radius=.02;lambda=0;
    for(int trial=0;trial<maxit;++trial) {
      lambda=eigen_lower(d.curvature(a-radius,a+radius,b-radius,b+radius));
      if(lambda>0 && (std::hypot(point.ga,point.gb)+2*point.ge)/lambda<
         1e-8/std::max(1.,2*std::exp(b-a)))break;
      radius/=2;
    }
    if(lambda>0 && (std::hypot(point.ga,point.gb)+2*point.ge)/lambda<
       1e-8/std::max(1.,2*std::exp(b-a)))break;
    const double det=point.aa*point.bb-point.ab*point.ab;
    if(!(point.aa<0 && point.bb<0 && det>0))return fail(ZinbCertificateFailure::Arithmetic);
    const double da=(-point.bb*point.ga+point.ab*point.gb)/det;
    const double db=(point.ab*point.ga-point.aa*point.gb)/det;
    bool accepted=false;
    for(int j=0;j<maxit;++j) {
      const double step=std::ldexp(1.,-j),aa=a+step*da,bb=b+step*db;
      const auto next=d.at(aa,bb);
      if(std::isfinite(next.ll) && next.ll>=point.ll-point.error){a=aa;b=bb;point=next;accepted=true;break;}
    }
    if(!accepted)return fail(ZinbCertificateFailure::Arithmetic);
  }
  radius=.02;lambda=0;
  for(int trial=0;trial<maxit;++trial) {
    lambda=eigen_lower(d.curvature(a-radius,a+radius,b-radius,b+radius));
    if(lambda>0 && (std::hypot(point.ga,point.gb)+2*point.ge)/lambda<
       1e-8/std::max(1.,2*std::exp(b-a)))break;
    radius/=2;
  }
  const double coordinate_error=(std::hypot(point.ga,point.gb)+2*point.ge)/lambda;
  const double r=std::exp(a),t=std::exp(b),u=t/r;
  const double tl_local=std::exp(b-radius),th_local=std::exp(b+radius);
  const double pi_error=std::isnan(fixed_pi)?(d.k/d.n)*th_local*zinb_global_detail::K(tl_local)/(tl_local*tl_local)*coordinate_error:0;
  if(!(lambda>0 && coordinate_error<2e-7 && 2*u*coordinate_error<2e-7 && pi_error<2e-7))
    return fail(ZinbCertificateFailure::Arithmetic);
  const double lower=point.ll-point.error,gap=lower-face.upper;
  out.stop_detail.candidates.push_back({{r,std::exp(-u),d.inflation(t)},lower,point.ll+point.error,StopModel::None});
  out.stop_detail.loglik_lower=lower;
  if(!(gap>d.pad(std::abs(lower)+std::abs(face.upper))))return fail(ZinbCertificateFailure::TailUnresolved);
  // Exterior coercivity bounds, each with a strict witness advantage.
  const double rl=std::min(.5,std::exp(lower/d.k)/2),tl=std::exp(lower/d.k)/2;
  const double rh=std::max(4*r,2*d.A/gap+1);
  if(!(rl>0 && tl>0 && std::isfinite(rh)))return fail(ZinbCertificateFailure::Arithmetic);
  zinb_global_detail::Sum c_sum;c_sum.add(d.factorial);
  for(const auto& kv:d.counts)for(int j=1;j<kv.first;++j)c_sum.add(kv.second*std::log1p(j/rl));
  const double C=c_sum.value;
  double th=std::max(4*t,2*d.S/d.k);
  int work=0;
  while(-d.k*th+d.S*std::log(th)+C>=lower-d.pad(d.k*th+d.S*std::abs(std::log(th))+std::abs(C))) {
    if(++work>maxit){out.fit.iterations=work;return fail(ZinbCertificateFailure::Arithmetic);}
    if(!std::isfinite(th*2))return fail(ZinbCertificateFailure::Arithmetic);
    th*=2;
  }
  const double al=std::log(rl),ah=std::log(rh),bl=std::log(tl),bh=std::log(th);
  if(!(al<a-radius && a+radius<ah && bl<b-radius && b+radius<bh))return fail(ZinbCertificateFailure::Arithmetic);
  struct Cell {double al,ah,bl,bh,upper;};
  std::vector<Cell> cells;
  auto order=[](const Cell& x,const Cell& y){return x.upper<y.upper;};
  auto insert=[&](double l,double h0,double v,double w) {
    const double ac=l+(h0-l)/2,bc=v+(w-v)/2,wa=(h0-l)/2,wb=(w-v)/2;
    const auto p=d.at(ac,bc);const auto c=d.curvature(l,h0,v,w);
    const double aa=std::max(std::abs(c.aa.lo),std::abs(c.aa.hi));
    const double bb=std::max(std::abs(c.bb.lo),std::abs(c.bb.hi));
    const double raw_upper=p.ll+p.error+(std::abs(p.ga)+p.ge)*wa+(std::abs(p.gb)+p.ge)*wb+
      .5*(aa*wa*wa+2*c.ab.hi*wa*wb+bb*wb*wb);
    const double ub=raw_upper+d.pad(std::abs(raw_upper));
    ++work;
    if(!std::isfinite(ub))return false;
    if(ub>=lower){cells.push_back({l,h0,v,w,ub});std::push_heap(cells.begin(),cells.end(),order);}
    return true;
  };
  const std::vector<double> acuts{al,a-radius,a+radius,ah},bcuts{bl,b-radius,b+radius,bh};
  for(int i=0;i<3;++i)for(int j=0;j<3;++j)if(i!=1 || j!=1)
    if(!insert(acuts[i],acuts[i+1],bcuts[j],bcuts[j+1]))return fail(ZinbCertificateFailure::Arithmetic);
  const std::int64_t budget=certificate_budget(maxit);
  while(!cells.empty()) {
    out.fit.iterations=work;
    if(work+2>budget){out.stop_detail.segment_loglik_upper=cells.front().upper;return fail(ZinbCertificateFailure::Budget);}
    std::pop_heap(cells.begin(),cells.end(),order);const auto c=cells.back();cells.pop_back();
    if(c.ah-c.al>=c.bh-c.bl) {
      const double mid=c.al+(c.ah-c.al)/2;
      if(!(mid>c.al && mid<c.ah))return fail(ZinbCertificateFailure::Arithmetic);
      if(!insert(c.al,mid,c.bl,c.bh)||!insert(mid,c.ah,c.bl,c.bh))return fail(ZinbCertificateFailure::Arithmetic);
    } else {
      const double mid=c.bl+(c.bh-c.bl)/2;
      if(!(mid>c.bl && mid<c.bh))return fail(ZinbCertificateFailure::Arithmetic);
      if(!insert(c.al,c.ah,c.bl,mid)||!insert(c.al,c.ah,mid,c.bh))return fail(ZinbCertificateFailure::Arithmetic);
    }
  }
  const double p=std::exp(-u),actual_t=-r*std::log(p),pi=d.inflation(actual_t);
  const double round_error=std::abs(std::log(actual_t/t));
  if(!(p>0 && p<1 && std::isfinite(pi) && round_error<1e-7 &&
      2*u*coordinate_error+u*round_error<2e-7 && pi_error+round_error<2e-7))return fail(ZinbCertificateFailure::Arithmetic);
  if(std::isnan(fixed_pi) && pi>0 && std::abs((1-pi)*(-std::expm1(-actual_t))-d.k/d.n)>1e-7*(d.k/d.n))
    return fail(ZinbCertificateFailure::Arithmetic);
  auto encoded=d;encoded.pi=pi;
  out.fit={r,p,pi,encoded.at(a,std::log(actual_t)).ll,work,ZinbStatus::Ok};
  // Keep the historical proposal only inside the independently certified
  // local coefficient enclosure. This avoids changing existing goldens.
  if(candidate.status==ZinbStatus::Ok && std::abs(std::log(candidate.size/r))<2e-7 && std::abs(std::log(candidate.prob/p))<2e-7 &&
     std::abs(candidate.pi-pi)<2e-7 &&
     std::abs(std::log(-candidate.size*std::log(candidate.prob)/actual_t))<2e-7) {
    out.fit=candidate;auto kept=d;kept.pi=candidate.pi;out.fit.logLik=kept.at(std::log(candidate.size),std::log(-candidate.size*std::log(candidate.prob))).ll;out.fit.status=ZinbStatus::Ok;out.fit.iterations=work;
  }
  out.stop_detail.loglik_upper=point.ll+point.error+
    std::pow(std::hypot(point.ga,point.gb)+2*point.ge,2)/(2*lambda);
  return out;
}


// Signed first-order Poisson-tail bound plus complete finite-rectangle cover.
inline ZinbCertificateResult zinb_zip_certificate(ispan x,double fixed_pi=NAN,
    double tol=1e-10,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto out=zinb_fixed_size_mle(x,1,NAN,fixed_pi,tol,maxit);
  if(out.fit.status!=ZinbStatus::Ok)return out;
  zinb_global_detail::Data d;d.n=double(x.size());d.pi=fixed_pi;
  for(int v:x){++d.counts[v];d.S+=v;d.k+=v>0;d.M=std::max(d.M,v);}
  if(d.k==0){out.fit=zinb_candidate_mle(x,tol,maxit,NAN,NAN,fixed_pi);return out;}
  out.fit={NAN,NAN,NAN,NAN,0,ZinbStatus::IterationFailure};out.stop_detail={};
  auto fail=[&](ZinbCertificateFailure reason) {
    out.failure=reason;out.fit.status=ZinbStatus::IterationFailure;
    out.stop_detail.budget_exhausted=reason==ZinbCertificateFailure::Budget;
    out.stop_detail.comparison_tie=reason==ZinbCertificateFailure::ComparisonTie;
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  if(d.M>100000 || d.S>4503599627370496.)return fail(ZinbCertificateFailure::Workload);
  d.n0=d.n-d.k;d.z=d.n0/d.n;
  zinb_global_detail::Sum fs,as;
  for(const auto& kv:d.counts){fs.add(-kv.second*special::lgamma(kv.first+1.));as.add(kv.second*kv.first*(kv.first-1)/2);}
  d.factorial=fs.value;d.A=as.value;
  const auto face=zinb_global_detail::zip_upper(d,maxit);
  if(!face.ok)return fail(ZinbCertificateFailure::Arithmetic);
  for(const auto& peak:face.peaks) {
    const double rate=peak.lo+(peak.hi-peak.lo)/2;
    out.stop_detail.candidates.push_back({{rate,d.inflation(rate)},peak.lower,peak.upper,StopModel::Zip});
  }
  out.stop_detail.loglik_lower=face.lower;
  const double A=d.A+d.pad(d.A),critical=2*A/d.S;
  if(!(face.lo>critical+d.pad(face.lo+critical)))return fail(ZinbCertificateFailure::TailUnresolved);
  const double cut=critical+(face.lo-critical)/2;
  const double rl=std::min(.5,std::exp(face.lower/d.k)/2),tl=std::exp(face.lower/d.k)/2;
  if(!(rl>0 && tl>0 && tl<cut))return fail(ZinbCertificateFailure::Arithmetic);
  zinb_global_detail::Sum cs;cs.add(d.factorial);
  for(const auto& kv:d.counts)for(int j=1;j<kv.first;++j)cs.add(kv.second*std::log1p(j/rl));
  double th=std::max(4*face.lambda,2*d.S/d.k);int work=0;
  while(-d.k*th+d.S*std::log(th)+cs.value>=face.lower-d.pad(d.k*th+d.S*std::abs(std::log(th))+std::abs(cs.value))) {
    if(++work>maxit){out.fit.iterations=work;return fail(ZinbCertificateFailure::Arithmetic);}
    if(!std::isfinite(th*2))return fail(ZinbCertificateFailure::Arithmetic);
    th*=2;
  }
  const auto at_low=d.zip_at(std::log(tl)),at_cut=d.zip_at(std::log(cut));
  double low_upper=std::max(at_low.ll+at_low.error,at_cut.ll+at_cut.error);
  for(const auto& peak:face.peaks)
    if(peak.lo!=face.lo || peak.hi!=face.hi)low_upper=std::max(low_upper,peak.upper);
  const double gap=face.lower-low_upper-d.pad(std::abs(face.lower)+std::abs(low_upper));
  if(!(gap>0))return fail(ZinbCertificateFailure::ComparisonTie);
  const double den_cut=d.S*cut/2-A,den_high=d.S*th/2-A;
  if(!(den_cut>d.pad(d.S*cut/2+A) && den_high>0))return fail(ZinbCertificateFailure::Arithmetic);
  const double rh=std::max({1.,2*A/gap,d.S*cut*cut/(4*den_cut),d.S*th*th/(4*den_high)});
  if(!std::isfinite(rh) || !(rh>rl))return fail(ZinbCertificateFailure::Arithmetic);
  struct Cell {double al,ah,bl,bh,upper;};
  std::vector<Cell> cells;auto order=[](const Cell& a,const Cell& b){return a.upper<b.upper;};
  auto insert=[&](double al,double ah,double bl,double bh) {
    const double a=al+(ah-al)/2,b=bl+(bh-bl)/2,wa=(ah-al)/2,wb=(bh-bl)/2;
    const auto p=d.at(a,b);const auto c=d.curvature(al,ah,bl,bh);
    const double aa=std::max(std::abs(c.aa.lo),std::abs(c.aa.hi)),bb=std::max(std::abs(c.bb.lo),std::abs(c.bb.hi));
    const double raw=p.ll+p.error+(std::abs(p.ga)+p.ge)*wa+(std::abs(p.gb)+p.ge)*wb+
      (aa*wa*wa+2*c.ab.hi*wa*wb+bb*wb*wb)/2;
    const double upper=raw+d.pad(std::abs(raw));++work;
    if(!std::isfinite(upper))return false;
    if(upper>=face.lower){cells.push_back({al,ah,bl,bh,upper});std::push_heap(cells.begin(),cells.end(),order);}
    return true;
  };
  if(!insert(std::log(rl),std::log(rh),std::log(tl),std::log(th)))return fail(ZinbCertificateFailure::Arithmetic);
  const std::int64_t budget=certificate_budget(maxit);
  while(!cells.empty()) {
    out.fit.iterations=work;
    if(work+2>budget){out.stop_detail.segment_loglik_upper=cells.front().upper;return fail(ZinbCertificateFailure::Budget);}
    std::pop_heap(cells.begin(),cells.end(),order);const auto c=cells.back();cells.pop_back();
    if(c.ah-c.al>=c.bh-c.bl) {
      const double mid=c.al+(c.ah-c.al)/2;
      if(!(mid>c.al && mid<c.ah))return fail(ZinbCertificateFailure::Arithmetic);
      if(!insert(c.al,mid,c.bl,c.bh)||!insert(mid,c.ah,c.bl,c.bh))return fail(ZinbCertificateFailure::Arithmetic);
    }else{
      const double mid=c.bl+(c.bh-c.bl)/2;
      if(!(mid>c.bl && mid<c.bh))return fail(ZinbCertificateFailure::Arithmetic);
      if(!insert(c.al,c.ah,c.bl,mid)||!insert(c.al,c.ah,mid,c.bh))return fail(ZinbCertificateFailure::Arithmetic);
    }
  }
  out.fit.status=face.pi>0?ZinbStatus::ZipLimit:ZinbStatus::PoissonLimit;out.fit.iterations=work;
  out.stop_detail.limit_model=face.pi>0?StopModel::Zip:StopModel::Pois;
  out.stop_detail.limit_coef=face.pi>0?std::vector<double>{face.lambda,face.pi}:std::vector<double>{face.lambda};
  out.stop_detail.limit_loglik=face.ll;out.stop_detail.loglik_lower=face.lower;out.stop_detail.loglik_upper=face.upper;
  return out;
}
inline ZinbCertificateResult zinb_certified_mle(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=NAN,double fixed_prob=NAN,double fixed_pi=NAN) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(!std::isnan(fixed_size))return zinb_fixed_size_mle(x,fixed_size,fixed_prob,fixed_pi,tol,maxit);
  if(!std::isnan(fixed_prob))return zinb_fixed_prob_mle(x,fixed_prob,fixed_pi,tol,maxit);
  auto out=zinb_fixed_size_mle(x,1,NAN,fixed_pi,tol,maxit);
  if(out.fit.status==ZinbStatus::InvalidInput || out.failure==ZinbCertificateFailure::Workload)return out;
  zinb_global_detail::Data d;d.pi=fixed_pi;d.n=double(x.size());
  std::vector<int> positive;
  for(int v:x){++d.counts[v];d.S+=v;d.k+=v>0;d.M=std::max(d.M,v);if(v>0)positive.push_back(v);}
  if(d.k==0){out.fit=zinb_candidate_mle(x,tol,maxit,NAN,NAN,fixed_pi);return out;}
  auto fail=[&](ZinbCertificateFailure reason) {
    out.fit.status=ZinbStatus::IterationFailure;out.failure=reason;
    out.stop_detail.budget_exhausted=reason==ZinbCertificateFailure::Budget;
    out.stop_detail.comparison_tie=reason==ZinbCertificateFailure::ComparisonTie;
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  d.n0=d.n-d.k;d.z=d.n0/d.n;
  zinb_global_detail::Sum initial_factorial;
  for(const auto& kv:d.counts)initial_factorial.add(-kv.second*special::lgamma(kv.first+1.));
  d.factorial=initial_factorial.value;
  // With no zeros, inflation is either held or maximized at zero. With
  // inflation held at zero, the entire likelihood is ordinary NB.
  if(d.n0==0 || fixed_pi==0 || (std::isnan(fixed_pi) && d.z<std::exp(-d.S/d.n)-d.pad(d.z+std::exp(-d.S/d.n)))) {
    const auto nb=nbinom_mle(x,tol,maxit);out.failure=ZinbCertificateFailure::None;
    const double pi=std::isnan(fixed_pi)?0:fixed_pi;
    out.stop_detail=nb.stop_detail;
    if(nb.status==NbinomStatus::Ok) {
      auto base=d;base.pi=pi;
      const double ll=base.at(std::log(nb.size),std::log(-nb.size*std::log(nb.prob))).ll;
      if(!std::isfinite(ll))return fail(ZinbCertificateFailure::Arithmetic);
      out.fit={nb.size,nb.prob,pi,ll,nb.iterations,ZinbStatus::Ok};return out;
    }
    if(nb.status==NbinomStatus::Underdispersed) {
      const double mean=d.S/d.n;
      auto base=d;base.pi=pi;const auto evaluated=base.zip_at(std::log(mean));const double ll=evaluated.ll;
      out.fit={NAN,NAN,NAN,NAN,nb.iterations,pi>0?ZinbStatus::ZipLimit:ZinbStatus::PoissonLimit};
      out.stop_detail=limit_detail(pi>0?std::vector<double>{mean,pi}:std::vector<double>{mean},ll,pi>0?StopModel::Zip:StopModel::Pois);
      out.stop_detail.loglik_lower=ll-evaluated.error;out.stop_detail.loglik_upper=ll+evaluated.error;return out;
    }
    out.fit.iterations=nb.iterations;
    return fail(nb.certificate_failure==NbinomCertificateFailure::Budget?ZinbCertificateFailure::Budget:ZinbCertificateFailure::Arithmetic);
  }
  out=zinb_joint_certificate(x,fixed_pi,tol,maxit);
  if(out.fit.status==ZinbStatus::Ok)return out;
  auto tail=zinb_zip_certificate(x,fixed_pi,tol,maxit);
  const int combined_work=out.fit.iterations+tail.fit.iterations;
  out.fit.iterations=combined_work;tail.fit.iterations=combined_work;
  if(tail.fit.status==ZinbStatus::ZipLimit || tail.fit.status==ZinbStatus::PoissonLimit)return tail;
  if(!std::isnan(fixed_pi))return out.failure==ZinbCertificateFailure::Budget?out:tail;
  // A certified truncated-NB profile with no feasible finite stationary
  // maximum leaves only the ordinary-NB stationary point and the ZIP face.
  const auto zt=ztnbinom_mle({positive.data(),positive.size()},tol,maxit);
  out.fit.iterations+=zt.iterations;
  bool excluded=zt.status==ZtnbinomStatus::PoissonLimit;
  if(zt.status==ZtnbinomStatus::Ok) {
    if(zt.size==0 || zt.prob==1)excluded=true;
    else {
      const double f0=std::exp(zt.size*std::log(zt.prob));
      if(f0>d.z+d.pad(d.z+f0))excluded=true;
      else if(f0<d.z-d.pad(d.z+f0)) {
        auto rescue=zinb_joint_certificate(x,NAN,tol,maxit,zt.size,zt.prob);
        rescue.fit.iterations+=out.fit.iterations;
        return rescue; // The full rectangle certificate remains authoritative.
      } else return fail(ZinbCertificateFailure::ComparisonTie);
    }
  }
  if(!excluded) {
    if(zt.certificate_failure==ZtnbinomCertificateFailure::Budget)return fail(ZinbCertificateFailure::Budget);
    return fail(ZinbCertificateFailure::Arithmetic);
  }
  d.pi=NAN;
  zinb_global_detail::Sum factorial_sum;
  for(const auto& kv:d.counts)factorial_sum.add(-kv.second*special::lgamma(kv.first+1.));
  d.factorial=factorial_sum.value;
  const auto face=zinb_global_detail::zip_upper(d,maxit);
  if(!face.ok)return fail(ZinbCertificateFailure::Arithmetic);
  out.stop_detail={};
  out.stop_detail.candidates.push_back({{face.lambda,face.pi},face.lower,face.upper,StopModel::Zip});
  const auto nb=nbinom_mle(x,tol,maxit);
  out.fit.iterations+=nb.iterations;const int total_work=out.fit.iterations;
  double nb_lower=-INFINITY,nb_upper=-INFINITY;
  if(nb.status==NbinomStatus::Ok && nb.size>0 && nb.prob>0 && nb.prob<1) {
    auto base=d;base.pi=0;
    const auto p=base.at(std::log(nb.size),std::log(-nb.size*std::log(nb.prob)));
    zinb_global_detail::Sum harmonic;
    for(const auto& kv:d.counts)for(int j=0;j<kv.first;++j)harmonic.add(kv.second/(nb.size+j));
    const double score=harmonic.value+d.n*std::log(nb.prob),dr=2e-6*nb.size,rl=nb.size-dr;
    const double error=p.error+(std::abs(score)+d.pad(harmonic.value+d.n*std::abs(std::log(nb.prob))))*dr+
      .5*(d.S/(rl*rl)+d.n/rl)*dr*dr;
    nb_lower=p.ll-p.error;nb_upper=p.ll+error;
    out.stop_detail.candidates.push_back({{nb.size,nb.prob,0},nb_lower,nb_upper,StopModel::None});
  } else if(nb.status!=NbinomStatus::Underdispersed) {
    return fail(nb.certificate_failure==NbinomCertificateFailure::Budget?ZinbCertificateFailure::Budget:ZinbCertificateFailure::Arithmetic);
  }
  out.stop_detail.loglik_lower=std::max(nb_lower,face.lower);
  const double margin=d.pad(std::abs(face.ll)+(std::isfinite(nb_lower)?std::abs(nb_lower):0));
  if(nb_lower>face.upper+margin) {
    auto base=d;base.pi=0;
    out.fit={nb.size,nb.prob,0,base.at(std::log(nb.size),std::log(-nb.size*std::log(nb.prob))).ll,total_work,ZinbStatus::Ok};out.failure=ZinbCertificateFailure::None;
    out.stop_detail.loglik_upper=nb_upper;return out;
  }
  if(face.lower>nb_upper+margin) {
    out.fit={NAN,NAN,NAN,NAN,total_work,face.pi>0?ZinbStatus::ZipLimit:ZinbStatus::PoissonLimit};
    out.failure=ZinbCertificateFailure::None;
    out.stop_detail.limit_coef=face.pi>0?std::vector<double>{face.lambda,face.pi}:std::vector<double>{face.lambda};
    out.stop_detail.limit_model=face.pi>0?StopModel::Zip:StopModel::Pois;
    out.stop_detail.limit_loglik=face.ll;
    out.stop_detail.loglik_lower=face.lower;out.stop_detail.loglik_upper=face.upper;return out;
  }
  return fail(ZinbCertificateFailure::ComparisonTie);
}
inline ZinbResult zinb_mle(ispan x,double tol,int maxit,
    double fixed_size,double fixed_prob,double fixed_pi) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto result=zinb_certified_mle(x,tol,maxit,fixed_size,fixed_prob,fixed_pi);
  result.fit.stop_detail=result.stop_detail;
  result.fit.certificate_failure=result.failure;
  return result.fit;
}
} // namespace cppunivariate
