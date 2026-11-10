#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "dpqr.hpp"
#include <vector>
#include <limits>
namespace cppunivariate {
enum class FoldnormStatus { Ok, InvalidInput, Unbounded, IterationFailure, AccuracyFailure };
struct FoldnormResult { double mu,sigma,logLik; int iterations; FoldnormStatus status; StopDetail stop_detail{}; };
namespace foldnorm_detail {
// Search every sampled local maximum, including both endpoints. Log spacing
// resolves the weakly identified half-normal face as well as narrow normal fits.
template <typename Function>
inline double maximize(Function f,double lo,double hi,double tol,int maxit,int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  const int grids=512;
  std::vector<double> u(grids+1),v(grids+1);
  double best=lo,bestv=f(lo);
  for (int i=0;i<=grids;++i) {
    u[i]=lo+(hi-lo)*i/grids; v[i]=f(u[i]);
    if (v[i]>bestv) { best=u[i]; bestv=v[i]; }
  }
  for (int i=1;i<grids;++i) if (v[i]>=v[i-1] && v[i]>=v[i+1]) {
    double a=u[i-1],b=u[i+1],c=b-(b-a)*.6180339887498949,d=a+(b-a)*.6180339887498949;
    double fc=f(c),fd=f(d);
    int k=0;
    for (;k<maxit && b-a>tol;++k) {
      if (fc>fd) { b=d; d=c; fd=fc; c=b-(b-a)*.6180339887498949; fc=f(c); }
      else { a=c; c=d; fc=fd; d=a+(b-a)*.6180339887498949; fd=f(d); }
    }
    iterations=std::max(iterations,k);
    if (fc>bestv) {best=c;bestv=fc;} if (fd>bestv) {best=d;bestv=fd;}
  }
  return best;
}
// For a joint fit near the half-normal face put b=mu^2/mean(x^2).
// The profile-score sign is that of H(b) below. Unlike the ordinary score,
// this expansion does not subtract two terms of order mu to recover mu^3.
struct FaceScore { double value,derivative_upper; };
inline FaceScore face_score(span y,double m2,double b,double lower,double upper) {
  // tanh(t)=sum a_j t^(2j+1), from tanh'=1-tanh^2.
  CPPUNIV_EXTENDED_REAL a[42]{};a[0]=1;
  for(int j=1;j<42;++j) {
    CPPUNIV_EXTENDED_REAL sum=0;
    for(int k=0;k<j;++k)sum+=a[k]*a[j-1-k];
    a[j]=-sum/(2*j+1);
  }
  CPPUNIV_EXTENDED_REAL fourth=0,correction=0,derivative=0;
  for(double z:y) {
    const CPPUNIV_EXTENDED_REAL z2=static_cast<CPPUNIV_EXTENDED_REAL>(z)*z/m2,z4=z2*z2;
    const CPPUNIV_EXTENDED_REAL u=b*z2/((1-b)*(1-b));
    const CPPUNIV_EXTENDED_REAL ul=lower*z2/((1-lower)*(1-lower));
    const CPPUNIV_EXTENDED_REAL uh=upper*z2/((1-upper)*(1-upper));
    CPPUNIV_EXTENDED_REAL c=0,du=0;
    for(int k=40;k>=1;--k)c=(c+a[k+1])*u;
    CPPUNIV_EXTENDED_REAL lp=1,hp=1;
    for(int k=1;k<=40;++k) {
      const CPPUNIV_EXTENDED_REAL coefficient=k*a[k+1];
      du+=coefficient*(coefficient>0?hp:lp);lp*=ul;hp*=uh;
    }
    // |tanh(t)|<3 on |t|<=1; Cauchy's estimate gives |a_j|<=3.
    // The derivative tail of c(u) after degree 40 is bounded by this sum.
    du+=3*hp*(41-40*uh)/((1-uh)*(1-uh));
    fourth+=z4/y.size();correction+=z4*c/y.size();
    derivative+=z4*z2*du/y.size();
  }
  const CPPUNIV_EXTENDED_REAL h=(1-fourth/3)-2*b+b*b+correction;
  const CPPUNIV_EXTENDED_REAL dh=-2*(1-upper)+
    derivative*(1+upper)/((1-upper)*(1-upper)*(1-upper));
  return {static_cast<double>(h),static_cast<double>(dh)};
}
}
inline FoldnormResult foldnorm_mle(span x,double tol=1e-10,int maxit=200,
    double fixed_mu=std::numeric_limits<double>::quiet_NaN(),
    double fixed_sigma=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  FoldnormResult out{special::kNaN,special::kNaN,special::kNaN,0,FoldnormStatus::InvalidInput};
  const bool fm=!std::isnan(fixed_mu),fs=!std::isnan(fixed_sigma);
  if (x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0 ||
      (fm && (!std::isfinite(fixed_mu) || fixed_mu<0)) ||
      (fs && (!std::isfinite(fixed_sigma) || fixed_sigma<=0))) return out;
  double scale=0;bool constant=true;
  for (double z:x) {
    if (!std::isfinite(z) || z<0) return out;
    scale=std::max(scale,z);constant=constant && z==x[0];
  }
  if (!fs && constant && (!fm || x[0]==fixed_mu)) {out.status=FoldnormStatus::Unbounded;return out;}
  scale=std::max(scale,fm ? fixed_mu : 0.0);
  scale=std::max(scale,fs ? fixed_sigma : 0.0);
  if (scale==0) {out.status=FoldnormStatus::Unbounded;return out;}
  double normal_mu=NAN,normal_sd=NAN;
  // At a well-separated positive mode the reflected density and all its
  // score corrections are exponentially small. Keep original-data centered
  // residuals here; normalization can erase the narrow normal spread.
  if(!fm && !fs) {
    const double anchor=x[0];double offset=0,spread=0;
    for(double z:x)spread=std::max(spread,std::abs(z-anchor));
    for(double z:x)offset+=((z-anchor)/spread)/x.size();
    double ss=0,minimum=x[0];
    for(double z:x){const double d=(z-anchor)/spread-offset;ss+=d*d/x.size();minimum=std::min(minimum,z);}
    const double location=anchor+spread*offset,sd=spread*std::sqrt(ss);
    if(sd>0 && minimum/sd>40 && location/sd>40) {
      normal_mu=location;normal_sd=sd;
    }
  }
  std::vector<double> y; double m=0,m2=0;
  for (double z:x) { y.push_back(z/scale); m+=y.back()/x.size(); m2+=y.back()*y.back()/x.size(); }
  double variance=0;
  for (double z:y) variance+=(z-m)*(z-m)/y.size();
  const double mu0=fm ? fixed_mu/scale : 0, s0=fs ? fixed_sigma/scale : 0;
  double residual=0;
  for (double z:y) residual+=(z-mu0)*(z-mu0)/y.size();
  if (!fs && (fm ? residual==0 : variance==0)) {out.status=FoldnormStatus::Unbounded;return out;}
  const auto ll=[&](double mu,double s) { double value=0; for (double z:y) value+=distributions::dfoldnorm(z,mu,s,true); return value; };
  double mu=mu0,s=s0;
  if (fs && !fm) {
    // tanh(c*mu)/mu strictly decreases: at most one positive score root.
    if (m2>s*s) {
      double lo=0,hi=m;
      for (int k=0;k<maxit;++k) {
        const double mid=(lo+hi)*.5; double score=-mid;
        for (double z:y) score+=z*std::tanh(mid*z/(s*s))/y.size();
        if (score>0) lo=mid; else hi=mid;
        out.iterations=k+1;
        if (hi-lo<tol*std::max(hi,1e-10)) break;
      }
      mu=(lo+hi)*.5;
    }
  } else if (!fs && fm) {
    if (mu==0) s=std::sqrt(m2);
    else {
      const double lo=.5*std::log(residual),hi=.5*std::log(m2+mu*mu);
      s=std::exp(foldnorm_detail::maximize([&](double t){return ll(mu,std::exp(t));},lo,hi,tol,maxit,out.iterations));
      // At fixed positive mu, a=mu/sigma^2 has a concave log(a) term
      // plus the same convex correction, so chord bounds apply here too.
      const auto correction=[&](double a) {double value=0;for (double z:y) value+=std::log1p(std::exp(-2*a*z));return value;};
      struct Interval {double lo,hi;};
      std::vector<Interval> pending{{mu/(m2+mu*mu),mu/residual}};
      double best=ll(mu,s);int visited=0;
      while (!pending.empty() && visited<certificate_budget(maxit)) {
        const Interval iv=pending.back();pending.pop_back();++visited;
        if (iv.lo==iv.hi) continue;
        const double al=correction(iv.lo),ar=correction(iv.hi);
        const double slope=(ar-al)/(iv.hi-iv.lo);
        const double candidate=std::max(iv.lo,std::min(iv.hi,y.size()/(y.size()*residual/mu-2*slope)));
        const double candidate_s=std::sqrt(mu/candidate),value=ll(mu,candidate_s);
        if (value>best) {best=value;s=candidate_s;}
        const double weight=(candidate-iv.lo)/(iv.hi-iv.lo);
        const double upper=value+(1-weight)*al+weight*ar-correction(candidate);
        if (upper<=best+tol*y.size()) continue;
        const double mid=std::sqrt(iv.lo)*std::sqrt(iv.hi);
        if (mid<=iv.lo || mid>=iv.hi) continue;
        pending.push_back({iv.lo,mid});pending.push_back({mid,iv.hi});
      }
      if (!pending.empty()) {
        out.status=FoldnormStatus::IterationFailure;
        out.stop_detail=discrete_budget(out.stop_detail);
        const double witness_ll=ll(mu,s)-x.size()*std::log(scale);
        out.stop_detail.candidates.push_back({{mu*scale,s*scale},witness_ll,witness_ll});
        out.stop_detail.loglik_lower=witness_ll;return out;
      }
    }
  } else if (!fs) {
    s=std::sqrt(m2); mu=0;
    // a=mu/sigma^2. Exact maximizer in sigma at fixed a.
    const double amax=m/variance;
    const auto sigma2=[&](double a){return 2*m2/(1+std::hypot(1.0,2*std::sqrt(m2)*a));};
    const auto profile=[&](double t) { const double a=std::expm1(t),v=sigma2(a);return ll(a*v,std::sqrt(v)); };
    double t=foldnorm_detail::maximize(profile,0,std::log1p(amax),tol,maxit,out.iterations);
    double best=profile(t);
    // Certify the grid incumbent to a per-observation likelihood tolerance.
    // A(a)=sum log(1+exp(-2*a*x)) is convex. Replace A by its chord:
    // the remaining profile is strictly concave, with derivative n*(mean-mu(a)).
    // Its maximum under the chord occurs at mu=mean+chord_slope/n.
    const auto correction=[&](double a) {
      double value=0;for (double z:y) value+=std::log1p(std::exp(-2*a*z));return value;
    };
    struct Interval { double lo,hi; int depth; };
    std::vector<Interval> pending{{0,amax,0}};
    int visited=0;
    while (!pending.empty() && visited<certificate_budget(maxit)) {
      const Interval iv=pending.back();pending.pop_back();++visited;
      const double al=correction(iv.lo),ar=correction(iv.hi);
      const double target=std::max(0.0,std::min(m,m+(ar-al)/((iv.hi-iv.lo)*y.size())));
      const double denominator=(m2-target*target);
      const double candidate=std::max(iv.lo,std::min(iv.hi,target/denominator));
      const double value=profile(std::log1p(candidate));
      if (value>best) {best=value;t=std::log1p(candidate);}
      const double weight=(candidate-iv.lo)/(iv.hi-iv.lo);
      const double upper=value+(1-weight)*al+weight*ar-correction(candidate);
      if (upper<=best+tol*y.size()) continue;
      const double mid=std::expm1((std::log1p(iv.lo)+std::log1p(iv.hi))*.5);
      if (mid<=iv.lo || mid>=iv.hi) continue;
      pending.push_back({iv.lo,mid,iv.depth+1});pending.push_back({mid,iv.hi,iv.depth+1});
    }
    if (!pending.empty()) {
        out.status=FoldnormStatus::IterationFailure;
        out.stop_detail=discrete_budget(out.stop_detail);
        const double witness_ll=ll(mu,s)-x.size()*std::log(scale);
        out.stop_detail.candidates.push_back({{mu*scale,s*scale},witness_ll,witness_ll});
        out.stop_detail.loglik_lower=witness_ll;return out;
      }
    const double a=std::expm1(t),v=sigma2(a);
    if (best>ll(0,s)+1e-11*y.size()) { mu=a*v;s=std::sqrt(v); }
  }
  tol=std::min(tol,1e-10);
  // The global search chooses the basin; solve its analytic score rather
  // than comparing likelihood values at nearly identical coordinates.
  bool face_polish=false;
  if(!fm && !fs && (mu==0 || mu<.1*s)) {
    const double h0=foldnorm_detail::face_score({y.data(),y.size()},m2,0,0,0).value;
    const double margin=32*std::numeric_limits<double>::epsilon();
    if(mu==0 && std::abs(h0)<=margin) {
      // Quartic identification is unresolved at the floating-point moment
      // margin. A zero location can then miss a positive root by sqrt(eps).
      // Exception: the {0,c} sample with at most one third positive mass
      // has an exact half-normal maximum (see the dossier).
      double atom=0;std::size_t positives=0;bool two_atoms=true;
      for(double z:y)if(z>0) {
        if(atom==0)atom=z;
        two_atoms=two_atoms && z==atom;++positives;
      }
      if(!(two_atoms && positives<=y.size()/3)) {
        out.status=FoldnormStatus::AccuracyFailure;return out;
      }
    }
    if(h0>margin || mu>0) {
      // Retain the global likelihood bound. A boundary with H(0)>0 is
      // a local minimum; a positive incumbent keeps its own local bracket.
      double zmax=0;for(double z:y)zmax=std::max(zmax,z*z/m2);
      const double limit=std::min(m*m/m2,.0625/zmax),incumbent=mu*mu/m2;
      double lo=mu>0?.5*incumbent:0,hi=mu>0?std::min(limit,1.5*incumbent):limit;
      const auto score=[&](double b){return foldnorm_detail::face_score({y.data(),y.size()},m2,b,b,b).value;};
      const bool bracketed=lo<hi && score(lo)>margin && score(hi)<-margin;
      if(!bracketed && mu==0) {out.status=FoldnormStatus::AccuracyFailure;return out;}
      if(bracketed) {
        bool accurate=false;
        for(int k=0;k<maxit;++k) {
          const double mid=lo+(hi-lo)/2,h=score(mid);
          if(h>0)lo=mid;else hi=mid;
          const double ml=std::sqrt(m2*lo),mh=std::sqrt(m2*hi);
          const double sl=std::sqrt(m2*(1-hi)),sh=std::sqrt(m2*(1-lo));
          if(mh-ml<=.25*tol*std::max(ml,sl) && sh-sl<=.25*tol*sl &&
             score(lo)>margin && score(hi)<-margin &&
             foldnorm_detail::face_score({y.data(),y.size()},m2,mid,lo,hi).derivative_upper<0) {
            const double hl=score(lo),hh=score(hi),b=lo+(hi-lo)*hl/(hl-hh);
            mu=std::sqrt(m2*b);s=std::sqrt(m2*(1-b));accurate=true;face_polish=true;break;
          }
        }
        if(!accurate) {out.status=FoldnormStatus::AccuracyFailure;return out;}
      }
    }
  }
  const bool normal_polish=!fs && !fm && mu>0 && std::isfinite(normal_sd);
  if(normal_polish) {mu=normal_mu/scale;s=normal_sd/scale;}
  else if(!fs && mu>0 && !face_polish) {
    double a=mu/(s*s),previous=INFINITY;
    bool accurate=false;
    for(int k=0;k<maxit;++k) {
      double v=s*s,dm=0,ds=0,g=0,h=0;
      if(!fm) {
        v=2*m2/(1+std::hypot(1.0,2*std::sqrt(m2)*a));
        mu=a*v;s=std::sqrt(v);
        const double derivative=v/(1+2*a*a*v);
        dm=derivative/mu;ds=-a*v/(1+2*a*a*v);
        g=m-mu;h=-derivative;
        for(double z:y) {
          const double q=std::exp(-2*a*z),w=q/(1+q);
          g-=2*z*w/y.size();h+=4*z*z*w*(1-w)/y.size();
        }
      } else {
        g=.5/a-.5*(m2+mu*mu)/mu;h=-.5/(a*a);
        for(double z:y) {const double t=std::tanh(a*z);g+=z*t/y.size();h+=z*z*(1-t*t)/y.size();}
        ds=-.5/a;
      }
      const double step=g/h;
      const double error=std::abs(step)*std::max(std::abs(dm),std::abs(ds));
      if(!(h<0 && std::isfinite(step)) || !(a-step>0))break;
      if(error<=.01*tol && ((std::isfinite(previous) && error<=.25*previous) || error<64*std::numeric_limits<double>::epsilon())) {accurate=true;break;}
      previous=error;a-=step;
    }
    if(!accurate) {out.status=FoldnormStatus::IterationFailure;return out;}
    if(fm)s=std::sqrt(mu/a);
  }
  out.mu=fm ? fixed_mu : mu*scale;out.sigma=fs ? fixed_sigma : s*scale;
  out.logLik=ll(mu,s)-x.size()*std::log(scale);
  if(normal_polish) {
    out.mu=normal_mu;out.sigma=normal_sd;out.logLik=0;
    for(double z:x)out.logLik+=distributions::dfoldnorm(z,normal_mu,normal_sd,true);
  }
  if(!(out.sigma>0 && std::isfinite(out.sigma)) ||
     (!fs && std::abs(out.sigma/scale-s)>.25*tol*s) ||
     (!fm && std::abs(out.mu/scale-mu)>.25*tol*std::max(std::abs(mu),s))) {
    out.status=FoldnormStatus::AccuracyFailure;return out;
  }
  out.status=out.iterations>=maxit ? FoldnormStatus::IterationFailure : FoldnormStatus::Ok;
  return out;
}
}
