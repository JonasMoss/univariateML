#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "../../core/transformed_data.hpp"
#include <cstdint>
#include "dpqr.hpp"
#include <vector>
#include <limits>
namespace cppunivariate {
enum class FoldnormStatus { Ok, InvalidInput, Unbounded, IterationFailure, AccuracyFailure };
struct FoldnormResult { double mu,sigma,logLik; int iterations; FoldnormStatus status; StopDetail stop_detail{}; bool singleton_proved = false; bool certified = true; };
namespace foldnorm_detail {
// Exact equality for a bounded integer chart. A failed conversion leaves
// the quartic comparison to the guarded floating-point moment test.
inline bool quartic_equality(span x,double scale) {
  if(x.size()>32768)return false;
  const int shift=7-std::ilogb(scale);
  std::uint64_t second=0,fourth=0;
  for(double value:x) {
    const double integer=std::scalbn(value,shift);
    if(integer!=std::floor(integer) || std::scalbn(integer,-shift)!=value)return false;
    const auto u=static_cast<std::uint64_t>(integer);
    const auto square=u*u;
    second+=square;fourth+=square*square;
  }
  return x.size()*fourth==3*second*second;
}

inline double held_variance(span y,double mu,double residual,double upper,
                            double tol,int maxit,int& iterations) {
  // T'(v) >= 1 - 1/(1/2+1/sqrt(3)) > .07. The absolute
  // residual therefore gives a variance enclosure even at a rounded zero.
  constexpr double slope_lower=.07;
  double lo=residual,hi=std::max(residual,upper);
  double v=lo;
  for(int i=0;i<maxit;++i) {
    ++iterations;
    transformed_detail::Sum terms;
    for(double x:y) {
      const double product=2*mu*x;
      const double e=std::exp(-product/v);
      terms.push(2*product*e/(1+e));
    }
    const double correction=terms.get()/y.size();
    const double t=(v-residual)-correction;
    const double error=32*std::numeric_limits<double>::epsilon()*
      (v+residual+correction);
    if(std::abs(t)+error<.25*tol*v*slope_lower)return v;
    if(t< -error)lo=v;
    else if(t>error)hi=v;
    else break;
    const double next=std::sqrt(lo)*std::sqrt(hi);
    if(!(next>lo && next<hi))break;
    v=next;
  }
  return NAN;
}

// Search every sampled local maximum, including both endpoints. Log spacing
// resolves the weakly identified half-normal face as well as narrow normal fits.
template <typename Function>
inline double maximize(Function f,double lo,double hi,double tol,int maxit,int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  const int grids=static_cast<int>(std::min<std::int64_t>(512,certificate_budget(maxit)-1));
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
  out.singleton_proved=fs || fm;
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
  std::vector<double> y;
  y.reserve(x.size());
  transformed_detail::Sum first,second;
  for(double z:x) {
    const double v=z/scale;
    y.push_back(v);
    first.push(v);second.push(v*v);
  }
  const double m=first.get()/x.size(),m2=second.get()/x.size();
  double variance=0;
  for (double z:y) variance+=(z-m)*(z-m)/y.size();
  const double mu0=fm ? fixed_mu/scale : 0, s0=fs ? fixed_sigma/scale : 0;
  double residual=0;
  if(fm && !fs) {
    transformed_detail::Sum squares;
    for(double v:x) {
      const double d=(v-fixed_mu)/scale;
      squares.push(d*d);
    }
    residual=squares.get()/x.size();
  }
  if (!fs && (fm ? residual==0 : variance==0)) {out.status=FoldnormStatus::Unbounded;return out;}
  const auto ll=[&](double mu,double s) { double value=0; for (double z:y) value+=distributions::dfoldnorm(z,mu,s,true); return value; };
  double mu=mu0,s=s0;
  bool half_proved=false,positive_proved=false;
  if(!fm && !fs) {
    transformed_detail::Sum fourth,sixth;
    for(double z:y) {
      const double z2=z*z;
      fourth.push(z2*z2);
      sixth.push(z2*z2*z2);
    }
    const double A=(fourth.get()/y.size())/(m2*m2);
    const double B=(sixth.get()/y.size())/(m2*m2*m2);
    const double M2=m*m/m2;
    const double Q=3*(1-M2)*(1-M2)+1.2*(B/A)*M2;
    const double pad=256*std::numeric_limits<double>::epsilon()*(1+y.size()*
      std::numeric_limits<double>::epsilon());
    const double quartic_pad=pad*(1+A);
    const bool equality=std::abs(A-3)<=quartic_pad && foldnorm_detail::quartic_equality(x,scale);
    double atom=0;
    std::size_t positives=0;
    bool two_atoms=true;
    for(double v:x)if(v>0) {
      if(atom==0)atom=v;
      two_atoms=two_atoms && v==atom;
      ++positives;
    }
    half_proved=(two_atoms && positives<=x.size()/3) ||
      ((A>3+quartic_pad || equality) && A>Q+pad*(1+A+Q));
    positive_proved=A<3-quartic_pad || (equality && B>15+pad*(1+B)) ||
      M2>.75+pad;
    out.certified=half_proved;
    out.singleton_proved=half_proved;
  }
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
      s=std::sqrt(foldnorm_detail::held_variance({y.data(),y.size()},mu,residual,
        m2+mu*mu,tol,maxit,out.iterations));
      if(!std::isfinite(s)) {out.status=FoldnormStatus::IterationFailure;return out;}
    }
  } else if (!fs) {
    s=std::sqrt(m2); mu=0;
    if(!half_proved) {
      // a=mu/sigma^2. Exact maximizer in sigma at fixed a.
      const double amax=m/variance;
      const auto sigma2=[&](double a){return 2*m2/(1+std::hypot(1.0,2*std::sqrt(m2)*a));};
      const auto profile=[&](double t) { const double a=std::expm1(t),v=sigma2(a);return ll(a*v,std::sqrt(v)); };
      double t=foldnorm_detail::maximize(profile,0,std::log1p(amax),tol,maxit,out.iterations);
      double best=profile(t);
      // Refine the grid incumbent using per-observation likelihood bounds.
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
      const double a=std::expm1(t),v=sigma2(a);
      const double half_value=ll(0,s);
      const double comparison_pad=128*std::numeric_limits<double>::epsilon()*
        (y.size()+std::abs(best)+std::abs(half_value));
      if(a>0 && best>half_value+comparison_pad) positive_proved=true;
      if(a>0 && best>half_value+std::min(1e-11*y.size(),comparison_pad)) {
        mu=a*v;
        s=std::sqrt(v);
      }
    }
  }
  tol=std::min(tol,1e-10);
  // The global search chooses the basin; solve its analytic score rather
  // than comparing likelihood values at nearly identical coordinates.
  bool face_polish=false;
  if(!fm && !fs && !half_proved && (mu==0 || mu<.1*s)) {
    const double h0=foldnorm_detail::face_score({y.data(),y.size()},m2,0,0,0).value;
    const double margin=32*std::numeric_limits<double>::epsilon();
    if(mu==0 && (std::abs(h0)<=margin || (positive_proved && h0<=margin))) {
      // The incumbent is either refuted by a separate sign test or lies
      // at an unresolved quartic comparison. Neither can justify mu=0.
      {out.status=FoldnormStatus::AccuracyFailure;out.mu=mu*scale;out.sigma=s*scale;out.logLik=ll(mu,s)-x.size()*std::log(scale);{if(out.sigma>0)record_accuracy(out,tol,{out.mu,out.sigma},{out.sigma,out.sigma},std::sqrt(margin));else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}}return out;
    }
    if(h0>margin || mu>0) {
      // Retain the global likelihood bound. A boundary with H(0)>0 is
      // a local minimum; a positive incumbent keeps its own local bracket.
      double zmax=0;for(double z:y)zmax=std::max(zmax,z*z/m2);
      const double limit=std::min(m*m/m2,.0625/zmax),incumbent=mu*mu/m2;
      double lo=mu>0?.5*incumbent:0,hi=mu>0?std::min(limit,1.5*incumbent):limit;
      const auto score=[&](double b){return foldnorm_detail::face_score({y.data(),y.size()},m2,b,b,b).value;};
      const bool bracketed=lo<hi && score(lo)>margin && score(hi)<-margin;
      if(!bracketed && mu==0) {{out.status=FoldnormStatus::AccuracyFailure;out.mu=fm?fixed_mu:mu*scale;out.sigma=fs?fixed_sigma:s*scale;out.logLik=ll(mu,s)-x.size()*std::log(scale);{if(out.sigma>0 && out.mu>=0)record_accuracy(out,tol,{out.mu,out.sigma},{fm?0:std::max(std::abs(out.mu),out.sigma),fs?0:out.sigma});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}}return out;}
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
        if(!accurate) {{out.status=FoldnormStatus::AccuracyFailure;out.mu=fm?fixed_mu:mu*scale;out.sigma=fs?fixed_sigma:s*scale;out.logLik=ll(mu,s)-x.size()*std::log(scale);{if(out.sigma>0 && out.mu>=0)record_accuracy(out,tol,{out.mu,out.sigma},{fm?0:std::max(std::abs(out.mu),out.sigma),fs?0:out.sigma});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}}return out;}
      }
    }
  }
  const bool normal_polish=!fs && !fm && mu>0 && std::isfinite(normal_sd);
  if(normal_polish) {mu=normal_mu/scale;s=normal_sd/scale;}
  else if(!fs && !fm && mu>0 && !face_polish) {
    double a=mu/(s*s),previous=INFINITY;
    bool accurate=false;
    for(int k=0;k<maxit;++k) {
      const double v=2*m2/(1+std::hypot(1.0,2*std::sqrt(m2)*a));
      mu=a*v;s=std::sqrt(v);
      const double derivative=v/(1+2*a*a*v);
      const double dm=derivative/mu,ds=-a*derivative;
      double g=m-mu,h=-derivative;
      for(double z:y) {
        const double q=std::exp(-2*a*z),w=q/(1+q);
        g-=2*z*w/y.size();h+=4*z*z*w*(1-w)/y.size();
      }
      const double step=g/h;
      const double error=std::abs(step)*std::max(std::abs(dm),std::abs(ds));
      if(!(h<0 && std::isfinite(step)) || !(a-step>0))break;
      if(error<=.01*tol && ((std::isfinite(previous) && error<=.25*previous) || error<64*std::numeric_limits<double>::epsilon())) {accurate=true;break;}
      previous=error;a-=step;
    }
    if(!accurate) {out.status=FoldnormStatus::IterationFailure;return out;}
  }
  out.mu=fm ? fixed_mu : mu*scale;out.sigma=fs ? fixed_sigma : s*scale;
  out.logLik=ll(mu,s)-x.size()*std::log(scale);
  if(normal_polish) {
    out.mu=normal_mu;out.sigma=normal_sd;out.logLik=0;
    for(double z:x)out.logLik+=distributions::dfoldnorm(z,normal_mu,normal_sd,true);
  }
  if(fm && !fs && out.sigma>0 && std::isfinite(out.sigma)) {
    transformed_detail::Sum value;
    for(double z:x)value.push(distributions::dfoldnorm(z,out.mu,out.sigma,true));
    out.logLik=value.get();
  }
  if(!(out.sigma>0 && std::isfinite(out.sigma)) ||
     (!fs && std::abs(out.sigma/scale-s)>.25*tol*s) ||
     (!fm && std::abs(out.mu/scale-mu)>.25*tol*std::max(std::abs(mu),s))) {
    {out.status=FoldnormStatus::AccuracyFailure;{if(out.sigma>0 && out.mu>=0)record_accuracy(out,tol,{out.mu,out.sigma},{fm?0:std::max(std::abs(out.mu),out.sigma),fs?0:out.sigma});else{out.stop_detail.accuracy_stop=AccuracyStop::None;out.stop_detail.attainable_tol=NAN;}}}return out;
  }
  out.status=!fm && out.iterations>=maxit ? FoldnormStatus::IterationFailure : FoldnormStatus::Ok;
  return out;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const FoldnormResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.stop_detail.budget_exhausted) return FitOutcome::budget();
  switch (r.status) {
    case FoldnormStatus::Ok: return r.singleton_proved ? FitOutcome::singleton(r.certified) : FitOutcome::unknown_cardinality(r.certified);
    case FoldnormStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case FoldnormStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case FoldnormStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case FoldnormStatus::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Tolerance);
  }
  return {};
}
} // namespace cppunivariate
