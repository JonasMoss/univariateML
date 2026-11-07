#pragma once
// Included inside cppunivariate, after the proposal fitter. No d/p/q/r changes.
namespace tlambda_cert_detail {
inline double margin(double value) {
  return 512*std::numeric_limits<double>::epsilon()*(1+std::abs(value));
}
inline bool contains(double lo,double hi,double mu,double sigma,double l) {
  return tlambda_detail::valid(mu,sigma,l) && (l<=0 ||
      (tlambda_detail::slack(lo,mu,sigma,l)>=0 && tlambda_detail::slack(hi,mu,sigma,l)>=0));
}
inline double width_slack(double lo,double hi,double sigma,double l) {
  if(!(l/2>0))return NAN;
  return special::support_residual(hi,lo,sigma,-l/2);
}
struct Inverse {
  double lo=NAN,hi=NAN; // bounds on -log of the smaller tail
  bool ok=false,solver_failure=false;
};
// Finite analytic brackets, theory/proofs.md section 1. Computed comparisons
// use the package's conditional exact-arithmetic standard (README.md).
inline Inverse inverse(double z,double l,int maxit,double logslack=NAN) {
  Inverse out;
  const double log2=std::log(2.0);
  if(!(z>=0) || !std::isfinite(z) || !std::isfinite(l))return out;
  if(z==0)return {log2,log2,true,false};
  if(l>0 && std::isnan(logslack) && z>=1/l) {
    if(z==1/l)return {INFINITY,INFINITY,true,false};
    return out;
  }
  if(l==1 || l==2) {
    const double logd=std::isnan(logslack)?std::log(std::fma(-l,z,1.0)):logslack;
    const double t=std::log(2.0)-logd;
    return {t,t,std::isfinite(t),false};
  }
  if(l==0) {
    const double t=z+std::log1p(std::exp(-z));
    return {t,t,std::isfinite(t),false};
  }
  double a,b,goal=std::log(z);
  const bool deficit=l>0 && l*z>.5;
  if(l<0) {
    const double c=-l,az=std::log(c)+goal;
    a=std::max(log2,tlambda_detail::logadd(az,0)/c);
    b=tlambda_detail::logadd(az,c*log2)/c;
  } else {
    const double d=std::fma(-l,z,1.0);
    if(std::isnan(logslack) && !(d>0))return out;
    const double logd=std::isnan(logslack)?std::log(d):logslack;
    if(deficit)goal=logd;
    if(l<1){a=log2;b=(log2-logd)/l;}
    else {a=log2;b=(std::log1p(l)-logd);}
  }
  if(!(std::isfinite(a) && std::isfinite(b) && a<=b))return out;
  // Pad the explicit bracket before evaluating conditional signs.
  a=std::max(log2,a-margin(a));b+=margin(b);
  double t=a+(b-a)/2;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    const double da=tlambda_detail::logderiv(-a,l),db=tlambda_detail::logderiv(-b,l);
    if(b-a<=2e-14*(1+b) && std::abs(da-db)<=2e-13*(1+std::max(std::abs(da),std::abs(db))))
      return {a,b,true,false};
    if(t==a || t==b)return out;
    const double f=deficit?tlambda_detail::logdeficit(-t,l):tlambda_detail::logmag(-t,l);
    if(!std::isfinite(f))return out;
    if(deficit ? f>goal : f<goal)a=t;else b=t;
    const double ld=tlambda_detail::logderiv(-t,l);
    const double slope=std::exp((deficit?std::log(l):0)-t+ld-f);
    double next=deficit?t+(f-goal)/slope:t-(f-goal)/slope;
    if(std::isfinite(next) && next>a && next<b) {
      const double ca=std::max(a,next-margin(next)),cb=std::min(b,next+margin(next));
      const double fa=deficit?tlambda_detail::logdeficit(-ca,l):tlambda_detail::logmag(-ca,l);
      const double fb=deficit?tlambda_detail::logdeficit(-cb,l):tlambda_detail::logmag(-cb,l);
      if(deficit ? (fa>=goal && fb<=goal) : (fa<=goal && fb>=goal)){a=ca;b=cb;}
    }
    // A Newton proposal never replaces an unchecked side of the bracket.
    if(!(next>a && next<b) || next==t || !std::isfinite(next))next=a+(b-a)/2;
    t=next;
  }
  out.lo=a;out.hi=b;out.solver_failure=true;return out;
}
struct Sum {
  double value=0,carry=0;
  void add(double v) {const double y=v-carry,t=value+y;carry=(t-value)-y;value=t;}
};
struct Value {
  double ll=-INFINITY,lower=-INFINITY,upper=-INFINITY;
  double ga=0,gb=0,ga_error=0,gb_error=0,haa=0,hab=0,hbb=0;
  bool ok=false,solver_failure=false;
};
inline Value value(span x,double a,double b,double l,int maxit,bool scores=false) {
  Value out;
  if(!(a>0) || !std::isfinite(a+b))return out;
  const double base=std::log(a);
  Sum objective,lower,upper;
  out.ga=x.size()/a;out.haa=-static_cast<double>(x.size())/(a*a);
  Sum gradient_a{out.ga},gradient_b,absolute_a{out.ga},absolute_b;
  double absolute_log=0;
  for(double v:x) {
    const double z=std::fma(a,v,b),az=std::abs(z);
    if(l>0 && az>=1/l) {
      if(az>1/l || l<1 || scores)return out;
      // Positive-density contacts are evaluated exactly on their stratum.
      const double ld=l==1?std::log(2.0):0;
      objective.add(base-ld);lower.add(base-ld);upper.add(base-ld);absolute_log+=std::abs(base)+std::abs(ld);
      continue;
    }
    if(!scores && l>1) {
      const double dz=margin(az);
      const auto low=inverse(std::max(0.,az-dz),l,maxit),high=inverse(std::min(1/l,az+dz),l,maxit);
      if(!low.ok || !high.ok){out.solver_failure=low.solver_failure || high.solver_failure;return out;}
      const double dl=tlambda_detail::logderiv(-low.lo,l),dh=tlambda_detail::logderiv(-high.hi,l);
      objective.add(base-(dl/2+dh/2));lower.add(base-std::max(dl,dh));upper.add(base-std::min(dl,dh));
      absolute_log+=std::abs(base)+std::max(std::abs(dl),std::abs(dh));continue;
    }
    const auto inv=inverse(az,l,maxit);
    if(!inv.ok){out.solver_failure=inv.solver_failure;return out;}
    const double dl=tlambda_detail::logderiv(-inv.lo,l),dh=tlambda_detail::logderiv(-inv.hi,l);
    const double t=inv.lo+(inv.hi-inv.lo)/2;
    const double ld=tlambda_detail::logderiv(-t,l);
    objective.add(base-ld);lower.add(base-std::max(dl,dh));upper.add(base-std::min(dl,dh));
    absolute_log+=std::abs(base)+std::max(std::abs(dl),std::abs(dh));
    if(scores) {
      const double p=std::exp(-t),q=-std::expm1(-t),lp=-t,lq=std::log(q),c=1-l;
      double s=0,h=0;
      if(l==0){s=-std::copysign(1-2*p,z);h=-2*p*q;}
      else {
        s=-std::copysign(c*std::exp((l-2)*lp-2*ld)*(-std::expm1((l-2)*(lq-lp))),z);
        // Positive curvature expansion avoids cancellation of DD''-2D'^2.
        h=-c*(l*(std::exp((-2*c-2)*lp-4*ld)+std::exp((-2*c-2)*lq-4*ld))+
            (c+1)*(std::exp(-c*lp+(-c-2)*lq-4*ld)+std::exp(-c*lq+(-c-2)*lp-4*ld))+
            4*c*std::exp((-c-1)*(lp+lq)-4*ld));
      }
      if(!std::isfinite(s+h))return out;
      auto magnitude=[&](double u) {
        if(l==0)return 1-2*std::exp(-u);
        const double qlog=std::log1p(-std::exp(-u));
        return c*std::exp((l-2)*(-u)-2*tlambda_detail::logderiv(-u,l))*(-std::expm1((l-2)*(qlog+u)));
      };
      const double sl=magnitude(inv.lo),sh=magnitude(inv.hi);
      const double error=std::max(std::abs(std::abs(s)-sl),std::abs(std::abs(s)-sh))+margin(std::max(sl,sh));
      out.ga_error+=std::abs(v)*error;out.gb_error+=error;
      gradient_a.add(v*s);gradient_b.add(s);absolute_a.add(std::abs(v*s));absolute_b.add(std::abs(s));
      out.haa+=v*v*h;out.hab+=v*h;out.hbb+=h;
    }
  }
  out.ga=gradient_a.value;out.gb=gradient_b.value;
  out.ga_error+=margin(absolute_a.value);out.gb_error+=margin(absolute_b.value);
  const double pad=margin(absolute_log+x.size())*(1+x.size()*std::numeric_limits<double>::epsilon());
  out.ll=objective.value;out.lower=lower.value-pad;out.upper=upper.value+pad;
  out.ok=std::isfinite(out.ll+out.lower+out.upper);return out;
}
// Point witnesses use the original affine support residual and the settled
// rounded-endpoint convention, avoiding a second endpoint after normalization.
inline Value point_value(span x,double mu,double sigma,double l,int maxit) {
  Value out;
  if(!(sigma>0) || !std::isfinite(mu) || !std::isfinite(sigma))return out;
  const double base=-std::log(sigma);
  Sum objective,lower,upper;
  double absolute_log=0;
  auto add=[&](double a,double b) {
    objective.add(base-(a/2+b/2));lower.add(base-std::max(a,b));upper.add(base-std::min(a,b));
    absolute_log+=std::abs(base)+std::max(std::abs(a),std::abs(b));
  };
  for(double v:x) {
    if(l>=1 && (v==mu-sigma/l || v==mu+sigma/l)) {
      const double d=l==1?std::log(2.0):0;add(d,d);continue;
    }
    const double gap=l>0?tlambda_detail::slack(v,mu,sigma,l):1;
    if(l>0 && gap<=0) {
      if(gap<0 || l<1)return out;
      const double d=l==1?std::log(2.0):0;add(d,d);continue;
    }
    if(l==1 || l==2) {
      const double d=l==1?std::log(2.0):0;add(d,d);continue;
    }
    const double z=std::exp(tlambda_detail::logstandard(v,mu,sigma));
    const auto inv=inverse(z,l,maxit,l>0?std::log(gap)-std::log(sigma):NAN);
    if(!inv.ok){out.solver_failure=inv.solver_failure;return out;}
    add(tlambda_detail::logderiv(-inv.lo,l),tlambda_detail::logderiv(-inv.hi,l));
  }
  const double pad=margin(absolute_log+x.size());
  out.ll=objective.value;out.lower=lower.value-pad;out.upper=upper.value+pad;
  out.ok=std::isfinite(out.ll+out.lower+out.upper);return out;
}
inline TlambdaResult stopped(TlambdaResult out,bool budget=false,bool tie=false) {
  out.status=TlambdaStatus::CertificateFailure;out.certified=false;
  out.stop_detail.budget_exhausted=budget;out.stop_detail.comparison_tie=tie;
  out.stop_detail.loglik_upper=INFINITY;
  if(std::isfinite(out.mu) && out.sigma>0 && std::isfinite(out.sigma) && std::isfinite(out.lambda) &&
      std::isfinite(out.certificate_lower) && std::isfinite(out.certificate_upper) && out.certificate_lower<=out.certificate_upper) {
    out.stop_detail.candidates.push_back({{out.mu,out.sigma,out.lambda},out.certificate_lower,out.certificate_upper});
    out.stop_detail.loglik_lower=out.certificate_lower;
  }
  return out;
}
inline TlambdaResult fixed_nonnegative(span x,double tol,int maxit,double l,double mu,double sigma) {
  const bool hm=std::isfinite(mu),hs=std::isfinite(sigma);
  TlambdaResult out{mu,sigma,l,NAN,0,TlambdaStatus::InvalidData};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0)return out;
  std::vector<double> raw(x.begin(),x.end());
  for(double v:raw)if(!std::isfinite(v))return out;
  std::sort(raw.begin(),raw.end());
  const double origin=hm?mu:raw.front()/2+raw.back()/2;
  double unit=std::max(std::abs(raw.front()-origin),std::abs(raw.back()-origin));
  if(hs)unit=std::max(unit,sigma);
  if(!std::isfinite(unit))return stopped(out);
  if(unit==0){out.status=TlambdaStatus::Unbounded;return out;}
  std::vector<double> y;for(double v:raw) {
    const double q=(v-origin)/unit;
    if(!std::isfinite(q) || (q==0 && v!=origin))return out;
    y.push_back(q);
  }
  for(std::size_t j=1;j<y.size();++j)if(raw[j]!=raw[j-1] && y[j]==y[j-1])return out;
  const span z{y.data(),y.size()};
  const double n=x.size(),lo=y.front(),hi=y.back(),radius=std::max(std::abs(lo),std::abs(hi));
  const double m=hm?0:lo/2+hi/2,s=hs?sigma/unit:NAN;
  auto recover=[&](double centre,double scale) {
    TlambdaResult r{hm?mu:origin+unit*centre,hs?sigma:unit*scale,l,NAN,0,TlambdaStatus::CertificateFailure};
    r.logLik=tlambda_detail::likelihood(x,r.mu,r.sigma,l);
    return r;
  };
  if(hm && hs) {
    out=recover(0,s);
    const auto v=point_value(x,mu,sigma,l,maxit);
    if(!v.ok) {
      if(l>0 && !std::isfinite(out.logLik)) {
        for(double v:x) {
          const bool alias=l>=1 && (v==mu-sigma/l || v==mu+sigma/l);
          if(!alias && (tlambda_detail::slack(v,mu,sigma,l)<0 || (l<1 && tlambda_detail::slack(v,mu,sigma,l)==0))){out.status=TlambdaStatus::InvalidSupport;return out;}
        }
      }
      return stopped(out);
    }
    if(!std::isfinite(out.logLik))return stopped(out);
    if(out.logLik<v.lower-margin(v.lower) || out.logLik>v.upper+margin(v.upper))return stopped(out);
    out.certified=true;out.status=TlambdaStatus::Ok;
    out.certificate_lower=v.lower;out.certificate_upper=v.upper;return out;
  }
  if(!hs && lo==hi && (!hm || lo==0)){out.status=TlambdaStatus::Unbounded;return out;}
  if(l==1 || l==2 || (l>1 && hm && !hs)) {
    const double r=std::max(std::abs(lo-m),std::abs(hi-m));
    double scale=hs?s:l*r;
    const double ideal_scale=scale;
    if(!(scale>0))return stopped(out);
    if(hs) {
      const double feasible=width_slack(raw.front(),raw.back(),sigma,l);
      if(!std::isfinite(feasible))return stopped(out);
      if(feasible<0){out.status=TlambdaStatus::InvalidSupport;return out;}
    }
    out=recover(m,scale);
    if(!hs) {
      const double rawradius=std::max(std::abs(raw.front()-out.mu),std::abs(raw.back()-out.mu));
      out.sigma=std::max(out.sigma,l*rawradius);
      for(std::int64_t j=0;j<solver_budget(maxit) && !contains(raw.front(),raw.back(),out.mu,out.sigma,l);++j)
        out.sigma=std::nextafter(out.sigma,INFINITY);
    }
    if(!contains(raw.front(),raw.back(),out.mu,out.sigma,l))return stopped(out);
    out.logLik=tlambda_detail::likelihood(x,out.mu,out.sigma,l);
    if(!std::isfinite(out.logLik))return stopped(out);
    if(l>1 && l!=2) {
      const auto ideal=value(z,1/ideal_scale,-m/ideal_scale,l,maxit);
      const auto actual=point_value(x,out.mu,out.sigma,l,maxit);
      if(!ideal.ok || !actual.ok)return stopped(out);
      out.certificate_lower=actual.lower;out.certificate_upper=std::max(actual.upper,ideal.upper-n*std::log(unit)+margin(n*std::log(unit)));
      if(out.certificate_upper-out.certificate_lower>n*tol)return stopped(out);
    } else {
      const auto actual=point_value(x,out.mu,out.sigma,l,maxit);
      if(!actual.ok)return stopped(out);
      const double ideal=hs?-n*(std::log(sigma)+std::log(2/l)):
          (hm?-n*(std::log(2.)+std::max(tlambda_detail::logstandard(raw.front(),mu,1),tlambda_detail::logstandard(raw.back(),mu,1))):
          -n*tlambda_detail::logstandard(raw.back(),raw.front(),1));
      out.certificate_lower=actual.lower;out.certificate_upper=std::max(actual.upper,ideal+margin(n+std::abs(ideal)));
      if(!std::isfinite(ideal) || out.certificate_upper-out.certificate_lower>n*tol)return stopped(out);
    }
    out.certified=true;out.status=TlambdaStatus::Ok;return out;
  }
  if(l>1) {
    // Held scale: convex log likelihood, hence only the two contacts compete.
    // Joint location-scale contacts are handled by the box routine below.
    if(!hs)return out;
    const double left=hi-s/l,right=lo+s/l;
    const double feasible=width_slack(raw.front(),raw.back(),sigma,l);
    if(!std::isfinite(feasible))return stopped(out);
    if(feasible<0){out.status=TlambdaStatus::InvalidSupport;return out;}
    if(left>right)return stopped(out);
    auto a=recover(left,s),b=recover(right,s);
    const double target=origin+unit*(left/2+right/2);
    const auto repair=[&](TlambdaResult r) {
      for(std::int64_t j=0;j<solver_budget(maxit) && !contains(raw.front(),raw.back(),r.mu,r.sigma,l);++j) {
        const double next=std::nextafter(r.mu,target);if(next==r.mu)break;r.mu=next;
      }
      if(!contains(raw.front(),raw.back(),r.mu,r.sigma,l))r.logLik=NAN;
      else r.logLik=tlambda_detail::likelihood(x,r.mu,r.sigma,l);
      return r;
    };
    a=repair(a);b=repair(b);
    if(!std::isfinite(a.logLik) || !std::isfinite(b.logLik))return stopped(a);
    auto va=point_value(x,a.mu,a.sigma,l,maxit),vb=point_value(x,b.mu,b.sigma,l,maxit);
    const auto ia=value(z,1/s,-left/s,l,maxit),ib=value(z,1/s,-right/s,l,maxit);
    if(!va.ok || !vb.ok || !ia.ok || !ib.ok)return stopped(a);
    const double shift=n*std::log(unit),pad=margin(shift);
    va.upper=std::max(va.upper,ia.upper-shift+pad);vb.upper=std::max(vb.upper,ib.upper-shift+pad);
    if(left!=right && !(va.lower>vb.upper || vb.lower>va.upper)) {
      a=stopped(a,false,true);a.stop_detail.candidates.clear();
      a.stop_detail.candidates.push_back({{a.mu,a.sigma,l},va.lower,va.upper});
      a.stop_detail.candidates.push_back({{b.mu,b.sigma,l},vb.lower,vb.upper});
      a.stop_detail.loglik_lower=std::max(va.lower,vb.lower);return a;
    }
    out=va.lower>vb.upper?a:b;
    if(!std::isfinite(out.logLik))return stopped(out);
    out.certificate_lower=std::max(va.lower,vb.lower);
    out.certificate_upper=std::max(va.upper,vb.upper);
    if(out.certificate_upper-out.certificate_lower>n*tol)return stopped(out);
    out.certified=true;out.status=TlambdaStatus::Ok;return out;
  }

  if(hs && l>0) {
    const double feasible=width_slack(raw.front(),raw.back(),sigma,l);
    if(!std::isfinite(feasible))return stopped(out);
    if(feasible<=0){out.status=TlambdaStatus::InvalidSupport;return out;}
  }
  double a=hs?1/s:1/std::max(.5,1.2*l*radius),b=hm?0:-m*a;
  if(!hm && !hs && l>0)a=1/std::max(.5,.6*l*(hi-lo)),b=-m*a;
  Value v=value(z,a,b,l,maxit,true);
  out=recover(-b/a,1/a);
  if(!v.ok)return stopped(out);
  // Compact superlevel ranges in inverse-scale coordinates. Tangents dominate
  // this concave objective on its entire convex domain, not just locally.
  double amax=a,bmax=std::abs(b);
  if(!hs) {
    if(l>0)amax=hm?1/(l*radius):2/(l*(hi-lo));
    else {
      double distance=0;const double centre=hm?0:y[y.size()/2];
      for(double q:y)distance+=std::abs(q-centre);
      if(!(distance>0))return stopped(out);
      amax=std::max(a,n/distance);
      bool closed=false;
      for(std::int64_t j=0;j<solver_budget(maxit);++j) {
        if(n*std::log(amax)-amax*distance<v.lower-margin(v.lower)){closed=true;break;}
        amax*=2;if(!std::isfinite(amax))return stopped(out);
      }
      if(!closed)return stopped(out);
    }
  }
  if(!hm)bmax=amax*std::max(std::abs(lo),std::abs(hi))+(l>0?1/l:std::log(amax)-v.lower/n);
  if(!(amax>0) || !std::isfinite(amax+bmax))return stopped(out);
  std::int64_t evaluations=1;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    if(evaluations>=certificate_budget(maxit))return stopped(out,true);
    out=recover(-b/a,1/a);out.iterations=static_cast<int>(j);
    const auto actual=point_value(x,out.mu,out.sigma,l,maxit);++evaluations;
    if(!actual.ok)return stopped(out);
    out.certificate_lower=actual.lower;out.certificate_upper=actual.upper;
    // Roundoff padding on score cancellation and tangent support calculation.
    const double pa=v.ga_error;
    const double pb=v.gb_error;
    const double gap=(hs?0:(std::abs(v.ga)+pa)*std::max(a,amax-a))+
        (hm?0:(std::abs(v.gb)+pb)*(bmax+std::abs(b)));
    double da=0,db=0;
    if(hm)da=-v.ga/v.haa;
    else if(hs)db=-v.gb/v.hbb;
    else {
      const double det=v.haa*v.hbb-v.hab*v.hab;
      if(!(det>0) || !std::isfinite(det))return stopped(out);
      da=(-v.hbb*v.ga+v.hab*v.gb)/det;
      db=(v.hab*v.ga-v.haa*v.gb)/det;
    }
    if(!std::isfinite(da+db))return stopped(out);
    const double coefficient_step=std::max(std::abs(da/a),std::abs((db-b*da/a)/a));
    if(gap+v.upper-v.lower<=std::min(tol,1e-9)*n && coefficient_step<=1e-9) {
      const double shift=n*std::log(unit),shift_pad=margin(shift);
      const double lower=actual.lower,upper=std::max(actual.upper,v.upper+gap-shift+shift_pad);
      if(!std::isfinite(out.logLik) || out.logLik<lower-margin(lower) || out.logLik>upper+margin(upper))return stopped(out);
      if(upper-lower>tol*n)return stopped(out);
      out.certificate_lower=lower;out.certificate_upper=upper;out.certified=true;out.status=TlambdaStatus::Ok;return out;
    }
    bool accepted=false;
    double step=1;
    while(evaluations<certificate_budget(maxit)) {
      const double na=a+step*da,nb=b+step*db;
      if(na==a && nb==b)return stopped(out);
      const auto next=value(z,na,nb,l,maxit,true);++evaluations;
      if(next.solver_failure)return stopped(out);
      if(next.ok && next.ll>=v.ll-margin(v.ll)) {a=na;b=nb;v=next;accepted=true;break;}
      step/=2;
    }
    if(!accepted)return stopped(out,true);
  }
  return stopped(out);
}

// Scalar held-scale location searches and support-contact curves for every
// lambda>1; scalar nuisance searches for a held negative shape.
inline TlambdaResult scalar_boxes(span x,double tol,int maxit,double l,double mu,double sigma) {
  const bool hm=std::isfinite(mu),hs=std::isfinite(sigma),contact=!hm && !hs;
  TlambdaResult out{mu,sigma,l,NAN,0,TlambdaStatus::InvalidData};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0)return out;
  std::vector<double> raw(x.begin(),x.end());for(double v:raw)if(!std::isfinite(v))return out;
  std::sort(raw.begin(),raw.end());
  if(!hs && raw.front()==raw.back() && (!hm || raw.front()==mu)){out.status=TlambdaStatus::Unbounded;return out;}
  if(l<0 && hm && !hs) {
    const double k=std::count(raw.begin(),raw.end(),mu),numerator=std::fma(l,k,raw.size()-k);
    if(numerator<0){out.status=TlambdaStatus::Unbounded;return out;}
    if(numerator==0) {
      const double r=1+1/(-l);
      double face=(l-2)*k*std::log(2.0)-r*(raw.size()-k)*std::log(-l);
      for(double v:raw)if(v!=mu)face-=r*tlambda_detail::logstandard(v,mu,1);
      if(!std::isfinite(face))return stopped(out);
      out.status=TlambdaStatus::CollapseLimit;out.stop_detail.limit_coef={mu,0,l};
      out.stop_detail.limit_loglik=face;out.stop_detail.loglik_lower=out.stop_detail.loglik_upper=face;return out;
    }
  }
  const double origin=hm?mu:raw.front()/2+raw.back()/2;
  double unit=std::max(std::abs(raw.front()-origin),std::abs(raw.back()-origin));
  if(hs)unit=std::max(unit,sigma);
  if(!(unit>0) || !std::isfinite(unit))return stopped(out);
  std::vector<double> y;for(double v:raw) {
    const double q=(v-origin)/unit;
    if(!std::isfinite(q) || (q==0 && v!=origin))return out;
    y.push_back(q);
  }
  for(std::size_t j=1;j<y.size();++j)if(raw[j]!=raw[j-1] && y[j]==y[j-1])return out;
  const double n=x.size(),xmin=y.front(),xmax=y.back(),mid=xmin/2+xmax/2;
  const double fixed_s=hs?sigma/unit:NAN;
  if(l>0 && hs) {
    const double feasible=width_slack(raw.front(),raw.back(),sigma,l);
    if(!std::isfinite(feasible))return stopped(out);
    if(feasible<0){out.status=TlambdaStatus::InvalidSupport;return out;}
  }
  const double logunit=std::log(unit),logM=(l<0 || l>2)?(l-2)*std::log(2.0):0;
  std::int64_t work=0;
  double best=-INFINITY,bestupper=-INFINITY;
  bool arithmetic=false,exhausted=false;
  auto propose=[&](double t) {
    if(work>=certificate_budget(maxit)){exhausted=true;return;}
    ++work;
    const double m=hm?0:t;
    double s=hs?fixed_s:(contact?l*std::max(std::abs(xmin-m),std::abs(xmax-m)):std::exp(t));
    TlambdaResult r{hm?mu:origin+unit*m,hs?sigma:unit*s,l,NAN,static_cast<int>(work),TlambdaStatus::CertificateFailure};
    if(!(r.sigma>0) || !std::isfinite(r.mu+r.sigma))return;
    if(contact) {
      const double radius=std::max(std::abs(raw.front()-r.mu),std::abs(raw.back()-r.mu));
      r.sigma=std::max(r.sigma,l*radius);
      for(std::int64_t j=0;j<solver_budget(maxit) && !contains(raw.front(),raw.back(),r.mu,r.sigma,l);++j)
        r.sigma=std::nextafter(r.sigma,INFINITY);
      if(!contains(raw.front(),raw.back(),r.mu,r.sigma,l)){arithmetic=true;return;}
    }
    if(l>0 && !contains(raw.front(),raw.back(),r.mu,r.sigma,l))return;
    r.logLik=tlambda_detail::likelihood(x,r.mu,r.sigma,l);
    if(!std::isfinite(r.logLik))return;
    auto v=point_value(x,r.mu,r.sigma,l,maxit);
    if(!v.ok){arithmetic=true;return;}
    const double lower=v.lower,upper=v.upper;
    if(r.logLik<lower-margin(lower) || r.logLik>upper+margin(upper)){arithmetic=true;return;}
    if(lower>best){best=lower;bestupper=upper;out=r;out.certificate_lower=lower;out.certificate_upper=upper;}
  };
  const auto proposal=tlambda_mle_impl(x,tol,maxit,l,mu,sigma);
  if(proposal.status==TlambdaStatus::Ok)propose(hm?std::log(proposal.sigma/unit):(proposal.mu-origin)/unit);
  propose(hm?0:mid);
  if(!(std::isfinite(best)))return stopped(out,exhausted);
  // All bounds are derived from this strict sublevel target, never a fixed cap.
  const double ell=best+n*logunit-std::max(1e-8,tol)*n;
  double left=0,right=0;
  if(l<0) {
    const double a=-l,c=1/a,r=1+c,logC=-r*std::log(a);
    if(hm) {
      double k=0,logcoef=0;
      for(double v:y)if(v==0)++k;else logcoef-=r*std::log(std::abs(v));
      const double numerator=std::fma(-a,k,n-k),e=numerator/a;
      if(numerator<0){out.status=TlambdaStatus::Unbounded;return out;}
      logcoef+=k*logM+(n-k)*logC;
      if(numerator==0) {
        out.status=TlambdaStatus::CollapseLimit;
        out.stop_detail.limit_coef={mu,0,l};out.stop_detail.limit_loglik=logcoef-n*logunit;
        out.stop_detail.loglik_lower=out.stop_detail.loglik_upper=out.stop_detail.limit_loglik;
        return out;
      }
      if(!(e>0))return stopped(out);
      left=(ell-logcoef)/e;right=logM-ell/n;
    } else {
      const double logB=(c*logM+logC)/r;
      const double distance=std::exp(logB-ell/n);
      left=xmin-distance;right=xmax+distance;
    }
  } else {
    const double smax=hs?fixed_s:std::exp(logM-ell/n);
    left=xmax-smax/l;right=xmin+smax/l;
  }
  left-=margin(left);right+=margin(right);
  if(!std::isfinite(left+right) || left>right)return stopped(out);
  struct Box {double lo,hi,upper;};
  auto bound=[&](double a,double b) -> double {
    if(work>=certificate_budget(maxit)){exhausted=true;return INFINITY;}
    ++work;
    const double mlo=hm?0:a,mhi=hm?0:b;
    double smin=hs?fixed_s:std::exp(a),smax=hs?fixed_s:std::exp(b);
    if(contact) {
      const double centre=std::max(a,std::min(b,mid));
      smin=l*std::max(std::abs(xmin-centre),std::abs(xmax-centre));
      smax=l*std::max({std::abs(xmin-a),std::abs(xmin-b),std::abs(xmax-a),std::abs(xmax-b)});
    }
    smin*=1-512*std::numeric_limits<double>::epsilon();smax*=1+512*std::numeric_limits<double>::epsilon();
    if(!(smin>0) || !std::isfinite(smax)){arithmetic=true;return INFINITY;}
    Sum upper{-n*std::log(smin)};
    double absolute_log=std::abs(upper.value)+std::abs(n*logunit);
    for(double v:y) {
      const double delta=margin(std::abs(v)+std::abs(mlo)+std::abs(mhi));
      const double dmin=v>=mlo && v<=mhi?0:std::max(0.,std::min(std::abs(v-mlo),std::abs(v-mhi))-delta);
      const double dmax=std::max(std::abs(v-mlo),std::abs(v-mhi))+delta;
      if(l>0 && dmin>smax/l)return -INFINITY;
      // D is smallest at the centre for l<1 or l>2; at the endpoints for 1<l<2.
      const double zbound=(l<1 || l>2)?dmin/smax:dmax/smin;
      const double az=(l<1 || l>2)?std::max(0.,zbound-margin(zbound)):std::min(1/l,zbound+margin(zbound));
      if(l>1 && az==1/l)continue;
      const auto inv=inverse(az,l,maxit);
      if(!inv.ok){arithmetic=true;return INFINITY;}
      const double t=(l<1 || l>2)?inv.lo:inv.hi;
      const double d=tlambda_detail::logderiv(-t,l);upper.add(-d);absolute_log+=std::abs(d);
    }
    const double result=upper.value+margin(absolute_log+n)-n*logunit;
    if(!std::isfinite(result)){arithmetic=true;return INFINITY;}
    return result;
  };
  std::vector<Box> boxes;
  auto order=[](const Box& a,const Box& b){return a.upper<b.upper;};
  boxes.push_back({left,right,bound(left,right)});
  double discarded=-INFINITY;
  while(!boxes.empty() && !arithmetic && !exhausted) {
    std::pop_heap(boxes.begin(),boxes.end(),order);const auto box=boxes.back();boxes.pop_back();
    const double gap=std::max(tol,margin(best)/n)*n;
    if(!std::isfinite(best+gap)){arithmetic=true;break;}
    if(box.upper<=best+gap){discarded=std::max(discarded,box.upper);continue;}
    const double m=box.lo+(box.hi-box.lo)/2;
    if(m==box.lo || m==box.hi){arithmetic=true;break;}
    propose(m);
    for(const auto& interval:{std::array<double,2>{box.lo,m},std::array<double,2>{m,box.hi}}) {
      const double upper=bound(interval[0],interval[1]);
      if(upper>best+gap){boxes.push_back({interval[0],interval[1],upper});std::push_heap(boxes.begin(),boxes.end(),order);}
      else discarded=std::max(discarded,upper);
    }
  }
  out.iterations=static_cast<int>(work);
  if(arithmetic || exhausted) {
    out=stopped(out,exhausted);out.stop_detail.segment_loglik_upper=boxes.empty()?INFINITY:boxes.front().upper;return out;
  }
  out.status=TlambdaStatus::Ok;out.certified=true;
  out.certificate_lower=best;out.certificate_upper=std::max({bestupper,discarded,best});return out;
}
// Collapse at a free location: settled at l=-1, unresolved equalities elsewhere.
inline TlambdaResult collapse_check(span x,double tol,int maxit,double l) {
  TlambdaResult out{NAN,NAN,l,NAN,0,TlambdaStatus::InvalidData};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0)return out;
  std::vector<double> y(x.begin(),x.end());for(double v:y)if(!std::isfinite(v))return out;
  std::sort(y.begin(),y.end());
  double face=-INFINITY,location=NAN;
  for(std::size_t i=0;i<y.size();) {
    std::size_t j=i+1;while(j<y.size() && y[j]==y[i])++j;
    const double k=j-i,numerator=std::fma(l,k,y.size()-k);
    if(numerator<0){out.status=TlambdaStatus::Unbounded;return out;}
    if(numerator==0) {
      const double r=1+1/(-l);
      double f=(l-2)*k*std::log(2.0)-r*(y.size()-k)*std::log(-l);
      for(double v:y)if(v!=y[i])f-=r*tlambda_detail::logstandard(v,y[i],1);
      if(f>face){face=f;location=y[i];}
    }
    i=j;
  }
  if(std::isfinite(face)) {
    out=stopped(out);
    out.status=l== -1?TlambdaStatus::CollapseLimit:TlambdaStatus::CertificateFailure;
    out.stop_detail.limit_coef={location,0,l};out.stop_detail.limit_loglik=face;
    out.stop_detail.loglik_lower=face;
    if(l== -1)out.stop_detail.loglik_upper=face;
  }
  return out;
}

inline TlambdaResult shape_only(span x,double tol,int maxit,double mu,double sigma) {
  TlambdaResult out{mu,sigma,NAN,NAN,0,TlambdaStatus::InvalidData};
  if(x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<=0 || !(sigma>0))return out;
  std::vector<double> y;
  for(double v:x) {
    if(!std::isfinite(v))return out;
    const double z=(v-mu)/sigma;
    if(!std::isfinite(z))return stopped(out);
    y.push_back(z);
  }
  const double n=x.size(),shift=n*std::log(sigma);
  double best=-INFINITY,bestupper=-INFINITY;
  bool numerical=false,budget=false;
  std::int64_t work=0;
  auto propose=[&](double l) {
    if(work>=certificate_budget(maxit)){budget=true;return;}
    ++work;
    if(l>0 && std::any_of(x.begin(),x.end(),[&](double z){
      const double gap=tlambda_detail::slack(z,mu,sigma,l);return l<1?gap<=0:gap<0;
    }))return;
    const auto v=point_value(x,mu,sigma,l,maxit);
    if(!v.ok){numerical=true;return;}
    const double ll=tlambda_detail::likelihood(x,mu,sigma,l),lower=v.lower,upper=v.upper;
    if(!std::isfinite(ll) || ll<lower-margin(lower) || ll>upper+margin(upper)){numerical=true;return;}
    if(lower>best){best=lower;bestupper=upper;out={mu,sigma,l,ll,static_cast<int>(work),TlambdaStatus::CertificateFailure};out.certificate_lower=lower;out.certificate_upper=upper;}
  };
  const auto proposal=tlambda_mle_impl(x,tol,maxit,NAN,mu,sigma);
  if(proposal.status==TlambdaStatus::Ok)propose(proposal.lambda);
  propose(0);propose(1);
  if(!std::isfinite(best))return stopped(out,budget);
  // For l=-a, g<=2^(-a-2); this excludes all shapes below -A.
  const double target=best-std::max(tol,1e-8)*n;
  const double A=std::max(1.0,-(target/n+std::log(sigma))/std::log(2.0)-2+margin(target/n));
  if(!std::isfinite(A))return stopped(out);
  struct Box {double lo,hi,upper;};
  auto bound=[&](double lo,double hi) -> double {
    if(work>=certificate_budget(maxit)){budget=true;return INFINITY;}
    ++work;
    Sum upper{-shift};double absolute_log=std::abs(shift);
    for(double z:y) {
      const double magnitude=std::max(0.,std::abs(z)-margin(z));
      if(lo>0 && magnitude>=1/lo)return -INFINITY;
      const auto inv=inverse(magnitude,lo,maxit);
      if(!inv.ok){numerical=true;return INFINITY;}
      const double d=tlambda_detail::logderiv(-inv.lo,hi);upper.add(-d);absolute_log+=std::abs(d);
    }
    const double result=upper.value+margin(absolute_log+n);
    if(!std::isfinite(result)){numerical=true;return INFINITY;}
    return result;
  };
  auto order=[](const Box& a,const Box& b){return a.upper<b.upper;};
  std::vector<Box> boxes{{-A,1,bound(-A,1)}};
  double discarded=-INFINITY;
  while(!boxes.empty() && !budget && !numerical) {
    std::pop_heap(boxes.begin(),boxes.end(),order);const Box box=boxes.back();boxes.pop_back();
    const double gap=n*std::max(tol,margin(best)/n);
    if(!std::isfinite(best+gap)){numerical=true;break;}
    if(box.upper<=best+gap){discarded=std::max(discarded,box.upper);continue;}
    const double mid=box.lo+(box.hi-box.lo)/2;
    if(mid==box.lo || mid==box.hi){numerical=true;break;}
    propose(mid);
    for(const auto& interval:{std::array<double,2>{box.lo,mid},std::array<double,2>{mid,box.hi}}) {
      const double upper=bound(interval[0],interval[1]);
      if(upper>best+gap){boxes.push_back({interval[0],interval[1],upper});std::push_heap(boxes.begin(),boxes.end(),order);}
      else discarded=std::max(discarded,upper);
    }
  }
  out.iterations=static_cast<int>(work);
  if(budget || numerical){out=stopped(out,budget);out.stop_detail.segment_loglik_upper=boxes.empty()?INFINITY:boxes.front().upper;return out;}
  out.certified=true;out.status=TlambdaStatus::Ok;
  out.certificate_lower=best;out.certificate_upper=std::max({bestupper,discarded,best});return out;
}
inline TlambdaResult fit(span x,double tol,int maxit,double l,double mu=NAN,double sigma=NAN) {
  const bool hl=std::isfinite(l),hm=std::isfinite(mu),hs=std::isfinite(sigma);
  if((!std::isnan(mu) && !hm) || (!std::isnan(sigma) && (!hs || sigma<=0)))
    return {NAN,NAN,l,NAN,0,TlambdaStatus::InvalidData};
  if(hl && hm && hs)return fixed_nonnegative(x,tol,maxit,l,mu,sigma);
  if(hl && l>=0) {
    if(l>1 && l!=2 && ((!hm && !hs) || (l>2 && hs && !hm)))return scalar_boxes(x,tol,maxit,l,mu,sigma);
    return fixed_nonnegative(x,tol,maxit,l,mu,sigma);
  }
  if(hl && l<0) {
    if(hm || hs)return scalar_boxes(x,tol,maxit,l,mu,sigma);
    const auto collapse=collapse_check(x,tol,maxit,l);
    if(collapse.status!=TlambdaStatus::InvalidData)return collapse;
  }
  if(!hl && std::isnan(l) && hm && hs)return shape_only(x,tol,maxit,mu,sigma);
  const auto legacy=tlambda_mle_impl(x,tol,maxit,l,mu,sigma);
  if(legacy.status==TlambdaStatus::InvalidData && std::isnan(l) && !hm && !hs && x.size()>=3 && tol>0 && std::isfinite(tol) && maxit>0) {
    std::vector<double> y(x.begin(),x.end());
    if(std::all_of(y.begin(),y.end(),[](double v){return std::isfinite(v);})) {
      std::sort(y.begin(),y.end());std::size_t largest=1,run=1;
      for(std::size_t j=1;j<y.size();++j){run=y[j]==y[j-1]?run+1:1;largest=std::max(largest,run);}
      if(largest==y.size()-largest) {
        // The l=-1 face is only a lower bound when shape is free. It must
        // never be promoted to a nonattainment decision from a tolerance tie.
        auto out=collapse_check(x,tol,maxit,-1);
        out.status=TlambdaStatus::CertificateFailure;out.stop_detail.loglik_upper=INFINITY;
        const auto uniform=tlambda_detail::uniform_fit(y.front(),y.back(),static_cast<double>(y.size()),1,maxit,tol);
        if(uniform.status==TlambdaStatus::Ok) {
          const double pad=margin(uniform.logLik);
          out.stop_detail.candidates.push_back({{uniform.mu,uniform.sigma,1},uniform.logLik-pad,uniform.logLik+pad});
          out.stop_detail.loglik_lower=std::max(out.stop_detail.loglik_lower,uniform.logLik-pad);
        }
        return out;
      }
    }
  }
  return legacy;
}
} // namespace tlambda_cert_detail
