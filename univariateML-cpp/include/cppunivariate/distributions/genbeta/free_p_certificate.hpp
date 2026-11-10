#pragma once
#include <climits>
// Included after power_profile_certificate.hpp; reuse its compensated reductions.
namespace cppunivariate {
namespace genbeta_free_p_cert {
inline double shape_score(double p,double q,double r,double& error) {
  const double eps=std::numeric_limits<double>::epsilon();
  const double l=std::log1p(q/p),a=trgamma_detail::log_minus_digamma(p),
    b=trgamma_detail::log_minus_digamma(q+p);
  // Below the asymptotic seam, include the uncancelled log/digamma
  // operands in the practical rounding allowance.
  const double operands=(p<100?2*std::abs(std::log(p)):0)+
    (q+p<100?2*std::abs(std::log(q+p)):0);
  error=64*eps*(std::abs(l)+std::abs(a)+std::abs(b)+r+operands);
  return l+a-b-r;
}
struct ShapeRange { double lower=NAN,upper=NAN; bool enclosed=false; };
// For all s in [sl,sh], q in [ql,qh], the conditional p root decreases
// with s and increases with q. Only the two diagonal corner solves are needed.
inline ShapeRange inner_range(double sl,double sh,double ql,double qh,int maxit) {
  ShapeRange out;
  if(!(sl>0&&sh>=sl&&ql>0&&qh>=ql)||!std::isfinite(sh+qh))return out;
  auto score=[&](double p,double q,double s,double& error) {
    const double value=shape_score(p,q,s,error);error*=2;return value;
  };
  auto corner=[&](double s,double q) {
    ShapeRange z;
    double lo=0,hi=std::max(1.,2*q/s),err=0;
    if(!std::isfinite(hi))return z;
    // This same upper bracket is used by the held-q certificate.
    if(!(score(hi,q,s,err)< -err))return z;
    for(std::int64_t i=0;i<solver_budget(maxit);++i) {
      const double mid=lo+(hi-lo)/2;
      if(!(mid>lo&&mid<hi))break;
      const double value=score(mid,q,s,err);
      if(!std::isfinite(value+err))return z;
      if(value>err)lo=mid;
      else if(value< -err)hi=mid;
      else break;
    }
    // Every retained endpoint was independently sign validated. No rounded
    // zero is accepted as a root; a wide interval is still a valid enclosure.
    if(lo>0&&hi>lo){z.lower=lo;z.upper=hi;z.enclosed=true;}
    return z;
  };
  const auto l=corner(sh,ql),h=corner(sl,qh);
  if(l.enclosed&&h.enclosed)out={l.lower,h.upper,true};
  return out;
}

struct Point {
  double h=0,r=0,d=0,k=0,plo=0,phi=0,p=0,value=0,lower=0,upper=0,
    slo=0,shi=0,jlo=0,jhi=0;
  bool ok=false;
};
inline GenbetaResult fit(const std::vector<double>& y,double fixed_q,double scale,
    double mean,double jac,double tol,int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GenbetaResult out{nan,fixed_q,nan,scale,nan,0,GenbetaStatus::SearchUnresolved};
  out.certificate_failure=GenbetaCertificateFailure::Arithmetic;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  const double accuracy=std::min(tol,1e-9);
  const std::int64_t budget=certificate_budget(maxit);
  std::int64_t work=0;
  GenbetaCertificateFailure reason=GenbetaCertificateFailure::Arithmetic;
  genbeta_power_cert::Sum total;for(double z:y)total.add(z);
  mean=static_cast<double>(total.value/n);
  std::vector<double> v;v.reserve(y.size());
  genbeta_power_cert::Sum logs;double vmin=INFINITY;
  for(double z:y){const double w=z/mean;if(!(w>0)||!std::isfinite(w))return out;
    v.push_back(w);logs.add(std::log(w));vmin=std::min(vmin,w);}
  const double g=static_cast<double>(logs.value/n),shift=jac-n*std::log(mean);
  if(!(vmin<1)||!std::isfinite(shift))return out;
  const double q=fixed_q; // Held shape2; Point.p encloses the free shape1.
  auto pscore=[&](double p,double r,double& error) {
    return shape_score(p,q,r,error);
  };
  auto evaluate=[&](double h) {
    reason=GenbetaCertificateFailure::Arithmetic;
    Point z;z.h=h;
    if(++work>budget||!(h>0)||!std::isfinite(h))return z;
    genbeta_power_cert::Sum r,d,k;
    for(double w:v) {
      const double t=h*w,e=std::exp(-t),den=-std::expm1(-t);
      r.add(-(t<.6931471805599453?std::log(den):std::log1p(-e)));
      d.add(w*e/den);k.add(w*w*e/(den*den));
    }
    z.r=static_cast<double>(r.value/n);z.d=static_cast<double>(d.value/n);z.k=static_cast<double>(k.value/n);
    if(!(z.r>0)||!std::isfinite(z.r+z.d+z.k))return z;
    double lo=0,hi=std::max(1.,2*q/h),error=0;
    if(!std::isfinite(hi))return z;
    bool solved=false;
    for(std::int64_t it=0;it<solver_budget(maxit);++it) {
      const double mid=lo+(hi-lo)/2,s=pscore(mid,h,error);
      if(!std::isfinite(s)||!std::isfinite(error))return z;
      if(s>0)lo=mid;else hi=mid;
      if(lo>0&&hi-lo<=1e-13*lo){solved=true;break;}
    }
    if(!solved){reason=GenbetaCertificateFailure::ShapeSolve;return z;}
    z.p=lo+(hi-lo)/2;
    const double tri=special::trigamma(z.p),trip=special::trigamma(z.p+q);
    const double jp=128*eps*(std::abs(tri)+std::abs(trip));
    const double jlower=std::max(q/z.p/(z.p+q),tri-trip-jp);
    pscore(z.p,h,error);
    const double radius=std::max(4e-13,4*error/(z.p*jlower));
    if(!(radius<.01)||!std::isfinite(radius))return z;
    // Independently validate the p enclosure with padded opposite signs.
    z.plo=z.p*(1-radius);z.phi=z.p*(1+radius);
    const double sl=pscore(z.plo,h,error),el=error;
    const double sh=pscore(z.phi,h,error),eh=error;
    if(!(sl>el&&sh< -eh)){reason=GenbetaCertificateFailure::ShapeSigns;return z;}
    // J(p)=trigamma(p)-trigamma(p+q) is positive and decreasing.
    // Evaluate at the p bracket ends, not just its midpoint.
    const double tl=special::trigamma(z.plo),tpl=special::trigamma(z.plo+q),
      th=special::trigamma(z.phi),tph=special::trigamma(z.phi+q);
    z.jlo=std::max(q/z.phi/(z.phi+q),th-tph-128*eps*(std::abs(th)+std::abs(tph)));
    z.jhi=tl-tpl+128*eps*(std::abs(tl)+std::abs(tpl));
    if(!(z.jlo>0&&z.jhi>=z.jlo)||!std::isfinite(z.jhi))return z;
    const double logbeta=gb2_detail::logbeta(q,z.p);
    z.value=std::log(h)-z.p*h-(q-1)*z.r-logbeta;
    const double pad=512*eps*(1+std::abs(std::log(h))+std::abs(z.p*h)+
      std::abs((q-1)*z.r)+std::abs(logbeta));
    const double qs=pscore(z.p,h,error);
    z.lower=z.value-pad;
    z.upper=z.value+pad+(std::abs(qs)+error)*(z.phi-z.plo);
    const double spad=256*eps*(1/h+z.phi+std::abs((q-1)*z.d));
    z.slo=1/h-z.phi+(q-1)*z.d-spad;
    z.shi=1/h-z.plo+(q-1)*z.d+spad;
    z.ok=std::isfinite(z.lower)&&std::isfinite(z.upper)&&std::isfinite(z.slo)&&std::isfinite(z.shi);
    return z;
  };
  // Start from a moderate power; conditional fitting supplies a lower witness.
  Point witness=evaluate(1);
  auto remember=[&](const Point& z) {
    if(!z.ok)return;
    if(!witness.ok||z.lower>witness.lower)witness=z;
  };
  auto failure=[&](bool exhausted) {
    out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
    out.stop_detail.budget_exhausted=exhausted;
    out.certificate_failure=exhausted?GenbetaCertificateFailure::Budget:reason;
    if(witness.ok&&std::isfinite(witness.h/mean)&&witness.h/mean>0) {
      out.stop_detail.candidates={{{witness.p,q,witness.h/mean,scale},
        n*witness.lower+shift,n*witness.upper+shift,StopModel::None}};
      out.stop_detail.loglik_lower=n*witness.lower+shift;
      out.stop_detail.loglik_upper=INFINITY;
    }
    return out;
  };
  if(!witness.ok)return failure(work>budget);
  // Proposals improve the lower witness; only interval coverage certifies it.
  Point pl=witness,ph=witness;
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    if(pl.slo>0&&ph.shi<0)break;
    const double power=pl.slo<=0?pl.h*.5:ph.h*2;
    Point z=evaluate(power);if(!z.ok)return failure(work>budget);remember(z);
    if(pl.slo<=0)pl=z;else ph=z;
  }
  if(pl.slo>0&&ph.shi<0)for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const double power=std::sqrt(pl.h)*std::sqrt(ph.h);
    Point z=evaluate(power);if(!z.ok)return failure(work>budget);remember(z);
    if(ph.h-pl.h<1e-7*pl.h)break;
    if(z.slo>0)pl=z;else if(z.shi<0)ph=z;else break;
  }
  // Uniform gamma and fixed-endpoint power envelopes; see this job's proof.
  const double gamma_base=q>=100?.5*std::log(q)-.5*std::log(6.2831853071795864769)-gamma_detail::stirlerr(q):q*std::log(q)-q-special::lgamma(q);
  const double gamma=gamma_base+(q-1)*g;
  const double gamma_pad=512*eps*(1+std::abs(gamma_base)+std::abs((q-1)*g));
  if(!std::isfinite(gamma)){reason=GenbetaCertificateFailure::Arithmetic;return failure(false);}
  double left=.5,right=std::max(2.,1/vmin);
  bool closed=false;
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const double upper=gamma+gamma_pad+std::max(1.,q)*left;
    if(upper+512*eps*(1+std::abs(upper))<witness.lower){closed=true;break;}
    left*=.5;if(!(left>0))break;
  }
  if(!closed){reason=GenbetaCertificateFailure::GammaNeighborhood;return failure(false);}
  closed=false;
  const double A=std::max(0.,special::digamma(q)-special::digamma(1.));
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const double R=-std::log1p(-std::exp(-right*vmin));
    const double upper=-1+A*(q+1)/right+std::max(0.,1-q)*R;
    if(std::isfinite(upper)&&upper+512*eps*(1+std::abs(upper))<witness.lower){closed=true;break;}
    right*=2;if(!std::isfinite(right))break;
  }
  if(!closed){reason=GenbetaCertificateFailure::PowerNeighborhood;return failure(false);}
  struct Segment{Point l,h;};
  Point l=evaluate(left);if(!l.ok)return failure(work>budget);
  Point h=evaluate(right);if(!h.ok)return failure(work>budget);
  remember(l);remember(h);
  std::vector<Segment> queue{{l,h}};
  std::vector<FitCandidate> roots;
  std::vector<Point> points;
  while(!queue.empty()) {
    if(work>=budget)return failure(true);
    ++work;
    const auto box=queue.back();queue.pop_back();
    const auto& L=box.l;const auto& H=box.h;
    // Free shape1 decreases with h; D,K decrease; J decreases in shape1.
    const double smin=1/H.h-L.phi+(q-1)*(q>=1?H.d:L.d);
    const double smax=1/L.h-H.plo+(q-1)*(q>=1?L.d:H.d);
    const double spad=512*eps*(1/L.h+L.phi+std::abs((q-1)*L.d));
    const double envelope=std::min(L.upper+std::max(0.,smax+spad)*(H.h-L.h),
      H.upper+std::max(0.,-smin+spad)*(H.h-L.h));
    if(envelope+512*eps*(1+std::abs(envelope))<witness.lower)continue;
    if(smin>spad||smax< -spad)continue;
    const double cmax=-1/(H.h*H.h)+1/L.jlo+(q>=1?-(q-1)*H.k:(1-q)*L.k);
    const double cmin=-1/(L.h*L.h)+1/H.jhi+(q>=1?-(q-1)*L.k:(1-q)*H.k);
    const double cpad=1024*eps*(1/(L.h*L.h)+1/L.jlo+std::abs((q-1)*L.k));
    if(cmin>cpad)continue; // Strictly convex: no interior local maximum.
    if(cmax< -cpad) {
      // Strict profile concavity on this whole segment.
      if(L.shi<0||H.slo>0)continue;
      if(!(L.slo>0&&H.shi<0)){reason=GenbetaCertificateFailure::RootSigns;return failure(false);}
      Point a=L,b=H,m;bool converged=false;
      for(std::int64_t it=0;it<solver_budget(maxit);++it) {
        if(work>=budget)return failure(true);
        const double center=a.h+(b.h-a.h)/2;
        m=evaluate(center);if(!m.ok)return failure(work>budget);remember(m);
        if(m.slo>0)a=m;
        else if(m.shi<0)b=m;
        else {
          // A rounded zero does not enclose a root. Validate nearby signs.
          const double radius=accuracy*.125/std::max(1.,center/(m.plo*m.jlo));
          Point al=evaluate(center*(1-radius));if(!al.ok)return failure(work>budget);
          Point bh=evaluate(center*(1+radius));if(!bh.ok)return failure(work>budget);
          if(!(al.h>=L.h&&bh.h<=H.h&&al.slo>0&&bh.shi<0)){reason=GenbetaCertificateFailure::RootSigns;return failure(false);}
          a=al;b=bh;
        }
        if((b.h-a.h)/a.h<accuracy*.5 && (a.phi-b.plo)/b.plo<accuracy*.5){converged=true;break;}
      }
      if(!converged){reason=GenbetaCertificateFailure::CoefficientInclusion;return failure(false);}
      m=evaluate(a.h+(b.h-a.h)/2);if(!m.ok)return failure(work>budget);remember(m);
      if(!(m.p>=b.plo&&m.p<=a.phi)){reason=GenbetaCertificateFailure::CoefficientInclusion;return failure(false);}
      if(!(m.h/mean>0)||!std::isfinite(m.h/mean))return failure(false);
      const double upper=m.upper+std::max(std::abs(m.slo),std::abs(m.shi))*(b.h-a.h);
      roots.push_back({{m.p,q,m.h/mean,scale},n*m.lower+shift,n*upper+shift,StopModel::None});
      points.push_back(m);
      continue;
    }
    const double mid=H.h/L.h>4?std::sqrt(L.h)*std::sqrt(H.h):L.h+(H.h-L.h)/2;
    if(!(mid>L.h&&mid<H.h))return failure(false);
    Point m=evaluate(mid);if(!m.ok)return failure(work>budget);remember(m);
    queue.push_back({m,H});queue.push_back({L,m});
  }
  if(roots.empty())return failure(false);
  std::size_t best=0;for(std::size_t i=1;i<roots.size();++i)
    if(roots[i].loglik_lower>roots[best].loglik_lower)best=i;
  for(std::size_t i=0;i<roots.size();++i)if(i!=best&&roots[i].loglik_upper>=roots[best].loglik_lower) {
    out.certificate_failure=GenbetaCertificateFailure::None;
    out.stop_detail.candidates=roots;out.stop_detail.comparison_tie=true;
    out.stop_detail.loglik_lower=roots[best].loglik_lower;out.stop_detail.loglik_upper=INFINITY;
    out.iterations=static_cast<int>(work);return out;
  }
  const auto& bestpoint=points[best];
  out.shape1=bestpoint.p;out.shape3=bestpoint.h/mean;
  out.logLik=n*bestpoint.value+shift;out.iterations=static_cast<int>(work);
  if(std::isfinite(out.shape3)&&out.shape3>0&&std::isfinite(out.logLik)) {
    out.status=GenbetaStatus::Ok;out.certificate_failure=GenbetaCertificateFailure::None;
  }
  return out;
}
} // namespace genbeta_free_p_cert
} // namespace cppunivariate
