#pragma once
#include <climits>
// Included after GenbetaResult and genbeta_detail::profile are defined.
namespace cppunivariate {
namespace genbeta_power_cert {
struct Sum {
  CPPUNIV_EXTENDED_REAL value=0,correction=0;
  void add(double x) {
    const CPPUNIV_EXTENDED_REAL delta=x-correction,next=value+delta;
    correction=(next-value)-delta;value=next;
  }
};
struct Point {
  double h=0,r=0,d=0,k=0,qlo=0,qhi=0,q=0,value=0,lower=0,upper=0,
    slo=0,shi=0,jlo=0,jhi=0;
  bool ok=false;
};
inline GenbetaResult fit(const std::vector<double>& y,double p,double scale,
    double mean,double jac,double tol,int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  GenbetaResult out{p,nan,nan,scale,nan,0,GenbetaStatus::SearchUnresolved};
  out.certificate_failure=GenbetaCertificateFailure::Arithmetic;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  const double accuracy=std::min(tol,1e-9);
  const std::int64_t budget=certificate_budget(maxit);
  std::int64_t work=0;
  GenbetaCertificateFailure reason=GenbetaCertificateFailure::Arithmetic;
  Sum total;for(double z:y)total.add(z);
  mean=static_cast<double>(total.value/n);
  std::vector<double> v;v.reserve(y.size());
  Sum logs;double vmin=INFINITY;
  for(double z:y){const double w=z/mean;if(!(w>0)||!std::isfinite(w))return out;
    v.push_back(w);logs.add(std::log(w));vmin=std::min(vmin,w);}
  const double g=static_cast<double>(logs.value/n),shift=jac-n*std::log(mean);
  if(!(vmin<1)||!std::isfinite(shift))return out;
  auto qscore=[&](double q,double r,double& error) {
    const double l=std::log1p(p/q),a=trgamma_detail::log_minus_digamma(q),
      b=trgamma_detail::log_minus_digamma(p+q);
    // Below the asymptotic seam the helper subtracts log(x)-digamma(x).
    // Its error scales with those operands, not just their small difference.
    const double operands=(q<100?2*std::abs(std::log(q)):0)+
      (p+q<100?2*std::abs(std::log(p+q)):0);
    error=64*eps*(std::abs(l)+std::abs(a)+std::abs(b)+r+operands);
    return l+a-b-r;
  };
  auto evaluate=[&](double h) {
    reason=GenbetaCertificateFailure::Arithmetic;
    Point z;z.h=h;
    if(++work>budget||!(h>0)||!std::isfinite(h))return z;
    Sum r,d,k;
    for(double w:v) {
      const double t=h*w,e=std::exp(-t),den=-std::expm1(-t);
      r.add(-(t<.6931471805599453?std::log(den):std::log1p(-e)));
      d.add(w*e/den);k.add(w*w*e/(den*den));
    }
    z.r=static_cast<double>(r.value/n);z.d=static_cast<double>(d.value/n);z.k=static_cast<double>(k.value/n);
    if(!(z.r>0)||!std::isfinite(z.r+z.d+z.k))return z;
    double lo=0,hi=std::max(1.,2*p/z.r),error=0;
    if(!std::isfinite(hi))return z;
    bool solved=false;
    for(std::int64_t it=0;it<solver_budget(maxit);++it) {
      const double mid=lo+(hi-lo)/2,s=qscore(mid,z.r,error);
      if(!std::isfinite(s)||!std::isfinite(error))return z;
      if(s>0)lo=mid;else hi=mid;
      if(lo>0&&hi-lo<=1e-13*lo){solved=true;break;}
    }
    if(!solved){reason=GenbetaCertificateFailure::ShapeSolve;return z;}
    z.q=lo+(hi-lo)/2;
    const double tri=special::trigamma(z.q),trip=special::trigamma(z.q+p);
    const double jp=128*eps*(std::abs(tri)+std::abs(trip));
    const double jlower=std::max(p/z.q/(z.q+p),tri-trip-jp);
    qscore(z.q,z.r,error);
    const double radius=std::max(4e-13,4*error/(z.q*jlower));
    if(!(radius<.01)||!std::isfinite(radius))return z;
    // Independently validate the q enclosure with padded opposite signs.
    z.qlo=z.q*(1-radius);z.qhi=z.q*(1+radius);
    const double sl=qscore(z.qlo,z.r,error),el=error;
    const double sh=qscore(z.qhi,z.r,error),eh=error;
    if(!(sl>el&&sh< -eh)){reason=GenbetaCertificateFailure::ShapeSigns;return z;}
    // J(q)=trigamma(q)-trigamma(q+p) is positive and decreasing.
    // Evaluate at the q bracket ends, not just its midpoint.
    const double tl=special::trigamma(z.qlo),tpl=special::trigamma(z.qlo+p),
      th=special::trigamma(z.qhi),tph=special::trigamma(z.qhi+p);
    z.jlo=std::max(p/z.qhi/(z.qhi+p),th-tph-jp);
    z.jhi=tl-tpl+jp;
    if(!(z.jlo>0&&z.jhi>=z.jlo)||!std::isfinite(z.jhi))return z;
    const double logbeta=gb2_detail::logbeta(p,z.q);
    z.value=std::log(h)-p*h-(z.q-1)*z.r-logbeta;
    const double pad=512*eps*(1+std::abs(std::log(h))+std::abs(p*h)+
      std::abs((z.q-1)*z.r)+std::abs(logbeta));
    const double qs=qscore(z.q,z.r,error);
    z.lower=z.value-pad;
    z.upper=z.value+pad+(std::abs(qs)+error)*(z.qhi-z.qlo);
    const double spad=256*eps*(1/h+p+std::abs((z.q-1)*z.d));
    z.slo=1/h-p+(z.qlo-1)*z.d-spad;
    z.shi=1/h-p+(z.qhi-1)*z.d+spad;
    z.ok=std::isfinite(z.lower)&&std::isfinite(z.upper)&&std::isfinite(z.slo)&&std::isfinite(z.shi);
    return z;
  };
  // q=1, h=1/p is a feasible exponential member with normalized value -1.
  Point witness=evaluate(1/p);
  auto remember=[&](const Point& z) {
    if(!z.ok)return;
    if(!witness.ok||z.lower>witness.lower)witness=z;
  };
  auto failure=[&](bool exhausted) {
    out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
    out.stop_detail.budget_exhausted=exhausted;
    out.certificate_failure=exhausted?GenbetaCertificateFailure::Budget:reason;
    if(witness.ok&&std::isfinite(witness.h/mean)&&witness.h/mean>0) {
      out.stop_detail.candidates={{{p,witness.q,witness.h/mean,scale},
        n*witness.lower+shift,n*witness.upper+shift,StopModel::None}};
      out.stop_detail.loglik_lower=n*witness.lower+shift;
      out.stop_detail.loglik_upper=INFINITY;
    }
    return out;
  };
  if(!witness.ok)return failure(work>budget);
  // Uniform tail bounds, valid for arbitrary q; bounds.md proves them.
  double left=.5,right=std::max(2.,std::max(.6931471805599453/vmin,1/(p*(1-vmin))));
  bool closed=false;
  const double A=p>1?std::log(p)+.5772156649015328606:0;
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const double R=-std::log(left)-g;
    const double u1=R>=1&&R>A?-g+std::max(0.,1-p)*left-std::log(R-A)-1:INFINITY;
    const double u2=std::log(left)-special::lgamma(p)+p*std::log1p(p);
    const double upper=std::max(u1,u2);
    if(upper+512*eps*(1+std::abs(upper))<witness.lower){closed=true;break;}
    left*=.5;if(!(left>0))break;
  }
  if(!closed){reason=GenbetaCertificateFailure::Tail;return failure(false);}
  closed=false;
  for(std::int64_t it=0;it<solver_budget(maxit);++it) {
    const double upper=std::log(right)-p*(1-vmin)*right+p*std::log(n)+1+p*std::log(p)-special::lgamma(p);
    if(std::isfinite(upper)&&upper+512*eps*(1+std::abs(upper))<witness.lower){closed=true;break;}
    right*=2;if(!std::isfinite(right))break;
  }
  if(!closed){reason=GenbetaCertificateFailure::Tail;return failure(false);}
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
    // D and K decrease, q increases, and J decreases with h.
    const double smin=1/H.h-p+(L.qlo>=1?(L.qlo-1)*H.d:(L.qlo-1)*L.d);
    const double smax=1/L.h-p+(H.qhi>=1?(H.qhi-1)*L.d:(H.qhi-1)*H.d);
    const double spad=512*eps*(1/L.h+p+std::abs((L.qlo-1)*L.d)+std::abs((H.qhi-1)*L.d));
    if(smin>spad||smax< -spad)continue;
    const double cmax=-1/(H.h*H.h)+(L.qlo>=1?-(L.qlo-1)*H.k:(1-L.qlo)*L.k)+L.d*L.d/H.jlo;
    const double cmin=-1/(L.h*L.h)+(H.qhi>=1?-(H.qhi-1)*L.k:(1-H.qhi)*H.k)+H.d*H.d/L.jhi;
    const double cpad=1024*eps*(1/(L.h*L.h)+std::abs((L.qlo-1)*L.k)+
      std::abs((H.qhi-1)*L.k)+L.d*L.d/H.jlo);
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
          const double radius=accuracy*.125/std::max(1.,center*m.d/(m.qlo*m.jlo));
          Point al=evaluate(center*(1-radius));if(!al.ok)return failure(work>budget);
          Point bh=evaluate(center*(1+radius));if(!bh.ok)return failure(work>budget);
          if(!(al.h>=L.h&&bh.h<=H.h&&al.slo>0&&bh.shi<0)){reason=GenbetaCertificateFailure::RootSigns;return failure(false);}
          a=al;b=bh;
        }
        if((b.h-a.h)/a.h<accuracy*.5 && (b.qhi-a.qlo)/a.qlo<accuracy*.5){converged=true;break;}
      }
      if(!converged){reason=GenbetaCertificateFailure::CoefficientInclusion;return failure(false);}
      m=evaluate(a.h+(b.h-a.h)/2);if(!m.ok)return failure(work>budget);remember(m);
      if(!(m.q>=a.qlo&&m.q<=b.qhi)){reason=GenbetaCertificateFailure::CoefficientInclusion;return failure(false);}
      if(!(m.h/mean>0)||!std::isfinite(m.h/mean))return failure(false);
      const double upper=m.upper+std::max(std::abs(m.slo),std::abs(m.shi))*(b.h-a.h);
      roots.push_back({{p,m.q,m.h/mean,scale},n*m.lower+shift,n*upper+shift,StopModel::None});
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
  out.shape2=bestpoint.q;out.shape3=bestpoint.h/mean;
  out.logLik=n*bestpoint.value+shift;out.iterations=static_cast<int>(work);
  if(std::isfinite(out.shape3)&&out.shape3>0&&std::isfinite(out.logLik)) {
    out.status=GenbetaStatus::Ok;out.certificate_failure=GenbetaCertificateFailure::None;
  }
  return out;
}
} // namespace genbeta_power_cert
} // namespace cppunivariate
