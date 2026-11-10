#pragma once
// The linear power term is a gamma rate score. In squared power, a signed
// quadratic coefficient and an explicit fourth-order remainder control the face.
namespace cppunivariate {
namespace genbeta_gamma_face_cert {
struct Neighborhood {
  double h=NAN,quadratic=NAN,fourth=INFINITY,upper=INFINITY,kl=NAN,kh=NAN,
    ql=NAN,qh=NAN,nl=NAN,nh=NAN;
  bool enclosed=false;
};
inline Neighborhood neighborhood(double mu,double c,double m2,double m4,
    const GenbetaTailBounds& tail,int maxit,std::int64_t& work) {
  Neighborhood out;
  const double e=std::numeric_limits<double>::epsilon(),cp=1024*e*(1+std::abs(c)+std::abs(std::log(mu))),
    mp=1024*e*(1+m2),mlo=std::max(0.,m2-mp),mhi=m2+mp,
    ulo=mu*(1-1024*e),uhi=mu*(1+1024*e);
  if(!(c>cp&&ulo>0&&tail.small_quadratic_domain>0&&std::isfinite(m4)))return out;
  double kl=1/(2*(c+cp)),kh=1/(c-cp);
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    const double k=kl+(kh-kl)/2,a=trgamma_detail::log_minus_digamma(k),s=a-c,
      err=cp+256*e*(std::abs(a)+(k<100?2*std::abs(std::log(k)):0));
    if(s>err)kl=k;else if(s< -err)kh=k;else break;
  }
  out.kl=kl;out.kh=kh;
  auto coefficient=[&](double k) {
    const double z=k-1/k;
    return ((k-1)*(k>=1?mhi:mlo)-z*(z>=0?ulo*ulo:uhi*uhi))/24;
  };
  // C0(k) is convex: its maximum over the gamma shape enclosure is at an end.
  const double c2=std::max(coefficient(kl),coefficient(kh))+
    4096*e*(1+kh+1/kl+mhi+uhi*uhi);
  out.quadratic=c2;
  if(!(c2<0))return out;
  const double A=tail.small_quadratic_domain*uhi,
    q0=(std::exp(-A)-.5)/(c+cp)*(1-4096*e),
    denominator=c-cp-A*A*mhi/(24*ulo*ulo);
  if(!(A>0&&q0>0&&denominator>0))return out;
  const double Q=(1/denominator)*(1+4096*e),Qk=std::max(Q,kh),
    lower_cost=kh*uhi*uhi/6+std::max(0.,1-kl)*mhi/24,
    D=(tail.small_quadratic_coefficient+lower_cost)*(1+4096*e);
  if(!(D>0)||!std::isfinite(D))return out;
  double H=std::min(tail.small_quadratic_domain*(1-8192*e),1/uhi);
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    if(++work>certificate_budget(maxit))return out;
    const double coarse_lo=q0*H/std::expm1(H*uhi)-H/2,
      coarse_hi=Q/ulo+H*(Q+1)/2;
    if(coarse_lo>0) {
      // G-g >= (q-k)^2/(4 Qk^2) + q0*(nu-q/mu)^2/(2 coarse_hi^2).
      const double qr=2*Qk*std::sqrt(D)*H,nr=coarse_hi*std::sqrt(2*D/q0)*H;
      double ql=std::max(q0,kl-qr),qh=std::min(Q,kh+qr),
        nl=std::max(coarse_lo,ql/uhi-nr),nh=std::min(coarse_hi,qh/ulo+nr);
      if(qh>=ql&&nl>qh*H/2&&nh>=nl) {
        // Centered remainder score bounds contract the conditional optimum
        // by O(h^2), improving the O(h) objective-gap enclosure uniformly.
        for(std::int64_t j=0;j<solver_budget(maxit);++j) {
          const double gap=nl-qh*H/2,ratio=qh*qh*H*H/(4*nl*nl),
            path=std::min(nl,ql/uhi),
            Bn=(qh/(12*gap*gap*gap)+qh*qh*qh/(12*nl*nl*nl*(1-ratio)))*(1+8192*e),
            Bq=(qh*qh/(8*nl*nl*(1-ratio))+1/(24*gap*gap)+mhi/24)*(1+8192*e),
            radius_n=nh/ulo*Bn*H*H,
            gamma_qmax=std::max(qh,kh),
            radius_q=2*gamma_qmax*gamma_qmax*(Bq+nh/(ulo*path)*Bn)*H*H,
            error=8192*e*(1+qh+kh+nh+1/ulo+mhi),
            qlo=std::max(ql,kl-radius_q-error),qhi=std::min(qh,kh+radius_q+error),
            nlo=std::max(nl,qlo/uhi-radius_n-error),nhi=std::min(nh,qhi/ulo+radius_n+error);
          if(!(qhi>=qlo&&nhi>=nlo))break;
          if(qlo==ql&&qhi==qh&&nlo==nl&&nhi==nh)break;
          ql=qlo;qh=qhi;nl=nlo;nh=nhi;
        }
        const double pathlo=std::min(nl,ql/uhi),pathhi=std::max(nh,qh/ulo),
          qmin=std::min(ql,kl),qmax=std::max(qh,kh);
        double cubic=std::max(std::abs(ql-ql*ql*ql),std::abs(qh-qh*qh*qh));
        const double turn=1/std::sqrt(3.);
        if(ql<=turn&&turn<=qh)cubic=std::max(cubic,2/(3*std::sqrt(3.)));
        const double variance=std::max(std::abs(mlo-uhi*uhi),std::abs(mhi-ulo*ulo)),
          Lq=(variance+uhi*uhi/(qmin*qmin))/24,
          Ln=cubic/(12*pathlo*pathlo*pathlo)*(1+4096*e),
          R=qh*qh*qh/(96*nl*nl*(nl*nl-qh*qh*H*H/4))+
            std::max(0.,1-ql)*(m4*(1+1024*e)+1024*e)/2880,
          C4=(qmax*qmax*Lq*Lq+pathhi*pathhi*Ln*Ln/(2*ql)+R)*(1+8192*e),
          upper=c2+C4*H*H,pad=8192*e*(1+std::abs(c2)+C4*H*H);
        if(std::isfinite(upper)&&upper< -pad) {
          out.h=H;out.fourth=C4;out.upper=upper+pad;
          out.ql=ql;out.qh=qh;out.nl=nl;out.nh=nh;out.enclosed=true;return out;
        }
      }
    }
    H*=.5;if(!(H>0))break;
  }
  return out;
}
inline GenbetaResult verify(const std::vector<double>& y,double mean,double jac,
    double tol,int maxit,GenbetaResult out) {
  const double n=static_cast<double>(y.size()),e=std::numeric_limits<double>::epsilon(),
    shift=jac-n*std::log(mean),original=out.logLik;
  const auto legacy=out;
  std::int64_t work=out.iterations;
  auto unresolved=[&](GenbetaCertificateFailure reason) {
    out=legacy;out.allfree_certified=false;out.certificate_failure=reason;
    out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
    out.stop_detail.loglik_upper=INFINITY;return out;
  };
  if(work>=certificate_budget(maxit))return unresolved(GenbetaCertificateFailure::Budget);
  if(!valid_stop_coefficients(StopModel::UnitGamma,out.stop_detail.limit_coef))
    return unresolved(GenbetaCertificateFailure::Arithmetic);
  const double k=out.stop_detail.limit_coef[0],scale=out.stop_detail.limit_coef[1],
    pad=8192*e*(1+std::abs(original)+std::abs(jac)+n*(1+k+std::abs(std::log(scale)))),
    lower=(original-pad-shift)/n;
  std::vector<double> v;genbeta_power_cert::Sum sum,logs,second,fourth;
  for(double z:y){const double w=z/mean;v.push_back(w);sum.add(w);logs.add(std::log(w));second.add(w*w);fourth.add(w*w*w*w);}
  const double mu=static_cast<double>(sum.value/n),c=std::log(mu)-static_cast<double>(logs.value/n),
    m2=static_cast<double>(second.value/n),m4=static_cast<double>(fourth.value/n);
  out.tail_bounds=genbeta_allfree_bounds::tails(y,mean,jac,k,original-pad,maxit);
  const auto tail=neighborhood(mu,c,m2,m4,out.tail_bounds,maxit,work);
  if(!tail.enclosed)return unresolved(work>certificate_budget(maxit)?GenbetaCertificateFailure::Budget:GenbetaCertificateFailure::GammaNeighborhood);
  const double radius=std::min(tol*.5,.25);
  if(!(tail.kl>=k*(1-radius)&&tail.kh<=k*(1+radius)&&
       mean*(1-4096*e)/tail.kh>=scale*(1-radius)&&mean*(1+4096*e)/tail.kl<=scale*(1+radius)))
    return unresolved(GenbetaCertificateFailure::CoefficientInclusion);
  if(!out.tail_bounds.large_excluded)return unresolved(GenbetaCertificateFailure::PowerNeighborhood);
  const double gu=(out.tail_bounds.gamma_loglik_upper-shift)/n,
    margin=8192*e*(1+std::abs(gu)+std::abs(lower));
  std::sort(v.begin(),v.end());
  struct Point {genbeta_allfree_cert::Point profile;genbeta_power_face_cert::R r;};
  FitCandidate improvement;bool contradicted=false,comparison_blocked=false;
  auto evaluate=[&](double h) {
    Point point{};auto& z=point.profile;z.h=h;
    if(++work>certificate_budget(maxit))return point;
    point.r=genbeta_power_face_cert::stats_sorted(v,h);z.r=point.r.r;
    const auto p=genbeta_detail::profile(v,h,NAN,NAN,maxit);
    const double pp=p.ok?p.a:1/h,qq=p.ok?p.b:1;
    const auto b=genbeta_allfree_bounds::conditional(v,h,pp,qq,0,maxit);
    genbeta_power_cert::Sum d,curvature;
    for(double w:v) {
      const double t=h*w,exp=std::exp(-t),den=-std::expm1(-t);
      d.add(w*exp/den);curvature.add(w*w*exp/(den*den));
    }
    z.d=static_cast<double>(d.value/n);z.k=static_cast<double>(curvature.value/n);
    z.upper=h<=tail.h?gu+tail.upper*h*h:INFINITY;
    if(b.enclosed) {
      z.shape={b.shape1_lower,b.shape1_upper,b.shape2_lower,b.shape2_upper,true};
      z.lower=b.loglik_lower/n;z.upper=std::min(z.upper,b.loglik_upper/n);
      const double sp=2048*e*(1/h+z.shape.ph*(1+std::abs(mu))+(1+z.shape.qh)*z.d);
      z.slo=1/h-z.shape.ph*mu+(z.shape.ql-1)*z.d-sp;
      z.shi=1/h-z.shape.pl*mu+(z.shape.qh-1)*z.d+sp;
      // Every cell containing this feasible witness has upper >= its lower.
      // Our fixed comparison threshold cannot exclude such a cell, however
      // deeply it is subdivided. Retain the uncertified legacy face now.
      if(z.lower+margin>=lower)comparison_blocked=true;
      if(n*z.lower+shift>out.tail_bounds.gamma_loglik_upper) {
        contradicted=true;improvement={{pp,qq,h/mean,out.scale},n*z.lower+shift,b.loglik_upper+shift,StopModel::None};
      }
    }
    z.ok=std::isfinite(point.r.lr+z.d+z.k);return point;
  };
  struct Cell {Point l,h;};
  std::vector<Cell> queue{{evaluate(tail.h),evaluate(out.tail_bounds.large_cutoff)}};
  while(!queue.empty()) {
    if(contradicted) {
      out.status=GenbetaStatus::SearchUnresolved;out.allfree_certified=false;
      out.certificate_failure=GenbetaCertificateFailure::LegacyContradicted;
      out.stop_detail=comparison_detail({legacy.stop_detail.limit_coef,original-pad,original+pad,StopModel::UnitGamma},improvement);
      out.stop_detail.comparison_tie=false;
      out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));return out;
    }
    if(comparison_blocked)return unresolved(GenbetaCertificateFailure::GammaNeighborhood);
    if(++work>certificate_budget(maxit))return unresolved(GenbetaCertificateFailure::Budget);
    const auto cell=queue.back();queue.pop_back();const auto& l=cell.l.profile;const auto& h=cell.h.profile;
    if(!l.ok||!h.ok)return unresolved(GenbetaCertificateFailure::Arithmetic);
    double upper=genbeta_power_face_cert::gamma_upper(cell.l.r,cell.h.r,mu,maxit);
    if(h.h<=tail.h)upper=std::min(upper,gu+tail.upper*l.h*l.h);
    if(!(upper+margin<lower)) {
      const auto b=genbeta_allfree_cert::enclose(l,h,c+1024*e*(1+std::abs(c)),maxit);
      if(b.shape.enclosed)upper=std::min(upper,std::min(l.upper+std::max(0.,b.smax)*(h.h-l.h),h.upper+std::max(0.,-b.smin)*(h.h-l.h)));
    }
    if(upper+margin<lower)continue;
    const double mid=h.h/l.h>4?std::sqrt(l.h)*std::sqrt(h.h):l.h+(h.h-l.h)/2;
    if(!(mid>l.h&&mid<h.h))return unresolved(GenbetaCertificateFailure::Arithmetic);
    const auto m=evaluate(mid);queue.push_back({m,cell.h});queue.push_back({cell.l,m});
  }
  out.allfree_certified=true;out.certificate_failure=GenbetaCertificateFailure::None;
  out.iterations=static_cast<int>(std::min<std::int64_t>(work,INT_MAX));
  out.stop_detail.loglik_lower=original-pad;out.stop_detail.loglik_upper=out.tail_bounds.gamma_loglik_upper;
  out.stop_detail.candidates={{legacy.stop_detail.limit_coef,original-pad,original+pad,StopModel::UnitGamma}};
  return out;
}
} // namespace genbeta_gamma_face_cert
} // namespace cppunivariate
