#pragma once
// Included by mle.hpp after its result and sufficient-statistic proposer.
namespace cppunivariate {
namespace genbeta_allfree_bounds {
struct ShapeBox { double pl=0,ph=INFINITY,ql=0,qh=INFINITY; bool enclosed=false; };
// Uniform over a power cell: the statistic rectangle contains the curved
// sufficient-statistic path. A failed contrast asks the caller to subdivide.
inline ShapeBox rectangle(double sl,double sh,double rl,double rh,double c,int maxit) {
  ShapeBox out;
  const double eps=std::numeric_limits<double>::epsilon();
  if(!(sl>0&&sh>=sl&&rl>0&&rh>=rl)||!std::isfinite(sh+rh+c))return out;
  const double cp=sl+std::log(-std::expm1(-rl)),cq=rl+std::log(-std::expm1(-sl));
  const double pad=512*eps*(1+sh+rh+std::abs(std::log(-std::expm1(-rl)))+
    std::abs(std::log(-std::expm1(-sl))));
  if(!(cp>pad&&cq>pad))return out;
  double pl=std::exp(-rh)/sh,ph=1/(cp-pad),
    ql=std::max(std::exp(-sh)/rh,std::exp(-2-c-sh)),qh=1/(cq-pad);
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    pl=std::max(pl,ql*std::exp(-sh)/(-std::expm1(-sh)));
    ph=std::min(ph,qh/sl+1);
    const auto pr=genbeta_free_p_cert::inner_range(sl,sh,ql,qh,maxit);
    if(pr.enclosed){pl=std::max(pl,pr.lower);ph=std::min(ph,pr.upper);}
    const auto qr=genbeta_free_p_cert::inner_range(rl,rh,pl,ph,maxit);
    const double oldlo=ql,oldhi=qh;
    if(qr.enclosed){ql=std::max(ql,qr.lower);qh=std::min(qh,qr.upper);}
    if(!(pl>0&&ph>=pl&&ql>0&&qh>=ql)||!std::isfinite(ph+qh))return out;
    if(qh-ql<1e-12*ql||(ql<=oldlo*(1+1e-12)&&qh>=oldhi*(1-1e-12)))break;
  }
  out={pl,ph,ql,qh,true};return out;
}
inline GenbetaConditionalBounds conditional(const std::vector<double>& y,
    double power,double p,double q,double jac,int maxit) {
  GenbetaConditionalBounds out;
  if(!(power>0&&p>0&&q>0)||!std::isfinite(power+p+q)||y.empty())return out;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  genbeta_power_cert::Sum ss,rr,ly;
  for(double v:y) {
    const double z=power*v;
    if(!(z>0)||!std::isfinite(z))return out;
    ss.add(z);ly.add(std::log(v));rr.add(-(z<.6931471805599453?std::log(-std::expm1(-z)):std::log1p(-std::exp(-z))));
  }
  const double s=static_cast<double>(ss.value/n),r=static_cast<double>(rr.value/n);
  const double statpad=128*eps*(1+s+r);
  const double sl=s-statpad,sh=s+statpad,rl=r-statpad,rh=r+statpad;
  if(!(sl>0&&rl>0)||!std::isfinite(sh+rh))return out;
  // Contrasts characterize the interior of the beta sufficient-statistic
  // domain. Their lower bounds make the conjugate rectangle uniform over
  // the statistics' practical floating-point allowance.
  const double cp=sl+std::log(-std::expm1(-rl));
  const double cq=rl+std::log(-std::expm1(-sl));
  const double contrastpad=256*eps*(1+sh+rh+std::abs(std::log(-std::expm1(-rl)))+
    std::abs(std::log(-std::expm1(-sl))));
  if(!(cp>contrastpad&&cq>contrastpad))return out;
  double pl=std::exp(-rh)/sh,ph=1/(cp-contrastpad),
    ql=std::exp(-sh)/rh,qh=1/(cq-contrastpad);
  // The attained exponential member gives normalized likelihood -1 at
  // every h. For q<=1, the gamma comparison and Gamma(q)>=exp(-1)/q
  // force q>=exp(-2-c-h), uniformly across the small-power/large-p seam.
  const double c=std::log(s/power)-static_cast<double>(ly.value/n);
  const double cpad=256*eps*(1+std::abs(std::log(s/power))+std::abs(static_cast<double>(ly.value/n)));
  ql=std::max(ql,std::exp(-2-std::max(0.,c+cpad)-sh));
  // psi(p+q)-psi(p)>log(1+q/p), and its upper bound is
  // q/p+q/p^2. Thus q/expm1(s)<p<q/s+1. Both bounds hold uniformly.
  pl=std::max(pl,ql*std::exp(-sh)/(-std::expm1(-sh)));
  ph=std::min(ph,qh/sl+1);
  // Tighten with the held-q inner root over each entire q interval. Its
  // symmetric use bounds q over the retained p interval. These inclusions
  // do not rely on a joint Newton solve succeeding.
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    const auto pr=genbeta_free_p_cert::inner_range(sl,sh,ql,qh,maxit);
    if(pr.enclosed){pl=std::max(pl,pr.lower);ph=std::min(ph,pr.upper);}
    const auto qr=genbeta_free_p_cert::inner_range(rl,rh,pl,ph,maxit);
    const double oldlo=ql,oldhi=qh;
    if(qr.enclosed){ql=std::max(ql,qr.lower);qh=std::min(qh,qr.upper);}
    if(!(pl>0&&ph>=pl&&ql>0&&qh>=ql)||!std::isfinite(ph+qh))return out;
    if(qh-ql<1e-12*ql || (ql<=oldlo*(1+1e-12)&&qh>=oldhi*(1-1e-12)))break;
  }
  pl=std::max(pl,ql*std::exp(-sh)/(-std::expm1(-sh)));
  ph=std::min(ph,qh/sl+1);
  if(!(pl>0&&ph>=pl&&ql>0&&qh>=ql))return out;
  const double logbeta=gb2_detail::logbeta(p,q);
  const double val=std::log(power)-p*s-(q-1)*r-logbeta;
  const double gp=std::log1p(q/p)+trgamma_detail::log_minus_digamma(p)-
    trgamma_detail::log_minus_digamma(p+q)-s;
  const double gq=std::log1p(p/q)+trgamma_detail::log_minus_digamma(q)-
    trgamma_detail::log_minus_digamma(p+q)-r;
  const double scorepad=256*eps*(1+sh+rh+std::abs(std::log(p))+std::abs(std::log(q))+
    std::abs(std::log(p+q))+1/p+1/q);
  const double pad=1024*eps*(1+std::abs(std::log(power))+p*sh+std::abs(q-1)*rh+
    std::abs(logbeta))+(p+std::abs(q-1))*statpad;
  // A concave function is below every tangent plane. Maximize that plane
  // over a rectangle known to contain its unique conditional maximizer.
  const double tangent=std::max(gp*(pl-p),gp*(ph-p))+
    std::max(gq*(ql-q),gq*(qh-q))+
    scorepad*(std::max(std::abs(pl-p),std::abs(ph-p))+
      std::max(std::abs(ql-q),std::abs(qh-q)));
  out.shape1_lower=pl;out.shape1_upper=ph;out.shape2_lower=ql;out.shape2_upper=qh;
  out.loglik_value=n*val+jac;
  out.loglik_lower=n*(val-pad)+jac;
  out.loglik_upper=n*(val+pad+std::max(0.,tangent))+jac;
  out.enclosed=std::isfinite(out.loglik_lower)&&std::isfinite(out.loglik_upper)&&
    out.loglik_upper>=out.loglik_lower;
  return out;
}
inline GenbetaTailBounds tails(const std::vector<double>& y,double mean,double jac,
    double gamma_shape,double witness,int maxit) {
  GenbetaTailBounds out;
  const double n=static_cast<double>(y.size()),eps=std::numeric_limits<double>::epsilon();
  if(y.empty()||!(mean>0&&gamma_shape>0)||!std::isfinite(mean+gamma_shape))return out;
  genbeta_power_cert::Sum logs,vsum,second,gaps;
  double vmin=INFINITY;
  for(double z:y){const double v=z/mean;if(!(v>0)||!std::isfinite(v))return out;
    logs.add(std::log(v));vsum.add(v);second.add(v*v);vmin=std::min(vmin,v);}
  for(double z:y)gaps.add(z/mean-vmin);
  const double mu=static_cast<double>(vsum.value/n),c=std::log(mu)-static_cast<double>(logs.value/n),
    delta=static_cast<double>(gaps.value/n),m2=static_cast<double>(second.value/n);
  const double cpad=512*eps*(1+std::abs(c)+std::abs(std::log(mu)));
  if(!(c>cpad&&delta>256*eps))return out;
  // alpha(k)=log(k)-digamma(k) lies between 1/(2k) and 1/k.
  // The profiled gamma likelihood is strictly concave in k, so its tangent
  // over that entire interval bounds the unrestricted gamma maximum above.
  const double kl=1/(2*(c+cpad)),kh=1/(c-cpad),k=gamma_shape;
  const double base=k>=100?.5*std::log(k)-.5*std::log(6.2831853071795864769)-
    gamma_detail::stirlerr(k):k*std::log(k)-k-special::lgamma(k);
  const double gv=base-(k-1)*c-std::log(mu),score=trgamma_detail::log_minus_digamma(k)-c;
  const double gpad=1024*eps*(1+std::abs(base)+std::abs((k-1)*c)+
    std::abs(std::log(k)))*(1+std::max(std::abs(kl-k),std::abs(kh-k)))+
    cpad*(1+kh+k);
  const double gu=gv+std::max(score*(kl-k),score*(kh-k))+gpad;
  const double shift=jac-n*std::log(mean),Q=2/(c-cpad);
  const double gamma_total=n*gu+shift;
  out.gamma_loglik_upper=gamma_total+2048*eps*(1+std::abs(gamma_total)+std::abs(shift));
  out.small_power_domain=(c-cpad)/(2*mu);out.small_power_slope=std::max(1.,Q)*mu;
  // Center the gamma rate at h*(p+(q-1)/2), cancelling the linear term.
  // The resulting gamma-ratio and sinh remainders are uniformly quadratic.
  // cq=c-mean(T(hv))+T(h*mu), T(z)=log(sinh(z/2)/(z/2)).
  // Convexity and 0<=T(z)<=z^2/24 give c-h^2*m2/24<=cq<=c.
  // alpha(q)>1/(2q) and p>=q/expm1(s) imply
  // q>(exp(-s)-1/2)/cq. These give much tighter uniform q bounds.
  const double A=std::min({(c-cpad)/2,std::log(4./3),
    std::log1p(1/(4*(c+cpad))),std::sqrt(12*(c-cpad)*mu*mu/m2)})*(1-4096*eps),
    q0=(std::exp(-A)-.5)/(c+cpad),
    qmax=1/(c-cpad-A*A*m2/(24*mu*mu));
  if(A>0&&q0>0) {
    const double ratio=std::expm1(A)/A,factor=1-std::expm1(A)/(2*q0);
    out.small_quadratic_domain=A/mu;
    out.small_quadratic_coefficient=mu*mu*ratio*ratio/(8*q0*factor*factor)+
      std::max(0.,qmax-1)*m2/24;
    out.small_quadratic_coefficient*=1+4096*eps;
  }
  double L=out.small_power_domain;
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    double correction=out.small_power_slope*L;
    if(L<=out.small_quadratic_domain)correction=std::min(correction,out.small_quadratic_coefficient*L*L);
    const double upper=n*(gu+correction)+shift;
    const double margin=2048*eps*(1+std::abs(upper)+std::abs(witness));
    if(std::isfinite(upper)) {
      out.small_cutoff=L;out.small_upper=upper+2048*eps*(1+std::abs(upper));
      if(upper+margin<witness){out.small_excluded=true;break;}
    }
    if(!(witness>n*gu+shift))break; // unsigned bound cannot settle this face
    L*=.5;if(!(L>0))break;
  }
  const double C=std::log(n)+.57721566490153286061,
    W=-std::log(delta)-1;
  // Uniformly exclude an entire q strip around the attained exponential
  // ridge. This avoids treating the singular held-q=1 inner problem as an
  // isolated all-free maximum. The moving-endpoint face strictly beats it.
  const double Rmin=-std::log(-std::expm1(-vmin));
  double width=.5;
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    const double ql=1-width,qh=1+width;
    const double below=width*std::max(c+cpad+1+1/ql,Rmin-special::digamma(ql)),
      above=width*(std::log(2*qh+1)+.57721566490153286061);
    const double upper=-1+std::max(below,above),margin=4096*eps*(1+std::abs(W)+std::abs(upper));
    if(upper+margin<W) {
      out.ridge_shape2_lower=ql;out.ridge_shape2_upper=qh;
      const double value=n*upper+shift;
      out.ridge_loglik_upper=value+4096*eps*(1+std::abs(value));
      break;
    }
    width*=.5;if(!(1-width<1&&1+width>1))break;
  }
  double H=std::max(std::log(2.)/vmin,(std::log(4*n)+3)/delta);
  for(std::int64_t i=0;i<solver_budget(maxit);++i) {
    const double denom=H*delta-std::log(n)-1;
    const double B=1/denom,z=H*vmin,R=-std::log1p(-std::exp(-z));
    const double unsigned_upper=W+B*(std::log(4*n+1)+.57721566490153286061)+R;
    double upper=unsigned_upper;
    // A strict signed neighborhood below the moving-endpoint face. The
    // ratio used here decreases for every h>=H; it is not a pointwise test.
    const double rhs=std::log(H*mu)-C+1;
    const double margin=4096*eps*(1+std::abs(std::log(H))+std::abs(C)+std::abs(W));
    if(H>=1/vmin&&B<std::exp(-C-1)*(1-4096*eps)&&rhs>margin&&
       2*std::exp(1.)*H*mu*std::exp(-z)<rhs-margin)upper=W;
    const double total=n*upper+shift;
    if(denom>2&&std::isfinite(total)) {
      out.large_cutoff=H;out.large_upper=total+4096*eps*(1+std::abs(total));
      if(out.large_upper+4096*eps*(1+std::abs(witness))<witness){out.large_excluded=true;break;}
      if(upper==W)break;
    }
    H*=2;if(!std::isfinite(H))break;
  }
  return out;
}
} // namespace genbeta_allfree_bounds
} // namespace cppunivariate
