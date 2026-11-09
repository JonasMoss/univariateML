#pragma once
#include "../beta/mle.hpp"
#include <vector>

namespace cppunivariate {
namespace betapr_detail {
// Near concentration, keep Y_i-Y_0 from the original observations. Forming
// Y_i first would discard relative information in its small dispersion.
inline BetaResult centered_shapes(span x, double scale, double tol, int maxit, bool& arithmetic_failure) {
  const double n=static_cast<double>(x.size()), anchor=x[0];
  const double ratio0=std::min(anchor,scale)/std::max(anchor,scale);
  const double y0=anchor<=scale?ratio0/(1+ratio0):1/(1+ratio0);
  const double yc=anchor<=scale?1/(1+ratio0):ratio0/(1+ratio0);
  std::vector<double> delta;
  double offset=0, spread=0, logs=0;
  for(double v:x) {
    const double relative_difference=v<=scale?
        ((v-anchor)/scale)/(1+v/scale):((v-anchor)/v)/(1+scale/v);
    const double d=relative_difference*yc;
    delta.push_back(d); offset+=d/n; spread=std::max(spread,std::abs(d));
    logs+=std::log(v/scale)-2*std::log1p(v/scale);
  }
  double t=1, kl=0, mu=y0, mc=yc;
  bool inner_ok=true;
  const auto profile=[&](double z,double& q,double& dq) {
    const double displacement=z*spread;
    mu=y0+displacement; mc=yc-displacement; kl=0;double gap=0;
    if(!(mu>0 && mc>0))return false;
    for(double dy:delta) {
      const double d=dy-displacement,u=d/mu,w=-d/mc;
      if(!(std::abs(u)<.1 && std::abs(w)<.1))return false;
      kl+=mu*beta_detail::u_minus_log1p(u)+mc*beta_detail::u_minus_log1p(w);
      gap+=std::log1p(u)-std::log1p(w);
    }
    kl/=n;gap/=n;
    if(!(kl>0)) return false;
    const auto fd=[&](double tau,double& f,double& df) {
      const double tt=std::exp(tau),a=mu*tt,b=mc*tt;
      f=-kl+mu*beta_detail::psi_increment_rest(a,b)+mc*beta_detail::psi_increment_rest(b,a);
      df=tt*(beta_detail::trigamma_minus_inv(tt)-mu*mu*beta_detail::trigamma_minus_inv(a)-mc*mc*beta_detail::trigamma_minus_inv(b));
      return std::isfinite(f);
    };
    const auto root=beta_detail::decreasing_root(fd,std::log(.5/kl),0,1e-15,
      std::log(std::numeric_limits<double>::min()),std::log(std::numeric_limits<double>::max()),solver_budget(maxit));
    if(!root.ok) {inner_ok=false;return false;}
    t=std::exp(root.t);
    const double a=mu*t,b=mc*t;
    q=gap+beta_detail::psi_increment_rest(a,b)-beta_detail::psi_increment_rest(b,a);
    dq=-spread/(mu*mc); // Newton guide; computed brackets decide.
    return std::isfinite(q);
  };
  const double width=std::min(tol,1e-12);
  const auto root=beta_detail::decreasing_root(profile,offset/spread,width,width,
    -y0/spread,yc/spread,solver_budget(maxit));
  double q=0,dq=0;
  if(!root.ok || !inner_ok || !profile(root.t,q,dq) || !std::isfinite(t))
    return {NAN,NAN,NAN,static_cast<int>(root.evaluations),BetaStatus::IterationFailure};
  // The log-X density drops the parameter-independent Jacobian. Its
  // optimum provides a unit-invariant check on coefficient restoration.
  const double ideal_normalizer=.5*std::log(mu*mc*t/(2*pi))+
      beta_detail::stirling_remainder(t)-beta_detail::stirling_remainder(mu*t)-
      beta_detail::stirling_remainder(mc*t);
  const double ideal_quality=n*(-t*kl+ideal_normalizer);
  const double b=mc*t;
  // Assemble the two shapes jointly. Independently rounding mu*t and mc*t
  // can perturb their ratio by an ulp of the absolute mean, much larger
  // than the centered mean-root error. Recover the desired odds from the
  // raw anchor ratio and the centered displacement, using division and
  // product residuals before the final rounding of alpha.
  const double ratio=anchor/scale;
  const double ratio_error=std::fma(-ratio,scale,anchor)/scale;
  const double offset_at_root=root.t*spread;
  const double adjustment=offset_at_root*(1+ratio)/(yc-offset_at_root);
  const double product=b*ratio;
  const double a=product+(std::fma(b,ratio,-product)+b*(ratio_error+adjustment));
  if(!(a>0 && std::isfinite(a) && b>0 && std::isfinite(b))) {
    arithmetic_failure=true;
    return {NAN,NAN,NAN,static_cast<int>(root.evaluations),BetaStatus::IterationFailure};
  }
  // Recover the mean displacement of the *returned* shapes. TwoProduct
  // residuals avoid cancellation in a*s-b*x0; power-of-two scaling avoids
  // overflow without changing the products' relative information.
  const int shape_exp=std::ilogb(std::max(a,b))+1;
  const int data_exp=std::ilogb(std::max(scale,anchor))+1;
  const double aa=std::ldexp(a,-shape_exp),bb=std::ldexp(b,-shape_exp);
  const double ss=std::ldexp(scale,-data_exp),xx=std::ldexp(anchor,-data_exp);
  const double p=aa*ss,r=bb*xx;
  const double displacement=((p-r)+(std::fma(aa,ss,-p)-std::fma(bb,xx,-r)))/((aa+bb)*(ss+xx));
  mu=aa/(aa+bb);mc=bb/(aa+bb);t=a+b;kl=0;
  for(double dy:delta) {
    const double d=dy-displacement;
    kl+=mu*beta_detail::u_minus_log1p(d/mu)+mc*beta_detail::u_minus_log1p(-d/mc);
  }
  kl/=n;
  const double normalizer=.5*std::log(mu*mc*t/(2*pi))+
    beta_detail::stirling_remainder(t)-beta_detail::stirling_remainder(a)-beta_detail::stirling_remainder(b);
  const double quality=n*(-t*kl+normalizer);
  const double ll=-n*(t*kl)-logs+n*normalizer;
  const double margin=128*std::numeric_limits<double>::epsilon()*
      (1+std::abs(ideal_quality)+n*(std::abs(t*kl)+std::abs(normalizer)));
  if(!std::isfinite(quality) || !std::isfinite(ll) ||
      ideal_quality-quality>tol*std::max(1.0,std::abs(ideal_quality))+margin) {
    arithmetic_failure=true;
    return {NAN,NAN,NAN,static_cast<int>(root.evaluations),BetaStatus::IterationFailure};
  }
  return {a,b,ll,static_cast<int>(root.evaluations),BetaStatus::Ok};
}
} // namespace betapr_detail
} // namespace cppunivariate
