// Included after gb2_detail and the beta fitter.
#pragma once
namespace cppunivariate {
namespace gb2_concave_detail {

// Bernoulli KL from a mean with odds mu/(1-mu) to its logit shifted by d.
inline double logit_kl(double mu, double d) {
  const double e = std::expm1(d);
  return mu * trgamma_detail::expm1_minus(d) -
      beta_detail::u_minus_log1p(mu * e);
}

// With held power and scale the likelihood in the two shapes is strictly
// concave. Solve the two beta scores without rounding the data to probabilities.
inline Gb2Result concentrated_beta(span x, double a, double b, double tol, int maxit) {
  Gb2Result out{NAN,a,NAN,b,NAN,0,Gb2Status::SearchUnresolved};
  if(x.size()<2 || tol<1e-13)return out;
  const double anchor=x[0];
  const auto base_log=transformed_detail::add(transformed_detail::log_pair(anchor),
      transformed_detail::neg(transformed_detail::log_pair(b)));
  const auto base_pair=transformed_detail::mul({a,0},base_log);
  const double base=transformed_detail::value(base_pair);
  if(!(std::abs(base)<4))return out;
  std::vector<double> delta;
  transformed_detail::Sum mean, jacobian;
  const double n=x.size();
  for(double v:x) {
    const double shift=a*gb2_accuracy::log_ratio(v,anchor);
    if(!(std::abs(shift)<1e-4))return out;
    delta.push_back(shift);
    mean.push(shift/n);
    jacobian.push(std::log(v));
  }
  const double center=mean.get(), eta=base+center;
  const double mu0=1/(1+std::exp(-eta));
  transformed_detail::Sum divergence;
  for(double v:delta)divergence.push(logit_kl(mu0,v-center)/n);
  const double kl0=divergence.get();
  if(!(kl0>0 && std::isfinite(1/kl0)))return out;

  const double target=std::min(tol,1e-10);
  const double eta_width=std::min(target/32,std::sqrt(kl0*target)/32);
  const double tau_width=target/32;
  const double eps=std::numeric_limits<double>::epsilon();
  const double limit=std::log(std::numeric_limits<double>::max());
  std::int64_t work=certificate_budget(maxit);
  double tau=std::log(.5/kl0), mu=mu0, mc=1-mu0, kl=kl0;
  if(!(tau_width>4*eps*(1+std::abs(tau))))return out;
  const auto ray=[&](double gap) {
    if(work<=0)return false;
    const double h=eta+gap;
    mu=1/(1+std::exp(-h));
    mc=1/(1+std::exp(h));
    kl=kl0+logit_kl(mu,-gap);
    const auto score=[&](double z,double& f,double& df) {
      const double t=std::exp(z), p=mu*t, q=mc*t;
      f=(mu*beta_detail::psi_increment_rest(p,q)+mc*beta_detail::psi_increment_rest(q,p))/kl-1;
      df=-1; // The concentration score is asymptotic to 1/(2t).
      return std::isfinite(f);
    };
    const auto root=beta_detail::decreasing_root(score,tau,tau_width,tau_width,0,limit,work);
    work-=root.evaluations;
    out.iterations+=static_cast<int>(root.evaluations);
    tau=root.t;
    return root.ok;
  };
  const auto profile=[&](double gap,double& f,double& df) {
    if(!ray(gap) || work<=0)return false;
    const double t=std::exp(tau), p=mu*t, q=mc*t;
    f=beta_detail::psi_increment_rest(p,q)-beta_detail::psi_increment_rest(q,p)-gap;
    df=-1;
    return std::isfinite(f);
  };
  const auto root=beta_detail::decreasing_root(profile,0,eta_width,eta_width,-.01,.01,
      work,std::max(eta_width,kl0*32));
  if(!root.ok || work<=0 || !ray(root.t))return out;
  const double t=std::exp(tau), p=mu*t, q=mc*t;
  // Evaluate at the returned shape ratio. Even a one-ulp error in the
  // ratio can measurably lower the likelihood on a concentrated sample.
  const auto log_odds=transformed_detail::add(transformed_detail::log_pair(p),
      transformed_detail::neg(transformed_detail::log_pair(q)));
  const auto mean_logit=transformed_detail::add(base_pair,{center,0});
  const double gap=transformed_detail::value(transformed_detail::add(
      mean_logit,transformed_detail::neg(log_odds)));
  const double public_kl=kl0+logit_kl(p/(p+q),gap);
  const double ll=n*(std::log(a)-(p+q)*public_kl+.5*std::log(mu*mc*t/(2*cppunivariate::pi))+
      beta_detail::stirling_remainder(t)-beta_detail::stirling_remainder(p)-
      beta_detail::stirling_remainder(q))-jacobian.get();
  // Keep this reduction within the regime where shape rounding preserves
  // the fitted likelihood. For narrower samples the ordinary failure path
  // still handles a collapsed transform.
  const double loss=n*((p+q)*public_kl-t*kl);
  if(!(p>0 && q>0 && std::isfinite(ll) &&
       loss<=target*std::max(1.,std::abs(ll))))return out;
  out.shape1=q;out.shape3=p;out.logLik=ll;out.status=Gb2Status::Ok;
  return out;
}

} // namespace gb2_concave_detail
} // namespace cppunivariate
