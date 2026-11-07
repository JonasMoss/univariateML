#pragma once
// Included after the result, likelihood and scale definitions in mle.hpp.
namespace cppunivariate {
// Both fixed-shape location problems have a strictly decreasing ratio score.
// The profiled-scale ratio proof, arithmetic contract and limits are in README.
inline FrechetResult frechet_fixed_shape_certificate(span g,double a,bool held_scale,
                                                       double tol,int maxit) {
  FrechetResult out{a,NAN,NAN,NAN,0,FrechetStatus::NumericalFailure};
  const double n=static_cast<double>(g.size());
  const double margin=256*std::numeric_limits<double>::epsilon()*n*(1+a)*(1+a);
  auto finish=[&](double u) {
    const double e=std::exp(u);
    out.mu=-e;out.sigma=held_scale ? 1 : frechet_scale(g,a,-e);
    out.logLik=frechet_loglik(g,a,out.mu,out.sigma);
    // Large held powers amplify rounded log distances into a fake point fit.
    // Reject loss of the per-observation likelihood precision used on restore.
    double magnitude=1+std::abs(std::log(out.sigma));
    for(double v:g)magnitude=std::max(magnitude,1+std::abs(std::log(v+e))+std::abs(std::log(out.sigma)));
    const double loss=256*std::numeric_limits<double>::epsilon()*a*magnitude;
    if(!(loss<=1e-7))out.status=FrechetStatus::NumericalFailure;
    if(!(e>0) || !(out.sigma>0) || !std::isfinite(out.sigma) || !std::isfinite(out.logLik)) out.status=FrechetStatus::NumericalFailure;
    return out;
  };
  // A finite witness is retained even if the subsequent bracket exhausts.
  finish(0);
  auto stopped=[&](bool budget) {
    out.status=budget ? FrechetStatus::IterationFailure : FrechetStatus::NumericalFailure;
    // Both loops count solver iterations; exhaustion is non-convergence.
    out.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(out.logLik) && std::isfinite(margin)) {
      out.stop_detail.loglik_lower=out.logLik-margin;
      out.stop_detail.candidates.push_back({{a,out.mu,out.sigma},out.logLik-margin,out.logLik+margin});
    }
    return out;
  };
  auto score=[&](double u) {
    const double e=std::exp(u);
    if(!(e>0) || !std::isfinite(e))return std::numeric_limits<double>::quiet_NaN();
    double A=0,B=0,C=0;
    for(double v:g) {
      const double r=e/(v+e),p=std::pow(r,a);
      A+=p*r;B+=p;C+=r;
    }
    return (held_scale ? -a*u+std::log(A/C) : std::log(n/C)+std::log(A/B))-
           std::log1p(1/a);
  };
  double lo=-1,hi=1,sl=score(lo),sh=score(hi);
  int j=0;
  for(;j<maxit && !(sl>0 && sh<0);++j) {
    if(!(sl>0)){lo*=2;sl=score(lo);}
    if(!(sh<0)){hi*=2;sh=score(hi);}
    if(!std::isfinite(sl) || !std::isfinite(sh))return stopped(false);
  }
  if(j==maxit)return stopped(true);
  for(j=0;j<maxit;++j) {
    const double mid=lo+(hi-lo)/2,s=score(mid);
    if(!std::isfinite(s))return stopped(false);
    if(mid==lo || mid==hi)return stopped(false);
    if(s>0)lo=mid;else hi=mid;
    if(hi-lo<=tol*(1+std::abs(mid))) {
      out.status=FrechetStatus::Ok;out.iterations=j+1;return finish((lo+hi)/2);
    }
  }
  return stopped(true);
}
} // namespace cppunivariate
