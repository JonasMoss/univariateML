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

namespace cppunivariate {
// Two distinct values, minimum multiplicity k<n/2, no public holds.
// Convexity in inverse shape reduces the global comparison to lambda=1
// and the Gumbel face. See README for the reduction and arithmetic margin.
inline FrechetResult frechet_two_point_certificate(span x,double m,double M,
                                                   std::size_t k,double tol,int maxit) {
  maxit=static_cast<int>(solver_budget(maxit));
  FrechetResult out{1,NAN,NAN,NAN,0,FrechetStatus::NumericalFailure};
  const double n=static_cast<double>(x.size()),p=static_cast<double>(k)/n,q=1-p;
  const double d=M-m;
  if(!(d>0) || !std::isfinite(d))return out;
  const double h=p*(static_cast<double>(x.size()-2*k)/n)/(std::sqrt(p*q)+p);
  const double s=static_cast<double>(x.size()-2*k)/n;
  out.mu=m-d*h;out.sigma=d*s;
  out.logLik=frechet_loglik(x,1,out.mu,out.sigma);
  const double logd=std::log(d);
  const double l1=n*(std::log(s)-2*p*std::log(h)-2*q*std::log1p(h)-1-logd);
  const double margin=4096*std::numeric_limits<double>::epsilon()*n*
    (1+std::abs(logd)+std::abs(std::log(h)));
  if(std::isfinite(out.logLik)) {
    out.stop_detail.candidates.push_back({{1,out.mu,out.sigma},out.logLik-margin,out.logLik+margin});
    out.stop_detail.loglik_lower=out.logLik-margin;
  }
  out.stop_detail.loglik_upper=INFINITY;
  auto finite_fallback=[&]() {
    const double eps=256*std::numeric_limits<double>::epsilon();
    if(tol<4*eps || !std::isnormal(out.sigma) || !std::isnormal(d*h) ||
       !std::isfinite(out.mu) || !std::isfinite(out.logLik) ||
       eps*(std::abs(m)+d*h)>tol*.25*std::max(std::abs(out.mu),out.sigma) ||
       std::abs(out.logLik-l1)>margin)return false;
    out.status=FrechetStatus::Ok;out.certified=false;out.stop_detail={};return true;
  };
  // Strictly concave Gumbel objective; derivative is decreasing from +inf
  // to -q. This bracket and all refinements are solver-budget work.
  auto score=[&](double t) {
    const double e=q*std::exp(-t);
    return 1/t-q+e/(p+e);
  };
  auto value=[&](double t) {
    return n*(std::log(t)-q*t-std::log(p+q*std::exp(-t))-1-logd);
  };
  double lo=1/q,hi=2/q;
  int it=0;
  for(;it<maxit && score(hi)>=0;++it)hi*=2;
  if(it==maxit || !std::isfinite(hi)) {
    out.status=FrechetStatus::IterationFailure;return out;
  }
  for(;it<maxit;++it) {
    const double t=lo+(hi-lo)/2,derivative=score(t);
    if(!std::isfinite(derivative) || t==lo || t==hi)return out;
    const double l0=value(t);
    // Concavity bounds the root's value by its tangent on the bracket.
    const double upper=l0+n*std::abs(derivative)*(hi-lo)/2;
    out.stop_detail.loglik_lower=std::max(l1,l0)-margin;
    out.stop_detail.loglik_upper=std::max(l1,upper)+margin;
    out.iterations=it+1;
    if(l1>upper+2*margin) {
      // Propagate elementary arithmetic uncertainty into public coefficients.
      const double eps=256*std::numeric_limits<double>::epsilon();
      if(tol<4*eps || !std::isnormal(out.sigma) || !std::isnormal(d*h) ||
         !std::isfinite(out.mu) || !std::isfinite(out.logLik) ||
         eps*(std::abs(m)+d*h)>tol*.25*std::abs(out.mu) ||
         std::abs(out.logLik-l1)>margin) {finite_fallback();return out;}
      out.status=FrechetStatus::Ok;out.stop_detail={};return out;
    }
    if(l0>l1+2*margin && hi-lo<=std::min(tol,1e-8)*t) {
      const double scale=d/t;
      const double location=m-scale*std::log(p+q*std::exp(-t));
      const double evaluated=gev_loglik(x,location,scale,0);
      if(!std::isnormal(scale) || !std::isfinite(location) || !std::isfinite(evaluated) ||
         std::abs(evaluated-l0)>margin)return out;
      // The bounded supremum is approached but no finite shape attains it.
      out.status=FrechetStatus::GumbelLimit;
      out.stop_detail.candidates.clear();
      out.stop_detail.limit_coef={location,scale};
      out.stop_detail.limit_loglik=l0;
      return out;
    }
    if(hi-lo<=32*std::numeric_limits<double>::epsilon()*t) {
      out.stop_detail.comparison_tie=!out.stop_detail.candidates.empty() &&
        !(l0>l1+2*margin);
      if(out.stop_detail.comparison_tie)finite_fallback();
      return out;
    }
    if(derivative>0)lo=t;else hi=t;
  }
  out.status=FrechetStatus::IterationFailure;return out;
}
} // namespace cppunivariate

namespace cppunivariate {
namespace frechet_held_detail {
inline gev_held_detail::Evaluation evaluate(span data,gev_held_detail::Box b) {
  double D=0,k=0;for(double v:data) {D=std::max(D,v);if(v==0)++k;}
  const double n=static_cast<double>(data.size()),c=std::asinh(D/2);
  // A uniform enclosure of the unique inner root on this whole shape slab.
  const double lower=(std::log(k/n)-std::log1p(1/b.x.lo))/b.x.lo;
  const double error=128*std::numeric_limits<double>::epsilon()*(1+std::abs(lower));
  if(b.y.lo!=b.y.hi && b.y.hi<lower-error)return {{},-INFINITY,false};
  double upper=INFINITY;
  if(b.x.lo*c>=std::log1p(1/b.x.lo))
    upper=std::log1p(b.x.hi)+std::max(-(b.x.lo+1)*c,c-1-b.x.lo*b.x.lo*c*c/2)/n;

  const auto a=gev_held_detail::variable(b.x,0),u=gev_held_detail::variable(b.y,1);
  const auto e=gev_held_detail::exponential(u);
  gev_held_detail::Accumulator sum;
  for(double v:data) {
    const auto z=gev_held_detail::logarithm(gev_held_detail::add(gev_held_detail::constant(v),e));
    const auto term=gev_held_detail::add(gev_held_detail::mul(gev_held_detail::add(a,gev_held_detail::constant(1)),z),
      gev_held_detail::exponential(gev_held_detail::scale(gev_held_detail::mul(a,z),-1)));
    sum.add(gev_held_detail::scale(term,-1));
  }
  const auto result=gev_held_detail::margin(gev_held_detail::add(sum.mean(data.size()),gev_held_detail::logarithm(a)));
  return {result,std::isfinite(result.v.hi)?std::min(upper,result.v.hi):upper,gev_held_detail::finite(result)};
}
}
// Public scale is one in these minimum-pivot coordinates. The fixed-shape
// location root lies in k/(2n)<=e<=1 for every lambda>=1. The shape tail
// envelope below excludes lambda=infinity without a free-scale Gumbel face.
inline FrechetResult frechet_held_scale_certificate(span data,double tol,int maxit,FrechetResult fallback) {
  if(fallback.status!=FrechetStatus::Ok||!std::isfinite(fallback.logLik)||
     tol<1024*std::numeric_limits<double>::epsilon())return fallback;
  const double n=static_cast<double>(data.size());
  double D=0,k=0;for(double v:data) {D=std::max(D,v);if(v==0)++k;}
  const double c=std::asinh(D/2);if(!(c>0)||!std::isfinite(c))return fallback;
  double upper=std::max({2.,2*n/c,2*std::sqrt(n)/c});bool tail=false;
  for(int it=0;it<maxit;++it) {
    const double t=upper*c;
    const double envelope=n*std::log1p(upper)+std::max(-(upper+1)*c,c-1-t*t/2);
    if(envelope<fallback.logLik-1e-8*(n+std::abs(fallback.logLik))) {tail=true;break;}
    upper*=2;if(!std::isfinite(upper))break;
  }
  if(!tail)return fallback;
  // Strictly outward bounds; neither location edge can be a maximum.
  const gev_held_detail::Box domain{{1,upper},{std::log(k/(2*n))-1e-8,1e-8}};
  auto eval=[&](gev_held_detail::Box b) {return frechet_held_detail::evaluate(data,b);};
  auto precise=[&](gev_held_detail::Box b,double t) {
    return b.x.hi-b.x.lo<=t*.25*(b.x.lo+b.x.hi)/2&&std::expm1(b.y.hi-b.y.lo)<=t*.25;
  };
  const auto result=gev_held_detail::cover(domain,fallback.lambda,std::log(-fallback.mu),std::min(tol,.01),maxit,eval,precise,true,{},NAN,std::log(k/n));
  if(result.contradicted) {
    fallback.lambda=result.competitor.x.lo;fallback.mu=-std::exp(result.competitor.y.lo);
    fallback.logLik=frechet_loglik(data,fallback.lambda,fallback.mu,1);
    fallback.status=FrechetStatus::SearchContradicted;return fallback;
  }
  if(!result.certified)return fallback;
  fallback.lambda=(result.root.x.lo+result.root.x.hi)/2;
  fallback.mu=-std::exp((result.root.y.lo+result.root.y.hi)/2);
  fallback.logLik=frechet_loglik(data,fallback.lambda,fallback.mu,1);
  fallback.certified=true;return fallback;
}
} // namespace cppunivariate
