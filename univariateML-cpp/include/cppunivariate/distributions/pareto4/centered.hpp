#pragma once
// Implementation included by mle.hpp after Pareto4Result is declared.

namespace cppunivariate {
namespace pareto4_detail {
// Positive-left/negative-right unique-root bracket. A capped bisection is
// accepted only if its log-coordinate width meets the requested tolerance.
template<class Score> inline double centered_root(Score score,double tol,int maxit) {
  double lo=-1,hi=1;
  for(int i=0;i<maxit && score(lo)<0;++i)lo*=2;
  for(int i=0;i<maxit && score(hi)>0;++i)hi*=2;
  if(!(score(lo)>=0 && score(hi)<=0))return NAN;
  for(int i=0;i<maxit;++i) {
    const double mid=lo+(hi-lo)/2;
    if(mid==lo || mid==hi)return mid;
    const double g=score(mid);
    if(!std::isfinite(g))return NAN;
    if(g==0)return mid;
    if(g>0)lo=mid;else hi=mid;
  }
  return hi-lo<=std::min(1e-12,tol/8)?lo+(hi-lo)/2:NAN;
}

// Fixed first shape: strict concavity in (power, power*log(scale)) gives
// a unique root. Standardizing centered logs preserves tiny raw spacings.
inline Pareto4Result centered_shape(span x,double min,double a,double tol,int maxit) {
  const double anchor=x[0]-min,n=static_cast<double>(x.size());
  // Error-free subtraction residual: the centered spacings refer to the
  // exact excess, even when its absolute anchor needs two doubles.
  const double virtual_b=anchor-x[0];
  const double residual=(x[0]-(anchor-virtual_b))+(-min-virtual_b);
  if(!std::isfinite(residual))
    return {a,NAN,NAN,min,NAN,0,BurrStatus::NumericalFailure};
  std::vector<double> logs; double unit=0;
  for(double v:x) {
    const double l=std::log1p((v-x[0])/anchor);
    logs.push_back(l);unit=std::max(unit,std::abs(l));
  }
  if(!(unit>0))return {a,NAN,NAN,min,NAN,0,BurrStatus::Unbounded};
  for(double& l:logs)l/=unit;
  const auto location=[&](double k) {
    return centered_root([&](double c) {
      double g=-n;
      for(double l:logs)g+=(a+1)*burr_hold_detail::sigmoid(k*l-c);
      return g;
    },tol,maxit);
  };
  const double root=centered_root([&](double z) {
    const double k=std::exp(z),c=location(k);double g=n;
    for(double l:logs)g+=k*l*(1-(a+1)*burr_hold_detail::sigmoid(k*l-c));
    return g;
  },tol,maxit);
  const double k=std::exp(root),c=location(k),b=k/unit;
  const double expansion=std::expm1(c/b);
  const double scale=residual==0?std::fma(anchor,expansion,anchor):
      anchor+std::fma(anchor,expansion,residual);
  if(!(b>0 && scale>0 && std::isfinite(b) && std::isfinite(scale)))
    return {a,NAN,NAN,min,NAN,0,BurrStatus::IterationFailure};
  // Re-evaluate at the represented scale, rather than at the latent root.
  // Differences from x[0] remain centered until after division by scale.
  const double shift=((anchor-scale)+residual)/scale;
  const double log_anchor=std::log(anchor)+std::log1p(residual/anchor);
  double ll=n*(std::log(a)+std::log(b)),quality=ll,ideal=ll,magnitude=std::abs(ll);
  for(std::size_t i=0;i<x.size();++i) {
    const double u=b*std::log1p(shift+(x[i]-x[0])/scale);
    const double tail=burr_softplus(-u)+a*burr_softplus(u);
    const double term=log_anchor+unit*logs[i]+tail;
    const double optimal_u=k*logs[i]-c;
    const double ideal_tail=burr_softplus(-optimal_u)+a*burr_softplus(optimal_u);
    ll-=term;quality-=tail;ideal-=ideal_tail;magnitude+=std::abs(tail)+std::abs(ideal_tail);
  }
  // Compare log-excess densities, so a change of measurement units cannot
  // change the criterion through a parameter-independent Jacobian.
  // A separated likelihood loss proves that restoration is inadequate;
  // this is an arithmetic failure, not an unresolved certificate comparison.
  const double margin=128*std::numeric_limits<double>::epsilon()*(1+magnitude);
  if(!std::isfinite(ideal) || ideal-quality>tol*std::max(1.0,std::abs(ideal))+margin)
    return {a,NAN,NAN,min,NAN,0,BurrStatus::NumericalFailure};
  return {a,b,scale,min,ll,0,std::isfinite(ll)?BurrStatus::Ok:BurrStatus::NumericalFailure};
}
}
} // namespace cppunivariate
