#pragma once
// Internal building blocks, not a global shape solver. Included in namespace
// cppunivariate after GedResult. Exact-arithmetic inequalities and the practical
// comparison margin are documented in README.md.
namespace ged_shape_detail {
inline constexpr double euler = 0.577215664901532860606512090082402431;
inline constexpr double zeta2 = 1.644934066848226436472415166646025189;
inline constexpr double log2 = 0.693147180559945309417232121458176568;
inline constexpr double log3 = 1.098612288668109691395245236922525705;
inline double margin(double size,double n) {
  return 4096*std::numeric_limits<double>::epsilon()*(1+size)*(1+n*std::numeric_limits<double>::epsilon());
}
// Likelihood sums can cancel: include the magnitudes of the normalizer and
// scale terms, not just the magnitude of their signed total.
inline double likelihood_margin(double magnitude,double n,double sd=1,double nu=1) {
  double components=5+2*std::abs(std::log(sd));
  if(nu<1)components+=std::abs(special::lgamma(1/nu))+std::abs(special::lgamma(3/nu));
  return margin(magnitude+n*components,n);
}
struct Sum {
  double value=0,correction=0;
  void add(double term) {
    const double y=term-correction,t=value+y;
    correction=(t-value)-y;value=t;
  }
};
struct Moment {
  double log_lower = -INFINITY, log_upper = INFINITY;
  int iterations = 0;
  bool valid = false;
};
// Enclose log(min_m mean |x-m|^p) / p, or the held-location moment.
// Convex supporting lines give a lower bound even if the location solve stops.
inline Moment moment(span x,double p,double held_mean,int maxit) {
  Moment out;
  if(x.empty() || !(p>=1) || !std::isfinite(p) ||
      (!std::isnan(held_mean) && !std::isfinite(held_mean)))return out;
  double xmin=x[0],xmax=x[0];
  for(double v:x){if(!std::isfinite(v))return out;xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  const bool held=std::isfinite(held_mean);
  const double origin=held?held_mean:xmin/2+xmax/2;
  const double unit=std::max(std::abs(xmin-origin),std::abs(xmax-origin));
  if(unit<std::numeric_limits<double>::min() || !std::isfinite(unit))return out;
  std::vector<double> y;
  for(double v:x){const double z=(v-origin)/unit;if(!std::isfinite(z))return out;y.push_back(z);}
  double a=held?0:(xmin-origin)/unit,b=held?0:(xmax-origin)/unit;
  if(!held && p==1) {
    std::sort(y.begin(),y.end());
    a=y[(y.size()-1)/2];b=y[y.size()/2];
  } else if(!held) {
    for(std::int64_t k=0;k<solver_budget(maxit);++k) {
      const double c=a/2+b/2;
      double radius=0;for(double v:y)radius=std::max(radius,std::abs(v-c));
      Sum score,absolute;
      for(double v:y)if(v!=c) {
        const double term=std::pow(std::abs(v-c)/radius,p-1);
        score.add(c>v?term:-term);absolute.add(term);
      }
      ++out.iterations;
      const double pad=margin(absolute.value,x.size())*(1+p);
      if(score.value>pad)b=c;
      else if(score.value< -pad)a=c;
      else break; // The supporting-line enclosure below remains valid.
      if(b-a<=32*std::numeric_limits<double>::epsilon())break;
    }
  }
  const double c=a/2+b/2;
  double radius=0;for(double v:y)radius=std::max(radius,std::abs(v-c));
  if(!(radius>0) || !std::isfinite(radius))return out;
  const double n=static_cast<double>(x.size());
  Sum values,gradients,absolutes,distances;
  for(double v:y) {
    const double d=std::abs(v-c)/radius;
    values.add(std::pow(d,p));
    if(v!=c) {
      const double g=std::pow(d,p-1);
      gradients.add(c>v?g:-g);absolutes.add(g);
    }
    // Lower enclosure obtained by minimizing each residual independently.
    const double min_distance=v<a?a-v:(v>b?v-b:0);
    distances.add(std::pow(min_distance/radius,p));
  }
  const double value=values.value/n,gradient=gradients.value/n,absolute=absolutes.value/n,distance_lower=distances.value/n;
  const double error=margin(value+distance_lower+absolute,n)*(1+p);
  // For p=1 every median is optimal; the kink does not need a derivative.
  double lower=(held || p==1)?value-error:std::max(distance_lower-error,
      value-error-p*(std::abs(gradient)+error)*(b-a)/radius);
  lower=std::max(0.,lower);
  const double scale=std::log(unit)+std::log(radius);
  out.log_lower=lower>0?scale+std::log(lower)/p:-INFINITY;
  out.log_upper=scale+std::log(value+error)/p;
  const double log_error=margin(std::abs(scale)+std::abs(out.log_upper),n)*(1+p);
  out.log_lower-=log_error;out.log_upper+=log_error;
  out.valid=std::isfinite(out.log_upper) && !std::isnan(out.log_lower);
  return out;
}
// Free-scale profile: ell(p)/n = h(p) - log R_p.
inline double h(double p) {
  return -log2-special::lgamma(1+1/p)-(1+std::log(p))/p;
}
// min_{u in R} k |1+u|^p + l |1-u|^p. Increasing in p>=1,
// with limit 2 sqrt(k l); stable also when k/l is near one or p is large.
inline double endpoint_power(double k,double l,double p) {
  if(!(k>0 && l>0 && p>=1) || !std::isfinite(k+l+p))return NAN;
  if(p==1)return 2*std::min(k,l);
  const double s=p-1,z=std::abs(std::log(k)-std::log(l))/(2*s);
  const double logcosh=z<.5?std::log1p(2*std::pow(std::sinh(z/2),2)):
      z+std::log1p(std::exp(-2*z))-log2;
  return std::exp(log2+(std::log(k)+std::log(l))/2-s*logcosh);
}
struct Bound {
  double upper = INFINITY;
  int iterations = 0;
  bool valid = false;
  // An endpoint comparison within roundoff cannot justify a discontinuous
  // infinite-shape penalty. In that case the bound drops that penalty.
  bool endpoint_uncertain = false;
};
inline bool input(span x,double mean,double sd,double p) {
  if(x.empty() || !(p>=1) || !std::isfinite(p) ||
      (!std::isnan(mean) && !std::isfinite(mean)) ||
      (!std::isnan(sd) && (!(sd>0) || !std::isfinite(sd))))return false;
  for(double v:x)if(!std::isfinite(v))return false;
  return true;
}
// Whole half-line nu>=p; holds are NaN when free. Finite means/sds remain
// fixed throughout the bound. No evaluated candidate is needed.
inline Bound tail(span x,double p,double mean,double sd,int maxit) {
  Bound out;
  if(!input(x,mean,sd,p))return out;
  const double n=static_cast<double>(x.size());
  if(std::isnan(sd)) {
    const auto m=moment(x,p,mean,maxit);out.iterations=m.iterations;
    if(!m.valid)return out;
    double lower=m.log_lower;
    double radius=0;
    if(std::isfinite(mean)) {
      for(double v:x)radius=std::max(radius,std::abs(v-mean));
    } else {
      double lo=x[0],hi=x[0];
      for(double v:x){lo=std::min(lo,v);hi=std::max(hi,v);}
      radius=hi/2-lo/2;
    }
    // Every admissible location has an extreme residual of at least radius.
    // This supplies the explicit uniform remainder n*log(n)/p even when a
    // coarse location bracket makes the supporting-line bound uninformative.
    if(radius>0 && std::isfinite(radius)) {
      const double extreme=std::log(radius)-std::log(n)/p;
      lower=std::max(lower,extreme-margin(std::abs(extreme),n)*(1+p));
    }
    out.upper=n*(-log2-lower);
    if(radius>=std::numeric_limits<double>::min() && std::isfinite(radius)) {
      // log Gamma(1+t)>=-gamma*t and R_v>=radius*n^(-1/v)
      // give ell(v)<=-n*log(2*radius) for every
      // v>=p>=n*exp(gamma-1). This closes a uniform-winning tail too.
      const double comparison=std::log(p)+1-euler-std::log(n);
      if(comparison>margin(std::log(p)+std::log(n),n))
        out.upper=std::min(out.upper,-n*(log2+std::log(radius))+
            margin(n*(1+std::abs(std::log(radius))),n)*(1+p));
    }
  } else {
    // log b(nu) <= log(sqrt(3)*sd)+gamma/nu and
    // log A(nu) <= -log(2*sqrt(3)*sd)+(9/4)*zeta(2)/nu^2.
    const double logr=std::log(sd)+log3/2;
    double penalty=0;
    if(std::isfinite(mean)) {
      Sum terms;
      for(double v:x)if(v!=mean) {
        const double z=std::log(std::abs(v-mean))-logr;
        const double error=margin(std::abs(logr)+std::abs(z),n);
        if(z>error)terms.add(std::exp(p*(z-error)));
        else if(z>=-error)out.endpoint_uncertain=true;
      }
      penalty=terms.value;
    } else {
      double lo=x[0],hi=x[0],k=0,l=0;
      for(double v:x){lo=std::min(lo,v);hi=std::max(hi,v);}
      for(double v:x){k+=v==lo;l+=v==hi;}
      if(hi>lo) {
        const double half=hi/2-lo/2;
        const double z=std::log(half)-logr;
        const double error=margin(std::abs(logr)+std::abs(z),n);
        if(z>error)penalty=std::exp(p*(z-error))*endpoint_power(k,l,p);
        else if(z>=-error)out.endpoint_uncertain=true;
      }
    }
    // Overflow is not used as proof: an arithmetic failure gives no bound.
    if(!std::isfinite(penalty))return out;
    out.upper=n*(-log2-logr+(2.25*zeta2/p)/p)-std::exp(-euler)*penalty;
  }
  if(std::isnan(out.upper) || out.upper== -INFINITY)return out;
  if(std::isfinite(out.upper))out.upper+=likelihood_margin(std::abs(out.upper),n,std::isnan(sd)?1:sd)*(1+p);
  out.valid=true;return out;
}
// Entire finite interval [p,q], including the Laplace seam p=1. The moment
// lower bound is valid without assuming convergence of a numerical minimizer.
inline Bound interval(span x,double p,double q,double mean,double sd,int maxit) {
  Bound out;
  if(!input(x,mean,sd,p) || !(q>=p) || !std::isfinite(q))return out;
  const auto m=moment(x,p,mean,maxit);out.iterations=m.iterations;
  if(!m.valid)return out;
  const double n=static_cast<double>(x.size());
  if(std::isnan(sd))out.upper=n*(h(q)-m.log_lower);
  else {
    const double t=1/p;
    // |d log b / dt| <= 3 on 0<=t<=1; unlike a tail bound this
    // enclosure shrinks to the exact b(p) as q decreases to p.
    const double logb=std::log(sd)+log3/2+
        (special::lgamma(1+t)-special::lgamma(1+3*t))/2;
    const double logb_upper=logb+3*(t-1/q)+margin(std::abs(logb),n)*(1+q);
    const double logpeak=-log2-std::log(sd)-log3/2-
        1.5*special::lgamma(1+t)+.5*special::lgamma(1+3*t);
    // z^v >= z^p - (q-p)/(e*p), all z>=0 and p<=v<=q.
    const double penalty=std::exp(p*(m.log_lower-logb_upper));
    if(!std::isfinite(penalty))return out;
    out.upper=n*(logpeak-std::max(0.,penalty-(q-p)/(std::exp(1.)*p)));
  }
  if(std::isnan(out.upper) || out.upper== -INFINITY)return out;
  if(std::isfinite(out.upper))out.upper+=likelihood_margin(std::abs(out.upper),n,std::isnan(sd)?1:sd)*(1+q);
  out.valid=true;return out;
}
} // namespace ged_shape_detail
