#pragma once
#include "../../core/span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
enum class HuberStatus { Ok, InvalidData, UnboundedLikelihood, IterationFailure, NormalLimit, LaplaceLimit };
struct HuberResult {
  double mu, sigma, epsilon, logLik;
  int iterations;
  HuberStatus status;
};
inline double huber_rho(double z, double epsilon) {
  const double a = std::abs(z);
  return a <= epsilon ? z*z/2 : epsilon*(a-epsilon/2);
}
inline double huber_z(double epsilon) {
  return std::sqrt(2*std::acos(-1.0))*std::erf(epsilon/std::sqrt(2.0)) +
      2*std::exp(-epsilon*epsilon/2)/epsilon;
}
inline double huber_psi(double z, double epsilon) {
  return std::max(-epsilon, std::min(epsilon, z));
}
// Sorted, normalized data. The only nontrivial zero-score interval is the
// gap between the central observations, eroded by epsilon*sigma at each end.
inline double huber_location(const std::vector<double>& y, double sigma,
                             double epsilon) {
  const std::size_t n = y.size();
  if (n % 2 == 0 && (y[n/2]-y[n/2-1])/2 >= epsilon*sigma)
    return y[n/2-1]/2 + y[n/2]/2;
  double lo = y.front(), hi = y.back();
  for (int j=0; j<100; ++j) {
    const double m = lo/2+hi/2;
    if (m == lo || m == hi) break;
    double score=0;
    for (double v:y) score += huber_psi((v-m)/sigma, epsilon);
    if (score == 0) return m;
    if (score > 0) lo=m; else hi=m;
  }
  return lo/2+hi/2;
}
// Maximize mean log likelihood in inverse scale eta and scaled location h.
// Centering/scaling above keeps the 2 by 2 information matrix well conditioned.
inline bool huber_newton(const std::vector<double>& y, double epsilon,
    bool fm, bool fs, double tol, int maxit, double& m, double& s, int& iterations) {
  const double n=static_cast<double>(y.size());
  double eta=1/s, h=m*eta;
  auto objective = [&](double a, double b) {
    double loss=0;
    for (double v:y) loss+=huber_rho(a*v-b,epsilon);
    return std::log(a)-loss/n;
  };
  for (int j=0;j<maxit;++j) {
    double gp=0, gh=0, aa=0, ab=0, bb=0;
    for (double v:y) {
      const double w=fm ? v-m : v;
      const double z=fm ? eta*w : eta*v-h;
      const double psi=huber_psi(z,epsilon);
      const double d=std::abs(z)<epsilon ? 1 : (std::abs(z)==epsilon ? .5 : 0);
      gp+=w*psi; gh+=psi;
      aa+=d*w*w; ab-=d*w; bb+=d;
    }
    const double g1=1/eta-gp/n, g2=gh/n;
    // Dimensionless scale and location scores; no objective/step stopping.
    if ((fs || std::abs(eta*g1-(fm ? 0 : h*g2))<=tol) &&
        (fm || std::abs(g2)<=tol)) {
      s=1/eta;
      if (!fm) m=h/eta;
      iterations=j;
      return true;
    }
    aa=1/(eta*eta)+aa/n; ab/=n; bb/=n;
    double de=0, dh=0;
    if (fm) de=g1/aa;
    else if (fs) {
      if (!(bb>0)) return false;
      dh=g2/bb;
    } else {
      const double det=aa*bb-ab*ab;
      if (!(det>0)) return false;
      de=(bb*g1-ab*g2)/det; dh=(aa*g2-ab*g1)/det;
    }
    const double slope=g1*de+(fm ? 0 : g2*dh);
    const double old=objective(eta,h);
    bool accepted=false;
    double step=1;
    for (int k=0;k<60;++k,step*=.5) {
      const double a=eta+step*de;
      const double b=fm ? m*a : h+step*dh;
      if (a>0 && std::isfinite(a) && std::isfinite(b)) {
        const double value=objective(a,b);
        // Permit rounding at machine precision while still requiring scores.
        if (value>=old+1e-4*step*slope-
            8*std::numeric_limits<double>::epsilon()*(1+std::abs(old))) {
          eta=a; h=b; accepted=true; break;
        }
      }
    }
    if (!accepted) return false;
  }
  return false;
}
inline HuberResult huber_fixed_mle(span x, double tol=1e-10, int maxit=200,
    double epsilon=1.345,
    double mu=std::numeric_limits<double>::quiet_NaN(),
    double sigma=std::numeric_limits<double>::quiet_NaN(), bool sorted=false) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  HuberResult r{mu,sigma,epsilon,nan,0,HuberStatus::InvalidData};
  const bool fm=!std::isnan(mu), fs=!std::isnan(sigma);
  if (x.size()==0 || !(epsilon>0) || !std::isfinite(epsilon) ||
      !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fm && !std::isfinite(mu)) ||
      (fs && (!(sigma>0) || !std::isfinite(sigma)))) return r;
  std::vector<double> y;
  y.reserve(x.size());
  for (double v:x) { if (!std::isfinite(v)) return r; y.push_back(v); }
  if (!sorted) std::sort(y.begin(),y.end());
  const double center=y[y.size()/2];
  double unit=std::max(std::abs(y.front()-center),std::abs(y.back()-center));
  if (fm) unit=std::max(unit,std::abs(mu-center));
  if (unit==0 && !fs) { r.status=HuberStatus::UnboundedLikelihood; return r; }
  if (unit==0) unit=sigma;
  if (!std::isfinite(unit)) return r;
  for (double& v:y) v=(v-center)/unit;
  double m=fm ? (mu-center)/unit : 0;
  double s=fs ? sigma/unit : 1;
  const double n=static_cast<double>(y.size());
  if (!fs) {
    // Median/MAD start; a zero MAD (ties) falls back to the sample range.
    std::vector<double> deviations;
    deviations.reserve(y.size());
    for (double v:y) deviations.push_back(std::abs(v));
    std::nth_element(deviations.begin(),deviations.begin()+y.size()/2,deviations.end());
    s=deviations[y.size()/2]*1.4826;
    if (!(s>0)) s=1;
  }
  const double initial_m=m, initial_s=s;
  bool converged=(fm && fs) || huber_newton(y,epsilon,fm,fs,tol,maxit,m,s,r.iterations);
  if (!converged) {
    m=initial_m; s=initial_s;
  }
  if (!converged && !fs) {
    double lo=0, hi=1/s;
    auto score = [&](double eta) {
      const double loc=fm ? m : huber_location(y,1/eta,epsilon);
      double total=0;
      for (double v:y) { const double z=(v-loc)*eta; total+=z*huber_psi(z,epsilon); }
      return total/n-1;
    };
    int brackets=0;
    while (score(hi)<0 && brackets++<1024) hi*=2;
    if (!std::isfinite(hi)) { r.status=HuberStatus::IterationFailure; return r; }
    r.status=HuberStatus::IterationFailure;
    for (int j=0;j<maxit;++j) {
      const double eta=lo/2+hi/2;
      const double g=score(eta);
      r.iterations=j+1;
      s=1/eta;
      if (std::abs(g)<=tol) { r.status=HuberStatus::Ok; break; }
      if (g>0) hi=eta; else lo=eta;
    }
  } else r.status=HuberStatus::Ok;
  if (!fm) {
    // Preserve the exact midpoint convention, also when Newton found another
    // point in the same flat interval. Only the fallback needs a location solve.
    if (!converged || (y.size()%2==0 &&
        (y[y.size()/2]-y[y.size()/2-1])/2>=epsilon*s))
      m=huber_location(y,s,epsilon);
  }
  r.mu=fm ? mu : center+unit*m;
  r.sigma=fs ? sigma : unit*s;
  r.logLik=-n*(std::log(r.sigma)+std::log(huber_z(epsilon)));
  for (double v:y) r.logLik-=huber_rho((v-m)/s,epsilon);
  return r;
}

// NaN epsilon requests estimation; the public default remains held at 1.345.
inline HuberResult huber_mle(span x, double tol=1e-10, int maxit=200,
    double epsilon=1.345,
    double mu=std::numeric_limits<double>::quiet_NaN(),
    double sigma=std::numeric_limits<double>::quiet_NaN()) {
  if (!std::isnan(epsilon)) return huber_fixed_mle(x,tol,maxit,epsilon,mu,sigma);
  // Validate through the fixed solver before computing either face.
  HuberResult best=huber_fixed_mle(x,tol,maxit,1.345,mu,sigma);
  if (best.status==HuberStatus::InvalidData ||
      best.status==HuberStatus::UnboundedLikelihood) return best;
  std::vector<double> y(x.begin(),x.end());
  std::sort(y.begin(),y.end());
  const span data{y.data(),y.size()};
  const bool fm=!std::isnan(mu), fs=!std::isnan(sigma);
  const double n=static_cast<double>(y.size());
  // Analytic Gaussian face under the same held-coordinate constraints.
  long double total=0;
  for (double v:y) total+=static_cast<long double>(v)-y[0];
  const double nm=fm ? mu : static_cast<double>(y[0]+total/n);
  long double ss=0;
  for (double v:y) { const long double d=static_cast<long double>(v)-nm; ss+=d*d; }
  const double ns=fs ? sigma : static_cast<double>(std::sqrt(ss/n));
  const double normal=-n*(std::log(ns)+.5*std::log(2*std::acos(-1.0)))-
      static_cast<double>(ss/(2*static_cast<long double>(ns)*ns));
  double laplace=-std::numeric_limits<double>::infinity();
  if (!fs) {
    const double lm=fm ? mu : y[y.size()/2];
    long double ad=0;
    for (double v:y) ad+=std::abs(static_cast<long double>(v)-lm);
    const double b=static_cast<double>(ad/n);
    laplace=-n*(1+std::log(2.0)+std::log(b));
  }
  bool failed=false;
  auto evaluate = [&](double t) {
    HuberResult r=huber_fixed_mle(data,tol,maxit,std::exp(t),mu,sigma,true);
    if (r.status!=HuberStatus::Ok) failed=true;
    else if (best.status!=HuberStatus::Ok || r.logLik>best.logLik) best=r;
    return r.status==HuberStatus::Ok ? r.logLik :
        -std::numeric_limits<double>::infinity();
  };
  // c >= max |x-m_normal|/s_normal is exactly the normal quadratic loss;
  // the remaining normalizer decreases monotonically to its Gaussian limit.
  double largest=0;
  for (double v:y) largest=std::max(largest,std::abs((v-nm)/ns));
  const double upper=std::log(std::max(8.0,largest*2));
  // With scale held, f(x) <= c exp(c*c/2)/(2*sigma), uniformly
  // in location. This bounds the excluded small-c tail below the incumbent.
  double lower=fs ? std::min(-9.0,std::log(2.0)+std::log(sigma)+
      best.logLik/n-2) : -9.0;
  // The same Laplace bound also discards any small-c interval that
  // cannot improve the already available finite incumbent. This matters
  // for large samples: tiny cores otherwise cost many inner iterations.
  if (!fs && best.logLik>laplace)
    lower=std::max(lower,.5*std::log(2*(best.logLik-laplace)/n));
  const double spacing=.125;
  const int count=static_cast<int>(std::ceil((upper-lower)/spacing));
  std::vector<double> ts, values;
  for (int i=0;i<=count;++i) {
    const double t=lower+(upper-lower)*i/count;
    ts.push_back(t); values.push_back(evaluate(t));
  }
  for (int i=1;i<count;++i) {
    // Ignore flat numerical plateaus: faces are compared exactly below.
    if (!(values[i]>=values[i-1] && values[i]>=values[i+1] &&
          (values[i]>values[i-1] || values[i]>values[i+1]))) continue;
    double lo=ts[i-1], hi=ts[i+1];
    const double ratio=(std::sqrt(5.0)-1)/2;
    double a=hi-ratio*(hi-lo), b=lo+ratio*(hi-lo);
    double fa=evaluate(a), fb=evaluate(b);
    for (int j=0;j<80 && hi-lo>1e-8;++j) {
      if (fa<fb) { lo=a; a=b; fa=fb; b=lo+ratio*(hi-lo); fb=evaluate(b); }
      else { hi=b; b=a; fb=fa; a=hi-ratio*(hi-lo); fa=evaluate(a); }
    }
  }
  const double slack=1e-8*n;
  if (normal>=best.logLik-slack && normal>=laplace)
    best.status=HuberStatus::NormalLimit;
  else if (laplace>=best.logLik-slack)
    best.status=HuberStatus::LaplaceLimit;
  else if (failed) best.status=HuberStatus::IterationFailure;
  return best;
}
} // namespace cppunivariate
