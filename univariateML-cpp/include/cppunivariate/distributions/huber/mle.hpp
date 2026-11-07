#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/face_compare.hpp"
#include "../../core/precision.hpp"
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
 StopDetail stop_detail{}; };
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
                             double epsilon,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  const std::size_t n = y.size();
  if (n % 2 == 0 && (y[n/2]-y[n/2-1])/2 >= epsilon*sigma)
    return y[n/2-1]/2 + y[n/2]/2;
  double lo = y.front(), hi = y.back();
  for (int j=0; j<maxit; ++j) {
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
  maxit = static_cast<int>(solver_budget(maxit));
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
    for (int k=0;k<maxit;++k,step*=.5) {
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
// Inner solve on data normalized once by the caller. The original reductions
// and initial values are retained for the final reproducibility polish.
inline HuberResult huber_fixed_solve(const std::vector<double>& y,
    double center, double unit, double m, double s, bool fm, bool fs,
    double mu, double sigma, double epsilon, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  HuberResult r{mu,sigma,epsilon,std::numeric_limits<double>::quiet_NaN(),
      0,HuberStatus::InvalidData};
  const double n=static_cast<double>(y.size());
  const double initial_m=m, initial_s=s;
  bool converged=(fm && fs) || huber_newton(y,epsilon,fm,fs,tol,maxit,m,s,r.iterations);
  if (!converged) {
    m=initial_m; s=initial_s;
  }
  if (!converged && !fs) {
    double lo=0, hi=1/s;
    auto score = [&](double eta) {
      const double loc=fm ? m : huber_location(y,1/eta,epsilon,maxit);
      double total=0;
      for (double v:y) { const double z=(v-loc)*eta; total+=z*huber_psi(z,epsilon); }
      return total/n-1;
    };
    int brackets=0;
    while (score(hi)<0 && brackets++<maxit) hi*=2;
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
      m=huber_location(y,s,epsilon,maxit);
  }
  if (!converged && !fm) {
    double score=0;
    for (double v:y) score+=huber_psi((v-m)/s,epsilon);
    if (!(std::isfinite(score) && std::abs(score)/n<=std::max(tol,1e-7)))
      r.status=HuberStatus::IterationFailure;
  }
  r.mu=fm ? mu : center+unit*m;
  r.sigma=fs ? sigma : unit*s;
  r.logLik=-n*(std::log(r.sigma)+std::log(huber_z(epsilon)));
  for (double v:y) r.logLik-=huber_rho((v-m)/s,epsilon);
  return r;
}

inline HuberResult huber_fixed_mle(span x, double tol=1e-10, int maxit=200,
    double epsilon=1.345,
    double mu=std::numeric_limits<double>::quiet_NaN(),
    double sigma=std::numeric_limits<double>::quiet_NaN(), bool sorted=false) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  if (!fs) {
    // Median/MAD start; a zero MAD (ties) falls back to the sample range.
    std::vector<double> deviations;
    deviations.reserve(y.size());
    for (double v:y) deviations.push_back(std::abs(v));
    std::nth_element(deviations.begin(),deviations.begin()+y.size()/2,deviations.end());
    s=deviations[y.size()/2]*1.4826;
    if (!(s>0)) s=1;
  }
  return huber_fixed_solve(y,center,unit,m,s,fm,fs,mu,sigma,epsilon,tol,maxit);
}

// Exact sufficient statistics for a sorted sample. Long-double prefixes limit
// cancellation when subtracting nearby cumulative sums. No bin approximation.
struct HuberProfileData {
  std::vector<double> y;
  std::vector<CPPUNIV_EXTENDED_REAL> sum, square;
  double center, unit;
  explicit HuberProfileData(const std::vector<double>& x, double mu)
      : y(x), center(x[x.size()/2]), unit(std::max(
          std::abs(x.front()-center),std::abs(x.back()-center))) {
    if (std::isfinite(mu)) unit=std::max(unit,std::abs(mu-center));
    if (!(unit>0)) unit=1;
    sum.push_back(0); square.push_back(0);
    for (double& v:y) {
      v=(v-center)/unit;
      sum.push_back(sum.back()+v);
      square.push_back(square.back()+static_cast<CPPUNIV_EXTENDED_REAL>(v)*v);
    }
  }
  // z=a*y-b: central quadratic moments plus signed linear tail moments.
  void moments(double a, double b, double c, CPPUNIV_EXTENDED_REAL& loss,
      double& gp, double& gh, double& aa, double& ab, double& bb) const {
    const double left=(b-c)/a, right=(b+c)/a;
    const std::size_t l=std::lower_bound(y.begin(),y.end(),left)-y.begin();
    const std::size_t r=std::upper_bound(y.begin(),y.end(),right)-y.begin();
    const CPPUNIV_EXTENDED_REAL k=r-l, sy=sum[r]-sum[l], sq=square[r]-square[l];
    // Explicit signed counts avoid unsigned subtraction.
    const CPPUNIV_EXTENDED_REAL signed_count=static_cast<CPPUNIV_EXTENDED_REAL>(y.size()-r)-l;
    const CPPUNIV_EXTENDED_REAL signed_sum=sum.back()-sum[r]-sum[l];
    const CPPUNIV_EXTENDED_REAL zsum=a*sy-b*k;
    loss=(a*a*sq-2*a*b*sy+b*b*k)/2+
        c*(a*signed_sum-b*signed_count)-c*c*(y.size()-(r-l))/2;
    gp=static_cast<double>(a*sq-b*sy+c*signed_sum);
    gh=static_cast<double>(zsum+c*signed_count);
    aa=static_cast<double>(sq); ab=-static_cast<double>(sy); bb=static_cast<double>(k);
    // At kinks the original solver assigns half curvature.
    for (double edge:{left,right}) {
      const auto first=std::lower_bound(y.begin(),y.end(),edge);
      const auto last=std::upper_bound(first,y.end(),edge);
      if (first!=last && std::abs(a*edge-b)==c) {
        const double half=.5*(last-first);
        aa-=half*edge*edge; ab+=half*edge; bb-=half;
      }
    }
  }
  HuberResult fit(double c, double mu, double sigma, double tol, int maxit,
                  const HuberResult& start) const {
    const bool fm=std::isfinite(mu), fs=std::isfinite(sigma);
    const double n=static_cast<double>(y.size());
    double a=unit/(fs ? sigma : start.sigma);
    double b=((fm ? mu : start.mu)-center)*a/unit;
    for (int j=0;j<maxit;++j) {
      CPPUNIV_EXTENDED_REAL loss=0;
      double gp=0,gh=0,aa=0,ab=0,bb=0;
      moments(a,b,c,loss,gp,gh,aa,ab,bb);
      const double m=fm ? (mu-center)/unit : 0;
      if (fm) { gp-=m*gh; aa+=2*m*ab+m*m*bb; }
      const double g1=1/a-gp/n, g2=gh/n;
      if ((fs || std::abs(a*g1-(fm ? 0 : b*g2))<=tol) &&
          (fm || std::abs(g2)<=tol))
      {
        if (!fm && y.size()%2==0 &&
            (y[y.size()/2]-y[y.size()/2-1])/2>=c/a) {
          b=(y[y.size()/2-1]/2+y[y.size()/2]/2)*a;
          moments(a,b,c,loss,gp,gh,aa,ab,bb);
        }
        return {fm ? mu : center+unit*b/a, fs ? sigma : unit/a,c,
          -n*(std::log(unit/a)+std::log(huber_z(c)))-static_cast<double>(loss),
          j,HuberStatus::Ok};
      }
      aa=1/(a*a)+aa/n; ab/=n; bb/=n;
      double da=0,db=0;
      if (fm && fs) break;
      if (fm) da=g1/aa;
      else if (fs) { if (!(bb>0)) break; db=g2/bb; }
      else {
        const double det=aa*bb-ab*ab;
        if (!(det>0)) break;
        da=(bb*g1-ab*g2)/det; db=(aa*g2-ab*g1)/det;
      }
      const double slope=g1*da+(fm ? 0 : g2*db);
      const CPPUNIV_EXTENDED_REAL old=std::log(a)-loss/n;
      bool accepted=false;
      double step=1;
      for (int k=0;k<maxit;++k,step*=.5) {
        const double na=a+step*da, nb=fm ? m*na : b+step*db;
        if (!(na>0) || !std::isfinite(na) || !std::isfinite(nb)) continue;
        CPPUNIV_EXTENDED_REAL nl=0;
        moments(na,nb,c,nl,gp,gh,aa,ab,bb);
        if (std::log(na)-nl/n>=old+1e-4*step*slope-
            8*std::numeric_limits<double>::epsilon()*(1+std::abs(old))) {
          a=na; b=nb; accepted=true; break;
        }
      }
      if (!accepted) break;
    }
    // The original safeguarded profile fallback, with the same sufficient
    // statistics replacing full-data reductions and location score sums.
    auto location = [&](double eta) {
      if (fm) return (mu-center)/unit;
      if (y.size()%2==0 && (y[y.size()/2]-y[y.size()/2-1])/2>=c/eta)
        return y[y.size()/2-1]/2+y[y.size()/2]/2;
      double lo=y.front(), hi=y.back();
      for (int j=0;j<maxit;++j) {
        const double m=lo/2+hi/2;
        if (m==lo || m==hi) break;
        CPPUNIV_EXTENDED_REAL loss=0;
        double gp=0,gh=0,aa=0,ab=0,bb=0;
        moments(eta,m*eta,c,loss,gp,gh,aa,ab,bb);
        if (gh==0) return m;
        if (gh>0) lo=m; else hi=m;
      }
      return lo/2+hi/2;
    };
    double lo=0, hi=unit/(fs ? sigma : start.sigma);
    bool bracketed=false;
    int refinements=0;
    for (int j=0;j<maxit;++j) {
      a=fs || !bracketed ? hi : lo/2+hi/2;
      b=a*location(a);
      CPPUNIV_EXTENDED_REAL loss=0;
      double gp=0,gh=0,aa=0,ab=0,bb=0;
      moments(a,b,c,loss,gp,gh,aa,ab,bb);
      const double score=(a*gp-b*gh)/n-1;
      if ((fs || std::abs(score)<=tol) &&
          (fm || (std::isfinite(gh) && std::abs(gh)/n<=std::max(tol,1e-7))))
        return {fm ? mu : center+unit*b/a,fs ? sigma : unit/a,c,
          -n*(std::log(unit/a)+std::log(huber_z(c)))-static_cast<double>(loss),
          j,HuberStatus::Ok};
      if (fs) break;
      if (!bracketed && score<0) { hi*=2; continue; }
      bracketed=true;
      if (++refinements>maxit) break;
      if (score>0) hi=a; else lo=a;
    }
    return {mu,sigma,c,0,0,HuberStatus::IterationFailure};
  }
};

// NaN epsilon requests estimation; the public default remains held at 1.345.
inline HuberResult huber_mle(span x, double tol=1e-10, int maxit=200,
    double epsilon=1.345,
    double mu=std::numeric_limits<double>::quiet_NaN(),
    double sigma=std::numeric_limits<double>::quiet_NaN(),
    bool exact_scan=false) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  CPPUNIV_EXTENDED_REAL total=0;
  for (double v:y) total+=static_cast<CPPUNIV_EXTENDED_REAL>(v)-y[0];
  const double nm=fm ? mu : static_cast<double>(y[0]+total/n);
  CPPUNIV_EXTENDED_REAL ss=0;
  for (double v:y) { const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-nm; ss+=d*d; }
  const double ns=fs ? sigma : static_cast<double>(std::sqrt(ss/n));
  const double normal=-n*(std::log(ns)+.5*std::log(2*std::acos(-1.0)))-
      static_cast<double>(ss/(2*static_cast<CPPUNIV_EXTENDED_REAL>(ns)*ns));
  double laplace=-std::numeric_limits<double>::infinity();
  if (!fs) {
    const double lm=fm ? mu : y[y.size()/2];
    CPPUNIV_EXTENDED_REAL ad=0;
    for (double v:y) ad+=std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-lm);
    const double b=static_cast<double>(ad/n);
    laplace=-n*(1+std::log(2.0)+std::log(b));
  }
  const bool accelerated=y.size()>4096 && !exact_scan;
  const HuberProfileData prepared(accelerated ? y : std::vector<double>{0},mu);
  double initial_s=fs ? sigma/prepared.unit : 1;
  if (accelerated && !fs) {
    std::vector<double> deviations;
    deviations.reserve(y.size());
    for (double v:prepared.y) deviations.push_back(std::abs(v));
    std::nth_element(deviations.begin(),deviations.begin()+y.size()/2,deviations.end());
    initial_s=deviations[y.size()/2]*1.4826;
    if (!(initial_s>0)) initial_s=1;
  }
  auto original_fit = [&](double c) {
    if (!accelerated) return huber_fixed_mle(data,tol,maxit,c,mu,sigma,true);
    double m=fm ? (mu-prepared.center)/prepared.unit : 0, s=initial_s;
    int iterations=0;
    // Retain the historical choice of recovery path, but do not shorten
    // either local solve: both use the caller's complete allowance.
    if(maxit<=std::max(12.0,16000000.0/n))
      return huber_fixed_solve(prepared.y,prepared.center,prepared.unit,m,s,
          fm,fs,mu,sigma,c,tol,maxit);
    const int cold_limit=maxit;
    if (!(fm && fs) && !huber_newton(prepared.y,c,fm,fs,tol,
        cold_limit,m,s,iterations)) {
      // Full reductions can stall on rounding near convergence at large n.
      // Recover with exact prefixes instead of hundreds of full-data passes.
      const HuberResult seed=prepared.fit(c,mu,sigma,tol,maxit,best);
      if (seed.status==HuberStatus::Ok) {
        m=(seed.mu-prepared.center)/prepared.unit;
        s=seed.sigma/prepared.unit;
      }
    }
    return huber_fixed_solve(prepared.y,prepared.center,prepared.unit,m,s,
        fm,fs,mu,sigma,c,tol,maxit);
  };
  HuberResult warm=best;
  bool failed=false;
  auto evaluate = [&](double t, bool nominate=false) {
    HuberResult r=nominate ? prepared.fit(std::exp(t),mu,sigma,tol,maxit,warm) :
        original_fit(std::exp(t));
    // Retain the robust original fallback, including its midpoint convention.
    if (nominate && r.status!=HuberStatus::Ok)
      r=original_fit(std::exp(t));
    if (r.status==HuberStatus::Ok) {
      warm=r;
    }
    if (r.status!=HuberStatus::Ok) failed=true;
    else if (!nominate && (best.status!=HuberStatus::Ok || r.logLik>best.logLik)) best=r;
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
    ts.push_back(t); values.push_back(evaluate(t,accelerated));
  }
  double winning_lo=0, winning_hi=0;
  double winning_value=-std::numeric_limits<double>::infinity();
  if (accelerated) {
    // Endpoints are retained even when the profile is monotone.
    evaluate(ts.front()); evaluate(ts.back());
  }
  for (int i=1;i<count;++i) {
    // Ignore flat numerical plateaus: faces are compared exactly below.
    if (!(values[i]>=values[i-1] && values[i]>=values[i+1] &&
          (values[i]>values[i-1] || values[i]>values[i+1]))) continue;
    double lo=ts[i-1], hi=ts[i+1];
    const double ratio=(std::sqrt(5.0)-1)/2;
    double a=hi-ratio*(hi-lo), b=lo+ratio*(hi-lo);
    double fa=evaluate(a,accelerated), fb=evaluate(b,accelerated);
    double peak=std::max(values[i],std::max(fa,fb));
    for (int j=0;j<maxit && hi-lo>1e-8;++j) {
      if (fa<fb) { lo=a; a=b; fa=fb; b=lo+ratio*(hi-lo); fb=evaluate(b,accelerated); }
      else { hi=b; b=a; fb=fa; a=hi-ratio*(hi-lo); fa=evaluate(a,accelerated); }
      peak=std::max(peak,std::max(fa,fb));
    }
    if (accelerated && peak>winning_value) {
      winning_value=peak;
      winning_lo=ts[i-1]; winning_hi=ts[i+1];
    }
  }
  if (accelerated && winning_hi>winning_lo) {
    // Preserve the legacy full-reduction rounding convention at the winner.
    // All nominated peaks above were refined on the full sample, via exact
    // sufficient statistics; this last polish is solely for reproducibility.
    double lo=winning_lo, hi=winning_hi;
    evaluate((lo+hi)/2);
    const double ratio=(std::sqrt(5.0)-1)/2;
    double a=hi-ratio*(hi-lo), b=lo+ratio*(hi-lo);
    double fa=evaluate(a), fb=evaluate(b);
    for (int j=0;j<maxit && hi-lo>1e-8;++j) {
      if (fa<fb) { lo=a; a=b; fa=fb; b=lo+ratio*(hi-lo); fb=evaluate(b); }
      else { hi=b; b=a; fb=fa; a=hi-ratio*(hi-lo); fa=evaluate(a); }
    }
  }
  const double slack=1e-8*n;
  // With both coordinates held the common Gaussian quadratic cancels.
  // rho_c(z)-z^2/2 = -max(|z|-c,0)^2/2 in exact arithmetic.
  FaceComparison normal_comparison=compare_face(normal,best.logLik,slack);
  if(fm && fs && best.status==HuberStatus::Ok && best.epsilon>0 &&
      std::isfinite(best.epsilon)) {
    double gain=n*(std::log(huber_z(best.epsilon))-.5*std::log(2*std::acos(-1.0)));
    double gain_scale=n+std::abs(gain);
    for(double v:y) {
      const double excess=std::max(0.,std::abs((v-mu)/sigma)-best.epsilon);
      gain-=.5*excess*excess;
      gain_scale+=.5*excess*excess;
    }
    normal_comparison=compare_face(gain,0,slack,gain_scale);
  }
  if(normal_comparison==FaceComparison::NumericalTie || face_numerical_tie(laplace,best.logLik,slack) || (normal_comparison==FaceComparison::FirstWins && laplace>=best.logLik-slack && face_numerical_tie(normal,laplace,0))) {
    StopDetail d;d.comparison_tie=true;d.loglik_upper=INFINITY;d.loglik_lower=normal;
    d.candidates.push_back({{nm,ns},normal,normal,StopModel::Norm});
    if(std::isfinite(laplace)) {
      const double lm=fm?mu:y[y.size()/2];double ad=0;for(double v:y)ad+=std::abs(v-lm);
      d.candidates.push_back({{lm,ad/n},laplace,laplace,StopModel::Laplace});d.loglik_lower=std::max(d.loglik_lower,laplace);
    }
    if(best.sigma>0 && std::isfinite(best.logLik)) {
      d.candidates.push_back({{best.mu,best.sigma,best.epsilon},best.logLik,best.logLik,StopModel::None});d.loglik_lower=std::max(d.loglik_lower,best.logLik);
    }
    best.stop_detail=d;best.status=HuberStatus::IterationFailure;return best;
  }
  if (normal_comparison==FaceComparison::FirstWins && normal>=laplace)
    best.status=HuberStatus::NormalLimit;
  else if (laplace>=best.logLik-slack)
    best.status=HuberStatus::LaplaceLimit;
  else if (failed) best.status=HuberStatus::IterationFailure;
  if(best.status==HuberStatus::NormalLimit)best.stop_detail=limit_detail({nm,ns},normal,StopModel::Norm);
  if(best.status==HuberStatus::LaplaceLimit) {
    const double lm=fm?mu:y[y.size()/2];
    double ad=0;for(double v:y)ad+=std::abs(v-lm);
    best.stop_detail=limit_detail({lm,ad/n},laplace,StopModel::Laplace);
  }
  // The held-coordinate shape search must not accept a nonstationary
  // finite candidate merely because its sampled grid missed a narrow peak.
  if(fm && fs && best.status==HuberStatus::Ok) {
    const double c=best.epsilon;
    double score=2*n*std::exp(-c*c/2)/(c*huber_z(c));
    for(double v:y) {
      const double z=std::abs((v-mu)/sigma);
      if(z>c) score-=c*(z-c);
    }
    if(!std::isfinite(score) || std::abs(score)>1e-3)
      best.status=HuberStatus::IterationFailure;
  }
  return best;
}
} // namespace cppunivariate
