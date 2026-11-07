// Historical proposal; the certified public dispatcher is included below.
#pragma once
#include "../../core/work_budget.hpp"
#include "../nbinom/mle.hpp"
#include "../../core/stop_detail.hpp"
#include "../zip/mle.hpp"
#include "../ztpois/profile.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <map>

namespace cppunivariate {
enum class ZinbCertificateFailure { None, Arithmetic, Budget, ComparisonTie, Workload, TailUnresolved };
enum class ZinbStatus { Ok, InvalidInput, InvalidSize, IterationFailure, ZipLimit, PoissonLimit, ZeroLimit };
struct ZinbResult {
  double size, prob, pi, logLik;
  int iterations;
  ZinbStatus status;
  StopDetail stop_detail{};
  ZinbCertificateFailure certificate_failure=ZinbCertificateFailure::None;
};
struct ZinbCertificateResult {
  ZinbResult fit;
  StopDetail stop_detail;
  ZinbCertificateFailure failure=ZinbCertificateFailure::None;
};

inline ZinbResult zinb_candidate_mle(ispan x, double tol = 1e-10, int maxit = 200,
                         double fixed_size = std::numeric_limits<double>::quiet_NaN(),
                         double fixed_prob = std::numeric_limits<double>::quiet_NaN(),
                         double fixed_pi = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  ZinbResult best{nan, nan, nan, -std::numeric_limits<double>::infinity(), 0,
                  ZinbStatus::IterationFailure};
  const bool fixed = !std::isnan(fixed_size);
  if (fixed && (!(fixed_size >= 0) || !std::isfinite(fixed_size))) {
    best.status = ZinbStatus::InvalidSize; return best;
  }
  if (x.size() == 0 || !(tol > 0) || !std::isfinite(tol) || maxit < 1) {
    best.status = ZinbStatus::InvalidInput; return best;
  }
  const bool fp=!std::isnan(fixed_prob), fi=!std::isnan(fixed_pi);
  if((fp && (!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob))) ||
     (fi && (!(fixed_pi>=0 && fixed_pi<=1)||!std::isfinite(fixed_pi)))) {
    best.status=ZinbStatus::InvalidInput;return best;
  }
  std::map<int, int> counts;
  double sum = 0, zeros = 0;
  for (int v : x) {
    if (v < 0) { best.status = ZinbStatus::InvalidInput; return best; }
    ++counts[v]; sum += v; if (v == 0) ++zeros;
  }
  const double n = static_cast<double>(x.size()), z = zeros / n;
  if(sum>0 && (fixed_size==0 || (fp && fixed_prob==1)||(fi && fixed_pi==1))) {
    best.status=ZinbStatus::InvalidInput;return best;
  }
  if(sum==0) {
    const double r=fixed?fixed_size:0, p=fp?fixed_prob:(fixed_size>0?1:.5);
    const double pi=fi?fixed_pi:(r==0 || p==1?0:1);
    return {r,p,pi,n*std::log(pi+(1-pi)*std::pow(p,r)),0,ZinbStatus::Ok};
  }
  const double positive_mean = sum / (n - zeros);
  if(!fixed && maxit < std::ceil(std::log(std::min(tol,0.5)/2)/std::log(0.6180339887498948482))) {
    best.status=ZinbStatus::IterationFailure;return best;
  }
  if(fi && !fp && fixed_pi>0 && maxit < std::ceil(std::log(std::min(tol,0.5)/2)/std::log(0.6180339887498948482))) {
    best.status=ZinbStatus::IterationFailure;return best;
  }
  // Evaluate the exact pi profile. Log-sum-exp prevents zero-mass underflow.
  auto evaluate = [&](double r, double p, double mu = std::numeric_limits<double>::quiet_NaN()) {
    const double logf0 = r * (std::isnan(mu)?std::log(p):-std::log1p(mu/r));
    const double log1mp = std::isnan(mu)?std::log1p(-p):std::log(mu)-std::log(r+mu);
    const double f0 = std::exp(logf0);
    const double pi = fi ? fixed_pi : (f0 < z ? (z - f0) / (-std::expm1(logf0)) : 0);
    double ll = 0;
    for (const auto &kv : counts) {
      double l;
      if (kv.first == 0) {
        const double a = std::log(pi), b = std::log1p(-pi) + logf0;
        const double m = std::max(a, b);
        l = m + std::log1p(std::exp(std::min(a, b) - m));
      } else {
        const double v = kv.first;
        double gamma_difference = special::lgamma(r + v) - special::lgamma(r);
        // log rising factorial expansion avoids subtracting two enormous
        // log-gammas near the Poisson face. Sum powers of 0,...,v-1.
        if (r > 1e4 && v/r < 1e-3) {
          const double k = v-1;
          const double s1 = k*v/2, s2 = k*v*(2*v-1)/6;
          const double s3 = s1*s1;
          const double s4 = k*v*(2*v-1)*(3*k*k+3*k-1)/30;
          gamma_difference = v*std::log(r) + s1/r - s2/(2*r*r)
              + s3/(3*r*r*r) - s4/(4*r*r*r*r);
        }
        l = std::log1p(-pi) + gamma_difference
            - special::lgamma(v + 1) + logf0 + v * log1mp;
      }
      ll += kv.second * l;
    }
    return ZinbResult{r, p, pi, ll, 0, ZinbStatus::Ok};
  };
  // For fixed r, the truncated NB score in p is zero precisely when its
  // conditional mean equals the empirical positive mean. It decreases
  // continuously from infinity to one as p increases from zero to one.
  auto profile = [&](double r) {
    if(fp) return evaluate(r,fixed_prob);
    if(fi) {
      // At a stationary probability the base mean is between the sample
      // mean and positive mean. Search that compact interval, retaining
      // stable log probabilities near the Poisson face.
      auto at=[&](double t) {
        const double mu=std::exp(t);
        return evaluate(r,r/(r+mu),mu);
      };
      const double a=std::log(sum/n), b=std::log(positive_mean);
      if(fixed_pi==0 || zeros==0) return at(a);
      auto out=at(a);
      const int steps=std::max(2,static_cast<int>(std::ceil((b-a)/.125)));
      const double step=(b-a)/steps;
      for(int j=0;j<=steps;++j) {
        const double t=a+j*step;
        const auto mid=at(t);
        if(mid.logLik>out.logLik)out=mid;
        if(j>0 && mid.logLik<at(t-step).logLik)continue;
        if(j<steps && mid.logLik<at(t+step).logLik)continue;
        const double opt=zero_detail::maximize([&](double u){return at(u).logLik;},std::max(a,t-step),std::min(b,t+step),tol,maxit);
        const auto candidate=at(opt);
        if(candidate.logLik>out.logLik)out=candidate;
      }
      return out;
    }
    const double p = zero_detail::nb_prob(r, positive_mean,maxit);
    // The join belongs to the NB branch. Its optimum has p=r/(r+mean).
    if (r * std::log(p) <= std::log(z)) return evaluate(r, p);
    return evaluate(r, r / (r + sum / n));
  };
  if (fixed) return profile(fixed_size);
  // Include the attained NB member explicitly, reusing its exact fitter.
  const auto nb = nbinom_mle(x, std::min(tol, 1e-10), maxit, nan, fixed_prob);
  if (std::isfinite(nb.size) && nb.size > 0 && nb.prob < 1) {
    best = evaluate(nb.size, nb.prob);
  }
  const auto initial = profile(1);
  if (initial.logLik > best.logLik) best = initial;
  const auto zip = fp ? ZipResult{nan,nan,nan,ZipStatus::InvalidInput} : zip_mle(x,nan,fixed_pi);
  // Expand both ends until the lower end has negligible positive mass and
  // the upper end agrees with the independently maximized infinite-size
  // face. No finite size cap is returned as an approximation to that face.
  int lower = -2, upper = 2;
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (lower > -700 && profile(std::exp(lower)).logLik > best.logLik - n); ++bracket_pass)
    lower -= 2;
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (upper < (fp?700:32) && (fp ? profile(std::exp(upper)).logLik>best.logLik-n : (zip.status != ZipStatus::Ok ||
         std::abs(profile(std::exp(upper)).logLik - zip.logLik) > 1e-10*n))); ++bracket_pass)
    upper += 2;
  if (lower > -700 && profile(std::exp(lower)).logLik > best.logLik - n) {best.status=ZinbStatus::IterationFailure;return best;}
  if (upper >= (fp?700:32) || (fp ? profile(std::exp(upper)).logLik>best.logLik-n : (zip.status != ZipStatus::Ok || std::abs(profile(std::exp(upper)).logLik-zip.logLik)>1e-10*n))) {
    best.status = ZinbStatus::IterationFailure;
    return best; // Floating-point resolution exhausted; never return a capped fit.
  }
  const double step = 1;
  for (int j = lower; j <= upper; ++j) {
    const double t = j;
    const auto mid = profile(std::exp(t));
    if (mid.logLik > best.logLik) best = mid;
    if (mid.logLik < profile(std::exp(t-step)).logLik ||
        mid.logLik < profile(std::exp(t+step)).logLik) continue;
    double a = t-step, b = t+step;
    constexpr double ratio = 0.6180339887498948482;
    double c = b-ratio*(b-a), d = a+ratio*(b-a);
    auto fc = profile(std::exp(c)), fd = profile(std::exp(d));
    int i = 0;
    for (; i < maxit && b-a > tol; ++i) {
      if (fc.logLik > fd.logLik) {
        b=d; d=c; fd=fc; c=b-ratio*(b-a); fc=profile(std::exp(c));
      } else {
        a=c; c=d; fc=fd; d=a+ratio*(b-a); fd=profile(std::exp(d));
      }
    }
    auto candidate = fc.logLik > fd.logLik ? fc : fd;
    candidate.iterations = i;
    if (b-a>tol) candidate.status=ZinbStatus::IterationFailure;
    if (candidate.logLik > best.logLik) best = candidate;
  }
  if (zip.status == ZipStatus::Ok && zip.logLik >= best.logLik - 1e-8*n) {
    best.status = zip.pi == 0 ? ZinbStatus::PoissonLimit : ZinbStatus::ZipLimit;
  }
  return best;
}
} // namespace cppunivariate

namespace cppunivariate {
namespace zinb_cert_detail { inline double zero_logmass(double logzero,double pi); }
inline ZinbResult zinb_mle(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN(),
    double fixed_pi=std::numeric_limits<double>::quiet_NaN());
inline ZinbCertificateResult zinb_fixed_size_mle(ispan x,double size,
    double fixed_prob=std::numeric_limits<double>::quiet_NaN(),
    double fixed_pi=std::numeric_limits<double>::quiet_NaN(),double tol=1e-12,int maxit=200);
inline ZinbCertificateResult zinb_fixed_prob_mle(ispan x,double prob,
    double fixed_pi=std::numeric_limits<double>::quiet_NaN(),double tol=1e-12,int maxit=200);
}
#include "certificate.hpp"
#include "global_certificate.hpp"
