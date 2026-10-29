#pragma once
#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <map>
#include <vector>
namespace cppunivariate {
enum class PigStatus { Ok, PoissonLimit, ZeroLimit, InvalidInput, SearchFailure };
struct PigResult {
  double mean, shape, logLik;
  int iterations;
  PigStatus status;
};
namespace pig_detail {
struct Counts {
  std::map<int, int> frequencies;
  double n = 0, average = 0;
  explicit Counts(ispan x) {
    for (int k : x) { ++frequencies[k]; average += k; ++n; }
    average /= n;
  }
  double likelihood(double mean, double shape) const {
    const auto lp = log_masses(frequencies.rbegin()->first, mean, shape);
    long double ll=0;
    for (const auto &kv : frequencies) ll += static_cast<long double>(kv.second)*lp[kv.first];
    return static_cast<double>(ll);
  }
  double poisson(double mean) const {
    long double ll=0;
    for (const auto &kv : frequencies)
      ll += static_cast<long double>(kv.second)*(kv.first*std::log(mean)-mean-special::lgamma(kv.first+1.0));
    return static_cast<double>(ll);
  }
};
// Exhaust the log-parameter grid, then refine every bracketed peak rather
// than assuming unimodality. Bounds include the explicit limiting faces.
// This is a numerical global search, not an interval-arithmetic certificate.
template <typename F>
inline PigResult search(F f, double center, double tol, int maxit, double other_center=std::numeric_limits<double>::quiet_NaN()) {
  center=std::max(-650.0,std::min(650.0,center));
  other_center=std::isnan(other_center)?center:std::max(-650.0,std::min(650.0,other_center));
  const double step=0.5, lo=std::min(center,other_center)-48.0, hi=std::max(center,other_center)+48.0;
  std::vector<double> ts, ys;
  for (double t=lo; t<=hi; t+=step) { ts.push_back(t); ys.push_back(f(t)); }
  double best=-std::numeric_limits<double>::infinity(), bt=center;
  int iterations=0;
  bool failed=false;
  for (std::size_t i=1; i+1<ts.size(); ++i) {
    if (!(ys[i]>=ys[i-1] && ys[i]>=ys[i+1]) ||
        (ys[i]==ys[i-1] && ys[i]==ys[i+1])) continue;
    double a=ts[i-1], b=ts[i+1];
    const double g=0.6180339887498948482;
    double c=b-g*(b-a), d=a+g*(b-a), fc=f(c), fd=f(d);
    int it=0;
    for (; it<maxit && b-a>tol; ++it) {
      if (fc>fd) { b=d; d=c; fd=fc; c=b-g*(b-a); fc=f(c); }
      else { a=c; c=d; fc=fd; d=a+g*(b-a); fd=f(d); }
    }
    iterations+=it;
    if (std::max(fc,fd)>best) { best=std::max(fc,fd); bt=fc>fd?c:d; failed=b-a>tol; }
  }
  // Endpoints beating every bracket signal an unresolved escape/search.
  if (ys.front()>best || ys.back()>best) {
    const bool left=ys.front()>ys.back();
    best=left?ys.front():ys.back(); bt=left?ts.front():ts.back(); failed=true;
  }
  return {bt, 0, best, iterations, failed?PigStatus::SearchFailure:PigStatus::Ok};
}
}
// NaN means free. Each fixed subset is evaluated or profiled exactly;
// there is no generic multi-parameter optimiser.
inline PigResult pig_mle(ispan x, double tol=1e-9, int maxit=200,
 double mean=std::numeric_limits<double>::quiet_NaN(),
 double shape=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fm=!std::isnan(mean), fs=!std::isnan(shape);
  const auto invalid=PigResult{nan,nan,nan,0,PigStatus::InvalidInput};
  if (x.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fm && (!(mean>0)||!std::isfinite(mean))) ||
      (fs && (!(shape>0)||!std::isfinite(shape)))) return invalid;
  for (int k:x) if (k<0) return invalid;
  const pig_detail::Counts data(x);
  if (fm && fs) return {mean,shape,data.likelihood(mean,shape),0,PigStatus::Ok};
  if (data.average==0) return {fm?mean:0,fs?shape:0,0,0,PigStatus::ZeroLimit};
  if (!fm && !fs) mean=data.average;
  const double tie=1e-8*data.n;
  if (!fs) {
    const double poisson=data.poisson(mean);
    auto result=pig_detail::search([&](double t) {return data.likelihood(mean,std::exp(t));},
                                  2*std::log(mean),tol,maxit,2*std::log(data.average));
    if (poisson>=result.logLik-tie)
      return {mean,std::numeric_limits<double>::infinity(),poisson,result.iterations,PigStatus::PoissonLimit};
    return {mean,std::exp(result.mean),result.logLik,result.iterations,result.status};
  }
  // For fixed shape, the Levy endpoint (mean=infinity) is strictly
  // improved by a small positive t=shape/mean: ell(t)=ell(0)+n*t+O(t^2).
  auto result=pig_detail::search([&](double t) {return data.likelihood(std::exp(t),shape);},
                                std::log(data.average),tol,maxit);
  return {std::exp(result.mean),shape,result.logLik,result.iterations,result.status};
}
}
