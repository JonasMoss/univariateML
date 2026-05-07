// Laplace MLE: median for location, mean absolute deviation for scale.

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct LaplaceResult {
  double mu;
  double sigma;
  double logLik;
};

// Negative log-likelihood of Laplace(mu, sigma):
//   nll(mu, sigma) = n*log(2*sigma) + sum |x - mu| / sigma.
//
// The mu-direction is non-smooth: |x - mu| has a kink at each x_i, so the
// classical observed-information Hessian (= sum of second derivatives of
// nll) is degenerate in mu (zero almost everywhere, Dirac masses at the
// observations). Fisher's information equality `E[-d^2 log f] = E[s^2]`
// breaks for the same reason — but the score-squared form
// `I_{mu mu} = E[(sign(x-mu)/sigma)^2] = 1/sigma^2`
// is well-defined and is the right asymptotic-variance quantity. The MLE
// of mu (the sample median) is asymptotically N(mu, sigma^2/n), matching
// `(n/sigma^2)^{-1}` exactly.
//
// We therefore expose `hessian()` returning the *Fisher information*
// `n * I` rather than the (degenerate) observed information for the mu
// row/column. For sigma the two coincide at the MLE: the standard
// d^2/dsigma^2 of the nll is `-n/sigma^2 + 2*sum|x-mu|/sigma^3`, which at
// the MLE (where sum|x-mu_hat| = n*sigma_hat) reduces to `n/sigma^2`.
//
// Both parameters are asymptotically normal; regular_params = {1, 1}.
struct LaplaceCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    if (!(sigma > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S = 0.0;
    for (auto v : x) S += std::fabs(v - mu);
    return n * std::log(2.0 * sigma) + S / sigma;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double sigma = theta[1];
    const double n = static_cast<double>(x.size());
    double sum_sign = 0.0, S = 0.0;
    for (auto v : x) {
      const double d = v - mu;
      sum_sign += (d > 0.0) - (d < 0.0);
      S += std::fabs(d);
    }
    g[0] = -sum_sign / sigma;
    g[1] = n / sigma - S / (sigma * sigma);
  }
  // Returns the *Fisher information* matrix at theta (n * I per obs).
  // For Laplace this differs from the second-derivative observed info in
  // the mu-direction; the Fisher form is the correct asymptotic-variance
  // matrix and matches the observed info for sigma at the MLE.
  void hessian(const double *theta, double *H) const {
    const double sigma = theta[1];
    const double n = static_cast<double>(x.size());
    const double s2 = sigma * sigma;
    H[0] = n / s2;        // Fisher info for mu (asymptotic-variance matrix)
    H[1] = 0.0;           // E[score_mu * score_sigma] = 0 by symmetry
    H[2] = 0.0;
    H[3] = n / s2;        // Fisher info for sigma (== observed at MLE)
  }
};

inline LaplaceResult laplace_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN};
  }
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  const double mu = sorted[n / 2];
  double abs_sum = 0.0;
  for (auto v : x) abs_sum += std::abs(v - mu);
  const double sigma = abs_sum / static_cast<double>(n);
  const double logLik =
      -static_cast<double>(n) * (1.0 + std::log(2.0 * sigma));
  return {mu, sigma, logLik};
}

}  // namespace mlcore
