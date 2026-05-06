// Zipf distribution (d/p/q/r)
#include "common.h"

namespace dist_impl {

arma::vec dzipf(const arma::vec &x, const arma::vec &N, const arma::vec &s, bool log = false) {
  const uword n = recycle_length(x.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    if (!(Ni > 0.0 && si > 0.0) || !std::isfinite(xi) ||
        Ni != std::floor(Ni)) {
      out[i] = NA_REAL;
      continue;
    }
    if (xi < 1.0 || xi > Ni || xi != std::floor(xi)) {
      out[i] = log ? R_NegInf : 0.0;
      continue;
    }
    const int Nin = static_cast<int>(Ni);
    double sum_norm = 0.0;
    for (int k = 1; k <= Nin; ++k)
      sum_norm += std::pow(static_cast<double>(k), -si);
    const double log_norm = std::log(sum_norm);
    const double y = -si * std::log(xi) - log_norm;
    out[i] = log ? y : std::exp(y);
  }
  return out;
}

arma::vec pzipf(const arma::vec &q, const arma::vec &N, const arma::vec &s, bool lower_tail = true,
          bool log_p = false) {
  const uword n = recycle_length(q.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    if (!(Ni > 0.0 && si > 0.0) || !std::isfinite(qi) ||
        Ni != std::floor(Ni)) {
      out[i] = NA_REAL;
      continue;
    }
    if (qi < 1.0) {
      out[i] = lower_tail ? 0.0 : 1.0;
      out[i] = log_p ? std::log(out[i]) : out[i];
      continue;
    }
    if (qi >= Ni) {
      out[i] = lower_tail ? 1.0 : 0.0;
      out[i] = log_p ? std::log(out[i]) : out[i];
      continue;
    }
    const int Nin = static_cast<int>(Ni);
    const int qin = static_cast<int>(std::floor(qi));
    double num = 0.0, den = 0.0;
    for (int k = 1; k <= Nin; ++k) {
      const double w = std::pow(static_cast<double>(k), -si);
      den += w;
      if (k <= qin)
        num += w;
    }
    double prob = num / den;
    if (!lower_tail)
      prob = 1.0 - prob;
    out[i] = log_p ? std::log(prob) : prob;
  }
  return out;
}

arma::vec qzipf(const arma::vec &p, const arma::vec &N, const arma::vec &s, bool lower_tail = true,
          bool log_p = false) {
  const uword n = recycle_length(p.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    double pi = p[i % p.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    if (!(Ni > 0.0 && si > 0.0) || Ni != std::floor(Ni) || !std::isfinite(pi)) {
      out[i] = NA_REAL;
      continue;
    }
    if (log_p)
      pi = std::exp(pi);
    if (!lower_tail)
      pi = 1.0 - pi;
    if (pi <= 0.0) {
      out[i] = 1.0;
      continue;
    }
    if (pi >= 1.0) {
      out[i] = Ni;
      continue;
    }
    const int Nin = static_cast<int>(Ni);
    double den = 0.0;
    for (int k = 1; k <= Nin; ++k)
      den += std::pow(static_cast<double>(k), -si);
    double cum = 0.0;
    int k_ans = Nin;
    for (int k = 1; k <= Nin; ++k) {
      cum += std::pow(static_cast<double>(k), -si) / den;
      if (cum >= pi) {
        k_ans = k;
        break;
      }
    }
    out[i] = static_cast<double>(k_ans);
  }
  return out;
}

arma::vec rzipf(int n, double N, double s) {
  if (n < 0 || !(N > 0.0 && s > 0.0) || N != std::floor(N)) {
    Rcpp::stop("Invalid parameters for rzipf");
  }
  const int Nin = static_cast<int>(N);
  arma::vec cum(Nin);
  double total = 0.0;
  for (int k = 1; k <= Nin; ++k)
    total += std::pow(static_cast<double>(k), -s);
  double running = 0.0;
  for (int k = 1; k <= Nin; ++k) {
    running += std::pow(static_cast<double>(k), -s) / total;
    cum[k - 1] = running;
  }
  arma::vec out(static_cast<uword>(n));
  for (int i = 0; i < n; ++i) {
    double u = R::runif(0.0, 1.0);
    int k_ans =
        std::lower_bound(cum.begin(), cum.end(), u) - cum.begin() + 1;
    out[i] = static_cast<double>(k_ans);
  }
  return out;
}
} // namespace dist_impl
