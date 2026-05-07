// Vendored R nmath dpqr primitives, namespaced.
//
// Each function is a port of the upstream R source under
// instructions/nmath/<name>.c — bit-for-bit equivalent algorithms so that
// when src/distributions/snorm.cpp / logitnorm.cpp swap from Rf_dnorm4 etc.
// to mlcore::nmath::dnorm4, the dpqr regression goldens stay unchanged.
//
// Phase 1 covers the closed-form / Chebyshev primitives needed by snorm
// and logitnorm. Phase 2 (deferred) will add dgamma / pgamma / qgamma /
// dt / pt / qt with the lgamma / bd0 / dpois_raw / chebyshev / stirlerr
// helpers they pull in.

#pragma once

namespace mlcore {
namespace nmath {

// Chebyshev series evaluation (helper used by pnorm5).
double chebyshev_eval(double x, const double *a, int n);

// Normal: density, CDF, quantile.
double dnorm4(double x, double mu, double sigma, int give_log);
double pnorm5(double x, double mu, double sigma, int lower_tail, int log_p);
void pnorm_both(double x, double *cum, double *ccum, int i_tail, int log_p);
double qnorm5(double p, double mu, double sigma, int lower_tail, int log_p);

// Logistic: CDF, quantile.
double plogis(double x, double location, double scale, int lower_tail, int log_p);
double qlogis(double p, double location, double scale, int lower_tail, int log_p);

}  // namespace nmath
}  // namespace mlcore
