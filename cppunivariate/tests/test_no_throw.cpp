#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/special/special.hpp"
#include <cmath>
#include <limits>

TEST_CASE("special-function errors return nonfinite values") {
#define CHECK_NONFINITE(expression) \
  do { \
    double result = 0.0; \
    CHECK_NOTHROW(result = (expression)); \
    CHECK_FALSE(std::isfinite(result)); \
  } while (false)
  CHECK_NONFINITE(cppunivariate::special::digamma(0.0));
  CHECK_NONFINITE(cppunivariate::special::digamma(-1.0));
  CHECK_NONFINITE(cppunivariate::special::trigamma(0.0));
  CHECK_NONFINITE(cppunivariate::special::tetragamma(0.0));
  CHECK_NONFINITE(cppunivariate::special::psigamma(-1.0, 3));
  CHECK_NONFINITE(cppunivariate::special::lbeta(-0.5, 2.0));
  CHECK_NONFINITE(cppunivariate::special::lgamma(std::numeric_limits<double>::max()));
}

TEST_CASE("distribution errors return NaN without exceptions") {
#define CHECK_NAN(expression) \
  do { \
    double result = 0.0; \
    CHECK_NOTHROW(result = (expression)); \
    CHECK(std::isnan(result)); \
  } while (false)
  CHECK_NAN(cppunivariate::special::pnorm(0, 0, -1, true, false));
  CHECK_NAN(cppunivariate::special::qnorm(0.5, 0, -1, true, false));
  CHECK_NAN(cppunivariate::special::plogis(0, 0, -1, true, false));
  CHECK_NAN(cppunivariate::special::qlogis(2, 0, 1, true, false));
  CHECK_NAN(cppunivariate::special::pgamma(1, -1, 1, true, false));
  CHECK_NAN(cppunivariate::special::qgamma(-0.1, 1, 1, true, false));
  CHECK_NAN(cppunivariate::special::pt(1, -1, true, false));
  CHECK_NAN(cppunivariate::special::qt(1.1, 2, false, false));
  CHECK_NAN(cppunivariate::special::beta_cdf(0.5, -1, 2, true));
  CHECK_NAN(cppunivariate::special::beta_quantile(2, 1, 2, true));
  CHECK_NAN(cppunivariate::special::binomial_cdf(1, -1, 0.5, true));
  CHECK_NAN(cppunivariate::special::binomial_quantile(-1, 10, 0.5, true));
  CHECK_NAN(cppunivariate::special::poisson_cdf(1, -1, true));
  CHECK_NAN(cppunivariate::special::poisson_quantile(2, 1, true));
  CHECK_NAN(cppunivariate::special::negative_binomial_cdf(1, -1, 0.5, true));
  CHECK_NAN(cppunivariate::special::negative_binomial_quantile(2, 1, 0.5, true));
  CHECK_NAN(cppunivariate::special::inverse_gaussian_cdf(1, -1, 1, true));
  CHECK_NAN(cppunivariate::special::inverse_gaussian_quantile(2, 1, 1, true));
}

TEST_CASE("overflow and endpoint errors return infinity") {
  CHECK_NONFINITE(cppunivariate::special::qnorm(1, 0, 1, true, false));
  CHECK_NONFINITE(cppunivariate::special::qgamma(1, 1, 1, true, false));
  CHECK_NONFINITE(cppunivariate::special::qnorm(0.99, 0,
      std::numeric_limits<double>::max(), true, false));
}
#undef CHECK_NAN
#undef CHECK_NONFINITE
