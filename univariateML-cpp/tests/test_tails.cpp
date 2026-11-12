#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/special/tails.hpp"
#include <cmath>
#include <limits>

TEST_CASE("tail helpers: log complement extremes and domain") {
  CHECK(cppunivariate::special::log1mexp(-1) == doctest::Approx(-0.4586751453870819).epsilon(2e-15));
  CHECK(cppunivariate::special::log_neg_log1mexp(-1) == doctest::Approx(std::log(-std::log1p(-std::exp(-1.0)))));
  CHECK(cppunivariate::special::log1mexp(-1e-300) == doctest::Approx(-690.7755278982137).epsilon(2e-15));
  CHECK(cppunivariate::special::log1mexp(-745.0) == -std::exp(-745.0));
  CHECK(cppunivariate::special::log1mexp(-1e4) == 0.0);
  CHECK(cppunivariate::special::log1mexp(-std::numeric_limits<double>::infinity()) == 0.0);
  CHECK(cppunivariate::special::log1mexp(0.0) == -std::numeric_limits<double>::infinity());
  CHECK(std::isnan(cppunivariate::special::log1mexp(0.1)));
  CHECK(std::isnan(cppunivariate::special::log1mexp(std::numeric_limits<double>::quiet_NaN())));
  for (double x : {-1e-300, -0.6931471805599453, -1.0, -745.0, -1e4}) {
    const auto t = cppunivariate::special::tails_from_log(x, false);
    CHECK(t.valid);
    CHECK(t.upper == x);
    CHECK(cppunivariate::special::tail_probability(t, false, true) == x);
    CHECK(cppunivariate::special::tail_probability(t, true, false) == doctest::Approx(-std::expm1(x)).epsilon(2e-13));
  }
}
TEST_CASE("tail helpers: log arithmetic infinities and cancellation") {
  const double inf = std::numeric_limits<double>::infinity();
  CHECK(cppunivariate::special::log_sum_exp(-inf, -inf) == -inf);
  CHECK(cppunivariate::special::log_sum_exp(inf, inf) == inf);
  CHECK(cppunivariate::special::log_sum_exp(-1e4, -1e4) == doctest::Approx(-1e4+std::log(2.0)));
  CHECK(cppunivariate::special::log_diff_exp(-1e4, -1e4) == -inf);
  CHECK(cppunivariate::special::log_diff_exp(-inf, -inf) == -inf);
  CHECK(cppunivariate::special::log_diff_exp(0.0, -1e-300) == doctest::Approx(-690.7755278982137));
  CHECK(cppunivariate::special::log_diff_exp(0.0, -inf) == 0.0);
  CHECK(cppunivariate::special::log_diff_exp(inf, 0.0) == inf);
  CHECK(std::isnan(cppunivariate::special::log_diff_exp(inf, inf)));
  CHECK(std::isnan(cppunivariate::special::log_diff_exp(-2.0, -1.0)));
  CHECK(std::isnan(cppunivariate::special::log_sum_exp(0.0, std::numeric_limits<double>::quiet_NaN())));
}
TEST_CASE("tail helpers: stable nested log and quantile input") {
  const double inf = std::numeric_limits<double>::infinity();
  CHECK(cppunivariate::special::log_neg_log1mexp(-1e4) == -1e4);
  CHECK(cppunivariate::special::log_neg_log1mexp(-745.0) == -745.0);
  CHECK(cppunivariate::special::log_neg_log1mexp(-inf) == -inf);
  CHECK(cppunivariate::special::log_neg_log1mexp(0.0) == inf);
  CHECK(std::isnan(cppunivariate::special::log_neg_log1mexp(1.0)));
  for (bool lower : {false, true}) {
    for (double p : {0.0, 1e-300, 0.5, std::nextafter(1.0, 0.0), 1.0}) {
      const auto t = cppunivariate::special::quantile_log_tails(p, lower, false);
      REQUIRE(t.valid);
      CHECK((lower ? t.lower : t.upper) == std::log(p));
      CHECK((lower ? t.upper : t.lower) == std::log1p(-p));
    }
    for (double p : {-inf, -1e4, -745.0, -1e-300, 0.0}) {
      const auto t = cppunivariate::special::quantile_log_tails(p, lower, true);
      CHECK(t.valid);
      CHECK((lower ? t.lower : t.upper) == p);
    }
    for (double p : {-1.0, 1.01, inf, -inf, std::numeric_limits<double>::quiet_NaN()}) {
      const auto t = cppunivariate::special::quantile_log_tails(p, lower, false);
      CHECK_FALSE(t.valid);
      CHECK(std::isnan(t.lower));
      CHECK(std::isnan(t.upper));
      CHECK(std::isnan(cppunivariate::special::tail_probability(t, lower, true)));
    }
    CHECK_FALSE(cppunivariate::special::quantile_log_tails(0.01, lower, true).valid);
  }
}

TEST_CASE("tail helpers: portable product and affine range guards") {
  CHECK(cppunivariate::special::log1p_product(1e200,1e200) == doctest::Approx(921.03403719761827361).epsilon(2e-15));
  CHECK(cppunivariate::special::log_log1p_product(1e-200,1e-200) == doctest::Approx(-921.03403719761827361).epsilon(2e-15));
  CHECK(cppunivariate::special::log1p_product(2,3) == doctest::Approx(std::log(7.0)).epsilon(2e-15));
  CHECK(cppunivariate::special::log_log1p_product(2,3) == doctest::Approx(std::log(std::log(7.0))).epsilon(2e-15));
  CHECK(cppunivariate::special::log1p_product(1,0) == 0);
  CHECK(cppunivariate::special::log_log1p_product(1,0) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::special::scaled_difference(1e308,-1e308,1e308) == 2);
  CHECK(cppunivariate::special::scaled_difference(-1e308,1e308,1e308) == -2);
  CHECK(cppunivariate::special::scaled_difference(cppunivariate::special::kPosInf,1e308,1e-308) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::special::scaled_difference(3,1,2) == 1);
}

TEST_CASE("discrete inversion: failed comparisons, overflowing widths and stagnation") {
  const auto target=cppunivariate::special::discrete_log_tails(.5,true,false);
  for(int failed_at : {0,1,2,3,4,5}) {
    int calls=0;
    const double q=cppunivariate::special::discrete_quantile(target,0,16,[&](double x) {
      if(calls++==failed_at) return cppunivariate::special::LogTails{NAN,NAN,false};
      return cppunivariate::special::tails_from_log(x<7 ? -2.0 : -.1,true);
    });
    CHECK(std::isnan(q));
    CHECK(calls<=failed_at+1);
  }
  int calls=0;
  const double largest=std::numeric_limits<double>::max();
  CHECK(cppunivariate::special::discrete_bisect(-largest,largest,[&](double x) {
    ++calls;return int(x>=7);
  })==7);
  CHECK(calls<=std::numeric_limits<double>::max_exponent+5);
  CHECK(cppunivariate::special::discrete_bisect(1e308,largest,
      [](double x){return int(x>=1.5e308);})==1.5e308);
  calls=0;
  CHECK(std::isnan(cppunivariate::special::discrete_search(0,INFINITY,[&](double) {
    ++calls;return 0;
  })));
  CHECK(calls<=std::numeric_limits<double>::max_exponent+3);
  CHECK(std::isnan(cppunivariate::special::discrete_signed_search(0,1,
      [](double){return -1;})));
}

TEST_CASE("gamma ratio recurrence: cancellation cannot stall a discrete CDF") {
  CHECK(std::isnan(cppunivariate::special::log_gamma_ratio(300,1,1e308)));
  CHECK(std::isfinite(cppunivariate::special::log_gamma_ratio(1,.5,1)));
}

TEST_CASE("discrete inversion certifies adjacent doubles across the integer boundary") {
  for (double h : {9007199254740991.0,9007199254740992.0,
                   9007199254740994.0,1e30,1e308,-1e30}) {
    const double before=std::abs(h)<=9007199254740992.0 ? h-1 : std::nextafter(h,-INFINITY);
    int calls=0;
    const double q=cppunivariate::special::discrete_bisect(before,
        std::nextafter(h,INFINITY),[&](double x) {++calls;return int(x>=h);});
    CHECK(q==h);
    CHECK(calls<60);
  }
}

TEST_CASE("discrete inversion checks overflow against the last finite predecessor") {
  const double largest=std::numeric_limits<double>::max();
  CHECK(cppunivariate::special::discrete_search(0,INFINITY,
      [=](double x) {return int(x>=largest);})==largest);
  CHECK(cppunivariate::special::discrete_search(0,INFINITY,
      [](double x) {return int(x==INFINITY);})==INFINITY);
  CHECK(std::isnan(cppunivariate::special::discrete_search(0,INFINITY,
      [](double x) {return x==INFINITY ? -1 : 0;})));
  CHECK(cppunivariate::special::discrete_signed_search(-largest,1,
      [](double x) {return int(x!=-INFINITY);})==-largest);
}

TEST_CASE("signed expansion keeps progressing when the seed step is below one ULP") {
  const double centre=1e30;
  for (double h : {centre-1e16,centre+1e16}) {
    CHECK(cppunivariate::special::discrete_signed_search(centre,1,
        [=](double x) {return int(x>=h);})==h);
  }
}

TEST_CASE("signed expansion crosses zero even when its step reaches DBL_MAX") {
  const double largest=std::numeric_limits<double>::max();
  for (double centre : {-largest,largest}) for (double h : {-largest/2,largest/2}) {
    CHECK(cppunivariate::special::discrete_signed_search(centre,1,
        [=](double x) {return int(x>=h);})==h);
  }
}

TEST_CASE("tail helpers: guard softplus product overflow before multiplication") {
  // Once a*b is enormous and positive, log(softplus(a*b)) = log(a)+log(b)
  // to binary64 precision. A finite-input guard exposed GCC 15's inference
  // b=Inf inside the old a*b==Inf branch when this helper was inlined.
  for (double a : {1e307, 1e308, std::numeric_limits<double>::max()}) {
    REQUIRE(std::isfinite(a));
    for (double b : {2.0, 230.25850929940458}) {
      CHECK(cppunivariate::special::log_softplus_product(a,b) ==
            doctest::Approx(std::log(a)+std::log(b)).epsilon(2e-15));
    }
  }
  CHECK(cppunivariate::special::log_softplus_product(2,3) ==
        doctest::Approx(std::log(std::log1p(std::exp(6.0)))));
  CHECK(cppunivariate::special::log_softplus_product(1e308,-230) ==
        cppunivariate::special::kNegInf);
  CHECK(cppunivariate::special::log_softplus_product(1e308,0) ==
        doctest::Approx(std::log(std::log(2.0))));
}
