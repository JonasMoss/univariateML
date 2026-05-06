// Smoke tests for the multi-D BFGS minimiser. Exercises the optimiser on
// known closed-form problems (quadratic, Rosenbrock) so failures here
// localise to the optimiser rather than to any estimator that calls it.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "../../src/ml/core/bfgs.hpp"
#include "../../src/ml/core/finite_diff.hpp"
#include "../../src/ml/core/reparam.hpp"

#include <cmath>

namespace {

// f(x, y) = (x - 3)^2 + (y + 1)^2
struct Quadratic {
  int dim() const { return 2; }
  double operator()(const double *t) const {
    const double a = t[0] - 3.0;
    const double b = t[1] + 1.0;
    return a * a + b * b;
  }
  void gradient(const double *t, double *g) const {
    g[0] = 2.0 * (t[0] - 3.0);
    g[1] = 2.0 * (t[1] + 1.0);
  }
};

// Rosenbrock f(x, y) = (1 - x)^2 + 100 * (y - x^2)^2
struct Rosenbrock {
  int dim() const { return 2; }
  double operator()(const double *t) const {
    const double a = 1.0 - t[0];
    const double b = t[1] - t[0] * t[0];
    return a * a + 100.0 * b * b;
  }
  void gradient(const double *t, double *g) const {
    const double x = t[0];
    const double y = t[1];
    g[0] = -2.0 * (1.0 - x) - 400.0 * x * (y - x * x);
    g[1] = 200.0 * (y - x * x);
  }
};

}  // namespace

TEST_CASE("bfgs: quadratic with analytic gradient") {
  Quadratic c;
  const double theta0[] = {0.0, 0.0};
  auto r = mlcore::bfgs_minimize(c, theta0);
  CHECK(r.status == mlcore::BfgsStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(3.0).epsilon(1e-6));
  CHECK(r.theta[1] == doctest::Approx(-1.0).epsilon(1e-6));
  CHECK(r.f == doctest::Approx(0.0).epsilon(1e-10));
}

TEST_CASE("bfgs: Rosenbrock with analytic gradient") {
  Rosenbrock c;
  const double theta0[] = {-1.2, 1.0};
  mlcore::BfgsControl ctl;
  ctl.maxit = 200;
  ctl.grad_tol = 1e-8;
  auto r = mlcore::bfgs_minimize(c, theta0, ctl);
  CHECK(r.status == mlcore::BfgsStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(1.0).epsilon(1e-5));
  CHECK(r.theta[1] == doctest::Approx(1.0).epsilon(1e-5));
}

TEST_CASE("bfgs: Rosenbrock with finite-difference gradient") {
  Rosenbrock c;
  mlcore::finite_diff_cost<Rosenbrock> fd{c};
  const double theta0[] = {-1.2, 1.0};
  mlcore::BfgsControl ctl;
  ctl.maxit = 400;
  ctl.grad_tol = 1e-6;
  auto r = mlcore::bfgs_minimize(fd, theta0, ctl);
  CHECK(r.status == mlcore::BfgsStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(1.0).epsilon(1e-4));
  CHECK(r.theta[1] == doctest::Approx(1.0).epsilon(1e-4));
}

TEST_CASE("bfgs: finite_diff_gradient matches analytic gradient on Rosenbrock") {
  Rosenbrock c;
  const double t[] = {0.5, -0.3};
  double g_ana[2];
  c.gradient(t, g_ana);
  double g_fd[2];
  mlcore::finite_diff_gradient(c, t, g_fd);
  CHECK(g_fd[0] == doctest::Approx(g_ana[0]).epsilon(1e-6));
  CHECK(g_fd[1] == doctest::Approx(g_ana[1]).epsilon(1e-6));
}

TEST_CASE("reparam: log round-trip") {
  for (double x : {0.5, 1.0, 7.25, 1e-3, 1e3}) {
    CHECK(mlcore::from_log(mlcore::to_log(x)) ==
          doctest::Approx(x).epsilon(1e-15));
  }
}

TEST_CASE("reparam: log_shift round-trip with c = 2 (nu > 2)") {
  for (double x : {2.5, 3.0, 8.0, 100.0}) {
    CHECK(mlcore::from_log_shift(mlcore::to_log_shift(x, 2.0), 2.0) ==
          doctest::Approx(x).epsilon(1e-15));
  }
}
