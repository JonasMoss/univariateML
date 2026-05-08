// Smoke tests for the multi-D Newton minimiser. Closed-form problems with
// known analytic gradients and Hessians, plus an FD-fallback case to validate
// the `finite_diff_hessian_cost` adapter end-to-end.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "../../src/ml/core/cholesky.hpp"
#include "../../src/ml/core/finite_diff.hpp"
#include "../../src/ml/core/newton_md.hpp"

#include <cmath>

namespace {

// f(x, y) = (x - 3)^2 + (y + 1)^2 — exactly quadratic, single Newton step
// from any start should land on the optimum.
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
  void hessian(const double *, double *H) const {
    H[0] = 2.0; H[1] = 0.0;
    H[2] = 0.0; H[3] = 2.0;
  }
};

// Ill-conditioned ellipse: condition number 1e6.
struct IllEllipse {
  int dim() const { return 2; }
  double operator()(const double *t) const {
    return 1.0 * t[0] * t[0] + 1e6 * t[1] * t[1];
  }
  void gradient(const double *t, double *g) const {
    g[0] = 2.0 * t[0];
    g[1] = 2e6 * t[1];
  }
  void hessian(const double *, double *H) const {
    H[0] = 2.0; H[1] = 0.0;
    H[2] = 0.0; H[3] = 2e6;
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
  void hessian(const double *t, double *H) const {
    const double x = t[0];
    const double y = t[1];
    // d2f/dx2 = 2 - 400*(y - 3 x^2); d2f/dxdy = -400 x; d2f/dy2 = 200
    H[0] = 2.0 - 400.0 * (y - 3.0 * x * x);
    H[1] = -400.0 * x;
    H[2] = -400.0 * x;
    H[3] = 200.0;
  }
};

// f(x, y) = (x^2 - 1)^2 + y^2. Two minima at (±1, 0); H_xx = 12 x^2 - 4 is
// indefinite for |x| < 1/sqrt(3). From (0.5, 0.1) the start has indefinite
// H but the optimiser should ramp lambda and converge to (1, 0).
struct IndefStart {
  int dim() const { return 2; }
  double operator()(const double *t) const {
    const double a = t[0] * t[0] - 1.0;
    return a * a + t[1] * t[1];
  }
  void gradient(const double *t, double *g) const {
    g[0] = 4.0 * t[0] * (t[0] * t[0] - 1.0);
    g[1] = 2.0 * t[1];
  }
  void hessian(const double *t, double *H) const {
    H[0] = 12.0 * t[0] * t[0] - 4.0;
    H[1] = 0.0;
    H[2] = 0.0;
    H[3] = 2.0;
  }
};

}  // namespace

TEST_CASE("newton_md: quadratic converges in one step") {
  Quadratic c;
  const double theta0[] = {0.0, 0.0};
  auto r = cppunivariate::newton_md_minimize(c, theta0);
  CHECK(r.status == cppunivariate::NewtonMdStatus::Ok);
  // Levenberg ridge (lambda0 = 1e-8) biases the first Newton step by ~5e-9
  // for a problem with H = 2I. After convergence the bias is gone, but
  // step_tol can fire before grad_tol on a second well-conditioned step.
  CHECK(r.theta[0] == doctest::Approx(3.0).epsilon(1e-6));
  CHECK(r.theta[1] == doctest::Approx(-1.0).epsilon(1e-6));
  CHECK(r.iterations <= 3);
}

TEST_CASE("newton_md: ill-conditioned ellipse") {
  IllEllipse c;
  const double theta0[] = {1.0, 1.0};
  auto r = cppunivariate::newton_md_minimize(c, theta0);
  CHECK(r.status == cppunivariate::NewtonMdStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(0.0).epsilon(1e-8));
  CHECK(r.theta[1] == doctest::Approx(0.0).epsilon(1e-8));
}

TEST_CASE("newton_md: Rosenbrock with analytic Hessian") {
  Rosenbrock c;
  const double theta0[] = {-1.2, 1.0};
  cppunivariate::NewtonMdControl ctl;
  ctl.maxit = 200;
  ctl.grad_tol = 1e-8;
  auto r = cppunivariate::newton_md_minimize(c, theta0, ctl);
  CHECK(r.status == cppunivariate::NewtonMdStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(1.0).epsilon(1e-5));
  CHECK(r.theta[1] == doctest::Approx(1.0).epsilon(1e-5));
}

TEST_CASE("newton_md: Rosenbrock with FD Hessian via adapter") {
  Rosenbrock c;
  cppunivariate::finite_diff_hessian_cost<Rosenbrock> fd{c};
  const double theta0[] = {-1.2, 1.0};
  cppunivariate::NewtonMdControl ctl;
  ctl.maxit = 200;
  ctl.grad_tol = 1e-5;
  auto r = cppunivariate::newton_md_minimize(fd, theta0, ctl);
  CHECK(r.status == cppunivariate::NewtonMdStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(1.0).epsilon(1e-3));
  CHECK(r.theta[1] == doctest::Approx(1.0).epsilon(1e-3));
}

TEST_CASE("newton_md: indefinite Hessian at start ramps lambda and recovers") {
  IndefStart c;
  const double theta0[] = {0.5, 0.1};  // H_xx = 12*0.25 - 4 = -1 (indefinite)
  auto r = cppunivariate::newton_md_minimize(c, theta0);
  CHECK(r.status == cppunivariate::NewtonMdStatus::Ok);
  CHECK(r.theta[0] == doctest::Approx(1.0).epsilon(1e-5));
  CHECK(r.theta[1] == doctest::Approx(0.0).epsilon(1e-5));
}

TEST_CASE("newton_md: NaN start returns BadInit") {
  Quadratic c;
  const double nan = std::nan("");
  const double theta0[] = {nan, 0.0};
  auto r = cppunivariate::newton_md_minimize(c, theta0);
  CHECK(r.status == cppunivariate::NewtonMdStatus::BadInit);
}

TEST_CASE("finite_diff_hessian agrees with analytic on Rosenbrock") {
  Rosenbrock c;
  const double t[] = {0.5, -0.3};
  double H_ana[4];
  c.hessian(t, H_ana);
  double H_fd[4];
  cppunivariate::finite_diff_hessian(c, t, H_fd);
  CHECK(H_fd[0] == doctest::Approx(H_ana[0]).epsilon(1e-4));
  CHECK(H_fd[1] == doctest::Approx(H_ana[1]).epsilon(1e-4));
  CHECK(H_fd[2] == doctest::Approx(H_ana[2]).epsilon(1e-4));
  CHECK(H_fd[3] == doctest::Approx(H_ana[3]).epsilon(1e-4));
  // Symmetry exact (we mirror in the implementation).
  CHECK(H_fd[1] == H_fd[2]);
}

TEST_CASE("cholesky: factor + solve a known SPD system") {
  // A = [[4, 2], [2, 3]] is SPD; A^{-1} = [[3/8, -1/4], [-1/4, 1/2]].
  double A[4] = {4.0, 2.0, 2.0, 3.0};
  CHECK(cppunivariate::cholesky_factor(A, 2));
  // Solve A x = (1, 0) -> x = column 0 of A^{-1} = (3/8, -1/4)
  double x[2] = {1.0, 0.0};
  cppunivariate::cholesky_solve(A, x, 2);
  CHECK(x[0] == doctest::Approx(0.375).epsilon(1e-12));
  CHECK(x[1] == doctest::Approx(-0.25).epsilon(1e-12));
}

TEST_CASE("cholesky: invert_symmetric_pd matches by-hand inverse") {
  double A[4] = {4.0, 2.0, 2.0, 3.0};
  CHECK(cppunivariate::invert_symmetric_pd(A, 2));
  CHECK(A[0] == doctest::Approx(0.375).epsilon(1e-12));
  CHECK(A[1] == doctest::Approx(-0.25).epsilon(1e-12));
  CHECK(A[2] == doctest::Approx(-0.25).epsilon(1e-12));
  CHECK(A[3] == doctest::Approx(0.5).epsilon(1e-12));
}

TEST_CASE("cholesky: factor returns false on non-PD matrix") {
  double A[4] = {1.0, 2.0, 2.0, 1.0};  // det = -3
  CHECK_FALSE(cppunivariate::cholesky_factor(A, 2));
}
