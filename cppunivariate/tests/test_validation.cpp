// AI-first validation harness — C++ side of the round-trip.
//
// Reads quality/derived/<id>/eval_request.json (data + theta sweep),
// evaluates each registered Cost at every theta, and writes
// quality/derived/<id>/cpp_eval.json. The Python harness then diffs
// the result against the symbolically-derived ground truth in
// symbolic_grid.json.
//
// Pilot id set: exp. Phase 3 adds std; Phase 4 adds norm. Each id
// gets a TEST_CASE so doctest reports failures per-distribution.
//
// Smoke-checks built into this target:
//   - nll/grad finite at the MLE
//   - score-residual ||g||_2 < 1e-3 at the MLE (mirrors the existing
//     gate from test_estimators.cpp, applied uniformly via the harness
//     rather than per-distribution boilerplate)
//
// The substantive numerical match (grad/Hess vs symbolic at 1e-12) is
// asserted Python-side by `python -m quality.harness validate <id>`.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "cppunivariate/core/finite_diff.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "eval_io.hpp"

#include <cmath>
#include <cstdlib>
#include <functional>
#include <map>
#include <string>
#include <vector>

namespace {

std::string validation_dir() {
  const char *dir = std::getenv("UMVL_VALIDATION_DIR");
  return dir ? dir : "../../quality/derived";
}

struct CostEval {
  double nll;
  std::vector<double> grad;
  std::vector<std::vector<double>> hess;
};

using CostFactory = std::function<CostEval(const std::vector<double> &,
                                           const std::vector<double> &)>;

// Evaluator for Costs with full analytical interface (operator/gradient/hessian).
// Used by exp/lomax/most-distros — see std variant below for the FD-grad case.
template <typename Cost>
CostEval eval_with(const std::vector<double> &data,
                   const std::vector<double> &theta) {
  Cost cost{mltest::as_span(data)};
  CostEval out;
  out.nll = cost(theta.data());
  const std::size_t n = static_cast<std::size_t>(cost.dim());
  out.grad.assign(n, 0.0);
  cost.gradient(theta.data(), out.grad.data());
  std::vector<double> H_flat(n * n, 0.0);
  cost.hessian(theta.data(), H_flat.data());
  out.hess.assign(n, std::vector<double>(n, 0.0));
  for (std::size_t i = 0; i < n; ++i)
    for (std::size_t j = 0; j < n; ++j)
      out.hess[i][j] = H_flat[i * n + j];
  return out;
}

// Evaluator for discrete distros whose Cost takes ispan<int> (pois, geom,
// nbinom, lgser, etc.). Round-trips the harness's double `data` array into
// a transient int vector whose lifetime spans the cost evaluation.
template <typename Cost>
CostEval eval_int(const std::vector<double> &data,
                  const std::vector<double> &theta) {
  std::vector<int> int_data;
  int_data.reserve(data.size());
  for (double v : data) int_data.push_back(static_cast<int>(v));
  Cost cost{cppunivariate::ispan{int_data.data(), int_data.size()}};
  CostEval out;
  out.nll = cost(theta.data());
  const std::size_t n = static_cast<std::size_t>(cost.dim());
  out.grad.assign(n, 0.0);
  cost.gradient(theta.data(), out.grad.data());
  std::vector<double> H_flat(n * n, 0.0);
  cost.hessian(theta.data(), H_flat.data());
  out.hess.assign(n, std::vector<double>(n, 0.0));
  for (std::size_t i = 0; i < n; ++i)
    for (std::size_t j = 0; j < n; ++j)
      out.hess[i][j] = H_flat[i * n + j];
  return out;
}

// Evaluator for Costs whose gradient is computed via finite differences in
// production (std, snorm, sstd, sged, ged, paralogis — the BFGS-with-FD
// family). We mirror the production path: wrap the inner Cost with
// finite_diff_cost to get a numerical gradient. The harness skips the
// gradient-vs-symbolic check at 1e-12 for these (see Spec.cost_has_gradient);
// the FD grad lives in cpp_eval.json as advisory + gates only the
// score-residual smoke-check at the MLE.
template <typename Cost>
CostEval eval_fd_grad(const std::vector<double> &data,
                      const std::vector<double> &theta) {
  Cost cost{mltest::as_span(data)};
  cppunivariate::finite_diff_cost<Cost> fd{cost};
  CostEval out;
  out.nll = cost(theta.data());
  const std::size_t n = static_cast<std::size_t>(cost.dim());
  out.grad.assign(n, 0.0);
  fd.gradient(theta.data(), out.grad.data());
  std::vector<double> H_flat(n * n, 0.0);
  cost.hessian(theta.data(), H_flat.data());
  out.hess.assign(n, std::vector<double>(n, 0.0));
  for (std::size_t i = 0; i < n; ++i)
    for (std::size_t j = 0; j < n; ++j)
      out.hess[i][j] = H_flat[i * n + j];
  return out;
}

void run_validation(const std::string &id, CostFactory fn) {
  const std::string base = validation_dir();
  const std::string req_path = base + "/" + id + "/eval_request.json";
  auto req = mltest::load_eval_request(req_path);
  REQUIRE(req.id == id);
  REQUIRE(req.thetas.size() > 0);
  REQUIRE(req.data.size() > 0);

  std::vector<double> nlls;
  std::vector<std::vector<double>> grads;
  std::vector<std::vector<std::vector<double>>> hessians;
  nlls.reserve(req.thetas.size());
  grads.reserve(req.thetas.size());
  hessians.reserve(req.thetas.size());

  for (const auto &theta : req.thetas) {
    REQUIRE(theta.size() == req.n_params);
    auto e = fn(req.data, theta);
    nlls.push_back(e.nll);
    grads.push_back(std::move(e.grad));
    hessians.push_back(std::move(e.hess));
  }

  mltest::write_cpp_eval(base + "/" + id + "/cpp_eval.json", req, nlls,
                         grads, hessians);

  // Smoke-check: at the MLE (thetas[0]), values are finite.
  CHECK(std::isfinite(nlls[0]));
  for (double g : grads[0]) CHECK(std::isfinite(g));

  // Score-residual gate at the MLE (uniform across all registered ids).
  double gnorm2 = 0.0;
  for (double g : grads[0]) gnorm2 += g * g;
  CHECK(std::sqrt(gnorm2) < 1e-3);
}

}  // namespace

TEST_CASE("validation::exp") {
  run_validation("exp", eval_with<cppunivariate::ExpCost>);
}

TEST_CASE("validation::std") {
  run_validation("std", eval_fd_grad<cppunivariate::StudentTCost>);
}

TEST_CASE("validation::norm") {
  run_validation("norm", eval_with<cppunivariate::NormCost>);
}

TEST_CASE("validation::gamma") {
  run_validation("gamma", eval_with<cppunivariate::GammaCost>);
}

TEST_CASE("validation::rayleigh") {
  run_validation("rayleigh", eval_with<cppunivariate::RayleighCost>);
}

TEST_CASE("validation::weibull") {
  run_validation("weibull", eval_with<cppunivariate::WeibullCost>);
}

TEST_CASE("validation::gumbel") {
  run_validation("gumbel", eval_with<cppunivariate::GumbelCost>);
}

TEST_CASE("validation::logis") {
  run_validation("logis", eval_with<cppunivariate::LogisticCost>);
}

TEST_CASE("validation::lnorm") {
  run_validation("lnorm", eval_with<cppunivariate::LognormalCost>);
}

TEST_CASE("validation::cauchy") {
  run_validation("cauchy", eval_with<cppunivariate::CauchyCost>);
}

TEST_CASE("validation::llogis") {
  run_validation("llogis", eval_with<cppunivariate::LogLogisticCost>);
}

TEST_CASE("validation::kumar") {
  run_validation("kumar", eval_with<cppunivariate::KumarCost>);
}

TEST_CASE("validation::betapr") {
  run_validation("betapr", eval_with<cppunivariate::BetaPrimeCost>);
}

TEST_CASE("validation::beta") {
  run_validation("beta", eval_with<cppunivariate::BetaCost>);
}

TEST_CASE("validation::naka") {
  run_validation("naka", eval_with<cppunivariate::NakaCost>);
}

TEST_CASE("validation::invgamma") {
  run_validation("invgamma", eval_with<cppunivariate::InvGammaCost>);
}

TEST_CASE("validation::burr") {
  run_validation("burr", eval_with<cppunivariate::BurrCost>);
}

TEST_CASE("validation::invburr") {
  run_validation("invburr", eval_with<cppunivariate::InvBurrCost>);
}

TEST_CASE("validation::invweibull") {
  run_validation("invweibull", eval_with<cppunivariate::InvWeibullCost>);
}

TEST_CASE("validation::gompertz") {
  run_validation("gompertz", eval_with<cppunivariate::GompertzCost>);
}

TEST_CASE("validation::invgauss") {
  run_validation("invgauss", eval_with<cppunivariate::InvGaussCost>);
}

TEST_CASE("validation::logitnorm") {
  run_validation("logitnorm", eval_with<cppunivariate::LogitnormCost>);
}

TEST_CASE("validation::paralogis") {
  run_validation("paralogis", eval_fd_grad<cppunivariate::ParalogisCost>);
}

TEST_CASE("validation::ged") {
  run_validation("ged", eval_fd_grad<cppunivariate::GedCost>);
}

TEST_CASE("validation::pois") {
  run_validation("pois", eval_int<cppunivariate::PoisCost>);
}

TEST_CASE("validation::geom") {
  run_validation("geom", eval_int<cppunivariate::GeomCost>);
}

TEST_CASE("validation::nbinom") {
  run_validation("nbinom", eval_int<cppunivariate::NbinomCost>);
}

TEST_CASE("validation::lgser") {
  run_validation("lgser", eval_int<cppunivariate::LgserCost>);
}
