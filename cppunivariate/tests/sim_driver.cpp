// Simulation-recovery driver for the validation harness.
//
// Reads a JSON request from argv[1], writes a JSON response to argv[2].
// For each (N, seed) pair, it draws N samples from the named distribution
// at the supplied true parameters, fits via the corresponding *_mle, and
// records the recovered estimates.
//
// Not a ctest — this is invoked by quality/harness/simulate.py during
// `just simulate <id>`. Returning the fits in a single batch avoids
// fork/exec overhead per replicate (the alternative would be ~M*|N|
// process spawns).
//
// Input schema (flat 1D arrays so the existing JSON helpers suffice;
// see quality/harness/simulate.py for the producer):
//   {
//     "id": "std",                 // distribution id
//     "true_params": [m, sd, nu],  // natural-space, in *_mle's Result order
//     "Ns":   [200, 200, ..., 5000, 5000, ...],   // sample size per rep
//     "seeds":[1,    2,   ..., 1, 2, ...],         // RNG seed per rep
//     "tol":  1e-4,
//     "maxit":300
//   }
//
// Output schema:
//   {
//     "id":  "std",
//     "Ns":  [...],
//     "seeds": [...],
//     "estimates": [[m_hat, sd_hat, nu_hat], ...],   // one row per rep
//     "logLiks": [...],
//     "iterations": [...],
//     "statuses":   [...]   // 0 == Ok, see *Status enum for non-zero
//   }

#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/std/dpqr.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "eval_io.hpp"
#include "json_loader.hpp"

#include <cstddef>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using Rng = cppunivariate::distributions::StdRng<std::mt19937_64>;

struct SimRequest {
  std::string id;
  std::vector<double> true_params;
  std::vector<double> Ns;
  std::vector<double> seeds;
  double tol;
  int maxit;
};

SimRequest load_request(const std::string &path) {
  const std::string text = mltest::detail::slurp(path);
  SimRequest r;
  r.id = mltest::detail::parse_string_at(
      text, mltest::detail::find_key(text, "id"));
  r.true_params = mltest::detail::parse_array_at(
      text, mltest::detail::find_key(text, "true_params"));
  r.Ns = mltest::detail::parse_array_at(
      text, mltest::detail::find_key(text, "Ns"));
  r.seeds = mltest::detail::parse_array_at(
      text, mltest::detail::find_key(text, "seeds"));
  std::size_t p = mltest::detail::find_key(text, "tol");
  r.tol = (p == std::string::npos) ? 1e-4 : mltest::detail::parse_number_at(text, p);
  p = mltest::detail::find_key(text, "maxit");
  r.maxit = (p == std::string::npos)
                ? 300
                : static_cast<int>(mltest::detail::parse_number_at(text, p));
  if (r.Ns.size() != r.seeds.size())
    throw std::runtime_error("Ns and seeds must have equal length");
  return r;
}

struct FitResult {
  std::vector<double> estimates;
  double logLik;
  int iterations;
  int status;
};

FitResult fit_std(const std::vector<double> &x, double tol, int maxit) {
  auto r = cppunivariate::student_t_mle(mltest::as_span(x), tol, maxit);
  return {{r.mu, r.sigma, r.nu},
          r.logLik,
          r.iterations,
          static_cast<int>(r.status)};
}

FitResult fit_exp(const std::vector<double> &x, double, int) {
  auto r = cppunivariate::exp_mle(mltest::as_span(x));
  return {{r.rate}, r.logLik, 0, 0};
}

std::vector<double> sample_std(Rng &rng, std::size_t N,
                               const std::vector<double> &theta) {
  const double m = theta[0], sd = theta[1], nu = theta[2];
  std::vector<double> out(N);
  for (std::size_t i = 0; i < N; ++i)
    out[i] = cppunivariate::distributions::rstd(rng, m, sd, nu);
  return out;
}

std::vector<double> sample_exp(Rng &rng, std::size_t N,
                               const std::vector<double> &theta) {
  const double rate = theta[0];
  std::vector<double> out(N);
  for (std::size_t i = 0; i < N; ++i)
    out[i] = cppunivariate::distributions::rexp(rng, rate);
  return out;
}

void write_response(const std::string &path, const SimRequest &req,
                    const std::vector<FitResult> &fits) {
  std::ofstream out(path);
  if (!out)
    throw std::runtime_error("could not open " + path + " for writing");
  std::vector<std::vector<double>> estimates;
  std::vector<double> logLiks, iterations, statuses;
  estimates.reserve(fits.size());
  logLiks.reserve(fits.size());
  iterations.reserve(fits.size());
  statuses.reserve(fits.size());
  for (const auto &f : fits) {
    estimates.push_back(f.estimates);
    logLiks.push_back(f.logLik);
    iterations.push_back(static_cast<double>(f.iterations));
    statuses.push_back(static_cast<double>(f.status));
  }
  out << "{\n";
  out << "  \"id\": \"" << req.id << "\",\n";
  out << "  \"Ns\": " << mltest::detail::fmt_1d(req.Ns) << ",\n";
  out << "  \"seeds\": " << mltest::detail::fmt_1d(req.seeds) << ",\n";
  out << "  \"estimates\": " << mltest::detail::fmt_2d(estimates) << ",\n";
  out << "  \"logLiks\": " << mltest::detail::fmt_1d(logLiks) << ",\n";
  out << "  \"iterations\": " << mltest::detail::fmt_1d(iterations) << ",\n";
  out << "  \"statuses\": " << mltest::detail::fmt_1d(statuses) << "\n";
  out << "}\n";
}

}  // namespace

int main(int argc, char **argv) {
  if (argc != 3) {
    std::cerr << "usage: sim_driver <input.json> <output.json>\n";
    return 64;  // EX_USAGE
  }
  try {
    auto req = load_request(argv[1]);

    std::vector<FitResult> fits;
    fits.reserve(req.Ns.size());
    for (std::size_t i = 0; i < req.Ns.size(); ++i) {
      const std::size_t N = static_cast<std::size_t>(req.Ns[i]);
      const std::uint64_t seed = static_cast<std::uint64_t>(req.seeds[i]);
      Rng rng(seed);
      std::vector<double> sample;
      FitResult fit;
      if (req.id == "std") {
        sample = sample_std(rng, N, req.true_params);
        fit = fit_std(sample, req.tol, req.maxit);
      } else if (req.id == "exp") {
        sample = sample_exp(rng, N, req.true_params);
        fit = fit_exp(sample, req.tol, req.maxit);
      } else {
        throw std::runtime_error("sim_driver: unknown id '" + req.id +
                                 "' (extend dispatch in sim_driver.cpp)");
      }
      fits.push_back(std::move(fit));
    }
    write_response(argv[2], req, fits);
    return 0;
  } catch (const std::exception &e) {
    std::cerr << "sim_driver error: " << e.what() << "\n";
    return 1;
  }
}
