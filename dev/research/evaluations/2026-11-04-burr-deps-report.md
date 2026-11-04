# TASK-116: certified InvBurr and the Pareto IV golden gate

InvBurr now uses the Burr global certificate for its public all-free fit and
its all-free held overload. Pareto IV has a tested explicit C++ certified
entrypoint, `pareto4_mle_certified`, but its public default and R binding retain
the legacy fitter: the certified root exceeds the unchanged frozen golden
coefficient tolerances. All required final development and full suites passed
on this configuration. No commit was made.

Tested base: `54b0d06a3e447f944940fc0dd4b5b87235d2acc2`, worktree
`codex/burr-deps`. The uncommitted source, certificate engine, fixtures and
reproducers are identified by SHA256 in
[validation.json](2026-11-04-burr-deps-data/validation.json). The
[evidence instructions](2026-11-04-burr-deps-data/README.md) give the commands
and distinguish transformation checks, independent numerical checks and the
inherited mathematical certificate.

The mathematical transfer is complete for both families. For positive data,
reciprocation is a bijection, with `y_i = 1/x_i`, unchanged shapes and
`scale_x = 1/scale_y`. Its log likelihood is
`ell_x = ell_y - 2 sum(log(x_i))`. The additive Jacobian is independent of
all fitted coefficients, so it preserves finite maximizers, uniqueness,
strict likelihood comparisons and unattained suprema. The Weibull face
becomes inverse Weibull with unchanged limiting shape and reciprocal limiting
scale. The Pareto face becomes power, with endpoint `max(x)` and power equal
to the Pareto tail exponent.

For Pareto IV with threshold `min` held, `y_i = x_i - min` has unit Jacobian.
The three regular coefficients and log likelihood are unchanged. Burr's
Weibull and Pareto faces become shifted Weibull and shifted Pareto; their
fitters operate on `x - min`. This argument does not estimate the threshold.
The existing structural restrictions on a free threshold remain unchanged.

The guarantee is **certified in exact arithmetic, conditional on the computed
comparisons, with the Burr engine's stated practical floating-point margin**.
The [Burr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md)
and [production certificate report](2026-11-04-burr-port-report.md) specify
that margin and its libm assumptions. No outward-rounded arithmetic or
universal joint uniqueness theorem is claimed. Each accepted finite outcome
inherits a sample-specific unique global maximum certificate. The independent
numerical checks below corroborate the returned approximation; they do not
supply the global proof.

The [InvBurr fitter](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp)
now calls `burr_mle_certified`. The unused legacy BFGS seed solve was removed
from this path; historical starting controls remain accepted and validated.
Limit likelihoods transport the certificate value with the Jacobian, without
a second face optimizer. Boundary fields retain the existing sentinel
convention: infinite shape1 for inverse Weibull, or zero shape1 and infinite
shape2 for power; their scale field describes the limiting model. The power
endpoint is taken directly from the original maximum, avoiding a one-ulp
support error from a reciprocal round trip.

`CertificateTie`, `CertificateBudget` and `NumericalFailure` remain distinct
InvBurr statuses. The [common adapter](../../../bindings/common/distributions/invburr/fit.hpp)
turns them into explicit errors with “no verified MLE”; it cannot rescue a
failed global certificate through a local score check. Winning faces retain
`mlinvweibull` and `mlpower` recommendations. Constant samples retain the
unbounded-likelihood stop. Successful finite results additionally require a
positive finite transformed scale and finite transformed log likelihood.

Both certified transformations reject arithmetic overflow and the merging of
distinct original observations into equal transformed values. These numerical
stops prevent artificial constant samples or endpoint multiplicities from
entering the certificate. Genuine original ties remain allowed. The guard is
restricted to the new all-free paths. Held-subset algorithms and GB2's legacy
seed are unchanged. The [InvBurr dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md)
updates `fit`, existence, controls and all-free limit detection; the
[Pareto IV dossier](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/README.md)
keeps production metadata on the legacy default and documents its certified
candidate and gate.

Native cases were appended to both families' `test.cpp` files. They transport
all 51 retained Burr certificate workloads and compare statuses, coefficients
and likelihoods against the actual binary64 transformed delegate. Finite
likelihoods are also summed through the unchanged family densities. InvBurr's
all-free held entrypoint is compared with its public entrypoint. Additional
cases cover invalid controls and starts, reciprocal overflow, excess overflow,
artificial ties and constant samples. Existing golden, independent oracle,
score, equivariance and exhaustive held-subset cases remain unchanged. New
exported [R tests](../../../bindings/r/distributions/invburr/test.R) check
InvBurr's tie, budget, numerical, constant-sample and transformation diagnostics.

The [80-digit check](2026-11-04-burr-deps-data/verification.json) independently
solves analytic total log-likelihood scores on the frozen binary64 golden
inputs and their computed transformations. Scores use log shape1, log shape2
and log Burr scale coordinates; inversion changes only the last coordinate's
sign. The objective is the total likelihood, without mean normalization.
Acceptance thresholds are `1e-10` relative coefficient error, `1e-8` absolute
likelihood error and `1e-7` total-score norm.

| Golden sample | Maximum relative error of certified coefficients | Absolute likelihood error | Total-score norm |
|---|---:|---:|---:|
| InvBurr | 3.49e-16 | 4.31e-13 | 1.69e-13 |
| Pareto IV candidate | 4.57e-15 | 1.37e-12 | 2.96e-13 |

**Open: Pareto IV public default and binding switch.** An actual default-switch
attempt failed two native coefficient assertions and the R golden scale
assertion; the likelihood and independent score checks passed. The retained
[failure excerpt](2026-11-04-burr-deps-data/compatibility.txt) and independent
root check reproduce the conflict. The root's relative differences from the
frozen Pareto IV golden are:

| Coefficient | Root / golden - 1 |
|---|---:|
| shape1 | -2.73439938e-8 |
| shape2 | 6.59197387e-8 |
| scale | 2.00308626e-7 |

The unchanged native allowance is `1e-8` and the R allowance is `1e-7`.
The explicit [C++ candidate](../../../univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp)
agrees with the independent root, so moving its root closer to the historical
coefficients would conceal the compatibility conflict. No golden or tolerance
was changed. Resolving this gate requires authorization to correct the frozen
Pareto IV reference or adjust its regression allowances. The remaining coupled
implementation is to route the public all-free branch to the existing candidate,
enable separate certificate diagnostics in its common adapter, and retain the
exported R refusal tests from the rejected switch attempt. There is no missing
mathematical transformation argument. InvBurr passes every unchanged golden
tolerance and therefore remains switched.

The final [timing run](2026-11-04-burr-deps-data/timing.txt) uses the unchanged
500-observation golden from each family, one discarded warmup and seven
alternating legacy/certified pairs, with within-sample medians. The InvBurr
baseline is its retained original public implementation, including its seed
solve. Pareto IV compares the current public default with the complete
certified candidate, including excess construction and transformation guards.
Both compile together with C++17 and `-O2` and run in one local CPU slot.

| Fit | Legacy | Certified | Ratio |
|---|---:|---:|---:|
| InvBurr | 19.44 ms | 84.52 ms | 4.35 |
| Pareto IV | 12.25 ms | 94.37 ms | 7.70 |

These are paired snapshots on a shared workstation, without a cross-machine
performance bound. The certificate uses the unchanged 4096-profile budget;
`iterlim` controls proposal solves rather than enlarging that global budget.

All final commands finished in the foreground on the **local workstation**,
through the prescribed locks, with nice scheduling and BLAS/OpenMP pinned to
one thread. Shared-slot waits were allowed to finish. Final results and full
log hashes are retained in `validation.json`.

| Final command | Result |
|---|---|
| `/tmp/umvl-cpu/run4 nice -n 10 just cpp-test` | 106/106 dev shards passed |
| `/tmp/umvl-cpu/run4 nice -n 10 just cpp-test-no-long-double` | 127/127 passed |
| `/tmp/umvl-cpu/run4 nice -n 10 just test` | FAIL 0, WARN 0, SKIP 153, PASS 20518 |
| `/tmp/umvl-cpu/run4 nice -n 10 env UMVL_TEST_TIER=full just cpp-test` | 127/127 full shards passed |
| `/tmp/umvl-cpu/run4 nice -n 10 just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 43335 |

The 153 dev skips are expected tier skips; full skips are zero. The failed
default-switch experiment is separate from these final green runs. No new
permitted test failures were introduced. Environment: Linux 7.0.0-38 x86-64,
glibc 2.43, GCC 15.2.0, R 4.6.1, Python 3.14.5 and mpmath 1.3.0. Other
compilers, operating systems and JavaScript/wasm were not run in this job.

No sub-agents, simbox, dependency installations, board edits, Git writes or
publication were used. All authored changes are in this worktree. Settled
d/p/q/r, `special/`, truth fixtures and code-generation sources were left
unchanged; no changes to those components are needed. The broader Burr dossier
still names InvBurr in its legacy-dependent sentence; correcting that sentence
is outside this job's restriction to the two dependent families. Its historical
port report remains valid as a dated record.

The requested artifact-pane display was attempted after writing the report.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. No viewer configuration, main checkout, other worktree or
external copy was changed; the report remains at the requested worktree path.
