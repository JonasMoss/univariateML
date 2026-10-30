# nhyper implementation report

No commits or git write commands.

## Design and mathematics

**Proved:** extraDistr's observation is TOTAL draws T until the r-th white
ball, not black draws. For r>0, r <= T <= n+r; r=0 is defined as T=0.
P(T=x) = (r/x) P(H_x=r), H_x hypergeometric with m white and n black.
P(T<=q) = P(H_floor(q)>=r). These identities supply stable density/CDFs.

Known population N and stopping count r are design constants. Estimate
integer m (white balls), with n=N-m; optionally hold m to evaluate likelihood.
As in hyper, N is an input sum constraint, not a redundant coefficient.
The metadata marks r required; NULL N produces an explicit design error.
Goldens supply both via fit_args. Fixed attributes give df 1 or 0.

For h observations, feasible m is [r, N-max(x)+r] on the integers.
L(m)/L(m-1) = product_i m/(m-r) *
(N-m+1-x_i+r)/(N-m+1). Both factors are nonincreasing in m, strictly
for the first when r>0. Thus likelihood is log concave and bisection
finds a global maximizer. An adjacent equality returns the smaller with
warning, using the existing hyper compensated ratio/tie machinery.
All observations equal r attain m=N. All x=0 at r=0 leave every m
unidentified (return 0, warn); N=0 admits one composition.
Likelihood is bounded by 1 and the feasible set is finite: **no feasible
sample has an infinite MLE under this known-N design**. No escape status
or limits entry is appropriate for this finite, closed model.

For r>0, as n,m grow with m/(m+n)->p in (0,1), T-r converges to
NB(size=r,prob=p). Each ordered stopped sequence has limiting probability
p^r*(1-p)^(x-r); there are choose(x-1,r-1) such sequences.
The limit-model probability MLE is r/mean(T), so the correct referral is
`mlnbinom(x - r, size = r)`, not `mlnbinom(x, size = r)`.
Joint unknown population/composition and inverse-sampling population
estimation are explicitly unsupported. No sample criterion for their
unrestricted infinite maximum is claimed.

**Checked numerically:** 1,540 single-observation small-urn likelihood grids;
120 varied iid samples against independent full integer reference grids;
C++ boundary/tie/status and tail inversion checks; large-population log masses;
28-point negative-binomial limit comparison, seeded reference RNG agreement,
500-observation golden and symbolic log likelihood.
**Assumed:** independent repetitions restore the same urn, with known N,r.

## Files changed

Own folder `univariateML-cpp/include/cppunivariate/distributions/nhyper/`:
README.md, mle.hpp, dpqr.hpp, adapter.cpp, test.cpp, test.R, golden.json.
Shared files (each separately):

- univariateML-cpp/include/cppunivariate/cppunivariate.hpp — include registration.
- bindings/r/CANONICAL_NAMES.md — coefficient/reference registration.
- bindings/r/R/code_helpers.R — runtime-checked d/p/q/r reference repairs.
- bindings/r/tests/testthat/test_input_checks.R — design argument switch.
- bindings/r/tests/testthat/test-audit-input.R — design argument switch.
- codex-report.md — this report.

## Validation

- `UMVL_GOLDEN_IDS=nhyper just goldens`: 1 new golden; existing goldens unchanged.
- `just cpp-test`: 5/5 ctest targets, 0 failures.
- Focused C++ `nhyper*`: 4/4 cases; 4,775/4,775 assertions.
- `just quality nhyper`: 1/1 passed (integer parameters held; symbolic LL check).
- Final `just test`: FAIL 0 / WARN 0 / SKIP 1 / PASS 6842. The skip is the
  unchanged optional {sn} reference. The first run exposed two test-fixture
  errors (sample() singleton semantics and retained attributes), now fixed.

All builds/tests/computations run through /tmp/umvl-cpu/run, one thread.

## Reference defects: ready-to-paste KNOWN_ISSUES rows

| Package | Defect | Reproducer | Effect here |
|---|---|---|---|
| extraDistr 1.10.0.5 | `nhyper_table` indexes empty vectors when n=0 (including n=m=r=0) | `extraDistr::dnhyper(0, 0, 0, 0)` (run in a disposable R process) | runtime repair defines degenerate urns; d/p/q/r repaired together |
| extraDistr 1.10.0.5 | Negative-hypergeometric counts are converted to 32-bit integers | `extraDistr::dnhyper(3, 1e12, 1e12, 3, log=TRUE)` throws "value out of integer range", should be about `log(1/8)` | runtime repair uses stable base hypergeometric probabilities and binary inversion |
| extraDistr 1.10.0.5 | `dnhyper(log=TRUE)` logs an underflowed mass | `extraDistr::dnhyper(1003,1000,1000,3,log=TRUE)` is -Inf, should be about -1369.143 | repaired at runtime with the hypergeometric log mass |
| extraDistr 1.10.0.5 | `pnhyper` upper log tails subtract from one | `extraDistr::pnhyper(500,1000,1000,3,lower.tail=FALSE,log.p=TRUE)` is -Inf, should be about -418.2051 | repaired at runtime with the direct hypergeometric tail |
| extraDistr 1.10.0.5 | `qnhyper` loses extreme upper log probabilities by conversion/complement and lower-CDF rounding | `extraDistr::qnhyper(-1000,1000,1000,3,lower.tail=FALSE,log.p=TRUE)` returns 63, should be 886 | runtime repair inverts the requested log tail directly |

Each entry has a safe finite-tail broken() probe on a 1000-black urn,
never probing the crashing boundary. RNG uses the common table-backend
density sentinel; p/q check their own log-tail defects.
The native boundary crash was observed
in a separate Rscript process (exit 139).

## Open questions / limitations

- Known N is deliberately the supported design; no claim to fitting the
  two-free-count family or to a global sample escape criterion for it.
- C++ scalar CDF/quantile sum hypergeometric tails; very broad central tails
  can be expensive. R uses base phyper and binary inversion.
- Runtime repairs use base hypergeometric identities, preserving reference
  total-draw parameterization; upstream is not changed.
- The existing generic R input guard limits observations to integer.max,
  though core counts/design quantities support 2^53-1.
- Existing bootstrapml reconstructs held parameters by coefficient lookup;
  the synthetic held sum N (also used by hyper) is absent from coefficients.
  Such bootstrap refits need a shared infrastructure fix outside this job.

Upstream C++ source inspected: nhyper_table allocates vectors of length n
and accesses h[0], t[n-1] without an n=0 guard. Native d/log d uses a
normalized probability table; p upper tails subtract from one; q exp()s
log probabilities before inversion; r uses the same inverse table.
Source: https://raw.githubusercontent.com/twolodzko/extraDistr/master/src/negative-hypergeometric-distribution.cpp
(saved under /tmp/umvl-codex/nhyper/upstream.cpp).
