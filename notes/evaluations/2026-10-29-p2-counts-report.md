# TASK-81: P2 counts

Branch: `codex/p2-counts`. No commits, board edits, dependency installs, shared
source edits, domain expansions, or golden changes.

## Files

Within `cppunivariate/include/cppunivariate/distributions/`:

- `pois`, `geom`, `nbinom`, `zip`, `zinb`, `zmpois`, `ztpois`, `zmgeom`,
  `ztgeom`, `zmnbinom`, `ztnbinom`, `zmlogarithmic`, `lgser`: `README.md`,
  `adapter.cpp`, `mle.hpp`, `test.R`, `test.cpp`.
- `pig`: `test.R`, `test.cpp`; its existing holds needed no implementation.
- This report is the only file added outside the family folders.

## Holds

All 19 missing coefficient arguments now default to `NULL`; C++ uses NaN for
free coefficients. Held values are preserved exactly and reported in `fixed`.
This adds 33 nonempty hold combinations; all 40 nonempty combinations across
these 14 families are exercised, including the seven previously implemented.

| Family | Added holds and method |
|---|---|
| pois | lambda: direct Poisson log likelihood |
| geom | prob: direct geometric log likelihood |
| nbinom | prob, size+prob: decreasing size score/root or direct likelihood |
| zip | lambda, pi, both: clipped inflation profile, bounded scalar rate profiles, direct likelihood |
| zinb | prob, pi and all combinations with size: exact inflation profile, bounded base-mean profiles, scalar size search, direct likelihood |
| zmpois | lambda, p0, both: Bernoulli zero factor plus positive truncated Poisson likelihood |
| ztpois | lambda: direct conditional likelihood, including its point-mass endpoint |
| zmgeom | prob, p0, both: Bernoulli zero factor plus positive geometric likelihood |
| ztgeom | prob: direct conditional likelihood, including its point-mass endpoint |
| zmnbinom | prob, p0 and all combinations with size: Bernoulli factor plus truncated NB profiles |
| ztnbinom | prob, size+prob: size profile including the attained logarithmic member |
| zmlogarithmic | prob, p0, both: Bernoulli factor plus direct logarithmic likelihood |
| lgser | theta: direct log-series likelihood |
| pig | Existing mean/shape holds retained; additional regression/oracle coverage |

No missing hold was classified `never`: none was converted to `no-mle` and
none remains `not-implemented`. Incompatible endpoint holds stop explicitly.
All-zero nonunique modified fits preserve supplied base parameters and p0.
A held empirical zero fraction leaves the conditional shape fit unchanged.

New positive-size-domain zero escapes are declared for held-prob NB and
held-prob/held-inflation ZINB. Held-inflation ZIP all-zero fits stop because
the reference does not evaluate lambda zero. Held zero ZIP rate is rejected
with its reference-domain reason. Held-inflation ZINB compares against the
ZIP face retaining that inflation. Held-p0 modified NB retains its conditional
zero-modified Poisson escape. Insufficient scalar-search iterations stop.

## Mathematical versus numerical evidence

Exact identities/arguments inherited from P0: modified likelihood
factorization; clipped inflation solutions; strict concavity of NB in size
at held probability; conditional-mean equations; attained size-zero
logarithmic member of truncated/modified NB. Stationary fixed-inflation
base means lie between the sample mean and positive sample mean, yielding
a compact inner search. The NB held-prob score uses stable reciprocal sums
and a digamma-difference expansion near probability one. Held-probability
NB likelihoods use stable rising factorials at the resulting large sizes.

Scalar grids refine bracketed peaks and compare declared limiting faces.
They are numerical searches, not interval-certified global maximizers;
no unimodality proof is claimed for mixture/truncated-NB/PIG profiles.
The independent R oracle enumerates free endpoints and uses three optim
starts on former reference densities; PIG uses an independent scaled-Bessel
formula to avoid actuar cancellation. Separated mixture peaks receive
additional nine-start checks.

## Validation

Final simbox commands:

- `just quality pois geom nbinom zip zinb zmpois ztpois zmgeom ztgeom zmnbinom ztnbinom zmlogarithmic lgser pig`: 14/14 pass.
- `just cpp-test`: six targets completed; only the two allowed assertions
  failed (`betapr/test.cpp:33`, `std/test.cpp:40`). Distribution suite:
  444 cases, 442 passed, two known failures; 513,868 assertions, 513,866 passed.
  The other five targets passed. All 88 count-family cases passed, including the new NB likelihood regression.
- `just test`: 11,736 passed expectations, three allowed failures, 16 warnings,
  zero skipped. Failures were only frozen `sstd` quantiles and the `bnbinom`
  and `std` goldens. All 71 count-family R test blocks and generic conformance
  passed. Both test commands exit nonzero solely because of these known
  machine-dependent failures; there are no additional failures.

Logs: `/tmp/umvl-codex/p2-counts/{cpp-final,r-final,quality-simbox}.log`.

Added tests comprise 37 R test blocks and 28 C++ test cases.
The oracle sweep covers 200 held-sample configurations: every nonempty
subset, n=20/100, ties, n=2 and a Poisson/truncated-Poisson limit sample.
Family files contain 71 R test blocks and 88 C++ test cases in total.
Additional cases cover incompatible endpoints, zero-only and all-one
samples, zero-share holds, logarithmic members, held-inflation escapes,
iteration failure and NB probability close to one.

## Open questions

- Certified global optimization remains open, as in the P0 analysis.
- NB size-zero and PIG shape-zero domain changes remain P3, as requested.
- Shared follow-up: `special::dnbinom` loses log-likelihood precision at very
  large size (observed error 0.00798 on `c(0,1,5)`, held prob `1-1e-12`).
  The new held-probability fitter avoids that seam with stable rising
  factorials. The shared density and pre-existing held-size path were left
  unchanged under the file restrictions; they merit a separate repair.
