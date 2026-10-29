# bnbinom implementation report

No commits or Git-writing commands were run. Existing goldens are unchanged.

## Files

- Added `cppunivariate/include/cppunivariate/distributions/bnbinom/`:
  `README.md`, `mle.hpp`, `dpqr.hpp`, `adapter.cpp`, `test.cpp`, `test.R`,
  `golden.json`.
- Added `notes/evaluations/2026-10-29-bnbinom.md` and its independent
  exploration script `2026-10-29-bnbinom-oracle.R`.
- Updated `cppunivariate.hpp`, `univariateML/CANONICAL_NAMES.md`, and
  `univariateML/R/code_helpers.R`.
- Extended `nbinom/mle.hpp` with an optional held-size argument so BNB
  evaluates its NB faces through the existing core fitter. The old
  three-argument path is unchanged.

## Mathematics: proved in the analysis note

- Gamma-product Poisson mixture; exact size/beta interchangeability.
  Waring at size=1; shifted Yule--Simon at size=beta=1; tail exponent alpha+1.
- Proper Hellinger closure: NB, mirror NB, Poisson, delta_0; explicit
  reachability table for all eight fixed/free subsets.
- Alpha down to zero alone loses mass to infinity, rather than defining a
  proper face; defective simultaneous escapes cannot improve a positive-count
  sample's likelihood.
- Finite existence for samples containing a positive count when alpha is
  held or two coefficients are held. Strict improvement over all reachable
  faces is sufficient in the remaining subsets.
- NB inward coefficient is n*r/(2*m)*(v-m-m*m/r), with population-form v.
  A positive coefficient at the best NB face implies finite existence.
- Positive-probability nonexistence: all-zero samples; also constant positive
  samples when all coefficients are free. Bernoulli samples select Poisson.
- Off-diagonal maxima have swapped companions. Folding removes this label
  duplication; it does not establish uniqueness.

## Mathematics: checked numerically

- External extraDistr masses versus exact lgamma/lbeta formula; symmetry.
- All eight fixed/free subsets versus a 12-start independent nlminb oracle
  (R seed 4821). Golden independent oracle: -1275.6474863695578;
  C++: -1275.6474863695587 (seed 13086).
- Diagonal finite maxima occur (gamma-product seed 1: about
  1.22131, 2.38886, 1.22131); other seeds give asymmetric pairs.
- Strictly overdispersed samples can select NB: 20 zeros and 20 fives
  have NB size about .493768 and inward coefficient about -35.187.
- Several underdispersed stress samples select Poisson. This is not a
  general theorem.

## Assumptions and open questions

- No full-family uniqueness or folded-unimodality theorem was verified.
  Literature found: Irwin 1975, Xekalaki 1984, GWRM manual, and Tang et al.
  2023 on existence/uniqueness for the two-parameter Waring subfamily only.
- Whether *every* variance<=mean sample selects Poisson remains open here.
- Numerical global optimality is not certified. Deterministic multistart
  BFGS uses up to 30 starts, log shapes in (-28,28), and a score gate;
  unresolved score/range results raise an error. Very extreme finite fits
  may exceed this range. No large shape substitutes for a boundary model.
- Gamma increments use extended precision and a high-order Stirling
  evaluation, with moderate/huge-shape checks; this is floating-point
  evaluation, not interval arithmetic.
- Actual NB and Poisson likelihoods are compared; each reachable face
  wins ties within 1e-8*n. Held shapes are preserved, including for the
  mirror NB route. All-held fits evaluate likelihood without a search.
- R quantiles retain reference-CDF inaccuracies. A cumulative one-million
  count-work budget prevents heavy-tail qml()/plot() hangs. C++ p/q also
  have finite work budgets (NaN on exhaustion).

## Validation

- `just cpp-test`: 4/4 ctest targets pass; 294 doctest cases and
  505121 assertions total (distribution_tests: 284 cases / 494840 assertions;
  BFGS: 6 / 21; sampling: 1 / 10204; no-throw: 3 / 56).
- `just quality bnbinom`: pass; `just quality`: 70/70 pass.
- `UMVL_GOLDEN_IDS=bnbinom just goldens`: one new golden written;
  no existing golden changed.
- `just test`: 4337 expectations pass; 0 failures, 0 warnings, 0 skips.
- New model tests cover all fixed subsets, score residuals, mirror, diagonal,
  face selection, oracle agreement, quantile inverse inequalities/work limits,
  and failure tolerance in discrete model selection. `git diff --check` passes.

Timing: standalone C++17 `g++ -O2`, one CPU slot, nice 10, n=1,000,000,
std::mt19937_64 seed 20261029, gamma-product draws from (1,2,5):
**5.57262 seconds** for the fitter, including aggregation and face comparison,
excluding sampling and compilation. Status Ok; estimates approximately
(.997983, 1.99636, 4.99517). Scratch programs/logs: `/tmp/umvl-codex/bnbinom/`.

## Reference defects: extraDistr 1.10.0.5

No reference replacements or KNOWN_ISSUES changes were made.

- Log-mass cancellation: `extraDistr::dbnbinom(0:3,2,4e14,1e14,log=TRUE)`
  gives (-.46875,-1.40625,-2.5625,-3.90625), versus the effectively identical
  NB limit `dnbinom(0:3,2,.8,log=TRUE)` =
  (-.4462871,-1.3625778,-2.5665506,-3.8883065).
- Invalid CDF values: `extraDistr::pbnbinom(0:3,2,4e14,1e14)` gives about
  (2.804569,3.184127,3.235494,3.242446), all above one.
- Ordinary-shape consistency check:
  `max(abs(extraDistr::pbnbinom(0:100,1,2,5)-cumsum(extraDistr::dbnbinom(0:100,1,2,5))))`
  is 1.11e-15; no ordinary-shape CDF defect was found by this check.

The artifact viewer rejected this worktree as outside its configured roots;
files remain in the requested worktree and are directly linkable.
