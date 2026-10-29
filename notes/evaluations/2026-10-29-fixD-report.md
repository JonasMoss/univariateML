# fixD — TASK-78.7

Base: f4d1c1224bee8e71a907b383b31c8d1b2eafe8d4, branch codex/fixD.
No commits or board edits. No fitter, adapter, README, R-source or golden edits.
Scratch and full logs: `/tmp/umvl-codex/fixD/`.

## Files and family changes

The twelve assigned `distributions/<id>/dpqr.hpp` and `test.cpp` files,
`special/tails.hpp`, `special/special.hpp`, this report, and the justified
Bhattacharjee oracle correction (`bhatt/truth.py` and generated `truth.json`).

- invgamma: subtraction lost tails and reciprocal gamma quantiles; evaluate gamma tails at log(beta/x), invert the opposite log tail, and handle infinite observations/endpoints.
- gamma: log tails and log-input quantiles underflowed, negative support and zero density limits were wrong; use regularized gamma P/Q with log continuations, log-coordinate inversion, and direct rate arithmetic.
- trgamma: the old underflow anchor lost gamma tails and quantile endpoints; retain the transformed argument and unit-gamma quantile in log coordinates.
- invtrgamma: opposite-tail inversion and overflowing reciprocal transforms failed; share the same log-coordinate gamma machinery with tail reversal.
- lgamma: infinite observations and log inputs failed; compose the corrected gamma distribution with log/exp and its density Jacobian.
- invgauss: logging an underflowed PDF/CDF and unbracketed upper inversion failed; compute log density directly, use normal Mills-ratio differences, and bracket quantiles in log(x).
- lnorm: normal log tails/quantiles underflowed and infinite density arguments returned NaN; compose the corrected normal functions and return zero density at infinity.
- norm: normal tail underflow and exponentiating log inputs lost finite answers; use a double Mills continued fraction, log-tail inversion, and a compensated median expansion.
- tnorm: quadrature lost extremely remote interval tails and median offsets; use normal log-tail interval ratios and endpoint expansions, retaining local quadrature where narrow/divergent parent coordinates need it.
- hnorm: erfc and log-input quantiles underflowed; use normal log survival, a log-scale small-distance continuation, and retain scaled quantiles before exponentiating. Names remain dhalfnorm/phalfnorm/qhalfnorm/rhalfnorm.
- slash: quantile stopping lost the median and standardization overflow lost finite power tails; use a centered inverse expansion, log distances, and guarded affine inversion, all in double.
- bhatt: median inversion stopped too early and extended intermediates hid range issues; use a centered inverse expansion, shared normal/Mills logs, log convolution widths, and guarded affine inversion; retain the original central-density rounding behind double-range guards.

All twelve truth cases have `may_fail` removed, with default truth tolerances.
Targeted cases include mpmath generating comments, log quantiles, endpoints,
extreme observations, remote normal truncation, and representable scaled results.
Sampler bodies and random-number consumption are unchanged.

## Shared helpers

`special.hpp` adds only no-throw `gamma_p`/`gamma_q` backend wrappers.
New shared numerical helpers live in `tails.hpp`:
normal log survival/inversion (normal, lognormal, half-normal, truncated normal),
Mills logs and their derivative (inverse Gaussian and Bhattacharjee),
compensated probability offsets (normal, truncated normal, slash, Bhattacharjee),
guarded expm1 affine transforms (slash and Bhattacharjee), and unit-gamma log
tails/inversion (gamma, inverse gamma, loggamma, transformed gamma pair).
The new helper names are `normal_log_upper`, `normal_quantile`,
`normal_log_mills`, `normal_log_mills_slope`, `probability_offset`,
`expm1_affine`, `gamma_log_tails`, and `gamma_log_quantile`.
New tail arithmetic and inversions use double; complements use the existing
helpers. Bhatt's pre-existing long-double fitter helpers and central density
are retained for rounding compatibility only, behind guards which ensure all
central-density intermediates fit in double. Extreme inputs take the new
log-domain double path. This preserves the strict existing R fit golden;
converting the central calculation to double moved its estimates by 5e-8.
No extended exponent range is required by the repaired d/p/q implementation.

## Oracle correction

For Bhattacharjee, H(z)=z*Phi(z)+phi(z), and Phi(-z)=1-Phi(z) implies
H(b)-H(-b)=b. Consequently F(mu)=1/2 exactly, with a unique median mu.
The generic root search had stored spurious natural-p=1/2 medians around
-3.08149e-33. The family route now returns both exact center log tails.
A 100-digit mpmath check at b=.02,.2,2,20,200 has residual below 4e-101.
Proof and numerical check: `bhatt-median-proof.py` / `.log` in scratch.
Regeneration with `just dpqr-truth bhatt` verified all eight points (26.18 s),
producing 152 x rows and 352 quantile rows. It fixes three zero medians,
refines four log-median values by one ulp, and removes one redundant x row;
additional changes are independently recomputed adjacent-double brackets.
There are no hand edits to truth.json.

## Validation

Baseline, before implementation changes:

- Local `just test`: **FAIL 0 | WARN 0 | SKIP 1 | PASS 10545**.
  Skip: optional sn package. Documentation-generation messages are in the log.
- The reviewer interrupted the initial local C++ build when moving builds
  to simbox; the completed C++ baseline is therefore the simbox run.
- Simbox `just cpp-test`: **5/6 CTest targets passed**. Distribution suite:
  **503 cases, 501 passed, 2 failed, 3 skipped; 955567 assertions,
  946618 passed, 8949 failed**, including assertions allowed by may_fail.
  The two hard failures precede edits: betapr n20 slow-oracle fixture status,
  and std golden nu (3.97767 versus 3.97765). Both are outside fixD.
- Independent 700-digit inverse-Gaussian formula probe: **180 tail checks**,
  maximum normalized error **1.384e-13**, x from 1e-300 to 1e300.
  This probe translates the double formulas; it is separate from C++ validation.

- Simbox focused truth runner: **12/12 cases, 31227/31227 assertions passed**,
  no skips, default tolerances. Tables cover **76 parameter points,
  1508 observation rows, 3344 quantile rows**, including log inputs -800
  and -10000; sampler checks use **1520000 draws**.
- All 48 public signatures match HEAD; all 12 sampler bodies are byte-identical.

- Final simbox `just cpp-test`: **5/6 targets passed**, with exactly the same
  two hard failing cases as baseline (checked automatically by case name).
  Distribution suite: **547 cases, 545 passed, 2 failed, 3 skipped;
  955685 assertions, 947593 passed, 8092 failed**, including permitted failures
  belonging to the other fix jobs. All five other CTest targets pass.
- Assigned-family suite (all tests from the twelve files): **103/103 cases,
  54845/54845 assertions passed**. Other files are filtered out.
  The change adds **44 regression cases and 131 assertions**; regeneration
  removes thirteen truth-harness assertions associated with the redundant row.
- `just quality invgamma gamma trgamma invtrgamma lgamma invgauss lnorm norm
  tnorm hnorm slash bhatt`: **12/12 pass**.

The original central Bhatt density and helper rounding reproduce the base fit
bit for bit (mu, sigma, a, logLik) in the O0 comparison probe; a strict C++
golden regression now checks it. The 15-case tnorm run also passes all 5140
assertions, including quantiles -700/-10000 at zero endpoints and a
standardized interval width below binary64 range.

Final simbox `just test`: **FAIL 3 | WARN 0 | SKIP 0 | PASS 10547**.
All assigned-family R checks, including the unchanged Bhatt golden, pass.
The remaining failures are sstd's frozen quantile reference and the bnbinom
and std goldens. A namespace-isolated bridge compiled against HEAD headers
proves all three base/current results **bit-identical** on simbox
(`base-r-check.cpp`, `.R`, `.log`). Thus these are pre-existing platform
failures, not regressions from fixD. The std nu drift is 1.76e-5; bnbinom's
largest parameter drift is 1.13e-9. The sstd error is on its strict 1e-12 bound.
Simbox has sn installed (no skips); the local baseline skips that comparison.
No tolerance was loosened and no golden changed.
All later compiles and test suites run through `/tmp/umvl-simbox/run`.
Small local mpmath probes use `/tmp/umvl-cpu/run`.


## Review follow-up

The two baseline C++ failures and three proven base-identical simbox R
failures remain for cross-machine follow-up (TASK-75). No unresolved fixD
truth-table or assigned-family regression failures remain. No commits made.
`git diff --check` passes; public signatures, sampler bodies, goldens,
fitters and R sources are unchanged.
