# R controls and modifier routes for 2.0

TASK-150.1.6, step 5c job 3. Worktree `codex/ctrl-r`, starting at `d5ca6052`.

The generated R fitters now expose `zero = "none"` and `truncate = NULL` after
`...` according to the eligibility metadata. All current fitters are eligible
for the truncation formal. Each modifier has its own help entry. The normal
adapter path still checks exact argument names, and modifier fits still require
named holds and controls. `mlpois(x, TRUE)` continues to set `na.rm`.

The existing modifier tolerance arguments are now named `tol`, with a default
of `1e-10`. The public route already used that numerical default. The private
truncated-normal adapter also declares `1e-10` in place of `1e-12`. The
existing iteration defaults carry through, including 512 for a general
binomial window with free size, 400 for other general windows and censoring,
and 200 for the zero routes. The active modifier selects its controls from
its adapter formals or the shared window and censoring interface.

`model_select()` forwards accuracy and budget controls only when the active
route accepts them. Holds, positional arguments and the missing-required-hold
check retain their previous handling. An ordinary fit on interval data made up
entirely of exact points uses the base fitter's controls. The all-held base
path validates `tol` and `iterlim` and preserves validation of legacy names
still accepted by the corresponding adapters. `bootstrapml()` uses its
`...` for the reducer and reconstructs refits from the original holds and
modifier attributes, so the refit path needed no filtering change.
`logmom_burr_c()` still calls the internal R Newton helper. Its option is now
named `tol` and the helper keeps its `eps^0.25` default.

## Validation

All completed suites ran on the workstation. Simbox returned exit 75 for the
R and C++ suite attempts, including the double-only attempt. A direct SSH
check timed out on port 22.

| Check | Result |
|---|---|
| `just regen` | Passed. Codegen checked each public adapter against the dossier control and hold names. An independent formals check matched all 15 private adapters against the corresponding entries in `modifier-laws.json`. |
| `just test` | 36,356 passes, 29 warnings, 157 skips, zero failures. |
| `just test-full` | 65,756 passes, 29 warnings, one skip, zero failures. The truth-generator suite passed 36 tests. The 16 private modifier test files ran as `test-modifier-kernel-*`. The sole skip was the private Zipf check without reference values. |
| `just cpp-test` | 110/110. |
| `just cpp-test-no-long-double` | 154/154 on GCC 15.2. |
| `just js-build` and `just js-test` | Built successfully and passed 586/586 tests. |
| `python3 dev/tools/check_links.py` | Zero new broken links, 47 inherited broken links. |

The first R dev run caught two tests that still expected the old signatures
and two new assertions that relied on a call attribute absent from the returned
fits. The signature tests now use eligibility, and the forwarding tests check
the effect of an invalid tolerance on the selected fitters. The resulting
targeted rerun passed 10,632 expectations. That targeted command reserved one
slot, and testthat started two workers because its worker count was left
implicit. The final suites used the four-slot reservation.

### Default route snapshots

Each mode matched 1,950 input keys against its stored baseline. In each mode
five records differ by the spelling `reltol` to `tol` in their diagnostic
message. The affected records are truncated normal with fully held coefficients
on `empty`, `nan`, `inf` and `neginf` inputs, and zero-inflated negative
binomial on the all-free `empty` input. All other result fields match exactly
in both modes. The field-level comparator exits 1 for the five messages.
[Native diff](2026-11-20-ctrl-r-data/routes-native-diff.txt) ·
[Double-only diff](2026-11-20-ctrl-r-data/routes-double-diff.txt) ·
[Exact message comparison](2026-11-20-ctrl-r-data/diagnostic-diff.txt) ·
[Native provenance](2026-11-20-ctrl-r-data/routes-native.provenance.json) ·
[Double-only provenance](2026-11-20-ctrl-r-data/routes-double.provenance.json)

## Route accuracy audit

This audit compares the implemented stopping rules with the
[coefficient accuracy contract](../../design/accuracy-contract.md). A bracket
claim is conditional on the computed score comparisons.

- Zero-truncated and zero-modified geometric use the reciprocal positive mean
  and, for the hurdle, the observed zero fraction. The right-censored
  exponential, Rayleigh, Pareto and Lomax scopes also use closed forms.
- Finite zeta windows use a monotone log-count score and require padded opposite
  signs within the public exponent neighbourhood. The finite Zipf regression
  adapter exercises this same route.
  [Finite zeta solver](../../../univariateML-cpp/include/cppunivariate/distributions/zeta/truncation.hpp)
- Zero-truncated and zero-modified binomial use the positive-count probability
  solve and discrete-size checks. The zero-modified fit has a separate
  sub-rounding check for its zero fraction. Zero-inflated binomial uses
  held-probability or positive-count root checks and tests rounding and
  propagation into the inflation coefficient for tight requests.
  [ZT binomial](../../../univariateML-cpp/include/cppunivariate/distributions/binom/ztbinom_mle.hpp) ·
  [ZM binomial](../../../univariateML-cpp/include/cppunivariate/distributions/binom/zmbinom_mle.hpp) ·
  [ZI binomial](../../../univariateML-cpp/include/cppunivariate/distributions/binom/zib_mle.hpp)
- Interior zero-truncated and zero-inflated negative-binomial estimates use
  the positive-information Newton polish in log-size and logit coordinates.
  A small absolute displacement in either coordinate bounds the relative public
  displacement in exact arithmetic. Zero-modified negative binomial delegates
  its positive part to the zero-truncated fit.
  [Negative-binomial polish](../../../univariateML-cpp/include/cppunivariate/distributions/nbinom/ztnbinom_accuracy.hpp)

### Open

| Route | Missing accuracy or work-control evidence |
|---|---|
| Truncated normal | The damped Newton stop uses natural-coordinate steps. The subsequent enclosure checks public mean and scale using a hard-coded `1e-8` at `norm/tnorm_mle.hpp:608`. Acceptance needs a bound in public coefficient units at the requested tolerance. |
| General Poisson window | `bindings/common/modifiers.hpp:126` checks a controlled conditional root, then returns the separate legacy `fit_tpois` result. The returned coefficient needs an inclusion check against the controlled bracket. The private Poisson window solver still has a fixed 120-step loop in `pois/tpois_profile.hpp:66`. |
| Zero-truncated and zero-modified Poisson | `pois/ztpois_mle.hpp:19` uses 100 bisections with rounded score comparisons and a fixed maximum. The zero-modified route delegates its positive part to that solve at `pois/zmpois_mle.hpp:33` and in `core/zero_modified_fit.hpp:34`. These routes need a requested-width, work-capped root rule and a rounding check on the free zero fraction where applicable. |
| Zero-inflated Poisson | `pois/zip_certificate.hpp:55` uses a relative rate width of `1e-13`, regardless of the public request. Its route takes `iterlim`. A configurable rate-width check and a floating-point error check for the Lambert W and inflation formulas at `pois/zip_mle.hpp:118–124` are needed to expose `tol` here. |
| Zero-modified logarithmic | `lgser/zmlogarithmic_mle.hpp:15–28` uses a fixed mean-residual gate and fixed replay width. The positive coefficient needs acceptance at the requested public tolerance, and the primary solve needs to share the replay's work allowance. |
| General count windows and private truncated binomial | The controlled binomial, geometric and held-size negative-binomial windows derive exact-arithmetic width bounds from their monotone score brackets. At `core/truncation.hpp:123`, the upper-end update includes a score whose sign is ambiguous inside the recurrence error margin. The integer-size binomial profile at `binom/tbinom_mle.hpp:121–135` also needs a final sign or rounding margin in public probability units. Its natural-coordinate width controls relative probability error conditional on the score comparisons. |
| Zero-modified and boundary negative-binomial fits | The closed form at `nbinom/zmnbinom_mle.hpp:24` returns a free zero fraction without checking whether its rounding meets a sub-rounding request. With held size and probability on `c(0,1,2)` it returns certified `p0 = 1/3` rounded with error `1.85e-17` at `tol = 1e-20`. The interior polish also bypasses boundary coordinates at `nbinom/ztnbinom_accuracy.hpp:11–14`. Those paths need their own public-coordinate checks. [Reproduction](2026-11-20-ctrl-r-data/zmnbinom-precision.R) · [Output](2026-11-20-ctrl-r-data/zmnbinom-precision.txt) |
| Censored Weibull | `weibull/censored.hpp:155–157` bounds log-shape and then profiles scale. The joint route needs an inclusion or sensitivity bound for public scale over that shape bracket. The scale-held all-censored branch at lines 220–229 uses its noise estimate for the certificate flag and needs a separate local accuracy decision when noise divided by curvature exceeds `tol`. |

## Job 4

- Change the JS modifier options, declarations, examples and tests to `tol`.
  The shared seam already passes the number positionally. Regenerate the base
  JS fit declarations after the family migrations and run the JS generator
  consistency checks.
- Update the site control placeholders, dossier schema, repository instructions
  and design notes to reflect explicit R modifiers, the two controls and
  control-free closed forms. Bring the diagnostics audit route signatures and
  the shared C++ test tolerance constants into line with the merged family jobs.
