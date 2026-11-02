# Cauchy held-location scale bracket (R6)

Completed 2026-11-02 in `codex/cauchy-bracket`, based on
`2dad9da3f2fe9e538e8acfa231cca83d77412ca5`, with the uncommitted patch identified
below. No commits, board edits, package installs, or changes to other families
were made. All computation ran locally through the shared CPU slot locks;
simbox was not used. All required final runs finished successfully.

## Result and change

For `mlcauchy(c(0,1e-300,1), location=0)`, the corrected fit returns
scale `1.000000000000251e-300` and log-likelihood `686.64819106010577`.
The old radius-minus-350 bracket returned approximately `9.92959e-153`,
with log-likelihood `346.5658`, without bracketing the score root.
Uniform rescaling did not expose the defect because it preserved residual ratios.

The held-location branch now:

1. Computes positive residuals in log coordinates. If subtraction of two finite
   extremes overflows, it uses `log(abs(x/2-location/2))+log(2)`.
2. Starts at the smallest and largest positive log residual. It expands downward
   and upward until it has verified strictly positive and strictly negative
   scores, respectively. Expansion stops at the logs of the smallest positive
   and largest finite double scales.
3. Returns `IterationFailure` if it cannot establish that bracket. Otherwise it
   bisects, stopping at log-width below `1e-12` or a computed zero score, within
   `iterlim`. Iteration exhaustion is a failure.
4. Checks that the returned scale is finite and positive and that its score,
   recomputed after exponentiation and rounding, has magnitude at most
   `1e-10*n`. A nonfinite likelihood also fails.
5. Evaluates the held-location likelihood with log residuals and the stable
   softplus expression, avoiding overflow in squared residual ratios.

The binding reports `IterationFailure` before inspecting the intentionally
missing parameters in a failed result, preserving a numerical-failure diagnostic.
The default joint fitter and the held-scale location search are unchanged.
No golden, d/p/q/r, special-function, truth, or code-generation source changed.

The former fixed 350-log-unit lower span is rejected: it has no relation to the
smallest residual when residual magnitudes differ. Verified score signs replace
that heuristic. The canonical Cauchy dossier records the algorithm and its
floating-point conditions.

## Mathematical certificate and numerical limits

Put `t=log(scale)` and let `k` count residuals exactly zero. The total
log-likelihood score is

```
g(t) = -k + sum(tanh(log(abs(x-location))-t)),
```

where the sum contains only nonzero residuals. Its derivative is
`-sum(sech(log(abs(x-location))-t)^2)`, strictly negative whenever there is a
nonzero residual. Its limits are `n-2*k` and `-n`. Consequently, when `2*k<n`,
there is exactly one root, and strict concavity in `t` makes it the unique global
scale maximum. The existing `NoMLE` treatment for `2*k>=n` remains unchanged.
This is a mathematical result, rather than a conclusion from the test grid.

Bisection certifies the root in exact arithmetic conditional on the computed
score comparisons. The practical floating-point margins are the log-width and
rounded-scale score checks above. This is not outward-rounded interval
arithmetic or a universal error bound for `tanh` and summation. A computed zero
is accepted under that conditional-comparison convention and the score gate.
When the representable range cannot supply strict signs, the implementation
stops rather than returning its edge. Tests include both a conservative failure
at the smallest positive residual scale and an optimum above the finite double
range. The existing free-location multimodality/search guarantee is outside
this change; the dossier continues to mark overall certification partial.

## Regressions and independent checks

C++ cases were appended to the end of `cauchy/test.cpp`; R cases were added to
`bindings/r/distributions/cauchy/test.R`. They cover:

- The reported reproducer and pairs of positive residuals from `1e-310`
  (subnormal) through `1e300`, including spans of 610 decimal orders.
- An overflowing residual difference with a representable optimum:
  `c(-1e308,0,1e308)` at location `-1e308`. The returned scale divided by
  `1e308` agrees with `sqrt((sqrt(73)-5)/6)`.
- Failure to obtain a strict bracket at the floating-point floor, an optimum
  above the finite scale range, and deliberate iteration exhaustion.
- A held scale remaining exactly held; existing held-subset and default golden
  checks also pass. Goldens were not regenerated.

For data `(0,a,b)`, the exact positive root satisfies
`3*s^4+(a^2+b^2)*s^2-a^2*b^2=0`. Writing `u=a/b`, its stable normalized form is

```
s/a = sqrt(2/(1+u^2+sqrt((1+u^2)^2+12*u^2))).
```

The test pairs have `b/a>=1e10`. Thus `s/a` differs from one by order `u^2`,
and the maximum likelihood agrees, within the stated test tolerances, with
`-3*log(pi)-log(a)-2*log(b)-log(2)`. Tests check log-scale error below `1e-9`
in C++, R comparisons at tolerance `1e-10`, and the analytical likelihood at
`1e-10`. These checks derive from the likelihood equation independently of the
bisection implementation.

Two preliminary focused R runs failed only newly added reference assertions:
`stats::dcauchy(...,log=TRUE)` produced `-Inf` on extreme inputs, first through
squared ratios and then through the density denominator. That unsuitable
assertion was removed; the analytical root/likelihood checks cover every extreme
case. The final focused run and all subsequent full runs are green.

## Final validation

Environment: Linux `7.0.0-38-generic`, x86_64, GNU C++ 15.2.0,
R 4.6.1 (2026-06-24). Every row below ran on the workstation. Long commands used
`nice -n 10`; BLAS/OpenMP threads were pinned to one. Temporary files and logs
were confined to `build/tmp` and `build/scratch` in this worktree.

| Command | Final result |
|---|---|
| `just cpp-test` | 106/106 dev shards passed |
| `just cpp-test-no-long-double` | 127/127 tests passed |
| Focused R `test_local(...,filter="dist-cauchy")`, full tier | FAIL 0, WARN 0, SKIP 0, PASS 155 |
| `just test` | FAIL 0, WARN 0, SKIP 151, PASS 20151 |
| `just test-full` | FAIL 0, WARN 0, SKIP 0, PASS 41754 |
| `UMVL_TEST_TIER=full just cpp-test` | 127/127 tests passed |
| `just js-build` | exit 0 |
| `just js-test` | 413/413 passed; no failures or skips |
| `git diff --check` | passed |

The 151 R dev skips are tier exclusions; the full tier has none. There were no
permitted test failures. Configuration emitted the existing CMake CMP0167
warning; the WebAssembly compiler emitted an existing tautological-comparison
warning in `binom/mle.hpp`. Neither is a test failure; neither source was edited.

Reproduction setup and commands (do not nest the slot wrappers):

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 just cpp-test-no-long-double
/tmp/umvl-cpu/run4 nice -n 10 just test
/tmp/umvl-cpu/run4 nice -n 10 just test-full
/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full nice -n 10 just cpp-test
/tmp/umvl-cpu/run4 nice -n 10 just js-build
/tmp/umvl-cpu/run4 nice -n 10 just js-test
```

The final R/full-C++/JS commands ran sequentially within one four-slot wrapper,
with a shell that stopped on the first error. The audit comparisons used one
slot. No command was left running when this report was written.

## Read-only audit of other families

Screening commands:

```sh
rg -l 'bisect|bracket|hi.*lo|lo.*hi' univariateML-cpp/include/cppunivariate/distributions/*/mle.hpp
rg -n 'double lo.*hi|double lower.*upper|return.*mid' univariateML-cpp/include/cppunivariate/distributions/*/mle.hpp
```

The table lists source suspects and actual R reproducer attempts. Line numbers
refer to the tested base revision; each path is under the distribution directory
named in the first column. This is a source-screening audit, not a proof that
all remaining numerical searches are correct. A mathematically justified data
bound is different from an unchecked numerical range: for example the
held-shape log-logistic location bracket is bounded by the observed log data;
Beta and Kumar explicitly check their expanded upper signs; Burr checks both
signs; CMB additionally checks the scores in its returned coordinates. These
were not classified as the reported Cauchy failure.

| Family / source | Suspect | One-line attempt and observation |
|---|---|---|
| `lst/mle.hpp:271` | Same radius-minus-350 scale bracket, no endpoint-sign gate | `mllst(c(0,1e-300,1),mu=0,df=1)` returns sigma `9.92959e-153`, ll `346.5658`; sigma `1e-300` gives ll `686.6482`. Confirmed non-maximizer. |
| `slash/mle.hpp:17` | Fixed lower log scale and unchecked scale-score bisection | `mlslash(c(0,1e-300,1),mu=0)` returns sigma `9.92959e-153`, ll `345.8569`; sigma `1e-300` gives ll `686.3928`. Confirmed non-maximizer. |
| `lomax/mle.hpp:334` | Fixed `[-745,710]` log-lambda range without sign check | `mllomax(c(1e300,2e300),kappa=1e300)` returns lambda `4.940656e-324`, ll `-1.482197e277`; total log-lambda score is `-1.482197e277`. Confirmed unchecked edge when the true root is below the representable range. Opposite attempt `mllomax(c(1e-300,2e-300),kappa=1e-300)` stops. |
| `gompertz/mle.hpp:134` | Upper bound limits the largest `b*x` to 710, without score verification | `mlgompertz(c(1e-300,2e-300),a=1e-300)` returns b `3.55e302`, ll `-316.5511`; b `6e302` gives ll `418.4489`. Confirmed non-maximizer. |
| `logis/mle.hpp:212` | Radius-minus-350 held-location scale bracket | `mllogis(c(0,1e-300,1),location=0)` returns scale `.3082959`, ll `-2.562699`. This attempt does not expose a defect; the large residual controls the logistic score. |
| `gamma/mle.hpp:198` | Fixed log-shape range without explicit sign check | `mlgamma(c(1e300,1e300),scale=1e-300)` stops with `IterationFailure`; no unsafe successful result observed. |
| `llogis/mle.hpp:79` | Fixed log-shape range without explicit sign check | `mlllogis(c(1,1+1e-15),scale=1)` returns shape `2.161149e15`, ll `66.6594`; no bracket failure established. |
| `weibull/mle.hpp:83` | Fixed log-shape range without explicit sign check | `mlweibull(c(1,1+1e-15),scale=1)` returns shape `9.548445e14`, ll `66.1586`; no bracket failure established. |
| `ht/mle.hpp:36` | Capped expansion loops followed by unchecked bisection | `mlht(c(0,1e-300,1),mu=0,nu=1)` correctly returns sigma `1e-300`, ll `688.7276`; no failure established. |
| `sgomp/mle.hpp:25` | Fixed lower log-eta bound and width-only success | `mlsgomp(c(1e-300,2e-300),b=1)` returns eta `0`, ll `0`. This attempt selects the boundary branch and does not exercise the suspect interior solve. |
| `gumbel/mle.hpp:138` | Expansion can stop at a finite doubling limit without an explicit sign gate | `mlgumbel(c(0,1e-300),mu=0)` returns sigma `4.466986e-301`, ll `1379.818`; no failure established. |
| `gpd/mle.hpp:53` | Expansion/bisection without an explicit finite, opposite-sign bracket check | `mlgpd(c(1e308,1e308),mu=0,xi=1)` returns sigma `1e308`, ll `-1421.165`, consistent with the stationary scale for this sample. |
| `zipf/mle.hpp:134` | Fallback has capped upper expansion and returns after 100 bisections without a final score gate | `mlzipf(c(1L,2L,2L),N=10,iterlim=1)` returns shape `1.841522`, ll `-4.071614`; no non-maximizer established by this attempt. |

An ancillary screened case, `mlnaka(c(1,1+1e-15),scale=1)`, returns shape
`4.056482e29` and ll `1.407375e14`. Its shape bracket uses analytical gamma
bounds rather than the Cauchy radius heuristic. The very large likelihood is
recorded as a separate numerical observation, not diagnosed here as a bracket
failure.

The confirmed comparisons can be reproduced after loading the generated package
with `pkgload::load_all("univariateML-r")`, under a CPU slot:

```r
# LST at df=1 is the Cauchy likelihood; negligible (a/b)^2 corrections here.
-3*log(pi)-log(1e-300)-log(2)                 # 686.6482
# Slash at zero has density 1/(2*sqrt(2*pi)*sigma).
-1.5*log(2*pi)-log(1e-300)-log(2)+log(-expm1(-.5)) # 686.3928
x <- c(1e-300,2e-300); a <- 1e-300; b <- 6e302
length(x)*log(a)+b*sum(x)-sum(exp(log(a)-log(b)+b*x)-a/b) # 418.4489
x <- c(1e300,2e300); k <- 1e300
f <- mllomax(x,kappa=k)
length(x)-(k+1)*sum(plogis(log(f["lambda"])+log(x))) # -1.482197e277
```

### Open follow-ups from the audit

- `lst`, `slash`, `lomax`, and `gompertz`: confirmed unsafe successful results
  above need separate repairs, regression cases and certificates. No fixes were
  authorized outside Cauchy in this job.
- The remaining table entries need endpoint-sign/score analysis or a failing
  reproducer before they can be called defects. In particular, the `sgomp`
  attempt did not reach its interior solve. The Nakagami likelihood observation
  needs a separate stable-likelihood comparison; this audit did not settle it.
- This job introduces no needed changes to settled d/p/q/r, special functions,
  truth fixtures, or generators. The historical held-scale location-search
  guarantee is unchanged and was not extended by the Cauchy scale proof.

## Tested patch identity

All hashes are SHA-256, taken after the final tests. Paths are relative to the
repository root. The report itself was added afterward; source was unchanged.

```
d7934a56ca2719648046d942a23b79023f44edb21b3ec24c71cdb92c99b5bb0e  bindings/common/distributions/cauchy/fit.hpp
f69f2becf2fb09f46d1a2f841d6885ad31558db609f34697d2b533df959be57d  bindings/r/distributions/cauchy/test.R
8d3c986fc0d9d90dd07f1a88e1c1b2fd3804a62c06f4d53faef2d9dfbef4cb79  univariateML-cpp/include/cppunivariate/distributions/cauchy/mle.hpp
a0012ddd7a4fece917787a5afbef393ad3748d5de44b2219cd804f490c985b4d  univariateML-cpp/include/cppunivariate/distributions/cauchy/test.cpp
e9b12a0b7e212c13e4e660b8d2d9e938455868adcd1237d139da1719cdfe841f  univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md
```

## Artifact delivery

The prescribed `codex-math-web/bin/show` command was attempted after writing this
report. The viewer returned HTTP 403: this worktree is outside its configured
readable roots. The report remains at its requested path. No viewer settings,
main-checkout files, or other-worktree files were changed to bypass that limit.
