# Existence and reader-facing prose: existence-6

TASK-101.6.4.1. Completed for `hnorm`, `foldnorm`, `tnorm`, `snorm`,
`logitnorm` and `lgamma` in branch `codex/existence-6`, based on
`6fea876ebeec88594a6de4b8af0db4adb5a7d452`. Validation finished on
2026-11-01 at 22:54 CET; the report filename follows the job brief.
Changes are uncommitted, as requested.

Only the six distribution dossiers and this report were authored. Each dossier
now has `existence: {uniqueness, statement, reason}`. Limit paths and detection
sentences use statistical prose, and R usage and dated implementation notes
have their own sections. The two families with advice, `hnorm` and `tnorm`,
have revised full sentences. Neither old advice string occurs in its common
binding header, and no family test regex matches the changed advice. Thus no
binding messages or test regexes needed changes.

All protected front-matter fields are unchanged. In particular, **there are no
status changes**, changes to held sets, fitting domains, firmness or numerical
flags, relations, fit declarations, or inference declarations. No fitting,
d/p/q/r, golden, truth, generator or test code changed. These families have no
structural hold-reason or fit-domain-reason entries to rewrite. The statements
below assume nonempty, finite data in the stated support and valid held values;
mathematical existence does not guarantee a successful floating-point solve.

## Family statements and evidence

| Family | Existence statement in one line | Evidence grade and uniqueness | Status change |
|---|---|---|---|
| `hnorm` | The free fit is unique exactly for nonconstant samples; held endpoint needs a positive residual sum of squares, while held positive scale permits constant samples. | Proved; uniqueness proved. | None. |
| `foldnorm` | The free fit exists for nonconstant nonnegative data; held location fails only when all data equal it, and held positive scale gives a unique location fit. | Existence proved; joint and held-location uniqueness unknown; held-scale uniqueness proved. | None. |
| `tnorm` | After profiling free bounds, a unique finite parent fit exists under the stated strict moment inequalities; coincident profiled bounds have no continuous-density MLE. | Proved; uniqueness proved whenever the finite fit exists. | None. |
| `snorm` | A nonconstant free fit exists exactly when the two-piece profile has an interior global minimizer; otherwise its supremum is half-normal, while constant samples are unbounded. | Profile criterion proved; joint uniqueness unknown; general held-subset classification remains unknown. | None. |
| `logitnorm` | The free fit is unique exactly for nonconstant proportions; held location fails only when every logit equals it, while held positive scale permits constant samples. | Proved by transformation; uniqueness proved. | None. |
| `lgamma` | The free fit is unique exactly for nonconstant data above one; holding either gamma coefficient gives a unique finite estimate of the other. | Proved by transformation; uniqueness proved. | None. |

**Half-normal.** At any positive scale, moving the endpoint towards the sample
minimum increases likelihood. At the fitted or compatible held endpoint,
maximization over scale gives the unique positive root mean square residual.
A zero residual sum instead makes likelihood diverge as scale vanishes.
There is no disagreement between the revised existence, limits and hold policy,
and no open mathematical question for these supported subsets.

**Folded normal.** For nonconstant data, at least one residual remains nonzero
as scale vanishes, so the product of densities tends to zero. As scale grows,
each density is bounded by a constant divided by scale. With scale bounded
away from zero and infinity, location tending to infinity also loses all
likelihood. Continuity and the included location boundary zero therefore give
attainment on a compact level set. The same argument covers held location
unless every residual is zero.

With scale held, the positive-location score equation is
$\overline{x\tanh(x\mu/\sigma^2)}/\mu=1$. Each nonzero summand decreases
strictly in positive $\mu$. The unique maximum is positive when
$\overline{x^2}>\sigma^2$, and zero otherwise. This proves that conditional
uniqueness, including the equality case; it does not prove joint uniqueness.
The dossier retains the likelihood-tolerance comparison with the half-normal
boundary and describes the chord bounds present in the held-location code.

Open: joint and held-positive-location uniqueness are not settled. The dossier
has no `limits` record for its documented constant-data collapse. Adding such
a record exceeds the permitted edits to existing limit text. The protected
`fit` declaration remains conservative about held-subset certification; the
current code also contains chord bounds for held positive location.

**Truncated normal.** Free bounds profile exactly to sample extremes, provided
the resulting interval is nonempty. On fixed bounds the natural likelihood is
strictly concave in $(\mu/\sigma^2,-1/(2\sigma^2))$; holding either parent
coefficient gives an affine restriction. For a nonconstant sample, profiling
the linear natural parameter gives a finite normal fit precisely when its
variance is below the variance of the exponential tilt with matching mean.
The one-sided thresholds are $(\bar x-a)^2$ and $(b-\bar x)^2$. On the whole
line every nonconstant sample has a normal MLE.

With mean held, write $q=\overline{(x-\mu)^2}$ and
$q_0=\inf_{a\le x\le b}(x-\mu)^2$. The expected squared distance increases
strictly from $q_0$ to the uniform expectation as scale increases on a bounded
interval; on an unbounded interval the upper expectation is infinite. Thus
$q>q_0$ and the finite-interval strict upper inequality are necessary and
sufficient. With scale held, the model expectation of the observation increases
strictly through the open interval as the parent location varies, giving a
finite root exactly when the empirical mean is strictly inside it. Two
coincident profiled bounds cannot define a continuous-density fit, even when
the parent coefficients are held.

Disagreements corrected in prose: the old assertion that holding scale always
gives a finite location estimate omitted endpoint-only samples. The held-mean
argument also omitted the lower squared-distance boundary when the held mean
is outside the interval. Exponential-limit detection text now explicitly
requires a nonconstant sample and both parent coefficients free; numerical
variance expressions were replaced by sentences. The existing numerical
face preference within $10^{-8}n$ log-likelihood units remains documented.

Open: existing limit records omit collapse, and the protected `fit.note` claim
that collapse cases stop is too broad. The endpoint reproducer below returns
a non-maximizer with mean held outside the support. The dossier now warns
about this behavior. A fitter fix should detect observations all equal to the
projection of the held mean onto the interval before solving for scale,
including the reflected case and profiled bounds. Holding scale at endpoint-only
data currently stops with a Newton-failure message suggesting more iterations,
although no finite location maximum exists. Both diagnostics need correction
outside this prose-only job. The linked October 29 P0 report predates the
current closed-endpoint convention and is historical evidence, not a current
classification of free truncation bounds.

**Two-piece normal.** For a proposed mode $c$, profiling its left and right
scales reduces maximization to minimizing
$Q(c)=(A(c)^{1/3}+B(c)^{1/3})^3$, where $A$ and $B$ are the sums of squared
residuals on the two sides. The continuous profile attains its minimum between
the sample extremes. An interior minimizer has two positive scales; an endpoint
minimizer gives a half-normal limit. An interior minimizer tying an endpoint
still gives a mathematical MLE. This is an existence criterion, not a
uniqueness proof. The dossier distinguishes the free profile's algebraic
candidate enumeration from the conditional numerical scans. The held-moment
endpoint penalty $m^2/(8h^4)$ per endpoint observation is retained.

Open: joint uniqueness and a general finite-fit criterion for the nonsymmetric
held subsets remain unknown. Conditional scans and local refinements are not
a global certificate. The declared collapse status `Unbounded` matches the
held path, but the fully free constant-sample path returns `InvalidStart`;
changing the single metadata status would merely move the mismatch. The
reproducer below records both errors. Static inspection also finds that common
binding half-normal messages always describe endpoints at sample extremes,
although held-moment boundary comparisons can constrain the endpoint differently.
Those messages are outside the permitted hold/advice string edits.

**Logit-normal.** The logit transform is one-to-one and adds a
coefficient-independent Jacobian to the normal log likelihood. Normal location
and residual-scale maximization therefore prove all stated existence and
uniqueness claims. No disagreement or open mathematical question was found
for the supported subsets. Historical tail and quantile implementation facts
were retained under Decision history.

**Log-gamma.** Set $y=\log x>0$. Its Jacobian is independent of shape and
rate. The free gamma shape equation has left side $\log k-\psi(k)$,
strictly decreasing from infinity to zero, and right side
$\log\bar y-\overline{\log y}$, positive exactly for nonconstant samples.
Held shape gives rate $k/\bar y$; held rate leaves a unique digamma root.
The proof concerns exact transforms; the existing diagnostic distinction for
precision-collapsed transformed samples is retained.

Open: the dossier has no limit record for its documented unbounded
constant-data path (shape and rate grow at fixed ratio). No other field
disagreement or mathematical uniqueness gap was found.

These arguments introduce no external citations. No web searches were used,
and the existing reference fields were preserved.

## Reproducible diagnostic findings

After regeneration, run the following locally through a CPU slot, with
`TMPDIR`, `JUST_TEMPDIR` and `XDG_RUNTIME_DIR` pointing to `build/tmp`:

```r
pkgload::load_all("univariateML-r", quiet = TRUE)
tryCatch(mltnorm(rep(0, 3), a = 0, b = 1, sd = 1),
         error = function(e) conditionMessage(e))
mltnorm(rep(0, 3), a = 0, b = 1, mean = -1)
tryCatch(mlsnorm(rep(0, 3)), error = function(e) conditionMessage(e))
tryCatch(mlsnorm(rep(0, 3), mean = 0),
         error = function(e) conditionMessage(e))
```

Observed results:

- The held-scale truncated-normal fit stops: “Truncated normal Newton iteration
  did not converge; increase `iterlim`.”
- The held-mean truncated-normal fit returns approximately
  `mean = -1, sd = 9.357e-09, a = 0, b = 1`. This is not an MLE.
- Fully free constant `snorm` stops with “Skew-normal MLE requires at least 2
  finite observations with positive variance.” The held-mean version instead
  reports an unbounded conditional likelihood and scale collapse.

The truncated-normal failures have short direct proofs. At zero on $[0,1]$
with mean $-1$, dividing numerator and normalization by the endpoint exponent
gives
$f_\sigma(0)=1/\int_0^1\exp\{-(2x+x^2)/(2\sigma^2)\}\,dx
\sim\sigma^{-2}$. With scale one and mean $-M$, the same calculation gives
$f_M(0)=1/\int_0^1\exp(-Mx-x^2/2)\,dx\sim M$.
Thus both three-observation likelihoods diverge. Numerical endpoint densities
illustrate these proved paths; they are not the proof:

| Held mean −1: scale | Log density at zero | Held scale 1: mean | Log density at zero |
|---:|---:|---:|---:|
| 0.10 | 4.614932 | −10 | 2.312372 |
| 0.03 | 7.014014 | −100 | 4.605270 |
| 0.01 | 9.210440 | −1000 | 6.907756 |

These numbers come from `dtnorm(0, mean = ..., sd = ..., a = 0, b = 1,
log = TRUE)`. They are selected fixed-parameter evaluations, not a fitted
maximum or a numerical existence certificate.

## Validation

All runs used the local workstation, never simbox. Builds, tests and numerical
checks ran through `/tmp/umvl-cpu/run4`, under its `nice -n 10` wrapper with
at most four cores and BLAS/OpenMP pinned to one thread per worker. Temporary
files and logs stayed in `build/tmp` and `build/scratch`.
Environment: Linux `7.0.0-38-generic`, x86_64; R 4.6.1 (2026-06-24),
Rcpp 1.1.2, testthat 3.3.2, roxygen2 8.0.0. No packages were installed.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed, including the final batch regeneration. |
| Requested targeted R filter | Passed twice: **FAIL 0, WARN 0, SKIP 8, PASS 8031**. |
| R help scan, ignoring Non-ASCII diagnostics | No diagnostics for any of the six families; explicit final assertions passed. The all-family scan prints 26 existing diagnostics for untouched `mlst.Rd` only. |
| `python3 site/scripts/data.py` | Passed twice; final run derived 99 families in 2.25 seconds. |
| `just relations` | **157 edges, 0 failed**, with symbolic and numerical comparisons as reported by the harness. |
| `/tmp/umvl-cpu/run4 just test` | Passed twice. Final: **FAIL 0, WARN 0, SKIP 151, PASS 20102**, 194.945 seconds test elapsed time. |
| Protected front-matter comparison against `HEAD` | Passed for all six families, including after final regeneration. |
| `git diff --check` | Passed. |

The targeted command, run inside `univariateML-r`, was:

```sh
env UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions \
  Rscript -e "testthat::test_local('.', filter = 'dist-(hnorm|foldnorm|tnorm|snorm|logitnorm|lgamma)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
```

The help scan used the requested `tools::checkRd()` loop over `man/ml*.Rd`,
filtering `Non-ASCII`; the final check additionally asserted that each of the
six family files had zero remaining diagnostics. Inspection of generated
`mltnorm.Rd` confirmed both the coincident-bound statement and the numerical
endpoint warning. The `st` source is unchanged against `HEAD`.

The scope audit parsed the old and new YAML, removed `existence`, parameter
`advice`, and limit `when`/`detect`, and compared all remaining fields for exact
parsed equality. No hold or fit-domain reasons changed. This checks the
protected-field contract independently of generator success.

The 8 targeted and 151 dev skips are tier/dependency skips, not passes or
permitted failures. No test failures were accepted. Full-tier, C++ and JS suites
were not needed for this documentation-only change and were not run. Golden
regression and relation checks do not establish the mathematical existence
claims or certify the existing conditional searches. The open issues above
remain explicit despite the green requested test runs.

Artifact delivery: the prescribed `show` command was attempted after writing
this report. The viewer returned HTTP 403 because this worktree is outside
its configured readable roots. The report remains at the requested path;
no viewer configuration or other checkout was changed.
