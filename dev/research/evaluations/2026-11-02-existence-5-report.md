# Existence statements and prose: existence-5

TASK-101.6.4.1. Worktree `existence-5`, branch `codex/existence-5`.
Tested source: base `6fea876ebeec88594a6de4b8af0db4adb5a7d452` plus the six
uncommitted dossier changes fingerprinted below. No commits were made.

The six dossiers now contain `existence` statements, parameter paths separated
from data conditions in the existing limits, and reader-facing descriptions of
holding coefficients and fitting. R interface details are in `R notes`.
Dated implementation material is in `Decision history`.

Mathematical statements concern nonempty finite samples in the stated support,
with admissible finite held values and positive likelihood. Mathematical
existence and uniqueness are separate from successful floating-point fitting
and from completeness of a numerical search. No new external citation was used;
the arguments are given in the dossiers. Existing references were preserved.

## Results by family

| Family | Statement in one line | Grade | Status changes |
| --- | --- | --- | --- |
| `invweibull` | Both free: a unique finite MLE exists exactly for nonconstant positive data; held shape permits constants; held scale fails only when every observation equals it. | Proved | None |
| `frechet` | Unrestricted joint likelihood is always unbounded; the restricted free-location problem has known collapse and Gumbel competitors but no complete proved criterion or joint uniqueness result; held-location and several other conditional cases are proved below. | Unknown jointly; conditional results proved; half-tie comparison numerical | None |
| `dweibull` | Both free: a unique finite MLE exists exactly when the sample range is at least two; holding either coefficient gives the stated scalar criteria, including an unidentified shape for all-zero data with probability held. | Proved | None |
| `gompertz` | Both free: a unique finite MLE exists for nonconstant data exactly when `mean(x^2) < 2*mean(x)^2`; constants diverge; held cases follow the scalar score tests, with a flat growth likelihood for zeros when initial hazard is held. | Proved | None |
| `sgomp` | Every nonconstant nonnegative sample has a finite joint MLE; either single held coefficient gives a unique finite conditional MLE under its stated data condition, but joint uniqueness is unknown. | Existence and conditional uniqueness proved; joint uniqueness unknown | None |
| `naka` | Both free: a unique finite MLE exists exactly for nonconstant positive data; held shape permits constants; held scale fails only when every squared observation equals it. | Proved | None |

### Inverse Weibull

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invweibull/README.md).
Taking reciprocals gives Weibull observations with shape preserved and Weibull
scale equal to the reciprocal of the public inverse-Weibull scale. The Jacobian
is independent of the parameters. The profiled shape score is strictly
decreasing, with the required opposite endpoint signs for nonconstant data.
Holding scale also leaves a strictly decreasing shape score, except that
matching constant data allow shape to increase without bound. Holding shape
leaves the explicit scale maximizer. Holding both evaluates the likelihood.

**Disagreements and open entries:** The existing `limits` inventory records the
held-scale collapse but omits the fully free constant-sample collapse. The new
existence statement and prose cover both. Adding a limit record is outside this
job's permitted limit-text edits. The common adapter's unbounded-likelihood
message mentions a held scale even for fully free constant data, and describes
transformed observations rather than the public data condition. A follow-up
should distinguish those cases. That string does not repeat a changed structural
hold reason or advice, so it was outside the authorized message edits.
No mathematical existence or uniqueness question remains within the stated scope.

### Fréchet

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/frechet/README.md).
Holding location gives inverse Weibull on shifted observations. Holding location
and shape gives the explicit scale maximizer. Holding location and scale gives
a strictly concave shape likelihood, with divergence exactly at matching
constant shifted data.

With only shape held, put `p = lambda*(n-k)-k`, where `k` is the minimum
multiplicity. The profiled likelihood has order `epsilon^p` as the threshold
approaches the minimum. For `p > 0`, it tends to zero at both ends of the location
gap domain and therefore attains a finite maximum. For `p <= 0`, a global
derivative bound proves rejection, including the finite unattained equality
face. The dossier gives the derivative and the inequality using
`r_i = epsilon/(x_i-min(x)+epsilon)`.

With shape and scale held, the location score has exactly one zero. The ratio
`sum(d_i^(-lambda-1))/sum(d_i^(-1))` decreases strictly as the location gap
increases, by a nonnegative covariance inequality. Its endpoint limits bracket
the score zero. This proves a unique finite location MLE, even for constants
and shapes below one.

A separate explicit path, `mu = min(x)-sigma*exp(-L)` and `lambda = c/L`,
shows an unrestricted spike even with scale held: likelihood grows in proportion
to `exp(k*L)/L^n`. The lower shape restriction excludes this path. The fixed-shape
scale-collapse path and the Gumbel density limit are also derived explicitly.
The Gumbel limit is proved as a density limit; that grade does not certify the
numerical comparison against all finite candidates.

**Disagreements resolved:** The old restriction reason asserted prevention of
free-threshold collapse without the exceptional-tie qualification. The new
reason states that qualification and explains the small-shape spike with scale
held. The old prose described a score-gate exception to optimizer failure that
is not implemented by the Fréchet common adapter. It now states the actual
failure behavior: unsuccessful fits stop.

**Open:** Joint uniqueness, a complete restricted joint existence criterion,
and global coverage of the free-location searches remain unproved. The
nonconstant, scale-held restricted problem also needs a complete argument rather
than an inference from the older analysis's qualified classification. The
half-minimum-tie face comparison retains `firmness: numerical`. A proof that the
face always dominates every finite competitor is absent. Finite grids and
refined candidates do not supply that proof. In particular, the held profiles
search a dimensionless log gap on `[-32,32]`; checking its edges does not by
itself exclude every larger maximum outside that interval for a profile without
a proved shape. The joint GEV profile likewise uses numerical root discovery.
No stronger optimizer certificate is claimed.

**Open outside ownership:** The Fréchet row of
[the shared theory note](../ml-behaviour.tex) says that a spike with shape tending
to zero is “not excluded” by the lower bound one. That phrase is wrong: the bound
excludes that path. The dossier now gives the correct argument. The shared note
was explicitly excluded from edits.

### Discrete Weibull

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dweibull/README.md).
The retained Hessian derivation proves strict joint concavity in
`(log(-log(q)), beta)` once a count is positive. Either hold is an affine
restriction in those coordinates. Boundary arguments show attainment exactly
when the free-fit sample range is at least two. Constants and adjacent counts
have excluded saturated limiting fits. With shape held, a positive count is
necessary and sufficient for a unique probability estimate. With probability
held and a positive count present, a count of at least two is necessary and
sufficient for a unique shape estimate. All zeros with probability held leave
shape unidentified; the warning and representative shape one remain documented
in R notes. Both held coefficients leave likelihood evaluation.

**Disagreements resolved:** The generic numerical-search disclaimer was replaced
by a description consistent with the existing concavity proof and scalar
bracketing method. This does not turn numerical regression into proof; the
explicit Hessian and boundary arguments supply the mathematical result.
**Open:** None for the stated existence classification. The existing
representability limit near probability one, conditional refit and failure when
the probability rounds to one remain documented. No numerical behavior changed.

### Gompertz

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gompertz/README.md).
Profiling initial hazard gives `a = n/I(b)`, with
`I(b) = sum(integral_0^x_i exp(b*t) dt)`. The profile is a linear function minus
`n*log(I(b))`. Strict convexity of the log moment-generating integral proves
strict profile concavity. Its derivative at zero gives the raw-second-moment
criterion, and its limiting derivative is negative for nonconstant data.
With initial hazard held, the growth likelihood has strictly negative second
derivative for positive sample sum; its score at zero is
`sum(x)-a*sum(x^2)/2`. With growth held, initial hazard is analytic. Zeros with
initial hazard held give a flat likelihood in growth, represented by growth one.
All-zero data with initial hazard free are unbounded. Positive constants with
both free also have unbounded likelihood.

**Disagreements resolved:** The original free exponential-face data condition
also included all-zero data algebraically, although the public fitter classifies
those as unbounded. The new condition explicitly requires nonconstant data.
The held-initial-hazard exponential face now explicitly requires positive sample
sum, separating it from the flat all-zero case.
**Open:** The limits inventory omits fully free constant-sample collapse.
The mathematical classification itself is settled under the stated domain.

### Shifted Gompertz

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/sgomp/README.md).
At held rate, the shape score decreases strictly whenever a positive observation
is present and has a negative limiting value. A nonpositive initial score selects
the included exponential member. For all-zero data the score is the negative
constant `-n`, so shape zero is again the unique conditional maximum.

At held shape the second rate derivative is strictly negative, including the
term `-n/b^2`; the complete formula is in the existence reason. For positive
sample sum the likelihood tends to zero at both ends of the rate domain.
This proves a unique finite rate maximizer, including the explicit exponential
fit at held shape zero. All-zero data with free rate are unbounded.

For the joint nonconstant case, the two rate envelopes in the dossier exclude
arbitrarily small and large rates uniformly over shape. Bounded rate bounds the
maximizing shape, and shape zero is included. Compact likelihood level sets
therefore prove attainment. Constant positive data instead diverge on the path
`eta = exp(b*x_1)`, with rate increasing without bound.

**Disagreements:** No contradictory held sets or status fields were found.
**Open:** Joint uniqueness and certified enumeration of every rate-profile
maximum remain unknown. Conditional concavity does not imply joint concavity.
The current finite grid is not such an enumeration certificate. The family also
has no `limits` records for its existing constant/all-zero collapse failures;
those should be added in a separately authorized metadata change. The attained
shape-zero exponential member must not be described as an escape.

### Nakagami

[Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/naka/README.md).
Squaring gives gamma observations, but holding the public scale holds the mean
of that gamma law, not its gamma scale. With public scale free its maximizer is
the empirical second moment. The shape equation has left side
`log(m)-digamma(m)`, strictly decreasing from infinity to zero. With scale free,
the right side is the positive Jensen gap for nonconstant squared data. With
scale held, it is the mean of `z-1-log(z)`, with `z=x^2/scale`, and vanishes exactly
when every squared observation equals the held scale. Zero right side gives
unbounded shape likelihood. Holding shape always gives the unique scale
maximizer, including for constants. Both held coefficients leave evaluation.

**Disagreements and open entries:** The existing limit records only the
held-scale collapse; the fully free constant-sample case is missing from that
inventory. Both cases are now stated in the existence field and prose. No
mathematical classification question remains here. Squaring outside
floating-point range is documented separately as numerical failure.

## Shared documentation follow-ups

The full-sentence source text exposes a generator incompatibility outside the
editable scope. `bindings/r/scripts/codegen_wrappers.R`, in `limit_sentence`,
prepends “When” and “as”, adds punctuation, and formats a non-math data condition
as code. For example, generated `mlinvweibull.Rd` contains
“when Every observation equals the held scale ... .,” and “as The free shape ... ..”.
The generator needs to render complete source sentences directly. Neither that
script nor generated help was edited manually.

The same generic template states non-existence after a numerical face comparison
unless the separate `numerical` flag is present. Fréchet's protected flags were
left unchanged. Its new text explicitly distinguishes a proved limiting path
from an uncertified numerical comparison; the renderer should preserve that
qualification rather than imply a global non-existence certificate.

No common-adapter string duplicated the changed Fréchet advice. No structural
hold reason changed. Consequently no common adapter or R test regex needed an
edit. The misleading inverse-Weibull diagnostic described above needs a separate
message change under broader ownership.

## Validation

All builds, tests and numerical validation ran locally through
`/tmp/umvl-cpu/run` or `run4`, under the
launchers' `nice -n 10` and one-thread BLAS/OpenMP settings. Heavy commands held
at most four slots. No simbox, package installation or sub-agent was used.
Temporary files were under this worktree's `build/tmp` and `build/scratch`.
Environment: Linux `7.0.0-38-generic`, R `4.6.1`, GNU C++ `15.2.0`.

| Check | Result |
| --- | --- |
| Semantic YAML comparison with HEAD, excluding only authorized text and the new existence field | PASS for all six; protected fields, held sets, bounds, statuses, flags, relations, examples and catalog data unchanged |
| `/tmp/umvl-cpu/run4 just regen` | PASS on the corrected final sources |
| Required targeted `testthat::test_local` filter | FAIL 0, WARN 0, SKIP 8, PASS 7868; 170.1 seconds reported by testthat |
| Required all-`ml*.Rd` scan, filtering Non-ASCII diagnostics | No diagnostics for the six assigned families; 26 diagnostics confined to untouched `mlst.Rd` |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | PASS; 99 families derived |
| `/tmp/umvl-cpu/run just relations` | PASS; 157 edges, 0 failed |
| `/tmp/umvl-cpu/run4 just cpp-test` | PASS; 106/106 dev shards, 0 failed; CTest reported 46.98 seconds |
| Final `/tmp/umvl-cpu/run4 just test` | FAIL 0, WARN 0, SKIP 151, PASS 20102; elapsed 128.090 seconds for its final timed R test command |
| `git diff --check` | PASS |

The targeted filter was
`dist-(invweibull|frechet|dweibull|gompertz|sgomp|naka)|conformance|restricted|generated-dpqr|parameter`,
with `stop_on_failure = TRUE` and `UMVL_DISTRIBUTIONS_DIR` pointing to this
worktree's canonical distribution directory. The eight targeted skips require
the full tier. Existing test seeds and tolerances were unchanged. The full tier
was not required for these documentation-only changes; fitting, d/p/q/r and test
infrastructure code were untouched. Regression and conformance passes are
integration evidence, separate from the mathematical arguments above.

The Rd diagnostics outside ownership are unknown math macros and lost braces
at `mlst.Rd:33`, plus an empty section at lines 57–59. The assigned six families
meet the requested clean-help condition. CMake emitted its CMP0167 developer
warning, and login shells emitted the existing missing Cargo-environment notice;
these did not fail the commands or produce testthat warnings.

The first regeneration attempt failed because four newly edited YAML data
conditions lacked closing quotes. Those quotes were corrected. The subsequent
semantic YAML check, regeneration and every test run listed as passing above
used the corrected final sources. No status, default fit or golden was changed.
All started validation commands have finished.

## Tested dossier fingerprints

SHA-256 of each final canonical `README.md`:

- `invweibull`: `015455e151057b5611b02ed0df0a6aa13698e44a11035e007f6b935236dde88e`
- `frechet`: `6e48162575ebc61213d1f5badb903056d0b59a137e107e7c1017b3f6a4ab753e`
- `dweibull`: `1081a75b967a7ce4693bb87e3e2f9d602b7adc813a81400899d15201521596ec`
- `gompertz`: `e8107ffa6292041cc95b0fa33d3d7f617fa59eda885b5da7773e1641a208caca`
- `sgomp`: `6f51fedcb49068b0c8047da8dfca0eced7de750bf2bdfe7f18aca2cade361e6f`
- `naka`: `8159019db8f82ce003ecccf147441ba45a59d736bd856e986ad3e6a123ab3bdb`

## Artifact display

The required local `bin/show` command was attempted after the report was written.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. The report remains at the requested worktree path. No file was
copied into the main checkout or another directory, and no viewer configuration
was changed.
