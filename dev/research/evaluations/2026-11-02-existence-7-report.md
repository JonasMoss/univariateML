# existence-7: existence statements and reader-facing prose

TASK-101.6.4.1. Work completed in `codex/existence-7`, based on
`6fea876ebeec88594a6de4b8af0db4adb5a7d452`. Validation ran locally on
2026-11-01 (Europe/Oslo). No commits, board edits, installs, other-worktree
edits or numerical-code changes were made.

## Scope and results

The six canonical dossiers now have `existence` fields, coefficient explanations,
complete limit-path and detection sentences, and conditional-fit explanations.
R-specific usage is in `## R notes`; historical implementation material is in
`## Decision history`. No family has advice, fit-domain reasons or structural
hold exceptions requiring a message change. Binding messages and test regexes
therefore remain unchanged.

All statements assume nonempty positive finite samples and valid held coefficients.
Holding all coefficients evaluates the likelihood. The schema's uniqueness grade
applies to the family-wide claim; proved conditional results do not make an
unresolved joint uniqueness claim `proved`.

| Family | Statement in one line | Grade and argument |
|---|---|---|
| [llogis](../../../univariateML-cpp/include/cppunivariate/distributions/llogis/README.md) | The joint MLE exists uniquely for nonconstant samples; holding shape always gives a unique scale, while holding scale gives a unique shape unless every observation equals it. | **Proved.** Logarithms give a logistic likelihood strictly concave in inverse scale and standardized location; the boundary bounds give attainment in exactly these cases. |
| [burr](../../../univariateML-cpp/include/cppunivariate/distributions/burr/README.md) | Joint existence is data-dependent and joint uniqueness unresolved; fixed scale gives existence for nonconstant data with an observation below it, while held shapes have the conditional results detailed below. | **Unknown** jointly; **proved** conditional results and concentrating paths; joint boundary selection remains **numerical**. Profile the first shape at fixed scale; with the second shape held use the exact Lomax existence criterion; with the first shape held use strict concavity on log data. |
| [invburr](../../../univariateML-cpp/include/cppunivariate/distributions/invburr/README.md) | The Burr classification transfers by reciprocation, reversing the fixed-scale data inequality and replacing Weibull/Pareto by inverse Weibull/power. | **Unknown** jointly; conditional claims **proved** by the parameter-independent reciprocal Jacobian and the Burr arguments. |
| [paralogis](../../../univariateML-cpp/include/cppunivariate/distributions/paralogis/README.md) | The joint MLE exists for nonconstant samples and is unbounded for constant samples; either held coefficient permits a finite maximum for every sample, with scale unique when shape is held. | Existence **proved** by the new uniform density envelope below; family-wide uniqueness **unknown**. Fixed-scale endpoint bounds prove attainment, while the held-shape scale score is strictly monotone. |
| [invparalogis](../../../univariateML-cpp/include/cppunivariate/distributions/invparalogis/README.md) | The paralogistic classification transfers by reciprocation, including finite held-coefficient fits for constant data. | Existence **proved** by reciprocation; family-wide uniqueness **unknown**. |
| [betapr](../../../univariateML-cpp/include/cppunivariate/distributions/betapr/README.md) | Joint existence is data-dependent and uniqueness unresolved; fixed-scale nonconstant data and the one-free-coordinate problems have unique maxima, while one held shape with free scale retains a gamma or inverse-gamma escape. | **Unknown** jointly and for general single-held-shape existence; fixed-scale and one-coordinate claims **proved** by the beta transformation and monotone scores. Gamma-ratio asymptotics prove the limiting densities, not global selection by the numerical search. |

No new external citation was needed or added. These are explicit local arguments,
with links to the prior family analyses retained. No web searches were used.

## Arguments, disagreements and status changes

### Log-logistic

The log-data likelihood is
`n log b - 2 sum log(2 cosh((b y_i-a)/2))`, up to a data-only term, with
`b = shape` and `a = shape * log(scale)`. Its Hessian is negative definite.
After profiling location, a nonconstant sample forces the likelihood to minus
infinity as inverse scale grows; it also tends to minus infinity as inverse
scale tends to zero. Held shape and held scale are affine slices of these
coordinates. This proves the stated unique maxima and collapse exception.

No status changed. **Open metadata gap:** the existing collapse limit record
is restricted to `[scale]`. Its prose now correctly describes that held-scale
case; the existence field also describes the all-free collapse. Adding a separate
all-free limit record requires a protected held-set/limit-inventory change and
was not done.

### Burr and inverse Burr

With the first shape `k` held, the log-data likelihood is strictly concave in
`c` and `c log(s)` and has a unique maximum for nonconstant data. With `c` held,
`X^c` is Lomax, so the exact existence criterion is the one in the
[Lomax dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md),
not a sample-variance shortcut. With both shapes held, the log-scale score has
one root for every positive sample. Holding `k` and scale leaves a strictly
concave likelihood in `c`, unbounded only when all observations equal scale.
Holding `c` and scale leaves an explicit positive `k` estimate.

**Disagreement corrected in the prose:** the historical
[P0 generalized-beta analysis](2026-10-29-p0-genbeta.md) says that an observation
below held Burr scale guarantees a finite maximum, without excluding constant
samples. For constant `x0 < s`, set `z=(x0/s)^c` and profile
`k=1/log1p(z)`. The density at the observation is

`c/(e*x0) * z/((1+z)*log1p(z)) ~ c/(e*x0)`.

It diverges as `c` grows. Thus constant data below a held Burr scale are also
unbounded. Inverse Burr has the corresponding exception above its held scale.
The dossiers now distinguish these cases from the nonconstant existence result.
The historical report is preserved, outside this job's edit scope.

No status changed. **Open diagnostic gap:** the existing fixed-scale fitters
explicitly recognize collapse at the held scale but not this additional path.
On `mlburr(rep(0.5,3), scale=1)` and
`mlinvburr(rep(2,3), scale=1)`, the current bindings both stop with
`Constrained likelihood search failed to converge.` They should identify
unbounded likelihood. No finite estimate was returned in these probes.
The analytic path proves divergence; the numerical values below only illustrate it.

| Burr power `c` | Log-density at `x0=0.5`, held `s=1`, profiled `k` |
|---:|---:|
| 10 | 1.99524427027 |
| 20 | 2.68887897728 |
| 40 | 3.38202663467 |

For nonconstant observations all at least held Burr scale, the Pareto-path
supremum includes the penalty `Z log(2)` for observations equal to scale.
The reciprocal power-path statement retains this penalty. Neither dossier
asserts a complete all-free sample criterion or joint uniqueness.

### Paralogistic and inverse paralogistic

The dossiers contain a direct joint existence proof, strengthening the prior
boundary argument. For paralogistic log-data, write `a=shape`, `v=log(scale)`
and `t=a(y-v)+log(a)`. The density on the log-data axis is
`g_a(t)=a*exp(t)/(1+exp(t)/a)^(a+1)`. For `a>=1`,

`g_a(t) <= 4^(3/5)*a*exp(-3*abs(t)/5)`.

This follows from the log derivative being at most `-3/5` above `log(4)`,
the bound `g_a(t)<=a*exp(t)` for negative `t`, and the maximum being at most
`a`. For nonconstant log-data range `Delta`, the joint log-likelihood is at
most `n log(4^(3/5)*a) - (3/5)*a*Delta`, uniformly over location. As
`a` tends to zero, the maximum density
`a*(a/(a+1))^(a+1)` tends to zero. Escaping location on compact positive
shape intervals also forces likelihood to zero. Hence a finite joint maximum
exists. For constant data, `v=y0+log(a)/a` gives density asymptotic to `a/e`.
Reciprocation proves the inverse statements.

**Every status change:** both dossiers' sole unbounded-likelihood limit records
changed from `InvalidStart` to `Unbounded`. The concentrating path above proves
unboundedness, and the existing `ParalogisStatus::Unbounded` core path and
reciprocal delegate already report it. This corrects metadata without changing
fitter behavior or enum definitions.

**Open:** joint uniqueness and uniqueness of the fixed-scale shape maximum
remain unproved. The latter's endpoint argument proves existence only; it
must not be promoted to uniqueness. The joint optimizer has no global certificate.

### Beta prime

With scale held, `Y=X/(b+X)` gives a beta likelihood with a parameter-independent
Jacobian. Strict concavity gives uniqueness for nonconstant data; constant data
permit two-shape collapse. A single free beta shape has a decreasing digamma
score. With both shapes held, the log-scale score decreases from `nq` to `-np`.
The gamma and inverse-gamma limiting paths and their total-variation convergence
are justified by gamma-ratio asymptotics and Scheffé's lemma.

For constant data with `p` held and scale profiled, the derivative per observation
in the free shape is `psi(p+q)-psi(q)-log((p+q)/q)>0`. Integrating
`psi'(t)>1/t` proves the sign. Reciprocation gives the other held-shape case.
These are finite unattained limit suprema, rather than unbounded collapse.

No status changed. The existing `InvalidStart` record for two-free-shape constant
data agrees with the implementation, although it is less specific than a
dedicated unbounded status. **Open:** a complete joint sample criterion, joint
uniqueness, and exact general sample criteria for the one-held-shape/free-scale
problems remain unresolved. The existing bounded numerical profile and joint
multistart searches are not global certificates; their limitations are stated
rather than turned into theorems.

## Generated-help and detection follow-ups

- **Open, protected numerical flags:** the Burr-family and beta-prime limit
  records retain their existing flags and `firmness: proved`. The proved claim
  is the limiting path. Their detection comparisons are numerical, and the new
  `detect` sentences say so. The corresponding records need a separate review
  of `numerical: true` so generated help does not assert mathematical
  nonexistence from a numerical comparison. The unchanged `10^-8*n` preference
  can select a limit over a tied or slightly better finite maximum.
- **Open, generator prose:** the current help template embeds complete sentences
  after `when` and `as`, adding punctuation again. This produces phrases such as
  `as The shape ... scale..`. The dossier fields follow the required Site prose
  rule; the generator should render full sentences directly. No generated or
  codegen source was edited.
- The all-family Rd scan found 26 diagnostics in the untouched `mlst.Rd`
  (raw-math macros, an empty section and lost braces). None concerned these six
  families. This requires a separate skew-t/codegen fix.

## Validation

All builds and computations ran on the local workstation through
`/tmp/umvl-cpu/run4`, with the lock's `nice -n 10`, four build/test workers and
`OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=MKL_NUM_THREADS=1`. Slot waits completed
normally. Temporary files used `build/tmp`; logs and the deterministic probe
used `build/scratch`. No simbox run was made.

Environment: Linux x86_64 `7.0.0-38-generic`; GCC 15.2.0; Python 3.14.5;
R 4.6.1; testthat 3.3.2; Rcpp 1.1.2; BH 1.90.0.1; roxygen2 8.0.0.
Existing test fixtures, seeds and tolerances were retained; no stochastic study
or new numerical estimator was introduced.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed. |
| R `testthat::test_local('.', filter='dist-(llogis\|burr\|invburr\|paralogis\|invparalogis\|betapr)\|conformance\|restricted\|generated-dpqr\|parameter', stop_on_failure=TRUE)` with `UMVL_DISTRIBUTIONS_DIR` set | **FAIL 0, WARN 0, SKIP 16, PASS 7799**. Skips require the full tier. |
| All `man/ml*.Rd` through `tools::checkRd`, excluding Non-ASCII diagnostics | No output for the six owned families. The unowned skew-t diagnostics are recorded above. |
| `python3 site/scripts/data.py` | Passed; 99 families generated. |
| `just relations` | Passed; **157 edges, 0 failed**. |
| `just cpp-test` | Passed; **106/106 dev shards**. CTest elapsed 75.28 seconds, excluding compilation. |
| Deterministic fixed-scale Burr/inverse-Burr probe | Both stopped, with the diagnostic gap recorded above. Analytic path values agree with divergence. This is a diagnostic observation, not a regression pass claiming correct classification. |
| Final `/tmp/umvl-cpu/run4 just test` | **FAIL 0, WARN 0, SKIP 151, PASS 20102**; test elapsed 205.337 seconds. |
| Protected front-matter comparison against `HEAD` | Passed for all six dossiers after excluding new existence fields, permitted prose edits and exactly the two declared status corrections. |
| `git diff --check` | Passed. |

The sequential validation batch held one four-slot reservation while computing;
commands inside it did not nest slot wrappers. Both R runs set
`UMVL_DISTRIBUTIONS_DIR` to the worktree's canonical distribution directory.
Full-tier R oracles were not run for this documentation-only change. No
fitting, d/p/q/r or test infrastructure was edited. The recorded skips are not
passes and the tests do not prove the unresolved global mathematical claims.

The boundary probe is reproducible after package regeneration:

```r
pkgload::load_all("univariateML-r", quiet = TRUE)
try(mlburr(rep(0.5, 3), scale = 1))
try(mlinvburr(rep(2, 3), scale = 1))
for (c in c(10, 20, 40)) {
  z <- 0.5^c
  k <- 1 / log1p(z)
  print(log(k) + log(c) - log(0.5) + log(z) - (k + 1) * log1p(z))
}
```

Tested dossier SHA-256 values (base revision plus these uncommitted sources):

```text
llogis       82e84f12c0a8b0743772ecfed476c7e381bdeecf90b2618bb21967a5191bb0f7
burr         45e9ec972b0937a8ad0d5efcfbf9d09d0951d25868f6a10f58c31c394116df7c
invburr      be5ad4d0ec057775bd0f2694fd2586e905135928fdd82ce4448c4a44056edc27
paralogis    53c2814ea8e9f2184fad1155460105f8d5958df667608a6d2a07e9efca026338
invparalogis 5608301eec62039e2e12eec599e0c037182b0e69b53b85ecd30977c44ef77a51
betapr       c60ec46ca03498b67567ca5f618773898f7266aa1aae255a3a7d903d57a94353
```

Artifact presentation: the requested `show` command was attempted, but the viewer
returned HTTP 403 because this worktree is outside its configured readable roots.
The report remains at the requested worktree path; no viewer configuration or
files outside the worktree were changed.
