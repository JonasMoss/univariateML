# existence-13: existence statements and reader-facing prose

Completed 2026-11-02 for TASK-101.6.4.1 on `codex/existence-13`.
Validated source: `9578745158d3d15c210707f34a7ce83f37b4296e` plus the
uncommitted dossier changes in this worktree. No commits or board edits were
made. Only the five owned dossiers and this report were authored.

## Scope and grading

Each dossier now has `existence: {uniqueness, statement, reason}`. The
statements distinguish the unrestricted model, the fitting restriction where
applicable, and held coefficients. Limit paths and detection conditions are
sentences that distinguish mathematical limits from numerical comparisons.
Coefficient meanings are explained, R-specific alternatives are in R notes,
and dated implementation evidence is retained in decision history.

The YAML uniqueness grade is `proved` for `dnorm` and `unknown` for the other
four families. An unknown joint uniqueness grade does not negate the proved
conditional results below. Numerical fit comparisons and passing regression
tests are not proofs of existence or global optimality.

No status, held set, fitting bound, exemption, firmness grade, numerical flag,
relation, fitting control, inference claim, or other protected metadata changed.
No fitting code, d/p/q/r code, golden, truth table, or authored test changed.
There were no hold-reason or advice changes requiring matching adapter strings
or R regex edits. The revised `st` fitting-restriction reason has no identical
string in its common binding.

## Family findings

### st — Azzalini skew-t

**Statement:** The unrestricted all-free likelihood is unbounded for every
nonempty sample; restricting free degrees of freedom to at least one prevents
the stated divergence below half-mass ties but does not establish attainment
or uniqueness, and general held-subset existence remains unknown.

**Grade:** Proved collapse and boundedness results; unknown general attainment
and uniqueness. With fixed finite skewness and degrees of freedom, collapse at
a location observed `k` times has order `omega^(nu*(n-k)-k)`. Taking skewness
zero and sufficiently small positive degrees of freedom proves unrestricted
divergence. The uniform Cauchy-density bound establishes boundedness for
`nu >= 1` when no multiplicity exceeds half the sample. Holding positive scale
also gives a uniform density bound. A held location away from all data gives
bounded likelihood, without proving attainment.

**Disagreements and changes:** Equality of the collapse exponent gives a finite
path limit, not a non-existence theorem. The limit text and existence statement
now say so explicitly. The restriction reason describes its actual scope
rather than suggesting an existence guarantee. Numerical face-detection text
is separated from the proved fixed-parameter limit paths. **Status changes:
none**; `ScaleBoundary` appropriately denotes an unresolved boundary.

**Open:** Joint moving-location/scale/shape paths, general held-set attainment
and uniqueness, and possible held-set exemptions from the degrees-of-freedom
restriction. The multistart and face searches have no global certificate.
The existing unbounded diagnostic says “at a tied observation”, although an
unrestricted collapse can use a singleton multiplicity; changing that message
is outside the permitted hold/advice string edits.

Canonical arguments and retained numerical history:
[st dossier](../../../univariateML-cpp/include/cppunivariate/distributions/st/README.md).

### slash

**Statement:** With both coefficients free, attainment holds below half-mass
ties, divergence occurs above half, and exact half-mass cases require the
interior-versus-collapse comparison; held location gives the stated unique
scale root, while held scale guarantees attainment without location uniqueness.

**Grade:** Proved existence conditions and held-location scale uniqueness;
unknown general joint uniqueness. The collapse exponent is `n-2k`. Other
escaping paths lose likelihood, and free location can be confined to the
sample range. At exactly half mass the dossier retains the exact variational
criterion using the finite values `B_v`, including its equality case. With
location held at the half-mass value, every positive-scale likelihood lies
strictly below its collapse supremum. Separated symmetric observations prove
that held-scale location maxima need not be unique.

**Disagreements and changes:** No contradiction remains between the owned
fields. Detection now explicitly distinguishes numerical free-location face
comparison from the exact held-location half-mass result. The existing limits
list has no separate unbounded-majority record; the new existence statement
and prose cover it without changing the list structure. **Status changes:
none.**

**Open:** Free-scale location mode isolation and a verified implementation of
the scale-root bracket, including neighborhoods of half-mass collapse. The
current fixed lower log-scale bracket is not an error enclosure. The
held-scale certificate remains conditional on computed comparisons, with its
stated practical `1e-9*(1+abs(logLik))` margin, not outward-rounded arithmetic.

Canonical criterion, counterexamples and certificate scope:
[slash dossier](../../../univariateML-cpp/include/cppunivariate/distributions/slash/README.md).

### emg — exponentially modified Gaussian

**Statement:** The all-free likelihood is unbounded for constant samples and
can escape to normal or translated-exponential limits otherwise; joint
existence and uniqueness lack a complete criterion, while holding both physical
scales gives a unique finite location maximum.

**Grade:** Unknown joint existence classification and uniqueness; proved
limiting laws and conditional concavity results; numerical face selection.
The independent-sum representation gives the normal limit as the exponential
rate diverges and the translated-exponential limit as normal scale vanishes.
With a held endpoint at an observation, the latter density has the half-value
penalty. Fixed dimensionless shape `lambda*sigma` gives the strictly concave
location/inverse-scale subproblem established in the linked local proof.
Holding the physical rate alone is a different constraint. With both physical
scales held, strict location log-concavity and vanishing tails give a unique
finite location maximum.

**Disagreements and changes:** The prose now allows every finite real normal
location in the limit argument and clarifies dimensionless shape versus held
physical rate. R fitter alternatives and their failure to automatically
preserve holds are in R notes. The constant-data unbounded case is described
in existence/prose, although the existing limits list contains only the two
named faces. **Status changes: none.**

**Open:** A complete criterion for general held subsets and a global outer
shape certificate. A face winning within a numerical tolerance does not prove
non-existence of a finite maximizer. Neither empirical skewness sign nor
fixed-shape concavity settles this outer problem.

Canonical qualifications and proof/evidence links:
[emg dossier](../../../univariateML-cpp/include/cppunivariate/distributions/emg/README.md).

### bhatt — Bhattacharjee

**Statement:** The all-free constant-sample likelihood is unbounded; for
nonconstant data the uniform face can prevent attainment, whereas `a = 0` is
an attained normal member; held positive normal scale guarantees attainment,
and the held-positive-width cases have the stated range/distance criteria.

**Grade:** Proved selected held-case existence and conditional location
uniqueness; argued general face exhaustion; unknown joint uniqueness and
general data-only existence classification. The bounds by `1/(2a)` for
positive width and by `1/(sqrt(2*pi)*sigma)` control scale divergence. With
normal scale held positive, diverging width loses likelihood, bounded width
and escaping location lose likelihood, and the remaining width domain includes
zero. Compactness therefore gives attainment, with location held or free.

With positive width held and location free, attainment holds exactly when the
sample range exceeds `2a`. With both location and positive width held, it holds
exactly when some observed distance exceeds `a`. Otherwise the pointwise
uniform-face bounds, including the half-density factors at held endpoints,
give an unattained supremum. With both scales held, free location is strictly
log-concave and has a unique finite maximum. These arguments do not prove
uniqueness of fitted scales.

**Disagreements and changes:** The old sentence saying constant samples
collapse *only* when both scales are free contradicted the included normal
member: holding `a = 0` still permits normal scale collapse. It now includes
width free **or held at zero**, with normal scale free and location able to
coincide with the data. The existing normal delegate already returns the
unbounded status in that case. “Holding a removes the normal face” is qualified
to positive held width. Normal membership, uniform escape and held coefficients
now agree across the prose and fields. The limits list still omits a separate
constant-collapse record. **Status changes: none.**

**Open:** Global free-shape mode exclusion, joint uniqueness, and a complete
general free/held-scale classification by sample statistics. The fixed-shape
strict-concavity theorem does not certify the numerical width search.

Canonical arguments and retained numerical history:
[bhatt dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bhatt/README.md).

### dnorm — discretized normal

**Statement:** With both coefficients free a unique finite MLE exists exactly
when the integer sample range is at least two; held positive scale always gives
a unique location fit, and held location gives a unique scale fit unless all
observed cell closures contain that location.

**Grade:** Proved. Strict concavity in `(mu/sigma, 1/sigma)`, affine held
constraints, and compact positive-likelihood level sets give attainment and
uniqueness away from the declared zero-scale exceptions. Constant and adjacent
cell samples approach their saturated multinomial bounds but cannot attain
them at positive scale. In the exceptional held-location case, every finite
cell probability is strictly below its zero-scale limit. Likelihood is bounded
by one, so these failures are unattained suprema rather than divergence.

**Disagreements and changes:** No inconsistency remains between existence,
limits, holds and domains. The introduction now explicitly says floor of a
latent normal variable, and explains that coefficients describe the latent
normal. R Bernoulli alternatives and the dated numerical-tail note were moved
to their respective sections. **Status changes: none.**

The concavity reference's bibliographic identity was verified against the
publisher search record: Burridge, J. (1981). *A Note on Maximum Likelihood
Estimation for Regression Models Using Grouped Data*. Journal of the Royal
Statistical Society, Series B (Methodological), **43**(1), 41–45.
[Publisher DOI record](https://doi.org/10.1111/j.2517-6161.1981.tb01146.x).
The compactness and boundary arguments are retained in the dossier; no claim
is made that the publisher's full article was accessible through the web tool.
One web search was used, within the eight-search budget.

**Open:** No remaining mathematical existence/uniqueness question for the
stated finite-sample regimes. Finite iteration budgets can still cause a
numerical fitting failure.

[dnorm dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dnorm/README.md).

## Out-of-scope generated-help issue

The generic limit preamble in generated `mlst.Rd` declares that the MLE does
not exist for scale-collapse equality, immediately before the dossier's
qualification that the finite path limit does not establish non-existence.
The status itself is appropriately unresolved; changing it would not repair
the generator's interpretation. The code generator needs a way to distinguish
an unresolved boundary from proved non-existence, and to avoid presenting
numerical face selection as a global theorem. `bindings/r/scripts/` and new
machine-readable limit fields were outside this job, so neither was edited.

## Validation

All runs used the **local workstation**, never simbox. Environment: Linux
`7.0.0-38-generic`, R `4.6.1`, GCC `15.2.0`, existing Rcpp/BH dependencies and
the shared ccache configured by the Justfile. CPU work used the shared slot
wrappers with `nice -n 10`, one BLAS/OpenMP thread, and at most four build/test
workers. Commands remained in the foreground through slot waits and completion.

| Check | Machine | Result |
|---|---|---|
| `run4 just regen` | Local | Pass; final `just test` regenerated again after the final prose correction. |
| Requested targeted R filter | Local | FAIL 0, WARN 0, SKIP 12, PASS 8067. All skips require the full tier. |
| All-family Rd scan, Non-ASCII filtered | Local | All five owned families clean; nine diagnostics in unchanged `mlskellam.Rd` only. |
| Final five-family Rd check | Local | Pass, no diagnostics, including the final Bhattacharjee text. |
| `python3 site/scripts/data.py` | Local | Pass; 99 families. Repeated after the final prose correction. |
| `just relations` | Local | 157 edges, 0 failed. |
| Required control-character grep | Local | No output; repeated on the final dossiers. |
| `just cpp-test` | Local | 106/106 dev shards passed; 0 failed. |
| Final `just test` | Local | FAIL 0, WARN 0, SKIP 151, PASS 20121. All skips require the full tier; counts match the supplied baseline. |
| Final protected-metadata comparison | Local | Deep YAML equality after removing only permitted prose/existence fields; all five pass. |
| `git diff --check` | Local | Pass. |

The wider Rd diagnostics concern a Skellam display-math paragraph (unknown
macros and lost braces). They are not diagnostics in this job's families and
were not repaired across the ownership boundary. Full-tier, no-long-double
and JavaScript suites were not required for these dossier-only changes; no
fitting, d/p/q/r or test-infrastructure implementation changed.

Reproduction, from this worktree root:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p build/tmp build/scratch
/tmp/umvl-cpu/run4 just regen
(cd univariateML-r && /tmp/umvl-cpu/run4 env \
  UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions \
  Rscript -e "testthat::test_local('.', filter = 'dist-(st|slash|emg|bhatt|dnorm)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)")
(cd univariateML-r && /tmp/umvl-cpu/run Rscript -e \
  'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }')
/tmp/umvl-cpu/run python3 site/scripts/data.py
/tmp/umvl-cpu/run4 just relations
grep -lP '[\x00-\x08\x0b\x0c\x0e-\x1f]' \
  univariateML-cpp/include/cppunivariate/distributions/{st,slash,emg,bhatt,dnorm}/README.md
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test
```

The control-character grep is expected to exit 1 because it finds no match.
The final owned-family Rd check additionally stops on any non-ASCII-filtered
diagnostic for `mlst`, `mlslash`, `mlemg`, `mlbhatt` or `mldnorm`. Raw logs are
under `build/scratch/` in this worktree; the durable outcomes and reproduction
commands are recorded above.

## Delivery

The requested report exists at this path. The `codex-math-web/bin/show`
attempt returned HTTP 403: this worktree is absent from the viewer's configured
readable roots, despite being the current thread's working directory. Opening
it through that helper remains **open** pending viewer registration of this
worktree. No viewer configuration or file outside this worktree was changed;
the report is provided as a direct file link in the final response.
