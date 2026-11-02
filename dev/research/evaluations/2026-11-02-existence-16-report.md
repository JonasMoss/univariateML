# existence-16 report — 2026-11-02

Completed TASK-101.6.4.1 for `cmb`, `cmp`, `hyper`, `nhyper` and `lgser` in
`codex/existence-16`. Tested source: `bbb37f337af143d1dcd3eb48881353fbf6aec69f`
plus the five uncommitted dossier revisions identified below. No commits,
board edits, package installations, sub-agents or simbox runs were made.

The change adds existence statements for four families and revises the
existing CMB statement. Reader-facing Details now explain the statistical
coefficients and the effect of holding them. R interface conventions are
under R notes. The existing mathematical derivations, numerical margins,
work budgets, failure rules and historical evidence remain accessible in
the dossiers, with implementation records under Decision history.

## Family results

### cmb

**Statement:** Free-size existence has exact saturated special cases and
conditional tail certificates, but no complete general criterion or global
uniqueness result; fixed-size identifiable fits are unique when their
empirical sufficient statistic is interior.

**Grade:** `unknown` for general free-size existence/uniqueness, with proved
special cases and fixed-design results. For trial count at least two, the
statistic `(x, log choose(n,x))` has a strictly concave upper polygonal hull.
Its covariance is positive definite, so the log likelihood is strictly
concave in log odds and dispersion. The polygon interior gives the unique
finite interior maximum. Its upper and lower edges expose adjacent and
endpoint laws. Endpoint probabilities also attain endpoint point masses.
Holding interior probability reduces the condition to an empirical log
binomial coefficient strictly between its minimum and maximum. Holding
finite dispersion leaves a unique probability fit for positive trial count.
At sizes zero and one the stated non-identifiabilities remain.

Saturated categorical-likelihood bounds prove the free-size constant,
Bernoulli, adjacent-positive, endpoint-pair and uniform cases. A finite
candidate above the CMP face, together with the retained truncation and
entropy bounds, gives a finite search cutoff in the documented cases.
These certificates are exact-arithmetic arguments conditional on computed
comparisons, with practical floating-point margins; they do not establish
unconditional rounding enclosures or uniqueness across integer sizes.

**Status changes:** None. In particular, a sample on zero and one has an
attained Bernoulli maximum even though the fitter stops on its tie with
the adjacent CMP face. The existence statement now separates attainment
from that stopping policy.

**Field reconciliation:** The fixed-size minimality argument is explicitly
restricted to size at least two. The CMP limit keeps `firmness: proved`
for the limiting path and `numerical: true` for its unresolved detection;
no exploratory comparison is promoted to a non-existence proof.

**Open:** A full free-size existence criterion, global uniqueness across
sizes, and a classification of all free-dispersion limiting paths remain
unsettled. The retained record also identifies the unsupported central-size
construction for positive constants above 5,000,000 with interior probability
held. No fitter change was authorized for these questions.

### cmp

**Statement:** With both coefficients free, zero data attain the zero-rate
member, positive constants and adjacent pairs have unattained suprema, and
other samples have a unique finite maximum, possibly geometric.

**Grade:** `proved`, with zero-rate non-identifiability explicitly excluded
from uniqueness. The covariance of `x` and `log(x!)` is positive definite
at positive rate and dispersion. Strict concavity in `(log(lambda), -nu)`
therefore gives at most one identifiable maximizer. At fixed positive mean,
profiling rate makes expected log factorial strictly decrease with
dispersion from its geometric value to the adjacent-integer lower hull.
Its derivative is the negative Schur complement of that covariance matrix.
An empirical statistic above the hull consequently has either a unique
positive-dispersion moment solution or the attained constrained geometric
maximum. Hull data give the stated exposed-face suprema.

With dispersion held, the mean is strictly increasing in log rate. With
positive rate held, expected log factorial strictly decreases to zero as
dispersion grows. At the lower dispersion endpoint it is finite for rate
below one and diverges otherwise. Thus some count exceeding one gives a
unique finite dispersion fit; data confined to zero and one instead
approach the Bernoulli limit. A held zero rate requires zero data and
leaves dispersion unidentified. All-held fits only evaluate a valid law.

**Status changes:** None.

**Field reconciliation:** The two-point condition now explicitly requires
both adjacent values in the joint fit and includes constant zero/one
samples when a positive rate is held. This excludes the attained free-rate
zero member from the joint escape claim. The geometric boundary and its
normalizability condition agree with the existing domain and fitter.

**Open:** No mathematical uniqueness gap remains for the declared
identifiable submodels. Numerical moment residuals alone are not a
likelihood-error certificate, and normalizer/work/representation failures
remain possible. The existing `fit.certified: partial` is unchanged.

### hyper

**Statement:** Fully free constant samples attain likelihood one; known
population gives finite attainment; with positive composition and draw
count held, population estimation has a finite maximum exactly when at
least one observation is positive.

**Grade:** `proved` for these conditional claims; `unknown` for the general
fully free criterion and uniqueness. A known population leaves a finite
integer grid. With both positive counts held and positive empirical mean,
the adjacent-population ratio is eventually below one. All-zero data
instead have likelihood increasing strictly toward one as population grows.
Zero held composition or draw count leaves compatible zero data unidentified.
The retained binomial/Poisson envelopes certify finite attainment when a
candidate strictly beats the relevant limiting suprema. Symmetry and
adjacent ties prevent a blanket uniqueness claim.

**Status changes:** None.

**Field reconciliation:** The old all-zero limit wording omitted that the
positive composition and draw counts must both be held. If either can be
zero, a finite zero point mass is available. The revised path and detection
condition state this scope. The protected `fixed: [m]` array is unchanged;
the prose supplies the additional draw-count condition. Unresolved and tied
face comparisons are explicitly distinguished from proved escapes.

**Open:** A complete sample criterion for joint unbounded-design existence
and its tie structure is not asserted. Binomial and Poisson face outcomes
used by the current fitter have no separate `limits` records in this dossier;
adding records or changing their held sets was outside the permitted edits.

### nhyper

**Statement:** Fully free constants attain likelihood one; feasible
known-population or known-composition fits attain a finite maximum;
unbounded population/composition searches are certified when a finite
candidate strictly exceeds the relevant negative-binomial suprema.

**Grade:** `proved` for finite-grid/tail statements; `unknown` for the general
joint criterion and uniqueness. With known population the feasible grid is
finite. With white-ball count held and positive stopping count, every factor
of the adjacent-population ratio is below one beyond
`K * max(x) / r`. A free positive stopping count has only finitely many
feasible values, so the same argument leaves a finite union of grids.
When composition is also free, the retained growing-population bound
compares with shifted negative-binomial laws. A strict improvement gives a
finite cutoff, conditional on the computed comparisons and stated margin.
Zero-only samples allow stopping count zero; mixed zero and positive data
are infeasible. Ties and unidentified zero-stop designs remain possible.

**Status changes:** None.

**Field reconciliation:** The old reader prose and protected control text
suggested narrower support for population and stopping-count estimation.
The prose now agrees with the current joint fitter and tests. The protected
`r.control` descriptions still say population estimation needs held `m`
and stopping-count estimation needs `m` or `N`. The protected `fit.note`
also retains an obsolete statement about unsupported joint designs. Those
fields were not authorized for editing.

**Open:** General joint existence/uniqueness and unresolved face ties remain
unclassified. The growing-population face statuses have no `limits` records;
adding them and repairing the stale control/catalog text need a separate edit.

### lgser

**Statement:** A free-parameter finite MLE exists uniquely exactly when the
positive-count sample mean exceeds one; all-one data have an unattained
point-mass supremum, while holding an interior parameter specifies the law.

**Grade:** `proved`. In natural coordinate `log(theta)`, curvature is minus
sample size times a strictly positive variance. The mean increases from
one to infinity. Equivalently, with `u = -log(1-theta)`,
`expm1(u)/u` increases strictly from one to infinity. Mean matching has
exactly one root for a sample mean above one. At an all-one sample the
optimum is the excluded limit `theta -> 0`.

**Status changes:** None.

**Field reconciliation:** The existing prose and fitter already agreed on
the all-one non-existence case. The dossier has no corresponding `limits`
record, although its fitter has a point-mass status. The protected
`fit.note` also contains unrelated generic wording about Zipf endpoints
and modified zero mass. Neither field structure was changed.

**Open:** Adding the missing point-mass limit record and removing the
unrelated catalog wording are metadata follow-ups. There is no remaining
mathematical existence or uniqueness question for this one-parameter model.

## Scope and validation

No support, coefficient domains, held sets, statuses, fitting arguments,
examples, relations, inference claims, limit grades/flags, algorithm code,
bindings, tests or goldens changed. A parsed-YAML comparison against HEAD,
removing only `existence`, limit `when`/`detect`, and coefficient `advice`,
confirmed equality of all remaining fields for each family. The revised
advice strings in `hyper` and `nhyper` do not occur verbatim in their common
binding headers. No matching hold/advice error string or test regex needed
an update. There are no structural hold or fitting-domain restriction
reasons to rewrite in these five dossiers.

No external searches or new bibliographic references were needed: the
arguments are given directly and linked to retained repository analyses.
Inherited bibliographies were left unchanged.

All computational checks ran locally through `/tmp/umvl-cpu/run` or `run4`,
under the wrappers' `nice -n 10`, one BLAS/OpenMP thread per worker, and at
most four workers. No simbox runs were used. Temporary directories were
`build/tmp`; scratch scripts and logs are in ignored `build/scratch`.
Environment: GNU C++ 15.2.0, R 4.6.1 (2026-06-24), testthat 3.3.2,
roxygen2 8.0.0. Existing test seeds and numerical tolerances were unchanged.
The source arguments establish mathematical claims; regression tests and
relations checks do not independently prove those claims.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed. |
| Requested targeted `testthat::test_local` filter, with `UMVL_DISTRIBUTIONS_DIR`, dev tier and four workers | 9,299 pass; 0 fail; 0 warn; 5 full-tier skips; 153.5 seconds of test runtime. |
| `tools::checkRd` on all generated `man/ml*.Rd`, excluding Non-ASCII messages | Passed; zero output, including all five families. |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | Passed; derived 99 families; 1.51 seconds of generation runtime. |
| `/tmp/umvl-cpu/run just relations` | 157 edges; 0 failed. |
| `/tmp/umvl-cpu/run4 just cpp-test` | 106/106 dev shards passed; 0 failed; 51.55 seconds of test runtime. |
| Final `/tmp/umvl-cpu/run4 just test` | 20,456 pass; 0 fail; 0 warn; 152 full-tier skips; 124.086 seconds elapsed for the R test command. |
| Control-character grep on the five dossiers | No matches (the expected grep exit status is 1). |
| Display-math delimiter scan | No unsupported backslash-bracket delimiters. |
| `git diff --check` | Passed. |
| Parsed protected-field comparison | Passed for all five dossiers. |

The targeted command was:

```sh
cd univariateML-r
/tmp/umvl-cpu/run4 env TESTTHAT_CPUS=4 UMVL_TEST_TIER=dev \
  UMVL_DISTRIBUTIONS_DIR="$PWD/../univariateML-cpp/include/cppunivariate/distributions" \
  Rscript -e "testthat::test_local('.', filter = 'dist-(cmb|cmp|hyper|nhyper|lgser)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
```

The final dev counts above are the observed result, rather than the older
baseline counts in the brief. All skips explicitly require the full test
tier. The full tier was not required because no fitting, probability
evaluation or test infrastructure changed. Long shared-slot waits were
allowed to finish without interruption; reported runtimes exclude those
queue waits. Every started validation/environment command has finished.

Dossier SHA-256 values identify the uncommitted text tested:

| Family | SHA-256 |
|---|---|
| `cmb` | `74dd24853c2d5771e33248dfce1807586bb487cb2c4d52acc07634df76cb6475` |
| `cmp` | `c493c9c4e0594480c410d6a0933b05d14f2dc2acc85dc145e0921f688cde89f5` |
| `hyper` | `3cf6df8968c1d2c211e85ad52545c7b4cb9d484b4cd01e109e90d3cd1553f198` |
| `nhyper` | `1874a8bc17ef3c7cb4009cf92294513e35fdd582dd52c9839855792a9c4b9d3a` |
| `lgser` | `049f0f9d7c1de0c42c6235991f7874bebb17d6f094b0d9953ece35eb04e6cd72` |

Artifact display: the requested `show` command was attempted, but the viewer returned HTTP 403 because this worktree is outside its configured readable roots. The report remains at the path above; no viewer configuration or file outside the worktree was changed.
