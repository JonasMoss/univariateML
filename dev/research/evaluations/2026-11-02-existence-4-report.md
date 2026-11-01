# Existence-4: existence statements and reader-facing prose

Completed TASK-101.6.4.1 for cauchy, hcauchy, laplace, dlaplace, gumbel and
invexp. Checks ran on the local workstation on 2026-11-01, Europe/Oslo.
The filename follows the requested job date.

The six dossiers now contain existence statements and checkable arguments,
including held-coefficient cases and nonunique maxima. All six use the
mathematical grade **proved**, with the exceptions stated explicitly.
This grade does not certify the numerical implementation.
No new citation is needed for these direct derivations; no web searches were used.

Deliverable changes are limited to the six README files and this report.
Temporary scripts and logs remain under ignored build/scratch. Support, parameter
domains and kinds, held sets, fit restrictions, statuses, limit firmness and
numerical flags, inference declarations, catalog fields, examples, fitting
code, density code, tests and goldens are unchanged. A final parsed comparison
against HEAD confirmed equality of every protected front-matter field after
removing existence, parameter advice, and limit when/detect text.

The half-Cauchy advice was rewritten as two full sentences. Neither the old
advice nor an identical copied message occurs in its binding-neutral fitter
or R test regexes, so those files needed no edits. Required-argument messages
are regenerated from the dossier.

## Family results

### cauchy

**Statement:** The joint MLE exists uniquely when every multiplicity is below
$n/2$; balanced two-point samples have a continuum of maxima, and all other
samples with a multiplicity at least $n/2$ have no finite MLE. Held location
gives a unique scale exactly below its half-mass threshold; held positive
scale always gives a location maximum, possibly nonunique.

**Grade: proved.** Collapse has order $\sigma^{n-2k}$. At equality, the
transformation $v=-1/(\mu+i\sigma-a)$ gives a global upper bound attained
only for balanced two-point data. Below the threshold, boundary decay gives
attainment. Joint uniqueness follows from strict convexity of the negative
log likelihood along every connecting vertical line or circle orthogonal to
the real axis. The dossier gives the parameterizations and second derivatives,
so the argument does not rely on a search or a concavity assertion in
ordinary location-scale coordinates. Held-location scale has a strictly
decreasing log-scale score. For held scale, the sample $(-a,a)$ with
$0<\sigma<a$ has the two location maxima
$\pm\sqrt{a^2-\sigma^2}$.

**Status changes:** none. The previous limit detection text was vague;
it now gives the exact free/held conditions and balanced exception.
The historical P0 report left the half-mass comparison qualified; the
explicit bound supplies the missing argument without changing the existing
status contract. A protected taxonomy discrepancy remains: fit.method says
profile-search although the all-free implementation uses safeguarded
two-coordinate Newton iteration. The held-scale branch searches location gaps.

**Open:** O1 and O2 below concern numerical fitting and that taxonomy tag.
There is no remaining mathematical existence or joint-uniqueness question
in the stated regimes. See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cauchy/README.md).

### hcauchy

**Statement:** With scale free, a unique finite MLE exists exactly when
$2k<n$, counting ties at the sample minimum for a free endpoint or at the
held endpoint otherwise; equality gives a finite unattained supremum and
$2k>n$ gives unbounded likelihood. Held positive scale gives a unique free
endpoint even for constant samples.

**Grade: proved.** Increasing the endpoint towards the sample minimum
increases the likelihood. Given that endpoint, the log-scale score decreases
strictly from $n-2k$ to $-n$ when some residual is positive.
Collapse order $\sigma^{n-2k}$ separates the two nonexistence cases.

**Status changes:** none. The previous prose claimed that even one
observation above the endpoint identifies a finite scale. That sentence was
too broad and contradicted the stated tie criterion: at held endpoint zero,
$(0,0,1)$ is unbounded and $(0,1)$ has a finite unattained supremum.
The rewrite states the criterion consistently. No current disagreement
remains among existence, limits, holds and fit restrictions.

**Open:** no family-specific mathematical question; O3 affects generated
limit prose. See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/hcauchy/README.md).

### laplace

**Statement:** Nonconstant samples have finite joint maxima at all sample
medians, with one common scale; the pair is unique exactly when the median
interval is a singleton. Held location gives a unique positive scale unless
all residuals vanish, and held positive scale permits constant data.

**Grade: proved.** Absolute-deviation minimization identifies exactly the
median interval. For positive total deviation $A$, scale maximization gives
$b=A/n$ uniquely and the profile decreases in $A$. Zero deviation gives
unbounded collapse. The fitter's upper-median selection is preserved.

**Status changes:** none. **Field disagreements:** none.
**Open:** no family-specific mathematical question; O3 affects generated
limit prose. See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/laplace/README.md).

### dlaplace

**Statement:** Nonconstant integer samples have finite maxima at every integer
median, with one common scale; constant samples have only a point-mass
supremum in the declared open scale domain. A held integer location gives an
interior scale unless all residuals vanish; held interior scale permits
constant samples.

**Grade: proved.** The coefficient of mean absolute deviation $d$ is
$\log p<0$, so integer medians maximize location. For $d>0$,
$d(1-p^2)=2p$ has the unique maximizing root
$p=d/(\sqrt{1+d^2}+1)$. For $d=0$, likelihood decreases strictly in $p$
and its supremum is approached only at excluded $p=0$.
The smallest-integer-median selection and its nonuniqueness warning are retained.

**Status changes:** none. **Field disagreements:** none.
**Open:** no family-specific mathematical question; O3 affects generated
limit prose. See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/dlaplace/README.md).

### gumbel

**Statement:** Nonconstant samples have a unique finite joint MLE; constant
samples are unbounded. Held location gives a unique scale unless every
observation equals it, and held positive scale always gives a unique location.

**Grade: proved.** After profiling location, the scale equation is
$F(\sigma)=\sigma+E_\sigma[x]-\bar x=0$ with weights proportional to
$e^{-x_i/\sigma}$. Its derivative is
$1+\operatorname{Var}_\sigma(x)/\sigma^2>0$, its zero-scale limit is
$\min x_i-\bar x<0$ for nonconstant data, and it tends to infinity.
The profile score has opposite sign to $F$. With location held, the
inverse-scale likelihood is strictly concave and its score crosses zero
unless all residuals vanish.

**Status changes:** none. **Field disagreements:** none.
**Open:** no family-specific mathematical question; O3 affects generated
limit prose. See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/gumbel/README.md).

### invexp

**Statement:** Every nonempty finite positive sample, including a constant
sample, has a unique finite scale MLE, its harmonic mean; holding scale
leaves likelihood evaluation.

**Grade: proved.** The scale-dependent likelihood is
$n\log\theta-\theta\sum_i x_i^{-1}$, strictly concave with unique stationary
point $n/\sum_i x_i^{-1}$. Reciprocating observations changes the likelihood
only by a parameter-independent Jacobian.

**Status changes:** none. **Field disagreements:** none.
**Open:** none for the stated mathematical claim. Numerical inability to
represent a reciprocal remains a numerical failure, not nonexistence.
See the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invexp/README.md).

## Open numerical and generation issues

### O1: confirmed Cauchy held-location bracket failure

This is an existing implementation failure, outside the permitted mle.hpp
edits. It is documented in the reader-facing dossier and reproduced here,
rather than being treated as a mathematical nonexistence case.

For data $(0,10^{-300},1)$ and held location zero, $k=1$ and $n=3$,
so the scale MLE exists uniquely. The current lower bracket is effectively
$\log(\text{radius})-350=-350$. Its endpoints do not bracket the scale root,
but the iteration still returns the lower edge as a successful fit.

| Quantity | Observed value |
|---|---:|
| Returned scale | 9.9295903962683649e-153 |
| Returned log likelihood | 346.56581034245141 |
| Explicit candidate scale | 1e-300 |
| Independently evaluated candidate log likelihood | 686.64819106010555 |
| Candidate improvement | 340.08238071765413 |

The candidate is not asserted to be an exact numerical root. Its much
higher likelihood already disproves maximality of the returned fit.
The comparison uses stable log arithmetic and exceeds an absolute
$10^{-6}$ comparison margin by more than eight orders of magnitude.
It is a numerical counterexample, not an interval-arithmetic certificate.
There is no random seed.

Reproduce after building the local package:

~~~r
pkgload::load_all("univariateML-r", compile = FALSE, quiet = TRUE)
x <- c(0, 1e-300, 1)
fit <- mlcauchy(x, location = 0)
candidate_ll <- function(scale) {
  r <- abs(x)
  h <- pmax(r, scale)
  sum(-log(pi) + log(scale) - 2*log(h) -
      log1p((pmin(r, scale)/h)^2))
}
unname(fit["scale"])
attr(fit, "logLik")
candidate_ll(1e-300)
candidate_ll(1e-300) - attr(fit, "logLik")
~~~

**Missing:** adaptive lower-bracket expansion, verified opposing score signs,
and numerical-failure handling when a usable bracket cannot be established.
Increasing the iteration budget alone does not repair a root outside the
bracket. A regression case must exercise disparate residual magnitudes:
the existing uniform-rescaling checks preserve their ratios and miss this
failure. No numerical fix, new restriction, status change or golden change
was made in this prose job.

### O2: remaining Cauchy certification and method tag

Mathematical joint uniqueness does not certify the current Newton stopping
rule. The conditional location grid has no root-isolation certificate.
Validated termination checks and a complete conditional comparison, or a
numerical-failure result when unresolved, remain needed. The protected
profile-search method tag also needs reconciliation with the primary
all-free Newton route. The existing partial certification flag was retained.

### O3: generated R limit sentences still expect clauses

The required full-sentence when/detect prose exposes a template mismatch in
[limit_sentence()](../../../bindings/r/scripts/codegen_wrappers.R).
Lines 52–77 insert text into “When … as …” and add punctuation.
Generated help can consequently contain “as The scale …” and doubled
periods. Plain detect sentences without dollar-delimited math are formatted
as verbatim code. The generator needs to accept full sentences and render
plain detect prose as prose. That script was explicitly outside this job's
ownership and was not edited. These are presentation issues; the six owned
Rd files pass the requested syntax checks.

## Validation

Tested source: HEAD **6fea876ebeec88594a6de4b8af0db4adb5a7d452**, branch
codex/existence-4, plus the six uncommitted dossier edits identified below.
All executable source remains at that revision. No commits were made.

All runs used the **local Linux workstation**, never simbox. CPU work used
the prescribed run/run4 locks, nice priority 10, and one BLAS/OpenMP thread
per worker. The R dev suite used four workers and the C++ build/test used
at most four slots. Environment: R 4.6.1, testthat 3.3.2, GNU C++ 15.2.0.
Existing suite seeds, tolerances and goldens were unchanged.

| Check | Result |
|---|---|
| run4 just regen | Passed; final just test also regenerated help from the final dossier text. |
| Requested targeted R filter | FAIL 0, WARN 0, SKIP 6, PASS 7936. All six skips require the full tier. |
| Final six-family Rd assertion, excluding Non-ASCII diagnostics | Passed with no output and zero diagnostics. |
| Requested all-family Rd scan | Exit 0; 26 diagnostics, all for untouched mlst.Rd; none for the owned six. |
| python3 site/scripts/data.py, repeated on final text | Passed; derived 99 families. Final generation took 1.52 seconds. |
| just relations | 157 edges; 0 failed. |
| run4 just cpp-test | 106/106 dev shards passed; test time 75.41 seconds. |
| Final run4 just test | FAIL 0, WARN 0, SKIP 151, PASS 20102; elapsed 128.355 seconds. |
| Final protected-field comparison | All six match HEAD after removing only the allowed text fields and existence. |
| git diff --check | Passed. |
| Deterministic Cauchy diagnostic probe | Exposed O1; it is not evidence of fitter correctness. |

The all-family Rd diagnostics concern raw mathematics and an empty section
in st. They remain outside this job's family ownership. CMake also emitted
the nonfatal CMP0167/FindBoost developer policy notice during configuration.
Neither notice represents a failing owned-family test.

The full R tier was not run: fitting code, density code and test
infrastructure were not changed. The skips above are retained missing
coverage, not passes. Regression checks support unchanged behavior on their
workloads; the mathematical arguments independently support the existence
claims. O1 demonstrates why these claims do not certify every numerical fit.

Reproducible commands, from the worktree root:

~~~sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp
export XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR"
/tmp/umvl-cpu/run4 just regen

(cd univariateML-r &&
 /tmp/umvl-cpu/run4 env \
 UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions \
 Rscript -e "testthat::test_local('.', filter = 'dist-(cauchy|hcauchy|laplace|dlaplace|gumbel|invexp)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)")

(cd univariateML-r &&
 /tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }')

/tmp/umvl-cpu/run python3 site/scripts/data.py
/tmp/umvl-cpu/run just relations
/tmp/umvl-cpu/run4 just cpp-test
/tmp/umvl-cpu/run4 just test

(cd univariateML-r &&
 /tmp/umvl-cpu/run Rscript -e 'for (id in c("cauchy","hcauchy","laplace","dlaplace","gumbel","invexp")) { r <- tools::checkRd(paste0("man/ml", id, ".Rd")); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) stop(paste(r, collapse="\n")) }')
/tmp/umvl-cpu/run python3 site/scripts/data.py
~~~

Final dossier SHA-256 values:

~~~text
cauchy   3f24e02de14af3b867d5dfc6208398922e6ea475e4afc872ade689d156d68079
hcauchy  bd0cd96ad8b1aa00f514f7a8ded86e014758a2781ea08c6ee4f696db0f28e9e6
laplace  3e9a41fea3f3070485c83febb4c63b135c38db34f6af0d95beea25bb11240edc
dlaplace 22780e0920d96f1bb7eaa2466999618c7b27d0b19803e5f65ec783b21e35dbeb
gumbel   a7ab153679f52695b83ccd3799309c21e71eadcd8eb26c01694a03879cff9161
invexp   e65d9c147fdf24f2f1d59eaa3ec75e8f049be81cbddaf6a91326771dad872202
~~~

## Artifact delivery

The prescribed show command was attempted after writing the report. The viewer
returned HTTP 403 because this worktree is absent from its configured readable
roots. The report remains at the requested path in this worktree. No viewer
configuration or other checkout was changed.
