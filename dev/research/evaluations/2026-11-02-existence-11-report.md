# Existence-11: modified-count existence statements and prose

Completed 2026-11-02 for TASK-101.6.4.1. Tested source: commit
2dad9da3f2fe9e538e8acfa231cca83d77412ca5 plus the six dossier edits in
this worktree. This report was written after all runs finished. No commit,
board edit, installation, push or publication was performed.

The six dossiers now distinguish mathematical attainment, uniqueness,
unidentified boundary coefficients and numerical search limitations. R-only
instructions and warnings are under R notes. ZIB's dated search evidence is
under Decision history. No new external theorem citation was introduced;
the arguments below are given directly or linked to existing local analyses.

## Family results

### zip

**Statement:** The free fit always attains a maximum, unique with positive
counts; held positive rate gives unique inflation, while held inflation below
one gives a finite rate maximum exactly when a positive count occurs.

**Grade:** Proved for these existence results and free-fit/held-rate
uniqueness; unknown for uniqueness at arbitrary held inflation. The dossier
therefore uses uniqueness: unknown.

At held rate, inflation is the clipped empirical-zero adjustment. On the
positive-inflation branch, the rate solves
$\lambda/(1-e^{-\lambda})=\bar x/(1-z)$, where $z$ is the zero fraction.
The left side is strictly increasing. The other branch is the ordinary
Poisson maximum. At held inflation below one, any positive observation makes
likelihood vanish at both rate endpoints, ensuring a finite maximum. With
only zeros the supremum instead requires the excluded zero rate. Unit
inflation leaves rate unidentified. Holding all coefficients evaluates
likelihood subject to compatibility.

**Status changes:** None. **Field disagreement:** None. The zero-rate
exclusion is the declared parameter-domain contract, not a claim that a
point mass at zero cannot be defined mathematically.

**Open:** A global stationary-root certificate and a general uniqueness
classification for held inflation. The existing grid compares separated
rate peaks without excluding every missed peak.

### zinb

**Statement:** All-zero samples attain nonunique maxima; positive samples
require a finite-size versus ZIP-boundary comparison, while held finite
size or held probability below one removes the infinite-size escape.

**Grade:** Argued for the global profile characterization and boundary
compactification; unknown for general uniqueness. The dossier uses
uniqueness: unknown.

Let $Q(r)$ be the globally profiled finite-size log likelihood and
$Q_\infty$ the corresponding maximized ZIP boundary, with inflation held
when it is held in the original fit. A finite point with
$Q(r)\ge Q_\infty$ ensures attainment; otherwise only the boundary attains
the supremum. Equality at a finite point counts as attainment. At held
finite positive size, the probability endpoints either give an attained
zero law or lose positive-count likelihood. At held interior probability,
positive-count likelihood vanishes at infinite size. Size zero is attained.
These arguments cover further holds and compatible endpoint cases but do
not prove uniqueness.

**Status changes:** None. **Field disagreement:** The proved limit paths
are consistent with the domains. The former detector wording could be read
as a mathematical existence test; it now explicitly describes the numerical
comparison and its limitation.

**Open:** Global size-profile and held-inflation probability-profile
certification, and uniqueness beyond identified conditional cases.
The implemented $10^{-8}n$ boundary allowance can reject a finite maximum
nearly tied with ZIP or Poisson. It also cannot exclude an unsampled peak.

### zib

**Statement:** All-zero samples attain a nonunique maximum; a held feasible
integer size guarantees attainment, and a held interior probability gives
a finite integer maximum, but the free-size/free-probability comparison with
ZIP is not globally certified.

**Grade:** Proved for fixed-size attainment, the size-one nonidentifiability
and held-interior-probability attainment; unknown for general uniqueness
and global classification by the implemented size search. The dossier uses
uniqueness: unknown.

At fixed size the probability square is compact and the likelihood is
continuous. Size one identifies only $(1-\pi)p$. With probability held in
$(0,1)$ and a positive count, every observed positive-count probability
vanishes at infinite size, forcing finite attainment. With both
probabilities held and all observations zero, size zero attains likelihood
one. The integer profile with probability free still needs a tail bound
covering both its positive-binomial and ordinary-binomial branches; see the
[size-search investigation](2026-10-31-size-search-report.md).

**Status changes:** None. **Field disagreement:** No domain or limit-status
contradiction was found. A concrete README/implementation disagreement was
found in the all-zero held-probability case, described below. The mathematical
statement records the actual maximum, and R notes give the workaround.

**Open:** Fix that boundary branch and add its regression; certify the
free-size probability profiles, including held inflation; settle uniqueness
where identifiable. Neither grid stabilization nor a ZIP likelihood within
$10^{-8}n$ certifies the global comparison.

### zmpois

**Statement:** Every nonempty sample has an attained maximum, unique when
positives occur; all-zero data leave rate unidentified, and compatible holds
preserve attainment.

**Grade:** Proved, with the stated identifiability exceptions. The dossier
uses uniqueness: proved.

The zero-indicator Bernoulli likelihood separates from the positive
zero-truncated Poisson likelihood. The first uniquely estimates the zero
fraction. The positive conditional mean increases strictly with log rate,
with derivative equal to conditional variance, from one to infinity.
Its unique mean match includes rate zero for positive observations all
equal to one. Without positives, rate is absent from the likelihood.
Held zero rate excludes counts above one; zero-probability endpoint holds
must match the observed zero indicators.

**Status changes:** None. **Field disagreement:** None.

**Open:** No mathematical existence or uniqueness question remains within
this stated sample and hold contract. These arguments are not a general
floating-point error bound for extreme inputs.

### zmgeom

**Statement:** Every nonempty sample has an attained maximum, unique with
positives at their reciprocal mean and the empirical zero fraction;
all-zero data leave the base probability unidentified.

**Grade:** Proved, including compatible held subsets. The dossier uses
uniqueness: proved.

The likelihood factors into independent zero-indicator and positive
geometric factors. For $k>0$ positives, the latter is
$p^k(1-p)^{\sum_{x_i>0}(x_i-1)}$, uniquely maximized at their reciprocal
mean. The endpoint $p=1$ is attained when all positives equal one.
Endpoint holds are checked for support compatibility. With no positives,
the base probability is unidentified.

**Status changes:** None. **Field disagreement:** None.

**Open:** No mathematical existence or uniqueness question remains within
the stated contract. Numerical identity tests do not establish accuracy on
every possible machine input.

### zmnbinom

**Statement:** Zero-only and zero/one samples attain maxima with unidentified
base coordinates; other free fits depend on the zero-modified Poisson
boundary, while held finite size or interior probability ensures attainment.

**Grade:** Proved for likelihood separation, fixed-size probability
uniqueness with positives and the attained logarithmic member; argued for
the global profile compactification; unknown for general uniqueness.
The dossier uses uniqueness: unknown.

The zero-indicator factor separates. At each fixed size the positive
conditional mean is strictly monotone, determining a unique probability
when positives occur. Size zero is the continuous logarithmic-series
member. At held interior probability the size tail has vanishing likelihood.
With size and probability free, the positive profile compares finite
$r\ge0$, including the logarithmic member, with the zero-truncated Poisson
boundary. Holding only the zero probability leaves this problem unchanged.
The [count-family analysis](2026-10-29-p0-counts.md) gives the profile
characterization.

**Status changes:** None. **Field disagreement:** None in the declared
domains or limiting paths. The detector is now described as a numerical
comparison rather than a proof of non-existence.

**Open:** A global size-profile certificate, including the held-probability
search, and general uniqueness. The finite grid and local refinement do
not exclude missed peaks. The $10^{-8}n$ boundary allowance can reject
nearly tied finite maxima.

## Confirmed ZIB boundary defect

The free-size all-zero branch in zib/mle.hpp selects size one whenever
probability is held. If inflation is also held, that choice can be a
non-maximizer. This violates the R6 contract independently of the general
size-search certification gap. It is data-dependent, so adding a structural
holds exception would misdescribe it.

Reproducer from the repository root after regeneration:

    pkgload::load_all("univariateML-r", quiet = TRUE)
    x <- c(0, 0, 0)
    f <- mlzib(x, size = NULL, prob = 0.5, pi = 0.2)
    print(f)
    attr(f, "logLik")
    sum(univariateML:::dzib(x, size = 0, prob = 0.5, pi = 0.2, log = TRUE))

Observed: size one, probability 0.5, inflation 0.2, and log likelihood
$3\log(0.6)=-1.5324768713$. The permitted size-zero member has log
likelihood zero and preserves both held probabilities. This is also a
direct mathematical counterexample: all zero masses equal one at size zero.

The missing implementation change is to include/select size zero in this
free-size all-zero held-probability branch, with regression coverage for
held inflation and probability endpoints. Fitter and new-test edits are
explicitly excluded from this documentation job. No fitter status was
changed. The workaround is to supply size = 0 explicitly, now stated in
the dossier's R notes. The existing green suites do not cover this defect.

## Scope and validation

Only the six permitted README files and this report changed. A parsed-YAML
comparison against HEAD verified that every other machine-readable field
is identical after removing existence and limit when/detect prose. In
particular, status values, held sets, domains, fit metadata, relations,
inference metadata, examples, firmness and numerical flags are unchanged.
There were no holds reasons or parameter advice fields requiring message
updates, so common binding messages and R regexes remain unchanged.
No density, fitter, truth fixture, golden or generator was edited.

All runs used the local Linux x86_64 workstation, never simbox. Environment:
R 4.6.1, GNU C++ 15.2.0, BH 1.90.0-1, C++17. The slot launchers supplied
nice level 10 and one BLAS/OpenMP thread; four-slot commands used the
configured four-worker build/test budget. TMPDIR, JUST_TEMPDIR and
XDG_RUNTIME_DIR were set to this worktree's build/tmp. Scratch logs remain
under build/scratch and are not durable evidence; commands and the
counterexample are retained here.

| Check | Result |
|---|---|
| /tmp/umvl-cpu/run4 just regen | Passed. Existing generated-help link-resolution diagnostics remain. |
| Parsed-YAML invariant audit against HEAD | Passed for all six families. |
| Requested targeted testthat filter | 8,114 passes; 0 failures; 0 warnings; 16 tier-related skips. |
| tools::checkRd over all generated ml help, excluding Non-ASCII diagnostics | No diagnostics for these six families. Untouched mlskellam.Rd and mlst.Rd still report malformed-math/section diagnostics. |
| ZIB deterministic boundary probe | Confirmed the defect above; not counted as a passing optimality test. |
| python3 site/scripts/data.py | Passed; derived 99 families. |
| just relations | Passed all 157 records; 0 failed. |
| /tmp/umvl-cpu/run4 just cpp-test | Passed 106/106 development shards. Initial system-Boost configuration failed; the recipe's BH-header fallback configured and built successfully. |
| /tmp/umvl-cpu/run4 just test | Passed: FAIL 0, WARN 0, SKIP 151, PASS 20121. |
| git diff --check | Passed. |

The targeted invocation, from univariateML-r, was:

    /tmp/umvl-cpu/run4 env \
      UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions \
      Rscript -e "testthat::test_local('.', filter = 'dist-(zip|zinb|zib|zmpois|zmgeom|zmnbinom)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"

The help check used the requested loop over man/ml*.Rd with Non-ASCII
diagnostics removed. No external diagnostic was repaired because other
families and generators are outside this job's ownership.

No full-tier run was required: this change touches prose and existence
metadata, not fitting, d/p/q/r or test infrastructure. The skipped slow
oracles and exhaustive full-tier cases are missing coverage in this run,
not passes. Regression, numerical relations and truth comparisons do not
prove the global existence or uniqueness claims; those rest on the
arguments and explicit qualifications above.

## Report display

The requested artifact-pane display was attempted with the standard show
command. The viewer returned HTTP 403 because this worktree is outside its
configured readable roots. The report remains at its requested worktree
path; no file was copied outside the worktree and no viewer configuration
was changed.
