# Existence-12: six zero-modified and zero-truncated count families

Completed 2026-11-02 for TASK-101.6.4.1 in `codex/existence-12`, based on source revision `0ea173e1fe57e179d8571ccc60dcec7ccfdd5ffb`, with the uncommitted dossier changes described here. No commits, board edits, installations, other-worktree edits or simbox runs were made.

The six dossiers now have an `existence` statement and argument, reader-facing coefficient and held-case explanations, and separate R notes. The binomial search history remains accessible under Decision history. No fitting code, defaults, fixtures, generated d/p/q/r sources or protected metadata changed. There were no existing `holds`, `fit_domain` or parameter `advice` entries in these six dossiers to rewrite, so no binding message or test regex needed synchronization. A parsed comparison against HEAD confirmed that every existing front-matter value except limit `when` and `detect` is unchanged.

## Family results

### zmbinom

**Statement:** The zero fraction profiles independently; positive counts have the zero-truncated binomial existence problem, with unidentified base coefficients on all-zero data and attained, nonunique base coefficients when positives are all one.

**Grade:** unknown for joint free-size existence and uniqueness. Proved conditional results: at held trial count greater than one, strict concavity in logit probability gives a unique positive-count probability fit; at held interior probability, each observed positive-count mass tends to zero at infinite size, so an integer maximum is attained. The Bernoulli zero-indicator likelihood gives the unique empirical zero fraction. Holding the zero probability leaves the conditional problem unchanged. The dossier covers boundary probabilities, trial count one and all-held evaluation.

**Status changes:** none. **Disagreements:** the former limit text described a comparison against a maximized finite likelihood although the grid is not globally certified. It also described the tolerance using total sample size. Detection text now says best searched value and uses the number of positives, as the delegated fitter does. It excludes the all-one positive sample, which bypasses that comparison. The unchanged `firmness: proved` supports the limiting path, not a certificate of the numerical diagnostic.

**Open:** a general free-size criterion, joint uniqueness and a complete integer-profile/tail certificate, inherited from ztbinom.

### zmlogarithmic

**Statement:** Every nonempty nonnegative integer sample attains a maximum; the zero fraction is unique, and the positive-count probability is unique when positives are present, including probability zero for positive counts all equal to one.

**Grade:** proved, with the explicit exception that no positives leave the base probability unidentified. The likelihood factors into Bernoulli and logarithmic-series parts. In log probability, the latter has curvature minus sample size times conditional variance. Its mean increases from one to infinity, giving exactly one score root above mean one and an attained endpoint at mean one. Compatible holds preserve this factorization.

**Status changes:** none. **Disagreements:** the protected `fit.method: multistart` and its note about a log-shape score and a finite Zipf endpoint do not describe this Bernoulli/one-dimensional logarithmic-series fit. These catalog fields were left unchanged under the ownership restriction.

**Open:** no mathematical existence or uniqueness gap in the stated compatible cases. Extreme floating-point root accuracy is separate from the exact-arithmetic argument and was not newly certified here.

### ztpois

**Statement:** Every nonempty positive integer sample has a unique rate MLE; it is zero for all-one samples and otherwise uniquely matches the conditional mean.

**Grade:** proved. In log rate the curvature is minus sample size times the strictly positive conditional variance. The conditional mean increases from one to infinity. The zero-rate member attains the all-one maximum. A held rate only evaluates likelihood and must be compatible.

**Status changes:** none. **Disagreements:** none between the existence statement, parameter domain and attained boundary. The inherited fit note mentions separation of zero mass although this family has no zero-mass coefficient; no protected catalog field was edited.

**Open:** no mathematical existence or uniqueness gap in the stated cases. Fixed-count bisection remains floating-point computation, not an outward-rounded certificate.

### ztgeom

**Statement:** Every nonempty positive integer sample has the unique estimate equal to the reciprocal sample mean, including probability one for all-one data.

**Grade:** proved. The log likelihood is `m log(p) + (T-m) log(1-p)`, where `m` is sample size and `T` the count total. Its strictly concave interior maximum is `m/T`; when `T=m` it increases to the attained endpoint. Holding probability leaves only compatible likelihood evaluation.

**Status changes:** none. **Disagreements:** protected `fit.method: scalar-solve` describes a fit that is actually closed form. The generic fit note's reference to degenerate samples stopping must not be read as excluding the valid all-one endpoint. The prose now states its attainment explicitly.

**Open:** none for mathematical existence or uniqueness in the stated cases.

### ztnbinom

**Statement:** Joint free-size existence and uniqueness remain unresolved here beyond the declared Poisson escape and attained logarithmic and point-mass members; fixed size gives a unique probability fit, while fixed interior probability guarantees finite size attainment without a uniqueness claim.

**Grade:** unknown jointly; proved for the stated conditional attainment results. For fixed positive size, the natural parameter is log failure probability and curvature is minus sample size times conditional variance. The size-zero logarithmic member obeys the same argument. With interior probability held, likelihood extends continuously to size zero and vanishes at infinity, so a maximum is attained. Probability one with all-one data leaves free size unidentified.

**Status changes:** none. **Disagreements:** the former limit detection wording overstated a numerical grid comparison as a comparison with a maximized finite likelihood. The new wording identifies the searched value and tolerance. The unchanged proved limit grade does not certify that search. The dossier now explains the fixed log-size grid from -24 to 24 and the fact that increasing the iteration budget does not enlarge it.

**Open:** joint existence criterion, joint uniqueness and global certification of both the joint and held-probability searches. No fitter change was authorized. In particular, the existing grid can return successful values without excluding every unvisited peak; this remains a concrete numerical-validation gap.

### ztbinom

**Statement:** Joint free-size existence and uniqueness are not established generally; fixed trial count greater than one gives a unique probability estimate, and fixed interior probability gives a finite integer maximum without a uniqueness assertion.

**Grade:** unknown jointly; proved conditional results. Strict concavity in logit probability yields conditional-mean matching or an attained endpoint. At fixed interior probability the observed masses vanish at infinite trial count, ensuring a finite integer maximum. Trial count one leaves probability unidentified. Held probability zero permits only ones and leaves trial count unidentified; held probability one requires constant counts and determines trial count.

**Status changes:** none. **Disagreements:** detection now describes the best searched value rather than a certified maximized profile. The proved limit path and numerical detection are explicitly distinguished. The free-size grid's stopping rule is not a global certificate, consistent with the protected partial-certification metadata.

**Open:** a general free-size existence criterion, integer-profile uniqueness/unimodality and a global search certificate. The continuous-profile literature observation does not resolve the integer question.

## References and scope of verification

The self-contained conditional arguments above do not depend on external uniqueness citations. Two searches verified the identities of the historical references retained in ztbinom's Decision history:

- DeRiggi, D. F. (1983). “Unimodality of Likelihood Functions for the Binomial Distribution.” *Journal of the American Statistical Association*, 78(381), 181–183. [Publisher record](https://www.tandfonline.com/doi/abs/10.1080/01621459.1983.10477949). This concerns the untruncated model.
- DeRiggi, D. (1994). “Sufficient conditions for unimodality of the positive binomial likelihood function.” *Statistics & Probability Letters*, 19(1), 1–4. [DOI](https://doi.org/10.1016/0167-7152(94)90061-2), [bibliographic record](https://explore.openaire.eu/search/publication?pid=10.1016%2F0167-7152%2894%2990061-2). The counterexample discussion remains attributed to the existing size-search investigation. The publisher full text returned HTTP 403. Neither its full proof nor the correction was independently checked in this job, and neither is used to claim an integer-size result.

## Checks

All builds and tests ran locally through the shared slot locks, under `nice -n 10`, with BLAS/OpenMP threads pinned to one and at most four build/test workers. Temporary files and logs are under `build/tmp` and `build/scratch`. Environment: Linux x86_64, R 4.6.1 (2026-06-24), GCC 15.2.0, Python 3.14.5. No simulations or new random workloads were introduced.

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Exit 0. Repeated in the final validation batch and by `just test`. |
| Requested targeted `testthat::test_local` filter | FAIL 0, WARN 0, SKIP 15, PASS 7757. The 15 skips require the full tier. |
| All `ml*.Rd` through `tools::checkRd`, filtering Non-ASCII diagnostics | No diagnostics for these six families. Existing diagnostics were printed for `mlskellam.Rd` and `mlst.Rd`. |
| `python3 site/scripts/data.py` | Exit 0; 99 families generated. |
| `just relations` | 157 edges, 0 failed. |
| `just cpp-test` | 106/106 development shards passed. |
| Final `just test` | FAIL 0, WARN 0, SKIP 151, PASS 20121; development tier, about 129 seconds after regeneration. |
| Control-character grep on the six dossiers | No output. |
| Parsed protected-metadata comparison against HEAD | Passed for all six. |
| `git diff --check` | Passed. |

The targeted invocation was initially attempted before the queued regeneration had assembled the package. It did not start a test run because the package directory was absent; it was rerun successfully after regeneration. No failing test result was suppressed.

The final sequential batch ran as `/tmp/umvl-cpu/run4 bash build/scratch/final-checks.sh`: regeneration, help checking, site data, relations, C++ development tests, then `just test`. It held slots only while computing and contained no nested slot calls or waits. A full-tier suite was not required for this prose-only change: fitting, d/p/q/r and test infrastructure were untouched. Roxygen regeneration exited successfully but retained two unresolved `ml` link diagnostics in generated documentation outside these six families.

### Open generator issue outside this job's scope

The R help limit template currently wraps `detect` in “When …, the maximum likelihood estimator does not exist” and appends `when` after “as …”. Full sentences required by the dossier schema therefore produce constructions such as “When With …” and doubled punctuation. More seriously, the template presents the numerical diagnostic as proof of non-existence. The R notes now explicitly qualify that diagnostic, but the template itself needs an authorized change in `bindings/r/scripts/`: render the two full sentences directly and distinguish a proved path from a numerically detected escape. The protected numerical flags were not changed. Passing `checkRd` establishes syntactic validity, not resolution of this semantic/template issue.

## Tested dossier fingerprints

SHA-256 identifies the exact uncommitted source used by the final validation:

```text
zmbinom       d18a20ceca884eec8b2842acaf0bfc72f42ad365969a8b695c8a82015369b339
zmlogarithmic 72e599330188edb2797f7413a838f2e731f644d58f225f69f2903fb2dadd557b
ztpois        4734ec1f4ab5d163bf24bce4b867e10f12b3985a112a300d5ec7ae5720a8c722
ztgeom        61b49e3758673401173ab0b4f606e3d340ce8861e3c652a8c91631d021eb5ddb
ztnbinom      591d478939df4f5584dc065afa5ad9af983aabb5c68320db8affebe20b16aa47
ztbinom       eaa37f693f0208283c3b632c363373419b216ad4a7170be632968f6900eaa1b2
```
