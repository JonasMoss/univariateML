# Existence statements and prose: existence-3

Completed 2026-10-31 for TASK-101.6.4.1 in `codex/existence-3`.
Tested source: `bfdf257a2ec2c44fb0b986d2ade376239e9c0d55` plus the five
README changes described here. No commits, package installations, board edits,
sub-agents or simbox runs were made.

## Scope and evidence

Added `existence: {uniqueness, statement, reason}` and revised the reader-facing
prose and limit descriptions for `lomax`, `invpareto`, `weibull`, `logis` and
`kumar`. Existence statements cover the joint fit before conditional fits.
Parameter meanings, held fits and numerical limitations are explained in
Details. R-specific behaviour is in R notes, and Weibull's historical start
and diagnostic notes are in Decision history.

**No statuses changed.** No held sets, parameter domains, fit bounds, fit
arguments, fit declarations, relations, examples, densities, fixtures or
catalog fields changed. A YAML comparison against HEAD, excluding only the
new existence fields and limit `when`/`detect` text, passed for every family.
These five families have no advice or structural hold reasons needing a
wording change, so no binding messages or test regexes changed. Limit text
uses complete grammatical clauses without terminal punctuation, as required
by the schema for insertion into generated help sentences.

The arguments below are mathematical evidence, independently of test results.
The [canonical behaviour note](../ml-behaviour.tex), especially its Lomax
and inverse-Pareto theorem and Kumaraswamy profile argument, supplies the
longer derivations. No new external references were added or relied on;
no web searches were used. Existing bibliographic fields were preserved.

## Lomax

**Statement:** A finite joint MLE exists iff the normalized profile criterion
$\psi(u)\le0$ holds somewhere in $0<u<\bar u$; either single held coefficient
gives a unique finite MLE on every nonempty positive sample.

**Grade:** Proved existence criterion and conditional uniqueness; unknown joint
uniqueness (`uniqueness: unknown`). In the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/lomax/README.md),
$y_i=x_i/\bar x$, $q=\lambda\bar x$ and
$A(q)=n^{-1}\sum_i\log(1+qy_i)$. Profiling gives $\hat\kappa=1/A(q)$
and gap $n\{\log q-\log A(q)-A(q)\}$ from the best exponential.
The gap tends to zero at zero and to minus infinity at infinity. At
$q=ue^u$ its sign is that of $-\psi(u)$, where
$\psi(u)=n^{-1}\sum_i\log(e^{-u}+uy_i)$.
For $u\ge\bar u=\bar x/(\prod_i x_i)^{1/n}$ it is strictly positive.
Continuity therefore gives the stated attainment criterion, including ties
with the exponential. The held-rate likelihood is strictly concave in shape;
the held-shape score in log-rate strictly decreases from $n$ to $-n\kappa$.

**Agreement and open items:** The limit status remains `ExponentialLimit`.
The numerical detection threshold is not the exact existence criterion:
a finite MLE tying or improving on the exponential by at most $10^{-8}n$
can be rejected. The dossier now explicitly distinguishes these statements.
Joint uniqueness and a global certificate for the existing finite-grid and
local-refinement search remain open. A proof of profile unimodality, or
certified bounds covering intervals between search points, would be needed.
The two-observation transition near ratio 28.81 is described as numerical;
the exact criterion handles equality rather than asserting an exact rounded
cutoff. The moment condition is retained as a local test only.

## Inverse Pareto

**Statement:** Joint existence is exactly Lomax existence for the reciprocals;
either held shape or held scale gives a unique finite conditional MLE,
including for constant positive data.

**Grade:** Proved existence reduction and conditional uniqueness; unknown joint
uniqueness (`uniqueness: unknown`). The
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/invpareto/README.md)
uses $z_i=1/x_i$, Lomax rate $\lambda=\theta$ and shape $\kappa=\alpha$.
The log-Jacobian $-2\sum_i\log x_i$ is independent of the coefficients,
so the reduction preserves all maxima and every held subset. The normalized
criterion and counterexample to the moment-based rejection rule are retained.

**Agreement and open items:** No status change; `ExponentialLimit` denotes the
inverse exponential escape here. The tolerance-versus-attainment distinction
and lack of a global search certificate are inherited from Lomax. Joint
uniqueness remains open for the same reason. No additional disagreement with
held cases was found.

## Weibull

**Statement:** Nonconstant positive data have a unique finite joint MLE;
constant data have unbounded likelihood. Held shape always gives a unique
finite scale, while held scale gives a unique finite shape unless every
observation equals that scale.

**Grade:** Proved existence and uniqueness (`uniqueness: proved`). The
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/weibull/README.md)
gives the profiled shape score in $z_i=\log x_i$ as
$1/k+\bar z-\sum_i z_i e^{kz_i}/\sum_i e^{kz_i}$.
Its derivative is $-1/k^2$ minus a weighted variance, and its endpoint
limits have opposite signs for nonconstant data. The held-scale score also
strictly decreases and crosses zero unless all log-ratios vanish.
Held shape gives $\hat\lambda=(n^{-1}\sum_i x_i^k)^{1/k}$.
At constant observations $c$, taking scale $c$ and $k\to\infty$ makes
the density there $k/(ec)$, proving unboundedness.

**Agreement and open items:** No status change. The existing `limits` record
covers only held-scale collapse; it omits the joint constant-data case.
The new existence statement and Details supply that missing reader-facing
fact. Adding a machine-readable limit record is outside this job's permitted
limit-text edits and remains open. The common binding's `Unbounded` error
also mentions a held scale when both coefficients are free; it needs separate
message-edit authorization. The mathematical cases themselves are settled.

## Logistic

**Statement:** Nonconstant real data have a unique finite joint MLE;
constant data permit scale collapse. Held location gives a unique scale unless
all observations equal it; held scale gives a unique location even for
constant data.

**Grade:** Proved existence and uniqueness (`uniqueness: proved`). In the
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/logis/README.md),
$a=\mu/s$ and $b=1/s$ give log-likelihood
$n\log b-2\sum_i\log\{2\cosh((bx_i-a)/2)\}$.
The Hessian is negative definite. For nonconstant data, minimizing
$\sum_i|bx_i-a|$ over $a$ still gives linear growth in $b$, which dominates
$n\log b$. The other boundary directions also drive the likelihood to
minus infinity. Thus the strictly concave likelihood attains a unique
maximum. Conditional fits restrict it to a line; the residual-zero case
is precisely held-location collapse.

**Agreement and open items:** No status change; the existing `InvalidInput`
collapse status is retained as the fitter's actual diagnostic contract.
The mathematical limit description is consistent with that case. The joint
constant-data binding error is generic (non-finite or invalid parameters)
rather than explaining unbounded likelihood. Improving it is outside the
permitted hold-reason message edits. No mathematical uniqueness question
remains for the cases stated.

## Kumaraswamy

**Statement:** Every nonconstant sample in $(0,1)$ has a finite joint MLE;
constant data have unbounded likelihood. Either held shape gives a unique
finite conditional MLE, including on constant interior data.

**Grade:** Proved existence and conditional uniqueness; unknown joint uniqueness
(`uniqueness: unknown`). The
[dossier](../../../univariateML-cpp/include/cppunivariate/distributions/kumar/README.md)
profiles $b=-n/S(a)$, with $t_i=-\log x_i$ and
$S(a)=\sum_i\log(1-e^{-at_i})$. The profile is
$-n\log|\log a|+O(1)$ at zero and
$a(n\min_i t_i-\sum_i t_i)+n\log a+O(1)$ at infinity.
For nonconstant data it tends to minus infinity at both ends and attains a
maximum. For constant data the linear term vanishes and it diverges upward.
The explicit path $a\to\infty$, $b=c^{-a}$ demonstrates collapse at $c$.
Held $a$ gives a strictly concave likelihood in $b$; held $b$ gives a strictly
decreasing shape score, including when $b<1$ by the inequality stated in the
existence reason.

**Agreement and open items:** No status change or contradiction among the
existing limit and held cases was found. Coercivity proves attainment, not
joint uniqueness or correctness of sign-based bisection of the joint profile.
Joint uniqueness and global certification of the Newton/profile fallback
remain open. A monotonicity or unimodality proof, or a search that bounds and
compares every possible maximum, is missing. No fitter was changed in this
prose-only task.

## Shared generated-help limitation

The generated Lomax and inverse-Pareto limit sentences say that the MLE
"does not exist" whenever the numerical detector triggers. That is too strong
at a tie, within its comparison tolerance, or if its search misses a better
maximum. The new existence text and R notes explicitly qualify it. The common
binding messages have the same overstatement. Correcting the shared sentence
generator and these non-hold error messages is outside scope; the exact
mathematical existence criterion, rather than the detector, is authoritative.

## Checks

Environment: workstation Linux `7.0.0-38-generic`, GNU C++ `15.2.0`,
R `4.6.1` (2026-06-24), BH `1.90.0.1`, Rcpp `1.1.2`, testthat `3.3.2`,
roxygen2 `8.0.0`, Python `3.14.5`. Commands used the shared CPU-slot locks,
which set BLAS/OpenMP to one thread and run commands under `nice -n 10`.
Four-slot commands used four build threads. Temporary files, logs and the
metadata comparison script are in `build/tmp` and `build/scratch`.
No fixtures or goldens were regenerated. Test seeds and tolerances were those
in the unchanged tests; no tolerances were relaxed. Test passes are regression
and finite numerical evidence, not proofs of the statements above.

Setup, from the worktree root:

```sh
export TMPDIR=$PWD/build/tmp JUST_TEMPDIR=$PWD/build/tmp XDG_RUNTIME_DIR=$PWD/build/tmp
mkdir -p "$TMPDIR" build/scratch
```

| Check | Result |
|---|---|
| `/tmp/umvl-cpu/run4 just regen` | Passed, including a final regeneration after prose review. An initial draft had an unterminated YAML quote in Kumaraswamy; it was corrected before the successful runs. |
| Targeted R command below | FAIL 0, WARN 0, SKIP 7, PASS 7732. Skips require the full tier. |
| Rd check below | No output for any owned family. The all-family scan printed one unrelated `ml_check_args.Rd:5` title-period warning, left unchanged. |
| `/tmp/umvl-cpu/run python3 site/scripts/data.py` | Passed; derived 98 families. |
| Protected-metadata comparison | Passed for all five families. |
| `/tmp/umvl-cpu/run4 just cpp-test` | 105/105 dev shards passed; zero failures. CTest elapsed 33.33 seconds. |
| `/tmp/umvl-cpu/run4 just test-full` | FAIL 0, WARN 0, SKIP 1, PASS 41289. The skip is optional `{sn}` not installed. R elapsed 164.820 seconds. |
| `/tmp/umvl-cpu/run4 just test` (last test run) | FAIL 0, WARN 0, SKIP 151, PASS 19843. Skips are tier exclusions and the optional comparison. R elapsed 68.868 seconds. |
| `git diff --check` | Passed. |

The counts above are those observed on this source revision, rather than the
separate master-baseline counts in the brief. CPU queue waits are excluded
from the reported test elapsed times.

Targeted command, from `univariateML-r/`:

```sh
/tmp/umvl-cpu/run4 env UMVL_DISTRIBUTIONS_DIR=$PWD/../univariateML-cpp/include/cppunivariate/distributions Rscript -e "testthat::test_local('.', filter = 'dist-(lomax|invpareto|weibull|logis|kumar)|conformance|restricted|generated-dpqr|parameter', stop_on_failure = TRUE)"
```

Help command, from `univariateML-r/`, under a single CPU slot:

```sh
/tmp/umvl-cpu/run Rscript -e 'for (f in Sys.glob("man/ml*.Rd")) { r <- tools::checkRd(f); r <- r[!grepl("Non-ASCII", r)]; if (length(r)) print(r) }'
```

Generated help was also read to check mathematical notation and sentence
joins. All commands had finished before this report was written. Final tracked
changes comprise only the five owned dossiers and this report.
