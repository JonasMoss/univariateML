# TASK-93 p2b-binom: free integer size

Implemented five of the eleven subsets. Six remain `not-implemented` with
specific reasons, as permitted by item 2. No default fit or golden changed.
No commits, board edits, installations, sub-agents, or other worktrees used.

## Algorithms and guarantees

| Family; held coordinates | Result | Algorithm / guarantee |
|---|---|---|
| tbinom; `{}` | implemented | Numerical integer profile; estimated bounds are sample extremes. |
| tbinom; `{a}` | implemented | Same, lower bound held exactly. |
| tbinom; `{b}` | implemented | Same, upper bound held exactly. |
| tbinom; `{a,b}` | implemented | Same, both bounds held exactly. |
| cmb; `{prob,nu}` | implemented | Enumerate integers with an analytic upper bound on the entire remaining tail. |
| cmb; `{}` | retained | P0b's limiting comparison omits CMP and geometric laws; joint limiting paths unresolved. |
| cmb; `{prob}` | retained | Constrained free-dispersion limiting paths unresolved; the empirical two-point entropy is not the held-probability face. |
| cmb; `{nu}` | retained | Positive held dispersion admits a CMP face; P0b's proposed two-point comparison is incorrect. |
| bbinom; `{}` | retained | Fixed-size binomial/two-point escapes and the NB face need a joint comparison across sizes; no reliable tail stopping rule established. |
| bbinom; `{alpha}` | retained | The NB face has held size alpha, but ruling out later integer-profile improvements remains unresolved. |
| bbinom; `{beta}` | retained | Reflection changes the data with candidate size; the P0b fixed-alpha NB argument does not transfer to this subset. |

The tbinom fixed-size conditional-mean root is unique (positive conditional
variance). Its new free-size path uses eta = log(N*p/(1-p)) and the kernel
`x*eta - lgamma(x+1) + sum(j=1..x-1) log1p(-j/N)`. Mode-centred recurrence
normalizes it without subtracting terms of order N*log(N). Setting 1/N=0
calculates the exact truncated-Poisson face in the same coordinates.
The total free-coordinate score must be below 1e-7 before returning a solve.
Existing fixed-size and held-probability paths remain unchanged.

Scan 128 consecutive sizes, then expand by factor 1.5; refine every sampled
peak using integer ternary subdivision and final enumeration. Stop after
four expansions within 1e-9*n of the exact face. If the best finite value
does not exceed the face by that tolerance, stop with `Use mltpois`.
The 512-expansion budget and recurrence term budget produce failures, never
capped estimates. There is no size magnitude cap. This is a numerical
search: neither sparse peak discovery nor local refinement is globally
certified. Size representability failure is reported rather than rounded
into a fit. For no truncation, delegate to binom's proved DeRiggi unimodal
search and cancellation-free adjacent difference.

Constant samples attain likelihood one at N=max(x), p=1. Two-point
conditioning supports give a saturated Bernoulli fit already at finite N;
equality with the Poisson face must not be mistaken for an escape.
All four bound subsets declare the truncated-Poisson limit with `fixed:`.
Held values are copied exactly and recorded in `fixed`.

For CMB with both coordinates held, write weights as C(N,k)^nu * exp(eta*k).
For nu>0, use the k=max(x)+1 normalizer term: each ratio
C(M,x)/C(M,k) decreases for all M>=N+1. For nu<0, use
Z>=max(1,exp(M*eta)); log C(M,x) increases with M, so the resulting upper
bound decreases. At nu=0 the weights do not depend on size and Z increases:
the first feasible size is optimal. Endpoint probabilities and N=0 are
handled exactly. Four million cumulative normalizer terms form a work
budget; there is no estimate selected at a size cap. The algorithm does
not assume unimodality. Enumeration and bounds use floating-point arithmetic,
not outward-rounded interval arithmetic.

## Corrections to the supplied analysis

P0b's blanket geometric-coercivity argument for held CMB coordinates is
false at nu=0, p<1/2: the limiting law is geometric, with ratio p/(1-p).
The finite maximum nevertheless occurs at the smallest feasible size,
because its normalizer increases. For positive held nu and odds=lambda/N^nu,
the size limit has weights lambda^k/(k!)^nu: a CMP law with mass on more than
two integers. New tests demonstrate both facts directly with `dcmb`.

The inherited tbinom changes had an exponential-grid magnitude cap, omitted
refinement, and lost accuracy by subtracting large log-gamma terms. These
were replaced; its local status enum and wrapper dispatch were retained.
No unsupported CMB or beta-binomial subset was enabled by guessing a limit.

## Tests

New reference profiles use extraDistr::dtbinom at each integer size with an
independent R probability optimization; the exact limiting likelihood uses
extraDistr::dtpois. Cases include n=20/100, ties, overdispersion, Poisson-like
samples, all four held-bound subsets, constants, saturated two-point windows,
incompatible designs, and the untruncated optimum above 1e5.
CMB profiles use the package's dcmb, with a separate dbinom check at nu=1;
n=20/100, ties, overdispersion, near-limit data, both signs of dispersion,
zero dispersion, probability endpoints, invalid controls and work exhaustion.
Beta-binomial tests verify all three retained diagnostics and demonstrate
the held-alpha negative-binomial face with extraDistr::dbbinom/dnbinom.
Appended doctests check size enumeration, endpoint and budget statuses,
truncated-Poisson escapes, stable profiles through size 1e14, finite saturated
fits, and a numerical work failure.

Intermediate validation exposed incorrect inherited assertions that all
bound choices for x=(2,3,4) escape: widening its lower conditioning window
actually gives a finite fit. Those assertions now check both outcomes.
Constant-data reference optimizations also exposed positive log probabilities
from extraDistr's CDF subtraction; their oracle uses the exact attained bound
zero instead of maximizing that numerical artifact.

Targeted checks: 16 C++ cases / 4,953 assertions passed; R files passed
96 (bbinom), 648 (cmb), and 460 (tbinom) expectations, with no warnings.
`just quality bbinom cmb tbinom`: 3/3 passed.
`just cpp-test`: 6/6 CTest targets passed. The distribution binary passed
850 cases and 1,199,347 assertions; all six binaries together passed 872
cases and 1,210,394 assertions. Final runtime: 451.73 seconds.
`just test`: `[ FAIL 0 | WARN 0 | SKIP 1 | PASS 23370 ]`; the skip is
the pre-existing missing optional `{sn}` reference package.
Logs: `/tmp/umvl-codex/p2b-binom/final-{cpp,r,quality}.log`.
All computations used `/tmp/umvl-cpu/run`; compiler/R temporary files were
redirected into this worktree after a shared /tmp quota failure.
`git diff --check`: clean. No golden or forbidden file changed.

## Open items / outside-file changes

- Complete the six retained subsets only after deriving their actual held
  limiting laws and reliable numerical comparisons, including fixed-size
  nonattainment while searching size. Their READMEs and wrapper errors now
  state the blockers rather than the obsolete reference-cap explanation.
- Global certification of the genuinely truncated free-probability integer
  profile remains open; numerical search limitations are explicit.
- Correct the P0b CMB and beta-held reflection claims in a future research
  revision. No evaluation input or forbidden file was edited in this job.
- The shared tpois/profile.hpp still subtracts large log-gamma terms in
  fixed-size fits. The new tbinom path avoids that issue locally; a shared
  precision fix is outside the assigned files. No TASK-78-owned file changed.
- extraDistr::dtbinom's rare-conditioning subtraction can return masses above
  one on constant data. No reference density was patched; the tests use exact
  point-mass likelihoods for these cases.
- CMB d/p/q/r still enforce size <= 1e7 for allocation safety. Supporting
  larger returned fits would also need a TASK-78-owned density implementation
  change; this enumeration reports work exhaustion before that allocation.
