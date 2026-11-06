# sstd certificate report — TASK-116.11

> **Not merged (2026-11-06).** The code stays on branch `codex/sstd-cert` (commit a24a7673).
> Its routing lives only in the R adapter, so R and JS would disagree, and the
> certified subsets are unit-skew special cases. The mathematics and the
> fixed-skew multimodality evidence below are kept; the held-mean, held-nu
> scale certificate belongs in std (TASK-116.13).

Source base: `bb734cd78b846a565d8835606328bb1285a82db6`, branch
`codex/sstd-cert`. Changes are uncommitted. No board edits, dependency installs,
pushes, or changes to other worktrees were made. Long R/C++ suites used the
explicitly authorized simbox mirror.

## Result

The all-free default is unchanged, and `fit.certified` remains `no`.
General skew certification is **open**. The delivered certificates cover:

- **Held `xi = 1`, mean/sd/nu free:** exact reduction to the existing std
  global certificate, including terminal numerical, budget, tie, normal,
  ordinary-scale t2 and collapse stops. This is an exactly held submodel;
  an estimated skew near one never qualifies.
- **Held mean, nu and `xi = 1`, sd free:** a strictly monotone scale-score
  certificate. It returns the unique global maximum, proves unboundedness,
  or identifies the finite unattained scale-zero supremum at tie equality.
  Unresolved arithmetic or solver exhaustion stops numerically.
- **Two observations, held sd/nu and `xi = 1`, mean free:** an analytic
  quartic comparison. A unique maximum is returned when representable with
  the documented margins; two global maxima cause a tie stop retaining both
  witnesses. The flat seam and inaccurate midpoint arithmetic stop numerically.

All-held evaluation remains available. Every other free subset retains its
existing implementation and numerical qualifications. The
[16-subset inventory](2026-11-06-sstd-cert-data/subsets.md) identifies the exact
remaining certificate needed for each subset.

## Mathematics and numerical contract

The full [derivation](2026-11-06-sstd-cert-data/derivation.md) distinguishes
proofs from numerical evidence. With unit skew, held mean and shape, put
`d_i = x_i - mean`, `m = nu - 2`, `l = log(sd)`, and let `k` count zero
residuals. The total likelihood score is

\[
 S(l)=-n+(\nu+1)\sum_{d_i\ne0}
       \frac{d_i^2}{m\exp(2l)+d_i^2}.
\]

Each nonzero summand strictly decreases. The endpoint scores are
`nu*(n-k)-k` and `-n`. A positive first limit establishes a unique global
scale maximum. A negative limit establishes unboundedness at zero scale.
Equality makes the likelihood strictly decreasing at every positive scale,
with finite supremum

\[
 n C(\nu)+\frac{(\nu+1)(n-k)}2\log(\nu-2)
 - (\nu+1)\sum_{d_i\ne0}\log|d_i|,
\quad
 C(\nu)=\log\Gamma((\nu+1)/2)-\log\Gamma(\nu/2)
         -\tfrac12\log(\pi(\nu-2)).
\]

All-zero residuals are unbounded. The minimum and maximum nonzero absolute
residuals, multiplied by
`sqrt((nu*(n-k)-k)/(n*(nu-2)))`, bracket the finite scale root. Thus no
arbitrary finite search window or assumed unimodality is needed.

The implementation uses logarithmic residual arithmetic, the established
stable standardized-t normalizer, fused multiply-add for the collapse-exponent
comparison, a `128*epsilon*n*(nu+1)` score margin, and log-scale bracket width
`2e-12*(1+max(abs(endpoints)))`. A sign-uncertain midpoint only proposes a
new bracket: its new endpoint signs must verify it. Failure stops rather than
returning a capped estimate. Equality-stop bounds use
`128*epsilon*(1+sum(abs(likelihood terms))+n)` around the computed supremum.
The new scale loop permits `min(iterlim,100000)` solver iterations. The std
reduction receives `128LL*min(iterlim,100000)` certificate evaluations, with
solver iterations clamped separately to 100000.

For observations `{-a,a}`, held sd `s`, unit skew and held nu, set
`c=(nu-2)*s*s`. Maximizing likelihood is equivalent to minimizing

\[
 P(\mu)=\mu^4+2(c-a^2)\mu^2+(c+a^2)^2.
\]

If `a*a > c`, there are **two global maxima** at
`mean = +/-sqrt(a*a-c)`; otherwise zero is the unique mathematical maximum.
In particular `{-2,2}`, sd=1, nu=3 has maximizers `+/-sqrt(3)`.
This disproves uniqueness for all supported conditional subsets; it does not
settle uniqueness of the all-free fit.

The two-observation implementation rationalizes the small inward displacement
from each observation, avoiding cancellation in midpoint-minus-offset.
`{1,1e100}` is a regression case. It stops within
`128*epsilon*(a*a+c)` of the standardized discriminant seam, including zero,
and checks midpoint residual balance in held-sd units. The adjacent-observation
case `{1,nextafter(1,2)}`, sd=1e-20, nu=1e10 must stop because rounding its
unique midpoint materially changes the likelihood. Tie likelihood bounds use
`1e-10*(1+abs(logLik))`.

These certificates follow the package standard: exact-arithmetic arguments
conditional on computed comparisons and stated practical floating-point
margins. No outward-rounded interval arithmetic is claimed. The std delegate
inherits its parent's certificate and floating-point contract.

## Why general skew remains open

The [seeded six-point example](2026-11-06-sstd-cert-data/skew-scale.json),
reproduced by [skew_scale.py](2026-11-06-sstd-cert-data/skew_scale.py), holds
mean=0, nu=3 and xi=10. Its scale score has numerical roots at log(sd)
approximately 3.76466556, 4.85013938 and 5.89476524, with
maximum/minimum/maximum sign changes. The two numerical local maxima have
log likelihoods approximately -50.43173618 and -38.80730256. This is numerical
multimodality evidence, not a proof that all roots were enumerated or that the
larger witness is globally optimal. It rules out extending the symmetric
monotone-score argument or trusting a single unimodal refinement.

The following items are open:

1. **General fixed-skew scale and location profiles:** exclude every other
   stationary branch, compare all competing maxima, and bound the exterior
   scale tails. A score check or finite scan is insufficient.
2. **Joint shape/skew profiles and held moments:** construct global bounds
   under the curved public-mean/public-sd constraints in two-piece coordinates.
   Prove exclusion or comparison of coupled escapes, not just the separately
   fitted infinite-nu, t2 and half-t faces. No completeness theorem for the
   current joint face list was established.
3. **Shared diagnostic presentation:** the new core payloads are exposed as
   structured R conditions. The unchanged common JS binding still fails
   safely through its existing statuses/messages and does not expose these
   new payloads. Closing that presentation gap requires common-binding routing;
   no shared binding header was changed here.
4. **Inherited work-cap policy:** the reused std/lst certificate retains
   pre-existing `min(40,maxit)` and `min(60,maxit)` refinement ceilings, and
   legacy skew searches retain fixed caps/windows. New sstd work respects the
   stated budget multipliers. Removing the inherited ceilings requires shared
   implementation changes beyond this job's append-only shared-header scope.
   They can limit recovery from a numerical stop; they do not authorize an
   unchecked successful estimate.

These gaps are why the default and family certification flag were preserved.
No change to settled d/p/q/r, special functions, truth fixtures, code generators,
or generated density contracts was needed.

## Implementation and diagnostics

`distributions/sstd/scale_certificate.hpp` contains the scale certificate;
`sstd/mle.hpp` selects the exact submodels and appends `StopDetail` without
changing existing aggregate-field positions. Certificate failures never pass
through the all-free BFGS score rescue. Finite stopped-family candidates use
`mean, sd, nu, xi`; parent normal, ordinary-scale t2 and collapse candidates
retain their own model coordinates. No zero standard deviation is presented
as a fitted law. Held-fit controls now reject nonfinite tolerance explicitly.

The sstd R adapter presents limit, nomle, tie, budget and numerical conditions
for the new paths. Tests independently evaluate densities at diagnostic
witnesses. Tests also cover the analytic scale polynomial, scales from 1e-150
to 1e150, low iteration limits, neighboring df values around collapse equality,
the exact two-location counterexample, remote endpoints, and normal/collapse
face names. New C++ cases were appended to the dossier test file.

## Validation

All commands finished before this report was written. Source fingerprints,
commands and final summaries are retained in
[validation.json](2026-11-06-sstd-cert-data/validation.json).

| Run | Machine | Result |
|---|---|---|
| Targeted sstd C++ executable | Workstation, one slot | 26 cases, 3823 assertions passed; 2 unrelated main-file cases filtered |
| `just cpp-test` | simbox | 106/106 passed; test wall time 56.95 s |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 127/127 passed; 98.66 s |
| `just cpp-test-no-long-double` | simbox | 127/127 passed; 76.84 s |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20886; 117.865 s |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43706; 326.188 s |
| `just js-build` | Workstation, four slots | Passed; 99 families generated |
| `just js-test` | Workstation, four-slot reservation | 432/432 passed, no skips; 100.392 s |
| `just site-check` | Workstation, same reservation | Passed; 99 family pages and C++ snippets, MathML and links checked |

The dev runs preceded the last diagnostic-default initializer / R error-wording
clarifications; the full tiers cover the final corresponding sources. Dev skips
are tier exclusions, not full-tier missing coverage. No simbox exit-75 fallback
was needed. All workstation computation used the shared slot wrappers, with
single-thread BLAS/OpenMP; long commands ran under their nice settings. The
simbox wrapper supplied nice and thread settings there.

Workstation compiler: GCC 15.2.0 (Ubuntu 15.2.0-16ubuntu1), Node 22.22.1.
Site checking reported Python 3.14.5 and SymPy 1.14.0. Simbox used R 4.6.1 and
GCC 15.2.0. Builds used installed BH headers; no dependencies were installed.
The initial standalone compiler attempt failed because system Boost was absent;
using the existing BH include directory resolved it before tests ran.
Shared-header compiler warnings and existing shell-profile cargo-path notices
were observed. Newly introduced missing-initializer warnings were fixed; final
full builds contain none for `SstdResult`.

The site check rewrote one pre-existing screenshot outside this job's scope;
it was restored byte-for-byte from the read-only Git object. No Git command
that changes repository state was run. No new figures or screenshots are needed.
Golden fixtures are unchanged, and all golden regression checks passed.

## Before/after timings

[Timing source](2026-11-06-sstd-cert-data/timing.cpp),
[before](2026-11-06-sstd-cert-data/timing-before.txt), and
[after](2026-11-06-sstd-cert-data/timing-after.txt) are retained. The workload is
the 500-observation sstd golden sample (seed 12791), tol=1.220703e-4,
iterlim=200, compiled with `g++ -O2 -std=c++17`, installed BH headers and the
standalone test JSON loader. Fits ran on the workstation under one shared slot.
The baseline include tree was copied before editing the fitter. To reconstruct
it, copy the current include tree into scratch and replace sstd/mle.hpp with
`git show bb734cd7:univariateML-cpp/include/cppunivariate/distributions/sstd/mle.hpp`;
compile the timing source with that include tree first on the include path.
The removed baseline-copy intermediate occupied about 16 MB.

| Fit | Repetitions | Before, ms/fit | After, ms/fit |
|---|---:|---:|---:|
| All-free default | 3 | 41.113 | 26.223 |
| Held mean=0, nu=5, xi=1; sd free | 100 | 7.303 | 0.454 |
| Held xi=1; mean/sd/nu free | 3 | 16.505 | 68.855 |

These are illustrative wall times under shared load, not controlled performance
estimates. The scale certificate was about 16 times faster in this measurement;
the three-free global certificate costs more than the old numerical search.
The all-free default returned **bit-identical coefficients and likelihood** in
the two binaries; its timing variation is not an algorithmic speed claim.
The scale-only result changed by about 1.9e-8 in sd; the symmetric three-free
result changed by less than 1e-6 in each coefficient. Likelihood differences
were below 2e-11. No golden was regenerated or loosened.

## Artifact presentation

The requested `show` invocation was attempted after every suite completed.
The viewer returned HTTP 403 because this worktree is outside its configured
readable roots. Viewer access is **open**: the worktree must be registered as
readable before the report can appear in the artifact pane. The report remains
at its requested path. No viewer configuration or files outside this worktree
were changed to bypass that restriction.
