# std/lst held-subset certificates — TASK-116.13

The held-shape routes now either certify a global maximizer to the required
coefficient accuracy or stop with a structured limit, tie, budget or numerical
reason. The held-ordinary-scale lst route also has a sufficient certificate
for an attained Cauchy optimum with shape free. The three other free-shape
held subsets retain their previous numerical searches and return estimates
when those searches succeed; lack of global certification alone is not a
failure. Both family
`fit.certified` flags remain `partial`; their notes describe the actual coverage.

This is an uncommitted change on `codex/std-held`, based on
`017b9f8ec3bed7610ccd41c62738bce1ea987316`. No commits, board edits, package
installs, or pushes were made. Changes are confined to std/lst fitters,
dossiers and binding routes/tests, the appended JS regression tests, and this
report's evidence directory. Default all-free fitting routes, golden/truth
fixtures, d/p/q/r, special functions, code generators and shared core headers
are unchanged. No changes to those settled components are needed for the
implemented certificates.

## Coverage

Here “shape” means lst df or std nu, and “scale” means ordinary lst sigma or
standardized std sd. Certificate claims are conditional on the computed comparisons
with the stated practical floating-point margins, following the package
certificate standard. An arithmetic or representability failure can refuse
even when a mathematical maximum exists. Successful numerical searches in
uncertified subsets do not acquire a globality claim from these certificates.

| Held parameters | Result |
| --- | --- |
| None | Existing all-free certificate unchanged. |
| Location and shape | Strictly decreasing log-scale score gives the unique maximum, exact unboundedness, or a proved unattained scale-zero supremum. Supported at every valid fixed shape, including lst below one. |
| Scale and shape | Exhaustive scalar location isolation and likelihood comparison; unresolved equal maxima give a tie stop. Degenerate roots or exhausted work give numerical/budget stops. |
| Shape | At shape at least one, strict atom condition plus a two-dimensional root inclusion certifies the joint location/scale maximum. Critical equality has a proved collapse supremum, except for the balanced two-point Cauchy maximum curve. Joint lst fits below one refuse unless exact unboundedness is established. |
| Ordinary lst scale only | A uniform shape-score bound can prove decreasing likelihood on df >= 1; the Cauchy endpoint then receives the complete scalar location certificate. Outside that proved regime, the previous numerical profile search remains available. A certificate budget/root/tie failure still stops. This certificate does not transfer to held std sd. |
| Location only; location and scale; std sd only | Previous numerical profile searches and conditional face comparisons are restored. Finite estimates are returned on search success; exact unboundedness, diagnosed limits, unresolved comparisons and genuine search failures still stop. These general profiles remain uncertified. |
| All parameters | Evaluate the supplied proper point; there is no optimization to certify. |

The structural `holds` lists remain empty. Missing certificates are not
structural parameter exceptions.

## Mathematical and numerical changes

The [derivation](2026-11-07-std-held-data/derivation.md) gives the proofs,
conditions, inequalities and remaining gaps. It links the existing
[geodesic-convexity study](2026-11-04-t-cert-report.md), rather than deriving
globality from a local Hessian.

For fixed location, the log-scale score has endpoint sign
`df*(n-k)-k`, with k observations at that location. Every nonzero-residual
contribution decreases strictly. A positive sign gives one global maximum;
a negative sign gives unbounded likelihood; zero gives a finite unattained
supremum with its explicit likelihood enclosure and collapse-path details.
The sign uses fused multiply-add at the supplied binary64 shape. Stored
`8/3`, for example, is below the exact 8-to-3 critical ratio and must not be
mistaken for equality by a separately rounded product.

For fixed scale and shape, every location maximum lies in the data range.
The new family header encloses that entire range, excludes intervals by
score sign or the curvature of a minimum, and isolates maxima using a
positive score-derivative lower bound. Every surviving maximum participates
in the likelihood comparison. Uniqueness is false: for observations -b,+b,
ordinary scale s and shape v, the two maxima are
`+/-sqrt(b*b-v*s*s)` when `b*b > v*s*s`. For std the threshold is
`b*b > (v-2)*sd*sd`. The tests use the exact symmetric ties and an independent
80-digit asymmetric root. At the threshold the stationary root is degenerate;
the implementation may refuse instead of inventing a simple-root certificate.

For joint fixed shape at least one, EM only nominates a score-checked point.
A Newton refinement must pass a two-dimensional contraction inclusion before
acceptance. The global theorem supplies globality. At critical atom equality,
geodesic convexity along a ray to the atom proves the finite collapse value
is the global supremum. Finite attainment is excluded except for balanced
two-point Cauchy data. That exceptional maximum curve is checked explicitly;
the represented location is projected onto its scale curve. Adjacent doubles
with no representable interior location cause a numerical stop.

The additional free-shape lst certificate bounds the shape derivative
uniformly over location, checks the finite shape interval, and closes the
infinite tail analytically. Its sufficient range/scale condition is stated
in the derivation. Shape and location stages share the same evaluation budget.
A failed sufficient condition is not evidence for a competing face.

Scalar location accuracy is measured against enclosed empirical population
standard deviation. Using the held scale would hide a bad location when that
scale is enormous; the regression sample `(-2,-1,0,1,3)` at scale `1e100` now
returns location 0.2. Root width is at most `1e-8*min(1,s/unit)` in normalized
coordinates, with a final physical width at most `1e-6` times empirical
spread. Joint transformed widths are at most `1e-6` in normalized location
and relative scale units. Scalar scale brackets have width at most `1e-10`
in log scale, with strict signs at a practical `+/-5e-9` bracket.

Physical scales additionally pass a logarithmic round-trip check at `5e-9`.
This prevents a subnormal rounded coefficient from passing on a small score
alone. Std transport also rejects overflowing or inaccurately represented sd.
When held sd maps to a nonnormal ordinary scale, nonconstant location search
refuses; fixed points and constant-sample location maxima are evaluated
directly in standardized coordinates. Displayed diagnostic likelihoods are
evaluated at their represented coefficients, separately from root-value
upper bounds. Proper ordinary-t witnesses retain their model coordinates
when sd is free.

The interval operations reuse the existing `64*epsilon` practical padding
and binary-carry sums. Scalar score signs use the documented `256*epsilon`
allowance for log weights, data size and reduction magnitude. These are
conditional practical-arithmetic certificates, not outward-rounded interval
arithmetic or proofs of libm error bounds. Solver work is
`1*min(iterlim,100000)` and certificate work is
`128*min(iterlim,100000)`; no newly added work cap is independent of iterlim.
Solver exhaustion retains its core enum and is transported as a structured
numerical stop. R and JS routing is binding-neutral.


## Retained numerical free-shape searches

Review feedback corrected the initial choice to refuse every held free-shape
fit without a global certificate. That refusal policy is withdrawn. All new
certificates are retained; the three uncertified subsets use their previous
searches through the binding-neutral routes. Family-wide certification stays
`partial` because numerical availability and a global certificate are separate
claims.

For lst, location held, scale held outside the sufficient Cauchy regime, and
location/scale held use the existing profiled search in log(df-floor), with
floor zero when location is held and one otherwise. The nomination grid is
-12,...,16 by 0.5, followed by golden-section refinements at tolerance `1e-8`.
For std the analogous grid in log(nu-2) is -18,...,16 by 0.5. Std's ordinary
scale is recomputed from the held sd at each trial nu. The previous conditional
normal, eligible lower-face and lst Cauchy comparisons remain numerical.

Inner nuisance searches and outer refinements must converge; failed searches
are not promoted to successful estimates, and existing finite-score and
representability checks remain. The retained Cauchy certificate can still
produce a genuine budget stop before numerical fallback: on the golden lst
held-scale case, `iterlim=1` exhausts that certificate attempt. Tests distinguish
this budget stop from optimizer exhaustion. These finite grids and local
refinements do not exclude every remote maximum or establish signed uniform
tail bounds. The remaining proof work below concerns certification, not
availability of the numerical procedures.

## Validation

Every final command finished with exit status zero. R and ordinary C++ dev/full
suites ran on simbox through `simbox-run`, with no exit-75 fallback.
The complete no-long-double suite ran on the workstation as requested,
alongside JS/site and independent checks through the CPU slot locks. The final
simbox log is [final-simbox-10.log.gz](2026-11-07-std-held-data/final-simbox-10.log.gz);
the workstation C++ log is [no-long-double-restored.log.gz](2026-11-07-std-held-data/no-long-double-restored.log.gz).
The [validation record](2026-11-07-std-held-data/validation.json) contains exact
commands, machines and compressed-log hashes; the
[source manifest](2026-11-07-std-held-data/source-manifest.json) records the
uncommitted implementation/test hashes. Ten native source/test fingerprints
were compared directly with simbox.

| Final run | Machine | Result |
| --- | --- | --- |
| Standalone C++17 held checker | Both | Passed, including extreme-coordinate, collapse, Cauchy-curve and physical-witness assertions. |
| `just test` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 21066. Skips are dev-tier exclusions. |
| `just test-full` | simbox | FAIL 0, WARN 0, SKIP 0, PASS 43886. |
| `just cpp-test` | simbox | 106/106; 47.28 s. |
| `UMVL_TEST_TIER=full just cpp-test` | simbox | 129/129; 52.55 s. |
| `just cpp-test-no-long-double` | Workstation | 129/129; 117.18 s. |
| `just js-build` and `just js-test` | Workstation | Build succeeded; 444/444 tests, no failures, cancellations or skips. |
| `GOMAXPROCS=4 just site-check` | Workstation | Passed: 99 family pages, MathML, links, graph, 99 C++17 snippets and runtime asset checks. |

The JS build, also invoked by site-check, emits the existing
`-Wtautological-compare` warning in unchanged `binom/mle.hpp:160`. There are no
R test warnings. Native build logs also retain existing indentation warnings
in unchanged certificate/invburr test code and a copy warning in the unchanged
lst range loop. The site audit's unrelated regenerated screenshot was
restored to its original bytes.

The unchanged all-free goldens pass their existing relative tolerance
`3e-5`; no tolerance or fixture was relaxed. The independent
[80-digit checks](2026-11-07-std-held-data/checks.json) verify the asymmetric
root, two-point rational score identity, scalar scale identity, actual-hold
normal-seam coefficients, lower-shape and normal-tail value inequalities,
and the sufficient shape-envelope bound. Those finite checks are numerical
evidence; the general claims follow the linked derivation. No simulation
seed is needed: every workload is deterministic.

R, JS and C++ regressions again require finite golden-data fits for all three
free-shape subsets, preserve held coefficients and likelihood expectations,
and require stops on exhausted work. Conditional lower-face expectations are
restored. The certified-mode, collapse, accuracy and representability tests
from the original implementation remain in place.

Earlier compile, restriction-test and site-link failures, the withdrawn
refusal policy, and the corrected exhaustion-test expectation are recorded in
[development events](2026-11-07-std-held-data/development-events.json) and the
retained superseded logs. Only the final frozen-source logs establish the
results above. The first review rerun had one new test expecting a numerical
stop where the retained Cauchy attempt correctly returned a budget stop; only
test expectations changed before the final green runs. The log collector was
also adjusted to accept the workstation CTest summary's additional
“0 tests failed” wording.

## Timings

The [timing source](2026-11-07-std-held-data/timing.cpp) fits both families to
500 observations `sin(1.71*j)+0.3*cos(0.41*j)`, j=0,...,499, with held
location 0.2, scale 1.3 and shape 3 as applicable; tolerance `1.220703e-4`,
iterlim 200. Each row is the combined wall time of one lst and one std call.
The baseline executable was compiled from the original revision before edits;
it was not rebuilt against changed headers. Both executables ran consecutively
under one workstation slot. This is one paired observation on a shared machine,
not a statistical performance guarantee.

| Held | Before, seconds | After, seconds | Outcome in both runs |
| --- | ---: | ---: | --- |
| Location | 0.160743 | 0.145999 | Both numerical searches select the normal limit. |
| Scale | 7.44383 | 7.40934 | lst selects the normal limit; std returns a finite numerical estimate. |
| Location, scale | 0.00206286 | 0.00208877 | lst selects the normal limit; std returns a finite numerical estimate. |
| Shape | 0.000112982 | 0.00207493 | Both finite joint fits; the after fits require root inclusion. |
| Location, shape | 0.00140811 | 0.00150195 | Both finite scalar scale fits; after brackets certify accuracy. |
| Scale, shape | 0.0579991 | 0.00440589 | Both finite scalar location fits; after fits isolate all possible maxima. |

All six before/after status pairs match. The three free-shape rows again
perform their numerical searches; the slow held-sd profile is retained.
The earlier very short refusal timings are withdrawn and separately labelled
[timing-refusal-superseded.txt](2026-11-07-std-held-data/timing-refusal-superseded.txt).
Raw current status codes and paired timings are retained in the evidence.

## Open global certificate coverage

1. **Std free nu with any nontrivial held subset.** General finite maxima
   and normal/nu-down-to-two comparisons need actual-hold profile envelopes,
   root inclusion for every surviving branch, and signed uniform tail bounds.
   With sd held, ordinary scale changes with nu and the lower face is a
   collapse path, not the positive ordinary-scale t2 face. Existing all-free
   nuisance fits cannot enforce these holds. Exact unboundedness remains
   available; the previous numerical searches and face comparisons remain
   available for the other cases, without global certification.
2. **Lst free df outside the sufficient held-scale Cauchy regime.** Held
   location/free scale, held location/scale, and the remaining held-scale
   profiles need finite-shape root/branch isolation and endpoint comparisons.
   The derivation establishes explicit small-shape and normal-tail value
   bounds, including the different actual-hold seam coefficients; their
   integration and signed normal-winner remainders are not implemented.
   Held-location profiles can extend below one, where the existing all-free
   derivative engine cannot be reused unchanged. Increasing the old grid or
   iterlim does not close these missing certificates. The previous numerical
   procedures remain available while that proof work is open.
3. **Joint lst location/scale at fixed df below one.** The nonnegative
   Busemann-coefficient theorem no longer applies. Global two-dimensional
   enumeration or another theorem is needed, including all collapse-face
   comparisons. Negative collapse exponents are still proved unbounded;
   strict and critical remaining cases refuse numerically with a feasible
   witness where available.
4. **Degenerate scalar roots and physical representability.** Current root
   isolation may reach a numerical/budget stop at a degenerate maximum or an
   unresolved likelihood tie. Subnormal/overflowing coefficients may also
   prevent the required accuracy. These are explicit stops, not capped or
   unchecked estimates; improved multiplicity handling or arithmetic could
   expand availability.
