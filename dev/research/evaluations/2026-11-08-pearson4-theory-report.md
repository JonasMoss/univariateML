# Pearson IV: existence, escapes and numerical design — TASK-161

Completed 9 November 2026 (Oslo) on `codex/pearson4-theory`, based on
`2ed5514b32ec72ded95809c834453d24f025f850`. Only this report and its evidence
directory are added. No production code, dossier, golden, test infrastructure,
roadmap, status file or board was changed. No commit, installation, sub-agent
or publication was made.

Pearson IV has a useful compactification and conditional concavity, but a
general fitter cannot assume a monotone scale score or a unique maximum.
Its nondegenerate boundary laws are reflected/translated inverse gamma
(type V) and normal. **A finite-shape gamma law is not an IV escape.**
An all-domain rejection sampler derived here also removes the m>1 restriction
of the generator described in the PearsonDS documentation.

## Deliverables

* [Proofs, coordinates, collapse faces, compactification, fitter structure and inference](2026-11-08-pearson4-theory-data/proofs.md).
* [All 16 free subsets, including constants, ties and hold-dependent escapes](2026-11-08-pearson4-theory-data/subsets.md).
* [Numerical algorithms and error accounting](2026-11-08-pearson4-theory-data/numerics.md).
* [Sources and access limitations](2026-11-08-pearson4-theory-data/sources.md).
* [Finished validation record](2026-11-08-pearson4-theory-data/validation.json).

## Coordinates and fitted domain

Recommend `mlpearson4(x, location=NULL, scale=NULL, m=NULL, nu=NULL,
tol=1e-10, iterlim=100)`, with location λ, scale α>0, m>1/2 and real ν.
All coefficients are regular and holdable. Location is the root-pair centre,
not a mean or mode; scale is an affine scale, not a standard deviation.
These names match PearsonDS. The accuracy contract supersedes the older
argument-name proposal's optimizer controls.

Heinrich's a is α. In Nagahara's differential-equation convention,
μ=λ, τ=α, b=m and δ=−ν/(2m). Student t is the interior ν=0 slice with
df=2m−1 and t scale α/√(2m−1). A held Pearson scale therefore differs
from a held t scale when shape varies. Source-access qualifications are
recorded rather than treating inaccessible full texts as inspected.

With location and scale free, recommend m≥1: it is the weakest bound
independent of sample size protecting every multiplicity strictly below
half the sample against collapse. It does not guarantee a finite IV MLE,
because a proper excluded boundary can still win. Held-location and
held-scale subsets admit proved exemptions; the inventory also states
the consequences of retaining the broader R5 restriction instead. This
research does not implement a policy change. Sample-dependent failures
do not belong in `holds:`.

## Existence and escaping rates

Collapse at an observation repeated k times has exponent

$$e=(2m-1)(n-k)-k.$$

Negative e proves unboundedness; positive e excludes that collapse.
Equality has an explicit finite likelihood face, with a free location
approaching λ=t+αν/(2m) to centre the collapsing mode. Free-skew faces
include infinite-skew endpoints: a collapsing V law can retain a nonzero
face when every noncentral observation is on its supported side. The
proof covers simultaneous shape, skew and scale rates.

Equality does not imply nonattainment. For m=1,ν=0 and balanced two-cluster
data, an exact product identity gives a continuum of finite maximizers.
With the same held shapes, half ties and at least three distinct values,
it gives an unattained supremum. Held-location equality with ν=0 is also
settled exactly. Other attained-point decisions remain explicitly open.

An angular product bound also settles m=1 with nonzero held skew when a
half-tie is at the appropriate extreme, including balanced two-cluster
data: the supremum is unattained. With skew free, an extreme half-tie at
m=1 is likewise proved nonattainment. These additional exact cases are
included in the proofs and inventory.

The main proof uses mode c, curvature width w, D=√(2m), and bounded skew k:

$$c=\lambda-\alpha\nu/(2m),\qquad
w={\alpha\sqrt{1+(\nu/(2m))^2}\over\sqrt{2m}},\qquad
k={\nu/(2m)\over\sqrt{1+(\nu/(2m))^2}}.$$

The standardized log-density derivative is
−u/[1−2ku/D+u²/D²]. Integrating it gives uniform density envelopes for
all m≥1 and arbitrary skew. An incumbent likelihood and the distinct-data
spacing give finite c bounds and positive lower/upper w bounds when
2k_max<n. Compactifying 1/D and k gives precisely IV, V and normal faces,
with conservative computable normal-face enclosures.

The V path is α→0, ν=−sβ/α, λ→t, shape a=2m−1, with support
s(x−t)>0. The proofs give its log likelihood, orientation and threshold
constraints. The normal path is m→∞ with c,w finite, uniformly over skew
rates. A held Pearson location need not fix the normal-limit mean; a held
Pearson scale can still permit a normal escape. Holding location and scale
together forbids a positive-width normal face. All 16 cases are tabulated.

The exact inequality |x−λ|f(x)≤2m−1 excludes m↓1/2 uniformly when
location is held and unobserved. An explicit modal bound handles held
scale. Otherwise small m supplies unbounded collapse in the stated cases.
The mode chart handles divergent location, scale and skew jointly. Its
exhaustive closure and the positivity constraint on the Pearson quadratic
both rule out a finite-shape gamma boundary.

## Fitter and inference

With location and scale held, the shapes form a strictly concave exponential
family. Skew-only fits always attain uniquely. Joint shape fits attain
uniquely on nonconstant data; constant data can concentrate as m→∞.
These conditional results do not imply joint nuisance/shape concavity.

Nonzero skew destroys scale-score monotonicity. A two-point counterexample
has at least three stationary scales. Fixed-shape scalar scores become
polynomials after clearing positive denominators: extend lst root isolation,
shift the location bracket, and enumerate every scale root. General fits
need compactified boxes and explicit face comparisons. A likelihood-value
gap alone establishes neither coefficient accuracy nor an escape.

Structured stops distinguish unbounded collapse, constant-sample shape
concentration, proved V/normal suprema, unresolved boundary comparison,
numerical failure and budget exhaustion. A free-threshold V escape cannot
say `use mlinvgamma`: that model is absent. Normal delegation must retain
any inherited constraint. All proposed fitting caps use the shared ledger.

The interior four-parameter model is DQM with finite full-rank information
for every m>1/2, without moment assumptions on X. At ν=0 both odd and even
score blocks remain full rank. For V, use inward coordinate ε=α² with
β=|ν|α fixed. The independently derived log-density correction is

$$H(y)={a(a^2-1)\over6\beta^2}-{a+1\over2y^2}+{\beta\over3y^3}.$$

It has zero V expectation and gives an independent square-integrable
one-sided score. The translated V endpoint is infinitely flat and its
threshold is DQM; α is not a regular first-order boundary coordinate.
At the normal face the original four coordinates lose their finite
identifiable interpretation, so ordinary four-parameter Wald inference
is not justified.

## Finished numerical evidence

All four numerical jobs exited zero on the **workstation**, through
`/tmp/umvl-cpu/run`, under `nice -n 10`, with BLAS/OpenMP pinned to one
thread. No simbox or production suite was used; the task explicitly
requires evidence scripts for this research-only scope.

| Evidence | Result |
|---|---|
| [check.py](2026-11-08-pearson4-theory-data/check.py), [checks.json](2026-11-08-pearson4-theory-data/checks.json) | 53 checks: complex gamma, both tails, central/hypergeometric primitives, normalization, quantiles, V/normal paths and scores. 90-digit references; main prototypes at 65 digits. |
| [structure.py](2026-11-08-pearson4-theory-data/structure.py), [structure.json](2026-11-08-pearson4-theory-data/structure.json) | 137 records: normalizers, gamma product, positive quadrature enclosures, scale-root signs, Cauchy continuum, envelopes, mode scores and rank evidence. 90 digits. |
| [hyper.py](2026-11-08-pearson4-theory-data/hyper.py), [hyper.json](2026-11-08-pearson4-theory-data/hyper.json) | Four own complex 2F1 sums, with future-term remainder bounds; 65-digit sums versus 90-digit references. |
| [sampler.py](2026-11-08-pearson4-theory-data/sampler.py), [sampler.json](2026-11-08-pearson4-theory-data/sampler.json) | 320 accepted draws from 1,791 proposals at 80 digits, including m=0.5001, m=10,000 and skew 10^8. |
| [validate.py](2026-11-08-pearson4-theory-data/validate.py) | Source hashes, thresholds and all 16 inventory rows verified: 514 numerical/draw records total. |

The design supplies bounded Stirling/log-ratio normalization; positive
angular/reciprocal quadrature; convergent tails with Cauchy remainders;
safeguarded log-tail quantiles; and a normalizer-free rejection sampler
using only open uniforms. For m≥1, expected sampler proposals are proved
below 35 uniformly in shape/skew; it also applies to every 1/2<m<1.
R supplies the uniform callback and retains RNG ownership in the binding.

Small tails are computed directly. The m=0.5001 quantile checks reach log
absolute values above 40,000, requiring log-distance arithmetic and explicit
representational-overflow handling. The numerical document distinguishes
analytic remainder bounds from empirical test tolerances. These prototype
results do not establish uniform binary64 accuracy. The first run's invalid
tail-only quantile test bracket and its correction are recorded openly;
all retained final scripts and outputs pass.

The final quantile run additionally checks every bisection sign against
the series remainder and working-arithmetic margin, retaining relative
distance brackets narrower than 10^(−50). The repository-wide Markdown
checker found six existing issues outside this job (missing generated
artifacts and parser false positives); they are recorded in validation.json.
The new bundle's destinations are checked separately. The artifact viewer
rejects this worktree because it is outside its configured readable roots;
the files remain in the authorized worktree.

## Precise open entries

1. **E: exact boundary attainment decisions.** General collapse/V/normal
   ties need a signed neighborhood inequality and global interior exclusion,
   or an exact maximizing-set decision. A tolerance tie, negative local
   boundary score or value-gap certificate is insufficient. Unsettled
   cases must return `BoundaryComparisonUnresolved`.
2. **N: normal-face inference.** The formal expansion gives skewness
   −4k/D and excess kurtosis 6(1+4k²)/D², suggesting cubic/quartic cone
   directions. Uniform L2/LAN remainders and profiling/localization remain
   missing. No normal-face LR mixture or Wald calibration is claimed.
3. **G: production numerical margins.** Implement the combined normal-regime
   gamma branches, bounded shape-score derivatives, representable-output
   rules and measured floating-point margins. Unresolved cancellation
   must fail rather than silently lose accuracy.
4. **H: optional published accelerators.** The real series, local complex
   primitive and own 2F1 summation are checked. Every global Heinrich/Willink
   transformation and branch constant has not been independently validated.
   Positive quadrature supplies the correctness fallback.

Reproduce each linked script with the job's temporary-directory environment
through `/tmp/umvl-cpu/run nice -n 10 python3`, then run `validate.py`.
All lasting findings and open decisions are retained in this bundle.
