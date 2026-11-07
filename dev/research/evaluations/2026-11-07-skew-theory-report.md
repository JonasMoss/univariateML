# Certifiable structure for sstd and st — TASK-116.15

Source: `31e2b66d395dd87068e1e204e934298407ea5955`, branch
`codex/skew-theory`. This is a research deliverable. No fitting code, library
tests, goldens, bindings, family dossiers, board records or tracked existing
files were changed; no commit, installation or publication was made.

The likelihoods have enough structure to support global certificates without
assuming unimodality. For sstd, fixed degrees of freedom give algebraic
constraints and rational scores in mode/half-scale coordinates. For st,
explicit Student-tail bounds confine conditional skewness maxima. Uniform
Student envelopes give finite nuisance search ranges and exhaust the escape
coordinates below the strict tie thresholds. Collapse equalities also reduce
to explicit lower-dimensional face problems. These results establish routes
for implementation; they do not certify the current multistart fitters.

The complete [proofs and algorithms](2026-11-07-skew-theory-data/proofs.md)
and [all 32 free-subset entries](2026-11-07-skew-theory-data/subsets.md)
are part of this report. The latter distinguishes mathematical reductions,
remaining numerical machinery, held-value exceptions and equality cases.
[Reproduction code](2026-11-07-skew-theory-data/check.py),
[computed evidence](2026-11-07-skew-theory-data/checks.json),
[validation record](2026-11-07-skew-theory-data/validation.json) and
[source fingerprints](2026-11-07-skew-theory-data/source-manifest.sha256)
are retained beside them.

## Proved sstd structure

Use mode c and positive denominator half-widths a,b; these are sqrt(nu)
times ordinary t half-scales. With m=nu−2,

\[
 f(x)=\frac{K_\nu}{a+b}
   \left(1+\frac{(x-c)^2}{a_s^2}\right)^{-(\nu+1)/2},\qquad
 M=c+h_\nu(b-a),\quad
 S^2=\frac{a^2-ab+b^2}{m}-h_\nu^2(b-a)^2,\quad \xi^2=b/a,
\]

where a_s is a on the left and b on the right,
h_nu=Gamma((nu−1)/2)/(sqrt(pi)Gamma(nu/2)), and
K_nu=2Gamma((nu+1)/2)/(sqrt(pi)Gamma(nu/2)). M,S are the public mean and
sd. This map is bijective in the interior. For fixed nu, a held mean is an
affine constraint, a held sd is a nondegenerate ellipse, and a held xi fixes
a linear half-width ratio. Side contributions separate except for the
normalizer −n log(a+b); independent half-side maximization is invalid.

Several concrete consequences follow.

* **Mean only free:** on each observation cell, clearing positive
  denominators gives a location-score polynomial of degree at most 2n−1.
  Isolate every root, include seams, compare all candidates; location tails
  vanish. Multiple global maxima are possible.
* **Sd only free, arbitrary held xi:** partition positive u=1/S at the
  shifted-residual sign changes. The cleared score has degree at most 2n
  on each of at most n+1 cells. Compare every root, seam and the explicit
  u=infinity limit. This closes the mathematical scalar route even when
  the scale score has three or more roots.
* **Xi only free with both moments held:** put r=xi² and adjoin the
  positive square root of its quadratic variance coefficient. Stationary
  and seam equations are algebraic; the two skew endpoints have the exact
  held-moment penalties described below.
* **Several moment/skew coordinates free, nu held:** eliminate holds and
  enumerate polynomial critical components, including observation strata.
  The likelihood is constant along each connected critical component;
  isolated simple roots must not be assumed. This is finite mathematical
  structure, with potentially expensive computation.
* **Mode and nu fixed, unconstrained half-widths:** for each ratio, the
  common-width score is strictly decreasing and has an explicit root
  bracket. This yields a one-dimensional ratio profile. Holding public
  moments generally invalidates fixing the mode; the constraint equations
  above must be used instead.

Sections 1–3 of the proofs give the equations, domains, denominator signs,
root degrees and endpoint comparisons needed to implement these reductions.

## Uniform exclusion and all escape coordinates

Let k_max be the largest tie multiplicity. The proved envelopes are

\[
 t_\nu(z)\le3(1+z^2)^{-3/2}\quad(\nu\ge2),\qquad
 t_\nu(z)\le(1+z^2)^{-1}\quad(\nu\ge1).
\]

For sstd, in ordinary half-scales with total width W, these imply
f(x)≤6W²/(W²+(x−c)²)^(3/2), uniformly over skewness and nu>2.
For st, f(x)≤2omega/(omega²+(x−xi)²), uniformly over alpha and nu≥1.
A feasible incumbent likelihood and the distinct-data spacing give explicit
upper and lower width bounds and a finite location range. Near a tie of
multiplicity k the upper bounds have powers W^(2(n−k)−k) for sstd and
omega^((n−k)−k) for st. These exclude small widths uniformly when
3k_max<2n and 2k_max<n, respectively. Held location and held scale can
weaken the sufficient tie condition, as detailed in the subset inventory.
For held nu, use its exact threshold nu(n−k)>k; st held nu may be below one.

Consequently, in those strict regimes, every likelihood-relevant escaping
sequence has a subsequence on the following faces or their intersections:

| Family | Exhaustive coordinate faces after nuisance exclusion |
|---|---|
| sstd | nu=2 two-piece t; either zero half-scale (half-t); nu=infinity two-piece normal; half-t2 and half-normal intersections. |
| st | alpha=±infinity half-t; nu=infinity skew-normal; half-normal intersections. Free nu=1 is an attained restriction face. |

This is a sequential compactness proof controlling joint paths, not merely
a list of fixed-parameter limits. Public holds must be retained on each
face. Sstd with sd held excludes the positive-width nu=2 face, but does
not exclude width collapse as nu approaches two. Finite held xi removes
sstd's half-faces. St with omega held excludes scale collapse altogether.

The proof also gives a global-in-residual bound for replacing t_nu by a
normal kernel. For st it gives a uniform product-likelihood error bound
for the skew-normal limit **even with alpha unbounded**, using a chi-square
mixture and logarithmic scale bounds for the normal CDF. This controls the
joint alpha/nu corner. Quantitative monotone tail majorants and a globally
certified limit-model profiler still need implementation; evaluating the
error formula only at one finite nu cutoff is insufficient.

## Endpoint densities: corrections that affect certification

Weak convergence alone does not determine likelihood at an observed endpoint.
The following are exact mathematical distinctions, not floating-point effects.

For sstd with nu fixed and **both mean and sd held**, a shrinking half-width
forces the mode toward a half-t endpoint through the thin side. Put
v=1/(nu−2)−h_nu² and rho=h_nu/[2(nu−2)v]. Each observation at that endpoint
has density multiplier (1+rho²)^(-(nu+1)/2) relative to the closed half-t
endpoint. At nu=3 the multiplier is **0.6042259416**. Substituting the closed
half-t density would overstate the reachable likelihood.

When nu is free and tends to a finite half-t face, its standardized absolute
moment is strictly increasing in nu. This makes the constrained endpoint
strictly decreasing in nu; an O(a) change in nu cancels the thin-side mode
shift. Thus the full endpoint factor is reachable at every finite free-nu
half-t face, including with both moments held.

At the joint normal/half-scale corner the df perturbation moves the endpoint
in the same direction as the thin-side shift. With both moments held the
largest reachable multiplier is exp(−rho_N²/2), where
rho_N=sqrt(2/pi)/[2(1−2/pi)]; its value is **0.5473573954**.
With only one or neither moment held, multiplier one is attainable.
The reflected faces have the same factors. Section 6 proves these statements,
including the strict gamma-ratio monotonicity needed to exclude a degenerate
finite-nu slope.

For st, **free location** permits the closed half-t endpoint supremum by
approaching the endpoint from its favorable side while |alpha| grows.
If location is **held exactly at that endpoint**, the skewing factor stays
one, rather than two: divide the closed half-t likelihood by 2^k for k ties.
Other direct holds do not change this distinction. These corrections apply
to half-normal corners as well.

## Conditional st skewness and equality faces

For held nuisance coordinates write q_i=z_i sqrt((nu+1)/(nu+z_i²)).
If all nonzero q_i have one sign, the likelihood is strictly monotone in
alpha and its supremum is on a half-face. If every q_i is zero, alpha is
unidentifiable. With both signs present, section 7 gives explicit finite
bounds −A_- and A_+ outside which the skew score has fixed inward signs.
The score is analytic and has finitely many roots in that bracket. A complete
root count or compact upper-bound subdivision can therefore replace a scan.
Global log-concavity is unavailable: log Student CDF is convex in its far
negative tail.

A zero scale-collapse exponent does not itself establish non-existence.
This job reduces those equality faces as follows:

* **Sstd:** section 13 gives an explicit one-dimensional ratio profile for
  the full collapse supremum, including its boundary ratios. Free mean
  places the mode at the tie; held mean fixes a known tied-kernel penalty.
  With free nu the critical face uses nu=2. A held sd imposes an exact
  width/df relation and does not change the reduced supremum. The profile
  has rational logarithmic derivative and can be enumerated algebraically.
* **St:** section 14 reduces held-alpha faces to a scalar tied-location
  profile. With alpha free and off-tie observations on both sides, explicit
  tail exclusions leave a compact two-variable profile, or one variable
  with held location. With all off-tie observations on one side, the
  supremum is explicit, including the free/held endpoint factor. For free
  nu and a half-sample tie, the only nonzero collapse shape is nu=1; its
  extra df/scale rate factor is at most one. Uniform tail and sign arguments
  exclude further moving-alpha layers.

These are face-value and reduction theorems. Each supremum must still be
compared against every finite branch and other face before an implementation
can classify attainment, a winning escape, or a tie.

## Counterexamples and numerical evidence

For two observations −A,A with symmetric skew held, location maximization
minimizes l^4+2(D−A²)l²+(D+A²)². When A²>D there are two global maxima at
±sqrt(A²−D). Taking A=2,D=1 gives exact maxima **±sqrt(3)** for both
sstd (sd=1,nu=3,xi=1) and st (omega=1,nu=1,alpha=0).
This disproves blanket uniqueness claims for held subsets; it does not settle
all-free uniqueness.

The prior [six-point sstd example](2026-11-06-sstd-cert-data/skew-scale.json)
is strengthened to a proof of non-unimodality. Exact rational interval
arithmetic, with pi enclosed using Machin's identity and alternating-series
remainders, gives scale-score signs +,−,+,− at four ordered rational values
of sigma_u/sd. The data are the fixture's exact displayed decimals.
Continuity proves two separated local-maximum branches. This does not count
all roots or prove either branch globally optimal.

Other checks are numerical: the coordinate/density identities agreed within
1.14e−13 on the stated finite grid; analytic skew brackets had their predicted
endpoint signs; both endpoint penalties, the free-df sstd equality profile,
and free/held-location st equality paths approached their derived limits.
Seeds, exact rational enclosures, parameter grids, tolerances, versions and
results are in the reproduction files. These checks support the derivations;
they are neither interval proofs of every floating-point formula nor tests
of a new fitter.

The premise that finite-nu st shares skew-normal's location/skew singularity
at alpha=0 needs correction. Its two scores are proportional to
z/(nu+z²) and z/sqrt(nu+z²), which are not proportional to each other and
have finite second moments. Their 2×2 information block is positive definite.
This statement does not establish full four-parameter DQM or information rank,
and does not change the inference implementation.

## Concrete follow-up and open entries

The subset inventory assigns every hold combination to its exact algebraic,
scalar or compact-box route. A follow-up should first implement the sstd
fixed-shape scalar enumerators and st conditional skew bracket, then the
boundary profilers and global searches. It should preserve all candidates
and root counts, use the proved incumbent-based exterior bounds, and compare
full likelihood bounds under the actual holds. It must not reuse a local
optimizer or an uncertified parent face fitter as a global exclusion proof.

The remaining open entries are precise:

1. **Numerical certificate machinery.** Implement root isolation with
   computed real coefficients, multiple-root/critical-component handling,
   convergent compact-box bounds, and quantitative face-neighborhood
   remainders. The equations and face suprema are provided; a tested
   implementation and floating-point error analysis are not. Start with
   the library's practical 128-epsilon term-sum comparison allowance and
   existing likelihood-comparison contract, but validate margins for the
   new root counts and enclosures rather than assuming they are sufficient.
2. **Global limit-model profilers.** Certify the constrained snorm/sn,
   half-t, t2 and half-normal face optimizations needed by the inventory.
   The current numerical face calls do not complete these routes. Use the
   endpoint factors above, not an unconditional closed endpoint density.
3. **Full sample-by-sample attainment decisions.** No closed-form universal
   criterion is established beyond the stated divergence, monotonicity
   and conditional cases. Comparing the enumerated finite and boundary
   suprema is the proposed decision procedure; its implementation and
   calibration are still required. Equality is not automatically a stop
   for non-existence.
4. **All-free uniqueness and feasible cost.** No proof of unique all-free
   maximizers or useful worst-case runtime is obtained. Polynomial critical
   enumeration may be expensive for large n, and compact subdivision can
   have arbitrarily small likelihood gaps. Exact ties and degenerate
   critical sets require explicit outcomes, rather than forced success.

Every solver cap must be min(iterlim,100000), and certificate work
128 times that quantity, with the repository's separate enumeration unit
where appropriate. A budget stop retains witnesses. Uncertain coefficient
signs, unresolved likelihood comparisons or unconverged bounds stop
numerically. A capped or unchecked estimate is never licensed by these
research results. No change to settled dpqr, special-function, truth,
code-generation or binding contracts is required by this report; any new
certification-specific numerical helpers belong to the follow-up's reviewed
scope.

## Finished validation

All listed commands completed before this report was written.

| Final run | Machine | Result |
|---|---|---|
| `just cpp-test` through `simbox-run` | simbox | 106/106 dev shards passed; ctest wall time 28.16 s. |
| `just test` through `simbox-run` | simbox | FAIL 0, WARN 0, SKIP 154, PASS 20983; elapsed 60.184 s. |
| `python3 …/check.py` through `simbox-run` | simbox | All exact-sign, numerical, equality-face and 32-subset document checks passed. |
| Same final `check.py` through `run` | Workstation | All checks passed; 70-digit mpmath checks and exact rational sign enclosures. |

The R skips are dev-tier exclusions. Full R/C++, no-long-double, JS and site
suites were not required: this job changed no fitting, d/p/q/r, test
infrastructure, bindings, public fitter signatures or family dossier text.
The dev suites check the unchanged source baseline; research checks validate
the new evidence, not fitter certification. No simbox exit-75 fallback was
needed. Workstation slot waits were allowed to finish without interruption.
Simbox's wrapper supplied nice and thread settings; local computing used
its required slot wrapper with one BLAS/OpenMP thread. No command remained
running when the report was written.

An exploratory equality-limit check at nu−2=1e−9 missed its requested 1e−7
absolute tolerance because the endpoint displacement is O(sqrt(nu−2)).
Adding nu−2=1e−15 resolved that finite-cutoff issue; the final checks passed
on both machines. No theorem or library tolerance was weakened. This failure
and its correction are recorded in the validation file.

## Artifact pane

The required `show` command was attempted. The viewer returned HTTP 403:
this worktree is outside its configured readable roots. Pane access remains
open until this worktree is registered with the viewer. No configuration or
files outside the authorized worktree were changed to bypass that restriction.
The report and all evidence remain at the linked local paths.
