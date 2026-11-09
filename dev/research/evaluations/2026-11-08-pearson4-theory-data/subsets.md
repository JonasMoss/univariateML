# All 16 free subsets of Pearson IV

Read with [proofs.md](proofs.md). F lists free coefficients; its complement is
held at finite proper values. L=location λ, S=scale α, M=m; skew is written ν.
Every held m>1/2 is accepted mathematically. For free m this inventory
recommends m≥1 if L,S are both free, and m>1/2 otherwise (proved exemptions;
implementation-policy adoption is a separate decision). No package defaults
are changed. n>0; empty/nonfinite input is an input failure.

`Compare B` has a precise meaning: optimize the listed accessible excluded
faces as well as every finite candidate. A finite maximizer exists iff the
maximum over the compactified closure is achieved at an eligible finite
point. A strict interior improvement proves this; a proved strict face win
proves nonattainment; equality needs an attained-point decision. This is a
proved necessary-and-sufficient variational characterization, **not** an
available exact sample predicate. Open equality decision E and normal-face
inference N are specified in proofs.md. Numerical ties must stop unresolved.

For fixed m define e_j=(2m−1)(n−k_j)−k_j. With L held use only
k=count(x=L); k=0 if the location is unobserved. With L free use every
observed multiplicity. The fixed-shape equality face is equation (1),
with h=0 for held L and h=ν/(2m) for free L. For free ν include its
infinite endpoints (3). For free m≥1 half-tie faces lie at m=1.
`Normal(c free)` means both mean and positive standard deviation are free;
`Normal(c=L)` holds the mean, not the Pearson root centre of a finite fit.
V denotes a reflected inverse gamma, not the skew coefficient in this table.

| # | Free F | Attainment, unboundedness and every eligible nonzero face | Search route |
|---|---|---|---|
| 0 | ∅ | Pure positive full-support density evaluation. No optimization or escape. | Log density with bounded numerical error. |
| 1 | L | Always attained for any sample and arbitrary held shapes. No escape face. Uniqueness fails, even at m=1,ν=0. | Isolate all roots of degree ≤2n−1 location polynomial on [xmin+αν/(2m),xmax+αν/(2m)]. |
| 2 | S | e<0: unbounded. e>0: attained. e=0: compare the collapse face with all positive stationary roots. If held ν=0, or every noncentral ν(x_i−L)≤0, face strictly dominates every finite scale, so no MLE. A positive coefficient in (2) proves finite improvement for skewed equality cases. No V/normal. | ν=0 monotone lst score; otherwise degree ≤2n polynomial isolation, finite candidates plus α=0 face. Equality E only if comparison cannot be resolved. |
| 3 | M | With L,S,ν held, all x=L gives unbounded shape concentration; otherwise a unique finite m maximizes on m>1/2. m↓1/2 gives zero likelihood. No proper excluded face. | Strict scalar concavity, bracketed score. If a restriction m≥1 is retained by policy, its endpoint is an attained candidate. |
| 4 | ν | Always a unique finite maximum, including constant samples. ν→±∞ loses likelihood with all nuisances held. | Strict concavity; decreasing shape score whose expectation runs between the angular endpoints. |
| 5 | L,S | Any e_j<0: unbounded. All e_j>0: attained, including held m<1 when sample multiplicities permit. At equality compare every collapse face. No V/normal since m,ν held. At m=1,ν=0 and half ties, exactly two balanced clusters have a continuum of finite MLEs; at least three distinct values give the unattained face. At m=1,ν≠0, a half-tie at the maximum for ν>0 or minimum for ν<0 gives nonattainment, including balanced two-cluster data. | Rational two-dimensional scores or exhaustive boxes; retain positive-dimensional stationary sets. Other equality cases E. |
| 6 | L,M | S,ν held. Constant data: unbounded m∞ concentration with L→t. Every nonconstant sample: attained. m↓1/2 vanishes; α held and ν held forbid normal and V. No tie threshold needed. | Concave conditional m fit, location root proposals, compact boxes; m∞ concentration excluded for nonconstant data using (5) with sufficiently large M. |
| 7 | L,ν | S,m held. Always attained, including constant samples. No V/normal or collapse face. Uniform peak decay excludes |ν|∞ even with L tracking the mode. | Profile the unique conditional ν fit; finite two-dimensional boxes or profile derivative bounds. No profile unimodality claimed. |
| 8 | S,M | L,ν held. Any x=L: unbounded (choose m near 1/2 and collapse S). With no x=L: compare Normal(c=L), standard deviation free. Lower shape end vanishes uniformly by (11), and finite-m collapse vanishes. Constant samples away from L belong to this no-tie case and have positive normal residual variance. No V. | Shape concavity conditional on S, compact c/w chart with exact hold constraints, normal-face likelihood (10). |
| 9 | S,ν | L,m held. e<0: unbounded. e>0: compare V with threshold t=L, shape a=2m−1, either orientation; only a side containing all observations has nonzero V likelihood. At e=0 also compare sup_ν F_L(m,ν), attained at finite ν since its infinite-skew limit is zero for held L and k>0. No normal. | Concave ν profile, scalar/2D boxes and V/collapse face comparisons. E in unresolved ties. |
| 10 | M,ν | L,S held. Constant sample at **any** t: unbounded m∞, ν/(2m)=(L−t)/S. Nonconstant sample: unique finite maximum. No proper excluded face; lower shape end and divergent skew vanish; simultaneous m,ν∞ has zero width at bounded mode. | Strict joint shape concavity, escaping concentration diagnosed separately. |
| 11 | L,S,M | ν held; free m≥1. 2k_max>n: unbounded at m=1. 2k_max<n: compare Normal(c free). Half ties: also compare F_t(1,ν) for each tied t, including the interacting m→1 collapse rates. No V, since finite held ν forbids it. Constant samples are unbounded. | Fixed-shape rational proposals, free-shape compact boxes with normal and m=1/collapse boundaries. E in equality decisions. |
| 12 | L,S,ν | m held. Any e_j<0: unbounded. All e_j>0: compare V with shape a held and threshold free, either orientation. e_j=0: also compare sup_ν F_t(m,ν), including (3). No normal with fixed m. At m=1 a half-tie at either extreme is proved nonattainment with free skew, including balanced two-cluster data. | Compactified k chart, rational nuisance proposals, concave ν conditionals, V threshold search. E in equalities. |
| 13 | L,M,ν | S held, m>1/2. Constant sample: unbounded shape concentration. Nonconstant sample: compare Normal(c free), accessible through L∼√m,ν∼m^(3/2). V forbidden by S held. No finite-m scale collapse; no multiplicity guard required beyond nonconstancy. | Concave shape conditionals, boxes constrained by α held; include the normal face even though the original scale is held. |
| 14 | S,M,ν | L held, m>1/2. Any x=L: unbounded by small-m scale collapse. With no x=L: compare V with t=L, a free>0, and Normal(c free). In particular held L does **not** fix the normal mean if ν is free. Lower shape end vanishes by (11). Constant samples away from L are unbounded by m∞ shape concentration, overriding the face comparison. | Exact hold-constrained compactification, concave shape conditionals, V and normal boundary searches. |
| 15 | L,S,M,ν | m≥1. 2k_max>n: unbounded. 2k_max<n: compare V with a≥1, threshold free and either orientation, and Normal(c free). Half ties: also compare sup_ν F_t(1,ν), including both infinite-skew endpoints and m/skew/scale joint rates. Constant samples unbounded. | Full compactified four-dimensional certificate with separate m=1, V, normal and collapse strata. E in unresolved equalities. |

All proper V faces require every y_i=s(x_i−t)>0; a data point at the
threshold makes ℓV=−∞. A free threshold lies strictly outside the sample
range; both orientations must be tried, with threshold escapes to infinity
and shape→∞ completing the normal corner when eligible. Constant-sample
V concentration is also unbounded when shape and threshold/scale freedom
allow it. It is never a checked finite V optimum.

If the package retains m≥1 for every free-m subset by the broad R5 policy,
rows 3,6,8,10,13,14 need this modification: m=1 is eligible and flagged;
small-m unboundedness in rows 8,14 is replaced by the fixed-minimum
collapse test at m=1. At held L, ties k>n/2 give unboundedness, k<n/2
have the same proper faces above, and k=n/2 adds F_L(1,ν). Constant-sample
shape spikes still apply. This alternative is stated explicitly so that an
exemption recommendation cannot silently change the statistical problem.

There is no positive-likelihood gamma boundary in any row. m↓1/2 is an
improper/zero-likelihood end outside the spike regimes, never an output
parameter. Divergence of L,S,ν singly is insufficient analysis: the
hold-dependent normal constructions, νS finite V paths, and their collapse
corners are all included above. No sample-dependent failure belongs in
`holds:`; A7 permits only structural exceptions, and none of these 16
subsets is structurally impossible for every sample.
