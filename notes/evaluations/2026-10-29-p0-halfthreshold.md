# P0 half-threshold analysis (2026-10-29)

Scope: mathematical likelihoods, finite admissible parameters, nonempty finite samples, and all proper hold subsets. `mu` in hcauchy and ht is the proposed translation. Unless otherwise stated, observations lie in the support. “Master” refers to `/tmp/umvl-p0lib`; N means not implemented, Y implemented, P partially agrees. Numerical optimization is not a proof of global optimality. `oracle.R` and `paths.R` are reproducible probes; oracle results and path probes are saved alongside the scripts in `notes/evaluations/2026-10-29-p0-halfthreshold/`.

For free thresholds of hnorm, hcauchy and ht we use the closed endpoint density of the proposed extraDistr half-family translation. Master hnorm instead maps the free threshold through dtnorm, whose endpoint is open; it returns the preceding floating-point number. If strict dtnorm support is taken literally, the supremum is not attained. Fatigue has a strictly open endpoint.

## hnorm

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | always if nonconstant; otherwise never | constant: scale collapse | P: finite predecessor of minimum, not exact mathematical maximizer under the translated closed reference |
| mu | always if mu ≤ min and some x > mu; otherwise never for all x = mu | scale collapse; mu > min invalid | Y, agrees |
| sigma | always for finite positive sigma | mu = min, including tied minima | N |

**Proved.** For fixed sigma the likelihood increases with mu up to the minimum; profiling sigma gives the stated result. Ties at the minimum pose no problem unless every observation equals it. The all-held case is evaluation, not estimation.

## hcauchy (translated)

Let k count observations at the minimum if mu is free, or at held mu otherwise.

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | always if 2k < n; never otherwise | collapse c=1: 2k>n unbounded, 2k=n finite unattained | N (free mu) |
| mu | always if mu ≤ min and 2k < n; never otherwise | same collapse; mu > min invalid | P: only mu=0 exists, agrees there |
| sigma | always | mu = min | N |

**Proved.** The likelihood increases in mu, so mu = min. The log-scale score decreases strictly from n−2k to −n for nonconstant data, establishing the unique root and both boundary cases. When the scale is held, no collapse is possible.

## ht (translated)

Let k count observations at the free or held endpoint. `normal` means the corresponding translated half-normal with the same holds and optimized remaining coefficients.

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | never without restriction; under nu ≥ 1, data-dependent for 2k≤n, unbounded if 2k>n | collapse c=nu; normal escape; at 2k=n a finite scale-zero face can also compete | N (free mu) |
| mu | never if k>0 without restriction; otherwise data-dependent; under nu ≥ 1 use the same tie cases as above | collapse when endpoint observations exist; otherwise normal escape | P: mu=0 only; agrees subject to numerical search |
| nu | always if nu(n−k)>k; never if ≤ | scale collapse, equality finite unattained | N (free mu) |
| sigma | data-dependent | normal escape as nu→∞ | N (free mu) |
| mu, nu | always if nu(n−k)>k; never if ≤ | same scale collapse; invalid if mu>min | P: mu=0 only; agrees |
| mu, sigma | data-dependent | normal escape as nu→∞ | P: mu=0 only; agrees subject to numerical search |
| nu, sigma | always | mu=min | N |

**Proved.** For each finite nu and sigma the likelihood is decreasing in x−mu, hence the free threshold is the minimum. The scale score yields the stated fixed-nu criterion. The collapse exponent is nu, and nu≥1 prevents this divergence when 2k<n. This restriction does **not** ensure an attained MLE: the half-normal face can dominate. For a free nu and fixed scale, nu→0 gives zero density even at the endpoint; with positive distances and free scale, the pointwise bound in the README excludes nu→0. A finite MLE exists when some finite nu (with profiled scale if free) attains at least the normal-face likelihood; if the normal face is strictly larger than every finite profile value, the supremum is unattained. Equality requires checking whether a finite maximizer also attains it; master uses a numerical tie tolerance and can reject such a tie. For nu≥1, if 2k=n the nu=1, sigma→0 face has log likelihood n log(2/pi)−2Σ_{x_i>mu} log(x_i−mu); compare it and the normal face with all finite candidates. At 2k>n the nu=1 path diverges. If 2k<n, no scale-zero divergence remains; with nu→∞, a simultaneous sigma→0 makes the positive-distance tail penalty dominate the polynomial endpoint gain, while sigma→∞ sends the likelihood to zero. Thus the only nondegenerate remaining face is normal. The scale-zero claim follows by splitting according as nu*sigma² stays bounded or diverges and applying log(1+t) bounds; this is an asymptotic argument rather than a certified numerical search.

## foldnorm

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | always for nonconstant nonnegative data; never for constant data | collapse at the common observation | Y; numerical profile certification |
| mu | always unless all observations equal held mu | scale collapse in that exceptional case | Y |
| sigma | always for positive held sigma | mu=0 is an attained half-normal member | Y |

**Proved.** With sigma fixed, the continuous likelihood tends to zero as mu→∞ and includes mu=0. With mu fixed, sigma→∞ gives zero; sigma→0 gives zero unless all x=mu, when it diverges. For both free, a collapsing sigma cannot fit two distinct observations and the remaining boundaries vanish; mu=0 is included. For x=0 the formula uses the continuous endpoint value.

## fatigue

Write t_i=x_i−mu>0, k for multiplicity of the minimum. Gamma(1/2) below denotes the scaled chi-square-one component of the limiting defective distribution, not a full probability density.

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | never | spike: beta→0, alpha² beta fixed, mu→min; half the mass collapses and the remainder tends to gamma(1/2) | N |
| mu | always for nonconstant t; never for constant t | constant sample collapses alpha→0, beta=t | P: only mu=0, agrees on ordinary samples |
| alpha | always for nonconstant data; never for constant data | constant: beta→0, mu→min | N |
| beta | always if 3k<n; never if 3k>n; data-dependent if 3k=n | endpoint spike with alpha² proportional to 1/(min−mu); equality has finite boundary likelihood which must be compared with finite candidates | N |
| alpha, mu | always | beta has finite optimum | N |
| beta, mu | always unless every t_i=beta; never in that exception | alpha→0 collapse | N |
| alpha, beta | always | mu has finite optimum below min | N |

**Proved paths.** With delta=min(x)−mu, beta=e, alpha²=s/e, delta=c e²/s, each minimum density grows as e⁻² and every other density tends to half a gamma(1/2) density, so the full likelihood diverges for any k≥1. The reference README gives this argument. With beta fixed and alpha²=A/delta, each minimum density is proportional to delta⁻¹ and each other density to delta¹ᐟ²; L is proportional to delta^((n−3k)/2). Equality produces a finite, nonzero boundary profile; an interior maximum must beat or attain that limit. The paths are confirmed in `paths.R`. Fixed alpha or beta alone therefore changes the mechanism; a blanket `mu`-free ban would discard potentially estimable cases. With fixed beta, profiling gives alpha² = mean(t/beta + beta/t − 2). As delta→0 its likelihood behaves as above; as mu→−∞ it tends to zero. For 3k<n and nonconstant observations it is continuous, vanishes at both ends and attains a maximum. For 3k=n, the alpha-profiled finite edge log likelihood is

    k (0.5 log(beta/A) − log(2) − 0.5 log(2*pi) − beta/(2*A))
    + sum_{i:x_i>min(x)} [log(d_i+beta) − 1.5 log(d_i) − 0.5 log(beta*A) − log(2) − 0.5 log(2*pi)],

where A=k beta/n and d_i=x_i−min(x). A finite maximizer exists if the profiled curve has an interior point above this value; a tie requires checking attainment. Numerical examples with beta=1: (0,1,1) has an interior maximum −2.353 versus edge −3.302; (0,.1,1) has edge −0.446 and best sampled local maximum −1.357 (a grid check, not a global certificate).

With alpha held positive, an endpoint approach and nonconstant data incur an exponential penalty involving beta/delta + D/beta, D>0; its minimum is of order delta⁻¹ᐟ² and beats all polynomial density factors. The other parameter-space tails also give zero likelihood. Thus the maximum is attained for nonconstant samples. With mu fixed, profiling alpha and differentiating in beta shows that beta→0 and beta→∞ yield half-gamma and half-inverse-gamma defective faces but the finite profile increases away from each face; they cannot be global maxima. Constant samples collapse at alpha→0, beta=t.

A further free-parameter escape is **normal**: mu=−M, beta=M+m, alpha=s/M, M→∞ gives N(m,s²). It survives any fixed upper bound on alpha, so such a bound removes the displayed alpha→∞ endpoint spikes but does not by itself ensure compactness or rule out an unattained supremum. In `fatigue-normal-results.csv`, a left-skew sample under alpha ≤ 0.5 approaches the normal face with mu≈−3967 and alpha≈0.00031; the search is numerical, not a proof that this face globally dominates. A two-sided shape restriction 0<a ≤ alpha ≤ A<∞ does restore existence for nonconstant samples: uniformly over that compact alpha interval, the endpoint and beta-zero penalties just described exclude mu→min and beta→0; the density also vanishes uniformly when beta→∞ or mu→−∞. Constant samples can still collapse with beta→0 and mu→min. No weakest positive a or finite A exists, so choosing one is a modelling decision. A lower bound on beta alone does not remove the fixed-beta path. Fatigue support is strictly open; holding mu at or above the minimum is invalid.

## betapr

Write a=shape1, b=shape2, s=scale. Assume all x>0. `gamma(a)` is the gamma face with shape a fixed when a is held, otherwise optimized; `invgamma(b)` analogously. To test existence, for each positive s maximize over the unheld shape(s), then compare that one-dimensional scale profile with the independently maximized indicated face(s). A finite scale where the profile is strictly above every face gives an attained MLE; if every finite value is strictly below the best face, the MLE is absent. If the supremum ties a face, check whether a finite scale also attains it. These are implicit, computable conditions, not a closed-form sample statistic.

| Held | Class and condition | Limit/mechanism | Master |
|---|---|---|---|
| none | data-dependent for nonconstant samples; constant never | gamma (b,s→∞ with s/b fixed), inverse-gamma (a→∞, s→0 with as fixed); constant collapse | P: face comparison, tolerance and numerical search |
| a | data-dependent | gamma(a); no constant-data collapse with a held | N: shape1 is silently ignored, not held |
| b | data-dependent | invgamma(b); no constant-data collapse with b held | N: shape2 silently ignored |
| s | always for nonconstant data; never for constant data | beta transformed-data collapse | Y |
| a, b | always | unique scale score root | N: shapes silently ignored |
| a, s | always | unique b score root | N: shape1 silently ignored |
| b, s | always | unique a score root | N: shape2 silently ignored |

**Arguments.** At fixed scale, x/(s+x) is beta data, so strict concavity in the two shapes gives a unique maximum for nonconstant samples. With a held, the only nondegenerate open face is gamma(a); with b held it is inverse-gamma(b). Both shapes tending to infinity collapse to a point, not a lognormal. The beta transformation and gamma-ratio representation are stated in [VGAM beta-prime documentation](https://search.r-project.org/CRAN/refmans/VGAM/html/betaprime.html); the gamma and inverse-gamma limits are stated in [Crooks, Field Guide to Continuous Probability Distributions, §13](https://threeplusone.com/pubs/FieldGuide.pdf). For both held, the negative log-scale score is a n−(a+b) Σx/(s+x), strictly increasing from −bn to an and hence has one root. For one shape and scale held, the beta log-likelihood is strictly concave in the remaining shape and its score crosses zero. The closure/face completeness for the free-scale cases is an argument from asymptotics, not an independently certified global maximum theorem; the practical finite-versus-face condition needs a globally reliable optimizer.

## Parameter kinds, restrictions and reference domains

The proposed `threshold` kind for hnorm, hcauchy, ht and fatigue mu agrees with their translation semantics. Foldnorm mu is a parent-normal magnitude, not a threshold; `regular` is appropriate even though its domain includes 0. Other coefficients are regular. The design’s ht restriction has collapse exponent nu and applies when mu and sigma are free. If sigma is free and held mu happens to coincide with observations, the same bound also prevents collapse for ties below n/2; however this is a probability-zero sample under a continuously distributed fixed-threshold model, so R5 does not itself require the restriction there. Such samples still need a diagnostic under R6. It is unnecessary with sigma held or with held mu strictly below the data. It prevents collapse for minimum multiplicity below n/2, but does not prevent the normal escape. No hnorm or hcauchy shape restriction is available or necessary for the stated tie condition. For fatigue, an alpha upper bound removes the explicit spikes but leaves the normal escape; a compact positive alpha interval restores existence on nonconstant samples, but has no weakest endpoints. Holding mu is a proven way to remove the three-parameter endpoint path.

R7: `extraDistr::dht(..., nu=Inf)` returns the proper half-normal density (checked numerically), although the README says nu ∈ (0, Inf) and master treats infinity as an escape. If R7 includes the reference's extended-real value, it is an attained endpoint and changes all half-normal escape classifications. This is a design decision requiring resolution, not silently assumed here. Zero and negative shape/scale inputs tested return NaN. Infinite scales in some reference functions return zero density and are not proper probability laws. Foldnorm is own density, and its mu≥0 is the identified parameterization rather than the signed parent-normal domain.

## Proposed README YAML

Exact `holds` semantics in the design name the *held subset*, so data-dependent failures belong in `limits`/diagnostics, not unconditional `no-mle` entries. The all-free fatigue subset is universally no-MLE; the other unconditional exclusions do not apply across all valid samples. These entries describe master implementation gaps, including the proposed translated coefficients; the `mu`-held hcauchy/ht and fatigue entries cover all held values, while master supports only its implicit zero.

```yaml
# hnorm
params:
  - {name: mu, kind: threshold}
holds:
  - {held: [sigma], status: not-implemented, reason: held sigma is not accepted}
---
# hcauchy (after adding mu)
params:
  - {name: mu, kind: threshold}
holds:
  - {held: [], status: not-implemented, reason: translated free threshold unavailable}
  - {held: [mu], status: not-implemented, reason: only implicit mu=0 is supported}
  - {held: [sigma], status: not-implemented, reason: held sigma and translated threshold unavailable}
---
# ht (after adding mu)
params:
  - name: nu
    fit_domain:
      lower: 1
      restriction: {mechanism: collapse, while_free: [mu, sigma], reason: 'nu >= 1 prevents collapse for minimum multiplicity below n/2'}
  - {name: mu, kind: threshold}
holds:
  - {held: [], status: not-implemented, reason: translated free threshold unavailable}
  - {held: [mu], status: not-implemented, reason: only implicit mu=0 is supported}
  - {held: [nu], status: not-implemented, reason: translated free threshold unavailable}
  - {held: [sigma], status: not-implemented, reason: translated free threshold unavailable}
  - {held: [mu, nu], status: not-implemented, reason: only implicit mu=0 is supported}
  - {held: [mu, sigma], status: not-implemented, reason: only implicit mu=0 is supported}
  - {held: [nu, sigma], status: not-implemented, reason: translated free threshold unavailable}
---
# foldnorm: no non-regular kinds, restrictions or unsupported proper holds
holds: []
---
# fatigue: no numerical alpha bounds are proposed without a modelling decision.
# A candidate fit_domain alpha interval [a, A], 0 < a <= A < Inf,
# has restriction: {mechanism: spike, while_free: [mu], reason: bounded alpha excludes endpoint and normal escapes on nonconstant data}.
params:
  - {name: mu, kind: threshold}
holds:
  - {held: [], status: no-mle, reason: 'free threshold and both free parameters give unbounded endpoint spike'}
  - {held: [mu], status: not-implemented, reason: only implicit mu=0 is supported}
  - {held: [alpha], status: not-implemented, reason: holding alpha and estimating threshold unavailable}
  - {held: [beta], status: not-implemented, reason: holding beta and estimating threshold unavailable}
  - {held: [alpha, mu], status: not-implemented, reason: holding alpha unavailable}
  - {held: [beta, mu], status: not-implemented, reason: holding beta unavailable}
  - {held: [alpha, beta], status: not-implemented, reason: estimating threshold unavailable}
---
# betapr
holds:
  - {held: [shape1], status: not-implemented, reason: shape arguments are ignored}
  - {held: [shape2], status: not-implemented, reason: shape arguments are ignored}
  - {held: [shape1, shape2], status: not-implemented, reason: shape arguments are ignored}
  - {held: [shape1, scale], status: not-implemented, reason: shape arguments are ignored}
  - {held: [shape2, scale], status: not-implemented, reason: shape arguments are ignored}
```

The schema’s single `while_free: [mu, sigma]` follows the design’s almost-every-sample rule, but does not prevent collapse when mu is held exactly at an observation. Those exceptional samples require an R6 diagnostic; applying the bound there would need a data-conditional rule or a deliberately stronger restriction. Fatigue's all-free no-MLE entry is unconditional for valid samples, unlike the tie-dependent fixed-beta subset.

## Numerical checks and limits of the evidence

On simbox, `oracle.R` evaluated 323 family/sample/hold combinations with four `nlminb` starts, transformed coordinates, endpoint candidates and separately optimized half-normal, gamma and inverse-gamma faces. Samples include n=2, 3, 5, 20, 100 and 1000 from the README simulation parameters, ties, constant samples, endpoint observations and each identified nondegenerate limit law (including fatigue's normal, defective gamma and defective inverse-gamma components). The output `oracle-results.csv` records estimates, convergence, faces, package estimates and diagnostic messages. For folded normal the oracle evaluates the README formula directly; it matched the package density at every sampled point, maximum absolute log-density difference 0. `probes.R` saves the singular paths, reference domain probes, held-threshold placements and restricted half-t cases; `fatigue-profile.R` gives an independent one-dimensional alpha-profile check, and `fatigue-normal.R` probes the additional normal face under bounded alpha.

All 108 returned master fits agree with the reference-density oracle within `1e-6*n`; the largest absolute difference was 1.01e-11. The per-subset counts below include package diagnostics, not just successful fits. A diagnostic selecting an open face was checked against the independently computed face; finite optimizer values approach the face from below in these cases. Some unsupported combinations and boundary candidates are included in the 323 rows.

| family | implemented holds; cases / returned fits | max absolute log-likelihood difference per subset |
|---|---|---|
| hnorm | none 11/10; mu 11/11 | 0; 1.03e-14 |
| hcauchy | mu=0 11/10 | 0 |
| ht | mu=0 11/4; mu=0,nu 11/11; mu=0,sigma 11/7 | 9.95e-13; 1.01e-11; 0 |
| foldnorm | none 11/10; mu 11/11; sigma 11/11 | 1.01e-13; 6.41e-13; 0 |
| fatigue | mu=0 11/10 | 1.01e-11 |
| betapr | none 10/4; scale 10/9 | 6.93e-12; 1.40e-12 |

The numerical search has bounded log coordinates and is not a global certificate; 29 of the 323 best runs report `nlminb` nonzero convergence and several represent singular or asymptotic paths. Near huge shapes, reference log densities can lose precision. Finite-versus-face decisions near equality require a more accurate certified search. The beta-prime closure and the claim that an alpha upper bound alone removes every unbounded fatigue path are not fully proven here, so the corresponding classification/restriction retains that qualification. A restricted-half-t case with exactly half the data at the endpoint also requires explicit comparison of its finite scale-zero face, normal face, and finite candidates. The independently saved path results substantiate the qualitative limits, not an existence proof.
