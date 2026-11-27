# Held-parameter likelihoods in Lean

TASK-394 · 2026-11-27

Lean checked 53 public theorems indexed by 124 fragment rows.
The singleton theorem covers all 72 atlas regimes with every coefficient held.
The fragment fills criterion, failure and uniqueness coverage for those 72
rows under this task's singleton exception. The other rows carry theorem
statements and scope notes, with coverage left for atlas review.
[Atlas fragment](../existence-atlas/lean-map.d/lean-heldsweep.csv)

## Source readings and discrepancies

- The beta-negative-binomial dossier explicitly excludes a general labeled
  singleton claim at held alpha. It gives a strict diagonal exclusion on
  the golden sample using finite numerical comparisons with a stated margin.
  The Lean theorem discharges the attainment and symmetry steps from a
  strict diagonal exclusion hypothesis. A proof of the computed comparisons
  is still needed to obtain that concrete Lean counterexample.
  [Dossier](../../../univariateML-cpp/include/cppunivariate/distributions/bnbinom/README.md#alpha-held-pair-certificate)
  · [Diagonal exclusion](2026-11-06-bnbinom-cert-report.md#concrete-failure-of-labeled-uniqueness)
- The cited CMB report concerns Conway–Maxwell–binomial likelihoods. Its
  unrestricted-size analysis uses CMP as a limiting family. The exact
  held-rate CMP classification comes from the CMP dossier. The rate-zero
  theorem describes the real likelihood on the fiber, including the
  identically zero objective on incompatible data. The fitting interface
  rejects those incompatible samples.
  [CMB report](2026-11-25-cmb-math-report.md#unrestricted-integer-size)
  · [CMP dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md)
- At Bhattacharjee range equality the two endpoint weights share a single
  free probability. With free centre, the optimal weight is the lower
  endpoint frequency among endpoint observations. With centre held, every
  endpoint weight is one half. The compatible parent likelihood and the
  closed uniform likelihood therefore have different endpoint values.
  [Bhattacharjee report](2026-11-25-bhatt-math-report.md#existence-and-the-faces)
- Zero-shape PIG is an attained atom on all-zero data. With positive shape
  held, mean zero lies outside the domain and gives an unattained
  supremum. The binary positive-data fit is written in the Bessel gap
  coordinate, which is bijective with the positive mean at fixed shape.
  [PIG report](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask)
- The GEV statements at shape minus one concern the attained
  reverse-exponential candidate. The half-contact path at shape one
  reaches the value given by the source. Deciding whether the full
  held-location fit attains that value requires an envelope over all
  shapes, including the open-to-closed support transition at minus one.
  [GEV report](2026-11-25-gev-math-report.md#existence-and-faces-under-each-single-hold)

## Proof effort and remaining obligations

| Target | Proof work and result | Open |
| --- | --- | --- |
| 1. Singleton domains | One order-theoretic argument gives feasibility, unique maximality and the supremum. The atlas domain keys were compared with each of the 72 empty free-coordinate masks. | None for mathematical singleton domains. |
| 2. CMP | The proof uses dominated convergence for the Bernoulli path, a strict partition comparison on binary data, and strict convexity of the centered partition on nonbinary data. When the geometric endpoint is excluded, finite partial sums force divergence at dispersion zero. | A characterization of the nonbinary optimizer by its moment score is a separate result. |
| 2. Skellam | The public uniform tail bound gives attainment on each half-line. A Cauchy product and Vandermonde's identity give a strict Turán inequality. Strict log likelihood concavity proves uniqueness, including the rate-zero boundary when its support permits the data. Reflection handles the other hold. | The separate signed-Poisson problem when the *held* rate is zero lies outside the requested positive-rate target. |
| 2. PIG | Rationalizing the gap gives the all-zero path and strict bound. A square identity gives the fixed-shape chart bijection. In the binary case an explicit strictly decreasing gap score has opposite endpoint signs. | General positive data with a count at least two require the log-polynomial derivative identities on the held-shape chart and the decrease comparison from the degree-variance bound. The existing Bessel module exports the variance inequality but keeps the coefficient positivity, degree-mean bounds and derivative identities private. |
| 2. Grouped normal | The working log likelihood is restricted to affine lines for uniqueness. The normalized CDF likelihood supplies compactness on the finite branches and the strict unattained zero-scale height on the contact branch. | A public Gaussian-integral identity for the existing working cell mass is needed to formally transfer the working-log-likelihood uniqueness result to the normalized CDF objective. The existing cell-integral conversion is private. |
| 2. Beta-negative binomial | Existing compact attainment plus size–beta symmetry gives labeled nonuniqueness from an exact diagonal exclusion. Existing scalar maxima and all-zero paths also give complete equivalences for the three two-hold masks. | A verified bound over the diagonal for the concrete golden sample, from zero through infinity. A separate global comparison is needed for any claim about uniqueness after ordering size and beta. |
| 2. Bhattacharjee | The existing complementary envelope specializes to the midrange. Factoring the endpoint trace reduces its peak to a binomial fit. At held centre the strict CDF inequalities give the exact half-mass penalties. The ordinary normal log likelihood gives an exact argmax transfer at width zero. | Full positive-width held-scale and free-width comparisons in the atlas are broader claims than these equality and normal-member results. |
| 2. Discrete Weibull | Four pointwise limits follow by tracking the two adjacent survivals on each held fiber. All are stated in the established log-negative-log probability chart. | None for the four requested pointwise laws. The positive-data scalar attainment predicates already have separate declarations in the source module. |
| 2. GEV | Direct reverse-exponential comparisons solve the fixed-scale and specified fixed-location subproblems at shape minus one. At held sample minimum the shape-one expansion gives the majority divergence and half-contact finite path. | The global held-scale compactness and upper semicontinuity on the shape interval, held-location sub-half confinement and the sharp half-contact envelope and equality set, and the two global tie constructions. The fixed-location reverse-exponential result assumes the held location is below the maximum and at most the sample mean. |
| 2. Gompertz | Held-growth and held-hazard tails follow from the logarithm and exponential growth estimates. The growth-zero path identifies the exponential rate exactly. The existing all-zero divergence is `UnivariateML.Gompertz.heldRate_zero`. | None for the named held paths. The existing `heldHazard_face` also supplies the negative-score comparison. |
| 3. Other scalar masks | Affine inverse coordinates give the logistic and Gumbel hold fits and constant collapse. The Double Pareto score and the exact exchange of its two rates give the two held-kink masks. The closed negative-binomial seam is evaluated for each hold. | The remaining written scalar obligations are listed below. |

## Other written scalar obligations

The following index lists one-free-coordinate atlas regimes with written
claims outside the scope of the new results. Some already have individual
Lean ingredients in the source modules, and some require additional case
splits or comparisons.

| Family | Held | Written claim requiring coverage at its stated scope |
| --- | --- | --- |
| `bbinom` | `alpha+size` | For known size ≥1 and positive held alpha, a finite beta exists exactly when neither endpoint is the common count. At size zero attainment is automatic. Constant endpoint selects the corresponding point mass; at size zero all beta values tie. |
| `bbinom` | `beta+size` | For known size ≥1 and positive held beta, a finite alpha exists exactly when neither endpoint is the common count. At size zero attainment is automatic. Constant endpoint selects the corresponding point mass; at size zero all alpha values tie. |
| `betapr` | `shape1+shape2` | Always for positive data. No failure. Unique. |
| `bhatt` | `a+mu` | For positive held a, iff maximum absolute residual exceeds a. At a=0, iff some residual is nonzero. Uniform envelope wins when positive a covers every observation. At a=0 zero residuals yield collapse. Unique when attained. |
| `bhatt` | `a+sigma` | Always for positive held sigma and valid a. No failure. Unique location. |
| `bhatt` | `mu+sigma` | Unique normal iff the held-centre second moment does not exceed the held variance. |
| `burr` | `scale+shape1` | Exactly when some observation differs from held scale. Otherwise divergence. Unique free power. |
| `cmb` | `nu+prob` | Always for feasible samples on the capped trial-size domain. No escape when only a finitely capped integer coefficient is free. All sizes tied for the largest finite likelihood; at dispersion zero and interior probability, the smallest feasible size is unique. |
| `cmb` | `nu+size` | Always on data compatible with the fixed size. No escape; probability endpoints are included. Unique probability for positive size. At size zero any probability maximizes. |
| `cmb` | `prob+size` | At fixed N≥2 and interior p, iff the empirical log binomial coefficient lies strictly between its extrema over counts. At held boundary p or size zero or one, all compatible samples attain. An extremal empirical log binomial coefficient selects an excluded centered or endpoint face at interior probability and N≥2. Unique at interior score root. In the flat size-zero, size-one and boundary probability regimes every dispersion maximizes. |
| `cmp` | `nu` | Always for admissible data and held finite dispersion. No escape. Unique rate, including zero if all observations are zero. |
| `expweibull` | `exponent+scale` | Exactly when some observation differs from held scale. Otherwise unbounded concentration. Unique free shape. |
| `expweibull` | `exponent+shape` | Always for positive samples and holds. No failure. Unique free scale. |
| `fatigue` | `alpha+mu` | For every nonempty strictly positive excess sample at finite positive held alpha, scale attains. The scale-end penalties in the positive excess likelihood rule out both scale escapes. For constant samples a singleton occurs at alpha at most two and a reciprocal pair at alpha above two. |
| `fpareto` | `scale+shape1+shape2+shape3` | For finite nonempty compatible observations, exactly when the held lower power a is at least one. At a<1 the sample minimum supplies an unbounded threshold spike. Unique threshold. At a=1 it touches the minimum; above one the normalized score has one interior root. |
| `frechet` | `lambda+sigma` | Always for admissible held shape and scale. No escape. Unique location. |
| `frechet` | `mu+sigma` | Exactly when some strictly positive excess differs from held sigma. Otherwise unbounded likelihood. Unique. |
| `gb2` | `scale+shape1+shape2` | Always on positive samples. No failure. Unique free beta shape. |
| `gb2` | `scale+shape2+shape3` | Always on positive samples. No failure. Unique free beta shape. |
| `ged` | `mean+sd` | Off exact real width contact a maximum exists; at contact compare all finite candidates against the held-mean endpoint trace. The uniform can win only at exact contact. |
| `ged` | `nu+sd` | Always for positive held scale and shape. No escape. Unique above one, median interval at one, and precisely the minimizing knots below one. |
| `genbeta` | `scale+shape1+shape3` | Always on strictly interior data. No failure. Unique. |
| `genbeta` | `scale+shape2+shape3` | Always on strictly interior data. No failure. Unique. |
| `gengamma` | `Q+mu` | Exactly when some log observation differs from held mu. At zero residuals scale collapse is unbounded. Unique scale at every finite held Q. |
| `gengamma` | `Q+sigma` | Every nonempty positive sample attains. No escape. Unique location at fixed scale and shape. |
| `gengamma` | `mu+sigma` | Always on finite positive samples and admissible holds. No escape: the likelihood tends to zero at both signed shape ends. If every log observation equals held mu, the unique shape is Q=0. |
| `ht` | `mu+nu` | Exactly when exponent is positive for a compatible held endpoint. Equality gives a finite unattained collapse and a negative exponent gives divergence. |
| `invburr` | `scale+shape1` | Exactly when some observation differs from held scale. |
| `invparalogis` | `scale` | Reciprocity transfers the exact held-scale tie to inverse paralogistic. |
| `nbinom` | `prob` | Transfer the positive-data fit from the open log domain to the full likelihood with its degenerate endpoints. |
| `nbinom` | `size` | Transfer the positive-data fit from the open log domain to the full likelihood with its degenerate endpoints. |
| `nhyper` | `N+m` | Exactly when the feasible finite stopping-count set is nonempty. No escape within a finite known urn. |
| `nhyper` | `N+r` | Exactly when composition count has a feasible value in the held population and stopping count. No escape within a finite known urn. |
| `nhyper` | `m+r` | Always on feasible data. No escape; for positive held stopping count the likelihood along the ray goes to zero. For positive held stopping count, the crossing yields one or two adjacent integers. With r=0 all feasible urns maximize. |
| `nsbeta` | `max+shape1+shape2` | The free-end shape is shape1 and the held-end shape is shape2. Exactly when the farthest distance from the held endpoint is positive, the fixed free-end shape is at least one, and held-end factors allow positive likelihood. A free-end shape below one gives a spike; all observations at the held end with held-end shape one concentrate as width tends to zero. A zero held-end factor or incompatible observations preclude positive likelihood. Unique width; it is the farthest distance for free-end shape one and the unique interior score root above one. |
| `nsbeta` | `min+shape1+shape2` | The free-end shape is shape2 and the held-end shape is shape1. Exactly when the farthest distance from the held endpoint is positive, the fixed free-end shape is at least one, and held-end factors allow positive likelihood. A free-end shape below one gives a spike; all observations at the held end with held-end shape one concentrate as width tends to zero. A zero held-end factor or incompatible observations preclude positive likelihood. Unique width; it is the farthest distance for free-end shape one and the unique interior score root above one. |
| `paralogis` | `scale` | A real two-point sample with integer multiplicities has two distinct maximizing shape bands for the fixed held scale. |
| `pareto2` | `scale+shape` | For finite nonempty compatible observations, exactly when the held lower power a is at least one. At a<1 the sample minimum supplies an unbounded threshold spike. Unique threshold. At a=1 it touches the minimum; above one the normalized score has one interior root. |
| `pareto3` | `scale+shape` | For finite nonempty compatible observations, exactly when the held lower power a is at least one. At a<1 the sample minimum supplies an unbounded threshold spike. Unique threshold. At a=1 it touches the minimum; above one the normalized score has one interior root. |
| `pareto4` | `min+scale+shape1` | Exactly when some positive excess differs from held scale. |
| `pareto4` | `scale+shape1+shape2` | For finite nonempty compatible observations, exactly when the held lower power a is at least one. At a<1 the sample minimum supplies an unbounded threshold spike. Unique threshold. At a=1 it touches the minimum; above one the normalized score has one interior root. |
| `pgw` | `exponent+scale` | Exactly when some observation differs from held scale. |
| `pig` | `mean` | Join the prior positive-count profile with the all-zero argmax restriction at the held mean. |
| `sged` | `mean+nu+xi` | Exactly when at least one residual differs from held mean at finite positive held shapes. At zero residuals scale collapse is unbounded. Unique scale from the power loss profile. |
| `sged` | `nu+sd+xi` | Always at positive held sd and valid finite shapes. No escape in location due to the density tails. Above shape one the location maximum is unique. |
| `sgomp` | `eta` | Exactly when sample sum is positive. |
| `slash` | `mu` | At equality scale zero gives a finite supremum; above half it diverges. |
| `slash` | `sigma` | Always for positive held scale. No failure. Symmetric separated samples can give multiple location maxima. |
| `sn` | `alpha+omega` | Always on nonempty finite samples. No failure. Unique. |
| `sn` | `alpha+xi` | Exactly when at least one observation differs from held location. Zero residuals collapse without bound. Unique. |
| `sn` | `omega+xi` | Exactly when both strict residual signs occur or all residuals are zero. A single strict residual sign yields an excluded half-normal envelope, penalized at zero residuals. Unique with both signs. At zero residuals every finite slant maximizes. |
| `snorm` | `mean+sd` | Exactly when an admissible finite stationary value reaches the larger compatible half-normal trace, or no limiting face is feasible. Boundary comparisons preserve held mean and sd and penalize exact endpoint ties. All finite stationary or constant intervals and included joins at the highest value. |
| `snorm` | `mean+xi` | Exactly when some residual from held mean is nonzero. All residuals zero give unbounded collapse. Unique scale. |
| `snorm` | `sd+xi` | Always on finite nonempty data for valid holds. No escape. Unique mean. |
| `sstd` | `mean+nu+xi` | On compatible held shapes and location, the symmetric case has a monotone scale score; strict exponent inequalities settle divergence or confinement more generally. Negative exponent gives unbounded scale collapse; symmetric equality gives an unattained finite face. |
| `st` | `alpha+nu+xi` | With finite held slant and df and compatible location, the finite one-dimensional scale maximum exists in the strict positive-exponent regime. At exact zero exponent, attainment iff a finite value reaches the scale-zero edge. Negative exponent gives divergence; zero exponent gives a finite scale-zero envelope that competes with finite fits. |
| `triang` | `a+b` | No escape in the closed mode interval. If both held endpoints are occupied every mode has zero likelihood and the fitting API rejects the data. |
| `triang` | `a+c` | Exactly on compatible data except a constant at the common held endpoint and mode. Compatible collapse at a common held endpoint and mode can diverge. |
| `triang` | `b+c` | Exactly on compatible data except a constant at the common held endpoint and mode. Compatible collapse at a common held endpoint and mode can diverge. |


The broader multi-coordinate held profiles and exact transfers in the
[coverage inventory](2026-11-27-atlas-coverage-report.md#remaining-written-claims-by-family-and-proof-requirement)
remain at their prior scope except where a theorem below proves a new
case. In particular, the Double Pareto one-rate profiles with **kink free**
still require the finite knot comparisons and the competing one-sided
face comparison from the source report.

## Lean verification

Every added file passed `lake env lean` from `dev/lean`. The final
`lake build` used temporary root imports for all added modules.
Both ran on the workstation through `/tmp/umvl-cpu/run` with `nice -n 10`.

`Build completed successfully (7896 jobs).`

`git diff --exit-code dev/lean/UnivariateML.lean` passed after removal of the imports. Every new declaration has a checked proof.

## Theorems

### Singleton domains

`UnivariateML.HeldSweepSingleton.singleton_fit`

The supplied feasible parameter uniquely maximizes the likelihood and its likelihood is the supremum.

```lean
theorem singleton_fit {P : Type*} (L : P → ℝ) (p : P) :
    (∃! q, q ∈ ({p} : Set P) ∧ IsMaxOn L {p} q) ∧
      IsGreatest (L '' ({p} : Set P)) (L p) ∧
      sSup (L '' ({p} : Set P)) = L p
```

### CMP

`UnivariateML.HeldSweepCMP.held_rate_bernoulli_path`

The entire sample likelihood tends to the Bernoulli likelihood with odds equal to the held rate.

```lean
theorem held_rate_bernoulli_path {n : ℕ} (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    Tendsto (fun ν ↦ CMP.likelihood x r ν) atTop
      (𝓝 (∏ i, (if x i ≤ 1 then r ^ x i else 0) / (1 + r)))
```

`UnivariateML.HeldSweepCMP.held_rate_binary_supremum`

For a nonempty binary sample the Bernoulli limit is the strict unattained supremum.

```lean
theorem held_rate_binary_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) {r : ℝ} (hr : 0 < r) :
    let D := {ν : ℝ | (r, ν) ∈ CMP.rateDomain}
    let b := ∏ i, r ^ x i / (1 + r)
    IsLUB (CMP.likelihood x r '' D) b ∧ ∀ ν ∈ D, CMP.likelihood x r ν < b
```

`UnivariateML.HeldSweepCMP.held_rate_nonbinary`

Any count above one gives a unique maximizing dispersion, including the geometric endpoint when feasible.

```lean
theorem held_rate_nonbinary {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 1 < x i) {r : ℝ} (hr : 0 < r) :
    ∃! ν, (r, ν) ∈ CMP.rateDomain ∧
      IsMaxOn (CMP.likelihood x r) {v : ℝ | (r, v) ∈ CMP.rateDomain} ν
```

`UnivariateML.HeldSweepCMP.held_zero_rate`

All nonnegative dispersions tie at rate zero, with likelihood one on all-zero data and zero otherwise.

```lean
theorem held_zero_rate {n : ℕ} (x : Fin n → ℕ) :
    ∀ ν ∈ Ici (0 : ℝ),
      IsMaxOn (CMP.likelihood x 0) (Ici 0) ν ∧
        CMP.likelihood x 0 ν = (if ∀ i, x i = 0 then 1 else 0)
```

### Skellam

`UnivariateML.HeldSweepSkellam.held_first_attainment`

A positive first rate held gives an attained maximum.

```lean
theorem held_first_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃ v ∈ Ici 0, IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v
```

`UnivariateML.HeldSweepSkellam.likelihood_reflection`

Reflecting the counts and swapping the rates preserves the likelihood.

```lean
theorem likelihood_reflection {n : ℕ} (x : Fin n → ℤ) (p : ℝ × ℝ) :
    likelihood (fun i ↦ -x i) p.swap = likelihood x p
```

`UnivariateML.HeldSweepSkellam.held_second_attainment`

A positive second rate held gives an attained maximum.

```lean
theorem held_second_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃ v ∈ Ici 0, IsMaxOn (fun w ↦ likelihood x (w, r)) (Ici 0) v
```

`UnivariateML.HeldSweepSkellam.held_first_unique`

The positive first-rate hold has exactly one maximum, including when the second rate is zero.

```lean
theorem held_first_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (r, w)) (Ici 0) v
```

`UnivariateML.HeldSweepSkellam.held_second_unique`

The positive second-rate hold has exactly one maximum.

```lean
theorem held_second_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {r : ℝ} (hr : 0 < r) :
    ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (w, r)) (Ici 0) v
```

### Poisson–inverse Gaussian

`UnivariateML.HeldSweepPIG.all_zero_argmax`

On all-zero samples the entire shape-zero ray is the maximizing set. Restricting the ray to a held mean fixes the maximizer.

```lean
theorem all_zero_argmax {n : ℕ} (hn : 0 < n) {p : ℝ × ℝ}
    (hp : p ∈ Ioi (0 : ℝ) ×ˢ Ici 0) :
    IsMaxOn (fun q : ℝ × ℝ ↦ zeroLikelihood n q.1 q.2)
      (Ioi (0 : ℝ) ×ˢ Ici 0) p ↔ p.2 = 0
```

`UnivariateML.HeldSweepPIG.held_shape_all_zero`

At positive shape on all-zero data, mean zero is excluded and the likelihood approaches its unattained supremum one.

```lean
theorem held_shape_all_zero {n : ℕ} (hn : 0 < n) {lam : ℝ} (hlam : 0 < lam) :
    Tendsto (fun μ ↦ zeroLikelihood n μ lam) (𝓝[>] (0 : ℝ)) (𝓝 1) ∧
      IsLUB ((fun μ ↦ zeroLikelihood n μ lam) '' Ioi (0 : ℝ)) 1 ∧
      ∀ μ > 0, zeroLikelihood n μ lam < 1
```

`UnivariateML.HeldSweepPIG.held_shape_gap_bijOn`

At a fixed positive shape the Bessel gap parametrizes the entire positive mean fiber.

```lean
theorem held_shape_gap_bijOn {lam : ℝ} (hl : 0 < lam) :
    BijOn (fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2) - lam / μ)
      (Ioi 0) (Ioo 0 (sqrt (2 * lam)))
```

`UnivariateML.HeldSweepPIG.held_shape_binary_unique`

For positive binary data the held-shape likelihood has a unique maximizing Bessel gap.

```lean
theorem held_shape_binary_unique {n S lam : ℝ} (hn : 0 < n) (hS : 0 < S) (hl : 0 < lam) :
    ∃! d, d ∈ Ioo (0 : ℝ) (sqrt (2 * lam)) ∧
      IsMaxOn (binaryHeldShapeLog n S lam) (Ioo 0 (sqrt (2 * lam))) d
```

### Grouped normal

`UnivariateML.HeldSweepDNorm.held_location_atMostOne`

Strict concavity gives at most one maximum in the held-location working log likelihood.

```lean
theorem held_location_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun B ↦ workingLogLik x (m * B, B)) (Ioi 0) a)
    (hmb : IsMaxOn (fun B ↦ workingLogLik x (m * B, B)) (Ioi 0) b) :
    a = b
```

`UnivariateML.HeldSweepDNorm.held_scale_atMostOne`

Strict concavity gives at most one maximum in the held-scale working log likelihood.

```lean
theorem held_scale_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {B : ℝ} (hB : 0 < B) {a b : ℝ}
    (hma : IsMaxOn (fun A ↦ workingLogLik x (A, B)) univ a)
    (hmb : IsMaxOn (fun A ↦ workingLogLik x (A, B)) univ b) :
    a = b
```

`UnivariateML.HeldSweepDNorm.held_location_contact`

Every cell mass grows strictly with inverse scale on the contact branch.

```lean
theorem held_location_contact {n : ℕ} (x : Fin n → ℤ) (m : ℝ)
    (hx : ∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    ∀ i, StrictMonoOn (fun B ↦ mass (x i : ℝ) B m) (Ioi 0)
```

`UnivariateML.HeldSweepDNorm.held_location_contact_supremum`

At contact the zero-scale likelihood limit gives a factor of one per interior cell and one half per endpoint cell. Every finite likelihood is strictly lower.

```lean
theorem held_location_contact_supremum {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℤ) (m : ℝ) (hx : ∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    let u := ∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1)
    Tendsto (heldLocationLikelihood x m) atTop (𝓝 u) ∧
      IsLUB (heldLocationLikelihood x m '' Ioi 0) u ∧
      ∀ B > 0, heldLocationLikelihood x m B < u
```

`UnivariateML.HeldSweepDNorm.held_scale_attainment`

With scale held positive, the normalized CDF likelihood attains a maximum over locations.

```lean
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {B : ℝ} (hB : 0 < B) :
    ∃ m, IsMaxOn (fun m ↦ heldLocationLikelihood x m B) univ m
```

`UnivariateML.HeldSweepDNorm.held_location_away_attainment`

If any cell excludes the held location, the normalized CDF likelihood attains a finite-scale maximum.

```lean
theorem held_location_away_attainment {n : ℕ} (x : Fin n → ℤ) (m : ℝ)
    (hx : ∃ i, m ∉ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    ∃ B ∈ Ioi (0 : ℝ), IsMaxOn (heldLocationLikelihood x m) (Ioi 0) B
```

### Beta-negative binomial

`UnivariateML.HeldSweepBNBinom.held_alpha_two_of_diagonal_exclusion`

If a feasible point beats every diagonal point, attainment and symmetry give two distinct labeled maxima.

```lean
theorem held_alpha_two_of_diagonal_exclusion {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    {w : ℝ × ℝ} (hw : w ∈ Ioi (0 : ℝ) ×ˢ Ioi 0)
    (hex : ∀ d > 0, logLikelihood x d a d < logLikelihood x w.1 a w.2) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let f := fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2
    ∃ p ∈ D, p.1 ≠ p.2 ∧ IsMaxOn f D p ∧ p.swap ∈ D ∧ IsMaxOn f D p.swap
```

`UnivariateML.HeldSweepBNBinom.held_alpha_beta_iff`

The free size attains a unique finite fit exactly when some observation is positive.

```lean
theorem held_alpha_beta_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x t a b) (Ioi 0) r) ↔
      ∃ i, 0 < x i
```

`UnivariateML.HeldSweepBNBinom.held_alpha_size_iff`

The free beta attains a unique finite fit exactly when some observation is positive.

```lean
theorem held_alpha_size_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r a t) (Ioi 0) b) ↔
      ∃ i, 0 < x i
```

`UnivariateML.HeldSweepBNBinom.held_beta_size_iff`

The free alpha attains a unique finite fit exactly when some observation is positive.

```lean
theorem held_beta_size_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {b r : ℝ} (hb : 0 < b) (hr : 0 < r) :
    (∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r t b) (Ioi 0) a) ↔
      ∃ i, 0 < x i
```

### Bhattacharjee

`UnivariateML.HeldSweepBhatt.range_equality`

The range-equality supremum is the optimized complementary endpoint trace at the midrange.

```lean
theorem range_equality {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a : ℝ} (ha : 0 < a) (i j : Fin n)
    (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j) (he : x j - x i = 2 * a) :
    let c := (x i + x j) / 2
    ∃ w ∈ Icc (0 : ℝ) 1, IsMaxOn (uniformTrace x a c) (Icc 0 1) w ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0))
        (uniformTrace x a c w) ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < uniformTrace x a c w
```

`UnivariateML.HeldSweepBhatt.range_equality_height`

The exact trace maximum uses the fraction of lower endpoint observations among all endpoints.

```lean
theorem range_equality_height {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a : ℝ} (ha : 0 < a) (i j : Fin n)
    (hlo : ∀ k, x i ≤ x k) (hhi : ∀ k, x k ≤ x j) (he : x j - x i = 2 * a) :
    let z := (Finset.univ.filter (fun k ↦ x k = x i)).card
    let o := (Finset.univ.filter (fun k ↦ x k = x j)).card
    let w := (z : ℝ) / ((z : ℝ) + o)
    let u := w^z * (1 - w)^o / (2 * a)^n
    IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0)) u ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < u
```

`UnivariateML.HeldSweepBhatt.held_centre_supremum`

When the held interval covers all observations the supremum is unattained and penalized by one half per endpoint observation.

```lean
theorem held_centre_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| ≤ a) :
    let u := ∏ i, (if |x i - m| = a then (1 : ℝ) / 2 else 1) / (2 * a)
    IsLUB (likelihood x a m '' Ioi 0) u ∧ ∀ s > 0, likelihood x a m s < u
```

`UnivariateML.HeldSweepBhatt.normal_member_log`

At zero width the log likelihood equals the ordinary normal log likelihood.

```lean
theorem normal_member_log {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < likelihood x 0 m s ∧ log (likelihood x 0 m s) = Normal.logLik x m s
```

`UnivariateML.HeldSweepBhatt.normal_member_argmax`

At zero width the full argmax set agrees with the ordinary normal argmax set.

```lean
theorem normal_member_argmax {n : ℕ} (x : Fin n → ℝ) {p : ℝ × ℝ} (hp : p ∈ Normal.domain) :
    IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x 0 q.1 q.2) Normal.domain p ↔
      IsMaxOn (fun q : ℝ × ℝ ↦ Normal.logLik x q.1 q.2) Normal.domain p
```

### Discrete Weibull

`UnivariateML.HeldSweepDWeibull.held_probability_shape_zero`

Shape zero in the limit keeps the zero atom with mass one minus the supplied probability. The rest of the mass escapes.

```lean
theorem held_probability_shape_zero (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ mass k (a, b)) (𝓝[>] (0 : ℝ))
      (𝓝 (if k = 0 then 1 - exp (-exp a) else 0))
```

`UnivariateML.HeldSweepDWeibull.held_probability_shape_top`

Infinite shape gives the Bernoulli law at the held probability.

```lean
theorem held_probability_shape_top (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ mass k (a, b)) atTop
      (𝓝 (if k = 0 then 1 - exp (-exp a) else if k = 1 then exp (-exp a) else 0))
```

`UnivariateML.HeldSweepDWeibull.held_shape_probability_zero`

Probability tending to zero gives the point mass at zero.

```lean
theorem held_shape_probability_zero (b : ℝ) (k : ℕ) :
    Tendsto (fun a ↦ mass k (a, b)) atTop (𝓝 (if k = 0 then 1 else 0))
```

`UnivariateML.HeldSweepDWeibull.held_shape_probability_one`

Probability tending to one gives a zero limit at every finite count.

```lean
theorem held_shape_probability_one (b : ℝ) (k : ℕ) :
    Tendsto (fun a ↦ mass k (a, b)) atBot (𝓝 0)
```

### GEV

`UnivariateML.HeldSweepGEV.reverse_held_scale`

At shape minus one the fitted location places the upper endpoint at the sample maximum.

```lean
theorem reverse_held_scale {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (j : Fin n)
    (hj : ∀ i, x i ≤ x j) {s : ℝ} (hs : 0 < s) :
    ∃! m, (m, s) ∈ domain x (-1) ∧
      IsMaxOn (fun u ↦ logLik x (-1) (u, s)) {u | (u, s) ∈ domain x (-1)} m
```

`UnivariateML.HeldSweepGEV.reverse_held_location`

At shape minus one with location below the maximum and at most the sample mean, the fitted scale places the upper endpoint at the sample maximum.

```lean
theorem reverse_held_location {n : ℕ} {x : Fin n → ℝ} (j : Fin n)
    (hj : ∀ i, x i ≤ x j) {m : ℝ} (hm : m < x j)
    (havg : (n : ℝ) * m ≤ ∑ i, x i) :
    ∃! s, (m, s) ∈ domain x (-1) ∧
      IsMaxOn (fun v ↦ logLik x (-1) (m, v)) {v | (m, v) ∈ domain x (-1)} s
```

`UnivariateML.HeldSweepGEV.held_minimum_majority_collapse`

A strict majority of observations at the held minimum gives unbounded likelihood along a feasible shape-one path.

```lean
theorem held_minimum_majority_collapse {n : ℕ} (x : Fin n → ℝ) {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n < 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    (∀ s > 0, (m, s) ∈ domain x 1) ∧
      Tendsto (fun s ↦ logLik x 1 (m, s)) (𝓝[>] (0 : ℝ)) atTop
```

`UnivariateML.HeldSweepGEV.held_minimum_half_path`

At exact half contact the shape-one path has the finite held-minimum collapse value.

```lean
theorem held_minimum_half_path {n : ℕ} (x : Fin n → ℝ) {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    Tendsto (fun s ↦ logLik x 1 (m, s)) (𝓝[>] (0 : ℝ))
      (𝓝 (∑ i, if x i = m then (-1 : ℝ) else -2 * log (x i - m)))
```

### Gompertz

`UnivariateML.HeldSweepGompertz.held_growth_hazard_zero`

At fixed growth the likelihood tends to zero as hazard tends to zero.

```lean
theorem held_growth_hazard_zero {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    Tendsto (fun a ↦ logLik x a b) (𝓝[>] (0 : ℝ)) atBot
```

`UnivariateML.HeldSweepGompertz.held_growth_hazard_infty`

At fixed growth the likelihood tends to zero at infinite hazard when the integrated hazard is positive.

```lean
theorem held_growth_hazard_infty {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (h : 0 < integrated x b) : Tendsto (fun a ↦ logLik x a b) atTop atBot
```

`UnivariateML.HeldSweepGompertz.held_hazard_exponential_path`

At fixed hazard and growth tending to zero the limit is the exponential likelihood at the supplied hazard.

```lean
theorem held_hazard_exponential_path {n : ℕ} (x : Fin n → ℝ) (a : ℝ) :
    Tendsto (logLik x a) (𝓝[>] (0 : ℝ))
      (𝓝 ((n : ℝ) * log a - a * sampleSum x))
```

`UnivariateML.HeldSweepGompertz.held_hazard_growth_infty`

At held positive hazard and positive sample sum, infinite growth sends the likelihood to zero.

```lean
theorem held_hazard_growth_infty {n : ℕ} (x : Fin n → ℝ)
    (hx : ∀ i, 0 ≤ x i) (hpos : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    Tendsto (logLik x a) atTop atBot
```

### Logistic and Gumbel

`UnivariateML.HeldSweepLocScale.logistic_held_scale`

A positive held scale gives a unique location for every nonempty logistic sample.

```lean
theorem logistic_held_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {B : ℝ} (hB : 0 < B) :
    ∃! A, A ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun a ↦ inverseLogLik logisticLoss x (B, a)) univ A
```

`UnivariateML.HeldSweepLocScale.gumbel_held_scale`

A positive held scale gives a unique location for every nonempty Gumbel sample.

```lean
theorem gumbel_held_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {B : ℝ} (hB : 0 < B) :
    ∃! A, A ∈ (univ : Set ℝ) ∧
      IsMaxOn (fun a ↦ inverseLogLik gumbelLoss x (B, a)) univ A
```

`UnivariateML.HeldSweepLocScale.logistic_held_location`

The logistic scale fit exists uniquely exactly when an observation differs from the held location.

```lean
theorem logistic_held_location {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! B, B ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun b ↦ inverseLogLik logisticLoss x (b, m * b)) (Ioi 0) B) ↔
        ∃ i, x i ≠ m
```

`UnivariateML.HeldSweepLocScale.gumbel_held_location`

The Gumbel scale fit exists uniquely exactly when an observation differs from the held location.

```lean
theorem gumbel_held_location {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    (∃! B, B ∈ Ioi (0 : ℝ) ∧
      IsMaxOn (fun b ↦ inverseLogLik gumbelLoss x (b, m * b)) (Ioi 0) B) ↔
        ∃ i, x i ≠ m
```

`UnivariateML.HeldSweepLocScale.logistic_held_location_collapse`

Constant observations at the held location yield unbounded likelihood as scale shrinks.

```lean
theorem logistic_held_location_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun b ↦ inverseLogLik logisticLoss (fun _ : Fin n ↦ m) (b, m * b)) atTop atTop
```

`UnivariateML.HeldSweepLocScale.gumbel_held_location_collapse`

Constant observations at the held location yield unbounded likelihood as scale shrinks.

```lean
theorem gumbel_held_location_collapse {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun b ↦ inverseLogLik gumbelLoss (fun _ : Fin n ↦ m) (b, m * b)) atTop atTop
```

### Double Pareto

`UnivariateML.HeldSweepDoublePareto.held_left_rate_unique`

A positive right-side deviation gives a unique free right-rate fit.

```lean
theorem held_left_rate_unique {L R a : ℝ} (ha : 0 < a) (hR : 0 < R) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective L R a) (Ioi 0) b
```

`UnivariateML.HeldSweepDoublePareto.held_left_rate_zero_deviation`

At zero right-side deviation the strict supremum is the power face with left rate held.

```lean
theorem held_left_rate_zero_deviation {L a : ℝ} (ha : 0 < a) :
    Tendsto (objective L 0 a) atTop (𝓝 (log a - a * L)) ∧
      IsLUB (objective L 0 a '' Ioi 0) (log a - a * L) ∧
      ∀ b > 0, objective L 0 a b < log a - a * L
```

`UnivariateML.HeldSweepDoublePareto.held_right_rate_unique`

A positive left-side deviation gives a unique free left-rate fit.

```lean
theorem held_right_rate_unique {L R b : ℝ} (hb : 0 < b) (hL : 0 < L) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun r ↦ objective L R r b) (Ioi 0) a
```

`UnivariateML.HeldSweepDoublePareto.held_right_rate_zero_deviation`

At zero left-side deviation the strict supremum is the Pareto face with right rate held.

```lean
theorem held_right_rate_zero_deviation {R b : ℝ} (hb : 0 < b) :
    Tendsto (fun a ↦ objective 0 R a b) atTop (𝓝 (log b - b * R)) ∧
      IsLUB ((fun a ↦ objective 0 R a b) '' Ioi 0) (log b - b * R) ∧
      ∀ a > 0, objective 0 R a b < log b - b * R
```

### Negative binomial

`UnivariateML.HeldSweepNegBinomial.held_probability_zero_data`

On all zeros, probability one leaves size unidentified and an interior held probability gives size zero.

```lean
theorem held_probability_zero_data {n : ℕ} (hn : 0 < n) {p r : ℝ}
    (hp : p ∈ Ioc (0 : ℝ) 1) (hr : r ∈ Ici (0 : ℝ)) :
    IsMaxOn (fun s ↦ zeroLikelihood n s p) (Ici 0) r ↔ r = 0 ∨ p = 1
```

`UnivariateML.HeldSweepNegBinomial.held_size_zero_data`

On all zeros, size zero leaves probability unidentified and a positive held size gives probability one.

```lean
theorem held_size_zero_data {n : ℕ} (hn : 0 < n) {p r : ℝ}
    (hr : 0 ≤ r) (hp : p ∈ Ioc (0 : ℝ) 1) :
    IsMaxOn (zeroLikelihood n r) (Ioc 0 1) p ↔ r = 0 ∨ p = 1
```
