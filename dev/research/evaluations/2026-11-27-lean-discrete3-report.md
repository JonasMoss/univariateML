# TASK-388 Lean formalization for discrete count families

## Admitted lemmas

None.

## Scope findings

The Skellam atlas audit describes a “mean-zero two-sign slice.” The cited proof establishes uniqueness when there is a single observed magnitude, including observations at both signs of that magnitude. An unrestricted interpretation of “two-sign” would include the mean-zero, multiple-magnitude real-weight co-winners constructed later in the same report. The report leaves integer-sample ties open. The Lean uniqueness theorem below uses the single-magnitude hypothesis. [Atlas audit](2026-11-27-atlas-audit-report.md#2-stale-or-missing-statements) · [Skellam uniqueness and coexistence](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem)

The PIG held-positive-shape uniqueness claim requires at least one positive count. On all-zero data its supremum is at the excluded mean-zero endpoint. With shape zero and a free mean, the compatible all-zero sample instead has a ray of maximizers. [PIG report](2026-11-25-pig-math-report.md#existence-and-uniqueness-by-held-mask)

The supported CMB size is capped at $10^7$. An unrestricted-size CMP escape cannot replace a comparison of finite supported sizes. Also, the written CMB report identifies a wrong support and likelihood in the fitter's former held-probability odd-center construction. At size three, held probability $3/4$ and singleton count two, the center face has support $\{1,2\}$ and gives the observed count mass $3/4$. The endpoint theorem below is for the distinct negative-dispersion face. [CMB report](2026-11-25-cmb-math-report.md#held-size)

## pig

**Checked.** On samples containing a positive count, the all-free fit is attained and unique exactly when the population variance exceeds the sample mean. At and below the threshold the Poisson value at the sample mean is a strict, unattained supremum. The positive Bessel chart is bijective with the positive mean and shape domain. Shape zero has likelihood zero on these samples. The held-mean profile result also gives the unique interior optimizer on the positive-score branch and the strict Poisson endpoint on the other branch.

The variance proof uses the finite coefficient recurrence to establish the Bessel polynomial ODE. Dividing the variance difference by the square of the inverse Bessel argument gives a continuous function at zero with strictly negative value. At a zero of the variance difference in the interior, its derivative in the inverse argument is strictly negative, so a first zero is impossible. This also verifies the strict factorization used in the compact-shape curvature proof. The likelihood ratio extends continuously to the far endpoint by moving powers of the shrinking odds factor inside the finite sum. This gives compact attainment without a separate asymptotic bound for the log likelihood. The logarithm tangent inequality gives strict profiling at fixed Bessel argument, and the empirical-moment identity converts the Poisson score to the variance criterion. The direct workstation check passed with no admissions. [PIG curvature proof](2026-11-06-pig-cert-data/concavity-proof.md)

**Open outside the requested all-free positive-count theorem.** The separate held-positive-shape crossing theorem and the all-zero shape-zero maximizing set remain to be formalized in this module.

### `UnivariateML.Discrete3PIG.fixedArgument_strict`

```lean
theorem fixedArgument_strict {n S z t u : ℝ}  (hS : 0 < S)
    (ht : t ∈ Ioo 0 z) (hu : u ∈ Ioo 0 z)
    (hroot : n * (z ^ 2 - t ^ 2) = 2 * S * t) (hne : u ≠ t) :
    fixedArgument n S z u < fixedArgument n S z t
```

At fixed Bessel argument, the mean-profile root strictly beats every other feasible point. The parameter-independent terms on that slice are omitted.

### `UnivariateML.Discrete3PIG.binary_poisson_iff`

```lean
theorem binary_poisson_iff {n S μ : ℝ} (hS : 0 < S) :
    (∀ w ∈ Ioo (0 : ℝ) 1, binaryGain n S μ w < 0) ↔ n*μ ≤ 2*S
```

On positive binary data, all interior shape log likelihoods lie below Poisson exactly at a nonpositive boundary score. The general profile theorem below includes the positive-score branch.

### `UnivariateML.Discrete3PIG.bessel_variance_bound`

```lean
theorem bessel_variance_bound {k : ℕ} (hk : 2 ≤ k) {y : ℝ} (hy : 0 < y) :
    degreeVariance k y <
      degreeMean k y * ((k : ℝ)^2-(degreeMean k y)^2) / ((k : ℝ)^2+(degreeMean k y)^2)
```

The variance of the polynomial degree is strictly below the barrier at every positive inverse argument for counts at least two. The mean and variance are computed from the Bessel polynomial and its first two derivatives.

### `UnivariateML.Discrete3PIG.cellGain_strictConcave`

```lean
theorem cellGain_strictConcave {k : ℕ} (hk : 0 < k) {μ : ℝ} (hμ : 0 < μ) :
    StrictConcaveOn ℝ (Ioo 0 1) (cellGain k μ)
```

For every positive count, the log likelihood relative to Poisson is strictly concave in compact shape. The result uses positive held mean.

### `UnivariateML.Discrete3PIG.held_mean_profile`

```lean
theorem held_mean_profile {n : ℕ} {x : Fin n → ℕ} {μ : ℝ} (hμ : 0 < μ)
    (hx : ∃ i, 0 < x i) :
    if 0 < inwardScore x μ then
      ∃! w, w ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (sampleGain x μ) (Ioo 0 1) w
    else
      IsLUB ((sampleGain x μ) '' Ioo 0 1) 0 ∧
        ∀ w ∈ Ioo (0 : ℝ) 1, sampleGain x μ w < 0
```

For a positive sample and positive held mean, the inward score determines the unique interior optimum or the strict Poisson supremum. The log likelihood is measured relative to the Poisson value, so its boundary value is zero.

### `UnivariateML.Discrete3PIG.all_free_positive`

```lean
theorem all_free_positive {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    ((∃ p ∈ positiveChart, IsMaxOn (chartLogLik x) positiveChart p) ↔
      sampleMean x < sampleVariance x) ∧
    (sampleMean x < sampleVariance x →
      ∃! p, p ∈ positiveChart ∧ IsMaxOn (chartLogLik x) positiveChart p) ∧
    (sampleVariance x ≤ sampleMean x →
      IsLUB (chartLogLik x '' positiveChart) (poissonLogLik x (sampleMean x)) ∧
        ∀ p ∈ positiveChart, chartLogLik x p < poissonLogLik x (sampleMean x))
```

The all-free finite maximum exists exactly when the sample variance exceeds the sample mean. It is unique when it exists. Otherwise the Poisson value at the sample mean is an unattained supremum. The sample contains a positive count. The log likelihood drops the common factorial term.

### `UnivariateML.Discrete3PIG.parameterChart_bijOn`

```lean
theorem parameterChart_bijOn :
    BijOn parameterChart (Ioi 0 ×ˢ Ioi 0) positiveChart
```

The Bessel argument and shape-to-mean ratio give a bijective chart on the positive parameter quadrant. The theorem identifies the domain used by the all-free result.

## dnorm

**Checked.** The all-free discretized-normal likelihood has one finite maximizer if the occupied integer range is at least two. Constant samples have the strict point-mass supremum. Samples occupying both adjacent cells have the strict Bernoulli supremum at the empirical proportions. The working chart is bijective with ordinary location and positive scale.

The integration-by-parts proof differentiates the auxiliary field and applies the fundamental theorem to the boundary term. The two squares have strictly positive integral when either endpoint velocity is nonzero. The affine endpoint map is injective for each cell, so linewise strict concavity holds for every nonempty sample. For the existence step, the Gaussian tail limits and the bound by interval width trap each positive likelihood level in a closed rectangle when the extreme occupied cell closures are disjoint. Compactness gives attainment and strict concavity gives uniqueness. The collapse paths send inverse scale to infinity while fixing either the cell center or the standardized common seam. An intermediate-value argument chooses the seam yielding the empirical split, and the existing binomial optimizer bounds the finite likelihood strictly below that split's height. The direct workstation check passed with no admissions. [Discretized-normal report](2026-11-25-dnorm-math-report.md)

**Open outside the requested joint theorem.** The general held-location and held-scale existence statements remain separate. The cell monotonicity and constant fixed-scale results below remain available.

### `UnivariateML.Discrete3DNorm.midpoint_strict`

```lean
theorem midpoint_strict {k B μ : ℝ} (hB : 0 < B) (hμ : μ ≠ k+1/2) :
    mass k B μ < mass k B (k+1/2)
```

With positive scale held and a constant cell observed, its midpoint strictly beats every other location. The cell mass omits the common Gaussian normalization.

### `UnivariateML.Discrete3DNorm.cell_mass_strictMono_of_mem`

```lean
theorem cell_mass_strictMono_of_mem {k μ : ℝ} (hμ : μ ∈ Icc k (k+1)) :
    StrictMonoOn (fun B ↦ mass k B μ) (Ioi 0)
```

A cell whose closure contains the held mean has mass strictly increasing with inverse scale. The result is per cell.

### `UnivariateML.Discrete3DNorm.endpoint_information_identity`

```lean
theorem endpoint_information_identity {a b va vb : ℝ} (hab : a < b) :
    let p := intervalMass a b
    let s := intervalScore a b va vb
    let w := directionField a b va vb
    s^2*p + (b*kernel b*vb^2-a*kernel a*va^2) =
      ∫ z in a..b, ((s+z*w z)^2+(w z)^2)*kernel z
```

The cell directional information times mass is an integral of weighted squares. The kernel is unnormalized. Affine endpoint velocities and an ordered interval are used.

### `UnivariateML.Discrete3DNorm.endpoint_information_pos`

```lean
theorem endpoint_information_pos {a b va vb : ℝ} (hab : a < b)
    (hne : va ≠ 0 ∨ vb ≠ 0) :
    (intervalScore a b va vb)^2 +
      (b*kernel b*vb^2-a*kernel a*va^2)/intervalMass a b > 0
```

The cell directional information is strictly positive in every nonzero endpoint direction. The endpoints are finite and strictly ordered.

### `UnivariateML.Discrete3DNorm.workingCellLog_strictConcave`

```lean
theorem workingCellLog_strictConcave (k : ℝ) :
    StrictConcaveOn ℝ workingDomain (workingCellLog k)
```

The log mass of any cell is strictly concave in inverse-scale coordinates. The center in the source parameterization is taken to be zero.

### `UnivariateML.Discrete3DNorm.workingLogLik_strictConcave`

```lean
theorem workingLogLik_strictConcave {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    StrictConcaveOn ℝ workingDomain (workingLogLik x)
```

Every nonempty sample has a strictly concave log likelihood in inverse-scale coordinates. No range condition is needed for concavity.

### `UnivariateML.Discrete3DNorm.joint_of_gap`

```lean
theorem joint_of_gap {n : ℕ} {x : Fin n → ℤ} {i j : Fin n} (hgap : x i+1 < x j) :
    ∃! p, p ∈ workingDomain ∧ IsMaxOn (workingLikelihood x) workingDomain p
```

A pair of observed cells with a positive separation gives a unique finite maximum. The condition is equivalent to integer range at least two.

### `UnivariateML.Discrete3DNorm.constant_supremum`

```lean
theorem constant_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℤ}
    {k : ℤ} (hx : ∀ i, x i = k) :
    IsLUB (workingLikelihood x '' workingDomain) 1 ∧
      ∀ p ∈ workingDomain, workingLikelihood x p < 1
```

A nonempty constant sample has the strict, unattained point-mass supremum of one. This uses the product likelihood.

### `UnivariateML.Discrete3DNorm.adjacent_supremum`

```lean
theorem adjacent_supremum {n : ℕ} {x : Fin n → ℤ} {k : ℤ}
    (hx : ∀ i, x i = k ∨ x i = k+1)
    (hz : 0 < cellCount x k) (ho : 0 < cellCount x (k+1)) :
    IsLUB (workingLikelihood x '' workingDomain)
        (adjacentHeight (cellCount x k) (cellCount x (k+1))) ∧
      ∀ p ∈ workingDomain, workingLikelihood x p <
        adjacentHeight (cellCount x k) (cellCount x (k+1))
```

Two occupied adjacent cells have the strict, unattained Bernoulli supremum at the empirical proportions. The face height is the product likelihood with the observed multiplicities.

### `UnivariateML.Discrete3DNorm.joint_range_criterion`

```lean
theorem joint_range_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    ((∃ p ∈ workingDomain, IsMaxOn (workingLikelihood x) workingDomain p) ↔
      ∃ i j, x i+1 < x j) ∧
    ((∃ i j, x i+1 < x j) →
      ∃! p, p ∈ workingDomain ∧ IsMaxOn (workingLikelihood x) workingDomain p)
```

The discretized-normal maximum exists exactly when the integer range is at least two, and it is then unique. Constant and adjacent cases are exhausted in the nonattainment direction.

### `UnivariateML.Discrete3DNorm.workingChart_bijOn`

```lean
theorem workingChart_bijOn : BijOn workingChart workingDomain workingDomain
```

The working chart is bijective on positive scale. The same formula inverts the map.

## skellam

**Checked.** The actual Poisson convolution likelihood attains on the closed nonnegative quadrant for every nonempty signed integer sample. A mean-zero sample supported on both signs of one positive magnitude has one maximizer. The proof places it on the positive diagonal.

For attainment, rectangle-local summable bounds give continuity through the axes, and the zeroth convolution term gives positivity at positive rates. A Poisson atom of positive order is largest at the rate equal to its order. Stirling's lower bound shows that those maxima vanish at large orders. At the finitely many smaller orders, exponential decay gives a common rate cutoff. Convolution transfers either Poisson bound to every signed atom. This discharges the compactness hypotheses.

For uniqueness, the mean-zero power factors depend only on the rate product, so at fixed product the equal-rate point strictly improves the exponential factor. Both axes lose because both signs are observed. In the diagonal proof the positive series in squared rate has derivative equal to the series at the next order, and the factorial recurrence gives its differential equation. The derivative of the scaled likelihood derivative has the sign of the squared count magnitude minus twice the diagonal rate. The scaled derivative is positive up to that turning point, using the zero likelihood at rate zero and positivity inside. It is strictly decreasing thereafter and has at most one zero. Attainment supplies the critical point. The direct workstation check passed with no admissions. [Skellam report](2026-11-25-skellam2-math-report.md#reflection-and-a-uniqueness-theorem)

**Open outside the requested slice.** The positive held-rate score theorem and general joint cardinality are separate. The one-magnitude Lean theorem assumes mean zero and both signs explicitly.

### `UnivariateML.Discrete3Skellam.fixedProduct_strict`

```lean
theorem fixedProduct_strict {z d t u : ℝ} (hz : 0 < z)
    (hroot : d = (z/2) * (exp t - exp (-t))) (hne : u ≠ t) :
    fixedProduct z d u < fixedProduct z d t
```

At fixed positive rate product, the stationary log ratio strictly beats every other ratio. The Bessel contribution is constant on the slice.

### `UnivariateML.Discrete3Skellam.mean_zero_diagonal`

```lean
theorem mean_zero_diagonal {z u : ℝ} (hz : 0 < z) (hu : u ≠ 0) :
    fixedProduct z 0 u < fixedProduct z 0 0
```

For mean zero, unequal positive rates lose to equal rates at the same product in the fixed-product objective. The full sample theorem below also proves uniqueness along the diagonal on its stated slice.

### `UnivariateML.Discrete3Skellam.attained_of_uniform_tail`

```lean
theorem attained_of_uniform_tail {L : ℝ × ℝ → ℝ}
    (hc : ContinuousOn L (Ici 0 ×ˢ Ici 0))
    {p : ℝ × ℝ} (hp : p ∈ Ici 0 ×ˢ Ici 0) (hpos : 0 < L p)
    (htail : ∀ ε > 0, ∃ R : ℝ, ∀ q ∈ Ici 0 ×ˢ Ici 0,
      (R < q.1 ∨ R < q.2) → L q < ε) :
    ∃ q ∈ Ici 0 ×ˢ Ici 0, IsMaxOn L (Ici 0 ×ˢ Ici 0) q
```

A continuous quadrant likelihood with a positive point and uniform vanishing tails attains. The generic compactness step is instantiated by the Skellam sample theorem.

### `UnivariateML.Discrete3Skellam.mass_pos`

```lean
theorem mass_pos (k : ℤ) {p : ℝ × ℝ} (h₁ : 0 < p.1) (h₂ : 0 < p.2) :
    0 < mass k p
```

Every signed Skellam atom is strictly positive at positive rates. The atom is the Poisson convolution series.

### `UnivariateML.Discrete3Skellam.mass_continuousOn`

```lean
theorem mass_continuousOn (k : ℤ) : ContinuousOn (mass k) domain
```

Skellam masses are continuous throughout the closed rate quadrant. The statement includes both axes and the origin.

### `UnivariateML.Discrete3Skellam.poisson_atoms_uniform_tail`

```lean
theorem poisson_atoms_uniform_tail {ε : ℝ} (hε : 0 < ε) :
    ∃ R : ℝ, ∀ r > R, 0 ≤ r → ∀ j, poissonMass r j < ε
```

Poisson atoms vanish uniformly over their order when rate tends to infinity. This supplies the tail estimate for each convolution factor.

### `UnivariateML.Discrete3Skellam.likelihood_uniform_tail`

```lean
theorem likelihood_uniform_tail {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {ε : ℝ} (hε : 0 < ε) :
    ∃ R : ℝ, ∀ q ∈ domain, (R < q.1 ∨ R < q.2) → likelihood x q < ε
```

For a nonempty sample the Skellam likelihood is uniformly small when either rate is sufficiently large. The sample likelihood is a finite product of convolution masses.

### `UnivariateML.Discrete3Skellam.joint_attained`

```lean
theorem joint_attained {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) :
    ∃ p ∈ domain, IsMaxOn (likelihood x) domain p
```

Every nonempty signed integer sample has a Skellam maximum on the closed quadrant. The conclusion allows a zero rate.

### `UnivariateML.Discrete3Skellam.joint_unique_one_magnitude`

```lean
theorem joint_unique_one_magnitude {n : ℕ} (hn : 0 < n) {x : Fin n → ℤ}
    {k : ℕ} (hk : 0 < k) (hx : ∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ))
    (hmean : ∑ i, x i = 0) (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p
```

A mean-zero sample with one positive magnitude and both signs has exactly one maximizer. Both signs exclude the axes. The proof reduces to the equal-rate line.

## cmp

**Checked.** With dispersion held at any feasible value, the CMP rate has a unique maximizer. At dispersion zero the explicit geometric ratio includes the zero-rate atom. At positive dispersion the zero-rate atom fits all-zero data. Otherwise the unique positive rate matches the empirical mean.

For positive dispersion, multiplying a weight by a power of the count is dominated by a fixed increase in log rate. This gives summability of the moments and local uniform bounds for termwise differentiation. The derivative of the mean is the variance and is positive because every count has positive weight. Dominated convergence gives the small-rate limit. At large rate each finite initial block loses to a single higher-count weight, which gives an arbitrarily large lower bound for the mean. The score theorem gives the unique log-rate optimum, and an exponential identity transfers it to the full likelihood including the rate-zero atom. The direct workstation check passed with no admissions. [CMP dossier](../../../univariateML-cpp/include/cppunivariate/distributions/cmp/README.md)

**Open outside the requested held-dispersion theorem.** The general joint geometric-face comparison and the held-rate dispersion fit remain separate. The existing CMP module proves the adjacent and all-zero results on its stated scopes.

### `UnivariateML.Discrete3CMP.held_zero_dispersion`

```lean
theorem held_zero_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : r ∈ Ico 0 1) :
    IsMaxOn (fun q ↦ CMP.likelihood x q 0) (Ico 0 1) r ↔
      r = (∑ i, (x i : ℝ)) / (n + ∑ i, (x i : ℝ))
```

At zero dispersion the rate is the geometric ratio S/(n+S), including rate zero. The admissible rate interval at zero dispersion is half-open.

### `UnivariateML.Discrete3CMP.modelMean_strictMono`

```lean
theorem modelMean_strictMono {ν : ℝ} (hν : 0 < ν) :
    StrictMono (modelMean ν)
```

At positive dispersion the model mean is strictly increasing in log rate. The proof differentiates the summable moment series.

### `UnivariateML.Discrete3CMP.modelMean_atBot`

```lean
theorem modelMean_atBot {ν : ℝ} (hν : 0 < ν) :
    Tendsto (modelMean ν) atBot (𝓝 0)
```

The mean tends to zero as the log rate tends to minus infinity. The dispersion is held positive.

### `UnivariateML.Discrete3CMP.modelMean_atTop`

```lean
theorem modelMean_atTop {ν : ℝ} (hν : 0 < ν) :
    Tendsto (modelMean ν) atTop atTop
```

The mean tends to infinity as the log rate tends to infinity. The dispersion is held positive.

### `UnivariateML.Discrete3CMP.held_positive_dispersion`

```lean
theorem held_positive_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 < ν) :
    (∃! r, r ∈ Ici (0 : ℝ) ∧ IsMaxOn (fun q ↦ CMP.likelihood x q ν) (Ici 0) r) ∧
    ∀ r ∈ Ici (0 : ℝ), IsMaxOn (fun q ↦ CMP.likelihood x q ν) (Ici 0) r ↔
      (r = 0 ∧ ∀ i, x i = 0) ∨
        (0 < r ∧ modelMean ν (log r) = Discrete.avg x)
```

The positive-dispersion CMP has one rate maximizer. It is zero on all-zero samples and matches the empirical mean otherwise. The dispersion is held and the rate includes its atom at zero.

## cmb

**Checked.** The fixed-size results give exact point-mass, adjacent-pair and endpoint-pair suprema in the free regular-coefficient chart. At held interior probability, the old endpoint result is complemented by the even-center point mass and odd-center two-point law. The odd-center result includes either singleton of the central pair. Size zero is the zero atom, and size one gives the attained Bernoulli likelihood and empirical probability. At positive fixed size the mean is strictly increasing in log odds for every held dispersion, with limits zero and the size.

The proof expresses the log-binomial statistic as a constant minus two log factorials, and reuses the two supporting lines for the log-factorial graph. Choosing a slope between adjacent edges exposes an interior singleton. At central counts the exposing slope is zero in log odds, so the ray varies only dispersion. For a free-probability pair the base odds can match the empirical proportions. The finite likelihood conditional on the pair is at most the saturated value, and the excluded cells make the unconditional likelihood strictly smaller. The finite moment derivative is the variance. At either end of the odds chart, recentering at the appropriate endpoint weight makes the nonendpoint weights converge to zero. The direct workstation check passed with no admissions. [CMB report](2026-11-25-cmb-math-report.md#held-size)

**Open outside the fixed-size face results.** The general capped free-size candidate comparison, full fixed-size hull classification, and all closed-probability maximizer sets at sizes above one remain separate. The held-dispersion interior theorem uses the natural-parameter objective.

### `UnivariateML.Discrete3CMB.exposed_mass`

```lean
theorem exposed_mass {K : Type*} [Fintype K] [DecidableEq K]
    (base slope : K → ℝ) (F : Finset K) (k : K)
    (hk : k ∈ F) (hzero : ∀ j ∈ F, slope j = 0)
    (hneg : ∀ j ∉ F, slope j < 0) :
    Tendsto (fun t ↦ rayMass base slope t k) atTop
      (𝓝 (exp (base k) / ∑ j ∈ F, exp (base j)))
```

A normalized exponential ray converges to the conditional weights on its exposed face. Uses finite support and the exposing signs.

### `UnivariateML.Discrete3CMB.exposed_supremum`

```lean
theorem exposed_supremum {K : Type*} [Fintype K] [DecidableEq K] {n : ℕ}
    (hn : 0 < n) (base slope : K → ℝ) (F : Finset K) (x : Fin n → K)
    (hx : ∀ i, x i ∈ F) (hzero : ∀ j ∈ F, slope j = 0)
    (hneg : ∀ j ∉ F, slope j < 0) (hout : ∃ j, j ∉ F) :
    let u := ∏ i, exp (base (x i)) / ∑ j ∈ F, exp (base j)
    IsLUB (range (fun t ↦ ∏ i, rayMass base slope t (x i))) u ∧
      ∀ t, (∏ i, rayMass base slope t (x i)) < u
```

The normalized face product is the strict, unattained supremum along the ray. The face weights are fixed and an excluded cell exists.

### `UnivariateML.Discrete3CMB.endpoint_supremum`

```lean
theorem endpoint_supremum {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (η : ℝ) (x : Fin n → Fin (N+1))
    (hx : ∀ i, x i = 0 ∨ x i = Fin.last N) :
    let u := ∏ i, exp (η * (x i).val) / (1 + exp (η * N))
    IsLUB (range (likelihood x η)) u ∧ ∀ ν, likelihood x η ν < u
```

At held size and interior probability, endpoint-only data have the exact strict endpoint-law supremum. The upper-to-lower odds on the face are exp(Nη).

### `UnivariateML.Discrete3CMB.held_dispersion_interior`

```lean
theorem held_dispersion_interior {N : ℕ} (hN : 0 < N) (ν m : ℝ)
    (hm : m ∈ Ioo (0 : ℝ) N) :
    ∃! η, IsMaxOn (heldDispersionLogLik N ν m) univ η
```

An interior empirical mean at positive size gives a unique finite log-odds optimum for held dispersion. The objective is the per-observation parameter-dependent term.

### `UnivariateML.Discrete3CMB.adjacent_supremum`

```lean
theorem adjacent_supremum {N n : ℕ} (hN : 2 ≤ N) (x : Fin n → Fin (N+1))
    {l u : Fin (N+1)} (hlu : u.val = l.val+1)
    (hx : ∀ i, x i = l ∨ x i = u)
    (hz : 0 < multiplicity x l) (ho : 0 < multiplicity x u) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2))
        (pairHeight (multiplicity x l) (multiplicity x u)) ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < pairHeight (multiplicity x l) (multiplicity x u)
```

Two occupied adjacent cells at fixed size have the strict saturated face supremum at the empirical split. Size is at least two. Both free regular coefficients range over the interior probability chart.

### `UnivariateML.Discrete3CMB.endpoints_free_supremum`

```lean
theorem endpoints_free_supremum {N n : ℕ} (hN : 2 ≤ N) (x : Fin n → Fin (N+1))
    (hx : ∀ i, x i = 0 ∨ x i = Fin.last N)
    (hz : 0 < multiplicity x 0) (ho : 0 < multiplicity x (Fin.last N)) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2))
        (pairHeight (multiplicity x 0) (multiplicity x (Fin.last N))) ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 <
        pairHeight (multiplicity x 0) (multiplicity x (Fin.last N))
```

Both occupied endpoints at fixed size give the strict saturated endpoint supremum. Size is at least two and the regular coefficients are free.

### `UnivariateML.Discrete3CMB.constant_interior_supremum`

```lean
theorem constant_interior_supremum {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1))
    {k : Fin (N+1)} (hk₀ : 0 < k.val) (hkN : k.val < N) (hx : ∀ i, x i = k) :
    IsLUB (range (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2)) 1 ∧
      ∀ p : ℝ × ℝ, likelihood x p.1 p.2 < 1
```

An interior constant count has an unattained point-mass supremum at fixed size. Both regular coefficients are free.

### `UnivariateML.Discrete3CMB.odd_center_supremum`

```lean
theorem odd_center_supremum {N n c : ℕ} (hN : N = 2*c+1) (hc : 0 < c)
    (hn : 0 < n) (η : ℝ) (x : Fin n → Fin (N+1))
    (hx : ∀ i, (x i).val = c ∨ (x i).val = c+1) :
    let u := ∏ i, exp (η*(x i).val)/(exp (η*c)+exp (η*(c+1 : ℕ)))
    IsLUB (range (likelihood x η)) u ∧ ∀ ν, likelihood x η ν < u
```

An odd held size and held interior probability give the central-pair face on data supported there. The upper-cell mass is the held probability, including when only the upper cell is observed.

### `UnivariateML.Discrete3CMB.even_center_supremum`

```lean
theorem even_center_supremum {N n c : ℕ} (hN : N = 2*c) (hc : 0 < c)
    (hn : 0 < n) (η : ℝ) (x : Fin n → Fin (N+1)) (hx : ∀ i, (x i).val = c) :
    IsLUB (range (likelihood x η)) 1 ∧ ∀ ν, likelihood x η ν < 1
```

An even held size and held interior probability give the strict central point-mass supremum on the central singleton. The size is positive.

### `UnivariateML.Discrete3CMB.size_zero_likelihood`

```lean
theorem size_zero_likelihood {n : ℕ} (x : Fin n → Fin 1) (p ν : ℝ) :
    closedLikelihood x p ν = 1
```

Size zero gives likelihood one on the entire closed probability chart. Every observation is necessarily the zero cell.

### `UnivariateML.Discrete3CMB.bernoulli_likelihood`

```lean
theorem bernoulli_likelihood {n : ℕ} (x : Fin n → Fin 2) {p : ℝ}
    (hp : p ∈ Icc (0 : ℝ) 1) (ν : ℝ) :
    closedLikelihood x p ν = p^multiplicity x 1 * (1-p)^multiplicity x 0
```

At size one the exact likelihood is Bernoulli throughout the closed probability chart. The dispersion drops out of the law.

### `UnivariateML.Discrete3CMB.bernoulli_fit`

```lean
theorem bernoulli_fit {n : ℕ} (hn : 0 < n) (x : Fin n → Fin 2)
    (ν : ℝ) {p : ℝ} (hp : p ∈ Icc (0 : ℝ) 1) :
    IsMaxOn (fun q ↦ closedLikelihood x q ν) (Icc 0 1) p ↔
      p = (multiplicity x 1 : ℝ)/(multiplicity x 1 + (multiplicity x 0 : ℝ))
```

At size one the probability maximizer is the empirical success fraction. This holds at any held dispersion and includes empirical endpoints.

### `UnivariateML.Discrete3CMB.modelMean_strictMono`

```lean
theorem modelMean_strictMono {N : ℕ} (hN : 0 < N) (ν : ℝ) :
    StrictMono (modelMean N ν)
```

The fixed-size mean is strictly increasing in log odds for any held real dispersion. Size is positive.

### `UnivariateML.Discrete3CMB.modelMean_limits`

```lean
theorem modelMean_limits (N : ℕ) (ν : ℝ) :
    Tendsto (modelMean N ν) atBot (𝓝 0) ∧ Tendsto (modelMean N ν) atTop (𝓝 (N : ℝ))
```

The fixed-size means tend to zero and to the size at opposite ends of the log-odds chart. Holds for every finite size and real dispersion.

## bnbinom

**Checked.** Each of the three two-held regimes has a unique finite fit on positive samples. With alpha alone held, the positive-sample likelihood attains its maximum. All-zero samples have the corresponding unattained zero-atom faces. The fully held domain is a singleton.

The two-held proofs represent each log-likelihood score as a Laplace transform with a single sign change. The digamma difference kernel follows from the Gamma recurrence and convex log-Gamma secant bounds. Dominated limits give opposite endpoint signs for the scaled transform. For alpha alone held, small size or beta forces a positive-count atom to zero. A diagonal Gamma-ratio bound handles the region where both free shapes grow, and uniform control on bounded smaller shapes handles the remaining tails. The direct workstation check passed with no admissions. [Beta-negative-binomial report](2026-11-25-bnbinom-math-report.md)

**Open outside the held results.** The theorem with alpha alone held establishes attainment. General uniqueness and the unrestricted face comparison remain separate.

### `UnivariateML.Discrete3BNBinom.tilted_score_neg`

```lean
theorem tilted_score_neg {g : ℝ → ℝ} {c δ : ℝ} (hc : 0 < c) (hδ : 0 < δ)
    (hleft : ∀ t ∈ Ioo 0 c, g t < 0) (hright : ∀ t > c, 0 < g t)
    (hg : IntegrableOn g (Ioi 0))
    (ht : IntegrableOn (fun t ↦ exp (-δ*t) * g t) (Ioi 0))
    (hzero : ∫ t in Ioi 0, g t = 0) :
    (∫ t in Ioi 0, exp (-δ*t) * g t) < 0
```

A Laplace score with a single positive-to-negative crossing is strictly negative after a zero. Uses integrability and strict sign on an interval.

### `UnivariateML.Discrete3BNBinom.score_root_unique`

```lean
theorem score_root_unique {g : ℝ → ℝ} {c : ℝ} (hc : 0 < c)
    (hleft : ∀ t ∈ Ioo 0 c, g t < 0) (hright : ∀ t > c, 0 < g t)
    (hint : ∀ a > 0, IntegrableOn (fun t ↦ exp (-a*t) * g t) (Ioi 0))
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hza : ∫ t in Ioi 0, exp (-a*t) * g t = 0)
    (hzb : ∫ t in Ioi 0, exp (-b*t) * g t = 0) : a = b
```

A score with the tilted sign comparison has at most one zero. The held-shape proofs instantiate the comparison for the normalized kernels.

### `UnivariateML.Discrete3BNBinom.held_beta_size`

```lean
theorem held_beta_size {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r t b) (Ioi 0) a
```

Free alpha has a unique positive maximizer on a positive sample. Size and beta are positive and fixed.

### `UnivariateML.Discrete3BNBinom.held_alpha_beta`

```lean
theorem held_alpha_beta {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x t a b) (Ioi 0) r
```

Free size has a unique positive maximizer on a positive sample. Both beta-prior shapes are positive and fixed.

### `UnivariateML.Discrete3BNBinom.logLikelihood_swap`

```lean
theorem logLikelihood_swap {n : ℕ} (x : Fin n → ℕ) (r a b : ℝ) :
    logLikelihood x r a b = logLikelihood x b a r
```

Swapping size and beta preserves the entire log likelihood. The symmetry yields the second held-alpha scalar result.

### `UnivariateML.Discrete3BNBinom.held_alpha_size`

```lean
theorem held_alpha_size {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r a t) (Ioi 0) b
```

Free beta has a unique positive maximizer on a positive sample. Alpha and size are positive and fixed.

### `UnivariateML.Discrete3BNBinom.all_zero_held_faces`

```lean
theorem all_zero_held_faces {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 0)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    logLikelihood x r a b < 0 ∧
      Tendsto (fun t ↦ logLikelihood x t a b) (𝓝[>] (0 : ℝ)) (𝓝 0) ∧
      Tendsto (fun t ↦ logLikelihood x r a t) (𝓝[>] (0 : ℝ)) (𝓝 0) ∧
      Tendsto (fun t ↦ logLikelihood x r t b) atTop (𝓝 0)
```

The three scalar held regimes have strict all-zero suprema approached by their zero-atom limits. These same limits supply paths when only alpha is held.

### `UnivariateML.Discrete3BNBinom.held_alpha_attained`

```lean
theorem held_alpha_attained {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi 0,
      IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) p
```

With positive alpha held, a positive sample has a finite size and beta maximizer. The conclusion is attainment.

### `UnivariateML.Discrete3BNBinom.fully_held`

```lean
theorem fully_held {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ × ℝ) :
    ∃! q, q ∈ ({p} : Set (ℝ × ℝ × ℝ)) ∧
      IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ logLikelihood x q.1 q.2.1 q.2.2) {p} q
```

A fixed parameter triple uniquely maximizes on its singleton domain. The algebraic singleton statement holds for any triple.

## dgamma

**Checked.** For a nonempty sample and either positive coefficient held, a finite maximizer exists exactly when some count is positive. With either coefficient held, the finite maximizer is unique. On all-zero samples the exact supremum is one, reached in the limit as rate grows with shape held or as shape decreases to zero with rate held. Holding both coefficients gives the singleton evaluation.

For held shape the cell mass derivative in log rate is the difference of Gamma density fluxes at its two endpoints. Integration by parts identifies the log-mass derivative with the conditional mean of shape minus the unit-rate variable. The two endpoints strictly bracket that conditional mean, so the second derivative is negative, including for the cell beginning at zero. The CDF limits give compact positive likelihood levels. For held rate the small-shape bound uses the first moment of the unit-rate Gamma law, and the large-shape bound uses its exponentially tilted integral. Dominated convergence supplies continuity in positive shape. These bounds establish attainment. For uniqueness, differentiation under the integral expresses the cell log-mass curvature as the conditional log-Gamma variance minus the full log-Gamma variance. The Gamma density in log coordinates, evaluated at the inverse CDF, extends to zero at both probability endpoints and is strictly concave. This gives strict contraction of the log-quantile spacings when the probability interval is rescaled to the cell. A change of variables transports the first two moments to uniform quantiles, and the product integral of squared spacings proves the strict variance comparison. [Discretized-Gamma report](2026-11-25-dgamma-math-report.md#held-rate)

**Open outside the requested held regimes.** Joint range-two attainment, the joint collapse faces, and the joint sample-dependent uniqueness certificate remain separate.

### `UnivariateML.Discrete3DGamma.all_zero_held_shape`

```lean
theorem all_zero_held_shape {a : ℝ} (ha : 0 < a) {n : ℕ} (hn : 0 < n) :
    Tendsto (fun r ↦ (cdf (gammaMeasure a 1) r)^n) atTop (𝓝 1) ∧
      ∀ r > 0, (cdf (gammaMeasure a 1) r)^n < 1
```

At held shape the all-zero likelihood tends to one as rate increases and is strictly smaller at every finite positive rate. Uses the unit-rate CDF at the upper edge of the zero cell.

### `UnivariateML.Discrete3DGamma.held_shape`

```lean
theorem held_shape {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    ∃! r, r ∈ Ioi 0 ∧ IsMaxOn (likelihood x a) (Ioi 0) r
```

A positive count gives a unique finite rate maximizer at held positive shape. Uses strict concavity in log rate.

### `UnivariateML.Discrete3DGamma.all_zero_held_rate`

```lean
theorem all_zero_held_rate {r : ℝ} (hr : 0 < r) {n : ℕ} (hn : 0 < n) :
    Tendsto (fun a ↦ (cdf (gammaMeasure a 1) r)^n) (𝓝[>] 0) (𝓝 1) ∧
      ∀ a > 0, (cdf (gammaMeasure a 1) r)^n < 1
```

At held rate the all-zero likelihood tends to one as shape decreases to zero and is strictly smaller at every finite positive shape. Uses the unit-rate CDF at the upper edge of the zero cell.

### `UnivariateML.Discrete3DGamma.held_rate_attained`

```lean
theorem held_rate_attained {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    ∃ a ∈ Ioi 0, IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a
```

A positive count gives a finite shape maximizer at held positive rate. Unconditional attainment proved by compact positive levels.

### `UnivariateML.Discrete3DGamma.held_shape_iff`

```lean
theorem held_shape_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    (∃ r ∈ Ioi 0, IsMaxOn (likelihood x a) (Ioi 0) r) ↔ ∃ i, 0 < x i
```

At held shape a finite rate maximizer exists exactly when the nonempty sample has a positive count. Includes all-zero nonattainment.

### `UnivariateML.Discrete3DGamma.held_rate_iff`

```lean
theorem held_rate_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ} (hr : 0 < r) :
    (∃ a ∈ Ioi 0, IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a) ↔ ∃ i, 0 < x i
```

At held rate a finite shape maximizer exists exactly when the nonempty sample has a positive count. Includes all-zero nonattainment.

### `UnivariateML.Discrete3DGamma.fully_held`

```lean
theorem fully_held {n : ℕ} (x : Fin n → ℕ) (a r : ℝ) :
    ∃! p : ℝ × ℝ, p ∈ ({(a,r)} : Set (ℝ × ℝ)) ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ likelihood x q.1 q.2) {(a,r)} p
```

The supplied pair is the unique maximizer on its singleton domain. The algebraic statement holds for any parameter pair.

### `UnivariateML.Discrete3DGamma.held_rate`

```lean
theorem held_rate {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {r : ℝ} (hr : 0 < r) :
    ∃! a, a ∈ Ioi 0 ∧ IsMaxOn (fun b ↦ likelihood x b r) (Ioi 0) a
```

A positive count gives a unique finite shape maximizer at held positive rate. Strict shape curvature follows from log-Gamma quantile spacing contraction.

## Effort by target

| Target | Proof work in this pass |
| --- | --- |
| PIG | Bessel coefficient ODE, strict variance barrier, compact shape profile, empirical-moment criterion and parameter chart transfer |
| Discretized normal | Integration by parts, strict cell information, compact likelihood levels and both collapse paths |
| Skellam | Boundary-continuous convolution, uniform Stirling tail, quadrant attainment and diagonal series crossing |
| CMP | Termwise derivatives for moments, mean endpoint limits and rate-zero comparison |
| CMB | Exposing lines for the binomial statistic, saturated face suprema, Bernoulli fit and finite mean endpoints |
| Beta-negative binomial | Digamma kernel construction, three score crossings, three zero-atom limits and compactness for fixed alpha |
| Discretized gamma | Rate flux curvature, shape tail bounds and continuity, both held existence criteria, shape curvature as a variance difference and strict log-quantile spacing contraction |

## Verification

- With all seven `public import UnivariateML.Family.Discrete3…` lines temporarily appended to `dev/lean/UnivariateML.lean`, the workstation ran `/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake build` from `dev/lean`. It built the package root and reported **Build completed successfully (7790 jobs).** Style linter warnings remain.
- After restoring the original file, `git diff --exit-code dev/lean/UnivariateML.lean` exited 0.
- The worktree version of `python3 dev/tools/existence_atlas.py --check` exited 2 because its argument parser predates that option. The same command using the checker and atlas data from `68fcd92b` in `build/scratch/atlas-task391`, with the worktree proof sources and final atlas fragment, exited 0. The fragment has 71 rows covering 63 public theorems.
- A direct `lake env lean` check with `#print axioms UnivariateML.Discrete3DGamma.held_rate` reported `propext`, `Classical.choice` and `Quot.sound`.
