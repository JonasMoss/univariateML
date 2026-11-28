# TASK-494 · Second pass on the discrete-family base claims

The second pass discharged 15 of the 27 targets and recorded the remaining obligations for the other 12. It added 52 checked public theorems in four modules. The 56 mappings in the atlas fragment include theorems that support more than one regime. Coverage is blank for independent review. [First-pass ledger](2026-11-28-lean-base8-report.md#open-proof-obligations-and-effort) · [Atlas](../existence-atlas/atlas.csv) · [New fragment](../existence-atlas/lean-map.d/lean-base8b.csv)

- **Beta-binomial.** Completed the held-alpha and held-beta attainment and escape classifications, the held-beta maximizing set, single-shape uniqueness at fixed size, and the held-size criterion and excluded heights. The unrestricted results cover the all-zero ridge, positive constants, the mixed-binary ridge, mixed endpoints, and the underdispersed finite-prefix comparison. The `(7,8,13)` peak witness uses exact rational inequalities. [Beta-binomial module](../../lean/UnivariateML/Family/Base8bBBinom.lean)
- **Conway–Maxwell–binomial.** Completed the capped unrestricted criterion and the two exceptional saturated suprema, including the excluded face tied by mixed binary data. At a rational dispersion the positive odds root and each capped profile height are algebraic, and the exact height comparison retains all co-winners. The `(7,8,13)` example proves a global tie at the first zero of the profile-height difference. [CMB module](../../lean/UnivariateML/Family/Base8bCMB.lean)
- **Beta-negative-binomial.** Completed the held-beta and held-size criteria and countwise limits of every maximizing escape, including all-zero data and equality with an attained finite height. At held alpha, every off-diagonal binary fit is strictly improved on the diagonal. The two symmetric integer-hold charts have exact rational masses. [Beta-negative-binomial module](../../lean/UnivariateML/Family/Base8bBNBinom.lean)

## Source scope and discrepancies

The written beta-binomial held-size failure statement is for size at least two. The atlas row compresses that qualifier. Extending the endpoint-only exclusion to size one would contradict the finite mixed-Bernoulli ridge. The new held-size criterion explicitly handles size zero and all size-one cases, and the failure theorem joins the excluded heights and paths at size at least two. [Written beta-binomial proof](2026-11-25-bbinom-math-report.md) · [Coverage review 6b](2026-11-27-atlas-coverage6b-report.md)

The CMB size-three full-support saturation test in the source requires the frequency identity `(n₂/n₁)^3 = n₃/n₀`. The atlas wording about compatible histograms at size three needs that condition when invoking saturation. The corresponding row remains open. The CMB statements use a natural cap, specialized to 10,000,000 for the implemented model. [Written CMB saturation tests](2026-11-27-uniq-discrete-math-report.md#cmb-held-all-free) · [Coverage review 6a](2026-11-27-atlas-coverage6a-report.md)

The held-beta and held-size beta-negative-binomial criteria identify a stationary profile ordinate by the two coordinate scores. The conditional score selects the unique conditional root. This supplies the exact existence comparison, including equality. A strictly negative conditional curvature certificate and differentiability of the selected profile chart remain separate. The sources on integer-hold elimination, continuous stationary-set finiteness, and asymptotic tail coefficients have separate obligations listed below. [Written beta-negative-binomial comparison](2026-11-27-uniq-atlas-math-report.md#bnbinom-held-beta)

## Verification

All four direct Lean file checks passed on the workstation through the single-slot lock. The final aggregate Lake build passed **8,568 jobs** with the four imports temporarily present. Lake emitted linter warnings and completed successfully. The aggregate imports were restored, and the root-module Git diff was empty. The cold preparation build passed 8,562 jobs using the existing Mathlib cache.

The atlas check passed on the workstation. Each new fragment row points to its public declaration and leaves coverage empty. The finished modules contain no admitted declarations. [Atlas fragment](../existence-atlas/lean-map.d/lean-base8b.csv)

## Target ledger

| Family and hold | Target | Result and effort |
| --- | --- | --- |
| bbinom, all free | Criterion | **Open.** Proved constant-data classification, the mixed-binary ridge, strict global nonattainment for mixed zero and a maximum at least two, and the finite underdispersed prefix comparison against the optimized binomial when the smallest feasible size has an interior observation. An attained equidispersed height must still be shown algebraic and unequal to the transcendental Poisson height. The overdispersed comparison needs full growing-size NB exhaustion and the equality argument. |
| bbinom, all free | Failure | **Open.** Proved the all-zero attainment ridge, constant-height exclusion, and simultaneous fixed-size binomial and endpoint-mixture limits. With growing size the adjacent-count ratio restricts possible limit recurrences. In the scaled chart the finite-size ratio multiplier is bounded by the reciprocal of one minus the scaled dispersion divided by size. A uniform geometric tail bound below unit limiting dispersion and its conversion into normalization remain to formalize, together with the unit-dispersion and zero-atom branches. |
| bbinom, all free | Uniqueness | **Open.** Proved the constant maximizing set, exact mixed-Bernoulli ridge, mixed endpoint exclusion and exact rational separated-peak comparisons at sizes 36 through 39 and size 13. The finite-size algebraic maximizing sets and effective tail cutoffs under strict face comparisons and a losing Poisson tail remain. |
| bbinom, alpha | Criterion | **Checked.** Used the normalized score for each singleton conditional fit and classified growing-size traces via a scaled gamma quotient. Compactified the probability ratio in [0,1], extracted the fitted NB limit and transferred equality or improvement to a finite maximizer. Joined the zero and positive-constant branches. |
| bbinom, alpha | Failure | **Checked.** Joined the constant upper-atom path and strict exclusion, compactification of every nonconstant maximizing escape to the optimized NB, and the all-zero ridge with fixed-size lower-atom paths and the exact growing beta-to-size criterion. |
| bbinom, alpha | Uniqueness | **Open.** Proved the integer-alpha rational mass identity by cancellation of the gamma products and the explicit mixed-binary singleton for every positive hold. Turning the resulting rational extension into an integer candidate procedure requires a real semialgebraic monotonicity theorem and exact root recovery. The short noninteger equidispersion expansion additionally needs a uniform remainder with derivatives. |
| bbinom, alpha+size | Uniqueness | **Checked.** Replaced the Laplace crossing with a finite weighted-sum comparison. The denominator reweights toward higher indices as beta increases. The numerator factor is antitone in indices and decreases strictly in beta where a reflected count contributes. Both maxima would be roots of a strictly decreasing normalized score. Treated the zero-size and full-count branches separately. |
| bbinom, beta | Criterion | **Checked.** Proved uniform large-size loss at a positive observation by splitting the free shape into small, intermediate, and large ranges. Compact positive superlevels then give finite attainment on nonconstant data. Joined the constant branches. |
| bbinom, beta | Failure | **Checked.** Joined the bounded-size upper and lower atom recoveries, compact positive superlevels on nonconstant data, and the growing zero-mass iff using matching shifted harmonic sum bounds. |
| bbinom, beta | Uniqueness | **Checked.** Combined the reflected conditional singleton with the uniform positive-cell bound to obtain a finite size scan. The comparison keeps every tied height. This bounds every winning size without needing the stronger eventual monotonicity condition used in the written proof. |
| bbinom, beta+size | Uniqueness | **Checked.** Reflected the sample inside a fixed size, swapped the shape arguments in the public log objective and transported the alpha-held theorem. |
| bbinom, size | Criterion | **Checked.** Joined the public interior dispersion iff to the strict saturated endpoint bound and the explicit size-zero and mixed-Bernoulli witnesses. The proof covers both endpoint constants at every positive size. |
| bbinom, size | Failure | **Checked.** Joined the sharp excluded heights for nonpositive dispersion with an interior observation, both endpoint constants, and mixed endpoints. Produced a recovering path in every case and countwise atom paths for both constants. The theorem states finite exclusion and sharp limiting heights, as in the atlas row. |
| bbinom, size | Uniqueness | **Open.** The binomial comparison and saturation argument cover the boundary heights. Cleared the positive denominators in the exact universal argmax formula. The attained interior still needs finite stationary candidate recovery by semialgebraic geometry, uniqueness through size eight, and the endpoint-rich curvature certificate. The strict Bernoulli tangent used in the mixed-binary theorem proves the size-one ridge on mixed data. |
| cmb, all free | Criterion | **Checked.** Separated any noninterior empirical moment by a linear functional. The sign of its height coefficient restricts exposed support to endpoints, a consecutive pair, or one cell. The other slices have maxima, and a finite capped scan attains. Constants and mixed binary data have explicit finite fits. |
| cmb, all free | Failure | **Checked.** A two-cell conditioning bound excludes every finite fit at every compatible size for both exceptional histograms. At the smallest compatible size a fixed-size face realizes the supremum. On mixed binary data the adjacent face at each compatible size above one ties the size-one Bernoulli optimum. |
| cmb, all free | Uniqueness | **Open.** Proved the constant maximizing sets and the exact conditional maximizer graph in ordinary attained strata. The remaining full-support tests are the size-two solution, size-three frequency identity, and the prime-exponent proportionality test for rational dispersion. |
| cmb, nu | Uniqueness | **Open for effective isolation.** Proved the unique positive odds root, its algebraicity, algebraicity of the exact conditional height, and the complete capped argmax comparison, including the constant strata. Used compact profile continuity and exact endpoint comparisons to prove the global tie at the first zero specified in the source. The public earlier zero-dispersion theorem supplies the smallest-size comparison. An executable root isolation and exact algebraic comparison procedure certified in Lean remains. |
| bnbinom, all free | Criterion | **Open.** Reviewed the zero-atom proof and the stationary-coordinate results. The fixed-hold gamma-ratio bounds do not control all varying shapes. The compact face comparison needs the joint NB and Poisson classification, the variance-versus-mean rule in the same likelihood chart, and a finite-height comparison at equality on the full labeled seam. |
| bnbinom, all free | Failure | **Open.** At fixed beta, compactified the free-shape total and ratio, with both endpoint and defective limits checked. For three varying shapes, ratios in the `rb/(alpha+r+b)` chart identify candidate NB and Poisson recurrences. Below unit limiting dispersion, the formal normalization could use the mixture identity for the mean and Markov’s inequality. The existing gamma chart lacks that distribution-wide identity. At unit limiting dispersion one must also control the defective zero-atom limit and show it cannot give positive likelihood on positive-count data. |
| bnbinom, alpha | Uniqueness | **Open.** The public theorem establishes the exact `(0,200)` label tie. In the binary proof, at fixed `p1/p0` the Euler product factors increase with the sum of the two shapes, with strictness from its second factor. Isolated the second factor to pass a strict comparison to the infinite-product limit and ruled out off-diagonal binary winners. Strict diagonal score monotonicity still needs locally uniform derivative control or a strict convexity argument that retains one strict summand at the limit. The golden strict exclusion requires replay of the rational log-gamma interval cover and both exterior bounds in Lean. |
| bnbinom, beta | Criterion | **Checked.** Used the fixed-hold compactification and optimized NB law to promote an equal-or-better finite witness to attainment. The characterization states the two stationary equations through the unique conditional size root. The analytic derivative of the profiled score is unused and remains unformalized. |
| bnbinom, beta | Failure | **Checked.** Proved bounded and expanding total traces, then applied the subsequence contradiction at each count to every maximizing escape. Joined the all-zero atom and the optimized NB face with its fitted mean, including equality with a finite winner. |
| bnbinom, beta | Uniqueness | **Open.** Proved exact countwise rational reduction at integer holds. No quantifier elimination, finite analytic stationary-set argument, or uniform second-order NB comparison is formalized. |
| bnbinom, size | Criterion | **Checked.** Transported the zero exclusion, stationarity through the conditional root, and fitted NB comparison, including equality, by the exact shape swap. |
| bnbinom, size | Failure | **Checked.** The swap transports the joined all-zero and positive-count classification for every maximizing escape, with the held size as the limiting NB shape. |
| bnbinom, size | Uniqueness | **Open.** The integer rational formula and the swap are checked. Exact set extraction and the compact stationary-set and second-order arguments still need formalization. |

## Checked Lean statements

### Beta-binomial

#### `UnivariateML.Base8bBBinom.all_free_constant`

The unrestricted all-zero maximum is the size-zero shape ridge. Positive constants have an unattained supremum of one. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L109)

- `bbinom`, `all free` · criterion, failure, uniqueness

```lean
theorem all_free_constant {n c : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c) :
    let feasible : Set (ℕ × ℝ × ℝ)
```

#### `UnivariateML.Base8bBBinom.endpoint_profile_bound`

The endpoint-only log likelihood is bounded by its fitted Bernoulli height, with strict loss at sizes greater than one. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L295)

- `bbinom`, `all free` · criterion, uniqueness

```lean
theorem endpoint_profile_bound {m n : ℕ} (hm : 0 < m) (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0 ∨ x i = m)
    (hp : proportion 1 (fun i ↦ if x i = 0 then 0 else 1) ∈ Ioo 0 1) :
    let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
    let B := binomialLogLik 1 y (proportion 1 y)
    (∀ a > 0, ∀ b > 0, logLik m x a b ≤ B ∧ (1 < m → logLik m x a b < B)) ∧
      (m = 1 → logLik m x (proportion 1 y) (1 - proportion 1 y) = B)
```

#### `UnivariateML.Base8bBBinom.held_size_criterion`

Attainment holds exactly at size zero, for mixed Bernoulli data, or with an interior count and positive dispersion excess at size at least two. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L387)

- `bbinom`, `size` · criterion

```lean
theorem held_size_criterion {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      m = 0 ∨
      (m = 1 ∧ (∃ i, x i = 0) ∧ ∃ i, x i = 1) ∨
      (2 ≤ m ∧ (∃ i, 0 < x i ∧ x i < m) ∧
        (m : ℝ) * proportion m x * (1 - proportion m x) < variance x)
```

#### `UnivariateML.Base8bBBinom.underdispersed_attained_size_prefix`

Every identifiable attained fixed-size profile lies strictly below the dispersion threshold when the sample variance is below its mean. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L446)

- `bbinom`, `all free` · criterion, uniqueness

```lean
theorem underdispersed_attained_size_prefix {m n : ℕ} (hm : 2 ≤ m) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) (hi : ∃ i, 0 < x i ∧ x i < m)
    (hv : variance x < mean x) :
    (∃ q ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q) ↔
      (m : ℝ) < mean x ^ 2 / (mean x - variance x)
```

#### `UnivariateML.Base8bBBinom.held_alpha_size_unique`

At positive held size all attained beta fits coincide, and at size zero every positive beta maximizes. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L631)

- `bbinom`, `alpha+size` · uniqueness

```lean
theorem held_alpha_size_unique {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {a : ℝ} (ha : 0 < a) :
    (m = 0 → ∀ b > 0, IsMaxOn (logLik m x a) (Ioi 0) b) ∧
    (0 < m → ∀ b ∈ Ioi 0, ∀ c ∈ Ioi 0,
      IsMaxOn (logLik m x a) (Ioi 0) b → IsMaxOn (logLik m x a) (Ioi 0) c → b = c)
```

#### `UnivariateML.Base8bBBinom.held_beta_size_unique`

At positive held size all attained alpha fits coincide, and at size zero every positive alpha maximizes. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L665)

- `bbinom`, `beta+size` · uniqueness

```lean
theorem held_beta_size_unique {m n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m)
    {b : ℝ} (hb : 0 < b) :
    (m = 0 → ∀ a > 0, IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) a) ∧
    (0 < m → ∀ a ∈ Ioi 0, ∀ c ∈ Ioi 0,
      IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) a →
      IsMaxOn (fun a ↦ logLik m x a b) (Ioi 0) c → a = c)
```

#### `UnivariateML.Base8bBBinom.separated_peak_witness`

Checks the exact four rational comparisons around the size-38 binomial peak of (7,8,13), including the larger size-13 finite witness. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L679)

- `bbinom`, `all free` · uniqueness

```lean
theorem separated_peak_witness :
    let x : Fin 3 → ℕ := ![7, 8, 13]
    let binom (N : ℕ)
```

#### `UnivariateML.Base8bBBinom.fixed_size_binomial_trace`

Simultaneously growing total shape at fixed size gives the binomial trace for every limiting mixing proportion. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L707)

- `bbinom`, `all free` · failure

```lean
theorem fixed_size_binomial_trace {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hk : k ≤ m) {p : ℝ}
    (hS : Tendsto (fun t ↦ a t+b t) l atTop)
    (hP : Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 p)) :
    Tendsto (fun t ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a t+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b t+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a t+b t+(j : ℝ)))) l
      (𝓝 ((m.choose k : ℝ) * p^k * (1-p)^(m-k)))
```

#### `UnivariateML.Base8bBBinom.fixed_size_two_point_trace`

Simultaneously shrinking total shape at fixed positive size gives the endpoint-mixture trace. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L773)

- `bbinom`, `all free` · failure

```lean
theorem fixed_size_two_point_trace {ι : Type*} {l : Filter ι} {a b : ι → ℝ}
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < b t) {m k : ℕ} (hm : 0 < m) (hk : k ≤ m)
    {p : ℝ} (hS : Tendsto (fun t ↦ a t+b t) l (𝓝 0))
    (hP : Tendsto (fun t ↦ a t/(a t+b t)) l (𝓝 p)) :
    Tendsto (fun t ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a t+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b t+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a t+b t+(j : ℝ)))) l
      (𝓝 (if k = 0 then 1-p else if k = m then p else 0))
```

#### `UnivariateML.Base8bBBinom.integer_held_alpha_rational_mass`

The fixed-count mass at integer alpha has the written rational extension in size and beta. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L862)

- `bbinom`, `alpha` · uniqueness

```lean
theorem integer_held_alpha_rational_mass {A N k : ℕ} (hk : k ≤ N)
    {b : ℝ} (hb : 0 < b) :
    (N.choose k : ℝ) *
        ((∏ j ∈ Finset.range k, ((A : ℝ)+j)) *
          (∏ j ∈ Finset.range (N-k), (b+j))) /
        (∏ j ∈ Finset.range N, ((A : ℝ)+b+j)) =
      ((∏ j ∈ Finset.range A, (b+j)) / (∏ j ∈ Finset.range A, (b+(N : ℝ)+j))) *
        (((∏ j ∈ Finset.range k, ((N : ℝ)-j)) *
          (∏ j ∈ Finset.range k, ((A : ℝ)+j))) /
          ((k.factorial : ℝ) * (∏ j ∈ Finset.range k, (b+(N : ℝ)-k+j))))
```

#### `UnivariateML.Base8bBBinom.underdispersed_global_criterion`

On underdispersed samples with an interior observation at the smallest compatible size, attainment is equivalent to a finite prefix witness meeting the best binomial. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L905)

- `bbinom`, `all free` · criterion

```lean
theorem underdispersed_global_criterion {n M : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hM : ∃ i, x i = M) (hi : ∃ i, 0 < x i ∧ x i < M)
    (hv : variance x < mean x) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let cutoff := mean x ^ 2 / (mean x - variance x)
    let B := sSup (BinomialSize.profile x '' BinomialSize.feasible x)
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ∃ q ∈ D, (q.1 : ℝ) < cutoff ∧ B ≤ L q
```

#### `UnivariateML.Base8bBBinom.held_beta_uniform_positive_cell`

Each positive cell vanishes uniformly over the free positive shape as size grows. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1148)

- `bbinom`, `beta` · failure

```lean
theorem held_beta_uniform_positive_cell {k : ℕ} (hk : 0 < k) {b : ℝ} (hb : 0 < b)
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ m : ℕ in atTop, ∀ a > 0,
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b+(j : ℝ)))) /
        (∏ j ∈ Finset.range m, (a+b+(j : ℝ))) < ε
```

#### `UnivariateML.Base8bBBinom.held_beta_classification`

Recovers the complete zero-data ray, excludes positive constants and characterizes every finite size co-winner on nonconstant data. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1427)

- `bbinom`, `beta` · uniqueness

```lean
theorem held_beta_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    ((∀ i, x i = 0) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 0) ∧
    ((∃ c > 0, ∀ i, x i = c) → ¬ ∃ q ∈ D, IsMaxOn L D q) ∧
    ((∀ c, ¬ ∀ i, x i = c) →
      ∃ K : ℕ, ∃ a : ℕ → ℝ,
        (∀ m, (∀ i, x i ≤ m) → a m ∈ Ioi 0 ∧
          ∀ c ∈ Ioi 0, IsMaxOn (fun t ↦ logLik m x t b) (Ioi 0) c ↔ c = a m) ∧
        (∃ q ∈ D, IsMaxOn L D q) ∧
        ∀ q ∈ D, IsMaxOn L D q ↔ q.1 ≤ K ∧ q.2 = a q.1 ∧
          ∀ m ≤ K, (∀ i, x i ≤ m) → L (m,a m) ≤ L q)
```

#### `UnivariateML.Base8bBBinom.held_beta_criterion`

Finite attainment holds exactly on nonconstant data and the all-zero sample. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1449)

- `bbinom`, `beta` · criterion

```lean
theorem held_beta_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x q.2 b
    (∃ q ∈ D, IsMaxOn L D q) ↔ (∀ i, x i = 0) ∨ (∀ c, ¬ ∀ i, x i = c)
```

#### `UnivariateML.Base8bBBinom.held_beta_growing_zero_atom_iff`

At growing size the zero mass approaches one exactly when alpha times log size tends to zero. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1552)

- `bbinom`, `beta` · failure

```lean
theorem held_beta_growing_zero_atom_iff {ι : Type*} {l : Filter ι}
    {N : ι → ℕ} {a : ι → ℝ} {b : ℝ} (hb : 0 < b)
    (ha : ∀ᶠ t in l, 0 < a t) (hN : Tendsto N l atTop) :
    Tendsto (fun t ↦ (∏ j ∈ Finset.range (N t), (b+(j : ℝ))) /
      (∏ j ∈ Finset.range (N t), (a t+b+(j : ℝ)))) l (𝓝 1) ↔
      Tendsto (fun t ↦ a t * Real.log (N t : ℝ)) l (𝓝 0)
```

#### `UnivariateML.Base8bBBinom.held_beta_nonconstant_no_escape`

Every positive nonconstant-data superlevel lies inside finitely many sizes and a bounded positive shape interval. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1691)

- `bbinom`, `beta` · failure

```lean
theorem held_beta_nonconstant_no_escape {n : ℕ} (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {b ε : ℝ} (hb : 0 < b) (hε : 0 < ε) :
    ∃ K : ℕ, ∃ δ > 0, ∃ A : ℝ, ∀ m a, (∀ i, x i ≤ m) → 0 < a →
      ε ≤ Real.exp (logLik m x a b) → m ≤ K ∧ δ ≤ a ∧ a ≤ A
```

#### `UnivariateML.Base8bBBinom.held_beta_failure`

Joins the upper-atom path for positive constants, compact superlevels on nonconstant data and the bounded and growing zero-data escapes. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1704)

- `bbinom`, `beta` · failure

```lean
theorem held_beta_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ Real.exp (logLik q.1 x q.2 b)
    (∀ c > 0, (∀ i, x i = c) → IsLUB (L '' D) 1 ∧ (∀ q ∈ D, L q < 1) ∧
      Tendsto (fun a ↦ L (c,a)) atTop (𝓝 1)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ ε > 0,
      ∃ K : ℕ, ∃ δ > 0, ∃ A : ℝ, ∀ q ∈ D, ε ≤ L q →
        q.1 ≤ K ∧ δ ≤ q.2 ∧ q.2 ≤ A) ∧
    ((∀ i, x i = 0) →
      (∀ q ∈ D, L q = 1 ↔ q.1 = 0) ∧
      (∀ m > 0, Tendsto (fun a ↦ L (m,a)) (𝓝[>] 0) (𝓝 1)) ∧
      ∀ {N : ℕ → ℕ} {a : ℕ → ℝ}, (∀ᶠ j in atTop, 0 < a j) →
        Tendsto N atTop atTop →
        (Tendsto (fun j ↦ L (N j,a j)) atTop (𝓝 1) ↔
          Tendsto (fun j ↦ a j * Real.log (N j : ℝ)) atTop (𝓝 0)))
```

#### `UnivariateML.Base8bBBinom.held_alpha_growing_trace`

The entire growing-size count law converges to the held-alpha negative-binomial formula, including endpoint traces. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L1843)

- `bbinom`, `alpha` · failure

```lean
theorem held_alpha_growing_trace {ι : Type*} {l : Filter ι} {N : ι → ℕ}
    {b : ι → ℝ} {a p : ℝ} (ha : 0 < a) (hp : p ∈ Icc 0 1)
    (hb : ∀ᶠ t in l, 0 < b t) (hN : Tendsto N l atTop)
    (hP : Tendsto (fun t ↦ b t/(b t + (N t : ℝ))) l (𝓝 p)) (k : ℕ) :
    Tendsto (fun t ↦
      ((N t).choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (N t-k), (b t+(j : ℝ)))) /
          (∏ j ∈ Finset.range (N t), (a+b t+(j : ℝ)))) l
      (𝓝 (p^a * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) / k.factorial) * (1-p)^k))
```

#### `UnivariateML.Base8bBBinom.held_alpha_nonconstant_criterion`

On nonconstant data attainment holds exactly when a conditional beta optimum reaches the fitted negative-binomial height. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2108)

- `bbinom`, `alpha` · criterion

```lean
theorem held_alpha_nonconstant_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x a q.2
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ∃ q ∈ D, IsMaxOn (logLik q.1 x a) (Ioi 0) q.2 ∧ NegBinomial.profile x a ≤ L q
```

#### `UnivariateML.Base8bBBinom.held_alpha_criterion`

All-zero data attain, positive constants do not, and every nonconstant conditional profile has a unique fit. A finite global fit exists exactly when a profile meets the fitted negative-binomial height. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2137)

- `bbinom`, `alpha` · criterion

```lean
theorem held_alpha_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ logLik q.1 x a q.2
    ((∃ q ∈ D, IsMaxOn L D q) ↔
      (∀ i, x i = 0) ∨
      ((∀ c, ¬ ∀ i, x i = c) ∧ ∃ q ∈ D,
        IsMaxOn (logLik q.1 x a) (Ioi 0) q.2 ∧ NegBinomial.profile x a ≤ L q)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ m, (∀ i, x i ≤ m) →
      ∃! b, b ∈ Ioi 0 ∧ IsMaxOn (logLik m x a) (Ioi 0) b)
```

#### `UnivariateML.Base8bBBinom.held_alpha_nonconstant_maximizing_escape`

A maximizing escaping sequence on nonconstant data has growing size and converges to the fitted negative-binomial height and probability. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2227)

- `bbinom`, `alpha` · failure

```lean
theorem held_alpha_nonconstant_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hnc : ∀ c, ¬ ∀ i, x i = c) {a : ℝ} (ha : 0 < a)
    {N : ℕ → ℕ} {b : ℕ → ℝ} (hNx : ∀ j i, x i ≤ N j) (hb : ∀ j, 0 < b j)
    (he : ∀ K : ℕ, ∀ δ A : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (N j ≤ K ∧ b j ∈ Icc δ A))
    {H : ℝ} (hH : ∀ m, (∀ i, x i ≤ m) → ∀ c > 0, Real.exp (logLik m x a c) ≤ H)
    (hlim : Tendsto (fun j ↦ Real.exp (logLik (N j) x a (b j))) atTop (𝓝 H)) :
    H = Real.exp (NegBinomial.profile x a) ∧ Tendsto N atTop atTop ∧
      Tendsto (fun j ↦ b j/(b j+(N j : ℝ))) atTop (𝓝 (NegBinomial.prob x a))
```

#### `UnivariateML.Base8bBBinom.held_alpha_failure`

Joins the excluded upper atom, the fitted negative-binomial escape, and the size-zero ridge with its bounded and growing lower-atom escapes. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2385)

- `bbinom`, `alpha` · failure

```lean
theorem held_alpha_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℕ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ 0 < q.2}
    let L := fun q : ℕ × ℝ ↦ Real.exp (logLik q.1 x a q.2)
    (∀ c > 0, (∀ i, x i = c) → IsLUB (L '' D) 1 ∧ (∀ q ∈ D, L q < 1) ∧
      Tendsto (fun b ↦ L (c,b)) (𝓝[>] 0) (𝓝 1)) ∧
    ((∀ c, ¬ ∀ i, x i = c) → ∀ {N : ℕ → ℕ} {b : ℕ → ℝ},
      (∀ j, (N j,b j) ∈ D) →
      (∀ K : ℕ, ∀ δ A : ℝ, 0 < δ →
        ∀ᶠ j in atTop, ¬ (N j ≤ K ∧ b j ∈ Icc δ A)) →
      ∀ {H : ℝ}, (∀ q ∈ D, L q ≤ H) →
        Tendsto (fun j ↦ L (N j,b j)) atTop (𝓝 H) →
        H = Real.exp (NegBinomial.profile x a) ∧ Tendsto N atTop atTop ∧
          Tendsto (fun j ↦ b j/(b j+(N j : ℝ))) atTop (𝓝 (NegBinomial.prob x a))) ∧
    ((∀ i, x i = 0) →
      (∀ q ∈ D, L q = 1 ↔ q.1 = 0) ∧
      (∀ m > 0, Tendsto (fun b ↦ L (m,b)) atTop (𝓝 1)) ∧
      ∀ {N : ℕ → ℕ} {b : ℕ → ℝ}, (∀ j, 0 < b j) → Tendsto N atTop atTop →
        (Tendsto (fun j ↦ L (N j,b j)) atTop (𝓝 1) ↔
          Tendsto (fun j ↦ b j/(N j : ℝ)) atTop atTop))
```

#### `UnivariateML.Base8bBBinom.mixed_binary_maximizers`

Mixed zero-one data attain only at size one and exactly along the empirical-proportion shape ridge. Each positive alpha intersects the mixed binary global maximizing ridge in one beta. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2487)

- `bbinom`, `all free` · criterion, uniqueness
- `bbinom`, `alpha` · uniqueness

```lean
theorem mixed_binary_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) (h0 : ∃ i, x i = 0) (h1 : ∃ i, x i = 1) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let p := proportion 1 x
    (∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1/(q.2.1+q.2.2) = p) ∧
    (∀ a > 0, ∃! b, (1,a,b) ∈ D ∧ IsMaxOn L D (1,a,b))
```

#### `UnivariateML.Base8bBBinom.held_size_polynomial_argmax`

The held-size argmax is exactly a universally quantified polynomial comparison after clearing positive denominators. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2575)

- `bbinom`, `size` · uniqueness

```lean
theorem held_size_polynomial_argmax {m n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    let A := fun a b : ℝ ↦ ∏ i, (∏ j ∈ Finset.range (x i), (a+(j : ℝ))) *
      (∏ j ∈ Finset.range (m-x i), (b+(j : ℝ)))
    let B := fun a b : ℝ ↦ (∏ j ∈ Finset.range m, (a+b+(j : ℝ)))^n
    ∀ q : ℝ × ℝ, q ∈ domain →
      (IsMaxOn (fun q : ℝ × ℝ ↦ logLik m x q.1 q.2) domain q ↔
        ∀ r : ℝ × ℝ, r ∈ domain → A r.1 r.2 * B q.1 q.2 ≤ A q.1 q.2 * B r.1 r.2)
```

#### `UnivariateML.Base8bBBinom.fixed_size_point_mass_traces`

The lower and upper atoms are realized countwise at fixed positive beta. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2603)

- `bbinom`, `size` · failure

```lean
theorem fixed_size_point_mass_traces {m k : ℕ} (hk : k ≤ m) {b : ℝ} (hb : 0 < b) :
    let P := fun a : ℝ ↦
      (m.choose k : ℝ) * ((∏ j ∈ Finset.range k, (a+(j : ℝ))) *
        (∏ j ∈ Finset.range (m-k), (b+(j : ℝ)))) /
          (∏ j ∈ Finset.range m, (a+b+(j : ℝ)))
    Tendsto P (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
      Tendsto P atTop (𝓝 (if k = m then 1 else 0))
```

#### `UnivariateML.Base8bBBinom.held_size_failure`

Joins the strictly excluded binomial, point-mass and mixed-endpoint heights at size at least two, with recovering likelihood paths for all branches. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2615)

- `bbinom`, `size` · failure

```lean
theorem held_size_failure {m n : ℕ} (hm : 2 ≤ m) (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∀ i, x i ≤ m) :
    let L := fun q : ℝ × ℝ ↦ logLik m x q.1 q.2
    let Excluded := fun v ↦ IsLUB (L '' domain) v ∧ ∀ q ∈ domain, L q < v
    ((∃ i, 0 < x i ∧ x i < m) →
      variance x ≤ (m : ℝ) * proportion m x * (1 - proportion m x) →
      Excluded (binomialLogLik m x (proportion m x)) ∧
      Tendsto (fun s : ℝ ↦ logLik m x (proportion m x / s)
        ((1 - proportion m x) / s)) (𝓝[>] 0) (𝓝 (binomialLogLik m x (proportion m x)))) ∧
    ((∀ i, x i = 0) → Excluded 0 ∧
      ∀ b > 0, Tendsto (fun a ↦ L (a,b)) (𝓝[>] 0) (𝓝 0)) ∧
    ((∀ i, x i = m) → Excluded 0 ∧
      ∀ b > 0, Tendsto (fun a ↦ L (a,b)) atTop (𝓝 0)) ∧
    ((∀ i, x i = 0 ∨ x i = m) → (∃ i, x i = 0) → (∃ i, x i = m) →
      let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
      let p := proportion 1 y
      let B := binomialLogLik 1 y p
      Excluded B ∧
        Tendsto (fun s : ℝ ↦ logLik m x (p*s) ((1-p)*s)) (𝓝[>] 0) (𝓝 B))
```

#### `UnivariateML.Base8bBBinom.all_free_mixed_endpoint_failure`

The saturated mixture on zero and one positive count above one strictly beats every finite compatible fit and is approached at the smallest compatible size. [Lean source](../../lean/UnivariateML/Family/Base8bBBinom.lean#L2676)

- `bbinom`, `all free` · criterion, failure, uniqueness

```lean
theorem all_free_mixed_endpoint_failure {n u : ℕ} (hn : 0 < n) (hu : 2 ≤ u)
    (x : Fin n → ℕ) (hx : ∀ i, x i = 0 ∨ x i = u)
    (h0 : ∃ i, x i = 0) (hu' : ∃ i, x i = u) :
    let D : Set (ℕ × ℝ × ℝ) := {q | (∀ i, x i ≤ q.1) ∧ q.2 ∈ domain}
    let L := fun q : ℕ × ℝ × ℝ ↦ logLik q.1 x q.2.1 q.2.2
    let y : Fin n → ℕ := fun i ↦ if x i = 0 then 0 else 1
    let p := proportion 1 y
    let H := binomialLogLik 1 y p
    IsLUB (L '' D) H ∧ (∀ q ∈ D, L q < H) ∧
      Tendsto (fun t : ℝ ↦ L (u,p*t,(1-p)*t)) (𝓝[>] 0) (𝓝 H)
```

### Conway–Maxwell–binomial

#### `UnivariateML.Base8bCMB.all_free_pair_failure`

The capped likelihood supremum is the saturated pair height for the two exceptional histograms. A mixed binary fit attains that height at size one while larger adjacent faces tie it. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L166)

- `cmb`, `all free` · failure

```lean
theorem all_free_pair_failure {n B l u : ℕ} (hn : 0 < n) (huB : u ≤ B)
    (x : Fin n → ℕ) (hx : ∀ i, x i = l ∨ x i = u) (hlu : l < u)
    (hl : ∃ i, x i = l) (hu : ∃ i, x i = u) (he : l = 0 ∨ u = l+1) :
    let z := (Finset.univ.filter fun i ↦ x i = l).card
    let o := (Finset.univ.filter fun i ↦ x i = u).card
    let H := pairHeight z o
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    IsLUB (L '' D) H ∧
      (2 ≤ u → ∀ q ∈ D, L q < H) ∧
      (u = 1 →
        (∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1 = (o : ℝ)/(o+z)) ∧
        ∀ N ≤ B, 2 ≤ N →
          IsLUB ((fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2) ''
            (Icc 0 1 ×ˢ univ)) H ∧
            ∀ p ∈ Icc 0 1, ∀ ν, sizeLikelihood x N p ν < H)
```

#### `UnivariateML.Base8bCMB.all_free_constant_maximizers`

Identifies all constant-data maximizers, including the two ways zero data can attain their atom. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L453)

- `cmb`, `all free` · uniqueness

```lean
theorem all_free_constant_maximizers {n B c : ℕ} (hn : 0 < n) (hc : c ≤ B)
    (x : Fin n → ℕ) (hx : ∀ i, x i = c) :
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    IsLUB (L '' D) 1 ∧
    ∀ q ∈ D, IsMaxOn L D q ↔
      (q.1 = 0 ∧ c = 0) ∨ (q.2.1 = 0 ∧ c = 0) ∨ (q.2.1 = 1 ∧ q.1 = c)
```

#### `UnivariateML.Base8bCMB.all_free_criterion`

On the capped size domain, all feasible samples attain except the two mixed pairs named in the atlas. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L550)

- `cmb`, `all free` · criterion

```lean
theorem all_free_criterion {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hB : ∀ i, x i ≤ B) :
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ¬ ∃ l u : ℕ, 2 ≤ u ∧ l < u ∧ (l = 0 ∨ u = l+1) ∧
        (∀ i, x i = l ∨ x i = u) ∧ (∃ i, x i = l) ∧ (∃ i, x i = u)
```

#### `UnivariateML.Base8bCMB.all_free_maximizer_profiles`

Every identifiable attained size has one conditional optimum, and all sizes tied at the largest height are retained. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L621)

- `cmb`, `all free` · uniqueness

```lean
theorem all_free_maximizer_profiles {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) (hlarge : ∃ i, 2 ≤ x i)
    (hbad : ¬ ∃ l u : ℕ, 2 ≤ u ∧ l < u ∧ (l = 0 ∨ u = l+1) ∧
      (∀ i, x i = l ∨ x i = u) ∧ (∃ i, x i = l) ∧ (∃ i, x i = u)) :
    let S : Set ℕ := {N | N ≤ B ∧ ∀ i, x i ≤ N}
    let P := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ∈ S ∧ q.2 ∈ P}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    ∃ best : ℕ → ℝ × ℝ,
      (∀ N ∈ S, ∀ q ∈ P,
        IsMaxOn (fun q : ℝ × ℝ ↦ L (N,q)) P q ↔ q = best N) ∧
      (∀ N ∈ S, best N ∈ P) ∧
      ∀ q ∈ D, IsMaxOn L D q ↔
        q.2 = best q.1 ∧ IsMaxOn (fun N ↦ L (N, best N)) S q.1
```

#### `UnivariateML.Base8bCMB.rational_dispersion_weight`

The fixed-size weight for each count and rational dispersion is the unique positive solution of its power equation. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L681)

- `cmb`, `nu` · uniqueness

```lean
theorem rational_dispersion_weight {N d : ℕ} (hd : 0 < d) (a : ℤ)
    (k : Fin (N+1)) :
    let w := Real.exp ((((a : ℝ)/d) * height N k))
    0 < w ∧ w^d = (N.choose k.val : ℝ)^a ∧
      ∀ u > 0, u^d = (N.choose k.val : ℝ)^a → u = w
```

#### `UnivariateML.Base8bCMB.held_dispersion_height_continuous`

The maximized probability likelihood is continuous in held dispersion on each compact interval. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L806)

- `cmb`, `nu` · uniqueness

```lean
theorem held_dispersion_height_continuous {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ N) (a b : ℝ) :
    ContinuousOn (fun ν ↦ sSup ((fun p ↦ sizeLikelihood x N p ν) '' Icc 0 1))
      (Icc a b)
```

#### `UnivariateML.Base8bCMB.held_dispersion_exact_tie`

For (7,8,13) and every cap at least 38, size 13 and a larger size tie globally at the first zero of the capped height difference in (0,1). [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L885)

- `cmb`, `nu` · uniqueness

```lean
theorem held_dispersion_exact_tie {B : ℕ} (hB : 38 ≤ B) :
    let x : Fin 3 → ℕ := ![7,8,13]
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    let H := fun N ν ↦ sSup ((fun p ↦ sizeLikelihood x N p ν) '' Icc 0 1)
    let S := Finset.Icc 14 B
    let hS : S.Nonempty := ⟨38, Finset.mem_Icc.mpr ⟨by decide,hB⟩⟩
    ∃ ν ∈ Ioo (0 : ℝ) 1,
      IsLeast {v ∈ Icc 0 1 | S.sup' hS (fun N ↦ H N v) = H 13 v} ν ∧
      ∃ p, ∃ q ∈ D, 13 < q.1 ∧ (13,p) ∈ D ∧
      IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D (13,p) ∧
      IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D q
```

#### `UnivariateML.Base8bCMB.held_dispersion_positive_odds_root`

The held-dispersion weighted mean equation has exactly one positive odds root for every interior mean. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L1107)

- `cmb`, `nu` · uniqueness

```lean
theorem held_dispersion_positive_odds_root {N : ℕ} {μ : ℝ}
    (hμ : μ ∈ Ioo (0 : ℝ) N) (ν : ℝ) :
    ∃! r : ℝ, 0 < r ∧
      (∑ k : Fin (N+1), ((k.val : ℝ)-μ) * Real.exp (ν*height N k) * r^k.val) = 0
```

#### `UnivariateML.Base8bCMB.rational_dispersion_algebraic_odds`

At a rational hold and an interior rational mean, the unique positive polynomial odds root is real algebraic. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L1115)

- `cmb`, `nu` · uniqueness

```lean
theorem rational_dispersion_algebraic_odds {N d : ℕ} (hd : 0 < d) (a : ℤ)
    (μ : ℚ) (hμ : (μ : ℝ) ∈ Ioo (0 : ℝ) (N : ℝ)) :
    let w := fun k : Fin (N+1) ↦ Real.exp (((a : ℝ)/d)*height N k)
    ∃! r : ℝ, 0 < r ∧ IsAlgebraic ℚ r ∧
      (∑ k : Fin (N+1), ((k.val : ℝ)-(μ : ℝ))*w k*r^k.val) = 0
```

#### `UnivariateML.Base8bCMB.rational_dispersion_algebraic_profile`

At an interior empirical mean the maximizing odds and exact polynomial-form probability height are real algebraic. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L1156)

- `cmb`, `nu` · uniqueness

```lean
theorem rational_dispersion_algebraic_profile {N n d : ℕ}
    (hn : 0 < n) (hd : 0 < d) (a : ℤ) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ N)
    (h₀ : ∃ i, 0 < x i) (h₁ : ∃ i, x i < N) :
    let ν := (a : ℝ)/d
    let w := fun k : Fin (N+1) ↦ Real.exp (ν*height N k)
    let y : Fin n → Fin (N+1) := fun i ↦ ⟨x i,Nat.lt_succ_of_le (hx i)⟩
    let μ := (∑ i, (x i : ℝ)) / n
    ∃ r > 0, IsAlgebraic ℚ r ∧
      (∑ k, ((k.val : ℝ)-μ)*w k*r^k.val) = 0 ∧
      let H := ∏ i, (w (y i) * r^(x i)) / (∑ k, w k*r^k.val)
      IsAlgebraic ℚ H ∧
      sizeLikelihood x N (r/(1+r)) ν = H ∧
      IsMaxOn (fun p ↦ sizeLikelihood x N p ν) (Icc 0 1) (r/(1+r))
```

#### `UnivariateML.Base8bCMB.rational_dispersion_capped_argmax`

Every compatible conditional height is algebraic at a rational hold, and the finite exact comparison retains all global co-winners, including the constant strata. [Lean source](../../lean/UnivariateML/Family/Base8bCMB.lean#L1288)

- `cmb`, `nu` · uniqueness

```lean
theorem rational_dispersion_capped_argmax {B n d : ℕ}
    (hn : 0 < n) (hd : 0 < d) (a : ℤ) (x : Fin n → ℕ) :
    let S : Set ℕ := {N | N ≤ B ∧ ∀ i, x i ≤ N}
    let D : Set (ℕ × ℝ) := {q | q.1 ∈ S ∧ q.2 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ ↦ sizeLikelihood x q.1 q.2 ((a : ℝ)/d)
    ∃ H : ℕ → ℝ,
      (∀ N ∈ S, IsAlgebraic ℚ (H N) ∧
        (∃ p ∈ Icc 0 1, L (N,p) = H N) ∧ ∀ p ∈ Icc 0 1, L (N,p) ≤ H N) ∧
      ∀ q ∈ D, IsMaxOn L D q ↔ L q = H q.1 ∧ ∀ N ∈ S, H N ≤ H q.1
```

### Beta-negative-binomial

#### `UnivariateML.Base8bBNBinom.integer_held_shape_count_mass`

At a positive integer symmetric hold, the gamma-chart count mass is exactly a rational function of the two free shapes. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L71)

- `bnbinom`, `beta` · uniqueness
- `bnbinom`, `size` · uniqueness

```lean
theorem integer_held_shape_count_mass {r a : ℝ} (hr : 0 < r) (ha : 0 < a)
    {b : ℕ} (k : ℕ) :
    gammaMass k r a b =
      ((∏ j ∈ Finset.range b, (a + j)) / (∏ j ∈ Finset.range b, (a+r+j))) *
        (((∏ j ∈ Finset.range k, (r+j)) * (∏ j ∈ Finset.range k, ((b : ℝ)+j))) /
          ((∏ j ∈ Finset.range k, (a+r+(b : ℝ)+j)) * k.factorial)) ∧
    gammaMass k (b : ℝ) a r = gammaMass k r a (b : ℝ)
```

#### `UnivariateML.Base8bBNBinom.held_beta_growing_total`

A diverging free-shape total and limiting ratio give the negative-binomial face, including both degenerate endpoint limits. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L108)

- `bnbinom`, `beta` · failure

```lean
theorem held_beta_growing_total {ι : Type*} {l : Filter ι} {a r : ι → ℝ} {b p : ℝ}
    (hb : 0 < b) (hp : p ∈ Icc 0 1)
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < r t)
    (hT : Tendsto (fun t ↦ a t + r t) l atTop)
    (hP : Tendsto (fun t ↦ a t / (a t+r t)) l (𝓝 p)) (k : ℕ) :
    Tendsto (fun t ↦ gammaMass k (r t) (a t) b) l
      (𝓝 (p^b * (∏ j ∈ Finset.range k, ((1-p)*(b+j)/((j : ℝ)+1)))))
```

#### `UnivariateML.Base8bBNBinom.held_beta_bounded_boundary`

At a bounded free-shape total, a parameter boundary leaves only an atom at zero whose mass is the limiting alpha fraction. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L179)

- `bnbinom`, `beta` · failure

```lean
theorem held_beta_bounded_boundary {ι : Type*} {l : Filter ι} {a r : ι → ℝ}
    {b p T : ℝ} (hb : 0 < b) (hp : p ∈ Icc 0 1) (hT : 0 ≤ T)
    (hpos : ∀ᶠ t in l, 0 < a t ∧ 0 < r t)
    (hTotal : Tendsto (fun t ↦ a t+r t) l (𝓝 T))
    (hP : Tendsto (fun t ↦ a t/(a t+r t)) l (𝓝 p))
    (hBoundary : T = 0 ∨ p = 0 ∨ p = 1) (k : ℕ) :
    Tendsto (fun t ↦ gammaMass k (r t) (a t) b) l (𝓝 (if k = 0 then p else 0))
```

#### `UnivariateML.Base8bBNBinom.held_beta_positive_maximizing_escape`

On positive-count data, a maximizing escape subsequence converges countwise to the optimized negative binomial, including in the finite-tie case. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L375)

- `bnbinom`, `beta` · failure

```lean
theorem held_beta_positive_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    H = (∏ i, faceMass (x i) b p) ∧
      ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
        Tendsto (fun j ↦ gammaMass k (r (φ j)) (a (φ j)) b) atTop
          (𝓝 (faceMass k b p))
```

#### `UnivariateML.Base8bBNBinom.held_size_positive_maximizing_escape`

The symmetric size hold has the same fitted negative-binomial escape result. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L431)

- `bnbinom`, `size` · failure

```lean
theorem held_size_positive_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) b a r) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) b (a j) (r j)) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    H = (∏ i, faceMass (x i) b p) ∧
      ∃ φ : ℕ → ℕ, StrictMono φ ∧ ∀ k,
        Tendsto (fun j ↦ gammaMass k b (a (φ j)) (r (φ j))) atTop
          (𝓝 (faceMass k b p))
```

#### `UnivariateML.Base8bBNBinom.gamma_likelihood_eq_exp_averageLogMass`

The gamma-chart likelihood agrees with the product-scale version of the existing exposed log objective. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L478)

- `bnbinom`, `beta` · criterion, failure
- `bnbinom`, `size` · criterion, failure

```lean
theorem gamma_likelihood_eq_exp_averageLogMass {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    (∏ i, gammaMass (x i) r a b) =
      exp ((n : ℝ) * FinishBNBinom.averageLogMass x r a b)
```

#### `UnivariateML.Base8bBNBinom.held_beta_maximizing_escape`

Every maximizing escaping sequence converges countwise to the zero atom on zero data or to the optimized fixed-size NB on positive-count data. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L577)

- `bnbinom`, `beta` · failure

```lean
theorem held_beta_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) r a b) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) (r j) (a j) b) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    let m := fun k ↦ if (∀ i, x i = 0) then (if k = 0 then 1 else 0) else faceMass k b p
    H = (∏ i, m (x i)) ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k (r j) (a j) b) atTop (𝓝 (m k))
```

#### `UnivariateML.Base8bBNBinom.held_size_maximizing_escape`

The symmetric size hold has the same countwise escape classification. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L605)

- `bnbinom`, `size` · failure

```lean
theorem held_size_maximizing_escape {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b)
    {a r : ℕ → ℝ} (hpos : ∀ j, 0 < a j ∧ 0 < r j)
    (he : ∀ δ M : ℝ, 0 < δ →
      ∀ᶠ j in atTop, ¬ (a j ∈ Icc δ M ∧ r j ∈ Icc δ M))
    {H : ℝ} (hH : ∀ r > 0, ∀ a > 0, (∏ i, gammaMass (x i) b a r) ≤ H)
    (hlim : Tendsto (fun j ↦ ∏ i, gammaMass (x i) b (a j) (r j)) atTop (𝓝 H)) :
    let p := NegBinomial.prob x b
    let m := fun k ↦ if (∀ i, x i = 0) then (if k = 0 then 1 else 0) else faceMass k b p
    H = (∏ i, m (x i)) ∧ ∀ k,
      Tendsto (fun j ↦ gammaMass k b (a j) (r j)) atTop (𝓝 (m k))
```

#### `UnivariateML.Base8bBNBinom.held_beta_face_comparison`

On positive-count data a finite candidate at or above the fitted NB height implies attainment, including equality. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L715)

- `bnbinom`, `beta` · criterion

```lean
theorem held_beta_face_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) q.1 q.2 b
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    (∃ q ∈ D, IsMaxOn L D q) ↔ ∃ q ∈ D, F ≤ L q
```

#### `UnivariateML.Base8bBNBinom.held_size_face_comparison`

The same finite versus optimized NB comparison holds at fixed size. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L752)

- `bnbinom`, `size` · criterion

```lean
theorem held_size_face_comparison {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) b q.2 q.1
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    (∃ q ∈ D, IsMaxOn L D q) ↔ ∃ q ∈ D, F ≤ L q
```

#### `UnivariateML.Base8bBNBinom.held_beta_criterion`

All-zero samples have no maximum. With a positive count, the finite fit exists exactly when a two-coordinate stationary point meets the fitted NB face. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L846)

- `bnbinom`, `beta` · criterion

```lean
theorem held_beta_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) q.1 q.2 b
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    let A := FinishBNBinom.averageLogMass x
    (∃ q ∈ D, IsMaxOn L D q) ↔
      (∃ i, 0 < x i) ∧ ∃ q ∈ D,
      HasDerivAt (fun r ↦ A r q.2 b) 0 q.1 ∧
      HasDerivAt (fun a ↦ A q.1 a b) 0 q.2 ∧ F ≤ L q
```

#### `UnivariateML.Base8bBNBinom.held_size_criterion`

The size hold transports the two score equations, all-zero nonattainment and the optimized NB threshold. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L895)

- `bnbinom`, `size` · criterion

```lean
theorem held_size_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {b : ℝ} (hb : 0 < b) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let L := fun q : ℝ × ℝ ↦ ∏ i, gammaMass (x i) b q.2 q.1
    let F := ∏ i, faceMass (x i) b (NegBinomial.prob x b)
    let A := FinishBNBinom.averageLogMass x
    (∃ q ∈ D, IsMaxOn L D q) ↔
      (∃ i, 0 < x i) ∧ ∃ q ∈ D,
      HasDerivAt (fun r ↦ A b q.2 r) 0 q.1 ∧
      HasDerivAt (fun a ↦ A b a q.1) 0 q.2 ∧ F ≤ L q
```

#### `UnivariateML.Base8bBNBinom.held_alpha_binary_diagonal`

Each off-diagonal point on binary data is strictly improved by a diagonal point with the same first count ratio. [Lean source](../../lean/UnivariateML/Family/Base8bBNBinom.lean#L983)

- `bnbinom`, `alpha` · uniqueness

```lean
theorem held_alpha_binary_diagonal {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) {a r b : ℝ} (ha : 0 < a) (hr : 0 < r) (hb : 0 < b)
    (hne : r ≠ b) :
    ∃ t > 0, t*t/(a+t+t) = r*b/(a+r+b) ∧
      (∏ i, gammaMass (x i) r a b) < ∏ i, gammaMass (x i) t a t
```

### Shared gamma-ratio theorem

#### `UnivariateML.Base8bGammaRatio.scaled_limit`

A scaled gamma-ratio limit remains valid at zero even if the smaller argument stays bounded. [Lean source](../../lean/UnivariateML/Generic/Base8bGammaRatio.lean#L112)

- `bbinom`, `alpha` · failure
- `bnbinom`, `beta` · failure

```lean
theorem scaled_limit {ι : Type*} {l : Filter ι} {a T : ι → ℝ} {b p : ℝ}
    (hb : 0 < b) (hp : 0 ≤ p) (ha : ∀ᶠ t in l, 0 < a t)
    (hT : Tendsto T l atTop)
    (hr : Tendsto (fun t ↦ a t / T t) l (𝓝 p)) :
    Tendsto (fun t ↦ (Gamma (a t+b)/Gamma (a t)) /
      (Gamma (T t+b)/Gamma (T t))) l (𝓝 (p^b))
```
