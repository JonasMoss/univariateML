# Almost-everywhere uniqueness in Lean for positive-support and threshold families

TASK-445 · 2026-11-27

Lean checks 30 new almost-everywhere finite-winner comparisons, three exact Lomax-crossing transfers, two pointwise unit-exponent results, one exact fatigue singleton, and five GEV reverse-exponential results, including the derivative of the moving-contact chart. Seven shared theorems handle fixed-count likelihoods and analytic score incidence. The table records the outcome of all 44 target rows, including the 11 whose main comparison was already checked in AE2. GEV comparisons across shapes remain open in all three regimes. The all-free generalized and transformed gamma comparisons start at three levels.

## Scope and method

The sample space is `Fin n → ℝ` with Lebesgue measure in the physical levels. Each `w i` is a fixed positive integer cardinality. In the incidence theorems `n` is the number of level coordinates, and the result restricts to each ordered chamber of distinct levels. The pointwise separation applications explicitly require the indicated number of distinct values. In both cases a likelihood winner is a feasible finite point that majorizes every feasible finite point in the chart. Two such winners agree wherever the comparison applies. Excluded limits may share the attained height without adding a finite maximizer.

For the incidence step the analytic identity theorem gives a finite positive order at each zero of a nonidentically zero score difference. On the stratum with orders `k i + 1`, the ith equation is the `k i`th derivative of the score difference at the ith level. Its derivative with respect to that level is nonzero. Derivatives in the other level coordinates vanish, so the sample block is diagonal and invertible. There are countably many order vectors. The shared proof applies `AE1Incidence.ae_no_implicit_incidence` and then the winning-derivative theorem. The fixed-threshold GB2 results, both fixed-endpoint generalized beta results, and the gamma charts use finite-level score separation directly.

The strict-support threshold charts compare the interior finite branch for Feller–Pareto and Pareto IV. Their closed unit-exponent theorems prove uniqueness conditional on attainment with positive count weights. The existing unit-exponent existence results apply to ordinary nonconstant samples. Both Fréchet fits use the dossier restriction to shapes at least one. The auxiliary chart writes the shape as one plus a square, then compares the physical parameters of the winners. Generalized gamma retains `Q = 0` in its likelihood and derivative. The continuous divided difference of the exponential removes the apparent singularity in the score at `Q = 0`. The inverse transformed gamma uses the public scale directly through the signed power `-p`.

## Source discrepancies and limits of the statements

- The Fréchet held-scale summary gives `(0,1,r_S)`. The detailed source specifies three zeros, four copies of `1/20`, and one `r_S ∈ [39,40]`, as does TASK-445. The Lean comparison uses arbitrary positive counts. The exact crossing would need the detailed sample and its interval bounds. [Summary](2026-11-27-uniq-shape-math-report.md#frechet-held-sigma) · [Detailed proof](2026-11-25-frechet-math-report.md#fréchet-single-holds)
- For GEV, applying the open-domain theorem to the original reverse-exponential chart drops its attained closed boundary. The GEV module reparametrizes contact by the indexed maximum and differentiates the resulting height on that chart. The held-location optimizer can also have slack. A comparison across GEV shapes needs a common winner argument that includes contact and then a separate incidence check. [GEV comparison](2026-11-25-gev-math-report.md#global-ties-with-public-scale-or-location-held)
- The fixed-endpoint generalized beta score gives an equivalence class at upper shape one. To obtain parameter uniqueness when the upper shape is free, the Lean proof replaces a unit-shape candidate by upper shape two and increases the power at fixed lower-shape–power product. The log likelihood strictly improves at every observed level for sufficiently large power. This supplies a comparison within the finite model.
- The three-level gamma separation results do not establish small-stratum nonattainment. The existing weighted two-level result bounds an already profiled signed height. Using it for arbitrary positive count weights still requires the profile-to-full-likelihood comparison and a feasible power-face maximizing sequence. The Prentice normal seam must also be compared in the generalized gamma case. [Weighted profile](../../lean/UnivariateML/Family/Shape3GenGamma.lean#L1439)

## Verification

The final workstation run used `/tmp/umvl-cpu/run4 env LEAN_NUM_THREADS=4 nice -n 10 /home/jonas/.elan/bin/lake build` from `dev/lean`, with the thirteen AE3 modules temporarily imported in the root module. Lake reported **Build completed successfully (8382 jobs).** The temporary imports were removed and `git diff --exit-code -- UnivariateML.lean` passed. The build emitted linter warnings, including flexible-simplification warnings in the fatigue score proof. The atlas fragment records checked statements and their coverage. Shared declarations use the schema's reserved `generic` row. [Atlas fragment](../existence-atlas/lean-map.d/lean-ae3.csv)

## Effort and open obligations by target

“Checked” refers to Lean. Open entries identify the step left over by the attempted comparison or transfer. Some source classifications would require a separate formalization of the stationary lists or interval certificates.

| Family | Held | Result and effort |
| --- | --- | --- |
| fatigue | alpha | Checked at five levels. Clearing the rational score denominators identifies the rightmost double pole, then the scale pole and shape coefficient. Open: semialgebraic connected components of the full stationary set and a path argument for constant height on each. |
| fatigue | alpha+beta | The three-level comparison is already `AE2Fatigue.held_alpha_beta_ae_unique`. The small-shape and constant cases are in `Shape3Fatigue.held_shape_scale_threshold_unique`. Open: enumerate positive rational threshold roots and implement the exact algebraic-input height comparison for the remaining branch. |
| fatigue | alpha+mu | The three-level result and reciprocal two-level example are already in AE2. The constant classification is in `Shape3Fatigue.held_shape_threshold_constant_scales`. Open: the algebraic-input height comparison of the outer roots. |
| fatigue | beta | Checked at five levels with the rational score incidence. Checked the exact sample `(0,1,1)` at scale one by profiling shape and showing that the cleared quartic stationary numerator divided by the threshold distance is strictly increasing with one positive zero. Open: the general finite stationary list and comparisons with the critical face. |
| fpareto | min | Checked at four levels by GB2 score separation on the shifted support. Open: conditional power/scale uniqueness and the connected stationary classification. |
| fpareto | min+shape1 | Checked the held-upper-shape restriction at four levels. Open: connected stationary classification and the reciprocal replicated Burr crossing. The available Lomax crossing holds the power fixed and does not certify a free-power winner. |
| fpareto | min+shape3 | Checked the held-lower-shape restriction at four levels. Open: connected stationary classification and the free-power Burr crossing, including the uniform upper bounds off the two candidate bands. |
| fpareto | scale+shape2+shape3 | Checked at five levels above the unit lower-end exponent. The pole identifies threshold and the remaining score coefficient identifies upper shape. Checked pointwise uniqueness at unit exponent with closed contact and beta strict convexity. |
| gengamma | all free | Checked signed/signed, signed/normal and normal/normal separation in one continuous chart at three levels. Open: arbitrary-count one- and two-level nonattainment, the reflected global pair, and the finite signed list. The strict weighted bound already applies to the signed profile, while the proof also needs its normalization/height bridge and the normal comparison. |
| gengamma | mu | Checked the restricted chart at three levels. The ordinary sign-symmetric normal result is `Shape3GenGamma.held_location_symmetric_normal`. Open: exhaustive weighted signed lists and comparison to the normal seam and faces. |
| gengamma | mu+sigma | Checked the one-coordinate chart at three levels. The existing `Shape3GenGamma.all_contact_held_location_scale_normal` covers the all-contact singleton. |
| gengamma | sigma | Checked the restricted chart at three levels. Open: formalize the strict positive quadratic coefficient `(d⁴ - 1)/12` in the optimized height expansion and use it to exclude the normal branch on the open two-level region, together with attainment and reflection. |
| betapr | all free | The three-level comparison is already in AE2. Open: the exact log-symmetric four-way comparison, specifically the uniform comparison of the two finite heights with the optimized gamma faces along the crossing path. |
| betapr | shape1 | The three-level result is already in AE2. Checked the exact Lomax transfer using the beta recurrence and reciprocal scale. |
| betapr | shape2 | The three-level result is already in AE2. Checked the exact reciprocal sample and the data-only log-Jacobian transfer. |
| gb2 | all free | Checked the four-level comparison directly using the supplied separation theorem and the full beta normalizer. |
| gb2 | shape1 | Checked the four-level comparison by restriction. Open: the reciprocal replicated free-power Burr crossing at unit upper shape. |
| gb2 | shape3 | Checked the four-level comparison by restriction. Open: global maximization in two separated free-power Burr bands and the strict off-band and face bounds. |
| genbeta | scale | Checked at four levels. The proof excludes the unit-shape class by a feasible strict improvement. Open: conditional beta-shape fit existence and the finite-power list above both faces. |
| genbeta | scale+shape2 | Checked the nonunit result at four levels. AE2 already proves the fixed-endpoint unit ridge and exact rate. |
| genbeta | shape2 | Checked above one at seven levels. The rescaled endpoint derivative tends to `-(u - 1)`, identifying the endpoint. Four interior test points then identify the lower shape and power. The closed unit ridge and exact example rate are already in AE2. |
| gev | all free | Checked the reverse-exponential branch and its moving-contact derivative. Open: a common winning-derivative statement for the strict-support signed charts, the Gumbel seam, and the sample-indexed contact chart. At noncontact levels the contact score is the constant reciprocal scale. The maximum level has an extra derivative term from the moving boundary. An attempted application with the same score equation at every level therefore fails at that coordinate. The reduced system must omit that equation and account for the indexed maximum in its projection. The exact finite/contact crossing requires the compact interval height bounds. |
| gev | mu | Checked the reverse-exponential scale optimizer with both slack and contact. Open: glue the moving scale (x_{max}-mu) contact chart to the slack and other-shape charts and prove null comparison at five levels. The held-location transition requires the outside height bounds at `t_L`. |
| gev | sigma | Checked the reverse-exponential optimizer at (x_{max}-sigma) and its contact derivative. Open: the noncontact score equations must exclude ties with the other branches after the common winner step. The exact two-level transition requires the competing branch comparison at `r_S`. |
| pareto4 | min | Checked the four-level Burr chart at the held threshold. Open: the replicated free-power Burr crossing. |
| pareto4 | min+shape2 | The almost-everywhere and pointwise two-level results are already checked. Checked the exact unit-power crossing transfer in shape/rate coordinates, including translated thresholds. |
| pareto4 | scale+shape2 | Checked above unit power at five levels by restricting the movable-endpoint Feller–Pareto result. Checked pointwise uniqueness with closed contact at unit power. |
| burr | all free | Checked the four-level comparison using the lower-shape-one GB2 chart. Open: the exact replicated-log example needs the strict compact two-band comparison and the uniform bounds against both faces. |
| burr | shape2 | The AE2 comparison and `Shape3Burr.held_power_two_level_unique` already check. `Shape2Burr.held_power_exact_tie` gives an exact powered Lomax transfer with 21 observations. Open: the distinct powered four-level crossing from the target. |
| expweibull | all free | Checked at seven levels by dividing the log-axis score by its inner power, which gives a finite nonzero right limit. Ratios identify shape and scale before a finite evaluation identifies the exponent. Open: the first profile crossing and the interval comparison excluding the separator and all exterior powers. |
| expweibull | scale | Checked at five levels by restricting the same score argument. Open: compactly confined finite shape-root list with the profiled exponent on the attained branch. |
| frechet | all free | Checked at seven levels. The rescaled physical score diverges at the larger threshold, and a finite test point and tail limit identify scale and shape. Open: pointwise two-level shape-one uniqueness at minimum minority and the exact `r_F` crossing, including the uniform split at `13/20`. |
| frechet | sigma | Checked at five levels with shape one included. Open: the uniform interval maximum comparison over `[39,40]`, the separator at `6/5`, and the large-shape tail bound for the exact crossing. |
| invburr | all free | Checked at four levels in the positive-power GB2 chart with upper shape one. Open: the exact reciprocal replicated free-power tie. |
| invburr | shape2 | The AE2 comparison and pointwise two-level result already check. `Shape2InvBurr.held_power_exact_tie` checks the specified three-level reciprocal Lomax transfer. |
| invtrgamma | all free | Checked at three levels in the inverse public-scale chart. Open: one- and two-level nonattainment to reach every sample size, the exact replicated-outlier crossing, and finite signed height comparisons. |
| invtrgamma | scale | Checked at three levels. Open: derive the strict profile decrease at zero oriented mean and nonzero variance and use it for the pointwise singleton. The remaining finite stationary list is also open. |
| pgw | scale | Checked at five levels. The left score limit identifies shape, the first derivative at zero identifies scale, and strict monotonicity at powered excess one identifies the exponent. Open: compact confinement and finiteness of the stationary roots. |
| pgw | shape | Checked at five levels by restriction. Open: finite stationary list and exact comparison to the optimized transformed Gompertz height, retaining finite equality. |
| trgamma | all free | Checked at three levels. Open: one- and two-level nonattainment, the reciprocal finite/finite tie, and the exact twelve-level finite/power equality. The latter needs the source's uniform small- and large-power bounds along the path. |
| trgamma | scale | Checked at three levels. Open: the zero-oriented-mean score inequality and finite stationary comparison, as for the inverse orientation. |
| invpareto | all free | Already checked by AE2, `Endpoint3LomaxTransfer.inverse_pareto_two_level_unique`, and `TiesLomax.inverse_pareto_global_tie`. |
| lomax | all free | Already checked by AE2, `LomaxTwoPoint.joint_unique`, and `TiesLomax.three_level_global_tie`. |
| pareto2 | min | Already checked by AE2, `Endpoint3LomaxTransfer.paretoII_two_level_unique`, and `TiesLomax.pareto_ii_global_tie`. |

## Checked family statements

### `UnivariateML.AE3ExpWeibull.all_free_ae_unique`

With shape, scale and exponent free, at most one finite exponentiated Weibull winner exists for almost every fixed-count stratum with at least seven levels. Positive observations and parameters. Finite winner comparison. [AE3ExpWeibull.lean](../../lean/UnivariateML/Family/AE3ExpWeibull.lean#L236)

```lean
theorem all_free_ae_unique (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 3 → ℝ) → Parameters := fun p ↦ (p 0, p 1, p 2)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3ExpWeibull.held_scale_ae_unique`

With scale held, at most one finite exponentiated Weibull winner exists for almost every fixed-count stratum with at least five levels. Positive observations and parameters. Finite winner comparison. [AE3ExpWeibull.lean](../../lean/UnivariateML/Family/AE3ExpWeibull.lean#L256)

```lean
theorem held_scale_ae_unique (s : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (p 0, s, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3Fatigue.held_alpha_ae_unique`

With shape held, finite winners agree for almost every fixed-count five-level or larger stratum. Positive shape and scale. Fitted threshold strictly below all levels. [AE3Fatigue.lean](../../lean/UnivariateML/Family/AE3Fatigue.lean#L280)

```lean
theorem held_alpha_ae_unique (a : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (AE2Fatigue.domain (heldAlpha a))
        (AE2Fatigue.likelihood (heldAlpha a) w) x p →
      UniqueAE.Winner (AE2Fatigue.domain (heldAlpha a))
        (AE2Fatigue.likelihood (heldAlpha a) w) x q →
      p = q
```

### `UnivariateML.AE3Fatigue.held_beta_ae_unique`

With scale held, finite winners agree for almost every fixed-count five-level or larger stratum. Positive shape and scale. Fitted threshold strictly below all levels. [AE3Fatigue.lean](../../lean/UnivariateML/Family/AE3Fatigue.lean#L298)

```lean
theorem held_beta_ae_unique (b : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (AE2Fatigue.domain (heldBeta b)) (AE2Fatigue.likelihood (heldBeta b) w) x p →
      UniqueAE.Winner (AE2Fatigue.domain (heldBeta b)) (AE2Fatigue.likelihood (heldBeta b) w) x q →
      p = q
```

### `UnivariateML.AE3FatigueCritical.held_beta_critical_sample_unique`

With scale one, the sample consisting of zero once and one twice has exactly one finite maximizing shape and threshold below zero. The second coordinate is the positive distance from the threshold to zero. Scale one, with counts one and two at levels zero and one. [AE3FatigueCritical.lean](../../lean/UnivariateML/Family/AE3FatigueCritical.lean#L232)

```lean
theorem held_beta_critical_sample_unique :
    let S : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun z : ℝ × ℝ ↦ AE2Fatigue.likelihood (fun p : ℝ × ℝ ↦ ⟨p.1, 1, -p.2⟩)
      (fun i : Fin 2 ↦ if i = 0 then 1 else 2) z (fun i ↦ if i = 0 then 0 else 1)
    ∃! z, z ∈ S ∧ IsMaxOn L S z
```

### `UnivariateML.AE3Frechet.all_free_ae_unique`

With shape at least one and all coefficients free, finite Fréchet winners agree almost everywhere on each fixed-count stratum with at least seven levels. Shape at least one, positive scale, and fitted threshold strictly below all levels. [AE3Frechet.lean](../../lean/UnivariateML/Family/AE3Frechet.lean#L251)

```lean
theorem all_free_ae_unique (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (fittedDomain id) (likelihood (density id) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (fittedDomain id) (likelihood (density id) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3Frechet.held_sigma_ae_unique`

With scale held and shape at least one, finite Fréchet winners agree almost everywhere on each fixed-count stratum with at least five levels. Shape at least one, positive scale, and fitted threshold strictly below all levels. [AE3Frechet.lean](../../lean/UnivariateML/Family/AE3Frechet.lean#L275)

```lean
theorem held_sigma_ae_unique (s : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun p ↦ (p.1, p.2, s)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (fittedDomain param) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3GB2.gb2_all_free_ae_unique`

All-free GB2 winners agree almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L181)

```lean
theorem gb2_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 id) (likelihood 0 id w) x p →
        UniqueAE.Winner (domain 0 id) (likelihood 0 id w) x q → p = q
```

### `UnivariateML.AE3GB2.gb2_held_shape1_ae_unique`

GB2 at held upper shape has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L189)

```lean
theorem gb2_held_shape1_ae_unique (u : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.gb2_held_shape3_ae_unique`

GB2 at held lower shape has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L201)

```lean
theorem gb2_held_shape3_ae_unique (l : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.fpareto_held_min_ae_unique`

Feller–Pareto at held minimum has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L213)

```lean
theorem fpareto_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m id)
        (likelihood m id w) x p →
        UniqueAE.Winner (domain m id)
          (likelihood m id w) x q → p = q
```

### `UnivariateML.AE3GB2.fpareto_held_min_shape1_ae_unique`

Feller–Pareto at held minimum and upper shape has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L223)

```lean
theorem fpareto_held_min_shape1_ae_unique (m u : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.fpareto_held_min_shape3_ae_unique`

Feller–Pareto at held minimum and lower shape has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L235)

```lean
theorem fpareto_held_min_shape3_ae_unique (m l : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.burr_all_free_ae_unique`

Burr XII has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L246)

```lean
theorem burr_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.pareto4_held_min_ae_unique`

Pareto IV at held minimum has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L258)

```lean
theorem pareto4_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.invburr_all_free_ae_unique`

Inverse Burr in its positive-power GB2 chart has at most one attained winner almost everywhere on four or more levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L270)

```lean
theorem invburr_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)) w) x q → p = q
```

### `UnivariateML.AE3GB2.fpareto_held_scale_shape2_shape3_ae_unique`

With power, lower shape and scale held and lower-end exponent above one, finite Feller–Pareto winners agree almost everywhere on at least five levels. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L353)

```lean
theorem fpareto_held_scale_shape2_shape3_ae_unique (p l s : ℝ)
    (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p l s))
        (AE3WeightedChart.likelihood (movingKernel p l s) (movingNormalizer p l s) w) x a →
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p l s))
        (AE3WeightedChart.likelihood (movingKernel p l s) (movingNormalizer p l s) w) x b →
      a = b
```

### `UnivariateML.AE3GB2.pareto4_held_scale_shape2_ae_unique`

Pareto IV with power and scale held is the lower-shape-one restriction of the moving-minimum Feller–Pareto chart. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L444)

```lean
theorem pareto4_held_scale_shape2_ae_unique (p s : ℝ) (hn : 5 ≤ n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p 1 s))
        (AE3WeightedChart.likelihood (movingKernel p 1 s) (movingNormalizer p 1 s) w) x a →
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p 1 s))
        (AE3WeightedChart.likelihood (movingKernel p 1 s) (movingNormalizer p 1 s) w) x b →
      a = b
```

### `UnivariateML.AE3GB2.fpareto_unit_lower_end_unique`

With the lower-end exponent fixed at one, two attained Feller–Pareto winners agree pointwise and both have threshold equal to the sample minimum. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L468)

```lean
theorem fpareto_unit_lower_end_unique {p l s : ℝ} (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmin : ∀ j, x i ≤ x j)
    {a b : ℝ × ℝ} (ha : a ∈ unitSupport x) (hb : b ∈ unitSupport x)
    (hA : IsMaxOn (unitLikelihood p l s w x) (unitSupport x) a)
    (hB : IsMaxOn (unitLikelihood p l s w x) (unitSupport x) b) :
    a = b ∧ a.2 = x i
```

### `UnivariateML.AE3GB2.pareto4_unit_power_unique`

At unit held power the Pareto IV threshold is closed, and its attained winner is pointwise unique. Strict support except in the explicit closed unit-exponent theorems. [AE3GB2.lean](../../lean/UnivariateML/Family/AE3GB2.lean#L546)

```lean
theorem pareto4_unit_power_unique {s : ℝ} (hs : 0 < s)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmin : ∀ j, x i ≤ x j)
    {a b : ℝ × ℝ} (ha : a ∈ unitSupport x) (hb : b ∈ unitSupport x)
    (hA : IsMaxOn (unitLikelihood 1 1 s w x) (unitSupport x) a)
    (hB : IsMaxOn (unitLikelihood 1 1 s w x) (unitSupport x) b) :
    a = b ∧ a.2 = x i
```

### `UnivariateML.AE3GEV.reverseLikelihood_eq_gev`

The unweighted branch agrees with the existing GEV likelihood even when the maximum contacts its closed right endpoint. Reverse-exponential branch alone, including support contact. [AE3GEV.lean](../../lean/UnivariateML/Family/AE3GEV.lean#L39)

```lean
theorem reverseLikelihood_eq_gev (x : Fin n → ℝ) (p : ℝ × ℝ) :
    reverseLikelihood (fun _ ↦ 1) x p = GEV.logLik x (-1) p
```

### `UnivariateML.AE3GEV.reverse_contact_hasDeriv`

On the open set where the indexed maximum stays unique, the contact branch can be expressed with the scale as its free coordinate. This expression differentiates even though the original domain has a closed boundary. Reverse-exponential branch alone, including support contact. [AE3GEV.lean](../../lean/UnivariateML/Family/AE3GEV.lean#L57)

```lean
theorem reverse_contact_hasDeriv (w : Fin n → ℕ) (i : Fin n) (s : ℝ) (x : Fin n → ℝ) :
    HasFDerivAt (fun y ↦ reverseLikelihood w y (y i - s, s)) (contactDerivative w i s) x
```

### `UnivariateML.AE3GEV.reverse_held_sigma_unique`

At held positive scale, the reverse-exponential branch has a unique winner, whose right endpoint is the sample maximum. Reverse-exponential branch alone, including support contact. [AE3GEV.lean](../../lean/UnivariateML/Family/AE3GEV.lean#L110)

```lean
theorem reverse_held_sigma_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmax : ∀ j, x j ≤ x i) {s : ℝ} (hs : 0 < s) :
    ∃! μ, (μ, s) ∈ reverseDomain x ∧
      IsMaxOn (fun m ↦ reverseLikelihood w x (m, s)) {m | (m, s) ∈ reverseDomain x} μ
```

### `UnivariateML.AE3GEV.reverse_all_free_unique`

On nonconstant samples, the all-free reverse-exponential submodel has a unique winner at contact, with scale equal to the weighted mean gap. Reverse-exponential branch alone, including support contact. [AE3GEV.lean](../../lean/UnivariateML/Family/AE3GEV.lean#L132)

```lean
theorem reverse_all_free_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i j : Fin n) (hmax : ∀ k, x k ≤ x i) (hj : x j < x i) :
    ∃! p, p ∈ reverseDomain x ∧ IsMaxOn (reverseLikelihood w x) (reverseDomain x) p
```

### `UnivariateML.AE3GEV.reverse_held_mu_unique`

At held location, the reverse-exponential fit may be at contact or have slack. On nonconstant data the weighted branch has exactly one winner. Reverse-exponential branch alone, including support contact. [AE3GEV.lean](../../lean/UnivariateML/Family/AE3GEV.lean#L192)

```lean
theorem reverse_held_mu_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i j : Fin n) (hmax : ∀ k, x k ≤ x i) (hj : x j < x i) (μ : ℝ) :
    ∃! s, (μ, s) ∈ reverseDomain x ∧
      IsMaxOn (fun s ↦ reverseLikelihood w x (μ, s)) {s | (μ, s) ∈ reverseDomain x} s
```

### `UnivariateML.AE3GenBeta.held_scale_shape2_ae_unique`

Nonunit fixed upper shape and endpoint give at most one finite winner almost everywhere on four or more levels. Strictly interior observations. The moving-endpoint theorem requires upper shape above one. [AE3GenBeta.lean](../../lean/UnivariateML/Family/AE3GenBeta.lean#L106)

```lean
theorem held_scale_shape2_ae_unique (s u : ℝ) (hu : u ≠ 1) (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, u, z.2)
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain (support s param))
          (likelihood (kernel s param) (normalizer s param) w) x p →
        UniqueAE.Winner (domain (support s param))
          (likelihood (kernel s param) (normalizer s param) w) x q → p = q
```

### `UnivariateML.AE3GenBeta.held_scale_ae_unique`

At fixed endpoint, an attained finite fit is almost everywhere unique on four or more levels, including when the upper shape is free. Strictly interior observations. The moving-endpoint theorem requires upper shape above one. [AE3GenBeta.lean](../../lean/UnivariateML/Family/AE3GenBeta.lean#L233)

```lean
theorem held_scale_ae_unique (s : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain (support s id))
          (likelihood (kernel s id) (normalizer s id) w) x p →
        UniqueAE.Winner (domain (support s id))
          (likelihood (kernel s id) (normalizer s id) w) x q → p = q
```

### `UnivariateML.AE3GenBeta.held_shape2_ae_unique`

At held upper shape above one, finite generalized beta winners with a free endpoint agree almost everywhere on strata with at least seven levels. Strictly interior observations. The moving-endpoint theorem requires upper shape above one. [AE3GenBeta.lean](../../lean/UnivariateML/Family/AE3GenBeta.lean#L402)

```lean
theorem held_shape2_ae_unique (u : ℝ) (hn : 7 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (movingSupport u))
        (likelihood (movingDensity u) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (movingSupport u))
        (likelihood (movingDensity u) (fun _ ↦ 0) w) x q → p = q
```

### `UnivariateML.AE3GenGamma.all_free_ae_unique`

Finite Prentice winners agree almost everywhere on three or more levels. Positive observations and Prentice scale. Both signed branches and the normal seam. [AE3GenGamma.lean](../../lean/UnivariateML/Family/AE3GenGamma.lean#L228)

```lean
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := id
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q
```

### `UnivariateML.AE3GenGamma.held_mu_ae_unique`

Finite Prentice winners agree almost everywhere on three or more levels. Positive observations and Prentice scale. Both signed branches and the normal seam. [AE3GenGamma.lean](../../lean/UnivariateML/Family/AE3GenGamma.lean#L238)

```lean
theorem held_mu_ae_unique (μ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun z : ℝ × ℝ ↦ (μ, z.1, z.2))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q
```

### `UnivariateML.AE3GenGamma.held_mu_sigma_ae_unique`

Finite Prentice winners agree almost everywhere on three or more levels. Positive observations and Prentice scale. Both signed branches and the normal seam. [AE3GenGamma.lean](../../lean/UnivariateML/Family/AE3GenGamma.lean#L249)

```lean
theorem held_mu_sigma_ae_unique (μ σ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun Q : ℝ ↦ (μ, σ, Q))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q
```

### `UnivariateML.AE3GenGamma.held_sigma_ae_unique`

Finite Prentice winners agree almost everywhere on three or more levels. Positive observations and Prentice scale. Both signed branches and the normal seam. [AE3GenGamma.lean](../../lean/UnivariateML/Family/AE3GenGamma.lean#L259)

```lean
theorem held_sigma_ae_unique (σ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun z : ℝ × ℝ ↦ (z.1, σ, z.2))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q
```

### `UnivariateML.AE3LomaxTies.betapr_held_shape1_unit_tie`

At beta-prime lower shape one, the exact crossing has two winners. The chart uses upper shape and reciprocal scale. The exact three-level sample with multiplicities 1, 10, 10. [AE3LomaxTies.lean](../../lean/UnivariateML/Family/AE3LomaxTies.lean#L73)

```lean
theorem betapr_held_shape1_unit_tie :
    let param : ℝ × ℝ → AE2BetaPrime.Parameters := fun v ↦ (1, v.1, v.2⁻¹)
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2BetaPrime.likelihood param (fun _ ↦ 1) v (crossingSample a))
          Lomax.domain v} = {p, q}
```

### `UnivariateML.AE3LomaxTies.betapr_held_shape2_unit_tie`

At beta-prime upper shape one, the reciprocal exact crossing has two winners in lower shape and scale coordinates. The exact three-level sample with multiplicities 1, 10, 10. [AE3LomaxTies.lean](../../lean/UnivariateML/Family/AE3LomaxTies.lean#L100)

```lean
theorem betapr_held_shape2_unit_tie :
    let param : ℝ × ℝ → AE2BetaPrime.Parameters := fun v ↦ (v.1, 1, v.2)
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2BetaPrime.likelihood param (fun _ ↦ 1) v (fun i ↦ (crossingSample a i)⁻¹))
        Lomax.domain v} = {p, q}
```

### `UnivariateML.AE3LomaxTies.pareto4_held_min_unit_power_tie`

At Pareto IV power one and held minimum, translating the exact Lomax crossing preserves the two-winner set in shape and rate coordinates. The exact three-level sample with multiplicities 1, 10, 10. [AE3LomaxTies.lean](../../lean/UnivariateML/Family/AE3LomaxTies.lean#L126)

```lean
theorem pareto4_held_min_unit_power_tie (m : ℝ) :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2Lomax.likelihood 1 m (fun _ ↦ 1) v (fun i ↦ crossingSample a i + m))
        Lomax.domain v} = {p, q}
```

### `UnivariateML.AE3PGW.held_scale_ae_unique`

With scale held, at most one finite PGW winner exists on almost every fixed-count stratum with five or more levels. Positive observations and parameters. Finite winner comparison. [AE3PGW.lean](../../lean/UnivariateML/Family/AE3PGW.lean#L243)

```lean
theorem held_scale_ae_unique (a : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (p 0, a, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3PGW.held_shape_ae_unique`

With shape held, at most one finite PGW winner exists on almost every fixed-count stratum with five or more levels. Positive observations and parameters. Finite winner comparison. [AE3PGW.lean](../../lean/UnivariateML/Family/AE3PGW.lean#L262)

```lean
theorem held_shape_ae_unique (b : ℝ) (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : (Fin 2 → ℝ) → Parameters := fun p ↦ (b, p 0, p 1)
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x p →
      UniqueAE.Winner (domain (support param)) (likelihood (density param) (fun _ ↦ 0) w) x q →
      p = q
```

### `UnivariateML.AE3TrGamma.trgamma_all_free_ae_unique`

Finite trgamma winners agree almost everywhere on strata with three or more levels. Positive observations, positive shapes, and public scale in the forward and inverse models. [AE3TrGamma.lean](../../lean/UnivariateML/Family/AE3TrGamma.lean#L145)

```lean
theorem trgamma_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support id))
          (likelihood (kernel 1 id) (normalizer id) w) x p →
        UniqueAE.Winner (domain (support id))
          (likelihood (kernel 1 id) (normalizer id) w) x q → p = q
```

### `UnivariateML.AE3TrGamma.trgamma_held_scale_ae_unique`

Finite trgamma winners agree almost everywhere on strata with three or more levels. Positive observations, positive shapes, and public scale in the forward and inverse models. [AE3TrGamma.lean](../../lean/UnivariateML/Family/AE3TrGamma.lean#L155)

```lean
theorem trgamma_held_scale_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, z.2, a)
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (kernel 1 param) (normalizer param) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (kernel 1 param) (normalizer param) w) x q → p = q
```

### `UnivariateML.AE3TrGamma.invtrgamma_all_free_ae_unique`

Finite invtrgamma winners agree almost everywhere on strata with three or more levels. Positive observations, positive shapes, and public scale in the forward and inverse models. [AE3TrGamma.lean](../../lean/UnivariateML/Family/AE3TrGamma.lean#L168)

```lean
theorem invtrgamma_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support id))
          (likelihood (kernel (-1) id) (normalizer id) w) x p →
        UniqueAE.Winner (domain (support id))
          (likelihood (kernel (-1) id) (normalizer id) w) x q → p = q
```

### `UnivariateML.AE3TrGamma.invtrgamma_held_scale_ae_unique`

Finite invtrgamma winners agree almost everywhere on strata with three or more levels. Positive observations, positive shapes, and public scale in the forward and inverse models. [AE3TrGamma.lean](../../lean/UnivariateML/Family/AE3TrGamma.lean#L178)

```lean
theorem invtrgamma_held_scale_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, z.2, a)
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (kernel (-1) param) (normalizer param) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (kernel (-1) param) (normalizer param) w) x q → p = q
```

## Shared results

### `UnivariateML.AE3AnalyticIncidence.ae_no_analytic_incidence`

If every coordinate is a zero of finite positive order of a jointly analytic score difference, the sample projection is null when the pair chart has lower dimension. Reusable fixed-count chart or incidence lemma. [AE3AnalyticIncidence.lean](../../lean/UnivariateML/Generic/AE3AnalyticIncidence.lean#L43)

```lean
theorem ae_no_analytic_incidence {d n : ℕ} (hdn : d < n)
    (f : (Fin d → ℝ) × ℝ → ℝ) (S : Set (((Fin d → ℝ) × (Fin n → ℝ))))
    (ha : ∀ z ∈ S, ∀ i, AnalyticAt ℝ f (z.1, z.2 i))
    (ho : ∀ z ∈ S, ∀ i, ∃ k : ℕ,
      iteratedDeriv k (fun t ↦ f (z.1, t)) (z.2 i) = 0 ∧
      iteratedDeriv (k + 1) (fun t ↦ f (z.1, t)) (z.2 i) ≠ 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p, (p, x) ∉ S
```

### `UnivariateML.AE3AnalyticIncidence.exists_derivative_before_nonzero`

A zero of an analytic function that is nonzero somewhere on its connected domain has a vanishing derivative immediately followed by a nonvanishing one. Reusable fixed-count chart or incidence lemma. [AE3AnalyticIncidence.lean](../../lean/UnivariateML/Generic/AE3AnalyticIncidence.lean#L101)

```lean
theorem exists_derivative_before_nonzero {f : ℝ → ℝ} {U : Set ℝ} {x : ℝ}
    (ha : AnalyticOnNhd ℝ f U) (hU : IsPreconnected U) (hx : x ∈ U)
    (hz : f x = 0) (hne : ∃ y ∈ U, f y ≠ 0) :
    ∃ k : ℕ, iteratedDeriv k f x = 0 ∧ iteratedDeriv (k + 1) f x ≠ 0
```

### `UnivariateML.AE3AnalyticIncidence.ae_no_identified_score_ties`

Analytic score differences that are nonidentically zero on each competing pair's common interval cannot vanish simultaneously at more levels than the number of pair coordinates, except on a null set. Reusable fixed-count chart or incidence lemma. [AE3AnalyticIncidence.lean](../../lean/UnivariateML/Generic/AE3AnalyticIncidence.lean#L119)

```lean
theorem ae_no_identified_score_ties {d n : ℕ} (hdn : d < n)
    (f : (Fin d → ℝ) × ℝ → ℝ) (S : Set (Fin d → ℝ))
    (U : (Fin d → ℝ) → Set ℝ) (hU : ∀ p ∈ S, IsPreconnected (U p))
    (ha : ∀ p ∈ S, ∀ t ∈ U p, AnalyticAt ℝ f (p, t))
    (hne : ∀ p ∈ S, ∃ t ∈ U p, f (p, t) ≠ 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ S, (∀ i, x i ∈ U p) →
      ¬ ∀ i, f (p, x i) = 0
```

### `UnivariateML.AE3WeightedChart.ae_winning_class`

On a fixed-count chart, separation of the scalar scores of winners almost everywhere implies uniqueness of the winning class almost everywhere. Reusable fixed-count chart or incidence lemma. [AE3WeightedChart.lean](../../lean/UnivariateML/Generic/AE3WeightedChart.lean#L48)

```lean
theorem ae_winning_class [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] {A : Type*} {U : Set (P × ℝ)} (hU : IsOpen U)
    {f d : P × ℝ → ℝ} {c : P → ℝ} (hf : ContinuousOn f U) (hd : ContinuousOn d U)
    (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (cls : P → A) (good : (Fin n → ℝ) → Prop)
    (hsep : ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q →
      (∀ i, d (p, x i) = d (q, x i)) → cls p = cls q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q
```

### `UnivariateML.AE3WeightedChart.ae_winning_class_analytic`

Joint analyticity provides continuity of both the scalar kernel and its sample score. Reusable fixed-count chart or incidence lemma. [AE3WeightedChart.lean](../../lean/UnivariateML/Generic/AE3WeightedChart.lean#L101)

```lean
theorem ae_winning_class_analytic [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P] {A : Type*}
    {U : Set (P × ℝ)} (hU : IsOpen U) {f : P × ℝ → ℝ} {c : P → ℝ}
    (hf : AnalyticOnNhd ℝ f U) (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (cls : P → A) (good : (Fin n → ℝ) → Prop)
    (hsep : ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q →
      (∀ i, deriv (fun t ↦ f (p, t)) (x i) = deriv (fun t ↦ f (q, t)) (x i)) →
      cls p = cls q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q
```

### `UnivariateML.AE3WeightedChart.ae_unique_class_of_analytic_score`

An analytic score identifying winning classes on the common support yields almost-everywhere class uniqueness when the number of levels exceeds twice the free parameter dimension. Reusable fixed-count chart or incidence lemma. [AE3WeightedChart.lean](../../lean/UnivariateML/Generic/AE3WeightedChart.lean#L132)

```lean
theorem ae_unique_class_of_analytic_score {m : ℕ} {A : Type*} (hn : m + m < n)
    {U : Set ((Fin m → ℝ) × ℝ)} (hU : IsOpen U)
    {f d : (Fin m → ℝ) × ℝ → ℝ} {c : (Fin m → ℝ) → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : (Fin m → ℝ) × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (hconn : ∀ p q, IsPreconnected {t | (p, t) ∈ U ∧ (q, t) ∈ U})
    (cls : (Fin m → ℝ) → A)
    (hident : ∀ p q, cls p ≠ cls q → (∃ t, (p, t) ∈ U ∧ (q, t) ∈ U) →
      ∃ t, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) ≠ d (q, t))
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q
```

### `UnivariateML.AE3WeightedChart.ae_unique_of_analytic_score`

An identifiable analytic score on the common support yields almost-everywhere uniqueness when the number of levels exceeds twice the free parameter dimension. Reusable fixed-count chart or incidence lemma. [AE3WeightedChart.lean](../../lean/UnivariateML/Generic/AE3WeightedChart.lean#L186)

```lean
theorem ae_unique_of_analytic_score {m : ℕ} (hn : m + m < n)
    {U : Set ((Fin m → ℝ) × ℝ)} (hU : IsOpen U)
    {f d : (Fin m → ℝ) × ℝ → ℝ} {c : (Fin m → ℝ) → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : (Fin m → ℝ) × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (hconn : ∀ p q, IsPreconnected {t | (p, t) ∈ U ∧ (q, t) ∈ U})
    (hident : ∀ p q, p ≠ q → (∃ t, (p, t) ∈ U ∧ (q, t) ∈ U) →
      ∃ t, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) ≠ d (q, t))
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → p = q
```
