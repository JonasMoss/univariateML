# Almost-everywhere uniqueness in Lean

TASK-438 · 2026-11-27

Lean checks 11 fixed-count almost-everywhere uniqueness results, the exact fatigue two-level tie, and two generalized-beta ridge classifications with the specified rates checked in the examples. The main statements are below. The open rows distinguish work undertaken in Lean from a review of the mathematical proof route. Chart and score arguments from the cited reports were used only at the scope stated in each theorem.

## Discrepancies in the target list and sources

- The Fréchet held-scale summary names the sample `(0,1,r_S)` with `r_S∈[39,40]`. The detailed held-scale proof uses three zeros, four copies of `1/20` and one `r_S`, as specified in the task. The all-free crossing is also on three levels, with a different certificate and interval. [Shape summary](2026-11-27-uniq-shape-math-report.md#frechet-held-sigma) · [Detailed held-scale crossing](2026-11-25-frechet-math-report.md#fréchet-single-holds)
- The written incidence argument for several families bounds the dimension of a projection over *all* competing parameter pairs. A finite zero set for each fixed pair, even a proved null set, does not by itself give this projected measure-zero result. Mathlib has isolated-zero and implicit-function results and tools for null differentiable images. The proof still needs a countable relative cover by implicit graphs stratified by root multiplicity, together with a lower-dimensional image argument. No such generic bridge was added here. [Analytic incidence construction](2026-11-27-uniq-locscale-math-report.md)

## Scope and verification

The sample domain for an almost-everywhere statement is `Fin n → ℝ` with Lebesgue measure. Here `n` counts distinct level coordinates, and `w i` is the integer count of level `i`. The implications apply on each ordered chamber with the stated number of distinct levels. They compare finite winners, meaning precisely feasible parameters whose likelihood bounds all feasible finite parameter values from above. Attainment is represented by the winner hypotheses.

Fatigue uses the rational spatial score on the open strict-support chart. The likelihood omits the constant common to every fit. At fixed shape and threshold the equation for equal scores reduces to a strictly convex function of the excess. At fixed shape and scale it reduces to a ratio strictly increasing in each reciprocal excess. The latter proof needs only two distinct levels and the exported theorem uses the requested three-level scope.

The Lomax family of charts uses the held power of the physical excess, including negative power for inverse models. The scale coordinate is the positive rate in transformed units. The contribution of the change-of-variable Jacobian is common to all fits. The derivative is computed in the original level coordinates. The nonzero factor supplied by the transform cancels, so the argument proves the statement directly under Lebesgue measure in those coordinates. Two distinct transformed levels determine the coefficients of the affine equation obtained by clearing the two score denominators.

The beta-prime chart keeps the beta normalization and works with shape1, shape2 and scale, in that order. Clearing positive denominators gives a quadratic in the original observation. Three distinct roots force the coefficients to vanish. The held-shape conclusions apply the same separation result to the appropriate two-dimensional charts.

The generalized-beta unit-shape result works directly on the closed endpoint domain when scale is free. For a fixed positive rate the height strictly decreases with scale above the sample maximum. The log likelihood as a function of rate attains its unique optimum at the reciprocal weighted mean log distance. Every positive factorization of the rate gives a winner. The almost-everywhere argument is unnecessary on this branch.

The checked proofs are independent of the likelihood comparison with limiting distributions. An excluded face attaining the same limiting height does not add a point to the finite maximizer set. The included unit-shape generalized-beta contact is handled explicitly. No endpoint stratum is inferred from the open-domain theorem.

All four module checks passed on the workstation through `/tmp/umvl-cpu/run nice -n 10 /home/jonas/.elan/bin/lake env lean`. A subsequent `lake build` through the same wrapper, with all four modules temporarily imported in the library root, exited zero and reported `Build completed successfully (8312 jobs).` The temporary imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed. Existing modules emitted linter warnings. The new modules emitted none. No commits were made.

## Checked Lean statements

### `UnivariateML.AE2Fatigue.held_alpha_mu_ae_unique`

With shape and threshold held, two finite winning scales agree almost everywhere on every fixed-count stratum with at least three levels. Positive held shape and strictly positive excesses. Algebraic-input height comparisons are outside the theorem. [AE2Fatigue.lean](../../lean/UnivariateML/Family/AE2Fatigue.lean#L184)

```lean
theorem held_alpha_mu_ae_unique {a c : ℝ} (ha : 0 < a) (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ b d, UniqueAE.Winner (domain fun s ↦ ⟨a, s, c⟩)
        (likelihood (fun s ↦ ⟨a, s, c⟩) w) x b →
        UniqueAE.Winner (domain fun s ↦ ⟨a, s, c⟩)
        (likelihood (fun s ↦ ⟨a, s, c⟩) w) x d → b = d
```

### `UnivariateML.AE2Fatigue.held_alpha_beta_ae_unique`

With shape and scale held, two finite winning thresholds agree almost everywhere on every fixed-count stratum with at least three levels. Positive held shape and scale, with the fitted threshold below every level. Exact stationary-root comparison is outside the theorem. [AE2Fatigue.lean](../../lean/UnivariateML/Family/AE2Fatigue.lean#L277)

```lean
theorem held_alpha_beta_ae_unique {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ c d, UniqueAE.Winner (domain fun s ↦ ⟨a, b, s⟩)
        (likelihood (fun s ↦ ⟨a, b, s⟩) w) x c →
        UniqueAE.Winner (domain fun s ↦ ⟨a, b, s⟩)
        (likelihood (fun s ↦ ⟨a, b, s⟩) w) x d → c = d
```

### `UnivariateML.AE2Fatigue.held_alpha_mu_two_level_tie`

The sample (1/2, 2) at shape three and threshold zero has exactly the two indicated winning scales. Exact global maximizing set for the specified sample and holds. The two roots have product one. [AE2Fatigue.lean](../../lean/UnivariateML/Family/AE2Fatigue.lean#L330)

```lean
theorem held_alpha_mu_two_level_tie :
    {b | UniqueAE.Winner (domain fun s ↦ ⟨3, s, 0⟩)
      (likelihood (fun s ↦ ⟨3, s, 0⟩) fun _ ↦ 1) ![(1 : ℝ) / 2, 2] b} =
      {((47 : ℝ) - Real.sqrt 1809) / 20, ((47 : ℝ) + Real.sqrt 1809) / 20}
```

### `UnivariateML.AE2Lomax.lomax_ae_unique`

Two finite Lomax winners agree almost everywhere on each fixed-count positive stratum with at least two levels. Positive data and positive integer level counts. The pointwise two-level result and exact crossing have existing proofs. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L181)

```lean
theorem lomax_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (1) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (1) 0 w) x q → p = q
```

### `UnivariateML.AE2Lomax.pareto2_held_min_ae_unique`

Two finite Pareto II winners agree almost everywhere on each fixed-count positive-excess stratum with at least two levels. Held threshold and strictly positive excesses. The pointwise two-level result and exact crossing have existing proofs. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L188)

```lean
theorem pareto2_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain m) (likelihood (1) m w) x p →
        UniqueAE.Winner (domain m) (likelihood (1) m w) x q → p = q
```

### `UnivariateML.AE2Lomax.invpareto_ae_unique`

Two finite inverse Pareto winners agree almost everywhere on each fixed-count positive stratum with at least two levels. Reciprocal positive observations in the likelihood and Lebesgue measure in the original coordinates. The pointwise two-level result and exact crossing have existing proofs. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L195)

```lean
theorem invpareto_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (-1) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (-1) 0 w) x q → p = q
```

### `UnivariateML.AE2Lomax.burr_held_shape2_ae_unique`

Two finite Burr XII winners at held power agree almost everywhere on each fixed-count positive stratum with at least two levels. Positive held power and strictly positive observations. The exact powered four-level crossing is outside the theorem. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L202)

```lean
theorem burr_held_shape2_ae_unique {c : ℝ} (hc : 0 < c) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood c 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood c 0 w) x q → p = q
```

### `UnivariateML.AE2Lomax.invburr_held_shape2_ae_unique`

Two finite inverse Burr winners at held power agree almost everywhere on each fixed-count positive stratum with at least two levels. Negative power on the positive observations, with the public held power positive. The pointwise and exact tie transfers are outside the theorem. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L209)

```lean
theorem invburr_held_shape2_ae_unique {c : ℝ} (hc : 0 < c) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (-c) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (-c) 0 w) x q → p = q
```

### `UnivariateML.AE2Lomax.pareto4_held_min_shape2_ae_unique`

Two finite Pareto IV winners at held threshold and power agree almost everywhere on each fixed-count positive-excess stratum with at least two levels. Positive held power and strictly positive excesses. The pointwise and exact tie transfers are outside the theorem. [AE2Lomax.lean](../../lean/UnivariateML/Family/AE2Lomax.lean#L217)

```lean
theorem pareto4_held_min_shape2_ae_unique (m : ℝ) {c : ℝ} (hc : 0 < c)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain m) (likelihood c m w) x p →
        UniqueAE.Winner (domain m) (likelihood c m w) x q → p = q
```

### `UnivariateML.AE2BetaPrime.all_free_ae_unique`

Two finite beta-prime winners agree almost everywhere on every fixed-count positive stratum with at least three levels. Positive shape and scale chart on strictly positive observations. Exact finite/faces four-way crossing is outside the theorem. [AE2BetaPrime.lean](../../lean/UnivariateML/Family/AE2BetaPrime.lean#L204)

```lean
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain id) (likelihood id w) x p →
        UniqueAE.Winner (domain id) (likelihood id w) x q → p = q
```

### `UnivariateML.AE2BetaPrime.held_shape1_ae_unique`

At held shape1, two finite beta-prime winners agree almost everywhere on each fixed-count positive stratum with at least three levels. Positive held lower beta shape and strictly positive observations. Exact unit-shape Lomax tie transfer is outside the theorem. [AE2BetaPrime.lean](../../lean/UnivariateML/Family/AE2BetaPrime.lean#L211)

```lean
theorem held_shape1_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain fun z ↦ (a, z)) (likelihood (fun z ↦ (a, z)) w) x p →
        UniqueAE.Winner (domain fun z ↦ (a, z)) (likelihood (fun z ↦ (a, z)) w) x q → p = q
```

### `UnivariateML.AE2BetaPrime.held_shape2_ae_unique`

At held shape2, two finite beta-prime winners agree almost everywhere on each fixed-count positive stratum with at least three levels. Positive held upper beta shape and strictly positive observations. Exact reciprocal unit-shape tie transfer is outside the theorem. [AE2BetaPrime.lean](../../lean/UnivariateML/Family/AE2BetaPrime.lean#L219)

```lean
theorem held_shape2_ae_unique (b : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain fun z : ℝ × ℝ ↦ (z.1, b, z.2))
        (likelihood (fun z : ℝ × ℝ ↦ (z.1, b, z.2)) w) x p →
        UniqueAE.Winner (domain fun z : ℝ × ℝ ↦ (z.1, b, z.2))
          (likelihood (fun z : ℝ × ℝ ↦ (z.1, b, z.2)) w) x q → p = q
```

### `UnivariateML.AE2GenBetaUnit.held_scale_unit_ridge`

At fixed scale and unit upper beta shape the whole maximizing ridge has rate equal to the reciprocal weighted mean log distance. Nonempty strictly interior sample at held scale and unit upper shape. The nonunit almost-everywhere branch is open in this job. [AE2GenBetaUnit.lean](../../lean/UnivariateML/Family/AE2GenBetaUnit.lean#L93)

```lean
theorem held_scale_unit_ridge (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (s : ℝ) (hx : ∀ i, 0 < x i) (hs : ∀ i, x i < s)
    (hn : 0 < n) :
    {p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) |
      IsMaxOn (likelihood w x s) (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) p} =
    {p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) |
      p.1 * p.2 = (∑ i, (w i : ℝ)) / logDistance w x s}
```

### `UnivariateML.AE2GenBetaUnit.unit_ridge_at_max`

At unit upper beta shape and free scale the whole maximizing ridge uses the sample maximum and the reciprocal weighted mean log distance there. Positive nonconstant sample on the closed unit upper-endpoint branch. The above-unit almost-everywhere branch is open in this job. [AE2GenBetaUnit.lean](../../lean/UnivariateML/Family/AE2GenBetaUnit.lean#L112)

```lean
theorem unit_ridge_at_max (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (hx : ∀ i, 0 < x i) (i j : Fin n) (hmax : ∀ k, x k ≤ x i)
    (hgap : x j < x i) :
    let S : Set (ℝ × ℝ × ℝ) := {z | (∀ k, x k ≤ z.1) ∧
      z.2 ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)}
    {z ∈ S | IsMaxOn (fun z ↦ likelihood w x z.1 z.2) S z} =
      {z ∈ S | z.1 = x i ∧ z.2.1 * z.2.2 = (∑ k, (w k : ℝ)) / logDistance w x (x i)}
```

## Effort and remaining obligations by regime

“Checked” refers to the new theorems above. “Reviewed” means the source argument and available Lean inputs were inspected, with the remaining proof identified. It does not represent a failed Lean proof script or a completed formalization.

| Family | Held | Status and effort |
| --- | --- | --- |
| fatigue | alpha | Open after the general fatigue support, likelihood and sample-derivative chart was checked. Equal-threshold comparisons are covered by the scale crossing argument. Different thresholds need the projected zero-set lemma at pair dimension four. The rightmost double pole can certify nonidentity after clearing denominators, but a fixed-pair degree bound alone does not give the required five-level result. The finitely many semialgebraic stationary components and a piecewise smooth path in each still need formalization. |
| fatigue | alpha+beta | Checked the three-level result by the monotone reciprocal-excess ratio. The pre-existing `Shape3Fatigue.held_shape_scale_threshold_unique` checks the constant and small-shape cases. The stationary-root list and its exact algebraic height comparison remain open here. |
| fatigue | alpha+mu | Checked the three-level result by the strictly convex crossing function. Checked the two-level tie by maximizing `log v - v/(36/5)` for `v = b + 1/b + 5/2` and factoring the equation at its optimum. The pre-existing `Shape3Fatigue.held_shape_threshold_constant_scales` checks the constant case. The general algebraic-input height comparison remains open here. |
| fatigue | beta | Open after checking the general fatigue chart. Different thresholds need the two-free-parameter projected incidence argument. The rational profile comparison at the critical contact and the exact `(0,1,1)` stationary-root count at scale one also remain. The strict finite support chart by itself compares only finite candidates. |
| fpareto | min | Reviewed. After subtracting the held minimum, the missing four-level input is the four-exponential zero count for two GB2 scores. The conditional power and scale fit and stationary-component classification are separate obligations. |
| fpareto | min+shape1 | Reviewed. Needs the same four-rate comparison restricted to fixed upper shape, plus the exact reciprocal Burr crossing and the stationary-component classification. |
| fpareto | min+shape3 | Reviewed. Needs the four-rate comparison at fixed lower shape, the exact Burr crossing transfer and the stationary-component classification. |
| fpareto | scale+shape2+shape3 | Reviewed. Above unit lower-end exponent, a score pole determines the threshold and the tail determines the upper beta shape. The projected incidence bound for that two-parameter chart remains. At the unit exponent the endpoint must be included and the upper-shape normalizer must give strict concavity there. |
| gengamma | all free | Reviewed existing `Shape3GenGamma.signed_score_separation` and `signed_normal_score_separation`. Needs the assembled signed-plus-normal chart with the same likelihood normalization across charts, and the bridge from the two-level profile inequality to nonattainment for all fixed multiplicities. The exact reflected global pair and finite candidate list remain separate. |
| gengamma | mu | Reviewed. Needs the signed-plus-normal chart restricted to fixed log location, with the correct relation between the log-power and exponential coefficient. The sign-symmetry Stirling comparison and finite candidate list remain separate. |
| gengamma | mu+sigma | Reviewed. Needs a fixed-location, fixed-scale signed chart plus the normal member in the same comparison. The all-contact normal singleton already appears as `Shape3GenGamma.all_contact_held_location_scale_normal`. |
| gengamma | sigma | Reviewed. Needs the signed-plus-normal chart restricted to fixed positive scale. The reflected tie needs the strict normal-height exclusion on an open two-level region and a proof of attainment on that region. |
| betapr | all free | Checked the three-level result by clearing denominators and factoring the differences of the quadratic at three observations. The exact four-way crossing with both gamma faces remains open. |
| betapr | shape1 | Checked the three-level result by restriction of the beta-prime chart. The exact unit-lower-shape Lomax crossing needs a transfer to the beta-prime maximizing set. |
| betapr | shape2 | Checked the three-level result by restriction. The inverse unit-shape exact crossing needs the reciprocal transfer of the entire maximizing set. |
| gb2 | all free | Reviewed. The missing four-level input is the Rolle induction for up to four distinct exponential rates, with coincident rates combined before applying it, and parameter identification from the zero coefficients. |
| gb2 | shape1 | Reviewed. Needs the GB2 bound restricted to fixed upper beta shape and a transfer of the replicated Burr crossing at the unit hold. |
| gb2 | shape3 | Reviewed. Needs the GB2 bound restricted to fixed lower beta shape and the replicated Burr crossing at the unit hold. |
| genbeta | scale | Reviewed. Off unit upper shape the fixed-endpoint log-score comparison needs a four-rate Rolle bound. If an upper shape equals one, the finite score is constant within its rate class. The free-shape result must show a ridge member cannot win against the moving cutoff approached by feasible parameters. Conditional beta-shape uniqueness and finiteness above both faces remain. |
| genbeta | scale+shape2 | Checked the unit-shape ridge and the rate `2/(3 log 2)` by the log inequality and exact substitution. For nonunit held shape the fixed-endpoint four-rate bound and chart application remain. |
| genbeta | shape2 | Checked the unit-shape ridge including the support contact and the rate `2/log 2`. Above unit held shape, the endpoint pole and left-tail coefficient must identify the candidate, followed by the six-dimensional projected incidence argument. Contact at that exponent has zero density. |
| gev | all free | Reviewed. Needs the smooth Gumbel, Fréchet and negative-shape charts in one comparison and the moving reverse-exponential endpoint height chart. The seven-level separation needs the projected incidence bound with the contact level determined by the endpoint. The exact finite/contact crossing remains. |
| gev | mu | Reviewed. Needs the five-level projected incidence bound for the restricted moving-contact and support-slack cases at fixed location. The exact held-location transition remains. |
| gev | sigma | Reviewed. Needs the five-level projected incidence bound and an attained reverse-exponential contact chart at fixed scale. The exact two-level transition remains. |
| pareto4 | min | Reviewed. The full Burr transfer requires four-level logistic separation at free power and the exact replicated crossing. |
| pareto4 | min+shape2 | Checked the almost-everywhere result in the powered-excess chart. The pointwise result is already `Endpoint3LomaxTransfer.paretoIV_two_level_unique`. The exact unit-power transfer of the full two-winner set remains to be stated in this chart. |
| pareto4 | scale+shape2 | Reviewed. The above-unit branch needs the projected score incidence via the threshold pole and tail coefficient. The unit-power contact comparison needs to use the closed threshold and the explicit conditional upper-shape maximum. |
| burr | all free | Reviewed. Needs the four-rate Rolle argument for free power and the two compact profile bands that certify a replicated-log crossing above both faces. |
| burr | shape2 | Checked the almost-everywhere result directly under physical level measure. `Shape3Burr.held_power_two_level_unique` covers the pointwise result. The exact three-level unit-power and powered four-level comparisons were not added to this module. |
| expweibull | all free | Reviewed. Need to formalize score identification from right exponential growth and the left limit, then the six-dimensional projected incidence bound. The exact first profile crossing needs its compact interval height bounds. |
| expweibull | scale | Reviewed. At fixed scale the parameter pair dimension is four. The analytic zero-graph projection is open, as is the finite stationary list with the uniquely profiled exponent on the winning branch. |
| frechet | all free | Reviewed. The threshold and two power terms of the physical score give the source identity test. The projected incidence argument at pair dimension six, the pointwise two-level shape-one winner and the exact three-level crossing remain. |
| frechet | sigma | Reviewed. Needs separation on the held-scale chart with shape one retained and pair dimension four. The exact held-scale claim depends on two interval maximum bounds, an excluded separator and a uniform high-shape tail bound. The sample discrepancy is recorded above. |
| invburr | all free | Reviewed. Needs the reflected free-power logistic comparison and the reciprocal transfer of the replicated Burr crossing. |
| invburr | shape2 | Checked the almost-everywhere comparison with negative power of the physical data. `Shape3Burr.inverse_held_power_two_level_unique` covers the pointwise result. The exact reciprocal tie transfer was not added to this module. |
| invtrgamma | all free | Reviewed. The existing `Shape3TrGamma.oriented_score_separation` supplies a three-level input. Needs the signed log-score derivative chart in the original levels and the one- and two-level nonattainment bridge. The replicated-outlier crossing and candidate-height comparison remain. |
| invtrgamma | scale | Reviewed. Needs the negative-orientation chart with the exponential coefficient fixed by the power and the held scale. The zero-mean uniqueness and finite stationary-list proof remain separate. |
| pgw | scale | Reviewed. The source score comparison uses the left constant limit and right exponential growth rate and coefficient. Needs their identity proof in Lean and the projected incidence bound at fixed scale. The compact finite shape list remains separate. |
| pgw | shape | Reviewed. Needs the analogous fixed-shape projection and an exact analytic comparison of finite roots with the optimized transformed Gompertz face. |
| trgamma | all free | Reviewed. Needs the positive-orientation chart using the existing three-level score separation and the two-level nonattainment bridge. The reciprocal finite/finite tie and the twelve-level finite/power equality remain. |
| trgamma | scale | Reviewed. Needs the positive-orientation chart with the exponential coefficient determined by the held scale, plus the pointwise zero-mean result and finite list comparison. |
| invpareto | all free | Checked the two-level-threshold almost-everywhere result in the physical sample. Existing `Endpoint3LomaxTransfer.inverse_pareto_two_level_unique` and `TiesLomax.inverse_pareto_global_tie` cover the pointwise and exact tie parts. |
| lomax | all free | Checked the two-level-threshold almost-everywhere result. Existing `LomaxTwoPoint.joint_unique` and `TiesLomax.three_level_global_tie` cover the pointwise and exact tie parts. |
| pareto2 | min | Checked the two-level-threshold almost-everywhere result for positive excesses. Existing `Endpoint3LomaxTransfer.paretoII_two_level_unique` and `TiesLomax.pareto_ii_global_tie` cover the pointwise and exact tie parts. |

The new mappings are in [lean-ae2.csv](../existence-atlas/lean-map.d/lean-ae2.csv).
