# Positive-shape base claims in Lean

TASK-493 · 2026-11-28

Six new public theorems checked in Lean. They cover the generalized-beta failure claim with all three shapes fixed, each nonstandard-beta single-shape spike claim, and the GEV held-location failure claim. The other two theorems cover the generalized-beta constant branch with scale and first shape held, and the held-shape PGW stationary comparison without the retained finite-list proof. Forty-nine of the 53 targets remain open at their full requested scope. Coverage grades in the atlas fragment are blank for independent review.

## Source scope and deviations

No source claim was refuted by these formalizations. The generalized-beta extended log likelihood spells out the dossier's contact convention with an explicit piecewise extended-real value. The nonstandard-beta path statements take the opposite endpoint as an arbitrary strictly containing endpoint, which is always available for a finite sample. The held-shape PGW theorem compares actual stationary pairs to the fitted Gompertz value, including equality. It does not supply the source's retained scalar list.

## Verification

- All four new modules passed individual `lake env lean` checks on the workstation through the one-slot runner.
- The workstation aggregate `lake build` passed with **8,550 jobs** through the four-slot runner with temporary imports. Those imports were removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. The build emitted linter warnings, including header warnings in the new modules and two overlong lines in the generalized-beta module.
- `python3 dev/tools/existence_atlas.py --check` passed. The new modules contain no proof placeholders.
- The worktree changes were left uncommitted.

## Checked declarations

### `UnivariateML.Base9GenBeta.held_scale_shape1_constant_no_fit`

Targets `genbeta` with `scale+shape1` held, criterion and failure. A constant interior sample has unbounded likelihood and no finite fit. Constant branch of criterion and failure. Nonconstant coercivity and characterization of all constant escapes are open.

Reused the proven constant ray and showed that it stays in the feasible shape-power quadrant and exceeds any finite bound.

```lean
theorem held_scale_shape1_constant_no_fit {n : ℕ} (hn : 0 < n)
    {v b p : ℝ} (hv : 0 < v) (hb : v < b) (hp : 0 < p) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun u : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) b p u.1 u.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ u ∈ D, IsMaxOn L D u
```

### `UnivariateML.Base9GenBeta.fixed_shapes_failure`

Targets `genbeta` with `shape1+shape2+shape3` held, failure. Subunit upper shape gives endpoint divergence exactly at sample contact. At upper shape at least one the likelihood attains and finite superlevels lie in compact feasible sets. Failure claim, including constant data and ties, on the extended endpoint convention.

Combined the existing endpoint optimum and spike necessity with a change of variable for arbitrary approaching paths. Proved the superlevel bounds separately at infinity and at open support contact.

```lean
theorem fixed_shapes_failure {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (_hq : 0 < q) (hc : 0 < c) :
    let D := if q = 1 then Ici M else Ioi M
    let L := fun b ↦ logLik x b p q c
    let E : ℝ → EReal := fun b ↦
      if M < b ∨ (b = M ∧ q = 1) then (L b : EReal) else if b = M ∧ q < 1 then ⊤ else ⊥
    (∀ b, E b = ⊤ ↔ b = M ∧ q < 1) ∧
    (q < 1 → Tendsto (fun t ↦ L (M / (1 - t) ^ c⁻¹)) (𝓝[>] 0) atTop ∧
      ∀ {ι : Type} {l : Filter ι} {b : ι → ℝ}, (∀ᶠ j in l, b j ∈ D) →
        (Tendsto (L ∘ b) l atTop ↔ Tendsto b l (𝓝 M))) ∧
    (1 ≤ q → (∃ b ∈ D, IsMaxOn L D b) ∧
      ∀ A : ℝ, ∃ K : Set ℝ, IsCompact K ∧ K ⊆ D ∧
        ∀ b ∈ D, A ≤ L b → b ∈ K)
```

### `UnivariateML.Base9PGW.held_shape_criterion`

Targets `pgw` with `shape` held, criterion. At held shape the finite-fit criterion compares an actual stationary point to the optimized Gompertz face, includes finite equality, and rules out constant samples. The scalar reduction and finiteness of the retained stationary list are not asserted.

Combined the stationary comparison with face optimization and applied the narrow-sample exclusion to constants.

```lean
theorem held_shape_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    let L := fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b p.1 p.2
    let G := AllFreePGW.gompertzLogLik x b
    ∃ r ∈ Ioi 0, IsMaxOn G (Ioi 0) r ∧
      ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
        (∃ i j, x i ≠ x j) ∧ ∃ a > 0, ∃ c > 0,
          deriv (fun s ↦ L (s, c)) a = 0 ∧
          deriv (fun s ↦ L (a, s)) c = 0 ∧ G r ≤ L (a, c))
```

### `UnivariateML.Base9NonstandardBeta.held_shape1_endpoint_spike`

Targets `nsbeta` with `shape1` held, failure. Fixing a lower endpoint outside the sample, a subunit upper shape gives divergence along a feasible path approaching the maximum. The lower endpoint is a free choice and can be any point below the sample. All tied maxima and constant samples are allowed.

Translated the width-coordinate spike and proved strict feasibility and the limiting endpoint in the original coordinates.

```lean
variable {n : ℕ} {x : Fin n → ℝ} {l r a b : ℝ}

theorem held_shape1_endpoint_spike (hl : ∀ i, l < x i) (hr : ∀ i, x i ≤ r)
    (hmax : ∃ i, x i = r) (_ha : 0 < a) (_hb : 0 < b) (hb1 : b < 1) :
    let R := fun t : ℝ ↦ l + (r - l) / (1 - t)
    (∀ᶠ t in 𝓝[>] 0, ∀ i, x i ∈ Ioo l (R t)) ∧
      Tendsto R (𝓝[>] 0) (𝓝 r) ∧
      Tendsto (fun t ↦ NonstandardBeta.logLik x l (R t) (a, b)) (𝓝[>] 0) atTop
```

### `UnivariateML.Base9NonstandardBeta.held_shape2_endpoint_spike`

Targets `nsbeta` with `shape2` held, failure. Fixing an upper endpoint outside the sample, a subunit lower shape gives divergence along a feasible path approaching the minimum. The upper endpoint is a free choice and can be any point above the sample. All tied minima and constant samples are allowed.

Reflected the upper-endpoint construction, restored the shapes in the original likelihood, and checked feasibility and convergence.

```lean
variable {n : ℕ} {x : Fin n → ℝ} {l r a b : ℝ}

theorem held_shape2_endpoint_spike (hl : ∀ i, l ≤ x i) (hr : ∀ i, x i < r)
    (hmin : ∃ i, x i = l) (ha : 0 < a) (ha1 : a < 1) (hb : 0 < b) :
    let L := fun t : ℝ ↦ -(-r + (r - l) / (1 - t))
    (∀ᶠ t in 𝓝[>] 0, ∀ i, x i ∈ Ioo (L t) r) ∧
      Tendsto L (𝓝[>] 0) (𝓝 l) ∧
      Tendsto (fun t ↦ NonstandardBeta.logLik x (L t) r (a, b)) (𝓝[>] 0) atTop
```

### `UnivariateML.Base9GEV.held_location_failure`

Targets `gev` with `mu` held, failure. On nonnegative residuals, the shape-one scale collapse diverges at majority contact. At exact half contact it reaches the finite supremum and finite fits exist precisely for equal positive residuals. Held-location failure claim. The negative residual and strict sub-half cases belong to the criterion claim.

Joined the shape-one collapse paths with the sharp half-contact bound and its reverse-exponential equality characterization.

```lean
theorem held_location_failure {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {m : ℝ} (hm : ∀ i, m ≤ x i) :
    let k := (Finset.univ.filter fun i ↦ x i = m).card
    let D : Set (ℝ × ℝ) := {p | p.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, p.2) ∈ GEV.domain x p.1}
    let L := fun p : ℝ × ℝ ↦ GEV.logLik x p.1 (m, p.2)
    let P := fun s : ℝ ↦ (1, s)
    (∀ s > 0, P s ∈ D) ∧
    (n < 2 * k → Tendsto (L ∘ P) (𝓝[>] 0) atTop ∧
      ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p) ∧
    (n = 2 * k → Tendsto (L ∘ P) (𝓝[>] 0) (𝓝 (Shape2GEV.halfValue x m)) ∧
      IsLUB (L '' D) (Shape2GEV.halfValue x m) ∧
      ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s))
```

## Open targets

The table records the proof work left at this revision. A source or lemma review is distinguished from a checked implementation. “Both” refers to the criterion and failure targets in the same hold mask.

| Family | Held | Target | Work done and missing proof |
|---|---|---|---|
| genbeta | scale | criterion | Located the gamma path and constant concentration path. Need a uniform superlevel exhaustion for all three free parameters, the optimized moving-endpoint power value, and the stationary comparison. |
| genbeta | scale | failure | Reviewed the inherited face classification. Need the necessity of the fitted gamma parameters or the moving-power contact condition, and the constant-data collapse classification. |
| genbeta | scale+shape1 | criterion | Checked unboundedness and nonattainment for every constant sample. Need uniform power and upper-shape tail bounds for nonconstants and compact superlevels. |
| genbeta | scale+shape1 | failure | Checked a feasible divergent constant path. Need compact superlevels for nonconstants, and the collapse and joint-rate conclusions for arbitrary divergent constant paths. |
| genbeta | scale+shape1 | uniqueness | Reviewed the Schur-complement argument. Need the differentiated conditional shape profile, the trigamma integral inequality, and the strict second derivative bound for every positive held first shape. |
| genbeta | scale+shape2 | criterion | Identified the existing unit ridge and gamma path. Need the strict constant comparisons and a complete superlevel envelope with the corresponding optimized face comparisons for both nonunit branches. |
| genbeta | scale+shape2 | failure | Reviewed the split at unit shape. Need the fixed-endpoint power trace below one, exhaustive gamma/power bounds, and a strict finite witness that excludes the power face above one. |
| genbeta | shape1 | failure | Located the existing subunit spike and constant diverging path. Need the joint endpoint/upper-shape/power rates at fixed first shape, including necessity on the log-concave branch. |
| genbeta | shape1+shape2 | failure | Located the joint finite-fit theorem and the fixed-shape constant ray. Need compactness of every nonconstant finite superlevel for varying power and endpoint, and the corresponding subunit joint divergence classification. |
| genbeta | shape2 | criterion | Reviewed the conditional fixed-beta-shape optimization. Need the compactified upper-envelope argument with the optimized shifted gamma and power values above one, and a joined statement with the unit and constant/subunit cases. |
| genbeta | shape2 | failure | Located the fixed-shape spike and unit ridge. Need exhaustion above one for varying power, first shape and endpoint, including the fitted power contact trace, and a joined treatment of the remaining branches. |
| genbeta | shape2+shape3 | uniqueness | Located the existing held-power criterion. Need the strictly concave first-shape profile above one from the trigamma difference bound and the closed-contact uniqueness statement at one. |
| genbeta | shape3 | failure | Located the fixed-power regular superlevel and concentration results. Need the full joint-rate spike/collapse classification at fixed power with both beta shapes and endpoint varying. |
| gb2 | all free | both | Reviewed the mode, curvature and rate charts. Need normalized-density superlevel bounds, sequential compactness and exhaustive finite- and infinite-power limits, with both gamma orientations and all supported one-sided contact traces. The criterion then needs optimized face values and stationary comparison. |
| gb2 | shape1 | both | Located the inverse transformed-gamma realizability result. Need the held-upper-shape exterior bounds in rate coordinates, the fitted upper-cutoff power trace including ties, and the optimized comparison with constants. |
| gb2 | shape3 | both | Reviewed the reflected regime. Need the exact transfer of the complete held-upper-shape exterior and stationary comparison, including the lower-cutoff Pareto trace and constants. |
| betapr | all free | both | Located both path limits and constant unboundedness in AllFreeBetaPrime. Need uniform compactness of the nuisance fit throughout the ratio profile and exhaustive escape to one of the two gamma faces before applying the comparison. |
| betapr | shape1 | criterion | Located the held-first-shape face comparison in Endpoint3BetaPrime. Need a unique analytic conditional scale and the signed score tails, including the noninteger digamma asymptotic argument that prevents cancellation at every order and yields a finite stationary list. |
| betapr | shape2 | criterion | Located the reciprocal likelihood identity and held-second comparison. Need transport of the finite stationary list and its exact equality test from shape1. |
| fpareto | min | both | Reviewed the positive-excess reduction. Need transfer of the complete all-free GB2 exterior, including the supported one-sided envelopes, and proof that the stationary set gives finitely many component heights. |
| fpareto | min+shape1 | both | Located the inverse transformed-gamma path on positive excesses. Need transfer of held-upper-shape exhaustion including tied contacts, then the optimized upper-cutoff power and inverse transformed-gamma comparison over stationary components. |
| fpareto | min+shape3 | both | Located the transformed-gamma path on positive excesses. Need transfer of held-lower-shape exhaustion including tied contacts, then the fitted Pareto and transformed-gamma comparison over stationary components. |
| pgw | all free | criterion | Located the two face paths. Need all-free nuisance compactness, the strict large-inverse-exponent exclusion and analytic collar at zero, then a finite retained stationary list and concentration on constants. |
| pgw | all free | failure | Reviewed the joint inequalities on the powered coordinate and the two face constructions. Need exhaustion uniform in the scale and both shapes, including minimum contacts on the moving threshold path and the constant branch. |
| pgw | all free | uniqueness | The full maximizer set requires the all-free finite retained list, proof that all winners appear on it, and a bijection between tying profile roots and physical maximizing triples. |
| pgw | shape | criterion | Checked a joined constant/nonconstant criterion against the optimized face, with finite equality. Need the analytic nuisance profile at inverse exponent zero, a strict collar and a finite retained scalar stationary list. |
| expweibull | all free | both | Reviewed the two log-coordinate faces. Need uniform boundedness and exhaustive small- and large-shape profile limits in the joint chart, optimized face heights including exact upper contacts, and the constant-data concentration branch. |
| fatigue | beta | criterion | Reviewed the squared rational profile. Need its exact identity after profiling shape, the endpoint exponent and positive trace, the zero infinite-threshold tail, and the intermediate value argument at critical multiplicity. |
| fatigue | beta | failure | Reviewed the ratio penalty at an arbitrary shape. Need the exhaustive two constant-data divergence mechanisms, the nonconstant threshold trace with joint penalty, and the shape-divergence limit. |
| frechet | all free | both | Located unrestricted unboundedness, the half-tie supremum and majority divergence in AllFreeFrechet. Need compactness of the positive reciprocal-shape profile extended by the optimized Gumbel at zero, including the nuisance bounds as reciprocal shape goes to zero. |
| trgamma | all free | both | Located the lognormal and power paths and constant concentration. Need nuisance profiling with the inverse digamma relation, matching upper exteriors to the two path values, and signed score bounds to confine the finite stationary list. |
| invtrgamma | all free | both | Located the inverse paths and concentration. Need to transport the completed stationary and exterior comparison by reciprocity, retaining the full Pareto contact trace. |
| invburr | all free | failure | Reviewed the power and inverse-Weibull routes. Need the reciprocal transport of the normalized-log convex-order dominance test and the sufficient inverse-Weibull comparison, with exact strictness, contact and constant-data cases. |
| nsbeta | max+shape1 | uniqueness | Reviewed the downward-crossing proof for the profiled width score. Need the conditional digamma root and differentiability, the trigamma lower bound, both exterior signs and closed unit-shape contact in the reflected coordinates. |
| nsbeta | min+shape2 | uniqueness | The same profiled-width argument is needed in the forward coordinates, with all support and nonattainment branches handled before asserting uniqueness. |
| gev | all free | criterion | Located the majority and half-tie paths. Need compact upper-semicontinuous superlevels for a strict minimum minority, and the global free-location half-tie bound with its balanced two-value equality characterization. |
| gev | all free | failure | Located the unrestricted upper-endpoint spike and fitted majority divergence. Need the free-location global half-tie pair bound and strictness away from the balanced two-value case. |
| gev | mu | criterion | Checked the majority and half-contact alternatives in the failure theorem. Need compact upper-semicontinuous superlevels when a residual is negative or the zero count is strictly below half. |
