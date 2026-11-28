# Regular count windows and signed zeta in Lean

28 November 2026 · TASK-484 · branch `codex/lean-trunc4` · base `e8b62e8a`

Lean checked sixteen count-window statements and one signed-zeta statement. The count conclusions give attainment, the exact public coefficient and nominal window pairs, and the excluded laws. The held-size negative-binomial statements also cover size zero. The atlas fragment has one mapping per declaration with coverage left blank for independent grading. [Reviewed claims](../existence-atlas/modifier-proofs.csv) · [New mappings](../existence-atlas/modifier-lean-map.d/lean-trunc4.csv) · [Independent review](2026-11-28-atlas-modcov2-report.md#open-formalization)

## Count-window statements

`Window` admits a negative-infinite lower cut and a positive-infinite upper cut. For ordinary conditioning, the auxiliary reduction represents a negative-infinite lower cut by real zero because both retain the same nonnegative counts. The comparison maps onto the old real-cut candidate set and lifts each candidate to an allowed extended window. The exact maximizing pairs are then computed on the extended windows themselves, including the negative-infinite lower cut wherever it belongs to the maximizing fiber. The alias test still requires the exact nominal `[1, Inf]` cuts. [Lean source](../../lean/UnivariateML/Family/Trunc4Count.lean) · [Written known-cut proofs](2026-11-27-trunc-expfam-math-report.md) · [Written estimated-cut proofs](2026-11-27-trunc-est-math-report.md#joint-profiling-lemma)

`RegularClaim` joins the public coefficient bijection, ordinary optimum, positive-window atom and all ordinary bound ties. A retained singleton is flat over compatible ordinary coefficients. On a nonsingleton a fit has one public coefficient, even if several nominal windows attain. The geometric finite threshold is the retained midpoint. The exit laws identify the concentrating count or the normalized carrier and the public coefficient limit along the same tilt path. At zero probability the geometric and size-one negative-binomial carriers give a discrete uniform. The finite negative-tilt upper case also gives strict increase of the scalar log likelihood up to zero tilt.

`NBClaim` selects `RegularClaim` at positive size. At size zero, all zeros give every probability in `(0,1]` on every allowed window retaining zero. A positive sample has one fit when the exact `[1, Inf]` alias is allowed. Probability one is equivalent to unit mean. At interior probability the equation `m = (1-p)/(p * (-log p))` identifies the unique root. Incompatible data have an empty positive-likelihood candidate set.

| Declaration | Reading |
| --- | --- |
| `pois_known` | The fixed-window rate attains at an interior mean, an admitted lower atom or a compatible singleton. The lower and finite upper failures approach the corresponding atoms at rate zero and infinite rate. |
| `pois_lower_estimated` | The lower cut tightens to the sample minimum. Nonconstant data fit, and a positive constant on a nonsingleton reaches the lower atom, admitted only when the positive alias applies. |
| `pois_upper_estimated` | The upper cut tightens to the sample maximum. A constant on a nonsingleton reaches the excluded upper atom. An available looser alias can add the all-ones rate-zero pair to a singleton fit. |
| `pois_both_estimated` | The two sample extrema give the tight window, and every nonempty sample attains. The exact pairs include the singleton fibers and eligible positive aliases. |
| `geom_known` | The ordinary nonsingleton fit uses a mean strictly between the bottom and the midpoint, with an infinite threshold on a ray, or the admitted lower atom. The two excluded laws are the lower atom and the bounded discrete uniform. |
| `geom_lower_estimated` | The tight lower cut is the sample minimum. On a nonsingleton the lower atom is admitted at zero or via the alias, and a mean at or above the finite midpoint selects the uniform limit. |
| `geom_upper_estimated` | The tight upper cut is the sample maximum. The same midpoint is the finite threshold, and an available alias can add a looser all-ones fit. |
| `geom_both_estimated` | A fit exists exactly on a singleton or below the midpoint of the sample extrema. The exact pairs retain ordinary plateaus and the eligible alias. |
| `binom_known` | With size held, the upper retained count is capped by size. An interior mean or an admitted atom gives the nonsingleton fit. An excluded lower or upper atom is approached at probability zero or one. |
| `binom_lower_estimated` | The lower cut tightens to the sample minimum with size clipping on the upper side. A nonsingleton constant uses the lower atom, with admission at zero or at one via the alias. |
| `binom_upper_estimated` | The upper cut tightens to the sample maximum. The upper atom on a nonsingleton is admitted when that maximum equals the held size and excluded below it. |
| `binom_both_estimated` | Every sample compatible with the held size attains at the extrema design, with all maximizing nominal pairs given by the ordinary fibers and the eligible alias. |
| `nbinom_known` | At positive size the ordinary probability uses the retained-bottom and carrier-mean thresholds. Failures approach the lower atom or finite rising-factorial law. At size zero the two compatible branches give the ordinary zero atom and the unique logarithmic alias fit. |
| `nbinom_lower_estimated` | At positive size the bottom is the sample minimum and the same carrier threshold applies. At size zero a compatible positive sample uses exactly the alias even when the sample minimum is above one. |
| `nbinom_upper_estimated` | At positive size the top is the sample maximum and the excluded upper law uses the carrier on that finite window. At size zero a looser infinite upper cut can be required by the unique logarithmic fit. |
| `nbinom_both_estimated` | At positive size the two extrema determine the ordinary window and carrier threshold. At size zero the all-zero fiber and the exact positive alias are the two attainment branches. |

Here are the proposition definitions used by the count statements.

```lean
def RegularClaim {n : ℕ} (x : Fin n → ℕ) (s : RegularCount) (lo hi : ℕ)
    (m : ℝ) (d : Design) (fixed : Window) : Prop :=
  let T := tight d fixed lo hi
  let K := effectiveCut s T
  let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
  let G := fun p ↦ likelihood x (s.weights p) K.kept
  let eligible := atomEligible x d fixed
  let M := countMaxima x s d fixed
  BijOn s.coefficient s.parent s.coefficientDomain ∧
    (M.Nonempty ↔ s.attains K m ∨ eligible) ∧
    (∀ q, q ∈ M ↔
      (eligible ∧ q = publish s atomPair) ∨
      ∃ p, q.1 = s.coefficient p ∧ (p,q.2) ∈ ordinary x s d fixed ∧
        IsMaxOn G E p ∧ (∀ k ∈ cells q.2 \ cells T, s.weights p k = 0) ∧
        (eligible → G p = 1)) ∧
    (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn G E p) ↔ s.attains K m)) ∧
    (¬ K.nonsingleton → ∀ p ∈ E, G p = 1) ∧
    (M = ∅ ↔ s.escapes K m ∧ ¬ eligible) ∧
    (M = ∅ → FaceLaws s K m) ∧
    (s = .geometric → ∀ l u, K = .finite l u →
      Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) s.carrier =
        ((l : ℝ) + u) / 2) ∧
    (K.nonsingleton → (Prod.fst '' M).Subsingleton)
```

```lean
def ZeroNBClaim {n : ℕ} (x : Fin n → ℕ) (m : ℝ) (d : Design) (fixed : Window) : Prop :=
  let M := zeroNBMaxima x m d fixed
  let Z := ∀ i, x i = 0
  let P := ∀ i, 1 ≤ x i
  let W := {w | allows d fixed w ∧ 0 ∈ cells w}
  ((M.Nonempty) ↔ Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow) ∧
  (Z → M = Ioc 0 1 ×ˢ W) ∧
  (P ∧ allows d fixed aliasWindow → ∃ p ∈ Ioc 0 1,
    M = {(p,aliasWindow)} ∧ (p = 1 ↔ m = 1) ∧
      (p < 1 → m = logarithmicMean p ∧ ∀ q ∈ Ioo 0 1, (m = logarithmicMean q ↔ q = p))) ∧
  (¬ (Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow) →
    zeroOrdinary x d fixed ∪ zeroAlias x d fixed = ∅)
```

```lean
def NBClaim {n : ℕ} (x : Fin n → ℕ) (r : ℝ) (lo hi : ℕ) (m : ℝ)
    (d : Design) (fixed : Window) : Prop :=
  if r = 0 then ZeroNBClaim x m d fixed
  else RegularClaim x (.negBinomial r) lo hi m d fixed
```

### Poisson

```lean
theorem pois_known {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .known fixed

theorem pois_lower_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .lower fixed

theorem pois_upper_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .upper fixed

theorem pois_both_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    RegularClaim x .poisson lo hi m .both (⊥, ⊤) ∧
    (countMaxima x (.poisson) .both (⊥, ⊤)).Nonempty
```

### Geometric

```lean
theorem geom_known {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .known fixed

theorem geom_lower_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .lower fixed

theorem geom_upper_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .upper fixed

theorem geom_both_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    RegularClaim x .geometric lo hi m .both (⊥, ⊤) ∧
    ((countMaxima x .geometric .both (⊥, ⊤)).Nonempty ↔ lo = hi ∨ m < ((lo : ℝ) + hi) / 2)
```

### Held-size binomial

```lean
theorem binom_known (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .known fixed

theorem binom_lower_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .lower fixed

theorem binom_upper_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .upper fixed

theorem binom_both_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N) :
    RegularClaim x (.binomial N) lo hi m .both (⊥, ⊤) ∧
    (countMaxima x (.binomial N) .both (⊥, ⊤)).Nonempty
```

### Held-size negative binomial

```lean
theorem nbinom_known {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .known fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .known fixed

theorem nbinom_lower_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .lower fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .lower fixed

theorem nbinom_upper_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .upper fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .upper fixed

theorem nbinom_both_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    NBClaim x r lo hi m .both (⊥, ⊤)
```

## Signed zeta

`UnivariateML.Trunc4Zeta.signed_zeta_known` treats a compatible sample on the finite signed window from one to `N`. For `N = 1` every exponent maximizes and the sample log likelihood is zero. An interior empirical log mean gives a unique finite exponent, with exponent zero when the empirical log mean equals the uniform log mean. The two remaining cases are samples entirely at one or entirely at `N`. Their maximizer sets are empty, the masses approach their respective atoms along the signed infinite-exponent path, and the sample likelihood approaches its least upper bound of one. [Lean source](../../lean/UnivariateML/Family/Trunc4Zeta.lean) · [Written signed-zeta proof](2026-11-27-trunc-expfam-math-report.md#zeta-free-truncateknown)

```lean
theorem signed_zeta_known {N n : ℕ} (hN : 1 ≤ N) (hn : 0 < n)
    (x : Fin n → Finset.Icc 1 N) :
    let m := (∑ i, log ((x i).val : ℝ)) / (n : ℝ)
    let L := fun s ↦ ∑ i, log (mass N (x i).val s)
    let M := {s | IsMaxOn L univ s}
    let atom := fun (e : ℕ) (F : Filter ℝ) ↦
      (∀ j ∈ Finset.Icc 1 N, Tendsto (mass N j) F (𝓝 (if j = e then 1 else 0))) ∧
      IsLUB (range (fun s ↦ exp (L s))) 1 ∧ Tendsto (fun s ↦ exp (L s)) F (𝓝 1)
    ((N = 1 ∧ M = univ ∧ (∀ s, L s = 0) ∨
      1 < N ∧
        (m ∈ Ioo 0 (log (N : ℝ)) ∧ (∃! s, s ∈ M) ∨
         (∀ i, (x i).val = 1) ∧ m = 0 ∧ M = ∅ ∧ atom 1 atTop ∨
         (∀ i, (x i).val = N) ∧ m = log (N : ℝ) ∧ M = ∅ ∧ atom N atBot)) ∧
    (M.Nonempty ↔ N = 1 ∨ m ∈ Ioo 0 (log (N : ℝ)))) ∧
    (m = (∑ k ∈ Finset.Icc 1 N, log (k : ℝ)) / ((Finset.Icc 1 N).card : ℝ) → 0 ∈ M)
```

## Source scope and effort

The reviewed known-window phrase “admitted endpoint atom” includes the exact positive alias in the written count proofs. Eligibility depends on the nominal cuts even when a different window retains the same ordinary cells. The zero-size NB probability range here is the public `(0,1]` domain. For signed zeta, an interior constant sample still fits, and a sample using both endpoints has an interior log mean. These are consequences of the reviewed wording. The proof work did not require a correction to a reviewed row.

The count statements express the laws as pointwise limits on retained cells. The written count argument also discusses total variation. The Lean count failure statements join nonattainment to the law and coefficient limits, and carry the finite upper monotonicity comparison. A separate joint count `IsLUB` statement or a total-variation statement would require an additional declaration. The signed-zeta theorem states the least upper bound and its likelihood convergence explicitly.

| Target | Proof work |
| --- | --- |
| 0. Imports | Replaced the umbrella Mathlib import with four modules for ordered finite products, indicators, real summability, and linear arithmetic. Scanned every modifier mapping fragment for references to the edited file. There were zero line values to update. |
| 1. Regular count windows | Transported candidate comparisons through a surjective reduction of extended windows, while preserving the exact alias. Transferred all maxima back to nominal fibers, attached the public charts and pointwise laws, and proved that an eligible alias excludes an ordinary nonsingleton tie. For size zero, compared the two disjoint compatible candidate sets and proved strict monotonicity of the logarithmic mean equation in public probability. |
| 2. Signed zeta | Transferred optimization to the actual sample log likelihood. Used the two strict log-mean bounds to exhaust the cases and transported the existing endpoint mass and supremum proofs to the sample likelihood. |

## Verification

All Lean runs used the workstation and the shared slot wrapper under `nice -n 10`. The three direct `lake env lean` checks passed. The aggregate `lake build` with both temporary root imports reported `Build completed successfully (8490 jobs).` This is down from the 18,798-job aggregate cited in the brief. The build used the existing Mathlib cache and emitted style lints for module headers, line length and tactic style. The temporary imports were removed and `git diff --exit-code dev/lean/UnivariateML.lean` passed.

Lean's `#print axioms` reported only `propext`, `Classical.choice` and `Quot.sound` for each of the 17 public theorems. The atlas check `python3 dev/tools/existence_atlas.py --check` and `git diff --check` passed. The permanent aggregate imports and coverage grades are left for coordination and independent review.
