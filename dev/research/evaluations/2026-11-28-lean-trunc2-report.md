# Count truncation in Lean

TASK-480 on `codex/lean-trunc2`, base `d7f83bb4`. The Lean statements use exact real likelihoods for nonempty count samples.

## What the theorems establish

`regular_window_fit` covers fixed cuts and each of the three estimated-cut designs. It resolves real cuts to the retained integer interval or ray and clips the binomial upper count to the held size. The lower-estimated design replaces the lower cut with the sample minimum, the upper-estimated design replaces the upper cut with the sample maximum, and the both-estimated design does both. The ordinary maximizer set keeps exactly the tight-window coefficient fits and their zero-removed-mass nominal fibres. The separate positive-window atom is included at its exact nominal bounds when available. The atlas lists the declarations against the reviewed modifiers and leaves coverage grades for independent review. [Atlas fragment](../existence-atlas/modifier-lean-map.d/lean-trunc2.csv)

| Family | Ordinary nonsingleton criterion and excluded laws |
| --- | --- |
| Poisson | On a finite window the mean must be strictly between the retained endpoints, or be zero with the zero-rate atom admitted. A ray has only the lower threshold. Failure at a positive lower cut selects the lower atom, and failure at the finite upper endpoint selects the upper atom. |
| Geometric | The interior mean is strictly above the lower cut and, on a finite window, strictly below the midpoint. The zero-count atom is included. A finite upper-threshold failure approaches the discrete uniform, and the excluded lower face is the atom at the first retained count. |
| Binomial with size held | After clipping, the mean must be strictly interior or select an admitted endpoint at zero or the held size. The two excluded faces are atoms at the retained endpoints. |
| Negative binomial with positive size held | The interior mean is strictly above the lower cut and, on a finite window, strictly below the normalized rising-factorial carrier mean. The zero-count atom is included. Failure selects the excluded lower atom or the finite zero-tilt carrier. |

Every compatible ordinary singleton has likelihood one throughout the compatible coefficient domain. The ordinary coefficient, when attained on a nonsingleton, is unique. Nominal cuts can still vary across a mass plateau. The probability and rate charts identify the fitted coefficients in the public parameter domains. The geometric zero-truncated result identifies the optimizer itself as the reciprocal positive mean.

At NB size zero, every admissible probability and every allowed nominal window retaining zero fit all-zero data. Positive data can fit on the declared `[1, Inf]` alias, with one joint coefficient-window pair. Unit mean selects the atom at one. Above unit mean the fitted probability is interior and the logarithmic model mean matches the sample mean.

## Source discrepancy and formal scope

The generic tight-window supremum identity requires a common parent mass function at every compared window. The estimated-cut math report already corrects the broader transfer in the known-window report. With held NB size zero, data `(1, 2)`, fixed lower cut one, and estimated upper cut, the ordinary tight window `[1, 2]` has zero event mass at every ordinary coefficient. The declared `[1, Inf]` logarithmic alias can still attain. For Poisson all-ones data with the same design, the ordinary tight singleton and the looser zero-rate alias can both maximize. The Lean union handles these cases explicitly. [Estimated-cut correction](2026-11-27-trunc-est-math-report.md#correction-to-the-known-window-transfer) · [Known-cut proof](2026-11-27-trunc-expfam-math-report.md#count-windows)

The regular-family theorem assumes compatible sample geometry, positive held NB size and a held binomial size at least the sample maximum. The size-zero NB theorem gives the separate compatibility criterion. The sample mean and the attained sample extrema are supplied as hypotheses. Nominal lower cuts are real and nominal upper cuts are real or infinite. A separate negative-infinite lower sentinel is outside this representation. The included lattice of an uncut lower side is represented by a real lower cut at or below zero.

Family proofs use the full ordinary sample objective through carrier cancellation. The size-zero alias uses the shifted scalar likelihood, with positive sample-dependent factors and powers omitted. The boundary statements identify the laws through pointwise convergence of conditional masses. A standalone total-variation statement and a product-likelihood limit identity at each named face remain outside the theorem statements. The nonattainment test and the conditional cell limits are checked separately. These scope choices are recorded for the independent atlas review.

## Effort by target

| Target | Main proof effort |
| --- | --- |
| 1. Estimated-cut profiling | Formalized both nominal cut operations, summable indicator decomposition, exact equality on removed cells, bounded-supremum comparison and transfer of the entire maximizing set. |
| 2. Poisson and geometric | Joined the inherited finite and ray theorems, identified the geometric carrier midpoint, and transferred positive sample likelihoods through the full coefficient chart. |
| 3. Held-size binomial and negative binomial | Proved clipping and endpoint compatibility, transferred the finite and ray objectives with held-size carriers, joined the extra alias atom to the ordinary profile, and treated both size-zero NB branches. The logarithmic score calculation gives the interior alias mean equation. |
| 4. Geometric zero-truncation | Specialized the existing shifted-geometric MLE equivalence and checked the reciprocal coefficient belongs to the probability domain, including unit mean. |

## Verification

The workstation ran both direct `lake env lean` checks and the aggregate `lake build` through the local slot wrapper under `nice -n 10`. The direct checks exited zero. The aggregate build included a temporary root import of the family module, which imports the generic module. The import was removed and `git diff --exit-code -- dev/lean/UnivariateML.lean` passed. The atlas generator's `--check` passed with 146 rows and empty coverage. Both new modules contain complete proofs.

`lake build` reported `Build completed successfully (18788 jobs).`.

## Generic theorem statements

### `UnivariateML.TruncEstimated.tight_cells`

The design obtained by replacing each free cut with its observed extreme is feasible and retains every observation. Its cells are contained in each compatible candidate window. [Lean source](../../lean/UnivariateML/Generic/TruncEstimated.lean#L42)

```lean
theorem tight_cells {ι : Type*} {x : ι → ℕ} {m h : ℕ}
    (hm : ∃ i, x i = m) (hh : ∃ i, x i = h)
    (hb : ∀ i, m ≤ x i ∧ x i ≤ h) {d fixed w}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    allows d fixed (tight d fixed m h) ∧
      (∀ i, x i ∈ cells (tight d fixed m h)) ∧ cells (tight d fixed m h) ⊆ cells w
```

### `UnivariateML.TruncEstimated.likelihood_shrink`

For a summable common mass function and a nonempty sample of positive-mass observations, tightening preserves positive conditioning mass and weakly raises the likelihood. The likelihoods are equal exactly when each removed cell has zero mass. [Lean source](../../lean/UnivariateML/Generic/TruncEstimated.lean#L88)

```lean
theorem likelihood_shrink {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {f : ℕ → ℝ}
    (hf : ∀ k, 0 ≤ f k) (hs : Summable f) (hx : ∀ i, 0 < f (x i))
    {A B : Set ℕ} (hab : A ⊆ B) (ha : ∀ i, x i ∈ A) :
    0 < mass f A ∧ likelihood x f B ≤ likelihood x f A ∧
      (likelihood x f B = likelihood x f A ↔ ∀ k ∈ B \ A, f k = 0)
```

### `UnivariateML.TruncEstimated.likelihood_le_one`

Every compatible conditional sample likelihood is at most one. This bound lets the added atom at one be compared against the entire ordinary model. [Lean source](../../lean/UnivariateML/Generic/TruncEstimated.lean#L130)

```lean
theorem likelihood_le_one {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {f : ℕ → ℝ}
    (hf : ∀ k, 0 ≤ f k) (hs : Summable f) {A : Set ℕ}
    (hx : ∀ i, x i ∈ A ∧ 0 < f (x i)) :
    likelihood x f A ≤ 1
```

### `UnivariateML.TruncEstimated.likelihood_singleton`

On an admitted singleton, every observation has conditional mass one, so the sample likelihood is one. [Lean source](../../lean/UnivariateML/Generic/TruncEstimated.lean#L142)

```lean
theorem likelihood_singleton {n : ℕ} {x : Fin n → ℕ} {f : ℕ → ℝ}
    {j : ℕ} (hx : ∀ i, x i = j) (hj : 0 < f j) :
    likelihood x f {j} = 1
```

### `UnivariateML.TruncEstimated.profile`

The joint and tight-window suprema agree. The joint maximizing set is the tight maximizing coefficient set paired with every admissible nominal window that removes only zero-mass cells when tightened. Attainment transfers in both directions. [Lean source](../../lean/UnivariateML/Generic/TruncEstimated.lean#L157)

```lean
theorem profile {P : Type*} {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {m h : ℕ}
    (hm : ∃ i, x i = m) (hh : ∃ i, x i = h) (hb : ∀ i, m ≤ x i ∧ x i ≤ h)
    {f : P → ℕ → ℝ} {D : Set P}
    (hf : ∀ p ∈ D, ∀ k, 0 ≤ f p k) (hs : ∀ p ∈ D, Summable (f p))
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    let T := tight d fixed m h
    let E := {p ∈ D | ∀ i, 0 < f p (x i)}
    let C := candidates x f D d fixed
    let L := fun p : P × Window ↦ likelihood x (f p.1) (cells p.2)
    sSup (L '' C) = sSup ((fun p ↦ L (p,T)) '' E) ∧
      (∀ q ∈ C, IsMaxOn L C q ↔
        IsMaxOn (fun p ↦ L (p,T)) E q.1 ∧
          ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0) ∧
      ((∃ q ∈ C, IsMaxOn L C q) ↔ ∃ p ∈ E, IsMaxOn (fun r ↦ L (r,T)) E p)
```

## Family theorem statements

### `UnivariateML.Trunc2Known.geom_zt_reciprocal`

The geometric zero-truncated maximizer equals the reciprocal positive sample mean, including probability one at unit mean. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L30)

```lean
theorem geom_zt_reciprocal {m p : ℝ} (hm : 1 ≤ m) :
    (p ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (Discrete.Geometric.likelihood (m-1)) (Ioc 0 1) p) ↔
      p = 1 / m
```

### `UnivariateML.Trunc2Known.nbinom_zero_alias`

The size-zero positive-window logarithmic chart has exactly one maximizing coefficient for a positive sample mean. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L62)

```lean
theorem nbinom_zero_alias {m : ℝ} (hm : 1 ≤ m) :
    ∃! p, p ∈ closedLowerDomain (Iio 0) ∧
      IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1)) (closedLowerDomain (Iio 0)) p
```

### `UnivariateML.Trunc2Known.nbinom_zero_ordinary`

The ordinary size-zero NB candidate domain requires all-zero data and a nominal window retaining zero. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L504)

```lean
theorem nbinom_zero_ordinary {P : Type*} {D : Set P} {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (d : Design) (fixed : Window) (p : P) (w : Window) :
    let f := fun (_ : P) (k : ℕ) ↦ if k = 0 then (1 : ℝ) else 0
    (p,w) ∈ candidates x f D d fixed ↔
      p ∈ D ∧ allows d fixed w ∧ 0 ∈ cells w ∧ ∀ i, x i = 0
```

### `UnivariateML.Trunc2Known.regular_known`

The ordinary fixed effective window has the given attainment criterion, one fit on a nonsingleton when it attains, and likelihood one across compatible coefficients on a singleton. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1332)

```lean
theorem regular_known (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i ∈ K.kept)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let L := fun p ↦ likelihood x (s.weights p) K.kept
    ((∃ p ∈ E, IsMaxOn L E p) ↔ s.attains K m) ∧
      (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn L E p) ↔ s.attains K m)) ∧
      (¬ K.nonsingleton → ∀ p ∈ E, L p = 1)
```

### `UnivariateML.Trunc2Known.regular_coefficient_chart`

The coefficient chart is a bijection to the public rate or probability domain. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1662)

```lean
theorem regular_coefficient_chart (s : RegularCount) :
    BijOn s.coefficient s.parent s.coefficientDomain
```

### `UnivariateML.Trunc2Known.regular_known_failure`

On compatible ordinary data the absence of a maximizer is equivalent to the stated excluded endpoint or carrier boundary case. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1815)

```lean
theorem regular_known_failure (s : RegularCount) (K : CountCut) (hv : s.valid K)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i ∈ K.kept)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let L := fun p ↦ likelihood x (s.weights p) K.kept
    (¬ ∃ p ∈ E, IsMaxOn L E p) ↔ s.escapes K m
```

### `UnivariateML.Trunc2Known.regular_window_fit`

Nominal fixed and estimated cuts resolve to integer cells, with held-size binomial clipping, an exact ordinary and alias attainment test, and all maximizing coefficient-window pairs. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1826)

```lean
theorem regular_window_fit (s : RegularCount)
    (hr : ∀ r, s = .negBinomial r → 0 < r)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w)
    (hN : ∀ N, s = .binomial N → hi ≤ N) :
    let T := tight d fixed lo hi
    let K := s.resolve T
    let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
    let C := candidates x s.weights s.parent d fixed
    let G := fun p ↦ likelihood x (s.weights p) K.kept
    let a : Sum ℝ Bool × Window := (Sum.inr false, positiveWindow)
    let eligible := allows d fixed positiveWindow ∧ ∀ i, x i = 1
    let A := {q | eligible ∧ q = a}
    let L := fun q ↦ if q = a then 1 else likelihood x (s.weights q.1) (cells q.2)
    ((∃ q ∈ C ∪ A, IsMaxOn L (C ∪ A) q) ↔ s.attains K m ∨ eligible) ∧
      (∀ q, (q ∈ C ∪ A ∧ IsMaxOn L (C ∪ A) q) ↔
        q ∈ A ∨ (q ∈ C ∧ IsMaxOn G E q.1 ∧
          (∀ k ∈ cells q.2 \ cells T, s.weights q.1 k = 0) ∧ (eligible → G q.1 = 1))) ∧
      (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn G E p) ↔ s.attains K m)) ∧
      (¬ K.nonsingleton → ∀ p ∈ E, G p = 1) ∧
      ((¬ ∃ q ∈ C ∪ A, IsMaxOn L (C ∪ A) q) ↔ s.escapes K m ∧ ¬ eligible)
```

### `UnivariateML.Trunc2Known.nbinom_zero_window_fit`

Size-zero NB fits all zeros through the ordinary atom or positive data through the available logarithmic alias. It identifies every compatible zero-atom pair and the unique positive-alias pair. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1871)

```lean
theorem nbinom_zero_window_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (d : Design) (fixed : Window) :
    let D := closedLowerDomain (Iio 0)
    let C : Set (Option ℝ × Window) :=
      {q | q.1 ∈ D ∧ allows d fixed q.2 ∧ 0 ∈ cells q.2 ∧ ∀ i, x i = 0}
    let A : Set (Option ℝ × Window) :=
      {q | q.1 ∈ D ∧ allows d fixed positiveWindow ∧ q.2 = positiveWindow}
    let P := ∀ i, 1 ≤ x i
    let W := C ∪ {q ∈ A | P}
    let F := fun q : Option ℝ × Window ↦ closedLowerLikelihood logSeriesCarrier (m-1) q.1
    let L := fun q ↦ if q.2 = positiveWindow then F q else 1
    (∀ q, (q ∈ W ∧ IsMaxOn L W q) ↔
      q ∈ C ∨ (P ∧ q ∈ A ∧ IsMaxOn F A q)) ∧
    ((∃ q ∈ W, IsMaxOn L W q) ↔
      (∀ i, x i = 0) ∧ (∃ w, allows d fixed w ∧ 0 ∈ cells w) ∨
      P ∧ allows d fixed positiveWindow) ∧
    (P → ((∃! q, q ∈ W ∧ IsMaxOn L W q) ↔ allows d fixed positiveWindow))
```

### `UnivariateML.Trunc2Known.regular_finite_faces`

Finite conditional cell masses approach both endpoint atoms for unrestricted tilts or the lower atom and normalized carrier for negative tilts. Above the carrier threshold the finite log objective strictly increases toward the excluded face. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L1999)

```lean
theorem regular_finite_faces (s : RegularCount) {l u j : ℕ} (hlu : l < u)
    (hv : s.valid (.finite l u)) (hj : j ∈ Finset.Icc l u) (m : ℝ) :
    let c := s.carrier
    let M := finiteCountMass l u c j
    Tendsto M atBot (𝓝 (if j = l then 1 else 0)) ∧
      match s with
      | .poisson | .binomial _ => Tendsto M atTop (𝓝 (if j = u then 1 else 0))
      | _ => Tendsto M (𝓝[<] 0) (𝓝 (c j / ∑ k ∈ Finset.Icc l u, c k)) ∧
        (Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ≤ m →
          StrictMonoOn (finiteLogLik l u c m) (Iic 0))
```

### `UnivariateML.Trunc2Known.regular_ray_face`

The shifted ray mass approaches the atom at its first retained count. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L2024)

```lean
theorem regular_ray_face (s : RegularCount) (l j : ℕ) (hv : s.valid (.ray l)) :
    let c := fun k ↦ s.carrier (l+k)
    Tendsto (fun t : ℝ ↦ c j * exp t ^ j / (∑' k : ℕ, c k * exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0))
```

### `UnivariateML.Trunc2Known.geom_carrier_midpoint`

The finite-window geometric zero-tilt threshold is the midpoint of the retained integer extremes. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L2038)

```lean
theorem geom_carrier_midpoint {l u : ℕ} (hlu : l ≤ u) :
    Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) =
      ((l : ℝ) + u) / 2
```

### `UnivariateML.Trunc2Known.nbinom_zero_alias_atom`

The logarithmic alias selects its probability-one atom exactly at unit positive mean. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L2076)

```lean
theorem nbinom_zero_alias_atom {m : ℝ} (_hm : 1 ≤ m) :
    IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1))
      (closedLowerDomain (Iio 0)) none ↔ m = 1
```

### `UnivariateML.Trunc2Known.nbinom_zero_alias_match`

An interior size-zero NB alias maximizer matches the sample mean to the logarithmic model mean in the public probability chart. [Lean source](../../lean/UnivariateML/Family/Trunc2Known.lean#L2143)

```lean
theorem nbinom_zero_alias_match {m t : ℝ} (ht : t < 0)
    (hmax : IsMaxOn (closedLowerLikelihood logSeriesCarrier (m-1))
      (closedLowerDomain (Iio 0)) (some t)) :
    m = exp t / ((1-exp t) * (-log (1-exp t)))
```

