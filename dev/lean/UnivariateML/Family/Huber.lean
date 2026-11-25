module

public import UnivariateML.Generic.LocationScale
public import UnivariateML.Family.Laplace
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Held-shape Huber likelihood

The likelihood omits the fixed normalization constant. The inverse chart
uses `(η, ν) = (1/σ, μ/σ)`. A nonempty sample and positive shape are assumed
where required.

## Deviations

The maximizer sets describe exact real arithmetic. The C++ fitter stores a
single location, using the midpoint convention for flat location intervals.
Free shape and floating-point certification are outside these statements.
-/

@[expose] public section

open Set Real

namespace UnivariateML.Huber

noncomputable section

variable {n : ℕ}

def clip (ε z : ℝ) : ℝ := max (-ε) (min ε z)

def rho (ε z : ℝ) : ℝ := clip ε z * z - (clip ε z) ^ 2 / 2

def inverseLogLik (ε : ℝ) (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  LocationScale.inverseLogLik (rho ε) x

def logLik (ε : ℝ) (x : Fin n → ℝ) : ℝ × ℝ → ℝ :=
  LocationScale.logLik (rho ε) x

def inverseMaximizers (ε : ℝ) (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p ∈ LocationScale.inverseDomain | IsMaxOn (inverseLogLik ε x) LocationScale.inverseDomain p}

def jointMaximizers (ε : ℝ) (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p ∈ LocationScale.domain | IsMaxOn (logLik ε x) LocationScale.domain p}

def locationMaximizers (ε : ℝ) (x : Fin n → ℝ) (σ : ℝ) : Set ℝ :=
  {μ | IsMaxOn (fun t ↦ logLik ε x (t, σ)) univ μ}

def erodedMedian (ε : ℝ) (x : Fin n → ℝ) (hn : 0 < n) (σ : ℝ) : Set ℝ :=
  Icc (Laplace.lowerMedian x hn + ε * σ) (Laplace.upperMedian x hn - ε * σ)

private theorem clip_bounds {ε : ℝ} (hε : 0 ≤ ε) (z : ℝ) :
    -ε ≤ clip ε z ∧ clip ε z ≤ ε :=
  ⟨le_max_left _ _, max_le (by linarith) (min_le_left _ _)⟩

private theorem support_gap {ε : ℝ} (hε : 0 ≤ ε) {c : ℝ}
    (hc : c ∈ Icc (-ε) ε) (z : ℝ) :
    c * z - c ^ 2 / 2 + (c - clip ε z) ^ 2 / 2 ≤ rho ε z := by
  have ha : -ε ≤ c := hc.1
  have hb : c ≤ ε := hc.2
  unfold rho clip
  rcases le_or_gt z (-ε) with hl | hl
  · rw [min_eq_right (show z ≤ ε by linarith), max_eq_left hl]
    nlinarith [mul_nonneg (show 0 ≤ c + ε by linarith) (show 0 ≤ -ε - z by linarith)]
  · rcases le_or_gt ε z with hr | hr
    · rw [min_eq_left hr, max_eq_right (by linarith : -ε ≤ ε)]
      nlinarith [mul_nonneg (sub_nonneg.mpr hb) (sub_nonneg.mpr hr)]
    · rw [min_eq_right hr.le, max_eq_right hl.le]
      ring_nf
      exact le_rfl

private theorem support {ε : ℝ} (hε : 0 ≤ ε) {c : ℝ}
    (hc : c ∈ Icc (-ε) ε) (z : ℝ) :
    c * z - c ^ 2 / 2 ≤ rho ε z := by
  linarith [support_gap hε hc z, sq_nonneg (c - clip ε z)]

/-- The clipped-score expression is the usual quadratic/linear Huber loss. -/
theorem rho_eq_piecewise {ε z : ℝ} (hε : 0 ≤ ε) :
    rho ε z = if |z| ≤ ε then z ^ 2 / 2 else ε * (|z| - ε / 2) := by
  by_cases h : |z| ≤ ε
  · rw [ite_eq_left h]
    obtain ⟨hl, hr⟩ := abs_le.mp h
    simp only [rho, clip, min_eq_right hr, max_eq_right hl]
    ring
  · rw [ite_eq_right h]
    rcases le_or_gt 0 z with hz | hz
    · rw [abs_of_nonneg hz] at h ⊢
      simp only [rho, clip, min_eq_left (le_of_not_ge h), max_eq_right (by linarith : -ε ≤ ε)]
      ring
    · rw [abs_of_neg hz] at h ⊢
      simp only [rho, clip, min_eq_right (by linarith : z ≤ ε),
        max_eq_left (by linarith [lt_of_not_ge h] : z ≤ -ε)]
      ring

private theorem convex_rho {ε : ℝ} (hε : 0 ≤ ε) : ConvexOn ℝ univ (rho ε) := by
  refine ⟨convex_univ, ?_⟩
  intro z _ w _ a b ha hb hab
  let c := clip ε (a * z + b * w)
  have hc : c ∈ Icc (-ε) ε := clip_bounds hε _
  change c * (a * z + b * w) - c ^ 2 / 2 ≤ a * rho ε z + b * rho ε w
  calc
    _ = a * (c * z - c ^ 2 / 2) + b * (c * w - c ^ 2 / 2) := by
      linear_combination (c ^ 2 / 2) * hab
    _ ≤ _ := add_le_add (mul_le_mul_of_nonneg_left (support hε hc z) ha)
      (mul_le_mul_of_nonneg_left (support hε hc w) hb)

private theorem linear_bound {ε : ℝ} (hε : 0 < ε) :
    LocationScale.LinearLowerBound (rho ε) := by
  refine ⟨ε, hε, ε ^ 2 / 2, fun z ↦ ?_⟩
  rcases le_or_gt 0 z with hz | hz
  · rw [abs_of_nonneg hz]
    exact support hε.le ⟨by linarith, le_rfl⟩ z
  · rw [abs_of_neg hz]
    have h := support hε.le (show -ε ∈ Icc (-ε) ε from ⟨le_rfl, by linarith⟩) z
    nlinarith

/-- At fixed shape the nuisance log likelihood is concave in the inverse chart. -/
theorem inverse_concave {ε : ℝ} (hε : 0 < ε) (x : Fin n → ℝ) :
    ConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ε x) :=
  LocationScale.concave_inverseLogLik (convex_rho hε.le) x

/-- Nonconstant samples have at least one joint maximizer. -/
theorem joint_exists {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) : (jointMaximizers ε x).Nonempty :=
  LocationScale.exists_mle (convex_rho hε.le) (linear_bound hε) hx

/-- At a constant sample the joint likelihood is unbounded above. -/
theorem joint_constant_unbounded {ε : ℝ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik ε (fun _ : Fin n ↦ m) '' LocationScale.domain) :=
  LocationScale.not_bddAbove_logLik_constant (rho ε) hn m

/-- Convex combinations of maximizers in the inverse chart are maximizers. -/
theorem inverse_maximizers_convex {ε : ℝ} (hε : 0 < ε) (x : Fin n → ℝ) :
    Convex ℝ (inverseMaximizers ε x) := by
  intro p hp q hq a b ha hb hab
  have hp' := hp.1
  have hq' := hq.1
  have hc := inverse_concave hε x
  refine ⟨hc.1 hp' hq' ha hb hab, ?_⟩
  intro r hr
  have h := hc.2 hp' hq' ha hb hab
  simp only [smul_eq_mul] at h
  have hrp := hp.2 hr
  have hrq := hq.2 hr
  change inverseLogLik ε x r ≤ inverseLogLik ε x p at hrp
  change inverseLogLik ε x r ≤ inverseLogLik ε x q at hrq
  calc
    inverseLogLik ε x r = a * inverseLogLik ε x r + b * inverseLogLik ε x r := by
      rw [← add_mul, hab, one_mul]
    _ ≤ _ := (add_le_add (mul_le_mul_of_nonneg_left hrp ha)
      (mul_le_mul_of_nonneg_left hrq hb)).trans h

private theorem inverse_formula (ε : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) :
    inverseLogLik ε x p = n * log p.1 - ∑ i, rho ε (p.1 * x i - p.2) := by
  simp [inverseLogLik, LocationScale.inverseLogLik, Finset.sum_sub_distrib, nsmul_eq_mul]

/-- All attained joint optima in the inverse chart have the same inverse scale. -/
theorem inverse_scale_eq (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p q : ℝ × ℝ} (hp : p ∈ inverseMaximizers ε x) (hq : q ∈ inverseMaximizers ε x) :
    p.1 = q.1 := by
  by_contra hne
  let m := (1 / 2 : ℝ) • p + (1 / 2 : ℝ) • q
  have hlog : (1 / 2 : ℝ) * log p.1 + (1 / 2 : ℝ) * log q.1 < log m.1 :=
    strictConcaveOn_log_Ioi.2 hp.1.1 hq.1.1 hne (by norm_num) (by norm_num) (by norm_num)
  have hl (i : Fin n) : rho ε (m.1 * x i - m.2) ≤
      (1 / 2 : ℝ) * rho ε (p.1 * x i - p.2) + (1 / 2 : ℝ) * rho ε (q.1 * x i - q.2) := by
    have h := (convex_rho hε.le).2 (mem_univ (p.1 * x i - p.2))
      (mem_univ (q.1 * x i - q.2)) (show (0 : ℝ) ≤ 1 / 2 by norm_num)
      (show (0 : ℝ) ≤ 1 / 2 by norm_num) (by norm_num)
    convert h using 2
    simp [m, smul_eq_mul]
    ring
  have hs := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hl i)
  simp only [Finset.sum_add_distrib, ← Finset.mul_sum] at hs
  have hm := inverse_maximizers_convex hε x hp hq
    (show (0 : ℝ) ≤ 1 / 2 by norm_num) (show (0 : ℝ) ≤ 1 / 2 by norm_num) (by norm_num)
  have hmp := hp.2 hm.1
  have hqp := hq.2 hp.1
  have hpq := hp.2 hq.1
  change inverseLogLik ε x m ≤ inverseLogLik ε x p at hmp
  change inverseLogLik ε x p ≤ inverseLogLik ε x q at hqp
  change inverseLogLik ε x q ≤ inverseLogLik ε x p at hpq
  rw [inverse_formula, inverse_formula] at hmp hqp hpq
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  linarith [mul_lt_mul_of_pos_left hlog hN]


private theorem hasDerivAt_rho {ε : ℝ} (hε : 0 ≤ ε) (z : ℝ) :
    HasDerivAt (rho ε) (clip ε z) z := by
  rw [hasDerivAt_iff_isLittleO]
  apply Asymptotics.IsBigO.trans_isLittleO _ (Asymptotics.isLittleO_pow_sub_sub z
    (show 1 < 2 by decide))
  refine Asymptotics.IsBigO.of_norm_le (fun w ↦ ?_)
  simp only [smul_eq_mul, Real.norm_eq_abs]
  have hl := support hε (show clip ε z ∈ Icc (-ε) ε from clip_bounds hε z) w
  have hu := support_gap hε (show clip ε w ∈ Icc (-ε) ε from clip_bounds hε w) z
  have hnonneg : 0 ≤ rho ε w - rho ε z - (w - z) * clip ε z := by
    change clip ε z * w - clip ε z ^ 2 / 2 ≤ rho ε w at hl
    unfold rho
    change clip ε z * w - clip ε z ^ 2 / 2 ≤
      clip ε w * w - clip ε w ^ 2 / 2 at hl
    linarith
  rw [abs_of_nonneg hnonneg]
  rw [sq_abs]
  have he : rho ε w = clip ε w * w - clip ε w ^ 2 / 2 := rfl
  nlinarith [sq_nonneg (w - z - (clip ε w - clip ε z)), sq_nonneg (w - z)]

private theorem clip_eq_neg {ε z : ℝ} (hε : 0 < ε) :
    clip ε z = -ε ↔ z ≤ -ε := by
  unfold clip
  constructor
  · intro h
    rcases min_choice ε z with he | he
    · rw [he, max_eq_right (by linarith : -ε ≤ ε)] at h
      linarith
    · exact he ▸ (max_eq_left_iff.mp h)
  · intro h
    rw [min_eq_right (by linarith : z ≤ ε), max_eq_left h]

private theorem clip_eq_pos {ε z : ℝ} (hε : 0 < ε) :
    clip ε z = ε ↔ ε ≤ z := by
  unfold clip
  constructor
  · intro h
    rcases max_choice (-ε) (min ε z) with he | he
    · linarith
    · exact (min_eq_left_iff.mp (he.symm.trans h))
  · intro h
    rw [min_eq_left h, max_eq_right (by linarith : -ε ≤ ε)]

private theorem eq_of_clip_eq_of_core {ε z w : ℝ} (hε : 0 < ε)
    (hl : -ε < clip ε z) (hu : clip ε z < ε) (h : clip ε z = clip ε w) : z = w := by
  have core (v : ℝ) (hl : -ε < clip ε v) (hu : clip ε v < ε) : clip ε v = v := by
    have hv₁ : -ε ≤ v := by
      by_contra! hn
      exact hl.ne' ((clip_eq_neg hε).mpr hn.le)
    have hv₂ : v ≤ ε := by
      by_contra! hn
      exact hu.ne ((clip_eq_pos hε).mpr hn.le)
    simp [clip, min_eq_right hv₂, max_eq_right hv₁]
  exact (core z hl hu).symm.trans (h.trans (core w (h ▸ hl) (h ▸ hu)))

private theorem clip_continuous (ε : ℝ) : Continuous (clip ε) :=
  continuous_const.max (continuous_const.min continuous_id)

def locationScore (ε : ℝ) (x : Fin n → ℝ) (σ μ : ℝ) : ℝ :=
  ∑ i, clip ε ((x i - μ) / σ)

private theorem hasDerivAt_location {ε σ : ℝ} (hε : 0 < ε)
    (x : Fin n → ℝ) (μ : ℝ) :
    HasDerivAt (fun t ↦ logLik ε x (t, σ)) (locationScore ε x σ μ / σ) μ := by
  have hterm (i : Fin n) :
      HasDerivAt (fun t ↦ -log σ - rho ε ((x i - t) / σ))
        (clip ε ((x i - μ) / σ) / σ) μ := by
    have ha := ((hasDerivAt_const μ (x i)).sub (hasDerivAt_id μ)).div_const σ
    convert (hasDerivAt_const μ (-log σ)).sub
      ((hasDerivAt_rho hε.le ((x i - μ) / σ)).comp μ ha) using 1
    · rfl
    · ring
  rw [locationScore, Finset.sum_div]
  exact HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hterm i)

/-- At held positive scale, a location maximizes exactly when its clipped score is zero. -/
theorem mem_locationMaximizers_iff {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) (μ : ℝ) :
    μ ∈ locationMaximizers ε x σ ↔ locationScore ε x σ μ = 0 := by
  constructor
  · intro h
    have hm : IsMaxOn (fun t ↦ logLik ε x (t, σ)) univ μ := h
    have hd := hm.isLocalMax (Filter.univ_mem : (univ : Set ℝ) ∈ nhds μ)
    have he := (hasDerivAt_location (σ := σ) hε x μ).deriv
    rw [hd.deriv_eq_zero] at he
    exact (div_eq_zero_iff.mp he.symm).resolve_right hσ.ne'
  · intro h t _
    let c (i : Fin n) := clip ε ((x i - μ) / σ)
    have hi (i : Fin n) : -log σ - rho ε ((x i - t) / σ) ≤
        -log σ - rho ε ((x i - μ) / σ) + c i * ((t - μ) / σ) := by
      have hh := support hε.le (show c i ∈ Icc (-ε) ε from clip_bounds hε.le _) ((x i - t) / σ)
      change _ ≤ -log σ - (c i * ((x i - μ) / σ) - (c i) ^ 2 / 2) + _
      simp only [sub_div] at hh ⊢
      nlinarith
    have hs := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hi i)
    rw [Finset.sum_add_distrib, ← Finset.sum_mul, show ∑ i, c i = 0 from h, zero_mul,
      add_zero] at hs
    exact hs

/-- Every nonempty sample has a maximizing location at held positive scale. -/
theorem heldScale_exists (_hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) : (locationMaximizers ε x σ).Nonempty := by
  let M : ℝ := (∑ i, |x i|) + ε * σ
  have hM : 0 ≤ M := add_nonneg (Finset.sum_nonneg fun _ _ ↦ abs_nonneg _) (by positivity)
  have hx (i : Fin n) : |x i| ≤ ∑ j, |x j| :=
    Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j)) (Finset.mem_univ i)
  have hleft (i : Fin n) : clip ε ((x i - -M) / σ) = ε :=
    (clip_eq_pos hε).mpr ((le_div_iff₀ hσ).mpr (by dsimp [M]; linarith [hx i, neg_abs_le (x i)]))
  have hright (i : Fin n) : clip ε ((x i - M) / σ) = -ε :=
    (clip_eq_neg hε).mpr ((div_le_iff₀ hσ).mpr (by dsimp [M]; linarith [hx i, le_abs_self (x i)]))
  have hc : Continuous (locationScore ε x σ) := by
    unfold locationScore
    exact continuous_finsetSum _ fun i _ ↦ (clip_continuous ε).comp
      ((continuous_const.sub continuous_id).div_const σ)
  have hz : (0 : ℝ) ∈ Icc (locationScore ε x σ M) (locationScore ε x σ (-M)) := by
    simp only [locationScore, hleft, hright, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul, mem_Icc]
    exact ⟨mul_nonpos_of_nonneg_of_nonpos (Nat.cast_nonneg _) (neg_nonpos.mpr hε.le),
      mul_nonneg (Nat.cast_nonneg _) hε.le⟩
  obtain ⟨μ, _, hμ⟩ := intermediate_value_Icc' (show -M ≤ M by linarith) hc.continuousOn hz
  exact ⟨μ, (mem_locationMaximizers_iff hε hσ x μ).mpr hμ⟩

private theorem clip_eq_of_location_max {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    {x : Fin n → ℝ} {μ ν : ℝ} (hμ : μ ∈ locationMaximizers ε x σ)
    (hν : ν ∈ locationMaximizers ε x σ) (i : Fin n) :
    clip ε ((x i - μ) / σ) = clip ε ((x i - ν) / σ) := by
  let r (t : ℝ) (j : Fin n) := (x j - t) / σ
  let c (j : Fin n) := clip ε (r μ j)
  have hzero : ∑ j, c j = 0 := (mem_locationMaximizers_iff hε hσ x μ).mp hμ
  have hi (j : Fin n) : rho ε (r μ j) - c j * ((ν - μ) / σ) +
      (c j - clip ε (r ν j)) ^ 2 / 2 ≤ rho ε (r ν j) := by
    have h := support_gap hε.le (show c j ∈ Icc (-ε) ε from clip_bounds hε.le _) (r ν j)
    have hr : r ν j = r μ j - (ν - μ) / σ := by dsimp [r]; ring
    change c j * r ν j - (c j) ^ 2 / 2 + _ ≤ _ at h
    change c j * r μ j - (c j) ^ 2 / 2 - _ + _ ≤ _
    linear_combination h - c j * hr
  have hs := Finset.sum_le_sum (s := Finset.univ) fun j _ ↦ hi j
  rw [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.sum_mul, hzero,
    zero_mul, sub_zero] at hs
  have hm : IsMaxOn (fun t ↦ logLik ε x (t, σ)) univ ν := hν
  have hl := hm (mem_univ μ)
  change logLik ε x (μ, σ) ≤ logLik ε x (ν, σ) at hl
  simp only [logLik, LocationScale.logLik, Finset.sum_sub_distrib] at hl
  have hle : (∑ j, (c j - clip ε (r ν j)) ^ 2 / 2) ≤ 0 := by linarith
  have hsingle := Finset.single_le_sum (s := Finset.univ)
    (f := fun j ↦ (c j - clip ε (r ν j)) ^ 2 / 2) (fun j _ ↦ by positivity)
    (Finset.mem_univ i)
  change c i = clip ε (r ν i)
  nlinarith [hsingle.trans hle]


private theorem mem_eroded_of_distinct_max (hn : 0 < n) {ε σ : ℝ}
    (hε : 0 < ε) (hσ : 0 < σ) {x : Fin n → ℝ} {μ ν : ℝ}
    (hμ : μ ∈ locationMaximizers ε x σ) (hν : ν ∈ locationMaximizers ε x σ)
    (hne : μ ≠ ν) : μ ∈ erodedMedian ε x hn σ := by
  let c (i : Fin n) := clip ε ((x i - μ) / σ)
  have ht (i : Fin n) : c i = -ε ∨ c i = ε := by
    rcases clip_bounds hε.le ((x i - μ) / σ) with ⟨hl, hu⟩
    rcases hl.eq_or_lt with hl | hl
    · exact Or.inl hl.symm
    rcases hu.eq_or_lt with hu | hu
    · exact Or.inr hu
    have hr := eq_of_clip_eq_of_core hε hl hu (clip_eq_of_location_max hε hσ hμ hν i)
    exact False.elim (hne (by rw [div_left_inj₀ hσ.ne'] at hr; linarith))
  have hsign (i : Fin n) : c i * (x i - μ) = ε * |x i - μ| := by
    rcases ht i with h | h
    · have hr := (div_le_iff₀ hσ).mp ((clip_eq_neg hε).mp h)
      rw [h, abs_of_nonpos (by nlinarith [mul_pos hε hσ] : x i - μ ≤ 0)]
      ring
    · have hr := (le_div_iff₀ hσ).mp ((clip_eq_pos hε).mp h)
      rw [h, abs_of_nonneg (by nlinarith [mul_pos hε hσ] : 0 ≤ x i - μ)]
  have hzero : ∑ i, c i = 0 := (mem_locationMaximizers_iff hε hσ x μ).mp hμ
  have habs (t : ℝ) : (∑ i, |x i - μ|) ≤ ∑ i, |x i - t| := by
    have hi (i : Fin n) : ε * |x i - μ| + c i * (μ - t) ≤ ε * |x i - t| := by
      rw [← hsign]
      have he : c i * (x i - μ) + c i * (μ - t) = c i * (x i - t) := by ring
      rw [he]
      rcases ht i with h | h
      · rw [h, neg_mul, ← mul_neg]
        exact mul_le_mul_of_nonneg_left (by linarith [neg_abs_le (x i - t)]) hε.le
      · rw [h]
        exact mul_le_mul_of_nonneg_left (le_abs_self _) hε.le
    have hs := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hi i
    rw [Finset.sum_add_distrib, ← Finset.sum_mul, hzero, zero_mul, add_zero,
      ← Finset.mul_sum, ← Finset.mul_sum] at hs
    exact (mul_le_mul_iff_right₀ hε).mp hs
  have hm : μ ∈ Laplace.medianSet x hn := by
    rw [← Laplace.heldScale hn x (show (0 : ℝ) < 1 by norm_num)]
    intro t _
    change Laplace.logLik x t 1 ≤ Laplace.logLik x μ 1
    simp [Laplace.logLik, Laplace.logDensity, Finset.sum_sub_distrib]
    linarith [habs t]
  have hlow (i : Fin n) (hi : x i ≤ μ) : x i + ε * σ ≤ μ := by
    rcases ht i with h | h
    · have hr := (div_le_iff₀ hσ).mp ((clip_eq_neg hε).mp h)
      linarith
    · have hr := (le_div_iff₀ hσ).mp ((clip_eq_pos hε).mp h)
      nlinarith [mul_pos hε hσ]
  have hhigh (i : Fin n) (hi : μ ≤ x i) : μ ≤ x i - ε * σ := by
    rcases ht i with h | h
    · have hr := (div_le_iff₀ hσ).mp ((clip_eq_neg hε).mp h)
      nlinarith [mul_pos hε hσ]
    · have hr := (le_div_iff₀ hσ).mp ((clip_eq_pos hε).mp h)
      linarith
  exact ⟨hlow _ hm.1, hhigh _ hm.2⟩

private theorem location_max_of_mem_eroded (hn : 0 < n) {ε σ : ℝ}
    (hε : 0 < ε) (hσ : 0 < σ) {x : Fin n → ℝ} {μ : ℝ}
    (hμ : μ ∈ erodedMedian ε x hn σ) : μ ∈ locationMaximizers ε x σ := by
  let l : Fin n := ⟨(n - 1) / 2, by omega⟩
  let u : Fin n := ⟨n / 2, by omega⟩
  have hlo : Laplace.orderStat x l + ε * σ ≤ μ := hμ.1
  have hhi : μ ≤ Laplace.orderStat x u - ε * σ := hμ.2
  have heven : n % 2 = 0 := by
    by_contra h
    have he : l = u := Fin.ext (by dsimp [l, u]; omega)
    rw [he] at hlo
    nlinarith [mul_pos hε hσ]
  let f (i : Fin n) := clip ε ((Laplace.orderStat x i - μ) / σ)
  have hl (i : Fin n) (hi : i ≤ l) : f i = -ε := by
    have hr := Tuple.monotone_sort x hi
    exact (clip_eq_neg hε).mpr ((div_le_iff₀ hσ).mpr (by
      change Laplace.orderStat x i ≤ Laplace.orderStat x l at hr
      linarith))
  have hu (i : Fin n) (hi : u ≤ i) : f i = ε := by
    have hr := Tuple.monotone_sort x hi
    exact (clip_eq_pos hε).mpr ((le_div_iff₀ hσ).mpr (by
      change Laplace.orderStat x u ≤ Laplace.orderStat x i at hr
      linarith))
  have hpair (i : Fin n) : f i + f i.rev = 0 := by
    rcases le_or_gt i l with h | h
    · have hr : u ≤ i.rev := by
        have hh : (i : ℕ) ≤ (n - 1) / 2 := h
        change n / 2 ≤ (i.rev : ℕ)
        rw [Fin.val_rev]
        omega
      rw [hl _ h, hu _ hr, neg_add_cancel]
    · have hh : (n - 1) / 2 < (i : ℕ) := h
      have hi : u ≤ i := by change n / 2 ≤ (i : ℕ); omega
      have hr : i.rev ≤ l := by
        change (i.rev : ℕ) ≤ (n - 1) / 2
        rw [Fin.val_rev]
        omega
      rw [hu _ hi, hl _ hr, add_neg_cancel]
  have hrev : ∑ i : Fin n, f i.rev = ∑ i : Fin n, f i :=
    Equiv.sum_comp (Fin.revPerm : Equiv.Perm (Fin n)) f
  have hs : ∑ i : Fin n, (f i + f i.rev) = 0 := by simp [hpair]
  rw [Finset.sum_add_distrib, hrev, ← two_mul] at hs
  have hz : ∑ i : Fin n, f i = 0 := (mul_eq_zero.mp hs).resolve_left (by norm_num)
  apply (mem_locationMaximizers_iff hε hσ x μ).mpr
  unfold locationScore
  rw [← Equiv.sum_comp (Tuple.sort x) (fun i ↦ clip ε ((x i - μ) / σ))]
  exact hz

/-- A nonempty eroded central interval is the entire maximizing location set. -/
theorem heldScale_interval (hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    {x : Fin n → ℝ} (hgap : (erodedMedian ε x hn σ).Nonempty) :
    locationMaximizers ε x σ = erodedMedian ε x hn σ := by
  obtain ⟨a, ha⟩ := hgap
  have hma := location_max_of_mem_eroded hn hε hσ ha
  apply Subset.antisymm
  · intro μ hμ
    rcases eq_or_ne μ a with rfl | hne
    · exact ha
    · exact mem_eroded_of_distinct_max hn hε hσ hμ hma hne
  · exact fun _ h ↦ location_max_of_mem_eroded hn hε hσ h

/-- Location is unique exactly when the eroded central gap has no positive width. -/
theorem heldScale_unique_iff (hn : 0 < n) {ε σ : ℝ} (hε : 0 < ε) (hσ : 0 < σ)
    (x : Fin n → ℝ) :
    (∃! μ, μ ∈ locationMaximizers ε x σ) ↔
      Laplace.upperMedian x hn - Laplace.lowerMedian x hn ≤ 2 * (ε * σ) := by
  let a := Laplace.lowerMedian x hn + ε * σ
  let b := Laplace.upperMedian x hn - ε * σ
  have he : b ≤ a ↔ Laplace.upperMedian x hn - Laplace.lowerMedian x hn ≤ 2 * (ε * σ) := by
    dsimp [a, b]
    constructor <;> intro h <;> linarith
  rw [← he]
  constructor
  · rintro ⟨m, _, hm⟩
    by_contra! h
    have hma : a ∈ locationMaximizers ε x σ :=
      location_max_of_mem_eroded hn hε hσ (show a ∈ erodedMedian ε x hn σ from ⟨le_rfl, h.le⟩)
    have hmb : b ∈ locationMaximizers ε x σ :=
      location_max_of_mem_eroded hn hε hσ (show b ∈ erodedMedian ε x hn σ from ⟨h.le, le_rfl⟩)
    exact h.ne ((hm a hma).trans (hm b hmb).symm)
  · intro h
    obtain ⟨m, hm⟩ := heldScale_exists hn hε hσ x
    refine ⟨m, hm, fun t ht ↦ ?_⟩
    by_contra hne
    have h₁ : t ∈ Icc a b := mem_eroded_of_distinct_max hn hε hσ ht hm hne
    have h₂ : m ∈ Icc a b := mem_eroded_of_distinct_max hn hε hσ hm ht (fun h ↦ hne h.symm)
    exact hne (le_antisymm (h₁.2.trans (h.trans h₂.1)) (h₂.2.trans (h.trans h₁.1)))


private theorem inverse_max_of_joint {ε : ℝ} {x : Fin n → ℝ} {p : ℝ × ℝ}
    (hp : p ∈ jointMaximizers ε x) :
    (LocationScale.inverseEquiv ⟨p, hp.1⟩ : ℝ × ℝ) ∈ inverseMaximizers ε x := by
  let q := LocationScale.inverseEquiv ⟨p, hp.1⟩
  refine ⟨q.property, ?_⟩
  intro r hr
  obtain ⟨t, ht⟩ := LocationScale.inverseEquiv.surjective ⟨r, hr⟩
  have he : (LocationScale.inverseEquiv t : ℝ × ℝ) = r := congrArg Subtype.val ht
  change inverseLogLik ε x r ≤ inverseLogLik ε x q
  rw [← he]
  have hmax : logLik ε x t ≤ logLik ε x p := hp.2 t.property
  simpa only [inverseLogLik, logLik, q, LocationScale.inverseLogLik_inverseEquiv] using hmax

/-- Any joint maximizer determines the common scale and the full location slice. -/
theorem joint_eq_product (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x) :
    jointMaximizers ε x = locationMaximizers ε x p.2 ×ˢ {p.2} := by
  ext q
  constructor
  · intro hq
    have hs := inverse_scale_eq hn hε (inverse_max_of_joint hq) (inverse_max_of_joint hp)
    change q.2⁻¹ = p.2⁻¹ at hs
    have he : q.2 = p.2 := inv_injective hs
    refine ⟨?_, he⟩
    intro t _
    have ht : (t, q.2) ∈ LocationScale.domain := ⟨mem_univ _, hq.1.2⟩
    have hm : logLik ε x (t, q.2) ≤ logLik ε x q := hq.2 ht
    change logLik ε x (t, p.2) ≤ logLik ε x (q.1, p.2)
    rw [← he, Prod.mk.eta]
    exact hm
  · intro hq
    have he : q.2 = p.2 := hq.2
    have hm : IsMaxOn (fun t ↦ logLik ε x (t, p.2)) univ q.1 := hq.1
    have h : logLik ε x p ≤ logLik ε x q := by
      have hh := hm (mem_univ p.1)
      change logLik ε x (p.1, p.2) ≤ logLik ε x (q.1, p.2) at hh
      simpa only [← he, Prod.mk.eta] using
        (show logLik ε x p ≤ logLik ε x (q.1, p.2) from hh)
    have hdom : q ∈ LocationScale.domain := ⟨mem_univ _, he.symm ▸ hp.1.2⟩
    exact ⟨hdom, fun _ hr ↦ (hp.2 hr).trans h⟩

/-- At a joint optimum with a nonempty eroded gap, the set is exactly that segment. -/
theorem joint_interval (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x)
    (hgap : (erodedMedian ε x hn p.2).Nonempty) :
    jointMaximizers ε x = erodedMedian ε x hn p.2 ×ˢ {p.2} := by
  rw [joint_eq_product hn hε hp, heldScale_interval hn hε hp.1.2 hgap]

/-- The joint MLE is unique exactly when the central gap is at most two transition widths. -/
theorem joint_unique_iff (hn : 0 < n) {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ}
    {p : ℝ × ℝ} (hp : p ∈ jointMaximizers ε x) :
    (∃! q, q ∈ jointMaximizers ε x) ↔
      Laplace.upperMedian x hn - Laplace.lowerMedian x hn ≤ 2 * (ε * p.2) := by
  rw [joint_eq_product hn hε hp, ← heldScale_unique_iff hn hε hp.1.2 x]
  constructor
  · rintro ⟨q, hq, hu⟩
    exact ⟨q.1, hq.1, fun a ha ↦ congrArg Prod.fst (hu (a, p.2) ⟨ha, rfl⟩)⟩
  · rintro ⟨μ, hμ, hu⟩
    exact ⟨(μ, p.2), ⟨hμ, rfl⟩, fun q hq ↦ Prod.ext (hu q.1 hq.1) hq.2⟩

def scaleScore (ε : ℝ) (x : Fin n → ℝ) (μ η : ℝ) : ℝ :=
  ∑ i, (η * (x i - μ)) * clip ε (η * (x i - μ))

private theorem held_inverse_formula (ε : ℝ) (x : Fin n → ℝ) (μ : ℝ) {η : ℝ}
    (_hη : 0 < η) :
    logLik ε x (μ, η⁻¹) = n * log η - ∑ i, rho ε (η * (x i - μ)) := by
  simp only [logLik, LocationScale.logLik, log_inv, neg_neg, div_inv_eq_mul,
    mul_comm (x _ - μ) η, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]

private theorem strict_scale_of_score (hn : 0 < n) {ε : ℝ} (hε : 0 < ε)
    {x : Fin n → ℝ} {μ η t : ℝ} (hη : 0 < η) (ht : 0 < t)
    (hs : scaleScore ε x μ η = n) (hne : t ≠ η) :
    logLik ε x (μ, t⁻¹) < logLik ε x (μ, η⁻¹) := by
  let d (i : Fin n) := x i - μ
  let z (i : Fin n) := η * d i
  let c (i : Fin n) := clip ε (z i)
  have hi (i : Fin n) : rho ε (z i) + (t / η - 1) * (z i * c i) ≤ rho ε (t * d i) := by
    have h := support hε.le (show c i ∈ Icc (-ε) ε from clip_bounds hε.le _) (t * d i)
    convert h using 1
    change c i * z i - (c i) ^ 2 / 2 + (t / η - 1) * (z i * c i) =
      c i * (t * d i) - (c i) ^ 2 / 2
    dsimp only [z]
    field_simp [hη.ne']
    ring
  have hsum := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hi i)
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, show ∑ i, z i * c i = n from hs] at hsum
  have hr : t / η ≠ 1 := fun h ↦ hne (by simpa using (div_eq_iff hη.ne').mp h)
  have hlog := log_lt_sub_one_of_pos (div_pos ht hη) hr
  rw [log_div ht.ne' hη.ne'] at hlog
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  rw [held_inverse_formula ε x μ ht, held_inverse_formula ε x μ hη]
  change n * log t - ∑ i, rho ε (t * d i) < n * log η - ∑ i, rho ε (z i)
  linarith [mul_lt_mul_of_pos_left hlog hN]

private theorem scale_score_root {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    ∃ η > 0, scaleScore ε x μ η = n := by
  obtain ⟨j, hj⟩ := hx
  have hn : (0 : ℝ) < n := by exact_mod_cast (Nat.zero_le j.val).trans_lt j.isLt
  let D := |x j - μ|
  have hD : 0 < D := abs_pos.mpr (sub_ne_zero.mpr hj)
  let T := (ε + n / ε) / D
  have hT : 0 < T := by positivity
  have hzero : scaleScore ε x μ 0 = 0 := by simp [scaleScore]
  have hcont : Continuous (scaleScore ε x μ) := by
    unfold scaleScore
    exact continuous_finsetSum _ fun i _ ↦
      (continuous_id.mul continuous_const).mul
        ((clip_continuous ε).comp (continuous_id.mul continuous_const))
  have hnonneg (z : ℝ) : 0 ≤ z * clip ε z := by
    rcases le_or_gt 0 z with hz | hz
    · exact mul_nonneg hz (le_trans (le_min hε.le hz) (le_max_right _ _))
    · exact mul_nonneg_of_nonpos_of_nonpos hz.le
        (max_le (by linarith) ((min_le_right _ _).trans hz.le))
  have hTD : T * D = ε + n / ε := div_mul_cancel₀ _ hD.ne'
  have hR : ε ≤ |T * (x j - μ)| := by
    rw [abs_mul, abs_of_pos hT]
    change ε ≤ T * D
    rw [hTD]
    exact le_add_of_nonneg_right (div_nonneg hn.le hε.le)
  have htj : T * (x j - μ) * clip ε (T * (x j - μ)) = ε * (T * D) := by
    rcases le_or_gt 0 (T * (x j - μ)) with hz | hz
    · rw [(clip_eq_pos hε).mpr (by rwa [abs_of_nonneg hz] at hR), mul_comm _ ε,
        ← abs_of_nonneg hz, abs_mul, abs_of_pos hT]
    · rw [(clip_eq_neg hε).mpr (by rw [abs_of_neg hz] at hR; linarith), mul_neg,
        mul_comm _ ε, ← mul_neg, ← abs_of_neg hz, abs_mul, abs_of_pos hT]
  have hupper : (n : ℝ) ≤ scaleScore ε x μ T := calc
    _ ≤ ε * (T * D) := by
      rw [hTD, mul_add, mul_comm ε ((n : ℝ) / ε), div_mul_cancel₀ _ hε.ne']
      nlinarith
    _ ≤ _ := by
      rw [← htj]
      exact Finset.single_le_sum (s := Finset.univ)
        (fun i _ ↦ hnonneg (T * (x i - μ))) (Finset.mem_univ j)
  obtain ⟨η, hη, he⟩ := intermediate_value_Icc hT.le hcont.continuousOn
    (show (n : ℝ) ∈ Icc (scaleScore ε x μ 0) (scaleScore ε x μ T) from
      ⟨hzero.symm ▸ hn.le, hupper⟩)
  refine ⟨η, lt_of_le_of_ne hη.1 ?_, he⟩
  intro hz
  rw [← hz, hzero] at he
  exact hn.ne' he.symm

/-- With held location and a nonzero residual the scale MLE is unique. -/
theorem heldLocation_existsUnique {ε : ℝ} (hε : 0 < ε) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    ∃! σ, 0 < σ ∧ IsMaxOn (fun s ↦ logLik ε x (μ, s)) (Ioi 0) σ := by
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact (Nat.zero_le i.val).trans_lt i.isLt
  obtain ⟨η, hη, hs⟩ := scale_score_root hε hx
  have hstrict (s : ℝ) (hs' : 0 < s) (hne : s ≠ η⁻¹) :
      logLik ε x (μ, s) < logLik ε x (μ, η⁻¹) := by
    have he : s⁻¹ ≠ η := fun h ↦ hne (by simpa using congrArg Inv.inv h)
    simpa only [inv_inv] using strict_scale_of_score hn hε hη (inv_pos.mpr hs') hs he
  refine ⟨η⁻¹, ⟨inv_pos.mpr hη, ?_⟩, ?_⟩
  · intro s hs'
    rcases eq_or_ne s η⁻¹ with rfl | hne
    · exact le_rfl
    · exact (hstrict s hs' hne).le
  · intro s hs'
    by_contra hne
    exact (not_le_of_gt (hstrict s hs'.1 hne)) (hs'.2 (inv_pos.mpr hη))

/-- When all held-location residuals vanish, the likelihood is unbounded above. -/
theorem heldLocation_unbounded (hn : 0 < n) {ε μ : ℝ} {x : Fin n → ℝ}
    (hx : ∀ i, x i = μ) :
    ∀ M : ℝ, ∃ σ > 0, M < logLik ε x (μ, σ) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  intro M
  let η := exp ((M + 1) / n + rho ε 0)
  have hη : 0 < η := exp_pos _
  refine ⟨η⁻¹, inv_pos.mpr hη, ?_⟩
  rw [held_inverse_formula ε x μ hη]
  simp only [hx, sub_self, mul_zero, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, η, log_exp, mul_add, mul_comm (n : ℝ) ((M + 1) / n),
    div_mul_cancel₀ _ hN.ne', add_sub_cancel_right]
  linarith

end

end UnivariateML.Huber
