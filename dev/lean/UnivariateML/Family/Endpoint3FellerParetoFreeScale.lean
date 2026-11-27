module

public import UnivariateML.Family.Endpoint3FellerPareto

import Mathlib.Tactic

set_option linter.style.header false

/-! # Joint uniqueness for fixed-shape threshold and scale -/

@[expose] public section

open Real Set
open scoped BigOperators

namespace UnivariateML.Endpoint3FellerParetoFreeScale

open Endpoint2FellerParetoFreeScale

variable {n : ℕ}

private noncomputable def odds (c r : ℝ) : ℝ := r ^ c / (1 + r ^ c)

private theorem odds_strictMono {c : ℝ} (hc : 0 < c) :
    StrictMonoOn (odds c) (Ici 0) := by
  intro x hx y hy hxy
  have hpow := rpow_lt_rpow (show 0 ≤ x from hx) hxy hc
  unfold odds
  apply (div_lt_div_iff₀ (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg hx c))
    (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg hy c))).mpr
  linarith

private theorem scale_cell_deriv {a b c K y z : ℝ}
    (hc : 0 < c) (hy : 0 ≤ y) (hreg : 0 < y ∨ a = 1) :
    HasDerivAt (fun v ↦ K - log (exp v) + (a - 1) * log (y / exp v) -
      ((a + b) / c) * log (1 + (y / exp v) ^ c))
      (-a + (a + b) * odds c (y / exp z)) z := by
  rcases eq_or_lt_of_le hy with rfl | hy
  · have ha := hreg.resolve_left (lt_irrefl _)
    subst a
    have h := (hasDerivAt_const z K).sub (hasDerivAt_id z)
    change HasDerivAt (fun v ↦ K - v) (0 - 1) z at h
    simpa only [log_exp, zero_div, zero_rpow hc.ne', odds, sub_self, zero_mul, add_zero,
      log_one, sub_zero, zero_add, mul_zero, zero_sub] using h
  · have he (v : ℝ) : log (y / exp v) = log y - v := by
      rw [log_div hy.ne' (exp_pos v).ne', log_exp]
    have hp (v : ℝ) : (y / exp v) ^ c = exp (c * (log y - v)) := by
      rw [rpow_def_of_pos (div_pos hy (exp_pos v)), he, mul_comm]
    have hl := (hasDerivAt_const z (log y)).sub (hasDerivAt_id z)
    have hb := (hl.const_mul c).exp
    have hf := (hb.const_add 1).log (add_pos_of_pos_of_nonneg one_pos (exp_pos _).le).ne'
    have h := (((hasDerivAt_const z K).sub (hasDerivAt_id z)).add
      (hl.const_mul (a - 1))).sub (hf.const_mul ((a + b) / c))
    simp only [log_exp, he, hp]
    convert h using 1
    · rfl
    · simp only [Pi.sub_apply, id_eq]
      unfold odds
      rw [← hp z]
      field_simp [hc.ne', (add_pos_of_pos_of_nonneg one_pos
        (rpow_nonneg (div_nonneg hy.le (exp_pos z).le) c)).ne']
      ring

private theorem scale_balance {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c K : ℝ} (hc : 0 < c) {v : ℝ × ℝ}
    (hv : v ∈ domain a) (hm : IsMaxOn (logLik d a b c K) (domain a) v) :
    (a + b) * (∑ i, odds c ((v.2 + d i) / v.1)) = (n : ℝ) * a := by
  let f := fun z : ℝ ↦ logLik d a b c K (exp z, v.2)
  have hs : 0 < v.1 := hv.1
  have ht : 0 ≤ v.2 := by
    by_cases ha : a = 1
    · simpa only [ha, ite_true, mem_Ici] using hv.2
    · exact (show 0 < v.2 by simpa only [ha, ite_false, mem_Ioi] using hv.2).le
  have hreg (i : Fin n) : 0 < v.2 + d i ∨ a = 1 := by
    by_cases ha : a = 1
    · exact Or.inr ha
    · have ht : 0 < v.2 := by simpa only [ha, ite_false, mem_Ioi] using hv.2
      exact Or.inl (add_pos_of_pos_of_nonneg ht (hd i))
  have hderiv : HasDerivAt f (∑ i, (-a + (a + b) *
      odds c ((v.2 + d i) / v.1))) (log v.1) := by
    have h (i : Fin n) := scale_cell_deriv (a := a) (b := b) (K := K)
      (z := log v.1) hc (add_nonneg ht (hd i)) (hreg i)
    simpa only [f, logLik, Endpoint2FellerPareto.logLik, exp_log hs] using
      HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ h i)
  have hmax : IsMaxOn f univ (log v.1) := by
    intro z _
    have hz : (exp z, v.2) ∈ domain a := ⟨exp_pos z, hv.2⟩
    change f z ≤ f (log v.1)
    have hh : logLik d a b c K (exp z, v.2) ≤ logLik d a b c K v := hm hz
    simpa only [f, exp_log hs, Prod.eta] using hh
  have hz := (hmax.isLocalMax (by simp)).hasDerivAt_eq_zero hderiv
  simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.mul_sum] at hz
  linarith

private theorem weighted_rearrangement (w r h : Fin n → ℝ) (hw : ∀ i, 0 ≤ w i)
    (horder : ∀ i j, 0 ≤ (h i - h j) * (r i - r j)) :
    (∑ i, w i * h i) * (∑ i, w i * r i) ≤
      (∑ i, w i * r i * h i) * (∑ i, w i) := by
  let k (i j : Fin n) := w i * w j * h i * (r i - r j)
  have hterm (i j : Fin n) : 0 ≤ k i j + k j i := by
    have he : k i j + k j i = (w i * w j) * ((h i - h j) * (r i - r j)) := by
      dsimp [k]
      ring
    rw [he]
    exact mul_nonneg (mul_nonneg (hw i) (hw j)) (horder i j)
  have hh : 0 ≤ ∑ i, ∑ j, (k i j + k j i) :=
    Finset.sum_nonneg fun i _ ↦ Finset.sum_nonneg fun j _ ↦ hterm i j
  have heq : (∑ i, ∑ j, k i j) =
      (∑ i, w i * r i * h i) * (∑ i, w i) -
        (∑ i, w i * h i) * (∑ i, w i * r i) := by
    calc
      _ = ∑ i, (w i * r i * h i * (∑ j, w j) - w i * h i * (∑ j, w j * r j)) := by
        apply Finset.sum_congr rfl
        intro i _
        rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_sub_distrib]
        apply Finset.sum_congr rfl
        intro j _
        dsimp [k]
        ring
      _ = _ := by rw [Finset.sum_sub_distrib, ← Finset.sum_mul, ← Finset.sum_mul]
  have hs : (∑ i, ∑ j, k j i) = ∑ i, ∑ j, k i j := Finset.sum_comm
  simp_rw [Finset.sum_add_distrib] at hh
  rw [hs, heq] at hh
  linarith

private theorem profile_odds_strict {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hne : ∃ i j, d i ≠ d j) {c s₁ s₂ t₁ t₂ : ℝ}
    (hc : 0 < c) (hs₁ : 0 < s₁) (hs₂ : 0 < s₂) (ht₁ : 0 < t₁) (ht : t₁ < t₂)
    (hbal : (∑ i, odds c ((t₁ + d i) / s₁)) = ∑ i, odds c ((t₂ + d i) / s₂)) :
    (∑ i, (t₁ + d i)⁻¹ * odds c ((t₁ + d i) / s₁)) / (∑ i, (t₁ + d i)⁻¹) <
      (∑ i, (t₂ + d i)⁻¹ * odds c ((t₂ + d i) / s₂)) / (∑ i, (t₂ + d i)⁻¹) := by
  obtain ⟨j, k, hjk⟩ := hne
  let u (i : Fin n) := odds c ((t₁ + d i) / s₁)
  let v (i : Fin n) := odds c ((t₂ + d i) / s₂)
  let w₁ (i : Fin n) := (t₁ + d i)⁻¹
  let w₂ (i : Fin n) := (t₂ + d i)⁻¹
  let r (i : Fin n) := (t₁ + d i) / (t₂ + d i)
  have hy₁ (i : Fin n) : 0 < t₁ + d i := add_pos_of_pos_of_nonneg ht₁ (hd i)
  have hy₂ (i : Fin n) : 0 < t₂ + d i := add_pos_of_pos_of_nonneg (ht₁.trans ht) (hd i)
  have hW₁ : 0 < ∑ i, w₁ i :=
    Finset.sum_pos' (fun i _ ↦ (inv_pos.mpr (hy₁ i)).le) ⟨j, Finset.mem_univ _, inv_pos.mpr (hy₁ j)⟩
  have hW₂ : 0 < ∑ i, w₂ i :=
    Finset.sum_pos' (fun i _ ↦ (inv_pos.mpr (hy₂ i)).le) ⟨j, Finset.mem_univ _, inv_pos.mpr (hy₂ j)⟩
  have hss : s₁ < s₂ := by
    by_contra! h
    have hh (i : Fin n) : u i < v i :=
      odds_strictMono hc (div_nonneg (hy₁ i).le hs₁.le) (div_nonneg (hy₂ i).le hs₂.le)
        ((div_le_div_of_nonneg_left (hy₁ i).le hs₂ h).trans_lt
          (div_lt_div_of_pos_right (by linarith) hs₂))
    exact (lt_irrefl _) (lt_of_lt_of_eq
      (Finset.sum_lt_sum_of_nonempty ⟨j, Finset.mem_univ _⟩ (fun i _ ↦ hh i)) hbal.symm)
  have hratio (i : Fin n) : w₁ i * r i = w₂ i := by
    dsimp [w₁, r, w₂]
    field_simp [(hy₁ i).ne', (hy₂ i).ne']
  have horder {i j : Fin n} (hh : d i ≤ d j) : u i ≤ u j ∧ r i ≤ r j := by
    refine ⟨(odds_strictMono hc).monotoneOn (div_nonneg (hy₁ i).le hs₁.le)
      (div_nonneg (hy₁ j).le hs₁.le) (div_le_div_of_nonneg_right (by linarith) hs₁.le), ?_⟩
    apply (div_le_div_iff₀ (hy₂ i) (hy₂ j)).mpr
    nlinarith [mul_nonneg (sub_nonneg.mpr ht.le) (sub_nonneg.mpr hh)]
  have hcmp := weighted_rearrangement w₁ r u (fun i ↦ (inv_pos.mpr (hy₁ i)).le)
    (fun i j ↦ by
      rcases le_total (d i) (d j) with hh | hh
      · exact mul_nonneg_of_nonpos_of_nonpos (sub_nonpos.mpr (horder hh).1)
          (sub_nonpos.mpr (horder hh).2)
      · exact mul_nonneg (sub_nonneg.mpr (horder hh).1) (sub_nonneg.mpr (horder hh).2))
  simp only [hratio] at hcmp
  have hpre : (∑ i, w₁ i * u i) / (∑ i, w₁ i) ≤
      (∑ i, w₂ i * u i) / (∑ i, w₂ i) := (div_le_div_iff₀ hW₁ hW₂).mpr hcmp
  let D := (s₁ * t₂ - s₂ * t₁) / (s₂ - s₁)
  let W := (t₂ + D)⁻¹
  have hss₀ := sub_pos.mpr hss
  have hD : D * (s₂ - s₁) = s₁ * t₂ - s₂ * t₁ := div_mul_cancel₀ _ hss₀.ne'
  have hD₀ : 0 < t₂ + D := by
    have he : (t₂ + D) * (s₂ - s₁) = s₂ * (t₂ - t₁) := by linarith [hD]
    have hprod : 0 < (t₂ + D) * (s₂ - s₁) := by
      rw [he]
      exact mul_pos hs₂ (sub_pos.mpr ht)
    exact pos_of_mul_pos_left hprod hss₀.le
  have hcross (i : Fin n) :
      ((t₂ + d i) / s₂ - (t₁ + d i) / s₁) * (s₂ * s₁) =
        (D - d i) * (s₂ - s₁) := by
    calc
      _ = s₁ * (t₂ + d i) - s₂ * (t₁ + d i) := by
        field_simp [hs₁.ne', hs₂.ne']
      _ = _ := by linarith [hD]
  have hpair (i : Fin n) : 0 ≤ (w₂ i - W) * (v i - u i) ∧
      (d i ≠ D → 0 < (w₂ i - W) * (v i - u i)) := by
    rcases lt_trichotomy (d i) D with h | h | h
    · have hw : W < w₂ i := (inv_lt_inv₀ hD₀ (hy₂ i)).mpr (by linarith)
      have hh := pos_of_mul_pos_left
        ((hcross i).symm ▸ mul_pos (sub_pos.mpr h) hss₀) (mul_nonneg hs₂.le hs₁.le)
      have hlt : u i < v i := odds_strictMono hc
        (div_nonneg (hy₁ i).le hs₁.le) (div_nonneg (hy₂ i).le hs₂.le) (sub_pos.mp hh)
      have hpos := mul_pos (sub_pos.mpr hw) (sub_pos.mpr hlt)
      exact ⟨hpos.le, fun _ ↦ hpos⟩
    · have he : w₂ i = W := by simp [w₂, W, h]
      simp [he, h]
    · have hw : w₂ i < W := (inv_lt_inv₀ (hy₂ i) hD₀).mpr (by linarith)
      have hh := neg_of_mul_neg_left
        ((hcross i).symm ▸ mul_neg_of_neg_of_pos (sub_neg.mpr h) hss₀) (mul_nonneg hs₂.le hs₁.le)
      have hlt : v i < u i := odds_strictMono hc
        (div_nonneg (hy₂ i).le hs₂.le) (div_nonneg (hy₁ i).le hs₁.le) (sub_neg.mp hh)
      have hpos := mul_pos_of_neg_of_neg (sub_neg.mpr hw) (sub_neg.mpr hlt)
      exact ⟨hpos.le, fun _ ↦ hpos⟩
  have hne : ∃ i, d i ≠ D := by
    by_contra! h
    exact hjk ((h j).trans (h k).symm)
  obtain ⟨i, hi⟩ := hne
  have hpos : 0 < ∑ i, (w₂ i - W) * (v i - u i) :=
    Finset.sum_pos' (fun i _ ↦ (hpair i).1) ⟨i, Finset.mem_univ _, (hpair i).2 hi⟩
  have heq (i : Fin n) : (w₂ i - W) * (v i - u i) =
      (w₂ i * v i - w₂ i * u i) - W * (v i - u i) := by ring
  change (∑ i, u i) = ∑ i, v i at hbal
  simp only [heq, Finset.sum_sub_distrib, ← Finset.mul_sum, ← hbal, sub_self, mul_zero,
    sub_zero] at hpos
  exact hpre.trans_lt (div_lt_div_of_pos_right (sub_pos.mp hpos) hW₂)

private theorem scale_unique_at_gap {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {c t s₁ s₂ : ℝ} (hc : 0 < c) (ht : 0 ≤ t)
    (hs₁ : 0 < s₁) (hs₂ : 0 < s₂) (hpos : ∃ i, 0 < t + d i)
    (hbal : (∑ i, odds c ((t + d i) / s₁)) = ∑ i, odds c ((t + d i) / s₂)) :
    s₁ = s₂ := by
  have H (s v : ℝ) (hs : 0 < s) (hv : 0 < v) (hsv : s < v) :
      (∑ i, odds c ((t + d i) / v)) < ∑ i, odds c ((t + d i) / s) := by
    obtain ⟨i, hi⟩ := hpos
    have hnon (i : Fin n) : 0 ≤ t + d i := add_nonneg ht (hd i)
    apply Finset.sum_lt_sum
    · intro j _
      exact (odds_strictMono hc).monotoneOn (div_nonneg (hnon j) hv.le)
        (div_nonneg (hnon j) hs.le) (div_le_div_of_nonneg_left (hnon j) hs hsv.le)
    · exact ⟨i, Finset.mem_univ _, odds_strictMono hc
        (div_nonneg hi.le hv.le) (div_nonneg hi.le hs.le)
        (div_lt_div_of_pos_left hi hs hsv)⟩
  rcases lt_trichotomy s₁ s₂ with h | h | h
  · exact ((H s₁ s₂ hs₁ hs₂ h).ne hbal.symm).elim
  · exact h
  · exact ((H s₂ s₁ hs₂ hs₁ h).ne hbal).elim

private theorem normalized_score_odds {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s t : ℝ} (hab : 0 < a + b) (ht : 0 < t)
    (h : Endpoint2FellerParetoRegular.normalizedScore d a b c s t = 0) :
    (∑ i, (t + d i)⁻¹ * odds c ((t + d i) / s)) / (∑ i, (t + d i)⁻¹) =
      (a - 1) / (a + b) := by
  obtain ⟨i, _⟩ := hz
  let w := fun j : Fin n ↦ (t + d j)⁻¹
  let f := fun j : Fin n ↦ odds c ((t + d j) / s)
  have hw₁ (j : Fin n) : 0 < w j := inv_pos.mpr (add_pos_of_pos_of_nonneg ht (hd j))
  have hw : 0 < ∑ j, w j :=
    Finset.sum_pos' (fun j _ ↦ (hw₁ j).le) ⟨i, Finset.mem_univ _, hw₁ i⟩
  change (∑ i, w i * (a - 1 - (a + b) * f i)) / (∑ i, w i) = 0 at h
  have hh := (div_eq_zero_iff.mp h).resolve_right hw.ne'
  have he (i : Fin n) : w i * (a - 1 - (a + b) * f i) =
      (a - 1) * w i - (a + b) * (w i * f i) := by ring
  simp only [he, Finset.sum_sub_distrib, ← Finset.mul_sum] at hh
  change (∑ i, w i * f i) / (∑ i, w i) = (a - 1) / (a + b)
  apply (div_eq_div_iff hw.ne' hab.ne').mpr
  linarith

/-- At attained fixed shapes, the joint scale and threshold are
unique. At lower power one the threshold is closed contact, and
at strictly larger lower power the threshold gap is positive. -/
theorem fixed_shapes_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c)
    (ha : 1 ≤ a) (hE : 0 < exponent d b) :
    ∃! v, v ∈ domain a ∧ IsMaxOn (logLik d a b c K) (domain a) v ∧
      (v.2 = 0 ↔ a = 1) := by
  have hab : 0 < a + b := by linarith
  have hn : 0 < n := by obtain ⟨j, _⟩ := hz; exact j.isLt.pos
  have hpos : ∃ i, 0 < d i := by
    by_contra! h
    have he (i : Fin n) : d i = 0 := le_antisymm (h i) (hd i)
    have hh : (∑ _i : Fin n, (-1 : ℝ)) < 0 := by
      simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_neg,
        mul_one, neg_lt_zero]
      exact Nat.cast_pos.mpr hn
    exact (lt_asymm (by simpa only [exponent, he, ite_true] using hE) hh)
  have hne : ∃ i j, d i ≠ d j := by
    obtain ⟨i, hi⟩ := hpos
    obtain ⟨j, hj⟩ := hz
    exact ⟨i, j, by rw [hj]; exact hi.ne'⟩
  let L := logLik d a b c K
  have hgap (v : ℝ × ℝ) (hv : v ∈ domain a) : 0 ≤ v.2 := by
    by_cases he : a = 1
    · simpa [he] using hv.2
    · exact (show 0 < v.2 by simpa [he] using hv.2).le
  have ht0 (v : ℝ × ℝ) (hv : v ∈ domain a) (hm : IsMaxOn L (domain a) v) :
      v.2 = 0 ↔ a = 1 := by
    constructor
    · intro h
      by_contra! ha'
      have ht : 0 < v.2 := by simpa [ha'] using hv.2
      exact ht.ne' h
    · intro ha'
      have he := (Endpoint3FellerPareto.fixed_regular_threshold_uniqueness hd hz hb hc hv.1
        (K := K - log v.1)).1 ha'
      apply he.2 v.2 (hgap v hv)
      intro t ht
      have hD : (v.1,t) ∈ domain a := ⟨hv.1, by simpa [ha'] using ht⟩
      have hh : L (v.1,t) ≤ L v := hm hD
      exact hh
  have hnorm (v : ℝ × ℝ) (hv : v ∈ domain a) (hm : IsMaxOn L (domain a) v)
      (ha' : 1 < a) :
      (∑ i, (v.2 + d i)⁻¹ * odds c ((v.2 + d i) / v.1)) / (∑ i, (v.2 + d i)⁻¹) =
        (a - 1) / (a + b) := by
    have ht : 0 < v.2 := by simpa [ha'.ne'] using hv.2
    apply normalized_score_odds hd hz hab ht
    apply ((Endpoint2FellerParetoRegular.interior_threshold_fit hd hz ha' hb hc hv.1
      (K := K - log v.1)).2 v.2 ht).mp
    intro t ht'
    have hD : (v.1,t) ∈ domain a := ⟨hv.1, by simpa [ha'.ne'] using ht'⟩
    have hh : L (v.1,t) ≤ L v := hm hD
    exact hh
  have hex := (fixed_shapes_criterion hd hz hb hc (K := K)).mpr ⟨ha, hE⟩
  obtain ⟨v, hv, hm⟩ := hex
  refine ⟨v, ⟨hv, hm, ht0 v hv hm⟩, ?_⟩
  rintro w ⟨hw, hwm, _⟩
  have hbal : (∑ i, odds c ((w.2 + d i) / w.1)) =
      ∑ i, odds c ((v.2 + d i) / v.1) :=
    (mul_right_inj' hab.ne').mp
      ((scale_balance hd hc hw hwm).trans (scale_balance hd hc hv hm).symm)
  have hgap_eq : w.2 = v.2 := by
    rcases eq_or_lt_of_le ha with he | he
    · exact ((ht0 w hw hwm).mpr he.symm).trans ((ht0 v hv hm).mpr he.symm).symm
    · have hW : 0 < w.2 := by simpa [he.ne'] using hw.2
      have hV : 0 < v.2 := by simpa [he.ne'] using hv.2
      rcases lt_trichotomy w.2 v.2 with h | h | h
      · have hh := profile_odds_strict hd hne hc hw.1 hv.1 hW h hbal
        rw [hnorm w hw hwm he, hnorm v hv hm he] at hh
        exact (lt_irrefl _ hh).elim
      · exact h
      · have hh := profile_odds_strict hd hne hc hv.1 hw.1 hV h hbal.symm
        rw [hnorm w hw hwm he, hnorm v hv hm he] at hh
        exact (lt_irrefl _ hh).elim
  refine Prod.ext (scale_unique_at_gap hd hc (hgap v hv) hw.1 hv.1 ?_ ?_) hgap_eq
  · exact hpos.imp fun i hi ↦ add_pos_of_nonneg_of_pos (hgap v hv) hi
  · simpa only [hgap_eq] using hbal

end UnivariateML.Endpoint3FellerParetoFreeScale

end
