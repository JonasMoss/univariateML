module

public import UnivariateML.Generic.Endpoint2BetaWidth
public import UnivariateML.Family.Transfers3

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized beta endpoint and shape transfers

The support is `(0,b]`. The upper endpoint has finite positive
density at contact exactly when the second beta shape is one.
-/

@[expose] public section

open Real Set Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2GeneralizedBeta

noncomputable section

variable {n : ℕ}

/-- Powered unit-interval coordinates of the sample. -/
def unitData (x : Fin n → ℝ) (b c : ℝ) (i : Fin n) : ℝ := (x i / b) ^ c

/-- Dossier log density summed over the sample. At upper shape one
it also evaluates the finite closed endpoint density. -/
def logLik (x : Fin n → ℝ) (b p q c : ℝ) : ℝ :=
  ∑ i, (log c - log (x i) - Beta.logBeta (p, q) + p * log (unitData x b c i) +
    (q - 1) * log (1 - unitData x b c i))

private def jacobian (x : Fin n → ℝ) (b c : ℝ) : ℝ :=
  ∑ i, (log c - log (x i) + log (unitData x b c i))

private theorem beta_transfer (x : Fin n → ℝ) (b p q c : ℝ) :
    logLik x b p q c = Beta.logLik (unitData x b c) (p, q) + jacobian x b c := by
  simp only [logLik, Beta.logLik, jacobian, Finset.sum_add_distrib,
    Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  ring

private theorem unit_mem {x : Fin n → ℝ} {b c : ℝ} (hb : 0 < b) (hc : 0 < c)
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (i : Fin n) : unitData x b c i ∈ Ioo (0 : ℝ) 1 := by
  have hp := div_pos (hx i).1 hb
  have hlt := (strictMonoOn_rpow_Ici_of_exponent_pos hc) hp.le
    (show (1 : ℝ) ∈ Ici 0 from zero_le_one) ((div_lt_one hb).mpr (hx i).2)
  exact ⟨rpow_pos_of_pos hp c, by simpa [unitData] using hlt⟩

/-- With scale, power and first beta shape fixed, the free second beta
shape always has a unique maximum on strictly interior samples. -/
theorem held_first_shape_fit (hn : 0 < n) {x : Fin n → ℝ} {b c p : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hp : 0 < p) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    ∃! q, q ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun v ↦ logLik x b p v c) (Ioi 0) q :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ))) (fun q _ ↦
    beta_transfer x b p q c)).mpr (Beta.heldFirst hn (unit_mem hb hc hx) hp)

/-- With scale, power and second beta shape fixed, the free first beta
shape always has a unique maximum on strictly interior samples. -/
theorem held_second_shape_fit (hn : 0 < n) {x : Fin n → ℝ} {b c q : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hq : 0 < q) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    ∃! p, p ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun v ↦ logLik x b v q c) (Ioi 0) p :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ))) (fun p _ ↦
    beta_transfer x b p q c)).mpr (Beta.heldSecond hn (unit_mem hb hc hx) hq)

/-- Holding scale and power gives the beta fixed-mean concentration ray
on any constant interior sample. -/
theorem held_scale_power_constant_path (hn : 0 < n) {b c v : ℝ} (hb : 0 < b) (hc : 0 < c)
    (hv : v ∈ Ioo (0 : ℝ) b) :
    let u := (v / b) ^ c
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ v) b (t * u) (t * (1 - u)) c)
      atTop atTop := by
  dsimp only
  let x : Fin n → ℝ := fun _ ↦ v
  let u := (v / b) ^ c
  have hu : u ∈ Ioo (0 : ℝ) 1 := unit_mem (x := x) hb hc (fun _ ↦ hv) ⟨0, hn⟩
  exact (tendsto_atTop_add_const_right _ (jacobian x b c) (Beta.constant_unbounded hn hu)).congr
    (fun t ↦ (beta_transfer x b (t * u) (t * (1 - u)) c).symm)

/-- With scale and power fixed, the two beta shapes have a unique global
maximum exactly on nonconstant interior data. Constants have no finite fit. -/
theorem held_scale_power_criterion (hn : 0 < n) {x : Fin n → ℝ} {b c : ℝ}
    (hb : 0 < b) (hc : 0 < c) (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) :
    let L : ℝ × ℝ → ℝ := fun p ↦ logLik x b p.1 p.2 c
    ((∃ p ∈ Beta.domain, IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j) ∧
      ((∃! p, p ∈ Beta.domain ∧ IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j) := by
  intro L
  have huniq : (∃! p, p ∈ Beta.domain ∧ IsMaxOn L Beta.domain p) ↔
      ∃ i j, x i ≠ x j := by
    rw [Transfer.existsUnique_max_iff (bijOn_id Beta.domain) (fun p _ ↦
      beta_transfer x b p.1 p.2 c)]
    rw [Beta.jointMLE_iff hn (unit_mem hb hc hx)]
    constructor
    · rintro ⟨i,j,h⟩
      exact ⟨i, j, fun he ↦ h (by simp [unitData, he])⟩
    · rintro ⟨i,j,h⟩
      refine ⟨i,j,fun he ↦ h ?_⟩
      have he' := (strictMonoOn_rpow_Ici_of_exponent_pos hc).injOn
        (div_nonneg (hx i).1.le hb.le) (div_nonneg (hx j).1.le hb.le) he
      exact (div_left_inj₀ hb.ne').mp he'
  refine ⟨⟨?_, fun h ↦ (huniq.mpr h).exists.elim fun p hp ↦ ⟨p,hp⟩⟩, huniq⟩
  rintro ⟨p, _, hm⟩
  by_contra! he
  let j : Fin n := ⟨0, hn⟩
  let u := unitData x b c j
  have hu := unit_mem hb hc hx j
  have hex : x = fun _ ↦ x j := funext fun i ↦ he i j
  have hpath := (held_scale_power_constant_path hn hb hc (hx j)).eventually_gt_atTop (L p)
  obtain ⟨t, ht, ht'⟩ := (hpath.and (eventually_gt_atTop (0 : ℝ))).exists
  have hdom : (t * u, t * (1 - u)) ∈ Beta.domain :=
    ⟨mul_pos ht' hu.1, mul_pos ht' (sub_pos.mpr hu.2)⟩
  have heq : L (t * u, t * (1 - u)) =
      logLik (fun _ : Fin n ↦ x j) b (t * u) (t * (1 - u)) c :=
    congrArg (fun y : Fin n → ℝ ↦ logLik y b (t * u) (t * (1 - u)) c) hex
  exact ht.not_ge (heq ▸ hm hdom)

private theorem endpoint_transfer {x : Fin n → ℝ} {b M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hb : 0 < b) (hM : 0 < M) :
    logLik x b p q c =
      Endpoint2BetaWidth.logLik (unitData x M c) q p ((M / b) ^ c) +
        ∑ i, (log c - log (x i) - Beta.logBeta (p, q) + p * log (unitData x M c i)) := by
  unfold logLik Endpoint2BetaWidth.logLik
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have he : unitData x b c i = (M / b) ^ c * unitData x M c i := by
    unfold unitData
    rw [← mul_rpow (div_nonneg hM.le hb.le) (div_nonneg (hx i).le hM.le)]
    congr 1
    field_simp [hM.ne', hb.ne']
  have hu : 0 < unitData x M c i := rpow_pos_of_pos (div_pos (hx i) hM) c
  rw [he, log_mul (rpow_pos_of_pos (div_pos hM hb) c).ne' hu.ne']
  ring

private theorem unit_chart {x : Fin n → ℝ} {M c : ℝ} (hM : 0 < M) (hc : 0 < c)
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) :
    (∀ i, unitData x M c i ∈ Icc (0 : ℝ) 1) ∧ ∃ i, unitData x M c i = 1 := by
  refine ⟨fun i ↦ ⟨rpow_nonneg (div_nonneg (hx i).le hM.le) _, ?_⟩, ?_⟩
  · have h := (strictMonoOn_rpow_Ici_of_exponent_pos hc).monotoneOn
      (div_nonneg (hx i).le hM.le) (show (1 : ℝ) ∈ Ici 0 from zero_le_one)
      ((div_le_one hM).mpr (hle i))
    simpa [unitData] using h
  · exact heq.imp (fun i hi ↦ by simp [unitData, hi, hM.ne'])

private theorem chart_on_path {M c t : ℝ} (hM : 0 < M) (hc : 0 < c) (ht : 0 < t) :
    (M / (M / t ^ c⁻¹)) ^ c = t := by
  rw [div_div_cancel₀ hM.ne', ← rpow_mul ht.le, inv_mul_cancel₀ hc.ne', rpow_one]

private theorem endpoint_bij {M c : ℝ} (hM : 0 < M) (hc : 0 < c) :
    BijOn (fun b : ℝ ↦ (M / b) ^ c) (Ioi M) (Ioo 0 1) := by
  have hmono := strictMonoOn_rpow_Ici_of_exponent_pos hc
  refine ⟨?_, ?_, ?_⟩
  · intro b hb
    have hp := div_pos hM (hM.trans hb)
    refine ⟨rpow_pos_of_pos hp _, ?_⟩
    simpa using hmono hp.le (show (1 : ℝ) ∈ Ici 0 from zero_le_one)
      ((div_lt_one (hM.trans hb)).mpr hb)
  · intro b hb b' hb' he
    have hr := hmono.injOn (div_nonneg hM.le (hM.trans hb).le)
      (div_nonneg hM.le (hM.trans hb').le) he
    rw [div_eq_mul_inv, div_eq_mul_inv] at hr
    exact inv_injective (mul_left_cancel₀ hM.ne' hr)
  · intro u hu
    have hp := rpow_pos_of_pos hu.1 c⁻¹
    have hlt : u ^ c⁻¹ < 1 := by
      simpa using (strictMonoOn_rpow_Ici_of_exponent_pos (inv_pos.mpr hc))
        hu.1.le (show (1 : ℝ) ∈ Ici 0 from zero_le_one) hu.2
    refine ⟨M / u ^ c⁻¹, ?_, chart_on_path hM hc hu.1⟩
    apply (lt_div_iff₀ hp).mpr
    nlinarith [mul_lt_mul_of_pos_left hlt hM]

/-- A subunit upper beta shape gives a free-endpoint spike on every
positive sample, with all tied maxima included. -/
theorem endpoint_spike {x : Fin n → ℝ} {M p q c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hq : q < 1) (hc : 0 < c) :
    Tendsto (fun t ↦ logLik x (M / (1 - t) ^ c⁻¹) p q c) (𝓝[>] 0) atTop := by
  have hM : 0 < M := by obtain ⟨i, hi⟩ := heq; exact hi ▸ hx i
  obtain ⟨hy, hy1⟩ := unit_chart hM hc hx hle heq
  let K := ∑ i, (log c - log (x i) - Beta.logBeta (p, q) + p * log (unitData x M c i))
  apply (tendsto_atTop_add_const_right _ K (Endpoint2BetaWidth.spike hy hy1 hq p)).congr'
  filter_upwards [mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with t ht
  have htp : 0 < 1 - t := sub_pos.mpr ht
  rw [endpoint_transfer hx (div_pos hM (rpow_pos_of_pos htp _)) hM, chart_on_path hM hc htp]

/-- A positive allowed first shape and power suffice for a divergent
free-scale path whenever the second shape is free. The sets encode
arbitrary holds on the first shape or power. -/
theorem free_second_shape_unbounded {x : Fin n → ℝ} {M p c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hp : 0 < p) (hc : 0 < c)
    {A C : Set ℝ} (hA : p ∈ A) (hC : c ∈ C) :
    let D : Set (ℝ × ℝ × ℝ × ℝ) := {v | (M < v.1 ∨ (v.1 = M ∧ v.2.2.1 = 1)) ∧
      0 < v.2.1 ∧ v.2.1 ∈ A ∧ 0 < v.2.2.1 ∧ 0 < v.2.2.2 ∧ v.2.2.2 ∈ C}
    let L : ℝ × ℝ × ℝ × ℝ → ℝ := fun v ↦ logLik x v.1 v.2.1 v.2.2.1 v.2.2.2
    ¬ BddAbove (L '' D) ∧ ¬ ∃ v ∈ D, IsMaxOn L D v := by
  intro D L
  have hu : ¬ BddAbove (L '' D) := by
    rintro ⟨B, hB⟩
    have hM : 0 < M := by obtain ⟨i, hi⟩ := heq; exact hi ▸ hx i
    have hq : (1 / 2 : ℝ) < 1 := by norm_num
    have hpath := (endpoint_spike (p := p) hx hle heq hq hc).eventually_gt_atTop B
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 1 - t ∈ Ioo (0 : ℝ) 1 := by
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
        mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with t ht ht'
      exact ⟨sub_pos.mpr ht', sub_lt_self _ ht⟩
    obtain ⟨t, hgt, ht⟩ := (hpath.and hnear).exists
    let b := M / (1 - t) ^ c⁻¹
    have hb : M < b := by
      have hlt : (1 - t) ^ c⁻¹ < 1 := by
        simpa using (strictMonoOn_rpow_Ici_of_exponent_pos (inv_pos.mpr hc))
          ht.1.le (show (1 : ℝ) ∈ Ici 0 from zero_le_one) ht.2
      exact (lt_div_iff₀ (rpow_pos_of_pos ht.1 _)).mpr
        (by nlinarith [mul_lt_mul_of_pos_left hlt hM])
    have hd : (b,p,1/2,c) ∈ D := ⟨Or.inl hb, hp, hA, by norm_num, hc, hC⟩
    exact hgt.not_ge (hB ⟨(b,p,1/2,c), hd, rfl⟩)
  exact ⟨hu, by rintro ⟨v, _, hm⟩; exact hu hm.bddAbove⟩


/-- When only the endpoint is free, a finite optimum exists exactly at
upper shape at least one. At one it is closed contact. Above one the
chart score characterizes the unique interior optimum. -/
theorem fixed_shapes_endpoint_fit (hn : 0 < n) {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) :
    let D := if q = 1 then Ici M else Ioi M
    let L := fun b ↦ logLik x b p q c
    ((∃ b ∈ D, IsMaxOn L D b) ↔ 1 ≤ q) ∧
    (q = 1 → IsMaxOn L D M ∧ ∀ b ∈ D, IsMaxOn L D b → b = M) ∧
    (1 < q → ∃! b, b ∈ D ∧ IsMaxOn L D b) := by
  have hM : 0 < M := by obtain ⟨i, hi⟩ := heq; exact hi ▸ hx i
  obtain ⟨hy, hy1⟩ := unit_chart hM hc hx hle heq
  have hsub (hq : q < 1) :
      ¬ ∃ b ∈ Ioi M, IsMaxOn (fun b ↦ logLik x b p q c) (Ioi M) b := by
    rintro ⟨b, _, hmax⟩
    have hlarge := (endpoint_spike (p := p) hx hle heq hq hc).eventually_gt_atTop (logLik x b p q c)
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 1 - t ∈ Ioo (0 : ℝ) 1 := by
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
        mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with t ht ht'
      exact ⟨sub_pos.mpr ht', sub_lt_self _ ht⟩
    obtain ⟨t, hgt, ht⟩ := (hlarge.and hnear).exists
    have hmem : M / (1 - t) ^ c⁻¹ ∈ Ioi M := by
      have hr := rpow_pos_of_pos ht.1 c⁻¹
      have hlt : (1 - t) ^ c⁻¹ < 1 := by
        simpa using (strictMonoOn_rpow_Ici_of_exponent_pos (inv_pos.mpr hc))
          ht.1.le (show (1 : ℝ) ∈ Ici 0 from zero_le_one) ht.2
      exact (lt_div_iff₀ hr).mpr (by nlinarith [mul_lt_mul_of_pos_left hlt hM])
    exact hgt.not_ge (hmax hmem)
  rcases lt_trichotomy q 1 with hq | rfl | hq
  · simp only [hq.ne, ite_false, hq.not_ge, iff_false, false_implies, true_and]
    exact ⟨hsub hq, fun he ↦ (hq.not_ge he.le).elim⟩
  · simp only [ite_true, le_refl, true_implies, lt_irrefl, false_implies, and_true, iff_true]
    have hM' : M ∈ Ici M := le_rfl
    have hstrict (b : ℝ) (hb : M < b) : logLik x b p 1 c < logLik x M p 1 c := by
      have hu := (endpoint_bij hM hc).mapsTo hb
      have he (u : ℝ) : Endpoint2BetaWidth.logLik (unitData x M c) 1 p u =
          (n : ℝ) * p * log u := by simp [Endpoint2BetaWidth.logLik, nsmul_eq_mul, mul_assoc]
      rw [endpoint_transfer hx (hM.trans hb) hM, endpoint_transfer hx hM hM,
        div_self hM.ne', one_rpow, he, he, log_one, mul_zero]
      apply add_lt_add_left
      exact mul_neg_of_pos_of_neg (mul_pos (Nat.cast_pos.mpr hn) hp) (log_neg hu.1 hu.2)
    have hbest : IsMaxOn (fun b ↦ logLik x b p 1 c) (Ici M) M := by
      intro b hb
      rcases eq_or_lt_of_le hb with rfl | hlt
      · exact le_rfl
      · exact (hstrict b hlt).le
    exact ⟨⟨M, hM', hbest⟩, hbest, fun b hb h ↦ by
      by_contra! hne
      exact (hstrict b (lt_of_le_of_ne hb (Ne.symm hne))).not_ge (h hM')⟩
  · have hne : q ≠ 1 := hq.ne'
    simp only [hne, ite_false, false_implies, true_and, hq.le, iff_true]
    let K := ∑ i, (log c - log (x i) - Beta.logBeta (p, q) + p * log (unitData x M c i))
    have he (b : ℝ) (hb : b ∈ Ioi M) :
        logLik x b p q c =
          Endpoint2BetaWidth.logLik (unitData x M c) q p ((M / b) ^ c) + K :=
      endpoint_transfer hx (hM.trans hb) hM
    obtain ⟨u, ⟨hu, hs⟩, huq⟩ := (Endpoint2BetaWidth.interior_max hn hy hy1 hq hp).1
    have uniq : ∃! u, u ∈ Ioo (0 : ℝ) 1 ∧
        IsMaxOn (Endpoint2BetaWidth.logLik (unitData x M c) q p) (Ioo 0 1) u := by
      have hscore := (Endpoint2BetaWidth.interior_max hn hy hy1 hq hp).2
      exact ⟨u, ⟨hu, (hscore _ hu).mpr hs⟩, fun v hv ↦ huq v ⟨hv.1, (hscore _ hv.1).mp hv.2⟩⟩
    have huniq := (Transfer.existsUnique_max_iff (endpoint_bij hM hc) he).mpr uniq
    exact ⟨huniq.exists.elim fun b hb ↦ ⟨b,hb⟩, fun _ ↦ huniq⟩

/-- At fixed upper shape above one, the unique endpoint optimum is the
interior zero of the transformed score. -/
theorem fixed_shapes_endpoint_score (hn : 0 < n) {x : Fin n → ℝ} {M p q c b : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hq : 1 < q) (hc : 0 < c) (hb : M < b) :
    IsMaxOn (fun s ↦ logLik x s p q c) (Ioi M) b ↔
      Endpoint2BetaWidth.score (unitData x M c) q p ((M / b) ^ c) = 0 := by
  have hM : 0 < M := by obtain ⟨i, hi⟩ := heq; exact hi ▸ hx i
  obtain ⟨hy, hy1⟩ := unit_chart hM hc hx hle heq
  have hbij := endpoint_bij hM hc
  have htransfer := Transfer.isMaxOn_iff hbij
    (fun s h ↦ endpoint_transfer (c := c) (p := p) (q := q) hx (hM.trans h) hM) hb
  exact htransfer.trans ((Endpoint2BetaWidth.interior_max hn hy hy1 hq hp).2 _ (hbij.mapsTo hb))

/-- Sum of log distances below the endpoint. -/
def logDistanceSum (x : Fin n → ℝ) (b : ℝ) : ℝ :=
  ∑ i, log (b / x i)

private theorem logBeta_one {p : ℝ} (hp : 0 < p) :
    Beta.logBeta (p, 1) = -log p := by
  unfold Beta.logBeta Beta.normalizer
  change log (Gamma p * Gamma 1 / Gamma (p + 1)) = -log p
  rw [Gamma_one, mul_one, Gamma_add_one hp.ne', log_div (Gamma_pos_of_pos hp).ne'
    (mul_pos hp (Gamma_pos_of_pos hp)).ne', log_mul hp.ne' (Gamma_pos_of_pos hp).ne']
  ring

private theorem ridge_formula {x : Fin n → ℝ} {b p c : ℝ}
    (hx : ∀ i, 0 < x i) (hb : 0 < b) (hp : 0 < p) (hc : 0 < c) :
    logLik x b p 1 c =
      (n : ℝ) * log (p * c) - (p * c) * logDistanceSum x b - ∑ i, log (x i) := by
  have h (i : Fin n) :
      log c - log (x i) - Beta.logBeta (p, 1) + p * log (unitData x b c i) +
          (1 - 1) * log (1 - unitData x b c i) =
        log (p * c) - (p * c) * log (b / x i) - log (x i) := by
    rw [logBeta_one hp, unitData, log_rpow (div_pos (hx i) hb) c,
      log_mul hp.ne' hc.ne', log_div hb.ne' (hx i).ne', log_div (hx i).ne' hb.ne']
    ring
  simp only [logLik, h, logDistanceSum, Finset.sum_sub_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

private theorem rate_strict_max (hn : 0 < n) {D r : ℝ} (hD : 0 < D) (hr : 0 < r)
    (hne : r ≠ (n : ℝ) / D) :
    (n : ℝ) * log r - r * D < (n : ℝ) * log ((n : ℝ) / D) - ((n : ℝ) / D) * D := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  let R : ℝ := (n : ℝ) / D
  have hR : 0 < R := div_pos hnR hD
  have hRD : R * D = n := div_mul_cancel₀ _ hD.ne'
  have hne' : r / R ≠ 1 := by
    intro he
    exact hne (by simpa using (div_eq_iff hR.ne').mp he)
  have hlog := mul_lt_mul_of_pos_left (log_lt_sub_one_of_pos (div_pos hr hR) hne') hnR
  rw [log_div hr.ne' hR.ne'] at hlog
  have hmul : (n : ℝ) * (r / R - 1) = r * D - R * D := by
    rw [← hRD]
    field_simp [hR.ne']
  rw [hmul] at hlog
  change (n : ℝ) * log r - r * D < (n : ℝ) * log R - R * D
  linarith

private theorem positive_distance_sum {x : Fin n → ℝ} {M : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (hne : ∃ i, x i < M) :
    0 < logDistanceSum x M := by
  obtain ⟨j, hj⟩ := hne
  apply Finset.sum_pos'
  · exact fun i _ ↦ log_nonneg ((one_le_div (hx i)).mpr (hle i))
  · exact ⟨j, Finset.mem_univ _, log_pos ((one_lt_div (hx j)).mpr hj)⟩

/-- At held scale and upper shape one, the full maximizing set is the
product ridge. Constant interior samples satisfy the same formula. -/
theorem held_scale_ridge (hn : 0 < n) {x : Fin n → ℝ} {b : ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (p : ℝ × ℝ) (hp : p ∈ Beta.domain) :
    IsMaxOn (fun u : ℝ × ℝ ↦ logLik x b u.1 1 u.2) Beta.domain p ↔
      p.1 * p.2 = (n : ℝ) / logDistanceSum x b := by
  let D := logDistanceSum x b
  let R := (n : ℝ) / D
  have hb := (hx ⟨0, hn⟩).1.trans (hx ⟨0, hn⟩).2
  have hD : 0 < D := positive_distance_sum (fun i ↦ (hx i).1) (fun i ↦ (hx i).2.le)
    ⟨⟨0, hn⟩, (hx ⟨0, hn⟩).2⟩
  have hR : 0 < R := div_pos (Nat.cast_pos.mpr hn) hD
  have hform (u : ℝ × ℝ) (hu : u ∈ Beta.domain) :
      logLik x b u.1 1 u.2 = (n : ℝ) * log (u.1 * u.2) - (u.1 * u.2) * D - ∑ i, log (x i) :=
    ridge_formula (fun i ↦ (hx i).1) hb hu.1 hu.2
  have hcmp (u : ℝ × ℝ) (hu : u ∈ Beta.domain) (hne : u.1 * u.2 ≠ R) :
      logLik x b u.1 1 u.2 < logLik x b 1 1 R := by
    rw [hform u hu, hform (1,R) ⟨one_pos,hR⟩, one_mul]
    exact sub_lt_sub_right (rate_strict_max hn hD (mul_pos hu.1 hu.2) hne) _
  constructor
  · intro hm
    by_contra! hne
    exact (hcmp p hp hne).not_ge (hm (show (1,R) ∈ Beta.domain from ⟨one_pos,hR⟩))
  · intro he
    change p.1 * p.2 = R at he
    have hpe : logLik x b p.1 1 p.2 = logLik x b 1 1 R := by
      rw [hform p hp, hform (1,R) ⟨one_pos,hR⟩, one_mul, he]
    intro u hu
    change logLik x b u.1 1 u.2 ≤ logLik x b p.1 1 p.2
    rw [hpe]
    by_cases h : u.1 * u.2 = R
    · rw [hform u hu, hform (1,R) ⟨one_pos,hR⟩, one_mul, h]
    · exact (hcmp u hu h).le

/-- With free scale and upper shape one, nonconstant data have the
complete maximizing ridge at the closed sample maximum. -/
theorem free_endpoint_ridge (hn : 0 < n) {x : Fin n → ℝ} {M : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (hne : ∃ i, x i < M)
    (u : ℝ × ℝ × ℝ) (hu : u ∈ Ici M ×ˢ Beta.domain) :
    IsMaxOn (fun v : ℝ × ℝ × ℝ ↦ logLik x v.1 v.2.1 1 v.2.2)
        (Ici M ×ˢ Beta.domain) u ↔
      u.1 = M ∧ u.2.1 * u.2.2 = (n : ℝ) / logDistanceSum x M := by
  let L := fun v : ℝ × ℝ × ℝ ↦ logLik x v.1 v.2.1 1 v.2.2
  let P := Ici M ×ˢ Beta.domain
  let D := logDistanceSum x M
  let R := (n : ℝ) / D
  have hM : 0 < M := (hx ⟨0, hn⟩).trans_le (hle ⟨0, hn⟩)
  have hD : 0 < D := positive_distance_sum hx hle hne
  have hR : 0 < R := div_pos (Nat.cast_pos.mpr hn) hD
  have hm : (M, 1, R) ∈ P := ⟨le_rfl, one_pos, hR⟩
  have hform (b p c : ℝ) (hb : M ≤ b) (hp : 0 < p) (hc : 0 < c) :
      L (b,p,c) = (n : ℝ) * log (p * c) - (p * c) * logDistanceSum x b - ∑ i, log (x i) :=
    ridge_formula hx (hM.trans_le hb) hp hc
  have hscale (b p c : ℝ) (hb : M < b) (hp : 0 < p) (hc : 0 < c) :
      L (b,p,c) < L (M,p,c) := by
    rw [hform b p c hb.le hp hc, hform M p c le_rfl hp hc]
    have hh : logDistanceSum x M < logDistanceSum x b := by
      let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
      apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
      intro i _
      exact log_lt_log (div_pos hM (hx i)) (div_lt_div_of_pos_right hb (hx i))
    linarith [mul_lt_mul_of_pos_left hh (mul_pos hp hc)]
  have hcontact (v : ℝ × ℝ × ℝ) (hv : v ∈ P) : L v ≤ L (M,v.2) := by
    obtain ⟨b,p,c⟩ := v
    rcases eq_or_lt_of_le (show M ≤ b from hv.1) with rfl | hb
    · rfl
    · exact (hscale b p c hb hv.2.1 hv.2.2).le
  have hrate (p c : ℝ) (hp : 0 < p) (hc : 0 < c) :
      L (M,p,c) ≤ L (M,1,R) ∧ (p * c ≠ R → L (M,p,c) < L (M,1,R)) := by
    rw [hform M p c le_rfl hp hc, hform M 1 R le_rfl one_pos hR, one_mul]
    by_cases h : p * c = R
    · simp [h]
    · have hl := sub_lt_sub_right (rate_strict_max hn hD (mul_pos hp hc) h)
        (∑ i, log (x i))
      exact ⟨hl.le, fun _ ↦ hl⟩
  change IsMaxOn L P u ↔ u.1 = M ∧ u.2.1 * u.2.2 = R
  constructor
  · intro h
    have he : u.1 = M := by
      by_contra! hne'
      have hu' : M < u.1 := lt_of_le_of_ne hu.1 (Ne.symm hne')
      exact (hscale u.1 u.2.1 u.2.2 hu' hu.2.1 hu.2.2).not_ge
        (h (show (M,u.2) ∈ P from ⟨le_rfl, hu.2⟩))
    refine ⟨he, ?_⟩
    by_contra! hne'
    have hl := (hrate u.2.1 u.2.2 hu.2.1 hu.2.2).2 hne'
    exact hl.not_ge (by
      have hh : L (M, 1, R) ≤ L u := h hm
      simpa [L, he] using hh)
  · rintro ⟨he, hr⟩ v hv
    have heq : L u = L (M, 1, R) := by
      change L (u.1,u.2.1,u.2.2) = _
      rw [he, hform M _ _ le_rfl hu.2.1 hu.2.2, hform M 1 R le_rfl one_pos hR, one_mul, hr]
    exact (hcontact v hv).trans ((hrate v.2.1 v.2.2 hv.2.1 hv.2.2).1.trans heq.ge)

/-- The binary sample at held scale one realizes the specified exact
product ridge. -/
theorem held_scale_two_point_ridge (p : ℝ × ℝ) (hp : p ∈ Beta.domain) :
    let x : Fin 2 → ℝ := ![1/2, 1/4]
    IsMaxOn (fun u : ℝ × ℝ ↦ logLik x 1 u.1 1 u.2) Beta.domain p ↔
      p.1 * p.2 = 2 / (3 * log 2) := by
  intro x
  have hx (i : Fin 2) : x i ∈ Ioo (0 : ℝ) 1 := by
    fin_cases i <;> norm_num [x]
  have h : logDistanceSum x 1 = 3 * log 2 := by
    simp only [logDistanceSum, Fin.sum_univ_two, x, Matrix.cons_val_zero,
      Matrix.cons_val_one]
    norm_num
    rw [show (4 : ℝ) = 2 * 2 by norm_num, log_mul (by norm_num) (by norm_num)]
    ring
  simpa only [h, Nat.cast_ofNat] using held_scale_ridge (by norm_num : 0 < (2 : ℕ)) hx p hp

/-- The binary sample whose maximum is one realizes the specified exact
closed-contact ridge with the endpoint free. -/
theorem free_endpoint_two_point_ridge (u : ℝ × ℝ × ℝ)
    (hu : u ∈ Ici (1 : ℝ) ×ˢ Beta.domain) :
    let x : Fin 2 → ℝ := ![1/2, 1]
    IsMaxOn (fun v : ℝ × ℝ × ℝ ↦ logLik x v.1 v.2.1 1 v.2.2)
        (Ici (1 : ℝ) ×ˢ Beta.domain) u ↔
      u.1 = 1 ∧ u.2.1 * u.2.2 = 2 / log 2 := by
  intro x
  have hx (i : Fin 2) : 0 < x i := by fin_cases i <;> norm_num [x]
  have hle (i : Fin 2) : x i ≤ 1 := by fin_cases i <;> norm_num [x]
  have hne : ∃ i, x i < 1 := ⟨0, by norm_num [x]⟩
  have h : logDistanceSum x 1 = log 2 := by
    norm_num [logDistanceSum, Fin.sum_univ_two, x]
    rw [log_div (by norm_num) (by norm_num), log_one]
    ring
  simpa only [h, Nat.cast_ofNat] using
    free_endpoint_ridge (by norm_num : 0 < (2 : ℕ)) hx hle hne u hu

/-- With fixed beta shapes and free power, a constant sample collapses
at a shrinking positive log distance from the free endpoint. -/
theorem constant_sample_free_power_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) (p q : ℝ) :
    Tendsto (fun c ↦ logLik (fun _ : Fin n ↦ v) (v * exp c⁻¹) p q c) atTop atTop := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  let K := (n : ℝ) *
    (-log v - Beta.logBeta (p,q) - p + (q - 1) * log (1 - exp (-1)))
  apply Tendsto.congr' _
    ((tendsto_log_atTop.const_mul_atTop hnR).atTop_add (tendsto_const_nhds (x := K)))
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
  have hb : 0 < v * exp c⁻¹ := mul_pos hv (exp_pos _)
  have he : (v / (v * exp c⁻¹)) ^ c = exp (-1) := by
    have hr := div_pos hv hb
    rw [← exp_log (rpow_pos_of_pos hr c), log_rpow hr, log_div hv.ne' hb.ne',
      log_mul hv.ne' (exp_pos _).ne', log_exp]
    congr 1
    have hm := mul_inv_cancel₀ hc.ne'
    linarith [show c * (log v - (log v + c⁻¹)) = -(c * c⁻¹) by ring]
  simp [logLik, unitData, he, K, nsmul_eq_mul]
  ring

/-- At free-scale contact, upper shape one permits arbitrary concentration
on a constant sample with either held positive power or held first shape
through the corresponding positive product. -/
theorem constant_sample_contact_rate_path (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    Tendsto (fun r ↦ logLik (fun _ : Fin n ↦ v) v r 1 1) atTop atTop := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  apply Tendsto.congr' _
    ((tendsto_log_atTop.const_mul_atTop hnR).atTop_add
      (tendsto_const_nhds (x := -(n : ℝ) * log v)))
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
  rw [ridge_formula (fun _ ↦ hv) hv hr one_pos]
  simp [logDistanceSum, div_self hv.ne', nsmul_eq_mul]
  ring

end

end UnivariateML.Endpoint2GeneralizedBeta

end
