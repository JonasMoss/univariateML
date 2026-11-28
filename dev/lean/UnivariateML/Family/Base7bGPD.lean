module

public import UnivariateML.Family.Base7GPD
import Mathlib.MeasureTheory.Integral.IntervalIntegral.FundThmCalculus
import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog

set_option linter.style.header false

/-! Uniqueness with fixed GPD scale.

The proof compares integrated secants using the same kernel ordering as
the derivative test, including secants that cross the exponential shape.
-/

@[expose] public section

namespace UnivariateML.Base7bGPD

open Filter Set Real GPD LocScaleGPD
open scoped Interval Topology

private theorem support_between {z a t : ℝ} (ht : t ∈ Icc 0 z)
    (ha : 0 < 1 + a * z) : 0 < 1 + a * t := by
  rcases le_total 0 a with h | h
  · linarith [mul_nonneg h ht.1]
  · linarith [mul_le_mul_of_nonpos_left ht.2 h]

private theorem logRatio_eq_integral {z a : ℝ} (hz : 0 ≤ z)
    (ha : 0 < 1 + a * z) : logRatio z a = ∫ t in (0 : ℝ)..z, (1 + a * t)⁻¹ := by
  have hderiv (t : ℝ) (ht : t ∈ Icc 0 z) :
      HasDerivAt (fun u ↦ logRatio u a) (1 + a * t)⁻¹ t := by
    by_cases he : a = 0
    · simpa [he, logRatio] using hasDerivAt_id' t
    · have H := (((hasDerivAt_id t).const_mul a).const_add 1).log
        (show 1 + a * t ≠ 0 from (support_between ht ha).ne')
      convert H.div_const a using 1
      · simp [logRatio, he]
      · simp only [id_eq]
        field_simp [he, (support_between ht ha).ne']
  have hcont : ContinuousOn (fun t : ℝ ↦ (1 + a * t)⁻¹) (Icc 0 z) :=
    (by fun_prop : Continuous fun t : ℝ ↦ 1 + a * t).continuousOn.inv₀
      (fun _ ht ↦ (support_between ht ha).ne')
  rw [intervalIntegral.integral_eq_sub_of_hasDerivAt_of_le hz
    (fun t ht ↦ (hderiv t ht).continuousAt.continuousWithinAt)
    (fun t ht ↦ hderiv t ⟨ht.1.le, ht.2.le⟩)
    (hcont.intervalIntegrable_of_Icc hz)]
  simp [logRatio]

private noncomputable def kernel (a b t : ℝ) : ℝ := (t - 1) / ((1 + a * t) * (1 + b * t))

private theorem kernel_cont {a b z : ℝ}
    (ha : 0 < 1 + a * z) (hb : 0 < 1 + b * z) :
    ContinuousOn (kernel a b) (Icc 0 z) := by
  unfold kernel
  exact (by fun_prop : Continuous fun t : ℝ ↦ t - 1).continuousOn.div
    (by fun_prop) (fun _ ht ↦ (mul_pos (support_between ht ha) (support_between ht hb)).ne')

private theorem density_difference {s y a b : ℝ} (hy : 0 ≤ y / s)
    (ha : 0 < 1 + a * (y / s)) (hb : 0 < 1 + b * (y / s)) :
    logDensity s b y - logDensity s a y =
      (b - a) * ∫ t in (0 : ℝ)..(y / s), kernel a b t := by
  have hc (c : ℝ) (hc : 0 < 1 + c * (y / s)) :
      IntervalIntegrable (fun t : ℝ ↦ (1 + c * t)⁻¹) MeasureTheory.volume 0 (y / s) :=
    ((by fun_prop : Continuous fun t : ℝ ↦ 1 + c * t).continuousOn.inv₀
      (fun _ ht ↦ (support_between ht hc).ne')).intervalIntegrable_of_Icc hy
  have he :
      (1 + a) * (∫ t in (0 : ℝ)..(y / s), (1 + a * t)⁻¹) -
        (1 + b) * (∫ t in (0 : ℝ)..(y / s), (1 + b * t)⁻¹) =
      (b - a) * ∫ t in (0 : ℝ)..(y / s), kernel a b t := by
    simp_rw [← intervalIntegral.integral_const_mul]
    rw [← intervalIntegral.integral_sub ((hc a ha).const_mul _) ((hc b hb).const_mul _)]
    apply intervalIntegral.integral_congr
    rw [uIcc_of_le hy]
    intro t ht
    have hta := (support_between ht ha).ne'
    have htb := (support_between ht hb).ne'
    unfold kernel
    dsimp only
    rw [← div_eq_mul_inv, ← div_eq_mul_inv, div_sub_div _ _ hta htb]
    ring
  unfold logDensity
  rw [logRatio_eq_integral hy ha, logRatio_eq_integral hy hb, ← he]
  ring

private theorem kernel_comparison {a b c z : ℝ} (hac : a < c) (hz : 0 ≤ z)
    (ha : 0 < 1 + a * z) (hb : 0 < 1 + b * z) (hc : 0 < 1 + c * z)
    (hc1 : 0 < 1 + c) :
    let q := (1 + a) / (1 + c)
    (∫ t in (0 : ℝ)..z, kernel b c t) ≤ q * (∫ t in (0 : ℝ)..z, kernel a b t) ∧
    (1 < z → (∫ t in (0 : ℝ)..z, kernel b c t) <
      q * (∫ t in (0 : ℝ)..z, kernel a b t)) := by
  intro q
  let f := kernel b c
  let g := fun t ↦ q * kernel a b t
  have hf : ContinuousOn f (Icc 0 z) := kernel_cont hb hc
  have hg : ContinuousOn g (Icc 0 z) := continuousOn_const.mul (kernel_cont ha hb)
  have hdiff (t : ℝ) (ht : t ∈ Icc 0 z) :
      g t - f t = (c - a) * (t - 1) ^ 2 /
        ((1 + c) * (1 + a * t) * (1 + b * t) * (1 + c * t)) := by
    have hta := (support_between ht ha).ne'
    have htb := (support_between ht hb).ne'
    have htc := (support_between ht hc).ne'
    dsimp [g, f, q, kernel]
    field_simp [hta, htb, htc, hc1.ne']
    ring
  have hden (t : ℝ) (ht : t ∈ Icc 0 z) :
      0 < (1 + c) * (1 + a * t) * (1 + b * t) * (1 + c * t) :=
    mul_pos (mul_pos (mul_pos hc1 (support_between ht ha)) (support_between ht hb))
      (support_between ht hc)
  rw [← intervalIntegral.integral_const_mul]
  constructor
  · apply intervalIntegral.integral_mono_on hz (hf.intervalIntegrable_of_Icc hz)
      (hg.intervalIntegrable_of_Icc hz)
    intro t ht
    apply sub_nonneg.mp
    rw [hdiff t ht]
    exact div_nonneg (mul_nonneg (sub_nonneg.mpr hac.le) (sq_nonneg _)) (hden t ht).le
  · intro h
    apply intervalIntegral.integral_lt_integral_of_continuousOn_of_le_of_exists_lt
      (by linarith : 0 < z) hf hg
    · intro t ht
      have ht' : t ∈ Icc 0 z := ⟨ht.1.le, ht.2⟩
      apply sub_nonneg.mp
      rw [hdiff t ht']
      exact div_nonneg (mul_nonneg (sub_nonneg.mpr hac.le) (sq_nonneg _)) (hden t ht').le
    · have hmem : (0 : ℝ) ∈ Icc 0 z := ⟨le_rfl, hz⟩
      refine ⟨0, hmem, sub_pos.mp ?_⟩
      rw [hdiff 0 hmem]
      exact div_pos (mul_pos (sub_pos.mpr hac) (by norm_num)) (hden 0 hmem)

private theorem normalized_support {s y a : ℝ} (hs : 0 < s) (h : 0 < s + a * y) :
    0 < 1 + a * (y / s) := by
  rw [show 1 + a * (y / s) = (s + a * y) / s by field_simp]
  exact div_pos h hs

private theorem nonuniform_at_most_one {n : ℕ} (y : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {j : Fin n} (hj : s < y j) {a c : ℝ}
    (ha : a ∈ shapeDomain y s) (hc : c ∈ shapeDomain y s)
    (hma : IsMaxOn (logLik y s) (shapeDomain y s) a)
    (hmc : IsMaxOn (logLik y s) (shapeDomain y s) c) : a = c := by
  have hnu {v : ℝ} (hv : v ∈ shapeDomain y s) : v ≠ -1 := by
    intro e
    have H := (hv.2.2 j).2
    rw [e] at H
    simp at H
    linarith
  have support {v : ℝ} (hv : v ∈ shapeDomain y s) (i : Fin n) :
      0 < 1 + v * (y i / s) := normalized_support hs (by
    simpa only [ite_eq_right (hnu hv)] using (hv.2.2 i).2)
  have hz (i : Fin n) : 0 ≤ y i / s := div_nonneg (ha.2.2 i).1 hs.le
  suffices no_order : ∀ {u v : ℝ}, u ∈ shapeDomain y s → v ∈ shapeDomain y s →
      IsMaxOn (logLik y s) (shapeDomain y s) u →
      IsMaxOn (logLik y s) (shapeDomain y s) v → ¬ u < v by
    exact le_antisymm (le_of_not_gt (no_order hc ha hmc hma))
      (le_of_not_gt (no_order ha hc hma hmc))
  intro u v hu hv hmu hmv huv
  let b := (u + v) / 2
  have hub : u < b := by dsimp [b]; linarith
  have hbv : b < v := by dsimp [b]; linarith
  have hu1 : -1 < u := lt_of_le_of_ne hu.1 (Ne.symm (hnu hu))
  have hb1 : -1 < b := hu1.trans hub
  have hv1 : 0 < 1 + v := by linarith
  have hb : b ∈ shapeDomain y s := by
    refine ⟨hb1.le, hs, fun i ↦ ⟨(hu.2.2 i).1, ?_⟩⟩
    rw [ite_eq_right hb1.ne']
    have h₁ := (hu.2.2 i).2
    have h₂ := (hv.2.2 i).2
    rw [ite_eq_right (hnu hu)] at h₁
    rw [ite_eq_right (hnu hv)] at h₂
    dsimp [b]
    linarith
  let J (r t : ℝ) := ∑ i, ∫ z in (0 : ℝ)..(y i / s), kernel r t z
  have hsum {r t : ℝ} (hr : r ∈ shapeDomain y s) (ht : t ∈ shapeDomain y s) :
      logLik y s t - logLik y s r = (t - r) * J r t := by
    unfold logLik J
    rw [← Finset.sum_sub_distrib, Finset.mul_sum]
    exact Finset.sum_congr rfl fun i _ ↦ density_difference (hz i) (support hr i) (support ht i)
  have hleft : J u b ≤ 0 := by
    have H : logLik y s b ≤ logLik y s u := hmu hb
    have he := hsum hu hb
    exact nonpos_of_mul_nonpos_right (by linarith) (sub_pos.mpr hub)
  have hright : 0 ≤ J b v := by
    have H : logLik y s b ≤ logLik y s v := hmv hb
    have he := hsum hb hv
    exact nonneg_of_mul_nonneg_right (by linarith) (sub_pos.mpr hbv)
  have hlt : J b v < ((1 + u) / (1 + v)) * J u b := by
    unfold J
    rw [Finset.mul_sum]
    apply Finset.sum_lt_sum
    · intro i _
      exact (kernel_comparison huv (hz i) (support hu i) (support hb i) (support hv i) hv1).1
    · exact ⟨j, Finset.mem_univ _, (kernel_comparison huv (hz j) (support hu j)
        (support hb j) (support hv j) hv1).2 ((lt_div_iff₀ hs).mpr (by simpa using hj))⟩
  have hnon : 0 ≤ (1 + u) / (1 + v) := div_nonneg (by linarith) hv1.le
  exact (not_lt_of_ge hright) (hlt.trans_le (mul_nonpos_of_nonneg_of_nonpos hnon hleft))

/-- The complete fixed-threshold, fixed-scale uniqueness alternatives. -/
theorem held_threshold_scale_uniqueness {n : ℕ} (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) {s : ℝ} (hs : 0 < s) :
    let D := shapeDomain y s
    let L := logLik y s
    ((∀ i, y i = 0) → D = Ici (-1) ∧ ∀ ξ ∈ D, IsMaxOn L D ξ) ∧
    ((∃ i, 0 < y i) → ∃! ξ, ξ ∈ D ∧ IsMaxOn L D ξ) := by
  intro D L
  constructor
  · intro hz
    have h := held_scale_all_zero s hs y hz
    exact ⟨h.1, fun ξ hξ κ hκ ↦ (h.2 κ hκ).le.trans_eq (h.2 ξ hξ).symm⟩
  · intro hp
    by_cases hsmall : ∀ i, y i ≤ s
    · obtain ⟨hu, hm, hs⟩ := held_scale_uniform_unique y hs (fun i ↦ ⟨hy i, hsmall i⟩) hp
      exact ⟨-1, ⟨hu, hm⟩, fun v hv ↦ by
        by_contra hn
        exact (hs v hv.1 hn).not_ge (hv.2 hu)⟩
    · push Not at hsmall
      obtain ⟨j, hj⟩ := hsmall
      obtain ⟨v, hv, hm⟩ := held_threshold_scale_attains y hy hs
      exact ⟨v, ⟨hv, hm⟩, fun u hu ↦ nonuniform_at_most_one y hs hj hu.1 hv hu.2 hm⟩

/-- At held scale, the uniform interval and the constant ridge give all
winners when the span fits. Otherwise the unique winner is nonuniform
and its threshold is the sample minimum. -/
theorem held_scale_uniqueness {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let D := {p : ℝ × ℝ | p.2 ∈ shapeDomain (fun k ↦ x k - p.1) s}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) s p.2
    (x j - x i ≤ s → ∀ p, (p ∈ D ∧ IsMaxOn L D p) ↔
      (p.2 = -1 ∧ p.1 ∈ Icc (x j - s) (x i)) ∨
        (-1 ≤ p.2 ∧ p.1 = x i ∧ x i = x j)) ∧
    (s < x j - x i → (∃! p, p ∈ D ∧ IsMaxOn L D p) ∧
      ∀ p ∈ D, IsMaxOn L D p → p.1 = x i ∧ -1 < p.2) := by
  intro D L
  refine ⟨Base7GPD.held_scale_uniform_argmax x hs hi hj, ?_⟩
  intro hspan
  have hprofile (p : ℝ × ℝ) (hp : p ∈ D) (hmax : IsMaxOn L D p) :
      p.1 = x i ∧ -1 < p.2 := by
    change p.2 ∈ shapeDomain (fun k ↦ x k - p.1) s at hp
    have hm : p.1 ≤ x i := sub_nonneg.mp (hp.2.2 i).1
    have hnu : p.2 ≠ -1 := by
      intro e
      have H := (hp.2.2 j).2
      rw [e] at H
      simp at H
      linarith
    refine ⟨hm.eq_or_lt.resolve_right ?_, hp.1.lt_of_ne hnu.symm⟩
    intro hlt
    obtain ⟨hv, H⟩ := nonuniform_location_strict hn x hnu hp hi hlt
    exact H.not_ge (hmax (a := (x i, p.2)) hv)
  obtain ⟨m, v, hv, hm⟩ := held_scale_attains hn x hs
  have hmax : IsMaxOn L D (m, v) := fun p hp ↦ hm p.1 p.2 hp
  refine ⟨⟨(m, v), ⟨hv, hmax⟩, ?_⟩, hprofile⟩
  rintro ⟨q, u⟩ ⟨hu, hmu⟩
  have hq : q = x i := (hprofile (q, u) hu hmu).1
  have hm' : m = x i := (hprofile (m, v) hv hmax).1
  subst q
  subst m
  have ha : IsMaxOn (logLik (fun k ↦ x k - x i) s)
      (shapeDomain (fun k ↦ x k - x i) s) u :=
    fun a ha ↦ hmu (a := (x i, a)) ha
  have hb : IsMaxOn (logLik (fun k ↦ x k - x i) s)
      (shapeDomain (fun k ↦ x k - x i) s) v :=
    fun a ha ↦ hmax (a := (x i, a)) ha
  exact Prod.ext rfl (nonuniform_at_most_one _ hs hspan hu hv ha hb)

private theorem loss_nonneg {z a : ℝ} (hz : 0 ≤ z) (ha : 0 < 1 + a * z)
    (ha1 : -1 ≤ a) : 0 ≤ (1 + a) * logRatio z a := by
  rw [logRatio_eq_integral hz ha]
  apply mul_nonneg (by linarith)
  exact intervalIntegral.integral_nonneg hz fun t ht ↦ inv_nonneg.mpr (support_between ht ha).le

private theorem shape_bound {n : ℕ} {y : Fin n → ℝ} {s a : ℝ}
    (ha : a ∈ shapeDomain y s) :
    logLik y s a ≤ -(n : ℝ) * log s := by
  have H (i : Fin n) : logDensity s a (y i) ≤ -log s := by
    by_cases e : a = -1
    · simp [e, logDensity]
    · have H := normalized_support ha.2.1 (by
        simpa only [ite_eq_right e] using (ha.2.2 i).2)
      exact sub_le_self _ (loss_nonneg (div_nonneg (ha.2.2 i).1 ha.2.1.le) H ha.1)
  simpa [logLik, nsmul_eq_mul] using Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ H i)

private theorem log_le_loss {z a : ℝ} (hz : 1 ≤ z) (ha : 0 < 1 + a * z)
    (ha1 : -1 < a) : log z ≤ (1 + a) * logRatio z a := by
  have h1 : (1 : ℝ) ∈ Icc 0 z := ⟨zero_le_one, hz⟩
  have hpos (t : ℝ) (ht : t ∈ Icc 1 z) : 0 < 1 + a * t :=
    support_between ⟨zero_le_one.trans ht.1, ht.2⟩ ha
  let f := fun t ↦ (1 + a) * logRatio t a - log t
  have hd (t : ℝ) (ht : t ∈ Icc 1 z) :
      HasDerivAt f ((1 + a) * (1 + a * t)⁻¹ - t⁻¹) t := by
    have H : HasDerivAt (fun u ↦ logRatio u a) ((1 + a * t)⁻¹) t := by
      by_cases he : a = 0
      · simpa [he, logRatio] using hasDerivAt_id' t
      · have H := (((hasDerivAt_id' t).const_mul a).const_add 1).log (hpos t ht).ne'
        convert H.div_const a using 1
        · simp [logRatio, he]
        · field_simp [he, (hpos t ht).ne']
    exact (H.const_mul _).sub (hasDerivAt_log (zero_lt_one.trans_le ht.1).ne')
  have hm : MonotoneOn f (Icc 1 z) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
    · exact fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt
    · intro t ht
      rw [interior_Icc] at ht
      exact (hd t ⟨ht.1.le, ht.2.le⟩).differentiableAt.differentiableWithinAt
    · intro t ht
      rw [interior_Icc] at ht
      have hmem : t ∈ Icc 1 z := ⟨ht.1.le, ht.2.le⟩
      rw [(hd t hmem).deriv, sub_nonneg, ← div_eq_mul_inv, inv_eq_one_div,
        div_le_div_iff₀ (zero_lt_one.trans ht.1) (hpos t hmem)]
      nlinarith only [ht.1]
  have hle := hm (left_mem_Icc.mpr hz) (right_mem_Icc.mpr hz) hz
  have hnon := loss_nonneg (z := 1) zero_le_one (support_between h1 ha) ha1.le
  dsimp [f] at hle
  rw [log_one, sub_zero] at hle
  linarith

private theorem receding_location {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) (m a : ℕ → ℝ)
    (hm : Tendsto m atTop atBot)
    (hv : ∀ᶠ t in atTop, a t ∈ shapeDomain (fun j ↦ x j - m t) s) :
    Tendsto (fun t ↦ logLik (fun j ↦ x j - m t) s (a t)) atTop atBot := by
  let j : Fin n := ⟨0, hn⟩
  let z := fun t ↦ (x j - m t) / s
  have hz : Tendsto z atTop atTop := by
    have H := tendsto_atTop_add_const_left _ (x j) (tendsto_neg_atBot_atTop.comp hm)
    simpa only [z, Function.comp_apply, sub_eq_add_neg, div_eq_mul_inv] using
      H.atTop_mul_const (inv_pos.mpr hs)
  have hlim := tendsto_atBot_add_const_left _ (-(n : ℝ) * log s)
    (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hz))
  apply tendsto_atBot_mono' _ _ hlim
  filter_upwards [hv, hz.eventually_gt_atTop 1] with t hvalid hzt
  have hnu : a t ≠ -1 := by
    intro e
    have H := (hvalid.2.2 j).2
    rw [e] at H
    simp at H
    have hgap : s < x j - m t := by simpa only [one_mul] using (lt_div_iff₀ hs).mp hzt
    linarith
  have ha : -1 < a t := hvalid.1.lt_of_ne hnu.symm
  have hnorm (i : Fin n) : 0 < 1 + a t * ((x i - m t) / s) :=
    normalized_support hs (by simpa only [ite_eq_right hnu] using (hvalid.2.2 i).2)
  have H := log_le_loss hzt.le (hnorm j) ha
  have hsum := Finset.single_le_sum (f := fun i ↦ (1 + a t) *
    logRatio ((x i - m t) / s) (a t))
    (fun i _ ↦ loss_nonneg (div_nonneg (hvalid.2.2 i).1 hs.le) (hnorm i) ha.le)
    (Finset.mem_univ j)
  change logLik (fun i ↦ x i - m t) s (a t) ≤ -(n : ℝ) * log s - log (z t)
  simp only [logLik, logDensity, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  linarith

private theorem infinite_scaled_gap {n : ℕ} (hn : 0 < n) (M : ℝ) {s : ℝ}
    (m a : ℕ → ℝ) (ha : Tendsto a atTop atTop)
    (hg : Tendsto (fun t ↦ a t * ((M - m t) / s)) atTop atTop) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ M - m t) s (a t)) atTop atBot := by
  let g := fun t ↦ 1 + a t * ((M - m t) / s)
  have hg' : Tendsto g atTop atTop := tendsto_atTop_add_const_left _ 1 hg
  have hl := tendsto_log_atTop.comp hg'
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have H := tendsto_atBot_add_const_left _ (-(n : ℝ) * log s)
    (tendsto_neg_atTop_atBot.comp (hl.const_mul_atTop hn'))
  apply tendsto_atBot_mono' _ _ H
  filter_upwards [ha.eventually_gt_atTop 0, hl.eventually_ge_atTop 0] with t hat hlt
  change 0 ≤ log (g t) at hlt
  change logLik (fun _ : Fin n ↦ M - m t) s (a t) ≤
    -(n : ℝ) * log s - n * log (g t)
  have hle : log (g t) ≤ (1 + a t) * (log (g t) / a t) := by
    rw [← mul_div_assoc, le_div_iff₀ hat]
    nlinarith only [hlt]
  simp only [logLik, logDensity, logRatio, ite_eq_right hat.ne', Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  have hmul := mul_le_mul_of_nonneg_left hle hn'.le
  dsimp [g] at hmul
  linarith

private theorem uniform_contact_trace {n : ℕ} (x : Fin n → ℝ) {s m : ℝ}
    (hs : 0 < s) (hx : ∀ i, x i ∈ Icc m (m + s)) :
    (∀ᶠ a in 𝓝[>] (-1 : ℝ), a ∈ shapeDomain (fun i ↦ x i - m) s) ∧
      Tendsto (fun a ↦ logLik (fun i ↦ x i - m) s a) (𝓝[>] (-1 : ℝ))
        (𝓝 (-(n : ℝ) * log s)) := by
  have hn : ∀ᶠ a : ℝ in 𝓝[>] (-1 : ℝ), -1 < a ∧ a < 0 :=
    (show ∀ᶠ a : ℝ in 𝓝[>] (-1 : ℝ), -1 < a from self_mem_nhdsWithin).and
      (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds (by norm_num)))
  have hz (i : Fin n) : (x i - m) / s ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg (sub_nonneg.mpr (hx i).1) hs.le,
      (div_le_one hs).mpr (by linarith [(hx i).2])⟩
  have hv : ∀ᶠ a in 𝓝[>] (-1 : ℝ), a ∈ shapeDomain (fun i ↦ x i - m) s := by
    filter_upwards [hn] with a ha
    refine ⟨ha.1.le, hs, fun i ↦ ⟨sub_nonneg.mpr (hx i).1, ?_⟩⟩
    rw [ite_eq_right ha.1.ne']
    have H := mul_le_mul_of_nonpos_left (show x i - m ≤ s by linarith [(hx i).2]) ha.2.le
    nlinarith only [H, mul_pos hs (show 0 < 1 + a by linarith)]
  refine ⟨hv, ?_⟩
  have aid : Tendsto (fun a : ℝ ↦ a) (𝓝[>] (-1 : ℝ)) (𝓝 (-1)) :=
    tendsto_id'.mpr nhdsWithin_le_nhds
  have ht : Tendsto (fun a : ℝ ↦ (1 + a) * log (1 + a)) (𝓝[>] (-1 : ℝ)) (𝓝 0) := by
    have H := aid.const_add 1
    have hzero : (1 : ℝ) + -1 = 0 := by norm_num
    rw [hzero] at H
    have hcomp := (continuous_mul_log.continuousAt (x := (0 : ℝ))).tendsto.comp H
    rw [zero_mul] at hcomp
    exact hcomp.congr (fun _ ↦ rfl)
  have htrat := ht.div aid (by norm_num : (-1 : ℝ) ≠ 0)
  have hper (i : Fin n) :
      Tendsto (fun a ↦ (1 + a) * logRatio ((x i - m) / s) a)
        (𝓝[>] (-1 : ℝ)) (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
      (by simpa using htrat)
    · filter_upwards [hn] with a ha
      have H := mul_le_mul_of_nonpos_left (hz i).2 ha.2.le
      exact loss_nonneg (hz i).1 (by linarith) ha.1.le
    · filter_upwards [hn] with a ha
      change (1 + a) * logRatio ((x i - m) / s) a ≤ ((1 + a) * log (1 + a)) / a
      rw [logRatio, ite_eq_right ha.2.ne, mul_div_assoc]
      apply mul_le_mul_of_nonneg_left _ (by linarith [ha.1])
      apply (div_le_div_right_of_neg ha.2).mpr
      exact log_le_log (by linarith [ha.1])
        (by linarith [mul_le_mul_of_nonpos_left (hz i).2 ha.2.le])
  have H := tendsto_finsetSum Finset.univ fun i _ ↦
    (tendsto_const_nhds (x := -log s)).sub (hper i)
  simpa [logLik, logDensity, nsmul_eq_mul] using H

/-- At positive held scale a finite optimum exists. Receding location and
nonuniform contact give zero likelihood, and positive infinite-shape paths
require a constant sample and threshold approaching its common value. The
scaled-gap trace includes its zero limit at infinity. Feasible uniform
contact is the attainable upper envelope, also reached from nonuniform shapes. -/
theorem held_scale_boundary_classification {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    let D := fun m ↦ shapeDomain (fun i ↦ x i - m) s
    let L := fun m ↦ logLik (fun i ↦ x i - m) s
    (∃ m a, a ∈ D m ∧ ∀ q b, b ∈ D q → L q b ≤ L m a) ∧
    (∀ m a : ℕ → ℝ, (∀ᶠ t in atTop, a t ∈ D (m t)) →
      (Tendsto m atTop atBot → Tendsto (fun t ↦ L (m t) (a t)) atTop atBot) ∧
      (∀ B : ℝ, Tendsto a atTop atTop → (∀ᶠ t in atTop, B ≤ L (m t) (a t)) →
        ∃ M, (∀ i, x i = M) ∧ Tendsto m atTop (𝓝 M)) ∧
      (∀ q b : ℝ, Tendsto m atTop (𝓝 q) → Tendsto a atTop (𝓝 b) → -1 < b →
        (∃ j, 0 < x j - q ∧ s + b * (x j - q) = 0) →
          Tendsto (fun t ↦ L (m t) (a t)) atTop atBot)) ∧
    (∀ M : ℝ, (∀ i, x i = M) → ∀ m a : ℕ → ℝ, Tendsto a atTop atTop →
      let gap := fun t ↦ a t * ((M - m t) / s)
      (∀ v ≥ 0, Tendsto gap atTop (𝓝 v) →
        Tendsto (fun t ↦ L (m t) (a t)) atTop (𝓝 (-(n : ℝ) * (log s + log (1 + v))))) ∧
      (Tendsto gap atTop atTop → Tendsto (fun t ↦ L (m t) (a t)) atTop atBot)) ∧
    (∀ m a, a ∈ D m → L m a ≤ -(n : ℝ) * log s) ∧
    (∀ m, (∀ i, x i ∈ Icc m (m + s)) →
      (-1 : ℝ) ∈ D m ∧ L m (-1) = -(n : ℝ) * log s ∧
      (∀ᶠ a in 𝓝[>] (-1 : ℝ), a ∈ D m) ∧
      Tendsto (L m) (𝓝[>] (-1 : ℝ)) (𝓝 (-(n : ℝ) * log s))) := by
  intro D L
  have H := Base7GPD.held_scale_failure hn x hs
  refine ⟨H.1, ?_, ?_, fun _ _ ha ↦ shape_bound ha, ?_⟩
  · intro m a hv
    refine ⟨fun hm ↦ receding_location hn x hs m a hm hv,
      fun B ha hb ↦ H.2 m a B hv ha hb, ?_⟩
    rintro q b hm ha hb ⟨j, hj, hc⟩
    exact held_scale_nonuniform_upper_contact x hs hb m a hm ha hv hj hc
  · intro M hx m a ha
    change ((∀ v ≥ 0, _) ∧ _)
    have he : x = fun _ ↦ M := funext hx
    subst x
    exact ⟨fun v hv hg ↦ Base7GPD.held_scale_constant_infinite_shape_trace M hs m a ha hv hg,
      infinite_scaled_gap hn M m a ha⟩
  · intro m hm
    refine ⟨⟨le_rfl, hs, fun i ↦ ⟨sub_nonneg.mpr (hm i).1, ?_⟩⟩, ?_,
      uniform_contact_trace x hs hm⟩
    · have H := (hm i).2
      simp
      linarith
    · simp [L, logLik, logDensity, nsmul_eq_mul]

end UnivariateML.Base7bGPD

end
