module

public import UnivariateML.Family.Endpoint2FellerParetoFreeScale
public import UnivariateML.Generic.Endpoint3GammaRatio

import Mathlib.Tactic

set_option linter.style.header false

/-! # Fixed-shape Feller–Pareto failure branches -/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Endpoint3FellerPareto

variable {n : ℕ}

open Endpoint2FellerParetoFreeScale

/-- The strict-interior threshold spike and the negative scale exponent
give unbounded likelihood. At regular lower power and zero exponent the
sharp finite envelope is the strictly unattained supremum. -/
theorem fixed_shapes_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c K : ℝ} (hb : 0 < b) (hc : 0 < c) :
    let L := logLik d a b c K
    let D := domain a
    ((a < 1 ∨ exponent d b < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ a ∧ exponent d b = 0) →
        ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C) := by
  dsimp only
  constructor
  · rintro (ha | hE)
    · exact (Endpoint2FellerPareto.unbounded_of_threshold_path hd hz
        (s := 1) (b := b) (K := K) ha hc one_pos (fun t ↦ (1, t))
        (by
          intro t ht
          exact ⟨⟨one_pos, by simpa [domain, ha.ne] using ht⟩,
            by simp [logLik]⟩)).1
    · exact negative_exponent_unbounded hd hc hE
  · rintro ⟨ha, hE⟩
    exact zero_exponent_envelope hd hz ha hb hc hE

/-- With scale, lower beta shape and power fixed, a constant sample
concentrates as the upper beta shape increases and the threshold gap shrinks. -/
theorem constant_upper_shape_path (hn : 0 < n) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s) :
    Tendsto (fun q ↦ Endpoint2FellerPareto.interiorLogLik (fun _ : Fin n ↦ 0)
      p q c s (s * (q⁻¹) ^ c⁻¹)) atTop atTop := by
  let A := log c - log s - log (Gamma p) - (1 + p) - 1
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hbase : Tendsto (fun q : ℝ ↦ (n : ℝ) * (c⁻¹ * log q + A)) atTop atTop :=
    (tendsto_atTop_add_const_right _ A
      (tendsto_log_atTop.const_mul_atTop (inv_pos.mpr hc))).const_mul_atTop hN
  apply tendsto_atTop_mono' _ ?_ hbase
  filter_upwards [eventually_gt_atTop (1 : ℝ),
    (Endpoint3GammaRatio.log_gamma_ratio p hp).eventually
      (Ioi_mem_nhds (by norm_num : (-1 : ℝ) < 0))]
    with q hq hr
  have hq₀ : 0 < q := zero_lt_one.trans hq
  have hu : 0 < q⁻¹ := inv_pos.mpr hq₀
  have hpow : ((q⁻¹) ^ c⁻¹) ^ c = q⁻¹ := by
    rw [← rpow_mul hu.le, inv_mul_cancel₀ hc.ne', rpow_one]
  have hratio : s * (q⁻¹) ^ c⁻¹ / s = (q⁻¹) ^ c⁻¹ := by
    rw [mul_comm, mul_div_cancel_right₀ _ hs.ne']
  have hlog : log (1 + q⁻¹) ≤ q⁻¹ := by
    linarith [log_le_sub_one_of_pos (show 0 < 1 + q⁻¹ by positivity)]
  have hpenalty : (p + q) * log (1 + q⁻¹) ≤ 1 + p := by
    calc
      _ ≤ (p + q) * q⁻¹ := mul_le_mul_of_nonneg_left hlog (add_nonneg hp.le hq₀.le)
      _ = 1 + p * q⁻¹ := by rw [add_mul, mul_inv_cancel₀ hq₀.ne', add_comm]
      _ ≤ 1 + p := add_le_add_right
        (mul_le_of_le_one_right hp.le ((inv_le_one₀ hq₀).mpr hq.le)) 1
  unfold Endpoint2FellerPareto.interiorLogLik Endpoint2FellerPareto.logLik
  simp only [add_zero, hratio, hpow, log_rpow hu, log_inv, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  apply mul_le_mul_of_nonneg_left _ hN.le
  have he : (c * p + c * q) / c = p + q := by
    rw [← mul_add, mul_div_cancel_left₀ _ hc.ne']
  rw [he, add_comm p q]
  change -1 < log (Gamma (q + p)) - log (Gamma q) - p * log q at hr
  have hmul : c * c⁻¹ = 1 := mul_inv_cancel₀ hc.ne'
  dsimp [A]
  linear_combination (norm := ring_nf) hpenalty + le_of_lt hr +
    p * log q * hmul
  exact le_rfl

/-- With fixed scale, lower beta shape and power, the subunit lower
power and constant-data branches both have unbounded likelihood. -/
theorem held_scale_lower_shape_power_failure {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {p c s : ℝ}
    (hp : 0 < p) (hc : 0 < c) (hs : 0 < s)
    (h : c * p < 1 ∨ ∀ i, d i = 0) :
    ∀ B : ℝ, ∃ t > 0, ∃ q > 0, B < Endpoint2FellerPareto.interiorLogLik d p q c s t := by
  intro B
  rcases h with hsub | hconst
  · have hlim : Tendsto (Endpoint2FellerPareto.interiorLogLik d p 1 c s) (𝓝[>] 0) atTop :=
      Endpoint2FellerPareto.threshold_spike hd hz hsub hc hs
    have ht : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
    obtain ⟨t, hB, ht⟩ := ((hlim.eventually_gt_atTop B).and ht).exists
    exact ⟨t, ht, 1, one_pos, hB⟩
  · have hn : 0 < n := by obtain ⟨i, _⟩ := hz; exact i.isLt.pos
    obtain ⟨q, hB, hq⟩ := (((constant_upper_shape_path hn hp hc hs).eventually_gt_atTop B).and
      (eventually_gt_atTop (0 : ℝ))).exists
    rw [show d = fun _ ↦ 0 from funext hconst]
    exact ⟨_, mul_pos hs (rpow_pos_of_pos (inv_pos.mpr hq) _), q, hq, hB⟩

/-- The unique closed threshold at lower power one, and the unique
positive score root above one, in the common threshold kernel. Pareto II
uses lower power one, Pareto III sets both powers equal, and Pareto IV
sets the lower power equal to the transformation power. -/
theorem fixed_regular_threshold_uniqueness {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let L := Endpoint2FellerPareto.logLik d a b c s K
    let S := Endpoint2FellerParetoRegular.normalizedScore d a b c s
    (a = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < a → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0) := by
  intro L S
  constructor
  · intro ha
    subst a
    have hn : 0 < n := by obtain ⟨i, _⟩ := hz; exact i.isLt.pos
    obtain ⟨t, ⟨ht, hm⟩, hu⟩ :=
      Endpoint2FellerPareto.threshold_contact_unique hn hd hb hc hs (K := K)
    have h0 := (Endpoint2FellerParetoRegular.fixed_scale_shapes_fit
      hd hz hb hc hs (a := 1) (K := K)).2.2.2 rfl
    simp only [ite_true] at h0
    exact ⟨h0, fun v hv hmax ↦ (hu v ⟨hv, hmax⟩).trans (hu 0 ⟨le_rfl, h0⟩).symm⟩
  · intro ha
    exact Endpoint2FellerParetoRegular.interior_threshold_fit hd hz ha hb hc hs

end UnivariateML.Endpoint3FellerPareto

end
