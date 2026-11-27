module

public import UnivariateML.Family.Endpoint2ParetoIV
public import UnivariateML.Family.Endpoint3FellerParetoHeldScale
public import UnivariateML.Family.Endpoint3FellerParetoFreeScale

import Mathlib.Tactic

set_option linter.style.header false

/-! # Pareto IV with fixed scale and power -/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint3ParetoIV

variable {n : ℕ}

private theorem constant_path (hn : 0 < n) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    Tendsto (fun t ↦ ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ 0)
      (-t) (((t / s) ^ c)⁻¹) c s) (𝓝[>] 0) atTop := by
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hbase : Tendsto (fun t : ℝ ↦ (n : ℝ) * (log c - log t - 2))
      (𝓝[>] 0) atTop := by
    have h := (tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero)
    convert (tendsto_atTop_add_const_right _ (log c - 2) h).const_mul_atTop hN using 1
    ext t
    simp only [Function.comp_apply]
    ring
  apply tendsto_atTop_mono' _ ?_ hbase
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
    mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hs)] with t ht hts
  let u := (t / s) ^ c
  have hu : 0 < u := rpow_pos_of_pos (div_pos ht hs) _
  have hu' : u ≤ 1 := by
    dsimp [u]
    exact rpow_le_one (div_nonneg ht.le hs.le)
      ((div_le_one hs).mpr hts.le) hc.le
  have hl : log (1 + u) ≤ u := by
    linarith [log_le_sub_one_of_pos (show 0 < 1 + u by linarith)]
  have hp := mul_le_mul_of_nonneg_left hl (by positivity : 0 ≤ u⁻¹ + 1)
  have hmul : (u⁻¹ + 1) * u = 1 + u := by
    rw [add_mul, inv_mul_cancel₀ hu.ne', one_mul]
  have hlog : log u⁻¹ = c * log (s / t) := by
    rw [log_inv, show u = (t / s) ^ c from rfl, log_rpow (div_pos ht hs),
      log_div ht.ne' hs.ne', log_div hs.ne' ht.ne']
    ring
  unfold ShapeFacesParetoIV.logLik
  simp only [zero_sub, neg_neg, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  change (n : ℝ) * (log c - log t - 2) ≤
    (n : ℝ) * (log u⁻¹ + log c - log t - c * log (s / t) - (u⁻¹ + 1) * log (1 + u))
  rw [hlog]
  apply mul_le_mul_of_nonneg_left _ hN.le
  linarith

/-- At fixed scale and power, either a subunit power or a constant
sample makes the objective unbounded. This holds on strictly positive
threshold gaps, so including the finite unit-power contact gives the
same conclusion. -/
theorem held_scale_power_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s)
    (h : c < 1 ∨ ∀ i, d i = 0) :
    ∀ B : ℝ, ∃ t > 0, ∃ k > 0, B < ShapeFacesParetoIV.logLik d (-t) k c s := by
  intro B
  have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  rcases h with hsub | hconst
  · have hlim := Endpoint2ParetoIV.threshold_spike hd hz one_pos hc hsub hs
    obtain ⟨t, hgt, ht⟩ := ((hlim.eventually_gt_atTop B).and hpos).exists
    exact ⟨t, ht, 1, one_pos, hgt⟩
  · have hn : 0 < n := by obtain ⟨i, _⟩ := hz; exact i.isLt.pos
    have hlim := constant_path hn hc hs
    obtain ⟨t, hgt, ht⟩ := ((hlim.eventually_gt_atTop B).and hpos).exists
    rw [show d = fun _ ↦ 0 from funext hconst]
    exact ⟨t, ht, _, inv_pos.mpr (rpow_pos_of_pos (div_pos ht hs) _), hgt⟩

private theorem softplus_split (c z : ℝ) :
    log (1 + exp (c * z)) = c * max 0 z + log (1 + exp (-(c * |z|))) := by
  rcases le_total 0 z with hz | hz
  · rw [max_eq_right hz, abs_of_nonneg hz]
    have he : 1 + exp (c * z) = exp (c * z) * (1 + exp (-(c * z))) := by
      rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
    rw [he, log_mul (exp_pos _).ne' (by positivity), log_exp]
  · simp [max_eq_left hz, abs_of_nonpos hz]

private theorem density_on_power_ray {r s y : ℝ} (hr : 0 < r) (hs : 0 < s)
    (hy : 0 < y) :
    Tendsto (fun c : ℝ ↦ exp (log (r / c) + log c - log y - c * log (s / y) -
      (r / c + 1) * log (1 + (y / s) ^ c))) atTop
      (𝓝 (if y < s then 0 else if y = s then r / (2 * y)
        else r / y * exp (-(r * log (y / s))))) := by
  let z := log (y / s)
  let A := min z 0
  let B := max 0 z
  let δ : ℝ := if z = 0 then log 2 else 0
  have hA : Tendsto (fun c : ℝ ↦ exp (c * A)) atTop (𝓝 (if z < 0 then 0 else 1)) := by
    by_cases h : z < 0
    · simp only [h, ite_true]
      exact tendsto_exp_atBot.comp
        (tendsto_id.atTop_mul_const_of_neg (show A < 0 by simpa [A, min_eq_left h.le] using h))
    · simp [A, min_eq_right (le_of_not_gt h), h]
  have hδ : Tendsto (fun c : ℝ ↦ log (1 + exp (-(c * |z|)))) atTop (𝓝 δ) := by
    by_cases h : z = 0
    · simp [δ, h, show (1 : ℝ) + 1 = 2 by norm_num]
    · have he := tendsto_exp_atBot.comp
        (tendsto_neg_atTop_atBot.comp (tendsto_id.atTop_mul_const (abs_pos.mpr h)))
      have hsum : Tendsto (fun c : ℝ ↦ 1 + exp (-(c * |z|))) atTop (𝓝 1) := by
        simpa only [add_zero, Function.comp_def, id_eq] using he.const_add (1 : ℝ)
      simpa only [δ, h, ite_false, log_one, Function.comp_def] using
        (continuousAt_log one_ne_zero).tendsto.comp hsum
  have hrate : Tendsto (fun c : ℝ ↦ r / c + 1) atTop (𝓝 1) := by
    simpa only [div_eq_mul_inv, zero_add, mul_zero] using
      (tendsto_inv_atTop_zero.const_mul r).add_const (1 : ℝ)
  have hC : Tendsto (fun c : ℝ ↦ exp (-((r / c + 1) * log (1 + exp (-(c * |z|))))))
      atTop (𝓝 (exp (-δ))) := by
    simpa only [one_mul, Function.comp_def] using
      (continuous_exp.continuousAt.tendsto.comp (hrate.mul hδ).neg)
  have hlimit := ((hA.const_mul (r / y)).mul
    (tendsto_const_nhds (x := exp (-(r * B))))).mul hC
  have hsign : z < 0 ↔ y < s := (log_neg_iff (div_pos hy hs)).trans (div_lt_one hs)
  have hzero : z = 0 ↔ y = s := by
    constructor
    · intro h
      apply (div_eq_one_iff_eq hs.ne').mp
      apply log_injOn_pos (div_pos hy hs) one_pos
      simpa only [log_one] using h
    · intro h
      simp [z, h, hs.ne']
  have hvalue :
      (r / y * (if z < 0 then 0 else 1) * exp (-(r * B))) * exp (-δ) =
        if y < s then 0 else if y = s then r / (2 * y)
          else r / y * exp (-(r * log (y / s))) := by
    rcases lt_trichotomy y s with h | h | h
    · simp [h, hsign.mpr h]
    · have hz := hzero.mpr h
      simp [h, hz, δ, B, exp_neg, exp_log (by norm_num : (0 : ℝ) < 2)]
      ring
    · have h' : ¬ z < 0 := fun hz ↦ h.not_ge (hsign.mp hz).le
      have hz : z ≠ 0 := fun e ↦ h.ne' (hzero.mp e)
      simp [h.not_gt, h.ne', h', hz, δ, B, max_eq_right (le_of_not_gt h'), z]
  rw [hvalue] at hlimit
  apply hlimit.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
  have hB : A + B = z := by simpa [A, B, max_comm] using min_add_max z (0 : ℝ)
  have hz : log (s / y) = -z := by
    simp only [z, log_div hs.ne' hy.ne', log_div hy.ne' hs.ne']
    ring
  rw [hz, rpow_def_of_pos (div_pos hy hs), mul_comm (log (y / s)) c,
    softplus_split, log_div hr.ne' hc.ne']
  change _ = exp (log r - log c + log c - log y - c * -z -
    (r / c + 1) * (c * B + log (1 + exp (-(c * |z|)))))
  rw [← exp_log (div_pos hr hy), log_div hr.ne' hy.ne', ← exp_add, ← exp_add, ← exp_add]
  congr 1
  rw [← hB]
  field_simp [hc.ne']
  ring

/-- At fixed scale and exactly fixed shape product, a Pareto IV density
below cutoff tends to zero, at cutoff to half the Pareto density, and
above cutoff to the Pareto density. Products retain every tied factor. -/
theorem fixed_scale_pareto_trace {x : Fin n → ℝ} {μ r s : ℝ}
    (hx : ∀ i, μ < x i) (hr : 0 < r) (hs : 0 < s) :
    let y := fun i ↦ x i - μ
    Tendsto (fun c : ℝ ↦ exp (ShapeFacesParetoIV.logLik x μ (r / c) c s)) atTop
      (𝓝 (∏ i, if y i < s then 0 else if y i = s then r / (2 * y i)
        else r / y i * exp (-(r * log (y i / s))))) := by
  intro y
  unfold ShapeFacesParetoIV.logLik
  simp_rw [exp_sum]
  apply tendsto_finsetProd
  intro i _
  exact density_on_power_ray hr hs (sub_pos.mpr (hx i))

/-- Pareto IV threshold likelihood with the finite closed contact
extension at unit power. -/
noncomputable def thresholdLogLik (d : Fin n → ℝ) (k c s t : ℝ) : ℝ :=
  Endpoint2FellerPareto.logLik d c (c * k) c s (log k + log c - log s) t

private theorem regular_kernel {d : Fin n → ℝ} {k c s t : ℝ} (hk : 0 < k) :
    Endpoint2FellerPareto.interiorLogLik d 1 k c s t = thresholdLogLik d k c s t := by
  have hΓ : Gamma (1 + k) = k * Gamma k := by
    simpa only [add_comm] using Gamma_add_one hk.ne'
  unfold Endpoint2FellerPareto.interiorLogLik thresholdLogLik
  simp only [mul_one, Gamma_one, log_one, zero_add, hΓ,
    log_mul hk.ne' (Gamma_pos_of_pos hk).ne']
  congr 1
  ring

/-- On strictly containing thresholds the regular kernel agrees with
the physical Pareto IV likelihood. At feasible unit-power contact
the same kernel evaluates the continuous density from inside support. -/
theorem threshold_likelihood_transfer {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {k c s t : ℝ} (_hk : 0 < k) (hc : 0 < c) (hs : 0 < s) (ht : 0 < t) :
    ShapeFacesParetoIV.logLik d (-t) k c s = thresholdLogLik d k c s t := by
  have hcoef : (c + c * k) / c = k + 1 := by
    calc
      (c + c * k) / c = c * (1 + k) / c := by congr 1; ring
      _ = k + 1 := by rw [mul_div_cancel_left₀ _ hc.ne', add_comm]
  unfold ShapeFacesParetoIV.logLik thresholdLogLik Endpoint2FellerPareto.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hy : 0 < t + d i := add_pos_of_pos_of_nonneg ht (hd i)
  simp only [sub_neg_eq_add, add_comm (d i) t, hcoef]
  rw [log_div hs.ne' hy.ne', log_div hy.ne' hs.ne']
  ring

/-- Both structural Pareto IV rays are feasible. A subunit lower power
permits threshold spiking at every positive held scale and upper shape.
Free scale and upper shape also permit the simultaneous collapse at
every positive power. -/
theorem structural_spike_and_collapse {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) :
    (∀ k c s : ℝ, 0 < k → 0 < c → c < 1 → 0 < s →
      Tendsto (fun t ↦ ShapeFacesParetoIV.logLik d (-t) k c s) (𝓝[>] 0) atTop) ∧
    (∀ c : ℝ, 0 < c → Tendsto
      (fun T ↦ ShapeFacesParetoIV.logLik d (-exp (-T)) T⁻¹ c (exp (-T))) atTop atTop) := by
  exact ⟨fun k c s hk hc hlt hs ↦ Endpoint2ParetoIV.threshold_spike hd hz hk hc hlt hs,
    fun c hc ↦ Endpoint2ParetoIV.simultaneous_collapse hd hz hc⟩

/-- At fixed scale and power, Pareto IV attains exactly on nonconstant
samples with power at least one, including closed unit-power contact. -/
theorem held_scale_power_criterion {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ (if c = 1 then Ici (0 : ℝ) else Ioi 0)
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d v.1 c s v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ c ∧ ∃ i, 0 < d i := by
  intro D L
  have h := Endpoint3FellerParetoHeldScale.criterion hd hz one_pos hc hs
  simp only [mul_one] at h
  have he (v : ℝ × ℝ) (hv : v ∈ D) :
      Endpoint2FellerPareto.interiorLogLik d 1 v.1 c s v.2 = L v :=
    regular_kernel hv.1
  have hMax (v : ℝ × ℝ) (hv : v ∈ D) :
      IsMaxOn (fun w : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d 1 w.1 c s w.2)
        D v ↔ IsMaxOn L D v := by
    constructor <;> intro hm w hw
    · have hh : Endpoint2FellerPareto.interiorLogLik d 1 w.1 c s w.2 ≤
          Endpoint2FellerPareto.interiorLogLik d 1 v.1 c s v.2 := hm hw
      rw [he v hv, he w hw] at hh
      exact hh
    · change Endpoint2FellerPareto.interiorLogLik d 1 w.1 c s w.2 ≤
        Endpoint2FellerPareto.interiorLogLik d 1 v.1 c s v.2
      rw [he v hv, he w hw]
      exact hm hw
  constructor
  · rintro ⟨v, hv, hm⟩
    exact h.mp ⟨v, hv, (hMax v hv).mpr hm⟩
  · intro hreg
    obtain ⟨v, hv, hm⟩ := h.mpr hreg
    exact ⟨v, hv, (hMax v hv).mp hm⟩

/-- With Pareto IV scale and shapes held, unit power maximizes at
closed threshold contact. Higher power maximizes at the unique
interior normalized-score root. -/
theorem held_scale_shapes_threshold {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c s : ℝ} (hk : 0 < k) (hc : 0 < c) (hs : 0 < s) :
    let L := thresholdLogLik d k c s
    let S := Endpoint2FellerParetoRegular.normalizedScore d c (c * k) c s
    (c = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < c → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0) :=
  Endpoint3FellerPareto.fixed_regular_threshold_uniqueness hd hz (mul_pos hc hk) hc hs

/-- With the Pareto IV shapes held and scale free, subunit power or
negative collapse exponent makes the likelihood unbounded. At regular
power and zero exponent the finite envelope is strictly unattained. -/
theorem held_shapes_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d k c v.1 v.2
    let E := Endpoint2FellerParetoFreeScale.exponent d (c * k)
    ((c < 1 ∨ E < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ c ∧ E = 0) → ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C) :=
  Endpoint3FellerPareto.fixed_shapes_failure hd hz (mul_pos hc hk) hc

/-- At a held threshold and scale, a constant positive excess at or
below scale gives unbounded likelihood along diverging power and
upper shape equal to the reciprocal powered excess ratio. -/
theorem held_scale_constant_power_path (hn : 0 < n) {μ x s : ℝ}
    (hx : μ < x) (hs : 0 < s) (hle : x - μ ≤ s) :
    Tendsto (fun c ↦ ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x) μ
      (((x - μ) / s) ^ c)⁻¹ c s) atTop atTop := by
  let y := x - μ
  have hy : 0 < y := sub_pos.mpr hx
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hbase : Tendsto (fun c : ℝ ↦ (n : ℝ) * (log c - (log y + 2))) atTop atTop := by
    simpa only [sub_eq_add_neg] using
      (tendsto_atTop_add_const_right _ (-(log y + 2)) tendsto_log_atTop).const_mul_atTop hN
  apply tendsto_atTop_mono' _ ?_ hbase
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
  let u := (y / s) ^ c
  have hu : 0 < u := rpow_pos_of_pos (div_pos hy hs) _
  have hu' : u ≤ 1 := rpow_le_one (div_nonneg hy.le hs.le) ((div_le_one hs).mpr hle) hc.le
  have hl : log (1 + u) ≤ u := by
    linarith [log_le_sub_one_of_pos (show 0 < 1 + u by linarith)]
  have hp := mul_le_mul_of_nonneg_left hl (by positivity : 0 ≤ u⁻¹ + 1)
  have hmul : (u⁻¹ + 1) * u = 1 + u := by
    rw [add_mul, inv_mul_cancel₀ hu.ne', one_mul]
  have hlog : log u⁻¹ = c * log (s / y) := by
    rw [log_inv, show u = (y / s) ^ c from rfl, log_rpow (div_pos hy hs),
      log_div hy.ne' hs.ne', log_div hs.ne' hy.ne']
    ring
  unfold ShapeFacesParetoIV.logLik
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  change (n : ℝ) * (log c - (log y + 2)) ≤
    (n : ℝ) * (log u⁻¹ + log c - log y - c * log (s / y) - (u⁻¹ + 1) * log (1 + u))
  rw [hlog]
  apply mul_le_mul_of_nonneg_left _ hN.le
  linarith

/-- At a constant positive excess, a fixed positive upper shape
permits divergence as the power grows with scale equal to the common
excess. The scale may be fixed at that value or chosen on the ray. -/
theorem constant_excess_fixed_upper_shape_path (hn : 0 < n) {μ x k : ℝ}
    (hx : μ < x) :
    Tendsto (fun c ↦ ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x) μ k c (x - μ))
      atTop atTop := by
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  let C := log k - log (x - μ) - (k + 1) * log 2
  have he (c : ℝ) :
      ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x) μ k c (x - μ) =
        (n : ℝ) * log c + (n : ℝ) * C := by
    simp only [ShapeFacesParetoIV.logLik, div_self (sub_pos.mpr hx).ne', log_one, one_rpow,
      mul_zero, sub_zero, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, C]
    norm_num
    ring
  simpa only [he] using tendsto_atTop_add_const_right _ ((n : ℝ) * C)
    (tendsto_log_atTop.const_mul_atTop hN)

/-- When a finite Pareto IV fit with both shapes held exists, the
joint scale and threshold are unique. The threshold reaches sample
contact exactly at unit power. -/
theorem held_shapes_joint_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) (ha : 1 ≤ c)
    (hE : 0 < Endpoint2FellerParetoFreeScale.exponent d (c * k)) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d k c v.1 v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = 0 ↔ c = 1) := by
  dsimp only
  have h (v : ℝ × ℝ) : thresholdLogLik d k c v.1 v.2 =
      Endpoint2FellerParetoFreeScale.logLik d c (c * k) c (log k + log c) v := rfl
  simp only [h]
  exact Endpoint3FellerParetoFreeScale.fixed_shapes_unique hd hz (mul_pos hc hk) hc ha hE

/-- At fixed scale and unit power, the Pareto IV optimum is unique on
nonconstant data and has closed threshold contact. -/
theorem held_scale_unit_power_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) (hne : ∃ i, 0 < d i) {s : ℝ} (hs : 0 < s) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ thresholdLogLik d v.1 1 s v.2
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ v.2 = 0 := by
  intro D L
  let F := fun v : ℝ × ℝ ↦ Endpoint2FellerPareto.interiorLogLik d 1 v.1 1 s v.2
  have h (v : ℝ × ℝ) (hv : v ∈ D) : F v = L v := regular_kernel hv.1
  have he (v : ℝ × ℝ) (hv : v ∈ D) : IsMaxOn L D v ↔ IsMaxOn F D v := by
    constructor <;> intro hm w hw
    · change F w ≤ F v
      rw [h w hw, h v hv]
      exact hm hw
    · have hh : F w ≤ F v := hm hw
      rw [h w hw, h v hv] at hh
      exact hh
  obtain ⟨v, ⟨hv, hm, heq⟩, hu⟩ :=
    Endpoint3FellerParetoHeldScale.unit_lower_power_unique hd hz hne one_pos one_pos hs (by simp)
  exact ⟨v, ⟨hv, (he v hv).mpr hm, heq⟩, fun w hw ↦
    hu w ⟨hw.1, (he w hw.1).mp hw.2.1, hw.2.2⟩⟩

end UnivariateML.Endpoint3ParetoIV

end
