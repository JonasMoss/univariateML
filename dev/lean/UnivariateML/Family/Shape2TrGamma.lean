module

public import UnivariateML.Family.AllFreeTransformedGamma
import Mathlib.Analysis.SpecialFunctions.Gaussian.GaussianIntegral
import Mathlib.Analysis.Real.Pi.Bounds

set_option linter.style.header false

/-!
Held-scale transformed gamma, including the finite endpoint trace and
the unique mean-zero slice.

## Deviations

The held-scale path fixes the shape-power product exactly, and the strict trace comparison
assumes the sample is in the power support. The centered uniqueness slice requires a nonzero
residual, as in the written proof.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2TrGamma

open TransformedGamma

noncomputable section

variable {n : ℕ}

private theorem avg_const (hn : 0 < n) (c : ℝ) : Gamma.avg (fun _ : Fin n ↦ c) = c := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [Gamma.avg, nsmul_eq_mul, hN]

private theorem held_form (x : Fin n → ℝ) (k p a : ℝ) :
    logLik x k p a = log p - log (Real.Gamma k) - Gamma.avgLog x -
      Gamma.avg (fun i ↦ exp (p * (log (x i) - log a))) +
      (k * p) * Gamma.avg (fun i ↦ log (x i) - log a) := by
  simp only [logLik, loss, Gamma.avg, Finset.sum_sub_distrib, mul_assoc, ← Finset.mul_sum]
  ring

/-- With scale held at or above every observation, a fixed positive
shape-power product gives the power trace with the full contact penalty. -/
theorem held_scale_power_path {x : Fin n → ℝ} {a d : ℝ}
    (_ha : 0 < a) (hd : 0 < d) (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ a) :
    Tendsto (fun q : ℝ ↦ logLik x (d * q) q⁻¹ a) (𝓝[>] 0)
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0))) := by
  let z : Fin n → ℝ := fun i ↦ log (x i) - log a
  have hq : Tendsto (fun q : ℝ ↦ q) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have harg : Tendsto (fun q : ℝ ↦ d * q + 1) (𝓝[>] 0) (𝓝 1) := by
    simpa only [mul_zero, zero_add] using (hq.const_mul d).add_const 1
  have hΓ : Tendsto (fun q : ℝ ↦ log (Real.Gamma (d * q + 1))) (𝓝[>] 0) (𝓝 0) := by
    have h := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp harg
    simpa only [Real.Gamma_one, Function.comp_apply, log_one] using
      h.log (show Real.Gamma 1 ≠ 0 by simp)
  have hpt (i : Fin n) : Tendsto (fun q : ℝ ↦ exp (q⁻¹ * z i)) (𝓝[>] 0)
      (𝓝 (if x i = a then 1 else 0)) := by
    by_cases he : x i = a
    · simp [he, z]
    · have hz : z i < 0 :=
        sub_neg.mpr (log_lt_log (hx i) ((hle i).lt_of_ne he))
      have hneg := tendsto_inv_nhdsGT_zero.atTop_mul_const_of_neg hz
      rw [ite_eq_right he]
      exact tendsto_exp_atBot.comp hneg
  have hmean : Tendsto (fun q : ℝ ↦ Gamma.avg (fun i ↦ exp (q⁻¹ * z i))) (𝓝[>] 0)
      (𝓝 (Gamma.avg (fun i ↦ if x i = a then 1 else 0))) :=
    (tendsto_finsetSum Finset.univ (fun i _ ↦ hpt i)).div_const _
  have hf := (hΓ.neg.const_add (log d - Gamma.avgLog x + d * Gamma.avg z)).sub hmean
  simp only [neg_zero, add_zero] at hf
  apply hf.congr'
  filter_upwards [self_mem_nhdsWithin] with q (hqpos : 0 < q)
  have hk := mul_pos hd hqpos
  have hg : log (Real.Gamma (d * q + 1)) = log d + log q + log (Real.Gamma (d * q)) := by
    rw [Real.Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne',
      log_mul hd.ne' hqpos.ne']
  rw [held_form, log_inv, show d * q * q⁻¹ = d by field_simp]
  change _ + -log (Real.Gamma (d * q + 1)) - _ = _
  linarith only [hg]

/-- The centered held-scale slice has a unique maximizer whenever at least
one observation differs from the held scale. -/
theorem held_scale_centered_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hne : ∃ i, x i ≠ a)
    (hmean : Gamma.avg (fun i ↦ log (x i) - log a) = 0) :
    ∃! q : ℝ × ℝ, q ∈ Gamma.domain ∧
      IsMaxOn (fun r ↦ logLik x r.1 r.2 a) Gamma.domain q := by
  obtain ⟨k, ⟨hk, hkm⟩, huk⟩ := Gamma.heldRate (fun _ : Fin n ↦ 1) one_pos
  obtain ⟨p, ⟨hp, hpm⟩, hup⟩ := heldScaleShape hn hx hk ha hne
  have hf (k p : ℝ) : logLik x k p a =
      log p - Gamma.avg (fun i ↦ exp (p * (log (x i) - log a))) -
        Gamma.avgLog x - log (Real.Gamma k) := by
    rw [held_form, hmean]
    ring
  have hg (k : ℝ) : Gamma.logLik (fun _ : Fin n ↦ 1) k 1 = -log (Real.Gamma k) - 1 := by
    rw [Gamma.logLik_eq_avg hn]
    simp [Gamma.logDensity, avg_const hn]
  have hcompare (q r : ℝ × ℝ) :
      logLik x q.1 q.2 a ≤ logLik x r.1 q.2 a ↔
        Gamma.logLik (fun _ : Fin n ↦ 1) q.1 1 ≤
          Gamma.logLik (fun _ : Fin n ↦ 1) r.1 1 := by
    rw [hf, hf, hg, hg]
    constructor <;> intro h <;> linarith only [h]
  refine ⟨(k, p), ⟨⟨hk, hp⟩, ?_⟩, ?_⟩
  · intro q hq
    exact ((hcompare q (k, p)).2 (hkm hq.1)).trans (hpm hq.2)
  · rintro ⟨k', p'⟩ ⟨⟨hk', hp'⟩, hm⟩
    have hks : IsMaxOn (fun t ↦ Gamma.logLik (fun _ : Fin n ↦ 1) t 1) (Ioi 0) k' := by
      intro j hj
      exact (hcompare (j, p') (k', p')).1
        (isMaxOn_iff.mp hm (j, p') ⟨hj, hp'⟩)
    have he := huk k' ⟨hk', hks⟩
    subst k'
    have hps : IsMaxOn (fun t ↦ logLik x k t a) (Ioi 0) p' :=
      fun j hj ↦ isMaxOn_iff.mp hm (k, j) ⟨hk, hj⟩
    exact Prod.ext rfl (hup p' ⟨hp', hps⟩)


private theorem gamma_negative_chord :
    ∃ C > 0, ∀ q : ℝ, 0 ≤ q → q ≤ 1 / 2 →
      log (Real.Gamma (1 + q)) ≤ -(C * q) := by
  have hg : Real.Gamma (1 + 1 / 2) < 1 := by
    rw [add_comm, Real.Gamma_add_one (by norm_num : (1 / 2 : ℝ) ≠ 0),
      Real.Gamma_one_half_eq]
    have h : sqrt π < 2 := (Real.sqrt_lt' (by norm_num : (0 : ℝ) < 2)).2
      (by norm_num; exact pi_lt_four)
    linarith
  have hl : log (Real.Gamma (1 + 1 / 2)) < 0 :=
    log_neg (Gamma_pos_of_pos (by norm_num)) hg
  refine ⟨-2 * log (Real.Gamma (1 + 1 / 2)), by linarith, ?_⟩
  intro q hq hq'
  have h := convexOn_log_Gamma.2
    (show (1 : ℝ) ∈ Ioi 0 from one_pos)
    (show (1 + 1 / 2 : ℝ) ∈ Ioi 0 by norm_num)
    (show 0 ≤ 1 - 2 * q by linarith)
    (show 0 ≤ 2 * q by positivity)
    (show 1 - 2 * q + 2 * q = 1 by ring)
  have he : (1 - 2 * q) * (1 : ℝ) + (2 * q) * (1 + 1 / 2) = 1 + q := by ring
  simp only [smul_eq_mul, Function.comp_apply, Real.Gamma_one, log_one, mul_zero,
    zero_add, he] at h
  convert h using 1
  ring

/-- Each positive-rate power trace at a held endpoint supporting the
whole sample lies strictly below a feasible point, with contacts included. -/
theorem held_scale_power_trace_beaten {x : Fin n → ℝ} {a d : ℝ}
    (_ha : 0 < a) (hd : 0 < d) (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ a) :
    ∃ k > 0, ∃ p > 0,
      log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0) < logLik x k p a := by
  let z : Fin n → ℝ := fun i ↦ log (x i) - log a
  let t : Fin n → ℝ := fun i ↦ if x i = a then 1 else 0
  let E (q : ℝ) : ℝ := Gamma.avg (fun i ↦ (exp (q⁻¹ * z i) - t i) / q)
  have hpoint (i : Fin n) :
      Tendsto (fun q ↦ (exp (q⁻¹ * z i) - t i) / q) (𝓝[>] 0) (𝓝 0) := by
    by_cases he : x i = a
    · simp [he, z, t]
    · have hz : z i < 0 := sub_neg.mpr (log_lt_log (hx i) ((hle i).lt_of_ne he))
      have h := (tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero 1 (-z i)
        (neg_pos.mpr hz)).comp (tendsto_inv_nhdsGT_zero :
          Tendsto (fun q : ℝ ↦ q⁻¹) (𝓝[>] 0) atTop)
      apply h.congr
      intro q
      simp only [Function.comp_apply, rpow_one, neg_neg, t, ite_eq_right he, sub_zero,
        div_eq_mul_inv]
      rw [mul_comm (z i) q⁻¹, mul_comm]
  have hE : Tendsto E (𝓝[>] 0) (𝓝 0) := by
    have h := (tendsto_finsetSum Finset.univ (fun i _ ↦ hpoint i)).div_const (n : ℝ)
    simpa only [E, Gamma.avg, Finset.sum_const_zero, zero_div] using h
  obtain ⟨C, hC, hΓ⟩ := gamma_negative_chord
  have hs : 0 < 1 / (2 * d) := by positivity
  have hlt : ∀ᶠ q : ℝ in 𝓝[>] 0, q < 1 / (2 * d) :=
    mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hs)
  obtain ⟨q, hq, hlt, hsmall⟩ := ((show ∀ᶠ q : ℝ in 𝓝[>] 0, 0 < q from
      self_mem_nhdsWithin).and (hlt.and
      (hE.eventually (eventually_lt_nhds (mul_pos hC hd))))).exists
  have hk := mul_pos hd hq
  refine ⟨d * q, hk, q⁻¹, inv_pos.mpr hq, ?_⟩
  have hb : d * q ≤ 1 / 2 := by
    have h := (lt_div_iff₀ (by positivity : 0 < 2 * d)).1 hlt
    linarith
  have hc := hΓ (d * q) hk.le hb
  have hrec : log (Real.Gamma (1 + d * q)) = log d + log q + log (Real.Gamma (d * q)) := by
    rw [add_comm, Real.Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne',
      log_mul hd.ne' hq.ne']
  have hsum : E q * q =
      Gamma.avg (fun i ↦ exp (q⁻¹ * z i)) - Gamma.avg t := by
    simp only [E, Gamma.avg, ← Finset.sum_div, Finset.sum_sub_distrib]
    ring_nf
    field_simp
  rw [held_form, log_inv, show d * q * q⁻¹ = d by field_simp]
  change _ - Gamma.avg t < _ - Gamma.avg (fun i ↦ exp (q⁻¹ * z i)) + _
  nlinarith only [hrec, hc, hsum, mul_lt_mul_of_pos_right hsmall hq]


/-- The constant sample at or above a positive held scale is unbounded
along a path with power increasing and gamma shape chosen at the mode. -/
theorem held_scale_constant_concentration (hn : 0 < n) {v a : ℝ}
    (ha : 0 < a) (hva : a ≤ v) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (exp (p * (log v - log a))) p a)
      atTop atTop := by
  let z := log v - log a
  have hform (p : ℝ) : logLik (fun _ : Fin n ↦ v) (exp (p * z)) p a =
      log p - log v + Gamma.logLik (fun _ : Fin n ↦ 1) (exp (p * z)) (exp (p * z)) := by
    simp only [logLik, Gamma.avgLog, avg_const hn, loss, Gamma.logLik, log_exp]
    norm_num
    ring
  rcases hva.eq_or_lt with rfl | hv
  · change Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ a) (exp (p * z)) p a) atTop atTop
    simp_rw [hform]
    simp only [show z = 0 by dsimp [z]; ring, mul_zero, exp_zero, sub_eq_add_neg]
    exact tendsto_atTop_add_const_right _ _
      (tendsto_atTop_add_const_right _ _ tendsto_log_atTop)
  · have hz : 0 < z := sub_pos.mpr (log_lt_log ha hv)
    have hex := tendsto_exp_atTop.comp (tendsto_id.atTop_mul_const hz)
    have hgam := (Gamma.constant_unbounded (n := n) hn one_pos).comp hex
    simp only [div_one] at hgam
    have hlim := tendsto_atTop_add_const_right _ (-log v) hgam
    apply tendsto_atTop_mono' _ ?_ hlim
    filter_upwards [eventually_ge_atTop (1 : ℝ)] with p hp
    rw [hform]
    dsimp only [Function.comp_apply, id_eq]
    linarith only [log_nonneg hp]

end

end UnivariateML.Shape2TrGamma

end
