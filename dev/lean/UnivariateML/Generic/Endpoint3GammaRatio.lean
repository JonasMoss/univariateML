module

public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.Convex.Slope

import Mathlib.Tactic

set_option linter.style.header false

/-! # Fixed-increment gamma ratios at infinity -/

@[expose] public section

open Real Filter Set
open scoped Topology

namespace UnivariateML.Endpoint3GammaRatio

private theorem logGamma_step {x : ℝ} (hx : 0 < x) :
    log (Gamma (x + 1)) - log (Gamma x) = log x := by
  rw [Gamma_add_one hx.ne', log_mul hx.ne' (Gamma_pos_of_pos hx).ne']
  ring

/-- The fixed positive increment of log Gamma is asymptotic to the
increment times the log of the argument. The two neighboring unit
secants bound the intermediate secant by log convexity. -/
theorem log_gamma_ratio (p : ℝ) (hp : 0 < p) :
    Tendsto (fun x ↦ log (Gamma (x + p)) - log (Gamma x) - p * log x)
      atTop (𝓝 0) := by
  have hlim (a : ℝ) :
      Tendsto (fun x : ℝ ↦ p * (log (x + a) - log x)) atTop (𝓝 0) := by
    have h₁ : Tendsto (fun x : ℝ ↦ 1 + a * x⁻¹) atTop (𝓝 (1 : ℝ)) := by
      simpa using tendsto_const_nhds.add (tendsto_inv_atTop_zero.const_mul a)
    have h₂ := ((continuousAt_log one_ne_zero).tendsto.comp h₁).const_mul p
    simp only [log_one, mul_zero] at h₂
    apply h₂.congr'
    filter_upwards [eventually_gt_atTop (max 0 (-a))] with x hx
    have hx₀ := (le_max_left 0 (-a)).trans_lt hx
    have hx₁ : 0 < x + a := by linarith [(le_max_right 0 (-a)).trans_lt hx]
    have he : 1 + a * x⁻¹ = (x + a) / x := by field_simp
    change p * log (1 + a * x⁻¹) = p * (log (x + a) - log x)
    rw [he, log_div hx₁.ne' hx₀.ne']
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' (hlim (-1)) (hlim p)
  · filter_upwards [eventually_gt_atTop (1 : ℝ)] with x hx
    have hm : 0 < x - 1 := sub_pos.mpr hx
    have h := convexOn_log_Gamma.slope_mono_adjacent (x := x - 1) (y := x) (z := x + p)
      hm (by linarith : 0 < x + p) (by linarith) (by linarith)
    simp only [Function.comp_apply, sub_sub_cancel, add_sub_cancel_left, div_one] at h
    have he : log (Gamma x) - log (Gamma (x - 1)) = log (x - 1) := by
      simpa only [sub_add_cancel] using logGamma_step hm
    rw [he] at h
    have hh := (le_div_iff₀ hp).mp h
    rw [show x + (-1 : ℝ) = x - 1 by ring]
    linarith
  · filter_upwards [eventually_gt_atTop (1 : ℝ)] with x hx
    have hxp : 0 < x + p := by linarith
    have h := convexOn_log_Gamma.slope_mono_adjacent (x := x) (y := x + p) (z := x + p + 1)
      (by linarith : 0 < x) (by linarith : 0 < x + p + 1) (by linarith) (by linarith)
    simp only [Function.comp_apply, add_sub_cancel_left, div_one, logGamma_step hxp] at h
    have hh := (div_le_iff₀ hp).mp h
    linarith

/-- At zero shape, the gamma normalizer ratio divided by the small
shape has log limit zero. This uses the gamma recurrence at zero, so
it applies to fixed increments of any positive size. -/
theorem log_gamma_ratio_at_zero {p : ℝ} (hp : 0 < p) :
    Tendsto (fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) -
      log q - log (Gamma p)) (𝓝[>] 0) (𝓝 0) := by
  have hcont {x : ℝ} (hx : 0 < x) : ContinuousAt (fun y ↦ log (Gamma y)) x :=
    ((Real.differentiableAt_Gamma (fun m ↦ by
      have hm : (0 : ℝ) ≤ m := Nat.cast_nonneg m
      linarith)).continuousAt).log (Gamma_pos_of_pos hx).ne'
  have h0 : Tendsto (fun q : ℝ ↦ q) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have h1 : Tendsto (fun q : ℝ ↦ q + 1) (𝓝[>] 0) (𝓝 1) := by
    simpa only [zero_add] using h0.add_const 1
  have hpa : Tendsto (fun q : ℝ ↦ p + q) (𝓝[>] 0) (𝓝 p) := by
    simpa only [add_zero] using h0.const_add p
  have h := (((hcont hp).tendsto.comp hpa).sub
    ((hcont one_pos).tendsto.comp h1)).sub_const (log (Gamma p))
  simp only [Gamma_one, log_one, sub_zero, sub_self] at h
  apply h.congr'
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with q hq
  change 0 < q at hq
  change log (Gamma (p + q)) - log (Gamma (q + 1)) - log (Gamma p) = _
  rw [Gamma_add_one hq.ne', log_mul hq.ne' (Gamma_pos_of_pos hq).ne']
  ring

end UnivariateML.Endpoint3GammaRatio

end
