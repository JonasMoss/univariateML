module

public import UnivariateML.Family.Misc
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Gompertz held paths

The limits are on the log-likelihood scale. The growth-zero limit is the
log likelihood of the exponential law at the supplied initial hazard.
-/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.HeldSweepGompertz

open Gompertz

/-- At held growth, the likelihood vanishes as initial hazard tends to zero. -/
theorem held_growth_hazard_zero {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (b : ℝ) :
    Tendsto (fun a ↦ logLik x a b) (𝓝[>] (0 : ℝ)) atBot := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hl : Tendsto (fun a : ℝ ↦ (n : ℝ) * log a) (𝓝[>] 0) atBot :=
    tendsto_log_nhdsGT_zero.const_mul_atBot hn'
  have ht : Tendsto (fun a : ℝ ↦ b * sampleSum x - a * integrated x b)
      (𝓝[>] 0) (𝓝 (b * sampleSum x)) := by
    have hc : Continuous (fun a : ℝ ↦ b * sampleSum x - a * integrated x b) := by fun_prop
    have h := hc.continuousAt.continuousWithinAt (s := Ioi 0) (x := (0 : ℝ))
    change Tendsto _ _ (𝓝 (b * sampleSum x - 0 * integrated x b)) at h
    simpa only [zero_mul, sub_zero] using h
  have he (a : ℝ) : logLik x a b =
      (n : ℝ) * log a + (b * sampleSum x - a * integrated x b) := by
    unfold logLik
    ring
  simpa only [he] using hl.atBot_add ht

/-- At held growth, positive integrated hazard makes the likelihood vanish
as initial hazard diverges. -/
theorem held_growth_hazard_infty {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (h : 0 < integrated x b) : Tendsto (fun a ↦ logLik x a b) atTop atBot := by
  have hl : Tendsto (fun a : ℝ ↦ (n : ℝ) * (log a / a)) atTop (𝓝 0) := by
    have hlog : Tendsto (fun a : ℝ ↦ log a / a) atTop (𝓝 0) :=
      (tendsto_pow_log_div_mul_add_atTop (1 : ℝ) 0 1 one_ne_zero).congr (fun a ↦ by simp)
    simpa only [mul_zero] using hlog.const_mul (n : ℝ)
  have ht : ∀ᶠ a : ℝ in atTop, (n : ℝ) * (log a / a) < integrated x b / 2 :=
    hl.eventually (Iio_mem_nhds (by positivity))
  have hu : Tendsto (fun a : ℝ ↦ b * sampleSum x + a * (-(integrated x b / 2))) atTop atBot :=
    tendsto_atBot_add_const_left _ _ (tendsto_id.atTop_mul_const_of_neg (by linarith))
  refine tendsto_atBot_mono' _ ?_ hu
  filter_upwards [ht, eventually_gt_atTop (0 : ℝ)] with a ha hap
  have hh := mul_lt_mul_of_pos_right ha hap
  have he : ((n : ℝ) * (log a / a)) * a = (n : ℝ) * log a := by field_simp
  rw [he] at hh
  unfold logLik
  linarith

/-- The held-hazard growth-zero limit has the exponential log likelihood
at exactly the held hazard. -/
theorem held_hazard_exponential_path {n : ℕ} (x : Fin n → ℝ) (a : ℝ) :
    Tendsto (logLik x a) (𝓝[>] (0 : ℝ))
      (𝓝 ((n : ℝ) * log a - a * sampleSum x)) := by
  have h := (hasDerivAt_integrated_zero x).continuousAt.continuousWithinAt (s := Ioi 0)
  change Tendsto (integrated x) (𝓝[>] 0) (𝓝 (integrated x 0)) at h
  rw [integrated_zero] at h
  have hv : Tendsto (fun b : ℝ ↦ b * sampleSum x) (𝓝[>] 0) (𝓝 0) := by
    have hb : Tendsto id (𝓝[>] (0 : ℝ)) (𝓝 0) :=
      tendsto_nhdsWithin_of_tendsto_nhds tendsto_id
    simpa only [id_eq, zero_mul] using hb.mul_const (sampleSum x)
  change Tendsto (fun b : ℝ ↦ (n : ℝ) * log a + b * sampleSum x - a * integrated x b) _ _
  simpa only [add_zero] using
    (tendsto_const_nhds (x := (n : ℝ) * log a)).add hv |>.sub (h.const_mul a)

/-- At held positive hazard, growth going to infinity gives zero
likelihood whenever a nonnegative sample contains a positive observation. -/
theorem held_hazard_growth_infty {n : ℕ} (x : Fin n → ℝ)
    (hx : ∀ i, 0 ≤ x i) (hpos : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    Tendsto (logLik x a) atTop atBot := by
  obtain ⟨i, hi⟩ := hpos
  have ht : Tendsto (fun b : ℝ ↦ exp (b * x i) / b^2) atTop atTop := by
    simpa only [rpow_two, mul_comm] using tendsto_exp_mul_div_rpow_atTop 2 (x i) hi
  let C := (sampleSum x + 1) / a
  have ht' := ht.eventually_ge_atTop (C + 1)
  have hbound : ∀ᶠ b : ℝ in atTop, logLik x a b ≤ (n : ℝ) * log a - b := by
    filter_upwards [ht', eventually_ge_atTop (1 : ℝ)] with b hb hb1
    have hp : 0 < b := zero_lt_one.trans_le hb1
    have hsum : (exp (b * x i) - 1) / b ≤ integrated x b := by
      rw [integrated_eq x hp.ne']
      exact Finset.single_le_sum (fun j _ ↦ div_nonneg
        (sub_nonneg.mpr (one_le_exp_iff.mpr (mul_nonneg hp.le (hx j)))) hp.le) (Finset.mem_univ i)
    have hc : C * b ≤ (exp (b * x i) - 1) / b := by
      have he := (le_div_iff₀ (pow_pos hp 2)).mp hb
      have hb2 : 1 ≤ b^2 := by nlinarith
      apply (le_div_iff₀ hp).mpr
      nlinarith only [he, hb2]
    have hmul := mul_le_mul_of_nonneg_left (hc.trans hsum) ha.le
    have he : a * (C * b) = b * (sampleSum x + 1) := by
      dsimp [C]
      field_simp
    rw [he] at hmul
    unfold logLik
    linarith
  apply tendsto_atBot_mono' _ hbound
  change Tendsto (fun b : ℝ ↦ (n : ℝ) * log a - b) atTop atBot
  simpa only [sub_eq_add_neg] using
    tendsto_atBot_add_const_left atTop ((n : ℝ) * log a) tendsto_neg_atTop_atBot

end UnivariateML.HeldSweepGompertz

end
