module

public import UnivariateML.Family.DoublePareto
public import UnivariateML.Generic.Score
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-! # Double Pareto with held kink and one held rate -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.HeldSweepDoublePareto

open DoublePareto

private theorem swap_rates (L R a b : ℝ) :
    objective L R a b = objective R L b a := by
  unfold objective
  rw [mul_comm a b, add_comm a b]
  ring

/-- At a held kink and positive left rate, positive right deviation gives
a unique finite right-rate maximum. -/
theorem held_left_rate_unique {L R a : ℝ} (ha : 0 < a) (hR : 0 < R) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective L R a) (Ioi 0) b := by
  let g := fun b : ℝ ↦ a / (b * (a + b)) - R
  let b₀ := (sqrt (a^2 + 4 * a / R) - a) / 2
  have h₀ : 0 < b₀ := by
    have h : a < sqrt (a^2 + 4 * a / R) := by
      calc
        a = sqrt (a^2) := (sqrt_sq ha.le).symm
        _ < _ := sqrt_lt_sqrt (sq_nonneg _) (lt_add_of_pos_right _ (by positivity))
    exact half_pos (sub_pos.mpr h)
  have hroot : g b₀ = 0 := by
    have hsq := sq_sqrt (show 0 ≤ a^2 + 4 * a / R by positivity)
    have hb : b₀ * (a + b₀) = a / R := by
      dsimp [b₀]
      linear_combination hsq / 4
    dsimp [g]
    rw [hb]
    field_simp
    ring
  have hd (b : ℝ) (hb : b ∈ Ioi (0 : ℝ)) : HasDerivAt (objective L R a) (g b) b := by
    have hb' : 0 < b := hb
    have hab : 0 < a + b := add_pos ha hb'
    have haux : objective L R a =ᶠ[𝓝 b]
        (fun t ↦ log a + log t - log (a + t) - a * L - t * R) := by
      filter_upwards [Ioi_mem_nhds hb'] with t (ht : 0 < t)
      unfold objective
      rw [log_div (mul_pos ha ht).ne' (add_pos ha ht).ne', log_mul ha.ne' ht.ne']
    apply HasDerivAt.congr_of_eventuallyEq _ haux
    have h := (((((hasDerivAt_id b).log hb'.ne').const_add (log a)).sub
      (((hasDerivAt_id b).const_add a).log hab.ne')).sub_const (a * L)).sub
      ((hasDerivAt_id b).mul_const R)
    convert h using 1
    · ext t
      simp only [Pi.sub_apply, id_eq]
    · simp only [g, id_eq, one_mul]
      field_simp [hb'.ne', hab.ne']
      ring
  have hc : ContinuousOn g (Ioi 0) := by
    unfold g
    apply ContinuousOn.sub _ continuousOn_const
    exact continuousOn_const.div (by fun_prop) (fun b hb ↦
      (mul_pos (show 0 < b from hb) (add_pos ha (show 0 < b from hb))).ne')
  have hs : StrictAntiOn g (Ioi 0) := by
    intro b hb c hc hbc
    have hb' : 0 < b := hb
    have hc' : 0 < c := hc
    unfold g
    apply sub_lt_sub_right
    exact div_lt_div_of_pos_left ha (mul_pos hb' (add_pos ha hb'))
      (mul_lt_mul hbc (add_lt_add_right hbc a).le (add_pos ha hb') hc'.le)
  have h₁ : b₀ / 2 ∈ Ioi (0 : ℝ) := half_pos h₀
  have h₂ : 2 * b₀ ∈ Ioi (0 : ℝ) := by
    change 0 < 2 * b₀
    positivity
  have hb₀ : b₀ ∈ Ioi (0 : ℝ) := h₀
  have ha' := hs h₁ hb₀ (half_lt_self h₀)
  have hb' := hs hb₀ h₂ (by linarith)
  rw [hroot] at ha' hb'
  obtain ⟨⟨b, hb, hu⟩, hf⟩ := Generic.existsUnique_score_zero_of_signs
    (convex_Ioi 0) hd hc hs h₁ h₂ (by linarith) ha' hb'
  exact ⟨b, ⟨hb.1, (hf b hb.1).mpr hb.2⟩, fun t ht ↦ hu t ⟨ht.1, (hf t ht.1).mp ht.2⟩⟩

/-- At zero right deviation the fixed-left-rate power face is the strictly
unattained supremum, reached by diverging right rate. -/
theorem held_left_rate_zero_deviation {L a : ℝ} (ha : 0 < a) :
    Tendsto (objective L 0 a) atTop (𝓝 (log a - a * L)) ∧
      IsLUB (objective L 0 a '' Ioi 0) (log a - a * L) ∧
      ∀ b > 0, objective L 0 a b < log a - a * L := by
  have hlt (b : ℝ) (hb : 0 < b) : objective L 0 a b < log a - a * L := by
    unfold objective
    rw [mul_zero, sub_zero]
    apply sub_lt_sub_right
    exact log_lt_log (by positivity) ((div_lt_iff₀ (by positivity)).mpr (by nlinarith))
  have hd : Tendsto (fun b : ℝ ↦ 1 + a * b⁻¹) atTop (𝓝 1) := by
    simpa only [mul_zero, add_zero] using
      (tendsto_const_nhds (x := (1 : ℝ))).add (tendsto_inv_atTop_zero.const_mul a)
  have hn : Tendsto (fun b : ℝ ↦ a / (1 + a * b⁻¹)) atTop (𝓝 a) := by
    simpa only [Pi.div_def, div_one] using (tendsto_const_nhds (x := a)).div hd one_ne_zero
  have ht : Tendsto (objective L 0 a) atTop (𝓝 (log a - a * L)) := by
    apply (((continuousAt_log ha.ne').tendsto.comp hn).sub_const (a * L)).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    have he : a / (1 + a * b⁻¹) = a * b / (a + b) := by
      field_simp [hb.ne', (add_pos ha hb).ne', show 1 + a * b⁻¹ ≠ 0 by positivity]
      ring
    simp [he, objective]
  refine ⟨ht, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨b, hb, rfl⟩
    exact (hlt b hb).le
  · intro c hc
    apply le_of_tendsto ht
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    exact hc ⟨b, hb, rfl⟩

/-- The matching held-right-rate result follows by swapping the deviations. -/
theorem held_right_rate_unique {L R b : ℝ} (hb : 0 < b) (hL : 0 < L) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun r ↦ objective L R r b) (Ioi 0) a := by
  simp_rw [swap_rates L R]
  exact held_left_rate_unique hb hL

/-- At zero left deviation, the held-right-rate Pareto face is a strictly
unattained supremum. -/
theorem held_right_rate_zero_deviation {R b : ℝ} (hb : 0 < b) :
    Tendsto (fun a ↦ objective 0 R a b) atTop (𝓝 (log b - b * R)) ∧
      IsLUB ((fun a ↦ objective 0 R a b) '' Ioi 0) (log b - b * R) ∧
      ∀ a > 0, objective 0 R a b < log b - b * R := by
  simp_rw [swap_rates 0 R]
  exact held_left_rate_zero_deviation hb

end UnivariateML.HeldSweepDoublePareto

end
