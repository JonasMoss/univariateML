module

public import UnivariateML.Family.DiscreteWeibull

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Discrete Weibull held-fiber boundary laws

Each limit is pointwise at every count. At vanishing shape the pointwise
limit is a subprobability, with the remaining mass escaping to infinity.
-/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.HeldSweepDWeibull

open DiscreteWeibull

private theorem survival_shape_zero (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ survival k (a, b)) (𝓝[>] (0 : ℝ))
      (𝓝 (if k = 0 then 1 else exp (-exp a))) := by
  by_cases hk : k = 0
  · simp [survival, hk]
  · rw [ite_eq_right hk]
    have hc : Continuous (fun b : ℝ ↦ exp (-exp (a + b * log (k : ℝ)))) := by fun_prop
    have ht : Tendsto (fun b : ℝ ↦ exp (-exp (a + b * log (k : ℝ))))
        (𝓝[>] 0) (𝓝 (exp (-exp (a + 0 * log (k : ℝ))))) := hc.continuousAt.continuousWithinAt
    simpa only [survival, hk, ite_false, endpoint, zero_mul, add_zero] using ht

private theorem survival_shape_top (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ survival k (a, b)) atTop
      (𝓝 (if k = 0 then 1 else if k = 1 then exp (-exp a) else 0)) := by
  by_cases h : k ≤ 1
  · interval_cases k <;> simp [survival, endpoint]
  · have hk0 : k ≠ 0 := by omega
    have hk1 : k ≠ 1 := by omega
    have hk : (1 : ℝ) < k := by exact_mod_cast (show 1 < k by omega)
    have ht := tendsto_atTop_add_const_left atTop a
      (tendsto_id.atTop_mul_const (log_pos hk))
    simpa only [survival, hk0, hk1, ite_false, endpoint, id_eq, Function.comp_def] using
      tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_exp_atTop.comp ht))

/-- With probability held, the vanishing-shape limit puts the zero-cell
mass at zero and sends all remaining mass to infinity. -/
theorem held_probability_shape_zero (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ mass k (a, b)) (𝓝[>] (0 : ℝ))
      (𝓝 (if k = 0 then 1 - exp (-exp a) else 0)) := by
  have h := (survival_shape_zero a k).sub (survival_shape_zero a (k + 1))
  by_cases hk : k = 0 <;> simpa [mass, hk] using h

/-- With probability held, large shape gives the Bernoulli law whose
probability of one is exactly the supplied survival at one. -/
theorem held_probability_shape_top (a : ℝ) (k : ℕ) :
    Tendsto (fun b ↦ mass k (a, b)) atTop
      (𝓝 (if k = 0 then 1 - exp (-exp a) else if k = 1 then exp (-exp a) else 0)) := by
  have h := (survival_shape_top a k).sub (survival_shape_top a (k + 1))
  by_cases hk : k ≤ 1
  · interval_cases k <;> simpa [mass] using h
  · have h₀ : k ≠ 0 := by omega
    have h₁ : k ≠ 1 := by omega
    have h₂ : k + 1 ≠ 1 := by omega
    simpa [mass, h₀, h₁, h₂] using h

private theorem survival_probability_zero (b : ℝ) {k : ℕ} (hk : k ≠ 0) :
    Tendsto (fun a ↦ survival k (a, b)) atTop (𝓝 0) := by
  have ht := tendsto_atTop_add_const_right atTop (b * log (k : ℝ)) tendsto_id
  simpa only [survival, hk, ite_false, endpoint, id_eq, Function.comp_def] using
    tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_exp_atTop.comp ht))

private theorem survival_probability_one (b : ℝ) (k : ℕ) :
    Tendsto (fun a ↦ survival k (a, b)) atBot (𝓝 1) := by
  by_cases hk : k = 0
  · simp [survival, hk]
  · have ht := tendsto_atBot_add_const_right atBot (b * log (k : ℝ)) tendsto_id
    have h := (tendsto_exp_atBot.comp ht).neg
    simpa only [survival, hk, ite_false, endpoint, id_eq, Function.comp_def, neg_zero, exp_zero]
      using continuous_exp.continuousAt.tendsto.comp h

/-- With shape held, sending probability to zero gives the zero atom.
Probability goes to zero when log negative log probability goes to infinity. -/
theorem held_shape_probability_zero (b : ℝ) (k : ℕ) :
    Tendsto (fun a ↦ mass k (a, b)) atTop (𝓝 (if k = 0 then 1 else 0)) := by
  have h := survival_probability_zero b (Nat.succ_ne_zero k)
  by_cases hk : k = 0
  · subst k
    simpa [mass, survival] using tendsto_const_nhds.sub h
  · simpa [mass, hk] using (survival_probability_zero b hk).sub h

/-- With shape held, sending probability to one sends all mass to infinity. -/
theorem held_shape_probability_one (b : ℝ) (k : ℕ) :
    Tendsto (fun a ↦ mass k (a, b)) atBot (𝓝 0) := by
  simpa only [mass, sub_self] using
    (survival_probability_one b k).sub (survival_probability_one b (k + 1))

end UnivariateML.HeldSweepDWeibull

end
