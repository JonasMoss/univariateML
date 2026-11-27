module

public import UnivariateML.Family.LocScaleTukeyLambda

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Held Tukey-lambda uniform branches

At shape one or two and fixed positive scale, the density is constant
throughout the closed support, including its endpoints.

## Deviations

These theorems
describe that part of the held-shape and held-scale claim.
-/

@[expose] public section

open Set Real
open UnivariateML.LocScaleTukeyLambda

namespace UnivariateML.AE1TukeyLambda

private theorem uniform_weight (p : ℝ) {lam : ℝ} (hlam : lam = 1 ∨ lam = 2) :
    weight lam p = lam / 2 := by
  rcases hlam with rfl | rfl
  · norm_num [weight]
  · unfold weight
    rw [ite_eq_right (by norm_num), ite_eq_right (by norm_num), show (2 : ℝ) - 1 = 1 by norm_num,
      rpow_one, rpow_one]
    rw [show p + (1-p) = 1 by ring]
    norm_num

private theorem uniform_quantile (p : ℝ) {lam : ℝ} (hlam : lam = 1 ∨ lam = 2) :
    quantile lam p = (2*p - 1) / lam := by
  rcases hlam with rfl | rfl
  · simp only [quantile, rpow_one]
    ring
  · simp only [quantile, rpow_two]
    ring

/-- With uniform shape one or two and positive scale held, the entire
maximizing location set is the intersection of the closed support intervals. -/
theorem held_uniform_shape_scale_locations {n : ℕ} (x : Fin n → ℝ)
    {lam s : ℝ} (hlam : lam = 1 ∨ lam = 2) (hs : 0 < s) (m : ℝ) :
    (∃ p : Fin n → ℝ, (m, p) ∈ heldScaleGraph x lam s ∧
      IsMaxOn (graphLikelihood lam s) (heldScaleGraph x lam s) (m, p)) ↔
      ∀ j, m ∈ Icc (x j - s / lam) (x j + s / lam) := by
  have hp : 0 < lam := by rcases hlam with rfl | rfl <;> norm_num
  have hloc := held_positive_shape_scale_location_graph x hp hs m
  constructor
  · rintro ⟨p, hp', _⟩ j
    have hq := (hp' j).2
    rw [uniform_quantile _ hlam] at hq
    have hb := (hp' j).1
    have h₁ : -(s / lam) ≤ s * ((2 * p j - 1) / lam) := by
      rw [← mul_div_assoc, ← neg_div]
      exact div_le_div_of_nonneg_right (by nlinarith [hb.1]) hp.le
    have h₂ : s * ((2 * p j - 1) / lam) ≤ s / lam := by
      rw [← mul_div_assoc]
      exact div_le_div_of_nonneg_right (by nlinarith [hb.2]) hp.le
    exact ⟨by linarith, by linarith⟩
  · intro hm
    have habs (j : Fin n) : |x j - m| ≤ s / lam :=
      abs_le.mpr ⟨by linarith [(hm j).2], by linarith [(hm j).1]⟩
    obtain ⟨p, hp', _⟩ := hloc.mpr habs
    refine ⟨p, hp', fun q _ ↦ ?_⟩
    simp only [graphLikelihood, uniform_weight _ hlam, mem_ofPred_eq, le_refl]

end UnivariateML.AE1TukeyLambda

end
