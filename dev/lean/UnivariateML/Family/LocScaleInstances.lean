module

public import UnivariateML.Generic.LocationScale
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Logistic, Gumbel, normal and Laplace likelihoods

These are the joint fits with location and positive scale free. Additive
normalizing constants are omitted for normal and Laplace. The Laplace
application gives concavity and attainment. The description by medians is
outside the scope of this module.

## Deviations

The normal dossier also gives explicit estimators. The four dossiers cover
held coefficients and implementation behavior beyond the joint-fit claims
here. The logistic dossier denotes the transformed coordinates in the
opposite order and uses an equivalent hyperbolic cosine expression.
-/

@[expose] public section

open Set Real UnivariateML.LocationScale

namespace UnivariateML.LocScaleInstances

noncomputable section

variable {n : ℕ}

def logisticLoss (t : ℝ) : ℝ := t + 2 * log (1 + exp (-t))

def gumbelLoss (t : ℝ) : ℝ := t + exp (-t)

def normalLoss (t : ℝ) : ℝ := t ^ 2 / 2

def laplaceLoss (t : ℝ) : ℝ := |t|

private theorem negExp_deriv (t : ℝ) :
    HasDerivAt (fun z : ℝ ↦ exp (-z)) (-exp (-t)) t := by
  simpa using ((hasDerivAt_id t).neg.exp)

private theorem logistic_deriv (t : ℝ) :
    HasDerivAt logisticLoss (1 - 2 / (1 + exp t)) t := by
  have h := (hasDerivAt_id t).add
    ((((negExp_deriv t).const_add 1).log (by positivity)).const_mul 2)
  change HasDerivAt logisticLoss (1 + 2 * (-exp (-t) / (1 + exp (-t)))) t at h
  rw [exp_neg] at h
  convert h using 1
  have hp : 0 < exp t := exp_pos _
  have hq : 0 < 1 + exp t := by positivity
  have hr : 0 < 1 + (exp t)⁻¹ := by positivity
  field_simp [hp.ne', hq.ne', hr.ne']
  ring

private theorem logistic_strict : StrictConvexOn ℝ univ logisticLoss := by
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (logistic_deriv t).continuousAt)
  intro x y hxy
  rw [(logistic_deriv x).deriv, (logistic_deriv y).deriv]
  have h := div_lt_div_of_pos_left (show (0 : ℝ) < 2 by norm_num)
    (show 0 < 1 + exp x by positivity)
    (show 1 + exp x < 1 + exp y by linarith only [exp_lt_exp.mpr hxy])
  linarith

private theorem logistic_tail : LinearLowerBound logisticLoss := by
  refine ⟨1, zero_lt_one, 0, fun t ↦ ?_⟩
  simp only [one_mul, sub_zero, logisticLoss]
  by_cases ht : 0 ≤ t
  · rw [abs_of_nonneg ht]
    have h := log_nonneg (show 1 ≤ 1 + exp (-t) by linarith only [exp_pos (-t)])
    linarith
  · rw [abs_of_neg (lt_of_not_ge ht)]
    have h := log_le_log (exp_pos (-t)) (show exp (-t) ≤ 1 + exp (-t) by linarith)
    rw [log_exp] at h
    linarith

private theorem gumbel_deriv (t : ℝ) : HasDerivAt gumbelLoss (1 - exp (-t)) t := by
  rw [sub_eq_add_neg]
  exact (hasDerivAt_id t).add (negExp_deriv t)

private theorem gumbel_strict : StrictConvexOn ℝ univ gumbelLoss := by
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (gumbel_deriv t).continuousAt)
  intro x y hxy
  rw [(gumbel_deriv x).deriv, (gumbel_deriv y).deriv]
  exact sub_lt_sub_left (exp_lt_exp.mpr (neg_lt_neg hxy)) 1

private theorem gumbel_tail : LinearLowerBound gumbelLoss := by
  refine ⟨1, zero_lt_one, 0, fun t ↦ ?_⟩
  simp only [one_mul, sub_zero, gumbelLoss]
  by_cases ht : 0 ≤ t
  · rw [abs_of_nonneg ht]
    linarith [exp_pos (-t)]
  · rw [abs_of_neg (lt_of_not_ge ht)]
    have h : 1 + -t / 2 ≤ exp (-t / 2) := by
      linarith [add_one_le_exp (-t / 2)]
    have hsq := pow_le_pow_left₀ (show 0 ≤ 1 + -t / 2 by linarith) h 2
    rw [← exp_nat_mul] at hsq
    have he : (2 : ℝ) * (-t / 2) = -t := by ring
    norm_num only [Nat.cast_ofNat] at hsq
    rw [he] at hsq
    nlinarith [sq_nonneg (-t / 2 - 1)]

private theorem normal_strict : StrictConvexOn ℝ univ normalLoss := by
  have hd (t : ℝ) : HasDerivAt normalLoss t t := by
    change HasDerivAt (fun z : ℝ ↦ z ^ 2 / 2) t t
    convert ((hasDerivAt_id t).pow 2).div_const 2 using 1 <;> simp
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr fun t ↦ (hd t).continuousAt)
  intro x y hxy
  rwa [(hd x).deriv, (hd y).deriv]

private theorem normal_tail : LinearLowerBound normalLoss := by
  refine ⟨1, zero_lt_one, 1 / 2, fun t ↦ ?_⟩
  unfold normalLoss
  simp only [one_mul]
  nlinarith [sq_nonneg (|t| - 1), sq_abs t]

private theorem laplace_convex : ConvexOn ℝ univ laplaceLoss := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ a b ha hb _
  simp only [laplaceLoss, smul_eq_mul]
  calc
    |a * x + b * y| ≤ |a * x| + |b * y| := abs_add_le _ _
    _ = a * |x| + b * |y| := by rw [abs_mul, abs_mul, abs_of_nonneg ha, abs_of_nonneg hb]

private theorem laplace_tail : LinearLowerBound laplaceLoss :=
  ⟨1, zero_lt_one, 0, fun t ↦ by simp [laplaceLoss]⟩

/-- The joint logistic MLE exists and is unique for nonconstant samples. -/
theorem logistic_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik logisticLoss x) domain p :=
  existsUnique_mle logistic_strict logistic_tail hx

/-- The joint Gumbel MLE exists and is unique for nonconstant samples. -/
theorem gumbel_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik gumbelLoss x) domain p :=
  existsUnique_mle gumbel_strict gumbel_tail hx

/-- The joint normal MLE exists and is unique for nonconstant samples. -/
theorem normal_existsUnique_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃! p, p ∈ domain ∧ IsMaxOn (logLik normalLoss x) domain p :=
  existsUnique_mle normal_strict normal_tail hx

/-- The Laplace log likelihood is concave in inverse-scale coordinates. -/
theorem laplace_concave (x : Fin n → ℝ) :
    ConcaveOn ℝ inverseDomain (inverseLogLik laplaceLoss x) :=
  concave_inverseLogLik laplace_convex x

/-- The joint Laplace MLE exists for nonconstant samples. -/
theorem laplace_exists_mle {x : Fin n → ℝ} (hx : Nonconstant x) :
    ∃ p ∈ domain, IsMaxOn (logLik laplaceLoss x) domain p :=
  exists_mle laplace_convex laplace_tail hx

end

end UnivariateML.LocScaleInstances
