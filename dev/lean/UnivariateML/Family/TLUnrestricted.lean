module

public import UnivariateML.Family.LocScaleTukeyLambda
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Unrestricted Tukey lambda shape

For any nonempty sample, place the location at one observation and let
positive shape and scale tend to infinity proportionally.

## Deviations

This covers the unrestricted-domain clause of the all-free failure claim.
The bounded fitted branch requires a separate escape classification.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TLUnrestricted

open LocScaleTukeyLambda

private theorem weight_gt_one_lower {l p : ℝ} (hl : 1 < l) (hp : p ∈ Icc 0 1) :
    1/2 ≤ weight l p := by
  simp only [weight,not_lt.mpr hl.le,hl.ne',ite_false]
  have hr : 0 < l-1 := sub_pos.mpr hl
  have h₀ := rpow_le_one hp.1 hp.2 hr.le
  have h₁ := rpow_le_one (sub_nonneg.mpr hp.2) (by linarith [hp.1]) hr.le
  have hd : 0 < p^(l-1)+(1-p)^(l-1) := by
    rcases hp.2.eq_or_lt with h | h
    · simp [h,zero_rpow hr.ne']
    · exact add_pos_of_nonneg_of_pos (rpow_nonneg hp.1 _) (rpow_pos_of_pos (sub_pos.mpr h) _)
  exact (le_div_iff₀ hd).mpr (by linarith)

private theorem center_weight {l : ℝ} (hl : 1 < l) :
    weight l (1/2) = exp (l*log 2)/4 := by
  simp only [weight, not_lt.mpr hl.le, hl.ne',ite_false,show (1 : ℝ)-1/2 = 1/2 by norm_num]
  rw [rpow_def_of_pos (by norm_num : (0 : ℝ) < 1/2),
    show log ((1:ℝ)/2) = -log 2 by rw [log_div (by norm_num) (by norm_num),log_one,zero_sub]]
  rw [show -log 2*(l-1) = log 2 - l*log 2 by ring, exp_sub,
    exp_log (by norm_num : (0 : ℝ) < 2)]
  field_simp
  ring

/-- Allowing arbitrarily large positive shapes makes the free likelihood
unbounded on every nonempty sample. The path uses location at an observation
and scale proportional to shape. -/
theorem unrestricted_unbounded {n : ℕ} (x : Fin n → ℝ) (i : Fin n) :
    ∀ B : ℝ, ∃ l > 1, ∃ s > 0, ∃ p : Fin n → ℝ,
      (x i,p) ∈ heldScaleGraph x l s ∧ B < graphLikelihood l s (x i,p) := by
  let A := (∑ j, |x j-x i|) + 1
  have hA : 0 < A := by dsimp [A]; positivity
  have hlog : 0 < log (2 : ℝ) := log_pos (by norm_num)
  let C : ℝ := (log 2)^n / (2*(2*A)^n)
  have hC : 0 < C := by dsimp [C]; positivity
  have ht : Tendsto (fun l : ℝ ↦ C * (exp (l*log 2) / (l*log 2)^(n : ℝ))) atTop atTop :=
    ((tendsto_exp_div_rpow_atTop (n : ℝ)).comp
      (tendsto_id.atTop_mul_const hlog)).const_mul_atTop hC
  intro B
  obtain ⟨l, hl, hB⟩ := ((eventually_gt_atTop (1 : ℝ)).and (ht.eventually_gt_atTop B)).exists
  let s := l*A
  have hl' : 0 < l := zero_lt_one.trans hl
  have hs : 0 < s := mul_pos hl' hA
  have he : s/l = A := by dsimp [s]; field_simp
  have hh (j : Fin n) : |x j-x i| ≤ s/l := by
    rw [he]
    exact (Finset.single_le_sum (f := fun k ↦ |x k-x i|)
      (fun k _ ↦ abs_nonneg _) (Finset.mem_univ j)).trans (by dsimp [A]; linarith)
  obtain ⟨p,hp,_⟩ := (held_positive_shape_scale_location_graph x hl' hs (x i)).mpr hh
  classical
  let q : Fin n → ℝ := fun j ↦ if j = i then 1/2 else p j
  have hq : (x i,q) ∈ heldScaleGraph x l s := by
    intro j
    by_cases hj : j=i
    · subst j
      have hzero : quantile l (1/2) = 0 := by
        unfold quantile
        rw [show (1:ℝ)-1/2=1/2 by norm_num, sub_self,zero_div]
      exact ⟨by change q i ∈ Icc (0 : ℝ) 1; norm_num [q], by
        change x i + s * quantile l (q i) = x i
        rw [show q i = 1/2 by simp [q],hzero,mul_zero,add_zero]⟩
    · simpa [q,hj] using hp j
  have hqi : q i = 1/2 := by simp [q]
  let L (j : Fin n) := (1/(2*s)) * (if j = i then exp (l*log 2)/2 else 1)
  have hsum : (∏ j, L j) ≤ graphLikelihood l s (x i,q) := by
    unfold graphLikelihood
    apply Finset.prod_le_prod₀
    · intro j _
      dsimp [L]
      split_ifs <;> positivity
    · intro j _
      by_cases hj : j=i
      · subst j
        change L i ≤ weight l (q i)/s
        rw [hqi,center_weight hl]
        simp only [L,ite_true]
        field_simp
        norm_num
      · change L j ≤ weight l (q j)/s
        rw [show L j = (1/2)/s by simp only [L,hj,ite_false,mul_one]; ring]
        exact div_le_div_of_nonneg_right (weight_gt_one_lower hl (hq j).1) hs.le
  have heq : (∏ j, L j) = C * (exp (l*log 2) / (l*log 2)^(n : ℝ)) := by
    unfold L
    rw [Finset.prod_mul_distrib]
    simp [C, rpow_natCast, s, mul_pow]
    field_simp

  exact ⟨l,hl,s,hs,q,hq,hB.trans_le (heq ▸ hsum)⟩

end UnivariateML.TLUnrestricted
end
