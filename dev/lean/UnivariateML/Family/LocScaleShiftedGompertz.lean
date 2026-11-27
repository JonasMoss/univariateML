module

public import UnivariateML.Family.ShapeFacesShiftedGompertz

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Exact attainment criteria for nonempty nonnegative samples. -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.LocScaleShiftedGompertz

open ShapeFacesShiftedGompertz

variable {n : ℕ}

/-- With rate and shape free, the shifted Gompertz fit attains precisely
on nonconstant nonnegative samples. -/
theorem joint_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i) :
    (∃ p ∈ jointDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) jointDomain p) ↔
    ∃ i j, x i ≠ x j := by
  refine ⟨?_,joint_attains hn hx⟩
  rintro ⟨p,_,hm⟩
  by_contra! h
  let j : Fin n := ⟨0,hn⟩
  have he : x = fun _ ↦ x j := funext fun i ↦ h i j
  have hv := (joint_constant_unbounded hn (x j) (hx j)).eventually_gt_atTop (logLik x p.1 p.2)
  obtain ⟨b,hb,hv⟩ := ((eventually_gt_atTop (0 : ℝ)).and hv).exists
  have hk : (b,exp (b*x j)) ∈ jointDomain := ⟨hb,(exp_pos _).le⟩
  have hbnd := hm hk
  change logLik x b (exp (b*x j)) ≤ logLik x p.1 p.2 at hbnd
  exact hv.not_ge ((congrArg (fun y ↦ logLik y b (exp (b*x j))) he).symm.le.trans hbnd)

/-- At held fitted shape, the shifted Gompertz rate attains precisely
when the sample sum is positive. -/
theorem held_shape_criterion (hn : 0 < n) (x : Fin n → ℝ) (hx : ∀ i, 0 ≤ x i)
    {η : ℝ} (hη : 0 ≤ η) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun r ↦ logLik x r η) (Ioi 0) b) ↔ 0 < ∑ i, x i := by
  refine ⟨?_,fun h ↦ (held_shape_rate_unique hn hx hη h).exists⟩
  rintro ⟨b,_,hm⟩
  by_contra! h
  have hsum : ∑ i, x i = 0 := le_antisymm h (Finset.sum_nonneg fun i _ ↦ hx i)
  have he : x = fun _ ↦ 0 := funext fun i ↦
    le_antisymm ((Finset.single_le_sum (f := x) (fun a _ ↦ hx a) (Finset.mem_univ i)).trans_eq hsum)
      (hx i)
  have hv := (all_zero_rate_unbounded hn η).eventually_gt_atTop (logLik x b η)
  obtain ⟨r,hr,hv⟩ := ((eventually_gt_atTop (0 : ℝ)).and hv).exists
  have hbnd := hm (show r ∈ Ioi (0 : ℝ) from hr)
  change logLik x r η ≤ logLik x b η at hbnd
  exact hv.not_ge ((congrArg (fun y ↦ logLik y r η) he).symm.le.trans hbnd)

end UnivariateML.LocScaleShiftedGompertz

end
