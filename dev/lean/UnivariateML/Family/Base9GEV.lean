module

public import UnivariateML.Family.Shape3GEV

/-!
# Generalized extreme value with location held

## Deviations

The held-location failure claim concerns all-nonnegative residuals.
The negative-residual and strict-sub-half attainment cases are separate.
-/

@[expose] public section

open Filter Set
open scoped Topology

namespace UnivariateML.Base9GEV

/-- With all residuals nonnegative, majority contact gives unbounded
likelihood. At half contact the finite collapse value is the supremum
and is unattained precisely when the positive residuals differ. -/
theorem held_location_failure {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {m : ℝ} (hm : ∀ i, m ≤ x i) :
    let k := (Finset.univ.filter fun i ↦ x i = m).card
    let D : Set (ℝ × ℝ) := {p | p.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, p.2) ∈ GEV.domain x p.1}
    let L := fun p : ℝ × ℝ ↦ GEV.logLik x p.1 (m, p.2)
    let P := fun s : ℝ ↦ (1, s)
    (∀ s > 0, P s ∈ D) ∧
    (n < 2 * k → Tendsto (L ∘ P) (𝓝[>] 0) atTop ∧
      ¬ BddAbove (L '' D) ∧ ¬ ∃ p ∈ D, IsMaxOn L D p) ∧
    (n = 2 * k → Tendsto (L ∘ P) (𝓝[>] 0) (𝓝 (Shape2GEV.halfValue x m)) ∧
      IsLUB (L '' D) (Shape2GEV.halfValue x m) ∧
      ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s)) := by
  intro k D L P
  have H := Shape3GEV.held_location_contact_classification hn hm
  refine ⟨?_, ?_, ?_⟩
  · intro s hs
    refine ⟨by norm_num, hs, fun i ↦ ?_⟩
    simp only [P, GEV.supportArg, one_mul, ite_eq_right (show (1 : ℝ) ≠ -1 by norm_num)]
    exact add_pos_of_pos_of_nonneg one_pos (div_nonneg (sub_nonneg.mpr (hm i)) hs.le)
  · intro hk
    exact ⟨(HeldSweepGEV.held_minimum_majority_collapse x hm hk).2,
      H.1 hk, fun ⟨_, _, h⟩ ↦ H.1 hk h.bddAbove⟩
  · intro hk
    exact ⟨HeldSweepGEV.held_minimum_half_path x hm hk, H.2 hk⟩

end UnivariateML.Base9GEV

end
