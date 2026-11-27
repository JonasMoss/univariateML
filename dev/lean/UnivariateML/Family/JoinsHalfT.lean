module

public import UnivariateML.Family.FinishHalfT

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.FinishHalfT UnivariateML.SkewLSHalfT

namespace UnivariateML.JoinsHalfT

/-- Strict majority at the minimum is precisely the unbounded branch.
Otherwise nonattainment is equivalent to the absence of an eligible
finite candidate reaching the larger half-normal or collapse face, which
is then the exact supremum. -/
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) :
    let L := fun p : ℝ × (ℝ × ℝ) ↦ likelihood x p.1 p.2.1 p.2.2
    let F := max (normalFace x (x i)) (collapseFace x (x i))
    ((¬ BddAbove (L '' domain x)) ↔ n < 2 * TDist.multiplicity x (x i)) ∧
    (2 * TDist.multiplicity x (x i) ≤ n →
      ((¬ ∃ p ∈ domain x, IsMaxOn L (domain x) p) ↔
        ¬ ∃ ν ≥ (1 : ℝ), 0 < TDist.exponent x (x i) ν ∧
          (ν = 1 ∨ HasDerivAt (scaleProfile x (x i)) 0 ν) ∧
          F ≤ scaleProfile x (x i) ν) ∧
      ((¬ ∃ p ∈ domain x, IsMaxOn L (domain x) p) → IsLUB (L '' domain x) F)) := by
  dsimp only
  exact ⟨all_free_unbounded_iff x hi, fun hc ↦
    ⟨((all_free_criterion x hi).trans (and_iff_right hc)).not,
      unattained_face_supremum x hi hc⟩⟩

end UnivariateML.JoinsHalfT

end
