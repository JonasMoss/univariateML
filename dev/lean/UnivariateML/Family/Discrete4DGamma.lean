module

public import UnivariateML.Family.AllFreeDGamma

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Discretised gamma all-free saturated suprema

The constant and two-occupied-adjacent-cell branches share the same
positive shape and rate domain.
-/

@[expose] public section

open Set
open scoped BigOperators Classical

namespace UnivariateML.Discrete4DGamma

open Discrete3DGamma

/-- A constant sample or a sample occupying two adjacent cells has an
unattained saturated supremum. The two-cell height uses its empirical split. -/
theorem saturated_suprema {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) :
    let L := {v | ∃ a > 0, ∃ r > 0, likelihood x a r = v}
    ((∃ j, ∀ i, x i = j) → IsLUB L 1 ∧ ∀ a > 0, ∀ r > 0, likelihood x a r < 1) ∧
    (∀ j, (∀ i, x i = j ∨ x i = j+1) → (∃ i, x i = j) → (∃ i, x i = j+1) →
      let m := (Finset.univ.filter fun i ↦ x i = j).card
      let k := (Finset.univ.filter fun i ↦ x i ≠ j).card
      let w := (m : ℝ)/(m+k)
      IsLUB L (w^m*(1-w)^k) ∧ ∀ a > 0, ∀ r > 0, likelihood x a r < w^m*(1-w)^k) := by
  intro L
  refine ⟨?_,fun j hx hl hu ↦ AllFreeDGamma.adjacent_supremum x j hx hl hu⟩
  rintro ⟨j,hj⟩
  have he : x = fun _ ↦ j := funext hj
  subst x
  exact AllFreeDGamma.constant_supremum hn j

end UnivariateML.Discrete4DGamma

end
