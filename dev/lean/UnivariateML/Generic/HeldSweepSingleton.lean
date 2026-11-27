module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Order.Filter.Extr

@[expose] public section

namespace UnivariateML.HeldSweepSingleton

/-- The supplied parameter uniquely maximizes any real likelihood on its
singleton feasible set, even when the likelihood there is zero. -/
theorem singleton_fit {P : Type*} (L : P → ℝ) (p : P) :
    (∃! q, q ∈ ({p} : Set P) ∧ IsMaxOn L {p} q) ∧
      IsGreatest (L '' ({p} : Set P)) (L p) ∧
      sSup (L '' ({p} : Set P)) = L p := by
  have hm : IsMaxOn L ({p} : Set P) p := by
    intro q hq
    exact le_of_eq (congrArg L (Set.mem_singleton_iff.mp hq))
  rw [Set.image_singleton]
  refine ⟨⟨p, ⟨by simp, hm⟩, fun q hq ↦ Set.mem_singleton_iff.mp hq.1⟩, ?_, csSup_singleton _⟩
  exact ⟨by simp, fun q hq ↦ le_of_eq (Set.mem_singleton_iff.mp hq)⟩

end UnivariateML.HeldSweepSingleton

end
