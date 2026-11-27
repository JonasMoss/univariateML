module

public import UnivariateML.Family.BinomialSize

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.BinomialSize

namespace UnivariateML.JoinsBinomialSize

/-- The joint binomial fit exists on all-zero data and exactly under
underdispersion on positive-mean data. On all-zero data both feasible
parameter axes comprise the maximizing set. -/
theorem all_free_criterion {m : ℕ} (x : Fin m → ℕ) (hm : 0 < m) :
    let L := fun q : ℕ × ℝ ↦ Discrete.Binomial.sampleLikelihood x q.1 q.2
    ( {q ∈ parameterDomain | IsMaxOn L parameterDomain q}.Nonempty ↔
      (∀ i, x i = 0) ∨ variance x < mean x) ∧
    ((∀ i, x i = 0) → ∀ q ∈ parameterDomain,
      IsMaxOn L parameterDomain q ↔ q.1 = 0 ∨ q.2 = 0) := by
  intro L
  by_cases h : ∀ i, x i = 0
  · have hx : x = fun _ ↦ 0 := funext h
    have hz : ∀ q ∈ parameterDomain, IsMaxOn L parameterDomain q ↔ q.1 = 0 ∨ q.2 = 0 := by
      intro q hq
      dsimp only [L]
      rw [hx]
      exact all_zero_maximizers hm q hq
    refine ⟨⟨fun _ ↦ Or.inl h, fun _ ↦ ?_⟩, fun _ ↦ hz⟩
    have hq : ((0 : ℕ), (0 : ℝ)) ∈ parameterDomain := ⟨mem_univ _, by simp⟩
    exact ⟨(0,0), hq, (hz _ hq).mpr (Or.inl rfl)⟩
  · obtain ⟨i, hi⟩ := not_forall.mp h
    have hmean : 0 < mean x := by
      unfold mean
      exact div_pos
        (Finset.sum_pos' (fun j _ ↦ Nat.cast_nonneg _)
          ⟨i, Finset.mem_univ _, Nat.cast_pos.mpr (Nat.pos_of_ne_zero hi)⟩)
        (Nat.cast_pos.mpr hm)
    exact ⟨by simpa only [h, false_or] using joint_mle_iff_underdispersion x hm hmean,
      fun hh ↦ (h hh).elim⟩

end UnivariateML.JoinsBinomialSize

end
