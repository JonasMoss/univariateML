module

public import UnivariateML.Family.DiscreteWeibull
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Real
open UnivariateML.DiscreteWeibull

namespace UnivariateML.JoinsDiscreteWeibull

/-- The gap branch has a unique finite fit. Without a gap, the whole
sample is constant or occupies two adjacent cells. In those cases the
point mass or the empirical two-cell law gives the unattained supremum. -/
theorem all_free_classification {n : ℕ} (x : Fin n → ℕ) (hn : 0 < n) :
    let M := ∃ p ∈ domain, IsMaxOn (logLik x) domain p
    ((∃ i j, x i + 2 ≤ x j) → ∃! p, p ∈ domain ∧ IsMaxOn (logLik x) domain p) ∧
    (¬ (∃ i j, x i + 2 ≤ x j) → ¬ M ∧
      ((∃ m, (∀ i, x i = m) ∧ sSup (logLik x '' domain) = 0) ∨
        ∃ m, (∀ i, x i = m ∨ x i = m + 1) ∧ (∃ i, x i = m) ∧ (∃ i, x i = m + 1) ∧
          sSup (logLik x '' domain) =
            ∑ i, log (if x i = m + 1 then upperFraction x m else 1 - upperFraction x m))) := by
  intro M
  refine ⟨(joint_existsUnique_iff hn).mpr, fun hgap ↦ ?_⟩
  obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hcell (j : Fin n) : x j = x i ∨ x j = x i + 1 := by
    have hlo := hi j (Finset.mem_univ j)
    have hup : ¬ x i + 2 ≤ x j := fun h ↦ hgap ⟨i,j,h⟩
    omega
  by_cases hc : ∀ j, x j = x i
  · obtain ⟨hB, hM⟩ := constant_sup hn hc
    exact ⟨hM, Or.inl ⟨x i, hc, hB⟩⟩
  · obtain ⟨j, hj⟩ := not_forall.mp hc
    have hl : ∃ j, x j = x i := ⟨i,rfl⟩
    have hr : ∃ j, x j = x i + 1 := ⟨j,(hcell j).resolve_left hj⟩
    obtain ⟨hB, hM⟩ := adjacent_sup hcell hl hr
    exact ⟨hM, Or.inr ⟨x i,hcell,hl,hr,hB⟩⟩

end UnivariateML.JoinsDiscreteWeibull

end
