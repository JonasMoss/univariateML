module

public import UnivariateML.Family.FinishCMB
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.Discrete3CMB UnivariateML.FinishCMB

namespace UnivariateML.JoinsCMB

/-- Every compatible size-zero or size-one sample attains a maximum.
For larger held sizes, attainment requires an endpoint-constant sample
or an empirical moment pair strictly inside the moment hull. -/
theorem held_size_criterion {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N + 1)) :
    (∃ p ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ),
      IsMaxOn (fun p : ℝ × ℝ ↦ closedLikelihood x p.1 p.2) (Icc 0 1 ×ˢ univ) p) ↔
    N ≤ 1 ∨ (∀ i, x i = 0) ∨ (∀ i, x i = Fin.last N) ∨
      empirical x ∈ interior (convexHull ℝ (range (statistic N))) := by
  by_cases hN : 2 ≤ N
  · rw [held_size_closed_criterion hN hn]
    simp [show ¬ N ≤ 1 by omega]
  have hsmall : N ≤ 1 := by omega
  refine ⟨fun _ ↦ Or.inl hsmall, fun _ ↦ ?_⟩
  rcases (show N = 0 ∨ N = 1 by omega) with rfl | rfl
  · refine ⟨(0, 0), ⟨by norm_num, mem_univ _⟩, ?_⟩
    intro q _
    change closedLikelihood x q.1 q.2 ≤ closedLikelihood x 0 0
    rw [size_zero_likelihood, size_zero_likelihood]
  · let o : ℝ := Discrete3CMB.multiplicity x 1
    let z : ℝ := Discrete3CMB.multiplicity x 0
    have ho : 0 ≤ o := Nat.cast_nonneg _
    have hz : 0 ≤ z := Nat.cast_nonneg _
    let p := o / (o + z)
    have hp : p ∈ Icc (0 : ℝ) 1 := by
      refine ⟨div_nonneg ho (add_nonneg ho hz), ?_⟩
      by_cases hd : 0 < o + z
      · exact (div_le_one hd).mpr (le_add_of_nonneg_right hz)
      · have he : o + z = 0 := le_antisymm (le_of_not_gt hd) (add_nonneg ho hz)
        simp [p, he]
    refine ⟨(p, 0), ⟨hp, mem_univ _⟩, ?_⟩
    intro q hq
    have hmax := (bernoulli_fit hn x q.2 hp).mpr rfl
    calc
      closedLikelihood x q.1 q.2 ≤ closedLikelihood x p q.2 := hmax hq.1
      _ = closedLikelihood x p 0 := by rw [bernoulli_likelihood x hp, bernoulli_likelihood x hp]

end UnivariateML.JoinsCMB

end
