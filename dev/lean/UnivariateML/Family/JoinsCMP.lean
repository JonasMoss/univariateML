module

public import UnivariateML.Family.HeldSweepCMP
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.HeldSweepCMP

namespace UnivariateML.JoinsCMP

/-- Positive held rate has a unique fit exactly when some count exceeds
one. Binary samples instead have the strict Bernoulli supremum. At zero
rate every dispersion maximizes the flat likelihood, which is positive
exactly for compatible all-zero data. -/
theorem held_rate {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {r : ℝ} (hr : 0 ≤ r) :
    let D := {ν : ℝ | (r, ν) ∈ CMP.rateDomain}
    let M := fun ν ↦ ν ∈ D ∧ IsMaxOn (CMP.likelihood x r) D ν
    (0 < r →
      ((∃ ν, M ν) ↔ ∃ i, 1 < x i) ∧
      ((∃ i, 1 < x i) → ∃! ν, M ν) ∧
      ((∀ i, x i ≤ 1) →
        let b := ∏ i, r ^ x i / (1 + r)
        IsLUB (CMP.likelihood x r '' D) b ∧ ∀ ν ∈ D, CMP.likelihood x r ν < b)) ∧
    (r = 0 → ∀ ν ∈ D, M ν ∧ CMP.likelihood x r ν = (if ∀ i, x i = 0 then 1 else 0)) := by
  intro D M
  refine ⟨fun hpos ↦ ⟨⟨?_, fun h ↦ (held_rate_nonbinary hn x h hpos).exists⟩,
    fun h ↦ held_rate_nonbinary hn x h hpos, fun h ↦ held_rate_binary_supremum hn x h hpos⟩, ?_⟩
  · rintro ⟨ν, hv, hm⟩
    by_contra h
    push Not at h
    obtain ⟨hB, hlt⟩ := held_rate_binary_supremum hn x h hpos
    exact (hlt ν hv).not_ge (hB.2 (by
      rintro _ ⟨w, hw, rfl⟩
      exact hm hw))
  · rintro rfl ν hv
    have he : D = Ici (0 : ℝ) := by
      ext q
      simp [D, CMP.rateDomain]
    have hz := held_zero_rate x ν (he ▸ hv)
    exact ⟨⟨hv, he ▸ hz.1⟩, hz.2⟩

end UnivariateML.JoinsCMP

end
