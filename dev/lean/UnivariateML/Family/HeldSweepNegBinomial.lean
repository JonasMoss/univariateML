module

public import UnivariateML.Family.NegBinomial

import Mathlib.Tactic

set_option linter.style.header false

/-! # Negative-binomial held degeneracies on all-zero data -/

@[expose] public section

open Set

namespace UnivariateML.HeldSweepNegBinomial

open NegBinomial

/-- With probability held on all zeros, the entire size fiber maximizes
at probability one. At an interior probability the unique size is zero. -/
theorem held_probability_zero_data {n : ℕ} (hn : 0 < n) {p r : ℝ}
    (hp : p ∈ Ioc (0 : ℝ) 1) (hr : r ∈ Ici (0 : ℝ)) :
    IsMaxOn (fun s ↦ zeroLikelihood n s p) (Ici 0) r ↔ r = 0 ∨ p = 1 := by
  have h := zeroLikelihood_eq_one_iff hn hr hp.1 hp.2
  constructor
  · intro hm
    have hle : 1 ≤ zeroLikelihood n r p := by
      have h₀ : zeroLikelihood n 0 p ≤ zeroLikelihood n r p :=
        hm (show (0 : ℝ) ∈ Ici 0 from le_rfl)
      simpa only [zeroLikelihood, mul_zero, Real.rpow_zero] using h₀
    exact h.2.mp (le_antisymm h.1 hle)
  · intro he s hs
    have hle := (zeroLikelihood_eq_one_iff hn hs hp.1 hp.2).1
    change zeroLikelihood n s p ≤ zeroLikelihood n r p
    rwa [h.2.mpr he]

/-- With size held on all zeros, probability one is the unique optimizer
at positive size. At size zero every admissible probability maximizes. -/
theorem held_size_zero_data {n : ℕ} (hn : 0 < n) {p r : ℝ}
    (hr : 0 ≤ r) (hp : p ∈ Ioc (0 : ℝ) 1) :
    IsMaxOn (zeroLikelihood n r) (Ioc 0 1) p ↔ r = 0 ∨ p = 1 := by
  have h := zeroLikelihood_eq_one_iff hn hr hp.1 hp.2
  constructor
  · intro hm
    have hle : 1 ≤ zeroLikelihood n r p := by
      have h₁ : zeroLikelihood n r 1 ≤ zeroLikelihood n r p :=
        hm (show (1 : ℝ) ∈ Ioc 0 1 from ⟨zero_lt_one, le_rfl⟩)
      simpa only [zeroLikelihood, Real.one_rpow] using h₁
    exact h.2.mp (le_antisymm h.1 hle)
  · intro he s hs
    have hle := (zeroLikelihood_eq_one_iff hn hr hs.1 hs.2).1
    change zeroLikelihood n r s ≤ zeroLikelihood n r p
    rwa [h.2.mpr he]

end UnivariateML.HeldSweepNegBinomial

end
