module

public import UnivariateML.Family.AllFreeDGamma
public import Mathlib.Analysis.Analytic.IsolatedZeros

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! The compact analytic step in the discretized-gamma candidate argument.

## Deviations
This is conditional on analyticity and confinement of score zeros to a
positive compact interval. The likelihood-specific analytic rate profile,
its score tails, and the sample-only trace bound are separate obligations.
-/

@[expose] public section

namespace UnivariateML.Base7DGamma

open Set Filter

/-- Once the rate profile has analytic nonzero score, a compact interval
containing every stationary shape contains finitely many candidates.
When a global optimum is attained, comparing the candidates is sufficient. -/
theorem finite_profile_candidates (P : ℝ → ℝ) {a b : ℝ} (ha : 0 < a)
    (han : AnalyticOnNhd ℝ (deriv P) (Ioi 0))
    (hne : ∃ t > 0, deriv P t ≠ 0)
    (hcut : ∀ t > 0, deriv P t = 0 → t ∈ Icc a b)
    (hatt : ∃ t ∈ Ioi 0, IsMaxOn P (Ioi 0) t) :
    let C := {t ∈ Icc a b | deriv P t = 0}
    C.Finite ∧ ∀ t, (t ∈ Ioi 0 ∧ IsMaxOn P (Ioi 0) t) ↔
      t ∈ C ∧ IsMaxOn P C t := by
  intro C
  obtain ⟨u, hu, hnu⟩ := hne
  have hdis : ∀ᶠ t in codiscreteWithin (Ioi (0 : ℝ)), deriv P t ≠ 0 := by
    rcases han.eqOn_zero_or_eventually_ne_zero_of_preconnected
      (convex_Ioi (0 : ℝ)).isPreconnected with he | h
    · exact (hnu (he hu)).elim
    · exact h
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun _ ht ↦ ha.trans_le ht.1
  have hf := (isCompact_Icc (a := a) (b := b)).finite_sdiff_of_mem_codiscreteWithin
    (show {t | deriv P t ≠ 0} ∈ codiscreteWithin (Icc a b) from
      codiscreteWithin_mono hsub hdis)
  have hzero {t : ℝ} (ht : t ∈ Ioi 0) (hm : IsMaxOn P (Ioi 0) t) : t ∈ C := by
    have hz := (hm.isLocalMax (isOpen_Ioi.mem_nhds ht)).deriv_eq_zero
    exact ⟨hcut t ht hz, hz⟩
  refine ⟨hf.subset (fun t ht ↦ ⟨ht.1, fun h ↦ h ht.2⟩), fun t ↦ ⟨?_, ?_⟩⟩
  · rintro ⟨ht, hm⟩
    exact ⟨hzero ht hm, fun v hv ↦ hm (hsub hv.1)⟩
  · rintro ⟨ht, hm⟩
    obtain ⟨v, hv, hmv⟩ := hatt
    refine ⟨hsub ht.1, ?_⟩
    intro z hz
    exact (show P z ≤ P v from hmv hz).trans (hm (hzero hv hmv))

end UnivariateML.Base7DGamma

end
