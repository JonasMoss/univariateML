module

public import UnivariateML.Family.Endpoint2ParetoII
public import UnivariateML.Family.Endpoint2FellerParetoRegular

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Pareto II threshold contact

## Deviations

The atlas subunit lower-power spike has an empty branch for Pareto II.
Its lower power is identically one under the Pareto II parameterization.
-/

@[expose] public section

open Real Set
open scoped BigOperators

namespace UnivariateML.Endpoint3ParetoII

/-- At held scale and shape the fitted Pareto II threshold is exactly
the sample minimum, represented here by zero threshold gap. -/
theorem threshold_contact_iff {n : ℕ} {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hz : ∃ i, d i = 0) {q s : ℝ} (hq : 0 < q) (hs : 0 < s)
    {t : ℝ} (ht : 0 ≤ t) :
    IsMaxOn (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) (Ici 0) t ↔ t = 0 := by
  have heq : (fun u ↦ Transfer.ParetoII.logLik d (-u) q s) =
      Endpoint2FellerPareto.logLik d 1 q 1 s (log q - log s) := by
    funext u
    unfold Transfer.ParetoII.logLik Endpoint2FellerPareto.logLik
    apply Finset.sum_congr rfl
    intro i _
    simp [Transfer.ParetoII.logDensity, add_comm]
  have h0 := (Endpoint2FellerParetoRegular.fixed_scale_shapes_fit hd hz
    hq one_pos hs (a := 1) (K := log q - log s)).2.2.2 rfl
  simp only [ite_true] at h0
  rw [← heq] at h0
  have hn : 0 < n := by obtain ⟨i, _⟩ := hz; exact i.isLt.pos
  obtain ⟨u, _, hu⟩ := Endpoint2ParetoII.held_scale_shape_threshold hn hd hq hs
  exact ⟨fun hm ↦ (hu t ⟨ht, hm⟩).trans (hu 0 ⟨le_rfl, h0⟩).symm,
    fun h ↦ h.symm ▸ h0⟩

end UnivariateML.Endpoint3ParetoII

end
