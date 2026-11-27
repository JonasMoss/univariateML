module

public import UnivariateML.Family.Transfers
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

set_option linter.style.header false

/-! # The fitted inverse exponential face -/

@[expose] public section

open Real Filter Set
open scoped Topology

namespace UnivariateML.Endpoint3InversePareto

/-- When the finite inverse Pareto witness test fails, the fitted inverse
exponential value is a strict least upper bound, reached in the limit along
the fitted-rate path. This includes constant positive samples. -/
theorem inverse_exponential_failure {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Transfer.reciprocal x)),
      Lomax.psi (Transfer.reciprocal x) u ≤ 0) :
    let L := fun p : ℝ × ℝ ↦ Transfer.InvPareto.logLik x p.1 p.2
    let r := (Lomax.avg (Transfer.reciprocal x))⁻¹
    let C := Transfer.InvPareto.bestInvExp x
    IsLUB (L '' Lomax.domain) C ∧
      (∀ p ∈ Lomax.domain, L p < C) ∧
      Tendsto (fun k ↦ L (k, r / k)) atTop (𝓝 C) := by
  intro L r C
  have hr : 0 < r := by
    apply inv_pos.mpr
    exact div_pos (Finset.sum_pos (fun i _ ↦ inv_pos.mpr (hx i))
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩) (Nat.cast_pos.mpr hn)
  have hstrict : ∀ p ∈ Lomax.domain, L p < C :=
    (Transfer.InvPareto.exponential_sup_of_no_witness hn hx hw).2
  have hlim : Tendsto (fun k ↦ L (k, r / k)) atTop (𝓝 C) :=
    Transfer.InvPareto.face_path hx hr
  have hdom : ∀ᶠ k in atTop, (k, r / k) ∈ Lomax.domain :=
    (eventually_gt_atTop (0 : ℝ)).mono fun _ hk ↦ ⟨hk, div_pos hr hk⟩
  refine ⟨⟨?_, fun _ hB ↦ Generic.limit_le_upperBound hdom hlim hB⟩, hstrict, hlim⟩
  rintro _ ⟨p, hp, rfl⟩
  exact (hstrict p hp).le

end UnivariateML.Endpoint3InversePareto

end
