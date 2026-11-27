module

public import UnivariateML.Family.Shape2InvBurr

set_option linter.style.header false

/-! # Inverse Burr at held scale

The reciprocal likelihood and its Jacobian enter through the existing
inverse Burr face comparison. All three sample branches are joined here.
-/

@[expose] public section

open Real Set
namespace UnivariateML.Shape3InvBurr
open ShapeFacesInverseBurr Transfer
noncomputable section

/-- Constants on or above the scale diverge. Samples on or below the
scale with a strict inequality lose to the Pareto trace. In the remaining
nonconstant branch a finite fit exists. -/
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (hs : 0 < s) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.2 p.1 s
    let z := BurrHeld.logRatios (reciprocal x) s⁻¹
    let B := BurrHeld.paretoValue z +
      (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i)))
    ((∃ p ∈ BurrHeld.shapeDomain, IsMaxOn L BurrHeld.shapeDomain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, s < x i) ∧
    (∀ v ≥ s, (∀ i, x i = v) → ¬ BddAbove (L '' BurrHeld.shapeDomain)) ∧
    ((∀ i, x i ≤ s) → (∃ i, x i < s) →
      IsLUB ((fun p ↦ L p / n) '' BurrHeld.shapeDomain) B ∧
        ∀ p ∈ BurrHeld.shapeDomain, L p / n < B) := by
  refine ⟨Shape2InvBurr.held_scale_attains_iff hn hx hs, ?_, held_scale_face hn hx hs⟩
  intro v hv he
  have he' : x = fun _ ↦ v := funext he
  simpa only [he'] using held_scale_constant_unbounded hn hs hv

end
end UnivariateML.Shape3InvBurr
end
