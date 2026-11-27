module

public import UnivariateML.Family.Shape2PGW

set_option linter.style.header false

/-! # Power generalized Weibull at held scale

The threshold trace includes the endpoint density at every scale contact.
-/

@[expose] public section

open Real Set Filter
namespace UnivariateML.Shape3PGW
open ShapeFacesPGW AllFreePGW
noncomputable section

/-- All held-scale outcomes, including concentration on constants at or
below scale and the strict threshold supremum on the supported branch. -/
theorem held_scale_classification {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    let L := fun p : ℝ × ℝ ↦ logLik x p.1 a p.2
    ((∃ p ∈ Weibull.domain, IsMaxOn L Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, x i < a) ∧
    (∀ v ≤ a, (∀ i, x i = v) →
      ∃ c : ℝ → ℝ, (∀ b, 0 < c b) ∧
        Tendsto (fun b ↦ L (b, c b)) atTop atTop) ∧
    ((∀ i, a ≤ x i) → (∃ i, a < x i) →
      ∃ k > 0, IsLUB (L '' Weibull.domain) (thresholdLogLik x a k) ∧
        ∀ p ∈ Weibull.domain, L p < thresholdLogLik x a k) := by
  refine ⟨held_scale_exists_iff hn ha hx, ?_, held_scale_threshold_supremum hn ha⟩
  intro v hv h
  have he : x = fun _ ↦ v := funext h
  have hvpos : 0 < v := by rw [← h ⟨0, hn⟩]; exact hx ⟨0, hn⟩
  simpa only [he] using Shape2PGW.held_scale_constant_at_or_below (n := n) hn hvpos hv

end
end UnivariateML.Shape3PGW
end
