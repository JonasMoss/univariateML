module

public import UnivariateML.Family.Discrete5HyperResidue

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Held-design comparison with the binomial face.

## Deviations
The comparison ranges over all finite urns. The reduction to a finite
prefix and two offsets per residue is not asserted.
-/

@[expose] public section

namespace UnivariateML.Base7Hyper

open Filter Set Hypergeometric Discrete4Hyper
open scoped Topology

/-- A held-draws fit exists exactly when a compatible finite urn reaches
or exceeds the height of a maximizing binomial face. -/
theorem held_draw_attains_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (k : ℕ)
    (hx : ∀ i, x i ≤ k) {p : ℝ} (hp : p ∈ Icc 0 1)
    (hmax : IsMaxOn (fun q ↦ ∏ i, binomialFace k (x i) q) (Icc 0 1) p) :
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧
      ∀ i, Compatible v.1 v.2 k (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood v.1 v.2 k x
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, (∏ i, binomialFace k (x i) p) ≤ L v := by
  dsimp only
  constructor
  · rintro ⟨v, hv, hm⟩
    obtain ⟨A, N, _, hcomp, _, hlim⟩ := held_draw_reaches_binomial_face x k hx hp
    refine ⟨v, hv, le_of_tendsto' hlim (fun t ↦ ?_)⟩
    apply hm (a := (A t, N t))
    have hc := hcomp t ⟨0, hh⟩
    exact ⟨hc.1, hc.2.1, hcomp t⟩
  · rintro ⟨v, hv, hb⟩
    by_cases hs : ∃ w ∈ {v : ℕ × ℕ | v.1 ≤ v.2 ∧ k ≤ v.2 ∧
      ∀ i, Compatible v.1 v.2 k (x i)},
      (∏ i, binomialFace k (x i) p) < likelihood w.1 w.2 k x
    · obtain ⟨w, hw, hwlt⟩ := hs
      obtain ⟨K, N, hK, hN, hc, H⟩ := held_draw_strict_witness x k
        hw.1 hw.2.1 hw.2.2 (fun q hq ↦ (hmax hq).trans_lt hwlt)
      exact ⟨(K, N), ⟨hK, hN, hc⟩, fun w hw ↦ H w.1 w.2 hw.1 hw.2.1 hw.2.2⟩
    · refine ⟨v, hv, fun w hw ↦ ?_⟩
      exact (le_of_not_gt (fun hwlt ↦ hs ⟨w, hw, hwlt⟩)).trans hb

/-- The same height criterion applies with the marked count held. -/
theorem held_mark_attains_iff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (m : ℕ)
    (hx : ∀ i, x i ≤ m) {p : ℝ} (hp : p ∈ Icc 0 1)
    (hmax : IsMaxOn (fun q ↦ ∏ i, binomialFace m (x i) q) (Icc 0 1) p) :
    let D := {v : ℕ × ℕ | v.1 ≤ v.2 ∧ m ≤ v.2 ∧
      ∀ i, Compatible m v.2 v.1 (x i)}
    let L := fun v : ℕ × ℕ ↦ likelihood m v.2 v.1 x
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, (∏ i, binomialFace m (x i) p) ≤ L v := by
  have hD : {v : ℕ × ℕ | v.1 ≤ v.2 ∧ m ≤ v.2 ∧ ∀ i, Compatible m v.2 v.1 (x i)} =
      {v : ℕ × ℕ | v.1 ≤ v.2 ∧ m ≤ v.2 ∧ ∀ i, Compatible v.1 v.2 m (x i)} := by
    ext v
    simp only [mem_ofPred_eq]
    constructor <;> rintro ⟨h₁, h₂, h₃⟩ <;> refine ⟨h₁, h₂, fun i ↦ ?_⟩ <;>
      have hi := h₃ i <;> unfold Compatible at hi ⊢ <;> omega
  dsimp only
  rw [hD]
  simp only [← likelihood_swap _ _ m x]
  exact held_draw_attains_iff hh x m hx hp hmax

end UnivariateML.Base7Hyper

end
