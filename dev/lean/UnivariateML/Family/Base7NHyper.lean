module

public import UnivariateML.Family.Discrete5NHyper

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Held-composition failure and all-free slice comparison.

## Deviations
The all-free comparison uses order-theoretic slice suprema. Effective slice
computation and exhaustion of winning escapes remain open.
-/

@[expose] public section

namespace UnivariateML.Base7NHyper

open Filter Set Discrete4NHyper
open scoped Topology

/-- With marked and stopping counts held, a compatible zero stopping count
gives a flat likelihood, while every positive stopping count has zero limit
as the population grows. Here `j` counts the unmarked population. -/
theorem held_mark_stop_failure {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ j, ∀ i, Compatible (m + j) m r (x i)) :
    (r = 0 ∧ (∀ i, x i = 0) ∧ ∀ j, totalLikelihood x (m + j) m r = 1) ∨
    (0 < r ∧ Tendsto (fun j : ℕ ↦ totalLikelihood x (m + j) m r) atTop (𝓝 0)) := by
  rcases Discrete5NHyper.held_composition_stopping hh x hf with H | H
  · exact Or.inl ⟨H.1, H.2.1, H.2.2.2⟩
  · exact Or.inr ⟨H.1, H.2.2.1⟩

/-- All finite winners for constant data, including the flat zero branch. -/
theorem constant_maximizers_iff {h : ℕ} (hh : 0 < h) (N m r c : ℕ) :
    (∀ A B C, totalLikelihood (fun _ : Fin h ↦ c) A B C ≤
      totalLikelihood (fun _ : Fin h ↦ c) N m r) ↔
    m ≤ N ∧ r ≤ m ∧ r = c ∧ (c = 0 ∨ m = N) := by
  obtain ⟨he, hb⟩ := constant_attains_saturated (h := h) c
  rw [← constant_saturated_iff hh]
  exact ⟨fun H ↦ (hb N m r).antisymm (he ▸ H c c c), fun H A B C ↦ H ▸ hb A B C⟩

private theorem bounded_likelihood {h : ℕ} (x : Fin h → ℕ) (N m r : ℕ) :
    totalLikelihood x N m r ≤ 1 := by
  unfold totalLikelihood
  apply Finset.prod_le_one₀
  · intro i _
    unfold totalMass NegHypergeometric.mass NegHypergeometric.reducedMass
    split_ifs <;> positivity
  · intro i _
    have h := (constant_attains_saturated (h := 1) (x i)).2 N m r
    simpa [totalLikelihood] using h

/-- The unbounded all-free search on positive data is a finite comparison
of slice suprema. A finite urn wins precisely when it reaches the largest
slice height. This formulation retains equality with an unattained slice. -/
theorem positive_stopping_slice_maximizers {h : ℕ} (x : Fin h → ℕ) {i : Fin h}
    (hi : ∀ j, x i ≤ x j) (hx : 0 < x i) :
    let D := {p : ℕ × ℕ × ℕ | ∀ j, Compatible p.1 p.2.1 p.2.2 (x j)}
    let L := fun p : ℕ × ℕ × ℕ ↦ totalLikelihood x p.1 p.2.1 p.2.2
    let S := Finset.Icc 1 (x i)
    let V := fun r : ℕ ↦ {v : ℝ | ∃ N m, (N, m, r) ∈ D ∧ v = L (N, m, r)}
    ∃ r ∈ S, (∀ s ∈ S, sSup (V s) ≤ sSup (V r)) ∧
      ∀ p ∈ D, IsMaxOn L D p ↔ L p = sSup (V r) := by
  classical
  intro D L S V
  have hS {p : ℕ × ℕ × ℕ} (hp : p ∈ D) : p.2.2 ∈ S := by
    have hc := (hp i).2.2
    have hr : p.2.2 ≠ 0 := by
      intro hz
      simp [hz, hx.ne'] at hc
    simp only [ite_eq_right hr] at hc
    exact Finset.mem_Icc.mpr ⟨Nat.pos_of_ne_zero hr, hc.1⟩
  have hV (r : ℕ) (hr : r ∈ S) : (V r).Nonempty := by
    let N := Finset.univ.sup x
    have hr' := Finset.mem_Icc.mp hr
    have hN (j : Fin h) : x j ≤ N := Finset.le_sup (Finset.mem_univ j)
    refine ⟨L (N, r, r), N, r, fun j ↦ ?_, rfl⟩
    have hlow : r ≤ x j := hr'.2.trans (hi j)
    refine ⟨hlow.trans (hN j), le_rfl, ?_⟩
    simp only [ite_eq_right (show r ≠ 0 by omega)]
    exact ⟨hlow, Nat.sub_le_sub_right (hN j) r⟩
  have hb (r : ℕ) : BddAbove (V r) := by
    refine ⟨1, ?_⟩
    rintro _ ⟨N, m, _, rfl⟩
    exact bounded_likelihood x N m r
  have hmS : S.Nonempty := ⟨1, Finset.mem_Icc.mpr ⟨le_rfl, hx⟩⟩
  obtain ⟨r, hr, hmax⟩ := Finset.exists_max_image S (fun r ↦ sSup (V r)) hmS
  have hbound (p : ℕ × ℕ × ℕ) (hp : p ∈ D) : L p ≤ sSup (V r) :=
    (le_csSup (hb _) (show L p ∈ V p.2.2 from ⟨p.1, p.2.1, hp, rfl⟩)).trans (hmax _ (hS hp))
  refine ⟨r, hr, hmax, fun p hp ↦ ?_⟩
  constructor
  · intro hm
    apply (hbound p hp).antisymm
    apply csSup_le (hV r hr)
    rintro _ ⟨N, m, hd, rfl⟩
    exact hm hd
  · intro he
    exact fun q hq ↦ (hbound q hq).trans_eq he.symm

end UnivariateML.Base7NHyper

end
