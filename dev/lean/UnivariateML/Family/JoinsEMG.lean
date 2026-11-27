module

public import UnivariateML.Family.FinishEMG
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.SkewLSEMG UnivariateML.FinishEMG

namespace UnivariateML.JoinsEMG

private theorem constant_of_not_nonconstant {n : ℕ} {x : Fin n → ℝ} (hn : 0 < n)
    (h : ¬ LocationScale.Nonconstant x) : ∃ m, x = fun _ ↦ m := by
  unfold LocationScale.Nonconstant at h
  push Not at h
  exact ⟨x ⟨0, hn⟩, funext fun i ↦ h i ⟨0, hn⟩⟩

private theorem not_bddAbove_of_no_bound {α : Type*} {f : α → ℝ} {D : Set α}
    (h : ¬ BddAbove (f '' D)) : ¬ ∃ q ∈ D, IsMaxOn f D q := by
  rintro ⟨q, _, hq⟩
  exact h ⟨f q, by
    rintro _ ⟨p, hp, rfl⟩
    exact hq hp⟩

/-- A finite EMG maximum exists exactly on the nonconstant branch with a
stationary value at least as large as both compactified face values. -/
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) (i : Fin n)
    (hi : ∀ j, x i ≤ x j) :
    (∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
      IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
        (Ioo 0 1 ×ˢ LocationScale.domain) q) ↔
    LocationScale.Nonconstant x ∧
      ∃ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t ∧
        max (compactProfile x i 0) (compactProfile x i 1) ≤ compactProfile x i t := by
  by_cases hx : LocationScale.Nonconstant x
  · exact (FinishEMG.all_free_criterion hx i hi).symm.trans (and_iff_right hx).symm
  · obtain ⟨m, hm⟩ := constant_of_not_nonconstant i.isLt.pos hx
    have hu := FinishEMG.constant_log_unbounded i.isLt.pos m
    rw [← hm] at hu
    exact iff_of_false (not_bddAbove_of_no_bound hu) (fun h ↦ hx h.1)

/-- The image of the log likelihood is unbounded on constant data. On
nonconstant data without a finite fit its supremum is the larger face. -/
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) (i : Fin n)
    (hi : ∀ j, x i ≤ x j) :
    let D := Ioo (0 : ℝ) 1 ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2
    (¬ LocationScale.Nonconstant x → ¬ BddAbove (L '' D)) ∧
    (LocationScale.Nonconstant x →
      ((¬ ∃ q ∈ D, IsMaxOn L D q) ↔
        ∀ t ∈ Ioo 0 1, HasDerivAt (compactProfile x i) 0 t →
          compactProfile x i t < max (compactProfile x i 0) (compactProfile x i 1)) ∧
      ((¬ ∃ q ∈ D, IsMaxOn L D q) →
        IsLUB (L '' D) (max (compactProfile x i 0) (compactProfile x i 1)))) := by
  dsimp only
  refine ⟨?_, fun hx ↦ ⟨?_, FinishEMG.unattained_face_supremum hx i hi⟩⟩
  · intro hx
    obtain ⟨m, rfl⟩ := constant_of_not_nonconstant i.isLt.pos hx
    exact FinishEMG.constant_log_unbounded i.isLt.pos m
  · rw [← FinishEMG.all_free_criterion hx i hi]
    push Not
    rfl

end UnivariateML.JoinsEMG

end
