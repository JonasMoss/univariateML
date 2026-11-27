module

public import UnivariateML.Family.FinishSN
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set
open UnivariateML.FinishSN

namespace UnivariateML.JoinsSN

private theorem constant_bound {n : ℕ} {x : Fin n → ℝ} (i : Fin n)
    (h : ¬ LocationScale.Nonconstant x) :
    ¬ BddAbove (range (fun ap : ℝ × LocationScale.inverseDomain ↦
      objective x ap.1 ap.2)) := by
  unfold LocationScale.Nonconstant at h
  push Not at h
  have he : x = fun _ ↦ x i := funext fun j ↦ h j i
  rw [he]
  exact FinishSN.constant_unbounded i.isLt.pos (x i)

/-- For the Azzalini skew normal, finite attainment requires nonconstant
data and a finite point reaching both optimized half-normal bounds. -/
theorem all_free_criterion {n : ℕ} (x : Fin n → ℝ) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ p : ℝ × LocationScale.inverseDomain,
      IsMaxOn (fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2) univ p) ↔
    LocationScale.Nonconstant x ∧
      ∃ (α : ℝ) (p : LocationScale.inverseDomain),
        max (face x (x i)) (face x (x j)) ≤ objective x α p := by
  by_cases hx : LocationScale.Nonconstant x
  · exact (FinishSN.all_free_criterion hx i j hi hj).trans (and_iff_right hx).symm
  · refine iff_of_false ?_ (fun h ↦ hx h.1)
    rintro ⟨p, hp⟩
    apply constant_bound i hx
    refine ⟨objective x p.1 p.2, ?_⟩
    rintro _ ⟨q, rfl⟩
    exact hp (mem_univ q)

/-- Constant data have unbounded log likelihood. On nonconstant data, a
strict loss at every finite point is equivalent to nonattainment, and the
larger optimized half-normal value is then the supremum. -/
theorem all_free_failure {n : ℕ} (x : Fin n → ℝ) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    let L := fun ap : ℝ × LocationScale.inverseDomain ↦ objective x ap.1 ap.2
    let F := max (face x (x i)) (face x (x j))
    (¬ LocationScale.Nonconstant x → ¬ BddAbove (range L)) ∧
    (LocationScale.Nonconstant x →
      ((¬ ∃ p, IsMaxOn L univ p) ↔ ∀ p, L p < F) ∧
      ((¬ ∃ p, IsMaxOn L univ p) → IsLUB (range L) F)) := by
  dsimp only
  refine ⟨constant_bound i, fun hx ↦ ⟨?_, FinishSN.unattained_face_supremum hx i j hi hj⟩⟩
  rw [FinishSN.all_free_criterion hx i j hi hj]
  push Not
  exact ⟨fun h p ↦ h p.1 p.2, fun h a p ↦ h (a,p)⟩

end UnivariateML.JoinsSN

end
