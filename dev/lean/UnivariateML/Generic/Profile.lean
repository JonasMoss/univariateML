module

public import Mathlib.Order.Filter.Extr

set_option linter.style.header false

/-! # Maximizing a profile over attained fibers -/

@[expose] public section

open Set

namespace UnivariateML.Generic

/-- Maximizing the profile of attained fiber maxima gives a joint maximizer. -/
theorem isMaxOn_prod_of_profile {Θ Φ V : Type*} [Preorder V]
    {L : Θ → Φ → V} {T : Set Θ} {F : Set Φ} {best : Θ → Φ} {t : Θ}
    (hfiber : ∀ θ ∈ T, best θ ∈ F ∧ IsMaxOn (L θ) F (best θ))
    (ht : t ∈ T) (hprofile : IsMaxOn (fun θ ↦ L θ (best θ)) T t) :
    (t, best t) ∈ T ×ˢ F ∧ IsMaxOn (fun p : Θ × Φ ↦ L p.1 p.2) (T ×ˢ F) (t, best t) :=
  ⟨⟨ht, (hfiber t ht).1⟩, fun p hp ↦
    ((hfiber p.1 hp.1).2 hp.2).trans (hprofile hp.1)⟩

/-- With unique fiber maximizers, the set of feasible joint maximizers is the
graph of the maximizing profile points. -/
theorem joint_maximizers_eq_profile_graph {Θ Φ V : Type*} [Preorder V]
    {L : Θ → Φ → V} {T : Set Θ} {F : Set Φ} {best : Θ → Φ}
    (hfiber : ∀ θ ∈ T, best θ ∈ F ∧ IsMaxOn (L θ) F (best θ))
    (hunique : ∀ θ ∈ T, ∀ φ ∈ F, IsMaxOn (L θ) F φ → φ = best θ) :
    {p ∈ T ×ˢ F | IsMaxOn (fun p : Θ × Φ ↦ L p.1 p.2) (T ×ˢ F) p} =
      (fun θ ↦ (θ, best θ)) '' {θ ∈ T | IsMaxOn (fun θ ↦ L θ (best θ)) T θ} := by
  ext ⟨θ, φ⟩
  constructor
  · rintro ⟨⟨hθ, hφ⟩, hm⟩
    have he : φ = best θ := hunique θ hθ φ hφ (fun ψ hψ ↦
      hm (show (θ, ψ) ∈ T ×ˢ F from ⟨hθ, hψ⟩))
    subst φ
    refine ⟨θ, ⟨hθ, ?_⟩, rfl⟩
    intro t ht
    exact hm (show (t, best t) ∈ T ×ˢ F from ⟨ht, (hfiber t ht).1⟩)
  · rintro ⟨t, ⟨ht, hm⟩, h⟩
    rw [← h]
    exact isMaxOn_prod_of_profile hfiber ht hm

end UnivariateML.Generic
