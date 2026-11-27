module

public import UnivariateML.Family.AE4SGED

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Superunit generalized-error winners

## Deviations

The chart is the symmetric subfamily of the superunit SGED chart. The
result compares two superunit winners, allowing arbitrary closed variance
holds. It leaves the shape-one median class and comparisons with it open.
-/

@[expose] public section

open Set Filter MeasureTheory Real

namespace UnivariateML.AE4GED

noncomputable section

/-- Positive-width symmetric superunit candidates with a closed
restriction on the variance. -/
def domain (V : Set ℝ) : Set AE4SGED.Parameter :=
  {p | 1 < p.1 ∧ 0 < p.2.2.1 ∧ p.2.2.1 = p.2.2.2 ∧
    (p.2.2.1)^2 * (Gamma (3/p.1) / Gamma (1/p.1)) ∈ V}

/-- On a four-level chart, the symmetric GED has almost everywhere at
most one superunit maximizer, both with variance held and with variance
free. Apply with a singleton for held variance and `univ` otherwise. -/
theorem superunit_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 4 ≤ n) (V : Set ℝ) (hV : IsClosed V) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ domain V, ∀ q ∈ domain V,
        IsMaxOn (fun r ↦ AE4SGED.logLik w r x) (domain V) p →
        IsMaxOn (fun r ↦ AE4SGED.logLik w r x) (domain V) q → p = q := by
  let O : Set AE4SGED.Parameter := Ioi 1 ×ˢ (univ ×ˢ Ioi 0 ×ˢ univ)
  have hO : IsOpen O := isOpen_Ioi.prod (isOpen_univ.prod (isOpen_Ioi.prod isOpen_univ))
  have hp (p : O) : 0 < p.val.1 := zero_lt_one.trans p.property.1
  have hpc : Continuous (fun p : O ↦ p.val.1) := by fun_prop
  have hl : Continuous (fun p : O ↦ p.val.2.2.1) := by fun_prop
  have hr : Continuous (fun p : O ↦ p.val.2.2.2) := by fun_prop
  have hγ (c : ℝ) (hc : 0 < c) : Continuous (fun p : O ↦ Gamma (c/p.val.1)) := by
    have hh := (continuous_const (y := c)).div hpc (fun p ↦ (hp p).ne')
    rw [continuous_iff_continuousAt]
    intro p
    exact (differentiableAt_Gamma (fun m ↦ by
      have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
      have hpos := div_pos hc (hp p)
      linarith)).continuousAt.comp hh.continuousAt
  let v (p : O) := (p.val.2.2.1)^2 * (Gamma (3/p.val.1)/Gamma (1/p.val.1))
  have hv : Continuous v := (hl.pow 2).mul ((hγ 3 (by norm_num)).div (hγ 1 one_pos)
    (fun p ↦ (Gamma_pos_of_pos (one_div_pos.mpr (hp p))).ne'))
  let C : Set O := {p | p.val.2.2.1 = p.val.2.2.2 ∧ v p ∈ V}
  have hC : IsClosed C := (isClosed_eq hl hr).inter (hV.preimage hv)
  obtain ⟨F,hF,hEq⟩ := isClosed_induced_iff.mp hC
  have he : domain V = O ∩ F := by
    ext p
    constructor
    · intro h
      have ho : p ∈ O := ⟨h.1,mem_univ _,h.2.1,mem_univ _⟩
      have hc : (⟨p,ho⟩ : O) ∈ C := h.2.2
      rw [← hEq] at hc
      exact ⟨ho,hc⟩
    · rintro ⟨ho,hf⟩
      have hc : (⟨p,ho⟩ : O) ∈ Subtype.val ⁻¹' F := hf
      rw [hEq] at hc
      exact ⟨ho.1,ho.2.2.1,hc⟩
  exact AE4SGED.superunit_ae_unique w hw hn _ (he ▸ hO.isLocallyClosed.inter hF.isLocallyClosed)
    (fun _ hp ↦ ⟨hp.1,hp.2.1,hp.2.2.1 ▸ hp.2.1⟩)

end

end UnivariateML.AE4GED

end
