module

public import UnivariateML.Family.LomaxTwoPoint
public import UnivariateML.Family.ShapeFacesParetoIV

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Transfer of the two-level Lomax singleton

## Deviations

The theorem in each family covers the pointwise two-level branch. The
almost-everywhere conclusion on higher count strata is separate.
-/

@[expose] public section

open Real Set

namespace UnivariateML.Endpoint3LomaxTransfer

variable {n : ℕ}

private theorem pair_bij {f g : ℝ → ℝ}
    (hf : ∀ x > 0, 0 < f x) (hg : ∀ x > 0, 0 < g x)
    (hl : ∀ x > 0, g (f x) = x) (hr : ∀ x > 0, f (g x) = x) :
    BijOn (fun v : ℝ × ℝ ↦ (v.1, f v.2)) Lomax.domain Lomax.domain := by
  refine ⟨fun v hv ↦ ⟨hv.1, hf _ hv.2⟩, ?_, ?_⟩
  · intro u hu v hv he
    have h₁ := congrArg (fun p : ℝ × ℝ ↦ p.1) he
    have h₂ := congrArg (fun p : ℝ × ℝ ↦ g p.2) he
    exact Prod.ext h₁ (by simpa only [hl _ hu.2, hl _ hv.2] using h₂)
  · intro v hv
    exact ⟨(v.1, g v.2), ⟨hv.1, hg _ hv.2⟩, Prod.ext rfl (hr _ hv.2)⟩

private theorem two_level_transfer {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b)
    {L : ℝ × ℝ → ℝ} {φ : ℝ × ℝ → ℝ × ℝ} {C : ℝ}
    (hφ : BijOn φ Lomax.domain Lomax.domain)
    (he : ∀ v ∈ Lomax.domain, L v = Lomax.logLik x (φ v).1 (φ v).2 + C) :
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v := by
  intro h
  exact (Transfer.existsUnique_max_iff hφ he).mpr
    (LomaxTwoPoint.joint_unique hn hx h2 ((Transfer.exists_max_iff hφ he).mp h))

/-- An attained two-level Pareto II fit at a held threshold is unique. -/
theorem paretoII_two_level_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ v.1 v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v := by
  have hφ := pair_bij (f := Inv.inv) (g := Inv.inv)
    (fun _ h ↦ inv_pos.mpr h) (fun _ h ↦ inv_pos.mpr h)
    (fun _ _ ↦ inv_inv _) (fun _ _ ↦ inv_inv _)
  obtain ⟨a, b, h⟩ := h2
  exact two_level_transfer (x := Transfer.ParetoII.excess x μ) hn
    (fun i ↦ sub_pos.mpr (hx i))
    ⟨a - μ, b - μ, fun i ↦ (h i).imp (fun hi ↦ congrArg (· - μ) hi)
      (fun hi ↦ congrArg (· - μ) hi)⟩
    hφ (fun v _ ↦ (Transfer.ParetoII.logLik_transfer x μ v.1 v.2).trans (add_zero _).symm)

/-- An attained two-level inverse Pareto fit is unique. -/
theorem inverse_pareto_two_level_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik x v.1 v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v := by
  obtain ⟨a, b, h⟩ := h2
  exact two_level_transfer (x := Transfer.reciprocal x) hn
    (fun i ↦ inv_pos.mpr (hx i))
    ⟨a⁻¹, b⁻¹, fun i ↦ (h i).imp (congrArg Inv.inv) (congrArg Inv.inv)⟩
    (bijOn_id _) (fun v _ ↦ Transfer.InvPareto.logLik_transfer x v.1 v.2)

/-- An attained two-level Pareto IV fit at held threshold and power is
unique. Raising the positive excesses to the held power preserves
the two levels and transfers to the Lomax singleton. -/
theorem paretoIV_two_level_unique (hn : 0 < n) {x : Fin n → ℝ} {μ c : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (h2 : ∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) :
    let L := fun v : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ v.1 c v.2
    (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
      ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v := by
  let y := ShapeFacesParetoIV.excess x μ
  have hy : ∀ i, 0 < y i := fun i ↦ sub_pos.mpr (hx i)
  let f := Transfer.Burr.rate c
  let g := fun r : ℝ ↦ r⁻¹ ^ c⁻¹
  have hφ := pair_bij (f := f) (g := g)
    (fun s hs ↦ inv_pos.mpr (rpow_pos_of_pos hs c))
    (fun r hr ↦ rpow_pos_of_pos (inv_pos.mpr hr) _)
    (fun s hs ↦ by
      dsimp [f, g, Transfer.Burr.rate]
      rw [inv_inv, ← rpow_mul hs.le, mul_inv_cancel₀ hc.ne', rpow_one])
    (fun r hr ↦ by
      dsimp [f, g, Transfer.Burr.rate]
      rw [← rpow_mul (inv_pos.mpr hr).le, inv_mul_cancel₀ hc.ne', rpow_one, inv_inv])
  obtain ⟨a, b, h⟩ := h2
  apply two_level_transfer (x := Transfer.Burr.powered y c)
    (C := Transfer.Burr.powerJac y c) hn
    (fun i ↦ rpow_pos_of_pos (hy i) c)
    ⟨(a - μ) ^ c, (b - μ) ^ c, fun i ↦
      (h i).imp (congrArg (fun t ↦ (t - μ) ^ c)) (congrArg (fun t ↦ (t - μ) ^ c))⟩ hφ
  intro v hv
  rw [ShapeFacesParetoIV.logLik_excess]
  exact Transfer.Burr.logLik_transfer hy c v.1 hv.2

end UnivariateML.Endpoint3LomaxTransfer

end
