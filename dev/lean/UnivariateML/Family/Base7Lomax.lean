module

public import UnivariateML.Family.AE2Lomax
public import UnivariateML.Family.TiesLomax

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! The fixed-count, two-level and exceptional uniqueness branches.

## Coordinate conventions
The two-level and exceptional branches use the physical likelihood.
The almost-everywhere branch uses the fixed-count likelihood with its
parameter-independent terms dropped and positive rate as coordinate.
-/

@[expose] public section

namespace UnivariateML.Base7Lomax

open MeasureTheory Set

/-- Almost-everywhere uniqueness on count strata, pointwise uniqueness at
attained two-level samples, and an exact global tie. -/
theorem uniqueness :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (1) 0 w) x p →
          UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (1) 0 w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃! a, a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹ ∧
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        Lomax.bestExp x < Lomax.logLik x p.1 p.2) :=
  ⟨fun w hw ↦ AE2Lomax.lomax_ae_unique w hw,
    fun hn _x hx h2 ↦ fun he ↦ LomaxTwoPoint.joint_unique hn hx h2 he,
    TiesLomax.three_level_global_tie⟩

end UnivariateML.Base7Lomax

end
