module

public import UnivariateML.Family.AE2Lomax
public import UnivariateML.Family.TiesLomax
public import UnivariateML.Family.Endpoint3LomaxTransfer

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! The fixed-count, two-level and exceptional uniqueness branches.

## Coordinate conventions
The two-level and exceptional branches use the physical likelihood.
The almost-everywhere branch uses the fixed-count likelihood with its
parameter-independent terms dropped and positive rate as coordinate.
The exceptional branch gives the explicit transferred three-level sample.
-/

@[expose] public section

namespace UnivariateML.Base7InversePareto

open MeasureTheory Set

private theorem reciprocal_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then 1 else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2 := by
  obtain ⟨a, ⟨ha, h⟩, _⟩ := TiesLomax.three_level_global_tie
  let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  let y := Transfer.reciprocal x
  have ha' : 0 < a := (by positivity : 0 < ((2 : ℝ) ^ 512)⁻¹).trans ha.1
  have hx (i : Fin 21) : 0 < x i := by dsimp [x]; split_ifs <;> positivity
  have hy (i : Fin 21) : 0 < y i := inv_pos.mpr (hx i)
  have hrec : Transfer.reciprocal y = x := by
    ext i
    simp [y, Transfer.reciprocal]
  obtain ⟨p, hp, q, hq, hps, hqs, hpm, hqm, hall, hexp⟩ := h
  have hlik (b : ℝ × ℝ) :
      Transfer.InvPareto.logLik y b.1 b.2 =
        Lomax.logLik x b.1 b.2 + Transfer.reciprocalJac y := by
    rw [Transfer.InvPareto.logLik_transfer, hrec]
  have hmap (b : ℝ × ℝ) (hb : b ∈ Lomax.domain) :
      IsMaxOn (fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik y v.1 v.2) Lomax.domain b ↔
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b :=
    Transfer.isMaxOn_iff (bijOn_id Lomax.domain) (fun v _ ↦ hlik v) hb
  refine ⟨a, ha, hy, p, q, hps.trans hqs, ?_, ?_⟩
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      exact hall v hv ((hmap v hv).mp hm)
    · rintro (rfl | rfl)
      · exact ⟨hp, (hmap _ hp).mpr hpm⟩
      · exact ⟨hq, (hmap _ hq).mpr hqm⟩
  · change Transfer.InvPareto.bestInvExp y < Transfer.InvPareto.logLik y p.1 p.2
    rw [Transfer.InvPareto.bestInvExp, hrec, hlik p]
    exact add_lt_add_left hexp _

/-- Almost-everywhere uniqueness on count strata, pointwise uniqueness at
attained two-level samples, and an exact global tie. -/
theorem uniqueness :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (-1) 0 w) x p →
          UniqueAE.Winner (AE2Lomax.domain 0) (AE2Lomax.likelihood (-1) 0 w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then 1 else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2) :=
  ⟨fun w hw ↦ AE2Lomax.invpareto_ae_unique w hw,
    fun hn _x hx h2 ↦ Endpoint3LomaxTransfer.inverse_pareto_two_level_unique hn hx h2,
    reciprocal_three_level_tie⟩

end UnivariateML.Base7InversePareto

end
