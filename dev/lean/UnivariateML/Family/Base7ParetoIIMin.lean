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

namespace UnivariateML.Base7ParetoIIMin

open MeasureTheory Set

private theorem zero_threshold_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x 0 < Transfer.ParetoII.logLik x 0 q.1 q.2 := by
  obtain ⟨a, ⟨ha, h⟩, _⟩ := TiesLomax.three_level_global_tie
  let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  have ha' : 0 < a := (by positivity : 0 < ((2 : ℝ) ^ 512)⁻¹).trans ha.1
  have hx (i : Fin 21) : 0 < x i := by dsimp [x]; split_ifs <;> positivity
  have hexc : Transfer.ParetoII.excess x 0 = x := by
    ext i
    simp [Transfer.ParetoII.excess]
  obtain ⟨p, hp, q, hq, hps, hqs, hpm, hqm, hall, hexp⟩ := h
  let φ (v : ℝ × ℝ) : ℝ × ℝ := (v.1, v.2⁻¹)
  have hinv (v : ℝ × ℝ) : φ (φ v) = v := by simp [φ]
  have hdom (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) : φ v ∈ Lomax.domain :=
    ⟨hv.1, inv_pos.mpr hv.2⟩
  have hφ : BijOn φ Lomax.domain Lomax.domain := by
    refine ⟨hdom, ?_, ?_⟩
    · intro v _ w _ h
      simpa only [hinv] using congrArg φ h
    · exact fun v hv ↦ ⟨φ v, hdom v hv, hinv v⟩
  have hlik (v : ℝ × ℝ) :
      Transfer.ParetoII.logLik x 0 v.1 v.2 = Lomax.logLik x (φ v).1 (φ v).2 := by
    rw [Transfer.ParetoII.logLik_transfer, hexc]
  have hmap (v : ℝ × ℝ) (hv : v ∈ Lomax.domain) :
      IsMaxOn (fun b : ℝ × ℝ ↦ Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v ↔
        IsMaxOn (fun b : ℝ × ℝ ↦ Lomax.logLik x b.1 b.2) Lomax.domain (φ v) :=
    Transfer.isMaxOn_iff (c := 0) hφ (by intro b _; simp only [add_zero, hlik]) hv
  refine ⟨a, ha, hx, φ q, φ p, (inv_lt_inv₀ hq.2 hp.2).mpr (hps.trans hqs), ?_, ?_⟩
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      rcases hall (φ v) (hdom v hv) ((hmap v hv).mp hm) with h | h
      · exact Or.inr (by simpa only [hinv] using congrArg φ h)
      · exact Or.inl (by simpa only [hinv] using congrArg φ h)
    · rintro (rfl | rfl)
      · exact ⟨hdom q hq, (hmap _ (hdom q hq)).mpr (by simpa only [hinv] using hqm)⟩
      · exact ⟨hdom p hp, (hmap _ (hdom p hp)).mpr (by simpa only [hinv] using hpm)⟩
  · change Transfer.ParetoII.bestShiftedExp x 0 < Transfer.ParetoII.logLik x 0 (φ p).1 (φ p).2
    rw [Transfer.ParetoII.bestShiftedExp, hexc, hlik (φ p), hinv]
    exact hexp

/-- Almost-everywhere uniqueness on count strata, pointwise uniqueness at
attained two-level samples, and an exact global tie. -/
theorem uniqueness (μ : ℝ) :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
        ∀ p q, UniqueAE.Winner (AE2Lomax.domain μ) (AE2Lomax.likelihood (1) μ w) x p →
          UniqueAE.Winner (AE2Lomax.domain μ) (AE2Lomax.likelihood (1) μ w) x q → p = q) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, μ < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.ParetoII.logLik x μ v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x 0 < Transfer.ParetoII.logLik x 0 q.1 q.2) :=
  ⟨fun w hw ↦ AE2Lomax.pareto2_held_min_ae_unique μ w hw,
    fun hn _x hx h2 ↦ Endpoint3LomaxTransfer.paretoII_two_level_unique hn hx h2,
    zero_threshold_three_level_tie⟩

end UnivariateML.Base7ParetoIIMin

end
