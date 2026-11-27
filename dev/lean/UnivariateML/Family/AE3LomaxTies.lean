module

public import UnivariateML.Family.AE2BetaPrime
public import UnivariateML.Family.AE2Lomax
public import UnivariateML.Family.TiesLomax
import Mathlib.Tactic

set_option linter.style.header false

/-! Exact unit-shape transfers of the three-level Lomax crossing. -/

@[expose] public section

open Set Real

namespace UnivariateML.AE3LomaxTies

noncomputable section

/-- The exact Lomax crossing sample, with its witness as the first level. -/
def crossingSample (a : ℝ) (i : Fin 21) : ℝ :=
  if i.val = 0 then a else if i.val ≤ 10 then 1 else 100

private theorem positive_crossing {a : ℝ} (ha : 0 < a) (i : Fin 21) :
    0 < crossingSample a i := by
  unfold crossingSample
  split
  · exact ha
  · split <;> norm_num

private theorem crossing_transfers (T : (Fin 21 → ℝ) → (ℝ × ℝ) → ℝ)
    (h : ∀ x : Fin 21 → ℝ, (∀ i, 0 < x i) →
      ∃ C : ℝ, ∀ p ∈ Lomax.domain, T x p = Lomax.logLik x p.1 p.2 + C) :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (T (crossingSample a)) Lomax.domain v} = {p, q} := by
  obtain ⟨a, ⟨ha, ht⟩, _⟩ := TiesLomax.three_level_global_tie
  obtain ⟨p, hp, q, hq, hps, hqs, hm₁, hm₂, hall, _⟩ := ht
  let x := crossingSample a
  have hx : ∀ i, 0 < x i := positive_crossing ((by positivity : 0 < ((2 : ℝ) ^ 512)⁻¹).trans ha.1)
  obtain ⟨C, hC⟩ := h x hx
  have ht (b : ℝ × ℝ) (hb : b ∈ Lomax.domain) :
      IsMaxOn (T x) Lomax.domain b ↔
        IsMaxOn (fun v ↦ Lomax.logLik x v.1 v.2) Lomax.domain b :=
    Transfer.isMaxOn_iff (bijOn_id Lomax.domain) hC hb
  refine ⟨a, ha, p, q, hps.trans hqs, ?_⟩
  ext b
  simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
  constructor
  · rintro ⟨hb, hm⟩
    exact hall b hb ((ht b hb).mp hm)
  · rintro (rfl | rfl)
    · exact ⟨hp, (ht _ hp).mpr hm₁⟩
    · exact ⟨hq, (ht _ hq).mpr hm₂⟩

private theorem beta_one {k : ℝ} (hk : 0 < k) :
    Beta.logBeta (k, 1) = - log k := by
  unfold Beta.logBeta Beta.normalizer
  change log (Gamma k * Gamma 1 / Gamma (k + 1)) = _
  rw [Gamma_one, mul_one, Gamma_add_one hk.ne',
    show Gamma k / (k * Gamma k) = 1 / k by
      field_simp [hk.ne', (Gamma_pos_of_pos hk).ne'],
    log_div one_ne_zero hk.ne', log_one, zero_sub]

private theorem beta_symm (k l : ℝ) : Beta.logBeta (k, l) = Beta.logBeta (l, k) := by
  unfold Beta.logBeta Beta.normalizer
  change log (Gamma k * Gamma l / Gamma (k + l)) =
    log (Gamma l * Gamma k / Gamma (l + k))
  rw [mul_comm, add_comm]

/-- At beta-prime lower shape one, the exact crossing has two winners.
The chart uses upper shape and reciprocal scale. -/
theorem betapr_held_shape1_unit_tie :
    let param : ℝ × ℝ → AE2BetaPrime.Parameters := fun v ↦ (1, v.1, v.2⁻¹)
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2BetaPrime.likelihood param (fun _ ↦ 1) v (crossingSample a))
          Lomax.domain v} = {p, q} := by
  dsimp only
  apply crossing_transfers (fun x v ↦
    AE2BetaPrime.likelihood (fun v ↦ (1, v.1, v.2⁻¹)) (fun _ ↦ 1) v x)
  intro x hx
  refine ⟨0, fun p hp ↦ ?_⟩
  have hr : 0 < p.2 := hp.2
  have hk : 0 < p.1 := hp.1
  rw [add_zero]
  unfold Lomax.logLik AE2BetaPrime.likelihood
  apply Finset.sum_congr rfl
  intro i _
  have harg : x i + p.2⁻¹ = (1 + p.2 * x i) * p.2⁻¹ := by
    field_simp [hr.ne']
    ring
  have hpos : 0 < 1 + p.2 * x i := by have := hx i; positivity
  simp only [Lomax.logDensity, Nat.cast_one, one_mul]
  rw [beta_symm, beta_one hk, harg, log_mul hpos.ne' (inv_ne_zero hr.ne'), log_inv]
  ring

/-- At beta-prime upper shape one, the reciprocal exact crossing has two
winners in lower shape and scale coordinates. -/
theorem betapr_held_shape2_unit_tie :
    let param : ℝ × ℝ → AE2BetaPrime.Parameters := fun v ↦ (v.1, 1, v.2)
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2BetaPrime.likelihood param (fun _ ↦ 1) v (fun i ↦ (crossingSample a i)⁻¹))
        Lomax.domain v} = {p, q} := by
  dsimp only
  apply crossing_transfers (fun x v ↦ AE2BetaPrime.likelihood (fun v ↦ (v.1, 1, v.2))
    (fun _ ↦ 1) v (fun i ↦ (x i)⁻¹))
  intro x hx
  refine ⟨∑ i, 2 * log (x i), fun p hp ↦ ?_⟩
  have hr : 0 < p.2 := hp.2
  have hk : 0 < p.1 := hp.1
  unfold Lomax.logLik AE2BetaPrime.likelihood
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have harg : (x i)⁻¹ + p.2 = (1 + p.2 * x i) * (x i)⁻¹ := by
    field_simp [(hx i).ne']
  have hpos : 0 < 1 + p.2 * x i := by have := hx i; positivity
  simp only [Lomax.logDensity, Nat.cast_one, one_mul]
  rw [beta_one hk, harg, log_mul hpos.ne' (inv_ne_zero (hx i).ne'), log_inv]
  ring

/-- At Pareto IV power one and held minimum, translating the exact Lomax
crossing preserves the two-winner set in shape and rate coordinates. -/
theorem pareto4_held_min_unit_power_tie (m : ℝ) :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹, ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
      {v ∈ Lomax.domain | IsMaxOn (fun v ↦
        AE2Lomax.likelihood 1 m (fun _ ↦ 1) v (fun i ↦ crossingSample a i + m))
        Lomax.domain v} = {p, q} := by
  apply crossing_transfers (fun x v ↦ AE2Lomax.likelihood 1 m (fun _ ↦ 1) v (fun i ↦ x i + m))
  intro x hx
  refine ⟨0, fun p _ ↦ ?_⟩
  unfold AE2Lomax.likelihood Lomax.logLik Lomax.logDensity
  simp only [add_zero, Nat.cast_one, one_mul, add_sub_cancel_right]
  simp only [exp_log (hx _)]

end

end UnivariateML.AE3LomaxTies

end
