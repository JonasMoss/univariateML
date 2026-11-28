module

public import UnivariateML.Family.GBTTrGamma

set_option linter.style.header false

/-! The large-power upper envelope for inverse transformed gamma.

## Deviations

The envelopes treat both ends of the power coordinate. The compact
stationary comparison is a separate obligation.
-/

@[expose] public section

open Real Filter Topology
namespace UnivariateML.GBTInvTrGamma

/-- For nonconstant positive samples, the Pareto face bounds the
large-power exterior uniformly in the two nuisance parameters. The index
of the sample minimum need not be unique. -/
theorem pareto_exterior {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, x j ≤ x i)
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ p : ℝ in atTop, ∀ k > 0, ∀ a > 0,
      InverseTransformedGamma.logLik x k p a <
        log (Gamma.avgLog x - log (x j))⁻¹ - Gamma.avgLog x - 1 + ε := by
  let y := InverseTransformedGamma.reciprocal x
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (hx i)
  have hne' : ∃ i j, y i ≠ y j := by
    obtain ⟨i, l, hil⟩ := hne
    exact ⟨i, l, fun h ↦ hil (inv_injective h)⟩
  have hmax (i : Fin n) : log (y i) ≤ log (y j) :=
    log_le_log (hy i) ((inv_le_inv₀ (hx i) (hx j)).mpr (hj i))
  have havg : Gamma.avgLog y = -Gamma.avgLog x := by
    simp [y, Gamma.avgLog, Gamma.avg, InverseTransformedGamma.reciprocal, log_inv,
      Finset.sum_neg_distrib, neg_div]
  filter_upwards [GBTTrGamma.power_exterior hn hy hne' j hmax hε] with p hp
  intro k hk a ha
  have h := hp k hk a⁻¹ (inv_pos.mpr ha)
  rw [InverseTransformedGamma.logLik_reciprocal]
  change TransformedGamma.logLik y k p a⁻¹ + -2 * Gamma.avgLog x < _
  have he : log (y j) - Gamma.avgLog y = Gamma.avgLog x - log (x j) := by
    rw [havg]
    simp only [y, InverseTransformedGamma.reciprocal, log_inv]
    ring
  rw [he, havg] at h
  linarith

/-- The fitted lognormal upper envelope is invariant under the reciprocal
transfer after adding the Jacobian on the original sample. -/
theorem lognormal_exterior {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {ε : ℝ} (hε : 0 < ε) :
    let V := Gamma.avg (fun i ↦ (log (x i) - Gamma.avgLog x)^2)
    ∀ᶠ t : ℝ in 𝓝[>] 0, ∀ k > 0, ∀ a > 0,
      InverseTransformedGamma.logLik x k t a <
        -log (2 * π) / 2 - log V / 2 - 1 / 2 - Gamma.avgLog x + ε := by
  intro V
  let y := InverseTransformedGamma.reciprocal x
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (hx i)
  have hn' : ∃ i j, y i ≠ y j := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij (inv_injective h)⟩
  have havg : Gamma.avgLog y = -Gamma.avgLog x := by
    simp [y, Gamma.avgLog, Gamma.avg, InverseTransformedGamma.reciprocal, log_inv,
      Finset.sum_neg_distrib, neg_div]
  have hV : Gamma.avg (fun i ↦ (log (y i) - Gamma.avgLog y)^2) = V := by
    rw [havg]
    dsimp [V, y, InverseTransformedGamma.reciprocal]
    congr 1
    funext i
    rw [log_inv, show -log (x i) - -Gamma.avgLog x =
      -(log (x i) - Gamma.avgLog x) by ring, neg_sq]
  filter_upwards [GBTTrGamma.lognormal_exterior hn hy hn' hε] with t ht
  intro k hk a ha
  have h := ht k hk a⁻¹ (inv_pos.mpr ha)
  rw [hV, havg] at h
  rw [InverseTransformedGamma.logLik_reciprocal]
  change TransformedGamma.logLik y k t a⁻¹ + -2 * Gamma.avgLog x < _
  linarith

/-- The attainment comparison, including the unbounded constant branch,
transports to inverse transformed gamma with its lower-cutoff Pareto value. -/
theorem attainment_witness_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let V := Gamma.avg (fun i ↦ (log (x i) - Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - Gamma.avgLog x
    let P := log (Gamma.avgLog x - log (x j))⁻¹ - Gamma.avgLog x - 1
    ((∃ i j, x i ≠ x j) →
      ((∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
          InverseTransformedGamma.logLik x k' t' a' ≤
            InverseTransformedGamma.logLik x k t a) ↔
        ∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ InverseTransformedGamma.logLik x k t a)) ∧
    ((∀ i j, x i = x j) →
      ∀ C : ℝ, ∃ k > 0, ∃ t > 0, ∃ a > 0, C < InverseTransformedGamma.logLik x k t a) := by
  intro V N P
  let y := InverseTransformedGamma.reciprocal x
  let J := -2 * Gamma.avgLog x
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (hx i)
  have hymax (i : Fin n) : log (y i) ≤ log (y j) :=
    log_le_log (hy i) ((inv_le_inv₀ (hx i) (hx j)).mpr (hj i))
  have hyavg : Gamma.avgLog y = -Gamma.avgLog x := by
    simp [y, Gamma.avgLog, Gamma.avg, InverseTransformedGamma.reciprocal, log_inv,
      Finset.sum_neg_distrib, neg_div]
  have hV : Gamma.avg (fun i ↦ (log (y i) - Gamma.avgLog y)^2) = V := by
    rw [hyavg]
    dsimp [y, V, InverseTransformedGamma.reciprocal]
    congr 1
    funext i
    rw [log_inv, show -log (x i) - -Gamma.avgLog x =
      -(log (x i) - Gamma.avgLog x) by ring, neg_sq]
  have hD : log (y j) - Gamma.avgLog y = Gamma.avgLog x - log (x j) := by
    rw [hyavg]
    simp only [y, InverseTransformedGamma.reciprocal, log_inv]
    ring
  have hL (k t a : ℝ) : InverseTransformedGamma.logLik x k t a =
      TransformedGamma.logLik y k t a⁻¹ + J :=
    InverseTransformedGamma.logLik_reciprocal x k t a
  have hT := GBTTrGamma.attainment_witness_criterion hn hy j hymax
  dsimp only at hT
  rw [hV, hD, hyavg] at hT
  have hB : max (-log (2 * π) / 2 - log V / 2 - 1 / 2 - -Gamma.avgLog x)
      (log (Gamma.avgLog x - log (x j))⁻¹ - -Gamma.avgLog x - 1) + J = max N P := by
    rw [← max_add_add_right]
    congr 1 <;> dsimp [N, P, J] <;> ring
  constructor
  · intro hne
    have hyne : ∃ i l, y i ≠ y l := by
      obtain ⟨i, l, hil⟩ := hne
      exact ⟨i, l, fun h ↦ hil (inv_injective h)⟩
    have h := hT.1 hyne
    constructor
    · rintro ⟨k, hk, t, ht, a, ha, hm⟩
      have hf : ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
          TransformedGamma.logLik y k' t' a' ≤ TransformedGamma.logLik y k t a⁻¹ := by
        intro k' hk' t' ht' a' ha'
        have h' := hm k' hk' t' ht' a'⁻¹ (inv_pos.mpr ha')
        rw [hL, hL, inv_inv] at h'
        exact le_of_add_le_add_right h'
      obtain ⟨k', hk', t', ht', a', ha', h'⟩ := h.mp ⟨k, hk, t, ht, a⁻¹, inv_pos.mpr ha, hf⟩
      refine ⟨k', hk', t', ht', a'⁻¹, inv_pos.mpr ha', ?_⟩
      rw [hL, inv_inv, ← hB]
      exact add_le_add_left h' J
    · rintro ⟨k, hk, t, ht, a, ha, hw⟩
      rw [hL, ← hB] at hw
      obtain ⟨k', hk', t', ht', a', ha', hm⟩ :=
        h.mpr ⟨k, hk, t, ht, a⁻¹, inv_pos.mpr ha, le_of_add_le_add_right hw⟩
      refine ⟨k', hk', t', ht', a'⁻¹, inv_pos.mpr ha', fun k'' hk'' t'' ht'' a'' ha'' ↦ ?_⟩
      rw [hL, hL, inv_inv]
      exact add_le_add_left (hm k'' hk'' t'' ht'' a''⁻¹ (inv_pos.mpr ha'')) J
  · intro hc C
    obtain ⟨k, hk, t, ht, a, ha, h⟩ := hT.2
      (fun i l ↦ congrArg Inv.inv (hc i l)) (C - J)
    refine ⟨k, hk, t, ht, a⁻¹, inv_pos.mpr ha, ?_⟩
    rw [hL, inv_inv]
    linarith

end UnivariateML.GBTInvTrGamma
end
