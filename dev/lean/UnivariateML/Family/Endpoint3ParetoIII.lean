module

public import UnivariateML.Family.Endpoint2ParetoIII
public import UnivariateML.Family.Endpoint3FellerParetoFreeScale

import Mathlib.Tactic

set_option linter.style.header false

/-! # Pareto III threshold results through the common kernel -/

@[expose] public section

open Real Set Filter
open scoped BigOperators Topology

namespace UnivariateML.Endpoint3ParetoIII

variable {n : ℕ}

private theorem kernel (d : Fin n → ℝ) (t c s : ℝ) (hc : c ≠ 0) :
    ParetoIII.logLik d (-t) (c, s) =
      Endpoint2FellerPareto.logLik d c c c s (log c - log s) t := by
  rw [ParetoIII.logLik_transfer]
  unfold LogLogistic.logLik LogLogistic.logDensity Endpoint2FellerPareto.logLik
  apply Finset.sum_congr rfl
  intro i _
  have h : (c + c) / c * log (1 + ((t + d i) / s) ^ c) =
      2 * log (1 + ((t + d i) / s) ^ c) := by
    rw [← two_mul, mul_div_cancel_right₀ _ hc]
  simp only [ParetoIII.excess, sub_neg_eq_add, add_comm (d i) t, h]

/-- With both Pareto III regular coefficients held, the unique
threshold is contact at unit shape and the unique normalized-score
root is the maximizer above unit shape. -/
theorem held_scale_shape_threshold {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    let L := fun t ↦ ParetoIII.logLik d (-t) (c, s)
    let S := Endpoint2FellerParetoRegular.normalizedScore d c c c s
    (c = 1 → IsMaxOn L (Ici 0) 0 ∧ ∀ t ∈ Ici 0, IsMaxOn L (Ici 0) t → t = 0) ∧
    (1 < c → (∃! t, t ∈ Ioi 0 ∧ S t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ S t = 0) := by
  dsimp only
  simp only [kernel d _ c s hc.ne']
  exact Endpoint3FellerPareto.fixed_regular_threshold_uniqueness
    hd hz hc hc hs (K := log c - log s)

/-- With Pareto III scale held and shape free, a feasible subunit
shape gives unbounded likelihood along strictly containing thresholds. -/
theorem held_scale_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {s : ℝ} (hs : 0 < s) :
    ∀ B : ℝ, ∃ t > 0, ∃ c > 0, B < ParetoIII.logLik d (-t) (c, s) := by
  intro B
  have hlim := Endpoint2ParetoIII.threshold_spike hd hz
    (c := 1 / 2) (by norm_num) (by norm_num) hs
  have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  obtain ⟨t, hgt, ht⟩ := ((hlim.eventually_gt_atTop B).and hpos).exists
  exact ⟨t, ht, 1 / 2, by norm_num, hgt⟩

/-- With Pareto III shape held, either the subunit spike or negative
collapse exponent gives divergence. The regular zero-exponent face
has a strictly unattained finite likelihood supremum. -/
theorem held_shape_failure {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ ParetoIII.logLik d (-v.2) (c, v.1)
    let E := Endpoint2FellerParetoFreeScale.exponent d c
    ((c < 1 ∨ E < 0) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ c ∧ E = 0) → ∃ C : ℝ, IsLUB (L '' D) C ∧ ∀ v ∈ D, L v < C) := by
  dsimp only
  rw [show (fun v : ℝ × ℝ ↦ ParetoIII.logLik d (-v.2) (c, v.1)) =
      Endpoint2FellerParetoFreeScale.logLik d c c c (log c) from
    funext fun v ↦ kernel d v.2 c v.1 hc.ne']
  simpa only [kernel d _ c _ hc.ne', Endpoint2FellerParetoFreeScale.logLik] using
    Endpoint3FellerPareto.fixed_shapes_failure hd hz hc hc (a := c) (K := log c)

/-- When the held Pareto III shape admits a finite fit, its scale
and threshold are unique. The threshold reaches the minimum exactly
at unit shape. -/
theorem held_shape_unique {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) (ha : 1 ≤ c)
    (hE : 0 < Endpoint2FellerParetoFreeScale.exponent d c) :
    let D := Endpoint2FellerParetoFreeScale.domain c
    let L := fun v : ℝ × ℝ ↦ ParetoIII.logLik d (-v.2) (c, v.1)
    ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = 0 ↔ c = 1) := by
  dsimp only
  simp only [kernel d _ c _ hc.ne']
  exact Endpoint3FellerParetoFreeScale.fixed_shapes_unique hd hz hc hc ha hE

end UnivariateML.Endpoint3ParetoIII

end
