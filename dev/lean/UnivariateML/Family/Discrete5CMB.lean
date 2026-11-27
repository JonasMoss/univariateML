module

public import UnivariateML.Family.Discrete4CMB

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Real
open scoped Topology BigOperators

namespace UnivariateML.Discrete5CMB

open Discrete3CMB

private noncomputable def raw (N : ℕ) (ν p : ℝ) (j : Fin (N+1)) : ℝ :=
  exp (ν * height N j) * p ^ j.val * (1-p) ^ (N-j.val)

private theorem raw_left {N : ℕ} (ν : ℝ) (j : Fin (N+1)) :
    raw N ν 0 j = if j = 0 then 1 else 0 := by
  by_cases hj : j = 0
  · subst j
    simp [raw, height]
  · have hj' : j.val ≠ 0 := fun h ↦ hj (Fin.ext h)
    simp [raw, hj, zero_pow hj']

private theorem raw_right {N : ℕ} (ν : ℝ) (j : Fin (N+1)) :
    raw N ν 1 j = if j = Fin.last N then 1 else 0 := by
  by_cases hj : j = Fin.last N
  · subst j
    simp [raw, height]
  · have hj' : N-j.val ≠ 0 := by
      intro h
      have H := j.isLt
      exact hj (Fin.ext (by simp only [Fin.val_last]; omega))
    simp [raw, hj, zero_pow hj']

private theorem raw_scaled {N : ℕ} (ν : ℝ) {p : ℝ} (hp : p ∈ Ioo 0 1)
    (j : Fin (N+1)) :
    raw N ν p j = (1-p)^N * weight N (log (p/(1-p))) ν j := by
  have hq : 0 < 1-p := sub_pos.mpr hp.2
  have hj : j.val + (N-j.val) = N := by have := j.isLt; omega
  have hpow : (1-p)^N = (1-p)^j.val * (1-p)^(N-j.val) :=
    (congrArg ((1-p)^ ·) hj).symm.trans (pow_add (1-p) j.val (N-j.val))
  unfold raw weight
  rw [exp_add, mul_comm (log (p/(1-p))) (j.val : ℝ), exp_nat_mul,
    exp_log (div_pos hp.1 hq), div_pow, hpow]
  field_simp

private theorem single_eq {N : ℕ} (ν : ℝ) {p : ℝ} (hp : p ∈ Icc 0 1)
    (j : Fin (N+1)) :
    closedLikelihood (fun _ : Fin 1 ↦ j) p ν = raw N ν p j / ∑ k, raw N ν p k := by
  by_cases h₀ : p = 0
  · subst p
    simp only [closedLikelihood, ite_true, raw_left, Fin.forall_fin_one]
    by_cases hj : j = 0 <;> simp [hj]
  by_cases h₁ : p = 1
  · subst p
    simp only [closedLikelihood, one_ne_zero, ite_false, ite_true, raw_right,
      Fin.forall_fin_one]
    by_cases hj : j = Fin.last N <;> simp [hj]
  have hi : p ∈ Ioo 0 1 := ⟨lt_of_le_of_ne hp.1 (Ne.symm h₀),lt_of_le_of_ne hp.2 h₁⟩
  have hq : 0 < 1-p := sub_pos.mpr hi.2
  simp only [closedLikelihood, ite_eq_right h₀, ite_eq_right h₁,
    likelihood, Fin.prod_univ_one, raw_scaled ν hi, ← Finset.mul_sum]
  rw [mul_div_mul_left _ _ (pow_pos hq _).ne']

private theorem denominator_cont (N : ℕ) (ν : ℝ) : Continuous (fun p ↦ ∑ k, raw N ν p k) := by
  unfold raw
  fun_prop

private theorem raw_closed_endpoint {N : ℕ} (ν : ℝ) (j e : Fin (N+1)) (p : ℝ)
    (he : ∀ k, raw N ν p k = if k = e then 1 else 0) (s : Set ℝ)
    (hs : s ⊆ Icc 0 1) :
    Tendsto (fun q ↦ closedLikelihood (fun _ : Fin 1 ↦ j) q ν) (𝓝[s] p)
      (𝓝 (if j = e then 1 else 0)) := by
  have hm : (∑ k, raw N ν p k) = 1 := by simp [he]
  have hc : Continuous (fun q ↦ raw N ν q j) := by unfold raw; fun_prop
  have h := hc.continuousAt.tendsto.div (denominator_cont N ν).continuousAt.tendsto
    (by rw [hm]; exact one_ne_zero)
  rw [hm, he, div_one] at h
  apply (h.mono_left nhdsWithin_le_nhds).congr'
  have hf : ∀ᶠ q in 𝓝[s] p, q ∈ s := self_mem_nhdsWithin
  exact hf.mono fun q hq ↦ (single_eq ν (hs hq) j).symm

/-- Fixed finite dispersion admits both deterministic probability limits.
The size-zero support has a single cell, which carries both endpoint limits. -/
theorem held_dispersion_probability_endpoints {N : ℕ} (ν : ℝ) (j : Fin (N+1)) :
    Tendsto (fun p ↦ closedLikelihood (fun _ : Fin 1 ↦ j) p ν)
      (𝓝[Icc (0 : ℝ) 1] 0) (𝓝 (if j = 0 then 1 else 0)) ∧
    Tendsto (fun p ↦ closedLikelihood (fun _ : Fin 1 ↦ j) p ν)
      (𝓝[Icc (0 : ℝ) 1] 1) (𝓝 (if j = Fin.last N then 1 else 0)) :=
  ⟨raw_closed_endpoint ν j 0 0 (raw_left ν) _ Subset.rfl,
    raw_closed_endpoint ν j (Fin.last N) 1 (raw_right ν) _ Subset.rfl⟩

end UnivariateML.Discrete5CMB

end
