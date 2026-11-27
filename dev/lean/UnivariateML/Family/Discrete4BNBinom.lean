module

public import UnivariateML.Family.Discrete3BNBinom

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Beta-negative-binomial zero-count branches and held alpha

The zero-atom paths here give limits of the likelihood on all-zero data.
Pointwise mass convergence on other counts and the positive-data face
comparisons require additional arguments.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators

namespace UnivariateML.Discrete4BNBinom

open Discrete3BNBinom

/-- On all-zero data the likelihood is strictly below one at every
positive triple. Each of the three free-coordinate routes reaches one. -/
theorem zero_atom_likelihood_paths {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    exp (logLikelihood x r a b) < 1 ∧
      Tendsto (fun t ↦ exp (logLikelihood x t a b)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun t ↦ exp (logLikelihood x r a t)) (𝓝[>] 0) (𝓝 1) ∧
      Tendsto (fun t ↦ exp (logLikelihood x r t b)) atTop (𝓝 1) := by
  obtain ⟨h,h₁,h₂,h₃⟩ := all_zero_held_faces hn hx hr ha hb
  exact ⟨exp_lt_one_iff.mpr h, by simpa [Function.comp_def] using continuous_exp.continuousAt.tendsto.comp h₁,
    by simpa [Function.comp_def] using continuous_exp.continuousAt.tendsto.comp h₂, by simpa [Function.comp_def] using continuous_exp.continuousAt.tendsto.comp h₃⟩

private theorem no_max_zero {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) (p : ℝ × ℝ) (hp : p ∈ Ioi 0 ×ˢ Ioi 0) {a : ℝ} (ha : 0 < a) :
    ¬ IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) p := by
  intro H
  have h := all_zero_held_faces hn hx hp.1 ha hp.2
  have hB : 0 ≤ logLikelihood x p.1 a p.2 := by
    apply le_of_tendsto h.2.1
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    exact H (show (t,p.2) ∈ Ioi (0 : ℝ) ×ˢ Ioi 0 from ⟨ht,hp.2⟩)
  exact h.1.not_ge hB

/-- At any positive held alpha, a finite joint size and beta fit exists
exactly when a positive count occurs. -/
theorem held_alpha_attainment_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi 0,
      IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) p) ↔
      ∃ i, 0 < x i := by
  constructor
  · rintro ⟨p,hp,hM⟩
    by_contra! h
    exact no_max_zero hn x (fun i ↦ Nat.eq_zero_of_le_zero (h i)) p hp ha hM
  · exact fun h ↦ held_alpha_attained hn h ha

/-- All-zero data have a strictly unattained supremum over every
positive triple, with a feasible path shrinking size alone. -/
theorem all_free_zero_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0 ×ˢ Ioi 0
    let L := fun p : ℝ × ℝ × ℝ ↦ exp (logLikelihood x p.1 p.2.1 p.2.2)
    IsLUB (L '' D) 1 ∧ ∀ p ∈ D, L p < 1 := by
  intro D L
  have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : L p < 1 :=
    (zero_atom_likelihood_paths hn x hx hp.1 hp.2.1 hp.2.2).1
  refine ⟨⟨?_,?_⟩,hstrict⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hstrict p hp).le
  · intro C hC
    have ht := (zero_atom_likelihood_paths hn x hx zero_lt_one zero_lt_one zero_lt_one).2.1
    apply le_of_tendsto ht
    filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with t ht
    exact hC (mem_image_of_mem L (show (t,1,1) ∈ D from ⟨ht,zero_lt_one,zero_lt_one⟩))

end UnivariateML.Discrete4BNBinom

end
