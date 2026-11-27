module

public import UnivariateML.Family.Discrete4BNBinom

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Real
open scoped Topology BigOperators

namespace UnivariateML.Discrete5BNBinom

open Discrete3BNBinom

/-- The beta-negative-binomial mass, using its zero mass and the exact
successive-cell ratios. -/
noncomputable def countMass (k : ℕ) (r a b : ℝ) : ℝ :=
  exp (logLikelihood (fun _ : Fin 1 ↦ 0) r a b) *
    (∏ j ∈ Finset.range k, (((r+(j : ℝ))*(b+(j : ℝ))) /
        ((a+r+b+(j : ℝ))*((j : ℝ)+1))))

private theorem atom_from_limit {ι : Type*} {l : Filter ι} {r a b : ι → ℝ} (k : ℕ)
    (hzero : Tendsto (fun t ↦ exp (logLikelihood (fun _ : Fin 1 ↦ 0)
      (r t) (a t) (b t))) l (𝓝 1))
    (g : ℕ → ℝ) (hg : g 0 = 0)
    (hstep : ∀ j : ℕ, Tendsto (fun t ↦ ((r t+(j : ℝ))*(b t+(j : ℝ))) /
      ((a t+r t+b t+(j : ℝ))*((j : ℝ)+1))) l (𝓝 (g j))) :
    Tendsto (fun t ↦ countMass k (r t) (a t) (b t)) l (𝓝 (if k = 0 then 1 else 0)) := by
  have h := hzero.mul (tendsto_finsetProd (Finset.range k) (fun j _ ↦ hstep j))
  have hg' : (∏ j ∈ Finset.range k, g j) = (if k = 0 then 1 else 0) := by
    by_cases hk : k = 0
    · simp [hk]
    · simp only [ite_eq_right hk]
      exact Finset.prod_eq_zero (Finset.mem_range.mpr (Nat.pos_of_ne_zero hk)) hg
  simpa only [countMass, one_mul, hg'] using h

/-- Each of the three declared zero-atom paths converges at every count.
With a held symmetric shape, use the shrinking path for the free shape
or the shared increasing-alpha path. -/
theorem zero_atom_count_paths (k : ℕ) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun t ↦ countMass k t a b) (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
    Tendsto (fun t ↦ countMass k r a t) (𝓝[>] 0) (𝓝 (if k = 0 then 1 else 0)) ∧
    Tendsto (fun t ↦ countMass k r t b) atTop (𝓝 (if k = 0 then 1 else 0)) := by
  have hz := Discrete4BNBinom.zero_atom_likelihood_paths (by decide : 0 < 1)
    (fun _ : Fin 1 ↦ 0) (fun _ ↦ rfl) hr ha hb
  refine ⟨?_,?_,?_⟩
  · let g (j : ℕ) : ℝ := ((j : ℝ)*(b+(j : ℝ))) / ((a+b+(j : ℝ))*((j : ℝ)+1))
    refine atom_from_limit k hz.2.1 g (by simp [g]) fun j ↦ ?_
    have h : ContinuousAt (fun t : ℝ ↦ ((t+(j : ℝ))*(b+(j : ℝ))) /
        ((a+t+b+(j : ℝ))*((j : ℝ)+1))) 0 := by
      apply ContinuousAt.div
      · fun_prop
      · fun_prop
      · simp only [add_zero]
        positivity
    simpa [g] using h.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  · let g (j : ℕ) : ℝ := ((r+(j : ℝ))*(j : ℝ)) / ((a+r+(j : ℝ))*((j : ℝ)+1))
    refine atom_from_limit k hz.2.2.1 g (by simp [g]) fun j ↦ ?_
    have h : ContinuousAt (fun t : ℝ ↦ ((r+(j : ℝ))*(t+(j : ℝ))) /
        ((a+r+t+(j : ℝ))*((j : ℝ)+1))) 0 := by
      apply ContinuousAt.div
      · fun_prop
      · fun_prop
      · simp only [add_zero]
        positivity
    simpa [g] using h.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  · refine atom_from_limit k hz.2.2.2 (fun _ ↦ 0) rfl fun j ↦ ?_
    have h : Tendsto (fun t : ℝ ↦ t+r+b+(j : ℝ)) atTop atTop := by
      apply tendsto_atTop_mono (fun t ↦ ?_) tendsto_id
      change t ≤ t+r+b+(j : ℝ)
      linarith [Nat.cast_nonneg (α := ℝ) j]
    have hd := h.atTop_mul_const (show 0 < (j : ℝ)+1 by positivity)
    simpa only [div_eq_mul_inv, mul_zero, Function.comp_def] using
      (tendsto_inv_atTop_zero.comp hd).const_mul ((r+(j : ℝ))*(b+(j : ℝ)))

end UnivariateML.Discrete5BNBinom

end
