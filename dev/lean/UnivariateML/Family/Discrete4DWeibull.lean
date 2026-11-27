module

public import UnivariateML.Family.HeldSweepDWeibull

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Discrete Weibull excluded held-mask faces and point paths

The probability coordinate is `a = log (-log q)`. Sending it to positive
infinity sends the survival probability `q` to zero.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4DWeibull

open DiscreteWeibull HeldSweepDWeibull

/-- The sample likelihood on the probability scale. -/
noncomputable def likelihood {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ) : ℝ :=
  ∏ i, mass (x i) p

private theorem binary_mass {β : ℝ} (hβ : 0 < β) (a : ℝ) {k : ℕ} (hk : k ≤ 1) :
    0 < mass k (a,β) ∧
      mass k (a,β) ≤ (if k = 0 then 1-exp (-exp a) else exp (-exp a)) ∧
      (k = 1 → mass k (a,β) < exp (-exp a)) := by
  have hq : exp (-exp a) < 1 := exp_lt_one_iff.mpr (neg_neg_of_pos (exp_pos _))
  interval_cases k
  · simp only [mass,survival,endpoint,Nat.reduceAdd,Nat.one_ne_zero,ite_true,ite_false,
      Nat.cast_one,log_one,mul_zero,add_zero]
    exact ⟨sub_pos.mpr hq,le_rfl,by omega⟩
  · have h₂ : a < a+β*log (2 : ℝ) := lt_add_of_pos_right _ (mul_pos hβ (log_pos (by norm_num)))
    have hp : exp (-exp (a+β*log (2 : ℝ))) < exp (-exp a) :=
      exp_lt_exp.mpr (neg_lt_neg (exp_lt_exp.mpr h₂))
    have he : mass 1 (a,β) = exp (-exp a) - exp (-exp (a+β*log (2 : ℝ))) := by
      norm_num [mass,survival,endpoint]
    rw [he,ite_eq_right one_ne_zero]
    exact ⟨sub_pos.mpr hp,sub_le_self _ (exp_pos _).le,fun _ ↦ sub_lt_self _ (exp_pos _)⟩

/-- Holding probability fixed on a binary sample with a positive count
gives a strict excluded Bernoulli supremum at infinite shape. -/
theorem held_probability_binary_supremum {n : ℕ} (x : Fin n → ℕ) (a : ℝ)
    (hx : ∀ i, x i ≤ 1) (hp : ∃ i, x i = 1) :
    let L := fun β ↦ likelihood x (a,β)
    let U := ∏ i, if x i = 0 then 1-exp (-exp a) else exp (-exp a)
    Tendsto L atTop (𝓝 U) ∧ IsLUB (L '' Ioi 0) U ∧ ∀ β > 0, L β < U := by
  intro L U
  have hlim : Tendsto L atTop (𝓝 U) := by
    unfold L U likelihood
    apply tendsto_finsetProd
    intro i _
    have hh := hx i
    have he : x i = 0 ∨ x i = 1 := by omega
    rcases he with h | h <;> simpa [h] using held_probability_shape_top a (x i)
  have hstrict (β : ℝ) (hβ : 0 < β) : L β < U := by
    obtain ⟨i,hi⟩ := hp
    unfold L likelihood U
    exact Finset.prod_lt_prod₀ (fun i _ ↦ (binary_mass hβ a (hx i)).1)
      (fun i _ ↦ (binary_mass hβ a (hx i)).2.1)
      ⟨i,Finset.mem_univ _,by simpa [hi] using (binary_mass hβ a (hx i)).2.2 hi⟩
  refine ⟨hlim,⟨?_,?_⟩,hstrict⟩
  · rintro _ ⟨β,hβ,rfl⟩
    exact (hstrict β hβ).le
  · intro C hC
    exact le_of_tendsto hlim ((eventually_gt_atTop (0 : ℝ)).mono fun β hβ ↦
      hC (mem_image_of_mem L hβ))

/-- Holding shape fixed on an all-zero sample gives the strict excluded
zero atom as the survival probability tends to zero. -/
theorem held_shape_all_zero_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i = 0) (β : ℝ) (_hβ : 0 < β) :
    let L := fun a ↦ likelihood x (a,β)
    Tendsto L atTop (𝓝 1) ∧ IsLUB (range L) 1 ∧ ∀ a, L a < 1 := by
  intro L
  have hlim : Tendsto L atTop (𝓝 1) := by
    have H := tendsto_finsetProd Finset.univ (fun i _ ↦ held_shape_probability_zero β (x i))
    simpa only [L,likelihood,hx,ite_true,Finset.prod_const_one] using H
  have hstrict (a : ℝ) : L a < 1 := by
    have H : L a = (1-exp (-exp a))^n := by
      simp [L,likelihood,hx,mass,survival,endpoint]
    rw [H]
    exact pow_lt_one₀ (sub_nonneg.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (exp_pos _))).le)
      (sub_lt_self _ (exp_pos _)) hn.ne'
  refine ⟨hlim,⟨?_,?_⟩,hstrict⟩
  · rintro _ ⟨a,rfl⟩
    exact (hstrict a).le
  · exact fun C hC ↦ le_of_tendsto hlim (Filter.Eventually.of_forall fun a ↦ hC ⟨a,rfl⟩)

private theorem survival_split (c : ℝ) (hc : 0 < c) (j : ℕ) (hne : (j : ℝ) ≠ c) :
    Tendsto (fun β : ℝ ↦ survival j (-β*log c,β)) atTop
      (𝓝 (if (j : ℝ) < c then 1 else 0)) := by
  by_cases hj : j = 0
  · subst j
    simp [survival,hc]
  have hj' : 0 < (j : ℝ) := Nat.cast_pos.mpr (Nat.pos_of_ne_zero hj)
  have he (β : ℝ) : survival j (-β*log c,β) =
      exp (-exp (β*(log (j : ℝ)-log c))) := by
    unfold survival endpoint
    rw [ite_eq_right hj]
    congr 2
    ring
  simp_rw [he]
  rcases lt_or_gt_of_ne hne with h | h
  · rw [ite_eq_left h]
    have ht : Tendsto (fun β : ℝ ↦ β*(log (j : ℝ)-log c)) atTop atBot :=
      tendsto_id.atTop_mul_const_of_neg (sub_neg.mpr (log_lt_log hj' h))
    have hh := continuous_exp.continuousAt.tendsto.comp (tendsto_exp_atBot.comp ht).neg
    simpa [Function.comp_def] using hh
  · rw [ite_eq_right h.not_gt]
    have ht : Tendsto (fun β : ℝ ↦ β*(log (j : ℝ)-log c)) atTop atTop :=
      tendsto_id.atTop_mul_const (sub_pos.mpr (log_lt_log hc h))
    simpa [Function.comp_def] using
      tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_exp_atTop.comp ht))

/-- With free probability and shape, holding the collapse location
inside a positive cell gives its point mass. At a zero common count
the fixed-shape zero-probability path supplies the atom. -/
theorem free_point_paths (j k : ℕ) {c : ℝ} (h₁ : (j : ℝ) < c) (h₂ : c < (j : ℝ)+1) :
    Tendsto (fun β : ℝ ↦ mass k (-β*log c,β)) atTop (𝓝 (if k = j then 1 else 0)) ∧
    (j = 0 → ∀ β > 0, Tendsto (fun a : ℝ ↦ mass k (a,β)) atTop
      (𝓝 (if k = 0 then 1 else 0))) := by
  refine ⟨?_,fun _ β _ ↦ held_shape_probability_zero β k⟩
  have hc := (Nat.cast_nonneg j).trans_lt h₁
  have hne (i : ℕ) : (i : ℝ) ≠ c := by
    by_cases h : i ≤ j
    · exact ne_of_lt ((Nat.cast_le.mpr h).trans_lt h₁)
    · have hi : (j : ℝ)+1 ≤ i := by exact_mod_cast (show j+1 ≤ i by omega)
      exact ne_of_gt (h₂.trans_le hi)
  unfold mass
  have H := (survival_split c hc k (hne k)).sub (survival_split c hc (k+1) (hne _))
  simp only [Nat.cast_add,Nat.cast_one] at H
  rcases lt_trichotomy k j with h | rfl | h
  · have hk : (k : ℝ)+1 ≤ j := by exact_mod_cast (show k+1 ≤ j by omega)
    have hk' : (k : ℝ) < c := by linarith
    have hk'' : (k : ℝ)+1 < c := by linarith
    simpa [h.ne,hk',hk''] using H
  · have h₂' : ¬ (k : ℝ)+1 < c := by linarith
    simpa [h₁,h₂'] using H
  · have hk : (j : ℝ)+1 ≤ k := by exact_mod_cast (show j+1 ≤ k by omega)
    have hk' : ¬ (k : ℝ) < c := by linarith
    have hk'' : ¬ (k : ℝ)+1 < c := by linarith
    simpa [h.ne',hk',hk''] using H

end UnivariateML.Discrete4DWeibull

end
