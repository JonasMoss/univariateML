module

public import UnivariateML.Family.HeldSweepNegBinomial

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Negative binomial held fits on the closed coefficient domains

The mass-scale likelihood agrees with the existing log likelihood at regular
parameters. Size zero and probability one use the zero-atom convention.
-/

@[expose] public section

open Set

namespace UnivariateML.Discrete4NBinom

open NegBinomial

noncomputable def likelihood {n : ℕ} (x : Fin n → ℕ) (r p : ℝ) : ℝ := by
  classical
  exact if (∀ i, x i = 0) then zeroLikelihood n r p
    else if 0 < r ∧ p ∈ Ioo 0 1 then Real.exp (logLik x r p) else 0

private theorem positive_interior {n : ℕ} {x : Fin n → ℕ} (hx : ¬ ∀ i, x i = 0)
    {r p : ℝ} (hr : 0 < r) (hp : p ∈ Ioo 0 1) :
    likelihood x r p = Real.exp (logLik x r p) := by
  simp [likelihood, hx, hr, hp]

private theorem pos_mean {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ¬ ∀ i, x i = 0) :
    0 < mean x := by
  apply (mean_pos_iff hn x).2
  push Not at hx
  obtain ⟨i,hi⟩ := hx
  exact ⟨i,Nat.pos_of_ne_zero hi⟩

/-- A held interior probability has exactly one maximizer over nonnegative
size, with optimum zero on all-zero data. Probability one is compatible
only with zeros, for which the entire size ray maximizes. -/
theorem held_probability {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioc 0 1) :
    ((∀ i, x i = 0) →
      ∀ r ∈ Ici (0 : ℝ), IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r ↔ r = 0 ∨ p = 1) ∧
    ((¬ ∀ i, x i = 0) →
      (p < 1 → ∃! r, r ∈ Ici (0 : ℝ) ∧ IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r) ∧
      (p = 1 → ∀ r, likelihood x r p = 0)) := by
  refine ⟨?_, ?_⟩
  · intro hx r hr
    simpa only [likelihood, ite_eq_left hx] using HeldSweepNegBinomial.held_probability_zero_data hn hp hr
  · intro hx
    refine ⟨fun hp₁ ↦ ?_, fun h r ↦ ?_⟩
    · have hp' : p ∈ Ioo 0 1 := ⟨hp.1,hp₁⟩
      obtain ⟨r,⟨hr,hM⟩,hU⟩ := heldProb hn (pos_mean hn hx) hp'
      have hm : IsMaxOn (fun s ↦ likelihood x s p) (Ici 0) r := by
        intro s hs
        change likelihood x s p ≤ likelihood x r p
        rw [positive_interior hx hr hp']
        by_cases hs₀ : s = 0
        · rw [hs₀]
          simpa [likelihood, hx] using (Real.exp_pos (logLik x r p)).le
        · have hs' : 0 < s := lt_of_le_of_ne hs (Ne.symm hs₀)
          rw [positive_interior hx hs' hp']
          exact Real.exp_le_exp.mpr (hM hs')
      refine ⟨r,⟨hr.le,hm⟩,fun s ⟨hs,hS⟩ ↦ ?_⟩
      have hs' : 0 < s := by
        by_contra! H
        have he : s = 0 := le_antisymm H hs
        have H' := hS (show r ∈ Ici 0 from hr.le)
        change likelihood x r p ≤ likelihood x s p at H'
        rw [positive_interior hx hr hp', he] at H'
        exact (Real.exp_pos (logLik x r p)).not_ge (by simpa [likelihood,hx] using H')
      refine hU s ⟨hs',fun t ht ↦ ?_⟩
      have H := hS (show t ∈ Ici 0 from ht.le)
      change likelihood x t p ≤ likelihood x s p at H
      rw [positive_interior hx ht hp', positive_interior hx hs' hp'] at H
      exact Real.exp_le_exp.mp H
    · simp [likelihood,hx,h]

/-- At held positive size the fitted probability is unique, including
probability one on all-zero data. At size zero only zero data are compatible
and all admissible probabilities maximize. -/
theorem held_size {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : 0 ≤ r) :
    ((∀ i, x i = 0) →
      ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood x r) (Ioc 0 1) p ↔ r = 0 ∨ p = 1) ∧
    ((¬ ∀ i, x i = 0) →
      (0 < r → prob x r ∈ Ioc (0 : ℝ) 1 ∧
        ∀ p ∈ Ioc (0 : ℝ) 1, IsMaxOn (likelihood x r) (Ioc 0 1) p ↔ p = prob x r) ∧
      (r = 0 → ∀ p, likelihood x r p = 0)) := by
  refine ⟨?_, ?_⟩
  · intro hx p hp
    have he : likelihood x r = zeroLikelihood n r := by
      funext q
      simp [likelihood,hx]
    rw [he]
    exact HeldSweepNegBinomial.held_size_zero_data hn hr hp
  · intro hx
    refine ⟨fun hr' ↦ ?_, fun h p ↦ ?_⟩
    · obtain ⟨hq,hM⟩ := heldSize hn (pos_mean hn hx) hr'
      have hstrict (p : ℝ) (hp : p ∈ Ioc (0 : ℝ) 1) (he : p ≠ prob x r) :
          likelihood x r p < likelihood x r (prob x r) := by
        rw [positive_interior hx hr' hq]
        by_cases hp₁ : p = 1
        · simpa [likelihood,hx,hp₁] using Real.exp_pos (logLik x r (prob x r))
        · have hp' : p ∈ Ioo 0 1 := ⟨hp.1,lt_of_le_of_ne hp.2 hp₁⟩
          rw [positive_interior hx hr' hp']
          exact Real.exp_lt_exp.mpr (hM p hp' he)
      refine ⟨⟨hq.1,hq.2.le⟩,fun p hp ↦ ?_⟩
      constructor
      · intro h
        by_contra he
        exact (hstrict p hp he).not_ge (h ⟨hq.1,hq.2.le⟩)
      · rintro rfl s hs
        by_cases he : s = prob x r
        · exact he ▸ (le_rfl : likelihood x r (prob x r) ≤ likelihood x r (prob x r))
        · exact (hstrict s hs he).le
    · simp [likelihood,hx,h]

end UnivariateML.Discrete4NBinom

end
