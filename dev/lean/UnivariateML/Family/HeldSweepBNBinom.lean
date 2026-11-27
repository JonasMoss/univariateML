module

public import UnivariateML.Family.Discrete3BNBinom
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Beta-negative-binomial with alpha held

## Deviations

The theorem takes diagonal exclusion as a hypothesis. The dossier's
finite arithmetic comparison for the golden sample is a separate step.
-/

@[expose] public section

open Set Filter
open scoped Topology

namespace UnivariateML.HeldSweepBNBinom

open Discrete3BNBinom

/-- A feasible point strictly above the whole diagonal certifies at least two
labeled global maxima at held alpha. -/
theorem held_alpha_two_of_diagonal_exclusion {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    {w : ℝ × ℝ} (hw : w ∈ Ioi (0 : ℝ) ×ˢ Ioi 0)
    (hex : ∀ d > 0, logLikelihood x d a d < logLikelihood x w.1 a w.2) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi 0
    let f := fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2
    ∃ p ∈ D, p.1 ≠ p.2 ∧ IsMaxOn f D p ∧ p.swap ∈ D ∧ IsMaxOn f D p.swap := by
  obtain ⟨p, hp, hm⟩ := held_alpha_attained hn hx ha
  have hneq : p.1 ≠ p.2 := by
    intro he
    have h := hex p.1 hp.1
    rw [he] at h
    have h' : logLikelihood x w.1 a w.2 ≤ logLikelihood x p.1 a p.2 := hm hw
    rw [he] at h'
    exact h.not_ge h'
  refine ⟨p, hp, hneq, hm, ⟨hp.2, hp.1⟩, fun q hq ↦ ?_⟩
  change logLikelihood x q.1 a q.2 ≤ logLikelihood x p.2 a p.1
  rw [← logLikelihood_swap x p.1 a p.2]
  exact hm hq

private theorem no_max_of_zero_limit {f : ℝ → ℝ} {F : Filter ℝ} [F.NeBot]
    (hF : ∀ᶠ t in F, 0 < t) (ht : Tendsto f F (𝓝 0)) (hn : ∀ t > 0, f t < 0) :
    ¬ ∃ t ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) t := by
  rintro ⟨t, ht', hmax⟩
  have h : 0 ≤ f t := le_of_tendsto ht (hF.mono fun _ hp ↦ hmax hp)
  exact (hn t ht').not_ge h

/-- With alpha and beta held positive, a size maximum exists uniquely
exactly when at least one observation is positive. -/
theorem held_alpha_beta_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    (∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x t a b) (Ioi 0) r) ↔
      ∃ i, 0 < x i := by
  refine ⟨?_, fun hx ↦ held_alpha_beta hn hx ha hb⟩
  rintro ⟨r, ⟨hr, hm⟩, _⟩
  by_contra! hx
  have hz : ∀ i, x i = 0 := fun i ↦ Nat.eq_zero_of_le_zero (hx i)
  have h := all_zero_held_faces hn hz (show 0 < r from hr) ha hb
  exact no_max_of_zero_limit eventually_mem_nhdsWithin h.2.1
    (fun t ht ↦ (all_zero_held_faces hn hz ht ha hb).1) ⟨r, hr, hm⟩

/-- With alpha and size held positive, a beta maximum exists uniquely
exactly when at least one observation is positive. -/
theorem held_alpha_size_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    (∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r a t) (Ioi 0) b) ↔
      ∃ i, 0 < x i := by
  simp_rw [logLikelihood_swap x r a]
  exact held_alpha_beta_iff hn x ha hr

/-- With beta and size held positive, an alpha maximum exists uniquely
exactly when at least one observation is positive. -/
theorem held_beta_size_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {b r : ℝ} (hb : 0 < b) (hr : 0 < r) :
    (∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r t b) (Ioi 0) a) ↔
      ∃ i, 0 < x i := by
  refine ⟨?_, fun hx ↦ held_beta_size hn hx hr hb⟩
  rintro ⟨a, ⟨ha, hm⟩, _⟩
  by_contra! hx
  have hz : ∀ i, x i = 0 := fun i ↦ Nat.eq_zero_of_le_zero (hx i)
  have h := all_zero_held_faces hn hz hr (show 0 < a from ha) hb
  exact no_max_of_zero_limit (eventually_gt_atTop 0) h.2.2.2
    (fun t ht ↦ (all_zero_held_faces hn hz hr ht hb).1) ⟨a, ha, hm⟩

end UnivariateML.HeldSweepBNBinom

end
