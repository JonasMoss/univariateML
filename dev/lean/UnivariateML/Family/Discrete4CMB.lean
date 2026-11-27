module

public import UnivariateML.Family.Discrete3CMB

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Conway–Maxwell–binomial saturated constants and capped comparison
-/

@[expose] public section

open Set Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4CMB

open Discrete3CMB

private theorem normalizer_pos (N : ℕ) (η ν : ℝ) :
    0 < ∑ j : Fin (N+1), weight N η ν j :=
  Finset.sum_pos (fun _ _ ↦ exp_pos _) Finset.univ_nonempty

private theorem mass_lt_one {N : ℕ} (hN : 0 < N) (η ν : ℝ) (k : Fin (N+1)) :
    weight N η ν k / (∑ j : Fin (N+1), weight N η ν j) < 1 := by
  let j : Fin (N+1) := if k = 0 then Fin.last N else 0
  have hj : j ≠ k := by
    by_cases h : k = 0
    · have ht : (Fin.last N : Fin (N+1)) ≠ 0 := fun e ↦ (Nat.ne_of_gt hN) (congrArg Fin.val e)
      simpa [j,h] using ht
    · simp [j,h,eq_comm]
  have hpos : 0 < ∑ t ∈ Finset.univ.erase k, weight N η ν t :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨j,Finset.mem_erase.mpr ⟨hj,Finset.mem_univ _⟩⟩
  rw [div_lt_one (normalizer_pos N η ν), ← Finset.sum_erase_add _ _ (Finset.mem_univ k)]
  linarith

private theorem interior_le_one {N n : ℕ} (x : Fin n → Fin (N+1)) (η ν : ℝ) :
    likelihood x η ν ≤ 1 := by
  apply Finset.prod_le_one₀
  · intro i _
    exact div_nonneg (exp_pos _).le (normalizer_pos N η ν).le
  · intro i _
    exact (div_le_one (normalizer_pos N η ν)).mpr
      (Finset.single_le_sum (f := weight N η ν) (fun _ _ ↦ (exp_pos _).le) (Finset.mem_univ (x i)))

private theorem interior_lt_one {N n : ℕ} (hN : 0 < N) (hn : 0 < n)
    (x : Fin n → Fin (N+1)) (η ν : ℝ) : likelihood x η ν < 1 := by
  have H := Finset.prod_lt_prod_of_nonempty₀
    (s := (Finset.univ : Finset (Fin n)))
    (f := fun i ↦ weight N η ν (x i) / ∑ j, weight N η ν j)
    (g := fun _ ↦ (1 : ℝ))
    (fun i _ ↦ div_pos (exp_pos _) (normalizer_pos N η ν))
    (fun i _ ↦ mass_lt_one hN η ν (x i)) ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  simpa only [likelihood, Finset.prod_const_one] using H

/-- At any feasible trial size, the likelihood is bounded by one.
On a nonempty constant sample all saturated parameters are exactly
the size-zero fiber or the matching endpoint atom at positive size. -/
theorem constant_saturated_iff {N n : ℕ} (hn : 0 < n) (c : Fin (N+1))
    {p : ℝ} (_hp : p ∈ Icc 0 1) (ν : ℝ) :
    closedLikelihood (fun _ : Fin n ↦ c) p ν ≤ 1 ∧
      (closedLikelihood (fun _ : Fin n ↦ c) p ν = 1 ↔
        N = 0 ∨ (p = 0 ∧ c.val = 0) ∨ (p = 1 ∧ c.val = N)) := by
  by_cases hN : N = 0
  · subst N
    simp [size_zero_likelihood]
  have hN' := Nat.pos_of_ne_zero hN
  have hi : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  let := hi
  by_cases h₀ : p = 0
  · have he : c = 0 ↔ c.val = 0 := Fin.ext_iff
    by_cases hc : c = 0 <;> simp [closedLikelihood, h₀, hN, hc, ← he]
  by_cases h₁ : p = 1
  · have he : c = Fin.last N ↔ c.val = N := Fin.ext_iff
    by_cases hc : c = Fin.last N <;> simp [closedLikelihood,h₁,hN,hc, ← he]
  · have H := interior_lt_one hN' hn (fun _ : Fin n ↦ c) (log (p/(1-p))) ν
    simp only [closedLikelihood, ite_eq_right h₀, ite_eq_right h₁]
    exact ⟨H.le, iff_of_false H.ne (by simp [hN,h₀,h₁])⟩

/-- Integer support and the two probability endpoints of a fixed CMB size. -/
def Compatible {n : ℕ} (x : Fin n → ℕ) (N : ℕ) (p : ℝ) : Prop :=
  (∀ i, x i ≤ N) ∧ (p = 0 → ∀ i, x i = 0) ∧
    (p = 1 → ∀ i, x i = N)

/-- Likelihood on common integer data, extended by zero outside the trial-size support. -/
noncomputable def sizeLikelihood {n : ℕ} (x : Fin n → ℕ) (N : ℕ) (p ν : ℝ) : ℝ := by
  classical
  exact if h : ∀ i, x i ≤ N then
    closedLikelihood (fun i ↦ ⟨x i,Nat.lt_succ_of_le (h i)⟩) p ν
  else 0

private theorem zero_dispersion_order {n N M : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ M) (hNM : M < N) {p : ℝ} (hp : p ∈ Ioo 0 1) :
    sizeLikelihood x N p 0 < sizeLikelihood x M p 0 := by
  let η := log (p/(1-p))
  have hf (J : ℕ) (hxJ : ∀ i, x i ≤ J) :
      sizeLikelihood x J p 0 = ∏ i, exp (η * x i) /
        (∑ j ∈ Finset.range (J+1), exp (η * j)) := by
    unfold sizeLikelihood
    rw [dite_eq_left hxJ]
    simp only [closedLikelihood, ite_eq_right hp.1.ne', ite_eq_right hp.2.ne, likelihood,
      weight, zero_mul, add_zero, η]
    rw [Fin.sum_univ_eq_sum_range (fun j ↦ exp (log (p/(1-p)) * j))]
  have hNJ : 0 < ∑ j ∈ Finset.range (M+1), exp (η * j) :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨0,Finset.mem_range.mpr (Nat.zero_lt_succ _)⟩
  have hnorm : (∑ j ∈ Finset.range (M+1), exp (η * j)) <
      ∑ j ∈ Finset.range (N+1), exp (η * j) := by
    apply Finset.sum_lt_sum_of_subset (i := M+1)
    · exact Finset.range_subset_range.mpr (by omega)
    · exact Finset.mem_range.mpr (by omega)
    · simp
    · exact exp_pos _
    · exact fun _ _ _ ↦ (exp_pos _).le
  rw [hf N (fun i ↦ (hx i).trans hNM.le), hf M hx]
  exact Finset.prod_lt_prod_of_nonempty₀
    (fun i _ ↦ div_pos (exp_pos _) (hNJ.trans hnorm))
    (fun i _ ↦ div_lt_div_of_pos_left (exp_pos _) hNJ hnorm)
    ⟨⟨0,hn⟩,Finset.mem_univ _⟩

/-- Holding probability and dispersion reduces the fit to exact comparison
of a finite compatible grid. Every size tied for the best height maximizes. -/
theorem held_probability_dispersion_capped {n B : ℕ} (x : Fin n → ℕ) (p ν : ℝ) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    D.Finite ∧ (D.Nonempty ↔ ∃ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N) := by
  dsimp
  let grid := (Finset.range (B+1)).filter fun N ↦ Compatible x N p
  have he (N : ℕ) : N ∈ grid ↔ N ≤ B ∧ Compatible x N p := by simp [grid]
  refine ⟨grid.finite_toSet.subset (fun N hN ↦ (he N).mpr hN), ?_⟩
  constructor
  · rintro ⟨N,hN⟩
    obtain ⟨J,hJ,hmax⟩ := grid.exists_max_image (fun J ↦ sizeLikelihood x J p ν)
      ⟨N,(he N).mpr hN⟩
    exact ⟨J,(he J).mp hJ,fun K hK ↦ hmax K ((he K).mpr hK)⟩
  · rintro ⟨N,hN,_⟩
    exact ⟨N,hN⟩

/-- At zero dispersion and held interior probability, the smallest
compatible size is the sole maximizer over the capped trial-size domain. -/
theorem held_probability_zero_dispersion {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p : ℝ} (hp : p ∈ Ioo 0 1) (hB : ∀ i, x i ≤ B) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    ∀ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p 0) D N ↔ N = Finset.univ.sup x := by
  dsimp
  let M := Finset.univ.sup x
  have hx (i : Fin n) : x i ≤ M := Finset.le_sup (Finset.mem_univ i)
  have hM : M ≤ B ∧ Compatible x M p := ⟨Finset.sup_le fun i _ ↦ hB i,
    hx,fun h ↦ (hp.1.ne' h).elim,fun h ↦ (hp.2.ne h).elim⟩
  intro N hN
  have hMle : M ≤ N := Finset.sup_le fun i _ ↦ hN.2.1 i
  constructor
  · intro h
    by_contra he
    have ht := zero_dispersion_order hn x hx (lt_of_le_of_ne hMle (Ne.symm he)) hp
    exact ht.not_ge (h hM)
  · rintro rfl J hJ
    have hMJ : M ≤ J := Finset.sup_le fun i _ ↦ hJ.2.1 i
    rcases eq_or_lt_of_le hMJ with he | he
    · exact he ▸ (le_rfl : sizeLikelihood x M p 0 ≤ sizeLikelihood x M p 0)
    · exact (zero_dispersion_order hn x hx he hp).le

private noncomputable def meanCount {N n : ℕ} (x : Fin n → Fin (N+1)) : ℝ :=
  (∑ i, ((x i).val : ℝ)) / n

private noncomputable def logistic (η : ℝ) : ℝ := exp η / (1+exp η)

private theorem logistic_mem (η : ℝ) : logistic η ∈ Ioo 0 1 := by
  unfold logistic
  exact ⟨by positivity, (div_lt_one (by positivity)).mpr (by linarith)⟩

private theorem log_odds_logistic (η : ℝ) : log (logistic η / (1-logistic η)) = η := by
  have h : 0 < 1+exp η := by positivity
  unfold logistic
  rw [show exp η/(1+exp η)/(1-exp η/(1+exp η)) = exp η by field_simp [h.ne']; ring]
  exact log_exp _

private theorem logistic_log_odds {p : ℝ} (hp : p ∈ Ioo 0 1) :
    logistic (log (p/(1-p))) = p := by
  have h : 0 < 1-p := sub_pos.mpr hp.2
  unfold logistic
  rw [exp_log (div_pos hp.1 h)]
  field_simp [h.ne']
  ring

private theorem likelihood_pos' {N n : ℕ} (x : Fin n → Fin (N+1)) (η ν : ℝ) :
    0 < likelihood x η ν :=
  Finset.prod_pos fun _ _ ↦ div_pos (exp_pos _) (normalizer_pos N η ν)

private theorem likelihood_compare {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1))
    (ν a b : ℝ) :
    likelihood x a ν ≤ likelihood x b ν ↔
      heldDispersionLogLik N ν (meanCount x) a ≤ heldDispersionLogLik N ν (meanCount x) b := by
  have hn' : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  have hlog (η : ℝ) : log (likelihood x η ν) =
      (n : ℝ) * heldDispersionLogLik N ν (meanCount x) η + ν * ∑ i, height N (x i) := by
    let Z := ∑ j : Fin (N+1), weight N η ν j
    have hZ : 0 < Z := normalizer_pos N η ν
    unfold likelihood
    rw [log_prod (fun i _ ↦ div_ne_zero (show weight N η ν (x i) ≠ 0 from (exp_pos _).ne') hZ.ne')]
    have he (i : Fin n) : log (weight N η ν (x i) / Z) =
        η * (x i).val + ν * height N (x i) - log Z := by
      rw [log_div (show weight N η ν (x i) ≠ 0 from (exp_pos _).ne') hZ.ne', weight, log_exp]
    change (∑ i, log (weight N η ν (x i) / Z)) = _
    simp only [he, Finset.sum_sub_distrib, Finset.sum_add_distrib,
      ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    have hE : (∑ j : Fin (N+1), exp (ν * height N j) * exp ((j.val : ℝ) * η)) = Z := by
      apply Finset.sum_congr rfl
      intro j _
      rw [← exp_add, weight]
      congr 1
      ring
    simp only [heldDispersionLogLik, ExpFamily.logLik, ExpFamily.logPartition, RCLike.inner_apply,
      conj_trivial, hE, meanCount]
    field_simp [hn'.ne']
    ring
  rw [← log_le_log_iff (likelihood_pos' x a ν) (likelihood_pos' x b ν), hlog, hlog,
    add_le_add_iff_right, mul_le_mul_iff_right₀ hn']

/-- A positive fixed size has exactly one fitted probability at any held
finite dispersion. The endpoint fits are included. Size zero is flat. -/
theorem held_dispersion_size {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1)) (ν : ℝ) :
    (N = 0 → ∀ p ∈ Icc (0 : ℝ) 1, closedLikelihood x p ν = 1) ∧
    (0 < N → ∃! p, p ∈ Icc (0 : ℝ) 1 ∧
      IsMaxOn (fun q ↦ closedLikelihood x q ν) (Icc 0 1) p) := by
  refine ⟨fun h p _ ↦ ?_, fun hN ↦ ?_⟩
  · subst N
    exact size_zero_likelihood x p ν
  have hbound (p : ℝ) : closedLikelihood x p ν ≤ 1 := by
    unfold closedLikelihood
    split
    · split <;> norm_num
    split
    · split <;> norm_num
    exact interior_le_one x _ _
  have hzero : (0 : Fin (N+1)) ≠ Fin.last N := fun e ↦ hN.ne' (congrArg Fin.val e).symm
  let i : Fin n := ⟨0,hn⟩
  by_cases h₀ : ∀ i, x i = 0
  · have h₁ : ¬ ∀ i, x i = Fin.last N := fun h ↦ hzero ((h₀ i).symm.trans (h i))
    have hv : closedLikelihood x 0 ν = 1 := by simp [closedLikelihood,h₀]
    refine ⟨0,⟨by norm_num,fun p _ ↦ ?_⟩,fun p ⟨hp,hpmax⟩ ↦ ?_⟩
    · change closedLikelihood x p ν ≤ closedLikelihood x 0 ν
      rw [hv]
      exact hbound p
    · by_contra hp₀
      have he := hpmax (show (0 : ℝ) ∈ Icc 0 1 by norm_num)
      change closedLikelihood x 0 ν ≤ closedLikelihood x p ν at he
      rw [hv] at he
      by_cases hp₁ : p = 1
      · exact (by norm_num : ¬ (1 : ℝ) ≤ 0) (by simpa [closedLikelihood,hp₁,h₁] using he)
      · rw [closedLikelihood, ite_eq_right hp₀, ite_eq_right hp₁] at he
        exact (interior_lt_one hN hn x _ ν).not_ge he
  by_cases h₁ : ∀ i, x i = Fin.last N
  · have hv : closedLikelihood x 1 ν = 1 := by simp [closedLikelihood,h₁]
    refine ⟨1,⟨by norm_num,fun p _ ↦ ?_⟩,fun p ⟨hp,hpmax⟩ ↦ ?_⟩
    · change closedLikelihood x p ν ≤ closedLikelihood x 1 ν
      rw [hv]
      exact hbound p
    · by_contra hp₁
      have he := hpmax (show (1 : ℝ) ∈ Icc 0 1 by norm_num)
      change closedLikelihood x 1 ν ≤ closedLikelihood x p ν at he
      rw [hv] at he
      by_cases hp₀ : p = 0
      · exact (by norm_num : ¬ (1 : ℝ) ≤ 0) (by simpa [closedLikelihood,hp₀,h₀] using he)
      · rw [closedLikelihood, ite_eq_right hp₀, ite_eq_right hp₁] at he
        exact (interior_lt_one hN hn x _ ν).not_ge he
  have hn' : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  have hm : meanCount x ∈ Ioo (0 : ℝ) N := by
    obtain ⟨j,hj⟩ := not_forall.mp h₀
    obtain ⟨k,hk⟩ := not_forall.mp h₁
    have hj' : 0 < ((x j).val : ℝ) := Nat.cast_pos.mpr (Nat.pos_of_ne_zero (by
      intro e
      exact hj (Fin.ext e)))
    have hk' : ((x k).val : ℝ) < N := by
      exact_mod_cast (show (x k).val < N by
        have hh := (x k).isLt
        have hne : (x k).val ≠ N := fun e ↦ hk (Fin.ext e)
        omega)
    refine ⟨div_pos (Finset.sum_pos' (fun i _ ↦ Nat.cast_nonneg _) ⟨j,Finset.mem_univ _,hj'⟩) hn', ?_⟩
    apply (div_lt_iff₀ hn').mpr
    have h := Finset.sum_lt_sum (s := Finset.univ)
      (f := fun i ↦ ((x i).val : ℝ)) (g := fun _ ↦ (N : ℝ))
      (fun i _ ↦ by exact_mod_cast Nat.le_of_lt_succ (x i).isLt)
      ⟨k,Finset.mem_univ _,hk'⟩
    simpa [mul_comm] using h
  obtain ⟨η,hη,hunique⟩ := held_dispersion_interior hN ν (meanCount x) hm
  have hpη := logistic_mem η
  have hclosed (p : ℝ) (hp : p ∈ Ioo 0 1) :
      closedLikelihood x p ν = likelihood x (log (p/(1-p))) ν := by
    simp [closedLikelihood,hp.1.ne',hp.2.ne]
  have hval : closedLikelihood x (logistic η) ν = likelihood x η ν := by
    rw [hclosed _ hpη, log_odds_logistic]
  have hends : closedLikelihood x 0 ν = 0 ∧ closedLikelihood x 1 ν = 0 := by
    simp [closedLikelihood,h₀,h₁]
  refine ⟨logistic η,⟨⟨hpη.1.le,hpη.2.le⟩,fun p hp ↦ ?_⟩,fun p ⟨hp,hpmax⟩ ↦ ?_⟩
  · change closedLikelihood x p ν ≤ closedLikelihood x (logistic η) ν
    rw [hval]
    by_cases hp₀ : p = 0
    · rw [hp₀,hends.1]
      exact (likelihood_pos' x η ν).le
    by_cases hp₁ : p = 1
    · rw [hp₁,hends.2]
      exact (likelihood_pos' x η ν).le
    have hp' : p ∈ Ioo 0 1 := ⟨lt_of_le_of_ne hp.1 (Ne.symm hp₀),lt_of_le_of_ne hp.2 hp₁⟩
    rw [hclosed _ hp']
    exact (likelihood_compare hn x ν _ _).mpr (hη (mem_univ _))
  · have hpos : 0 < closedLikelihood x p ν := by
      have he := hpmax (show logistic η ∈ Icc 0 1 from ⟨hpη.1.le,hpη.2.le⟩)
      change closedLikelihood x (logistic η) ν ≤ closedLikelihood x p ν at he
      rw [hval] at he
      exact (likelihood_pos' x η ν).trans_le he
    have hp' : p ∈ Ioo 0 1 := by
      refine ⟨lt_of_le_of_ne hp.1 ?_,lt_of_le_of_ne hp.2 ?_⟩
      · rintro rfl
        rw [hends.1] at hpos
        exact (lt_irrefl _ hpos)
      · rintro rfl
        rw [hends.2] at hpos
        exact (lt_irrefl _ hpos)
    have he := hunique (log (p/(1-p))) (show IsMaxOn _ univ _ from by
      intro t _
      have ht := logistic_mem t
      have H := hpmax (show logistic t ∈ Icc 0 1 from ⟨ht.1.le,ht.2.le⟩)
      change closedLikelihood x (logistic t) ν ≤ closedLikelihood x p ν at H
      rw [hclosed _ ht, log_odds_logistic, hclosed _ hp'] at H
      exact (likelihood_compare hn x ν _ _).mp H)
    rw [← logistic_log_odds hp', he]

/-- At held dispersion, the closed probability profiles each attain and
a finite comparison over the capped trial designs gives a global fit. -/
theorem held_dispersion_capped {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (ν : ℝ)
    (hx : ∀ i, x i ≤ B) :
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    ∃ q ∈ D, IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D q := by
  dsimp
  let grid := (Finset.range (B+1)).filter fun J ↦ ∀ i, x i ≤ J
  have he (J : ℕ) : J ∈ grid ↔ J ≤ B ∧ ∀ i, x i ≤ J := by simp [grid]
  have hprofile : ∀ J, ∃ p ∈ Icc (0 : ℝ) 1, ∀ q ∈ Icc (0 : ℝ) 1,
      sizeLikelihood x J q ν ≤ sizeLikelihood x J p ν := by
    intro J
    by_cases hJ : ∀ i, x i ≤ J
    · let y (i : Fin n) : Fin (J+1) := ⟨x i,Nat.lt_succ_of_le (hJ i)⟩
      have H := held_dispersion_size hn y ν
      by_cases hz : J = 0
      · refine ⟨0,by norm_num,fun q hq ↦ ?_⟩
        unfold sizeLikelihood
        rw [dite_eq_left hJ, dite_eq_left hJ, H.1 hz q hq, H.1 hz 0 (by norm_num)]
      · obtain ⟨p,⟨hp,hmax⟩,_⟩ := H.2 (Nat.pos_of_ne_zero hz)
        refine ⟨p,hp,fun q hq ↦ ?_⟩
        have Hq := hmax hq
        change closedLikelihood y q ν ≤ closedLikelihood y p ν at Hq
        simpa only [sizeLikelihood, dite_eq_left hJ] using Hq
    · refine ⟨0,by norm_num,fun q _ ↦ ?_⟩
      simp [sizeLikelihood,hJ]
  choose p hp hbest using hprofile
  obtain ⟨J,hJ,hmax⟩ := grid.exists_max_image (fun K ↦ sizeLikelihood x K (p K) ν)
    ⟨B,(he B).mpr ⟨le_rfl,hx⟩⟩
  obtain ⟨hJB,hJx⟩ := (he J).mp hJ
  refine ⟨(J,p J),⟨hJB,hJx,hp J⟩,?_⟩
  rintro ⟨K,q⟩ ⟨hKB,hKx,hq⟩
  exact (hbest K q hq).trans (hmax K ((he K).mpr ⟨hKB,hKx⟩))

/-- Zero dispersion and at least one positive count force the smallest
compatible size, even when probability is optimized. -/
theorem held_zero_dispersion_size_unique {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) :
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    ∀ q ∈ D, IsMaxOn (fun t ↦ sizeLikelihood x t.1 t.2 0) D q →
      q.1 = Finset.univ.sup x := by
  dsimp
  let M := Finset.univ.sup x
  rintro ⟨N,p⟩ ⟨hNB,hNx,hp⟩ hmax
  have hMx (i : Fin n) : x i ≤ M := Finset.le_sup (Finset.mem_univ i)
  have hMN : M ≤ N := Finset.sup_le fun i _ ↦ hNx i
  have hMB := hMN.trans hNB
  by_contra! he
  have hlt : M < N := lt_of_le_of_ne hMN (Ne.symm he)
  have hstrict (p : ℝ) (hp : p ∈ Ioo 0 1) :
      sizeLikelihood x N p 0 < sizeLikelihood x M p 0 :=
    zero_dispersion_order hn x hMx hlt hp
  have hpos : 0 < sizeLikelihood x M (1/2) 0 := by
    unfold sizeLikelihood
    rw [dite_eq_left hMx]
    simp only [closedLikelihood, show (1/2 : ℝ) ≠ 0 by norm_num,
      show (1/2 : ℝ) ≠ 1 by norm_num, ite_false]
    exact likelihood_pos' _ _ _
  have hmem (p : ℝ) (hp : p ∈ Icc 0 1) :
      (M,p) ∈ {q : ℕ × ℝ | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1} :=
    ⟨hMB,hMx,hp⟩
  by_cases h₀ : p = 0
  · obtain ⟨i,hi⟩ := hx
    have hne : ¬ ∀ j, (⟨x j,Nat.lt_succ_of_le (hNx j)⟩ : Fin (N+1)) = 0 :=
      fun H ↦ hi.ne' (congrArg Fin.val (H i))
    have hval : sizeLikelihood x N p 0 = 0 := by
      simp only [sizeLikelihood, dite_eq_left hNx, closedLikelihood, h₀, ite_true, ite_eq_right hne]
    have H := hmax (hmem (1/2) (by norm_num))
    change sizeLikelihood x M (1/2) 0 ≤ sizeLikelihood x N p 0 at H
    rw [hval] at H
    exact hpos.not_ge H
  by_cases h₁ : p = 1
  · let i : Fin n := ⟨0,hn⟩
    have hne : ¬ ∀ j, (⟨x j,Nat.lt_succ_of_le (hNx j)⟩ : Fin (N+1)) = Fin.last N := by
      intro H
      have hi : x i = N := congrArg Fin.val (H i)
      exact (hMx i).not_gt (hi.symm ▸ hlt)
    have hval : sizeLikelihood x N p 0 = 0 := by
      simp [sizeLikelihood,hNx,closedLikelihood,h₁,hne]
    have H := hmax (hmem (1/2) (by norm_num))
    change sizeLikelihood x M (1/2) 0 ≤ sizeLikelihood x N p 0 at H
    rw [hval] at H
    exact hpos.not_ge H
  have hp' : p ∈ Ioo 0 1 := ⟨lt_of_le_of_ne hp.1 (Ne.symm h₀),lt_of_le_of_ne hp.2 h₁⟩
  exact (hstrict p hp').not_ge (hmax (hmem p hp))

/-- Mixed binary observations have exactly the capped size-one
Bernoulli maximizers, with dispersion arbitrary. -/
theorem mixed_binary_maximizers {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ 1) (hz : ∃ i, x i = 0) (ho : ∃ i, x i = 1) (hB : 1 ≤ B) :
    let z := (Finset.univ.filter fun i ↦ x i = 0).card
    let o := (Finset.univ.filter fun i ↦ x i = 1).card
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    ∀ q ∈ D, IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2.1 q.2.2) D q ↔
      q.1 = 1 ∧ q.2.1 = (o : ℝ)/(o+z) := by
  intro z o D
  have hz' : 0 < z := by
    obtain ⟨i,hi⟩ := hz
    exact Finset.card_pos.mpr ⟨i,Finset.mem_filter.mpr ⟨Finset.mem_univ _,hi⟩⟩
  have ho' : 0 < o := by
    obtain ⟨i,hi⟩ := ho
    exact Finset.card_pos.mpr ⟨i,Finset.mem_filter.mpr ⟨Finset.mem_univ _,hi⟩⟩
  let p₀ : ℝ := (o : ℝ)/(o+z)
  have hzR : 0 < (z : ℝ) := Nat.cast_pos.mpr hz'
  have hoR : 0 < (o : ℝ) := Nat.cast_pos.mpr ho'
  have hden : 0 < (o : ℝ)+z := add_pos hoR hzR
  have hp₀ : p₀ ∈ Icc (0 : ℝ) 1 := by
    unfold p₀
    exact ⟨(div_pos hoR hden).le, (div_le_one hden).mpr (by linarith)⟩
  have hpSub : 1-p₀ = (z : ℝ)/(z+o) := by
    unfold p₀
    rw [add_comm (z : ℝ) o]
    field_simp
    ring
  have hH : 0 < pairHeight z o := by unfold pairHeight; positivity
  have hw {N : ℕ} (hN : 1 ≤ N) :
      let y : Fin n → Fin (N+1) := fun i ↦ ⟨x i,Nat.lt_succ_of_le ((hx i).trans hN)⟩
      let u : Fin (N+1) := ⟨1,Nat.lt_succ_of_le hN⟩
      (∀ i, y i = 0 ∨ y i = u) ∧ Discrete3CMB.multiplicity y 0 = z ∧ Discrete3CMB.multiplicity y u = o := by
    intro y u
    refine ⟨fun i ↦ ?_,?_,?_⟩
    · have h := hx i
      rcases (show x i = 0 ∨ x i = 1 by omega) with h | h
      · exact Or.inl (Fin.ext h)
      · exact Or.inr (Fin.ext h)
    · simp only [Discrete3CMB.multiplicity,Fin.ext_iff]
      rfl
    · simp only [Discrete3CMB.multiplicity,Fin.ext_iff]
      rfl
  let y₁ (i : Fin n) : Fin 2 := ⟨x i,Nat.lt_succ_of_le (hx i)⟩
  have hcz : Discrete3CMB.multiplicity y₁ 0 = z := (hw (N := 1) le_rfl).2.1
  have hco : Discrete3CMB.multiplicity y₁ 1 = o := by
    simp only [Discrete3CMB.multiplicity,Fin.ext_iff]
    rfl
  have hbase (ν : ℝ) : sizeLikelihood x 1 p₀ ν = pairHeight z o := by
    unfold sizeLikelihood
    rw [dite_eq_left hx]
    change closedLikelihood y₁ p₀ ν = _
    rw [bernoulli_likelihood _ hp₀,hcz,hco,hpSub]
    unfold pairHeight p₀
    rw [add_comm (o : ℝ) z,mul_comm]
  have hbern (p ν : ℝ) (hp : p ∈ Icc (0 : ℝ) 1) :
      sizeLikelihood x 1 p ν ≤ pairHeight z o ∧
        (sizeLikelihood x 1 p ν = pairHeight z o ↔ p = p₀) := by
    let y := y₁
    have hm {q : ℝ} (hq : q ∈ Icc (0 : ℝ) 1) :
        IsMaxOn (fun t ↦ closedLikelihood y t ν) (Icc 0 1) q ↔ q = p₀ := by
      rw [bernoulli_fit hn y ν hq,show Discrete3CMB.multiplicity y 0 = z from hcz,
        show Discrete3CMB.multiplicity y 1 = o from hco]
    have he (q : ℝ) : sizeLikelihood x 1 q ν = closedLikelihood y q ν := by
      simp [sizeLikelihood,hx,y,y₁]
    have hval := (he p₀).symm.trans (hbase ν)
    refine ⟨?_,?_⟩
    · rw [he,← hval]
      exact ((hm hp₀).mpr rfl) hp
    · rw [he,← hval]
      constructor
      · intro eq
        apply (hm hp).mp
        intro t ht
        have ht' := ((hm hp₀).mpr rfl) ht
        change closedLikelihood y t ν ≤ closedLikelihood y p₀ ν at ht'
        change closedLikelihood y t ν ≤ closedLikelihood y p ν
        rwa [eq]
      · rintro rfl
        rfl
  have hstrict (N : ℕ) (hN : N ≠ 1) (p ν : ℝ) (hp : p ∈ Icc (0 : ℝ) 1) :
      sizeLikelihood x N p ν < pairHeight z o := by
    by_cases ha : 1 ≤ N
    · have hN' : 2 ≤ N := by omega
      let y (i : Fin n) : Fin (N+1) := ⟨x i,Nat.lt_succ_of_le ((hx i).trans ha)⟩
      let u : Fin (N+1) := ⟨1,Nat.lt_succ_of_le ha⟩
      have H := hw ha
      have Y (i : Fin n) : y i = 0 ∨ y i = u := H.1 i
      have H₁ : 0 < Discrete3CMB.multiplicity y 0 := H.2.1 ▸ hz'
      have H₂ : 0 < Discrete3CMB.multiplicity y u := H.2.2 ▸ ho'
      have hA (η : ℝ) := (adjacent_supremum hN' y (u := u) (l := 0) rfl Y H₁ H₂).2 (η,ν)
      have he : sizeLikelihood x N p ν = closedLikelihood y p ν := by
        simp [sizeLikelihood,fun i ↦ (hx i).trans ha,y]
      rw [he]
      by_cases h₀ : p = 0
      · obtain ⟨i,hi⟩ := ho
        have hne : ¬ ∀ i, y i = 0 := fun H ↦ (show x i ≠ 0 by omega) (congrArg Fin.val (H i))
        simpa [closedLikelihood,h₀,hne] using hH
      by_cases h₁ : p = 1
      · obtain ⟨i,hi⟩ := hz
        have hne : ¬ ∀ i, y i = Fin.last N := fun H ↦
          (show x i ≠ N by omega) (congrArg Fin.val (H i))
        simpa [closedLikelihood,h₁,hne] using hH
      rw [closedLikelihood,ite_eq_right h₀,ite_eq_right h₁]
      have ha' := hA (log (p/(1-p)))
      rw [show Discrete3CMB.multiplicity y 0 = z from H.2.1,
        show Discrete3CMB.multiplicity y u = o from H.2.2] at ha'
      exact ha'
    · obtain ⟨i,hi⟩ := ho
      have hb : ¬ ∀ i, x i ≤ N := fun H ↦ ha (hi ▸ H i)
      simpa [sizeLikelihood,hb] using hH
  rintro ⟨N,p,ν⟩ hq
  have hp := hq.2
  have hmem : (1,p₀,ν) ∈ D := ⟨hB,hp₀⟩
  constructor
  · intro hmax
    have hb := hmax hmem
    change sizeLikelihood x 1 p₀ ν ≤ sizeLikelihood x N p ν at hb
    rw [hbase ν] at hb
    have he : N = 1 := by
      by_contra he
      exact (hstrict N he p ν hp).not_ge hb
    subst N
    exact ⟨rfl,(hbern p ν hp).2.mp (le_antisymm (hbern p ν hp).1 hb)⟩
  · rintro ⟨rfl,rfl⟩ ⟨N,q,ρ⟩ hq'
    change sizeLikelihood x N q ρ ≤ sizeLikelihood x 1 p₀ ν
    rw [hbase ν]
    by_cases he : N = 1
    · subst N
      exact (hbern q ρ hq'.2).1
    · exact (hstrict N he q ρ hq'.2).le

end UnivariateML.Discrete4CMB

end
