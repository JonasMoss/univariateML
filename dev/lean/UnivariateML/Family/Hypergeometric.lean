module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Analysis.SpecificLimits.Basic
public import Mathlib.Data.Fin.VecNotation
public import Mathlib.Data.Nat.Choose.Cast

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Hypergeometric scalar likelihoods and discrete uniform endpoints

The hypergeometric sample consists of natural counts. Discrete uniform observations
and endpoints are integers.

## Deviations

The population escape is on the unbounded mathematical parameter ray. The
maximal-count atom instead uses the implementation's finite count bound. The
atom statement applies to a maximal held draw or success count. Holding just the
population at the bound leaves distinct all-zero maximizers.
-/

@[expose] public section

open Set Filter Topology
open scoped BigOperators Classical

namespace UnivariateML.Hypergeometric

noncomputable section

variable {h : ℕ}

/-- The constraints on the margins and the four cells of the sampling table. -/
def Compatible (K N n x : ℕ) : Prop :=
  K ≤ N ∧ n ≤ N ∧ x ≤ K ∧ x ≤ n ∧ K+n ≤ N+x

/-- Hypergeometric mass with zero outside the common support. -/
def mass (K N n x : ℕ) : ℝ := by
  classical
  exact if Compatible K N n x then
    (K.factorial : ℝ) * n.factorial * (N-K).factorial * (N-n).factorial /
      ((N.factorial : ℝ) * x.factorial * (K-x).factorial * (n-x).factorial *
        (N+x-K-n).factorial)
  else 0

def likelihood (K N n : ℕ) (x : Fin h → ℕ) : ℝ :=
  ∏ i, mass K N n (x i)

private theorem fact_pos (j : ℕ) : (0 : ℝ) < j.factorial :=
  Nat.cast_pos.mpr (Nat.factorial_pos _)

theorem mass_choose {K N n x : ℕ} (hc : Compatible K N n x) :
    mass K N n x = (K.choose x : ℝ) * (N-K).choose (n-x) / N.choose n := by
  rcases hc with ⟨hK, hn, hxK, hxn, hs⟩
  have hh : n-x ≤ N-K := by omega
  have he : N-K-(n-x) = N+x-K-n := by omega
  rw [mass, ite_eq_left ⟨hK, hn, hxK, hxn, hs⟩, Nat.cast_choose ℝ hxK,
    Nat.cast_choose ℝ hh, Nat.cast_choose ℝ hn, he]
  field_simp (discharger := positivity)

theorem mass_swap (K N n x : ℕ) : mass K N n x = mass n N K x := by
  have hs : Compatible K N n x ↔ Compatible n N K x := by unfold Compatible; omega
  have he : N+x-n-K = N+x-K-n := by omega
  by_cases H : Compatible K N n x
  · rw [mass, ite_eq_left H, mass, ite_eq_left (hs.mp H), he]
    ring
  · rw [mass, ite_eq_right H, mass, ite_eq_right (fun H' ↦ H (hs.mpr H'))]

theorem likelihood_swap (K N n : ℕ) (x : Fin h → ℕ) :
    likelihood K N n x = likelihood n N K x :=
  Finset.prod_congr rfl fun i _ ↦ mass_swap K N n (x i)

private theorem mass_pos {K N n x : ℕ} (hc : Compatible K N n x) :
    0 < mass K N n x := by
  rw [mass, ite_eq_left hc]
  exact div_pos (mul_pos (mul_pos (mul_pos (fact_pos _) (fact_pos _)) (fact_pos _))
    (fact_pos _)) (mul_pos (mul_pos (mul_pos (mul_pos (fact_pos _) (fact_pos _))
      (fact_pos _)) (fact_pos _)) (fact_pos _))

private theorem likelihood_pos {K N n : ℕ} {x : Fin h → ℕ}
    (hc : ∀ i, Compatible K N n (x i)) : 0 < likelihood K N n x :=
  Finset.prod_pos fun i _ ↦ mass_pos (hc i)

/-- The feasible composition interval for held population and draw count. -/
def compositionDomain (N n : ℕ) (x : Fin h → ℕ) : Set ℕ :=
  {K | K ≤ N ∧ ∀ i, x i ≤ K ∧ x i ≤ n ∧ K+n ≤ N+x i}

/-- The feasible population ray for held success and draw counts. -/
def populationDomain (K n : ℕ) (x : Fin h → ℕ) : Set ℕ :=
  {N | K ≤ N ∧ n ≤ N ∧ ∀ i, x i ≤ K ∧ x i ≤ n ∧ K+n ≤ N+x i}

private theorem composition_support {K N n : ℕ} {x : Fin h → ℕ}
    (hn : n ≤ N) (hs : K ∈ compositionDomain N n x) (i : Fin h) :
    Compatible K N n (x i) :=
  ⟨hs.1, hn, (hs.2 i).1, (hs.2 i).2.1, (hs.2 i).2.2⟩

private theorem population_support {K N n : ℕ} {x : Fin h → ℕ}
    (hs : N ∈ populationDomain K n x) (i : Fin h) :
    Compatible K N n (x i) :=
  ⟨hs.1, hs.2.1, (hs.2.2 i).1, (hs.2.2 i).2.1, (hs.2.2 i).2.2⟩

def compositionRatio (K N n : ℕ) (x : Fin h → ℕ) : ℝ :=
  ∏ i, ((K+1 : ℕ) : ℝ) * ((N+x i-K-n : ℕ) : ℝ) / (((K+1-x i : ℕ) : ℝ) * ((N-K : ℕ) : ℝ))

def populationRatio (K N n : ℕ) (x : Fin h → ℕ) : ℝ :=
  ∏ i, ((N+1-K : ℕ) : ℝ) * ((N+1-n : ℕ) : ℝ) / (((N+1 : ℕ) : ℝ) * ((N+1+x i-K-n : ℕ) : ℝ))

private theorem cast_fact_succ (j : ℕ) :
    ((j+1).factorial : ℝ) = ((j : ℝ)+1) * j.factorial := by
  simp [Nat.factorial_succ, Nat.cast_mul]

private theorem composition_mass_step {K N n x : ℕ} (hc : Compatible K N n x)
    (hd : Compatible (K+1) N n x) :
    mass (K+1) N n x = mass K N n x *
      (((K+1 : ℕ) : ℝ) * ((N+x-K-n : ℕ) : ℝ) / (((K+1-x : ℕ) : ℝ) * ((N-K : ℕ) : ℝ))) := by
  rcases hc with ⟨hK, hn, hxK, hxn, hs⟩
  have hd' := hd.2.2.2.2
  have hK' := hd.1
  rw [mass, ite_eq_left hd, mass, ite_eq_left ⟨hK, hn, hxK, hxn, hs⟩,
    show K+1-x = K-x+1 by omega,
    show N-K = (N-(K+1))+1 by omega,
    show N+x-K-n = (N+x-(K+1)-n)+1 by omega]
  simp only [cast_fact_succ, Nat.cast_add, Nat.cast_one]
  field_simp (discharger := positivity)

private theorem population_mass_step {K N n x : ℕ} (hc : Compatible K N n x) :
    mass K (N+1) n x = mass K N n x *
      (((N+1-K : ℕ) : ℝ) * ((N+1-n : ℕ) : ℝ) / (((N+1 : ℕ) : ℝ) * ((N+1+x-K-n : ℕ) : ℝ))) := by
  rcases hc with ⟨hK, hn, hxK, hxn, hs⟩
  have hd : Compatible K (N+1) n x := ⟨by omega, by omega, hxK, hxn, by omega⟩
  rw [mass, ite_eq_left hd, mass, ite_eq_left ⟨hK, hn, hxK, hxn, hs⟩,
    show N+1-K = N-K+1 by omega, show N+1-n = N-n+1 by omega,
    show N+1+x-K-n = N+x-K-n+1 by omega]
  simp only [cast_fact_succ, Nat.cast_add, Nat.cast_one]
  field_simp (discharger := positivity)

theorem composition_adjacent {K N n : ℕ} {x : Fin h → ℕ} (hn : n ≤ N)
    (hc : K ∈ compositionDomain N n x) (hd : K+1 ∈ compositionDomain N n x) :
    likelihood (K+1) N n x = likelihood K N n x * compositionRatio K N n x := by
  rw [likelihood, likelihood, compositionRatio, ← Finset.prod_mul_distrib]
  exact Finset.prod_congr rfl fun i _ ↦
    composition_mass_step (composition_support hn hc i) (composition_support hn hd i)

theorem population_adjacent {K N n : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) :
    likelihood K (N+1) n x = likelihood K N n x * populationRatio K N n x := by
  rw [likelihood, likelihood, populationRatio, ← Finset.prod_mul_distrib]
  exact Finset.prod_congr rfl fun i _ ↦ population_mass_step (population_support hc i)

private theorem prod_ratio_eq_one {a b : Fin h → ℕ} (hb : ∀ i, 0 < b i) :
    (∏ i, (a i : ℝ) / (b i : ℝ)) = 1 ↔ (∏ i, a i) = ∏ i, b i := by
  have hp : 0 < ∏ i, (b i : ℝ) := Finset.prod_pos fun i _ ↦ Nat.cast_pos.mpr (hb i)
  rw [Finset.prod_div_distrib, div_eq_iff hp.ne', one_mul]
  exact_mod_cast (show ((∏ i, a i : ℕ) : ℝ) = ((∏ i, b i : ℕ) : ℝ) ↔
    (∏ i, a i) = ∏ i, b i from Nat.cast_inj)

private theorem mul_eq_self_iff {a r : ℝ} (ha : 0 < a) : a * r = a ↔ r = 1 := by
  constructor
  · intro H
    apply mul_left_cancel₀ ha.ne'
    simpa using H
  · rintro rfl
    simp

theorem composition_tie_iff {K N n : ℕ} {x : Fin h → ℕ} (hn : n ≤ N)
    (hc : K ∈ compositionDomain N n x) (hd : K+1 ∈ compositionDomain N n x) :
    likelihood (K+1) N n x = likelihood K N n x ↔
      (∏ i, (K+1)*(N+x i-K-n)) = ∏ i, (K+1-x i)*(N-K) := by
  rw [composition_adjacent hn hc hd, mul_eq_self_iff (likelihood_pos (composition_support hn hc))]
  simpa only [compositionRatio, Nat.cast_mul] using
    (prod_ratio_eq_one (a := fun i ↦ (K+1)*(N+x i-K-n))
      (b := fun i ↦ (K+1-x i)*(N-K)) fun i ↦ by
        have := hc.2 i
        have := hd.1
        exact Nat.mul_pos (by omega) (by omega))

theorem population_tie_iff {K N n : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) :
    likelihood K (N+1) n x = likelihood K N n x ↔
      (∏ _ : Fin h, (N+1-K)*(N+1-n)) = ∏ i, (N+1)*(N+1+x i-K-n) := by
  rw [population_adjacent hc, mul_eq_self_iff (likelihood_pos (population_support hc))]
  simpa only [populationRatio, Nat.cast_mul] using
    (prod_ratio_eq_one (a := fun _ ↦ (N+1-K)*(N+1-n))
      (b := fun i ↦ (N+1)*(N+1+x i-K-n)) fun i ↦ by
        have := hc.2.2 i
        exact Nat.mul_pos (by omega) (by omega))


/-- Exact cancellation can produce a tie even when the observation-wise
ratios differ from one. -/
theorem mixed_composition_tie :
    likelihood 3 5 2 (![0, 2] : Fin 2 → ℕ) = likelihood 2 5 2 ![0, 2] ∧
      (3 * (5+0-2-2) : ℕ) ≠ (3-0)*(5-2) := by
  have h₂ : 2 ∈ compositionDomain 5 2 (![0, 2] : Fin 2 → ℕ) := by
    refine ⟨by decide, fun i ↦ ?_⟩
    fin_cases i <;> decide
  have h₃ : 3 ∈ compositionDomain 5 2 (![0, 2] : Fin 2 → ℕ) := by
    refine ⟨by decide, fun i ↦ ?_⟩
    fin_cases i <;> decide
  refine ⟨(composition_tie_iff (by decide) h₂ h₃).2 ?_, by decide⟩
  decide

private theorem composition_factor {K N n : ℕ} {x : Fin h → ℕ}
    (hc : K ∈ compositionDomain N n x) (hd : K+1 ∈ compositionDomain N n x)
    (i : Fin h) :
    ((K+1 : ℕ) : ℝ) * ((N+x i-K-n : ℕ) : ℝ) /
        (((K+1-x i : ℕ) : ℝ) * ((N-K : ℕ) : ℝ)) =
      (1 + (x i : ℝ)/((K : ℝ)+1-x i)) *
        (1 - ((n : ℝ)-x i)/((N : ℝ)-K)) := by
  have h := hc.2 i
  have hk := hd.1
  have hx : x i ≤ K+1 := by omega
  have hNK : K ≤ N := by omega
  have hsub : (N+x i-K : ℕ) ≥ n := by omega
  have hsub' : K ≤ N+x i := by omega
  have phx : (x i : ℝ) < (K : ℝ)+1 := by exact_mod_cast (show x i < K+1 by omega)
  have phk : (K : ℝ) < N := by exact_mod_cast (show K < N by omega)
  push_cast [Nat.cast_sub hx, Nat.cast_sub hNK, Nat.cast_sub hsub, Nat.cast_sub hsub']
  field_simp (discharger := linarith)
  ring

/-- Each adjacent composition ratio strictly decreases when the held draw
count is positive and both steps remain feasible. -/
theorem composition_ratio_strict {K J N n : ℕ} {x : Fin h → ℕ} (hh : 0 < h)
    (hn : 0 < n) (hKJ : K < J)
    (hK : K ∈ compositionDomain N n x) (hK' : K+1 ∈ compositionDomain N n x)
    (hJ : J ∈ compositionDomain N n x) (hJ' : J+1 ∈ compositionDomain N n x) :
    compositionRatio J N n x < compositionRatio K N n x := by
  unfold compositionRatio
  apply Finset.prod_lt_prod_of_nonempty₀
  · intro i _
    have h := (hJ'.2 i).2.2
    have hx := (hJ.2 i).1
    have hN := hJ'.1
    exact div_pos (mul_pos (by positivity) (Nat.cast_pos.mpr (by omega)))
      (mul_pos (Nat.cast_pos.mpr (by omega)) (Nat.cast_pos.mpr (by omega)))
  · intro i _
    rw [composition_factor hK hK' i, composition_factor hJ hJ' i]
    have hkj : (K : ℝ) < J := by exact_mod_cast hKJ
    have hx : (x i : ℝ) ≤ K := by exact_mod_cast (hK.2 i).1
    have hxn : (x i : ℝ) ≤ n := by exact_mod_cast (hK.2 i).2.1
    have hj : (J : ℝ) < N := by exact_mod_cast (show J < N from hJ'.1)
    have hs : (J : ℝ)+1+n ≤ (N : ℝ)+x i := by exact_mod_cast (hJ'.2 i).2.2
    have p : 0 < (K : ℝ)+1-x i := by linarith
    have q : 0 < (N : ℝ)-J := by linarith
    have a₁ : 1 + (x i : ℝ)/((J : ℝ)+1-x i) ≤ 1 + (x i : ℝ)/((K : ℝ)+1-x i) :=
      add_le_add_right
        (div_le_div_of_nonneg_left (Nat.cast_nonneg (x i)) p
          (show (K : ℝ)+1-x i ≤ (J : ℝ)+1-x i by linarith)) 1
    have a₂ : 1 - ((n : ℝ)-x i)/((N : ℝ)-J) ≤ 1 - ((n : ℝ)-x i)/((N : ℝ)-K) :=
      sub_le_sub_left
        (div_le_div_of_nonneg_left (sub_nonneg.mpr hxn) q
          (show (N : ℝ)-J ≤ N-K by linarith)) 1
    have b₁ : 0 < 1 + (x i : ℝ)/((K : ℝ)+1-x i) :=
      add_pos_of_pos_of_nonneg zero_lt_one (div_nonneg (Nat.cast_nonneg _) p.le)
    have b₂ : 0 < 1 - ((n : ℝ)-x i)/((N : ℝ)-J) := by
      rw [sub_pos, div_lt_iff₀ q, one_mul]
      linarith
    by_cases hz : x i = 0
    · have hp : 0 < (n : ℝ)-x i := by
        have hn' : (0 : ℝ) < n := by exact_mod_cast hn
        simp [hz, hn']
      have H := sub_lt_sub_left (div_lt_div_of_pos_left hp q (show (N : ℝ)-J < N-K by linarith)) 1
      exact (mul_le_mul_of_nonneg_right a₁ b₂.le).trans_lt (mul_lt_mul_of_pos_left H b₁)
    · have hp : (0 : ℝ) < x i := Nat.cast_pos.mpr (Nat.pos_of_ne_zero hz)
      have H := add_lt_add_right
        (div_lt_div_of_pos_left hp p (show (K : ℝ)+1-x i < (J : ℝ)+1-x i by linarith)) 1
      exact (mul_lt_mul_of_pos_right H b₂).trans_le (mul_le_mul_of_nonneg_left a₂ b₁.le)
  · exact ⟨⟨0, hh⟩, Finset.mem_univ _⟩

private def popCenter (K n A : ℝ) : ℝ := (A-K)*(A-n)/A

private theorem population_ratio_real {K N n : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) :
    populationRatio K N n x =
      ∏ i, popCenter K n ((N : ℝ)+1) / ((N : ℝ)+1+x i-K-n) := by
  unfold populationRatio popCenter
  apply Finset.prod_congr rfl
  intro i _
  have hi := hc.2.2 i
  have h₁ : K ≤ N+1 := by have := hc.1; omega
  have h₂ : n ≤ N+1 := by have := hc.2.1; omega
  have h₃ : K ≤ N+1+x i := by omega
  have h₄ : n ≤ N+1+x i-K := by omega
  push_cast [Nat.cast_sub h₁, Nat.cast_sub h₂, Nat.cast_sub h₃, Nat.cast_sub h₄]
  rw [div_div]

private theorem popCenter_pos {K n A : ℝ} (hA : 0 < A) (hK : K < A) (hn : n < A) :
    0 < popCenter K n A :=
  div_pos (mul_pos (sub_pos.mpr hK) (sub_pos.mpr hn)) hA

private theorem popCenter_eq {K n A : ℝ} (hA : 0 < A) :
    popCenter K n A = A-K-n+K*n/A := by
  unfold popCenter
  field_simp [hA.ne']
  ring

/-- Shifting positive factors and the normalizer by the same amount preserves
a nonnegative sum of normalized logs, by concavity of the logarithm. -/
private theorem shifted_product {a : Fin h → ℝ} {C d : ℝ} (hC : 0 < C) (hd : 0 < d)
    (ha : ∀ i, 0 < a i) (H : 1 ≤ ∏ i, a i/C) :
    1 ≤ ∏ i, (a i+d)/(C+d) := by
  have hdC : 0 < C+d := by linarith
  have p (i : Fin h) : 0 < a i/C := div_pos (ha i) hC
  have q (i : Fin h) : 0 < (a i+d)/(C+d) := div_pos (by linarith [ha i]) hdC
  have Hsum : 0 ≤ ∑ i, Real.log (a i/C) := by
    rw [← Real.log_prod fun i _ ↦ (p i).ne']
    exact Real.log_nonneg H
  apply (Real.log_nonneg_iff (Finset.prod_pos fun i _ ↦ q i)).1
  rw [Real.log_prod fun i _ ↦ (q i).ne']
  have Hj (i : Fin h) :
      (C/(C+d)) * Real.log (a i/C) ≤ Real.log ((a i+d)/(C+d)) := by
    have hj := strictConcaveOn_log_Ioi.concaveOn.2
      (show a i/C ∈ Ioi 0 from p i) (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)
      (div_nonneg hC.le hdC.le) (div_nonneg hd.le hdC.le)
      (show C/(C+d)+d/(C+d)=1 by rw [← add_div, div_self hdC.ne'])
    have he : (C/(C+d)) * (a i/C) + (d/(C+d))*1 = (a i+d)/(C+d) := by
      field_simp [hC.ne', hdC.ne']
    simpa only [smul_eq_mul, Real.log_one, mul_zero, add_zero, he] using hj
  calc
    0 ≤ (C/(C+d)) * ∑ i, Real.log (a i/C) := mul_nonneg (by positivity) Hsum
    _ = ∑ i, (C/(C+d)) * Real.log (a i/C) := Finset.mul_sum ..
    _ ≤ ∑ i, Real.log ((a i+d)/(C+d)) := Finset.sum_le_sum fun i _ ↦ Hj i

/-- Once a feasible population ratio reaches one or below, all later ratios
are strictly below one when both held counts are positive. -/
theorem population_single_crossing {K n N J : ℕ} {x : Fin h → ℕ} (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hc : N ∈ populationDomain K n x) (hNJ : N < J)
    (H : populationRatio K N n x ≤ 1) : populationRatio K J n x < 1 := by
  let A : ℝ := (N : ℝ)+1
  let B : ℝ := (J : ℝ)+1
  let C := popCenter K n A
  let D := popCenter K n B
  let a (i : Fin h) : ℝ := A+x i-K-n
  have hJ : J ∈ populationDomain K n x :=
    ⟨hc.1.trans hNJ.le, hc.2.1.trans hNJ.le, fun i ↦ by have := hc.2.2 i; omega⟩
  have hAB : A < B := by dsimp [A, B]; exact_mod_cast (show N+1 < J+1 by omega)
  have hA : 0 < A := by dsimp [A]; positivity
  have hB := hA.trans hAB
  have hKC : (K : ℝ) < A := by dsimp [A]; exact_mod_cast (show K < N+1 by have := hc.1; omega)
  have hnC : (n : ℝ) < A := by dsimp [A]; exact_mod_cast (show n < N+1 by have := hc.2.1; omega)
  have hC : 0 < C := popCenter_pos hA hKC hnC
  have hD : 0 < D := popCenter_pos hB (hKC.trans hAB) (hnC.trans hAB)
  have ha (i : Fin h) : 0 < a i := by
    have hi : (K : ℝ)+n ≤ (N : ℝ)+x i := by exact_mod_cast (hc.2.2 i).2.2
    dsimp [a, A]
    linarith
  have hshift : D < C+(B-A) := by
    dsimp [D, C]
    rw [popCenter_eq hA, popCenter_eq hB]
    have hp : 0 < (K : ℝ)*n := mul_pos (by exact_mod_cast hK) (by exact_mod_cast hn)
    linarith [div_lt_div_of_pos_left hp hA hAB]
  rw [population_ratio_real hc] at H
  have H' : 1 ≤ ∏ i, a i/C := by
    have He : (∏ i, C/a i) = (∏ i, a i/C)⁻¹ := by
      rw [← Finset.prod_inv_distrib]
      simp only [inv_div]
    change (∏ i, C/a i) ≤ 1 at H
    rw [He] at H
    exact (inv_le_one₀ (Finset.prod_pos fun i _ ↦ div_pos (ha i) hC)).1 H
  have HS := shifted_product hC (sub_pos.mpr hAB) ha H'
  have HB : 1 < ∏ i, (a i+(B-A))/D := by
    apply HS.trans_lt
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      exact div_pos (add_pos (ha i) (sub_pos.mpr hAB)) (by linarith)
    · intro i _
      exact div_lt_div_of_pos_left (add_pos (ha i) (sub_pos.mpr hAB)) hD hshift
    · exact ⟨⟨0, hh⟩, Finset.mem_univ _⟩
  rw [population_ratio_real hJ]
  have E (i : Fin h) : B+x i-K-n = a i+(B-A) := by dsimp [a]; ring
  change (∏ i, D/(B+x i-K-n)) < 1
  simp only [E]
  have He : (∏ i, D/(a i+(B-A))) = (∏ i, (a i+(B-A))/D)⁻¹ := by
    rw [← Finset.prod_inv_distrib]
    simp only [inv_div]
  rw [He]
  exact (inv_lt_one₀ (Finset.prod_pos fun i _ ↦
    div_pos (add_pos (ha i) (sub_pos.mpr hAB)) hD)).2 HB


/-- Characterize the maxima by the first feasible index whose next step either
leaves the interval or fails to increase the likelihood. -/
private theorem interval_maximizers (L R : ℕ → ℝ) (D : Set ℕ) (b : ℕ)
    (hb : b ∈ D) (lower : ∀ j ∈ D, b ≤ j)
    (closed : ∀ j ∈ D, ∀ k, b ≤ k → k ≤ j → k ∈ D)
    (pos : ∀ j ∈ D, 0 < L j)
    (step : ∀ j ∈ D, j+1 ∈ D → L (j+1) = L j * R j)
    (cross : ∀ j k, j ∈ D → j+1 ∈ D → k ∈ D → k+1 ∈ D → j < k →
      R j ≤ 1 → R k < 1)
    (hit : ∃ j, b ≤ j ∧ (j+1 ∉ D ∨ R j ≤ 1)) :
    ∃ k ∈ D, ∀ j, (j ∈ D ∧ IsMaxOn L D j) ↔
      j = k ∨ (j = k+1 ∧ k+1 ∈ D ∧ R k = 1) := by
  classical
  let k := Nat.find hit
  obtain ⟨hbk, hstop⟩ := Nat.find_spec hit
  have advance (j : ℕ) (hj : b ≤ j) (hjk : j < k) : j+1 ∈ D ∧ 1 < R j := by
    have H := Nat.find_min hit hjk
    exact ⟨by by_contra hn; exact H ⟨hj, Or.inl hn⟩,
      lt_of_not_ge (fun hn ↦ H ⟨hj, Or.inr hn⟩)⟩
  have hk : k ∈ D := by
    by_cases he : k = b
    · rwa [he]
    · have H := (advance (k-1) (by omega) (by omega)).1
      rwa [show k-1+1 = k by omega] at H
  have riseAux (s : ℕ) (hs : s ≤ k) (j : ℕ) (hj : j ∈ D) (hjs : j < s) :
      L j < L s := by
    induction s with
    | zero => omega
    | succ t ih =>
      have ht' := closed k hk t (by have := lower j hj; omega) (by omega)
      have H := advance t (lower t ht') (by omega)
      have inc : L t < L (t+1) := by
        rw [step t ht' H.1]
        simpa using mul_lt_mul_of_pos_left H.2 (pos t ht')
      by_cases he : j = t
      · simpa [he] using inc
      · exact (ih (by omega) (by omega)).trans inc
  have rise (j : ℕ) (hj : j ∈ D) (hjk : j < k) : L j < L k :=
    riseAux k le_rfl j hj hjk
  have next (hn : k+1 ∈ D) : L (k+1) ≤ L k := by
    rw [step k hk hn]
    simpa using mul_le_mul_of_nonneg_left (hstop.resolve_left (fun H ↦ H hn)) (pos k hk).le
  have tie (hn : k+1 ∈ D) : L (k+1) = L k ↔ R k = 1 := by
    rw [step k hk hn, mul_eq_self_iff (pos k hk)]
  have fall (j : ℕ) (hj : j ∈ D) (hj' : k+1 < j) : L j < L (k+1) := by
    induction j with
    | zero => omega
    | succ t ih =>
      have hn := closed (t+1) hj (k+1) (by omega) (by omega)
      have ht' := closed (t+1) hj t (by omega) (by omega)
      have dec : L (t+1) < L t := by
        rw [step t ht' hj]
        simpa using mul_lt_mul_of_pos_left
          (cross k t hk hn ht' hj (by omega) (hstop.resolve_left (fun H ↦ H hn))) (pos t ht')
      by_cases he : t = k+1
      · simpa [he] using dec
      · exact dec.trans (ih ht' (by omega))
  have best : IsMaxOn L D k := by
    intro j hj
    change L j ≤ L k
    rcases lt_trichotomy j k with hlt | rfl | hgt
    · exact (rise j hj hlt).le
    · rfl
    have hn := closed j hj (k+1) (by omega) (by omega)
    by_cases he : j = k+1
    · simpa [he] using next hn
    · exact (fall j hj (by omega)).le.trans (next hn)
  refine ⟨k, hk, fun j ↦ ⟨?_, ?_⟩⟩
  · rintro ⟨hj, hm⟩
    have H : L k ≤ L j := hm hk
    rcases lt_trichotomy j k with hlt | rfl | hgt
    · exact False.elim ((rise j hj hlt).not_ge H)
    · exact Or.inl rfl
    have hn := closed j hj (k+1) (by omega) (by omega)
    by_cases he : j = k+1
    · exact Or.inr ⟨he, hn, (tie hn).mp ((next hn).antisymm (he ▸ H))⟩
    · exact False.elim (((fall j hj (by omega)).trans_le (next hn)).not_ge H)
  · rintro (rfl | ⟨rfl, hn, he⟩)
    · exact ⟨hk, best⟩
    · exact ⟨hn, fun j hj ↦ (best hj).trans_eq ((tie hn).mpr he).symm⟩

theorem composition_maximizers {N n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hn : 0 < n) (hN : n ≤ N) (hne : (compositionDomain N n x).Nonempty) :
    ∃ k ∈ compositionDomain N n x, ∀ j,
      (j ∈ compositionDomain N n x ∧ IsMaxOn (fun K ↦ likelihood K N n x)
        (compositionDomain N n x) j) ↔
      j = k ∨ (j = k+1 ∧ k+1 ∈ compositionDomain N n x ∧ compositionRatio k N n x = 1) := by
  classical
  let b := Nat.find hne
  have hb := Nat.find_spec hne
  have lower (j : ℕ) (hj : j ∈ compositionDomain N n x) : b ≤ j := Nat.find_min' hne hj
  refine interval_maximizers _ _ _ b hb lower ?_ ?_ (fun _ ↦ composition_adjacent hN) ?_ ?_
  · intro j hj k hk hkj
    refine ⟨hkj.trans hj.1, fun i ↦ ?_⟩
    have := hb.2 i
    have := hj.2 i
    omega
  · intro j hj
    exact likelihood_pos (composition_support hN hj)
  · intro j k hj hj' hk hk' hjk H
    exact (composition_ratio_strict hh hn hjk hj hj' hk hk').trans_le H
  · exact ⟨N, hb.1, Or.inl (fun H ↦ by have := H.1; omega)⟩

theorem draws_adjacent {K N n : ℕ} {x : Fin h → ℕ} (hK : K ≤ N)
    (hc : n ∈ compositionDomain N K x) (hd : n+1 ∈ compositionDomain N K x) :
    likelihood K N (n+1) x = likelihood K N n x * compositionRatio n N K x := by
  rw [likelihood_swap K N (n+1), likelihood_swap K N n, composition_adjacent hK hc hd]

theorem draws_tie_iff {K N n : ℕ} {x : Fin h → ℕ} (hK : K ≤ N)
    (hc : n ∈ compositionDomain N K x) (hd : n+1 ∈ compositionDomain N K x) :
    likelihood K N (n+1) x = likelihood K N n x ↔
      (∏ i, (n+1)*(N+x i-n-K)) = ∏ i, (n+1-x i)*(N-n) := by
  rw [likelihood_swap K N (n+1), likelihood_swap K N n]
  exact composition_tie_iff hK hc hd

/-- With population and composition held, the draw-count fit is the
composition fit with the two margins interchanged. -/
theorem draws_maximizers {N K : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hN : K ≤ N) (hne : (compositionDomain N K x).Nonempty) :
    ∃ k ∈ compositionDomain N K x, ∀ j,
      (j ∈ compositionDomain N K x ∧ IsMaxOn (fun n ↦ likelihood K N n x)
        (compositionDomain N K x) j) ↔
      j = k ∨ (j = k+1 ∧ k+1 ∈ compositionDomain N K x ∧ compositionRatio k N K x = 1) := by
  have E : (fun n ↦ likelihood K N n x) = fun n ↦ likelihood n N K x :=
    funext fun n ↦ likelihood_swap K N n x
  rw [E]
  exact composition_maximizers x hh hK hN hne

theorem composition_flat {N K : ℕ} (x : Fin h → ℕ) (hK : K ≤ N) (hz : ∀ i, x i = 0) :
    likelihood K N 0 x = 1 := by
  apply Finset.prod_eq_one
  intro i _
  rw [hz i, mass_choose (show Compatible K N 0 0 from ⟨hK, by omega, by omega, by omega, by omega⟩)]
  simp

theorem population_flat {K N n : ℕ} {x : Fin h → ℕ} (hc : N ∈ populationDomain K n x)
    (hz : K = 0 ∨ n = 0) : likelihood K N n x = 1 := by
  rcases hz with rfl | rfl
  · rw [likelihood_swap]
    exact composition_flat x hc.2.1 fun i ↦ Nat.eq_zero_of_le_zero (hc.2.2 i).1
  · exact composition_flat x hc.1 fun i ↦ Nat.eq_zero_of_le_zero (hc.2.2 i).2.1

private theorem desc_ratio_limit (K j : ℕ) :
    Tendsto (fun N : ℕ ↦ (((N-K).descFactorial j : ℕ) : ℝ) / N.descFactorial j)
      atTop (𝓝 1) := by
  induction j with
  | zero => simp
  | succ j ih =>
    have H := (tendsto_add_mul_div_add_mul_atTop_nhds (-((K : ℝ)+j)) (-(j : ℝ))
      (1 : ℝ) one_ne_zero).mul ih
    simp only [one_mul, div_one] at H
    apply H.congr'
    filter_upwards [eventually_ge_atTop (K+j)] with N hN
    have hk : K ≤ N := by omega
    have hj : j ≤ N-K := by omega
    have hj' : j ≤ N := by omega
    simp only [Nat.descFactorial_succ, Nat.cast_mul, Nat.cast_sub hj, Nat.cast_sub hj',
      Nat.cast_sub hk]
    rw [div_mul_div_comm]
    have h₁ : -((K : ℝ)+j) + N = (N : ℝ)-K-j := by ring
    have h₂ : -(j : ℝ) + N = (N : ℝ)-j := by ring
    rw [h₁, h₂]

private theorem desc_extra_limit (j x : ℕ) :
    Tendsto (fun N : ℕ ↦ (N.descFactorial j : ℝ) / N.descFactorial (j+x))
      atTop (𝓝 (if x = 0 then 1 else 0)) := by
  induction x with
  | zero =>
    simp only [add_zero, ite_true]
    apply tendsto_const_nhds.congr'
    filter_upwards [eventually_ge_atTop j] with N hN
    exact (div_self (Nat.cast_ne_zero.mpr (Nat.descFactorial_pos.mpr hN).ne')).symm
  | succ x ih =>
    have H : Tendsto (fun N : ℕ ↦ 1 / ((N : ℝ)-(j+x : ℝ))) atTop (𝓝 0) := by
      simpa [sub_eq_add_neg, add_comm] using
        (tendsto_add_mul_div_add_mul_atTop_nhds (1 : ℝ) (-(j+x : ℝ)) 0 one_ne_zero)
    have HH := ih.mul H
    simp only [mul_zero, Nat.succ_ne_zero, ite_false] at HH ⊢
    apply HH.congr'
    filter_upwards [eventually_ge_atTop (j+x)] with N hN
    rw [show j+(x+1) = j+x+1 by omega, Nat.descFactorial_succ N (j+x),
      Nat.cast_mul, Nat.cast_sub hN, Nat.cast_add, div_mul_div_comm, mul_one,
      mul_comm ((N : ℝ)-_)]

/-- For fixed margins the mass converges pointwise to the atom at zero. -/
theorem population_mass_limit (K n x : ℕ) :
    Tendsto (fun N : ℕ ↦ mass K N n x) atTop (𝓝 (if x = 0 then 1 else 0)) := by
  by_cases hs : x ≤ K ∧ x ≤ n
  · have E (N : ℕ) (hN : K+n ≤ N) :
        mass K N n x = ((K.choose x : ℝ) * (n.descFactorial x : ℝ)) *
          (((N-K).descFactorial (n-x) : ℝ) / N.descFactorial (n-x)) *
          ((N.descFactorial (n-x) : ℝ) / N.descFactorial n) := by
      have h₂ : n ≤ N := by omega
      have h₃ : n-x ≤ N := by omega
      have hd (a b : ℕ) :
          (a.descFactorial b : ℝ) = (b.factorial : ℝ) * a.choose b := by
        rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
      have A : (n.descFactorial x : ℝ) * ((n-x).factorial : ℝ) = n.factorial := by
        rw [mul_comm]
        exact_mod_cast Nat.factorial_mul_descFactorial hs.2
      have hp : (0 : ℝ) < N.descFactorial (n-x) :=
        Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr h₃)
      have hq : (0 : ℝ) < N.choose n := Nat.cast_pos.mpr (Nat.choose_pos h₂)
      have hc : Compatible K N n x := ⟨by omega, h₂, hs.1, hs.2, by omega⟩
      rw [mass_choose hc, hd (N-K), hd N n]
      field_simp [hp.ne', hq.ne', (fact_pos n).ne']
      linear_combination -((K.choose x : ℝ) * ((N-K).choose (n-x) : ℝ)) * A
    by_cases hz : x = 0
    · subst x
      simp only [ite_true]
      apply (desc_ratio_limit K n).congr'
      filter_upwards [eventually_ge_atTop (K+n)] with N hN
      have hd : (N.descFactorial n : ℝ) ≠ 0 :=
        Nat.cast_ne_zero.mpr (Nat.descFactorial_pos.mpr (by omega)).ne'
      simp [E N hN, hd]
    · have He : n-x+x = n := by omega
      have H := desc_extra_limit (n-x) x
      rw [He] at H
      have HH := ((tendsto_const_nhds (x := (K.choose x : ℝ) * (n.descFactorial x : ℝ))).mul
        (desc_ratio_limit K (n-x))).mul H
      simp only [mul_zero, hz, ite_false] at HH ⊢
      exact HH.congr' ((eventually_ge_atTop (K+n)).mono fun N hN ↦ (E N hN).symm)
  · have hz : x ≠ 0 := by intro H; subst x; simp at hs
    simp [hz, mass, Compatible, show ∀ N : ℕ, ¬ (K ≤ N ∧ n ≤ N ∧ x ≤ K ∧ x ≤ n ∧
      K+n ≤ N+x) from fun _ H ↦ hs ⟨H.2.2.1, H.2.2.2.1⟩]

theorem population_likelihood_limit (K n : ℕ) (x : Fin h → ℕ) :
    Tendsto (fun N ↦ likelihood K N n x) atTop (𝓝 (if (∀ i, x i = 0) then 1 else 0)) := by
  classical
  have H := tendsto_finsetProd Finset.univ (fun i _ ↦ population_mass_limit K n (x i))
  unfold likelihood
  convert H using 2
  split_ifs with hx
  · simp [hx]
  · push Not at hx
    obtain ⟨i, hi⟩ := hx
    symm
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    simp [hi]

private theorem population_upward {K n N J : ℕ} {x : Fin h → ℕ}
    (hc : N ∈ populationDomain K n x) (hNJ : N ≤ J) : J ∈ populationDomain K n x :=
  ⟨hc.1.trans hNJ, hc.2.1.trans hNJ, fun i ↦ by have := hc.2.2 i; omega⟩

theorem population_maximizers {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hs : ∀ i, x i ≤ K ∧ x i ≤ n)
    (hx : ∃ i, 0 < x i) :
    ∃ k ∈ populationDomain K n x, ∀ j,
      (j ∈ populationDomain K n x ∧ IsMaxOn (fun N ↦ likelihood K N n x)
        (populationDomain K n x) j) ↔
      j = k ∨ (j = k+1 ∧ populationRatio K k n x = 1) := by
  classical
  have hne : (populationDomain K n x).Nonempty :=
    ⟨K+n, by exact ⟨by omega, by omega, fun i ↦ ⟨(hs i).1, (hs i).2, by omega⟩⟩⟩
  let b := Nat.find hne
  have hb := Nat.find_spec hne
  have hl (j : ℕ) (hj : j ∈ populationDomain K n x) : b ≤ j := Nat.find_min' hne hj
  have hit : ∃ j, b ≤ j ∧ (j+1 ∉ populationDomain K n x ∨ populationRatio K j n x ≤ 1) := by
    by_contra H
    have rising (j : ℕ) (hj : b ≤ j) : 1 < populationRatio K j n x :=
      lt_of_not_ge (fun hle ↦ H ⟨j, hj, Or.inr hle⟩)
    have mono (j : ℕ) (hj : b ≤ j) : likelihood K b n x ≤ likelihood K j n x := by
      induction j with
      | zero => simp [show b = 0 by omega]
      | succ j ih =>
        by_cases he : b = j+1
        · simp [he]
        have hj' : b ≤ j := by omega
        have hs' := population_upward hb hj'
        rw [population_adjacent hs']
        exact (ih hj').trans (by
          simpa using mul_le_mul_of_nonneg_left (rising j hj').le
            (likelihood_pos (population_support hs')).le)
    have hnot : ¬ ∀ i, x i = 0 := by
      obtain ⟨i, hi⟩ := hx
      exact fun H ↦ hi.ne' (H i)
    have lim := population_likelihood_limit K n x
    rw [ite_eq_right hnot] at lim
    exact (likelihood_pos (population_support hb)).not_ge
      (ge_of_tendsto lim ((eventually_ge_atTop b).mono fun j hj ↦ mono j hj))
  obtain ⟨k, hk, hm⟩ := interval_maximizers (fun N ↦ likelihood K N n x)
    (fun N ↦ populationRatio K N n x) _ b hb hl
    (fun _ _ _ hb' _ ↦ population_upward hb hb')
    (fun _ hj ↦ likelihood_pos (population_support hj))
    (fun _ hj _ ↦ population_adjacent hj)
    (fun _ _ hj _ _ _ hlt H ↦ population_single_crossing hh hK hn hj hlt H) hit
  refine ⟨k, hk, fun j ↦ ?_⟩
  simpa only [population_upward hk (Nat.le_succ _), true_and] using hm j

theorem population_zero_escape {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hz : ∀ i, x i = 0) :
    StrictMonoOn (fun N ↦ likelihood K N n x) (populationDomain K n x) ∧
    Tendsto (fun N ↦ likelihood K N n x) atTop (𝓝 1) ∧
    IsLUB ((fun N ↦ likelihood K N n x) '' populationDomain K n x) 1 ∧
    ¬ ∃ N ∈ populationDomain K n x,
      IsMaxOn (fun N ↦ likelihood K N n x) (populationDomain K n x) N := by
  have mono : StrictMonoOn (fun N ↦ likelihood K N n x) (populationDomain K n x) := by
    intro N hN J _ hNJ
    apply Nat.rel_of_forall_rel_succ_of_le_of_lt (· < ·)
      (f := fun N ↦ likelihood K N n x) (a := N) _ le_rfl hNJ
    intro j hj
    have hs := population_upward hN hj
    rw [population_adjacent hs, population_ratio_real hs]
    have H : 1 < ∏ i : Fin h, popCenter K n ((j : ℝ)+1) / ((j : ℝ)+1+x i-K-n) := by
      conv_lhs => rw [← show (∏ _ : Fin h, (1 : ℝ)) = 1 by simp]
      apply Finset.prod_lt_prod_of_nonempty₀
      · simp
      · intro i _
        have hi := (hs.2.2 i).2.2
        rw [hz i] at hi
        have hi' : (K : ℝ)+n ≤ j := by exact_mod_cast (show K+n ≤ j by omega)
        have hA : 0 < (j : ℝ)+1 := by positivity
        have hp : 0 < (K : ℝ)*n := mul_pos (by exact_mod_cast hK) (by exact_mod_cast hn)
        simp only [hz i, Nat.cast_zero, add_zero]
        rw [lt_div_iff₀ (by linarith), one_mul, popCenter_eq hA]
        linarith [div_pos hp hA]
      · exact ⟨⟨0, hh⟩, Finset.mem_univ _⟩
    simpa using mul_lt_mul_of_pos_left H (likelihood_pos (population_support hs))
  have lim : Tendsto (fun N ↦ likelihood K N n x) atTop (𝓝 1) := by
    simpa [hz] using population_likelihood_limit K n x
  refine ⟨mono, lim, ⟨?_, ?_⟩, ?_⟩
  · rintro _ ⟨N, hN, rfl⟩
    exact ge_of_tendsto lim ((eventually_ge_atTop N).mono fun J hj ↦
      mono.monotoneOn hN (population_upward hN hj) hj)
  · intro u hu
    have hb : K+n ∈ populationDomain K n x :=
      ⟨by omega, by omega, fun i ↦ by simp [hz i]⟩
    exact le_of_tendsto lim ((eventually_ge_atTop (K+n)).mono fun J hj ↦
      hu ⟨J, population_upward hb hj, rfl⟩)
  · rintro ⟨N, hN, hm⟩
    have hs := population_upward hN (Nat.le_succ N)
    exact (mono hN hs (Nat.lt_succ_self N)).not_ge (hm hs)

/-- On a truncated population ray a positive-margin all-zero sample has its
unique maximum at the cap, provided that cap is feasible. -/
theorem capped_zero_population_maximizer {K n B : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hK : 0 < K) (hn : 0 < n) (hz : ∀ i, x i = 0)
    (hB : B ∈ populationDomain K n x) (j : ℕ) :
    (j ∈ populationDomain K n x ∩ Iic B ∧ IsMaxOn (fun N ↦ likelihood K N n x)
      (populationDomain K n x ∩ Iic B) j) ↔ j = B := by
  have mono := (population_zero_escape x hh hK hn hz).1
  have hb : B ∈ populationDomain K n x ∩ Iic B := ⟨hB, mem_Iic.mpr le_rfl⟩
  constructor
  · rintro ⟨hj, hm⟩
    by_contra he
    exact (mono hj.1 hB (lt_of_le_of_ne hj.2 he)).not_ge (hm hb)
  · rintro rfl
    exact ⟨hb, fun _ hj ↦ mono.monotoneOn hj.1 hB hj.2⟩

theorem population_nonattainment_iff {K n : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hs : ∀ i, x i ≤ K ∧ x i ≤ n) :
    (¬ ∃ N ∈ populationDomain K n x,
      IsMaxOn (fun N ↦ likelihood K N n x) (populationDomain K n x) N) ↔
      0 < K ∧ 0 < n ∧ ∀ i, x i = 0 := by
  refine ⟨?_, fun ⟨hK, hn, hx⟩ ↦ (population_zero_escape x hh hK hn hx).2.2.2⟩
  intro H
  have hb : K+n ∈ populationDomain K n x :=
    ⟨by omega, by omega, fun i ↦ ⟨(hs i).1, (hs i).2, by omega⟩⟩
  have hp : 0 < K ∧ 0 < n := by
    by_contra hp
    have hz : K = 0 ∨ n = 0 := by omega
    exact H ⟨K+n, hb, fun J hj ↦ by
      change likelihood K J n x ≤ likelihood K (K+n) n x
      rw [population_flat hj hz, population_flat hb hz]⟩
  refine ⟨hp.1, hp.2, ?_⟩
  by_contra! Hx
  obtain ⟨i, hi⟩ := Hx
  obtain ⟨k, _, hm⟩ := population_maximizers x hh hp.1 hp.2 hs ⟨i, Nat.pos_of_ne_zero hi⟩
  exact H ⟨k, (hm k |>.2 (Or.inl rfl)).1, (hm k |>.2 (Or.inl rfl)).2⟩

/-- The swap gives a second distinct maximizer on any swap-invariant search
domain, whenever both free margins of a maximizer are different. -/
theorem distinct_swap_maximizers (x : Fin h → ℕ) (D : Set (ℕ × ℕ × ℕ))
    (hD : ∀ {K N n}, (K,N,n) ∈ D → (n,N,K) ∈ D) {K N n : ℕ}
    (hp : (K,N,n) ∈ D) (hne : K ≠ n)
    (hm : IsMaxOn (fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x) D (K,N,n)) :
    (n,N,K) ∈ D ∧ (n,N,K) ≠ (K,N,n) ∧
      IsMaxOn (fun p : ℕ × ℕ × ℕ ↦ likelihood p.1 p.2.1 p.2.2 x) D (n,N,K) := by
  refine ⟨hD hp, fun H ↦ hne (congrArg Prod.fst H).symm, fun j hj ↦ ?_⟩
  exact (hm hj).trans_eq (likelihood_swap K N n x)

theorem capped_held_draw_zero_atom {B K N : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hBN : B ≤ N) (hNB : N ≤ B) (hz : ∀ i, x i = 0) :
    likelihood K N B x = (if K = 0 then 1 else 0) := by
  have he : N = B := by omega
  subst N
  by_cases hk : K = 0
  · subst K
    rw [likelihood_swap, composition_flat x le_rfl hz, ite_eq_left rfl]
  · rw [ite_eq_right hk]
    apply Finset.prod_eq_zero (Finset.mem_univ (⟨0, hh⟩ : Fin h))
    rw [hz _, mass, ite_eq_right]
    intro H
    have := H.2.2.2.2
    omega

theorem capped_held_success_zero_atom {B n N : ℕ} (x : Fin h → ℕ) (hh : 0 < h)
    (hBN : B ≤ N) (hNB : N ≤ B) (hz : ∀ i, x i = 0) :
    likelihood B N n x = (if n = 0 then 1 else 0) := by
  rw [likelihood_swap, capped_held_draw_zero_atom x hh hBN hNB hz]

private theorem mass_zero_le_one (K N n : ℕ) : mass K N n 0 ≤ 1 := by
  by_cases hc : Compatible K N n 0
  · rw [mass_choose hc, Nat.choose_zero_right, Nat.cast_one, one_mul, Nat.sub_zero]
    have H : (0 : ℝ) < N.choose n := Nat.cast_pos.mpr (Nat.choose_pos hc.2.1)
    apply (div_le_one₀ H).2
    exact_mod_cast Nat.choose_le_choose n (Nat.sub_le N K)
  · rw [mass, ite_eq_right hc]
    exact zero_le_one

theorem capped_population_zero_tie {B : ℕ} (x : Fin h → ℕ) (hB : 0 < B)
    (hz : ∀ i, x i = 0) :
    (0,B) ≠ (B,0) ∧
      IsMaxOn (fun p : ℕ × ℕ ↦ likelihood p.1 B p.2 x) univ (0,B) ∧
      IsMaxOn (fun p : ℕ × ℕ ↦ likelihood p.1 B p.2 x) univ (B,0) := by
  have H := composition_flat x (K := B) (N := B) le_rfl hz
  have bound (K n : ℕ) : likelihood K B n x ≤ 1 := by
    unfold likelihood
    conv_rhs => rw [← show (∏ _ : Fin h, (1 : ℝ)) = 1 by simp]
    apply Finset.prod_le_prod₀
    · intro i _
      by_cases hc : Compatible K B n (x i)
      · exact (mass_pos hc).le
      · simp [mass, hc]
    · intro i _
      rw [hz i]
      exact mass_zero_le_one K B n
  refine ⟨fun he ↦ hB.ne' (congrArg Prod.fst he).symm, ?_, ?_⟩
  · intro p _
    change likelihood p.1 B p.2 x ≤ likelihood 0 B B x
    rw [likelihood_swap 0 B B, H]
    exact bound p.1 p.2
  · intro p _
    change likelihood p.1 B p.2 x ≤ likelihood B B 0 x
    rw [H]
    exact bound p.1 p.2

end

end UnivariateML.Hypergeometric

namespace UnivariateML.DiscreteUniform

noncomputable section

variable {h : ℕ}

def compatible (x : Fin h → ℤ) (a b : ℤ) : Prop :=
  ∀ i, a ≤ x i ∧ x i ≤ b

def likelihood (x : Fin h → ℤ) (a b : ℤ) : ℝ := by
  classical
  exact if compatible x a b then ((b : ℝ)-a+1)⁻¹ ^ h else 0

private theorem sample_nonempty (x : Fin h → ℤ) (hh : 0 < h) :
    (Finset.univ.image x).Nonempty :=
  ⟨x ⟨0, hh⟩, Finset.mem_image_of_mem x (Finset.mem_univ _)⟩

def sampleMin (x : Fin h → ℤ) (hh : 0 < h) : ℤ :=
  (Finset.univ.image x).min'
    ⟨x ⟨0, hh⟩, Finset.mem_image_of_mem x (Finset.mem_univ _)⟩

def sampleMax (x : Fin h → ℤ) (hh : 0 < h) : ℤ :=
  (Finset.univ.image x).max'
    ⟨x ⟨0, hh⟩, Finset.mem_image_of_mem x (Finset.mem_univ _)⟩

theorem compatible_iff (x : Fin h → ℤ) (hh : 0 < h) (a b : ℤ) :
    compatible x a b ↔ a ≤ sampleMin x hh ∧ sampleMax x hh ≤ b := by
  constructor
  · intro H
    obtain ⟨i, _, hi⟩ := Finset.mem_image.mp (Finset.min'_mem _ (sample_nonempty x hh))
    obtain ⟨j, _, hj⟩ := Finset.mem_image.mp (Finset.max'_mem _ (sample_nonempty x hh))
    change x i = sampleMin x hh at hi
    change x j = sampleMax x hh at hj
    exact ⟨hi ▸ (H i).1, hj ▸ (H j).2⟩
  · intro H i
    have hi := Finset.mem_image_of_mem x (Finset.mem_univ i)
    exact ⟨H.1.trans (Finset.min'_le _ _ hi), (Finset.le_max' _ _ hi).trans H.2⟩

private theorem width_pos {x : Fin h → ℤ} {a b : ℤ} (hh : 0 < h)
    (hc : compatible x a b) : 0 < (b : ℝ)-a+1 := by
  have H : (a : ℝ) ≤ b := by
    exact_mod_cast ((hc ⟨0, hh⟩).1.trans (hc ⟨0, hh⟩).2)
  linarith

theorem likelihood_pos_iff (x : Fin h → ℤ) (hh : 0 < h) (a b : ℤ) :
    0 < likelihood x a b ↔ compatible x a b := by
  by_cases hc : compatible x a b
  · simp only [likelihood, hc, ite_true, iff_true]
    exact pow_pos (inv_pos.mpr (width_pos hh hc)) _
  · simp [likelihood, hc]

private theorem strict_of_width {x : Fin h → ℤ} (hh : 0 < h) {a b c d : ℤ}
    (hc : compatible x c d) (w : d-c < b-a) :
    likelihood x a b < likelihood x c d := by
  by_cases hab : compatible x a b
  · rw [likelihood, ite_eq_left hab, likelihood, ite_eq_left hc]
    have w' : (d : ℝ)-c < (b : ℝ)-a := by exact_mod_cast w
    exact pow_lt_pow_left₀ (inv_strictAnti₀ (width_pos hh hc) (by linarith))
      (inv_nonneg.mpr (width_pos hh hab).le) hh.ne'
  · rw [likelihood, ite_eq_right hab]
    exact (likelihood_pos_iff x hh c d).mpr hc

private theorem strict_of_inclusion {x : Fin h → ℤ} (hh : 0 < h) {a b c d : ℤ}
    (hc : compatible x c d) (ha : a ≤ c) (hb : d ≤ b) (hne : (a,b) ≠ (c,d)) :
    likelihood x a b < likelihood x c d := by
  apply strict_of_width hh hc
  by_contra! H
  exact hne (Prod.ext (by omega) (by omega))

theorem joint_maximizer (x : Fin h → ℤ) (hh : 0 < h) (p : ℤ × ℤ) :
    IsMaxOn (fun q : ℤ × ℤ ↦ likelihood x q.1 q.2) univ p ↔
      p = (sampleMin x hh, sampleMax x hh) := by
  let a := sampleMin x hh
  let b := sampleMax x hh
  have hc : compatible x a b := (compatible_iff x hh a b).mpr ⟨le_rfl, le_rfl⟩
  have strict (q : ℤ × ℤ) (hne : q ≠ (a,b)) : likelihood x q.1 q.2 < likelihood x a b := by
    by_cases hq : compatible x q.1 q.2
    · obtain ⟨ha, hb⟩ := (compatible_iff x hh _ _).mp hq
      exact strict_of_inclusion hh hc ha hb hne
    · rw [likelihood, ite_eq_right hq]
      exact (likelihood_pos_iff x hh _ _).mpr hc
  constructor
  · intro hm
    by_contra! H
    exact (strict p H).not_ge (hm (mem_univ (a,b)))
  · rintro rfl q _
    by_cases he : q = (a,b)
    · rw [he]
      exact le_rfl
    · exact (strict q he).le

theorem held_lower_maximizer (x : Fin h → ℤ) (hh : 0 < h) {a : ℤ}
    (ha : a ≤ sampleMin x hh) (b : ℤ) :
    IsMaxOn (likelihood x a) univ b ↔ b = sampleMax x hh := by
  let d := sampleMax x hh
  have hc : compatible x a d := (compatible_iff x hh _ _).mpr ⟨ha, le_rfl⟩
  have strict (j : ℤ) (hne : j ≠ d) : likelihood x a j < likelihood x a d := by
    by_cases hj : compatible x a j
    · exact strict_of_inclusion hh hc le_rfl ((compatible_iff x hh _ _).mp hj).2
        (fun H ↦ hne (congrArg Prod.snd H))
    · rw [likelihood, ite_eq_right hj]
      exact (likelihood_pos_iff x hh _ _).mpr hc
  constructor
  · intro hm
    by_contra! H
    exact (strict b H).not_ge (hm (mem_univ d))
  · rintro rfl j _
    by_cases he : j = d
    · rw [he]
      exact le_rfl
    · exact (strict j he).le

theorem held_upper_maximizer (x : Fin h → ℤ) (hh : 0 < h) {b : ℤ}
    (hb : sampleMax x hh ≤ b) (a : ℤ) :
    IsMaxOn (fun j ↦ likelihood x j b) univ a ↔ a = sampleMin x hh := by
  let c := sampleMin x hh
  have hc : compatible x c b := (compatible_iff x hh _ _).mpr ⟨le_rfl, hb⟩
  have strict (j : ℤ) (hne : j ≠ c) : likelihood x j b < likelihood x c b := by
    by_cases hj : compatible x j b
    · exact strict_of_inclusion hh hc ((compatible_iff x hh _ _).mp hj).1 le_rfl
        (fun H ↦ hne (congrArg Prod.fst H))
    · rw [likelihood, ite_eq_right hj]
      exact (likelihood_pos_iff x hh _ _).mpr hc
  constructor
  · intro hm
    by_contra! H
    exact (strict a H).not_ge (hm (mem_univ c))
  · rintro rfl j _
    by_cases he : j = c
    · rw [he]
      exact le_rfl
    · exact (strict j he).le

theorem incompatible_held_zero (x : Fin h → ℤ) (hh : 0 < h) {a b : ℤ}
    (H : sampleMin x hh < a ∨ b < sampleMax x hh) : likelihood x a b = 0 := by
  rw [likelihood, ite_eq_right]
  intro hc
  obtain ⟨ha, hb⟩ := (compatible_iff x hh a b).mp hc
  exact H.elim ha.not_gt hb.not_gt

end

end UnivariateML.DiscreteUniform
