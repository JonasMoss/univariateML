module

public import UnivariateML.Family.HeldSweepSkellam
public import UnivariateML.Family.Discrete

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Skellam fits with a rate held

The zero held rate restricts the support to the corresponding signed
Poisson half-line. The positive held rate cases use the closed quadrant.
-/

@[expose] public section

open Set Real
open scoped BigOperators

namespace UnivariateML.Discrete4Skellam

open Discrete3Skellam HeldSweepSkellam

private theorem atom_zero (k : ℕ) : poissonMass 0 k = if k = 0 then 1 else 0 := by
  by_cases h : k = 0 <;> simp [poissonMass,h,zero_pow]

private theorem zero_second_mass (k : ℤ) (r : ℝ) :
    mass k (r,0) = if 0 ≤ k then poissonMass r k.toNat else 0 := by
  unfold mass
  rw [tsum_eq_sum (s := {0})]
  · simp only [Finset.sum_singleton, Nat.zero_add, atom_zero]
    by_cases h : 0 ≤ k
    · simp [h,Int.toNat_of_nonpos (by omega : -k ≤ 0)]
    · have he : (-k).toNat ≠ 0 := by omega
      simp [h,he]
  · intro j hj
    have h : j ≠ 0 := by simpa using hj
    have he : j+(-k).toNat ≠ 0 := by omega
    simp [atom_zero,h]

private theorem positive_poisson_product {n : ℕ} (hn : 0 < n)
    (y : Fin n → ℕ) (r : ℝ) (hr : 0 ≤ r) :
    (∏ i, poissonMass r (y i)) =
      Discrete.Poisson.likelihood (Discrete.avg y) r ^ n /
        (∏ i, ((y i).factorial : ℝ)) := by
  by_cases hz : r = 0
  · subst r
    by_cases hy : ∀ i, y i = 0
    · simp [atom_zero,hy,Discrete.Poisson.likelihood,Discrete.avg]
    · obtain ⟨j,hj⟩ := not_forall.mp hy
      have hzero : (∏ i, poissonMass 0 (y i)) = 0 :=
        Finset.prod_eq_zero (Finset.mem_univ j) (by simp [atom_zero,hj])
      simp [hzero,Discrete.Poisson.likelihood, (Discrete.avg_eq_zero_iff y).not.mpr hy,zero_pow hn.ne']
  · have hr' : 0 < r := lt_of_le_of_ne hr (Ne.symm hz)
    have hn' : (n : ℝ) ≠ 0 := Nat.cast_ne_zero.mpr hn.ne'
    rw [Discrete.Poisson.likelihood, ite_eq_right hz, ← exp_nat_mul]
    have he (j : ℕ) : poissonMass r j = exp ((j : ℝ)*log r-r) / j.factorial := by
      rw [poissonMass, sub_eq_add_neg, exp_add, exp_nat_mul, exp_log hr', mul_comm]
    simp_rw [he, Finset.prod_div_distrib, ← exp_sum]
    congr 2
    simp only [Finset.sum_sub_distrib, ← Finset.sum_mul, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, Discrete.avg]
    field_simp [hn']

private theorem zero_second_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (hx : ∀ i, 0 ≤ x i) :
    ∃! r, r ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (w,0)) (Ici 0) r := by
  let y := fun i ↦ (x i).toNat
  let a := Discrete.avg y
  have ha : 0 ≤ a := by
    unfold a Discrete.avg
    exact div_nonneg (Finset.sum_nonneg fun i _ ↦ Nat.cast_nonneg _) (Nat.cast_nonneg _)
  have hD : 0 < ∏ i, ((y i).factorial : ℝ) := by positivity
  have hval (r : ℝ) (hr : 0 ≤ r) : likelihood x (r,0) =
      Discrete.Poisson.likelihood a r ^ n / ∏ i, ((y i).factorial : ℝ) := by
    unfold likelihood
    simp only [zero_second_mass, ite_eq_left (hx _)]
    exact positive_poisson_product hn y r hr
  have hnonneg (r : ℝ) : 0 ≤ Discrete.Poisson.likelihood a r := by
    unfold Discrete.Poisson.likelihood
    split
    · split <;> norm_num
    · exact (exp_pos _).le
  have hcompare {r w : ℝ} (hr : 0 ≤ r) (hw : 0 ≤ w) :
      likelihood x (w,0) ≤ likelihood x (r,0) ↔
        Discrete.Poisson.likelihood a w ≤ Discrete.Poisson.likelihood a r := by
    rw [hval w hw,hval r hr,div_le_div_iff_of_pos_right hD]
    exact pow_le_pow_iff_left₀ (hnonneg w) (hnonneg r) hn.ne'
  have hfit {r : ℝ} (hr : r ∈ Ici (0 : ℝ)) :
      IsMaxOn (fun w ↦ likelihood x (w,0)) (Ici 0) r ↔
        IsMaxOn (Discrete.Poisson.likelihood a) (Ici 0) r := by
    simp only [isMaxOn_iff]
    exact forall₂_congr fun w hw ↦ hcompare hr hw
  refine ⟨a,⟨ha,(hfit ha).mpr ((Discrete.Poisson.unique_mle ha a ha).mpr rfl)⟩,?_⟩
  rintro r ⟨hr,hmax⟩
  exact (Discrete.Poisson.unique_mle ha r hr).mp ((hfit hr).mp hmax)

/-- With the second rate held, positive values admit a unique first rate.
At zero, precisely the nonnegative signed samples admit a unique fit.
Any negative observation forces likelihood zero throughout that fiber. -/
theorem held_second {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {r : ℝ} (hr : 0 ≤ r) :
    ((0 < r ∨ ∀ i, 0 ≤ x i) →
      ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (w,r)) (Ici 0) v) ∧
    (r = 0 → (∃ i, x i < 0) → ∀ v, likelihood x (v,r) = 0) := by
  refine ⟨?_,?_⟩
  · rintro (hp | hx)
    · exact held_second_unique hn x hp
    · rcases eq_or_lt_of_le hr with he | he
      · subst r
        exact zero_second_fit hn x hx
      · exact held_second_unique hn x he
  · rintro rfl ⟨j,hj⟩ v
    exact Finset.prod_eq_zero (Finset.mem_univ j) (by simp [zero_second_mass,hj.not_ge])

/-- Reflection transfers the held-rate result to the first rate. At zero
the admissible signed counts are nonpositive. -/
theorem held_first {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {r : ℝ} (hr : 0 ≤ r) :
    ((0 < r ∨ ∀ i, x i ≤ 0) →
      ∃! v, v ∈ Ici 0 ∧ IsMaxOn (fun w ↦ likelihood x (r,w)) (Ici 0) v) ∧
    (r = 0 → (∃ i, 0 < x i) → ∀ v, likelihood x (r,v) = 0) := by
  have he : (fun w ↦ likelihood (fun i ↦ -x i) (w,r)) =
      (fun w ↦ likelihood x (r,w)) := funext fun w ↦ likelihood_reflection x (r,w)
  have H := held_second hn (fun i ↦ -x i) hr
  rw [he] at H
  refine ⟨by simpa only [neg_nonneg] using H.1, ?_⟩
  intro hz hx v
  rw [← likelihood_reflection x (r,v)]
  exact H.2 hz (by simpa only [neg_lt_zero] using hx) v

private theorem poisson_atom_lt (k : ℕ) {r : ℝ} (hr : 0 ≤ r) (hne : r ≠ k) :
    poissonMass r k < poissonMass k k := by
  let f := Discrete.Poisson.likelihood (k : ℝ)
  have hk : (k : ℝ) ∈ Ici 0 := Nat.cast_nonneg _
  have hM : IsMaxOn f (Ici 0) (k : ℝ) :=
    (Discrete.Poisson.unique_mle hk k hk).mpr rfl
  have hle : f r ≤ f k := hM hr
  have hlt : f r < f k := lt_of_le_of_ne hle (fun he ↦
    hne ((Discrete.Poisson.unique_mle hk r hr).mp (fun w hw ↦ by
      change f w ≤ f r
      rw [he]
      exact hM hw)))
  have hv (u : ℝ) (hu : 0 ≤ u) : poissonMass u k = f u / (k.factorial : ℝ) := by
    simpa [f,Discrete.avg] using positive_poisson_product (n := 1) (by omega) (fun _ ↦ k) u hu
  rw [hv r hr,hv k hk]
  exact div_lt_div_of_pos_right hlt (by positivity)

private theorem poisson_atom_le (k : ℕ) {r : ℝ} (hr : 0 ≤ r) :
    poissonMass r k ≤ poissonMass k k := by
  by_cases he : r = k
  · rw [he]
  · exact (poisson_atom_lt k hr he).le

private theorem optimal_atom_strictAnti : StrictAnti (fun k : ℕ ↦ poissonMass k k) := by
  apply strictAnti_nat_of_succ_lt
  intro k
  have hp : (0 : ℝ) < (k+1 : ℕ) := by positivity
  have he : poissonMass (k+1 : ℕ) (k+1) = poissonMass (k+1 : ℕ) k := by
    simp only [poissonMass,Nat.factorial_succ,Nat.cast_mul,pow_succ]
    field_simp [hp.ne']
  rw [he]
  exact poisson_atom_lt k hp.le (by exact_mod_cast (show k+1 ≠ k by omega))

private theorem poisson_summable {r : ℝ} (hr : 0 ≤ r) : HasSum (poissonMass r) 1 :=
  ProbabilityTheory.hasSum_one_poissonMeasure ⟨r,hr⟩

private theorem nonnegative_atom_fit (k : ℕ) {p : ℝ × ℝ} (hp : p ∈ domain) :
    p = ((k : ℝ),0) ∨ mass (k : ℤ) p < mass (k : ℤ) ((k : ℝ),0) := by
  rcases p with ⟨a,b⟩
  have ha : 0 ≤ a := hp.1
  have hb : 0 ≤ b := hp.2
  have he : mass (k : ℤ) ((k : ℝ),0) = poissonMass k k := by
    simp [zero_second_mass]
  by_cases hz : b = 0
  · subst b
    by_cases h : a = k
    · exact Or.inl (by simp [h])
    · exact Or.inr (by simpa [zero_second_mass] using poisson_atom_lt k ha h)
  · have hb' : 0 < b := lt_of_le_of_ne hb (Ne.symm hz)
    right
    rw [he]
    unfold mass
    simp only [Int.toNat_natCast, Int.toNat_of_nonpos (by omega : -(k : ℤ) ≤ 0), add_zero]
    let C := poissonMass k k
    have hpj (j : ℕ) : poissonMass a (j+k) ≤ C :=
      (poisson_atom_le _ ha).trans (optimal_atom_strictAnti.antitone (by omega : k ≤ j+k))
    have hb₀ (j : ℕ) : 0 ≤ poissonMass b j := by unfold poissonMass; positivity
    have hs (j : ℕ) : 0 ≤ poissonMass a (j+k)*poissonMass b j := by
      unfold poissonMass
      positivity
    have hL (j : ℕ) : poissonMass a (j+k)*poissonMass b j ≤ C*poissonMass b j :=
      mul_le_mul_of_nonneg_right (hpj j) (hb₀ j)
    have hL' : poissonMass a (1+k)*poissonMass b 1 < C*poissonMass b 1 :=
      mul_lt_mul_of_pos_right ((poisson_atom_le _ ha).trans_lt
        (optimal_atom_strictAnti (by omega : k < 1+k))) (by unfold poissonMass; positivity)
    have ht := Summable.tsum_lt_tsum_of_nonneg hs hL (i := 1) hL'
      ((poisson_summable hb).summable.mul_left C)
    rwa [tsum_mul_left,(poisson_summable hb).tsum_eq,mul_one] at ht

/-- Every nonempty constant Skellam sample has a unique maximizing pair.
The compatible Poisson axis supplies the fit, including the origin for zero. -/
theorem constant_unique {n : ℕ} (hn : 0 < n) (c : ℤ) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood (fun _ : Fin n ↦ c)) domain p := by
  have positive (k : ℕ) : ∃! p, p ∈ domain ∧
      IsMaxOn (likelihood (fun _ : Fin n ↦ (k : ℤ))) domain p := by
    let q : ℝ × ℝ := (k,0)
    have hq : q ∈ domain := ⟨Nat.cast_nonneg _,le_rfl⟩
    have hl (p : ℝ × ℝ) : likelihood (fun _ : Fin n ↦ (k : ℤ)) p = mass (k : ℤ) p ^ n := by
      simp [likelihood]
    have hstrict (p : ℝ × ℝ) (hp : p ∈ domain) (hne : p ≠ q) :
        likelihood (fun _ : Fin n ↦ (k : ℤ)) p <
          likelihood (fun _ : Fin n ↦ (k : ℤ)) q := by
      rw [hl,hl]
      have hnon : 0 ≤ mass (k : ℤ) p := by
        have ha : 0 ≤ p.1 := hp.1
        have hb : 0 ≤ p.2 := hp.2
        unfold mass
        exact tsum_nonneg fun _ ↦ by unfold poissonMass; positivity
      exact pow_lt_pow_left₀ ((nonnegative_atom_fit k hp).resolve_left hne) hnon hn.ne'
    refine ⟨q,⟨hq,fun p hp ↦ ?_⟩,fun p ⟨hp,hmax⟩ ↦ ?_⟩
    · by_cases he : p = q
      · subst p
        exact le_rfl
      · exact (hstrict p hp he).le
    · by_contra he
      exact (hstrict p hp he).not_ge (hmax hq)
  by_cases hc : 0 ≤ c
  · exact Int.toNat_of_nonneg hc ▸ positive c.toNat
  · have hcn : 0 ≤ -c := by omega
    have h := Int.toNat_of_nonneg hcn ▸ positive (-c).toNat
    obtain ⟨p,⟨hp,hm⟩,hu⟩ := h
    have hswap (p : ℝ × ℝ) (hp : p ∈ domain) : p.swap ∈ domain := ⟨hp.2,hp.1⟩
    have he (p : ℝ × ℝ) : likelihood (fun _ : Fin n ↦ -c) p.swap =
        likelihood (fun _ : Fin n ↦ c) p := likelihood_reflection _ p
    refine ⟨p.swap,⟨hswap p hp,fun q hq ↦ ?_⟩,fun q ⟨hq,hqM⟩ ↦ ?_⟩
    · have H := hm (hswap q hq)
      change likelihood (fun _ : Fin n ↦ -c) q.swap ≤ likelihood (fun _ : Fin n ↦ -c) p at H
      rw [he q] at H
      change likelihood (fun _ : Fin n ↦ c) q ≤ likelihood (fun _ : Fin n ↦ c) p.swap
      rw [← he p.swap, Prod.swap_swap]
      exact H
    · have H := hu q.swap ⟨hswap q hq,fun w hw ↦ ?_⟩
      · simpa using congrArg Prod.swap H
      · have H := hqM (hswap w hw)
        change likelihood (fun _ : Fin n ↦ c) w.swap ≤ likelihood (fun _ : Fin n ↦ c) q at H
        rw [← he w.swap, ← he q, Prod.swap_swap] at H
        exact H

/-- The proved all-free uniqueness scopes are constants and mean-zero
samples supported on both signs of one positive magnitude. -/
theorem joint_unique_at_stated_scope {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (hx : (∃ c, ∀ i, x i = c) ∨
      (∃ k : ℕ, 0 < k ∧ (∀ i, x i = (k : ℤ) ∨ x i = -(k : ℤ)) ∧
        (∑ i, x i = 0) ∧ (∃ i, 0 < x i) ∧ ∃ i, x i < 0)) :
    ∃! p, p ∈ domain ∧ IsMaxOn (likelihood x) domain p := by
  rcases hx with ⟨c,hc⟩ | ⟨k,hk,hK,hs,hp,hm⟩
  · have he : x = fun _ ↦ c := funext hc
    rw [he]
    exact constant_unique hn c
  · exact joint_unique_one_magnitude hn hk hK hs hp hm

end UnivariateML.Discrete4Skellam

end
