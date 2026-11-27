module

public import UnivariateML.Family.Discrete3Skellam
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Skellam fixed-product mean profiling for arbitrary signed samples

The convolution changes by a common exponential factor under a change
of log rate ratio at fixed product. This works at zero rates as well.
No radial uniqueness claim is made for samples with several magnitudes.
-/

@[expose] public section

open Real Set Filter
open UnivariateML.Discrete3Skellam

namespace UnivariateML.FinishSkellam

noncomputable section

private theorem poisson_tilt (r u : ℝ) (j : ℕ) :
    poissonMass (r*exp u) j =
      exp (r-r*exp u+(j : ℝ)*u)*poissonMass r j := by
  unfold poissonMass
  rw [mul_pow, ← exp_nat_mul]
  calc
    _ = exp (-(r*exp u)+(j : ℝ)*u) * r^j/(j.factorial : ℝ) := by rw [exp_add]; ring
    _ = _ := by
      simp only [← mul_div_assoc, ← mul_assoc, ← exp_add]
      rw [show r-r*exp u+(j : ℝ)*u + -r = -(r*exp u)+(j : ℝ)*u by ring]

private theorem mass_tilt (k : ℤ) (a b u : ℝ) :
    mass k (a*exp u,b*exp (-u)) =
      exp (a+b-a*exp u-b*exp (-u)+(k : ℝ)*u)*mass k (a,b) := by
  unfold mass
  rw [← tsum_mul_left]
  refine tsum_congr fun j ↦ ?_
  simp only [poisson_tilt]
  rw [show
    (exp (a-a*exp u+((j+k.toNat : ℕ) : ℝ)*u)*poissonMass a (j+k.toNat)) *
      (exp (b-b*exp (-u)+((j+(-k).toNat : ℕ) : ℝ)*(-u))*poissonMass b (j+(-k).toNat)) =
    (exp (a-a*exp u+((j+k.toNat : ℕ) : ℝ)*u) *
      exp (b-b*exp (-u)+((j+(-k).toNat : ℕ) : ℝ)*(-u))) *
      (poissonMass a (j+k.toNat)*poissonMass b (j+(-k).toNat)) by ring, ← exp_add]
  have hk : (k.toNat : ℝ)-((-k).toNat : ℝ) = (k : ℝ) := by
    exact_mod_cast (show (k.toNat : ℤ)-((-k).toNat : ℤ) = k by omega)
  push_cast
  congr 2
  linear_combination u*hk

/-- Tilting the rate ratio at fixed product applies a known exponential
factor to the sample likelihood, for every signed sample. -/
theorem likelihood_tilt {n : ℕ} (x : Fin n → ℤ) (a b u : ℝ) :
    likelihood x (a*exp u,b*exp (-u)) =
      exp ((n : ℝ)*(a+b-a*exp u-b*exp (-u)) + (∑ i, (x i : ℝ))*u) *
        likelihood x (a,b) := by
  unfold likelihood
  simp only [mass_tilt, Finset.prod_mul_distrib, ← exp_sum, Finset.sum_add_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_mul]

/-- Every joint maximizer fits the signed sample mean, including on a
compatible Poisson axis. -/
theorem maximizing_rate_difference {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
    p.1-p.2 = (∑ i, (x i : ℝ))/n := by
  obtain ⟨a,b⟩ := p
  have hbase : ((1,1) : ℝ × ℝ) ∈ domain := ⟨zero_le_one,zero_le_one⟩
  have hpos : 0 < likelihood x (a,b) :=
    (Finset.prod_pos (fun i _ ↦ mass_pos (x i) one_pos one_pos) :
      0 < likelihood x (1,1)).trans_le (hm hbase)
  let f (u : ℝ) := likelihood x (a*exp u,b*exp (-u))
  have hmax : IsMaxOn f univ 0 := by
    intro u _
    have ht : (a*exp u,b*exp (-u)) ∈ domain :=
      ⟨mul_nonneg hp.1 (exp_pos _).le,mul_nonneg hp.2 (exp_pos _).le⟩
    simpa [f] using hm ht
  let S : ℝ := ∑ i, (x i : ℝ)
  let E (u : ℝ) := (n : ℝ)*(a+b-a*exp u-b*exp (-u))+S*u
  have hE : HasDerivAt E ((n : ℝ)*(-a+b)+S) 0 := by
    have hneg : HasDerivAt (fun u : ℝ ↦ exp (-u)) (-1) 0 := by
      simpa using ((hasDerivAt_id (0 : ℝ)).neg.exp)
    have h := ((((hasDerivAt_exp (0 : ℝ)).const_mul a).const_sub (a+b)).sub
      (hneg.const_mul b)).const_mul (n : ℝ)
    convert h.add ((hasDerivAt_id (0 : ℝ)).const_mul S) using 1
    · rfl
    · simp
  have hzero : E 0 = 0 := by simp [E, sub_eq_add_neg, add_assoc]
  have hd := hE.exp.mul_const (likelihood x (a,b))
  rw [hzero,exp_zero,one_mul] at hd
  have hfun : (fun u ↦ exp (E u)*likelihood x (a,b)) = f :=
    funext fun u ↦ (likelihood_tilt x a b u).symm
  rw [hfun] at hd
  have hscore : ((n : ℝ)*(-a+b)+S)*likelihood x (a,b) = 0 :=
    hd.deriv.symm.trans (hmax.isLocalMax univ_mem).deriv_eq_zero
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  apply (eq_div_iff hn'.ne').mpr
  have he := (mul_eq_zero.mp hscore).resolve_right hpos.ne'
  dsimp [S] at he
  nlinarith only [he]

private theorem mass_reflect (k : ℤ) (a b : ℝ) :
    mass (-k) (b,a) = mass k (a,b) := by
  unfold mass
  apply tsum_congr
  intro j
  simp [mul_comm]

private theorem zero_axis {k : ℤ} (hk : 0 < k) (b : ℝ) : mass k (0,b) = 0 := by
  unfold mass
  have hpos : 0 < k.toNat := by omega
  have h (j : ℕ) : poissonMass 0 (j+k.toNat) = 0 := by
    simp [poissonMass, zero_pow (show j+k.toNat ≠ 0 by omega)]
  simp [h]

private theorem two_signs_positive_rates {n : ℕ} {x : Fin n → ℤ}
    (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) (p : ℝ × ℝ) (hp : p ∈ domain)
    (hm : IsMaxOn (likelihood x) domain p) : 0 < p.1 ∧ 0 < p.2 := by
  have hbase : ((1,1) : ℝ × ℝ) ∈ domain := ⟨zero_le_one,zero_le_one⟩
  have hL : 0 < likelihood x p :=
    (Finset.prod_pos (fun i _ ↦ mass_pos (x i) one_pos one_pos) :
      0 < likelihood x (1,1)).trans_le (hm hbase)
  have hnonzero (i : Fin n) : mass (x i) p ≠ 0 := by
    intro he
    exact hL.ne' (Finset.prod_eq_zero (Finset.mem_univ i) he)
  constructor
  · rcases lt_or_eq_of_le (show 0 ≤ p.1 from hp.1) with h | h
    · exact h
    · obtain ⟨i,hi⟩ := hpos
      apply False.elim (hnonzero i ?_)
      rw [show p = (0,p.2) from Prod.ext h.symm rfl]
      exact zero_axis hi _
  · rcases lt_or_eq_of_le (show 0 ≤ p.2 from hp.2) with h | h
    · exact h
    · obtain ⟨i,hi⟩ := hneg
      apply False.elim (hnonzero i ?_)
      rw [show p = (p.1,0) from Prod.ext rfl h.symm, ← mass_reflect]
      exact zero_axis (neg_pos.mpr hi) _

/-- For mean zero and arbitrary observed magnitudes, two-sided data have a
maximizer on the positive diagonal, and every maximizer lies there. -/
theorem mean_zero_two_signs {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (hmean : ∑ i, x i = 0)
    (hpos : ∃ i, 0 < x i) (hneg : ∃ i, x i < 0) :
    (∃ r > 0, IsMaxOn (likelihood x) domain (r,r)) ∧
      ∀ p ∈ domain, IsMaxOn (likelihood x) domain p → 0 < p.1 ∧ p.1 = p.2 := by
  have hd (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p) :
      0 < p.1 ∧ p.1 = p.2 := by
    refine ⟨(two_signs_positive_rates hpos hneg p hp hm).1,?_⟩
    have h := maximizing_rate_difference hn x p hp hm
    rw [← Int.cast_sum, hmean, Int.cast_zero, zero_div] at h
    exact sub_eq_zero.mp h
  obtain ⟨p,hp,hm⟩ := joint_attained hn x
  obtain ⟨hr,he⟩ := hd p hp hm
  exact ⟨⟨p.1,hr,(show p = (p.1,p.1) from Prod.ext rfl he.symm) ▸ hm⟩,hd⟩

/-- Reflection cannot generate a distinct maximizing pair at any signed mean. -/
theorem swapped_maximizers_equal {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) (hp : p ∈ domain) (hm : IsMaxOn (likelihood x) domain p)
    (hs : IsMaxOn (likelihood x) domain (p.2, p.1)) :
    (p.2, p.1) = p := by
  have h := maximizing_rate_difference hn x p hp hm
  have h' := maximizing_rate_difference hn x (p.2, p.1) ⟨hp.2,hp.1⟩ hs
  have he : p.1 = p.2 := by linarith
  exact Prod.ext he.symm he

/-- Mean-zero likelihoods are invariant under exchanging any two positive
rates, for every magnitude histogram. -/
theorem mean_zero_swap {n : ℕ} (x : Fin n → ℤ) (hmean : ∑ i, x i = 0)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    likelihood x (b,a) = likelihood x (a,b) := by
  have hu : a*exp (log (b/a)) = b := by rw [exp_log (div_pos hb ha)]; field_simp
  have hv : b*exp (-log (b/a)) = a := by rw [exp_neg,exp_log (div_pos hb ha)]; field_simp
  have h := likelihood_tilt x a b (log (b/a))
  rw [hu,hv,← Int.cast_sum,hmean,Int.cast_zero,zero_mul,add_zero] at h
  simpa [show a+b-b-a = 0 by ring] using h


/-- Fitting the signed mean makes a positive rate pair the unique
likelihood maximizer on its fixed-product slice. -/
theorem fixed_product_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hmean : a - b = (∑ i, (x i : ℝ)) / n) (u : ℝ) (hu : u ≠ 0) :
    likelihood x (a*exp u,b*exp (-u)) < likelihood x (a,b) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hS := (eq_div_iff hn'.ne').mp hmean
  have hp := mul_lt_mul_of_pos_left (add_one_lt_exp hu) ha
  have hm := mul_lt_mul_of_pos_left (add_one_lt_exp (neg_ne_zero.mpr hu)) hb
  have hE : a+b-a*exp u-b*exp (-u)+(a-b)*u < 0 := by linarith only [hp,hm]
  have h : (n : ℝ)*(a+b-a*exp u-b*exp (-u))+(∑ i, (x i : ℝ))*u < 0 := by
    rw [← hS]
    linarith only [mul_neg_of_pos_of_neg hn' hE]
  rw [likelihood_tilt]
  exact mul_lt_of_lt_one_left (Finset.prod_pos (fun i _ ↦ mass_pos (x i) ha hb))
    (exp_lt_one_iff.mpr h)

/-- Joint maximization reduces exactly to the closed mean-matching set,
which includes a compatible Poisson face. -/
theorem mean_profile_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    (p : ℝ × ℝ) :
    let C := {q ∈ domain | q.1-q.2 = (∑ i, (x i : ℝ))/n}
    (p ∈ domain ∧ IsMaxOn (likelihood x) domain p) ↔
      p ∈ C ∧ IsMaxOn (likelihood x) C p := by
  dsimp only
  constructor
  · rintro ⟨hp,hm⟩
    exact ⟨⟨hp,maximizing_rate_difference hn x p hp hm⟩,fun a ha ↦ hm ha.1⟩
  · rintro ⟨hp,hm⟩
    obtain ⟨q,hq,hqmax⟩ := joint_attained hn x
    exact ⟨hp.1,fun a ha ↦ (hqmax ha).trans (hm ⟨hq,
      maximizing_rate_difference hn x q hq hqmax⟩)⟩

end

end UnivariateML.FinishSkellam
