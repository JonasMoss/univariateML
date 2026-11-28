module

public import UnivariateML.Family.Base7Hyper
public import UnivariateML.Family.BinomialSize
import Mathlib.Topology.Sequences
import Mathlib.Probability.Distributions.Poisson.Basic

set_option linter.style.header false

/-! A uniform cutoff for the corner with two large hypergeometric designs.

## Deviations
The cutoff takes a height strictly above the optimized Poisson likelihood.
It does not supply the quadratic cutoff at equidispersion or the slice test.
-/

@[expose] public section

namespace UnivariateML.Base7bHyper

open Filter Set Real Hypergeometric Discrete4Hyper
open scoped Topology

private theorem poisson_product_max {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hX : 0 < ∑ i, x i) {t : ℝ} (ht : 0 ≤ t) :
    let μ : ℝ := (∑ i, (x i : ℝ)) / h
    (∏ i, (t ^ x i / (x i).factorial * exp (-t))) ≤
      ∏ i, (μ ^ x i / (x i).factorial * exp (-μ)) := by
  intro μ
  let X := ∑ i, x i
  have hhr : 0 < (h : ℝ) := by exact_mod_cast hh
  have hXr : 0 < (X : ℝ) := by exact_mod_cast hX
  have he : μ * h = (X : ℝ) := by
    dsimp [μ, X]
    push_cast
    exact div_mul_cancel₀ _ hhr.ne'
  have hμ : 0 < μ := by nlinarith
  simp only [Finset.prod_mul_distrib, Finset.prod_div_distrib, Finset.prod_pow_eq_pow_sum,
    Finset.prod_const, Finset.card_univ, Fintype.card_fin, ← exp_nat_mul]
  have H : (t / μ) ^ X ≤ exp ((t / μ - 1) * X) := by
    rw [mul_comm, exp_nat_mul]
    exact pow_le_pow_left₀ (div_nonneg ht hμ.le)
      (by linarith [add_one_le_exp (t / μ - 1)]) _
  have hpow : t ^ X ≤ μ ^ X * exp ((t / μ - 1) * X) := by
    rw [div_pow, div_le_iff₀ (pow_pos hμ _)] at H
    simpa [mul_comm] using H
  calc
    _ ≤ (μ ^ X * exp ((t / μ - 1) * X)) / (∏ i, ((x i).factorial : ℝ)) *
        exp ((h : ℝ) * -t) := by gcongr
    _ = _ := by
      have E : (t / μ - 1) * (X : ℝ) + h * -t = h * -μ := by
        rw [← he]
        field_simp [hμ.ne']
        ring
      rw [← E, exp_add]
      ring

private theorem large_design_envelope {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    {S K N k : ℕ} (hS : (∑ i, x i) < S) (hK : S ≤ K) (hk : S ≤ k)
    (hc : ∀ i, Compatible K N k (x i)) :
    let C : ℕ := ∑ i, x i
    let b : ℝ := 1 - (C : ℝ) / S
    let t : ℝ := b ^ 2 * ((K : ℝ) * k / N)
    likelihood K N k x ≤
      (∏ i, (t ^ x i / (x i).factorial * exp (-t))) / (b ^ 3) ^ C := by
  let C : ℕ := ∑ i, x i
  let b : ℕ → ℝ := fun S ↦ 1 - (C : ℝ) / S
  let rate : ℝ := (K : ℝ) * k / N
  let t : ℝ := b S ^ 2 * rate
  have hSr : 0 < (S : ℝ) := by exact_mod_cast (show 0 < S by omega)
  have hbr : 0 < b S := sub_pos.mpr ((div_lt_one hSr).mpr (by exact_mod_cast hS))
  have hi : Nonempty (Fin h) := ⟨⟨0, hh⟩⟩
  have hKN := (hc (Classical.choice hi)).1
  have hkN := (hc (Classical.choice hi)).2.1
  have hNr : 0 < (N : ℝ) := by exact_mod_cast (show 0 < N by omega)
  have ht : 0 ≤ t := by dsimp [t, rate]; positivity
  have hfac (i : Fin h) :
      mass K N k (x i) ≤
        (t ^ x i / (x i).factorial * exp (-t)) / (b S ^ 3) ^ x i := by
    let c := x i
    have hcC : c ≤ C := Finset.single_le_sum (fun _ _ ↦ Nat.zero_le _) (Finset.mem_univ i)
    have hcN : c < N := by omega
    have hdepl (j : ℕ) (hj : S ≤ j) :
        b S * (j : ℝ) ≤ (j : ℝ) - c := by
      have hjr : (S : ℝ) ≤ j := by exact_mod_cast hj
      have H := mul_le_mul_of_nonneg_left hjr (show 0 ≤ (C : ℝ) / S by positivity)
      have E : (C : ℝ) / S * S = C := div_mul_cancel₀ _ hSr.ne'
      have Hc : (c : ℝ) ≤ C := by exact_mod_cast hcC
      unfold b
      nlinarith
    have hn : b S ≤ 1 - (c : ℝ) / N := by
      have H := (le_div_iff₀ hNr).mpr (hdepl N (hK.trans hKN))
      rwa [sub_div, div_self hNr.ne'] at H
    have hmul : t ≤ (((K : ℝ) - c) * ((k : ℝ) - c)) / N := by
      dsimp [t, rate]
      apply (le_div_iff₀ hNr).mpr
      rw [← mul_div_assoc, div_mul_cancel₀ _ hNr.ne']
      rw [show b S ^ 2 * ((K : ℝ) * k) = (b S * K) * (b S * k) by ring]
      exact mul_le_mul (hdepl K hK) (hdepl k hk) (by positivity)
        ((by positivity : 0 ≤ b S * (K : ℝ)).trans (hdepl K hK))
    calc
      _ ≤ rate ^ c / ((c.factorial : ℝ) * (1 - (c : ℝ) / N) ^ c) *
          exp (-(((K : ℝ) - c) * ((k : ℝ) - c) / N)) :=
        poisson_envelope_mass (hc i) hcN
      _ ≤ rate ^ c / ((c.factorial : ℝ) * b S ^ c) * exp (-t) := by
        gcongr
      _ = _ := by
        change rate ^ c / ((c.factorial : ℝ) * b S ^ c) * exp (-t) =
          t ^ c / (c.factorial : ℝ) * exp (-t) / (b S ^ 3) ^ c
        rw [show t ^ c = (b S ^ 2) ^ c * rate ^ c from mul_pow _ _ _,
          show (b S ^ 3) ^ c = (b S ^ 2) ^ c * b S ^ c by
            rw [← mul_pow]
            ring]
        field_simp [hbr.ne']
  calc
    likelihood K N k x ≤
      ∏ i, (t ^ x i / (x i).factorial * exp (-t)) / (b S ^ 3) ^ x i :=
      Finset.prod_le_prod₀ (fun i _ ↦ by
        unfold mass
        split <;> positivity) (fun i _ ↦ hfac i)
    _ = _ := by rw [Finset.prod_div_distrib, Finset.prod_pow_eq_pow_sum]

private theorem depletion_factor_tendsto_one (C : ℕ) :
    Tendsto (fun S : ℕ ↦ (1 - (C : ℝ) / S)) atTop (𝓝 1) := by
  simpa using
    (tendsto_const_nhds (x := (1 : ℝ))).sub
      ((tendsto_const_nhds (x := (C : ℝ))).div_atTop tendsto_natCast_atTop_atTop)

/-- Above the fitted positive-mean Poisson height, all compatible urns with
both design counts sufficiently large are excluded, uniformly in the rate. -/
theorem large_design_cutoff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hX : 0 < ∑ i, x i) {B : ℝ}
    (hB : let μ : ℝ := (∑ i, (x i : ℝ)) / h
          (∏ i, μ ^ x i / (x i).factorial * exp (-μ)) < B) :
    ∃ S : ℕ, ∀ K N k : ℕ, S ≤ K → S ≤ k →
      (∀ i, Compatible K N k (x i)) → likelihood K N k x < B := by
  let μ : ℝ := (∑ i, (x i : ℝ)) / h
  let P : ℝ := ∏ i, μ ^ x i / (x i).factorial * exp (-μ)
  let C : ℕ := ∑ i, x i
  let b : ℕ → ℝ := fun S ↦ 1 - (C : ℝ) / S
  have hb : Tendsto b atTop (𝓝 1) := depletion_factor_tendsto_one C
  have hlim : Tendsto (fun S ↦ P / (b S ^ 3) ^ C) atTop (𝓝 P) := by
    simpa only [one_pow, div_one, Pi.div_def] using
      (tendsto_const_nhds (x := P)).div ((hb.pow 3).pow C) (by simp)
  obtain ⟨S, hS, hSB⟩ := ((eventually_gt_atTop C).and
    (hlim.eventually (eventually_lt_nhds hB))).exists
  have hSr : 0 < (S : ℝ) := by exact_mod_cast (show 0 < S by omega)
  have hbS : 0 ≤ b S := (sub_pos.mpr ((div_lt_one hSr).mpr (by exact_mod_cast hS))).le
  refine ⟨S, fun K N k hK hk hc ↦ ?_⟩
  exact ((large_design_envelope hh x hS hK hk hc).trans
    (div_le_div_of_nonneg_right
      (poisson_product_max hh x hX (t := b S ^ 2 * ((K : ℝ) * k / N)) (by positivity))
      (pow_nonneg (pow_nonneg hbS 3) C))).trans_lt hSB

/-- Jointly growing compatible designs with vanishing rate lose on any
sample containing a positive count. At diverging rate every sample loses. -/
theorem joint_rate_extremes {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hK : Tendsto K atTop atTop) (hk : Tendsto k atTop atTop)
    (hc : ∀ᶠ j in atTop, ∀ i, Compatible (K j) (N j) (k j) (x i)) :
    let rate := fun j ↦ (K j : ℝ) * k j / N j
    ((0 < ∑ i, x i) → Tendsto rate atTop (𝓝 0) →
      Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop (𝓝 0)) ∧
    (Tendsto rate atTop atTop →
      Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop (𝓝 0)) := by
  intro rate
  let C : ℕ := ∑ i, x i
  obtain ⟨S, hS⟩ := exists_gt C
  let b : ℝ := 1 - (C : ℝ) / S
  have hbr : 0 < b := sub_pos.mpr ((div_lt_one (by
    exact_mod_cast (show 0 < S by omega))).mpr (by exact_mod_cast hS))
  let t := fun j ↦ b ^ 2 * rate j
  let D : ℝ := (∏ i, ((x i).factorial : ℝ)) * (b ^ 3) ^ C
  have hD : 0 < D := by dsimp [D]; positivity
  have hupper : ∀ᶠ j in atTop,
      likelihood (K j) (N j) (k j) x ≤ (t j ^ C * exp (-(h : ℝ) * t j)) / D := by
    filter_upwards [hc, hK.eventually_ge_atTop S, hk.eventually_ge_atTop S] with j hj hjK hjk
    apply (large_design_envelope hh x hS hjK hjk hj).trans_eq
    change (∏ i, (t j ^ x i / (x i).factorial * exp (-t j))) / (b ^ 3) ^ C =
      t j ^ C * exp (-(h : ℝ) * t j) / D
    simp only [D, C, Finset.prod_mul_distrib, Finset.prod_div_distrib,
      Finset.prod_pow_eq_pow_sum, Finset.prod_const, Finset.card_univ, Fintype.card_fin,
      ← exp_nat_mul]
    rw [show (h : ℝ) * -t j = -(h : ℝ) * t j by ring]
    ring
  have hlower : ∀ᶠ j in atTop, 0 ≤ likelihood (K j) (N j) (k j) x :=
    Eventually.of_forall fun j ↦ Finset.prod_nonneg fun i _ ↦ by
      unfold mass
      split <;> positivity
  have coeff : 0 < (h : ℝ) := by exact_mod_cast hh
  constructor
  · intro hC hr
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ?_ hlower hupper
    have ht : Tendsto t atTop (𝓝 0) := by simpa [t] using hr.const_mul (b ^ 2)
    have he := (continuous_exp.continuousAt.tendsto.comp (ht.const_mul (-(h : ℝ))))
    have hp := ht.pow C
    have hc0 : C ≠ 0 := Nat.ne_of_gt hC
    simpa [zero_pow hc0, mul_zero] using (hp.mul he).div_const D
  · intro hr
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ?_ hlower hupper
    have ht : Tendsto t atTop atTop := hr.const_mul_atTop (by positivity)
    let u := fun j ↦ (h : ℝ) * t j
    have hu : Tendsto u atTop atTop := ht.const_mul_atTop coeff
    have he := (tendsto_pow_mul_exp_neg_atTop_nhds_zero C).comp hu
    have H := he.div_const ((h : ℝ) ^ C * D)
    rw [zero_div] at H
    apply H.congr
    intro j
    change u j ^ C * exp (-u j) / ((h : ℝ) ^ C * D) =
      t j ^ C * exp (-(h : ℝ) * t j) / D
    rw [show -u j = -(h : ℝ) * t j by dsimp [u]; ring]
    dsimp [u]
    rw [mul_pow]
    field_simp [coeff.ne', hD.ne']


private theorem mass_falling {K N k c : ℕ} (hK : K ≤ N) (hk : k ≤ N) (hc : c ≤ k) :
    mass K N k c =
      (k.choose c : ℝ) * K.descFactorial c * (N-K).descFactorial (k-c) /
        N.descFactorial k := by
  by_cases h : Compatible K N k c
  · rw [mass_choose h]
    have hd (a b : ℕ) : (a.descFactorial b : ℝ) = b.factorial * (a.choose b : ℝ) := by
      rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
    rw [hd, hd, hd]
    have e : (k.choose c : ℝ) * c.factorial * (k-c).factorial = k.factorial := by
      exact_mod_cast Nat.choose_mul_factorial_mul_factorial hc
    have hn : (N.choose k : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.choose_pos hk)).ne'
    field_simp [hn]
    linear_combination -((K.choose c : ℝ) * ((N-K).choose (k-c) : ℝ)) * e
  · rw [mass, ite_eq_right h]
    by_cases hcK : c ≤ K
    · have hs : N-K < k-c := by
        unfold Compatible at h
        omega
      simp [Nat.descFactorial_of_lt hs]
    · simp [Nat.descFactorial_of_lt (Nat.lt_of_not_ge hcK)]

private theorem split_mass {K N k c : ℕ} (hc : Compatible K N k c) (hN : c < N) :
    mass K N k c =
      ((K.descFactorial c : ℝ) * k.descFactorial c / (c.factorial * (N.descFactorial c : ℝ))) *
      (((N - K).descFactorial (k - c) : ℝ) / ((N - c).descFactorial (k - c) : ℝ)) := by
  have hck := hc.2.2.2.1
  have hD : 0 < (N.descFactorial c : ℝ) :=
    Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hN.le)
  have hE : 0 < ((N - c).descFactorial (k - c) : ℝ) :=
    Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (by have := hc.2.1; omega))
  have he : (N.descFactorial k : ℝ) =
      ((N - c).descFactorial (k - c) : ℝ) * (N.descFactorial c : ℝ) := by
    exact_mod_cast (Nat.descFactorial_mul_descFactorial (n := N) hck).symm
  have hk' : (k.descFactorial c : ℝ) = (c.factorial : ℝ) * (k.choose c : ℝ) := by
    rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
  rw [mass_falling hc.1 hc.2.1 hck, he, hk']
  field_simp [hD.ne', hE.ne', show (c.factorial : ℝ) ≠ 0 by positivity]

private theorem exp_neg_div_le_sub {z : ℝ} (hz : z < 1) :
    exp (-(z / (1 - z))) ≤ 1 - z := by
  have hp := sub_pos.mpr hz
  have H := log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [log_inv] at H
  have he : (1 - z)⁻¹ - 1 = z / (1 - z) := by field_simp [hp.ne']; ring
  rw [he] at H
  exact (exp_le_exp.mpr (by linarith : -(z / (1 - z)) ≤ log (1 - z))).trans_eq (exp_log hp)

private theorem depletion_sandwich {K N k c : ℕ} (hcK : c ≤ K) (hck : c ≤ k)
    (hgap : K + k < N) :
    let T := ((N - K).descFactorial (k - c) : ℝ) / ((N - c).descFactorial (k - c) : ℝ)
    exp (-(((K : ℝ) - c) * ((k : ℝ) - c)) / ((N : ℝ) - K - k)) ≤ T ∧
    T ≤ exp (-(((K : ℝ) - c) * ((k : ℝ) - c)) / (N : ℝ)) := by
  intro T
  let R := Finset.range (k - c)
  let u : ℝ := ((K : ℝ) - c) / (N : ℝ)
  let v : ℝ := ((K : ℝ) - c) / ((N : ℝ) - K - k)
  have hcK' : (c : ℝ) ≤ K := by exact_mod_cast hcK
  have hkc' : (c : ℝ) ≤ k := by exact_mod_cast hck
  have hNr : 0 < (N : ℝ) := by exact_mod_cast (show 0 < N by omega)
  have hgap' : (K : ℝ) + k < N := by exact_mod_cast hgap
  have facet (j : ℕ) (hj : j ∈ R) :
      exp (-v) ≤ ((N - K - j : ℕ) : ℝ) / ((N - c - j : ℕ) : ℝ) ∧
      ((N - K - j : ℕ) : ℝ) / ((N - c - j : ℕ) : ℝ) ≤ exp (-u) := by
    have hj' := Finset.mem_range.mp hj
    have hjs : (c : ℝ) + j ≤ k := by exact_mod_cast (show c + j ≤ k by omega)
    have hKn : K ≤ N := by omega
    have hcn : c ≤ N := by omega
    have hKj : j ≤ N - K := by omega
    have hcj : j ≤ N - c := by omega
    have hden : 0 < (N : ℝ) - c - j := by linarith
    have hnum : 0 < (N : ℝ) - K - j := by linarith
    let z : ℝ := ((K : ℝ) - c) / ((N : ℝ) - c - j)
    have he : ((N - K - j : ℕ) : ℝ) / ((N - c - j : ℕ) : ℝ) = 1 - z := by
      push_cast [Nat.cast_sub hKn, Nat.cast_sub hcn, Nat.cast_sub hKj, Nat.cast_sub hcj]
      dsimp [z]
      field_simp [hden.ne']
      ring
    have hratio : 1 - z = ((N : ℝ) - K - j) / ((N : ℝ) - c - j) := by
      dsimp [z]
      field_simp [hden.ne']
      ring
    have hz : z < 1 := sub_pos.mp (hratio ▸ div_pos hnum hden)
    have hrl : z / (1 - z) ≤ v := by
      rw [hratio]
      have H : z / (((N : ℝ) - K - j) / ((N : ℝ) - c - j)) =
          ((K : ℝ) - c) / ((N : ℝ) - K - j) := by
        dsimp [z]
        field_simp [hden.ne', hnum.ne']
      rw [H]
      exact div_le_div_of_nonneg_left (sub_nonneg.mpr hcK') (by linarith) (by linarith)
    have hru : u ≤ z := div_le_div_of_nonneg_left (sub_nonneg.mpr hcK') hden
      (by linarith [Nat.cast_nonneg (α := ℝ) c, Nat.cast_nonneg (α := ℝ) j])
    rw [he]
    exact ⟨(exp_le_exp.mpr (neg_le_neg hrl)).trans (exp_neg_div_le_sub hz),
      (sub_le_sub_left hru 1).trans (by simpa [sub_eq_add_neg, add_comm] using add_one_le_exp (-u))⟩
  change _ ≤ _ ∧ _ ≤ _
  dsimp [T]
  rw [Nat.descFactorial_eq_prod_range, Nat.descFactorial_eq_prod_range,
    Nat.cast_prod, Nat.cast_prod, ← Finset.prod_div_distrib]
  have hprod (a : ℝ) : (∏ _ ∈ R, exp (-a)) = exp (-(a * ((k : ℝ) - c))) := by
    simp only [R, Finset.prod_const, Finset.card_range, ← exp_nat_mul, Nat.cast_sub hck]
    congr 1
    ring
  constructor
  · rw [show -(((K : ℝ) - c) * ((k : ℝ) - c)) / ((N : ℝ) - K - k) =
        -(v * ((k : ℝ) - c)) by dsimp [v]; ring, ← hprod v]
    exact Finset.prod_le_prod₀ (fun j _ ↦ (exp_pos _).le) (fun j hj ↦ (facet j hj).1)
  · rw [show -(((K : ℝ) - c) * ((k : ℝ) - c)) / (N : ℝ) =
        -(u * ((k : ℝ) - c)) by dsimp [u]; ring, ← hprod u]
    exact Finset.prod_le_prod₀ (fun j _ ↦ by positivity) (fun j hj ↦ (facet j hj).2)

private theorem normalized_descFactorial {A : ℕ → ℕ} (hA : Tendsto A atTop atTop) (c : ℕ) :
    Tendsto (fun t ↦ ((A t).descFactorial c : ℝ) / (A t : ℝ) ^ c) atTop (𝓝 1) := by
  have hfac (j : ℕ) : Tendsto (fun t ↦ ((A t - j : ℕ) : ℝ) / (A t : ℝ)) atTop (𝓝 1) := by
    refine ((depletion_factor_tendsto_one j).comp hA).congr' ?_
    filter_upwards [hA.eventually_ge_atTop (max 1 j)] with t ht
    have hj := (le_max_right 1 j).trans ht
    have hA0 : (A t : ℝ) ≠ 0 :=
      (Nat.cast_pos.mpr ((le_max_left 1 j).trans ht)).ne'
    simp [Nat.cast_sub hj, sub_div, div_self hA0]
  simpa only [Nat.descFactorial_eq_prod_range, Nat.cast_prod, Finset.prod_div_distrib,
    Finset.prod_const, Finset.card_range, one_pow] using
    tendsto_finsetProd (Finset.range c) (fun j _ ↦ hfac j)

private theorem mass_joint_limit (K N k : ℕ → ℕ) (hK : Tendsto K atTop atTop)
    (hk : Tendsto k atTop atTop) (c : ℕ) (hc : ∀ᶠ j in atTop, Compatible (K j) (N j) (k j) c)
    {r : ℝ} (hr : Tendsto (fun j ↦ (K j : ℝ) * k j / N j) atTop (𝓝 r)) :
    Tendsto (fun j ↦ mass (K j) (N j) (k j) c) atTop (𝓝 (r ^ c / c.factorial * exp (-r))) := by
  let rate := fun j ↦ (K j : ℝ) * k j / N j
  have hN : Tendsto N atTop atTop :=
    tendsto_atTop_mono' _ (hc.mono fun j hj ↦ hj.1) hK
  have hsmall (A B : ℕ → ℕ) (hB : Tendsto B atTop atTop)
      (hab : ∀ j, rate j = (A j : ℝ) * B j / N j) :
      Tendsto (fun j ↦ (A j : ℝ) / N j) atTop (𝓝 0) := by
    have hi : Tendsto (fun j ↦ (B j : ℝ)⁻¹) atTop (𝓝 0) :=
      tendsto_inv_atTop_zero.comp (tendsto_natCast_atTop_atTop.comp hB)
    have H := hr.mul hi
    rw [mul_zero] at H
    apply H.congr'
    filter_upwards [hB.eventually_gt_atTop 0] with j hj
    change rate j * (B j : ℝ)⁻¹ = _
    rw [hab j]
    have hb0 : (B j : ℝ) ≠ 0 := (Nat.cast_pos.mpr hj).ne'
    field_simp [hb0]
  have hpK := hsmall K k hk (fun _ ↦ rfl)
  have hpk := hsmall k K hK (fun t ↦ by simp [rate, mul_comm])
  have hgap : ∀ᶠ j in atTop, K j + k j < N j := by
    have ht := hpK.add hpk
    rw [add_zero] at ht
    filter_upwards [ht.eventually_lt_const (show (0 : ℝ) < 1 by norm_num),
      hN.eventually_gt_atTop 0] with j hj hn
    change (K j : ℝ) / N j + (k j : ℝ) / N j < 1 at hj
    rw [← add_div, div_lt_one (Nat.cast_pos.mpr hn), ← Nat.cast_add] at hj
    exact_mod_cast hj
  have hprefix : Tendsto (fun j ↦ (K j).descFactorial c * ((k j).descFactorial c : ℝ) /
      ((c.factorial : ℝ) * (N j).descFactorial c)) atTop (𝓝 (r ^ c / c.factorial)) := by
    have h := ((normalized_descFactorial hK c).mul (normalized_descFactorial hk c)).div
      (normalized_descFactorial hN c) one_ne_zero
    have H := ((hr.pow c).div_const (c.factorial : ℝ)).mul h
    simp only [div_one, mul_one] at H
    apply H.congr'
    filter_upwards [hK.eventually_gt_atTop 0, hk.eventually_gt_atTop 0,
      hN.eventually_gt_atTop c] with j hj hjk hjN
    have ha : (K j : ℝ) ≠ 0 := (Nat.cast_pos.mpr hj).ne'
    have hb : (k j : ℝ) ≠ 0 := (Nat.cast_pos.mpr hjk).ne'
    have hn : (N j : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.zero_lt_of_lt hjN)).ne'
    have hd : ((N j).descFactorial c : ℝ) ≠ 0 :=
      (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hjN.le)).ne'
    change rate j ^ c / c.factorial *
      (((K j).descFactorial c / (K j : ℝ) ^ c) *
      ((k j).descFactorial c / (k j : ℝ) ^ c) /
      ((N j).descFactorial c / (N j : ℝ) ^ c)) = _
    dsimp [rate]
    rw [div_pow, mul_pow]
    field_simp [ha, hb, hn, hd, show (c.factorial : ℝ) ≠ 0 by positivity]
  have hbalanced : Tendsto (fun j ↦ ((K j : ℝ) - c) * ((k j : ℝ) - c) / N j)
      atTop (𝓝 r) := by
    have hinv : Tendsto (fun j ↦ (N j : ℝ)⁻¹) atTop (𝓝 0) :=
      tendsto_inv_atTop_zero.comp (tendsto_natCast_atTop_atTop.comp hN)
    have H := ((hr.sub (hpK.const_mul (c : ℝ))).sub (hpk.const_mul (c : ℝ))).add
      (hinv.const_mul ((c : ℝ) ^ 2))
    simp only [mul_zero, sub_zero, add_zero] at H
    apply H.congr
    intro j
    change rate j - c * ((K j : ℝ) / N j) - c * ((k j : ℝ) / N j) +
      (c : ℝ) ^ 2 * (N j : ℝ)⁻¹ = _
    dsimp [rate]
    simp only [div_eq_mul_inv]
    ring
  have hden : Tendsto (fun j ↦ ((N j : ℝ) - K j - k j) / (N j : ℝ))
      atTop (𝓝 1) := by
    have H := ((tendsto_const_nhds (x := (1 : ℝ))).sub hpK).sub hpk
    simp only [sub_zero] at H
    apply H.congr'
    filter_upwards [hN.eventually_gt_atTop 0] with j hj
    rw [sub_div, sub_div, div_self (Nat.cast_pos.mpr hj).ne']
  have hlow := hbalanced.div hden one_ne_zero
  rw [div_one] at hlow
  have hlow' : Tendsto (fun j ↦ ((K j : ℝ) - c) * ((k j : ℝ) - c) /
      ((N j : ℝ) - K j - k j)) atTop (𝓝 r) := by
    apply hlow.congr'
    filter_upwards [hgap] with j hj
    have h1 : (N j : ℝ) ≠ 0 := (Nat.cast_pos.mpr (show 0 < N j by omega)).ne'
    have h2 : (N j : ℝ) - K j - k j ≠ 0 := by
      have hJ : (K j : ℝ) + k j < N j := by exact_mod_cast hj
      linarith
    change (((K j : ℝ) - c) * ((k j : ℝ) - c) / N j) /
        (((N j : ℝ) - K j - k j) / (N j : ℝ)) = _
    field_simp [h1, h2]
  have hD : Tendsto (fun j ↦ (((N j - K j).descFactorial (k j - c) : ℝ) /
      ((N j - c).descFactorial (k j - c) : ℝ))) atTop (𝓝 (exp (-r))) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le'
      (continuous_exp.continuousAt.tendsto.comp hlow'.neg)
      (continuous_exp.continuousAt.tendsto.comp hbalanced.neg)
    · filter_upwards [hc, hgap] with j hj hJ
      simpa only [Function.comp_apply, neg_div] using
        (depletion_sandwich hj.2.2.1 hj.2.2.2.1 hJ).1
    · filter_upwards [hc, hgap] with j hj hJ
      simpa only [Function.comp_apply, neg_div] using
        (depletion_sandwich hj.2.2.1 hj.2.2.2.1 hJ).2
  apply (hprefix.mul hD).congr'
  filter_upwards [hc, hN.eventually_gt_atTop c] with j hj hJ
  exact (split_mass hj hJ).symm

/-- For jointly diverging designs, convergence of the rate to a finite
value gives the full Poisson likelihood limit. This includes rate zero
and all-zero samples. -/
theorem joint_poisson_limit {h : ℕ} (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hK : Tendsto K atTop atTop) (hk : Tendsto k atTop atTop)
    (hc : ∀ᶠ j in atTop, ∀ i, Compatible (K j) (N j) (k j) (x i))
    {r : ℝ} (hr : Tendsto (fun j ↦ (K j : ℝ) * k j / N j) atTop (𝓝 r)) :
    Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop
      (𝓝 (∏ i, r ^ x i / (x i).factorial * exp (-r))) :=
  tendsto_finsetProd _ fun i _ ↦ mass_joint_limit K N k hK hk (x i)
    (hc.mono fun _ h ↦ h i) hr

private theorem constant_subsequence_if_not_divergent (A : ℕ → ℕ)
    (hA : ¬ Tendsto A atTop atTop) :
    ∃ (c : ℕ) (f : ℕ → ℕ), Tendsto f atTop atTop ∧ ∀ j, A (f j) = c := by
  rw [tendsto_atTop] at hA
  push Not at hA
  obtain ⟨B, hB⟩ := hA
  have H : ∃ᶠ j in atTop, ∃ c ∈ Finset.range B, A j = c :=
    hB.mono fun j hj ↦ ⟨A j, Finset.mem_range.mpr hj, rfl⟩
  obtain ⟨c, -, hc⟩ := (Finset.frequently_exists _).mp H
  obtain ⟨f, hf, hcf⟩ := exists_seq_forall_of_frequently hc
  exact ⟨c, f, hf, hcf⟩

private theorem fixed_design_face {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hc : ∀ t i, Compatible (K t) (N t) (k t) (x i))
    (hN : Tendsto N atTop atTop) {L : ℝ}
    (hL : Tendsto (fun j ↦ likelihood (K j) (N j) (k j) x) atTop (𝓝 L))
    (hk : ¬ Tendsto k atTop atTop) :
    ∃ d : ℕ, (∀ i, x i ≤ d) ∧ ∃ p ∈ Icc (0 : ℝ) 1, ∃ f : ℕ → ℕ,
      Tendsto f atTop atTop ∧ (∀ j, k (f j) = d) ∧
      Tendsto (fun j ↦ (K (f j) : ℝ) / N (f j)) atTop (𝓝 p) ∧
      L = ∏ i, binomialFace d (x i) p := by
  obtain ⟨d, g, hg, hdg⟩ := constant_subsequence_if_not_divergent k hk
  let i : Fin h := ⟨0, hh⟩
  have hd (j : Fin h) : x j ≤ d := hdg 0 ▸ (hc (g 0) j).2.2.2.1
  have hratio (t : ℕ) : (K (g t) : ℝ) / N (g t) ∈ Icc (0 : ℝ) 1 := by
    refine ⟨by positivity, ?_⟩
    by_cases hn : N (g t) = 0
    · simp [hn]
    · exact (div_le_one (Nat.cast_pos.mpr (Nat.pos_of_ne_zero hn))).mpr
        (by exact_mod_cast (hc (g t) i).1)
  obtain ⟨p, hp, a, ha, hpa⟩ := isCompact_Icc.tendsto_subseq hratio
  let f := g ∘ a
  have hf : Tendsto f atTop atTop := hg.comp ha.tendsto_atTop
  have hdf (j : ℕ) : k (f j) = d := hdg (a j)
  have hlim := held_draw_binomial_limit x d hp (hN.comp hf)
    (Eventually.of_forall fun j ↦ (hc (f j) i).1) hpa
  have H : Tendsto (fun j ↦ likelihood (K (f j)) (N (f j)) d x) atTop (𝓝 L) :=
    (hL.comp hf).congr fun j ↦ by simp only [Function.comp_apply, hdf]
  exact ⟨d, hd, p, hp, f, hf, hdf, hpa, tendsto_nhds_unique H hlim⟩

private theorem binomial_constant_escape {h d c : ℕ} (hh : 0 < h) (hc : c ≤ d)
    {p : ℝ} (hp : p ∈ Icc 0 1) (H : (∏ _ : Fin h, binomialFace d c p) = 1) :
    d = 0 ∨ (p = 0 ∧ c = 0) ∨ (p = 1 ∧ c = d) := by
  by_cases hd : d = 0
  · exact Or.inl hd
  by_cases p₀ : p = 0
  · refine Or.inr (Or.inl ⟨p₀, ?_⟩)
    by_contra he
    simp [binomialFace, p₀, he, hh.ne'] at H
  by_cases p₁ : p = 1
  · refine Or.inr (Or.inr ⟨p₁, ?_⟩)
    by_contra he
    have hcd : d - c ≠ 0 := by omega
    simp [binomialFace, p₁, hcd, hh.ne'] at H
  have hp₀ : 0 < p := hp.1.lt_of_ne (Ne.symm p₀)
  have hp₁ : 0 < 1 - p := sub_pos.mpr (hp.2.lt_of_ne p₁)
  let R := Finset.range (d + 1)
  let f := fun i ↦ binomialFace d i p
  have hS : ∑ i ∈ R, f i = 1 := by
    have H := add_pow p (1 - p) d
    rw [show p + (1 - p) = 1 by ring, one_pow] at H
    simpa [R, f, binomialFace, mul_comm, mul_left_comm, mul_assoc] using H.symm
  have hR : c ∈ R := Finset.mem_range.mpr (by omega)
  let j : ℕ := if c = 0 then 1 else 0
  have hj : j ≠ c := by dsimp [j]; split <;> omega
  have hjd : j ≤ d := by dsimp [j]; split <;> omega
  have hjR : j ∈ R := Finset.mem_range.mpr (by omega)
  have hjpos : 0 < f j :=
    mul_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos hjd)) (pow_pos hp₀ _)) (pow_pos hp₁ _)
  have hs := Finset.sum_le_sum_of_subset_of_nonneg (f := f)
    (show {j, c} ⊆ R from Finset.insert_subset hjR (Finset.singleton_subset_iff.mpr hR))
    (fun i _ _ ↦ by dsimp [f, binomialFace]; positivity)
  rw [Finset.sum_insert (by simpa using hj), Finset.sum_singleton, hS] at hs
  have hlt : f c < 1 := by linarith
  have hpow := pow_lt_one₀ (show 0 ≤ f c by dsimp [f, binomialFace]; positivity) hlt hh.ne'
  simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin] at H
  exact ((ne_of_lt hpow) H).elim

private theorem poisson_constant_escape {h c : ℕ} (hh : 0 < h) {r : ℝ} (hr : 0 ≤ r)
    (H : (∏ _ : Fin h, r ^ c / c.factorial * exp (-r)) = 1) : r = 0 ∧ c = 0 := by
  by_cases he : r = 0
  · refine ⟨he, ?_⟩
    by_contra hz
    simp [he, hz, hh.ne'] at H
  have hpos : 0 < r := hr.lt_of_ne (Ne.symm he)
  let f (j : ℕ) := exp (-r) * r ^ j / (j.factorial : ℝ)
  have hsum : HasSum f 1 := ProbabilityTheory.hasSum_one_poissonMeasure ⟨r, hr⟩
  have hs := hsum.summable.sum_le_tsum ({c, c + 1} : Finset ℕ)
    (fun i _ ↦ show 0 ≤ f i by dsimp [f]; positivity)
  have hj : c ∉ ({c + 1} : Finset ℕ) := by simp
  rw [hsum.tsum_eq, Finset.sum_insert hj, Finset.sum_singleton] at hs
  have hp : 0 < f (c + 1) := by dsimp [f]; positivity
  have hlt : f c < 1 := by linarith
  have hpow := pow_lt_one₀ (show 0 ≤ f c by dsimp [f]; positivity) hlt hh.ne'
  simp only [Finset.prod_const, Finset.card_univ, Fintype.card_fin] at H
  have hf : f c = r ^ c / c.factorial * exp (-r) := by dsimp [f]; ring
  rw [hf, H] at hpow
  exact (lt_irrefl _ hpow).elim

/-- Every positive likelihood limit along diverging compatible populations
has a subsequence of one of the two binomial orientations or of jointly
growing designs with a finite Poisson rate. On samples with positive counts
the Poisson rate is strictly positive. At the saturated constant height
the faces are deterministic, including the rate-zero Poisson at zero. -/
theorem positive_population_escape {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (K N k : ℕ → ℕ) (hc : ∀ t i, Compatible (K t) (N t) (k t) (x i))
    (hN : Tendsto N atTop atTop) {L : ℝ} (hL : 0 < L)
    (hl : Tendsto (fun t ↦ likelihood (K t) (N t) (k t) x) atTop (𝓝 L)) :
    (∃ d : ℕ, (∀ i, x i ≤ d) ∧ ∃ p ∈ Icc (0 : ℝ) 1, ∃ f : ℕ → ℕ,
      Tendsto f atTop atTop ∧
      (((∀ j, k (f j) = d) ∧ Tendsto (fun j ↦ (K (f j) : ℝ) / N (f j)) atTop (𝓝 p)) ∨
       ((∀ j, K (f j) = d) ∧ Tendsto (fun j ↦ (k (f j) : ℝ) / N (f j)) atTop (𝓝 p))) ∧
      L = (∏ i, binomialFace d (x i) p) ∧
      (∀ c, (∀ i, x i = c) → L = 1 →
        d = 0 ∨ (p = 0 ∧ c = 0) ∨ (p = 1 ∧ c = d))) ∨
    (∃ r ≥ (0 : ℝ), ∃ f : ℕ → ℕ, Tendsto f atTop atTop ∧
      Tendsto (K ∘ f) atTop atTop ∧ Tendsto (k ∘ f) atTop atTop ∧
      Tendsto (fun j ↦ (K (f j) : ℝ) * k (f j) / N (f j)) atTop (𝓝 r) ∧
      ((0 < ∑ i, x i) → 0 < r) ∧
      L = (∏ i, r ^ x i / (x i).factorial * exp (-r)) ∧
      (∀ c, (∀ i, x i = c) → L = 1 → r = 0 ∧ c = 0)) := by
  by_cases hk : Tendsto k atTop atTop
  · by_cases hK : Tendsto K atTop atTop
    · let rate := fun j ↦ (K j : ℝ) * k j / N j
      let q := fun j ↦ rate j / (1 + rate j)
      have hnon (j : ℕ) : 0 ≤ rate j := by dsimp [rate]; positivity
      have hq (j : ℕ) : q j ∈ Icc (0 : ℝ) 1 := by
        have h := hnon j
        exact ⟨div_nonneg h (by positivity), (div_le_one (by positivity)).mpr (by linarith)⟩
      obtain ⟨p, hp, f, hf, hqf⟩ := isCompact_Icc.tendsto_subseq hq
      have hff := hf.tendsto_atTop
      have hkf := hk.comp hff
      have hKf := hK.comp hff
      have hcf : ∀ᶠ j in atTop, ∀ i, Compatible (K (f j)) (N (f j)) (k (f j)) (x i) :=
        Eventually.of_forall fun j ↦ hc (f j)
      have hcases := joint_rate_extremes hh x (K ∘ f) (N ∘ f) (k ∘ f) hKf hkf hcf
      have hpe : p ≠ 1 := by
        intro e
        subst p
        have hr : Tendsto (rate ∘ f) atTop atTop := by
          apply tendsto_atTop.mpr
          intro b
          by_cases hb : 0 ≤ b
          · have hb' : b / (1 + b) < 1 := (div_lt_one (by positivity)).mpr (by linarith)
            filter_upwards [hqf.eventually (Ioi_mem_nhds hb')] with j hj
            change b / (1 + b) < rate (f j) / (1 + rate (f j)) at hj
            have H := (div_lt_div_iff₀ (by positivity : 0 < 1 + b)
              (show 0 < 1 + rate (f j) by linarith [hnon (f j)])).mp hj
            change b ≤ rate (f j)
            nlinarith only [H]
          · exact Eventually.of_forall fun j ↦ (le_of_not_ge hb).trans (hnon (f j))
        exact hL.ne' (tendsto_nhds_unique (hl.comp hff) (hcases.2 hr))
      let r := p / (1 - p)
      have hp' : 0 < 1 - p := sub_pos.mpr (hp.2.lt_of_ne hpe)
      have hr : Tendsto (fun j ↦ rate (f j)) atTop (𝓝 r) := by
        have H := hqf.div (tendsto_const_nhds.sub hqf) hp'.ne'
        apply H.congr
        intro j
        change q (f j) / (1 - q (f j)) = rate (f j)
        dsimp [q]
        have H₀ : 0 < 1 + rate (f j) := by linarith [hnon (f j)]
        have E : 1 - rate (f j) / (1 + rate (f j)) = 1 / (1 + rate (f j)) := by
          field_simp [H₀.ne']
          ring
        rw [E]
        field_simp [H₀.ne']
      have hr₀ : 0 ≤ r := div_nonneg hp.1 hp'.le
      have hpos (hx : 0 < ∑ i, x i) : 0 < r := by
        apply hr₀.lt_of_ne
        intro e
        have hz : Tendsto (fun j ↦ rate (f j)) atTop (𝓝 0) := e ▸ hr
        exact hL.ne' (tendsto_nhds_unique (hl.comp hff) (hcases.1 hx hz))
      have H := joint_poisson_limit x (K ∘ f) (N ∘ f) (k ∘ f) hKf hkf hcf hr
      have he := tendsto_nhds_unique (hl.comp hff) H
      refine Or.inr ⟨r, hr₀, f, hff, hKf, hkf, hr, hpos, he, fun c hcx hL1 ↦ ?_⟩
      exact poisson_constant_escape hh hr₀ (by simpa only [hcx] using he.symm.trans hL1)
    · have hs (t : ℕ) (i : Fin h) : Compatible (k t) (N t) (K t) (x i) := by
        have H := hc t i
        unfold Compatible at H ⊢
        omega
      have hswap : Tendsto (fun j ↦ likelihood (k j) (N j) (K j) x) atTop (𝓝 L) :=
        hl.congr fun j ↦ likelihood_swap _ _ _ _
      obtain ⟨d, hd, p, hp, f, hf, hdf, hprop, H⟩ :=
        fixed_design_face hh x k N K hs hN hswap hK
      refine Or.inl ⟨d, hd, p, hp, f, hf, Or.inr ⟨hdf, hprop⟩, H, fun c hcx hL1 ↦ ?_⟩
      exact binomial_constant_escape hh (hcx ⟨0, hh⟩ ▸ hd ⟨0, hh⟩) hp
        (by simpa only [hcx] using H.symm.trans hL1)
  · obtain ⟨d, hd, p, hp, f, hf, hdf, hprop, H⟩ := fixed_design_face hh x K N k hc hN hl hk
    refine Or.inl ⟨d, hd, p, hp, f, hf, Or.inl ⟨hdf, hprop⟩, H, fun c hcx hL1 ↦ ?_⟩
    exact binomial_constant_escape hh (hcx ⟨0, hh⟩ ▸ hd ⟨0, hh⟩) hp
      (by simpa only [hcx] using H.symm.trans hL1)

/-- Underdispersion supplies a highest binomial face strictly above
Poisson, and that height excludes all urns with both design counts
sufficiently large. This is the underdispersed corner cutoff. -/
theorem underdispersion_corner_cutoff {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hX : 0 < ∑ i, x i) (hv : BinomialSize.variance x < BinomialSize.mean x) :
    let μ := BinomialSize.mean x
    ∃ d ∈ BinomialSize.feasible x, ∃ S : ℕ,
      IsMaxOn (BinomialSize.profile x) (BinomialSize.feasible x) d ∧
      (∏ i, μ ^ x i / (x i).factorial * exp (-μ)) <
        (∏ i, binomialFace d (x i) (μ / d)) ∧
      ∀ K N k : ℕ, S ≤ K → S ≤ k → (∀ i, Compatible K N k (x i)) →
        likelihood K N k x < ∏ i, binomialFace d (x i) (μ / d) := by
  intro μ
  have hμ : 0 < μ := by
    unfold μ BinomialSize.mean
    exact div_pos (by exact_mod_cast hX) (by exact_mod_cast hh)
  obtain ⟨d, hd, hm⟩ := (BinomialSize.finite_profile_maximizers_of_underdispersion x hh hv).1
  let M := Finset.univ.sup x
  have hfeas : ∀ᶠ N : ℕ in atTop, N ∈ BinomialSize.feasible x :=
    (eventually_ge_atTop M).mono fun _ hN i ↦ (Finset.le_sup (Finset.mem_univ i)).trans hN
  obtain ⟨N, hN, hstrict⟩ :=
    (hfeas.and (BinomialSize.eventually_poisson_lt_profile x hh hv)).exists
  have hlt : BinomialSize.poissonLogLik x < BinomialSize.profile x d := hstrict.trans_le (hm hN)
  have hface : exp (BinomialSize.profile x d) = ∏ i, binomialFace d (x i) (μ / d) :=
    BinomialSize.exp_profile_eq_sampleLikelihood x hh hμ hd
  have he : exp (BinomialSize.poissonLogLik x) = ∏ i, μ ^ x i / (x i).factorial * exp (-μ) := by
    unfold BinomialSize.poissonLogLik
    rw [exp_sum]
    apply Finset.prod_congr rfl
    intro i _
    change exp ((x i : ℝ) * log μ - log ((x i).factorial : ℝ) - μ) = _
    rw [exp_sub, exp_sub, exp_nat_mul, exp_log hμ,
      exp_log (by positivity : 0 < ((x i).factorial : ℝ)), exp_neg, div_eq_mul_inv]
  have H := exp_lt_exp.mpr hlt
  rw [he, hface] at H
  obtain ⟨S, hS⟩ := large_design_cutoff hh x hX H
  exact ⟨d, hd, S, hm, H, hS⟩

end UnivariateML.Base7bHyper

end
