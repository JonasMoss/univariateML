module

public import UnivariateML.Family.Base7bNHyper
public import Mathlib.Algebra.Polynomial.RingDivision
import UnivariateML.Family.Discrete
import Mathlib.Topology.Instances.Nat
import Mathlib.Data.Rat.BigOperators
import Mathlib.Analysis.Polynomial.CauchyBound
import Mathlib.Analysis.Polynomial.Order

set_option linter.style.header false

/-! Exact fixed-stop tails and finite slice comparison for the negative
hypergeometric likelihood.

## Deviations
Elementary log bounds give a wider offset band than the two sharp
offsets from the adjacent-ratio expansion. The tail table compares all
compatible rays in that band. Factoring away positive scalars makes the
face polynomials monic products and exposes the obstruction to identical
equality.
-/

@[expose] public section

namespace UnivariateML.Base7bNHyperResidue

open Filter Set Polynomial
open scoped Topology

private theorem prod_multiplicity {ι : Type*} (s : Finset ι) (f : ι → ℚ[X])
    (hf : ∀ i ∈ s, f i ≠ 0) (x : ℚ) :
    rootMultiplicity x (∏ i ∈ s, f i) = ∑ i ∈ s, rootMultiplicity x (f i) := by
  classical
  induction s using Finset.induction_on with
  | empty => simp
  | insert i s hi ih =>
      have hs : ∀ j ∈ s, f j ≠ 0 := fun j hj ↦ hf j (Finset.mem_insert_of_mem hj)
      rw [Finset.prod_insert hi, Finset.sum_insert hi,
        rootMultiplicity_mul (mul_ne_zero (hf i (Finset.mem_insert_self _ _))
          (Finset.prod_ne_zero_iff.mpr hs)), ih hs]

private theorem multiplicity_scaled_falling (d : ℕ) {a : ℚ} (ha : a ≠ 0) :
    rootMultiplicity 0 (∏ j ∈ Finset.range d, (X - C ((j : ℚ) / a))) =
      if d = 0 then 0 else 1 := by
  rw [prod_multiplicity _ _ (fun j _ ↦ X_sub_C_ne_zero _) 0]
  simp only [rootMultiplicity_X_sub_C]
  rcases d with _ | d
  · simp
  · rw [Finset.sum_eq_single 0]
    · simp
    · intro b hb hne
      have H : (0 : ℚ) ≠ (b : ℚ) / a := by
        intro e
        have hcast := (div_eq_zero_iff.mp e.symm).resolve_right ha
        exact hne (Nat.cast_eq_zero.mp hcast)
      simp [H]
    · simp

/-- On every affine composition ray with positive slopes, the fixed-stop
face comparison has a nonzero polynomial whenever some draw count exceeds
the positive stopping count. The displayed products are monic, so failure
of proportionality excludes equality after any nonzero normalization. -/
theorem affine_face_polynomial_nonzero {h r : ℕ} (hr : 0 < r)
    (y : Fin h → ℕ) (hy : ∃ i, 0 < y i) {a b : ℚ} (ha : 0 < a) (hb : 0 < b)
    (c q : ℚ) (hq : q ≠ 0) :
    let U := ∏ i : Fin h,
      ((∏ j ∈ Finset.range r, (X - C (((j : ℚ) - c) / a))) *
       (∏ j ∈ Finset.range (y i), (X - C (((j : ℚ) + c) / b))))
    let D := ∏ i : Fin h, ∏ j ∈ Finset.range (r + y i), (X - C (j : ℚ))
    U - C q * D ≠ 0 := by
  intro U D he
  have heq := sub_eq_zero.mp he
  obtain ⟨i, hi⟩ := hy
  have hD {t : ℚ} (ht : t < 0) : D.eval t ≠ 0 := by
    unfold D
    rw [eval_prod]
    apply Finset.prod_ne_zero_iff.mpr
    intro k _
    rw [eval_prod]
    apply Finset.prod_ne_zero_iff.mpr
    intro j _
    have hj : 0 ≤ (j : ℚ) := by positivity
    simp [sub_ne_zero.mpr (ht.trans_le hj).ne]
  have hexclude {t : ℚ} (ht : t < 0) (hz : U.eval t = 0) : False := by
    rw [heq, eval_mul, eval_C] at hz
    exact (mul_ne_zero hq (hD ht)) hz
  rcases lt_trichotomy c 0 with hc | rfl | hc
  · let t := c / b
    refine hexclude (div_neg_of_neg_of_pos hc hb) ?_
    unfold U
    rw [eval_prod]
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    rw [eval_mul]
    apply mul_eq_zero.mpr
    right
    rw [eval_prod]
    apply Finset.prod_eq_zero (Finset.mem_range.mpr hi)
    simp
  · have hm (i : Fin h) :
        rootMultiplicity 0
          ((∏ j ∈ Finset.range r, (X - C (((j : ℚ) - 0) / a))) *
           (∏ j ∈ Finset.range (y i), (X - C (((j : ℚ) + 0) / b)))) =
          1 + (if y i = 0 then 0 else 1) := by
      rw [rootMultiplicity_mul (mul_ne_zero
        (Finset.prod_ne_zero_iff.mpr (fun _ _ ↦ X_sub_C_ne_zero _))
        (Finset.prod_ne_zero_iff.mpr (fun _ _ ↦ X_sub_C_ne_zero _)))]
      simp only [sub_zero, add_zero, multiplicity_scaled_falling _ ha.ne',
        multiplicity_scaled_falling _ hb.ne', hr.ne', ↓reduceIte]
    have hUm : rootMultiplicity 0 U = ∑ i : Fin h, (1 + (if y i = 0 then 0 else 1)) := by
      unfold U
      rw [prod_multiplicity _ _ (fun _ _ ↦ mul_ne_zero
        (Finset.prod_ne_zero_iff.mpr (fun _ _ ↦ X_sub_C_ne_zero _))
        (Finset.prod_ne_zero_iff.mpr (fun _ _ ↦ X_sub_C_ne_zero _))) 0]
      simp only [hm]
    have hDm : rootMultiplicity 0 D = h := by
      unfold D
      rw [prod_multiplicity _ _ (fun _ _ ↦
        Finset.prod_ne_zero_iff.mpr (fun _ _ ↦ X_sub_C_ne_zero _)) 0]
      have H (k : Fin h) :
          rootMultiplicity 0 (∏ j ∈ Finset.range (r + y k), (X - C (j : ℚ))) = 1 := by
        simpa [hr.ne'] using
          multiplicity_scaled_falling (r + y k) (a := 1) one_ne_zero
      simp only [H, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one]
      rfl
    have H := Finset.sum_lt_sum (s := Finset.univ) (f := fun _ : Fin h ↦ (1 : ℕ))
      (g := fun i ↦ 1 + (if y i = 0 then 0 else 1))
      (fun i _ ↦ by omega) ⟨i, Finset.mem_univ _, by simp [hi.ne']⟩
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one] at H
    rw [heq, rootMultiplicity_C_mul hq, hDm] at hUm
    exact H.ne hUm
  · let t := -c / a
    refine hexclude (div_neg_of_neg_of_pos (neg_neg_of_pos hc) ha) ?_
    unfold U
    rw [eval_prod]
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    rw [eval_mul]
    apply mul_eq_zero.mpr
    left
    rw [eval_prod]
    apply Finset.prod_eq_zero (Finset.mem_range.mpr hr)
    simp

private theorem monic_falling_eval {N m d : ℕ} {a c : ℚ}
    (ha : a ≠ 0) (hm : (m : ℚ) = a * N + c) (hd : d ≤ m) :
    (∏ j ∈ Finset.range d, (X - C (((j : ℚ) - c) / a))).eval (N : ℚ) =
      (m.descFactorial d : ℚ) / a ^ d := by
  rw [eval_prod, Nat.descFactorial_eq_prod_range, Nat.cast_prod,
    show a ^ d = ∏ _ ∈ Finset.range d, a by simp, ← Finset.prod_div_distrib]
  apply Finset.prod_congr rfl
  intro j hj
  have hj' : j ≤ m := (Finset.mem_range.mp hj).le.trans hd
  rw [eval_sub, eval_X, eval_C, Nat.cast_sub hj']
  field_simp [ha]
  linarith only [hm]

/-- On a specified affine fixed-stop ray, a nonzero rational polynomial
gives the exact sign of the likelihood difference from the corresponding
shifted negative-binomial face. The sample has a positive stopping count
and at least one observation strictly above it. -/
theorem affine_face_comparison {h r : ℕ} (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) {a b : ℚ} (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1)
    (c : ℚ) :
    let F : ℚ := ∏ i, ((r + y i - 1).choose (r - 1) : ℚ) * a ^ r * b ^ y i
    ∃ P : ℚ[X], P ≠ 0 ∧ ∀ N m : ℕ, (m : ℚ) = a * N + c →
      (∀ i, Discrete4NHyper.Compatible N m r (r + y i)) →
      let L := Discrete4NHyper.totalLikelihood (fun i ↦ r + y i) N m r
      (0 < P.eval (N : ℚ) ↔ (F : ℝ) < L) ∧
      (P.eval (N : ℚ) = 0 ↔ L = (F : ℝ)) ∧
      (P.eval (N : ℚ) < 0 ↔ L < (F : ℝ)) := by
  intro F
  let A := ∏ i : Fin h,
    ((∏ j ∈ Finset.range r, (X - C (((j : ℚ) - c) / a))) *
     (∏ j ∈ Finset.range (y i), (X - C (((j : ℚ) + c) / b))))
  let D := ∏ i : Fin h, ∏ j ∈ Finset.range (r + y i), (X - C (j : ℚ))
  refine ⟨A - D, ?_, ?_⟩
  · simpa only [C_1, one_mul] using
      affine_face_polynomial_nonzero hr y hy ha hb c 1 one_ne_zero
  · intro N m hm hc L
    obtain ⟨i₀, -⟩ := hy
    have hmN : m ≤ N := (hc i₀).1
    have hrm : r ≤ m := (hc i₀).2.1
    have hyN (i : Fin h) : y i ≤ N - m := by
      have H := (hc i).2.2
      rw [ite_eq_right hr.ne'] at H
      simpa using H.2
    have hcN (i : Fin h) : r + y i ≤ N := by
      have := hyN i
      omega
    have hNm : ((N - m : ℕ) : ℚ) = b * N + -c := by
      rw [Nat.cast_sub hmN, hm]
      linear_combination -(N : ℚ) * hab
    have heA : A.eval (N : ℚ) = ∏ i : Fin h,
        ((m.descFactorial r : ℚ) / a ^ r) * (((N - m).descFactorial (y i) : ℚ) / b ^ y i) := by
      unfold A
      rw [eval_prod]
      apply Finset.prod_congr rfl
      intro i _
      rw [eval_mul, monic_falling_eval ha.ne' hm hrm]
      simpa only [sub_neg_eq_add] using
        congrArg (fun u : ℚ ↦ ((m.descFactorial r : ℚ) / a ^ r) * u)
          (monic_falling_eval hb.ne' hNm (hyN i))
    have heD : D.eval (N : ℚ) = ∏ i, (N.descFactorial (r + y i) : ℚ) := by
      unfold D
      rw [eval_prod]
      apply Finset.prod_congr rfl
      intro i _
      simpa only [sub_zero, div_one, one_pow, one_mul, add_zero] using
        monic_falling_eval one_ne_zero
          (show (N : ℚ) = (1 : ℚ) * N + 0 by simp) (hcN i)
    have hDpos : 0 < ((D.eval (N : ℚ) : ℚ) : ℝ) := by
      rw [heD, Rat.cast_prod]
      exact Finset.prod_pos fun i _ ↦ by
        rw [Rat.cast_natCast]
        exact Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (hcN i))
    have hFpos : 0 < (F : ℝ) := by
      have ha' : 0 < (a : ℝ) := by exact_mod_cast ha
      have hb' : 0 < (b : ℝ) := by exact_mod_cast hb
      unfold F
      rw [Rat.cast_prod]
      exact Finset.prod_pos fun i _ ↦ by
        simp only [Rat.cast_mul, Rat.cast_natCast, Rat.cast_pow]
        exact mul_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos (by omega))) (pow_pos ha' _))
          (pow_pos hb' _)
    have hL : L = ∏ i, ((r + y i - 1).choose (r - 1) : ℝ) *
        ((m.descFactorial r : ℝ) * ((N - m).descFactorial (y i) : ℝ) /
          (N.descFactorial (r + y i) : ℝ)) := by
      unfold L Discrete4NHyper.totalLikelihood
      apply Finset.prod_congr rfl
      intro i _
      rw [Discrete4NHyper.totalMass, ite_eq_left (hc i), Nat.add_sub_cancel_left,
        NegHypergeometric.mass, NegHypergeometric.reducedMass, Nat.add_sub_of_le hmN]
    have he : ((A.eval (N : ℚ) : ℚ) : ℝ) * (F : ℝ) = L * ((D.eval (N : ℚ) : ℚ) : ℝ) := by
      rw [heA, heD, hL]
      simp only [F, Rat.cast_prod, Rat.cast_mul, Rat.cast_div, Rat.cast_pow, Rat.cast_natCast,
        ← Finset.prod_mul_distrib]
      apply Finset.prod_congr rfl
      intro i _
      have ha' : (a : ℝ) ≠ 0 := by exact_mod_cast ha.ne'
      have hb' : (b : ℝ) ≠ 0 := by exact_mod_cast hb.ne'
      have hn' : (N.descFactorial (r + y i) : ℝ) ≠ 0 :=
        (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr (hcN i))).ne'
      field_simp [ha', hb', hn']
    have hP : (((A - D).eval (N : ℚ) : ℚ) : ℝ) * (F : ℝ) =
        (L - (F : ℝ)) * ((D.eval (N : ℚ) : ℚ) : ℝ) := by
      rw [eval_sub, Rat.cast_sub, sub_mul, he]
      ring
    refine ⟨?_, ?_, ?_⟩
    · rw [← Rat.cast_pos (K := ℝ), ← mul_pos_iff_of_pos_right hFpos, hP,
        mul_pos_iff_of_pos_right hDpos, sub_pos]
    · rw [← Rat.cast_eq_zero (α := ℝ), ← mul_eq_zero_iff_right hFpos.ne', hP,
        mul_eq_zero, or_iff_left hDpos.ne', sub_eq_zero]
    · have H {v z : ℝ} (hz : 0 < z) : v * z < 0 ↔ v < 0 := by
        simpa only [zero_mul] using (mul_lt_mul_iff_left₀ hz : v * z < 0 * z ↔ v < 0)
      rw [← Rat.cast_lt_zero (K := ℝ), ← H hFpos, hP, H hDpos, sub_neg]

private theorem log_one_sub_bounds {z : ℝ} (h : z < 1) :
    -(z / (1 - z)) ≤ Real.log (1 - z) ∧ Real.log (1 - z) ≤ -z := by
  have hp := sub_pos.mpr h
  have H := Real.log_le_sub_one_of_pos (inv_pos.mpr hp)
  rw [Real.log_inv] at H
  have he : (1 - z)⁻¹ - 1 = z / (1 - z) := by field_simp [hp.ne']; ring
  rw [he] at H
  exact ⟨by linarith, (Real.log_le_sub_one_of_pos hp).trans_eq (by ring)⟩

private theorem stop_ratio_log_bounds {h N r m Y : ℕ} (y : Fin h → ℕ)
    (hy : ∀ i, y i ≤ Y) (hrm : r ≤ m) (hmN : m + Y < N) :
    let R := Discrete4NHyper.compositionRatio (fun i ↦ r + y i) N r m
    let S : ℝ := (h : ℝ) * r
    let T : ℝ := ∑ i, (y i : ℝ)
    0 < R ∧
    S / ((m : ℝ) + 1) - T / ((N : ℝ) - m - Y) ≤ Real.log R ∧
    Real.log R ≤ S / ((m : ℝ) + 1 - r) - T / ((N : ℝ) - m) := by
  intro R S T
  let A := (m : ℝ) + 1
  let B := (N : ℝ) - m
  have hAr : 0 < A - r := by
    have H : (r : ℝ) ≤ m := by exact_mod_cast hrm
    dsimp [A]; linarith
  have hA : 0 < A := by dsimp [A]; positivity
  have hBY : 0 < B - Y := by
    have H : (m : ℝ) + Y < N := by exact_mod_cast hmN
    dsimp [B]; linarith
  have hB : 0 < B := by linarith [Nat.cast_nonneg (α := ℝ) Y]
  let p : ℝ := (r : ℝ) / A
  let q (i : Fin h) : ℝ := (y i : ℝ) / B
  have hp : p < 1 := (div_lt_one hA).mpr (by linarith)
  have hq (i : Fin h) : q i < 1 := by
    have H : (y i : ℝ) ≤ Y := by exact_mod_cast hy i
    exact (div_lt_one hB).mpr (by linarith)
  have he (i : Fin h) :
      A / (A - r) * ((B - (y i : ℝ)) / B) = (1 - q i) / (1 - p) := by
    dsimp [p, q]
    field_simp [hAr.ne', hA.ne', hB.ne', (sub_pos.mpr hp).ne']
  have hR : R = ∏ i, (1 - q i) / (1 - p) := by
    unfold R Discrete4NHyper.compositionRatio
    simp only [Nat.add_sub_cancel_left]
    exact Finset.prod_congr rfl fun i _ ↦ he i
  have hf (i : Fin h) : 0 < (1 - q i) / (1 - p) :=
    div_pos (sub_pos.mpr (hq i)) (sub_pos.mpr hp)
  rw [hR]
  refine ⟨Finset.prod_pos (fun i _ ↦ hf i), ?_⟩
  rw [Real.log_prod (fun i _ ↦ (hf i).ne')]
  have hlog (i : Fin h) : Real.log ((1 - q i) / (1 - p)) =
      Real.log (1 - q i) - Real.log (1 - p) :=
    Real.log_div (sub_pos.mpr (hq i)).ne' (sub_pos.mpr hp).ne'
  simp only [hlog]
  have hpn : p / (1 - p) = (r : ℝ) / (A - r) := by
    dsimp [p]
    field_simp [hA.ne', hAr.ne', (sub_pos.mpr hp).ne']
  have hqn (i : Fin h) : q i / (1 - q i) = (y i : ℝ) / (B - y i) := by
    have H : 0 < B - y i := by
      have hi : (y i : ℝ) ≤ Y := by exact_mod_cast hy i
      linarith
    dsimp [q]
    field_simp [hB.ne', H.ne', (sub_pos.mpr (hq i)).ne']
  have hl (i : Fin h) : (r : ℝ) / A - (y i : ℝ) / (B - Y) ≤
      Real.log (1 - q i) - Real.log (1 - p) := by
    have H := (log_one_sub_bounds hp).2
    have Q := (log_one_sub_bounds (hq i)).1
    rw [hqn i] at Q
    have hdiv := div_le_div_of_nonneg_left (Nat.cast_nonneg (α := ℝ) (y i)) hBY
      (show B - Y ≤ B - y i by
        have hi : (y i : ℝ) ≤ Y := by exact_mod_cast hy i
        linarith)
    change Real.log (1 - p) ≤ -((r : ℝ) / A) at H
    linarith only [H, Q, hdiv]
  have hu (i : Fin h) : Real.log (1 - q i) - Real.log (1 - p) ≤
      (r : ℝ) / (A - r) - (y i : ℝ) / B := by
    have H := (log_one_sub_bounds hp).1
    have Q := (log_one_sub_bounds (hq i)).2
    rw [hpn] at H
    change Real.log (1 - q i) ≤ -((y i : ℝ) / B) at Q
    linarith only [H, Q]
  have hsum (a b : ℝ) :
      (∑ i : Fin h, ((r : ℝ) / a - (y i : ℝ) / b)) = S / a - T / b := by
    simp only [S, T, Finset.sum_sub_distrib, div_eq_mul_inv, ← Finset.sum_mul,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  exact ⟨(hsum A (B - Y)) ▸ Finset.sum_le_sum (fun i _ ↦ hl i),
    (hsum (A - r) B) ▸ Finset.sum_le_sum (fun i _ ↦ hu i)⟩

/-- The fixed-stop composition ratio has strict outer signs around the
fitted marked proportion. This result uses exact log bounds on the
finite product and works for arbitrary excess-draw multiplicities. -/
theorem held_stop_ratio_brackets {h N r m Y : ℕ} (y : Fin h → ℕ)
    (hy : ∀ i, y i ≤ Y) (hrm : r ≤ m) (hmN : m + Y < N)
    (hS : 0 < (h : ℝ) * r) (hT : 0 < ∑ i, (y i : ℝ)) :
    let p := ((h : ℝ) * r) / ((h : ℝ) * r + ∑ i, (y i : ℝ))
    let R := Discrete4NHyper.compositionRatio (fun i ↦ r + y i) N r m
    ((m : ℝ) + 1 + Y < p * N → 1 < R) ∧ (p * N + r < (m : ℝ) → R < 1) := by
  intro p R
  let S := (h : ℝ) * r
  let T : ℝ := ∑ i, (y i : ℝ)
  have hp : 0 < p := div_pos hS (add_pos hS hT)
  have hp1 : p < 1 := (div_lt_one (add_pos hS hT)).mpr (by linarith)
  have hST : p * (S + T) = S := div_mul_cancel₀ _ (add_pos hS hT).ne'
  have hT' : (1 - p) * (S + T) = T := by linarith [hST]
  have hmr : (r : ℝ) ≤ m := by exact_mod_cast hrm
  have hYN : (m : ℝ) + Y < N := by exact_mod_cast hmN
  obtain ⟨hR, hl, hu⟩ := stop_ratio_log_bounds y hy hrm hmN
  constructor
  · intro H
    apply (Real.log_lt_log_iff zero_lt_one hR).mp
    rw [Real.log_one]
    have Ha : (m : ℝ) + 1 < p * ((N : ℝ) - Y + 1) := by
      have Hb : p * (Y : ℝ) ≤ Y := by
        nlinarith only [mul_nonneg (sub_nonneg.mpr hp1.le) (Nat.cast_nonneg (α := ℝ) Y)]
      nlinarith only [H, Hb, hp]
    have Hc := mul_lt_mul_of_pos_right Ha (add_pos hS hT)
    have Hdiv : T / ((N : ℝ) - m - Y) < S / ((m : ℝ) + 1) := by
      apply (div_lt_div_iff₀ (by linarith) (by positivity)).mpr
      nlinarith only [Hc, congrArg (fun z : ℝ ↦ z * ((N : ℝ) - Y + 1)) hST]
    linarith only [hl, Hdiv]
  · intro H
    apply (Real.log_lt_log_iff hR zero_lt_one).mp
    rw [Real.log_one]
    have Ha : p * N + (1 - p) * ((r : ℝ) - 1) < m := by
      have Hb : (1 - p) * ((r : ℝ) - 1) ≤ r := by
        nlinarith only [mul_nonneg hp.le (Nat.cast_nonneg (α := ℝ) r), hp1]
      linarith
    have Hc := mul_lt_mul_of_pos_right Ha (add_pos hS hT)
    have Hdiv : S / ((m : ℝ) + 1 - r) < T / ((N : ℝ) - m) := by
      apply (div_lt_div_iff₀ (by linarith) (by linarith [Nat.cast_nonneg (α := ℝ) Y])).mpr
      nlinarith only [Hc, congrArg (fun z : ℝ ↦ z * (N : ℝ)) hST,
        congrArg (fun z : ℝ ↦ z * ((r : ℝ) - 1)) hT']
    linarith only [hu, Hdiv]

/-- At sufficiently large populations a finite integer-offset band contains
all compatible composition maximizers for a positive held stop and at
least one positive excess draw count. -/
theorem held_stop_population_band {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let Y := NegHypergeometric.maxBlackDraws y
    let Z := max Y r + 2
    let p := ((h : ℝ) * r) / ((h : ℝ) * r + ∑ i, (y i : ℝ))
    ∃ B : ℕ, ∀ N ≥ B,
      let D := {m : ℕ | ∀ i, Discrete4NHyper.Compatible N m r (x i)}
      ∀ m ∈ D, IsMaxOn (fun m ↦ Discrete4NHyper.totalLikelihood x N m r) D m →
        ⌊p * N⌋₊ - Z < m ∧ m ≤ ⌊p * N⌋₊ + Z + 1 := by
  intro x Y Z p
  have hYS (i : Fin h) : y i ≤ Y := Finset.le_sup (Finset.mem_univ i)
  have hS : 0 < (h : ℝ) * r := mul_pos (Nat.cast_pos.mpr hh) (Nat.cast_pos.mpr hr)
  have hT : 0 < ∑ i, (y i : ℝ) := by
    obtain ⟨i, hi⟩ := hy
    exact (Nat.cast_pos.mpr hi).trans_le
      (Finset.single_le_sum (fun j _ ↦ Nat.cast_nonneg (y j)) (Finset.mem_univ i))
  have hp : 0 < p := div_pos hS (add_pos hS hT)
  have hp1 : p < 1 := (div_lt_one (add_pos hS hT)).mpr (by linarith)
  have hP : Tendsto (fun N : ℕ ↦ p * N) atTop atTop :=
    tendsto_natCast_atTop_atTop.const_mul_atTop hp
  have hQ : Tendsto (fun N : ℕ ↦ (1 - p) * N) atTop atTop :=
    tendsto_natCast_atTop_atTop.const_mul_atTop (sub_pos.mpr hp1)
  obtain ⟨B, hB⟩ := eventually_atTop.mp
    ((hP.eventually_gt_atTop ((2 : ℝ) * Z + 2)).and (hQ.eventually_gt_atTop ((2 : ℝ) * Z + 2)))
  refine ⟨B, ?_⟩
  intro N hN D m hm hMax
  have ⟨hP', hQ'⟩ := hB N hN
  let A := ⌊p * N⌋₊
  let L := A - Z
  let U := A + Z
  have hA : (A : ℝ) ≤ p * N := Nat.floor_le (by positivity)
  have hpA : p * N < (A : ℝ) + 1 := Nat.lt_floor_add_one _
  have hAZ : 2 * Z ≤ A := by
    exact_mod_cast (show ((2 * Z : ℕ) : ℝ) ≤ A by push_cast; linarith)
  have hYr : Y + 2 ≤ Z := Nat.add_le_add_right (le_max_left _ _) _
  have hrr : r + 2 ≤ Z := Nat.add_le_add_right (le_max_right _ _) _
  have hLr : r ≤ L := by dsimp [L]; omega
  have hUr : r ≤ U := by dsimp [U]; omega
  have hUN : U + Y < N := by
    have hYr' : (Y : ℝ) + 2 ≤ Z := by exact_mod_cast hYr
    exact_mod_cast (show ((U + Y : ℕ) : ℝ) < N by
      simp only [U, Nat.cast_add]
      nlinarith only [hA, hQ', hYr'])
  have hLbr : (L : ℝ) + 1 + Y < p * N := by
    dsimp [L]
    rw [Nat.cast_sub (by omega : Z ≤ A)]
    have H : (Y : ℝ) + 2 ≤ Z := by exact_mod_cast hYr
    linarith
  have hUbr : p * N + r < (U : ℝ) := by
    simp only [U, Nat.cast_add]
    have H : (r : ℝ) + 2 ≤ Z := by exact_mod_cast hrr
    linarith
  have hdom (v : ℕ) :
      v ∈ D ↔ v ∈ Icc r (N - Y) := by
    constructor
    · intro hv
      have hmN := (hv ⟨0, hh⟩).1
      have hrm := (hv ⟨0, hh⟩).2.1
      have hY : Y ≤ N - v := Finset.sup_le fun i _ ↦ by
        have H := (hv i).2.2
        rw [ite_eq_right hr.ne'] at H
        simpa [x] using H.2
      exact ⟨hrm, by omega⟩
    · intro hv i
      have h₁ : r ≤ v := hv.1
      have h₂ : v ≤ N - Y := hv.2
      have hY := hYS i
      refine ⟨by omega, h₁, ?_⟩
      rw [ite_eq_right hr.ne']
      refine ⟨Nat.le_add_right _ _, ?_⟩
      simp only [x, Nat.add_sub_cancel_left]
      omega
  obtain ⟨b, hb, hout, hin, hfit⟩ :=
    Discrete4NHyper.fixed_population_stop_crossing hh x hr
      (fun i ↦ Nat.le_add_right _ _) ⟨m, hm⟩
  have he : (fun i ↦ x i - r) = y := by funext i; simp [x]
  rw [he] at hb hout hin hfit
  have hbL : L < b := by
    by_contra! H
    have hrb := hb.1
    have hbN : b + Y < N := by dsimp [L, U] at *; omega
    have H' : (b : ℝ) ≤ L := by exact_mod_cast H
    have hbr : (b : ℝ) + 1 + Y < p * N := by linarith
    have hl := (held_stop_ratio_brackets y hYS hrb hbN hS hT).1 hbr
    exact hl.not_ge (hout (by omega))
  have hbU : b ≤ U := by
    by_contra! H
    have hu := (held_stop_ratio_brackets y hYS hUr hUN hS hT).2 hUbr
    exact hu.not_ge (hin U ⟨hUr, by omega⟩ H).le
  have mmax : IsMaxOn (fun m ↦ Discrete4NHyper.totalLikelihood x N m r) (Icc r (N - Y)) m :=
    fun v hv ↦ hMax ((hdom v).mpr hv)
  rcases (hfit m ((hdom m).mp hm)).mp mmax with he | ⟨he, _, _⟩
  · subst m
    exact ⟨hbL, hbU.trans (Nat.le_succ _)⟩
  · exact ⟨by omega, by omega⟩

private theorem tail_sign (p : ℚ[X]) (hp : p ≠ 0) :
    ∃ B : ℕ, ∀ N ≥ B,
      (0 < p.eval (N : ℚ) ↔ 0 < p.leadingCoeff) ∧
      p.eval (N : ℚ) ≠ 0 ∧
      (p.eval (N : ℚ) < 0 ↔ p.leadingCoeff < 0) := by
  let P := p.map (Rat.castHom ℝ)
  have hP : P ≠ 0 := (Polynomial.map_ne_zero_iff (Rat.cast_injective (α := ℝ))).mpr hp
  obtain ⟨B, hB⟩ := exists_nat_gt (P.cauchyBound : ℝ)
  refine ⟨B, fun N hN ↦ ?_⟩
  have hroot (y : ℝ) (hy : P.IsRoot y) : y < (N : ℝ) := by
    have H : ‖y‖ < (P.cauchyBound : ℝ) := hy.norm_lt_cauchyBound hP
    exact (le_abs_self y).trans_lt
      ((by simpa only [Real.norm_eq_abs] using H : |y| < (P.cauchyBound : ℝ)).trans
        (hB.trans_le (by exact_mod_cast hN)))
  have hcoeff : P.leadingCoeff = (p.leadingCoeff : ℝ) :=
    leadingCoeff_map_of_injective (Rat.cast_injective (α := ℝ)) p
  have hev : P.eval (N : ℝ) = ((p.eval (N : ℚ) : ℚ) : ℝ) := by
    change P.eval (N : ℝ) = Rat.castHom ℝ (p.eval (N : ℚ))
    simpa only [map_natCast, P] using
      (eval_map_apply (p := p) (f := Rat.castHom ℝ) (N : ℚ))
  rcases lt_or_gt_of_ne (leadingCoeff_ne_zero.mpr hp) with hneg | hpos
  · have h : P.eval (N : ℝ) < 0 := eval_lt_zero_of_roots_lt_of_leadingCoeff_nonpos (P := P)
      hroot (by rw [hcoeff]; exact_mod_cast hneg.le)
    have he : p.eval (N : ℚ) < 0 := by rw [hev] at h; exact_mod_cast h
    simp [hneg, he, hneg.not_gt, he.not_gt, he.ne]
  · have h : 0 < P.eval (N : ℝ) := zero_lt_eval_of_roots_lt_of_leadingCoeff_nonneg (P := P)
      hroot (by rw [hcoeff]; exact_mod_cast hpos.le)
    have he : 0 < p.eval (N : ℚ) := by rw [hev] at h; exact_mod_cast h
    simp [hpos, he, hpos.not_gt, he.not_gt, he.ne']

/-- The fixed-stop tail is confined to finitely many rational rays.
On those rays the sign of a nonzero polynomial's leading coefficient
decides the comparison with the fitted face beyond a common cutoff. -/
theorem held_stop_affine_tail_table {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let Z := max (NegHypergeometric.maxBlackDraws y) r + 2
    let E := Finset.Icc (-((d * (Z + 2) : ℕ) : ℤ)) ((d * (Z + 2) : ℕ) : ℤ)
    let D := fun N : ℕ ↦ {m : ℕ | ∀ i, Discrete4NHyper.Compatible N m r (x i)}
    let L := fun N m ↦ Discrete4NHyper.totalLikelihood x N m r
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) *
      ((a : ℚ) / d) ^ r * ((b : ℚ) / d) ^ y i
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      (∀ N ≥ B, ∀ m ∈ D N, IsMaxOn (L N) (D N) m →
        ∃ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e) ∧
      ∀ N ≥ B, ∀ m ∈ D N, ∀ e ∈ E, (d : ℤ) * m = (a : ℤ) * N + e →
        ((F : ℝ) < L N m ↔ 0 < (P e).leadingCoeff) ∧
        L N m ≠ (F : ℝ) ∧
        (L N m < (F : ℝ) ↔ (P e).leadingCoeff < 0) := by
  classical
  intro x a b d Z E D L F
  have ha : 0 < a := Nat.mul_pos hh hr
  have hb : 0 < b := by
    obtain ⟨i, hi⟩ := hy
    exact hi.trans_le (Finset.single_le_sum (fun j _ ↦ Nat.zero_le (y j)) (Finset.mem_univ i))
  have hd : 0 < d := Nat.add_pos_left ha b
  have hdq : (d : ℚ) ≠ 0 := by exact_mod_cast hd.ne'
  have hap : 0 < (a : ℚ) / d := div_pos (by exact_mod_cast ha) (by exact_mod_cast hd)
  have hbp : 0 < (b : ℚ) / d := div_pos (by exact_mod_cast hb) (by exact_mod_cast hd)
  have hab : (a : ℚ) / d + (b : ℚ) / d = 1 := by
    rw [← add_div, show (a : ℚ) + b = d by simp [d], div_self hdq]
  let p : ℝ := (a : ℝ) / d
  have he : p = ((h : ℝ) * r) / ((h : ℝ) * r + ∑ i, (y i : ℝ)) := by
    simp [p, d, a, b]
  have hp : 0 < p := div_pos (by exact_mod_cast ha) (by exact_mod_cast hd)
  have hpd : p * (d : ℝ) = a := div_mul_cancel₀ _ (by exact_mod_cast hd.ne')
  obtain ⟨B, hB⟩ := held_stop_population_band hh hr y hy
  have H (e : ℤ) := affine_face_comparison hr y hy hap hbp hab ((e : ℚ) / d)
  choose P hn hP using H
  choose C hC using fun e ↦ tail_sign (P e) (hn e)
  refine ⟨max B (E.sup C), P, ?_, ?_⟩
  · intro N hN m hm hmMax
    obtain ⟨hlo, hhi⟩ := hB N ((le_max_left _ _).trans hN) m hm hmMax
    rw [← he] at hlo hhi
    let A := ⌊p * N⌋₊
    have hA₀ : (A : ℝ) ≤ p * N := Nat.floor_le (by positivity)
    have hA₁ : p * N < (A : ℝ) + 1 := Nat.lt_floor_add_one _
    have hmL : (A : ℝ) - (Z : ℝ) ≤ m := by
      by_cases hZ : Z ≤ A
      · have H : ((A - Z : ℕ) : ℝ) ≤ m := by exact_mod_cast hlo.le
        rwa [Nat.cast_sub hZ] at H
      · have H : (A : ℝ) ≤ Z := by exact_mod_cast Nat.le_of_not_ge hZ
        linarith [Nat.cast_nonneg (α := ℝ) m]
    have hmU : (m : ℝ) ≤ (A : ℝ) + Z + 1 := by exact_mod_cast hhi
    let e : ℤ := (d : ℤ) * m - (a : ℤ) * N
    have hep : (e : ℝ) = (d : ℝ) * ((m : ℝ) - p * N) := by
      push_cast [e]
      linear_combination (N : ℝ) * hpd
    have hdr : 0 ≤ (d : ℝ) := Nat.cast_nonneg _
    have hwidth : -((d : ℝ) * ((Z : ℝ) + 2)) ≤ (e : ℝ) ∧
        (e : ℝ) ≤ (d : ℝ) * ((Z : ℝ) + 2) := by
      rw [hep, ← mul_neg]
      exact ⟨mul_le_mul_of_nonneg_left (by linarith) hdr,
        mul_le_mul_of_nonneg_left (by linarith) hdr⟩
    exact ⟨e, Finset.mem_Icc.mpr ⟨by exact_mod_cast hwidth.1, by exact_mod_cast hwidth.2⟩,
      by dsimp [e]; omega⟩
  · intro N hN m hm e he hrel
    have hN' : C e ≤ N := (Finset.le_sup he).trans ((le_max_right _ _).trans hN)
    have hA : (m : ℚ) = ((a : ℚ) / d) * N + (e : ℚ) / d := by
      have H : (d : ℚ) * m = (a : ℚ) * N + e := by exact_mod_cast hrel
      field_simp [hdq]
      linarith
    have H := hP e N m hA hm
    have C' := hC e N hN'
    exact ⟨H.1.symm.trans C'.1, fun hEq ↦ C'.2.1 (H.2.1.mpr hEq),
      H.2.2.symm.trans C'.2.2⟩

private theorem stop_face_coefficient {t r : ℕ} (hr : 0 < r) (htr : r ≤ t) :
    ((r : ℝ) / t) * (t.choose r : ℝ) = ((t - 1).choose (r - 1) : ℝ) := by
  have ht : 0 < t := hr.trans_le htr
  have H := Nat.add_one_mul_choose_eq (t - 1) (r - 1)
  rw [Nat.sub_add_cancel ht, Nat.sub_add_cancel hr] at H
  have H' : (t : ℝ) * ((t - 1).choose (r - 1) : ℝ) = t.choose r * (r : ℝ) := by
    exact_mod_cast H
  have ht' : (t : ℝ) ≠ 0 := (Nat.cast_pos.mpr ht).ne'
  field_simp [ht']
  linarith only [H']

private theorem stop_face_product {h r : ℕ} (hr : 0 < r) (y : Fin h → ℕ) (p : ℝ) :
    (∏ i, ((r : ℝ) / (r + y i : ℕ)) * Discrete4Hyper.binomialFace (r + y i) r p) =
      ∏ i, ((r + y i - 1).choose (r - 1) : ℝ) * p ^ r * (1 - p) ^ y i := by
  apply Finset.prod_congr rfl
  intro i _
  unfold Discrete4Hyper.binomialFace
  rw [Nat.add_sub_cancel_left, ← stop_face_coefficient hr (Nat.le_add_right r (y i))]
  ring

/-- The fixed-stop face is optimized at a rational marked fraction; the
full coefficient, needed to compare different stopping counts, is retained. -/
theorem held_stop_optimized_face {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let p := ((a : ℚ) / d : ℚ)
    let q := ((b : ℚ) / d : ℚ)
    let F : ℚ := ∏ i, ((r + y i - 1).choose (r - 1) : ℚ) * p ^ r * q ^ y i
    let G := fun t : ℝ ↦ ∏ i, ((r : ℝ) / (r + y i : ℕ)) *
      Discrete4Hyper.binomialFace (r + y i) r t
    ((p : ℝ) ∈ Icc 0 1) ∧ G (p : ℝ) = (F : ℝ) ∧
      IsMaxOn G (Icc 0 1) (p : ℝ) := by
  intro a b d p q F G
  have ha : 0 < a := Nat.mul_pos hh hr
  have hb : 0 < b := by
    obtain ⟨i, hi⟩ := hy
    exact hi.trans_le (Finset.single_le_sum (fun j _ ↦ Nat.zero_le (y j)) (Finset.mem_univ i))
  have ha' : 0 < (a : ℝ) := by exact_mod_cast ha
  have hb' : 0 < (b : ℝ) := by exact_mod_cast hb
  have hd' : 0 < (a : ℝ) + b := add_pos ha' hb'
  have hp : (p : ℝ) = (a : ℝ) / (a + b) := by simp [p, d]
  have hq : (q : ℝ) = (b : ℝ) / (a + b) := by simp [q, d]
  have hpI : (p : ℝ) ∈ Icc 0 1 := by
    rw [hp]
    exact ⟨(div_pos ha' hd').le, (div_le_one hd').mpr (by linarith)⟩
  have hq' : (q : ℝ) = 1 - (p : ℝ) := by
    rw [hp, hq]
    field_simp [hd'.ne']
    ring
  have hF : G (p : ℝ) = (F : ℝ) := by
    unfold G
    rw [stop_face_product hr]
    simp only [F, Rat.cast_prod, Rat.cast_mul, Rat.cast_pow, Rat.cast_natCast, hq']
  let C := ∏ i, ((r + y i - 1).choose (r - 1) : ℝ)
  have hG (t : ℝ) (ht : t ∈ Icc 0 1) :
      G t = C * Discrete.Binomial.likelihood (a : ℝ) (b : ℝ) t := by
    unfold G
    rw [stop_face_product hr]
    simp only [Finset.prod_mul_distrib, Finset.prod_pow_eq_pow_sum, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    have H : t ^ a * (1 - t) ^ b = Discrete.Binomial.likelihood (a : ℝ) (b : ℝ) t := by
      by_cases ht₀ : t = 0
      · simp [ht₀, ha.ne', Discrete.Binomial.likelihood, ha'.ne']
      by_cases ht₁ : t = 1
      · simp [ht₁, hb.ne', Discrete.Binomial.likelihood, hb'.ne']
      have H₀ : 0 < t := ht.1.lt_of_ne (Ne.symm ht₀)
      have H₁ : 0 < 1 - t := sub_pos.mpr (ht.2.lt_of_ne ht₁)
      rw [Discrete.Binomial.likelihood, ite_eq_right (by simp [ht₀, ht₁]), Real.exp_add,
        ← Real.log_pow, ← Real.log_pow, Real.exp_log (pow_pos H₀ _), Real.exp_log (pow_pos H₁ _)]
    rw [← H]
    dsimp [a, b, C]
    ring
  refine ⟨hpI, hF, fun t ht ↦ ?_⟩
  change G t ≤ G (p : ℝ)
  rw [hG t ht, hG _ hpI]
  exact mul_le_mul_of_nonneg_left
    (((Discrete.Binomial.fixed_size_mle ha'.le hb'.le hd' _ hpI).mpr hp) ht)
    (by dsimp [C]; positivity)

private theorem falling_bounds {N t X : ℕ} (htX : t ≤ X) (hXN : X ≤ N) :
    ((N : ℝ) - X) ^ t ≤ (N.descFactorial t : ℝ) ∧
      (N.descFactorial t : ℝ) ≤ (N : ℝ) ^ t := by
  constructor
  · have H := Finset.prod_le_prod₀ (s := Finset.range t)
      (f := fun _ ↦ (N : ℝ) - X) (g := fun j ↦ ((N - j : ℕ) : ℝ))
      (fun _ _ ↦ by exact sub_nonneg.mpr (by exact_mod_cast hXN)) (fun j hj ↦ by
        have hjX := (Finset.mem_range.mp hj).le.trans htX
        rw [Nat.cast_sub (hjX.trans hXN)]
        exact sub_le_sub_left (by exact_mod_cast hjX) _)
    simpa [Nat.descFactorial_eq_prod_range, Nat.cast_prod] using H
  · have H := Finset.prod_le_prod₀ (s := Finset.range t)
      (f := fun j ↦ ((N - j : ℕ) : ℝ)) (g := fun _ ↦ (N : ℝ))
      (fun _ _ ↦ Nat.cast_nonneg _) (fun j _ ↦ by exact_mod_cast Nat.sub_le N j)
    simpa [Nat.descFactorial_eq_prod_range, Nat.cast_prod] using H

/-- An explicit finite-search bound once a finite witness beats the face.
The bound uses only rational arithmetic when the likelihood heights are
rational, and it is uniform over every compatible marked count. -/
theorem held_stop_explicit_cutoff {h r : ℕ} (hr : 0 < r) (y : Fin h → ℕ)
    {F W : ℝ} (hW : 0 < W) (hFW : F < W)
    (hF : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, ((r : ℝ) / (r + y i : ℕ)) * Discrete4Hyper.binomialFace (r + y i) r p) ≤ F) :
    let x := fun i ↦ r + y i
    let X := Finset.univ.sup x
    let T := ∑ i, x i
    ∀ N : ℕ, X < N → (W * (T : ℝ) * X) / (W - F) < (N : ℝ) →
      ∀ m ≤ N, (∀ i, Discrete4NHyper.Compatible N m r (x i)) →
        Discrete4NHyper.totalLikelihood x N m r < W := by
  intro x X T N hXN hNW m hm hc
  let p : ℝ := (m : ℝ) / N
  let q : ℝ := 1 - (X : ℝ) / N
  have hn : 0 < (N : ℝ) := Nat.cast_pos.mpr (Nat.zero_le X |>.trans_lt hXN)
  have hp : p ∈ Icc 0 1 :=
    ⟨div_nonneg (Nat.cast_nonneg _) hn.le, (div_le_one hn).mpr (by exact_mod_cast hm)⟩
  have hq : 0 < q := sub_pos.mpr ((div_lt_one hn).mpr (by exact_mod_cast hXN))
  have hp' : 1 - p = ((N - m : ℕ) : ℝ) / N := by
    rw [Nat.cast_sub hm]
    unfold p
    field_simp [hn.ne']
  have hq' : q = ((N : ℝ) - X) / N := by
    unfold q
    field_simp [hn.ne']
  have hxi (i : Fin h) : x i ≤ X := Finset.le_sup (Finset.mem_univ i)
  have hlocal (i : Fin h) :
      Discrete4NHyper.totalMass N m r (x i) * q ^ x i ≤
        ((x i - 1).choose (r - 1) : ℝ) * p ^ r * (1 - p) ^ y i := by
    have hxm : x i ≤ N := (hxi i).trans hXN.le
    let C : ℝ := ((x i - 1).choose (r - 1) : ℝ)
    let M : ℝ := (m.descFactorial r : ℝ) * ((N - m).descFactorial (y i) : ℝ)
    let B : ℝ := (N.descFactorial (x i) : ℝ)
    have hB : 0 < B := Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hxm)
    have hmass : Discrete4NHyper.totalMass N m r (x i) = C * (M / B) := by
      unfold C M B x
      rw [Discrete4NHyper.totalMass, ite_eq_left (hc i), Nat.add_sub_cancel_left,
        NegHypergeometric.mass, NegHypergeometric.reducedMass, Nat.add_sub_of_le hm]
    have hD := (falling_bounds (hxi i) hXN.le).1
    have hrm : r ≤ m := (hc i).2.1
    have hy : y i ≤ N - m := by
      have H := (hc i).2.2
      rw [ite_eq_right hr.ne'] at H
      simpa [x] using H.2
    have hM : M ≤ (m : ℝ) ^ r * ((N - m : ℕ) : ℝ) ^ y i :=
      mul_le_mul ((falling_bounds hrm (le_refl m)).2)
        ((falling_bounds hy (le_refl (N - m))).2) (by positivity) (by positivity)
    have hT : (M / B) * ((N : ℝ) - X) ^ x i ≤ (m : ℝ) ^ r * ((N - m : ℕ) : ℝ) ^ y i :=
      (mul_le_mul_of_nonneg_left hD (by unfold M; positivity)).trans_eq
        (div_mul_cancel₀ _ hB.ne') |>.trans hM
    rw [hmass, hp', hq', div_pow, show p ^ r = (m : ℝ) ^ r / (N : ℝ) ^ r by
      simp only [p, div_pow], div_pow]
    have hpow : (N : ℝ) ^ x i = (N : ℝ) ^ r * (N : ℝ) ^ y i := by simp [x, pow_add]
    have he₁ : C * (M / B) * (((N : ℝ) - X) ^ x i / (N : ℝ) ^ x i) =
        (C * ((M / B) * ((N : ℝ) - X) ^ x i)) / (N : ℝ) ^ x i := by ring
    have he₂ : C * ((m : ℝ) ^ r / (N : ℝ) ^ r) * (((N - m : ℕ) : ℝ) ^ y i / (N : ℝ) ^ y i) =
        (C * ((m : ℝ) ^ r * ((N - m : ℕ) : ℝ) ^ y i)) / (N : ℝ) ^ x i := by
      rw [hpow]
      ring
    change C * (M / B) * (((N : ℝ) - X) ^ x i / (N : ℝ) ^ x i) ≤ _
    rw [he₁, he₂]
    exact div_le_div_of_nonneg_right (mul_le_mul_of_nonneg_left hT (by unfold C; positivity))
      (by positivity)
  let L := Discrete4NHyper.totalLikelihood x N m r
  have HS : L * q ^ T ≤ F := by
    have H := Finset.prod_le_prod₀ (s := Finset.univ)
      (f := fun i ↦ Discrete4NHyper.totalMass N m r (x i) * q ^ x i)
      (g := fun i ↦ ((x i - 1).choose (r - 1) : ℝ) * p ^ r * (1 - p) ^ y i)
      (fun i _ ↦ by
        rw [Discrete4NHyper.totalMass, ite_eq_left (hc i)]
        unfold NegHypergeometric.mass NegHypergeometric.reducedMass
        positivity) (fun i _ ↦ hlocal i)
    change (∏ i, Discrete4NHyper.totalMass N m r (x i) * q ^ x i) ≤ _ at H
    rw [Finset.prod_mul_distrib, Finset.prod_pow_eq_pow_sum] at H
    have H' := hF p hp
    rw [stop_face_product hr] at H'
    exact H.trans H'
  have hBernoulli : 1 - (T : ℝ) * ((X : ℝ) / N) ≤ q ^ T := by
    have H := (div_lt_one hn).mpr (by exact_mod_cast hXN : (X : ℝ) < N)
    simpa only [q, sub_eq_add_neg, mul_neg] using
      (one_add_mul_le_pow (a := -((X : ℝ) / N)) (by linarith) T)
  have hgap : F < W * (1 - (T : ℝ) * ((X : ℝ) / N)) := by
    have H := (div_lt_iff₀ (sub_pos.mpr hFW)).mp hNW
    rw [show W * (1 - (T : ℝ) * ((X : ℝ) / N)) =
        (W * N - W * (T : ℝ) * X) / N by field_simp [hn.ne']]
    apply (lt_div_iff₀ hn).mpr
    linarith only [H]
  exact (mul_lt_mul_iff_left₀ (pow_pos hq _)).mp
    (HS.trans_lt (hgap.trans_le (mul_le_mul_of_nonneg_left hBernoulli hW.le)))

private theorem tail_residue {a d N : ℕ} (e : ℤ) (hd : 0 < d)
    (h : (d : ℤ) ∣ (a : ℤ) * N + e) :
    ∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e := by
  refine ⟨N % d, Nat.mod_lt _ hd, ?_⟩
  have H : (N % d : ℕ) + d * (N / d) = N := Nat.mod_add_div _ _
  have H' : ((N % d : ℕ) : ℤ) + (d : ℤ) * ((N / d : ℕ) : ℤ) = N := by exact_mod_cast H
  have hsub : (d : ℤ) ∣ (a : ℤ) * (d : ℤ) * ((N / d : ℕ) : ℤ) :=
    ⟨(a : ℤ) * ((N / d : ℕ) : ℤ), by ring⟩
  convert dvd_sub h hsub using 1
  linear_combination (a : ℤ) * H'

private theorem compatible_tail {h a d r : ℕ} (hr : 0 < r) (y : Fin h → ℕ)
    (ha : 0 < a) (had : a < d) (e : ℤ) (j : ℕ)
    (hj : (d : ℤ) ∣ (a : ℤ) * j + e) (B : ℕ) :
    ∃ N ≥ B, ∃ m : ℕ, (∀ i, Discrete4NHyper.Compatible N m r (r + y i)) ∧
      (d : ℤ) * m = (a : ℤ) * N + e := by
  obtain ⟨q, hq⟩ := hj
  let Y := NegHypergeometric.maxBlackDraws y
  let W := max r Y
  let c : ℕ := B + e.natAbs + d * W + 1
  let N := j + d * c
  let M : ℤ := q + (a : ℤ) * c
  have hd : 0 < d := ha.trans had
  have hcN : c ≤ N := by
    calc
      c = 1 * c := (one_mul c).symm
      _ ≤ d * c := Nat.mul_le_mul_right c hd
      _ ≤ N := Nat.le_add_left _ _
  have hBN : B ≤ N := (by dsimp [c]; omega : B ≤ c).trans hcN
  have hN : e.natAbs + d * W ≤ N := (by dsimp [c]; omega : e.natAbs + d * W ≤ c).trans hcN
  have hN' : (e.natAbs : ℤ) + d * W ≤ (N : ℤ) := by exact_mod_cast hN
  have hM : (d : ℤ) * M = (a : ℤ) * N + e := by
    dsimp [M, N]
    linear_combination -hq
  have hE₁ : -(e.natAbs : ℤ) ≤ e := by omega
  have hE₂ : e ≤ (e.natAbs : ℤ) := by omega
  have ha' : (1 : ℤ) ≤ a := by exact_mod_cast ha
  have hda : (a : ℤ) + 1 ≤ d := by exact_mod_cast (Nat.succ_le_of_lt had)
  have hd' : (0 : ℤ) < d := by exact_mod_cast hd
  have hMlo : (W : ℤ) ≤ M := by
    by_contra! H
    have h₁ := mul_lt_mul_of_pos_left H hd'
    have h₂ := mul_le_mul_of_nonneg_right ha' (Int.natCast_nonneg N)
    nlinarith only [h₁, h₂, hM, hN', hE₁]
  have hMhi : M + W ≤ (N : ℤ) := by
    by_contra! H
    have h₁ := mul_lt_mul_of_pos_left H hd'
    have h₂ := mul_le_mul_of_nonneg_right hda (Int.natCast_nonneg N)
    nlinarith only [h₁, h₂, hM, hN', hE₂]
  let m := M.toNat
  have hm : (m : ℤ) = M := Int.toNat_of_nonneg ((Int.natCast_nonneg W).trans hMlo)
  have hmW : W ≤ m := by exact_mod_cast (show (W : ℤ) ≤ m by rw [hm]; exact hMlo)
  have hmN : m + W ≤ N := by exact_mod_cast (show (m : ℤ) + W ≤ N by rw [hm]; exact hMhi)
  refine ⟨N, hBN, m, fun i ↦ ?_, by rw [hm, hM]⟩
  have hYi : y i ≤ W := (Finset.le_sup (Finset.mem_univ i)).trans (le_max_right _ _)
  refine ⟨by omega, (le_max_left _ _).trans hmW, ?_⟩
  simp only [ite_eq_right hr.ne', Nat.add_sub_cancel_left]
  exact ⟨Nat.le_add_right _ _, by omega⟩

private theorem population_winner {h N r : ℕ} (hh : 0 < h) (y : Fin h → ℕ)
    {m : ℕ} (hm : ∀ i, Discrete4NHyper.Compatible N m r (r + y i)) :
    ∃ u, (∀ i, Discrete4NHyper.Compatible N u r (r + y i)) ∧
      IsMaxOn (fun u ↦ Discrete4NHyper.totalLikelihood (fun i ↦ r + y i) N u r)
        {u | ∀ i, Discrete4NHyper.Compatible N u r (r + y i)} u := by
  classical
  let D := (Finset.range (N + 1)).filter fun u ↦
    ∀ i, Discrete4NHyper.Compatible N u r (r + y i)
  have hD (u : ℕ) : u ∈ D ↔ ∀ i, Discrete4NHyper.Compatible N u r (r + y i) := by
    simp only [D, Finset.mem_filter, Finset.mem_range]
    exact ⟨And.right, fun H ↦ ⟨Nat.lt_succ_of_le (H ⟨0, hh⟩).1, H⟩⟩
  obtain ⟨u, hu, hmax⟩ := Finset.exists_max_image D
    (fun u ↦ Discrete4NHyper.totalLikelihood (fun i ↦ r + y i) N u r) ⟨m, (hD m).mpr hm⟩
  exact ⟨u, (hD u).mp hu, fun v hv ↦ hmax v ((hD v).mpr hv)⟩

/-- For a positive held stop with any positive excess, a finite prefix
and the signs on its compatible residue classes decide strict
improvement, finite equality, or strict face dominance. The equality
branch gives every finite co-winner. A strict witness confines every
winner to a nonempty finite search box. -/
theorem held_stop_positive_decision {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (y : Fin h → ℕ)
    (hy : ∃ i, 0 < y i) :
    let x := fun i ↦ r + y i
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    let Z := max (NegHypergeometric.maxBlackDraws y) r + 2
    let E := Finset.Icc (-((d * (Z + 2) : ℕ) : ℤ)) ((d * (Z + 2) : ℕ) : ℤ)
    let D := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (x i)}
    let L := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2 r
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) *
      ((a : ℚ) / d) ^ r * ((b : ℚ) / d) ^ y i
    ∃ (B : ℕ) (P : ℤ → ℚ[X]),
      let strict := (∃ N < B, ∃ m, (N, m) ∈ D ∧ (F : ℝ) < L (N, m)) ∨
        (∃ e ∈ E, (∃ j < d, (d : ℤ) ∣ (a : ℤ) * j + e) ∧ 0 < (P e).leadingCoeff)
      (strict ↔ ∃ v ∈ D, (F : ℝ) < L v) ∧
      (strict → ∃ C : ℕ, let box := {v ∈ D | v.1 < C}
        (∃ v ∈ box, IsMaxOn L box v ∧ (F : ℝ) < L v) ∧
        ∀ v, (v ∈ D ∧ IsMaxOn L D v) ↔ v ∈ box ∧ IsMaxOn L box v) ∧
      (¬ strict → IsLUB (L '' D) (F : ℝ) ∧
        ∀ v ∈ D, IsMaxOn L D v ↔ v.1 < B ∧ L v = (F : ℝ)) := by
  classical
  intro x a b d Z E D L F
  have ha : 0 < a := Nat.mul_pos hh hr
  have hb : 0 < b := by
    obtain ⟨i, hi⟩ := hy
    exact hi.trans_le (Finset.single_le_sum (fun j _ ↦ Nat.zero_le (y j)) (Finset.mem_univ i))
  have had : a < d := Nat.lt_add_of_pos_right hb
  let p : ℚ := (a : ℚ) / d
  let G := fun t : ℝ ↦ ∏ i, ((r : ℝ) / (x i : ℕ)) * Discrete4Hyper.binomialFace (x i) r t
  obtain ⟨hp, hpF, hG⟩ := held_stop_optimized_face hh hr y hy
  change (p : ℝ) ∈ Icc 0 1 at hp
  change G (p : ℝ) = (F : ℝ) at hpF
  change IsMaxOn G (Icc 0 1) (p : ℝ) at hG
  obtain ⟨B, P, hband, hsign⟩ := held_stop_affine_tail_table hh hr y hy
  refine ⟨B, P, ?_⟩
  intro strict
  have hstrict : strict ↔ ∃ v ∈ D, (F : ℝ) < L v := by
    constructor
    · rintro (⟨N, -, m, hv, hval⟩ | ⟨e, he, ⟨j, -, hj⟩, hpos⟩)
      · exact ⟨(N, m), hv, hval⟩
      · obtain ⟨N, hN, m, hm, hray⟩ := compatible_tail hr y ha had e j hj B
        exact ⟨(N, m), hm, (hsign N hN m hm e he hray).1.mpr hpos⟩
    · rintro ⟨⟨N, m⟩, hm, hval⟩
      by_cases hN : N < B
      · exact Or.inl ⟨N, hN, m, hm, hval⟩
      · have hN' := le_of_not_gt hN
        obtain ⟨u, hu, hmax⟩ := population_winner hh y hm
        obtain ⟨e, he, hrel⟩ := hband N hN' u hu hmax
        exact Or.inr ⟨e, he, tail_residue e (ha.trans had) ⟨u, hrel.symm⟩,
          (hsign N hN' u hu e he hrel).1.mp (hval.trans_le (hmax hm))⟩
  refine ⟨hstrict, ?_, ?_⟩
  · intro hs
    obtain ⟨w, hw, hwval⟩ := hstrict.mp hs
    let X := Finset.univ.sup x
    let T := ∑ i, x i
    let U := (L w * (T : ℝ) * X) / (L w - (F : ℝ))
    let C := max (X + 1) (⌊U⌋₊ + 1)
    have hF₀ : 0 ≤ (F : ℝ) := by
      have hp₀ : 0 ≤ (p : ℝ) := hp.1
      have hp₁ : 0 ≤ 1 - (p : ℝ) := sub_nonneg.mpr hp.2
      rw [← hpF]
      unfold G Discrete4Hyper.binomialFace
      positivity
    have hC (N : ℕ) (hN : C ≤ N) (m : ℕ) (hm : (N, m) ∈ D) : L (N, m) < L w := by
      have hX : X < N := (Nat.lt_succ_self _).trans_le ((le_max_left _ _).trans hN)
      have hU : U < (N : ℝ) := (Nat.lt_floor_add_one U).trans_le (by
        exact_mod_cast (show ⌊U⌋₊ + 1 ≤ N from (le_max_right _ _).trans hN))
      exact held_stop_explicit_cutoff hr y (hF₀.trans_lt hwval) hwval
        (fun t ht ↦ (show G t ≤ G (p : ℝ) from hG ht).trans_eq hpF)
        N hX hU m (hm ⟨0, hh⟩).1 hm
    let S := ((Finset.range C) ×ˢ (Finset.range C)).filter fun v ↦ v ∈ D
    have hS (v : ℕ × ℕ) : v ∈ S ↔ v ∈ D ∧ v.1 < C := by
      simp only [S, Finset.mem_filter, Finset.mem_product, Finset.mem_range]
      exact ⟨fun ⟨H, hv⟩ ↦ ⟨hv, H.1⟩,
        fun ⟨hv, hvC⟩ ↦ ⟨⟨hvC, ((hv ⟨0, hh⟩).1).trans_lt hvC⟩, hv⟩⟩
    have hwC : w.1 < C := by
      by_contra! H
      exact (lt_irrefl _ (hC w.1 H w.2 hw))
    obtain ⟨u, hu, hum⟩ := Finset.exists_max_image S L ⟨w, (hS w).mpr ⟨hw, hwC⟩⟩
    have huval := hwval.trans_le (hum _ ((hS w).mpr ⟨hw, hwC⟩))
    refine ⟨C, ⟨u, (hS u).mp hu, fun v hv ↦ hum v ((hS v).mpr hv), huval⟩, fun v ↦ ?_⟩
    constructor
    · rintro ⟨hv, hmax⟩
      have hvC : v.1 < C := by
        by_contra! H
        exact (hC v.1 H v.2 hv).not_ge (hmax hw)
      exact ⟨⟨hv, hvC⟩, fun q hq ↦ hmax hq.1⟩
    · rintro ⟨hv, hmax⟩
      exact ⟨hv.1, fun q hq ↦ by
        by_cases hqC : q.1 < C
        · exact hmax ⟨hq, hqC⟩
        · exact ((hC q.1 (le_of_not_gt hqC) q.2 hq).trans_le
            (hmax ⟨hw, hwC⟩)).le⟩
  · intro hn
    have hub (v : ℕ × ℕ) (hv : v ∈ D) : L v ≤ (F : ℝ) := by
      by_contra! H
      exact hn (hstrict.mpr ⟨v, hv, H⟩)
    obtain ⟨m, N, -, hc, -, hlim⟩ :=
      Base7bNHyper.held_stop_face_reachable hr x (fun i ↦ Nat.le_add_right r (y i)) hp
    change Tendsto (fun k ↦ L (N k, m k)) atTop (𝓝 (G (p : ℝ))) at hlim
    rw [hpF] at hlim
    have hleast (t : ℝ) (ht : t ∈ upperBounds (L '' D)) : (F : ℝ) ≤ t :=
      le_of_tendsto' hlim fun k ↦ ht ⟨(N k, m k), hc k, rfl⟩
    refine ⟨⟨by rintro _ ⟨v, hv, rfl⟩; exact hub v hv, fun t ht ↦ hleast t ht⟩, fun v hv ↦ ?_⟩
    constructor
    · intro hmax
      have heq : L v = (F : ℝ) :=
        (hub v hv).antisymm (hleast _ (by rintro _ ⟨q, hq, rfl⟩; exact hmax hq))
      refine ⟨?_, heq⟩
      by_contra! hN
      have hmax' : IsMaxOn (fun m ↦ L (v.1, m))
          {m | ∀ i, Discrete4NHyper.Compatible v.1 m r (x i)} v.2 :=
        fun q hq ↦ hmax (a := (v.1, q)) hq
      obtain ⟨e, he, hrel⟩ := hband v.1 hN v.2 hv hmax'
      exact (hsign v.1 hN v.2 hv e he hrel).2.1 heq
    · rintro ⟨-, heq⟩ q hq
      exact (hub q hq).trans_eq heq.symm

private theorem compatible_likelihood_rational {h N m r : ℕ} (hr : 0 < r) (x : Fin h → ℕ)
    (hc : ∀ i, Discrete4NHyper.Compatible N m r (x i)) :
    ∃ q : ℚ, (q : ℝ) = Discrete4NHyper.totalLikelihood x N m r := by
  classical
  let q : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) *
    ((m.descFactorial r : ℚ) * ((N - m).descFactorial (x i - r) : ℚ) /
      (N.descFactorial (x i) : ℚ))
  refine ⟨q, ?_⟩
  rw [show (q : ℝ) = ∏ i, ((x i - 1).choose (r - 1) : ℝ) *
      ((m.descFactorial r : ℝ) * ((N - m).descFactorial (x i - r) : ℝ) /
        (N.descFactorial (x i) : ℝ)) by push_cast [q]; rfl]
  unfold Discrete4NHyper.totalLikelihood
  apply Finset.prod_congr rfl
  intro i _
  have H := (hc i).2.2
  rw [ite_eq_right hr.ne'] at H
  rw [Discrete4NHyper.totalMass, ite_eq_left (hc i), NegHypergeometric.mass,
    NegHypergeometric.reducedMass, Nat.add_sub_of_le (hc i).1, Nat.add_sub_of_le H.1]

private theorem stopped_constant {h : ℕ} (hh : 0 < h) (r : ℕ) :
    let x : Fin h → ℕ := fun _ ↦ r
    let D := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (x i)}
    let L := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2 r
    (r, r) ∈ D ∧ L (r, r) = 1 ∧
    (∀ v ∈ D, IsMaxOn L D v ↔ r = 0 ∨ v.1 = v.2) ∧ IsLUB (L '' D) 1 := by
  intro x D L
  have hrD : (r, r) ∈ D := by
    intro i
    refine ⟨le_rfl, le_rfl, ?_⟩
    by_cases hr : r = 0 <;> simp [x, hr]
  obtain ⟨hrL, hb⟩ := Discrete4NHyper.constant_attains_saturated (h := h) r
  have he (v : ℕ × ℕ) (hv : v ∈ D) : L v = 1 ↔ r = 0 ∨ v.1 = v.2 := by
    rw [Discrete4NHyper.constant_saturated_iff hh]
    have H := hv ⟨0, hh⟩
    exact ⟨fun a ↦ a.2.2.2.imp_right Eq.symm,
      fun a ↦ ⟨H.1, H.2.1, rfl, a.imp_right Eq.symm⟩⟩
  refine ⟨hrD, hrL, fun v hv ↦ ?_, ?_⟩
  · rw [← he v hv]
    exact ⟨fun hm ↦ (hb _ _ _).antisymm (hrL ▸ hm hrD),
      fun eq q _ ↦ eq ▸ hb _ _ _⟩
  · exact ⟨by rintro _ ⟨v, -, rfl⟩; exact hb _ _ _,
      fun _ H ↦ hrL ▸ H ⟨(r, r), hrD, rfl⟩⟩

/-- The complete held-stop comparison. Zero stop gives precisely the flat
compatible zero branch. Positive saturated data give exactly the all-marked
urns. In every other compatible case the slice height is rational, the
entire maximizing set is a finite search, and absence of a finite equality
selects the optimized shifted negative-binomial face strictly. -/
theorem held_stop_complete_search {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) (r : ℕ)
    (hf : ∃ N m, ∀ i, Discrete4NHyper.Compatible N m r (x i)) :
    let D := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (x i)}
    let L := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2 r
    let p : ℚ := ((h * r : ℕ) : ℚ) / ((∑ i, x i : ℕ) : ℚ)
    let F : ℚ := ∏ i, ((x i - 1).choose (r - 1) : ℚ) * p ^ r * (1 - p) ^ (x i - r)
    (r = 0 ∧ (∀ i, x i = 0) ∧ (∀ v ∈ D, L v = 1 ∧ IsMaxOn L D v) ∧
      IsLUB (L '' D) 1) ∨
    (0 < r ∧ (∀ i, x i = r) ∧ (r, r) ∈ D ∧
      (∀ v ∈ D, IsMaxOn L D v ↔ v.1 = v.2) ∧ IsLUB (L '' D) 1) ∨
    (0 < r ∧ (∃ i, r < x i) ∧ ∃ (H : ℚ) (C : ℕ),
      IsLUB (L '' D) (H : ℝ) ∧ (F : ℝ) ≤ (H : ℝ) ∧
      (∀ v ∈ D, IsMaxOn L D v ↔ v.1 < C ∧ L v = (H : ℝ)) ∧
      ((∃ v ∈ D, IsMaxOn L D v) ↔ ∃ v ∈ D, v.1 < C ∧ L v = (H : ℝ)) ∧
      ((¬ ∃ v ∈ D, v.1 < C ∧ L v = (H : ℝ)) →
        H = F ∧ ∀ v ∈ D, L v < (F : ℝ))) := by
  classical
  intro D L p F
  obtain ⟨N, m, hc⟩ := hf
  by_cases hr : r = 0
  · subst r
    have hx (i : Fin h) : x i = 0 := by simpa using (hc i).2.2
    have ex : x = fun _ ↦ 0 := funext hx
    subst x
    obtain ⟨-, -, hm, hsup⟩ := stopped_constant hh 0
    refine Or.inl ⟨rfl, fun _ ↦ rfl, ?_, hsup⟩
    intro v hv
    have H := Discrete4NHyper.constant_saturated_iff (N := v.1) (m := v.2)
      (r := 0) (c := 0) hh
    exact ⟨H.mpr ⟨(hv ⟨0, hh⟩).1, Nat.zero_le _, rfl, Or.inl rfl⟩,
      (hm v hv).mpr (Or.inl rfl)⟩
  have hr' : 0 < r := Nat.pos_of_ne_zero hr
  have hle (i : Fin h) : r ≤ x i := by
    have H := (hc i).2.2
    rw [ite_eq_right hr] at H
    exact H.1
  by_cases hconst : ∀ i, x i = r
  · have ex : x = fun _ ↦ r := funext hconst
    subst x
    obtain ⟨hD, -, hm, hsup⟩ := stopped_constant hh r
    exact Or.inr (Or.inl ⟨hr', fun _ ↦ rfl, hD,
      fun v hv ↦ (hm v hv).trans (or_iff_right hr), hsup⟩)
  · push Not at hconst
    obtain ⟨i, hi⟩ := hconst
    have hi' : r < x i := (hle i).lt_of_ne (Ne.symm hi)
    refine Or.inr (Or.inr ⟨hr', ⟨i, hi'⟩, ?_⟩)
    let y := fun i ↦ x i - r
    have hy : ∃ i, 0 < y i := ⟨i, Nat.sub_pos_of_lt hi'⟩
    have ex : (fun i ↦ r + y i) = x := funext fun i ↦ Nat.add_sub_of_le (hle i)
    let a := h * r
    let b := ∑ i, y i
    let d := a + b
    have hd : d = ∑ i, x i := by
      have H : ∑ i, (r + y i) = ∑ i, x i := congrArg (fun z : Fin h → ℕ ↦ ∑ i, z i) ex
      simpa [d, a, b, Finset.sum_add_distrib, nsmul_eq_mul] using H
    have hd' : (d : ℚ) ≠ 0 := by
      have H : 0 < d := Nat.add_pos_left (Nat.mul_pos hh hr') b
      exact_mod_cast H.ne'
    have hq : ((b : ℚ) / d : ℚ) = 1 - (a : ℚ) / d := by
      field_simp [hd']
      simp [d]
    let F' : ℚ := ∏ i, ((r + y i - 1).choose (r - 1) : ℚ) *
      ((a : ℚ) / d) ^ r * ((b : ℚ) / d) ^ y i
    have hF : F' = F := by
      unfold F' F
      rw [hq, hd]
      exact Finset.prod_congr rfl fun i _ ↦ by rw [congrFun ex i]
    let D' := {v : ℕ × ℕ | ∀ i, Discrete4NHyper.Compatible v.1 v.2 r (r + y i)}
    let L' := fun v : ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood (fun i ↦ r + y i) v.1 v.2 r
    have hD' : D' = D := by
      ext v
      simp only [D', D, mem_ofPred_eq, congrFun ex]
    have hL' : L' = L := by
      unfold L' L
      rw [ex]
    obtain ⟨B, P, ht⟩ := held_stop_positive_decision hh hr' y hy
    dsimp only at ht
    have hs : _ ↔ ∃ v ∈ D', (F' : ℝ) < L' v := ht.1
    rw [hD', hL', hF] at hs
    by_cases hstrict : ∃ v ∈ D, (F : ℝ) < L v
    · obtain ⟨C, ⟨w, hw, hwm, hwval⟩, hmax⟩ := ht.2.1 (hs.mpr hstrict)
      change w ∈ {v ∈ D' | v.1 < C} at hw
      change IsMaxOn L' {v ∈ D' | v.1 < C} w at hwm
      change (F' : ℝ) < L' w at hwval
      change ∀ v, (v ∈ D' ∧ IsMaxOn L' D' v) ↔
        v ∈ {v ∈ D' | v.1 < C} ∧ IsMaxOn L' {v ∈ D' | v.1 < C} v at hmax
      simp only [hD', hL', hF] at hw hwm hwval hmax
      have hmw := (hmax w).mpr ⟨hw, hwm⟩
      change w ∈ D ∧ IsMaxOn L D w at hmw
      obtain ⟨H, hH⟩ := compatible_likelihood_rational hr' x hmw.1
      change (H : ℝ) = L w at hH
      refine ⟨H, C, ?_, hH.symm ▸ hwval.le, ?_, ?_, ?_⟩
      · exact ⟨by rintro _ ⟨v, hv, rfl⟩; rw [hH]; exact hmw.2 hv,
          fun _ h ↦ hH ▸ h ⟨w, hmw.1, rfl⟩⟩
      · intro v hv
        constructor
        · intro hm
          exact ⟨((hmax v).mp ⟨hv, hm⟩).1.2, hH.symm ▸ (hmw.2 hv).antisymm (hm hmw.1)⟩
        · rintro ⟨-, he⟩
          exact fun q hq ↦ (hmw.2 hq).trans_eq (he.trans hH).symm
      · exact ⟨fun _ ↦ ⟨w, hw.1, hw.2, hH.symm⟩, fun _ ↦ ⟨w, hmw.1, hmw.2⟩⟩
      · intro h
        exact (h ⟨w, hw.1, hw.2, hH.symm⟩).elim
    · obtain ⟨hsup, hmax⟩ := ht.2.2 (mt hs.mp hstrict)
      change IsLUB (L' '' D') (F' : ℝ) at hsup
      change ∀ v ∈ D', IsMaxOn L' D' v ↔ v.1 < B ∧ L' v = (F' : ℝ) at hmax
      simp only [hD', hL', hF] at hsup hmax
      refine ⟨F, B, hsup, le_rfl, hmax, ?_, ?_⟩
      · exact ⟨fun ⟨v, hv, hm⟩ ↦ ⟨v, hv, (hmax v hv).mp hm⟩,
          fun ⟨v, hv, he⟩ ↦ ⟨v, hv, (hmax v hv).mpr he⟩⟩
      · intro H
        refine ⟨rfl, fun v hv ↦ ?_⟩
        apply lt_of_le_of_ne (hsup.1 ⟨v, hv, rfl⟩)
        intro he
        have hm : IsMaxOn L D v :=
          fun q hq ↦ (hsup.1 ⟨q, hq, rfl⟩).trans_eq he.symm
        exact H ⟨v, hv, (hmax v hv).mp hm⟩

/-- The complete all-free decision reduces to the finite set of stopping
counts. Slice heights are rational and finite co-winners, including ties
with a face-only slice, are retained. The deterministic and incompatible
zero branches are included. -/
theorem all_free_rational_search {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) :
    let D := {v : ℕ × ℕ × ℕ | ∀ j, Discrete4NHyper.Compatible v.1 v.2.1 v.2.2 (x j)}
    let L := fun v : ℕ × ℕ × ℕ ↦ Discrete4NHyper.totalLikelihood x v.1 v.2.1 v.2.2
    let V := fun r : ℕ ↦ {z : ℝ | ∃ N m, (N, m, r) ∈ D ∧ z = L (N, m, r)}
    ((∃ i j, x i = 0 ∧ 0 < x j) → D = ∅) ∧
    (∀ c : ℕ, (∀ i, x i = c) →
      ((c, c, c) ∈ D ∧ IsMaxOn L D (c, c, c)) ∧
      ∀ v ∈ D, IsMaxOn L D v ↔ v.2.2 = c ∧ (c = 0 ∨ v.2.1 = v.1)) ∧
    (∀ i, (∀ j, x i ≤ x j) → 0 < x i → (∃ j, x i < x j) →
      let S := Finset.Icc 1 (x i)
      ∃ (H : ℕ → ℚ) (C : ℕ → ℕ) (r : ℕ), r ∈ S ∧ (∀ s ∈ S, H s ≤ H r) ∧
        (∀ s ∈ S, IsLUB (V s) (H s : ℝ)) ∧
        (∀ v ∈ D, IsMaxOn L D v ↔ v.2.2 ∈ S ∧ v.1 < C v.2.2 ∧ L v = (H r : ℝ)) ∧
        ((∃ v ∈ D, IsMaxOn L D v) ↔
          ∃ s ∈ S, H s = H r ∧ ∃ N m, N < C s ∧ (N, m, s) ∈ D ∧ L (N, m, s) = (H r : ℝ))) := by
  classical
  intro D L V
  refine ⟨?_, ?_, ?_⟩
  · intro h
    apply Set.eq_empty_iff_forall_notMem.mpr
    intro v hv
    exact (Base7bNHyper.all_free_maximizing_escape hh x).1 h ⟨v.1, v.2.1, v.2.2, hv⟩
  · intro c hx
    have ex : x = fun _ ↦ c := funext hx
    subst x
    obtain ⟨he, hb⟩ := Discrete4NHyper.constant_attains_saturated (h := h) c
    have hc : (c, c, c) ∈ D := by
      intro i
      refine ⟨le_rfl, le_rfl, ?_⟩
      by_cases h : c = 0 <;> simp [h]
    refine ⟨⟨hc, ?_⟩, fun v hv ↦ ?_⟩
    · intro v _
      change L v ≤ L (c, c, c)
      rw [show L (c, c, c) = 1 from he]
      exact hb v.1 v.2.1 v.2.2
    constructor
    · intro hmax
      have hsat : L v = 1 := (hb v.1 v.2.1 v.2.2).antisymm (he ▸ hmax hc)
      exact (Discrete4NHyper.constant_saturated_iff hh v.1 v.2.1 v.2.2 c).mp hsat |>.2.2
    · intro H
      have hsat : L v = 1 := (Discrete4NHyper.constant_saturated_iff hh v.1 v.2.1 v.2.2 c).mpr
        ⟨(hv ⟨0, hh⟩).1, (hv ⟨0, hh⟩).2.1, H⟩
      intro q _
      change L q ≤ L v
      rw [hsat]
      exact hb q.1 q.2.1 q.2.2
  · intro i hi hpos hne S
    obtain ⟨j, hj⟩ := hne
    have hA (s : ℕ) (hs : s ∈ S) :
        ∃ (H : ℚ) (C : ℕ), IsLUB (V s) (H : ℝ) ∧
          ∀ N m, (N, m, s) ∈ D →
            (IsMaxOn (fun v : ℕ × ℕ ↦ L (v.1, v.2, s)) {v | (v.1, v.2, s) ∈ D} (N, m) ↔
              N < C ∧ L (N, m, s) = (H : ℝ)) := by
      have hs' : 1 ≤ s ∧ s ≤ x i := Finset.mem_Icc.mp hs
      have hr : 0 < s := hs'.1
      have hxs (k : Fin h) : s ≤ x k := hs'.2.trans (hi k)
      let T := Finset.univ.sup x
      have hf : ∃ N m, ∀ k, Discrete4NHyper.Compatible N m s (x k) := by
        refine ⟨T, s, fun k ↦ ?_⟩
        have hk : x k ≤ T := Finset.le_sup (Finset.mem_univ k)
        refine ⟨(hxs k).trans hk, le_rfl, ?_⟩
        rw [ite_eq_right hr.ne']
        exact ⟨hxs k, Nat.sub_le_sub_right hk s⟩
      rcases held_stop_complete_search hh x s hf with hz | hc | ⟨_, _, H, C, hsup, _, hmax, _, _⟩
      · exact (hr.ne' hz.1).elim
      · exact (hj.ne ((hc.2.1 i).trans (hc.2.1 j).symm)).elim
      · have hV : (fun v : ℕ × ℕ ↦ L (v.1, v.2, s)) ''
              {v : ℕ × ℕ | (v.1, v.2, s) ∈ D} = V s := by
          ext z
          constructor
          · rintro ⟨⟨N, m⟩, hv, rfl⟩
            exact ⟨N, m, hv, rfl⟩
          · rintro ⟨N, m, hv, rfl⟩
            exact ⟨(N, m), hv, rfl⟩
        exact ⟨H, C, hV ▸ hsup, fun N m hm ↦ hmax (N, m) hm⟩
    have Hchoice (s : ℕ) : ∃ (H : ℚ) (C : ℕ), s ∈ S →
        IsLUB (V s) (H : ℝ) ∧ ∀ N m, (N, m, s) ∈ D →
          (IsMaxOn (fun v : ℕ × ℕ ↦ L (v.1, v.2, s)) {v | (v.1, v.2, s) ∈ D} (N, m) ↔
            N < C ∧ L (N, m, s) = (H : ℝ)) := by
      by_cases hs : s ∈ S
      · obtain ⟨H, C, h⟩ := hA s hs
        exact ⟨H, C, fun _ ↦ h⟩
      · exact ⟨0, 0, fun h ↦ (hs h).elim⟩
    choose H C hSlice using Hchoice
    have hS : S.Nonempty := ⟨1, Finset.mem_Icc.mpr ⟨le_rfl, hpos⟩⟩
    obtain ⟨r, hr, hmax⟩ := Finset.exists_max_image S H hS
    have hstop (v : ℕ × ℕ × ℕ) (hv : v ∈ D) : v.2.2 ∈ S := by
      have h := (hv i).2.2
      have hn : v.2.2 ≠ 0 := by
        intro he
        simp [he, hpos.ne'] at h
      rw [ite_eq_right hn] at h
      exact Finset.mem_Icc.mpr ⟨Nat.pos_of_ne_zero hn, h.1⟩
    have hglob (v : ℕ × ℕ × ℕ) (hv : v ∈ D) : L v ≤ (H r : ℝ) := by
      have hs := hstop v hv
      exact ((hSlice v.2.2 hs).1.1 ⟨v.1, v.2.1, hv, rfl⟩).trans
        (by exact_mod_cast hmax _ hs)
    have hwinners : ∀ v ∈ D, IsMaxOn L D v ↔
        v.2.2 ∈ S ∧ v.1 < C v.2.2 ∧ L v = (H r : ℝ) := by
      intro v hv
      have hs := hstop v hv
      constructor
      · intro hm
        have He : (H r : ℝ) ≤ L v := (hSlice r hr).1.2 (by
          rintro _ ⟨N, m, hw, rfl⟩
          exact hm hw)
        have Hs : IsMaxOn (fun w : ℕ × ℕ ↦ L (w.1, w.2, v.2.2))
            {w | (w.1, w.2, v.2.2) ∈ D} (v.1, v.2.1) :=
          fun q hq ↦ hm (a := (q.1, q.2, v.2.2)) hq
        exact ⟨hs, ((hSlice _ hs).2 _ _ hv).mp Hs |>.1, (hglob v hv).antisymm He⟩
      · rintro ⟨-, -, he⟩ q hq
        exact (hglob q hq).trans_eq he.symm
    refine ⟨H, C, r, hr, hmax, fun s hs ↦ (hSlice s hs).1, hwinners, ?_⟩
    constructor
    · rintro ⟨⟨N, m, s⟩, hv, hw⟩
      obtain ⟨hs, hN, hL⟩ := (hwinners (N, m, s) hv).mp hw
      have HB : L (N, m, s) ≤ (H s : ℝ) := (hSlice s hs).1.1 ⟨N, m, hv, rfl⟩
      have HQ : (H r : ℝ) ≤ (H s : ℝ) := hL ▸ HB
      exact ⟨s, hs, (hmax s hs).antisymm (by exact_mod_cast HQ), N, m, hN, hv, hL⟩
    · rintro ⟨s, hs, -, N, m, hN, hv, hL⟩
      exact ⟨(N, m, s), hv, (hwinners (N, m, s) hv).mpr ⟨hs, hN, hL⟩⟩

end UnivariateML.Base7bNHyperResidue

end
