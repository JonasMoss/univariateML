module

public import UnivariateML.Family.Discrete4Hyper
public import Mathlib.Algebra.Polynomial.Degree.Defs

import Mathlib.Analysis.Polynomial.CauchyBound
import Mathlib.Analysis.Polynomial.Order
import Mathlib.Data.Rat.BigOperators
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Polynomial
open scoped BigOperators

namespace UnivariateML.Discrete5HyperResidue

open Hypergeometric

private noncomputable def falling (P : ℚ[X]) (d : ℕ) : ℚ[X] :=
  ∏ j ∈ Finset.range d, (P - C (j : ℚ))

private theorem eval_falling (P : ℚ[X]) (N M d : ℕ) (hP : P.eval (N : ℚ) = (M : ℚ))
    (hd : d ≤ M) : (falling P d).eval (N : ℚ) = (M.descFactorial d : ℚ) := by
  rw [falling, Polynomial.eval_prod, Nat.descFactorial_eq_prod_range, Nat.cast_prod]
  apply Finset.prod_congr rfl
  intro j hj
  rw [eval_sub, eval_C, hP, Nat.cast_sub (by have := Finset.mem_range.mp hj; omega)]

private theorem mass_falling {m N k c : ℕ} (hc : Compatible m N k c) :
    mass m N k c =
      (k.choose c : ℝ) * m.descFactorial c * (N-m).descFactorial (k-c) /
        N.descFactorial k := by
  rw [mass_choose hc]
  have hd (a b : ℕ) : (a.descFactorial b : ℝ) = b.factorial * (a.choose b : ℝ) := by
    rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
  rw [hd,hd,hd]
  have he : (k.choose c : ℝ) * c.factorial * (k-c).factorial = k.factorial := by
    exact_mod_cast Nat.choose_mul_factorial_mul_factorial hc.2.2.2.1
  have hN : (N.choose k : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.choose_pos hc.2.1)).ne'
  field_simp [hN]
  linear_combination -((m.choose c : ℝ) * ((N-m).choose (k-c) : ℝ)) * he

private noncomputable def numerator {h : ℕ} (k : ℕ) (x : Fin h → ℕ) (a b : ℚ) : ℚ[X] :=
  let P := C a * X + C b
  ∏ i, C (k.choose (x i) : ℚ) * falling P (x i) * falling (X-P) (k-x i)

private noncomputable def denominator (k h : ℕ) : ℚ[X] := (falling X k)^h

private theorem eval_numerator {h : ℕ} (k : ℕ) (x : Fin h → ℕ) (a b : ℚ)
    {m N : ℕ} (hm : (m : ℚ) = a*N+b) (hc : ∀ i, Compatible m N k (x i)) :
    (numerator k x a b).eval (N : ℚ) =
      ∏ i, (k.choose (x i) : ℚ) * m.descFactorial (x i) * (N-m).descFactorial (k-x i) := by
  let P := C a * X + C b
  have hP : P.eval (N : ℚ) = m := by simp [P,hm]
  unfold numerator
  rw [eval_prod]
  apply Finset.prod_congr rfl
  intro i _
  have hi := hc i
  have he : (X-P).eval (N : ℚ) = ((N-m : ℕ) : ℚ) := by
    rw [eval_sub, eval_X, hP, Nat.cast_sub hi.1]
  have hkm : k-x i ≤ N-m := by rcases hi with ⟨_,_,_,_,_⟩; omega
  change (C (k.choose (x i) : ℚ) * falling P (x i) * falling (X-P) (k-x i)).eval (N : ℚ) = _
  simp only [eval_mul,eval_C,eval_falling P N m (x i) hP hi.2.2.1,
    eval_falling (X-P) N (N-m) (k-x i) he hkm]

private theorem eval_denominator (k h N : ℕ) (hk : k ≤ N) :
    (denominator k h).eval (N : ℚ) = (N.descFactorial k : ℚ)^h := by
  simp only [denominator,eval_pow,eval_falling X N N k (eval_X (x := (N : ℚ))) hk]

/-- Clearing the positive falling-factorial denominator gives an explicit
rational polynomial for the signed comparison on any affine composition ray.

## Deviations

This proves the algebraic comparison after the offsets have been specified.
The eventual first-crossing bracketing that selects those offsets is separate. -/
private theorem affine_face_polynomial {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (a b face : ℚ) :
    ∃ P : ℚ[X], ∀ m N : ℕ, (m : ℚ) = a*N+b →
      (∀ i, Compatible m N k (x i)) →
      ((0 < P.eval (N : ℚ) ↔ (face : ℝ) < likelihood m N k x) ∧
       (P.eval (N : ℚ) = 0 ↔ likelihood m N k x = (face : ℝ)) ∧
       (P.eval (N : ℚ) < 0 ↔ likelihood m N k x < (face : ℝ))) := by
  refine ⟨numerator k x a b - C face * denominator k h, fun m N hm hc ↦ ?_⟩
  have hk := (hc ⟨0,hh⟩).2.1
  let D : ℝ := (N.descFactorial k : ℝ)^h
  have hD : 0 < D := by
    have hd : 0 < N.descFactorial k := Nat.descFactorial_pos.mpr hk
    exact pow_pos (Nat.cast_pos.mpr hd) _
  have hL : likelihood m N k x =
      (∏ i, (k.choose (x i) : ℝ) * m.descFactorial (x i) *
        (N-m).descFactorial (k-x i)) / D := by
    unfold likelihood
    simp only [mass_falling (hc _), Finset.prod_div_distrib, Finset.prod_const,
      Finset.card_univ, Fintype.card_fin, D]
  let P := numerator k x a b - C face * denominator k h
  change (0 < P.eval (N : ℚ) ↔ _) ∧ (P.eval (N : ℚ) = 0 ↔ _) ∧ (P.eval (N : ℚ) < 0 ↔ _)
  have he : ((P.eval (N : ℚ) : ℚ) : ℝ) = (likelihood m N k x - (face : ℝ))*D := by
    simp only [P,eval_sub,eval_mul,eval_C,eval_numerator k x a b hm hc,
      eval_denominator k h N hk, Rat.cast_sub,Rat.cast_mul,Rat.cast_pow,Rat.cast_natCast,
      Rat.cast_prod,hL,D]
    field_simp [show (N.descFactorial k : ℝ) ≠ 0 from
      (Nat.cast_pos.mpr (Nat.descFactorial_pos.mpr hk)).ne']
  constructor
  · rw [← Rat.cast_pos (K := ℝ), he, mul_pos_iff_of_pos_right hD, sub_pos]
  constructor
  · rw [← Rat.cast_eq_zero (α := ℝ), he, mul_eq_zero, or_iff_left hD.ne', sub_eq_zero]
  · rw [← Rat.cast_lt_zero (K := ℝ), he]
    simpa only [zero_mul, sub_neg] using
      (mul_lt_mul_iff_left₀ hD : (likelihood m N k x - (face : ℝ))*D < 0*D ↔ _ )

private theorem polynomial_tail_sign (p : ℚ[X]) :
    ∃ B : ℕ, ∀ N ≥ B,
      (0 < p.eval (N : ℚ) ↔ 0 < p.leadingCoeff) ∧
      (p.eval (N : ℚ) = 0 ↔ p = 0) ∧
      (p.eval (N : ℚ) < 0 ↔ p.leadingCoeff < 0) := by
  by_cases hp : p = 0
  · subst p
    exact ⟨0,fun _ _ ↦ by simp⟩
  let P := p.map (Rat.castHom ℝ)
  have hP : P ≠ 0 := map_ne_zero hp
  obtain ⟨B,hB⟩ := exists_nat_gt (P.cauchyBound : ℝ)
  refine ⟨B,fun N hN ↦ ?_⟩
  have hroot (y : ℝ) (hy : P.IsRoot y) : y < (N : ℝ) := by
    have H : ‖y‖ < (P.cauchyBound : ℝ) := hy.norm_lt_cauchyBound hP
    exact (le_abs_self y).trans_lt
      ((by simpa only [Real.norm_eq_abs] using H : |y| < (P.cauchyBound : ℝ)).trans
        (hB.trans_le (by exact_mod_cast hN)))
  have hcoeff : P.leadingCoeff = (p.leadingCoeff : ℝ) := by simp [P]
  have hev : P.eval (N : ℝ) = ((p.eval (N : ℚ) : ℚ) : ℝ) := by
    change P.eval (N : ℝ) = Rat.castHom ℝ (p.eval (N : ℚ))
    simpa only [map_natCast, P] using
      (eval_map_apply (p := p) (f := Rat.castHom ℝ) (N : ℚ))
  rcases lt_or_gt_of_ne (leadingCoeff_ne_zero.mpr hp) with hneg | hpos
  · have h : P.eval (N : ℝ) < 0 := eval_lt_zero_of_roots_lt_of_leadingCoeff_nonpos (P := P)
      hroot (by rw [hcoeff]; exact_mod_cast hneg.le)
    have he : p.eval (N : ℚ) < 0 := by rw [hev] at h; exact_mod_cast h
    simp [hp,hneg,he,hneg.not_gt,he.not_gt,he.ne]
  · have h : 0 < P.eval (N : ℝ) := zero_lt_eval_of_roots_lt_of_leadingCoeff_nonneg (P := P)
      hroot (by rw [hcoeff]; exact_mod_cast hpos.le)
    have he : 0 < p.eval (N : ℚ) := by rw [hev] at h; exact_mod_cast h
    simp [hp,hpos,he,hpos.not_gt,he.not_gt,he.ne']

/-- A rational affine composition ray has eventually constant comparison
against any rational face. The identically equal tail occurs exactly when
the explicit cleared polynomial is zero. This is the comparison stage of
the residue algorithm once its candidate offsets are available. -/
theorem affine_face_tail {h : ℕ} (hh : 0 < h) (k : ℕ) (x : Fin h → ℕ)
    (a b face : ℚ) :
    ∃ (P : ℚ[X]) (B : ℕ), ∀ m N : ℕ, B ≤ N → (m : ℚ) = a*N+b →
      (∀ i, Compatible m N k (x i)) →
      (((face : ℝ) < likelihood m N k x ↔ 0 < P.leadingCoeff) ∧
       (likelihood m N k x = (face : ℝ) ↔ P = 0) ∧
       (likelihood m N k x < (face : ℝ) ↔ P.leadingCoeff < 0)) := by
  obtain ⟨P,hP⟩ := affine_face_polynomial hh k x a b face
  obtain ⟨B,hB⟩ := polynomial_tail_sign P
  refine ⟨P,B,fun m N hN hm hc ↦ ?_⟩
  have hp := hP m N hm hc
  have hb := hB N hN
  exact ⟨hp.1.symm.trans hb.1,hp.2.1.symm.trans hb.2.1,hp.2.2.symm.trans hb.2.2⟩

/-- The affine-ray comparison stage for fixed marks, by design symmetry. -/
theorem affine_face_tail_held_mark {h : ℕ} (hh : 0 < h) (m : ℕ) (x : Fin h → ℕ)
    (a b face : ℚ) :
    ∃ (P : ℚ[X]) (B : ℕ), ∀ k N : ℕ, B ≤ N → (k : ℚ) = a*N+b →
      (∀ i, Compatible m N k (x i)) →
      (((face : ℝ) < likelihood m N k x ↔ 0 < P.leadingCoeff) ∧
       (likelihood m N k x = (face : ℝ) ↔ P = 0) ∧
       (likelihood m N k x < (face : ℝ) ↔ P.leadingCoeff < 0)) := by
  obtain ⟨P,B,hP⟩ := affine_face_tail hh m x a b face
  refine ⟨P,B,fun k N hN hk hc ↦ ?_⟩
  have hc' (i : Fin h) : Compatible k N m (x i) := by
    have hi := hc i
    unfold Compatible at hi ⊢
    omega
  simpa only [likelihood_swap k N m x] using hP k N hN hk hc'

end UnivariateML.Discrete5HyperResidue

end
