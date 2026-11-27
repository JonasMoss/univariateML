module

public import UnivariateML.Family.AE1SSTD
public import UnivariateML.Family.TwoPieceStudent

import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Two-piece Student with free shape

## Deviations

The four-level result compares finite candidates. Three balanced levels,
nonattainment at smaller sizes and the reflected examples are separate.
-/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap Real
open scoped Topology

namespace UnivariateML.AE4SSTD

noncomputable section

variable {n : ℕ}

private def score (ν : ℝ) (p : ℝ × ℝ × ℝ) (t : ℝ) : ℝ :=
  (ν + 1) * (max (p.1 - t) 0 / (p.2.1^2 + (max (p.1 - t) 0)^2) -
    max (t - p.1) 0 / (p.2.2^2 + (max (t - p.1) 0)^2))

private def sampleDeriv (w : Fin n → ℕ) (ν : ℝ) (p : ℝ × ℝ × ℝ)
    (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score ν p (x i)) • proj i

private theorem square_max_deriv (z : ℝ) :
    HasDerivAt (fun t : ℝ ↦ (max t 0)^2) (2 * max z 0) z := by
  have hs (t : ℝ) : HasDerivAt (fun u : ℝ ↦ u^2) (2*t) t := by
    simpa only [Pi.pow_def, id_eq, Nat.cast_ofNat, pow_one, mul_one, Nat.reduceSub] using
      (hasDerivAt_id t).pow 2
  rcases lt_trichotomy z 0 with h | rfl | h
  · rw [max_eq_right h.le, mul_zero]
    apply (hasDerivAt_const z (0 : ℝ)).congr_of_eventuallyEq
    filter_upwards [Iio_mem_nhds h] with t ht
    simp [max_eq_right ht.le]
  · simp only [max_self, mul_zero]
    have h₁ : HasDerivWithinAt (fun t : ℝ ↦ (max t 0)^2) 0 (Ici 0) 0 := by
      apply (by simpa using (hs 0).hasDerivWithinAt (s := Ici 0) :
        HasDerivWithinAt (fun u : ℝ ↦ u^2) 0 (Ici 0) 0).congr_of_mem _ (by simp)
      intro t ht
      rw [max_eq_left ht]
    have h₂ : HasDerivWithinAt (fun t : ℝ ↦ (max t 0)^2) 0 (Iic 0) 0 := by
      apply (hasDerivAt_const 0 (0 : ℝ)).hasDerivWithinAt.congr_of_mem _ (by simp)
      intro t ht
      simp [max_eq_right ht]
    exact (h₁.union h₂).hasDerivAt (by simp)
  · rw [max_eq_left h.le]
    apply (hs z).congr_of_eventuallyEq
    filter_upwards [Ioi_mem_nhds h] with t ht
    rw [max_eq_left ht.le]

private theorem term_deriv (ν : ℝ) (p : ℝ × ℝ × ℝ) (t : ℝ)
    (ha : 0 < p.2.1) (hb : 0 < p.2.2) :
    HasDerivAt (fun u ↦ -log (p.2.1 + p.2.2) - (ν + 1) / 2 *
      (log (1 + (max (p.1 - u) 0 / p.2.1)^2) +
       log (1 + (max (u - p.1) 0 / p.2.2)^2))) (score ν p t) t := by
  have aux (s z : ℝ) (hs : 0 < s) :
      HasDerivAt (fun y ↦ log (1 + (max y 0 / s)^2))
        (2 * max z 0 / (s^2 + (max z 0)^2)) z := by
    have h := ((hasDerivAt_const z (1 : ℝ)).add
        ((square_max_deriv z).div_const (s^2))).log
      (show 1 + (max z 0)^2 / s^2 ≠ 0 by positivity)
    convert h using 1
    · ext y
      simp [div_pow]
    · change 2 * max z 0 / (s^2 + (max z 0)^2) =
        (0 + 2 * max z 0 / s^2) / (1 + (max z 0)^2 / s^2)
      have hden : 0 < 1 + (max z 0)^2 / s^2 := by positivity
      have hsum : 0 < s^2 + (max z 0)^2 := by positivity
      apply (div_eq_div_iff hsum.ne' hden.ne').mpr
      field_simp [hs.ne']
      ring
  have hl := (aux p.2.1 (p.1 - t) ha).comp t ((hasDerivAt_id t).const_sub p.1)
  have hr := (aux p.2.2 (t - p.1) hb).comp t ((hasDerivAt_id t).sub_const p.1)
  convert (((hl.add hr).const_mul ((ν + 1) / 2)).const_sub
    (-log (p.2.1 + p.2.2))) using 1
  · rfl
  · unfold score
    ring

private theorem same_shape_join {ν : ℝ} (hν : 2 < ν) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    {p q : ℝ × ℝ × ℝ} (hp : 0 < p.2.1 ∧ 0 < p.2.2) (hq : 0 < q.2.1 ∧ 0 < q.2.2)
    {x : Fin n → ℝ} (hx : Function.Injective x) (hn : 3 ≤ n)
    (hc : p.1 = q.1) (he : ∀ i, score ν p (x i) = score ν q (x i))
    (hval : AE1SSTD.fixedShapeLogLik w ν p x = AE1SSTD.fixedShapeLogLik w ν q x) : p = q := by
  have hν' : ν + 1 ≠ 0 := by linarith
  have width {a b r : ℝ} (ha : 0 < a) (hb : 0 < b) (hr : 0 < r)
      (e : r / (a^2+r^2) = r / (b^2+r^2)) : a = b := by
    have hh := (div_eq_div_iff (by positivity) (by positivity)).mp e
    have hz : r * (b^2-a^2) = 0 := by linear_combination hh
    have hsq := sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_left hr.ne')
    nlinarith
  have hl (i : Fin n) (h : x i < p.1) : p.2.1 = q.2.1 := by
    have hh := he i
    simp only [score, ← hc, max_eq_left (sub_nonneg.mpr h.le),
      max_eq_right (sub_nonpos.mpr h.le), zero_div, sub_zero] at hh
    exact width hp.1 hq.1 (sub_pos.mpr h) (mul_left_cancel₀ hν' hh)
  have hr (i : Fin n) (h : p.1 < x i) : p.2.2 = q.2.2 := by
    have hh := he i
    simp only [score, ← hc, max_eq_left (sub_nonneg.mpr h.le),
      max_eq_right (sub_nonpos.mpr h.le), zero_div, zero_sub] at hh
    exact width hp.2 hq.2 (sub_pos.mpr h) (neg_inj.mp (mul_left_cancel₀ hν' hh))
  have left (i : Fin n) : max (p.1 - x i) 0 / p.2.1 = max (q.1 - x i) 0 / q.2.1 := by
    rw [← hc]
    by_cases h : x i < p.1
    · rw [hl i h]
    · simp [max_eq_right (sub_nonpos.mpr (le_of_not_gt h))]
  have right (i : Fin n) : max (x i - p.1) 0 / p.2.2 = max (x i - q.1) 0 / q.2.2 := by
    rw [← hc]
    by_cases h : p.1 < x i
    · rw [hr i h]
    · simp [max_eq_right (sub_nonpos.mpr (le_of_not_gt h))]
  have hwpos : 0 < ∑ i, (w i : ℝ) := by
    let _ : Nonempty (Fin n) := ⟨⟨0, by omega⟩⟩
    exact Finset.sum_pos (fun i _ ↦ by exact_mod_cast hw i) Finset.univ_nonempty
  unfold AE1SSTD.fixedShapeLogLik at hval
  simp only [mul_sub, Finset.sum_sub_distrib, ← Finset.sum_mul, left, right] at hval
  have hlogs := neg_inj.mp (mul_left_cancel₀ hwpos.ne' (sub_left_injective hval))
  have hsum := strictMonoOn_log.injOn (add_pos hp.1 hp.2) (add_pos hq.1 hq.2) hlogs
  have hsome : ∃ i, x i ≠ p.1 := by
    by_contra! h
    let i : Fin n := ⟨0, by omega⟩
    let j : Fin n := ⟨1, by omega⟩
    have hij : i ≠ j := by simp [i, j]
    exact hij (hx ((h i).trans (h j).symm))
  obtain ⟨i, hi⟩ := hsome
  rcases hi.lt_or_gt with hi | hi
  · exact Prod.ext hc (Prod.ext (hl i hi) (by linarith [hl i hi]))
  · exact Prod.ext hc (Prod.ext (by linarith [hr i hi]) (hr i hi))

private def cubic (u v A B d r : ℝ) :=
  (v-u)*r^3 + d*(v-2*u)*r^2 + (v*A-u*B-u*d^2)*r + v*A*d

private theorem cross {u v A B d r : ℝ} (hA : 0 < A) (hB : 0 < B)
    (he : u * (r / (A + r ^ 2)) = v * ((r + d) / (B + (r + d) ^ 2))) :
    cubic u v A B d r = 0 := by
  unfold cubic
  rw [← mul_div_assoc, ← mul_div_assoc] at he
  have h := (div_eq_div_iff (by positivity) (by positivity)).mp he
  linear_combination -h

private theorem one_exterior {u v A B d r s : ℝ} (hv : 0 < v) (hvu : v ≤ u)
    (hA : 0 < A) (hd : 0 < d) (hr : 0 < r) (hs : 0 < s)
    (er : cubic u v A B d r = 0) (es : cubic u v A B d s = 0) : r = s := by
  have h : (r-s)*((v-u)*r*s*(r+s)+d*(v-2*u)*r*s-v*A*d) = 0 := by
    unfold cubic at er es
    linear_combination s*er-r*es
  have h₁ : (v-u)*r*s*(r+s) ≤ 0 := by
    exact mul_nonpos_of_nonpos_of_nonneg
      (mul_nonpos_of_nonpos_of_nonneg
        (mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr hvu) hr.le) hs.le) (by positivity)
  have h₂ : d*(v-2*u)*r*s ≤ 0 := by
    exact mul_nonpos_of_nonpos_of_nonneg
      (mul_nonpos_of_nonpos_of_nonneg
        (mul_nonpos_of_nonneg_of_nonpos hd.le (by linarith)) hr.le) hs.le
  have h₃ : 0 < v*A*d := by positivity
  exact sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_right (by linarith))

private theorem two_exterior {u v A B d r s t : ℝ} (hu : 0 < u) (huv : u < v)
    (hA : 0 < A) (hd : 0 < d) (hr : 0 < r) (hs : 0 < s) (ht : 0 < t)
    (hrs : r ≠ s) (hrt : r ≠ t)
    (er : cubic u v A B d r = 0) (es : cubic u v A B d s = 0)
    (et : cubic u v A B d t = 0) : s = t := by
  have aux (y : ℝ) (hy : r ≠ y) (ey : cubic u v A B d y = 0) :
      (v-u)*r*y*(r+y)+d*(v-2*u)*r*y-v*A*d = 0 := by
    have h : (r-y)*((v-u)*r*y*(r+y)+d*(v-2*u)*r*y-v*A*d) = 0 := by
      unfold cubic at er ey
      linear_combination y*er-r*ey
    exact (mul_eq_zero.mp h).resolve_left (sub_ne_zero.mpr hy)
  have h : (s-t)*((v-u)*r*s*t+v*A*d) = 0 := by
    linear_combination t*(aux s hrs es)-s*(aux t hrt et)
  have hpos : 0 < (v-u)*r*s*t+v*A*d :=
    add_pos (by positivity) (by have : 0 < v := hu.trans huv; positivity)
  exact sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_right hpos.ne')

private theorem left_score (ν : ℝ) (p : ℝ × ℝ × ℝ) {t : ℝ} (h : t ≤ p.1) :
    score ν p t = (ν+1)*((p.1-t)/(p.2.1^2+(p.1-t)^2)) := by
  simp [score, max_eq_left (sub_nonneg.mpr h), max_eq_right (sub_nonpos.mpr h)]

private theorem right_score (ν : ℝ) (p : ℝ × ℝ × ℝ) {t : ℝ} (h : p.1 ≤ t) :
    score ν p t = -((ν+1)*((t-p.1)/(p.2.2^2+(t-p.1)^2))) := by
  simp [score, max_eq_left (sub_nonneg.mpr h), max_eq_right (sub_nonpos.mpr h)]

private theorem ordered_levels {x : Fin n → ℝ} (hx : Function.Injective x) (hn : 4 ≤ n) :
    ∃ a b c d : ℝ, a < b ∧ b < c ∧ c < d ∧
      a ∈ Set.range x ∧ b ∈ Set.range x ∧ c ∈ Set.range x ∧ d ∈ Set.range x := by
  classical
  let S := Finset.univ.image x
  have hS : 4 ≤ S.card := by simpa [S, Finset.card_image_of_injective _ hx] using hn
  let f := S.orderEmbOfCardLe hS
  have mem (i : Fin 4) : f i ∈ Set.range x := by
    have h := Finset.orderEmbOfCardLe_mem S hS i
    simpa [S, f] using h
  exact ⟨f 0, f 1, f 2, f 3, f.strictMono (by decide), f.strictMono (by decide),
    f.strictMono (by decide), mem 0, mem 1, mem 2, mem 3⟩

private theorem distinct_join_impossible {ν η : ℝ} (hν : 2 < ν) (hη : 2 < η)
    {p q : ℝ × ℝ × ℝ} (hp : 0 < p.2.1 ∧ 0 < p.2.2) (hq : 0 < q.2.1 ∧ 0 < q.2.2)
    (hj : p.1 < q.1) {a b c d : ℝ} (hab : a < b) (hbc : b < c) (hcd : c < d)
    (he : ∀ t ∈ ({a,b,c,d} : Finset ℝ), score ν p t = score η q t) : False := by
  have hu : 0 < ν+1 := by linarith
  have hv : 0 < η+1 := by linarith
  have side (t : ℝ) (e : score ν p t = score η q t) : t < p.1 ∨ q.1 < t := by
    by_contra! h
    rw [right_score _ _ h.1, left_score _ _ h.2] at e
    rcases eq_or_lt_of_le h.1 with hh | hh
    · rw [← hh, sub_self, zero_div, mul_zero, neg_zero] at e
      exact (mul_pos hv (div_pos (sub_pos.mpr hj) (by positivity))).ne' e.symm
    · have hl : 0 < (ν+1)*((t-p.1)/(p.2.2^2+(t-p.1)^2)) :=
        mul_pos hu (div_pos (sub_pos.mpr hh) (by positivity))
      have hr : 0 ≤ (η+1)*((q.1-t)/(q.2.1^2+(q.1-t)^2)) :=
        mul_nonneg hv.le (div_nonneg (sub_nonneg.mpr h.2) (by positivity))
      linarith
  have left (t : ℝ) (ht : t < p.1) (e : score ν p t = score η q t) :
      cubic (ν+1) (η+1) (p.2.1^2) (q.2.1^2) (q.1-p.1) (p.1-t) = 0 := by
    apply cross (sq_pos_of_pos hp.1) (sq_pos_of_pos hq.1)
    rw [show p.1-t+(q.1-p.1) = q.1-t by ring]
    simpa only [left_score ν p ht.le, left_score η q (ht.trans hj).le] using e
  have right (t : ℝ) (ht : q.1 < t) (e : score ν p t = score η q t) :
      cubic (η+1) (ν+1) (q.2.2^2) (p.2.2^2) (q.1-p.1) (t-q.1) = 0 := by
    apply cross (sq_pos_of_pos hq.2) (sq_pos_of_pos hp.2)
    rw [show t-q.1+(q.1-p.1) = t-p.1 by ring]
    rw [right_score ν p (hj.trans ht).le, right_score η q ht.le] at e
    exact neg_inj.mp e.symm
  have ea := he a (by simp)
  have eb := he b (by simp)
  have ec := he c (by simp)
  have ed := he d (by simp)
  rcases le_or_gt η ν with h | h
  · rcases side b eb with hb | hb
    · have ha := hab.trans hb
      have hh := one_exterior hv (by linarith) (sq_pos_of_pos hp.1) (sub_pos.mpr hj)
        (sub_pos.mpr ha) (sub_pos.mpr hb) (left a ha ea) (left b hb eb)
      linarith
    · have hc := hb.trans hbc
      have hd := hc.trans hcd
      rcases h.eq_or_lt with h | h
      · have hh := one_exterior hu (by linarith) (sq_pos_of_pos hq.2) (sub_pos.mpr hj)
          (sub_pos.mpr hb) (sub_pos.mpr hc) (right b hb eb) (right c hc ec)
        linarith
      · have hh := two_exterior hv (by linarith) (sq_pos_of_pos hq.2) (sub_pos.mpr hj)
          (sub_pos.mpr hb) (sub_pos.mpr hc) (sub_pos.mpr hd)
          (by intro hh; linarith) (by intro hh; linarith)
          (right b hb eb) (right c hc ec) (right d hd ed)
        linarith
  · rcases side c ec with hc | hc
    · have hb := hbc.trans hc
      have ha := hab.trans hb
      have hh := two_exterior hu (by linarith) (sq_pos_of_pos hp.1) (sub_pos.mpr hj)
        (sub_pos.mpr ha) (sub_pos.mpr hb) (sub_pos.mpr hc)
        (by intro hh; linarith) (by intro hh; linarith)
        (left a ha ea) (left b hb eb) (left c hc ec)
      linarith
    · have hd := hc.trans hcd
      have hh := one_exterior hu (by linarith) (sq_pos_of_pos hq.2) (sub_pos.mpr hj)
        (sub_pos.mpr hc) (sub_pos.mpr hd) (right c hc ec) (right d hd ed)
      linarith

private theorem same_join_shape {ν η A B c a b : ℝ} (hA : 0 < A) (hB : 0 < B)
    (ha : a ≠ c) (hb : b ≠ c) (hrad : (a - c) ^ 2 ≠ (b - c) ^ 2)
    (ea : (ν + 1) * ((a - c) / (A + (a - c) ^ 2)) = (η + 1) * ((a - c) / (B + (a - c) ^ 2)))
    (eb : (ν + 1) * ((b - c) / (A + (b - c) ^ 2)) = (η + 1) * ((b - c) / (B + (b - c) ^ 2))) : ν = η := by
  have aux (t : ℝ) (ht : t ≠ c)
      (e : (ν+1)*((t-c)/(A+(t-c)^2)) = (η+1)*((t-c)/(B+(t-c)^2))) :
      (ν+1)*(B+(t-c)^2) = (η+1)*(A+(t-c)^2) := by
    rw [← mul_div_assoc, ← mul_div_assoc] at e
    have hh := (div_eq_div_iff (by positivity) (by positivity)).mp e
    apply mul_left_cancel₀ (sub_ne_zero.mpr ht)
    linear_combination hh
  have hh : (ν-η)*((a-c)^2-(b-c)^2) = 0 := by
    linear_combination aux a ha ea - aux b hb eb
  exact sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_right (sub_ne_zero.mpr hrad))

private theorem shape_and_join {ν η : ℝ} (hν : 2 < ν) (hη : 2 < η)
    {p q : ℝ × ℝ × ℝ} (hp : 0 < p.2.1 ∧ 0 < p.2.2) (hq : 0 < q.2.1 ∧ 0 < q.2.2)
    {x : Fin n → ℝ} (hx : Function.Injective x) (hn : 4 ≤ n)
    (he : ∀ i, score ν p (x i) = score η q (x i)) : ν = η ∧ p.1 = q.1 := by
  obtain ⟨a,b,c,d,hab,hbc,hcd,ha,hb,hc,hd⟩ := ordered_levels hx hn
  have e (t : ℝ) (ht : t ∈ ({a,b,c,d} : Finset ℝ)) : score ν p t = score η q t := by
    have hr : t ∈ Set.range x := by
      simp only [Finset.mem_insert, Finset.mem_singleton] at ht
      rcases ht with rfl | rfl | rfl | rfl <;> assumption
    obtain ⟨i, rfl⟩ := hr
    exact he i
  have hj : p.1 = q.1 := by
    rcases lt_trichotomy p.1 q.1 with h | h | h
    · exact False.elim (distinct_join_impossible hν hη hp hq h hab hbc hcd e)
    · exact h
    · exact False.elim (distinct_join_impossible hη hν hq hp h hab hbc hcd
        (fun t ht ↦ (e t ht).symm))
  refine ⟨?_, hj⟩
  by_cases h : b < p.1
  · have h' := hab.trans h
    have ea := e a (by simp)
    have eb := e b (by simp)
    rw [left_score ν p h'.le, left_score η q (hj ▸ h'.le), ← hj] at ea
    rw [left_score ν p h.le, left_score η q (hj ▸ h.le), ← hj] at eb
    have convertSign (t u : ℝ) :
        (p.1-t)/(u+(p.1-t)^2) = -( (t-p.1)/(u+(t-p.1)^2)) := by
      rw [show p.1-t = -(t-p.1) by ring, neg_sq, neg_div]
    simp only [convertSign, mul_neg, neg_inj] at ea eb
    have hsq : (b-p.1)^2 < (a-p.1)^2 := by
      apply sq_lt_sq.mpr
      rw [abs_of_neg (sub_neg.mpr h), abs_of_neg (sub_neg.mpr h')]
      linarith
    exact same_join_shape (sq_pos_of_pos hp.1) (sq_pos_of_pos hq.1) h'.ne h.ne
      hsq.ne' ea eb
  · have h' : p.1 < c := (le_of_not_gt h).trans_lt hbc
    have h'' := h'.trans hcd
    have ec := e c (by simp)
    have ed := e d (by simp)
    rw [right_score ν p h'.le, right_score η q (hj ▸ h'.le), ← hj] at ec
    rw [right_score ν p h''.le, right_score η q (hj ▸ h''.le), ← hj] at ed
    have hsq : (c-p.1)^2 < (d-p.1)^2 := by
      apply sq_lt_sq.mpr
      rw [abs_of_pos (sub_pos.mpr h'), abs_of_pos (sub_pos.mpr h'')]
      linarith
    exact same_join_shape (sq_pos_of_pos hp.2) (sq_pos_of_pos hq.2) h'.ne' h''.ne'
      hsq.ne (neg_inj.mp ec) (neg_inj.mp ed)

/-- Shape, join, left denominator width, right denominator width. -/
abbrev Parameter := ℝ × (ℝ × ℝ × ℝ)

/-- Weighted free-shape likelihood after taking logarithms. -/
def logLik (w : Fin n → ℕ) (p : Parameter) (x : Fin n → ℝ) : ℝ :=
  (∑ i, (w i : ℝ)) * TwoPieceStudent.logPeak p.1 +
    AE1SSTD.fixedShapeLogLik w p.1 p.2 x

private theorem log_gamma_continuous {P : Type*} [TopologicalSpace P]
    {f : P → ℝ} (hf : Continuous f) (hp : ∀ p, 0 < f p) :
    Continuous (fun p ↦ log (Gamma (f p))) := by
  apply Continuous.log _ (fun p ↦ (Gamma_pos_of_pos (hp p)).ne')
  rw [continuous_iff_continuousAt]
  intro p
  exact ((differentiableAt_Gamma (fun m ↦ by
    have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
    linarith [hp p])).continuousAt).comp hf.continuousAt

private theorem chart (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set Parameter) (hA : IsLocallyClosed A)
    (hparam : ∀ p ∈ A, 2 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q →
        ∀ i, score p.1 p.2 (x i) = score q.1 q.2 (x i) := by
  let _ : LocallyCompactSpace A := hA.locallyCompactSpace
  let L (p : A) := logLik w p.val
  let D (p : A) := sampleDeriv w p.val.1 p.val.2
  have hwidth (p : A) := (hparam p.val p.property).2
  have hν (p : A) := (hparam p.val p.property).1
  have hpeak : Continuous (fun z : A × (Fin n → ℝ) ↦ TwoPieceStudent.logPeak z.1.val.1) := by
    have hq : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.1) := by fun_prop
    have h₁ := log_gamma_continuous ((hq.add continuous_const).div_const 2)
      (fun z ↦ show 0 < (z.1.val.1 + 1) / 2 by linarith [hν z.1])
    have h₂ := log_gamma_continuous (hq.div_const 2)
      (fun z ↦ show 0 < z.1.val.1 / 2 by linarith [hν z.1])
    have h₃ := (hq.mul continuous_const).log
      (fun z ↦ (mul_pos (lt_trans (by norm_num) (hν z.1)) pi_pos).ne')
    have h₄ := hq.log (fun z ↦ (lt_trans (by norm_num) (hν z.1)).ne')
    exact (continuous_const.add ((h₁.sub h₂).sub (h₃.div_const 2))).add (h₄.div_const 2)
  have hL : Continuous (fun z : A × (Fin n → ℝ) ↦ L z.1 z.2) := by
    unfold L logLik AE1SSTD.fixedShapeLogLik
    apply (hpeak.const_mul _).add
    apply continuous_finsetSum Finset.univ
    intro i _
    have ha : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2.1) := by fun_prop
    have hb : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2.2) := by fun_prop
    have hl : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.1.val.2.1 - z.2 i) 0 / z.1.val.2.2.1) :=
      (by fun_prop : Continuous fun z : A × (Fin n → ℝ) ↦ max (z.1.val.2.1 - z.2 i) 0).div
        ha (fun z ↦ (hwidth z.1).1.ne')
    have hr : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.2.1) 0 / z.1.val.2.2.2) :=
      (by fun_prop : Continuous fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.2.1) 0).div
        hb (fun z ↦ (hwidth z.1).2.ne')
    have hl' := ((hl.pow 2).const_add 1).log (fun _ ↦
      (add_pos_of_pos_of_nonneg one_pos (sq_nonneg _)).ne')
    have hr' := ((hr.pow 2).const_add 1).log (fun _ ↦
      (add_pos_of_pos_of_nonneg one_pos (sq_nonneg _)).ne')
    have hnorm := ((ha.add hb).log (fun z ↦ (add_pos (hwidth z.1).1 (hwidth z.1).2).ne')).neg
    have hs : Continuous (fun z : A × (Fin n → ℝ) ↦ (z.1.val.1 + 1) / 2) := by fun_prop
    exact (hnorm.sub (hs.mul (hl'.add hr'))).const_mul (w i : ℝ)
  have hD : Continuous (fun z : A × (Fin n → ℝ) ↦ D z.1 z.2) := by
    unfold D sampleDeriv score
    apply continuous_finsetSum Finset.univ
    intro i _
    have hl : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.1.val.2.1 - z.2 i) 0) := by fun_prop
    have hr : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.2.1) 0) := by fun_prop
    have ha : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2.1) := by fun_prop
    have hb : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2.2) := by fun_prop
    have hl' := hl.div ((ha.pow 2).add (hl.pow 2)) (fun z ↦
      (add_pos_of_pos_of_nonneg (sq_pos_of_pos (hwidth z.1).1) (sq_nonneg _)).ne')
    have hr' := hr.div ((hb.pow 2).add (hr.pow 2)) (fun z ↦
      (add_pos_of_pos_of_nonneg (sq_pos_of_pos (hwidth z.1).2) (sq_nonneg _)).ne')
    have hs : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.1 + 1) := by fun_prop
    exact ((hs.mul (hl'.sub hr')).const_mul (w i : ℝ)).smul continuous_const
  have hd (p : A) (x : Fin n → ℝ) : HasFDerivAt (L p) (D p x) x := by
    unfold L D logLik AE1SSTD.fixedShapeLogLik sampleDeriv
    apply HasFDerivAt.const_add
    apply HasFDerivAt.fun_sum
    intro i _
    let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have h := ((term_deriv p.val.1 p.val.2 (x i) (hwidth p).1
      (hwidth p).2).const_mul (w i : ℝ)).comp_hasFDerivAt x (f.hasFDerivAt (x := x))
    exact h
  have hae := UniqueAE.ae_winners_derivative (μ := volume) (S := (univ : Set (A × (Fin n → ℝ))))
    isOpen_univ hL.continuousOn hD.continuousOn (fun p x _ ↦ hd p x)
  filter_upwards [hae] with x hx p hp q hq hmp hmq i
  have he := hx ⟨p, hp⟩ ⟨q, hq⟩ ⟨mem_univ _, fun r _ ↦ hmp r.property⟩
     ⟨mem_univ _, fun r _ ↦ hmq r.property⟩
  have eval (r : A) :
      D r x (Pi.single i 1) = (w i : ℝ) * score r.val.1 r.val.2 (x i) := by
    simp only [D, sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hj
      simp [Pi.single_eq_of_ne hj]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) he
  rw [eval, eval] at h
  exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') h

/-- Four distinct levels suffice for a.e. uniqueness on every locally closed
finite-parameter restriction of the two-piece Student chart. -/
theorem ae_unique_four_levels (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 4 ≤ n)
    (A : Set Parameter) (hA : IsLocallyClosed A)
    (hparam : ∀ p ∈ A, 2 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ logLik w r x) A p →
        IsMaxOn (fun r ↦ logLik w r x) A q → p = q := by
  filter_upwards [chart w hw A hA hparam] with x hx hi p hp q hq hmp hmq
  have hν (p : A) := (hparam p.val p.property).1
  have hwidth (p : A) := (hparam p.val p.property).2
  have he' := hx p hp q hq hmp hmq
  obtain ⟨hs, hj⟩ := shape_and_join (hν ⟨p,hp⟩) (hν ⟨q,hq⟩)
    (hwidth ⟨p,hp⟩) (hwidth ⟨q,hq⟩) hi hn he'
  change p.1 = q.1 at hs
  change p.2.1 = q.2.1 at hj
  have hv : logLik w p x = logLik w q x := le_antisymm (hmq hp) (hmp hq)
  unfold logLik at hv
  rw [← hs] at hv he'
  exact Prod.ext hs (same_shape_join (hν ⟨p,hp⟩) w hw
    (hwidth ⟨p,hp⟩) (hwidth ⟨q,hq⟩) hi (by omega) hj he' (add_left_cancel hv))


/-- Admissible finite parameters with closed restrictions on mean, variance
and squared skew. Use a singleton to hold a public coefficient. -/
def domain (M V K : Set ℝ) : Set Parameter :=
  {p | 2 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2 ∧
    AE1SSTD.mean p.1 p.2 ∈ M ∧ AE1SSTD.variance p.1 p.2 ∈ V ∧
    AE1SSTD.skewRatio p.2 ∈ K}

private theorem domain_locallyClosed (M V K : Set ℝ)
    (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    IsLocallyClosed (domain M V K) := by
  let O : Set Parameter := Ioi 2 ×ˢ (univ ×ˢ Ioi 0 ×ˢ Ioi 0)
  have hO : IsOpen O := isOpen_Ioi.prod (isOpen_univ.prod (isOpen_Ioi.prod isOpen_Ioi))
  have hν (p : O) : 2 < p.val.1 := p.property.1
  have ha (p : O) : 0 < p.val.2.2.1 := p.property.2.2.1
  have hb (p : O) : 0 < p.val.2.2.2 := p.property.2.2.2
  have hνc : Continuous (fun p : O ↦ p.val.1) := by fun_prop
  have hγ {f : O → ℝ} (hc : Continuous f) (hp : ∀ p, 0 < f p) :
      Continuous (fun p ↦ Gamma (f p)) := by
    rw [continuous_iff_continuousAt]
    intro p
    exact (differentiableAt_Gamma (fun m ↦ by
      have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
      linarith [hp p])).continuousAt.comp hc.continuousAt
  have hh : Continuous (fun p : O ↦ TwoPieceStudent.halfMoment p.val.1) := by
    unfold TwoPieceStudent.halfMoment
    have h₁ := hγ ((hνc.sub continuous_const).div_const 2)
      (fun p ↦ show 0 < (p.val.1 - 1) / 2 by linarith [hν p])
    have h₂ := hγ (hνc.div_const 2)
      (fun p ↦ show 0 < p.val.1 / 2 by linarith [hν p])
    exact h₁.div (h₂.const_mul _) (fun p ↦
      (mul_pos (sqrt_pos.mpr pi_pos) (Gamma_pos_of_pos (by linarith [hν p]))).ne')
  have hm : Continuous (fun p : O ↦ AE1SSTD.mean p.val.1 p.val.2) := by
    unfold AE1SSTD.mean
    fun_prop
  have hv : Continuous (fun p : O ↦ AE1SSTD.variance p.val.1 p.val.2) := by
    unfold AE1SSTD.variance
    apply Continuous.sub _ (by fun_prop)
    exact Continuous.div (by fun_prop) (by fun_prop) (fun p ↦
      (mul_pos (by norm_num) (sub_pos.mpr (hν p))).ne')
  have hk : Continuous (fun p : O ↦ AE1SSTD.skewRatio p.val.2) := by
    unfold AE1SSTD.skewRatio
    exact Continuous.div (by fun_prop) (by fun_prop) (fun p ↦ (ha p).ne')
  let C : Set O := {p | AE1SSTD.mean p.val.1 p.val.2 ∈ M ∧
    AE1SSTD.variance p.val.1 p.val.2 ∈ V ∧ AE1SSTD.skewRatio p.val.2 ∈ K}
  have hC : IsClosed C := (hM.preimage hm).inter ((hV.preimage hv).inter (hK.preimage hk))
  obtain ⟨F, hF, hEq⟩ := isClosed_induced_iff.mp hC
  have hD : domain M V K = O ∩ F := by
    ext p
    constructor
    · intro h
      have ho : p ∈ O := ⟨h.1, mem_univ _, h.2.1, h.2.2.1⟩
      have hc : (⟨p, ho⟩ : O) ∈ C := h.2.2.2
      rw [← hEq] at hc
      exact ⟨ho, hc⟩
    · rintro ⟨ho, hf⟩
      have hc : (⟨p, ho⟩ : O) ∈ Subtype.val ⁻¹' F := hf
      rw [hEq] at hc
      exact ⟨ho.1, ho.2.2.1, ho.2.2.2, hc⟩
  exact hD ▸ hO.isLocallyClosed.inter hF.isLocallyClosed

/-- Four-level finite uniqueness with free shape and arbitrary combinations
of held mean, standard deviation and skew. A free coefficient uses `univ`.
An attained maximum is unique almost everywhere on each fixed-count stratum. -/
theorem ae_unique_with_holds (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 4 ≤ n)
    (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V) (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ domain M V K, ∀ q ∈ domain M V K,
        IsMaxOn (fun r ↦ logLik w r x) (domain M V K) p →
        IsMaxOn (fun r ↦ logLik w r x) (domain M V K) q → p = q := by
  exact ae_unique_four_levels w hw hn _ (domain_locallyClosed M V K hM hV hK)
    (fun _ h ↦ ⟨h.1, h.2.1, h.2.2.1⟩)

/-- At a held mean and unit skew, two unequal nonzero residual radii already
suffice for uniqueness almost everywhere. Variance may be held or free. -/
theorem held_mean_unit_skew_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m : ℝ)
    (V : Set ℝ) (hV : IsClosed V) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∃ i j, x i ≠ m ∧ x j ≠ m ∧ (x i-m)^2 ≠ (x j-m)^2) →
      ∀ p ∈ domain {m} V {1}, ∀ q ∈ domain {m} V {1},
        IsMaxOn (fun r ↦ logLik w r x) (domain {m} V {1}) p →
        IsMaxOn (fun r ↦ logLik w r x) (domain {m} V {1}) q → p = q := by
  let A := domain {m} V {1}
  have hA : IsLocallyClosed A := domain_locallyClosed _ _ _ isClosed_singleton hV
    isClosed_singleton
  have hparam : ∀ p ∈ A, 2 < p.1 ∧ 0 < p.2.2.1 ∧ 0 < p.2.2.2 :=
    fun _ h ↦ ⟨h.1,h.2.1,h.2.2.1⟩
  have heq (p : Parameter) (hp : p ∈ A) :
      p.2.1 = m ∧ p.2.2.2 = p.2.2.1 := by
    have h : p.2.2.2 / p.2.2.1 = 1 := hp.2.2.2.2.2
    have hw := (div_eq_iff hp.2.1.ne').mp h
    rw [one_mul] at hw
    have hm : AE1SSTD.mean p.1 p.2 = m := hp.2.2.2.1
    simpa [AE1SSTD.mean,hw] using And.intro hm hw
  have esc (p : Parameter) (hp : p ∈ A) (t : ℝ) :
      score p.1 p.2 t = -((p.1+1)*((t-m)/(p.2.2.1^2+(t-m)^2))) := by
    obtain ⟨hm,hw⟩ := heq p hp
    rcases le_or_gt t m with h | h
    · rw [left_score p.1 p.2 (hm.symm ▸ h), hm,
        show m-t = -(t-m) by ring, neg_sq, neg_div, mul_neg]
    · rw [right_score p.1 p.2 (hm.symm ▸ h.le), hm, hw]
  filter_upwards [chart w hw A hA hparam] with x hx hlevels p hp q hq hmp hmq
  obtain ⟨i,j,hi,hj,hij⟩ := hlevels
  have h (k : Fin n) := hx p hp q hq hmp hmq k
  simp only [esc p hp, esc q hq, neg_inj] at h
  have hs := same_join_shape (sq_pos_of_pos (hparam p hp).2.1)
    (sq_pos_of_pos (hparam q hq).2.1) hi hj hij (h i) (h j)
  have hden (a : ℝ) (ha : 0 < a) : a^2+(x i-m)^2 ≠ 0 := by positivity
  have hsc := h i
  rw [← hs, ← mul_div_assoc, ← mul_div_assoc] at hsc
  have hz := (div_eq_div_iff (hden _ (hparam p hp).2.1) (hden _ (hparam q hq).2.1)).mp hsc
  have hmul : ((p.1+1)*(x i-m))*(q.2.2.1^2-p.2.2.1^2) = 0 := by
    linear_combination hz
  have hsq := sub_eq_zero.mp ((mul_eq_zero.mp hmul).resolve_left
    (mul_ne_zero (by linarith [(hparam p hp).1]) (sub_ne_zero.mpr hi)))
  have hwidthP := (hparam p hp).2.1
  have hwidthQ := (hparam q hq).2.1
  have hwidth : p.2.2.1 = q.2.2.1 := by nlinarith
  exact Prod.ext hs (Prod.ext ((heq p hp).1.trans (heq q hq).1.symm)
    (Prod.ext hwidth ((heq p hp).2.trans (hwidth.trans (heq q hq).2.symm))))


/-- A single level of positive count never attains an all-free finite
two-piece Student maximum. Shrinking both widths at that level improves
every candidate centered there, and moving the join to that level cannot
lower the likelihood. -/
theorem constant_nonattainment {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (x : Fin n → ℝ) (c : ℝ) (hx : ∀ i, x i = c) (p : Parameter)
    (hp : p ∈ domain univ univ univ) :
    ¬ IsMaxOn (fun q ↦ logLik w q x) (domain univ univ univ) p := by
  have hν : 2 < p.1 := hp.1
  have ha : 0 < p.2.2.1 := hp.2.1
  have hb : 0 < p.2.2.2 := hp.2.2.1
  let q : Parameter := (p.1,c,p.2.2.1/2,p.2.2.2/2)
  have hq : q ∈ domain univ univ univ :=
    ⟨hν,half_pos ha,half_pos hb,mem_univ _,mem_univ _,mem_univ _⟩
  have hpeak : log (q.2.2.1+q.2.2.2) < log (p.2.2.1+p.2.2.2) := by
    apply log_lt_log
    · exact add_pos (half_pos ha) (half_pos hb)
    · dsimp [q]
      linarith
  let _ : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
  have hsum : AE1SSTD.fixedShapeLogLik w p.1 p.2 x <
      AE1SSTD.fixedShapeLogLik w p.1 q.2 x := by
    unfold AE1SSTD.fixedShapeLogLik
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    have hnon (a : ℝ) : 0 ≤ log (1+a^2) :=
      log_nonneg (by nlinarith [sq_nonneg a])
    have hs : 0 ≤ (p.1+1)/2 := by linarith
    have hpen := mul_nonneg hs (add_nonneg
      (hnon (max (p.2.1-x i) 0 / p.2.2.1)) (hnon (max (x i-p.2.1) 0 / p.2.2.2)))
    apply mul_lt_mul_of_pos_left _ (by exact_mod_cast hw i)
    have he : q.2.1 = x i := (hx i).symm
    rw [he]
    simp only [sub_self,max_self,zero_div,zero_pow (by decide : 2 ≠ 0),add_zero,
      log_one,mul_zero,sub_zero]
    linarith
  intro hm
  have h := hm hq
  exact (not_le_of_gt (add_lt_add_right hsum ((∑ i, (w i : ℝ)) *
    TwoPieceStudent.logPeak p.1))) h

end

end UnivariateML.AE4SSTD

end
