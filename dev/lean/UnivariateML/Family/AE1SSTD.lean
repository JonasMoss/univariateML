module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Family.TwoPieceStudent

import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Two-piece Student with fixed shape

The chart uses the join and the two denominator widths. At fixed shape and
with positive widths, the omitted peak normalization depends only on shape.
The theorem permits locally closed restrictions on these coordinates, so
long as both widths stay positive.

## Deviations

The result addresses finite winners at fixed shape, including joins at
observations. Boundary comparisons and the stationary-component inventory
are separate.
-/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap Real
open scoped Topology

namespace UnivariateML.AE1SSTD

noncomputable section

variable {n : ℕ}

/-- Fixed-shape two-piece Student log likelihood, up to a shape-only constant.
The triple lists join, left width, and right width. -/
def fixedShapeLogLik (w : Fin n → ℕ) (ν : ℝ) (p : ℝ × ℝ × ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (-log (p.2.1 + p.2.2) - (ν + 1) / 2 *
    (log (1 + (max (p.1 - x i) 0 / p.2.1)^2) +
     log (1 + (max (x i - p.1) 0 / p.2.2)^2)))

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

private theorem exterior {A B d r s : ℝ} (hA : 0 < A) (hd : 0 < d) (hr : 0 < r) (hs : 0 < s)
    (er : r / (A + r ^ 2) = (r + d) / (B + (r + d) ^ 2))
    (es : s / (A + s ^ 2) = (s + d) / (B + (s + d) ^ 2)) (hB : 0 < B) : r = s := by
  have quad (z : ℝ) (eq : z / (A + z^2) = (z+d) / (B + (z+d)^2)) :
      d*z^2 + (d^2+B-A)*z - d*A = 0 := by
    have h := (div_eq_div_iff (by positivity) (by positivity)).mp eq
    linear_combination h
  have h : (r - s) * (d*r*s + d*A) = 0 := by
    linear_combination s * quad r er - r * quad s es
  exact sub_eq_zero.mp ((mul_eq_zero.mp h).resolve_right (by positivity))

private theorem equal_score {ν : ℝ} (hν : 2 < ν) {p q : ℝ × ℝ × ℝ}
    (hp : 0 < p.2.1 ∧ 0 < p.2.2) (hq : 0 < q.2.1 ∧ 0 < q.2.2)
    {x : Fin n → ℝ} (hx : Function.Injective x) (hn : 3 ≤ n)
    (he : ∀ i, score ν p (x i) = score ν q (x i)) : p.1 = q.1 := by
  have lval (r : ℝ × ℝ × ℝ) (z : ℝ) (h : z ≤ r.1) :
      score ν r z = (ν + 1) * ((r.1-z)/(r.2.1^2+(r.1-z)^2)) := by
    simp [score, max_eq_left (sub_nonneg.mpr h), max_eq_right (sub_nonpos.mpr h)]
  have rval (r : ℝ × ℝ × ℝ) (z : ℝ) (h : r.1 ≤ z) :
      score ν r z = -((ν + 1) * ((z-r.1)/(r.2.2^2+(z-r.1)^2))) := by
    simp [score, max_eq_left (sub_nonneg.mpr h), max_eq_right (sub_nonpos.mpr h)]
  have hshape : 0 < ν + 1 := by linarith
  have sep (u v : ℝ × ℝ × ℝ) (hu : 0 < u.2.1 ∧ 0 < u.2.2)
      (hv : 0 < v.2.1 ∧ 0 < v.2.2) (h : u.1 < v.1)
      (e : ∀ i, score ν u (x i) = score ν v (x i)) : False := by
    have side (i : Fin n) : x i < u.1 ∨ v.1 < x i := by
      by_contra! hn'
      have e' := e i
      rw [rval _ _ hn'.1, lval _ _ hn'.2] at e'
      rcases eq_or_lt_of_le hn'.1 with hh | hh
      · rw [← hh, sub_self, zero_div, mul_zero, neg_zero] at e'
        exact (mul_pos hshape (div_pos (sub_pos.mpr (hh ▸ h)) (by positivity))).ne' e'.symm
      · have hl : 0 < (ν + 1) * ((x i-u.1)/(u.2.2^2+(x i-u.1)^2)) :=
          mul_pos hshape (div_pos (sub_pos.mpr hh) (by positivity))
        have hr : 0 ≤ (ν + 1) * ((v.1-x i)/(v.2.1^2+(v.1-x i)^2)) :=
          mul_nonneg hshape.le (div_nonneg (sub_nonneg.mpr hn'.2) (by positivity))
        linarith
    have left (i j : Fin n) (hi : x i < u.1) (hj : x j < u.1) : i = j := by
      have f (k : Fin n) (hk : x k < u.1) :
          (u.1-x k)/(u.2.1^2+(u.1-x k)^2) =
          ((u.1-x k)+(v.1-u.1))/(v.2.1^2+((u.1-x k)+(v.1-u.1))^2) := by
        have hh := e k
        rw [lval _ _ hk.le, lval _ _ (hk.trans h).le] at hh
        rw [show (u.1-x k)+(v.1-u.1) = v.1-x k by ring]
        exact mul_left_cancel₀ hshape.ne' hh
      have hh := exterior (sq_pos_of_pos hu.1) (sub_pos.mpr h)
        (sub_pos.mpr hi) (sub_pos.mpr hj) (f i hi) (f j hj) (sq_pos_of_pos hv.1)
      exact hx (by linarith)
    have right (i j : Fin n) (hi : v.1 < x i) (hj : v.1 < x j) : i = j := by
      have f (k : Fin n) (hk : v.1 < x k) :
          (x k-v.1)/(v.2.2^2+(x k-v.1)^2) =
          ((x k-v.1)+(v.1-u.1))/(u.2.2^2+((x k-v.1)+(v.1-u.1))^2) := by
        have hh := e k
        rw [rval _ _ (h.trans hk).le, rval _ _ hk.le] at hh
        rw [show (x k-v.1)+(v.1-u.1) = x k-u.1 by ring]
        exact mul_left_cancel₀ hshape.ne' (neg_inj.mp hh.symm)
      have hh := exterior (sq_pos_of_pos hv.2) (sub_pos.mpr h)
        (sub_pos.mpr hi) (sub_pos.mpr hj) (f i hi) (f j hj) (sq_pos_of_pos hu.2)
      exact hx (by linarith)
    let i : Fin n := ⟨0, by omega⟩
    let j : Fin n := ⟨1, by omega⟩
    let k : Fin n := ⟨2, by omega⟩
    have hij : i ≠ j := by simp [i, j]
    have hik : i ≠ k := by simp [i, k]
    have hjk : j ≠ k := by simp [j, k]
    rcases side i with hi | hi <;> rcases side j with hj | hj <;>
      rcases side k with hk | hk <;> first
      | exact hij (left i j hi hj)
      | exact hij (right i j hi hj)
      | exact hik (left i k hi hk)
      | exact hik (right i k hi hk)
      | exact hjk (left j k hj hk)
      | exact hjk (right j k hj hk)
  exact le_antisymm (le_of_not_gt fun h ↦ sep q p hq hp h (fun i ↦ (he i).symm))
    (le_of_not_gt fun h ↦ sep p q hp hq h he)

private theorem separates {ν : ℝ} (hν : 2 < ν) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    {p q : ℝ × ℝ × ℝ} (hp : 0 < p.2.1 ∧ 0 < p.2.2) (hq : 0 < q.2.1 ∧ 0 < q.2.2)
    {x : Fin n → ℝ} (hx : Function.Injective x) (hn : 3 ≤ n)
    (he : ∀ i, score ν p (x i) = score ν q (x i))
    (hval : fixedShapeLogLik w ν p x = fixedShapeLogLik w ν q x) : p = q := by
  have hc := equal_score hν hp hq hx hn he
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
  unfold fixedShapeLogLik at hval
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

/-- At fixed shape, three distinct levels suffice for almost-everywhere
uniqueness of any attained finite fit on a locally closed nuisance chart. -/
theorem held_nu_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) {ν : ℝ} (hν : 2 < ν)
    (hn : 3 ≤ n) (A : Set (ℝ × ℝ × ℝ)) (hA : IsLocallyClosed A)
    (hwidth : ∀ p ∈ A, 0 < p.2.1 ∧ 0 < p.2.2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ A, ∀ q ∈ A, IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) A p →
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) A q → p = q := by
  let _ : LocallyCompactSpace A := hA.locallyCompactSpace
  let L (p : A) := fixedShapeLogLik w ν p.val
  let D (p : A) := sampleDeriv w ν p.val
  have hL : Continuous (fun z : A × (Fin n → ℝ) ↦ L z.1 z.2) := by
    unfold L fixedShapeLogLik
    apply continuous_finsetSum Finset.univ
    intro i _
    have ha : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.1) := by fun_prop
    have hb : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2) := by fun_prop
    have hl : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.1.val.1 - z.2 i) 0 / z.1.val.2.1) :=
      (by fun_prop : Continuous fun z : A × (Fin n → ℝ) ↦ max (z.1.val.1 - z.2 i) 0).div
        ha (fun z ↦ (hwidth z.1.val z.1.property).1.ne')
    have hr : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.1) 0 / z.1.val.2.2) :=
      (by fun_prop : Continuous fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.1) 0).div
        hb (fun z ↦ (hwidth z.1.val z.1.property).2.ne')
    have hl' := ((hl.pow 2).const_add 1).log (fun _ ↦
      (add_pos_of_pos_of_nonneg one_pos (sq_nonneg _)).ne')
    have hr' := ((hr.pow 2).const_add 1).log (fun _ ↦
      (add_pos_of_pos_of_nonneg one_pos (sq_nonneg _)).ne')
    have hnorm := ((ha.add hb).log (fun z ↦ (add_pos (hwidth _ z.1.property).1
      (hwidth _ z.1.property).2).ne')).neg
    exact (hnorm.sub ((hl'.add hr').const_mul ((ν + 1) / 2))).const_mul (w i : ℝ)
  have hD : Continuous (fun z : A × (Fin n → ℝ) ↦ D z.1 z.2) := by
    unfold D sampleDeriv score
    apply continuous_finsetSum Finset.univ
    intro i _
    have hl : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.1.val.1 - z.2 i) 0) := by fun_prop
    have hr : Continuous (fun z : A × (Fin n → ℝ) ↦ max (z.2 i - z.1.val.1) 0) := by fun_prop
    have ha : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.1) := by fun_prop
    have hb : Continuous (fun z : A × (Fin n → ℝ) ↦ z.1.val.2.2) := by fun_prop
    have hl' := hl.div ((ha.pow 2).add (hl.pow 2)) (fun z ↦
      (add_pos_of_pos_of_nonneg (sq_pos_of_pos (hwidth _ z.1.property).1) (sq_nonneg _)).ne')
    have hr' := hr.div ((hb.pow 2).add (hr.pow 2)) (fun z ↦
      (add_pos_of_pos_of_nonneg (sq_pos_of_pos (hwidth _ z.1.property).2) (sq_nonneg _)).ne')
    exact (((hl'.sub hr').const_mul (ν + 1)).const_mul (w i : ℝ)).smul continuous_const
  have hd (p : A) (x : Fin n → ℝ) : HasFDerivAt (L p) (D p x) x := by
    unfold L D fixedShapeLogLik sampleDeriv
    apply HasFDerivAt.fun_sum
    intro i _
    let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have h := (term_deriv ν p.val (x i) (hwidth _ p.property).1
      (hwidth _ p.property).2).const_mul (w i : ℝ)
    have hh := h.comp_hasFDerivAt x (f.hasFDerivAt (x := x))
    exact hh
  have hae := UniqueAE.ae_winners_derivative (μ := volume) (S := (univ : Set (A × (Fin n → ℝ))))
    isOpen_univ hL.continuousOn hD.continuousOn (fun p x _ ↦ hd p x)
  filter_upwards [hae] with x hx hi p hp q hq hmp hmq
  have he := hx ⟨p, hp⟩ ⟨q, hq⟩ ⟨mem_univ _, fun r _ ↦ hmp r.property⟩
     ⟨mem_univ _, fun r _ ↦ hmq r.property⟩
  apply separates hν w hw (hwidth p hp) (hwidth q hq) hi hn _ (le_antisymm (hmq hp) (hmp hq))
  intro i
  have eval (r : A) :
      D r x (Pi.single i 1) = (w i : ℝ) * score ν r.val (x i) := by
    simp only [D, sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hj
      simp [Pi.single_eq_of_ne hj]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) he
  rw [eval, eval] at h
  exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') h


/-- Population mean in denominator width coordinates. -/
def mean (ν : ℝ) (p : ℝ × ℝ × ℝ) :=
  p.1 + TwoPieceStudent.halfMoment ν * (p.2.2 - p.2.1)

/-- Population variance in denominator width coordinates. -/
def variance (ν : ℝ) (p : ℝ × ℝ × ℝ) :=
  ((p.2.1 + p.2.2)^2 + 3 * (p.2.2 - p.2.1)^2) / (4 * (ν - 2)) -
    (TwoPieceStudent.halfMoment ν * (p.2.2 - p.2.1))^2

/-- Right-to-left width ratio, equal to the square of skew. -/
def skewRatio (p : ℝ × ℝ × ℝ) : ℝ := p.2.2 / p.2.1

/-- Positive widths subject to closed restrictions on the public coefficients. -/
def heldNuDomain (ν : ℝ) (M V K : Set ℝ) : Set (ℝ × ℝ × ℝ) :=
  {p | 0 < p.2.1 ∧ 0 < p.2.2 ∧ mean ν p ∈ M ∧
    variance ν p ∈ V ∧ skewRatio p ∈ K}

/-- Fixed-shape uniqueness with any combination of mean, standard
deviation and skew held. To hold a coefficient, use the singleton of its
mean, squared standard deviation or squared skew respectively. Use the
whole real line for a free coefficient. -/
theorem held_nu_with_holds_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) {ν : ℝ} (hν : 2 < ν)
    (hn : 3 ≤ n) (M V K : Set ℝ) (hM : IsClosed M) (hV : IsClosed V)
    (hK : IsClosed K) :
    ∀ᵐ x : Fin n → ℝ ∂volume, Function.Injective x →
      ∀ p ∈ heldNuDomain ν M V K, ∀ q ∈ heldNuDomain ν M V K,
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) (heldNuDomain ν M V K) p →
        IsMaxOn (fun r ↦ fixedShapeLogLik w ν r x) (heldNuDomain ν M V K) q → p = q := by
  let O : Set (ℝ × ℝ × ℝ) := univ ×ˢ Ioi 0 ×ˢ Ioi 0
  have hO : IsOpen O := isOpen_univ.prod (isOpen_Ioi.prod isOpen_Ioi)
  have hm : Continuous (mean ν) := by unfold mean; fun_prop
  have hv : Continuous (variance ν) := by unfold variance; fun_prop
  let P : Set (ℝ × ℝ × ℝ) := {p | mean ν p ∈ M ∧ variance ν p ∈ V}
  have hp : IsClosed P := (hM.preimage hm).inter (hV.preimage hv)
  let R : Set O := {p | skewRatio p.val ∈ K}
  have hr : IsClosed R := hK.preimage (by
    unfold skewRatio
    have ha : Continuous (fun p : O ↦ p.val.2.1) := by fun_prop
    have hb : Continuous (fun p : O ↦ p.val.2.2) := by fun_prop
    exact hb.div ha (fun p ↦ (show 0 < p.val.2.1 from p.property.2.1).ne'))
  obtain ⟨C, hC, hEq⟩ := isClosed_induced_iff.mp hr
  have hA : heldNuDomain ν M V K = O ∩ (P ∩ C) := by
    ext p
    constructor
    · intro hh
      have ho : p ∈ O := ⟨mem_univ _, hh.1, hh.2.1⟩
      refine ⟨ho, ⟨⟨hh.2.2.1, hh.2.2.2.1⟩, ?_⟩⟩
      have h : (⟨p, ho⟩ : O) ∈ R := hh.2.2.2.2
      rw [← hEq] at h
      exact h
    · rintro ⟨ho, ⟨⟨hm, hv⟩, hC⟩⟩
      have h : (⟨p, ho⟩ : O) ∈ Subtype.val ⁻¹' C := hC
      rw [hEq] at h
      exact ⟨ho.2.1, ho.2.2, hm, hv, h⟩
  exact held_nu_ae_unique w hw hν hn _ (hA ▸ (hO.isLocallyClosed.inter
    (hp.inter hC).isLocallyClosed)) (fun _ h ↦ ⟨h.1, h.2.1⟩)

end
end UnivariateML.AE1SSTD

end
