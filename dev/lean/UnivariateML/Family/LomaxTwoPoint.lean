module

public import UnivariateML.Family.Lomax

import Mathlib.Analysis.Analytic.IsolatedZeros
import Mathlib.Analysis.Calculus.LocalExtr.Rolle
import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Analysis.Convex.Deriv
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Lomax joint uniqueness on two-point samples

The positive stationary rates are controlled by the roots of a strictly
concave cubic. A pair of global maxima would supply a third stationary rate
between them, even if the profile were constant on the intervening interval.

## Deviations

Joint uniqueness is stated for finite samples. The cubic calculation and
strict concavity also hold for arbitrary positive normalized real weights.
The counterexample establishes separated strict local maxima.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.LomaxTwoPoint

open Lomax

noncomputable section

variable {n : ℕ}

def Ar (x : Fin n → ℝ) (r : ℝ) : ℝ := avg (fun i ↦ log (1 + r * x i))

def U (x : Fin n → ℝ) (r : ℝ) : ℝ := avg (fun i ↦ (1 + r * x i)⁻¹)

def B (x : Fin n → ℝ) (r : ℝ) : ℝ := 1 - U x r

def C (x : Fin n → ℝ) (r : ℝ) : ℝ := avg (fun i ↦ r * x i / (1 + r * x i)^2)

def H (x : Fin n → ℝ) (r : ℝ) : ℝ := Ar x r - B x r / U x r

def F (x : Fin n → ℝ) (r : ℝ) : ℝ := log (r * avg x) - log (Ar x r) - Ar x r

private theorem cast_pos (hn : 0 < n) : (0 : ℝ) < n := by exact_mod_cast hn

private theorem avg_pos (hn : 0 < n) {v : Fin n → ℝ} (hv : ∀ i, 0 < v i) :
    0 < avg v := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact div_pos (Finset.sum_pos (fun i _ ↦ hv i) Finset.univ_nonempty) (cast_pos hn)

private theorem factor_pos {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ}
    (hr : 0 ≤ r) (i : Fin n) : 0 < 1 + r * x i :=
  add_pos_of_pos_of_nonneg zero_lt_one (mul_nonneg hr (hx i).le)

private theorem Ar_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {r : ℝ} (hr : 0 < r) : 0 < Ar x r :=
  avg_pos hn fun i ↦ log_pos (lt_add_of_pos_right _ (mul_pos hr (hx i)))

private theorem U_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {r : ℝ} (hr : 0 ≤ r) : 0 < U x r :=
  avg_pos hn fun i ↦ inv_pos.mpr (factor_pos hx hr i)

private theorem avg_deriv {f : Fin n → ℝ → ℝ} {v : Fin n → ℝ} {r : ℝ}
    (h : ∀ i, HasDerivAt (f i) (v i) r) :
    HasDerivAt (fun t ↦ avg (fun i ↦ f i t)) (avg v) r := by
  simpa only [avg, Finset.sum_apply] using
    (HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ h i)).div_const (n : ℝ)

private theorem linear_deriv (a r : ℝ) :
    HasDerivAt (fun t ↦ 1 + t * a) a r := by
  simpa using ((hasDerivAt_id r).mul_const a).const_add 1

private theorem ar_deriv {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ} (hr : 0 ≤ r) :
    HasDerivAt (Ar x) (avg (fun i ↦ x i / (1 + r * x i))) r :=
  avg_deriv fun i ↦ (linear_deriv (x i) r).log (factor_pos hx hr i).ne'

private theorem u_deriv {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ} (hr : 0 ≤ r) :
    HasDerivAt (U x) (avg (fun i ↦ -x i / (1 + r * x i)^2)) r :=
  avg_deriv fun i ↦ (linear_deriv (x i) r).inv (factor_pos hx hr i).ne'

private theorem ar_mul (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {r : ℝ} (hr : 0 ≤ r) : r * avg (fun i ↦ x i / (1 + r * x i)) = B x r := by
  have h (i : Fin n) : r * (x i / (1 + r * x i)) = 1 - (1 + r * x i)⁻¹ := by
    field_simp [(factor_pos hx hr i).ne']
    ring
  simp only [B, U, avg, ← mul_div_assoc, Finset.mul_sum, h, Finset.sum_sub_distrib,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one]
  field_simp [(cast_pos hn).ne']

private theorem u_mul (x : Fin n → ℝ) (r : ℝ) :
    r * avg (fun i ↦ -x i / (1 + r * x i)^2) = -C x r := by
  simp only [C, avg, ← mul_div_assoc, Finset.mul_sum, mul_neg, neg_div, Finset.sum_neg_distrib]

private theorem h_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ}
    (hr : 0 ≤ r) :
    HasDerivAt (H x)
      (avg (fun i ↦ x i / (1 + r * x i)) +
        avg (fun i ↦ -x i / (1 + r * x i)^2) / U x r ^ 2) r := by
  have hu := (U_pos hn hx hr).ne'
  have hd := ((ar_deriv hx hr).add_const 1).sub ((u_deriv hx hr).inv hu)
  have he : (fun t ↦ Ar x t + 1 - (U x t)⁻¹) =ᶠ[𝓝 r] H x := by
    filter_upwards [(u_deriv hx hr).continuousAt.eventually_ne hu] with t ht
    unfold H B
    field_simp [ht]
    ring
  convert hd.congr_of_eventuallyEq he.symm using 1
  ring

private theorem f_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {r : ℝ}
    (hr : 0 < r) : HasDerivAt (F x) ((U x r / Ar x r * H x r) / r) r := by
  have ha := (Ar_pos hn hx hr).ne'
  have hu := (U_pos hn hx hr.le).ne'
  have hm := (avg_pos hn hx).ne'
  have h := (ar_deriv hx hr.le)
  have hd := ((((hasDerivAt_id r).mul_const (avg x)).log (mul_ne_zero hr.ne' hm)).sub
    (h.log ha)).sub h
  convert hd using 1
  · rfl
  have hb := ar_mul hn hx hr.le
  have he := (eq_div_iff hr.ne').mpr (by simpa [mul_comm] using hb)
  rw [he]
  simp only [id_eq]
  unfold H
  field_simp [ha, hu, hm, hr.ne']
  unfold B
  ring

/-- The derivative of the profile gap in log-rate coordinates. -/
theorem profile_logRate_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (z : ℝ) :
    HasDerivAt (fun t ↦ F x (exp t)) (U x (exp z) / Ar x (exp z) * H x (exp z)) z := by
  convert (f_deriv hn hx (exp_pos z)).comp z (hasDerivAt_exp z) using 1
  · rfl
  rw [div_mul_cancel₀ _ (exp_pos z).ne']

/-- The derivative of the zero-equivalent score in log-rate coordinates. -/
theorem score_logRate_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (z : ℝ) :
    HasDerivAt (fun t ↦ H x (exp t)) (B x (exp z) - C x (exp z) / U x (exp z)^2) z := by
  convert (h_deriv hn hx (exp_pos z).le).comp z (hasDerivAt_exp z) using 1
  · rfl
  rw [mul_comm, mul_add, ar_mul hn hx (exp_pos z).le, ← mul_div_assoc, u_mul,
    neg_div, sub_eq_add_neg]

/-- The cubic obtained by removing the factor r² from the derivative numerator. -/
def cubic (m b d r : ℝ) : ℝ :=
  m * (b - m) - d + (m * b^2 - d * (b + 3*m))*r -
    d * (b*m + 2*d)*r^2 - b*d^2*r^3

/-- With normalized weights, s = b + m, D = 1 + sr + dr² and N = 1 + br. -/
theorem numerator_eq (m b d r : ℝ) :
    let D := 1 + (b + m)*r + d*r^2
    let N := 1 + b*r
    (D - N)*N^2 - r*D*(N*(b+m+2*d*r) - b*D) = r^2 * cubic m b d r := by
  dsimp [cubic]
  ring

/-- Positive mean, complementary mean and support product give strict concavity. -/
theorem cubic_strictConcave {m b d : ℝ} (hm : 0 < m) (hb : 0 < b) (hd : 0 < d) :
    StrictConcaveOn ℝ (Ioi 0) (cubic m b d) := by
  have h₁ (r : ℝ) : HasDerivAt (cubic m b d)
      (m*b^2-d*(b+3*m) - d*(b*m+2*d)*2*r - b*d^2*3*r^2) r := by
    unfold cubic
    convert (((((hasDerivAt_id r).const_mul (m*b^2-d*(b+3*m))).const_add
      (m*(b-m)-d)).sub (((hasDerivAt_id r).pow 2).const_mul (d*(b*m+2*d)))).sub
      (((hasDerivAt_id r).pow 3).const_mul (b*d^2))) using 1
    · ext t
      simp only [Pi.sub_apply, Pi.pow_apply, id_eq]
    · simp only [id_eq, Nat.cast_ofNat]
      ring
  have h₂ (r : ℝ) : HasDerivAt (deriv (cubic m b d))
      (- (d*(b*m+2*d)*2 + b*d^2*6*r)) r := by
    have he := funext fun t ↦ (h₁ t).deriv
    rw [he]
    convert ((((hasDerivAt_id r).const_mul (d*(b*m+2*d)*2)).const_sub
      (m*b^2-d*(b+3*m))).sub (((hasDerivAt_id r).pow 2).const_mul (b*d^2*3))) using 1
    · ext t
      simp only [Pi.sub_apply, Pi.pow_apply, id_eq]
    · simp only [id_eq, Nat.cast_ofNat]
      ring
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioi 0)
    (fun r _ ↦ (h₁ r).continuousAt.continuousWithinAt)
  intro r hr
  rw [Function.iterate_succ_apply', Function.iterate_one, (h₂ r).deriv]
  have hr' : 0 < r := hr
  exact neg_neg_of_pos (by positivity)

/-- Three ordered positive roots would contradict strict concavity at the middle one. -/
theorem cubic_no_three_zeros {m b d : ℝ} (hm : 0 < m) (hb : 0 < b) (hd : 0 < d)
    {a c e : ℝ} (ha : 0 < a) (hac : a < c) (hce : c < e)
    (h₁ : cubic m b d a = 0) (h₂ : cubic m b d c = 0) (h₃ : cubic m b d e = 0) :
    False := by
  have he : e ∈ Ioi (0 : ℝ) := ha.trans (hac.trans hce)
  have ha' : a ∈ Ioi (0 : ℝ) := ha
  have H := (cubic_strictConcave hm hb hd).2 ha' he (hac.trans hce).ne
    (div_pos (sub_pos.mpr hce) (sub_pos.mpr (hac.trans hce)))
    (div_pos (sub_pos.mpr hac) (sub_pos.mpr (hac.trans hce))) (by
      rw [← add_div]
      convert div_self (sub_ne_zero.mpr (hac.trans hce).ne') using 1
      ring)
  have hc : (e-c)/(e-a)*a + (c-a)/(e-a)*e = c := by
    field_simp [(sub_pos.mpr (hac.trans hce)).ne']
    ring
  simp only [smul_eq_mul, h₁, h₃, mul_zero, zero_add, hc, h₂, lt_irrefl] at H

/-- The expansion and concavity hold for arbitrary positive normalized weights. -/
theorem weighted_cubic {a c w : ℝ} (ha : 0 < a) (hc : 0 < c)
    (hw : 0 < w) (hw' : w < 1) :
    let m := w*a + (1-w)*c
    let b := a+c-m
    let d := a*c
    let D := fun r : ℝ ↦ (1+r*a)*(1+r*c)
    let N := fun r : ℝ ↦ w*(1+r*c) + (1-w)*(1+r*a)
    (∀ r, (D r - N r)*(N r)^2 - r*D r*(N r*(a+c+2*d*r) - b*D r)
      = r^2 * cubic m b d r) ∧ StrictConcaveOn ℝ (Ioi 0) (cubic m b d) := by
  dsimp only
  have hm := add_pos (mul_pos hw ha) (mul_pos (sub_pos.mpr hw') hc)
  have hb : 0 < a+c-(w*a+(1-w)*c) := by
    have h := add_pos (mul_pos hw hc) (mul_pos (sub_pos.mpr hw') ha)
    linarith
  refine ⟨fun r ↦ ?_, cubic_strictConcave hm hb (mul_pos ha hc)⟩
  have h := numerator_eq (w*a+(1-w)*c) (a+c-(w*a+(1-w)*c)) (a*c) r
  dsimp only at h
  linear_combination h

private theorem avg_mix (hn : 0 < n) {x : Fin n → ℝ} {a c : ℝ}
    (hs : ∀ i, x i = a ∨ x i = c) (f : ℝ → ℝ) :
    avg (fun i ↦ f (x i)) =
      avg (fun i ↦ if x i = a then 1 else 0) * f a +
        (1 - avg (fun i ↦ if x i = a then 1 else 0)) * f c := by
  classical
  let v : Fin n → ℝ := fun i ↦ if x i = a then 1 else 0
  have he (i : Fin n) : f (x i) = v i * (f a - f c) + f c := by
    by_cases h : x i = a
    · simp [v, h]
    · rw [show v i = 0 by simp [v, h], (hs i).resolve_left h, zero_mul, zero_add]
  change _ = avg v * f a + (1-avg v)*f c
  simp only [he, avg, Finset.sum_add_distrib, ← Finset.sum_mul, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp [(cast_pos hn).ne']
  ring

private theorem weight_bounds (hn : 0 < n) (x : Fin n → ℝ) (a : ℝ) :
    avg (fun i ↦ if x i = a then 1 else 0) ∈ Icc (0 : ℝ) 1 := by
  classical
  constructor
  · exact div_nonneg (Finset.sum_nonneg fun i _ ↦ by split <;> norm_num) (cast_pos hn).le
  · apply (div_le_iff₀ (cast_pos hn)).2
    have hsum : ∑ i : Fin n, (if x i = a then (1 : ℝ) else 0) ≤ ∑ _ : Fin n, (1 : ℝ) :=
      Finset.sum_le_sum (fun i _ ↦ by split <;> norm_num)
    simpa using hsum

private theorem two_point_u (hn : 0 < n) {x : Fin n → ℝ} {a c : ℝ}
    (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {r : ℝ} (hr : 0 ≤ r) :
    U x r = (1 + (a+c-avg x)*r) / (1 + (a+c)*r + (a*c)*r^2) := by
  have h₁ : 0 < 1+r*a := by positivity
  have h₂ : 0 < 1+r*c := by positivity
  have hD : 1 + (a+c)*r + (a*c)*r^2 = (1+r*a)*(1+r*c) := by ring
  unfold U
  have hm := avg_mix hn hs id
  simp only [id_eq] at hm
  rw [avg_mix hn hs (fun t ↦ (1+r*t)⁻¹), hD, hm]
  field_simp [h₁.ne', h₂.ne']
  ring

/-- The score derivative on a sample supported by two positive points is
the cubic times a positive factor at every positive rate. -/
theorem two_point_score_deriv (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {r : ℝ} (hr : 0 < r) :
    let m := avg x
    let b := a+c-m
    let d := a*c
    let D := 1 + (b+m)*r + d*r^2
    let N := 1 + b*r
    r * deriv (H x) r = r^2 * cubic m b d r / (D*N^2) := by
  let m := avg x
  let b := a+c-m
  let d := a*c
  let D := fun t : ℝ ↦ 1 + (b+m)*t + d*t^2
  let N := fun t : ℝ ↦ 1 + b*t
  have hs' : b+m = a+c := by dsimp [b]; ring
  have hD : 0 < D r := by dsimp [D]; rw [hs']; positivity
  have hU : U x r = N r / D r := by simpa only [D, N, hs'] using two_point_u hn ha hc hs hr.le
  have hN : N r ≠ 0 := by
    intro h
    have hp := U_pos hn hx hr.le
    rw [hU, h, zero_div] at hp
    exact (lt_irrefl _ hp)
  have hN' : HasDerivAt N b r := by
    simpa [N, mul_comm] using linear_deriv b r
  have hD' : HasDerivAt D (b+m+2*d*r) r := by
    convert (((hasDerivAt_id r).const_mul (b+m)).const_add 1).add
      (((hasDerivAt_id r).pow 2).const_mul d) using 1
    · rfl
    · simp only [id_eq, Nat.cast_ofNat]
      ring
  have hEq : U x =ᶠ[𝓝 r] fun t ↦ N t / D t := by
    filter_upwards [Ioi_mem_nhds hr] with t ht
    simpa only [D, N, hs'] using two_point_u hn ha hc hs (mem_Ioi.mp ht).le
  have hu' := ((hN'.div hD' hD.ne').congr_of_eventuallyEq hEq).deriv
  rw [(u_deriv hx hr.le).deriv] at hu'
  change r * deriv (H x) r = r^2 * cubic m b d r / (D r * (N r)^2)
  rw [(h_deriv hn hx hr.le).deriv, mul_add, ar_mul hn hx hr.le, hu', hU]
  have hB : B x r = (D r - N r) / D r := by
    unfold B
    rw [hU]
    field_simp [hD.ne']
  rw [hB, ← numerator_eq m b d r]
  change _ = ((D r - N r)*(N r)^2 - r*D r*(N r*(b+m+2*d*r) - b*D r)) /
    (D r*(N r)^2)
  field_simp [hD.ne', hN]
  ring

private theorem score_stationary_cubic (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {r : ℝ} (hr : 0 < r) (hz : deriv (H x) r = 0) :
    cubic (avg x) (a+c-avg x) (a*c) r = 0 := by
  have h := two_point_score_deriv hn hx ha hc hs hr
  dsimp only at h
  rw [hz, mul_zero] at h
  have hD : 0 < 1 + (a + c - avg x + avg x) * r + a*c*r^2 := by
    rw [sub_add_cancel]
    positivity
  have hU := two_point_u hn ha hc hs hr.le
  have hN : 1 + (a+c-avg x)*r ≠ 0 := by
    intro hn'
    rw [hn', zero_div] at hU
    exact (U_pos hn hx hr.le).ne' hU
  have hden := mul_ne_zero hD.ne' (pow_ne_zero 2 hN)
  have hnum := (div_eq_zero_iff.mp h.symm).resolve_right hden
  exact (mul_eq_zero.mp hnum).resolve_left (pow_ne_zero _ hr.ne')

private theorem score_zero_at_zero (hn : 0 < n) (x : Fin n → ℝ) : H x 0 = 0 := by
  have hN := (cast_pos hn).ne'
  simp [H, B, U, Ar, avg, hN]

private theorem score_root_between (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a b : ℝ} (ha : 0 ≤ a) (hab : a < b) (he : H x a = H x b) :
    ∃ t ∈ Ioo a b, deriv (H x) t = 0 :=
  exists_deriv_eq_zero hab
    (fun _t ht ↦ (h_deriv hn hx (ha.trans ht.1)).continuousAt.continuousWithinAt) he

private theorem no_three_score_zeros (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {u v w : ℝ} (hu : 0 < u) (huv : u < v) (hvw : v < w)
    (h₁ : H x u = 0) (h₂ : H x v = 0) (h₃ : H x w = 0) : False := by
  obtain ⟨i, hi, hzi⟩ := score_root_between hn hx le_rfl hu
    ((score_zero_at_zero hn x).trans h₁.symm)
  obtain ⟨j, hj, hzj⟩ := score_root_between hn hx hu.le huv (h₁.trans h₂.symm)
  obtain ⟨k, hk, hzk⟩ := score_root_between hn hx (hu.trans huv).le hvw (h₂.trans h₃.symm)
  have hb : 0 < a+c-avg x := by
    have h := weight_bounds hn x a
    have hm := avg_mix hn hs id
    simp only [id_eq] at hm
    rw [hm]
    have hb' : 0 < avg (fun i ↦ if x i = a then 1 else 0) * c +
        (1 - avg (fun i ↦ if x i = a then 1 else 0)) * a := by
      rcases h.1.eq_or_lt with hw | hw
      · simpa [← hw] using ha
      · exact add_pos_of_pos_of_nonneg (mul_pos hw hc) (mul_nonneg (sub_nonneg.mpr h.2) ha.le)
    linarith
  exact cubic_no_three_zeros (avg_pos hn hx) hb (mul_pos ha hc) hi.1
    (hi.2.trans hj.1) (hj.2.trans hk.1)
    (score_stationary_cubic hn hx ha hc hs hi.1 hzi)
    (score_stationary_cubic hn hx ha hc hs (hu.trans hj.1) hzj)
    (score_stationary_cubic hn hx ha hc hs ((hu.trans huv).trans hk.1) hzk)

private theorem F_eq_gap (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (r : ℝ) :
    F x r = gap x (r * avg x) := by
  have hm := (avg_pos hn hx).ne'
  have h (i : Fin n) : r * avg x * y x i = r * x i := by
    unfold y
    field_simp [hm]
  simp only [F, gap, A, h, Ar]

private theorem joint_to_profile (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ × ℝ} (hp : p ∈ domain)
    (hmax : IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) :
    p.1 = shape x p.2 ∧ IsMaxOn (F x) (Ioi 0) p.2 := by
  have hsh := shape_profile hn hx hp.2
  have he : p.1 = shape x p.2 := by
    by_contra h
    exact (not_le_of_gt (hsh.2 p.1 hp.1 h))
      (hmax (show (shape x p.2, p.2) ∈ domain from ⟨hsh.1, hp.2⟩))
  refine ⟨he, ?_⟩
  intro t ht
  have h := hmax (show (shape x t, t) ∈ domain from ⟨(shape_profile hn hx ht).1, ht⟩)
  change logLik x (shape x t) t ≤ logLik x p.1 p.2 at h
  rw [he] at h
  have hle := sub_le_sub_right h (bestExp x)
  rw [profile_gap hn hx ht, profile_gap hn hx hp.2, ← F_eq_gap hn hx, ← F_eq_gap hn hx] at hle
  exact le_of_mul_le_mul_left hle (cast_pos hn)

private theorem stationary_score (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {r : ℝ} (hr : 0 < r) (hz : deriv (F x) r = 0) : H x r = 0 := by
  rw [(f_deriv hn hx hr).deriv] at hz
  exact (mul_eq_zero.mp ((div_eq_zero_iff.mp hz).resolve_right hr.ne')).resolve_left
    (div_pos (U_pos hn hx hr.le) (Ar_pos hn hx hr)).ne'

private theorem same_max_rate (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) (hs : ∀ i, x i = a ∨ x i = c)
    {u v : ℝ} (hu : 0 < u) (hv : 0 < v)
    (hmu : IsMaxOn (F x) (Ioi 0) u) (hmv : IsMaxOn (F x) (Ioi 0) v) : u = v := by
  have h (p q : ℝ) (hp : 0 < p) (hq : 0 < q)
      (hmp : IsMaxOn (F x) (Ioi 0) p) (hmq : IsMaxOn (F x) (Ioi 0) q) : ¬ p < q := by
    intro hpq
    obtain ⟨t, ht, hd⟩ := exists_deriv_eq_zero hpq
      (fun r hr ↦ (f_deriv hn hx (hp.trans_le hr.1)).continuousAt.continuousWithinAt)
      (le_antisymm (hmq hp) (hmp hq))
    exact no_three_score_zeros hn hx ha hc hs hp ht.1 ht.2
      (stationary_score hn hx hp (hmp.isLocalMax (Ioi_mem_nhds hp)).deriv_eq_zero)
      (stationary_score hn hx (hp.trans ht.1) hd)
      (stationary_score hn hx hq (hmq.isLocalMax (Ioi_mem_nhds hq)).deriv_eq_zero)
  exact le_antisymm (le_of_not_gt (h v u hv hu hmv hmu)) (le_of_not_gt (h u v hu hv hmu hmv))

/-- A nonempty positive sample taking at most two values has at most one
joint maximizer. Given attainment, that maximizer exists uniquely. -/
theorem joint_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hs : ∃ a c : ℝ, ∀ i, x i = a ∨ x i = c)
    (he : ∃ p ∈ domain, IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 q.2) domain p) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  obtain ⟨p, hp, hmp⟩ := he
  refine ⟨p, ⟨hp, hmp⟩, ?_⟩
  rintro q ⟨hq, hmq⟩
  obtain ⟨a, c, hs⟩ := hs
  have support : ∃ a c : ℝ, 0 < a ∧ 0 < c ∧ ∀ i, x i = a ∨ x i = c := by
    let i : Fin n := ⟨0, hn⟩
    by_cases ha : 0 < a
    · by_cases hc : 0 < c
      · exact ⟨a, c, ha, hc, hs⟩
      · exact ⟨a, a, ha, ha, fun j ↦ Or.inl ((hs j).resolve_right
          (fun hj ↦ hc (hj ▸ hx j)))⟩
    · have hc' (j : Fin n) : x j = c := (hs j).resolve_left (fun hj ↦ ha (hj ▸ hx j))
      exact ⟨c, c, hc' i ▸ hx i, hc' i ▸ hx i, fun j ↦ Or.inl (hc' j)⟩
  obtain ⟨a, c, ha, hc, hs⟩ := support
  obtain ⟨hsp, hfp⟩ := joint_to_profile hn hx hp hmp
  obtain ⟨hsq, hfq⟩ := joint_to_profile hn hx hq hmq
  have hrate := same_max_rate hn hx ha hc hs hq.2 hp.2 hfq hfp
  exact Prod.ext (by rw [hsq, hsp, hrate]) hrate

/-- The Lomax existence criterion also gives a unique maximizer on two-point data. -/
theorem joint_unique_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hs : ∃ a c : ℝ, ∀ i, x i = a ∨ x i = c) :
    (∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p) ↔
      ∃ u ∈ Ioo 0 (uBar x), psi x u ≤ 0 := by
  constructor
  · rintro ⟨p, ⟨hp, hm⟩, _⟩
    exact (exists_max_iff hn hx).mp ⟨p, hp, hm⟩
  · intro h
    exact joint_unique hn hx hs ((exists_max_iff hn hx).mpr h)

private theorem scaled_log_bound (k : ℕ) {t q : ℝ} (ht : |t| < 1)
    (hq : q = 2^k * (1-t)) :
    |log q - ((k : ℝ) * log 2 -
      ∑ i ∈ Finset.range 12, t^(i+1)/(i+1))| ≤ |t|^13/(1-|t|) := by
  rw [hq, log_mul (by positivity) (sub_pos.mpr ((abs_lt.mp ht).2)).ne', log_pow]
  convert abs_log_sub_add_sum_range_le ht 12 using 2
  ring

private def exampleSample : Fin 4 → ℝ := ![1, 3000, 3000, 22000]

private theorem example_positive (i : Fin 4) : 0 < exampleSample i := by
  fin_cases i <;> norm_num [exampleSample]

private theorem F_analytic (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    AnalyticOnNhd ℝ (F x) (Ioi 0) := by
  intro r hr
  have hpos : 0 < r := hr
  have hai : AnalyticAt ℝ (Ar x) r := by
    unfold Ar avg
    apply AnalyticAt.div (g := fun _ ↦ (n : ℝ)) _ analyticAt_const (cast_pos hn).ne'
    apply Finset.analyticAt_fun_sum
    intro i _
    have hl : AnalyticAt ℝ (fun t ↦ 1 + t * x i) r := by fun_prop
    exact hl.log (factor_pos hx hpos.le i)
  have hmean : AnalyticAt ℝ (fun t ↦ t * avg x) r := by fun_prop
  exact ((hmean.log (mul_pos hpos (avg_pos hn hx))).sub
    (hai.log (Ar_pos hn hx hpos))).sub hai

private theorem strict_peak (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) (hab : a < b) (hsa : 0 < H x a) (hsb : H x b < 0) :
    ∃ r ∈ Ioo a b, ∀ᶠ t in 𝓝[≠] r, F x t < F x r := by
  have hb := ha.trans hab
  have hda := f_deriv hn hx ha
  have hdb := f_deriv hn hx hb
  have hpa := div_pos (mul_pos (div_pos (U_pos hn hx ha.le) (Ar_pos hn hx ha)) hsa) ha
  have hnb := div_neg_of_neg_of_pos
    (mul_neg_of_pos_of_neg (div_pos (U_pos hn hx hb.le) (Ar_pos hn hx hb)) hsb) hb
  obtain ⟨r, hr, hm⟩ := isCompact_Icc.exists_isMaxOn (nonempty_Icc.mpr hab.le)
    (fun t ht ↦ (f_deriv hn hx (ha.trans_le ht.1)).continuousAt.continuousWithinAt :
      ContinuousOn (F x) (Icc a b))
  have hne₁ : r ≠ a := by
    intro he
    have hma : IsMaxOn (F x) (Icc a b) a := he ▸ hm
    have ht := sub_mem_posTangentConeAt_of_segment_subset
      ((convex_Icc a b).segment_subset (left_mem_Icc.mpr hab.le) (right_mem_Icc.mpr hab.le))
    have hc := hma.isLocalMaxOn.hasFDerivWithinAt_nonpos hda.hasFDerivAt.hasFDerivWithinAt ht
    exact (not_le_of_gt (mul_pos (sub_pos.mpr hab) hpa)) (by simpa [smul_eq_mul] using hc)
  have hne₂ : r ≠ b := by
    intro he
    have hmb : IsMaxOn (F x) (Icc a b) b := he ▸ hm
    have ht := sub_mem_posTangentConeAt_of_segment_subset
      ((convex_Icc a b).segment_subset (right_mem_Icc.mpr hab.le) (left_mem_Icc.mpr hab.le))
    have hc := hmb.isLocalMaxOn.hasFDerivWithinAt_nonpos hdb.hasFDerivAt.hasFDerivWithinAt ht
    exact (not_le_of_gt (mul_pos_of_neg_of_neg (sub_neg.mpr hab) hnb))
      (by simpa [smul_eq_mul] using hc)
  have hri : r ∈ Ioo a b := ⟨lt_of_le_of_ne hr.1 hne₁.symm, lt_of_le_of_ne hr.2 hne₂⟩
  have hrp : r ∈ Ioi (0 : ℝ) := ha.trans hri.1
  have han := F_analytic hn hx
  refine ⟨r, hri, ?_⟩
  have hne : ∀ᶠ t in 𝓝[≠] r, F x t ≠ F x r := by
    refine ((han r hrp).eventually_eq_or_eventually_ne
      (analyticAt_const (v := F x r))).resolve_left ?_
    intro he
    have hall := han.eqOn_of_preconnected_of_eventuallyEq
      (fun _ _ ↦ analyticAt_const) (convex_Ioi (0 : ℝ)).isPreconnected hrp he
    have hev : F x =ᶠ[𝓝 a] fun _ ↦ F x r := by
      filter_upwards [Ioi_mem_nhds ha] with t ht
      exact hall ht
    have hz := (hda.congr_of_eventuallyEq hev.symm).deriv
    rw [deriv_const] at hz
    exact (ne_of_gt hpa) hz.symm
  have hm' := hm.isLocalMax (mem_of_superset (Ioo_mem_nhds hri.1 hri.2) Ioo_subset_Icc_self)
  filter_upwards [hne, hm'.filter_mono nhdsWithin_le_nhds] with t ht hle
  exact lt_of_le_of_ne hle ht

private theorem example_sign_1 : 0 < H exampleSample (1/100000) := by
  have h1 : (-3/10000 : ℝ) < log (100001/100000 : ℝ) := by
    have h := scaled_log_bound 1 (t := 99999/200000) (q := 100001/100000)
      (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  have h2 : (147/5000 : ℝ) < log (103/100 : ℝ) := by
    have h := scaled_log_bound 1 (t := 97/200) (q := 103/100) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  have h3 : (497/2500 : ℝ) < log (61/50 : ℝ) := by
    have h := scaled_log_bound 1 (t := 39/100) (q := 61/50) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  norm_num [H, Ar, B, U, avg, exampleSample, Fin.sum_univ_succ]
  linarith only [h1, h2, h3]

private theorem example_sign_2 : H exampleSample (1/1000) < 0 := by
  have h1 : log (1001/1000 : ℝ) < (13/10000 : ℝ) := by
    have h := scaled_log_bound 1 (t := 999/2000) (q := 1001/1000) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  have h2 : log (4 : ℝ) < (13863/10000 : ℝ) := by
    have h := scaled_log_bound 2 (t := 0) (q := 4) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  have h3 : log (23 : ℝ) < (6271/2000 : ℝ) := by
    have h := scaled_log_bound 5 (t := 9/32) (q := 23) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  norm_num [H, Ar, B, U, avg, exampleSample, Fin.sum_univ_succ]
  linarith only [h1, h2, h3]

private theorem example_sign_3 : 0 < H exampleSample (1/10) := by
  have h1 : (119/1250 : ℝ) < log (11/10 : ℝ) := by
    have h := scaled_log_bound 1 (t := 9/20) (q := 11/10) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  have h2 : (5707/1000 : ℝ) < log (301 : ℝ) := by
    have h := scaled_log_bound 9 (t := 211/512) (q := 301) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  have h3 : (15393/2000 : ℝ) < log (2201 : ℝ) := by
    have h := scaled_log_bound 12 (t := 1895/4096) (q := 2201) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).1
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_gt_d9]
  norm_num [H, Ar, B, U, avg, exampleSample, Fin.sum_univ_succ]
  linarith only [h1, h2, h3]

private theorem example_sign_4 : H exampleSample (1) < 0 := by
  have h1 : log (2 : ℝ) < (1733/2500 : ℝ) := by
    have h := scaled_log_bound 1 (t := 0) (q := 2) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  have h2 : log (3001 : ℝ) < (20017/2500 : ℝ) := by
    have h := scaled_log_bound 12 (t := 1095/4096) (q := 3001) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  have h3 : log (22001 : ℝ) < (99989/10000 : ℝ) := by
    have h := scaled_log_bound 15 (t := 10767/32768) (q := 22001) (by norm_num) (by norm_num)
    have h' := (abs_le.mp h).2
    norm_num [Finset.sum_range_succ] at h'
    linarith only [h', log_two_lt_d9]
  norm_num [H, Ar, B, U, avg, exampleSample, Fin.sum_univ_succ]
  linarith only [h1, h2, h3]

/-- The sample (1, 3000, 3000, 22000) has two strict local profile maxima
in disjoint positive rate brackets. -/
theorem exists_two_strict_local_maxima :
    ∃ x : Fin 4 → ℝ, (∀ i, 0 < x i) ∧
      ∃ u ∈ Ioo (1/100000 : ℝ) (1/1000), ∃ v ∈ Ioo (1/10 : ℝ) 1,
        (∀ᶠ t in 𝓝[≠] u, F x t < F x u) ∧ (∀ᶠ t in 𝓝[≠] v, F x t < F x v) := by
  obtain ⟨u, hu, hmu⟩ := strict_peak (by decide) example_positive
    (by norm_num) (by norm_num) example_sign_1 example_sign_2
  obtain ⟨v, hv, hmv⟩ := strict_peak (by decide) example_positive
    (by norm_num) (by norm_num) example_sign_3 example_sign_4
  exact ⟨exampleSample, example_positive, u, hu, v, hv, hmu, hmv⟩

end
end UnivariateML.LomaxTwoPoint
