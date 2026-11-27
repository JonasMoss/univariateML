module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Calculus.Deriv.Polynomial
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.Calculus.Deriv.Slope
public import Mathlib.Topology.Order.Compact
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import UnivariateML.Generic.Profile
import Mathlib.Tactic

set_option linter.style.header false

/-! Poisson–inverse Gaussian likelihood on samples containing a positive count.
The Bessel-argument chart gives the exact mean profile. Strict shape curvature
and the Poisson endpoint score determine whether the supremum is attained.
The chart covers all positive means and shapes. Shape zero is incompatible
with the positive-count samples in the main theorem. -/

@[expose] public section

namespace UnivariateML.Discrete3PIG

open Set Real Filter
open scoped Topology

/-- The part of the log likelihood that varies with (t=lambda/mu)
when the Bessel argument (z) is fixed. -/
noncomputable def fixedArgument (n S z t : ℝ) : ℝ :=
  n * t + S * log ((z ^ 2 - t ^ 2) / 2)

/-- A point at the sample mean is the strict maximizer at its Bessel argument. -/
theorem fixedArgument_strict {n S z t u : ℝ}  (hS : 0 < S)
    (ht : t ∈ Ioo 0 z) (hu : u ∈ Ioo 0 z)
    (hroot : n * (z ^ 2 - t ^ 2) = 2 * S * t) (hne : u ≠ t) :
    fixedArgument n S z u < fixedArgument n S z t := by
  have ht' := ht.1
  have hu' := hu.1
  have htz := ht.2
  have huz := hu.2
  have hz : 0 < z := ht'.trans htz
  have htpos : 0 < (z ^ 2 - t ^ 2) / 2 := by nlinarith [mul_pos (sub_pos.mpr htz) (show 0 < z + t by positivity)]
  have hupos : 0 < (z ^ 2 - u ^ 2) / 2 := by nlinarith [mul_pos (sub_pos.mpr huz) (show 0 < z + u by positivity)]
  have hlog := Real.log_le_sub_one_of_pos (div_pos hupos htpos)
  rw [Real.log_div hupos.ne' htpos.ne'] at hlog
  have hmul := mul_le_mul_of_nonneg_right hlog hS.le
  have hsquare : 0 < (u - t) ^ 2 := sq_pos_of_ne_zero (sub_ne_zero.mpr hne)
  have hb : (z ^ 2 - u ^ 2) / (z ^ 2 - t ^ 2) - 1 = (t ^ 2 - u ^ 2) / (z ^ 2 - t ^ 2) := by
    field_simp [show z ^ 2 - t ^ 2 ≠ 0 by linarith]
    ring
  have hr : ((z ^ 2 - u ^ 2) / 2) / ((z ^ 2 - t ^ 2) / 2) =
      (z ^ 2 - u ^ 2) / (z ^ 2 - t ^ 2) := by field_simp [htpos.ne']
  rw [hr, hb] at hmul
  have hbound : (log ((z ^ 2 - u ^ 2) / 2) - log ((z ^ 2 - t ^ 2) / 2)) * S *
      (z ^ 2 - t ^ 2) ≤ (t ^ 2 - u ^ 2) * S := by
    apply (le_div_iff₀ (show 0 < z ^ 2 - t ^ 2 by linarith)).mp
    rw [div_mul_eq_mul_div] at hmul
    exact hmul
  have hterm := congrArg (fun a : ℝ ↦ a * (u - t)) hroot
  have hneg : 0 < S * (u - t)^2 := mul_pos hS hsquare
  unfold fixedArgument
  apply lt_of_sub_neg
  apply neg_of_mul_neg_left (b := z ^ 2 - t ^ 2) _ (by linarith)
  nlinarith only [hbound, hterm, hneg]

/-- Held-mean binary-sample log likelihood relative to its Poisson value. -/
noncomputable def binaryGain (n S μ w : ℝ) : ℝ :=
  n * μ * w + S * (log (1-w) - log (1+w))

private theorem binary_deriv (n S μ : ℝ) {w : ℝ} (hw : w ∈ Ioo (-1 : ℝ) 1) :
    HasDerivAt (binaryGain n S μ) (n * μ - 2*S / (1-w^2)) w := by
  have hl : 0 < 1-w := sub_pos.mpr hw.2
  have hr : 0 < 1+w := by linarith [hw.1]
  have hl' := ((hasDerivAt_const w (1 : ℝ)).sub (hasDerivAt_id w)).log hl.ne'
  have hr' := ((hasDerivAt_const w (1 : ℝ)).add (hasDerivAt_id w)).log hr.ne'
  have hd := ((hasDerivAt_id w).const_mul (n*μ)).add ((hl'.sub hr').const_mul S)
  convert hd using 1
  · rfl
  · change _ = n*μ*1 + S*((0-1)/(1-w) - (0+1)/(1+w))
    rw [show 1-w^2 = (1-w)*(1+w) by ring]
    field_simp [hl.ne', hr.ne']
    ring

/-- On positive binary data the held-mean Poisson face wins exactly when the
held mean is at most twice the sample mean. This includes score equality. -/
theorem binary_poisson_iff {n S μ : ℝ} (hS : 0 < S) :
    (∀ w ∈ Ioo (0 : ℝ) 1, binaryGain n S μ w < 0) ↔ n*μ ≤ 2*S := by
  have hzero : binaryGain n S μ 0 = 0 := by simp [binaryGain]
  have hd (w : ℝ) (hw : w ∈ Ico (0 : ℝ) 1) :
      HasDerivAt (binaryGain n S μ) (n*μ - 2*S/(1-w^2)) w :=
    binary_deriv n S μ ⟨by linarith [hw.1], hw.2⟩
  constructor
  · intro h
    by_contra! hp
    let A := n*μ
    have hA : 0 < A := by dsimp [A]; linarith
    let q := (A-2*S)/(2*A)
    have hq : 0 < q := div_pos (sub_pos.mpr hp) (by positivity)
    have hqhalf : q < 1/2 := (div_lt_iff₀ (by positivity)).mpr (by linarith)
    let w := sqrt q
    have hw : 0 < w := sqrt_pos.mpr hq
    have hws : w^2 = q := sq_sqrt hq.le
    have hw1 : w < 1 := by nlinarith
    have hinc : StrictMonoOn (binaryGain n S μ) (Icc 0 w) := by
      apply strictMonoOn_of_deriv_pos (convex_Icc _ _)
        (fun t ht ↦ (hd t ⟨ht.1, ht.2.trans_lt hw1⟩).continuousAt.continuousWithinAt)
      intro t ht
      rw [interior_Icc] at ht
      rw [(hd t ⟨ht.1.le, ht.2.trans hw1⟩).deriv]
      apply sub_pos.mpr
      have hts : t^2 < q := by nlinarith [mul_pos (sub_pos.mpr ht.2) (add_pos hw ht.1)]
      have hden : 0 < 1-t^2 := by linarith
      apply (div_lt_iff₀ hden).mpr
      have hqa : A*q = (A-2*S)/2 := by dsimp [q]; field_simp
      have hlt := mul_lt_mul_of_pos_left hts hA
      change 2*S < A*(1-t^2)
      nlinarith only [hqa, hlt, hp]
    have hwgain := hinc (show 0 ∈ Icc 0 w from ⟨le_rfl, hw.le⟩)
      (show w ∈ Icc 0 w from ⟨hw.le, le_rfl⟩) hw
    rw [hzero] at hwgain
    exact (not_lt_of_ge hwgain.le) (h w ⟨hw, hw1⟩)
  · intro h w hw
    have hanti : StrictAntiOn (binaryGain n S μ) (Ico (0 : ℝ) 1) := by
      apply strictAntiOn_of_deriv_neg (convex_Ico _ _)
        (fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt)
      intro t ht
      rw [interior_Ico] at ht
      rw [(hd t ⟨ht.1.le, ht.2⟩).deriv]
      have hp : 0 < 1-t^2 := by nlinarith [mul_pos (sub_pos.mpr ht.2) (show 0 < 1+t by linarith [ht.1])]
      have hh : 2*S < 2*S/(1-t^2) :=
        (lt_div_iff₀ hp).mpr (by nlinarith [mul_pos hS (sq_pos_of_pos ht.1)])
      linarith
    rw [← hzero]
    exact hanti ⟨le_rfl, zero_lt_one⟩ ⟨hw.1.le, hw.2⟩ hw.1

-- A differentiable function that starts negative cannot first reach zero
-- at a point where its derivative is strictly negative.
private theorem negative_of_crossing {f : ℝ → ℝ}
    (hd : DifferentiableOn ℝ f (Ioi 0))
    (hinitial : ∀ b > 0, ∃ a ∈ Ioo 0 b, f a < 0)
    (hcross : ∀ z > 0, f z = 0 → deriv f z < 0) :
    ∀ b > 0, f b < 0 := by
  intro b hb
  by_contra! hfb
  obtain ⟨a, ha, hfa⟩ := hinitial b hb
  have hc : ContinuousOn f (Icc a b) := hd.continuousOn.mono (fun x hx ↦ ha.1.trans_le hx.1)
  have hzNonempty : (Icc a b ∩ f ⁻¹' {0}).Nonempty :=
    intermediate_value_Icc ha.2.le hc ⟨hfa.le, hfb⟩
  have hzCompact : IsCompact (Icc a b ∩ f ⁻¹' {0}) :=
    isCompact_Icc.of_isClosed_subset
      ((IsClosed.inter_preimage_val_iff isClosed_Icc).mp
        (isClosed_singleton.preimage hc.domRestrict)) inter_subset_left
  obtain ⟨z, hz, hmin⟩ := hzCompact.exists_isMinOn hzNonempty continuous_id.continuousOn
  have hz0 : f z = 0 := hz.2
  have haz : a < z := lt_of_le_of_ne hz.1.1 (by
    intro h
    rw [← h] at hz0
    exact hfa.ne hz0)
  have hdz : HasDerivAt f (deriv f z) z :=
    (hd.differentiableAt (Ioi_mem_nhds (ha.1.trans haz))).hasDerivAt
  have hs := hdz.tendsto_slope.mono_left (nhdsLT_le_nhdsNE z)
  have hsneg := hs.eventually (Iio_mem_nhds (hcross z (ha.1.trans haz) hz0))
  obtain ⟨y, hy, hyneg⟩ := (show ∀ᶠ y in 𝓝[<] z, y ∈ Ioo a z ∧ slope f z y < 0 from
    Filter.Eventually.and (Ioo_mem_nhdsLT haz) hsneg).exists
  have hfy : 0 < f y := by
    rw [slope_def_field, hz0, sub_zero] at hyneg
    rcases div_neg_iff.mp hyneg with h | h
    · exact h.1
    · linarith [hy.2]
  have hcy := hc.mono (Icc_subset_Icc_right (hy.2.le.trans hz.1.2))
  obtain ⟨u, hu, huz⟩ := intermediate_value_Icc hy.1.le hcy ⟨hfa.le, hfy.le⟩
  exact (not_lt_of_ge (hmin ⟨⟨hu.1, hu.2.trans (hy.2.le.trans hz.1.2)⟩, huz⟩))
    (hu.2.trans_lt hy.2)

private noncomputable def varianceBarrier (k e : ℝ) : ℝ :=
  e * (k^2-e^2) / (k^2+e^2)

private theorem barrier_deriv {k e : ℝ} (hk : 0 < k) :
    HasDerivAt (varianceBarrier k)
      ((k^4-4*k^2*e^2-e^4)/(k^2+e^2)^2) e := by
  have hden : 0 < k^2+e^2 := by positivity
  have hd := ((hasDerivAt_id e).mul
    ((hasDerivAt_const e (k^2)).sub ((hasDerivAt_id e).pow 2))).div
      ((hasDerivAt_const e (k^2)).add ((hasDerivAt_id e).pow 2)) hden.ne'
  convert hd using 1
  · rfl
  · simp only [Pi.add_apply, Pi.sub_apply, Pi.mul_apply, Pi.pow_apply, id_eq, zero_add, zero_sub, mul_one, one_mul]
    congr 1
    ring

private theorem crossing_identity {k e y : ℝ} (hk : 0 < k) (he : 0 < e)
    (_hy : 0 < y) (hrel : varianceBarrier k e = k*(k-1)-(2/y+1)*e-e^2) :
    2*e/y - (2/y+1+2*e)*varianceBarrier k e -
        ((k^4-4*k^2*e^2-e^4)/(k^2+e^2)^2)*varianceBarrier k e =
      -(2*e*k*(e+k)^2*(e^3+e^2*k+ k^2*(k-e))/(e^2+k^2)^3) := by
  let b := varianceBarrier k e
  have hd : k^2+e^2 ≠ 0 := by positivity
  have hz : 2/y = (k*(k-1)-e-e^2-b)/e := by
    apply (eq_div_iff he.ne').2
    dsimp only [b]
    linarith only [hrel]
  have hy' : 2*e/y = (2/y)*e := by ring
  rw [hy', hz]
  dsimp only [b, varianceBarrier]
  rw [show e^2+k^2 = k^2+e^2 by ring]
  field_simp [he.ne', hd]
  ring

/-- Strict variance bound from the two Bessel moment identities and the
negative sign of the variance difference near zero inverse argument. -/
private theorem variance_bound_from_moments {k : ℝ} (hk : 0 < k)
    {E V : ℝ → ℝ}
    (hE : ∀ y > 0, 0 < E y ∧ E y < k)
    (hdE : ∀ y > 0, HasDerivAt E (V y / y) y)
    (hV : ∀ y > 0, V y = k*(k-1)-(2/y+1)*E y-(E y)^2)
    (hstart : ∀ b > 0, ∃ a ∈ Ioo 0 b, V a < varianceBarrier k (E a)) :
    ∀ y > 0, V y < varianceBarrier k (E y) := by
  let f := fun y ↦ k*(k-1)-(2/y+1)*E y-(E y)^2 - varianceBarrier k (E y)
  have hd (y : ℝ) (hy : 0 < y) :
      HasDerivAt f
        ((2 / y^2) * E y - (2/y+1+2*E y)*(V y/y) -
          ((k^4-4*k^2*(E y)^2-(E y)^4)/(k^2+(E y)^2)^2)*(V y/y)) y := by
    have hr := (((hasDerivAt_const y (2 : ℝ)).div (hasDerivAt_id y) hy.ne').add_const 1)
    have hh := (((hasDerivAt_const y (k*(k-1))).sub (hr.mul (hdE y hy))).sub
      ((hdE y hy).pow 2)).sub ((barrier_deriv hk).comp y (hdE y hy))
    convert hh using 1
    · rfl
    · simp only [Pi.div_apply, id_eq, zero_mul, zero_sub, mul_one]
      ring
  have hneg := negative_of_crossing
    (f := f) (fun y hy ↦ (hd y hy).differentiableAt.differentiableWithinAt)
    (fun b hb ↦ by
      obtain ⟨a, ha, hlt⟩ := hstart b hb
      exact ⟨a, ha, by dsimp [f]; rw [← hV a ha.1]; exact sub_neg.mpr hlt⟩)
    (fun y hy hroot ↦ by
      have hroot' : V y = varianceBarrier k (E y) := by
        dsimp [f] at hroot
        linarith only [hroot, hV y hy]
      have hcross := crossing_identity hk (hE y hy).1 hy (hroot'.symm.trans (hV y hy))
      rw [(hd y hy).deriv]
      apply neg_of_mul_neg_left (b := y) _ hy.le
      have hneg : -(2 * E y * k * (E y+k)^2 *
          ((E y)^3+(E y)^2*k+k^2*(k-E y))/( (E y)^2+k^2)^3) < 0 := by
        have he := (hE y hy).1
        have hke := sub_pos.mpr (hE y hy).2
        exact neg_neg_of_pos (by positivity)
      have hscaled : ((2/y^2)*E y-(2/y+1+2*E y)*(V y/y)-
          ((k^4-4*k^2*(E y)^2-(E y)^4)/(k^2+(E y)^2)^2)*(V y/y))*y =
          2*E y/y-(2/y+1+2*E y)*V y-
            ((k^4-4*k^2*(E y)^2-(E y)^4)/(k^2+(E y)^2)^2)*V y := by
        field_simp [hy.ne']
      rwa [hscaled, hroot', hcross])
  intro y hy
  have hh := hneg y hy
  dsimp [f] at hh
  rw [← hV y hy] at hh
  exact sub_neg.mp hh

open Polynomial
open scoped Polynomial BigOperators

private noncomputable def besselCoeff (k : ℕ) : ℕ → ℝ
  | 0 => 1
  | j+1 => ((k : ℝ)*((k : ℝ)-1) - (j : ℝ)*((j : ℝ)+1)) /
      (2*((j : ℝ)+1)) * besselCoeff k j

/-- The half-integer Bessel polynomial of order `k - 1` for a positive count. -/
@[no_expose] noncomputable def besselPolynomial (k : ℕ) : ℝ[X] :=
  ∑ j ∈ Finset.range k, C (besselCoeff k j) * X^j

private theorem besselCoeff_zero_of_le {k j : ℕ} (hk : 0 < k) (hj : k ≤ j) :
    besselCoeff k j = 0 := by
  induction j with
  | zero => omega
  | succ j ih =>
    rw [besselCoeff]
    by_cases h : k ≤ j
    · rw [ih h, mul_zero]
    · have he : k = j + 1 := by omega
      subst k
      push_cast
      ring

private theorem besselCoeff_pos {k j : ℕ} (hj : j < k) : 0 < besselCoeff k j := by
  induction j with
  | zero => simp [besselCoeff]
  | succ j ih =>
    rw [besselCoeff]
    apply mul_pos _ (ih (by omega))
    apply div_pos _ (by positivity)
    have h : (j : ℝ)+1 < k := by exact_mod_cast hj
    have hj' : 0 ≤ (j : ℝ) := Nat.cast_nonneg _
    nlinarith [mul_pos (sub_pos.mpr h) (show 0 < (k : ℝ) + j by linarith)]

private theorem coeff_besselPolynomial {k : ℕ} (hk : 0 < k) (j : ℕ) :
    (besselPolynomial k).coeff j = besselCoeff k j := by
  classical
  simp only [besselPolynomial, finsetSum_coeff, coeff_C_mul_X_pow]
  rw [Finset.sum_ite_eq]
  by_cases hj : j < k
  · simp [hj]
  · simp [hj, besselCoeff_zero_of_le hk (by omega : k ≤ j)]

private theorem bessel_ode {k : ℕ} (hk : 0 < k) :
    X^2 * (besselPolynomial k).derivative.derivative +
      C 2 * X * (besselPolynomial k).derivative + C 2 * (besselPolynomial k).derivative =
        C ((k : ℝ)*((k : ℝ)-1)) * besselPolynomial k := by
  let Q := besselPolynomial k
  change X^2 * Q.derivative.derivative + C 2 * X * Q.derivative +
    C 2 * Q.derivative = C ((k : ℝ)*((k : ℝ)-1)) * Q
  have hrec (j : ℕ) : 2*((j : ℝ)+1)*besselCoeff k (j+1) =
      ((k : ℝ)*((k : ℝ)-1)-(j : ℝ)*((j : ℝ)+1))*besselCoeff k j := by
    rw [besselCoeff]
    field_simp
  ext j
  have hcoeff (i : ℕ) : Q.coeff i = besselCoeff k i := coeff_besselPolynomial hk i
  rcases j with _ | _ | j
  · simpa [coeff_X_pow_mul', coeff_derivative, hcoeff, mul_assoc] using hrec 0
  · simp only [coeff_add, mul_assoc, coeff_C_mul, coeff_X_pow_mul', coeff_X_mul, coeff_derivative, hcoeff]
    norm_num
    have h := hrec 1
    norm_num at h
    nlinarith only [h]
  · have hj : 2 ≤ j.succ.succ := by omega
    have h : j.succ.succ - 2 = j := by omega
    simp only [coeff_add, mul_assoc, coeff_C_mul, coeff_X_pow_mul',
      ite_eq_left hj, coeff_X_mul, h, coeff_derivative, hcoeff]
    push_cast
    have hh := hrec j.succ.succ
    push_cast at hh
    nlinarith only [hh]

private theorem bessel_eval_pos {k : ℕ} (hk : 0 < k) {y : ℝ} (hy : 0 ≤ y) :
    0 < (besselPolynomial k).eval y := by
  unfold besselPolynomial
  simp only [Polynomial.eval_finsetSum, eval_mul, eval_C, eval_pow, eval_X]
  apply Finset.sum_pos' (fun j hj ↦
    mul_nonneg (besselCoeff_pos (Finset.mem_range.mp hj)).le (pow_nonneg hy j))
  exact ⟨0, Finset.mem_range.mpr hk, by simp [besselCoeff]⟩

private theorem bessel_first_moment (k : ℕ) (hk : 0 < k) (y : ℝ) :
    y*(besselPolynomial k).derivative.eval y =
      ∑ j ∈ Finset.range k, (j : ℝ)*(besselCoeff k j*y^j) := by
  let Q := besselPolynomial k
  have h : X * Q.derivative =
      ∑ j ∈ Finset.range k, C ((j : ℝ)*besselCoeff k j) * X^j := by
    classical
    ext j
    simp only [finsetSum_coeff, coeff_C_mul_X_pow]
    rw [Finset.sum_ite_eq]
    by_cases hj : j < k
    · rw [ite_eq_left (Finset.mem_range.mpr hj)]
      cases j with
      | zero => simp
      | succ j =>
        rw [coeff_X_mul, coeff_derivative, coeff_besselPolynomial hk]
        push_cast
        ring
    · rw [ite_eq_right (fun h ↦ hj (Finset.mem_range.mp h))]
      cases j with
      | zero => simp
      | succ j =>
        rw [coeff_X_mul, coeff_derivative, coeff_besselPolynomial hk,
          besselCoeff_zero_of_le hk (show k ≤ j+1 by omega), zero_mul]
  have he := congrArg (fun p : ℝ[X] ↦ p.eval y) h
  simp only [eval_mul, eval_X, Polynomial.eval_finsetSum, eval_C, eval_pow, mul_assoc] at he
  exact he

/-- Mean of the polynomial degree with weights `aⱼ yʲ`. -/
noncomputable def degreeMean (k : ℕ) (y : ℝ) : ℝ :=
  y * ((besselPolynomial k).derivative.eval y / (besselPolynomial k).eval y)

/-- Variance of the polynomial degree with weights `aⱼ yʲ`, in terms of
the first and second factorial moments. -/
noncomputable def degreeVariance (k : ℕ) (y : ℝ) : ℝ :=
  degreeMean k y + y^2 *
    ((besselPolynomial k).derivative.derivative.eval y / (besselPolynomial k).eval y -
      ((besselPolynomial k).derivative.eval y / (besselPolynomial k).eval y)^2)

private theorem degreeMean_bounds {k : ℕ} (hk : 2 ≤ k) {y : ℝ} (hy : 0 < y) :
    0 < degreeMean k y ∧ degreeMean k y < k := by
  let Q := besselPolynomial k
  have hk' : 0 < k := by omega
  have hQ : 0 < Q.eval y := bessel_eval_pos hk' hy.le
  have hm := bessel_first_moment k hk' y
  unfold degreeMean
  rw [← mul_div_assoc, hm]
  constructor
  · apply div_pos _ hQ
    apply Finset.sum_pos' (fun j hj ↦ mul_nonneg (Nat.cast_nonneg j)
      (mul_nonneg (besselCoeff_pos (Finset.mem_range.mp hj)).le (pow_nonneg hy.le _)))
    exact ⟨1, Finset.mem_range.mpr (by omega), by
      simpa using mul_pos (besselCoeff_pos (show 1 < k by omega)) hy⟩
  · apply (div_lt_iff₀ hQ).mpr
    change _ < (k : ℝ) * Q.eval y
    unfold Q besselPolynomial
    simp only [Polynomial.eval_finsetSum, eval_mul, eval_C, eval_pow, eval_X, Finset.mul_sum]
    apply Finset.sum_lt_sum_of_nonempty ⟨0, Finset.mem_range.mpr hk'⟩
    intro j hj
    have hj' := Finset.mem_range.mp hj
    exact mul_lt_mul_of_pos_right (by exact_mod_cast hj') (mul_pos (besselCoeff_pos hj') (pow_pos hy _))

private theorem degreeMean_deriv {k : ℕ} (hk : 0 < k) {u : ℝ} (hu : 0 < u) :
    HasDerivAt (degreeMean k) (degreeVariance k u / u) u := by
  let Q := besselPolynomial k
  have hQ : Q.eval u ≠ 0 := (bessel_eval_pos hk hu.le).ne'
  have hL := (Q.derivative.hasDerivAt u).div (Q.hasDerivAt u) hQ
  have hE := (hasDerivAt_id u).mul hL
  convert hE using 1
  · rfl
  · change degreeVariance k u / u =
      1*(Q.derivative.eval u / Q.eval u) +
        u*((Q.derivative.derivative.eval u*Q.eval u -
          Q.derivative.eval u*Q.derivative.eval u)/(Q.eval u)^2)
    change (u*(Q.derivative.eval u/Q.eval u) + u^2*
      (Q.derivative.derivative.eval u/Q.eval u-(Q.derivative.eval u/Q.eval u)^2))/u = _
    field_simp [hu.ne', hQ]

/-- The strict Bessel-polynomial variance bound for every count at least two
and every positive inverse Bessel argument. -/
theorem bessel_variance_bound {k : ℕ} (hk : 2 ≤ k) {y : ℝ} (hy : 0 < y) :
    degreeVariance k y <
      degreeMean k y * ((k : ℝ)^2-(degreeMean k y)^2) / ((k : ℝ)^2+(degreeMean k y)^2) := by
  let Q := besselPolynomial k
  have hk' : 0 < k := by omega
  have hkR : (0 : ℝ) < k := by exact_mod_cast hk'
  have hkR1 : (1 : ℝ) < k := by exact_mod_cast hk
  have hQ (u : ℝ) (hu : 0 ≤ u) : 0 < Q.eval u := bessel_eval_pos hk' hu
  apply variance_bound_from_moments hkR (fun u hu ↦ degreeMean_bounds hk hu) (fun _ hu ↦ degreeMean_deriv hk' hu) _ _ y hy
  · intro u hu
    have h := congrArg (fun p : ℝ[X] ↦ p.eval u) (bessel_ode hk')
    simp only [eval_add, eval_mul, eval_pow, eval_X, eval_C] at h
    change u^2*Q.derivative.derivative.eval u + 2*u*Q.derivative.eval u +
      2*Q.derivative.eval u = (k : ℝ)*((k : ℝ)-1)*Q.eval u at h
    have hne := (hQ u hu.le).ne'
    have he : u^2*(Q.derivative.derivative.eval u/Q.eval u) +
        (2*u+2)*(Q.derivative.eval u/Q.eval u) = (k : ℝ)*((k : ℝ)-1) := by
      field_simp [hne]
      linear_combination h
    have hh : (2/u+1)*(u*(Q.derivative.eval u/Q.eval u)) =
        (2+u)*(Q.derivative.eval u/Q.eval u) := by
      field_simp [hu.ne', hne]
    change u*(Q.derivative.eval u/Q.eval u) + u^2*
        (Q.derivative.derivative.eval u/Q.eval u-(Q.derivative.eval u/Q.eval u)^2) =
      (k : ℝ)*((k : ℝ)-1)-(2/u+1)*(u*(Q.derivative.eval u/Q.eval u))-
        (u*(Q.derivative.eval u/Q.eval u))^2
    rw [hh]
    linear_combination he
  · let L := fun u ↦ Q.derivative.eval u / Q.eval u
    let A := fun u ↦ Q.derivative.derivative.eval u / Q.eval u - (L u)^2 +
      2*u*(L u)^3 / ((k : ℝ)^2+(u*L u)^2)
    have h0 : Q.eval 0 = 1 := by
      rw [← coeff_zero_eq_eval_zero, coeff_besselPolynomial hk']
      rfl
    have h1 : Q.derivative.eval 0 = (k : ℝ)*((k : ℝ)-1)/2 := by
      rw [← coeff_zero_eq_eval_zero, coeff_derivative, coeff_besselPolynomial hk']
      norm_num [besselCoeff]
    have h2 : Q.derivative.derivative.eval 0 =
        ((k : ℝ)*((k : ℝ)-1)-2)*((k : ℝ)*((k : ℝ)-1))/4 := by
      rw [← coeff_zero_eq_eval_zero, coeff_derivative, coeff_derivative,
        coeff_besselPolynomial hk']
      norm_num [besselCoeff]
      ring
    have hA0 : A 0 < 0 := by
      dsimp [A, L]
      rw [h0, h1, h2]
      norm_num
      nlinarith [mul_pos hkR (sub_pos.mpr hkR1)]
    have hQc (P : ℝ[X]) : ContinuousAt (fun u ↦ P.eval u) 0 :=
      (P.hasDerivAt 0).continuousAt
    have hL : ContinuousAt L 0 := (hQc _).div (hQc _) (by simp [h0])
    have hA : ContinuousAt A 0 := by
      apply ((hQc _).div (hQc _) (by simp [h0]) |>.sub (hL.pow 2)).add
      apply ((continuousAt_const.mul continuousAt_id).mul (hL.pow 3)).div
      · exact continuousAt_const.add ((continuousAt_id.mul hL).pow 2)
      · simp [hkR.ne']
    have hnear := hA.eventually (Iio_mem_nhds hA0)
    intro b hb
    have hmem : ∀ᶠ u in 𝓝[>] (0 : ℝ), u ∈ Ioo 0 b :=
      Ioo_mem_nhdsGT hb
    obtain ⟨a, ha, hneg⟩ := (hmem.and (hnear.filter_mono nhdsWithin_le_nhds)).exists
    refine ⟨a, ha, ?_⟩
    have hneg' : A a < 0 := hneg
    have hf : degreeVariance k a - varianceBarrier k (degreeMean k a) = a^2*A a := by
      change a*L a + a^2*(Q.derivative.derivative.eval a/Q.eval a-(L a)^2) -
        (a*L a*((k : ℝ)^2-(a*L a)^2)/((k : ℝ)^2+(a*L a)^2)) =
        a^2*(Q.derivative.derivative.eval a/Q.eval a-(L a)^2+
          2*a*(L a)^3/((k : ℝ)^2+(a*L a)^2))
      have hp : 0 < (k : ℝ)^2 + (a*L a)^2 := by positivity
      field_simp [hp.ne', (hQ a ha.1.le).ne']
      ring
    apply sub_neg.mp
    rw [hf]
    exact mul_neg_of_pos_of_neg (sq_pos_of_pos ha.1) hneg'

private noncomputable def inverseArgument (μ w : ℝ) : ℝ :=
  2*w/(μ*(1-w^2))

private noncomputable def argumentRatio (w : ℝ) : ℝ :=
  (1+w^2)/(w*(1-w^2))

private theorem one_sub_sq_pos {w : ℝ} (hw : w ∈ Ioo (-1 : ℝ) 1) :
    0 < 1-w^2 := by
  have h₁ : 0 < 1-w := sub_pos.mpr hw.2
  have h₂ : 0 < 1+w := by linarith [hw.1]
  nlinarith [mul_pos h₁ h₂]

private theorem inverseArgument_deriv {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ioo (-1 : ℝ) 1) :
    HasDerivAt (inverseArgument μ) (2*(1+w^2)/(μ*(1-w^2)^2)) w := by
  have h := one_sub_sq_pos hw
  have hden := ((hasDerivAt_const w (1 : ℝ)).sub ((hasDerivAt_id w).pow 2)).const_mul μ
  convert ((hasDerivAt_id w).const_mul 2).div hden (mul_pos hμ h).ne' using 1
  · rfl
  · change _ = (2*1*(μ*(1-w^2))-(2*w)*(μ*(0-2*w^(2-1)*1)))/(μ*(1-w^2))^2
    field_simp [hμ.ne', h.ne']
    ring

private theorem inverseArgument_deriv_ratio {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (inverseArgument μ) (inverseArgument μ w * argumentRatio w) w := by
  have hw' : w ∈ Ioo (-1 : ℝ) 1 := ⟨by linarith [hw.1], hw.2⟩
  convert inverseArgument_deriv hμ hw' using 1
  unfold inverseArgument argumentRatio
  field_simp [hμ.ne', hw.1.ne', (one_sub_sq_pos hw').ne']

private theorem argumentRatio_deriv {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt argumentRatio ((w^4+4*w^2-1)/(w^2*(1-w^2)^2)) w := by
  have hp := one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩
  have hsq := (hasDerivAt_id w).pow 2
  have hn := (hasDerivAt_const w (1 : ℝ)).add hsq
  have hd := (hasDerivAt_id w).mul ((hasDerivAt_const w (1 : ℝ)).sub hsq)
  convert hn.div hd (mul_pos hw.1 hp).ne' using 1
  · rfl
  · change _ =
      ((0+2*w^(2-1)*1)*(w*(1-w^2))-
        (1+w^2)*(1*(1-w^2)+w*(0-2*w^(2-1)*1)))/(w*(1-w^2))^2
    rw [mul_pow]
    congr 1
    ring

/-- Held-mean log mass relative to Poisson, extended to inverse argument zero. -/
@[no_expose] noncomputable def cellGain (k : ℕ) (μ w : ℝ) : ℝ :=
  binaryGain 1 k μ w + log ((besselPolynomial (max k 1)).eval (inverseArgument μ w))

private theorem besselPolynomial_one : besselPolynomial 1 = 1 := by
  simp [besselPolynomial, besselCoeff]

private theorem cellGain_deriv {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (cellGain k μ)
      (1*μ - 2*(k : ℝ)/(1-w^2) +
        degreeMean (max k 1) (inverseArgument μ w)*argumentRatio w) w := by
  have hw' : w ∈ Ioo (-1 : ℝ) 1 := ⟨by linarith [hw.1], hw.2⟩
  have hy : 0 < inverseArgument μ w :=
    div_pos (by linarith [hw.1]) (mul_pos hμ (one_sub_sq_pos hw'))
  let Q := besselPolynomial (max k 1)
  have hQ : 0 < Q.eval (inverseArgument μ w) := bessel_eval_pos (by omega) hy.le
  have hl := ((Q.hasDerivAt (inverseArgument μ w)).log hQ.ne').comp w
    (inverseArgument_deriv_ratio hμ hw)
  convert (binary_deriv 1 k μ hw').add hl using 1
  · rfl
  · change _ + (inverseArgument μ w *
      (Q.derivative.eval (inverseArgument μ w)/Q.eval (inverseArgument μ w))) * argumentRatio w =
        _ + (Q.derivative.eval (inverseArgument μ w)/Q.eval (inverseArgument μ w)) *
          (inverseArgument μ w * argumentRatio w)
    ring

private theorem curvature_factor {k e v w : ℝ} (hk : 0 < k) (he : 0 < e)
    (hw : w ∈ Ioo (0 : ℝ) 1) (hv : v < e*(k^2-e^2)/(k^2+e^2)) :
    -4*k*w + (e*(w^4+4*w^2-1)+v*(1+w^2)^2)/w^2 < 0 := by
  have hw2 := sq_pos_of_pos hw.1
  have hk2 : 0 < k^2+e^2 := by positivity
  have hpos : 0 < (1+w^2)^2/w^2 := by positivity
  have hi := mul_lt_mul_of_pos_right hv hpos
  have hfactor :
      -4*k*w + (e*(w^4+4*w^2-1)+(e*(k^2-e^2)/(k^2+e^2))*(1+w^2)^2)/w^2 =
        2*(e-k*w)^2*(e*w^2-e-2*k*w)/(w^2*(k^2+e^2)) := by
    field_simp [hw.1.ne', hk2.ne']
    ring
  have hn : e*w^2-e-2*k*w ≤ 0 := by
    have hp := mul_pos he (one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩)
    nlinarith [mul_pos hk hw.1]
  have hr : 2*(e-k*w)^2*(e*w^2-e-2*k*w)/(w^2*(k^2+e^2)) ≤ 0 :=
    div_nonpos_of_nonpos_of_nonneg (mul_nonpos_of_nonneg_of_nonpos (by positivity) hn) (by positivity)
  apply lt_of_lt_of_le _ hr
  rw [← hfactor]
  linear_combination hi

private theorem cellGain_second {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    HasDerivAt (deriv (cellGain k μ))
      ((-4*(k : ℝ)*w +
        (degreeMean (max k 1) (inverseArgument μ w)*(w^4+4*w^2-1)+
          degreeVariance (max k 1) (inverseArgument μ w)*(1+w^2)^2)/w^2) /
        (1-w^2)^2) w := by
  let y := inverseArgument μ w
  let m := max k 1
  let E := degreeMean m y
  let V := degreeVariance m y
  have hp := one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩
  have hy : 0 < y := div_pos (by linarith [hw.1]) (mul_pos hμ hp)
  have hy' := inverseArgument_deriv_ratio hμ hw
  have hE := (degreeMean_deriv (show 0 < m by dsimp [m]; omega) hy).comp w hy'
  have hR := argumentRatio_deriv hw
  have hden := (hasDerivAt_const w (1 : ℝ)).sub ((hasDerivAt_id w).pow 2)
  have hmain := ((hasDerivAt_const w (1*μ)).sub
    ((hasDerivAt_const w (2*(k : ℝ))).div hden hp.ne')).add (hE.mul hR)
  have hmain' : HasDerivAt (fun t ↦ 1*μ - 2*(k : ℝ)/(1-t^2) +
      degreeMean m (inverseArgument μ t)*argumentRatio t)
      ((-4*(k : ℝ)*w + (E*(w^4+4*w^2-1)+V*(1+w^2)^2)/w^2)/(1-w^2)^2) w := by
    convert hmain using 1
    · rfl
    · change _ = 0-(0*(1-w^2)-(2*(k : ℝ))*(0-2*w^(2-1)*1))/(1-w^2)^2 +
          (V/y*(y*argumentRatio w)*argumentRatio w +
            E*((w^4+4*w^2-1)/(w^2*(1-w^2)^2)))
      unfold argumentRatio
      field_simp [hw.1.ne', hp.ne', hy.ne']
      ring
  apply hmain'.congr_of_eventuallyEq
  filter_upwards [Ioo_mem_nhds hw.1 hw.2] with t ht
  exact (cellGain_deriv (k := k) hμ ht).deriv

private theorem cellGain_curvature {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    deriv (deriv (cellGain k μ)) w ≤ 0 ∧
      (0 < k → deriv (deriv (cellGain k μ)) w < 0) := by
  rw [(cellGain_second (k := k) hμ hw).deriv]
  have hp := one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩
  by_cases hk : 2 ≤ k
  · have hy : 0 < inverseArgument μ w :=
      div_pos (by linarith [hw.1]) (mul_pos hμ hp)
    have hm : max k 1 = k := max_eq_left (by omega)
    simp only [hm]
    have h := curvature_factor (k := (k : ℝ)) (by positivity)
      (degreeMean_bounds hk hy).1 hw (bessel_variance_bound hk hy)
    have hn := div_neg_of_neg_of_pos h (sq_pos_of_pos hp)
    exact ⟨hn.le, fun _ ↦ hn⟩
  · have hm : max k 1 = 1 := max_eq_right (by omega)
    have he : degreeMean (max k 1) (inverseArgument μ w) = 0 := by
      simp [hm, degreeMean, besselPolynomial_one]
    have hv : degreeVariance (max k 1) (inverseArgument μ w) = 0 := by
      simp [hm, degreeVariance, degreeMean, besselPolynomial_one]
    rw [he, hv, zero_mul, zero_mul, zero_add, zero_div, add_zero]
    constructor
    · have hn : -4*(k : ℝ)*w ≤ 0 := by
        have h₁ := mul_nonneg (Nat.cast_nonneg (α := ℝ) k) hw.1.le
        nlinarith
      exact div_nonpos_of_nonpos_of_nonneg hn (sq_nonneg _)
    · intro h
      have hn : -4*(k : ℝ)*w < 0 := by
        have h₁ : (0 : ℝ) < k := by exact_mod_cast h
        nlinarith [mul_pos h₁ hw.1]
      exact div_neg_of_neg_of_pos hn (sq_pos_of_pos hp)

/-- The cell log likelihood is strictly concave in compact shape for each positive count. -/
theorem cellGain_strictConcave {k : ℕ} (hk : 0 < k) {μ : ℝ} (hμ : 0 < μ) :
    StrictConcaveOn ℝ (Ioo 0 1) (cellGain k μ) := by
  apply strictConcaveOn_of_deriv2_neg' (convex_Ioo _ _)
  · exact fun w hw ↦ (cellGain_deriv hμ hw).continuousAt.continuousWithinAt
  · exact fun w hw ↦ (cellGain_curvature hμ hw).2 hk

private noncomputable def cellScore (k : ℕ) (μ w : ℝ) : ℝ :=
  1*μ - 2*(k : ℝ)/(1-w^2) +
    ((besselPolynomial (max k 1)).derivative.eval (inverseArgument μ w) /
      (besselPolynomial (max k 1)).eval (inverseArgument μ w)) *
      (2*(1+w^2)/(μ*(1-w^2)^2))

private theorem cellGain_deriv_closed_left {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ico (0 : ℝ) 1) :
    HasDerivAt (cellGain k μ) (cellScore k μ w) w := by
  have hw' : w ∈ Ioo (-1 : ℝ) 1 := ⟨by linarith [hw.1], hw.2⟩
  have hy : 0 ≤ inverseArgument μ w :=
    div_nonneg (by linarith [hw.1]) (mul_pos hμ (one_sub_sq_pos hw')).le
  let Q := besselPolynomial (max k 1)
  have hQ : 0 < Q.eval (inverseArgument μ w) := bessel_eval_pos (by omega) hy
  exact (binary_deriv 1 k μ hw').add
    (((Q.hasDerivAt _).log hQ.ne').comp w (inverseArgument_deriv hμ hw'))

private theorem cellScore_continuousAt {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ico (0 : ℝ) 1) : ContinuousAt (cellScore k μ) w := by
  have hw' : w ∈ Ioo (-1 : ℝ) 1 := ⟨by linarith [hw.1], hw.2⟩
  have hp := one_sub_sq_pos hw'
  have hy : 0 ≤ inverseArgument μ w := div_nonneg (by linarith [hw.1]) (mul_pos hμ hp).le
  let Q := besselPolynomial (max k 1)
  have hQ : Q.eval (inverseArgument μ w) ≠ 0 := (bessel_eval_pos (by omega) hy).ne'
  have hyc := (inverseArgument_deriv hμ hw').continuousAt
  have hQc (P : ℝ[X]) := (P.hasDerivAt (inverseArgument μ w)).continuousAt.comp hyc
  have hL := (hQc Q.derivative).div (hQc Q) hQ
  have hd : ContinuousAt (fun t : ℝ ↦ 1-t^2) w :=
    continuousAt_const.sub (continuousAt_id.pow 2)
  have hdy : ContinuousAt (fun t : ℝ ↦ 2*(1+t^2)/(μ*(1-t^2)^2)) w :=
    ((continuousAt_const.add (continuousAt_id.pow 2)).const_mul 2).div
      ((hd.pow 2).const_mul μ) (mul_pos hμ (sq_pos_of_pos hp)).ne'
  exact (continuousAt_const.sub (continuousAt_const.div hd hp.ne')).add (hL.mul hdy)

private theorem cellScore_zero (k : ℕ) {μ : ℝ} (_hμ : 0 < μ) :
    cellScore k μ 0 = μ-2*(k : ℝ)+(k : ℝ)*((k : ℝ)-1)/μ := by
  let K := max k 1
  have hK : 0 < K := by dsimp [K]; omega
  have hQ : (besselPolynomial K).eval 0 = 1 := by
    rw [← coeff_zero_eq_eval_zero, coeff_besselPolynomial hK]
    rfl
  have hQ' : (besselPolynomial K).derivative.eval 0 = (K : ℝ)*((K : ℝ)-1)/2 := by
    rw [← coeff_zero_eq_eval_zero, coeff_derivative, coeff_besselPolynomial hK]
    norm_num [besselCoeff]
  have hKval : (K : ℝ)*((K : ℝ)-1) = (k : ℝ)*((k : ℝ)-1) := by
    cases k with
    | zero => norm_num [K]
    | succ k => simp [K]
  unfold cellScore
  rw [show inverseArgument μ 0 = 0 by simp [inverseArgument]]
  change _ - _ + ((besselPolynomial K).derivative.eval 0 /
    (besselPolynomial K).eval 0)*_ = _
  rw [hQ', hQ, hKval]
  norm_num
  ring

/-- Sum of the log mass differences relative to the Poisson law at the same mean. -/
noncomputable def sampleGain {n : ℕ} (x : Fin n → ℕ) (μ w : ℝ) : ℝ :=
  ∑ i, cellGain (x i) μ w

private noncomputable def sampleScore {n : ℕ} (x : Fin n → ℕ) (μ w : ℝ) : ℝ :=
  ∑ i, cellScore (x i) μ w

private theorem sampleGain_deriv {n : ℕ} (x : Fin n → ℕ) {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ico (0 : ℝ) 1) :
    HasDerivAt (sampleGain x μ) (sampleScore x μ w) w :=
  HasDerivAt.fun_sum (fun i _ ↦ cellGain_deriv_closed_left (k := x i) hμ hw)

private theorem sampleScore_strictAnti {n : ℕ} {x : Fin n → ℕ} {μ : ℝ} (hμ : 0 < μ)
    (hx : ∃ i, 0 < x i) : StrictAntiOn (sampleScore x μ) (Ico 0 1) := by
  apply strictAntiOn_of_deriv_neg (convex_Ico _ _)
  · exact continuousOn_finsetSum _ (fun i _ ↦ fun w hw ↦
      (cellScore_continuousAt (k := x i) hμ hw).continuousWithinAt)
  · intro w hw
    rw [interior_Ico] at hw
    have hdc (i : Fin n) : HasDerivAt (cellScore (x i) μ)
        (deriv (deriv (cellGain (x i) μ)) w) w := by
      apply ((cellGain_second (k := x i) hμ hw).congr_of_eventuallyEq
        (f₁ := cellScore (x i) μ) ?_).congr_deriv
      · exact (cellGain_second (k := x i) hμ hw).deriv.symm
      · filter_upwards [Ioo_mem_nhds hw.1 hw.2] with t ht
        exact (cellGain_deriv_closed_left (k := x i) hμ ⟨ht.1.le, ht.2⟩).deriv.symm
    unfold sampleScore
    rw [(HasDerivAt.fun_sum (fun i (_ : i ∈ Finset.univ) ↦ hdc i)).deriv]
    obtain ⟨i, hi⟩ := hx
    exact Finset.sum_neg' (fun j _ ↦ (cellGain_curvature (k := x j) hμ hw).1)
      ⟨i, Finset.mem_univ _, (cellGain_curvature (k := x i) hμ hw).2 hi⟩

private noncomputable def closedCellGain (k : ℕ) (μ w : ℝ) : ℝ :=
  exp (μ*w) * ∑ j ∈ Finset.range (max k 1), besselCoeff (max k 1) j *
    ((1-w)/(1+w))^(k-j) * (2*w/(μ*(1+w)^2))^j

private theorem closedCellGain_continuousOn (k : ℕ) {μ : ℝ} (hμ : 0 < μ) :
    ContinuousOn (closedCellGain k μ) (Icc 0 1) := by
  have hden (w : ℝ) (hw : w ∈ Icc (0 : ℝ) 1) : 0 < 1+w := by linarith [hw.1]
  have hc : ContinuousOn (fun w : ℝ ↦ 1+w) (Icc 0 1) := by fun_prop
  have hq : ContinuousOn (fun w : ℝ ↦ (1-w)/(1+w)) (Icc 0 1) :=
    (by fun_prop : ContinuousOn (fun w : ℝ ↦ 1-w) _).div hc fun w hw ↦ (hden w hw).ne'
  have hu : ContinuousOn (fun w : ℝ ↦ 2*w/(μ*(1+w)^2)) (Icc 0 1) :=
    (by fun_prop : ContinuousOn (fun w : ℝ ↦ 2*w) _).div
      ((hc.pow 2).const_mul μ) fun w hw ↦ (mul_pos hμ (sq_pos_of_pos (hden w hw))).ne'
  unfold closedCellGain
  apply (by fun_prop : ContinuousOn (fun w : ℝ ↦ exp (μ*w)) _).mul
  exact continuousOn_finsetSum _ fun j _ ↦ ((hq.pow _).const_mul _).mul (hu.pow _)

private theorem closedCellGain_one {k : ℕ} (hk : 0 < k) (μ : ℝ) :
    closedCellGain k μ 1 = 0 := by
  unfold closedCellGain
  rw [max_eq_left (show 1 ≤ k by omega)]
  suffices h : (∑ j ∈ Finset.range k, besselCoeff k j * ((1-(1:ℝ))/(1+1))^(k-j) *
      (2*1/(μ*(1+1)^2))^j) = 0 by rw [h, mul_zero]
  apply Finset.sum_eq_zero
  intro j hj
  have h : k-j ≠ 0 := by have := Finset.mem_range.mp hj; omega
  simp [zero_pow h]

private theorem closedCellGain_eq_exp {k : ℕ} {μ w : ℝ} (hμ : 0 < μ)
    (hw : w ∈ Ico (0 : ℝ) 1) :
    closedCellGain k μ w = exp (cellGain k μ w) := by
  let q := (1-w)/(1+w)
  let y := inverseArgument μ w
  have hp : 0 < 1-w := sub_pos.mpr hw.2
  have hd : 0 < 1+w := by linarith [hw.1]
  have hden := one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩
  have hQ : 0 < (besselPolynomial (max k 1)).eval y :=
    bessel_eval_pos (k := max k 1) (by omega)
    (div_nonneg (by linarith [hw.1] : 0 ≤ 2*w) (mul_pos hμ hden).le)
  have hqy : 2*w/(μ*(1+w)^2) = q*y := by
    dsimp [q, y, inverseArgument]
    rw [show 1-w^2 = (1-w)*(1+w) by ring]
    field_simp [hμ.ne', hd.ne', hp.ne']
  unfold cellGain
  change _ = exp (binaryGain 1 (k : ℝ) μ w + log ((besselPolynomial (max k 1)).eval y))
  rw [exp_add, exp_log hQ, binaryGain, one_mul, exp_add]
  rw [← log_div hp.ne' hd.ne']
  change closedCellGain k μ w = exp (μ*w) * exp ((k : ℝ)*log q) *
    (besselPolynomial (max k 1)).eval y
  rw [exp_nat_mul, exp_log (div_pos hp hd), mul_assoc]
  unfold closedCellGain
  rw [hqy]
  congr 1
  unfold besselPolynomial
  simp only [Polynomial.eval_finsetSum, eval_mul, eval_C, eval_pow, eval_X, Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro j hj
  have hjk : j ≤ k := by have := Finset.mem_range.mp hj; omega
  change besselCoeff (max k 1) j * q^(k-j) * (q*y)^j =
    q^k*(besselCoeff (max k 1) j*y^j)
  rw [mul_pow, mul_assoc (besselCoeff _ _), ← mul_assoc (q^(k-j)),
    ← pow_add q (k-j) j, Nat.sub_add_cancel hjk]
  ring

private theorem sampleGain_zero {n : ℕ} (x : Fin n → ℕ) (μ : ℝ) :
    sampleGain x μ 0 = 0 := by
  apply Finset.sum_eq_zero
  intro i _
  have h : (besselPolynomial (max (x i) 1)).eval 0 = 1 := by
    rw [← coeff_zero_eq_eval_zero, coeff_besselPolynomial (by omega)]
    rfl
  simp [cellGain, binaryGain, inverseArgument, h]

/-- The score at the Poisson endpoint, in the compact shape coordinate. -/
noncomputable def inwardScore {n : ℕ} (x : Fin n → ℕ) (μ : ℝ) : ℝ :=
  ∑ i, (μ-2*(x i : ℝ)+(x i : ℝ)*((x i : ℝ)-1)/μ)

private theorem sampleScore_zero {n : ℕ} (x : Fin n → ℕ) {μ : ℝ} (hμ : 0 < μ) :
    sampleScore x μ 0 = inwardScore x μ :=
  Finset.sum_congr rfl (fun i _ ↦ cellScore_zero (x i) hμ)

/-- The held-mean profile has a unique interior optimum for a positive inward
score, and otherwise has the strict, unattained Poisson supremum. -/
theorem held_mean_profile {n : ℕ} {x : Fin n → ℕ} {μ : ℝ} (hμ : 0 < μ)
    (hx : ∃ i, 0 < x i) :
    if 0 < inwardScore x μ then
      ∃! w, w ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn (sampleGain x μ) (Ioo 0 1) w
    else
      IsLUB ((sampleGain x μ) '' Ioo 0 1) 0 ∧
        ∀ w ∈ Ioo (0 : ℝ) 1, sampleGain x μ w < 0 := by
  let f := sampleGain x μ
  have hf0 : f 0 = 0 := sampleGain_zero x μ
  have h0 : (0 : ℝ) ∈ Ico 0 1 := ⟨le_rfl, zero_lt_one⟩
  have ha := sampleScore_strictAnti hμ hx
  have hscore := sampleScore_zero x hμ
  by_cases hs : 0 < inwardScore x μ
  · rw [ite_eq_left hs]
    let F := fun w ↦ ∏ i, closedCellGain (x i) μ w
    have hF (w : ℝ) (hw : w ∈ Ico (0 : ℝ) 1) : F w = exp (f w) := by
      unfold F f sampleGain
      rw [Real.exp_sum]
      exact Finset.prod_congr rfl fun i _ ↦ closedCellGain_eq_exp hμ hw
    have hF1 : F 1 = 0 := by
      obtain ⟨i, hi⟩ := hx
      exact Finset.prod_eq_zero (i := i) (Finset.mem_univ _) (closedCellGain_one hi μ)
    have hFc : ContinuousOn F (Icc 0 1) :=
      continuousOn_finsetProd _ fun i _ ↦ closedCellGain_continuousOn (x i) hμ
    obtain ⟨w, hw, hwmax⟩ := isCompact_Icc.exists_isMaxOn
      (show (Icc (0 : ℝ) 1).Nonempty from ⟨0, le_rfl, zero_le_one⟩) hFc
    have hsl := (sampleGain_deriv x hμ h0).tendsto_slope.mono_left (nhdsGT_le_nhdsNE 0)
    have hnear := hsl.eventually (Ioi_mem_nhds (hscore.symm ▸ hs))
    obtain ⟨a, ha', haSlope⟩ := (show ∀ᶠ a in 𝓝[>] (0 : ℝ), a ∈ Ioo 0 1 ∧ 0 < slope f 0 a from
      Filter.Eventually.and (Ioo_mem_nhdsGT zero_lt_one) hnear).exists
    have haz : 0 < f a := by
      rw [slope_def_field, hf0, sub_zero, sub_zero] at haSlope
      simpa using (lt_div_iff₀ ha'.1).mp haSlope
    have hFa : 1 < F a := by rw [hF a ⟨ha'.1.le, ha'.2⟩, ← exp_zero]; exact exp_lt_exp.mpr haz
    have hFw : 1 < F w := hFa.trans_le (hwmax ⟨ha'.1.le, ha'.2.le⟩)
    have hw' : w ∈ Ioo (0 : ℝ) 1 := by
      refine ⟨lt_of_le_of_ne hw.1 ?_, lt_of_le_of_ne hw.2 ?_⟩
      · intro h
        rw [← h, hF 0 h0, hf0, exp_zero] at hFw
        exact (lt_irrefl _) hFw
      · intro h
        rw [h, hF1] at hFw
        linarith
    have hopt : IsMaxOn f (Ioo (0 : ℝ) 1) w := by
      intro t ht
      have hh := hwmax (show t ∈ Icc (0 : ℝ) 1 from ⟨ht.1.le, ht.2.le⟩)
      change F t ≤ F w at hh
      rw [hF t ⟨ht.1.le, ht.2⟩, hF w ⟨hw'.1.le, hw'.2⟩] at hh
      exact exp_le_exp.mp hh
    have hroot (t : ℝ) (ht : t ∈ Ioo (0 : ℝ) 1) (hm : IsMaxOn f (Ioo 0 1) t) :
        sampleScore x μ t = 0 := by
      rw [← (sampleGain_deriv x hμ ⟨ht.1.le, ht.2⟩).deriv]
      exact (hm.isLocalMax (Ioo_mem_nhds ht.1 ht.2)).deriv_eq_zero
    refine ⟨w, ⟨hw', hopt⟩, fun t ht ↦ ?_⟩
    exact ha.injOn ⟨ht.1.1.le, ht.1.2⟩ ⟨hw'.1.le, hw'.2⟩
      ((hroot t ht.1 ht.2).trans (hroot w hw' hopt).symm)
  · rw [ite_eq_right hs]
    have hfAnti : StrictAntiOn f (Ico 0 1) := by
      apply strictAntiOn_of_deriv_neg (convex_Ico _ _)
        (fun t ht ↦ (sampleGain_deriv x hμ ht).continuousAt.continuousWithinAt)
      intro t ht
      rw [interior_Ico] at ht
      rw [(sampleGain_deriv x hμ ⟨ht.1.le, ht.2⟩).deriv]
      exact (ha h0 ⟨ht.1.le, ht.2⟩ ht.1).trans_le
        (hscore.trans_le (le_of_not_gt hs))
    have hlt (w : ℝ) (hw : w ∈ Ioo (0 : ℝ) 1) : f w < 0 :=
      (hfAnti h0 ⟨hw.1.le, hw.2⟩ hw.1).trans_eq hf0
    have hlim' : Tendsto f (𝓝[>] (0 : ℝ)) (𝓝 (f 0)) :=
      (sampleGain_deriv x hμ h0).continuousAt.continuousWithinAt
    have hlim : Tendsto f (𝓝[>] (0 : ℝ)) (𝓝 0) := by simpa only [hf0] using hlim'
    refine ⟨⟨?_, ?_⟩, hlt⟩
    · rintro _ ⟨a, ha', rfl⟩
      exact (hlt a ha').le
    · intro b hb
      apply le_of_tendsto hlim
      filter_upwards [Ioo_mem_nhdsGT (show (0 : ℝ) < 1 by norm_num)] with a ha'
      exact hb ⟨a, ha', rfl⟩

/-- Positive parameter domain in the Bessel argument and ratio chart
`(z, t)`, with `t = λ/μ` and `z = sqrt (2*λ + t²)`. -/
def positiveChart : Set (ℝ × ℝ) := {p | p.2 ∈ Ioo 0 p.1}

private noncomputable def sampleSum {n : ℕ} (x : Fin n → ℕ) : ℝ :=
  ∑ i, (x i : ℝ)

/-- The empirical mean and population variance (divisor `n`). -/
@[no_expose] noncomputable def sampleMean {n : ℕ} (x : Fin n → ℕ) : ℝ := sampleSum x / n

/-- Population variance of the observed counts. -/
noncomputable def sampleVariance {n : ℕ} (x : Fin n → ℕ) : ℝ :=
  (∑ i, ((x i : ℝ)-sampleMean x)^2) / n

/-- Joint PIG log likelihood in the positive Bessel chart, up to the
data-only factorial term. -/
@[no_expose] noncomputable def chartLogLik {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ) : ℝ :=
  fixedArgument n (sampleSum x) p.1 p.2 -
    (n : ℝ)*p.1 - sampleSum x*log p.1 +
      ∑ i, log ((besselPolynomial (max (x i) 1)).eval (1/p.1))

/-- Poisson log likelihood at rate `μ`, up to the same factorial term. -/
@[no_expose] noncomputable def poissonLogLik {n : ℕ} (x : Fin n → ℕ) (μ : ℝ) : ℝ :=
  -(n : ℝ)*μ + sampleSum x*log μ

private noncomputable def profilePoint (μ w : ℝ) : ℝ × ℝ :=
  (μ*(1-w^2)/(2*w), μ*(1-w)^2/(2*w))

private theorem profilePoint_mem {μ w : ℝ} (hμ : 0 < μ) (hw : w ∈ Ioo (0 : ℝ) 1) :
    profilePoint μ w ∈ positiveChart := by
  have hp := sub_pos.mpr hw.2
  refine ⟨div_pos (mul_pos hμ (sq_pos_of_pos hp)) (by linarith [hw.1]), ?_⟩
  change μ*(1-w)^2/(2*w) < μ*(1-w^2)/(2*w)
  apply div_lt_div_of_pos_right _ (by linarith [hw.1])
  apply mul_lt_mul_of_pos_left _ hμ
  nlinarith [mul_pos hw.1 hp]

private theorem profilePoint_arg {μ z : ℝ} (hμ : 0 < μ) (hz : 0 < z) :
    ∃ w ∈ Ioo (0 : ℝ) 1, (profilePoint μ w).1 = z ∧
      z^2-(profilePoint μ w).2^2 = 2*μ*(profilePoint μ w).2 := by
  let t := sqrt (z^2+μ^2)-μ
  have hs : (sqrt (z^2+μ^2))^2 = z^2+μ^2 := sq_sqrt (by positivity)
  have hs0 := sqrt_nonneg (z^2+μ^2)
  have ht : 0 < t := by
    dsimp [t]
    by_contra! hn
    have hsq := mul_nonneg (sub_nonneg.mpr hn) (add_nonneg hμ.le hs0)
    nlinarith [sq_pos_of_pos hz]
  have hroot : z^2-t^2 = 2*μ*t := by dsimp [t]; nlinarith only [hs]
  have htz : t < z := by
    by_contra! h
    have hh := mul_nonneg (sub_nonneg.mpr h) (add_nonneg ht.le hz.le)
    nlinarith only [hroot, hh, mul_pos hμ ht]
  let w := (z-t)/(z+t)
  have hzt := add_pos hz ht
  have hw : w ∈ Ioo (0 : ℝ) 1 := ⟨div_pos (sub_pos.mpr htz) hzt,
    (div_lt_one hzt).mpr (by linarith)⟩
  have hpair : profilePoint μ w = (z,t) := by
    have hdiff : z-t ≠ 0 := (sub_pos.mpr htz).ne'
    have hroot' : μ = (z^2-t^2)/(2*t) := (eq_div_iff (by positivity)).mpr (by nlinarith only [hroot])
    apply Prod.ext
    · dsimp [profilePoint, w]
      rw [hroot']
      field_simp [hzt.ne', hdiff, ht.ne']
      ring
    · dsimp [profilePoint, w]
      rw [hroot']
      field_simp [hzt.ne', hdiff, ht.ne']
      ring
  exact ⟨w, hw, by rw [hpair], by rw [hpair]; exact hroot⟩

private theorem profilePoint_logLik {n : ℕ} (x : Fin n → ℕ) {μ w : ℝ}
    (hμ : 0 < μ) (hw : w ∈ Ioo (0 : ℝ) 1) :
    chartLogLik x (profilePoint μ w) = poissonLogLik x μ + sampleGain x μ w := by
  let p := profilePoint μ w
  have hp := profilePoint_mem hμ hw
  have hz : 0 < p.1 := hp.1.trans hp.2
  have hShape : 0 < (p.1^2-p.2^2)/2 := by
    have h := mul_pos (sub_pos.mpr hp.2) (add_pos hz hp.1)
    nlinarith
  have h₁ : 1/p.1 = inverseArgument μ w := by
    dsimp [p, profilePoint, inverseArgument]
    field_simp [hw.1.ne', hμ.ne', (one_sub_sq_pos (w := w) ⟨by linarith [hw.1], hw.2⟩).ne']
  have h₂ : p.2-p.1 = μ*(w-1) := by
    dsimp [p, profilePoint]
    field_simp [hw.1.ne']
    ring
  have hratio : ((p.1^2-p.2^2)/2)/p.1 = μ*((1-w)/(1+w)) := by
    dsimp [p, profilePoint]
    have hp₁ := sub_pos.mpr hw.2
    have hp₂ : 0 < 1+w := by linarith [hw.1]
    rw [show 1-w^2 = (1-w)*(1+w) by ring]
    field_simp [hw.1.ne', hμ.ne', hp₁.ne', hp₂.ne']
    ring
  have hlog : log ((p.1^2-p.2^2)/2) - log p.1 =
      log μ + (log (1-w)-log (1+w)) := by
    rw [← log_div hShape.ne' hz.ne', hratio, log_mul hμ.ne' (div_pos
      (sub_pos.mpr hw.2) (by linarith [hw.1])).ne']
    rw [log_div (sub_pos.mpr hw.2).ne' (show 1+w ≠ 0 by linarith [hw.1])]
  have hs : sampleGain x μ w = (n : ℝ)*μ*w +
      sampleSum x*(log (1-w)-log (1+w)) +
      ∑ i, log ((besselPolynomial (max (x i) 1)).eval (inverseArgument μ w)) := by
    simp only [sampleGain, cellGain, binaryGain, one_mul, Finset.sum_add_distrib,
      ← Finset.sum_mul, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
      sampleSum]
  change chartLogLik x p = _
  rw [chartLogLik, fixedArgument, h₁, poissonLogLik, hs]
  linear_combination (n : ℝ)*h₂ + sampleSum x*hlog

private theorem sampleSum_pos {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    0 < sampleSum x := by
  obtain ⟨i, hi⟩ := hx
  exact Finset.sum_pos' (fun j _ ↦ Nat.cast_nonneg (x j))
    ⟨i, Finset.mem_univ _, by exact_mod_cast hi⟩

private theorem inwardScore_at_mean {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    0 < inwardScore x (sampleMean x) ↔ sampleMean x < sampleVariance x := by
  let m := sampleMean x
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact Fin.pos i
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hm : 0 < m := div_pos (sampleSum_pos hx) hn'
  have hs : sampleSum x = m*n := (div_eq_iff hn'.ne').mp rfl
  have hf : (∑ i, (x i : ℝ)*((x i : ℝ)-1)) =
      (∑ i, (x i : ℝ)^2) - sampleSum x := by
    simp only [mul_sub, mul_one, ← pow_two, Finset.sum_sub_distrib, sampleSum]
  have hv : (∑ i, ((x i : ℝ)-m)^2) =
      (∑ i, (x i : ℝ)^2)-2*m*sampleSum x+(n : ℝ)*m^2 := by
    simp_rw [sub_sq]
    simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, ← Finset.sum_mul,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, sampleSum]
    rw [← Finset.mul_sum]
    ring
  have hscore : inwardScore x m = (n : ℝ)*m-2*sampleSum x+
      ((∑ i, (x i : ℝ)*((x i : ℝ)-1))/m) := by
    simp [inwardScore, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.mul_sum,
      Finset.sum_div, sampleSum]
  have h : m*inwardScore x m = (n : ℝ)*(sampleVariance x-m) := by
    rw [hscore, hf]
    change m*((n : ℝ)*m-2*sampleSum x+((∑ i, (x i : ℝ)^2)-sampleSum x)/m) =
      (n : ℝ)*((∑ i, ((x i : ℝ)-m)^2)/n-m)
    rw [hv, hs]
    field_simp [hm.ne', hn'.ne']
    ring
  constructor
  · intro hi
    have hpos := mul_pos hm hi
    rw [h] at hpos
    exact sub_pos.mp (pos_of_mul_pos_right hpos hn'.le)
  · intro hi
    have hpos := mul_pos hn' (sub_pos.mpr hi)
    rw [← h] at hpos
    exact pos_of_mul_pos_right hpos hm.le

private theorem chart_profile_dominates {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {p : ℝ × ℝ} (hp : p ∈ positiveChart) :
    ∃ w ∈ Ioo (0 : ℝ) 1, chartLogLik x p ≤ chartLogLik x (profilePoint (sampleMean x) w) ∧
      (chartLogLik x p = chartLogLik x (profilePoint (sampleMean x) w) →
        p = profilePoint (sampleMean x) w) := by
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact Fin.pos i
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let m := sampleMean x
  have hS := sampleSum_pos hx
  have hm : 0 < m := div_pos hS hn'
  have hs : (n : ℝ)*m = sampleSum x := by dsimp [m, sampleMean]; field_simp
  obtain ⟨w, hw, hz, hroot⟩ := profilePoint_arg hm (hp.1.trans hp.2)
  let q := profilePoint m w
  have hq := profilePoint_mem hm hw
  have hz' : q.1 = p.1 := hz
  have hqroot : (n : ℝ)*(p.1^2-q.2^2) = 2*sampleSum x*q.2 := by
    rw [hroot, ← hs]
    ring
  refine ⟨w, hw, ?_⟩
  have cases_eq : p.2 = q.2 → p = q := fun h ↦ Prod.ext hz'.symm h
  by_cases h : p.2 = q.2
  · have he := cases_eq h
    exact ⟨by rw [he], fun _ ↦ he⟩
  · have hstrict := fixedArgument_strict hS (hz' ▸ hq) hp hqroot h
    have hlt : chartLogLik x p < chartLogLik x q := by
      unfold chartLogLik
      rw [hz']
      linarith only [hstrict]
    exact ⟨hlt.le, fun he ↦ (hlt.ne he).elim⟩

/-- The all-free PIG has a unique finite optimum exactly above the empirical
variance threshold. Otherwise the Poisson value at the empirical mean is
a strict, unattained supremum. The sample contains a positive count. -/
theorem all_free_positive {n : ℕ} {x : Fin n → ℕ} (hx : ∃ i, 0 < x i) :
    ((∃ p ∈ positiveChart, IsMaxOn (chartLogLik x) positiveChart p) ↔
      sampleMean x < sampleVariance x) ∧
    (sampleMean x < sampleVariance x →
      ∃! p, p ∈ positiveChart ∧ IsMaxOn (chartLogLik x) positiveChart p) ∧
    (sampleVariance x ≤ sampleMean x →
      IsLUB (chartLogLik x '' positiveChart) (poissonLogLik x (sampleMean x)) ∧
        ∀ p ∈ positiveChart, chartLogLik x p < poissonLogLik x (sampleMean x)) := by
  have hn : 0 < n := by obtain ⟨i, _⟩ := hx; exact Fin.pos i
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let m := sampleMean x
  have hm : 0 < m := div_pos (sampleSum_pos hx) hn'
  have hsplit := held_mean_profile hm hx
  have hiff := inwardScore_at_mean hx
  have hfinite (h : sampleMean x < sampleVariance x) :
      ∃! p, p ∈ positiveChart ∧ IsMaxOn (chartLogLik x) positiveChart p := by
    rw [ite_eq_left (hiff.mpr h)] at hsplit
    obtain ⟨w, ⟨hw, hmax⟩, huniq⟩ := hsplit
    let q := profilePoint m w
    have hq : q ∈ positiveChart := profilePoint_mem hm hw
    have hqmax : IsMaxOn (chartLogLik x) positiveChart q := by
      intro p hp
      obtain ⟨v, hv, hdom, _⟩ := chart_profile_dominates hx hp
      refine hdom.trans ?_
      change chartLogLik x (profilePoint m v) ≤ chartLogLik x (profilePoint m w)
      rw [profilePoint_logLik x hm hv, profilePoint_logLik x hm hw]
      exact (add_le_add_iff_left _).mpr (hmax hv)
    refine ⟨q, ⟨hq, hqmax⟩, fun p hp ↦ ?_⟩
    obtain ⟨v, hv, hdom, heq⟩ := chart_profile_dominates hx hp.1
    have v_max : IsMaxOn (sampleGain x m) (Ioo 0 1) v := by
      intro u hu
      have hh : chartLogLik x (profilePoint m u) ≤ chartLogLik x (profilePoint m v) :=
        (hp.2 (profilePoint_mem hm hu)).trans hdom
      rw [profilePoint_logLik x hm hu, profilePoint_logLik x hm hv] at hh
      exact (add_le_add_iff_left (poissonLogLik x m)).mp hh
    have hvw := huniq v ⟨hv, v_max⟩
    exact (heq (le_antisymm hdom (hp.2 (profilePoint_mem hm hv)))).trans (by rw [hvw])
  have hface (h : sampleVariance x ≤ sampleMean x) :
      IsLUB (chartLogLik x '' positiveChart) (poissonLogLik x m) ∧
        ∀ p ∈ positiveChart, chartLogLik x p < poissonLogLik x m := by
    rw [ite_eq_right (fun hs ↦ (not_lt_of_ge h) (hiff.mp hs))] at hsplit
    obtain ⟨hsup, hstrict⟩ := hsplit
    have hlt (p : ℝ × ℝ) (hp : p ∈ positiveChart) :
        chartLogLik x p < poissonLogLik x m := by
      obtain ⟨w, hw, hle, _⟩ := chart_profile_dominates hx hp
      refine hle.trans_lt ?_
      rw [profilePoint_logLik x hm hw]
      linarith only [hstrict w hw]
    refine ⟨⟨?_, ?_⟩, hlt⟩
    · rintro _ ⟨p, hp, rfl⟩
      exact (hlt p hp).le
    · intro b hb
      have hle : 0 ≤ b - poissonLogLik x m := by
        apply hsup.2
        rintro _ ⟨w, hw, rfl⟩
        have h := hb ⟨profilePoint m w, profilePoint_mem hm hw, rfl⟩
        change chartLogLik x (profilePoint m w) ≤ b at h
        rw [profilePoint_logLik x hm hw] at h
        linarith only [h]
      linarith only [hle]
  refine ⟨⟨?_, fun h ↦ (hfinite h).exists.imp fun _ hp ↦ hp⟩, hfinite, hface⟩
  rintro ⟨p, hp, hopt⟩
  by_contra! h
  obtain ⟨hsup, hstrict⟩ := hface h
  have hu : chartLogLik x p ∈ upperBounds (chartLogLik x '' positiveChart) := by
    rintro _ ⟨q, hq, rfl⟩
    exact hopt hq
  exact (not_le_of_gt (hstrict p hp)) (hsup.2 hu)

/-- Map mean and shape into the Bessel chart. -/
noncomputable def parameterChart (p : ℝ × ℝ) : ℝ × ℝ :=
  (sqrt (2*p.2+(p.2/p.1)^2), p.2/p.1)

private noncomputable def chartParameters (p : ℝ × ℝ) : ℝ × ℝ :=
  ((p.1^2-p.2^2)/(2*p.2), (p.1^2-p.2^2)/2)

private theorem chart_square_gap {p : ℝ × ℝ} (hp : p ∈ positiveChart) :
    0 < p.1^2-p.2^2 := by
  have hz := hp.1.trans hp.2
  nlinarith [mul_pos (sub_pos.mpr hp.2) (add_pos hz hp.1)]

private theorem parameterChart_mem {p : ℝ × ℝ} (hp : p ∈ Ioi 0 ×ˢ Ioi 0) :
    parameterChart p ∈ positiveChart := by
  have ht : 0 < p.2/p.1 := div_pos hp.2 hp.1
  have hs := sq_sqrt (show 0 ≤ 2*p.2+(p.2/p.1)^2 by
    have h : 0 < p.2 := hp.2
    positivity)
  refine ⟨ht, ?_⟩
  by_contra! h
  have hz := sqrt_nonneg (2*p.2+(p.2/p.1)^2)
  have hmul := mul_nonneg (sub_nonneg.mpr h) (add_nonneg ht.le hz)
  change sqrt (2*p.2+(p.2/p.1)^2) ≤ p.2/p.1 at h
  change 0 ≤ (p.2/p.1 - sqrt (2*p.2+(p.2/p.1)^2)) *
    (p.2/p.1 + sqrt (2*p.2+(p.2/p.1)^2)) at hmul
  have hshape : 0 < p.2 := hp.2
  nlinarith only [hs, hmul, hshape]

private theorem chartParameters_mem {p : ℝ × ℝ} (hp : p ∈ positiveChart) :
    chartParameters p ∈ Ioi 0 ×ˢ Ioi 0 := by
  have ht : 0 < p.2 := hp.1
  have hg := chart_square_gap hp
  exact ⟨div_pos hg (mul_pos (by norm_num : (0 : ℝ) < 2) ht),
    div_pos hg (by norm_num : (0 : ℝ) < 2)⟩

private theorem chartParameters_left_inv {p : ℝ × ℝ} (hp : p ∈ Ioi 0 ×ˢ Ioi 0) :
    chartParameters (parameterChart p) = p := by
  have hμ : p.1 ≠ 0 := hp.1.ne'
  have hshape : p.2 ≠ 0 := hp.2.ne'
  have hs := sq_sqrt (show 0 ≤ 2*p.2+(p.2/p.1)^2 by
    have h : 0 < p.2 := hp.2
    positivity)
  apply Prod.ext
  · dsimp [chartParameters, parameterChart]
    rw [hs]
    field_simp [hμ, hshape]
    ring
  · dsimp [chartParameters, parameterChart]
    rw [hs]
    ring

private theorem chartParameters_right_inv {p : ℝ × ℝ} (hp : p ∈ positiveChart) :
    parameterChart (chartParameters p) = p := by
  have hz := hp.1.trans hp.2
  have ht := hp.1
  have hg := chart_square_gap hp
  have hratio : (p.1^2-p.2^2)/2 / ((p.1^2-p.2^2)/(2*p.2)) = p.2 := by
    field_simp [hg.ne', ht.ne']
  apply Prod.ext
  · change sqrt (2*((p.1^2-p.2^2)/2)+
       ((p.1^2-p.2^2)/2 / ((p.1^2-p.2^2)/(2*p.2)))^2) = p.1
    rw [hratio, show 2*((p.1^2-p.2^2)/2)+p.2^2 = p.1^2 by ring, sqrt_sq hz.le]
  · exact hratio

/-- The positive Bessel chart is bijective with positive mean and shape. -/
theorem parameterChart_bijOn :
    BijOn parameterChart (Ioi 0 ×ˢ Ioi 0) positiveChart := by
  refine ⟨fun _ hp ↦ parameterChart_mem hp, ?_, ?_⟩
  · intro p hp q hq he
    simpa only [chartParameters_left_inv hp, chartParameters_left_inv hq] using
      congrArg chartParameters he
  · exact fun p hp ↦ ⟨chartParameters p, chartParameters_mem hp, chartParameters_right_inv hp⟩

end UnivariateML.Discrete3PIG

end
