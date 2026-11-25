module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.Real.Sqrt
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.Basic
public import Mathlib.Topology.Order.IntermediateValue
public import UnivariateML.Generic.Profile

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Birnbaum–Saunders likelihood with fixed threshold

The sample consists of the positive shifted observations. The log-likelihood
is per observation and retains the dossier's normalization. The joint
parameter pair is ordered as (scale, shape), with scale the profile variable.

The centered second moment of the reciprocal shifted observations gives
the strict inequality in the Cauchy–Schwarz step. The rescaled score is
strictly decreasing above the harmonic mean, and its root determines the
unique maximum of the scale profile.

## Deviations

The results concern exact real arithmetic on the positive interior of the
support. The scale bracket is for the case with shape free. The dossier's
near-constant three-observation formulas describe leading-order behavior.
The displayed scale approximation lies strictly below the MLE for a positive log gap.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Fatigue

noncomputable section

variable {n : ℕ}

def avg (t : Fin n → ℝ) : ℝ := (∑ i, t i) / n

def A (t : Fin n → ℝ) : ℝ := avg t

def reciprocalMean (t : Fin n → ℝ) (b : ℝ) : ℝ :=
  avg (fun i ↦ (t i + b)⁻¹)

def H (t : Fin n → ℝ) : ℝ := (reciprocalMean t 0)⁻¹

def xi (t : Fin n → ℝ) (b : ℝ) : ℝ := (reciprocalMean t b)⁻¹

def shapeSq (t : Fin n → ℝ) (b : ℝ) : ℝ := A t / b + b / H t - 2

def shape (t : Fin n → ℝ) (b : ℝ) : ℝ := sqrt (shapeSq t b)

def scaleTerm (t : Fin n → ℝ) (b : ℝ) : ℝ :=
  avg (fun i ↦ log (t i + b)) - log b / 2

def dataTerm (t : Fin n → ℝ) : ℝ :=
  -(3 * avg (fun i ↦ log (t i)) / 2) - log 2 - log (2 * π) / 2

def logDensity (a b t : ℝ) : ℝ :=
  log (t + b) - 3 * log t / 2 - log b / 2 - log (2 * a) - log (2 * π) / 2 -
    (t / b + b / t - 2) / (2 * a ^ 2)

def logLik (t : Fin n → ℝ) (a b : ℝ) : ℝ :=
  scaleTerm t b + dataTerm t - log a - shapeSq t b / (2 * a ^ 2)

def profile (t : Fin n → ℝ) (b : ℝ) : ℝ := logLik t (shape t b) b

def F (t : Fin n → ℝ) (b : ℝ) : ℝ :=
  (b - H t) ^ 2 + H t * (A t - H t) - (b - H t) * xi t b

def G (t : Fin n → ℝ) (b : ℝ) : ℝ :=
  H t * (A t - H t) / (b - H t) + (b - H t) - xi t b

def momentRatio (t : Fin n → ℝ) (b : ℝ) : ℝ :=
  avg (fun i ↦ ((t i + b)⁻¹) ^ 2) / reciprocalMean t b ^ 2

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem avg_const (hn : 0 < n) (c : ℝ) : avg (fun _ : Fin n ↦ c) = c := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [avg, nsmul_eq_mul, hN]

private theorem avg_add (u v : Fin n → ℝ) :
    avg (fun i ↦ u i + v i) = avg u + avg v := by
  simp [avg, Finset.sum_add_distrib, add_div]

private theorem avg_sub (u v : Fin n → ℝ) :
    avg (fun i ↦ u i - v i) = avg u - avg v := by
  simp [avg, Finset.sum_sub_distrib, sub_div]

private theorem avg_mul (c : ℝ) (u : Fin n → ℝ) :
    avg (fun i ↦ c * u i) = c * avg u := by
  unfold avg
  rw [← Finset.mul_sum, mul_div_assoc]

private theorem avg_div (u : Fin n → ℝ) (c : ℝ) :
    avg (fun i ↦ u i / c) = avg u / c := by
  unfold avg
  rw [← Finset.sum_div]
  ring

private theorem avg_shape_residual (hn : 0 < n) (t : Fin n → ℝ) (b : ℝ) :
    avg (fun i ↦ t i / b + b / t i - 2) = shapeSq t b := by
  simp only [avg_sub, avg_add, avg_const hn, avg_div, div_eq_mul_inv b, avg_mul]
  simp [shapeSq, A, H, reciprocalMean, div_eq_mul_inv]

/-- The decomposed objective is the mean of the dossier's normalized log density. -/
theorem logLik_eq_avg_logDensity (hn : 0 < n) (t : Fin n → ℝ) {a : ℝ}
    (ha : 0 < a) (b : ℝ) :
    logLik t a b = avg (fun i ↦ logDensity a b (t i)) := by
  unfold logLik scaleTerm dataTerm logDensity
  rw [avg_sub, avg_div, avg_shape_residual hn]
  simp only [avg_sub, avg_div, avg_mul, avg_const hn,
    log_mul (by norm_num : (2 : ℝ) ≠ 0) ha.ne']
  ring

private theorem avg_pos (hn : 0 < n) {u : Fin n → ℝ} (hu : ∀ i, 0 ≤ u i)
    (hpos : ∃ i, 0 < u i) : 0 < avg u := by
  obtain ⟨i, hi⟩ := hpos
  exact div_pos (Finset.sum_pos' (fun j _ ↦ hu j) ⟨i, Finset.mem_univ i, hi⟩)
    (by exact_mod_cast hn)

private theorem reciprocalMean_pos (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) : 0 < reciprocalMean t b :=
  avg_pos hn (fun i ↦ (inv_pos.mpr (ht i)).le) ⟨⟨0, hn⟩, inv_pos.mpr (ht _)⟩

private theorem H_pos (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i) :
    0 < H t :=
  inv_pos.mpr (reciprocalMean_pos hn (by simpa using ht))

/-- The profiled squared shape is the mean square of the standardized residuals. -/
theorem shapeSq_eq_mean_square (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shapeSq t b = avg (fun i ↦ (t i - b) ^ 2 / (t i * b)) := by
  have he (i : Fin n) : (t i - b) ^ 2 / (t i * b) =
      t i / b + b / t i - 2 := by
    field_simp [(ht i).ne', hb.ne']
    ring
  simp_rw [he]
  exact (avg_shape_residual hn t b).symm

/-- The conditional shape formula is the RMS of the dossier's square-root residuals. -/
theorem shape_eq_rms (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shape t b = sqrt (avg (fun i ↦ (sqrt (t i / b) - sqrt (b / t i)) ^ 2)) := by
  rw [shape, shapeSq_eq_mean_square hn ht hb]
  congr 1
  unfold avg
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  dsimp only
  rw [sqrt_div (ht i).le, sqrt_div hb.le]
  have hi : sqrt (t i) ≠ 0 := (sqrt_pos.2 (ht i)).ne'
  have hsb : sqrt b ≠ 0 := (sqrt_pos.2 hb).ne'
  have he : sqrt (t i) / sqrt b - sqrt b / sqrt (t i) =
      (t i - b) / (sqrt (t i) * sqrt b) := by
    field_simp [hi, hsb]
    nlinarith only [sq_sqrt (ht i).le, sq_sqrt hb.le]
  rw [he, div_pow, mul_pow, sq_sqrt (ht i).le, sq_sqrt hb.le]

/-- Positive squared shape is equivalent to at least one observation differing from the scale. -/
theorem shapeSq_pos_iff (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    0 < shapeSq t b ↔ ∃ i, t i ≠ b := by
  rw [shapeSq_eq_mean_square hn ht hb]
  constructor
  · contrapose!
    intro he
    simp [he, avg]
  · rintro ⟨i, hi⟩
    exact avg_pos hn (fun j ↦ div_nonneg (sq_nonneg _) (mul_pos (ht j) hb).le)
      ⟨i, div_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hi)) (mul_pos (ht i) hb)⟩

/-- Zero squared shape occurs exactly when every observation equals the scale. -/
theorem shapeSq_zero_iff (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    shapeSq t b = 0 ↔ ∀ i, t i = b := by
  constructor
  · intro h i
    by_contra hi
    have hpos := (shapeSq_pos_iff hn ht hb).mpr ⟨i, hi⟩
    exact hpos.ne' h
  · intro he
    rw [shapeSq_eq_mean_square hn ht hb]
    simp [avg, he]

/-- Nonconstant data have a positive profiled squared shape at every positive scale. -/
theorem shapeSq_pos (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) : 0 < shapeSq t b := by
  apply (shapeSq_pos_iff hn ht hb).mpr
  obtain ⟨i, j, hij⟩ := hne
  by_cases he : t i = b
  · exact ⟨j, fun hj ↦ hij (he.trans hj.symm)⟩
  · exact ⟨i, he⟩

private theorem H_lt_A (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) : H t < A t := by
  have hH := H_pos hn ht
  have h := shapeSq_pos hn ht hne hH
  rw [shapeSq, div_self hH.ne'] at h
  have h' : 1 < A t / H t := by linarith
  simpa using (lt_div_iff₀ hH).mp h'

/-- A positive squared shape gives the unique conditional maximizer at the held scale. -/
theorem heldScale_unique (t : Fin n → ℝ) {b : ℝ} (hq : 0 < shapeSq t b) :
    0 < shape t b ∧ (shape t b) ^ 2 = shapeSq t b ∧
      ∀ a > 0, a ≠ shape t b → logLik t a b < profile t b := by
  have hs : 0 < shape t b := sqrt_pos.2 hq
  have hs2 : (shape t b) ^ 2 = shapeSq t b := sq_sqrt hq.le
  refine ⟨hs, hs2, fun a ha hne ↦ ?_⟩
  let r := shape t b / a
  have hr : 0 < r := div_pos hs ha
  have hr2 : r ^ 2 ≠ 1 := by
    intro he
    have : r = 1 := by nlinarith
    exact hne (by simpa using ((div_eq_iff ha.ne').mp this).symm)
  have h := log_lt_sub_one_of_pos (sq_pos_of_ne_zero hr.ne') hr2
  rw [pow_two r, log_mul hr.ne' hr.ne', show log r = log (shape t b) - log a from
    log_div hs.ne' ha.ne'] at h
  have he : r * r = shapeSq t b / a ^ 2 := by
    rw [← pow_two, div_pow, hs2]
  rw [he] at h
  unfold profile logLik
  rw [hs2, show shapeSq t b / (2 * shapeSq t b) = 1 / 2 by field_simp [hq.ne']]
  rw [show shapeSq t b / (2 * a ^ 2) = (shapeSq t b / a ^ 2) / 2 by ring]
  linarith

private theorem hasDerivAt_reciprocalMean (t : Fin n → ℝ) {b : ℝ}
    (ht : ∀ i, 0 < t i + b) :
    HasDerivAt (reciprocalMean t) (-avg (fun i ↦ ((t i + b)⁻¹) ^ 2)) b := by
  have hd (i : Fin n) :
      HasDerivAt (fun s ↦ (t i + s)⁻¹) (-((t i + b)⁻¹) ^ 2) b := by
    change HasDerivAt ((fun s : ℝ ↦ t i + s)⁻¹) _ b
    simpa only [id_eq, neg_div, one_div, inv_pow] using
      ((hasDerivAt_id b).const_add (t i)).inv (ht i).ne'
  change HasDerivAt (fun s ↦ (∑ i, (t i + s)⁻¹) / n) _ b
  simpa only [avg, Finset.sum_neg_distrib, neg_div] using
    (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hd i)).div_const (n : ℝ)

private theorem momentRatio_gt_one (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) (hne : ∃ i j, t i ≠ t j) : 1 < momentRatio t b := by
  let u := fun i ↦ (t i + b)⁻¹
  let m := avg u
  have hm : 0 < m := reciprocalMean_pos hn ht
  obtain ⟨i, j, hij⟩ := hne
  have huv : u i ≠ u j := by
    intro he
    exact hij (add_right_cancel (inv_injective he))
  have hpos : ∃ i, 0 < (u i - m) ^ 2 := by
    by_cases he : u i = m
    · exact ⟨j, sq_pos_of_ne_zero (sub_ne_zero.mpr (fun hj ↦ huv (he.trans hj.symm)))⟩
    · exact ⟨i, sq_pos_of_ne_zero (sub_ne_zero.mpr he)⟩
  have h := avg_pos hn (fun i ↦ sq_nonneg (u i - m)) hpos
  have he (i : Fin n) : (u i - m) ^ 2 = u i ^ 2 - (2 * m) * u i + m ^ 2 := by ring
  simp only [he, avg_add, avg_sub, avg_mul, avg_const hn] at h
  change 1 < avg (fun i ↦ u i ^ 2) / m ^ 2
  apply (lt_div_iff₀ (sq_pos_of_pos hm)).mpr
  change 0 < avg (fun i ↦ u i ^ 2) - (2 * m) * m + m ^ 2 at h
  linarith

/-- The derivative of the shifted harmonic mean is the reciprocal moment ratio. -/
theorem hasDerivAt_xi (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) : HasDerivAt (xi t) (momentRatio t b) b := by
  change HasDerivAt ((reciprocalMean t)⁻¹) _ b
  simpa only [momentRatio, neg_neg] using
    (hasDerivAt_reciprocalMean t ht).inv (reciprocalMean_pos hn ht).ne'

/-- For nonconstant samples the shifted harmonic mean grows faster than the shift. -/
theorem xi_deriv_gt_one (hn : 0 < n) {t : Fin n → ℝ} {b : ℝ}
    (ht : ∀ i, 0 < t i + b) (hne : ∃ i j, t i ≠ t j) : 1 < deriv (xi t) b := by
  rw [(hasDerivAt_xi hn ht).deriv]
  exact momentRatio_gt_one hn ht hne

/-- The shifted harmonic mean lies strictly above its unit-slope tangent bound. -/
theorem xi_gt_add_H (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) : b + H t < xi t b := by
  have hd (s : ℝ) (hs : 0 ≤ s) :
      HasDerivAt (fun u ↦ xi t u - u) (momentRatio t s - 1) s := by
    change HasDerivAt (xi t - id) _ s
    exact (hasDerivAt_xi hn (fun i ↦ add_pos_of_pos_of_nonneg (ht i) hs)).sub
      (hasDerivAt_id s)
  have hm : StrictMonoOn (fun u ↦ xi t u - u) (Ici 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ici _) (fun s hs ↦
      (hd s hs).continuousAt.continuousWithinAt)
    rw [interior_Ici]
    intro s hs
    rw [(hd s (mem_Ioi.mp hs).le).deriv]
    exact sub_pos.mpr (momentRatio_gt_one hn
      (fun i ↦ add_pos (ht i) (mem_Ioi.mp hs)) hne)
  have he := hm (show (0 : ℝ) ∈ Ici 0 from le_rfl) (show b ∈ Ici 0 from hb.le) hb
  change xi t 0 - 0 < xi t b - b at he
  change H t - 0 < xi t b - b at he
  linarith

/-- The rescaled profile score decreases strictly above the harmonic mean. -/
theorem hasDerivAt_G (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : H t < b) :
    HasDerivAt (G t)
      (-(H t * (A t - H t)) / (b - H t) ^ 2 + 1 - momentRatio t b) b := by
  have bpos := (H_pos hn ht).trans hb
  have hd := (((hasDerivAt_id b).sub_const (H t)).inv (sub_ne_zero.mpr hb.ne')).const_mul
    (H t * (A t - H t))
  convert (hd.add ((hasDerivAt_id b).sub_const (H t))).sub
    (hasDerivAt_xi hn (fun i ↦ add_pos (ht i) bpos)) using 1
  · funext s
    simp [G, div_eq_mul_inv]
  · simp [id_eq, div_eq_mul_inv]

/-- The derivative of the rescaled score is strictly negative above the harmonic mean. -/
theorem G_deriv_neg (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : H t < b) : deriv (G t) b < 0 := by
  rw [(hasDerivAt_G hn ht hb).deriv]
  have h := momentRatio_gt_one hn (fun i ↦ add_pos (ht i) ((H_pos hn ht).trans hb)) hne
  have hd := div_pos (mul_pos (H_pos hn ht) (sub_pos.mpr (H_lt_A hn ht hne)))
    (sq_pos_of_ne_zero (sub_ne_zero.mpr hb.ne'))
  rw [neg_div]
  linarith

private theorem hasDerivAt_shapeSq (t : Fin n → ℝ) {b : ℝ} (hb : 0 < b) :
    HasDerivAt (shapeSq t) (1 / H t - A t / b ^ 2) b := by
  convert ((((hasDerivAt_id b).inv hb.ne').const_mul (A t)).add
    ((hasDerivAt_id b).div_const (H t))).sub_const 2 using 1
  · funext s
    simp [shapeSq, div_eq_mul_inv]
  · simp [id_eq, div_eq_mul_inv]
    ring

private theorem hasDerivAt_scaleTerm {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    {b : ℝ} (hb : 0 < b) :
    HasDerivAt (scaleTerm t) (reciprocalMean t b - b⁻¹ / 2) b := by
  have hd (i : Fin n) : HasDerivAt (fun s ↦ log (t i + s)) ((t i + b)⁻¹) b := by
    simpa using ((hasDerivAt_id b).const_add (t i)).log (add_pos (ht i) hb).ne'
  exact ((HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ hd i)).div_const (n : ℝ)).sub
    ((hasDerivAt_log hb.ne').div_const 2)

/-- The profile derivative is the source score numerator times a positive factor. -/
theorem hasDerivAt_profile (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) :
    HasDerivAt (profile t)
      (reciprocalMean t b / (H t * b * shapeSq t b) * F t b) b := by
  have hH := (H_pos hn ht).ne'
  have hq := shapeSq_pos hn ht hne hb
  have hm := (reciprocalMean_pos hn (fun i ↦ add_pos (ht i) hb)).ne'
  have hd := (((hasDerivAt_scaleTerm ht hb).add_const (dataTerm t)).sub
    (((hasDerivAt_shapeSq t hb).log hq.ne').div_const 2)).sub_const (1 / 2)
  have he : reciprocalMean t b - b⁻¹ / 2 -
      ((1 / H t - A t / b ^ 2) / shapeSq t b) / 2 =
        reciprocalMean t b / (H t * b * shapeSq t b) * F t b := by
    unfold F xi
    field_simp [hH, hb.ne', hq.ne', hm]
    unfold shapeSq
    field_simp [hH, hb.ne']
    ring
  rw [he] at hd
  apply hd.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hb] with s hs
  have hq' := shapeSq_pos hn ht hne hs
  have hss := sq_sqrt hq'.le
  have hlog : log (shape t s) = log (shapeSq t s) / 2 := by
    unfold shape
    have hsqrt := (sqrt_pos.2 hq').ne'
    have hl := log_mul hsqrt hsqrt
    rw [← pow_two, hss] at hl
    linarith only [hl]
  simp only [profile, logLik, Pi.sub_apply]
  change scaleTerm t s + dataTerm t - log (shape t s) -
      shapeSq t s / (2 * shape t s ^ 2) =
    scaleTerm t s + dataTerm t - log (shapeSq t s) / 2 - 1 / 2
  change shape t s ^ 2 = shapeSq t s at hss
  rw [hlog, hss, show shapeSq t s / (2 * shapeSq t s) = 1 / 2 by field_simp [hq'.ne']]

/-- The derivative multiplier of the scale score numerator is strictly positive. -/
theorem scale_score_factor_pos (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 < b) :
    0 < reciprocalMean t b / (H t * b * shapeSq t b) :=
  div_pos (reciprocalMean_pos hn (fun i ↦ add_pos (ht i) hb))
    (mul_pos (mul_pos (H_pos hn ht) hb) (shapeSq_pos hn ht hne hb))

private theorem F_eq_mul_G (t : Fin n → ℝ) {b : ℝ} (hb : H t < b) :
    F t b = (b - H t) * G t b := by
  unfold F G
  field_simp [sub_ne_zero.mpr hb.ne']
  ring

private theorem F_pos_of_le_H (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : 0 ≤ b) (hbH : b ≤ H t) : 0 < F t b := by
  have hxi : 0 < xi t b :=
    inv_pos.mpr (reciprocalMean_pos hn (fun i ↦ add_pos_of_pos_of_nonneg (ht i) hb))
  exact sub_pos.mpr ((mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr hbH) hxi.le).trans_lt
    (add_pos_of_nonneg_of_pos (sq_nonneg _)
      (mul_pos (H_pos hn ht) (sub_pos.mpr (H_lt_A hn ht hne)))))

private theorem F_upper_neg (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) :
    F t ((H t + A t) / 2) < 0 := by
  let u := (H t + A t) / 2
  have hu : H t < u := by dsimp [u]; linarith [H_lt_A hn ht hne]
  have hx := xi_gt_add_H hn ht hne ((H_pos hn ht).trans hu)
  have h := mul_lt_mul_of_pos_left hx (sub_pos.mpr hu)
  change F t u < 0
  unfold F
  dsimp [u] at h ⊢
  nlinarith only [h]

private theorem F_continuousOn (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i) :
    ContinuousOn (F t) (Ici 0) := by
  have hc : ContinuousOn (xi t) (Ici 0) := fun b hb ↦
    (hasDerivAt_xi hn (fun i ↦ add_pos_of_pos_of_nonneg (ht i) hb)).continuousAt.continuousWithinAt
  exact (((continuousOn_id.sub continuousOn_const).pow 2).add continuousOn_const).sub
    ((continuousOn_id.sub continuousOn_const).mul hc)

/-- There is a root in the strict bracket, with positive and negative scores on its two sides. -/
theorem scale_root_signs (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) :
    ∃ b ∈ Ioo (H t) ((H t + A t) / 2), F t b = 0 ∧
      (∀ s ∈ Ioo 0 b, 0 < F t s) ∧ (∀ s ∈ Ioi b, F t s < 0) := by
  let u := (H t + A t) / 2
  have hH := H_pos hn ht
  have hu : H t < u := by dsimp [u]; linarith [H_lt_A hn ht hne]
  have hlo := F_pos_of_le_H hn ht hne hH.le le_rfl
  have hhi := F_upper_neg hn ht hne
  obtain ⟨b, hb, hz⟩ := intermediate_value_Icc' hu.le
    ((F_continuousOn hn ht).mono (fun _ hs ↦ hH.le.trans hs.1))
    (show (0 : ℝ) ∈ Icc (F t u) (F t (H t)) from ⟨hhi.le, hlo.le⟩)
  have hbH : H t < b := lt_of_le_of_ne hb.1 (fun he ↦ hlo.ne' (he ▸ hz))
  have hbu : b < u := lt_of_le_of_ne hb.2 (fun he ↦ hhi.ne (show F t u = 0 from he ▸ hz))
  have ha : StrictAntiOn (G t) (Ioi (H t)) := by
    apply strictAntiOn_of_deriv_neg (convex_Ioi _) (fun s hs ↦
      (hasDerivAt_G hn ht hs).continuousAt.continuousWithinAt)
    rw [interior_Ioi]
    exact fun _ hs ↦ G_deriv_neg hn ht hne hs
  have hg : G t b = 0 := by
    rw [F_eq_mul_G t hbH] at hz
    exact (mul_eq_zero.mp hz).resolve_left (sub_ne_zero.mpr hbH.ne')
  refine ⟨b, ⟨hbH, hbu⟩, hz, ?_, ?_⟩
  · intro s hs
    by_cases hsH : s ≤ H t
    · exact F_pos_of_le_H hn ht hne hs.1.le hsH
    · rw [F_eq_mul_G t (lt_of_not_ge hsH)]
      exact mul_pos (sub_pos.mpr (lt_of_not_ge hsH)) (hg ▸ ha (lt_of_not_ge hsH) hbH hs.2)
  · intro s hs
    have hsH := hbH.trans hs
    rw [F_eq_mul_G t hsH]
    exact mul_neg_of_pos_of_neg (sub_pos.mpr hsH) (hg ▸ ha hbH hsH hs)

/-- The scale numerator has a simple root above the harmonic mean. -/
theorem F_deriv_neg_at_root (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) {b : ℝ} (hb : H t < b) (hz : F t b = 0) :
    deriv (F t) b < 0 := by
  have hG := hasDerivAt_G hn ht hb
  have hg : G t b = 0 := by
    rw [F_eq_mul_G t hb] at hz
    exact (mul_eq_zero.mp hz).resolve_left (sub_ne_zero.mpr hb.ne')
  have hd := ((hasDerivAt_id b).sub_const (H t)).mul hG
  have hd' : HasDerivAt (F t) _ b := hd.congr_of_eventuallyEq (by
    filter_upwards [Ioi_mem_nhds hb] with s hs
    exact F_eq_mul_G t hs)
  rw [hd'.deriv, hg]
  simp only [id_eq, mul_zero, zero_add, ← hG.deriv]
  exact mul_neg_of_pos_of_neg (sub_pos.mpr hb) (G_deriv_neg hn ht hne hb)

/-- Nonconstant positive data have a unique joint MLE with scale in the strict bracket. -/
theorem jointMLE (hn : 0 < n) {t : Fin n → ℝ} (ht : ∀ i, 0 < t i)
    (hne : ∃ i j, t i ≠ t j) :
    ∃ p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik t q.2 q.1) domain p ∧
      p.1 ∈ Ioo (H t) ((H t + A t) / 2) ∧
      ∀ q ∈ domain, IsMaxOn (fun r ↦ logLik t r.2 r.1) domain q → q = p := by
  obtain ⟨b, hb, _, hleft, hright⟩ := scale_root_signs hn ht hne
  have hb0 := (H_pos hn ht).trans hb.1
  have hc : ContinuousOn (profile t) (Ioi 0) := fun s hs ↦
    (hasDerivAt_profile hn ht hne hs).continuousAt.continuousWithinAt
  have hl : StrictMonoOn (profile t) (Ioc 0 b) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioc _ _) (hc.mono fun _ hs ↦ hs.1)
    rw [interior_Ioc]
    intro s hs
    rw [(hasDerivAt_profile hn ht hne hs.1).deriv]
    exact mul_pos (scale_score_factor_pos hn ht hne hs.1) (hleft s hs)
  have hr : StrictAntiOn (profile t) (Ici b) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici _) (hc.mono fun _ hs ↦ hb0.trans_le hs)
    rw [interior_Ici]
    intro s hs
    have hs0 := hb0.trans hs
    rw [(hasDerivAt_profile hn ht hne hs0).deriv]
    exact mul_neg_of_pos_of_neg (scale_score_factor_pos hn ht hne hs0) (hright s hs)
  have hstrict (s : ℝ) (hs : 0 < s) (hne' : s ≠ b) : profile t s < profile t b := by
    rcases lt_or_gt_of_ne hne' with h | h
    · exact hl ⟨hs, h.le⟩ ⟨hb0, le_rfl⟩ h
    · exact hr (show b ∈ Ici b from le_rfl) h.le h
  have hm : IsMaxOn (profile t) (Ioi 0) b := by
    intro s hs
    by_cases he : s = b
    · simp [he]
    · exact (hstrict s hs he).le
  have hf (s : ℝ) (hs : s ∈ Ioi (0 : ℝ)) :=
    heldScale_unique t (shapeSq_pos hn ht hne hs)
  have hfiber : ∀ s ∈ Ioi (0 : ℝ),
      shape t s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik t a s) (Ioi 0) (shape t s) := by
    intro s hs
    refine ⟨(hf s hs).1, fun a ha ↦ ?_⟩
    by_cases he : a = shape t s
    · simp [he]
    · exact ((hf s hs).2.2 a ha he).le
  have hfu : ∀ s ∈ Ioi (0 : ℝ), ∀ a ∈ Ioi (0 : ℝ),
      IsMaxOn (fun u ↦ logLik t u s) (Ioi 0) a → a = shape t s := by
    intro s hs a ha hmax
    by_contra he
    exact (not_le_of_gt ((hf s hs).2.2 a ha he)) (hmax (hf s hs).1)
  obtain ⟨hp, hmax⟩ := Generic.isMaxOn_prod_of_profile hfiber hb0 hm
  refine ⟨(b, shape t b), hp, hmax, hb, fun q hq hqm ↦ ?_⟩
  have hg := Generic.joint_maximizers_eq_profile_graph hfiber hfu
  have hmem : q ∈ {p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) |
      IsMaxOn (fun p ↦ logLik t p.2 p.1) (Ioi 0 ×ˢ Ioi 0) p} := ⟨hq, hqm⟩
  rw [hg] at hmem
  obtain ⟨s, ⟨hs, hsm⟩, rfl⟩ := hmem
  have he : s = b := by
    by_contra he
    exact (not_le_of_gt (hstrict s hs he)) (hsm hb0)
  rw [he]

/-- Zero squared shape makes the held-scale log-likelihood diverge at shape zero. -/
theorem heldScale_unbounded (t : Fin n → ℝ) {b : ℝ} (hq : shapeSq t b = 0) :
    Tendsto (fun a ↦ logLik t a b) (𝓝[>] 0) atTop := by
  have hlim : Tendsto (fun a : ℝ ↦ -log a) (𝓝[>] 0) atTop :=
    tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero
  refine tendsto_atTop.2 fun M ↦ ?_
  filter_upwards [tendsto_atTop.1 hlim (M - (scaleTerm t b + dataTerm t))] with a ha
  rw [logLik, hq, zero_div, sub_zero]
  change M - (scaleTerm t b + dataTerm t) ≤ -log a at ha
  linarith

/-- A constant sample at its common scale has log-likelihood diverging as shape tends to zero. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun a ↦ logLik (fun _ : Fin n ↦ c) a c) (𝓝[>] 0) atTop :=
  heldScale_unbounded _ ((shapeSq_zero_iff hn (fun _ ↦ hc) hc).mpr fun _ ↦ rfl)

end

end UnivariateML.Fatigue
