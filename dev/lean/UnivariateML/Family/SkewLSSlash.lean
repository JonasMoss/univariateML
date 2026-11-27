module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Complex.ExponentialBounds
public import Mathlib.Analysis.Calculus.Deriv.Slope
public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import UnivariateML.Generic.Score
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Slash held-location score and vertical collapse

## Deviations

The free-centre comparison applies to exact opposite-pair residual
blocks, as in the corrected written proof. The held-centre scale
classification applies to every nonempty sample.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.SkewLSSlash

noncomputable section

def scaleScore {n : ℕ} (r : Fin n → ℝ) (s : ℝ) : ℝ :=
  ∑ i, if r i = 0 then -1 else
    1 - 2 * ((r i)^2 / (2*s^2) / (exp ((r i)^2 / (2*s^2)) - 1))

/-- The held-location log-scale score strictly decreases unless all residuals vanish. -/
theorem scale_score_strictAnti {n : ℕ} {r : Fin n → ℝ} (hr : ∃ i, r i ≠ 0) :
    StrictAntiOn (scaleScore r) (Ioi 0) := by
  intro s hs t ht hst
  have hs' : 0 < s := hs
  have ht' : 0 < t := ht
  have hterm (i : Fin n) (hi : r i ≠ 0) :
      (1 - 2 * ((r i)^2 / (2*t^2) / (exp ((r i)^2 / (2*t^2)) - 1))) <
      (1 - 2 * ((r i)^2 / (2*s^2) / (exp ((r i)^2 / (2*s^2)) - 1))) := by
    let a := (r i)^2 / (2*t^2)
    let b := (r i)^2 / (2*s^2)
    have ha : 0 < a := by dsimp [a]; positivity
    have hb : 0 < b := by dsimp [b]; positivity
    have hab : a < b := by
      exact div_lt_div_of_pos_left (sq_pos_of_ne_zero hi) (by positivity) (by nlinarith)
    have hc := strictConvexOn_exp.secant_strict_mono (mem_univ (0 : ℝ)) (mem_univ a)
      (mem_univ b) ha.ne' hb.ne' hab
    simp only [exp_zero, sub_zero] at hc
    have hpa : 0 < exp a - 1 := sub_pos.mpr (one_lt_exp_iff.mpr ha)
    have hpb : 0 < exp b - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hb)
    have he := (div_lt_div_iff₀ ha hb).mp hc
    have hlt : b / (exp b - 1) < a / (exp a - 1) :=
      (div_lt_div_iff₀ hpb hpa).mpr (by simpa only [mul_comm] using he)
    change 1 - 2 * (a / (exp a - 1)) < 1 - 2 * (b / (exp b - 1))
    linarith
  unfold scaleScore
  obtain ⟨i, hi⟩ := hr
  apply Finset.sum_lt_sum _ ⟨i, Finset.mem_univ _, by simpa [hi] using hterm i hi⟩
  intro j _
  by_cases hj : r j = 0
  · simp [hj]
  · simpa [hj] using (hterm j hj).le

private def c : ℝ := 1 / sqrt (2 * π)

private theorem c_pos : 0 < c := by unfold c; positivity

/-- Slash density at residual `r` in the fitted scale convention. -/
def density (r s : ℝ) : ℝ :=
  if r = 0 then (1 / sqrt (2 * π)) / (2*s) else
    (1 / sqrt (2 * π)) * s / r^2 * (1 - exp (-(r^2/(2*s^2))))

/-- The held-centre log likelihood in the original positive scale. -/
def logLik {n : ℕ} (r : Fin n → ℝ) (s : ℝ) : ℝ := ∑ i, log (density (r i) s)

private def u (r t : ℝ) : ℝ := r^2 / 2 * exp (-2*t)

private theorem u_pos {r : ℝ} (hr : r ≠ 0) (t : ℝ) : 0 < u r t := by
  unfold u
  exact mul_pos (div_pos (sq_pos_of_ne_zero hr) (by norm_num)) (exp_pos _)

private theorem u_scale (r t : ℝ) : u r t = r^2 / (2*(exp t)^2) := by
  rw [u, show -2*t = -(t+t) by ring, exp_neg, exp_add]
  ring

private theorem u_deriv (r t : ℝ) : HasDerivAt (u r) (-2*u r t) t := by
  have hd : HasDerivAt (fun t : ℝ ↦ -2*t) (-2) t := by
    simpa only [id_eq, mul_one] using (hasDerivAt_id t).const_mul (-2 : ℝ)
  convert hd.exp.const_mul (r^2/2) using 1
  · rfl
  · change -2*(r^2/2*exp (-2*t)) = _
    ring

private theorem term_log {r : ℝ} (hr : r ≠ 0) (t : ℝ) :
    log (density r (exp t)) = log (c/r^2) + t + log (1-exp (-(u r t))) := by
  have h0 : 0 < 1 - exp (-(u r t)) :=
    sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (u_pos hr t)))
  have hc : 0 < c/r^2 := div_pos c_pos (sq_pos_of_ne_zero hr)
  have he : density r (exp t) = (c/r^2 * exp t) * (1-exp (-(u r t))) := by
    simp only [density, hr, ite_false, u_scale, c]
    ring
  rw [he, log_mul (mul_ne_zero hc.ne' (exp_pos _).ne') h0.ne',
    log_mul hc.ne' (exp_pos _).ne', log_exp]

private theorem term_log_tied (t : ℝ) :
    log (density 0 (exp t)) = log (c/2) - t := by
  have hc : 0 < c/2 := div_pos c_pos (by norm_num)
  have he : density 0 (exp t) = (c/2) / exp t := by simp only [density, ite_true, c]; ring
  rw [he, log_div hc.ne' (exp_pos _).ne', log_exp]

private theorem term_deriv (r t : ℝ) :
    HasDerivAt (fun y ↦ log (density r (exp y)))
      (if r = 0 then -1 else 1 - 2 * (u r t / (exp (u r t)-1))) t := by
  by_cases hr : r = 0
  · subst r
    simp only [ite_true]
    have hd := (hasDerivAt_const t (log (c/2))).sub (hasDerivAt_id t)
    convert hd using 1
    · exact funext term_log_tied
    · norm_num
  · simp only [hr, ite_false]
    have hne : 1 - exp (-(u r t)) ≠ 0 :=
      (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (u_pos hr t)))).ne'
    have hd := ((hasDerivAt_const t (1 : ℝ)).sub (u_deriv r t).neg.exp).log hne
    have hbase := (hasDerivAt_const t (log (c/r^2))).add (hasDerivAt_id t)
    convert hbase.add hd using 1
    · exact funext (term_log hr)
    · change 1-2*(u r t / (exp (u r t)-1)) =
        0+1+(0-exp (-(u r t)) * -(-2*u r t))/(1-exp (-(u r t)))
      simp only [zero_add, zero_sub, neg_mul, neg_neg, exp_neg]
      have hep : exp (u r t) ≠ 0 := (exp_pos _).ne'
      have hden : exp (u r t) - 1 ≠ 0 := (sub_pos.mpr (one_lt_exp_iff.mpr (u_pos hr t))).ne'
      change 1-2*(u r t / (exp (u r t)-1)) =
        1 + -( (exp (u r t))⁻¹ * (2 * u r t)) / (1-(exp (u r t))⁻¹)
      have hrec : 1 - (exp (u r t))⁻¹ ≠ 0 := by simpa only [exp_neg] using hne
      field_simp
      ring

/-- The scale score is the derivative in log scale of the actual likelihood. -/
theorem log_scale_derivative {n : ℕ} (r : Fin n → ℝ) (t : ℝ) :
    HasDerivAt (fun y ↦ logLik r (exp y)) (scaleScore r (exp t)) t := by
  unfold logLik scaleScore
  simp only [← u_scale]
  exact HasDerivAt.fun_sum fun i _ ↦ term_deriv (r i) t

private theorem u_atBot {r : ℝ} (hr : r ≠ 0) : Tendsto (u r) atBot atTop := by
  have h := tendsto_id.const_mul_atBot_of_neg (show (-2 : ℝ) < 0 by norm_num)
  change Tendsto (fun t : ℝ ↦ r^2/2*exp (-2*t)) atBot atTop
  simpa [Function.comp_def] using (tendsto_exp_atTop.comp h).const_mul_atTop
    (div_pos (sq_pos_of_ne_zero hr) (by norm_num : (0 : ℝ) < 2))

private theorem u_atTop {r : ℝ} (hr : r ≠ 0) : Tendsto (u r) atTop (𝓝[>] 0) := by
  apply tendsto_inf.mpr
  exact ⟨by
    have h := tendsto_id.const_mul_atTop_of_neg (show (-2 : ℝ) < 0 by norm_num)
    change Tendsto (fun t : ℝ ↦ r^2/2*exp (-2*t)) atTop (𝓝 0)
    simpa [Function.comp_def] using (tendsto_exp_atBot.comp h).const_mul (r^2/2),
    tendsto_principal.mpr (Eventually.of_forall fun t ↦ u_pos hr t)⟩

private theorem ratio_atTop :
    Tendsto (fun y : ℝ ↦ y / (exp y - 1)) atTop (𝓝 0) := by
  have h := tendsto_inv_atTop_zero.comp (tendsto_exp_div_pow_atTop 1)
  have hnorm : Tendsto (fun y : ℝ ↦ 1-exp (-y)) atTop (𝓝 1) := by
    simpa using (tendsto_const_nhds (x := (1 : ℝ))).sub tendsto_exp_neg_atTop_nhds_zero
  have hd := h.div hnorm one_ne_zero
  simp only [pow_one, zero_div] at hd
  refine hd.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with y hy
  have hex : exp y ≠ 0 := (exp_pos _).ne'
  have hgap : exp y - 1 ≠ 0 := (sub_pos.mpr (one_lt_exp_iff.mpr hy)).ne'
  change (exp y / y)⁻¹ / (1 - exp (-y)) = y / (exp y - 1)
  rw [inv_div, exp_neg]
  field_simp

private theorem ratio_atZero :
    Tendsto (fun y : ℝ ↦ y / (exp y - 1)) (𝓝[>] 0) (𝓝 1) := by
  have ht : Tendsto (fun y : ℝ ↦ (exp y - 1)/y) (𝓝[>] 0) (𝓝 1) := by
    simpa [div_eq_mul_inv, mul_comm] using (hasDerivAt_exp 0).tendsto_slope_zero_right
  simpa only [inv_div, inv_one] using ht.inv₀ one_ne_zero

/-- Number of observations at the held centre. -/
def ties {n : ℕ} (r : Fin n → ℝ) : ℕ := (Finset.univ.filter fun i ↦ r i = 0).card

private theorem vertical_exponent_sum {n : ℕ} (r : Fin n → ℝ) :
    (∑ i : Fin n, if r i = 0 then (-1 : ℝ) else 1) = n - 2 * ties r := by
  have he (i : Fin n) : (if r i = 0 then (-1 : ℝ) else 1) =
      1 - if r i = 0 then 2 else 0 := by split_ifs <;> norm_num
  simp only [he, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, mul_one, ties, ← Finset.sum_filter]
  simp [mul_comm]

/-- The log-scale score tends to the collapse exponent and to minus the
sample size at the two ends of the log-scale line. -/
theorem log_scale_score_limits {n : ℕ} (r : Fin n → ℝ) :
    Tendsto (fun t ↦ scaleScore r (exp t)) atBot (𝓝 ((n : ℝ) - 2 * ties r)) ∧
      Tendsto (fun t ↦ scaleScore r (exp t)) atTop (𝓝 (-(n : ℝ))) := by
  unfold scaleScore
  simp only [← u_scale]
  constructor
  · rw [← vertical_exponent_sum r]
    refine tendsto_finsetSum Finset.univ fun i _ ↦ ?_
    by_cases hi : r i = 0
    · simp [hi]
    · simpa [hi, Function.comp_apply] using
        ((ratio_atTop.comp (u_atBot hi)).const_mul (2 : ℝ)).const_sub 1
  · have hsum' : (∑ _ : Fin n, (-1 : ℝ)) = -(n : ℝ) := by simp [nsmul_eq_mul]
    rw [← hsum']
    refine tendsto_finsetSum Finset.univ fun i _ ↦ ?_
    by_cases hi : r i = 0
    · simp [hi]
    · simpa only [hi, ite_false, Function.comp_apply,
        show (1 : ℝ) - 2*1 = -1 by norm_num] using
        ((ratio_atZero.comp (u_atTop hi)).const_mul (2 : ℝ)).const_sub 1

private theorem log_scale_score_continuous {n : ℕ} (r : Fin n → ℝ) :
    Continuous (fun t ↦ scaleScore r (exp t)) := by
  unfold scaleScore
  simp only [← u_scale]
  refine continuous_finsetSum Finset.univ (fun i _ ↦ ?_)
  by_cases hi : r i = 0
  · simpa only [hi, ite_true] using
      (continuous_const : Continuous (fun _ : ℝ ↦ (-1 : ℝ)))
  · simp only [hi, ite_false]
    have hc : Continuous (u (r i)) := continuous_iff_continuousAt.mpr
      (fun t ↦ (u_deriv (r i) t).continuousAt)
    exact continuous_const.sub (continuous_const.mul
      (hc.div ((continuous_exp.comp hc).sub continuous_const) (fun t ↦
        (sub_pos.mpr (one_lt_exp_iff.mpr (u_pos hi t))).ne')))

private theorem log_scale_score_strictAnti {n : ℕ} {r : Fin n → ℝ} (hr : ∃ i, r i ≠ 0) :
    StrictAnti (fun t ↦ scaleScore r (exp t)) :=
  fun _ _ h ↦ scale_score_strictAnti hr (exp_pos _) (exp_pos _) (exp_lt_exp.mpr h)

private theorem nonzero_of_nonnegative_exponent {n : ℕ} (hn : 0 < n)
    {r : Fin n → ℝ} (he : 0 ≤ (n : ℝ) - 2 * ties r) : ∃ i, r i ≠ 0 := by
  by_contra! hall
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  simp [ties, hall] at he
  linarith

/-- At held centre, the unique scale maximizer exists exactly when fewer
than half the observations are tied there. -/
theorem held_centre_scale_iff {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ) :
    (∃ s ∈ Ioi (0 : ℝ), IsMaxOn (logLik r) (Ioi 0) s) ↔ 2 * ties r < n := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  constructor
  · rintro ⟨s, hs, hm⟩
    by_contra! he
    have he' : (n : ℝ) - 2 * ties r ≤ 0 := by
      have : (n : ℝ) ≤ 2*(ties r : ℝ) := by exact_mod_cast he
      linarith
    have hneg : scaleScore r (exp (log s)) < 0 := by
      by_cases hr : ∃ i, r i ≠ 0
      · have ha := log_scale_score_strictAnti hr
        let t := log s - 1
        have hbound : scaleScore r (exp t) ≤ (n : ℝ) - 2 * ties r :=
          ge_of_tendsto (log_scale_score_limits r).1
            ((eventually_le_atBot t).mono (fun _ hy ↦ ha.antitone hy))
        exact (ha (show t < log s by dsimp [t]; linarith)).trans_le (hbound.trans he')
      · push Not at hr
        simp [scaleScore, hr, nsmul_eq_mul, hn']
    have hl : IsLocalMax (fun t ↦ logLik r (exp t)) (log s) :=
      Eventually.of_forall fun t ↦ by
        change logLik r (exp t) ≤ logLik r (exp (log s))
        rw [exp_log hs]
        exact hm (show exp t ∈ Ioi (0 : ℝ) from exp_pos _)
    exact hneg.ne ((log_scale_derivative r (log s)).deriv.symm.trans hl.deriv_eq_zero)
  · intro he
    have he' : (0 : ℝ) < n - 2 * ties r := by
      have : (2 : ℝ) * (ties r : ℝ) < n := by exact_mod_cast he
      linarith
    have hr := nonzero_of_nonnegative_exponent hn he'.le
    obtain ⟨⟨s, ⟨hs, hz⟩, _⟩, hm⟩ := Generic.existsUnique_log_score_zero
      (log_scale_derivative r) (log_scale_score_continuous r) (log_scale_score_strictAnti hr)
      he' (neg_neg_of_pos hn') (log_scale_score_limits r).1 (log_scale_score_limits r).2
    exact ⟨s, hs, (hm s hs).mpr hz⟩

/-- In the attained regime the maximizing scale is unique and is exactly the
positive zero of the scale score. -/
theorem held_centre_scale_unique {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ)
    (he : 2 * ties r < n) :
    (∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik r) (Ioi 0) s) ∧
      ∀ s > 0, IsMaxOn (logLik r) (Ioi 0) s ↔ scaleScore r s = 0 := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have he' : (0 : ℝ) < n - 2 * ties r := by
    have : (2 : ℝ) * (ties r : ℝ) < n := by exact_mod_cast he
    linarith
  have hr := nonzero_of_nonnegative_exponent hn he'.le
  obtain ⟨⟨s, ⟨hs, hz⟩, hu⟩, hm⟩ := Generic.existsUnique_log_score_zero
    (log_scale_derivative r) (log_scale_score_continuous r) (log_scale_score_strictAnti hr)
    he' (neg_neg_of_pos hn') (log_scale_score_limits r).1 (log_scale_score_limits r).2
  refine ⟨⟨s, ⟨hs, (hm s hs).mpr hz⟩, ?_⟩, fun t ht ↦ ?_⟩
  · rintro t ⟨ht, hmax⟩
    exact hu t ⟨ht, (hm t ht).mp hmax⟩
  · simpa only [exp_log ht] using hm t ht

/-- Constant term of the log likelihood at the held centre as scale tends
to zero, after removal of the collapse exponent. -/
def collapseLog {n : ℕ} (r : Fin n → ℝ) : ℝ :=
  ∑ i, if r i = 0 then log ((1 / sqrt (2 * π))/2)
       else log ((1 / sqrt (2 * π))/(r i)^2)

private theorem logLik_decompose {n : ℕ} (r : Fin n → ℝ) (t : ℝ) :
    logLik r (exp t) = ((n : ℝ)-2 * ties r)*t + collapseLog r +
      ∑ i, if r i = 0 then 0 else log (1-exp (-(u (r i) t))) := by
  rw [← vertical_exponent_sum r, Finset.sum_mul]
  unfold logLik collapseLog
  rw [← Finset.sum_add_distrib, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : r i = 0
  · simp only [hi, ite_true, neg_mul, one_mul, add_zero]
    rw [term_log_tied]
    unfold c
    ring
  · simp only [hi, ite_false, one_mul]
    rw [term_log hi]
    unfold c
    ring

private theorem collapse_remainder {n : ℕ} (r : Fin n → ℝ) :
    Tendsto (fun t : ℝ ↦ ∑ i, if r i = 0 then 0 else log (1-exp (-(u (r i) t))))
      atBot (𝓝 0) := by
  rw [← Finset.sum_const_zero (s := (Finset.univ : Finset (Fin n)))]
  refine tendsto_finsetSum Finset.univ (fun i _ ↦ ?_)
  by_cases hi : r i = 0
  · simp [hi]
  · have hex := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (u_atBot hi))
    have h : Tendsto (fun t ↦ 1 - exp (-(u (r i) t))) atBot (𝓝 1) := by
      simpa [Function.comp_def] using hex.const_sub (1 : ℝ)
    simpa [hi, Function.comp_def] using (continuousAt_log one_ne_zero).tendsto.comp h

/-- A strict majority at the held location sends the log likelihood to
positive infinity as scale tends to zero. -/
theorem held_centre_unbounded {n : ℕ} (r : Fin n → ℝ) (he : n < 2 * ties r) :
    Tendsto (logLik r) (𝓝[>] 0) atTop := by
  have he' : (n : ℝ)-2 * ties r < 0 := by
    have : (n : ℝ) < (2 : ℝ) * (ties r : ℝ) := by exact_mod_cast he
    linarith
  have ht : Tendsto (fun t ↦ logLik r (exp t)) atBot atTop := by
    simp_rw [logLik_decompose]
    exact ((tendsto_id.const_mul_atBot_of_neg he').atTop_add
      (tendsto_const_nhds (x := collapseLog r))).atTop_add (collapse_remainder r)
  refine (ht.comp tendsto_log_nhdsGT_zero).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
  simp [Function.comp_apply, exp_log hs]

/-- A half tie at the held location gives the finite, unattained
scale-zero log-likelihood supremum. -/
theorem held_centre_half_tie {n : ℕ} (hn : 0 < n) (r : Fin n → ℝ) (he : n = 2 * ties r) :
    Tendsto (logLik r) (𝓝[>] 0) (𝓝 (collapseLog r)) ∧
      IsLUB (logLik r '' Ioi 0) (collapseLog r) ∧
      ∀ s > 0, logLik r s < collapseLog r := by
  have he' : (n : ℝ)-2 * ties r = 0 := sub_eq_zero.mpr (by exact_mod_cast he)
  have ht : Tendsto (fun t ↦ logLik r (exp t)) atBot (𝓝 (collapseLog r)) := by
    simp_rw [logLik_decompose, he', zero_mul, zero_add]
    simpa only [add_zero] using (collapse_remainder r).const_add (collapseLog r)
  have ht' : Tendsto (logLik r) (𝓝[>] 0) (𝓝 (collapseLog r)) := by
    refine (ht.comp tendsto_log_nhdsGT_zero).congr' ?_
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
    simp [Function.comp_apply, exp_log hs]
  obtain ⟨i, hi⟩ := nonzero_of_nonnegative_exponent hn (le_of_eq he'.symm)
  have hstrict (s : ℝ) (hs : 0 < s) : logLik r s < collapseLog r := by
    let t := log s
    rw [← exp_log hs, logLik_decompose, he', zero_mul, zero_add]
    have hterm (j : Fin n) (hj : r j ≠ 0) :
        log (1-exp (-(u (r j) t))) < 0 := by
      apply log_neg
      · exact sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (u_pos hj t)))
      · linarith [exp_pos (-(u (r j) t))]
    have hsum : (∑ j, if r j = 0 then 0 else log (1-exp (-(u (r j) t)))) < 0 := by
      rw [← Finset.sum_const_zero (s := (Finset.univ : Finset (Fin n)))]
      refine Finset.sum_lt_sum (fun j _ ↦ ?_) ⟨i, Finset.mem_univ _, ?_⟩
      · by_cases hj : r j = 0
        · simp [hj]
        · simpa [hj] using (hterm j hj).le
      · simpa [hi] using hterm i hi
    linarith
  refine ⟨ht', ⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨s, hs, rfl⟩
    exact (hstrict s hs).le
  · intro z hz
    have h : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    exact le_of_tendsto ht' (h.mono fun s hs ↦ hz (mem_image_of_mem _ hs))

/-- One tied and one nonzero observation contribute this factor when
the scale powers are cancelled before taking the limit. -/
private def collapseBlock (c d s : ℝ) : ℝ :=
  c^2 / (2*d^2) * (1 - exp (-(d^2 / (2*s^2))))

/-- The vertical half-tie limit is the product of the cancelled block limits. -/
private theorem block_product_limit {k : ℕ} (c : ℝ) (d : Fin k → ℝ) (hd : ∀ i, d i ≠ 0) :
    Tendsto (fun s ↦ ∏ i, collapseBlock c (d i) s) (𝓝[>] 0)
      (𝓝 (∏ i, c^2 / (2*(d i)^2))) := by
  refine tendsto_finsetProd _ fun i _ ↦ ?_
  have hpow : Tendsto (fun s : ℝ ↦ (s⁻¹)^2) (𝓝[>] 0) atTop :=
    tendsto_inv_nhdsGT_zero.atTop_pow₀ (show (2 : ℕ) ≠ 0 by decide)
  have h := hpow.const_mul_atTop_of_neg
    (show -(d i)^2 / 2 < 0 by have := sq_pos_of_ne_zero (hd i); linarith)
  have he : Tendsto (fun s : ℝ ↦ -((d i)^2 / (2*s^2))) (𝓝[>] 0) atBot := by
    convert h using 1
    ext s
    simp only [inv_pow]
    ring
  unfold collapseBlock
  have hfactor := (tendsto_const_nhds (x := (1 : ℝ))).sub (tendsto_exp_atBot.comp he)
  simpa [Function.comp_apply] using hfactor.const_mul (c^2 / (2*(d i)^2))

/-- At a held centre with an exact half tie, every positive scale lies
strictly below the vertical collapse value. -/
private theorem block_product_strict {k : ℕ} (hk : 0 < k) {c : ℝ} (hc : 0 < c)
    (d : Fin k → ℝ) (hd : ∀ i, d i ≠ 0) {s : ℝ} (hs : 0 < s) :
    (∏ i, collapseBlock c (d i) s) < ∏ i, c^2 / (2*(d i)^2) := by
  let : Nonempty (Fin k) := ⟨⟨0, hk⟩⟩
  refine Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ ?_) (fun i _ ↦ ?_) Finset.univ_nonempty
  · unfold collapseBlock
    have h : 0 < (d i)^2 / (2*s^2) := div_pos (sq_pos_of_ne_zero (hd i)) (by positivity)
    exact mul_pos (div_pos (by positivity) (by have := sq_pos_of_ne_zero (hd i); positivity))
      (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos h)))
  · unfold collapseBlock
    exact mul_lt_of_lt_one_right
      (div_pos (by positivity) (by have := sq_pos_of_ne_zero (hd i); positivity))
      (by linarith [exp_pos (-((d i)^2 / (2*s^2)))])

private theorem block_identity {d s : ℝ} (hd : d ≠ 0) (hs : 0 < s) :
    density 0 s * density d s = collapseBlock c d s := by
  simp only [density, collapseBlock, c, ite_true, hd, ite_false]
  field_simp [hd, hs.ne']

/-- The vertical half-tie path pairs each tied observation with one
nonzero residual and converges to the product of the cancelled factors. -/
theorem half_tie_path {k : ℕ} (d : Fin k → ℝ) (hd : ∀ i, d i ≠ 0) :
    Tendsto (fun s ↦ ∏ i, density 0 s * density (d i) s) (𝓝[>] 0)
      (𝓝 (∏ i, (1 / sqrt (2 * π))^2 / (2*(d i)^2))) := by
  refine (block_product_limit c d hd).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
  exact Finset.prod_congr rfl (fun i _ ↦ (block_identity (hd i) hs).symm)

/-- On the held-centre half-tie slice every positive scale is strictly
below the vertical trace, stated using the normalized likelihood. -/
theorem half_tie_strict {k : ℕ} (hk : 0 < k) (d : Fin k → ℝ)
    (hd : ∀ i, d i ≠ 0) {s : ℝ} (hs : 0 < s) :
    (∏ i, density 0 s * density (d i) s) <
      ∏ i, (1 / sqrt (2 * π))^2 / (2*(d i)^2) := by
  simp only [block_identity (hd _) hs]
  exact block_product_strict hk c_pos d hd hs

private def A (t : ℝ) : ℝ := if t = 0 then 1 else (1-exp (-t))/t

private theorem A_pos {t : ℝ} (ht : 0 ≤ t) : 0 < A t := by
  rcases ht.eq_or_lt with h | h
  · subst t
    simp [A]
  · simp only [A, h.ne', ite_false]
    exact div_pos (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos h))) h

private theorem A_le_one {t : ℝ} (ht : 0 ≤ t) : A t ≤ 1 := by
  rcases ht.eq_or_lt with h | h
  · subst t
    simp [A]
  · simp only [A, h.ne', ite_false]
    apply (div_le_iff₀ h).mpr
    linarith [add_one_le_exp (-t)]

private theorem A_lt_inv {t : ℝ} (ht : 0 < t) : A t < 1/t := by
  simp only [A, ht.ne', ite_false]
  exact (div_lt_div_iff_of_pos_right ht).mpr (by linarith [exp_pos (-t)])

private theorem A_le_rational {t : ℝ} (ht : 0 ≤ t) : A t ≤ 1/(1+t/2) := by
  let f (t : ℝ) := (1+t/2)*exp (-t) - (1-t/2)
  have hd (t : ℝ) : HasDerivAt f ((1-(1+t)*exp (-t))/2) t := by
    have h := (hasDerivAt_id t).div_const (2 : ℝ)
    have ht := ((h.const_add 1).mul (hasDerivAt_id t).neg.exp).sub (h.const_sub 1)
    convert ht using 1
    · rfl
    · change (1-(1+t)*exp (-t))/2 =
        1/2*exp (-t)+(1+t/2)*(exp (-t)*(-1)) - (- (1/2))
      ring
  have hmono := monotoneOn_of_deriv_nonneg (convex_Ici (0 : ℝ))
    (show Continuous f from continuous_iff_continuousAt.mpr
      fun t ↦ (hd t).continuousAt).continuousOn
    (fun t _ ↦ (hd t).differentiableAt.differentiableWithinAt)
    (fun t _ ↦ by
      rw [(hd t).deriv]
      have he := mul_le_mul_of_nonneg_right (add_one_le_exp t) (exp_pos (-t)).le
      rw [← exp_add, add_neg_cancel, exp_zero] at he
      linarith)
  have hf : 0 ≤ f t := by
    have hh := hmono (show (0 : ℝ) ∈ Ici 0 from le_rfl) ht ht
    have he : f 0 = 0 := by simp [f]
    rwa [he] at hh
  rcases ht.eq_or_lt with h | h
  · subst t
    simp [A]
  · simp only [A, h.ne', ite_false]
    apply (div_le_div_iff₀ h (show 0 < 1+t/2 by linarith)).mpr
    dsimp [f] at hf
    nlinarith only [hf]

private theorem small_pair_exponential {x y : ℝ} (hx : 0 ≤ x) (hx' : x < 1 / 2)
    (hy : y ≤ 8 / 3) : 2*x^2 < exp (-(y*(1-x)^2)) := by
  let u := 1/2-x
  have hu : 0 ≤ u := by dsimp [u]; linarith
  have hu' : u ≤ 1/2 := by dsimp [u]; linarith
  have hlin : (8/3 : ℝ)*(u+u^2) ≤ 4*u := by
    nlinarith [mul_nonneg hu (sub_nonneg.mpr hu')]
  have he : -(2/3 : ℝ) - 4*u ≤ -(y*(1-x)^2) := by
    have hh := mul_le_mul_of_nonneg_right hy (sq_nonneg (1-x))
    dsimp [u] at hlin ⊢
    nlinarith only [hh, hlin]
  have hlog : (2/3 : ℝ) < log 2 := by
    linarith [log_two_gt_d9]
  have hhalf : (1/2 : ℝ) < exp (-(2/3)) := by
    rw [show (1/2 : ℝ) = exp (-log 2) by rw [exp_neg, exp_log (by norm_num)]; norm_num]
    exact exp_lt_exp.mpr (by linarith)
  have hs : 2*x ≤ exp (-2*u) := by
    have h := add_one_le_exp (-2*u)
    dsimp [u] at h
    linarith
  have hsq := pow_le_pow_left₀ (show 0 ≤ 2*x by positivity) hs 2
  rw [show exp (-2*u)^2 = exp (-4*u) by
    rw [pow_two, ← exp_add]; congr 1; ring] at hsq
  calc
    2*x^2 ≤ (1/2)*exp (-4*u) := by nlinarith only [hsq]
    _ < exp (-(2/3))*exp (-4*u) := mul_lt_mul_of_pos_right hhalf (exp_pos _)
    _ ≤ _ := by rw [← exp_add]; exact exp_le_exp.mpr (by linarith)

private theorem paired_ratio_lt_one {x y : ℝ} (hx : 0 ≤ x) (hy : 0 < y) :
    y^2 * A (x^2*y)^2 * A (y*(1-x)^2) * A (y*(1+x)^2) < 1 := by
  let w := x^2*y
  let l := y*(1-x)^2
  let r := y*(1+x)^2
  have hw : 0 ≤ w := by dsimp [w]; positivity
  have hl : 0 ≤ l := by dsimp [l]; positivity
  have hr : 0 < r := by dsimp [r]; positivity
  change y^2 * A w ^ 2 * A l * A r < 1
  by_cases hx' : 1/2 ≤ x
  · have hb : w * A w ^ 2 ≤ 1/2 := by
      have hq : 0 < 1+w/2 := by linarith
      have hh := pow_le_pow_left₀ (A_pos hw).le (A_le_rational hw) 2
      calc
        _ ≤ w * (1/(1+w/2))^2 := mul_le_mul_of_nonneg_left hh hw
        _ ≤ _ := by
          rw [div_pow, one_pow, ← mul_div_assoc, mul_one]
          apply (div_le_iff₀ (sq_pos_of_pos hq)).mpr
          nlinarith [sq_nonneg (w-2)]
    have hx0 : 0 < x := by linarith
    have hmain : y * A w ^ 2 ≤ (1/2)/x^2 := by
      apply (le_div_iff₀ (sq_pos_of_pos hx0)).mpr
      dsimp [w] at hb
      calc
        _ = x^2*y*A (x^2*y)^2 := by ring
        _ ≤ _ := hb
    have hden : 1/2 < x^2*(1+x)^2 := by
      have hh₁ : (1/4 : ℝ) ≤ x^2 := by nlinarith
      have hh₂ : (9/4 : ℝ) ≤ (1+x)^2 := by nlinarith
      have hh := mul_le_mul hh₁ hh₂ (by norm_num : (0 : ℝ) ≤ 9/4) (sq_nonneg x)
      linarith
    calc
      _ ≤ y^2 * A w ^ 2 * 1 * (1/r) :=
        mul_le_mul (mul_le_mul_of_nonneg_left (A_le_one hl) (by positivity))
          (A_lt_inv hr).le (A_pos hr.le).le (by positivity)
      _ = (y*A w ^ 2)/(1+x)^2 := by dsimp [r]; field_simp
      _ ≤ ((1/2)/x^2)/(1+x)^2 :=
        div_le_div_of_nonneg_right hmain (sq_nonneg _)
      _ < 1 := by
        rw [div_div]
        exact (div_lt_iff₀ (mul_pos (sq_pos_of_pos hx0)
          (sq_pos_of_pos (by positivity : 0 < 1+x)))).mpr (by linarith)
  · have hxlt : x < 1/2 := lt_of_not_ge hx'
    have hlp : 0 < l := by
      dsimp [l]
      exact mul_pos hy (sq_pos_of_pos (by linarith))
    have hx2 : 0 ≤ x^2 := sq_nonneg _
    have hu : 0 < 1-x^2 := by nlinarith
    have hden : (1-x)^2*(1+x)^2 = (1-x^2)^2 := by ring
    by_cases hy' : 8/3 ≤ y
    · have hspread : 1 ≤ y*(1-x^2)/2 := by
        have hb : (3/4 : ℝ) ≤ 1-x^2 := by nlinarith
        nlinarith only [mul_le_mul hy' hb (by norm_num : (0 : ℝ) ≤ 3/4) hy.le]
      have hdom : A w ≤ 1-x^2 := by
        refine (A_le_rational hw).trans ?_
        apply (div_le_iff₀ (show 0 < 1+w/2 by linarith)).mpr
        have he := mul_nonneg hx2 (sub_nonneg.mpr hspread)
        dsimp [w]
        nlinarith only [he]
      calc
        _ < y^2*A w^2*(1/l)*(1/r) := by
          have hpos : 0 < y^2*A w^2 := by
            have := A_pos hw
            positivity
          exact (mul_le_mul_of_nonneg_right
            (mul_le_mul_of_nonneg_left (A_lt_inv hlp).le hpos.le) (A_pos hr.le).le).trans_lt
            (mul_lt_mul_of_pos_left (A_lt_inv hr) (mul_pos hpos (by positivity)))
        _ = A w^2/((1-x)^2*(1+x)^2) := by
          dsimp [l, r]
          field_simp [show 1-x ≠ 0 by linarith, show 1+x ≠ 0 by linarith]
        _ = A w^2/(1-x^2)^2 := by rw [hden]
        _ ≤ 1 := (div_le_one (sq_pos_of_pos hu)).mpr
          (pow_le_pow_left₀ (A_pos hw).le hdom 2)
    · have hlt := small_pair_exponential hx hxlt (le_of_not_ge hy')
      have hcore : 1 - exp (-l) < (1-x^2)^2 := by
        dsimp [l] at hlt ⊢
        nlinarith only [hlt, sq_nonneg (x^2)]
      have hfact : y^2*A w^2*A l*A r =
          (A w^2*(1-exp (-l))*(1-exp (-r)))/(1-x^2)^2 := by
        have hAl : A l = (1-exp (-l))/l := by simp [A, hlp.ne']
        have hAr : A r = (1-exp (-r))/r := by simp [A, hr.ne']
        rw [hAl, hAr, ← hden]
        have hlEq : l = y*(1-x)^2 := rfl
        have hrEq : r = y*(1+x)^2 := rfl
        generalize 1-exp (-l) = a
        generalize 1-exp (-r) = b
        rw [hlEq, hrEq]
        field_simp [show 1-x ≠ 0 by linarith, show 1+x ≠ 0 by linarith]
      have hp (t : ℝ) (ht : 0 < t) : 0 < 1-exp (-t) :=
        sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht))
      rw [hfact, div_lt_one (sq_pos_of_pos hu)]
      apply lt_of_le_of_lt _ hcore
      calc
        _ ≤ 1^2*(1-exp (-l))*1 := by
          have hh := pow_le_pow_left₀ (A_pos hw).le (A_le_one hw) 2
          exact mul_le_mul (mul_le_mul_of_nonneg_right hh (hp l hlp).le)
            (by linarith [exp_pos (-r)]) (hp r hr).le (by positivity [hp l hlp])
        _ = _ := by ring

private theorem density_A (v : ℝ) {s : ℝ} (hs : 0 < s) :
    density v s = (c/(2*s))*A (v^2/(2*s^2)) := by
  by_cases hv : v = 0
  · subst v
    simp [density, A, c]
  · have he : v^2/(2*s^2) ≠ 0 := by positivity
    simp only [density, c, hv, ite_false, A, he]
    field_simp [hv, hs.ne']

private theorem density_pos (v : ℝ) {s : ℝ} (hs : 0 < s) : 0 < density v s := by
  rw [density_A v hs]
  exact mul_pos (div_pos c_pos (by positivity)) (A_pos (by positivity))

/-- Product for a block with observations (0,0,-d,d), evaluated at
location `m` and scale `s`. -/
def pairedBlock (d m s : ℝ) : ℝ :=
  density (-m) s ^ 2 * density (-d-m) s * density (d-m) s

/-- Limit of a paired block at the tied centre and shrinking scale. -/
def pairedTrace (d : ℝ) : ℝ := ((1 / sqrt (2 * π))^2 / (2*d^2))^2

/-- The corrected opposite-pair inequality holds at every finite centre
and positive scale. -/
theorem paired_block_lt_collapse {d s : ℝ} (hd : 0 < d) (hs : 0 < s) (m : ℝ) :
    pairedBlock d m s < pairedTrace d := by
  let x := |m|/d
  let y := d^2/(2*s^2)
  have hx : 0 ≤ x := by dsimp [x]; positivity
  have hy : 0 < y := by dsimp [y]; positivity
  have hratio := paired_ratio_lt_one hx hy
  let P := y^2 * A (x^2*y)^2 * A (y*(1-x)^2) * A (y*(1+x)^2)
  change P < 1 at hratio
  have he0 : (-m)^2/(2*s^2) = x^2*y := by
    dsimp [x, y]
    rw [div_pow, sq_abs, neg_sq]
    field_simp [hd.ne', hs.ne']
  have he (m' : ℝ) :
      A ((-d-m')^2/(2*s^2))*A ((d-m')^2/(2*s^2)) =
      A (y*(1-m'/d)^2)*A (y*(1+m'/d)^2) := by
    have hp : (-d-m')^2/(2*s^2) = y*(1+m'/d)^2 := by
      dsimp [y]
      field_simp [hd.ne', hs.ne']
      ring
    have hm' : (d-m')^2/(2*s^2) = y*(1-m'/d)^2 := by
      dsimp [y]
      field_simp [hd.ne', hs.ne']
    rw [hp, hm', mul_comm]
  have hpair : A ((-d-m)^2/(2*s^2))*A ((d-m)^2/(2*s^2)) =
      A (y*(1-x)^2)*A (y*(1+x)^2) := by
    rcases le_or_gt 0 m with hm | hm
    · simpa only [x, abs_of_nonneg hm] using he m
    · rw [show (-d-m)^2 = (d-(-m))^2 by ring,
        show (d-m)^2 = (-d-(-m))^2 by ring, mul_comm]
      simpa only [x, abs_of_neg hm] using he (-m)
  have hid : pairedBlock d m s = pairedTrace d * P := by
    unfold pairedBlock
    rw [density_A _ hs, density_A _ hs, density_A _ hs, he0]
    calc
      _ = (c/(2*s))^4 * A (x^2*y)^2 *
          (A ((-d-m)^2/(2*s^2))*A ((d-m)^2/(2*s^2))) := by ring
      _ = _ := by
        rw [hpair]
        have hconst : (c/(2*s))^4 = pairedTrace d * y^2 := by
          dsimp [pairedTrace, c, y]
          field_simp [hd.ne', hs.ne']
        rw [hconst]
        dsimp [P]
        ring
  rw [hid]
  apply mul_lt_of_lt_one_right _ hratio
  unfold pairedTrace
  have : 0 < 1 / sqrt (2*π) := c_pos
  positivity

private theorem paired_block_vertical {d : ℝ} (hd : 0 < d) :
    Tendsto (pairedBlock d 0) (𝓝[>] 0) (𝓝 (pairedTrace d)) := by
  let e : Fin 1 → ℝ := fun _ ↦ d
  have ht := block_product_limit c e (fun _ ↦ hd.ne')
  have ht' : Tendsto (collapseBlock c d) (𝓝[>] 0) (𝓝 (c^2/(2*d^2))) := by
    simpa [e, Fin.prod_univ_one] using ht
  refine (ht'.pow 2).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
  unfold collapseBlock pairedBlock density c
  simp only [neg_zero, ite_true, sub_zero, hd.ne', neg_ne_zero.mpr hd.ne', ite_false, neg_sq]
  field_simp [hd.ne', hs.ne']

/-- If every off-point residual is paired with its opposite and each
such pair has two tied observations, the vertical limit is the strict
global supremum over all centres and positive scales. -/
theorem paired_collapse_supremum {k : ℕ} (hk : 0 < k) (d : Fin k → ℝ) (hd : ∀ i, 0 < d i) :
    IsLUB ((fun p : ℝ × ℝ ↦ ∏ i, pairedBlock (d i) p.1 p.2) '' (univ ×ˢ Ioi 0))
      (∏ i, pairedTrace (d i)) ∧
    ∀ m s : ℝ, 0 < s → (∏ i, pairedBlock (d i) m s) < ∏ i, pairedTrace (d i) := by
  let : Nonempty (Fin k) := ⟨⟨0, hk⟩⟩
  have hstrict (m s : ℝ) (hs : 0 < s) :
      (∏ i, pairedBlock (d i) m s) < ∏ i, pairedTrace (d i) := by
    refine Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ ?_)
      (fun i _ ↦ paired_block_lt_collapse (hd i) hs m) Finset.univ_nonempty
    unfold pairedBlock
    exact mul_pos (mul_pos (sq_pos_of_pos (density_pos _ hs)) (density_pos _ hs)) (density_pos _ hs)
  have ht := tendsto_finsetProd Finset.univ (fun i _ ↦ paired_block_vertical (hd i))
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hstrict p.1 p.2 hp.2).le
  · intro z hz
    have he : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    exact le_of_tendsto ht (he.mono fun s hs ↦ hz ⟨(0, s), ⟨mem_univ _, hs⟩, rfl⟩)

end

end UnivariateML.SkewLSSlash
