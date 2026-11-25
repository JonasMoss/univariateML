module

public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.Calculus.DSlope
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.Series
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Folded-normal likelihood with one coefficient held

The scale coordinate is physical variance. The likelihood is expressed per
observation and drops the parameter-independent Gaussian normalization.

## Deviations

The variance derivative bound uses 1/17 in place of the sharper source
constant. Strict decrease of the normalized location score requires a
positive observation. The all-zero sample has constant normalized score
and still has a unique maximizing location at zero.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.FoldedNormal

noncomputable section

variable {n : ℕ}

def avg (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

def D (x : Fin n → ℝ) (μ : ℝ) : ℝ := avg (fun i ↦ (x i - μ) ^ 2)

def m₂ (x : Fin n → ℝ) : ℝ := avg (fun i ↦ (x i) ^ 2)

def logLik (x : Fin n → ℝ) (μ t : ℝ) : ℝ :=
  -log t / 2 - D x μ / (2 * t) +
    avg (fun i ↦ log (1 + exp (-(2 * μ * x i) / t)))

def T (x : Fin n → ℝ) (μ t : ℝ) : ℝ :=
  t - D x μ - avg (fun i ↦ 4 * μ * x i / (1 + exp (2 * μ * x i / t)))

/-- The sum of the two Gaussian components on the nonnegative half-line. -/
def density (x μ t : ℝ) : ℝ :=
  (exp (-((x - μ) ^ 2) / (2 * t)) + exp (-((x + μ) ^ 2) / (2 * t))) /
    sqrt (2 * π * t)

private theorem avg_mono {a b : Fin n → ℝ} (h : ∀ i, a i ≤ b i) : avg a ≤ avg b :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (by positivity)

private theorem avg_const (hn : 0 < n) (c : ℝ) :
    avg (fun _ : Fin n ↦ c) = c := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [avg, nsmul_eq_mul, hN]

private theorem avg_add (a b : Fin n → ℝ) :
    avg (fun i ↦ a i + b i) = avg a + avg b := by
  simp [avg, Finset.sum_add_distrib, add_div]

private theorem avg_mul (c : ℝ) (a : Fin n → ℝ) :
    avg (fun i ↦ c * a i) = c * avg a := by
  rw [avg, avg, ← Finset.mul_sum, mul_div_assoc]

/-- The objective differs from the mean log-density by a Gaussian constant. -/
theorem avg_log_density (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    avg (fun i ↦ log (density (x i) μ t)) = logLik x μ t - log (2 * π) / 2 := by
  have hc : 0 < 2 * π := mul_pos (by norm_num) pi_pos
  have h (y : ℝ) : log (density y μ t) =
      (-(1 / (2 * t)) * (y - μ) ^ 2 + log (1 + exp (-(2 * μ * y) / t))) +
        (-log t / 2 - log (2 * π) / 2) := by
    unfold density
    have hfactor : exp (-((y - μ) ^ 2) / (2 * t)) + exp (-((y + μ) ^ 2) / (2 * t)) =
        exp (-((y - μ) ^ 2) / (2 * t)) * (1 + exp (-(2 * μ * y) / t)) := by
      rw [show -((y + μ) ^ 2) / (2 * t) =
        -((y - μ) ^ 2) / (2 * t) + -(2 * μ * y) / t by ring]
      rw [exp_add, mul_add, mul_one]
    rw [hfactor]
    rw [log_div (by positivity) (sqrt_pos.mpr (mul_pos hc ht)).ne',
      log_mul (exp_pos _).ne' (by positivity), log_exp,
      log_sqrt (mul_pos hc ht).le, log_mul hc.ne' ht.ne']
    ring
  simp only [h, avg_add, avg_mul, avg_const hn, logLik, D]
  ring

private theorem hasDerivAt_avg {f : Fin n → ℝ → ℝ} {d : Fin n → ℝ} {t : ℝ}
    (h : ∀ i, HasDerivAt (f i) (d i) t) :
    HasDerivAt (fun s ↦ avg (fun i ↦ f i s)) (avg d) t :=
  (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦ h i)).div_const (n : ℝ)

private theorem hasDerivAt_const_div (c : ℝ) {t : ℝ} (ht : t ≠ 0) :
    HasDerivAt (fun s ↦ c / s) (-c / t ^ 2) t := by
  simpa [Pi.div_def, id_eq] using (hasDerivAt_const t c).div (hasDerivAt_id t) ht

private theorem hasDerivAt_reflection (c : ℝ) {t : ℝ} (ht : t ≠ 0) :
    HasDerivAt (fun s ↦ log (1 + exp (-c / s)))
      (c / (1 + exp (c / t)) / t ^ 2) t := by
  have h := (((hasDerivAt_const_div (-c) ht).exp).const_add 1).log
    (show 1 + exp (-c / t) ≠ 0 by positivity)
  convert h using 1
  rw [neg_div, exp_neg]
  have he := exp_pos (c / t)
  field_simp
  ring

/-- The variance derivative of the mean log-likelihood. -/
theorem hasDerivAt_logLik_variance (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (logLik x μ) (-T x μ t / (2 * t ^ 2)) t := by
  have h := (((hasDerivAt_log ht.ne').neg.div_const 2).sub
    (hasDerivAt_const_div (D x μ / 2) ht.ne')).add
    (hasDerivAt_avg fun i ↦ hasDerivAt_reflection (2 * μ * x i) ht.ne')
  convert h using 1
  · funext s
    simp only [logLik, Pi.add_apply, Pi.sub_apply, Pi.neg_apply]
    ring
  · unfold T
    rw [show (fun i ↦ 2 * μ * x i / (1 + exp (2 * μ * x i / t)) / t ^ 2) =
        (fun i ↦ (1 / (2 * t ^ 2)) * (4 * μ * x i / (1 + exp (2 * μ * x i / t)))) by
      funext i
      ring, avg_mul]
    generalize avg (fun i ↦ 4 * μ * x i / (1 + exp (2 * μ * x i / t))) = A
    field_simp
    ring

private theorem cosh_lower (z : ℝ) : (17 / 16 : ℝ) * z ^ 2 ≤ 1 + cosh z := by
  have h := (hasSum_cosh z).summable.sum_le_tsum (s := Finset.range 3)
    (fun j _ ↦ div_nonneg (Even.pow_nonneg ⟨j, by omega⟩ _) (by positivity))
  rw [(hasSum_cosh z).tsum_eq] at h
  norm_num [Finset.sum_range_succ] at h
  nlinarith [sq_nonneg (z ^ 2 - 27 / 4)]

private theorem hasDerivAt_logistic (c : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (fun s ↦ 2 * c / (1 + exp (c / s)))
      ((c / t) ^ 2 / (1 + cosh (c / t))) t := by
  have h := (hasDerivAt_const t (2 * c)).div
    (((hasDerivAt_const_div c ht.ne').exp).const_add 1)
    (show 1 + exp (c / t) ≠ 0 by positivity)
  convert h using 1
  rw [cosh_eq, exp_neg]
  have he := exp_pos (c / t)
  have hd : 1 + (exp (c / t) + (exp (c / t))⁻¹) / 2 ≠ 0 := by positivity
  field_simp
  ring

private theorem hasDerivAt_T (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (T x μ)
      (1 - avg (fun i ↦ (2 * μ * x i / t) ^ 2 / (1 + cosh (2 * μ * x i / t)))) t := by
  convert ((hasDerivAt_id t).sub_const (D x μ)).sub
    (hasDerivAt_avg fun i ↦ hasDerivAt_logistic (2 * μ * x i) ht) using 1
  funext s
  simp only [T, id_eq]
  congr 2
  funext i
  ring

/-- A uniform positive lower bound for the derivative of the variance residual. -/
theorem deriv_T_lower (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {t : ℝ} (ht : 0 < t) :
    (1 / 17 : ℝ) ≤ deriv (T x μ) t := by
  rw [(hasDerivAt_T x μ ht).deriv]
  have h := avg_mono (a := fun i ↦ (2 * μ * x i / t) ^ 2 /
      (1 + cosh (2 * μ * x i / t))) (b := fun _ ↦ (16 / 17 : ℝ)) (fun i ↦ by
    apply (div_le_iff₀ (add_pos zero_lt_one (cosh_pos _))).2
    linarith [cosh_lower (2 * μ * x i / t)])
  rw [avg_const hn] at h
  linarith

private theorem T_strictMono (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    StrictMonoOn (T x μ) (Ioi 0) := by
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
  · exact fun t ht ↦ (hasDerivAt_T x μ ht).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ioi] at ht
    linarith [deriv_T_lower hn x μ ht]

private theorem residual_add (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    D x μ + avg (fun i ↦ 2 * μ * x i) = m₂ x + μ ^ 2 := by
  rw [D, ← avg_add]
  have h : (fun i ↦ (x i - μ) ^ 2 + 2 * μ * x i) =
      (fun i ↦ (x i) ^ 2 + μ ^ 2) := by funext i; ring
  rw [h, avg_add, avg_const hn]
  rfl

private theorem T_bracket (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {μ : ℝ} (hμ : 0 ≤ μ) :
    T x μ (D x μ) ≤ 0 ∧ 0 ≤ T x μ (m₂ x + μ ^ 2) := by
  constructor
  · unfold T
    rw [sub_self, zero_sub, neg_nonpos]
    exact div_nonneg (Finset.sum_nonneg fun i _ ↦ by have := hx i; positivity) (by positivity)
  · have hb : 0 ≤ m₂ x + μ ^ 2 := by unfold m₂ avg; positivity
    have h := avg_mono (a := fun i ↦ 4 * μ * x i /
        (1 + exp (2 * μ * x i / (m₂ x + μ ^ 2)))) (b := fun i ↦ 2 * μ * x i) (by
      intro i
      have hi := hx i
      apply (div_le_iff₀ (by positivity)).2
      have he : 1 ≤ exp (2 * μ * x i / (m₂ x + μ ^ 2)) := one_le_exp (by positivity)
      have hm := mul_le_mul_of_nonneg_left (show 2 ≤ 1 + exp
        (2 * μ * x i / (m₂ x + μ ^ 2)) by linarith) (show 0 ≤ 2 * μ * x i by positivity)
      linarith only [hm])
    unfold T
    linarith [residual_add hn x μ]

private theorem strictMaxOn_of_score {f : ℝ → ℝ} {r : ℝ} {s : Set ℝ}
    (hs : Convex ℝ s) (hc : ContinuousOn f s) (hr : r ∈ s)
    (hp : ∀ y ∈ interior s, y < r → 0 < deriv f y)
    (hm : ∀ y ∈ interior s, r < y → deriv f y < 0) :
    ∀ y ∈ s, y ≠ r → f y < f r := by
  intro y hy hne
  rcases lt_or_gt_of_ne hne with h | h
  · have hi : Icc y r ⊆ s := hs.ordConnected.out hy hr
    apply strictMonoOn_of_deriv_pos (convex_Icc y r) (hc.mono hi) _
      (left_mem_Icc.mpr h.le) (right_mem_Icc.mpr h.le) h
    intro z hz
    exact hp z (interior_mono hi hz) ((interior_Icc (a := y) (b := r)) ▸ hz).2
  · have hi : Icc r y ⊆ s := hs.ordConnected.out hr hy
    apply strictAntiOn_of_deriv_neg (convex_Icc r y) (hc.mono hi) _
      (left_mem_Icc.mpr h.le) (right_mem_Icc.mpr h.le) h
    intro z hz
    exact hm z (interior_mono hi hz) ((interior_Icc (a := r) (b := y)) ▸ hz).1

private theorem variance_root_strictMax (hn : 0 < n) {x : Fin n → ℝ} {μ r : ℝ}
    (hr : 0 < r) (hz : T x μ r = 0) :
    ∀ t > 0, t ≠ r → logLik x μ t < logLik x μ r := by
  apply strictMaxOn_of_score (s := Ioi (0 : ℝ)) (convex_Ioi _)
    (fun t ht ↦ (hasDerivAt_logLik_variance x μ ht).continuousAt.continuousWithinAt) hr
  · intro t ht htr
    rw [interior_Ioi] at ht
    have h := T_strictMono hn x μ ht hr htr
    rw [hz] at h
    rw [(hasDerivAt_logLik_variance x μ ht).deriv]
    exact div_pos (neg_pos.mpr h) (mul_pos (by norm_num) (pow_pos ht 2))
  · intro t ht hrt
    rw [interior_Ioi] at ht
    have h := T_strictMono hn x μ hr ht hrt
    rw [hz] at h
    rw [(hasDerivAt_logLik_variance x μ ht).deriv]
    exact div_neg_of_neg_of_pos (neg_neg_of_pos h) (mul_pos (by norm_num) (pow_pos ht 2))

/-- The unique variance maximizer is bracketed by the residual and second moments. -/
theorem heldLocation (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {μ : ℝ} (hμ : 0 ≤ μ) (hD : 0 < D x μ) :
    ∃ r ∈ Icc (D x μ) (m₂ x + μ ^ 2),
      0 < r ∧ T x μ r = 0 ∧
        ∀ t > 0, t ≠ r → logLik x μ t < logLik x μ r := by
  let b := m₂ x + μ ^ 2
  have hDb : D x μ ≤ b := by
    have h : 0 ≤ avg (fun i ↦ 2 * μ * x i) :=
      div_nonneg (Finset.sum_nonneg fun i _ ↦ by have := hx i; positivity) (by positivity)
    linarith [residual_add hn x μ]
  have hc : ContinuousOn (T x μ) (Icc (D x μ) b) := fun _ ht ↦
    (hasDerivAt_T x μ (hD.trans_le ht.1)).continuousAt.continuousWithinAt
  obtain ⟨r, hr, hz⟩ := intermediate_value_Icc hDb hc (T_bracket hn hx hμ)
  exact ⟨r, hr, hD.trans_le hr.1, hz, variance_root_strictMax hn (hD.trans_le hr.1) hz⟩

/-- At zero location the maximizing variance is the sample second moment. -/
theorem halfNormal (hn : 0 < n) {x : Fin n → ℝ} (hm : 0 < m₂ x) :
    ∀ t > 0, t ≠ m₂ x → logLik x 0 t < logLik x 0 (m₂ x) := by
  apply variance_root_strictMax hn hm
  simp [T, D, m₂, avg]

/-- Vanishing residual moment gives divergent likelihood at vanishing variance. -/
theorem heldLocation_unbounded {x : Fin n → ℝ} {μ : ℝ} (hD : D x μ = 0) :
    Tendsto (logLik x μ) (𝓝[>] 0) atTop := by
  have h : Tendsto (fun t : ℝ ↦ -log t / 2) (𝓝[>] 0) atTop :=
    (tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero).atTop_div_const (by norm_num)
  refine tendsto_atTop_mono (fun t ↦ ?_) h
  simp only [logLik, hD, zero_div, sub_zero]
  apply le_add_of_nonneg_right
  exact div_nonneg (Finset.sum_nonneg fun i _ ↦ log_nonneg (le_add_of_nonneg_right (exp_pos _).le))
    (by positivity)

private theorem hasDerivAt_tanh (z : ℝ) :
    HasDerivAt tanh (1 / cosh z ^ 2) z := by
  rw [show tanh = (fun u ↦ sinh u / cosh u) by funext u; exact tanh_eq_sinh_div_cosh u]
  convert (hasDerivAt_sinh z).div (hasDerivAt_cosh z) (cosh_pos z).ne' using 1
  rw [← cosh_sq_sub_sinh_sq z]
  ring

private theorem tanhRatio_continuous : Continuous (dslope tanh 0) := by
  rw [continuous_iff_continuousAt]
  intro z
  by_cases h : z = 0
  · subst z
    exact continuousAt_dslope_same.mpr (hasDerivAt_tanh 0).differentiableAt
  · exact (continuousAt_dslope_of_ne h).mpr (hasDerivAt_tanh z).continuousAt

private theorem tanhRatio_strictAnti :
    StrictAntiOn (dslope tanh 0) (Ici 0) := by
  apply strictAntiOn_of_deriv_neg (convex_Ici _)
  · exact tanhRatio_continuous.continuousOn
  · intro z hz
    rw [interior_Ici] at hz
    have hp : 0 < z := hz
    have he : dslope tanh 0 =ᶠ[𝓝 z] (fun u ↦ tanh u / u) := by
      filter_upwards [dslope_eventuallyEq_slope_of_ne tanh hp.ne'] with u hu
      simpa [slope_def_field, tanh_zero] using hu
    have h := (hasDerivAt_tanh z).div (hasDerivAt_id z) hp.ne'
    rw [he.deriv_eq]
    change deriv (tanh / id) z < 0
    rw [h.deriv]
    apply div_neg_of_neg_of_pos _ (pow_pos hp 2)
    simp only [id_eq, tanh_eq_sinh_div_cosh, mul_one]
    have hs : 2 * z < sinh (2 * z) := self_lt_sinh_iff.mpr (by positivity)
    rw [sinh_two_mul] at hs
    have hc := cosh_pos z
    have hnum : (1 / cosh z ^ 2 * z - sinh z / cosh z) * cosh z ^ 2 < 0 := by
      field_simp
      nlinarith only [hs]
    exact neg_of_mul_neg_left hnum (pow_nonneg hc.le _)

/-- The normalized location score, with its continuous extension at zero. -/
def locationScore (x : Fin n → ℝ) (t μ : ℝ) : ℝ :=
  avg (fun i ↦ (x i ^ 2 / t) * dslope tanh 0 (x i * μ / t)) - 1

private theorem locationScore_continuous (x : Fin n → ℝ) (t : ℝ) :
    Continuous (locationScore x t) := by
  unfold locationScore avg
  exact ((continuous_finsetSum _ fun i _ ↦
    (tanhRatio_continuous.comp ((continuous_const.mul continuous_id).div_const t)).const_mul
      (x i ^ 2 / t)).div_const (n : ℝ)).sub continuous_const

private theorem locationScore_zero (x : Fin n → ℝ) (t : ℝ) :
    locationScore x t 0 = m₂ x / t - 1 := by
  simp only [locationScore, mul_zero, zero_div, dslope_same, (hasDerivAt_tanh 0).deriv,
    cosh_zero, one_pow, div_one, mul_one, m₂, avg, ← Finset.sum_div]
  ring

/-- The extended score agrees with the usual normalized score at positive locations. -/
theorem locationScore_eq (x : Fin n → ℝ) {t μ : ℝ} (ht : 0 < t) (hμ : 0 < μ) :
    locationScore x t μ = avg (fun i ↦ x i * tanh (x i * μ / t)) / μ - 1 := by
  unfold locationScore
  rw [show avg (fun i ↦ x i * tanh (x i * μ / t)) / μ =
      avg (fun i ↦ (1 / μ) * (x i * tanh (x i * μ / t))) by rw [avg_mul]; ring]
  congr 2
  funext i
  by_cases hx : x i = 0
  · simp [hx]
  · rw [dslope_of_ne tanh (div_ne_zero (mul_ne_zero hx hμ.ne') ht.ne'),
      slope_def_field, tanh_zero, sub_zero, sub_zero]
    field_simp

/-- A positive observation gives strict decrease, including from the extension at zero. -/
theorem locationScore_strictAnti (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    (hpos : ∃ i, 0 < x i) {t : ℝ} (ht : 0 < t) :
    StrictAntiOn (locationScore x t) (Ici 0) := by
  intro a ha b hb hab
  have ha' : 0 ≤ a := ha
  have hb' : 0 ≤ b := hb
  have hpoint (i : Fin n) : dslope tanh 0 (x i * b / t) ≤ dslope tanh 0 (x i * a / t) :=
    tanhRatio_strictAnti.antitoneOn
      (div_nonneg (mul_nonneg (hx i) ha') ht.le)
      (div_nonneg (mul_nonneg (hx i) hb') ht.le)
      (div_le_div_of_nonneg_right (mul_le_mul_of_nonneg_left hab.le (hx i)) ht.le)
  obtain ⟨j, hj⟩ := hpos
  have hj' := tanhRatio_strictAnti
    (div_nonneg (mul_nonneg hj.le ha') ht.le)
    (div_nonneg (mul_nonneg hj.le hb') ht.le)
    (div_lt_div_of_pos_right (mul_lt_mul_of_pos_left hab hj) ht)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  unfold locationScore avg
  apply sub_lt_sub_right (div_lt_div_of_pos_right ?_ hN) 1
  apply Finset.sum_lt_sum
  · exact fun i _ ↦ mul_le_mul_of_nonneg_left (hpoint i) (by positivity)
  · exact ⟨j, Finset.mem_univ j, mul_lt_mul_of_pos_left hj' (by positivity)⟩

private theorem reflection_cosh (z : ℝ) :
    log (1 + exp (-(2 * z))) = log (2 * cosh z) - z := by
  have h : 1 + exp (-(2 * z)) = exp (-z) * (2 * cosh z) := by
    rw [cosh_eq, show 2 * ((exp z + exp (-z)) / 2) = exp z + exp (-z) by ring,
      mul_add, ← exp_add, ← exp_add, neg_add_cancel, exp_zero]
    congr 2
    ring
  rw [h, log_mul (exp_pos _).ne' (mul_pos (by norm_num) (cosh_pos z)).ne', log_exp]
  ring

private theorem logLik_cosh (hn : 0 < n) (x : Fin n → ℝ) (t μ : ℝ) :
    logLik x μ t = -log t / 2 - (m₂ x + μ ^ 2) / (2 * t) +
      avg (fun i ↦ log (2 * cosh (x i * μ / t))) := by
  have h (i : Fin n) : log (1 + exp (-(2 * μ * x i) / t)) =
      log (2 * cosh (x i * μ / t)) + (-(1 / (2 * t))) * (2 * μ * x i) := by
    rw [show -(2 * μ * x i) / t = -(2 * (x i * μ / t)) by ring, reflection_cosh]
    ring
  simp only [logLik, h, avg_add, avg_mul]
  rw [← residual_add hn x μ]
  simp only [avg_mul]
  generalize avg (fun i ↦ log (2 * cosh (x i * μ / t))) = L
  ring

private theorem hasDerivAt_locationTerm (c μ : ℝ) :
    HasDerivAt (fun a ↦ log (2 * cosh (c * a))) (c * tanh (c * μ)) μ := by
  have h := (((hasDerivAt_id μ).const_mul c).cosh.const_mul 2).log
    (mul_pos (by norm_num) (cosh_pos (c * μ))).ne'
  convert h using 1
  · rfl
  · simp only [tanh_eq_sinh_div_cosh, id_eq, mul_one]
    ring

private theorem hasDerivAt_logLik_location_raw (hn : 0 < n) (x : Fin n → ℝ)
    (t μ : ℝ) :
    HasDerivAt (fun a ↦ logLik x a t)
      ((avg (fun i ↦ x i * tanh (x i * μ / t)) - μ) / t) μ := by
  have h₁ := ((hasDerivAt_const μ (-log t / 2)).sub
    ((((hasDerivAt_id μ).pow 2).const_add (m₂ x)).div_const (2 * t)))
  have h₂ := hasDerivAt_avg (fun i ↦ hasDerivAt_locationTerm (x i / t) μ)
  convert h₁.add h₂ using 1
  · funext a
    simp only [Pi.add_apply, Pi.sub_apply, logLik_cosh hn]
    congr 2
    funext i
    rw [div_mul_eq_mul_div]
  · simp only [id_eq, mul_one, zero_sub]
    rw [show (fun i ↦ x i / t * tanh (x i / t * μ)) =
        (fun i ↦ t⁻¹ * (x i * tanh (x i * μ / t))) by
      funext i
      rw [div_mul_eq_mul_div]
      ring, avg_mul]
    ring

private theorem hasDerivAt_logLik_location (hn : 0 < n) (x : Fin n → ℝ)
    {t μ : ℝ} (ht : 0 < t) (hμ : 0 < μ) :
    HasDerivAt (fun a ↦ logLik x a t) (μ / t * locationScore x t μ) μ := by
  convert hasDerivAt_logLik_location_raw hn x t μ using 1
  rw [locationScore_eq x ht hμ]
  field_simp

private theorem locationScore_nonpos_of_zero_sample {x : Fin n → ℝ}
    (h : ∀ i, x i = 0) (t μ : ℝ) : locationScore x t μ = -1 := by
  simp [locationScore, avg, h]

private theorem locationScore_upper {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t μ : ℝ} (ht : 0 < t) (hμ : 0 < μ) :
    locationScore x t μ ≤ avg x / μ - 1 := by
  rw [locationScore_eq x ht hμ]
  apply sub_le_sub_right (div_le_div_of_nonneg_right (avg_mono ?_) hμ.le) _
  intro i
  have h : tanh (x i * μ / t) ≤ 1 := by
    rw [tanh_eq_sinh_div_cosh]
    exact (div_le_iff₀ (cosh_pos _)).2 (by simpa using (sinh_lt_cosh (x i * μ / t)).le)
  simpa using mul_le_mul_of_nonneg_left h (hx i)

/-- If the second moment is at most the variance, zero strictly beats every positive location. -/
theorem heldScale_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) (hm : m₂ x ≤ t) :
    ∀ μ > 0, logLik x μ t < logLik x 0 t := by
  have h := strictAntiOn_of_deriv_neg (convex_Ici (0 : ℝ))
    (f := fun a ↦ logLik x a t)
    (fun a _ ↦ (hasDerivAt_logLik_location_raw hn x t a).continuousAt.continuousWithinAt) (by
      intro a ha
      rw [interior_Ici] at ha
      have ha' : 0 < a := ha
      rw [(hasDerivAt_logLik_location hn x ht ha').deriv]
      apply mul_neg_of_pos_of_neg (div_pos ha' ht)
      by_cases hp : ∃ i, 0 < x i
      · have hs := locationScore_strictAnti hn hx hp ht (show (0 : ℝ) ∈ Ici 0 by simp) ha'.le ha'
        rw [locationScore_zero] at hs
        have hb := (div_le_iff₀ ht).2 (show m₂ x ≤ 1 * t by simpa using hm)
        linarith
      · have hz i : x i = 0 := le_antisymm (le_of_not_gt fun hi ↦ hp ⟨i, hi⟩) (hx i)
        rw [locationScore_nonpos_of_zero_sample hz]
        norm_num)
  exact fun μ hμ ↦ h (show (0 : ℝ) ∈ Ici 0 by simp) (le_of_lt hμ) hμ

/-- Above the second-moment threshold there is a unique positive score root and location maximum. -/
theorem heldScale_positive (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) (hm : t < m₂ x) :
    ∃ r > 0, locationScore x t r = 0 ∧
      (∀ μ > 0, locationScore x t μ = 0 → μ = r) ∧
      ∀ μ ≥ 0, μ ≠ r → logLik x μ t < logLik x r t := by
  have hp : ∃ i, 0 < x i := by
    by_contra! h
    have hz i : x i = 0 := le_antisymm (h i) (hx i)
    have hz₂ : m₂ x = 0 := by simp [m₂, avg, hz]
    exact ht.not_gt (hz₂ ▸ hm)
  have hAnti := locationScore_strictAnti hn hx hp ht
  let b := avg x + 1
  have hb : 0 < b := by
    have h : 0 ≤ avg x := div_nonneg (Finset.sum_nonneg fun i _ ↦ hx i) (by positivity)
    dsimp [b]
    linarith
  have h0 : 0 < locationScore x t 0 := by
    rw [locationScore_zero, sub_pos]
    exact (lt_div_iff₀ ht).2 (by simpa using hm)
  have hTop : locationScore x t b < 0 := by
    refine (locationScore_upper hx ht hb).trans_lt ?_
    rw [sub_neg, div_lt_iff₀ hb]
    dsimp [b]
    linarith
  obtain ⟨r, hr, hroot⟩ := intermediate_value_Icc' hb.le
    (locationScore_continuous x t).continuousOn ⟨hTop.le, h0.le⟩
  have hr' : 0 < r := lt_of_le_of_ne hr.1 (by
    intro h
    subst r
    exact h0.ne' hroot)
  refine ⟨r, hr', hroot, fun μ hμ hzero ↦ ?_, ?_⟩
  · exact hAnti.injOn hμ.le hr'.le (hzero.trans hroot.symm)
  · apply strictMaxOn_of_score (s := Ici 0) (convex_Ici _)
      (fun a _ ↦ (hasDerivAt_logLik_location_raw hn x t a).continuousAt.continuousWithinAt) hr'.le
    · intro μ hμ hlt
      rw [interior_Ici] at hμ
      have hμ' : 0 < μ := hμ
      have hs := hAnti hμ'.le hr'.le hlt
      rw [hroot] at hs
      rw [(hasDerivAt_logLik_location hn x ht hμ').deriv]
      exact mul_pos (div_pos hμ' ht) hs
    · intro μ hμ hlt
      rw [interior_Ici] at hμ
      have hμ' : 0 < μ := hμ
      have hs := hAnti hr'.le hμ'.le hlt
      rw [hroot] at hs
      rw [(hasDerivAt_logLik_location hn x ht hμ').deriv]
      exact mul_neg_of_pos_of_neg (div_pos hμ' ht) hs

/-- Zero is a maximizing location exactly when the second moment is at most the variance. -/
theorem heldScale_zero_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 ≤ x i)
    {t : ℝ} (ht : 0 < t) :
    IsMaxOn (fun μ ↦ logLik x μ t) (Ici 0) 0 ↔ m₂ x ≤ t := by
  constructor
  · intro h
    by_contra! hm
    obtain ⟨r, hr, _, _, hmax⟩ := heldScale_positive hn hx ht hm
    exact (not_lt_of_ge (h hr.le)) (hmax 0 le_rfl hr.ne)
  · intro hm μ hμ
    have h : 0 ≤ μ := hμ
    rcases h.eq_or_lt with he | hp
    · rw [← he]
      exact le_rfl
    · exact (heldScale_zero hn hx ht hm μ hp).le

end

end UnivariateML.FoldedNormal
