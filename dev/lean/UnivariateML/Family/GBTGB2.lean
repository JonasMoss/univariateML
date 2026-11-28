module

public import UnivariateML.Family.Endpoint3GB2
public import UnivariateML.Generic.GBTLogConcave
public import UnivariateML.Generic.Endpoint3GammaRatio
public import Mathlib.Analysis.SpecialFunctions.Sigmoid
public import Mathlib.Analysis.SpecialFunctions.ImproperIntegrals
public import Mathlib.MeasureTheory.Function.JacobianOneDim

set_option linter.style.header false

/-! Normalization, superlevel bounds and boundary traces for the GB2 log chart.

## Deviations

The peak, mode and tail-rate bounds apply jointly to all parameters. The
trace theorems identify specified limits. Exhaustion of the full family of
escaping sequences remains open.
-/

@[expose] public section

open Real Set MeasureTheory Filter Topology
namespace UnivariateML.GBTGB2

private theorem loss_reflect (p q z : ℝ) :
    Shape2BetaPrime.logisticLoss q p (-z) = Shape2BetaPrime.logisticLoss p q z := by
  have he : 1 + exp (-z) = (1 + exp z) * exp (-z) := by
    rw [add_mul, one_mul, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  unfold Shape2BetaPrime.logisticLoss
  rw [he, log_mul (by positivity) (exp_pos _).ne', log_exp]
  ring

/-- Reciprocal data and scale swap the beta shapes, with the exact
sample-only Jacobian offset. This identity is pointwise in the parameters. -/
theorem reciprocal_logLik {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (b p q : ℝ) :
    Shape2GB2.logLik (fun i ↦ (x i)⁻¹) s⁻¹ b (q, p) =
      Shape2GB2.logLik x s b (p, q) + ∑ i, 2 * log (x i) := by
  have hbeta : Beta.logBeta (q, p) = Beta.logBeta (p, q) := by
    simp [Beta.logBeta, Beta.normalizer, mul_comm, add_comm]
  unfold Shape2GB2.logLik
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [hbeta, log_inv]
  rw [log_div (inv_ne_zero (hx i).ne') (inv_ne_zero hs.ne'), log_inv, log_inv,
    log_div (hx i).ne' hs.ne',
    show b * (-log (x i) - -log s) = -(b * (log (x i) - log s)) by ring, loss_reflect]
  ring

noncomputable section

/-- The log density on the log-observation coordinate, with log scale
`v`, positive power `a`, and lower and upper beta shapes `p,q`. -/
def logDensity (v a p q y : ℝ) : ℝ :=
  let u := sigmoid (a * (y - v))
  log a - Beta.logBeta (p, q) + p * log u + q * log (1 - u)

private theorem logDensity_deriv (v a p q y : ℝ) :
    HasDerivAt (logDensity v a p q)
      (a * (p - (p + q) * sigmoid (a * (y - v)))) y := by
  let u (t : ℝ) := sigmoid (a * (t - v))
  have hu : HasDerivAt u (u y * (1 - u y) * a) y :=
    (hasDerivAt_sigmoid _).comp y (((hasDerivAt_id y).sub_const v).const_mul a)
    |>.congr_deriv (by change u y * (1 - u y) * (a * 1) = _; rw [mul_one])
  have he := (((hu.log (sigmoid_pos _).ne').const_mul p).const_add
    (log a - Beta.logBeta (p, q))).add
    (((hu.const_sub 1).log (sub_pos.mpr (sigmoid_lt_one _)).ne').const_mul q)
  convert he using 1
  · rfl
  · change a * (p - (p + q) * u y) =
      p * (u y * (1 - u y) * a / u y) + q * (-(u y * (1 - u y) * a) / (1 - u y))
    have h₁ : 0 < u y := sigmoid_pos _
    have h₂ : 0 < 1 - u y := sub_pos.mpr (sigmoid_lt_one _)
    field_simp [h₁.ne', h₂.ne']
    ring

private theorem logDensity_concave (v : ℝ) {a p q : ℝ}
    (ha : 0 < a) (hp : 0 < p) (hq : 0 < q) : ConcaveOn ℝ univ (logDensity v a p q) := by
  apply Antitone.concaveOn_univ_of_deriv
    (fun y ↦ (logDensity_deriv v a p q y).differentiableAt)
  intro x y hxy
  rw [(logDensity_deriv v a p q x).deriv, (logDensity_deriv v a p q y).deriv]
  gcongr

private theorem mode_max (v : ℝ) {a p q : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q) :
    ∀ y, logDensity v a p q y ≤ logDensity v a p q (v + log (p / q) / a) := by
  let m := v + log (p / q) / a
  have hm : sigmoid (a * (m - v)) = p / (p + q) := by
    have he : a * (m - v) = log (p / q) := by dsimp [m]; field_simp; ring
    rw [he, sigmoid_def, ← log_inv, exp_log (inv_pos.mpr (div_pos hp hq))]
    field_simp
  have hzero : deriv (logDensity v a p q) m = 0 := by
    rw [(logDensity_deriv v a p q m).deriv, hm, mul_div_cancel₀ _ (add_pos hp hq).ne', sub_self,
      mul_zero]
  have h := logDensity_concave v ha hp hq
  have hd := (logDensity_deriv v a p q m).differentiableAt
  intro y
  rcases lt_trichotomy y m with hy | rfl | hy
  · have hh := h.deriv_le_slope (mem_univ y) (mem_univ m) hy hd
    rw [hzero, slope_def_field] at hh
    have hsign := (le_div_iff₀ (sub_pos.mpr hy)).mp hh
    exact sub_nonneg.mp (by simpa using hsign)
  · exact le_rfl
  · have hh := h.slope_le_deriv (mem_univ m) (mem_univ y) hy hd
    rw [hzero, slope_def_field] at hh
    have hsign := (div_le_iff₀ (sub_pos.mpr hy)).mp hh
    exact sub_nonpos.mp (by simpa using hsign)

private theorem betaKernel_normalized {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let K := fun u : ℝ ↦ exp ((p - 1) * log u + (q - 1) * log (1 - u))
    IntegrableOn K (Ioo 0 1) ∧ ∫ u in Ioo (0 : ℝ) 1, K u = Beta.normalizer (p, q) := by
  intro K
  have he {z : ℝ} (hz : z ∈ Ioo (0 : ℝ) 1) :
      (K z : ℂ) = (z : ℂ) ^ ((p : ℂ) - 1) * (1 - (z : ℂ)) ^ ((q : ℂ) - 1) := by
    have h₁ : 0 < z := hz.1
    have h₂ : 0 < 1 - z := sub_pos.mpr hz.2
    dsimp [K]
    rw [exp_add, mul_comm (p - 1), mul_comm (q - 1),
      ← rpow_def_of_pos h₁, ← rpow_def_of_pos h₂, Complex.ofReal_mul,
      Complex.ofReal_cpow h₁.le, Complex.ofReal_cpow h₂.le]
    push_cast
    ring
  constructor
  · have hi := (Complex.betaIntegral_convergent
      (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq)).1
    exact (Integrable.iff_ofReal (𝕜 := ℂ)).mpr
      ((hi.mono_set Ioo_subset_Ioc_self).congr
        ((ae_restrict_mem measurableSet_Ioo).mono fun _ hz ↦ (he hz).symm))
  · apply Complex.ofReal_injective
    rw [← integral_complex_ofReal, ← integral_Ioc_eq_integral_Ioo,
      ← intervalIntegral.integral_of_le zero_le_one]
    trans Complex.betaIntegral p q
    · exact intervalIntegral.integral_congr_Ioo_of_le zero_le_one (fun _ hz ↦ he hz)
    · rw [Complex.betaIntegral_eq_Gamma_mul_div _ _
        (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq), ← Complex.ofReal_add]
      simp only [Beta.normalizer, Complex.ofReal_div,
        Complex.ofReal_mul, Complex.Gamma_ofReal]

private theorem density_normalized (v : ℝ) {a p q : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q) :
    Integrable (fun y ↦ exp (logDensity v a p q y)) volume ∧
      ∫ y, exp (logDensity v a p q y) = 1 := by
  let u (y : ℝ) := sigmoid (a * (y - v))
  let d (y : ℝ) := u y * (1 - u y) * a
  let B := Beta.normalizer (p, q)
  let K (z : ℝ) := exp ((p - 1) * log z + (q - 1) * log (1 - z))
  have hB : 0 < B := div_pos (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq))
    (Gamma_pos_of_pos (add_pos hp hq))
  have hder (y : ℝ) : HasDerivWithinAt u (d y) univ y := by
    apply HasDerivAt.hasDerivWithinAt
    exact (hasDerivAt_sigmoid _).comp y (((hasDerivAt_id y).sub_const v).const_mul a)
      |>.congr_deriv (by dsimp [d]; ring)
  have hu : InjOn u univ := fun x _ y _ h ↦ by
    have he := mul_left_cancel₀ ha.ne' (sigmoid_injective h)
    linarith only [he]
  have hu_range : u '' univ = Ioo 0 1 := by
    rw [← range_sigmoid]
    ext z
    constructor
    · rintro ⟨y, _, hy⟩
      exact ⟨a * (y - v), hy⟩
    · rintro ⟨t, rfl⟩
      refine ⟨v + t / a, mem_univ _, congrArg sigmoid ?_⟩
      field_simp
      ring
  have hid (y : ℝ) : |d y| • (B⁻¹ * K (u y)) = exp (logDensity v a p q y) := by
    have h₁ : 0 < u y := sigmoid_pos _
    have h₂ : 0 < 1 - u y := sub_pos.mpr (sigmoid_lt_one _)
    have hd : 0 < d y := mul_pos (mul_pos h₁ h₂) ha
    rw [abs_of_pos hd, smul_eq_mul, logDensity]
    change d y * (B⁻¹ * exp ((p - 1) * log (u y) + (q - 1) * log (1 - u y))) =
      exp (log a - log B + p * log (u y) + q * log (1 - u y))
    rw [show log a - log B + p * log (u y) + q * log (1 - u y) =
      (log (u y) + log (1 - u y) + log a - log B) +
        ((p - 1) * log (u y) + (q - 1) * log (1 - u y)) by ring]
    conv_rhs =>
      rw [exp_add, exp_sub, exp_add, exp_add, exp_log h₁, exp_log h₂, exp_log ha, exp_log hB]
    dsimp [d]
    ring
  have hchange := integrableOn_image_iff_integrableOn_abs_deriv_smul
    MeasurableSet.univ (fun y _ ↦ hder y) hu (fun z ↦ B⁻¹ * K z)
  rw [hu_range] at hchange
  have hI := hchange.mp ((betaKernel_normalized hp hq).1.const_mul B⁻¹)
  simp only [hid, integrableOn_univ] at hI
  refine ⟨hI, ?_⟩
  have h := integral_image_eq_integral_abs_deriv_smul
    MeasurableSet.univ (fun y _ ↦ hder y) hu (fun z ↦ B⁻¹ * K z)
  rw [hu_range] at h
  simp only [hid, Measure.restrict_univ] at h
  rw [← h, integral_const_mul, (betaKernel_normalized hp hq).2, inv_mul_cancel₀ hB.ne']

/-- On every nonconstant log-sample superlevel, GB2 has bounded mode
and a peak bounded above and away from zero, uniformly in all parameters. -/
theorem log_sample_superlevel {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hne : ∃ i j, y i ≠ y j) (c : ℝ) :
    ∃ B M : ℝ, ∀ (v : ℝ) {a p q : ℝ}, 0 < a → 0 < p → 0 < q →
      c ≤ (∑ i, logDensity v a p q (y i)) / n →
      let m := v + log (p / q) / a
      exp c ≤ exp (logDensity v a p q m) ∧
        exp (logDensity v a p q m) ≤ B ∧ |m| ≤ M := by
  obtain ⟨B, M, h⟩ := GBTLogConcave.sample_superlevel hn y hne c
  refine ⟨B, M, fun v a p q ha hp hq hc ↦ ?_⟩
  exact h (logDensity_concave v ha hp hq) (mode_max v ha hp hq)
    (density_normalized v ha hp hq).1 (density_normalized v ha hp hq).2 hc

/-- Passing to log observations changes the GB2 sample likelihood only
by the log Jacobian, independently of the parameters. -/
theorem logLik_log_chart {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (a p q : ℝ) :
    Shape2GB2.logLik x s a (p, q) + ∑ i, log (x i) =
      ∑ i, logDensity (log s) a p q (log (x i)) := by
  have hsig (z : ℝ) : log (sigmoid z) = z - log (1 + exp z) := by
    have h : sigmoid z = exp z / (1 + exp z) := by
      rw [sigmoid_def, exp_neg]
      field_simp [show 1 + exp z ≠ 0 by positivity, (exp_pos z).ne']
      exact add_comm _ _
    rw [h, log_div (exp_pos _).ne' (by positivity), log_exp]
  have hneg (z : ℝ) : log (1 - sigmoid z) = -log (1 + exp z) := by
    rw [← sigmoid_neg, sigmoid_def, neg_neg, log_inv]
  rw [Shape2GB2.logLik, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, hsig, hneg, Shape2BetaPrime.logisticLoss,
    log_div (hx i).ne' hs.ne']
  ring

/-- The peak is bounded by the harmonic combination of the two tail rates.
Consequently both rates stay away from zero on any positive superlevel. -/
theorem peak_rate_bound (v m : ℝ) {a p q : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q) :
    exp (logDensity v a p q m) * ((a * p)⁻¹ + (a * q)⁻¹) ≤ 1 := by
  let f := logDensity v a p q
  let A := a * p
  let D := -(a * q)
  have hA : 0 < A := mul_pos ha hp
  have hD : D < 0 := neg_neg_of_pos (mul_pos ha hq)
  have hconc := logDensity_concave v ha hp hq
  have hder (y : ℝ) := logDensity_deriv v a p q y
  have hslope (y : ℝ) : D ≤ deriv f y ∧ deriv f y ≤ A := by
    rw [(hder y).deriv]
    have h₁ := mul_nonneg (add_pos hp hq).le (sigmoid_nonneg (a * (y - v)))
    have h₂ := mul_le_mul_of_nonneg_left (sigmoid_le_one (a * (y - v))) (add_pos hp hq).le
    dsimp [A, D]
    constructor <;> nlinarith only [mul_nonneg ha.le h₁, mul_le_mul_of_nonneg_left h₂ ha.le]
  have hleft (y : ℝ) (hy : y ∈ Iic m) : f m + A * (y - m) ≤ f y := by
    rcases lt_or_eq_of_le hy with h | rfl
    · have h₁ := hconc.slope_le_deriv (mem_univ y) (mem_univ m) h (hder y).differentiableAt
      rw [slope_def_field] at h₁
      have h₂ := (div_le_iff₀ (sub_pos.mpr h)).mp (h₁.trans (hslope y).2)
      linarith only [h₂]
    · simp
  have hright (y : ℝ) (hy : y ∈ Ioi m) : f m + D * (y - m) ≤ f y := by
    have h₁ := hconc.deriv_le_slope (mem_univ m) (mem_univ y) hy (hder y).differentiableAt
    rw [slope_def_field] at h₁
    linarith only [(le_div_iff₀ (sub_pos.mpr hy)).mp ((hslope y).1.trans h₁)]
  have hI := (density_normalized v ha hp hq).1
  have he (r y : ℝ) : exp (f m + r * (y - m)) = exp (f m - r * m) * exp (r * y) := by
    rw [← exp_add]
    congr 1
    ring
  have hL := setIntegral_mono_on
    ((integrableOn_exp_mul_Iic hA m).const_mul (exp (f m - A * m)))
    hI.integrableOn measurableSet_Iic
    (fun y hy ↦ by rw [← he]; exact exp_le_exp.mpr (hleft y hy))
  have hR := setIntegral_mono_on
    ((integrableOn_exp_mul_Ioi hD m).const_mul (exp (f m - D * m)))
    hI.integrableOn measurableSet_Ioi
    (fun y hy ↦ by rw [← he]; exact exp_le_exp.mpr (hright y hy))
  rw [integral_const_mul, integral_exp_mul_Iic hA] at hL
  rw [integral_const_mul, integral_exp_mul_Ioi hD] at hR
  have hm := add_le_add hR hL
  rw [← compl_Ioi, integral_add_compl measurableSet_Ioi hI, (density_normalized v ha hp hq).2]
    at hm
  have hl (r : ℝ) : exp (f m - r * m) * exp (r * m) = exp (f m) := by
    rw [← exp_add, sub_add_cancel]
  rw [← mul_div_assoc, ← mul_div_assoc, mul_neg, hl D, hl A, neg_div, ← div_neg,
    show -D = a * q from neg_neg _] at hm
  rw [mul_add, ← div_eq_mul_inv, ← div_eq_mul_inv, add_comm]
  exact hm

/-- Both tail rates on a log-likelihood superlevel are at least the
exponential of its mean height. Thus either fixed beta shape excludes
arbitrarily small power even when the other shape and scale can move. -/
theorem superlevel_tail_rates {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    {v a p q c : ℝ} (ha : 0 < a) (hp : 0 < p) (hq : 0 < q)
    (hlevel : c ≤ (∑ i, logDensity v a p q (y i)) / n) :
    exp c ≤ a * p ∧ exp c ≤ a * q := by
  let m := v + log (p / q) / a
  let H := exp (logDensity v a p q m)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hmax := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ mode_max v ha hp hq (y i))
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hmax
  have hc : exp c ≤ H := exp_le_exp.mpr
    ((mul_le_mul_iff_right₀ hn').mp (by
      simpa only [mul_comm] using ((le_div_iff₀ hn').mp hlevel).trans hmax))
  have h := peak_rate_bound v m ha hp hq
  change H * ((a * p)⁻¹ + (a * q)⁻¹) ≤ 1 at h
  rw [mul_add] at h
  have hp' := mul_pos ha hp
  have hq' := mul_pos ha hq
  constructor
  · apply hc.trans
    have h₁ : H / (a * p) ≤ 1 := by
      rw [div_eq_mul_inv]
      have hH : 0 ≤ H := (exp_pos _).le
      linarith only [h, mul_nonneg hH (inv_pos.mpr hq').le]
    simpa only [one_mul] using (div_le_iff₀ hp').mp h₁
  · apply hc.trans
    have h₁ : H / (a * q) ≤ 1 := by
      rw [div_eq_mul_inv]
      have hH : 0 ≤ H := (exp_pos _).le
      linarith only [h, mul_nonneg hH (inv_pos.mpr hp').le]
    simpa only [one_mul] using (div_le_iff₀ hq').mp h₁

/-- At held lower beta shape, the moving log scale below the cutoff
realizes the full right-exponential trace at every observation,
including any number of exact cutoff ties. -/
theorem pareto_cutoff_path {n : ℕ} (y : Fin n → ℝ) {p μ D : ℝ}
    (hp : 0 < p) (hD : 0 < D) (hy : ∀ i, μ ≤ y i) :
    Tendsto (fun a : ℝ ↦
      ∑ i, logDensity (μ - log a / a) a p (D / a) (y i)) atTop
      (𝓝 (∑ i, (log D - D * (y i - μ)))) := by
  let q (a : ℝ) := D / a
  let v (a : ℝ) := μ - log a / a
  let z (a : ℝ) (i : Fin n) := a * (y i - v a)
  have hq : Tendsto q atTop (𝓝 0) := by
    simpa [q, div_eq_mul_inv] using tendsto_inv_atTop_zero.const_mul D
  have hv : Tendsto v atTop (𝓝 μ) := by
    simpa [v] using (show Tendsto (fun a : ℝ ↦ log a / a) atTop (𝓝 0) by
      simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero).const_sub μ
  have hq0 : Tendsto q atTop (𝓝[>] 0) := by
    rw [nhdsWithin, tendsto_inf]
    exact ⟨hq, tendsto_principal.mpr
      ((eventually_gt_atTop (0 : ℝ)).mono (fun a ha ↦ div_pos hD ha))⟩
  have hnorm : Tendsto (fun a ↦ log a - Beta.logBeta (p, q a)) atTop (𝓝 (log D)) := by
    have h := ((Endpoint3GammaRatio.log_gamma_ratio_at_zero hp).comp hq0).const_add (log D)
    simp only [Function.comp_def, add_zero] at h
    apply h.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    have hqa : 0 < q a := div_pos hD ha
    rw [Beta.logBeta, Beta.normalizer,
      log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hqa)).ne'
        (Gamma_pos_of_pos (add_pos hp hqa)).ne',
      log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hqa).ne',
      show log (q a) = log D - log a from log_div hD.ne' ha.ne']
    ring
  have hpen (i : Fin n) : Tendsto (fun a ↦ log (1 + exp (-z a i))) atTop (𝓝 0) := by
    have he : Tendsto (fun a ↦ exp (-z a i)) atTop (𝓝 0) := by
      apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds tendsto_inv_atTop_zero
      · exact Eventually.of_forall (fun a ↦ (exp_pos _).le)
      · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
        have hz : log a ≤ z a i := by
          dsimp [z, v]
          rw [show a * (y i - (μ - log a / a)) = a * (y i - μ) + log a by
            field_simp; ring]
          linarith only [mul_nonneg ha.le (sub_nonneg.mpr (hy i))]
        rw [show a⁻¹ = exp (-log a) by rw [exp_neg, exp_log ha]]
        exact exp_le_exp.mpr (neg_le_neg hz)
    simpa using (he.const_add 1).log (by norm_num)
  have hpoint (i : Fin n) := (hnorm.sub ((hv.const_sub (y i)).const_mul D)).sub
    ((hq.const_add p).mul (hpen i))
  refine (tendsto_finsetSum Finset.univ fun i _ ↦ ?_)
  apply (show Tendsto (fun a ↦ log a - Beta.logBeta (p, q a) - D * (y i - v a) -
    (p + q a) * log (1 + exp (-z a i))) atTop (𝓝 (log D - D * (y i - μ))) by
      simpa using hpoint i).congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
  have hz : 1 + exp (-z a i) = (1 + exp (z a i)) * exp (-z a i) := by
    rw [add_mul, one_mul, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  have hb : q a * z a i = D * (y i - v a) := by
    dsimp [q, z]
    field_simp
  have hs : log (1 - sigmoid (z a i)) = -(z a i) - log (1 + exp (-z a i)) := by
    rw [← sigmoid_neg, sigmoid_def, neg_neg, log_inv, hz,
      log_mul (by positivity) (exp_pos _).ne', log_exp]
    ring
  unfold logDensity
  change _ = log a - Beta.logBeta (p, q a) + p * log (sigmoid (z a i)) +
    q a * log (1 - sigmoid (z a i))
  rw [hs, sigmoid_def, log_inv, ← hb]
  ring

private theorem logDensity_reflect (v a p q y : ℝ) :
    logDensity (-v) a q p (-y) = logDensity v a p q y := by
  have hbeta : Beta.logBeta (q, p) = Beta.logBeta (p, q) := by
    simp [Beta.logBeta, Beta.normalizer, mul_comm, add_comm]
  unfold logDensity
  rw [show a * (-y - -v) = -(a * (y - v)) by ring, sigmoid_neg, hbeta]
  ring_nf

/-- At held upper beta shape, the reflected moving log scale realizes
the full left-exponential trace, including exact ties at the upper cutoff. -/
theorem power_cutoff_path {n : ℕ} (y : Fin n → ℝ) {q μ A : ℝ}
    (hq : 0 < q) (hA : 0 < A) (hy : ∀ i, y i ≤ μ) :
    Tendsto (fun a : ℝ ↦
      ∑ i, logDensity (μ + log a / a) a (A / a) q (y i)) atTop
      (𝓝 (∑ i, (log A - A * (μ - y i)))) := by
  have h := pareto_cutoff_path (fun i ↦ -y i) hq hA (fun i ↦ neg_le_neg (hy i))
  simp only [neg_sub_neg] at h
  refine h.congr (fun a ↦ ?_)
  apply Finset.sum_congr rfl
  intro i _
  rw [show -μ - log a / a = -(μ + log a / a) by ring, logDensity_reflect]

/-- At held lower shape the bounded-power exterior with diverging upper
shape has the transformed-gamma trace, allowing both power and mode to move. -/
theorem held_lower_gamma_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    (y : Fin n → ℝ) {p a₀ m₀ : ℝ} (hp : 0 < p) (ha₀ : 0 < a₀)
    {a q m : ι → ℝ} (ha : Tendsto a l (𝓝 a₀))
    (hq : Tendsto q l atTop) (hm : Tendsto m l (𝓝 m₀)) :
    Tendsto (fun j ↦ ∑ i, logDensity (m j - log (p / q j) / a j) (a j) p (q j) (y i)) l
      (𝓝 (∑ i, (log a₀ + p * log p - log (Real.Gamma p) +
        p * a₀ * (y i - m₀) - p * exp (a₀ * (y i - m₀))))) := by
  let v (j : ι) := m j - log (p / q j) / a j
  let W (i : Fin n) (j : ι) := p * exp (a j * (y i - m j))
  let u (i : Fin n) (j : ι) := W i j / q j
  have hpos := (ha.eventually (Ioi_mem_nhds ha₀)).and
    (hq.eventually (eventually_gt_atTop (0 : ℝ)))
  have hlog1p : Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
    have hb : HasDerivAt (fun t : ℝ ↦ 1 + t) 1 0 := (hasDerivAt_id _).const_add _
    have hl : HasDerivAt log (1 : ℝ) (1 + (0 : ℝ)) := by
      simpa using hasDerivAt_log one_ne_zero
    have h := (hl.comp 0 hb).tendsto_slope_zero
    simpa [add_comm, div_eq_mul_inv, mul_comm] using h
  have hn (j : ι) (hqj : 0 < q j) :
      -Beta.logBeta (p, q j) + p * log (p / q j) =
        (log (Real.Gamma (q j + p)) - log (Real.Gamma (q j)) - p * log (q j)) +
        p * log p - log (Real.Gamma p) := by
    rw [Beta.logBeta, Beta.normalizer, add_comm p (q j),
      log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hqj)).ne'
        (Gamma_pos_of_pos (add_pos hqj hp)).ne',
      log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hqj).ne',
      log_div hp.ne' hqj.ne']
    ring
  have hnorm := ((Endpoint3GammaRatio.log_gamma_ratio p hp).comp hq).add_const
    (p * log p - log (Real.Gamma p))
  apply tendsto_finsetSum Finset.univ
  intro i _
  have hδ := ha.mul (hm.const_sub (y i))
  have hW := (Real.continuous_exp.continuousAt.tendsto.comp hδ).const_mul p
  have hqinv := tendsto_inv_atTop_zero.comp hq
  have hu : Tendsto (u i) l (𝓝 0) := by
    simpa [u, div_eq_mul_inv, Function.comp_def] using hW.mul hqinv
  have hu0 : Tendsto (u i) l (𝓝[≠] 0) := by
    rw [nhdsWithin, tendsto_inf]
    refine ⟨hu, tendsto_principal.mpr ?_⟩
    filter_upwards [hpos] with j hj
    exact (div_pos (mul_pos hp (exp_pos _)) hj.2).ne'
  have hC := (hu.const_mul p).add hW
  have hpen := hC.mul (hlog1p.comp hu0)
  have h := (((ha.log ha₀.ne').add hnorm).add (hδ.const_mul p)).sub hpen
  simp only [mul_zero, zero_add, mul_one, Function.comp_def] at h
  rw [show log a₀ + (p * log p - log (Real.Gamma p)) + p * (a₀ * (y i - m₀)) -
    p * exp (a₀ * (y i - m₀)) = log a₀ + p * log p - log (Real.Gamma p) +
    p * a₀ * (y i - m₀) - p * exp (a₀ * (y i - m₀)) by ring] at h
  apply h.congr'
  filter_upwards [hpos] with j hj
  have ha' : 0 < a j := hj.1
  have hq' : 0 < q j := hj.2
  let z := a j * (y i - v j)
  have hz : z = a j * (y i - m j) + log (p / q j) := by
    dsimp [z, v]
    field_simp
    ring
  have hexp : exp z = u i j := by
    rw [hz, exp_add, exp_log (div_pos hp hq')]
    dsimp [u, W]
    ring
  have hsig : log (sigmoid z) = z - log (1 + exp z) := by
    have he : sigmoid z = exp z / (1 + exp z) := by
      rw [sigmoid_def, exp_neg]
      field_simp [show 1 + exp z ≠ 0 by positivity, (exp_pos z).ne']
      exact add_comm _ _
    rw [he, log_div (exp_pos _).ne' (by positivity), log_exp]
  have hneg : log (1 - sigmoid z) = -log (1 + exp z) := by
    rw [← sigmoid_neg, sigmoid_def, neg_neg, log_inv]
  change _ = log (a j) - Beta.logBeta (p, q j) +
    p * log (sigmoid z) + q j * log (1 - sigmoid z)
  rw [hsig, hneg, hexp, hz]
  have hu' : u i j ≠ 0 := (div_pos (mul_pos hp (exp_pos _)) hq').ne'
  have hqu : q j * u i j = W i j := by
    dsimp [u]
    field_simp
  change q j * u i j = p * exp (a j * (y i - m j)) at hqu
  rw [← hqu]
  have he := hn j hq'
  rw [show log (Real.Gamma (q j + p)) - log (Real.Gamma (q j)) - p * log (q j) +
    (p * log p - log (Real.Gamma p)) = -Beta.logBeta (p, q j) + p * log (p / q j) by
      linarith only [he]]
  field_simp [hu']
  ring

/-- The reciprocal bounded-power exterior at held upper shape has
the inverse transformed-gamma trace, with a moving mode and power. -/
theorem held_upper_gamma_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    (y : Fin n → ℝ) {q a₀ m₀ : ℝ} (hq : 0 < q) (ha₀ : 0 < a₀)
    {a p m : ι → ℝ} (ha : Tendsto a l (𝓝 a₀))
    (hp : Tendsto p l atTop) (hm : Tendsto m l (𝓝 m₀)) :
    Tendsto (fun j ↦ ∑ i, logDensity (m j - log (p j / q) / a j) (a j) (p j) q (y i)) l
      (𝓝 (∑ i, (log a₀ + q * log q - log (Real.Gamma q) +
        q * a₀ * (m₀ - y i) - q * exp (a₀ * (m₀ - y i))))) := by
  have h := held_lower_gamma_trace (fun i ↦ -y i) hq ha₀ ha hp hm.neg
  simp only [neg_sub_neg] at h
  apply h.congr'
  filter_upwards [hp.eventually (eventually_gt_atTop (0 : ℝ))] with j hj
  apply Finset.sum_congr rfl
  intro i _
  rw [show -m j - log (q / p j) / a j = -(m j - log (p j / q) / a j) by
    rw [log_div hq.ne' hj.ne', log_div hj.ne' hq.ne']
    ring, logDensity_reflect]

end

end UnivariateML.GBTGB2
end
