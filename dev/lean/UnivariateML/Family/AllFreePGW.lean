module

public import UnivariateML.Family.ShapeFacesPGW

set_option linter.style.header false

/-! The fixed-scale threshold path for power generalized Weibull. -/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.AllFreePGW
noncomputable section

variable {n : ℕ}

def thresholdLogLik (x : Fin n → ℝ) (a k : ℝ) : ℝ :=
  ∑ i, (log k - log (x i) - (if x i = a then log 2 else 0) +
    k * log (x i / a) - (exp (k * log (x i / a)) - 1))

private def correction (z : ℝ) : ℝ := log (1 + exp (-z))

private theorem softplus_split (z : ℝ) :
    log (1 + exp z) = z + correction z := by
  unfold correction
  have h : exp z * (1 + exp (-z)) = 1 + exp z := by
    rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  rw [← h, log_mul (exp_pos _).ne' (by positivity), log_exp]

private theorem correction_bound {z : ℝ} (hz : 0 ≤ z) :
    0 < correction z ∧ correction z ≤ log 2 := by
  unfold correction
  refine ⟨log_pos (by linarith [exp_pos (-z)]), log_le_log (by positivity) ?_⟩
  linarith [exp_le_one_iff.mpr (neg_nonpos.mpr hz)]

private theorem corrected_density {a x b k : ℝ} (ha : 0 < a) (hx : a ≤ x)
    (hb : 0 < b) (hk : 0 < k) :
    ShapeFacesPGW.logDensity b a (k / b) x =
      log k - log x - correction (b * log (x / a)) +
        (k * log (x / a) + (k / b) * correction (b * log (x / a))) -
        (exp (k * log (x / a) + (k / b) * correction (b * log (x / a))) - 1) := by
  let d := log (x / a)
  have hp := ha.trans_le hx
  unfold ShapeFacesPGW.logDensity
  rw [rpow_def_of_pos (div_pos hp ha), mul_comm (log (x / a)) b]
  change log b + log (k / b) - log x + b * d +
    (k / b - 1) * log (1 + exp (b * d)) -
    ((1 + exp (b * d)) ^ (k / b) - 1) =
      log k - log x - correction (b * d) + (k * d + (k / b) * correction (b * d)) -
        (exp (k * d + (k / b) * correction (b * d)) - 1)
  rw [log_div hk.ne' hb.ne', rpow_def_of_pos (by positivity), softplus_split]
  have he : (b * d + correction (b * d)) * (k / b) =
      k * d + (k / b) * correction (b * d) := by field_simp
  rw [he]
  linear_combination (1 : ℝ) * he

private theorem correction_limit {a x : ℝ} (ha : 0 < a) (hx : a ≤ x) :
    Tendsto (fun b : ℝ ↦ correction (b * log (x / a))) atTop
      (𝓝 (if x = a then log 2 else 0)) := by
  by_cases h : x = a
  · subst x
    simp [ha.ne', correction, show (1 + 1 : ℝ) = 2 by norm_num]
  · have hd : 0 < log (x / a) :=
      log_pos ((one_lt_div ha).mpr (lt_of_le_of_ne hx (Ne.symm h)))
    have hlin : Tendsto (fun b : ℝ ↦ b * log (x / a)) atTop atTop :=
      tendsto_id.atTop_mul_const hd
    have he := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp hlin)
    have hsum : Tendsto (fun b : ℝ ↦ 1 + exp (-(b * log (x / a)))) atTop (𝓝 1) := by
      simpa [Function.comp_def] using he.const_add 1
    simpa [h, correction] using hsum.log (by norm_num : (1 : ℝ) ≠ 0)

/-- A fixed scale at or below every observation gives the threshold limit.
Each observation equal to the scale contributes a factor of one half. -/
theorem held_scale_threshold_path {x : Fin n → ℝ} {a k : ℝ} (ha : 0 < a)
    (hk : 0 < k) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun b : ℝ ↦ ShapeFacesPGW.logLik x b a (k / b)) atTop
      (𝓝 (thresholdLogLik x a k)) := by
  unfold ShapeFacesPGW.logLik thresholdLogLik
  apply tendsto_finsetSum
  intro i _
  have hc := correction_limit ha (hx i)
  have hr : Tendsto (fun b : ℝ ↦ k / b) atTop (𝓝 0) :=
    by simpa [div_eq_mul_inv] using (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul k
  have hv : Tendsto (fun b : ℝ ↦ k * log (x i / a) +
      (k / b) * correction (b * log (x i / a))) atTop (𝓝 (k * log (x i / a))) := by
    simpa using (hr.mul hc).const_add (k * log (x i / a))
  have hExp := (Real.continuous_exp.continuousAt.tendsto.comp hv).sub_const 1
  apply Tendsto.congr' _ (((tendsto_const_nhds.sub hc).add hv).sub hExp)
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  exact (corrected_density ha (hx i) hb hk).symm

/-- At or below the minimum, each finite density loses to the threshold
density at the same product of shape and exponent. -/
theorem held_scale_logDensity_lt_threshold {a x b k : ℝ} (ha : 0 < a)
    (hx : a ≤ x) (hb : 0 < b) (hk : 0 < k) :
    ShapeFacesPGW.logDensity b a (k / b) x <
      log k - log x - (if x = a then log 2 else 0) +
        k * log (x / a) - (exp (k * log (x / a)) - 1) := by
  rw [corrected_density ha hx hb hk]
  let u := k * log (x / a)
  let z := b * log (x / a)
  let e := (k / b) * correction z
  have hd := log_nonneg ((le_div_iff₀ ha).mpr (by simpa using hx))
  have hz : 0 ≤ z := mul_nonneg hb.le hd
  have hu : 0 ≤ u := mul_nonneg hk.le hd
  have he : 0 < e := mul_pos (div_pos hk hb) (correction_bound hz).1
  have hpen : (if x = a then log 2 else 0) ≤ correction z := by
    split_ifs with h
    · simp [z, h, ha.ne', correction, show (1 + 1 : ℝ) = 2 by norm_num]
    · exact (correction_bound hz).1.le
  have hE : exp u + e < exp (u + e) := by
    rw [exp_add]
    have h₁ := mul_lt_mul_of_pos_left (add_one_lt_exp he.ne') (exp_pos u)
    have h₂ := mul_le_mul_of_nonneg_right (one_le_exp_iff.mpr hu) he.le
    linarith only [show exp u * (e + 1) < exp u * exp e from h₁, h₂]
  change log k - log x - correction z + (u + e) - (exp (u + e) - 1) < _
  change _ < log k - log x - (if x = a then log 2 else 0) + u - (exp u - 1)
  linarith

/-- A positive held scale at or below all observations has no finite
maximizer. This includes constant samples and ties at the scale. -/
theorem held_scale_no_max_of_le_all (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, a ≤ x i) :
    ¬ ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p := by
  rintro ⟨⟨b, c⟩, hp, hm⟩
  have hb : 0 < b := hp.1
  have hc : 0 < c := hp.2
  let k := b * c
  have hk : 0 < k := mul_pos hb hc
  have hkc : k / b = c := by dsimp [k]; field_simp
  have hlt : ShapeFacesPGW.logLik x b a c < thresholdLogLik x a k := by
    rw [← hkc]
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      (fun i _ ↦ held_scale_logDensity_lt_threshold ha (hx i) hb hk)
  have hle : thresholdLogLik x a k ≤ ShapeFacesPGW.logLik x b a c := by
    apply le_of_tendsto (held_scale_threshold_path ha hk hx)
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact hm (show (t, k / t) ∈ Weibull.domain from ⟨ht, div_pos hk ht⟩)
  exact hlt.not_ge hle

def gompertzLogLik (x : Fin n → ℝ) (b r : ℝ) : ℝ :=
  ∑ i, (log b - log (x i) + log (r * (x i) ^ b) +
    r * (x i) ^ b + 1 - exp (r * (x i) ^ b))

/-- The power-transformed Gompertz face along a path with fixed shape. -/
theorem gompertz_path {x : Fin n → ℝ} {b r : ℝ} (hb : 0 < b) (hr : 0 < r)
    (hx : ∀ i, 0 < x i) :
    Tendsto (fun h : ℝ ↦ ShapeFacesPGW.logLik x b ((h * r) ^ (-1 / b)) h⁻¹)
      (𝓝[>] 0) (𝓝 (gompertzLogLik x b r)) := by
  unfold ShapeFacesPGW.logLik gompertzLogLik
  apply tendsto_finsetSum
  intro i _
  let t := r * (x i) ^ b
  have ht : 0 < t := mul_pos hr (rpow_pos_of_pos (hx i) _)
  have hd : HasDerivAt (fun h : ℝ ↦ log (1 + h * t)) t 0 := by
    simpa using (((hasDerivAt_id (0 : ℝ)).mul_const t).const_add 1).log
      (by simp : 1 + id (0 : ℝ) * t ≠ 0)
  have hu : Tendsto (fun h : ℝ ↦ h⁻¹ * log (1 + h * t)) (𝓝[>] 0) (𝓝 t) := by
    simpa using hd.tendsto_slope_zero_right
  have hl : Tendsto (fun h : ℝ ↦ log (1 + h * t)) (𝓝[>] 0) (𝓝 0) := by
    simpa using hd.continuousAt.tendsto.mono_left
      (nhdsWithin_le_nhds (s := Set.Ioi (0 : ℝ)))
  have hE := Real.continuous_exp.continuousAt.tendsto.comp hu
  have hsum := ((tendsto_const_nhds (x := log b - log (x i) + log t)).add hu).sub hl
  have hlim := (hsum.add_const 1).sub hE
  refine (by simpa [Function.comp_def] using hlim : Tendsto
    (fun h : ℝ ↦ log b - log (x i) + log t + h⁻¹ * log (1 + h * t) -
      log (1 + h * t) + 1 - exp (h⁻¹ * log (1 + h * t))) (𝓝[>] 0)
      (𝓝 (log b - log (x i) + log t + t + 1 - exp t))).congr' ?_
  filter_upwards [self_mem_nhdsWithin] with h hh
  have hp : 0 < h := hh
  have hscale : 0 < (h * r) ^ (-1 / b) := rpow_pos_of_pos (mul_pos hp hr) _
  have hlog : b * log (x i / (h * r) ^ (-1 / b)) = log (h * t) := by
    rw [log_div (hx i).ne' hscale.ne', log_rpow (mul_pos hp hr), log_mul hp.ne' ht.ne']
    dsimp [t]
    rw [log_mul hr.ne' (rpow_pos_of_pos (hx i) b).ne', log_rpow (hx i),
      log_mul hp.ne' hr.ne']
    field_simp; ring
  have hpow : (x i / (h * r) ^ (-1 / b)) ^ b = h * t := by
    rw [rpow_def_of_pos (div_pos (hx i) hscale), mul_comm, hlog, exp_log (mul_pos hp ht)]
  rw [ShapeFacesPGW.logDensity, hpow, hlog, log_inv, log_mul hp.ne' ht.ne',
    rpow_def_of_pos (by positivity : 0 < 1 + h * t), mul_comm (log (1 + h * t))]
  ring

/-- The cumulative hazard on the fixed-scale threshold path, on both
sides of the threshold and at equality. -/
theorem threshold_hazard_path {x a k : ℝ} (hx : 0 < x) (ha : 0 < a) (hk : 0 < k) :
    Tendsto (fun b : ℝ ↦ (1 + (x / a) ^ b) ^ (k / b) - 1) atTop
      (𝓝 (if a ≤ x then (x / a) ^ k - 1 else 0)) := by
  let d := log (x / a)
  have hp := div_pos hx ha
  have hk0 : Tendsto (fun b : ℝ ↦ k / b) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv] using (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul k
  have hlog (b : ℝ) :
      (1 + (x / a) ^ b) ^ (k / b) - 1 =
        exp ((k / b) * log (1 + exp (b * d))) - 1 := by
    rw [rpow_def_of_pos hp, mul_comm (log (x / a)) b,
      rpow_def_of_pos (by positivity)]
    congr 2
    exact mul_comm _ _
  by_cases hax : a ≤ x
  · rw [ite_eq_left hax, rpow_def_of_pos hp, mul_comm (log (x / a))]
    have hc := correction_limit ha hax
    have ht : Tendsto (fun b : ℝ ↦ k * d + (k / b) * correction (b * d))
        atTop (𝓝 (k * d)) := by simpa using (hk0.mul hc).const_add (k * d)
    apply ((Real.continuous_exp.continuousAt.tendsto.comp ht).sub_const 1).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    rw [hlog b, Function.comp_apply, softplus_split]
    congr 2
    field_simp
  · rw [ite_eq_right hax]
    have hd : d < 0 := log_neg hp ((div_lt_one ha).mpr (lt_of_not_ge hax))
    have he := tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg hd)
    have hsum : Tendsto (fun b : ℝ ↦ 1 + exp (b * d)) atTop (𝓝 1) := by
      simpa [Function.comp_def] using he.const_add 1
    have hv := hk0.mul (hsum.log one_ne_zero)
    simp only [log_one, mul_zero] at hv
    have ht := (Real.continuous_exp.continuousAt.tendsto.comp hv).sub_const 1
    simp only [exp_zero, sub_self] at ht
    apply ht.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b _
    exact (hlog b).symm

/-- Constants strictly below the held scale concentrate by sending the
shape to infinity and taking the reciprocal powered ratio as exponent. -/
theorem held_scale_constant_below {a x : ℝ} (hn : 0 < n) (hx : 0 < x) (ha : x < a) :
    Tendsto (fun b : ℝ ↦ ShapeFacesPGW.logLik (fun _ : Fin n ↦ x)
      b a ((x / a) ^ b)⁻¹) atTop atTop := by
  let d := log (x / a)
  have ha0 := hx.trans ha
  have hd : d < 0 := log_neg (div_pos hx ha0) ((div_lt_one ha0).mpr ha)
  let q := fun b : ℝ ↦ exp (b * d)
  have ht : Tendsto q atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg hd)
  have htr : Tendsto q atTop (𝓝[>] 0) := by
    rw [tendsto_nhdsWithin_iff]
    exact ⟨ht, Eventually.of_forall (fun b ↦ exp_pos _)⟩
  have hD : HasDerivAt (fun t : ℝ ↦ log (1 + t)) 1 0 := by
    simpa using ((hasDerivAt_id (0 : ℝ)).const_add 1).log
      (by simp : (1 : ℝ) + id 0 ≠ 0)
  have hu : Tendsto (fun b ↦ (q b)⁻¹ * log (1 + q b)) atTop (𝓝 1) := by
    simpa [Function.comp_def] using hD.tendsto_slope_zero_right.comp htr
  have hl : Tendsto (fun b ↦ log (1 + q b)) atTop (𝓝 0) := by
    simpa [Function.comp_def] using hD.continuousAt.tendsto.comp ht
  have he := Real.continuous_exp.continuousAt.tendsto.comp hu
  have hrem := ((((hu.sub hl).sub he).add_const 1).sub_const (log x))
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  apply ((tendsto_log_atTop.atTop_add hrem).const_mul_atTop hN).congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  have hq : (x / a) ^ b = q b := by rw [rpow_def_of_pos (div_pos hx ha0), mul_comm]
  simp only [ShapeFacesPGW.logLik, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ShapeFacesPGW.logDensity, hq, log_inv, Function.comp_apply]
  rw [rpow_def_of_pos (by positivity : 0 < 1 + q b), mul_comm (log (1 + q b)),
    show log (q b) = b * d by exact log_exp _]
  change _ = (n : ℝ) * (log b - b * d - log x + b * d +
    ((q b)⁻¹ - 1) * log (1 + q b) - (exp ((q b)⁻¹ * log (1 + q b)) - 1))
  ring

private theorem exp_loss_antitone {u v : ℝ} (hu : 0 ≤ u) (huv : u ≤ v) :
    v + 1 - exp v ≤ u + 1 - exp u := by
  have h₁ := mul_le_mul_of_nonneg_left (add_one_le_exp (v - u)) (exp_pos u).le
  rw [← exp_add, show u + (v - u) = v by ring] at h₁
  have h₂ := mul_le_mul_of_nonneg_right (one_le_exp_iff.mpr hu) (sub_nonneg.mpr huv)
  linarith only [h₁, h₂]

private theorem gompertz_matched_point {b a c x X : ℝ} (hb : 0 < b) (ha : 0 < a)
    (hc : 0 < c) (hx : 0 < x) (hxX : x ≤ X) (hhalf : 1 / 2 ≤ (x / X) ^ b) :
    let r := c * log (1 + (X / a) ^ b) / X ^ b
    ShapeFacesPGW.logDensity b a c x <
      log b - log x + log (r * x ^ b) + r * x ^ b + 1 - exp (r * x ^ b) := by
  dsimp only
  have hX := hx.trans_le hxX
  let p := (x / X) ^ b
  let t := (X / a) ^ b
  let U := c * log (1 + t)
  have hp : 0 < p := rpow_pos_of_pos (div_pos hx hX) _
  have hp1 : p ≤ 1 := by
    dsimp [p]
    exact rpow_le_one (div_nonneg hx.le hX.le) ((div_le_one hX).mpr hxX) hb.le
  have ht : 0 < t := rpow_pos_of_pos (div_pos hX ha) _
  have hpT : (x / a) ^ b = p * t := by
    rw [← mul_rpow (div_nonneg hx.le hX.le) (div_nonneg hX.le ha.le)]
    congr 1
    field_simp
  have hr : (U / X ^ b) * x ^ b = p * U := by
    dsimp [p]
    rw [div_rpow hx.le hX.le]
    ring
  change ShapeFacesPGW.logDensity b a c x <
    log b - log x + log ((U / X ^ b) * x ^ b) + (U / X ^ b) * x ^ b +
      1 - exp ((U / X ^ b) * x ^ b)
  rw [hr]
  have hlog : p * log (1 + t) ≤ log (1 + p * t) := by
    have h := strictConcaveOn_log_Ioi.concaveOn.2
      (show 0 < 1 + t by positivity)
      (show (0 : ℝ) < 1 by norm_num) hp.le (sub_nonneg.mpr hp1)
      (show p + (1 - p) = 1 by ring)
    simpa only [smul_eq_mul, log_one, mul_zero, add_zero,
      show p * (1 + t) + (1 - p) * 1 = 1 + p * t by ring] using h
  have hU : 0 < p * U := mul_pos hp (mul_pos hc (log_pos (by linarith)))
  have hrat : t / (1 + p * t) < log (1 + t) := by
    apply lt_of_le_of_lt _ (lt_log_one_add_of_pos ht)
    apply (div_le_div_iff₀ (by positivity) (by positivity)).2
    nlinarith only [mul_nonneg (show 0 ≤ 2 * p - 1 by linarith only [hhalf]) (sq_nonneg t)]
  have hbound : c * (p * t) / (1 + p * t) < p * U := by
    have h := mul_lt_mul_of_pos_left hrat (mul_pos hc hp)
    dsimp [U]
    convert h using 1 <;> ring
  have hlhs : ShapeFacesPGW.logDensity b a c x =
      log b - log x + log (c * (p * t) / (1 + p * t)) +
        c * log (1 + p * t) + 1 - exp (c * log (1 + p * t)) := by
    unfold ShapeFacesPGW.logDensity
    have hP : 0 < p * t := mul_pos hp ht
    have h1 : 0 < 1 + p * t := by positivity
    have hl : log (p * t) = b * log (x / a) := by
      rw [← hpT, log_rpow (div_pos hx ha), mul_comm]
    rw [hpT, log_div (mul_pos hc hP).ne' h1.ne', log_mul hc.ne' hP.ne', hl,
      rpow_def_of_pos h1, mul_comm (log (1 + p * t))]
    ring
  rw [hlhs]
  have hE := exp_loss_antitone hU.le
    (show p * U ≤ c * log (1 + p * t) by
      dsimp [U]
      linarith only [mul_le_mul_of_nonneg_left hlog hc.le,
        show p * (c * log (1 + t)) = c * (p * log (1 + t)) by ring])
  have hstrict := log_lt_log (by positivity : 0 < c * (p * t) / (1 + p * t)) hbound
  linarith only [hE, hstrict]

/-- At a held positive shape, a powered observation-to-maximum ratio at
least one half for every observation excludes every finite maximizer. -/
theorem held_shape_narrow_no_max (hn : 0 < n) {x : Fin n → ℝ} {b X : ℝ}
    (hb : 0 < b) (hx : ∀ i, 0 < x i) (hX : ∀ i, x i ≤ X)
    (hnarrow : ∀ i, 1 / 2 ≤ (x i / X) ^ b) :
    ¬ ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p := by
  rintro ⟨⟨a, c⟩, hp, hm⟩
  have ha : 0 < a := hp.1
  have hc : 0 < c := hp.2
  have hX0 : 0 < X := (hx ⟨0, hn⟩).trans_le (hX ⟨0, hn⟩)
  let r := c * log (1 + (X / a) ^ b) / X ^ b
  have hr : 0 < r := div_pos (mul_pos hc (log_pos (by
    linarith [rpow_pos_of_pos (div_pos hX0 ha) b]))) (rpow_pos_of_pos hX0 _)
  have hlt : ShapeFacesPGW.logLik x b a c < gompertzLogLik x b r := by
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      (fun i _ ↦ gompertz_matched_point hb ha hc (hx i) (hX i) (hnarrow i))
  have hle : gompertzLogLik x b r ≤ ShapeFacesPGW.logLik x b a c := by
    apply le_of_tendsto (gompertz_path hb hr hx)
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    exact hm (show ((h * r) ^ (-1 / b), h⁻¹) ∈ Weibull.domain from
      ⟨rpow_pos_of_pos (mul_pos hh hr) _, inv_pos.mpr hh⟩)
  exact hlt.not_ge hle

private theorem chart_density {a b c x : ℝ} (ha : 0 < a) (hx : 0 < x)
    (hc : 0 < c) :
    let t := exp (b * log (x / a))
    let u := c * log (1 + t)
    ShapeFacesPGW.logDensity b a c x =
      log b - log x + log (c * (t / (1 + t))) + (u + 1 - exp u) := by
  dsimp only
  let t := exp (b * log (x / a))
  have ht : 0 < t := exp_pos _
  have h1 : 0 < 1 + t := by positivity
  rw [ShapeFacesPGW.logDensity, rpow_def_of_pos (div_pos hx ha), mul_comm (log (x / a))]
  change log b + log c - log x + b * log (x / a) + (c - 1) * log (1 + t) -
    ((1 + t) ^ c - 1) = _
  rw [rpow_def_of_pos h1, log_mul hc.ne' (div_pos ht h1).ne', log_div ht.ne' h1.ne',
    show log t = b * log (x / a) by exact log_exp _, mul_comm c]
  ring

private theorem logistic_le_log {t : ℝ} (ht : 0 < t) : t / (1 + t) ≤ log (1 + t) := by
  have h1 : 0 < 1 + t := by positivity
  have h := log_le_sub_one_of_pos (inv_pos.mpr h1)
  rw [log_inv] at h
  have h' : t / (1 + t) = 1 - (1 + t)⁻¹ := by field_simp; ring
  linarith only [h', h]

private theorem density_bounds {a b c x : ℝ} (ha : 0 < a) (hx : 0 < x) (hc : 0 < c) :
    let u := c * log (1 + exp (b * log (x / a)))
    let L := ShapeFacesPGW.logDensity b a c x - (log b - log x)
    L ≤ 0 ∧ L ≤ log u ∧ L ≤ -log u ∧ L ≤ log c := by
  dsimp only
  let t := exp (b * log (x / a))
  let u := c * log (1 + t)
  have ht : 0 < t := exp_pos _
  have hu : 0 < u := mul_pos hc (log_pos (by linarith))
  rw [chart_density ha hx hc]
  change log b - log x + log (c * (t / (1 + t))) + (u + 1 - exp u) -
    (log b - log x) ≤ 0 ∧ _ ≤ log u ∧ _ ≤ -log u ∧ _ ≤ log c
  have hlog := log_le_log (by positivity : 0 < c * (t / (1 + t)))
    (mul_le_mul_of_nonneg_left (logistic_le_log ht) hc.le)
  have hlogc := log_le_log (by positivity : 0 < c * (t / (1 + t)))
    (mul_le_of_le_one_right hc.le ((div_le_one (by positivity)).mpr (by linarith)))
  have he := quadratic_le_exp_of_nonneg hu.le
  have hl := log_le_sub_one_of_pos (show 0 < u^2 / 2 by positivity)
  rw [log_div (pow_pos hu 2).ne' two_ne_zero, log_pow] at hl
  have h2 : log 2 < 1 := by
    rw [log_lt_iff_lt_exp (by norm_num : (0 : ℝ) < 2)]
    have := quadratic_le_exp_of_nonneg (x := 1) zero_le_one
    linarith
  exact ⟨by nlinarith only [hlog, he, hl, h2, sq_nonneg u],
    by nlinarith only [hlog, he, sq_nonneg u],
    by linarith only [hlog, he, hl, h2],
    by nlinarith only [hlogc, he, sq_nonneg u]⟩

/-- For nonconstant data with a held scale above some observation, joint
optimization over the shape and exponent attains a finite maximum. -/
theorem held_scale_attains {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    (hx : ∀ i, 0 < x i) {i j : Fin n} (hij : x i < x j) (hia : x i < a) :
    ∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p := by
  have hn : 0 < (n : ℝ) := by exact_mod_cast i.isLt.pos
  let d : Fin n → ℝ := fun i ↦ log (x i / a)
  have hdi : d i < 0 := log_neg (div_pos (hx i) ha) ((div_lt_one ha).mpr hia)
  have hdij : d i < d j := log_lt_log (div_pos (hx i) ha)
    (div_lt_div_of_pos_right hij ha)
  let δ := min (d j - d i) (-d i)
  have hδ : 0 < δ := lt_min (sub_pos.mpr hdij) (neg_pos.mpr hdi)
  let f := fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x p.1 a p.2
  have hcont : ContinuousOn f Weibull.domain := by
    apply continuousOn_finsetSum
    intro t _
    unfold ShapeFacesPGW.logDensity
    -- Express the powered ratios using a fixed logarithm.
    simp_rw [rpow_def_of_pos (div_pos (hx t) ha)]
    have hpos (p : ℝ × ℝ) : 0 < 1 + exp (log (x t / a) * p.1) := by positivity
    simp_rw [rpow_def_of_pos (hpos _)]
    have hfst : ContinuousOn (fun p : ℝ × ℝ ↦ log p.1) Weibull.domain :=
      continuous_fst.continuousOn.log (fun p hp ↦ (show 0 < p.1 from hp.1).ne')
    have hsnd : ContinuousOn (fun p : ℝ × ℝ ↦ log p.2) Weibull.domain :=
      continuous_snd.continuousOn.log (fun p hp ↦ (show 0 < p.2 from hp.2).ne')
    have harg : Continuous (fun p : ℝ × ℝ ↦ 1 + exp (log (x t / a) * p.1)) := by
      fun_prop
    have hL := harg.log (fun p ↦ (hpos p).ne')
    exact ((((hfst.add hsnd).sub continuousOn_const).add (by fun_prop)).add
      ((continuous_snd.sub continuous_const).mul hL).continuousOn).sub
        ((Real.continuous_exp.comp (hL.mul continuous_snd)).sub continuous_const).continuousOn
  have hnonempty : Weibull.domain.Nonempty := ⟨(1, 1), one_pos, one_pos⟩
  apply Generic.exists_isMaxOn_of_tendsto_cocompact hnonempty hcont
  apply tendsto_atBot.2
  intro M
  let J := ∑ t, log (x t)
  let lo := exp ((M + J) / n)
  let hi := max 1 ((log 2 - J - M - (n : ℝ) * (1 + log (δ / (2 * n)))) / (δ / 2))
  let A := log (1 + exp (hi * d i))
  have hA : 0 < A := log_pos (by linarith [exp_pos (hi * d i)])
  let cmin := exp ((M + J) / n - log hi)
  let cmax := exp ((n : ℝ) * log hi - J - M) / A
  let K : Set (ℝ × ℝ) := Set.Icc lo hi ×ˢ Set.Icc cmin cmax
  have hKD : K ⊆ Weibull.domain := fun _ h ↦
    ⟨(exp_pos _).trans_le h.1.1, (exp_pos _).trans_le h.2.1⟩
  have hcomp : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hlevel (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) (hM : M ≤ f p) : p ∈ K := by
    have hb : 0 < p.1 := hp.1
    have hc : 0 < p.2 := hp.2
    let u := fun t ↦ p.2 * log (1 + exp (p.1 * d t))
    let L := fun t ↦ ShapeFacesPGW.logDensity p.1 a p.2 (x t) - (log p.1 - log (x t))
    have hL (t : Fin n) : L t ≤ 0 ∧ L t ≤ log (u t) ∧
        L t ≤ -log (u t) ∧ L t ≤ log p.2 := density_bounds ha (hx t) hc
    have hf : f p = (n : ℝ) * log p.1 - J + ∑ t, L t := by
      simp only [f, ShapeFacesPGW.logLik, J, L, Finset.sum_sub_distrib, Finset.sum_const,
        Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      ring
    have hu (t : Fin n) : 0 < u t := mul_pos hc (log_pos (by linarith [exp_pos (p.1 * d t)]))
    have hsum : (∑ t, L t) ≤ 0 := Finset.sum_nonpos (fun t _ ↦ (hL t).1)
    have hij' : i ≠ j := fun h ↦ hij.ne (congrArg x h)
    have hpair : (∑ t, L t) ≤ log (u i / u j) := by
      -- Apply the subset bound to the negated losses.
      have hneg := Finset.sum_le_sum_of_subset_of_nonneg
        (f := fun t ↦ -L t) (Finset.subset_univ {i, j})
        (fun t _ _ ↦ neg_nonneg.mpr (hL t).1)
      rw [Finset.sum_pair hij', Finset.sum_neg_distrib] at hneg
      rw [log_div (hu i).ne' (hu j).ne']
      linarith only [hneg, (hL i).2.1, (hL j).2.2.1]
    have hratio : u i / u j ≤ 2 * exp (-p.1 * δ) := by
      let s := exp (p.1 * d i)
      let t := exp (p.1 * d j)
      have hs : 0 < s := exp_pos _
      have ht : 0 < t := exp_pos _
      have ht' : 0 < log (1 + t) := log_pos (by linarith)
      dsimp [u]
      rw [mul_div_mul_left _ _ hc.ne']
      change log (1 + s) / log (1 + t) ≤ _
      calc
        _ ≤ s / (t / (1 + t)) := div_le_div₀ hs.le
          (by linarith [log_le_sub_one_of_pos (show 0 < 1 + s by positivity)])
          (by positivity) (logistic_le_log ht)
        _ = s / t + s := by field_simp
        _ ≤ _ := by
          dsimp [s, t]
          rw [← exp_sub]
          have h1 : p.1 * d i - p.1 * d j ≤ -p.1 * δ := by
            nlinarith only [mul_le_mul_of_nonneg_left (min_le_left (d j - d i) (-d i)) hb.le]
          have h2 : p.1 * d i ≤ -p.1 * δ := by
            nlinarith only [mul_le_mul_of_nonneg_left (min_le_right (d j - d i) (-d i)) hb.le]
          linarith only [exp_le_exp.mpr h1, exp_le_exp.mpr h2]
    have hpair' := hpair.trans (log_le_log (div_pos (hu i) (hu j)) hratio)
    rw [log_mul two_ne_zero (exp_pos _).ne', log_exp] at hpair'
    have hlo : lo ≤ p.1 := by
      rw [← exp_log hb, exp_le_exp]
      exact (div_le_iff₀ hn).2 (by linarith only [hf, hM, hsum])
    have hhi : p.1 ≤ hi := by
      have hD : 0 < δ / (2 * (n : ℝ)) := by positivity
      have hlog := log_le_sub_one_of_pos (mul_pos hD hb)
      rw [log_mul hD.ne' hb.ne'] at hlog
      have hprod : (n : ℝ) * (δ / (2 * (n : ℝ)) * p.1) = δ * p.1 / 2 := by
        field_simp
      apply le_max_of_le_right
      apply (le_div_iff₀ (show 0 < δ / 2 by positivity)).2
      nlinarith only [mul_le_mul_of_nonneg_left hlog hn.le, hprod, hf, hM, hpair']
    have hlog := log_le_log hb hhi
    have hcsum : (∑ t, L t) ≤ (n : ℝ) * log p.2 := by
      simpa [nsmul_eq_mul] using Finset.sum_le_sum (s := Finset.univ)
        (fun t _ ↦ (hL t).2.2.2)
    have hcmin : cmin ≤ p.2 := by
      rw [← exp_log hc, exp_le_exp]
      apply (le_of_mul_le_mul_left _ hn)
      have he : (n : ℝ) * ((M + J) / n - log hi) = M + J - n * log hi := by field_simp
      linarith only [he, hf, hM, hcsum, mul_le_mul_of_nonneg_left hlog hn.le]
    have hsingle : (∑ t, L t) ≤ -log (u i) := by
      have h := Finset.single_le_sum (f := fun t ↦ -L t)
        (fun t _ ↦ neg_nonneg.mpr (hL t).1) (Finset.mem_univ i)
      rw [Finset.sum_neg_distrib] at h
      linarith only [h, (hL i).2.2.1]
    have hAi : p.2 * A ≤ u i := by
      apply mul_le_mul_of_nonneg_left _ hc.le
      apply log_le_log (by positivity)
      simpa only [add_le_add_iff_left, exp_le_exp] using mul_le_mul_of_nonpos_right hhi hdi.le
    have hcmax : p.2 ≤ cmax := by
      apply (le_div_iff₀ hA).2
      rw [← exp_log (mul_pos hc hA), exp_le_exp]
      have hlogA := log_le_log (mul_pos hc hA) hAi
      linarith only [hlogA, hsingle, hf, hM, mul_le_mul_of_nonneg_left hlog hn.le]
    exact ⟨⟨hlo, hhi⟩, ⟨hcmin, hcmax⟩⟩
  have hsubcomp : IsCompact ((Subtype.val : Weibull.domain → ℝ × ℝ) ⁻¹' K) :=
    Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage' hcomp
      (by simpa only [Subtype.range_coe] using hKD)
  have hev : ∀ᶠ p : Weibull.domain in cocompact Weibull.domain,
      p ∈ ((Subtype.val : Weibull.domain → ℝ × ℝ) ⁻¹' K)ᶜ :=
    hsubcomp.compl_mem_cocompact
  exact hev.mono
    (fun p hp ↦ (lt_of_not_ge fun hM ↦ hp (hlevel p p.property hM)).le)

/-- Holding the scale leaves a finite maximizer exactly for nonconstant
positive data with an observation strictly below the scale. -/
theorem held_scale_exists_iff (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, 0 < x i) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x q.1 a q.2) Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, x i < a := by
  constructor
  · intro hm
    have hlt : ∃ i, x i < a := by
      by_contra! hh
      exact held_scale_no_max_of_le_all hn ha hh hm
    refine ⟨?_, hlt⟩
    by_contra! he
    obtain ⟨i, hi⟩ := hlt
    obtain ⟨t, ht⟩ : ∃ t : ℝ, x = fun _ ↦ t := ⟨x i, funext fun j ↦ he j i⟩
    subst x
    obtain ⟨p, _, hmax⟩ := hm
    have hpath := held_scale_constant_below hn (hx i) hi
    obtain ⟨b, hb, hB⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (hpath.eventually (eventually_gt_atTop
        (ShapeFacesPGW.logLik (fun _ : Fin n ↦ t) p.1 a p.2)))).exists
    let c : ℝ := ((t / a) ^ b)⁻¹
    have hq : (b, c) ∈ Weibull.domain :=
      ⟨hb, inv_pos.mpr (rpow_pos_of_pos (div_pos (hx i) ha) b)⟩
    have hle := isMaxOn_iff.mp hmax (b, c) hq
    exact hB.not_ge hle
  · rintro ⟨hne, i, hi⟩
    obtain ⟨j, _, hj⟩ := Finset.exists_min_image Finset.univ x
      ⟨i, Finset.mem_univ _⟩
    have hj' (k : Fin n) : x j ≤ x k := hj k (Finset.mem_univ _)
    obtain ⟨k, hk⟩ : ∃ k, x j < x k := by
      by_contra! h
      obtain ⟨r, s, hrs⟩ := hne
      exact hrs ((le_antisymm (h r) (hj' r)).trans (le_antisymm (h s) (hj' s)).symm)
    exact held_scale_attains ha hx hk ((hj' i).trans_lt hi)

private def logSlope (z : ℝ) : ℝ := if z = 0 then 1 else log (1 + z) / z

private theorem logSlope_cont : ContinuousOn logSlope (Ici 0) := by
  intro z hz
  by_cases h : z = 0
  · subst z
    apply ContinuousAt.continuousWithinAt
    rw [continuousAt_iff_punctured_nhds]
    have hd : HasDerivAt (fun t : ℝ ↦ log (1 + t)) 1 0 := by
      simpa using ((hasDerivAt_id (0 : ℝ)).const_add 1).log
        (by simp : (1 : ℝ) + id 0 ≠ 0)
    have ht : Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
      simpa [div_eq_mul_inv, mul_comm] using hd.tendsto_slope_zero
    simp only [logSlope]
    apply ht.congr'
    filter_upwards [self_mem_nhdsWithin] with t (ht : t ≠ 0)
    simp [logSlope, ht]
  · have hz' : 0 < z := lt_of_le_of_ne hz (Ne.symm h)
    have ht : ContinuousAt (fun t : ℝ ↦ log (1 + t) / t) z :=
      (((continuousAt_id (x := z)).const_add 1).log (by simp only [id_eq]; linarith)).div
        continuousAt_id h
    apply (ht.congr ?_).continuousWithinAt
    filter_upwards [Ioi_mem_nhds hz'] with t ht
    simp [logSlope, ht.ne']

private def fixedShapeChart (x : Fin n → ℝ) (b : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, let t := p.1 * (x i) ^ b
       let u := t * logSlope (p.2 * t)
       log b - log (x i) + log t - log (1 + p.2 * t) + u + 1 - exp u

private def chartDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ici 0

private theorem chart_cont {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) (b : ℝ) :
    ContinuousOn (fixedShapeChart x b) chartDomain := by
  apply continuousOn_finsetSum
  intro i _
  let t (p : ℝ × ℝ) := p.1 * (x i) ^ b
  have ht : Continuous t := continuous_fst.mul_const _
  have htp (p : ℝ × ℝ) (hp : p ∈ chartDomain) : 0 < t p :=
    mul_pos (show 0 < p.1 from hp.1) (rpow_pos_of_pos (hx i) b)
  have harg : Continuous (fun p : ℝ × ℝ ↦ p.2 * t p) := continuous_snd.mul ht
  have hz (p : ℝ × ℝ) (hp : p ∈ chartDomain) : 0 ≤ p.2 * t p :=
    mul_nonneg hp.2 (htp p hp).le
  have hu : ContinuousOn (fun p : ℝ × ℝ ↦ t p * logSlope (p.2 * t p)) chartDomain :=
    ht.continuousOn.mul (logSlope_cont.comp harg.continuousOn hz)
  exact (((((continuousOn_const.add (ht.continuousOn.log (fun p hp ↦ (htp p hp).ne'))).sub
    ((harg.const_add 1).continuousOn.log (fun p hp ↦ by linarith [hz p hp]))).add
    hu).add continuousOn_const).sub (Real.continuous_exp.continuousOn.comp hu
    (fun _ _ ↦ mem_univ _)))

private theorem chart_endpoint (x : Fin n → ℝ) (b r : ℝ) :
    fixedShapeChart x b (r, 0) = gompertzLogLik x b r := by
  simp [fixedShapeChart, gompertzLogLik, logSlope]

private theorem chart_physical {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {b r h : ℝ}
    (hb : 0 < b) (hr : 0 < r) (hh : 0 < h) :
    fixedShapeChart x b (r, h) =
      ShapeFacesPGW.logLik x b ((h * r) ^ (-1 / b)) h⁻¹ := by
  unfold ShapeFacesPGW.logLik fixedShapeChart
  apply Finset.sum_congr rfl
  intro i _
  let t := r * (x i) ^ b
  have ht : 0 < t := mul_pos hr (rpow_pos_of_pos (hx i) _)
  have ha : 0 < (h * r) ^ (-1 / b) := rpow_pos_of_pos (mul_pos hh hr) _
  have hlog : b * log (x i / (h * r) ^ (-1 / b)) = log (h * t) := by
    rw [log_div (hx i).ne' ha.ne', log_rpow (mul_pos hh hr), log_mul hh.ne' ht.ne']
    dsimp [t]
    rw [log_mul hr.ne' (rpow_pos_of_pos (hx i) b).ne', log_rpow (hx i),
      log_mul hh.ne' hr.ne']
    field_simp
    ring
  have hpow : (x i / (h * r) ^ (-1 / b)) ^ b = h * t := by
    rw [rpow_def_of_pos (div_pos (hx i) ha), mul_comm, hlog, exp_log (mul_pos hh ht)]
  change log b - log (x i) + log t - log (1 + h * t) +
      t * logSlope (h * t) + 1 - exp (t * logSlope (h * t)) = _
  rw [logSlope, ite_eq_right (mul_pos hh ht).ne']
  have hu : t * (log (1 + h * t) / (h * t)) = h⁻¹ * log (1 + h * t) := by
    field_simp [hh.ne', ht.ne']
  rw [hu, ShapeFacesPGW.logDensity, hpow, hlog, log_inv, log_mul hh.ne' ht.ne',
    rpow_def_of_pos (by positivity : 0 < 1 + h * t), mul_comm (log (1 + h * t))]
  ring

private theorem chart_inverse {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {b a c : ℝ}
    (hb : 0 < b) (ha : 0 < a) (hc : 0 < c) :
    fixedShapeChart x b (c / a ^ b, c⁻¹) = ShapeFacesPGW.logLik x b a c := by
  have hab : 0 < a ^ b := rpow_pos_of_pos ha _
  rw [chart_physical hx hb (div_pos hc hab) (inv_pos.mpr hc), inv_inv]
  have he : c⁻¹ * (c / a ^ b) = (a ^ b)⁻¹ := by field_simp
  rw [he, ← rpow_neg ha.le, ← rpow_mul ha.le, show -(b : ℝ) * (-1 / b) = 1 by field_simp,
    rpow_one]

private theorem r_bound {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {b r h : ℝ}
    (hb : 0 < b) (hr : 0 < r) (hh : 0 < h) (i : Fin n) (M : ℝ)
    (hM : M ≤ fixedShapeChart x b (r, h)) :
    let B := (n : ℝ) * log b - ∑ j, log (x j) - M
    let v := (x i) ^ b
    r ∈ Icc (exp (-B) / v) (exp B * exp (exp (2 * B)) / v) ∧ h ≤ exp B := by
  dsimp only
  let B := (n : ℝ) * log b - ∑ j, log (x j) - M
  let a := (h * r) ^ (-1 / b)
  have ha : 0 < a := rpow_pos_of_pos (mul_pos hh hr) _
  let c := h⁻¹
  have hc : 0 < c := inv_pos.mpr hh
  let u := fun j ↦ c * log (1 + exp (b * log (x j / a)))
  let L := fun j ↦ ShapeFacesPGW.logDensity b a c (x j) - (log b - log (x j))
  have hL (j : Fin n) : L j ≤ 0 ∧ L j ≤ log (u j) ∧ L j ≤ -log (u j) ∧
      L j ≤ log c := density_bounds ha (hx j) hc
  have hbase : -B ≤ ∑ j, L j := by
    have hval := chart_physical hx hb hr hh
    rw [hval] at hM
    unfold L B ShapeFacesPGW.logLik at *
    simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    linarith
  have hsingle : (∑ j, L j) ≤ L i := by
    have hs := Finset.single_le_sum (f := fun j ↦ -L j)
      (fun j _ ↦ neg_nonneg.mpr (hL j).1) (Finset.mem_univ i)
    rw [Finset.sum_neg_distrib] at hs
    linarith
  have hhB : h ≤ exp B := by
    rw [← exp_log hh, exp_le_exp]
    have hbound := (hL i).2.2.2
    change L i ≤ log h⁻¹ at hbound
    rw [log_inv] at hbound
    linarith only [hbase, hsingle, hbound]
  let v := (x i) ^ b
  have hv : 0 < v := rpow_pos_of_pos (hx i) b
  let t := r * v
  have ht : 0 < t := mul_pos hr hv
  have hE : exp (b * log (x i / a)) = h * t := by
    have hA : a ^ b = (h * r)⁻¹ := by
      dsimp [a]
      rw [← rpow_mul (mul_pos hh hr).le,
        show -1 / b * b = -1 by field_simp, rpow_neg_one]
    rw [mul_comm, ← rpow_def_of_pos (div_pos (hx i) ha),
      div_rpow (hx i).le ha.le, hA, div_inv_eq_mul]
    dsimp [t, v]
    ring
  have hue : u i = h⁻¹ * log (1 + h * t) := by dsimp [u, c]; rw [hE]
  have hui : 0 < u i := by rw [hue]; exact mul_pos hc (log_pos (by linarith [mul_pos hh ht]))
  have huB : u i ≤ exp B := by
    rw [← exp_log hui, exp_le_exp]
    linarith only [hbase, hsingle, (hL i).2.2.1]
  have huLo : exp (-B) ≤ u i := by
    rw [← exp_log hui, exp_le_exp]
    linarith only [hbase, hsingle, (hL i).2.1]
  have hUT : u i ≤ t := by
    rw [hue, inv_mul_eq_div]
    apply (div_le_iff₀ hh).2
    linarith only [log_le_sub_one_of_pos (show 0 < 1 + h * t by positivity)]
  have hUexp : exp (h * u i) = 1 + h * t := by
    rw [hue, ← mul_assoc, mul_inv_cancel₀ hh.ne', one_mul, exp_log (by positivity)]
  have hT : t ≤ u i * exp (h * u i) := by
    apply (le_of_mul_le_mul_left _ hh)
    have hbound := add_one_le_exp (-(h * u i))
    have hp := mul_le_mul_of_nonneg_left hbound (exp_pos (h * u i)).le
    rw [← exp_add, add_neg_cancel, exp_zero] at hp
    nlinarith only [hp, hUexp]
  have hT' : t ≤ exp B * exp (exp (2 * B)) := by
    apply hT.trans
    apply mul_le_mul huB _ (exp_pos _).le (exp_pos _).le
    apply exp_le_exp.mpr
    rw [show 2 * B = B + B by ring, exp_add]
    exact mul_le_mul hhB huB hui.le (exp_pos _).le
  exact ⟨⟨(div_le_iff₀ hv).2 (huLo.trans hUT), (le_div_iff₀ hv).2 hT'⟩, hhB⟩

/-- With shape held, a finite fit attains exactly when some finite witness
ties or beats every Gompertz likelihood. The witness need not beat the
face strictly. -/
theorem held_shape_attains_iff_face {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ a > 0, ∃ c > 0, ∀ r > 0, gompertzLogLik x b r ≤ ShapeFacesPGW.logLik x b a c := by
  constructor
  · rintro ⟨⟨a, c⟩, ⟨ha, hc⟩, hm⟩
    refine ⟨a, ha, c, hc, fun r hr ↦ le_of_tendsto (gompertz_path hb hr hx) ?_⟩
    filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
    exact hm (show ((h * r) ^ (-1 / b), h⁻¹) ∈ Weibull.domain from
      ⟨rpow_pos_of_pos (mul_pos hh hr) _, inv_pos.mpr hh⟩)
  · rintro ⟨a, ha, c, hc, hw⟩
    let f := fixedShapeChart x b
    let M := ShapeFacesPGW.logLik x b a c
    let B := (n : ℝ) * log b - ∑ j, log (x j) - M
    let i : Fin n := ⟨0, hn⟩
    let v := (x i) ^ b
    let K : Set (ℝ × ℝ) :=
      Icc (exp (-B) / v) (exp B * exp (exp (2 * B)) / v) ×ˢ Icc 0 (exp B)
    have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
    have hKD : K ⊆ chartDomain := fun _ hp ↦
      ⟨(div_pos (exp_pos _) (rpow_pos_of_pos (hx i) _)).trans_le hp.1.1, hp.2.1⟩
    let w := (c / a ^ b, c⁻¹)
    have hw₁ : 0 < w.1 := div_pos hc (rpow_pos_of_pos ha _)
    have hw₂ : 0 < w.2 := inv_pos.mpr hc
    have he : f w = M := chart_inverse hx hb ha hc
    have hmem {p : ℝ × ℝ} (hp : p ∈ Weibull.domain) (hM : M ≤ f p) : p ∈ K := by
      have h := r_bound hx hb hp.1 hp.2 i M hM
      exact ⟨h.1, (show 0 < p.2 from hp.2).le, h.2⟩
    have hwK : w ∈ K := hmem ⟨hw₁, hw₂⟩ he.ge
    obtain ⟨p, hp, hm⟩ := hK.exists_isMaxOn ⟨w, hwK⟩ ((chart_cont hx b).mono hKD)
    have hp₁ : 0 < p.1 := (hKD hp).1
    have hp₂ : 0 ≤ p.2 := (hKD hp).2
    have hglobal {q : ℝ × ℝ} (hq : q ∈ Weibull.domain) : f q ≤ f p := by
      by_cases h : M ≤ f q
      · exact isMaxOn_iff.mp hm q (hmem hq h)
      · exact (lt_of_not_ge h).le.trans (he ▸ isMaxOn_iff.mp hm w hwK)
    have hout (q : ℝ × ℝ) (hq : q ∈ Weibull.domain) :
        ShapeFacesPGW.logLik x b q.1 q.2 ≤ f p := by
      have hq₁ : 0 < q.1 := hq.1
      have hq₂ : 0 < q.2 := hq.2
      rw [← chart_inverse hx hb hq₁ hq₂]
      exact hglobal ⟨div_pos hq₂ (rpow_pos_of_pos hq₁ _), inv_pos.mpr hq₂⟩
    rcases hp₂.eq_or_lt with h | h
    · have hface : f p ≤ M := by
        rcases p with ⟨r, h'⟩
        change 0 = h' at h
        subst h'
        rw [show f (r, 0) = gompertzLogLik x b r from chart_endpoint x b r]
        exact hw r hp₁
      exact ⟨(a, c), ⟨ha, hc⟩, isMaxOn_iff.mpr fun q hq ↦ (hout q hq).trans hface⟩
    · let q := ((p.2 * p.1) ^ (-1 / b), p.2⁻¹)
      refine ⟨q, ⟨rpow_pos_of_pos (mul_pos h hp₁) _, inv_pos.mpr h⟩, ?_⟩
      apply isMaxOn_iff.mpr
      intro z hz
      exact (hout z hz).trans_eq (chart_physical hx hb hp₁ h)

set_option maxHeartbeats 800000 in
/-- The held-shape criterion can be tested at finite stationary points.
The nonstrict face comparison preserves finite equality. -/
theorem held_shape_attains_iff_stationary {x : Fin n → ℝ} (hn : 0 < n)
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ a > 0, ∃ c > 0,
        deriv (fun s ↦ ShapeFacesPGW.logLik x b s c) a = 0 ∧
        deriv (ShapeFacesPGW.logLik x b a) c = 0 ∧
        ∀ r > 0, gompertzLogLik x b r ≤ ShapeFacesPGW.logLik x b a c := by
  constructor
  · rintro ⟨⟨a, c⟩, ⟨ha, hc⟩, hm⟩
    have h₁ : IsMaxOn (fun s ↦ ShapeFacesPGW.logLik x b s c) (Ioi 0) a :=
      isMaxOn_iff.mpr (fun s hs ↦ isMaxOn_iff.mp hm (s,c)
        (show (s,c) ∈ Weibull.domain from ⟨hs,hc⟩))
    have h₂ : IsMaxOn (ShapeFacesPGW.logLik x b a) (Ioi 0) c :=
      isMaxOn_iff.mpr (fun s hs ↦ isMaxOn_iff.mp hm (a,s)
        (show (a,s) ∈ Weibull.domain from ⟨ha,hs⟩))
    refine ⟨a, ha, c, hc, (h₁.isLocalMax (Ioi_mem_nhds ha)).deriv_eq_zero,
      (h₂.isLocalMax (Ioi_mem_nhds hc)).deriv_eq_zero, fun r hr ↦ ?_⟩
    apply le_of_tendsto (gompertz_path hb hr hx)
    filter_upwards [self_mem_nhdsWithin] with t (ht : 0 < t)
    exact isMaxOn_iff.mp hm _ (show ((t * r) ^ (-1 / b), t⁻¹) ∈ Weibull.domain from
      ⟨rpow_pos_of_pos (mul_pos ht hr) _, inv_pos.mpr ht⟩)
  · rintro ⟨a, ha, c, hc, _, _, h⟩
    exact (held_shape_attains_iff_face hn hx hb).mpr ⟨a, ha, c, hc, h⟩

private def faceObjective (y : Fin n → ℝ) (r : ℝ) : ℝ :=
  (n : ℝ) * log r + ∑ i, (r * y i + 1 - exp (r * y i))

private theorem face_unique (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i) {j : Fin n}
    (hj : 0 < y j) :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (faceObjective y) (Ioi 0) r := by
  have hn : (0 : ℝ) < n := by exact_mod_cast j.isLt.pos
  have hconc : StrictConcaveOn ℝ (Ioi 0) (faceObjective y) := by
    refine ⟨convex_Ioi 0, fun r hr s hs hrs a b ha hb hab ↦ ?_⟩
    have hl := strictConcaveOn_log_Ioi.2 hr hs hrs ha hb hab
    simp only [smul_eq_mul] at hl
    have hterm (i : Fin n) :
        a * (r * y i + 1 - exp (r * y i)) + b * (s * y i + 1 - exp (s * y i)) ≤
          (a*r+b*s) * y i + 1 - exp ((a*r+b*s) * y i) := by
      have he := convexOn_exp.2 (mem_univ (r*y i)) (mem_univ (s*y i)) ha.le hb.le hab
      simp only [smul_eq_mul] at he
      rw [show (a*r+b*s) * y i = a*(r*y i)+b*(s*y i) by ring]
      linarith only [he, hab]
    have hsum := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hterm i)
    simp only [Finset.sum_add_distrib, ← Finset.mul_sum] at hsum
    simp only [faceObjective, smul_eq_mul]
    linarith only [hsum, mul_lt_mul_of_pos_left hl hn]
  have hfen (r : ℝ) : faceObjective y r ≤ (n : ℝ) * log r := by
    unfold faceObjective
    have h := Finset.sum_nonpos (s := Finset.univ)
      (fun i _ ↦ sub_nonpos.mpr (add_one_le_exp (r * y i)))
    linarith
  have hsingle (r : ℝ) (hr : 0 < r) :
      faceObjective y r ≤ (n : ℝ) * log r + 1 - r * y j := by
    let f := fun i ↦ r * y i + 1 - exp (r * y i)
    have h := Finset.single_le_sum (f := fun i ↦ -f i)
      (fun i _ ↦ neg_nonneg.mpr (sub_nonpos.mpr (add_one_le_exp (r * y i))))
      (Finset.mem_univ j)
    rw [Finset.sum_neg_distrib] at h
    have he := quadratic_le_exp_of_nonneg (mul_nonneg hr.le (hy j))
    have h2 : 2 * (r * y j) ≤ exp (r * y j) := by
      nlinarith only [he, sq_nonneg (r * y j - 1)]
    unfold faceObjective
    dsimp [f] at h
    linarith only [h, h2]
  apply Generic.existsUnique_isMaxOn_of_isOpen_of_strictConcaveOn_of_tendsto_cocompact
    (nonempty_Ioi (a := (0 : ℝ))) isOpen_Ioi hconc
  apply tendsto_atBot.2
  intro M
  let d := y j / (2 * (n : ℝ))
  have hd : 0 < d := div_pos hj (by positivity)
  let l := exp (M / n)
  let u := (1 - M - n * (log d + 1)) / (y j / 2)
  have hsub : Icc l u ⊆ Ioi (0 : ℝ) := fun _ hp ↦ (exp_pos _).trans_le hp.1
  have hK : IsCompact (Subtype.val ⁻¹' Icc l u : Set (Ioi (0 : ℝ))) :=
    Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage' isCompact_Icc
      (by simpa only [Subtype.range_coe] using hsub)
  have hev : ∀ᶠ r : Ioi (0 : ℝ) in cocompact (Ioi (0 : ℝ)),
      r ∈ (Subtype.val ⁻¹' Icc l u : Set (Ioi (0 : ℝ)))ᶜ :=
    hK.compl_mem_cocompact
  apply hev.mono
  intro r hr
  by_contra! h
  have hr' : 0 < (r : ℝ) := r.property
  apply hr
  change (r : ℝ) ∈ Icc l u
  refine ⟨?_, ?_⟩
  · rw [← exp_log hr', exp_le_exp]
    exact (div_le_iff₀ hn).2 (by linarith [hfen (r : ℝ)])
  · have hlog := log_le_sub_one_of_pos (mul_pos hd hr')
    rw [log_mul hd.ne' hr'.ne'] at hlog
    have hprod : (n : ℝ) * (d * (r : ℝ)) = y j * (r : ℝ) / 2 := by
      dsimp [d]
      field_simp
    apply (le_div_iff₀ (half_pos hj)).2
    nlinarith only [h, hsingle (r : ℝ) hr', hprod, mul_le_mul_of_nonneg_left hlog hn.le]

/-- The threshold face has a unique positive exponent whenever at least
one observation lies strictly above the held threshold. -/
theorem threshold_unique {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    (hx : ∀ i, a ≤ x i) (hne : ∃ i, a < x i) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (thresholdLogLik x a) (Ioi 0) k := by
  let y := fun i ↦ log (x i / a)
  obtain ⟨i, hi⟩ := hne
  have hy (i : Fin n) : 0 ≤ y i := log_nonneg ((one_le_div ha).2 (hx i))
  have hyi : 0 < y i := log_pos ((one_lt_div ha).2 hi)
  let C := ∑ i, (-log (x i) - (if x i = a then log 2 else 0))
  have hform (k : ℝ) : thresholdLogLik x a k = faceObjective y k + C := by
    simp only [thresholdLogLik, faceObjective, y, C, Finset.sum_add_distrib,
      Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
      Finset.sum_neg_distrib]
    ring
  apply (existsUnique_congr (fun k ↦ and_congr_right fun _ ↦ ?_)).mp (face_unique y hy hyi)
  simp only [isMaxOn_iff, hform, add_le_add_iff_right]

/-- The power-transformed Gompertz face has a unique optimizing scale at
each positive held shape, even on constant data. -/
theorem gompertz_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (gompertzLogLik x b) (Ioi 0) r := by
  let y : Fin n → ℝ := fun i ↦ (x i) ^ b
  have hy (i : Fin n) : 0 < y i := rpow_pos_of_pos (hx i) _
  let C := ∑ i, (log b - log (x i) + log (y i))
  have hform (r : ℝ) (hr : 0 < r) : gompertzLogLik x b r = faceObjective y r + C := by
    simp only [gompertzLogLik, faceObjective, C, ← show ∀ i, y i = (x i)^b from fun _ ↦ rfl,
      log_mul hr.ne' (hy _).ne', Finset.sum_add_distrib, Finset.sum_sub_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    ring
  apply (existsUnique_congr (fun r ↦ and_congr_right fun hr ↦ ?_)).mp
    (face_unique y (fun i ↦ (hy i).le) (hy ⟨0, hn⟩))
  simp only [isMaxOn_iff]
  exact ⟨fun h s hs ↦ by rw [hform s hs, hform r hr]; linarith [h s hs],
    fun h s hs ↦ by have h' := h s hs; rw [hform s hs, hform r hr] at h'; linarith⟩

/-- The optimized threshold likelihood is the strict all-free-in-shape-and-
exponent supremum when the fixed scale is at or below the minimum and some
observation is strictly above the scale. -/
theorem held_scale_threshold_supremum (hn : 0 < n) {x : Fin n → ℝ} {a : ℝ}
    (ha : 0 < a) (hx : ∀ i, a ≤ x i) (hne : ∃ i, a < x i) :
    ∃ k > 0,
      IsLUB ((fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x p.1 a p.2) '' Weibull.domain)
        (thresholdLogLik x a k) ∧
      ∀ p ∈ Weibull.domain, ShapeFacesPGW.logLik x p.1 a p.2 < thresholdLogLik x a k := by
  obtain ⟨k, ⟨hk, hm⟩, _⟩ := threshold_unique ha hx hne
  have hstrict (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) :
      ShapeFacesPGW.logLik x p.1 a p.2 < thresholdLogLik x a k := by
    have hb : 0 < p.1 := hp.1
    have hc : 0 < p.2 := hp.2
    let t := p.1 * p.2
    have ht : 0 < t := mul_pos hb hc
    have he : t / p.1 = p.2 := by dsimp [t]; field_simp
    apply lt_of_lt_of_le _ (isMaxOn_iff.mp hm t ht)
    rw [← he]
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      (fun i _ ↦ held_scale_logDensity_lt_threshold ha (hx i) hb ht)
  refine ⟨k, hk, ⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hstrict p hp).le
  · intro v hv
    apply le_of_tendsto (held_scale_threshold_path ha hk hx)
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    exact hv ⟨(t, k / t), ⟨ht, div_pos hk ht⟩, rfl⟩

/-- The cumulative hazard on the fixed-shape Gompertz path converges at
every positive observation. -/
theorem gompertz_hazard_path {b r x : ℝ} (hb : 0 < b) (hr : 0 < r) (hx : 0 < x) :
    Tendsto (fun h : ℝ ↦ (1 + (x / (h * r) ^ (-1 / b)) ^ b) ^ h⁻¹ - 1)
      (𝓝[>] 0) (𝓝 (exp (r * x ^ b) - 1)) := by
  let t := r * x ^ b
  have hd : HasDerivAt (fun h : ℝ ↦ log (1 + h * t)) t 0 := by
    simpa using (((hasDerivAt_id (0 : ℝ)).mul_const t).const_add 1).log
      (by simp : 1 + id (0 : ℝ) * t ≠ 0)
  have hu : Tendsto (fun h : ℝ ↦ h⁻¹ * log (1 + h * t)) (𝓝[>] 0) (𝓝 t) := by
    simpa using hd.tendsto_slope_zero_right
  apply ((Real.continuous_exp.continuousAt.tendsto.comp hu).sub_const 1).congr'
  filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
  have ha := rpow_pos_of_pos (mul_pos hh hr) (-1 / b)
  have hp : ((h * r) ^ (-1 / b)) ^ b = (h * r)⁻¹ := by
    rw [← rpow_mul (mul_pos hh hr).le, show -1 / b * b = -1 by field_simp, rpow_neg_one]
  have hratio : (x / (h * r) ^ (-1 / b)) ^ b = h * t := by
    rw [div_rpow hx.le ha.le, hp, div_inv_eq_mul]
    dsimp [t]
    ring
  rw [hratio, rpow_def_of_pos (by positivity : 0 < 1 + h * t)]
  simp only [Function.comp_apply, mul_comm]

/-- With scale free, approaching the threshold from below at log distance
`h` while shape grows as `h⁻²` attains the unpenalized endpoint likelihood. -/
theorem free_threshold_path {x : Fin n → ℝ} {a k : ℝ} (ha : 0 < a)
    (hk : 0 < k) (hx : ∀ i, a ≤ x i) :
    Tendsto (fun h : ℝ ↦ ShapeFacesPGW.logLik x (h^2)⁻¹ (a * exp (-h)) (k * h^2))
      (𝓝[>] 0) (𝓝 (∑ i, (log k - log (x i) + k * log (x i / a) -
        (exp (k * log (x i / a)) - 1)))) := by
  unfold ShapeFacesPGW.logLik
  apply tendsto_finsetSum
  intro i _
  let d := log (x i / a)
  have hd : 0 ≤ d := log_nonneg ((one_le_div ha).2 (hx i))
  have hi : 0 < x i := ha.trans_le (hx i)
  have h0 : Tendsto (fun h : ℝ ↦ h) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hlin : Tendsto (fun h : ℝ ↦ (h^2)⁻¹ * (d + h)) (𝓝[>] 0) atTop := by
    apply tendsto_atTop.mpr
    intro B
    filter_upwards [self_mem_nhdsWithin, (tendsto_inv_nhdsGT_zero (𝕜 := ℝ)).eventually
      (eventually_ge_atTop B)] with h (hh : 0 < h) hB
    apply hB.trans
    have hs : 0 < h^2 := sq_pos_of_pos hh
    apply (le_inv_mul_iff₀ hs).2
    calc
      h^2 * h⁻¹ = h := by field_simp
      _ ≤ d + h := by linarith
  have he := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp hlin)
  have hs : Tendsto (fun h : ℝ ↦ 1 + exp (-((h^2)⁻¹ * (d + h)))) (𝓝[>] 0) (𝓝 1) := by
    simpa [Function.comp_def] using he.const_add 1
  have hc : Tendsto (fun h : ℝ ↦ correction ((h^2)⁻¹ * (d + h))) (𝓝[>] 0) (𝓝 0) := by
    simpa [correction] using hs.log one_ne_zero
  have hv : Tendsto (fun h : ℝ ↦ k * (d + h) +
      (k * h^2) * correction ((h^2)⁻¹ * (d + h))) (𝓝[>] 0) (𝓝 (k * d)) := by
    simpa using ((h0.const_add d).const_mul k).add (((h0.pow 2).const_mul k).mul hc)
  have hf := ((tendsto_const_nhds (x := log k - log (x i))).sub hc).add hv
  have hlim := hf.sub ((Real.continuous_exp.continuousAt.tendsto.comp hv).sub_const 1)
  simp only [sub_zero] at hlim
  refine hlim.congr' ?_
  filter_upwards [self_mem_nhdsWithin] with h (hh : 0 < h)
  have ha' : 0 < a * exp (-h) := mul_pos ha (exp_pos _)
  have hle : a * exp (-h) ≤ x i :=
    (mul_le_of_le_one_right ha.le (exp_le_one_iff.mpr (neg_nonpos.mpr hh.le))).trans (hx i)
  have hb : 0 < (h^2)⁻¹ := inv_pos.mpr (sq_pos_of_pos hh)
  have hscale : log (x i / (a * exp (-h))) = d + h := by
    rw [log_div hi.ne' ha'.ne', log_mul ha.ne' (exp_pos _).ne', log_exp]
    dsimp [d]
    rw [log_div hi.ne' ha.ne']
    ring
  rw [← show k / (h^2)⁻¹ = k * h^2 by simp [div_eq_mul_inv],
    corrected_density ha' hle hb hk, hscale]
  simp [div_eq_mul_inv]

end
end UnivariateML.AllFreePGW

end
