module

public import UnivariateML.Family.Shape2InvTrGamma
public import UnivariateML.Family.Shape3GenGamma

set_option linter.style.header false

/-! # Held-scale traces for the transformed-gamma pair

The forward and inverse profiles use one oriented log-excess chart.
The held-scale comparison uses a power profile with the gamma shape
optimized. The tail bounds include the contact penalty for a supported
trace and the exponential loss for a sample outside that support.

## Deviations

The all-free separation result concerns finite scores at three distinct
levels. Almost-everywhere uniqueness and all-free face exhaustion require
further results.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.Shape3TrGamma
noncomputable section

/-- With scale fixed, every supported power path has a finite improvement,
including at ties with the scale. Constants at or above scale diverge. -/
theorem held_scale_trace_and_concentration {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∀ v ≥ a, (∀ i, x i = v) →
      Tendsto (fun p ↦ TransformedGamma.logLik x (exp (p * (log v - log a))) p a)
        atTop atTop) ∧
    ((∀ i, x i ≤ a) → ∀ d > 0,
      let B := log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)
      Tendsto (fun q : ℝ ↦ TransformedGamma.logLik x (d * q) q⁻¹ a) (𝓝[>] 0) (𝓝 B) ∧
      ∃ k > 0, ∃ p > 0, B < TransformedGamma.logLik x k p a) := by
  constructor
  · intro v hv he
    have he' : x = fun _ ↦ v := funext he
    simpa only [he'] using Shape2TrGamma.held_scale_constant_concentration (n := n) hn ha hv
  · intro hle d hd
    exact ⟨Shape2TrGamma.held_scale_power_path ha hd hx hle,
      Shape2TrGamma.held_scale_power_trace_beaten ha hd hx hle⟩

/-- The reciprocal held-scale result has the same finite improvement of
every supported Pareto trace. Its concentration branch reverses the order
between the common observation and the held scale. -/
theorem inverse_held_scale_trace_and_concentration {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∀ v ≤ a, (∀ i, x i = v) →
      Tendsto (fun p ↦ InverseTransformedGamma.logLik x (exp (p * (log a - log v))) p a)
        atTop atTop) ∧
    ((∀ i, a ≤ x i) → ∀ d > 0,
      let B := log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0)
      Tendsto (fun q : ℝ ↦ InverseTransformedGamma.logLik x (d * q) q⁻¹ a)
        (𝓝[>] 0) (𝓝 B) ∧
      ∃ k > 0, ∃ p > 0, B < InverseTransformedGamma.logLik x k p a) := by
  constructor
  · intro v hv he
    have he' : x = fun _ ↦ v := funext he
    have hvpos : 0 < v := by rw [← he ⟨0, hn⟩]; exact hx ⟨0, hn⟩
    simpa only [he'] using Shape2InvTrGamma.held_scale_constant_concentration (n := n) hn hvpos hv
  · intro hle d hd
    exact ⟨Shape2InvTrGamma.held_scale_pareto_path ha hd hle,
      Shape2InvTrGamma.held_scale_pareto_trace_beaten ha hd hle⟩

private theorem oriented_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    (z : Fin n → ℝ) (J : ℝ) {k p : ι → ℝ} {d : ℝ}
    (hk : Tendsto k l (𝓝[>] 0)) (hkp : Tendsto (fun j ↦ k j * p j) l (𝓝 d))
    (hd : 0 < d) (hz : ∀ i, z i ≤ 0) :
    Tendsto (fun j ↦ log (p j) - log (Gamma (k j)) - J -
      Gamma.avg (fun i ↦ TransformedGamma.loss (k j) (p j * z i))) l
      (𝓝 (log d - J + d * Gamma.avg z - Gamma.avg (fun i ↦ if z i = 0 then 1 else 0))) := by
  have hp : Tendsto p l atTop := by
    refine tendsto_atTop_mono' _ ?_
      ((tendsto_inv_nhdsGT_zero.comp hk).const_mul_atTop (half_pos hd))
    filter_upwards [hk.eventually self_mem_nhdsWithin,
      hkp.eventually (Ioi_mem_nhds (half_lt_self hd))] with j hj hP
    exact (div_le_iff₀ hj).2 (by simpa [mul_comm] using hP.le)
  have hg : Tendsto (fun j ↦ log (Gamma (k j + 1))) l (𝓝 0) := by
    have h0 : Tendsto k l (𝓝 0) := hk.mono_right nhdsWithin_le_nhds
    have h1 : Tendsto (fun j ↦ k j + 1) l (𝓝 1) := by simpa using h0.add_const 1
    have hc := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp h1
    simpa [Function.comp_def] using hc.log (by simp)
  have hf (i : Fin n) : Tendsto (fun j ↦ exp (p j * z i)) l
      (𝓝 (if z i = 0 then 1 else 0)) := by
    by_cases h : z i = 0
    · simpa only [h, mul_zero, exp_zero, ite_true] using
        (tendsto_const_nhds : Tendsto (fun _ : ι ↦ (1 : ℝ)) l (𝓝 1))
    · have h' := lt_of_le_of_ne (hz i) h
      simpa only [Function.comp_def, mul_comm, ite_eq_right h] using
        tendsto_exp_atBot.comp (hp.const_mul_atTop_of_neg h')
  have hF : Tendsto (fun j ↦ Gamma.avg (fun i ↦ exp (p j * z i))) l
      (𝓝 (Gamma.avg (fun i ↦ if z i = 0 then 1 else 0))) := by
    unfold Gamma.avg
    exact (tendsto_finsetSum Finset.univ (fun i _ ↦ hf i)).div_const _
  have hlim := ((((hkp.log hd.ne').sub hg).sub_const J).add
    (hkp.mul_const (Gamma.avg z))).sub hF
  simp only [sub_zero] at hlim
  refine hlim.congr' ?_
  filter_upwards [hk.eventually self_mem_nhdsWithin, hp.eventually (eventually_gt_atTop (0 : ℝ))]
    with j hj hjp
  have hΓ : log (Gamma (k j + 1)) = log (k j) + log (Gamma (k j)) := by
    rw [Gamma_add_one hj.ne', log_mul hj.ne' (Gamma_pos_of_pos hj).ne']
  simp only [hΓ, log_mul hj.ne' hjp.ne', Gamma.avg, TransformedGamma.loss,
    Finset.sum_sub_distrib, mul_assoc, ← Finset.mul_sum]
  ring

/-- For every moving positive rate with finite positive limit, the
held-scale transformed-gamma trace has its contact penalty. -/
theorem held_scale_varying_rate_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    {k p : ι → ℝ} {d : ℝ} (hd : 0 < d)
    (hk : Tendsto k l (𝓝[>] 0)) (hkp : Tendsto (fun j ↦ k j * p j) l (𝓝 d))
    (hle : ∀ i, x i ≤ a) :
    Tendsto (fun j ↦ TransformedGamma.logLik x (k j) (p j) a) l
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log (x i) - log a) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0))) := by
  have hc (i : Fin n) : log (x i) - log a = 0 ↔ x i = a := by
    rw [sub_eq_zero]
    exact ⟨log_injOn_pos (hx i) ha, congrArg log⟩
  simpa only [TransformedGamma.logLik, hc] using
    oriented_trace (fun i ↦ log (x i) - log a) (Gamma.avgLog x) hk hkp hd
      (fun i ↦ sub_nonpos.mpr (log_le_log (hx i) (hle i)))

/-- Reversing the log-excess orientation gives the varying-rate trace for
held-scale inverse transformed gamma, with the same contact penalty. -/
theorem inverse_held_scale_varying_rate_trace {ι : Type*} {l : Filter ι} {n : ℕ}
    {x : Fin n → ℝ} {a : ℝ} (ha : 0 < a)
    {k p : ι → ℝ} {d : ℝ} (hd : 0 < d)
    (hk : Tendsto k l (𝓝[>] 0)) (hkp : Tendsto (fun j ↦ k j * p j) l (𝓝 d))
    (hle : ∀ i, a ≤ x i) :
    Tendsto (fun j ↦ InverseTransformedGamma.logLik x (k j) (p j) a) l
      (𝓝 (log d - Gamma.avgLog x + d * Gamma.avg (fun i ↦ log a - log (x i)) -
        Gamma.avg (fun i ↦ if x i = a then 1 else 0))) := by
  have hc (i : Fin n) : log a - log (x i) = 0 ↔ x i = a := by
    rw [sub_eq_zero, eq_comm]
    exact ⟨log_injOn_pos (ha.trans_le (hle i)) ha, congrArg log⟩
  simpa only [InverseTransformedGamma.logLik, hc] using
    oriented_trace (fun i ↦ log a - log (x i)) (Gamma.avgLog x) hk hkp hd
      (fun i ↦ sub_nonpos.mpr (log_le_log ha (hle i)))

private def chart {n : ℕ} (z : Fin n → ℝ) (J : ℝ) (k p : ℝ) : ℝ :=
  log p - log (Gamma k) - J -
    Gamma.avg (fun i ↦ TransformedGamma.loss k (p * z i))

private theorem chart_eq {n : ℕ} (z : Fin n → ℝ) (J k p : ℝ) :
    chart z J k p = log p - J + k * (p * Gamma.avg z) - log (Gamma k) -
      Gamma.avg (fun i ↦ exp (p * z i)) := by
  simp only [chart, TransformedGamma.loss, Gamma.avg, Finset.sum_sub_distrib,
    ← Finset.mul_sum]
  ring

private theorem lg_step {k : ℝ} (hk : 0 < k) :
    log (Gamma (k + 1)) = log k + log (Gamma k) := by
  rw [Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne']

private theorem lg_deriv {k : ℝ} (hk : 0 < k) :
    HasDerivAt (log ∘ Gamma) (UnivariateML.Gamma.digamma k) k := by
  have h := (Real.hasDerivAt_Gamma_of_pos hk).log (Gamma_pos_of_pos hk).ne'
  unfold UnivariateML.Gamma.digamma
  exact h.deriv ▸ h

private theorem lg_linear_lower {k : ℝ} (hk : 0 < k) :
    -2 * log (Gamma (1 / 2)) * k - log k ≤ log (Gamma k) := by
  have h := convexOn_log_Gamma.slope_mono_adjacent (show (1 / 2 : ℝ) ∈ Ioi 0 by norm_num)
    (show 0 < k + 1 by positivity) (show (1 / 2 : ℝ) < 1 by norm_num)
    (show 1 < k + 1 by linarith)
  simp only [Function.comp_apply, Gamma_one, log_one, zero_sub,
    sub_zero, add_sub_cancel_right] at h
  have h' := (le_div_iff₀ hk).1 h
  rw [lg_step hk] at h'
  linarith only [h',
    show (-(log (Gamma (1 / 2))) / (1 - 1 / 2)) * k =
      -2 * log (Gamma (1 / 2)) * k by ring]

private theorem digamma_bounds {k : ℝ} (hk : 0 < k) :
    UnivariateML.Gamma.digamma k ≤ log k ∧
      (1 < k → log (k - 1) ≤ UnivariateML.Gamma.digamma k) := by
  constructor
  · have h := convexOn_log_Gamma.deriv_le_slope hk (show 0 < k + 1 by positivity)
      (lt_add_one k) (lg_deriv hk).differentiableAt
    simpa only [UnivariateML.Gamma.digamma, slope_def_field, Function.comp_apply, lg_step hk,
      show k + 1 - k = 1 by ring, div_one, add_sub_cancel_right] using h
  · intro h1
    have hk' : 0 < k - 1 := sub_pos.mpr h1
    have h := convexOn_log_Gamma.slope_le_deriv hk' hk (sub_lt_self _ one_pos)
      (lg_deriv hk).differentiableAt
    have h' := lg_step hk'
    simp only [sub_add_cancel] at h'
    simpa only [UnivariateML.Gamma.digamma, slope_def_field, Function.comp_apply,
      show k - (k - 1) = 1 by ring, div_one, h', add_sub_cancel_right] using h

private theorem avg_const {n : ℕ} (hn : 0 < n) (c : ℝ) :
    Gamma.avg (fun _ : Fin n ↦ c) = c := by
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [Gamma.avg, nsmul_eq_mul, hnR]

private theorem optimizes_shape {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) (J p : ℝ) :
    ∃ k > 0, IsMaxOn (fun t ↦ chart z J t p) (Ioi 0) k ∧
      exp (p * Gamma.avg z) ≤ k ∧ k ≤ exp (p * Gamma.avg z) + 1 := by
  let m := p * Gamma.avg z
  let x : Fin n → ℝ := fun _ ↦ exp m
  obtain ⟨k, ⟨hk, hm⟩, _⟩ :=
    UnivariateML.Gamma.heldRate x (β := 1) one_pos
  have he (t : ℝ) : chart z J t p = UnivariateML.Gamma.logLik x t 1 +
      (log p - J + m + exp m - Gamma.avg (fun i ↦ exp (p * z i))) := by
    rw [chart_eq, UnivariateML.Gamma.logLik]
    simp only [log_one, mul_zero, zero_sub, Gamma.avgLog, x, log_exp, avg_const hn, one_mul]
    dsimp [m]
    ring
  have hmax : IsMaxOn (fun t ↦ chart z J t p) (Ioi 0) k :=
    fun t ht ↦ by
      change chart z J t p ≤ chart z J k p
      rw [he t, he k]
      exact add_le_add_left (hm ht) _
  have hder : HasDerivAt (fun t ↦ chart z J t p) (m - UnivariateML.Gamma.digamma k) k := by
    have hh := (((hasDerivAt_id k).mul_const m).sub (lg_deriv hk)).const_add
      (log p - J - Gamma.avg (fun i ↦ exp (p * z i)))
    convert hh using 1
    · funext t
      rw [chart_eq]
      dsimp [m, Function.comp_def]
      ring
    · simp
  have hs := (hmax.isLocalMax (Ioi_mem_nhds hk)).hasDerivAt_eq_zero hder
  have hscore := sub_eq_zero.mp hs
  have ⟨lo, hi⟩ := digamma_bounds hk
  refine ⟨k, hk, hmax, ?_, ?_⟩
  · rw [← exp_log hk, exp_le_exp]
    exact hscore.le.trans lo
  · by_cases h : 1 < k
    · have he' := (hi h).trans hscore.ge
      have hk' : 0 < k - 1 := sub_pos.mpr h
      have hle : k - 1 ≤ exp m := by
        rw [← exp_log hk', exp_le_exp]
        exact he'
      linarith
    · linarith [exp_pos m]

private theorem chart_continuous {n : ℕ} (z : Fin n → ℝ) (J : ℝ) :
    ContinuousOn (fun p : ℝ × ℝ ↦ chart z J p.1 p.2) Gamma.domain := by
  let D := Gamma.domain
  have hk : ContinuousOn (fun p : ℝ × ℝ ↦ p.1) D := continuous_fst.continuousOn
  have hp : ContinuousOn (fun p : ℝ × ℝ ↦ p.2) D := continuous_snd.continuousOn
  have hG : ContinuousOn (fun p : ℝ × ℝ ↦ log (Gamma p.1)) D := by
    apply ContinuousOn.log
    · exact differentiableOn_Gamma_Ioi.continuousOn.comp hk (fun p hp ↦ hp.1)
    · exact fun p hp ↦ (Gamma_pos_of_pos hp.1).ne'
  have hE : ContinuousOn (fun p : ℝ × ℝ ↦ Gamma.avg (fun i ↦ exp (p.2 * z i))) D := by
    unfold Gamma.avg
    exact (continuousOn_finsetSum _ (fun i _ ↦ (hp.mul_const (z i)).rexp)).div_const _
  have hlog := hp.log (fun p hp ↦ (show 0 < p.2 from hp.2).ne')
  have h := (((hlog.sub (continuousOn_const (c := J))).add
    (hk.mul (hp.mul_const (Gamma.avg z)))).sub hG).sub hE
  exact h.congr fun p _ ↦ chart_eq z J p.1 p.2

private theorem chart_nonneg_avg {n : ℕ} (f : Fin n → ℝ) (hf : ∀ i, 0 ≤ f i) :
    0 ≤ Gamma.avg f := div_nonneg (Finset.sum_nonneg fun i _ ↦ hf i) (Nat.cast_nonneg _)

private theorem chart_low_power {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) (J v : ℝ) :
    ∃ δ > 0, ∀ p ∈ Ioo 0 δ, ∀ k > 0, chart z J k p < v := by
  let m := Gamma.avg z
  let c := -2 * log (Gamma (1 / 2))
  let B := (|m| + |c| + 1) * (exp |m| + 1) - J
  obtain ⟨d, hd, hnear⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp
    ((tendsto_atBot_add_const_right _ B tendsto_log_nhdsGT_zero).eventually
      (eventually_lt_atBot v))
  refine ⟨min d 1, lt_min hd one_pos, fun p hp k hk ↦ ?_⟩
  obtain ⟨s, hs, hm, _, hS⟩ := optimizes_shape hn z J p
  have hp1 : p ≤ 1 := hp.2.le.trans (min_le_right _ _)
  have hpm : p * m ≤ |m| := by
    calc
      _ ≤ p * |m| := mul_le_mul_of_nonneg_left (le_abs_self _) hp.1.le
      _ ≤ _ := by nlinarith only [mul_le_mul_of_nonneg_right hp1 (abs_nonneg m)]
  have hs' : s ≤ exp |m| + 1 := hS.trans (add_le_add_left (exp_le_exp.mpr hpm) _)
  have hl : chart z J s p ≤ log p + B := by
    rw [chart_eq]
    dsimp [B]
    have hγ := lg_linear_lower hs
    have hlog := log_le_sub_one_of_pos hs
    have hC : 0 ≤ |m| + |c| + 1 := by positivity
    have h1 := mul_le_mul_of_nonneg_left hpm hs.le
    have h2 := mul_le_mul_of_nonneg_right (neg_le_abs c) hs.le
    have h3 := mul_le_mul_of_nonneg_left hs' hC
    have h4 := chart_nonneg_avg (fun i ↦ exp (p * z i)) (fun i ↦ (exp_pos _).le)
    change c * s - log s ≤ log (Gamma s) at hγ
    nlinarith only [hγ, hlog, h1, h2, h3, h4]
  exact (hm hk).trans_lt (hl.trans_lt (hnear ⟨hp.1, hp.2.trans_le (min_le_left _ _)⟩))

private theorem chart_supported_upper {n : ℕ} (z : Fin n → ℝ) (J : ℝ)
    (hm : Gamma.avg z < 0) :
    let B := -log (-Gamma.avg z) - 1 - J - Gamma.avg (fun i ↦ if z i = 0 then 1 else 0)
    ∀ v > B, ∃ K > 0, ∀ p ≥ K, ∀ k > 0, chart z J k p < v := by
  intro B v hv
  let m := Gamma.avg z
  let c := -2 * log (Gamma (1 / 2))
  let ρ := Gamma.avg (fun i ↦ if z i = 0 then 1 else 0)
  let R := fun p : ℝ ↦ -m + c / p
  have hR : Tendsto R atTop (𝓝 (-m)) := by
    simpa only [R, div_eq_mul_inv, mul_zero, add_zero] using
      (tendsto_inv_atTop_zero.const_mul c).const_add (-m)
  have hL : Tendsto (fun p ↦ -log (R p) - 1 - J - ρ) atTop (𝓝 B) :=
    (((hR.log (neg_pos.mpr hm).ne').neg.sub_const 1).sub_const J).sub_const ρ
  obtain ⟨K, hK⟩ := eventually_atTop.1 ((hR.eventually (Ioi_mem_nhds (neg_pos.mpr hm))).and
    (hL.eventually (Iio_mem_nhds hv)))
  refine ⟨max 1 K, zero_lt_one.trans_le (le_max_left _ _), ?_⟩
  intro p hp k hk
  have hp' : 0 < p := zero_lt_one.trans_le ((le_max_left _ _).trans hp)
  obtain ⟨hr, hval⟩ := hK p ((le_max_right _ _).trans hp)
  have hr' : 0 < R p := hr
  have hγ := lg_linear_lower hk
  have hρ : ρ ≤ Gamma.avg (fun i ↦ exp (p * z i)) := by
    unfold Gamma.avg
    apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg _)
    apply Finset.sum_le_sum
    intro i _
    by_cases h : z i = 0
    · simp [h]
    · simp [h, (exp_pos (p * z i)).le]
  have hlog := log_le_sub_one_of_pos (mul_pos (mul_pos hr' hp') hk)
  rw [log_mul (mul_pos hr' hp').ne' hk.ne', log_mul hr'.ne' hp'.ne'] at hlog
  have he : R p * p = -m * p + c := by
    dsimp [R]
    field_simp
  rw [he] at hlog
  apply lt_of_le_of_lt _ hval
  rw [chart_eq]
  change c * k - log k ≤ log (Gamma k) at hγ
  nlinarith only [hγ, hρ, hlog]

private theorem chart_unsupported_upper {n : ℕ} (hn : 0 < n) {z : Fin n → ℝ}
    {j : Fin n} (hz : 0 < z j) (hm : Gamma.avg z < z j) (J v : ℝ) :
    ∃ K > 0, ∀ p ≥ K, ∀ k > 0, chart z J k p < v := by
  let m := Gamma.avg z
  let c := -2 * log (Gamma (1 / 2))
  let B := |m| + |c| + 1
  let M := max m 0
  let δ := z j - M
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hB : 0 < B := by dsimp [B]; positivity
  have hM : 0 ≤ M := le_max_right _ _
  have hδ : 0 < δ := sub_pos.mpr (max_lt hm hz)
  let K := max 1 (max (8 * n * B / δ ^ 2) (max (8 * n / (z j) ^ 2) (1 - J - v)))
  refine ⟨K, zero_lt_one.trans_le (le_max_left _ _), ?_⟩
  intro p hp k hk
  have h1 : 1 ≤ p := (le_max_left _ _).trans hp
  have hp0 := zero_lt_one.trans_le h1
  have hTail := (le_max_right (1 : ℝ) _).trans hp
  have hBigδ : 8 * n * B / δ ^ 2 ≤ p := (le_max_left _ _).trans hTail
  have hBigz : 8 * n / (z j) ^ 2 ≤ p :=
    (le_max_left _ _).trans ((le_max_right _ _).trans hTail)
  have hBigp : 1 - J - v ≤ p := (le_max_right _ _).trans ((le_max_right _ _).trans hTail)
  obtain ⟨s, hs, hmax, _, hS⟩ := optimizes_shape hn z J p
  have hExp : s ≤ 2 * exp (p * M) := by
    have h := exp_le_exp.mpr (mul_le_mul_of_nonneg_left (le_max_left m 0) hp0.le)
    have h' := one_le_exp_iff.mpr (mul_nonneg hp0.le hM)
    linarith only [hS, h, h']
  have hγ := lg_linear_lower hs
  have hlog := log_le_sub_one_of_pos hs
  have hb : p * m ≤ p * |m| := mul_le_mul_of_nonneg_left (le_abs_self _) hp0.le
  have hC : -c ≤ p * |c| := (neg_le_abs c).trans (by
    nlinarith only [mul_le_mul_of_nonneg_right h1 (abs_nonneg c)])
  have hshape : s * (p * m) - log (Gamma s) ≤ (p * B) * (2 * exp (p * M)) := by
    change c * s - log s ≤ log (Gamma s) at hγ
    have hcB : p * m - c + 1 ≤ p * B := by dsimp [B]; linarith
    have hS' := mul_le_mul_of_nonneg_left hExp (mul_nonneg hp0.le hB.le)
    nlinarith only [hγ, hlog, hS', mul_le_mul_of_nonneg_left hcB hs.le]
  have hEd : 4 * n * p * B ≤ exp (p * δ) := by
    have hh := mul_le_mul_of_nonneg_right
      ((div_le_iff₀ (sq_pos_of_pos hδ)).1 hBigδ) hp0.le
    nlinarith only [quadratic_le_exp_of_nonneg (mul_nonneg hp0.le hδ.le), hh,
      mul_nonneg hp0.le hδ.le]
  have hprod : (p * B) * (2 * exp (p * M)) ≤ exp (p * z j) / (2 * n) := by
    apply (le_div_iff₀ (show (0 : ℝ) < 2 * n by positivity)).2
    have h := mul_le_mul_of_nonneg_right hEd (exp_pos (p * M)).le
    rw [← exp_add, show p * δ + p * M = p * z j by dsimp [δ]; ring] at h
    nlinarith only [h]
  have hz' : 2 * p ≤ exp (p * z j) / (2 * n) := by
    apply (le_div_iff₀ (show (0 : ℝ) < 2 * n by positivity)).2
    have hh := mul_le_mul_of_nonneg_right
      ((div_le_iff₀ (sq_pos_of_pos hz)).1 hBigz) hp0.le
    nlinarith only [quadratic_le_exp_of_nonneg (mul_nonneg hp0.le hz.le), hh,
      mul_nonneg hp0.le hz.le]
  have hE : exp (p * z j) / n ≤ Gamma.avg (fun i ↦ exp (p * z i)) := by
    unfold Gamma.avg
    exact div_le_div_of_nonneg_right
      (Finset.single_le_sum (f := fun i : Fin n ↦ exp (p * z i))
        (fun i _ ↦ (exp_pos _).le) (Finset.mem_univ j)) hnR.le
  have hh : chart z J s p < v := by
    rw [chart_eq]
    nlinarith only [hshape.trans hprod, hz', hE, log_le_sub_one_of_pos hp0, hBigp,
      show exp (p * z j) / (2 * n) = (exp (p * z j) / n) / 2 by ring]
  exact (hmax hk).trans_lt hh

private theorem chart_attains {n : ℕ} (hn : 0 < n) (z : Fin n → ℝ) (J : ℝ)
    (h : (∃ j, 0 < z j ∧ Gamma.avg z < z j) ∨
      (Gamma.avg z < 0 ∧ ∃ k₀ > 0, ∃ p₀ > 0,
        -log (-Gamma.avg z) - 1 - J - Gamma.avg (fun i ↦ if z i = 0 then 1 else 0) <
          chart z J k₀ p₀)) :
    ∃ p ∈ Gamma.domain, IsMaxOn (fun q : ℝ × ℝ ↦ chart z J q.1 q.2) Gamma.domain p := by
  let m := Gamma.avg z
  obtain ⟨k₀, hk₀, p₀, hp₀, hhi⟩ : ∃ k₀ > 0, ∃ p₀ > 0,
      ∃ K > 0, ∀ p ≥ K, ∀ k > 0, chart z J k p < chart z J k₀ p₀ := by
    rcases h with ⟨j, hj, hjm⟩ | ⟨hm, k₀, hk₀, p₀, hp₀, hv⟩
    · exact ⟨1, one_pos, 1, one_pos, chart_unsupported_upper hn hj hjm J _⟩
    · exact ⟨k₀, hk₀, p₀, hp₀, chart_supported_upper z J hm _ hv⟩
  let v := chart z J k₀ p₀
  obtain ⟨K, hK, hfar⟩ := hhi
  obtain ⟨δ, hδ, hnear⟩ := chart_low_power hn z J v
  let W := K * |m|
  let R := Icc (exp (-W)) (exp W + 1) ×ˢ Icc δ K
  have hR : R ⊆ Gamma.domain :=
    fun q hq ↦ ⟨(exp_pos _).trans_le hq.1.1, hδ.trans_le hq.2.1⟩
  have hlocal (p : ℝ) (hp : 0 < p) (hv : ∃ k > 0, v ≤ chart z J k p) :
      ∃ k, (k, p) ∈ R ∧ IsMaxOn (fun t ↦ chart z J t p) (Ioi 0) k := by
    obtain ⟨k₁, hk₁, hv₁⟩ := hv
    have hl : δ ≤ p := le_of_not_gt fun ht ↦ (hnear p ⟨hp, ht⟩ k₁ hk₁).not_ge hv₁
    have hh : p ≤ K := le_of_not_gt fun ht ↦ (hfar p ht.le k₁ hk₁).not_ge hv₁
    obtain ⟨k, hk, hmax, hlo, hup⟩ := optimizes_shape hn z J p
    have hw : |p * m| ≤ W := by
      rw [abs_mul, abs_of_pos hp]
      exact mul_le_mul_of_nonneg_right hh (abs_nonneg _)
    obtain ⟨hlo', hup'⟩ := abs_le.mp hw
    exact ⟨k, ⟨⟨(exp_le_exp.mpr hlo').trans hlo,
      hup.trans (add_le_add_left (exp_le_exp.mpr hup') _)⟩, hl, hh⟩, hmax⟩
  obtain ⟨k₁, hk₁, hm₁⟩ := hlocal p₀ hp₀ ⟨k₀, hk₀, le_rfl⟩
  obtain ⟨p, hp, hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn
    ⟨(k₁, p₀), hk₁⟩ ((chart_continuous z J).mono hR)
  have hv : v ≤ chart z J p.1 p.2 := (hm₁ hk₀).trans (hm hk₁)
  refine ⟨p, hR hp, fun q hq ↦ ?_⟩
  by_cases hqv : v ≤ chart z J q.1 q.2
  · obtain ⟨k, hk, hmk⟩ := hlocal q.2 hq.2 ⟨q.1, hq.1, hqv⟩
    exact (hmk hq.1).trans (hm hk)
  · exact (le_of_not_ge hqv).trans hv

private theorem avg_lt_of_strict {n : ℕ} (hn : 0 < n) {z : Fin n → ℝ} {v : ℝ}
    (h : ∀ i, z i ≤ v) (hs : ∃ i, z i < v) : Gamma.avg z < v := by
  obtain ⟨i, hi⟩ := hs
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  unfold Gamma.avg
  apply (div_lt_iff₀ hnR).2
  have hsum := Finset.sum_lt_sum (fun j (_ : j ∈ Finset.univ) ↦ h j)
    ⟨i, Finset.mem_univ i, hi⟩
  simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_comm]
    using hsum

/-- At a positive held scale, a transformed-gamma fit exists exactly
outside the constant samples at or above the hold. -/
theorem held_scale_attains_iff {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Gamma.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ TransformedGamma.logLik x q.1 q.2 a) Gamma.domain p) ↔
      ¬ ∃ v ≥ a, ∀ i, x i = v := by
  let z := fun i ↦ log (x i) - log a
  let J := Gamma.avgLog x
  constructor
  · rintro ⟨p, _, hm⟩ ⟨v, hv, h⟩
    have he : x = fun _ ↦ v := funext h
    have ht := Shape2TrGamma.held_scale_constant_concentration (n := n) hn ha hv
    rw [← he] at ht
    obtain ⟨q, hq, hval⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop (TransformedGamma.logLik x p.1 p.2 a)))).exists
    have hD : (exp (q * (log v - log a)), q) ∈ Gamma.domain := ⟨exp_pos _, hq⟩
    exact hval.not_ge (hm hD)
  · intro h
    change ∃ p ∈ Gamma.domain, IsMaxOn (fun q : ℝ × ℝ ↦ chart z J q.1 q.2) Gamma.domain p
    apply chart_attains hn
    by_cases hle : ∀ i, x i ≤ a
    · right
      have hz : ∀ i, z i ≤ 0 := fun i ↦ sub_nonpos.mpr (log_le_log (hx i) (hle i))
      have hstrict : ∃ i, z i < 0 := by
        by_contra! hz'
        apply h
        refine ⟨a, le_rfl, fun i ↦ ?_⟩
        apply (log_injOn_pos (hx i) ha)
        exact sub_eq_zero.mp (le_antisymm (hz i) (hz' i))
      have hm := avg_lt_of_strict hn hz hstrict
      have hrate : 0 < (-Gamma.avg z)⁻¹ := inv_pos.mpr (neg_pos.mpr hm)
      obtain ⟨k, hk, p, hp, hB⟩ := Shape2TrGamma.held_scale_power_trace_beaten ha hrate hx hle
      refine ⟨hm, k, hk, p, hp, ?_⟩
      change log ((-Gamma.avg z)⁻¹) - J + (-Gamma.avg z)⁻¹ * Gamma.avg z -
          Gamma.avg (fun i ↦ if x i = a then 1 else 0) < chart z J k p at hB
      rw [log_inv, show (-Gamma.avg z)⁻¹ * Gamma.avg z = -1 by field_simp [(ne_of_lt hm)]] at hB
      have he (i : Fin n) : z i = 0 ↔ x i = a := by
        change log (x i) - log a = 0 ↔ _
        rw [sub_eq_zero]
        exact ⟨log_injOn_pos (hx i) ha, congrArg log⟩
      simp only [he]
      linarith only [hB]
    · left
      push Not at hle
      obtain ⟨i, hi⟩ := hle
      obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x ⟨i, Finset.mem_univ _⟩
      have hmax (k : Fin n) : x k ≤ x j := hj k (Finset.mem_univ _)
      have haj := hi.trans_le (hmax i)
      have hs : ∃ k, z k < z j := by
        by_contra! hge
        apply h
        refine ⟨x j, haj.le, fun k ↦ le_antisymm (hmax k) ?_⟩
        exact (log_le_log_iff (hx j) (hx k)).mp (by
          have h := hge k
          dsimp [z] at h
          linarith)
      refine ⟨j, sub_pos.mpr (log_lt_log ha haj), avg_lt_of_strict hn (fun k ↦ ?_) hs⟩
      dsimp [z]
      exact sub_le_sub_right (log_le_log (hx k) (hmax k)) _

/-- The held-scale criterion transfers to inverse transformed gamma,
reversing the side of the hold for constant concentration. -/
theorem inverse_held_scale_attains_iff {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Gamma.domain, IsMaxOn
      (fun q : ℝ × ℝ ↦ InverseTransformedGamma.logLik x q.1 q.2 a) Gamma.domain p) ↔
      ¬ ∃ v ≤ a, ∀ i, x i = v := by
  let y := InverseTransformedGamma.reciprocal x
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (hx i)
  have he (p : ℝ × ℝ) (_ : p ∈ Gamma.domain) :
      InverseTransformedGamma.logLik x p.1 p.2 a =
        (fun q : ℝ × ℝ ↦ TransformedGamma.logLik y q.1 q.2 a⁻¹) (id p) +
          (-2 * Gamma.avgLog x) := InverseTransformedGamma.logLik_reciprocal x p.1 p.2 a
  let g : ℝ × ℝ → ℝ := fun q ↦ TransformedGamma.logLik y q.1 q.2 a⁻¹
  rw [Transfer.exists_max_iff (g := g) (bijOn_id Gamma.domain) he,
    held_scale_attains_iff hn hy (inv_pos.mpr ha)]
  apply not_congr
  constructor
  · rintro ⟨v, hv, h⟩
    have hv0 : 0 < v := (inv_pos.mpr ha).trans_le hv
    refine ⟨v⁻¹, ?_, fun i ↦ ?_⟩
    · simpa only [inv_inv] using inv_anti₀ (inv_pos.mpr ha) hv
    · simpa only [y, InverseTransformedGamma.reciprocal, inv_inv] using congrArg Inv.inv (h i)
  · rintro ⟨v, hv, h⟩
    have hv0 : 0 < v := by rw [← h ⟨0, hn⟩]; exact hx ⟨0, hn⟩
    exact ⟨v⁻¹, inv_anti₀ hv0 hv, fun i ↦ congrArg Inv.inv (h i)⟩

/-- On positive constants, either free shape can concentrate at a free
scale. The held-scale concentration paths cover contact and the
constant branch above the hold. -/
theorem constant_concentration_masks {n : ℕ} (hn : 0 < n) {v : ℝ} (_hv : 0 < v) :
    let x := fun _ : Fin n ↦ v
    (∀ p > 0, Tendsto (fun k ↦ TransformedGamma.logLik x k p
      (TransformedGamma.scale x k p)) atTop atTop) ∧
    (∀ k > 0, Tendsto (fun p ↦ TransformedGamma.logLik x k p v) atTop atTop) ∧
    (∀ a > 0, a ≤ v →
      Tendsto (fun p ↦ TransformedGamma.logLik x (exp (p * (log v - log a))) p a)
        atTop atTop) := by
  exact ⟨fun p hp ↦ TransformedGamma.heldPower_constant_unbounded hn v hp,
    fun k _ ↦ TransformedGamma.heldShape_constant_unbounded hn v k,
    fun a ha hva ↦ Shape2TrGamma.held_scale_constant_concentration hn ha hva⟩

/-- The same concentration paths for the reciprocal family include
held-scale contact and the constant branch below the hold. -/
theorem inverse_constant_concentration_masks {n : ℕ} (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    let x := fun _ : Fin n ↦ v
    (∀ p > 0, Tendsto (fun k ↦ InverseTransformedGamma.logLik x k p
      (TransformedGamma.scale (fun _ : Fin n ↦ v⁻¹) k p)⁻¹) atTop atTop) ∧
    (∀ k > 0, Tendsto (fun p ↦ InverseTransformedGamma.logLik x k p v) atTop atTop) ∧
    (∀ a ≥ v,
      Tendsto (fun p ↦ InverseTransformedGamma.logLik x (exp (p * (log a - log v))) p a)
        atTop atTop) := by
  exact ⟨fun p hp ↦ InverseTransformedGamma.heldPower_constant_unbounded hn v hp,
    fun k _ ↦ InverseTransformedGamma.heldShape_constant_unbounded hn v k,
    fun a hva ↦ Shape2InvTrGamma.held_scale_constant_concentration hn hv hva⟩

/-- Three distinct log levels identify power, gamma shape and scale
from the transformed-gamma observation score. Taking negative signed
power gives the inverse orientation in the same statement. -/
theorem oriented_score_separation {k p a k' p' a' x y z : ℝ}
    (hp : p ≠ 0) (hp' : p' ≠ 0) (ha : 0 < a) (ha' : 0 < a')
    (hxy : x < y) (hyz : y < z)
    (h : ∀ t ∈ ({x, y, z} : Set ℝ),
      k * p - p * exp (p * (t - log a)) =
        k' * p' - p' * exp (p' * (t - log a'))) :
    p = p' ∧ k = k' ∧ a = a' := by
  let A := exp (-(p * log a))
  let A' := exp (-(p' * log a'))
  have hE (t : ℝ) (p s : ℝ) : p * exp (p * (t - log s)) =
      exp (-(p * log s)) * p * exp (p * t) := by
    rw [mul_sub, exp_sub, exp_neg]
    ring
  have hs (t : ℝ) (ht : t ∈ ({x, y, z} : Set ℝ)) :
      k * p - A * p * exp (p * t) = k' * p' - A' * p' * exp (p' * t) := by
    simpa only [hE, A, A'] using h t ht
  obtain ⟨he, hk, hA⟩ := Shape3GenGamma.signed_score_separation hp hp'
    (show 0 < A from exp_pos _) (show 0 < A' from exp_pos _) hxy hyz hs
  subst p'
  refine ⟨rfl, hk, log_injOn_pos ha ha' ?_⟩
  exact mul_left_cancel₀ hp (neg_injective (exp_injective hA))

end
end UnivariateML.Shape3TrGamma
end
