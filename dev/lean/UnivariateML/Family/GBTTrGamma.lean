module

public import UnivariateML.Family.AllFreeTransformedGamma
public import UnivariateML.Family.Shape3GenGamma

set_option linter.style.header false

/-! The transformed-gamma nuisance profile in the free-power chart.

## Deviations

The attainment comparison uses a finite feasible witness. Finiteness of
the stationary list and the joint nuisance escape classification remain open.
-/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.GBTTrGamma
noncomputable section

variable {n : ℕ}

/-- The mean log likelihood with scale optimized at a fixed shape and power. -/
def scaleProfile (x : Fin n → ℝ) (k p : ℝ) : ℝ :=
  log p - UnivariateML.Gamma.avgLog x + k * log k - k - log (Gamma k) -
    k * UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)

private theorem avgLog_power (x : Fin n → ℝ) (p : ℝ) :
    UnivariateML.Gamma.avgLog (TransformedGamma.powerSample x p) =
      p * UnivariateML.Gamma.avgLog x := by
  simp only [UnivariateML.Gamma.avgLog, UnivariateML.Gamma.avg, TransformedGamma.powerSample,
    log_exp, ← Finset.mul_sum]
  ring

/-- At fixed positive shape and power, the profile is attained at the
closed-form scale and bounds the likelihood at every positive scale. -/
theorem scaleProfile_spec (hn : 0 < n) (x : Fin n → ℝ) {k p : ℝ}
    (hk : 0 < k) (hp : 0 < p) :
    TransformedGamma.logLik x k p (TransformedGamma.scale x k p) = scaleProfile x k p ∧
      ∀ a > 0, TransformedGamma.logLik x k p a ≤ scaleProfile x k p := by
  have hm : 0 < UnivariateML.Gamma.avg (TransformedGamma.powerSample x p) := by
    apply div_pos
    · exact Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    · exact_mod_cast hn
  have hrate : TransformedGamma.powerRate p (TransformedGamma.scale x k p) =
      UnivariateML.Gamma.rate (TransformedGamma.powerSample x p) k := by
    unfold TransformedGamma.powerRate TransformedGamma.scale UnivariateML.Gamma.rate
    rw [log_exp, show -p * (log (UnivariateML.Gamma.avg
      (TransformedGamma.powerSample x p) / k) / p) =
      log (k / UnivariateML.Gamma.avg (TransformedGamma.powerSample x p)) by
        rw [log_div hm.ne' hk.ne', log_div hk.ne' hm.ne']
        field_simp
        ring, exp_log (div_pos hk hm)]
  have he : TransformedGamma.logLik x k p (TransformedGamma.scale x k p) =
      scaleProfile x k p := by
    rw [TransformedGamma.logLik_power hn, hrate, ← UnivariateML.Gamma.profile,
      UnivariateML.Gamma.profile_eq hn (x := TransformedGamma.powerSample x p)
        (fun _ ↦ exp_pos _) hk, avgLog_power]
    unfold scaleProfile
    ring
  refine ⟨he, fun a ha ↦ ?_⟩
  rw [← he]
  by_cases h : a = TransformedGamma.scale x k p
  · rw [h]
  · exact ((TransformedGamma.heldShapes_scale hn x hk hp).2 a ha h).le

/-- The conditional maximizing shape is the unique positive solution of
the inverse digamma score-gap equation. -/
theorem conditional_score (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {p : ℝ} (hp : 0 < p) :
    ∃! k : ℝ, 0 < k ∧ UnivariateML.Gamma.scoreGap k =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p) ∧
      IsMaxOn (fun t ↦ scaleProfile x t p) (Ioi 0) k := by
  obtain ⟨⟨k, a⟩, ⟨hka, hmax⟩, _⟩ := TransformedGamma.heldPower hn hx hne hp
  have hk : 0 < k := hka.1
  have h : IsMaxOn (fun t ↦ scaleProfile x t p) (Ioi 0) k := by
    intro t ht
    change scaleProfile x t p ≤ scaleProfile x k p
    rw [← (scaleProfile_spec hn x ht hp).1]
    have hmem : (t, TransformedGamma.scale x t p) ∈ UnivariateML.Gamma.domain :=
      ⟨ht, exp_pos _⟩
    exact (isMaxOn_iff.mp hmax _ hmem).trans ((scaleProfile_spec hn x hk hp).2 a hka.2)
  have he (t : ℝ) (ht : 0 < t) : scaleProfile x t p =
      UnivariateML.Gamma.profile (TransformedGamma.powerSample x p) t +
        (log p + (p - 1) * UnivariateML.Gamma.avgLog x) := by
    rw [UnivariateML.Gamma.profile_eq hn (x := TransformedGamma.powerSample x p)
      (fun _ ↦ exp_pos _) ht, avgLog_power]
    unfold scaleProfile
    ring
  have hd := ((UnivariateML.Gamma.hasDerivAt_profile hn
    (x := TransformedGamma.powerSample x p) (fun _ ↦ exp_pos _) hk).add_const
    (log p + (p - 1) * UnivariateML.Gamma.avgLog x)).congr_of_eventuallyEq
    (by filter_upwards [Ioi_mem_nhds hk] with t ht using he t ht)
  have hs := (h.isLocalMax (Ioi_mem_nhds hk)).hasDerivAt_eq_zero hd
  refine ⟨k, ⟨hk, sub_eq_zero.mp hs, h⟩, fun t ht ↦ ?_⟩
  exact UnivariateML.Gamma.scoreGap_strictAnti.injOn ht.1 hk
    (ht.2.1.trans (sub_eq_zero.mp hs).symm)

/-- The large-power sample score gap divided by power tends to the mean
log gap from the maximum. The estimate uses only a single maximizing
observation, so it also applies to tied maxima. -/
theorem gap_div_power_atTop (hn : 0 < n) (x : Fin n → ℝ) (j : Fin n)
    (hj : ∀ i, log (x i) ≤ log (x j)) :
    Tendsto (fun p : ℝ ↦
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p) / p) atTop
      (𝓝 (log (x j) - UnivariateML.Gamma.avgLog x)) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let M := log (x j)
  let μ := UnivariateML.Gamma.avgLog x
  have hlo : Tendsto (fun p : ℝ ↦ M - μ - log (n : ℝ) / p) atTop (𝓝 (M - μ)) := by
    simpa [div_eq_mul_inv] using (tendsto_inv_atTop_zero.const_mul (log (n : ℝ))).const_sub (M - μ)
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hlo tendsto_const_nhds
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    let z := TransformedGamma.powerSample x p
    have hsum : z j ≤ ∑ i, z i :=
      Finset.single_le_sum (fun i _ ↦ (exp_pos (p * log (x i))).le) (Finset.mem_univ j)
    have h := log_le_log (div_pos (exp_pos (p * M)) hn')
      (div_le_div_of_nonneg_right hsum hn'.le)
    change log (exp (p * M) / n) ≤ log (UnivariateML.Gamma.avg z) at h
    rw [log_div (exp_pos _).ne' hn'.ne', log_exp] at h
    rw [UnivariateML.Gamma.jensenGap, avgLog_power, le_div_iff₀ hp, sub_mul,
      div_mul_cancel₀ _ hp.ne']
    dsimp [μ]
    nlinarith
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    let z := TransformedGamma.powerSample x p
    have hsum : (∑ i, z i) ≤ (n : ℝ) * exp (p * M) := by
      calc
        (∑ i, z i) ≤ ∑ _i : Fin n, exp (p * M) := by
          exact Finset.sum_le_sum (fun i _ ↦ exp_le_exp.mpr
            (mul_le_mul_of_nonneg_left (hj i) hp.le))
        _ = _ := by simp
    have hm : 0 < UnivariateML.Gamma.avg z :=
      div_pos (Finset.sum_pos (fun i _ ↦ exp_pos (p * log (x i)))
        ⟨j, Finset.mem_univ j⟩) hn'
    have hupper : UnivariateML.Gamma.avg z ≤ exp (p * M) :=
      (div_le_iff₀ hn').mpr (by rw [mul_comm]; exact hsum)
    have h := log_le_log hm hupper
    rw [log_exp] at h
    rw [UnivariateML.Gamma.jensenGap, avgLog_power, div_le_iff₀ hp]
    dsimp [μ]
    nlinarith

private theorem scoreGap_mul_at_zero :
    Tendsto (fun k : ℝ ↦ k * UnivariateML.Gamma.scoreGap k) (𝓝[>] 0) (𝓝 1) := by
  let ψ := UnivariateML.Gamma.digamma
  let g : ℝ → ℝ := log ∘ Real.Gamma
  have hd {k : ℝ} (hk : 0 < k) : HasDerivAt g (ψ k) k :=
    ((Real.hasDerivAt_Gamma_of_pos hk).log (Gamma_pos_of_pos hk).ne').differentiableAt.hasDerivAt
  have hr {k : ℝ} (hk : 0 < k) : ψ (k + 1) = k⁻¹ + ψ k := by
    have h := (hd (show 0 < k + 1 by positivity)).comp k
      ((hasDerivAt_id k).add_const 1)
    have h' := (hasDerivAt_log hk.ne').add (hd hk)
    have he : (fun t ↦ log t + g t) =ᶠ[𝓝 k] g ∘ (fun t ↦ t + 1) := by
      filter_upwards [Ioi_mem_nhds hk] with t ht
      change log t + log (Real.Gamma t) = log (Real.Gamma (t + 1))
      rw [Real.Gamma_add_one ht.ne', log_mul ht.ne' (Gamma_pos_of_pos ht).ne']
    simpa using h.unique (h'.congr_of_eventuallyEq he.symm)
  have hmono : MonotoneOn ψ (Ioi 0) :=
    convexOn_log_Gamma.monotoneOn_deriv (fun _ hk ↦ (hd hk).differentiableAt)
  have hk0 : Tendsto (fun k : ℝ ↦ k) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hψ : Tendsto (fun k : ℝ ↦ k * ψ (k + 1)) (𝓝[>] 0) (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le'
      (by simpa using hk0.mul_const (ψ 1)) (by simpa using hk0.mul_const (ψ 2))
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k hk
      change 0 < k at hk
      have h := hmono one_pos (show 0 < k + 1 by positivity) (show 1 ≤ k + 1 by linarith)
      exact mul_le_mul_of_nonneg_left h hk.le
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
        nhdsWithin_le_nhds (Iio_mem_nhds (show (0 : ℝ) < 1 by norm_num))] with k hk hk'
      change 0 < k at hk
      change k < 1 at hk'
      have h := hmono (show 0 < k + 1 by positivity) (show (0 : ℝ) < 2 by norm_num)
        (show k + 1 ≤ 2 by linarith)
      exact mul_le_mul_of_nonneg_left h hk.le
  have h := ((continuous_mul_log.continuousAt (x := (0 : ℝ))).tendsto.mono_left
    (nhdsWithin_le_nhds (s := Ioi (0 : ℝ))))
  have H := (h.sub hψ).add_const 1
  simp only [zero_mul, sub_zero, zero_add] at H
  apply H.congr'
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with k hk
  unfold UnivariateML.Gamma.scoreGap
  change k * log k - k * ψ (k + 1) + 1 = k * (log k - ψ k)
  rw [hr hk, mul_add, mul_inv_cancel₀ hk.ne']
  ring

/-- The conditional shape and scale profile reach the fitted power height
at large power. Thus this height is also an upper envelope over arbitrary
positive shape and scale when power tends to infinity. -/
theorem power_profile_atTop (hn : 0 < n) (x : Fin n → ℝ) (j : Fin n)
    (hj : ∀ i, log (x i) ≤ log (x j))
    (hD : 0 < log (x j) - UnivariateML.Gamma.avgLog x)
    {k : ℝ → ℝ} (hk : ∀ p > 0, 0 < k p)
    (hs : ∀ p > 0, UnivariateML.Gamma.scoreGap (k p) =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)) :
    Tendsto (fun p ↦ scaleProfile x (k p) p) atTop
      (𝓝 (log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
        UnivariateML.Gamma.avgLog x - 1)) := by
  let D := log (x j) - UnivariateML.Gamma.avgLog x
  let K (p : ℝ) := UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)
  have hK : Tendsto (fun p ↦ K p / p) atTop (𝓝 D) := gap_div_power_atTop hn x j hj
  have hKtop : Tendsto K atTop atTop := by
    apply tendsto_atTop_mono' _ _ (tendsto_id.const_mul_atTop (show 0 < D / 2 by positivity))
    filter_upwards [hK.eventually (Ioi_mem_nhds (show D / 2 < D by linarith)),
      eventually_gt_atTop (0 : ℝ)] with p hp hp0
    simpa [mul_comm] using ((lt_div_iff₀ hp0).mp hp).le
  have hk0 : Tendsto k atTop (𝓝[>] 0) := by
    rw [nhdsWithin, tendsto_inf]
    refine ⟨tendsto_order.2 ⟨?_, ?_⟩, tendsto_principal.mpr ?_⟩
    · intro a ha
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
      exact ha.trans (hk p hp)
    · intro b hb
      filter_upwards [hKtop.eventually (eventually_gt_atTop (UnivariateML.Gamma.scoreGap b)),
        eventually_gt_atTop (0 : ℝ)] with p hp hp0
      by_contra! hle
      exact hp.not_ge (by
        change UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p) ≤ _
        rw [← hs p hp0]
        exact UnivariateML.Gamma.scoreGap_strictAnti.antitoneOn hb (hk p hp0) hle)
    · exact (eventually_gt_atTop (0 : ℝ)).mono (fun p hp ↦ hk p hp)
  have hk' : Tendsto k atTop (𝓝 0) := hk0.mono_right nhdsWithin_le_nhds
  have hkK : Tendsto (fun p ↦ k p * K p) atTop (𝓝 1) := by
    apply (scoreGap_mul_at_zero.comp hk0).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    change k p * UnivariateML.Gamma.scoreGap (k p) = k p * K p
    rw [hs p hp]
  have hpk : Tendsto (fun p ↦ p * k p) atTop (𝓝 D⁻¹) := by
    simpa only [one_div] using (hkK.div hK hD.ne').congr' (by
      filter_upwards [eventually_gt_atTop (0 : ℝ),
        hK.eventually (Ioi_mem_nhds hD)] with p hp hpos
      have h : K p ≠ 0 := by
        intro he
        simp [he] at hpos
      change (k p * K p) / (K p / p) = p * k p
      field_simp [hp.ne', h])
  have hlogΓ := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt.tendsto.comp
    (by simpa using hk'.add_const 1 : Tendsto (fun p ↦ k p + 1) atTop (𝓝 1))
  simp only [Real.Gamma_one, Function.comp_def] at hlogΓ
  have hlogΓ' := hlogΓ.log one_ne_zero
  have hkl := continuous_mul_log.continuousAt.tendsto.comp hk'
  have h := (((((hpk.log (inv_ne_zero hD.ne')).sub_const
    (UnivariateML.Gamma.avgLog x)).add hkl).sub hk').sub hlogΓ').sub hkK
  simp only [Function.comp_def, zero_mul, add_zero, sub_zero, log_one] at h
  refine h.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
  unfold scaleProfile
  rw [log_mul hp.ne' (hk p hp).ne', Real.Gamma_add_one (hk p hp).ne',
    log_mul (hk p hp).ne' (Gamma_pos_of_pos (hk p hp)).ne']
  ring

private theorem max_log_gap_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j)) :
    0 < log (x j) - UnivariateML.Gamma.avgLog x := by
  obtain ⟨i, hi⟩ : ∃ i, log (x i) < log (x j) := by
    by_contra! h
    obtain ⟨r,s,hrs⟩ := hne
    exact hrs (log_injOn_pos (hx r) (hx s)
      ((le_antisymm (hj r) (h r)).trans (le_antisymm (hj s) (h s)).symm))
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  apply sub_pos.mpr
  unfold UnivariateML.Gamma.avgLog UnivariateML.Gamma.avg
  apply (div_lt_iff₀ hn').mpr
  calc
    (∑ r, log (x r)) < ∑ _r : Fin n, log (x j) :=
      Finset.sum_lt_sum (fun r _ ↦ hj r) ⟨i, Finset.mem_univ i, hi⟩
    _ = _ := by simp [mul_comm]

/-- Every sufficiently large power has a uniform upper bound over all
positive shape and scale, given by the fitted power face plus any slack. -/
theorem power_exterior (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j))
    {ε : ℝ} (hε : 0 < ε) :
    ∀ᶠ p : ℝ in atTop, ∀ k > 0, ∀ a > 0,
      TransformedGamma.logLik x k p a <
        log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
          UnivariateML.Gamma.avgLog x - 1 + ε := by
  classical
  choose k hk using (fun p : Ioi (0 : ℝ) ↦
    (conditional_score hn hx hne p.property).exists)
  let K (p : ℝ) := if h : 0 < p then k ⟨p, h⟩ else 1
  have hspec (p : ℝ) (hp : 0 < p) := hk ⟨p, hp⟩
  have hlim := power_profile_atTop hn x j hj (max_log_gap_pos hn hx hne j hj) (k := K)
    (fun p hp ↦ by simpa [K, hp] using (hspec p hp).1)
    (fun p hp ↦ by simpa [K, hp] using (hspec p hp).2.1)
  filter_upwards [eventually_gt_atTop (0 : ℝ),
    hlim.eventually (Iio_mem_nhds (lt_add_of_pos_right _ hε))] with p hp hbound
  intro t ht a ha
  have hm : scaleProfile x t p ≤ scaleProfile x (K p) p := by
    simpa only [K, dite_eq_left hp] using isMaxOn_iff.mp (hspec p hp).2.2 t ht
  exact ((scaleProfile_spec hn x ht hp).2 a ha |>.trans hm).trans_lt hbound

private theorem exp_second_order (z : ℝ) :
    Tendsto (fun t : ℝ ↦ (exp (t * z) - 1 - t * z) / t ^ 2)
      (𝓝[>] 0) (𝓝 (z ^ 2 / 2)) := by
  by_cases hz : z = 0
  · simp [hz]
  have h : (fun u : ℝ ↦ exp u - (1 + u + u ^ 2 / 2)) =o[𝓝 0] (fun u ↦ u ^ 2) := by
    convert Real.exp_sub_sum_range_succ_isLittleO_pow 2 using 1
    funext u
    norm_num [Finset.sum_range_succ]
  have h' := (Asymptotics.isLittleO_iff_tendsto
    (fun u hu ↦ by have : u = 0 := eq_zero_of_pow_eq_zero hu; simp [this])).mp h
  have ht : Tendsto (fun t : ℝ ↦ t * z) (𝓝[>] 0) (𝓝 0) := by
    have h0 : Tendsto (fun t : ℝ ↦ t) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    simpa using h0.mul_const z
  have H := ((h'.comp ht).const_mul (z ^ 2)).add_const (z ^ 2 / 2)
  simp only [mul_zero, zero_add, Function.comp_def] at H
  apply H.congr'
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
  have ht' : 0 < t := ht
  field_simp [ht'.ne', hz]
  ring

/-- At small positive power the powered-sample score gap divided by
squared power tends to half the empirical log variance. -/
theorem gap_div_power_sq_at_zero (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (hne : ∃ i j, x i ≠ x j) :
    Tendsto (fun t : ℝ ↦
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x t) / t ^ 2) (𝓝[>] 0)
      (𝓝 (UnivariateML.Gamma.avg (fun i ↦
        (log (x i) - UnivariateML.Gamma.avgLog x) ^ 2) / 2)) := by
  let μ := UnivariateML.Gamma.avgLog x
  let z (i : Fin n) := log (x i) - μ
  let M (t : ℝ) := UnivariateML.Gamma.avg (fun i ↦ exp (t * z i))
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hz : ∑ i, z i = 0 := by
    simp only [z, μ, UnivariateML.Gamma.avgLog, UnivariateML.Gamma.avg,
      Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    field_simp
    ring
  have hK (t : ℝ) :
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x t) = log (M t) := by
    have hM : M t = exp (-(t * μ)) *
        UnivariateML.Gamma.avg (TransformedGamma.powerSample x t) := by
      simp only [M, z, mul_sub, exp_sub, TransformedGamma.powerSample, UnivariateML.Gamma.avg,
        div_eq_mul_inv, exp_neg]
      simp_rw [show ∀ i, exp (t * log (x i)) * (exp (t * μ))⁻¹ =
        (exp (t * μ))⁻¹ * exp (t * log (x i)) from fun _ ↦ mul_comm _ _,
        ← Finset.mul_sum]
      ring
    have hp : 0 < UnivariateML.Gamma.avg (TransformedGamma.powerSample x t) := by
      apply div_pos
      · exact Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      · exact_mod_cast hn
    rw [hM, log_mul (exp_pos _).ne' hp.ne', log_exp, UnivariateML.Gamma.jensenGap, avgLog_power]
    ring
  have hMb : Tendsto (fun t ↦ (M t - 1) / t ^ 2) (𝓝[>] 0)
      (𝓝 (UnivariateML.Gamma.avg (fun i ↦ z i ^ 2) / 2)) := by
    have h := (tendsto_finsetSum Finset.univ (fun i _ ↦ exp_second_order (z i))).div_const (n : ℝ)
    unfold UnivariateML.Gamma.avg
    simp only [← Finset.sum_div] at h
    rw [div_right_comm]
    apply h.congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
    have ht' : 0 < t := ht
    simp only [Finset.sum_div, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum, hz, mul_zero, sub_zero, M,
      UnivariateML.Gamma.avg]
    field_simp [hn', ht'.ne']
    rw [mul_sub, mul_one, mul_comm (n : ℝ), ← Finset.sum_div, div_mul_cancel₀ _ hn']
  have hM : Tendsto (fun t ↦ M t - 1) (𝓝[>] 0) (𝓝 0) := by
    have h (i : Fin n) : Tendsto (fun t ↦ exp (t * z i)) (𝓝[>] 0) (𝓝 1) := by
      have hc : ContinuousAt (fun t : ℝ ↦ exp (t * z i)) 0 :=
        Real.continuous_exp.continuousAt.comp (continuous_id.mul continuous_const).continuousAt
      simpa using hc.tendsto.mono_left (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
    simpa [M, UnivariateML.Gamma.avg, hn'] using
      ((tendsto_finsetSum Finset.univ (fun i _ ↦ h i)).div_const (n : ℝ)).sub_const 1
  have hneM : ∀ᶠ t in 𝓝[>] (0 : ℝ), M t - 1 ≠ 0 := by
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
    have ht' : 0 < t := ht
    intro he
    have hz : log (M t) = 0 := by rw [sub_eq_zero.mp he, log_one]
    have h := (UnivariateML.Gamma.jensenGap_spec hn
      (x := TransformedGamma.powerSample x t) (fun _ ↦ exp_pos _)).2
    obtain ⟨i, j, hij⟩ := hne
    apply hij
    apply log_injOn_pos (hx i) (hx j)
    exact mul_left_cancel₀ ht'.ne' (exp_injective (h.mp ((hK t).trans hz) i j))
  have hp : Tendsto (fun t ↦ M t - 1) (𝓝[>] 0) (𝓝[≠] 0) := by
    rw [show (𝓝[≠] (0 : ℝ)) = 𝓝 0 ⊓ 𝓟 ({0}ᶜ) from rfl, tendsto_inf]
    exact ⟨hM, tendsto_principal.mpr hneM⟩
  have hl : Tendsto (fun t : ℝ ↦ log (1 + t) / t) (𝓝[≠] 0) (𝓝 1) := by
    have hb : HasDerivAt (fun t : ℝ ↦ 1 + t) 1 0 := (hasDerivAt_id _).const_add _
    have hl : HasDerivAt log (1 : ℝ) (1 + (0 : ℝ)) := by
      simpa using hasDerivAt_log one_ne_zero
    simpa [add_comm, div_eq_mul_inv, mul_comm] using (hl.comp 0 hb).tendsto_slope_zero
  have h := ((hl.comp hp).mul hMb)
  simp only [one_mul] at h
  apply h.congr'
  filter_upwards [hneM] with t ht
  change log (1 + (M t - 1)) / (M t - 1) * ((M t - 1) / t ^ 2) = _
  rw [hK, show 1 + (M t - 1) = M t by ring]
  field_simp

private def logGammaPeak (k : ℝ) : ℝ :=
  (k - 1 / 2) * log k - k - log (Real.Gamma k)

private theorem logGammaPeak_atTop :
    Tendsto logGammaPeak atTop (𝓝 (-log (2 * π) / 2)) := by
  let f := GenGammaTwoPoint.sampleLogLik (fun _ : Fin 1 ↦ (1 : ℝ))
  let z (k : ℝ) : ℝ × ℝ × ℝ := (0, 1, (sqrt k)⁻¹)
  have hQ := tendsto_inv_atTop_zero.comp tendsto_sqrt_atTop
  have hZ : Tendsto z atTop (𝓝 ((0, 1, 0) : ℝ × ℝ × ℝ)) :=
    tendsto_const_nhds.prodMk_nhds (tendsto_const_nhds.prodMk_nhds hQ)
  have hDomain : ∀ᶠ k in atTop, z k ∈ GenGammaTwoPoint.domain :=
    Eventually.of_forall (fun k ↦ show 0 < (z k).2.1 from one_pos)
  have hDom : ((0, 1, 0) : ℝ × ℝ × ℝ) ∈ GenGammaTwoPoint.domain :=
    show (0 : ℝ) < 1 from one_pos
  have h := ((Shape3GenGamma.likelihood_continuous (fun _ : Fin 1 ↦ (1 : ℝ))) _ hDom).tendsto
  have hlim := h.comp (tendsto_nhdsWithin_iff.mpr ⟨hZ, hDomain⟩)
  have hval : f (0, 1, 0) = -log (2 * π) / 2 := by
    simp [f, GenGammaTwoPoint.sampleLogLik, GenGammaTwoPoint.logDensity, neg_div]
  change Tendsto (f ∘ z) atTop (𝓝 (f (0, 1, 0))) at hlim
  rw [hval] at hlim
  apply hlim.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
  have hs := sqrt_pos.mpr hk
  have he : 1 / ((sqrt k)⁻¹)^2 = k := by
    rw [inv_pow, div_inv_eq_mul, sq_sqrt hk.le, one_mul]
  simp only [Function.comp_def, f, z, GenGammaTwoPoint.sampleLogLik, Fin.sum_univ_one,
    Nat.cast_one, div_one, GenGammaTwoPoint.logDensity, ite_eq_right (inv_ne_zero hs.ne'),
    log_one, sub_zero, mul_zero, exp_zero, sub_self, log_inv, log_sqrt hk.le, he,
    abs_of_pos (inv_pos.mpr hs)]
  unfold logGammaPeak
  ring

private theorem gamma_gap_pos (b : ℝ) (hb : 0 < b) : 0 < UnivariateML.Gamma.scoreGap b := by
  have hnonneg : 0 ≤ UnivariateML.Gamma.scoreGap (b + 1) := by
    apply le_of_tendsto UnivariateML.Gamma.scoreGap_tendsto_atTop
    filter_upwards [eventually_ge_atTop (b + 1)] with t ht
    have hb' : 0 < b + 1 := by positivity
    exact UnivariateML.Gamma.scoreGap_strictAnti.antitoneOn hb' (hb'.trans_le ht) ht
  exact hnonneg.trans_lt (UnivariateML.Gamma.scoreGap_strictAnti hb
    (by positivity : 0 < b + 1) (lt_add_one b))

/-- The fitted lognormal value is a uniform small-power upper envelope over
positive shape and scale, including samples with tied observations. -/
theorem lognormal_exterior (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) {ε : ℝ} (hε : 0 < ε) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    ∀ᶠ t : ℝ in 𝓝[>] 0, ∀ k > 0, ∀ a > 0,
      TransformedGamma.logLik x k t a <
        -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x + ε := by
  classical
  intro V
  let K (t : ℝ) := UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x t)
  have hV : 0 < V := by
    obtain ⟨i, j, hij⟩ := hne
    have hne' : ∃ l, log (x l) ≠ UnivariateML.Gamma.avgLog x := by
      by_contra! h
      exact hij (log_injOn_pos (hx i) (hx j) ((h i).trans (h j).symm))
    obtain ⟨i, hi⟩ := hne'
    exact div_pos (Finset.sum_pos' (fun _ _ ↦ sq_nonneg _) ⟨i, Finset.mem_univ _,
      sq_pos_of_ne_zero (sub_ne_zero.mpr hi)⟩) (by exact_mod_cast hn)
  have hK := gap_div_power_sq_at_zero hn hx hne
  change Tendsto (fun t ↦ K t / t^2) (𝓝[>] 0) (𝓝 (V / 2)) at hK
  have ht0 : Tendsto (fun t : ℝ ↦ t) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hK0 : Tendsto K (𝓝[>] 0) (𝓝 0) := by
    have h := hK.mul (ht0.pow 2)
    simp only [zero_pow (by norm_num : (2 : ℕ) ≠ 0), mul_zero] at h
    apply h.congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
    exact div_mul_cancel₀ _ (pow_ne_zero _ (ne_of_gt ht))
  choose k hk using (fun t : Ioi (0 : ℝ) ↦ (conditional_score hn hx hne t.property).exists)
  let κ (t : ℝ) := if h : 0 < t then k ⟨t, h⟩ else 1
  have hκ (t : ℝ) (ht : 0 < t) :
      0 < κ t ∧ UnivariateML.Gamma.scoreGap (κ t) = K t ∧
      IsMaxOn (fun w ↦ scaleProfile x w t) (Ioi 0) (κ t) := by
    simpa only [κ, dite_eq_left ht] using hk ⟨t, ht⟩
  have hκtop : Tendsto κ (𝓝[>] 0) atTop := by
    apply tendsto_atTop.2
    intro b
    let b' := max b 1
    have hb : 0 < b' := zero_lt_one.trans_le (le_max_right _ _)
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      hK0.eventually (Iio_mem_nhds (gamma_gap_pos b' hb))] with t ht hsmall
    apply (le_max_left b 1).trans
    by_contra! hbelow
    exact hsmall.not_ge (by
      rw [← (hκ t ht).2.1]
      exact UnivariateML.Gamma.scoreGap_strictAnti.antitoneOn (hκ t ht).1 hb hbelow.le)
  have hV2 : 0 < V / 2 := by positivity
  have hB := (logGammaPeak_atTop.comp hκtop).sub
    (((hK.const_mul 2).log (by positivity : 2 * (V / 2) ≠ 0)).div_const 2)
  have hB' := (hB.sub_const (1 / 2)).sub_const (UnivariateML.Gamma.avgLog x)
  have he : 2 * (V / 2) = V := by ring
  rw [he] at hB'
  have hhigh := hB'.eventually (Iio_mem_nhds (lt_add_of_pos_right _ hε))
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)), hhigh,
    hK.eventually (Ioi_mem_nhds hV2)] with t ht high htK
  intro w hw a ha
  have ht' : 0 < t := ht
  have hkt := (hκ t ht).1
  have hKt : 0 < K t := (div_pos_iff_of_pos_right (sq_pos_of_pos ht')).mp htK
  have hlog := log_le_sub_one_of_pos (show 0 < 2 * (κ t) * K t by positivity)
  rw [log_mul (by positivity : 2 * κ t ≠ 0) hKt.ne', log_mul two_ne_zero hkt.ne'] at hlog
  have hnorm : log (2 * (K t / t ^ 2)) = log 2 + log (K t) - 2 * log t := by
    rw [log_mul two_ne_zero (div_pos hKt (sq_pos_of_pos ht')).ne',
      log_div hKt.ne' (sq_pos_of_pos ht').ne', log_pow]
    norm_num
    ring
  have hupper : scaleProfile x (κ t) t ≤
      logGammaPeak (κ t) - log (2 * (K t / t ^ 2)) / 2 -
        1 / 2 - UnivariateML.Gamma.avgLog x := by
    rw [hnorm]
    change log t - UnivariateML.Gamma.avgLog x + κ t * log (κ t) - κ t -
      log (Real.Gamma (κ t)) - κ t * K t ≤ _
    unfold logGammaPeak
    linarith only [hlog]
  exact (((scaleProfile_spec hn x hw ht').2 a ha).trans
    (isMaxOn_iff.mp (hκ t ht).2.2 w hw)).trans_lt (hupper.trans_lt high)

private theorem gap_contAt (hn : 0 < n) (x : Fin n → ℝ) (p : ℝ) :
    ContinuousAt (fun t ↦ UnivariateML.Gamma.jensenGap
      (TransformedGamma.powerSample x t)) p := by
  let M (t : ℝ) := UnivariateML.Gamma.avg (TransformedGamma.powerSample x t)
  have hm : 0 < M p := by
    apply div_pos
    · exact Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    · exact_mod_cast hn
  have hM : Continuous M := by
    unfold M UnivariateML.Gamma.avg TransformedGamma.powerSample
    apply Continuous.div_const
    exact continuous_finsetSum _ (fun _ _ ↦ Real.continuous_exp.comp
      (continuous_id.mul continuous_const))
  have he (t : ℝ) : UnivariateML.Gamma.jensenGap
      (TransformedGamma.powerSample x t) = log (M t) - t * UnivariateML.Gamma.avgLog x :=
    congrArg (log (M t) - ·) (avgLog_power x t)
  simp_rw [he]
  exact (hM.continuousAt.log hm.ne').sub (continuous_id.mul continuous_const).continuousAt

private theorem shape_contOn (hn : 0 < n) (x : Fin n → ℝ) {k : ℝ → ℝ}
    (hk : ∀ p > 0, 0 < k p)
    (hs : ∀ p > 0, UnivariateML.Gamma.scoreGap (k p) =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)) :
    ContinuousOn k (Ioi 0) := by
  intro p hp
  have hp' : 0 < p := hp
  let K (p : ℝ) := UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)
  have hKS (t : ℝ) (ht : 0 < t) : UnivariateML.Gamma.scoreGap (k t) = K t := hs t ht
  change Tendsto k (𝓝[Ioi 0] p) (𝓝 (k p))
  have hK : Tendsto K (𝓝[Ioi 0] p) (𝓝 (K p)) :=
    (gap_contAt hn x p).tendsto.mono_left nhdsWithin_le_nhds
  apply tendsto_order.2
  constructor
  · intro b hb
    by_cases hb0 : 0 < b
    · have hbK : K p < UnivariateML.Gamma.scoreGap b := by
        rw [← hKS p hp']
        exact UnivariateML.Gamma.scoreGap_strictAnti hb0 (hk p hp') hb
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
        hK.eventually (Iio_mem_nhds hbK)] with t ht h
      by_contra! hle
      have hh := UnivariateML.Gamma.scoreGap_strictAnti.antitoneOn (hk t ht) hb0 hle
      exact h.not_ge ((hKS t ht) ▸ hh)
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
      exact (le_of_not_gt hb0).trans_lt (hk t ht)
  · intro b hb
    have hb0 := (hk p hp').trans hb
    have hbK : UnivariateML.Gamma.scoreGap b < K p := by
      rw [← hKS p hp']
      exact UnivariateML.Gamma.scoreGap_strictAnti (hk p hp') hb0 hb
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      hK.eventually (Ioi_mem_nhds hbK)] with t ht h
    by_contra! hle
    have hh := UnivariateML.Gamma.scoreGap_strictAnti.antitoneOn hb0 (hk t ht) hle
    exact h.not_ge ((hKS t ht) ▸ hh)

private theorem profile_contOn (hn : 0 < n) (x : Fin n → ℝ) {k : ℝ → ℝ}
    (hk : ∀ p > 0, 0 < k p)
    (hs : ∀ p > 0, UnivariateML.Gamma.scoreGap (k p) =
      UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x p)) :
    ContinuousOn (fun t ↦ scaleProfile x (k t) t) (Ioi 0) := by
  intro t ht
  have ht' : 0 < t := ht
  have h := shape_contOn hn x hk hs t ht
  have hΓ := ((Real.hasDerivAt_Gamma_of_pos (hk t ht')).continuousAt.log
    (Gamma_pos_of_pos (hk t ht')).ne').comp_continuousWithinAt h
  exact (((((continuousAt_log ht'.ne').continuousWithinAt.sub continuousWithinAt_const).add
    (h.mul (h.log (hk t ht').ne'))).sub h).sub hΓ).sub
    (h.mul (gap_contAt hn x t).continuousWithinAt)

/-- A finite feasible height meeting both fitted exterior heights suffices
for an all-free transformed-gamma maximizer, including exact equality. -/
theorem attains_of_face_witness (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j)) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x
    let P := log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
      UnivariateML.Gamma.avgLog x - 1
    (∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ TransformedGamma.logLik x k t a) →
      ∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
        TransformedGamma.logLik x k' t' a' ≤ TransformedGamma.logLik x k t a := by
  classical
  intro V N P hW
  obtain ⟨k₀, hk₀, t₀, ht₀, a₀, ha₀, hW⟩ := hW
  by_cases hstrict : ∃ k > 0, ∃ t > 0, ∃ a > 0, max N P < TransformedGamma.logLik x k t a
  swap
  · push Not at hstrict
    exact ⟨k₀, hk₀, t₀, ht₀, a₀, ha₀,
      fun k hk t ht a ha ↦ (hstrict k hk t ht a ha).trans hW⟩
  obtain ⟨k₁, hk₁, t₁, ht₁, a₁, ha₁, hW₁⟩ := hstrict
  let w := TransformedGamma.logLik x k₁ t₁ a₁
  let ε := (w - max N P) / 2
  have hε : 0 < ε := by dsimp [ε, w]; linarith
  have hL := lognormal_exterior hn hx hne hε
  have hR := power_exterior hn hx hne j hj hε
  obtain ⟨R, hR⟩ := eventually_atTop.mp hR
  obtain ⟨δ, hδ, hL⟩ := Metric.eventually_nhds_iff.mp (eventually_nhdsWithin_iff.mp hL)
  let l := min (δ / 2) t₁
  let r := max R t₁
  have hl : 0 < l := lt_min (by positivity) ht₁
  have h₁ : t₁ ∈ Icc l r := ⟨min_le_right _ _, le_max_right _ _⟩
  choose k hk using (fun t : Ioi (0 : ℝ) ↦ (conditional_score hn hx hne t.property).exists)
  let κ (t : ℝ) := if h : 0 < t then k ⟨t, h⟩ else 1
  have hκ (t : ℝ) (ht : 0 < t) :
      0 < κ t ∧ UnivariateML.Gamma.scoreGap (κ t) =
        UnivariateML.Gamma.jensenGap (TransformedGamma.powerSample x t) ∧
      IsMaxOn (fun u ↦ scaleProfile x u t) (Ioi 0) (κ t) := by
    simpa only [κ, dite_eq_left ht] using hk ⟨t, ht⟩
  have hcont := profile_contOn hn x (fun t ht ↦ (hκ t ht).1) (fun t ht ↦ (hκ t ht).2.1)
  obtain ⟨t, ht, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨t₁, h₁⟩
    (hcont.mono (fun _ ht ↦ hl.trans_le ht.1))
  have ht' := hl.trans_le ht.1
  have harg := scaleProfile_spec hn x (hκ t ht').1 ht'
  have hw : w ≤ scaleProfile x (κ t) t :=
    (((scaleProfile_spec hn x hk₁ ht₁).2 a₁ ha₁).trans
      (isMaxOn_iff.mp (hκ t₁ ht₁).2.2 k₁ hk₁)).trans (isMaxOn_iff.mp hmax t₁ h₁)
  refine ⟨κ t, (hκ t ht').1, t, ht', TransformedGamma.scale x (κ t) t,
    exp_pos _, fun k' hk' t' ht'₁ a' ha' ↦ ?_⟩
  rw [harg.1]
  by_cases htl : t' < l
  · have hdist : dist t' 0 < δ := by
      rw [Real.dist_eq, sub_zero, abs_of_pos ht'₁]
      exact (htl.trans_le (min_le_left _ _)).trans (by linarith)
    have h := hL hdist ht'₁ k' hk' a' ha'
    have hN : N + ε < w := by
      dsimp [ε, w]
      linarith only [le_max_left N P, hW₁]
    exact (h.trans hN).le.trans hw
  by_cases htr : r < t'
  · have h := hR t' ((le_max_left R t₁).trans htr.le) k' hk' a' ha'
    have hP : P + ε < w := by
      dsimp [ε, w]
      linarith only [le_max_right N P, hW₁]
    exact (h.trans hP).le.trans hw
  exact (((scaleProfile_spec hn x hk' ht'₁).2 a' ha').trans
    (isMaxOn_iff.mp (hκ t' ht'₁).2.2 k' hk')).trans
      (isMaxOn_iff.mp hmax t' ⟨le_of_not_gt htl, le_of_not_gt htr⟩)

/-- On nonconstant data, a finite fit exists exactly when a finite
feasible height reaches both optimized exteriors. Every constant
positive sample has unbounded likelihood. This version makes no
finiteness assertion about the stationary powers. -/
theorem attainment_witness_criterion (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) (hj : ∀ i, log (x i) ≤ log (x j)) :
    let V := UnivariateML.Gamma.avg (fun i ↦ (log (x i) - UnivariateML.Gamma.avgLog x)^2)
    let N := -log (2 * π) / 2 - log V / 2 - 1 / 2 - UnivariateML.Gamma.avgLog x
    let P := log (log (x j) - UnivariateML.Gamma.avgLog x)⁻¹ -
      UnivariateML.Gamma.avgLog x - 1
    ((∃ i j, x i ≠ x j) →
      ((∃ k > 0, ∃ t > 0, ∃ a > 0, ∀ k' > 0, ∀ t' > 0, ∀ a' > 0,
          TransformedGamma.logLik x k' t' a' ≤ TransformedGamma.logLik x k t a) ↔
        ∃ k > 0, ∃ t > 0, ∃ a > 0, max N P ≤ TransformedGamma.logLik x k t a)) ∧
    ((∀ i j, x i = x j) →
      ∀ C : ℝ, ∃ k > 0, ∃ t > 0, ∃ a > 0, C < TransformedGamma.logLik x k t a) := by
  intro V N P
  constructor
  · intro hne
    refine ⟨?_, attains_of_face_witness hn hx hne j hj⟩
    rintro ⟨k, hk, t, ht, a, ha, hm⟩
    let μ := UnivariateML.Gamma.avgLog x
    let D := log (x j) - μ
    have hD : 0 < D := max_log_gap_pos hn hx hne j hj
    have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    have hP : P ≤ TransformedGamma.logLik x k t a := by
      have hp := AllFreeTransformedGamma.power_path (x := x) (inv_pos.mpr hD) (hx j)
        (fun i ↦ ⟨hx i, (log_le_log_iff (hx i) (hx j)).mp (hj i)⟩)
      have hmean :
          UnivariateML.Gamma.avg (fun i ↦ D⁻¹ * (log (x i) - log (x j))) = -1 := by
        simp only [UnivariateML.Gamma.avg, ← Finset.mul_sum,
          Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
          Fintype.card_fin, nsmul_eq_mul]
        have he : (∑ i, log (x i)) / n = μ := rfl
        rw [mul_div_assoc, sub_div, mul_div_cancel_left₀ _ hn', he]
        rw [show μ - log (x j) = -D by dsimp [D]; ring,
          mul_neg, inv_mul_cancel₀ hD.ne']
      rw [hmean] at hp
      have hh := le_of_tendsto hp (by
        filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with p hp
        exact hm _ (mul_pos (inv_pos.mpr hD) hp) _ (inv_pos.mpr hp) _ (exp_pos _))
      simpa [P, D, μ, sub_eq_add_neg] using hh
    have hV : 0 < V := by
      obtain ⟨i, l, hil⟩ := hne
      obtain ⟨r, hr⟩ : ∃ r, log (x r) ≠ μ := by
        by_contra! h
        exact hil (log_injOn_pos (hx i) (hx l) ((h i).trans (h l).symm))
      exact div_pos (Finset.sum_pos' (fun _ _ ↦ sq_nonneg _) ⟨r, Finset.mem_univ _,
        sq_pos_of_ne_zero (sub_ne_zero.mpr hr)⟩) (by exact_mod_cast hn)
    have hN : N ≤ TransformedGamma.logLik x k t a := by
      have hσ := sqrt_pos.mpr hV
      have hl := AllFreeTransformedGamma.lognormal_path hn x μ hσ
      have hval : UnivariateML.Gamma.avg (fun i ↦ -log (sqrt V) - log (2 * π) / 2 - log (x i) -
          ((log (x i) - μ) / sqrt V)^2 / 2) = N := by
        have hterm (i : Fin n) : ((log (x i) - μ) / sqrt V)^2 / 2 =
            (log (x i) - μ)^2 / (2 * V) := by
          rw [div_pow, sq_sqrt hV.le, div_div, mul_comm V]
        simp_rw [hterm]
        simp only [log_sqrt hV.le, UnivariateML.Gamma.avg, Finset.sum_sub_distrib,
          Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
        rw [← Finset.sum_div]
        simp only [sub_div, mul_div_cancel_left₀ _ hn']
        rw [div_right_comm (∑ i, (log (x i) - μ)^2) (2 * V) (n : ℝ)]
        change -(log V / 2) - log (2 * π) / 2 - μ -
          (V / (2 * V)) = N
        have he : V / (2 * V) = 1 / 2 := by field_simp
        rw [he]
        dsimp [N, μ]
        ring
      rw [hval] at hl
      apply le_of_tendsto hl
      filter_upwards [eventually_gt_atTop (0 : ℕ)] with r hr
      have hr' : (0 : ℝ) < r := by exact_mod_cast hr
      exact hm _ hr' _ (one_div_pos.mpr (mul_pos hσ (sqrt_pos.mpr hr'))) _ (exp_pos _)
    exact ⟨k, hk, t, ht, a, ha, max_le hN hP⟩
  · intro h C
    have he : x = fun _ ↦ x j := funext fun i ↦ h i j
    have hp := TransformedGamma.heldShape_constant_unbounded hn (x j) 1
    obtain ⟨t, ht, hC⟩ := ((eventually_gt_atTop (0 : ℝ)).and (hp.eventually_gt_atTop C)).exists
    exact ⟨1, one_pos, t, ht, x j, hx j, by rw [he]; exact hC⟩

end
end UnivariateML.GBTTrGamma
end
