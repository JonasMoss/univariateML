module

public import UnivariateML.Family.StudentFaces

import Mathlib.Tactic
import Mathlib.Analysis.Convex.SpecificFunctions.Basic
import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
import Mathlib.Analysis.Real.Pi.Bounds

set_option linter.style.header false

/-!
# The Gaussian face of the Student likelihood

The limits allow both nuisance coordinates to move, including vanishing
ordinary scale when a nonzero limiting residual remains.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Student2Normal

private theorem normalizer_limit :
    Tendsto TDist.logNormalizer atTop (𝓝 (-(log (2 * π) / 2))) := by
  have h := TDist.density_tendsto_normal 0 0 (s := 1) (a := fun _ ↦ 1)
    zero_lt_one tendsto_const_nhds
  have h' : Tendsto (fun v ↦ exp (TDist.logNormalizer v)) atTop
      (𝓝 (exp (-(log (2 * π) / 2)))) := by
    simpa [TDist.density, TDist.normalDensity] using h
  simpa only [log_exp] using h'.log (exp_ne_zero _)

/-- At a finite limiting location and positive limiting ordinary scale, the
joint Student likelihood tends to the matched Gaussian product. -/
theorem moving_normal_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ} {b s : ℝ} (hs : 0 < s)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l
      (𝓝 (∏ i, TDist.normalDensity b s (x i))) := by
  have ht (y : ℝ) : Tendsto (fun j ↦ TDist.density (m j) (v j) (a j) y) l
      (𝓝 (TDist.normalDensity b s y)) := by
    let R := fun j ↦ (y - m j)^2 / (a j)^2
    let q := fun j ↦ R j / v j
    have hR : Tendsto R l (𝓝 ((y - b)^2 / s^2)) :=
      (((tendsto_const_nhds (x := y)).sub hm).pow 2).div (ha.pow 2) (by positivity)
    have hq : Tendsto q l (𝓝 0) := hR.div_atTop hv
    have hu : Tendsto (fun j ↦ v j * log (1 + q j)) l (𝓝 ((y - b)^2 / s^2)) := by
      have hl : Tendsto (fun j ↦ R j / (1 + q j)) l (𝓝 ((y - b)^2 / s^2)) := by
        simpa only [add_zero, div_one, Pi.div_def] using hR.div (hq.const_add 1) (by norm_num)
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le' hl hR ?_ ?_
      · filter_upwards [hv.eventually_gt_atTop 0, ha.eventually (Ioi_mem_nhds hs)] with j hj hj'
        change 0 < a j at hj'
        have hqj : 0 ≤ q j := by dsimp [q, R]; positivity
        have hp : 0 < 1 + q j := by positivity
        have he : R j / (1 + q j) = v j * (1 - (1 + q j)⁻¹) := by
          dsimp [q]
          field_simp [hj.ne', show v j + R j ≠ 0 by
            exact (add_pos_of_pos_of_nonneg hj (by dsimp [R]; positivity)).ne']
          ring
        rw [he]
        exact mul_le_mul_of_nonneg_left (one_sub_inv_le_log_of_pos hp) hj.le
      · filter_upwards [hv.eventually_gt_atTop 0] with j hj
        have hqj : 0 ≤ q j := by dsimp [q, R]; positivity
        calc
          v j * log (1 + q j) ≤ v j * ((1 + q j) - 1) :=
            mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (by positivity)) hj.le
          _ = R j := by rw [add_sub_cancel_left]; exact mul_div_cancel₀ _ hj.ne'
    have hf : Tendsto (fun j ↦ ((1 + 1 / v j) / 2)) l (𝓝 (1 / 2)) := by
      simpa using (((tendsto_const_nhds (x := (1 : ℝ))).div_atTop hv).const_add 1).div_const 2
    have hk : Tendsto (fun j ↦ (v j + 1) / 2 * log (1 + q j)) l
        (𝓝 ((y - b)^2 / s^2 / 2)) := by
      have H := hf.mul hu
      rw [show (1 / 2 : ℝ) * ((y - b)^2 / s^2) = ((y - b)^2 / s^2 / 2) by ring] at H
      refine H.congr' ?_
      filter_upwards [hv.eventually_gt_atTop 0] with j hj
      field_simp [hj.ne']
    have h := ((normalizer_limit.comp hv).sub (ha.log hs.ne')).sub hk
    refine continuous_exp.continuousAt.tendsto.comp (h.congr' ?_)
    exact Eventually.of_forall fun j ↦ by
      dsimp [q, R]
      rw [div_div, mul_comm ((a j)^2)]
  refine (tendsto_finsetProd Finset.univ fun i _ ↦ ht (x i)).congr' ?_
  exact Eventually.of_forall fun j ↦ by
    change (∏ i, TDist.density (m j) (v j) (a j) (x i)) = TDist.likelihood x (m j) (v j) (a j)
    rw [TDist.likelihood, TDist.logLik, exp_sum]
    rfl

private theorem kernel_monotone {N v c : ℝ} (hN : 0 < N) (hv : N ≤ v)
    (hc : 0 ≤ c) : N * log (1 + c / N) ≤ v * log (1 + c / v) := by
  have hv' : 0 < v := hN.trans_le hv
  have hw : 0 ≤ N / v := by positivity
  have hw' : N / v ≤ 1 := (div_le_iff₀ hv').mpr (by simpa using hv)
  have h := strictConcaveOn_log_Ioi.concaveOn.2
    (show (1 + c / N : ℝ) ∈ Ioi 0 by change 0 < _; positivity)
    (show (1 : ℝ) ∈ Ioi 0 by norm_num)
    hw (sub_nonneg.mpr hw') (show N / v + (1 - N / v) = 1 by ring)
  simp only [smul_eq_mul, mul_one, log_one, mul_zero, add_zero] at h
  rw [show N / v * (1 + c / N) + (1 - N / v) = 1 + c / v by field_simp; ring] at h
  have H := mul_le_mul_of_nonneg_left h hv'.le
  rw [← mul_assoc, mul_div_cancel₀ _ hv'.ne'] at H
  simpa only [mul_comm v N, mul_div_cancel_right₀ _ hv'.ne'] using H

/-- At infinite shape and zero scale, a single nonzero limiting residual
forces the whole Student likelihood to vanish. -/
theorem zero_scale_normal_corner {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ} {b : ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l atTop)
    (hx : ∃ i, x i ≠ b) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 0) := by
  obtain ⟨i, hi⟩ := hx
  let N : ℝ := (n : ℝ) + 1
  have hN : 0 < N := by dsimp [N]; positivity
  let d := (x i-b)^2 / 2
  have hd : 0 < d := half_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hi))
  have hdist := (((tendsto_const_nhds (x := x i)).sub hm).pow 2).eventually
    (Ioi_mem_nhds (half_lt_self (sq_pos_of_ne_zero (sub_ne_zero.mpr hi))))
  have hpos := (tendsto_nhdsWithin_iff.mp ha).2
  have hBound : ∀ᶠ j in l, TDist.logLik x (m j) (v j) (a j) ≤
      (N - n) * log (a j) + ((n : ℝ) * TDist.logNormalizer (v j) - N / 2 * log (d/N)) := by
    filter_upwards [hpos, hv.eventually_ge_atTop N, hdist] with j hj hjv hjd
    change 0 < a j at hj
    change d < (x i - m j)^2 at hjd
    have hjv' := hN.trans_le hjv
    let K (k : Fin n) := log (1 + (x k - m j)^2 / (v j * (a j)^2))
    have hK (k : Fin n) : 0 ≤ K k := log_nonneg (le_add_of_nonneg_right (by positivity))
    have hsum : K i ≤ ∑ k, K k :=
      Finset.single_le_sum (fun k _ ↦ hK k) (Finset.mem_univ i)
    have hstep := kernel_monotone hN hjv (show 0 ≤ (x i - m j)^2 / (a j)^2 by positivity)
    rw [div_div, div_div, mul_comm ((a j)^2) N, mul_comm ((a j)^2) (v j)] at hstep
    have hlog : log (d / N) - 2 * log (a j) ≤
        log (1 + (x i - m j)^2 / (N * (a j)^2)) := by
      rw [show 2 * log (a j) = log ((a j)^2) by rw [log_pow]; norm_num, ← log_div (by positivity : d / N ≠ 0) (by positivity)]
      apply log_le_log (by positivity)
      rw [div_div]
      exact (div_le_div_of_nonneg_right hjd.le (by positivity)).trans (le_add_of_nonneg_left zero_le_one)
    have hpen : N * (log (d/N) - 2 * log (a j)) ≤ (v j + 1) * ∑ k, K k := by
      calc
        _ ≤ N * log (1 + (x i - m j)^2 / (N * (a j)^2)) :=
          mul_le_mul_of_nonneg_left hlog hN.le
        _ ≤ v j * K i := hstep
        _ ≤ (v j + 1) * K i := mul_le_mul_of_nonneg_right (by linarith) (hK i)
        _ ≤ _ := mul_le_mul_of_nonneg_left hsum (by positivity)
    have he : TDist.logLik x (m j) (v j) (a j) =
        (n : ℝ) * (TDist.logNormalizer (v j) - log (a j)) - (v j + 1) / 2 * ∑ k, K k := by
      unfold TDist.logLik
      simp [Finset.sum_sub_distrib, ← Finset.mul_sum, K, mul_sub]
    rw [he]
    nlinarith only [hpen]
  have hR := ((normalizer_limit.comp hv).const_mul (n : ℝ)).sub_const (N/2 * log (d/N))
  have hlog := tendsto_log_nhdsGT_zero.comp ha
  have h := (hlog.const_mul_atBot (show 0 < N - n by dsimp [N]; linarith)).atBot_add hR
  exact tendsto_exp_atBot.comp (tendsto_atBot_mono' l hBound h)

private theorem logGamma_differentiable_pos {z : ℝ} (hz : 0 < z) :
    DifferentiableAt ℝ (fun t : ℝ ↦ log (Gamma t)) z :=
  (differentiableAt_Gamma (fun m ↦ ne_of_gt
    ((neg_nonpos.mpr (Nat.cast_nonneg m)).trans_lt hz))).log (Gamma_pos_of_pos hz).ne'

private theorem logGamma_continuous_pos {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun t : ℝ ↦ log (Gamma t)) z :=
  (logGamma_differentiable_pos hz).continuousAt

private theorem normalizer_continuous {ν : ℝ} (hν : 0 < ν) :
    ContinuousAt TDist.logNormalizer ν := by
  unfold TDist.logNormalizer
  refine ContinuousAt.sub (ContinuousAt.sub ?_ ?_) ?_
  · exact (logGamma_continuous_pos (show 0 < (ν+1)/2 by positivity)).comp'
      (f := fun t : ℝ ↦ (t+1)/2) (x := ν) (by fun_prop)
  · exact (logGamma_continuous_pos (show 0 < ν/2 by positivity)).comp'
      (f := fun t : ℝ ↦ t/2) (x := ν) (by fun_prop)
  · exact ((by fun_prop : Continuous (fun t : ℝ ↦ t*π)).continuousAt.log
      (by positivity)).div_const 2


private theorem normalizer_bounded : ∃ C, ∀ v ≥ 1, TDist.logNormalizer v ≤ C := by
  let A : ℝ := -(log (2 * π) / 2) + 1
  obtain ⟨V, hV⟩ := eventually_atTop.mp
    (normalizer_limit.eventually (Iio_mem_nhds (show -(log (2 * π) / 2) < A by dsimp [A]; linarith)))
  have hcont : ContinuousOn TDist.logNormalizer (Icc 1 V) :=
    fun v hv ↦ (normalizer_continuous (zero_lt_one.trans_le hv.1)).continuousWithinAt
  obtain ⟨B, hB⟩ := isCompact_Icc.bddAbove_image hcont
  refine ⟨max A B, fun v hv ↦ ?_⟩
  by_cases h : V ≤ v
  · exact (le_of_lt (hV v h)).trans (le_max_left _ _)
  · exact (hB ⟨v, ⟨hv, le_of_not_ge h⟩, rfl⟩).trans (le_max_right _ _)

/-- At fitted df at least one, large ordinary scale or remote location
forces the likelihood to zero uniformly in the varying df. -/
theorem moving_shape_remote_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (ha : ∀ᶠ j in l, 0 < a j) (hv : ∀ᶠ j in l, 1 ≤ v j)
    (h : Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l (𝓝 0) := by
  obtain ⟨C, hC⟩ := normalizer_bounded
  have ht (y : ℝ) : Tendsto (fun j ↦ TDist.density (m j) (v j) (a j) y) l (𝓝 0) := by
    rcases h with hs | hm
    · have hlim := tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _ C
        (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hs)))
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ hlim
      filter_upwards [ha, hv] with j hj hjv
      have hv' := zero_lt_one.trans_le hjv
      have hk : 0 ≤ log (1+(y-m j)^2/(v j*(a j)^2)) :=
        log_nonneg (le_add_of_nonneg_right (by positivity))
      apply exp_le_exp.mpr
      dsimp [TDist.density]
      linarith [hC _ hjv, mul_nonneg (show 0 ≤ (v j + 1) / 2 by positivity) hk]
    · have hu : Tendsto (fun b : ℝ ↦ |y-b|) (cocompact ℝ) atTop := by
        rw [cocompact_eq_atBot_atTop]
        exact (tendsto_abs_atTop_atTop.comp
          (tendsto_atTop_add_const_left _ _ tendsto_neg_atBot_atTop)).sup
          (tendsto_abs_atBot_atTop.comp
            (tendsto_atBot_add_const_left _ _ tendsto_neg_atTop_atBot))
      have hr := hu.comp hm
      have hlim := tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _ C
        (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hr)))
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ hlim
      filter_upwards [ha, hv, hr.eventually_gt_atTop 0] with j hj hjv hjr
      have hv' := zero_lt_one.trans_le hjv
      change 0 < |y - m j| at hjr
      let R := (y - m j)^2 / (a j)^2
      have hR : 0 < R := div_pos (sq_pos_of_ne_zero (abs_pos.mp hjr)) (sq_pos_of_pos hj)
      have hbase := kernel_monotone zero_lt_one hjv hR.le
      rw [div_one, one_mul, div_div, mul_comm ((a j)^2)] at hbase
      have hK : 0 ≤ log (1+(y-m j)^2/(v j*(a j)^2)) :=
        log_nonneg (le_add_of_nonneg_right (by positivity))
      have hlog : 2 * (log |y - m j| - log (a j)) ≤ log (1+R) := by
        have he : log R = 2 * (log |y - m j| - log (a j)) := by
          dsimp [R]
          rw [← sq_abs (y - m j), log_div (by positivity) (by positivity), log_pow, log_pow]
          push_cast
          ring
        rw [← he]
        exact log_le_log hR (le_add_of_nonneg_left zero_le_one)
      change exp (TDist.logNormalizer (v j) - log (a j) -
        (v j + 1) / 2 * log (1+(y-m j)^2/(v j*(a j)^2))) ≤ exp (C - log |y - m j|)
      apply exp_le_exp.mpr
      nlinarith only [hlog, hbase, hC _ hjv, hK]
  have H := tendsto_finsetProd Finset.univ fun i _ ↦ ht (x i)
  rw [show (∏ _ : Fin n, (0 : ℝ)) = 0 by simp [hn.ne']] at H
  refine H.congr' ?_
  · exact Eventually.of_forall fun j ↦ by
      change (∏ i, TDist.density (m j) (v j) (a j) (x i)) = TDist.likelihood x (m j) (v j) (a j)
      rw [TDist.likelihood, TDist.logLik, exp_sum]
      rfl


/-- The centered unit-scale Student density is bounded by the unit
Gaussian peak for every positive shape. -/
theorem normalizer_le_gaussian {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer q ≤ -(log (2*π)/2) := by
  let t := q/2
  have ht : 0 < t := by unfold t; positivity
  have H := convexOn_log_Gamma.2 (show t ∈ Ioi 0 from ht)
    (show t+1 ∈ Ioi 0 from add_pos ht one_pos)
    (show 0 ≤ (1 : ℝ)/2 by norm_num) (show 0 ≤ (1 : ℝ)/2 by norm_num)
    (show (1 : ℝ)/2+1/2=1 by norm_num)
  simp only [Function.comp_apply,smul_eq_mul] at H
  rw [show (1:ℝ)/2*t+1/2*(t+1)=t+1/2 by ring,
    Gamma_add_one ht.ne',log_mul ht.ne' (Gamma_pos_of_pos ht).ne'] at H
  unfold TDist.logNormalizer
  rw [show (q+1)/2 = t+1/2 by dsimp [t]; ring, show q/2 = t from rfl,
    show q*π = t*(2*π) by unfold t; ring,log_mul ht.ne' (by positivity : (2:ℝ)*π ≠ 0)]
  linarith

/-- A quartic remainder bounds Student log likelihood above the matched
location–scale Gaussian log likelihood, with no data branch excluded. -/
theorem gaussian_log_likelihood_remainder {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    {q s : ℝ} (hq : 0 < q) (hs : 0 < s) :
    TDist.logLik x m q s ≤
      (∑ i, log (TDist.normalDensity m s (x i))) +
        (q+1)/(4*q^2) * ∑ i, ((x i-m)/s)^4 := by
  unfold TDist.logLik
  rw [Finset.mul_sum,← Finset.sum_add_distrib]
  apply Finset.sum_le_sum
  intro i _
  let u := (x i-m)^2/s^2
  have hu : 0 ≤ u := by unfold u; positivity
  let v := u/q
  have hv : 0 ≤ v := div_nonneg hu hq.le
  have hL : v-v^2/2 ≤ log (1+v) := by
    apply le_trans _ (le_log_one_add_of_nonneg hv)
    apply (le_div_iff₀ (show 0 < v+2 by positivity)).2
    have hv3 : 0 ≤ v^2*v := by positivity
    nlinarith
  have hQ : (q+1)/2*log (1+v) ≥ u/2-(q+1)/(4*q^2)*u^2 := by
    have he := mul_le_mul_of_nonneg_left hL (show 0 ≤ (q+1)/2 by positivity)
    have hW : (q+1)/2*(v-v^2/2) =
        u/2-(q+1)/(4*q^2)*u^2+u/(2*q) := by
      unfold v
      field_simp
      ring
    rw [hW] at he
    linarith [div_nonneg hu (show 0 ≤ 2*q by positivity)]
  have h1 : (x i-m)^2/(q*s^2) = v := by unfold v u; field_simp
  have h2 : (((x i-m)/s)^4) = u^2 := by unfold u; field_simp
  rw [h1,h2,TDist.normalDensity,log_exp]
  change TDist.logNormalizer q-log s-(q+1)/2*log (1+v) ≤
    -(log (2*π)/2)-log s-u/2+(q+1)/(4*q^2)*u^2
  linarith [normalizer_le_gaussian hq]


/-- Uniform remote bound for the centered Student density in the
standard-deviation-one chart. It allows shape to approach two. -/
theorem std_unit_density_remote_upper {q r : ℝ} (hq : 2 < q) (hr : 0 < r) :
    TDist.density 0 q (TDist.ordinaryScale q 1) r ≤ 9/r^3 := by
  let a := TDist.ordinaryScale q 1
  have hq0 : 0 < q := by linarith
  have hq2 : 0 < q-2 := sub_pos.mpr hq
  have ha : 0 < a := by dsimp [a,TDist.ordinaryScale]; positivity
  have hW : q*a^2 = q-2 := by
    unfold a TDist.ordinaryScale
    rw [one_mul,sq_sqrt (by positivity)]
    field_simp
  have hLA : log a = (log (q-2)-log q)/2 := by
    unfold a TDist.ordinaryScale
    rw [one_mul,log_sqrt (by positivity),log_div hq2.ne' hq0.ne']
  have hN : TDist.logNormalizer q ≤ 0 := (normalizer_le_gaussian hq0).trans
    (by have h : 0 ≤ log (2*π) := log_nonneg (by linarith [Real.pi_gt_three]); linarith)
  have hlog (x : ℝ) (hx : 0 < x) : 2*log r-log x ≤ log (1+r^2/x) := by
    have h := log_le_log (show 0 < r^2/x by positivity)
      (show r^2/x ≤ 1+r^2/x by linarith)
    rwa [log_div (by positivity) hx.ne',log_pow,Nat.cast_ofNat] at h
  have hc : 0 ≤ log (1+r^2/(q-2)) := log_nonneg (le_add_of_nonneg_right (by positivity))
  have hfinal : TDist.logNormalizer q-log a-(q+1)/2*log (1+r^2/(q-2)) ≤
      2*log 3-3*log r := by
    rcases le_or_gt q 3 with h | h
    · have hLog := log_le_log hq0 h
      have hL : log (q-2) ≤ 0 := log_nonpos hq2.le (by linarith)
      have hP := mul_le_mul_of_nonneg_right (show (3:ℝ)/2 ≤ (q+1)/2 by linarith) hc
      have h' := hlog (q-2) hq2
      rw [hLA]
      linarith [log_nonneg (by norm_num : (1:ℝ) ≤ 3)]
    · have hrat : q/(q-2) ≤ 3 := (div_le_iff₀ hq2).2 (by linarith)
      have hLog := log_le_log (div_pos hq0 hq2) hrat
      rw [log_div hq0.ne' hq2.ne'] at hLog
      have hh := kernel_monotone (N := 3) (v := q) (c := r^2)
        (by norm_num) h.le (sq_nonneg r)
      have hR := div_le_div_of_nonneg_left (sq_nonneg r) hq2 (show q-2 ≤ q by linarith)
      have hL := log_le_log (show 0 < 1+r^2/q by positivity)
        (show 1+r^2/q ≤ 1+r^2/(q-2) by linarith)
      have he := mul_le_mul_of_nonneg_left hL hq0.le
      rw [hLA]
      linarith [hlog 3 (by norm_num), mul_nonneg hc (show 0 ≤ (1:ℝ)/2 by norm_num)]
  have h9 : exp (2*log 3-3*log r) = 9/r^3 := by
    rw [exp_sub,show (2:ℝ)=((2:ℕ):ℝ) by norm_num,show (3:ℝ)=((3:ℕ):ℝ) by norm_num,
      exp_nat_mul,exp_nat_mul,exp_log hr]
    norm_num [exp_log (by norm_num : 0 < (3:ℝ))]
  change TDist.density 0 q a r ≤ 9/r^3
  rw [TDist.density,sub_zero,hW,← h9,exp_le_exp]
  exact hfinal


private theorem kernel_shape_lower {N q z : ℝ} (hN : 1 ≤ N) (hq : 0 < q)
    (hNq : N ≤ q+1) :
    N*log (1+z^2/N) ≤ (q+1)*log (1+z^2/q) := by
  have hN0 : 0 < N := zero_lt_one.trans_le hN
  have hz : 0 ≤ log (1+z^2/q) := log_nonneg (le_add_of_nonneg_right (by positivity))
  rcases le_total q N with hh | hh
  · have hm := div_le_div_of_nonneg_left (sq_nonneg z) hq hh
    have h := log_le_log (by positivity : 0 < 1+z^2/N)
      (show 1+z^2/N ≤ 1+z^2/q by linarith)
    exact (mul_le_mul_of_nonneg_left h hN0.le).trans (mul_le_mul_of_nonneg_right hNq hz)
  · exact (kernel_monotone hN0 hh (sq_nonneg z)).trans
      (mul_le_mul_of_nonneg_right (by linarith) hz)

private theorem normalizer_nonpositive {q : ℝ} (hq : 0 < q) :
    TDist.logNormalizer q ≤ 0 := by
  have h : 0 ≤ log (2*π) := log_nonneg (by linarith [pi_gt_three])
  linarith [normalizer_le_gaussian hq]

/-- Uniform rational Student density envelope on the fitted branch. -/
theorem student_density_rational_upper {m q s y : ℝ} (hq : 1 ≤ q) (hs : 0 < s) :
    TDist.density m q s y ≤ 2*s/(s^2+(y-m)^2) := by
  have hq0 : 0 < q := zero_lt_one.trans_le hq
  let z := (y-m)/s
  have hh := kernel_shape_lower (N := 2) (z := z) (by norm_num) hq0 (by linarith)
  have hL : log ((1+z^2)/2) ≤ log (1+z^2/2) :=
    log_le_log (by positivity) (by linarith)
  have hEq : (y-m)^2/(q*s^2) = z^2/q := by unfold z; field_simp
  unfold TDist.density
  rw [hEq]
  have he : TDist.logNormalizer q-log s-(q+1)/2*log (1+z^2/q) ≤
      -log ((1+z^2)/2)-log s := by linarith [normalizer_nonpositive hq0]
  refine (exp_le_exp.mpr he).trans (le_of_eq ?_)
  rw [exp_sub,exp_neg,exp_log (by positivity),exp_log hs]
  unfold z
  field_simp

/-- Uniform cubic remote bound for ordinary Student density at shape
at least two. -/
theorem student_density_cubic_remote_upper {m q s y : ℝ} (hq : 2 ≤ q) (hs : 0 < s)
    (hy : y ≠ m) :
    TDist.density m q s y ≤ 6*s^2/|y-m|^3 := by
  have hq0 : 0 < q := by linarith
  let z := (y-m)/s
  have hz : z ≠ 0 := div_ne_zero (sub_ne_zero.mpr hy) hs.ne'
  have hh := kernel_shape_lower (N := 3) (z := z) (by norm_num) hq0 (by linarith)
  have hL : log (z^2/3) ≤ log (1+z^2/3) :=
    log_le_log (by positivity) (by linarith)
  have hEq : (y-m)^2/(q*s^2) = z^2/q := by unfold z; field_simp
  unfold TDist.density
  rw [hEq]
  have he : TDist.logNormalizer q-log s-(q+1)/2*log (1+z^2/q) ≤
      -((3:ℝ)/2)*log (z^2/3)-log s := by linarith [normalizer_nonpositive hq0]
  refine (exp_le_exp.mpr he).trans ?_
  have hLog :
      -((3:ℝ)/2)*log (z^2/3) = (3:ℕ)*log (sqrt 3)-(3:ℕ)*log |z| := by
    rw [log_div (by positivity) (by norm_num),← sq_abs z,log_pow,log_sqrt (by norm_num)]
    norm_num
    ring
  rw [hLog,exp_sub,exp_sub,exp_nat_mul,exp_nat_mul,
    exp_log (by positivity : 0 < sqrt 3),exp_log (abs_pos.mpr hz),exp_log hs]
  have hR : (sqrt (3:ℝ))^3 ≤ 6 := by
    rw [show (3:ℕ) = 2+1 by decide,pow_succ,sq_sqrt (by norm_num)]
    have hh : sqrt (3:ℝ) ≤ 2 := by exact (sqrt_le_iff).2 (by norm_num)
    linarith
  have hpos : 0 < |z|^3 := pow_pos (abs_pos.mpr hz) _
  refine (div_le_div_of_nonneg_right (div_le_div_of_nonneg_right hR hpos.le) hs.le).trans
    (le_of_eq ?_)
  unfold z
  rw [abs_div,abs_of_pos hs,div_pow]
  field_simp

end UnivariateML.Student2Normal
end
