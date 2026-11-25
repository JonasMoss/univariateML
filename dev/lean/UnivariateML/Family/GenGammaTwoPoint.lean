module

public import UnivariateML.Family.Gamma
public import UnivariateML.Generic.Limit
public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp
import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Analysis.Real.Pi.Bounds
import Mathlib.NumberTheory.Harmonic.GammaDeriv
import Mathlib.Tactic

set_option linter.style.header false

/-! # Balanced two-point generalized gamma

The standardized log observations are one copy of each of `-1` and `1`
with equal weights. Replicating the pair leaves the mean likelihood unchanged.

## Deviations

The source's small-power and large-power bounds cover the two nonzero
orientations. We treat the lognormal seam separately. The large-power proof
extends the source's bound beyond the shape cutoff by monotonicity there,
so it applies pointwise without requiring a shape optimizer. The dossier's
stronger assertion of strict global monotonicity in the optimized gamma shape
is outside this theorem.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.GenGammaTwoPoint

noncomputable section

/-- Log mean exponential on either orientation of the standardized pair. -/
def cumulant (t : ℝ) : ℝ := log (cosh t)

/-- Mean standardized likelihood after maximizing over location, for positive
absolute log-power `t` and positive gamma shape `k`. -/
def profile (t k : ℝ) : ℝ :=
  log t + (k * log k - k - log (Real.Gamma k)) - k * cumulant t

private theorem gamma_step {k : ℝ} (hk : 0 < k) :
    log (Real.Gamma (k + 1)) = log k + log (Real.Gamma k) := by
  rw [Real.Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne']

private theorem cumulant_eq (t : ℝ) :
    cumulant t = t - log 2 + log (1 + exp (-2 * t)) := by
  have h : cosh t = exp t / 2 * (1 + exp (-2 * t)) := by
    rw [Real.cosh_eq]
    have he : exp t * exp (-2 * t) = exp (-t) := by
      rw [← exp_add]
      congr 1
      ring
    linarith only [he]
  rw [cumulant, h, log_mul (by positivity) (by positivity),
    log_div (by positivity) (by norm_num), log_exp]

private theorem cumulant_lower (t : ℝ) : t - log 2 ≤ cumulant t := by
  rw [cumulant_eq]
  have := log_nonneg (show 1 ≤ 1 + exp (-2 * t) by linarith [exp_pos (-2 * t)])
  linarith

private theorem log_two_bound : log 2 < (7 : ℝ) / 10 :=
  log_two_lt_d9.trans (by norm_num)

/-- Along either sign, absolute log-power grows as the inverse gamma shape. -/
theorem profile_face_limit :
    Tendsto (fun t : ℝ ↦ profile t (1 / t)) atTop (𝓝 (-1)) := by
  have hi : Tendsto (fun t : ℝ ↦ 1 / t) atTop (𝓝 0) := by
    simpa only [one_div] using tendsto_inv_atTop_zero
  have hip : Tendsto (fun t : ℝ ↦ 1 / t) atTop (𝓝[>] 0) := by
    rw [tendsto_nhdsWithin_iff]
    exact ⟨hi, (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦ one_div_pos.mpr ht⟩
  have hl : Tendsto (fun k : ℝ ↦ k * log k) (𝓝[>] 0) (𝓝 0) := by
    simpa [mul_comm] using (tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos)
  have hg : ContinuousAt (fun k : ℝ ↦ log (Real.Gamma (k + 1))) 0 := by
    have h := (Real.hasDerivAt_Gamma_of_pos (s := 1) one_pos).continuousAt
    have hc : ContinuousAt (fun k : ℝ ↦ Real.Gamma (k + 1)) 0 :=
      h.comp_of_eq ((continuousAt_id (x := (0 : ℝ))).add_const (1 : ℝ)) (by norm_num)
    exact hc.log (by simp)
  have hgt : Tendsto (fun t : ℝ ↦ log (Real.Gamma (1 / t + 1))) atTop (𝓝 0) := by
    simpa [Function.comp_def] using hg.tendsto.comp hi
  have he : Tendsto (fun t : ℝ ↦ exp (-2 * t)) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_id.const_mul_atTop_of_neg (by norm_num : (-2 : ℝ) < 0))
  have hlog : Tendsto (fun t : ℝ ↦ log (1 + exp (-2 * t))) atTop (𝓝 0) := by
    simpa using (he.const_add 1).log (by norm_num : (1 + 0 : ℝ) ≠ 0)
  have ha : Tendsto (fun t : ℝ ↦ log 2 - log (1 + exp (-2 * t))) atTop (𝓝 (log 2)) := by
    simpa using hlog.const_sub (log 2)
  have hb : Tendsto (fun t : ℝ ↦ 1 - 1 / t * (log 2 - log (1 + exp (-2 * t))))
      atTop (𝓝 1) := by
    simpa using (hi.mul ha).const_sub (1 : ℝ)
  have h := (((hl.comp hip).sub hi).sub hgt).sub hb
  have h' : Tendsto (fun t : ℝ ↦
      (1 / t * log (1 / t) - 1 / t - log (Real.Gamma (1 / t + 1))) -
        (1 - 1 / t * (log 2 - log (1 + exp (-2 * t))))) atTop (𝓝 (-1)) := by
    simpa [Function.comp_def] using h
  apply h'.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  unfold profile
  rw [cumulant_eq, gamma_step (one_div_pos.mpr ht), show log (1 / t) = -log t by
    rw [one_div, log_inv]]
  field_simp
  ring

private def stirlingMain (x : ℝ) : ℝ := (x - 1 / 2) * log x - x

private def stirlingError (x : ℝ) : ℝ := log (Real.Gamma x) - stirlingMain x

private theorem stirlingMain_deriv {x : ℝ} (hx : 0 < x) :
    HasDerivAt stirlingMain (log x - 1 / (2 * x)) x := by
  convert (((hasDerivAt_id x).sub_const (1 / 2 : ℝ)).mul
    (hasDerivAt_log hx.ne')).sub (hasDerivAt_id x) using 1
  · funext y
    simp [stirlingMain, sub_eq_add_neg, add_comm]
  · simp only [id_eq]
    field_simp
    ring

private theorem log_lower_rational {u : ℝ} (hu : 0 ≤ u) :
    2 * u / (2 + u) ≤ log (1 + u) := by
  let f : ℝ → ℝ := fun v ↦ log (1 + v) - 2 * v / (2 + v)
  have hd (v : ℝ) (hv : v ∈ Ici (0 : ℝ)) :
      HasDerivAt f (v ^ 2 / ((1 + v) * (2 + v) ^ 2)) v := by
    have ha : 0 < 1 + v := by linarith [mem_Ici.mp hv]
    have hb : 0 < 2 + v := by linarith [mem_Ici.mp hv]
    convert (((hasDerivAt_const v (1 : ℝ)).add (hasDerivAt_id v)).log ha.ne').sub
      (((hasDerivAt_id v).const_mul 2).div
        ((hasDerivAt_const v (2 : ℝ)).add (hasDerivAt_id v)) hb.ne') using 1
    · rfl
    · simp only [id_eq, Pi.add_apply]
      field_simp
      ring
  have hm : MonotoneOn f (Ici 0) := monotoneOn_of_hasDerivWithinAt_nonneg
    (convex_Ici _) (fun v hv ↦ (hd v hv).continuousAt.continuousWithinAt)
    (fun v hv ↦ (hd v (interior_subset hv)).hasDerivWithinAt) (by
      intro v hv
      have := mem_Ici.mp (interior_subset hv)
      positivity)
  have h := hm (show (0 : ℝ) ∈ Ici 0 by simp) hu hu
  simpa [f, sub_nonneg] using h

private theorem stirlingError_step {x : ℝ} (hx : 0 < x) :
    stirlingError (x + 1) ≤ stirlingError x := by
  have h := log_lower_rational (show 0 ≤ 1 / x by positivity)
  rw [show 1 + 1 / x = (x + 1) / x by field_simp,
    log_div (by positivity) hx.ne'] at h
  have h' : 1 ≤ (x + 1 / 2) * (log (x + 1) - log x) := by
    have he : (x + 1 / 2) * (2 * (1 / x) / (2 + 1 / x)) = 1 := by
      field_simp
    calc
      1 = _ := he.symm
      _ ≤ _ := mul_le_mul_of_nonneg_left h (by positivity)
  unfold stirlingError stirlingMain
  rw [gamma_step hx]
  linarith only [h']

private theorem stirlingError_nat {n : ℕ} (hn : 1 < n) :
    log (2 * π) / 2 ≤ stirlingError n := by
  have hn0 : n ≠ 0 := by omega
  have hp : (0 : ℝ) < n := by exact_mod_cast Nat.pos_of_ne_zero hn0
  have h := gamma_step hp
  rw [Real.Gamma_nat_eq_factorial] at h
  unfold stirlingError stirlingMain
  linarith only [h, Stirling.le_log_factorial_stirling hn0]

-- Unit shifts decrease the correction. At a large shifted argument, compare
-- it to the factorial bound using a lower secant of the convex log Gamma.
private theorem stirling_lower {k : ℝ} (hk : 0 < k) :
    log (2 * π) / 2 ≤ stirlingError k := by
  have hstep (n : ℕ) : stirlingError (k + n) ≤ stirlingError k := by
    induction n with
    | zero => simp
    | succ n ih =>
      rw [Nat.cast_succ, ← add_assoc]
      exact (stirlingError_step (by positivity)).trans ih
  have happrox (n : ℕ) (hn : 1 < n) :
      log (2 * π) / 2 - k * ((k + 1) / ((n : ℝ) - 1)) ≤ stirlingError (k + n) := by
    have hn1 : (1 : ℝ) < n := by exact_mod_cast hn
    have hp : 0 < (n : ℝ) - 1 := by linarith
    have hn0 : (0 : ℝ) < n := by linarith
    have hnk : (n : ℝ) < k + n := by linarith
    have hc := convexOn_log_Gamma.slope_mono_adjacent
      (show (n : ℝ) - 1 ∈ Ioi 0 from hp)
      (show k + n ∈ Ioi 0 from add_pos hk hn0) (show (n : ℝ) - 1 < n by linarith) hnk
    have hg := gamma_step hp
    rw [sub_add_cancel] at hg
    simp only [Function.comp_apply] at hc
    rw [hg, show (n : ℝ) - ((n : ℝ) - 1) = 1 by ring,
      show k + n - n = k by ring] at hc
    have hc' : k * log ((n : ℝ) - 1) ≤ log (Real.Gamma (k + n)) -
        log (Real.Gamma n) := by
      rw [add_sub_cancel_right, div_one] at hc
      simpa [mul_comm, hg] using (le_div_iff₀ hk).mp hc
    have hq : stirlingMain (k + n) - stirlingMain n ≤ k * log (k + n) := by
      obtain ⟨z, hz, he⟩ := exists_hasDerivAt_eq_slope stirlingMain
        (fun x ↦ log x - 1 / (2 * x)) hnk
        (fun z hz ↦ (stirlingMain_deriv (hn0.trans_le hz.1)).continuousAt.continuousWithinAt)
        (fun z hz ↦ stirlingMain_deriv (hn0.trans hz.1))
      rw [show k + n - n = k by ring] at he
      have hz0 := hn0.trans hz.1
      have hl := log_le_log hz0 hz.2.le
      rw [mul_comm k]
      apply (div_le_iff₀ hk).mp
      rw [← he]
      have : 0 ≤ 1 / (2 * z) := by positivity
      linarith
    have hl := log_le_sub_one_of_pos (div_pos (show 0 < k + n by positivity) hp)
    rw [log_div (by positivity) hp.ne',
      show (k + n) / ((n : ℝ) - 1) - 1 = (k + 1) / ((n : ℝ) - 1) by
        field_simp; ring] at hl
    have hm := mul_le_mul_of_nonneg_left hl hk.le
    have hs := stirlingError_nat hn
    unfold stirlingError at *
    linarith only [hc', hq, hm, hs]
  by_contra! h
  obtain ⟨n, hn⟩ := exists_nat_gt (1 + k * (k + 1) /
    (log (2 * π) / 2 - stirlingError k))
  have hpos : 0 < log (2 * π) / 2 - stirlingError k := sub_pos.mpr h
  have hn' : (1 : ℝ) < n := by
    have : 0 < k * (k + 1) / (log (2 * π) / 2 - stirlingError k) := by positivity
    linarith
  have hn'' : 1 < n := by exact_mod_cast hn'
  have hh : k * ((k + 1) / ((n : ℝ) - 1)) <
      log (2 * π) / 2 - stirlingError k := by
    rw [← mul_div_assoc, div_lt_iff₀ (sub_pos.mpr hn')]
    have hh := (div_lt_iff₀ hpos).mp (show
      k * (k + 1) / (log (2 * π) / 2 - stirlingError k) < (n : ℝ) - 1 by linarith)
    simpa [mul_comm] using hh
  linarith only [happrox n hn'', hstep n, hh]

private def tilt (t : ℝ) : ℝ := sinh t / cosh t

private theorem cumulant_deriv (t : ℝ) : HasDerivAt cumulant (tilt t) t :=
  (hasDerivAt_cosh t).log (cosh_pos t).ne'

private theorem tilt_deriv (t : ℝ) : HasDerivAt tilt (1 / cosh t ^ 2) t := by
  convert (hasDerivAt_sinh t).div (hasDerivAt_cosh t) (cosh_pos t).ne' using 1
  · rfl
  · rw [← sq, ← sq, cosh_sq_sub_sinh_sq]

private theorem tilt_lower {t : ℝ} (ht : 0 ≤ t) : t / cosh t ^ 2 ≤ tilt t := by
  rcases ht.eq_or_lt with rfl | ht
  · simp [tilt]
  obtain ⟨z, hz, he⟩ := exists_hasDerivAt_eq_slope tilt
    (fun z ↦ 1 / cosh z ^ 2) ht
    (fun z _ ↦ (tilt_deriv z).continuousAt.continuousWithinAt) (fun z _ ↦ tilt_deriv z)
  have hc : cosh z ≤ cosh t := cosh_le_cosh.mpr (by
    simpa [abs_of_pos hz.1, abs_of_pos ht] using hz.2.le)
  have hc' : cosh z ^ 2 ≤ cosh t ^ 2 := by gcongr
  simp only [tilt, sinh_zero, zero_div, sub_zero] at he
  have h := one_div_le_one_div_of_le (by positivity : 0 < cosh z ^ 2) hc'
  rw [he] at h
  calc
    t / cosh t ^ 2 = 1 / cosh t ^ 2 * t := by ring
    _ ≤ (tilt t / t) * t := mul_le_mul_of_nonneg_right h ht.le
    _ = tilt t := div_mul_cancel₀ _ ht.ne'

-- The secant of the tilt gives t K''(t) ≤ K'(t). Hence
-- 2 K(t) - t K'(t) grows from zero, which gives the ratio comparison.
private theorem cumulant_ratio_anti :
    AntitoneOn (fun t : ℝ ↦ cumulant t / t ^ 2) (Ioi 0) := by
  let f : ℝ → ℝ := fun t ↦ 2 * cumulant t - t * tilt t
  have hd (t : ℝ) : HasDerivAt f (tilt t - t / cosh t ^ 2) t := by
    convert ((cumulant_deriv t).const_mul 2).sub
      ((hasDerivAt_id t).mul (tilt_deriv t)) using 1
    · rfl
    · simp only [id_eq]
      ring
  have hm : MonotoneOn f (Ici 0) := monotoneOn_of_hasDerivWithinAt_nonneg
    (convex_Ici _) (fun t _ ↦ (hd t).continuousAt.continuousWithinAt)
    (fun t _ ↦ (hd t).hasDerivWithinAt)
    (fun t ht ↦ sub_nonneg.mpr (tilt_lower (interior_subset ht)))
  have hf {t : ℝ} (ht : 0 < t) : 0 ≤ f t := by
    simpa [f, cumulant] using hm (show (0 : ℝ) ∈ Ici 0 by simp) ht.le ht.le
  have hr (t : ℝ) (ht : 0 < t) :
      HasDerivAt (fun t : ℝ ↦ cumulant t / t ^ 2) (-f t / t ^ 3) t := by
    convert (cumulant_deriv t).div ((hasDerivAt_id t).pow 2) (pow_ne_zero _ ht.ne')
      using 1
    · rfl
    · simp only [f, id_eq, Pi.pow_apply]
      field_simp
      ring
  exact antitoneOn_of_hasDerivWithinAt_nonpos (convex_Ioi _)
    (fun t ht ↦ (hr t ht).continuousAt.continuousWithinAt)
    (fun t ht ↦ (hr t (interior_subset ht)).hasDerivWithinAt)
    (fun t ht ↦ div_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr (hf (interior_subset ht)))
      (by have : 0 < t := interior_subset ht; positivity))

private theorem small_profile_lt {t k : ℝ} (ht : 0 < t) (ht3 : t ≤ 3) (hk : 0 < k) :
    profile t k < -1 := by
  have hc := cumulant_ratio_anti ht (show (3 : ℝ) ∈ Ioi 0 by norm_num) ht3
  have hb : exp 1 / (4 * π) < cumulant t / t ^ 2 := by
    have hl := cumulant_lower 3
    have he : exp 1 < (11 : ℝ) / 4 := exp_one_lt_d9.trans (by norm_num)
    have hp := pi_gt_three
    refine lt_of_lt_of_le ?_ hc
    norm_num
    apply (div_lt_iff₀ (show 0 < 4 * π by positivity)).mpr
    have hm := mul_lt_mul_of_pos_right hp (show 0 < (4 : ℝ) * (23 / 90) by norm_num)
    have hv : (23 : ℝ) / 90 < cumulant 3 / 9 := by linarith [log_two_bound]
    nlinarith only [he, hm, mul_lt_mul_of_pos_left hv (show 0 < 4 * π by positivity)]
  have hK : 0 < cumulant t := by
    have h : 0 < cumulant t / t ^ 2 := (by positivity : 0 < exp 1 / (4 * π)).trans hb
    simpa using (lt_div_iff₀ (sq_pos_of_pos ht)).mp h
  have hk2 : 0 < 2 * k * cumulant t := by positivity
  have hlog := log_le_sub_one_of_pos hk2
  have hconst : log (t ^ 2) + 1 < log (4 * π * cumulant t) := by
    have h := (div_lt_div_iff₀ (by positivity : 0 < 4 * π) (by positivity : 0 < t ^ 2)).mp hb
    have hh := log_lt_log (by positivity : 0 < exp 1 * t ^ 2) h
    rw [log_mul (by positivity) (by positivity), log_exp, mul_comm (cumulant t)] at hh
    linarith
  have hs := stirling_lower hk
  rw [log_pow] at hconst
  rw [log_mul (by positivity : (2 * k : ℝ) ≠ 0)
    (by positivity : cumulant t ≠ 0), log_mul (by norm_num : (2 : ℝ) ≠ 0) hk.ne'] at hlog
  rw [show 4 * π * cumulant t = 2 * (2 * π) * cumulant t by ring,
    log_mul (by positivity) (by positivity),
    log_mul (by norm_num : (2 : ℝ) ≠ 0) (by positivity)] at hconst
  norm_num at hconst
  unfold stirlingError stirlingMain at hs
  unfold profile
  linarith only [hlog, hconst, hs]

private theorem logGamma_deriv {x : ℝ} (hx : 0 < x) :
    HasDerivAt (log ∘ Real.Gamma) (Gamma.digamma x) x := by
  rw [Gamma.digamma]
  have h := (Real.hasDerivAt_Gamma_of_pos hx).log (Gamma_pos_of_pos hx).ne'
  exact h.differentiableAt.hasDerivAt

private theorem logGamma_lower {k : ℝ} (hk : 0 < k) :
    -eulerMascheroniConstant * k ≤ log (Real.Gamma (k + 1)) := by
  have h := convexOn_log_Gamma.deriv_le_slope (show (1 : ℝ) ∈ Ioi 0 by norm_num)
    (show k + 1 ∈ Ioi 0 from add_pos hk one_pos) (show 1 < k + 1 by linarith)
    (logGamma_deriv one_pos).differentiableAt
  have hd : HasDerivAt (log ∘ Real.Gamma) (-eulerMascheroniConstant) 1 := by
    simpa [Function.comp_def] using Real.hasDerivAt_Gamma_one.log (show Real.Gamma 1 ≠ 0 by simp)
  rw [hd.deriv] at h
  simp only [slope_def_field, Function.comp_apply, Real.Gamma_one, log_one, sub_zero,
    add_sub_cancel_right] at h
  exact (le_div_iff₀ hk).mp h

private theorem log_le_digamma_succ {k : ℝ} (hk : 0 < k) :
    log k ≤ Gamma.digamma (k + 1) := by
  have hp : 0 < k + 1 := by positivity
  have h := convexOn_log_Gamma.slope_le_deriv (show k ∈ Ioi 0 from hk)
    (show k + 1 ∈ Ioi 0 from hp) (lt_add_one k) (logGamma_deriv hp).differentiableAt
  rw [(logGamma_deriv hp).deriv] at h
  simpa [slope_def_field, gamma_step hk, show k + 1 - k = 1 by ring] using h

private theorem profile_recurrence {t k : ℝ} (ht : 0 < t) (hk : 0 < k) :
    profile t k = log (t * k) + k * (log k - 1) -
      log (Real.Gamma (k + 1)) - k * cumulant t := by
  rw [profile, log_mul ht.ne' hk.ne', gamma_step hk]
  ring

private theorem profile_deriv (t : ℝ) {k : ℝ} (hk : 0 < k) :
    HasDerivAt (profile t) (1 / k + log k - Gamma.digamma (k + 1) - cumulant t) k := by
  have h := ((((hasDerivAt_log hk.ne').const_add (log t)).add
    ((hasDerivAt_id k).mul ((hasDerivAt_log hk.ne').sub_const 1))).sub
    ((logGamma_deriv (show 0 < k + 1 by positivity)).comp k
      ((hasDerivAt_id k).add_const 1))).sub ((hasDerivAt_id k).mul_const (cumulant t))
  have h' : HasDerivAt (fun k ↦ log t + log k + k * (log k - 1) -
      log (Real.Gamma (k + 1)) - k * cumulant t)
      (1 / k + log k - Gamma.digamma (k + 1) - cumulant t) k := by
    convert h using 1
    · rfl
    · simp only [id_eq]
      field_simp
      ring
  apply h'.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hk] with x hx
  unfold profile
  rw [gamma_step hx]
  ring

private theorem large_profile_lt {t k : ℝ} (ht : 3 ≤ t) (hk : 0 < k) :
    profile t k < -1 := by
  have ht0 : 0 < t := by linarith
  have hK : 2 < cumulant t := by linarith [cumulant_lower t, log_two_bound]
  have hK0 : 0 < cumulant t := by linarith
  have hc : 0 < 1 / cumulant t := by positivity
  have hsmall {x : ℝ} (hx : 0 < x) (hxc : x ≤ 1 / cumulant t) :
      profile t x < -1 := by
    have hx2 : 2 * x < 1 := by
      have h := (le_div_iff₀ hK0).mp hxc
      nlinarith [mul_pos hx (sub_pos.mpr hK)]
    have hl2 := log_neg (show 0 < 2 * x by positivity) hx2
    rw [log_mul (by norm_num) hx.ne'] at hl2
    have hg := logGamma_lower hx
    have hlog := log_le_sub_one_of_pos (mul_pos ht0 hx)
    have he : log x + log 2 + eulerMascheroniConstant - 1 < 0 := by
      linarith [eulerMascheroniConstant_lt_two_thirds]
    have h1 := mul_nonpos_of_nonneg_of_nonpos hx.le
      (sub_nonpos.mpr (cumulant_lower t))
    have h2 := mul_neg_of_pos_of_neg hx he
    rw [profile_recurrence ht0 hx]
    linarith only [hg, hlog, h1, h2]
  -- Above the cutoff the derivative of the shape profile is nonpositive.
  by_cases hh : k ≤ 1 / cumulant t
  · exact hsmall hk hh
  have hm : AntitoneOn (profile t) (Ici (1 / cumulant t)) := by
    apply antitoneOn_of_hasDerivWithinAt_nonpos (convex_Ici _)
    · intro x hx
      exact (profile_deriv t (hc.trans_le hx)).continuousAt.continuousWithinAt
    · intro x hx
      exact (profile_deriv t (hc.trans_le (interior_subset hx))).hasDerivWithinAt
    · intro x hx
      have hx' : 1 / cumulant t ≤ x := interior_subset hx
      have hx0 := hc.trans_le hx'
      have hd : 1 / x ≤ cumulant t := by
        calc
          1 / x ≤ 1 / (1 / cumulant t) := one_div_le_one_div_of_le hc hx'
          _ = cumulant t := one_div_one_div _
      linarith [log_le_digamma_succ hx0]
  exact (hm (le_refl _) (le_of_not_ge hh) (le_of_not_ge hh)).trans_lt (hsmall hc le_rfl)

/-- Every finite point of the location profile on either reflected side is below the face. -/
theorem profile_lt_face {t k : ℝ} (ht : 0 < t) (hk : 0 < k) :
    profile t k < -1 := by
  by_cases h : t ≤ 3
  · exact small_profile_lt ht h hk
  · exact large_profile_lt (le_of_not_ge h) hk

/-- Finite standardized Prentice parameters (location, scale, signed shape). -/
def domain : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1}

/-- Log density on the standardized log-observation axis. -/
def logDensity (μ σ Q z : ℝ) : ℝ :=
  if Q = 0 then
    -log σ - log (2 * π) / 2 - ((z - μ) / σ) ^ 2 / 2
  else
    let k := 1 / Q ^ 2
    let w := Q / σ * (z - μ)
    log (|Q| / σ) + (k * log k - k - log (Real.Gamma k)) -
      k * (exp w - 1 - w)

/-- Standardized mean log likelihood for the equally weighted pair. -/
def logLik (p : ℝ × ℝ × ℝ) : ℝ :=
  (logDensity p.1 p.2.1 p.2.2 (-1) + logDensity p.1 p.2.1 p.2.2 1) / 2

private theorem signed_abs_cosh (r : ℝ) : cosh |r| = cosh r := by
  rcases le_total 0 r with h | h
  · rw [abs_of_nonneg h]
  · rw [abs_of_nonpos h, cosh_neg]

/-- Location profiling removes an explicit nonpositive exponential loss. -/
theorem logLik_eq_profile {μ σ Q : ℝ} (hσ : 0 < σ) (hQ : Q ≠ 0) :
    let t := |Q| / σ
    let k := 1 / Q ^ 2
    let u := cumulant t - Q / σ * μ
    logLik (μ, σ, Q) = profile t k + k * (u + 1 - exp u) := by
  have he : (exp (Q / σ * (-1 - μ)) + exp (Q / σ * (1 - μ))) / 2 =
      exp (cumulant (|Q| / σ) - Q / σ * μ) := by
    have habs : |Q| / σ = |Q / σ| := by rw [abs_div, abs_of_pos hσ]
    rw [exp_sub, cumulant, exp_log (cosh_pos _), habs, signed_abs_cosh, Real.cosh_eq]
    rw [show Q / σ * (-1 - μ) = -(Q / σ) - Q / σ * μ by ring,
      show Q / σ * (1 - μ) = Q / σ - Q / σ * μ by ring, exp_sub, exp_sub]
    ring
  dsimp [logLik, logDensity, profile]
  simp only [ite_eq_right hQ]
  linarith only [congrArg (fun v ↦ (1 / Q ^ 2) * v) he]

/-- At fixed nonzero signed shape and positive scale the location profile
is attained at the location that sets the exponential loss to zero. -/
theorem location_profile {σ Q : ℝ} (hσ : 0 < σ) (hQ : Q ≠ 0) :
    IsGreatest (Set.range (fun μ ↦ logLik (μ, σ, Q))) (profile (|Q| / σ) (1 / Q ^ 2)) := by
  have hr : Q / σ ≠ 0 := div_ne_zero hQ hσ.ne'
  have hk : 0 < 1 / Q ^ 2 := one_div_pos.mpr (sq_pos_of_ne_zero hQ)
  constructor
  · refine ⟨cumulant (|Q| / σ) / (Q / σ), ?_⟩
    dsimp only
    rw [logLik_eq_profile hσ hQ, mul_div_cancel₀ _ hr, sub_self, exp_zero,
      zero_add, sub_self, mul_zero, add_zero]
  · rintro _ ⟨μ, rfl⟩
    dsimp only
    rw [logLik_eq_profile hσ hQ]
    have he := add_one_le_exp (cumulant (|Q| / σ) - Q / σ * μ)
    exact add_le_of_nonpos_right (mul_nonpos_of_nonneg_of_nonpos hk.le (by linarith))

private theorem normal_lt_face {μ σ : ℝ} (hσ : 0 < σ) :
    logLik (μ, σ, 0) < -1 := by
  have hlog := log_le_sub_one_of_pos (show 0 < 1 / σ ^ 2 by positivity)
  rw [log_div one_ne_zero (by positivity), log_one, log_pow, Nat.cast_ofNat] at hlog
  have hc : 1 < log (2 * π) := by
    have h : exp 1 < 2 * π := by linarith [exp_one_lt_three, pi_gt_three]
    simpa using log_lt_log (exp_pos 1) h
  have hn : 0 ≤ μ ^ 2 / σ ^ 2 := by positivity
  simp only [logLik, logDensity, ite_true, div_pow]
  have he : (((-1 : ℝ) - μ) ^ 2 / σ ^ 2 + (1 - μ) ^ 2 / σ ^ 2) / 2 =
      1 / σ ^ 2 + μ ^ 2 / σ ^ 2 := by ring
  linarith only [he, hlog, hc, hn]

/-- Every finite Prentice parameter, including the lognormal seam, is strictly below the face. -/
theorem logLik_lt_face {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) : logLik p < -1 := by
  rcases p with ⟨μ, σ, Q⟩
  change 0 < σ at hp
  by_cases hQ : Q = 0
  · subst Q
    exact normal_lt_face hp
  have hk : 0 < 1 / Q ^ 2 := one_div_pos.mpr (sq_pos_of_ne_zero hQ)
  have ht : 0 < |Q| / σ := div_pos (abs_pos.mpr hQ) hp
  exact ((location_profile hp hQ).2 (mem_range_self μ)).trans_lt (profile_lt_face ht hk)

/-- Power path in Prentice coordinates, with scale times shape equal to one. -/
def powerPath (t : ℝ) : ℝ × ℝ × ℝ :=
  (cumulant t / t, 1 / sqrt t, sqrt t)

/-- Pareto path, obtained by reflecting the standardized log axis. -/
def paretoPath (t : ℝ) : ℝ × ℝ × ℝ :=
  (-(cumulant t / t), 1 / sqrt t, -(sqrt t))

private theorem powerPath_eq {t : ℝ} (ht : 0 < t) :
    logLik (powerPath t) = profile t (1 / t) := by
  have hs := sqrt_pos.mpr ht
  have hq : sqrt t / (1 / sqrt t) = t := by
    rw [div_div_eq_mul_div, div_one, ← sq, sq_sqrt ht.le]
  unfold powerPath
  rw [logLik_eq_profile (one_div_pos.mpr hs) hs.ne', abs_of_pos hs, hq, sq_sqrt ht.le,
    mul_div_cancel₀ _ ht.ne', sub_self, exp_zero, zero_add, sub_self, mul_zero, add_zero]

private theorem paretoPath_eq {t : ℝ} (ht : 0 < t) :
    logLik (paretoPath t) = profile t (1 / t) := by
  have hs := sqrt_pos.mpr ht
  have hq : sqrt t / (1 / sqrt t) = t := by
    rw [div_div_eq_mul_div, div_one, ← sq, sq_sqrt ht.le]
  unfold paretoPath
  rw [logLik_eq_profile (one_div_pos.mpr hs) (neg_ne_zero.mpr hs.ne'), abs_neg,
    abs_of_pos hs, hq, neg_sq, sq_sqrt ht.le, neg_div, hq, neg_mul_neg,
    mul_div_cancel₀ _ ht.ne', sub_self, exp_zero, zero_add, sub_self, mul_zero, add_zero]

/-- The power likelihood tends to the common face value. -/
theorem power_limit : Tendsto (logLik ∘ powerPath) atTop (𝓝 (-1)) :=
  profile_face_limit.congr' ((eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ (powerPath_eq ht).symm)

/-- The Pareto likelihood tends to the common face value. -/
theorem pareto_limit : Tendsto (logLik ∘ paretoPath) atTop (𝓝 (-1)) :=
  profile_face_limit.congr' ((eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ (paretoPath_eq ht).symm)

/-- The standardized supremum is minus one and no finite parameter attains it. -/
theorem balanced_two_point_noMLE :
    sSup (logLik '' domain) = -1 ∧ ¬ ∃ p ∈ domain, IsMaxOn logLik domain p := by
  have hp : ∀ᶠ t : ℝ in atTop, powerPath t ∈ domain :=
    (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ one_div_pos.mpr (sqrt_pos.mpr ht)
  have hu : (-1 : ℝ) ∈ upperBounds (logLik '' domain) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (logLik_lt_face hp).le
  have he : sSup (logLik '' domain) = -1 := le_antisymm
    (csSup_le ⟨logLik (0, 1, 0), mem_image_of_mem _ (by norm_num [domain])⟩ hu)
    (Generic.limit_le_sSup hp power_limit ⟨-1, hu⟩)
  refine ⟨he, ?_⟩
  rintro ⟨p, hp', hm⟩
  have h := Generic.limit_le_upperBound hp power_limit (u := logLik p) (by
    rintro _ ⟨q, hq, rfl⟩
    exact hm hq)
  exact (logLik_lt_face hp').not_ge h

/-- The standardized logs of a positive pair with a nonzero gap are exactly
the two signs under centering at the log midpoint and dividing by the half-gap. -/
theorem standardize_pair {a b : ℝ} (ha : 0 < a) (hab : a < b) :
    let m := (log a + log b) / 2
    let d := (log b - log a) / 2
    (log a - m) / d = -1 ∧ (log b - m) / d = 1 := by
  have hl := log_lt_log ha hab
  dsimp
  constructor <;> field_simp <;> ring

private theorem location_limit :
    Tendsto (fun t : ℝ ↦ cumulant t / t) atTop (𝓝 1) := by
  have hi : Tendsto (fun t : ℝ ↦ 1 / t) atTop (𝓝 0) := by
    simpa only [one_div] using tendsto_inv_atTop_zero
  have he : Tendsto (fun t : ℝ ↦ exp (-2 * t)) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_id.const_mul_atTop_of_neg (by norm_num : (-2 : ℝ) < 0))
  have hl : Tendsto (fun t : ℝ ↦ log (1 + exp (-2 * t))) atTop (𝓝 0) := by
    simpa using (he.const_add 1).log (by norm_num : (1 + 0 : ℝ) ≠ 0)
  have h : Tendsto (fun t : ℝ ↦ 1 + (log (1 + exp (-2 * t)) - log 2) * (1 / t))
      atTop (𝓝 1) := by
    simpa using ((hl.sub_const (log 2)).mul hi).const_add (1 : ℝ)
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  rw [cumulant_eq]
  field_simp
  ring

/-- The power path moves the location to the upper log endpoint, sends the
shape to positive infinity, and keeps the scale–shape product equal to one. -/
theorem power_coordinates :
    Tendsto (fun t ↦ (powerPath t).1) atTop (𝓝 1) ∧
    Tendsto (fun t ↦ (powerPath t).2.2) atTop atTop ∧
    ∀ t > 0, (powerPath t).2.1 * |(powerPath t).2.2| = 1 := by
  refine ⟨location_limit, tendsto_sqrt_atTop, ?_⟩
  intro t ht
  simp [powerPath, abs_of_pos (sqrt_pos.mpr ht), (sqrt_pos.mpr ht).ne']

/-- The Pareto path moves the location to the lower log endpoint, sends the
shape to negative infinity, and keeps the scale–absolute-shape product equal to one. -/
theorem pareto_coordinates :
    Tendsto (fun t ↦ (paretoPath t).1) atTop (𝓝 (-1)) ∧
    Tendsto (fun t ↦ (paretoPath t).2.2) atTop atBot ∧
    ∀ t > 0, (paretoPath t).2.1 * |(paretoPath t).2.2| = 1 := by
  refine ⟨location_limit.neg, tendsto_neg_atTop_atBot.comp tendsto_sqrt_atTop, ?_⟩
  intro t ht
  simp [paretoPath, abs_of_pos (sqrt_pos.mpr ht), (sqrt_pos.mpr ht).ne']

/-- Replicating the two standardized observations with equal positive counts
gives the same mean log likelihood. The equivalence allows any observation order. -/
theorem balanced_mean_eq {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    (p : ℝ × ℝ × ℝ) :
    (∑ i : Fin n, logDensity p.1 p.2.1 p.2.2
      (if (e i).1 = 0 then -1 else 1)) / n = logLik p := by
  classical
  let f : Fin 2 × Fin m → ℝ := fun j ↦
    logDensity p.1 p.2.1 p.2.2 (if j.1 = 0 then -1 else 1)
  have hn : n = 2 * m := by simpa using Fintype.card_congr e
  have he : (∑ i : Fin n, f (e i)) = ∑ j : Fin 2 × Fin m, f j :=
    e.sum_comp f
  change (∑ i : Fin n, f (e i)) / n = _
  rw [he, Fintype.sum_prod_type]
  simp [f, Fin.sum_univ_two, logLik, hn]
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  field_simp

private theorem logDensity_standardize {μ σ Q y m d : ℝ} (hσ : 0 < σ) (hd : 0 < d) :
    logDensity μ σ Q y =
      logDensity ((μ - m) / d) (σ / d) Q ((y - m) / d) - log d := by
  have harg : (((y - m) / d) - ((μ - m) / d)) / (σ / d) = (y - μ) / σ := by
    field_simp
    ring
  by_cases hQ : Q = 0
  · simp only [logDensity, ite_eq_left hQ, harg]
    rw [log_div hσ.ne' hd.ne']
    ring
  · have he : Q / (σ / d) * (((y - m) / d) - ((μ - m) / d)) = Q / σ * (y - μ) := by
      calc
        _ = Q * ((((y - m) / d) - ((μ - m) / d)) / (σ / d)) := by ring
        _ = _ := by rw [harg]; ring
    have hlog : log (|Q| / (σ / d)) = log (|Q| / σ) + log d := by
      rw [div_div_eq_mul_div, mul_div_assoc, log_mul (abs_ne_zero.mpr hQ)
        (div_ne_zero hd.ne' hσ.ne'), log_div hd.ne' hσ.ne',
        log_div (abs_ne_zero.mpr hQ) hσ.ne']
      ring
    simp only [logDensity, ite_eq_right hQ, he, hlog]
    ring

/-- Balanced raw positive observations reduce to the standardized pair.
The additive shift consists of the log-coordinate scale and the log Jacobian. -/
theorem balanced_sample_transfer {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    {x : Fin n → ℝ} {a b : ℝ} (ha : 0 < a) (hab : a < b)
    (hx : ∀ i, x i = if (e i).1 = 0 then a else b)
    {μ σ Q : ℝ} (hσ : 0 < σ) :
    let c := (log a + log b) / 2
    let d := (log b - log a) / 2
    (∑ i : Fin n, (logDensity μ σ Q (log (x i)) - log (x i))) / n =
      logLik ((μ - c) / d, σ / d, Q) - log d - c := by
  classical
  let c := (log a + log b) / 2
  let d := (log b - log a) / 2
  let f : Fin 2 × Fin m → ℝ := fun j ↦
    logDensity μ σ Q (log (if j.1 = 0 then a else b)) - log (if j.1 = 0 then a else b)
  have hd : 0 < d := by dsimp [d]; linarith [log_lt_log ha hab]
  have hn : n = 2 * m := by simpa using Fintype.card_congr e
  have he : (∑ i : Fin n, f (e i)) = ∑ j : Fin 2 × Fin m, f j := e.sum_comp f
  change _ = logLik ((μ - c) / d, σ / d, Q) - log d - c
  simp_rw [hx]
  change (∑ i : Fin n, f (e i)) / n = _
  have hsum : (∑ j : Fin 2 × Fin m, f j) / n =
      ((m : ℝ) * (logDensity μ σ Q (log a) - log a) +
        (m : ℝ) * (logDensity μ σ Q (log b) - log b)) / ((2 : ℝ) * m) := by
    rw [hn, Fintype.sum_prod_type]
    simp [f, Fin.sum_univ_two, mul_sub, sub_add_sub_comm]
  rw [he, hsum]
  rw [logDensity_standardize hσ hd (y := log a) (m := c),
    logDensity_standardize hσ hd (y := log b) (m := c)]
  obtain ⟨hleft, hright⟩ := standardize_pair ha hab
  change (log a - c) / d = -1 at hleft
  change (log b - c) / d = 1 at hright
  rw [hleft, hright]
  dsimp [logLik, c]
  have hm' : (m : ℝ) ≠ 0 := by exact_mod_cast hm.ne'
  field_simp
  ring

/-- Mean log likelihood on the positive observation axis, including its Jacobian. -/
def sampleLogLik {n : ℕ} (x : Fin n → ℝ) (p : ℝ × ℝ × ℝ) : ℝ :=
  (∑ i, (logDensity p.1 p.2.1 p.2.2 (log (x i)) - log (x i))) / n

/-- For any equally replicated positive pair, the raw mean log-likelihood
supremum is the standardized common face value plus the data-dependent shift. -/
theorem balanced_sample_noMLE {n m : ℕ} (hm : 0 < m) (e : Fin n ≃ Fin 2 × Fin m)
    {x : Fin n → ℝ} {a b : ℝ} (ha : 0 < a) (hab : a < b)
    (hx : ∀ i, x i = if (e i).1 = 0 then a else b) :
    let c := (log a + log b) / 2
    let d := (log b - log a) / 2
    sSup (sampleLogLik x '' domain) = -1 - log d - c ∧
      (∀ p ∈ domain, sampleLogLik x p < -1 - log d - c) ∧
      ¬ ∃ p ∈ domain, IsMaxOn (sampleLogLik x) domain p := by
  let c := (log a + log b) / 2
  let d := (log b - log a) / 2
  have hd : 0 < d := by dsimp [d]; linarith [log_lt_log ha hab]
  let restore : ℝ × ℝ × ℝ → ℝ × ℝ × ℝ := fun p ↦ (c + d * p.1, d * p.2.1, p.2.2)
  have hrestore {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) : restore p ∈ domain :=
    mul_pos hd hp
  have htransfer {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
      sampleLogLik x (restore p) = logLik p - log d - c := by
    change 0 < p.2.1 at hp
    have h := balanced_sample_transfer hm e ha hab hx
      (μ := c + d * p.1) (Q := p.2.2) (mul_pos hd hp)
    change sampleLogLik x (restore p) = _ at h
    have hμ : (c + d * p.1 - c) / d = p.1 := by field_simp; ring
    have hσ : d * p.2.1 / d = p.2.1 := by field_simp
    change sampleLogLik x (restore p) =
      logLik ((c + d * p.1 - c) / d, d * p.2.1 / d, p.2.2) - log d - c at h
    simpa only [hμ, hσ, Prod.eta] using h
  have hstrict {p : ℝ × ℝ × ℝ} (hp : p ∈ domain) :
      sampleLogLik x p < -1 - log d - c := by
    rcases p with ⟨μ, σ, Q⟩
    change 0 < σ at hp
    rw [show sampleLogLik x (μ, σ, Q) = logLik ((μ - c) / d, σ / d, Q) - log d - c from
      balanced_sample_transfer hm e ha hab hx hp]
    have h := logLik_lt_face (p := ((μ - c) / d, σ / d, Q)) (div_pos hp hd)
    linarith
  have hp : ∀ᶠ t : ℝ in atTop, powerPath t ∈ domain :=
    (eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ one_div_pos.mpr (sqrt_pos.mpr ht)
  have hfeas : ∀ᶠ t : ℝ in atTop, restore (powerPath t) ∈ domain := hp.mono fun _ ↦ hrestore
  have hl : Tendsto (fun t ↦ sampleLogLik x (restore (powerPath t))) atTop
      (𝓝 (-1 - log d - c)) :=
    ((power_limit.sub_const (log d)).sub_const c).congr' (hp.mono fun _ ht ↦ (htransfer ht).symm)
  have hu : (-1 - log d - c) ∈ upperBounds (sampleLogLik x '' domain) := by
    rintro _ ⟨p, hp, rfl⟩
    exact (hstrict hp).le
  refine ⟨le_antisymm
    (csSup_le ⟨sampleLogLik x (0, 1, 0), mem_image_of_mem _ (by norm_num [domain])⟩ hu)
    (Generic.limit_le_sSup hfeas hl ⟨_, hu⟩), fun _ hp ↦ hstrict hp, ?_⟩
  rintro ⟨p, hp', hm⟩
  have h := Generic.limit_le_upperBound hfeas hl (u := sampleLogLik x p) (by
    rintro _ ⟨q, hq, rfl⟩
    exact hm hq)
  exact (hstrict hp').not_ge h

end

end UnivariateML.GenGammaTwoPoint
