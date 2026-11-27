module

public import UnivariateML.Generic.AE4Gaussian
public import UnivariateML.Generic.UniqueAE
public import Mathlib.MeasureTheory.Integral.DominatedConvergence
public import Mathlib.MeasureTheory.Integral.IntervalIntegral.FundThmCalculus

import Mathlib.Tactic

set_option linter.style.header false

/-!
# The positive-width Bhattacharjee score ordering

## Deviations

The a.e. comparison holds at fixed center and Gaussian scale, including
width zero. It does not characterize stationary widths, the normal threshold
or exceptional crossings.
-/

@[expose] public section

open Set Real MeasureTheory Filter ContinuousLinearMap
open scoped Topology
open UnivariateML.SkewLSGaussian

namespace UnivariateML.AE4Bhatt

noncomputable section

private theorem average_strict {f w : ℝ → ℝ} (hf : Continuous f) (hw : Continuous w)
    (hwpos : ∀ t, 0 < w t) (hmono : StrictMonoOn f (Ici 0))
    {a b : ℝ} (ha : 0 < a) (hab : a < b) :
    (∫ v in 0..a, f v*w v) / (∫ v in 0..a, w v) <
      (∫ v in 0..b, f v*w v) / (∫ v in 0..b, w v) := by
  have hf' := hf.mul hw
  let A := ∫ v in 0..a, w v
  let B := ∫ v in a..b, w v
  let P := ∫ v in 0..a, f v*w v
  let Q := ∫ v in a..b, f v*w v
  have hA : 0 < A := intervalIntegral.integral_pos ha hw.continuousOn
    (fun t _ ↦ (hwpos t).le) ⟨0,⟨le_rfl,ha.le⟩,hwpos 0⟩
  have hB : 0 < B := intervalIntegral.integral_pos hab hw.continuousOn
    (fun t _ ↦ (hwpos t).le) ⟨a,⟨le_rfl,hab.le⟩,hwpos a⟩
  have hP : P < f a * A := by
    unfold P A
    rw [← intervalIntegral.integral_const_mul]
    apply intervalIntegral.integral_lt_integral_of_continuousOn_of_le_of_exists_lt ha
      hf'.continuousOn (hw.const_mul _).continuousOn
    · intro x hx
      exact mul_le_mul_of_nonneg_right (hmono.monotoneOn hx.1.le ha.le hx.2) (hwpos x).le
    · exact ⟨0,⟨le_rfl,ha.le⟩,mul_lt_mul_of_pos_right (hmono le_rfl ha.le ha) (hwpos 0)⟩
  have hQ : f a * B < Q := by
    unfold Q B
    rw [← intervalIntegral.integral_const_mul]
    apply intervalIntegral.integral_lt_integral_of_continuousOn_of_le_of_exists_lt hab
      (hw.const_mul _).continuousOn hf'.continuousOn
    · intro x hx
      exact mul_le_mul_of_nonneg_right
        (hmono.monotoneOn ha.le (ha.trans hx.1).le hx.1.le) (hwpos x).le
    · exact ⟨b,⟨hab.le,le_rfl⟩,
        mul_lt_mul_of_pos_right (hmono ha.le (ha.trans hab).le hab) (hwpos b)⟩
  have hW := intervalIntegral.integral_add_adjacent_intervals (μ := volume)
    (hw.intervalIntegrable 0 a) (hw.intervalIntegrable a b)
  have hF := intervalIntegral.integral_add_adjacent_intervals (μ := volume)
    (hf'.intervalIntegrable 0 a) (hf'.intervalIntegrable a b)
  change A+B = _ at hW
  change P+Q = _ at hF
  change P/A < (∫ v in 0..b, (f*w) v) / (∫ v in 0..b, w v)
  rw [← hW, ← hF]
  have hPB : P*B < (f a*A)*B := mul_lt_mul_of_pos_right hP hB
  have hQA : (f a*B)*A < Q*A := mul_lt_mul_of_pos_right hQ hA
  apply (div_lt_div_iff₀ hA (add_pos hA hB)).mpr
  have hPQ : P*B < Q*A := calc
    P*B < (f a*A)*B := hPB
    _ = (f a*B)*A := by ring
    _ < Q*A := hQA
  linarith only [hPQ]

private def W (d s v : ℝ) := φ ((d-v)/s) + φ ((d+v)/s)

private def tilt (d s v : ℝ) := v * (1 - 2/(1 + exp (2*d*v/s^2)))

private theorem tilt_mul_W (d s v : ℝ) (hs : 0 < s) :
    tilt d s v * W d s v = v * (φ ((d-v)/s) - φ ((d+v)/s)) := by
  have he : φ ((d-v)/s) = exp (2*d*v/s^2) * φ ((d+v)/s) := by
    rw [phi_formula, phi_formula, mul_left_comm, ← exp_add]
    congr 2
    field_simp [hs.ne']
    ring
  unfold tilt W
  rw [he]
  have hne : 1 + exp (2*d*v/s^2) ≠ 0 := (add_pos one_pos (exp_pos _)).ne'
  field_simp [hne]
  ring

private theorem tilt_strict {d s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    StrictMonoOn (tilt d s) (Ici 0) := by
  have hk : 0 < 2*d/s^2 := by positivity
  let g (v : ℝ) := 1 - 2/(1 + exp (2*d*v/s^2))
  have hden (v : ℝ) : 0 < 1 + exp (2*d*v/s^2) := by positivity
  have hmono : StrictMono g := by
    intro x y hxy
    have hxy' : 2*d*x/s^2 < 2*d*y/s^2 := by
      exact div_lt_div_of_pos_right (mul_lt_mul_of_pos_left hxy (by positivity))
        (sq_pos_of_pos hs)
    have he := exp_lt_exp.mpr hxy'
    unfold g
    exact sub_lt_sub_left (div_lt_div_of_pos_left (by norm_num) (hden x)
      (add_lt_add_right he 1)) 1
  have hg0 : g 0 = 0 := by norm_num [g]
  intro x hx y _ hxy
  change x*g x < y*g y
  have hx0 : 0 ≤ x := hx
  have hy0 : 0 < y := hx0.trans_lt hxy
  have hgx : 0 ≤ g x := hg0 ▸ hmono.monotone hx0
  exact (mul_le_mul_of_nonneg_right hxy.le hgx).trans_lt
    (mul_lt_mul_of_pos_left (hmono hxy) hy0)

/-- The tilted conditional mean for a Bhattacharjee fit with residual
`d`, Gaussian scale `s`, and positive uniform half-width `a`. -/
def tiltedMean (d s a : ℝ) : ℝ :=
  (∫ v in 0..a, v * (φ ((d-v)/s) - φ ((d+v)/s))) /
    (∫ v in 0..a, (φ ((d-v)/s) + φ ((d+v)/s)))

/-- At a positive residual and fixed Gaussian scale, the conditional mean
of the uniform displacement strictly increases with its positive width. -/
theorem tiltedMean_strictMono {d s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    StrictMonoOn (tiltedMean d s) (Ioi 0) := by
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun z ↦ (phi_hasDerivAt z).continuousAt)
  have hW : Continuous (W d s) := by unfold W; fun_prop
  have ht : Continuous (tilt d s) := by
    unfold tilt
    apply continuous_id.mul (continuous_const.sub _)
    exact continuous_const.div (by fun_prop) (fun x ↦
      (add_pos one_pos (exp_pos (2*d*x/s^2))).ne')
  intro a ha b _ hab
  have h := average_strict ht hW (fun v ↦ add_pos (phi_pos _) (phi_pos _))
    (tilt_strict hd hs) ha hab
  simp_rw [tilt_mul_W d s _ hs] at h
  exact h


/-- The spatial score of the positive-width Gaussian–uniform convolution. -/
def spatialScore (d s a : ℝ) : ℝ :=
  (φ ((d+a)/s) - φ ((d-a)/s)) / (s*(Φ ((d+a)/s) - Φ ((d-a)/s)))

private theorem score_formula (d : ℝ) {s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    spatialScore d s a = (tiltedMean d s a - d)/s^2 := by
  let V (v : ℝ) := v*(φ ((d-v)/s) - φ ((d+v)/s))
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hc : Continuous (W d s) := by unfold W; fun_prop
  have hv : Continuous V := by unfold V; fun_prop
  have hw : 0 < ∫ v in 0..a, W d s v := intervalIntegral.integral_pos ha hc.continuousOn
    (fun t _ ↦ (add_pos (phi_pos _) (phi_pos _)).le)
    ⟨0,⟨le_rfl,ha.le⟩,add_pos (phi_pos _) (phi_pos _)⟩
  have hPlus (v : ℝ) : HasDerivAt (fun y ↦ (d+y)/s) (1/s) v :=
    ((hasDerivAt_id v).const_add d).div_const s
  have hMinus (v : ℝ) : HasDerivAt (fun y ↦ (d-y)/s) (-1/s) v :=
    ((hasDerivAt_id v).const_sub d).div_const s
  have hMass (v : ℝ) : HasDerivAt (fun y ↦ Φ ((d+y)/s) - Φ ((d-y)/s)) (W d s v / s) v := by
    convert ((cdf_hasDerivAt _).comp v (hPlus v)).sub
      ((cdf_hasDerivAt _).comp v (hMinus v)) using 1
    · rfl
    · unfold W
      ring
  have hEnd (v : ℝ) : HasDerivAt (fun y ↦ φ ((d+y)/s)-φ ((d-y)/s))
      ((V v-d*W d s v)/s^2) v := by
    convert ((phi_hasDerivAt _).comp v (hPlus v)).sub
      ((phi_hasDerivAt _).comp v (hMinus v)) using 1
    · rfl
    · unfold V W
      ring
  have hi := intervalIntegral.integral_eq_sub_of_hasDerivAt (fun v _ ↦ hMass v)
    ((hc.div_const s).intervalIntegrable 0 a)
  have hj := intervalIntegral.integral_eq_sub_of_hasDerivAt (fun v _ ↦ hEnd v)
    (((hv.sub (hc.const_mul d)).div_const (s^2)).intervalIntegrable 0 a)
  simp only [add_zero, sub_zero, sub_self] at hi hj
  rw [intervalIntegral.integral_div] at hi hj
  rw [intervalIntegral.integral_sub (hv.intervalIntegrable 0 a)
    ((hc.const_mul d).intervalIntegrable 0 a), intervalIntegral.integral_const_mul] at hj
  unfold spatialScore tiltedMean
  change (φ ((d+a)/s) - φ ((d-a)/s)) / (s*(Φ ((d+a)/s) - Φ ((d-a)/s))) =
    ((∫ v in 0..a, V v) / (∫ v in 0..a, W d s v) - d)/s^2
  rw [← hi, ← hj]
  field_simp [hw.ne', hs.ne']

/-- At a positive residual the Bhattacharjee spatial score strictly
increases with positive width, at held center and Gaussian scale. -/
theorem spatialScore_strictMono {d s : ℝ} (hd : 0 < d) (hs : 0 < s) :
    StrictMonoOn (spatialScore d s) (Ioi 0) := by
  intro a ha b hb hab
  rw [score_formula d hs ha, score_formula d hs hb]
  exact div_lt_div_of_pos_right
    (sub_lt_sub_right (tiltedMean_strictMono hd hs ha hb hab) d) (sq_pos_of_pos hs)


/-- Unnormalized paired Gaussian average on a fixed integration interval. -/
def mass (d s a : ℝ) :=
  ∫ v in (0 : ℝ)..1, φ ((d-a*v)/s) + φ ((d+a*v)/s)

/-- First moment of the displacement in the same paired integral. -/
def moment (d s a : ℝ) :=
  ∫ v in (0 : ℝ)..1, (a*v)*(φ ((d-a*v)/s) - φ ((d+a*v)/s))

private theorem mass_pos (d s a : ℝ) : 0 < mass d s a := by
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  apply intervalIntegral.integral_pos zero_lt_one (by fun_prop)
  · exact fun _ _ ↦ (add_pos (phi_pos _) (phi_pos _)).le
  · exact ⟨0,⟨le_rfl,zero_le_one⟩,add_pos (phi_pos _) (phi_pos _)⟩

private theorem mass_formula (d : ℝ) {s a : ℝ} (hs : 0 < s) (ha : 0 < a) :
    mass d s a = s/a*(Φ ((d+a)/s)-Φ ((d-a)/s)) := by
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hw : Continuous (W d s) := by unfold W; fun_prop
  have hd (v : ℝ) :
      HasDerivAt (fun y ↦ Φ ((d+y)/s)-Φ ((d-y)/s)) (W d s v/s) v := by
    have h₁ := (cdf_hasDerivAt ((d+v)/s)).comp v
      (((hasDerivAt_id v).const_add d).div_const s)
    have h₂ := (cdf_hasDerivAt ((d-v)/s)).comp v
      (((hasDerivAt_id v).const_sub d).div_const s)
    convert h₁.sub h₂ using 1
    · rfl
    · unfold W
      ring
  have he := intervalIntegral.integral_eq_sub_of_hasDerivAt (fun v _ ↦ hd v)
    ((hw.div_const s).intervalIntegrable 0 a)
  rw [intervalIntegral.integral_div] at he
  simp only [add_zero, sub_zero, sub_self] at he
  have hscale :=
    intervalIntegral.smul_integral_comp_mul_left (W d s) (a := (0 : ℝ)) (b := 1) a
  change a * mass d s a = _ at hscale
  rw [mul_zero, mul_one] at hscale
  rw [← hscale] at he
  field_simp [ha.ne', hs.ne'] at he ⊢
  linear_combination he

private def continuousScore (m s a x : ℝ) :=
  (moment (x-m) s a / mass (x-m) s a - (x-m))/s^2

private theorem mean_scale (d s : ℝ) {a : ℝ} (ha : 0 < a) :
    moment d s a / mass d s a = tiltedMean d s a := by
  let V (v : ℝ) := v*(φ ((d-v)/s) - φ ((d+v)/s))
  have hm := intervalIntegral.smul_integral_comp_mul_left V (a := (0 : ℝ)) (b := 1) a
  have hw := intervalIntegral.smul_integral_comp_mul_left (W d s) (a := (0 : ℝ)) (b := 1) a
  change a*moment d s a = _ at hm
  change a*mass d s a = _ at hw
  simp only [mul_zero, mul_one] at hm hw
  change _ = (∫ v in 0..a, V v)/(∫ v in 0..a, W d s v)
  rw [← hm, ← hw, mul_div_mul_left _ _ ha.ne']

private theorem moment_pos (d : ℝ) {s a : ℝ} (hd : 0 < d) (hs : 0 < s) (ha : 0 < a) :
    0 < moment d s a := by
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have h (v : ℝ) (hv : 0 < v) :
      0 < (a*v)*(φ ((d-a*v)/s)-φ ((d+a*v)/s)) := by
    rw [phi_formula, phi_formula]
    have hab : ((d-a*v)/s)^2 < ((d+a*v)/s)^2 := by
      rw [div_pow, div_pow, div_lt_div_iff_of_pos_right (sq_pos_of_pos hs)]
      nlinarith [mul_pos hd (mul_pos ha hv)]
    exact mul_pos (mul_pos ha hv) (sub_pos.mpr
      (mul_lt_mul_of_pos_left (exp_lt_exp.mpr (by linarith)) (by positivity)))
  apply intervalIntegral.integral_pos zero_lt_one (by fun_prop)
  · intro v hv
    exact (h v hv.1).le
  · exact ⟨1,⟨zero_le_one,le_rfl⟩,h 1 zero_lt_one⟩

private theorem even_phi (x : ℝ) : φ (-x) = φ x := by simp [phi_formula]

private theorem mass_reflect (d s a : ℝ) : mass (-d) s a = mass d s a := by
  unfold mass
  apply intervalIntegral.integral_congr
  intro v _
  dsimp only
  rw [show (-d-a*v)/s = -((d+a*v)/s) by ring,
      show (-d+a*v)/s = -((d-a*v)/s) by ring, even_phi, even_phi, add_comm]

private theorem moment_reflect (d s a : ℝ) : moment (-d) s a = -moment d s a := by
  unfold moment
  rw [← intervalIntegral.integral_neg]
  apply intervalIntegral.integral_congr
  intro v _
  dsimp only
  rw [show (-d-a*v)/s = -((d+a*v)/s) by ring,
      show (-d+a*v)/s = -((d-a*v)/s) by ring, even_phi, even_phi]
  ring


private theorem score_injective {m s x a b : ℝ} (hs : 0 < s)
    (hx : x ≠ m) (ha : 0 ≤ a) (hb : 0 ≤ b)
    (he : continuousScore m s a x = continuousScore m s b x) : a = b := by
  have hq := (div_left_inj₀ (sq_pos_of_pos hs).ne').mp he
  have h := sub_left_injective hq
  let d := x-m
  have hd : d ≠ 0 := sub_ne_zero.mpr hx
  change moment d s a / mass d s a = moment d s b / mass d s b at h
  have aux (d : ℝ) (hd : 0 < d)
      (h : moment d s a / mass d s a = moment d s b / mass d s b) : a = b := by
    have hz : moment d s 0 = 0 := by simp [moment]
    rcases ha.eq_or_lt with ha | ha <;> rcases hb.eq_or_lt with hb | hb
    · linarith
    · rw [← ha, hz, zero_div] at h
      exact False.elim ((div_pos (moment_pos d hd hs hb) (mass_pos _ _ _)).ne' h.symm)
    · rw [← hb, hz, zero_div] at h
      exact False.elim ((div_pos (moment_pos d hd hs ha) (mass_pos _ _ _)).ne' h)
    · rw [mean_scale d s ha, mean_scale d s hb] at h
      exact (tiltedMean_strictMono hd hs).injOn ha hb h
  rcases hd.lt_or_gt with hd | hd
  · apply aux (-d) (neg_pos.mpr hd)
    simp only [mass_reflect,moment_reflect,neg_div,h]
  · exact aux d hd h

private theorem log_term_derivative (m : ℝ) {s a : ℝ} (hs : 0 < s) (ha : 0 ≤ a) (x : ℝ) :
    HasDerivAt (fun t ↦ log (mass (t-m) s a / (2*s))) (continuousScore m s a x) x := by
  rcases ha.eq_or_lt with ha | ha
  · subst a
    have hmass (t : ℝ) : mass (t-m) s 0 = 2 * φ ((t-m)/s) := by
      simp [mass,intervalIntegral.integral_const,two_mul]
    have hφ := (phi_hasDerivAt ((x-m)/s)).comp x (((hasDerivAt_id x).sub_const m).div_const s)
    have h := (hφ.div_const s).log (div_pos (phi_pos _) hs).ne'
    convert h using 1
    · funext t
      rw [hmass, mul_div_mul_left _ _ (by norm_num : (2 : ℝ) ≠ 0)]
      rfl
    · unfold continuousScore
      simp only [moment, zero_mul, intervalIntegral.integral_zero, zero_div, zero_sub,
        Function.comp_apply, id_eq]
      field_simp [hs.ne', (phi_pos _).ne']
  · let B (t : ℝ) := Φ ((t-m+a)/s) - Φ ((t-m-a)/s)
    have hmass (t : ℝ) : 0 < B t := sub_pos.mpr (cdf_strictMono
      ((div_lt_div_iff_of_pos_right hs).mpr (by linarith)))
    have h₁ := (cdf_hasDerivAt ((x-m+a)/s)).comp x
      ((((hasDerivAt_id x).sub_const m).add_const a).div_const s)
    have h₂ := (cdf_hasDerivAt ((x-m-a)/s)).comp x
      ((((hasDerivAt_id x).sub_const m).sub_const a).div_const s)
    have h := ((h₁.sub h₂).div_const (2*a)).log (div_pos (hmass x) (by positivity)).ne'
    convert h using 1
    · funext t
      rw [mass_formula (t-m) hs ha]
      change log (s/a* B t / (2*s)) = log (B t/(2*a))
      congr 1
      field_simp [hs.ne',ha.ne']
    · unfold continuousScore
      rw [mean_scale _ _ ha, ← score_formula _ hs ha]
      unfold spatialScore
      change _ = ((φ ((x-m+a)/s)*(1/s)-φ ((x-m-a)/s)*(1/s))/(2*a)) /
        (B x/(2*a))
      dsimp only [B]
      field_simp [hs.ne',ha.ne',(hmass x).ne']

/-- Fixed-count Bhattacharjee log likelihood including the zero-width
normal. The fixed integration interval makes the chart continuous at zero. -/
def logLik {n : ℕ} (w : Fin n → ℕ) (m s a : ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * log (mass (x i-m) s a / (2*s))

/-- At a held center and positive Gaussian scale, the attained finite
Bhattacharjee width is almost everywhere unique on every nonempty
fixed-count chart of freely varying levels. The normal width is included. -/
theorem held_mu_sigma_ae_unique {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a ∈ Ici 0, ∀ b ∈ Ici 0,
      IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) a →
      IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) b → a = b := by
  let A := Ici (0 : ℝ)
  let _ : LocallyCompactSpace A := isClosed_Ici.isLocallyClosed.locallyCompactSpace
  let E := A × (Fin n → ℝ)
  let L (a : A) := logLik w m s a.val
  let D (a : A) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
    ∑ i, ((w i : ℝ) * continuousScore m s a.val (x i)) • proj i
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hmass (i : Fin n) : Continuous (fun z : E ↦ mass (z.2 i-m) s z.1.val) := by
    unfold mass
    apply intervalIntegral.continuous_parametric_intervalIntegral_of_continuous'
    unfold Function.uncurry E
    fun_prop
  have hmom (i : Fin n) : Continuous (fun z : E ↦ moment (z.2 i-m) s z.1.val) := by
    unfold moment
    apply intervalIntegral.continuous_parametric_intervalIntegral_of_continuous'
    unfold Function.uncurry E
    fun_prop
  have hL : Continuous (fun z : E ↦ L z.1 z.2) := by
    unfold L logLik
    apply continuous_finsetSum Finset.univ
    intro i _
    exact (((hmass i).div_const _).log (fun _ ↦
      (div_pos (mass_pos _ _ _) (by positivity)).ne')).const_mul (w i : ℝ)
  have hD : Continuous (fun z : E ↦ D z.1 z.2) := by
    unfold D continuousScore
    apply continuous_finsetSum Finset.univ
    intro i _
    have hx : Continuous (fun z : E ↦ z.2 i-m) := by unfold E; fun_prop
    exact (((((hmom i).div (hmass i) (fun _ ↦ (mass_pos _ _ _).ne')).sub hx).div_const
      _).const_mul (w i : ℝ)).smul continuous_const
  have hder (a : A) (x : Fin n → ℝ) : HasFDerivAt (L a) (D a x) x := by
    unfold L D logLik
    apply HasFDerivAt.fun_sum
    intro i _
    let ev : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have h := ((log_term_derivative m hs a.property (x i)).const_mul
      (w i : ℝ)).comp_hasFDerivAt x (ev.hasFDerivAt (x := x))
    exact h
  let j : Fin n := ⟨0,hn⟩
  have hae : ∀ᵐ t : ℝ ∂volume, t ≠ m := by rw [ae_iff]; simp
  have haj : ∀ᵐ x : Fin n → ℝ ∂volume, x j ≠ m :=
    (Measure.quasiMeasurePreserving_eval (fun _ : Fin n ↦ (volume : Measure ℝ)) j).ae hae
  have hu := UniqueAE.ae_winners_derivative (μ := volume) (S := (univ : Set E)) isOpen_univ
    hL.continuousOn hD.continuousOn (fun a x _ ↦ hder a x)
  filter_upwards [hu, haj] with x hx hj a ha b hb hma hmb
  have he := hx ⟨a,ha⟩ ⟨b,hb⟩ ⟨mem_univ _,fun p _ ↦ hma p.property⟩
    ⟨mem_univ _,fun p _ ↦ hmb p.property⟩
  have eval (r : A) : D r x (Pi.single j 1) =
      (w j : ℝ) * continuousScore m s r.val (x j) := by
    simp only [D, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single j]
    · simp
    · intro i _ hji
      simp [Pi.single_eq_of_ne hji]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single j 1)) he
  rw [eval, eval] at h
  exact score_injective hs hj ha hb (mul_left_cancel₀ (by exact_mod_cast (hw j).ne') h)

/-- If all observations equal the held center, the normal member is the
unique finite Bhattacharjee fit at a positive held Gaussian scale. -/
theorem held_mu_sigma_all_center_unique {n : ℕ} (w : Fin n → ℕ)
    (hw : ∀ i, 0 < w i) (hn : 0 < n) (m : ℝ) {s : ℝ} (hs : 0 < s)
    (x : Fin n → ℝ) (hx : ∀ i, x i = m) :
    ∃! a : ℝ, a ∈ Ici 0 ∧ IsMaxOn (fun r ↦ logLik w m s r x) (Ici 0) a := by
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hmass {a : ℝ} (ha : 0 < a) : mass 0 s a < mass 0 s 0 := by
    have hφle (z : ℝ) : φ z ≤ φ 0 := by
      rw [phi_formula, phi_formula]
      apply mul_le_mul_of_nonneg_left _ (by positivity)
      exact exp_le_exp.mpr (by nlinarith [sq_nonneg z])
    have hφlt {z : ℝ} (hz : z ≠ 0) : φ z < φ 0 := by
      rw [phi_formula, phi_formula]
      apply mul_lt_mul_of_pos_left _ (by positivity)
      exact exp_lt_exp.mpr (by nlinarith [sq_pos_of_ne_zero hz])
    unfold mass
    apply intervalIntegral.integral_lt_integral_of_continuousOn_of_le_of_exists_lt zero_lt_one
      (by fun_prop) (by fun_prop)
    · intro v _
      simp only [zero_mul, sub_zero, add_zero, zero_div]
      exact add_le_add (hφle _) (hφle _)
    · refine ⟨1, ⟨zero_le_one,le_rfl⟩, ?_⟩
      simp only [mul_one, sub_zero, add_zero, zero_add, zero_div]
      exact add_lt_add (hφlt (div_ne_zero (by linarith) hs.ne'))
        (hφlt (div_ne_zero ha.ne' hs.ne'))
  have hstrict {a : ℝ} (ha : 0 < a) : logLik w m s a x < logLik w m s 0 x := by
    let _ : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
    unfold logLik
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    rw [hx i, sub_self]
    exact mul_lt_mul_of_pos_left (log_lt_log (div_pos (mass_pos _ _ _) (by positivity))
      (div_lt_div_of_pos_right (hmass ha) (by positivity))) (by exact_mod_cast hw i)
  refine ⟨0, ⟨le_rfl, ?_⟩, ?_⟩
  · intro a ha
    rcases eq_or_lt_of_le (show 0 ≤ a from ha) with h | h
    · subst a
      exact le_rfl
    · exact (hstrict h).le
  · rintro a ⟨ha,hmax⟩
    by_contra hn'
    have h := hmax (show (0 : ℝ) ∈ Ici 0 from le_rfl)
    exact (hstrict (lt_of_le_of_ne ha (Ne.symm hn'))).not_ge h

end

end UnivariateML.AE4Bhatt

end
