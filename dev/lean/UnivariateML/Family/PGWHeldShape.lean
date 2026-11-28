module

public import UnivariateML.Family.PGWProfile
public import Mathlib.Analysis.Calculus.Deriv.Prod

/-! Scalar inverse-exponent profile with PGW shape held. -/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.PGWHeldShape

noncomputable section

open PGWEnvelope

variable {n : ℕ}

def chartObjective (y : Fin n → ℝ) (b : ℝ) (p : ℝ × ℝ) : ℝ :=
  chart y (b, exp p.2, p.1)

private theorem chart_analytic (y : Fin n → ℝ) {b : ℝ} (hb : 0 < b)
    {p : ℝ × ℝ} (hh : 0 ≤ p.1) : AnalyticAt ℝ (chartObjective y b) p := by
  have h : AnalyticAt ℝ (fun q : ℝ × ℝ ↦ (b, exp q.2, q.1)) p :=
    analyticAt_const.prod ((analyticAt_rexp.comp analyticAt_snd).prod analyticAt_fst)
  exact (analytic_chart y (p := (b, exp p.2, p.1)) ⟨hb, exp_pos _, hh⟩).comp_of_eq h rfl

private def faceBase (v : ℝ) : ℝ := v + loss (exp v)

private theorem face_deriv2_neg (v : ℝ) : deriv (deriv faceBase) v < 0 := by
  have h (w : ℝ) : HasDerivAt faceBase
      (1 + exp w - exp (exp w) * exp w) w := by
    unfold faceBase loss
    convert ((hasDerivAt_id w).add (((hasDerivAt_exp w).add_const 1).sub
      (hasDerivAt_exp w).exp)) using 1
    · funext t
      simp only [Pi.add_apply, Pi.sub_apply, id_eq]
    · ring
  rw [show deriv faceBase = _ from funext fun w ↦ (h w).deriv]
  have hd := (((hasDerivAt_exp v).const_add 1).sub
    ((hasDerivAt_exp v).exp.mul (hasDerivAt_exp v))).deriv
  change deriv (fun w ↦ 1 + exp w - exp (exp w) * exp w) v = _ at hd
  rw [hd]
  have he : 1 ≤ exp (exp v) := one_le_exp_iff.mpr (exp_pos _).le
  have hh := mul_le_mul_of_nonneg_right he (exp_pos v).le
  have hpos : 0 < exp (exp v) * exp v * exp v := by positivity
  nlinarith only [hh, hpos]

private theorem inner_deriv2_neg {c : ℝ} (hc : 0 < c) (z : ℝ) :
    deriv (deriv (ShapeFacesPGW.logKernel c)) z < 0 := by
  let q := logistic
  let L := softplus
  have hL (s : ℝ) : HasDerivAt L (q s) s :=
    ((hasDerivAt_exp s).const_add 1).log (by positivity)
  have hq (s : ℝ) : HasDerivAt q (exp s / (1 + exp s) ^ 2) s := by
    convert (hasDerivAt_exp s).div ((hasDerivAt_exp s).const_add 1) (by positivity) using 1
    · funext t
      simp [q, logistic]
    · ring_nf
  let slope := fun v ↦ 1 + (c - 1 - c * exp (c * L v)) * q v
  have h (v : ℝ) : HasDerivAt (ShapeFacesPGW.logKernel c) (slope v) v := by
    convert ((hasDerivAt_id v).add ((hL v).const_mul (c - 1))).sub
      (((hL v).const_mul c).exp) using 1
    · rfl
    · dsimp [slope]; ring
  rw [show deriv (ShapeFacesPGW.logKernel c) = slope from funext fun v ↦ (h v).deriv]
  have hd := ((((hL z).const_mul c).exp).const_mul c).const_sub (c - 1)
  have hs : HasDerivAt slope
    (-(c * (exp (c * L z) * (c * q z))) * q z +
      (c - 1 - c * exp (c * L z)) * (exp z / (1 + exp z) ^ 2)) z :=
    (hd.mul (hq z)).const_add 1
  rw [hs.deriv]
  have hL₀ : 0 < L z := log_pos (by linarith [exp_pos z])
  have hA : c - 1 - c * exp (c * L z) < 0 := by
    have he : 1 ≤ exp (c * L z) := one_le_exp_iff.mpr (mul_nonneg hc.le hL₀.le)
    linarith [mul_le_mul_of_nonneg_left he hc.le]
  have hq₀ : 0 < q z := by dsimp [q, logistic]; positivity
  exact add_neg_of_nonpos_of_neg
    (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr (by positivity)) hq₀.le)
    (mul_neg_of_neg_of_pos hA (by positivity))

private theorem face_point {b v w : ℝ} (hb : 0 < b) :
    log (b * (exp v * exp w) / (1 + 0 * (exp v * exp w))) +
      loss (exp v * exp w * logSlope (0 * (exp v * exp w))) =
      log b + faceBase (v + w) := by
  simp [faceBase, logSlope, ← exp_add, log_mul hb.ne' (exp_pos _).ne', add_assoc]

private theorem inner_point {b v w h : ℝ} (hb : 0 < b) (hh : 0 < h) :
    log (b * (exp v * exp w) / (1 + h * (exp v * exp w))) +
      loss (exp v * exp w * logSlope (h * (exp v * exp w))) =
      log b + log h⁻¹ + 1 + ShapeFacesPGW.logKernel h⁻¹ (v + w + log h) := by
  let t := exp v * exp w
  have ht : 0 < t := by dsimp [t]; positivity
  have hd : 0 < 1 + h * t := by positivity
  have he : exp (v + w + log h) = h * t := by
    rw [exp_add, exp_add, exp_log hh, mul_comm]
  have hL : t * (log (1 + h * t) / (h * t)) = h⁻¹ * log (1 + h * t) := by
    field_simp
  change log (b * t / (1 + h * t)) + loss (t * logSlope (h * t)) = _
  unfold ShapeFacesPGW.logKernel ShapeFacesPGW.softplus loss logSlope
  rw [ite_eq_right (mul_pos hh ht).ne', he, hL, log_inv,
    log_div (mul_pos hb ht).ne' hd.ne', log_mul hb.ne' ht.ne']
  simp only [t, log_mul (exp_pos v).ne' (exp_pos w).ne', log_exp]
  ring

/-- At a fixed inverse exponent the nuisance log-likelihood is a sum
of a log shape and translated copies of a base with negative curvature. -/
theorem joint_section_forms (y : Fin n → ℝ) {h : ℝ} (hh : 0 ≤ h) :
    ∃ (K d : ℝ) (f : ℝ → ℝ),
      (∀ v, deriv (deriv f) v < 0) ∧ (∀ v, DifferentiableAt ℝ f v) ∧
      ∀ {b : ℝ}, 0 < b → ∀ v,
        chartObjective y b (h, v) =
          (n : ℝ) * log b + K + ∑ i, f (v + b * y i + d) := by
  rcases hh.eq_or_lt with hz | hh
  · subst h
    refine ⟨0, 0, faceBase, face_deriv2_neg, ?_, ?_⟩
    · unfold faceBase loss
      fun_prop
    · intro b hb v
      have he : chartObjective y b (0, v) = ∑ i, (log b + faceBase (v + b * y i)) := by
        unfold chartObjective chart
        exact Finset.sum_congr rfl fun i _ ↦ face_point (v := v) (w := b * y i) hb
      simpa [Finset.sum_add_distrib] using he
  · refine ⟨(n : ℝ) * (log h⁻¹ + 1), log h, ShapeFacesPGW.logKernel h⁻¹,
      inner_deriv2_neg (inv_pos.mpr hh), ?_, ?_⟩
    · intro v
      unfold ShapeFacesPGW.logKernel ShapeFacesPGW.softplus
      apply DifferentiableAt.sub
      · apply differentiableAt_id.add
        exact (((differentiableAt_exp.const_add 1).log (by positivity)).const_mul _)
      · exact differentiableAt_exp.comp _ (((differentiableAt_exp.const_add 1).log
          (by positivity)).const_mul _)
    · intro b hb v
      have he : chartObjective y b (h, v) =
          ∑ i, (log b + log h⁻¹ + 1 + ShapeFacesPGW.logKernel h⁻¹ (v + b * y i + log h)) := by
        unfold chartObjective chart
        exact Finset.sum_congr rfl fun i _ ↦ inner_point (v := v) (w := b * y i) hb hh
      simp only [he, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ,
        Fintype.card_fin, nsmul_eq_mul]
      ring

private theorem section_forms (hn : 0 < n) (y : Fin n → ℝ) {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    ∃ (K : ℝ) (f : ℝ → ℝ) (d : Fin n → ℝ),
      (∀ v, deriv (deriv f) v < 0) ∧ (∀ v, DifferentiableAt ℝ f v) ∧
      (fun v ↦ chartObjective y b (h, v)) = fun v ↦ ∑ i, (K + f (v + d i)) := by
  obtain ⟨K, d, f, hf, hd, he⟩ := joint_section_forms y hh
  refine ⟨log b + K / n, f, fun i ↦ b * y i + d, hf, hd, ?_⟩
  funext v
  rw [he hb v]
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← add_assoc]
  field_simp

private theorem nuisance_deriv2 (hn : 0 < n) (y : Fin n → ℝ) {b h : ℝ}
    (hb : 0 < b) (hh : 0 ≤ h) (v : ℝ) :
    deriv (deriv (fun r ↦ chartObjective y b (h, r))) v < 0 := by
  obtain ⟨K, f, d, hf, hd, he⟩ := section_forms hn y hb hh
  rw [he]
  have ht (u : ℝ) : deriv (fun r ↦ ∑ i, (K + f (r + d i))) u =
      ∑ i, deriv f (u + d i) := by
    apply HasDerivAt.deriv
    exact HasDerivAt.fun_sum (fun i _ ↦ by simpa using
      (((hd (u + d i)).hasDerivAt.comp u ((hasDerivAt_id u).add_const (d i))).const_add K))
  rw [funext ht]
  have hdd (i : Fin n) : DifferentiableAt ℝ (deriv f) (v + d i) := by
    by_contra h
    have h' := deriv_zero_of_not_differentiableAt h
    exact (hf (v + d i)).ne h'
  have hsum : HasDerivAt (fun r ↦ ∑ i, deriv f (r + d i))
      (∑ i, deriv (deriv f) (v + d i)) v := by
    exact HasDerivAt.fun_sum (fun i _ ↦ by simpa [Function.comp_def] using
      ((hdd i).hasDerivAt.comp v ((hasDerivAt_id v).add_const (d i))))
  rw [hsum.deriv]
  exact Finset.sum_neg (fun i _ ↦ hf (v + d i)) ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private theorem physical_chart {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 < h) (v : ℝ) :
    chartObjective (fun i ↦ log (x i)) b (h, v) =
      ShapeFacesPGW.logLik x b (exp (-(log h + v) / b)) h⁻¹ + ∑ i, log (x i) := by
  unfold chartObjective
  rw [chart_surjective _ hb (exp_pos _) hh,
    ← physical_eq hx hb (inv_pos.mpr hh) (exp_pos (-(log h + v) / b)),
    log_exp, log_mul hh.ne' (exp_pos _).ne', log_exp]

private theorem face_chart {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (v : ℝ) :
    chartObjective (fun i ↦ log (x i)) b (0, v) =
      AllFreePGW.gompertzLogLik x b (exp v) + ∑ i, log (x i) := by
  unfold chartObjective chart AllFreePGW.gompertzLogLik
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have he : (x i) ^ b = exp (b * log (x i)) := by
    rw [rpow_def_of_pos (hx i), mul_comm]
  rw [he]
  simp only [zero_mul, add_zero, div_one, logSlope, ite_true,
    mul_one, loss, log_mul hb.ne' (mul_pos (exp_pos _) (exp_pos _)).ne']
  ring

theorem section_has_max (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    ∃ v, IsMaxOn (fun w ↦ chartObjective (fun i ↦ log (x i)) b (h, w)) univ v := by
  rcases hh.eq_or_lt with he | hh
  · subst h
    obtain ⟨r, hr, hm⟩ := (AllFreePGW.gompertz_unique (b := b) hn hx).exists
    have hr' : 0 < r := hr
    refine ⟨log r, fun v _ ↦ ?_⟩
    change chartObjective (fun i ↦ log (x i)) b (0, v) ≤
      chartObjective (fun i ↦ log (x i)) b (0, log r)
    rw [face_chart hx hb, face_chart hx hb, exp_log hr']
    exact add_le_add_left (hm (exp_pos v)) _
  · obtain ⟨a, ha, hm⟩ :=
      (ShapeFacesPGW.held_exponent_shape_unique hn hx hb (inv_pos.mpr hh)).exists
    have ha' : 0 < a := ha
    let v := -b * log a - log h
    have hv : -(log h + v) / b = log a := by dsimp [v]; field_simp; ring
    refine ⟨v, fun w _ ↦ ?_⟩
    change chartObjective (fun i ↦ log (x i)) b (h, w) ≤
      chartObjective (fun i ↦ log (x i)) b (h, v)
    rw [physical_chart hx hb hh, physical_chart hx hb hh, hv, exp_log ha']
    exact add_le_add_left (hm (exp_pos _)) _

def nuisance (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (h : ℝ) : ℝ :=
  if hh : 0 ≤ h then (section_has_max hn hx hb hh).choose else 0

private theorem nuisance_max (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    IsMaxOn (fun v ↦ chartObjective (fun i ↦ log (x i)) b (h, v)) univ
      (nuisance hn hx hb h) := by
  rw [nuisance, dite_eq_left hh]
  exact (section_has_max hn hx hb hh).choose_spec

def profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) (h : ℝ) : ℝ :=
  chartObjective (fun i ↦ log (x i)) b (h, nuisance hn hx hb h)

private def score (y : Fin n → ℝ) (b : ℝ) (p : ℝ × ℝ) : ℝ :=
  deriv (fun v ↦ chartObjective y b (p.1, v)) p.2

private theorem score_analytic (y : Fin n → ℝ) {b : ℝ} (hb : 0 < b)
    {p : ℝ × ℝ} (hh : 0 ≤ p.1) : AnalyticAt ℝ (score y b) p := by
  let F := chartObjective y b
  let A : ((ℝ × ℝ) →L[ℝ] ℝ) →L[ℝ] ℝ := ContinuousLinearMap.apply ℝ ℝ (0, 1)
  have h := (chart_analytic y hb hh).fderiv
  obtain ⟨s, hs, hf⟩ := (chart_analytic y hb hh).exists_ball_analyticOnNhd
  have he : A ∘ fderiv ℝ F =ᶠ[𝓝 p] score y b := by
    have hmem : ∀ᶠ q : ℝ × ℝ in 𝓝 p, q ∈ Metric.ball p s := Metric.ball_mem_nhds p hs
    filter_upwards [hmem] with q hq
    have hd : HasDerivAt (fun v : ℝ ↦ (q.1, v)) (0, 1) q.2 := by
      simpa using (hasDerivAt_const q.2 q.1).prodMk (hasDerivAt_id q.2)
    have hF : HasFDerivAt F (fderiv ℝ F q) q := (hf q hq).differentiableAt.hasFDerivAt
    exact (HasFDerivAt.comp_hasDerivAt_of_eq q.2 hF hd rfl).deriv.symm
  exact (analyticAt_congr he).mp ((A.analyticAt _).comp h)

private theorem score_invertible (hn : 0 < n) (y : Fin n → ℝ)
    {b : ℝ} (hb : 0 < b) {p : ℝ × ℝ} (hh : 0 ≤ p.1) :
    (fderiv ℝ (score y b) p ∘L ContinuousLinearMap.inr ℝ ℝ ℝ).IsInvertible := by
  let A := fderiv ℝ (score y b) p ∘L ContinuousLinearMap.inr ℝ ℝ ℝ
  have hA : A 1 < 0 := by
    have hd : HasDerivAt (fun v : ℝ ↦ (p.1, v)) (0, 1) p.2 := by
      simpa using (hasDerivAt_const p.2 p.1).prodMk (hasDerivAt_id p.2)
    have hS := (score_analytic y hb hh).differentiableAt.hasFDerivAt
    have h := (HasFDerivAt.comp_hasDerivAt_of_eq p.2 hS hd rfl).deriv
    change deriv (deriv (fun v ↦ chartObjective y b (p.1, v))) p.2 = A 1 at h
    rw [← h]
    exact nuisance_deriv2 hn y hb hh p.2
  have ha (t : ℝ) : A t = t * A 1 := by
    conv_lhs => rw [← mul_one t]
    exact A.map_smul t 1
  let B : ℝ →L[ℝ] ℝ := (A 1)⁻¹ • ContinuousLinearMap.id ℝ ℝ
  have hB (t : ℝ) : B t = (A 1)⁻¹ * t := rfl
  refine ContinuousLinearMap.IsInvertible.of_inverse (g := B) ?_ ?_
  · apply ContinuousLinearMap.ext
    intro t
    change A (B t) = t
    rw [ha, hB]
    field_simp [hA.ne]
  · apply ContinuousLinearMap.ext
    intro t
    change B (A t) = t
    rw [hB, ha t]
    calc
      (A 1)⁻¹ * (t * A 1) = t * ((A 1)⁻¹ * A 1) := by ring
      _ = t := by rw [inv_mul_cancel₀ hA.ne, mul_one]

private theorem nuisance_zero (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    score (fun i ↦ log (x i)) b (h, nuisance hn hx hb h) = 0 :=
  ((nuisance_max hn hx hb hh).isLocalMax univ_mem).deriv_eq_zero

/-- The optimized held-shape likelihood has a real-analytic extension
locally at every nonnegative inverse exponent, including the face. -/
theorem analytic_profile_extension (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b h : ℝ} (hb : 0 < b) (hh : 0 ≤ h) :
    ∃ F : ℝ → ℝ, AnalyticAt ℝ F h ∧
      F =ᶠ[𝓝[Ici 0] h] profile hn hx hb := by
  let y := fun i ↦ log (x i)
  let S := score y b
  let p := (h, nuisance hn hx hb h)
  obtain ⟨ψ, hψ, hψp, hψS, _⟩ :=
    PGWProfile.analytic_nuisance_branch (score_analytic y hb (p := p) hh)
      (score_invertible hn y hb (p := p) hh)
  let F := fun t ↦ chartObjective y b (t, ψ t)
  have hp : S p = 0 := nuisance_zero hn hx hb hh
  have hA : AnalyticAt ℝ F h :=
    (chart_analytic y hb (p := p) hh).comp_of_eq (analyticAt_id.prod hψ)
      (by simpa [p] using congrArg (fun v ↦ (h, v)) hψp)
  have he : F =ᶠ[𝓝 h ⊓ 𝓟 (Ici 0)] profile hn hx hb := by
    filter_upwards [eventually_nhdsWithin_of_eventually_nhds hψS, self_mem_nhdsWithin]
      with t ht (ht₀ : 0 ≤ t)
    have hinj : Function.Injective (fun v ↦ S (t, v)) :=
      (strictAnti_of_deriv_neg (fun v ↦ nuisance_deriv2 hn y hb ht₀ v)).injective
    have he' : ψ t = nuisance hn hx hb t := hinj (ht.trans (hp.trans
      (nuisance_zero hn hx hb ht₀).symm))
    simp [F, profile, y, he']
  exact ⟨F, hA, he⟩


private theorem profile_tail (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) : Tendsto (profile hn hx hb) atTop atBot := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have ht := tendsto_atBot_add_const_left _ ((n : ℝ) * log b)
    (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.const_mul_atTop hnR))
  apply tendsto_atBot_mono' _ ?_ ht
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with h hh
  unfold profile chartObjective
  rw [chart_surjective _ hb (exp_pos _) hh]
  have h' := (logProduct_le (y := fun i ↦ log (x i))
    (μ := -(log (h * exp (nuisance hn hx hb h))) / b) hb (inv_pos.mpr hh)).2
  rw [log_mul hb.ne' (inv_pos.mpr hh).ne', log_inv] at h'
  dsimp only [Function.comp_apply]
  linarith

private theorem face_value (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) :
    let r := exp (nuisance hn hx hb 0)
    r ∈ Ioi 0 ∧ IsMaxOn (AllFreePGW.gompertzLogLik x b) (Ioi 0) r ∧
      profile hn hx hb 0 = AllFreePGW.gompertzLogLik x b r + ∑ i, log (x i) := by
  intro r
  refine ⟨exp_pos _, ?_, face_chart hx hb _⟩
  intro t ht
  have h := isMaxOn_iff.mp (nuisance_max hn hx hb (h := 0) le_rfl) (log t) (mem_univ _)
  rw [face_chart hx hb, face_chart hx hb, exp_log (show 0 < t from ht)] at h
  exact (add_le_add_iff_right _).mp h

/-- At any positive held shape, a finite fit exists exactly when a retained
stationary profile value ties or beats the fitted Gompertz face on
nonconstant data. The retained list includes both closed window endpoints. -/
theorem held_shape_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 < b) :
    ∃ (F : ℝ → ℝ) (ε H : ℝ) (R : Set ℝ),
      AnalyticOnNhd ℝ F (Ici 0) ∧ EqOn F (profile hn hx hb) (Ici 0) ∧
      0 < ε ∧ ε ≤ H ∧ (∀ h ∈ Ioo 0 ε, deriv F h ≠ 0) ∧
      (StrictMonoOn F (Ioo 0 ε) ∨ StrictAntiOn F (Ioo 0 ε)) ∧
      (∀ h > H, F h < F 0) ∧
      R = Icc ε H ∩ {h | deriv F h = 0} ∧ R.Finite ∧
      ((∃ p ∈ Weibull.domain, IsMaxOn
        (fun q : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b q.1 q.2) Weibull.domain p) ↔
        (∃ i j, x i ≠ x j) ∧ ∃ h ∈ R, F 0 ≤ F h) := by
  obtain ⟨F, hF, hEq⟩ := PGWProfile.extend_nonnegative
    (fun _ hh ↦ analytic_profile_extension hn hx hb hh)
  have hTail : Tendsto F atTop atBot := (profile_tail hn hx hb).congr'
    ((eventually_ge_atTop (0 : ℝ)).mono fun _ ht ↦ (hEq ht).symm)
  have hnz : ∃ h ∈ Ici (0 : ℝ), deriv F h ≠ 0 := by
    by_contra! h
    have hconst {v w : ℝ} (hv : v ∈ Ioi 0) (hw : w ∈ Ioi 0) : F v = F w :=
      isOpen_Ioi.is_const_of_deriv_eq_zero
      (convex_Ioi (0 : ℝ)).isPreconnected
      (fun t ht ↦ (hF t (le_of_lt ht)).differentiableAt.differentiableWithinAt)
      (fun _ ht ↦ h _ (le_of_lt ht)) hv hw
    obtain ⟨t, ht, ht'⟩ := ((hTail.eventually (eventually_lt_atBot (F 1))).and
      (eventually_gt_atTop (0 : ℝ))).exists
    exact ht.ne (hconst ht' zero_lt_one)
  obtain ⟨⟨ε, hε, hcollar⟩, _⟩ := PGWProfile.analytic_collar_and_finite
    hF.deriv (convex_Ici _).isPreconnected (show (0 : ℝ) ∈ Ici 0 from le_rfl)
    hnz (K := ∅) isCompact_empty (empty_subset _)
  obtain ⟨H₀, hH₀⟩ := eventually_atTop.mp (hTail.eventually (eventually_lt_atBot (F 0)))
  let H := max ε H₀
  let R := Icc ε H ∩ {h | deriv F h = 0}
  have hHR : Icc ε H ⊆ Ici 0 := fun _ ht ↦ hε.le.trans ht.1
  obtain ⟨_, hR⟩ := PGWProfile.analytic_collar_and_finite hF.deriv
    (convex_Ici _).isPreconnected (show (0 : ℝ) ∈ Ici 0 from le_rfl) hnz
    isCompact_Icc hHR
  have hH (t : ℝ) (ht : H < t) : F t < F 0 := hH₀ t ((le_max_right ..).trans ht.le)
  refine ⟨F, ε, H, R, hF, hEq, hε, le_max_left .., hcollar,
    PGWProfile.signed_collar hε hF hcollar, hH, rfl, hR, ?_⟩
  let G := AllFreePGW.gompertzLogLik x b
  let C := ∑ i, log (x i)
  let T := fun p : ℝ × ℝ ↦ ShapeFacesPGW.logLik x b p.1 p.2
  let r := exp (nuisance hn hx hb 0)
  obtain ⟨hr, hG, heG⟩ := face_value hn hx hb
  have hF₀ : F 0 = G r + C := (hEq (show 0 ≤ (0 : ℝ) from le_rfl)).trans heG
  have hval {h : ℝ} (hh : 0 < h) (v : ℝ) :
      chartObjective (fun i ↦ log (x i)) b (h, v) =
        T (exp (-(log h + v) / b), h⁻¹) + C := physical_chart hx hb hh v
  have hfit {a c : ℝ} (ha : 0 < a) (hc : 0 < c) :
      T (a, c) + C ≤ F c⁻¹ := by
    let h := c⁻¹
    let v := -b * log a - log h
    have he : -(log h + v) / b = log a := by dsimp [v]; field_simp; ring
    have hm := isMaxOn_iff.mp (nuisance_max hn hx hb (inv_pos.mpr hc).le) v (mem_univ _)
    rw [hval (inv_pos.mpr hc) v, he, exp_log ha, inv_inv] at hm
    exact hm.trans_eq (hEq (inv_pos.mpr hc).le).symm
  have hprofile {h : ℝ} (hh : 0 < h) :
      F h = T (exp (-(log h + nuisance hn hx hb h) / b), h⁻¹) + C :=
    (hEq hh.le).trans (hval hh _)
  constructor
  · rintro ⟨⟨a, c⟩, ⟨ha, hc⟩, hm⟩
    have ht : 0 < c⁻¹ := inv_pos.mpr hc
    have hne : ∃ i j, x i ≠ x j := ((Base9PGW.held_shape_criterion hn hx hb).choose_spec.2.2.mp
      ⟨(a, c), ⟨ha, hc⟩, hm⟩).1
    have hM : IsMaxOn F (Ioi 0) c⁻¹ := by
      intro t htp
      have ht' : 0 < t := htp
      change F t ≤ F c⁻¹
      rw [hprofile ht']
      let a' := exp (-(log t + nuisance hn hx hb t) / b)
      have hmem : (a', t⁻¹) ∈ Weibull.domain := ⟨exp_pos _, inv_pos.mpr ht'⟩
      exact (add_le_add_left (isMaxOn_iff.mp hm (a', t⁻¹) hmem) C).trans (hfit ha hc)
    have hscore : deriv F c⁻¹ = 0 := (hM.isLocalMax (Ioi_mem_nhds ht)).deriv_eq_zero
    have hbase : F 0 ≤ F c⁻¹ := by
      have hs : Tendsto F (𝓝[>] 0) (𝓝 (F 0)) :=
        (hF 0 le_rfl).continuousAt.continuousWithinAt
      apply le_of_tendsto hs
      filter_upwards [self_mem_nhdsWithin] with t (ht' : 0 < t)
      exact hM ht'
    have hε' : ε ≤ c⁻¹ := le_of_not_gt fun hlt ↦ (hcollar _ ⟨ht, hlt⟩) hscore
    have hH' : c⁻¹ ≤ H := le_of_not_gt fun hlt ↦ (hH _ hlt).not_ge hbase
    exact ⟨hne, c⁻¹, ⟨⟨hε', hH'⟩, hscore⟩, hbase⟩
  · rintro ⟨_, t, ht, hbase⟩
    have ht₀ : 0 < t := hε.trans_le ht.1.1
    rw [hF₀, hprofile ht₀] at hbase
    have he : G r ≤ T (exp (-(log t + nuisance hn hx hb t) / b), t⁻¹) :=
      (add_le_add_iff_right C).mp hbase
    exact (AllFreePGW.held_shape_attains_iff_face hn hx hb).mpr
      ⟨_, exp_pos _, _, inv_pos.mpr ht₀, fun s hs ↦ (hG hs).trans he⟩


end

end UnivariateML.PGWHeldShape

end
