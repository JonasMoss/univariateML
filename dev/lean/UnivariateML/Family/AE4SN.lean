module

public import UnivariateML.Generic.AE4Analytic
public import UnivariateML.Generic.AE4Gaussian
public import UnivariateML.Family.SkewLSSN

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Skew-normal finite fits

## Deviations

The finite-candidate comparisons here concern the smooth positive-scale
chart. Exact crossings and comparisons with half-normal faces are separate.
-/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap Real
open scoped Topology
open UnivariateML.SkewLSGaussian UnivariateML.AE4Gaussian

namespace UnivariateML.AE4SN

noncomputable section

private def mRatio (u : ℝ) := φ u / Φ u

private theorem ratio_atTop : Tendsto mRatio atTop (𝓝 0) := by
  have h := (tendsto_id.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0) :
    Tendsto (fun t : ℝ ↦ t^2) atTop atTop).atTop_mul_const
      (show (0 : ℝ) < 1/2 by norm_num)
  have hp := (tendsto_exp_neg_atTop_nhds_zero.comp h).const_mul (1 / sqrt (2*π))
  rw [mul_zero] at hp
  have hφ : Tendsto φ atTop (𝓝 0) := hp.congr (fun t ↦ by
    simp [phi_formula, div_eq_mul_inv])
  have h := hφ.div cdf_atTop (by norm_num)
  rw [zero_div] at h
  exact h

private theorem ratio_neg : Tendsto (fun u ↦ mRatio (-u) / u) atTop (𝓝 1) := by
  let C := 1/sqrt (2*π)
  have hC : 0 < C := by unfold C; positivity
  have h := (tendsto_const_nhds (x := C)).div mills hC.ne'
  rw [div_self hC.ne'] at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with u hu
  simp only [Pi.div_apply, mRatio, phi_formula, neg_sq, exp_neg]
  change C / (u*exp (u^2/2)*Φ (-u)) = C * (exp (u^2/2))⁻¹ / Φ (-u) / u
  field_simp [(exp_pos (u^2/2)).ne', (cdf_pos (-u)).ne', hu.ne']

private def zscore (m s x : ℝ) := (x-m)/s

private def score (m s a x : ℝ) :=
  (-zscore m s x + a * mRatio (a*zscore m s x))/s

private theorem standardized_top (m : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (zscore m s) atTop atTop := by
  unfold zscore
  simpa only [div_eq_mul_inv, sub_eq_add_neg, id_eq] using
    (tendsto_atTop_add_const_right atTop (-m) tendsto_id).atTop_mul_const (inv_pos.mpr hs)

private theorem standardized_div (m : ℝ) {s : ℝ} (hs : s ≠ 0) :
    Tendsto (fun x ↦ zscore m s x / x) atTop (𝓝 (1/s)) := by
  have h := ((tendsto_inv_atTop_zero.const_mul m).const_sub 1).div_const s
  rw [mul_zero, sub_zero] at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with x hx
  unfold zscore
  field_simp [hs, hx.ne']

private theorem correction_favorable (m : ℝ) {s a : ℝ} (hs : 0 < s) (ha : 0 ≤ a) :
    Tendsto (fun x ↦ a * mRatio (a*zscore m s x)) atTop (𝓝 0) := by
  rcases ha.eq_or_lt with ha | ha
  · subst a
    simp
  · have h := (ratio_atTop.comp ((standardized_top m hs).const_mul_atTop ha)).const_mul a
    simpa only [Function.comp_def, mul_zero] using h

private theorem slope {m s a : ℝ} (hs : 0 < s) :
    Tendsto (fun x ↦ score m s a x / x) atTop
      (𝓝 (-(1 + (min a 0)^2) / s^2)) := by
  have hz := standardized_div m hs.ne'
  have hcorr : Tendsto (fun x ↦ a*mRatio (a*zscore m s x)/x) atTop
      (𝓝 (-(min a 0)^2/s)) := by
    rcases le_or_gt 0 a with ha | ha
    · have h := (correction_favorable m hs ha).mul tendsto_inv_atTop_zero
      simpa only [div_eq_mul_inv, min_eq_right ha, zero_pow (by decide : 2 ≠ 0),
        neg_zero, zero_mul] using h
    · let u (x : ℝ) := (-a)*zscore m s x
      have hu : Tendsto u atTop atTop :=
        (standardized_top m hs).const_mul_atTop (neg_pos.mpr ha)
      have h₁ : Tendsto (fun x ↦ u x/x) atTop (𝓝 ((-a)*(1/s))) := by
        simpa only [u, mul_div_assoc] using hz.const_mul (-a)
      have h := ((ratio_neg.comp hu).mul h₁).const_mul a
      rw [show a*(1*((-a)*(1/s))) = -(min a 0)^2/s by
        rw [min_eq_left ha.le]; ring] at h
      apply h.congr'
      filter_upwards [hu (eventually_gt_atTop (0 : ℝ))] with x hx
      change a * ((φ (-u x)/Φ (-u x))/(u x) * (u x/x)) =
        a*(φ (a*zscore m s x)/Φ (a*zscore m s x))/x
      have he : -u x = a*zscore m s x := by unfold u; ring
      rw [he]
      field_simp [(cdf_pos _).ne', hx.ne']
  have h := (hz.neg.add hcorr).div_const s
  rw [show (-(1/s) + -(min a 0)^2/s)/s = -(1+(min a 0)^2)/s^2 by ring] at h
  apply h.congr
  intro x
  unfold score
  ring

private theorem score_reflect (m s a x : ℝ) :
    score m s a (-x) = -score (-m) s (-a) x := by
  have hz : zscore m s (-x) = -zscore (-m) s x := by unfold zscore; ring
  rw [score, score, hz, mul_neg, neg_mul]
  ring

private theorem favorable_offset (m : ℝ) {s a : ℝ} (hs : 0 < s) (ha : 0 ≤ a) :
    Tendsto (fun x ↦ score m s a x+x/s^2) atTop (𝓝 (m/s^2)) := by
  have h := ((correction_favorable m hs ha).div_const s).const_add (m/s^2)
  rw [zero_div, add_zero] at h
  apply h.congr
  intro x
  unfold score zscore
  ring

private theorem identifies {m s a m' s' a' : ℝ} (hs : 0 < s) (hs' : 0 < s')
    (he : ∀ x, score m s a x = score m' s' a' x) : m = m' ∧ s = s' ∧ a = a' := by
  have hright : (1+(min a 0)^2)/s^2 = (1+(min a' 0)^2)/s'^2 := by
    have h := tendsto_nhds_unique (slope (m := m) (a := a) hs)
      ((slope (m := m') (a := a') hs').congr (fun t ↦ by rw [he]))
    exact neg_inj.mp (by simpa only [neg_div] using h)
  have href (t : ℝ) : score (-m) s (-a) t = score (-m') s' (-a') t := by
    simpa only [score_reflect, neg_inj] using he (-t)
  have hleft : (1+(min (-a) 0)^2)/s^2 = (1+(min (-a') 0)^2)/s'^2 := by
    have h := tendsto_nhds_unique (slope (m := -m) (a := -a) hs)
      ((slope (m := -m') (a := -a') hs').congr (fun t ↦ by rw [href]))
    exact neg_inj.mp (by simpa only [neg_div] using h)
  have hs2 : s^2 = s'^2 := by
    have aux (b c : ℝ) (hc : 0 < c) :
        min ((1+(min b 0)^2)/c^2) ((1+(min (-b) 0)^2)/c^2) = 1/c^2 := by
      rcases le_or_gt 0 b with h | h
      · rw [min_eq_right h, zero_pow (by decide : 2 ≠ 0), add_zero]
        exact min_eq_left (div_le_div_of_nonneg_right (by nlinarith [sq_nonneg (min (-b) 0)])
          (sq_nonneg c))
      · rw [min_eq_right (by linarith : 0 ≤ -b), zero_pow (by decide : 2 ≠ 0), add_zero]
        exact min_eq_right (div_le_div_of_nonneg_right (by nlinarith [sq_nonneg (min b 0)])
          (sq_nonneg c))
    have h := congrArg₂ min hright hleft
    rw [aux a s hs, aux a' s' hs'] at h
    have hh := (div_eq_div_iff (sq_pos_of_pos hs).ne' (sq_pos_of_pos hs').ne').mp h
    linarith
  have hscale : s = s' := by nlinarith
  subst s'
  have hs₀ : s^2 ≠ 0 := (sq_pos_of_pos hs).ne'
  have hr := (div_left_inj₀ hs₀).mp hright
  have hl := (div_left_inj₀ hs₀).mp hleft
  have hm (x y : ℝ) (h : 1+(min x 0)^2 = 1+(min y 0)^2) : min x 0 = min y 0 := by
    have hx : min x 0 ≤ 0 := min_le_right _ _
    have hy : min y 0 ≤ 0 := min_le_right _ _
    nlinarith
  have hslant : a = a' := by
    have hx (x : ℝ) : min x 0 - min (-x) 0 = x := by
      rcases le_or_gt 0 x with h | h
      · rw [min_eq_right h, min_eq_left (by linarith : -x ≤ 0)]; ring
      · rw [min_eq_left h.le, min_eq_right (by linarith : 0 ≤ -x)]; ring
    rw [← hx a, ← hx a', hm _ _ hr, hm _ _ hl]
  subst a'
  refine ⟨?_,rfl,rfl⟩
  rcases le_or_gt 0 a with ha | ha
  · have h := tendsto_nhds_unique (favorable_offset m hs ha)
      ((favorable_offset m' hs ha).congr (fun t ↦ by rw [he]))
    exact (div_left_inj₀ hs₀).mp h
  · have ha' : 0 ≤ -a := by linarith
    have h := tendsto_nhds_unique (favorable_offset (-m) hs ha')
      ((favorable_offset (-m') hs ha').congr (fun t ↦ by rw [href]))
    exact neg_inj.mp ((div_left_inj₀ hs₀).mp h)


/-- Weighted skew-normal log likelihood, with a parameter-independent
additive normalization omitted. -/
def logLik {n : ℕ} (w : Fin n → ℕ) (m s a : ℝ) (x : Fin n → ℝ) :=
  ∑ i, (w i : ℝ) * (-log s - ((x i-m)/s)^2/2 + log (Φ (a*((x i-m)/s))))

private def term (m s a t : ℝ) :=
  -log s - (zscore m s t)^2/2 + log (Φ (a*zscore m s t))

private theorem term_derivative (m s a t : ℝ) :
    HasDerivAt (term m s a) (score m s a t) t := by
  have hz : HasDerivAt (zscore m s) (1/s) t := ((hasDerivAt_id t).sub_const m).div_const s
  have hf := ((hz.pow 2).div_const 2).const_sub (-log s)
  have hΦ := ((cdf_hasDerivAt _).log (cdf_pos _).ne').comp t (hz.const_mul a)
  convert hf.add hΦ using 1
  · rfl
  · unfold score mRatio
    ring_nf

variable {d n : ℕ}

/-- The skew-normal chart with positive Gaussian scale. -/
def domain (s : (Fin d → ℝ) → ℝ) : Set (Fin d → ℝ) := {p | 0 < s p}

private theorem chart (hdn : d + d < n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (m s a : (Fin d → ℝ) → ℝ)
    (hm : ∀ p, AnalyticAt ℝ m p) (hs : ∀ p, AnalyticAt ℝ s p) (ha : ∀ p, AnalyticAt ℝ a p)
    (hinj : Function.Injective (fun p ↦ (m p, s p, a p))) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ domain s, ∀ q ∈ domain s,
      IsMaxOn (fun t ↦ logLik w (m t) (s t) (a t) x) (domain s) p →
      IsMaxOn (fun t ↦ logLik w (m t) (s t) (a t) x) (domain s) q → p = q := by
  have hmc : Continuous m := continuous_iff_continuousAt.mpr (fun p ↦ (hm p).continuousAt)
  have hsc : Continuous s := continuous_iff_continuousAt.mpr (fun p ↦ (hs p).continuousAt)
  have hac : Continuous a := continuous_iff_continuousAt.mpr (fun p ↦ (ha p).continuousAt)
  have hO : IsOpen (domain s) := isOpen_Ioi.preimage hsc
  apply AE4Analytic.ae_unique_analytic_scores hdn w hw (domain s) hO
    (fun p ↦ term (m p) (s p) (a p)) (fun p ↦ score (m p) (s p) (a p))
  · let T : Set ((Fin d → ℝ) × ℝ) := domain s ×ˢ univ
    have hc (f : (Fin d → ℝ) → ℝ) (hf : Continuous f) :
        ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ f z.1) T :=
      (hf.comp continuous_fst).continuousOn
    have ht : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ z.2) T :=
      continuous_snd.continuousOn
    have hz : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ zscore (m z.1) (s z.1) z.2) T :=
      (ht.sub (hc m hmc)).div (hc s hsc) (fun _ hz ↦ (show 0 < s _ from hz.1).ne')
    have hlog := (hc s hsc).log (fun _ hz ↦ (show 0 < s _ from hz.1).ne')
    have hΦ := (cdf_continuous.comp_continuousOn ((hc a hac).mul hz)).log
      (fun _ _ ↦ (cdf_pos _).ne')
    exact (hlog.neg.sub ((hz.pow 2).div_const 2)).add hΦ
  · intro p hp t
    have hm' := (hm p).comp (analyticAt_fst (p := (p,t)))
    have hs' := (hs p).comp (analyticAt_fst (p := (p,t)))
    have ha' := (ha p).comp (analyticAt_fst (p := (p,t)))
    have hs₀ : s p ≠ 0 := (show 0 < s p from hp).ne'
    have hz : AnalyticAt ℝ (fun z : (Fin d → ℝ) × ℝ ↦ zscore (m z.1) (s z.1) z.2)
        (p,t) := (analyticAt_snd.sub hm').div hs' hs₀
    have harg := ha'.mul hz
    have hratio := (AnalyticAt.comp (analyticAt_phi _) harg).div
      (AnalyticAt.comp (analyticAt_Phi _) harg) (cdf_pos _).ne'
    exact (hz.neg.add (ha'.mul hratio)).div hs' hs₀
  · intro p _ t
    exact term_derivative (m p) (s p) (a p) t
  · intro p hp q hq he
    obtain ⟨h₁,h₂,h₃⟩ := identifies hp hq he
    exact hinj (Prod.ext h₁ (Prod.ext h₂ h₃))

/-- At seven freely varying levels, any attained finite skew-normal fit is
almost everywhere unique. The coordinates are location, scale and slant. -/
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 7 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      ∀ p ∈ domain (fun p : Fin 3 → ℝ ↦ p 1), ∀ q ∈ domain (fun p : Fin 3 → ℝ ↦ p 1),
        IsMaxOn (fun r ↦ logLik w (r 0) (r 1) (r 2) x) (domain (fun p ↦ p 1)) p →
        IsMaxOn (fun r ↦ logLik w (r 0) (r 1) (r 2) x) (domain (fun p ↦ p 1)) q → p = q := by
  apply chart (d := 3) (by omega) w hw (fun p ↦ p 0) (fun p ↦ p 1) (fun p ↦ p 2)
    ((proj 0 : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt)
    ((proj 1 : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt)
    ((proj 2 : (Fin 3 → ℝ) →L[ℝ] ℝ).analyticAt)
  intro p q h
  funext i
  fin_cases i
  · exact congrArg Prod.fst h
  · exact congrArg (fun z ↦ z.2.1) h
  · exact congrArg (fun z ↦ z.2.2) h

/-- At held positive scale and five freely varying levels, any attained
finite skew-normal fit is almost everywhere unique. -/
theorem held_omega_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 5 ≤ n)
    {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : Fin 2 → ℝ,
      IsMaxOn (fun r ↦ logLik w (r 0) s (r 1) x) univ p →
      IsMaxOn (fun r ↦ logLik w (r 0) s (r 1) x) univ q → p = q := by
  have hu : domain (fun _ : Fin 2 → ℝ ↦ s) = univ := by ext p; simp [domain, hs]
  rw [← hu]
  have h := chart (d := 2) (by omega) w hw (fun p ↦ p 0) (fun _ ↦ s) (fun p ↦ p 1)
    ((proj 0 : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt) (fun _ ↦ analyticAt_const)
    ((proj 1 : (Fin 2 → ℝ) →L[ℝ] ℝ).analyticAt) (by
      intro p q h
      funext i
      fin_cases i
      · exact congrArg Prod.fst h
      · exact congrArg (fun z ↦ z.2.2) h)
  filter_upwards [h] with x hx p q hp hq
  exact hx p hs q hs hp hq

end

end UnivariateML.AE4SN

end
