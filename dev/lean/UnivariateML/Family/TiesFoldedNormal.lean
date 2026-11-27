module

public import UnivariateML.Family.FoldedNormal

import Mathlib.Analysis.Complex.ExponentialBounds
import Mathlib.Analysis.SpecialFunctions.Sqrt
import Mathlib.Tactic

set_option linter.style.header false

/-!
# An exact half-normal and positive-location tie

The construction uses the source sample with 99 copies of a varying
observation and one copy of 1. A score inequality excludes positive
stationary points near the half-normal uniformly over the sample path.

## Deviations

The theorem proves a unique transition on the stated sample path. It
leaves the number of positive-location global winners at the transition open.
-/

@[expose] public section

namespace UnivariateML.TiesFoldedNormal

open Set Real
open FoldedNormal (avg D m₂ logLik T)

noncomputable section

private def sample (e : ℝ) (i : Fin 100) : ℝ := if i = 0 then 1 else e
private def mean (e : ℝ) : ℝ := (99 * e + 1) / 100
private def variance (e : ℝ) : ℝ := 99 * (1 - e) ^ 2 / 10000
private def moment (e : ℝ) (k : ℕ) : ℝ := (99 * e ^ k + 1) / 100
private def foldedDomain : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0

private theorem avg_sample (e : ℝ) (f : ℝ → ℝ) :
    avg (fun i ↦ f (sample e i)) = (99 * f e + f 1) / 100 := by
  unfold avg
  rw [Fin.sum_univ_succ]
  simp [sample]
  ring

private theorem sample_nonneg {e : ℝ} (he : 0 ≤ e) (i : Fin 100) : 0 ≤ sample e i := by
  unfold sample
  split <;> positivity

private theorem moment_eq (e : ℝ) : m₂ (sample e) = moment e 2 := by
  rw [m₂, avg_sample e (fun z ↦ z ^ 2)]
  simp [moment]

private theorem moment2_pos (e : ℝ) : 0 < moment e 2 := by unfold moment; positivity

private theorem residual_eq (e μ : ℝ) :
    D (sample e) μ = variance e + (μ - mean e) ^ 2 := by
  rw [D, avg_sample e (fun z ↦ (z - μ) ^ 2)]
  unfold variance mean
  ring

private theorem var_lower {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    3 / 500 ≤ variance e := by
  have h₁ : 4 / 5 ≤ 1 - e := by linarith only [he.2]
  have h₂ := pow_le_pow_left₀ (by norm_num : (0 : ℝ) ≤ 4 / 5) h₁ 2
  unfold variance
  linarith only [h₂]

private theorem moment2_bound {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    moment e 2 ≤ 31 / 625 := by
  have h := pow_le_pow_left₀ he.1 he.2 2
  unfold moment
  linarith only [h]

private theorem fourth_dominates {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    4 * moment e 2 ^ 2 < moment e 4 := by
  have h₁ := pow_le_pow_left₀ (moment2_pos e).le (moment2_bound he) 2
  have h₂ : (1 / 100 : ℝ) ≤ moment e 4 := by
    unfold moment
    linarith only [pow_nonneg he.1 4]
  linarith only [h₁, h₂]

private theorem tanh_exp (z : ℝ) : tanh z = 1 - 2 / (1 + exp (2 * z)) := by
  have he : exp (2 * z) = exp z / exp (-z) := by
    rw [← exp_sub]
    congr 1
    ring
  rw [Real.tanh_eq, he]
  have hp := exp_pos z
  have hm := exp_pos (-z)
  have hs : 0 < exp z + exp (-z) := by positivity
  have hq : 0 < 1 + exp z / exp (-z) := by positivity
  field_simp [hp.ne', hm.ne', hs.ne', hq.ne']
  ring

private theorem tanh_deriv (z : ℝ) : HasDerivAt tanh (1 - tanh z ^ 2) z := by
  have h := (hasDerivAt_sinh z).div (hasDerivAt_cosh z) (cosh_pos z).ne'
  convert h using 1
  · funext t
    exact tanh_eq_sinh_div_cosh t
  · rw [tanh_eq_sinh_div_cosh]
    field_simp [(cosh_pos z).ne']

private theorem tanh_lower {c R z : ℝ} (hc : 0 ≤ c) (hcR : c * (1 + R) ≤ 1)
    (hz : z ∈ Icc 0 R) : c * z ≤ tanh z := by
  rw [tanh_exp]
  have hden : 0 < 1 + exp (2 * z) := by positivity
  have hz₀ : 0 ≤ z := hz.1
  have h₁ : c * (1 + z) ≤ 1 :=
    (mul_le_mul_of_nonneg_left (add_le_add_right hz.2 1) hc).trans hcR
  have h₂ := mul_le_mul_of_nonneg_right h₁ hz₀
  have h₃ : 2 / (1 + exp (2 * z)) ≤ 1 / (1 + z) := by
    apply (div_le_div_iff₀ hden (by positivity)).mpr
    linarith only [add_one_le_exp (2 * z)]
  have h₄ : c * z ≤ z / (1 + z) := (le_div_iff₀ (by positivity)).mpr (by nlinarith only [h₂])
  have h₅ : z / (1 + z) = 1 - 1 / (1 + z) := by field_simp; ring
  linarith only [h₃, h₄, h₅]

private theorem tanh_cubic {c R z : ℝ} (hc : 0 ≤ c) (hcR : c * (1 + R) ≤ 1)
    (hz : z ∈ Icc 0 R) : tanh z ≤ z - (c ^ 2 / 3) * z ^ 3 := by
  let f (w : ℝ) := w - (c ^ 2 / 3) * w ^ 3 - tanh w
  have hd (w : ℝ) : HasDerivAt f (tanh w ^ 2 - (c * w) ^ 2) w := by
    have h := ((hasDerivAt_id w).sub (((hasDerivAt_id w).pow 3).const_mul (c ^ 2 / 3))).sub
      (tanh_deriv w)
    convert h using 1
    · rfl
    · simp only [id_eq, Nat.cast_ofNat]
      ring
  have hmono : MonotoneOn f (Icc 0 R) := by
    apply monotoneOn_of_deriv_nonneg (convex_Icc _ _)
      (fun w _ ↦ (hd w).continuousAt.continuousWithinAt)
      (fun w _ ↦ (hd w).differentiableAt.differentiableWithinAt)
    intro w hw
    rw [(hd w).deriv]
    have hw' : w ∈ Icc 0 R := interior_subset hw
    exact sub_nonneg.mpr (pow_le_pow_left₀ (mul_nonneg hc hw'.1) (tanh_lower hc hcR hw') 2)
  have h := hmono (left_mem_Icc.mpr (hz.1.trans hz.2)) hz hz.1
  simp only [f, zero_pow (by decide : (3 : ℕ) ≠ 0), mul_zero, sub_zero, tanh_zero] at h
  exact sub_nonneg.mp h

private def locMean (e a : ℝ) : ℝ := (99 * (e * tanh (e * a)) + tanh a) / 100

private theorem locMean_eq (e μ t : ℝ) :
    avg (fun i ↦ sample e i * tanh (sample e i * μ / t)) = locMean e (μ / t) := by
  rw [show (fun i ↦ sample e i * tanh (sample e i * μ / t)) =
    (fun i ↦ sample e i * tanh (sample e i * (μ / t))) by
      funext i; rw [mul_div_assoc], avg_sample e (fun z ↦ z * tanh (z * (μ / t)))]
  simp [locMean]

private theorem variance_score (e μ t : ℝ) :
    T (sample e) μ t = t - moment e 2 - μ ^ 2 + 2 * μ * locMean e (μ / t) := by
  have hx (y : ℝ) :
      4 * μ * y / (1 + exp (2 * μ * y / t)) =
        2 * μ * (y - y * tanh (y * (μ / t))) := by
    rw [tanh_exp, show 2 * (y * (μ / t)) = 2 * μ * y / t by ring]
    ring
  rw [T, residual_eq, avg_sample e (fun z ↦ 4 * μ * z / (1 + exp (2 * μ * z / t)))]
  rw [hx e, hx 1]
  unfold moment variance mean locMean
  simp only [one_mul]
  ring

private theorem joint_scores {e μ t : ℝ} (he : 0 ≤ e) (hμ : 0 < μ) (ht : 0 < t)
    (hm : IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample e) p.1 p.2) foldedDomain (μ, t)) :
    μ = locMean e (μ / t) ∧ t = moment e 2 - μ ^ 2 := by
  have ht' : t < m₂ (sample e) := by
    by_contra! h
    exact (FoldedNormal.heldScale_zero (by decide) (sample_nonneg he) ht h μ hμ).not_ge
      (hm (show (0, t) ∈ foldedDomain from ⟨le_rfl, ht⟩))
  obtain ⟨v, hv, hvz, _, hvmax⟩ :=
    FoldedNormal.heldScale_positive (by decide) (sample_nonneg he) ht ht'
  have hμv : μ = v := by
    by_contra h
    exact (hvmax μ hμ.le h).not_ge (hm (show (v, t) ∈ foldedDomain from ⟨hv.le, ht⟩))
  rw [← hμv, FoldedNormal.locationScore_eq _ ht hμ, locMean_eq, sub_eq_zero] at hvz
  have hloc : μ = locMean e (μ / t) := by
    have h := (div_eq_iff hμ.ne').mp hvz
    linarith only [h]
  have hmax : IsMaxOn (logLik (sample e) μ) (Ioi 0) t :=
    fun u hu ↦ hm (show (μ, u) ∈ foldedDomain from ⟨hμ.le, hu⟩)
  have hvz' := (hmax.isLocalMax (Ioi_mem_nhds ht)).deriv_eq_zero
  rw [(FoldedNormal.hasDerivAt_logLik_variance _ μ ht).deriv] at hvz'
  have hT := neg_eq_zero.mp ((div_eq_zero_iff.mp hvz').resolve_right (by positivity))
  rw [variance_score, ← hloc] at hT
  exact ⟨hloc, by linarith only [hT]⟩

private theorem locMean_le_mean {e a : ℝ} (he : 0 ≤ e) :
    locMean e a ≤ mean e := by
  have ht (z : ℝ) : tanh z ≤ 1 := by
    rw [tanh_exp]
    have : 0 ≤ 2 / (1 + exp (2 * z)) := by positivity
    linarith only [this]
  unfold locMean mean
  linarith only [mul_le_mul_of_nonneg_left (ht (e * a)) he, ht a]

private theorem locMean_cubic {e a c R : ℝ} (he : e ∈ Icc (0 : ℝ) 1)
    (ha : a ∈ Icc 0 R) (hc : 0 ≤ c) (hcR : c * (1 + R) ≤ 1) :
    locMean e a ≤ a * moment e 2 - (c ^ 2 / 3) * a ^ 3 * moment e 4 := by
  have harg : e * a ∈ Icc 0 R := ⟨mul_nonneg he.1 ha.1,
    (mul_le_mul_of_nonneg_right he.2 ha.1).trans_eq (one_mul a) |>.trans ha.2⟩
  have h₁ := mul_le_mul_of_nonneg_left (tanh_cubic hc hcR harg) he.1
  have h₂ := tanh_cubic hc hcR ha
  unfold locMean moment
  linear_combination (99 * h₁ + h₂) / 100

private theorem excluded_stationary {e μ t c R : ℝ} (he : e ∈ Icc (0 : ℝ) 1)
    (hμ : 0 < μ) (ht : 0 < t) (ha : μ / t ≤ R)
    (hc : 0 ≤ c) (hcR : c * (1 + R) ≤ 1)
    (hFourth : moment e 2 ^ 2 < (c ^ 2 / 3) * moment e 4)
    (hm : IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample e) p.1 p.2) foldedDomain (μ, t)) :
    False := by
  obtain ⟨hμeq, hteq⟩ := joint_scores he.1 hμ ht hm
  let a := μ / t
  have hap : 0 < a := div_pos hμ ht
  have hμat : μ = a * t := (div_mul_cancel₀ _ ht.ne').symm
  have hcub := locMean_cubic he (show a ∈ Icc 0 R from ⟨hap.le, ha⟩) hc hcR
  rw [show locMean e a = μ from hμeq.symm] at hcub
  have hnonneg : 0 ≤ (c ^ 2 / 3) * a ^ 3 * moment e 4 := by unfold moment; positivity
  have hμle : μ ≤ a * moment e 2 := by linarith only [hcub, hnonneg]
  have hsq := pow_le_pow_left₀ hμ.le hμle 2
  have hgap : (c ^ 2 / 3) * a ^ 3 * moment e 4 ≤ a * μ ^ 2 := by
    rw [hμat, hteq] at hcub
    linarith only [hcub]
  have hrequired : (c ^ 2 / 3) * a ^ 2 * moment e 4 ≤ μ ^ 2 := by
    apply le_of_mul_le_mul_left _ hap
    linear_combination hgap
  have hab := mul_lt_mul_of_pos_left hFourth (pow_pos hap 2)
  nlinarith only [hsq, hrequired, hab]

private theorem small_coordinate_excluded {e μ t : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5))
    (hμ : 0 < μ) (ht : 0 < t) (ha : μ / t ≤ 1 / 10)
    (hm : IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample e) p.1 p.2) foldedDomain (μ, t)) :
    False :=
  excluded_stationary ⟨he.1, he.2.trans (by norm_num)⟩ hμ ht ha
    (c := 9 / 10) (by norm_num) (by norm_num)
    (by linarith only [fourth_dominates he, sq_nonneg (moment e 2)]) hm

private theorem zero_sample_no_positive_max {μ t : ℝ} (hμ : 0 < μ) (ht : 0 < t)
    (hm : IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample 0) p.1 p.2) foldedDomain (μ, t)) :
    False := by
  obtain ⟨hμeq, hteq⟩ := joint_scores (by norm_num) hμ ht hm
  have hμle := (show μ = locMean 0 (μ / t) from hμeq).le.trans
    (locMean_le_mean (a := μ / t) (by norm_num : (0 : ℝ) ≤ 0))
  norm_num [mean] at hμle
  have hμsq := pow_le_pow_left₀ hμ.le hμle 2
  have ha : μ / t ≤ 2 := by
    have ht' : 99 / 10000 ≤ t := by
      norm_num [moment] at hteq
      linarith only [hteq, hμsq]
    exact (div_le_iff₀ ht).mpr (by linarith only [ht', hμle])
  exact excluded_stationary (by constructor <;> norm_num) hμ ht ha
    (c := 1 / 3) (by norm_num) (by norm_num) (by norm_num [moment]) hm

private def face (e : ℝ) := log 2 - log (moment e 2) / 2 - 1 / 2

private theorem face_eq (e : ℝ) : logLik (sample e) 0 (moment e 2) = face e := by
  have hD : D (sample e) 0 = moment e 2 := by simpa [D, m₂] using moment_eq e
  unfold logLik face
  simp only [hD, mul_zero, zero_mul, neg_zero, zero_div, exp_zero]
  rw [avg_sample e (fun _ ↦ log (1 + 1 : ℝ))]
  have hq : moment e 2 / (2 * moment e 2) = 1 / 2 := by
    field_simp [(moment2_pos e).ne']
  rw [hq, show (1 : ℝ) + 1 = 2 by norm_num]
  ring

private theorem face_positive {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    0 < face e := by
  have h := log_nonpos (moment2_pos e).le ((moment2_bound he).trans (by norm_num))
  unfold face
  linarith only [h, log_two_gt_d9]

private theorem half_normal_best (e t : ℝ) (ht : 0 < t) :
    logLik (sample e) 0 t ≤ face e := by
  rcases eq_or_ne t (moment e 2) with h | h
  · rw [h, face_eq]
  · rw [← face_eq, ← moment_eq]
    exact (FoldedNormal.halfNormal (by decide) (moment_eq e ▸ moment2_pos e) t ht
      (by rwa [moment_eq])).le

private theorem likelihood_upper {e μ t : ℝ} (he : 0 ≤ e) (hμ : 0 ≤ μ) (ht : 0 < t) :
    logLik (sample e) μ t ≤ log 2 - log t / 2 - D (sample e) μ / (2 * t) := by
  have hi (i : Fin 100) : log (1 + exp (-(2 * μ * sample e i) / t)) ≤ log 2 := by
    have hx := sample_nonneg he i
    have hh : -(2 * μ * sample e i) / t ≤ 0 :=
      div_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr (by positivity)) ht.le
    exact log_le_log (by positivity) (by linarith only [exp_le_one_iff.mpr hh])
  have h := Finset.sum_le_sum fun i (_ : i ∈ Finset.univ) ↦ hi i
  have hh : avg (fun i ↦ log (1 + exp (-(2 * μ * sample e i) / t))) ≤ log 2 := by
    unfold avg
    apply (div_le_iff₀ (by norm_num : (0 : ℝ) < 100)).mpr
    simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
      Nat.cast_ofNat, mul_comm] using h
  unfold logLik
  linarith only [hh]

private theorem small_variance_loses {e μ t : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5))
    (hμ : 0 ≤ μ) (ht : 0 < t) (ht' : t ≤ 1 / 10000) :
    logLik (sample e) μ t < face e := by
  have hD : (3 / 500 : ℝ) ≤ D (sample e) μ := by
    rw [residual_eq]
    linarith only [var_lower he, sq_nonneg (μ - mean e)]
  have hD' := div_le_div_of_nonneg_right hD (show 0 ≤ 2 * t by positivity)
  have hlog := log_le_sub_one_of_pos (div_pos (inv_pos.mpr ht) (by norm_num : (0 : ℝ) < 1000))
  rw [log_div (inv_pos.mpr ht).ne' (by norm_num), log_inv,
    show (1000 : ℝ) = 10 ^ 3 by norm_num, log_pow, log_ten_eq] at hlog
  norm_num at hlog
  have hInv : (10000 : ℝ) ≤ t⁻¹ := by
    rw [← one_div, le_div_iff₀ ht]
    linarith only [ht']
  have hRat : (3 / 500 : ℝ) / (2 * t) = (3 / 1000) * t⁻¹ := by
    rw [div_eq_mul_inv, mul_inv_rev]
    ring
  rw [hRat] at hD'
  apply lt_trans _ (face_positive he)
  linarith only [hD', hlog, hInv, likelihood_upper he.1 hμ ht,
    log_two_lt_d9, log_five_lt_d9]

private theorem large_variance_loses {e μ t : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5))
    (hμ : 0 ≤ μ) (ht : 16 ≤ t) : logLik (sample e) μ t < face e := by
  have htp : 0 < t := by linarith only [ht]
  have hD : 0 ≤ D (sample e) μ := by rw [residual_eq]; unfold variance; positivity
  have hb := div_nonneg hD (show 0 ≤ 2 * t by positivity)
  have hl := log_le_log (by norm_num : (0 : ℝ) < 16) ht
  rw [show (16 : ℝ) = 2 ^ 4 by norm_num, log_pow] at hl
  norm_num at hl
  apply lt_trans _ (face_positive he)
  linarith only [likelihood_upper he.1 hμ htp, hl, hb, log_two_gt_d9]

private theorem large_location_loses {e μ t : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5))
    (hμ : 16 ≤ μ) (ht : t ∈ Icc (1 / 10000 : ℝ) 16) :
    logLik (sample e) μ t < face e := by
  have htp : 0 < t := (by norm_num : (0 : ℝ) < 1 / 10000).trans_le ht.1
  have hmean : mean e ≤ 1 := by unfold mean; linarith only [he.2]
  have hD : 225 ≤ D (sample e) μ := by
    have h := pow_le_pow_left₀ (by norm_num : (0 : ℝ) ≤ 15)
      (show 15 ≤ μ - mean e by linarith only [hμ, hmean]) 2
    rw [residual_eq]
    have hv := var_lower he
    norm_num at h
    linarith only [h, hv]
  have hdiv : 7 ≤ D (sample e) μ / (2 * t) := by
    apply (le_div_iff₀ (by positivity)).mpr
    linarith only [ht.2, hD]
  have hl := log_le_log (by norm_num : (0 : ℝ) < 1 / 10000) ht.1
  rw [show (1 / 10000 : ℝ) = (10 ^ 4)⁻¹ by norm_num, log_inv, log_pow, log_ten_eq] at hl
  norm_num at hl
  apply lt_trans _ (face_positive he)
  linarith only [likelihood_upper he.1 (by linarith only [hμ]) htp,
    hdiv, hl, log_two_lt_d9, log_five_lt_d9]

private def box : Set (ℝ × ℝ) := Icc 0 16 ×ˢ Icc (1 / 10000) 16
private def separatedBox : Set (ℝ × ℝ) := box ∩ {p | p.2 / 10 ≤ p.1}

private theorem box_subset : box ⊆ foldedDomain := by
  intro p hp
  exact ⟨hp.1.1, (by norm_num : (0 : ℝ) < 1 / 10000).trans_le hp.2.1⟩

private theorem outside_box_loses {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5))
    {p : ℝ × ℝ} (hp : p ∈ foldedDomain) (hbox : p ∉ box) :
    logLik (sample e) p.1 p.2 < face e := by
  by_cases ht : 1 / 10000 ≤ p.2
  · by_cases ht' : p.2 ≤ 16
    · have hμ : 16 ≤ p.1 := le_of_not_ge fun h ↦ hbox ⟨⟨hp.1, h⟩, ⟨ht, ht'⟩⟩
      exact large_location_loses he hμ ⟨ht, ht'⟩
    · exact large_variance_loses he hp.1 (le_of_not_ge ht')
  · exact small_variance_loses he hp.1 hp.2 (le_of_not_ge ht)

private theorem face_in_box {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    (0, moment e 2) ∈ box := by
  refine ⟨⟨le_rfl, by norm_num⟩, ⟨?_, (moment2_bound he).trans (by norm_num)⟩⟩
  unfold moment
  linarith only [sq_nonneg e]

private theorem right_witness :
    let e : ℝ := 1 / 5
    let p := (mean e, variance e)
    p ∈ separatedBox ∧ face e < logLik (sample e) p.1 p.2 := by
  let e : ℝ := 1 / 5
  let μ := mean e
  let t := variance e
  have ht : 0 < t := by norm_num [e, t, variance]
  have hμ : 0 ≤ μ := by norm_num [e, μ, mean]
  have hbox : (μ, t) ∈ separatedBox := by
    refine ⟨⟨⟨hμ, ?_⟩, ⟨?_, ?_⟩⟩, ?_⟩
    · norm_num [μ, e, mean]
    · norm_num [t, e, variance]
    · norm_num [t, e, variance]
    · change t / 10 ≤ μ
      norm_num [μ, t, e, mean, variance]
  have hD : D (sample e) μ = t := by rw [residual_eq]; simp [μ, t]
  have hsum : 0 ≤ avg (fun i ↦ log (1 + exp (-(2 * μ * sample e i) / t))) := by
    unfold avg
    exact div_nonneg
      (Finset.sum_nonneg fun _ _ ↦ log_nonneg (le_add_of_nonneg_right (exp_pos _).le))
      (by norm_num)
  have hlt : 4 * t < moment e 2 := by norm_num [t, e, variance, moment]
  have hlog := log_lt_log (show 0 < 4 * t by positivity) hlt
  rw [log_mul (by norm_num) ht.ne', log_four_eq] at hlog
  refine ⟨hbox, ?_⟩
  change face e < logLik (sample e) μ t
  unfold face logLik
  rw [hD, show t / (2 * t) = 1 / 2 by field_simp]
  linarith only [hlog, hsum]


private theorem compact_box : IsCompact box := isCompact_Icc.prod isCompact_Icc

private theorem compact_separated : IsCompact separatedBox := by
  apply compact_box.inter_right
  exact isClosed_le (continuous_snd.div_const _) continuous_fst

private def gap (e : ℝ) (p : ℝ × ℝ) : ℝ :=
  logLik (sample e) p.1 (max (1 / 10000) p.2) - face e

private theorem gap_eq (e : ℝ) {p : ℝ × ℝ} (hp : p ∈ box) :
    gap e p = logLik (sample e) p.1 p.2 - face e := by
  unfold gap
  rw [max_eq_right hp.2.1]

private theorem continuous_gap : Continuous (Function.uncurry gap) := by
  let E (p : ℝ × (ℝ × ℝ)) := p.1
  let M (p : ℝ × (ℝ × ℝ)) := p.2.1
  let V (p : ℝ × (ℝ × ℝ)) := max (1 / 10000) p.2.2
  have hE : Continuous E := continuous_fst
  have hM : Continuous M := continuous_fst.comp continuous_snd
  have hV : Continuous V := continuous_const.max (continuous_snd.comp continuous_snd)
  have hVp (p) : 0 < V p :=
    (by norm_num : (0 : ℝ) < 1 / 10000).trans_le (le_max_left _ _)
  have hS (i : Fin 100) : Continuous (fun p ↦ sample (E p) i) := by
    by_cases hi : i = 0
    · simpa only [sample, ite_eq_left hi] using
        (continuous_const : Continuous (fun _ : ℝ × (ℝ × ℝ) ↦ (1 : ℝ)))
    · simpa only [sample, ite_eq_right hi] using hE
  have hterm (i : Fin 100) : Continuous (fun p ↦
      log (1 + exp (-(2 * M p * sample (E p) i) / V p))) := by
    have h₁ : Continuous (fun p ↦ 1 + exp (-(2 * M p * sample (E p) i) / V p)) := by
      have hnum : Continuous (fun p ↦ -(2 * M p * sample (E p) i)) := by fun_prop
      exact continuous_const.add (continuous_exp.comp (hnum.div hV fun p ↦ (hVp p).ne'))
    exact h₁.log fun p ↦ (show 0 < 1 + exp _ by positivity).ne'
  have hm : Continuous (fun p ↦ moment (E p) 2) := by unfold moment; fun_prop
  have hm' : Continuous (fun p ↦ face (E p)) := by
    have h := hm.log fun p ↦ (moment2_pos (E p)).ne'
    unfold face
    fun_prop
  have hD : Continuous (fun p ↦ D (sample (E p)) (M p)) := by
    unfold D avg
    apply Continuous.div_const
    exact continuous_finsetSum _ (fun i _ ↦ ((hS i).sub hM).pow 2)
  have hA : Continuous (fun p ↦
      avg (fun i ↦ log (1 + exp (-(2 * M p * sample (E p) i) / V p)))) := by
    unfold avg
    exact (continuous_finsetSum _ fun i _ ↦ hterm i).div_const _
  change Continuous (fun p ↦ logLik (sample (E p)) (M p) (V p) - face (E p))
  unfold logLik
  exact ((((hV.log (fun p ↦ (hVp p).ne')).neg.div_const 2).sub
    (hD.div (hV.const_mul 2) (fun p ↦ mul_ne_zero (by norm_num) (hVp p).ne'))).add
    hA).sub hm'

private theorem continuous_gap_section (e : ℝ) : Continuous (gap e) :=
  continuous_gap.comp
    ((continuous_const : Continuous (fun _ : ℝ × ℝ ↦ e)).prodMk continuous_id)

private theorem global_attainment {e : ℝ} (he : e ∈ Icc (0 : ℝ) (1 / 5)) :
    ∃ q ∈ box,
      IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample e) p.1 p.2) foldedDomain q := by
  obtain ⟨q, hq, hqm⟩ := compact_box.exists_isMaxOn ⟨_, face_in_box he⟩
    (continuous_gap_section e).continuousOn
  refine ⟨q, hq, fun p hp ↦ ?_⟩
  have hzero := hqm (face_in_box he)
  change gap e (0, moment e 2) ≤ gap e q at hzero
  rw [gap_eq e (face_in_box he), gap_eq e hq, face_eq, sub_self] at hzero
  change logLik (sample e) p.1 p.2 ≤ logLik (sample e) q.1 q.2
  by_cases hbox : p ∈ box
  · have hh := hqm hbox
    change gap e p ≤ gap e q at hh
    rw [gap_eq e hbox, gap_eq e hq, sub_le_sub_iff_right] at hh
    exact hh
  · exact (outside_box_loses he hp hbox).le.trans (sub_nonneg.mp hzero)

private theorem zero_sample_bound :
    ∀ p ∈ foldedDomain, logLik (sample 0) p.1 p.2 ≤ face 0 := by
  obtain ⟨q, hq, hqm⟩ := global_attainment (by constructor <;> norm_num :
    (0 : ℝ) ∈ Icc 0 (1 / 5))
  have hq' := box_subset hq
  have hzero : q.1 = 0 := hq'.1.eq_or_lt.elim Eq.symm
    (fun h ↦ (zero_sample_no_positive_max h hq'.2 hqm).elim)
  intro p hp
  have hh : logLik (sample 0) p.1 p.2 ≤ logLik (sample 0) q.1 q.2 := hqm hp
  rw [hzero] at hh
  exact hh.trans (half_normal_best 0 q.2 hq'.2)

set_option maxHeartbeats 1000000 in
-- The compact crossing elaborates the two likelihood comparisons on the full sample.
private theorem tie_exists :
    ∃ e ∈ Ioo (0 : ℝ) (1 / 5),
      let x : Fin 100 → ℝ := fun i ↦ if i = 0 then 1 else e
      let S : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
      let f := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
      (0, m₂ x) ∈ S ∧ IsMaxOn f S (0, m₂ x) ∧
        ∃ μ > (0 : ℝ), ∃ t > (0 : ℝ), IsMaxOn f S (μ, t) := by
  let H (e : ℝ) := sSup (gap e '' separatedBox)
  have hw := right_witness.1
  have hne : separatedBox.Nonempty := ⟨_, hw⟩
  have hcont : Continuous H := compact_separated.continuous_sSup continuous_gap
  have hmax (e : ℝ) := compact_separated.exists_sSup_image_eq_and_ge hne
    (continuous_gap_section e).continuousOn
  have hpositive {p : ℝ × ℝ} (hp : p ∈ separatedBox) : 0 < p.1 := by
    have ht : 0 < p.2 := (box_subset hp.1).2
    have h : p.2 / 10 ≤ p.1 := hp.2
    linarith only [ht, h]
  have hleft : H 0 < 0 := by
    obtain ⟨p, hp, hpval, _⟩ := hmax 0
    change H 0 = gap 0 p at hpval
    rw [hpval, gap_eq 0 hp.1]
    apply sub_neg.mpr
    by_contra! hb
    have hdom := box_subset hp.1
    exact zero_sample_no_positive_max (hpositive hp) hdom.2
      (fun q hq ↦ (zero_sample_bound q hq).trans hb)
  have hright : 0 < H (1 / 5) := by
    obtain ⟨p, _, hpval, hm⟩ := hmax (1 / 5)
    change H (1 / 5) = gap (1 / 5) p at hpval
    rw [hpval]
    apply lt_of_lt_of_le _ (hm _ hw)
    rw [gap_eq (1 / 5) hw.1]
    exact sub_pos.mpr right_witness.2
  obtain ⟨e, he, hz⟩ := intermediate_value_Icc (by norm_num : (0 : ℝ) ≤ 1 / 5)
    hcont.continuousOn (show 0 ∈ Icc (H 0) (H (1 / 5)) from ⟨hleft.le, hright.le⟩)
  have hei : e ∈ Ioo (0 : ℝ) (1 / 5) :=
    ⟨he.1.lt_of_ne (fun h ↦ hleft.ne (h.symm ▸ hz)),
      he.2.lt_of_ne (fun h ↦ hright.ne' (h ▸ hz))⟩
  obtain ⟨p, hp, hpval, hpmax⟩ := hmax e
  change H e = gap e p at hpval
  rw [hz, gap_eq e hp.1] at hpval
  have hpeq : logLik (sample e) p.1 p.2 = face e := by linarith only [hpval]
  obtain ⟨q, hq, hqm⟩ := global_attainment he
  have hqdom := box_subset hq
  have hqle : logLik (sample e) q.1 q.2 ≤ face e := by
    rcases hqdom.1.eq_or_lt with hq₀ | hq₀
    · rw [← hq₀]
      exact half_normal_best e q.2 hqdom.2
    · have hb : 1 / 10 < q.1 / q.2 := lt_of_not_ge fun h ↦
        small_coordinate_excluded he hq₀ hqdom.2 h hqm
      have hqsep : q ∈ separatedBox := by
        refine ⟨hq, ?_⟩
        change q.2 / 10 ≤ q.1
        have h := (lt_div_iff₀ hqdom.2).mp hb
        linarith only [h]
      have h := hpmax q hqsep
      rw [gap_eq e hq, gap_eq e hp.1, hpeq, sub_self] at h
      exact sub_nonpos.mp h
  have hbound (v : ℝ × ℝ) (hv : v ∈ foldedDomain) :
      logLik (sample e) v.1 v.2 ≤ face e := (hqm hv).trans hqle
  have hh : (0, m₂ (sample e)) ∈ foldedDomain := ⟨le_rfl, moment_eq e ▸ moment2_pos e⟩
  refine ⟨e, hei, hh, ?_, p.1, hpositive hp, p.2, (box_subset hp.1).2, ?_⟩
  · intro v hv
    change logLik (sample e) v.1 v.2 ≤ logLik (sample e) 0 (m₂ (sample e))
    rw [moment_eq, face_eq]
    exact hbound v hv
  · intro v hv
    exact (hbound v hv).trans_eq hpeq.symm

private theorem ratio_strictAnti : StrictAntiOn (fun z : ℝ ↦ tanh z / z) (Ioi 0) := by
  intro a ha b hb hab
  let x : Fin 1 → ℝ := fun _ ↦ 1
  have hpos : ∃ i, 0 < x i := ⟨0, zero_lt_one⟩
  have h := FoldedNormal.locationScore_strictAnti (n := 1) (by decide)
    (fun _ ↦ show 0 ≤ (1 : ℝ) by norm_num) hpos (show (0 : ℝ) < 1 by norm_num)
    (show a ∈ Ici 0 from ha.le) (show b ∈ Ici 0 from hb.le) hab
  rw [FoldedNormal.locationScore_eq x zero_lt_one ha,
    FoldedNormal.locationScore_eq x zero_lt_one hb] at h
  simpa [x, avg] using h

private def g (a u : ℝ) := log (2 * cosh (a * sqrt u))
private def dG (a u : ℝ) := (a ^ 2 / 2) * (tanh (a * sqrt u) / (a * sqrt u))

private theorem g_deriv {a u : ℝ} (ha : 0 < a) (hu : 0 < u) :
    HasDerivAt (g a) (dG a u) u := by
  have h := (Real.hasDerivAt_sqrt hu.ne').const_mul a
  have hc := ((hasDerivAt_cosh (a * sqrt u)).comp (h := fun t ↦ a * sqrt t) u h).const_mul 2
  convert hc.log (mul_ne_zero (by norm_num) (cosh_pos (a * sqrt u)).ne') using 1
  · rfl
  · unfold dG
    simp only [Function.comp_apply]
    rw [tanh_eq_sinh_div_cosh]
    field_simp [ha.ne', (sqrt_pos.mpr hu).ne', (cosh_pos (a * sqrt u)).ne']

private theorem g_score_strictAnti {a : ℝ} (ha : 0 < a) :
    StrictAntiOn (dG a) (Ioi 0) := by
  intro l hl h hh hlh
  have h₁ : 0 < a * sqrt l := mul_pos ha (sqrt_pos.mpr hl)
  have h₂ : 0 < a * sqrt h := mul_pos ha (sqrt_pos.mpr hh)
  have h₃ : a * sqrt l < a * sqrt h := mul_lt_mul_of_pos_left
    (Real.sqrt_lt_sqrt hl.le hlh) ha
  exact mul_lt_mul_of_pos_left (ratio_strictAnti h₁ h₂ h₃) (by positivity)

private def normLow (e : ℝ) := e ^ 2 / moment e 2
private def normHigh (e : ℝ) := 1 / moment e 2

private theorem normalized_sum (e : ℝ) : normHigh e = 100 - 99 * normLow e := by
  unfold normHigh normLow
  have hm := moment2_pos e
  field_simp [hm.ne']
  unfold moment
  ring

private theorem normLow_mem {e : ℝ} (he : 0 < e) (he' : e < 1) :
    normLow e ∈ Ioo (0 : ℝ) 1 := by
  refine ⟨div_pos (sq_pos_of_pos he) (moment2_pos e), ?_⟩
  have h := pow_lt_pow_left₀ he' he.le (by decide : (2 : ℕ) ≠ 0)
  unfold normLow
  apply (div_lt_one (moment2_pos e)).mpr
  unfold moment
  linarith only [h]

private theorem normLow_increases {e₁ e₂ : ℝ} (he₁ : 0 < e₁) (he : e₁ < e₂) :
    normLow e₁ < normLow e₂ := by
  unfold normLow
  apply (div_lt_div_iff₀ (moment2_pos e₁) (moment2_pos e₂)).mpr
  have h := pow_lt_pow_left₀ he he₁.le (by decide : (2 : ℕ) ≠ 0)
  unfold moment
  linear_combination h / 100

private def weightSum (a l : ℝ) := (99 * g a l + g a (100 - 99 * l)) / 100

private theorem weightSum_increases {a : ℝ} (ha : 0 < a) :
    StrictMonoOn (weightSum a) (Ioo (0 : ℝ) 1) := by
  have hd (l : ℝ) (hl : l ∈ Ioo (0 : ℝ) 1) :
      HasDerivAt (weightSum a) ((99 / 100) * (dG a l - dG a (100 - 99 * l))) l := by
    have hp : 0 < 100 - 99 * l := by linarith only [hl.2]
    have h₁ := (g_deriv ha hl.1).const_mul 99
    have harg : HasDerivAt (fun t : ℝ ↦ 100 - 99 * t) (-99) l := by
      simpa only [mul_one, id_eq] using ((hasDerivAt_id l).const_mul 99).const_sub 100
    have h₂ := (g_deriv ha hp).comp (h := fun t : ℝ ↦ 100 - 99 * t) l harg
    convert (h₁.add h₂).div_const 100 using 1
    · rfl
    · ring
  apply strictMonoOn_of_deriv_pos (convex_Ioo ..)
    (fun _ hl ↦ (hd _ hl).continuousAt.continuousWithinAt)
  intro l hl
  have hl' : l ∈ Ioo (0 : ℝ) 1 := interior_subset hl
  rw [(hd l hl').deriv]
  have hlt : l < 100 - 99 * l := by linarith only [hl'.2]
  have hp' : 100 - 99 * l ∈ Ioi (0 : ℝ) := hl'.1.trans hlt
  exact mul_pos (by norm_num) (sub_pos.mpr (g_score_strictAnti ha hl'.1 hp' hlt))

private theorem reflection_cosh (z : ℝ) :
    log (1 + exp (-(2 * z))) = log (2 * cosh z) - z := by
  have h : 1 + exp (-(2 * z)) = exp (-z) * (2 * cosh z) := by
    rw [cosh_eq, show 2 * ((exp z + exp (-z)) / 2) = exp z + exp (-z) by ring,
      mul_add, ← exp_add, ← exp_add, neg_add_cancel, exp_zero]
    congr 2
    ring
  rw [h, log_mul (exp_pos _).ne' (mul_pos (by norm_num) (cosh_pos z)).ne', log_exp]
  ring

private theorem likelihood_cosh (e μ t : ℝ) :
    logLik (sample e) μ t = -log t / 2 - (moment e 2 + μ ^ 2) / (2 * t) +
      (99 * log (2 * cosh (e * μ / t)) + log (2 * cosh (μ / t))) / 100 := by
  have h (x : ℝ) :
      log (1 + exp (-(2 * μ * x) / t)) =
        log (2 * cosh (x * μ / t)) - x * μ / t := by
    rw [show -(2 * μ * x) / t = -(2 * (x * μ / t)) by ring, reflection_cosh]
  unfold logLik
  rw [residual_eq, avg_sample e (fun x ↦ log (1 + exp (-(2 * μ * x) / t))), h e, h 1]
  simp only [one_mul]
  unfold variance mean moment
  ring

private def normalizedGap (e u v : ℝ) : ℝ :=
  logLik (sample e) (sqrt (moment e 2) * u) (moment e 2 * v) - face e

private theorem normalized_formula {e u v : ℝ} (he : 0 ≤ e) (hv : 0 < v) :
    normalizedGap e u v = -log v / 2 - (1 + u ^ 2) / (2 * v) +
      weightSum (u / v) (normLow e) - log 2 + 1 / 2 := by
  let m := moment e 2
  have hm : 0 < m := moment2_pos e
  have hsq : sqrt m ^ 2 = m := sq_sqrt hm.le
  have harg (x : ℝ) (hx : 0 ≤ x) :
      x * (sqrt m * u) / (m * v) = u / v * sqrt (x ^ 2 / m) := by
    rw [sqrt_div (sq_nonneg _), sqrt_sq_eq_abs, abs_of_nonneg hx]
    field_simp [hm.ne', hv.ne', (sqrt_pos.mpr hm).ne']
    linear_combination (x * u) * hsq
  have hscale : (m + (sqrt m * u) ^ 2) / (2 * (m * v)) = (1 + u ^ 2) / (2 * v) := by
    rw [mul_pow, hsq]
    field_simp
  change logLik (sample e) (sqrt m * u) (m * v) - face e = _
  have harg₁ : (sqrt m * u) / (m * v) = u / v * sqrt (1 / m) := by
    simpa only [one_mul, one_pow] using harg 1 (by norm_num)
  rw [likelihood_cosh, harg e he, harg₁, hscale, log_mul hm.ne' hv.ne']
  unfold face weightSum g
  rw [← normalized_sum]
  unfold normLow normHigh
  change _ - (log 2 - log m / 2 - 1 / 2) = _
  ring

private theorem normalizedGap_increases {e₁ e₂ u v : ℝ} (h₁ : 0 < e₁)
    (h₁₂ : e₁ < e₂) (h₂ : e₂ < 1) (hu : 0 < u) (hv : 0 < v) :
    normalizedGap e₁ u v < normalizedGap e₂ u v := by
  have hp₂ : 0 < e₂ := h₁.trans h₁₂
  rw [normalized_formula h₁.le hv, normalized_formula hp₂.le hv]
  have h := weightSum_increases (div_pos hu hv)
    (normLow_mem h₁ (h₁₂.trans h₂)) (normLow_mem hp₂ h₂) (normLow_increases h₁ h₁₂)
  linarith only [h]

private theorem positive_tie_excludes_later_face {e₁ e₂ μ t : ℝ} (h₁ : 0 < e₁)
    (h₁₂ : e₁ < e₂) (h₂ : e₂ < 1) (hμ : 0 < μ) (ht : 0 < t)
    (hvalue : logLik (sample e₁) μ t = face e₁) :
    ¬ IsMaxOn (fun p : ℝ × ℝ ↦ logLik (sample e₂) p.1 p.2) foldedDomain
      (0, moment e₂ 2) := by
  intro hm
  let m₁ := moment e₁ 2
  let m₂ := moment e₂ 2
  have hm₁ : 0 < m₁ := moment2_pos e₁
  have hm₂ : 0 < m₂ := moment2_pos e₂
  let u := μ / sqrt m₁
  let v := t / m₁
  have hu : 0 < u := div_pos hμ (sqrt_pos.mpr hm₁)
  have hv : 0 < v := div_pos ht hm₁
  have hzero : normalizedGap e₁ u v = 0 := by
    change logLik (sample e₁) (sqrt m₁ * u) (m₁ * v) - face e₁ = 0
    have hmu : sqrt m₁ * u = μ := by
      dsimp [u]
      rw [mul_comm, div_mul_cancel₀ _ (sqrt_pos.mpr hm₁).ne']
    have ht' : m₁ * v = t := by dsimp [v]; rw [mul_comm, div_mul_cancel₀ _ hm₁.ne']
    rw [hmu, ht', hvalue, sub_self]
  have hpos := normalizedGap_increases h₁ h₁₂ h₂ hu hv
  rw [hzero] at hpos
  have hp : (sqrt m₂ * u, m₂ * v) ∈ foldedDomain :=
    ⟨(mul_pos (sqrt_pos.mpr hm₂) hu).le, mul_pos hm₂ hv⟩
  have h := hm hp
  change logLik (sample e₂) (sqrt m₂ * u) (m₂ * v) ≤ logLik (sample e₂) 0 (moment e₂ 2) at h
  rw [face_eq] at h
  exact hpos.not_ge (sub_nonpos.mpr h)

private def isTie (e : ℝ) : Prop :=
  let f := fun p : ℝ × ℝ ↦ logLik (sample e) p.1 p.2
  (0, m₂ (sample e)) ∈ foldedDomain ∧
    IsMaxOn f foldedDomain (0, m₂ (sample e)) ∧
    ∃ μ > (0 : ℝ), ∃ t > (0 : ℝ), IsMaxOn f foldedDomain (μ, t)

private theorem ties_cannot_increase {e₁ e₂ : ℝ} (h₁ : 0 < e₁) (h₂ : e₂ < 1)
    (hlt : e₁ < e₂) (ht₁ : isTie e₁) (ht₂ : isTie e₂) : False := by
  obtain ⟨hp₀, hmax₀, μ, hμ, t, ht, hmax⟩ := ht₁
  have hp : (μ, t) ∈ foldedDomain := ⟨hμ.le, ht⟩
  have he : logLik (sample e₁) μ t = logLik (sample e₁) 0 (m₂ (sample e₁)) :=
    le_antisymm (hmax₀ hp) (hmax hp₀)
  rw [moment_eq, face_eq] at he
  have hm := ht₂.2.1
  rw [moment_eq] at hm
  exact positive_tie_excludes_later_face h₁ hlt h₂ hμ ht he hm

/-- The sample with 99 repeated observations and one 1 has a unique
transition parameter in (0, 1/5) where the half-normal and a positive
location both globally maximize the likelihood. -/
theorem half_normal_positive_location_tie :
    ∃! e, e ∈ Ioo (0 : ℝ) (1 / 5) ∧
      let x : Fin 100 → ℝ := fun i ↦ if i = 0 then 1 else e
      let S : Set (ℝ × ℝ) := Ici 0 ×ˢ Ioi 0
      let f := fun p : ℝ × ℝ ↦ logLik x p.1 p.2
      (0, m₂ x) ∈ S ∧ IsMaxOn f S (0, m₂ x) ∧
        ∃ μ > (0 : ℝ), ∃ t > (0 : ℝ), IsMaxOn f S (μ, t) := by
  change ∃! e, e ∈ Ioo (0 : ℝ) (1 / 5) ∧ isTie e
  obtain ⟨e, he, ht⟩ := tie_exists
  refine ⟨e, ⟨he, ht⟩, ?_⟩
  intro r hr
  rcases lt_trichotomy r e with h | h | h
  · exact (ties_cannot_increase hr.1.1 (he.2.trans (by norm_num)) h hr.2 ht).elim
  · exact h
  · exact (ties_cannot_increase he.1 (hr.1.2.trans (by norm_num)) h ht hr.2).elim

end
end UnivariateML.TiesFoldedNormal

end
