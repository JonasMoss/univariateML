module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Family.SkewLSGaussian

import Mathlib.Tactic

set_option linter.style.header false

/-!
# EMG at held location and scale

The spatial score varies strictly with rate. This proof obtains the
inverse Mills-ratio inequality by differentiating two auxiliary functions,
using the Gaussian tail limit at minus infinity.

## Deviations

The conclusion concerns finite rates on charts with freely varying levels.
The stationary-rate inventory and pointwise constant-residual comparison
are separate.
-/

@[expose] public section

open Set Filter MeasureTheory Real ContinuousLinearMap
open scoped Topology
open UnivariateML.SkewLSGaussian

namespace UnivariateML.AE1EMG

noncomputable section

variable {n : ℕ}

private def millsRatio (z : ℝ) : ℝ := φ z / Φ z

private theorem mills_deriv (z : ℝ) :
    HasDerivAt millsRatio (-(φ z * (φ z + z * Φ z)) / (Φ z)^2) z := by
  have h := (phi_hasDerivAt z).div (cdf_hasDerivAt z) (cdf_pos z).ne'
  convert h using 1
  · rfl
  · ring

private theorem gaussian_tail :
    Tendsto (fun t : ℝ ↦ t * exp (-(t^2 / 2))) atTop (𝓝 0) := by
  have h := (isLittleO_pow_exp_pos_mul_atTop 1 (b := 1 / 2) (by norm_num)).tendsto_div_nhds_zero
  refine squeeze_zero' ?_ ?_ h
  · filter_upwards [eventually_ge_atTop (0 : ℝ)] with t ht
    exact mul_nonneg ht (exp_pos _).le
  · filter_upwards [eventually_ge_atTop (1 : ℝ)] with t ht
    have hsq : t ≤ t^2 := by nlinarith [mul_nonneg (sub_nonneg.mpr ht) (show 0 ≤ t by linarith)]
    simpa only [pow_one, div_eq_mul_inv, ← exp_neg] using
      mul_le_mul_of_nonneg_left (exp_le_exp.mpr (show -(t^2/2) ≤ -(1/2*t) by linarith))
        (show 0 ≤ t by linarith)

private theorem polynomial_cdf_neg_limit :
    Tendsto (fun t : ℝ ↦ (1 + t^2) * Φ (-t)) atTop (𝓝 0) := by
  have hg : Tendsto (fun t : ℝ ↦ t^2 * Φ (-t)) atTop (𝓝 0) := by
    have h := gaussian_tail.mul mills
    have h' : Tendsto (fun t : ℝ ↦
        (t * exp (-(t^2/2))) * (t * exp (t^2/2) * Φ (-t))) atTop (𝓝 0) := by
      simpa using h
    refine h'.congr fun t ↦ ?_
    rw [show t * exp (-(t^2/2)) * (t * exp (t^2/2) * Φ (-t)) =
      (t^2 * Φ (-t)) * (exp (-(t^2/2)) * exp (t^2/2)) by ring,
      ← exp_add, neg_add_cancel, exp_zero, mul_one]
  have h := (cdf_atBot.comp tendsto_neg_atTop_atBot).add hg
  simpa [add_mul] using h

private theorem t_phi_neg_limit : Tendsto (fun t : ℝ ↦ t * φ (-t)) atTop (𝓝 0) := by
  have h : Tendsto (fun t : ℝ ↦ (1 / sqrt (2 * π)) * (t * exp (-(t^2 / 2)))) atTop (𝓝 0) := by
    simpa only [mul_zero] using gaussian_tail.const_mul (1 / sqrt (2 * π))
  refine h.congr fun t ↦ ?_
  rw [phi_formula, neg_sq]
  ring

private theorem phi_neg_limit : Tendsto (fun t : ℝ ↦ φ (-t)) atTop (𝓝 0) := by
  have hsq : Tendsto (fun t : ℝ ↦ t^2 / 2) atTop atTop :=
    (tendsto_id.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0)).atTop_div_const (by norm_num)
  have h := tendsto_exp_neg_atTop_nhds_zero.comp hsq
  simpa [phi_formula, neg_sq, Function.comp_def] using h.const_mul (1 / sqrt (2 * π))

private theorem pos_from_tail {f : ℝ → ℝ} (hf : StrictMono f)
    (hlim : Tendsto (fun t : ℝ ↦ f (-t)) atTop (𝓝 0)) (x : ℝ) : 0 < f x := by
  have he : ∀ᶠ t : ℝ in atTop, f (-t) ≤ f (x - 1) := by
    filter_upwards [eventually_ge_atTop (1 - x)] with t ht
    exact hf.monotone (by linarith)
  exact (le_of_tendsto hlim he).trans_lt (hf (sub_lt_self x zero_lt_one))

private theorem mills_lower_derivative_bound (z : ℝ) :
    0 < (Φ z)^2 - φ z * (φ z + z * Φ z) := by
  let B (t : ℝ) := (1 + t^2) * Φ t + t * φ t
  have hB (t : ℝ) : HasDerivAt B (2 * (φ t + t * Φ t)) t := by
    have h := ((((hasDerivAt_id t).pow 2).const_add 1).mul (cdf_hasDerivAt t)).add
      ((hasDerivAt_id t).mul (phi_hasDerivAt t))
    convert h using 1
    · rfl
    · simp only [Pi.pow_def, id_eq, Nat.cast_ofNat, Nat.reduceSub, pow_one]
      ring
  have hBmono : StrictMono B := strictMono_of_deriv_pos fun t ↦ by
    rw [(hB t).deriv]
    exact mul_pos two_pos (phi_add_mul_cdf_pos t)
  have hBlim : Tendsto (fun t : ℝ ↦ B (-t)) atTop (𝓝 0) := by
    simpa [B, neg_sq, neg_mul, sub_eq_add_neg] using
      polynomial_cdf_neg_limit.sub t_phi_neg_limit
  let H (t : ℝ) := (Φ t)^2 - φ t * (φ t + t * Φ t)
  have hH (t : ℝ) : HasDerivAt H (φ t * B t) t := by
    have h := ((cdf_hasDerivAt t).pow 2).sub ((phi_hasDerivAt t).mul
      ((phi_hasDerivAt t).add ((hasDerivAt_id t).mul (cdf_hasDerivAt t))))
    convert h using 1
    · rfl
    · change φ t * B t =
        (2 : ℝ) * (Φ t)^1 * φ t -
          ((-t * φ t) * (φ t + t * Φ t) + φ t * (-t * φ t + (1 * Φ t + t * φ t)))
      dsimp [B]
      ring
  have hHmono : StrictMono H := strictMono_of_deriv_pos fun t ↦ by
    rw [(hH t).deriv]
    exact mul_pos (phi_pos _) (pos_from_tail hBmono hBlim t)
  have hΦ := cdf_atBot.comp tendsto_neg_atTop_atBot
  have hG : Tendsto (fun t : ℝ ↦ φ (-t) + (-t) * Φ (-t)) atTop (𝓝 0) := by
    have h := phi_neg_limit.sub ((mills.mul
        (show Tendsto (fun t : ℝ ↦ exp (-(t^2/2))) atTop (𝓝 0) from by
          apply tendsto_exp_neg_atTop_nhds_zero.comp
          exact (tendsto_id.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0)).atTop_div_const (by norm_num))))
    have hh : Tendsto (fun t : ℝ ↦ φ (-t) - (t * exp (t^2/2) * Φ (-t)) *
        exp (-(t^2/2))) atTop (𝓝 0) := by simpa using h
    refine hh.congr fun t ↦ ?_
    rw [show (t * exp (t^2/2) * Φ (-t)) * exp (-(t^2/2)) =
      (t * Φ (-t)) * (exp (t^2/2) * exp (-(t^2/2))) by ring,
      ← exp_add, add_neg_cancel, exp_zero, mul_one, neg_mul, sub_eq_add_neg]
  have hHlim : Tendsto (fun t : ℝ ↦ H (-t)) atTop (𝓝 0) := by
    simpa [H] using (hΦ.pow 2).sub (phi_neg_limit.mul hG)
  exact pos_from_tail hHmono hHlim z

/-- Weighted EMG log likelihood at fixed location and Gaussian scale. -/
def logLik (w : Fin n → ℕ) (m s r : ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (log r + (r * s)^2 / 2 - r * (x i - m) +
    log (Φ ((x i - m) / s - r * s)))

private def score (m s r t : ℝ) : ℝ :=
  -r + millsRatio ((t - m) / s - r * s) / s

private theorem score_rate_strictAnti (m t : ℝ) {s : ℝ} (hs : 0 < s) :
    StrictAnti (fun r ↦ score m s r t) := by
  apply strictAnti_of_deriv_neg
  intro r
  let z := (t - m) / s - r * s
  have hz : HasDerivAt (fun r' ↦ (t - m) / s - r' * s) (-s) r := by
    simpa using (((hasDerivAt_id r).mul_const s).const_sub ((t - m) / s))
  have h := (hasDerivAt_neg r).add (((mills_deriv z).comp r hz).div_const s)
  have he : HasDerivAt (fun r ↦ score m s r t)
      (-( ((Φ z)^2 - φ z * (φ z + z * Φ z)) / (Φ z)^2)) r := by
    convert h using 1
    · rfl
    · field_simp [hs.ne', (cdf_pos z).ne']
      ring
  rw [he.deriv]
  exact neg_neg_of_pos (div_pos (mills_lower_derivative_bound z) (sq_pos_of_pos (cdf_pos z)))

private def sampleDeriv (w : Fin n → ℕ) (m s r : ℝ) (x : Fin n → ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score m s r (x i)) • proj i

private theorem chart (w : Fin n → ℕ) (m s : ℝ) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ Ioi 0, ∀ q ∈ Ioi 0,
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) p →
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) q →
      sampleDeriv w m s p x = sampleDeriv w m s q x := by
  let S : Set (ℝ × (Fin n → ℝ)) := Ioi 0 ×ˢ univ
  have hor (i : Fin n) : Continuous (fun z : ℝ × (Fin n → ℝ) ↦
      (z.2 i - m) / s - z.1 * s) := by fun_prop
  have hcdf (i : Fin n) : Continuous (fun z : ℝ × (Fin n → ℝ) ↦
      Φ ((z.2 i - m) / s - z.1 * s)) := cdf_continuous.comp (hor i)
  have hL : ContinuousOn (fun z : ℝ × (Fin n → ℝ) ↦ logLik w m s z.1 z.2) S := by
    unfold logLik
    apply continuousOn_finsetSum Finset.univ
    intro i _
    have hlog := continuous_fst.continuousOn.log (fun z : ℝ × (Fin n → ℝ) ↦
      fun hz : z ∈ S ↦ (show 0 < z.1 from hz.1).ne')
    have h₁ : Continuous (fun z : ℝ × (Fin n → ℝ) ↦ (z.1 * s)^2 / 2) := by fun_prop
    have h₂ : Continuous (fun z : ℝ × (Fin n → ℝ) ↦ z.1 * (z.2 i - m)) := by fun_prop
    exact (((hlog.add h₁.continuousOn).sub h₂.continuousOn).add
      (((hcdf i).log (fun _ ↦ (cdf_pos _).ne')).continuousOn)).const_mul (w i : ℝ)
  have hD : Continuous (fun z : ℝ × (Fin n → ℝ) ↦ sampleDeriv w m s z.1 z.2) := by
    unfold sampleDeriv score millsRatio
    apply continuous_finsetSum Finset.univ
    intro i _
    have hp : Continuous φ := continuous_iff_continuousAt.mpr fun z ↦
      (phi_hasDerivAt z).continuousAt
    exact ((continuous_fst.neg.add (((hp.comp (hor i)).div (hcdf i)
      (fun _ ↦ (cdf_pos _).ne')).div_const s)).const_mul (w i : ℝ)).smul continuous_const
  have hd (r : ℝ) (x : Fin n → ℝ) :
      HasFDerivAt (logLik w m s r) (sampleDeriv w m s r x) x := by
    unfold logLik sampleDeriv
    apply HasFDerivAt.fun_sum
    intro i _
    let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    let z := (x i - m) / s - r * s
    have hz : HasDerivAt (fun t ↦ (t - m) / s - r * s) (1 / s) (x i) :=
      (((hasDerivAt_id (x i)).sub_const m).div_const s).sub_const (r * s)
    have hΦ := ((cdf_hasDerivAt z).log (cdf_pos z).ne').comp (x i) hz
    have hrem := ((((hasDerivAt_id (x i)).sub_const m).const_mul r).const_sub
      (log r + (r * s)^2 / 2))
    have hterm : HasDerivAt (fun t ↦ log r + (r * s)^2 / 2 - r * (t - m) +
        log (Φ ((t - m) / s - r * s))) (score m s r (x i)) (x i) := by
      convert hrem.add hΦ using 1
      · rfl
      · simp [score, millsRatio, z, div_eq_mul_inv]
    have h := (hterm.const_mul (w i : ℝ)).comp_hasFDerivAt x (f.hasFDerivAt (x := x))
    exact h
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) (S := S)
    (isOpen_Ioi.prod isOpen_univ) hL hD.continuousOn (fun r x _ ↦ hd r x)]
    with x hx p hp q hq hmp hmq
  exact hx p q ⟨⟨hp, mem_univ _⟩, fun _ h ↦ hmp h.1⟩
    ⟨⟨hq, mem_univ _⟩, fun _ h ↦ hmq h.1⟩

/-- At held location and Gaussian scale, any attained finite EMG rate is
almost everywhere unique on each nonempty positive-count level chart. -/
theorem held_mu_sigma_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (hn : 0 < n)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ Ioi 0, ∀ q ∈ Ioi 0,
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) p →
      IsMaxOn (fun r ↦ logLik w m s r x) (Ioi 0) q → p = q := by
  filter_upwards [chart w m s] with x hx p hp q hq hmp hmq
  let i : Fin n := ⟨0, hn⟩
  have eval (r : ℝ) : sampleDeriv w m s r x (Pi.single i 1) = (w i : ℝ) * score m s r (x i) := by
    simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hj
      simp [Pi.single_eq_of_ne hj]
    · simp
  have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) (hx p hp q hq hmp hmq)
  rw [eval, eval] at h
  have hw' : (w i : ℝ) ≠ 0 := by exact_mod_cast (hw i).ne'
  exact (score_rate_strictAnti m (x i) hs).injective (mul_left_cancel₀ hw' h)

end
end UnivariateML.AE1EMG

end
