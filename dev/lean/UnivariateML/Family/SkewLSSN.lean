module

public import UnivariateML.Generic.LocationScale
public import UnivariateML.Family.SkewLSGaussian
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Skew-normal likelihood

## Deviations

The endpoint theorem is a held-centre path and retains the factor one
half at ties. The free-centre envelope uses a moving endpoint.
-/

@[expose] public section

open Set Filter Topology Real UnivariateML.SkewLSGaussian

namespace UnivariateML.SkewLSSN

noncomputable section

private def loss (F : ℝ → ℝ) (α z : ℝ) : ℝ := z^2 / 2 - Real.log (F (α * z))

/-- A positive log-concave CDF bounded by one gives a unique fixed-slant fit. -/
private theorem held_slant_of_cdf {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {F : ℝ → ℝ} (hF : ∀ z, F z ∈ Ioc 0 1)
    (hlog : ConcaveOn ℝ univ (fun z ↦ Real.log (F z))) (α : ℝ) :
    ∃! p, p ∈ LocationScale.domain ∧
      IsMaxOn (LocationScale.logLik (loss F α) x) LocationScale.domain p := by
  apply LocationScale.existsUnique_mle _ _ hx
  · refine ⟨convex_univ, ?_⟩
    intro p _ q _ hpq u v hu hv huv
    have h := hlog.2 (mem_univ (α*p)) (mem_univ (α*q)) hu.le hv.le huv
    simp only [smul_eq_mul] at h ⊢
    unfold loss
    rw [show α * (u*p + v*q) = u*(α*p) + v*(α*q) by ring]
    have hs : 0 < u*v*(p-q)^2 := mul_pos (mul_pos hu hv) (sq_pos_of_ne_zero (sub_ne_zero.mpr hpq))
    have he : u*p^2 + v*q^2 - (u*p + v*q)^2 = u*v*(p-q)^2 := by
      rw [show v = 1-u by linarith]
      ring
    linarith
  · refine ⟨1, one_pos, 1, fun z ↦ ?_⟩
    have h := Real.log_nonpos (hF (α*z)).1.le (hF (α*z)).2
    unfold loss
    nlinarith [sq_nonneg (|z| - 1), sq_abs z]

/-- Scale collapse along the sample centre is unbounded at any finite slant. -/
private theorem constant_collapse_of_cdf (F : ℝ → ℝ) (α : ℝ) {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (LocationScale.logLik (loss F α) (fun _ : Fin n ↦ m) '' LocationScale.domain) :=
  LocationScale.not_bddAbove_logLik_constant (loss F α) hn m

/-- The right half-normal path retains the factor F(0) at every endpoint tie
and vanishes if any residual has the wrong sign. The left path follows
by reflecting all residuals. -/
private theorem half_normal_path_of_cdf {n : ℕ} {F : ℝ → ℝ}
    (hzero : Tendsto F atBot (𝓝 0)) (hone : Tendsto F atTop (𝓝 1))
    (r w : Fin n → ℝ) :
    Tendsto (fun α : ℝ ↦ ∏ i, w i * F (α * r i)) atTop
      (𝓝 (∏ i, w i * (if 0 < r i then 1 else if r i < 0 then 0 else F 0))) := by
  refine tendsto_finsetProd _ fun i _ ↦ tendsto_const_nhds.mul ?_
  split_ifs with hp hm
  · exact hone.comp (tendsto_id.atTop_mul_const hp)
  · exact hzero.comp (tendsto_id.atTop_mul_const_of_neg hm)
  · have he : r i = 0 := le_antisymm (le_of_not_gt hp) (le_of_not_gt hm)
    simp [he]

/-- The normalized Gaussian factor times the slant CDF is the skew-normal density. -/
def density (α ξ ω x : ℝ) : ℝ :=
  (2 * φ ((x - ξ)/ω) / ω) * Φ (α * ((x - ξ)/ω))

/-- A sum of log densities in the original location and scale coordinates. -/
def logLik {n : ℕ} (α : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑ i, log (density α p.1 p.2 (x i))

private theorem logLik_transfer {n : ℕ} (α : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ)
    (hp : p ∈ LocationScale.domain) :
    logLik α x p = LocationScale.logLik (loss Φ α) x p +
      n * log (2 * (1 / sqrt (2 * π))) := by
  let C := 2 * (1 / sqrt (2 * π))
  have hC : 0 < C := by dsimp [C]; positivity
  have hω : 0 < p.2 := hp.2
  have hconst : (∑ _ : Fin n, log C) = n * log C := by simp [nsmul_eq_mul]
  change _ = LocationScale.logLik (loss Φ α) x p + n * log C
  rw [logLik, LocationScale.logLik, ← hconst, ← Finset.sum_add_distrib]
  · apply Finset.sum_congr rfl
    intro i _
    let z := (x i - p.1)/p.2
    have hden : density α p.1 p.2 (x i) = (C * exp (-(z^2/2)) / p.2) * Φ (α*z) := by
      unfold density
      rw [phi_formula]
      dsimp [z, C]
      ring
    rw [hden, log_mul (div_ne_zero (mul_ne_zero hC.ne' (exp_pos _).ne') hω.ne')
      (cdf_pos _).ne', log_div (mul_ne_zero hC.ne' (exp_pos _).ne') hω.ne',
      log_mul hC.ne' (exp_pos _).ne', log_exp]
    change log C + -(z^2/2) - log p.2 + log (Φ (α*z)) =
      -log p.2 - (z^2/2 - log (Φ (α*z))) + log C
    ring

/-- At fixed finite slant, every nonconstant sample has exactly one
location–scale MLE for the Mathlib Gaussian skew density. -/
theorem held_slant {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) (α : ℝ) :
    ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (logLik α x) LocationScale.domain p := by
  have he (p : ℝ × ℝ) (hp : p ∈ LocationScale.domain) :
      IsMaxOn (logLik α x) LocationScale.domain p ↔
        IsMaxOn (LocationScale.logLik (loss Φ α) x) LocationScale.domain p := by
    simp only [isMaxOn_iff]
    constructor <;> intro h q hq
    · have hm := h q hq
      rw [logLik_transfer α x p hp, logLik_transfer α x q hq] at hm
      exact le_of_add_le_add_right hm
    · rw [logLik_transfer α x p hp, logLik_transfer α x q hq]
      exact add_le_add_left (h q hq) _
  obtain ⟨p, ⟨hp, hm⟩, hu⟩ := held_slant_of_cdf hx
    (fun z ↦ ⟨cdf_pos z, (cdf_lt_one z).le⟩) cdf_strictLogConcave.concaveOn α
  exact ⟨p, ⟨hp, (he p hp).mpr hm⟩, fun q hq ↦ hu q ⟨hq.1, (he q hq.1).mp hq.2⟩⟩

/-- A constant sample has unbounded log likelihood at every fixed finite slant. -/
theorem constant_collapse (α : ℝ) {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (logLik α (fun _ : Fin n ↦ m) '' LocationScale.domain) := by
  rintro ⟨B, hB⟩
  apply constant_collapse_of_cdf Φ α hn m
  refine ⟨B - n * log (2 * (1 / sqrt (2 * π))), ?_⟩
  rintro _ ⟨p, hp, rfl⟩
  have h := hB (mem_image_of_mem (logLik α (fun _ : Fin n ↦ m)) hp)
  change logLik α (fun _ : Fin n ↦ m) p ≤ B at h
  rw [logLik_transfer _ _ _ hp] at h
  linarith

/-- With location and scale held, the right half-normal limit retains a
factor one half at each observation exactly at the location. -/
theorem half_normal_path {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ) {ω : ℝ} (hω : 0 < ω) :
    Tendsto (fun α : ℝ ↦ ∏ i, density α ξ ω (x i)) atTop
      (𝓝 (∏ i, (2 * φ ((x i - ξ)/ω) / ω) *
        (if ξ < x i then 1 else if x i < ξ then 0 else 1/2))) := by
  have hneg (i : Fin n) : (x i - ξ)/ω < 0 ↔ x i < ξ := by
    rw [div_lt_iff₀ hω, zero_mul, sub_neg]
  simpa only [density, cdf_zero, div_pos_iff_of_pos_right hω, hneg, sub_pos] using
    half_normal_path_of_cdf cdf_atBot cdf_atTop (fun i ↦ (x i - ξ)/ω)
      (fun i ↦ 2 * φ ((x i - ξ)/ω) / ω)

/-- The unpenalized product of Gaussian factors at a moving half-normal endpoint. -/
def halfNormalEnvelope {n : ℕ} (x : Fin n → ℝ) (ξ ω : ℝ) : ℝ :=
  ∏ i, 2 * φ ((x i-ξ)/ω) / ω

/-- A right half-normal face at an endpoint at or below the sample is
reachable without endpoint penalties by moving the location from outside. -/
theorem right_moving_half_normal {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ}
    (hω : 0 < ω) (hx : ∀ i, c ≤ x i) :
    Tendsto (fun t : ℝ ↦ ∏ i, density (t^2) (c-t⁻¹) ω (x i)) atTop
      (𝓝 (halfNormalEnvelope x c ω)) := by
  unfold halfNormalEnvelope
  refine tendsto_finsetProd Finset.univ (fun i _ ↦ ?_)
  let z (t : ℝ) := (x i - (c-t⁻¹))/ω
  have hz : Tendsto z atTop (𝓝 ((x i-c)/ω)) := by
    have hc : Tendsto (fun t : ℝ ↦ c-t⁻¹) atTop (𝓝 c) := by
      simpa only [sub_zero] using (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_sub c
    change Tendsto (fun t : ℝ ↦ (x i - (c-t⁻¹))/ω) _ _
    exact (hc.const_sub (x i)).div_const ω
  have harg : Tendsto (fun t : ℝ ↦ t^2*z t) atTop atTop := by
    have hlin : Tendsto (fun t : ℝ ↦ t/ω) atTop atTop := by
      simpa only [div_eq_inv_mul, id_eq] using tendsto_id.const_mul_atTop (inv_pos.mpr hω)
    refine tendsto_atTop_mono' _ ?_ hlin
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have hp := mul_nonneg (sq_nonneg t) (sub_nonneg.mpr (hx i))
    dsimp [z]
    have he : t^2 * ((x i-(c-t⁻¹))/ω) = (t^2*(x i-c)+t)/ω := by
      field_simp [ht.ne', hω.ne']
      ring
    rw [he]
    exact (div_le_div_iff_of_pos_right hω).mpr (by linarith only [hp])
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hb := ((hφ.continuousAt.tendsto.comp hz).const_mul (2 : ℝ)).div_const ω
  have ht := hb.mul (cdf_atTop.comp harg)
  simpa only [density, Function.comp_apply, mul_one] using ht

private theorem reflected_density (α ξ ω x : ℝ) :
    density (-α) (-ξ) ω (-x) = density α ξ ω x := by
  unfold density
  rw [show (-x - -ξ)/ω = -((x-ξ)/ω) by ring, neg_mul_neg]
  congr 2
  simp [phi_formula]

/-- A left half-normal face is reachable from outside the sample by the
reflected path. -/
theorem left_moving_half_normal {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ}
    (hω : 0 < ω) (hx : ∀ i, x i ≤ c) :
    Tendsto (fun t : ℝ ↦ ∏ i, density (-(t^2)) (c+t⁻¹) ω (x i)) atTop
      (𝓝 (halfNormalEnvelope x c ω)) := by
  have ht := right_moving_half_normal (fun i ↦ -x i) (c := -c) hω
    (fun i ↦ neg_le_neg (hx i))
  have ht' : halfNormalEnvelope (fun i ↦ -x i) (-c) ω = halfNormalEnvelope x c ω := by
    unfold halfNormalEnvelope
    apply Finset.prod_congr rfl
    intro i _
    rw [show (-x i - -c)/ω = -((x i-c)/ω) by ring]
    simp [phi_formula]
  rw [ht'] at ht
  refine ht.congr fun t ↦ ?_
  apply Finset.prod_congr rfl
  intro i _
  have he := reflected_density (t^2) (-c-t⁻¹) ω (-x i)
  simpa only [neg_neg, neg_sub, sub_neg_eq_add, add_comm] using he.symm

/-- Along any feasible location–scale path converging to a positive-scale
endpoint, the unpenalized half-normal product bounds the limiting likelihood
from above, uniformly over slants. -/
theorem half_normal_envelope_upper {n : ℕ} (x : Fin n → ℝ) {c ω : ℝ} (hω : 0 < ω)
    {ι : Type*} {l : Filter ι} {p : ι → ℝ × ℝ} (hp : Tendsto p l (𝓝 (c, ω)))
    (α : ι → ℝ) {b : ℝ} (hb : halfNormalEnvelope x c ω < b) :
    ∀ᶠ i in l, (∏ j, density (α i) (p i).1 (p i).2 (x j)) < b := by
  have hξ : Tendsto (fun i ↦ (p i).1) l (𝓝 c) := continuous_fst.continuousAt.tendsto.comp hp
  have hω' : Tendsto (fun i ↦ (p i).2) l (𝓝 ω) := continuous_snd.continuousAt.tendsto.comp hp
  have hφ : Continuous φ := continuous_iff_continuousAt.mpr
    (fun t ↦ (phi_hasDerivAt t).continuousAt)
  have hprod : Tendsto (fun i ↦ halfNormalEnvelope x (p i).1 (p i).2) l
      (𝓝 (halfNormalEnvelope x c ω)) := by
    refine tendsto_finsetProd Finset.univ fun j _ ↦ ?_
    have h := (hξ.const_sub (x j)).div hω' hω.ne'
    exact ((hφ.continuousAt.tendsto.comp h).const_mul (2 : ℝ)).div hω' hω.ne'
  filter_upwards [hprod.eventually (eventually_lt_nhds hb),
    hω'.eventually (eventually_gt_nhds hω)] with i hi hscale
  refine lt_of_le_of_lt ?_ hi
  unfold density halfNormalEnvelope
  apply Finset.prod_le_prod₀
  · intro j _
    exact (mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hscale) (cdf_pos _)).le
  · intro j _
    exact mul_le_of_le_one_right (div_nonneg (mul_nonneg (by norm_num) (phi_pos _).le) hscale.le)
      (cdf_lt_one _).le

end

end UnivariateML.SkewLSSN
