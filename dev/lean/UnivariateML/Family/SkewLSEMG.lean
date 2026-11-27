module

public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import Mathlib.Analysis.SpecialFunctions.ExpDeriv
public import UnivariateML.Family.SkewLSGaussian
public import Mathlib.Analysis.Convex.Continuous
public import Mathlib.Topology.Order.ExtrClosure
public import UnivariateML.Generic.LocationScale
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Exponentially modified Gaussian

## Deviations

Both face theorems are fixed-location paths. The critical-value
comparison assumes a continuous compactified profile with differentiable
interior.
-/

@[expose] public section

open Set Filter Topology Real UnivariateML.SkewLSGaussian

namespace UnivariateML.SkewLSEMG

noncomputable section

private def densityOf (F : ℝ → ℝ) (rate μ σ x : ℝ) : ℝ :=
  rate * exp ((rate * σ)^2 / 2 - rate * (x - μ)) * F ((x - μ) / σ - rate * σ)

/-- The translated exponential trace gives a factor F(0) at the held endpoint. -/
private theorem exponential_path_of_cdf {F : ℝ → ℝ}
    (hzero : Tendsto F atBot (𝓝 0)) (hone : Tendsto F atTop (𝓝 1))
    (hF : ContinuousAt F 0) (rate μ x : ℝ) :
    Tendsto (fun σ ↦ densityOf F rate μ σ x) (𝓝[>] 0)
      (𝓝 (rate * exp (-rate * (x - μ)) *
        (if μ < x then 1 else if x < μ then 0 else F 0))) := by
  have hσ : Tendsto (fun σ : ℝ ↦ σ) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hs : Tendsto (fun σ ↦ rate * σ) (𝓝[>] 0) (𝓝 0) := by
    simpa using hσ.const_mul rate
  have hc : Tendsto (fun _ : ℝ ↦ rate * (x - μ)) (𝓝[>] 0) (𝓝 (rate * (x - μ))) :=
    tendsto_const_nhds
  have he := Real.continuous_exp.continuousAt.tendsto.comp (((hs.pow 2).div_const 2).sub hc)
  unfold densityOf
  have hprefactor : Tendsto (fun σ ↦ rate * exp ((rate * σ)^2 / 2 - rate * (x - μ)))
      (𝓝[>] 0) (𝓝 (rate * exp (-rate * (x - μ)))) := by
    simpa [Function.comp_def, neg_mul] using he.const_mul rate
  refine hprefactor.mul ?_
  split_ifs with hp hm
  · apply hone.comp
    have hd := tendsto_inv_nhdsGT_zero.const_mul_atTop (sub_pos.mpr hp)
    simpa only [div_eq_mul_inv, Function.comp_apply, sub_eq_add_neg] using hd.atTop_add hs.neg
  · apply hzero.comp
    have hd := tendsto_inv_nhdsGT_zero.const_mul_atTop_of_neg (sub_neg.mpr hm)
    simpa only [div_eq_mul_inv, Function.comp_apply, sub_eq_add_neg] using hd.atBot_add hs.neg
  · have h : x = μ := le_antisymm (le_of_not_gt hp) (le_of_not_gt hm)
    simpa [h, Function.comp_def] using hF.tendsto.comp (by simpa only [neg_zero] using hs.neg)

/-- A Gaussian Mills-ratio limit implies the fixed-location,
fixed-scale normal path as the rate tends to infinity. -/
private theorem normal_path_of_cdf {F : ℝ → ℝ} (c : ℝ)
    (hmills : Tendsto (fun y ↦ y * exp (y ^ 2 / 2) * F (-y)) atTop (𝓝 c))
    (μ x : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun rate ↦ densityOf F rate μ σ x) atTop
      (𝓝 (c / σ * exp (-((x - μ) / σ)^2 / 2))) := by
  let z := (x - μ) / σ
  let y (rate : ℝ) := rate * σ - z
  have hy : Tendsto y atTop atTop := by
    simpa [y, sub_eq_add_neg] using
      tendsto_atTop_add_const_right atTop (-z) (tendsto_id.atTop_mul_const hσ)
  have hr : Tendsto (fun rate ↦ (1 + z * (y rate)⁻¹) / σ) atTop (𝓝 (1 / σ)) := by
    simpa using
      ((tendsto_inv_atTop_zero.comp hy).const_mul z |>.const_add 1).div_const σ
  have ht := ((hmills.comp hy).mul hr).mul_const (exp (-z^2 / 2))
  have he : ∀ᶠ rate in (atTop : Filter ℝ),
      ((y rate * exp ((y rate)^2 / 2) * F (-y rate)) *
        ((1 + z * (y rate)⁻¹) / σ)) * exp (-z^2 / 2) =
      densityOf F rate μ σ x := by
    filter_upwards [hy.eventually (eventually_gt_atTop 0)] with rate hypos
    have hyne : y rate ≠ 0 := hypos.ne'
    have hz : z * σ = x - μ := div_mul_cancel₀ _ hσ.ne'
    have hrate : y rate * ((1 + z * (y rate)⁻¹) / σ) = rate := by
      field_simp
      dsimp [y]
      ring
    have harg : -y rate = (x - μ) / σ - rate * σ := by dsimp [y, z]; ring
    have hexp : y rate ^ 2 / 2 + (-z^2 / 2) = (rate * σ)^2 / 2 - rate * (x - μ) := by
      rw [← hz]
      dsimp [y]
      ring
    calc
      _ = (y rate * ((1 + z * (y rate)⁻¹) / σ)) *
          (exp ((y rate)^2 / 2) * exp (-z^2 / 2)) * F (-y rate) := by ring
      _ = _ := by rw [hrate, ← exp_add, hexp, harg]; rfl
  have ht' := ht.congr' (show _ from he)
  simpa only [mul_one_div] using ht'

/-- The exponentially modified Gaussian density in physical parameters. -/
def density (rate μ σ x : ℝ) : ℝ :=
  rate * exp ((rate * σ)^2 / 2 - rate * (x - μ)) * Φ ((x - μ) / σ - rate * σ)

/-- With rate and location held, the scale-zero path has half the
translated-exponential density at the endpoint. -/
theorem exponential_path (rate μ x : ℝ) :
    Tendsto (fun σ ↦ density rate μ σ x) (𝓝[>] 0)
      (𝓝 (rate * exp (-rate * (x - μ)) *
        (if μ < x then 1 else if x < μ then 0 else 1/2))) := by
  simpa only [density, densityOf, cdf_zero] using
    exponential_path_of_cdf cdf_atBot cdf_atTop cdf_continuous.continuousAt rate μ x

/-- With Gaussian location and positive scale held, infinite rate gives the
normal density at that location and scale. -/
theorem normal_path (μ x : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun rate ↦ density rate μ σ x) atTop (𝓝 (φ ((x - μ)/σ) / σ)) := by
  rw [phi_formula]
  have h := normal_path_of_cdf (1 / sqrt (2 * π)) mills μ x hσ
  simpa only [density, densityOf, div_mul_eq_mul_div, neg_div] using h

private theorem density_positive {rate σ : ℝ} (hr : 0 < rate) (μ x : ℝ) :
    0 < density rate μ σ x := by
  unfold density
  exact mul_pos (mul_pos hr (exp_pos _)) (cdf_pos _)

private theorem density_location_left {rate σ : ℝ} (hr : 0 < rate) (x : ℝ) :
    Tendsto (fun μ ↦ density rate μ σ x) atBot (𝓝 0) := by
  let pref (μ : ℝ) := rate * exp ((rate*σ)^2/2 - rate*(x-μ))
  have he : Tendsto (fun μ : ℝ ↦ (rate*σ)^2/2 - rate*(x-μ)) atBot atBot := by
    convert tendsto_atBot_add_const_right atBot
      ((rate*σ)^2/2 - rate*x) (tendsto_id.const_mul_atBot hr) using 1
    ext μ
    simp only [id_eq]
    ring
  have ht : Tendsto pref atBot (𝓝 0) := by
    simpa [pref, Function.comp_def] using (tendsto_exp_atBot.comp he).const_mul rate
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le tendsto_const_nhds ht
  · intro μ
    exact (density_positive (σ := σ) hr μ x).le
  · intro μ
    unfold density pref
    exact mul_le_of_le_one_right (mul_nonneg hr.le (exp_pos _).le) (cdf_lt_one _).le

private theorem density_location_right {rate σ : ℝ} (hσ : 0 < σ) (x : ℝ) :
    Tendsto (fun μ ↦ density rate μ σ x) atTop (𝓝 0) := by
  let z (μ : ℝ) := (x-μ)/σ
  let y (μ : ℝ) := rate*σ - z μ
  have hy : Tendsto y atTop atTop := by
    convert tendsto_atTop_add_const_right atTop (rate*σ-x/σ)
      (tendsto_id.const_mul_atTop (inv_pos.mpr hσ)) using 1
    ext μ
    dsimp [y, z]
    ring
  have hφ : Tendsto (fun μ ↦ exp (-(z μ)^2/2)) atTop (𝓝 0) := by
    have hnegz : Tendsto (fun μ ↦ -z μ) atTop atTop := by
      convert tendsto_atTop_add_const_left atTop (-(rate*σ)) hy using 1
      ext μ
      dsimp [y]
      ring
    have hz2 := (hnegz.atTop_pow₀ (show (2 : ℕ) ≠ 0 by decide)).atTop_mul_const
      (show (0 : ℝ) < 1/2 by norm_num)
    simpa [Function.comp_def, div_eq_mul_inv] using tendsto_exp_neg_atTop_nhds_zero.comp hz2
  have hfrac : Tendsto (fun μ ↦ rate / y μ) atTop (𝓝 0) := by
    simpa [div_eq_mul_inv, Function.comp_def] using (tendsto_inv_atTop_zero.comp hy).const_mul rate
  have ht := ((mills.comp hy).mul hfrac).mul hφ
  simp only [mul_zero] at ht
  refine ht.congr' ?_
  filter_upwards [hy.eventually (eventually_gt_atTop (0 : ℝ))] with μ hμ
  change (y μ * exp ((y μ)^2/2) * Φ (-y μ)) * (rate / y μ) *
    exp (-(z μ)^2/2) = density rate μ σ x
  have hz : z μ * σ = x - μ := div_mul_cancel₀ _ hσ.ne'
  have harg : -y μ = (x-μ)/σ - rate*σ := by dsimp [y, z]; ring
  have hexp : (y μ)^2/2 + (-(z μ)^2/2) = (rate*σ)^2/2-rate*(x-μ) := by
    rw [← hz]
    dsimp [y]
    ring
  calc
    _ = rate * (exp ((y μ)^2/2) * exp (-(z μ)^2/2)) * Φ (-y μ) := by
      field_simp [ne_of_gt hμ]
    _ = _ := by rw [← exp_add, hexp, harg]; rfl

/-- The sum of normalized EMG log densities. -/
def locationLogLik {n : ℕ} (x : Fin n → ℝ) (rate σ μ : ℝ) : ℝ :=
  ∑ i, log (density rate μ σ (x i))

private theorem location_log_formula {rate σ : ℝ} (hr : 0 < rate) (μ v : ℝ) :
    log (density rate μ σ v) =
      log rate + ((rate*σ)^2/2 - rate*(v-μ)) + log (Φ ((v-μ)/σ-rate*σ)) := by
  unfold density
  rw [log_mul (mul_ne_zero hr.ne' (exp_pos _).ne') (cdf_pos _).ne',
    log_mul hr.ne' (exp_pos _).ne', log_exp]

private theorem location_strictConcave {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate σ : ℝ} (hr : 0 < rate) (hσ : 0 < σ) :
    StrictConcaveOn ℝ univ (locationLogLik x rate σ) := by
  refine ⟨convex_univ, ?_⟩
  intro μ _ ν _ hμν u v hu hv huv
  simp only [locationLogLik, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  let z (m : ℝ) := (x i-m)/σ-rate*σ
  have hne : z μ ≠ z ν := by
    intro h
    apply hμν
    dsimp [z] at h
    have he : (x i-μ)/σ = (x i-ν)/σ := by linarith
    have h' := congrArg (fun w : ℝ ↦ w*σ) he
    simp only [div_mul_cancel₀ _ hσ.ne'] at h'
    linarith
  have hz : z (u*μ+v*ν) = u*z μ + v*z ν := by
    dsimp [z]
    rw [show v = 1-u by linarith]
    ring
  have hc := cdf_strictLogConcave.2 (mem_univ (z μ)) (mem_univ (z ν)) hne hu hv huv
  simp only [smul_eq_mul] at hc
  rw [← hz] at hc
  simp only [location_log_formula hr]
  change u * (log rate + ((rate*σ)^2/2-rate*(x i-μ)) + log (Φ (z μ))) +
    v * (log rate + ((rate*σ)^2/2-rate*(x i-ν)) + log (Φ (z ν))) <
      log rate + ((rate*σ)^2/2-rate*(x i-(u*μ+v*ν))) + log (Φ (z (u*μ+v*ν)))
  rw [show v = 1-u by linarith] at hc ⊢
  nlinarith only [hc]

/-- A positive held rate and normal scale leave a unique finite location
fit on every nonempty sample. -/
theorem held_rate_scale {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate σ : ℝ} (hr : 0 < rate) (hσ : 0 < σ) :
    ∃! μ, IsMaxOn (locationLogLik x rate σ) univ μ := by
  let L := locationLogLik x rate σ 0
  let e := exp (L/n - 1)
  have he : 0 < e := exp_pos _
  have hleft : ∀ᶠ μ : ℝ in atBot, ∀ i, density rate μ σ (x i) < e := by
    rw [Filter.eventually_all]
    exact fun i ↦ (density_location_left (σ := σ) hr (x i)).eventually (eventually_lt_nhds he)
  have hright : ∀ᶠ μ : ℝ in atTop, ∀ i, density rate μ σ (x i) < e := by
    rw [Filter.eventually_all]
    exact fun i ↦ (density_location_right (rate := rate) hσ (x i)).eventually
      (eventually_lt_nhds he)
  obtain ⟨A, hA⟩ := eventually_atBot.mp hleft
  obtain ⟨B, hB⟩ := eventually_atTop.mp hright
  let K := Icc (min A 0) (max B 0)
  have h0 : (0 : ℝ) ∈ K := ⟨min_le_right _ _, le_max_right _ _⟩
  have hc := location_strictConcave hn x hr hσ
  obtain ⟨m, hm, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨0, h0⟩
    ((hc.concaveOn.continuousOn isOpen_univ).mono (subset_univ _))
  have hglobal : IsMaxOn (locationLogLik x rate σ) univ m := by
    intro μ _
    by_cases hμ : μ ∈ K
    · exact hmax hμ
    · have hb : ∀ i, density rate μ σ (x i) < e := by
        by_cases hl : min A 0 ≤ μ
        · have hu : max B 0 < μ := lt_of_not_ge (fun hu ↦ hμ ⟨hl, hu⟩)
          exact hB μ ((le_max_left _ _).trans hu.le)
        · exact hA μ ((le_of_not_ge hl).trans (min_le_left _ _))
      have hsum : locationLogLik x rate σ μ ≤ n * log e := by
        unfold locationLogLik
        calc
          _ ≤ ∑ _ : Fin n, log e := Finset.sum_le_sum fun i _ ↦
            (log_lt_log (density_positive (σ := σ) hr μ (x i)) (hb i)).le
          _ = _ := by simp [nsmul_eq_mul]
      have hn' : (0 : ℝ) < n := by exact_mod_cast hn
      have heq : (n : ℝ) * (L/n) = L := by field_simp
      have hlevel : n * log e ≤ L := by dsimp [e]; rw [log_exp]; linarith
      exact (hsum.trans hlevel).trans (isMaxOn_iff.mp hmax 0 h0)
  exact ⟨m, hglobal, fun q hq ↦ hc.eq_of_isMaxOn hq hglobal (mem_univ _) (mem_univ _)⟩

private def weightedTail (y : ℝ) : ℝ := exp (y^2/2) * Φ (-y)

private theorem weightedTail_strictAnti : StrictAnti weightedTail := by
  have hd (y : ℝ) : HasDerivAt weightedTail
      (exp (y^2/2) * (y * Φ (-y) - φ (-y))) y := by
    have he : HasDerivAt (fun y : ℝ ↦ exp (y^2/2)) (exp (y^2/2)*y) y := by
      convert (((hasDerivAt_id y).pow 2).div_const 2).exp using 1
      · rfl
      · change exp (y^2/2)*y = exp (y^2/2)*(2*y^1*1/2)
        ring
    have hF : HasDerivAt (fun y ↦ Φ (-y)) (-φ (-y)) y := by
      simpa [Function.comp_def] using (cdf_hasDerivAt (-y)).comp y (hasDerivAt_id y).neg
    convert he.mul hF using 1
    · rfl
    · ring
  apply strictAnti_of_deriv_neg
  intro y
  rw [(hd y).deriv]
  exact mul_neg_of_pos_of_neg (exp_pos _) (by linarith [phi_add_mul_cdf_pos (-y)])

private theorem density_weightedTail (rate μ x : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    density rate μ σ x = rate * exp (-((x-μ)/σ)^2/2) *
      weightedTail (rate*σ - (x-μ)/σ) := by
  unfold density weightedTail
  rw [show -(rate*σ-(x-μ)/σ) = (x-μ)/σ-rate*σ by ring]
  have h := div_mul_cancel₀ (x-μ) hσ.ne'
  have he : (rate*σ)^2/2 - rate*(x-μ) =
      -((x-μ)/σ)^2/2 + (rate*σ-(x-μ)/σ)^2/2 := by
    conv_lhs => rw [← h]
    ring
  rw [he, exp_add]
  ring

/-- With location held weakly above an observation, every finite rate gives
a smaller density than the corresponding fixed-scale normal limit. -/
theorem normal_dominates_at_held_location {rate σ μ x : ℝ}
    (hr : 0 < rate) (hσ : 0 < σ) (hx : x ≤ μ) :
    density rate μ σ x < φ ((x-μ)/σ) / σ := by
  let k := rate*σ
  let z := (x-μ)/σ
  have hk : 0 < k := mul_pos hr hσ
  have hz : z ≤ 0 := div_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr hx) hσ.le
  have htail : k * weightedTail k < 1 / sqrt (2 * π) := by
    have hp := phi_add_mul_cdf_pos (-k)
    have h := mul_lt_mul_of_pos_left
      (show k * Φ (-k) < φ (-k) by linarith) (exp_pos (k^2/2))
    rw [phi_formula, neg_sq, show exp (k^2/2) *
      (1 / sqrt (2*π) * exp (-(k^2/2))) = 1 / sqrt (2*π) by
        calc
          _ = (1 / sqrt (2*π)) * (exp (k^2/2) * exp (-(k^2/2))) := by ring
          _ = _ := by rw [← exp_add, add_neg_cancel, exp_zero, mul_one]] at h
    dsimp [weightedTail]
    linarith only [h, show k * (exp (k^2/2)*Φ (-k)) = exp (k^2/2)*(k*Φ (-k)) by ring]
  have hw := weightedTail_strictAnti.antitone (show k ≤ k-z by linarith)
  have hbound : k * weightedTail (k-z) < 1 / sqrt (2*π) :=
    (mul_le_mul_of_nonneg_left hw hk.le).trans_lt htail
  rw [density_weightedTail rate μ x hσ, phi_formula]
  apply (lt_div_iff₀ hσ).mpr
  change rate * exp (-z^2/2) * weightedTail (k-z) * σ <
    (1 / sqrt (2*π)) * exp (-(z^2/2))
  rw [neg_div]
  calc
    _ = (k * weightedTail (k-z)) * exp (-(z^2/2)) := by dsimp [k]; ring
    _ < _ := mul_lt_mul_of_pos_right hbound (exp_pos _)

/-- With location at or above all observations and scale fixed, the normal
likelihood is the unattained rate-profile supremum. -/
theorem held_location_scale_normal_supremum {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) {μ σ : ℝ} (hσ : 0 < σ) (hx : ∀ i, x i ≤ μ) :
    IsLUB ((fun rate : ℝ ↦ ∏ i, density rate μ σ (x i)) '' Ioi 0)
      (∏ i, φ ((x i-μ)/σ)/σ) ∧
    ∀ rate > 0, (∏ i, density rate μ σ (x i)) < ∏ i, φ ((x i-μ)/σ)/σ := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  have ht := tendsto_finsetProd Finset.univ (fun i _ ↦ normal_path μ (x i) hσ)
  have hs (r : ℝ) (hr : 0 < r) :
      (∏ i, density r μ σ (x i)) < ∏ i, φ ((x i-μ)/σ)/σ :=
    Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ density_positive hr μ (x i))
      (fun i _ ↦ normal_dominates_at_held_location hr hσ (hx i)) Finset.univ_nonempty
  refine ⟨⟨?_, ?_⟩, hs⟩
  · rintro _ ⟨r, hr, rfl⟩
    exact (hs r hr).le
  · intro z hz
    exact le_of_tendsto ht ((eventually_gt_atTop (0 : ℝ)).mono
      (fun r hr ↦ hz (mem_image_of_mem _ hr)))

/-- At a constant sample's held location, with positive rate held, the
density supremum over scale is half the rate and is unattained. -/
theorem held_location_rate_at_sample {rate : ℝ} (hr : 0 < rate) (m : ℝ) :
    IsLUB ((fun σ ↦ density rate m σ m) '' Ioi 0) (rate/2) ∧
      ∀ σ > 0, density rate m σ m < rate/2 := by
  have hsmall (σ : ℝ) (hs : 0 < σ) : density rate m σ m < rate/2 := by
    rw [density_weightedTail rate m m hs]
    have ht := weightedTail_strictAnti (mul_pos hr hs)
    rw [show weightedTail 0 = 1/2 by simp [weightedTail, cdf_zero]] at ht
    simp only [sub_self, zero_div, zero_pow (by decide : (2 : ℕ) ≠ 0), neg_zero,
      exp_zero, mul_one, sub_zero]
    rw [show rate/2 = rate*(1/2) by ring]
    exact mul_lt_mul_of_pos_left ht hr
  have ht : Tendsto (fun σ ↦ density rate m σ m) (𝓝[>] 0) (𝓝 (rate/2)) := by
    simpa [div_eq_mul_inv] using exponential_path rate m m
  refine ⟨⟨?_, ?_⟩, hsmall⟩
  · rintro _ ⟨σ, hs, rfl⟩
    exact (hsmall σ hs).le
  · intro z hz
    have he : ∀ᶠ σ : ℝ in 𝓝[>] 0, σ ∈ Ioi 0 := self_mem_nhdsWithin
    exact le_of_tendsto ht (he.mono fun σ hσ ↦ hz (mem_image_of_mem _ hσ))

private theorem exponential_peak {rate μ σ x : ℝ} (hr : 0 < rate) (hs : 0 < σ) :
    density rate μ σ x < rate := by
  let z := (x-μ)/σ
  let k := rate*σ
  have hk : 0 < k := mul_pos hr hs
  by_cases hz : k/2 ≤ z
  · have he : (rate*σ)^2/2-rate*(x-μ) ≤ 0 := by
      have hh := mul_nonneg hk.le (sub_nonneg.mpr hz)
      rw [← div_mul_cancel₀ (x-μ) hs.ne']
      change k^2/2-rate*(z*σ) ≤ 0
      dsimp [k] at hh ⊢
      nlinarith only [hh]
    unfold density
    exact (mul_lt_of_lt_one_right (mul_pos hr (exp_pos _)) (cdf_lt_one _)).trans_le
      (mul_le_of_le_one_right hr.le (exp_le_one_iff.mpr he))
  · rw [density_weightedTail rate μ x hs]
    have hy : 0 < k-z := by linarith
    have ht := weightedTail_strictAnti hy
    rw [show weightedTail 0 = 1/2 by simp [weightedTail, cdf_zero]] at ht
    have he : exp (-z^2/2) ≤ 1 := exp_le_one_iff.mpr (by nlinarith [sq_nonneg z])
    exact (mul_lt_of_lt_one_right (mul_pos hr (exp_pos _)) (ht.trans (by norm_num))).trans_le
      (mul_le_of_le_one_right hr.le he)

/-- For constant data and held positive rate, varying location and scale
approaches the exponential peak, which no finite fit reaches. -/
theorem held_rate_at_sample {rate : ℝ} (hr : 0 < rate) (m : ℝ) :
    IsLUB ((fun p : ℝ × ℝ ↦ density rate p.1 p.2 m) '' (univ ×ˢ Ioi 0)) rate ∧
      ∀ μ σ : ℝ, 0 < σ → density rate μ σ m < rate := by
  have ht : Tendsto (fun t : ℝ ↦ density rate (m-t⁻¹) ((t⁻¹)^2) m) atTop (𝓝 rate) := by
    have hs := (tendsto_inv_atTop_zero (𝕜 := ℝ)).pow 2
    have hd : Tendsto (fun t : ℝ ↦ t - rate*(t⁻¹)^2) atTop atTop := by
      simpa only [sub_eq_add_neg, id_eq] using tendsto_id.atTop_add (hs.const_mul rate).neg
    have he : Tendsto (fun t : ℝ ↦ (rate*(t⁻¹)^2)^2/2 - rate*t⁻¹) atTop (𝓝 0) := by
      simpa using (((hs.const_mul rate).pow 2).div_const 2).sub
        ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul rate)
    have hpref := (continuous_exp.continuousAt.tendsto.comp he).const_mul rate
    have ht' := hpref.mul (cdf_atTop.comp hd)
    simp only [exp_zero, mul_one] at ht'
    refine ht'.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have hcancel : t⁻¹ / (t⁻¹)^2 = t := by field_simp [ht.ne']
    simp only [density, Function.comp_apply, sub_sub_cancel, hcancel]
  refine ⟨⟨?_, ?_⟩, fun μ σ hs ↦ exponential_peak hr hs⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (exponential_peak hr hp.2).le
  · intro z hz
    exact le_of_tendsto ht ((eventually_gt_atTop (0 : ℝ)).mono
      (fun t ht ↦ hz ⟨(m-t⁻¹, (t⁻¹)^2), ⟨mem_univ _, pow_pos (inv_pos.mpr ht) _⟩, rfl⟩))

/-- At a constant observation a fixed dimensionless shape and shrinking
Gaussian scale yield an unbounded density. The path holds location fixed. -/
theorem constant_collapse (m : ℝ) :
    Tendsto (fun σ ↦ density σ⁻¹ m σ m) (𝓝[>] 0) atTop := by
  have ht := tendsto_inv_nhdsGT_zero.atTop_mul_const
    (show 0 < exp (1/2) * Φ (-1) from mul_pos (exp_pos _) (cdf_pos _))
  refine Tendsto.congr' ?_ ht
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with σ (hσ : 0 < σ)
  simp [density, hσ.ne', mul_assoc]

/-- At the held common observation, the all-free rate and scale path
makes the likelihood unbounded for every nonempty constant sample. -/
theorem constant_sample_unbounded {n : ℕ} (hn : 0 < n) (m : ℝ) :
    Tendsto (fun σ ↦ ∏ _ : Fin n, density σ⁻¹ m σ m) (𝓝[>] 0) atTop := by
  simpa only [Finset.prod_const, Finset.card_univ, Fintype.card_fin] using
    (constant_collapse m).atTop_pow₀ hn.ne'

private theorem phi_below_peak {z : ℝ} (hz : z ≠ 0) : φ z < φ 0 := by
  rw [phi_formula, phi_formula]
  simp only [zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, neg_zero, exp_zero]
  have h : 0 < z^2 / 2 := div_pos (sq_pos_of_ne_zero hz) (by norm_num)
  exact mul_lt_mul_of_pos_left (exp_lt_one_iff.mpr (neg_lt_zero.mpr h)) (by positivity)

private theorem phi_at_most_peak (z : ℝ) : φ z ≤ φ 0 := by
  by_cases hz : z = 0
  · simp [hz]
  · exact (phi_below_peak hz).le

/-- The Gaussian peak strictly bounds every finite EMG density at a
held Gaussian scale. -/
theorem gaussian_peak_strict {rate μ σ x : ℝ} (hr : 0 < rate) (hσ : 0 < σ) :
    density rate μ σ x < φ 0 / σ := by
  let P := φ 0 / σ
  let z (t : ℝ) := (x-μ-t)/σ
  let k := rate*σ
  let C := rate * exp (k^2/2 - rate*(x-μ))
  let G (t : ℝ) := P*(1-exp (-rate*t)) + C*Φ (z t-k)
  have hz (t : ℝ) : HasDerivAt (fun t ↦ z t-k) (-(1/σ)) t := by
    have h := ((hasDerivAt_const t (x-μ)).sub (hasDerivAt_id t)).div_const σ
    convert h.sub_const k using 1
    · rfl
    · norm_num [neg_div]
  have hderiv (t : ℝ) : HasDerivAt G
      (rate*exp (-rate*t)*(P-φ (z t)/σ)) t := by
    have hlin : HasDerivAt (fun t : ℝ ↦ -rate*t) (-rate) t := by
      simpa only [id_eq, mul_one] using (hasDerivAt_id t).const_mul (-rate)
    have h := ((hlin.exp.const_sub 1).const_mul P).add
      (((cdf_hasDerivAt (z t-k)).comp t (hz t)).const_mul C)
    convert h using 1
    · rfl
    · change rate*exp (-rate*t)*(P-φ (z t)/σ) =
        P * -(exp (-rate*t)*(-rate)) + C*(φ (z t-k)*(-(1/σ)))
      have he : C * φ (z t-k) = rate*exp (-rate*t)*φ (z t) := by
        rw [phi_formula, phi_formula]
        have hzn : z t * σ = x-μ-t := div_mul_cancel₀ _ hσ.ne'
        have hpow : (k^2/2 - rate*(x-μ)) + -((z t-k)^2/2) =
            -rate*t + -(z t^2/2) := by
          dsimp [k]
          rw [show x-μ = z t*σ+t by linarith only [hzn]]
          ring
        dsimp [C]
        calc
          _ = rate*(1/sqrt (2*π))*exp ((k^2/2-rate*(x-μ)) + -((z t-k)^2/2)) := by
            rw [exp_add]
            ring
          _ = _ := by rw [hpow, exp_add]; ring
      have he' := congrArg (fun q : ℝ ↦ q/σ) he
      linarith only [show
        rate*exp (-rate*t)*(P-φ (z t)/σ) -
          (P * -(exp (-rate*t)*(-rate)) + C*(φ (z t-k)*(-(1/σ)))) =
            (C * φ (z t-k))/σ - (rate*exp (-rate*t)*φ (z t))/σ by ring, he']
  have hm : Monotone G := monotone_of_deriv_nonneg
    (fun t ↦ (hderiv t).differentiableAt) (fun t ↦ by
      rw [(hderiv t).deriv]
      exact mul_nonneg (mul_pos hr (exp_pos _)).le
        (sub_nonneg.mpr (div_le_div_of_nonneg_right (phi_at_most_peak _) hσ.le)))
  have hstrict : StrictMonoOn G (Ioi (x-μ)) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi _)
      (fun t _ ↦ (hderiv t).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ioi] at ht
    rw [(hderiv t).deriv]
    have hne : z t ≠ 0 :=
      div_ne_zero (sub_ne_zero.mpr (ne_of_lt (show x-μ < t from ht))) hσ.ne'
    exact mul_pos (mul_pos hr (exp_pos _))
      (sub_pos.mpr ((div_lt_div_iff_of_pos_right hσ).mpr (phi_below_peak hne)))
  have hlim : Tendsto G atTop (𝓝 P) := by
    have ht := tendsto_id.const_mul_atTop_of_neg (neg_neg_of_pos hr)
    have h₁ := ((tendsto_exp_atBot.comp ht).const_sub (1 : ℝ)).const_mul P
    have hzlim : Tendsto (fun t ↦ z t-k) atTop atBot := by
      convert tendsto_atBot_add_const_right atTop ((x-μ)/σ-k)
        (tendsto_id.const_mul_atTop_of_neg (neg_neg_of_pos (inv_pos.mpr hσ))) using 1
      ext t
      dsimp [z]
      ring
    simpa [G, Function.comp_def] using h₁.add ((cdf_atBot.comp hzlim).const_mul C)
  let a := max 0 (x-μ) + 1
  have ha : a ∈ Ioi (x-μ) := by
    change x-μ < max 0 (x-μ)+1
    linarith [le_max_right (0 : ℝ) (x-μ)]
  have ha0 : 0 ≤ a := by dsimp [a]; linarith [le_max_left (0 : ℝ) (x-μ)]
  have ha' : a+1 ∈ Ioi (x-μ) := by
    have h : x-μ < a := ha
    change x-μ < a+1
    linarith
  have hbound : G (a+1) ≤ P :=
    ge_of_tendsto hlim ((eventually_ge_atTop (a+1)).mono (fun _ ht ↦ hm ht))
  have hlt := (hm ha0).trans_lt ((hstrict ha ha' (lt_add_one a)).trans_le hbound)
  simpa [G, C, density, z, k] using hlt

/-- For a constant sample and held Gaussian scale, the normal peak is the
unattained supremum over location and rate, stated per observation. -/
theorem held_scale_at_sample (m : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    IsLUB ((fun p : ℝ × ℝ ↦ density p.2 p.1 σ m) '' (univ ×ˢ Ioi 0)) (φ 0/σ) ∧
    ∀ μ rate : ℝ, 0 < rate → density rate μ σ m < φ 0/σ := by
  have ht : Tendsto (fun rate ↦ density rate m σ m) atTop (𝓝 (φ 0/σ)) := by
    simpa only [sub_self, zero_div] using normal_path m m hσ
  refine ⟨⟨?_, ?_⟩, fun μ rate hr ↦ gaussian_peak_strict hr hσ⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (gaussian_peak_strict hp.2 hσ).le
  · intro z hz
    exact le_of_tendsto ht ((eventually_gt_atTop (0 : ℝ)).mono
      (fun r hr ↦ hz ⟨(m,r), ⟨mem_univ _, hr⟩, rfl⟩))

/-- When every observation is at or below a held location, raising the
rate toward the normal face improves on any finite rate, even if scale is free. -/
theorem held_location_above_no_fit {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (μ : ℝ) (hx : ∀ i, x i ≤ μ) :
    ¬ ∃ p ∈ (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ i, density p.1 μ p.2 (x i)) (Ioi 0 ×ˢ Ioi 0) p := by
  rintro ⟨p, hp, hm⟩
  obtain ⟨_, hstrict⟩ := held_location_scale_normal_supremum hn x hp.2 hx
  have ht := tendsto_finsetProd Finset.univ (fun i _ ↦ normal_path μ (x i) hp.2)
  have hbetter := ht.eventually (eventually_gt_nhds (hstrict p.1 hp.1))
  obtain ⟨r, hr, hbetter⟩ := ((eventually_gt_atTop (0 : ℝ)).and hbetter).exists
  exact hbetter.not_ge (isMaxOn_iff.mp hm (r,p.2) ⟨hr, hp.2⟩)

private theorem gaussian_tail_fourth {w : ℝ} (hw : 1 ≤ w) :
    1 - Φ w ≤ (16 / sqrt (2*π))/w^4 := by
  have hw0 : 0 < w := zero_lt_one.trans_le hw
  have hbound : 1-Φ w ≤ φ w / w := by
    rw [← cdf_reflect]
    apply (le_div_iff₀ hw0).mpr
    have h := phi_add_mul_cdf_pos (-w)
    rw [phi_formula, neg_sq, ← phi_formula] at h
    linarith
  have he : exp (-(w^2/4)) ≤ 4/w^2 := by
    rw [exp_neg]
    have h := inv_anti₀ (show 0 < w^2/4 by positivity)
      (show w^2/4 ≤ exp (w^2/4) by linarith [add_one_le_exp (w^2/4)])
    simpa only [inv_div] using h
  have hp := pow_le_pow_left₀ (exp_pos _).le he 2
  rw [show exp (-(w^2/4))^2 = exp (-(w^2/2)) by
    rw [pow_two, ← exp_add]; congr 1; ring, div_pow] at hp
  calc
    _ ≤ φ w / w := hbound
    _ ≤ φ w := (div_le_iff₀ hw0).mpr (le_mul_of_one_le_right (phi_pos _).le hw)
    _ ≤ _ := by
      rw [phi_formula]
      have hh := mul_le_mul_of_nonneg_left hp (show 0 ≤ 1/sqrt (2*π) by positivity)
      convert hh using 1
      field_simp
      ring

private theorem eventually_above_exp_face {rate μ x : ℝ} (hr : 0 < rate) (hx : μ < x) :
    ∀ᶠ t : ℝ in atTop, rate * exp (-rate*(x-μ)) < density rate μ t⁻¹ x := by
  let d := x-μ
  have hd : 0 < d := sub_pos.mpr hx
  let C := 256 / sqrt (2*π) / d^4
  have hC : 0 < C := by dsimp [C]; positivity
  have ht2 : Tendsto (fun t : ℝ ↦ t^2) atTop atTop :=
    tendsto_id.atTop_pow₀ (by decide : (2 : ℕ) ≠ 0)
  filter_upwards [eventually_ge_atTop (2/d), eventually_gt_atTop (0 : ℝ),
    ht2.eventually (eventually_ge_atTop (2*rate/d)),
    ht2.eventually (eventually_ge_atTop (rate^2)),
    ht2.eventually (eventually_gt_atTop (4*C/rate^2))] with t ht₁ ht ht₂ ht₃ ht₄
  let w := d*t-rate/t
  have hw : d*t/2 ≤ w := by
    have h := mul_le_mul_of_nonneg_right ((div_le_iff₀ hd).mp ht₂) (show 0 ≤ (1/2 : ℝ) by norm_num)
    have h' : rate/t ≤ d*t/2 := (div_le_iff₀ ht).mpr (by nlinarith only [h])
    dsimp [w]
    linarith
  have hw' : 1 ≤ w := by
    have h := (div_le_iff₀ hd).mp ht₁
    linarith only [h, hw]
  have htail : 1-Φ w ≤ C/t^4 := by
    refine (gaussian_tail_fourth hw').trans ?_
    have hp := pow_le_pow_left₀ (show 0 ≤ d*t/2 by positivity) hw 4
    have hc : 0 ≤ 16/sqrt (2*π) := by positivity
    have hpos : 0 < (d*t/2)^4 := by positivity
    calc
      _ ≤ (16/sqrt (2*π))/(d*t/2)^4 :=
        div_le_div_of_nonneg_left hc hpos hp
      _ = C/t^4 := by dsimp [C]; field_simp; ring
  let u := (rate/t)^2/2
  have hu : 0 < u := by dsimp [u]; positivity
  have hu' : u ≤ 1 := by
    dsimp [u]
    rw [div_pow]
    have h := (div_le_iff₀ (sq_pos_of_pos ht)).mpr
      (show rate^2 ≤ 1*t^2 by simpa only [one_mul] using ht₃)
    linarith
  have hgain : u/2 ≤ 1-exp (-u) := by
    have h := add_one_le_exp u
    have hh : exp (-u) ≤ 1/(1+u) := by
      rw [exp_neg, one_div]
      exact inv_anti₀ (by positivity) (by linarith)
    have hl : 1 / (1+u) ≤ 1-u/2 := by
      apply (div_le_iff₀ (show 0 < 1+u by positivity)).mpr
      nlinarith [mul_nonneg hu.le (sub_nonneg.mpr hu')]
    linarith
  have htail' : C/t^4 < u/2 := by
    apply (div_lt_iff₀ (pow_pos ht 4)).mpr
    have h := (div_lt_iff₀ (sq_pos_of_pos hr)).mp ht₄
    have he : u/2*t^4 = rate^2*t^2/4 := by dsimp [u]; field_simp; ring
    rw [he]
    nlinarith only [h]
  have hφ : exp (-u) < Φ w := by linarith only [htail, hgain, htail']
  have hprod : 1 < exp u * Φ w := by
    have h := mul_lt_mul_of_pos_left hφ (exp_pos u)
    rwa [← exp_add, add_neg_cancel, exp_zero] at h
  unfold density
  rw [show ((x-μ)/t⁻¹-rate*t⁻¹) = w by dsimp [w, d]; rw [div_inv_eq_mul]; ring,
    show (rate*t⁻¹)^2/2 = u by dsimp [u]; simp only [div_eq_mul_inv],
    sub_eq_add_neg u, exp_add]
  have h := mul_lt_mul_of_pos_left hprod (mul_pos hr (exp_pos (-rate*(x-μ))))
  rw [mul_one] at h
  convert h using 1
  ring_nf

private theorem scalar_face_iff {f : ℝ → ℝ} {L H : ℝ}
    (hc : ContinuousOn f (Ioi 0)) (hlow : Tendsto f (𝓝[>] 0) (𝓝 L))
    (hhigh : Tendsto f atTop (𝓝 H)) :
    (∃ s ∈ Ioi 0, IsMaxOn f (Ioi 0) s) ↔ ∃ s > 0, max L H ≤ f s := by
  constructor
  · rintro ⟨s, hs, hm⟩
    have hD : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioi 0 := self_mem_nhdsWithin
    exact ⟨s, hs, max_le
      (le_of_tendsto hlow (hD.mono fun _ ht ↦ hm ht))
      (le_of_tendsto hhigh ((eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ hm ht))⟩
  · rintro ⟨s₀, hs₀, hw⟩
    by_cases hb : ∃ s > 0, max L H < f s
    · obtain ⟨s₁, hs₁, hbetter⟩ := hb
      have hlo := hlow.eventually (eventually_lt_nhds ((le_max_left L H).trans_lt hbetter))
      have hhi := hhigh.eventually (eventually_lt_nhds ((le_max_right L H).trans_lt hbetter))
      obtain ⟨ε, hε, hε'⟩ := Metric.mem_nhdsWithin_iff.mp hlo
      obtain ⟨R, hR⟩ := eventually_atTop.mp hhi
      let A := min (ε/2) s₁
      let B := max R s₁
      have ha : 0 < A := lt_min (half_pos hε) hs₁
      have h₁ : s₁ ∈ Icc A B := ⟨min_le_right _ _, le_max_right _ _⟩
      obtain ⟨s, hs, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨s₁, h₁⟩
        (hc.mono fun _ hp ↦ ha.trans_le hp.1)
      refine ⟨s, ha.trans_le hs.1, fun z hz ↦ ?_⟩
      by_cases hmem : z ∈ Icc A B
      · exact hmax hmem
      · have hbelow : f z < f s₁ := by
          by_cases hza : z < A
          · apply hε'
            refine ⟨?_, hz⟩
            rw [Metric.mem_ball, Real.dist_eq, sub_zero, abs_of_pos hz]
            exact hza.trans ((min_le_left _ _).trans_lt (by linarith))
          · have hzb : B < z := lt_of_not_ge (fun h ↦ hmem ⟨le_of_not_gt hza, h⟩)
            exact hR z ((le_max_left _ _).trans hzb.le)
        exact hbelow.le.trans (isMaxOn_iff.mp hmax s₁ h₁)
    · exact ⟨s₀, hs₀, fun q hq ↦ (le_of_not_gt (fun h ↦ hb ⟨q, hq, h⟩)).trans hw⟩

/-- Product of fixed-endpoint exponential face values, including half
weights at ties and zero weight to the left of the endpoint. -/
def exponentialTrace {n : ℕ} (x : Fin n → ℝ) (rate μ : ℝ) : ℝ :=
  ∏ i, rate * exp (-rate*(x i-μ)) *
    (if μ < x i then 1 else if x i < μ then 0 else 1/2)

private theorem exp_trace_nonneg {n : ℕ} (x : Fin n → ℝ) {rate μ : ℝ} (hr : 0 < rate) :
    0 ≤ exponentialTrace x rate μ := by
  unfold exponentialTrace
  refine Finset.prod_nonneg fun i _ ↦ mul_nonneg (mul_pos hr (exp_pos _)).le ?_
  split_ifs <;> norm_num

private theorem scale_profile_large {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    Tendsto (fun s ↦ ∏ i, density rate μ s (x i)) atTop (𝓝 0) := by
  have ht (i : Fin n) : Tendsto (fun s ↦ density rate μ s (x i)) atTop (𝓝 0) := by
    have hu := tendsto_inv_atTop_zero.const_mul (φ 0)
    simp only [mul_zero] at hu
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hu
      (Eventually.of_forall fun s ↦ (density_positive (σ := s) hr μ (x i)).le) ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with s hs
    simpa only [div_eq_mul_inv] using (gaussian_peak_strict (μ := μ) (x := x i) hr hs).le
  simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun i _ ↦ ht i)

private theorem scale_profile_continuous {n : ℕ} (x : Fin n → ℝ) (rate μ : ℝ) :
    ContinuousOn (fun s ↦ ∏ i, density rate μ s (x i)) (Ioi 0) := by
  refine continuousOn_finsetProd Finset.univ fun i _ ↦ ?_
  unfold density
  have hd : ContinuousOn (fun s : ℝ ↦ (x i-μ)/s-rate*s) (Ioi 0) :=
    (ContinuousOn.div
      (show ContinuousOn (fun _ : ℝ ↦ x i-μ) (Ioi 0) from continuous_const.continuousOn)
      (show ContinuousOn (fun s : ℝ ↦ s) (Ioi 0) from continuous_id.continuousOn)
      (fun s hs ↦ (show 0 < s from hs).ne')).sub (by fun_prop)
  apply ContinuousOn.mul _ (cdf_continuous.comp_continuousOn hd)
  exact (by fun_prop)

/-- With rate and location held, Gaussian scale is attained exactly
when some finite positive scale reaches the exponential face trace. -/
theorem held_rate_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    (∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ i, density rate μ s (x i)) (Ioi 0) σ) ↔
      ∃ σ > 0, exponentialTrace x rate μ ≤ ∏ i, density rate μ σ (x i) := by
  have hlow : Tendsto (fun s ↦ ∏ i, density rate μ s (x i)) (𝓝[>] 0)
      (𝓝 (exponentialTrace x rate μ)) :=
    tendsto_finsetProd Finset.univ (fun i _ ↦ exponential_path rate μ (x i))
  simpa only [max_eq_left (exp_trace_nonneg x hr)] using
    scalar_face_iff (scale_profile_continuous x rate μ) hlow (scale_profile_large hn x hr)

/-- If the held rate is positive and the held location is strictly below
the sample, an interior scale exceeds the exponential face and a maximum exists. -/
theorem held_rate_location_below_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) (hx : ∀ i, μ < x i) :
    ∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ i, density rate μ s (x i)) (Ioi 0) σ := by
  have hb : ∀ᶠ t : ℝ in atTop,
      ∀ i, rate*exp (-rate*(x i-μ)) < density rate μ t⁻¹ (x i) := by
    rw [Filter.eventually_all]
    exact fun i ↦ eventually_above_exp_face hr (hx i)
  obtain ⟨t, ht, hbetter⟩ := ((eventually_gt_atTop (0 : ℝ)).and hb).exists
  apply (held_rate_location_criterion hn x hr).mpr
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  refine ⟨t⁻¹, inv_pos.mpr ht, ?_⟩
  unfold exponentialTrace
  simp only [hx, ite_true, mul_one]
  exact (Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ mul_pos hr (exp_pos _))
    (fun i _ ↦ hbetter i) Finset.univ_nonempty).le

/-- If an observation lies to the left of the held location, the
exponential face vanishes and a positive scale attains the maximum. -/
theorem held_rate_location_above_observation {n : ℕ} (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) {i : Fin n} (hi : x i < μ) :
    ∃ σ ∈ Ioi (0 : ℝ), IsMaxOn (fun s ↦ ∏ j, density rate μ s (x j)) (Ioi 0) σ := by
  apply (held_rate_location_criterion (Nat.zero_lt_of_lt i.isLt) x hr).mpr
  refine ⟨1, zero_lt_one, ?_⟩
  have he : exponentialTrace x rate μ = 0 := by
    unfold exponentialTrace
    exact Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi, not_lt_of_ge hi.le])
  rw [he]
  exact Finset.prod_nonneg fun j _ ↦ (density_positive hr μ (x j)).le

/-- With location and Gaussian scale held, a finite rate attains the
maximum exactly when it reaches the fixed normal face value. -/
theorem held_location_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    (∃ rate ∈ Ioi (0 : ℝ), IsMaxOn (fun r ↦ ∏ i, density r μ σ (x i)) (Ioi 0) rate) ↔
      ∃ rate > 0, (∏ i, φ ((x i-μ)/σ)/σ) ≤ ∏ i, density rate μ σ (x i) := by
  have hlow : Tendsto (fun r ↦ ∏ i, density r μ σ (x i)) (𝓝[>] 0) (𝓝 0) := by
    have hid : Tendsto (fun r : ℝ ↦ r) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    have ht (i : Fin n) : Tendsto (fun r ↦ density r μ σ (x i)) (𝓝[>] 0) (𝓝 0) := by
      have he : ∀ᶠ r : ℝ in 𝓝[>] 0, r ∈ Ioi 0 := self_mem_nhdsWithin
      exact tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hid
        (he.mono fun r hr ↦ (density_positive (σ := σ) hr μ (x i)).le)
        (he.mono fun _ hr ↦ (exponential_peak hr hσ).le)
    simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun i _ ↦ ht i)
  have hhigh := tendsto_finsetProd Finset.univ (fun i _ ↦ normal_path μ (x i) hσ)
  have hc : Continuous (fun r ↦ ∏ i, density r μ σ (x i)) := by
    refine continuous_finsetProd Finset.univ fun i _ ↦ ?_
    unfold density
    apply Continuous.mul _ (cdf_continuous.comp (by fun_prop))
    fun_prop
  have hpos : 0 ≤ ∏ i, φ ((x i-μ)/σ)/σ :=
    Finset.prod_nonneg fun _ _ ↦ div_nonneg (phi_pos _).le hσ.le
  simpa only [max_eq_right hpos] using scalar_face_iff hc.continuousOn hlow hhigh

private theorem scaled_mills_upper {y : ℝ} (hy : 0 < y) :
    weightedTail y ≤ φ 0/y := by
  have h := phi_add_mul_cdf_pos (-y)
  have hc : y*Φ (-y) ≤ φ (-y) := by linarith
  apply (le_div_iff₀ hy).mpr
  have hmul := mul_le_mul_of_nonneg_left hc (exp_pos (y^2/2)).le
  have he : exp (y^2/2)*φ (-y) = φ 0 := by
    simp only [phi_formula, neg_sq, zero_pow (by decide : (2 : ℕ) ≠ 0),
      zero_div, neg_zero, exp_zero, mul_one]
    calc
      _ = (1/sqrt (2*π))*(exp (y^2/2)*exp (-(y^2/2))) := by ring
      _ = _ := by rw [← exp_add, add_neg_cancel, exp_zero, mul_one]
  rw [he] at hmul
  unfold weightedTail
  nlinarith only [hmul]

private theorem density_location_bound {rate μ σ x : ℝ}
    (hr : 0 < rate) (hσ : 0 < σ) (hx : σ ≤ |x - μ|) :
    density rate μ σ x ≤ (2+4*φ 0)/|x-μ| := by
  let r := x-μ
  let z := r/σ
  let k := rate*σ
  let C := φ 0
  have hC : 0 < C := phi_pos _
  have hk : 0 < k := mul_pos hr hσ
  have hzr : z*σ = r := div_mul_cancel₀ _ hσ.ne'
  have hzr' : |z| *σ = |r| := by rw [← abs_of_pos hσ, ← abs_mul, hzr]
  have hzabs : 1 ≤ |z| := by
    rw [abs_div, abs_of_pos hσ]
    exact (le_div_iff₀ hσ).mpr (by simpa only [one_mul] using hx)
  have hlarge {v : ℝ} (hv : 0 < v) :
      exp (-(v^2/2)) ≤ 2/v^2 := by
    rw [exp_neg]
    have hn := inv_anti₀ (show 0 < v^2/2 by positivity)
      (show v^2/2 ≤ exp (v^2/2) by linarith [add_one_le_exp (v^2/2)])
    simpa only [inv_div] using hn
  by_cases hz : z ≤ 0
  · have hμ : x ≤ μ := by
      apply sub_nonpos.mp
      change r ≤ 0
      rw [← hzr]
      exact mul_nonpos_of_nonpos_of_nonneg hz hσ.le
    have hv : 0 < |z| := zero_lt_one.trans_le hzabs
    have hc : φ z = C*exp (-(|z|^2/2)) := by
      dsimp [C]
      simp [phi_formula, sq_abs]
    refine (normal_dominates_at_held_location hr hσ hμ).le.trans ?_
    change φ z/σ ≤ (2+4*C)/|r|
    rw [hc, ← hzr']
    apply (div_le_div_iff₀ hσ (mul_pos hv hσ)).mpr
    have hmid : exp (-(|z|^2/2))* |z| ≤ 2 := by
      have h := (le_div_iff₀ (sq_pos_of_pos hv)).mp (hlarge hv)
      have he := mul_le_mul_of_nonneg_left hzabs
        (show 0 ≤ exp (-(|z|^2/2))* |z| by positivity)
      nlinarith only [he, h]
    have htotal : C*exp (-(|z|^2/2))* |z| ≤ 2+4*C := by
      calc
        _ = C*(exp (-(|z|^2/2))* |z|) := by ring
        _ ≤ C*2 := mul_le_mul_of_nonneg_left hmid hC.le
        _ ≤ _ := by linarith
    nlinarith only [mul_le_mul_of_nonneg_right htotal hσ.le]
  · have hzpos : 0 < z := lt_of_not_ge hz
    have hz1 : 1 ≤ z := by simpa only [abs_of_pos hzpos] using hzabs
    have hrpos : 0 < r := by rw [← hzr]; positivity
    have htarget : (2 : ℝ)/r ≤ (2+4*C)/|r| := by
      rw [abs_of_pos hrpos]
      exact div_le_div_of_nonneg_right (by linarith) hrpos.le
    by_cases hkz : k ≤ z
    · have he : (rate*σ)^2/2 - rate*(x-μ) ≤ -(k*z/2) := by
        change k^2/2 - rate*r ≤ _
        rw [← hzr]
        have h := mul_le_mul_of_nonneg_left hkz hk.le
        dsimp [k] at h ⊢
        nlinarith only [h]
      have ht : (k*z/2)*exp (-(k*z/2)) ≤ 1 := by
        have h := mul_le_mul_of_nonneg_right (add_one_le_exp (k*z/2))
          (exp_pos (-(k*z/2))).le
        rw [← exp_add, add_neg_cancel, exp_zero] at h
        nlinarith [exp_pos (-(k*z/2))]
      refine le_trans ?_ htarget
      apply (le_div_iff₀ hrpos).mpr
      have hf : density rate μ σ x ≤ rate*exp (-(k*z/2)) := by
        unfold density
        exact (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (cdf_lt_one _).le).trans
          (mul_le_mul_of_nonneg_left (exp_le_exp.mpr he) hr.le)
      have hl := mul_le_mul_of_nonneg_right hf hrpos.le
      have hid : rate*exp (-(k*z/2))*r = 2*((k*z/2)*exp (-(k*z/2))) := by
        rw [← hzr]
        dsimp [k]
        ring
      rw [hid] at hl
      linarith only [hl, ht]
    · by_cases h2 : k ≤ 2*z
      · have htail : weightedTail (k-z) ≤ 1/2 := by
          have h := weightedTail_strictAnti.antitone (show 0 ≤ k-z by linarith)
          simpa [weightedTail, cdf_zero] using h
        have hsmall : density rate μ σ x ≤ rate*exp (-(z^2/2))*(1/2) := by
          rw [density_weightedTail rate μ x hσ, neg_div]
          exact mul_le_mul_of_nonneg_left htail (mul_pos hr (exp_pos _)).le
        refine le_trans ?_ htarget
        apply (le_div_iff₀ hrpos).mpr
        have hp := mul_le_mul_of_nonneg_right hsmall hrpos.le
        rw [← hzr] at hp
        have he : rate*exp (-(z^2/2))*(1/2)*(z*σ) = (k/2)*z*exp (-(z^2/2)) := by
          dsimp [k]
          ring
        rw [he] at hp
        have hh := mul_le_mul_of_nonneg_right (show k/2 ≤ z by linarith)
          (show 0 ≤ z*exp (-(z^2/2)) by positivity)
        have hz2 := (le_div_iff₀ (sq_pos_of_pos hzpos)).mp (hlarge hzpos)
        rw [← hzr]
        nlinarith only [hp, hh, hz2]
      · have hy : 0 < k-z := by linarith [lt_of_not_ge h2]
        have hkfrac : rate/(k-z) ≤ 2/σ := by
          apply (div_le_div_iff₀ hy hσ).mpr
          change k ≤ 2*(k-z)
          linarith [lt_of_not_ge h2]
        have he : density rate μ σ x ≤ (2/σ)*C*exp (-(z^2/2)) := by
          rw [density_weightedTail rate μ x hσ, neg_div]
          calc
            _ ≤ rate*exp (-(z^2/2))*(C/(k-z)) :=
              mul_le_mul_of_nonneg_left (scaled_mills_upper hy) (mul_pos hr (exp_pos _)).le
            _ = (rate/(k-z))*C*exp (-(z^2/2)) := by ring
            _ ≤ _ := mul_le_mul_of_nonneg_right
              (mul_le_mul_of_nonneg_right hkfrac hC.le) (exp_pos _).le
        rw [abs_of_pos hrpos]
        apply (le_div_iff₀ hrpos).mpr
        have hp := mul_le_mul_of_nonneg_right he hrpos.le
        have hmid : exp (-(z^2/2))*z ≤ 2 := by
          have h := (le_div_iff₀ (sq_pos_of_pos hzpos)).mp (hlarge hzpos)
          have ht := mul_le_mul_of_nonneg_left hz1 (show 0 ≤ exp (-(z^2/2))*z by positivity)
          nlinarith only [h, ht]
        have heq : (2/σ)*C*exp (-(z^2/2))*r = 2*C*(exp (-(z^2/2))*z) := by
          rw [← hzr]
          field_simp
        rw [heq] at hp
        nlinarith only [hp, mul_le_mul_of_nonneg_left hmid (show 0 ≤ 2*C by positivity)]

/-- Gaussian sample likelihood at the held scale and a proposed location. -/
def normalTrace {n : ℕ} (x : Fin n → ℝ) (μ σ : ℝ) : ℝ :=
  ∏ i, φ ((x i-μ)/σ)/σ

private theorem gaussian_relative_bound {rate μ σ v ε Z : ℝ}
    (hr : 0 < rate) (hσ : 0 < σ) (hε : 0 < ε)
    (hz : (v - μ) / σ ≤ Z) (hR : (1 + ε) * Z < ε * (rate * σ)) :
    density rate μ σ v ≤ (1+ε)*(φ ((v-μ)/σ)/σ) := by
  let k := rate*σ
  let z := (v-μ)/σ
  change (1+ε)*Z < ε*k at hR
  have h : (1+ε)*z < ε*k := (mul_le_mul_of_nonneg_left hz (by positivity)).trans_lt hR
  have hk : 0 < k := mul_pos hr hσ
  have hy : 0 < k-z := by
    by_contra! hn
    have h' := mul_le_mul_of_nonneg_left (show k ≤ z by linarith) (show 0 ≤ 1+ε by positivity)
    linarith only [h, h', hk]
  have hf : rate/(k-z) ≤ (1+ε)/σ := by
    apply (div_le_div_iff₀ hy hσ).mpr
    change k ≤ (1+ε)*(k-z)
    nlinarith only [h]
  have hφ : φ z = φ 0 * exp (-(z^2/2)) := by simp [phi_formula]
  rw [density_weightedTail rate μ v hσ, neg_div]
  change rate*exp (-(z^2/2))*weightedTail (k-z) ≤ (1+ε)*(φ z/σ)
  rw [hφ]
  calc
    _ ≤ rate*exp (-(z^2/2))*(φ 0/(k-z)) :=
      mul_le_mul_of_nonneg_left (scaled_mills_upper hy) (mul_pos hr (exp_pos _)).le
    _ = (rate/(k-z))*(φ 0*exp (-(z^2/2))) := by ring
    _ ≤ ((1+ε)/σ)*(φ 0*exp (-(z^2/2))) :=
      mul_le_mul_of_nonneg_right hf (mul_pos (phi_pos _) (exp_pos _)).le
    _ = _ := by ring

/-- With Gaussian scale held, a finite location and rate fit exists
exactly when some finite point reaches the optimized Gaussian face. -/
theorem held_scale_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ i, density p.2 p.1 σ (x i)) (univ ×ˢ Ioi 0) p) ↔
      ∃ μ rate : ℝ, 0 < rate ∧
        sSup (range (fun m ↦ normalTrace x m σ)) ≤ ∏ i, density rate μ σ (x i) := by
  let F (p : ℝ × ℝ) := ∏ i, density p.2 p.1 σ (x i)
  let N := sSup (range (fun m ↦ normalTrace x m σ))
  have hNbd : BddAbove (range (fun m ↦ normalTrace x m σ)) := by
    refine ⟨(φ 0/σ)^n, ?_⟩
    rintro _ ⟨m, rfl⟩
    unfold normalTrace
    have hb := Finset.prod_le_prod₀
      (f := fun i : Fin n ↦ φ ((x i-m)/σ)/σ) (g := fun _ ↦ φ 0/σ)
      (s := Finset.univ) (fun _ _ ↦ div_nonneg (phi_pos _).le hσ.le)
      (fun _ _ ↦ div_le_div_of_nonneg_right (phi_at_most_peak _) hσ.le)
    simpa only [Finset.prod_const, Finset.card_univ, Fintype.card_fin] using hb
  have hN (m : ℝ) : normalTrace x m σ ≤ N :=
    le_csSup hNbd (mem_range_self m)
  constructor
  · rintro ⟨p, hp, hm⟩
    refine ⟨p.1,p.2, hp.2, csSup_le (range_nonempty _) ?_⟩
    rintro _ ⟨m, rfl⟩
    exact le_of_tendsto (tendsto_finsetProd Finset.univ (fun i _ ↦ normal_path m (x i) hσ))
      ((eventually_gt_atTop (0 : ℝ)).mono
        (fun r hr ↦ isMaxOn_iff.mp hm (m,r) ⟨mem_univ _,hr⟩))
  · rintro ⟨m₀,r₀,hr₀,h₀⟩
    by_cases hb : ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)), N < F p
    · obtain ⟨p₁, hp₁, hbetter⟩ := hb
      let J := F p₁
      have hJ : 0 < J := Finset.prod_pos fun i _ ↦ density_positive hp₁.2 p₁.1 (x i)
      let e := min 1 J
      have he : 0 < e := lt_min zero_lt_one hJ
      have hePow : e^n ≤ J := by
        have h : e^n ≤ e := by
          obtain ⟨k, hk⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
          rw [hk, pow_succ]
          exact (mul_le_mul_of_nonneg_right (pow_le_one₀ he.le (min_le_left _ _)) he.le).trans_eq
            (one_mul e)
        exact h.trans (min_le_right _ _)
      let X := ∑ i, |x i|
      have hX : 0 ≤ X := Finset.sum_nonneg fun _ _ ↦ abs_nonneg _
      have hx (i : Fin n) : |x i| ≤ X :=
        Finset.single_le_sum (f := fun j ↦ |x j|) (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ i)
      let C := 2+4*φ 0
      have hC : 0 < C := by dsimp [C]; have := phi_pos 0; linarith
      let B := X + σ + C/e + |p₁.1|
      have hB : 0 < B := by dsimp [B]; positivity
      let Z := (X+B)/σ
      have hZ : 0 ≤ Z := by dsimp [Z]; positivity
      have hε : ∃ ε > 0, (1+ε)^n*N < J := by
        have hc : Tendsto (fun ε : ℝ ↦ (1+ε)^n*N) (𝓝[>] 0) (𝓝 N) := by
          have hf : Continuous (fun ε : ℝ ↦ (1+ε)^n*N) := by fun_prop
          have hbase : ContinuousAt (fun ε : ℝ ↦ (1+ε)^n*N) 0 := hf.continuousAt
          simpa using hbase.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
        have hd : ∀ᶠ ε : ℝ in 𝓝[>] 0, 0 < ε := self_mem_nhdsWithin
        exact (hd.and (hc.eventually (eventually_lt_nhds hbetter))).exists
      obtain ⟨ε, hε, hgap⟩ := hε
      let δ := min e p₁.2
      have hδ : 0 < δ := lt_min he hp₁.2
      let R := max (1 + (1+ε)*Z/(ε*σ)) p₁.2
      let K := Icc (-B) B ×ˢ Icc δ R
      have hK : K ⊆ univ ×ˢ Ioi 0 := fun _ hp ↦ ⟨mem_univ _, hδ.trans_le hp.2.1⟩
      have hK₁ : p₁ ∈ K := by
        refine ⟨abs_le.mp ?_, ⟨min_le_right _ _, le_max_right _ _⟩⟩
        dsimp [B]
        linarith [div_nonneg hC.le he.le]
      have hc : Continuous F := by
        refine continuous_finsetProd Finset.univ fun i _ ↦ ?_
        unfold density
        exact (by fun_prop : Continuous (fun p : ℝ × ℝ ↦
          p.2*exp ((p.2*σ)^2/2-p.2*(x i-p.1)))).mul
          (cdf_continuous.comp (by fun_prop))
      obtain ⟨p, hp, hm⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact K).exists_isMaxOn
        ⟨p₁,hK₁⟩ hc.continuousOn
      refine ⟨p, hK hp, fun q hq ↦ ?_⟩
      by_cases hqK : q ∈ K
      · exact hm hqK
      · have hcut : F q ≤ J := by
          have hrate : 0 < q.2 := hq.2
          have hsmall (hbnd : ∀ i, density q.2 q.1 σ (x i) ≤ e) : F q ≤ J := by
            have hh := Finset.prod_le_prod₀ (s := Finset.univ)
              (fun i _ ↦ (density_positive hrate q.1 (x i)).le) (fun i _ ↦ hbnd i)
            exact (show F q ≤ e^n by simpa using hh).trans hePow
          by_cases hBq : |q.1| ≤ B
          · by_cases hδq : δ ≤ q.2
            · have hqR : R < q.2 := lt_of_not_ge
                (fun h ↦ hqK ⟨abs_le.mp hBq, ⟨hδq,h⟩⟩)
              have hR : (1+ε)*Z < ε*(q.2*σ) := by
                have h : (1+ε)*Z/(ε*σ) < q.2 := by
                  have h' := (le_max_left _ p₁.2).trans_lt hqR
                  linarith
                have h' := (div_lt_iff₀ (mul_pos hε hσ)).mp h
                nlinarith only [h']
              have hpoint (i : Fin n) : density q.2 q.1 σ (x i) ≤
                  (1+ε)*(φ ((x i-q.1)/σ)/σ) := by
                apply gaussian_relative_bound hrate hσ hε _ hR
                apply (div_le_div_iff_of_pos_right hσ).mpr
                linarith [le_abs_self (x i), neg_le_abs q.1, hx i]
              have hp := Finset.prod_le_prod₀ (s := Finset.univ)
                (fun i _ ↦ (density_positive hrate q.1 (x i)).le) (fun i _ ↦ hpoint i)
              have hh : F q ≤ (1+ε)^n * normalTrace x q.1 σ := by
                simpa [Finset.prod_mul_distrib, normalTrace] using hp
              exact (hh.trans (mul_le_mul_of_nonneg_left (hN q.1) (by positivity))).trans hgap.le
            · apply hsmall
              intro i
              exact (exponential_peak (μ := q.1) (x := x i) hrate hσ).le.trans
                ((le_of_not_ge hδq).trans (min_le_left _ _))
          · apply hsmall
            intro i
            have htri : |q.1| ≤ |x i-q.1| + |x i| := by
              simpa [abs_sub_comm q.1 (x i)] using abs_sub_le q.1 (x i) 0
            have hb' : σ + C/e ≤ |x i-q.1| := by
              have hh : B < |q.1| := lt_of_not_ge hBq
              dsimp [B] at hh
              linarith [hx i, abs_nonneg p₁.1]
            have hpos : 0 < |x i-q.1| := by
              linarith [div_nonneg hC.le he.le]
            refine (density_location_bound hrate hσ (by linarith [div_nonneg hC.le he.le])).trans ?_
            apply (div_le_iff₀ hpos).mpr
            have hh := (div_le_iff₀ he).mp (show C/e ≤ |x i-q.1| by linarith)
            nlinarith only [hh]
        exact hcut.trans (isMaxOn_iff.mp hm p₁ hK₁)
    · have hall : ∀ q ∈ (univ ×ˢ Ioi (0 : ℝ)), F q ≤ N :=
        fun q hq ↦ le_of_not_gt (fun h ↦ hb ⟨q,hq,h⟩)
      exact ⟨(m₀,r₀), ⟨mem_univ _,hr₀⟩, fun q hq ↦ (hall q hq).trans h₀⟩

private theorem exponential_moving_path {rate c x : ℝ} (hc : c ≤ x) :
    Tendsto (fun t : ℝ ↦ density rate (c-t⁻¹) ((t⁻¹)^2) x) atTop
      (𝓝 (rate*exp (-rate*(x-c)))) := by
  have hs := (tendsto_inv_atTop_zero (𝕜 := ℝ)).pow 2
  have hp : Tendsto (fun t : ℝ ↦ (rate*(t⁻¹)^2)^2/2 - rate*(x-c+t⁻¹)) atTop
      (𝓝 (-rate*(x-c))) := by
    have ht := ((tendsto_inv_atTop_zero (𝕜 := ℝ)).const_add (x-c)).const_mul rate
    simpa [neg_mul] using (((hs.const_mul rate).pow 2).div_const 2).sub ht
  have hu : Tendsto (fun t : ℝ ↦ t-rate*(t⁻¹)^2) atTop atTop := by
    simpa only [sub_eq_add_neg, id_eq] using tendsto_id.atTop_add (hs.const_mul rate).neg
  have hd : Tendsto (fun t : ℝ ↦ (x-(c-t⁻¹))/(t⁻¹)^2-rate*(t⁻¹)^2) atTop atTop := by
    refine tendsto_atTop_mono' _ ?_ hu
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    have he : ((x-(c-t⁻¹))/(t⁻¹)^2) = (x-c)*t^2+t := by
      field_simp [ht.ne']
      ring
    rw [he]
    linarith [mul_nonneg (sub_nonneg.mpr hc) (sq_nonneg t)]
  have ht := ((continuous_exp.continuousAt.tendsto.comp hp).const_mul rate).mul (cdf_atTop.comp hd)
  simp only [mul_one] at ht
  refine ht.congr fun t ↦ ?_
  simp only [density, Function.comp_apply, show x-(c-t⁻¹)=x-c+t⁻¹ by ring]

/-- At held positive rate and free location and scale, the translated
exponential face is attained as a limit at the sample minimum. -/
theorem moving_exponential_face {n : ℕ} (x : Fin n → ℝ) {c : ℝ}
    (hc : ∀ i, c ≤ x i) (rate : ℝ) :
    Tendsto (fun t : ℝ ↦ ∏ i, density rate (c-t⁻¹) ((t⁻¹)^2) (x i)) atTop
      (𝓝 (∏ i, rate*exp (-rate*(x i-c)))) :=
  tendsto_finsetProd Finset.univ (fun i _ ↦ exponential_moving_path (rate := rate) (hc i))

private theorem scale_left_far_endpoint {rate c ε : ℝ} (hr : 0 < rate) (he : 0 < ε) :
    ∀ μ s : ℝ, c+ε ≤ μ → 0 < s →
      density rate μ s c ≤ (2*φ 0/ε^2)*s := by
  intro μ s hμ hs
  have hr' : c ≤ μ := by linarith
  have hsq : (ε/s)^2 ≤ ((c-μ)/s)^2 := by
    have h := (div_le_div_iff_of_pos_right hs).mpr (show ε ≤ μ-c by linarith)
    have hh := pow_le_pow_left₀ (show 0 ≤ ε/s by positivity) h 2
    rw [show (μ-c)/s = -((c-μ)/s) by ring, neg_sq] at hh
    exact hh
  have hφ : φ ((c-μ)/s) ≤ φ 0*exp (-((ε/s)^2/2)) := by
    simp only [phi_formula, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, neg_zero,
      exp_zero, mul_one]
    exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr (by linarith)) (by positivity)
  have hE : exp (-((ε/s)^2/2)) ≤ 2/(ε/s)^2 := by
    rw [exp_neg]
    have h := inv_anti₀ (show 0 < (ε/s)^2/2 by positivity)
      (show (ε/s)^2/2 ≤ exp ((ε/s)^2/2) by linarith [add_one_le_exp ((ε/s)^2/2)])
    simpa only [inv_div] using h
  calc
    _ ≤ φ ((c-μ)/s)/s := (normal_dominates_at_held_location hr hs hr').le
    _ ≤ (φ 0*(2/(ε/s)^2))/s := div_le_div_of_nonneg_right
      (hφ.trans (mul_le_mul_of_nonneg_left hE (phi_pos _).le)) hs.le
    _ = _ := by field_simp [he.ne', hs.ne']

private theorem exponential_below_width_bound {rate μ c s ε v : ℝ}
    (hr : 0 < rate) (hμ : μ ≤ c + ε) :
    density rate μ s v ≤ (rate*exp (-rate*(v-c)))*exp ((rate*s)^2/2+rate*ε) := by
  unfold density
  calc
    _ ≤ rate*exp ((rate*s)^2/2-rate*(v-μ)) :=
      mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (cdf_lt_one _).le
    _ ≤ rate*exp (-rate*(v-c)+((rate*s)^2/2+rate*ε)) := by
      apply mul_le_mul_of_nonneg_left _ hr.le
      apply exp_le_exp.mpr
      nlinarith only [mul_le_mul_of_nonneg_left hμ hr.le]
    _ = _ := by rw [exp_add, mul_assoc]

/-- At held positive rate, a finite fit exists exactly when a finite
location and scale reach the optimized translated-exponential face. -/
theorem held_rate_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {rate : ℝ} (hr : 0 < rate) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ j, density rate p.1 p.2 (x j)) (univ ×ˢ Ioi 0) p) ↔
      ∃ μ s : ℝ, 0 < s ∧ (∏ j, rate*exp (-rate*(x j-x i))) ≤
        ∏ j, density rate μ s (x j) := by
  let c := x i
  let E := ∏ j, rate*exp (-rate*(x j-c))
  let F (p : ℝ × ℝ) := ∏ j, density rate p.1 p.2 (x j)
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  have hE : 0 < E := Finset.prod_pos (fun _ _ ↦ mul_pos hr (exp_pos _))
  constructor
  · rintro ⟨p, hp, hm⟩
    refine ⟨p.1,p.2,hp.2,le_of_tendsto (moving_exponential_face x hi rate) ?_⟩
    exact (eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦
      isMaxOn_iff.mp hm (c-t⁻¹,(t⁻¹)^2) ⟨mem_univ _,pow_pos (inv_pos.mpr ht) _⟩
  · rintro ⟨m₀,s₀,hs₀,h₀⟩
    by_cases hb : ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)), E < F p
    · obtain ⟨p₁,hp₁,hbetter⟩ := hb
      let J := F p₁
      have hJ : 0 < J := hE.trans hbetter
      let e := min 1 J
      have he : 0 < e := lt_min zero_lt_one hJ
      have hePow : e^n ≤ J := by
        have h : e^n ≤ e := by
          obtain ⟨k,hk⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
          rw [hk,pow_succ]
          exact (mul_le_mul_of_nonneg_right (pow_le_one₀ he.le (min_le_left _ _)) he.le).trans_eq
            (one_mul e)
        exact h.trans (min_le_right _ _)
      obtain ⟨ε,hε,hclose⟩ : ∃ ε > 0, E*exp ((n : ℝ)*(rate*ε)) < J := by
        have hc : Tendsto (fun ε : ℝ ↦ E*exp ((n : ℝ)*(rate*ε))) (𝓝[>] 0) (𝓝 E) := by
          have hbase : ContinuousAt (fun ε : ℝ ↦ E*exp ((n : ℝ)*(rate*ε))) 0 := by fun_prop
          simpa using hbase.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
        have hevt : ∀ᶠ ε : ℝ in 𝓝[>] 0, 0 < ε := self_mem_nhdsWithin
        exact (hevt.and (hc.eventually (eventually_lt_nhds hbetter))).exists
      have hsmall : ∀ᶠ s : ℝ in 𝓝[>] 0, E*exp ((n : ℝ)*((rate*s)^2/2+rate*ε)) < J := by
        have hc : ContinuousAt (fun s : ℝ ↦ E*exp ((n : ℝ)*((rate*s)^2/2+rate*ε))) 0 := by
          fun_prop
        have ht := hc.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
        simp only [mul_zero, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, zero_add] at ht
        exact ht.eventually (eventually_lt_nhds hclose)
      obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp hsmall
      let C := 2+4*φ 0
      have hC : 0 < C := by dsimp [C]; linarith [phi_pos 0]
      let δ := min (min (a/2) (J/(rate^(n-1)*(2*φ 0/ε^2)))) p₁.2
      have hδ : 0 < δ :=
        lt_min (lt_min (half_pos ha) (div_pos hJ
          (mul_pos (pow_pos hr _) (div_pos (mul_pos (by norm_num) (phi_pos 0))
            (sq_pos_of_pos hε))))) hp₁.2
      let S := max (φ 0/e) p₁.2
      have hS : 0 < S := hp₁.2.trans_le (le_max_right _ _)
      let X := ∑ j, |x j|
      have hX : 0 ≤ X := Finset.sum_nonneg (fun _ _ ↦ abs_nonneg _)
      let B := X+S+C/e+|p₁.1|
      have hB : 0 < B := by dsimp [B]; positivity
      let K := Icc (-B) B ×ˢ Icc δ S
      have hK : K ⊆ univ ×ˢ Ioi 0 := fun _ hp ↦ ⟨mem_univ _,hδ.trans_le hp.2.1⟩
      have h₁ : p₁ ∈ K := by
        refine ⟨abs_le.mp ?_, ⟨min_le_right _ _, le_max_right _ _⟩⟩
        dsimp [B]
        linarith [div_nonneg hC.le he.le]
      have hc : ContinuousOn F (univ ×ˢ Ioi 0) := by
        refine continuousOn_finsetProd Finset.univ fun j _ ↦ ?_
        unfold density
        refine ContinuousOn.mul (by fun_prop) (cdf_continuous.comp_continuousOn ?_)
        refine ContinuousOn.sub (ContinuousOn.div (by fun_prop) continuous_snd.continuousOn
          (fun p hp ↦ (show 0 < p.2 from hp.2).ne')) (by fun_prop)
      obtain ⟨p,hp,hm⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact K).exists_isMaxOn
        ⟨p₁,h₁⟩ (hc.mono hK)
      refine ⟨p,hK hp,fun q hq ↦ ?_⟩
      by_cases hqK : q ∈ K
      · exact hm hqK
      · have hscale : 0 < q.2 := hq.2
        have hall (hq' : ∀ j, density rate q.1 q.2 (x j) ≤ e) : F q ≤ J := by
          have ht := Finset.prod_le_prod₀ (s := Finset.univ)
            (fun j _ ↦ (density_positive hr q.1 (x j)).le) (fun j _ ↦ hq' j)
          exact (show F q ≤ e^n by simpa using ht).trans hePow
        have hcut : F q ≤ J := by
          by_cases hS' : q.2 ≤ S
          · by_cases hδ' : δ ≤ q.2
            · have hfar : B < |q.1| := lt_of_not_ge
                (fun h ↦ hqK ⟨abs_le.mp h,⟨hδ',hS'⟩⟩)
              apply hall
              intro j
              have hx : |x j| ≤ X := Finset.single_le_sum
                (f := fun j ↦ |x j|) (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ j)
              have htri : |q.1| ≤ |x j-q.1|+|x j| := by
                simpa [abs_sub_comm q.1 (x j)] using abs_sub_le q.1 (x j) 0
              have hfar' : S+C/e ≤ |x j-q.1| := by
                dsimp [B] at hfar
                linarith [abs_nonneg p₁.1]
              have hpos : 0 < |x j-q.1| := by linarith [div_nonneg hC.le he.le]
              refine (density_location_bound hr hscale
                (by linarith [div_nonneg hC.le he.le])).trans ?_
              apply (div_le_iff₀ hpos).mpr
              have hh := (div_le_iff₀ he).mp (show C/e ≤ |x j-q.1| by linarith)
              nlinarith only [hh]
            · have hsmall' : q.2 < δ := lt_of_not_ge hδ'
              by_cases hloc : q.1 ≤ c+ε
              · have ht : E*exp ((n : ℝ)*((rate*q.2)^2/2+rate*ε)) < J := by
                  apply ha'
                  refine ⟨?_,hq.2⟩
                  rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hscale]
                  have hδa : δ ≤ a/2 := (min_le_left _ _).trans (min_le_left _ _)
                  exact (hsmall'.trans_le hδa).trans (by linarith)
                apply le_trans _ ht.le
                have hh := Finset.prod_le_prod₀ (s := Finset.univ)
                  (fun j _ ↦ (density_positive hr q.1 (x j)).le)
                  (fun j _ ↦ exponential_below_width_bound (s := q.2) (v := x j) hr hloc)
                simpa only [F,E,Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,
                  Fintype.card_fin,exp_nat_mul] using hh
              · have ht := scale_left_far_endpoint hr hε q.1 q.2 (le_of_not_ge hloc) hscale
                have hh : q.2 ≤ J/(rate^(n-1)*(2*φ 0/ε^2)) :=
                  hsmall'.le.trans ((min_le_left _ _).trans (min_le_right _ _))
                classical
                let f (j : Fin n) := density rate q.1 q.2 (x j)
                change ∏ j, f j ≤ J
                rw [← Finset.prod_erase_mul Finset.univ f (Finset.mem_univ i)]
                have hprod : (∏ j ∈ Finset.univ.erase i, f j) ≤ rate^(n-1) := by
                  have h := Finset.prod_le_prod₀ (s := Finset.univ.erase i)
                    (fun j _ ↦ (density_positive hr q.1 (x j)).le)
                    (fun j _ ↦ (exponential_peak (μ := q.1) (x := x j) hr hscale).le)
                  simpa using h
                calc
                  _ ≤ rate^(n-1) * f i := mul_le_mul_of_nonneg_right hprod
                    (density_positive hr q.1 (x i)).le
                  _ ≤ rate^(n-1)*((2*φ 0/ε^2)*q.2) :=
                    mul_le_mul_of_nonneg_left ht (by positivity)
                  _ ≤ J := by
                    have hpos : 0 < rate^(n-1)*(2*φ 0/ε^2) := by have := phi_pos 0; positivity
                    nlinarith only [(le_div_iff₀ hpos).mp hh]
          · apply hall
            intro j
            refine (gaussian_peak_strict (μ := q.1) (x := x j) hr hscale).le.trans ?_
            apply (div_le_iff₀ hscale).mpr
            have h := (div_le_iff₀ he).mp ((le_max_left _ p₁.2).trans (le_of_not_ge hS'))
            simpa only [mul_comm] using h
        exact hcut.trans (isMaxOn_iff.mp hm p₁ h₁)
    · have hall : ∀ q ∈ (univ ×ˢ Ioi (0 : ℝ)), F q ≤ E :=
        fun q hq ↦ le_of_not_gt (fun h ↦ hb ⟨q,hq,h⟩)
      exact ⟨(m₀,s₀),⟨mem_univ _,hs₀⟩,fun q hq ↦ (hall q hq).trans h₀⟩

private theorem product_bound_one {n : ℕ} (f : Fin n → ℝ) (M : ℝ)
    (hpos : ∀ i, 0 ≤ f i) (hM : ∀ i, f i ≤ M) (i : Fin n) :
    (∏ j, f j) ≤ M^(n-1)*f i := by
  classical
  rw [← Finset.prod_erase_mul Finset.univ f (Finset.mem_univ i)]
  have h := Finset.prod_le_prod₀ (s := Finset.univ.erase i)
    (fun j _ ↦ hpos j) (fun j _ ↦ hM j)
  exact mul_le_mul_of_nonneg_right (by simpa using h) (hpos i)

private theorem exp_power_tail {d : ℝ} (hd : 0 < d) (n : ℕ) :
    Tendsto (fun r : ℝ ↦ r^n*exp (-(r*d))) atTop (𝓝 0) := by
  have ht := (tendsto_pow_mul_exp_neg_atTop_nhds_zero n).comp
    (tendsto_id.atTop_mul_const hd)
  have h := ht.div_const (d^n)
  simp only [zero_div] at h
  refine h.congr fun r ↦ ?_
  change (r*d)^n*exp (-(r*d))/d^n = r^n*exp (-(r*d))
  rw [mul_pow]
  field_simp

private theorem gauss_power_tail {d : ℝ} (hd : 0 < d) (n : ℕ) :
    Tendsto (fun s : ℝ ↦ (s⁻¹)^n*exp (-((d/s)^2/2))) (𝓝[>] 0) (𝓝 0) := by
  have ht : Tendsto (fun w : ℝ ↦ w^n*exp (-(d^2*w^2/2))) atTop (𝓝 0) := by
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
      (tendsto_pow_mul_exp_neg_atTop_nhds_zero n)
    · exact (eventually_ge_atTop (0 : ℝ)).mono (fun w hw ↦
        mul_nonneg (pow_nonneg hw _) (exp_pos _).le)
    · filter_upwards [eventually_ge_atTop (2/d^2), eventually_ge_atTop (0 : ℝ)] with w hw hw0
      apply mul_le_mul_of_nonneg_left _ (pow_nonneg hw0 _)
      apply exp_le_exp.mpr
      have h := (div_le_iff₀ (sq_pos_of_pos hd)).mp hw
      nlinarith only [mul_le_mul_of_nonneg_left h hw0]
  have h := ht.comp tendsto_inv_nhdsGT_zero
  convert h using 1
  ext s
  simp only [Function.comp_apply, div_eq_mul_inv, mul_pow, inv_pow]

private theorem normalTrace_scale_bounds {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    Tendsto (normalTrace x μ) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (normalTrace x μ) atTop (𝓝 0) ∧
      BddAbove (normalTrace x μ '' Ioi 0) := by
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  let d := |x i-μ|
  have hd : 0 < d := abs_pos.mpr (sub_ne_zero.mpr hi)
  have hsmall : Tendsto (normalTrace x μ) (𝓝[>] 0) (𝓝 0) := by
    have ht := (gauss_power_tail hd n).const_mul ((φ 0)^n)
    simp only [mul_zero] at ht
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht ?_ ?_
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
      exact Finset.prod_nonneg fun _ _ ↦ div_nonneg (phi_pos _).le hs.le
    · filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s (hs : 0 < s)
      have hbound := product_bound_one (fun j ↦ φ ((x j-μ)/s)/s) (φ 0/s)
        (fun _ ↦ div_nonneg (phi_pos _).le hs.le)
        (fun _ ↦ div_le_div_of_nonneg_right (phi_at_most_peak _) hs.le) i
      refine hbound.trans_eq ?_
      have hφ : φ ((x i-μ)/s) = φ 0 * exp (-((d/s)^2/2)) := by
        simp only [phi_formula, d, div_pow, sq_abs, zero_pow (by decide : (2 : ℕ) ≠ 0),
          zero_div, neg_zero, exp_zero, mul_one]
      rw [hφ, div_eq_mul_inv (φ 0), mul_pow]
      have hsucc : (n-1)+1 = n := Nat.sub_add_cancel hn
      rw [← hsucc]
      simp only [Nat.add_sub_cancel_right, pow_succ]
      ring
  have hlarge : Tendsto (normalTrace x μ) atTop (𝓝 0) := by
    have ht (j : Fin n) : Tendsto (fun s ↦ φ ((x j-μ)/s)/s) atTop (𝓝 0) := by
      have h := tendsto_inv_atTop_zero.const_mul (φ 0)
      simp only [mul_zero] at h
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds h ?_ ?_
      · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hs ↦ div_nonneg (phi_pos _).le hs.le
      · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hs ↦
          by simpa only [div_eq_mul_inv] using div_le_div_of_nonneg_right (phi_at_most_peak _) hs.le
    change Tendsto (fun s ↦ ∏ j, φ ((x j-μ)/s)/s) atTop (𝓝 0)
    simpa only [Finset.prod_const, Finset.card_univ, Fintype.card_fin, zero_pow hn.ne'] using
      tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)
  have hc : ContinuousOn (normalTrace x μ) (Ioi 0) := by
    unfold normalTrace
    refine continuousOn_finsetProd Finset.univ (fun j _ ↦ ?_)
    have hφ : Continuous φ := continuous_iff_continuousAt.mpr
      (fun t ↦ (phi_hasDerivAt t).continuousAt)
    have hs : ContinuousOn (fun s : ℝ ↦ s) (Ioi 0) := continuous_id.continuousOn
    exact (hφ.comp_continuousOn (continuous_const.continuousOn.div hs
      (fun s hs ↦ (show 0 < s from hs).ne'))).div hs (fun s hs ↦ (show 0 < s from hs).ne')
  obtain ⟨s,hs,hm⟩ := (scalar_face_iff hc hsmall hlarge).mpr ⟨1,zero_lt_one,by
    simp only [max_self]
    exact Finset.prod_nonneg fun _ _ ↦ div_nonneg (phi_pos _).le zero_le_one⟩
  exact ⟨hsmall,hlarge,hm.bddAbove⟩

private theorem exponential_trace_bounds {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    Tendsto (fun r ↦ exponentialTrace x r μ) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun r ↦ exponentialTrace x r μ) atTop (𝓝 0) ∧
      BddAbove ((fun r ↦ exponentialTrace x r μ) '' Ioi 0) := by
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  let w (j : Fin n) : ℝ := if μ < x j then 1 else if x j < μ then 0 else 1/2
  have hw (j : Fin n) : 0 ≤ w j ∧ w j ≤ 1 := by dsimp [w]; split_ifs <;> norm_num
  have hf (r : ℝ) : exponentialTrace x r μ = ∏ j, r*exp (-r*(x j-μ))*w j := rfl
  have hsmall : Tendsto (fun r ↦ exponentialTrace x r μ) (𝓝[>] 0) (𝓝 0) := by
    simp_rw [hf]
    have h (j : Fin n) : Tendsto (fun r ↦ r*exp (-r*(x j-μ))*w j) (𝓝[>] 0) (𝓝 0) := by
      have hcont : ContinuousAt (fun r : ℝ ↦ r*exp (-r*(x j-μ))*w j) 0 := by fun_prop
      simpa using hcont.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ h j)
  have hlarge : Tendsto (fun r ↦ exponentialTrace x r μ) atTop (𝓝 0) := by
    by_cases hside : ∃ j, x j < μ
    · obtain ⟨j,hj⟩ := hside
      have hz (r : ℝ) : exponentialTrace x r μ = 0 := by
        rw [hf]
        exact Finset.prod_eq_zero (Finset.mem_univ j)
          (by simp [w,hj,not_lt_of_ge hj.le])
      simp_rw [hz]
      exact tendsto_const_nhds
    · push Not at hside
      have hi' : μ < x i := lt_of_le_of_ne (hside i) hi.symm
      let d := x i-μ
      have hd : 0 < d := sub_pos.mpr hi'
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
        (exp_power_tail hd n) ?_ ?_
      · exact (eventually_gt_atTop (0 : ℝ)).mono (fun r hr ↦ exp_trace_nonneg x hr)
      · filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
        rw [hf]
        have hnonneg (j : Fin n) : 0 ≤ r*exp (-r*(x j-μ))*w j :=
          mul_nonneg (mul_pos hr (exp_pos _)).le (hw j).1
        have hpoint (j : Fin n) : r*exp (-r*(x j-μ))*w j ≤ r :=
          (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (hw j).2).trans
            (mul_le_of_le_one_right hr.le (exp_le_one_iff.mpr
              (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr hr.le) (sub_nonneg.mpr (hside j)))))
        calc
          _ ≤ r^(n-1)*(r*exp (-r*(x i-μ))*w i) := product_bound_one _ r hnonneg hpoint i
          _ ≤ r^(n-1)*(r*exp (-(r*d))) := by
            rw [show -r*(x i-μ) = -(r*d) by dsimp [d]; ring]
            exact mul_le_mul_of_nonneg_left
              (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (hw i).2) (by positivity)
          _ = _ := by rw [← mul_assoc, ← pow_succ, Nat.sub_add_cancel hn]
  have hc : Continuous (fun r ↦ exponentialTrace x r μ) := by
    simp_rw [hf]
    exact continuous_finsetProd Finset.univ (fun j _ ↦ by fun_prop)
  obtain ⟨r,hr,hm⟩ := (scalar_face_iff hc.continuousOn hsmall hlarge).mpr
    ⟨1,zero_lt_one,by simpa using exp_trace_nonneg x zero_lt_one⟩
  exact ⟨hsmall,hlarge,hm.bddAbove⟩

private theorem gauss_bound_if_left {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i < μ) {rate s : ℝ} (hr : 0 < rate) (hs : 0 < s) :
    (∏ j, density rate μ s (x j)) ≤
      (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x i)/s)^2/2)) := by
  have hb := product_bound_one (fun j ↦ density rate μ s (x j)) (φ 0/s)
    (fun _ ↦ (density_positive hr _ _).le) (fun _ ↦ (gaussian_peak_strict hr hs).le) i
  calc
    _ ≤ (φ 0/s)^(n-1)*(φ ((x i-μ)/s)/s) := hb.trans
      (mul_le_mul_of_nonneg_left (normal_dominates_at_held_location hr hs hi.le).le
        (by have := phi_pos 0; positivity))
    _ = _ := by
      have he : (x i-μ)/s = -((μ-x i)/s) := by ring
      have hφ : φ ((x i-μ)/s) = φ 0*exp (-(((μ-x i)/s)^2/2)) := by
        rw [he]
        simp [phi_formula]
      rw [hφ, div_eq_mul_inv (φ 0), mul_pow]
      simp only [div_eq_mul_inv]
      have hsucc : (n-1)+1 = n := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt)
      rw [← hsucc]
      simp only [Nat.add_sub_cancel_right,pow_succ]
      ring

private theorem joint_collapse_bound {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : μ < x i) {rate s : ℝ} (hr : 0 < rate) (hs : 0 < s) :
    let d := x i-μ
    (∏ j, density rate μ s (x j)) ≤
      max (rate^n*exp (-(rate*(d/2))))
        (max ((φ 0)^(n-1)*d*(s⁻¹)^(n+1)*exp (-((d/s)^2/2)))
             (2*(φ 0)^n*(s⁻¹)^n*exp (-((d/s)^2/2)))) := by
  intro d
  let z := d/s
  let k := rate*s
  have hd : 0 < d := sub_pos.mpr hi
  have hz : 0 < z := div_pos hd hs
  have hzs : z*s = d := div_mul_cancel₀ _ hs.ne'
  have hk : 0 < k := mul_pos hr hs
  have hC := phi_pos 0
  by_cases hle : k ≤ z
  · apply le_trans ?_ (le_max_left _ _)
    have harg : (rate*s)^2/2-rate*(x i-μ) ≤ -(rate*(d/2)) := by
      change k^2/2-rate*d ≤ _
      have h := mul_le_mul_of_nonneg_left hle hk.le
      rw [← hzs]
      dsimp [k] at h ⊢
      nlinarith only [h]
    have hf : density rate μ s (x i) ≤ rate*exp (-(rate*(d/2))) := by
      unfold density
      exact (mul_le_of_le_one_right (mul_pos hr (exp_pos _)).le (cdf_lt_one _).le).trans
        (mul_le_mul_of_nonneg_left (exp_le_exp.mpr harg) hr.le)
    have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
    calc
      _ ≤ rate^(n-1)*density rate μ s (x i) := product_bound_one _ rate
        (fun _ ↦ (density_positive hr _ _).le) (fun _ ↦ (exponential_peak hr hs).le) i
      _ ≤ rate^(n-1)*(rate*exp (-(rate*(d/2)))) :=
        mul_le_mul_of_nonneg_left hf (by positivity)
      _ = _ := by rw [← mul_assoc, ← pow_succ, Nat.sub_add_cancel hn]
  · have hy : 0 < k-z := sub_pos.mpr (lt_of_not_ge hle)
    have hb := product_bound_one (fun j ↦ density rate μ s (x j)) (φ 0/s)
      (fun _ ↦ (density_positive hr _ _).le) (fun _ ↦ (gaussian_peak_strict hr hs).le) i
    by_cases h2 : k ≤ 2*z
    · apply le_trans ?_ ((le_max_left _ _).trans (le_max_right _ _))
      have htail : weightedTail (k-z) ≤ 1/2 := by
        have h := weightedTail_strictAnti.antitone hy.le
        simpa [weightedTail,cdf_zero] using h
      have hrate : rate/2 ≤ z/s := by
        apply (le_div_iff₀ hs).mpr
        dsimp [k] at h2
        nlinarith only [h2]
      have hf : density rate μ s (x i) ≤ (z/s)*exp (-(z^2/2)) := by
        rw [density_weightedTail rate μ (x i) hs, neg_div]
        calc
          _ ≤ rate*exp (-(z^2/2))*(1/2) :=
            mul_le_mul_of_nonneg_left htail (mul_pos hr (exp_pos _)).le
          _ = (rate/2)*exp (-(z^2/2)) := by ring
          _ ≤ _ := mul_le_mul_of_nonneg_right hrate (exp_pos _).le
      calc
        _ ≤ (φ 0/s)^(n-1)*((z/s)*exp (-(z^2/2))) := hb.trans
          (mul_le_mul_of_nonneg_left hf (by positivity))
        _ = _ := by
          have he : (n-1)+2 = n+1 := by have := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt); omega
          rw [← he, pow_add]
          dsimp [z]
          simp only [div_eq_mul_inv,mul_pow]
          ring
    · apply le_trans ?_ ((le_max_right _ _).trans (le_max_right _ _))
      have hrate : rate/(k-z) ≤ 2/s := by
        apply (div_le_div_iff₀ hy hs).mpr
        change k ≤ 2*(k-z)
        linarith [lt_of_not_ge h2]
      have hf : density rate μ s (x i) ≤ (2/s)*φ 0*exp (-(z^2/2)) := by
        rw [density_weightedTail rate μ (x i) hs, neg_div]
        calc
          _ ≤ rate*exp (-(z^2/2))*(φ 0/(k-z)) :=
            mul_le_mul_of_nonneg_left (scaled_mills_upper hy) (mul_pos hr (exp_pos _)).le
          _ = (rate/(k-z))*φ 0*exp (-(z^2/2)) := by ring
          _ ≤ _ := mul_le_mul_of_nonneg_right
            (mul_le_mul_of_nonneg_right hrate hC.le) (exp_pos _).le
      calc
        _ ≤ (φ 0/s)^(n-1)*((2/s)*φ 0*exp (-(z^2/2))) :=
          hb.trans (mul_le_mul_of_nonneg_left hf (by positivity))
        _ = _ := by
          have hsucc : (n-1)+1 = n := Nat.sub_add_cancel (Nat.zero_lt_of_lt i.isLt)
          rw [← hsucc]
          dsimp [z]
          simp only [pow_succ,div_eq_mul_inv,mul_pow]
          ring

private theorem small_scale_large_rate_bound {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) {J : ℝ} (hJ : 0 < J) :
    ∃ a > 0, ∃ R > 0, ∀ s rate : ℝ, 0 < s → s ≤ a → R ≤ rate →
      (∏ j, density rate μ s (x j)) ≤ J := by
  rcases lt_or_gt_of_ne hi with hi | hi
  · have h : Tendsto (fun s : ℝ ↦ (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x i)/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero, mul_assoc] using
        (gauss_power_tail (sub_pos.mpr hi) n).const_mul ((φ 0)^n)
    obtain ⟨δ,hδ,hb⟩ := Metric.mem_nhdsWithin_iff.mp (h.eventually (eventually_lt_nhds hJ))
    refine ⟨δ/2,half_pos hδ,1,zero_lt_one,fun s rate hs hsa hr ↦ ?_⟩
    refine (gauss_bound_if_left x μ hi (zero_lt_one.trans_le hr) hs).trans ?_
    apply (hb ⟨?_,hs⟩).le
    rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
    linarith
  · let d := x i-μ
    have hd : 0 < d := sub_pos.mpr hi
    obtain ⟨R,hR⟩ := eventually_atTop.mp
      ((exp_power_tail (half_pos hd) n).eventually (eventually_lt_nhds hJ))
    have h₁ : Tendsto (fun s : ℝ ↦ (φ 0)^(n-1)*d*(s⁻¹)^(n+1)*exp (-((d/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using (gauss_power_tail hd (n+1)).const_mul ((φ 0)^(n-1)*d)
    have h₂ : Tendsto (fun s : ℝ ↦ 2*(φ 0)^n*(s⁻¹)^n*exp (-((d/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using (gauss_power_tail hd n).const_mul (2*(φ 0)^n)
    obtain ⟨δ,hδ,hb⟩ := Metric.mem_nhdsWithin_iff.mp
      ((h₁.eventually (eventually_lt_nhds hJ)).and (h₂.eventually (eventually_lt_nhds hJ)))
    refine ⟨δ/2, half_pos hδ, max 1 R, zero_lt_one.trans_le (le_max_left _ _),
      fun s rate hs hsa hr ↦ ?_⟩
    have hb' := hb ⟨by rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]; linarith,hs⟩
    exact (joint_collapse_bound x μ hi (zero_lt_one.trans_le ((le_max_left _ _).trans hr)) hs).trans
      (max_le (hR _ ((le_max_right _ _).trans hr)).le (max_le hb'.1.le hb'.2.le))

private theorem bounded_rate_small_scale {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {E J R : ℝ} (hE : 0 ≤ E) (hJE : E < J)
    (hbound : ∀ rate > 0, exponentialTrace x rate μ ≤ E) :
    ∃ a > 0, ∀ s rate : ℝ, 0 < s → s ≤ a → 0 < rate → rate ≤ R →
      (∏ j, density rate μ s (x j)) ≤ J := by
  by_cases hside : ∃ j, x j < μ
  · obtain ⟨j,hj⟩ := hside
    have ht : Tendsto (fun s : ℝ ↦ (φ 0)^n*(s⁻¹)^n*exp (-(((μ-x j)/s)^2/2)))
        (𝓝[>] 0) (𝓝 0) := by
      simpa only [mul_zero,mul_assoc] using
        (gauss_power_tail (sub_pos.mpr hj) n).const_mul ((φ 0)^n)
    obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp
      (ht.eventually (eventually_lt_nhds (hE.trans_lt hJE)))
    refine ⟨a/2, half_pos ha, fun s rate hs hs' hr _ ↦
      (gauss_bound_if_left x μ hj hr hs).trans ?_⟩
    apply (ha' ⟨?_,hs⟩).le
    rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]
    linarith
  · push Not at hside
    have ht : Tendsto (fun s : ℝ ↦ E*exp ((n : ℝ)*((R*s)^2/2))) (𝓝[>] 0) (𝓝 E) := by
      have h : ContinuousAt (fun s : ℝ ↦ E*exp ((n : ℝ)*((R*s)^2/2))) 0 := by fun_prop
      simpa using h.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    obtain ⟨a,ha,ha'⟩ := Metric.mem_nhdsWithin_iff.mp (ht.eventually (eventually_lt_nhds hJE))
    refine ⟨a/2,half_pos ha,fun s rate hs hsa hr hrR ↦ ?_⟩
    let D := exp ((rate*s)^2/2)
    let f (j : Fin n) := rate*exp (-rate*(x j-μ))*(if μ < x j then 1 else 1/2)
    have htrace : (∏ j, f j) = exponentialTrace x rate μ := by
      simp only [f,exponentialTrace,not_lt_of_ge (hside _),ite_false]
    have hterm (j : Fin n) : density rate μ s (x j) ≤ f j*D := by
      unfold density
      have hΦ : Φ ((x j-μ)/s-rate*s) ≤ (if μ < x j then 1 else 1/2) := by
        split_ifs with hj
        · exact (cdf_lt_one _).le
        · have he : x j=μ := le_antisymm (le_of_not_gt hj) (hside j)
          rw [he,sub_self,zero_div,zero_sub,← cdf_zero]
          exact cdf_strictMono.monotone (neg_nonpos.mpr (mul_pos hr hs).le)
      have he : f j*D =
          rate * exp ((rate*s)^2/2-rate*(x j-μ))*(if μ<x j then 1 else 1/2) := by
        rw [sub_eq_add_neg,exp_add]
        unfold f D
        ring_nf
      rw [he]
      exact mul_le_mul_of_nonneg_left hΦ (mul_pos hr (exp_pos _)).le
    have hprod := Finset.prod_le_prod₀ (s := Finset.univ)
      (fun j _ ↦ (density_positive hr μ (x j)).le) (fun j _ ↦ hterm j)
    rw [Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,Fintype.card_fin,htrace] at hprod
    have hpow : D^n ≤ exp ((n : ℝ)*((R*s)^2/2)) := by
      change exp ((rate*s)^2/2)^n ≤ _
      rw [← exp_nat_mul]
      apply exp_le_exp.mpr
      have h := pow_le_pow_left₀ (mul_pos hr hs).le (mul_le_mul_of_nonneg_right hrR hs.le) 2
      exact mul_le_mul_of_nonneg_left (div_le_div_of_nonneg_right h (by norm_num)) (by positivity)
    have hb := mul_le_mul (hbound rate hr) hpow (by unfold D; positivity) hE
    exact (hprod.trans hb).trans (ha' ⟨by
      rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos hs]; linarith,hs⟩).le

/-- For a held location with a nonzero residual, finite attainment is
equivalent to a finite point reaching both optimized conditional faces. -/
theorem held_location_criterion {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {i : Fin n} (hi : x i ≠ μ) :
    (∃ p ∈ (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)) (Ioi 0 ×ˢ Ioi 0) p) ↔
      ∃ rate s : ℝ, 0 < rate ∧ 0 < s ∧
        max (sSup (normalTrace x μ '' Ioi 0))
          (sSup ((fun r ↦ exponentialTrace x r μ) '' Ioi 0)) ≤
        ∏ j, density rate μ s (x j) := by
  let N := sSup (normalTrace x μ '' Ioi 0)
  let E := sSup ((fun r ↦ exponentialTrace x r μ) '' Ioi 0)
  let F (p : ℝ × ℝ) := ∏ j, density p.1 μ p.2 (x j)
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  have hn : 0 < n := Nat.zero_lt_of_lt i.isLt
  have hN (s : ℝ) (hs : 0 < s) : normalTrace x μ s ≤ N :=
    le_csSup (normalTrace_scale_bounds x μ hi).2.2 (mem_image_of_mem _ hs)
  have hE (r : ℝ) (hr : 0 < r) : exponentialTrace x r μ ≤ E :=
    le_csSup (exponential_trace_bounds x μ hi).2.2 (mem_image_of_mem _ hr)
  have hE₀ : 0 ≤ E := (exp_trace_nonneg x zero_lt_one).trans (hE 1 zero_lt_one)
  constructor
  · rintro ⟨p,hp,hm⟩
    refine ⟨p.1,p.2,hp.1,hp.2,max_le ?_ ?_⟩
    · apply csSup_le (show (normalTrace x μ '' Ioi 0).Nonempty from
          ⟨_,mem_image_of_mem _ (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)⟩)
      rintro _ ⟨s,hs,rfl⟩
      exact le_of_tendsto (tendsto_finsetProd Finset.univ (fun j _ ↦ normal_path μ (x j) hs))
        ((eventually_gt_atTop (0 : ℝ)).mono fun r hr ↦ isMaxOn_iff.mp hm (r,s) ⟨hr,hs⟩)
    · apply csSup_le (show ((fun r ↦ exponentialTrace x r μ) '' Ioi 0).Nonempty from
          ⟨_,mem_image_of_mem _ (show (1 : ℝ) ∈ Ioi 0 from zero_lt_one)⟩)
      rintro _ ⟨r,hr,rfl⟩
      have hd : ∀ᶠ s : ℝ in 𝓝[>] 0, 0 < s := self_mem_nhdsWithin
      exact le_of_tendsto (tendsto_finsetProd Finset.univ (fun j _ ↦ exponential_path r μ (x j)))
        (hd.mono fun s hs ↦ isMaxOn_iff.mp hm (r,s) ⟨hr,hs⟩)
  · rintro ⟨r₀,s₀,hr₀,hs₀,h₀⟩
    by_cases hb : ∃ p ∈ D, max N E < F p
    · obtain ⟨p₁,hp₁,hbetter⟩ := hb
      let J := F p₁
      have hJ : 0 < J := hE₀.trans_lt ((le_max_right N E).trans_lt hbetter)
      let e := min 1 J
      have he : 0 < e := lt_min zero_lt_one hJ
      have hePow : e^n ≤ J := by
        have h : e^n ≤ e := by
          obtain ⟨k,hk⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
          rw [hk,pow_succ]
          exact (mul_le_mul_of_nonneg_right (pow_le_one₀ he.le (min_le_left _ _)) he.le).trans_eq
            (one_mul e)
        exact h.trans (min_le_right _ _)
      obtain ⟨a,ha,R₀,hR₀,hcorner⟩ := small_scale_large_rate_bound x μ hi hJ
      have hε : ∃ ε > 0, (1+ε)^n*N < J := by
        have hc : Tendsto (fun ε : ℝ ↦ (1+ε)^n*N) (𝓝[>] 0) (𝓝 N) := by
          have hbase : ContinuousAt (fun ε : ℝ ↦ (1+ε)^n*N) 0 := by fun_prop
          simpa using hbase.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
        have hd : ∀ᶠ ε : ℝ in 𝓝[>] 0, 0 < ε := self_mem_nhdsWithin
        have hnarrow := hc.eventually (eventually_lt_nhds ((le_max_left N E).trans_lt hbetter))
        exact (hd.and hnarrow).exists
      obtain ⟨ε,hε,hgap⟩ := hε
      let X := ∑ j, |x j-μ|
      have hX : 0 ≤ X := Finset.sum_nonneg fun _ _ ↦ abs_nonneg _
      let Z := X/a
      let R := max p₁.1 (max R₀ (1+(1+ε)*Z/(ε*a)))
      obtain ⟨δ,hδ,hfloor⟩ := bounded_rate_small_scale x μ hE₀
        ((le_max_right N E).trans_lt hbetter) hE
      let A := min e p₁.1
      have hA : 0 < A := lt_min he hp₁.1
      let B := min δ p₁.2
      have hB : 0 < B := lt_min hδ hp₁.2
      let S := max (φ 0/e) p₁.2
      let K := Icc A R ×ˢ Icc B S
      have hK : K ⊆ D := fun _ hp ↦ ⟨hA.trans_le hp.1.1,hB.trans_le hp.2.1⟩
      have h₁ : p₁ ∈ K := ⟨⟨min_le_right _ _,le_max_left _ _⟩,⟨min_le_right _ _,le_max_right _ _⟩⟩
      have hc : ContinuousOn F D := by
        refine continuousOn_finsetProd Finset.univ fun j _ ↦ ?_
        unfold density
        refine ContinuousOn.mul (by fun_prop) (cdf_continuous.comp_continuousOn ?_)
        refine ContinuousOn.sub (ContinuousOn.div (by fun_prop) continuous_snd.continuousOn
          (fun p hp ↦ (show 0 < p.2 from hp.2).ne')) (by fun_prop)
      obtain ⟨p,hp,hm⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact K).exists_isMaxOn
        ⟨p₁,h₁⟩ (hc.mono hK)
      refine ⟨p,hK hp,fun q hq ↦ ?_⟩
      by_cases hqK : q ∈ K
      · exact hm hqK
      · have hr : 0 < q.1 := hq.1
        have hs : 0 < q.2 := hq.2
        have hall (hb : ∀ j, density q.1 μ q.2 (x j) ≤ e) : F q ≤ J := by
          have ht := Finset.prod_le_prod₀ (s := Finset.univ)
            (fun j _ ↦ (density_positive hr μ (x j)).le) (fun j _ ↦ hb j)
          exact (show F q ≤ e^n by simpa using ht).trans hePow
        have hcut : F q ≤ J := by
          by_cases hAq : A ≤ q.1
          · by_cases hSq : q.2 ≤ S
            · by_cases hRq : q.1 ≤ R
              · have hlow : q.2 ≤ δ :=
                  (le_of_not_ge (fun h ↦ hqK ⟨⟨hAq,hRq⟩,⟨h,hSq⟩⟩)).trans (min_le_left _ _)
                exact hfloor q.2 q.1 hs hlow hr hRq
              · have hhigh : R < q.1 := lt_of_not_ge hRq
                by_cases ha' : a ≤ q.2
                · have hR' : (1+ε)*Z < ε*(q.1*q.2) := by
                    have hbig : 1+(1+ε)*Z/(ε*a) < q.1 :=
                      ((le_max_right R₀ _).trans (le_max_right _ _)).trans_lt hhigh
                    have h := (div_lt_iff₀ (mul_pos hε ha)).mp
                      (show (1+ε)*Z/(ε*a)<q.1 by linarith)
                    have hh := mul_le_mul_of_nonneg_left ha' (mul_pos hε hr).le
                    nlinarith only [h,hh]
                  have hterm (j : Fin n) : density q.1 μ q.2 (x j) ≤
                      (1+ε)*(φ ((x j-μ)/q.2)/q.2) := by
                    apply gaussian_relative_bound hr hs hε ?_ hR'
                    calc
                      _ ≤ |x j-μ|/q.2 := div_le_div_of_nonneg_right (le_abs_self _) hs.le
                      _ ≤ X/q.2 := div_le_div_of_nonneg_right
                        (Finset.single_le_sum (f := fun j ↦ |x j-μ|) (fun _ _ ↦ abs_nonneg _)
                          (Finset.mem_univ j)) hs.le
                      _ ≤ _ := div_le_div_of_nonneg_left hX ha ha'
                  have hp := Finset.prod_le_prod₀ (s := Finset.univ)
                    (fun j _ ↦ (density_positive hr μ (x j)).le) (fun j _ ↦ hterm j)
                  have hh : F q ≤ (1+ε)^n*normalTrace x μ q.2 := by
                    simpa [Finset.prod_mul_distrib,normalTrace] using hp
                  have hle := mul_le_mul_of_nonneg_left (hN q.2 hs) (by positivity : 0 ≤ (1+ε)^n)
                  exact (hh.trans hle).trans hgap.le
                · exact hcorner q.2 q.1 hs (le_of_not_ge ha')
                    (((le_max_left _ _).trans (le_max_right _ _)).trans hhigh.le)
            · apply hall
              intro j
              refine (gaussian_peak_strict (μ := μ) (x := x j) hr hs).le.trans ?_
              apply (div_le_iff₀ hs).mpr
              have h := (div_le_iff₀ he).mp ((le_max_left _ p₁.2).trans (le_of_not_ge hSq))
              simpa only [mul_comm] using h
          · apply hall
            intro j
            exact (exponential_peak (μ := μ) (x := x j) hr hs).le.trans
              ((le_of_not_ge hAq).trans (min_le_left _ _))
        exact hcut.trans (isMaxOn_iff.mp hm p₁ h₁)
    · have hall : ∀ q ∈ D, F q ≤ max N E :=
        fun q hq ↦ le_of_not_gt (fun h ↦ hb ⟨q,hq,h⟩)
      exact ⟨(r₀,s₀),⟨hr₀,hs₀⟩,fun q hq ↦ (hall q hq).trans h₀⟩

/-- An interior profile value at least as high as both faces gives attainment
at a stationary point. Conversely, an interior maximizer on the closure
is stationary and reaches both faces. -/
private theorem critical_value_of_compact_profile {P : ℝ → ℝ} (hP : ContinuousOn P (Icc 0 1))
    (hd : DifferentiableOn ℝ P (Ioo 0 1)) :
    (∃ t ∈ Ioo 0 1, HasDerivAt P 0 t ∧ max (P 0) (P 1) ≤ P t) ↔
    ∃ t ∈ Ioo 0 1, IsMaxOn P (Icc 0 1) t := by
  constructor
  · rintro ⟨t, ht, _, hface⟩
    obtain ⟨m, hm, hmax⟩ := isCompact_Icc.exists_isMaxOn
      ⟨t, Ioo_subset_Icc_self ht⟩ hP
    by_cases hi : m ∈ Ioo (0 : ℝ) 1
    · exact ⟨m, hi, hmax⟩
    · have he : m = 0 ∨ m = 1 := by
        simp only [mem_Ioo, not_and_or, not_lt] at hi
        exact hi.imp (le_antisymm · hm.1) (le_antisymm hm.2 ·)
      refine ⟨t, ht, fun q hq ↦ (hmax hq).trans ?_⟩
      exact he.elim (fun h ↦ h ▸ (le_max_left _ _).trans hface)
        (fun h ↦ h ▸ (le_max_right _ _).trans hface)
  · rintro ⟨t, ht, hmax⟩
    have hlocal : IsLocalMax P t :=
      Filter.mem_of_superset (Icc_mem_nhds ht.1 ht.2) (fun _ hq ↦ hmax hq)
    have hdiff := (hd t ht).differentiableAt (Ioo_mem_nhds ht.1 ht.2)
    exact ⟨t, ht, hlocal.deriv_eq_zero ▸ hdiff.hasDerivAt,
      max_le (hmax (by norm_num)) (hmax (by norm_num))⟩

private def fixedShapeLoss (k z : ℝ) : ℝ := k*z-log (Φ (z-k))

private theorem fixedShapeLoss_convex (k : ℝ) :
    StrictConvexOn ℝ univ (fixedShapeLoss k) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ hxy a b ha hb hab
  have hc := cdf_strictLogConcave.2 (mem_univ (x-k)) (mem_univ (y-k))
    (fun h ↦ hxy (sub_left_inj.mp h)) ha hb hab
  simp only [smul_eq_mul] at hc ⊢
  have he : a*(x-k)+b*(y-k) = a*x+b*y-k := by
    rw [show b = 1-a by linarith]
    ring
  rw [he] at hc
  unfold fixedShapeLoss
  linarith

private theorem fixedShapeLoss_bound {k : ℝ} (hk : 0 < k) :
    LocationScale.LinearLowerBound (fixedShapeLoss k) := by
  refine ⟨k, hk, 0, fun z ↦ ?_⟩
  simp only [sub_zero, fixedShapeLoss]
  rcases le_or_gt 0 z with hz | hz
  · rw [abs_of_nonneg hz]
    linarith [log_nonpos (cdf_pos (z-k)).le (cdf_lt_one (z-k)).le]
  · rw [abs_of_neg hz]
    let y := k-z
    have hy : 0 < y := by dsimp [y]; linarith
    have hb := weightedTail_strictAnti hy
    rw [show weightedTail 0 = 1/2 by simp [weightedTail, cdf_zero]] at hb
    have he : Φ (z-k) ≤ exp (-(z-k)^2/2) := by
      have hpos := exp_pos (y^2/2)
      have hle : exp (y^2/2) * Φ (-y) ≤ 1 := hb.le.trans (by norm_num)
      have hq : Φ (-y) ≤ 1/exp (y^2/2) :=
        (le_div_iff₀ hpos).mpr (by rwa [mul_comm])
      simpa only [y, one_div, ← exp_neg, show -(k-z) = z-k by ring,
        show (k-z)^2 = (z-k)^2 by ring, neg_div] using hq
    have hlog := log_le_log (cdf_pos (z-k)) he
    rw [log_exp] at hlog
    nlinarith only [hlog, sq_nonneg (z+k)]

/-- EMG log likelihood on the slice of fixed positive dimensionless shape
`rate * sigma = k`. -/
def shapeLogLik {n : ℕ} (x : Fin n → ℝ) (k : ℝ) (p : ℝ × ℝ) : ℝ :=
  locationLogLik x (k/p.2) p.2 p.1

private theorem shape_log_transfer {n : ℕ} (x : Fin n → ℝ) {k : ℝ} (hk : 0 < k)
    (p : ℝ × ℝ) (hp : p ∈ LocationScale.domain) :
    shapeLogLik x k p = LocationScale.logLik (fixedShapeLoss k) x p +
      n*(log k+k^2/2) := by
  have hs : 0 < p.2 := hp.2
  have hks : k/p.2*p.2 = k := div_mul_cancel₀ _ hs.ne'
  have hc : (∑ _ : Fin n, (log k+k^2/2)) = n*(log k+k^2/2) := by
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [shapeLogLik, locationLogLik, LocationScale.logLik, ← hc, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  rw [location_log_formula (div_pos hk hs), hks, log_div hk.ne' hs.ne']
  unfold fixedShapeLoss
  ring

/-- For a nonconstant sample every fixed positive dimensionless shape has
a unique finite location and scale optimizer. -/
theorem fixed_shape_fit {n : ℕ} {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {k : ℝ} (hk : 0 < k) :
    ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (shapeLogLik x k) LocationScale.domain p := by
  have he (p : ℝ × ℝ) (hp : p ∈ LocationScale.domain) :
      IsMaxOn (shapeLogLik x k) LocationScale.domain p ↔
      IsMaxOn (LocationScale.logLik (fixedShapeLoss k) x) LocationScale.domain p := by
    simp only [isMaxOn_iff]
    constructor <;> intro hm q hq
    · have h := hm q hq
      rw [shape_log_transfer x hk q hq, shape_log_transfer x hk p hp] at h
      exact le_of_add_le_add_right h
    · rw [shape_log_transfer x hk q hq, shape_log_transfer x hk p hp]
      exact add_le_add_left (hm q hq) _
  obtain ⟨p, ⟨hp, hm⟩, hu⟩ := LocationScale.existsUnique_mle
    (fixedShapeLoss_convex k) (fixedShapeLoss_bound hk) hx
  exact ⟨p, ⟨hp, (he p hp).mpr hm⟩, fun q hq ↦ hu q ⟨hq.1, (he q hq.1).mp hq.2⟩⟩

/-- Shape compactification in the chart `k = (1-t)/t`, with the
Gaussian endpoint at zero and the exponential endpoint at one. -/
def interiorProfile {n : ℕ} (x : Fin n → ℝ) (t : ℝ) : ℝ :=
  sSup (shapeLogLik x ((1-t)/t) '' LocationScale.domain)

private theorem attained_interior_profile {n : ℕ} {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {t : ℝ} (ht : t ∈ Ioo 0 1) :
    ∃ p ∈ LocationScale.domain,
      IsMaxOn (shapeLogLik x ((1-t)/t)) LocationScale.domain p ∧
      interiorProfile x t = shapeLogLik x ((1-t)/t) p := by
  obtain ⟨p, ⟨hp, hm⟩, _⟩ := fixed_shape_fit hx (div_pos (sub_pos.mpr ht.2) ht.1)
  exact ⟨p, hp, hm, (hm.isLUB hp).csSup_eq (Nonempty.image _ ⟨p, hp⟩)⟩

/-- Given a differentiable continuous compactification of the actual EMG
profile, an interior stationary value reaching both endpoints is equivalent
to a joint finite maximizer. The endpoints may tie the finite fit. -/
theorem critical_value_criterion {n : ℕ} {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {P : ℝ → ℝ}
    (hP : ContinuousOn P (Icc 0 1)) (hd : DifferentiableOn ℝ P (Ioo 0 1))
    (hprofile : EqOn P (interiorProfile x) (Ioo 0 1)) :
    (∃ t ∈ Ioo 0 1, HasDerivAt P 0 t ∧ max (P 0) (P 1) ≤ P t) ↔
      ∃ q ∈ (Ioo 0 1 ×ˢ LocationScale.domain),
        IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
          (Ioo 0 1 ×ˢ LocationScale.domain) q := by
  have hclosure : closure (Ioo (0 : ℝ) 1) = Icc 0 1 := closure_Ioo zero_ne_one
  rw [critical_value_of_compact_profile hP hd]
  constructor
  · rintro ⟨t, ht, hmax⟩
    obtain ⟨p, hp, _, he⟩ := attained_interior_profile hx ht
    refine ⟨(t,p), ⟨ht,hp⟩, ?_⟩
    intro q hq
    obtain ⟨v, hv, hmaxv, hev⟩ := attained_interior_profile hx hq.1
    change shapeLogLik x ((1-q.1)/q.1) q.2 ≤ shapeLogLik x ((1-t)/t) p
    rw [← he, ← hprofile ht]
    calc
      _ ≤ shapeLogLik x ((1-q.1)/q.1) v := hmaxv hq.2
      _ = P q.1 := hev.symm.trans (hprofile hq.1).symm
      _ ≤ P t := hmax (Ioo_subset_Icc_self hq.1)
  · rintro ⟨⟨t,p⟩, ⟨ht,hp⟩, hmax⟩
    have hlocal : IsMaxOn (shapeLogLik x ((1-t)/t)) LocationScale.domain p := by
      intro v hv
      exact isMaxOn_iff.mp hmax (t,v) ⟨ht,hv⟩
    have he : P t = shapeLogLik x ((1-t)/t) p := by
      rw [hprofile ht, interiorProfile]
      exact (hlocal.isLUB hp).csSup_eq (Nonempty.image _ ⟨p,hp⟩)
    have hm : IsMaxOn P (Ioo 0 1) t := by
      intro q hq
      obtain ⟨v, hv, _, hev⟩ := attained_interior_profile hx hq
      change P q ≤ P t
      rw [he, hprofile hq, hev]
      exact isMaxOn_iff.mp hmax (q,v) ⟨hq,hv⟩
    have hm' := hm.closure (hclosure.symm ▸ hP)
    exact ⟨t, ht, hclosure ▸ hm'⟩

end

end UnivariateML.SkewLSEMG
