module

public import Mathlib.Analysis.SpecialFunctions.Log.Basic
public import Mathlib.Topology.Order.IntermediateValue
public import UnivariateML.Family.SkewLSGaussian
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Bhattacharjee likelihood

## Deviations

The strict-coverage theorem gives the uniform supremum. The held-centre
endpoint theorem gives its path value. At range equality the joint envelope
maximizes the complementary endpoint trace over a compact interval of weights.
-/

@[expose] public section

open Set Filter Topology MeasureTheory Real UnivariateML.SkewLSGaussian

namespace UnivariateML.SkewLSBhatt

noncomputable section

private def coveredLik {n : ℕ} (F : ℝ → ℝ) (x : Fin n → ℝ) (a m s : ℝ) : ℝ :=
  ∏ i, (F ((x i - m + a) / s) - F ((x i - m - a) / s)) / (2 * a)

/-- Strict coverage gives the unattained uniform likelihood envelope. -/
private theorem strict_cover_uniform_of_cdf {n : ℕ} (hn : 0 < n) {F : ℝ → ℝ}
    (hF : StrictMono F) (hpos : ∀ z, 0 < F z) (hlt : ∀ z, F z < 1)
    (hzero : Tendsto F atBot (𝓝 0)) (hone : Tendsto F atTop (𝓝 1))
    {x : Fin n → ℝ} {a m : ℝ} (ha : 0 < a) (hx : ∀ i, |x i - m| < a) :
    IsLUB (coveredLik F x a m '' Ioi 0) ((1 / (2 * a)) ^ n) ∧
      ∀ m' s : ℝ, 0 < s → coveredLik F x a m' s < (1 / (2 * a)) ^ n := by
  have hden : 0 < 2 * a := by positivity
  have hprod : (∏ _ : Fin n, (1 / (2 * a))) = (1 / (2 * a)) ^ n := by simp
  have hstrict (m' s : ℝ) (hs : 0 < s) :
      coveredLik F x a m' s < (1 / (2 * a)) ^ n := by
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    unfold coveredLik
    rw [← hprod]
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      apply div_pos _ hden
      apply sub_pos.mpr (hF ((div_lt_div_iff_of_pos_right hs).mpr _))
      linarith
    · intro i _
      apply (div_lt_div_iff_of_pos_right hden).mpr
      linarith [hlt ((x i - m' + a) / s), hpos ((x i - m' - a) / s)]
    · exact Finset.univ_nonempty
  have ht : Tendsto (coveredLik F x a m) (𝓝[>] 0) (𝓝 ((1 / (2 * a)) ^ n)) := by
    rw [← hprod]
    unfold coveredLik
    refine tendsto_finsetProd _ ?_
    intro i _
    have hi := abs_lt.mp (hx i)
    have hp : 0 < x i - m + a := by linarith
    have hm : x i - m - a < 0 := by linarith
    have h1 := hone.comp (by
      simpa [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop hp)
    have h0 := hzero.comp (by
      simpa [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop_of_neg hm)
    simpa only [div_eq_mul_inv, sub_zero, Function.comp_apply] using (h1.sub h0).div_const (2 * a)
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨s, hs, rfl⟩
    exact (hstrict m s hs).le
  · intro b hb
    have he : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
    exact le_of_tendsto ht (he.mono (fun s hs ↦ hb ⟨s, hs, rfl⟩))

/-- At a held centre, endpoint observations contribute their CDF tail
weights to the collapse value. A strictly covered point contributes one. -/
private theorem held_centre_uniform_path_of_cdf {n : ℕ} {F : ℝ → ℝ}
    (hzero : Tendsto F atBot (𝓝 0)) (hone : Tendsto F atTop (𝓝 1))
    {x : Fin n → ℝ} {a m : ℝ} (ha : 0 < a) (hx : ∀ i, |x i - m| ≤ a) :
    Tendsto (coveredLik F x a m) (𝓝[>] 0)
      (𝓝 (∏ i, ((if x i - m = -a then F 0 else
        if x i - m = a then 1 - F 0 else 1) / (2*a)))) := by
  have htop {b : ℝ} (hb : 0 < b) :
      Tendsto (fun s ↦ F (b / s)) (𝓝[>] 0) (𝓝 1) := by
    simpa only [div_eq_mul_inv, Function.comp_def] using
      hone.comp (tendsto_inv_nhdsGT_zero.const_mul_atTop hb)
  have hbot {b : ℝ} (hb : b < 0) :
      Tendsto (fun s ↦ F (b / s)) (𝓝[>] 0) (𝓝 0) := by
    simpa only [div_eq_mul_inv, Function.comp_def] using
      hzero.comp (tendsto_inv_nhdsGT_zero.const_mul_atTop_of_neg hb)
  unfold coveredLik
  refine tendsto_finsetProd _ fun i _ ↦ ?_
  have hi := abs_le.mp (hx i)
  split_ifs with hl hr
  · have he : x i - m + a = 0 := by linarith
    have hn : x i - m - a < 0 := by linarith
    simp only [he, zero_div]
    simpa using (tendsto_const_nhds.sub (hbot hn)).div_const (2*a)
  · have he : x i - m - a = 0 := by linarith
    have hp : 0 < x i - m + a := by linarith
    simp only [he, zero_div]
    simpa using ((htop hp).sub tendsto_const_nhds).div_const (2*a)
  · have hp : 0 < x i - m + a := by
      rcases lt_or_eq_of_le hi.1 with hh | hh
      · linarith
      · exact (hl hh.symm).elim
    have hn : x i - m - a < 0 := by
      rcases lt_or_eq_of_le hi.2 with hh | hh
      · linarith
      · exact (hr hh).elim
    simpa using ((htop hp).sub (hbot hn)).div_const (2*a)

/-- The likelihood at positive width is the product of Gaussian interval masses
divided by the common interval length. -/
def likelihood {n : ℕ} (x : Fin n → ℝ) (a m s : ℝ) : ℝ :=
  ∏ i, (if a = 0 then φ ((x i - m) / s) / s else
    (Φ ((x i - m + a) / s) - Φ ((x i - m - a) / s)) / (2 * a))

private theorem likelihood_pos_width {n : ℕ} (x : Fin n → ℝ) {a : ℝ}
    (ha : 0 < a) (m s : ℝ) : likelihood x a m s = coveredLik Φ x a m s := by
  simp [likelihood, coveredLik, ha.ne']

/-- Strict coverage gives the unattained uniform likelihood envelope. -/
theorem strict_cover_uniform {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| < a) :
    IsLUB (likelihood x a m '' Ioi 0) ((1 / (2 * a)) ^ n) ∧
      ∀ m' s : ℝ, 0 < s → likelihood x a m' s < (1 / (2 * a)) ^ n := by
  simpa only [likelihood_pos_width x ha] using
    strict_cover_uniform_of_cdf hn cdf_strictMono cdf_pos cdf_lt_one cdf_atBot cdf_atTop ha hx

/-- At a held centre, every tied endpoint contributes one half to the uniform
trace. Every strictly covered observation contributes the full uniform density. -/
theorem held_centre_uniform_path {n : ℕ} {x : Fin n → ℝ} {a m : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - m| ≤ a) :
    Tendsto (likelihood x a m) (𝓝[>] 0)
      (𝓝 (∏ i, ((if |x i - m| = a then 1/2 else 1) / (2*a)))) := by
  have he (i : Fin n) : (if x i - m = -a then Φ 0 else
        if x i - m = a then 1 - Φ 0 else 1) =
      (if |x i - m| = a then 1/2 else 1) := by
    simp only [abs_eq (le_of_lt ha), cdf_zero]
    split_ifs <;> norm_num <;> tauto
  rw [show likelihood x a m = coveredLik Φ x a m from funext (likelihood_pos_width x ha m)]
  simpa only [he] using
    held_centre_uniform_path_of_cdf cdf_atBot cdf_atTop ha hx

private def pointDensity (a r s : ℝ) : ℝ :=
  if a = 0 then φ (r / s) / s else
    (Φ ((r + a) / s) - Φ ((r - a) / s)) / (2 * a)

private theorem phi_cont : Continuous φ := continuous_iff_continuousAt.mpr
  (fun x ↦ (phi_hasDerivAt x).continuousAt)

private theorem interval_mass (u v : ℝ) : Φ v - Φ u = ∫ z in u..v, φ z := by
  rw [cdf_integral, cdf_integral, intervalIntegral.integral_Iic_sub_Iic
    phi_integrable.integrableOn phi_integrable.integrableOn]

private theorem density_average {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    pointDensity a r s = (∫ z in (-1 : ℝ)..1, φ ((r + a * z) / s)) / (2*s) := by
  rcases ha.eq_or_lt with h | h
  · subst a
    simp [pointDensity, intervalIntegral.integral_const, smul_eq_mul]
    ring
  · simp only [pointDensity, h.ne', ite_false, interval_mass]
    have he (z : ℝ) : (r + a*z)/s = (a/s)*z + r/s := by ring
    simp_rw [he]
    rw [intervalIntegral.integral_comp_mul_add φ (div_ne_zero h.ne' hs.ne')]
    simp only [smul_eq_mul]
    rw [show a / s * (-1) + r / s = (r-a)/s by ring,
        show a / s * 1 + r / s = (r+a)/s by ring]
    field_simp

private theorem density_pos {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    0 < pointDensity a r s := by
  rcases ha.eq_or_lt with h | h
  · subst a
    simpa only [pointDensity, ite_true] using div_pos (phi_pos (r/s)) hs
  · simp only [pointDensity, h.ne', ite_false]
    exact div_pos (sub_pos.mpr (cdf_strictMono
      ((div_lt_div_iff_of_pos_right hs).mpr (by linarith)))) (by positivity)

private theorem density_bound_width {a r s : ℝ} (ha : 0 < a) :
    pointDensity a r s < 1 / (2*a) := by
  simp only [pointDensity, ha.ne', ite_false]
  apply (div_lt_div_iff_of_pos_right (show 0 < 2*a by positivity)).mpr
  linarith [cdf_lt_one ((r+a)/s), cdf_pos ((r-a)/s)]

private theorem density_bound_local {a r s M : ℝ} (ha : 0 ≤ a) (hs : 0 < s)
    (hM : ∀ z ∈ Icc (-1 : ℝ) 1, φ ((r + a * z) / s) ≤ M) :
    pointDensity a r s ≤ M / s := by
  rw [density_average ha hs]
  have hc : Continuous (fun z : ℝ ↦ φ ((r + a * z) / s)) :=
    phi_cont.comp (by fun_prop)
  have hb := intervalIntegral.integral_mono_on (μ := volume) (show (-1 : ℝ) ≤ 1 by norm_num)
    (hc.intervalIntegrable _ _) (continuous_const.intervalIntegrable _ _) hM
  rw [intervalIntegral.integral_const, smul_eq_mul] at hb
  apply (div_le_iff₀ (show 0 < 2*s by positivity)).mpr
  rw [show M / s * (2 * s) = 2*M by field_simp]
  linarith

private theorem gaussian_peak (z : ℝ) : φ z ≤ φ 0 := by
  simp only [phi_formula, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, neg_zero,
    exp_zero, mul_one]
  exact mul_le_of_le_one_right (by positivity) (exp_le_one_iff.mpr (neg_nonpos.mpr (by positivity)))

private theorem density_bound_scale {a r s : ℝ} (ha : 0 ≤ a) (hs : 0 < s) :
    pointDensity a r s ≤ φ 0 / s :=
  density_bound_local ha hs (fun _ _ ↦ gaussian_peak _)

private theorem likelihood_pos {n : ℕ} (x : Fin n → ℝ) {a m s : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) : 0 < likelihood x a m s :=
  Finset.prod_pos (fun i _ ↦ density_pos (r := x i - m) ha hs)

private theorem small_likelihood_of_one {n : ℕ} (x : Fin n → ℝ) {a m s M L : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) (hM : ∀ i, pointDensity a (x i - m) s ≤ M)
    (i : Fin n) (hi : pointDensity a (x i - m) s ≤ L / M ^ (n - 1)) (hMp : 0 < M) :
    likelihood x a m s ≤ L := by
  classical
  let f (j : Fin n) := pointDensity a (x j - m) s
  change ∏ j, f j ≤ L
  rw [← Finset.prod_erase_mul Finset.univ f (Finset.mem_univ i)]
  have hprod : (∏ j ∈ Finset.univ.erase i, f j) ≤ M ^ (n-1) := by
    refine (Finset.prod_le_prod₀ (fun j _ ↦ (density_pos (r := x j - m) ha hs).le)
      (fun j _ ↦ hM j)).trans_eq ?_
    simp
  calc
    _ ≤ M ^ (n-1) * f i := mul_le_mul_of_nonneg_right hprod (density_pos ha hs).le
    _ ≤ M ^ (n-1) * (L / M ^ (n - 1)) :=
      mul_le_mul_of_nonneg_left hi (by positivity)
    _ = L := by field_simp

private theorem phi_small_of_abs_ge {ε : ℝ} (he : 0 < ε) :
    ∃ R > 0, ∀ z, R ≤ |z| → φ z ≤ ε := by
  have ht : Tendsto φ atTop (𝓝 0) := by
    have h := (tendsto_id.atTop_pow₀ (show (2 : ℕ) ≠ 0 by decide) :
      Tendsto (fun x : ℝ ↦ x ^ 2) atTop atTop).atTop_mul_const (show (0 : ℝ) < 1/2 by norm_num)
    have he' := (tendsto_exp_neg_atTop_nhds_zero.comp h).const_mul (1 / sqrt (2 * π))
    have heq : φ = (fun z ↦ (1 / sqrt (2 * π)) * exp (-(z^2/2))) := funext phi_formula
    rw [heq]
    simpa [Function.comp_def, div_eq_mul_inv] using he'
  obtain ⟨R, hR⟩ := eventually_atTop.mp (ht.eventually (Iio_mem_nhds he))
  refine ⟨max 1 R, lt_of_lt_of_le zero_lt_one (le_max_left _ _), fun z hz ↦ ?_⟩
  have h := hR |z| ((le_max_right 1 R).trans hz)
  simpa only [mem_Iio, phi_formula, sq_abs] using h.le

private theorem density_far_centre {a r s A S δ R ε : ℝ}
    (ha : 0 ≤ a) (hs : 0 < s) (hA : a ≤ A) (hS : s ≤ S) (hδ : 0 < δ)
    (hδs : δ ≤ s) (hR : 0 < R) (hε : 0 ≤ ε)
    (htail : ∀ z, R ≤ |z| → φ z ≤ ε) (hr : A + S * R ≤ |r|) :
    pointDensity a r s ≤ ε / δ := by
  refine (density_bound_local ha hs (M := ε) (fun z hz ↦ htail _ ?_)).trans
    (div_le_div_of_nonneg_left hε hδ hδs)
  rw [abs_div, abs_of_pos hs, le_div_iff₀ hs]
  have hmul : |a*z| ≤ A := by
    rw [abs_mul, abs_of_nonneg ha]
    exact (mul_le_of_le_one_right ha (abs_le.mpr hz)).trans hA
  have htri : |r| ≤ |r + a*z| + |a*z| := by
    simpa using abs_sub (r + a*z) (a*z)
  have hSr := mul_le_mul_of_nonneg_right hS hR.le
  linarith

private theorem likelihood_continuous_fixed_scale {n : ℕ} (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) : ContinuousOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s)
      (Ici 0 ×ˢ univ) := by
  let f (i : Fin n) (p : ℝ × ℝ) (z : ℝ) := φ ((x i - p.2 + p.1*z)/s)
  have hcont (i : Fin n) : Continuous (fun p : ℝ × ℝ ↦
      (∫ z in (-1 : ℝ)..1, f i p z) / (2*s)) := by
    have h : Continuous (fun q : (ℝ × ℝ) × ℝ ↦ f i q.1 q.2) :=
      show Continuous (fun q : (ℝ × ℝ) × ℝ ↦ φ ((x i - q.1.2 + q.1.1*q.2)/s)) from
        phi_cont.comp (by fun_prop)
    simp only [intervalIntegral.integral_of_le (by norm_num : (-1 : ℝ) ≤ 1)]
    simp_rw [← MeasureTheory.integral_Icc_eq_integral_Ioc]
    exact (continuous_parametric_integral_of_continuous (μ := volume) h isCompact_Icc).div_const _
  refine (continuous_finsetProd Finset.univ (fun i _ ↦ hcont i)).continuousOn.congr ?_
  intro p hp
  apply Finset.prod_congr rfl
  intro i _
  exact density_average hp.1 hs

/-- Holding a positive Gaussian scale guarantees attainment, including at
the zero-width normal seam. -/
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) :
    ∃ p ∈ (Ici 0 ×ˢ (univ : Set ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s) (Ici 0 ×ˢ univ) p := by
  let i : Fin n := ⟨0, hn⟩
  let L := likelihood x 0 0 s
  have hL : 0 < L := likelihood_pos x le_rfl hs
  let M := φ 0 / s
  have hM : 0 < M := div_pos (phi_pos _) hs
  let A := 1 + 1 / (2 * (L / M^(n-1)))
  let ε := s * (L / M^(n-1))
  have hε : 0 < ε := by dsimp [ε]; positivity
  have hA : 0 < A := by dsimp [A]; positivity
  obtain ⟨R, hR, htail⟩ := phi_small_of_abs_ge hε
  let B := |x i| + A + s*R
  have hB : 0 < B := by dsimp [B]; positivity
  let K := Icc 0 A ×ˢ Icc (-B) B
  have hK : K ⊆ Ici 0 ×ˢ (univ : Set ℝ) := fun _ hp ↦ ⟨hp.1.1, mem_univ _⟩
  have hstart : (0, 0) ∈ K :=
    ⟨⟨le_rfl, hA.le⟩, ⟨by linarith, hB.le⟩⟩
  obtain ⟨p, hp, hmax⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact K).exists_isMaxOn
    ⟨(0, 0), hstart⟩ ((likelihood_continuous_fixed_scale x hs).mono hK)
  refine ⟨p, hK hp, fun q hq ↦ ?_⟩
  by_cases hqK : q ∈ K
  · exact hmax hqK
  · have small : likelihood x q.1 q.2 s ≤ L := by
      refine small_likelihood_of_one x hq.1 hs
        (fun j ↦ density_bound_scale (r := x j - q.2) hq.1 hs) i ?_ hM
      by_cases hwidth : q.1 ≤ A
      · have habs : B < |q.2| := by
          by_contra! hb
          exact hqK ⟨⟨hq.1, hwidth⟩, abs_le.mp hb⟩
        have htri := abs_sub_le q.2 (x i) 0
        rw [sub_zero, sub_zero, abs_sub_comm q.2 (x i)] at htri
        have hfar : A + s*R ≤ |x i - q.2| := by dsimp [B] at habs; linarith
        simpa only [ε, mul_div_cancel_left₀ _ hs.ne'] using
          density_far_centre hq.1 hs hwidth le_rfl hs le_rfl hR hε.le htail hfar
      · have hw : 0 < q.1 := hA.trans (lt_of_not_ge hwidth)
        refine (density_bound_width (r := x i - q.2) (s := s) hw).le.trans ?_
        apply (div_le_iff₀ (show 0 < 2*q.1 by positivity)).mpr
        have h := mul_le_mul_of_nonneg_left (le_of_not_ge hwidth)
          (show 0 ≤ L / M^(n-1) by positivity)
        have he := (div_mul_cancel₀ (1 : ℝ) (show 2 * (L / M^(n-1)) ≠ 0 by positivity))
        dsimp [A] at h
        nlinarith
    exact small.trans (hmax hstart)

/-- With both Gaussian scale and centre held, the maximum over nonnegative
width is attained. -/
theorem held_scale_centre_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (m : ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ a ∈ Ici (0 : ℝ), IsMaxOn (fun w ↦ likelihood x w m s) (Ici 0) a := by
  let i : Fin n := ⟨0, hn⟩
  let L := likelihood x 0 m s
  have hL : 0 < L := likelihood_pos x le_rfl hs
  let M := φ 0 / s
  have hM : 0 < M := div_pos (phi_pos _) hs
  let e := L / M^(n-1)
  have he : 0 < e := by dsimp [e]; positivity
  let A := 1 / (2*e)
  have hA : 0 < A := by dsimp [A]; positivity
  have hc : ContinuousOn (fun w ↦ likelihood x w m s) (Ici 0) :=
    (likelihood_continuous_fixed_scale x hs).comp
      (continuous_id.prodMk continuous_const).continuousOn (fun _ hw ↦ ⟨hw, mem_univ _⟩)
  have h0 : (0 : ℝ) ∈ Icc 0 A := ⟨le_rfl, hA.le⟩
  obtain ⟨a, ha, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨0, h0⟩
    (hc.mono fun _ hw ↦ hw.1)
  refine ⟨a, ha.1, fun w hw ↦ ?_⟩
  rcases le_or_gt w A with hwA | hwA
  · exact hmax ⟨hw, hwA⟩
  · have hwpos := hA.trans hwA
    have hb : 1 / (2*w) ≤ e := by
      apply (div_le_iff₀ (show 0 < 2*w by positivity)).mpr
      have h := (div_le_iff₀ (show 0 < 2*e by positivity)).mp hwA.le
      change 1 ≤ w * (2*e) at h
      linarith only [h, show w * (2*e) = e * (2*w) by ring]
    have hcut : likelihood x w m s ≤ L := small_likelihood_of_one x
      (m := m) (M := M) hwpos.le hs (fun _ ↦ density_bound_scale hwpos.le hs) i
      ((density_bound_width (r := x i-m) (s := s) hwpos).le.trans hb) hM
    exact hcut.trans (isMaxOn_iff.mp hmax 0 h0)

private theorem likelihood_continuous_pos_width {n : ℕ} (x : Fin n → ℝ)
    {a : ℝ} (ha : 0 < a) : ContinuousOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2)
      (univ ×ˢ Ioi 0) := by
  simp only [likelihood_pos_width x ha, coveredLik]
  refine continuousOn_finsetProd Finset.univ fun i _ ↦ ?_
  have hc (b : ℝ) : ContinuousOn (fun p : ℝ × ℝ ↦ Φ ((x i - p.1 + b) / p.2))
      (univ ×ˢ Ioi 0) :=
    cdf_continuous.comp_continuousOn ((by fun_prop : Continuous
      (fun p : ℝ × ℝ ↦ x i - p.1 + b)).continuousOn.div continuous_snd.continuousOn
      (fun p hp ↦ (show 0 < p.2 from hp.2).ne'))
  exact ((hc a).sub (by simpa only [sub_eq_add_neg] using hc (-a))).div_const _

private theorem positive_width_attainment {n : ℕ} (x : Fin n → ℝ) {a : ℝ}
    (ha : 0 < a) {i j : Fin n} (hgap : 2 * a < x j - x i) :
    ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) (univ ×ˢ Ioi 0) p := by
  let L := likelihood x a 0 1
  have hL : 0 < L := likelihood_pos x ha.le zero_lt_one
  let M := 1 / (2*a)
  have hM : 0 < M := by dsimp [M]; positivity
  let e := L / M^(n-1)
  have he : 0 < e := by dsimp [e]; positivity
  have he' : 0 < 2*a*e := by positivity
  obtain ⟨U, hU⟩ := eventually_atBot.mp (cdf_atBot.eventually (Iio_mem_nhds he'))
  obtain ⟨V, hV⟩ := eventually_atTop.mp (cdf_atTop.eventually
    (Ioi_mem_nhds (show 1 - 2*a*e < 1 by linarith)))
  let T := max 1 (max (-U) V)
  have hT : 0 < T := zero_lt_one.trans_le (le_max_left _ _)
  have hTU : -T ≤ U := by
    dsimp [T]
    linarith [le_max_left (-U) V, le_max_right (1 : ℝ) (max (-U) V)]
  have hTV : V ≤ T := (le_max_right _ _).trans (le_max_right _ _)
  let d := (x j - x i)/2 - a
  have hd : 0 < d := by dsimp [d]; linarith
  let δ := min 1 (d/T)
  have hδ : 0 < δ := lt_min zero_lt_one (div_pos hd hT)
  let S := 1 + φ 0 / e
  have hS : 1 ≤ S := le_add_of_nonneg_right (div_nonneg (phi_pos _).le he.le)
  obtain ⟨R, hR, htail⟩ := phi_small_of_abs_ge (mul_pos hδ he)
  let B := |x i| + a + S*R
  have hB : 0 < B := by dsimp [B]; have : 0 < S := zero_lt_one.trans_le hS; positivity
  let K := Icc (-B) B ×ˢ Icc δ S
  have hK : K ⊆ univ ×ˢ Ioi 0 := fun _ hp ↦ ⟨mem_univ _, hδ.trans_le hp.2.1⟩
  have hstart : ((0 : ℝ), (1 : ℝ)) ∈ K :=
    ⟨⟨by linarith, hB.le⟩, ⟨min_le_left _ _, hS⟩⟩
  obtain ⟨p, hp, hmax⟩ := (isCompact_Icc.prod isCompact_Icc : IsCompact K).exists_isMaxOn
    ⟨(0, 1), hstart⟩ ((likelihood_continuous_pos_width x ha).mono hK)
  refine ⟨p, hK hp, fun q hq ↦ ?_⟩
  by_cases hqK : q ∈ K
  · exact hmax hqK
  · change likelihood x a q.1 q.2 ≤ likelihood x a p.1 p.2
    have hbase : L ≤ likelihood x a p.1 p.2 := isMaxOn_iff.mp hmax (0, 1) hstart
    have hpos : 0 < q.2 := hq.2
    have hsmall : ∃ k, pointDensity a (x k - q.1) q.2 ≤ e := by
      by_cases hδq : δ ≤ q.2
      · by_cases hqS : q.2 ≤ S
        · have habs : B < |q.1| := by
            by_contra! hb
            exact hqK ⟨abs_le.mp hb, ⟨hδq, hqS⟩⟩
          have htri : |q.1| ≤ |x i - q.1| + |x i| := by
            simpa [abs_sub_comm q.1 (x i)] using abs_sub_le q.1 (x i) 0
          have hfar : a + S*R ≤ |x i - q.1| := by dsimp [B] at habs; linarith
          refine ⟨i, ?_⟩
          simpa only [mul_div_cancel_left₀ _ hδ.ne'] using
            density_far_centre ha.le hpos le_rfl hqS hδ hδq hR (mul_pos hδ he).le htail hfar
        · refine ⟨i, (density_bound_scale ha.le hpos).trans ?_⟩
          exact (div_le_iff₀ hpos).mpr (by
            have hh := (div_le_iff₀ he).mp
              (show φ 0 / e ≤ q.2 from le_trans (le_add_of_nonneg_left zero_le_one)
                (le_of_not_ge hqS))
            rwa [mul_comm] at hh)
      · have ht : q.2 * T ≤ d := by
          apply (le_div_iff₀ hT).mp
          exact (le_of_not_ge hδq).trans (min_le_right _ _)
        by_cases hm : (x i + x j)/2 ≤ q.1
        · have hz : ((x i - q.1 + a) / q.2) ≤ U := by
            apply le_trans _ hTU
            apply (div_le_iff₀ hpos).mpr
            dsimp [d] at ht
            linarith
          refine ⟨i, ?_⟩
          simp only [pointDensity, ha.ne', ite_false]
          apply (div_le_iff₀ (show 0 < 2*a by positivity)).mpr
          have h := hU _ hz
          change Φ _ < 2*a*e at h
          nlinarith [cdf_pos ((x i - q.1 - a)/q.2)]
        · have hz : V ≤ ((x j - q.1 - a) / q.2) := by
            apply hTV.trans
            apply (le_div_iff₀ hpos).mpr
            dsimp [d] at ht
            linarith
          refine ⟨j, ?_⟩
          simp only [pointDensity, ha.ne', ite_false]
          apply (div_le_iff₀ (show 0 < 2*a by positivity)).mpr
          have h := hV _ hz
          change 1 - 2*a*e < Φ _ at h
          nlinarith [cdf_lt_one ((x j - q.1 + a)/q.2)]
    obtain ⟨k, hk⟩ := hsmall
    have hcut : likelihood x a q.1 q.2 ≤ L := small_likelihood_of_one x
      (m := q.1) (M := M) ha.le hpos
      (fun j ↦ (density_bound_width (r := x j - q.1) (s := q.2) ha).le) k hk hM
    exact hcut.trans hbase

/-- Joint uniform-boundary trace. The lower endpoint receives weight
`w` and the upper endpoint receives the complementary weight `1 - w`. -/
def uniformTrace {n : ℕ} (x : Fin n → ℝ) (a c w : ℝ) : ℝ :=
  ∏ i, ((if x i-c+a = 0 then w else 1) -
    (if x i-c-a = 0 then w else 0)) / (2*a)

private theorem moving_centre_limit {n : ℕ} {x : Fin n → ℝ} {a c : ℝ}
    (ha : 0 < a) (hx : ∀ i, |x i - c| ≤ a) (b : ℝ) :
    Tendsto (fun t : ℝ ↦ likelihood x a (c + b/t) t⁻¹) atTop
      (𝓝 (uniformTrace x a c (Φ (-b)))) := by
  have he : ∀ᶠ t : ℝ in atTop, likelihood x a (c + b/t) t⁻¹ =
      ∏ i, (Φ ((x i-c+a)*t-b) - Φ ((x i-c-a)*t-b)) / (2*a) := by
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
    rw [likelihood_pos_width x ha, coveredLik]
    apply Finset.prod_congr rfl
    intro i _
    have hplus : (x i - (c+b/t) + a) / t⁻¹ = (x i-c+a)*t-b := by field_simp; ring
    have hminus : (x i - (c+b/t) - a) / t⁻¹ = (x i-c-a)*t-b := by field_simp; ring
    rw [hplus, hminus]
  refine Tendsto.congr' (Filter.EventuallyEq.symm (f := fun t ↦ likelihood x a (c+b/t) t⁻¹) he)
    (tendsto_finsetProd Finset.univ fun i _ ↦ ?_)
  have hb := abs_le.mp (hx i)
  have hright : Tendsto (fun t : ℝ ↦ Φ ((x i-c+a)*t-b)) atTop
      (𝓝 (if x i-c+a = 0 then Φ (-b) else 1)) := by
    split_ifs with h
    · simp [h]
    · have hp : 0 < x i-c+a := lt_of_le_of_ne (by linarith) (Ne.symm h)
      simpa [Function.comp_def, sub_eq_add_neg] using cdf_atTop.comp
        (tendsto_atTop_add_const_right atTop (-b) (tendsto_id.const_mul_atTop hp))
  have hleft : Tendsto (fun t : ℝ ↦ Φ ((x i-c-a)*t-b)) atTop
      (𝓝 (if x i-c-a = 0 then Φ (-b) else 0)) := by
    split_ifs with h
    · simp [h]
    · have hp : x i-c-a < 0 := lt_of_le_of_ne (by linarith) h
      simpa [Function.comp_def, sub_eq_add_neg] using cdf_atBot.comp
        (tendsto_atBot_add_const_right atTop (-b) (tendsto_id.const_mul_atTop_of_neg hp))
  exact (hright.sub hleft).div_const _
private theorem finite_lt_trace {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {a c m s : ℝ} (ha : 0 < a) (hs : 0 < s) :
    likelihood x a m s < uniformTrace x a c (Φ (-((m-c)/s))) := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  let b := (m-c)/s
  change (∏ i, pointDensity a (x i-m) s) < _
  refine Finset.prod_lt_prod_of_nonempty₀ (fun i _ ↦ density_pos ha.le hs)
    (fun i _ ↦ ?_) Finset.univ_nonempty
  simp only [pointDensity, ha.ne', ite_false]
  apply (div_lt_div_iff_of_pos_right (show 0 < 2*a by positivity)).mpr
  have hp (h : x i-c+a = 0) : (x i-m+a)/s = -b := by dsimp [b]; rw [← neg_div]; congr 1; linarith
  have hm (h : x i-c-a = 0) : (x i-m-a)/s = -b := by dsimp [b]; rw [← neg_div]; congr 1; linarith
  change _ < (if x i-c+a = 0 then Φ (-b) else 1) -
    (if x i-c-a = 0 then Φ (-b) else 0)
  by_cases h₁ : x i-c+a = 0 <;> by_cases h₂ : x i-c-a = 0
  · linarith
  · simp only [h₁, h₂, ite_true, ite_false, hp h₁]
    linarith [cdf_pos ((x i-m-a)/s)]
  · simp only [h₁, h₂, ite_true, ite_false, hm h₂]
    linarith [cdf_lt_one ((x i-m+a)/s)]
  · simp only [h₁, h₂, ite_false]
    linarith [cdf_lt_one ((x i-m+a)/s), cdf_pos ((x i-m-a)/s)]

private theorem cdf_value {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) : ∃ t, Φ t = w := by
  obtain ⟨u, hu⟩ := (cdf_atBot.eventually (eventually_lt_nhds hw.1)).exists
  obtain ⟨v, hv⟩ := (cdf_atTop.eventually (eventually_gt_nhds hw.2)).exists
  have hle : u ≤ v := le_of_not_gt (fun h ↦ (cdf_strictMono h).not_ge (hu.trans hv).le)
  exact (intermediate_value_Icc hle cdf_continuous.continuousOn ⟨hu.le, hv.le⟩).imp
    fun _ h ↦ h.2

/-- At positive held width, any compatible centre gives the uniform
envelope by maximizing the joint complementary endpoint trace. No
finite fit reaches the envelope. -/
theorem coverage_uniform_envelope {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a c : ℝ} (ha : 0 < a) (hx : ∀ i, |x i - c| ≤ a) :
    ∃ w ∈ Icc (0 : ℝ) 1, IsMaxOn (uniformTrace x a c) (Icc 0 1) w ∧
      IsLUB ((fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) '' (univ ×ˢ Ioi 0))
        (uniformTrace x a c w) ∧
      ∀ m s : ℝ, 0 < s → likelihood x a m s < uniformTrace x a c w := by
  have hc : Continuous (uniformTrace x a c) := by
    unfold uniformTrace
    refine continuous_finsetProd Finset.univ fun i _ ↦ ?_
    by_cases h₁ : x i-c+a = 0 <;> by_cases h₂ : x i-c-a = 0 <;>
      simp only [h₁, h₂, ite_true, ite_false] <;> fun_prop
  obtain ⟨w, hw, hm⟩ := isCompact_Icc.exists_isMaxOn
    (show (Icc (0 : ℝ) 1).Nonempty from ⟨0, by norm_num⟩) hc.continuousOn
  have hlt (m s : ℝ) (hs : 0 < s) : likelihood x a m s < uniformTrace x a c w :=
    (finite_lt_trace hn x ha hs).trans_le
      (hm ⟨(cdf_pos _).le, (cdf_lt_one _).le⟩)
  refine ⟨w, hw, hm, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hlt p.1 p.2 hp.2).le
  · intro z hz
    have hupper : Ioo (0 : ℝ) 1 ⊆ {v | uniformTrace x a c v ≤ z} := by
      intro v hv
      obtain ⟨b, hb⟩ := cdf_value hv
      have ht := moving_centre_limit ha hx (-b)
      rw [neg_neg, hb] at ht
      exact le_of_tendsto ht ((eventually_gt_atTop (0 : ℝ)).mono fun t ht ↦
        hz ⟨(c+(-b)/t,t⁻¹), ⟨mem_univ _, inv_pos.mpr ht⟩, rfl⟩)
    have hcl := closure_minimal hupper (isClosed_le hc continuous_const)
    rw [closure_Ioo zero_ne_one] at hcl
    exact hcl hw

private theorem no_max_of_coverage {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {a c : ℝ} (ha : 0 < a) (hx : ∀ i, |x i - c| ≤ a) :
    ¬ ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) (univ ×ˢ Ioi 0) p := by
  rintro ⟨p, hp, hm⟩
  obtain ⟨w, _, _, hlub, hlt⟩ := coverage_uniform_envelope hn ha hx
  exact (hlt p.1 p.2 hp.2).not_ge
    (hlub.2 (fun _ ⟨q, hq, he⟩ ↦ he ▸ (isMaxOn_iff.mp hm q hq)))

/-- At held positive width, a finite maximum exists exactly when the sample
range exceeds twice that width. The nonattainment direction includes equality. -/
theorem positive_width_iff {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun p : ℝ × ℝ ↦ likelihood x a p.1 p.2) (univ ×ˢ Ioi 0) p) ↔
      ∃ i j, 2*a < x j - x i := by
  constructor
  · intro h
    by_contra! hgap
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x Finset.univ_nonempty
    obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x Finset.univ_nonempty
    let c := (x i + x j)/2
    have hx (k : Fin n) : |x k - c| ≤ a := by
      have hlo := hi k (Finset.mem_univ k)
      have hhi := hj k (Finset.mem_univ k)
      exact abs_le.mpr (by dsimp [c]; constructor <;> linarith [hgap i j])
    exact no_max_of_coverage hn ha hx h
  · rintro ⟨i, j, h⟩
    exact positive_width_attainment x ha h

/-- The attained zero-width seam makes the actual constant-sample
likelihood unbounded on a nonempty sample. -/
theorem constant_normal_seam {n : ℕ} (hn : 0 < n) (m : ℝ) :
    ¬ BddAbove (likelihood (fun _ : Fin n ↦ m) 0 m '' Ioi 0) := by
  rintro ⟨b, hb⟩
  let k := max 1 b + 1
  have hk : 1 ≤ k := by dsimp [k]; linarith [le_max_left (1 : ℝ) b]
  have hkpos : 0 < k := zero_lt_one.trans_le hk
  let s := φ 0 / k
  have hs : 0 < s := div_pos (phi_pos _) hkpos
  have h := hb (mem_image_of_mem _ (show s ∈ Ioi (0 : ℝ) from hs))
  change likelihood (fun _ : Fin n ↦ m) 0 m s ≤ b at h
  have he : φ 0 / s = k := by dsimp [s]; field_simp [(phi_pos 0).ne']
  simp only [likelihood, ite_true, sub_self, zero_div, he, Finset.prod_const,
    Finset.card_univ, Fintype.card_fin] at h
  have hb' : b < k := by dsimp [k]; linarith [le_max_right (1 : ℝ) b]
  have hp : k ≤ k^n := by
    obtain ⟨d, rfl⟩ := Nat.exists_eq_succ_of_ne_zero hn.ne'
    rw [pow_succ]
    exact le_mul_of_one_le_left hkpos.le (one_le_pow₀ hk)
  exact hb'.not_ge (hp.trans h)

end

end UnivariateML.SkewLSBhatt
