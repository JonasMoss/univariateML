module

public import UnivariateML.Family.FinishSN
public import UnivariateML.Family.LocScaleCommon

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Held-coefficient skew-normal fits

## Deviations

The reflection theorem assumes a finite maximum of nonzero slant. The
written example establishes that premise with a rational enclosure of
the competing likelihood values. The fixed-scale theory here treats
constant samples; the full nonconstant comparison and exact reflected
example require the profiled half-normal comparison.
-/

@[expose] public section

open Set Filter Real
open scoped Topology
open UnivariateML.SkewLSSN UnivariateML.SkewLSGaussian

namespace UnivariateML.LocScaleSN

private theorem reflect_factor (α ξ ω t : ℝ) :
    density (-α) (-ξ) ω (-t) = density α ξ ω t := by
  have hz : (-t - -ξ) / ω = -((t - ξ) / ω) := by ring
  rw [density, hz, show φ (-((t - ξ) / ω)) = φ ((t - ξ) / ω) by simp [phi_formula]]
  simp [density]

private theorem reflect_value {n : ℕ} (x : Fin n → ℝ) (e : Fin n ≃ Fin n)
    (hx : ∀ j, x (e j) = -x j) (α ξ ω : ℝ) :
    logLik (-α) x (-ξ, ω) = logLik α x (ξ, ω) := by
  unfold logLik
  rw [← e.sum_comp (fun j ↦ log (density (-α) (-ξ) ω (x j)))]
  apply Finset.sum_congr rfl
  intro j _
  simp only [hx j, reflect_factor]

/-- If a symmetric sample has a nonzero-slant finite winner, reflection at
zero produces a distinct finite winner of the same height. -/
theorem reflected_winner {n : ℕ} (x : Fin n → ℝ) (e : Fin n ≃ Fin n)
    (hx : ∀ j, x (e j) = -x j) {α ξ ω : ℝ} (hα : α ≠ 0)
    (hm : IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLik q.1 x q.2)
      (univ ×ˢ LocationScale.domain) (α, ξ, ω)) :
    (α, ξ, ω) ≠ (-α, -ξ, ω) ∧
      IsMaxOn (fun q : ℝ × (ℝ × ℝ) ↦ logLik q.1 x q.2)
        (univ ×ˢ LocationScale.domain) (-α, -ξ, ω) := by
  refine ⟨fun h ↦ hα (by linarith [congrArg Prod.fst h]), ?_⟩
  rintro ⟨β, m, s⟩ hs
  change logLik β x (m,s) ≤ logLik (-α) x (-ξ,ω)
  have h := hm (show (-β,-m,s) ∈ univ ×ˢ LocationScale.domain from
    ⟨mem_univ _, mem_univ _, hs.2.2⟩)
  change logLik (-β) x (-m,s) ≤ logLik α x (ξ,ω) at h
  rw [reflect_value x e hx β m s] at h
  simpa only [reflect_value x e hx α ξ ω] using h

/-- At fixed slant, a constant sample has no finite fit and an unbounded
log likelihood. -/
theorem held_slant_constant_failure {n : ℕ} (hn : 0 < n) (m α : ℝ) :
    (¬ BddAbove (logLik α (fun _ : Fin n ↦ m) '' LocationScale.domain)) ∧
    ¬ ∃ p ∈ LocationScale.domain,
      IsMaxOn (logLik α (fun _ : Fin n ↦ m)) LocationScale.domain p := by
  refine ⟨constant_collapse α hn m, ?_⟩
  rintro ⟨p, hp, hm⟩
  exact constant_collapse α hn m ⟨_, (hm.isLUB hp).1⟩

private theorem log_density {α ξ ω t : ℝ} (hω : 0 < ω) :
    log (density α ξ ω t) =
      log (2 * φ 0 / ω) - ((t - ξ) / ω)^2 / 2 + log (Φ (α * ((t - ξ) / ω))) := by
  unfold density
  rw [log_mul (div_pos (mul_pos (by norm_num) (phi_pos _)) hω).ne' (cdf_pos _).ne']
  have he : 2 * φ ((t - ξ) / ω) / ω =
      (2 * φ 0 / ω) * exp (-(((t - ξ) / ω)^2 / 2)) := by
    simp [phi_formula]
    ring
  rw [he, log_mul (div_pos (mul_pos (by norm_num) (phi_pos _)) hω).ne' (exp_pos _).ne',
    log_exp]
  ring

private noncomputable def locationProfile {n : ℕ} (α ω : ℝ) (x : Fin n → ℝ) (ξ : ℝ) : ℝ :=
  ∑ i, (-((x i - ξ) / ω)^2 / 2 + log (Φ (α * ((x i - ξ) / ω))))

private theorem locationProfile_transfer {n : ℕ} (α : ℝ) (x : Fin n → ℝ) (ξ : ℝ)
    {ω : ℝ} (hω : 0 < ω) :
    logLik α x (ξ,ω) = n * log (2 * φ 0 / ω) + locationProfile α ω x ξ := by
  unfold logLik locationProfile
  have he (i : Fin n) : log (density α ξ ω (x i)) = log (2 * φ 0 / ω) +
      (-((x i - ξ) / ω)^2 / 2 + log (Φ (α * ((x i - ξ) / ω)))) := by
    rw [log_density hω]
    ring
  rw [Finset.sum_congr rfl (fun i _ ↦ he i), Finset.sum_add_distrib]
  simp [nsmul_eq_mul]

private theorem strictConcave_locationProfile {n : ℕ} (hn : 0 < n) (α : ℝ)
    {ω : ℝ} (hω : 0 < ω) (x : Fin n → ℝ) :
    StrictConcaveOn ℝ univ (locationProfile α ω x) := by
  refine ⟨convex_univ, ?_⟩
  intro p _ q _ hpq u v hu hv huv
  simp only [locationProfile, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  let P := (x i - p) / ω
  let Q := (x i - q) / ω
  have hne : P ≠ Q := by
    dsimp [P, Q]
    intro h
    have he := (div_left_inj₀ hω.ne').mp h
    exact hpq (by linarith)
  have he : (x i - (u * p + v * q)) / ω = u * P + v * Q := by
    dsimp [P, Q]
    rw [show v = 1 - u by linarith]
    ring
  rw [he]
  change u * (-P^2/2 + log (Φ (α*P))) + v * (-Q^2/2 + log (Φ (α*Q))) <
    -(u*P+v*Q)^2/2 + log (Φ (α*(u*P+v*Q)))
  rw [show α * (u*P+v*Q) = u*(α*P)+v*(α*Q) by ring]
  have hc := cdf_strictLogConcave.concaveOn.2 (mem_univ (α*P)) (mem_univ (α*Q))
    hu.le hv.le huv
  simp only [smul_eq_mul] at hc
  have hs : 0 < u * v * (P-Q)^2 := mul_pos (mul_pos hu hv) (sq_pos_of_ne_zero (sub_ne_zero.mpr hne))
  have hq : u*P^2 + v*Q^2 - (u*P+v*Q)^2 = u*v*(P-Q)^2 := by
    rw [show v = 1-u by linarith]
    ring
  linarith

private theorem locationProfile_attains {n : ℕ} (hn : 0 < n) (α : ℝ)
    {ω : ℝ} (hω : 0 < ω) (x : Fin n → ℝ) :
    ∃ ξ, IsMaxOn (locationProfile α ω x) univ ξ := by
  let f := locationProfile α ω x
  let i : Fin n := ⟨0, hn⟩
  have hterm (m : ℝ) (j : Fin n) :
      -((x j - m) / ω)^2 / 2 + log (Φ (α * ((x j - m) / ω))) ≤ 0 := by
    have h := log_nonpos (cdf_pos (α * ((x j - m) / ω))).le (cdf_lt_one _).le
    linarith [sq_nonneg ((x j - m) / ω)]
  have hb (m : ℝ) : f m ≤ -((x i - m) / ω)^2 / 2 := by
    unfold f locationProfile
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    linarith [Finset.sum_nonpos (s := Finset.univ.erase i) (fun j _ ↦ hterm m j),
      log_nonpos (cdf_pos (α * ((x i - m) / ω))).le (cdf_lt_one _).le]
  let R := |x i| + ω * (1 + sqrt (-2 * f 0))
  have hR : 0 ≤ R := by positivity
  have h0 : (0 : ℝ) ∈ Icc (-R) R := ⟨by linarith, hR⟩
  have hc : Continuous f := by
    unfold f locationProfile
    apply continuous_finsetSum Finset.univ
    intro j _
    have hC : Continuous (fun m : ℝ ↦ Φ (α * ((x j - m) / ω))) :=
      cdf_continuous.comp (by fun_prop)
    have hL := hC.log fun _ ↦ (cdf_pos _).ne'
    exact (show Continuous (fun m : ℝ ↦ -((x j - m) / ω)^2 / 2) by fun_prop).add hL
  obtain ⟨m, hm, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨0,h0⟩ hc.continuousOn
  refine ⟨m, fun q _ ↦ ?_⟩
  by_cases hq : q ∈ Icc (-R) R
  · exact hmax hq
  have hqR : R < |q| := lt_of_not_ge (fun h ↦ hq (abs_le.mp h))
  have hf : f 0 ≤ 0 := (hb 0).trans (by linarith [sq_nonneg ((x i - 0) / ω)])
  have hd : sqrt (-2 * f 0) < |x i - q| / ω := by
    apply (lt_div_iff₀ hω).mpr
    have htri := abs_sub_le q (x i) 0
    rw [sub_zero, sub_zero, abs_sub_comm q] at htri
    dsimp [R] at hqR
    nlinarith
  have hs : -2 * f 0 < ((x i - q) / ω)^2 := by
    have h := pow_lt_pow_left₀ hd (sqrt_nonneg _) (by decide : 2 ≠ 0)
    rw [sq_sqrt (by linarith), div_pow, sq_abs] at h
    simpa only [div_pow] using h
  exact ((hb q).trans (by linarith : -((x i-q)/ω)^2/2 ≤ f 0)).trans (hmax h0)

/-- With positive scale and finite slant held, every nonempty sample has
exactly one maximizing location. -/
theorem held_slant_scale_existsUnique {n : ℕ} (hn : 0 < n) (α : ℝ)
    {ω : ℝ} (hω : 0 < ω) (x : Fin n → ℝ) :
    ∃! ξ, IsMaxOn (fun m ↦ logLik α x (m,ω)) univ ξ := by
  have he (m : ℝ) : IsMaxOn (fun q ↦ logLik α x (q,ω)) univ m ↔
      IsMaxOn (locationProfile α ω x) univ m := by
    simp only [isMaxOn_iff, locationProfile_transfer α x _ hω, add_le_add_iff_left]
  obtain ⟨m, hm⟩ := locationProfile_attains hn α hω x
  refine ⟨m, (he m).mpr hm, ?_⟩
  intro q hq
  exact (strictConcave_locationProfile hn α hω x).eq_of_isMaxOn
    ((he q).mp hq) hm (mem_univ _) (mem_univ _)

private noncomputable def scaleProfile {n : ℕ} (x : Fin n → ℝ) (ξ α b : ℝ) : ℝ :=
  (n : ℝ) * log b + ∑ i, (-(b * (x i - ξ))^2 / 2 + log (Φ (α*b*(x i - ξ))))

private theorem scaleProfile_transfer {n : ℕ} (x : Fin n → ℝ) (ξ α : ℝ) {b : ℝ}
    (hb : 0 < b) : logLik α x (ξ,b⁻¹) = n * log (2 * φ 0) + scaleProfile x ξ α b := by
  unfold logLik scaleProfile
  have he (i : Fin n) : log (density α ξ b⁻¹ (x i)) =
      log (2 * φ 0) + log b + (-(b * (x i - ξ))^2 / 2 + log (Φ (α*b*(x i - ξ)))) := by
    rw [log_density (inv_pos.mpr hb), log_div (mul_pos (by norm_num) (phi_pos _)).ne'
      (inv_ne_zero hb.ne'), log_inv, div_inv_eq_mul]
    rw [show α * ((x i - ξ) * b) = α*b*(x i - ξ) by ring, mul_comm (x i - ξ) b]
    ring
  rw [Finset.sum_congr rfl (fun i _ ↦ he i), Finset.sum_add_distrib]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

private theorem strictConcave_scaleProfile {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (ξ α : ℝ) : StrictConcaveOn ℝ (Ioi 0) (scaleProfile x ξ α) := by
  refine ⟨convex_Ioi 0, ?_⟩
  intro p hp q hq hpq u v hu hv huv
  simp only [scaleProfile, smul_eq_mul]
  have hl := strictConcaveOn_log_Ioi.2 hp hq hpq hu hv huv
  change u * log p + v * log q < log (u * p + v * q) at hl
  have hs : u * (∑ i, (-(p * (x i - ξ))^2 / 2 + log (Φ (α*p*(x i - ξ))))) +
      v * (∑ i, (-(q * (x i - ξ))^2 / 2 + log (Φ (α*q*(x i - ξ))))) ≤
      ∑ i, (-((u*p+v*q) * (x i - ξ))^2 / 2 + log (Φ (α*(u*p+v*q)*(x i - ξ)))) := by
    simp only [Finset.mul_sum, ← Finset.sum_add_distrib]
    apply Finset.sum_le_sum
    intro i _
    let r := x i - ξ
    change u * (-(p*r)^2/2 + log (Φ (α*p*r))) + v * (-(q*r)^2/2 + log (Φ (α*q*r))) ≤
      -((u*p+v*q)*r)^2/2 + log (Φ (α*(u*p+v*q)*r))
    have he : α*(u*p+v*q)*r = u*(α*p*r)+v*(α*q*r) := by ring
    rw [he]
    have hc := cdf_strictLogConcave.concaveOn.2 (mem_univ (α*p*r)) (mem_univ (α*q*r))
      hu.le hv.le huv
    simp only [smul_eq_mul] at hc
    have hq' : u*(p*r)^2 + v*(q*r)^2 - ((u*p+v*q)*r)^2 = u*v*(p*r-q*r)^2 := by
      rw [show v = 1-u by linarith]
      ring
    linarith [mul_nonneg (mul_nonneg hu.le hv.le) (sq_nonneg (p*r-q*r))]
  have h := mul_lt_mul_of_pos_left hl (show (0 : ℝ) < n by exact_mod_cast hn)
  linarith

private theorem scaleProfile_attains {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (ξ α : ℝ) {i : Fin n} (hi : x i ≠ ξ) :
    ∃ b ∈ Ioi (0 : ℝ), IsMaxOn (scaleProfile x ξ α) (Ioi 0) b := by
  let f := scaleProfile x ξ α
  let c := (x i - ξ)^2 / 2
  have hc : 0 < c := div_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hi)) (by norm_num)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hterm (b : ℝ) (j : Fin n) : -(b * (x j - ξ))^2 / 2 + log (Φ (α*b*(x j - ξ))) ≤ 0 := by
    have h := log_nonpos (cdf_pos (α*b*(x j-ξ))).le (cdf_lt_one _).le
    linarith [sq_nonneg (b*(x j-ξ))]
  have hbound (b : ℝ) : f b ≤ (n : ℝ) * log b - c*b^2 := by
    unfold f scaleProfile
    rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i)]
    have h := Finset.sum_nonpos (s := Finset.univ.erase i) (fun j _ ↦ hterm b j)
    have hlog := log_nonpos (cdf_pos (α*b*(x i-ξ))).le (cdf_lt_one _).le
    dsimp [c]
    nlinarith
  let lo := min 1 (exp (f 1 / n))
  let hiB := max 1 ((n+1+|f 1|)/c)
  have hlo : 0 < lo := lt_min zero_lt_one (exp_pos _)
  have h1 : (1 : ℝ) ∈ Icc lo hiB := ⟨min_le_left _ _, le_max_left _ _⟩
  have hcont : ContinuousOn f (Ioi 0) := by
    unfold f scaleProfile
    apply ContinuousOn.add ((continuousOn_log.mono fun _ hb ↦ ne_of_gt hb).const_mul n)
    apply Continuous.continuousOn
    apply continuous_finsetSum Finset.univ
    intro j _
    have hC : Continuous (fun b : ℝ ↦ Φ (α*b*(x j-ξ))) := cdf_continuous.comp (by fun_prop)
    exact (show Continuous (fun b : ℝ ↦ -(b*(x j-ξ))^2/2) by fun_prop).add
      (hC.log fun _ ↦ (cdf_pos _).ne')
  have hsub : Icc lo hiB ⊆ Ioi 0 := fun _ h ↦ hlo.trans_le h.1
  obtain ⟨b,hb,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1,h1⟩ (hcont.mono hsub)
  refine ⟨b,hsub hb,fun q hq ↦ ?_⟩
  by_cases hqB : q ∈ Icc lo hiB
  · exact hm hqB
  have hout : f q ≤ f 1 := by
    by_cases hlow : q < lo
    · have hqexp : q ≤ exp (f 1 / n) := hlow.le.trans (min_le_right _ _)
      have hlog := (log_le_log_iff hq (exp_pos _)).mpr hqexp
      rw [log_exp] at hlog
      have h := (le_div_iff₀ hn').mp hlog
      linarith [hbound q, mul_nonneg hc.le (sq_nonneg q)]
    · have hqhi : hiB < q := lt_of_not_ge (fun h ↦ hqB ⟨le_of_not_gt hlow,h⟩)
      have hq1 : 1 ≤ q := (le_max_left _ _).trans hqhi.le
      have hqc : n+1+|f 1| ≤ c*q := by
        rw [mul_comm]
        exact (div_le_iff₀ hc).mp ((le_max_right _ _).trans hqhi.le)
      have hl := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hq) hn'.le
      have h := mul_le_mul_of_nonneg_right hqc (show 0 ≤ q from le_of_lt hq)
      have hAbs := mul_le_mul_of_nonneg_left hq1 (abs_nonneg (f 1))
      nlinarith [hbound q, neg_abs_le (f 1)]
  exact hout.trans (hm h1)

/-- For fixed slant and location, exactly one positive scale maximizes
as soon as a residual is nonzero. -/
theorem held_slant_location_existsUnique_of_nonzero {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (ξ α : ℝ) (hx : ∃ i, x i ≠ ξ) :
    ∃! ω, ω ∈ Ioi 0 ∧ IsMaxOn (fun s ↦ logLik α x (ξ,s)) (Ioi 0) ω := by
  obtain ⟨i, hi⟩ := hx
  obtain ⟨b, hb, hm⟩ := scaleProfile_attains hn x ξ α hi
  have he (u : ℝ) (hu : 0 < u) :
      IsMaxOn (fun s ↦ logLik α x (ξ,s)) (Ioi 0) u⁻¹ ↔
        IsMaxOn (scaleProfile x ξ α) (Ioi 0) u := by
    constructor
    · intro h q hq
      have hh := h (show q⁻¹ ∈ Ioi 0 from inv_pos.mpr hq)
      change logLik α x (ξ,q⁻¹) ≤ logLik α x (ξ,u⁻¹) at hh
      rwa [scaleProfile_transfer x ξ α hq, scaleProfile_transfer x ξ α hu, add_le_add_iff_left] at hh
    · intro h q hq
      have hh := h (show q⁻¹ ∈ Ioi 0 from inv_pos.mpr hq)
      change scaleProfile x ξ α q⁻¹ ≤ scaleProfile x ξ α u at hh
      change logLik α x (ξ,q) ≤ logLik α x (ξ,u⁻¹)
      have hqform := scaleProfile_transfer x ξ α (inv_pos.mpr hq)
      rw [inv_inv] at hqform
      rw [hqform, scaleProfile_transfer x ξ α hu]
      exact add_le_add_right hh _
  refine ⟨b⁻¹, ⟨inv_pos.mpr hb, (he b hb).mpr hm⟩, ?_⟩
  rintro w ⟨hw, hwmax⟩
  have hh := (he w⁻¹ (inv_pos.mpr hw)).mp (by simpa only [inv_inv] using hwmax)
  have h := (strictConcave_scaleProfile hn x ξ α).eq_of_isMaxOn hh hm (inv_pos.mpr hw) hb
  simpa only [inv_inv] using congrArg Inv.inv h

/-- With location and slant held, zero residuals cause unbounded scale collapse. -/
theorem held_slant_location_zero_residual_failure {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℝ) (ξ α : ℝ) (hx : ∀ i, x i = ξ) :
    ¬ BddAbove ((fun s ↦ logLik α x (ξ,s)) '' Ioi 0) := by
  rintro ⟨M, hM⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let b := exp ((M - n * log (2 * φ 0) - n * log (Φ 0)) / n + 1)
  have hb : 0 < b := exp_pos _
  have h := hM (mem_image_of_mem (fun s ↦ logLik α x (ξ,s))
    (show b⁻¹ ∈ Ioi 0 from inv_pos.mpr hb))
  change logLik α x (ξ,b⁻¹) ≤ M at h
  rw [scaleProfile_transfer x ξ α hb] at h
  simp only [scaleProfile, hx, sub_self, mul_zero, zero_pow (by decide : 2 ≠ 0),
    neg_zero, zero_div, zero_add, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, b, log_exp] at h
  have he : (n : ℝ) * ((M - n * log (2 * φ 0) - n * log (Φ 0)) / n) =
      M - n * log (2 * φ 0) - n * log (Φ 0) := mul_div_cancel₀ _ hn'.ne'
  linarith

/-- With slant and location held, scale attains exactly when a residual is
nonzero. The maximum is unique throughout that branch. -/
theorem held_slant_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ α : ℝ) :
    (∃ ω ∈ Ioi 0, IsMaxOn (fun s ↦ logLik α x (ξ,s)) (Ioi 0) ω) ↔ ∃ i, x i ≠ ξ := by
  constructor
  · rintro ⟨s, hs, hm⟩
    by_contra! hx
    exact held_slant_location_zero_residual_failure hn x ξ α hx ⟨_, (hm.isLUB hs).1⟩
  · intro hx
    obtain ⟨s, ⟨hs, hm⟩, _⟩ := held_slant_location_existsUnique_of_nonzero hn x ξ α hx
    exact ⟨s, hs, hm⟩

private noncomputable def slantProfile {n : ℕ} (r : Fin n → ℝ) (β : ℝ) : ℝ :=
  ∑ i, log (Φ (β * r i))

private theorem slant_continuous {n : ℕ} (r : Fin n → ℝ) : Continuous (slantProfile r) := by
  unfold slantProfile
  apply continuous_finsetSum Finset.univ
  intro j _
  exact (cdf_continuous.comp (continuous_id.mul continuous_const)).log
    (fun β ↦ (cdf_pos (β * r j)).ne')

private theorem slant_strictConcave {n : ℕ} (r : Fin n → ℝ) {j : Fin n} (hj : r j ≠ 0) :
    StrictConcaveOn ℝ univ (slantProfile r) := by
  refine ⟨convex_univ, ?_⟩
  intro p _ q _ hpq u v hu hv huv
  simp only [slantProfile, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  have harg (i : Fin n) : (u*p+v*q)*r i = u*(p*r i)+v*(q*r i) := by ring
  simp only [harg]
  apply Finset.sum_lt_sum _ ⟨j, Finset.mem_univ _, ?_⟩
  · intro i _
    exact cdf_strictLogConcave.concaveOn.2 (mem_univ (p*r i)) (mem_univ (q*r i)) hu.le hv.le huv
  · exact cdf_strictLogConcave.2 (mem_univ (p*r j)) (mem_univ (q*r j))
      (fun h ↦ hpq (mul_right_cancel₀ hj h)) hu hv huv

private theorem slant_le_term {n : ℕ} (r : Fin n → ℝ) (β : ℝ) (j : Fin n) :
    slantProfile r β ≤ log (Φ (β * r j)) := by
  unfold slantProfile
  rw [← Finset.sum_erase_add _ _ (Finset.mem_univ j)]
  exact add_le_of_nonpos_left (Finset.sum_nonpos (fun _ _ ↦ log_nonpos (cdf_pos _).le (cdf_lt_one _).le))

private theorem slant_existsUnique_of_signs {n : ℕ} (r : Fin n → ℝ)
    (hpos : ∃ j, 0 < r j) (hneg : ∃ j, r j < 0) :
    ∃! β, IsMaxOn (slantProfile r) univ β := by
  obtain ⟨i, hi⟩ := hpos
  obtain ⟨j, hj⟩ := hneg
  have htail {l : Filter ℝ} (k : Fin n)
      (h : Tendsto (fun β : ℝ ↦ β * r k) l atBot) :
      Tendsto (slantProfile r) l atBot := by
    have ht : Tendsto (fun β ↦ Φ (β * r k)) l (𝓝[>] 0) :=
      tendsto_nhdsWithin_of_tendsto_nhds_of_eventually_within _ (cdf_atBot.comp h)
        (Eventually.of_forall fun β ↦ cdf_pos (β * r k))
    exact tendsto_atBot_mono (fun β ↦ slant_le_term r β k) (tendsto_log_nhdsGT_zero.comp ht)
  have hlo := htail i (tendsto_id.atBot_mul_const hi)
  have hhi := htail j (tendsto_id.atTop_mul_const_of_neg hj)
  have hco : Tendsto (slantProfile r) (cocompact ℝ) atBot := by
    rw [cocompact_eq_atBot_atTop, tendsto_sup]
    exact ⟨hlo,hhi⟩
  have hstrict := slant_strictConcave r hi.ne'
  obtain ⟨β, _, hm⟩ := Generic.exists_isMaxOn_of_tendsto_cocompact
    (D := (univ : Set ℝ)) univ_nonempty (slant_continuous r).continuousOn
    (by
      have h : Tendsto (Subtype.val : ↥(univ : Set ℝ) → ℝ) (cocompact ↥(univ : Set ℝ)) (cocompact ℝ) :=
        isClosed_univ.isClosedEmbedding_subtypeVal.tendsto_cocompact
      exact hco.comp h)
  exact ⟨β, hm, fun q hq ↦ hstrict.eq_of_isMaxOn hq hm (mem_univ _) (mem_univ _)⟩

private theorem slant_strictMono_of_nonneg {n : ℕ} (r : Fin n → ℝ)
    (h : ∀ j, 0 ≤ r j) {k : Fin n} (hk : 0 < r k) : StrictMono (slantProfile r) := by
  intro p q hpq
  unfold slantProfile
  exact Finset.sum_lt_sum (fun j _ ↦ log_le_log (cdf_pos _)
    (cdf_strictMono.monotone (mul_le_mul_of_nonneg_right hpq.le (h j))))
    ⟨k, Finset.mem_univ _, log_lt_log (cdf_pos _)
      (cdf_strictMono (mul_lt_mul_of_pos_right hpq hk))⟩

private theorem slant_reflect {n : ℕ} (r : Fin n → ℝ) (β : ℝ) :
    slantProfile (fun j ↦ -r j) (-β) = slantProfile r β := by
  simp [slantProfile]

private theorem slant_maximizers {n : ℕ} (r : Fin n → ℝ) :
    ((∃ β, IsMaxOn (slantProfile r) univ β) ↔
      ((∃ j, 0 < r j) ∧ (∃ j, r j < 0)) ∨ (∀ j, r j = 0)) ∧
    (((∃ j, 0 < r j) ∧ (∃ j, r j < 0)) → ∃! β, IsMaxOn (slantProfile r) univ β) ∧
    ((∀ j, r j = 0) → ∀ β, IsMaxOn (slantProfile r) univ β) := by
  have hboth := fun h : (∃ j, 0 < r j) ∧ (∃ j, r j < 0) ↦ slant_existsUnique_of_signs r h.1 h.2
  have hall (h : ∀ j, r j = 0) (β : ℝ) : IsMaxOn (slantProfile r) univ β := by
    intro q _
    change slantProfile r q ≤ slantProfile r β
    simp [slantProfile, h]
  refine ⟨⟨?_, ?_⟩, hboth, hall⟩
  · rintro ⟨β, hm⟩
    by_cases hpos : ∃ j, 0 < r j
    · by_cases hneg : ∃ j, r j < 0
      · exact Or.inl ⟨hpos,hneg⟩
      · obtain ⟨k,hk⟩ := hpos
        have hinc := slant_strictMono_of_nonneg r (fun j ↦ le_of_not_gt (fun h ↦ hneg ⟨j,h⟩)) hk
        exact False.elim ((hinc (lt_add_one β)).not_ge (hm (mem_univ (β+1))))
    · by_cases hneg : ∃ j, r j < 0
      · obtain ⟨k,hk⟩ := hneg
        have hinc := slant_strictMono_of_nonneg (fun j ↦ -r j)
          (fun j ↦ neg_nonneg.mpr (le_of_not_gt (fun h ↦ hpos ⟨j,h⟩))) (neg_pos.mpr hk)
        have h := hinc (show -β < -(β - 1) by linarith)
        rw [slant_reflect r β, slant_reflect r (β-1)] at h
        exact False.elim (h.not_ge (hm (mem_univ (β-1))))
      · exact Or.inr (fun j ↦ le_antisymm (le_of_not_gt (fun h ↦ hpos ⟨j,h⟩))
          (le_of_not_gt (fun h ↦ hneg ⟨j,h⟩)))
  · rintro (h | h)
    · obtain ⟨β,hβ,_⟩ := hboth h
      exact ⟨β,hβ⟩
    · exact ⟨0,hall h 0⟩

/-- With location and scale held, both strict residual signs give exactly
one maximizer and all-zero residuals give the whole slant line. A lone
strict residual sign precludes a finite maximum. -/
theorem held_location_scale_maximizers {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ)
    {ω : ℝ} (hω : 0 < ω) :
    let M := fun α ↦ IsMaxOn (fun β ↦ logLik β x (ξ,ω)) univ α
    let Signs := (∃ j, ξ < x j) ∧ (∃ j, x j < ξ)
    let Zero := ∀ j, x j = ξ
    ((∃ α, M α) ↔ Signs ∨ Zero) ∧ (Signs → ∃! α, M α) ∧ (Zero → ∀ α, M α) := by
  let r (j : Fin n) := (x j - ξ) / ω
  have hf (α : ℝ) : logLik α x (ξ,ω) =
      (∑ j, (log (2*φ 0/ω) - r j ^ 2/2)) + slantProfile r α := by
    unfold logLik slantProfile
    rw [← Finset.sum_add_distrib]
    exact Finset.sum_congr rfl (fun _ _ ↦ log_density hω)
  have hm (α : ℝ) : IsMaxOn (fun β ↦ logLik β x (ξ,ω)) univ α ↔
      IsMaxOn (slantProfile r) univ α := by
    simp only [isMaxOn_iff, hf, add_le_add_iff_left]
  simpa only [hm, r, div_pos_iff_of_pos_right hω, sub_pos, div_lt_iff₀ hω, zero_mul,
    sub_neg, div_eq_zero_iff, hω.ne', or_false, sub_eq_zero] using slant_maximizers r

/-- One-sided skew-normal envelope at fixed location and scale. The
number of contact factors is determined by the data, including ties. -/
noncomputable def oneSidedEnvelope {n : ℕ} (x : Fin n → ℝ) (ξ ω : ℝ) : ℝ :=
  ∏ j, (2 * φ ((x j - ξ) / ω) / ω) * (if x j = ξ then 1/2 else 1)

private theorem right_face_of_nonneg {n : ℕ} (x : Fin n → ℝ) {ξ ω : ℝ} (hω : 0 < ω)
    (hx : ∀ j, ξ ≤ x j) {i : Fin n} (hi : ξ < x i) :
    IsLUB (range (fun α : ℝ ↦ ∏ j, density α ξ ω (x j))) (oneSidedEnvelope x ξ ω) ∧
      ∀ α : ℝ, (∏ j, density α ξ ω (x j)) < oneSidedEnvelope x ξ ω := by
  have hterm (α : ℝ) (j : Fin n) : density α ξ ω (x j) ≤
      (2 * φ ((x j - ξ) / ω) / ω) * (if x j = ξ then 1/2 else 1) := by
    unfold density
    by_cases he : x j = ξ
    · simp [he, cdf_zero]
    · simp only [he, ite_false]
      exact mul_le_mul_of_nonneg_left (cdf_lt_one _).le
        (div_nonneg (mul_nonneg (by norm_num) (phi_pos _).le) hω.le)
  have hlt (α : ℝ) : (∏ j, density α ξ ω (x j)) < oneSidedEnvelope x ξ ω := by
    unfold oneSidedEnvelope
    have hp (j : Fin n) : 0 < density α ξ ω (x j) :=
      mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hω) (cdf_pos _)
    refine Finset.prod_lt_prod₀ (fun j _ ↦ hp j) (fun j _ ↦ hterm α j)
      ⟨i, Finset.mem_univ i, ?_⟩
    unfold density
    simp only [hi.ne', ite_false]
    exact mul_lt_mul_of_pos_left (cdf_lt_one _)
      (div_pos (mul_pos (by norm_num) (phi_pos _)) hω)
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨α, rfl⟩
    exact (hlt α).le
  · intro B hB
    have hl := half_normal_path x ξ hω
    have he : (∏ j, (2 * φ ((x j - ξ) / ω) / ω) *
        (if ξ < x j then 1 else if x j < ξ then 0 else 1/2)) =
        oneSidedEnvelope x ξ ω := by
      unfold oneSidedEnvelope
      apply Finset.prod_congr rfl
      intro j _
      rcases (hx j).eq_or_lt with h | h
      · simp [← h]
      · simp [h, h.ne']
    rw [he] at hl
    exact le_of_tendsto' hl fun α ↦ hB (mem_range_self α)

/-- Under either single strict residual sign, the held-location and
held-scale supremum is the compatible half-normal trace with half density
per contact. Every finite slant loses strictly. -/
theorem held_location_scale_one_sign_failure {n : ℕ} (x : Fin n → ℝ) {ξ ω : ℝ} (hω : 0 < ω)
    (hside : ((∀ j, ξ ≤ x j) ∧ ∃ j, ξ < x j) ∨ ((∀ j, x j ≤ ξ) ∧ ∃ j, x j < ξ)) :
    IsLUB (range (fun α : ℝ ↦ ∏ j, density α ξ ω (x j))) (oneSidedEnvelope x ξ ω) ∧
      ∀ α : ℝ, (∏ j, density α ξ ω (x j)) < oneSidedEnvelope x ξ ω := by
  rcases hside with ⟨h, i, hi⟩ | ⟨h, i, hi⟩
  · exact right_face_of_nonneg x hω h hi
  · let y (j : Fin n) := -x j
    have hR := right_face_of_nonneg y hω (fun j ↦ neg_le_neg (h j)) (neg_lt_neg hi)
    have he : oneSidedEnvelope y (-ξ) ω = oneSidedEnvelope x ξ ω := by
      unfold oneSidedEnvelope
      apply Finset.prod_congr rfl
      intro j _
      have hz : (y j - -ξ) / ω = -((x j - ξ) / ω) := by unfold y; ring
      rw [hz, show φ (-((x j - ξ) / ω)) = φ ((x j - ξ) / ω) by simp [phi_formula]]
      simp [y]
    have hr : range (fun α : ℝ ↦ ∏ j, density α (-ξ) ω (y j)) =
        range (fun α : ℝ ↦ ∏ j, density α ξ ω (x j)) := by
      ext z
      constructor <;> rintro ⟨α, rfl⟩
      · refine ⟨-α, ?_⟩
        simp [y, ← reflect_factor (-α) ξ ω, neg_neg]
      · exact ⟨-α, by simp [y, reflect_factor]⟩
    rw [he, hr] at hR
    exact ⟨hR.1, fun α ↦ by simpa [y, reflect_factor] using hR.2 (-α)⟩

private theorem held_location_decompose {n : ℕ} (x : Fin n → ℝ) (ξ α : ℝ)
    {ω : ℝ} (hω : 0 < ω) :
    logLik α x (ξ,ω) =
      Normal.logLik x ξ ω + n * log 2 + slantProfile (fun j ↦ x j - ξ) (α/ω) := by
  have hπ : 0 < 2 * π := by positivity
  have hp : 0 < φ 0 := phi_pos _
  have hconst : log (2 * φ 0 / ω) = log 2 - (log (2 * π)) / 2 - log ω := by
    rw [log_div (mul_pos (by norm_num) hp).ne' hω.ne', log_mul (by norm_num) hp.ne']
    simp only [phi_formula, zero_pow (by decide : 2 ≠ 0), zero_div, neg_zero, exp_zero, mul_one,
      one_div, log_inv, log_sqrt hπ.le, sub_eq_add_neg]
  rw [Normal.logLik_formula x ξ hω]
  unfold logLik slantProfile Normal.residual
  have he (j : Fin n) : log (density α ξ ω (x j)) =
      (log 2 - log (2 * π) / 2 - log ω) - (x j - ξ)^2 / (2 * ω^2) +
        log (Φ ((α/ω) * (x j - ξ))) := by
    rw [log_density hω, hconst, div_pow]
    rw [show α * ((x j - ξ)/ω) = (α/ω)*(x j-ξ) by ring]
    ring
  rw [Finset.sum_congr rfl (fun j _ ↦ he j), Finset.sum_add_distrib, Finset.sum_sub_distrib]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    div_eq_mul_inv, ← Finset.sum_mul]
  ring

private theorem held_location_separation {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ)
    (p : ℝ × ℝ) (hp : 0 < p.2) :
    IsMaxOn (fun q : ℝ × ℝ ↦ logLik q.1 x (ξ,q.2)) (univ ×ˢ Ioi 0) p ↔
      IsMaxOn (Normal.logLik x ξ) (Ioi 0) p.2 ∧
        IsMaxOn (slantProfile (fun j ↦ x j - ξ)) univ (p.1/p.2) := by
  let β := p.1/p.2
  have hf := held_location_decompose x ξ p.1 hp
  constructor
  · intro hm
    constructor
    · intro s hs
      have h := hm (show (β*s,s) ∈ (univ : Set ℝ) ×ˢ Ioi 0 from ⟨mem_univ _,hs⟩)
      change logLik (β*s) x (ξ,s) ≤ logLik p.1 x (ξ,p.2) at h
      rw [held_location_decompose x ξ (β*s) hs, mul_div_cancel_right₀ _ (ne_of_gt hs), hf] at h
      change Normal.logLik x ξ s ≤ Normal.logLik x ξ p.2
      linarith
    · intro t _
      have h := hm (show (t*p.2,p.2) ∈ (univ : Set ℝ) ×ˢ Ioi 0 from ⟨mem_univ _,hp⟩)
      change logLik (t*p.2) x (ξ,p.2) ≤ logLik p.1 x (ξ,p.2) at h
      rw [held_location_decompose x ξ (t*p.2) hp, mul_div_cancel_right₀ _ hp.ne', hf] at h
      change slantProfile (fun j ↦ x j - ξ) t ≤ slantProfile (fun j ↦ x j - ξ) β
      linarith
  · rintro ⟨hN,hα⟩ q hq
    change logLik q.1 x (ξ,q.2) ≤ logLik p.1 x (ξ,p.2)
    rw [held_location_decompose x ξ q.1 hq.2, hf]
    have hn := hN hq.2
    have ha := hα (mem_univ (q.1/q.2))
    exact add_le_add (add_le_add_left hn _) ha

/-- With location held, a finite fit exists exactly when both strict
residual signs occur. In that case the full fit is unique. -/
theorem held_location_criterion_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ : ℝ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let M := fun p : ℝ × ℝ ↦ p ∈ S ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik q.1 x (ξ,q.2)) S p
    let Signs := (∃ j, ξ < x j) ∧ (∃ j, x j < ξ)
    ((∃ p, M p) ↔ Signs) ∧ (Signs → ∃! p, M p) := by
  dsimp only
  let r (j : Fin n) := x j - ξ
  have hUnique (h : (∃ j, ξ < x j) ∧ (∃ j, x j < ξ)) :
      ∃! p : ℝ × ℝ, p ∈ (univ : Set ℝ) ×ˢ Ioi 0 ∧
        IsMaxOn (fun q : ℝ × ℝ ↦ logLik q.1 x (ξ,q.2)) (univ ×ˢ Ioi 0) p := by
    obtain ⟨i, hi⟩ := h.1
    have hne : ∃ j, x j ≠ ξ := ⟨i, hi.ne'⟩
    obtain ⟨s, hs, hsu⟩ := (Normal.existsUnique_heldMean_iff hn x ξ).mpr hne
    obtain ⟨β, hβ, hβu⟩ := slant_existsUnique_of_signs r
      (by simpa only [r, sub_pos] using h.1) (by simpa only [r, sub_neg] using h.2)
    refine ⟨(β*s,s), ⟨⟨mem_univ _, hs.1⟩, ?_⟩, ?_⟩
    · apply (held_location_separation x ξ (β*s,s) hs.1).mpr
      refine ⟨hs.2, ?_⟩
      change IsMaxOn (slantProfile r) univ (β*s/s)
      rwa [mul_div_cancel_right₀ _ (ne_of_gt hs.1)]
    · rintro ⟨a,w⟩ ⟨hp,hm⟩
      have hsep := (held_location_separation x ξ (a,w) hp.2).mp hm
      have he : w = s := hsu w ⟨hp.2,hsep.1⟩
      have hβe : a/w = β := hβu _ hsep.2
      exact Prod.ext (by
        have h := (div_eq_iff (ne_of_gt hp.2)).mp hβe
        simpa only [he] using h) he
  refine ⟨⟨?_, fun h ↦ ?_⟩, hUnique⟩
  · rintro ⟨p,hp,hm⟩
    have hs := (held_location_separation x ξ p hp.2).mp hm
    rcases (slant_maximizers r).1.mp ⟨_,hs.2⟩ with h | h
    · simpa only [r, sub_pos, sub_neg] using h
    · have hx (j : Fin n) : x j = ξ := sub_eq_zero.mp (h j)
      exact False.elim (Normal.heldMean_unbounded hn hx ⟨_, (hs.1.isLUB hp.2).1⟩)
  · obtain ⟨p,hp,_⟩ := hUnique h
    exact ⟨p,hp⟩

private theorem log_oneSidedEnvelope {n : ℕ} (x : Fin n → ℝ) (ξ : ℝ)
    {ω : ℝ} (hω : 0 < ω) :
    log (oneSidedEnvelope x ξ ω) =
      Normal.logLik x ξ ω + n * log 2 + ∑ j, log (if x j = ξ then (1/2 : ℝ) else 1) := by
  have hp (j : Fin n) : 0 < (if x j = ξ then (1/2 : ℝ) else 1) := by split_ifs <;> norm_num
  have he (j : Fin n) : log ((2 * φ ((x j - ξ)/ω) / ω) *
      (if x j = ξ then (1/2 : ℝ) else 1)) =
      log (density 0 ξ ω (x j)) - log (Φ 0) + log (if x j = ξ then (1/2 : ℝ) else 1) := by
    rw [log_mul (div_pos (mul_pos (by norm_num) (phi_pos _)) hω).ne' (hp j).ne']
    unfold density
    rw [log_mul (div_pos (mul_pos (by norm_num) (phi_pos _)) hω).ne' (cdf_pos _).ne']
    simp
  unfold oneSidedEnvelope
  rw [log_prod (fun j _ ↦ (mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hω) (hp j)).ne'),
    Finset.sum_congr rfl (fun j _ ↦ he j), Finset.sum_add_distrib, Finset.sum_sub_distrib]
  change logLik 0 x (ξ,ω) - (∑ _ : Fin n, log (Φ 0)) + _ = _
  rw [held_location_decompose x ξ 0 hω]
  simp [slantProfile]

/-- At held location with one strict residual sign, the compatible
half-normal trace is optimized at the residual RMS. All finite joint
fits lose strictly to that reachable bound. -/
theorem held_location_one_sign_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (ξ : ℝ)
    (hside : ((∀ j, ξ ≤ x j) ∧ ∃ j, ξ < x j) ∨ ((∀ j, x j ≤ ξ) ∧ ∃ j, x j < ξ)) :
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 ξ p.2 (x j)
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let B := oneSidedEnvelope x ξ (Normal.sd x ξ)
    IsLUB (L '' S) B ∧ ∀ p ∈ S, L p < B := by
  let L (p : ℝ × ℝ) := ∏ j, density p.1 ξ p.2 (x j)
  let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
  let w := Normal.sd x ξ
  let B := oneSidedEnvelope x ξ w
  change IsLUB (L '' S) B ∧ ∀ p ∈ S, L p < B
  have hx : ∃ j, x j ≠ ξ := by
    rcases hside with ⟨_,j,hj⟩ | ⟨_,j,hj⟩
    · exact ⟨j,hj.ne'⟩
    · exact ⟨j,hj.ne⟩
  have hw' := (Normal.heldMean hn hx w).mpr rfl
  have hw : 0 < w := hw'.1
  have hpos {s : ℝ} (hs : 0 < s) : 0 < oneSidedEnvelope x ξ s := by
    unfold oneSidedEnvelope
    exact Finset.prod_pos fun _ _ ↦ mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hs)
      (by split_ifs <;> norm_num)
  have hlt (p : ℝ × ℝ) (hp : p ∈ S) : L p < B := by
    refine ((held_location_scale_one_sign_failure x hp.2 hside).2 p.1).trans_le ?_
    apply (log_le_log_iff (hpos hp.2) (hpos hw)).mp
    rw [log_oneSidedEnvelope x ξ hp.2, log_oneSidedEnvelope x ξ hw]
    exact add_le_add_left (add_le_add_left (hw'.2 hp.2) _) _
  refine ⟨⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hlt p hp).le
  · intro z hz
    apply (held_location_scale_one_sign_failure x hw hside).1.2
    rintro _ ⟨a,rfl⟩
    change L (a,w) ≤ z
    exact hz (mem_image_of_mem L (show (a,w) ∈ S from ⟨mem_univ _,hw⟩))

/-- With location held at all observations, free scale permits collapse
even if the slant is held at any chosen finite value along the path. -/
theorem held_location_zero_residual_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    (ξ : ℝ) (hx : ∀ j, x j = ξ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik p.1 x (ξ,p.2)) '' ((univ : Set ℝ) ×ˢ Ioi (0 : ℝ))) := by
  let L (p : ℝ × ℝ) := logLik p.1 x (ξ,p.2)
  let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
  let slice (s : ℝ) := logLik 0 x (ξ,s)
  have hsub : slice '' Ioi 0 ⊆ L '' S := by
    rintro _ ⟨s,hs,rfl⟩
    refine ⟨(0,s), ?_, rfl⟩
    exact ⟨mem_univ _,hs⟩
  exact fun hb ↦ held_slant_location_zero_residual_failure hn x ξ 0 hx (hb.mono hsub)

/-- Constant observations at held scale give the same excluded height on
both moving half-normal faces, and every finite location and slant loses. -/
theorem held_scale_constant_faces {n : ℕ} (hn : 0 < n) (m : ℝ) {ω : ℝ} (hω : 0 < ω) :
    let B := (2 * φ 0 / ω)^n
    (∀ α ξ : ℝ, (∏ _ : Fin n, density α ξ ω m) < B) ∧
    Tendsto (fun t : ℝ ↦ ∏ _ : Fin n, density (t^2) (m-t⁻¹) ω m) atTop (𝓝 B) ∧
    Tendsto (fun t : ℝ ↦ ∏ _ : Fin n, density (-(t^2)) (m+t⁻¹) ω m) atTop (𝓝 B) := by
  let x : Fin n → ℝ := fun _ ↦ m
  have hB : halfNormalEnvelope x m ω = (2 * φ 0 / ω)^n := by
    simp only [halfNormalEnvelope, x, sub_self, zero_div, Finset.prod_const,
      Finset.card_univ, Fintype.card_fin]
  refine ⟨?_, ?_, ?_⟩
  · intro α ξ
    let : Nonempty (Fin n) := ⟨⟨0,hn⟩⟩
    rw [← hB]
    unfold halfNormalEnvelope
    simp only [x, sub_self, zero_div]
    apply Finset.prod_lt_prod_of_nonempty₀ _ _ Finset.univ_nonempty
    · exact fun _ _ ↦ mul_pos (div_pos (mul_pos (by norm_num) (phi_pos _)) hω) (cdf_pos _)
    · intro _ _
      have hφ (z : ℝ) : φ z ≤ φ 0 := by
        rw [phi_formula, phi_formula]
        simp only [zero_pow (by decide : 2 ≠ 0), zero_div, neg_zero, exp_zero, mul_one]
        exact mul_le_of_le_one_right (by positivity) (exp_le_one_iff.mpr (by linarith [sq_nonneg z]))
      unfold density
      exact (mul_lt_of_lt_one_right (div_pos (mul_pos (by norm_num) (phi_pos _)) hω)
        (cdf_lt_one _)).trans_le (div_le_div_of_nonneg_right (by gcongr; exact hφ _) hω.le)
  · simpa only [x, hB] using right_moving_half_normal x hω (c := m) (fun _ ↦ le_rfl)
  · simpa only [x, hB] using left_moving_half_normal x hω (c := m) (fun _ ↦ le_rfl)

end UnivariateML.LocScaleSN

end
