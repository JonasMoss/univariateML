module

public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Convex.Jensen
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv
public import Mathlib.Analysis.SpecialFunctions.Stirling

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Gamma likelihood

The coefficients are positive shape and rate. The likelihood is written per
observation, which preserves the maximizers of the total for a nonempty sample.

## Deviations

The dossier and public fitter use scale, the reciprocal of the rate here.
The constant-sample likelihood diverges along the fixed-mean path, as the
dossier states. This corrects the boundedness premise in target 5.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Gamma

noncomputable section

variable {n : ℕ}

def avg (x : Fin n → ℝ) : ℝ := (∑ i, x i) / n

def avgLog (x : Fin n → ℝ) : ℝ := avg (fun i ↦ log (x i))

def jensenGap (x : Fin n → ℝ) : ℝ := log (avg x) - avgLog x

def logDensity (k β t : ℝ) : ℝ :=
  k * log β - log (Real.Gamma k) + (k - 1) * log t - β * t

def logLik (x : Fin n → ℝ) (k β : ℝ) : ℝ :=
  k * log β - log (Real.Gamma k) + (k - 1) * avgLog x - β * avg x

def rate (x : Fin n → ℝ) (k : ℝ) : ℝ := k / avg x

def profile (x : Fin n → ℝ) (k : ℝ) : ℝ := logLik x k (rate x k)

def digamma (k : ℝ) : ℝ := deriv (log ∘ Real.Gamma) k

def scoreGap (k : ℝ) : ℝ := log k - digamma k

def domain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem avg_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 < avg x := by
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  exact div_pos (Finset.sum_pos (fun i _ ↦ hx i) Finset.univ_nonempty) (by exact_mod_cast hn)

/-- The objective is the mean of the sample log-densities. -/
theorem logLik_eq_avg (hn : 0 < n) (x : Fin n → ℝ) (k β : ℝ) :
    logLik x k β = avg (fun i ↦ logDensity k β (x i)) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp only [logLik, logDensity, avgLog, avg, Finset.sum_sub_distrib, Finset.sum_add_distrib,
    ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp

private theorem avg_eq_sum (x : Fin n → ℝ) :
    avg x = ∑ i, (n : ℝ)⁻¹ • x i := by
  simp [avg, smul_eq_mul, ← Finset.mul_sum, div_eq_inv_mul]

/-- Equal-weight strict Jensen inequality, including its equality case. -/
theorem jensenGap_spec (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    0 ≤ jensenGap x ∧ (jensenGap x = 0 ↔ ∀ i j, x i = x j) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hw : ∀ i ∈ (Finset.univ : Finset (Fin n)), 0 < (n : ℝ)⁻¹ := fun _ _ ↦ inv_pos.mpr hn'
  have hsum : ∑ _i : Fin n, (n : ℝ)⁻¹ = 1 := by simp [hn'.ne']
  have hmem : ∀ i ∈ Finset.univ, x i ∈ Ioi (0 : ℝ) := fun i _ ↦ hx i
  constructor
  · simpa only [jensenGap, sub_nonneg, avgLog, avg_eq_sum] using
      strictConcaveOn_log_Ioi.concaveOn.le_map_sum (fun i hi ↦ (hw i hi).le) hsum hmem
  · rw [jensenGap, sub_eq_zero]
    have h := strictConcaveOn_log_Ioi.map_sum_eq_iff_of_pos hw hsum hmem
    have he : (log (avg x) = avgLog x) ↔ ∀ ⦃i j⦄, x i = x j := by
      simpa only [avgLog, avg_eq_sum, Finset.mem_univ, true_implies] using h
    exact ⟨fun h i j ↦ he.mp h (i := i) (j := j), fun h ↦ he.mpr (fun {i j} ↦ h i j)⟩

/-- The unique maximizing rate for a held positive shape. -/
theorem heldShape (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    0 < rate x k ∧ ∀ β > 0, β ≠ rate x k → logLik x k β < profile x k := by
  have hm := avg_pos hn hx
  have hr : 0 < rate x k := div_pos hk hm
  have he : rate x k * avg x = k := div_mul_cancel₀ _ hm.ne'
  refine ⟨hr, fun β hβ hne ↦ ?_⟩
  have H := log_lt_sub_one_of_pos (div_pos hβ hr) (by
    intro h
    exact hne ((div_eq_iff hr.ne').mp h |>.trans (one_mul _)))
  have E : β / rate x k * k = β * avg x := by
    calc
      β / rate x k * k = β * (k / rate x k) := by ring
      _ = β * avg x := by
        have he' : k / rate x k = avg x :=
          (div_eq_iff hr.ne').mpr (he.symm.trans (mul_comm _ _))
        rw [he']
  rw [log_div hβ.ne' hr.ne'] at H
  have H' := mul_lt_mul_of_pos_right H hk
  unfold profile logLik
  linarith only [H', E, he, show (β / rate x k - 1) * k = β / rate x k * k - k by ring,
    show (log β - log (rate x k)) * k = k * log β - k * log (rate x k) by ring]

/-- The rate-profiled log-likelihood per observation. -/
theorem profile_eq (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) :
    profile x k = k * log k - k - log (Real.Gamma k) - k * jensenGap x - avgLog x := by
  have hm := avg_pos hn hx
  simp only [profile, logLik, rate, log_div hk.ne' hm.ne', div_mul_cancel₀ _ hm.ne', jensenGap]
  ring

private theorem hasDerivAt_logGamma {k : ℝ} (hk : 0 < k) :
    HasDerivAt (log ∘ Real.Gamma) (digamma k) k :=
  ((((hasDerivAt_Gamma_of_pos hk).log (Gamma_pos_of_pos hk).ne').differentiableAt)).hasDerivAt

/-- The derivative of the profile is the Gamma score gap minus the sample gap. -/
theorem hasDerivAt_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) : HasDerivAt (profile x) (scoreGap k - jensenGap x) k := by
  have h := ((((hasDerivAt_id k).mul (hasDerivAt_log hk.ne')).sub (hasDerivAt_id k)).sub
    (hasDerivAt_logGamma hk)).sub ((hasDerivAt_id k).mul_const (jensenGap x))
  have h' := h.sub_const (avgLog x)
  change HasDerivAt (fun t ↦ t * log t - t - log (Real.Gamma t) - t * jensenGap x - avgLog x)
    (1 * log k + k * k⁻¹ - 1 - digamma k - 1 * jensenGap x) k at h'
  have he : 1 * log k + k * k⁻¹ - 1 - digamma k - 1 * jensenGap x =
      scoreGap k - jensenGap x := by
    rw [mul_inv_cancel₀ hk.ne', scoreGap]
    ring
  rw [he] at h'
  apply h'.congr_of_eventuallyEq
  filter_upwards [Ioi_mem_nhds hk] with t ht
  exact profile_eq hn hx ht

private theorem logGamma_add_one {k : ℝ} (hk : 0 < k) :
    log (Real.Gamma (k + 1)) = log k + log (Real.Gamma k) := by
  rw [Real.Gamma_add_one hk.ne', log_mul hk.ne' (Gamma_pos_of_pos hk).ne']

private theorem digamma_add_one {k : ℝ} (hk : 0 < k) :
    digamma (k + 1) = k⁻¹ + digamma k := by
  have h := (hasDerivAt_logGamma (show 0 < k + 1 by positivity)).comp k
    ((hasDerivAt_id k).add_const 1)
  have h' := (hasDerivAt_log hk.ne').add (hasDerivAt_logGamma hk)
  have he : (fun t ↦ log t + (log ∘ Real.Gamma) t) =ᶠ[𝓝 k]
      (log ∘ Real.Gamma) ∘ (fun t ↦ t + 1) := by
    filter_upwards [Ioi_mem_nhds hk] with t ht
    exact (logGamma_add_one ht).symm
  simpa using h.unique (h'.congr_of_eventuallyEq he.symm)

private theorem digamma_le_log {k : ℝ} (hk : 0 < k) : digamma k ≤ log k := by
  have h := convexOn_log_Gamma.deriv_le_slope (show k ∈ Ioi (0 : ℝ) from hk)
    (show k + 1 ∈ Ioi (0 : ℝ) from show 0 < k + 1 by positivity) (lt_add_one k)
    (hasDerivAt_logGamma hk).differentiableAt
  simp only [slope_def_field, Function.comp_apply, logGamma_add_one hk] at h
  rw [show k + 1 - k = 1 by ring, div_one, add_sub_cancel_right] at h
  exact h

private theorem log_sub_one_le_digamma {k : ℝ} (hk : 1 < k) :
    log (k - 1) ≤ digamma k := by
  have hp : 0 < k - 1 := by linarith
  have h := convexOn_log_Gamma.slope_le_deriv (show k - 1 ∈ Ioi (0 : ℝ) from hp)
    (show k ∈ Ioi (0 : ℝ) from zero_lt_one.trans hk) (show k - 1 < k by linarith)
    (hasDerivAt_logGamma (by linarith)).differentiableAt
  have he := logGamma_add_one hp
  rw [sub_add_cancel] at he
  simp only [slope_def_field, Function.comp_apply] at h
  rw [he, show k - (k - 1) = 1 by ring, div_one, add_sub_cancel_right] at h
  exact h

/-- The Gamma score gap converges to zero at large shape. -/
theorem scoreGap_tendsto_atTop : Tendsto scoreGap atTop (𝓝 0) := by
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
    (tendsto_inv_atTop_zero.comp (tendsto_atTop_add_const_right atTop (-1) tendsto_id))
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
    exact sub_nonneg.mpr (digamma_le_log hk)
  · filter_upwards [eventually_gt_atTop (1 : ℝ)] with k hk
    have hp : 0 < k - 1 := by linarith
    have hk' : 0 < k := by linarith
    have H := log_le_sub_one_of_pos (div_pos hk' hp)
    rw [log_div hk'.ne' hp.ne'] at H
    have E : k / (k - 1) - 1 = (k - 1)⁻¹ := by field_simp; ring
    rw [E] at H
    change log k - digamma k ≤ (k + -1)⁻¹
    rw [← sub_eq_add_neg]
    linarith [log_sub_one_le_digamma hk]

private def gapStep (u : ℝ) : ℝ := u - log (1 + u)

private theorem gapStep_strictMono : StrictMonoOn gapStep (Ioi 0) := by
  have hd (u : ℝ) (hu : 0 < u) : HasDerivAt gapStep (1 - (1 + u)⁻¹) u := by
    have h := (hasDerivAt_id u).sub
      (((hasDerivAt_id u).const_add 1).log (show 1 + u ≠ 0 by positivity))
    change HasDerivAt gapStep (1 - 1 / (1 + u)) u at h
    simpa only [one_div] using h
  apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    (fun u hu ↦ (hd u hu).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro u hu
  rw [(hd u hu).deriv]
  have hup : 0 < 1 + u := by have : 0 < u := hu; positivity
  rw [← one_div, sub_pos, div_lt_iff₀ hup]
  have : 0 < u := hu
  linarith

private theorem scoreGap_add_one {k : ℝ} (hk : 0 < k) :
    scoreGap k = scoreGap (k + 1) + gapStep k⁻¹ := by
  have he : 1 + k⁻¹ = (k + 1) / k := by field_simp
  rw [scoreGap, scoreGap, digamma_add_one hk, gapStep, he,
    log_div (show k + 1 ≠ 0 by positivity) hk.ne']
  ring

/-- The Gamma score gap strictly decreases at positive shape. -/
theorem scoreGap_strictAnti : StrictAntiOn scoreGap (Ioi 0) := by
  intro a ha b hb hab
  have hstep (m : ℕ) :
      scoreGap (a + (m + 1 : ℕ)) - scoreGap (b + (m + 1 : ℕ)) <
        scoreGap (a + m) - scoreGap (b + m) := by
    have ha' : 0 < a + (m : ℝ) := add_pos_of_pos_of_nonneg ha (Nat.cast_nonneg _)
    have hb' : 0 < b + (m : ℝ) := add_pos_of_pos_of_nonneg hb (Nat.cast_nonneg _)
    have hs := gapStep_strictMono (inv_pos.mpr hb') (inv_pos.mpr ha')
      (inv_strictAnti₀ ha' (add_lt_add_left hab _))
    have hA := scoreGap_add_one ha'
    have hB := scoreGap_add_one hb'
    push_cast
    rw [← add_assoc a, ← add_assoc b]
    linarith only [hA, hB, hs]
  -- The shifted difference decreases to zero, so its first step gives strictness.
  let d : ℕ → ℝ := fun m ↦ scoreGap (a + m) - scoreGap (b + m)
  have hd : Antitone d := antitone_nat_of_succ_le (fun m ↦ (hstep m).le)
  have ht (c : ℝ) : Tendsto (fun m : ℕ ↦ scoreGap (c + m)) atTop (𝓝 0) :=
    scoreGap_tendsto_atTop.comp (tendsto_atTop_add_const_left atTop c tendsto_natCast_atTop_atTop)
  have ht' : Tendsto d atTop (𝓝 0) := by simpa [d] using (ht a).sub (ht b)
  have H : 0 ≤ d 1 := le_of_tendsto ht'
    (by filter_upwards [eventually_ge_atTop (1 : ℕ)] with m hm using hd hm)
  have H' : d 1 < d 0 := hstep 0
  have : 0 < d 0 := H.trans_lt H'
  simpa [d, sub_pos] using this

/-- The Gamma score gap diverges at shape zero from the right. -/
theorem scoreGap_tendsto_zero : Tendsto scoreGap (𝓝[>] 0) atTop := by
  have hlog : Tendsto (fun k : ℝ ↦ log k * k) (𝓝[>] 0) (𝓝 0) := by
    simpa using (tendsto_log_mul_rpow_nhdsGT_zero (r := 1) one_pos)
  have hlog' : Tendsto (fun k : ℝ ↦ log (k + 1)) (𝓝[>] 0) (𝓝 0) := by
    have h : ContinuousAt (fun k : ℝ ↦ log (k + 1)) 0 :=
      ((continuousAt_id (x := (0 : ℝ))).add_const (1 : ℝ)).log (by norm_num)
    simpa using h.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  have hnum : Tendsto (fun k : ℝ ↦ 1 + log k * k - log (k + 1) * k) (𝓝[>] 0) (𝓝 1) := by
    simpa using (hlog.const_add 1).sub
      (hlog'.mul (tendsto_id.mono_left nhdsWithin_le_nhds))
  have he : ∀ᶠ k : ℝ in 𝓝[>] 0, (1 / 2 : ℝ) < 1 + log k * k - log (k + 1) * k :=
    hnum.eventually (Ioi_mem_nhds (by norm_num))
  have hhalf := tendsto_inv_nhdsGT_zero.const_mul_atTop (by norm_num : (0 : ℝ) < 1 / 2)
  refine tendsto_atTop_mono' _ ?_ hhalf
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)), he] with k hk hn
  have hk' : 0 < k := hk
  have H := digamma_le_log (show 0 < k + 1 by positivity)
  have H' := digamma_add_one hk'
  rw [← div_eq_mul_inv, div_le_iff₀ hk']
  have E : k * k⁻¹ = 1 := mul_inv_cancel₀ hk'.ne'
  unfold scoreGap
  have H'' := congrArg (fun y : ℝ ↦ y * k) H'
  nlinarith only [hn, H'', E, mul_le_mul_of_nonneg_right H hk'.le]

private theorem scoreGap_pos {k : ℝ} (hk : 0 < k) : 0 < scoreGap k :=
  (sub_nonneg.mpr (digamma_le_log (show 0 < k + 1 by positivity))).trans_lt
    (scoreGap_strictAnti hk (show 0 < k + 1 by positivity) (lt_add_one k))

private theorem strictConcave_of_score {f g : ℝ → ℝ}
    (hd : ∀ k > 0, HasDerivAt f (g k) k) (hg : StrictAntiOn g (Ioi 0)) :
    StrictConcaveOn ℝ (Ioi 0) f := by
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Ioi _)
    (fun k hk ↦ (hd k hk).continuousAt.continuousWithinAt)
  rw [interior_Ioi]
  intro a ha b hb hab
  rw [(hd a ha).deriv, (hd b hb).deriv]
  exact hg ha hb hab

private theorem maximum_of_score {f g : ℝ → ℝ}
    (hd : ∀ k > 0, HasDerivAt f (g k) k) (hg : StrictAntiOn g (Ioi 0))
    (hpos : ∃ a > 0, 0 < g a) (hneg : ∃ b > 0, g b < 0) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) k := by
  obtain ⟨a, ha, hga⟩ := hpos
  obtain ⟨b, hb, hgb⟩ := hneg
  have hab : a ≤ b := by
    by_contra! h
    exact (not_lt_of_ge (hgb.le.trans hga.le)) (hg hb ha h)
  have hsub : Icc a b ⊆ Ioi (0 : ℝ) := fun _ hk ↦ ha.trans_le hk.1
  have hc : ContinuousOn f (Ioi 0) := fun k hk ↦ (hd k hk).continuousAt.continuousWithinAt
  obtain ⟨k, hk, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨a, le_rfl, hab⟩ (hc.mono hsub)
  have hmax : IsMaxOn f (Ioi 0) k := by
    intro t ht
    by_cases hta : t < a
    · have hmono : StrictMonoOn f (Ioc 0 a) := by
        apply strictMonoOn_of_deriv_pos (convex_Ioc _ _)
          (hc.mono (fun _ hs ↦ hs.1))
        rw [interior_Ioc]
        intro s hs
        rw [(hd s hs.1).deriv]
        exact hga.trans (hg hs.1 ha hs.2)
      exact (hmono ⟨ht, hta.le⟩ ⟨ha, le_rfl⟩ hta).le.trans (hm ⟨le_rfl, hab⟩)
    by_cases hbt : b < t
    · have hanti : StrictAntiOn f (Ici b) := by
        apply strictAntiOn_of_deriv_neg (convex_Ici _)
          (hc.mono (fun _ hs ↦ hb.trans_le hs))
        rw [interior_Ici]
        intro s hs
        have hs' : 0 < s := hb.trans hs
        rw [(hd s hs').deriv]
        exact (hg hb hs' hs).trans hgb
      exact (hanti (show b ∈ Ici b from le_rfl) hbt.le hbt).le.trans (hm ⟨hab, le_rfl⟩)
    exact hm ⟨le_of_not_gt hta, le_of_not_gt hbt⟩
  exact ⟨k, ⟨hsub hk, hmax⟩, fun t ht ↦
    (strictConcave_of_score hd hg).eq_of_isMaxOn ht.2 hmax ht.1 (hsub hk)⟩

/-- Nonconstant positive samples have a unique joint MLE in shape–rate order. -/
theorem jointMLE (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) :
    ∃! p : ℝ × ℝ, p ∈ domain ∧ IsMaxOn (fun q ↦ logLik x q.1 q.2) domain p := by
  have hs := jensenGap_spec hn hx
  have hs' : 0 < jensenGap x := by
    obtain ⟨i, j, hij⟩ := hne
    exact hs.1.lt_of_ne (fun h ↦ hij (hs.2.mp h.symm i j))
  let g : ℝ → ℝ := fun t ↦ scoreGap t - jensenGap x
  have hpos : ∃ a > 0, 0 < g a := by
    obtain ⟨a, ha, hga⟩ := ((show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
      (scoreGap_tendsto_zero.eventually (eventually_gt_atTop (jensenGap x)))).exists
    exact ⟨a, ha, sub_pos.mpr hga⟩
  have hneg : ∃ b > 0, g b < 0 := by
    obtain ⟨b, hb, hgb⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (scoreGap_tendsto_atTop.eventually (Iio_mem_nhds hs'))).exists
    exact ⟨b, hb, sub_neg.mpr hgb⟩
  obtain ⟨k, ⟨hk, hm⟩, hu⟩ := maximum_of_score (g := g)
    (fun _ ht ↦ hasDerivAt_profile hn hx ht)
    (fun _ ha _ hb hab ↦ sub_lt_sub_right (scoreGap_strictAnti ha hb hab) _) hpos hneg
  have hrate (t : ℝ) (ht : 0 < t) := heldShape hn hx ht
  have hle (q : ℝ × ℝ) (hq : q ∈ domain) : logLik x q.1 q.2 ≤ profile x q.1 := by
    by_cases h : q.2 = rate x q.1
    · simp [h, profile]
    · exact ((hrate q.1 hq.1).2 q.2 hq.2 h).le
  have hp : (k, rate x k) ∈ domain := ⟨hk, (hrate k hk).1⟩
  refine ⟨(k, rate x k), ⟨hp, fun q hq ↦ (hle q hq).trans (hm hq.1)⟩, ?_⟩
  intro q ⟨hq, hmq⟩
  have hqr : q.2 = rate x q.1 := by
    by_contra h
    exact (not_lt_of_ge (hmq (show (q.1, rate x q.1) ∈ domain from ⟨hq.1, (hrate q.1 hq.1).1⟩)))
      ((hrate q.1 hq.1).2 q.2 hq.2 h)
  have hqm : IsMaxOn (profile x) (Ioi 0) q.1 := by
    intro t ht
    change logLik x t (rate x t) ≤ logLik x q.1 (rate x q.1)
    rw [← hqr]
    exact hmq (show (t, rate x t) ∈ domain from ⟨ht, (hrate t ht).1⟩)
  have hqk : q.1 = k := hu _ ⟨hq.1, hqm⟩
  exact Prod.ext hqk (hqr.trans (congrArg (rate x) hqk))

private theorem avg_const (hn : 0 < n) (c : ℝ) : avg (fun _ : Fin n ↦ c) = c := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [avg, mul_comm _ c, hn']

/-- Along the constant-sample fixed-mean path the log-likelihood diverges to positive infinity. -/
theorem constant_unbounded (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ logLik (fun _ : Fin n ↦ c) k (k / c)) atTop atTop := by
  let f := profile (fun _ : Fin n ↦ c)
  have hd (t : ℝ) (ht : 0 < t) : HasDerivAt f (scoreGap t) t := by
    simpa [f, jensenGap, avgLog, avg_const hn] using
      (hasDerivAt_profile hn (fun _ : Fin n ↦ hc) ht)
  have hmono : StrictMonoOn f (Ioi 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi _)
      (fun t ht ↦ (hd t ht).continuousAt.continuousWithinAt)
    rw [interior_Ioi]
    intro t ht
    rw [(hd t ht).deriv]
    exact scoreGap_pos ht
  have heq (m : ℕ) (hm : 0 < m) :
      f m = 1 / 2 * log m - (1 / 2 * log 2 + log (Stirling.stirlingSeq m) + log c) := by
    have hm' : (0 : ℝ) < m := by exact_mod_cast hm
    have hrec := logGamma_add_one hm'
    rw [Real.Gamma_nat_eq_factorial] at hrec
    have H := Stirling.log_stirlingSeq_formula m
    rw [log_mul (by norm_num : (2 : ℝ) ≠ 0) hm'.ne',
      log_div hm'.ne' (exp_pos _).ne', log_exp] at H
    change profile (fun _ : Fin n ↦ c) (m : ℝ) = _
    rw [profile_eq hn (fun _ ↦ hc) hm', jensenGap, avgLog, avg_const hn, avg_const hn]
    simp only [sub_self, mul_zero, sub_zero]
    linarith only [H, hrec]
  -- Factorial Stirling suffices along the integers because the real profile is increasing.
  have hseq : Tendsto (fun m : ℕ ↦ f m) atTop atTop := by
    have hlog := tendsto_log_atTop.comp (tendsto_natCast_atTop_atTop (R := ℝ))
    have hst := ((continuousAt_log (show √π ≠ 0 by positivity)).tendsto.comp
      Stirling.tendsto_stirlingSeq_sqrt_pi)
    have hfinite := ((hst.const_add (1 / 2 * log 2)).add_const (log c))
    refine ((hlog.const_mul_atTop (by norm_num : (0 : ℝ) < 1 / 2)).atTop_add hfinite.neg).congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℕ)] with m hm
    exact (heq m hm).symm.trans (sub_eq_add_neg _ _)
  have hf : Tendsto f atTop atTop := by
    refine tendsto_atTop_atTop.mpr (fun B ↦ ?_)
    obtain ⟨m, hm, hBm⟩ := ((eventually_gt_atTop (0 : ℕ)).and
      (hseq.eventually (eventually_ge_atTop B))).exists
    have hm' : (0 : ℝ) < m := by exact_mod_cast hm
    exact ⟨m, fun t ht ↦ hBm.trans (hmono.monotoneOn hm' (hm'.trans_le ht) ht)⟩
  change Tendsto (fun t ↦ profile (fun _ : Fin n ↦ c) t) atTop atTop at hf
  simpa only [profile, rate, avg_const hn] using hf

/-- The total likelihood itself diverges along the same path. -/
theorem constant_likelihood_tendsto (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun k ↦ exp ((n : ℝ) * logLik (fun _ : Fin n ↦ c) k (k / c))) atTop atTop :=
  tendsto_exp_atTop.comp ((constant_unbounded hn hc).const_mul_atTop (by exact_mod_cast hn))

/-- No positive shape and rate maximize the likelihood for a constant positive sample. -/
theorem constant_noMLE (hn : 0 < n) {c : ℝ} (hc : 0 < c) :
    ¬ ∃ p ∈ domain, IsMaxOn (fun q ↦ logLik (fun _ : Fin n ↦ c) q.1 q.2) domain p := by
  rintro ⟨p, _, hp⟩
  obtain ⟨t, ht, hlt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
    ((constant_unbounded hn hc).eventually
      (eventually_gt_atTop (logLik (fun _ : Fin n ↦ c) p.1 p.2)))).exists
  exact (not_lt_of_ge (hp (show (t, t / c) ∈ domain from ⟨ht, div_pos ht hc⟩))) hlt

private theorem hasDerivAt_heldRate (x : Fin n → ℝ) (β : ℝ) {k : ℝ} (hk : 0 < k) :
    HasDerivAt (fun t ↦ logLik x t β) (log β + avgLog x - digamma k) k := by
  unfold logLik
  convert (((((hasDerivAt_id k).mul_const (log β)).sub (hasDerivAt_logGamma hk)).add
    (((hasDerivAt_id k).sub_const 1).mul_const (avgLog x))).sub_const (β * avg x)) using 1
  · simp
  · ring

private theorem digamma_strictMono : StrictMonoOn digamma (Ioi 0) := by
  intro a ha b hb hab
  have h := scoreGap_strictAnti ha hb hab
  unfold scoreGap at h
  linarith [log_lt_log ha hab]

/-- The held-rate log-likelihood is strictly concave in shape. -/
theorem heldRate_strictConcave (x : Fin n → ℝ) (β : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (fun t ↦ logLik x t β) :=
  strictConcave_of_score (fun _ ht ↦ hasDerivAt_heldRate x β ht)
    (fun _ ha _ hb hab ↦ sub_lt_sub_left (digamma_strictMono ha hb hab) _)

/-- A held positive rate gives a unique maximizing shape, including for constant samples. -/
theorem heldRate (x : Fin n → ℝ) {β : ℝ} (_hβ : 0 < β) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLik x t β) (Ioi 0) k := by
  let d := log β + avgLog x
  have hpos : ∃ a > 0, 0 < d - digamma a := by
    have hbelow : ∀ᶠ t : ℝ in 𝓝[>] 0, t < 1 :=
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)
    obtain ⟨a, ha, ha1, hag⟩ := ((show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
      (hbelow.and (scoreGap_tendsto_zero.eventually (eventually_gt_atTop (-d))))).exists
    refine ⟨a, ha, ?_⟩
    unfold scoreGap at hag
    linarith [log_neg ha ha1]
  have hneg : ∃ b > 0, d - digamma b < 0 := by
    have hlog := tendsto_log_atTop.comp (tendsto_atTop_add_const_right atTop (-1) tendsto_id)
    obtain ⟨b, hb, hd⟩ := ((eventually_gt_atTop (1 : ℝ)).and
      (hlog.eventually (eventually_gt_atTop d))).exists
    refine ⟨b, zero_lt_one.trans hb, ?_⟩
    have hd' : d < log (b - 1) := hd
    linarith [log_sub_one_le_digamma hb]
  exact maximum_of_score (fun _ ht ↦ hasDerivAt_heldRate x β ht)
    (fun _ ha _ hb hab ↦ sub_lt_sub_left (digamma_strictMono ha hb hab) _) hpos hneg

end

end UnivariateML.Gamma
