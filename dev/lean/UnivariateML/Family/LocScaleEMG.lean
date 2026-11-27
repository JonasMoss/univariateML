module

public import UnivariateML.Family.FinishEMG
public import UnivariateML.Family.LocScaleCommon

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# EMG location–scale likelihoods

## Deviations

The all-free co-winner reduction takes the competing profile windows and
their endpoint comparisons as hypotheses. Constructing the EMG samples
that satisfy those hypotheses needs the normal-end Taylor expansion, the
quantile moment chart, and a bound on the exterior of the two windows.
The held-face comparisons include ties and constant data. Stationary-list
finiteness and the remaining held uniqueness arguments are separate.
-/

@[expose] public section

open Set Filter
open scoped Topology
open UnivariateML.SkewLSEMG UnivariateML.FinishEMG UnivariateML.SkewLSGaussian

namespace UnivariateML.LocScaleEMG

/-- The scaled normal-end polynomial at the report’s exact crossing. -/
noncomputable def crossingPolynomial (v : ℝ) : ℝ :=
  (144 / 23) * v ^ 3 - (231 / 23) * v ^ 4 + (126 / 23) * v ^ 5 - v ^ 6

/-- The limiting scaled EMG polynomial has exactly two equal global peaks
on the nonnegative shape axis. -/
theorem crossing_polynomial_peaks {v : ℝ} (hv : 0 ≤ v) :
    crossingPolynomial v ≤ 16 / 23 ∧
      (crossingPolynomial v = 16 / 23 ↔ v = 1 ∨ v = 2) := by
  have he : (16 : ℝ) / 23 - crossingPolynomial v =
      (v - 1) ^ 2 * (v - 2) ^ 2 * (v ^ 2 + (12 / 23) * v + 4 / 23) := by
    unfold crossingPolynomial
    ring
  have hq : 0 < v ^ 2 + (12 / 23) * v + 4 / 23 := by positivity
  refine ⟨sub_nonneg.mp (he ▸ (by positivity : 0 ≤
    (v - 1) ^ 2 * (v - 2) ^ 2 * (v ^ 2 + (12 / 23) * v + 4 / 23))), ?_⟩
  rw [eq_comm, ← sub_eq_zero, he]
  simp only [mul_eq_zero, pow_eq_zero_iff (by decide : 2 ≠ 0),
    sub_eq_zero, hq.ne', or_false]

private theorem profile_two_winners_lift {n : ℕ} {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {i : Fin n} (hi : ∀ j, x i ≤ x j)
    {u v : ℝ} (hu : u ∈ Ioo 0 1) (hv : v ∈ Ioo 0 1) (huv : u ≠ v)
    (hU : IsMaxOn (compactProfile x i) (Icc 0 1) u)
    (hV : IsMaxOn (compactProfile x i) (Icc 0 1) v) :
    let S := Ioo (0 : ℝ) 1 ×ˢ LocationScale.domain
    let L := fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2
    ∃ p ∈ S, ∃ q ∈ S, p ≠ q ∧ IsMaxOn L S p ∧ IsMaxOn L S q := by
  have hprofile := (compactification hx i hi).2.2
  have hfit (t : ℝ) (ht : t ∈ Ioo 0 1) :
      ∃ a ∈ LocationScale.domain,
        compactProfile x i t = shapeLogLik x ((1-t)/t) a ∧
        ∀ b ∈ LocationScale.domain, shapeLogLik x ((1-t)/t) b ≤ compactProfile x i t := by
    obtain ⟨a, ⟨ha, hm⟩, _⟩ := fixed_shape_fit hx (div_pos (sub_pos.mpr ht.2) ht.1)
    rw [hprofile ht]
    change ∃ a ∈ LocationScale.domain, sSup (_ '' _) = _ ∧ _
    have he := (hm.isLUB ha).csSup_eq (Nonempty.image _ ⟨a, ha⟩)
    exact ⟨a, ha, he, fun b hb ↦ (hm hb).trans_eq he.symm⟩
  have hlift (t : ℝ) (ht : t ∈ Ioo 0 1) (hm : IsMaxOn (compactProfile x i) (Icc 0 1) t) :
      ∃ a ∈ LocationScale.domain,
        IsMaxOn (fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik x ((1-p.1)/p.1) p.2)
          (Ioo 0 1 ×ˢ LocationScale.domain) (t,a) := by
    obtain ⟨a, ha, he, _⟩ := hfit t ht
    refine ⟨a, ha, ?_⟩
    rintro ⟨r,b⟩ ⟨hr,hb⟩
    obtain ⟨_, _, _, hbound⟩ := hfit r hr
    exact (hbound b hb).trans ((hm (Ioo_subset_Icc_self hr)).trans_eq he)
  obtain ⟨p, hp, hm⟩ := hlift u hu hU
  obtain ⟨q, hq, hqmax⟩ := hlift v hv hV
  exact ⟨(u,p), ⟨hu,hp⟩, (v,q), ⟨hv,hq⟩, fun h ↦ huv (congrArg Prod.fst h), hm, hqmax⟩

/-- A continuous crossing of two disjoint compact windows gives two finite
all-free EMG maximizers, provided the rest of the profile stays below the
larger window value throughout the crossing. -/
theorem two_finite_of_window_crossing {n : ℕ} {a b : ℝ} (hab : a ≤ b)
    (x : ℝ → Fin n → ℝ) (i : Fin n)
    (hx : ∀ A ∈ Icc a b, LocationScale.Nonconstant (x A))
    (hi : ∀ A ∈ Icc a b, ∀ j, x A i ≤ x A j)
    {K J : Set ℝ} (hK : IsCompact K) (hJ : IsCompact J)
    (hneK : K.Nonempty) (hneJ : J.Nonempty) (hdis : Disjoint K J)
    (hKsub : K ⊆ Ioo 0 1) (hJsub : J ⊆ Ioo 0 1)
    (hwindow : ContinuousOn (fun A ↦
      sSup (compactProfile (x A) i '' J) - sSup (compactProfile (x A) i '' K)) (Icc a b))
    (ha : sSup (compactProfile (x a) i '' J) ≤ sSup (compactProfile (x a) i '' K))
    (hb : sSup (compactProfile (x b) i '' K) ≤ sSup (compactProfile (x b) i '' J))
    (hgap : ∀ A ∈ Icc a b, ∀ t ∈ Icc (0 : ℝ) 1, t ∉ K → t ∉ J →
      compactProfile (x A) i t ≤ max (sSup (compactProfile (x A) i '' K))
        (sSup (compactProfile (x A) i '' J))) :
    ∃ A ∈ Icc a b,
      let S := Ioo (0 : ℝ) 1 ×ˢ LocationScale.domain
      let L := fun p : ℝ × (ℝ × ℝ) ↦ shapeLogLik (x A) ((1-p.1)/p.1) p.2
      ∃ p ∈ S, ∃ q ∈ S, p ≠ q ∧ IsMaxOn L S p ∧ IsMaxOn L S q := by
  have h := intermediate_value_Icc hab hwindow ⟨sub_nonpos.mpr ha, sub_nonneg.mpr hb⟩
  obtain ⟨A, hA, he⟩ := h
  have heq := sub_eq_zero.mp he
  have hc := (compactification (hx A hA) i (hi A hA)).1
  obtain ⟨u, hu, heU, hU⟩ := hK.exists_sSup_image_eq_and_ge hneK
    (hc.mono (hKsub.trans Ioo_subset_Icc_self))
  obtain ⟨v, hv, heV, hV⟩ := hJ.exists_sSup_image_eq_and_ge hneJ
    (hc.mono (hJsub.trans Ioo_subset_Icc_self))
  have huv : compactProfile (x A) i u = compactProfile (x A) i v :=
    heU.symm.trans (heq.symm.trans heV)
  have hm : IsMaxOn (compactProfile (x A) i) (Icc 0 1) u := by
    intro t ht
    by_cases htK : t ∈ K
    · exact hU t htK
    by_cases htJ : t ∈ J
    · exact (hV t htJ).trans_eq huv.symm
    have h := hgap A hA t ht htK htJ
    rw [← heq, max_self, heV] at h
    exact h.trans_eq huv.symm
  exact ⟨A, hA, profile_two_winners_lift (hx A hA) (hi A hA) (hKsub hu) (hJsub hv)
    (fun h ↦ disjoint_left.mp hdis (h ▸ hu) hv) hm (fun _ ht ↦ (hm ht).trans_eq huv)⟩

/-- Without a finite fit at held rate, every finite location and scale
strictly loses to the compatible moving exponential. That face is the
least upper bound, including for constant observations. -/
theorem held_rate_nonattainment {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {rate : ℝ} (hr : 0 < rate) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density rate p.1 p.2 (x j)
    let B := ∏ j, rate * Real.exp (-rate * (x j - x i))
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B := by
  let path (t : ℝ) := (x i - t⁻¹, (t⁻¹) ^ 2)
  apply LocScaleCommon.nonattainment_iff_of_face (S := (univ : Set ℝ) ×ˢ Ioi 0)
    (L := fun p : ℝ × ℝ ↦ ∏ j, density rate p.1 p.2 (x j)) (path := path)
    ((eventually_gt_atTop (0 : ℝ)).mono fun _ ht ↦ ⟨mem_univ _, pow_pos (inv_pos.mpr ht) 2⟩)
    (moving_exponential_face x hi rate)
  rw [held_rate_criterion x hi hr]
  exact ⟨fun ⟨m, s, hs, h⟩ ↦ ⟨(m, s), ⟨mem_univ _, hs⟩, h⟩,
    fun ⟨p, hp, h⟩ ↦ ⟨p.1, p.2, hp.2, h⟩⟩

private theorem standardized_atTop {α : Type*} {l : Filter α} {s μ : α → ℝ}
    {c z : ℝ} (hs : Tendsto s l (𝓝[>] 0)) (hμ : Tendsto μ l (𝓝 c)) (hz : c < z) :
    Tendsto (fun t ↦ (z - μ t) / s t) l atTop := by
  simpa [div_eq_mul_inv] using
    (tendsto_const_nhds.sub hμ).pos_mul_atTop (sub_pos.mpr hz) hs.inv_tendsto_nhdsGT_zero

/-- As scale tends to zero at held rate, every tied contact contributes the
same CDF factor. The three options are a finite standardized contact, a
contact approached from below at a greater distance than the scale, and
the opposite infinite standardized contact. -/
theorem held_rate_joint_exponential_trace {α : Type*} {l : Filter α} {n : ℕ}
    (x : Fin n → ℝ) {rate c penalty : ℝ} {s μ : α → ℝ}
    (hs : Tendsto s l (𝓝[>] 0)) (hμ : Tendsto μ l (𝓝 c))
    (hcontact : (∃ h : ℝ, penalty = Φ h ∧ Tendsto (fun t ↦ (c - μ t) / s t) l (𝓝 h)) ∨
      (penalty = 1 ∧ Tendsto (fun t ↦ (c - μ t) / s t) l atTop) ∨
      (penalty = 0 ∧ Tendsto (fun t ↦ (c - μ t) / s t) l atBot)) :
    Tendsto (fun t ↦ ∏ j, density rate (μ t) (s t) (x j)) l
      (𝓝 (∏ j, rate * Real.exp (-rate * (x j - c)) *
        (if c < x j then 1 else if x j = c then penalty else 0))) := by
  have hs₀ : Tendsto s l (𝓝 0) := hs.mono_right nhdsWithin_le_nhds
  have hrs := hs₀.const_mul rate
  have hr₀ : Tendsto (fun t ↦ rate * s t) l (𝓝 0) := by simpa using hrs
  have hneg : Tendsto (fun t ↦ -(rate * s t)) l (𝓝 0) := by simpa using hr₀.neg
  apply tendsto_finsetProd Finset.univ
  intro j _
  have hbase : Tendsto (fun t ↦ rate * Real.exp ((rate * s t)^2/2 - rate * (x j - μ t))) l
      (𝓝 (rate * Real.exp (-rate * (x j - c)))) := by
    have h := ((hr₀.pow 2).div_const 2).sub (((tendsto_const_nhds (x := x j)).sub hμ).const_mul rate)
    simpa [Function.comp_def, neg_mul] using (Real.continuous_exp.continuousAt.tendsto.comp h).const_mul rate
  have hCDF : Tendsto (fun t ↦ Φ ((x j - μ t) / s t - rate * s t)) l
      (𝓝 (if c < x j then 1 else if x j = c then penalty else 0)) := by
    rcases lt_trichotomy c (x j) with hj | hj | hj
    · simp only [hj, ite_true]
      exact cdf_atTop.comp (by
        simpa only [sub_eq_add_neg] using (standardized_atTop hs hμ hj).atTop_add hneg)
    · simp only [← hj, lt_self_iff_false, ite_false, ite_true]
      rcases hcontact with ⟨h, rfl, hh⟩ | ⟨rfl, hh⟩ | ⟨rfl, hh⟩
      · exact cdf_continuous.continuousAt.tendsto.comp (by simpa using hh.sub hr₀)
      · exact cdf_atTop.comp (by simpa only [sub_eq_add_neg] using hh.atTop_add hneg)
      · exact cdf_atBot.comp (by simpa only [sub_eq_add_neg] using hh.atBot_add hneg)
    · simp only [not_lt_of_ge hj.le, ite_false, hj.ne]
      have hbot : Tendsto (fun t ↦ (x j - μ t) / s t) l atBot := by
        have ht := ((tendsto_const_nhds (x := x j)).sub hμ).neg
        have hh : Tendsto (fun t ↦ -(x j - μ t) * (s t)⁻¹) l atTop :=
          ht.pos_mul_atTop (neg_pos.mpr (sub_neg.mpr hj)) hs.inv_tendsto_nhdsGT_zero
        simpa only [Function.comp_def, neg_mul, neg_neg, div_eq_mul_inv] using (tendsto_neg_atTop_atBot.comp hh)
      exact cdf_atBot.comp (by simpa only [sub_eq_add_neg] using hbot.atBot_add hneg)
  exact hbase.mul hCDF

/-- For held rate and location, the zero-scale trace includes half density
at each contact, and the infinite-scale trace vanishes. -/
theorem held_rate_location_scale_ends {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    Tendsto (fun s ↦ ∏ j, density rate μ s (x j)) (𝓝[>] 0) (𝓝 (exponentialTrace x rate μ)) ∧
      Tendsto (fun s ↦ ∏ j, density rate μ s (x j)) atTop (𝓝 0) := by
  refine ⟨tendsto_finsetProd Finset.univ (fun j _ ↦ exponential_path rate μ (x j)), ?_⟩
  have ht (j : Fin n) : Tendsto (fun s ↦ density rate μ s (x j)) atTop (𝓝 0) := by
    have hb : Tendsto (fun s : ℝ ↦ φ 0 / s) atTop (𝓝 0) := by
      simpa [div_eq_mul_inv] using (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul (φ 0)
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds hb ?_ ?_
    · exact Eventually.of_forall fun s ↦
        (mul_pos (mul_pos hr (Real.exp_pos _)) (cdf_pos _)).le
    · exact (eventually_gt_atTop (0 : ℝ)).mono fun _ hs ↦ (gaussian_peak_strict hr hs).le
  simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)

/-- For held location and scale, zero rate gives zero and infinite rate
gives the matched normal product. -/
theorem held_location_scale_rate_ends {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    Tendsto (fun rate ↦ ∏ j, density rate μ σ (x j)) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun rate ↦ ∏ j, density rate μ σ (x j)) atTop (𝓝 (normalTrace x μ σ)) := by
  refine ⟨?_, tendsto_finsetProd Finset.univ (fun j _ ↦ normal_path μ (x j) hσ)⟩
  have ht (j : Fin n) : Tendsto (fun r ↦ density r μ σ (x j)) (𝓝[>] 0) (𝓝 0) := by
    have hc : Continuous (fun r ↦ density r μ σ (x j)) := by
      unfold density
      exact (show Continuous (fun r : ℝ ↦ r * Real.exp ((r * σ)^2 / 2 - r * (x j - μ)))
        by fun_prop).mul (cdf_continuous.comp (by fun_prop))
    simpa [density] using (hc.continuousAt (x := (0 : ℝ))).tendsto.mono_left nhdsWithin_le_nhds
  simpa [zero_pow hn.ne'] using tendsto_finsetProd Finset.univ (fun j _ ↦ ht j)

/-- At fixed rate and location, failure of a finite maximum is strict
loss to the zero-scale face, which is then the exact supremum. -/
theorem held_rate_location_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {rate μ : ℝ} (hr : 0 < rate) :
    let S := Ioi (0 : ℝ)
    let L := fun s ↦ ∏ j, density rate μ s (x j)
    let B := exponentialTrace x rate μ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B :=
  LocScaleCommon.nonattainment_iff_of_face (l := 𝓝[>] (0 : ℝ)) (path := id)
    self_mem_nhdsWithin (held_rate_location_scale_ends hn x hr).1
    (held_rate_location_criterion hn x hr)

/-- At fixed location and scale, failure of a finite maximum is strict
loss to the infinite-rate normal face, which is then the exact supremum. -/
theorem held_location_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ σ : ℝ} (hσ : 0 < σ) :
    let S := Ioi (0 : ℝ)
    let L := fun rate ↦ ∏ j, density rate μ σ (x j)
    let B := normalTrace x μ σ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B :=
  LocScaleCommon.nonattainment_iff_of_face (l := atTop) (path := id)
    (eventually_gt_atTop 0) (held_location_scale_rate_ends hn x hσ).2
    (held_location_scale_criterion hn x hσ)

private theorem normalTrace_pos {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) : 0 < normalTrace x μ σ :=
  Finset.prod_pos fun _ _ ↦ div_pos (phi_pos _) hσ

private theorem log_normalTrace {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) : Real.log (normalTrace x μ σ) = Normal.logLik x μ σ := by
  rw [Normal.logLik_formula x μ hσ]
  unfold normalTrace
  rw [Real.log_prod (fun _ _ ↦ (div_pos (phi_pos _) hσ).ne')]
  have hπ : 0 < 2 * Real.pi := by positivity
  have hterm (j : Fin n) : Real.log (φ ((x j - μ) / σ) / σ) =
      -(1 / 2) * Real.log (2 * Real.pi) - Real.log σ - (x j - μ)^2 / (2 * σ^2) := by
    rw [Real.log_div (phi_pos _).ne' hσ.ne', phi_formula,
      Real.log_mul (one_div_pos.mpr (Real.sqrt_pos.mpr hπ)).ne' (Real.exp_pos _).ne',
      one_div, Real.log_inv, Real.log_sqrt hπ.le, Real.log_exp, div_pow]
    ring
  change (∑ j, Real.log (φ ((x j - μ) / σ) / σ)) = _
  have hs : (∑ j, Real.log (φ ((x j - μ) / σ) / σ)) =
      ∑ j, (-(1 / 2) * Real.log (2 * Real.pi) - Real.log σ - (x j - μ)^2 / (2 * σ^2)) :=
    Finset.sum_congr rfl (fun j _ ↦ hterm j)
  rw [hs, Finset.sum_sub_distrib, Finset.sum_sub_distrib]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    div_eq_mul_inv, ← Finset.sum_mul, Normal.residual]
  ring

private theorem held_scale_normal_value {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    sSup (range (fun m ↦ normalTrace x m σ)) = normalTrace x (Normal.mean x) σ := by
  have hm := (Normal.heldSd hn x hσ (Normal.mean x)).mpr rfl
  have hmax : IsMaxOn (fun m ↦ normalTrace x m σ) univ (Normal.mean x) := by
    intro m _
    change normalTrace x m σ ≤ normalTrace x (Normal.mean x) σ
    apply (Real.log_le_log_iff (normalTrace_pos x m hσ)
      (normalTrace_pos x (Normal.mean x) hσ)).mp
    rw [log_normalTrace x m hσ, log_normalTrace x (Normal.mean x) hσ]
    exact hm (mem_univ m)
  rw [← image_univ]
  exact (hmax.isLUB (mem_univ _)).csSup_eq (univ_nonempty.image (fun m ↦ normalTrace x m σ))

/-- At held Gaussian scale, a finite location and rate attain exactly when
a finite fit reaches the matched normal value at the sample mean. -/
theorem held_scale_mean_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.2 p.1 σ (x j)
    let B := normalTrace x (Normal.mean x) σ
    (∃ p ∈ S, IsMaxOn L S p) ↔ ∃ p ∈ S, B ≤ L p := by
  dsimp only
  rw [held_scale_criterion hn x hσ, held_scale_normal_value hn x hσ]
  exact ⟨fun ⟨m, r, hr, h⟩ ↦ ⟨(m,r), ⟨mem_univ _, hr⟩, h⟩,
    fun ⟨p, hp, h⟩ ↦ ⟨p.1, p.2, hp.2, h⟩⟩

/-- At held Gaussian scale, nonattainment occurs exactly when the normal
face at the sample mean is strictly above every finite candidate. -/
theorem held_scale_nonattainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    let S := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.2 p.1 σ (x j)
    let B := normalTrace x (Normal.mean x) σ
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < B) ∧ IsLUB (L '' S) B := by
  let path (r : ℝ) := (Normal.mean x, r)
  exact LocScaleCommon.nonattainment_iff_of_face (l := atTop) (path := path)
    ((eventually_gt_atTop (0 : ℝ)).mono fun _ hr ↦
      (show path _ ∈ (univ : Set ℝ) ×ˢ Ioi 0 from ⟨mem_univ _, hr⟩))
    (held_location_scale_rate_ends hn x hσ (μ := Normal.mean x)).2
    (held_scale_mean_criterion hn x hσ)

private theorem held_location_normal_value {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ : ℝ} (hx : ∃ i, x i ≠ μ) :
    IsGreatest (normalTrace x μ '' Ioi 0) (normalTrace x μ (Normal.sd x μ)) := by
  obtain ⟨hs, hm⟩ := (Normal.heldMean hn hx (Normal.sd x μ)).mpr rfl
  refine ⟨mem_image_of_mem _ hs, ?_⟩
  rintro _ ⟨s, h, rfl⟩
  apply (Real.log_le_log_iff (normalTrace_pos x μ h) (normalTrace_pos x μ hs)).mp
  rw [log_normalTrace x μ h, log_normalTrace x μ hs]
  exact hm h

private theorem exponentialTrace_formula {n : ℕ} (x : Fin n → ℝ)
    {μ rate : ℝ} (hr : 0 < rate) (hx : ∀ i, μ ≤ x i) :
    exponentialTrace x rate μ =
      Real.exp (Exponential.logLik (fun j ↦ x j - μ) rate) *
        ∏ j, (if μ < x j then (1 : ℝ) else 1 / 2) := by
  unfold exponentialTrace
  simp only [not_lt_of_ge (hx _), ite_false]
  rw [Finset.prod_mul_distrib]
  congr 1
  rw [Finset.prod_mul_distrib, Finset.prod_const, Finset.card_univ, Fintype.card_fin,
    ← Real.exp_sum, ← Finset.mul_sum, Exponential.logLik, Real.exp_sub, Real.exp_nat_mul,
    Real.exp_log hr, neg_mul, Real.exp_neg, div_eq_mul_inv]

/-- With location held and a nonzero residual, the exponential face is
maximized at the exponential rate of the residuals when compatible.
If any residual is negative, its whole face is zero. -/
theorem held_location_exponential_value {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ : ℝ} (hx : ∃ i, x i ≠ μ) :
    let B := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    IsGreatest ((fun r ↦ exponentialTrace x r μ) '' Ioi 0) B := by
  dsimp only
  split_ifs with hy
  · have hpos : ∃ j, 0 < x j - μ := by
      obtain ⟨j, hj⟩ := hx
      exact ⟨j, sub_pos.mpr ((hy j).lt_of_ne hj.symm)⟩
    obtain ⟨hr, hm⟩ := (Exponential.mle hn (fun j ↦ sub_nonneg.mpr (hy j)) hpos _).mpr rfl
    refine ⟨mem_image_of_mem _ hr, ?_⟩
    rintro _ ⟨r, hr', rfl⟩
    change exponentialTrace x r μ ≤ _
    rw [exponentialTrace_formula x hr' hy, exponentialTrace_formula x hr hy]
    apply mul_le_mul_of_nonneg_right (Real.exp_le_exp.mpr (hm hr'))
    exact Finset.prod_nonneg (fun _ _ ↦ by split_ifs <;> norm_num)
  · push Not at hy
    obtain ⟨j, hj⟩ := hy
    have hz (r : ℝ) : exponentialTrace x r μ = 0 := by
      unfold exponentialTrace
      apply Finset.prod_eq_zero (Finset.mem_univ j)
      simp [hj, not_lt_of_ge hj.le]
    refine ⟨⟨1, zero_lt_one, hz 1⟩, ?_⟩
    rintro _ ⟨r, _, rfl⟩
    change exponentialTrace x r μ ≤ 0
    rw [hz]

/-- With location held, all-zero residuals give unbounded likelihood along
the fixed standardized-rate collapse. -/
theorem held_location_zero_residual_collapse {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {μ : ℝ} (hx : ∀ i, x i = μ) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)) '' (Ioi 0 ×ˢ Ioi 0)) := by
  rintro ⟨B, hB⟩
  have ht := (constant_sample_unbounded hn μ).eventually_gt_atTop B
  have hS : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨s, hs, hb⟩ := (hS.and ht).exists
  have hp : (s⁻¹, s) ∈ Ioi (0 : ℝ) ×ˢ Ioi 0 := ⟨inv_pos.mpr hs, hs⟩
  have h := hB (mem_image_of_mem (fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)) hp)
  exact hb.not_ge (by simpa only [hx] using h)

/-- At held location, a finite fit requires a nonzero residual and a
finite value reaching both optimized face values. -/
theorem held_location_rms_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    let S := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)
    let N := normalTrace x μ (Normal.sd x μ)
    let E := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    (∃ p ∈ S, IsMaxOn L S p) ↔ (∃ i, x i ≠ μ) ∧ ∃ p ∈ S, max N E ≤ L p := by
  dsimp only
  by_cases hx : ∃ i, x i ≠ μ
  · have hN := held_location_normal_value hn x hx
    have hE := held_location_exponential_value hn x hx
    obtain ⟨i, hi⟩ := hx
    rw [held_location_criterion x μ hi, hN.isLUB.csSup_eq ⟨_, hN.1⟩,
      hE.isLUB.csSup_eq ⟨_, hE.1⟩]
    exact ⟨fun ⟨r, s, hr, hs, h⟩ ↦ ⟨⟨i, hi⟩, (r,s), ⟨hr,hs⟩, h⟩,
      fun ⟨_, p, hp, h⟩ ↦ ⟨p.1, p.2, hp.1, hp.2, h⟩⟩
  · refine iff_of_false (fun ⟨p, hp, hm⟩ ↦ ?_) (fun h ↦ hx h.1)
    exact held_location_zero_residual_collapse hn x (fun i ↦ not_not.mp (fun h ↦ hx ⟨i, h⟩))
      ⟨_, (hm.isLUB hp).1⟩

/-- For a nonzero held-location residual, if no finite point wins, the
larger optimized normal and exponential face is the exact supremum. -/
theorem held_location_face_supremum {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ)
    (hx : ∃ i, x i ≠ μ) :
    let S := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ ∏ j, density p.1 μ p.2 (x j)
    let N := normalTrace x μ (Normal.sd x μ)
    let E := if ∀ j, μ ≤ x j then
      exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
    (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < max N E) ∧ IsLUB (L '' S) (max N E) := by
  let S := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  let L (p : ℝ × ℝ) := ∏ j, density p.1 μ p.2 (x j)
  let N := normalTrace x μ (Normal.sd x μ)
  let E := if ∀ j, μ ≤ x j then
    exponentialTrace x (Exponential.rate (fun j ↦ x j - μ)) μ else 0
  change (¬ ∃ p ∈ S, IsMaxOn L S p) ↔ (∀ p ∈ S, L p < max N E) ∧ IsLUB (L '' S) (max N E)
  have hc : (∃ p ∈ S, IsMaxOn L S p) ↔ ∃ p ∈ S, max N E ≤ L p := by
    have h := held_location_rms_criterion hn x μ
    dsimp only at h
    exact h.trans (and_iff_right hx)
  rw [hc]
  constructor
  · intro hf
    have hlt (p : ℝ × ℝ) (hp : p ∈ S) : L p < max N E :=
      lt_of_not_ge (fun h ↦ hf ⟨p, hp, h⟩)
    refine ⟨hlt, ?_, ?_⟩
    · rintro _ ⟨p, hp, rfl⟩
      exact (hlt p hp).le
    · intro B hB
      apply max_le
      · obtain ⟨s, hs, he⟩ := (held_location_normal_value hn x hx).1
        change normalTrace x μ s = N at he
        rw [← he]
        apply le_of_tendsto (held_location_scale_rate_ends hn x hs).2
        filter_upwards [eventually_gt_atTop (0 : ℝ)] with r hr
        change L (r, s) ≤ B
        exact hB (mem_image_of_mem L (show (r, s) ∈ S from ⟨hr, hs⟩))
      · obtain ⟨r, hr, he⟩ := (held_location_exponential_value hn x hx).1
        change exponentialTrace x r μ = E at he
        rw [← he]
        apply le_of_tendsto (held_rate_location_scale_ends hn x hr).1
        filter_upwards [self_mem_nhdsWithin (a := (0 : ℝ)) (s := Ioi 0)] with s hs
        change L (r, s) ≤ B
        exact hB (mem_image_of_mem L (show (r, s) ∈ S from ⟨hr, hs⟩))
  · rintro ⟨hlt, _⟩ ⟨p, hp, h⟩
    exact (hlt p hp).not_ge h

/-- At held Gaussian scale and constant data, every feasible fit strictly
loses to the normal at the common observation. -/
theorem held_scale_constant_strict {n : ℕ} (hn : 0 < n) (m : ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∀ μ rate : ℝ, 0 < rate →
      (∏ _ : Fin n, density rate μ σ m) < normalTrace (fun _ : Fin n ↦ m) m σ := by
  intro μ rate hr
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  unfold normalTrace
  apply Finset.prod_lt_prod_of_nonempty₀ _ _ Finset.univ_nonempty
  · exact fun _ _ ↦ mul_pos (mul_pos hr (Real.exp_pos _)) (cdf_pos _)
  · exact fun _ _ ↦ by simpa only [sub_self, zero_div] using
      (gaussian_peak_strict (μ := μ) (x := m) hr hσ)

end UnivariateML.LocScaleEMG

end
