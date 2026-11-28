module

public import UnivariateML.Family.EVGEV
public import UnivariateML.Family.AllFreeFrechet
public import UnivariateML.Generic.Compactification

public import Mathlib.Analysis.Calculus.DSlope

set_option linter.style.header false

/-! # All-free Fréchet on the reciprocal-shape interval

The data are expressed as nonnegative gaps from an attained sample minimum.
The Gumbel seam uses the same translated sample.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.EVFrechet

noncomputable section

private theorem transport {n : ℕ} {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    (GEV.logLik g b '' GEV.domain g b) =
      {v | ∃ p ∈ Frechet.domain, Frechet.logLik g b⁻¹ p.1 p.2 = v} := by
  have hb1 : b ≠ -1 := by linarith
  have harg (p : ℝ × ℝ) (hs : 0 < p.2) (i : Fin n) :
      GEV.supportArg b (g i) (p.2 - p.1, b * p.2) = (g i + p.1) / p.2 := by
    unfold GEV.supportArg
    dsimp only
    field_simp
    ring
  have hval (p : ℝ × ℝ) (hs : 0 < p.2) :
      GEV.logLik g b (p.2 - p.1, b * p.2) = Frechet.logLik g b⁻¹ p.1 p.2 := by
    unfold GEV.logLik Frechet.logLik
    apply Finset.sum_congr rfl
    intro i _
    simp only [ite_eq_right hb.ne', harg p hs i]
    rw [log_mul hb.ne' hs.ne', Frechet.logDensity, log_inv]
    rw [show (-1 : ℝ) / b = -b⁻¹ by ring]
    ring
  ext v
  constructor
  · rintro ⟨q, hq, rfl⟩
    let p : ℝ × ℝ := (q.2 / b - q.1, q.2 / b)
    have hs : 0 < p.2 := div_pos hq.1 hb
    have hqeq : (p.2 - p.1, b * p.2) = q := by
      apply Prod.ext <;> dsimp [p] <;> field_simp
      ring
    obtain ⟨i, hi⟩ := hz
    have ha := hq.2 i
    rw [ite_eq_right hb1, ← hqeq, harg p hs i, hi, zero_add] at ha
    exact ⟨p, ⟨(div_pos_iff_of_pos_right hs).mp ha, hs⟩, (hval p hs).symm.trans
      (congrArg (GEV.logLik g b) hqeq)⟩
  · rintro ⟨p, hp, rfl⟩
    have hs : 0 < p.2 := hp.2
    refine ⟨(p.2 - p.1, b * p.2), ⟨mul_pos hb hs, ?_⟩, hval p hs⟩
    intro i
    rw [ite_eq_right hb1, harg p hs i]
    exact div_pos (add_pos_of_nonneg_of_pos (hg i) hp.1) hs

/-- Total optimized log likelihood in the reciprocal-shape chart. At zero
this is the optimized Gumbel value. -/
def profile {n : ℕ} (g : Fin n → ℝ) (b : ℝ) : ℝ :=
  sSup (GEV.logLik g b '' GEV.domain g b)

/-- At positive reciprocal shape this profile agrees with the original
Fréchet supremum over the endpoint gap and scale. -/
theorem profile_eq_frechet {n : ℕ} {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i)
    (hz : ∃ i, g i = 0) {b : ℝ} (hb : 0 < b) :
    profile g b = sSup {v | ∃ p ∈ Frechet.domain, Frechet.logLik g b⁻¹ p.1 p.2 = v} := by
  rw [profile, transport hg hz hb]

private theorem nonconstant {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hz : ∃ i, g i = 0) (hk : 2 * Frechet.minimumCount g ≤ n) :
    LocationScale.Nonconstant g := by
  obtain ⟨i, hi⟩ := hz
  unfold LocationScale.Nonconstant
  by_contra! h
  have hh : ∀ j, g j = 0 := fun j ↦ (h j i).trans hi
  have he : Frechet.minimumCount g = n := by simp [Frechet.minimumCount, hh]
  omega

private theorem profile_winner {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hk : 2 * Frechet.minimumCount g < n) {b : ℝ} (hb : b ∈ Icc (0 : ℝ) 1) :
    ∃ p ∈ GEV.domain g b, GEV.logLik g b p = profile g b ∧
      IsMaxOn (GEV.logLik g b) (GEV.domain g b) p := by
  have he : GEV.lowerGaps g 0 = g := by funext i; simp [GEV.lowerGaps]
  have h : ∃ p, GEV.IsMLE g b p := by
    rcases hb.1.eq_or_lt with h | h
    · rw [← h]
      exact (GEV.zero_unique (nonconstant hn hz hk.le)).exists
    · apply (GEV.positive_exists_iff hn h hg hz).mpr
      rw [he]
      have hk' : (2 : ℝ) * Frechet.minimumCount g < n := by exact_mod_cast hk
      have hs := mul_le_mul_of_nonneg_right hb.2 (Nat.cast_nonneg (Frechet.minimumCount g))
      linarith
  obtain ⟨p, hp, hm⟩ := h
  have hlu : IsLUB (GEV.logLik g b '' GEV.domain g b) (GEV.logLik g b p) := by
    refine ⟨?_, fun B hB ↦ hB (mem_image_of_mem _ hp)⟩
    rintro _ ⟨q, hq, rfl⟩
    exact hm hq
  exact ⟨p, hp, (hlu.csSup_eq ⟨_, mem_image_of_mem _ hp⟩).symm, hm⟩

/-- For a strict minimum minority every superlevel of the extended
reciprocal-shape profile is compact, including the optimized Gumbel seam. -/
theorem profile_compact {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hk : 2 * Frechet.minimumCount g < n) (v : ℝ) :
    IsCompact (Icc (0 : ℝ) 1 ∩ profile g ⁻¹' Ici v) := by
  let K := EVGEV.chart ∩ EVGEV.likelihood g ⁻¹' Ici (exp v)
  let E : Set EVGEV.Parameters := Prod.fst ⁻¹' Icc (0 : ℝ) 1
  have he : GEV.lowerGaps g 0 = g := by funext i; simp [GEV.lowerGaps]
  have hK : IsCompact K := EVGEV.all_free_compact hn hg hz (by rwa [he]) (exp_pos v)
  have hC : IsCompact (K ∩ E) := hK.inter_right (isClosed_Icc.preimage continuous_fst)
  have hEq : Prod.fst '' (K ∩ E) = Icc (0 : ℝ) 1 ∩ profile g ⁻¹' Ici v := by
    classical
    ext b
    constructor
    · rintro ⟨q, ⟨⟨hq, hval⟩, hb⟩, rfl⟩
      have hd : q.2 ∈ GEV.domain g q.1 := by
        by_contra h
        have hh : exp v ≤ 0 := by simpa [EVGEV.likelihood, h] using hval
        exact (exp_pos v).not_ge hh
      obtain ⟨p, _, hp, hm⟩ := profile_winner hn hg hz hk hb
      refine ⟨hb, ?_⟩
      change v ≤ profile g q.1
      rw [← hp]
      exact (exp_le_exp.mp (by simpa [EVGEV.likelihood, hd] using hval)).trans (hm hd)
    · rintro ⟨hb, hval⟩
      obtain ⟨p, hd, hv, _⟩ := profile_winner hn hg hz hk hb
      refine ⟨(b,p), ⟨⟨⟨⟨by linarith [hb.1], hb.2⟩, hd.1⟩, ?_⟩, hb⟩, rfl⟩
      change exp v ≤ EVGEV.likelihood g (b,p)
      simpa [EVGEV.likelihood, hd, hv] using hval
  rw [← hEq]
  exact hC.image continuous_fst

private theorem fixed_nuisance_continuous {n : ℕ} {g : Fin n → ℝ} {b : ℝ} {p : ℝ × ℝ}
    (ha : ∀ i, 0 < GEV.supportArg b (g i) p) :
    ContinuousAt (fun t ↦ GEV.logLik g t p) b := by
  apply tendsto_finsetSum
  intro i _
  let z := (g i - p.1) / p.2
  let a : ℝ → ℝ := fun t ↦ 1 + t * z
  let r : ℝ → ℝ := fun t ↦ -z * dslope log 1 (a t)
  have hA : Continuous a := by unfold a; fun_prop
  have hpos : 0 < a b := ha i
  have hD : ContinuousAt (dslope log 1) (a b) := by
    by_cases he : a b = 1
    · rw [he]
      exact continuousAt_dslope_same.mpr (differentiableAt_log one_ne_zero)
    · exact (continuousAt_dslope_of_ne he).mpr (continuousAt_log hpos.ne')
  have hR : ContinuousAt r b := continuousAt_const.mul (hD.comp hA.continuousAt)
  have hC : ContinuousAt (fun t ↦ -log p.2 + (1 + t) * r t - exp (r t)) b :=
    (continuousAt_const.add ((continuousAt_const.add continuousAt_id).mul hR)).sub
      (continuous_exp.continuousAt.comp hR)
  apply hC.congr
  filter_upwards [hA.continuousAt.eventually (Ioi_mem_nhds hpos)] with t ht
  change 0 < a t at ht
  change -log p.2 + (1 + t) * r t - exp (r t) =
    if t = 0 then -log p.2 - z - exp (-z)
    else -log p.2 + (-1 / t - 1) * log (a t) - (a t) ^ (-1 / t)
  by_cases h : t = 0
  · subst t
    simp [r, a, dslope_same, Real.deriv_log, sub_eq_add_neg]
  by_cases hZ : z = 0
  · simp [r, a, h, hZ]
  have hne : a t ≠ 1 := by
    intro he
    exact (mul_ne_zero h hZ) (by dsimp [a] at he; linarith)
  have hr : r t = log (a t) * (-1 / t) := by
    change -z * dslope log 1 (a t) = _
    rw [dslope_of_ne _ hne, slope_def_field, log_one, sub_zero,
      show a t - 1 = t * z by dsimp [a]; ring]
    field_simp
  rw [hr, ite_eq_right h, rpow_def_of_pos ht]
  field_simp
  ring

/-- For a strict minimum minority the extended profile is continuous on
the closed reciprocal-shape interval, in particular at the Gumbel seam. -/
theorem profile_continuous {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    (hk : 2 * Frechet.minimumCount g < n) :
    ContinuousOn (profile g) (Icc (0 : ℝ) 1) := by
  rw [continuousOn_iff_lower_upperSemicontinuousOn]
  refine ⟨?_, upperSemicontinuousOn_iff_preimage_Ici.mpr fun v ↦
    ⟨_, (profile_compact hn hg hz hk v).isClosed, by ext; simp⟩⟩
  intro b hb v hv
  obtain ⟨p, hp, he, _⟩ := profile_winner hn hg hz hk hb
  have hb1 : b ≠ -1 := by linarith [hb.1]
  have ha (i : Fin n) : 0 < GEV.supportArg b (g i) p :=
    by simpa only [ite_eq_right hb1] using hp.2 i
  have hsupport : ∀ᶠ t in 𝓝 b, ∀ i, 0 < GEV.supportArg t (g i) p := by
    rw [Filter.eventually_all]
    intro i
    have hc : Continuous (fun t ↦ GEV.supportArg t (g i) p) := by
      unfold GEV.supportArg; fun_prop
    exact hc.continuousAt.eventually (Ioi_mem_nhds (ha i))
  have hval := (fixed_nuisance_continuous ha).eventually
    (Ioi_mem_nhds (by rwa [← he] at hv))
  filter_upwards [self_mem_nhdsWithin, hsupport.filter_mono nhdsWithin_le_nhds,
    hval.filter_mono nhdsWithin_le_nhds] with t ht hs hL
  obtain ⟨q, _, heq, hm⟩ := profile_winner hn hg hz hk ht
  have hpd : p ∈ GEV.domain g t := by
    refine ⟨hp.1, fun i ↦ ?_⟩
    split_ifs <;> first | exact (hs i).le | exact hs i
  change v < profile g t
  rw [← heq]
  exact hL.trans_le (hm hpd)

/-- Values attainable at finite fitted Fréchet parameters. -/
def fittedValues {n : ℕ} (g : Fin n → ℝ) : Set ℝ :=
  {v | ∃ a ≥ 1, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v}

private theorem finite_cofinal {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hk : 2 * Frechet.minimumCount g < n) :
    (∀ b ∈ Ioc (0 : ℝ) 1, profile g b ∈ fittedValues g) ∧
    (∀ v ∈ fittedValues g, ∃ b ∈ Ioc (0 : ℝ) 1, v ≤ profile g b) := by
  constructor
  · intro b hb
    obtain ⟨p, hp, he, _⟩ := profile_winner hn hg hz hk ⟨hb.1.le, hb.2⟩
    have hv : profile g b ∈ GEV.logLik g b '' GEV.domain g b := ⟨p, hp, he⟩
    rw [transport hg hz hb.1] at hv
    obtain ⟨p, hp, hv⟩ := hv
    exact ⟨b⁻¹, (one_le_inv₀ hb.1).mpr hb.2, p, hp, hv⟩
  · rintro v ⟨a, ha, p, hp, rfl⟩
    have hap : 0 < a := zero_lt_one.trans_le ha
    have hb : a⁻¹ ∈ Ioc (0 : ℝ) 1 := ⟨inv_pos.mpr hap, (inv_le_one₀ hap).mpr ha⟩
    obtain ⟨q, hq, he, hm⟩ := profile_winner hn hg hz hk ⟨hb.1.le, hb.2⟩
    have hv : Frechet.logLik g a p.1 p.2 ∈ GEV.logLik g a⁻¹ '' GEV.domain g a⁻¹ := by
      rw [transport hg hz hb.1]
      exact ⟨p, hp, by rw [inv_inv]⟩
    obtain ⟨r, hr, hv⟩ := hv
    exact ⟨a⁻¹, hb, by rw [← he, ← hv]; exact hm hr⟩

private theorem profile_attainment_iff {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hk : 2 * Frechet.minimumCount g < n) :
    (∃ v, IsGreatest (fittedValues g) v) ↔
      ∃ b ∈ Ioc (0 : ℝ) 1, profile g 0 ≤ profile g b := by
  have hc := profile_continuous hn hg hz hk
  obtain ⟨hmem, hcof⟩ := finite_cofinal hn hg hz hk
  let D := Ioc (0 : ℝ) 1
  let K := Icc (0 : ℝ) 1
  have hsub : D ⊆ K := fun _ h ↦ ⟨h.1.le, h.2⟩
  have h0 : (0 : ℝ) ∈ closure D := by
    rw [show D = Ioc (0 : ℝ) 1 from rfl, closure_Ioc (by norm_num : (0 : ℝ) ≠ 1)]
    exact ⟨le_rfl, zero_le_one⟩
  constructor
  · rintro ⟨v, hv, huv⟩
    obtain ⟨b, hb, hbv⟩ := hcof v hv
    refine ⟨b, hb, ?_⟩
    let : (𝓝[D] (0 : ℝ)).NeBot := mem_closure_iff_nhdsWithin_neBot.mp h0
    have hlim := (hc 0 (show (0 : ℝ) ∈ K from ⟨le_rfl, zero_le_one⟩)).mono hsub
    apply le_of_tendsto hlim
    filter_upwards [self_mem_nhdsWithin] with t ht
    exact (huv (hmem t ht)).trans hbv
  · rintro ⟨b, hb, hv⟩
    have hboundary : ∀ u ∈ K \ D, profile g u ≤ profile g b := by
      intro u hu
      have hu0 : u = 0 := le_antisymm
        (le_of_not_gt (fun hpos ↦ hu.2 ⟨hpos, hu.1.2⟩)) hu.1.1
      simpa only [hu0] using hv
    obtain ⟨t, ht, hmax⟩ := Generic.exists_isMaxOn_of_boundary_le
      hsub isCompact_Icc hc hb hboundary
    refine ⟨profile g t, hmem t ht, ?_⟩
    intro w hw
    obtain ⟨r, hr, hwr⟩ := hcof w hw
    exact hwr.trans (hmax hr)

/-- Without the fitted shape restriction the likelihood is unbounded.
On the fitted branch an MLE exists exactly for a strict minimum minority
whose positive reciprocal-shape profile reaches the optimized Gumbel value. -/
theorem all_free_criterion {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    (¬ BddAbove {v | ∃ a > 0, ∃ p ∈ Frechet.domain, Frechet.logLik g a p.1 p.2 = v}) ∧
    ((∃ v, IsGreatest (fittedValues g) v) ↔ 2 * Frechet.minimumCount g < n ∧
      ∃ b ∈ Ioc (0 : ℝ) 1, profile g 0 ≤ profile g b) := by
  refine ⟨AllFreeFrechet.unrestricted_unbounded hn hg hz, ?_⟩
  rcases lt_trichotomy (2 * Frechet.minimumCount g) n with hk | hk | hk
  · simpa only [hk, true_and] using profile_attainment_iff hn hg hz hk
  · have h := AllFreeFrechet.half_minimum_supremum hn hg hk.symm
    have hnot : ¬ ∃ v, IsGreatest (fittedValues g) v := by
      rintro ⟨v, ⟨a, ha, p, hp, rfl⟩, hv⟩
      exact (h.2.1 a ha p hp).not_ge (h.1.2 hv)
    simp [hnot, hk]
  · have hu := AllFreeFrechet.majority_unbounded hn hg hz hk
    have hnot : ¬ ∃ v, IsGreatest (fittedValues g) v :=
      fun ⟨v, _, hv⟩ ↦ hu ⟨v, hv⟩
    simp [hnot, hk.not_gt]

/-- Constants and other majority ties diverge. Half ties approach a strict
finite collapse bound. With a strict minority, Gumbel wins as an unattained
supremum exactly when every positive reciprocal shape loses strictly. -/
theorem all_free_failure {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) :
    let k := Frechet.minimumCount g
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (g i)
    (n < 2 * k → ¬ BddAbove (fittedValues g)) ∧
    (n = 2 * k → IsLUB (fittedValues g) A ∧
      (∀ v ∈ fittedValues g, v < A) ∧
      Tendsto (Frechet.profile g 1) (𝓝[>] 0) (𝓝 A) ∧ profile g 0 < A) ∧
    (2 * k < n → ((∀ b ∈ Ioc (0 : ℝ) 1, profile g b < profile g 0) ↔
      IsLUB (fittedValues g) (profile g 0) ∧ ¬ ∃ v, IsGreatest (fittedValues g) v)) := by
  intro k A
  refine ⟨AllFreeFrechet.majority_unbounded hn hg hz, ?_, ?_⟩
  · intro hk
    have H := AllFreeFrechet.half_minimum_supremum hn hg hk
    refine ⟨H.1, (by rintro _ ⟨a, ha, p, hp, rfl⟩; exact H.2.1 a ha p hp), H.2.2, ?_⟩
    obtain ⟨p, hp, hm⟩ := (GEV.zero_unique (nonconstant hn hz hk.symm.le)).exists
    have hL : IsLUB (GEV.logLik g 0 '' GEV.domain g 0) (GEV.logLik g 0 p) :=
      ⟨by rintro _ ⟨q, hq, rfl⟩; exact hm hq, fun _ h ↦ h ⟨p, hp, rfl⟩⟩
    rw [profile, hL.csSup_eq ⟨_, p, hp, rfl⟩]
    have hgap : GEV.lowerGaps g 0 = g := by funext i; simp [GEV.lowerGaps]
    have hhalf := EVGEV.all_free_half_supremum (m := 0) hn hg (by rwa [hgap])
    have hd : (0,p) ∈ {q : EVGEV.Parameters |
        q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain g q.1} := ⟨by norm_num, hp⟩
    have hle := hhalf.1.1 ⟨(0,p), hd, rfl⟩
    simp only [sub_zero] at hle
    apply lt_of_le_of_ne hle
    intro he
    have he' := (hhalf.2.1 (0,p) hd).mp (by simpa using he)
    norm_num at he' 
  · intro hk
    obtain ⟨hmem, hcof⟩ := finite_cofinal hn hg hz hk
    have hc := profile_continuous hn hg hz hk
    let D := Ioc (0 : ℝ) 1
    let K := Icc (0 : ℝ) 1
    have hsub : D ⊆ K := fun _ h ↦ ⟨h.1.le, h.2⟩
    have hzero : (0 : ℝ) ∈ K := ⟨le_rfl, zero_le_one⟩
    have h0 : (0 : ℝ) ∈ closure D := by
      rw [show D = Ioc (0 : ℝ) 1 from rfl, closure_Ioc (by norm_num : (0 : ℝ) ≠ 1)]
      exact hzero
    constructor
    · intro hlt
      have hs := Generic.sSup_eq_and_not_exists_isMaxOn_of_boundary_gt hsub hc hzero h0 hlt
      refine ⟨⟨?_, ?_⟩, ?_⟩
      · intro v hv
        obtain ⟨b, hb, hle⟩ := hcof v hv
        exact hle.trans (hlt b hb).le
      · intro B hB
        let : (𝓝[D] (0 : ℝ)).NeBot := mem_closure_iff_nhdsWithin_neBot.mp h0
        apply le_of_tendsto ((hc 0 hzero).mono hsub)
        filter_upwards [self_mem_nhdsWithin] with b hb
        exact hB (hmem b hb)
      · rintro ⟨v, hv, huv⟩
        obtain ⟨b, hb, hbv⟩ := hcof v hv
        exact hs.2 ⟨b, hb, fun r hr ↦ (huv (hmem r hr)).trans hbv⟩
    · rintro ⟨hL, hnot⟩ b hb
      have hle := hL.1 (hmem b hb)
      exact lt_of_le_of_ne hle (fun he ↦ hnot ⟨profile g b, hmem b hb, he ▸ hL.1⟩)

end
end UnivariateML.EVFrechet

end
