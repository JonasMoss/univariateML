module

public import UnivariateML.Family.Shape2Fatigue
public import UnivariateML.Family.Transfers
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics

set_option linter.style.header false

/-! # Joint threshold and scale at fixed Birnbaum–Saunders shape

The chart uses the positive distance from the threshold to the sample
minimum and the positive scale. The minimum and a strict sample gap yield
a penalty uniform in scale at the threshold boundary.
-/

@[expose] public section

open Real Set Filter Topology
namespace UnivariateML.Shape3Fatigue
noncomputable section

private def C (a : ℝ) : ℝ :=
  3 / (2 * a ^ 2) - log (2 * a) - log (2 * π) / 2 - (log (2 * a ^ 2)⁻¹ + 1) / 2

private theorem density_bound {a b t : ℝ} (ha : 0 < a) (hb : 0 < b) (ht : 0 < t) :
    Fatigue.logDensity a b t ≤ C a - log t - (t / b + b / t) / (4 * a ^ 2) := by
  let u := t / b + b / t + 2
  let k := (2 * a ^ 2)⁻¹
  have hu : 0 < u := by dsimp [u]; positivity
  have hk : 0 < k := by dsimp [k]; positivity
  have he : u = (t + b) ^ 2 / (t * b) := by dsimp [u]; field_simp; ring
  have hl : log u = 2 * log (t + b) - (log t + log b) := by
    rw [he, log_div (by positivity) (mul_ne_zero ht.ne' hb.ne'),
      log_pow, log_mul ht.ne' hb.ne']
    norm_num
  have h := log_le_sub_one_of_pos (mul_pos hk hu)
  rw [log_mul hk.ne' hu.ne', hl] at h
  have hform : k * u = (t / b + b / t + 2) / (2 * a ^ 2) := by
    dsimp [k, u]; ring
  rw [hform] at h
  unfold Fatigue.logDensity C
  change _ ≤ 3 / (2 * a ^ 2) - log (2 * a) - log (2 * π) / 2 -
    (log k + 1) / 2 - log t - (t / b + b / t) / (4 * a ^ 2)
  linear_combination (norm := field_simp [ha.ne'] <;> ring_nf <;> norm_num) h / 2

private theorem mixed_bound {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {i j : Fin n} (hi : g i = 0)
    {a d b : ℝ} (ha : 0 < a) (hd : 0 < d) (hb : 0 < b) :
    Fatigue.logLik (fun r ↦ d + g r) a b ≤
      C a - log d - (g j / b + b / d) / (4 * a ^ 2 * n) := by
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  let K := 4 * a ^ 2
  have hK : 0 < K := by dsimp [K]; positivity
  have ht (r) : 0 < d + g r := add_pos_of_pos_of_nonneg hd (hg r)
  have hs := Finset.sum_le_sum (s := Finset.univ) fun r _ ↦
    (density_bound ha hb (ht r)).trans
      (sub_le_sub_right
        (sub_le_sub_left (log_le_log hd (le_add_of_nonneg_right (hg r))) _)
        (((d + g r) / b + b / (d + g r)) / K))
  have h1 : g j / b / K ≤ ∑ r : Fin n, (d + g r) / b / K :=
    (div_le_div_of_nonneg_right (div_le_div_of_nonneg_right
      (le_add_of_nonneg_left hd.le) hb.le) hK.le).trans
      (Finset.single_le_sum (f := fun r : Fin n ↦ (d + g r) / b / K)
        (fun r _ ↦ div_nonneg (div_nonneg (ht r).le hb.le) hK.le) (Finset.mem_univ j))
  have h2 : b / d / K ≤ ∑ r : Fin n, b / (d + g r) / K := by
    simpa only [hi, add_zero] using
      (Finset.single_le_sum (f := fun r : Fin n ↦ b / (d + g r) / K)
        (fun r _ ↦ by positivity [ht r]) (Finset.mem_univ i))
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, add_div, Finset.sum_add_distrib] at hs
  rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.avg]
  apply (div_le_iff₀ hnR).2
  change _ ≤ (C a - log d - (g j / b + b / d) / (K * n)) * n
  have he : (g j / b + b / d) / (K * n) * n = g j / b / K + b / d / K := by
    field_simp
  rw [sub_mul, he]
  simp only [add_div, Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul] at h1
  linarith only [hs, h1, h2]

/-- Approaching the minimum sends the log likelihood to minus infinity
uniformly over all positive scales on every nonconstant sample. -/
private theorem held_alpha_uniform_threshold_contact {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hp : ∃ j, 0 < g j)
    {a : ℝ} (ha : 0 < a) (v : ℝ) :
    ∃ δ > 0, ∀ d ∈ Ioo 0 δ, ∀ b > 0,
      Fatigue.logLik (fun i ↦ d + g i) a b < v := by
  obtain ⟨i, hi⟩ := hz
  obtain ⟨j, hj⟩ := hp
  let K := 4 * a ^ 2 * (n : ℝ)
  let c := 2 * sqrt (g j) / K
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hK : 0 < K := by dsimp [K]; positivity
  have hc : 0 < c := by dsimp [c]; positivity
  have hsmall : Tendsto (fun d : ℝ ↦ -log d * sqrt d) (𝓝[>] 0) (𝓝 0) := by
    simpa only [sqrt_eq_rpow, neg_mul, neg_zero] using
      (tendsto_log_mul_rpow_nhdsGT_zero (r := 1 / 2) (by norm_num)).neg
  have hroot : Tendsto (fun d : ℝ ↦ sqrt d) (𝓝[>] 0) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    have hc : Tendsto (fun d : ℝ ↦ sqrt d) (𝓝[>] 0) (𝓝 (sqrt 0)) :=
      continuous_sqrt.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
    exact ⟨by simpa only [sqrt_zero] using hc,
      eventually_mem_nhdsWithin.mono (fun d hd ↦ sqrt_pos.mpr hd)⟩
  have hInf := (tendsto_inv_nhdsGT_zero.comp hroot).const_mul_atTop (half_pos hc)
  have hlim := tendsto_atBot_add_const_right _ (C a) (tendsto_neg_atTop_atBot.comp hInf)
  have hboth := (hsmall.eventually (Iio_mem_nhds (half_pos hc))).and
    (hlim.eventually (eventually_lt_atBot v))
  obtain ⟨δ, hδ, hnear⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp hboth
  refine ⟨δ, hδ, fun d hd b hb ↦ ?_⟩
  obtain ⟨h1, h2⟩ := hnear hd
  have hs : 0 < sqrt d := sqrt_pos.mpr hd.1
  have hU : 0 < g j / b := div_pos hj hb
  have hV : 0 < b / d := div_pos hb hd.1
  have hp : (g j / b) * (b / d) = g j / d := by field_simp
  have he : sqrt (g j / b) * sqrt (b / d) = sqrt (g j) / sqrt d := by
    rw [← sqrt_mul hU.le, hp, sqrt_div hj.le]
  have ham : 2 * (sqrt (g j) / sqrt d) ≤ g j / b + b / d := by
    rw [← he]
    nlinarith only [sq_nonneg (sqrt (g j / b) - sqrt (b / d)),
      sq_sqrt hU.le, sq_sqrt hV.le]
  have hpen : c / sqrt d ≤ (g j / b + b / d) / K := by
    have h := div_le_div_of_nonneg_right ham hK.le
    convert h using 1
    dsimp [c]
    ring
  have hlog : -log d ≤ c / 2 / sqrt d := (le_div_iff₀ hs).2 (le_of_lt h1)
  refine lt_of_le_of_lt (mixed_bound hn hg hi ha hd.1 hb (j := j)) ?_
  change C a - log d - (g j / b + b / d) / K < v
  change -((c / 2) * (sqrt d)⁻¹) + C a < v at h2
  linarith only [h2, hpen, hlog,
    show c / sqrt d = 2 * (c / 2 / sqrt d) by ring,
    show c / 2 * (sqrt d)⁻¹ = c / 2 / sqrt d by ring]

/-- At fixed positive shape, every competitive superlevel is confined to a
closed positive rectangle on a nonconstant sample. -/
private theorem held_alpha_competitive_rectangle {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) (hp : ∃ j, 0 < g j)
    {a : ℝ} (ha : 0 < a) (v : ℝ) :
    ∃ lo hi s S : ℝ, 0 < lo ∧ 0 < s ∧
      ∀ d b, 0 < d → 0 < b → v ≤ Fatigue.logLik (fun i ↦ d + g i) a b →
        (d, b) ∈ Icc lo hi ×ˢ Icc s S := by
  obtain ⟨lo, hlo, hsmall⟩ := held_alpha_uniform_threshold_contact hn hg hz hp ha v
  obtain ⟨i, hi⟩ := hz
  obtain ⟨j, hj⟩ := hp
  let U := exp (C a - v)
  let K := 4 * a ^ 2 * (n : ℝ)
  let T := max 1 ((C a - log lo - v) * K)
  have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
  have hT : 0 < T := zero_lt_one.trans_le (le_max_left _ _)
  have hK : 0 < K := by dsimp [K]; positivity
  refine ⟨lo, U, g j / T, U * T, hlo, div_pos hj hT, ?_⟩
  intro d b hd hb hv
  have hlow : lo ≤ d := le_of_not_gt fun h ↦ (hsmall d ⟨hd, h⟩ b hb).not_ge hv
  have hbnd := hv.trans (mixed_bound hn hg hi ha hd hb (j := j))
  change v ≤ C a - log d - (g j / b + b / d) / K at hbnd
  have h1 := (div_pos hj hb).le
  have h2 := (div_pos hb hd).le
  have hhigh : d ≤ U := by
    change d ≤ exp (C a - v)
    rw [← exp_log hd, exp_le_exp]
    linarith only [hbnd, div_nonneg (add_nonneg h1 h2) hK.le]
  have hs : g j / b + b / d ≤ T := by
    apply le_trans _ (le_max_right (1 : ℝ) _)
    apply (div_le_iff₀ hK).1
    linarith only [hbnd, log_le_log hlo hlow]
  refine ⟨⟨hlow, hhigh⟩, ?_, ?_⟩
  · apply (div_le_iff₀ hT).2
    rw [mul_comm]
    exact (div_le_iff₀ hb).1 (by linarith only [hs, h2])
  · calc
      b ≤ T * d := (div_le_iff₀ hd).1 (by linarith only [hs, h1])
      _ ≤ U * T := by rw [mul_comm U]; gcongr

private theorem joint_continuous {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) {a : ℝ} (ha : 0 < a) :
    ContinuousOn (fun p : ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ p.1 + g i) a p.2)
      (Ioi 0 ×ˢ Ioi 0) := by
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
  have ht (i : Fin n) (p : ℝ × ℝ) (hp : p ∈ D) : 0 < p.1 + g i :=
    add_pos_of_pos_of_nonneg hp.1 (hg i)
  have hb (p : ℝ × ℝ) (hp : p ∈ D) : 0 < p.2 := hp.2
  have hsum : ContinuousOn (fun p : ℝ × ℝ ↦
      (∑ i : Fin n, Fatigue.logDensity a p.2 (p.1 + g i)) / n) D := by
    apply ContinuousOn.div_const
    apply continuousOn_finsetSum
    intro i _
    have h1 : ContinuousOn (fun p : ℝ × ℝ ↦ p.1 + g i) D :=
      continuous_fst.continuousOn.add continuousOn_const
    have h2 : ContinuousOn (fun p : ℝ × ℝ ↦ p.2) D := continuous_snd.continuousOn
    unfold Fatigue.logDensity
    exact (((((h1.add h2).log (fun p hp ↦ (add_pos (ht i p hp) (hb p hp)).ne')).sub
      (((h1.log (fun p hp ↦ (ht i p hp).ne')).const_mul 3).div_const 2)).sub
      ((h2.log (fun p hp ↦ (hb p hp).ne')).div_const 2)).sub continuousOn_const |>.sub
        continuousOn_const).sub
      ((((h1.div h2 (fun p hp ↦ (hb p hp).ne')).add
        (h2.div h1 (fun p hp ↦ (ht i p hp).ne'))).sub continuousOn_const).div_const _)
  exact hsum.congr fun p hp ↦ Fatigue.logLik_eq_avg_logDensity hn _ ha _

/-- With positive shape held, a finite threshold and scale maximize
exactly when the sample with its minimum subtracted has a positive gap. -/
theorem held_alpha_attains_iff {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun p : ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ p.1 + g i) a p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔ ∃ j, 0 < g j := by
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
  let L := fun p : ℝ × ℝ ↦ Fatigue.logLik (fun i ↦ p.1 + g i) a p.2
  constructor
  · rintro ⟨p, _, hm⟩
    by_contra! h
    have h0 (i : Fin n) : g i = 0 := le_antisymm (h i) (hg i)
    obtain ⟨d, hd, hv⟩ := (eventually_mem_nhdsWithin.and
      ((Shape2Fatigue.held_alpha_constant_collapse hn ha).eventually
        (eventually_gt_atTop (L p)))).exists
    have hle : L (d, d) ≤ L p := hm (show (d, d) ∈ D from ⟨hd, hd⟩)
    exact hv.not_ge (by simpa only [L, h0, add_zero] using hle)
  · intro hp
    have h11 : (1, 1) ∈ D := ⟨one_pos, one_pos⟩
    obtain ⟨lo, hi, s, S, hlo, hs, h⟩ :=
      held_alpha_competitive_rectangle hn hg hz hp ha (L (1, 1))
    let R := Icc lo hi ×ˢ Icc s S
    have hsub : R ⊆ D := fun p hp ↦ ⟨hlo.trans_le hp.1.1, hs.trans_le hp.2.1⟩
    have hr : (1, 1) ∈ R := h 1 1 one_pos one_pos le_rfl
    obtain ⟨p, hp, hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn
      ⟨(1, 1), hr⟩ ((joint_continuous hn hg ha).mono hsub)
    refine ⟨p, hsub hp, fun q hq ↦ ?_⟩
    by_cases hqL : L (1, 1) ≤ L q
    · exact hm (h q.1 q.2 hq.1 hq.2 hqL)
    · exact (le_of_not_ge hqL).trans (hm hr)

/-- Held-shape constants collapse. On nonconstant samples the likelihood
vanishes on escape from every compact subset of the joint chart. -/
theorem held_alpha_escape_classification {n : ℕ} (hn : 0 < n) {g : Fin n → ℝ}
    (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0) {a : ℝ} (ha : 0 < a) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    ((∀ i, g i = 0) →
      Tendsto (fun d ↦ Fatigue.logLik (fun i ↦ d + g i) a d) (𝓝[>] 0) atTop) ∧
    ((∃ j, 0 < g j) →
      Tendsto (fun p : D ↦ Fatigue.logLik (fun i ↦ p.val.1 + g i) a p.val.2)
        (cocompact D) atBot) := by
  intro D
  constructor
  · intro h
    simpa only [h, add_zero] using Shape2Fatigue.held_alpha_constant_collapse hn ha
  · intro hp
    refine tendsto_atBot.2 fun v ↦ ?_
    obtain ⟨lo, hi, s, S, hlo, hs, h⟩ := held_alpha_competitive_rectangle hn hg hz hp ha v
    let R := Icc lo hi ×ˢ Icc s S
    have hsub : R ⊆ D := fun p hp ↦ ⟨hlo.trans_le hp.1.1, hs.trans_le hp.2.1⟩
    have hR : IsCompact (Subtype.val ⁻¹' R : Set D) :=
      Topology.IsEmbedding.subtypeVal.isInducing.isCompact_preimage'
        (isCompact_Icc.prod isCompact_Icc)
        (by simpa only [Subtype.range_coe] using hsub)
    have hev : ∀ᶠ p : D in cocompact D, p ∈ (Subtype.val ⁻¹' R : Set D)ᶜ :=
      hR.compl_mem_cocompact
    apply hev.mono
    intro p hOut
    exact (lt_of_not_ge fun hge ↦ hOut (h p.val.1 p.val.2 p.property.1 p.property.2 hge)).le

/-- At held shape and threshold, constant positive excesses give one
scale when shape is at most two and two reciprocal scales above two. -/
theorem held_shape_threshold_constant_scales {n : ℕ} (hn : 0 < n)
    {a t : ℝ} (ha : 0 < a) (ht : 0 < t) :
    let r := a ^ 2 - 2
    let u := t * ((r - sqrt (r ^ 2 - 4)) / 2)
    let v := t * ((r + sqrt (r ^ 2 - 4)) / 2)
    let L := Fatigue.logLik (fun _ : Fin n ↦ t) a
    {b ∈ Ioi (0 : ℝ) | IsMaxOn L (Ioi 0) b} =
      if a ≤ 2 then {t} else {u, v} := by
  intro r u v L
  let U := fun b : ℝ ↦ t / b + b / t + 2
  let G := fun w : ℝ ↦ log w / 2 - w / (2 * a ^ 2)
  let C₀ := -log t - log (2 * a) - log (2 * π) / 2 + 2 / a ^ 2
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hU (b : ℝ) (hb : 0 < b) : 4 ≤ U b ∧ (U b = 4 ↔ b = t) := by
    have he : (U b - 4) * (t * b) = (b - t) ^ 2 := by dsimp [U]; field_simp; ring
    have htb : 0 < t * b := mul_pos ht hb
    constructor
    · apply sub_nonneg.mp
      exact nonneg_of_mul_nonneg_left (he ▸ sq_nonneg (b - t)) htb
    · constructor
      · intro h
        rw [h, sub_self, zero_mul] at he
        exact sub_eq_zero.mp (sq_eq_zero_iff.mp he.symm)
      · rintro rfl
        norm_num [U, ht.ne']
  have hL (b : ℝ) (hb : 0 < b) : L b = C₀ + G (U b) := by
    have he : U b = (t + b) ^ 2 / (t * b) := by dsimp [U]; field_simp; ring
    have hl : log (U b) = 2 * log (t + b) - (log t + log b) := by
      rw [he, log_div (by positivity) (mul_ne_zero ht.ne' hb.ne'),
        log_pow, log_mul ht.ne' hb.ne']
      norm_num
    change Fatigue.logLik (fun _ : Fin n ↦ t) a b = _
    rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.avg]
    simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
      mul_div_cancel_left₀ _ hnR, C₀, G, hl, Fatigue.logDensity]
    dsimp [U]
    ring
  have hstrict {w W : ℝ} (hw : 0 < w) (hW : 0 < W) (hne : w ≠ W) (h : a ^ 2 = W ∨
      (a ^ 2 ≤ W ∧ W < w)) : G w < G W := by
    have hlog := log_lt_sub_one_of_pos (div_pos hw hW)
      (fun he ↦ hne ((div_eq_one_iff_eq hW.ne').1 he))
    rw [log_div hw.ne' hW.ne'] at hlog
    have hc : (w / W - 1) ≤ (w - W) / a ^ 2 := by
      rw [show w / W - 1 = (w - W) / W by field_simp]
      rcases h with h | ⟨h, hw⟩
      · rw [h]
      · exact div_le_div_of_nonneg_left (sub_nonneg.mpr hw.le) (sq_pos_of_pos ha) h
    dsimp [G]
    linarith only [hlog, hc, show (w - W) / a ^ 2 =
      2 * (w / (2 * a ^ 2) - W / (2 * a ^ 2)) by ring]
  by_cases ha2 : a ≤ 2
  · rw [ite_eq_left ha2]
    have htU : U t = 4 := (hU t ht).2.mpr rfl
    have hwin (b : ℝ) (hb : 0 < b) (hne : b ≠ t) : L b < L t := by
      have hu : 4 < U b := lt_of_le_of_ne (hU b hb).1 (fun h ↦ hne ((hU b hb).2.mp h.symm))
      rw [hL b hb, hL t ht, htU]
      exact add_lt_add_right (hstrict (by linarith) (by norm_num) hu.ne'
        (Or.inr ⟨by nlinarith, hu⟩)) _
    ext b
    simp only [mem_ofPred_eq, mem_singleton_iff]
    constructor
    · rintro ⟨hb, hm⟩
      by_contra h
      exact (hwin b hb h).not_ge (hm ht)
    · intro he
      subst b
      refine ⟨ht, fun w hw ↦ ?_⟩
      by_cases h : w = t
      · subst w
        exact le_rfl
      · exact (hwin w hw h).le
  · rw [ite_eq_right ha2]
    have har : 2 < r := by dsimp [r]; nlinarith [lt_of_not_ge ha2]
    let d := sqrt (r ^ 2 - 4)
    have hdis : 0 < r ^ 2 - 4 := by nlinarith
    have hd : 0 < d := sqrt_pos.mpr hdis
    have hd2 : d ^ 2 = r ^ 2 - 4 := sq_sqrt hdis.le
    have hdr : d < r :=
      (sq_lt_sq₀ hd.le (show 0 ≤ r by linarith)).mp (by rw [hd2]; linarith)
    have hu : 0 < u := mul_pos ht (half_pos (sub_pos.mpr hdr))
    have hv : 0 < v := mul_pos ht (half_pos (add_pos (by linarith) hd))
    have hRoots (b : ℝ) (hb : 0 < b) : U b = a ^ 2 ↔ b = u ∨ b = v := by
      have huv₁ : u + v = t * r := by
        change t * ((r - d) / 2) + t * ((r + d) / 2) = t * r
        ring
      have huv₂ : u * v = t ^ 2 := by
        change t * ((r - d) / 2) * (t * ((r + d) / 2)) = t ^ 2
        linear_combination -(t ^ 2 / 4) * hd2
      have h : (U b - a ^ 2) * (t * b) = (b - u) * (b - v) := by
        calc
          _ = b ^ 2 - b * (t * r) + t ^ 2 := by dsimp [U, r]; field_simp; ring
          _ = _ := by linear_combination b * huv₁ - huv₂
      have htb : t * b ≠ 0 := mul_ne_zero ht.ne' hb.ne'
      rw [← sub_eq_zero, ← mul_left_inj' htb, zero_mul, h, mul_eq_zero]
      exact or_congr sub_eq_zero sub_eq_zero
    have hUu := (hRoots u hu).mpr (Or.inl rfl)
    have hUv := (hRoots v hv).mpr (Or.inr rfl)
    have hmax (b : ℝ) (hb : 0 < b) : L b ≤ C₀ + G (a ^ 2) := by
      rw [hL b hb]
      by_cases he : U b = a ^ 2
      · rw [he]
      · exact (add_lt_add_right (hstrict (by linarith [(hU b hb).1])
          (sq_pos_of_pos ha) he (Or.inl rfl)) _).le
    ext b
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hb, hm⟩
      apply (hRoots b hb).mp
      by_contra he
      have hlt := add_lt_add_right (hstrict
        (show 0 < U b by linarith [(hU b hb).1]) (sq_pos_of_pos ha) he (Or.inl rfl)) C₀
      have h : L u ≤ L b := hm hu
      rw [hL u hu, hUu, hL b hb] at h
      exact hlt.not_ge h
    · intro h
      have hp : 0 < b := by rcases h with rfl | rfl <;> assumption
      refine ⟨hp, fun s hs ↦ ?_⟩
      have he := (hRoots b hp).mpr h
      change L s ≤ L b
      rw [hL b hp, he]
      exact hmax s hs

private def gapScore (rate t : ℝ) : ℝ :=
  (t + 1)⁻¹ - 3 / (2 * t) - rate * (1 - t⁻¹ ^ 2)

private def gapScoreDeriv (rate t : ℝ) : ℝ :=
  -(t + 1)⁻¹ ^ 2 + 3 / (2 * t ^ 2) - rate * (2 / t ^ 3)

private theorem hasDerivAt_gapScore (rate : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (gapScore rate) (gapScoreDeriv rate t) t := by
  have h1 := ((hasDerivAt_id t).add_const 1).inv (by positivity : 0 < t + 1).ne'
  have h2 := (hasDerivAt_inv ht.ne').const_mul (3 / 2)
  have h3 := (((hasDerivAt_inv ht.ne').pow 2).const_sub 1).const_mul rate
  convert (h1.sub h2).sub h3 using 1
  · funext v
    simp only [gapScore, Pi.sub_apply, Pi.inv_apply, Pi.pow_apply, id_eq]
    ring
  · simp only [gapScoreDeriv, id_eq, show (2 : ℕ) - 1 = 1 by decide, pow_one, ← inv_pow]
    field_simp [ht.ne']
    ring

private theorem hasDerivAt_gapDensity (a : ℝ) {t : ℝ} (ht : 0 < t) :
    HasDerivAt (Fatigue.logDensity a 1) (gapScore (1 / (2 * a ^ 2)) t) t := by
  have ht1 : 0 < t + 1 := by positivity
  have h := ((((hasDerivAt_id t).add_const 1).log ht1.ne').sub
    (((hasDerivAt_log ht.ne').const_mul 3).div_const 2)).sub_const
      (log (2 * a) + log (2 * π) / 2)
  have h2 := (((hasDerivAt_id t).add (hasDerivAt_inv ht.ne')).sub_const 2).div_const
    (2 * a ^ 2)
  convert h.sub h2 using 1
  · funext v
    simp only [Fatigue.logDensity, Pi.sub_apply, Pi.add_apply, id_eq, log_one, zero_div,
      sub_zero, div_one, one_div]
    ring
  · simp only [gapScore, id_eq, one_div]
    ring

private theorem damped_score_neg {rate t : ℝ} (hrate : 1 ≤ rate) (ht : 0 < t) :
    gapScore rate t + gapScoreDeriv rate t < 0 := by
  have ht1 : 0 < t + 1 := by positivity
  have he :
      gapScore rate t + gapScoreDeriv rate t =
        t / (t + 1) ^ 2 + 3 * (1 - t) / (2 * t ^ 2) -
          rate * (1 - 1 / t ^ 2 + 2 / t ^ 3) := by
    dsimp [gapScore, gapScoreDeriv]
    field_simp
    ring
  have hquarter : t / (t + 1) ^ 2 ≤ 1 / 4 := by
    apply (div_le_iff₀ (sq_pos_of_pos ht1)).2
    nlinarith [sq_nonneg (t - 1)]
  rw [he]
  by_cases Ht : 1 ≤ t
  · have hmid : 3 * (1 - t) / (2 * t ^ 2) ≤ 0 := by
      exact div_nonpos_of_nonpos_of_nonneg (by linarith) (by positivity)
    have hlast : 3 / 4 ≤ 1 - 1 / t ^ 2 + 2 / t ^ 3 := by
      by_cases ht2 : 2 ≤ t
      · have hsq : 4 ≤ t ^ 2 := by
          have h := mul_nonneg (sub_nonneg.mpr ht2) (show 0 ≤ t + 2 by positivity)
          nlinarith only [h]
        have hq : 1 / t ^ 2 ≤ 1 / 4 := (div_le_iff₀ (sq_pos_of_pos ht)).2 (by linarith)
        linarith only [hq, show 0 ≤ 2 / t ^ 3 by positivity]
      · have hq : 1 / t ^ 2 ≤ 2 / t ^ 3 := by
          rw [div_le_div_iff₀ (sq_pos_of_pos ht) (by positivity)]
          nlinarith only [mul_nonneg (show 0 ≤ t ^ 2 by positivity)
            (show 0 ≤ 2 - t by linarith)]
        linarith only [hq]
    have h := mul_le_mul_of_nonneg_right hrate (show 0 ≤ 1 - 1 / t ^ 2 + 2 / t ^ 3 by linarith)
    linarith only [hquarter, hmid, hlast, h]
  · have ht' : t < 1 := lt_of_not_ge Ht
    have hpow : t ^ 3 ≤ 1 := pow_le_one₀ ht.le ht'.le
    have h1 := mul_le_mul_of_nonneg_right hquarter (show 0 ≤ t ^ 3 by positivity)
    have h2 : (3 * (1 - t) / (2 * t ^ 2)) * t ^ 3 ≤ 3 / 8 := by
      have he : (3 * (1 - t) / (2 * t ^ 2)) * t ^ 3 = (3 / 2) * t * (1 - t) := by
        field_simp
      rw [he]
      nlinarith [sq_nonneg (t - 1 / 2)]
    have h3 : (1 - 1 / t ^ 2 + 2 / t ^ 3) * t ^ 3 = t ^ 3 - t + 2 := by
      field_simp
    have h4 : 1 ≤ t ^ 3 - t + 2 := by linarith [show 0 ≤ t ^ 3 by positivity]
    refine neg_of_mul_neg_left ?_ (show 0 ≤ t ^ 3 by positivity)
    rw [sub_mul, add_mul, mul_assoc, h3]
    nlinarith only [h1, hpow, h2, h4,
      mul_le_mul_of_nonneg_right hrate (show 0 ≤ t ^ 3 - t + 2 by linarith)]

private theorem normalized_gap_derivative {n : ℕ} (hn : 0 < n)
    {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) {a d : ℝ} (ha : 0 < a) (hd : 0 < d) :
    HasDerivAt (fun e ↦ Fatigue.logLik (fun i ↦ e + g i) a 1)
      (Fatigue.avg (fun i ↦ gapScore (1 / (2 * a ^ 2)) (d + g i))) d := by
  have h := (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦
    ((hasDerivAt_gapDensity a (add_pos_of_pos_of_nonneg hd (hg i))).comp d
      ((hasDerivAt_id d).add_const (g i))))).div_const (n : ℝ)
  refine (h.congr_of_eventuallyEq ?_).congr_deriv ?_
  · filter_upwards [] with e
    exact (Fatigue.logLik_eq_avg_logDensity hn _ ha _)
  · simp only [Fatigue.avg, mul_one]

private theorem scale_normalize {n : ℕ} (hn : 0 < n) (g : Fin n → ℝ)
    {a b d : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hg : ∀ i, 0 ≤ g i) (hd : 0 < d) :
    Fatigue.logLik (fun i ↦ d + g i) a b =
      Fatigue.logLik (fun i ↦ d / b + g i / b) a 1 - log b := by
  rw [Fatigue.logLik_eq_avg_logDensity hn _ ha, Fatigue.logLik_eq_avg_logDensity hn _ ha]
  have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have he (i : Fin n) : Fatigue.logDensity a b (d + g i) =
      Fatigue.logDensity a 1 ((d + g i) / b) - log b := by
    have ht : 0 < d + g i := add_pos_of_pos_of_nonneg hd (hg i)
    unfold Fatigue.logDensity
    rw [div_add_one hb.ne', log_div (add_pos ht hb).ne' hb.ne', log_div ht.ne' hb.ne']
    simp only [log_one, zero_div, div_one, sub_zero]
    field_simp
    ring
  simp only [Fatigue.avg, he, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← add_div, sub_div,
    mul_div_cancel_left₀ _ hnR]

/-- The threshold fit at held shape and scale is a singleton for every
sample when squared shape is at most one half, and for every constant
sample at arbitrary positive shape. -/
theorem held_shape_scale_threshold_unique {n : ℕ} (hn : 0 < n)
    {g : Fin n → ℝ} (hg : ∀ i, 0 ≤ g i) (hz : ∃ i, g i = 0)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (h : a ^ 2 ≤ 1 / 2 ∨ ∀ i, g i = 0) :
    ∃! d, d ∈ Ioi (0 : ℝ) ∧ IsMaxOn
      (fun e ↦ Fatigue.logLik (fun i ↦ e + g i) a b) (Ioi 0) d := by
  let G := fun i ↦ g i / b
  have hG (i : Fin n) : 0 ≤ G i := div_nonneg (hg i) hb.le
  let f := fun e : ℝ ↦ Fatigue.logLik (fun i ↦ e + g i) a b
  let l := fun e : ℝ ↦ Fatigue.logLik (fun i ↦ e + G i) a 1
  let φ := fun e : ℝ ↦ e / b
  have hφ : BijOn φ (Ioi (0 : ℝ)) (Ioi 0) := by
    refine ⟨fun e he ↦ div_pos he hb, ?_, ?_⟩
    · intro e he d hd h
      exact (div_left_inj₀ hb.ne').mp h
    · intro e he
      exact ⟨e * b, mul_pos he hb, mul_div_cancel_right₀ _ hb.ne'⟩
  have hl (e : ℝ) (he : e ∈ Ioi 0) : f e = l (φ e) + (-log b) := by
    simpa only [f, l, φ, G, sub_eq_add_neg] using scale_normalize hn g ha hb hg he
  apply (Transfer.existsUnique_max_iff (g := l) hφ hl).mpr
  obtain ⟨j, hj⟩ := hz
  obtain ⟨d, hd, hm⟩ := Shape2Fatigue.held_alpha_beta_attains hn hG
    ⟨j, by simp [G, hj]⟩ ha one_pos
  let rate := 1 / (2 * a ^ 2)
  let S := fun e : ℝ ↦ Fatigue.avg (fun i ↦ gapScore rate (e + G i))
  have hzS (e : ℝ) (he : e ∈ Ioi 0) (hm : IsMaxOn l (Ioi 0) e) : S e = 0 :=
    (hm.isLocalMax (Ioi_mem_nhds he)).hasDerivAt_eq_zero
      (normalized_gap_derivative hn hG ha he)
  refine ⟨d, ⟨hd, hm⟩, fun e he ↦ ?_⟩
  have hd0 := hzS d hd hm
  have he0 := hzS e he.1 he.2
  rcases h with hsmall | hconst
  · have hrate : 1 ≤ rate := (le_div_iff₀ (show 0 < 2 * a ^ 2 by positivity)).2 (by
      linarith only [hsmall])
    have hD (u : ℝ) (hu : 0 < u) :
        HasDerivAt S (Fatigue.avg (fun i ↦ gapScoreDeriv rate (u + G i))) u := by
      have h := (HasDerivAt.fun_sum (u := Finset.univ) (fun i _ ↦
        ((hasDerivAt_gapScore rate (add_pos_of_pos_of_nonneg hu (hG i))).comp u
          ((hasDerivAt_id u).add_const (G i))))).div_const (n : ℝ)
      simpa only [S, Fatigue.avg, mul_one, Function.comp_def, id_eq] using h
    have hAnti : StrictAntiOn (fun u ↦ exp u * S u) (Ioi 0) := by
      apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
      · exact fun u hu ↦ ((hasDerivAt_exp u).mul (hD u hu)).continuousAt.continuousWithinAt
      · intro u hu
        rw [interior_Ioi] at hu
        change deriv (exp * S) u < 0
        rw [((hasDerivAt_exp u).mul (hD u hu)).deriv]
        have hnR : 0 < (n : ℝ) := by exact_mod_cast hn
        have hs : S u + Fatigue.avg (fun i ↦ gapScoreDeriv rate (u + G i)) < 0 := by
          dsimp [S, Fatigue.avg]
          rw [← add_div, ← Finset.sum_add_distrib]
          exact div_neg_of_neg_of_pos (Finset.sum_neg (fun i _ ↦
            damped_score_neg hrate (add_pos_of_pos_of_nonneg hu (hG i)))
              ⟨j, Finset.mem_univ _⟩) hnR
        nlinarith only [mul_neg_of_pos_of_neg (exp_pos u) hs]
    apply hAnti.injOn he.1 hd
    change exp e * S e = exp d * S d
    simp only [he0, hd0, mul_zero]
  · have hc (i : Fin n) : G i = 0 := by simp [G, hconst]
    have hnR : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    have hS (u : ℝ) : S u = gapScore rate u := by
      simp [S, Fatigue.avg, hc, nsmul_eq_mul, hnR]
    let P := fun t : ℝ ↦ t ^ 2 + (1 + a ^ 2) * t + (3 * a ^ 2 - 1) - t⁻¹
    have hroot (t : ℝ) (ht : 0 < t) (hz : S t = 0) : P t = 0 := by
      rw [hS] at hz
      have hk : gapScore rate t * (2 * a ^ 2 * t * (t + 1)) = -P t := by
        dsimp [gapScore, rate, P]
        field_simp [ha.ne', ht.ne', (show 0 < t + 1 by positivity).ne']
        ring
      rw [hz, zero_mul] at hk
      linarith
    have hmono : StrictMonoOn P (Ioi 0) := by
      intro s hs t ht hst
      have h₁ : s ^ 2 < t ^ 2 := sq_lt_sq₀ hs.le ht.le |>.mpr hst
      have h₂ := mul_lt_mul_of_pos_left hst (show 0 < 1 + a ^ 2 by positivity)
      have h₃ := (inv_lt_inv₀ ht hs).2 hst
      dsimp [P]
      linarith only [h₁, h₂, h₃]
    exact hmono.injOn he.1 hd ((hroot e he.1 he0).trans (hroot d hd hd0).symm)

end
end UnivariateML.Shape3Fatigue
end
