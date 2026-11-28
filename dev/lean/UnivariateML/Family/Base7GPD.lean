module

public import UnivariateML.Family.LocScaleGPD

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! Generalized Pareto failure with threshold held.

## Deviations
The fixed-threshold singularity is expressed using finite interior likelihoods.
The fixed-shape reference argmax uses the infinite upper-contact convention.
-/

@[expose] public section

namespace UnivariateML.Base7GPD

open Set Filter Real GPD LocScaleGPD
open scoped Topology

private theorem below_uniform_unbounded (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 ≤ y i)
    (hz : ¬ ∀ i, y i = 0)
    {ξ : ℝ} (hξ : ξ < -1) (C : ℝ) :
    ∃ σ ∈ scaleDomain y ξ, C < logLik y σ ξ := by
  classical
  obtain ⟨j, _, hj'⟩ := Finset.exists_max_image Finset.univ y
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hj i : y i ≤ y j := hj' i (Finset.mem_univ _)
  have hyj : 0 < y j := by
    by_contra! h
    exact hz (fun i ↦ (le_trans (hj i) h).antisymm (hy i))
  let c := -ξ * y j
  let p := 1 + 1 / ξ
  have hc : 0 < c := by dsimp [c]; exact mul_pos (by linarith) hyj
  have hp : 0 < p := by
    dsimp [p]
    have h : -1 < 1 / ξ := by
      rw [lt_div_iff_of_neg (show ξ < 0 by linarith)]
      linarith
    linarith
  have hξ₀ : ξ ≠ 0 := by linarith
  have hξ₁ : ξ ≠ -1 := by linarith
  let t := max 0 ((C + (n : ℝ) * log (c + 1)) / p - log c) + 1
  have ht : 0 < t := by dsimp [t]; positivity
  let σ := c + exp (-t)
  have hσ : 0 < σ := by dsimp [σ]; positivity
  have hs₁ : σ ≤ c + 1 := by
    dsimp [σ]
    have : exp (-t) ≤ 1 := exp_le_one_iff.mpr (by linarith)
    linarith
  have hlog := strictMonoOn_log.monotoneOn hσ (show 0 < c + 1 by positivity) hs₁
  have hsupport (i) : 0 < σ + ξ * y i := by
    have hm := mul_le_mul_of_nonpos_left (hj i) (show ξ ≤ 0 by linarith)
    dsimp [σ, c]
    linarith [exp_pos (-t)]
  have hbase (i) : 0 < 1 + ξ * (y i / σ) := by
    convert div_pos (hsupport i) hσ using 1
    field_simp
  have hle (i) : log (1 + ξ * (y i / σ)) ≤ 0 := by
    apply log_nonpos (hbase i).le
    have := mul_nonpos_of_nonpos_of_nonneg (show ξ ≤ 0 by linarith)
      (div_nonneg (hy i) hσ.le)
    linarith
  have hjlog : log (1 + ξ * (y j / σ)) = -t - log σ := by
    have he : 1 + ξ * (y j / σ) = exp (-t) / σ := by
      field_simp
      simp [σ, c]
    rw [he, log_div (exp_ne_zero _) hσ.ne', log_exp]
  have hlow := strictMonoOn_log.monotoneOn hc hσ
    (le_add_of_nonneg_right (exp_pos (-t)).le)
  refine ⟨σ, ⟨hσ, fun i ↦ ⟨(hy i), by simpa [hξ₁] using hsupport i⟩⟩, ?_⟩
  have hLL : logLik y σ ξ =
      (n : ℝ) * (-log σ) + ∑ i, -(p * log (1 + ξ * (y i / σ))) := by
    simp only [logLik, logDensity, logRatio, ite_eq_right hξ₀, sub_eq_add_neg,
      Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, p]
    congr 1
    apply Finset.sum_congr rfl
    intro i _
    field_simp
    ring
  rw [hLL, ← Finset.sum_erase_add _ _ (Finset.mem_univ j), hjlog]
  have hsum : 0 ≤ ∑ i ∈ Finset.univ.erase j, -(p * log (1 + ξ * (y i / σ))) :=
    Finset.sum_nonneg fun i _ ↦ neg_nonneg.mpr (mul_nonpos_of_nonneg_of_nonpos hp.le (hle i))
  have hnt : (0 : ℝ) ≤ n := by positivity
  have hC := le_max_right 0 ((C + (n : ℝ) * log (c + 1)) / p - log c)
  have hct : (C + (n : ℝ) * log (c + 1)) / p < t + log c := by
    dsimp [t]
    linarith
  have hct' := (div_lt_iff₀ hp).mp hct
  nlinarith only [hct', hsum, mul_le_mul_of_nonneg_left hlog hnt,
    mul_le_mul_of_nonneg_left hlow hp.le]
/-- At a held threshold, ties give a divergent fitted path, positive excesses
attain, and every reference shape below minus one has an unbounded objective. -/
theorem held_threshold_failure {n : ℕ} (hn : 0 < n) (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) :
    ((∃ i, y i = 0) →
      (∀ t : ℝ, exp (-t) ∈ scaleDomain y (2 * n)) ∧
      Tendsto (fun t : ℝ ↦ logLik y (exp (-t)) (2 * n)) atTop atTop) ∧
    ((∀ i, 0 < y i) → ∃ s ξ, -1 ≤ ξ ∧ s ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ t ∈ scaleDomain y κ, logLik y t κ ≤ logLik y s ξ) ∧
    (∀ ξ < -1, ∀ B : ℝ, ∃ s ∈ scaleDomain y ξ, B < logLik y s ξ) := by
  refine ⟨?_, (held_threshold_criterion hn y hy).mpr, ?_⟩
  · rintro ⟨i, hi⟩
    exact held_threshold_tie_path hn y hy hi
  · intro ξ hξ B
    by_cases hz : ∀ i, y i = 0
    · rw [show y = fun _ ↦ 0 from funext hz]
      obtain ⟨hs, hl⟩ := constant_fixed_shape_collapse hn ξ
      have hpos : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi 0 := self_mem_nhdsWithin
      obtain ⟨s, hB, hpos⟩ := ((hl.eventually_gt_atTop B).and hpos).exists
      exact ⟨s, hs s hpos, hB⟩
    · exact below_uniform_unbounded hn hy hz hξ B

/-- At fixed threshold and scale the optimum is attained. Positive
excesses exclude the infinite-shape end and finite nonuniform upper
contact has log likelihood tending to minus infinity. The uniform
contact belongs to the domain. -/
theorem held_threshold_scale_failure {n : ℕ} (y : Fin n → ℝ)
    (hy : ∀ i, 0 ≤ y i) {s : ℝ} (hs : 0 < s) :
    let D := shapeDomain y s
    let L := logLik y s
    (∃ ξ ∈ D, IsMaxOn L D ξ) ∧
    ((-1 : ℝ) ∈ D ↔ ∀ i, y i ≤ s) ∧
    ((∀ i, y i = 0) → ∀ ξ ∈ D, L ξ = -(n : ℝ) * log s) ∧
    (∀ ξ : ℕ → ℝ, (∀ᶠ k in atTop, ξ k ∈ D) → Tendsto ξ atTop atTop →
      (∃ i, 0 < y i) → Tendsto (L ∘ ξ) atTop atBot) ∧
    (∀ (ξ : ℕ → ℝ) (κ : ℝ), -1 < κ →
      (∀ᶠ k in atTop, ξ k ∈ D) → Tendsto ξ atTop (𝓝 κ) →
      (∃ i, 0 < y i ∧ s + κ * y i = 0) → Tendsto (L ∘ ξ) atTop atBot) := by
  dsimp only
  refine ⟨held_threshold_scale_attains y hy hs, ?_, ?_, ?_, ?_⟩
  · simp [shapeDomain, scaleDomain, hs, hy]
  · exact fun h ↦ (held_scale_all_zero s hs y h).2
  · rintro ξ hv hξ ⟨i, hi⟩
    exact infinite_shape_positive_excess y hs (fun _ ↦ 0) ξ hξ
      (by simpa using hv) (j := i) hi (Eventually.of_forall (fun _ ↦ by simp))
      |>.congr (fun _ ↦ by simp)
  · rintro ξ κ hκ hv hξ ⟨i, hi, hc⟩
    have h := held_scale_nonuniform_upper_contact y hs hκ (fun _ ↦ 0) ξ
      (M := 0) tendsto_const_nhds hξ (by simpa using hv)
      (j := i) (by simpa using hi) (by simpa using hc)
    simpa [Function.comp_def] using h

/-- In the fitted family every attained shape-held fit is unique and uses
the sample minimum. Below minus one, putting infinite log likelihood at
the compatible upper contact gives exactly the stated contact pairs. -/
theorem held_shape_maximizers {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (ξ : ℝ) :
    let D := {p : ℝ × ℝ | p.2 ∈ scaleDomain (fun k ↦ x k - p.1) ξ}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) p.2 ξ
    let R := {p : ℝ × ℝ | 0 < p.2 ∧
      ∀ k, 0 ≤ x k - p.1 ∧ 0 ≤ p.2 + ξ * (x k - p.1)}
    let E := fun p : ℝ × ℝ ↦
      if ∃ k, p.2 + ξ * (x k - p.1) = 0 then (⊤ : WithTop ℝ) else (L p : WithTop ℝ)
    ((∃ p ∈ D, IsMaxOn L D p) ↔
      -1 ≤ ξ ∧ x i < x j ∧ (ξ ≤ 0 ∨ 0 < collapseExponent (fun k ↦ x k - x i) ξ)) ∧
    (∀ p ∈ D, IsMaxOn L D p → p.1 = x i ∧
      ∀ q ∈ D, IsMaxOn L D q → q = p) ∧
    (ξ < -1 → ∀ p, (p ∈ R ∧ IsMaxOn E R p) ↔
      p.1 ≤ x i ∧ p.1 < x j ∧ p.2 = -ξ * (x j - p.1)) := by
  classical
  intro D L R E
  have H := held_shape_finite_criterion hn x hi hj ξ
  have equiv (p : ℝ × ℝ) : IsMaxOn L D p ↔
      ∀ m s, s ∈ scaleDomain (fun k ↦ x k - m) ξ →
        logLik (fun k ↦ x k - m) s ξ ≤ logLik (fun k ↦ x k - p.1) p.2 ξ := by
    exact ⟨fun h m s hms ↦ h (a := (m, s)) hms, fun h v hv ↦ h v.1 v.2 hv⟩
  refine ⟨?_, ?_, ?_⟩
  · exact ⟨fun ⟨p, hp, hm⟩ ↦ H.1.mp ⟨p, hp, (equiv p).mp hm⟩,
      fun h ↦ by
        obtain ⟨p, hp, hm⟩ := H.1.mpr h
        exact ⟨p, hp, (equiv p).mpr hm⟩⟩
  · intro p hp hm
    have hf := H.1.mp ⟨p, hp, (equiv p).mp hm⟩
    refine ⟨?_, fun q hq hqm ↦ H.2 q p ⟨hq, (equiv q).mp hqm⟩ ⟨hp, (equiv p).mp hm⟩⟩
    by_cases hu : ξ = -1
    · subst ξ
      exact ((held_uniform_shape_unique hn x hi hj hf.2.1 p.1 p.2).mp ⟨hp, (equiv p).mp hm⟩).1
    · have hle : p.1 ≤ x i := sub_nonneg.mp (hp.2 i).1
      by_contra hne
      have hlt := lt_of_le_of_ne hle hne
      obtain ⟨hD, hstrict⟩ := nonuniform_location_strict hn x hu ⟨hf.1, hp⟩ hi hlt
      exact hstrict.not_ge (hm (a := (x i, p.2)) hD.2)
  · intro hξ
    have hξneg : ξ < 0 := by linarith
    have hc (p : ℝ × ℝ) : (p ∈ R ∧ (∃ k, p.2 + ξ * (x k - p.1) = 0)) ↔
        p.1 ≤ x i ∧ p.1 < x j ∧ p.2 = -ξ * (x j - p.1) := by
      constructor
      · rintro ⟨hp, k, hk⟩
        have h0 := (hp.2 j).2
        have h1 := mul_le_mul_of_nonpos_left (sub_le_sub_right (hj k) p.1) hξneg.le
        have he : p.2 = -ξ * (x j - p.1) := by linarith
        have hpos : 0 < -ξ * (x j - p.1) := he ▸ hp.1
        exact ⟨sub_nonneg.mp (hp.2 i).1,
          sub_pos.mp (pos_of_mul_pos_right hpos (by linarith : 0 ≤ -ξ)), he⟩
      · rintro ⟨hmin, hmax, he⟩
        refine ⟨⟨he.symm ▸ mul_pos (neg_pos.mpr hξneg) (sub_pos.mpr hmax), fun k ↦ ?_⟩,
          j, by rw [he]; ring⟩
        refine ⟨sub_nonneg.mpr (hmin.trans (hi k)), ?_⟩
        rw [he]
        linarith only [mul_le_mul_of_nonpos_left (sub_le_sub_right (hj k) p.1) hξneg.le]
    let q := (x i - 1, -ξ * (x j - (x i - 1)))
    have hq := (hc q).mpr ⟨by dsimp [q]; linarith, by dsimp [q]; linarith [hi j], rfl⟩
    have hEq : E q = ⊤ := by simp [E, hq.2]
    intro p
    rw [← hc]
    constructor
    · rintro ⟨hp, hm⟩
      refine ⟨hp, ?_⟩
      have he : E p = ⊤ := top_unique (hEq ▸ hm hq.1)
      by_contra h
      simp [E, h] at he
    · rintro ⟨hp, hs⟩
      exact ⟨hp, fun r _ ↦ by simp [E, hs]⟩

/-- At fixed shape, constant data collapse. The positive-shape exponent
separates divergence from the finite, strictly unattained critical height.
The reference family below minus one has an unbounded interior objective. -/
theorem held_shape_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ k, x i ≤ x k) (ξ : ℝ) :
    let y := fun k ↦ x k - x i
    let V := {v | ∃ m s, s ∈ scaleDomain (fun k ↦ x k - m) ξ ∧
      v = logLik (fun k ↦ x k - m) s ξ}
    ((∀ k, x k = x i) → Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop) ∧
    (0 < ξ → collapseExponent y ξ < 0 →
      Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop) ∧
    ((∃ k, x i < x k) → 0 < ξ → collapseExponent y ξ = 0 →
      let H := ∑ k, if y k = 0 then (0 : ℝ) else -(1 + 1 / ξ) * log (ξ * y k)
      IsLUB V H ∧ ∀ v ∈ V, v < H) ∧
    (ξ < -1 → ¬ BddAbove V) := by
  intro y V
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro h
    have hy : y = fun _ ↦ 0 := funext (fun k ↦ sub_eq_zero.mpr (h k))
    rw [hy]
    exact (constant_fixed_shape_collapse hn ξ).2
  · exact held_positive_shape_negative_exponent y (fun k ↦ sub_nonneg.mpr (hi k))
  · intro hnc hξ hA
    have h := held_positive_shape_joint_critical_face hn x hi hnc hξ hA
    refine ⟨h.1, ?_⟩
    rintro _ ⟨m, s, hs, rfl⟩
    exact h.2 m s hs
  · intro hξ
    obtain ⟨m, _, hm⟩ := held_shape_below_uniform_unbounded hn x hξ
    rintro ⟨B, hB⟩
    obtain ⟨s, hs, hb⟩ := hm B
    exact hb.not_ge (hB ⟨m, s, hs, rfl⟩)

/-- At held scale a finite optimum always exists. If the shape diverges
and the log likelihood stays bounded below, the sample is constant and
the location converges to that common observation. -/
theorem held_scale_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    (∃ m ξ, ξ ∈ shapeDomain (fun i ↦ x i - m) s ∧
      ∀ q κ, κ ∈ shapeDomain (fun i ↦ x i - q) s →
        logLik (fun i ↦ x i - q) s κ ≤ logLik (fun i ↦ x i - m) s ξ) ∧
    (∀ (m ξ : ℕ → ℝ) (B : ℝ),
      (∀ᶠ t in atTop, ξ t ∈ shapeDomain (fun i ↦ x i - m t) s) →
      Tendsto ξ atTop atTop →
      (∀ᶠ t in atTop, B ≤ logLik (fun i ↦ x i - m t) s (ξ t)) →
      ∃ M, (∀ i, x i = M) ∧ Tendsto m atTop (𝓝 M)) := by
  refine ⟨held_scale_attains hn x hs, ?_⟩
  intro m ξ B hv hξ hb
  have hc : ∀ i j, x i = x j := by
    intro i j
    by_contra h
    have hne : ∃ k l, x k < x l := by
      rcases lt_or_gt_of_ne h with hlt | hlt
      · exact ⟨i, j, hlt⟩
      · exact ⟨j, i, hlt⟩
    have hbot := infinite_shape_nonconstant x hs m ξ hξ hv hne
    obtain ⟨_, hl, hg⟩ := ((hbot.eventually_lt_atBot B).and hb).exists
    exact hl.not_ge hg
  let i : Fin n := ⟨0, hn⟩
  refine ⟨x i, fun j ↦ hc j i, tendsto_order.2 ⟨?_, ?_⟩⟩
  · intro a ha
    by_contra h
    let f := atTop ⊓ 𝓟 {t : ℕ | m t ≤ a}
    have hfreq : ∃ᶠ t in atTop, m t ≤ a := by simpa [Filter.Frequently] using h
    have : f.NeBot := frequently_iff_neBot.mp hfreq
    have hf : f ≤ atTop := inf_le_left
    have hδ : ∀ᶠ t in f, x i - a ≤ x i - m t := by
      apply eventually_inf_principal.mpr
      exact Eventually.of_forall (fun t (ht : m t ≤ a) ↦ sub_le_sub_left ht _)
    have hbot := infinite_shape_positive_excess x hs m ξ (hξ.mono_left hf)
      (hv.filter_mono hf) (sub_pos.mpr ha) hδ
    obtain ⟨_, hl, hg⟩ := ((hbot.eventually_lt_atBot B).and (hb.filter_mono hf)).exists
    exact hl.not_ge hg
  · intro b hb'
    filter_upwards [hv] with t ht
    exact (sub_nonneg.mp (ht.2.2 i).1).trans_lt hb'

/-- On constant data with held scale, a convergent scaled location gap
along the infinite-shape end gives the stated positive trace. -/
theorem held_scale_constant_infinite_shape_trace {n : ℕ} (M : ℝ) {s : ℝ} (hs : 0 < s)
    (m ξ : ℕ → ℝ) (hξ : Tendsto ξ atTop atTop) {v : ℝ} (hv : 0 ≤ v)
    (hgap : Tendsto (fun t ↦ ξ t * ((M - m t) / s)) atTop (𝓝 v)) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ M - m t) s (ξ t)) atTop
      (𝓝 (-(n : ℝ) * (log s + log (1 + v)))) := by
  have hinv : Tendsto (fun t ↦ (ξ t)⁻¹) atTop (𝓝 0) := tendsto_inv_atTop_zero.comp hξ
  have he : Tendsto (fun t ↦ 1 + (ξ t)⁻¹) atTop (𝓝 1) := by
    simpa using tendsto_const_nhds.add hinv
  have harg : Tendsto (fun t ↦ 1 + ξ t * ((M - m t) / s)) atTop (𝓝 (1 + v)) :=
    tendsto_const_nhds.add hgap
  have hlog : Tendsto (fun t ↦ log (1 + ξ t * ((M - m t) / s))) atTop (𝓝 (log (1 + v))) :=
    (continuousAt_log (show 1 + v ≠ 0 by positivity)).tendsto.comp harg
  have hc : Tendsto (fun _ : ℕ ↦ log s) atTop (𝓝 (log s)) := tendsto_const_nhds
  have hlim := (hc.add (he.mul hlog)).const_mul (-(n : ℝ))
  apply (show Tendsto (fun t ↦ -(n : ℝ) *
    (log s + (1 + (ξ t)⁻¹) * log (1 + ξ t * ((M - m t) / s)))) atTop
      (𝓝 (-(n : ℝ) * (log s + log (1 + v)))) from by simpa using hlim).congr'
  filter_upwards [hξ.eventually_gt_atTop 0] with t ht
  simp only [logLik, logDensity, logRatio, ite_eq_right ht.ne', Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp [hs.ne', ht.ne']
  ring

/-- When the sample span fits in the held scale, the complete argmax is
the uniform location interval plus the constant-sample shape ridge. -/
theorem held_scale_uniform_argmax {n : ℕ} (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (hspan : x j - x i ≤ s) :
    let D := {p : ℝ × ℝ | p.2 ∈ shapeDomain (fun k ↦ x k - p.1) s}
    let L := fun p : ℝ × ℝ ↦ logLik (fun k ↦ x k - p.1) s p.2
    ∀ p, (p ∈ D ∧ IsMaxOn L D p) ↔
      (p.2 = -1 ∧ p.1 ∈ Icc (x j - s) (x i)) ∨
        (-1 ≤ p.2 ∧ p.1 = x i ∧ x i = x j) := by
  intro D L p
  have hw : ∃ m, ∀ k, x k - m ∈ Icc 0 s :=
    ⟨x i, fun k ↦ ⟨sub_nonneg.mpr (hi k), (sub_le_sub_right (hj k) _).trans hspan⟩⟩
  have he : (p ∈ D ∧ IsMaxOn L D p) ↔
      (p.2 = -1 ∧ ∀ k, x k - p.1 ∈ Icc 0 s) ∨
      (-1 ≤ p.2 ∧ ∀ k, x k = p.1) := by
    rw [← held_scale_uniform_maximizers x hs hw]
    exact and_congr_right fun _ ↦ ⟨fun hm m ξ hd ↦ hm (a := (m, ξ)) hd,
      fun hm q hq ↦ hm q.1 q.2 hq⟩
  rw [he]
  constructor
  · rintro (⟨hu, h⟩ | ⟨hξ, h⟩)
    · exact Or.inl ⟨hu, by
        have hmax := (h j).2
        exact ⟨by linarith, sub_nonneg.mp (h i).1⟩⟩
    · exact Or.inr ⟨hξ, (h i).symm, (h i).trans (h j).symm⟩
  · rintro (⟨hu, hm⟩ | ⟨hξ, hm, hc⟩)
    · refine Or.inl ⟨hu, fun k ↦ ⟨sub_nonneg.mpr (hm.2.trans (hi k)), ?_⟩⟩
      have h := hj k
      have hlo := hm.1
      linarith
    · exact Or.inr ⟨hξ, fun k ↦ ((hi k).antisymm (hc ▸ hj k)).symm.trans hm.symm⟩

end UnivariateML.Base7GPD

end
