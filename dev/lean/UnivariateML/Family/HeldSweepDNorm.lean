module

public import UnivariateML.Family.Discrete3DNorm
public import UnivariateML.Family.SkewLSGaussian

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Grouped normal held fibers

The held fibers are written in inverse scale and scaled location, using the
working chart of the joint grouped-normal formalization. The zero-scale
path is written directly as a difference of normalized Gaussian CDF values.

## Deviations

Attainment and the contact value use the normalized CDF likelihood.
Uniqueness uses the existing working log likelihood. An explicit Lean
identity linking those two representations is still needed.
-/

@[expose] public section

open Set Real Filter MeasureTheory
open scoped Topology

namespace UnivariateML.HeldSweepDNorm

open Discrete3DNorm

/-- Any attained maximum in the held-location working fiber is unique. -/
theorem held_location_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) (m : ℝ)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hma : IsMaxOn (fun B ↦ workingLogLik x (m * B, B)) (Ioi 0) a)
    (hmb : IsMaxOn (fun B ↦ workingLogLik x (m * B, B)) (Ioi 0) b) :
    a = b := by
  have hs : StrictConcaveOn ℝ (Ioi 0) (fun B ↦ workingLogLik x (m * B, B)) := by
    refine ⟨convex_Ioi _, fun s hs t ht hst u v hu hv huv ↦ ?_⟩
    have he : (m * (u * s + v * t), u * s + v * t) =
        u • (m * s, s) + v • (m * t, t) := by
      ext <;> simp only [Prod.smul_fst, Prod.smul_snd, Prod.fst_add, Prod.snd_add,
        smul_eq_mul]
      ring
    simp only [smul_eq_mul, he]
    exact (workingLogLik_strictConcave hn x).2 ⟨mem_univ _, hs⟩ ⟨mem_univ _, ht⟩
      (fun h ↦ hst (congrArg Prod.snd h)) hu hv huv
  exact hs.eq_of_isMaxOn hma hmb ha hb

/-- Any attained maximum at fixed positive inverse scale is unique. -/
theorem held_scale_atMostOne {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ)
    {B : ℝ} (hB : 0 < B) {a b : ℝ}
    (hma : IsMaxOn (fun A ↦ workingLogLik x (A, B)) univ a)
    (hmb : IsMaxOn (fun A ↦ workingLogLik x (A, B)) univ b) :
    a = b := by
  have hs : StrictConcaveOn ℝ univ (fun A ↦ workingLogLik x (A, B)) := by
    refine ⟨convex_univ, fun s _ t _ hst u v hu hv huv ↦ ?_⟩
    have he : (u * s + v * t, B) = u • (s, B) + v • (t, B) := by
      ext <;> simp only [Prod.smul_fst, Prod.smul_snd, Prod.fst_add, Prod.snd_add, smul_eq_mul]
      linear_combination -B * huv
    simp only [smul_eq_mul, he]
    exact (workingLogLik_strictConcave hn x).2 (x := (s, B)) (y := (t, B))
      ⟨mem_univ _, hB⟩ ⟨mem_univ _, hB⟩
      (fun h ↦ hst (congrArg Prod.fst h)) hu hv huv
  exact hs.eq_of_isMaxOn hma hmb (mem_univ _) (mem_univ _)

/-- On the zero-scale contact branch, every observed cell mass grows
strictly as inverse scale increases. -/
theorem held_location_contact {n : ℕ} (x : Fin n → ℤ) (m : ℝ)
    (hx : ∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    ∀ i, StrictMonoOn (fun B ↦ mass (x i : ℝ) B m) (Ioi 0) :=
  fun i ↦ cell_mass_strictMono_of_mem (hx i)

open SkewLSGaussian

/-- The normalized grouped-normal likelihood in public location and inverse scale. -/
noncomputable def heldLocationLikelihood {n : ℕ} (x : Fin n → ℤ) (m B : ℝ) : ℝ :=
  ∏ i, (Φ (((x i : ℝ) + 1 - m) * B) - Φ (((x i : ℝ) - m) * B))

private theorem contact_cell_limit {k m : ℝ} (hm : m ∈ Icc k (k + 1)) :
    Tendsto (fun B ↦ Φ ((k + 1 - m) * B) - Φ ((k - m) * B)) atTop
      (𝓝 (if m = k ∨ m = k + 1 then (1 : ℝ) / 2 else 1)) := by
  have hpos (a : ℝ) (ha : 0 < a) : Tendsto (fun B ↦ Φ (a * B)) atTop (𝓝 1) :=
    cdf_atTop.comp (tendsto_id.const_mul_atTop ha)
  have hneg (a : ℝ) (ha : a < 0) : Tendsto (fun B ↦ Φ (a * B)) atTop (𝓝 0) :=
    cdf_atBot.comp (tendsto_id.const_mul_atTop_of_neg ha)
  by_cases h₁ : m = k
  · subst m
    have h : k + 1 - k = 1 := by ring
    simpa only [h, sub_self, zero_mul, cdf_zero, true_or, ite_true,
      show (1 : ℝ) - 1 / 2 = 1 / 2 by norm_num] using
      (hpos (k + 1 - k) (by linarith)).sub
        (tendsto_const_nhds (x := Φ 0) (f := atTop))
  by_cases h₂ : m = k + 1
  · subst m
    simpa only [or_true, ite_true, sub_self, zero_mul, cdf_zero, sub_zero] using
      (tendsto_const_nhds (x := Φ 0) (f := atTop)).sub
        (hneg (k - (k + 1)) (by linarith))
  · simpa only [h₁, h₂, or_self, ite_false, sub_zero] using
      (hpos _ (sub_pos.mpr (lt_of_le_of_ne hm.2 h₂))).sub
        (hneg _ (sub_neg.mpr (lt_of_le_of_ne hm.1 (Ne.symm h₁))))

/-- When every closed cell contains the held location, interior cells
contribute one and endpoint cells one half to the strictly unattained
zero-scale supremum. -/
theorem held_location_contact_supremum {n : ℕ} (hn : 0 < n)
    (x : Fin n → ℤ) (m : ℝ) (hx : ∀ i, m ∈ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    let u := ∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1)
    Tendsto (heldLocationLikelihood x m) atTop (𝓝 u) ∧
      IsLUB (heldLocationLikelihood x m '' Ioi 0) u ∧
      ∀ B > 0, heldLocationLikelihood x m B < u := by
  dsimp only
  have hl : Tendsto (heldLocationLikelihood x m) atTop
      (𝓝 (∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1))) :=
    tendsto_finsetProd _ (fun i _ ↦ contact_cell_limit (hx i))
  have hlt (B : ℝ) (hB : 0 < B) :
      heldLocationLikelihood x m B <
      ∏ i, (if m = (x i : ℝ) ∨ m = (x i : ℝ) + 1 then (1 : ℝ) / 2 else 1) := by
    unfold heldLocationLikelihood
    apply Finset.prod_lt_prod_of_nonempty₀ _ _
      (show (Finset.univ : Finset (Fin n)).Nonempty from ⟨⟨0, hn⟩, Finset.mem_univ _⟩)
    · intro i _
      exact sub_pos.mpr (cdf_strictMono (mul_lt_mul_of_pos_right (by linarith) hB))
    · intro i _
      split_ifs with he
      · rcases he with he | he
        · rw [he, sub_self, zero_mul, cdf_zero]
          linarith [cdf_lt_one (((x i : ℝ) + 1 - (x i : ℝ)) * B)]
        · rw [he, sub_self, zero_mul, cdf_zero]
          linarith [cdf_pos (((x i : ℝ) - ((x i : ℝ) + 1)) * B)]
      · linarith [cdf_lt_one (((x i : ℝ) + 1 - m) * B), cdf_pos (((x i : ℝ) - m) * B)]
  refine ⟨hl, ⟨?_, ?_⟩, hlt⟩
  · rintro _ ⟨B, hB, rfl⟩
    exact (hlt B hB).le
  · intro c hc
    apply le_of_tendsto hl
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with B hB
    exact hc ⟨B, hB, rfl⟩

private theorem cdf_cell_bounds (k m B : ℝ) (hB : 0 < B) :
    let p := Φ ((k + 1 - m) * B) - Φ ((k - m) * B)
    0 < p ∧ p ≤ 1 := by
  dsimp only
  exact ⟨sub_pos.mpr (cdf_strictMono (mul_lt_mul_of_pos_right (by linarith) hB)),
    by linarith [cdf_lt_one ((k + 1 - m) * B), cdf_pos ((k - m) * B)]⟩

private theorem sample_pos {n : ℕ} (x : Fin n → ℤ) (m B : ℝ) (hB : 0 < B) :
    0 < heldLocationLikelihood x m B :=
  Finset.prod_pos (fun i _ ↦ (cdf_cell_bounds (x i : ℝ) m B hB).1)

private theorem sample_le_cell {n : ℕ} (x : Fin n → ℤ) (i : Fin n) (m B : ℝ)
    (hB : 0 < B) :
    heldLocationLikelihood x m B ≤ Φ (((x i : ℝ) + 1 - m) * B) - Φ (((x i : ℝ) - m) * B) := by
  simpa only [heldLocationLikelihood, Finset.prod_singleton] using
    Finset.prod_le_prod_of_subset_of_le_one₀
      (f := fun j ↦ Φ (((x j : ℝ) + 1 - m) * B) - Φ (((x j : ℝ) - m) * B))
      (Finset.subset_univ {i}) (fun j _ ↦ (cdf_cell_bounds (x j : ℝ) m B hB).1.le)
      (fun j _ _ ↦ (cdf_cell_bounds (x j : ℝ) m B hB).2)

private theorem attained_interval {D : Set ℝ} {f : ℝ → ℝ} (hc : Continuous f)
    {a b p : ℝ} (hab : Icc a b ⊆ D) (hp : p ∈ Icc a b)
    (hbound : ∀ t ∈ D, t ∉ Icc a b → f t ≤ f p) :
    ∃ t ∈ D, IsMaxOn f D t := by
  obtain ⟨t, ht, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨p, hp⟩ hc.continuousOn
  refine ⟨t, hab ht, fun u hu ↦ ?_⟩
  by_cases h : u ∈ Icc a b
  · exact hm h
  · exact (hbound u hu h).trans (hm hp)

/-- A held positive scale has an attained location fit for every nonempty sample. -/
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℤ) {B : ℝ} (hB : 0 < B) :
    ∃ m, IsMaxOn (fun m ↦ heldLocationLikelihood x m B) univ m := by
  let f := fun m ↦ heldLocationLikelihood x m B
  let k : ℝ := (x ⟨0, hn⟩ : ℤ)
  have hc : Continuous f := by
    unfold f heldLocationLikelihood
    apply continuous_finsetProd
    intro i _
    exact (cdf_continuous.comp (by fun_prop : Continuous (fun t : ℝ ↦ _))).sub
      (cdf_continuous.comp (by fun_prop : Continuous (fun t : ℝ ↦ _)))
  let p := f 0
  have hp : 0 < p := sample_pos x 0 B hB
  have ht (c : ℝ) : Tendsto (fun m ↦ (c - m) * B) atTop atBot := by
    simpa only [sub_eq_add_neg] using
      (tendsto_atBot_add_const_left atTop c tendsto_neg_atTop_atBot).atBot_mul_const hB
  have hb (c : ℝ) : Tendsto (fun m ↦ (c - m) * B) atBot atTop := by
    simpa only [sub_eq_add_neg] using
      (tendsto_atTop_add_const_left atBot c tendsto_neg_atBot_atTop).atTop_mul_const hB
  have ht' := (cdf_atBot.comp (ht (k + 1))).sub (cdf_atBot.comp (ht k))
  have hb' := (cdf_atTop.comp (hb (k + 1))).sub (cdf_atTop.comp (hb k))
  obtain ⟨u, hu⟩ := eventually_atTop.mp (ht'.eventually
    (Iio_mem_nhds (show (0 : ℝ) - 0 < p by simpa using hp)))
  obtain ⟨l, hl⟩ := eventually_atBot.mp (hb'.eventually
    (Iio_mem_nhds (show (1 : ℝ) - 1 < p by simpa using hp)))
  let a := min l 0
  let b := max u 0
  obtain ⟨m, _, hm⟩ := attained_interval hc (D := univ) (a := a) (b := b) (p := 0)
    (fun _ _ ↦ mem_univ _) ⟨min_le_right _ _, le_max_right _ _⟩ (by
      intro t _ ht
      have hcell := sample_le_cell x ⟨0, hn⟩ t B hB
      by_cases h : t < a
      · exact hcell.trans (hl t (h.le.trans (min_le_left _ _))).le
      · have h' : b < t := lt_of_not_ge (fun h' ↦ ht ⟨le_of_not_gt h, h'⟩)
        exact hcell.trans (hu t ((le_max_left _ _).trans h'.le)).le)
  exact ⟨m, hm⟩

/-- If some observed closed cell excludes the held location, the finite
inverse-scale fit is attained. -/
theorem held_location_away_attainment {n : ℕ} (x : Fin n → ℤ) (m : ℝ)
    (hx : ∃ i, m ∉ Icc (x i : ℝ) ((x i : ℝ) + 1)) :
    ∃ B ∈ Ioi (0 : ℝ), IsMaxOn (heldLocationLikelihood x m) (Ioi 0) B := by
  obtain ⟨i, hi⟩ := hx
  let k : ℝ := (x i : ℤ)
  let f := heldLocationLikelihood x m
  let p := f 1
  have hp : 0 < p := sample_pos x m 1 one_pos
  have hc : Continuous f := by
    unfold f heldLocationLikelihood
    apply continuous_finsetProd
    intro j _
    exact (cdf_continuous.comp (by fun_prop : Continuous (fun t : ℝ ↦ _))).sub
      (cdf_continuous.comp (by fun_prop : Continuous (fun t : ℝ ↦ _)))
  have hz : f 0 = 0 := by
    unfold f heldLocationLikelihood
    apply Finset.prod_eq_zero (Finset.mem_univ i)
    simp
  have hlow : ∃ a > 0, a ≤ 1 ∧ ∀ B > 0, B < a → f B ≤ p := by
    have h := hc.continuousAt (x := (0 : ℝ)).tendsto
    rw [hz] at h
    obtain ⟨ε, hε, hε'⟩ := Metric.eventually_nhds_iff.mp (h.eventually (Iio_mem_nhds hp))
    exact ⟨min 1 ε, lt_min one_pos hε, min_le_left _ _, fun B hB hB' ↦
      (hε' (y := B) (by
        rw [Real.dist_eq, sub_zero, abs_of_pos hB]
        exact hB'.trans_le (min_le_right _ _))).le⟩
  have ht : Tendsto (fun B ↦ Φ ((k + 1 - m) * B) - Φ ((k - m) * B)) atTop (𝓝 0) := by
    have hd : m < k ∨ k + 1 < m := by
      by_contra! h
      exact hi ⟨h.1, h.2⟩
    rcases hd with hd | hd
    · have h (c : ℝ) (hc : 0 < c) := cdf_atTop.comp (tendsto_id.const_mul_atTop hc)
      simpa only [sub_self, Function.comp_def, id_eq] using
        (h (k + 1 - m) (by linarith)).sub (h (k - m) (by linarith))
    · have h (c : ℝ) (hc : c < 0) := cdf_atBot.comp (tendsto_id.const_mul_atTop_of_neg hc)
      simpa only [sub_self, Function.comp_def, id_eq] using
        (h (k + 1 - m) (by linarith)).sub (h (k - m) (by linarith))
  obtain ⟨u, hu⟩ := eventually_atTop.mp (ht.eventually (Iio_mem_nhds hp))
  obtain ⟨a, ha, ha1, hlow⟩ := hlow
  let b := max 1 u
  have hb : 1 ≤ b := le_max_left _ _
  apply attained_interval hc (a := a) (b := b) (p := 1)
    (fun _ h ↦ ha.trans_le h.1) ⟨ha1, hb⟩
  intro B hB h
  by_cases h' : B < a
  · exact hlow B hB h'
  · have hhi : b < B := lt_of_not_ge (fun hhi ↦ h ⟨le_of_not_gt h', hhi⟩)
    exact (sample_le_cell x i m B hB).trans (hu B ((le_max_right _ _).trans hhi.le)).le

end UnivariateML.HeldSweepDNorm

end
