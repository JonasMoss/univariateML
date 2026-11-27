module

public import UnivariateML.Generic.Endpoint2BetaWidth
public import UnivariateML.Family.Transfers3

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Nonstandard beta with one endpoint held

The observations are distances from the held endpoint. `a` is the
free-side shape, `b` the held-side shape. Reflection covers either
endpoint orientation.

## Deviations

The real log-likelihood results use positive held-side factors. The
separate contact classification applies at the held end before
combining its density factor with factors at the free end.
-/

@[expose] public section

open Real Set Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2NonstandardBeta

noncomputable section

variable {n : ℕ}

/-- The nonstandard beta log likelihood measured from the held endpoint. -/
def widthLogLik (z : Fin n → ℝ) (a b W : ℝ) : ℝ :=
  NonstandardBeta.logLik z 0 W (b, a)

/-- Exactly the held-side conditions for every likelihood factor to be
positive and finite. -/
def heldCompatible (z : Fin n → ℝ) (b : ℝ) : Prop :=
  (∀ i, 0 ≤ z i) ∧ (b = 1 ∨ ∀ i, 0 < z i)

/-- Positive finite factors are possible exactly when the sample is strictly
inside at the held end or the held shape permits closed contact. -/
theorem held_contact_classification (z : Fin n → ℝ) (b : ℝ) :
    ¬ heldCompatible z b ↔ (∃ i, z i < 0) ∨ (b ≠ 1 ∧ ∃ i, z i = 0) := by
  unfold heldCompatible
  constructor
  · intro h
    by_cases hneg : ∃ i, z i < 0
    · exact Or.inl hneg
    · push Not at hneg
      refine Or.inr ⟨fun he ↦ h ⟨hneg, Or.inl he⟩, ?_⟩
      by_contra! hz
      exact h ⟨hneg, Or.inr (fun i ↦ lt_of_le_of_ne (hneg i) (Ne.symm (hz i)))⟩
  · rintro (⟨i, hi⟩ | ⟨hb, i, hi⟩) ⟨hz, h⟩
    · exact hi.not_ge (hz i)
    · rcases h with he | hp
      · exact hb he
      · exact (hp i).ne' hi

private def widthConstant (z : Fin n → ℝ) (a b Z : ℝ) : ℝ :=
  ∑ i, ((b - 1) * log (z i / Z) - Beta.logBeta (b, a) - log Z)

private theorem width_transfer {z : Fin n → ℝ} {a b Z W : ℝ} (hZ : 0 < Z)
    (hW : 0 < W) (hz : heldCompatible z b) :
    widthLogLik z a b W =
      Endpoint2BetaWidth.logLik (fun i ↦ z i / Z) a b (Z / W) + widthConstant z a b Z := by
  unfold widthLogLik NonstandardBeta.logLik Endpoint2BetaWidth.logLik widthConstant
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  have hfree : (W - z i) / W = 1 - (Z / W) * (z i / Z) := by
    field_simp [hW.ne', hZ.ne']
  have hheld : (b - 1) * log (z i / W) =
      (b - 1) * (log (z i / Z) + log (Z / W)) := by
    rcases hz.2 with rfl | hpos
    · simp
    · rw [log_div (hpos i).ne' hW.ne', log_div (hpos i).ne' hZ.ne',
        log_div hZ.ne' hW.ne']
      ring
  simp only [sub_zero, hfree, hheld, log_div hZ.ne' hW.ne']
  ring

private theorem width_chart {Z : ℝ} (hZ : 0 < Z) {W : ℝ} :
    W ∈ Ioi Z ↔ Z / W ∈ Ioo (0 : ℝ) 1 := by
  constructor
  · intro h
    have hw : 0 < W := hZ.trans h
    exact ⟨div_pos hZ hw, (div_lt_one hw).mpr h⟩
  · intro h
    have hW : 0 < W := by
      rcases (div_pos_iff.mp h.1) with hh | hh
      · exact hh.2
      · exact (hZ.not_gt hh.1).elim
    exact (div_lt_one hW).mp h.2

/-- With fixed positive shapes and a positive farthest distance, the
regular free endpoint has a unique maximizing width. This includes zeros
when the held-side shape is one. -/
theorem fixed_shapes_fit (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hZ : 0 < Z) (hz : heldCompatible z b) (hle : ∀ i, z i ≤ Z)
    (hcontact : ∃ i, z i = Z) (ha : 1 ≤ a) (hb : 0 < b) :
    let D := if a = 1 then Ici Z else Ioi Z
    (∃! W, W ∈ D ∧ IsMaxOn (widthLogLik z a b) D W) ∧
      (a = 1 → IsMaxOn (widthLogLik z a b) D Z) ∧
      (1 < a → ∀ W ∈ D, IsMaxOn (widthLogLik z a b) D W ↔
        Endpoint2BetaWidth.score (fun i ↦ z i / Z) a b (Z / W) = 0) := by
  rcases eq_or_lt_of_le ha with rfl | ha'
  · simp only [ite_true, true_implies, lt_irrefl, false_implies, and_true]
    have hZ' : Z ∈ Ici Z := le_rfl
    have heq (W : ℝ) (h : W ∈ Ici Z) : widthLogLik z 1 b W =
        Endpoint2BetaWidth.logLik (fun i ↦ z i / Z) 1 b (Z / W) + widthConstant z 1 b Z :=
      width_transfer hZ (hZ.trans_le h) hz
    have hunit (W : ℝ) (h : W ∈ Ici Z) : Z / W ∈ Ioc (0 : ℝ) 1 := by
      have hW := hZ.trans_le h
      exact ⟨div_pos hZ hW, (div_le_one hW).mpr h⟩
    obtain ⟨hm, hu⟩ := Endpoint2BetaWidth.contact_max hn (z := fun i ↦ z i / Z) hb
    have hbest : IsMaxOn (widthLogLik z 1 b) (Ici Z) Z := by
      intro W hW
      change widthLogLik z 1 b W ≤ widthLogLik z 1 b Z
      rw [heq W hW, heq Z hZ', div_self hZ.ne']
      exact add_le_add_left (hm (hunit W hW)) _
    refine ⟨⟨Z, ⟨hZ', hbest⟩, ?_⟩, hbest⟩
    rintro W ⟨hW, hmax⟩
    have hWpos := hZ.trans_le hW
    have hh : IsMaxOn (Endpoint2BetaWidth.logLik (fun i ↦ z i / Z) 1 b)
        (Ioc (0 : ℝ) 1) (Z / W) := by
      intro u hu'
      have hx : Z / u ∈ Ici Z := by
        apply (le_div_iff₀ hu'.1).mpr
        nlinarith [mul_le_mul_of_nonneg_left hu'.2 hZ.le]
      have hs : widthLogLik z 1 b (Z / u) ≤ widthLogLik z 1 b W := hmax hx
      rw [heq _ hx, heq _ hW, div_div_cancel₀ hZ.ne'] at hs
      exact le_of_add_le_add_right hs
    have he := hu _ (hunit W hW) hh
    exact by simpa using ((div_eq_iff hWpos.ne').mp he).symm
  · have hne : a ≠ 1 := ha'.ne'
    simp only [hne, false_implies, true_and]
    let y : Fin n → ℝ := fun i ↦ z i / Z
    have hy (i : Fin n) : y i ∈ Icc (0 : ℝ) 1 :=
      ⟨div_nonneg (hz.1 i) hZ.le, (div_le_one hZ).mpr (hle i)⟩
    have hy1 : ∃ i, y i = 1 := hcontact.imp (fun i hi ↦ by simp [y, hi, hZ.ne'])
    have hi := Endpoint2BetaWidth.interior_max hn hy hy1 ha' hb
    have hbij : BijOn (fun W : ℝ ↦ Z / W) (Ioi Z) (Ioo 0 1) := by
      refine ⟨fun W hW ↦ (width_chart hZ).mp hW, ?_, ?_⟩
      · intro W _ V _ h
        change Z / W = Z / V at h
        rw [div_eq_mul_inv, div_eq_mul_inv] at h
        exact inv_injective (mul_left_cancel₀ hZ.ne' h)
      · intro u hu
        have he : Z / (Z / u) = u := div_div_cancel₀ hZ.ne'
        exact ⟨Z / u, (width_chart hZ).mpr (he.symm ▸ hu), he⟩
    have htransfer (W : ℝ) (hW : W ∈ Ioi Z) :
        IsMaxOn (widthLogLik z a b) (Ioi Z) W ↔
          Endpoint2BetaWidth.score y a b (Z / W) = 0 :=
      (Transfer.isMaxOn_iff hbij (fun _ h ↦ width_transfer hZ (hZ.trans h) hz) hW).trans
        (hi.2 _ (hbij.mapsTo hW))
    obtain ⟨u, ⟨hu, hs⟩, huniq⟩ := hi.1
    obtain ⟨W, hW, he⟩ := hbij.surjOn hu
    change Z / W = u at he
    refine ⟨⟨W, ⟨hW, (htransfer W hW).mpr (he.symm ▸ hs)⟩, ?_⟩,
      fun _ V hV ↦ htransfer V hV⟩
    rintro V ⟨hV, hmax⟩
    exact hbij.injOn hV hW ((huniq _ ⟨hbij.mapsTo hV, (htransfer V hV).mp hmax⟩).trans he.symm)

/-- For subunit free-side shape and positive held-side factors, width
approaching the farthest observation gives unbounded likelihood. -/
theorem fixed_shapes_spike {z : Fin n → ℝ} {a b Z : ℝ} (hZ : 0 < Z)
    (hz : heldCompatible z b) (hle : ∀ i, z i ≤ Z) (hcontact : ∃ i, z i = Z)
    (ha : a < 1) :
    Tendsto (fun t ↦ widthLogLik z a b (Z / (1 - t))) (𝓝[>] 0) atTop := by
  have hy (i : Fin n) : z i / Z ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg (hz.1 i) hZ.le, (div_le_one hZ).mpr (hle i)⟩
  have hy1 : ∃ i, z i / Z = 1 := hcontact.imp (fun i hi ↦ by simp [hi, hZ.ne'])
  apply (tendsto_atTop_add_const_right _ (widthConstant z a b Z)
    (Endpoint2BetaWidth.spike hy hy1 ha b)).congr'
  filter_upwards [mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with t ht
  rw [width_transfer hZ (div_pos hZ (sub_pos.mpr ht)) hz, div_div_cancel₀ hZ.ne']

/-- At held-side shape one and with every observation at the held end,
shrinking width gives unbounded likelihood. -/
theorem zero_distances_collapse (hn : 0 < n) (a : ℝ) :
    Tendsto (widthLogLik (fun _ : Fin n ↦ 0) a 1) (𝓝[>] 0) atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  apply Tendsto.congr' _ ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg (by linarith :
    -(n : ℝ) < 0)).atTop_add (tendsto_const_nhds (x := -(n : ℝ) * Beta.logBeta (1, a))))
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with W hW
  have hp : 0 < W := hW
  simp [widthLogLik, NonstandardBeta.logLik, div_self hp.ne', nsmul_eq_mul]
  ring

/-- Widths with positive finite likelihood under the endpoint convention. -/
def finiteWidthDomain (z : Fin n → ℝ) (a b : ℝ) : Set ℝ :=
  {W | 0 < W ∧ heldCompatible z b ∧ (∀ i, z i ≤ W) ∧ (a = 1 ∨ ∀ i, z i < W)}

/-- The fixed-shape criterion covers contact at either end as well as
incompatible distances and the all-zero sample. -/
theorem fixed_shapes_criterion (hn : 0 < n) {z : Fin n → ℝ} {a b Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (hZeq : ∃ i, z i = Z) (hb : 0 < b) :
    (∃ W ∈ finiteWidthDomain z a b,
      IsMaxOn (widthLogLik z a b) (finiteWidthDomain z a b) W) ↔
      0 < Z ∧ 1 ≤ a ∧ heldCompatible z b := by
  have hdom (hZ : 0 < Z) (hc : heldCompatible z b) :
      finiteWidthDomain z a b = if a = 1 then Ici Z else Ioi Z := by
    obtain ⟨j, hj⟩ := hZeq
    ext W
    constructor
    · rintro ⟨_, _, hh, hstrict⟩
      by_cases ha : a = 1
      · simpa [ha, hj] using hh j
      · rcases hstrict with he | he
        · exact (ha he).elim
        · simpa [ha, hj] using he j
    · intro h
      by_cases ha : a = 1
      · have hW : Z ≤ W := by simpa [ha] using h
        exact ⟨hZ.trans_le hW, hc, fun i ↦ (hle i).trans hW, Or.inl ha⟩
      · have hW : Z < W := by simpa [ha] using h
        exact ⟨hZ.trans hW, hc, fun i ↦ (hle i).trans hW.le,
          Or.inr fun i ↦ (hle i).trans_lt hW⟩
  constructor
  · rintro ⟨W, hW, hm⟩
    have hc : heldCompatible z b := hW.2.1
    have hZ : 0 < Z := by
      by_contra! h
      have hzero (i : Fin n) : z i = 0 := le_antisymm ((hle i).trans h) (hc.1 i)
      have hb1 : b = 1 := by
        rcases hc.2 with he | he
        · exact he
        · exact ((he ⟨0, hn⟩).ne' (hzero _)).elim
      have hz : z = fun _ ↦ 0 := funext hzero
      have hlarge := (zero_distances_collapse hn a).eventually_gt_atTop (widthLogLik z a b W)
      have hpos : ∀ᶠ w : ℝ in 𝓝[>] 0, 0 < w := self_mem_nhdsWithin
      obtain ⟨w, hgt, hw⟩ := (hlarge.and hpos).exists
      have hD : w ∈ finiteWidthDomain z a b := ⟨hw, hc, fun i ↦ by rw [hzero]; exact hw.le,
        Or.inr fun i ↦ by rw [hzero]; exact hw⟩
      have hle' : widthLogLik z a b w ≤ widthLogLik z a b W := hm hD
      exact hgt.not_ge (by simpa only [hz, hb1] using hle')
    refine ⟨hZ, ?_, hc⟩
    by_contra! ha
    have hlarge := (fixed_shapes_spike hZ hc hle hZeq ha).eventually_gt_atTop (widthLogLik z a b W)
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t ∧ t < 1 :=
      (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
        (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
    obtain ⟨t, hgt, ht, ht'⟩ := (hlarge.and hnear).exists
    have htW : Z < Z / (1 - t) := by
      apply (lt_div_iff₀ (sub_pos.mpr ht')).mpr
      nlinarith [mul_pos hZ ht]
    have hD : Z / (1 - t) ∈ finiteWidthDomain z a b := by
      rw [hdom hZ hc, ite_eq_right ha.ne]
      exact htW
    exact hgt.not_ge (hm hD)
  · rintro ⟨hZ, ha, hc⟩
    obtain ⟨W, ⟨hW, hm⟩, _⟩ := (fixed_shapes_fit hn hZ hc hle hZeq ha hb).1
    rw [hdom hZ hc]
    exact ⟨W, hW, hm⟩

/-- Free shape at the moving endpoint precludes a positive finite fit,
whether the opposite shape is held or free. The domain excludes held
contacts whose fixed factor is zero or infinite. -/
theorem free_endpoint_shape_no_fit (hn : 0 < n) {z : Fin n → ℝ} {Z : ℝ}
    (hle : ∀ i, z i ≤ Z) (heq : ∃ i, z i = Z) (B : Set ℝ) :
    let D : Set (ℝ × ℝ × ℝ) :=
      {v | 0 < v.2.1 ∧ 0 < v.2.2 ∧ v.2.2 ∈ B ∧
        v.1 ∈ finiteWidthDomain z v.2.1 v.2.2}
    let L := fun v : ℝ × ℝ × ℝ ↦ widthLogLik z v.2.1 v.2.2 v.1
    ¬ ∃ v ∈ D, IsMaxOn L D v := by
  intro D L
  rintro ⟨⟨W,a,b⟩,hD,hm⟩
  have hc : heldCompatible z b := hD.2.2.2.2.1
  let q : ℝ := 1/2
  have hq : 0 < q := by norm_num [q]
  have hsub : q < 1 := by norm_num [q]
  have hwidth {w : ℝ} (hw : 0 < w) (hz : ∀ i, z i < w) : (w,q,b) ∈ D :=
    ⟨hq,hD.2.1,hD.2.2.1,hw,hc,fun i ↦ (hz i).le,Or.inr hz⟩
  by_cases hZ : 0 < Z
  · have hlarge := (fixed_shapes_spike hZ hc hle heq hsub).eventually_gt_atTop (L (W,a,b))
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t ∧ t < 1 :=
      (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
        (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
    obtain ⟨t,hgt,ht,ht'⟩ := (hlarge.and hnear).exists
    have hw : Z < Z / (1-t) :=
      (lt_div_iff₀ (sub_pos.mpr ht')).mpr (by nlinarith [mul_pos hZ ht])
    exact hgt.not_ge (hm (hwidth (hZ.trans hw) (fun i ↦ (hle i).trans_lt hw)))
  · have hz (i : Fin n) : z i = 0 := le_antisymm ((hle i).trans (le_of_not_gt hZ)) (hc.1 i)
    have hb : b = 1 := hc.2.resolve_right fun h ↦ (h ⟨0,hn⟩).ne' (hz _)
    have hlarge := (zero_distances_collapse hn q).eventually_gt_atTop (L (W,a,b))
    have hpos : ∀ᶠ w : ℝ in 𝓝[>] 0, 0 < w := self_mem_nhdsWithin
    obtain ⟨w,hgt,hw⟩ := (hlarge.and hpos).exists
    have hle' : widthLogLik z q b w ≤ L (W,a,b) :=
      hm (hwidth hw (fun i ↦ (hz i).symm ▸ hw))
    have he : z = fun _ ↦ 0 := funext hz
    exact hgt.not_ge (by simpa only [he,hb] using hle')

private theorem width_translation (x : Fin n → ℝ) (a b l W : ℝ) :
    NonstandardBeta.logLik x l (l + W) (b, a) =
      widthLogLik (fun i ↦ x i - l) a b W := by
  unfold NonstandardBeta.logLik widthLogLik
  unfold NonstandardBeta.logLik
  simp only [add_sub_cancel_left, sub_zero, show ∀ i, l + W - x i = W - (x i - l) by
    intro i
    ring]

private theorem reflection (x : Fin n → ℝ) (l r a b : ℝ) :
    NonstandardBeta.logLik (fun i ↦ -x i) (-r) (-l) (a, b) =
      NonstandardBeta.logLik x l r (b, a) := by
  have hB : Beta.logBeta (a, b) = Beta.logBeta (b, a) := by
    simp [Beta.logBeta, Beta.normalizer, mul_comm, add_comm]
  unfold NonstandardBeta.logLik
  apply Finset.sum_congr rfl
  intro i _
  simp only [hB, neg_sub_neg]
  ring

/-- Any subunit shape produces a free-endpoint spike. The other endpoint
can be taken strictly outside the sample, even for constant data. -/
theorem free_endpoints_unbounded (hn : 0 < n) (x : Fin n → ℝ) {a b : ℝ}
    (h : a < 1 ∨ b < 1) :
    let D : Set (ℝ × ℝ) := {p | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2}
    let L : ℝ × ℝ → ℝ := fun p ↦ NonstandardBeta.logLik x p.1 p.2 (b, a)
    ¬ BddAbove (L '' D) := by
  dsimp only
  have hright (y : Fin n → ℝ) (a b : ℝ) (ha : a < 1) (M : ℝ) :
      ∃ l r, l < r ∧ (∀ i, y i ∈ Ioo l r) ∧
        M < NonstandardBeta.logLik y l r (b, a) := by
    have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
    obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ y hne
    obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ y hne
    let l := y i - 1
    let z := fun k ↦ y k - l
    let Z := y j - l
    have hpos (k : Fin n) : 0 < z k := by
      have := hi k (Finset.mem_univ k)
      dsimp [z, l]
      linarith
    have hZ : 0 < Z := hpos j
    have hmax (k : Fin n) : z k ≤ Z :=
      sub_le_sub_right (hj k (Finset.mem_univ k)) _
    have hpath := (fixed_shapes_spike (b := b) hZ ⟨fun k ↦ (hpos k).le, Or.inr hpos⟩ hmax
      ⟨j, rfl⟩ ha).eventually_gt_atTop M
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t ∧ t < 1 :=
      (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
        (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
    obtain ⟨t, hM, ht, ht'⟩ := (hpath.and hnear).exists
    let W := Z / (1 - t)
    have hW : Z < W := by
      apply (lt_div_iff₀ (sub_pos.mpr ht')).mpr
      nlinarith [mul_pos hZ ht]
    refine ⟨l, l + W, by linarith, fun k ↦ ⟨?_, ?_⟩, ?_⟩
    · exact sub_pos.mp (hpos k)
    · have := (hmax k).trans_lt hW
      dsimp [z] at this
      linarith
    · rw [width_translation]
      exact hM
  rintro ⟨M, hM⟩
  rcases h with ha | hb
  · obtain ⟨l, r, hlr, hs, hlt⟩ := hright x a b ha M
    exact hlt.not_ge (hM ⟨(l,r), ⟨hlr, hs⟩, rfl⟩)
  · obtain ⟨l, r, hlr, hs, hlt⟩ := hright (fun k ↦ -x k) b a hb M
    have hdom : (-r, -l) ∈ {p : ℝ × ℝ | p.1 < p.2 ∧ ∀ i, x i ∈ Ioo p.1 p.2} :=
      ⟨neg_lt_neg hlr, fun k ↦ ⟨by linarith [(hs k).2], by linarith [(hs k).1]⟩⟩
    have he := reflection x (-r) (-l) a b
    simp only [neg_neg] at he
    exact hlt.not_ge (he.symm ▸ hM ⟨(-r,-l), hdom, rfl⟩)

/-- Both endpoints can shrink symmetrically around a constant sample
with any fixed positive beta shapes. -/
theorem free_endpoints_constant_collapse (hn : 0 < n) (a b v : ℝ) :
    Tendsto (fun w ↦ NonstandardBeta.logLik (fun _ : Fin n ↦ v)
      (v - w / 2) (v + w / 2) (b, a)) (𝓝[>] 0) atTop := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  let C := (n : ℝ) * ((b - 1) * log (1 / 2) + (a - 1) * log (1 / 2) - Beta.logBeta (b, a))
  apply Tendsto.congr' _ ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg
    (neg_neg_of_pos hnR)).atTop_add (tendsto_const_nhds (x := C)))
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with w hw
  have hw' : 0 < w := hw
  have hwidth : v + w / 2 - (v - w / 2) = w := by ring
  have hl : v - (v - w / 2) = w / 2 := by ring
  have hr : v + w / 2 - v = w / 2 := by ring
  have hratio : w / 2 / w = 1 / 2 := by field_simp [hw'.ne']
  simp [NonstandardBeta.logLik, hwidth, hl, hr, hratio, nsmul_eq_mul, C]
  ring_nf


/-- With both endpoints fixed, the beta shape-concentration limit
transfers exactly for every constant interior sample. -/
theorem held_endpoints_constant_unbounded (hn : 0 < n) {l r v : ℝ}
    (hl : l < v) (hr : v < r) :
    ¬ BddAbove (NonstandardBeta.logLik (fun _ : Fin n ↦ v) l r '' Beta.domain) :=
  NonstandardBeta.joint_unbounded hn (hl.trans hr) (fun _ ↦ ⟨hl, hr⟩)
    (by simp)

end

end UnivariateML.Endpoint2NonstandardBeta

end
