module

public import UnivariateML.Family.Endpoint2FellerPareto
public import Mathlib.Analysis.SpecialFunctions.Pow.Deriv
public import Mathlib.Analysis.Convex.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-scale, fixed-shape Feller–Pareto threshold

The normalized score is a weighted average of the per-observation
scores. Increasing the threshold gap lowers each of these scores and
shifts normalized weights toward the lower-scoring observations.
-/

@[expose] public section

open Real Set Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2FellerParetoRegular

noncomputable section

variable {n : ℕ}

private def weight (t d : ℝ) : ℝ := (t + d)⁻¹

private def pointScore (a b c s y : ℝ) : ℝ :=
  a - 1 - (a + b) * ((y / s) ^ c / (1 + (y / s) ^ c))

/-- Threshold-gap derivative divided by the sum of inverse excesses. -/
def normalizedScore (d : Fin n → ℝ) (a b c s t : ℝ) : ℝ :=
  (∑ i, (t + d i)⁻¹ * (a - 1 -
    (a + b) * (((t + d i) / s) ^ c / (1 + ((t + d i) / s) ^ c)))) /
    (∑ i, (t + d i)⁻¹)

private theorem pointScore_antitone {a b c s : ℝ} (hab : 0 < a + b)
    (hc : 0 < c) (hs : 0 < s) : StrictAntiOn (pointScore a b c s) (Ici 0) := by
  intro y hy z hz hyz
  have h₁ := div_nonneg hy hs.le
  have h₂ := div_nonneg hz hs.le
  have hlt : (y / s) ^ c < (z / s) ^ c :=
    (strictMonoOn_rpow_Ici_of_exponent_pos hc) h₁ h₂ (div_lt_div_of_pos_right hyz hs)
  have hfrac : (y / s) ^ c / (1 + (y / s) ^ c) <
      (z / s) ^ c / (1 + (z / s) ^ c) := by
    apply (div_lt_div_iff₀ (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg h₁ _))
      (add_pos_of_pos_of_nonneg one_pos (rpow_nonneg h₂ _))).mpr
    linarith
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left hfrac hab) _

private theorem rearrangement (w r h : Fin n → ℝ) (hw : ∀ i, 0 ≤ w i)
    (horder : ∀ i j, (h i - h j) * (r i - r j) ≤ 0) :
    (∑ i, w i * r i * h i) * (∑ i, w i) ≤
      (∑ i, w i * h i) * (∑ i, w i * r i) := by
  let k (i j : Fin n) := w i * w j * h i * (r i - r j)
  have hterm (i j : Fin n) : k i j + k j i ≤ 0 := by
    have he : k i j + k j i = (w i * w j) * ((h i - h j) * (r i - r j)) := by
      dsimp [k]
      ring
    rw [he]
    exact mul_nonpos_of_nonneg_of_nonpos (mul_nonneg (hw i) (hw j)) (horder i j)
  have hh : (∑ i, ∑ j, (k i j + k j i)) ≤ 0 := by
    exact Finset.sum_nonpos fun i _ ↦ Finset.sum_nonpos fun j _ ↦ hterm i j
  have heq : (∑ i, ∑ j, k i j) =
      (∑ i, w i * r i * h i) * (∑ i, w i) -
        (∑ i, w i * h i) * (∑ i, w i * r i) := by
    calc
      _ = ∑ i, (w i * r i * h i * (∑ j, w j) - w i * h i * (∑ j, w j * r j)) := by
        apply Finset.sum_congr rfl
        intro i _
        rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_sub_distrib]
        apply Finset.sum_congr rfl
        intro j _
        dsimp [k]
        ring
      _ = _ := by rw [Finset.sum_sub_distrib, ← Finset.sum_mul, ← Finset.sum_mul]
  have hs : (∑ i, ∑ j, k j i) = ∑ i, ∑ j, k i j := Finset.sum_comm
  simp_rw [Finset.sum_add_distrib] at hh
  rw [hs, heq] at hh
  linarith

private theorem positive_weight_sum (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {t : ℝ} (ht : 0 < t) : 0 < ∑ i, weight t (d i) := by
  have hh (i : Fin n) : 0 < weight t (d i) := inv_pos.mpr (add_pos_of_pos_of_nonneg ht (hd i))
  apply Finset.sum_pos'
  · exact fun i _ ↦ (hh i).le
  · exact ⟨⟨0,hn⟩, Finset.mem_univ _, hh _⟩

/-- The normalized threshold score is strictly decreasing on the
interior, including on constant samples and samples with tied minima. -/
theorem score_strictAnti (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c s : ℝ} (hab : 0 < a + b) (hc : 0 < c) (hs : 0 < s) :
    StrictAntiOn (normalizedScore d a b c s) (Ioi 0) := by
  intro t ht u hu htu
  let w := fun i : Fin n ↦ weight t (d i)
  let v := fun i : Fin n ↦ weight u (d i)
  let r := fun i : Fin n ↦ (t + d i) / (u + d i)
  let h := fun i : Fin n ↦ pointScore a b c s (t + d i)
  let g := fun i : Fin n ↦ pointScore a b c s (u + d i)
  have ht' (i : Fin n) : 0 < t + d i := add_pos_of_pos_of_nonneg ht (hd i)
  have hu' (i : Fin n) : 0 < u + d i := add_pos_of_pos_of_nonneg hu (hd i)
  have hw (i : Fin n) : 0 < w i := inv_pos.mpr (ht' i)
  have hv (i : Fin n) : 0 < v i := inv_pos.mpr (hu' i)
  have hR (i : Fin n) : w i * r i = v i := by
    change (t + d i)⁻¹ * ((t + d i) / (u + d i)) = (u + d i)⁻¹
    field_simp [(ht' i).ne', (hu' i).ne']
  have hH {i j : Fin n} (hij : d i ≤ d j) : h j ≤ h i :=
    (pointScore_antitone hab hc hs).antitoneOn (ht' i).le (ht' j).le (by linarith)
  have hratio {i j : Fin n} (hij : d i ≤ d j) : r i ≤ r j := by
    apply (div_le_div_iff₀ (hu' i) (hu' j)).mpr
    have hh := mul_nonneg (sub_nonneg.mpr htu.le) (sub_nonneg.mpr hij)
    linarith
  have hpair (i j : Fin n) : (h i - h j) * (r i - r j) ≤ 0 := by
    rcases le_total (d i) (d j) with hh | hh
    · exact mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr (hH hh)) (sub_nonpos.mpr (hratio hh))
    · exact mul_nonpos_of_nonpos_of_nonneg (sub_nonpos.mpr (hH hh)) (sub_nonneg.mpr (hratio hh))
  have hcmp := rearrangement w r h (fun i ↦ (hw i).le) hpair
  simp only [hR] at hcmp
  have hs₁ : 0 < ∑ i, w i := positive_weight_sum hn hd ht
  have hs₂ : 0 < ∑ i, v i := positive_weight_sum hn hd hu
  apply lt_of_lt_of_le (b := (∑ i, v i * h i) / (∑ i, v i))
  · apply div_lt_div_of_pos_right _ hs₂
    let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
    apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
    intro i _
    exact mul_lt_mul_of_pos_left
      ((pointScore_antitone hab hc hs) (ht' i).le (hu' i).le (by linarith)) (hv i)
  · exact (div_le_div_iff₀ hs₂ hs₁).mpr hcmp

private theorem score_derivative {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c s K t : ℝ} (hc : 0 < c) (hs : 0 < s) (ht : 0 < t) :
    HasDerivAt (Endpoint2FellerPareto.logLik d a b c s K)
      (∑ i, weight t (d i) * pointScore a b c s (t + d i)) t := by
  have hterm (i : Fin n) :
      HasDerivAt (fun t ↦ K + (a - 1) * log ((t + d i) / s) -
        ((a + b) / c) * log (1 + ((t + d i) / s) ^ c))
        (weight t (d i) * pointScore a b c s (t + d i)) t := by
    have hy : 0 < t + d i := add_pos_of_pos_of_nonneg ht (hd i)
    have hr := div_pos hy hs
    have harg := ((hasDerivAt_id t).add_const (d i)).div_const s
    have hpow := harg.rpow_const (p := c) (Or.inl hr.ne')
    have htail := (hpow.const_add 1).log
      (show 1 + ((t + d i) / s) ^ c ≠ 0 from (add_pos one_pos (rpow_pos_of_pos hr _)).ne')
    convert (((harg.log hr.ne').const_mul (a - 1)).const_add K).sub
      (htail.const_mul ((a + b) / c)) using 1
    · rfl
    · simp only [pointScore, weight, Function.id_def, one_div]
      rw [rpow_sub hr, rpow_one]
      have hpos : 0 < 1 + ((t + d i) / s) ^ c := by positivity
      field_simp [hc.ne', hs.ne', hy.ne', hpos.ne']
  convert HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ hterm i) using 1
  ext u
  simp [Endpoint2FellerPareto.logLik, Finset.sum_apply]

private theorem pointScore_continuousAt {a b c s y : ℝ} (hc : 0 < c) (hs : 0 < s)
    (hy : 0 ≤ y) : ContinuousAt (pointScore a b c s) y := by
  have hp : ContinuousAt (fun v : ℝ ↦ (v / s) ^ c) y :=
    (continuousAt_rpow_const _ c (Or.inr hc.le)).comp (continuousAt_id.div_const _)
  have hn : 0 < 1 + (y / s) ^ c :=
    add_pos_of_pos_of_nonneg one_pos (rpow_nonneg (div_nonneg hy hs.le) _)
  exact continuousAt_const.sub
    (continuousAt_const.mul (hp.div (continuousAt_const.add hp) hn.ne'))

private theorem score_continuousOn (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ContinuousOn (normalizedScore d a b c s) (Ioi 0) := by
  intro t ht
  have hw (i : Fin n) : ContinuousAt (fun u ↦ weight u (d i)) t :=
    (continuousAt_id.add continuousAt_const).inv₀ (add_pos_of_pos_of_nonneg ht (hd i)).ne'
  have hsum : ContinuousAt (fun u ↦ ∑ i, weight u (d i)) t :=
    tendsto_finsetSum Finset.univ (fun i _ ↦ (hw i).tendsto)
  have hnum : ContinuousAt (fun u ↦ ∑ i,
      weight u (d i) * pointScore a b c s (u + d i)) t := by
    apply tendsto_finsetSum Finset.univ
    intro i _
    have H := pointScore_continuousAt (a := a) (b := b) (y := t + d i) hc hs
      (add_pos_of_pos_of_nonneg ht (hd i)).le
    have H' : ContinuousAt (fun u : ℝ ↦ u + d i) t := continuousAt_id.add continuousAt_const
    exact ((hw i).mul (H.comp' (f := fun u : ℝ ↦ u + d i) (x := t) H')).tendsto
  exact (hnum.div hsum (positive_weight_sum hn hd ht).ne').continuousWithinAt

private theorem eventually_score_positive {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s : ℝ} (ha : 1 < a) (hc : 0 < c) (hs : 0 < s) :
    ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < normalizedScore d a b c s t := by
  classical
  obtain ⟨j,hj⟩ := hz
  let R (i : Fin n) (t : ℝ) :=
    (if d i = 0 then 1 else t / (t + d i)) * pointScore a b c s (t + d i)
  have hR (i : Fin n) : ContinuousAt (R i) 0 := by
    have harg : ContinuousAt (fun t : ℝ ↦ t + d i) 0 := continuousAt_id.add continuousAt_const
    have hscore := (pointScore_continuousAt (a := a) (b := b) (y := 0 + d i) hc hs
      (by simpa using hd i)).comp' (f := fun u : ℝ ↦ u + d i) (x := 0) harg
    by_cases hi : d i = 0
    · simpa only [R, ite_eq_left hi, one_mul, Function.comp_def] using hscore
    · simp only [R, ite_eq_right hi]
      exact (continuousAt_id.div harg (by simpa using hi)).mul hscore
  have he (i : Fin n) : R i 0 = if d i = 0 then a - 1 else 0 := by
    by_cases hi : d i = 0 <;> simp [R, hi, pointScore, zero_rpow hc.ne']
  have hsum : 0 < ∑ i, R i 0 := by
    apply Finset.sum_pos'
    · intro i _
      rw [he]
      split <;> linarith
    · exact ⟨j, Finset.mem_univ _, by rw [he, ite_eq_left hj]; linarith⟩
  have hlim : Tendsto (fun t ↦ ∑ i, R i t) (𝓝[>] 0) (𝓝 (∑ i, R i 0)) :=
    tendsto_finsetSum Finset.univ fun i _ ↦ (hR i).continuousWithinAt
  filter_upwards [hlim.eventually (Ioi_mem_nhds hsum),
    self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht htp
  have htp' : 0 < t := htp
  have H : (∑ i, R i t) = t * (∑ i, weight t (d i) * pointScore a b c s (t + d i)) := by
    rw [Finset.mul_sum]
    apply Finset.sum_congr rfl
    intro i _
    by_cases hi : d i = 0
    · simp [R, hi, weight, ← mul_assoc, mul_inv_cancel₀ htp'.ne']
    · simp only [R, ite_eq_right hi, weight, div_eq_mul_inv, mul_assoc]
  have ht' : 0 < ∑ i, R i t := ht
  rw [H] at ht'
  exact div_pos (pos_of_mul_pos_right ht' htp'.le) (positive_weight_sum j.isLt.pos hd htp')

private theorem exists_score_negative (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {a b c s : ℝ} (ha : 1 < a) (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    ∃ t > 0, normalizedScore d a b c s t < 0 := by
  let z := a + b + 1
  have hz : 0 < z := by dsimp [z]; linarith
  let t := s * z ^ c⁻¹
  have ht : 0 < t := mul_pos hs (rpow_pos_of_pos hz _)
  refine ⟨t, ht, ?_⟩
  have hpow : (t / s) ^ c = z := by
    dsimp [t]
    rw [mul_comm s, mul_div_cancel_right₀ _ hs.ne', ← rpow_mul hz.le,
      inv_mul_cancel₀ hc.ne', rpow_one]
  have hpoint (i : Fin n) : pointScore a b c s (t + d i) < 0 := by
    have hbase : t / s ≤ (t + d i) / s :=
      div_le_div_of_nonneg_right (by linarith [hd i]) hs.le
    have hpower : z ≤ ((t + d i) / s) ^ c := by
      rw [← hpow]
      exact (strictMonoOn_rpow_Ici_of_exponent_pos hc).monotoneOn
        (div_nonneg ht.le hs.le) (div_nonneg (add_nonneg ht.le (hd i)) hs.le) hbase
    have hv : 0 < 1 + ((t + d i) / s) ^ c := by linarith [hz.trans_le hpower]
    have hb' : 0 < b + 1 := by linarith
    have hcross := mul_le_mul_of_nonneg_left hpower hb'.le
    have hzz : a - 1 < (b + 1) * z := by
      dsimp [z]
      nlinarith [mul_pos hb hz, show z = a + b + 1 from rfl]
    unfold pointScore
    rw [← mul_div_assoc]
    exact sub_neg.mpr ((lt_div_iff₀ hv).mpr (by linarith [hzz.trans_le hcross]))
  apply div_neg_of_neg_of_pos _ (positive_weight_sum hn hd ht)
  apply Finset.sum_neg'
  · intro i _
    exact (mul_neg_of_pos_of_neg (inv_pos.mpr (add_pos_of_pos_of_nonneg ht (hd i))) (hpoint i)).le
  · exact ⟨⟨0,hn⟩, Finset.mem_univ _, mul_neg_of_pos_of_neg
      (inv_pos.mpr (add_pos_of_pos_of_nonneg ht (hd _))) (hpoint _)⟩

/-- At lower power above one the normalized score has exactly one interior
root, and that root is the unique likelihood maximum. -/
theorem interior_threshold_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (ha : 1 < a)
    (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let L := Endpoint2FellerPareto.logLik d a b c s K
    (∃! t, t ∈ Ioi (0 : ℝ) ∧ normalizedScore d a b c s t = 0) ∧
      ∀ t ∈ Ioi 0, IsMaxOn L (Ioi 0) t ↔ normalizedScore d a b c s t = 0 := by
  intro L
  have hn : 0 < n := by obtain ⟨j,_⟩ := hz; exact j.isLt.pos
  let F := normalizedScore d a b c s
  have hs₀ := score_strictAnti hn hd (show 0 < a + b by linarith) hc hs
  obtain ⟨v,hv,hFv⟩ := exists_score_negative hn hd ha hb hc hs
  have hl : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t ∧ t < v :=
    (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
      (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds hv))
  obtain ⟨u,hFu,hu,huv⟩ := ((eventually_score_positive hd hz (b := b) ha hc hs).and hl).exists
  have hsub : Icc u v ⊆ Ioi (0 : ℝ) := fun _ h ↦ hu.trans_le h.1
  obtain ⟨r, hr, hzero⟩ := intermediate_value_Icc' huv.le
    ((score_continuousOn (a := a) (b := b) hn hd hc hs).mono hsub)
    (show (0 : ℝ) ∈ Icc (F v) (F u) from ⟨hFv.le, hFu.le⟩)
  change F r = 0 at hzero
  have hr₀ : r ∈ Ioi (0 : ℝ) := hsub hr
  have hunique (t : ℝ) (ht : t ∈ Ioi (0 : ℝ)) (hz : F t = 0) : t = r :=
    hs₀.injOn ht hr₀ (hz.trans hzero.symm)
  have hderiv (t : ℝ) (ht : 0 < t) :
      HasDerivAt L ((∑ i, weight t (d i)) * F t) t := by
    have h := score_derivative (a := a) (b := b) (K := K) hd hc hs ht
    convert h using 1
    unfold F normalizedScore
    change (∑ i, weight t (d i)) *
      ((∑ i, weight t (d i) * pointScore a b c s (t+d i)) / (∑ i, weight t (d i))) = _
    field_simp [(positive_weight_sum hn hd ht).ne']
  have hcont : ContinuousOn L (Ioi (0 : ℝ)) :=
    fun t ht ↦ (hderiv t ht).continuousAt.continuousWithinAt
  have hinc : StrictMonoOn L (Ioi 0 ∩ Iic r) := by
    apply strictMonoOn_of_deriv_pos ((convex_Ioi 0).inter (convex_Iic r))
      (hcont.mono inter_subset_left)
    intro t ht
    have ht₀ := (interior_subset ht).1
    have htr : t < r := by simpa [interior_Iic] using (interior_mono inter_subset_right ht :
      t ∈ interior (Iic r))
    rw [(hderiv t ht₀).deriv]
    exact mul_pos (positive_weight_sum hn hd ht₀) (by
      have hh := hs₀ ht₀ hr₀ htr
      change F r < F t at hh
      simpa only [hzero] using hh)
  have hdec : StrictAntiOn L (Ioi 0 ∩ Ici r) := by
    apply strictAntiOn_of_deriv_neg ((convex_Ioi 0).inter (convex_Ici r))
      (hcont.mono inter_subset_left)
    intro t ht
    have ht₀ := (interior_subset ht).1
    have htr : r < t := by simpa [interior_Ici] using (interior_mono inter_subset_right ht :
      t ∈ interior (Ici r))
    rw [(hderiv t ht₀).deriv]
    exact mul_neg_of_pos_of_neg (positive_weight_sum hn hd ht₀) (by
      have hh := hs₀ hr₀ ht₀ htr
      change F t < F r at hh
      simpa only [hzero] using hh)
  have hstrict (t : ℝ) (ht : t ∈ Ioi 0) (hne : t ≠ r) : L t < L r := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hinc ⟨ht,h.le⟩ ⟨hr₀,le_rfl⟩ h
    · exact hdec ⟨hr₀,le_rfl⟩ ⟨ht,h.le⟩ h
  refine ⟨⟨r, ⟨hr₀,hzero⟩, fun t ht ↦ hunique t ht.1 ht.2⟩, fun t ht ↦ ?_⟩
  constructor
  · intro hm
    by_contra! hz
    exact (hstrict t ht (fun h ↦ hz (h.symm ▸ hzero))).not_ge (hm hr₀)
  · intro h
    rw [hunique t ht h]
    intro v hv
    by_cases he : v = r
    · simp [he]
    · exact (hstrict v hv he).le

/-- Holding all regular coefficients gives a unique finite threshold
exactly at lower power at least one. The subunit branch is unbounded,
and the unit lower power has a maximum at closed contact. -/
theorem fixed_scale_shapes_fit {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {a b c s K : ℝ} (hb : 0 < b) (hc : 0 < c) (hs : 0 < s) :
    let D := if a = 1 then Ici (0 : ℝ) else Ioi 0
    let L := Endpoint2FellerPareto.logLik d a b c s K
    ((∃ t ∈ D, IsMaxOn L D t) ↔ 1 ≤ a) ∧
      (1 ≤ a → ∃! t, t ∈ D ∧ IsMaxOn L D t) ∧
      (a < 1 → ¬ BddAbove (L '' D)) ∧
      (a = 1 → IsMaxOn L D 0) := by
  let L := Endpoint2FellerPareto.logLik d a b c s K
  have hn : 0 < n := by obtain ⟨j,_⟩ := hz; exact j.isLt.pos
  rcases lt_trichotomy a 1 with ha | rfl | ha
  · have hu : ¬ BddAbove (L '' Ioi (0 : ℝ)) := by
      rintro ⟨B, hB⟩
      have hpath := Endpoint2FellerPareto.threshold_spike hd hz (K := K) (b := b) ha hc hs
      have hgt := hpath.eventually_gt_atTop B
      have hpos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
      obtain ⟨t, ht, hp⟩ := (hgt.and hpos).exists
      exact ht.not_ge (hB (mem_image_of_mem L hp))
    simp only [ha.ne, ite_false, ha.not_ge, iff_false, false_implies, ha, true_implies, and_true,
      true_and]
    exact ⟨fun ⟨t, _, hm⟩ ↦ hu hm.bddAbove, hu⟩
  · simp only [ite_true, le_rfl, true_implies, iff_true, lt_irrefl, false_implies, true_and]
    have hu := Endpoint2FellerPareto.threshold_contact_unique hn hd hb hc hs (K := K)
    refine ⟨hu.exists.elim (fun t ht ↦ ⟨t,ht⟩), hu, ?_⟩
    intro t ht
    change Endpoint2FellerPareto.logLik d 1 b c s K t ≤ Endpoint2FellerPareto.logLik d 1 b c s K 0
    apply Finset.sum_le_sum
    intro i _
    have h₀ := div_nonneg (hd i) hs.le
    have h₁ := div_nonneg (add_nonneg ht (hd i)) hs.le
    have hpow := (strictMonoOn_rpow_Ici_of_exponent_pos hc).monotoneOn h₀ h₁
      (div_le_div_of_nonneg_right (show d i ≤ t + d i by linarith [show 0 ≤ t from ht]) hs.le)
    have hlog := log_le_log (show 0 < 1 + (d i / s) ^ c by linarith [rpow_nonneg h₀ c])
      (show 1 + (d i / s) ^ c ≤ 1 + ((t+d i) / s) ^ c by linarith)
    simp only [sub_self, zero_mul, add_zero, zero_add]
    linarith [mul_le_mul_of_nonneg_left hlog (show 0 ≤ (1 + b) / c by positivity)]
  · have hne : a ≠ 1 := ha.ne'
    have hu := interior_threshold_fit hd hz ha hb hc hs (K := K)
    obtain ⟨r, hr, huniq⟩ := hu.1
    have hm : IsMaxOn L (Ioi 0) r := (hu.2 r hr.1).mpr hr.2
    simp only [hne, ite_false, ha.le, iff_true, true_implies, ha.not_gt, false_implies, and_true]
    exact ⟨⟨r,hr.1,hm⟩, ⟨r,⟨hr.1,hm⟩,fun t h ↦ huniq t ⟨h.1,(hu.2 t h.1).mp h.2⟩⟩⟩

end

end UnivariateML.Endpoint2FellerParetoRegular

end
