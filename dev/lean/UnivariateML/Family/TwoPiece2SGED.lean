module

public import UnivariateML.Family.TwoPieceSGED
public import UnivariateML.Family.TwoPieceGED
public import Mathlib.Analysis.Convex.SpecificFunctions.Pow
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Fixed-shape asymmetric power losses and the held-skew shape comparison

## Deviations

The held-skew shape comparison proves the range-uniform supremum on the
fitted domain. Convergence of the optimizing parameters along the end and
the generic uniqueness statements remain open.
-/

@[expose] public section

open Set Real Filter
open scoped Topology
namespace UnivariateML.TwoPiece2SGED
open TwoPieceSGED

noncomputable section

variable {n : ℕ}

private def distance (w z : ℝ) : ℝ := max (-z) 0 / w + max z 0 / (1 - w)

private theorem distance_left (w : ℝ) {z : ℝ} (h : z ≤ 0) :
    distance w z = -z / w := by
  simp [distance, max_eq_left (neg_nonneg.mpr h), max_eq_right h]

private theorem distance_right (w : ℝ) {z : ℝ} (h : 0 ≤ z) :
    distance w z = z / (1 - w) := by
  simp [distance, max_eq_right (neg_nonpos.mpr h), max_eq_left h]

/-- The power loss in mode coordinates at unit total width. -/
def loss (x : Fin n → ℝ) (p w c : ℝ) : ℝ :=
  ∑ i, TwoPieceSGED.powerPenalty p w (x i - c)

private theorem distance_pow (p w z : ℝ) :
    TwoPieceSGED.powerPenalty p w z = (distance w z)^p := by
  unfold TwoPieceSGED.powerPenalty
  split
  · rename_i h
    rw [abs_of_neg h, distance_left w h.le]
  · rename_i h
    rw [abs_of_nonneg (le_of_not_gt h), distance_right w (le_of_not_gt h)]

private theorem gap_loss {p w : ℝ} (hp : 0 < p) (hp' : p < 1)
    (hw : w ∈ Ioo (0 : ℝ) 1) (hn : 0 < n)
    {x : Fin n → ℝ} {l m r : ℝ} (hl : l < m) (hr : m < r)
    (hgap : ∀ i, x i ≤ l ∨ r ≤ x i) :
    min (loss x p w l) (loss x p w r) < loss x p w m := by
  let a := (r - m) / (r - l)
  let b := (m - l) / (r - l)
  have hd := sub_pos.mpr (hl.trans hr)
  have ha : 0 < a := div_pos (sub_pos.mpr hr) hd
  have hb : 0 < b := div_pos (sub_pos.mpr hl) hd
  have hab : a + b = 1 := by dsimp [a, b]; field_simp; ring
  have hm : a*l + b*r = m := by dsimp [a, b]; field_simp; ring
  have hterm (i : Fin n) :
      a * (distance w (x i-l))^p + b * (distance w (x i-r))^p <
        (distance w (x i-m))^p := by
    have blend {u v z q : ℝ} (hq : 0 < q) (hu : 0 ≤ u) (hv : 0 ≤ v) (hne : u ≠ v)
        (he : a*u + b*v = z) :
        a * (u/q)^p + b * (v/q)^p < (z/q)^p := by
      have hc := (strictConcaveOn_rpow hp hp').2
        (show u/q ∈ Ici 0 from div_nonneg hu hq.le)
        (show v/q ∈ Ici 0 from div_nonneg hv hq.le)
        (fun e ↦ hne ((div_left_inj₀ hq.ne').mp e)) ha hb hab
      have hblend : a*(u/q) + b*(v/q) = z/q := by
        rw [← mul_div_assoc, ← mul_div_assoc, ← add_div, he]
      simpa only [smul_eq_mul, hblend] using hc
    rcases hgap i with h | h
    · rw [distance_left w (sub_nonpos.mpr h),
        distance_left w (sub_nonpos.mpr (h.trans (hl.trans hr).le)),
        distance_left w (sub_nonpos.mpr (h.trans hl.le))]
      exact blend hw.1 (by linarith) (by linarith) (by intro he; linarith)
        (by linear_combination hm - x i * hab)
    · rw [distance_right w (sub_nonneg.mpr ((hl.trans hr).le.trans h)),
        distance_right w (sub_nonneg.mpr h),
        distance_right w (sub_nonneg.mpr (hr.le.trans h))]
      exact blend (sub_pos.mpr hw.2) (by linarith) (by linarith) (by intro he; linarith)
        (by linear_combination x i * hab - hm)
  have hsum : a * loss x p w l + b * loss x p w r < loss x p w m := by
    simp only [loss, distance_pow]
    rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩ fun i _ ↦ hterm i
  let L := min (loss x p w l) (loss x p w r)
  calc
    L = a * L + b * L := by rw [← add_mul, hab, one_mul]
    _ ≤ a * loss x p w l + b * loss x p w r :=
      add_le_add (mul_le_mul_of_nonneg_left (min_le_left _ _) ha.le)
        (mul_le_mul_of_nonneg_left (min_le_right _ _) hb.le)
    _ < loss x p w m := hsum

private theorem improving_knot {ν w : ℝ} (hν : 0 < ν) (hν' : ν < 1)
    (hw : w ∈ Ioo (0 : ℝ) 1) (hn : 0 < n)
    {x : Fin n → ℝ} {m : ℝ} (hm : ∀ i, x i ≠ m) :
    ∃ i, loss x ν w (x i) < loss x ν w m := by
  classical
  let k : Fin n := ⟨0, hn⟩
  have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨k, Finset.mem_univ _⟩
  by_cases hL : ∃ i, x i < m
  · by_cases hR : ∃ i, m < x i
    · obtain ⟨i, hi, him⟩ := Finset.exists_max_image (Finset.univ.filter fun i ↦ x i < m) x (by
        obtain ⟨i, hi⟩ := hL
        exact ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩)
      obtain ⟨j, hj, hjm⟩ := Finset.exists_min_image (Finset.univ.filter fun j ↦ m < x j) x (by
        obtain ⟨j, hj⟩ := hR
        exact ⟨j, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hj⟩⟩)
      have hgap (k : Fin n) : x k ≤ x i ∨ x j ≤ x k := by
        rcases lt_or_gt_of_ne (hm k) with hk | hk
        · exact Or.inl (him k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
        · exact Or.inr (hjm k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
      have hh := gap_loss hν hν' hw hn (Finset.mem_filter.mp hi).2 (Finset.mem_filter.mp hj).2 hgap
      rcases le_total (loss x ν w (x i)) (loss x ν w (x j)) with he | he
      · exact ⟨i, by rwa [min_eq_left he] at hh⟩
      · exact ⟨j, by rwa [min_eq_right he] at hh⟩
    · obtain ⟨i, _, hi⟩ := Finset.exists_max_image Finset.univ x hne
      have hik : x i < m := lt_of_le_of_ne (le_of_not_gt (fun h ↦ hR ⟨i,h⟩)) (hm i)
      refine ⟨i, ?_⟩
      simp only [loss, distance_pow]
      apply Finset.sum_lt_sum_of_nonempty hne
      intro j _
      have hj := hi j (Finset.mem_univ j)
      rw [distance_left w (sub_nonpos.mpr hj),
        distance_left w (sub_nonpos.mpr (hj.trans hik.le))]
      exact rpow_lt_rpow (div_nonneg (by linarith) hw.1.le)
        (div_lt_div_of_pos_right (by linarith) hw.1) hν
  · obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x hne
    have hik : m < x i := lt_of_le_of_ne (le_of_not_gt (fun h ↦ hL ⟨i,h⟩)) (Ne.symm (hm i))
    refine ⟨i, ?_⟩
    simp only [loss, distance_pow]
    apply Finset.sum_lt_sum_of_nonempty hne
    intro j _
    have hj := hi j (Finset.mem_univ j)
    rw [distance_right w (sub_nonneg.mpr hj),
      distance_right w (sub_nonneg.mpr (hik.le.trans hj))]
    have hp := sub_pos.mpr hw.2
    exact rpow_lt_rpow (div_nonneg (sub_nonneg.mpr hj) hp.le)
      (div_lt_div_of_pos_right (by linarith) hp) hν

/-- At a positive shape below one, the location minimizers are exactly
the knots whose loss is smallest among the knots. -/
theorem minimizingKnots {ν w : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hw : w ∈ Ioo (0 : ℝ) 1) (hn : 0 < n)
    (x : Fin n → ℝ) (m : ℝ) :
    IsMinOn (loss x ν w) univ m ↔
      ∃ i, m = x i ∧ ∀ j, loss x ν w (x i) ≤ loss x ν w (x j) := by
  constructor
  · intro h
    by_cases he : ∃ i, m = x i
    · obtain ⟨i, rfl⟩ := he
      exact ⟨i, rfl, fun j ↦ h (mem_univ (x j))⟩
    · obtain ⟨i, hi⟩ := improving_knot hν hν' hw hn (fun i h ↦ he ⟨i, h.symm⟩)
      exact (hi.not_ge (h (mem_univ (x i)))).elim
  · rintro ⟨i, rfl, hi⟩ m _
    by_cases he : ∃ j, m = x j
    · obtain ⟨j,rfl⟩ := he
      exact hi j
    · obtain ⟨j,hj⟩ := improving_knot hν hν' hw hn (fun j h ↦ he ⟨j,h.symm⟩)
      exact (hi j).trans hj.le

private def rightSlope (x : Fin n → ℝ) (w m : ℝ) : ℝ :=
  ∑ i, if x i ≤ m then 1 / w else -(1 / (1 - w))

private theorem distance_max {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (z : ℝ) :
    distance w z = max (-z / w) (z / (1 - w)) := by
  rcases le_total z 0 with h | h
  · rw [distance_left w h, max_eq_left]
    exact (div_nonpos_of_nonpos_of_nonneg h (sub_pos.mpr hw.2).le).trans
      (div_nonneg (neg_nonneg.mpr h) hw.1.le)
  · rw [distance_right w h, max_eq_right]
    exact (div_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr h) hw.1.le).trans
      (div_nonneg h (sub_pos.mpr hw.2).le)

private theorem rightSlope_bound {x : Fin n → ℝ} {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (m t : ℝ) :
    loss x 1 w m + (t - m) * rightSlope x w m ≤ loss x 1 w t := by
  simp only [loss, distance_pow, rpow_one, rightSlope, Finset.mul_sum,
    ← Finset.sum_add_distrib]
  apply Finset.sum_le_sum
  intro i _
  rw [distance_max hw (x i - t)]
  by_cases h : x i ≤ m
  · rw [ite_eq_left h, distance_left w (sub_nonpos.mpr h)]
    apply le_max_of_le_left
    ring_nf
    rfl
  · rw [ite_eq_right h, distance_right w (sub_nonneg.mpr (le_of_not_ge h))]
    apply le_max_of_le_right
    ring_nf
    rfl

private theorem rightSlope_realized {x : Fin n → ℝ} (hn : 0 < n)
    (w m : ℝ) :
    ∃ t > m, loss x 1 w t = loss x 1 w m + (t - m) * rightSlope x w m := by
  let d (i : Fin n) := if m < x i then x i - m else 1
  have hd (i : Fin n) : 0 < d i := by
    unfold d
    split
    · exact sub_pos.mpr ‹m < x i›
    · norm_num
  obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ d
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let t := m + d i
  have hmt : m < t := lt_add_of_pos_right _ (hd i)
  refine ⟨t, hmt, ?_⟩
  simp only [loss, distance_pow, rpow_one, rightSlope, Finset.mul_sum,
    ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro j _
  by_cases h : x j ≤ m
  · rw [ite_eq_left h, distance_left w (sub_nonpos.mpr h),
      distance_left w (sub_nonpos.mpr (h.trans hmt.le))]
    ring
  · have hj := hi j (Finset.mem_univ j)
    have ht : t ≤ x j := by
      rwa [show d j = x j - m from ite_eq_left (lt_of_not_ge h), le_sub_iff_add_le,
        add_comm] at hj
    rw [ite_eq_right h, distance_right w (sub_nonneg.mpr (le_of_not_ge h)),
      distance_right w (sub_nonneg.mpr ht)]
    ring

private theorem rightSlope_nonneg_iff {x : Fin n → ℝ} {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (m : ℝ) :
    0 ≤ rightSlope x w m ↔ (n : ℝ) * w ≤
      ((Finset.univ.filter fun i ↦ x i ≤ m).card : ℝ) := by
  classical
  let S : Finset (Fin n) := Finset.univ.filter fun i ↦ x i ≤ m
  let T : Finset (Fin n) := Finset.univ.filter fun i ↦ ¬ x i ≤ m
  have hcard : (S.card : ℝ) + T.card = n := by
    exact_mod_cast (Finset.card_filter_add_card_filter_not (s := Finset.univ)
      (p := fun i ↦ x i ≤ m)).trans (by simp)
  have hf : rightSlope x w m = (S.card : ℝ) / w - (T.card : ℝ) / (1 - w) := by
    simp [rightSlope, Finset.sum_ite, S, T, nsmul_eq_mul, div_eq_mul_inv, sub_eq_add_neg]
  rw [hf, sub_nonneg, div_le_div_iff₀ (sub_pos.mpr hw.2) hw.1]
  constructor <;> intro h <;>
    nlinarith only [h, congrArg (fun k : ℝ ↦ k*w) hcard]

private theorem loss_one_reflect (x : Fin n → ℝ) (w m : ℝ) :
    loss (fun i ↦ -x i) 1 (1 - w) (-m) = loss x 1 w m := by
  simp only [loss, distance_pow, rpow_one]
  apply Finset.sum_congr rfl
  intro i _
  simp only [distance, show 1 - (1 - w) = w by ring,
    show -x i - -m = -(x i - m) by ring, neg_neg, add_comm]

/-- At shape one the weighted median inequalities give the entire set of
location minimizers, including balanced gaps and repeated observations. -/
theorem minimizingWeightedMedians (hn : 0 < n) (x : Fin n → ℝ)
    {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (m : ℝ) :
    IsMinOn (loss x 1 w) univ m ↔
      ((Finset.univ.filter fun i ↦ x i < m).card : ℝ) ≤ (n : ℝ) * w ∧
      (n : ℝ) * w ≤ ((Finset.univ.filter fun i ↦ x i ≤ m).card : ℝ) := by
  classical
  let y : Fin n → ℝ := fun i ↦ -x i
  have hw' : 1 - w ∈ Ioo (0 : ℝ) 1 := ⟨sub_pos.mpr hw.2, sub_lt_self _ hw.1⟩
  have hright {z : Fin n → ℝ} {v m : ℝ} (hv : v ∈ Ioo (0 : ℝ) 1) :
      (∀ t > m, loss z 1 v m ≤ loss z 1 v t) ↔ 0 ≤ rightSlope z v m := by
    constructor
    · intro h
      obtain ⟨t, ht, he⟩ := rightSlope_realized hn v m (x := z)
      have hle := h t ht
      rw [he, le_add_iff_nonneg_right] at hle
      exact nonneg_of_mul_nonneg_right hle (sub_pos.mpr ht)
    · intro h t ht
      exact (le_add_of_nonneg_right (mul_nonneg (sub_nonneg.mpr ht.le) h)).trans
        (rightSlope_bound hv m t)
  have hrefl : (∀ t < m, loss x 1 w m ≤ loss x 1 w t) ↔
      (∀ t > -m, loss y 1 (1 - w) (-m) ≤ loss y 1 (1 - w) t) := by
    constructor
    · intro h t ht
      rw [loss_one_reflect x w m, ← neg_neg t, loss_one_reflect x w (-t)]
      exact h (-t) (by linarith)
    · intro h t ht
      simpa only [y, loss_one_reflect] using h (-t) (by linarith)
  have hleft : (∀ t < m, loss x 1 w m ≤ loss x 1 w t) ↔
      ((Finset.univ.filter fun i ↦ x i < m).card : ℝ) ≤ (n : ℝ) * w := by
    rw [hrefl, hright hw', rightSlope_nonneg_iff hw']
    have hc : (((Finset.univ.filter fun i ↦ x i < m).card : ℕ) : ℝ) +
        ((Finset.univ.filter fun i ↦ y i ≤ -m).card : ℝ) = n := by
      have hc' : (Finset.univ.filter fun i ↦ x i < m).card +
          (Finset.univ.filter fun i ↦ y i ≤ -m).card = n := by
        simpa only [y, neg_le_neg_iff, not_lt, Finset.card_univ, Fintype.card_fin] using
          (Finset.card_filter_add_card_filter_not (s := Finset.univ) (fun i ↦ x i < m))
      exact_mod_cast hc'
    constructor <;> intro h <;> nlinarith only [h, hc]
  constructor
  · intro h
    exact ⟨hleft.mp (fun t _ ↦ h (mem_univ t)),
      (rightSlope_nonneg_iff hw m).mp ((hright hw).mp (fun t _ ↦ h (mem_univ t)))⟩
  · rintro ⟨hl, hr⟩ t _
    rcases lt_trichotomy t m with ht | rfl | ht
    · exact hleft.mpr hl t ht
    · exact le_rfl
    · exact (hright hw).mpr ((rightSlope_nonneg_iff hw m).mpr hr) t ht

private theorem logLik_eq {x : Fin n → ℝ} {p w s : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) (c : ℝ) :
    fixedSkewLogLik x p w (c,s) = -(n : ℝ) * log s - loss x p w c / s^p := by
  have hterm (i : Fin n) :
      powerPenalty p w ((x i-c)/s) = powerPenalty p w (x i-c) / s^p := by
    have hsign : (x i-c)/s < 0 ↔ x i-c < 0 := by rw [div_lt_iff₀ hs, zero_mul]
    unfold powerPenalty
    simp only [hsign, abs_div, abs_of_pos hs]
    have h (z a : ℝ) (ha : 0 < a) : (|z|/s/a)^p = (|z|/a)^p / s^p := by
      rw [div_div, mul_comm s a, ← div_div, div_rpow (div_nonneg (abs_nonneg _) ha.le) hs.le]
    split
    · exact h _ _ hw.1
    · exact h _ _ (sub_pos.mpr hw.2)
  unfold fixedSkewLogLik LocationScale.logLik loss
  simp only [hterm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.sum_div]
  ring

private theorem fixedWidth_iff {x : Fin n → ℝ} {p w s : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) (c : ℝ) :
    IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c ↔
      IsMinOn (loss x p w) univ c := by
  simp only [isMaxOn_iff, isMinOn_iff, logLik_eq hw hs, sub_le_sub_iff_left,
    div_le_div_iff_of_pos_right (rpow_pos_of_pos hs _)]

/-- At held positive standard deviation, shape and skew, all location
maximizers are classified by the three shape regimes. -/
theorem heldWidth_maximizers (hn : 0 < n) (x : Fin n → ℝ) {p w s : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    let M := {c | IsMaxOn (fun d ↦ fixedSkewLogLik x p w (d,s)) univ c}
    (1 < p → ∃ c, M = {c}) ∧
    (p = 1 → M = {c |
      ((Finset.univ.filter fun i ↦ x i < c).card : ℝ) ≤ (n : ℝ) * w ∧
      (n : ℝ) * w ≤ ((Finset.univ.filter fun i ↦ x i ≤ c).card : ℝ)}) ∧
    (p < 1 → M = {c | ∃ i, c = x i ∧ ∀ j, loss x p w (x i) ≤ loss x p w (x j)}) := by
  dsimp only
  refine ⟨?_, ?_, ?_⟩
  · intro h
    obtain ⟨c, hc, hu⟩ := (heldWidth hn x hp hw hs).2.2 h
    exact ⟨c, Set.ext fun d ↦ ⟨fun hd ↦ hu d hd, fun hd ↦ (mem_singleton_iff.mp hd).symm ▸ hc⟩⟩
  · rintro rfl
    exact Set.ext fun c ↦ (fixedWidth_iff hw hs c).trans (minimizingWeightedMedians hn x hw c)
  · intro h
    exact Set.ext fun c ↦ (fixedWidth_iff hw hs c).trans (minimizingKnots hp h hw hn x c)

private theorem loss_pos {x : Fin n → ℝ} {p w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1)
    (hx : LocationScale.Nonconstant x) (c : ℝ) : 0 < loss x p w c := by
  have hx' : ∃ i, x i ≠ c := by
    by_contra! he
    obtain ⟨i,j,hij⟩ := hx
    exact hij ((he i).trans (he j).symm)
  obtain ⟨i, hi⟩ := hx'
  have hdist : 0 < distance w (x i - c) := by
    rcases lt_or_gt_of_ne (sub_ne_zero.mpr hi) with h | h
    · rw [distance_left w h.le]
      exact div_pos (neg_pos.mpr h) hw.1
    · rw [distance_right w h.le]
      exact div_pos h (sub_pos.mpr hw.2)
  unfold loss
  exact (show 0 < powerPenalty p w (x i - c) by
    rw [distance_pow]
    exact rpow_pos_of_pos hdist _).trans_le
      (Finset.single_le_sum (f := fun j ↦ powerPenalty p w (x j-c))
        (fun j _ ↦ by
          unfold powerPenalty
          split
          · exact rpow_nonneg (div_nonneg (abs_nonneg _) hw.1.le) _
          · exact rpow_nonneg (div_nonneg (abs_nonneg _) (sub_pos.mpr hw.2).le) _)
        (Finset.mem_univ i))

private theorem conditionalWidth {p T : ℝ} (hn : (0 : ℝ) < n) (hp : 0 < p) (hT : 0 < T) :
    let s := (p*T/n)^p⁻¹
    0 < s ∧ ∀ u > 0, u ≠ s →
      -(n : ℝ)*log u - T/u^p < -(n : ℝ)*log s - T/s^p := by
  intro s
  have hbase := div_pos (mul_pos hp hT) hn
  have hs : 0 < s := rpow_pos_of_pos hbase _
  have hs' : s^p = p*T/n := rpow_inv_rpow hbase.le hp.ne'
  refine ⟨hs, fun u hu hne ↦ ?_⟩
  have hrel (v : ℝ) (hv : 0 < v) : T/v^p = (n/p)*(s/v)^p := by
    rw [div_rpow hs.le hv.le, hs']
    field_simp [(rpow_pos_of_pos hv p).ne', hp.ne', hn.ne']
  have hsize : T/s^p = n/p := by rw [hrel s hs, div_self hs.ne', one_rpow, mul_one]
  have hratio : 0 < s/u := div_pos hs hu
  have hnot : (s/u)^p ≠ 1 := by
    intro he
    have hh := congrArg Real.log he
    rw [log_rpow hratio, log_one, mul_eq_zero] at hh
    have hlu := hh.resolve_left hp.ne'
    rw [log_div hs.ne' hu.ne', sub_eq_zero] at hlu
    exact hne (Real.strictMonoOn_log.injOn hu hs hlu.symm)
  have h := log_lt_sub_one_of_pos (rpow_pos_of_pos hratio p) hnot
  rw [log_rpow hratio, log_div hs.ne' hu.ne'] at h
  have hl := mul_lt_mul_of_pos_left h (div_pos hn hp)
  have hh : n/p*(p*(log s-log u)) = n*(log s-log u) := by field_simp
  rw [hh] at hl
  nlinarith only [hl, hrel u hu, hsize]

/-- At fixed shape and skew, constants have no finite fit. On a
nonconstant sample all maximizing modes share one profiled width. Their
set is a singleton, the weighted median set, or the least-loss knots. -/
theorem heldSkew_maximizers (hn : 0 < n) (x : Fin n → ℝ) {p w : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) :
    let M := {cs ∈ LocationScale.domain |
      IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain cs}
    (¬ LocationScale.Nonconstant x → M = ∅) ∧
    (LocationScale.Nonconstant x → ∃ s > 0,
      (∀ c, IsMinOn (loss x p w) univ c → s = (p * loss x p w c / n)^p⁻¹) ∧
      (1 < p → ∃ c, M = {c} ×ˢ {s}) ∧
      (p = 1 → M = {c |
        ((Finset.univ.filter fun i ↦ x i < c).card : ℝ) ≤ (n : ℝ) * w ∧
        (n : ℝ) * w ≤ ((Finset.univ.filter fun i ↦ x i ≤ c).card : ℝ)} ×ˢ {s}) ∧
      (p < 1 → M = {c | ∃ i, c = x i ∧ ∀ j, loss x p w (x i) ≤ loss x p w (x j)} ×ˢ {s})) := by
  intro M
  have hex := (heldSkew_criterion hn x hp hw).1
  refine ⟨?_, ?_⟩
  · intro hx
    exact Set.eq_empty_iff_forall_notMem.mpr (fun cs hcs ↦ hx (hex.mp ⟨cs, hcs⟩))
  · intro hx
    obtain ⟨⟨c₀, u₀⟩, hu₀, hm₀⟩ := hex.mpr hx
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    have hu₀' : 0 < u₀ := hu₀.2
    have hc₀ : IsMinOn (loss x p w) univ c₀ :=
      (fixedWidth_iff hw hu₀' c₀).mp
        (fun d _ ↦ hm₀ (show (d,u₀) ∈ LocationScale.domain from ⟨mem_univ _, hu₀'⟩))
    let s := (p * loss x p w c₀ / n)^p⁻¹
    obtain ⟨hs, hstrict⟩ := conditionalWidth hN hp (loss_pos hw hx c₀)
    have hwidth : ∀ c, IsMinOn (loss x p w) univ c → s = (p * loss x p w c / n)^p⁻¹ := by
      intro c hc
      have he := le_antisymm (hc₀ (mem_univ c)) (hc (mem_univ c₀))
      dsimp only [s]
      rw [he]
    have hgraph : M = {c | IsMinOn (loss x p w) univ c} ×ˢ {s} := by
      ext ⟨c,u⟩
      change ((c,u) ∈ LocationScale.domain ∧
        IsMaxOn (fixedSkewLogLik x p w) LocationScale.domain (c,u)) ↔
          IsMinOn (loss x p w) univ c ∧ u = s
      constructor
      · rintro ⟨hu, hm⟩
        have hu' : 0 < u := hu.2
        have hc : IsMinOn (loss x p w) univ c :=
          (fixedWidth_iff hw hu' c).mp
            (fun d _ ↦ hm (show (d,u) ∈ LocationScale.domain from ⟨mem_univ _, hu'⟩))
        refine ⟨hc, ?_⟩
        by_contra hne
        have he := le_antisymm (hc (mem_univ c₀)) (hc₀ (mem_univ c))
        have hh := hm (show (c,s) ∈ LocationScale.domain from ⟨mem_univ _, hs⟩)
        change fixedSkewLogLik x p w (c,s) ≤ fixedSkewLogLik x p w (c,u) at hh
        rw [logLik_eq hw hs, logLik_eq hw hu', he] at hh
        exact (hstrict u hu' hne).not_ge hh
      · rintro ⟨hc,rfl⟩
        refine ⟨⟨mem_univ _, hs⟩, ?_⟩
        intro q hq
        have hq' : 0 < q.2 := hq.2
        have hb := (fixedWidth_iff hw hq' c₀).mpr hc₀ (mem_univ q.1)
        have he := le_antisymm (hc (mem_univ c₀)) (hc₀ (mem_univ c))
        refine hb.trans ?_
        change fixedSkewLogLik x p w (c₀,q.2) ≤ fixedSkewLogLik x p w (c,s)
        rw [logLik_eq hw hq', logLik_eq hw hs, he]
        rcases eq_or_ne q.2 s with h | h
        · rw [h]
        · exact (hstrict q.2 hq' h).le
    refine ⟨s, hs, hwidth, ?_, ?_, ?_⟩
    · intro h
      obtain ⟨c, hc⟩ := (heldWidth_maximizers hn x hp hw hs).1 h
      refine ⟨c, ?_⟩
      rw [hgraph, ← hc]
      congr 1
      exact Set.ext fun d ↦ (fixedWidth_iff hw hs d).symm
    · intro h
      rw [hgraph]
      congr 1
      subst p
      exact Set.ext fun c ↦ minimizingWeightedMedians hn x hw c
    · intro h
      rw [hgraph]
      congr 1
      exact Set.ext fun c ↦ minimizingKnots hp h hw hn x c

private theorem penalty_lb {p w : ℝ} (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (r : ℝ) :
    |r|^p ≤ powerPenalty p w r := by
  unfold powerPenalty
  split
  · exact rpow_le_rpow (abs_nonneg _)
      ((le_div_iff₀ hw.1).2 (mul_le_of_le_one_right (abs_nonneg _) hw.2.le)) hp.le
  · exact rpow_le_rpow (abs_nonneg _)
      ((le_div_iff₀ (sub_pos.mpr hw.2)).2
        (mul_le_of_le_one_right (abs_nonneg _) (sub_le_self _ hw.1.le))) hp.le

private theorem log_sub_power_atBot (n : ℕ) {p A : ℝ} (hp : 0 < p) (hA : 0 < A)
    (C : ℝ) : Tendsto (fun u : ℝ ↦ n * log u - A * u^p + C) atTop atBot := by
  have hlog := ((isLittleO_log_rpow_atTop hp).const_mul_left (n : ℝ)).bound
    (show 0 < A/2 by positivity)
  have ht := tendsto_atBot_add_const_right atTop C
    ((tendsto_rpow_atTop hp).const_mul_atTop_of_neg (show -(A/2) < 0 by linarith))
  apply tendsto_atBot_mono' _ _ ht
  filter_upwards [eventually_gt_atTop (0 : ℝ), hlog] with u hu hl
  rw [Real.norm_eq_abs, Real.norm_eq_abs, abs_of_pos (rpow_pos_of_pos hu _)] at hl
  nlinarith only [(le_abs_self _).trans hl]

/-- With mean and positive shape held, a nonzero residual rules out every
vanishing-width path, including simultaneous collapse of a side fraction.
On all-zero residuals a fixed-skew path gives unbounded likelihood. -/
theorem heldMeanShape_collapse (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) {p : ℝ}
    (hp : 0 < p) :
    ((∃ i, x i ≠ μ) →
      ∀ {ι : Type*} {l : Filter ι} {u w : ι → ℝ},
        Tendsto u l atTop → (∀ᶠ t in l, w t ∈ Ioo (0 : ℝ) 1) →
        Tendsto (fun t ↦ heldMeanSkew x μ p (w t) (u t)) l atBot) ∧
    ((∀ i, x i = μ) →
      ∀ w ∈ Ioo (0 : ℝ) 1, Tendsto (heldMeanSkew x μ p w) atTop atTop) := by
  refine ⟨?_, fun hx w _ ↦ heldMeanSkew_collapse hn hx⟩
  rintro ⟨i, hi⟩ ι l u w hu hw
  let d := x i - μ
  have hd : 0 < |d| := abs_pos.mpr (sub_ne_zero.mpr hi)
  let A := (|d|/2)^p
  let C := -(n : ℝ) * log (Gamma (1+1/p))
  have hA : 0 < A := rpow_pos_of_pos (half_pos hd) _
  apply tendsto_atBot_mono' _ _ ((log_sub_power_atBot n hp hA C).comp hu)
  filter_upwards [hu.eventually (eventually_gt_atTop (0 : ℝ)),
    hu.eventually (eventually_ge_atTop (2*|halfMoment p|/|d|)), hw] with t ht hb hwt
  let e := halfMoment p * (1 - 2*w t)
  have he : |e| ≤ |halfMoment p| := by
    rw [abs_mul]
    exact mul_le_of_le_one_right (abs_nonneg _) (abs_le.mpr
      ⟨by linarith [hwt.2], by linarith [hwt.1]⟩)
  have ha : (|d|/2)*u t ≤ |d*u t+e| := by
    have h := abs_sub_abs_le_abs_sub (d * u t) (-e)
    rw [sub_neg_eq_add, abs_neg, abs_mul, abs_of_pos ht] at h
    nlinarith only [h, he, (div_le_iff₀ hd).mp hb]
  have hterm : A * (u t)^p ≤ powerPenalty p (w t) (d*u t+e) := by
    unfold A
    rw [← mul_rpow (half_pos hd).le ht.le]
    exact (rpow_le_rpow (mul_nonneg (half_pos hd).le ht.le) ha hp.le).trans
      (penalty_lb hp hwt _)
  have hsum : A * (u t)^p ≤ ∑ j, powerPenalty p (w t) ((x j-μ)*u t+e) :=
    hterm.trans (Finset.single_le_sum
      (f := fun j ↦ powerPenalty p (w t) ((x j-μ)*u t+e))
      (fun _ _ ↦ (rpow_nonneg (abs_nonneg _) _).trans (penalty_lb hp hwt _))
      (Finset.mem_univ i))
  change heldMeanSkew x μ p (w t) (u t) ≤ n * log (u t) - A * (u t)^p + C
  unfold heldMeanSkew C
  change (n : ℝ) * (log (u t) - log (Gamma (1+1/p))) -
    (∑ j, powerPenalty p (w t) ((x j-μ)*u t+e)) ≤ _
  linarith

/-- The fitted-shape SGED log likelihood with width fraction fixed. -/
def freeShapeLogLik (x : Fin n → ℝ) (w p c s : ℝ) : ℝ :=
  fixedSkewLogLik x p w (c,s) - n * log (Gamma (1+1/p))

private theorem as_ged {x : Fin n → ℝ} {p w c s : ℝ}
    (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    freeShapeLogLik x w p c s =
      TwoPieceGED.kernelLogLik p (fun i ↦ distance w (x i-c)) 0 s + n * log 2 := by
  have hd (i : Fin n) : 0 ≤ distance w (x i-c) := by
    unfold distance
    exact add_nonneg (div_nonneg (le_max_right _ _) hw.1.le)
      (div_nonneg (le_max_right _ _) (sub_pos.mpr hw.2).le)
  have hg (j : Fin n) :
      (|distance w (x j-c) - 0|/s)^p = (distance w (x j-c))^p / s^p := by
    rw [sub_zero, abs_of_nonneg (hd j), div_rpow (hd j) hs.le]
  rw [freeShapeLogLik, logLik_eq hw hs, TwoPieceGED.kernelLogLik]
  simp_rw [hg, ← Finset.sum_div, loss, distance_pow]
  rw [log_mul (mul_pos (by norm_num) hs).ne', log_mul (by norm_num : (2 : ℝ) ≠ 0) hs.ne']
  · ring
  exact (Gamma_pos_of_pos (by have := div_pos one_pos hp; linarith)).ne'

private theorem clamp_loss {x : Fin n → ℝ} {p w l r c : ℝ}
    (hn : 0 < n) (hp : 0 < p) (hw : w ∈ Ioo (0 : ℝ) 1)
    (hx : ∀ i, l ≤ x i ∧ x i ≤ r) :
    loss x p w (min r (max l c)) ≤ loss x p w c := by
  by_cases hl : c < l
  · rw [max_eq_left hl.le]
    let i : Fin n := ⟨0, hn⟩
    have hlr : l ≤ r := (hx i).1.trans (hx i).2
    rw [min_eq_right hlr]
    simp only [loss, distance_pow]
    apply Finset.sum_le_sum
    intro i _
    rw [distance_right w (sub_nonneg.mpr (hx i).1),
      distance_right w (sub_nonneg.mpr (hl.le.trans (hx i).1))]
    exact rpow_le_rpow (div_nonneg (sub_nonneg.mpr (hx i).1) (sub_pos.mpr hw.2).le)
      (div_le_div_of_nonneg_right (sub_le_sub_left hl.le _) (sub_pos.mpr hw.2).le) hp.le
  · rw [max_eq_right (le_of_not_gt hl)]
    by_cases hr : c ≤ r
    · rw [min_eq_right hr]
    · rw [min_eq_left (le_of_not_ge hr)]
      simp only [loss, distance_pow]
      apply Finset.sum_le_sum
      intro i _
      rw [distance_left w (sub_nonpos.mpr (hx i).2),
        distance_left w (sub_nonpos.mpr ((hx i).2.trans (le_of_not_ge hr)))]
      exact rpow_le_rpow (div_nonneg (neg_nonneg.mpr (sub_nonpos.mpr (hx i).2)) hw.1.le)
        (div_le_div_of_nonneg_right (by linarith [le_of_not_ge hr]) hw.1.le) hp.le

private theorem profile_continuousAt {x : Fin n → ℝ} {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hx : LocationScale.Nonconstant x) {z : ℝ × ℝ} (hp : 0 < z.1) :
    ContinuousAt (fun y : ℝ × ℝ ↦
      freeShapeLogLik x w y.1 y.2 ((y.1 * loss x y.1 w y.2 / n)^y.1⁻¹)) z := by
  let Q (y : ℝ × ℝ) := loss x y.1 w y.2
  let s (y : ℝ × ℝ) := (y.1 * Q y / n)^y.1⁻¹
  have hpow (i : Fin n) : ContinuousAt (fun y : ℝ × ℝ ↦ (distance w (x i-y.2))^y.1) z := by
    have hdist : ContinuousAt (fun y : ℝ × ℝ ↦ distance w (x i-y.2)) z := by
      unfold distance
      fun_prop
    exact hdist.rpow continuous_fst.continuousAt (Or.inr hp)
  have hQ : ContinuousAt Q z := by
    simp only [Q, loss, distance_pow]
    classical
    have hsum (s : Finset (Fin n)) :
        ContinuousAt (fun y : ℝ × ℝ ↦ ∑ i ∈ s, (distance w (x i-y.2))^y.1) z := by
      induction s using Finset.induction_on with
      | empty => simp only [Finset.sum_empty]; exact continuousAt_const
      | insert i s hi hs =>
        simp only [Finset.sum_insert hi]
        exact (hpow i).add hs
    exact hsum Finset.univ
  have hs : ContinuousAt s z :=
    ((continuous_fst.continuousAt.mul hQ).div_const _).rpow
      (continuous_fst.continuousAt.inv₀ hp.ne') (Or.inr (inv_pos.mpr hp))
  have hn : 0 < n := by
    obtain ⟨i, j, h⟩ := hx
    exact Nat.zero_lt_of_lt i.isLt
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hs' : 0 < s z := (conditionalWidth hN hp (loss_pos hw hx z.2)).1
  have hG : ContinuousAt Gamma (1+1/z.1) := (differentiableAt_Gamma (by
    intro k
    have hh := div_pos one_pos hp
    linarith [Nat.cast_nonneg (α := ℝ) k])).continuousAt
  have hGc : ContinuousAt (fun y : ℝ × ℝ ↦ Gamma (1+1/y.1)) z :=
    hG.comp (f := fun y : ℝ × ℝ ↦ 1+1/y.1)
      (continuousAt_const.add (continuousAt_const.div continuous_fst.continuousAt hp.ne'))
  have hGpos := Gamma_pos_of_pos (show 0 < 1+1/z.1 by have := div_pos one_pos hp; linarith)
  have hcont : ContinuousAt (fun y : ℝ × ℝ ↦ -(n : ℝ) * log (s y) -
      Q y / (s y)^y.1 - n * log (Gamma (1+1/y.1))) z := by
    have hsp := hs.rpow continuous_fst.continuousAt (Or.inl hs'.ne')
    exact (((hs.log hs'.ne').const_mul _).sub
      (hQ.div hsp (rpow_pos_of_pos hs' _).ne')).sub ((hGc.log hGpos.ne').const_mul _)
  apply hcont.congr_of_eventuallyEq
  filter_upwards [continuous_fst.continuousAt (lt_mem_nhds hp)] with y hy
  have hs'' : 0 < s y := (conditionalWidth hN hy (loss_pos hw hx y.2)).1
  exact congrArg (fun L ↦ L - n * log (Gamma (1+1/y.1)))
    (logLik_eq hw hs'' y.2)

/-- At fixed positive skew and fitted shape at least one, constants
collapse. For nonconstant data the compact profile attains the global
maximum exactly when it reaches the range-uniform log likelihood. -/
theorem heldSkew_freeShape_criterion (hn : 0 < n) (x : Fin n → ℝ) {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    let V := {L : ℝ | ∃ p c s : ℝ, 1 ≤ p ∧ 0 < s ∧ L = freeShapeLogLik x w p c s}
    (¬ LocationScale.Nonconstant x → ¬ BddAbove V) ∧
    (LocationScale.Nonconstant x →
      ∃ l r : Fin n, (∀ i, x l ≤ x i ∧ x i ≤ x r) ∧
        let F := -(n : ℝ) * log (x r - x l)
        ((∃ L, IsGreatest V L) ↔ ∃ L ∈ V, F ≤ L) ∧
        ((∀ L ∈ V, L < F) → IsLUB V F)) := by
  intro V
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨?_, ?_⟩
  · intro hx hb
    let i : Fin n := ⟨0, hn⟩
    have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
    obtain ⟨M, hM⟩ := hb
    have ht := heldSkew_collapse (w := w) hn one_pos he
    have hg : freeShapeLogLik x w 1 = fun c s ↦ fixedSkewLogLik x 1 w (c,s) := by
      funext c s
      norm_num [freeShapeLogLik, Gamma_two]
    obtain ⟨u, hu, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop M))).exists
    exact hgt.not_ge (hM (show fixedSkewLogLik x 1 w (x i,1/u) ∈ V from
      ⟨1, x i, 1/u, le_rfl, one_div_pos.mpr hu, by rw [hg]⟩))
  · intro hx
    let i : Fin n := ⟨0, hn⟩
    obtain ⟨l, _, hl⟩ := Finset.exists_min_image Finset.univ x ⟨i, Finset.mem_univ _⟩
    obtain ⟨r, _, hr⟩ := Finset.exists_max_image Finset.univ x ⟨i, Finset.mem_univ _⟩
    have he (j : Fin n) : x l ≤ x j ∧ x j ≤ x r :=
      ⟨hl j (Finset.mem_univ _), hr j (Finset.mem_univ _)⟩
    refine ⟨l,r,he, ?_⟩
    let R := x r - x l
    let F := -(n : ℝ) * log R
    have hR : 0 < R := by
      apply sub_pos.mpr
      by_contra! hn
      obtain ⟨j,k,hjk⟩ := hx
      exact hjk (le_antisymm
        ((he j).2.trans (hn.trans (he k).1)) ((he k).2.trans (hn.trans (he j).1)))
    let c₀ := x l + w*R
    have hlower (c : ℝ) : ∃ j, R ≤ |distance w (x j-c) - 0| := by
      rcases le_total c₀ c with hc | hc
      · have hlc : x l ≤ c := (le_add_of_nonneg_right (mul_nonneg hw.1.le hR.le)).trans hc
        refine ⟨l, ?_⟩
        rw [sub_zero, distance_left w (sub_nonpos.mpr hlc),
          abs_of_nonneg (div_nonneg (by linarith) hw.1.le)]
        exact (le_div_iff₀ hw.1).mpr (by dsimp [c₀] at hc; nlinarith)
      · have hcr : c ≤ x r := hc.trans (by dsimp [c₀, R]; nlinarith [he r, hw.2])
        refine ⟨r, ?_⟩
        rw [sub_zero, distance_right w (sub_nonneg.mpr hcr),
          abs_of_nonneg (div_nonneg (sub_nonneg.mpr hcr) (sub_pos.mpr hw.2).le)]
        exact (le_div_iff₀ (sub_pos.mpr hw.2)).mpr (by dsimp [c₀, R] at *; nlinarith)
    have hupper (j : Fin n) : |distance w (x j-c₀) - 0| ≤ R := by
      rw [sub_zero]
      rcases le_total (x j) c₀ with h | h
      · rw [distance_left w (sub_nonpos.mpr h),
          abs_of_nonneg (div_nonneg (by linarith) hw.1.le)]
        exact (div_le_iff₀ hw.1).mpr (by dsimp [c₀]; nlinarith [(he j).1])
      · rw [distance_right w (sub_nonneg.mpr h),
          abs_of_nonneg (div_nonneg (sub_nonneg.mpr h) (sub_pos.mpr hw.2).le)]
        exact (div_le_iff₀ (sub_pos.mpr hw.2)).mpr (by dsimp [c₀, R]; nlinarith [(he j).2])
    have hF {L : ℝ} (hL : L ∈ upperBounds V) : F ≤ L := by
      have ht := (TwoPieceGED.uniform_path_freeSd hR hupper).2
      have hval : -(n : ℝ)*log (2*R) + n*log 2 = F := by
        dsimp [F]
        rw [log_mul (by norm_num : (2 : ℝ) ≠ 0) hR.ne']
        ring
      rw [← hval]
      apply le_of_tendsto (ht.add_const (n * log 2))
      filter_upwards [eventually_ge_atTop (1 : ℝ)] with p hp
      have hp' := zero_lt_one.trans_le hp
      let b := R*p^(1/p)
      have hb : 0 < b := mul_pos hR (rpow_pos_of_pos hp' _)
      exact hL ⟨p,c₀,b,hp,hb,by rw [as_ged hp' hw hb, TwoPieceGED.kernelLogLik_eq hp' hb]⟩
    let K := Icc 1 (TwoPieceGED.uniformCutoff n) ×ˢ Icc (x l) (x r)
    let prof (y : ℝ × ℝ) :=
      freeShapeLogLik x w y.1 y.2 ((y.1*loss x y.1 w y.2/n)^y.1⁻¹)
    have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
    have hKne : K.Nonempty := ⟨(1,x l), ⟨le_rfl, le_max_left _ _⟩, le_rfl, (he l).2⟩
    have hcont : ContinuousOn prof K := by
      intro y hy
      exact (profile_continuousAt hw hx (zero_lt_one.trans_le hy.1.1)).continuousWithinAt
    obtain ⟨y,hy,hm⟩ := hK.exists_isMaxOn hKne hcont
    have hy' : 0 < y.1 := zero_lt_one.trans_le hy.1.1
    have hyv : prof y ∈ V := ⟨y.1,y.2,_,hy.1.1,(conditionalWidth hN hy'
      (loss_pos hw hx y.2)).1,rfl⟩
    have hv {L : ℝ} (hL : L ∈ V) : L ≤ prof y ∨ L < F := by
      obtain ⟨p,c,s,hp,hs,rfl⟩ := hL
      have hp' := zero_lt_one.trans_le hp
      by_cases hP : p ≤ TwoPieceGED.uniformCutoff n
      · let m := min (x r) (max (x l) c)
        have hm' : (p,m) ∈ K := ⟨⟨hp,hP⟩,
          le_min (he l).2 (le_max_left _ _), min_le_left _ _⟩
        have hloc : freeShapeLogLik x w p c s ≤ freeShapeLogLik x w p m s := by
          simp only [freeShapeLogLik, logLik_eq hw hs]
          exact sub_le_sub_right (sub_le_sub_left
            (div_le_div_of_nonneg_right (clamp_loss hn hp' hw he) (rpow_nonneg hs.le _)) _) _
        have hc := conditionalWidth hN hp' (loss_pos (p := p) hw hx m)
        have hsmax : freeShapeLogLik x w p m s ≤ prof (p,m) := by
          by_cases h : s = (p*loss x p w m/n)^p⁻¹
          · simp [prof, h]
          · simp only [freeShapeLogLik, prof, logLik_eq hw hs, logLik_eq hw hc.1]
            exact sub_le_sub_right (hc.2 s hs h).le _
        exact Or.inl (hloc.trans (hsmax.trans (hm hm')))
      · have hf := TwoPieceGED.uniform_tail_excluded hn hR hs (lt_of_not_ge hP) (hlower c)
        rw [as_ged hp' hw hs]
        exact Or.inr (by
          dsimp [F]
          rw [log_mul (by norm_num : (2 : ℝ) ≠ 0) hR.ne'] at hf
          nlinarith only [hf])
    refine ⟨⟨?_, ?_⟩, ?_⟩
    · rintro ⟨L, hL, hmax⟩
      exact ⟨L, hL, hF hmax⟩
    · rintro ⟨L, hL, hwin⟩
      have hpy := (hv hL).elim (hwin.trans ·) (fun h ↦ (h.not_ge hwin).elim)
      exact ⟨prof y, hyv, fun _ h ↦ (hv h).elim id (fun h ↦ h.le.trans hpy)⟩
    · intro h
      exact ⟨fun L hL ↦ (h L hL).le, fun _ hL ↦ hF hL⟩

end

end UnivariateML.TwoPiece2SGED

end
