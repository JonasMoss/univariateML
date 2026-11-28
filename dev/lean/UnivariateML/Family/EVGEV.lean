module

public import UnivariateML.Family.Base9GEV
public import UnivariateML.Family.AllFreeGEV

public import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

set_option linter.style.header false

/-! # Pair bounds for the fitted GEV

The strictly supported pair is expressed in rate coordinates. The closed
reverse-exponential member is treated in physical location and scale.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.EVGEV

noncomputable section

private theorem convex_sinh : ConvexOn ℝ (Ici 0) sinh := by
  apply convexOn_of_deriv2_nonneg' (convex_Ici 0) differentiable_sinh.differentiableOn
    (by rw [Real.deriv_sinh]; exact differentiable_cosh.differentiableOn)
  intro v hv
  change 0 ≤ deriv (deriv sinh) v
  rw [Real.deriv_sinh, Real.deriv_cosh]
  exact sinh_nonneg_iff.mpr hv

private theorem sinhRatio_le {ξ u : ℝ} (hξ : |ξ| ≤ 1) (h₀ : ξ ≠ 0) (hu : 0 ≤ u) :
    sinh (ξ * u) / ξ ≤ sinh u := by
  have ha : 0 < |ξ| := abs_pos.mpr h₀
  have h := convex_sinh.2 (show u ∈ Ici (0 : ℝ) from hu)
    (show (0 : ℝ) ∈ Ici 0 from le_rfl) ha.le (sub_nonneg.mpr hξ)
    (show |ξ| + (1 - |ξ|) = 1 by ring)
  simp only [smul_eq_mul, mul_zero, add_zero, sinh_zero] at h
  have he : sinh (ξ * u) / ξ = sinh (|ξ| * u) / |ξ| := by
    rcases le_total 0 ξ with hp | hn
    · rw [abs_of_nonneg hp]
    · rw [abs_of_nonpos hn, neg_mul, sinh_neg, neg_div_neg_eq]
  rw [he]
  exact (div_le_iff₀ ha).2 (by linarith only [h])

/-- The standardized distance factor of a strictly supported pair. -/
def distanceFactor (ξ u : ℝ) : ℝ :=
  if ξ = 0 then u else sinh (ξ * u) / ξ

private theorem factor_lt_cosh {ξ u : ℝ} (hξ : |ξ| ≤ 1) (hu : 0 ≤ u) :
    distanceFactor ξ u < cosh u := by
  have hs : sinh u < cosh u := by
    rw [sinh_eq, cosh_eq]
    linarith [exp_pos (-u)]
  by_cases h : ξ = 0
  · rw [distanceFactor, ite_eq_left h]
    exact (self_le_sinh_iff.mpr hu).trans_lt hs
  · rw [distanceFactor, ite_eq_right h]
    exact (sinhRatio_le hξ h hu).trans_lt hs

/-- In the log-rate chart the two rates are `exp (v+u)` and `exp (v-u)`.
If their distance is `d`, the sum of the two log densities is strictly below
the sharp reverse-exponential bound, including at Gumbel shape zero. -/
theorem strict_pair_rate_bound {ξ u v d s : ℝ} (hξ : ξ ∈ Icc (-1 : ℝ) 1)
    (hu : 0 ≤ u) (hd : 0 < d) (hs : 0 < s)
    (hgap : d = 2 * s * exp (-ξ * v) * distanceFactor ξ u) :
    -2 * log s + 2 * (1 + ξ) * v - (exp (v + u) + exp (v - u)) <
      2 * (log 2 - 1 - log d) := by
  have hpos : 0 < distanceFactor ξ u := by
    by_contra! h
    rw [hgap] at hd
    exact (mul_nonpos_of_nonneg_of_nonpos (by positivity) h).not_gt hd
  have hlog := log_lt_log hpos (factor_lt_cosh (abs_le.mpr hξ) hu)
  have hcosh := cosh_pos u
  have hnorm := log_le_sub_one_of_pos (mul_pos (exp_pos v) hcosh)
  rw [log_mul (exp_pos _).ne' hcosh.ne', log_exp] at hnorm
  have hl : log d = log 2 + log s - ξ * v + log (distanceFactor ξ u) := by
    rw [hgap, log_mul (by positivity) hpos.ne', log_mul (by positivity) (exp_pos _).ne',
      log_mul two_ne_zero hs.ne', log_exp]
    ring
  have hsum : exp (v + u) + exp (v - u) = 2 * (exp v * cosh u) := by
    rw [cosh_eq, exp_add, sub_eq_add_neg, exp_add]
    ring
  rw [hl, hsum]
  linarith only [hlog, hnorm]

/-- At shape minus one the pair bound is attained exactly when the
upper observation is at contact and the scale is half the pair distance. -/
theorem reverse_pair_bound {d s gap : ℝ} (hd : 0 < d) (hs : 0 < s) (hg : 0 ≤ gap) :
    -2 * log s - (d + 2 * gap) / s ≤ 2 * (log 2 - 1 - log d) ∧
    (-2 * log s - (d + 2 * gap) / s = 2 * (log 2 - 1 - log d) ↔
      gap = 0 ∧ s = d / 2) := by
  let t := d / (2 * s)
  have ht : 0 < t := by dsimp [t]; positivity
  have hl : log t = log d - (log 2 + log s) := by
    dsimp [t]
    rw [log_div hd.ne' (by positivity), log_mul two_ne_zero hs.ne']
  have he : (d + 2 * gap) / s = 2 * t + 2 * (gap / s) := by dsimp [t]; ring
  have hgs := div_nonneg hg hs.le
  have hw := log_le_sub_one_of_pos ht
  rw [hl] at hw
  refine ⟨by rw [he]; linarith only [hw, hgs], ?_⟩
  constructor
  · intro h
    rw [he] at h
    have hzero : gap / s = 0 := by linarith only [h, hw, hgs]
    have hte : t = 1 := by
      by_contra hn
      have hlt := log_lt_sub_one_of_pos ht hn
      rw [hl] at hlt
      linarith only [h, hlt, hgs]
    exact ⟨(div_eq_zero_iff.mp hzero).resolve_right hs.ne', by
      have hh := (div_eq_iff (show 2 * s ≠ 0 by positivity)).mp hte
      linarith⟩
  · rintro ⟨rfl, rfl⟩
    rw [he]
    have ht' : t = 1 := by dsimp [t]; field_simp
    rw [ht', log_one] at hl
    rw [ht']
    simp only [zero_div, mul_zero, add_zero]
    linarith only [hl]

/-- The individual summand of the GEV log likelihood on its support. -/
def logCell (ξ x : ℝ) (p : ℝ × ℝ) : ℝ :=
  if ξ = 0 then -log p.2 - (x - p.1) / p.2 - exp (-((x - p.1) / p.2))
  else -log p.2 + (-1 / ξ - 1) * log (GEV.supportArg ξ x p) -
    GEV.supportArg ξ x p ^ (-1 / ξ)

private theorem physical_strict_pair {ξ x y : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hxy : x < y) (hs : 0 < p.2)
    (hx : 0 < GEV.supportArg ξ x p) (hy : 0 < GEV.supportArg ξ y p) :
    logCell ξ x p + logCell ξ y p < 2 * (log 2 - 1 - log (y - x)) := by
  by_cases hz : ξ = 0
  · subst ξ
    let v := (-((x - p.1) / p.2) - (y - p.1) / p.2) / 2
    let u := (y - x) / (2 * p.2)
    have hu : 0 ≤ u := by dsimp [u]; positivity
    have hgap : y - x = 2 * p.2 * exp (-(0 : ℝ) * v) * distanceFactor 0 u := by
      simp [distanceFactor, u]
      field_simp
    have h₁ : v + u = -((x - p.1) / p.2) := by dsimp [v, u]; ring
    have h₂ : v - u = -((y - p.1) / p.2) := by dsimp [v, u]; ring
    have hb := strict_pair_rate_bound hξ hu (sub_pos.mpr hxy) hs hgap
    rw [h₁, h₂] at hb
    simp only [logCell, ite_true]
    dsimp [v] at hb
    linarith only [hb]
  · let a := log (GEV.supportArg ξ x p) * (-1 / ξ)
    let b := log (GEV.supportArg ξ y p) * (-1 / ξ)
    let u := (a - b) / 2
    let v := (a + b) / 2
    have hu : 0 ≤ u := by
      have hxy' : 0 < (y - x) / p.2 := div_pos (sub_pos.mpr hxy) hs
      have hsupp : GEV.supportArg ξ y p - GEV.supportArg ξ x p =
          ξ * ((y - x) / p.2) := by unfold GEV.supportArg; ring
      rcases lt_or_gt_of_ne hz with hn | hp
      · have harg := mul_neg_of_neg_of_pos hn hxy'
        rw [← hsupp] at harg
        dsimp [u, a, b]
        exact div_nonneg (sub_nonneg.mpr (mul_le_mul_of_nonneg_right
          (log_le_log hy (by linarith)) (div_nonneg_of_nonpos (by norm_num) hn.le)))
          (by norm_num)
      · have harg := mul_pos hp hxy'
        rw [← hsupp] at harg
        dsimp [u, a, b]
        exact div_nonneg (sub_nonneg.mpr (mul_le_mul_of_nonpos_right
          (log_le_log hx (by linarith)) (div_nonpos_of_nonpos_of_nonneg (by norm_num) hp.le)))
          (by norm_num)
    have h₁ : v + u = a := by dsimp [v, u]; ring
    have h₂ : v - u = b := by dsimp [v, u]; ring
    have hax : exp (-(ξ * a)) = GEV.supportArg ξ x p := by
      rw [show -(ξ * a) = log (GEV.supportArg ξ x p) by dsimp [a]; field_simp,
        exp_log hx]
    have hby : exp (-(ξ * b)) = GEV.supportArg ξ y p := by
      rw [show -(ξ * b) = log (GEV.supportArg ξ y p) by dsimp [b]; field_simp,
        exp_log hy]
    have hgap : y - x = 2 * p.2 * exp (-ξ * v) * distanceFactor ξ u := by
      rw [distanceFactor, ite_eq_right hz, sinh_eq]
      have ha' : -ξ * v + -(ξ * u) = -(ξ * a) := by dsimp [v, u]; ring
      have hb' : -ξ * v + ξ * u = -(ξ * b) := by dsimp [v, u]; ring
      have he : 2 * p.2 * exp (-ξ * v) *
          ((exp (ξ * u) - exp (-(ξ * u))) / 2 / ξ) =
          p.2 * (exp (-ξ * v + ξ * u) - exp (-ξ * v + -(ξ * u))) / ξ := by
        rw [exp_add, exp_add]; ring
      rw [he, ha', hb', hax, hby]
      unfold GEV.supportArg
      field_simp
      ring
    have hbnd := strict_pair_rate_bound hξ hu (sub_pos.mpr hxy) hs hgap
    rw [h₁, h₂] at hbnd
    simp only [logCell, ite_eq_right hz, rpow_def_of_pos hx, rpow_def_of_pos hy]
    change -log p.2 + (-1 / ξ - 1) * log (GEV.supportArg ξ x p) - exp a +
      (-log p.2 + (-1 / ξ - 1) * log (GEV.supportArg ξ y p) - exp b) < _
    have hcoef : (-1 / ξ - 1) * log (GEV.supportArg ξ x p) +
        (-1 / ξ - 1) * log (GEV.supportArg ξ y p) = 2 * (1 + ξ) * v := by
      dsimp [v, a, b]; field_simp; ring
    linarith only [hbnd, hcoef]

private theorem physical_pair {ξ x y : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hxy : x < y) (hs : 0 < p.2)
    (hx : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p)
    (hy : if ξ = -1 then 0 ≤ GEV.supportArg ξ y p else 0 < GEV.supportArg ξ y p) :
    logCell ξ x p + logCell ξ y p ≤ 2 * (log 2 - 1 - log (y - x)) ∧
    (logCell ξ x p + logCell ξ y p = 2 * (log 2 - 1 - log (y - x)) ↔
      ξ = -1 ∧ p.1 + p.2 = y ∧ p.2 = (y - x) / 2) := by
  by_cases h : ξ = -1
  · subst ξ
    have hg : 0 ≤ p.1 + p.2 - y := by
      simp only [ite_true, GEV.supportArg, neg_mul, one_mul] at hy
      have := (div_le_iff₀ hs).mp (show (y - p.1) / p.2 ≤ 1 by linarith)
      linarith
    have hf : logCell (-1) x p + logCell (-1) y p =
        -2 * log p.2 - ((y - x) + 2 * (p.1 + p.2 - y)) / p.2 := by
      norm_num [logCell, GEV.supportArg]
      field_simp
      ring
    rw [hf]
    have hb := reverse_pair_bound (sub_pos.mpr hxy) hs hg
    exact ⟨hb.1, by rw [hb.2]; simp [sub_eq_zero]⟩
  · rw [ite_eq_right h] at hx hy
    have hb := physical_strict_pair hξ hxy hs hx hy
    exact ⟨hb.le, by simp [hb.ne, h]⟩

/-- Exactly half of the sample at its minimum gives the sharp collapse
supremum on the full fitted shape interval. A finite parameter attains it
exactly at reverse-exponential contact with every upper gap equal to twice
the scale. -/
theorem all_free_half_supremum {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hc : n = 2 * Frechet.minimumCount (GEV.lowerGaps x m)) :
    let D : Set (ℝ × (ℝ × ℝ)) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain x q.1}
    let L := fun q : ℝ × (ℝ × ℝ) ↦ GEV.logLik x q.1 q.2
    let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m)
    IsLUB (L '' D) A ∧
    (∀ q ∈ D, L q = A ↔ q.1 = -1 ∧
      ∀ j, m < x j → q.2.1 + q.2.2 = x j ∧ q.2.2 = (x j - m) / 2) ∧
    ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ d > 0, ∀ j, m < x j → x j - m = d) := by
  classical
  intro D L A
  let S := Finset.univ.filter fun i ↦ x i = m
  let T := Finset.univ.filter fun i ↦ x i ≠ m
  have hS : S.card = Frechet.minimumCount (GEV.lowerGaps x m) := by
    simp [S, Frechet.minimumCount, GEV.lowerGaps, sub_eq_zero]
  have hST : S.card + T.card = n := by
    simpa [S, T] using (Finset.card_filter_add_card_filter_not
      (s := (Finset.univ : Finset (Fin n))) (fun i ↦ x i = m))
  have hSc : 0 < S.card := by omega
  have hT : T.card = S.card := by omega
  have hSc' : 0 < (S.card : ℝ) := by exact_mod_cast hSc
  have hSne := Finset.card_pos.mp hSc
  have hTne := Finset.card_pos.mp (hT ▸ hSc)
  have hzS (i : Fin n) (hi : i ∈ S) : x i = m := (Finset.mem_filter.mp hi).2
  have hpT (j : Fin n) (hj : j ∈ T) : m < x j :=
    lt_of_le_of_ne (hm j) (Ne.symm (Finset.mem_filter.mp hj).2)
  have hc' : (n : ℝ) = 2 * (S.card : ℝ) := by exact_mod_cast (show n = 2 * S.card by omega)
  have hsumT : ∑ j ∈ T, log (x j - m) = ∑ j, log (x j - m) := by
    have he : ∑ i ∈ S, log (x i - m) = 0 :=
      Finset.sum_eq_zero fun i hi ↦ by simp [hzS i hi]
    have h := Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ x i = m)
      (fun i ↦ log (x i - m))
    change (∑ i ∈ S, log (x i - m)) + (∑ i ∈ T, log (x i - m)) = _ at h
    rwa [he, zero_add] at h
  have hbound (q : ℝ × (ℝ × ℝ)) (hq : q ∈ D) :
      L q ≤ A ∧ (L q = A ↔ q.1 = -1 ∧
        ∀ j, m < x j → q.2.1 + q.2.2 = x j ∧ q.2.2 = (x j - m) / 2) := by
    let f (i : Fin n) := logCell q.1 (x i) q.2
    let cap (j : Fin n) := 2 * (log 2 - 1 - log (x j - m))
    have hpair (i j : Fin n) (hi : i ∈ S) (hj : j ∈ T) :
        f i + f j ≤ cap j ∧
        (f i + f j = cap j ↔
          q.1 = -1 ∧ q.2.1 + q.2.2 = x j ∧ q.2.2 = (x j - m) / 2) := by
      have hxy : x i < x j := (hzS i hi).trans_lt (hpT j hj)
      simpa only [f, cap, hzS i hi] using
        physical_pair hq.1 hxy hq.2.1 (hq.2.2 i) (hq.2.2 j)
    have hleft : (∑ j ∈ T, ∑ i ∈ S, (f i + f j)) = (S.card : ℝ) * L q := by
      have heq := Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ x i = m) f
      change (∑ i ∈ S, f i) + (∑ i ∈ T, f i) = ∑ i, f i at heq
      simp only [Finset.sum_add_distrib, Finset.sum_const, nsmul_eq_mul, hT,
        ← Finset.mul_sum, ← mul_add, heq]
      rfl
    have hright : (∑ j ∈ T, ∑ _i ∈ S, cap j) = (S.card : ℝ) * A := by
      simp only [cap, Finset.sum_const, nsmul_eq_mul, ← Finset.mul_sum,
        Finset.sum_sub_distrib, hsumT, hT, A]
      rw [hc']
      ring
    have hle : ∀ j ∈ T, (∑ i ∈ S, (f i + f j)) ≤ ∑ _i ∈ S, cap j :=
      fun j hj ↦ Finset.sum_le_sum fun i hi ↦ (hpair i j hi hj).1
    have heq : L q = A ↔ ∀ j ∈ T, ∀ i ∈ S, f i + f j = cap j := by
      have hmul : L q = A ↔ (S.card : ℝ) * L q = (S.card : ℝ) * A :=
        (mul_right_inj' hSc'.ne').symm
      rw [hmul, ← hleft, ← hright, Finset.sum_eq_sum_iff_of_le hle]
      exact forall₂_congr fun j hj ↦
        Finset.sum_eq_sum_iff_of_le fun i hi ↦ (hpair i j hi hj).1
    refine ⟨?_, heq.trans ⟨?_, ?_⟩⟩
    · have h := Finset.sum_le_sum hle
      rw [hleft, hright] at h
      exact le_of_mul_le_mul_left h hSc'
    · intro h
      obtain ⟨i, hi⟩ := hSne
      obtain ⟨j, hj⟩ := hTne
      refine ⟨((hpair i j hi hj).2.mp (h j hj i hi)).1, ?_⟩
      intro l hl
      have hlT : l ∈ T := Finset.mem_filter.mpr ⟨Finset.mem_univ _, hl.ne'⟩
      exact ((hpair i l hi hlT).2.mp (h l hlT i hi)).2
    · rintro ⟨hξ, h⟩ j hj i hi
      exact (hpair i j hi hj).2.mpr ⟨hξ, h j (hpT j hj)⟩
  have hz : ∃ i, x i = m := by
    obtain ⟨i, hi⟩ := hSne
    exact ⟨i, hzS i hi⟩
  obtain ⟨hpath, _, hlim⟩ := AllFreeGEV.half_minimum_collapse_path hn hm hz hc
  have hup : IsLUB (L '' D) A := by
    refine ⟨?_, ?_⟩
    · rintro _ ⟨q, hq, rfl⟩
      exact (hbound q hq).1
    · intro b hb
      apply le_of_tendsto hlim
      filter_upwards [self_mem_nhdsWithin] with e he
      exact hb ⟨(1, GEV.collapsePath x m 1 e), ⟨by norm_num, hpath e he⟩, rfl⟩
  refine ⟨hup, fun q hq ↦ (hbound q hq).2, ⟨?_, ?_⟩⟩
  · rintro ⟨q, hq, hmax⟩
    have hval : L q = A := le_antisymm (hbound q hq).1 (hup.2 (by
      rintro _ ⟨r, hr, rfl⟩
      exact hmax hr))
    exact ⟨2 * q.2.2, by have := hq.2.1; positivity, fun j hj ↦ by
      have h := ((hbound q hq).2.mp hval).2 j hj
      linarith only [h.2]⟩
  · rintro ⟨d, hd, heq⟩
    let q : ℝ × (ℝ × ℝ) := (-1, m + d / 2, d / 2)
    have hq : q ∈ D := by
      refine ⟨by norm_num, by positivity, fun i ↦ ?_⟩
      change if (-1 : ℝ) = -1 then 0 ≤ GEV.supportArg (-1) (x i) q.2
        else 0 < GEV.supportArg (-1) (x i) q.2
      simp only [ite_true, GEV.supportArg, q, neg_mul, one_mul]
      have hi : x i ≤ m + d := by
        rcases (hm i).eq_or_lt with hi | hi
        · linarith
        · linarith [heq i hi]
      have hdiv := (div_le_iff₀ (show 0 < d / 2 by positivity)).mpr
        (show x i - (m + d / 2) ≤ 1 * (d / 2) by linarith)
      linarith
    have hval : L q = A := (hbound q hq).2.mpr ⟨rfl, fun j hj ↦ by
      dsimp [q]; constructor <;> linarith [heq j hj]⟩
    exact ⟨q, hq, fun r hr ↦ hval ▸ (hbound r hr).1⟩

/-- The unrestricted spike, fitted majority divergence, and fitted half-tie
collapse, including the exact balanced-sample exception. -/
theorem all_free_failure {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) :
    let D : Set (ℝ × (ℝ × ℝ)) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain x q.1}
    let L := fun q : ℝ × (ℝ × ℝ) ↦ GEV.logLik x q.1 q.2
    (¬ BddAbove {v | ∃ ξ : ℝ, ∃ p ∈ GEV.domain x ξ, GEV.logLik x ξ p = v}) ∧
    ∀ m : ℝ, (∀ i, m ≤ x i) → (∃ i, x i = m) →
      let k := Frechet.minimumCount (GEV.lowerGaps x m)
      let A := (n : ℝ) * (log 2 - 1) - 2 * ∑ i, log (x i - m)
      (n < 2 * k →
        Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0) atTop ∧
        ¬ BddAbove (L '' D) ∧ ¬ ∃ q ∈ D, IsMaxOn L D q) ∧
      (n = 2 * k →
        Tendsto (fun e ↦ GEV.logLik x 1 (GEV.collapsePath x m 1 e)) (𝓝[>] 0) (𝓝 A) ∧
        IsLUB (L '' D) A ∧
        ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ d > 0, ∀ j, m < x j → x j - m = d)) := by
  intro D L
  refine ⟨AllFreeGEV.unrestricted_unbounded hn x, ?_⟩
  intro m hm hz k A
  refine ⟨?_, ?_⟩
  · intro hk
    have hu : ¬ BddAbove (L '' D) := by
      intro h
      apply AllFreeGEV.majority_unbounded hn hm hz hk
      apply h.mono
      rintro _ ⟨ξ, hξ, p, hp, rfl⟩
      exact ⟨(ξ, p), ⟨hξ, hp⟩, rfl⟩
    have hk' : (n : ℝ) < 2 * Frechet.minimumCount (GEV.lowerGaps x m) := by
      exact_mod_cast hk
    have hE : Frechet.collapseExponent (GEV.lowerGaps x m) ((1 : ℝ)⁻¹) < 0 := by
      unfold Frechet.collapseExponent
      norm_num
      linarith
    exact ⟨(GEV.positive_collapse_path hn one_pos hm hz).2.2.2 hE,
      hu, fun ⟨_, _, h⟩ ↦ hu h.bddAbove⟩
  · intro hk
    exact ⟨(AllFreeGEV.half_minimum_collapse_path hn hm hz hk).2.2,
      (all_free_half_supremum hn hm hk).1, (all_free_half_supremum hn hm hk).2.2⟩

private theorem logCell_at_location (ξ : ℝ) (p : ℝ × ℝ) :
    logCell ξ p.1 p = -log p.2 - 1 := by
  by_cases h : ξ = 0 <;> simp [logCell, GEV.supportArg, h]

private theorem logCell_far {ξ x : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : 0 < p.2) (hx : x ≠ p.1)
    (ha : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p) :
    logCell ξ x p ≤ log p.2 + 2 * log 2 - 1 - 2 * log |x - p.1| := by
  have h₀ : if ξ = -1 then 0 ≤ GEV.supportArg ξ p.1 p else 0 < GEV.supportArg ξ p.1 p := by
    split_ifs <;> norm_num [GEV.supportArg]
  rcases lt_or_gt_of_ne hx with h | h
  · have hb := (physical_pair hξ h hs ha h₀).1
    rw [logCell_at_location] at hb
    rw [abs_of_neg (sub_neg.mpr h), neg_sub]
    linarith only [hb]
  · have hb := (physical_pair hξ h hs h₀ ha).1
    rw [logCell_at_location] at hb
    rw [abs_of_pos (sub_pos.mpr h)]
    linarith only [hb]

private def logRate (ξ x : ℝ) (p : ℝ × ℝ) : ℝ :=
  if ξ = 0 then -((x - p.1) / p.2) else log (GEV.supportArg ξ x p) * (-1 / ξ)

private theorem logCell_rate {ξ x : ℝ} {p : ℝ × ℝ} (h : 0 < GEV.supportArg ξ x p) :
    logCell ξ x p = -log p.2 + (1 + ξ) * logRate ξ x p - exp (logRate ξ x p) := by
  by_cases hz : ξ = 0
  · subst ξ
    simp [logCell, logRate]
    ring
  · simp only [logCell, logRate, ite_eq_right hz, rpow_def_of_pos h]
    field_simp
    ring

private theorem logCell_near {ξ x : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1)
    (ha : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p) :
    logCell ξ x p ≤ -log p.2 + 2 * log 2 := by
  by_cases h : 0 < GEV.supportArg ξ x p
  · rw [logCell_rate h]
    let r := logRate ξ x p
    change -log p.2 + (1 + ξ) * r - exp r ≤ _
    rcases le_total r 0 with hr | hr
    · nlinarith only [mul_nonpos_of_nonneg_of_nonpos (by linarith [hξ.1] : 0 ≤ 1 + ξ)
        hr, exp_pos r, log_pos (by norm_num : (1 : ℝ) < 2)]
    · have he := log_le_sub_one_of_pos (div_pos (exp_pos r) (by norm_num : (0 : ℝ) < 2))
      rw [log_div (exp_pos _).ne' two_ne_zero, log_exp] at he
      nlinarith only [he, mul_nonpos_of_nonpos_of_nonneg (by linarith [hξ.2] : ξ - 1 ≤ 0) hr]
  · have hξ' : ξ = -1 := by
      by_contra hn
      exact h (by simpa only [ite_eq_right hn] using ha)
    subst ξ
    have hzero : GEV.supportArg (-1) x p = 0 :=
      le_antisymm (le_of_not_gt h) (by simpa only [ite_true] using ha)
    norm_num [logCell, hzero]
    positivity

private theorem logCell_left {ξ x : ℝ} {p : ℝ × ℝ} (N : ℕ)
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : 0 < p.2) (hx : x < p.1)
    (ha : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p) :
    logCell ξ x p ≤ -log p.2 + (N + 2 : ℝ) * log (N + 2) -
      (N : ℝ) * log ((p.1 - x) / p.2) := by
  let u := (p.1 - x) / p.2
  have hu : 0 < u := div_pos (sub_pos.mpr hx) hs
  have heq : GEV.supportArg ξ x p = 1 - ξ * u := by unfold GEV.supportArg u; ring
  have harg : 0 < GEV.supportArg ξ x p := by
    by_cases h : ξ = -1
    · rw [heq, h]; linarith
    · simpa only [ite_eq_right h] using ha
  let r := logRate ξ x p
  have hr : 0 ≤ r := by
    unfold r logRate
    split_ifs with hz
    · rw [← neg_div, neg_sub]; exact hu.le
    · rw [heq]
      rcases lt_or_gt_of_ne hz with hn | hp
      · exact mul_nonneg (log_nonneg (by nlinarith only [mul_neg_of_neg_of_pos hn hu]))
          (div_nonneg_of_nonpos (by norm_num) hn.le)
      · exact mul_nonneg_of_nonpos_of_nonpos
          (log_nonpos (heq ▸ harg).le (by nlinarith only [mul_pos hp hu]))
          (div_nonpos_of_nonpos_of_nonneg (by norm_num) hp.le)
  have hy : 1 + u ≤ exp r := by
    by_cases hz : ξ = 0
    · have hr' : r = u := by simp [r, logRate, hz, u]; ring
      rw [hr']
      linarith [add_one_le_exp u]
    have hrel : exp (-ξ * r) = 1 - ξ * u := by
      rw [← heq, show -ξ * r = log (GEV.supportArg ξ x p) by
        simp only [r, logRate, ite_eq_right hz]; field_simp, exp_log harg]
    rcases lt_or_gt_of_ne hz with hn | hp
    · have h := convexOn_exp.2 (mem_univ r) (mem_univ (0 : ℝ)) (show 0 ≤ -ξ by linarith)
        (show 0 ≤ 1 + ξ by linarith [hξ.1]) (show -ξ + (1 + ξ) = 1 by ring)
      simp only [smul_eq_mul, mul_zero, add_zero, exp_zero, mul_one] at h
      rw [hrel] at h
      by_contra! hh
      nlinarith only [h, mul_pos_of_neg_of_neg hn (show exp r - (1 + u) < 0 by linarith)]
    · have h := add_one_le_exp (-ξ * r)
      rw [hrel] at h
      have hur : u ≤ r := by
        by_contra! hh
        nlinarith only [h, mul_pos hp (show 0 < u - r by linarith)]
      linarith [add_one_le_exp r]
  have hlog : log u ≤ r := by
    rw [← log_exp r]
    exact log_le_log hu (by linarith)
  let C : ℝ := N + 2
  have hC : 0 < C := by dsimp [C]; positivity
  have hn := log_le_sub_one_of_pos (div_pos (exp_pos r) hC)
  rw [log_div (exp_pos _).ne' hC.ne', log_exp] at hn
  have hbound := mul_le_mul_of_nonneg_left hn hC.le
  have hidentity : C * (exp r / C - 1) = exp r - C := by field_simp
  rw [hidentity] at hbound
  rw [logCell_rate harg]
  change -log p.2 + (1 + ξ) * r - exp r ≤ _
  dsimp [C] at hbound
  nlinarith only [hbound, hC, mul_nonpos_of_nonpos_of_nonneg
    (by linarith [hξ.2] : ξ - 1 ≤ 0) hr,
    mul_le_mul_of_nonneg_left hlog (Nat.cast_nonneg N)]

/-- Uniform log-density envelopes on the fitted branch. The far bound is
quadratic in distance, and on the left the decay beats any fixed power. -/
theorem fitted_logDensity_envelopes {ξ x : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : 0 < p.2)
    (ha : if ξ = -1 then 0 ≤ GEV.supportArg ξ x p else 0 < GEV.supportArg ξ x p) :
    logCell ξ x p ≤ -log p.2 + 2 * log 2 ∧
    (x ≠ p.1 → logCell ξ x p ≤ log p.2 + 2 * log 2 - 1 - 2 * log |x - p.1|) ∧
    (∀ N : ℕ, x < p.1 → logCell ξ x p ≤ -log p.2 + (N + 2 : ℝ) * log (N + 2) -
      (N : ℝ) * log ((p.1 - x) / p.2)) :=
  ⟨logCell_near hξ ha, fun h ↦ logCell_far hξ hs h ha, fun N h ↦ logCell_left N hξ hs h ha⟩

private theorem logLik_large {n : ℕ} (x : Fin n → ℝ) {ξ : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hp : p ∈ GEV.domain x ξ) :
    GEV.logLik x ξ p ≤ (n : ℝ) * (-log p.2 + 2 * log 2) := by
  change (∑ i, logCell ξ (x i) p) ≤ _
  simpa only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] using
    (Finset.sum_le_sum (fun i _ ↦ logCell_near hξ (hp.2 i)) :
    (∑ i, logCell ξ (x i) p) ≤ ∑ _i : Fin n, (-log p.2 + 2 * log 2))

private theorem logLik_left {n : ℕ} (x : Fin n → ℝ) {ξ δ : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hp : p ∈ GEV.domain x ξ)
    (hδ : 0 < δ) (i : Fin n) (hi : δ ≤ p.1 - x i) :
    GEV.logLik x ξ p ≤ log p.2 + ((n : ℝ) + 3) * log ((n : ℝ) + 3) +
      (n : ℝ) * (2 * log 2) - ((n : ℝ) + 1) * log δ := by
  let B := -log p.2 + 2 * log 2
  let C := ((n : ℝ) + 3) * log ((n : ℝ) + 3)
  have hcell := logCell_left (n + 1) hξ hp.1 (show x i < p.1 by linarith) (hp.2 i)
  have hlog := log_le_log hδ hi
  rw [log_div (by linarith : p.1 - x i ≠ 0) hp.1.ne'] at hcell
  push_cast at hcell
  rw [show (n : ℝ) + 1 + 2 = n + 3 by ring] at hcell
  have h0 := mul_le_mul_of_nonneg_left hlog (show 0 ≤ (n : ℝ) + 1 by positivity)
  have h2 := log_pos (by norm_num : (1 : ℝ) < 2)
  let H := -log p.2 + C - ((n : ℝ) + 1) * (log δ - log p.2)
  have hH : logCell ξ (x i) p ≤ H := by dsimp [H, C]; nlinarith only [hcell, h0]
  have hsum : (∑ j, logCell ξ (x j) p) ≤ ∑ j : Fin n, (B + if j = i then H - B else 0) := by
    apply Finset.sum_le_sum
    intro j _
    by_cases he : j = i
    · subst j; simpa using hH
    · simpa [he, B] using logCell_near hξ (hp.2 j)
  simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, Finset.sum_ite_eq', Finset.mem_univ, ite_true] at hsum
  change (∑ j, logCell ξ (x j) p) ≤ _
  dsimp [H, B, C] at hsum
  linarith only [hsum, h2]

private theorem logLik_distance {n : ℕ} (x : Fin n → ℝ) {ξ δ : ℝ} {p : ℝ × ℝ}
    (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hp : p ∈ GEV.domain x ξ)
    (hδ : 0 < δ) (T : Finset (Fin n)) (hT : ∀ i ∈ T, δ ≤ |x i - p.1|) :
    GEV.logLik x ξ p ≤
      (2 * (T.card : ℝ) - n) * log p.2 + (n : ℝ) * (2 * log 2) -
        (T.card : ℝ) * (1 + 2 * log δ) := by
  let B := -log p.2 + 2 * log 2
  let H := log p.2 + 2 * log 2 - 1 - 2 * log δ
  have hsum : (∑ i, logCell ξ (x i) p) ≤ ∑ i : Fin n, (B + if i ∈ T then H - B else 0) := by
    apply Finset.sum_le_sum
    intro i _
    by_cases hi : i ∈ T
    · have hdist := hT i hi
      have ht : x i ≠ p.1 := by rintro he; simp [he] at hdist; linarith
      have hb := logCell_far hξ hp.1 ht (hp.2 i)
      have hl := log_le_log hδ hdist
      simp only [ite_eq_left hi]
      dsimp [B, H]
      linarith only [hb, hl]
    · simpa [hi, B] using logCell_near hξ (hp.2 i)
  rw [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, Finset.sum_ite, Finset.sum_const_zero, add_zero, Finset.sum_const,
    nsmul_eq_mul] at hsum
  simp only [Finset.filter_mem_eq_inter, Finset.univ_inter] at hsum
  change (∑ i, logCell ξ (x i) p) ≤ _
  dsimp [B, H] at hsum
  linarith only [hsum]

private theorem positive_gap {n : ℕ} {x : Fin n → ℝ} {m : ℝ} :
    ∃ δ > 0, ∀ i, m < x i → 2 * δ ≤ x i - m := by
  classical
  let T := Finset.univ.filter fun i ↦ m < x i
  by_cases hT : T.Nonempty
  · obtain ⟨j, hj, hjmin⟩ := Finset.exists_min_image T x hT
    have hj' := (Finset.mem_filter.mp hj).2
    refine ⟨(x j - m) / 2, by linarith, fun i hi ↦ ?_⟩
    have h := hjmin i (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩)
    linarith
  · exact ⟨1, one_pos, fun i hi ↦ (hT ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩).elim⟩

private theorem scale_bounds {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m b : ℝ}
    (hcase : (∃ i, x i < m) ∨
      ((∀ i, m ≤ x i) ∧ 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n)) :
    ∃ lo > 0, ∃ hi > 0, ∀ ξ p, ξ ∈ Icc (-1 : ℝ) 1 → p ∈ GEV.domain x ξ →
      b ≤ GEV.logLik x ξ p → (p.1 = m ∨ (∀ i, m ≤ x i) ∧ (∃ i, x i = m)) → p.2 ∈ Icc lo hi := by
  classical
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  obtain ⟨δ, hδ, hsep⟩ := positive_gap (x := x) (m := m)
  let B := ((n : ℝ) + 3) * log ((n : ℝ) + 3) + (n : ℝ) * (2 * log 2)
  let T := Finset.univ.filter fun i ↦ m < x i
  let C := (n : ℝ) * (2 * log 2) - (T.card : ℝ) * (1 + 2 * log δ)
  let U := exp (2 * log 2 - b / n)
  let V := exp ((b - C) / (2 * (T.card : ℝ) - n))
  let W := fun d : ℝ ↦ exp (b - B + ((n : ℝ) + 1) * log d)
  let L := min V (W δ)
  have upper (ξ : ℝ) (p : ℝ × ℝ) (hξ : ξ ∈ Icc (-1 : ℝ) 1)
      (hp : p ∈ GEV.domain x ξ) (hb : b ≤ GEV.logLik x ξ p) : p.2 ≤ U := by
    rw [← exp_log hp.1, exp_le_exp]
    have he := hb.trans (logLik_large x hξ hp)
    rw [mul_comm] at he
    have h := (div_le_iff₀ hn').mpr he
    linarith only [h]
  rcases hcase with ⟨i, hi⟩ | ⟨hm, hk⟩
  · refine ⟨W (m - x i), exp_pos _, U, exp_pos _, fun ξ p hξ hp hb hloc ↦ ⟨?_, upper ξ p hξ hp hb⟩⟩
    rcases hloc with he | ⟨hm, _⟩
    · have h := hb.trans (logLik_left x hξ hp (sub_pos.mpr hi) i
        (by rw [he] : m - x i ≤ p.1 - x i))
      apply (exp_le_exp.mpr _).trans_eq (exp_log hp.1)
      dsimp [W, B]
      linarith only [h]
    · exact (hi.not_ge (hm i)).elim
  · have hcard : 2 * (T.card : ℝ) - n > 0 := by
      let S := Finset.univ.filter fun i ↦ x i = m
      have hS : S.card = Frechet.minimumCount (GEV.lowerGaps x m) := by
        simp [S, Frechet.minimumCount, GEV.lowerGaps, sub_eq_zero]
      have hT : T = Finset.univ.filter (fun i ↦ ¬ x i = m) := by
        ext i
        simp only [T, Finset.mem_filter, Finset.mem_univ, true_and]
        exact ⟨fun h ↦ h.ne', fun h ↦ lt_of_le_of_ne (hm i) (Ne.symm h)⟩
      have hST : S.card + T.card = n := by
        rw [hT]
        simpa [S] using (Finset.card_filter_add_card_filter_not
          (s := (Finset.univ : Finset (Fin n))) (fun i ↦ x i = m))
      have hk' : n < 2 * T.card := by omega
      have hk'' : (n : ℝ) < 2 * (T.card : ℝ) := by exact_mod_cast hk'
      linarith
    refine ⟨L, lt_min (exp_pos _) (exp_pos _), U, exp_pos _,
      fun ξ p hξ hp hb hloc ↦ ⟨?_, upper ξ p hξ hp hb⟩⟩
    by_cases hl : p.1 ≤ m + δ
    · have hfar : ∀ i ∈ T, δ ≤ |x i - p.1| := by
        intro i hi
        have h := hsep i (Finset.mem_filter.mp hi).2
        exact (show δ ≤ x i - p.1 by linarith).trans (le_abs_self _)
      have h := hb.trans (logLik_distance x hξ hp hδ T hfar)
      apply (min_le_left _ _).trans
      apply (exp_le_exp.mpr ((div_le_iff₀ hcard).mpr _)).trans_eq (exp_log hp.1)
      dsimp [C]
      linarith only [h]
    · rcases hloc with he | ⟨_, i, hi⟩
      · exact (hl (by linarith)).elim
      · have h := hb.trans (logLik_left x hξ hp hδ i (by linarith [lt_of_not_ge hl]))
        apply (min_le_right _ _).trans
        apply (exp_le_exp.mpr _).trans_eq (exp_log hp.1)
        dsimp [W, B]; linarith only [h]

/-- Physical parameters in the order shape, location, scale. -/
abbrev Parameters := ℝ × (ℝ × ℝ)

/-- Fitted shapes and strictly positive scale, with support extended by zero. -/
def chart : Set Parameters :=
  {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ 0 < q.2.2}

/-- Feasible physical parameters, including the closed reverse-exponential contact. -/
def domain {n : ℕ} (x : Fin n → ℝ) : Set Parameters :=
  {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ q.2 ∈ GEV.domain x q.1}

/-- The joint likelihood, with density zero off the support. -/
def likelihood {n : ℕ} (x : Fin n → ℝ) (q : Parameters) : ℝ := by
  classical
  exact if q.2 ∈ GEV.domain x q.1 then exp (GEV.logLik x q.1 q.2) else 0

private theorem usc_mul {α : Type*} [TopologicalSpace α] {D : Set α}
    {f g : α → ℝ} (hf : UpperSemicontinuousOn f D) (hg : UpperSemicontinuousOn g D)
    (hfn : ∀ x ∈ D, 0 ≤ f x) (hgn : ∀ x ∈ D, 0 ≤ g x) :
    UpperSemicontinuousOn (fun x ↦ f x * g x) D := by
  intro p hp b hb
  let h := fun e : ℝ ↦ (f p + e) * (g p + e)
  have hc : Continuous h := by unfold h; fun_prop
  have h0 : h 0 < b := by simpa [h] using hb
  have he := hc.continuousAt.eventually (Iio_mem_nhds h0)
  have hpos : ∀ᶠ e in 𝓝[>] (0 : ℝ), e ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨e, hp0, heb⟩ := (hpos.and (he.filter_mono nhdsWithin_le_nhds)).exists
  filter_upwards [hf p hp (f p + e) (lt_add_of_pos_right _ hp0),
    hg p hp (g p + e) (lt_add_of_pos_right _ hp0), self_mem_nhdsWithin]
    with q hqf hqg hq
  exact (mul_le_mul hqf.le hqg.le (hgn q hq)
    (add_nonneg (hfn p hp) hp0.le)).trans_lt heb

private theorem usc_prod {α ι : Type*} [TopologicalSpace α] {D : Set α}
    {f : ι → α → ℝ} (s : Finset ι) (hf : ∀ i ∈ s, UpperSemicontinuousOn (f i) D)
    (hn : ∀ i ∈ s, ∀ x ∈ D, 0 ≤ f i x) :
    UpperSemicontinuousOn (fun x ↦ ∏ i ∈ s, f i x) D := by
  classical
  induction s using Finset.induction_on with
  | empty =>
    simp only [Finset.prod_empty]
    exact continuousOn_const.upperSemicontinuousOn
  | @insert i s hi ih =>
    simp only [Finset.prod_insert hi]
    exact usc_mul (hf i (Finset.mem_insert_self ..))
      (ih (fun j hj ↦ hf j (Finset.mem_insert_of_mem hj))
        (fun j hj ↦ hn j (Finset.mem_insert_of_mem hj)))
      (hn i (Finset.mem_insert_self ..))
      (fun x hx ↦ Finset.prod_nonneg (fun j hj ↦ hn j (Finset.mem_insert_of_mem hj) x hx))

private def standardized (x : ℝ) (q : Parameters) : ℝ × ℝ :=
  ((q.2.1 - x) / q.2.2, q.1)

private def stdLik : (ℝ × ℝ) → ℝ :=
  JoinsGEV.heldScaleLikelihood (fun _ : Fin 1 ↦ 0) 1

private theorem stdLik_nonneg (p : ℝ × ℝ) : 0 ≤ stdLik p := by
  classical
  unfold stdLik JoinsGEV.heldScaleLikelihood
  split_ifs <;> positivity

private theorem likelihood_prod {n : ℕ} (x : Fin n → ℝ) {q : Parameters} (hq : q ∈ chart) :
    likelihood x q = (q.2.2⁻¹)^n * ∏ i, stdLik (standardized (x i) q) := by
  classical
  have hs : 0 < q.2.2 := hq.2
  let z := fun _ : Fin 1 ↦ (0 : ℝ)
  have hres (i : Fin n) :
      (0 - (standardized (x i) q).1) / 1 = (x i - q.2.1) / q.2.2 := by
    unfold standardized
    dsimp only
    ring
  have hsup (i : Fin n) :
      GEV.supportArg q.1 0 ((standardized (x i) q).1, 1) =
      GEV.supportArg q.1 (x i) q.2 := by
    unfold GEV.supportArg
    rw [hres]
  have hd (i : Fin n) :
      ((standardized (x i) q).1, 1) ∈ GEV.domain z q.1 ↔
        if q.1 = -1 then 0 ≤ GEV.supportArg q.1 (x i) q.2
        else 0 < GEV.supportArg q.1 (x i) q.2 := by
    change (0 < (1 : ℝ) ∧ ∀ _ : Fin 1, _) ↔ _
    simp only [zero_lt_one, true_and, hsup, Fin.forall_fin_one, z]
  have hcell (i : Fin n) :
      GEV.logLik z q.1 ((standardized (x i) q).1, 1) =
        logCell q.1 (x i) q.2 + log q.2.2 := by
    unfold GEV.logLik logCell
    simp only [Fin.sum_univ_one, z, log_one, neg_zero, hres, hsup]
    split_ifs <;> ring
  unfold likelihood
  split_ifs with h
  · have hval (i : Fin n) : stdLik (standardized (x i) q) =
        exp (logCell q.1 (x i) q.2) * q.2.2 := by
      have hi := (hd i).mpr (h.2 i)
      change (if ((standardized (x i) q).1, 1) ∈ GEV.domain z q.1 then
        exp (GEV.logLik z q.1 ((standardized (x i) q).1, 1)) else 0) = _
      rw [ite_eq_left hi, hcell, exp_add, exp_log hs]
    simp only [hval, Finset.prod_mul_distrib, Finset.prod_const, Finset.card_univ,
      Fintype.card_fin, ← exp_sum]
    change exp (GEV.logLik x q.1 q.2) =
      (q.2.2⁻¹)^n * (exp (GEV.logLik x q.1 q.2) * q.2.2^n)
    rw [inv_pow, mul_left_comm, inv_mul_cancel₀ (pow_ne_zero _ hs.ne'), mul_one]
  · have hbad : ∃ i, ¬ (((standardized (x i) q).1, 1) ∈ GEV.domain z q.1) := by
      by_contra! hh
      exact h ⟨hs, fun i ↦ (hd i).mp (hh i)⟩
    obtain ⟨i, hi⟩ := hbad
    have hzero : stdLik (standardized (x i) q) = 0 := by
      exact ite_eq_right hi
    rw [Finset.prod_eq_zero (Finset.mem_univ i) hzero, mul_zero]

/-- The support-extended likelihood is jointly upper semicontinuous on the fitted
shape interval with positive scale. -/
theorem fitted_upperSemicontinuous {n : ℕ} (x : Fin n → ℝ) :
    UpperSemicontinuousOn (likelihood x) chart := by
  suffices h : UpperSemicontinuousOn
      (fun q : Parameters ↦ (q.2.2⁻¹)^n * ∏ i, stdLik (standardized (x i) q)) chart by
    intro q hq b hb
    rw [likelihood_prod x hq] at hb
    filter_upwards [h q hq b hb, self_mem_nhdsWithin] with r hr hr'
    rwa [likelihood_prod x hr']
  apply usc_mul (f := fun q : Parameters ↦ (q.2.2⁻¹)^n)
  · apply ContinuousOn.upperSemicontinuousOn
    intro q hq
    exact (continuous_snd.snd.continuousAt.inv₀ (ne_of_gt hq.2)).pow n
      |>.continuousWithinAt
  · apply usc_prod Finset.univ
    · intro i _ q hq b hb
      have hf : ContinuousAt (standardized (x i)) q := by
        unfold standardized
        exact ((continuous_snd.fst.continuousAt.sub continuousAt_const).div
          continuous_snd.snd.continuousAt (ne_of_gt hq.2)).prodMk continuous_fst.continuousAt
      let E := (univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1
      have hm : Set.MapsTo (standardized (x i)) chart E := fun _ h ↦ ⟨mem_univ _, h.1⟩
      have husc := JoinsGEV.held_scale_upperSemicontinuous (fun _ : Fin 1 ↦ (0 : ℝ)) one_pos
      exact (husc _ (hm hq) b hb).filter_mono
        (hf.continuousWithinAt.tendsto_nhdsWithin hm)
    · exact fun _ _ _ _ ↦ stdLik_nonneg _
  · exact fun _ h ↦ pow_nonneg (inv_nonneg.mpr h.2.le) _
  · exact fun _ _ ↦ Finset.prod_nonneg (fun _ _ ↦ stdLik_nonneg _)

private theorem positive_level {n : ℕ} {x : Fin n → ℝ} {q : Parameters} {e : ℝ}
    (he : 0 < e) (hq : q ∈ chart) (h : e ≤ likelihood x q) :
    q ∈ domain x ∧ log e ≤ GEV.logLik x q.1 q.2 := by
  classical
  by_cases hd : q.2 ∈ GEV.domain x q.1
  · exact ⟨⟨hq.1, hd⟩, (log_le_iff_le_exp he).mpr (by simpa [likelihood, hd] using h)⟩
  · have hz : likelihood x q = 0 := by simp [likelihood, hd]
    exact (he.not_ge (hz ▸ h)).elim

private theorem compact_restricted {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ}
    {m : ℝ} {E : Set ℝ} (hE : IsClosed E)
    (hcase : (∃ i, x i < m) ∨
      ((∀ i, m ≤ x i) ∧ 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n))
    (hloc : ∀ l ∈ E, l = m ∨ (∀ i, m ≤ x i) ∧ ∃ i, x i = m)
    {e : ℝ} (he : 0 < e) :
    IsCompact ((chart ∩ {q : Parameters | q.2.1 ∈ E}) ∩ likelihood x ⁻¹' Ici e) := by
  classical
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  obtain ⟨lo, hlo, hi, hhi, hb⟩ := scale_bounds hn (b := log e) hcase
  let δ := exp ((log hi + 2 * log 2 - log e / n) / 2 + 1)
  have hδ : 0 < δ := exp_pos _
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ (fun i ↦ |x i|)
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let R := |x j| + δ
  let K := Icc (-1 : ℝ) 1 ×ˢ (Icc (-R) R ×ˢ Icc lo hi)
  let C := K ∩ {q : Parameters | q.2.1 ∈ E}
  have hK : IsCompact K := isCompact_Icc.prod (isCompact_Icc.prod isCompact_Icc)
  have hC : IsCompact C := hK.inter_right (hE.preimage continuous_snd.fst)
  have hsub : C ⊆ chart ∩ {q : Parameters | q.2.1 ∈ E} :=
    fun _ h ↦ ⟨⟨h.1.1, hlo.trans_le h.1.2.2.1⟩, h.2⟩
  suffices h : (chart ∩ {q : Parameters | q.2.1 ∈ E}) ∩ likelihood x ⁻¹' Ici e =
      C ∩ likelihood x ⁻¹' Ici e by
    rw [h]
    exact ((fitted_upperSemicontinuous x).mono (hsub.trans inter_subset_left))
      |>.isCompact_inter_preimage_Ici hC e
  refine Subset.antisymm ?_ (fun _ h ↦ ⟨hsub h.1, h.2⟩)
  rintro q ⟨⟨hq, hqE⟩, hv⟩
  obtain ⟨hd, hv'⟩ := positive_level he hq hv
  have hs := hb q.1 q.2 hd.1 hd.2 hv' (hloc _ hqE)
  have hR : |q.2.1| ≤ R := by
    by_contra! h
    have hfar : ∀ i : Fin n, δ ≤ |x i - q.2.1| := by
      intro i
      have ht : |q.2.1| ≤ |x i| + |x i - q.2.1| := by
        have ht := abs_add_le (x i) (q.2.1 - x i)
        rwa [show x i + (q.2.1 - x i) = q.2.1 by ring, abs_sub_comm] at ht
      have := hj i (Finset.mem_univ _)
      dsimp [R] at h
      linarith
    have hval := hv'.trans (logLik_distance x hd.1 hd.2 hδ Finset.univ (fun i _ ↦ hfar i))
    simp only [Finset.card_univ, Fintype.card_fin] at hval
    have hlog := log_le_log hd.2.1 hs.2
    have hh : log e / n ≤ log q.2.2 + 2 * log 2 - (1 + 2 * log δ) := by
      apply (div_le_iff₀ hn').mpr
      linarith only [hval]
    dsimp [δ] at hh
    rw [log_exp] at hh
    linarith
  exact ⟨⟨⟨hq.1, abs_le.mp hR, hs⟩, hqE⟩, hv⟩

/-- For an attained sample minimum with strict minority multiplicity, every
positive likelihood superlevel is compact in the free physical chart. -/
theorem all_free_compact {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m)
    (hk : 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n) {e : ℝ} (he : 0 < e) :
    IsCompact (chart ∩ likelihood x ⁻¹' Ici e) := by
  simpa using compact_restricted hn (E := univ) isClosed_univ
    (Or.inr ⟨hm, hk⟩) (fun _ _ ↦ Or.inr ⟨hm, hz⟩) he

/-- With location held, negative residuals or a strict minority of zeros
suffice for compact positive likelihood superlevels. -/
theorem held_location_compact {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (h : (∃ i, x i < m) ∨ 2 * Frechet.minimumCount (GEV.lowerGaps x m) < n)
    {e : ℝ} (he : 0 < e) :
    IsCompact ((chart ∩ {q : Parameters | q.2.1 = m}) ∩ likelihood x ⁻¹' Ici e) := by
  by_cases hn' : ∃ i, x i < m
  · exact compact_restricted hn (E := {m}) isClosed_singleton (Or.inl hn')
      (fun _ h ↦ Or.inl h) he
  · have hm : ∀ i, m ≤ x i := fun i ↦ le_of_not_gt (fun hi ↦ hn' ⟨i, hi⟩)
    exact compact_restricted hn (E := {m}) isClosed_singleton
      (Or.inr ⟨hm, h.resolve_left hn'⟩) (fun _ h ↦ Or.inl h) he

private theorem attain_restricted {n : ℕ} {x : Fin n → ℝ} {E : Set Parameters}
    (hE : E ⊆ chart)
    (hcomp : ∀ e > 0, IsCompact (E ∩ likelihood x ⁻¹' Ici e))
    {q₀ : Parameters} (h₀ : q₀ ∈ E) (h₀D : q₀ ∈ domain x) :
    ∃ q ∈ E ∩ domain x, IsMaxOn (fun r ↦ GEV.logLik x r.1 r.2) (E ∩ domain x) q := by
  classical
  let e := likelihood x q₀
  have he : 0 < e := by simp [e, likelihood, h₀D.2, exp_pos]
  have hne : (E ∩ likelihood x ⁻¹' Ici e).Nonempty := ⟨q₀, h₀, le_rfl⟩
  obtain ⟨q, hq, hmax⟩ := UpperSemicontinuousOn.exists_isMaxOn hne
    (hcomp e he) ((fitted_upperSemicontinuous x).mono (inter_subset_left.trans hE))
  have hd := (positive_level he (hE hq.1) hq.2).1
  refine ⟨q, ⟨hq.1, hd⟩, fun r hr ↦ ?_⟩
  have hcmp : likelihood x r ≤ likelihood x q := by
    by_cases h : e ≤ likelihood x r
    · exact hmax ⟨hr.1, h⟩
    · exact (le_of_not_ge h).trans hq.2
  change GEV.logLik x r.1 r.2 ≤ GEV.logLik x q.1 q.2
  simpa only [likelihood, ite_eq_left hr.2.2, ite_eq_left hd.2, exp_le_exp] using hcmp

/-- On the fitted branch a strict minimum minority attains a maximum.
At half contact exactly the balanced two-value samples attain, and
a strict minimum majority never attains. -/
theorem all_free_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i) (hz : ∃ i, x i = m) :
    let k := Frechet.minimumCount (GEV.lowerGaps x m)
    (∃ q ∈ domain x, IsMaxOn (fun r ↦ GEV.logLik x r.1 r.2) (domain x) q) ↔
      2 * k < n ∨ (n = 2 * k ∧ ∃ d > 0, ∀ i, m < x i → x i - m = d) := by
  intro k
  by_cases hlt : 2 * k < n
  · have hd : (0, 0, 1) ∈ domain x := by norm_num [domain, GEV.domain, GEV.supportArg]
    have hchart : domain x ⊆ chart := fun _ h ↦ ⟨h.1, h.2.1⟩
    have he : chart ∩ domain x = domain x := inter_eq_right.mpr hchart
    have h := attain_restricted (Subset.rfl : chart ⊆ chart)
      (fun _ he ↦ all_free_compact hn hm hz hlt he) (hchart hd) hd
    rw [he] at h
    simp [hlt, h]
  · by_cases heq : n = 2 * k
    · simpa only [domain, heq, lt_irrefl, false_or, true_and] using
        (all_free_half_supremum hn hm heq).2.2
    · have hgt : n < 2 * k := by omega
      have hh := ((all_free_failure hn x).2 m hm hz).1 hgt
      simp [hlt, heq, show ¬ ∃ q ∈ domain x,
        IsMaxOn (fun r ↦ GEV.logLik x r.1 r.2) (domain x) q from hh.2.2]

/-- On the fitted branch with location held, all negative-residual and
strict zero-minority samples attain. With all residuals nonnegative at
half contact attainment is equivalent to equality of all positive residuals. -/
theorem held_location_criterion {n : ℕ} (hn : 0 < n) {x : Fin n → ℝ} (m : ℝ) :
    let k := Frechet.minimumCount (GEV.lowerGaps x m)
    let D : Set (ℝ × ℝ) := {p | p.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, p.2) ∈ GEV.domain x p.1}
    let L := fun p : ℝ × ℝ ↦ GEV.logLik x p.1 (m, p.2)
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∃ i, x i < m) ∨ 2 * k < n ∨
        ((∀ i, m ≤ x i) ∧ n = 2 * k ∧ ∃ d > 0, ∀ i, m < x i → x i - m = d) := by
  classical
  intro k D L
  by_cases h : (∃ i, x i < m) ∨ 2 * k < n
  · have hd : (0, m, 1) ∈ domain x := by norm_num [domain, GEV.domain, GEV.supportArg]
    let E := chart ∩ {q : Parameters | q.2.1 = m}
    have hE : (0, m, 1) ∈ E := ⟨⟨hd.1, hd.2.1⟩, rfl⟩
    obtain ⟨q, hq, hmax⟩ := attain_restricted (E := E) inter_subset_left
      (fun _ he ↦ held_location_compact hn h he) hE hd
    have hloc : q.2.1 = m := hq.1.2
    have hqeq : q.2 = (m, q.2.2) := Prod.ext hloc rfl
    have hp : (q.1, q.2.2) ∈ D := ⟨hq.2.1, hqeq ▸ hq.2.2⟩
    have hmax' : IsMaxOn L D (q.1, q.2.2) := by
      intro r hr
      have hh := hmax (show (r.1, m, r.2) ∈ E ∩ domain x from
        ⟨⟨⟨hr.1, hr.2.1⟩, rfl⟩, hr⟩)
      change GEV.logLik x r.1 (m, r.2) ≤ GEV.logLik x q.1 q.2 at hh
      exact hqeq ▸ hh
    exact ⟨fun _ ↦ h.elim Or.inl (fun hh ↦ Or.inr (Or.inl hh)),
      fun _ ↦ ⟨(q.1, q.2.2), hp, hmax'⟩⟩
  · have hneg : ¬ ∃ i, x i < m := fun hn' ↦ h (Or.inl hn')
    have hlt : ¬ 2 * k < n := fun hn' ↦ h (Or.inr hn')
    have hm : ∀ i, m ≤ x i := fun i ↦ le_of_not_gt (fun hi ↦ hneg ⟨i, hi⟩)
    have hk : k = (Finset.univ.filter fun i ↦ x i = m).card := by
      simp [k, Frechet.minimumCount, GEV.lowerGaps, sub_eq_zero]
    have H := Base9GEV.held_location_failure hn hm
    by_cases he : n = 2 * k
    · simpa only [hneg, he, lt_irrefl, false_or, hm, implies_true, true_and] using
        (H.2.2 (by rwa [hk] at he)).2.2
    · have hgt : n < 2 * k := by omega
      simpa only [hneg, hlt, false_or, he, false_and, and_false, iff_false] using
        (H.2.1 (by rwa [hk] at hgt)).2.2

end
end UnivariateML.EVGEV

end
