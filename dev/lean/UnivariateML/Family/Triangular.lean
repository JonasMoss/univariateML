module

public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Data.Fin.Tuple.Sort
public import Mathlib.Order.Filter.Extr

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Triangular likelihood with held endpoints

The sample is nonempty. The density at the mode is the peak height, including
when the mode coincides with an endpoint.

## Deviations

The classical order-statistic result is extended to closed support. If both
endpoints are observed, the likelihood vanishes for every mode; the closed
model then has no positive-likelihood fit. The dossier's inference entry
(a < c < b) describes the interior regime. The results here also cover the
endpoint modes admitted by the fitter. Endpoint optimization is outside scope.
-/

@[expose] public section

noncomputable section

open Set Real
open scoped BigOperators

namespace UnivariateML.Triangular

variable {n : ℕ} {a b : ℝ}

/-- Density divided by the common peak height `2 / (b - a)`, on the support. -/
def factor (a b c z : ℝ) : ℝ :=
  if z < c then (z - a) / (c - a)
  else if c < z then (b - z) / (b - c)
  else 1

/-- The joint likelihood on the support, with held endpoints. -/
def likelihood (a b : ℝ) (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  (2 / (b - a)) ^ n * ∏ i, factor a b c (x i)

/-- The log-likelihood on modes with positive likelihood. -/
def logLik (a b : ℝ) (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  (n : ℝ) * log (2 / (b - a)) + ∑ i, log (factor a b c (x i))

def orderStat (x : Fin n → ℝ) (r : Fin n) : ℝ := x (Tuple.sort x r)

private theorem factor_left {c z : ℝ} (hz : a < z) (h : z ≤ c) :
    factor a b c z = (z - a) / (c - a) := by
  rcases h.lt_or_eq with hlt | rfl
  · simp [factor, hlt]
  · simp [factor, sub_ne_zero.mpr hz.ne']

private theorem factor_right {c z : ℝ} (hz : z < b) (h : c ≤ z) :
    factor a b c z = (b - z) / (b - c) := by
  rcases h.lt_or_eq with hlt | rfl
  · simp [factor, hlt, hlt.not_gt]
  · simp [factor, sub_ne_zero.mpr hz.ne']

private theorem factor_pos {c z : ℝ} (hz : z ∈ Icc a b)
    (ha : z = a → c = a) (hb : z = b → c = b) : 0 < factor a b c z := by
  unfold factor
  split_ifs with hl hr
  · have hza : a < z := by
      refine hz.1.lt_of_ne fun e ↦ hl.ne ?_
      rw [← e, ha e.symm]
    exact div_pos (sub_pos.mpr hza) (sub_pos.mpr (hza.trans hl))
  · have hzb : z < b := by
      refine hz.2.lt_of_ne fun e ↦ hr.ne ?_
      exact (hb e).trans e.symm
    exact div_pos (sub_pos.mpr hzb) (sub_pos.mpr (hr.trans hzb))
  · norm_num

private theorem likelihood_pos {x : Fin n → ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Icc a b) {c : ℝ}
    (ha : ∀ i, x i = a → c = a) (hb : ∀ i, x i = b → c = b) :
    0 < likelihood a b x c :=
  mul_pos (pow_pos (div_pos (by norm_num) (sub_pos.mpr hab)) _)
    (Finset.prod_pos fun i _ ↦ factor_pos (hx i) (ha i) (hb i))

private theorem likelihood_zero_left {x : Fin n → ℝ} {c : ℝ}
    (h : a < c) (ha : ∃ i, x i = a) : likelihood a b x c = 0 := by
  obtain ⟨i, hi⟩ := ha
  have hz : factor a b c (x i) = 0 := by simp [hi, factor, h]
  unfold likelihood
  rw [Finset.prod_eq_zero (Finset.mem_univ i) hz, mul_zero]

private theorem likelihood_zero_right {x : Fin n → ℝ} {c : ℝ}
    (h : c < b) (hb : ∃ i, x i = b) : likelihood a b x c = 0 := by
  obtain ⟨i, hi⟩ := hb
  have hz : factor a b c (x i) = 0 := by simp [hi, factor, h, h.not_gt]
  unfold likelihood
  rw [Finset.prod_eq_zero (Finset.mem_univ i) hz, mul_zero]

/-- A mode has positive likelihood exactly when every observed endpoint equals
that mode. In particular, the two endpoints cannot both occur in a positive fit. -/
theorem likelihood_pos_iff {x : Fin n → ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Icc a b) {c : ℝ} (hc : c ∈ Icc a b) :
    0 < likelihood a b x c ↔
      (∀ i, x i = a → c = a) ∧ (∀ i, x i = b → c = b) := by
  constructor
  · intro hp
    constructor
    · intro i hi
      by_contra h
      rw [likelihood_zero_left (hc.1.lt_of_ne (fun e ↦ h e.symm)) ⟨i, hi⟩] at hp
      exact (lt_irrefl 0) hp
    · intro i hi
      by_contra h
      rw [likelihood_zero_right (hc.2.lt_of_ne h) ⟨i, hi⟩] at hp
      exact (lt_irrefl 0) hp
  · rintro ⟨ha, hb⟩
    exact likelihood_pos hab hx ha hb

private theorem interior_factor_pos {z : ℝ} (hz : z ∈ Ioo a b) (c : ℝ) :
    0 < factor a b c z :=
  factor_pos ⟨hz.1.le, hz.2.le⟩ (fun h ↦ (hz.1.ne' h).elim) (fun h ↦ (hz.2.ne h).elim)

private theorem log_likelihood {x : Fin n → ℝ} (hab : a < b) {c : ℝ}
    (hp : ∀ i, 0 < factor a b c (x i)) :
    log (likelihood a b x c) = logLik a b x c := by
  have hk : 0 < 2 / (b - a) := div_pos (by norm_num) (sub_pos.mpr hab)
  rw [likelihood, log_mul (pow_pos hk _).ne' (Finset.prod_pos fun i _ ↦ hp i).ne',
    log_pow, Real.log_prod (fun i _ ↦ (hp i).ne')]
  rfl

private theorem log_affine_strict {p q s t u v : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hne : p ≠ q) (hs : 0 < s) (ht : 0 < t) (hst : s + t = 1)
    (he : u = s * p + t * q) :
    s * (v - log p) + t * (v - log q) > v - log u := by
  have h := strictConcaveOn_log_Ioi.2 (show p ∈ Ioi 0 from hp)
    (show q ∈ Ioi 0 from hq) hne hs ht hst
  simp only [smul_eq_mul] at h
  rw [he]
  nlinarith only [h, congrArg (fun w : ℝ ↦ w * v) hst]

private theorem log_factor_strictConvex {z l u : ℝ} (hz : z ∈ Ioo a b)
    (hgap : z ≤ l ∨ u ≤ z) :
    StrictConvexOn ℝ (Icc l u) (fun c ↦ log (factor a b c z)) := by
  refine ⟨convex_Icc _ _, ?_⟩
  intro p hp q hq hpq s t hs ht hst
  change log (factor a b (s • p + t • q) z) <
    s • log (factor a b p z) + t • log (factor a b q z)
  have hm := (convex_Icc l u) hp hq hs.le ht.le hst
  rcases hgap with hl | hu
  · have hform : ∀ c ∈ Icc l u, log (factor a b c z) = log (z - a) - log (c - a) := by
      intro c hc
      have hzc := hl.trans hc.1
      rw [factor_left hz.1 hzc, log_div (sub_pos.mpr hz.1).ne'
        (sub_pos.mpr (hz.1.trans_le hzc)).ne']
    rw [hform _ hm, hform _ hp, hform _ hq]
    simp only [smul_eq_mul]
    exact log_affine_strict (sub_pos.mpr (hz.1.trans_le (hl.trans hp.1)))
      (sub_pos.mpr (hz.1.trans_le (hl.trans hq.1))) (by intro he; apply hpq; linarith)
      hs ht hst (by nlinarith only [congrArg (fun w : ℝ ↦ w * a) hst])
  · have hform : ∀ c ∈ Icc l u, log (factor a b c z) = log (b - z) - log (b - c) := by
      intro c hc
      have hcz := hc.2.trans hu
      rw [factor_right hz.2 hcz, log_div (sub_pos.mpr hz.2).ne'
        (sub_pos.mpr (hcz.trans_lt hz.2)).ne']
    rw [hform _ hm, hform _ hp, hform _ hq]
    simp only [smul_eq_mul]
    exact log_affine_strict (sub_pos.mpr ((hp.2.trans hu).trans_lt hz.2))
      (sub_pos.mpr ((hq.2.trans hu).trans_lt hz.2)) (by intro he; apply hpq; linarith)
      hs ht hst (by nlinarith only [congrArg (fun w : ℝ ↦ w * b) hst])

/-- A closed interval with no observations in its interior is a strict convexity
interval for the log-likelihood when all observations are interior to the support. -/
theorem logLik_strictConvexOn_gap (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, x i ∈ Ioo a b) {l u : ℝ} (hgap : ∀ i, x i ≤ l ∨ u ≤ x i) :
    StrictConvexOn ℝ (Icc l u) (logLik a b x) := by
  refine ⟨convex_Icc _ _, ?_⟩
  intro p hp q hq hpq s t hs ht hst
  have hsum := Finset.sum_lt_sum_of_nonempty
    (show (Finset.univ : Finset (Fin n)).Nonempty from ⟨⟨0, hn⟩, Finset.mem_univ _⟩)
    (fun i _ ↦ (log_factor_strictConvex (hx i) (hgap i)).2 hp hq hpq hs ht hst)
  simp only [smul_eq_mul, Finset.sum_add_distrib, ← Finset.mul_sum] at hsum
  simp only [logLik, smul_eq_mul]
  nlinarith only [hsum, congrArg (fun w : ℝ ↦ w * ((n : ℝ) * log (2 / (b - a)))) hst]


private theorem logLik_lt_iff {x : Fin n → ℝ} (hab : a < b)
    (hx : ∀ i, x i ∈ Ioo a b) {c d : ℝ} :
    logLik a b x c < logLik a b x d ↔ likelihood a b x c < likelihood a b x d := by
  have hp (v : ℝ) (i : Fin n) : 0 < factor a b v (x i) := interior_factor_pos (hx i) v
  have hL (v : ℝ) : 0 < likelihood a b x v :=
    mul_pos (pow_pos (div_pos (by norm_num) (sub_pos.mpr hab)) _)
      (Finset.prod_pos fun i _ ↦ hp v i)
  rw [← log_likelihood hab (hp c), ← log_likelihood hab (hp d)]
  exact Real.log_lt_log_iff (hL c) (hL d)

private theorem above_gap {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Ioo a b) {c : ℝ} (hc : ∀ i, x i ≠ c) :
    ∃ i, likelihood a b x c < likelihood a b x (x i) := by
  classical
  have hn' : (Finset.univ : Finset (Fin n)).Nonempty := ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  let left := Finset.univ.filter fun i ↦ x i < c
  let right := Finset.univ.filter fun i ↦ c < x i
  by_cases hl : left.Nonempty
  · by_cases hr : right.Nonempty
    · obtain ⟨i, hi, hleft⟩ := left.exists_max_image x hl
      obtain ⟨j, hj, hright⟩ := right.exists_min_image x hr
      have hic : x i < c := (Finset.mem_filter.mp hi).2
      have hcj : c < x j := (Finset.mem_filter.mp hj).2
      have hgap (k : Fin n) : x k ≤ x i ∨ x j ≤ x k := by
        rcases lt_or_gt_of_ne (hc k) with hk | hk
        · exact Or.inl (hleft k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
        · exact Or.inr (hright k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
      have hij := hic.trans hcj
      have hseg : c ∈ openSegment ℝ (x i) (x j) := by
        rw [openSegment_eq_Ioo hij]
        exact ⟨hic, hcj⟩
      have hlt := (logLik_strictConvexOn_gap hn hx hgap).lt_on_openSegment
        (left_mem_Icc.mpr hij.le) (right_mem_Icc.mpr hij.le) hij.ne hseg
      rcases lt_max_iff.mp hlt with h | h
      · exact ⟨i, (logLik_lt_iff hab hx).mp h⟩
      · exact ⟨j, (logLik_lt_iff hab hx).mp h⟩
    · obtain ⟨i, _, hi⟩ := Finset.univ.exists_max_image x hn'
      have hic : x i < c := by
        apply lt_of_le_of_ne _ (hc i)
        exact le_of_not_gt fun h ↦ hr ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, h⟩⟩
      refine ⟨i, ?_⟩
      unfold likelihood
      apply mul_lt_mul_of_pos_left _ (pow_pos (div_pos (by norm_num) (sub_pos.mpr hab)) _)
      apply Finset.prod_lt_prod_of_nonempty₀ _ _ hn'
      · exact fun k _ ↦ interior_factor_pos (hx k) c
      · intro k hk
        have hki := hi k hk
        rw [factor_left (hx k).1 (hki.trans hic.le), factor_left (hx k).1 hki]
        exact div_lt_div_of_pos_left (sub_pos.mpr (hx k).1) (sub_pos.mpr (hx i).1)
          (sub_lt_sub_right hic _)
  · obtain ⟨i, _, hi⟩ := Finset.univ.exists_min_image x hn'
    have hci : c < x i := by
      apply lt_of_le_of_ne _ (hc i).symm
      exact le_of_not_gt fun h ↦ hl ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, h⟩⟩
    refine ⟨i, ?_⟩
    unfold likelihood
    apply mul_lt_mul_of_pos_left _ (pow_pos (div_pos (by norm_num) (sub_pos.mpr hab)) _)
    apply Finset.prod_lt_prod_of_nonempty₀ _ _ hn'
    · exact fun k _ ↦ interior_factor_pos (hx k) c
    · intro k hk
      have hik := hi k hk
      rw [factor_right (hx k).2 (hci.le.trans hik), factor_right (hx k).2 hik]
      exact div_lt_div_of_pos_left (sub_pos.mpr (hx k).2) (sub_pos.mpr (hx i).2)
        (sub_lt_sub_left hci _)

private theorem improves_off_sample {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) (hcompatible : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)))
    {c : ℝ} (hc : c ∈ Icc a b) (hne : ∀ i, x i ≠ c) :
    ∃ i, likelihood a b x c < likelihood a b x (x i) := by
  by_cases ha : ∃ i, x i = a
  · obtain ⟨i, hi⟩ := ha
    have hca : a < c := hc.1.lt_of_ne (hi.symm.trans_ne (hne i))
    refine ⟨i, ?_⟩
    rw [likelihood_zero_left hca ⟨i, hi⟩, hi]
    exact likelihood_pos hab hx (fun _ _ ↦ rfl)
      (fun j hj ↦ (hcompatible ⟨⟨i, hi⟩, ⟨j, hj⟩⟩).elim)
  · by_cases hb : ∃ i, x i = b
    · obtain ⟨i, hi⟩ := hb
      have hcb : c < b := hc.2.lt_of_ne (fun e ↦ hne i (hi.trans e.symm))
      refine ⟨i, ?_⟩
      rw [likelihood_zero_right hcb ⟨i, hi⟩, hi]
      exact likelihood_pos hab hx (fun j hj ↦ (ha ⟨j, hj⟩).elim) (fun _ _ ↦ rfl)
    · exact above_gap hab hn
        (fun i ↦ ⟨(hx i).1.lt_of_ne fun h ↦ ha ⟨i, h.symm⟩,
          (hx i).2.lt_of_ne fun h ↦ hb ⟨i, h⟩⟩) hne

/-- If both endpoints are observed, every feasible mode has zero likelihood. -/
theorem likelihood_zero_of_both_endpoints {x : Fin n → ℝ} (hab : a < b)
    (ha : ∃ i, x i = a) (hb : ∃ i, x i = b) (c : ℝ) :
    likelihood a b x c = 0 := by
  rcases lt_or_ge a c with hc | hc
  · exact likelihood_zero_left hc ha
  · exact likelihood_zero_right (hc.trans_lt hab) hb

/-- For compatible data, the global maximizers are exactly the observations
whose likelihood is largest among the observations. -/
theorem isMaxOn_iff_observation {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) (hcompatible : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)))
    {c : ℝ} (hc : c ∈ Icc a b) :
    IsMaxOn (likelihood a b x) (Icc a b) c ↔
      (∃ i, x i = c) ∧ ∀ i, likelihood a b x (x i) ≤ likelihood a b x c := by
  classical
  constructor
  · intro hm
    refine ⟨?_, fun i ↦ hm (hx i)⟩
    by_contra h
    obtain ⟨i, hi⟩ := improves_off_sample hab hn hx hcompatible hc
      (fun i hi ↦ h ⟨i, hi⟩)
    exact hi.not_ge (hm (hx i))
  · rintro ⟨_, hm⟩ d hd
    change likelihood a b x d ≤ likelihood a b x c
    by_cases h : ∃ i, x i = d
    · obtain ⟨i, rfl⟩ := h
      exact hm i
    · obtain ⟨i, hi⟩ := improves_off_sample hab hn hx hcompatible hd
        (fun i hi ↦ h ⟨i, hi⟩)
      exact hi.le.trans (hm i)

/-- A maximizer over the closed mode interval always exists. -/
theorem exists_isMaxOn {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) :
    ∃ c ∈ Icc a b, IsMaxOn (likelihood a b x) (Icc a b) c := by
  classical
  by_cases h : (∃ i, x i = a) ∧ (∃ i, x i = b)
  · refine ⟨a, left_mem_Icc.mpr hab.le, ?_⟩
    intro c _
    change likelihood a b x c ≤ likelihood a b x a
    rw [likelihood_zero_of_both_endpoints hab h.1 h.2 c,
      likelihood_zero_of_both_endpoints hab h.1 h.2 a]
  · obtain ⟨i, _, hi⟩ := Finset.univ.exists_max_image (fun i ↦ likelihood a b x (x i))
      (show (Finset.univ : Finset (Fin n)).Nonempty from ⟨⟨0, hn⟩, Finset.mem_univ _⟩)
    exact ⟨x i, hx i, (isMaxOn_iff_observation hab hn hx h (hx i)).mpr
      ⟨⟨i, rfl⟩, fun j ↦ hi j (Finset.mem_univ _)⟩⟩

/-- Normalized likelihood at rank `r`, with the tied factors at the mode
(including repeated endpoint observations) equal to one. -/
def rankWeight (a b : ℝ) (x : Fin n → ℝ) (r : Fin n) : ℝ :=
  ∏ i, factor a b (orderStat x r) (orderStat x i)

private theorem likelihood_eq_rankWeight (x : Fin n → ℝ) (r : Fin n) :
    likelihood a b x (orderStat x r) = (2 / (b - a)) ^ n * rankWeight a b x r := by
  unfold likelihood rankWeight
  rw [show (∏ i, factor a b (orderStat x r) (orderStat x i)) =
    ∏ i, factor a b (orderStat x r) (x i) from
      Equiv.prod_comp (Tuple.sort x) (fun i ↦ factor a b (orderStat x r) (x i))]

/-- At an interior candidate the normalized likelihood is the classical two
products over lower and higher ranks. Tied observations contribute one. -/
theorem rankWeight_eq_products {x : Fin n → ℝ} (r : Fin n)
    (hr : orderStat x r ∈ Ioo a b) :
    rankWeight a b x r =
      (∏ i ∈ Finset.univ.filter (· < r), (orderStat x i - a) / (orderStat x r - a)) *
      (∏ i ∈ Finset.univ.filter (r < ·), (b - orderStat x i) / (b - orderStat x r)) := by
  let f := fun i ↦ factor a b (orderStat x r) (orderStat x i)
  have hm : Monotone (orderStat x) := Tuple.monotone_sort x
  have hsplit := Finset.prod_filter_mul_prod_filter_not Finset.univ (· < r) f
  change (∏ i, f i) = _
  rw [← hsplit]
  congr 1
  · refine Finset.prod_congr rfl fun i hi ↦ ?_
    have hle := hm (Finset.mem_filter.mp hi).2.le
    rcases hle.lt_or_eq with h | h
    · simp [f, factor, h]
    · simp [f, factor, h, sub_ne_zero.mpr hr.1.ne']
  · have hremove :
        (∏ i ∈ (Finset.univ.filter fun i ↦ ¬ i < r).filter (r < ·), f i) =
          ∏ i ∈ Finset.univ.filter (fun i ↦ ¬ i < r), f i :=
      Finset.prod_filter_of_ne fun i hi hf ↦ by
        have hri := le_of_not_gt (Finset.mem_filter.mp hi).2
        refine hri.lt_of_ne ?_
        rintro rfl
        exact hf (by simp [f, factor])
    rw [← hremove]
    refine Finset.prod_congr (by
      ext i
      simp only [not_lt, Finset.mem_filter, Finset.mem_univ, true_and, and_iff_right_iff_imp]
      exact fun h ↦ h.le) fun i hi ↦ ?_
    have hle := hm (Finset.mem_filter.mp hi).2.le
    rcases hle.lt_or_eq with h | h
    · simp [f, factor, h, h.not_gt]
    · simp [f, factor, ← h, sub_ne_zero.mpr hr.2.ne']

/-- A compatible sample's maximizers are precisely the order statistics
whose rank weights are largest. -/
theorem isMaxOn_iff_orderStat {x : Fin n → ℝ} (hab : a < b) (hn : 0 < n)
    (hx : ∀ i, x i ∈ Icc a b) (hcompatible : ¬ ((∃ i, x i = a) ∧ (∃ i, x i = b)))
    {c : ℝ} (hc : c ∈ Icc a b) :
    IsMaxOn (likelihood a b x) (Icc a b) c ↔
      ∃ r, orderStat x r = c ∧ ∀ s, rankWeight a b x s ≤ rankWeight a b x r := by
  rw [isMaxOn_iff_observation hab hn hx hcompatible hc]
  have hscale : 0 < (2 / (b - a)) ^ n :=
    pow_pos (div_pos (by norm_num) (sub_pos.mpr hab)) _
  constructor
  · rintro ⟨⟨i, hi⟩, hm⟩
    obtain ⟨r, hr⟩ := (Tuple.sort x).surjective i
    have he : orderStat x r = c := by simpa [orderStat, hr] using hi
    refine ⟨r, he, fun s ↦ ?_⟩
    have h : likelihood a b x (orderStat x s) ≤ likelihood a b x (orderStat x r) :=
      he.symm ▸ hm (Tuple.sort x s)
    rwa [likelihood_eq_rankWeight, likelihood_eq_rankWeight,
      mul_le_mul_iff_of_pos_left hscale] at h
  · rintro ⟨r, hr, hm⟩
    refine ⟨⟨Tuple.sort x r, hr⟩, fun i ↦ ?_⟩
    obtain ⟨s, hs⟩ := (Tuple.sort x).surjective i
    have he : orderStat x s = x i := by simp [orderStat, hs]
    rw [← he, ← hr, likelihood_eq_rankWeight, likelihood_eq_rankWeight]
    exact mul_le_mul_of_nonneg_left (hm s) hscale.le

/-- On `[0,1]`, the sample `(1/4, 3/4)` has exactly two maximizing modes. -/
theorem rational_tie :
    {c ∈ Icc (0 : ℝ) 1 | IsMaxOn (likelihood 0 1 ![1/4, 3/4]) (Icc 0 1) c} =
      ({1/4, 3/4} : Set ℝ) := by
  let x : Fin 2 → ℝ := ![1/4, 3/4]
  have hx (i : Fin 2) : x i ∈ Ioo (0 : ℝ) 1 := by
    fin_cases i <;> norm_num [x, mem_Ioo, Matrix.cons_val_zero, Matrix.cons_val_one]
  have hf (i : Fin 2) : likelihood 0 1 x (x i) = 4 / 3 := by
    fin_cases i <;> norm_num [x, likelihood, Fin.prod_univ_two, factor,
      Matrix.cons_val_zero, Matrix.cons_val_one]
  ext c
  change c ∈ Icc (0 : ℝ) 1 ∧ IsMaxOn (likelihood 0 1 x) (Icc 0 1) c ↔ _
  constructor
  · rintro ⟨hc, hm⟩
    obtain ⟨⟨i, rfl⟩, _⟩ := (isMaxOn_iff_observation (by norm_num) (by decide)
      (fun i ↦ ⟨(hx i).1.le, (hx i).2.le⟩)
      (fun h ↦ h.1.elim fun j hj ↦ (hx j).1.ne' hj) hc).mp hm
    fin_cases i <;> simp [x, Matrix.cons_val_zero, Matrix.cons_val_one]
  · intro hc
    have he : ∃ i, x i = c := by
      simp only [mem_insert_iff, mem_singleton_iff] at hc
      rcases hc with rfl | rfl
      · exact ⟨0, rfl⟩
      · exact ⟨1, rfl⟩
    obtain ⟨i, rfl⟩ := he
    have hc' : x i ∈ Icc (0 : ℝ) 1 := ⟨(hx i).1.le, (hx i).2.le⟩
    refine ⟨hc', (isMaxOn_iff_observation (by norm_num) (by decide)
      (fun j ↦ ⟨(hx j).1.le, (hx j).2.le⟩)
      (fun h ↦ h.1.elim fun j hj ↦ (hx j).1.ne' hj) hc').mpr ⟨⟨i, rfl⟩, ?_⟩⟩
    intro j
    rw [hf i, hf j]

end UnivariateML.Triangular

end
