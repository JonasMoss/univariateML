module

public import UnivariateML.Generic.Truncation
public import Mathlib.Analysis.Analytic.Binomial
public import Mathlib.Analysis.Normed.Group.Tannery
public import Mathlib.Analysis.PSeries

/-! Lower-only count windows in log-tilt coordinates.

The index starts at the first retained count. The objective omits positive
carrier constants, and the input mean is measured from the first count.
The finite-parameter domain in these coordinates excludes the lower atom. -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TruncCount

noncomputable section

/-- Reciprocal per-observation likelihood, up to a positive carrier factor. -/
def centeredSum (c : ℕ → ℝ) (m t : ℝ) : ℝ :=
  ∑' k : ℕ, c k * exp (t * ((k : ℝ) - m))

private theorem centered_summable {c : ℕ → ℝ} {q m : ℝ}
    (hq : Summable (fun k : ℕ ↦ c k * exp q ^ k)) :
    Summable (fun k : ℕ ↦ c k * exp (q * ((k : ℝ) - m))) := by
  convert hq.mul_right (exp (-(q*m))) using 1
  ext k
  rw [show q * ((k : ℝ) - m) = (k : ℝ) * q + -(q * m) by ring, exp_add, exp_nat_mul,
    mul_assoc]

private theorem centered_strictConvex {D : Set ℝ} (hD : Convex ℝ D)
    {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) (m : ℝ)
    (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k)) :
    StrictConvexOn ℝ D (centeredSum c m) := by
  refine ⟨hD, fun x hx y hy hxy a b ha hb hab ↦ ?_⟩
  let f (t : ℝ) (k : ℕ) := c k * exp (t * ((k : ℝ) - m))
  have H (k : ℕ) (hk : (k : ℝ) = m → False) :
      f (a • x + b • y) k < a * f x k + b * f y k := by
    have hne : x * ((k : ℝ) - m) ≠ y * ((k : ℝ) - m) :=
      fun h ↦ hxy ((mul_left_inj' (sub_ne_zero.mpr hk)).mp h)
    have hh := mul_lt_mul_of_pos_left
      (strictConvexOn_exp.2 (mem_univ _) (mem_univ _) hne ha hb hab) (hc k)
    simp only [f, smul_eq_mul] at hh ⊢
    convert hh using 1 <;> ring_nf
  have hw (k : ℕ) : f (a • x + b • y) k ≤ a * f x k + b * f y k := by
    by_cases hk : (k : ℝ) = m
    · simp [f, hk, ← add_mul, hab]
    · exact (H k hk).le
  have hxs : Summable (f x) := centered_summable (m := m) (hs x hx)
  have hys : Summable (f y) := centered_summable (m := m) (hs y hy)
  have hzs : Summable (f (a • x + b • y)) :=
    centered_summable (m := m) (hs _ (hD hx hy ha.le hb.le hab))
  obtain ⟨k, hk⟩ : ∃ k : ℕ, (k : ℝ) ≠ m := by
    by_cases h : m = 0
    · exact ⟨1, by simp [h]⟩
    · exact ⟨0, by simpa using Ne.symm h⟩
  have hj := Summable.tsum_lt_tsum hw (H k hk) hzs
    ((hxs.mul_left a).add (hys.mul_left b))
  rw [(hxs.mul_left a).tsum_add (hys.mul_left b), tsum_mul_left, tsum_mul_left] at hj
  exact hj

private theorem centered_ge_term {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {m t : ℝ}
    (hs : Summable (fun k : ℕ ↦ c k * exp t ^ k)) (k : ℕ) :
    c k * exp (t * ((k : ℝ) - m)) ≤ centeredSum c m t :=
  (centered_summable (m := m) hs).le_tsum k fun j _ ↦ (mul_pos (hc j) (exp_pos _)).le

private theorem no_lower_atom_fit {D : Set ℝ} (htail : ∀ᶠ t : ℝ in atBot, t ∈ D)
    {c : ℕ → ℝ} (hc : ∀ k, 0 < c k)
    (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    {t : ℝ} (ht : t ∈ D) : ¬ IsMinOn (centeredSum c 0) D t := by
  intro hmin
  obtain ⟨s, hsD, hst⟩ := (htail.and (eventually_lt_atBot t)).exists
  have hle (k : ℕ) : c k * exp (s * ((k : ℝ) - 0)) ≤ c k * exp (t * ((k : ℝ) - 0)) :=
    mul_le_mul_of_nonneg_left (exp_le_exp.mpr (by simp; gcongr)) (hc k).le
  have hlt : c 1 * exp (s * ((1 : ℕ) - (0 : ℝ))) <
      c 1 * exp (t * ((1 : ℕ) - (0 : ℝ))) := by
    simpa using mul_lt_mul_of_pos_left (exp_lt_exp.mpr hst) (hc 1)
  exact (Summable.tsum_lt_tsum hle hlt (centered_summable (m := 0) (hs s hsD))
    (centered_summable (m := 0) (hs t ht))).not_ge (hmin hsD)

/-- An open log-tilt interval extending to the left has a unique minimum
at a strictly positive excess mean, provided the right end has a barrier
above the value at every interior tilt. -/
theorem lower_window_fit {D : Set ℝ} (hD : Convex ℝ D) (hopen : IsOpen D)
    (htail : ∀ᶠ t : ℝ in atBot, t ∈ D) {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {m : ℝ}
    (hm : 0 ≤ m) (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    (hRight : ∀ t ∈ D, ∃ u ∈ D, t < u ∧ centeredSum c m t < centeredSum c m u) :
    (∃! t, t ∈ D ∧ IsMinOn (centeredSum c m) D t) ↔ 0 < m := by
  have H := centered_strictConvex hD hc m hs
  constructor
  · rintro ⟨t, ⟨ht, hmin⟩, _⟩
    by_contra! h
    have he : m = 0 := by linarith
    subst m
    exact no_lower_atom_fit htail hc hs ht hmin
  · intro hm'
    obtain ⟨t, ht⟩ := htail.exists
    obtain ⟨u, hu, htu, hru⟩ := hRight t ht
    have hleft : Tendsto (fun s : ℝ ↦ c 0 * exp (s * (-m))) atBot atTop :=
      Tendsto.const_mul_atTop (hc 0) (tendsto_exp_atTop.comp
        (Tendsto.atBot_mul_const_of_neg (neg_lt_zero.mpr hm') (f := id) tendsto_id))
    obtain ⟨l, hl, hlt, hlow⟩ := (htail.and ((eventually_lt_atBot t).and
      (hleft.eventually (eventually_gt_atTop (centeredSum c m t))))).exists
    have hlower : centeredSum c m t < centeredSum c m l := hlow.trans_le (by
      simpa using centered_ge_term hc (m := m) (hs l hl) 0)
    have hsub : Icc l u ⊆ D := by
      rw [← segment_eq_Icc (hlt.trans htu).le]
      exact hD.segment_subset hl hu
    obtain ⟨s, hsI, hmin⟩ := isCompact_Icc.exists_isMinOn ⟨t, hlt.le, htu.le⟩
      ((H.convexOn.continuousOn hopen).mono hsub)
    have hst : centeredSum c m s ≤ centeredSum c m t := hmin ⟨hlt.le, htu.le⟩
    have hglobal : IsMinOn (centeredSum c m) D s := by
      intro v hv
      change centeredSum c m s ≤ centeredSum c m v
      by_cases hvi : v ∈ Icc l u
      · exact hmin hvi
      · by_contra! hvs
        by_cases hvl : v < l
        · have hbound := H.convexOn.le_max_of_mem_segment hv ht
            (show l ∈ segment ℝ v t by rw [segment_eq_Icc (hvl.trans hlt).le]; exact ⟨hvl.le, hlt.le⟩)
          exact hlower.not_ge (hbound.trans (max_le (hvs.le.trans hst) le_rfl))
        · have huv : u < v := by
            by_contra! h
            exact hvi ⟨le_of_not_gt hvl, h⟩
          have hbound := H.convexOn.le_max_of_mem_segment ht hv
            (show u ∈ segment ℝ t v by rw [segment_eq_Icc (htu.trans huv).le]; exact ⟨htu.le, huv.le⟩)
          exact hru.not_ge (hbound.trans (max_le le_rfl (hvs.le.trans hst)))
    exact ⟨s, ⟨hsub hsI, hglobal⟩, fun v hv ↦
      H.eq_of_isMinOn hv.2 hglobal hv.1 (hsub hsI)⟩

/-- At the lower sample mean the conditional distribution approaches the
first retained atom. This limit holds for each fixed retained count. -/
theorem lower_atom {c : ℕ → ℝ} (hc : ∀ k, 0 < c k) {b : ℝ}
    (hs : Summable (fun k : ℕ ↦ c k * exp b ^ k)) (j : ℕ) :
    Tendsto (fun t : ℝ ↦ c j * exp t ^ j / (∑' k : ℕ, c k * exp t ^ k))
      atBot (𝓝 (if j = 0 then 1 else 0)) := by
  have hterm (k : ℕ) : Tendsto (fun t : ℝ ↦ c k * exp t ^ k) atBot
      (𝓝 (if k = 0 then c 0 else 0)) := by
    by_cases hk : k = 0
    · subst k
      simp
    · simpa [hk, zero_pow hk] using (tendsto_exp_atBot.pow k).const_mul (c k)
  have hsum : Tendsto (fun t : ℝ ↦ ∑' k : ℕ, c k * exp t ^ k) atBot (𝓝 (c 0)) := by
    have H := tendsto_tsum_of_dominated_convergence hs hterm
      ((eventually_le_atBot b).mono fun t ht k ↦ by
        rw [Real.norm_of_nonneg (mul_nonneg (hc k).le (by positivity))]
        exact mul_le_mul_of_nonneg_left
          (pow_le_pow_left₀ (exp_pos _).le (exp_le_exp.mpr ht) _) (hc k).le)
    simpa only [tsum_ite_eq] using H
  have h := (hterm j).div hsum (hc 0).ne'
  split_ifs with hj
  · subst j
    simpa [Pi.div_def, div_self (hc 0).ne'] using h
  · simpa [Pi.div_def, hj] using h

private theorem upper_barrier {c : ℕ → ℝ} (hc : ∀ k, 0 < c k)
    (hs : ∀ t < (0 : ℝ), Summable (fun k : ℕ ↦ c k * exp t ^ k))
    (hd : ¬ Summable c) (m : ℝ) {t : ℝ} (ht : t < 0) :
    ∃ u < 0, t < u ∧ centeredSum c m t < centeredSum c m u := by
  have hsum : ∃ n, centeredSum c m t < ∑ k ∈ Finset.range n, c k := by
    by_contra! h
    exact hd (summable_of_sum_range_le (fun k ↦ (hc k).le) h)
  obtain ⟨n, hn⟩ := hsum
  have hlim : Continuous (fun u : ℝ ↦
      ∑ k ∈ Finset.range n, c k * exp (u * ((k : ℝ) - m))) := by fun_prop
  have hNear : ∀ᶠ u : ℝ in 𝓝[<] 0,
      centeredSum c m t < ∑ k ∈ Finset.range n, c k * exp (u * ((k : ℝ) - m)) := by
    apply (hlim.continuousAt.tendsto.mono_left nhdsWithin_le_nhds).eventually
    apply Ioi_mem_nhds
    simpa using hn
  have hu₁ : ∀ᶠ u : ℝ in 𝓝[<] 0, u < 0 := self_mem_nhdsWithin
  have hu₂ : ∀ᶠ u : ℝ in 𝓝[<] 0, t < u := mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds ht)
  obtain ⟨u, hu, htu, hlt⟩ := (hu₁.and (hu₂.and hNear)).exists
  refine ⟨u, hu, htu, hlt.trans_le ?_⟩
  exact (centered_summable (m := m) (hs u hu)).sum_le_tsum _ fun k _ ↦
    (mul_pos (hc k) (exp_pos _)).le

def poissonCarrier (a k : ℕ) : ℝ := 1 / ((a + k).factorial : ℝ)

private theorem poisson_summable (a : ℕ) (t : ℝ) :
    Summable (fun k : ℕ ↦ poissonCarrier a k * exp t ^ k) := by
  have h := (summable_nat_add_iff a).mpr (Real.summable_pow_div_factorial (exp t))
  convert h.mul_left ((exp t ^ a)⁻¹) using 1
  ext k
  simp [poissonCarrier, pow_add, Nat.add_comm k a, div_eq_mul_inv, mul_comm]
  field_simp

/-- On a Poisson lower-only window with its lower atom excluded, a unique
rate fits exactly when the mean exceeds the retained lower bound. -/
theorem poisson_lower_fit (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, IsMinOn (centeredSum (poissonCarrier a) (m - a)) univ t) ↔ (a : ℝ) < m := by
  have hc : ∀ k, 0 < poissonCarrier a k := fun k ↦ one_div_pos.mpr (by positivity)
  have hRight : ∀ t ∈ (univ : Set ℝ), ∃ u ∈ (univ : Set ℝ), t < u ∧
      centeredSum (poissonCarrier a) (m - a) t < centeredSum (poissonCarrier a) (m - a) u := by
    intro t _
    obtain ⟨k, hk⟩ := exists_nat_gt (m - a)
    have hlim : Tendsto (fun u : ℝ ↦ poissonCarrier a k * exp (u * ((k : ℝ) - (m - a))))
        atTop atTop := Tendsto.const_mul_atTop (hc k) (tendsto_exp_atTop.comp
          (Tendsto.atTop_mul_const (sub_pos.mpr hk) (f := id) tendsto_id))
    obtain ⟨u, htu, hlt⟩ := ((eventually_gt_atTop t).and
      (hlim.eventually (eventually_gt_atTop (centeredSum (poissonCarrier a) (m - a) t)))).exists
    exact ⟨u, mem_univ _, htu, hlt.trans_le (centered_ge_term hc (poisson_summable a u) k)⟩
  simpa only [mem_univ, true_and, sub_pos] using lower_window_fit convex_univ isOpen_univ
    (Eventually.of_forall (fun _ ↦ mem_univ _)) hc (sub_nonneg.mpr hm)
    (fun t _ ↦ poisson_summable a t) hRight

/-- The Poisson lower-only window approaches the first retained atom as
rate tends to zero in log-rate coordinates. -/
theorem poisson_lower_atom (a j : ℕ) :
    Tendsto (fun t : ℝ ↦ poissonCarrier a j * exp t ^ j /
      (∑' k : ℕ, poissonCarrier a k * exp t ^ k)) atBot (𝓝 (if j = 0 then 1 else 0)) :=
  lower_atom (fun _ ↦ one_div_pos.mpr (by positivity)) (poisson_summable a 0) j

private theorem geom_summable {t : ℝ} (ht : t < 0) :
    Summable (fun k : ℕ ↦ (1 : ℝ) * exp t ^ k) := by
  simpa using summable_geometric_of_lt_one (exp_pos t).le (exp_lt_one_iff.mpr ht)

/-- On a geometric lower-only window with the lower atom excluded, a
unique probability fits exactly when the mean exceeds the lower cut. -/
theorem geom_lower_fit (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, t < 0 ∧ IsMinOn (centeredSum (fun _ ↦ 1) (m - a)) (Iio 0) t) ↔
      (a : ℝ) < m := by
  have hd : ¬ Summable (fun _ : ℕ ↦ (1 : ℝ)) := by
    rw [summable_const_iff]
    simp
  simpa only [mem_Iio, sub_pos] using lower_window_fit (convex_Iio 0) isOpen_Iio
    (eventually_lt_atBot 0) (fun _ ↦ one_pos) (sub_nonneg.mpr hm)
    (fun _ h ↦ geom_summable h) (fun _ h ↦ upper_barrier (fun _ ↦ one_pos) (fun _ ↦ geom_summable) hd _ h)

/-- The geometric lower-only window approaches the first retained atom. -/
theorem geom_lower_atom (j : ℕ) :
    Tendsto (fun t : ℝ ↦ exp t ^ j / (∑' k : ℕ, exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0)) := by
  simpa only [one_mul] using lower_atom (c := fun _ ↦ 1) (fun _ ↦ one_pos)
    (geom_summable (show (-1 : ℝ) < 0 by norm_num)) j

def negBinomCarrier (r : ℝ) (k : ℕ) : ℝ := Ring.choose (r + k - 1) k

private theorem nb_pos {r : ℝ} (hr : 0 < r) (k : ℕ) : 0 < negBinomCarrier r k := by
  unfold negBinomCarrier
  have H := Ring.descPochhammer_eq_factorial_smul_choose (r + (k : ℝ) - 1) k
  rw [nsmul_eq_mul] at H
  have hp : 0 < (descPochhammer ℤ k).smeval (r + (k : ℝ) - 1) := by
    rw [Polynomial.descPochhammer_smeval_eq_ascPochhammer,
      Polynomial.ascPochhammer_smeval_cast, Polynomial.ascPochhammer_smeval_eq_eval,
      show r + (k : ℝ) - 1 - k + 1 = r by ring]
    exact ascPochhammer_pos k r hr
  exact pos_of_mul_pos_right (H ▸ hp) (by positivity)

private theorem nb_hasSum (r : ℝ) {q : ℝ} (hq : q ∈ Ioo (0 : ℝ) 1) :
    HasSum (fun k : ℕ ↦ negBinomCarrier r k * q ^ k) (1 / (1 - q) ^ r) := by
  have hball : q ∈ Metric.eball (0 : ℝ) 1 := by
    rw [← ENNReal.ofReal_one, Metric.eball_ofReal, Metric.mem_ball, Real.dist_eq, sub_zero,
      abs_of_pos hq.1]
    exact hq.2
  simpa [negBinomCarrier, FormalMultilinearSeries.ofScalars, smul_eq_mul] using
    (Real.one_div_one_sub_rpow_hasFPowerSeriesOnBall_zero r).hasSum_sub hball

private theorem nb_summable (a : ℕ) (r : ℝ) {t : ℝ} (ht : t < 0) :
    Summable (fun k : ℕ ↦ negBinomCarrier r (a+k) * exp t ^ k) := by
  have h := (summable_nat_add_iff a).mpr
    (nb_hasSum r ⟨exp_pos t, exp_lt_one_iff.mpr ht⟩).summable
  convert h.mul_left ((exp t ^ a)⁻¹) using 1
  ext k
  rw [Nat.add_comm k a, pow_add]
  field_simp

private theorem nb_not_summable {r : ℝ} (hr : 0 < r) (a : ℕ) :
    ¬ Summable (fun k : ℕ ↦ negBinomCarrier r (a+k)) := by
  intro h
  have hfull := (summable_nat_add_iff a).mp (by simpa [Nat.add_comm] using h)
  let C := ∑' k, negBinomCarrier r k
  have hBound (q : ℝ) (hq : q ∈ Ioo (0 : ℝ) 1) : 1 / (1 - q) ^ r ≤ C := by
    rw [← (nb_hasSum r hq).tsum_eq]
    apply (nb_hasSum r hq).summable.tsum_le_tsum _ hfull
    intro k
    conv_rhs => rw [← mul_one (negBinomCarrier r k)]
    exact mul_le_mul_of_nonneg_left (pow_le_one₀ hq.1.le hq.2.le) (nb_pos hr k).le
  have hl : Tendsto (fun q : ℝ ↦ (1 - q) ^ r) (𝓝[<] 1) (𝓝 0) := by
    have H : ContinuousAt (fun q : ℝ ↦ (1 - q) ^ r) 1 :=
      (continuousAt_const.sub continuousAt_id).rpow_const (Or.inr hr.le)
    simpa [zero_rpow hr.ne'] using H.tendsto.mono_left
      (nhdsWithin_le_nhds (s := Iio (1 : ℝ)))
  have hh : ∀ᶠ q : ℝ in 𝓝[<] 1, (1 - q) ^ r < 1 / (|C| + 1) :=
    hl.eventually (Iio_mem_nhds (by positivity))
  have hpos : ∀ᶠ q : ℝ in 𝓝[<] 1, 0 < q :=
    mem_nhdsWithin_of_mem_nhds (Ioi_mem_nhds zero_lt_one)
  have hlt : ∀ᶠ q : ℝ in 𝓝[<] 1, q < 1 := self_mem_nhdsWithin
  obtain ⟨q, hq₁, hq₂, hq⟩ := (hpos.and (hlt.and hh)).exists
  have hp : 0 < (1-q) ^ r := rpow_pos_of_pos (sub_pos.mpr hq₂) _
  have hb := (div_le_iff₀ hp).mp (hBound q ⟨hq₁,hq₂⟩)
  have ht := (lt_div_iff₀ (show 0 < |C| + 1 by positivity)).mp hq
  nlinarith [abs_nonneg C, le_abs_self C, mul_nonneg (sub_nonneg.mpr (le_abs_self C)) hp.le]

/-- At positive held size, an interior NB fit on a lower-only window
exists uniquely exactly when the mean exceeds its first retained count. -/
theorem nbinom_lower_fit {r : ℝ} (hr : 0 < r) (a : ℕ) {m : ℝ} (hm : (a : ℝ) ≤ m) :
    (∃! t, t < 0 ∧ IsMinOn (centeredSum (fun k ↦ negBinomCarrier r (a+k)) (m - a))
      (Iio 0) t) ↔ (a : ℝ) < m := by
  simpa only [mem_Iio, sub_pos] using lower_window_fit (convex_Iio 0) isOpen_Iio
    (eventually_lt_atBot 0) (fun k ↦ nb_pos hr (a+k)) (sub_nonneg.mpr hm)
    (fun _ h ↦ nb_summable a r h)
    (fun _ h ↦ upper_barrier (fun k ↦ nb_pos hr (a+k)) (fun _ h ↦ nb_summable a r h)
      (nb_not_summable hr a) _ h)

/-- Positive-size NB approaches the atom at the retained lower bound. -/
theorem nbinom_lower_atom {r : ℝ} (hr : 0 < r) (a j : ℕ) :
    Tendsto (fun t : ℝ ↦ negBinomCarrier r (a+j) * exp t ^ j /
      (∑' k : ℕ, negBinomCarrier r (a+k) * exp t ^ k)) atBot
      (𝓝 (if j = 0 then 1 else 0)) :=
  lower_atom (fun k ↦ nb_pos hr (a+k))
    (nb_summable a r (show (-1 : ℝ) < 0 by norm_num)) j

/-- All observations at the first retained count exclude a finite positive
Poisson rate if the lower atom is unavailable. -/
theorem poisson_lower_failure (a : ℕ) (t : ℝ) :
    ¬ IsMinOn (centeredSum (poissonCarrier a) 0) univ t :=
  no_lower_atom_fit (Eventually.of_forall (fun _ ↦ mem_univ _))
    (fun _ ↦ one_div_pos.mpr (by positivity)) (fun t _ ↦ poisson_summable a t) (mem_univ _)

/-- All observations at the first retained count exclude a finite interior
geometric probability if the lower atom is unavailable. -/
theorem geom_lower_failure {t : ℝ} (ht : t < 0) :
    ¬ IsMinOn (centeredSum (fun _ ↦ 1) 0) (Iio 0) t :=
  no_lower_atom_fit (eventually_lt_atBot 0) (fun _ ↦ one_pos) (fun _ ↦ geom_summable) ht

/-- All observations at the first retained count exclude a finite interior
NB probability if the lower atom is unavailable. -/
theorem nbinom_lower_failure {r : ℝ} (hr : 0 < r) (a : ℕ) {t : ℝ} (ht : t < 0) :
    ¬ IsMinOn (centeredSum (fun k ↦ negBinomCarrier r (a+k)) 0) (Iio 0) t :=
  no_lower_atom_fit (eventually_lt_atBot 0) (fun k ↦ nb_pos hr (a+k))
    (fun _ h ↦ nb_summable a r h) ht

/-- Per-observation likelihood after omitting the carrier product, with
`none` denoting the included lower atom. -/
def closedLowerLikelihood (c : ℕ → ℝ) (m : ℝ) : Option ℝ → ℝ
  | none => if m = 0 then (c 0)⁻¹ else 0
  | some t => (centeredSum c m t)⁻¹

/-- Adjoin the lower-atom parameter to an interior tilt domain. -/
def closedLowerDomain (D : Set ℝ) : Set (Option ℝ) :=
  {p | ∀ t, p = some t → t ∈ D}

private theorem closed_lower_positive {D : Set ℝ} {c : ℕ → ℝ} {m : ℝ}
    (hc : ∀ k, 0 < c k) (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    {p : Option ℝ} (hp : p ∈ closedLowerDomain D) :
    0 ≤ closedLowerLikelihood c m p := by
  cases p with
  | none =>
    unfold closedLowerLikelihood
    split_ifs
    · exact inv_nonneg.mpr (hc 0).le
    · exact le_rfl
  | some t =>
    exact inv_nonneg.mpr ((mul_pos (hc 0) (exp_pos _)).le.trans
      (centered_ge_term hc (hs t (hp t rfl)) 0))

private theorem closed_lower_fit {D : Set ℝ} {c : ℕ → ℝ}
    (hc : ∀ k, 0 < c k) {m : ℝ} (hm : 0 ≤ m)
    (hs : ∀ t ∈ D, Summable (fun k : ℕ ↦ c k * exp t ^ k))
    (hfit : (∃! t, t ∈ D ∧ IsMinOn (centeredSum c m) D t) ↔ 0 < m) :
    ∃! p, p ∈ closedLowerDomain D ∧
      IsMaxOn (closedLowerLikelihood c m) (closedLowerDomain D) p := by
  have hn : (none : Option ℝ) ∈ closedLowerDomain D := by simp [closedLowerDomain]
  have hmem (t : ℝ) : some t ∈ closedLowerDomain D ↔ t ∈ D := by
    simp [closedLowerDomain]
  have hpos (t : ℝ) (ht : t ∈ D) : 0 < centeredSum c m t :=
    (mul_pos (hc 0) (exp_pos _)).trans_le (centered_ge_term hc (hs t ht) 0)
  rcases hm.eq_or_lt with h | h
  · have hzero : m = 0 := h.symm
    have hc0 (t : ℝ) (ht : t ∈ D) : c 0 < centeredSum c m t := by
      let f (k : ℕ) := c k * exp (t * ((k : ℝ)-m))
      have h₁ : 0 < f 1 := mul_pos (hc 1) (exp_pos _)
      have H := (centered_summable (m := m) (hs t ht)).sum_le_tsum ({0,1} : Finset ℕ)
        (fun k _ ↦ (mul_pos (hc k) (exp_pos _)).le)
      have he : (∑ k ∈ ({0,1} : Finset ℕ), f k) = c 0 + f 1 := by
        simp [f, hzero]
      change (∑ k ∈ ({0,1} : Finset ℕ), f k) ≤ centeredSum c m t at H
      rw [he] at H
      linarith
    have hstrict (t : ℝ) (ht : t ∈ D) :
        closedLowerLikelihood c m (some t) < closedLowerLikelihood c m none := by
      change (centeredSum c m t)⁻¹ < (if m = 0 then (c 0)⁻¹ else 0)
      rw [ite_eq_left hzero]
      exact (inv_lt_inv₀ (hpos t ht) (hc 0)).mpr (hc0 t ht)
    refine ⟨none, ⟨hn, ?_⟩, ?_⟩
    · intro p hp
      cases p with
      | none => exact le_rfl
      | some t => exact (hstrict t ((hmem t).mp hp)).le
    · rintro (_ | t) ⟨hp,hpmax⟩
      · rfl
      · exact ((hstrict t ((hmem t).mp hp)).not_ge (hpmax hn)).elim
  · obtain ⟨t, ⟨ht, htmin⟩, huniq⟩ := hfit.mpr h
    have hnone : closedLowerLikelihood c m none = 0 := by
      simp [closedLowerLikelihood, h.ne']
    have hsome (u : ℝ) (hu : u ∈ D) :
        0 < closedLowerLikelihood c m (some u) := inv_pos.mpr (hpos u hu)
    refine ⟨some t, ⟨(hmem t).mpr ht, ?_⟩, ?_⟩
    · intro p hp
      cases p with
      | none =>
        change closedLowerLikelihood c m none ≤ closedLowerLikelihood c m (some t)
        rw [hnone]
        exact (hsome t ht).le
      | some u =>
        have hu := (hmem u).mp hp
        exact (inv_le_inv₀ (hpos u hu) (hpos t ht)).mpr (htmin hu)
    · rintro (_ | u) ⟨hp, hpmax⟩
      · have H : closedLowerLikelihood c m (some t) ≤ closedLowerLikelihood c m none :=
          hpmax ((hmem t).mpr ht)
        exact ((hsome t ht).not_ge (hnone ▸ H)).elim
      · have hu := (hmem u).mp hp
        congr 1
        refine huniq u ⟨hu, fun v hv ↦ ?_⟩
        exact (inv_le_inv₀ (hpos v hv) (hpos u hu)).mp (hpmax ((hmem v).mpr hv))

/-- The positive NB carrier at size zero is the logarithmic-series
carrier, with its count index shifted by one. -/
def logSeriesCarrier (k : ℕ) : ℝ := 1 / ((k : ℝ)+1)

private theorem logSeries_summable {t : ℝ} (ht : t < 0) :
    Summable (fun k : ℕ ↦ logSeriesCarrier k * exp t ^ k) := by
  apply (summable_geometric_of_lt_one (exp_pos t).le (exp_lt_one_iff.mpr ht)).of_nonneg_of_le
  · intro k
    exact mul_nonneg (by unfold logSeriesCarrier; positivity) (pow_nonneg (exp_pos t).le _)
  · intro k
    have hC : logSeriesCarrier k ≤ 1 := by
      unfold logSeriesCarrier
      exact (div_le_one (by positivity)).mpr (by linarith [Nat.cast_nonneg (α := ℝ) k])
    simpa only [one_mul] using mul_le_mul_of_nonneg_right hC (pow_nonneg (exp_pos t).le k)

private theorem logSeries_not_summable : ¬ Summable logSeriesCarrier := by
  intro h
  apply not_summable_one_div_natCast
  apply (summable_nat_add_iff 1).mp
  change Summable (fun n : ℕ ↦ 1 / ((n : ℝ) + 1)) at h
  simpa only [one_div, Nat.cast_add, Nat.cast_one] using h

/-- Positive conditioning for held NB size admits the lower atom.
At size zero the positive component uses the logarithmic carrier. -/
theorem nbinom_zt_fit {r : ℝ} (hr : 0 ≤ r) {m : ℝ} (hm : 1 ≤ m) :
    let c := if r = 0 then logSeriesCarrier else fun k ↦ negBinomCarrier r (1+k)
    ∃! p, p ∈ closedLowerDomain (Iio 0) ∧
      IsMaxOn (closedLowerLikelihood c (m-1)) (closedLowerDomain (Iio 0)) p := by
  dsimp only
  rcases hr.eq_or_lt with h | h
  · subst r
    rw [ite_eq_left rfl]
    have hc : ∀ k, 0 < logSeriesCarrier k := fun k ↦ one_div_pos.mpr (by positivity)
    have hS : ∀ t ∈ Iio (0 : ℝ), Summable (fun k : ℕ ↦ logSeriesCarrier k * exp t ^ k) :=
      fun _ ht ↦ logSeries_summable ht
    apply closed_lower_fit hc (sub_nonneg.mpr hm) hS
    exact lower_window_fit (convex_Iio 0) isOpen_Iio (eventually_lt_atBot 0) hc
      (sub_nonneg.mpr hm) hS
      (fun _ ht ↦ upper_barrier hc (fun _ ↦ logSeries_summable) logSeries_not_summable _ ht)
  · rw [ite_eq_right h.ne']
    apply closed_lower_fit (fun k ↦ nb_pos h (1+k)) (sub_nonneg.mpr hm)
      (fun _ ht ↦ nb_summable 1 r ht)
    simpa only [mem_Iio, sub_pos, Nat.cast_one] using nbinom_lower_fit h 1 (by simpa only [Nat.cast_one] using hm)

/-- Scalar count-window objective in the natural tilt. The mean is in
the original count coordinates. -/
def finiteLogLik (l u : ℕ) (h : ℕ → ℝ) (m : ℝ) : ℝ → ℝ :=
  Truncation.logLik (Finset.Icc l u) (fun k ↦ (k : ℝ)) h m

private theorem finite_bounds {l u : ℕ} (k : ℕ) (hk : k ∈ Finset.Icc l u) :
    (k : ℝ) ∈ Icc (l : ℝ) (u : ℝ) := by
  rw [Finset.mem_Icc] at hk
  exact ⟨by exact_mod_cast hk.1, by exact_mod_cast hk.2⟩

private theorem finite_fit_iff {l u : ℕ} (hlu : l < u) {h : ℕ → ℝ}
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) (m : ℝ) :
    (∃! t, IsMaxOn (finiteLogLik l u h m) univ t) ↔ m ∈ Ioo (l : ℝ) (u : ℝ) :=
  Truncation.scalar_existsUnique_iff hh (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩)
    (Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩) (by exact_mod_cast hlu) finite_bounds m

/-- The unique positive-rate fit on a nonsingleton bounded Poisson
window exists exactly at strictly interior means. -/
theorem poisson_bounded_fit {l u : ℕ} (hlu : l < u) (m : ℝ) :
    (∃! t, IsMaxOn (finiteLogLik l u (poissonCarrier 0) m) univ t) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) :=
  finite_fit_iff hlu (fun _ _ ↦ one_div_pos.mpr (by positivity)) m

/-- Fixed binomial size restricts the retained window to counts at most
that size. A nonsingleton window has exactly one interior probability
fit iff the mean lies strictly between its retained endpoints. -/
theorem binom_bounded_fit {l u N : ℕ} (hlu : l < u) (huN : u ≤ N) (m : ℝ) :
    (∃! t, IsMaxOn (finiteLogLik l u (fun k ↦ (N.choose k : ℝ)) m) univ t) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) :=
  finite_fit_iff hlu (fun k hk ↦ by
    exact_mod_cast (Nat.choose_pos ((Finset.mem_Icc.mp hk).2.trans huN))) m

/-- At positive held NB size the upper threshold is the finite
window's mean under its untilted carrier. -/
theorem nbinom_bounded_fit {r : ℝ} (hr : 0 < r) {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn (finiteLogLik l u (negBinomCarrier r) m) (Iio 0) t) ↔
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (negBinomCarrier r) :=
  Truncation.scalar_negative_existsUnique_iff (fun k _ ↦ nb_pos hr k)
    (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩) (Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩)
    (by exact_mod_cast hlu) finite_bounds hm

/-- The geometric upper threshold is the mean of the retained discrete
uniform distribution. This is expressed as the mean of the unit carrier. -/
theorem geom_bounded_fit {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn (finiteLogLik l u (fun _ ↦ 1) m) (Iio 0) t) ↔
      m < Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) :=
  Truncation.scalar_negative_existsUnique_iff (fun _ _ ↦ one_pos)
    (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩) (Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩)
    (by exact_mod_cast hlu) finite_bounds hm

/-- At or above the uniform-window mean every geometric tilt
is improved by approaching the excluded zero tilt. -/
theorem geom_bounded_upper_failure {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) ≤ m) :
    StrictMonoOn (finiteLogLik l u (fun _ ↦ 1) m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (finiteLogLik l u (fun _ ↦ 1) m) (Iio 0) t :=
  Truncation.scalar_negative_upper_failure (fun _ _ ↦ one_pos)
    (Finset.mem_Icc.mpr ⟨le_rfl, hlu.le⟩) (Finset.mem_Icc.mpr ⟨hlu.le, le_rfl⟩)
    (by exact_mod_cast hlu) hm

/-- At or above the normalized carrier mean, the positive-size NB
window has no finite probability maximizer. -/
theorem nbinom_bounded_upper_failure {r : ℝ} (hr : 0 < r) {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (negBinomCarrier r) ≤ m) :
    StrictMonoOn (finiteLogLik l u (negBinomCarrier r) m) (Iic 0) ∧
      ∀ t < 0, ¬ IsMaxOn (finiteLogLik l u (negBinomCarrier r) m) (Iio 0) t :=
  Truncation.scalar_negative_upper_failure (fun k _ ↦ nb_pos hr k)
    (Finset.mem_Icc.mpr ⟨le_rfl, hlu.le⟩) (Finset.mem_Icc.mpr ⟨hlu.le, le_rfl⟩)
    (by exact_mod_cast hlu) hm

/-- The conditional mass of a retained count at finite natural tilt. -/
def finiteCountMass (l u : ℕ) (h : ℕ → ℝ) (j : ℕ) (t : ℝ) : ℝ :=
  h j * exp ((j : ℝ)*t) / (∑ k ∈ Finset.Icc l u, h k * exp ((k : ℝ)*t))

private theorem finite_mass_center (l u : ℕ) (h : ℕ → ℝ) (j : ℕ) (t c : ℝ) :
    finiteCountMass l u h j t =
      h j * exp (t * ((j : ℝ)-c)) /
        (∑ k ∈ Finset.Icc l u, h k * exp (t * ((k : ℝ)-c))) := by
  have he (k : ℕ) :
      h k * exp (t * ((k : ℝ)-c)) = (h k * exp ((k : ℝ)*t)) * exp (-(t*c)) := by
    rw [show t * ((k : ℝ)-c) = (k : ℝ)*t + -(t*c) by ring, exp_add, mul_assoc]
  simp only [he, ← Finset.sum_mul, finiteCountMass]
  exact (mul_div_mul_right _ _ (exp_pos _).ne').symm

private theorem finite_atom_of_limits {l u j endpoint : ℕ} {h : ℕ → ℝ} {F : Filter ℝ}
    (he : endpoint ∈ Finset.Icc l u) (hj : j ∈ Finset.Icc l u) (hp : 0 < h endpoint)
    (hlim : ∀ k ∈ Finset.Icc l u, k ≠ endpoint →
      Tendsto (fun t : ℝ ↦ exp (t * ((k : ℝ)-endpoint))) F (𝓝 0)) :
    Tendsto (finiteCountMass l u h j) F (𝓝 (if j = endpoint then 1 else 0)) := by
  have H (k : ℕ) (hk : k ∈ Finset.Icc l u) :
      Tendsto (fun t : ℝ ↦ h k * exp (t * ((k : ℝ)-endpoint))) F
        (𝓝 (if k = endpoint then h endpoint else 0)) := by
    by_cases hh : k = endpoint
    · subst k
      simpa using (tendsto_const_nhds (x := h endpoint) (f := F))
    · simpa [hh] using (hlim k hk hh).const_mul (h k)
  have hS : Tendsto (fun t : ℝ ↦ ∑ k ∈ Finset.Icc l u, h k * exp (t*((k : ℝ)-endpoint)))
      F (𝓝 (h endpoint)) := by
    have := tendsto_finsetSum (Finset.Icc l u) H
    simpa [Finset.sum_ite_eq', he] using this
  have H' := (H j hj).div hS hp.ne'
  have H'' : (if j = endpoint then h endpoint else 0) / h endpoint =
      (if j = endpoint then 1 else 0) := by split_ifs <;> simp [hp.ne']
  rw [H''] at H'
  exact H'.congr fun t ↦ (finite_mass_center l u h j t endpoint).symm

/-- The finite-window distribution tends to its lower retained atom at
natural tilt minus infinity. -/
theorem finite_lower_atom {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (hh : 0 < h l) :
    Tendsto (finiteCountMass l u h j) atBot (𝓝 (if j = l then 1 else 0)) := by
  refine finite_atom_of_limits (Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩) hj hh ?_
  intro k hk hne
  have H : (l : ℝ) < (k : ℝ) := by
    exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp hk).1 (Ne.symm hne)
  exact tendsto_exp_atBot.comp (Tendsto.atBot_mul_const (sub_pos.mpr H) (f := id) tendsto_id)

/-- The finite-window distribution tends to its upper retained atom at
natural tilt plus infinity. -/
theorem finite_upper_atom {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (hh : 0 < h u) :
    Tendsto (finiteCountMass l u h j) atTop (𝓝 (if j = u then 1 else 0)) := by
  refine finite_atom_of_limits (Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩) hj hh ?_
  intro k hk hne
  have H : (k : ℝ) < (u : ℝ) := by
    exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp hk).2 hne
  exact tendsto_exp_atBot.comp (Tendsto.atTop_mul_const_of_neg (sub_neg.mpr H) (f := id) tendsto_id)

/-- At the excluded zero tilt the finite conditional law is the
normalized carrier, including the uniform law for a unit carrier. -/
theorem finite_zero_tilt_limit {l u j : ℕ} {h : ℕ → ℝ} (hlu : l ≤ u)
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) :
    Tendsto (finiteCountMass l u h j) (𝓝[<] 0)
      (𝓝 (h j / ∑ k ∈ Finset.Icc l u, h k)) := by
  have hd : (∑ k ∈ Finset.Icc l u, h k) ≠ 0 :=
    (Finset.sum_pos hh ⟨l, Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩).ne'
  have H : ContinuousAt (finiteCountMass l u h j) 0 := by
    unfold finiteCountMass
    refine ContinuousAt.div (by fun_prop) (by fun_prop) ?_
    simpa using hd
  have h := H.tendsto.mono_left (nhdsWithin_le_nhds (s := Iio (0 : ℝ)))
  simpa [finiteCountMass] using h

/-- Interior natural parameters and the admitted lower and upper atoms.
The two Booleans record whether the endpoint laws belong to the model. -/
def finiteCountDomain (lower upper : Bool) : Set (Sum ℝ Bool) :=
  {p | match p with
    | .inl _ => True
    | .inr false => lower = true
    | .inr true => upper = true}

/-- The two atom values are divided by the same carrier factor as the
finite-tilt likelihood. -/
def finiteCountLikelihood (l u : ℕ) (h : ℕ → ℝ) (m : ℝ) : Sum ℝ Bool → ℝ
  | .inl t => exp (finiteLogLik l u h m t)
  | .inr false => if m = l then (h l)⁻¹ else 0
  | .inr true => if m = u then (h u)⁻¹ else 0

private theorem atom_wins {l u j k : ℕ} {h : ℕ → ℝ}
    (hj : j ∈ Finset.Icc l u) (hk : k ∈ Finset.Icc l u) (hjk : j ≠ k)
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) (t : ℝ) :
    exp (finiteLogLik l u h j t) < (h j)⁻¹ := by
  let f (i : ℕ) := h i * exp ((i : ℝ) * t)
  have H : f j + f k ≤ ∑ i ∈ Finset.Icc l u, f i := by
    have hs : ({j,k} : Finset ℕ) ⊆ Finset.Icc l u := by simp [Finset.insert_subset_iff,hj,hk]
    simpa [hjk] using Finset.sum_le_sum_of_subset_of_nonneg hs (f := f)
      (fun i hi _ ↦ (mul_pos (hh i hi) (exp_pos _)).le)
  have hp : 0 < f j := mul_pos (hh _ hj) (exp_pos _)
  have hlt : f j < ∑ i ∈ Finset.Icc l u, f i :=
    (lt_add_of_pos_right _ (mul_pos (hh _ hk) (exp_pos _))).trans_le H
  change exp ((j : ℝ) * t - log (∑ i ∈ Finset.Icc l u, f i)) < (h j)⁻¹
  rw [exp_sub, exp_log (hp.trans hlt)]
  apply (div_lt_iff₀ (hp.trans hlt)).mpr
  rw [← one_div, div_mul_eq_mul_div, one_mul]
  exact (lt_div_iff₀ (hh j hj)).mpr (by simpa [f, mul_comm] using hlt)

/-- For a nonsingleton finite count window with unrestricted finite
tilts, attainment includes precisely the admitted mean-matching atoms. -/
theorem finite_count_fit {l u : ℕ} (hlu : l < u) {h : ℕ → ℝ}
    (hh : ∀ k ∈ Finset.Icc l u, 0 < h k) {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ))
    (lower upper : Bool) :
    (∃! p, p ∈ finiteCountDomain lower upper ∧
      IsMaxOn (finiteCountLikelihood l u h m) (finiteCountDomain lower upper) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ lower = true) ∨ (m = u ∧ upper = true) := by
  let D := finiteCountDomain lower upper
  let F := finiteCountLikelihood l u h m
  have hl : l ∈ Finset.Icc l u := Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩
  have hu : u ∈ Finset.Icc l u := Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩
  have hluR : (l : ℝ) < u := by exact_mod_cast hlu
  have hopen (t : ℝ) : Sum.inl t ∈ D := trivial
  have hin (t : ℝ) : 0 < F (.inl t) := exp_pos _
  have atom_case (b : Bool) {j other : ℕ} (hj : j ∈ Finset.Icc l u)
      (ho : other ∈ Finset.Icc l u) (hne : j ≠ other) (hmj : m = j)
      (hmEnd : m ∉ Ioo (l : ℝ) (u : ℝ))
      (h1 : F (.inr b) = (h j)⁻¹) (h2 : F (.inr (!b)) = 0) :
      (∃! p, p ∈ D ∧ IsMaxOn F D p) ↔ Sum.inr b ∈ D := by
    have hpos : 0 < F (.inr b) := h1 ▸ inv_pos.mpr (hh j hj)
    have H : ∀ p, p ≠ Sum.inr b → F p < F (.inr b) := by
      intro p hp
      cases p with
      | inl t =>
        change exp (finiteLogLik l u h m t) < F (.inr b)
        rw [h1, hmj]
        exact atom_wins hj ho hne hh t
      | inr v =>
        have hv : v = !b := by cases v <;> cases b <;> simp_all
        subst v
        rwa [h2]
    constructor
    · rintro ⟨p, ⟨hp,hmax⟩, _⟩
      by_cases hb : Sum.inr b ∈ D
      · exact hb
      · exfalso
        have hn : ¬ ∃ t, IsMaxOn (finiteLogLik l u h m) univ t := by
          intro H
          have hN : ∃! t, IsMaxOn (finiteLogLik l u h m) univ t := by
            obtain ⟨t, ht⟩ := H
            refine ⟨t, ht, fun v hv ↦ ?_⟩
            have H := Truncation.maximizers_constant_direction (Finset.Icc l u)
              (fun k ↦ (k : ℝ)) h ⟨l,hl⟩ hh hv ht l hl u hu
            change (l : ℝ) * (v-t) = (u : ℝ) * (v-t) at H
            have hz : ((u : ℝ)-l)*(v-t)=0 := by linarith
            exact sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_left (sub_pos.mpr hluR).ne')
          exact hmEnd ((finite_fit_iff hlu hh m).mp hN)
        cases p with
        | inl t =>
          apply hn
          refine ⟨t, fun v _ ↦ ?_⟩
          exact exp_le_exp.mp (hmax (hopen v))
        | inr v =>
          have hne : v ≠ b := fun he ↦ hb (he ▸ hp)
          have he : v = !b := by cases v <;> cases b <;> simp_all
          subst v
          exact (hin 0).not_ge (h2 ▸ hmax (hopen 0))
    · intro hb
      refine ⟨.inr b, ⟨hb, ?_⟩, ?_⟩
      · intro p _
        by_cases he : p = Sum.inr b
        · subst p
          exact le_rfl
        · exact (H p he).le
      · intro p ⟨_, hp⟩
        by_contra! hn
        exact (H p hn).not_ge (hp hb)
  rcases eq_or_lt_of_le hm.1 with h | h
  · have he : m ≠ (u : ℝ) := by linarith
    simpa [finiteCountDomain, F, finiteCountLikelihood, D, ← h, hluR.ne, he] using
      atom_case false hl hu hlu.ne h.symm (by intro H; exact H.1.ne h)
        (by simp [F, finiteCountLikelihood,h.symm]) (by simp [F, finiteCountLikelihood,he])
  rcases eq_or_lt_of_le hm.2 with he | he
  · simpa [finiteCountDomain, F, finiteCountLikelihood, D, he, hluR.ne', not_lt_of_ge] using
      atom_case true hu hl hlu.ne' he (by intro H; exact H.2.ne he) (by simp [F, finiteCountLikelihood,he])
        (by simp [F, finiteCountLikelihood,he,hluR.ne'])
  · refine iff_of_true ?_ (Or.inl ⟨h,he⟩)
    obtain ⟨t, ht, huniq⟩ := (finite_fit_iff hlu hh m).mpr ⟨h,he⟩
    have hnone (b : Bool) : F (.inr b) = 0 := by
      cases b <;> simp [F, finiteCountLikelihood, h.ne',he.ne]
    refine ⟨.inl t, ⟨hopen t, ?_⟩, ?_⟩
    · intro p _
      cases p with
      | inl s => exact exp_le_exp.mpr (ht (mem_univ s))
      | inr b =>
        change F (.inr b) ≤ F (.inl t)
        rw [hnone]
        exact (hin t).le
    · rintro (s | b) ⟨_, hmax⟩
      · congr 1
        exact huniq s fun v _ ↦ exp_le_exp.mp (hmax (hopen v))
      · exact ((hin t).not_ge (hnone b ▸ hmax (hopen t))).elim

/-- On a finite Poisson window, zero rate is available at lower count
zero or when a lower-atom extension has explicitly been selected. -/
theorem poisson_bounded_attainment {l u : ℕ} (hlu : l < u) {m : ℝ}
    (hm : m ∈ Icc (l : ℝ) (u : ℝ)) (extension : Bool) :
    let admitted := (l == 0) || extension
    (∃! p, p ∈ finiteCountDomain admitted false ∧
      IsMaxOn (finiteCountLikelihood l u (poissonCarrier 0) m)
        (finiteCountDomain admitted false) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ admitted = true) := by
  dsimp only
  rw [finite_count_fit hlu (h := poissonCarrier 0) (fun _ _ ↦ one_div_pos.mpr (by positivity)) hm]
  simp

/-- On a finite binomial window, the upper atom belongs to the parent at
the held size and the lower atom belongs at zero or by extension. -/
theorem binom_bounded_attainment {l u N : ℕ} (hlu : l < u) (huN : u ≤ N)
    {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) (extension : Bool) :
    let lower := (l == 0) || extension
    let upper := u == N
    (∃! p, p ∈ finiteCountDomain lower upper ∧
      IsMaxOn (finiteCountLikelihood l u (fun k ↦ (N.choose k : ℝ)) m)
        (finiteCountDomain lower upper) p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ lower = true) ∨ (m = u ∧ upper = true) :=
  finite_count_fit hlu (fun k hk ↦ by
    exact_mod_cast (Nat.choose_pos ((Finset.mem_Icc.mp hk).2.trans huN))) hm _ _

/-- The positive fixed-size binomial slice admits both atoms. At size
at least two its maximizing probability is unique. -/
theorem binom_zt_fit {N : ℕ} (hN : 1 < N) {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N) :
    ∃! p, p ∈ finiteCountDomain true true ∧
      IsMaxOn (finiteCountLikelihood 1 N (fun k ↦ (N.choose k : ℝ)) m)
        (finiteCountDomain true true) p := by
  have hh : ∀ k ∈ Finset.Icc 1 N, 0 < (N.choose k : ℝ) := fun k hk ↦ by
    exact_mod_cast (Nat.choose_pos (Finset.mem_Icc.mp hk).2)
  apply (finite_count_fit hN hh (by simpa only [Nat.cast_one] using hm) true true).mpr
  simp only [Nat.cast_one, and_true]
  rcases eq_or_lt_of_le hm.1 with h | h
  · exact Or.inr (Or.inl h.symm)
  rcases eq_or_lt_of_le hm.2 with he | he
  · exact Or.inr (Or.inr he)
  · exact Or.inl ⟨h,he⟩
/-- With binomial size one every positive-conditional probability
parameter gives unit likelihood at the only retained count. -/
theorem binom_zt_size_one (p : Sum ℝ Bool) :
    finiteCountLikelihood 1 1 (fun k ↦ ((1 : ℕ).choose k : ℝ)) 1 p = 1 := by
  rcases p with t | (_ | _)
  · change exp (finiteLogLik 1 1 (fun k ↦ ((1 : ℕ).choose k : ℝ)) 1 t) = 1
    have H := Truncation.singleton_logLik (fun k : ℕ ↦ (k : ℝ))
      (fun k ↦ ((1 : ℕ).choose k : ℝ)) 1 (by norm_num) t
    simpa [finiteLogLik] using congrArg exp H
  · norm_num [finiteCountLikelihood]
  · norm_num [finiteCountLikelihood]

/-- With positive counts at binomial size one the positive component
is flat and only the zero weight is identified. -/
theorem binom_zm_size_one {z k : ℕ} (hk : 0 < k) {w : ℝ} (hw : w ∈ Icc 0 1)
    (p : Sum ℝ Bool) :
    let D := finiteCountDomain true true
    let L := finiteCountLikelihood 1 1 (fun j ↦ ((1 : ℕ).choose j : ℝ)) 1
    IsMaxOn (fun t : Sum ℝ Bool × ℝ ↦ Truncation.bernoulli z k t.2 * L t.1 ^ k)
      (D ×ˢ Icc 0 1) (p,w) ↔ w = Truncation.zeroFraction z k := by
  dsimp only
  simp only [binom_zt_size_one, one_pow, mul_one]
  have hp (q : Sum ℝ Bool) : q ∈ finiteCountDomain true true := by
    rcases q with _ | (_ | _) <;> simp [finiteCountDomain]
  unfold Truncation.zeroFraction
  rw [← Truncation.bernoulli_max_iff (show 0 < z+k by omega) hw]
  constructor
  · intro H v hv
    exact H (a := (p,v)) ⟨hp p,hv⟩
  · intro H t ht
    exact H ht.2

/-- The fixed-size zero-modified NB fit is unique when positives
occur, including the logarithmic-series slice at held size zero. -/
theorem nbinom_zm_fit {r : ℝ} (hr : 0 ≤ r) {m : ℝ} (hm : 1 ≤ m) {z k : ℕ} (hk : 0 < k) :
    let c := if r = 0 then logSeriesCarrier else fun j ↦ negBinomCarrier r (1+j)
    let D := closedLowerDomain (Iio 0)
    ∃! p : Option ℝ × ℝ, p ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : Option ℝ × ℝ ↦
        Truncation.bernoulli z k t.2 * closedLowerLikelihood c (m-1) t.1 ^ k)
        (D ×ˢ Icc 0 1) p := by
  dsimp only
  let c := if r = 0 then logSeriesCarrier else fun j ↦ negBinomCarrier r (1+j)
  have hc : ∀ j, 0 < c j := by
    intro j
    by_cases h : r = 0
    · exact show 0 < (if r = 0 then logSeriesCarrier else _) j by
        rw [ite_eq_left h]
        exact one_div_pos.mpr (by positivity)
    · exact show 0 < (if r = 0 then logSeriesCarrier else _) j by
        rw [ite_eq_right h]
        exact nb_pos (lt_of_le_of_ne hr (Ne.symm h)) _
  have hs : ∀ t ∈ Iio (0 : ℝ), Summable (fun j : ℕ ↦ c j * exp t ^ j) := by
    intro t ht
    by_cases h : r = 0
    · unfold c
      rw [ite_eq_left h]
      exact logSeries_summable ht
    · unfold c
      rw [ite_eq_right h]
      exact nb_summable 1 r ht
  apply Truncation.modified_unique hk (fun _ hp ↦ closed_lower_positive hc hs hp) _
    (nbinom_zt_fit hr hm)
  have ht : (-1 : ℝ) ∈ Iio 0 := by norm_num
  refine ⟨some (-1), (by intro t htEq; cases htEq; exact ht), inv_pos.mpr ?_⟩
  exact (mul_pos (hc 0) (exp_pos _)).trans_le (centered_ge_term hc (hs (-1) ht) 0)

/-- The fixed-size zero-modified binomial fit is unique with positives
at size at least two. -/
theorem binom_zm_fit {N : ℕ} (hN : 1 < N) {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N)
    {z k : ℕ} (hk : 0 < k) :
    let D := finiteCountDomain true true
    let L := finiteCountLikelihood 1 N (fun j ↦ (N.choose j : ℝ)) m
    ∃! p : Sum ℝ Bool × ℝ, p ∈ D ×ˢ Icc 0 1 ∧
      IsMaxOn (fun t : Sum ℝ Bool × ℝ ↦ Truncation.bernoulli z k t.2 * L t.1 ^ k)
        (D ×ˢ Icc 0 1) p := by
  dsimp only
  refine Truncation.modified_unique hk ?_ ⟨Sum.inl 0, trivial, ?_⟩ (binom_zt_fit hN hm)
  swap
  exact exp_pos _
  rintro (t | (_ | _)) _
  · exact (exp_pos _).le
  · change 0 ≤ if m = (↑(1 : ℕ) : ℝ) then (↑(N.choose 1) : ℝ)⁻¹ else 0
    split_ifs <;> positivity
  · change 0 ≤ if m = (N : ℝ) then (↑(N.choose N) : ℝ)⁻¹ else 0
    split_ifs <;> positivity

end

end UnivariateML.TruncCount

end
