module

public import UnivariateML.Family.TruncCount
public import UnivariateML.Family.Discrete3CMB

/-! CMP with one held coefficient on known count windows.

Deviations: the bounded held-rate interior criterion is stated on the
nonflat branch, with the geometric endpoint and the flat two-cell
branch stated separately. See Trunc3CMPRay for held-rate ray attainment. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc3CMP

noncomputable section

def dispersionCarrier (ν : ℝ) (k : ℕ) : ℝ := exp (-ν * CMP.logFactorial k)

def rateCarrier (η : ℝ) (k : ℕ) : ℝ := exp (η * k)

/-- The finite window admits the zero-rate parent atom exactly when
zero is retained, and it gives positive likelihood exactly on all zeros.
This branch is independent of the dispersion. -/
theorem zero_rate_compatibility {l u n : ℕ} (x : Fin n → ℕ) :
    let w := fun k : ℕ ↦ if k = 0 then (1 : ℝ) else 0
    (0 < ∑ k ∈ Finset.Icc l u, w k) ∧ (∀ i, 0 < w (x i)) ↔
      l = 0 ∧ ∀ i, x i = 0 := by
  dsimp only
  rw [Finset.sum_ite_eq']
  have h (k : ℕ) : (0 : ℝ) < (if k = 0 then 1 else 0) ↔ k = 0 := by
    by_cases hk : k = 0 <;> simp [hk]
  simp only [h, Finset.mem_Icc, Nat.zero_le, and_true, Nat.le_zero]

/-- A retained singleton has flat objectives under either hold
throughout the compatible positive-rate chart. -/
theorem singleton_fibres (k : ℕ) (η ν t : ℝ) :
    Truncation.logLik {k} (fun j ↦ (j : ℝ)) (dispersionCarrier ν) k t =
        -log (dispersionCarrier ν k) ∧
      Truncation.logLik {k} CMP.logFactorial (rateCarrier η) (CMP.logFactorial k) t =
        -log (rateCarrier η k) :=
  ⟨Truncation.singleton_logLik _ _ k (exp_pos _) t,
    Truncation.singleton_logLik _ _ k (exp_pos _) t⟩

/-- At positive held dispersion, a finite window has the full signed
log-rate chart, with the zero-rate atom included exactly at lower cell zero. -/
theorem held_dispersion_finite {l u : ℕ} (hlu : l < u) {ν : ℝ} (_hν : 0 < ν)
    {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let D := TruncCount.finiteCountDomain (decide (l = 0)) false
    (∃! p, p ∈ D ∧ IsMaxOn
      (TruncCount.finiteCountLikelihood l u (dispersionCarrier ν) m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0) := by
  simpa using TruncCount.finite_count_fit hlu
    (h := dispersionCarrier ν) (fun _ _ ↦ exp_pos _) hm (decide (l = 0)) false

private theorem uniform_mean {l u : ℕ} (hlu : l ≤ u) :
    Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) =
      ((l : ℝ) + u) / 2 := by
  let A := Finset.Icc l u
  have hz : (A.card : ℝ) ≠ 0 := by
    exact_mod_cast (Finset.card_ne_zero.mpr ⟨l, Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩)
  have reflect (k : ℕ) (hk : k ∈ A) : l+u-k ∈ A := by
    have h := Finset.mem_Icc.mp hk
    exact Finset.mem_Icc.mpr ⟨by omega, by omega⟩
  have hs : (∑ k ∈ A, (k : ℝ)) = ∑ k ∈ A, ((l+u-k : ℕ) : ℝ) := by
    apply Finset.sum_bij (fun k _ ↦ l+u-k)
    · exact reflect
    · intro a ha b hb h
      have ha' := Finset.mem_Icc.mp ha
      have hb' := Finset.mem_Icc.mp hb
      omega
    · intro k hk
      have hk' := Finset.mem_Icc.mp hk
      exact ⟨l+u-k, reflect k hk, by omega⟩
    · intro k hk
      have hk' := Finset.mem_Icc.mp hk
      congr 1
      omega
  have ht : (∑ k ∈ A, ((l : ℝ) + u)) =
      (∑ k ∈ A, (k : ℝ)) + (∑ k ∈ A, ((l+u-k : ℕ) : ℝ)) := by
    rw [← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro k hk
    have hk' := Finset.mem_Icc.mp hk
    rw [Nat.cast_sub (by omega), Nat.cast_add]
    ring
  unfold Truncation.carrierMean
  change (∑ k ∈ A, 1 * (k : ℝ)) / (∑ _k ∈ A, (1 : ℝ)) = _
  simp only [one_mul, Finset.sum_const, nsmul_eq_mul, mul_one] at ht ⊢
  rw [div_eq_iff hz]
  linarith

/-- At zero dispersion, the finite geometric chart excludes zero tilt;
on interior sample means its unique finite fit requires a mean below
the uniform mean. -/
theorem held_zero_dispersion_finite {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : m ∈ Ioo (l : ℝ) (u : ℝ)) :
    (∃! t, t < 0 ∧ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) m)
      (Iio 0) t) ↔ m < ((l : ℝ) + u) / 2 :=
  (uniform_mean hlu.le) ▸ TruncCount.geom_bounded_fit hlu hm

/-- At and above the uniform mean the finite zero-dispersion objective
strictly improves towards the excluded geometric endpoint. -/
theorem held_zero_dispersion_failure {l u : ℕ} (hlu : l < u)
    {m : ℝ} (hm : ((l : ℝ) + u) / 2 ≤ m) :
    ∀ t < 0, ¬ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) (fun k ↦ (k : ℝ)) (fun _ ↦ 1) m)
      (Iio 0) t :=
  (Truncation.scalar_negative_upper_failure (fun _ _ ↦ one_pos)
    (Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩) (Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩)
    (by exact_mod_cast hlu) ((uniform_mean hlu.le).symm ▸ hm)).2

/-- At positive held dispersion, either extreme rate concentrates at
the corresponding retained cell. Admission is governed by the parent chart. -/
theorem held_dispersion_finite_atoms {l u j : ℕ} (hlu : l ≤ u)
    (hj : j ∈ Finset.Icc l u) (ν : ℝ) :
    Tendsto (TruncCount.finiteCountMass l u (dispersionCarrier ν) j) atBot
      (𝓝 (if j = l then 1 else 0)) ∧
    Tendsto (TruncCount.finiteCountMass l u (dispersionCarrier ν) j) atTop
      (𝓝 (if j = u then 1 else 0)) :=
  ⟨TruncCount.finite_lower_atom hlu hj (exp_pos _),
    TruncCount.finite_upper_atom hlu hj (exp_pos _)⟩

/-- As the geometric log rate approaches its excluded endpoint zero,
the finite window tends to the uniform distribution. -/
theorem held_zero_dispersion_uniform {l u j : ℕ} (hlu : l ≤ u) :
    Tendsto (TruncCount.finiteCountMass l u (fun _ ↦ 1) j) (𝓝[<] 0)
      (𝓝 (1 / ((Finset.Icc l u).card : ℝ))) := by
  simpa using TruncCount.finite_zero_tilt_limit (j := j) hlu
    (h := fun _ ↦ 1) (fun _ _ ↦ one_pos)

/-- At fixed log rate, positive dispersion is negative tilt on the
factorial statistic. For an interior empirical statistic the positive
dispersion root exists uniquely below the geometric carrier mean. -/
theorem held_rate_interior {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) {m : ℝ}
    (hm : m ∈ Ioo (CMP.logFactorial l) (CMP.logFactorial u)) :
    (∃! t, t < 0 ∧ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η) m) (Iio 0) t)
      ↔ m < Truncation.carrierMean (Finset.Icc l u) CMP.logFactorial (rateCarrier η) := by
  have mono : Monotone CMP.logFactorial := by
    intro a b hab
    apply log_le_log (by exact_mod_cast Nat.factorial_pos a)
    exact_mod_cast Nat.factorial_le hab
  exact Truncation.scalar_negative_existsUnique_iff (fun _ _ ↦ exp_pos _)
    (Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩) (Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩) hlu'
    (fun k hk ↦ ⟨mono (Finset.mem_Icc.mp hk).1, mono (Finset.mem_Icc.mp hk).2⟩) hm

/-- At the minimal empirical factorial statistic, dispersion can always
increase the likelihood on any nonflat finite held-rate window. -/
theorem held_rate_minimum_failure {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) :
    let D := if η < 0 then Iic (0 : ℝ) else Iio 0
    ∀ t ∈ D, ¬ IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η)
        (CMP.logFactorial l)) D t := by
  dsimp only
  intro t ht H
  let v := t-1
  have hvt : v < t := by dsimp [v]; linarith
  have hv : v ∈ (if η < 0 then Iic (0 : ℝ) else Iio 0) := by
    split_ifs at ht ⊢
    · exact hvt.le.trans ht
    · exact hvt.trans ht
  let w (s : ℝ) (k : ℕ) := rateCarrier η k * exp (s*(CMP.logFactorial k-CMP.logFactorial l))
  have eq (s : ℝ) :
      Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η)
        (CMP.logFactorial l) s = -log (∑ k ∈ Finset.Icc l u, w s k) := by
    have he (k : ℕ) : w s k = rateCarrier η k * exp (CMP.logFactorial k*s) *
        exp (-(CMP.logFactorial l*s)) := by
      dsimp [w]
      rw [mul_assoc, ← exp_add]
      congr 2
      ring
    have hp : 0 < ∑ k ∈ Finset.Icc l u, rateCarrier η k * exp (CMP.logFactorial k*s) :=
      Finset.sum_pos (fun _ _ ↦ mul_pos (exp_pos _) (exp_pos _))
        ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩
    change CMP.logFactorial l*s - log (∑ k ∈ Finset.Icc l u,
      rateCarrier η k * exp (CMP.logFactorial k*s)) = _
    simp only [he, ← Finset.sum_mul, log_mul hp.ne' (exp_pos _).ne', log_exp]
    ring
  have hs : (∑ k ∈ Finset.Icc l u, w v k) < ∑ k ∈ Finset.Icc l u, w t k := by
    apply Finset.sum_lt_sum
    · intro k hk
      have hk' : CMP.logFactorial l ≤ CMP.logFactorial k := by
        apply log_le_log (by exact_mod_cast Nat.factorial_pos l)
        exact_mod_cast Nat.factorial_le (Finset.mem_Icc.mp hk).1
      exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr
        (mul_le_mul_of_nonneg_right hvt.le (sub_nonneg.mpr hk'))) (exp_pos _).le
    · exact ⟨u,Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩,mul_lt_mul_of_pos_left
        (exp_lt_exp.mpr (mul_lt_mul_of_pos_right hvt (sub_pos.mpr hlu'))) (exp_pos _)⟩
  have hlog := log_lt_log (Finset.sum_pos (fun _ _ ↦ mul_pos (exp_pos _) (exp_pos _))
    ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩) hs
  have bad := H hv
  change Truncation.logLik _ _ _ _ v ≤ Truncation.logLik _ _ _ _ t at bad
  rw [eq,eq] at bad
  exact (neg_lt_neg hlog).not_ge bad

/-- At or above the geometric mean statistic the zero-dispersion
endpoint wins. Its inclusion in the parent chart requires negative log rate. -/
theorem held_rate_geometric {l u : ℕ} (hlu : l ≤ u) (η : ℝ)
    (hlu' : CMP.logFactorial l < CMP.logFactorial u) {m : ℝ}
    (hm : Truncation.carrierMean (Finset.Icc l u) CMP.logFactorial (rateCarrier η) ≤ m) :
    let D := if η < 0 then Iic (0 : ℝ) else Iio 0
    ∀ t ∈ D, IsMaxOn
      (Truncation.logLik (Finset.Icc l u) CMP.logFactorial (rateCarrier η) m) D t
      ↔ t = 0 ∧ η < 0 := by
  obtain ⟨hmono,hfail⟩ := Truncation.scalar_negative_upper_failure
    (fun _ _ ↦ exp_pos _) (Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩)
    (Finset.mem_Icc.mpr ⟨hlu,le_rfl⟩) hlu' hm
  dsimp only
  split_ifs with hη
  · intro t ht
    constructor
    · intro H
      exact ⟨eq_of_le_of_not_lt ht fun h ↦ (hmono ht le_rfl h).not_ge (H (a := 0) le_rfl),hη⟩
    · rintro ⟨rfl,_⟩ v hv
      exact hmono.monotoneOn hv le_rfl hv
  · intro t ht
    exact iff_of_false (hfail t ht) (fun h ↦ hη h.2)

/-- In the finite held-rate chart the zero-dispersion limit preserves
the rate carrier. The same limit applies at excluded rates. -/
theorem held_rate_geometric_limit {l u j : ℕ} (hlu : l ≤ u) (η : ℝ) :
    Tendsto (fun t : ℝ ↦
      rateCarrier η j * exp (t * CMP.logFactorial j) /
      ∑ k ∈ Finset.Icc l u, rateCarrier η k * exp (t * CMP.logFactorial k))
      (𝓝[<] 0) (𝓝 (rateCarrier η j / ∑ k ∈ Finset.Icc l u, rateCarrier η k)) := by
  have hp : 0 < ∑ k ∈ Finset.Icc l u, rateCarrier η k :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨l, Finset.mem_Icc.mpr ⟨le_rfl,hlu⟩⟩
  have H : ContinuousAt (fun t : ℝ ↦
      rateCarrier η j * exp (t * CMP.logFactorial j) /
      ∑ k ∈ Finset.Icc l u, rateCarrier η k * exp (t * CMP.logFactorial k)) 0 := by
    apply ContinuousAt.div (by fun_prop) (by fun_prop)
    simpa using hp.ne'
  simpa using H.tendsto.mono_left (nhdsWithin_le_nhds (s := Iio (0 : ℝ)))

/-- Infinite dispersion at fixed rate selects the minimal factorial
statistic set and conditions the rate carrier on it. -/
theorem held_rate_minimum_limit {l u : ℕ} (η : ℝ)
    (k : Finset.Icc l u)
    (hk : CMP.logFactorial k.val = CMP.logFactorial l) :
    let A := Finset.Icc l u
    let F : Finset A := Finset.univ.filter (fun j ↦ CMP.logFactorial j.val = CMP.logFactorial l)
    Tendsto (fun t ↦ Discrete3CMB.rayMass (fun j : A ↦ η * j.val)
      (fun j : A ↦ CMP.logFactorial l - CMP.logFactorial j.val) t k)
      atTop (𝓝 (exp (η * k.val) / ∑ j ∈ F, exp (η * j.val))) := by
  classical
  dsimp only
  apply Discrete3CMB.exposed_mass
  · simp [hk]
  · intro j hj
    have he : CMP.logFactorial j.val = CMP.logFactorial l :=
      (Finset.mem_filter.mp hj).2
    rw [he, sub_self]
  · intro j hj
    have hn : CMP.logFactorial j.val ≠ CMP.logFactorial l := by simpa using hj
    have hm : CMP.logFactorial l ≤ CMP.logFactorial j.val := by
      apply log_le_log (by exact_mod_cast Nat.factorial_pos l)
      exact_mod_cast Nat.factorial_le (Finset.mem_Icc.mp j.property).1
    exact sub_neg.mpr (lt_of_le_of_ne hm hn.symm)

/-- The minimal factorial face consists of the lower retained cell,
and also cell one if the lower cell is zero and one is retained. -/
theorem minimum_factorial_cells {l u k : ℕ} (hk : k ∈ Finset.Icc l u) :
    CMP.logFactorial k = CMP.logFactorial l ↔ k = l ∨ (l = 0 ∧ k = 1) := by
  constructor
  · intro he
    have hlk := (Finset.mem_Icc.mp hk).1
    rcases hlk.eq_or_lt with h | h
    · exact Or.inl h.symm
    · right
      by_contra! hbad
      have hk' : 1 < k := by
        by_contra! H
        exact hbad (by omega) (by omega)
      have hpred : l ≤ k-1 := by omega
      have hn : (k-1).factorial < k.factorial := by
        have hfact := Nat.factorial_pos (k-1)
        have hm := Nat.mul_lt_mul_of_pos_right hk' hfact
        nth_rw 2 [show k = (k-1)+1 by omega]
        rw [Nat.factorial_succ]
        simpa only [one_mul, show k-1+1 = k by omega] using hm
      have hf := (Nat.factorial_le hpred).trans_lt hn
      exact (log_lt_log (by exact_mod_cast Nat.factorial_pos l) (by exact_mod_cast hf)).ne he.symm
  · rintro (rfl | ⟨rfl,rfl⟩)
    · rfl
    · simp [CMP.logFactorial]

/-- On the two-cell window zero and one, the factorial statistic is
flat and every admissible dispersion ties at held positive rate. -/
theorem held_rate_binary_flat (η t : ℝ) :
    IsMaxOn (Truncation.logLik (Finset.Icc (0 : ℕ) 1) CMP.logFactorial (rateCarrier η) 0)
      (if η < 0 then Iic 0 else Iio 0) t := by
  have hA : Finset.Icc (0 : ℕ) 1 = {0,1} := by decide
  intro s _
  change Truncation.logLik _ _ _ _ s ≤ Truncation.logLik _ _ _ _ t
  simp [Truncation.logLik, Truncation.partition, hA, CMP.logFactorial]

private theorem positive_carrier_summable {ν : ℝ} (hν : 0 < ν) (t : ℝ) :
    Summable (fun k : ℕ ↦ dispersionCarrier ν (k+1) * exp t ^ k) := by
  have hs := (summable_nat_add_iff 1).mpr (CMP.summable_weight (η := t) (Or.inl hν))
  convert hs.mul_right (exp (-t)) using 1
  ext k
  simp only [dispersionCarrier, CMP.weight, ← exp_nat_mul, ← exp_add, Nat.cast_add, Nat.cast_one]
  congr 1
  ring

/-- On the positive ray, positive held dispersion gives a unique finite
log-rate optimum exactly for a positive excess mean over one. -/
theorem held_dispersion_positive {ν : ℝ} (hν : 0 < ν) {m : ℝ} (hm : 0 ≤ m) :
    (∃! t, IsMinOn (TruncCount.centeredSum (fun k ↦ dispersionCarrier ν (k+1)) m)
      univ t) ↔ 0 < m := by
  let c := fun k : ℕ ↦ dispersionCarrier ν (k + 1)
  have hs (t : ℝ) : Summable (fun k : ℕ ↦ c k * exp (t * ((k : ℝ) - m))) := by
    convert (positive_carrier_summable hν t).mul_right (exp (-(t*m))) using 1
    ext k
    rw [← exp_nat_mul, mul_assoc, ← exp_add]
    congr 2
    ring
  have hr (t : ℝ) (_ : t ∈ (univ : Set ℝ)) :
      ∃ u ∈ univ, t < u ∧ TruncCount.centeredSum c m t < TruncCount.centeredSum c m u := by
    obtain ⟨j, hj⟩ := exists_nat_gt m
    have H : Tendsto (fun u : ℝ ↦ c j * exp (u * ((j : ℝ)-m))) atTop atTop :=
      (tendsto_exp_atTop.comp (tendsto_id.atTop_mul_const (sub_pos.mpr hj))).const_mul_atTop
        (exp_pos _)
    obtain ⟨u, hu, htu⟩ := ((H.eventually (eventually_gt_atTop
      (TruncCount.centeredSum c m t))).and (eventually_gt_atTop t)).exists
    refine ⟨u, mem_univ _, htu, hu.trans_le ?_⟩
    exact (hs u).le_tsum j (fun k _ ↦ (mul_pos (exp_pos _) (exp_pos _)).le)
  simpa only [mem_univ, true_and] using TruncCount.lower_window_fit
    (c := c) convex_univ isOpen_univ (Filter.Eventually.of_forall (fun _ ↦ mem_univ _))
    (fun _ ↦ exp_pos _) hm (fun t _ ↦ positive_carrier_summable hν t) hr

/-- At positive held dispersion, the low-rate limit on the positive ray
is the atom at one (index zero after shifting the carrier). -/
theorem held_dispersion_positive_atom {ν : ℝ} (hν : 0 < ν) (j : ℕ) :
    Tendsto (fun t : ℝ ↦ dispersionCarrier ν (j+1) * exp t ^ j /
      ∑' k : ℕ, dispersionCarrier ν (k+1) * exp t ^ k)
      atBot (𝓝 (if j = 0 then 1 else 0)) :=
  TruncCount.lower_atom (fun _ ↦ exp_pos _) (positive_carrier_summable hν 0) j

/-- On the positive ray, at any held positive rate the infinite-dispersion
limit is the atom at one. A positive-dispersion series supplies domination. -/
theorem held_rate_positive_atom (η : ℝ) (j : ℕ) :
    Tendsto (fun ν ↦ CMP.weight η ν (j+1) / ∑' k : ℕ, CMP.weight η ν (k+1))
      atTop (𝓝 (if j = 0 then 1 else 0)) := by
  have hnon (k : ℕ) : 0 ≤ CMP.logFactorial (k+1) := by
    apply log_nonneg
    exact_mod_cast Nat.factorial_pos (k+1)
  have hpos {k : ℕ} (hk : k ≠ 0) : 0 < CMP.logFactorial (k+1) := by
    apply log_pos
    have H := Nat.factorial_pos k
    exact_mod_cast (show 1 < (k+1).factorial from
      (show 1 < k+1 by omega).trans_le (by
        rw [Nat.factorial_succ]
        exact Nat.le_mul_of_pos_right _ H))
  have hs := (summable_nat_add_iff 1).mpr
    (CMP.summable_weight (η := η) (ν := 1) (Or.inl one_pos))
  have ht (k : ℕ) : Tendsto (fun ν ↦ CMP.weight η ν (k+1)) atTop
      (𝓝 (if k = 0 then exp η else 0)) := by
    by_cases hk : k = 0
    · subst k
      simp [CMP.weight, CMP.logFactorial]
    · have H := tendsto_exp_atBot.comp
        (tendsto_id.atTop_mul_const_of_neg (neg_neg_of_pos (hpos hk)))
      apply ((H.const_mul (exp (η * ((k+1 : ℕ) : ℝ)))).congr _).mono_right
        (show 𝓝 (exp (η * ((k+1 : ℕ) : ℝ)) * 0) ≤
          𝓝 (if k = 0 then exp η else 0) by simp [hk])
      intro ν
      simp only [Function.comp_def, id_eq, ← exp_add, CMP.weight]
      congr 1
      ring
  have hsum : Tendsto (fun ν ↦ ∑' k : ℕ, CMP.weight η ν (k+1)) atTop (𝓝 (exp η)) := by
    have H := tendsto_tsum_of_dominated_convergence hs ht
      ((eventually_ge_atTop (1 : ℝ)).mono fun ν hν k ↦ by
        unfold CMP.weight
        rw [Real.norm_of_nonneg (exp_pos _).le]
        apply exp_le_exp.mpr
        nlinarith [mul_nonneg (sub_nonneg.mpr hν) (hnon k)])
    simpa only [tsum_ite_eq] using H
  have H := (ht j).div hsum (exp_pos η).ne'
  by_cases hj : j = 0 <;> simpa [Pi.div_def, hj, div_self (exp_pos η).ne'] using H

/-- On the positive ray at held zero dispersion, use the positive
geometric fit with the parent probability-one endpoint excluded. -/
theorem held_zero_dispersion_positive {m : ℝ} (hm : 1 ≤ m) :
    (∃! p, p ∈ Ioo (0 : ℝ) 1 ∧
      IsMaxOn (Discrete.Geometric.likelihood (m-1)) (Ioo 0 1) p) ↔ 1 < m := by
  -- Work in the equivalent signed log continuation probability chart
  -- by comparing directly with the closed-domain geometric fit.
  have hm0 : 0 ≤ m-1 := sub_nonneg.mpr hm
  by_cases he : m = 1
  · subst m
    simp only [lt_self_iff_false, iff_false]
    rintro ⟨p,⟨hp,H⟩,_⟩
    let q := (p+1)/2
    have hq : q ∈ Ioo (0 : ℝ) 1 := ⟨by dsimp [q]; linarith [hp.1], by dsimp [q]; linarith [hp.2]⟩
    have h := H hq
    change Discrete.Geometric.likelihood (1-1) q ≤ Discrete.Geometric.likelihood (1-1) p at h
    simp [Discrete.Geometric.likelihood, Discrete.Binomial.likelihood, hp.1.ne',
      hq.1.ne', hp.2.ne, hq.2.ne, exp_log hp.1, exp_log hq.1] at h
    dsimp [q] at h
    linarith [hp.2]
  · have hm' : 1 < m := lt_of_le_of_ne hm (Ne.symm he)
    refine iff_of_true ?_ hm'
    let p := 1 / (1 + (m-1))
    have hp : p ∈ Ioo (0 : ℝ) 1 :=
      ⟨one_div_pos.mpr (by linarith), (div_lt_one (by linarith)).mpr (by linarith)⟩
    have hclosed := (Discrete.Geometric.unique_mle hm0 p ⟨hp.1,hp.2.le⟩).mpr rfl
    refine ⟨p, ⟨hp,fun q hq ↦ hclosed ⟨hq.1,hq.2.le⟩⟩, fun q ⟨hq,H⟩ ↦ ?_⟩
    apply (Discrete.Geometric.unique_mle hm0 q ⟨hq.1,hq.2.le⟩).mp
    intro r hr
    rcases hr.2.eq_or_lt with hr' | hr'
    · subst r
      have hpos : 0 < Discrete.Geometric.likelihood (m-1) q := (Discrete.Geometric.held_positive_iff ⟨hq.1,hq.2.le⟩).mpr (Or.inl hq.2)
      have hz : Discrete.Geometric.likelihood (m-1) 1 = 0 := by
        simp [Discrete.Geometric.likelihood, Discrete.Binomial.likelihood,
          show m-1 ≠ 0 by linarith]
      change Discrete.Geometric.likelihood (m-1) 1 ≤ Discrete.Geometric.likelihood (m-1) q
      rw [hz]
      exact hpos.le
    · exact H ⟨hr.1,hr'⟩

end

end UnivariateML.Trunc3CMP

end
