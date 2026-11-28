module

public import UnivariateML.Family.Trunc3Zero
public import UnivariateML.Family.TruncCount

/-! Sample-level zero modification on the closed conditional charts.
Deviations: the scalar likelihoods omit positive carrier factors. The
binomial nonsingleton and negative-binomial charts use tagged endpoints. -/

@[expose] public section

open Set Real
open scoped Topology

namespace UnivariateML.Zero6

open Truncation

/-- Free (p_0) or a held zero probability. -/
def weightDomain (hold : Option ℝ) : Set ℝ :=
  match hold with
  | none => Icc 0 1
  | some w => Icc 0 1 ∩ {w}

noncomputable def chosenWeight (z k : ℕ) (hold : Option ℝ) : ℝ :=
  hold.getD (zeroFraction z k)

noncomputable def modified {P : Type*} (z k : ℕ) (L : P → ℝ) (t : P × ℝ) : ℝ :=
  bernoulli z k t.2 * (if k = 0 then 1 else L t.1)

private theorem selected {z k : ℕ} (hn : 0 < z+k) (hold : Option ℝ)
    {w : ℝ} (hw : w ∈ weightDomain hold) :
    IsMaxOn (bernoulli z k) (weightDomain hold) w ↔ w = chosenWeight z k hold := by
  cases hold with
  | none => exact bernoulli_max_iff hn hw
  | some r =>
    have h : w = r := hw.2
    exact ⟨fun _ ↦ h, fun _ _ hq ↦ by rw [h, show _ = r from hq.2]; exact le_rfl⟩

/-- The maximizer set on both sample branches, with both weight designs.
The right hand side is the positive-likelihood fit criterion. -/
theorem modified_fit {P : Type*} {D : Set P} {L : P → ℝ} {z k : ℕ}
    (hn : 0 < z+k) (hold : Option ℝ) (hL : ∀ q ∈ D, 0 ≤ L q)
    (hpos : k ≠ 0 → ∃ q ∈ D, 0 < L q)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ weightDomain hold) :
    (IsMaxOn (modified z k L) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k L (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → IsMaxOn L D p) := by
  let A : P → ℝ := fun q ↦ if k = 0 then 1 else L q
  have ha (q : P) (hq : q ∈ D) : 0 ≤ A q := by
    unfold A
    split_ifs <;> first | exact zero_le_one | exact hL q hq
  have hw' : weightDomain hold ⊆ Icc (0 : ℝ) 1 := by
    cases hold with
    | none => exact Subset.rfl
    | some r => exact inter_subset_left
  have hb (r : ℝ) (hr : r ∈ weightDomain hold) : 0 ≤ bernoulli z k r :=
    mul_nonneg (pow_nonneg (hw' hr).1 _) (pow_nonneg (sub_nonneg.mpr (hw' hr).2) _)
  change (IsMaxOn (fun t : P × ℝ ↦ bernoulli z k t.2 * A t.1)
    (D ×ˢ weightDomain hold) (p,w) ∧ 0 < bernoulli z k w * A p) ↔ _
  constructor
  · rintro ⟨H, Hpos⟩
    have hB : 0 < bernoulli z k w := by
      by_contra! h
      exact (not_lt_of_ge (mul_nonpos_of_nonpos_of_nonneg h (ha p hp))) Hpos
    have hA : 0 < A p := by
      by_contra! h
      exact (not_lt_of_ge (mul_nonpos_of_nonneg_of_nonpos hB.le h)) Hpos
    refine ⟨(selected hn hold hw).mp (fun r hr ↦
      (mul_le_mul_iff_of_pos_right hA).mp (H (a := (p,r)) ⟨hp,hr⟩)), hB, ?_⟩
    intro hk q hq
    have h := (mul_le_mul_iff_of_pos_left hB).mp (H (a := (q,w)) ⟨hq,hw⟩)
    simpa [A, hk] using h
  · rintro ⟨he, hB, hmax⟩
    have hW := (selected hn hold hw).mpr he
    have hA : IsMaxOn A D p := by
      intro q hq
      by_cases hk : k = 0
      · simp [A, hk]
      · simpa [A, hk] using hmax hk hq
    refine ⟨fun t ht ↦ mul_le_mul (hW ht.2) (hA ht.1) (ha _ ht.1) hB.le, mul_pos hB ?_⟩
    by_cases hk : k = 0
    · simp [A, hk]
    · obtain ⟨q,hq,hqpos⟩ := hpos hk
      exact lt_of_lt_of_le (show 0 < A q by simpa [A, hk] using hqpos) (hA hq)

private theorem powered_max {P : Type*} {D : Set P} {F : P → ℝ} {k : ℕ}
    (hk : k ≠ 0) (hF : ∀ q ∈ D, 0 ≤ F q) {p : P} (hp : p ∈ D) :
    IsMaxOn (fun q ↦ F q ^ k) D p ↔ IsMaxOn F D p := by
  constructor
  · intro H q hq
    by_contra! h
    change ¬ F q ≤ F p at h
    exact (pow_lt_pow_left₀ (lt_of_not_ge h) (hF p hp) hk).not_ge (H hq)
  · intro H q hq
    exact pow_le_pow_left₀ (hF q hq) (H hq) _

private theorem regular_modified {P : Type*} {D : Set P} {F : P → ℝ}
    {z k : ℕ} (hn : 0 < z+k) (hold : Option ℝ)
    (hF : ∀ q ∈ D, 0 ≤ F q) (hpos : ∃ q ∈ D, 0 < F q)
    (hparent : k ≠ 0 → ∃! q, q ∈ D ∧ IsMaxOn F D q)
    {p : P} {w : ℝ} (hp : p ∈ D) (hw : w ∈ weightDomain hold) :
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (∀ hk : k ≠ 0, p = Classical.choose (hparent hk).exists) := by
  rw [modified_fit hn hold (fun q hq ↦ pow_nonneg (hF q hq) _)
    (fun _ ↦ let ⟨q,hq,h⟩ := hpos; ⟨q,hq,pow_pos h _⟩) hp hw]
  refine and_congr_right fun _ ↦ and_congr_right fun _ ↦ forall_congr' fun hk ↦ ?_
  rw [powered_max hk hF hp]
  have hs := Classical.choose_spec (hparent hk).exists
  obtain ⟨q, _, huniq⟩ := hparent hk
  constructor
  · intro H
    exact (huniq p ⟨hp,H⟩).trans (huniq _ hs).symm
  · rintro rfl
    exact hs.2

/-- Pois, free or held p0, zero:modified criterion and uniqueness.
On positives the mean-matched conditional fit is unique, with the
zero-rate atom at one included. On all zeros every rate is tied. -/
theorem pois_modified {z k : ℕ} (hn : 0 < z+k) {m : ℝ} (hm : 1 ≤ m)
    (hold : Option ℝ) {p w : ℝ} (hp : p ∈ Ici 0) (hw : w ∈ weightDomain hold) :
    let F := Discrete.ZeroTruncatedPoisson.likelihood m
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (Ici 0 ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (k ≠ 0 → (m = 1 ∧ p = 0 ∨ 1 < m ∧ 0 < p ∧
        Discrete.ZeroTruncatedPoisson.conditionalMean p = m)) := by
  open Discrete.ZeroTruncatedPoisson in
  have hpos {q : ℝ} (hq : 0 < q) : 0 < likelihood m q :=
    (held_positive_iff hq.le).mpr (Or.inl hq)
  have hF (q : ℝ) (hq : q ∈ Ici (0 : ℝ)) : 0 ≤ Discrete.ZeroTruncatedPoisson.likelihood m q := by
    rcases eq_or_lt_of_le hq with he | ht
    · rw [← he]
      by_cases h : m = 1 <;> simp [Discrete.ZeroTruncatedPoisson.likelihood, h]
    · exact (hpos ht).le
  dsimp only
  rw [modified_fit hn hold (fun q hq ↦ pow_nonneg (hF q hq) _)
    (fun _ ↦ ⟨1, zero_le_one, pow_pos (hpos one_pos) _⟩) hp hw]
  refine and_congr_right fun _ ↦ and_congr_right fun _ ↦ forall_congr' fun hk ↦ ?_
  rw [powered_max hk hF hp]
  rcases hm.eq_or_lt with he | ht
  · rw [← he]
    simpa using Discrete.ZeroTruncatedPoisson.all_ones_attained p hp
  · simpa [ht.ne', ht] using Discrete.ZeroTruncatedPoisson.closed_domain_mle ht p hp

/-- Geom, free or held p0, zero:modified criterion and uniqueness.
The positive branch fits the reciprocal positive mean. -/
theorem geom_modified {z k : ℕ} (hn : 0 < z+k) {m : ℝ} (hm : 1 ≤ m)
    (hold : Option ℝ) {p w : ℝ} (hp : p ∈ Ioc 0 1) (hw : w ∈ weightDomain hold) :
    let F := Discrete.Geometric.likelihood (m-1)
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (Ioc 0 1 ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧ (k ≠ 0 → p = 1/m) := by
  have hF (q : ℝ) (_ : q ∈ Ioc (0 : ℝ) 1) : 0 ≤ Discrete.Geometric.likelihood (m-1) q := by
    unfold Discrete.Geometric.likelihood Discrete.Binomial.likelihood
    split_ifs
    · exact le_rfl
    · exact (Real.exp_pos _).le
  have hhalf : (1/2 : ℝ) ∈ Ioc 0 1 := ⟨by norm_num, by norm_num⟩
  have hpos := (Discrete.Geometric.held_positive_iff (a := m-1) hhalf).mpr
    (Or.inl (by norm_num))
  dsimp only
  rw [modified_fit hn hold (fun q hq ↦ pow_nonneg (hF q hq) _)
    (fun _ ↦ ⟨1/2, hhalf, pow_pos hpos _⟩) hp hw]
  refine and_congr_right fun _ ↦ and_congr_right fun _ ↦ forall_congr' fun hk ↦ ?_
  rw [powered_max hk hF hp]
  simpa only [show 1 + (m-1) = m by ring] using
    Discrete.Geometric.unique_mle (sub_nonneg.mpr hm) p hp

/-- Nbinom, size and p0+size, zero:modified criterion and uniqueness.
Includes the logarithmic carrier at size zero and the atom at probability one. -/
theorem nbinom_modified {r : ℝ} (hr : 0 ≤ r) {z k : ℕ} (hn : 0 < z+k)
    {m : ℝ} (hm : 1 ≤ m) (hold : Option ℝ) {p : Option ℝ} {w : ℝ}
    (hp : p ∈ TruncCount.closedLowerDomain (Iio 0)) (hw : w ∈ weightDomain hold) :
    let c := if r = 0 then TruncCount.logSeriesCarrier
      else fun j ↦ TruncCount.negBinomCarrier r (1+j)
    let D := TruncCount.closedLowerDomain (Iio 0)
    let F := TruncCount.closedLowerLikelihood c (m-1)
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (∀ _ : k ≠ 0, p = Classical.choose (TruncCount.nbinom_zt_fit hr hm).exists) := by
  dsimp only
  let c := if r = 0 then TruncCount.logSeriesCarrier
    else fun j ↦ TruncCount.negBinomCarrier r (1+j)
  have hc (j : ℕ) : 0 < c j := by
    unfold c TruncCount.logSeriesCarrier TruncCount.negBinomCarrier
    split_ifs with h
    · positivity
    · let j' := 1+j
      change 0 < Ring.choose (r + (j' : ℝ) - 1) j'
      have H := Ring.descPochhammer_eq_factorial_smul_choose (r + (j' : ℝ) - 1) j'
      rw [nsmul_eq_mul] at H
      have hp : 0 < (descPochhammer ℤ j').smeval (r + (j' : ℝ) - 1) := by
        rw [Polynomial.descPochhammer_smeval_eq_ascPochhammer,
          Polynomial.ascPochhammer_smeval_cast, Polynomial.ascPochhammer_smeval_eq_eval,
          show r + (j' : ℝ) - 1 - j' + 1 = r by ring]
        exact ascPochhammer_pos j' r (lt_of_le_of_ne hr (Ne.symm h))
      exact pos_of_mul_pos_right (H ▸ hp) (by positivity)
  have ht : some (-1 : ℝ) ∈ TruncCount.closedLowerDomain (Iio 0) := by
    intro t h
    cases h
    norm_num
  have hnn (t : ℝ) (j : ℕ) : 0 ≤ c j * Real.exp (t * ((j : ℝ) - (m-1))) :=
    mul_nonneg (hc j).le (Real.exp_pos _).le
  refine regular_modified hn hold ?_ ⟨some (-1),ht,?_⟩
    (fun _ ↦ TruncCount.nbinom_zt_fit hr hm) hp hw
  · rintro (_ | t) _
    · change 0 ≤ if m-1 = 0 then (c 0)⁻¹ else 0
      split_ifs
      · exact inv_nonneg.mpr (hc 0).le
      · exact le_rfl
    · exact inv_nonneg.mpr (tsum_nonneg (hnn t))
  · -- Positivity of the normalizer follows from its zeroth term.
    change 0 < (TruncCount.centeredSum c (m-1) (-1))⁻¹
    apply inv_pos.mpr
    dsimp [TruncCount.centeredSum]
    have hs : Summable (fun j ↦ c j * Real.exp (-1 * ((j : ℝ) - (m-1)))) := by
      let q := exp (-1 : ℝ)
      have hq : q ∈ Ioo (0 : ℝ) 1 := ⟨exp_pos _, exp_lt_one_iff.mpr (by norm_num)⟩
      have hS : Summable (fun j : ℕ ↦ c j * q^j) := by
        by_cases he : r = 0
        · simp only [c, ite_eq_left he]
          apply (summable_geometric_of_lt_one hq.1.le hq.2).of_nonneg_of_le
          · intro j
            exact mul_nonneg (by unfold TruncCount.logSeriesCarrier; positivity)
              (pow_nonneg hq.1.le _)
          · intro j
            have h : TruncCount.logSeriesCarrier j ≤ 1 :=
              (div_le_one (by positivity)).mpr (by linarith [Nat.cast_nonneg (α := ℝ) j])
            simpa only [one_mul] using mul_le_mul_of_nonneg_right h (pow_nonneg hq.1.le j)
        · have hball : q ∈ Metric.eball (0 : ℝ) 1 := by
            rw [← ENNReal.ofReal_one, Metric.eball_ofReal, Metric.mem_ball, Real.dist_eq,
              sub_zero, abs_of_pos hq.1]
            exact hq.2
          have h : Summable (fun j : ℕ ↦ TruncCount.negBinomCarrier r j * q^j) := by
            simpa [TruncCount.negBinomCarrier, FormalMultilinearSeries.ofScalars, smul_eq_mul] using
              ((Real.one_div_one_sub_rpow_hasFPowerSeriesOnBall_zero r).hasSum_sub hball).summable
          have h' := ((summable_nat_add_iff 1).mpr h).mul_left q⁻¹
          convert h' using 1
          ext j
          simp only [c, ite_eq_right he, Nat.add_comm 1 j, pow_succ]
          field_simp [hq.1.ne']
      convert hS.mul_right (exp (m-1)) using 1
      ext j
      rw [show (-1 : ℝ) * ((j : ℝ) - (m-1)) = -1 * j + (m-1) by ring,
        exp_add, mul_comm (-1 : ℝ) (j : ℝ), exp_nat_mul]
      exact (mul_assoc _ _ _).symm
    exact (mul_pos (hc 0) (Real.exp_pos _)).trans_le (hs.le_tsum 0 (fun j _ ↦ hnn (-1) j))

/-- Binom, size and p0+size, zero:modified criterion and uniqueness
at positive held size, including the flat size-one slice. -/
theorem binom_modified {N z k : ℕ} (hN : 0 < N) (hn : 0 < z+k)
    {m : ℝ} (hm : m ∈ Icc (1 : ℝ) N) (hold : Option ℝ) {p : Sum ℝ Bool} {w : ℝ}
    (hp : p ∈ TruncCount.finiteCountDomain true true) (hw : w ∈ weightDomain hold) :
    let D := TruncCount.finiteCountDomain true true
    let F := TruncCount.finiteCountLikelihood 1 N (fun j ↦ (N.choose j : ℝ)) m
    (IsMaxOn (modified z k (fun q ↦ F q ^ k)) (D ×ˢ weightDomain hold) (p,w) ∧
      0 < modified z k (fun q ↦ F q ^ k) (p,w)) ↔
    w = chosenWeight z k hold ∧ 0 < bernoulli z k w ∧
      (∀ _ : k ≠ 0, ∀ h : 1 < N,
        p = Classical.choose (TruncCount.binom_zt_fit h hm).exists) := by
  by_cases h : 1 < N
  · dsimp only
    rw [regular_modified hn hold ?_ ⟨Sum.inl 0,trivial,Real.exp_pos _⟩
      (fun _ ↦ TruncCount.binom_zt_fit h hm) hp hw]
    · exact and_congr_right fun _ ↦ and_congr_right fun _ ↦ forall_congr' fun _ ↦
        ⟨fun H _ ↦ H, fun H ↦ H h⟩
    · rintro (t | (_ | _)) _
      · exact (Real.exp_pos _).le
      · change 0 ≤ if m = (↑(1 : ℕ) : ℝ) then (↑(N.choose 1) : ℝ)⁻¹ else 0
        split_ifs <;> positivity
      · change 0 ≤ if m = (N : ℝ) then (↑(N.choose N) : ℝ)⁻¹ else 0
        split_ifs <;> positivity
  · have he : N = 1 := by omega
    subst N
    have he : m = 1 := le_antisymm (by simpa using hm.2) hm.1
    subst m
    dsimp only
    simp only [TruncCount.binom_zt_size_one, one_pow]
    rw [modified_fit hn hold (fun _ _ ↦ zero_le_one) (fun _ ↦ ⟨p,hp,one_pos⟩) hp hw]
    have hf : IsMaxOn (fun _ : Sum ℝ Bool ↦ (1 : ℝ))
        (TruncCount.finiteCountDomain true true) p := fun _ _ ↦ le_rfl
    simp [hf]

end UnivariateML.Zero6

end
