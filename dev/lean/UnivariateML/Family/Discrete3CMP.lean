module

public import UnivariateML.Generic.Score
public import Mathlib.Analysis.Calculus.SmoothSeries
public import Mathlib.Analysis.Normed.Group.Tannery
public import UnivariateML.Family.CMP
public import UnivariateML.Family.Discrete

set_option linter.style.header false

/-! CMP with held dispersion. At zero dispersion the family is geometric.
At positive dispersion the strictly increasing mean map identifies the
unique rate, or the zero-rate endpoint for an all-zero sample. -/

@[expose] public section

namespace UnivariateML.Discrete3CMP

open Set Real Filter
open scoped Topology
open scoped BigOperators

noncomputable section

private theorem geometric_likelihood {n : ℕ} (x : Fin n → ℕ) {r : ℝ} (hr : r ∈ Ico 0 1) :
    CMP.likelihood x r 0 =
      Discrete.Binomial.likelihood n (∑ i, (x i : ℝ)) (1-r) := by
  rcases eq_or_lt_of_le hr.1 with h0 | hp
  · subst r
    have hs : (∑ i, (x i : ℝ)) = 0 ↔ ∀ i, x i = 0 := by
      rw [Finset.sum_eq_zero_iff_of_nonneg (fun i _ ↦ Nat.cast_nonneg (x i))]
      simp
    by_cases hx : ∀ i, x i = 0 <;> simp [CMP.likelihood, Discrete.Binomial.likelihood, hs, hx]
  · have h1 : r < 1 := hr.2
    have hsub : 0 < 1-r := sub_pos.mpr h1
    have hw (k : ℕ) : CMP.weight (log r) 0 k = r ^ k := by
      unfold CMP.weight
      rw [zero_mul, sub_zero, mul_comm, exp_nat_mul, exp_log hp]
    rw [CMP.likelihood, ite_eq_right hp.ne', Discrete.Binomial.likelihood]
    simp only [hw, tsum_geometric_of_lt_one hp.le h1, div_inv_eq_mul]
    rw [ite_eq_right (by
      push Not
      exact ⟨fun h ↦ (ne_of_gt hsub h).elim, fun h ↦ (hp.ne' (by linarith)).elim⟩)]
    rw [show 1 - (1 - r) = r by ring]
    have hsum : (n : ℝ) * log (1-r) + (∑ i, (x i : ℝ)) * log r =
        ∑ i, ((x i : ℝ) * log r + log (1-r)) := by
      simp [Finset.sum_add_distrib, Finset.sum_mul, add_comm]
    rw [hsum, Real.exp_sum]
    apply Finset.prod_congr rfl
    intro i _
    rw [Real.exp_add, Real.exp_nat_mul, Real.exp_log hp, Real.exp_log hsub]

/-- On the zero-dispersion CMP subfamily the unique MLE is the geometric
ratio (S/(n+S)), including the rate-zero atom. -/
theorem held_zero_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r : ℝ} (hr : r ∈ Ico 0 1) :
    IsMaxOn (fun q ↦ CMP.likelihood x q 0) (Ico 0 1) r ↔
      r = (∑ i, (x i : ℝ)) / (n + ∑ i, (x i : ℝ)) := by
  let S : ℝ := ∑ i, (x i : ℝ)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hS : 0 ≤ S := Finset.sum_nonneg (fun i _ ↦ Nat.cast_nonneg (x i))
  have hden : 0 < (n : ℝ) + S := add_pos_of_pos_of_nonneg hn' hS
  let r₀ := S / (n+S)
  have hr₀ : r₀ ∈ Ico (0 : ℝ) 1 :=
    ⟨div_nonneg hS hden.le, (div_lt_one hden).mpr (by linarith)⟩
  have hopt : 1-r₀ = (n : ℝ) / (n+S) := by dsimp [r₀]; field_simp; ring
  have hmem {q : ℝ} (hq : q ∈ Ico (0 : ℝ) 1) : 1-q ∈ Icc (0 : ℝ) 1 :=
    ⟨sub_nonneg.mpr hq.2.le, sub_le_self _ hq.1⟩
  have hmax := (Discrete.Binomial.fixed_size_mle hn'.le hS hden _ (hmem hr₀)).mpr hopt
  have hR : IsMaxOn (fun q ↦ CMP.likelihood x q 0) (Ico 0 1) r₀ := by
    intro q hq
    change CMP.likelihood x q 0 ≤ CMP.likelihood x r₀ 0
    rw [geometric_likelihood x hq, geometric_likelihood x hr₀]
    exact hmax (hmem hq)
  refine ⟨fun hm ↦ ?_, fun h ↦ h ▸ hR⟩
  have hvalue : Discrete.Binomial.likelihood n S (1-r₀) ≤
      Discrete.Binomial.likelihood n S (1-r) := by
    simpa only [geometric_likelihood x hr, geometric_likelihood x hr₀] using
      (show CMP.likelihood x r₀ 0 ≤ CMP.likelihood x r 0 from hm hr₀)
  have he := (Discrete.Binomial.fixed_size_mle hn'.le hS hden _ (hmem hr)).mp
    (fun q hq ↦ (hmax hq).trans hvalue)
  change r = r₀
  linarith

private def momentTerm (d : ℕ) (ν η : ℝ) (k : ℕ) : ℝ :=
  (k : ℝ)^d*CMP.weight η ν k

private def moment (d : ℕ) (ν η : ℝ) : ℝ :=
  ∑' k, momentTerm d ν η k

private theorem momentTerm_nonneg (d : ℕ) (ν η : ℝ) (k : ℕ) :
    0 ≤ momentTerm d ν η k := by unfold momentTerm CMP.weight; positivity

private theorem momentTerm_mono (d : ℕ) (ν : ℝ) (k : ℕ) :
    Monotone (fun η ↦ momentTerm d ν η k) := by
  intro a b hab
  unfold momentTerm CMP.weight
  apply mul_le_mul_of_nonneg_left (exp_le_exp.mpr _) (by positivity)
  exact sub_le_sub_right (mul_le_mul_of_nonneg_right hab (Nat.cast_nonneg k)) _

private theorem moment_summable (d : ℕ) {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    Summable (momentTerm d ν η) := by
  apply Summable.of_nonneg_of_le (momentTerm_nonneg d ν η) _
    (CMP.summable_weight (η := η+(d : ℝ)) (Or.inl hν))
  intro k
  have hk : (k : ℝ) ≤ exp (k : ℝ) := by linarith [add_one_le_exp (k : ℝ)]
  have hpow : (k : ℝ)^d ≤ exp ((d : ℝ)*(k : ℝ)) := by
    rw [exp_nat_mul]
    exact pow_le_pow_left₀ (Nat.cast_nonneg k) hk d
  have he : exp ((d : ℝ)*(k : ℝ))*CMP.weight η ν k = CMP.weight (η+(d : ℝ)) ν k := by
    unfold CMP.weight
    rw [← exp_add]
    congr 1
    ring
  unfold momentTerm
  rw [← he]
  exact mul_le_mul_of_nonneg_right hpow (exp_pos _).le

private theorem momentTerm_deriv (d k : ℕ) (ν η : ℝ) :
    HasDerivAt (fun u ↦ momentTerm d ν u k) (momentTerm (d+1) ν η k) η := by
  have h := (((hasDerivAt_id η).mul_const (k : ℝ)).sub_const
    (ν*CMP.logFactorial k)).exp.const_mul ((k : ℝ)^d)
  convert h using 1
  · rfl
  · simp only [momentTerm, CMP.weight, pow_succ, id_eq, one_mul]
    ring

private theorem moment_deriv (d : ℕ) {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    HasDerivAt (moment d ν) (moment (d+1) ν η) η := by
  let D : Set ℝ := Ioo (η-1) (η+1)
  have hη : η ∈ D := by
    change η-1 < η ∧ η < η+1
    constructor <;> linarith
  have hb (k : ℕ) (t : ℝ) (ht : t ∈ D) :
      ‖momentTerm (d+1) ν t k‖ ≤ momentTerm (d+1) ν (η+1) k := by
    rw [Real.norm_of_nonneg (momentTerm_nonneg (d+1) ν t k)]
    exact momentTerm_mono (d+1) ν k (show t ≤ η+1 from ht.2.le)
  unfold moment
  exact hasDerivAt_tsum_of_isPreconnected (t := D)
    (g := fun k : ℕ ↦ fun u : ℝ ↦ momentTerm d ν u k)
    (g' := fun k : ℕ ↦ fun u : ℝ ↦ momentTerm (d+1) ν u k)
    (moment_summable (d+1) hν (η+1))
    isOpen_Ioo (convex_Ioo (η-1) (η+1)).isPreconnected
    (fun k t _ ↦ momentTerm_deriv d k ν t) hb hη (moment_summable d hν η) hη

private theorem moment_zero_eq (ν η : ℝ) :
    moment 0 ν η = ∑' k, CMP.weight η ν k := by
  simp [moment, momentTerm]

private theorem weight_zero (ν η : ℝ) : CMP.weight η ν 0 = 1 := by
  simp [CMP.weight, CMP.logFactorial]

private theorem moment_zero_ge_one {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    1 ≤ moment 0 ν η := by
  have h := (moment_summable 0 hν η).le_tsum 0 (fun k _ ↦ momentTerm_nonneg 0 ν η k)
  rw [moment_zero_eq]
  simpa only [momentTerm, pow_zero, one_mul, weight_zero] using h

private theorem moment_zero_pos {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    0 < moment 0 ν η := zero_lt_one.trans_le (moment_zero_ge_one hν η)

private theorem moment_one_pos {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    0 < moment 1 ν η := by
  have h := (moment_summable 1 hν η).le_tsum 1 (fun k _ ↦ momentTerm_nonneg 1 ν η k)
  exact (show 0 < momentTerm 1 ν η 1 by simp [momentTerm, CMP.weight, exp_pos]).trans_le h

/-- CMP mean at held dispersion and log rate. -/
@[no_expose] def modelMean (ν η : ℝ) : ℝ := moment 1 ν η / moment 0 ν η

private theorem modelMean_pos {ν : ℝ} (hν : 0 < ν) (η : ℝ) : 0 < modelMean ν η :=
  div_pos (moment_one_pos hν η) (moment_zero_pos hν η)

private theorem modelMean_deriv {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    HasDerivAt (modelMean ν)
      ((moment 2 ν η*moment 0 ν η-(moment 1 ν η)^2)/(moment 0 ν η)^2) η := by
  convert (moment_deriv 1 hν η).div (moment_deriv 0 hν η) (moment_zero_pos hν η).ne'
    using 1
  · rfl
  · rw [pow_two]

private theorem modelMean_deriv_pos {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    0 < deriv (modelMean ν) η := by
  let m := modelMean ν η
  have hm : 0 < m := modelMean_pos hν η
  have hZ := moment_zero_pos hν η
  let g := fun k : ℕ ↦ momentTerm 2 ν η k - (2*m)*momentTerm 1 ν η k +
    m^2*momentTerm 0 ν η k
  have ge (k : ℕ) : g k = ((k : ℝ)-m)^2*CMP.weight η ν k := by
    unfold g momentTerm
    ring
  have gs : Summable g :=
    ((moment_summable 2 hν η).sub ((moment_summable 1 hν η).mul_left (2*m))).add
      ((moment_summable 0 hν η).mul_left (m^2))
  have hG : 0 < ∑' k, g k := by
    have hh : 0 < g 0 := by
      rw [ge, Nat.cast_zero, zero_sub, neg_sq, weight_zero, mul_one]
      exact sq_pos_of_pos hm
    exact hh.trans_le (gs.le_tsum 0 fun k _ ↦ by rw [ge]; unfold CMP.weight; positivity)
  have hsum : (∑' k, g k) = moment 2 ν η - (2*m)*moment 1 ν η + m^2*moment 0 ν η := by
    unfold g moment
    rw [((moment_summable 2 hν η).sub ((moment_summable 1 hν η).mul_left _)).tsum_add
      ((moment_summable 0 hν η).mul_left _),
      (moment_summable 2 hν η).tsum_sub ((moment_summable 1 hν η).mul_left _),
      tsum_mul_left, tsum_mul_left]
  have he : moment 0 ν η*(moment 2 ν η-(2*m)*moment 1 ν η+m^2*moment 0 ν η) =
      moment 2 ν η*moment 0 ν η-(moment 1 ν η)^2 := by
    dsimp [m, modelMean]
    field_simp [hZ.ne']
    ring
  rw [(modelMean_deriv hν η).deriv, ← he, ← hsum]
  exact div_pos (mul_pos hZ hG) (sq_pos_of_pos hZ)

/-- At every positive dispersion the CMP mean is strictly increasing in log rate. -/
theorem modelMean_strictMono {ν : ℝ} (hν : 0 < ν) :
    StrictMono (modelMean ν) :=
  strictMono_of_deriv_pos (modelMean_deriv_pos hν)

/-- The mean tends to zero as the log rate tends to minus infinity. -/
theorem modelMean_atBot {ν : ℝ} (hν : 0 < ν) :
    Tendsto (modelMean ν) atBot (𝓝 0) := by
  have hterm (k : ℕ) : Tendsto (fun η ↦ momentTerm 1 ν η k) atBot (𝓝 0) := by
    cases k with
    | zero => simp [momentTerm]
    | succ k =>
      have hk : (0 : ℝ) < (k.succ : ℕ) := by positivity
      have h := tendsto_atBot_add_const_right atBot (-ν*CMP.logFactorial k.succ)
        (tendsto_id.atBot_mul_const hk)
      have he : Tendsto (fun η : ℝ ↦ CMP.weight η ν k.succ) atBot (𝓝 0) := by
        simpa only [CMP.weight, sub_eq_add_neg, neg_mul, Function.comp_def, id_eq] using
          tendsto_exp_atBot.comp h
      simpa only [momentTerm, mul_zero] using he.const_mul ((k.succ : ℝ)^1)
  have h : Tendsto (moment 1 ν) atBot (𝓝 0) := by
    have ht := tendsto_tsum_of_dominated_convergence (moment_summable 1 hν 0) hterm
      (by
        filter_upwards [eventually_le_atBot (0 : ℝ)] with η hη
        intro k
        rw [Real.norm_of_nonneg (momentTerm_nonneg 1 ν η k)]
        exact momentTerm_mono 1 ν k hη)
    change Tendsto (fun η ↦ ∑' k, momentTerm 1 ν η k) atBot (𝓝 0)
    simpa using ht
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le tendsto_const_nhds h
  · exact fun η ↦ (modelMean_pos hν η).le
  · exact fun η ↦ div_le_self (moment_one_pos hν η).le (moment_zero_ge_one hν η)

private def atom (ν η : ℝ) (k : ℕ) : ℝ :=
  CMP.weight η ν k / moment 0 ν η

private theorem atom_nonneg {ν : ℝ} (hν : 0 < ν) (η : ℝ) (k : ℕ) :
    0 ≤ atom ν η k := div_nonneg (exp_pos _).le (moment_zero_pos hν η).le

private theorem atom_summable {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    Summable (atom ν η) :=
  (CMP.summable_weight (Or.inl hν)).div_const _

private theorem atom_sum_one {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    (∑' k, atom ν η k) = 1 := by
  unfold atom
  rw [tsum_div_const, ← moment_zero_eq, div_self (moment_zero_pos hν η).ne']

private theorem atom_atTop {ν : ℝ} (hν : 0 < ν) {k L : ℕ} (hk : k < L) :
    Tendsto (fun η ↦ atom ν η k) atTop (𝓝 0) := by
  let d := (k : ℝ)-(L : ℝ)
  have hd : d < 0 := sub_neg.mpr (by exact_mod_cast hk)
  have ht := tendsto_atBot_add_const_right atTop
    (-(ν*(CMP.logFactorial k-CMP.logFactorial L))) (tendsto_id.atTop_mul_const_of_neg hd)
  let b := fun η : ℝ ↦ exp (η*d-(ν*(CMP.logFactorial k-CMP.logFactorial L)))
  have hb : Tendsto b atTop (𝓝 0) := by
    simpa only [b, sub_eq_add_neg, Function.comp_def, id_eq] using tendsto_exp_atBot.comp ht
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le tendsto_const_nhds hb
  · exact fun _ ↦ atom_nonneg hν _ _
  · intro η
    have hL : CMP.weight η ν L ≤ moment 0 ν η := by
      rw [moment_zero_eq]
      exact (CMP.summable_weight (Or.inl hν)).le_tsum L (fun _ _ ↦ (exp_pos _).le)
    have he : CMP.weight η ν k / CMP.weight η ν L = b η := by
      unfold b CMP.weight
      rw [← exp_sub]
      congr 1
      dsimp [d]
      ring
    rw [← he]
    exact div_le_div_of_nonneg_left (exp_pos _).le (exp_pos _) hL

private theorem modelMean_ge_truncated {ν : ℝ} (hν : 0 < ν) (η : ℝ) (L : ℕ) :
    (L : ℝ)*(1-∑ k ∈ Finset.range L, atom ν η k) ≤ modelMean ν η := by
  classical
  let f := fun k : ℕ ↦ if k < L then atom ν η k else 0
  have hnot (k : ℕ) (hk : k ∉ Finset.range L) : f k = 0 :=
    ite_eq_right (fun h ↦ hk (Finset.mem_range.mpr h))
  have hfSum : (∑' k, f k) = ∑ k ∈ Finset.range L, atom ν η k := by
    rw [tsum_eq_sum (s := Finset.range L) hnot]
    exact Finset.sum_congr rfl (fun k hk ↦ ite_eq_left (Finset.mem_range.mp hk))
  have hf : Summable f := summable_of_ne_finset_zero (s := Finset.range L) hnot
  have hs : Summable (fun k : ℕ ↦ (k : ℝ)*atom ν η k) := by
    have h := (moment_summable 1 hν η).div_const (moment 0 ν η)
    simpa only [momentTerm, pow_one, atom, mul_div_assoc] using h
  have hm : (∑' k : ℕ, (k : ℝ)*atom ν η k) = modelMean ν η := by
    simp only [atom, ← mul_div_assoc, tsum_div_const, modelMean, moment, momentTerm, pow_one]
  have hbound (k : ℕ) : (L : ℝ)*(atom ν η k-f k) ≤ (k : ℝ)*atom ν η k := by
    by_cases hk : k < L
    · rw [show f k = atom ν η k from ite_eq_left hk, sub_self, mul_zero]
      exact mul_nonneg (Nat.cast_nonneg _) (atom_nonneg hν η k)
    · have hL : (L : ℝ) ≤ k := by exact_mod_cast Nat.le_of_not_gt hk
      rw [show f k = 0 from ite_eq_right hk, sub_zero]
      exact mul_le_mul_of_nonneg_right hL (atom_nonneg hν η k)
  have hh := (((atom_summable hν η).sub hf).mul_left (L : ℝ)).tsum_le_tsum hbound hs
  rw [tsum_mul_left, (atom_summable hν η).tsum_sub hf, atom_sum_one hν η, hfSum, hm] at hh
  exact hh

/-- The mean tends to infinity as the log rate tends to infinity. -/
theorem modelMean_atTop {ν : ℝ} (hν : 0 < ν) :
    Tendsto (modelMean ν) atTop atTop := by
  apply tendsto_atTop.mpr
  intro M
  obtain ⟨L, hL⟩ := exists_nat_gt M
  have h : Tendsto (fun η ↦ ∑ k ∈ Finset.range L, atom ν η k) atTop (𝓝 0) := by
    simpa only [Finset.sum_const_zero] using
      tendsto_finsetSum (Finset.range L) (fun k hk ↦ atom_atTop hν (Finset.mem_range.mp hk))
  have hb : Tendsto (fun η ↦ (L : ℝ)*(1-∑ k ∈ Finset.range L, atom ν η k)) atTop
      (𝓝 (L : ℝ)) := by
    simpa only [sub_zero, mul_one] using ((tendsto_const_nhds (x := (1 : ℝ))).sub h).const_mul (L : ℝ)
  filter_upwards [hb.eventually (Ioi_mem_nhds hL)] with η hη
  exact hη.le.trans (modelMean_ge_truncated hν η L)

private def heldLogLik (ν m η : ℝ) : ℝ := η*m-log (moment 0 ν η)

private theorem heldLogLik_opt {ν m : ℝ} (hν : 0 < ν) (hm : 0 < m) :
    (∃! η, modelMean ν η = m) ∧
      ∀ η, IsMaxOn (heldLogLik ν m) univ η ↔ modelMean ν η = m := by
  let score := fun η ↦ m-modelMean ν η
  have hd (η : ℝ) : HasDerivAt (heldLogLik ν m) (score η) η := by
    change HasDerivAt (fun u : ℝ ↦ u*m-log (moment 0 ν u))
      (m-moment 1 ν η/moment 0 ν η) η
    have h := ((hasDerivAt_id η).mul_const m).sub
      ((moment_deriv 0 hν η).log (moment_zero_pos hν η).ne')
    convert h using 1
    · rfl
    · simp
  have hc : Continuous score :=
    continuous_const.sub (continuous_iff_continuousAt.mpr
      fun η ↦ (modelMean_deriv hν η).continuousAt)
  have hs : StrictAnti score := fun _ _ hab ↦ sub_lt_sub_left (modelMean_strictMono hν hab) _
  obtain ⟨a, ha⟩ := ((modelMean_atBot hν).eventually (Iio_mem_nhds hm)).exists
  obtain ⟨b, hb⟩ := ((modelMean_atTop hν).eventually
    (eventually_ge_atTop (m+1))).exists
  have hsa : 0 < score a := sub_pos.mpr ha
  have hsb : score b < 0 := by
    change m-modelMean ν b < 0
    have h : m+1 ≤ modelMean ν b := hb
    linarith
  have hab : a ≤ b := le_of_not_gt (fun h ↦ (not_lt_of_ge (hsb.le.trans hsa.le)) (hs h))
  have h := Generic.existsUnique_score_zero_of_signs (convex_univ (𝕜 := ℝ))
    (fun η _ ↦ hd η) hc.continuousOn (hs.strictAntiOn univ)
    (mem_univ a) (mem_univ b) hab hsa hsb
  have he (η : ℝ) : score η = 0 ↔ modelMean ν η = m := sub_eq_zero.trans eq_comm
  simpa only [mem_univ, true_and, true_implies, he] using h

private theorem likelihood_exp_eq {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 < ν) (η : ℝ) :
    CMP.likelihood x (exp η) ν =
      exp ((n : ℝ)*heldLogLik ν (Discrete.avg x) η-ν*(∑ i, CMP.logFactorial (x i))) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hZ := moment_zero_pos hν η
  rw [CMP.likelihood, ite_eq_right (exp_pos η).ne', log_exp, ← moment_zero_eq]
  have he (i : Fin n) : CMP.weight η ν (x i)/moment 0 ν η =
      exp (η*(x i : ℝ)-ν*CMP.logFactorial (x i)-log (moment 0 ν η)) := by
    rw [exp_sub, exp_log hZ, CMP.weight]
  simp_rw [he]
  rw [← Real.exp_sum]
  congr 1
  simp only [heldLogLik, Discrete.avg, Finset.sum_sub_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp [hn']
  ring

/-- Held positive dispersion always gives a unique CMP rate. It is zero
on all-zero samples and matches the empirical mean on positive data. -/
theorem held_positive_dispersion {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {ν : ℝ} (hν : 0 < ν) :
    (∃! r, r ∈ Ici (0 : ℝ) ∧ IsMaxOn (fun q ↦ CMP.likelihood x q ν) (Ici 0) r) ∧
    ∀ r ∈ Ici (0 : ℝ), IsMaxOn (fun q ↦ CMP.likelihood x q ν) (Ici 0) r ↔
      (r = 0 ∧ ∀ i, x i = 0) ∨
        (0 < r ∧ modelMean ν (log r) = Discrete.avg x) := by
  let f := fun q ↦ CMP.likelihood x q ν
  have hmem (r : ℝ) (hr : 0 ≤ r) : (r,ν) ∈ CMP.rateDomain := by
    refine ⟨hν.le, ?_⟩
    rcases eq_or_lt_of_le hr with h | h
    · exact Or.inl h.symm
    · exact Or.inr ⟨h, Or.inl hν⟩
  have hzero : (0 : ℝ) ∈ Ici 0 := show (0 : ℝ) ≤ 0 from le_rfl
  by_cases hx : ∀ i, x i = 0
  · have hb := (CMP.all_zero_maximizers hn x hx (0,ν) (hmem 0 le_rfl)).mpr rfl
    have hfit (r : ℝ) (hr : r ∈ Ici (0 : ℝ)) :
        IsMaxOn f (Ici 0) r ↔ r = 0 := by
      constructor
      · intro h
        apply (CMP.all_zero_maximizers hn x hx (r,ν) (hmem r hr)).mp
        intro p hp
        have he : f 0 ≤ f r := h hzero
        have he' : CMP.likelihood x p.1 p.2 ≤ f 0 := hb hp
        exact he'.trans he
      · rintro rfl q hq
        exact hb (hmem q hq)
    refine ⟨⟨0, ⟨hzero, (hfit 0 hzero).mpr rfl⟩,
      fun r hr ↦ (hfit r hr.1).mp hr.2⟩, ?_⟩
    intro r hr
    rw [hfit r hr]
    have hmean := (Discrete.avg_eq_zero_iff x).mpr hx
    rw [hmean]
    constructor
    · exact fun hz ↦ Or.inl ⟨hz, hx⟩
    · rintro (⟨hz, _⟩ | ⟨hp, he⟩)
      · exact hz
      · exact ((modelMean_pos hν (log r)).ne' he).elim
  · have hmean : 0 < Discrete.avg x := by
      have hne := fun h ↦ hx ((Discrete.avg_eq_zero_iff x).mp h)
      have hge : 0 ≤ Discrete.avg x := by
        unfold Discrete.avg
        exact div_nonneg (Finset.sum_nonneg (fun i _ ↦ Nat.cast_nonneg _)) (Nat.cast_nonneg _)
      exact lt_of_le_of_ne hge (Ne.symm hne)
    obtain ⟨⟨η₀, hη₀, hu⟩, hchar⟩ := heldLogLik_opt hν hmean
    have hfzero : f 0 = 0 := by simp [f, CMP.likelihood, hx]
    have hcompare (a b : ℝ) :
        f (exp a) ≤ f (exp b) ↔
          heldLogLik ν (Discrete.avg x) a ≤ heldLogLik ν (Discrete.avg x) b := by
      have hn' : (0 : ℝ) < n := by exact_mod_cast hn
      unfold f
      rw [likelihood_exp_eq hn x hν a, likelihood_exp_eq hn x hν b, exp_le_exp,
        sub_le_sub_iff_right]
      exact mul_le_mul_iff_right₀ hn'
    have hposFit (r : ℝ) (hr : 0 < r) :
        IsMaxOn f (Ici 0) r ↔ modelMean ν (log r) = Discrete.avg x := by
      rw [← hchar]
      constructor
      · intro h a _
        apply (hcompare a (log r)).mp
        rw [exp_log hr]
        exact h (exp_pos a).le
      · intro h q hq
        change f q ≤ f r
        rcases eq_or_lt_of_le (show 0 ≤ q from hq) with he | hp
        · rw [← he, hfzero]
          have h := exp_pos ((n : ℝ)*heldLogLik ν (Discrete.avg x) (log r)-
            ν*(∑ i, CMP.logFactorial (x i)))
          change 0 ≤ f r
          rw [← exp_log hr, show f (exp (log r)) = CMP.likelihood x (exp (log r)) ν from rfl,
            likelihood_exp_eq hn x hν]
          exact h.le
        · have hh := (hcompare (log q) (log r)).mpr (h (mem_univ _))
          rwa [exp_log hp, exp_log hr] at hh
    have hno : ¬ IsMaxOn f (Ici 0) 0 := by
      intro h
      have hh := h (show exp η₀ ∈ Ici (0 : ℝ) from (exp_pos η₀).le)
      change f (exp η₀) ≤ f 0 at hh
      rw [hfzero, show f (exp η₀) = CMP.likelihood x (exp η₀) ν from rfl,
        likelihood_exp_eq hn x hν] at hh
      exact (exp_pos _).not_ge hh
    have hfit (r : ℝ) (hr : r ∈ Ici (0 : ℝ)) :
        IsMaxOn f (Ici 0) r ↔ 0 < r ∧ modelMean ν (log r) = Discrete.avg x := by
      rcases eq_or_lt_of_le (show 0 ≤ r from hr) with he | hp
      · subst r
        simp [hno]
      · simp only [hp, true_and, hposFit r hp]
    have hη₀' : modelMean ν (log (exp η₀)) = Discrete.avg x := by rwa [log_exp]
    refine ⟨⟨exp η₀, ⟨(exp_pos _).le,
      (hfit (exp η₀) (exp_pos _).le).mpr ⟨exp_pos _, hη₀'⟩⟩, ?_⟩, ?_⟩
    · intro r hr
      obtain ⟨hp, he⟩ := (hfit r hr.1).mp hr.2
      have h := hu (log r) he
      rw [← h, exp_log hp]
    · intro r hr
      simpa only [hx, and_false, false_or] using hfit r hr

end

end UnivariateML.Discrete3CMP

end
