module

public import UnivariateML.Family.Trunc3CMPRay
public import UnivariateML.Family.Trunc3CMP
public import Mathlib.Analysis.Calculus.SmoothSeries

/-! Held-rate CMP moment matching and excluded geometric edge on the positive ray. -/

@[expose] public section

open Set Real Filter UnivariateML.Trunc3CMPRay
open scoped Topology

namespace UnivariateML.Trunc5CMPRay

noncomputable section

private theorem logFact_nonneg (k : ℕ) : 0 ≤ CMP.logFactorial k :=
  log_nonneg (by exact_mod_cast Nat.factorial_pos k)

private theorem term_sum (η m : ℝ) {t : ℝ} (ht : t < 0) :
    Summable (term η m t) := by
  have h := ((summable_nat_add_iff 1).mpr
    (CMP.summable_weight (η := η) (Or.inl (neg_pos.mpr ht)))).mul_right (exp (-t*m))
  convert h using 1
  ext k
  rw [term, CMP.weight, ← exp_add]
  congr 1
  ring

private theorem deriv_bound (η m : ℝ) {t : ℝ} (ht : t < 0) :
    ∃ bound : ℕ → ℝ, Summable bound ∧
      ∀ k s, s ∈ Ioo (2*t) (t/2) →
        ‖term η m s k * (CMP.logFactorial (k+1)-m)‖ ≤ bound k := by
  let δ := -t/4
  have hδ : 0 < δ := by dsimp [δ]; linarith
  let C := exp (2*|t| *|m|) * (1/δ + |m|)
  refine ⟨fun k ↦ C * term η 0 (t/4) k, (term_sum η 0 (by linarith)).mul_left C, ?_⟩
  intro k s hs
  let h := CMP.logFactorial (k+1)
  have hh : 0 ≤ h := logFact_nonneg _
  have hsabs : |s| ≤ 2*|t| := by
    rw [abs_of_neg ht, abs_of_neg (hs.2.trans (by linarith))]
    linarith [hs.1]
  have hsm : -s*m ≤ 2*|t| *|m| := calc
    -s*m ≤ |-s*m| := le_abs_self _
    _ = |s| *|m| := by rw [abs_mul, abs_neg]
    _ ≤ _ := mul_le_mul_of_nonneg_right hsabs (abs_nonneg _)
  have hd : |h-m| ≤ (1/δ + |m|)*exp (δ*h) := by
    have hexp : 1 ≤ exp (δ*h) := one_le_exp (mul_nonneg hδ.le hh)
    have hle : h ≤ (1/δ)*exp (δ*h) := by
      rw [one_div, ← div_eq_inv_mul, le_div_iff₀ hδ]
      linarith [add_one_le_exp (δ*h)]
    calc
      |h-m| ≤ h+|m| := by simpa [abs_of_nonneg hh] using abs_sub_le h 0 m
      _ ≤ _ := by
        rw [add_mul]
        linarith [mul_le_mul_of_nonneg_left hexp (abs_nonneg m)]
  rw [Real.norm_eq_abs, abs_mul, abs_of_pos (show 0 < term η m s k from exp_pos _)]
  change exp (η * (k+1 : ℕ) + s*(h-m)) * |h-m| ≤ _
  calc
    _ ≤ exp (η * (k+1 : ℕ) + s*(h-m)) * ((1/δ+|m|)*exp (δ*h)) :=
      mul_le_mul_of_nonneg_left hd (exp_pos _).le
    _ = (1/δ+|m|)*exp (η*(k+1 : ℕ) + (s+δ)*h + -s*m) := by
      rw [mul_left_comm, ← exp_add]
      congr 2
      ring
    _ ≤ C * term η 0 (t/4) k := by
      have hh' : (s+δ)*h ≤ (t/4)*h :=
        mul_le_mul_of_nonneg_right (by dsimp [δ]; linarith [hs.2]) hh
      dsimp [C,term]
      simp only [sub_zero]
      rw [mul_comm (exp (2*|t| *|m|)), mul_assoc, ← exp_add]
      apply mul_le_mul_of_nonneg_left (exp_le_exp.mpr _) (by positivity)
      linarith

private theorem centered_deriv (η m : ℝ) {t : ℝ} (ht : t < 0) :
    HasDerivAt (centered η m)
      (∑' k, term η m t k * (CMP.logFactorial (k+1)-m)) t := by
  obtain ⟨b,hb,hbound⟩ := deriv_bound η m ht
  have hmem : t ∈ Ioo (2*t) (t/2) := ⟨by linarith, by linarith⟩
  apply hasDerivAt_tsum_of_isPreconnected hb isOpen_Ioo
    (convex_Ioo _ _).isPreconnected _ hbound hmem (term_sum η m ht) hmem
  intro k s _
  unfold term
  exact (((hasDerivAt_id s).mul_const (CMP.logFactorial (k+1)-m)).const_add
    (η*(k+1 : ℕ))).exp.congr_deriv (by simp [mul_comm])

private theorem factorial_sum (η : ℝ) {t : ℝ} (ht : t < 0) :
    Summable (fun k ↦ term η 0 t k * CMP.logFactorial (k+1)) := by
  obtain ⟨b,hb,hbound⟩ := deriv_bound η 0 ht
  apply Summable.of_norm_bounded hb
  intro k
  simpa only [sub_zero] using hbound k t ⟨by linarith, by linarith⟩

/-- Conditional mean of log factorial on the positive ray. -/
def factorialMean (η t : ℝ) : ℝ :=
  (∑' k, term η 0 t k * CMP.logFactorial (k+1)) / centered η 0 t

/-- CMP, lambda held, `zero:truncated`, criterion. Every optimizer at
positive dispersion matches the empirical mean log factorial. -/
theorem held_rate_interior_match (η m : ℝ) {t : ℝ} (ht : t < 0)
    (H : IsMinOn (centered η m) (domain η) t) :
    factorialMean η t = m := by
  have hloc : IsLocalMin (centered η m) t :=
    H.isLocalMin (mem_of_superset (Iio_mem_nhds ht) (by
      intro s hs
      unfold domain
      split_ifs
      · exact hs.le
      · exact hs))
  have hzero := hloc.hasDerivAt_eq_zero (centered_deriv η m ht)
  have he (k : ℕ) : term η m t k = exp (-t*m) * term η 0 t k := by
    unfold term
    rw [← exp_add]
    congr 1
    ring
  simp only [he, mul_assoc, tsum_mul_left, exp_ne_zero, mul_eq_zero, false_or] at hzero
  simp only [mul_sub] at hzero
  rw [(factorial_sum η ht).tsum_sub ((term_sum η 0 ht).mul_right m),
    tsum_mul_right, ← centered, sub_eq_zero] at hzero
  have hp : 0 < centered η 0 t :=
    (exp_pos _).trans_le ((term_sum η 0 ht).le_tsum 0 fun _ _ ↦ (exp_pos _).le)
  exact (div_eq_iff hp.ne').mpr (by rw [hzero, mul_comm])

/-- CMP, lambda held, `zero:truncated`, failure. At a held rate at least
one, the factorial moment diverges at the excluded zero-dispersion edge. -/
theorem held_rate_excluded_factorial_mean {η : ℝ} (hη : 0 ≤ η) :
    Tendsto (factorialMean η) (𝓝[<] 0) atTop := by
  apply tendsto_atTop.mpr
  intro B
  obtain ⟨n,hn⟩ := exists_nat_gt (exp (2*max B 0 + 1))
  have hlog : 2*max B 0 + 1 < log (n : ℝ) :=
    (lt_log_iff_exp_lt (by linarith [exp_pos (2*max B 0 + 1)])).mpr hn
  have hn' : 0 < n := by exact_mod_cast (show 0 < (n : ℝ) by linarith [exp_pos (2*max B 0 + 1)])
  have ht0 : Tendsto (fun t ↦ ∑ k ∈ Finset.range n, term η 0 t k / centered η 0 t)
      (𝓝[<] 0) (𝓝 0) := by
    simpa using tendsto_finsetSum (Finset.range n)
      (fun k _ ↦ Trunc3CMPRay.held_rate_excluded_geometric hη k)
  filter_upwards [ht0.eventually (gt_mem_nhds (show (0:ℝ) < 1/2 by norm_num)),
    show ∀ᶠ t : ℝ in 𝓝[<] 0, t < 0 from self_mem_nhdsWithin] with t ht htneg
  let p := fun k ↦ term η 0 t k / centered η 0 t
  have hZ : 0 < centered η 0 t :=
    (exp_pos _).trans_le ((term_sum η 0 htneg).le_tsum 0 fun _ _ ↦ (exp_pos _).le)
  have hp : Summable p := (term_sum η 0 htneg).div_const _
  have hs : (∑' k, p k) = 1 := by dsimp [p]; rw [tsum_div_const, ← centered, div_self hZ.ne']
  have hp0 (k : ℕ) : 0 ≤ p k := div_nonneg (exp_pos _).le hZ.le
  have hsum : (∑' k, p (k+n)) = 1 - ∑ k ∈ Finset.range n, p k := by
    have H := hp.sum_add_tsum_nat_add n
    rw [hs] at H
    linarith
  let f := fun k ↦ p k * CMP.logFactorial (k+1)
  have hf : Summable f := by
    convert (factorial_sum η htneg).div_const (centered η 0 t) using 1
    ext k
    dsimp [f,p]
    ring
  have hm : factorialMean η t = ∑' k, f k := by
    dsimp [factorialMean,f,p]
    rw [← tsum_div_const]
    congr 1
    ext k
    ring
  have htail : log (n : ℝ) * (∑' k, p (k+n)) ≤ ∑' k, f (k+n) := by
    rw [← tsum_mul_left]
    apply Summable.tsum_le_tsum _ (((summable_nat_add_iff n).mpr hp).mul_left _)
      ((summable_nat_add_iff n).mpr hf)
    intro k
    have hk : n ≤ k+n+1 := by omega
    have hfac : n ≤ (k+n+1).factorial := hk.trans (Nat.self_le_factorial _)
    rw [mul_comm (log _)]
    exact mul_le_mul_of_nonneg_left
      (log_le_log (by exact_mod_cast hn') (by exact_mod_cast hfac)) (hp0 _)
  have htotal := hf.sum_add_tsum_nat_add n
  have hnon : 0 ≤ ∑ k ∈ Finset.range n, f k :=
    Finset.sum_nonneg fun k _ ↦ mul_nonneg (hp0 k) (logFact_nonneg _)
  rw [hm]
  change (∑ k ∈ Finset.range n, p k) < 1/2 at ht
  rw [hsum] at htail
  have hmax := le_max_left B 0
  have hlogpos : 0 ≤ log (n : ℝ) := by linarith [le_max_right B 0]
  nlinarith [mul_nonneg hlogpos (show 0 ≤ 1/2 - ∑ k ∈ Finset.range n, p k from by linarith)]

/-- CMP, lambda held, `zero:truncated`, criterion and failure. On the
positive ray at held positive rate there is a unique fit precisely when
the sample mean log factorial is positive. An interior fit matches that
mean. The included geometric chart obeys the boundary score test. At
zero empirical statistic there is no maximizing dispersion and the
infinite-dispersion path gives the atom at one. -/
theorem held_rate_positive_window (η : ℝ) {m : ℝ} (hm : 0 ≤ m) :
    let D := domain η
    let L := centered η m
    ((∃! t, t ∈ D ∧ IsMinOn L D t) ↔ 0 < m) ∧
    (∀ t < 0, IsMinOn L D t → factorialMean η t = m) ∧
    (η < 0 → (IsMinOn L D 0 ↔
      (∑' k : ℕ, exp (η*(k+1 : ℕ)) * (CMP.logFactorial (k+1)-m)) ≤ 0)) ∧
    (m = 0 → ¬ ∃ t ∈ D, IsMinOn L D t) ∧
    (∀ j, Tendsto (fun t ↦ CMP.weight η t (j+1) / ∑' k : ℕ, CMP.weight η t (k+1))
      atTop (𝓝 (if j = 0 then 1 else 0))) ∧
    (0 ≤ η → Tendsto (factorialMean η) (𝓝[<] 0) atTop) := by
  intro D L
  refine ⟨held_rate_positive_fit η hm, fun _ ht ↦ held_rate_interior_match η m ht,
    fun hη ↦ held_rate_geometric_edge hη hm, ?_, Trunc3CMP.held_rate_positive_atom η,
    held_rate_excluded_factorial_mean⟩
  rintro rfl ⟨t,ht,H⟩
  have ht' : t ≤ 0 := by
    unfold D domain at ht
    split_ifs at ht
    · exact ht
    · exact ht.le
  let s := t-1
  have hs : s < 0 := by dsimp [s]; linarith
  have hD : s ∈ D := by
    unfold D domain
    split_ifs
    · exact hs.le
    · exact hs
  have hsum : Summable (term η 0 t) := by
    rcases ht'.lt_or_eq with hlt | rfl
    · exact term_sum η 0 hlt
    · have hη : η < 0 := by
        by_contra hη
        simp [D,domain,hη] at ht
      have he : term η 0 0 = fun k ↦ CMP.weight η 0 (k+1) := by
        funext k
        simp [term,CMP.weight]
      rw [he]
      exact (summable_nat_add_iff 1).mpr (CMP.summable_weight (Or.inr ⟨rfl,hη⟩))
  have hle (k : ℕ) : term η 0 s k ≤ term η 0 t k := by
    simp only [term,sub_zero,exp_le_exp,add_le_add_iff_left]
    exact mul_le_mul_of_nonneg_right (by dsimp [s]; linarith) (logFact_nonneg _)
  have hlt : term η 0 s 1 < term η 0 t 1 := by
    simp only [term,sub_zero,exp_lt_exp,add_lt_add_iff_left]
    exact mul_lt_mul_of_pos_right (by dsimp [s]; linarith)
      (by apply log_pos; norm_num [Nat.factorial, CMP.logFactorial])
  exact (Summable.tsum_lt_tsum hle hlt (term_sum η 0 hs) hsum).not_ge (H hD)

end

end UnivariateML.Trunc5CMPRay

end
