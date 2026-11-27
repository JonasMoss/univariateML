module

public import UnivariateML.Family.TDist
public import UnivariateML.Family.StudentHalf
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog

import Mathlib.Tactic

set_option linter.style.header false

/-! # Paths to Student likelihood faces -/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.StudentFaces

noncomputable section

private theorem logGamma_differentiable_pos {z : ℝ} (hz : 0 < z) :
    DifferentiableAt ℝ (fun t : ℝ ↦ log (Gamma t)) z :=
  (differentiableAt_Gamma (fun m ↦ ne_of_gt
    ((neg_nonpos.mpr (Nat.cast_nonneg m)).trans_lt hz))).log (Gamma_pos_of_pos hz).ne'

private theorem logGamma_continuous_pos {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun t : ℝ ↦ log (Gamma t)) z :=
  (logGamma_differentiable_pos hz).continuousAt

private theorem normalizer_continuous {ν : ℝ} (hν : 0 < ν) :
    ContinuousAt TDist.logNormalizer ν := by
  unfold TDist.logNormalizer
  refine ContinuousAt.sub (ContinuousAt.sub ?_ ?_) ?_
  · exact (logGamma_continuous_pos (show 0 < (ν+1)/2 by positivity)).comp'
      (f := fun t : ℝ ↦ (t+1)/2) (x := ν) (by fun_prop)
  · exact (logGamma_continuous_pos (show 0 < ν/2 by positivity)).comp'
      (f := fun t : ℝ ↦ t/2) (x := ν) (by fun_prop)
  · exact ((by fun_prop : Continuous (fun t : ℝ ↦ t*π)).continuousAt.log
      (by positivity)).div_const 2

/-- Holding the ordinary scale fixed while df decreases to two recovers the
positive-scale Student-two likelihood. The corresponding standard deviation
diverges along the same path. -/
theorem std_student_two_path {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {a : ℝ}
    (ha : 0 < a) :
    let σ := fun ν : ℝ ↦ a * sqrt (ν / (ν - 2))
    Tendsto σ (𝓝[>] 2) atTop ∧
      Tendsto (fun ν ↦ exp (TDist.stdLogLik x μ ν (σ ν))) (𝓝[>] 2)
        (𝓝 (TDist.likelihood x μ 2 a)) := by
  dsimp
  have hgt : ∀ᶠ ν : ℝ in 𝓝[>] 2, 2 < ν := self_mem_nhdsWithin
  have hν : Tendsto (fun ν : ℝ ↦ ν) (𝓝[>] 2) (𝓝 2) :=
    tendsto_id.mono_left nhdsWithin_le_nhds
  have hzero : Tendsto (fun ν : ℝ ↦ ν - 2) (𝓝[>] 2) (𝓝[>] 0) := by
    refine tendsto_nhdsWithin_iff.mpr ⟨?_, hgt.mono fun ν h ↦ sub_pos.mpr h⟩
    simpa using hν.sub_const 2
  have hinv : Tendsto (fun ν : ℝ ↦ (ν - 2)⁻¹) (𝓝[>] 2) atTop :=
    tendsto_inv_nhdsGT_zero.comp hzero
  have hratio : Tendsto (fun ν : ℝ ↦ ν / (ν - 2)) (𝓝[>] 2) atTop := by
    simpa only [div_eq_mul_inv] using hν.pos_mul_atTop (by norm_num : (0 : ℝ) < 2) hinv
  refine ⟨(tendsto_sqrt_atTop.comp hratio).const_mul_atTop ha, ?_⟩
  have hcont : ContinuousAt (fun ν ↦ TDist.logLik x μ ν a) 2 := by
    unfold TDist.logLik
    apply tendsto_finsetSum
    intro i _
    refine ((normalizer_continuous (by norm_num)).sub continuousAt_const).sub
      (ContinuousAt.mul (by fun_prop) (ContinuousAt.log ?_ ?_))
    · exact continuousAt_const.add (continuousAt_const.div (by fun_prop) (by positivity))
    · positivity
  refine (continuous_exp.continuousAt.tendsto.comp
    (hcont.tendsto.mono_left nhdsWithin_le_nhds)).congr' ?_
  filter_upwards [hgt] with ν hv
  have hv' : 0 < ν := by linarith
  have hm : 0 < ν - 2 := sub_pos.mpr hv
  have hσ : 0 < a * sqrt (ν / (ν - 2)) := mul_pos ha (sqrt_pos.mpr (div_pos hv' hm))
  rw [Function.comp_apply, TDist.stdLogLik_eq x μ hv hσ, TDist.ordinaryScale, mul_assoc,
    ← sqrt_mul (by positivity), div_mul_div_comm, mul_comm ν (ν - 2),
    div_self (by positivity), sqrt_one, mul_one]

private def offAtom {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i ≠ μ

private theorem offAtom_count {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) :
    ((offAtom x μ).card : ℝ) = n - TDist.multiplicity x μ := by
  have h : TDist.multiplicity x μ + (offAtom x μ).card = n := by
    unfold TDist.multiplicity offAtom
    rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
  have h' : (TDist.multiplicity x μ : ℝ) + (offAtom x μ).card = n := by exact_mod_cast h
  linarith

private def fixedSdRemainder {n : ℕ} (x : Fin n → ℝ) (μ σ ν : ℝ) : ℝ :=
  n * (TDist.logNormalizer ν + log ν / 2) -
    (ν + 1) / 2 * ∑ i ∈ offAtom x μ, log ((ν - 2) * σ^2 + (x i - μ)^2)

private theorem fixedSd_decompose {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν σ : ℝ} (hν : 2 < ν) (hσ : 0 < σ) :
    TDist.stdLogLik x μ ν σ =
      TDist.exponent x μ ν / 2 * log ((ν - 2) * σ^2) + fixedSdRemainder x μ σ ν := by
  let q := (ν - 2) * σ^2
  have hq : 0 < q := by dsimp [q]; positivity
  have hv : 0 < ν := by linarith
  have hNorm :
      log (Gamma ((ν + 1) / 2)) - log (Gamma (ν / 2)) -
        log ((ν - 2) * π) / 2 - log σ =
      TDist.logNormalizer ν + log ν / 2 - log q / 2 := by
    unfold TDist.logNormalizer
    dsimp [q]
    rw [log_mul (sub_pos.mpr hν).ne' pi_pos.ne', log_mul hv.ne' pi_pos.ne',
      log_mul (sub_pos.mpr hν).ne' (by positivity : σ^2 ≠ 0), log_pow]
    ring
  have hKernel (i : Fin n) :
      log (1 + (x i - μ)^2 / q) = log (q + (x i - μ)^2) - log q := by
    rw [show 1 + (x i - μ)^2 / q = (q + (x i - μ)^2) / q by field_simp,
      log_div (by positivity) hq.ne']
  unfold TDist.stdLogLik fixedSdRemainder TDist.exponent
  simp_rw [hNorm]
  rw [Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  have hOff : (∑ i, log (1 + (x i - μ)^2 / q)) =
      ∑ i ∈ offAtom x μ, log (1 + (x i - μ)^2 / q) := by
    unfold offAtom
    exact (Finset.sum_filter_of_ne (s := Finset.univ) (p := fun i ↦ x i ≠ μ)
      (fun i _ hi he ↦ hi (by simp [he]))).symm
  rw [hOff]
  simp_rw [hKernel]
  rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul, offAtom_count]
  change _ = _ / 2 * log q + (↑n * _ - (ν + 1) / 2 *
    ∑ i ∈ offAtom x μ, log (q + (x i - μ)^2))
  ring

/-- The log-height of the fixed-standard-deviation collapse at critical
mass, written with the Student normalization at df two. -/
def fixedSdCollapseLog {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  n * (TDist.logNormalizer 2 + log 2 / 2) -
    3 / 2 * ∑ i ∈ Finset.univ.filter (fun i ↦ x i ≠ μ), log ((x i - μ)^2)

/-- The fixed-mean, fixed-standard-deviation likelihood has three df-two
limits, according to whether the atom at the held mean is below, at, or
above two thirds of the sample. -/
theorem std_fixed_sd_collapse {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    let L := fun ν : ℝ ↦ exp (TDist.stdLogLik x μ ν σ)
    (0 < TDist.exponent x μ 2 → Tendsto L (𝓝[>] 2) (𝓝 0)) ∧
    (TDist.exponent x μ 2 = 0 → Tendsto L (𝓝[>] 2) (𝓝 (exp (fixedSdCollapseLog x μ)))) ∧
    (TDist.exponent x μ 2 < 0 → Tendsto L (𝓝[>] 2) atTop) := by
  dsimp
  let q : ℝ → ℝ := fun ν ↦ (ν - 2) * σ^2
  have hgt : ∀ᶠ ν : ℝ in 𝓝[>] 2, 2 < ν := self_mem_nhdsWithin
  have h0 : Tendsto q (𝓝[>] 2) (𝓝 0) := by
    have H : ContinuousAt q 2 := by dsimp [q]; fun_prop
    simpa [q] using H.tendsto.mono_left nhdsWithin_le_nhds
  have hq : Tendsto q (𝓝[>] 2) (𝓝[>] 0) :=
    tendsto_nhdsWithin_iff.mpr ⟨h0, hgt.mono fun ν h ↦ mul_pos (sub_pos.mpr h) (sq_pos_of_pos hσ)⟩
  have hν : Tendsto (fun ν : ℝ ↦ ν) (𝓝[>] 2) (𝓝 2) :=
    tendsto_id.mono_left nhdsWithin_le_nhds
  have hr : Tendsto (fixedSdRemainder x μ σ) (𝓝[>] 2) (𝓝 (fixedSdCollapseLog x μ)) := by
    have hi (i : Fin n) (hi : i ∈ offAtom x μ) : x i ≠ μ := (Finset.mem_filter.mp hi).2
    have hS := tendsto_finsetSum (s := offAtom x μ) fun i hi' ↦
      (h0.add_const ((x i - μ)^2)).log (by simpa using (sq_pos_of_ne_zero
        (sub_ne_zero.mpr (hi i hi'))).ne')
    have hN := (normalizer_continuous (ν := 2) (by norm_num)).tendsto.comp hν
    have hLog := hν.log (by norm_num)
    have h := (hN.add (hLog.div_const 2)).const_mul (n : ℝ)
    change Tendsto (fun ν ↦ (n : ℝ) * (TDist.logNormalizer ν + log ν / 2) -
      (ν + 1) / 2 * ∑ i ∈ offAtom x μ, log (q ν + (x i - μ)^2)) _ _
    simpa [fixedSdCollapseLog, offAtom, show (2 : ℝ) + 1 = 3 by norm_num]
      using h.sub (((hν.add_const 1).div_const 2).mul hS)
  let c : ℝ := ((n : ℝ) - TDist.multiplicity x μ) / (2 * σ^2)
  have hsmall : Tendsto (fun ν ↦ c * (q ν * log (q ν))) (𝓝[>] 2) (𝓝 0) := by
    have hc := (continuous_mul_log.continuousAt (x := 0)).tendsto.comp h0
    simpa using hc.const_mul c
  have hR := hsmall.add hr
  rw [zero_add] at hR
  have hEq : (fun ν ↦ TDist.stdLogLik x μ ν σ) =ᶠ[𝓝[>] 2]
      fun ν ↦ TDist.exponent x μ 2 / 2 * log (q ν) +
        (c * (q ν * log (q ν)) + fixedSdRemainder x μ σ ν) := by
    filter_upwards [hgt] with ν hv
    rw [fixedSd_decompose x μ hv hσ]
    unfold q c TDist.exponent
    field_simp [hσ.ne']; ring
  have hl := tendsto_log_nhdsGT_zero.comp hq
  refine ⟨fun hp ↦ ?_, fun he ↦ ?_, fun hn ↦ ?_⟩
  · exact tendsto_exp_atBot.comp (((hl.const_mul_atBot (half_pos hp)).atBot_add hR).congr' hEq.symm)
  · apply continuous_exp.continuousAt.tendsto.comp
    refine hR.congr' ?_
    exact hEq.symm.mono fun _ h ↦ by simpa [he] using h
  · have hneg : TDist.exponent x μ 2 / 2 < 0 := by linarith
    exact tendsto_exp_atTop.comp (((hl.const_mul_atBot_of_neg hneg).atTop_add hR).congr' hEq.symm)

/-- The Gaussian end of the fixed mean and standard deviation likelihood
is the product at the same held mean and standard deviation. -/
theorem std_fixed_sd_normal {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ}
    (hσ : 0 < σ) :
    Tendsto (fun ν ↦ exp (TDist.stdLogLik x μ ν σ)) atTop
      (𝓝 (∏ i, TDist.normalDensity μ σ (x i))) := by
  refine (tendsto_finsetProd Finset.univ fun i _ ↦
    TDist.std_density_tendsto_normal μ (x i) hσ).congr' ?_
  filter_upwards [eventually_gt_atTop (2 : ℝ)] with ν hν
  rw [TDist.stdLogLik_eq x μ hν hσ, TDist.logLik, Real.exp_sum]
  rfl

/-- At held mean and df the ordinary-scale path diverges at a negative
exponent and gives a finite unattained supremum at exact equality. -/
theorem std_held_mean_df_collapse {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {ν : ℝ} (hν : 2 < ν) :
    (TDist.exponent x μ ν < 0 →
      Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) atTop) ∧
    (TDist.exponent x μ ν = 0 →
      Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x μ ν))) ∧
      sSup (TDist.likelihood x μ ν '' Ioi 0) = exp (TDist.collapseLogValue x μ ν) ∧
      ∀ a > 0, TDist.likelihood x μ ν a < exp (TDist.collapseLogValue x μ ν)) := by
  have hν' : 0 < ν := by linarith
  refine ⟨TDist.collapse_unbounded x μ hν', fun he ↦ ?_⟩
  have hx : ∃ i, x i ≠ μ := by
    by_contra! h
    rw [TDist.exponent_all_tied x μ ν h] at he
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  exact TDist.heldLocation_likelihood_equality x μ hν' hx he

/-- An atom above two thirds yields an allowed fixed df and a divergent
ordinary-scale path at that atom. This is the supercritical branch of the
free-df collapse comparison. -/
theorem std_supercritical_atom {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    (hm : 2 * n < 3 * TDist.multiplicity x μ) :
    ∃ ν > 2, Tendsto (TDist.likelihood x μ ν) (𝓝[>] 0) atTop := by
  let k : ℝ := TDist.multiplicity x μ
  have hm' : (2 : ℝ)*n < 3*k := by dsimp [k]; exact_mod_cast hm
  let d := (3*k - 2*n) / (2*(n+1))
  have hd : 0 < d := div_pos (by linarith) (by positivity)
  refine ⟨2+d, by linarith, TDist.collapse_unbounded x μ (by linarith) ?_⟩
  have hdk : d * ((n : ℝ) - k) < d * (2*(n+1)) := by
    apply mul_lt_mul_of_pos_left _ hd
    have hk : 0 ≤ k := by dsimp [k]; positivity
    have hn : 0 ≤ (n : ℝ) := by positivity
    linarith
  have hde : d * (2*((n : ℝ)+1)) = 3*k - 2*n :=
    div_mul_cancel₀ _ (by positivity)
  change (2+d)*((n : ℝ)-k)-k < 0
  nlinarith

private theorem critical_strict {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b m ν a : ℝ} (hν : 1 < ν) (ha : 0 < a) (he : TDist.exponent x b ν = 0) :
    TDist.likelihood x m ν a < exp (TDist.collapseLogValue x b ν) := by
  have hv : 0 < ν := by linarith
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hx : ∃ i, x i ≠ b := by
    by_contra! h
    rw [TDist.exponent_all_tied x b ν h] at he
    linarith
  by_cases hm : m = b
  · rw [hm]
    exact (TDist.heldLocation_likelihood_equality x b hv hx he).2.2 a ha
  let q := ν * a^2
  let d := m - b
  let Q := d^2 + q
  let k : ℝ := TDist.multiplicity x b
  let p : ℝ := (n : ℝ) - k
  let w := (ν + 1) / 2
  have hq : 0 < q := by dsimp [q]; positivity
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have hw : 0 < w := by dsimp [w]; positivity
  have hkp : k = ν*p := by
    unfold TDist.exponent at he
    dsimp [k, p]
    linarith
  have hp : 0 < p := by
    have h : (ν+1)*p = n := by dsimp [p] at *; linarith
    have hp' : 0 < (ν+1)*p := h.symm ▸ hn'
    exact pos_of_mul_pos_right hp' (by positivity)
  have hkp' : 0 < k-p := by
    rw [hkp, ← sub_one_mul]
    exact mul_pos (sub_pos.mpr hν) hp
  have hQq : q < Q := by dsimp [Q]; linarith [sq_pos_of_ne_zero (sub_ne_zero.mpr hm)]
  let D : Fin n → ℝ := fun i ↦ q + (x i - m)^2
  have hD (i : Fin n) : 0 < D i := by dsimp [D]; positivity
  have hOff (i : Fin n) (hi : i ∈ offAtom x b) :
      log ((x i - b)^2) + log q - log Q ≤ log (D i) := by
    have hi' : x i ≠ b := (Finset.mem_filter.mp hi).2
    have hr : 0 < (x i - b)^2 := sq_pos_of_ne_zero (sub_ne_zero.mpr hi')
    rw [← log_mul hr.ne' hq.ne', ← log_div (mul_pos hr hq).ne' hQ.ne']
    apply log_le_log (by positivity)
    apply (div_le_iff₀ hQ).mpr
    dsimp [D, Q, d]
    nlinarith only [sq_nonneg ((x i - b)*(m-b) - ((m-b)^2+q))]
  have hSum : k * log Q + (∑ i ∈ offAtom x b, log ((x i - b)^2)) +
      p * (log q - log Q) ≤ ∑ i, log (D i) := by
    have hOn : (∑ i ∈ Finset.univ.filter (fun i ↦ x i = b), log (D i)) =
        k * log Q := by
      calc
        _ = ∑ _i ∈ Finset.univ.filter (fun i ↦ x i = b), log Q := by
          apply Finset.sum_congr rfl
          intro i hi
          dsimp [D, Q, d]
          rw [(Finset.mem_filter.mp hi).2]
          congr 1
          ring
        _ = _ := by simp [k, TDist.multiplicity]
    have h := Finset.sum_le_sum (fun i hi ↦ hOff i hi)
    have heq : (∑ i ∈ offAtom x b, (log ((x i - b)^2) + log q - log Q)) =
        (∑ i ∈ offAtom x b, log ((x i - b)^2)) + p * (log q - log Q) := by
      simp only [add_sub_assoc, Finset.sum_add_distrib, Finset.sum_const, nsmul_eq_mul,
        offAtom_count, p, k]
    rw [heq] at h
    have hsplit := Finset.sum_filter_add_sum_filter_not Finset.univ (fun i ↦ x i = b)
      (fun i ↦ log (D i))
    change _ + ∑ i ∈ offAtom x b, log (D i) = _ at hsplit
    rw [hOn] at hsplit
    linarith
  have hExpand : TDist.logLik x m ν a =
      n * (TDist.logNormalizer ν - log a) -
        w * ((∑ i, log (D i)) - n * log q) := by
    have hi (i : Fin n) : log (1 + (x i-m)^2/q) = log (D i) - log q := by
      rw [show 1 + (x i-m)^2/q = D i / q by dsimp [D]; field_simp,
        log_div (hD i).ne' hq.ne']
    unfold TDist.logLik
    change (∑ i, (TDist.logNormalizer ν - log a - w * log (1 + (x i - m)^2 / q))) = _
    simp_rw [hi]
    rw [Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_sub_distrib]
    simp [mul_sub]
  have hLogq : log q = log ν + 2 * log a := by
    dsimp [q]
    rw [log_mul hv.ne' (by positivity : a^2 ≠ 0), log_pow]
    norm_num
  have hwP : 2*w*p = n := by
    dsimp [w, p] at *
    linarith
  have hstrict : 0 < w*(k-p)*(log Q - log q) :=
    mul_pos (mul_pos hw hkp') (sub_pos.mpr (log_lt_log hq hQq))
  rw [TDist.likelihood, exp_lt_exp, hExpand, TDist.collapseLogValue]
  have hbound := mul_le_mul_of_nonneg_left hSum hw.le
  have hb₁ := congrArg (fun t : ℝ ↦ t * log a) hwP
  have hb₂ : (n : ℝ) = k+p := by dsimp [p]; ring
  have hb₃ := congrArg (fun t : ℝ ↦ w*t*log q) hb₂
  change (n : ℝ) * TDist.logNormalizer ν + w *
    (p * log ν - ∑ i ∈ offAtom x b, log ((x i - b)^2)) >
      (n : ℝ) * (TDist.logNormalizer ν - log a) - w * ((∑ i, log (D i)) - n * log q)
  rw [hLogq] at hbound hstrict hb₃ ⊢
  nlinarith only [hbound, hstrict, hb₁, hb₃]

/-- For held df above one, a critical atom gives the exact finite,
unattained joint supremum. In particular, allowing location to move
cannot improve the critical atom's vertical collapse value. -/
theorem lst_critical_joint {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b ν : ℝ} (hν : 1 < ν) (he : TDist.exponent x b ν = 0) :
    IsLUB ((fun m : ℝ × ℝ ↦ TDist.likelihood x m.1 ν m.2) '' (univ ×ˢ Ioi 0))
      (exp (TDist.collapseLogValue x b ν)) ∧
      ∀ m a : ℝ, 0 < a → TDist.likelihood x m ν a < exp (TDist.collapseLogValue x b ν) := by
  have hs (m a : ℝ) (ha : 0 < a) := critical_strict x hn hν ha he (m := m)
  refine ⟨⟨?_, ?_⟩, hs⟩
  · rintro _ ⟨⟨m,a⟩, h, rfl⟩
    exact (hs m a h.2).le
  · intro z hz
    have hx : ∃ i, x i ≠ b := by
      by_contra! h
      rw [TDist.exponent_all_tied x b ν h] at he
      have hn' : (0 : ℝ) < n := by exact_mod_cast hn
      linarith
    apply le_of_tendsto (TDist.heldLocation_likelihood_equality x b (by linarith) hx he).1
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
    exact hz ⟨(b,a), ⟨mem_univ _, ha⟩, rfl⟩

private theorem open_shape_value_comparison {f : ℝ → ℝ} {a L N : ℝ}
    (hcont : ContinuousOn f (Ioi a))
    (hL : Tendsto f (𝓝[>] a) (𝓝 L)) (hN : Tendsto f atTop (𝓝 N)) :
    (∃ v ∈ Ioi a, IsMaxOn f (Ioi a) v) ↔
      ∃ v > a, max L N ≤ f v := by
  constructor
  · rintro ⟨v,hv,hm⟩
    refine ⟨v,hv,max_le ?_ ?_⟩
    · apply le_of_tendsto hL
      have hdom : ∀ᶠ w : ℝ in 𝓝[>] a, w ∈ Ioi a := self_mem_nhdsWithin
      exact hdom.mono fun _ h ↦ hm h
    · apply le_of_tendsto hN
      exact (eventually_gt_atTop a).mono fun _ h ↦ hm h
  · rintro ⟨v,hv,hface⟩
    by_cases hm : IsMaxOn f (Ioi a) v
    · exact ⟨v,hv,hm⟩
    · rw [isMaxOn_iff] at hm
      push Not at hm
      obtain ⟨w,hw,hbetter⟩ := hm
      have hw' : a < w := hw
      have hLt : L < f w := ((le_max_left L N).trans hface).trans_lt hbetter
      have hNt : N < f w := ((le_max_right L N).trans hface).trans_lt hbetter
      obtain ⟨ε,hε,hsmall⟩ := Metric.mem_nhdsWithin_iff.mp
        (hL.eventually (Iio_mem_nhds hLt))
      obtain ⟨u0,hu0⟩ := eventually_atTop.mp (hN.eventually (Iio_mem_nhds hNt))
      let l := a + min (ε/2) ((w-a)/2)
      let u := max w u0
      have hl : a < l := lt_add_of_pos_right _ (lt_min (half_pos hε) (half_pos (sub_pos.mpr hw')))
      have hlw : l ≤ w := by dsimp [l]; linarith [min_le_right (ε/2) ((w-a)/2)]
      have hwu : w ≤ u := le_max_left _ _
      have hsub : Icc l u ⊆ Ioi a := fun q hq ↦ hl.trans_le hq.1
      obtain ⟨p,hp,hpmax⟩ := isCompact_Icc.exists_isMaxOn ⟨w,hlw,hwu⟩ (hcont.mono hsub)
      refine ⟨p,hsub hp,fun q hq ↦ ?_⟩
      by_cases hqK : q ∈ Icc l u
      · exact hpmax hqK
      have hwp : f w ≤ f p := hpmax (show w ∈ Icc l u from ⟨hlw,hwu⟩)
      apply le_trans _ hwp
      by_cases hqsmall : q < l
      · have hball : q ∈ Metric.ball a ε := by
          rw [Metric.mem_ball, Real.dist_eq, abs_of_pos (sub_pos.mpr hq)]
          dsimp [l] at hqsmall
          linarith [min_le_left (ε/2) ((w-a)/2)]
        exact (hsmall ⟨hball,hq⟩).le
      · have hqu : u ≤ q := by
          by_contra! hh
          exact hqK ⟨le_of_not_gt hqsmall,hh.le⟩
        exact (hu0 q ((le_max_right w u0).trans hqu)).le

private theorem open_shape_comparison {f : ℝ → ℝ} {a L N : ℝ}
    (hd : ∀ v > a, DifferentiableAt ℝ f v)
    (hL : Tendsto f (𝓝[>] a) (𝓝 L)) (hN : Tendsto f atTop (𝓝 N)) :
    (∃ v ∈ Ioi a, IsMaxOn f (Ioi a) v) ↔
      ∃ v > a, HasDerivAt f 0 v ∧ max L N ≤ f v := by
  constructor
  · rintro ⟨v,hv,hm⟩
    have hv' : a < v := hv
    have hloc := hm.isLocalMax (Ioi_mem_nhds hv')
    have hdiff := (hd v hv).hasDerivAt
    rw [hloc.deriv_eq_zero] at hdiff
    refine ⟨v,hv,hdiff,max_le ?_ ?_⟩
    · apply le_of_tendsto hL
      have hdom : ∀ᶠ w : ℝ in 𝓝[>] a, w ∈ Ioi a := self_mem_nhdsWithin
      exact hdom.mono fun _ h ↦ hm h
    · apply le_of_tendsto hN
      exact (eventually_gt_atTop a).mono fun _ h ↦ hm h
  · rintro ⟨v,hv,_,hface⟩
    exact (open_shape_value_comparison
      (fun q hq ↦ (hd q hq).continuousAt.continuousWithinAt) hL hN).mpr ⟨v,hv,hface⟩

private theorem fixed_sd_differentiable {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν σ : ℝ} (hν : 2 < ν) (hσ : 0 < σ) :
    DifferentiableAt ℝ (fun v ↦ exp (TDist.stdLogLik x μ v σ)) ν := by
  have h1 : DifferentiableAt ℝ (fun v : ℝ ↦ log (Gamma ((v+1)/2))) ν :=
    (logGamma_differentiable_pos (by linarith : 0 < (ν+1)/2)).fun_comp'
      (f := fun v : ℝ ↦ (v+1)/2) ν (by fun_prop)
  have h2 : DifferentiableAt ℝ (fun v : ℝ ↦ log (Gamma (v/2))) ν :=
    (logGamma_differentiable_pos (by linarith : 0 < ν/2)).fun_comp'
      (f := fun v : ℝ ↦ v/2) ν (by fun_prop)
  have h3 : DifferentiableAt ℝ (fun v : ℝ ↦ log ((v-2)*π)) ν :=
    DifferentiableAt.log (by fun_prop) (mul_pos (sub_pos.mpr hν) pi_pos).ne'
  have hterm (i : Fin n) : DifferentiableAt ℝ
      (fun v : ℝ ↦ log (Gamma ((v+1)/2)) - log (Gamma (v/2)) -
        log ((v-2)*π)/2 - log σ - (v+1)/2*log (1+(x i-μ)^2/((v-2)*σ^2))) ν := by
    refine (((h1.sub h2).sub (h3.div_const 2)).sub_const _).sub
      (DifferentiableAt.mul (by fun_prop) (DifferentiableAt.log ?_ (by positivity)))
    exact DifferentiableAt.add (differentiableAt_const _)
      (DifferentiableAt.div (differentiableAt_const _) (by fun_prop)
      (mul_pos (sub_pos.mpr hν) (sq_pos_of_pos hσ)).ne')
  unfold TDist.stdLogLik
  have hd := HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦ (hterm i).hasDerivAt
  exact hd.exp.differentiableAt

/-- With mean and standard deviation held, a finite shape maximizes
exactly when the mass is at most critical and a stationary shape reaches
both the matched normal and the applicable lower face. -/
theorem std_fixed_sd_criterion {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let L := fun v ↦ exp (TDist.stdLogLik x μ v σ)
    let lower := if TDist.exponent x μ 2 = 0 then exp (fixedSdCollapseLog x μ) else 0
    let normal := ∏ i, TDist.normalDensity μ σ (x i)
    (∃ v ∈ Ioi 2, IsMaxOn L (Ioi 2) v) ↔
      0 ≤ TDist.exponent x μ 2 ∧
        ∃ v > 2, HasDerivAt L 0 v ∧ max lower normal ≤ L v := by
  dsimp only
  have h := std_fixed_sd_collapse x μ hσ
  by_cases he : 0 ≤ TDist.exponent x μ 2
  · have hl : Tendsto (fun v ↦ exp (TDist.stdLogLik x μ v σ)) (𝓝[>] 2)
        (𝓝 (if TDist.exponent x μ 2 = 0 then exp (fixedSdCollapseLog x μ) else 0)) := by
      rcases lt_or_eq_of_le he with hpos | hzero
      · rw [ite_eq_right hpos.ne']
        exact h.1 hpos
      · rw [ite_eq_left hzero.symm]
        exact h.2.1 hzero.symm
    simp only [he, true_and]
    exact open_shape_comparison (fun _ hv ↦ fixed_sd_differentiable x μ hv hσ)
      hl (std_fixed_sd_normal x μ hσ)
  · simp only [he, false_and, iff_false]
    rintro ⟨v,hv,hm⟩
    have ht := h.2.2 (lt_of_not_ge he)
    have hdom : ∀ᶠ w : ℝ in 𝓝[>] 2, w ∈ Ioi (2 : ℝ) := self_mem_nhdsWithin
    obtain ⟨w,hw,hgt⟩ := (hdom.and (ht.eventually_gt_atTop
      (exp (TDist.stdLogLik x μ v σ)))).exists
    exact hgt.not_ge (hm hw)

private theorem half_shape_differentiable {n : ℕ} (x : Fin n → ℝ) (μ : ℝ)
    {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s) :
    DifferentiableAt ℝ (fun v ↦ SkewLSHalfT.likelihood x μ v s) ν := by
  have h1 : DifferentiableAt ℝ (fun v : ℝ ↦ log (Gamma ((v+1)/2))) ν :=
    (logGamma_differentiable_pos (by positivity : 0 < (ν+1)/2)).fun_comp'
      (f := fun v : ℝ ↦ (v+1)/2) ν (by fun_prop)
  have h2 : DifferentiableAt ℝ (fun v : ℝ ↦ log (Gamma (v/2))) ν :=
    (logGamma_differentiable_pos (by positivity : 0 < ν/2)).fun_comp'
      (f := fun v : ℝ ↦ v/2) ν (by fun_prop)
  have h3 : DifferentiableAt ℝ (fun v : ℝ ↦ log (v*π)) ν :=
    DifferentiableAt.log (by fun_prop) (mul_pos hν pi_pos).ne'
  have hterm (i : Fin n) : DifferentiableAt ℝ
      (fun v : ℝ ↦ log (Gamma ((v+1)/2)) - log (Gamma (v/2)) -
        log (v*π)/2 - log s - (v+1)/2*log (1+(x i-μ)^2/(v*s^2))) ν := by
    refine (((h1.sub h2).sub (h3.div_const 2)).sub_const _).sub
      (DifferentiableAt.mul (by fun_prop) (DifferentiableAt.log ?_ (by positivity)))
    exact DifferentiableAt.add (differentiableAt_const _)
      (DifferentiableAt.div (differentiableAt_const _) (by fun_prop)
      (mul_pos hν (sq_pos_of_pos hs)).ne')
  unfold SkewLSHalfT.likelihood TDist.likelihood TDist.logLik TDist.logNormalizer
  exact ((HasDerivAt.fun_sum (u := Finset.univ) fun i _ ↦
    (hterm i).hasDerivAt).exp.const_mul _).differentiableAt

private theorem normalizer_small_df : Tendsto TDist.logNormalizer (𝓝[>] 0) atBot := by
  let r := fun v : ℝ ↦ log (Gamma ((v+1)/2)) - log (Gamma (v/2+1)) - log π/2 - log 2
  have hr : ContinuousAt r 0 := by
    dsimp [r]
    refine ContinuousAt.sub (ContinuousAt.sub (ContinuousAt.sub ?_ ?_) continuousAt_const)
      continuousAt_const
    · exact (logGamma_continuous_pos (show 0 < ((0 : ℝ)+1)/2 by norm_num)).comp'
        (f := fun v : ℝ ↦ (v+1)/2) (x := 0) (by fun_prop)
    · exact (logGamma_continuous_pos (show 0 < (0 : ℝ)/2+1 by norm_num)).comp'
        (f := fun v : ℝ ↦ v/2+1) (x := 0) (by fun_prop)
  refine ((tendsto_log_nhdsGT_zero.const_mul_atBot (by norm_num : 0 < (1 : ℝ)/2)).atBot_add
    (hr.tendsto.mono_left nhdsWithin_le_nhds)).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with v hv
  have hv' : 0 < v := hv
  have hh : 0 < v/2 := by positivity
  change (1/2)*log v + r v = TDist.logNormalizer v
  dsimp [r, TDist.logNormalizer]
  rw [Gamma_add_one hh.ne', log_mul hh.ne' (Gamma_pos_of_pos hh).ne',
    log_div hv'.ne' (by norm_num), log_mul hv'.ne' pi_pos.ne']
  ring

/-- With endpoint and scale held, the half-Student likelihood vanishes
at df zero, including when observations equal the endpoint. Its other
shape limit is the matched half-normal. -/
theorem half_fixed_endpoint_scale_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {s : ℝ} (hs : 0 < s) :
    Tendsto (fun v ↦ SkewLSHalfT.likelihood x μ v s) (𝓝[>] 0) (𝓝 0) ∧
      Tendsto (fun v ↦ SkewLSHalfT.likelihood x μ v s) atTop
        (𝓝 (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s)) := by
  refine ⟨?_, SkewLSHalfT.half_normal_path x μ hs⟩
  have ht : Tendsto (fun v ↦ (2*exp (TDist.logNormalizer v - log s))^n)
      (𝓝[>] 0) (𝓝 0) := by
    have h := tendsto_exp_atBot.comp (tendsto_atBot_add_const_right _
      (-log s) normalizer_small_df)
    simpa [sub_eq_add_neg, zero_pow hn.ne'] using (h.const_mul (2 : ℝ)).pow n
  refine squeeze_zero' (Eventually.of_forall fun v ↦ mul_nonneg (by positivity) (exp_pos _).le)
    ?_ ht
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with v hv
  have hv' : 0 < v := hv
  rw [SkewLSHalfT.likelihood, TDist.likelihood, TDist.logLik, exp_sum, mul_pow]
  apply mul_le_mul_of_nonneg_left _ (by positivity)
  rw [show exp (TDist.logNormalizer v - log s)^n =
      ∏ _i : Fin n, exp (TDist.logNormalizer v - log s) by simp]
  apply Finset.prod_le_prod₀ (fun _ _ ↦ (exp_pos _).le)
  intro i _
  apply exp_le_exp.mpr
  exact sub_le_self _ (mul_nonneg (by positivity)
    (log_nonneg (le_add_of_nonneg_right (by positivity))))

/-- At held half-Student endpoint and scale, a finite fit exists
exactly when some stationary shape reaches the matched half-normal. -/
theorem half_fixed_endpoint_scale_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) {s : ℝ} (hs : 0 < s) :
    let L := fun v ↦ SkewLSHalfT.likelihood x μ v s
    (∃ v ∈ Ioi 0, IsMaxOn L (Ioi 0) v) ↔
      ∃ v > 0, HasDerivAt L 0 v ∧ (∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s) ≤ L v := by
  dsimp only
  have h := half_fixed_endpoint_scale_ends x hn μ hs
  have hN : 0 ≤ ∏ i, 2*SkewLSGaussian.φ ((x i-μ)/s)/s :=
    Finset.prod_nonneg fun i _ ↦ div_nonneg
      (mul_nonneg (by norm_num) (SkewLSGaussian.phi_pos _).le) hs.le
  simpa only [max_eq_right hN] using open_shape_comparison
    (fun _ hv ↦ half_shape_differentiable x μ hv hs) h.1 h.2

private theorem density_residual_bound (μ y : ℝ) {ν s : ℝ}
    (hν : 0 < ν) (hs : 0 < s) (hy : y ≠ μ) :
    TDist.density μ ν s y ≤ exp (TDist.logNormalizer ν + log ν / 2 - log |y-μ|) := by
  let r := y-μ
  have hr : r ≠ 0 := sub_ne_zero.mpr hy
  have hq : 0 < ν*s^2 := by positivity
  have hratio : 0 < r^2/(ν*s^2) := div_pos (sq_pos_of_ne_zero hr) hq
  have hL := log_le_log hratio (le_add_of_nonneg_left (zero_le_one : (0 : ℝ) ≤ 1))
  have hpos := log_nonneg (le_add_of_nonneg_right hratio.le)
  have hU := mul_le_mul_of_nonneg_right (show (1 : ℝ)/2 ≤ (ν+1)/2 by linarith) hpos
  rw [log_div (pow_ne_zero _ hr) hq.ne', log_mul hν.ne' (by positivity),
    log_pow, log_pow] at hL
  push_cast at hL
  unfold TDist.density
  rw [exp_le_exp, log_abs]
  change TDist.logNormalizer ν - log s - (ν+1)/2*log (1+r^2/(ν*s^2)) ≤ _
  linarith

private theorem profile_small_df {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ)
    (hx : ∀ i, x i ≠ μ) :
    Tendsto (SkewLSHalfT.scaleProfile x μ) (𝓝[>] 0) (𝓝 0) := by
  let B (ν : ℝ) := (2 : ℝ)^n * ∏ i, exp (TDist.logNormalizer ν - log |x i-μ|)
  have hterm (i : Fin n) :
      Tendsto (fun ν ↦ exp (TDist.logNormalizer ν - log |x i-μ|)) (𝓝[>] 0) (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_atBot_add_const_right _ _ normalizer_small_df)
  have ht : Tendsto B (𝓝[>] 0) (𝓝 0) := by
    have h := (tendsto_finsetProd Finset.univ fun i _ ↦ hterm i).const_mul ((2 : ℝ)^n)
    simpa [B, zero_pow hn.ne'] using h
  have hub (ν : ℝ) (hν : 0 < ν) (hν1 : ν < 1) :
      B ν ∈ upperBounds (SkewLSHalfT.likelihood x μ ν '' Ioi 0) := by
    rintro _ ⟨s,hs,rfl⟩
    have hs' : 0 < s := hs
    unfold SkewLSHalfT.likelihood TDist.likelihood TDist.logLik
    rw [exp_sum]
    apply mul_le_mul_of_nonneg_left _ (by positivity)
    apply Finset.prod_le_prod₀ (fun i _ ↦ (Real.exp_pos _).le)
    intro i _
    have h := density_residual_bound μ (x i) hν hs' (hx i)
    exact h.trans (exp_le_exp.mpr (by linarith [log_nonpos hν.le hν1.le]))
  have hdom : ∀ᶠ ν : ℝ in 𝓝[>] 0, ν ∈ Ioo (0 : ℝ) 1 := by
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one)] with v hlo hhi
    exact ⟨hlo,hhi⟩
  refine squeeze_zero' ?_ ?_ ht
  · filter_upwards [hdom] with ν hν
    have H := le_csSup ⟨_,hub ν hν.1 hν.2⟩
      (mem_image_of_mem (SkewLSHalfT.likelihood x μ ν) (show (1 : ℝ) ∈ Ioi 0 from one_pos))
    exact (mul_pos (by positivity) (exp_pos _)).le.trans H
  · filter_upwards [hdom] with ν hν
    exact csSup_le ⟨_,1,one_pos,rfl⟩ (hub ν hν.1 hν.2)

private theorem eligible_profile {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ)
    (hx : ∀ i, x i ≠ μ) {ν : ℝ} (hν : 0 < ν) :
    (∃ i, x i ≠ μ) ∧ 0 < TDist.exponent x μ ν ∧
      ∃ s > 0, TDist.scaleScore x μ ν s = 0 ∧
        SkewLSHalfT.scaleProfile x μ ν = SkewLSHalfT.likelihood x μ ν s ∧
        ∀ a > 0, SkewLSHalfT.likelihood x μ ν a ≤ SkewLSHalfT.likelihood x μ ν s := by
  have hx' : ∃ i, x i ≠ μ := ⟨⟨0,hn⟩,hx _⟩
  have he : 0 < TDist.exponent x μ ν := by
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    simpa [TDist.exponent, TDist.multiplicity, hx] using mul_pos hν hn'
  obtain ⟨s,⟨hs,hr⟩,_⟩ := ((TDist.heldLocation_maximum x μ hν hx').2 he).1
  have hm := (((TDist.heldLocation_maximum x μ hν hx').2 he).2 s hs).mpr hr
  have hm' (a : ℝ) (ha : 0 < a) :
      SkewLSHalfT.likelihood x μ ν a ≤ SkewLSHalfT.likelihood x μ ν s :=
    mul_le_mul_of_nonneg_left (exp_le_exp.mpr (hm ha)) (by positivity)
  have hG : IsGreatest (SkewLSHalfT.likelihood x μ ν '' Ioi 0)
      (SkewLSHalfT.likelihood x μ ν s) := by
    refine ⟨mem_image_of_mem _ hs,?_⟩
    rintro _ ⟨a,ha,rfl⟩
    exact hm' a ha
  exact ⟨hx',he,s,hs,hr,hG.csSup_eq,hm'⟩

set_option maxHeartbeats 900000 in
-- The profile selection and two partial scores elaborate in the same proof.
/-- With endpoint held and scale and positive shape free, ties cause
divergence. In the tie-free branch attainment is equivalent to a
stationary shape and scale reaching the RMS half-normal. -/
theorem half_held_endpoint_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ) :
    let D := (Ioi (0 : ℝ)) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ p.1 p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ i, x i ≠ μ) ∧ ∃ ν > 0, ∃ s > 0,
        TDist.scaleScore x μ ν s = 0 ∧
        HasDerivAt (fun v ↦ SkewLSHalfT.likelihood x μ v s) 0 ν ∧
        SkewLSHalfT.normalFace x μ ≤ SkewLSHalfT.likelihood x μ ν s := by
  dsimp only
  by_cases hx : ∀ i, x i ≠ μ
  · let P := SkewLSHalfT.scaleProfile x μ
    let H := SkewLSHalfT.normalFace x μ
    have he (ν : ℝ) (hν : 0 < ν) := eligible_profile x hn μ hx hν
    have hcont : ContinuousOn P (Ioi 0) := fun ν hν ↦
      (SkewLSHalfT.scale_profile_continuous x μ (he ν hν).1 hν
        (he ν hν).2.1).continuousWithinAt
    have hx' : ∃ i, x i ≠ μ := ⟨⟨0,hn⟩,hx _⟩
    have hN : Tendsto P atTop (𝓝 H) := SkewLSHalfT.profiled_half_normal_limit x μ hx'
    constructor
    · rintro ⟨p,hp,hm⟩
      have hfeas (v a : ℝ) (hv : 0 < v) (ha : 0 < a) :
          SkewLSHalfT.likelihood x μ v a ≤ SkewLSHalfT.likelihood x μ p.1 p.2 :=
        isMaxOn_iff.mp hm (v,a) ⟨hv,ha⟩
      have hν : 0 < p.1 := hp.1
      have hs : 0 < p.2 := hp.2
      have hM : IsMaxOn (TDist.logLik x μ p.1) (Ioi 0) p.2 := by
        intro a ha
        have h := hfeas p.1 a hν ha
        simpa [SkewLSHalfT.likelihood, TDist.likelihood,
          mul_le_mul_iff_right₀ (by positivity : 0 < (2 : ℝ)^n)] using h
      have hscore := (((TDist.heldLocation_maximum x μ hν hx').2
        (he p.1 hν).2.1).2 p.2 hs).mp hM
      have hlocal : IsLocalMax (fun ν ↦ SkewLSHalfT.likelihood x μ ν p.2) p.1 := by
        have hpos : ∀ᶠ v : ℝ in 𝓝 p.1, 0 < v := Ioi_mem_nhds hν
        exact hpos.mono fun v hv ↦ hfeas v p.2 hv hs
      have hD := (half_shape_differentiable x μ hν hs).hasDerivAt
      rw [hlocal.deriv_eq_zero] at hD
      refine ⟨hx,p.1,hν,p.2,hs,hscore,hD,?_⟩
      apply le_of_tendsto hN
      filter_upwards [eventually_gt_atTop (0 : ℝ)] with v hv
      obtain ⟨_,_,a,ha,_,hprof,_⟩ := he v hv
      change SkewLSHalfT.scaleProfile x μ v ≤ _
      rw [hprof]
      exact hfeas v a hv ha
    · rintro ⟨_,ν,hν,s,hs,_,_,hface⟩
      obtain ⟨_,_,a,_,_,ha,hm⟩ := he ν hν
      have hw : max 0 H ≤ P ν := by
        change max 0 H ≤ SkewLSHalfT.scaleProfile x μ ν
        rw [ha]
        exact max_le (mul_pos (by positivity) (exp_pos _)).le (hface.trans (hm s hs))
      obtain ⟨v,hv,hmax⟩ :=
        (open_shape_value_comparison hcont (profile_small_df x hn μ hx) hN).mpr ⟨ν,hν,hw⟩
      obtain ⟨_,_,b,hb,_,hprof,_⟩ := he v hv
      refine ⟨(v,b),⟨hv,hb⟩,fun q hq ↦ ?_⟩
      change SkewLSHalfT.likelihood x μ q.1 q.2 ≤ SkewLSHalfT.likelihood x μ v b
      rw [← hprof]
      obtain ⟨_,_,a',_,_,ha',hm'⟩ := he q.1 hq.1
      exact (hm' q.2 hq.2).trans ((Eq.symm ha').le.trans (hmax hq.1))
  · simp only [hx,false_and,iff_false]
    rintro ⟨p,hp,hm⟩
    push Not at hx
    obtain ⟨i,hi⟩ := hx
    have htie : 0 < TDist.multiplicity x μ := by
      unfold TDist.multiplicity
      exact Finset.card_pos.mpr ⟨i,by simp [hi]⟩
    obtain ⟨v,hv,ht⟩ := SkewLSHalfT.held_endpoint_free_shape_unbounded x μ htie
    have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha,hgt⟩ := (hpos.and
      (ht.eventually_gt_atTop (SkewLSHalfT.likelihood x μ p.1 p.2))).exists
    exact hgt.not_ge (isMaxOn_iff.mp hm (v,a) ⟨hv,ha⟩)

set_option maxHeartbeats 750000 in
-- Transporting derivative and maximizing predicates expands several real-valued functions.
/-- The held-location ordinary Student criterion compares a joint
stationary shape and scale to the conditional Gaussian at RMS scale.
Every sample with a zero residual is in the divergence branch. -/
theorem lst_held_location_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (μ : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x μ p.1 p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ i, x i ≠ μ) ∧ ∃ ν > 0, ∃ s > 0,
        TDist.scaleScore x μ ν s = 0 ∧
        HasDerivAt (fun v ↦ TDist.likelihood x μ v s) 0 ν ∧
        SkewLSHalfT.normalFace x μ / (2 : ℝ)^n ≤ TDist.likelihood x μ ν s := by
  dsimp only
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  have hC : 0 < (2 : ℝ)^n := by positivity
  have hM (p : ℝ × ℝ) :
      IsMaxOn (fun q : ℝ × ℝ ↦ TDist.likelihood x μ q.1 q.2) D p ↔
      IsMaxOn (fun q : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ q.1 q.2) D p := by
    simp only [isMaxOn_iff, SkewLSHalfT.likelihood, mul_le_mul_iff_right₀ hC]
  have hD (ν s : ℝ) :
      HasDerivAt (fun v ↦ SkewLSHalfT.likelihood x μ v s) 0 ν ↔
        HasDerivAt (fun v ↦ TDist.likelihood x μ v s) 0 ν := by
    constructor
    · intro h
      have h' := h.div_const ((2 : ℝ)^n)
      simpa only [SkewLSHalfT.likelihood, mul_div_cancel_left₀ _ hC.ne', zero_div] using h'
    · intro h
      simpa only [SkewLSHalfT.likelihood, mul_zero] using h.const_mul ((2 : ℝ)^n)
  have hF (ν s : ℝ) :
      SkewLSHalfT.normalFace x μ ≤ SkewLSHalfT.likelihood x μ ν s ↔
        SkewLSHalfT.normalFace x μ / (2 : ℝ)^n ≤ TDist.likelihood x μ ν s := by
    rw [div_le_iff₀ hC, SkewLSHalfT.likelihood, mul_comm ((2 : ℝ)^n)]
  have H := half_held_endpoint_criterion x hn μ
  dsimp only at H
  constructor
  · rintro ⟨p,hp,hm⟩
    obtain ⟨hx,ν,hν,s,hs,hr,hd,hf⟩ := H.mp ⟨p,hp,(hM p).mp hm⟩
    exact ⟨hx,ν,hν,s,hs,hr,(hD ν s).mp hd,(hF ν s).mp hf⟩
  · rintro ⟨hx,ν,hν,s,hs,hr,hd,hf⟩
    obtain ⟨p,hp,hm⟩ := H.mpr ⟨hx,ν,hν,s,hs,hr,(hD ν s).mpr hd,(hF ν s).mpr hf⟩
    exact ⟨p,hp,(hM p).mpr hm⟩

set_option maxHeartbeats 750000 in
-- The conditional optimizer witnesses are used at both the compact maximum and the limit.
/-- In the tie-free held-endpoint case, failure of finite half-Student
attainment leaves the RMS half-normal as the exact supremum. -/
theorem half_held_endpoint_unattained_supremum {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (μ : ℝ) (hx : ∀ i, x i ≠ μ)
    (h : ¬ ∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ),
      IsMaxOn (fun q : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ q.1 q.2)
        (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) p) :
    IsLUB ((fun q : ℝ × ℝ ↦ SkewLSHalfT.likelihood x μ q.1 q.2) ''
      (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ))) (SkewLSHalfT.normalFace x μ) := by
  let P := SkewLSHalfT.scaleProfile x μ
  let H := SkewLSHalfT.normalFace x μ
  have he (ν : ℝ) (hν : 0 < ν) := eligible_profile x hn μ hx hν
  have hcont : ContinuousOn P (Ioi 0) := fun ν hν ↦
    (SkewLSHalfT.scale_profile_continuous x μ (he ν hν).1 hν (he ν hν).2.1).continuousWithinAt
  have hN : Tendsto P atTop (𝓝 H) :=
    SkewLSHalfT.profiled_half_normal_limit x μ (he 1 one_pos).1
  have hsupp : ∀ ν > 0, P ν < H := by
    intro ν hν
    by_contra! hval
    have hp : 0 < P ν := by
      obtain ⟨_,_,s,_,_,hP,_⟩ := he ν hν
      change 0 < SkewLSHalfT.scaleProfile x μ ν
      exact hP.symm ▸ mul_pos (by positivity) (exp_pos _)
    obtain ⟨v,hv,hm⟩ := (open_shape_value_comparison hcont (profile_small_df x hn μ hx) hN).mpr
      ⟨ν,hν,max_le hp.le hval⟩
    obtain ⟨_,_,a,ha,_,heq,_⟩ := he v hv
    apply h
    refine ⟨(v,a),⟨hv,ha⟩,fun p hp ↦ ?_⟩
    obtain ⟨_,_,b,_,_,hb,hbest⟩ := he p.1 hp.1
    have hpc : SkewLSHalfT.likelihood x μ p.1 p.2 ≤ P p.1 := (hbest _ hp.2).trans_eq hb.symm
    exact (hpc.trans (hm hp.1)).trans_eq heq
  constructor
  · rintro _ ⟨⟨v,s⟩,hp,rfl⟩
    obtain ⟨_,_,a,_,_,heq,hm⟩ := he v hp.1
    exact (((hm s hp.2).trans_eq heq.symm).trans_lt (hsupp v hp.1)).le
  · intro B hB
    apply le_of_tendsto hN
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with v hv
    obtain ⟨_,_,a,ha,_,heq,_⟩ := he v hv
    exact heq.le.trans (hB ⟨(v,a),⟨hv,ha⟩,rfl⟩)

private theorem density_product {n : ℕ} (x : Fin n → ℝ) (μ ν s : ℝ) :
    TDist.likelihood x μ ν s = ∏ i, TDist.density μ ν s (x i) := by
  rw [TDist.likelihood, TDist.logLik, exp_sum]
  rfl

/-- At held positive df, location escape in either direction and
large-scale escape give zero likelihood, uniformly in the other coordinate. -/
theorem lst_fixed_df_remote_ends {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {ν : ℝ} (hν : 0 < ν) {ι : Type*} {l : Filter ι} {m s : ι → ℝ}
    (hs : ∀ᶠ j in l, 0 < s j)
    (ht : Tendsto s l atTop ∨ Tendsto m l (cocompact ℝ)) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) ν (s j)) l (𝓝 0) := by
  have hterm (i : Fin n) :
      Tendsto (fun j ↦ TDist.density (m j) ν (s j) (x i)) l (𝓝 0) := by
    rcases ht with hlarge | hfar
    · have h := tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _
        (TDist.logNormalizer ν) (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hlarge)))
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ h
      filter_upwards [hs] with j hj
      unfold TDist.density
      simp only [Function.comp_apply, exp_le_exp]
      have hK : 0 ≤ log (1+(x i-m j)^2/(ν*s j^2)) :=
        log_nonneg (le_add_of_nonneg_right (by positivity))
      linarith [mul_nonneg (show 0 ≤ (ν+1)/2 by positivity) hK]
    · have hu : Tendsto (fun b : ℝ ↦ |x i-b|) (cocompact ℝ) atTop := by
        rw [cocompact_eq_atBot_atTop]
        exact (tendsto_abs_atTop_atTop.comp
          (tendsto_atTop_add_const_left _ _ tendsto_neg_atBot_atTop)).sup
          (tendsto_abs_atBot_atTop.comp
            (tendsto_atBot_add_const_left _ _ tendsto_neg_atTop_atBot))
      have hA := hu.comp hfar
      have h := tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _
        (TDist.logNormalizer ν + log ν/2)
        (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hA)))
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ h
      filter_upwards [hs, hA.eventually_gt_atTop (0 : ℝ)] with j hj hjm
      have hj' : x i ≠ m j := by intro he; simp [Function.comp_apply, he] at hjm
      exact density_residual_bound (m j) (x i) hν hj hj'
  simpa [density_product, zero_pow hn.ne'] using
    tendsto_finsetProd Finset.univ (fun i _ ↦ hterm i)

private def movingRemainder {n : ℕ} (x : Fin n → ℝ) (b μ ν a : ℝ) : ℝ :=
  n*TDist.logNormalizer ν + (ν+1)/2 *
    (((n : ℝ) - TDist.multiplicity x b) * log ν -
      ∑ i ∈ offAtom x b, log (ν*a^2+(x i-μ)^2))

private theorem moving_collapse_bound {n : ℕ} (x : Fin n → ℝ) (b μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    TDist.logLik x μ ν a ≤ TDist.exponent x b ν * log a + movingRemainder x b μ ν a := by
  let w := (ν+1)/2
  let K (i : Fin n) := log (1+(x i-μ)^2/(ν*a^2))
  have hq : 0 < ν*a^2 := by positivity
  have hK (i : Fin n) : 0 ≤ K i :=
    log_nonneg (le_add_of_nonneg_right (div_nonneg (sq_nonneg _) hq.le))
  have hApprox : (∑ i ∈ offAtom x b, K i) ≤ ∑ i, K i :=
    Finset.sum_le_sum_of_subset_of_nonneg (Finset.filter_subset _ _) (fun i _ _ ↦ hK i)
  have hEq (i : Fin n) :
      K i = log (ν*a^2+(x i-μ)^2) - log ν - 2*log a := by
    unfold K
    rw [show 1+(x i-μ)^2/(ν*a^2) = (ν*a^2+(x i-μ)^2)/(ν*a^2) by field_simp,
      log_div (by positivity) hq.ne', log_mul hν.ne' (by positivity), log_pow]
    norm_num
    ring
  calc
    TDist.logLik x μ ν a = n*(TDist.logNormalizer ν - log a) - w*(∑ i, K i) := by
      unfold TDist.logLik
      simp [Finset.sum_sub_distrib, ← Finset.mul_sum, K, w, mul_sub]
    _ ≤ n*(TDist.logNormalizer ν - log a) - w*(∑ i ∈ offAtom x b, K i) :=
      sub_le_sub_left (mul_le_mul_of_nonneg_left hApprox (by dsimp [w]; positivity)) _
    _ = _ := by
      simp_rw [hEq]
      rw [Finset.sum_sub_distrib, Finset.sum_sub_distrib, Finset.sum_const, Finset.sum_const,
        nsmul_eq_mul, nsmul_eq_mul, offAtom_count]
      unfold TDist.exponent movingRemainder w
      ring

/-- A joint small-scale path at a finite location has vanishing likelihood
at positive exponent. At zero exponent its sharp upper envelope is the
vertical collapse limit, which is finite and positive. -/
theorem lst_fixed_df_local_collapse {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {ν : ℝ}
    (hν : 0 < ν) {ι : Type*} {l : Filter ι} {m a : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) :
    (0 < TDist.exponent x b ν →
      Tendsto (fun j ↦ TDist.likelihood x (m j) ν (a j)) l (𝓝 0)) ∧
    (TDist.exponent x b ν = 0 →
      Tendsto (TDist.likelihood x b ν) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x b ν))) ∧
      ∀ B > exp (TDist.collapseLogValue x b ν),
        ∀ᶠ j in l, TDist.likelihood x (m j) ν (a j) < B) := by
  have hpos := (tendsto_nhdsWithin_iff.mp ha).2
  have ha0 := (tendsto_nhdsWithin_iff.mp ha).1
  have hR : Tendsto (fun j ↦ movingRemainder x b (m j) ν (a j)) l
      (𝓝 (TDist.collapseLogValue x b ν)) := by
    have hterm (i : Fin n) (hi : i ∈ offAtom x b) :
        Tendsto (fun j ↦ log (ν*a j^2+(x i-m j)^2)) l (𝓝 (log ((x i-b)^2))) := by
      have hp := sq_pos_of_ne_zero (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2)
      simpa using (((ha0.pow 2).const_mul ν).add
        (((tendsto_const_nhds (x := x i)).sub hm).pow 2)).log (by simpa using hp.ne')
    exact ((tendsto_const_nhds.sub (tendsto_finsetSum _ hterm)).const_mul _).const_add _
  have hbound : ∀ᶠ j in l, TDist.logLik x (m j) ν (a j) ≤
      TDist.exponent x b ν * log (a j) + movingRemainder x b (m j) ν (a j) :=
    hpos.mono fun j hj ↦ moving_collapse_bound x b (m j) hν hj
  constructor
  · intro hE
    have ht := ((tendsto_log_nhdsGT_zero.comp ha).const_mul_atBot hE).atBot_add hR
    exact tendsto_exp_atBot.comp (tendsto_atBot_mono' l hbound ht)
  · intro hE
    refine ⟨?_,fun B hB ↦ ?_⟩
    · simpa [hE] using (TDist.collapse_power x b hν).1
    · have ht := continuous_exp.continuousAt.tendsto.comp hR
      filter_upwards [hbound, ht.eventually (Iio_mem_nhds hB)] with j hj hj'
      rw [hE, zero_mul, zero_add] at hj
      exact (exp_le_exp.mpr hj).trans_lt hj'

private theorem closed_shape_maximum {f : ℝ → ℝ} {l B v : ℝ}
    (hc : ContinuousOn f (Ici l)) (hB : Tendsto f atTop (𝓝 B))
    (hv : l ≤ v) (hw : B ≤ f v) :
    ∃ p ∈ Ici l, IsMaxOn f (Ici l) p := by
  by_cases hm : IsMaxOn f (Ici l) v
  · exact ⟨v,hv,hm⟩
  rw [isMaxOn_iff] at hm
  push Not at hm
  obtain ⟨w,hw',hbetter⟩ := hm
  obtain ⟨u,hu⟩ := eventually_atTop.mp (hB.eventually (Iio_mem_nhds (hw.trans_lt hbetter)))
  let U := max w u
  have hU : w ∈ Icc l U := ⟨hw',le_max_left _ _⟩
  have hsub : Icc l U ⊆ Ici l := Icc_subset_Ici_self
  obtain ⟨p,hp,hM⟩ := isCompact_Icc.exists_isMaxOn ⟨w,hU⟩ (hc.mono hsub)
  refine ⟨p,hsub hp,fun q hq ↦ ?_⟩
  by_cases h : q ∈ Icc l U
  · exact hM h
  · have hq' : U < q := lt_of_not_ge (fun h' ↦ h ⟨hq,h'⟩)
    exact (hu q ((le_max_right w u).trans hq'.le)).le.trans (hM hU)

private theorem minimum_profile {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {ν s : ℝ} (hν : 0 < ν) (hs : 0 < s)
    (μ : ℝ) (hμ : ∀ j, μ ≤ x j) :
    SkewLSHalfT.likelihood x μ ν s ≤ SkewLSHalfT.likelihood x (x i) ν s := by
  obtain ⟨j,hj,H⟩ := StudentHalf.held_shape_scale_endpoint x i.isLt.pos hν hs
  have he : x i = x j := le_antisymm (hmin j) (hj i)
  exact (H (x i)).mpr he |>.2 hμ

set_option maxHeartbeats 650000 in
-- The endpoint profile and finite shape comparison both use the original two-coordinate domain.
/-- At held scale, finite half-Student attainment is equivalent to an
included floor or a stationary shape at the sample minimum reaching the
matched half-normal value. -/
theorem half_held_scale_criterion {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {s : ℝ} (hs : 0 < s) :
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x p.1 p.2 s
    let P := fun v ↦ SkewLSHalfT.likelihood x (x i) v s
    let H := ∏ j, 2 * SkewLSGaussian.φ ((x j-x i)/s)/s
    (∃ p ∈ D, IsMaxOn L D p) ↔
      ∃ v ≥ 1, (v = 1 ∨ HasDerivAt P 0 v) ∧ H ≤ P v := by
  dsimp only
  let P := fun v ↦ SkewLSHalfT.likelihood x (x i) v s
  have hN := SkewLSHalfT.half_normal_path x (x i) hs
  constructor
  · rintro ⟨p,hp,hm⟩
    have hν : 1 ≤ p.2 := hp.2
    have hmp : SkewLSHalfT.likelihood x p.1 p.2 s ≤ P p.2 :=
      minimum_profile x hmin (zero_lt_one.trans_le hν) hs p.1 hp.1
    have htop (v : ℝ) (hv : 1 ≤ v) : P v ≤ P p.2 :=
      (isMaxOn_iff.mp hm (x i,v) ⟨hmin,hv⟩).trans hmp
    refine ⟨p.2,hν,?_,le_of_tendsto hN ((eventually_ge_atTop (1 : ℝ)).mono htop)⟩
    rcases eq_or_lt_of_le hν with h | h
    · exact Or.inl h.symm
    · have hloc : IsLocalMax P p.2 := by
        have A : ∀ᶠ v : ℝ in 𝓝 p.2, 1 < v := Ioi_mem_nhds h
        exact A.mono fun v hv ↦ htop v hv.le
      have hD := (half_shape_differentiable x (x i) (zero_lt_one.trans h) hs).hasDerivAt
      have hzero : deriv (fun v ↦ SkewLSHalfT.likelihood x (x i) v s) p.2 = 0 :=
        hloc.deriv_eq_zero
      rw [hzero] at hD
      exact Or.inr hD
  · rintro ⟨v,hv,_,hval⟩
    have hcont : ContinuousOn P (Ici 1) := fun w hw ↦
      (half_shape_differentiable x (x i) (zero_lt_one.trans_le hw) hs).continuousAt.continuousWithinAt
    obtain ⟨w,hw,hM⟩ := closed_shape_maximum hcont hN hv hval
    refine ⟨(x i,w),⟨hmin,hw⟩,fun q hq ↦ ?_⟩
    exact (minimum_profile x hmin (zero_lt_one.trans_le hq.2) hs q.1 hq.1).trans (hM hq.2)

/-- At held half-Student scale, whenever a finite maximizer is absent,
the matched half-normal at the sample minimum is the exact supremum. -/
theorem half_held_scale_unattained_supremum {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hmin : ∀ j, x i ≤ x j) {s : ℝ} (hs : 0 < s)
    (h : ¬ ∃ p ∈ {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2},
      IsMaxOn (fun q ↦ SkewLSHalfT.likelihood x q.1 q.2 s)
        {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2} p) :
    IsLUB ((fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x p.1 p.2 s) ''
      {p | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2})
      (∏ j, 2 * SkewLSGaussian.φ ((x j-x i)/s)/s) := by
  let P := fun v ↦ SkewLSHalfT.likelihood x (x i) v s
  let H := ∏ j, 2 * SkewLSGaussian.φ ((x j-x i)/s)/s
  have hN : Tendsto P atTop (𝓝 H) := SkewLSHalfT.half_normal_path x (x i) hs
  have hcont : ContinuousOn P (Ici 1) := fun v hv ↦
    (half_shape_differentiable x (x i) (zero_lt_one.trans_le hv) hs).continuousAt.continuousWithinAt
  have hH (v : ℝ) (hv : 1 ≤ v) : P v < H := by
    by_contra! hv'
    obtain ⟨w,hw,hmax⟩ := closed_shape_maximum hcont hN hv hv'
    exact h ⟨(x i,w),⟨hmin,hw⟩,fun p hp ↦
      (minimum_profile x hmin (zero_lt_one.trans_le hp.2) hs p.1 hp.1).trans (hmax hp.2)⟩
  constructor
  · rintro _ ⟨p,hp,rfl⟩
    exact (minimum_profile x hmin (zero_lt_one.trans_le hp.2) hs p.1 hp.1).trans (hH _ hp.2).le
  · intro z hz
    apply le_of_tendsto hN
    filter_upwards [eventually_ge_atTop (1 : ℝ)] with v hv
    exact hz ⟨(x i,v),⟨hmin,hv⟩,rfl⟩

end
end UnivariateML.StudentFaces
end
