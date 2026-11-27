module

public import UnivariateML.Family.StudentFaces
public import UnivariateML.Family.Student2Normal

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Small-scale Student paths with moving shape

These bounds address the finite positive limiting-shape component of
escape classification. The remote and Gaussian ends, sharp paths,
and candidate comparisons are separate obligations.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Student2MovingCollapse

noncomputable section

-- The fixed-shape proof in StudentFaces uses the same local algebra.
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


private def offAtom {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) : Finset (Fin n) :=
  Finset.univ.filter fun i ↦ x i ≠ μ

private theorem offAtom_count {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) :
    ((offAtom x μ).card : ℝ) = n - TDist.multiplicity x μ := by
  have h : TDist.multiplicity x μ + (offAtom x μ).card = n := by
    unfold TDist.multiplicity offAtom
    rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
  have h' : (TDist.multiplicity x μ : ℝ) + (offAtom x μ).card = n := by exact_mod_cast h
  linarith

private def movingRemainder {n : ℕ} (x : Fin n → ℝ) (b μ ν a : ℝ) : ℝ :=
  n*TDist.logNormalizer ν + (ν+1)/2 *
    (((n : ℝ) - TDist.multiplicity x b) * log ν -
      ∑ i ∈ offAtom x b, log (ν*a^2+(x i-μ)^2))

private theorem moving_collapse_formula {n : ℕ} (x : Fin n → ℝ) (b μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    TDist.logLik x μ ν a = TDist.exponent x b ν * log a + movingRemainder x b μ ν a -
      (ν+1)/2 * ((∑ i, log (1+(x i-μ)^2/(ν*a^2))) -
        ∑ i ∈ offAtom x b, log (1+(x i-μ)^2/(ν*a^2))) := by
  let w := (ν+1)/2
  let K (i : Fin n) := log (1+(x i-μ)^2/(ν*a^2))
  have hq : 0 < ν*a^2 := by positivity
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
    _ = (n*(TDist.logNormalizer ν - log a) - w*(∑ i ∈ offAtom x b, K i)) -
          w*((∑ i, K i) - ∑ i ∈ offAtom x b, K i) := by ring
    _ = _ := by
      congr 1
      simp_rw [hEq]
      rw [Finset.sum_sub_distrib, Finset.sum_sub_distrib, Finset.sum_const, Finset.sum_const,
        nsmul_eq_mul, nsmul_eq_mul, offAtom_count]
      unfold TDist.exponent movingRemainder w
      ring


private theorem moving_collapse_bound {n : ℕ} (x : Fin n → ℝ) (b μ : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    TDist.logLik x μ ν a ≤ TDist.exponent x b ν * log a + movingRemainder x b μ ν a := by
  rw [moving_collapse_formula x b μ hν ha]
  have hq : 0 < ν*a^2 := by positivity
  have hsub : (∑ i ∈ offAtom x b, log (1+(x i-μ)^2/(ν*a^2))) ≤
      ∑ i, log (1+(x i-μ)^2/(ν*a^2)) :=
    Finset.sum_le_sum_of_subset_of_nonneg (Finset.filter_subset _ _) fun _ _ _ ↦
      log_nonneg (le_add_of_nonneg_right (div_nonneg (sq_nonneg _) hq.le))
  exact sub_le_self _ (mul_nonneg (by positivity) (sub_nonneg.mpr hsub))

private theorem vertical_collapse_formula {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {ν a : ℝ}
    (hν : 0 < ν) (ha : 0 < a) :
    TDist.logLik x b ν a = TDist.exponent x b ν * log a + movingRemainder x b b ν a := by
  rw [moving_collapse_formula x b b hν ha]
  have hsum : (∑ i ∈ offAtom x b, log (1+(x i-b)^2/(ν*a^2))) =
      ∑ i, log (1+(x i-b)^2/(ν*a^2)) := by
    apply Finset.sum_subset (Finset.filter_subset _ _)
    intro i _ hi
    have hx : x i = b := by simpa [offAtom] using hi
    simp [hx]
  rw [hsum, sub_self, mul_zero, sub_zero]

private theorem remainder_limit {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha0 : Tendsto a l (𝓝 0))
    (hv : Tendsto v l (𝓝 q)) :
    Tendsto (fun j ↦ movingRemainder x b (m j) (v j) (a j)) l
      (𝓝 (TDist.collapseLogValue x b q)) := by
  have hterm (i : Fin n) (hi : i ∈ offAtom x b) :
      Tendsto (fun j ↦ log (v j * a j ^ 2 + (x i - m j)^2)) l (𝓝 (log ((x i-b)^2))) := by
    have hp := sq_pos_of_ne_zero (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2)
    simpa using ((hv.mul (ha0.pow 2)).add
      (((tendsto_const_nhds (x := x i)).sub hm).pow 2)).log (by simpa using hp.ne')
  exact (((normalizer_continuous hq).tendsto.comp hv).const_mul (n : ℝ)).add
    (((hv.add_const 1).div_const 2).mul
      (((hv.log hq.ne').const_mul ((n : ℝ) - TDist.multiplicity x b)).sub
        (tendsto_finsetSum _ hterm)))

/-- Small-scale paths approaching a positive limiting shape vanish at
positive exponent. At criticality, approaching shape from above gives
the vertical collapse height as an upper bound on the limit superior. -/
theorem moving_shape_collapse {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0))
    (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x (m j) (v j) (a j)
    (0 < TDist.exponent x b q → Tendsto L l (𝓝 0)) ∧
    (TDist.exponent x b q = 0 → (∀ᶠ j in l, q ≤ v j) →
      ∀ B > exp (TDist.collapseLogValue x b q), ∀ᶠ j in l, L j < B) := by
  dsimp only
  have hpos := (tendsto_nhdsWithin_iff.mp ha).2
  have ha0 := (tendsto_nhdsWithin_iff.mp ha).1
  have hR := remainder_limit x b hq hm ha0 hv
  have hbound : ∀ᶠ j in l, TDist.logLik x (m j) (v j) (a j) ≤
      TDist.exponent x b (v j) * log (a j) + movingRemainder x b (m j) (v j) (a j) := by
    filter_upwards [hpos, hv.eventually (Ioi_mem_nhds hq)] with j hj hjv
    exact moving_collapse_bound x b (m j) hjv hj
  have he : Tendsto (fun j ↦ TDist.exponent x b (v j)) l (𝓝 (TDist.exponent x b q)) := by
    unfold TDist.exponent
    exact (hv.mul_const _).sub_const _
  have halog := tendsto_log_nhdsGT_zero.comp ha
  constructor
  · intro hEq
    have hevent := he.eventually (Ioi_mem_nhds (half_lt_self hEq))
    have hUpper : ∀ᶠ j in l, TDist.logLik x (m j) (v j) (a j) ≤
        TDist.exponent x b q / 2 * log (a j) + movingRemainder x b (m j) (v j) (a j) := by
      filter_upwards [hbound, hevent, halog.eventually_lt_atBot 0] with j hj hje hjlog
      exact hj.trans (add_le_add_left (mul_le_mul_of_nonpos_right hje.le hjlog.le) _)
    have ht := (halog.const_mul_atBot (half_pos hEq)).atBot_add hR
    exact tendsto_exp_atBot.comp (tendsto_atBot_mono' l hUpper ht)
  · intro hEq hge B hB
    have hoff : 0 ≤ (n : ℝ) - TDist.multiplicity x b := by rw [← offAtom_count]; positivity
    have ht := continuous_exp.continuousAt.tendsto.comp hR
    filter_upwards [hbound, hge, halog.eventually_lt_atBot 0,
      ht.eventually (Iio_mem_nhds hB)] with j hj hvj hjlog hlt
    have hej : 0 ≤ TDist.exponent x b (v j) := by
      unfold TDist.exponent at hEq ⊢
      linarith [mul_le_mul_of_nonneg_right hvj hoff]
    have hz : log (a j) ≤ 0 := hjlog.le
    have hb : TDist.logLik x (m j) (v j) (a j) ≤ movingRemainder x b (m j) (v j) (a j) :=
      hj.trans (add_le_of_nonpos_left (mul_nonpos_of_nonneg_of_nonpos hej hz))
    exact (exp_le_exp.mpr hb).trans_lt hlt

/-- At held location and a finite critical shape, a finite signed rate
adds the off-atom count times that rate to the log collapse height. -/
theorem critical_vertical_finite_rate {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    (he : TDist.exponent x b q = 0) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    {r : ℝ} (hr : Tendsto (fun j ↦ (v j - q) * log (a j)) l (𝓝 r)) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l
      (𝓝 (exp (((n : ℝ) - TDist.multiplicity x b) * r + TDist.collapseLogValue x b q))) := by
  obtain ⟨ha0, hpos⟩ := tendsto_nhdsWithin_iff.mp ha
  have hR := remainder_limit x b hq tendsto_const_nhds ha0 hv
  have hc : Tendsto (fun j ↦ TDist.exponent x b (v j) * log (a j)) l (𝓝 (((n : ℝ) - TDist.multiplicity x b) * r)) := by
    have H := hr.const_mul ((n : ℝ) - TDist.multiplicity x b)
    refine H.congr' (Eventually.of_forall fun j ↦ ?_)
    have hb : TDist.exponent x b (v j) = ((n : ℝ) - TDist.multiplicity x b) * (v j - q) := by
      unfold TDist.exponent at he ⊢
      linear_combination he
    change _ = TDist.exponent x b (v j) * log (a j)
    rw [hb, mul_assoc]
  apply continuous_exp.continuousAt.tendsto.comp
  have H := hc.add hR
  refine H.congr' ?_
  filter_upwards [hpos, hv.eventually (Ioi_mem_nhds hq)] with j hj hjv
  exact (vertical_collapse_formula x b hjv hj).symm

/-- A vertical critical collapse with vanishing shape-log-scale product
attains the critical envelope in the limit. -/
theorem critical_vertical_rate {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q : ℝ} (hq : 0 < q)
    (he : TDist.exponent x b q = 0) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q))
    (hr : Tendsto (fun j ↦ (v j - q) * log (a j)) l (𝓝 0)) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l
      (𝓝 (exp (TDist.collapseLogValue x b q))) := by
  simpa only [mul_zero, zero_add] using critical_vertical_finite_rate x b hq he ha hv hr

/-- At held location, approaching a negative limiting exponent through
positive finite shapes and vanishing scale forces divergence. At
criticality, the sign and magnitude of the shape-log-scale product
control divergence and vanishing on either side. -/
theorem held_location_positive_shape_collapse {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x b (v j) (a j)
    let rate := fun j ↦ (v j - q) * log (a j)
    (TDist.exponent x b q < 0 → Tendsto L l atTop) ∧
    (TDist.exponent x b q = 0 →
      (Tendsto rate l atTop → Tendsto L l atTop) ∧
      (Tendsto rate l atBot → Tendsto L l (𝓝 0))) := by
  dsimp only
  obtain ⟨ha0, hpos⟩ := tendsto_nhdsWithin_iff.mp ha
  have hR := remainder_limit x b hq tendsto_const_nhds ha0 hv
  have hEq : (fun j ↦ TDist.logLik x b (v j) (a j)) =ᶠ[l]
      fun j ↦ TDist.exponent x b (v j) * log (a j) + movingRemainder x b b (v j) (a j) := by
    filter_upwards [hpos, hv.eventually (Ioi_mem_nhds hq)] with j hj hjv
    exact vertical_collapse_formula x b hjv hj
  constructor
  · intro he
    have he' : Tendsto (fun j ↦ TDist.exponent x b (v j)) l (𝓝 (TDist.exponent x b q)) := by
      unfold TDist.exponent
      exact (hv.mul_const _).sub_const _
    have hlog := tendsto_log_nhdsGT_zero.comp ha
    have hf : ∀ᶠ j in l, TDist.exponent x b q / 2 * log (a j) +
        movingRemainder x b b (v j) (a j) ≤ TDist.logLik x b (v j) (a j) := by
      filter_upwards [hEq, he'.eventually (Iio_mem_nhds (show
        TDist.exponent x b q < TDist.exponent x b q / 2 by linarith)),
        hlog.eventually_lt_atBot 0] with j hj hj' hlogj
      change TDist.logLik x b (v j) (a j) = _ at hj
      rw [hj]
      exact add_le_add_left (mul_le_mul_of_nonpos_right hj'.le hlogj.le) _
    exact tendsto_exp_atTop.comp (tendsto_atTop_mono' l hf
      ((hlog.const_mul_atBot_of_neg (show TDist.exponent x b q / 2 < 0 by linarith)).atTop_add hR))
  · intro he
    have hoff : 0 < (n : ℝ) - TDist.multiplicity x b := by
      by_contra! h
      have hk : (0 : ℝ) < TDist.multiplicity x b := by
        have hn' : (0 : ℝ) < n := by exact_mod_cast hn
        linarith
      unfold TDist.exponent at he
      linarith [mul_nonpos_of_nonneg_of_nonpos hq.le h]
    have heq (j : ι) : TDist.exponent x b (v j) * log (a j) =
        ((n : ℝ) - TDist.multiplicity x b) * ((v j - q) * log (a j)) := by
      unfold TDist.exponent at he ⊢
      linear_combination log (a j) * he
    have hEq' := hEq.trans (Eventually.of_forall fun j ↦ by
      change TDist.exponent x b (v j) * log (a j) + _ = _
      rw [heq])
    exact ⟨fun h ↦ tendsto_exp_atTop.comp
      (((h.const_mul_atTop hoff).atTop_add hR).congr' hEq'.symm),
      fun h ↦ tendsto_exp_atBot.comp
        (((h.const_mul_atBot hoff).atBot_add hR).congr' hEq'.symm)⟩

private def smallShapeRemainder (v : ℝ) : ℝ :=
  log (Gamma ((v+1)/2)) - log (Gamma (v/2+1)) - log π/2 - log 2

private theorem smallShapeRemainder_continuous : ContinuousAt smallShapeRemainder 0 := by
  unfold smallShapeRemainder
  refine ContinuousAt.sub (ContinuousAt.sub (ContinuousAt.sub ?_ ?_) continuousAt_const)
    continuousAt_const
  · exact (logGamma_continuous_pos (show 0 < ((0 : ℝ)+1)/2 by norm_num)).comp'
      (f := fun v : ℝ ↦ (v+1)/2) (x := 0) (by fun_prop)
  · exact (logGamma_continuous_pos (show 0 < (0 : ℝ)/2+1 by norm_num)).comp'
      (f := fun v : ℝ ↦ v/2+1) (x := 0) (by fun_prop)

private theorem smallShapeFormula {v : ℝ} (hv : 0 < v) :
    TDist.logNormalizer v = log v / 2 + smallShapeRemainder v := by
  have hh : 0 < v/2 := by positivity
  dsimp [smallShapeRemainder, TDist.logNormalizer]
  rw [Gamma_add_one hh.ne', log_mul hh.ne' (Gamma_pos_of_pos hh).ne',
    log_div hv.ne' (by norm_num), log_mul hv.ne' pi_pos.ne']
  ring

private theorem normalizer_small : Tendsto TDist.logNormalizer (𝓝[>] 0) atBot := by
  refine ((tendsto_log_nhdsGT_zero.const_mul_atBot (by norm_num : 0 < (1 : ℝ)/2)).atBot_add
    (smallShapeRemainder_continuous.tendsto.mono_left nhdsWithin_le_nhds)).congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with v hv
  change (1/2) * log v + smallShapeRemainder v = TDist.logNormalizer v
  rw [smallShapeFormula hv]
  ring

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


/-- With location held, a small-shape path vanishes if all residuals are
nonzero, uniformly over positive scales. With contacts it still vanishes
when scale is eventually bounded below by a positive constant. -/
theorem held_location_zero_shape_vanishes {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : ∀ᶠ j in l, 0 < a j) (hv : Tendsto v l (𝓝[>] 0))
    (h : (∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l (𝓝 0) := by
  have hG := normalizer_small.comp hv
  have hpos := (tendsto_nhdsWithin_iff.mp hv).2
  have hsmall := (tendsto_nhdsWithin_iff.mp hv).1.eventually (Iio_mem_nhds zero_lt_one)
  have hD (i : Fin n) : Tendsto (fun j ↦ TDist.density b (v j) (a j) (x i)) l (𝓝 0) := by
    rcases h with h | ⟨s,hs,hscale⟩
    · have ht := tendsto_exp_atBot.comp (tendsto_atBot_add_const_right _ (-log |x i-b|) hG)
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ ht
      filter_upwards [ha, hpos, hsmall] with j hja hjv hjv'
      have hbound := density_residual_bound b (x i) hjv hja (h i)
      exact hbound.trans (exp_le_exp.mpr (by
        change v j < 1 at hjv'
        change TDist.logNormalizer (v j) + log (v j) / 2 - log |x i - b| ≤
          TDist.logNormalizer (v j) - log |x i - b|
        linarith [log_nonpos hjv.le hjv'.le]))
    · have ht := tendsto_exp_atBot.comp (tendsto_atBot_add_const_right _ (-log s) hG)
      refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ ht
      filter_upwards [ha,hpos,hscale] with j hja hjv hjscale
      have hjv' : 0 < v j := hjv
      have hk : 0 ≤ log (1+(x i-b)^2/(v j*(a j)^2)) :=
        log_nonneg (le_add_of_nonneg_right (by positivity))
      apply exp_le_exp.mpr
      change TDist.logNormalizer (v j) - log (a j) -
        (v j+1)/2 * log (1+(x i-b)^2/(v j*(a j)^2)) ≤ TDist.logNormalizer (v j) - log s
      linarith [log_le_log hs hjscale, mul_nonneg (show 0 ≤ (v j+1)/2 by positivity) hk]
  have H := tendsto_finsetProd Finset.univ fun i _ ↦ hD i
  rw [show (∏ _ : Fin n, (0 : ℝ)) = 0 by simp [hn.ne']] at H
  refine H.congr' (Eventually.of_forall fun j ↦ ?_)
  change (∏ i, TDist.density b (v j) (a j) (x i)) = TDist.likelihood x b (v j) (a j)
  rw [TDist.likelihood, TDist.logLik, exp_sum]
  rfl

private theorem normalizer_limit :
    Tendsto TDist.logNormalizer atTop (𝓝 (-(log (2 * π) / 2))) := by
  have h := TDist.density_tendsto_normal 0 0 (s := 1) (a := fun _ ↦ 1)
    zero_lt_one tendsto_const_nhds
  have h' : Tendsto (fun v ↦ exp (TDist.logNormalizer v)) atTop
      (𝓝 (exp (-(log (2 * π) / 2)))) := by
    simpa [TDist.density, TDist.normalDensity] using h
  simpa only [log_exp] using h'.log (exp_ne_zero _)


/-- If all residuals vanish, the zero-scale corner at infinite shape
diverges on every positive-scale approach. -/
theorem held_location_all_tied_normal_corner {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (hx : ∀ i, x i = b) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l atTop) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l atTop := by
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have ht := (tendsto_log_nhdsGT_zero.comp ha).const_mul_atBot hn'
  have H := tendsto_exp_atTop.comp
    ((tendsto_neg_atBot_atTop.comp ht).atTop_add ((normalizer_limit.comp hv).const_mul (n : ℝ)))
  refine H.congr' (Eventually.of_forall fun j ↦ ?_)
  change exp (-(n * log (a j)) + n * TDist.logNormalizer (v j)) = _
  unfold TDist.likelihood TDist.logLik
  simp [hx, sub_eq_add_neg, add_comm]

private theorem square_right_zero :
    Tendsto (fun t : ℝ ↦ t^2) (𝓝[>] 0) (𝓝[>] 0) := by
  have h0 : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  refine tendsto_nhdsWithin_iff.mpr ⟨by simpa using h0.pow 2, ?_⟩
  have hp : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  exact hp.mono fun _ h ↦ sq_pos_of_pos h

private theorem exponential_collapse :
    Tendsto (fun t : ℝ ↦ exp (-(t^2)⁻¹)) (𝓝[>] 0) (𝓝[>] 0) :=
  tendsto_exp_atBot_nhdsGT.comp
    (tendsto_neg_atTop_atBot.comp (tendsto_inv_nhdsGT_zero.comp square_right_zero))

/-- A held-location contact makes the simultaneous zero-shape and
zero-scale corner capable of divergence. The exponential scale path
also works when every observation is a contact. -/
theorem held_location_zero_shape_divergent_path {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hk : 0 < TDist.multiplicity x b) :
    Tendsto (fun t : ℝ ↦ TDist.likelihood x b t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop := by
  let a := fun t : ℝ ↦ exp (-(t^2)⁻¹)
  let p : ℝ := n - TDist.multiplicity x b
  let k : ℝ := TDist.multiplicity x b
  have hk' : 0 < k := by dsimp [k]; exact_mod_cast hk
  have hp : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  have h0 : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have h2 := (tendsto_nhdsWithin_iff.mp square_right_zero).1
  have ha0 := (tendsto_nhdsWithin_iff.mp exponential_collapse).1
  have hW : Tendsto (fun t : ℝ ↦ t^2 * log t) (𝓝[>] 0) (𝓝 0) := by
    have h := h0.mul (continuous_mul_log.continuousAt.tendsto.comp h0)
    have H : Tendsto (fun t : ℝ ↦ t * (t * log t)) (𝓝[>] 0) (𝓝 0) := by
      simpa only [id_eq, Function.comp_id, zero_mul] using h
    refine H.congr fun t ↦ ?_
    ring
  have hN : Tendsto (fun t : ℝ ↦ t^2 * TDist.logNormalizer t) (𝓝[>] 0) (𝓝 0) := by
    have H := (hW.div_const 2).add (h2.mul
      (smallShapeRemainder_continuous.tendsto.mono_left nhdsWithin_le_nhds))
    simp only [zero_div, zero_mul, add_zero] at H
    refine H.congr' ?_
    filter_upwards [hp] with t ht
    rw [smallShapeFormula ht]
    ring
  let S := fun t : ℝ ↦ ∑ i ∈ offAtom x b, log (t * (a t)^2 + (x i-b)^2)
  have hS : Tendsto S (𝓝[>] 0) (𝓝 (∑ i ∈ offAtom x b, log ((x i-b)^2))) := by
    apply tendsto_finsetSum
    intro i hi
    have hterm := ((h0.mul (ha0.pow 2)).add_const ((x i-b)^2)).log (by
      simpa using (sq_pos_of_ne_zero (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2)).ne')
    simpa only [id_eq, zero_mul, zero_add] using hterm
  have hE : Tendsto (fun t : ℝ ↦ -TDist.exponent x b t) (𝓝[>] 0) (𝓝 k) := by
    simpa [TDist.exponent, k, p] using ((h0.mul_const p).sub_const k).neg
  have hS' : Tendsto (fun t : ℝ ↦ p * (t^2 * log t) - t^2 * S t) (𝓝[>] 0) (𝓝 0) := by
    simpa only [mul_zero, zero_mul, sub_zero] using (hW.const_mul p).sub (h2.mul hS)
  have H := ((hN.const_mul (n : ℝ)).add hE).add ((h0.add_const 1 |>.div_const 2).mul hS')
  simp only [mul_zero, zero_add, add_zero] at H
  have hScaled : Tendsto (fun t : ℝ ↦ t^2 * TDist.logLik x b t (a t)) (𝓝[>] 0) (𝓝 k) := by
    refine H.congr' ?_
    filter_upwards [hp] with t ht
    have ha : 0 < a t := exp_pos _
    have hlog : t^2 * log (a t) = -1 := by
      dsimp [a]
      rw [log_exp, mul_neg, mul_inv_cancel₀ (pow_ne_zero 2 ht.ne')]
    change (n : ℝ) * (t^2 * TDist.logNormalizer t) + -TDist.exponent x b t +
      (t+1)/2 * (p * (t^2 * log t) - t^2 * S t) = _
    rw [vertical_collapse_formula x b ht ha]
    unfold movingRemainder
    change _ = t^2 * (TDist.exponent x b t * log (a t) +
      (n * TDist.logNormalizer t + (t+1)/2 * (p * log t - S t)))
    rw [show t^2 * (TDist.exponent x b t * log (a t) +
      (n * TDist.logNormalizer t + (t+1)/2 * (p * log t - S t))) =
      TDist.exponent x b t * (t^2 * log (a t)) +
        (n * (t^2 * TDist.logNormalizer t) + (t+1)/2 * (p * (t^2 * log t) - t^2*S t))
      by ring, hlog]
    ring
  apply tendsto_exp_atTop.comp
  refine ((tendsto_inv_nhdsGT_zero.comp square_right_zero).atTop_mul_pos hk' hScaled).congr' ?_
  filter_upwards [hp] with t ht
  change (t^2)⁻¹ * (t^2 * TDist.logLik x b t (a t)) = _
  rw [← mul_assoc, inv_mul_cancel₀ (pow_ne_zero 2 ht.ne'), one_mul]

/-- A critical positive shape on an unrestricted shape chart has a
divergent approach from below. The exponential scale path makes the
signed shape-log-scale rate tend to positive infinity. -/
theorem held_location_critical_below_path {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0) :
    Tendsto (fun t : ℝ ↦ TDist.likelihood x b (q-t) (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop := by
  have h0 : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hq' : Tendsto (fun t : ℝ ↦ q-t) (𝓝[>] 0) (𝓝 q) := by
    simpa only [sub_zero, id_eq] using (tendsto_const_nhds (x := q)).sub h0
  apply ((held_location_positive_shape_collapse x hn b hq exponential_collapse hq').2 he).1
  refine tendsto_inv_nhdsGT_zero.congr' ?_
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
  change t⁻¹ = (q-t-q) * log (exp (-(t^2)⁻¹))
  rw [log_exp]
  have hne : t ≠ 0 := ne_of_gt ht
  field_simp [hne]
  ring

private theorem normalizer_bounded : ∃ C, ∀ v > 0, TDist.logNormalizer v ≤ C := by
  obtain ⟨V,hV⟩ := eventually_atTop.mp (normalizer_limit.eventually
    (Iio_mem_nhds (show -(log (2*π)/2) < -(log (2*π)/2) + 1 by linarith)))
  have hsmall := normalizer_small.eventually_lt_atBot 0
  rw [eventually_nhdsWithin_iff] at hsmall
  obtain ⟨ε,hε,hs⟩ := Metric.eventually_nhds_iff.mp hsmall
  have hs' : ∀ v ∈ Ioo 0 ε, TDist.logNormalizer v < 0 := by
    intro v hv
    have hv' : dist v 0 < ε := by
      rw [Real.dist_eq, sub_zero, abs_of_pos hv.1]
      exact hv.2
    exact hs hv' hv.1
  have hc : ContinuousOn TDist.logNormalizer (Icc ε V) :=
    fun v hv ↦ (normalizer_continuous (hε.trans_le hv.1)).continuousWithinAt
  obtain ⟨B,hB⟩ := isCompact_Icc.bddAbove_image hc
  refine ⟨max 0 (max B (-(log (2*π)/2) + 1)), fun v hv ↦ ?_⟩
  by_cases hv' : v < ε
  · exact (hs' v ⟨hv,hv'⟩).le.trans (le_max_left _ _)
  by_cases hv'' : V ≤ v
  · exact (hV v hv'').le.trans ((le_max_right _ _).trans (le_max_right _ _))
  · exact (hB ⟨v,⟨le_of_not_gt hv',le_of_not_ge hv''⟩,rfl⟩).trans
      ((le_max_left _ _).trans (le_max_right _ _))

private theorem large_scale_vanishes {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {ι : Type*} {l : Filter ι} {a v : ι → ℝ}
    (ha : Tendsto a l atTop) (hv : ∀ᶠ j in l, 0 < v j) :
    Tendsto (fun j ↦ TDist.likelihood x b (v j) (a j)) l (𝓝 0) := by
  obtain ⟨C,hC⟩ := normalizer_bounded
  have ht := tendsto_exp_atBot.comp (tendsto_atBot_add_const_left _ C
    (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp ha)))
  have H : Tendsto (fun j ↦ exp (C - log (a j)) ^ n) l (𝓝 0) := by
    simpa [sub_eq_add_neg, zero_pow hn.ne'] using ht.pow n
  refine squeeze_zero' (Eventually.of_forall fun _ ↦ (exp_pos _).le) ?_ H
  filter_upwards [ha.eventually_gt_atTop 0,hv] with j hja hjv
  unfold TDist.likelihood TDist.logLik
  rw [exp_sum, ← (show (∏ _ : Fin n, exp (C - log (a j))) = exp (C - log (a j)) ^ n by simp)]
  apply Finset.prod_le_prod₀ (fun _ _ ↦ (exp_pos _).le)
  intro i _
  have hk : 0 ≤ log (1+(x i-b)^2/(v j*(a j)^2)) :=
    log_nonneg (le_add_of_nonneg_right (by positivity))
  apply exp_le_exp.mpr
  linarith [hC _ hjv, mul_nonneg (show 0 ≤ (v j+1)/2 by positivity) hk]

/-- Joint escape classification for ordinary Student at held location.
Contacts make the joint range unbounded. With no contacts, failure of
attainment leaves the RMS Gaussian supremum. The path clauses specify the
large scale, zero shape, positive shape and infinite shape corners. -/
theorem lst_held_location_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v a ↦ TDist.likelihood x b v a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D)) ↔ ∃ i, x i = b) ∧
    ((∀ i, x i ≠ b) → (¬ ∃ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ L q.1 q.2) D p) →
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D) (SkewLSHalfT.normalFace x b / (2 : ℝ)^n)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l atTop →
      (∀ᶠ j in l, 0 < v j) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      Tendsto v l (𝓝[>] 0) →
      ((∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    ((∃ i, x i = b) → Tendsto (fun t : ℝ ↦ L t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ q > 0, TDist.exponent x b q = 0 →
      Tendsto (fun t : ℝ ↦ L (q-t) (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {q : ℝ}, 0 < q →
      Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      ((0 < TDist.exponent x b q → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
        (TDist.exponent x b q < 0 → Tendsto (fun j ↦ L (v j) (a j)) l atTop))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {s : ℝ}, 0 < s →
      Tendsto a l (𝓝 s) → Tendsto v l atTop →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 (∏ i, TDist.normalDensity b s (x i)))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l (𝓝[>] 0) →
      Tendsto v l atTop →
      ((∃ i, x i ≠ b) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
      ((∀ i, x i = b) → Tendsto (fun j ↦ L (v j) (a j)) l atTop)) := by
  dsimp only
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  let L := fun p : ℝ × ℝ ↦ TDist.likelihood x b p.1 p.2
  let H := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x b p.1 p.2
  let C : ℝ := (2 : ℝ)^n
  have hC : 0 < C := by dsimp [C]; positivity
  have hHL (p : ℝ × ℝ) : H p = C * L p := rfl
  have hTie (h : ∃ i, x i = b) :
      Tendsto (fun t : ℝ ↦ TDist.likelihood x b t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop := by
    obtain ⟨i,hi⟩ := h
    exact held_location_zero_shape_divergent_path x b
      (Finset.card_pos.mpr ⟨i, by simp [hi]⟩)
  have hS (h : ∀ i, x i ≠ b) (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) (SkewLSHalfT.normalFace x b / C) := by
    have hH : ¬ ∃ p ∈ D, IsMaxOn H D p := by
      rintro ⟨p,hp,hm⟩
      apply hnm
      refine ⟨p,hp,fun q hq ↦ ?_⟩
      have hq' : H q ≤ H p := hm hq
      rw [hHL,hHL] at hq'
      exact le_of_mul_le_mul_left hq' hC
    have H' := StudentFaces.half_held_endpoint_unattained_supremum x hn b h hH
    constructor
    · rintro _ ⟨p,hp,rfl⟩
      have h := H'.1 (show H p ∈ H '' D from ⟨p,hp,rfl⟩)
      change H p ≤ SkewLSHalfT.normalFace x b at h
      rw [hHL] at h
      exact (le_div_iff₀ hC).mpr (by simpa [mul_comm] using h)
    · intro B hB
      apply (div_le_iff₀ hC).mpr
      apply H'.2
      rintro _ ⟨p,hp,rfl⟩
      change H p ≤ _
      rw [mul_comm, hHL]
      exact mul_le_mul_of_nonneg_left (hB ⟨p,hp,rfl⟩) hC.le
  refine ⟨⟨?_,?_⟩,hS,?_,?_,hTie,?_,?_,?_,?_⟩
  · intro hub
    by_contra! ht
    by_cases hm : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hm⟩ := hm
      exact hub ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    · exact hub ⟨_, (hS ht hm).1⟩
  · rintro ht ⟨B,hB⟩
    have hp : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
    obtain ⟨t,ht',hbad⟩ := (hp.and ((hTie ht).eventually_gt_atTop B)).exists
    have h := hB (show L (t,exp (-(t^2)⁻¹)) ∈ L '' D from ⟨(t,exp (-(t^2)⁻¹)),⟨ht',exp_pos _⟩,rfl⟩)
    exact hbad.not_ge h
  · intro ι l a v ha hv
    exact large_scale_vanishes x hn b ha hv
  · intro ι l a v ha hv h
    exact held_location_zero_shape_vanishes x hn b ha hv h
  · intro q hq he
    exact held_location_critical_below_path x hn b hq he
  · intro ι l a v q hq ha hv
    exact ⟨(moving_shape_collapse x b hq tendsto_const_nhds ha hv).1,
      (held_location_positive_shape_collapse x hn b hq ha hv).1⟩
  · intro ι l a v s hs ha hv
    exact Student2Normal.moving_normal_face x hs tendsto_const_nhds ha hv
  · intro ι l a v ha hv
    exact ⟨Student2Normal.zero_scale_normal_corner x tendsto_const_nhds ha hv,
      fun h ↦ held_location_all_tied_normal_corner x hn b h ha hv⟩

/-- With a compatible held half-Student endpoint, a contact gives
unbounded likelihood. On tie-free data the only positive excluded end
is the same-endpoint half-normal at a finite positive scale. -/
theorem half_held_endpoint_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ)
    (_hsupport : ∀ i, b ≤ x i) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v a ↦ SkewLSHalfT.likelihood x b v a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D)) ↔ ∃ i, x i = b) ∧
    ((∀ i, x i ≠ b) → (¬ ∃ p ∈ D, IsMaxOn (fun q : ℝ × ℝ ↦ L q.1 q.2) D p) →
      IsLUB ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' D) (SkewLSHalfT.normalFace x b)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l atTop →
      (∀ᶠ j in l, 0 < v j) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      Tendsto v l (𝓝[>] 0) →
      ((∀ i, x i ≠ b) ∨ ∃ s > 0, ∀ᶠ j in l, s ≤ a j) →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
    ((∃ i, x i = b) → Tendsto (fun t : ℝ ↦ L t (exp (-(t^2)⁻¹))) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {q : ℝ}, 0 < q →
      Tendsto a l (𝓝[>] 0) → Tendsto v l (𝓝 q) →
      ((0 < TDist.exponent x b q → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
        (TDist.exponent x b q < 0 → Tendsto (fun j ↦ L (v j) (a j)) l atTop) ∧
        (TDist.exponent x b q = 0 → ∀ r : ℝ,
          Tendsto (fun j ↦ (v j-q) * log (a j)) l (𝓝 r) →
          Tendsto (fun j ↦ L (v j) (a j)) l
            (𝓝 ((2 : ℝ)^n * exp (((n : ℝ) - TDist.multiplicity x b) * r +
              TDist.collapseLogValue x b q)))))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ} {s : ℝ}, 0 < s →
      Tendsto a l (𝓝 s) → Tendsto v l atTop →
      Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 ((2 : ℝ)^n * ∏ i, TDist.normalDensity b s (x i)))) ∧
    (∀ {ι : Type*} {l : Filter ι} {a v : ι → ℝ}, Tendsto a l (𝓝[>] 0) →
      Tendsto v l atTop →
      ((∃ i, x i ≠ b) → Tendsto (fun j ↦ L (v j) (a j)) l (𝓝 0)) ∧
      ((∀ i, x i = b) → Tendsto (fun j ↦ L (v j) (a j)) l atTop)) := by
  dsimp only
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  let C := (2 : ℝ)^n
  have hC : 0 < C := by dsimp [C]; positivity
  let H := fun p : ℝ × ℝ ↦ SkewLSHalfT.likelihood x b p.1 p.2
  let T := fun p : ℝ × ℝ ↦ TDist.likelihood x b p.1 p.2
  obtain ⟨hB,_,_,_,hTie,_,_,_,_⟩ := lst_held_location_failure.{0,0,0,0,0} x hn b
  have hu : (¬ BddAbove (H '' D)) ↔ ∃ i, x i = b := by
    constructor
    · intro hh
      apply hB.mp
      rintro ⟨B,hB'⟩
      apply hh
      refine ⟨C*B,?_⟩
      rintro _ ⟨p,hp,rfl⟩
      exact mul_le_mul_of_nonneg_left (hB' ⟨p,hp,rfl⟩ : T p ≤ B) hC.le
    · intro ht
      have h := (hTie ht).const_mul_atTop hC
      have hp : ∀ᶠ v : ℝ in 𝓝[>] 0, 0 < v := self_mem_nhdsWithin
      rintro ⟨B,hB'⟩
      obtain ⟨v,hv,hbad⟩ := (hp.and (h.eventually_gt_atTop B)).exists
      have hu : H (v,exp (-(v^2)⁻¹)) ≤ B := hB' ⟨(v,exp (-(v^2)⁻¹)),⟨hv,exp_pos _⟩,rfl⟩
      exact hbad.not_ge hu
  refine ⟨hu,StudentFaces.half_held_endpoint_unattained_supremum x hn b,?_,?_,?_,?_,?_,?_⟩
  · intro ι l a v ha hv
    simpa only [SkewLSHalfT.likelihood, mul_zero, C] using
      (large_scale_vanishes x hn b ha hv).const_mul C
  · intro ι l a v ha hv hs
    simpa only [SkewLSHalfT.likelihood, mul_zero, C] using
      (held_location_zero_shape_vanishes x hn b ha hv hs).const_mul C
  · intro ht
    exact (hTie ht).const_mul_atTop hC
  · intro ι l a v q hq ha hv
    refine ⟨?_,?_,?_⟩
    · intro he
      simpa only [SkewLSHalfT.likelihood, mul_zero, C] using
        ((moving_shape_collapse x b hq tendsto_const_nhds ha hv).1 he).const_mul C
    · intro he
      exact ((held_location_positive_shape_collapse x hn b hq ha hv).1 he).const_mul_atTop hC
    · intro he r hr
      exact (critical_vertical_finite_rate x b hq he ha hv hr).const_mul C
  · intro ι l a v s hs ha hv
    exact (Student2Normal.moving_normal_face x hs tendsto_const_nhds ha hv).const_mul C
  · intro ι l a v ha hv
    constructor
    · intro hx
      simpa only [SkewLSHalfT.likelihood, mul_zero, C] using
        (Student2Normal.zero_scale_normal_corner x tendsto_const_nhds ha hv hx).const_mul C
    · intro hx
      exact (held_location_all_tied_normal_corner x hn b hx ha hv).const_mul_atTop hC

private theorem critical_atom_formula {n : ℕ} (x : Fin n → ℝ)
    (b m : ℝ) {v a : ℝ} (hv : 0 < v) (ha : 0 < a) :
    TDist.logLik x m v a = TDist.exponent x b v * log a + movingRemainder x b m v a -
      (v+1)/2 * (TDist.multiplicity x b : ℝ) * log (1 + ((m-b)/a)^2/v) := by
  rw [moving_collapse_formula x b m hv ha]
  let K (i : Fin n) := log (1+(x i-m)^2/(v*a^2))
  have h := Finset.sum_filter_add_sum_filter_not (s := Finset.univ) (fun i ↦ x i = b) K
  change (∑ i ∈ Finset.univ.filter (fun i ↦ x i = b), K i) +
    (∑ i ∈ offAtom x b, K i) = ∑ i, K i at h
  have h' : (∑ i ∈ Finset.univ.filter (fun i ↦ x i = b), K i) =
      (TDist.multiplicity x b : ℝ) * log (1+((m-b)/a)^2/v) := by
    calc
      _ = ∑ _ ∈ Finset.univ.filter (fun i ↦ x i = b), log (1+((m-b)/a)^2/v) := by
        apply Finset.sum_congr rfl
        intro i hi
        have hx := (Finset.mem_filter.mp hi).2
        dsimp [K]
        rw [hx, div_pow, div_div, mul_comm (a^2), show (b-m)^2 = (m-b)^2 by ring]
      _ = _ := by simp [TDist.multiplicity]
  rw [h'] at h
  change _ - (v+1)/2 * ((∑ i, K i) - ∑ i ∈ offAtom x b, K i) = _
  rw [← h]
  ring

/-- At a critical atom the symmetric Student product has a joint
finite-displacement and finite-df-rate trace. Under a shape floor, an
unbounded scaled displacement or a negative infinite rate kills it. -/
theorem moving_critical_atom_base {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {q : ℝ} (hq : 0 < q) (he : TDist.exponent x b q = 0)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ}
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝[>] 0)) (hv : Tendsto v l (𝓝 q)) :
    let L := fun j ↦ TDist.likelihood x (m j) (v j) (a j)
    let δ := fun j ↦ (m j-b)/a j
    let ρ := fun j ↦ (v j-q)*log (a j)
    let k : ℝ := TDist.multiplicity x b
    (∀ d r : ℝ, Tendsto δ l (𝓝 d) → Tendsto ρ l (𝓝 r) →
      Tendsto L l (𝓝 (exp ((n-k)*r + TDist.collapseLogValue x b q -
        (q+1)/2*k*log (1+d^2/q))))) ∧
    (Tendsto ρ l atBot → Tendsto L l (𝓝 0)) ∧
    ((∀ᶠ j in l, q ≤ v j) → Tendsto (fun j ↦ (δ j)^2) l atTop →
      Tendsto L l (𝓝 0)) := by
  dsimp only
  let δ := fun j ↦ (m j-b)/a j
  let ρ := fun j ↦ (v j-q)*log (a j)
  let k : ℝ := TDist.multiplicity x b
  let p : ℝ := (n : ℝ)-k
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hp : 0 < p := by
    by_contra! h
    have hk : 0 < k := by dsimp [p] at h; linarith
    change q * p - k = 0 at he
    linarith [mul_nonpos_of_nonneg_of_nonpos hq.le h]
  have hk : 0 < k := by
    change q * p - k = 0 at he
    linarith [mul_pos hq hp]
  have ⟨ha0, hpos⟩ := tendsto_nhdsWithin_iff.mp ha
  have hvpos := hv.eventually (Ioi_mem_nhds hq)
  have hR := remainder_limit x b hq hm ha0 hv
  have hW : Tendsto (fun j ↦ (v j+1)/2*k) l (𝓝 ((q+1)/2*k)) :=
    (hv.add_const 1 |>.div_const 2).mul_const k
  have hEq : (fun j ↦ TDist.logLik x (m j) (v j) (a j)) =ᶠ[l]
      fun j ↦ p*ρ j + movingRemainder x b (m j) (v j) (a j) -
        (v j+1)/2*k*log (1+(δ j)^2/(v j)) := by
    filter_upwards [hpos,hvpos] with j hj hjv
    have he' : TDist.exponent x b (v j) * log (a j) = p*ρ j := by
      dsimp [p,k,ρ]
      unfold TDist.exponent at he ⊢
      linear_combination log (a j) * he
    exact (critical_atom_formula x b (m j) hjv hj).trans (by rw [he'])
  have hBound : ∀ᶠ j in l, TDist.logLik x (m j) (v j) (a j) ≤
      p*ρ j + movingRemainder x b (m j) (v j) (a j) := by
    filter_upwards [hEq,hvpos] with j hj hvj
    change TDist.logLik x (m j) (v j) (a j) = _ at hj
    rw [hj]
    have hvj' : 0 < v j := hvj
    exact sub_le_self _ (mul_nonneg (by positivity)
      (log_nonneg (le_add_of_nonneg_right (by positivity))))
  refine ⟨?_,?_,?_⟩
  · intro d r hd hr
    have hL := ((hd.pow 2).div hv hq.ne').const_add 1
    have h := ((hr.const_mul p).add hR).sub (hW.mul (hL.log (by positivity)))
    exact continuous_exp.continuousAt.tendsto.comp (h.congr' hEq.symm)
  · intro hr
    exact tendsto_exp_atBot.comp (tendsto_atBot_mono' l hBound
      ((hr.const_mul_atBot hp).atBot_add hR))
  · intro hvge hδ
    have hvInv := hv.inv₀ hq.ne'
    have hL : Tendsto (fun j ↦ 1 + (δ j)^2 / (v j)) l atTop := by
      have h := hδ.atTop_mul_pos (inv_pos.mpr hq) hvInv
      simpa only [δ, div_eq_mul_inv] using tendsto_atTop_add_const_left l 1 h
    have hLog := tendsto_log_atTop.comp hL
    have hLoss := hW.pos_mul_atTop (show 0 < (q+1)/2*k by positivity) hLog
    have hUpper : ∀ᶠ j in l, TDist.logLik x (m j) (v j) (a j) ≤
        -((v j+1)/2*k*log (1+(δ j)^2/(v j))) + movingRemainder x b (m j) (v j) (a j) := by
      filter_upwards [hEq,hvge,(tendsto_log_nhdsGT_zero.comp ha).eventually_lt_atBot 0]
        with j hj hjv hjlog
      change TDist.logLik x (m j) (v j) (a j) = _ at hj
      rw [hj]
      have hnon : p*ρ j ≤ 0 := mul_nonpos_of_nonneg_of_nonpos hp.le
        (mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hjv) hjlog.le)
      linarith only [hnon]
    exact tendsto_exp_atBot.comp (tendsto_atBot_mono' l hUpper
      ((tendsto_neg_atTop_atBot.comp hLoss).atBot_add hR))

end
end UnivariateML.Student2MovingCollapse
end
