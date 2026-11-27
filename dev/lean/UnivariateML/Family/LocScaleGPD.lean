module

public import UnivariateML.Family.GPD

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Generalized Pareto held coefficients

Threshold ties are handled on the fitted shape domain. The uniform
member retains positive density at both contacts.

## Deviations

Held-scale attainment is proved over the full fitted shape range. The
singleton comparison beyond the uniform support width remains open.
At fixed shape the fitted finite criterion and positive-shape collapse
comparison are exact. The reference branch below shape minus one uses
real-valued likelihoods on interior support, so the exact extended-real
maximizer set is outside this model.
-/

@[expose] public section

open Set Filter Real
open scoped Topology
open UnivariateML.GPD

namespace UnivariateML.LocScaleGPD

variable {n : ℕ}

private theorem positive_shape_logDensity {s ξ y : ℝ} (hs : 0 < s) (hξ : 0 < ξ) (hy : 0 ≤ y) :
    logDensity s ξ y = log s / ξ - (1+1/ξ)*log (s+ξ*y) := by
  unfold logDensity logRatio
  rw [ite_eq_right hξ.ne', show 1+ξ*(y/s) = (s+ξ*y)/s by field_simp,
    log_div (add_pos_of_pos_of_nonneg hs (mul_nonneg hξ.le hy)).ne' hs.ne']
  field_simp [hξ.ne']
  ring

/-- A threshold tie at held location permits divergence within the fitted
shape domain, along a fixed positive shape and vanishing scale. -/
theorem held_threshold_tie_path (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {j : Fin n} (hj : y j = 0) :
    let ξ := 2 * (n : ℝ)
    (∀ t : ℝ, exp (-t) ∈ scaleDomain y ξ) ∧
      Tendsto (fun t : ℝ ↦ logLik y (exp (-t)) ξ) atTop atTop := by
  let ξ := 2 * (n : ℝ)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hξ : 0 < ξ := by dsimp [ξ]; positivity
  have hξ' : ξ ≠ -1 := by linarith
  let C (i : Fin n) := (1+1/ξ)*log (1+ξ*y i)
  refine ⟨?_,?_⟩
  · intro t
    refine ⟨exp_pos _, fun i ↦ ⟨hy i,?_⟩⟩
    rw [ite_eq_right hξ']
    exact add_pos_of_pos_of_nonneg (exp_pos _) (mul_nonneg hξ.le (hy i))
  · have hsrc : Tendsto (fun t : ℝ ↦ t/2 - ∑ i ∈ Finset.univ.erase j, C i) atTop atTop :=
      tendsto_atTop_add_const_right _ _ (tendsto_id.atTop_div_const (by norm_num : (0 : ℝ) < 2))
    refine tendsto_atTop_mono' _ ?_ hsrc
    filter_upwards [eventually_ge_atTop (0 : ℝ)] with t ht
    have hterm (i : Fin n) : -t/ξ - C i ≤ logDensity (exp (-t)) ξ (y i) := by
      rw [positive_shape_logDensity (exp_pos _) hξ (hy i),log_exp]
      have h := log_le_log (add_pos_of_pos_of_nonneg (exp_pos _) (mul_nonneg hξ.le (hy i)))
        (add_le_add_left (exp_le_one_iff.mpr (neg_nonpos.mpr ht)) (ξ*y i))
      exact sub_le_sub_left (mul_le_mul_of_nonneg_left h (by positivity)) _
    have h0 : logDensity (exp (-t)) ξ (y j) = t := by
      simp [logDensity,logRatio,hj]
    have hs := Finset.sum_le_sum (s := Finset.univ.erase j) (fun i _ ↦ hterm i)
    rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul] at hs
    have hsum := Finset.sum_erase_add (s := Finset.univ)
      (f := fun i ↦ logDensity (exp (-t)) ξ (y i)) (Finset.mem_univ j)
    rw [h0] at hsum
    have hc : ((Finset.univ.erase j).card : ℝ) ≤ n := by
      have hc := Finset.card_le_card (Finset.erase_subset j (Finset.univ : Finset (Fin n)))
      simp at hc ⊢
    have hR : (n : ℝ) * (t/ξ) = t/2 := by
      dsimp [ξ]
      field_simp [hN.ne']
    have hm := mul_le_mul_of_nonneg_right hc (div_nonneg ht hξ.le)
    change t/2 - ∑ i ∈ Finset.univ.erase j, C i ≤ logLik y (exp (-t)) ξ
    unfold logLik
    rw [← hsum]
    rw [neg_div, mul_neg] at hs
    linarith

/-- The exact leading term of the log likelihood at zero scale and
positive held shape. A zero excess contributes exponent `-1`; each
positive excess contributes `1 / ξ`. -/
theorem held_positive_shape_collapse_trace (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {ξ : ℝ} (hξ : 0 < ξ) :
    let exponent := ∑ i, if y i = 0 then (-1 : ℝ) else 1/ξ
    let constant := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    Tendsto (fun s ↦ logLik y s ξ - exponent*log s) (𝓝[>] 0) (𝓝 constant) := by
  dsimp only
  have hterm (i : Fin n) :
      Tendsto (fun s ↦ logDensity s ξ (y i) - (if y i = 0 then (-1 : ℝ) else 1/ξ)*log s)
        (𝓝[>] 0) (𝓝 (if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i))) := by
    by_cases hi : y i = 0
    · simp only [hi,ite_true]
      have he : (fun s ↦ logDensity s ξ 0 - (-1 : ℝ)*log s) = fun _ ↦ 0 := by
        funext s
        simp [logDensity,logRatio]
      rw [he]
      exact tendsto_const_nhds
    · simp only [hi,ite_false]
      have hpos : 0 < ξ*y i := mul_pos hξ ((hy i).lt_of_ne (Ne.symm hi))
      have hlog : Tendsto (fun s : ℝ ↦ log (s+ξ*y i)) (𝓝[>] 0) (𝓝 (log (ξ*y i))) := by
        have hh : ContinuousAt (fun s : ℝ ↦ log (s+ξ*y i)) 0 :=
          (continuous_id.add continuous_const).continuousAt.log (by simpa using hpos.ne')
        simpa only [zero_add] using hh.tendsto.mono_left
          (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
      refine (hlog.const_mul (-(1+1/ξ))).congr' ?_
      filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
      rw [positive_shape_logDensity hs hξ (hy i)]
      ring
  have hsum := tendsto_finsetSum Finset.univ (fun i _ ↦ hterm i)
  simpa only [logLik,Finset.sum_sub_distrib,Finset.sum_mul] using hsum

/-- For a nonempty nonnegative excess sample, the shape-restricted joint
fit attains exactly when every excess is strictly positive. -/
theorem held_threshold_criterion (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i) :
    (∃ s ξ, -1 ≤ ξ ∧ s ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ t ∈ scaleDomain y κ, logLik y t κ ≤ logLik y s ξ) ↔
        ∀ i, 0 < y i := by
  refine ⟨?_, joint_exists hn⟩
  rintro ⟨s,ξ,_,_,hm⟩ i
  by_contra! h
  have hi := le_antisymm h (hy i)
  have hpath := held_threshold_tie_path hn y hy hi
  obtain ⟨t,ht⟩ := (hpath.2.eventually_gt_atTop (logLik y s ξ)).exists
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  exact ht.not_ge (hm _ (by linarith) _ (hpath.1 t))

/-- The feasible fitted shapes at held scale and nonnegative excesses. -/
def shapeDomain (y : Fin n → ℝ) (s : ℝ) : Set ℝ :=
  {ξ | -1 ≤ ξ ∧ s ∈ scaleDomain y ξ}

private theorem logRatio_pos {z ξ : ℝ} (hz : 0 < z) (hξ : 0 < 1+ξ*z) :
    0 < logRatio z ξ := by
  rcases lt_trichotomy ξ 0 with h | rfl | h
  · rw [logRatio,ite_eq_right h.ne]
    exact div_pos_of_neg_of_neg (log_neg hξ (by linarith [mul_neg_of_neg_of_pos h hz])) h
  · simpa [logRatio] using hz
  · rw [logRatio,ite_eq_right h.ne']
    exact div_pos (log_pos (by linarith [mul_pos h hz])) h

private theorem logRatio_nonneg {z ξ : ℝ} (hz : 0 ≤ z) (hξ : 0 < 1+ξ*z) :
    0 ≤ logRatio z ξ := by
  rcases hz.eq_or_lt with h | h
  · simp [← h,logRatio]
  · exact (logRatio_pos h hξ).le

private theorem shape_bound {y : Fin n → ℝ} {s ξ : ℝ} (h : ξ ∈ shapeDomain y s) :
    logLik y s ξ ≤ -(n : ℝ)*log s := by
  have ht (i : Fin n) : logDensity s ξ (y i) ≤ -log s := by
    unfold logDensity
    by_cases he : ξ = -1
    · simp [he]
    · have hi := (h.2.2 i).2
      rw [ite_eq_right he] at hi
      have hden : 0 < 1+ξ*(y i/s) := by
        rw [show 1+ξ*(y i/s) = (s+ξ*y i)/s by field_simp [h.2.1.ne']]
        exact div_pos hi h.2.1
      exact sub_le_self _ (mul_nonneg (by linarith [h.1])
        (logRatio_nonneg (div_nonneg (h.2.2 i).1 h.2.1.le) hden))
  calc
    _ ≤ ∑ _ : Fin n, -log s := Finset.sum_le_sum (fun i _ ↦ ht i)
    _ = _ := by simp [nsmul_eq_mul]

/-- For all-zero excesses, every fitted shape at the held scale has
the same likelihood and is a maximizer. -/
theorem held_scale_all_zero (s : ℝ) (hs : 0 < s) (y : Fin n → ℝ) (hy : ∀ i, y i = 0) :
    shapeDomain y s = Ici (-1) ∧
      ∀ ξ ∈ shapeDomain y s, logLik y s ξ = -(n : ℝ)*log s := by
  constructor
  · ext ξ
    simp [shapeDomain,scaleDomain,hs,hs.le,hy]
  · intro ξ _
    simp [logLik, logDensity, logRatio,hy,nsmul_eq_mul]

/-- With a positive excess and all excesses within the held scale, the
closed uniform member is the unique maximizing shape. -/
theorem held_scale_uniform_unique (y : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    (hy : ∀ i, y i ∈ Icc 0 s) (hne : ∃ i, 0 < y i) :
    (-1 : ℝ) ∈ shapeDomain y s ∧ IsMaxOn (logLik y s) (shapeDomain y s) (-1) ∧
      ∀ ξ ∈ shapeDomain y s, ξ ≠ -1 → logLik y s ξ < logLik y s (-1) := by
  have hm : (-1 : ℝ) ∈ shapeDomain y s := ⟨le_rfl,hs,fun i ↦ by simpa using hy i⟩
  have he : logLik y s (-1) = -(n : ℝ)*log s := by
    simp [logLik,logDensity,nsmul_eq_mul]
  refine ⟨hm,fun ξ hξ ↦ (shape_bound hξ).trans_eq he.symm,?_⟩
  intro ξ hξ hne'
  obtain ⟨j,hj⟩ := hne
  have hterm (i : Fin n) :
      logDensity s ξ (y i) = -log s - (1+ξ)*logRatio (y i/s) ξ := rfl
  have hden (i : Fin n) : 0 < 1+ξ*(y i/s) := by
    have hi := (hξ.2.2 i).2
    rw [ite_eq_right hne'] at hi
    rw [show 1+ξ*(y i/s) = (s+ξ*y i)/s by field_simp [hs.ne']]
    exact div_pos hi hs
  have hξ' : 0 < 1+ξ := by linarith [hξ.1.lt_of_ne hne'.symm]
  have ht : (∑ i, logDensity s ξ (y i)) < ∑ _ : Fin n, -log s := by
    refine Finset.sum_lt_sum (fun i _ ↦ ?_) ⟨j,Finset.mem_univ j,?_⟩
    · rw [hterm]
      exact sub_le_self _ (mul_nonneg hξ'.le
        (logRatio_nonneg (div_nonneg (hy i).1 hs.le) (hden i)))
    · rw [hterm]
      exact sub_lt_self _ (mul_pos hξ' (logRatio_pos (div_pos hj hs) (hden j)))
  rw [he]
  simpa [logLik,nsmul_eq_mul] using ht

/-- At held scale, whenever a uniform support can cover the sample,
the entire maximizing set is the feasible uniform locations together
with all fitted shapes at a common observation, if there is one. -/
theorem held_scale_uniform_maximizers (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    (hw : ∃ m, ∀ i, x i - m ∈ Icc 0 s) :
    ∀ m ξ, (ξ ∈ shapeDomain (fun i ↦ x i - m) s ∧
      ∀ m' ξ', ξ' ∈ shapeDomain (fun i ↦ x i - m') s →
        logLik (fun i ↦ x i - m') s ξ' ≤ logLik (fun i ↦ x i - m) s ξ) ↔
      (ξ = -1 ∧ ∀ i, x i - m ∈ Icc 0 s) ∨ (-1 ≤ ξ ∧ ∀ i, x i = m) := by
  obtain ⟨m₀,hm₀⟩ := hw
  have h0 : (-1 : ℝ) ∈ shapeDomain (fun i ↦ x i - m₀) s :=
    ⟨le_rfl,hs,fun i ↦ by simpa using hm₀ i⟩
  have he (b : ℝ) : logLik (fun i ↦ x i - b) s (-1) = -(n : ℝ)*log s := by
    simp [logLik,logDensity,nsmul_eq_mul]
  intro m ξ
  constructor
  · rintro ⟨hξ,hm⟩
    by_cases hu : ξ = -1
    · exact Or.inl ⟨hu,fun i ↦ by simpa [hu] using hξ.2.2 i⟩
    right
    refine ⟨hξ.1,fun i ↦ ?_⟩
    by_contra h
    have hpos : 0 < x i - m := (hξ.2.2 i).1.lt_of_ne (sub_ne_zero.mpr h).symm
    have hstrict : logLik (fun i ↦ x i - m) s ξ < -(n : ℝ)*log s := by
      have ht (j : Fin n) :
          0 < 1+ξ*((x j-m)/s) := by
        have hj := (hξ.2.2 j).2
        rw [ite_eq_right hu] at hj
        rw [show 1+ξ*((x j-m)/s) = (s+ξ*(x j-m))/s by field_simp [hs.ne']]
        exact div_pos hj hs
      have hh : 0 < 1+ξ := by linarith [hξ.1.lt_of_ne (Ne.symm hu)]
      have hsum : (∑ j, logDensity s ξ (x j-m)) < ∑ _ : Fin n, -log s := by
        refine Finset.sum_lt_sum (fun j _ ↦ ?_) ⟨i,Finset.mem_univ i,?_⟩
        · exact sub_le_self _ (mul_nonneg hh.le
            (logRatio_nonneg (div_nonneg (hξ.2.2 j).1 hs.le) (ht j)))
        · exact sub_lt_self _ (mul_pos hh (logRatio_pos (div_pos hpos hs) (ht i)))
      simpa [logLik,nsmul_eq_mul] using hsum
    exact hstrict.not_ge ((he m₀).symm.trans_le (hm m₀ (-1) h0))
  · rintro (⟨rfl,hm⟩ | ⟨hξ,hm⟩)
    · refine ⟨⟨le_rfl,hs,fun i ↦ by simpa using hm i⟩,fun m' ξ' h' ↦ ?_⟩
      exact (shape_bound h').trans_eq (he m).symm
    · have hh := held_scale_all_zero s hs (fun i ↦ x i - m) (fun i ↦ sub_eq_zero.mpr (hm i))
      have hf : ξ ∈ shapeDomain (fun i ↦ x i - m) s := by rw [hh.1]; exact hξ
      exact ⟨hf,fun m' ξ' h' ↦ (shape_bound h').trans_eq (hh.2 ξ hf).symm⟩

private theorem ratio_continuousAt (y v : ℝ) (hv : 0 < 1 + v * y) :
    ContinuousAt (logRatio y) v := by
  by_cases h : v = 0
  · subst v
    have hd : HasDerivAt (fun t : ℝ ↦ log (1 + t * y)) y 0 := by
      simpa using (((hasDerivAt_id (0 : ℝ)).mul_const y).const_add 1).log (by simp)
    apply continuousAt_iff_punctured_nhds.mpr
    rw [show logRatio y 0 = y by simp [logRatio]]
    have ht : Tendsto (fun t : ℝ ↦ log (1 + t * y) / t) (𝓝[≠] 0) (𝓝 y) := by
      simpa [smul_eq_mul, div_eq_mul_inv, mul_comm] using hd.tendsto_slope_zero
    apply ht.congr'
    filter_upwards [self_mem_nhdsWithin] with t ht
    simp [logRatio, show t ≠ 0 from ht]
  · have hc : ContinuousAt (fun t : ℝ ↦ log (1 + t * y) / t) v :=
      ((continuousAt_const.add (continuousAt_id.mul continuousAt_const)).log hv.ne').div
        continuousAt_id h
    apply hc.congr
    filter_upwards [isOpen_compl_singleton.mem_nhds (show v ∈ {0}ᶜ from h)] with t ht
    simp [logRatio, show t ≠ 0 from ht]

private theorem normalized_support {s ξ y : ℝ} (hs : 0 < s) (h : 0 < s+ξ*y) :
    0 < 1+ξ*(y/s) := by
  rw [show 1+ξ*(y/s) = (s+ξ*y)/s by field_simp [hs.ne']]
  exact div_pos h hs

private theorem single_term_bound {y : Fin n → ℝ} {s ξ : ℝ}
    (h : ξ ∈ shapeDomain y s) (hξ : ξ ≠ -1) (j : Fin n) :
    logLik y s ξ ≤ -(n : ℝ)*log s - (1+ξ)*logRatio (y j/s) ξ := by
  have hnon (i : Fin n) : 0 ≤ logRatio (y i/s) ξ := by
    have hi := (h.2.2 i).2
    rw [ite_eq_right hξ] at hi
    exact logRatio_nonneg (div_nonneg (h.2.2 i).1 h.2.1.le) (normalized_support h.2.1 hi)
  have hs := Finset.single_le_sum (f := fun i ↦ logRatio (y i/s) ξ)
    (fun i _ ↦ hnon i) (Finset.mem_univ j)
  have hm := mul_le_mul_of_nonneg_left hs (show 0 ≤ 1+ξ by linarith [h.1])
  simp [logLik,logDensity,Finset.sum_sub_distrib,nsmul_eq_mul,← Finset.mul_sum]
  linarith

/-- At held positive scale and threshold, every nonempty nonnegative
excess sample has an attained maximum on the fitted shape domain. -/
theorem held_threshold_scale_attains (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {s : ℝ} (hs : 0 < s) :
    ∃ ξ ∈ shapeDomain y s, IsMaxOn (logLik y s) (shapeDomain y s) ξ := by
  classical
  by_cases hcover : ∀ i, y i ≤ s
  · have h : (-1 : ℝ) ∈ shapeDomain y s :=
      ⟨le_rfl,hs,fun i ↦ by simpa using And.intro (hy i) (hcover i)⟩
    refine ⟨-1,h,fun ξ hξ ↦ ?_⟩
    simpa [logLik,logDensity,nsmul_eq_mul] using shape_bound hξ
  push Not at hcover
  obtain ⟨k,hk⟩ := hcover
  obtain ⟨j,_,hj⟩ := Finset.exists_max_image Finset.univ y ⟨k,Finset.mem_univ _⟩
  let M := y j
  let L := -s/M
  have hM : s < M := hk.trans_le (hj _ (Finset.mem_univ k))
  have hMp : 0 < M := hs.trans hM
  have hL0 : L < 0 := div_neg_of_neg_of_pos (neg_neg_of_pos hs) hMp
  have hL1 : -1 < L := (lt_div_iff₀ hMp).2 (by linarith)
  have hparam (ξ : ℝ) : ξ ∈ shapeDomain y s ↔ L < ξ := by
    constructor
    · intro h
      have hξ : ξ ≠ -1 := by
        intro he
        have hh := (h.2.2 j).2
        rw [he] at hh
        exact hM.not_ge (by simpa [M] using hh)
      have hjp := (h.2.2 j).2
      rw [ite_eq_right hξ] at hjp
      exact (div_lt_iff₀ hMp).2 (by linarith only [hjp])
    · intro h
      have hξ : -1 < ξ := hL1.trans h
      refine ⟨hξ.le,hs,fun i ↦ ⟨hy i,?_⟩⟩
      rw [ite_eq_right (ne_of_gt hξ)]
      by_cases h0 : 0 ≤ ξ
      · exact add_pos_of_pos_of_nonneg hs (mul_nonneg h0 (hy i))
      have hh := (div_lt_iff₀ hMp).1 h
      have hmul := mul_le_mul_of_nonpos_left (hj i (Finset.mem_univ i)) (le_of_not_ge h0)
      linarith only [hh,hmul]
  let F := logLik y s
  have hc : ContinuousOn F (Ioi L) := by
    intro ξ hξ
    have h := (hparam _).2 hξ
    have hne : ξ ≠ -1 := (hL1.trans hξ).ne'
    have ht (i : Fin n) : ContinuousAt (fun t ↦ logDensity s t (y i)) ξ := by
      have hi := (h.2.2 i).2
      rw [ite_eq_right hne] at hi
      exact continuousAt_const.sub ((continuousAt_const.add continuousAt_id).mul
        (ratio_continuousAt _ _ (normalized_support hs hi)))
    change ContinuousWithinAt (fun t ↦ ∑ i, logDensity s t (y i)) (Ioi L) ξ
    exact (tendsto_finsetSum Finset.univ (fun i _ ↦ (ht i).tendsto)).mono_left nhdsWithin_le_nhds
  let B := -(n : ℝ)*log s
  let C := max 1 ((B - F 0 + 1)/(1+L))
  have hC : 0 < C := lt_of_lt_of_le zero_lt_one (le_max_left _ _)
  have hCB : B - F 0 < (1+L)*C := by
    have hp : 0 < 1+L := by linarith
    have h := (div_le_iff₀ hp).1 (le_max_right 1 ((B - F 0 + 1)/(1+L)))
    linarith only [h]
  let a := min ((L+0)/2) ((exp (-C)-1)/(M/s))
  have haL : L < a := by
    apply lt_min (by linarith)
    exact (lt_div_iff₀ (div_pos hMp hs)).2 (by
      have hmul : L*(M/s) = -1 := by
        dsimp [L]
        field_simp [hs.ne',hMp.ne']
      rw [hmul]
      linarith [exp_pos (-C)])
  have ha0 : a < 0 := by
    exact (min_le_left _ _).trans_lt (by linarith)
  let b := max 1 ((exp (C+1)-1)/(M/s))
  have hb0 : 0 < b := zero_lt_one.trans_le (le_max_left _ _)
  have hleft (ξ : ℝ) (hξ : L < ξ) (ha : ξ < a) : F ξ < F 0 := by
    have h := (hparam _).2 hξ
    have hne : ξ ≠ -1 := (hL1.trans hξ).ne'
    have hx0 := ha.trans ha0
    have hh := (h.2.2 j).2
    rw [ite_eq_right hne] at hh
    have hz := normalized_support hs hh
    have hsmall : 1+ξ*(M/s) ≤ exp (-C) := by
      have he := (lt_div_iff₀ (div_pos hMp hs)).1 (ha.trans_le (min_le_right _ _))
      linarith only [he]
    have hlog := log_le_log hz hsmall
    rw [log_exp] at hlog
    have hr : C ≤ logRatio (M/s) ξ := by
      rw [logRatio,ite_eq_right hx0.ne,le_div_iff_of_neg hx0]
      have hm := mul_le_mul_of_nonneg_left (show -1 ≤ ξ from h.1) hC.le
      linarith only [hlog,hm]
    have hcoef : (1+L)*C ≤ (1+ξ)*logRatio (M/s) ξ :=
      mul_le_mul (by linarith) hr hC.le (by linarith [hL1])
    have hbound := single_term_bound h hne j
    change F ξ ≤ B - (1+ξ)*logRatio (M/s) ξ at hbound
    linarith
  have hright (ξ : ℝ) (hb : b < ξ) : F ξ < F 0 := by
    have hx0 := hb0.trans hb
    have h := (hparam _).2 (hL0.trans hx0)
    have hne : ξ ≠ -1 := (hL1.trans (hL0.trans hx0)).ne'
    have hh := (h.2.2 j).2
    rw [ite_eq_right hne] at hh
    have hz := normalized_support hs hh
    have he := (div_lt_iff₀ (div_pos hMp hs)).1
      ((le_max_right (1 : ℝ) ((exp (C+1)-1)/(M/s))).trans_lt hb)
    have hlog : C+1 ≤ log (1+ξ*(M/s)) := by
      simpa only [log_exp] using log_le_log (exp_pos (C+1)) (show exp (C+1) ≤ _ by linarith)
    have hratio : C ≤ (1+ξ)*logRatio (M/s) ξ := by
      rw [logRatio,ite_eq_right hx0.ne',← mul_div_assoc,le_div_iff₀ hx0]
      have hp := mul_le_mul_of_nonneg_left hlog hx0.le
      linarith only [hp,hlog,hC,hx0]
    have hBC : B - F 0 < C := by
      have : (1+L)*C ≤ C := by nlinarith [mul_nonpos_of_nonpos_of_nonneg hL0.le hC.le]
      exact hCB.trans_le this
    have hbound := single_term_bound h hne j
    change F ξ ≤ B - (1+ξ)*logRatio (M/s) ξ at hbound
    linarith
  have h0 : (0 : ℝ) ∈ Icc a b := ⟨ha0.le,hb0.le⟩
  have hsub : Icc a b ⊆ Ioi L := fun ξ hξ ↦ haL.trans_le hξ.1
  obtain ⟨ξ,hξ,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0,h0⟩ (hc.mono hsub)
  refine ⟨ξ,(hparam _).2 (hsub hξ),fun κ hκ ↦ ?_⟩
  by_cases hl : κ < a
  · exact (hleft κ ((hparam _).1 hκ) hl).le.trans (hm h0)
  by_cases hr : b < κ
  · exact (hright κ hr).le.trans (hm h0)
  exact hm ⟨le_of_not_gt hl,le_of_not_gt hr⟩

private theorem ratio_strict {z w ξ : ℝ} (hzw : z < w)
    (hz : 0 < 1+ξ*z) (hw : 0 < 1+ξ*w) : logRatio z ξ < logRatio w ξ := by
  rcases lt_trichotomy ξ 0 with h | rfl | h
  · rw [logRatio,logRatio,ite_eq_right h.ne,ite_eq_right h.ne]
    apply (div_lt_div_right_of_neg h).2
    apply log_lt_log hw
    have hh := mul_pos_of_neg_of_neg h (sub_neg.mpr hzw)
    linarith only [hh]
  · simpa [logRatio] using hzw
  · rw [logRatio,logRatio,ite_eq_right h.ne',ite_eq_right h.ne']
    apply div_lt_div_of_pos_right _ h
    apply log_lt_log hz
    have hh := mul_pos h (sub_pos.mpr hzw)
    linarith only [hh]

/-- Moving a feasible nonuniform location toward the sample minimum
strictly raises the likelihood at fixed scale and shape. -/
theorem nonuniform_location_strict (hn : 0 < n) (x : Fin n → ℝ)
    {s ξ m M : ℝ} (hξ : ξ ≠ -1) (h : ξ ∈ shapeDomain (fun i ↦ x i-m) s)
    (hM : ∀ i, M ≤ x i) (hm : m < M) :
    ξ ∈ shapeDomain (fun i ↦ x i-M) s ∧
      logLik (fun i ↦ x i-m) s ξ < logLik (fun i ↦ x i-M) s ξ := by
  have hs := h.2.1
  have haux (i : Fin n) : 0 < s+ξ*(x i-m) := by simpa only [ite_eq_right hξ] using (h.2.2 i).2
  have hsupport (i : Fin n) : 0 < s+ξ*(x i-M) := by
    by_cases hp : 0 ≤ ξ
    · exact add_pos_of_pos_of_nonneg hs (mul_nonneg hp (sub_nonneg.mpr (hM i)))
    have hd := mul_le_mul_of_nonpos_left (show x i-M ≤ x i-m by linarith) (le_of_not_ge hp)
    linarith [haux i]
  refine ⟨⟨h.1,hs,fun i ↦ ⟨sub_nonneg.mpr (hM i),by simpa only [ite_eq_right hξ] using hsupport i⟩⟩,?_⟩
  have hpos : 0 < 1+ξ := by linarith [h.1.lt_of_ne (Ne.symm hξ)]
  unfold logLik
  apply Finset.sum_lt_sum_of_nonempty (s := Finset.univ) ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  intro i _
  unfold logDensity
  exact sub_lt_sub_left (mul_lt_mul_of_pos_left (ratio_strict
    (div_lt_div_of_pos_right (by linarith : x i-M < x i-m) hs)
    (normalized_support hs (hsupport i)) (normalized_support hs (haux i))) hpos) _

/-- Every nonempty sample admits a joint threshold and shape maximum
at positive held scale on the fitted branch. -/
theorem held_scale_attains (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    ∃ m ξ, ξ ∈ shapeDomain (fun i ↦ x i-m) s ∧
      ∀ q κ, κ ∈ shapeDomain (fun i ↦ x i-q) s →
        logLik (fun i ↦ x i-q) s κ ≤ logLik (fun i ↦ x i-m) s ξ := by
  classical
  obtain ⟨i,_,hi⟩ := Finset.exists_min_image Finset.univ x ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  let M := x i
  have hM j : M ≤ x j := hi j (Finset.mem_univ j)
  obtain ⟨ξ,hξ,hm⟩ := held_threshold_scale_attains (fun j ↦ x j-M)
    (fun j ↦ sub_nonneg.mpr (hM j)) hs
  refine ⟨M,ξ,hξ,fun q κ hκ ↦ ?_⟩
  have hq : q ≤ M := sub_nonneg.mp (hκ.2.2 i).1
  rcases hq.eq_or_lt with rfl | hq
  · exact hm hκ
  by_cases hu : κ = -1
  · subst κ
    have huni : (-1 : ℝ) ∈ shapeDomain (fun i ↦ x i-M) s := by
      refine ⟨le_rfl,hs,fun j ↦ ⟨sub_nonneg.mpr (hM j),?_⟩⟩
      have hj := (hκ.2.2 j).2
      simp only [ite_true]
      simp only [ite_true] at hj
      linarith
    have he : logLik (fun j ↦ x j-q) s (-1) = logLik (fun j ↦ x j-M) s (-1) := by
      simp [logLik,logDensity]
    rw [he]
    exact hm huni
  · obtain ⟨hk,hh⟩ := nonuniform_location_strict hn x hu hκ hM hq
    exact hh.le.trans (hm hk)

/-- At held scale, every feasible infinite-shape path for which some
excess stays uniformly positive has log likelihood tending to minus infinity.
The threshold may move along the path. -/
theorem infinite_shape_positive_excess {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) (m ξ : A → ℝ) (hξ : Tendsto ξ l atTop)
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    {j : Fin n} {d : ℝ} (hd : 0 < d) (hj : ∀ᶠ t in l, d ≤ x j-m t) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot := by
  have hp : 0 < d/s := div_pos hd hs
  have hw := (tendsto_atTop_add_const_left _ 1 (hξ.atTop_mul_const hp))
  have hlog : Tendsto (fun t ↦ log (1+ξ t*(d/s))) l atTop := tendsto_log_atTop.comp hw
  have hsrc : Tendsto (fun t ↦ -(n : ℝ)*log s - log (1+ξ t*(d/s))) l atBot :=
    tendsto_atBot_add_const_left _ _ (tendsto_neg_atTop_atBot.comp hlog)
  apply tendsto_atBot_mono' l _ hsrc
  filter_upwards [hvalid,hj,hξ.eventually_gt_atTop 0] with t ht hdt hpos
  have hne : ξ t ≠ -1 := by linarith
  have hbound := single_term_bound ht hne j
  have ha : 1 ≤ 1+ξ t*(d/s) := by linarith [mul_pos hpos hp]
  have hb : 1+ξ t*(d/s) ≤ 1+ξ t*((x j-m t)/s) :=
    add_le_add_right (mul_le_mul_of_nonneg_left (div_le_div_of_nonneg_right hdt hs.le) hpos.le) _
  have hlogle := log_le_log (zero_lt_one.trans_le ha) hb
  have hl : 0 ≤ log (1+ξ t*((x j-m t)/s)) := log_nonneg (ha.trans hb)
  have hcomp : log (1+ξ t*(d/s)) ≤ (1+ξ t)*logRatio ((x j-m t)/s) (ξ t) := by
    rw [logRatio,ite_eq_right hpos.ne']
    calc
      _ ≤ ((1+ξ t)*log (1+ξ t*((x j-m t)/s)))/(ξ t) := by
        apply (le_div_iff₀ hpos).2
        have hm := mul_le_mul_of_nonneg_right hlogle hpos.le
        linarith only [hm,hl]
      _ = _ := mul_div_assoc _ _ _
  linarith only [hbound,hcomp]

/-- Every infinite-shape path at held scale vanishes for nonconstant
samples, including when the feasible threshold varies. -/
theorem infinite_shape_nonconstant {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s : ℝ} (hs : 0 < s) (m ξ : A → ℝ) (hξ : Tendsto ξ l atTop)
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    (hne : ∃ i j, x i < x j) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot := by
  obtain ⟨i,j,hij⟩ := hne
  refine infinite_shape_positive_excess x hs m ξ hξ hvalid (j := j) (sub_pos.mpr hij) ?_
  filter_upwards [hvalid] with t ht
  have hi := (ht.2.2 i).1
  linarith

/-- At the common observation, zero-scale collapse on constant data
diverges at every fixed shape, with positive scales remaining feasible. -/
theorem constant_fixed_shape_collapse (hn : 0 < n) (ξ : ℝ) :
    (∀ s > 0, s ∈ scaleDomain (fun _ : Fin n ↦ 0) ξ) ∧
      Tendsto (fun s ↦ logLik (fun _ : Fin n ↦ 0) s ξ) (𝓝[>] 0) atTop := by
  refine ⟨fun s hs ↦ ⟨hs,fun i ↦ ⟨le_rfl,?_⟩⟩,?_⟩
  · split_ifs
    · exact hs.le
    · simpa using hs
  · have hN : 0 < (n : ℝ) := by exact_mod_cast hn
    have h := (tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero).const_mul_atTop hN
    simpa [logLik,logDensity,logRatio,nsmul_eq_mul,mul_neg,neg_mul] using h

/-- At the fitted uniform shape, for nonconstant data the unique
maximum has location at the sample minimum and scale equal to the range. -/
theorem held_uniform_shape_unique (hn : 0 < n) (x : Fin n → ℝ)
    {i j : Fin n} (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (hspan : x i < x j) :
    ∀ m s, (s ∈ scaleDomain (fun k ↦ x k-m) (-1) ∧
      ∀ q t, t ∈ scaleDomain (fun k ↦ x k-q) (-1) →
        logLik (fun k ↦ x k-q) t (-1) ≤ logLik (fun k ↦ x k-m) s (-1)) ↔
      m = x i ∧ s = x j-x i := by
  have hp : 0 < x j-x i := sub_pos.mpr hspan
  have hwin : x j-x i ∈ scaleDomain (fun k ↦ x k-x i) (-1) :=
    ⟨hp,fun k ↦ ⟨sub_nonneg.mpr (hi k),by simpa using sub_le_sub_right (hj k) (x i)⟩⟩
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have he (q t : ℝ) : logLik (fun k ↦ x k-q) t (-1) = -(n : ℝ)*log t := by
    simp [logLik,logDensity,nsmul_eq_mul]
  have hb {m s : ℝ} (h : s ∈ scaleDomain (fun k ↦ x k-m) (-1)) :
      m ≤ x i ∧ x j-m ≤ s := ⟨sub_nonneg.mp (h.2 i).1,by simpa using (h.2 j).2⟩
  intro m s
  constructor
  · rintro ⟨h,hm⟩
    have hh := hm _ _ hwin
    rw [he,he] at hh
    have hb' := hb h
    have ht : s ≤ x j-x i := by
      by_contra! ht'
      have hl := log_lt_log hp ht'
      have hm := mul_lt_mul_of_neg_left hl (neg_neg_of_pos hN)
      linarith only [hh,hm]
    exact ⟨by linarith only [hb'.1,hb'.2,ht],by linarith only [hb'.1,hb'.2,ht]⟩
  · rintro ⟨rfl,rfl⟩
    refine ⟨hwin,fun q t h ↦ ?_⟩
    rw [he,he]
    obtain ⟨hq,ht⟩ := hb h
    have hl := log_le_log hp (show x j-x i ≤ t by linarith only [hq,ht])
    exact mul_le_mul_of_nonpos_left hl (neg_nonpos.mpr hN.le)

/-- Below the fitted shape floor, finite-density likelihoods are
unbounded for every nonempty sample, including constants. -/
theorem held_shape_below_uniform_unbounded (hn : 0 < n) (x : Fin n → ℝ)
    {ξ : ℝ} (hξ : ξ < -1) :
    ∃ m, (∀ i, m < x i) ∧ ∀ B : ℝ, ∃ s ∈ scaleDomain (fun i ↦ x i-m) ξ,
      B < logLik (fun i ↦ x i-m) s ξ := by
  classical
  obtain ⟨i,_,hi⟩ := Finset.exists_min_image Finset.univ x ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  have h : ∀ j, x i-1 < x j := fun j ↦ by linarith only [hi j (Finset.mem_univ j)]
  exact ⟨x i-1,h,fun B ↦ shapeBelow_unbounded hn (fun i ↦ sub_pos.mpr (h i)) hξ B⟩

/-- Approaching a finite nonuniform upper support contact at held scale
forces the likelihood to zero, even when the threshold moves. -/
theorem held_scale_nonuniform_upper_contact {A : Type*} {l : Filter A} (x : Fin n → ℝ)
    {s κ M : ℝ} (hs : 0 < s) (hκ : -1 < κ) (m ξ : A → ℝ)
    (hm : Tendsto m l (𝓝 M)) (hξ : Tendsto ξ l (𝓝 κ))
    (hvalid : ∀ᶠ t in l, ξ t ∈ shapeDomain (fun i ↦ x i-m t) s)
    {j : Fin n} (hj : 0 < x j-M) (hcontact : s+κ*(x j-M) = 0) :
    Tendsto (fun t ↦ logLik (fun i ↦ x i-m t) s (ξ t)) l atBot := by
  have hneg : κ < 0 := by
    by_contra! h
    linarith only [hcontact,hs,mul_nonneg h hj.le]
  let C := (1+κ)/2
  have hC : 0 < C := by dsimp [C]; linarith
  let v (t : A) := 1+ξ t*((x j-m t)/s)
  have he : 1+κ*((x j-M)/s) = 0 := by
    have h := congrArg (fun y : ℝ ↦ y/s) hcontact
    rw [add_div,div_self hs.ne',zero_div] at h
    simpa only [mul_div_assoc] using h
  have hlim : Tendsto v l (𝓝 0) := by
    rw [← he]
    exact (hξ.mul ((tendsto_const_nhds.sub hm).div_const s)).const_add 1
  have hnear : ∀ᶠ t in l, -1 < ξ t ∧ ξ t < 0 ∧ C ≤ 1+ξ t := by
    have hp : C-1 < κ := by dsimp [C]; linarith
    filter_upwards [hξ.eventually (Ioi_mem_nhds hκ),hξ.eventually (Iio_mem_nhds hneg),
      hξ.eventually (Ioi_mem_nhds hp)] with t ha hb hc
    exact ⟨ha,hb,by
      change C-1 < ξ t at hc
      linarith only [hc]⟩
  have hvp : ∀ᶠ t in l, v t ∈ Ioi (0 : ℝ) := by
    filter_upwards [hnear,hvalid] with t ht hv
    have hj := (hv.2.2 j).2
    rw [ite_eq_right ht.1.ne'] at hj
    exact normalized_support hs hj
  have hl := tendsto_log_nhdsGT_zero.comp (tendsto_nhdsWithin_iff.mpr ⟨hlim,hvp⟩)
  have hbound : Tendsto (fun t ↦ -(n : ℝ)*log s + C*log (v t)) l atBot :=
    tendsto_atBot_add_const_left _ _ (hl.const_mul_atBot hC)
  apply tendsto_atBot_mono' l _ hbound
  filter_upwards [hnear,hvalid,hvp,hl.eventually_lt_atBot 0] with t ht hv hp hlog
  change log (v t) < 0 at hlog
  have hd := single_term_bound hv ht.1.ne' j
  have hratio : -log (v t) ≤ logRatio ((x j-m t)/s) (ξ t) := by
    rw [logRatio,ite_eq_right ht.2.1.ne,le_div_iff_of_neg ht.2.1]
    change log (v t) ≤ -log (v t)*ξ t
    have hprod := mul_nonneg (neg_nonneg.mpr hlog.le) (show 0 ≤ 1+ξ t by linarith [ht.1])
    linarith only [hprod]
  have hmul := mul_le_mul ht.2.2 hratio (neg_nonneg.mpr hlog.le)
    (show 0 ≤ 1+ξ t by linarith [ht.1])
  linarith only [hd,hmul]

private theorem logDensity_of_ne_zero {σ ξ y : ℝ} (h : ξ ≠ 0) :
    logDensity σ ξ y = -log σ - (1 + 1 / ξ) * log (1 + ξ * (y / σ)) := by
  simp only [logDensity, logRatio, ite_eq_right h]
  field_simp
  ring

private noncomputable def scaleScore (y : Fin n → ℝ) (ξ σ : ℝ) : ℝ :=
  (∑ i, (1 + ξ) * y i / (σ + ξ * y i)) - n

private theorem scale_deriv {y : Fin n → ℝ} {ξ σ : ℝ}
    (hσ : 0 < σ) (hs : ∀ i, 0 < σ + ξ * y i) :
    HasDerivAt (fun s ↦ logLik y s ξ) (scaleScore y ξ σ / σ) σ := by
  have hd (i : Fin n) : HasDerivAt (fun s ↦ logDensity s ξ (y i))
      (((1 + ξ) * y i / (σ + ξ * y i) - 1) / σ) σ := by
    have hl := (hasDerivAt_log hσ.ne').neg
    have hq := (hasDerivAt_const σ (y i)).div (hasDerivAt_id σ) hσ.ne'
    by_cases hz : ξ = 0
    · subst ξ
      convert hl.sub hq using 1
      · ext s
        simp [logDensity, logRatio]
      · simp only [id_eq, zero_mul, mul_one]
        field_simp [hσ.ne']
        ring
    · have hp : 0 < 1 + ξ * (y i / σ) := by
        convert div_pos (hs i) hσ using 1
        field_simp
      have hlog := ((hq.const_mul ξ).const_add 1).log hp.ne'
      convert hl.sub (hlog.const_mul (1 + 1 / ξ)) using 1
      · ext s
        exact logDensity_of_ne_zero hz
      · dsimp
        rw [show 1 + ξ * (y i / σ) = (σ + ξ * y i) / σ by field_simp]
        field_simp [(hs i).ne']
        ring
  convert HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ hd i) using 1
  · ext s
    simp [logLik, Finset.sum_apply]
  · unfold scaleScore
    simp [sub_div, Finset.sum_sub_distrib, Finset.sum_div]
    exact div_eq_mul_inv _ _

private theorem held_scale_unique_of_pos_score {y : Fin n → ℝ}
    (hy : ∀ i, 0 ≤ y i) (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : -1 < ξ)
    (hstart : ∃ a ∈ scaleDomain y ξ, 0 < scaleScore y ξ a) :
    ∃! s, s ∈ scaleDomain y ξ ∧ IsMaxOn (fun t ↦ logLik y t ξ) (scaleDomain y ξ) s := by
  obtain ⟨i,hi⟩ := hnz
  obtain ⟨j,_,hj'⟩ := Finset.exists_max_image Finset.univ y ⟨i,Finset.mem_univ i⟩
  have hj k := hj' k (Finset.mem_univ k)
  have hMp : 0 < y j := hi.trans_le (hj i)
  have hξ' : 0 < 1+ξ := by linarith
  have hne : ξ ≠ -1 := hξ.ne'
  let f := scaleScore y ξ
  let c := max 0 (-ξ * y j)
  have hzero : 0 ≤ c := le_max_left _ _
  have hdom (s : ℝ) : s ∈ scaleDomain y ξ ↔ s ∈ Ioi c := by
    change (0 < s ∧ ∀ i, 0 ≤ y i ∧ _) ↔ c < s
    simp only [ite_eq_right hne, max_lt_iff, c]
    constructor
    · rintro ⟨hs, hi⟩
      exact ⟨hs, by linarith [(hi j).2]⟩
    · rintro ⟨hs, h⟩
      refine ⟨hs, fun i ↦ ⟨(hy i), ?_⟩⟩
      by_cases hx : 0 ≤ ξ
      · exact add_pos_of_pos_of_nonneg hs (mul_nonneg hx (hy i))
      · have := mul_le_mul_of_nonpos_left (hj i) (le_of_not_ge hx)
        linarith
  have hpos {s : ℝ} (hs : s ∈ Ioi c) (i : Fin n) : 0 < s + ξ * y i := by
    simpa [hne] using (((hdom s).mpr hs).2 i).2
  have hanti : StrictAntiOn f (Ioi c) := by
    intro a ha b hb hab
    unfold f scaleScore
    apply sub_lt_sub_right
    refine Finset.sum_lt_sum (fun i _ ↦ ?_) ⟨j, Finset.mem_univ _, ?_⟩
    · exact div_le_div_of_nonneg_left (mul_nonneg hξ'.le (hy i)) (hpos ha i) (by linarith)
    · exact (div_lt_div_iff_of_pos_left (mul_pos hξ' hMp) (hpos hb j) (hpos ha j)).mpr
        (by linarith)
  have hcont : ContinuousOn f (Ioi c) := by
    apply ContinuousOn.sub _ continuousOn_const
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.div (continuous_id.add continuous_const).continuousOn
      (fun s hs ↦ (hpos hs i).ne')
  obtain ⟨a,hadom,hfa⟩ := hstart
  have ha : a ∈ Ioi c := (hdom a).mp hadom
  obtain ⟨b, hb, hfb⟩ : ∃ b ∈ Ioi c, f b < 0 := by
    let b := c + y j + y j
    have hb : b ∈ Ioi c := by change c < b; dsimp [b]; linarith [hMp]
    refine ⟨b, hb, ?_⟩
    have hsum := Finset.sum_lt_sum_of_nonempty
      (s := (Finset.univ : Finset (Fin n))) ⟨j, Finset.mem_univ _⟩
      (fun i _ ↦ (div_lt_iff₀ (hpos hb i)).mpr
        (show (1 + ξ) * y i < 1 * (b + ξ * y i) by
          dsimp [b]
          nlinarith [hj i, hMp]))
    simpa [f, scaleScore, sub_neg] using hsum
  have hab : a ≤ b := by
    by_contra! h
    linarith [hanti hb ha h]
  have hsub : Icc a b ⊆ Ioi c := fun s hs ↦ lt_of_lt_of_le ha hs.1
  obtain ⟨s, hs, hz⟩ := intermediate_value_Icc' hab (hcont.mono hsub)
    (show (0 : ℝ) ∈ Icc (f b) (f a) from ⟨hfb.le, hfa.le⟩)
  have hsc := hsub hs
  have hex (r : ℝ) (hr : r ∈ Ioi c) (hne' : r ≠ s) :
      logLik y r ξ < logLik y s ξ := by
    rcases lt_or_gt_of_ne hne' with h | h
    · have hsub' : Icc r s ⊆ Ioi c := fun u hu ↦ lt_of_lt_of_le hr hu.1
      have hmon : StrictMonoOn (fun u ↦ logLik y u ξ) (Icc r s) := by
        apply strictMonoOn_of_deriv_pos (convex_Icc r s)
        · exact fun u hu ↦ (scale_deriv (lt_of_le_of_lt hzero (hsub' hu))
            (hpos (hsub' hu))).continuousAt.continuousWithinAt
        · intro u hu
          rw [interior_Icc] at hu
          have hp := hsub' ⟨hu.1.le, hu.2.le⟩
          rw [(scale_deriv (lt_of_le_of_lt hzero hp) (hpos hp)).deriv]
          exact div_pos (hz ▸ hanti hp hsc hu.2) (lt_of_le_of_lt hzero hp)
      exact hmon ⟨le_rfl, h.le⟩ ⟨h.le, le_rfl⟩ h
    · have hsub' : Icc s r ⊆ Ioi c := fun u hu ↦ lt_of_lt_of_le hsc hu.1
      have hmon : StrictAntiOn (fun u ↦ logLik y u ξ) (Icc s r) := by
        apply strictAntiOn_of_deriv_neg (convex_Icc s r)
        · exact fun u hu ↦ (scale_deriv (lt_of_le_of_lt hzero (hsub' hu))
            (hpos (hsub' hu))).continuousAt.continuousWithinAt
        · intro u hu
          rw [interior_Icc] at hu
          have hp := hsub' ⟨hu.1.le, hu.2.le⟩
          rw [(scale_deriv (lt_of_le_of_lt hzero hp) (hpos hp)).deriv]
          exact div_neg_of_neg_of_pos (hz ▸ hanti hsc hp hu.1) (lt_of_le_of_lt hzero hp)
      exact hmon ⟨le_rfl, h.le⟩ ⟨h.le, le_rfl⟩ h
  refine ⟨s, ⟨(hdom s).mpr hsc, ?_⟩, ?_⟩
  · intro r hr
    change logLik y r ξ ≤ logLik y s ξ
    by_cases he : r = s
    · simp [he]
    · exact (hex r ((hdom r).mp hr) he).le
  · intro r ⟨hr, hmax⟩
    by_contra he
    exact (not_le_of_gt (hex r ((hdom r).mp hr) he)) (hmax ((hdom s).mpr hsc))


/-- The zero-scale exponent for a positive held shape. Each threshold tie
contributes `-1`; each positive excess contributes `1 / ξ`. -/
noncomputable def collapseExponent (y : Fin n → ℝ) (ξ : ℝ) : ℝ :=
  ∑ i, if y i = 0 then -1 else 1/ξ

private theorem positive_shape_term {ξ s y : ℝ} (hξ : 0 < ξ) (hs : 0 < s)
    (hy : 0 ≤ y) :
    (1+ξ)*y/(s+ξ*y) ≤ (if y = 0 then 0 else (1+ξ)/ξ) := by
  by_cases he : y = 0
  · simp [he]
  · rw [ite_eq_right he]
    have ht := add_pos_of_pos_of_nonneg hs (mul_nonneg hξ.le hy)
    rw [div_le_div_iff₀ ht hξ]
    nlinarith only [mul_pos (show 0 < 1+ξ by linarith) hs]

private theorem positive_shape_term_strict {ξ s y : ℝ} (hξ : 0 < ξ)
    (hs : 0 < s) (hy : 0 < y) :
    (1+ξ)*y/(s+ξ*y) < (if y = 0 then 0 else (1+ξ)/ξ) := by
  rw [ite_eq_right hy.ne']
  have ht := add_pos_of_pos_of_nonneg hs (mul_nonneg hξ.le hy.le)
  rw [div_lt_div_iff₀ ht hξ]
  nlinarith only [mul_pos (show 0 < 1+ξ by linarith) hs]

private theorem exponent_formula (y : Fin n → ℝ) {ξ : ℝ} (hξ : ξ ≠ 0) :
    collapseExponent y ξ = (∑ i, if y i = 0 then 0 else (1+ξ)/ξ) - n := by
  have he (i : Fin n) : (if y i = 0 then (-1 : ℝ) else 1/ξ) =
      (if y i = 0 then 0 else (1+ξ)/ξ)-1 := by
    by_cases h : y i = 0
    · simp [h]
    · simp only [h,ite_false]
      field_simp [hξ]
      ring
  simp only [collapseExponent,he,Finset.sum_sub_distrib,Finset.sum_const,
    Finset.card_univ,Fintype.card_fin,nsmul_eq_mul,mul_one]

private theorem score_limit_positive_shape (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {ξ : ℝ} (hξ : 0 < ξ) :
    Tendsto (scaleScore y ξ) (𝓝[>] 0) (𝓝 (collapseExponent y ξ)) := by
  have ht (i : Fin n) :
      Tendsto (fun s ↦ (1+ξ)*y i/(s+ξ*y i)) (𝓝[>] 0)
        (𝓝 (if y i = 0 then 0 else (1+ξ)/ξ)) := by
    by_cases he : y i = 0
    · simpa only [he,ite_true,mul_zero,zero_div] using
        (tendsto_const_nhds : Tendsto (fun _ : ℝ ↦ (0 : ℝ)) (𝓝[>] 0) (𝓝 0))
    · have hp : 0 < y i := (hy i).lt_of_ne (Ne.symm he)
      have hcont : ContinuousAt (fun s : ℝ ↦ (1+ξ)*y i/(s+ξ*y i)) 0 :=
        continuousAt_const.div (continuousAt_id.add continuousAt_const)
          (by simpa only [zero_add] using (mul_pos hξ hp).ne')
      have hval : (1+ξ)*y i/(0+ξ*y i) = (1+ξ)/ξ := by
        rw [zero_add]
        field_simp [hp.ne',hξ.ne']
      simpa only [he,ite_false,hval] using hcont.tendsto.mono_left
        (nhdsWithin_le_nhds : 𝓝[>] (0 : ℝ) ≤ 𝓝 0)
  rw [exponent_formula y hξ.ne']
  exact (tendsto_finsetSum Finset.univ (fun i _ ↦ ht i)).sub_const (n : ℝ)

private theorem positive_score_witness (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : -1 < ξ)
    (h : ξ ≤ 0 ∨ 0 < collapseExponent y ξ) :
    ∃ a ∈ scaleDomain y ξ, 0 < scaleScore y ξ a := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hcoef : 0 < 1+ξ := by linarith
  by_cases hξ' : 0 < ξ
  · have hA := h.resolve_left (not_le_of_gt hξ')
    obtain ⟨a,ha,hscore⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
      ((score_limit_positive_shape y hy hξ').eventually (Ioi_mem_nhds hA))).exists
    refine ⟨a,⟨ha,fun i ↦ ⟨hy i,?_⟩⟩,hscore⟩
    rw [ite_eq_right hξ.ne']
    exact add_pos_of_pos_of_nonneg ha (mul_nonneg hξ'.le (hy i))
  · obtain ⟨i,hi⟩ := hnz
    obtain ⟨j,_,hj⟩ := Finset.exists_max_image Finset.univ y ⟨i,Finset.mem_univ i⟩
    have hM := hi.trans_le (hj i (Finset.mem_univ i))
    let δ := (1+ξ)*y j/(2*n)
    have hδ : 0 < δ := div_pos (mul_pos hcoef hM) (by positivity)
    let a := -ξ*y j + δ
    have ha : 0 < a := by dsimp [a]; linarith [mul_nonneg (neg_nonneg.mpr (le_of_not_gt hξ')) hM.le]
    have ht (i : Fin n) : 0 < a+ξ*y i := by
      dsimp [a]
      have hh := mul_le_mul_of_nonpos_left (hj i (Finset.mem_univ i)) (le_of_not_gt hξ')
      linarith only [hh,hδ]
    have hjval : (1+ξ)*y j/(a+ξ*y j) = 2*n := by
      rw [show a+ξ*y j = δ by dsimp [a]; ring]
      dsimp [δ]
      field_simp [hcoef.ne',hM.ne',hN.ne']
    have hsum := Finset.single_le_sum
      (f := fun i ↦ (1+ξ)*y i/(a+ξ*y i))
      (fun i _ ↦ div_nonneg (mul_nonneg hcoef.le (hy i)) (ht i).le) (Finset.mem_univ j)
    rw [hjval] at hsum
    refine ⟨a,⟨ha,fun i ↦ ⟨hy i,by simpa only [ite_eq_right hξ.ne'] using ht i⟩⟩,?_⟩
    unfold scaleScore
    linarith only [hsum,hN]

private theorem held_scale_requires_exponent (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ s : ℝ} (hξ : -1 < ξ)
    (hs : s ∈ scaleDomain y ξ) (hm : IsMaxOn (fun a ↦ logLik y a ξ) (scaleDomain y ξ) s) :
    ξ ≤ 0 ∨ 0 < collapseExponent y ξ := by
  by_cases hp : ξ ≤ 0
  · exact Or.inl hp
  right
  have hp' : 0 < ξ := lt_of_not_ge hp
  have ht (i : Fin n) : 0 < s+ξ*y i := by simpa only [ite_eq_right hξ.ne'] using (hs.2 i).2
  have he : scaleDomain y ξ ∈ 𝓝 s := by
    have hh : ∀ᶠ t in 𝓝 s, ∀ i, 0 < t+ξ*y i := by
      rw [eventually_all]
      intro i
      exact (continuousAt_id.add continuousAt_const).eventually (Ioi_mem_nhds (ht i))
    filter_upwards [Ioi_mem_nhds hs.1, hh] with t ht ht'
    exact ⟨ht,fun i ↦ ⟨hy i,by simpa only [ite_eq_right hξ.ne'] using ht' i⟩⟩
  have hz := (hm.isLocalMax he).hasDerivAt_eq_zero (scale_deriv hs.1 ht)
  have hz' : scaleScore y ξ s = 0 := (div_eq_zero_iff.mp hz).resolve_right hs.1.ne'
  obtain ⟨j,hj⟩ := hnz
  have hsum := Finset.sum_lt_sum (fun i (_ : i ∈ Finset.univ) ↦ positive_shape_term hp' hs.1 (hy i))
    ⟨j,Finset.mem_univ j,positive_shape_term_strict hp' hs.1 hj⟩
  rw [exponent_formula y hp'.ne']
  unfold scaleScore at hz'
  linarith only [hsum,hz']

/-- At a fixed nonuniform fitted shape, nonzero nonnegative excess data
have a unique scale optimum precisely on the positive-exponent branch
for positive shape, and always at nonpositive shape. -/
theorem held_nonuniform_scale_criterion (hn : 0 < n) (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : -1 < ξ) :
    (∃! s, s ∈ scaleDomain y ξ ∧ IsMaxOn (fun a ↦ logLik y a ξ) (scaleDomain y ξ) s) ↔
      ξ ≤ 0 ∨ 0 < collapseExponent y ξ := by
  refine ⟨?_,fun h ↦ held_scale_unique_of_pos_score hy hnz hξ
    (positive_score_witness hn y hy hnz hξ h)⟩
  rintro ⟨s,⟨hs,hm⟩,_⟩
  exact held_scale_requires_exponent y hy hnz hξ hs hm

/-- With fitted shape strictly above the uniform, the full joint
threshold and scale fit exists exactly on the scale-attainment branch
at the sample minimum. Whenever it exists it is unique. -/
theorem held_nonuniform_shape_criterion (hn : 0 < n) (x : Fin n → ℝ)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (hnc : ∃ j, x i < x j) {ξ : ℝ} (hξ : -1 < ξ) :
    let y := fun j ↦ x j-x i
    (∃! p : ℝ × ℝ, p.2 ∈ scaleDomain (fun j ↦ x j-p.1) ξ ∧
      ∀ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ →
        logLik (fun j ↦ x j-m) s ξ ≤ logLik (fun j ↦ x j-p.1) p.2 ξ) ↔
      ξ ≤ 0 ∨ 0 < collapseExponent y ξ := by
  let y := fun j ↦ x j-x i
  have hy : ∀ j, 0 ≤ y j := fun j ↦ sub_nonneg.mpr (hi j)
  have hpos : ∃ j, 0 < y j := by
    obtain ⟨j,hj⟩ := hnc
    exact ⟨j,sub_pos.mpr hj⟩
  have hp (m s : ℝ) (hs : s ∈ scaleDomain (fun j ↦ x j-m) ξ) :
      s ∈ scaleDomain y ξ ∧ logLik (fun j ↦ x j-m) s ξ ≤ logLik y s ξ := by
    have hl : m ≤ x i := sub_nonneg.mp (hs.2 i).1
    rcases hl.eq_or_lt with h | h
    · subst m
      exact ⟨hs,le_rfl⟩
    · have hh := nonuniform_location_strict hn x hξ.ne' ⟨hξ.le,hs⟩ hi h
      exact ⟨hh.1.2,hh.2.le⟩
  have hpin (m s : ℝ) (hs : s ∈ scaleDomain (fun j ↦ x j-m) ξ)
      (hm : ∀ q t, t ∈ scaleDomain (fun j ↦ x j-q) ξ →
        logLik (fun j ↦ x j-q) t ξ ≤ logLik (fun j ↦ x j-m) s ξ) : m = x i := by
    have hl : m ≤ x i := sub_nonneg.mp (hs.2 i).1
    rcases hl.eq_or_lt with h | h
    · exact h
    · have hh := nonuniform_location_strict hn x hξ.ne' ⟨hξ.le,hs⟩ hi h
      exact (hh.2.not_ge (hm (x i) s hh.1.2)).elim
  dsimp only
  constructor
  · rintro ⟨⟨m,s⟩,⟨hs,hm⟩,_⟩
    have he := hpin m s hs hm
    subst m
    exact held_scale_requires_exponent y hy hpos hξ hs (fun t ht ↦ hm (x i) t ht)
  · intro he
    obtain ⟨s,⟨hs,hm⟩,hu⟩ := (held_nonuniform_scale_criterion hn y hy hpos hξ).mpr he
    refine ⟨(x i,s),⟨hs,fun m t ht ↦ (hp m t ht).2.trans (hm (hp m t ht).1)⟩,?_⟩
    rintro ⟨m,t⟩ ⟨ht,htm⟩
    have he := hpin m t ht htm
    subst m
    exact congrArg (fun v ↦ (x i,v)) (hu t ⟨ht,fun q hq ↦ htm (x i) q hq⟩)

private theorem nonpositive_exponent_strictAnti (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent y ξ ≤ 0) :
    StrictAntiOn (fun s ↦ logLik y s ξ) (Ioi 0) := by
  have hsupport {s : ℝ} (hs : 0 < s) (i : Fin n) : 0 < s+ξ*y i :=
    add_pos_of_pos_of_nonneg hs (mul_nonneg hξ.le (hy i))
  apply strictAntiOn_of_deriv_neg (convex_Ioi 0)
    (fun s hs ↦ (scale_deriv hs (hsupport hs)).continuousAt.continuousWithinAt)
  intro s hs
  have hs' : 0 < s := interior_subset hs
  rw [(scale_deriv hs' (hsupport hs')).deriv]
  apply div_neg_of_neg_of_pos _ hs'
  obtain ⟨j,hj⟩ := hnz
  have hsum := Finset.sum_lt_sum (fun i (_ : i ∈ Finset.univ) ↦ positive_shape_term hξ hs' (hy i))
    ⟨j,Finset.mem_univ j,positive_shape_term_strict hξ hs' hj⟩
  rw [exponent_formula y hξ.ne'] at hA
  unfold scaleScore
  linarith only [hsum,hA]

/-- At a positive held shape with zero collapse exponent, the scale
supremum is the finite zero-scale trace and every positive scale loses. -/
theorem held_positive_shape_critical_face (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    (hnz : ∃ i, 0 < y i) {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent y ξ = 0) :
    let H := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    IsLUB ((fun s ↦ logLik y s ξ) '' Ioi 0) H ∧
      ∀ s > 0, logLik y s ξ < H := by
  let H := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
  have hl : Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) (𝓝 H) := by
    have h := held_positive_shape_collapse_trace y hy hξ
    change Tendsto (fun s ↦ logLik y s ξ - collapseExponent y ξ*log s) (𝓝[>] 0) (𝓝 H) at h
    simpa only [hA,zero_mul,sub_zero] using h
  have ha := nonpositive_exponent_strictAnti y hy hnz hξ hA.le
  have hlt (s : ℝ) (hs : 0 < s) : logLik y s ξ < H := by
    have hs₂ : 0 < s/2 := half_pos hs
    apply (ha hs₂ hs (half_lt_self hs)).trans_le
    apply ge_of_tendsto hl
    filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ)),
      nhdsWithin_le_nhds (Iio_mem_nhds hs₂)] with t ht ht'
    exact (ha ht hs₂ ht').le
  refine ⟨⟨?_,?_⟩,hlt⟩
  · rintro _ ⟨s,hs,rfl⟩
    exact (hlt s hs).le
  · intro u hu
    apply le_of_tendsto hl
    exact (eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).mono
      (fun s hs ↦ hu (mem_image_of_mem _ hs))

/-- A negative collapse exponent at positive held shape gives unbounded
likelihood along the zero-scale path. -/
theorem held_positive_shape_negative_exponent (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i)
    {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent y ξ < 0) :
    Tendsto (fun s ↦ logLik y s ξ) (𝓝[>] 0) atTop := by
  have h := held_positive_shape_collapse_trace y hy hξ
  have hmain := tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hA
  have hL := hmain.atTop_add h
  apply hL.congr'
  exact Eventually.of_forall (fun _ ↦ by dsimp [collapseExponent]; ring)

/-- The full held-shape finite-likelihood criterion. A fit exists
exactly on the fitted shape branch, on nonconstant data, and, at
positive shape, when the exponent at the sample minimum is positive.
Every such fit is unique. -/
theorem held_shape_finite_criterion (hn : 0 < n) (x : Fin n → ℝ) {i j : Fin n}
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) (ξ : ℝ) :
    let y := fun k ↦ x k-x i
    let Fit := fun p : ℝ × ℝ ↦ p.2 ∈ scaleDomain (fun k ↦ x k-p.1) ξ ∧
      ∀ m s, s ∈ scaleDomain (fun k ↦ x k-m) ξ →
        logLik (fun k ↦ x k-m) s ξ ≤ logLik (fun k ↦ x k-p.1) p.2 ξ
    ((∃ p, Fit p) ↔ -1 ≤ ξ ∧ x i < x j ∧ (ξ ≤ 0 ∨ 0 < collapseExponent y ξ)) ∧
      ∀ p q, Fit p → Fit q → p = q := by
  let y := fun k ↦ x k-x i
  let Fit := fun p : ℝ × ℝ ↦ p.2 ∈ scaleDomain (fun k ↦ x k-p.1) ξ ∧
    ∀ m s, s ∈ scaleDomain (fun k ↦ x k-m) ξ →
      logLik (fun k ↦ x k-m) s ξ ≤ logLik (fun k ↦ x k-p.1) p.2 ξ
  let Good := -1 ≤ ξ ∧ x i < x j ∧ (ξ ≤ 0 ∨ 0 < collapseExponent y ξ)
  have hnec (p : ℝ × ℝ) (hp : Fit p) : Good := by
    have hξ₀ : -1 ≤ ξ := by
      by_contra! h
      obtain ⟨m,_,hm⟩ := held_shape_below_uniform_unbounded hn x h
      obtain ⟨s,hs,hstrict⟩ := hm (logLik (fun k ↦ x k-p.1) p.2 ξ)
      exact hstrict.not_ge (hp.2 m s hs)
    have hspan : x i < x j := by
      by_contra! h
      have he : y = fun _ ↦ 0 := funext fun k ↦
        sub_eq_zero.mpr (le_antisymm ((hj k).trans h) (hi k))
      obtain ⟨hfeasible,hpath⟩ := constant_fixed_shape_collapse hn ξ
      obtain ⟨s,hs,hstrict⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
        (hpath.eventually_gt_atTop (logLik (fun k ↦ x k-p.1) p.2 ξ))).exists
      have hb : s ∈ scaleDomain y ξ := he ▸ hfeasible s hs
      have hc := hp.2 (x i) s hb
      change logLik y s ξ ≤ _ at hc
      rw [he] at hc
      exact hstrict.not_ge hc
    refine ⟨hξ₀,hspan,?_⟩
    by_cases hξ : ξ ≤ 0
    · exact Or.inl hξ
    have hξ' : -1 < ξ := by linarith [lt_of_not_ge hξ]
    have hp₀ : p.1 ≤ x i := sub_nonneg.mp (hp.1.2 i).1
    have hμ : p.1 = x i := hp₀.eq_or_lt.resolve_right (fun h ↦ by
      have hmove := nonuniform_location_strict hn x hξ'.ne' ⟨hξ₀,hp.1⟩ hi h
      exact hmove.2.not_ge (hp.2 _ _ hmove.1.2))
    have hs : p.2 ∈ scaleDomain y ξ := by simpa only [y,← hμ] using hp.1
    have hm : IsMaxOn (fun a ↦ logLik y a ξ) (scaleDomain y ξ) p.2 := by
      intro s hs
      have hm := hp.2 (x i) s hs
      change logLik y s ξ ≤ logLik y p.2 ξ
      simpa only [hμ] using hm
    exact held_scale_requires_exponent y (fun k ↦ sub_nonneg.mpr (hi k))
      ⟨j,sub_pos.mpr hspan⟩ hξ' hs hm
  have hsuf (h : Good) : ∃! p, Fit p := by
    rcases h.1.eq_or_lt with he | hp
    · have hh := held_uniform_shape_unique hn x hi hj h.2.1
      have hwin := (hh (x i) (x j-x i)).mpr ⟨rfl,rfl⟩
      refine ⟨(x i,x j-x i),?_,?_⟩
      · simpa only [Fit,←he] using hwin
      · rintro ⟨m,s⟩ hms
        have heq := (hh m s).mp (by simpa only [Fit,←he] using hms)
        exact Prod.ext heq.1 heq.2
    · exact (held_nonuniform_shape_criterion hn x hi ⟨j,h.2.1⟩ hp).mpr h.2.2
  exact ⟨⟨fun ⟨p,hp⟩ ↦ hnec p hp,fun h ↦ (hsuf h).exists⟩,
    fun p q hp hq ↦ (hsuf (hnec p hp)).unique hp hq⟩

/-- The critical positive-shape trace is the exact joint supremum
when location is free. The path at the observed minimum realizes its
height, and every feasible finite tuple loses strictly. -/
theorem held_positive_shape_joint_critical_face (hn : 0 < n) (x : Fin n → ℝ)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (hnc : ∃ j, x i < x j)
    {ξ : ℝ} (hξ : 0 < ξ) (hA : collapseExponent (fun j ↦ x j-x i) ξ = 0) :
    let y := fun j ↦ x j-x i
    let H := ∑ j, if y j = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y j)
    IsLUB {v | ∃ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ ∧
      v = logLik (fun j ↦ x j-m) s ξ} H ∧
    ∀ m s, s ∈ scaleDomain (fun j ↦ x j-m) ξ → logLik (fun j ↦ x j-m) s ξ < H := by
  let y := fun j ↦ x j-x i
  let H := ∑ j, if y j = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y j)
  have hy j : 0 ≤ y j := sub_nonneg.mpr (hi j)
  have hpos : ∃ j, 0 < y j := by
    obtain ⟨j,hj⟩ := hnc
    exact ⟨j,sub_pos.mpr hj⟩
  obtain ⟨hlub,hfin⟩ := held_positive_shape_critical_face y hy hpos hξ hA
  have hc : ξ ≠ -1 := by linarith
  have hlt (m s : ℝ) (hs : s ∈ scaleDomain (fun j ↦ x j-m) ξ) :
      logLik (fun j ↦ x j-m) s ξ < H := by
    have hm : m ≤ x i := sub_nonneg.mp (hs.2 i).1
    rcases hm.eq_or_lt with he | he
    · subst m
      exact hfin s hs.1
    · exact (nonuniform_location_strict hn x hc ⟨by linarith,hs⟩ hi he).2.trans (hfin s hs.1)
  refine ⟨⟨?_,?_⟩,hlt⟩
  · rintro _ ⟨m,s,hs,rfl⟩
    exact (hlt m s hs).le
  · intro B hB
    apply hlub.2
    rintro _ ⟨s,hs,rfl⟩
    refine hB ⟨x i,s,⟨hs,fun j ↦ ⟨hy j,?_⟩⟩,rfl⟩
    rw [ite_eq_right hc]
    exact add_pos_of_pos_of_nonneg hs (mul_nonneg hξ.le (hy j))

/-- The zero-scale log likelihood at positive shape, with the
threshold moving toward an observed minimum, has the same leading scale
exponent and the exact additional logarithmic contact penalty. -/
theorem moving_threshold_positive_shape_trace {A : Type*} {l : Filter A}
    (y : Fin n → ℝ) (hy : ∀ i, 0 ≤ y i) {ξ : ℝ} (hξ : 0 < ξ) (s δ : A → ℝ)
    (hs : Tendsto s l (𝓝[>] 0)) (hδ : Tendsto δ l (𝓝 0))
    (hδ₀ : ∀ᶠ t in l, 0 ≤ δ t) :
    let H := ∑ i, if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i)
    Tendsto (fun t ↦ logLik (fun i ↦ y i+δ t) (s t) ξ -
      collapseExponent y ξ*log (s t) +
      ∑ i, if y i = 0 then (1+1/ξ)*log (1+ξ*(δ t/s t)) else 0) l (𝓝 H) := by
  have hs₀ := (tendsto_nhdsWithin_iff.mp hs).1
  have hsp : ∀ᶠ t in l, 0 < s t := (tendsto_nhdsWithin_iff.mp hs).2
  let P (t : A) := (1+1/ξ)*log (1+ξ*(δ t/s t))
  let F (i : Fin n) (t : A) := logDensity (s t) ξ (y i+δ t) -
    (if y i = 0 then (-1 : ℝ) else 1/ξ)*log (s t) + (if y i = 0 then P t else 0)
  have hf (i : Fin n) : Tendsto (F i) l
      (𝓝 (if y i = 0 then (0 : ℝ) else -(1+1/ξ)*log (ξ*y i))) := by
    by_cases hi : y i = 0
    · simp only [hi,ite_true]
      apply tendsto_const_nhds.congr'
      filter_upwards [hsp] with t ht
      simp only [F,hi,ite_true,zero_add,logDensity_of_ne_zero hξ.ne',P]
      ring
    · simp only [hi,ite_false]
      have hpos : 0 < ξ*y i := mul_pos hξ ((hy i).lt_of_ne (Ne.symm hi))
      have hz : Tendsto (fun t ↦ s t+ξ*(y i+δ t)) l (𝓝 (ξ*y i)) := by
        simpa only [add_zero,zero_add] using hs₀.add ((hδ.const_add (y i)).const_mul ξ)
      have hlog := (continuousAt_log hpos.ne').tendsto.comp hz
      refine (hlog.const_mul (-(1+1/ξ))).congr' ?_
      filter_upwards [hsp,hδ₀] with t ht hd
      simp only [F,hi,ite_false,add_zero,Function.comp_apply]
      rw [positive_shape_logDensity ht hξ (add_nonneg (hy i) hd)]
      ring
  have hh := tendsto_finsetSum Finset.univ (fun i _ ↦ hf i)
  simpa only [F,P,logLik,collapseExponent,Finset.sum_add_distrib,Finset.sum_sub_distrib,
    Finset.sum_mul] using hh

end UnivariateML.LocScaleGPD

end
