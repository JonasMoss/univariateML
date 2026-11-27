module

public import UnivariateML.Family.AE1Huber

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Huber spatial-score separation

## Deviations

The separation lemmas assume the strict tail and strict core levels which
identify a fit. The all-free and held-center claims also require proving
those core hypotheses for attained winners.
-/

@[expose] public section

open Set Real
open UnivariateML.Huber

namespace UnivariateML.AE4Huber

noncomputable section

/-- Spatial score in inverse-variance and tail-slope coordinates. -/
def score (m B K x : ℝ) : ℝ := clip K (B*(m-x))

private theorem clip_bounds {K x : ℝ} (hK : 0 < K) :
    -K ≤ clip K x ∧ clip K x ≤ K :=
  ⟨le_max_left _ _, max_le (by linarith) (min_le_left _ _)⟩

private theorem clip_eq_of_strict {K x : ℝ} (hl : -K < clip K x) (hr : clip K x < K) :
    clip K x = x := by
  have hmin : -K < min K x := by
    by_contra! h
    rw [clip, max_eq_left h] at hl
    exact lt_irrefl _ hl
  have hx : x < K := by
    by_contra! h
    rw [clip, min_eq_left h, max_eq_right (by simpa [min_eq_left h] using hmin.le)] at hr
    exact lt_irrefl _ hr
  rw [clip, min_eq_right hx.le, max_eq_right]
  simpa only [min_eq_right hx.le] using hmin.le

private theorem score_le_abs {m B K x : ℝ} (hK : 0 < K) :
    |score m B K x| ≤ K := by
  obtain ⟨h₁,h₂⟩ := clip_bounds (x := B*(m-x)) hK
  exact abs_le.mpr ⟨h₁,h₂⟩

private theorem score_eq_linear {m B K x : ℝ} (h : |score m B K x| < K) :
    score m B K x = B*(m-x) := by
  exact clip_eq_of_strict (abs_lt.mp h).1 (abs_lt.mp h).2

private theorem tail_eq {m B K x : ℝ} (hK : 0 < K) (h : K ≤ |B * (m - x)|) :
    |score m B K x| = K := by
  rcases le_or_gt 0 (B*(m-x)) with hpos | hneg
  · rw [abs_of_nonneg hpos] at h
    rw [score, clip, min_eq_left h, max_eq_right (by linarith), abs_of_pos hK]
  · rw [abs_of_neg hneg] at h
    have hh : B*(m-x) ≤ -K := by linarith
    rw [score, clip, min_eq_right (by linarith), max_eq_left hh, abs_neg, abs_of_pos hK]

private theorem core_strict {m B K x : ℝ} (h : |B * (m - x)| < K) :
    |score m B K x| < K := by
  obtain ⟨h₁,h₂⟩ := abs_lt.mp h
  simpa [score, clip, min_eq_right h₂.le, max_eq_right h₁.le] using h

private theorem same_cutoff {n : ℕ} {x : Fin n → ℝ} {m B K m' B' K' : ℝ}
    (hK : 0 < K) (hK' : 0 < K')
    (ht : ∃ i, K ≤ |B * (m - x i)|) (ht' : ∃ i, K' ≤ |B'*(m'-x i)|)
    (he : ∀ i, score m B K (x i) = score m' B' K' (x i)) : K = K' := by
  obtain ⟨i,hi⟩ := ht
  obtain ⟨j,hj⟩ := ht'
  apply le_antisymm
  · rw [← tail_eq hK hi, he i]
    exact score_le_abs hK'
  · rw [← tail_eq hK' hj, ← he j]
    exact score_le_abs hK

/-- If each Huber candidate has a tail level, its tail slope is identified
by the sample scores. Two distinct strict core levels identify the inverse
variance and location as well. -/
theorem parameters_eq_of_scores {n : ℕ} {x : Fin n → ℝ} {m B K m' B' K' : ℝ}
    (hB : 0 < B) (hK : 0 < K) (hK' : 0 < K')
    (ht : ∃ i, K ≤ |B * (m - x i)|) (ht' : ∃ i, K' ≤ |B'*(m'-x i)|)
    {i j : Fin n} (hij : x i ≠ x j)
    (hi : |B * (m - x i)| < K) (hj : |B * (m - x j)| < K)
    (he : ∀ i, score m B K (x i) = score m' B' K' (x i)) :
    m = m' ∧ B = B' ∧ K = K' := by
  have hKeq := same_cutoff hK hK' ht ht' he
  subst K'
  have hh (k : Fin n) (hk : |B*(m-x k)| < K) : B*(m-x k) = B'*(m'-x k) := by
    have h := core_strict hk
    have h' := h
    rw [he k] at h'
    exact (score_eq_linear h).symm.trans ((he k).trans (score_eq_linear h'))
  have hz : (B-B')*(x i-x j) = 0 := by linear_combination hh j hj - hh i hi
  have hBB := sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hij))
  subst B'
  have hm := mul_left_cancel₀ hB.ne' (hh i hi)
  exact ⟨by linarith,rfl,rfl⟩

/-- At a held center one off-center strict core level, together with a
tail level for each candidate, identifies both of its free parameters. -/
theorem held_mu_parameters_eq_of_scores {n : ℕ} {x : Fin n → ℝ} {m B K B' K' : ℝ}
    (hK : 0 < K) (hK' : 0 < K')
    (ht : ∃ i, K ≤ |B * (m - x i)|) (ht' : ∃ i, K' ≤ |B'*(m - x i)|)
    {i : Fin n} (hi : x i ≠ m) (hc : |B * (m - x i)| < K)
    (he : ∀ j, score m B K (x j) = score m B' K' (x j)) :
    B = B' ∧ K = K' := by
  have hKe := same_cutoff hK hK' ht ht' he
  subst K'
  have h := core_strict hc
  have h' := h
  rw [he i] at h'
  exact ⟨mul_right_cancel₀ (sub_ne_zero.mpr hi.symm)
    ((score_eq_linear h).symm.trans ((he i).trans (score_eq_linear h'))), rfl⟩


private def kernel (t : ℝ) := exp (-(t^2/2))

private def I (c : ℝ) := ∫ t in (0 : ℝ)..c, kernel t

private def R (c : ℝ) := kernel c / I c

private theorem lower_integral {c : ℝ} (hc : 0 < c) : c*kernel c ≤ I c := by
  have h : Continuous kernel := by unfold kernel; fun_prop
  have hmono (t : ℝ) (ht : t ∈ Icc 0 c) : kernel c ≤ kernel t := by
    unfold kernel
    apply exp_le_exp.mpr
    nlinarith [mul_nonneg (sub_nonneg.mpr ht.2) (add_nonneg hc.le ht.1)]
  have he := intervalIntegral.integral_mono_on (μ := MeasureTheory.volume) hc.le
    (continuous_const.intervalIntegrable (μ := MeasureTheory.volume) 0 c)
    (h.intervalIntegrable 0 c) hmono
  simpa [I,intervalIntegral.integral_const,smul_eq_mul] using he

private theorem R_bounds {c : ℝ} (hc : 0 < c) : 0 < R c ∧ R c ≤ 1/c := by
  have he : 0 < kernel c := exp_pos _
  have hcI : 0 < I c := (mul_pos hc he).trans_le (lower_integral hc)
  exact ⟨div_pos he hcI, (div_le_div_iff₀ hcI hc).mpr (by linarith [lower_integral hc])⟩

/-- The reciprocal factor in the fixed-tail-slope Huber path derivative. -/
def pathReciprocal (c : ℝ) : ℝ :=
  c^2 + c * (exp (-(c^2/2)) / (∫ t in (0 : ℝ)..c, exp (-(t^2/2))))

/-- The reciprocal factor is strictly increasing at every positive
cutoff. This is the curvature step for the constant-tail-slope path. -/
theorem pathReciprocal_strictMono : StrictMonoOn pathReciprocal (Ioi 0) := by
  have hd (c : ℝ) (hc : c ∈ Ioi (0 : ℝ)) :
      HasDerivAt pathReciprocal (2*c+(1-c^2)*R c-c*(R c)^2) c := by
    have hc' : 0 < c := hc
    have he : HasDerivAt kernel (-c*kernel c) c := by
      convert ((((hasDerivAt_id c).pow 2).div_const 2).neg.exp) using 1
      · rfl
      · norm_num [kernel, Pi.neg_apply, Pi.pow_apply, id_eq]
        ring
    have hcont : Continuous kernel := by unfold kernel; fun_prop
    have hI := intervalIntegral.integral_hasDerivAt_right (hcont.intervalIntegrable 0 c)
      hcont.aestronglyMeasurable.stronglyMeasurableAtFilter hcont.continuousAt
    have hIpos : 0 < I c :=
      (mul_pos hc' (exp_pos (-(c^2/2)))).trans_le (lower_integral hc')
    have hr := he.div hI hIpos.ne'
    have h := ((hasDerivAt_id c).pow 2).add ((hasDerivAt_id c).mul hr)
    convert h using 1
    · rfl
    · simp only [id_eq, Pi.div_apply, Nat.cast_ofNat, Nat.reduceSub, pow_one]
      change _ = 2*c*1 + (1*(kernel c/I c) +
        c*(((-c*kernel c)*I c-kernel c*kernel c)/(I c)^2))
      unfold R
      field_simp [hIpos.ne']
      ring
  apply strictMonoOn_of_deriv_pos (convex_Ioi 0)
  · exact fun c hc ↦ (hd c hc).continuousAt.continuousWithinAt
  · intro c hc
    rw [interior_Ioi] at hc
    have hc' : 0 < c := hc
    rw [(hd c hc).deriv]
    obtain ⟨hR,hbound⟩ := R_bounds hc'
    have hcr : c*R c ≤ 1 := by
      rw [mul_comm]
      exact (le_div_iff₀ hc').mp hbound
    have h₁ := mul_nonneg hR.le (sub_nonneg.mpr hcr)
    have h₂ := mul_nonneg hc'.le (sub_nonneg.mpr hcr)
    nlinarith

end

end UnivariateML.AE4Huber

end
