module

public import UnivariateML.Generic.Score
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# A fixed-shape beta width profile

The normalized positions are in `[0,1]`, and at least one is one.
The variable is inverse width, scaled so that free-endpoint contact is
at one. Shape `a` is the shape at that endpoint.
-/

@[expose] public section

open Real Set Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2BetaWidth

noncomputable section

variable {n : ℕ}

/-- Log likelihood terms that depend on normalized inverse width. -/
def logLik (z : Fin n → ℝ) (a b u : ℝ) : ℝ :=
  ∑ i, (b * log u + (a - 1) * log (1 - u * z i))

/-- Derivative of the log likelihood on the strictly containing domain. -/
def score (z : Fin n → ℝ) (a b u : ℝ) : ℝ :=
  (n : ℝ) * b / u - (a - 1) * ∑ i, z i / (1 - u * z i)

private theorem gap_pos {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    {u : ℝ} (hu : u ∈ Ioo (0 : ℝ) 1) (i : Fin n) : 0 < 1 - u * z i := by
  have h := mul_le_mul_of_nonneg_left (hz i).2 hu.1.le
  linarith [hu.2]

/-- A subunit free-endpoint shape makes the finite log likelihood diverge
as the endpoint approaches any tied farthest observations. -/
theorem spike {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hcontact : ∃ i, z i = 1) {a : ℝ} (ha : a < 1) (b : ℝ) :
    Tendsto (fun t ↦ logLik z a b (1 - t)) (𝓝[>] 0) atTop := by
  classical
  let A (i : Fin n) : ℝ := if z i = 1 then a - 1 else 0
  let R (i : Fin n) (t : ℝ) : ℝ := b * log (1 - t) +
    (if z i = 1 then 0 else (a - 1) * log (1 - (1 - t) * z i))
  have hA : (∑ i, A i) < 0 := by
    obtain ⟨j, hj⟩ := hcontact
    apply Finset.sum_neg'
    · intro i _
      by_cases hi : z i = 1
      · simpa only [A, ite_eq_left hi] using sub_nonpos.mpr ha.le
      · simp [A, hi]
    · exact ⟨j, Finset.mem_univ _, by simpa [A, hj] using sub_neg.mpr ha⟩
  have hR (i : Fin n) : ContinuousAt (R i) 0 := by
    have hleft : ContinuousAt (fun t : ℝ ↦ b * log (1 - t)) 0 :=
      continuousAt_const.mul ((continuousAt_const.sub continuousAt_id).log (by norm_num))
    by_cases hi : z i = 1
    · simpa [R, hi] using hleft
    · have hpos : 0 < 1 - (1 - (0 : ℝ)) * z i := by
        simpa using sub_pos.mpr (lt_of_le_of_ne (hz i).2 hi)
      have harg : ContinuousAt (fun t : ℝ ↦ 1 - (1 - t) * z i) 0 := by fun_prop
      have hright : ContinuousAt (fun t : ℝ ↦ (a - 1) * log (1 - (1 - t) * z i)) 0 :=
        continuousAt_const.mul (harg.log hpos.ne')
      simp only [R, ite_eq_right hi]
      exact hleft.add hright
  apply Tendsto.congr' _ ((tendsto_log_nhdsGT_zero.const_mul_atBot_of_neg hA).atTop_add
    (tendsto_finsetSum Finset.univ fun i _ ↦ (hR i).continuousWithinAt))
  apply Eventually.of_forall
  intro t
  change (∑ i, A i) * log t + (∑ i, R i t) = logLik z a b (1 - t)
  unfold logLik
  rw [Finset.sum_mul, ← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : z i = 1 <;> simp [A, R, hi, add_comm]

/-- At free-endpoint shape one, the unique finite maximizer is closed
contact. -/
theorem contact_max (hn : 0 < n) {z : Fin n → ℝ} {b : ℝ} (hb : 0 < b) :
    IsMaxOn (logLik z 1 b) (Ioc (0 : ℝ) 1) 1 ∧
      ∀ u ∈ Ioc (0 : ℝ) 1, IsMaxOn (logLik z 1 b) (Ioc 0 1) u → u = 1 := by
  have h (u : ℝ) : logLik z 1 b u = ((n : ℝ) * b) * log u := by
    simp [logLik, nsmul_eq_mul, mul_assoc]
  have hcoef : 0 < (n : ℝ) * b := mul_pos (Nat.cast_pos.mpr hn) hb
  have hstrict : StrictMonoOn (logLik z 1 b) (Ioc (0 : ℝ) 1) := by
    intro u hu v _ huv
    rw [h, h]
    exact mul_lt_mul_of_pos_left (log_lt_log hu.1 huv) hcoef
  have h1 : (1 : ℝ) ∈ Ioc 0 1 := ⟨zero_lt_one, le_rfl⟩
  refine ⟨?_, ?_⟩
  · intro u hu
    rcases eq_or_lt_of_le hu.2 with rfl | hlt
    · exact le_rfl
    · exact (hstrict hu h1 hlt).le
  · intro u hu hm
    by_contra! hne
    exact (hstrict hu h1 (lt_of_le_of_ne hu.2 hne)).not_ge (hm h1)

/-- Above free-endpoint shape one, the maximum is at the unique interior
score root. The theorem allows zeros and repeats in the normalized data. -/
theorem interior_max (hn : 0 < n) {z : Fin n → ℝ} (hz : ∀ i, z i ∈ Icc (0 : ℝ) 1)
    (hcontact : ∃ i, z i = 1) {a b : ℝ} (ha : 1 < a) (hb : 0 < b) :
    (∃! u, u ∈ Ioo (0 : ℝ) 1 ∧ score z a b u = 0) ∧
      ∀ u ∈ Ioo (0 : ℝ) 1,
        IsMaxOn (logLik z a b) (Ioo 0 1) u ↔ score z a b u = 0 := by
  have hN : 0 < (n : ℝ) * b := mul_pos (Nat.cast_pos.mpr hn) hb
  have hA := sub_pos.mpr ha
  have hderiv (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) :
      HasDerivAt (logLik z a b) (score z a b u) u := by
    have ht (i : Fin n) :
        HasDerivAt (fun w ↦ b * log w + (a - 1) * log (1 - w * z i))
          (b / u - (a - 1) * (z i / (1 - u * z i))) u := by
      have harg := ((hasDerivAt_id u).mul_const (z i)).const_sub 1
      convert ((hasDerivAt_log hu.1.ne').const_mul b).add
        ((harg.log (gap_pos hz hu i).ne').const_mul (a - 1)) using 1
      · rfl
      · simp only [Function.id_def]
        ring
    convert HasDerivAt.sum (u := Finset.univ) (fun i _ ↦ ht i) using 1
    · ext w
      simp [logLik, Finset.sum_apply]
    · simp [score, Finset.sum_sub_distrib, ← Finset.mul_sum, nsmul_eq_mul, mul_div_assoc]
  have hcont : ContinuousOn (score z a b) (Ioo (0 : ℝ) 1) := by
    intro u hu
    apply ContinuousAt.continuousWithinAt
    have hsum : ContinuousAt (fun w ↦ ∑ i, z i / (1 - w * z i)) u := by
      apply tendsto_finsetSum Finset.univ
      intro i _
      have h : ContinuousAt (fun w : ℝ ↦ 1 - w * z i) u := by fun_prop
      exact (continuousAt_const.div h (gap_pos hz hu i).ne').tendsto
    exact (continuousAt_const.div continuousAt_id hu.1.ne').sub
      (continuousAt_const.mul hsum)
  have hmono : StrictAntiOn (score z a b) (Ioo (0 : ℝ) 1) := by
    intro u hu v hv huv
    have hsum : (∑ i, z i / (1 - u * z i)) ≤ ∑ i, z i / (1 - v * z i) := by
      apply Finset.sum_le_sum
      intro i _
      have hle := mul_le_mul_of_nonneg_right huv.le (hz i).1
      exact div_le_div_of_nonneg_left (hz i).1 (gap_pos hz hv i) (by linarith)
    unfold score
    linarith [div_lt_div_of_pos_left hN hu.1 huv, mul_le_mul_of_nonneg_left hsum hA.le]
  let u : ℝ := b / (2 * (a + b))
  let v : ℝ := 1 - (a - 1) / (2 * ((n : ℝ) * b + (a - 1)))
  have hu₀ : 0 < u := by
    exact div_pos hb (by linarith)
  have hu₁ : u < 1 / 2 := (div_lt_iff₀ (by linarith : 0 < 2 * (a + b))).2 (by linarith)
  have hv₀ : 1 / 2 < v := by
    have hδ := (div_lt_iff₀ (by linarith : 0 < 2 * ((n : ℝ) * b + (a - 1)))).2
      (show a - 1 < (1 / 2) * (2 * ((n : ℝ) * b + (a - 1))) by linarith)
    dsimp [v]
    linarith
  have hv₁ : v < 1 := by
    dsimp [v]
    linarith [div_pos hA (show 0 < 2 * ((n : ℝ) * b + (a - 1)) by linarith)]
  have hu : u ∈ Ioo (0 : ℝ) 1 := ⟨hu₀, by linarith⟩
  have hv : v ∈ Ioo (0 : ℝ) 1 := ⟨by linarith, hv₁⟩
  apply Generic.existsUnique_score_zero_of_signs (convex_Ioo 0 1) hderiv hcont hmono hu hv
    (hu₁.le.trans hv₀.le)
  · have hsum : (∑ i, z i / (1 - u * z i)) ≤ (n : ℝ) * (1 / (1 - u)) := by
      have hbound (i : Fin n) : z i / (1 - u * z i) ≤ 1 / (1 - u) := by
        apply (div_le_div_iff₀ (gap_pos hz hu i) (sub_pos.mpr hu.2)).2
        nlinarith [(hz i).2]
      simpa [nsmul_eq_mul] using Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hbound i)
    have hmul : u * (2 * (a + b)) = b := div_mul_cancel₀ _ (by linarith)
    have hlt : (a - 1) * u < b * (1 - u) := by
      nlinarith [hmul]
    have hdiv : (a - 1) / (1 - u) < b / u :=
      (div_lt_div_iff₀ (sub_pos.mpr hu.2) hu₀).2 hlt
    unfold score
    have hbound := mul_le_mul_of_nonneg_left hsum hA.le
    have hgap := mul_lt_mul_of_pos_left hdiv (Nat.cast_pos.mpr hn : (0 : ℝ) < n)
    rw [← mul_div_assoc, ← mul_div_assoc] at hgap
    rw [mul_one_div, ← mul_div_assoc, mul_comm (a - 1) (n : ℝ)] at hbound
    exact sub_pos.mpr (hbound.trans_lt hgap)
  · obtain ⟨j, hj⟩ := hcontact
    have hsum : 1 / (1 - v) ≤ ∑ i, z i / (1 - v * z i) := by
      have hbound := Finset.single_le_sum (s := Finset.univ) (f := fun i ↦ z i / (1 - v * z i))
        (fun i _ ↦ div_nonneg (hz i).1 (gap_pos hz hv i).le) (Finset.mem_univ j)
      simpa [hj] using hbound
    have hmul : (1 - v) * (2 * ((n : ℝ) * b + (a - 1))) = a - 1 := by
      dsimp [v]
      rw [sub_sub_cancel]
      exact div_mul_cancel₀ _ (by linarith)
    have hlt : (n : ℝ) * b / v < (a - 1) / (1 - v) := by
      apply (div_lt_div_iff₀ hv.1 (sub_pos.mpr hv.2)).2
      nlinarith [hmul]
    unfold score
    have hbound := mul_le_mul_of_nonneg_left hsum hA.le
    rw [mul_one_div] at hbound
    linarith

end

end UnivariateML.Endpoint2BetaWidth

end
