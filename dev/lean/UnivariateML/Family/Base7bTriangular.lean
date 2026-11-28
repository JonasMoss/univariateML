module

public import UnivariateML.Family.Base7Triangular

set_option linter.style.header false

/-! Comparison of the two knots on a two-level triangular sample.

## Deviations
This covers the two-level branch of the all-free uniqueness claim.
The higher-level gradient separation and null-set assertion are separate.
-/

@[expose] public section

namespace UnivariateML.Base7bTriangular

open Set Real Triangular LocScaleTriangular

private theorem left_height {n : ℕ} (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i = a ∨ x i = b) (w : ℝ × ℝ) :
    widthLikelihood x a w =
      (2 / (w.1 + w.2)) ^ n * ((w.2 - (b - a)) / w.2) ^
        (Finset.univ.filter (fun i ↦ x i = b)).card := by
  classical
  unfold widthLikelihood likelihood
  rw [show a + w.2 - (a - w.1) = w.1 + w.2 by ring]
  congr 1
  rw [← Finset.prod_const, Finset.prod_filter]
  apply Finset.prod_congr rfl
  intro i _
  rcases hx i with hi | hi
  · simp [hi, factor, hab.ne]
  · simp [hi, factor, hab, hab.not_gt]
    rw [show a + w.2 - b = w.2 - (b - a) by ring]

private theorem right_height {n : ℕ} (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i = a ∨ x i = b) (w : ℝ × ℝ) :
    widthLikelihood x b w =
      (2 / (w.1 + w.2)) ^ n * ((w.1 - (b - a)) / w.1) ^
        (Finset.univ.filter (fun i ↦ x i = a)).card := by
  classical
  unfold widthLikelihood likelihood
  rw [show b + w.2 - (b - w.1) = w.1 + w.2 by ring]
  congr 1
  rw [← Finset.prod_const, Finset.prod_filter]
  apply Finset.prod_congr rfl
  intro i _
  rcases hx i with hi | hi
  · simp [hi, factor, hab]
    rw [show a - (b - w.1) = w.1 - (b - a) by ring]
  · simp [hi, factor, hab.ne']

/-- On two distinct observed levels, the relative ordering of the profiled
heights is exactly the ordering of the counts at the modes. In particular
every more frequent level wins, and balanced counts give both modes. -/
theorem two_level_knot_comparison {n : ℕ} (x : Fin n → ℝ) {a b : ℝ} (hab : a < b)
    (hx : ∀ i, x i = a ∨ x i = b) (ha : ∃ i, x i = a) (hb : ∃ i, x i = b)
    {u v : ℝ × ℝ} (hu : u ∈ widthDomain x a) (hv : v ∈ widthDomain x b)
    (hmu : IsMaxOn (widthLikelihood x a) (widthDomain x a) u)
    (hmv : IsMaxOn (widthLikelihood x b) (widthDomain x b) v) :
    let A := (Finset.univ.filter (fun i ↦ x i = a)).card
    let B := (Finset.univ.filter (fun i ↦ x i = b)).card
    (widthLikelihood x a u ≤ widthLikelihood x b v ↔ A ≤ B) := by
  classical
  intro A B
  obtain ⟨i, hi⟩ := ha
  obtain ⟨j, hj⟩ := hb
  let d := b - a
  have hd : 0 < d := sub_pos.mpr hab
  have hA : 0 < A := Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  have hB : 0 < B := Finset.card_pos.mpr ⟨j, by simp [hj]⟩
  have domL {w : ℝ} (hw : d ≤ w) : (0, w) ∈ widthDomain x a := by
    refine ⟨le_rfl, hd.le.trans hw, by simpa using hd.trans_le hw, fun k ↦ ?_⟩
    rcases hx k with e | e
    · simp [e, hd.le.trans hw]
    · simp only [e, sub_zero]
      exact ⟨hab.le, by dsimp [d] at hw; linarith⟩
  have domR {w : ℝ} (hw : d ≤ w) : (w, 0) ∈ widthDomain x b := by
    refine ⟨hd.le.trans hw, le_rfl, by simpa using hd.trans_le hw, fun k ↦ ?_⟩
    rcases hx k with e | e
    · simp only [e, add_zero]
      exact ⟨by dsimp [d] at hw; linarith, hab.le⟩
    · simp [e, hd.le.trans hw]
  have hu₂ : d < u.2 := by
    have H := (hu.2.2.2 j).2
    rw [hj] at H
    have hweak : d ≤ u.2 := by dsimp [d]; linarith
    have hpos : 0 < widthLikelihood x a (0, d + 1) := by
      rw [left_height x hab hx]
      simp only [zero_add, show d + 1 - (b - a) = 1 by dsimp [d]; ring]
      positivity
    apply hweak.lt_of_ne
    intro e
    have hzero : widthLikelihood x a u = 0 := by
      rw [left_height x hab hx, ← e]
      change _ * ((d - d) / d) ^ B = 0
      simp [hB.ne']
    have H := hmu (domL (w := d + 1) (by linarith))
    rw [hzero] at H
    exact hpos.not_ge H
  have hv₁ : d < v.1 := by
    have H := (hv.2.2.2 i).1
    rw [hi] at H
    have hweak : d ≤ v.1 := by dsimp [d]; linarith
    have hpos : 0 < widthLikelihood x b (d + 1, 0) := by
      rw [right_height x hab hx]
      simp only [add_zero, show d + 1 - (b - a) = 1 by dsimp [d]; ring]
      positivity
    apply hweak.lt_of_ne
    intro e
    have hzero : widthLikelihood x b v = 0 := by
      rw [right_height x hab hx, ← e]
      change _ * ((d - d) / d) ^ A = 0
      simp [hA.ne']
    have H := hmv (domR (w := d + 1) (by linarith))
    rw [hzero] at H
    exact hpos.not_ge H
  let f (w : ℝ) := (w - d) / w
  have hf (w : ℝ) (hw : d < w) : 0 < f w ∧ f w < 1 := by
    have h := hd.trans hw
    exact ⟨div_pos (sub_pos.mpr hw) h, (div_lt_one h).mpr (by linarith)⟩
  have hpeak {w c : ℝ} (hw : 0 < w) (hc : 0 ≤ c) :
      (2 / (c + w)) ^ n ≤ (2 / w) ^ n :=
    pow_le_pow_left₀ (by positivity)
      (div_le_div_of_nonneg_left (by norm_num) hw (by linarith)) _
  have hleft : widthLikelihood x a u ≤ (2 / u.2) ^ n * f u.2 ^ B := by
    rw [left_height x hab hx]
    exact mul_le_mul_of_nonneg_right (hpeak (hd.trans hu₂) hu.1)
      (pow_nonneg (hf _ hu₂).1.le _)
  have hright : widthLikelihood x b v ≤ (2 / v.1) ^ n * f v.1 ^ A := by
    rw [right_height x hab hx, add_comm v.1 v.2]
    exact mul_le_mul_of_nonneg_right (hpeak (hd.trans hv₁) hv.2.1)
      (pow_nonneg (hf _ hv₁).1.le _)
  constructor
  · intro h
    by_contra! hBA
    have H : widthLikelihood x b v < widthLikelihood x a (0, v.1) := by
      rw [left_height x hab hx]
      simp only [zero_add]
      exact hright.trans_lt (mul_lt_mul_of_pos_left
        (pow_lt_pow_right_of_lt_one₀ (hf _ hv₁).1 (hf _ hv₁).2 hBA)
        (by have hp := hd.trans hv₁; positivity))
    exact H.not_ge ((hmu (domL hv₁.le)).trans h)
  · intro hAB
    apply hleft.trans
    apply le_trans _ (hmv (domR hu₂.le))
    rw [right_height x hab hx]
    simp only [add_zero]
    have hp : f u.2 ^ B ≤ f u.2 ^ A := by
      rcases hAB.lt_or_eq with hlt | he
      · exact (pow_lt_pow_right_of_lt_one₀ (hf _ hu₂).1 (hf _ hu₂).2 hlt).le
      · rw [he]
    exact mul_le_mul_of_nonneg_left hp (by have hp := hd.trans hu₂; positivity)

/-- In the extreme-versus-interior gradient comparison, the score
identity forces the interior candidate's likelihood ratio below one.
Here `I` is the extreme-level count and `J` counts all other levels. -/
theorem extreme_knot_ratio_lt_one {I J : ℕ} (hI : 0 < I)
    {a b d : ℝ} (hd : 0 < d) (ha : d < a) (hb : 0 < b)
    (hscore : (I : ℝ) / a = (J : ℝ) / b) :
    ((b + d) / (a + b)) ^ (I + J) *
      (1 - d / a) ^ I * (1 + d / b) ^ J < 1 := by
  have hap : 0 < a := hd.trans ha
  have hp : 0 < 1 - d / a := sub_pos.mpr ((div_lt_one hap).mpr ha)
  have hq : 0 < 1 + d / b := by positivity
  have hout : 0 < ((b + d) / (a + b)) := by positivity
  have hlt : (b + d) / (a + b) < 1 := (div_lt_one (by positivity)).mpr (by linarith)
  have ha' := log_le_sub_one_of_pos hp
  have hb' := log_le_sub_one_of_pos hq
  have hA := mul_le_mul_of_nonneg_left ha' (Nat.cast_nonneg (α := ℝ) I)
  have hB := mul_le_mul_of_nonneg_left hb' (Nat.cast_nonneg (α := ℝ) J)
  have hs := congrArg (fun v : ℝ ↦ v * d) hscore
  have hbnd : (I : ℝ) * log (1 - d / a) + (J : ℝ) * log (1 + d / b) ≤ 0 := by
    simp only [div_eq_mul_inv] at hA hB hs ⊢
    nlinarith only [hA, hB, hs]
  have hP : 0 < ((b + d) / (a + b)) ^ (I + J) * (1 - d / a) ^ I * (1 + d / b) ^ J :=
    mul_pos (mul_pos (pow_pos hout _) (pow_pos hp _)) (pow_pos hq _)
  apply (log_lt_log_iff hP zero_lt_one).mp
  rw [log_one, log_mul (by positivity) (by positivity), log_mul (by positivity) (by positivity),
    log_pow, log_pow, log_pow]
  have hneg := mul_neg_of_pos_of_neg
    (Nat.cast_pos.mpr (Nat.add_pos_left hI J) : 0 < ((I + J : ℕ) : ℝ)) (log_neg hout hlt)
  linarith only [hneg, hbnd]

end UnivariateML.Base7bTriangular

end
