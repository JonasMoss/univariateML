module

public import Mathlib.Analysis.Convex.SpecificFunctions.Pow
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.SpecificLimits.Basic
public import Mathlib.Topology.Order.Compact
public import UnivariateML.Generic.Limit

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Double Pareto likelihood and kink scan

The likelihood is per observation. On the log scale the term omitted from
the physical-data likelihood is the mean log observation.

## Deviations

On two-level data the endpoint profiles tie exactly when the two levels
have equal multiplicity. The held-kink uniqueness claim is conditional on
the kink. For a free kink, positive distances only establish a conditional
optimum. Finite global attainment also requires beating or tying both faces
and all other knots. The log-likelihood profile is strictly convex in a
gap. Its negative, the loss displayed in the dossier, is strictly concave.
-/

@[expose] public section

open Real Set Filter Topology
open scoped BigOperators

namespace UnivariateML.DoublePareto

noncomputable section

variable {n : ℕ}

def mean (z : Fin n → ℝ) : ℝ := (∑ i, z i) / n

def left (z : Fin n → ℝ) (c : ℝ) : ℝ := mean fun i ↦ max (c - z i) 0

def right (z : Fin n → ℝ) (c : ℝ) : ℝ := mean fun i ↦ max (z i - c) 0

def objective (L R a b : ℝ) : ℝ := log (a * b / (a + b)) - a * L - b * R

def logLik (z : Fin n → ℝ) (c a b : ℝ) : ℝ := objective (left z c) (right z c) a b

def radius (L R : ℝ) : ℝ := sqrt L + sqrt R

/-- The conditional supremum for nonnegative deviations of positive total. -/
def height (L R : ℝ) : ℝ := -1 - 2 * log (radius L R)

def profile (z : Fin n → ℝ) (c : ℝ) : ℝ := height (left z c) (right z c)

def leftRate (L R : ℝ) : ℝ := (sqrt L * radius L R)⁻¹

def rightRate (L R : ℝ) : ℝ := (sqrt R * radius L R)⁻¹

def rates : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

def rateLik (L R : ℝ) (p : ℝ × ℝ) : ℝ := objective L R p.1 p.2

private theorem radius_pos {L R : ℝ} (hL : 0 ≤ L) (_hR : 0 ≤ R) (h : 0 < L + R) :
    0 < radius L R := by
  rcases lt_or_eq_of_le hL with hL' | hL'
  · exact add_pos_of_pos_of_nonneg (sqrt_pos.2 hL') (sqrt_nonneg _)
  · exact add_pos_of_nonneg_of_pos (sqrt_nonneg _) (sqrt_pos.2 (by linarith))

/-- The rate bound extends to a zero distance, where the equality condition
cannot hold for positive finite rates. -/
theorem objective_le_height {L R a b : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R)
    (h : 0 < L + R) (ha : 0 < a) (hb : 0 < b) :
    objective L R a b ≤ height L R ∧
      (objective L R a b = height L R ↔
        a * sqrt L = b * sqrt R ∧ a * b / (a + b) * radius L R ^ 2 = 1) := by
  let t := a * b / (a + b) * radius L R ^ 2
  have hs := radius_pos hL hR h
  have hab : 0 < a + b := by positivity
  have ht : 0 < t := by dsimp [t]; positivity
  have hlog : log t = log (a * b / (a + b)) + 2 * log (radius L R) := by
    rw [show t = (a * b / (a + b)) * (radius L R * radius L R) by dsimp [t]; ring,
      log_mul (by positivity) (by positivity), log_mul hs.ne' hs.ne']
    ring
  have hsq : a * L + b * R - t = (a * sqrt L - b * sqrt R)^2 / (a + b) := by
    dsimp [t, radius]
    field_simp [hab.ne']
    linear_combination -(a^2 + a*b) * (sq_sqrt hL) - (b^2 + a*b) * (sq_sqrt hR)
  have hpen : t ≤ a * L + b * R := by
    linarith [div_nonneg (sq_nonneg (a * sqrt L - b * sqrt R)) hab.le]
  have hbound := log_le_sub_one_of_pos ht
  unfold objective height
  constructor
  · linarith
  constructor
  · intro he
    have ht1 : t = 1 := by
      by_contra hn
      linarith [log_lt_sub_one_of_pos ht hn]
    have hz : (a * sqrt L - b * sqrt R)^2 = 0 := by
      apply (div_eq_zero_iff.mp (show (a * sqrt L - b * sqrt R)^2 / (a + b) = 0 by
        rw [← hsq]; linarith)).resolve_right hab.ne'
    exact ⟨sub_eq_zero.mp (mul_self_eq_zero.mp (by simpa only [pow_two] using hz)), ht1⟩
  · rintro ⟨he, ht1⟩
    change t = 1 at ht1
    rw [he, sub_self, zero_pow (by decide : 2 ≠ 0), zero_div] at hsq
    rw [ht1, log_one] at hlog
    linarith

/-- Positive distances give an attained profile and explicit positive rates. -/
theorem fitted_rates {L R : ℝ} (hL : 0 < L) (hR : 0 < R) :
    (leftRate L R, rightRate L R) ∈ rates ∧
      objective L R (leftRate L R) (rightRate L R) = height L R := by
  have hl := sqrt_pos.2 hL
  have hr := sqrt_pos.2 hR
  have hs := radius_pos hL.le hR.le (by positivity)
  have ha : 0 < leftRate L R := by unfold leftRate; positivity
  have hb : 0 < rightRate L R := by unfold rightRate; positivity
  refine ⟨⟨ha, hb⟩, (objective_le_height hL.le hR.le (by positivity) ha hb).2.mpr ?_⟩
  constructor
  · unfold leftRate rightRate
    field_simp [hl.ne', hr.ne', hs.ne']
  · have hh : leftRate L R + rightRate L R ≠ 0 := by positivity
    rw [div_mul_eq_mul_div₀]
    apply (div_eq_iff hh).2
    unfold leftRate rightRate
    field_simp [hl.ne', hr.ne', hs.ne']
    unfold radius
    ring

/-- At a fixed kink, positive distances identify the entire conditional maximizer set. -/
theorem conditional_maximizers {L R : ℝ} (hL : 0 < L) (hR : 0 < R) (p : ℝ × ℝ) :
    (p ∈ rates ∧ IsMaxOn (rateLik L R) rates p) ↔
      p = (leftRate L R, rightRate L R) := by
  have hfit := fitted_rates hL hR
  constructor
  · rintro ⟨hp, hm⟩
    have ha : 0 < p.1 := hp.1
    have hb : 0 < p.2 := hp.2
    have hbd := objective_le_height hL.le hR.le (by positivity) ha hb
    have hge := hm hfit.1
    change objective L R (leftRate L R) (rightRate L R) ≤ objective L R p.1 p.2 at hge
    rw [hfit.2] at hge
    obtain ⟨he, ht⟩ := hbd.2.mp (le_antisymm hbd.1 hge)
    have hl := sqrt_pos.2 hL
    have hr := sqrt_pos.2 hR
    have hs := radius_pos hL.le hR.le (by positivity)
    have hsum : p.1 + p.2 ≠ 0 := by positivity
    rw [div_mul_eq_mul_div₀, div_eq_iff hsum, one_mul] at ht
    apply Prod.ext
    · change p.1 = leftRate L R
      unfold leftRate
      rw [← one_div]
      apply (eq_div_iff (show sqrt L * radius L R ≠ 0 by positivity)).2
      apply mul_right_cancel₀ (show p.2 * radius L R ≠ 0 by positivity)
      calc
        p.1 * (sqrt L * radius L R) * (p.2 * radius L R) =
            (p.1 * p.2 * radius L R ^ 2) * sqrt L := by ring
        _ = (p.1 + p.2) * sqrt L := by rw [ht]
        _ = 1 * (p.2 * radius L R) := by unfold radius; nlinarith
    · change p.2 = rightRate L R
      unfold rightRate
      rw [← one_div]
      apply (eq_div_iff (show sqrt R * radius L R ≠ 0 by positivity)).2
      apply mul_right_cancel₀ (show p.1 * radius L R ≠ 0 by positivity)
      calc
        p.2 * (sqrt R * radius L R) * (p.1 * radius L R) =
            (p.1 * p.2 * radius L R ^ 2) * sqrt R := by ring
        _ = (p.1 + p.2) * sqrt R := by rw [ht]
        _ = 1 * (p.1 * radius L R) := by unfold radius; nlinarith
  · rintro rfl
    refine ⟨hfit.1, ?_⟩
    intro q hq
    change objective L R q.1 q.2 ≤ objective L R (leftRate L R) (rightRate L R)
    rw [hfit.2]
    exact (objective_le_height hL.le hR.le (by positivity) hq.1 hq.2).1


private theorem objective_swap (L R a b : ℝ) : objective L R a b = objective R L b a := by
  unfold objective
  rw [mul_comm a b, add_comm a b]
  ring

private theorem height_swap (L R : ℝ) : height L R = height R L := by
  simp [height, radius, add_comm]

private theorem height_zero_left {R : ℝ} (hR : 0 < R) :
    height 0 R = log R⁻¹ - 1 := by
  simp only [height, radius, sqrt_zero, zero_add, log_inv]
  have h := log_mul (sqrt_pos.2 hR).ne' (sqrt_pos.2 hR).ne'
  rw [← pow_two, sq_sqrt hR.le] at h
  linarith

/-- At zero left deviation the optimized Pareto face is a strict upper bound
and is approached by diverging left rates with right rate `1/R`. -/
theorem pareto_face {R : ℝ} (hR : 0 < R) :
    (∀ a b : ℝ, 0 < a → 0 < b → objective 0 R a b < height 0 R) ∧
    height 0 R = log R⁻¹ - 1 ∧
    Tendsto (fun a : ℝ ↦ objective 0 R a R⁻¹) atTop (𝓝 (height 0 R)) := by
  refine ⟨?_, height_zero_left hR, ?_⟩
  · intro a b ha hb
    have h := objective_le_height le_rfl hR.le (by simpa) ha hb
    refine lt_of_le_of_ne h.1 fun he ↦ ?_
    have hz := (h.2.mp he).1
    simp only [sqrt_zero, mul_zero] at hz
    exact (mul_pos hb (sqrt_pos.2 hR)).ne' hz.symm
  · let b := R⁻¹
    have hb : 0 < b := inv_pos.mpr hR
    have hden : Tendsto (fun a : ℝ ↦ 1 + b * a⁻¹) atTop (𝓝 1) := by
      simpa using (tendsto_const_nhds (x := (1 : ℝ))).add (tendsto_inv_atTop_zero.const_mul b)
    have hnorm : Tendsto (fun a : ℝ ↦ b / (1 + b * a⁻¹)) atTop (𝓝 b) := by
      simpa [Pi.div_def] using (tendsto_const_nhds (x := b)).div hden one_ne_zero
    have hbR : b * R = 1 := inv_mul_cancel₀ hR.ne'
    have hlim := ((continuousAt_log hb.ne').tendsto.comp hnorm).sub
      (tendsto_const_nhds (x := (1 : ℝ)))
    rw [height_zero_left hR]
    change Tendsto (fun a : ℝ ↦ objective 0 R a b) atTop (𝓝 (log b - 1))
    apply (show Tendsto (fun a : ℝ ↦ log (b / (1 + b * a⁻¹)) - 1) atTop
      (𝓝 (log b - 1)) from hlim).congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    have hd : 0 < 1 + b * a⁻¹ := by positivity
    have hs : 0 < a + b := by positivity
    have he : b / (1 + b * a⁻¹) = a * b / (a + b) := by
      field_simp [ha.ne', hd.ne', hs.ne']
    simp [objective, he, hbR]

/-- At zero right deviation the power face is approached by diverging right
rates with left rate `1/L`. -/
theorem power_face {L : ℝ} (hL : 0 < L) :
    (∀ a b : ℝ, 0 < a → 0 < b → objective L 0 a b < height L 0) ∧
    height L 0 = log L⁻¹ - 1 ∧
    Tendsto (fun b : ℝ ↦ objective L 0 L⁻¹ b) atTop (𝓝 (height L 0)) := by
  obtain ⟨hub, he, ht⟩ := pareto_face hL
  rw [height_swap L 0]
  exact ⟨fun a b ha hb ↦ by rw [objective_swap]; exact hub b a hb ha,
    he, ht.congr fun b ↦ (objective_swap L 0 L⁻¹ b).symm⟩

/-- A common kink and zero deviations allow an arbitrarily large likelihood. -/
theorem zero_distances_unbounded (M : ℝ) :
    ∃ a b : ℝ, 0 < a ∧ 0 < b ∧ M < objective 0 0 a b := by
  let a := 2 * exp (M + 1)
  have ha : 0 < a := by dsimp [a]; positivity
  refine ⟨a, a, ha, ha, ?_⟩
  have h : a * a / (a + a) = exp (M + 1) := by
    have he := exp_pos (M + 1)
    dsimp [a]
    field_simp
    ring
  rw [objective, h, log_exp]
  linarith

/-- The both-free rate fit at a held kink exists uniquely exactly when both
distances are positive. -/
theorem held_kink_exists_iff {L R : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R) :
    (∃ p ∈ rates, IsMaxOn (rateLik L R) rates p) ↔ 0 < L ∧ 0 < R := by
  constructor
  · rintro ⟨p, hp, hm⟩
    have ha : 0 < p.1 := hp.1
    have hb : 0 < p.2 := hp.2
    have huv : objective L R p.1 p.2 ∈ upperBounds (rateLik L R '' rates) := by
      rintro _ ⟨q, hq, rfl⟩
      exact hm hq
    rcases lt_or_eq_of_le hL with hl | rfl <;> rcases lt_or_eq_of_le hR with hr | rfl
    · exact ⟨hl, hr⟩
    · obtain ⟨hs, _, ht⟩ := power_face hl
      have hf : ∀ᶠ b : ℝ in atTop, (L⁻¹, b) ∈ rates :=
        (eventually_gt_atTop 0).mono fun _ hb' ↦ ⟨inv_pos.mpr hl, hb'⟩
      exact ((hs _ _ ha hb).not_ge (Generic.limit_le_upperBound hf ht huv)).elim
    · obtain ⟨hs, _, ht⟩ := pareto_face hr
      have hf : ∀ᶠ a : ℝ in atTop, (a, R⁻¹) ∈ rates :=
        (eventually_gt_atTop 0).mono fun _ ha' ↦ ⟨ha', inv_pos.mpr hr⟩
      exact ((hs _ _ ha hb).not_ge (Generic.limit_le_upperBound hf ht huv)).elim
    · obtain ⟨a, b, ha, hb, h⟩ := zero_distances_unbounded (objective 0 0 p.1 p.2)
      exact (h.not_ge (hm (show (a, b) ∈ rates from ⟨ha, hb⟩))).elim
  · rintro ⟨hl, hr⟩
    exact ⟨_, (conditional_maximizers hl hr _).2 rfl⟩

/-- Uniqueness at a held kink includes all boundary and constant cases. -/
theorem held_kink_unique_iff {L R : ℝ} (hL : 0 ≤ L) (hR : 0 ≤ R) :
    (∃! p, p ∈ rates ∧ IsMaxOn (rateLik L R) rates p) ↔ 0 < L ∧ 0 < R := by
  constructor
  · rintro ⟨p, ⟨hp, hm⟩, _⟩
    exact (held_kink_exists_iff hL hR).1 ⟨p, hp, hm⟩
  · rintro ⟨hL', hR'⟩
    refine ⟨_, (conditional_maximizers hL' hR' _).2 rfl, ?_⟩
    exact fun p hp ↦ (conditional_maximizers hL' hR' p).1 hp

theorem constant_unbounded (c M : ℝ) :
    ∃ a b : ℝ, 0 < a ∧ 0 < b ∧ M < logLik (fun _ : Fin n ↦ c) c a b := by
  simpa [logLik, left, right, mean] using zero_distances_unbounded M


private theorem mean_nonneg {f : Fin n → ℝ} (hf : ∀ i, 0 ≤ f i) : 0 ≤ mean f :=
  div_nonneg (Finset.sum_nonneg fun i _ ↦ hf i) (Nat.cast_nonneg _)

private theorem mean_mono {f g : Fin n → ℝ} (h : ∀ i, f i ≤ g i) : mean f ≤ mean g :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦ h i) (Nat.cast_nonneg _)

private theorem mean_strict {f g : Fin n → ℝ} (h : ∀ i, f i ≤ g i)
    (i : Fin n) (hi : f i < g i) : mean f < mean g :=
  div_lt_div_of_pos_right
    (Finset.sum_lt_sum (fun j _ ↦ h j) ⟨i, Finset.mem_univ _, hi⟩)
    (by exact_mod_cast Nat.zero_lt_of_lt i.isLt)

private theorem mean_mix (f g : Fin n → ℝ) (a b : ℝ) :
    mean (fun i ↦ a * f i + b * g i) = a * mean f + b * mean g := by
  simp only [mean, Finset.sum_add_distrib, ← Finset.mul_sum, add_div, mul_div_assoc]

theorem left_pos_iff (z : Fin n → ℝ) (c : ℝ) :
    0 < left z c ↔ ∃ i, z i < c := by
  constructor
  · intro h
    by_contra! hz
    have : left z c = 0 := by
      simp [left, mean, fun i ↦ max_eq_right (sub_nonpos.mpr (hz i))]
    linarith
  · rintro ⟨i, hi⟩
    have h := mean_strict (f := fun _ : Fin n ↦ 0) (g := fun j ↦ max (c - z j) 0)
      (fun _ ↦ le_max_right _ _) i (lt_of_lt_of_le (sub_pos.mpr hi) (le_max_left _ _))
    simpa [left, mean] using h

theorem right_pos_iff (z : Fin n → ℝ) (c : ℝ) :
    0 < right z c ↔ ∃ i, c < z i := by
  simpa [left, right, neg_sub_neg, neg_lt_neg_iff, sub_eq_add_neg, add_comm] using
    left_pos_iff (fun i ↦ -z i) (-c)

private theorem left_nonneg (z : Fin n → ℝ) (c : ℝ) : 0 ≤ left z c :=
  mean_nonneg fun _ ↦ le_max_right _ _

private theorem right_nonneg (z : Fin n → ℝ) (c : ℝ) : 0 ≤ right z c :=
  mean_nonneg fun _ ↦ le_max_right _ _

private theorem left_strict (z : Fin n → ℝ) {x y : ℝ} (hxy : x < y)
    (i : Fin n) (hi : z i ≤ x) : left z x < left z y := by
  refine mean_strict (fun j ↦ max_le_max (sub_le_sub_right hxy.le (z j)) le_rfl) i ?_
  rw [max_eq_left (sub_nonneg.mpr hi), max_eq_left (sub_nonneg.mpr (hi.trans hxy.le))]
  linarith

private theorem right_strict (z : Fin n → ℝ) {x y : ℝ} (hxy : x < y)
    (i : Fin n) (hi : y ≤ z i) : right z y < right z x := by
  refine mean_strict (fun j ↦ max_le_max (sub_le_sub_left hxy.le (z j)) le_rfl) i ?_
  rw [max_eq_left (sub_nonneg.mpr hi), max_eq_left (sub_nonneg.mpr (hxy.le.trans hi))]
  linarith

private theorem deviations_affine (z : Fin n → ℝ) {x y a b : ℝ}
    (hx : x ≤ y) (hz : ∀ i, z i ≤ x ∨ y ≤ z i)
    (ha : 0 ≤ a) (hb : 0 ≤ b) (hab : a + b = 1) :
    left z (a * x + b * y) = a * left z x + b * left z y ∧
    right z (a * x + b * y) = a * right z x + b * right z y := by
  have h1 : x ≤ a * x + b * y := by
    nlinarith [mul_nonneg hb (sub_nonneg.mpr hx), congrArg (x * ·) hab]
  have h2 : a * x + b * y ≤ y := by
    nlinarith [mul_nonneg ha (sub_nonneg.mpr hx), congrArg (y * ·) hab]
  unfold left right
  constructor <;> rw [← mean_mix] <;> congr 1 <;> funext i
  · rcases hz i with hi | hi
    · rw [max_eq_left (sub_nonneg.mpr (hi.trans h1)), max_eq_left (sub_nonneg.mpr hi),
        max_eq_left (sub_nonneg.mpr (hi.trans hx))]
      linear_combination (z i) * hab
    · rw [max_eq_right (sub_nonpos.mpr (h2.trans hi)),
        max_eq_right (sub_nonpos.mpr (hx.trans hi)), max_eq_right (sub_nonpos.mpr hi)]
      ring
  · rcases hz i with hi | hi
    · rw [max_eq_right (sub_nonpos.mpr (hi.trans h1)), max_eq_right (sub_nonpos.mpr hi),
        max_eq_right (sub_nonpos.mpr (hi.trans hx))]
      ring
    · rw [max_eq_left (sub_nonneg.mpr (h2.trans hi)),
        max_eq_left (sub_nonneg.mpr (hx.trans hi)), max_eq_left (sub_nonneg.mpr hi)]
      linear_combination -(z i) * hab

private theorem gap_mix_lt (z : Fin n → ℝ) {x y a b : ℝ}
    (hxy : x < y) (hz : ∀ i, z i ≤ x ∨ y ≤ z i)
    (i j : Fin n) (hi : z i ≤ x) (hj : y ≤ z j)
    (ha : 0 < a) (hb : 0 < b) (hab : a + b = 1) :
    profile z (a * x + b * y) < a * profile z x + b * profile z y := by
  have hx := radius_pos (left_nonneg z x) (right_nonneg z x)
    (add_pos_of_nonneg_of_pos (left_nonneg z x)
      ((right_pos_iff z x).2 ⟨j, hxy.trans_le hj⟩))
  have hy := radius_pos (left_nonneg z y) (right_nonneg z y)
    (add_pos_of_pos_of_nonneg ((left_pos_iff z y).2 ⟨i, hi.trans_lt hxy⟩) (right_nonneg z y))
  obtain ⟨hl, hr⟩ := deviations_affine z hxy.le hz ha.le hb.le hab
  have hl' := strictConcaveOn_sqrt.2 (left_nonneg z x) (left_nonneg z y)
    (left_strict z hxy i hi).ne ha hb hab
  have hr' := strictConcaveOn_sqrt.concaveOn.2 (right_nonneg z x) (right_nonneg z y) ha.le hb.le hab
  simp only [smul_eq_mul] at hl' hr'
  have hrad : a * radius (left z x) (right z x) + b * radius (left z y) (right z y) <
      radius (left z (a * x + b * y)) (right z (a * x + b * y)) := by
    unfold radius
    rw [hl, hr]
    linarith
  have hlog := log_lt_log (by positivity : 0 < a * radius (left z x) (right z x) +
    b * radius (left z y) (right z y)) hrad
  have hchord := strictConcaveOn_log_Ioi.concaveOn.2 hx hy ha.le hb.le hab
  simp only [smul_eq_mul] at hchord
  unfold profile height
  linarith

/-- Strict convexity holds on the closed gap, with the face value used if
an endpoint is a sample extreme. -/
theorem profile_strictConvex_gap (z : Fin n → ℝ) {lo hi : ℝ}
    (hz : ∀ i, z i ≤ lo ∨ hi ≤ z i) (i j : Fin n)
    (hiL : z i ≤ lo) (hiR : hi ≤ z j) :
    StrictConvexOn ℝ (Icc lo hi) (profile z) := by
  refine ⟨convex_Icc _ _, ?_⟩
  intro x hx y hy hne a b ha hb hab
  simp only [smul_eq_mul]
  rcases lt_or_gt_of_ne hne with hxy | hyx
  · exact gap_mix_lt z hxy
      (fun k ↦ (hz k).imp (·.trans hx.1) (hy.2.trans ·))
      i j (hiL.trans hx.1) (hy.2.trans hiR) ha hb hab
  · simpa [add_comm] using gap_mix_lt z hyx
      (fun k ↦ (hz k).imp (·.trans hy.1) (hx.2.trans ·))
      i j (hiL.trans hy.1) (hx.2.trans hiR) hb ha (by linarith)

def Nonconstant (z : Fin n → ℝ) : Prop := ∃ i j, z i < z j

private theorem positive_total {z : Fin n → ℝ} (hz : Nonconstant z) (c : ℝ) :
    0 < left z c + right z c := by
  obtain ⟨i, j, hij⟩ := hz
  by_cases hi : z i < c
  · exact add_pos_of_pos_of_nonneg ((left_pos_iff z c).2 ⟨i, hi⟩) (right_nonneg z c)
  · exact add_pos_of_nonneg_of_pos (left_nonneg z c)
      ((right_pos_iff z c).2 ⟨j, (le_of_not_gt hi).trans_lt hij⟩)

private theorem left_zero_of_bound {z : Fin n → ℝ} {c : ℝ} (h : ∀ i, c ≤ z i) :
    left z c = 0 :=
  le_antisymm (le_of_not_gt fun hp ↦ by
    obtain ⟨i, hi⟩ := (left_pos_iff z c).1 hp
    exact hi.not_ge (h i)) (left_nonneg z c)

private theorem right_zero_of_bound {z : Fin n → ℝ} {c : ℝ} (h : ∀ i, z i ≤ c) :
    right z c = 0 :=
  le_antisymm (le_of_not_gt fun hp ↦ by
    obtain ⟨i, hi⟩ := (right_pos_iff z c).1 hp
    exact hi.not_ge (h i)) (right_nonneg z c)

/-- The sample minimum is a Pareto face on every nonconstant sample. -/
theorem sample_minimum_pareto {z : Fin n → ℝ} {lo : ℝ}
    (hlo : ∀ i, lo ≤ z i) (h : ∃ i, lo < z i) :
    (∀ a b : ℝ, 0 < a → 0 < b → logLik z lo a b < profile z lo) ∧
    profile z lo = log (right z lo)⁻¹ - 1 ∧
    Tendsto (fun a : ℝ ↦ logLik z lo a (right z lo)⁻¹) atTop (𝓝 (profile z lo)) := by
  simpa only [logLik, profile, left_zero_of_bound hlo] using
    pareto_face ((right_pos_iff z lo).2 h)

/-- The sample maximum is a power face on every nonconstant sample. -/
theorem sample_maximum_power {z : Fin n → ℝ} {hi : ℝ}
    (hhi : ∀ i, z i ≤ hi) (h : ∃ i, z i < hi) :
    (∀ a b : ℝ, 0 < a → 0 < b → logLik z hi a b < profile z hi) ∧
    profile z hi = log (left z hi)⁻¹ - 1 ∧
    Tendsto (fun b : ℝ ↦ logLik z hi (left z hi)⁻¹ b) atTop (𝓝 (profile z hi)) := by
  simpa only [logLik, profile, right_zero_of_bound hhi] using
    power_face ((left_pos_iff z hi).2 h)

private theorem max_profile_at_knot {z : Fin n → ℝ} (hz : Nonconstant z)
    {c : ℝ} (hc : ∀ i, z i ≠ c) : ∃ i, profile z c < profile z (z i) := by
  classical
  by_cases hL : ∃ i, z i < c
  · by_cases hR : ∃ i, c < z i
    · obtain ⟨i, hi, him⟩ := Finset.exists_max_image (Finset.univ.filter fun i ↦ z i < c) z (by
        obtain ⟨i, hi⟩ := hL
        exact ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩)
      obtain ⟨j, hj, hjm⟩ := Finset.exists_min_image (Finset.univ.filter fun j ↦ c < z j) z (by
        obtain ⟨j, hj⟩ := hR
        exact ⟨j, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hj⟩⟩)
      have hic := (Finset.mem_filter.mp hi).2
      have hcj := (Finset.mem_filter.mp hj).2
      have hgap : ∀ k, z k ≤ z i ∨ z j ≤ z k := by
        intro k
        rcases lt_or_gt_of_ne (hc k) with hk | hk
        · exact Or.inl (him k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
        · exact Or.inr (hjm k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
      have hd : 0 < z j - z i := sub_pos.mpr (hic.trans hcj)
      let a := (z j - c) / (z j - z i)
      let b := (c - z i) / (z j - z i)
      have ha : 0 < a := div_pos (sub_pos.mpr hcj) hd
      have hb : 0 < b := div_pos (sub_pos.mpr hic) hd
      have hab : a + b = 1 := by dsimp [a, b]; field_simp; ring
      have he : a * z i + b * z j = c := by dsimp [a, b]; field_simp; ring
      have h := gap_mix_lt z (hic.trans hcj) hgap i j le_rfl le_rfl ha hb hab
      rw [he] at h
      by_contra! H
      nlinarith [mul_nonneg ha.le (sub_nonneg.mpr (H i)),
        mul_nonneg hb.le (sub_nonneg.mpr (H j)), congrArg (profile z c * ·) hab]
    · obtain ⟨i, j, hij⟩ := hz
      obtain ⟨k, _, hk⟩ := Finset.exists_max_image Finset.univ z ⟨i, Finset.mem_univ _⟩
      push Not at hR
      have hkc : z k < c := lt_of_le_of_ne (hR k) (hc k)
      have hrad : radius (left z (z k)) (right z (z k)) < radius (left z c) (right z c) := by
        rw [right_zero_of_bound (fun l ↦ hk l (Finset.mem_univ _)), right_zero_of_bound hR]
        unfold radius
        exact add_lt_add_left (sqrt_lt_sqrt (left_nonneg _ _) (left_strict z hkc k le_rfl)) _
      have h := log_lt_log
        (radius_pos (left_nonneg _ _) (right_nonneg _ _) (positive_total ⟨i, j, hij⟩ _)) hrad
      exact ⟨k, by unfold profile height; linarith⟩
  · obtain ⟨i, j, hij⟩ := hz
    obtain ⟨k, _, hk⟩ := Finset.exists_min_image Finset.univ z ⟨i, Finset.mem_univ _⟩
    push Not at hL
    have hck : c < z k := lt_of_le_of_ne (hL k) (hc k).symm
    have hrad : radius (left z (z k)) (right z (z k)) < radius (left z c) (right z c) := by
      rw [left_zero_of_bound (fun l ↦ hk l (Finset.mem_univ _)), left_zero_of_bound hL]
      unfold radius
      exact add_lt_add_right (sqrt_lt_sqrt (right_nonneg _ _) (right_strict z hck k le_rfl)) _
    have h := log_lt_log
      (radius_pos (left_nonneg _ _) (right_nonneg _ _) (positive_total ⟨i, j, hij⟩ _)) hrad
    exact ⟨k, by unfold profile height; linarith⟩

def domain : Set (ℝ × (ℝ × ℝ)) := univ ×ˢ rates

def jointLik (z : Fin n → ℝ) (p : ℝ × (ℝ × ℝ)) : ℝ := logLik z p.1 p.2.1 p.2.2

private theorem profile_le_upperBound {z : Fin n → ℝ} (hz : Nonconstant z) (c : ℝ)
    {u : ℝ} (hu : u ∈ upperBounds (jointLik z '' domain)) : profile z c ≤ u := by
  let L := left z c
  let R := right z c
  change height L R ≤ u
  rcases lt_or_eq_of_le (left_nonneg z c) with hL | hL <;>
    rcases lt_or_eq_of_le (right_nonneg z c) with hR | hR
  · obtain ⟨hp, he⟩ := fitted_rates hL hR
    rw [← he]
    exact hu (mem_image_of_mem (jointLik z) (show (c, leftRate L R, rightRate L R) ∈ domain
      from ⟨mem_univ _, hp⟩))
  · change 0 = R at hR
    rw [← hR]
    have hf : ∀ᶠ b : ℝ in atTop, (c, L⁻¹, b) ∈ domain :=
      (eventually_gt_atTop 0).mono fun _ hb ↦ ⟨mem_univ _, inv_pos.mpr hL, hb⟩
    exact Generic.limit_le_upperBound (L := jointLik z) hf (by
      change Tendsto (fun b : ℝ ↦ objective L R L⁻¹ b) atTop (𝓝 (height L 0))
      rw [← hR]
      exact (power_face hL).2.2) hu
  · change 0 = L at hL
    rw [← hL]
    have hf : ∀ᶠ a : ℝ in atTop, (c, a, R⁻¹) ∈ domain :=
      (eventually_gt_atTop 0).mono fun _ ha ↦ ⟨mem_univ _, ha, inv_pos.mpr hR⟩
    exact Generic.limit_le_upperBound (L := jointLik z) hf (by
      change Tendsto (fun a : ℝ ↦ objective L R a R⁻¹) atTop (𝓝 (height 0 R))
      rw [← hL]
      exact (pareto_face hR).2.2) hu
  · have h := positive_total hz c
    linarith

private theorem profile_le_winner {z : Fin n → ℝ} (hz : Nonconstant z)
    {u : ℝ} (hu : ∀ i, profile z (z i) ≤ u) (c : ℝ) : profile z c ≤ u := by
  by_cases h : ∃ i, z i = c
  · obtain ⟨i, rfl⟩ := h
    exact hu i
  · push Not at h
    obtain ⟨i, hi⟩ := max_profile_at_knot hz h
    exact hi.le.trans (hu i)

/-- A winning observed profile gives the least upper bound, even if the
winning knot represents an unattained face. -/
theorem scan_supremum {z : Fin n → ℝ} (hz : Nonconstant z)
    (j : Fin n) (hj : ∀ i, profile z (z i) ≤ profile z (z j)) :
    IsLUB (jointLik z '' domain) (profile z (z j)) := by
  constructor
  · rintro _ ⟨p, hp, rfl⟩
    exact ((objective_le_height (left_nonneg _ _) (right_nonneg _ _)
      (positive_total hz _) hp.2.1 hp.2.2).1).trans (profile_le_winner hz hj _)
  · intro u hu
    exact profile_le_upperBound hz (z j) hu

/-- Interior winning knots and their fitted rates form the entire set of
finite global maximizers, including when a winning knot ties a face. -/
theorem joint_maximizers {z : Fin n → ℝ} (hz : Nonconstant z) (p : ℝ × (ℝ × ℝ)) :
    (p ∈ domain ∧ IsMaxOn (jointLik z) domain p) ↔
      (∃ j, z j = p.1) ∧ (∃ i, z i < p.1) ∧ (∃ i, p.1 < z i) ∧
        (∀ i, profile z (z i) ≤ profile z p.1) ∧
        p.2 = (leftRate (left z p.1) (right z p.1), rightRate (left z p.1) (right z p.1)) := by
  constructor
  · rintro ⟨hp, hm⟩
    have hu : jointLik z p ∈ upperBounds (jointLik z '' domain) := by
      rintro _ ⟨q, hq, rfl⟩
      exact hm hq
    have hbound := objective_le_height (left_nonneg z p.1) (right_nonneg z p.1)
      (positive_total hz _) hp.2.1 hp.2.2
    have hrate : IsMaxOn (rateLik (left z p.1) (right z p.1)) rates p.2 :=
      fun q hq ↦ hm (show (p.1, q) ∈ domain from ⟨mem_univ _, hq⟩)
    obtain ⟨hL, hR⟩ := (held_kink_exists_iff (left_nonneg z p.1) (right_nonneg z p.1)).1
      ⟨p.2, hp.2, hrate⟩
    have hcompare (i : Fin n) : profile z (z i) ≤ profile z p.1 :=
      (profile_le_upperBound hz (z i) hu).trans hbound.1
    have hobs : ∃ j, z j = p.1 := by
      by_contra! h
      obtain ⟨j, hj⟩ := max_profile_at_knot hz h
      exact hj.not_ge (hcompare j)
    exact ⟨hobs, (left_pos_iff _ _).1 hL, (right_pos_iff _ _).1 hR, hcompare,
      (conditional_maximizers hL hR _).1 ⟨hp.2, hrate⟩⟩
  · rintro ⟨⟨j, hj⟩, hl, hr, hcompare, hrate⟩
    have hf := fitted_rates ((left_pos_iff _ _).2 hl) ((right_pos_iff _ _).2 hr)
    have hp : p ∈ domain := ⟨mem_univ _, hrate.symm ▸ hf.1⟩
    refine ⟨hp, ?_⟩
    intro q hq
    have hub := (scan_supremum hz j (by simpa only [hj] using hcompare)).1
      (mem_image_of_mem (jointLik z) hq)
    have he : jointLik z p = profile z p.1 := by
      change rateLik (left z p.1) (right z p.1) p.2 = _
      rw [hrate]
      exact hf.2
    rwa [he, ← hj]

/-- Positive deviations at the held log kink mean precisely that there are
observations strictly on both sides. -/
theorem held_sample_exists_iff (z : Fin n → ℝ) (c : ℝ) :
    (∃ p ∈ rates, IsMaxOn (rateLik (left z c) (right z c)) rates p) ↔
      (∃ i, z i < c) ∧ (∃ i, c < z i) := by
  rw [held_kink_exists_iff (left_nonneg z c) (right_nonneg z c),
    left_pos_iff, right_pos_iff]

/-- The two-level sample has no finite fit, irrespective of multiplicities. -/
theorem two_level_no_mle {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    ¬ ∃ p ∈ domain, IsMaxOn (jointLik z) domain p := by
  obtain ⟨i, hi'⟩ := hlo
  obtain ⟨j, hj'⟩ := hhi
  have hnc : Nonconstant z := ⟨i, j, by rwa [hi', hj']⟩
  rintro ⟨p, hp, hm⟩
  obtain ⟨⟨k, hk⟩, ⟨l, hl⟩, ⟨r, hr⟩, _⟩ := (joint_maximizers hnc p).1 ⟨hp, hm⟩
  rcases hz k with hkl | hkh
  · have lle : lo ≤ z l := (hz l).elim (·.ge) (fun he ↦ by rw [he]; exact h.le)
    linarith
  · have rle : z r ≤ hi := (hz r).elim (fun he ↦ by rw [he]; exact h.le) (·.le)
    linarith

/-- Only the two one-sided face heights enter the two-level supremum. -/
theorem two_level_supremum {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    IsLUB (jointLik z '' domain) (max (profile z lo) (profile z hi)) := by
  obtain ⟨i, hi'⟩ := hlo
  obtain ⟨j, hj'⟩ := hhi
  have hnc : Nonconstant z := ⟨i, j, by rwa [hi', hj']⟩
  rcases le_total (profile z lo) (profile z hi) with hh | hh
  · rw [max_eq_right hh, ← hj']
    apply scan_supremum hnc j
    intro k
    rcases hz k with hk | hk
    · rwa [hk, hj']
    · rw [hk, hj']
  · rw [max_eq_left hh, ← hi']
    apply scan_supremum hnc i
    intro k
    rcases hz k with hk | hk
    · rw [hk, hi']
    · rwa [hk, hi']

def countBelow (z : Fin n → ℝ) (c : ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ z i < c).card

def countAtOrBelow (z : Fin n → ℝ) (c : ℝ) : ℕ :=
  (Finset.univ.filter fun i ↦ z i ≤ c).card

private theorem two_level_distances {z : Fin n → ℝ} {lo hi : ℝ}
    (h : lo < hi) (hz : ∀ i, z i = lo ∨ z i = hi) :
    right z lo = (hi - lo) * (n - countAtOrBelow z lo : ℝ) / n ∧
      left z hi = (hi - lo) * (countAtOrBelow z lo : ℝ) / n := by
  classical
  have he : ((Finset.univ.filter fun i ↦ ¬ z i ≤ lo).card : ℝ) =
      (n : ℝ) - countAtOrBelow z lo := by
    have hcard := Finset.card_filter_add_card_filter_not (s := Finset.univ)
      (fun i : Fin n ↦ z i ≤ lo)
    simp only [Finset.card_univ, Fintype.card_fin] at hcard
    have hr := congrArg (fun k : ℕ ↦ (k : ℝ)) hcard
    simp only [Nat.cast_add] at hr
    unfold countAtOrBelow
    linarith
  constructor
  · have hterms (i : Fin n) : max (z i - lo) 0 = if z i ≤ lo then 0 else hi - lo := by
      rcases hz i with hh | hh
      · simp [hh]
      · simp [hh, not_le.mpr h, max_eq_left (sub_nonneg.mpr h.le)]
    simp only [right, mean, hterms, Finset.sum_ite, Finset.sum_const_zero, zero_add,
      Finset.sum_const, nsmul_eq_mul, he]
    ring
  · have hterms (i : Fin n) : max (hi - z i) 0 = if z i ≤ lo then hi - lo else 0 := by
      rcases hz i with hh | hh
      · simp [hh, max_eq_left (sub_nonneg.mpr h.le)]
      · simp [hh, not_le.mpr h]
    simp only [left, mean, hterms, Finset.sum_ite, Finset.sum_const_zero, add_zero,
      Finset.sum_const, nsmul_eq_mul, countAtOrBelow]
    ring

/-- Endpoint ties on two-level data require equal multiplicities. -/
theorem two_level_tie_iff {z : Fin n → ℝ} {lo hi : ℝ} (h : lo < hi)
    (hlo : lo ∈ range z) (hhi : hi ∈ range z) (hz : ∀ i, z i = lo ∨ z i = hi) :
    profile z lo = profile z hi ↔ 2 * countAtOrBelow z lo = n := by
  obtain ⟨i, hi'⟩ := hlo
  obtain ⟨j, hj'⟩ := hhi
  have hn : (0 : ℝ) < n := by exact_mod_cast Nat.zero_lt_of_lt i.isLt
  have hl := (left_pos_iff z hi).2 ⟨i, by rwa [hi']⟩
  have hr := (right_pos_iff z lo).2 ⟨j, by rwa [hj']⟩
  have hlo' : left z lo = 0 :=
    left_zero_of_bound fun k ↦ (hz k).elim (·.ge) (fun he ↦ by rw [he]; exact h.le)
  have hhi' : right z hi = 0 :=
    right_zero_of_bound fun k ↦ (hz k).elim (fun he ↦ by rw [he]; exact h.le) (·.le)
  have he : profile z lo = profile z hi ↔ right z lo = left z hi := by
    rw [profile, profile, hlo', hhi', height_swap (left z hi) 0,
      height_zero_left hr, height_zero_left hl]
    simp only [log_inv, sub_left_inj, neg_inj]
    exact ⟨log_injOn_pos hr hl, congrArg log⟩
  rw [he, (two_level_distances h hz).1, (two_level_distances h hz).2]
  constructor
  · intro he
    have hh := (div_left_inj₀ hn.ne').mp he
    have hc := mul_left_cancel₀ (sub_ne_zero.mpr h.ne') hh
    exact_mod_cast (show (2 : ℝ) * countAtOrBelow z lo = n by linarith)
  · intro he
    have hh : (2 : ℝ) * countAtOrBelow z lo = n := by exact_mod_cast he
    rw [show (n : ℝ) - countAtOrBelow z lo = countAtOrBelow z lo by linarith]

/-- Two ordered, distinct observations give a tie of the two faces. -/
theorem two_observations_faces_tie {z : Fin 2 → ℝ} (h : z 0 < z 1) :
    profile z (z 0) = profile z (z 1) := by
  refine (two_level_tie_iff h ⟨0, rfl⟩ ⟨1, rfl⟩ ?_).2 ?_
  · intro i
    fin_cases i <;> simp
  · have hs : (Finset.univ.filter fun i : Fin 2 ↦ z i ≤ z 0) = {0} := by
      ext i
      fin_cases i <;> simp [h.not_ge]
    rw [countAtOrBelow, hs]
    decide

private def checkLoss (a b x c : ℝ) : ℝ := max (a * (c - x)) (b * (x - c))

private def totalLoss (z : Fin n → ℝ) (a b c : ℝ) : ℝ :=
  ∑ i, checkLoss a b (z i) c

private theorem checkLoss_left {a b x c : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b) (h : x ≤ c) :
    checkLoss a b x c = a * (c - x) :=
  max_eq_left ((mul_nonpos_of_nonneg_of_nonpos hb (sub_nonpos.mpr h)).trans
    (mul_nonneg ha (sub_nonneg.mpr h)))

private theorem checkLoss_right {a b x c : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b) (h : c ≤ x) :
    checkLoss a b x c = b * (x - c) :=
  max_eq_right ((mul_nonpos_of_nonneg_of_nonpos ha (sub_nonpos.mpr h)).trans
    (mul_nonneg hb (sub_nonneg.mpr h)))

private theorem held_rates_lik_eq {z : Fin n → ℝ} {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (c : ℝ) : logLik z c a b = log (a * b / (a + b)) - totalLoss z a b c / n := by
  have hterms (i : Fin n) :
      a * max (c - z i) 0 + b * max (z i - c) 0 = checkLoss a b (z i) c := by
    rcases le_total (z i) c with h | h
    · rw [checkLoss_left ha.le hb.le h, max_eq_left (sub_nonneg.mpr h),
        max_eq_right (sub_nonpos.mpr h), mul_zero, add_zero]
    · rw [checkLoss_right ha.le hb.le h, max_eq_left (sub_nonneg.mpr h),
        max_eq_right (sub_nonpos.mpr h), mul_zero, zero_add]
  unfold logLik objective left right
  have h := mean_mix (fun i ↦ max (c - z i) 0) (fun i ↦ max (z i - c) 0) a b
  simp only [hterms, mean] at h
  unfold totalLoss mean
  linarith

private def slope (a b : ℝ) (P : Fin n → Prop)
    [DecidablePred P] : ℝ := (∑ i, if P i then a else -b)

private theorem slope_eq (a b : ℝ) (P : Fin n → Prop)
    [DecidablePred P] :
    slope a b P = (a + b) * ((Finset.univ.filter P).card : ℝ) - b * n := by
  have hcard := Finset.card_filter_add_card_filter_not (s := Finset.univ) P
  simp only [Finset.card_univ, Fintype.card_fin] at hcard
  have hr := congrArg (fun k : ℕ ↦ (k : ℝ)) hcard
  simp only [Nat.cast_add] at hr
  simp only [slope, Finset.sum_ite, Finset.sum_const, nsmul_eq_mul]
  linear_combination -congrArg (b * ·) hr

private theorem support_loss (z : Fin n → ℝ) {a b c : ℝ} (ha : 0 ≤ a) (hb : 0 ≤ b)
    (P : Fin n → Prop) [DecidablePred P]
    (hl : ∀ i, P i → z i ≤ c) (hr : ∀ i, ¬ P i → c ≤ z i) (d : ℝ) :
    slope a b P * (d - c) ≤ totalLoss z a b d - totalLoss z a b c := by
  unfold slope totalLoss
  rw [Finset.sum_mul, ← Finset.sum_sub_distrib]
  apply Finset.sum_le_sum
  intro i _
  by_cases hi : P i
  · simp only [ite_eq_left hi, checkLoss_left ha hb (hl i hi)]
    have h := le_max_left (a * (d - z i)) (b * (z i - d))
    unfold checkLoss
    linarith
  · simp only [ite_eq_right hi, checkLoss_right ha hb (hr i hi)]
    have h := le_max_right (a * (d - z i)) (b * (z i - d))
    unfold checkLoss
    linarith

private theorem same_branch_loss (z : Fin n → ℝ) {a b c d : ℝ}
    (ha : 0 ≤ a) (hb : 0 ≤ b) (P : Fin n → Prop) [DecidablePred P]
    (hl : ∀ i, P i → z i ≤ c ∧ z i ≤ d)
    (hr : ∀ i, ¬ P i → c ≤ z i ∧ d ≤ z i) :
    totalLoss z a b d - totalLoss z a b c = slope a b P * (d - c) := by
  unfold slope totalLoss
  rw [Finset.sum_mul, ← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  by_cases h : P i
  · simp only [ite_eq_left h, checkLoss_left ha hb (hl i h).1, checkLoss_left ha hb (hl i h).2]
    ring
  · simp only [ite_eq_right h, checkLoss_right ha hb (hr i h).1, checkLoss_right ha hb (hr i h).2]
    ring

private theorem right_gap (z : Fin n → ℝ) (c : ℝ) :
    ∃ d, c < d ∧ ∀ i, c < z i → d ≤ z i := by
  classical
  by_cases h : (Finset.univ.filter fun i ↦ c < z i).Nonempty
  · obtain ⟨i, hi, hm⟩ := Finset.exists_min_image _ z h
    exact ⟨z i, (Finset.mem_filter.mp hi).2, fun j hj ↦
      hm j (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hj⟩)⟩
  · exact ⟨c + 1, by linarith, fun i hi ↦
      (h ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩).elim⟩

/-- The full held-rate kink set is the weighted quantile, with separate
strict and weak counts so that repeated observations are handled exactly. -/
theorem held_rates_maximizers (z : Fin n → ℝ) (hn : 0 < n) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) (c : ℝ) :
    IsMaxOn (fun d ↦ logLik z d a b) univ c ↔
      (a + b) * (countBelow z c : ℝ) ≤ b * n ∧
        b * n ≤ (a + b) * (countAtOrBelow z c : ℝ) := by
  classical
  let P : Fin n → Prop := fun i ↦ z i < c
  let Q : Fin n → Prop := fun i ↦ z i ≤ c
  have hP : slope a b P = (a + b) * (countBelow z c : ℝ) - b * n :=
    slope_eq a b P
  have hQ : slope a b Q = (a + b) * (countAtOrBelow z c : ℝ) - b * n :=
    slope_eq a b Q
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have he : IsMaxOn (fun d ↦ logLik z d a b) univ c ↔ ∀ d, 0 ≤
      totalLoss z a b d - totalLoss z a b c := by
    simp only [IsMaxOn, IsMaxFilter, Filter.eventually_principal, mem_univ, true_implies]
    apply forall_congr'
    intro d
    change logLik z d a b ≤ logLik z c a b ↔ _
    rw [held_rates_lik_eq ha hb, held_rates_lik_eq ha hb]
    rw [sub_le_sub_iff_left, div_le_div_iff_of_pos_right hn', sub_nonneg]
  rw [he]
  have hsign : ((a + b) * (countBelow z c : ℝ) ≤ b * n ∧
      b * n ≤ (a + b) * (countAtOrBelow z c : ℝ)) ↔
      slope a b P ≤ 0 ∧ 0 ≤ slope a b Q := by
    rw [hP, hQ, sub_nonpos, sub_nonneg]
  rw [hsign]
  constructor
  · intro h
    constructor
    · obtain ⟨e, hec, hez⟩ := right_gap (fun i ↦ -z i) (-c)
      have hd : -e < c := by linarith
      have hy := same_branch_loss z (c := c) (d := -e) ha.le hb.le P
        (fun i hi ↦ ⟨hi.le, by
          have hg := hez i (neg_lt_neg (show z i < c from hi))
          linarith only [hg]⟩)
        (fun i hi ↦ ⟨le_of_not_gt hi, hd.le.trans (le_of_not_gt hi)⟩)
      have hy' := h (-e)
      rw [hy] at hy'
      exact nonpos_of_mul_nonneg_left hy' (sub_neg.mpr hd)
    · obtain ⟨d, hd, hez⟩ := right_gap z c
      have hy := same_branch_loss z (c := c) (d := d) ha.le hb.le Q
        (fun i hi ↦ ⟨hi, hi.trans hd.le⟩)
        (fun i hi ↦ ⟨(lt_of_not_ge hi).le, hez i (lt_of_not_ge hi)⟩)
      have hy' := h d
      rw [hy] at hy'
      exact nonneg_of_mul_nonneg_left hy' (sub_pos.mpr hd)
  · rintro ⟨hl, hr⟩ d
    rcases le_total d c with hd | hd
    · exact (mul_nonneg_of_nonpos_of_nonpos hl (sub_nonpos.mpr hd)).trans
        (support_loss z ha.le hb.le P (fun _ h ↦ h.le) (fun _ ↦ le_of_not_gt) d)
    · exact (mul_nonneg hr (sub_nonneg.mpr hd)).trans
        (support_loss z ha.le hb.le Q (fun _ h ↦ h) (fun _ h ↦ (lt_of_not_ge h).le) d)


/-- Positive held rates always give a finite maximizing kink on nonempty data. -/
theorem held_rates_exists (z : Fin n → ℝ) (hn : 0 < n) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) : ∃ c, IsMaxOn (fun d ↦ logLik z d a b) univ c := by
  classical
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let i : Fin n := ⟨0, hn⟩
  obtain ⟨l, _, hl⟩ := Finset.exists_min_image Finset.univ z ⟨i, Finset.mem_univ _⟩
  obtain ⟨r, _, hr⟩ := Finset.exists_max_image Finset.univ z ⟨i, Finset.mem_univ _⟩
  have hzero : z l ≤ z r := hl r (Finset.mem_univ _)
  have hcont : Continuous (totalLoss z a b) := by
    unfold totalLoss checkLoss
    fun_prop
  obtain ⟨c, hc, hm⟩ := isCompact_Icc.exists_isMinOn
    (show (Icc (z l) (z r)).Nonempty from ⟨z l, le_rfl, hzero⟩) hcont.continuousOn
  refine ⟨c, fun d _ ↦ ?_⟩
  change logLik z d a b ≤ logLik z c a b
  rw [held_rates_lik_eq ha hb, held_rates_lik_eq ha hb,
    sub_le_sub_iff_left, div_le_div_iff_of_pos_right hn']
  by_cases hd : d ∈ Icc (z l) (z r)
  · exact hm hd
  by_cases hd' : d < z l
  · refine (hm (show z l ∈ Icc (z l) (z r) from ⟨le_rfl, hzero⟩)).trans ?_
    unfold totalLoss
    apply Finset.sum_le_sum
    intro j hj
    rw [checkLoss_right ha.le hb.le (hl j hj),
      checkLoss_right ha.le hb.le (hd'.le.trans (hl j hj))]
    exact mul_le_mul_of_nonneg_left (sub_le_sub_left hd'.le _) hb.le
  · have hdr : z r < d := lt_of_not_ge fun h ↦ hd ⟨le_of_not_gt hd', h⟩
    refine (hm (show z r ∈ Icc (z l) (z r) from ⟨hzero, le_rfl⟩)).trans ?_
    unfold totalLoss
    apply Finset.sum_le_sum
    intro j hj
    rw [checkLoss_left ha.le hb.le (hr j hj),
      checkLoss_left ha.le hb.le ((hr j hj).trans hdr.le)]
    exact mul_le_mul_of_nonneg_left (sub_le_sub_right hdr.le _) ha.le

/-- If the right slope vanishes in a gap with observed endpoints, that
entire closed gap is the held-rate maximizer set. -/
theorem held_rates_interval {z : Fin n → ℝ} (hn : 0 < n) {a b lo hi : ℝ}
    (ha : 0 < a) (hb : 0 < b) (h : lo < hi) (hlo : lo ∈ range z) (hhi : hi ∈ range z)
    (hz : ∀ i, z i ≤ lo ∨ hi ≤ z i)
    (hbal : (a + b) * (countAtOrBelow z lo : ℝ) = b * n) :
    {c | IsMaxOn (fun d ↦ logLik z d a b) univ c} = Icc lo hi := by
  classical
  obtain ⟨i, hiL⟩ := hlo
  obtain ⟨j, hiR⟩ := hhi
  have hab : 0 < a + b := add_pos ha hb
  ext c
  change IsMaxOn (fun d ↦ logLik z d a b) univ c ↔ c ∈ Icc lo hi
  rw [held_rates_maximizers z hn ha hb c, ← hbal]
  simp only [mul_le_mul_iff_right₀ hab, Nat.cast_le]
  constructor
  · rintro ⟨hl, hr⟩
    constructor
    · by_contra! hc
      have hs : (Finset.univ.filter fun k ↦ z k ≤ c) ⊆
          (Finset.univ.filter fun k ↦ z k ≤ lo) := by
        intro k hk
        simp only [Finset.mem_filter, Finset.mem_univ, true_and] at hk ⊢
        exact hk.trans hc.le
      have hlt := Finset.card_lt_card ((Finset.ssubset_iff_of_subset hs).2
        ⟨i, by simp [hiL], by simp [hiL, hc.not_ge]⟩)
      exact (not_lt_of_ge hr) hlt
    · by_contra! hc
      have hs : (Finset.univ.filter fun k ↦ z k ≤ lo) ⊆
          (Finset.univ.filter fun k ↦ z k < c) := by
        intro k hk
        simp only [Finset.mem_filter, Finset.mem_univ, true_and] at hk ⊢
        exact hk.trans_lt (h.trans hc)
      have hlt := Finset.card_lt_card ((Finset.ssubset_iff_of_subset hs).2
        ⟨j, by simp [hiR, hc], by simp [hiR, h.not_ge]⟩)
      exact (not_lt_of_ge hl) hlt
  · rintro ⟨hl, hr⟩
    constructor
    · apply Finset.card_le_card
      intro k hk
      simp only [Finset.mem_filter, Finset.mem_univ, true_and] at hk ⊢
      exact (hz k).resolve_right (not_le_of_gt (hk.trans_le hr))
    · apply Finset.card_le_card
      intro k hk
      simp only [Finset.mem_filter, Finset.mem_univ, true_and] at hk ⊢
      exact hk.trans hl

/-- Exact global existence after comparing the observed knots and faces. -/
theorem joint_exists_iff {z : Fin n → ℝ} (hz : Nonconstant z) :
    (∃ p ∈ domain, IsMaxOn (jointLik z) domain p) ↔
      ∃ j : Fin n, (∃ i, z i < z j) ∧ (∃ i, z j < z i) ∧
        ∀ i, profile z (z i) ≤ profile z (z j) := by
  constructor
  · rintro ⟨p, hp, hm⟩
    obtain ⟨⟨j, hj⟩, hl, hr, hc, _⟩ := (joint_maximizers hz p).1 ⟨hp, hm⟩
    exact ⟨j, by rw [hj]; exact hl, by rw [hj]; exact hr, by rw [hj]; exact hc⟩
  · rintro ⟨j, hl, hr, hc⟩
    exact ⟨_, (joint_maximizers hz
      (z j, leftRate (left z (z j)) (right z (z j)), rightRate (left z (z j)) (right z (z j)))).2
      ⟨⟨j, rfl⟩, hl, hr, hc, rfl⟩⟩

def logDensity (x scale a b : ℝ) : ℝ :=
  log (a * b / (a + b)) - log x +
    if x ≤ scale then a * log (x / scale) else -b * log (x / scale)

/-- The physical-data mean likelihood differs from the reduced log-scale
likelihood by the data-only Jacobian term. -/
theorem mean_logDensity_eq {x : Fin n → ℝ} (hn : 0 < n) (hx : ∀ i, 0 < x i)
    {scale : ℝ} (hs : 0 < scale) (a b : ℝ) :
    mean (fun i ↦ logDensity (x i) scale a b) =
      logLik (fun i ↦ log (x i)) (log scale) a b - mean (fun i ↦ log (x i)) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hterms (i : Fin n) :
      logDensity (x i) scale a b = log (a * b / (a + b)) - log (x i) -
        a * max (log scale - log (x i)) 0 - b * max (log (x i) - log scale) 0 := by
    unfold logDensity
    rw [log_div (hx i).ne' hs.ne']
    by_cases h : x i ≤ scale
    · have hh := log_le_log (hx i) h
      rw [ite_eq_left h, max_eq_left (sub_nonneg.mpr hh), max_eq_right (sub_nonpos.mpr hh)]
      ring
    · have hh := (log_lt_log hs (lt_of_not_ge h)).le
      rw [ite_eq_right h, max_eq_right (sub_nonpos.mpr hh), max_eq_left (sub_nonneg.mpr hh)]
      ring
  simp only [hterms, mean, logLik, objective, left, right, Finset.sum_sub_distrib,
    ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  field_simp
  ring

end

end UnivariateML.DoublePareto
