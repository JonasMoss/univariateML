module

public import UnivariateML.Generic.LocationScale

public import Mathlib.Analysis.Convex.Continuous
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Convex.SpecificFunctions.Deriv
public import Mathlib.Topology.Order.Compact
public import Mathlib.Analysis.Real.Sqrt
public import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
public import Mathlib.Analysis.Real.Pi.Bounds
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fernández–Steel two-piece normal

The side scales are positive. The public mean is the mode plus
`sqrt (2 / π)` times the difference of the side scales, and public skew
is the square root of their ratio.

## Deviations

The held-mean and held-skew results use inverse total width. At a fixed
positive skew it is a bijective coordinate for positive standard deviation.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPieceNormal

noncomputable section

variable {n : ℕ}

/-- Mean of the right half of a standard normal. -/
def halfMean : ℝ := sqrt (2 / π)

/-- Log likelihood in mode and side scales. -/
def logLik (x : Fin n → ℝ) (c a b : ℝ) : ℝ :=
  (n : ℝ) * log (halfMean / (a + b)) -
    ∑ i, (x i - c)^2 / (2 * (if x i < c then a else b)^2)

/-- With inverse total width `u`, the signed standardized residual
before division by its side fraction. -/
def residual (μ w u z : ℝ) : ℝ := (z - μ) * u + halfMean * (1 - 2 * w)

/-- The left width fraction is `w` and the right width fraction is `1-w`.
Thus the public skew is `sqrt ((1-w)/w)`. -/
def heldMeanSkew (x : Fin n → ℝ) (μ w u : ℝ) : ℝ :=
  (n : ℝ) * log (halfMean * u) -
    ∑ i, (residual μ w u (x i))^2 /
      (2 * (if residual μ w u (x i) < 0 then w else 1 - w)^2)

private theorem halfMean_pos : 0 < halfMean :=
  sqrt_pos.2 (div_pos (by norm_num) pi_pos)

/-- Inverse width recovers the actual two-piece likelihood at the held mean. -/
theorem heldMeanSkew_eq (x : Fin n → ℝ) (μ : ℝ) {w u : ℝ} (hu : 0 < u) :
    heldMeanSkew x μ w u =
      logLik x (μ - halfMean * ((1 - w) / u - w / u)) (w / u) ((1 - w) / u) := by
  have hwidth : w / u + (1 - w) / u = 1 / u := by ring
  have hres (z : ℝ) :
      (z - (μ - halfMean * ((1 - w) / u - w / u))) * u =
        residual μ w u z := by
    unfold residual
    field_simp
    ring
  unfold heldMeanSkew logLik
  rw [hwidth, div_div_eq_mul_div, div_one]
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  have hs : residual μ w u (x i) < 0 ↔
      x i < μ - halfMean * ((1 - w) / u - w / u) := by
    rw [← hres, mul_neg_iff, sub_neg]
    simp [hu, not_lt_of_ge hu.le]
  by_cases h : residual μ w u (x i) < 0
  · rw [ite_eq_left h, ite_eq_left (hs.mp h), ← hres]
    simp only [mul_pow, div_pow]
    rw [show 2 * (w ^ 2 / u ^ 2) = (2 * w ^ 2) / u ^ 2 by ring]
    rw [div_div_eq_mul_div]
  · rw [ite_eq_right h, ite_eq_right (mt hs.mpr h), ← hres]
    simp only [mul_pow, div_pow]
    rw [show 2 * ((1 - w) ^ 2 / u ^ 2) = (2 * (1 - w) ^ 2) / u ^ 2 by ring]
    rw [div_div_eq_mul_div]

/-- With all data at the held mean, the Gaussian exponent is constant
throughout the positive inverse-width ray and log likelihood is unbounded. -/
theorem heldMeanSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (heldMeanSkew x μ w) atTop atTop := by
  have hres (u : ℝ) (i : Fin n) :
      residual μ w u (x i) = halfMean * (1 - 2 * w) := by
    simp [residual, hx i]
  have hu : Tendsto (fun u : ℝ ↦ halfMean * u) atTop atTop :=
    (tendsto_const_mul_atTop_of_pos halfMean_pos).2 tendsto_id
  have hlog := tendsto_log_atTop.comp hu
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have h := (tendsto_const_mul_atTop_of_pos hn').2 hlog
  let penalty := ∑ _i : Fin n, (halfMean * (1 - 2 * w))^2 /
        (2 * (if halfMean * (1 - 2 * w) < 0 then w else 1 - w)^2)
  convert tendsto_atTop_add_const_right atTop (-penalty) h using 1
  funext u
  simp only [heldMeanSkew, hres, Function.comp_def, penalty, sub_eq_add_neg]

private def loss (w r : ℝ) : ℝ :=
  r^2 / (2 * (if r < 0 then w else 1 - w)^2)

private theorem loss_eq (w r : ℝ) :
    loss w r = (2 * w^2)⁻¹ * (max (-r) 0)^2 +
      (2 * (1 - w)^2)⁻¹ * (max r 0)^2 := by
  by_cases h : r < 0
  · rw [loss, ite_eq_left h, max_eq_left (by linarith : 0 ≤ -r), max_eq_right h.le]
    ring
  · rw [loss, ite_eq_right h, max_eq_right (by linarith : -r ≤ 0),
      max_eq_left (le_of_not_gt h)]
    ring

private theorem convex_hinge_sq (d e : ℝ) :
    ConvexOn ℝ univ (fun t : ℝ ↦ (max (d * t + e) 0)^2) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ a b ha hb hab
  simp only [smul_eq_mul]
  let X := max (d * x + e) 0
  let Y := max (d * y + e) 0
  have hX : 0 ≤ X := le_max_right _ _
  have hY : 0 ≤ Y := le_max_right _ _
  have hR : 0 ≤ a * X + b * Y := by positivity
  have hmix : d * (a * x + b * y) + e = a * (d * x + e) + b * (d * y + e) := by
    linear_combination -e * hab
  have hle : max (d * (a * x + b * y) + e) 0 ≤ a * X + b * Y := by
    rw [hmix]
    exact max_le (add_le_add (mul_le_mul_of_nonneg_left (le_max_left _ _) ha)
      (mul_le_mul_of_nonneg_left (le_max_left _ _) hb)) hR
  calc
    _ ≤ (a * X + b * Y)^2 := pow_le_pow_left₀ (le_max_right _ _) hle 2
    _ ≤ a * X^2 + b * Y^2 := by
      have he : a * X^2 + b * Y^2 - (a * X + b * Y)^2 = a * b * (X - Y)^2 := by
        linear_combination -(a * X^2 + b * Y^2) * hab
      linarith [show 0 ≤ a * b * (X - Y)^2 by positivity]

private theorem convex_loss_affine (w d e : ℝ) :
    ConvexOn ℝ univ (fun t : ℝ ↦ loss w (d * t + e)) := by
  have h₁ := (convex_hinge_sq (-d) (-e)).smul
    (show 0 ≤ (2 * w^2)⁻¹ by positivity)
  have h₂ := (convex_hinge_sq d e).smul
    (show 0 ≤ (2 * (1 - w)^2)⁻¹ by positivity)
  convert h₁.add h₂ using 1
  funext t
  simp only [loss_eq, Pi.add_apply, smul_eq_mul, neg_mul, neg_add_rev, add_comm (-e)]

private theorem loss_lower {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (r : ℝ) :
    r^2 / 2 ≤ loss w r := by
  unfold loss
  have hb : (if r < 0 then w else 1 - w) ∈ Ioo (0 : ℝ) 1 := by
    split
    · exact hw
    · exact ⟨by linarith [hw.2], by linarith [hw.1]⟩
  have hb' := pow_le_pow_left₀ hb.1.le hb.2.le 2
  exact div_le_div_of_nonneg_left (sq_nonneg _) (mul_pos (by norm_num) (pow_pos hb.1 _))
    (by simpa using mul_le_mul_of_nonneg_left hb' (show 0 ≤ (2 : ℝ) by norm_num))

private theorem heldMeanSkew_strictConcave (hn : 0 < n) (x : Fin n → ℝ) (μ w : ℝ) :
    StrictConcaveOn ℝ (Ioi 0) (heldMeanSkew x μ w) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨convex_Ioi _, ?_⟩
  intro u hu v hv huv a b ha hb hab
  change 0 < u at hu
  change 0 < v at hv
  have hp : 0 < a * u + b * v := by positivity
  have hl := strictConcaveOn_log_Ioi.2 hu hv huv ha hb hab
  simp only [smul_eq_mul] at hl ⊢
  have hl' : a * ((n : ℝ) * log (halfMean * u)) +
      b * ((n : ℝ) * log (halfMean * v)) <
      (n : ℝ) * log (halfMean * (a * u + b * v)) := by
    rw [log_mul halfMean_pos.ne' hu.ne', log_mul halfMean_pos.ne' hv.ne',
      log_mul halfMean_pos.ne' hp.ne']
    have he : a * log halfMean + b * log halfMean = log halfMean := by
      linear_combination log halfMean * hab
    nlinarith only [mul_lt_mul_of_pos_left hl hn', congrArg (fun t : ℝ ↦ n * t) he]
  have hh (t : ℝ) (i : Fin n) :
      loss w (residual μ w t (x i)) =
        (residual μ w t (x i))^2 /
          (2 * (if residual μ w t (x i) < 0 then w else 1 - w)^2) := rfl
  simp only [heldMeanSkew, ← hh]
  have hsum : ∑ i, loss w (residual μ w (a * u + b * v) (x i)) ≤
      a * (∑ i, loss w (residual μ w u (x i))) +
      b * (∑ i, loss w (residual μ w v (x i))) := by
    rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
    apply Finset.sum_le_sum
    intro i _
    simpa only [residual, smul_eq_mul] using
      (convex_loss_affine w (x i - μ) (halfMean * (1 - 2 * w))).2
        (mem_univ u) (mem_univ v) ha.le hb.le hab
  linarith

/-- A nonzero residual gives exactly one finite maximizing scale when
mean and skew are held. The scale is represented by inverse total width. -/
theorem heldMeanSkew_existsUnique (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hx : ∃ i, x i ≠ μ) :
    ∃! u, u ∈ Ioi (0 : ℝ) ∧ IsMaxOn (heldMeanSkew x μ w) (Ioi 0) u := by
  let f := heldMeanSkew x μ w
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨i, hi⟩ := hx
  let d := x i - μ
  let e := halfMean * (1 - 2 * w)
  let L := f 1
  have hd : 0 < d^2 := sq_pos_of_ne_zero (sub_ne_zero.mpr hi)
  have hbound (u : ℝ) : f u ≤ (n : ℝ) * log (halfMean * u) - (d*u + e)^2 / 2 := by
    change _ - (∑ j, loss w (residual μ w u (x j))) ≤ _
    have hs : loss w (residual μ w u (x i)) ≤
        ∑ j, loss w (residual μ w u (x j)) :=
      Finset.single_le_sum (fun j _ ↦ (by unfold loss; positivity :
        0 ≤ loss w (residual μ w u (x j)))) (Finset.mem_univ i)
    exact sub_le_sub_left ((loss_lower hw (d*u + e)).trans hs) _
  let A := min 1 (exp (L / n) / halfMean)
  have hA : 0 < A := lt_min one_pos (div_pos (exp_pos _) halfMean_pos)
  have hA1 : A ≤ 1 := min_le_left _ _
  have hsmall (u : ℝ) (hu : 0 < u) (huA : u ≤ A) : f u ≤ L := by
    have he : halfMean * u ≤ exp (L / n) := by
      have hh := (le_div_iff₀ halfMean_pos).mp (huA.trans (min_le_right _ _))
      simpa only [mul_comm] using hh
    have hl := log_le_log (mul_pos halfMean_pos hu) he
    rw [log_exp] at hl
    have hh := mul_le_mul_of_nonneg_left hl hn'.le
    have hL : (n : ℝ) * (L / n) = L := by field_simp
    linarith [hbound u, sq_nonneg (d*u + e)]
  let B := max 1 (8 * (n * halfMean + |L| + e^2 + 1) / d^2)
  have hB1 : 1 ≤ B := le_max_left _ _
  have hlarge (u : ℝ) (huB : B ≤ u) : f u ≤ L := by
    have hu1 : 1 ≤ u := hB1.trans huB
    have hu : 0 < u := zero_lt_one.trans_le hu1
    have hD : 8 * (n * halfMean + |L| + e^2 + 1) ≤ u * d^2 :=
      (div_le_iff₀ hd).mp ((le_max_right _ _).trans huB)
    have hDu := mul_le_mul_of_nonneg_right hD hu.le
    have hlog := mul_le_mul_of_nonneg_left
      (log_le_sub_one_of_pos (mul_pos halfMean_pos hu)) hn'.le
    have hsq : d^2 * u^2 ≤ 2 * (d*u + e)^2 + 2 * e^2 := by
      nlinarith only [sq_nonneg (d*u + 2*e)]
    have habs : -L ≤ |L| := neg_le_abs L
    have hp : 0 ≤ (|L| + e^2 + 1) * (u - 1) :=
      mul_nonneg (by positivity) (sub_nonneg.mpr hu1)
    have hnp : 0 ≤ (n : ℝ) * halfMean * u :=
      mul_nonneg (mul_nonneg hn'.le halfMean_pos.le) hu.le
    nlinarith only [hbound u, hDu, hlog, hsq, habs, hp, hnp, hn', sq_nonneg e]
  have hsc := heldMeanSkew_strictConcave hn x μ w
  have hcont : ContinuousOn f (Icc A B) :=
    (hsc.concaveOn.continuousOn isOpen_Ioi).mono (fun _ ht ↦ hA.trans_le ht.1)
  have h1 : (1 : ℝ) ∈ Icc A B := ⟨hA1, hB1⟩
  obtain ⟨u, hu, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩ hcont
  have hp : u ∈ Ioi (0 : ℝ) := hA.trans_le hu.1
  have hmax : IsMaxOn f (Ioi 0) u := by
    intro v hv
    by_cases hvA : A ≤ v
    · by_cases hvB : v ≤ B
      · exact hm (show v ∈ Icc A B from ⟨hvA, hvB⟩)
      · exact (hlarge v (le_of_not_ge hvB)).trans (hm h1)
    · exact (hsmall v hv (le_of_not_ge hvA)).trans (hm h1)
  exact ⟨u, ⟨hp, hmax⟩, fun _ ht ↦ hsc.eq_of_isMaxOn ht.2 hmax ht.1 hp⟩

/-- The held-mean and held-skew likelihood attains its maximum exactly
when the sample contains a nonzero residual from the mean. -/
theorem heldMeanSkew_exists_iff (hn : 0 < n) {x : Fin n → ℝ} {μ w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    (∃ u ∈ Ioi (0 : ℝ), IsMaxOn (heldMeanSkew x μ w) (Ioi 0) u) ↔
      ∃ i, x i ≠ μ := by
  constructor
  · rintro ⟨u, _, hmax⟩
    by_contra h
    have hx : ∀ i, x i = μ := fun i ↦ not_ne_iff.mp (fun hi ↦ h ⟨i, hi⟩)
    have ht := (heldMeanSkew_collapse (w := w) hn hx).eventually
      (eventually_gt_atTop (heldMeanSkew x μ w u))
    obtain ⟨v, hv, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and ht).exists
    exact hgt.not_ge (hmax hv)
  · intro hx
    obtain ⟨u, ⟨hu, hm⟩, _⟩ := heldMeanSkew_existsUnique hn hw hx
    exact ⟨u, hu, hm⟩

private theorem strictConvex_loss_affine {w d e : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1)
    (hd : d ≠ 0) :
    StrictConvexOn ℝ univ (fun t : ℝ ↦ loss w (d * t + e)) := by
  let al := (2 * w^2)⁻¹
  let ar := (2 * (1 - w)^2)⁻¹
  let k := min al ar
  have hal : 0 < al := inv_pos.mpr (mul_pos (by norm_num) (pow_pos hw.1 2))
  have har : 0 < ar := inv_pos.mpr (mul_pos (by norm_num)
    (pow_pos (sub_pos.mpr hw.2) 2))
  have hk : 0 < k := lt_min hal har
  have hleft : 0 ≤ al - k := sub_nonneg.mpr (min_le_left _ _)
  have hright : 0 ≤ ar - k := sub_nonneg.mpr (min_le_right _ _)
  have h₁ := (convex_hinge_sq (-d) (-e)).smul hleft
  have h₂ := (convex_hinge_sq d e).smul hright
  have h₀ : StrictConvexOn ℝ univ (fun t : ℝ ↦ k * (d*t+e)^2) := by
    refine ⟨convex_univ, ?_⟩
    intro x _ y _ hxy a b ha hb hab
    simp only [smul_eq_mul]
    have he : a * (k * (d*x+e)^2) + b * (k * (d*y+e)^2) -
        k * (d * (a*x+b*y) + e)^2 = k * a * b * d^2 * (x-y)^2 := by
      linear_combination -(k * (d^2*(a*x^2+b*y^2) - e^2)) * hab
    have hp : 0 < k * a * b * d^2 * (x-y)^2 :=
      mul_pos (mul_pos (mul_pos (mul_pos hk ha) hb) (sq_pos_of_ne_zero hd))
        (sq_pos_of_ne_zero (sub_ne_zero.mpr hxy))
    linarith
  convert h₀.add_convexOn (h₁.add h₂) using 1
  funext t
  rw [loss_eq]
  change al * _ + ar * _ = k * _ +
    ((al - k) * max (-d*t + -e) 0 ^ 2 + (ar - k) * max (d*t+e) 0 ^ 2)
  rw [show -d*t + -e = -(d*t+e) by ring]
  by_cases h : 0 ≤ d*t+e
  · rw [max_eq_right (neg_nonpos.mpr h), max_eq_left h]
    ring
  · rw [max_eq_left (by linarith : 0 ≤ -(d*t+e)), max_eq_right (le_of_not_ge h)]
    ring

/-- For positive fixed scale and skew, the log likelihood tends to minus
infinity at both location ends and admits exactly one maximizing mean.
The shape of the sample is unrestricted. -/
theorem heldScaleSkew (hn : 0 < n) (x : Fin n → ℝ) {w u : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hu : 0 < u) :
    Tendsto (fun μ ↦ heldMeanSkew x μ w u) (cocompact ℝ) atBot ∧
      ∃! μ, IsMaxOn (fun m ↦ heldMeanSkew x m w u) univ μ := by
  let f := fun μ ↦ heldMeanSkew x μ w u
  let C := (n : ℝ) * log (halfMean * u)
  let e (i : Fin n) := x i * u + halfMean * (1 - 2 * w)
  have hres (μ : ℝ) (i : Fin n) : residual μ w u (x i) = -u * μ + e i := by
    unfold residual e
    ring
  have hf (μ : ℝ) : f μ = C - ∑ i, loss w (-u * μ + e i) := by
    simp only [f, C, heldMeanSkew, loss, hres]
  have hsc : StrictConcaveOn ℝ univ f := by
    refine ⟨convex_univ, ?_⟩
    intro p hp q hq hpq a b ha hb hab
    simp only [smul_eq_mul, hf]
    have hsum : ∑ i, loss w (-u * (a*p+b*q) + e i) <
        a * (∑ i, loss w (-u*p+e i)) + b * (∑ i, loss w (-u*q+e i)) := by
      rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
      apply Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      intro i _
      simpa only [smul_eq_mul] using
        (strictConvex_loss_affine (e := e i) hw (neg_ne_zero.mpr hu.ne')).2
          hp hq hpq ha hb hab
    have hC : a*C+b*C=C := by linear_combination C * hab
    linarith
  let i : Fin n := ⟨0, hn⟩
  have hle (μ : ℝ) : f μ ≤ C - (-u * μ + e i)^2 / 2 := by
    rw [hf]
    have hterm : loss w (-u*μ+e i) ≤ ∑ j, loss w (-u*μ+e j) :=
      Finset.single_le_sum (f := fun j ↦ loss w (-u*μ+e j))
        (fun _ _ ↦ by unfold loss; positivity) (Finset.mem_univ i)
    exact sub_le_sub_left ((loss_lower hw _).trans hterm) _
  have htop : Tendsto f atTop atBot := by
    have hr : Tendsto (fun μ ↦ -u * μ + e i) atTop atBot :=
      tendsto_atBot_add_const_right _ _ (tendsto_id.const_mul_atTop_of_neg (neg_lt_zero.mpr hu))
    have hsq : Tendsto (fun μ ↦ (-u * μ + e i)^2) atTop atTop := by
      simpa only [Function.comp_def, neg_sq] using
        (tendsto_pow_atTop (by norm_num : (2 : ℕ) ≠ 0)).comp (tendsto_neg_atBot_atTop.comp hr)
    have ht : Tendsto (fun μ ↦ C - (-u * μ + e i)^2 / 2) atTop atBot := by
      simpa only [sub_eq_add_neg, add_comm C, Function.comp_def] using
        tendsto_atBot_add_const_right _ C
          (tendsto_neg_atTop_atBot.comp (hsq.atTop_div_const (by norm_num : (0 : ℝ) < 2)))
    exact tendsto_atBot_mono hle ht
  have hbot : Tendsto f atBot atBot := by
    have hr : Tendsto (fun μ ↦ -u * μ + e i) atBot atTop := by
      simpa only [Function.comp_def, id_eq, mul_neg, neg_mul] using
        (tendsto_atTop_add_const_right _ (e i)
          ((tendsto_const_mul_atTop_of_pos hu).2 (tendsto_id (α := ℝ) (x := atTop)))).comp
          tendsto_neg_atBot_atTop
    have hsq : Tendsto (fun μ ↦ (-u * μ + e i)^2) atBot atTop :=
      (tendsto_pow_atTop (by norm_num : (2 : ℕ) ≠ 0)).comp hr
    have ht : Tendsto (fun μ ↦ C - (-u * μ + e i)^2 / 2) atBot atBot := by
      simpa only [sub_eq_add_neg, add_comm C, Function.comp_def] using
        tendsto_atBot_add_const_right _ C
          (tendsto_neg_atTop_atBot.comp (hsq.atTop_div_const (by norm_num : (0 : ℝ) < 2)))
    exact tendsto_atBot_mono hle ht
  refine ⟨?_, ?_⟩
  · rw [cocompact_eq_atBot_atTop]
    exact tendsto_sup.mpr ⟨hbot, htop⟩
  obtain ⟨lo, hlo⟩ := eventually_atBot.mp (hbot.eventually (eventually_le_atBot (f 0)))
  obtain ⟨hi, hhi⟩ := eventually_atTop.mp (htop.eventually (eventually_le_atBot (f 0)))
  let I := Icc (min lo 0) (max hi 0)
  have h0 : (0 : ℝ) ∈ I := ⟨min_le_right _ _, le_max_right _ _⟩
  have hc : ContinuousOn f I :=
    (hsc.concaveOn.continuousOn isOpen_univ).mono (subset_univ _)
  obtain ⟨m, _, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨0, h0⟩ hc
  have hm' : IsMaxOn f univ m := by
    intro p _
    by_cases hp : p ∈ I
    · exact hm hp
    · apply le_trans _ (hm h0)
      by_cases hl : min lo 0 ≤ p
      · apply hhi
        exact (le_max_left _ _).trans (le_of_not_ge (fun hu ↦ hp ⟨hl, hu⟩))
      · exact hlo p ((le_of_not_ge hl).trans (min_le_left _ _))
  exact ⟨m, hm', fun q hq ↦ hsc.eq_of_isMaxOn hq hm' (mem_univ q) (mem_univ m)⟩

/-- The joint fit at fixed skew uses mode and total width. -/
def heldSkew (x : Fin n → ℝ) (w : ℝ) (p : ℝ × ℝ) : ℝ :=
  logLik x p.1 (w * p.2) ((1 - w) * p.2)

private theorem heldSkew_eq (x : Fin n → ℝ) (w : ℝ) (p : ℝ × ℝ)
    (hp : p ∈ LocationScale.domain) :
    heldSkew x w p = LocationScale.logLik (loss w) x p + n * log halfMean := by
  have hs : 0 < p.2 := hp.2
  have hwidth : w * p.2 + (1 - w) * p.2 = p.2 := by ring
  have he (i : Fin n) : loss w ((x i - p.1) / p.2) =
      (x i - p.1)^2 / (2 * (if x i < p.1 then w * p.2 else (1 - w) * p.2)^2) := by
    unfold loss
    have hsign : (x i - p.1) / p.2 < 0 ↔ x i < p.1 := by
      rw [div_lt_iff₀ hs, zero_mul, sub_neg]
    by_cases h : x i < p.1
    · rw [ite_eq_left h, ite_eq_left (hsign.mpr h), div_pow, div_div, mul_pow]
      ring
    · rw [ite_eq_right h, ite_eq_right (mt hsign.mp h), div_pow, div_div, mul_pow]
      ring
  unfold heldSkew logLik LocationScale.logLik
  rw [hwidth, log_div halfMean_pos.ne' hs.ne']
  simp only [he, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  ring

/-- If the sample is constant, the fixed-skew model has a collapsing
path at its common observation, expressed in inverse total width. -/
theorem heldSkew_collapse (hn : 0 < n) {x : Fin n → ℝ} {c : ℝ} (w : ℝ)
    (hx : ∀ i, x i = c) :
    Tendsto (fun u ↦ logLik x c (w / u) ((1 - w) / u)) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hw : Tendsto (fun u : ℝ ↦ halfMean * u) atTop atTop :=
    (tendsto_const_mul_atTop_of_pos halfMean_pos).2 tendsto_id
  have ht := (tendsto_const_mul_atTop_of_pos hn').2 (tendsto_log_atTop.comp hw)
  convert ht using 1
  funext u
  have hwidth : w / u + (1 - w) / u = 1 / u := by ring
  simp only [logLik, hx, sub_self, zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div,
    Finset.sum_const_zero, sub_zero, hwidth, div_div_eq_mul_div, div_one, Function.comp_def]

/-- With skew held, nonconstant data give exactly one joint maximizer
and constant data have no finite maximizer. -/
theorem heldSkew_existsUnique_iff (hn : 0 < n) (x : Fin n → ℝ) {w : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) :
    ((∃ p ∈ LocationScale.domain, IsMaxOn (heldSkew x w) LocationScale.domain p) ↔
      LocationScale.Nonconstant x) ∧
      (LocationScale.Nonconstant x →
        ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (heldSkew x w) LocationScale.domain p) := by
  suffices h :
      (∃ p ∈ LocationScale.domain, IsMaxOn (heldSkew x w) LocationScale.domain p) →
        LocationScale.Nonconstant x by
    suffices hu : LocationScale.Nonconstant x →
        ∃! p, p ∈ LocationScale.domain ∧ IsMaxOn (heldSkew x w) LocationScale.domain p by
      exact ⟨⟨h, fun hx ↦ (hu hx).exists⟩, hu⟩
    intro hx
    have hce : StrictConvexOn ℝ univ (loss w) := by
      simpa only [one_mul, add_zero] using
        (strictConvex_loss_affine (d := 1) (e := 0) hw one_ne_zero)
    have hl : LocationScale.LinearLowerBound (loss w) := by
      refine ⟨1, one_pos, 1, ?_⟩
      intro t
      have hs : |t|^2 = t^2 := sq_abs t
      nlinarith only [loss_lower hw t, sq_nonneg (|t| - 1), hs]
    have he (p : ℝ × ℝ) :
        (p ∈ LocationScale.domain ∧ IsMaxOn (heldSkew x w) LocationScale.domain p) ↔
          (p ∈ LocationScale.domain ∧
            IsMaxOn (LocationScale.logLik (loss w) x) LocationScale.domain p) := by
      constructor
      · rintro ⟨hp, hm⟩
        refine ⟨hp, fun q hq ↦ ?_⟩
        have hq' : heldSkew x w q ≤ heldSkew x w p := hm hq
        rw [heldSkew_eq x w q hq, heldSkew_eq x w p hp] at hq'
        exact le_of_add_le_add_right hq'
      · rintro ⟨hp, hm⟩
        refine ⟨hp, fun q hq ↦ ?_⟩
        change heldSkew x w q ≤ heldSkew x w p
        rw [heldSkew_eq x w q hq, heldSkew_eq x w p hp]
        have hh : LocationScale.logLik (loss w) x q ≤ LocationScale.logLik (loss w) x p := hm hq
        linarith
    obtain ⟨p, hp, hu⟩ := LocationScale.existsUnique_mle hce hl hx
    exact ⟨p, (he p).mpr hp, fun q hq ↦ hu q ((he q).mp hq)⟩
  · rintro ⟨p, _, hm⟩
    by_contra hx
    let i : Fin n := ⟨0, hn⟩
    have hc : ∀ j, x j = x i := fun j ↦ not_ne_iff.mp (fun hj ↦ hx ⟨j, i, hj⟩)
    obtain ⟨u, hu, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      ((heldSkew_collapse hn w hc).eventually (eventually_gt_atTop (heldSkew x w p)))).exists
    have hp : (x i, 1 / u) ∈ LocationScale.domain := ⟨mem_univ _, one_div_pos.mpr hu⟩
    have hh := hm hp
    change logLik x (x i) (w * (1 / u)) ((1 - w) * (1 / u)) ≤ heldSkew x w p at hh
    simp only [mul_one_div] at hh
    exact hgt.not_ge hh

/-- Squared residuals strictly to the left of the proposed mode. -/
def leftSquares (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  ∑ i, (max (c - x i) 0)^2

/-- Squared residuals strictly to the right of the proposed mode. -/
def rightSquares (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  ∑ i, (max (x i - c) 0)^2

private def cubeRoot (t : ℝ) : ℝ := t ^ (1 / 3 : ℝ)

private theorem cubeRoot_cube {t : ℝ} (ht : 0 ≤ t) : cubeRoot t ^ 3 = t := by
  rw [cubeRoot, ← rpow_natCast, ← rpow_mul ht]
  norm_num

private theorem cubeRoot_nonneg {t : ℝ} (ht : 0 ≤ t) : 0 ≤ cubeRoot t :=
  rpow_nonneg ht _

/-- The all-free criterion obtained by profiling both side scales. -/
def modeProfile (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  ((leftSquares x c) ^ (1 / 3 : ℝ) + (rightSquares x c) ^ (1 / 3 : ℝ))^3

private theorem splitCost_bound {l r w : ℝ} (hl : 0 ≤ l) (hr : 0 ≤ r)
    (hsum : 0 < l + r) (hw : w ∈ Ioo (0 : ℝ) 1) :
    (l + r)^3 ≤ l^3 / w^2 + r^3 / (1 - w)^2 ∧
    ((l + r)^3 = l^3 / w^2 + r^3 / (1 - w)^2 ↔ w = l / (l + r)) := by
  have hw' : 0 < 1 - w := sub_pos.mpr hw.2
  let D := w^2 * (1 - w)^2
  have hD : 0 < D := mul_pos (pow_pos hw.1 _) (pow_pos hw' _)
  let P := l * (1 - w) * (1 + w) + r * w * (2 - w)
  have hp : 0 < P := by
    have h₁ : 0 < (1 - w) * (1 + w) := mul_pos hw' (by linarith [hw.1])
    have h₂ : 0 < w * (2 - w) := mul_pos hw.1 (by linarith [hw.2])
    have hmin := lt_min h₁ h₂
    have hh := mul_pos hmin hsum
    have ha := mul_le_mul_of_nonneg_left (min_le_left ((1-w)*(1+w)) (w*(2-w))) hl
    have hb := mul_le_mul_of_nonneg_left (min_le_right ((1-w)*(1+w)) (w*(2-w))) hr
    unfold P
    nlinarith only [hh, ha, hb]
  have he :
      l^3 / w^2 + r^3 / (1 - w)^2 - (l+r)^3 =
        (l*(1-w)-r*w)^2 * P / D := by
    unfold D P
    field_simp [hw.1.ne', hw'.ne']
    ring
  have hnonneg : 0 ≤ (l*(1-w)-r*w)^2 * P / D := by positivity
  refine ⟨by linarith, ?_⟩
  rw [show ((l + r)^3 = l^3 / w^2 + r^3 / (1 - w)^2) ↔
    l^3 / w^2 + r^3 / (1 - w)^2 - (l + r)^3 = 0 from eq_comm.trans sub_eq_zero.symm,
    he, div_eq_zero_iff, mul_eq_zero, or_iff_left hD.ne',
    or_iff_left hp.ne', pow_eq_zero_iff (by decide : (2 : ℕ) ≠ 0)]
  rw [eq_div_iff hsum.ne']
  constructor <;> intro h <;> nlinarith only [h]

private def widthValue (k : ℝ) (n : ℕ) : ℝ :=
  n * log (halfMean / sqrt (k / n)) - n / 2

private theorem widthValue_strictAnti (hn : (0 : ℝ) < n) {k₁ k₂ : ℝ}
    (hk₁ : 0 < k₁) (hk : k₁ < k₂) : widthValue k₂ n < widthValue k₁ n := by
  have h₁ : 0 < sqrt (k₁ / n) := sqrt_pos.2 (div_pos hk₁ hn)
  have h₂ : 0 < sqrt (k₂ / n) := sqrt_pos.2 (div_pos (hk₁.trans hk) hn)
  have hs := sqrt_lt_sqrt (div_nonneg hk₁.le hn.le) (div_lt_div_of_pos_right hk hn)
  have hq : halfMean / sqrt (k₂ / n) < halfMean / sqrt (k₁ / n) :=
    div_lt_div_of_pos_left halfMean_pos h₁ hs
  have hl := log_lt_log (div_pos halfMean_pos h₂) hq
  unfold widthValue
  exact sub_lt_sub_right (mul_lt_mul_of_pos_left hl hn) _

private theorem width_max (hn : (0 : ℝ) < n) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    n * log (halfMean / s) - k / (2 * s^2) ≤ widthValue k n ∧
    (n * log (halfMean / s) - k / (2 * s^2) = widthValue k n ↔ s = sqrt (k / n)) := by
  let b := sqrt (k / n)
  have hb : 0 < b := sqrt_pos.2 (div_pos hk hn)
  have hbb : b^2 = k / n := sq_sqrt (div_nonneg hk.le hn.le)
  have he : k / (2 * s^2) = n / 2 * (b / s)^2 := by
    rw [div_pow, hbb]
    field_simp
  have hl : 0 < (b / s)^2 := pow_pos (div_pos hb hs) _
  have hlog : log ((b / s)^2) = 2 * (log (halfMean / s) - log (halfMean / b)) := by
    rw [log_pow, log_div hb.ne' hs.ne', log_div halfMean_pos.ne' hs.ne',
      log_div halfMean_pos.ne' hb.ne']
    norm_num
  have hineq := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hl)
    (show 0 ≤ (n : ℝ) / 2 by positivity)
  rw [hlog] at hineq
  change _ ≤ n * log (halfMean / b) - n / 2 ∧
    (_ = n * log (halfMean / b) - n / 2 ↔ s = b)
  refine ⟨by nlinarith only [hineq, he], ?_⟩
  constructor
  · intro h
    by_contra hne
    have hsq : (b / s)^2 ≠ 1 := by
      intro hsq
      have heq : b / s = 1 := by
        have hp := div_pos hb hs
        nlinarith only [hsq, mul_pos hp (show 0 < b / s + 1 by positivity)]
      exact hne (by simpa using ((div_eq_iff hs.ne').mp heq).symm)
    have hstrict := mul_lt_mul_of_pos_left (log_lt_sub_one_of_pos hl hsq)
      (show 0 < (n : ℝ) / 2 by positivity)
    rw [hlog] at hstrict
    nlinarith only [hstrict, he, h]
  · intro h
    rw [h] at he ⊢
    rw [div_self hb.ne', one_pow, mul_one] at he
    rw [he]

/-- Optimal left width fraction in the all-free profile, including endpoint modes. -/
def modeFraction (x : Fin n → ℝ) (c : ℝ) : ℝ :=
  leftSquares x c ^ (1 / 3 : ℝ) /
    (leftSquares x c ^ (1 / 3 : ℝ) + rightSquares x c ^ (1 / 3 : ℝ))

private theorem leftSquares_nonneg (x : Fin n → ℝ) (c : ℝ) :
    0 ≤ leftSquares x c := Finset.sum_nonneg (fun _ _ ↦ sq_nonneg _)

private theorem rightSquares_nonneg (x : Fin n → ℝ) (c : ℝ) :
    0 ≤ rightSquares x c := Finset.sum_nonneg (fun _ _ ↦ sq_nonneg _)

private theorem leftSquares_pos {x : Fin n → ℝ} {c : ℝ} :
    0 < leftSquares x c ↔ ∃ i, x i < c := by
  constructor
  · intro h
    by_contra! hno
    have he : leftSquares x c = 0 := by
      unfold leftSquares
      exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (hno i))]; norm_num)
    linarith
  · rintro ⟨i, hi⟩
    have h := Finset.single_le_sum (f := fun j ↦ (max (c - x j) 0)^2)
      (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i)
    rw [max_eq_left (sub_pos.mpr hi).le] at h
    exact (sq_pos_of_ne_zero (sub_pos.mpr hi).ne').trans_le h

private theorem rightSquares_pos {x : Fin n → ℝ} {c : ℝ} :
    0 < rightSquares x c ↔ ∃ i, c < x i := by
  constructor
  · intro h
    by_contra! hno
    have he : rightSquares x c = 0 := by
      unfold rightSquares
      exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (hno i))]; norm_num)
    linarith
  · rintro ⟨i, hi⟩
    have h := Finset.single_le_sum (f := fun j ↦ (max (x j - c) 0)^2)
      (fun _ _ ↦ sq_nonneg _) (Finset.mem_univ i)
    rw [max_eq_left (sub_pos.mpr hi).le] at h
    exact (sq_pos_of_ne_zero (sub_pos.mpr hi).ne').trans_le h

private theorem cubeSum_pos {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) (c : ℝ) :
    0 < cubeRoot (leftSquares x c) + cubeRoot (rightSquares x c) := by
  obtain ⟨i, j, hij⟩ := hx
  have hA := leftSquares_nonneg x c
  have hB := rightSquares_nonneg x c
  by_cases h : ∃ k, x k < c
  · exact add_pos_of_pos_of_nonneg (rpow_pos_of_pos (leftSquares_pos.mpr h) _) (cubeRoot_nonneg hB)
  · push Not at h
    have hh : ∃ k, c < x k := by
      by_contra! hh
      exact hij ((le_antisymm (hh i) (h i)).trans (le_antisymm (hh j) (h j)).symm)
    exact add_pos_of_nonneg_of_pos (cubeRoot_nonneg hA) (rpow_pos_of_pos (rightSquares_pos.mpr hh) _)

private def splitCost (x : Fin n → ℝ) (c w : ℝ) : ℝ :=
  leftSquares x c / w^2 + rightSquares x c / (1 - w)^2

private theorem heldSkew_cost (x : Fin n → ℝ) (c w s : ℝ) :
    heldSkew x w (c, s) = n * log (halfMean / s) - splitCost x c w / (2 * s^2) := by
  have hwidth : w*s+(1-w)*s=s := by ring
  have hterm (i : Fin n) :
      (x i - c)^2 / (2 * (if x i < c then w * s else (1 - w) * s)^2) =
        (max (c-x i) 0)^2 / (2 * (w*s)^2) +
        (max (x i-c) 0)^2 / (2 * ((1-w)*s)^2) := by
    by_cases h : x i < c
    · rw [ite_eq_left h, max_eq_left (sub_pos.mpr h).le,
        max_eq_right (sub_nonpos.mpr h.le), zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, add_zero]
      rw [show c - x i = -(x i - c) by ring, neg_sq]
    · have h' : c ≤ x i := le_of_not_gt h
      rw [ite_eq_right h, max_eq_right (sub_nonpos.mpr h'), max_eq_left (sub_nonneg.mpr h'),
        zero_pow (by decide : (2 : ℕ) ≠ 0), zero_div, zero_add]
  unfold heldSkew logLik
  simp only [hwidth, hterm, Finset.sum_add_distrib, ← Finset.sum_div, splitCost, leftSquares, rightSquares]
  rw [add_div, div_div, div_div, mul_pow, mul_pow]
  ring

private theorem profiled_bound (hn : (0 : ℝ) < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {c w s : ℝ}
    (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    heldSkew x w (c, s) ≤ widthValue (modeProfile x c) n ∧
      (heldSkew x w (c, s) = widthValue (modeProfile x c) n ↔
        w = modeFraction x c ∧ s = sqrt (modeProfile x c / n)) := by
  have h₁ := cubeRoot_nonneg (leftSquares_nonneg x c)
  have h₂ := cubeRoot_nonneg (rightSquares_nonneg x c)
  have hsum := cubeSum_pos hx c
  have hc := splitCost_bound h₁ h₂ hsum hw
  rw [cubeRoot_cube (leftSquares_nonneg x c), cubeRoot_cube (rightSquares_nonneg x c)] at hc
  change modeProfile x c ≤ splitCost x c w ∧
    (modeProfile x c = splitCost x c w ↔ w = modeFraction x c) at hc
  have hP : 0 < modeProfile x c := pow_pos hsum 3
  have hW := width_max hn (hP.trans_le hc.1) hs
  rw [← heldSkew_cost] at hW
  have hv : widthValue (splitCost x c w) n ≤ widthValue (modeProfile x c) n := by
    rcases hc.1.lt_or_eq with hl | he
    · exact (widthValue_strictAnti hn hP hl).le
    · rw [he]
  refine ⟨hW.1.trans hv, ?_⟩
  constructor
  · intro he
    have hE : modeProfile x c = splitCost x c w := by
      rcases hc.1.lt_or_eq with hl | he'
      · exact False.elim ((widthValue_strictAnti hn hP hl).not_ge (he ▸ hW.1))
      · exact he'
    have hS : s = sqrt (splitCost x c w / n) := hW.2.mp (by simpa only [← hE] using he)
    exact ⟨hc.2.mp hE, hS.trans (by rw [hE])⟩
  · rintro ⟨hw', hs'⟩
    have hE := hc.2.mpr hw'
    simpa only [← hE] using hW.2.mpr (by simpa only [← hE] using hs')

private theorem limit_at_modeFraction (hn : (0 : ℝ) < n)
    {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) (c : ℝ) :
    Tendsto (fun w ↦ heldSkew x w (c, sqrt (modeProfile x c / n)))
      (𝓝[Ioo 0 1] (modeFraction x c)) (𝓝 (widthValue (modeProfile x c) n)) := by
  let A := leftSquares x c
  let B := rightSquares x c
  let l := cubeRoot A
  let r := cubeRoot B
  let t := modeFraction x c
  let Q := modeProfile x c
  let s := sqrt (Q / n)
  have hA := leftSquares_nonneg x c
  have hB := rightSquares_nonneg x c
  have hl : 0 ≤ l := cubeRoot_nonneg hA
  have hr : 0 ≤ r := cubeRoot_nonneg hB
  have hl3 : l^3 = A := cubeRoot_cube hA
  have hr3 : r^3 = B := cubeRoot_cube hB
  have hsum : 0 < l+r := cubeSum_pos hx c
  have ht : t = l / (l+r) := rfl
  have ht' : 1 - t = r / (l+r) := by rw [ht]; field_simp; ring
  have he : splitCost x c t = Q := by
    change A / t^2 + B / (1-t)^2 = (l+r)^3
    rw [← hl3, ← hr3, ht', ht, div_pow, div_pow]
    by_cases h₁ : l = 0
    · have h₂ : r ≠ 0 := by linarith
      simp only [h₁, zero_pow (by decide : (3 : ℕ) ≠ 0), zero_div, zero_add]
      field_simp
    · by_cases h₂ : r = 0
      · simp only [h₂, zero_pow (by decide : (3 : ℕ) ≠ 0), zero_div, add_zero]
        field_simp
      · field_simp
  have hleft : ContinuousAt (fun w : ℝ ↦ A / w^2) t := by
    by_cases ha : A = 0
    · simpa only [ha, zero_div] using (continuousAt_const (y := (0 : ℝ)) (x := t))
    · have hpos : 0 < l := lt_of_le_of_ne hl (by intro h; exact ha (by rw [← hl3, ← h, zero_pow (by decide : (3 : ℕ) ≠ 0)]))
      exact continuousAt_const.div (continuousAt_id.pow 2)
        (pow_ne_zero 2 (ht.symm ▸ (div_pos hpos hsum).ne'))
  have hright : ContinuousAt (fun w : ℝ ↦ B / (1-w)^2) t := by
    by_cases hb : B = 0
    · simpa only [hb, zero_div] using (continuousAt_const (y := (0 : ℝ)) (x := t))
    · have hpos : 0 < r := lt_of_le_of_ne hr (by intro h; exact hb (by rw [← hr3, ← h, zero_pow (by decide : (3 : ℕ) ≠ 0)]))
      exact continuousAt_const.div ((continuousAt_const.sub continuousAt_id).pow 2)
        (pow_ne_zero 2 (ht'.symm ▸ (div_pos hpos hsum).ne'))
  have hC : ContinuousAt (splitCost x c) t := hleft.add hright
  have hL : ContinuousAt (fun w ↦ heldSkew x w (c, s)) t := by
    simp only [heldSkew_cost]
    exact continuousAt_const.sub (hC.div_const _)
  have hval : heldSkew x t (c, s) = widthValue Q n := by
    rw [heldSkew_cost, he]
    have hP : 0 < Q := pow_pos hsum _
    have hs : 0 < s := sqrt_pos.2 (div_pos hP hn)
    exact (width_max hn hP hs).2.mpr rfl
  rw [← hval]
  exact hL.continuousWithinAt

private theorem modeFraction_mem_Icc {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (c : ℝ) : modeFraction x c ∈ Icc 0 1 := by
  have hsum := cubeSum_pos hx c
  have hL := cubeRoot_nonneg (leftSquares_nonneg x c)
  have hR := cubeRoot_nonneg (rightSquares_nonneg x c)
  refine ⟨div_nonneg hL hsum.le, ?_⟩
  apply (div_le_iff₀ hsum).2
  change cubeRoot (leftSquares x c) ≤ 1 * (cubeRoot (leftSquares x c) + cubeRoot (rightSquares x c))
  linarith

/-- The all-free finite maximizing graph over mode, left width fraction
and total width. This describes every tie for a nonconstant sample. -/
theorem allFree_graph (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    {c w s : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) (hs : 0 < s) :
    (∀ (d v t : ℝ), v ∈ Ioo (0 : ℝ) 1 → 0 < t →
      heldSkew x v (d, t) ≤ heldSkew x w (c, s)) ↔
      IsMinOn (modeProfile x) univ c ∧
        w = modeFraction x c ∧ s = sqrt (modeProfile x c / n) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hcond := profiled_bound hN hx (c := c) hw hs
  constructor
  · intro hmax
    have hup (d : ℝ) : widthValue (modeProfile x d) n ≤ heldSkew x w (c, s) := by
      have hcl : modeFraction x d ∈ closure (Ioo (0 : ℝ) 1) := by
        rw [closure_Ioo (by norm_num : (0 : ℝ) ≠ 1)]
        exact modeFraction_mem_Icc hx d
      have : (𝓝[Ioo (0 : ℝ) 1] (modeFraction x d)).NeBot :=
        mem_closure_iff_nhdsWithin_neBot.mp hcl
      have ht : 0 < sqrt (modeProfile x d / n) :=
        sqrt_pos.2 (div_pos (pow_pos (cubeSum_pos hx d) _) hN)
      have hmem : ∀ᶠ v in 𝓝[Ioo (0 : ℝ) 1] (modeFraction x d), v ∈ Ioo (0 : ℝ) 1 :=
        self_mem_nhdsWithin
      exact le_of_tendsto (limit_at_modeFraction hN hx d)
        (hmem.mono (fun v hv ↦ hmax d v _ hv ht))
    have hEq := le_antisymm hcond.1 (hup c)
    refine ⟨fun d _ ↦ ?_, hcond.2.mp hEq⟩
    change modeProfile x c ≤ modeProfile x d
    by_contra! hcd
    exact (widthValue_strictAnti hN (pow_pos (cubeSum_pos hx d) _) hcd).not_ge
      ((hup d).trans hcond.1)
  · rintro ⟨hm, he⟩ d v t hv ht
    rw [hcond.2.mpr he]
    have hb := (profiled_bound hN hx hv ht (c := d)).1
    apply hb.trans
    rcases (show modeProfile x c ≤ modeProfile x d from hm (mem_univ d)).lt_or_eq with h | h
    · exact (widthValue_strictAnti hN (pow_pos (cubeSum_pos hx c) _) h).le
    · rw [h]

private theorem modeFraction_mem_Ioo {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (c : ℝ) :
    modeFraction x c ∈ Ioo 0 1 ↔ (∃ i, x i < c) ∧ (∃ j, c < x j) := by
  let l := cubeRoot (leftSquares x c)
  let r := cubeRoot (rightSquares x c)
  have hsum : 0 < l+r := cubeSum_pos hx c
  have hA := leftSquares_nonneg x c
  have hB := rightSquares_nonneg x c
  have hl : 0 < l ↔ 0 < leftSquares x c :=
    ⟨fun h ↦ (cubeRoot_cube hA) ▸ pow_pos h 3, fun h ↦ rpow_pos_of_pos h _⟩
  have hr : 0 < r ↔ 0 < rightSquares x c :=
    ⟨fun h ↦ (cubeRoot_cube hB) ▸ pow_pos h 3, fun h ↦ rpow_pos_of_pos h _⟩
  change (0 < l/(l+r) ∧ l/(l+r) < 1) ↔ _
  rw [div_lt_iff₀ hsum, one_mul, lt_add_iff_pos_right, hr, rightSquares_pos, ← leftSquares_pos, ← hl]
  exact and_congr_left' ⟨fun h ↦ (lt_div_iff₀ hsum).mp h |>.trans_eq' (zero_mul _),
    fun h ↦ div_pos h hsum⟩

private theorem modeProfile_continuous (x : Fin n → ℝ) : Continuous (modeProfile x) := by
  have hA : Continuous (leftSquares x) := by unfold leftSquares; fun_prop
  have hB : Continuous (rightSquares x) := by unfold rightSquares; fun_prop
  unfold modeProfile
  exact
    (((continuous_rpow_const (q := (1 / 3 : ℝ))).comp hA).add
      ((continuous_rpow_const (q := (1 / 3 : ℝ))).comp hB)).pow 3

private theorem modeProfile_le_of_side_le {x : Fin n → ℝ} {c d : ℝ}
    (hA : leftSquares x c ≤ leftSquares x d)
    (hB : rightSquares x c ≤ rightSquares x d) :
    modeProfile x c ≤ modeProfile x d := by
  unfold modeProfile
  have h₁ := rpow_le_rpow (leftSquares_nonneg x c) hA (by norm_num : (0 : ℝ) ≤ 1 / 3)
  have h₂ := rpow_le_rpow (rightSquares_nonneg x c) hB (by norm_num : (0 : ℝ) ≤ 1 / 3)
  exact pow_le_pow_left₀
    (add_nonneg (cubeRoot_nonneg (leftSquares_nonneg x c))
      (cubeRoot_nonneg (rightSquares_nonneg x c))) (add_le_add h₁ h₂) 3

private theorem modeProfile_extreme_lower {x : Fin n → ℝ} {a b c : ℝ}
    (ha : ∀ i, a ≤ x i) (hb : ∀ i, x i ≤ b) :
    (c ≤ a → modeProfile x a ≤ modeProfile x c) ∧
      (b ≤ c → modeProfile x b ≤ modeProfile x c) := by
  constructor
  · intro hc
    apply modeProfile_le_of_side_le
    · have hz : leftSquares x a = 0 := by
        unfold leftSquares
        exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (ha i))]; norm_num)
      rw [hz]
      exact leftSquares_nonneg _ _
    · apply Finset.sum_le_sum
      intro i _
      exact pow_le_pow_left₀ (le_max_right _ _)
        (max_le_max (sub_le_sub_left hc _) le_rfl) 2
  · intro hc
    apply modeProfile_le_of_side_le
    · apply Finset.sum_le_sum
      intro i _
      exact pow_le_pow_left₀ (le_max_right _ _)
        (max_le_max (sub_le_sub_right hc _) le_rfl) 2
    · have hz : rightSquares x b = 0 := by
        unfold rightSquares
        exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (hb i))]; norm_num)
      rw [hz]
      exact rightSquares_nonneg _ _

private theorem modeProfile_minimum {x : Fin n → ℝ} {a b : ℝ}
    (hab : a ≤ b) (ha : ∀ i, a ≤ x i) (hb : ∀ i, x i ≤ b) :
    ∃ c ∈ Icc a b, IsMinOn (modeProfile x) univ c := by
  obtain ⟨c, hc, hm⟩ := isCompact_Icc.exists_isMinOn ⟨a, le_rfl, hab⟩
    (modeProfile_continuous x).continuousOn
  refine ⟨c, hc, fun d _ ↦ ?_⟩
  by_cases had : a ≤ d
  · by_cases hdb : d ≤ b
    · exact hm (show d ∈ Icc a b from ⟨had, hdb⟩)
    · exact (hm (show b ∈ Icc a b from ⟨hab, le_rfl⟩)).trans
        ((modeProfile_extreme_lower ha hb).2 (le_of_not_ge hdb))
  · exact (hm (show a ∈ Icc a b from ⟨le_rfl, hab⟩)).trans
      ((modeProfile_extreme_lower ha hb).1 (le_of_not_ge had))

/-- A bracketed mode is a mode strictly between the sample extremes.
The all-free fit exists exactly when there is a bracketed minimizing
mode. This condition also excludes every constant sample. -/
theorem allFree_exists_iff (hn : 0 < n) (x : Fin n → ℝ) :
    (∃ c w s : ℝ, w ∈ Ioo 0 1 ∧ 0 < s ∧
      ∀ (d v t : ℝ), v ∈ Ioo (0 : ℝ) 1 → 0 < t →
        heldSkew x v (d, t) ≤ heldSkew x w (c, s)) ↔
    ∃ c : ℝ, (∃ i, x i < c) ∧ (∃ j, c < x j) ∧ IsMinOn (modeProfile x) univ c := by
  constructor
  · rintro ⟨c, w, s, hw, hs, hm⟩
    have hx : LocationScale.Nonconstant x := by
      by_contra hnc
      let i : Fin n := ⟨0, hn⟩
      have hc : ∀ j, x j = x i := fun j ↦ not_ne_iff.mp (fun hj ↦ hnc ⟨j, i, hj⟩)
      obtain ⟨u, hu, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
        ((heldSkew_collapse hn w hc).eventually
          (eventually_gt_atTop (heldSkew x w (c, s))))).exists
      have hb := hm (x i) w (1 / u) hw (one_div_pos.mpr hu)
      change logLik x (x i) (w * (1 / u)) ((1 - w) * (1 / u)) ≤ _ at hb
      simp only [mul_one_div] at hb
      exact hgt.not_ge hb
    obtain ⟨hmin, he, _⟩ := (allFree_graph hn hx hw hs).mp hm
    have hside := (modeFraction_mem_Ioo hx c).mp (he ▸ hw)
    exact ⟨c, hside.1, hside.2, hmin⟩
  · rintro ⟨c, ⟨i, hi⟩, ⟨j, hj⟩, hmin⟩
    have hx : LocationScale.Nonconstant x := ⟨i, j, (hi.trans hj).ne⟩
    have hw := (modeFraction_mem_Ioo hx c).mpr ⟨⟨i, hi⟩, ⟨j, hj⟩⟩
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    have hs := sqrt_pos.2 (div_pos (pow_pos (cubeSum_pos hx c) 3) hN)
    refine ⟨c, modeFraction x c, sqrt (modeProfile x c / n), hw, hs, ?_⟩
    exact (allFree_graph hn hx hw hs).mpr ⟨hmin, rfl, rfl⟩

/-- All values of the log likelihood at positive side scales. -/
def allFreeValues (x : Fin n → ℝ) : Set ℝ :=
  {L | ∃ c w s : ℝ, w ∈ Ioo 0 1 ∧ 0 < s ∧ L = heldSkew x w (c,s)}

/-- Total squared residual at the endpoint of a half-normal. -/
def endpointSquares (x : Fin n → ℝ) (c : ℝ) : ℝ := ∑ i, (x i - c)^2

/-- Log likelihood of a half-normal at a supporting endpoint. -/
def halfLogLik (x : Fin n → ℝ) (c b : ℝ) : ℝ :=
  n * log (halfMean / b) - endpointSquares x c / (2 * b^2)

private theorem allFreeValues_upper_iff (hn : (0 : ℝ) < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) {L : ℝ} :
    L ∈ upperBounds (allFreeValues x) ↔ ∀ c, widthValue (modeProfile x c) n ≤ L := by
  constructor
  · intro h c
    have hcl : modeFraction x c ∈ closure (Ioo (0 : ℝ) 1) := by
      rw [closure_Ioo (by norm_num : (0 : ℝ) ≠ 1)]
      exact modeFraction_mem_Icc hx c
    have : (𝓝[Ioo (0 : ℝ) 1] (modeFraction x c)).NeBot :=
      mem_closure_iff_nhdsWithin_neBot.mp hcl
    have hs : 0 < sqrt (modeProfile x c / n) :=
      sqrt_pos.2 (div_pos (pow_pos (cubeSum_pos hx c) _) hn)
    have hmem : ∀ᶠ w in 𝓝[Ioo (0 : ℝ) 1] (modeFraction x c), w ∈ Ioo (0 : ℝ) 1 :=
      self_mem_nhdsWithin
    exact le_of_tendsto (limit_at_modeFraction hn hx c) (hmem.mono fun w hw ↦
      h ⟨c, w, _, hw, hs, rfl⟩)
  · rintro h _ ⟨c, w, s, hw, hs, rfl⟩
    exact (profiled_bound hn hx (c := c) hw hs).1.trans (h c)

private theorem halfFaceProfile {x : Fin n → ℝ} {c : ℝ} (hx : LocationScale.Nonconstant x)
    (left : Bool) (hsupp : if left then ∀ j, x j ≤ c else ∀ j, c ≤ x j) :
    modeProfile x c = endpointSquares x c ∧ modeFraction x c = if left then 1 else 0 := by
  have hN := (cubeSum_pos hx c).ne'
  cases left
  · have hs : ∀ j, c ≤ x j := hsupp
    have hA : leftSquares x c = 0 := by
      unfold leftSquares
      exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (hs i))]; norm_num)
    have hB : rightSquares x c = endpointSquares x c := by
      unfold rightSquares endpointSquares
      exact Finset.sum_congr rfl (fun i _ ↦ by rw [max_eq_left (sub_nonneg.mpr (hs i))])
    have h3 := cubeRoot_cube (rightSquares_nonneg x c)
    constructor
    · simpa [modeProfile, cubeRoot, hA, hB] using h3
    · simp [modeFraction, hA]
  · have hs : ∀ j, x j ≤ c := hsupp
    have hB : rightSquares x c = 0 := by
      unfold rightSquares
      exact Finset.sum_eq_zero (fun i _ ↦ by rw [max_eq_right (sub_nonpos.mpr (hs i))]; norm_num)
    have hA : leftSquares x c = endpointSquares x c := by
      unfold leftSquares endpointSquares
      apply Finset.sum_congr rfl
      intro i _
      rw [max_eq_left (sub_nonneg.mpr (hs i)), show c-x i = -(x i-c) by ring, neg_sq]
    have h3 := cubeRoot_cube (leftSquares_nonneg x c)
    constructor
    · simpa [modeProfile, cubeRoot, hA, hB] using h3
    · have hnz : leftSquares x c ^ (1 / 3 : ℝ) ≠ 0 := by
        simpa [cubeRoot, hB] using hN
      simpa [modeFraction, hB] using (div_self hnz)

/-- A half-normal path at any supporting endpoint has its unpenalized
trace. Its value is the all-free supremum exactly when that endpoint
minimizes the mode profile. The endpoint RMS fixes its positive scale. -/
theorem allFree_halfFace (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) (c : ℝ) (left : Bool)
    (hsupp : if left then ∀ j, x j ≤ c else ∀ j, c ≤ x j) :
    let b := sqrt (endpointSquares x c / n)
    Tendsto (fun w ↦ heldSkew x w (c,b)) (𝓝[Ioo 0 1] (if left then 1 else 0))
      (𝓝 (halfLogLik x c b)) ∧
    (IsLUB (allFreeValues x) (halfLogLik x c b) ↔ IsMinOn (modeProfile x) univ c) := by
  intro b
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  obtain ⟨hQ, hw⟩ := halfFaceProfile hx left hsupp
  have hpos : 0 < modeProfile x c := pow_pos (cubeSum_pos hx c) _
  have he : halfLogLik x c b = widthValue (modeProfile x c) n := by
    unfold halfLogLik
    have hb : 0 < b := sqrt_pos.2 (div_pos (hQ ▸ hpos) hN)
    rw [← hQ]
    exact (width_max hN hpos hb).2.mpr (by simp only [b, hQ])
  refine ⟨?_, ?_⟩
  · rw [he, ← hw]
    simpa only [b, hQ] using (limit_at_modeFraction hN hx c)
  · rw [he]
    have hup := @allFreeValues_upper_iff n hN x hx
    constructor
    · intro h d _
      have hh := (hup.mp h.1) d
      change modeProfile x c ≤ modeProfile x d
      by_contra! hne
      exact (widthValue_strictAnti hN (pow_pos (cubeSum_pos hx d) _) hne).not_ge hh
    · intro hm
      refine ⟨hup.mpr ?_, fun _ hL ↦ (hup.mp hL) c⟩
      intro d
      rcases (show modeProfile x c ≤ modeProfile x d from hm (mem_univ d)).lt_or_eq with h | h
      · exact (widthValue_strictAnti hN hpos h).le
      · rw [h]

/-- Without an interior minimizing mode, the all-free supremum is a
half-normal trace at a sample extreme. Both orientations are allowed. -/
theorem allFree_endpoint_only (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x)
    (hmiss : ¬ ∃ c : ℝ, (∃ i, x i < c) ∧ (∃ j, c < x j) ∧ IsMinOn (modeProfile x) univ c) :
    ∃ (i : Fin n) (left : Bool),
      (if left then ∀ j, x j ≤ x i else ∀ j, x i ≤ x j) ∧
      IsLUB (allFreeValues x)
        (halfLogLik x (x i) (sqrt (endpointSquares x (x i) / n))) := by
  let j : Fin n := ⟨0, hn⟩
  obtain ⟨lo, _, hlo⟩ := Finset.exists_min_image Finset.univ x ⟨j, Finset.mem_univ _⟩
  obtain ⟨hi, _, hhi⟩ := Finset.exists_max_image Finset.univ x ⟨j, Finset.mem_univ _⟩
  have hl (i : Fin n) : x lo ≤ x i := hlo i (Finset.mem_univ i)
  have hh (i : Fin n) : x i ≤ x hi := hhi i (Finset.mem_univ i)
  obtain ⟨c, hc, hm⟩ := modeProfile_minimum (hl hi) hl hh
  by_cases h : c = x lo
  · exact ⟨lo, false, hl, (allFree_halfFace hn hx _ false hl).2.mpr (h ▸ hm)⟩
  · by_cases h' : c = x hi
    · exact ⟨hi, true, hh, (allFree_halfFace hn hx _ true hh).2.mpr (h' ▸ hm)⟩
    · exact False.elim (hmiss ⟨c, ⟨lo, lt_of_le_of_ne hc.1 (Ne.symm h)⟩,
        ⟨hi, lt_of_le_of_ne hc.2 h'⟩, hm⟩)

/-- One copy of each of `±2` and ten copies of each of `±1`, in order. -/
def reflectedSample (i : Fin 22) : ℝ :=
  if i.val = 0 then -2 else if i.val < 11 then -1 else if i.val < 21 then 1 else 2

private theorem reflectedSample_rev (i : Fin 22) :
    reflectedSample (Fin.revPerm i) = -reflectedSample i := by
  fin_cases i <;> norm_num [reflectedSample, Fin.revPerm, Fin.rev]

private theorem reflectedSample_squares (c : ℝ) :
    leftSquares reflectedSample (-c) = rightSquares reflectedSample c ∧
      rightSquares reflectedSample (-c) = leftSquares reflectedSample c := by
  constructor
  · unfold leftSquares rightSquares
    rw [← Equiv.sum_comp Fin.revPerm (fun i ↦ (max (-c - reflectedSample i) 0)^2)]
    apply Finset.sum_congr rfl
    intro i _
    rw [reflectedSample_rev, show -c - -reflectedSample i = reflectedSample i-c by ring]
  · unfold rightSquares leftSquares
    rw [← Equiv.sum_comp Fin.revPerm (fun i ↦ (max (reflectedSample i - -c) 0)^2)]
    apply Finset.sum_congr rfl
    intro i _
    rw [reflectedSample_rev, show -reflectedSample i - -c = c-reflectedSample i by ring]

/-- The exact symmetric sample has two distinct finite all-free
maximizers at reflected modes. The maximizing side scales are supplied
by `allFree_graph`. -/
theorem allFree_reflected_tie :
    ∃ c : ℝ, c ≠ 0 ∧
      modeFraction reflectedSample c ∈ Ioo 0 1 ∧
      modeFraction reflectedSample (-c) = 1 - modeFraction reflectedSample c ∧
      IsMinOn (modeProfile reflectedSample) univ c ∧
      IsMinOn (modeProfile reflectedSample) univ (-c) := by
  let x := reflectedSample
  have h0 : x 0 = -2 := by norm_num [x, reflectedSample]
  have h21 : x 21 = 2 := by norm_num [x, reflectedSample]
  have hnc : LocationScale.Nonconstant x := ⟨0, 21, by rw [h0, h21]; norm_num⟩
  have hrev (c : ℝ) : modeProfile x (-c) = modeProfile x c := by
    obtain ⟨ha, hb⟩ := reflectedSample_squares c
    unfold x modeProfile
    rw [ha, hb, add_comm]
  have hlo (i : Fin 22) : -2 ≤ x i := by
    unfold x reflectedSample
    split <;> norm_num
    split <;> norm_num
    split <;> norm_num
  have hhi (i : Fin 22) : x i ≤ 2 := by
    unfold x reflectedSample
    split <;> norm_num
    split <;> norm_num
    split <;> norm_num
  have hA0 : leftSquares x 0 = 14 := by
    norm_num [x, leftSquares, reflectedSample, Fin.sum_univ_succ, max_def]
  have hB0 : rightSquares x 0 = 14 := by
    norm_num [x, rightSquares, reflectedSample, Fin.sum_univ_succ, max_def]
  have hQ0 : modeProfile x 0 = 112 := by
    have hc := cubeRoot_cube (by norm_num : (0 : ℝ) ≤ 14)
    unfold modeProfile
    rw [hA0, hB0]
    change (cubeRoot 14 + cubeRoot 14)^3 = 112
    nlinarith only [hc]
  have hQlo : modeProfile x (-2) = 116 := by
    rw [(halfFaceProfile hnc false hlo).1]
    norm_num [x, endpointSquares, reflectedSample, Fin.sum_univ_succ]
  have hQhi : modeProfile x 2 = 116 := by
    rw [← hQlo, hrev]
  have hA : leftSquares x (1/2) = 115/4 := by
    norm_num [x, leftSquares, reflectedSample, Fin.sum_univ_succ, max_def]
  have hB : rightSquares x (1/2) = 19/4 := by
    norm_num [x, rightSquares, reflectedSample, Fin.sum_univ_succ, max_def]
  have hQ : modeProfile x (1/2) < 112 := by
    rw [modeProfile, hA, hB]
    change (cubeRoot (115/4) + cubeRoot (19/4))^3 < 112
    have hl : cubeRoot (115/4) < 31/10 := by
      apply (pow_lt_pow_iff_left₀ (cubeRoot_nonneg (by norm_num)) (by norm_num)
        (by decide : (3 : ℕ) ≠ 0)).mp
      rw [cubeRoot_cube (by norm_num)]
      norm_num
    have hr : cubeRoot (19/4) < 17/10 := by
      apply (pow_lt_pow_iff_left₀ (cubeRoot_nonneg (by norm_num)) (by norm_num)
        (by decide : (3 : ℕ) ≠ 0)).mp
      rw [cubeRoot_cube (by norm_num)]
      norm_num
    calc
      _ < (48/10 : ℝ)^3 :=
        (pow_lt_pow_iff_left₀
          (add_nonneg (cubeRoot_nonneg (by norm_num : (0 : ℝ) ≤ 115/4))
            (cubeRoot_nonneg (by norm_num : (0 : ℝ) ≤ 19/4)))
          (by norm_num) (by decide : (3 : ℕ) ≠ 0)).mpr (by linarith)
      _ < 112 := by norm_num
  obtain ⟨c, hc, hcmin⟩ := modeProfile_minimum (by norm_num : (-2 : ℝ) ≤ 2) hlo hhi
  have hcQ : modeProfile x c < 112 := (hcmin (mem_univ (1/2 : ℝ))).trans_lt hQ
  have hc0 : c ≠ 0 := by intro h; rw [h, hQ0] at hcQ; linarith
  have hclo : -2 < c := lt_of_le_of_ne hc.1 (by intro h; rw [← h, hQlo] at hcQ; linarith)
  have hchi : c < 2 := lt_of_le_of_ne hc.2 (by intro h; rw [h, hQhi] at hcQ; linarith)
  have hcf := (modeFraction_mem_Ioo hnc c).mpr
    ⟨⟨0, h0.symm ▸ hclo⟩, ⟨21, h21.symm ▸ hchi⟩⟩
  refine ⟨c, hc0, hcf, ?_, hcmin, ?_⟩
  · obtain ⟨ha, hb⟩ := reflectedSample_squares c
    have hsum := (cubeSum_pos hnc c).ne'
    unfold x at hsum
    unfold modeFraction
    rw [ha, hb, add_comm (rightSquares reflectedSample c ^ (1 / 3 : ℝ))]
    change cubeRoot (rightSquares reflectedSample c) /
      (cubeRoot (leftSquares reflectedSample c) + cubeRoot (rightSquares reflectedSample c)) =
      1 - cubeRoot (leftSquares reflectedSample c) /
        (cubeRoot (leftSquares reflectedSample c) + cubeRoot (rightSquares reflectedSample c))
    field_simp
    ring
  · intro d _
    change modeProfile x (-c) ≤ modeProfile x d
    rw [hrev]
    exact hcmin (mem_univ d)

/-- Variance divided by squared total width at left width fraction `w`. -/
def varianceFraction (w : ℝ) : ℝ :=
  (1 - halfMean^2) * (1 - 2*w)^2 + w * (1 - w)

private theorem halfVariance_gt : 1 / 4 < 1 - halfMean^2 := by
  have h : halfMean^2 = 2 / π := sq_sqrt (div_nonneg (by norm_num) pi_pos.le)
  rw [h]
  have h₁ : 2 / π < 3 / 4 := (div_lt_iff₀ pi_pos).mpr (by linarith [pi_gt_three])
  linarith

private theorem varianceFraction_pos {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    0 < varianceFraction w := by
  unfold varianceFraction
  exact add_pos_of_nonneg_of_pos
    (mul_nonneg (by linarith [halfVariance_gt]) (sq_nonneg _))
    (mul_pos hw.1 (sub_pos.mpr hw.2))

/-- With sd held, constant data give two equal half-normal traces and
every finite fit has strictly lower likelihood. -/
theorem heldSd_constant (hn : 0 < n) {x : Fin n → ℝ} {c σ : ℝ}
    (hσ : 0 < σ) (hx : ∀ i, x i = c) :
    let L := (n : ℝ) * log (halfMean * sqrt (1 - halfMean^2) / σ)
    (∀ d w : ℝ, w ∈ Ioo 0 1 →
      heldSkew x w (d, σ / sqrt (varianceFraction w)) < L) ∧
    ∀ left : Bool,
      Tendsto (fun w ↦ heldSkew x w (c, σ / sqrt (varianceFraction w)))
        (𝓝[Ioo 0 1] (if left then 1 else 0)) (𝓝 L) := by
  intro L
  let G := fun w : ℝ ↦ (n : ℝ) * log (halfMean * sqrt (varianceFraction w) / σ)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hL (w : ℝ) : heldSkew x w (c, σ / sqrt (varianceFraction w)) = G w := by
    rw [heldSkew_cost]
    simp only [splitCost, leftSquares, rightSquares, hx, sub_self, max_self,
      zero_pow (by decide : (2 : ℕ) ≠ 0), Finset.sum_const_zero, zero_div, add_zero, sub_zero]
    rw [div_div_eq_mul_div]
  refine ⟨?_, ?_⟩
  · intro d w hw
    have hpos := varianceFraction_pos hw
    have hs := sqrt_pos.2 hpos
    have hgap : varianceFraction w < 1 - halfMean^2 := by
      unfold varianceFraction
      have h := mul_neg_of_neg_of_pos (show 1-4*(1-halfMean^2) < 0 by linarith [halfVariance_gt])
        (mul_pos hw.1 (sub_pos.mpr hw.2))
      nlinarith only [h]
    have hlt := sqrt_lt_sqrt hpos.le hgap
    have hlog := log_lt_log (div_pos (mul_pos halfMean_pos hs) hσ)
      (div_lt_div_of_pos_right (mul_lt_mul_of_pos_left hlt halfMean_pos) hσ)
    have hG : G w < L := mul_lt_mul_of_pos_left hlog hN
    apply lt_of_le_of_lt _ hG
    rw [heldSkew_cost, div_div_eq_mul_div]
    have hc : 0 ≤ splitCost x d w :=
      add_nonneg (div_nonneg (leftSquares_nonneg _ _) (sq_nonneg _))
        (div_nonneg (rightSquares_nonneg _ _) (sq_nonneg _))
    exact sub_le_self _ (div_nonneg hc (by positivity))
  · intro left
    let w0 : ℝ := if left then 1 else 0
    have hv : varianceFraction w0 = 1 - halfMean^2 := by
      cases left <;> norm_num [w0, varianceFraction]
    have hcont : Continuous (fun w : ℝ ↦ halfMean * sqrt (varianceFraction w) / σ) := by
      unfold varianceFraction
      fun_prop
    have hden : halfMean * sqrt (varianceFraction w0) / σ ≠ 0 := by
      rw [hv]
      exact (div_pos (mul_pos halfMean_pos (sqrt_pos.2 (by linarith [halfVariance_gt]))) hσ).ne'
    have hG : ContinuousAt G w0 := continuousAt_const.mul (hcont.continuousAt.log hden)
    have he : G w0 = L := by simp only [G, L, hv]
    rw [← he]
    simp_rw [hL]
    exact hG.continuousWithinAt

/-- With both moments held, a datum at the limiting right-half endpoint
retains the exact Gaussian contact penalty as its standardized residual
approaches the endpoint from the narrow side. -/
theorem heldMeanSd_rightContact (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    let H := 1 - halfMean^2
    let z := μ - halfMean * (σ / sqrt H)
    Tendsto (fun w ↦
      let R := residual μ w (sqrt (varianceFraction w) / σ) z
      R^2 / (2 * (if R < 0 then w else 1 - w)^2))
      (𝓝[Ioo 0 1] 0) (𝓝 (halfMean^2 / (8 * H^2))) := by
  intro H z
  let q := sqrt H
  let a (w : ℝ) := sqrt (varianceFraction w)
  let R (w : ℝ) := residual μ w (a w / σ) z
  let G (w : ℝ) := halfMean * (-2 - (1 - 4*H) * (1-w) / (q * (a w + q)))
  have hH : 0 < H := by dsimp [H]; linarith [halfVariance_gt]
  have hq : 0 < q := sqrt_pos.2 hH
  have hq2 : q^2 = H := sq_sqrt hH.le
  have he (w : ℝ) (hw : w ∈ Ioo (0 : ℝ) 1) : R w / w = G w := by
    have ha : 0 < a w := sqrt_pos.2 (varianceFraction_pos hw)
    have ha2 : a w ^ 2 = varianceFraction w := sq_sqrt (varianceFraction_pos hw).le
    have hv : varianceFraction w = H + (1 - 4*H) * w * (1-w) := by
      unfold varianceFraction
      change _ * (1 - 2*w)^2 + _ = _
      ring
    have hd : (a w-q) * (a w+q) = (1-4*H) * w * (1-w) := by
      nlinarith only [ha2, hv, hq2]
    have hsum := add_pos ha hq
    have hratio : (a w-q) / (q*w) = (1-4*H)*(1-w) / (q*(a w+q)) := by
      field_simp [hq.ne', hw.1.ne', hsum.ne']
      nlinarith only [hd]
    have hR : R w / w = halfMean * (-2 - (a w-q)/(q*w)) := by
      change residual μ w (a w/σ) (μ - halfMean*(σ/q)) / w = _
      unfold residual
      field_simp [hσ.ne', hq.ne', hw.1.ne']
      ring
    rw [hR, hratio]
  have ha0 : a 0 = q := by unfold a q varianceFraction; norm_num [H]
  have hca : Continuous a := by unfold a varianceFraction; fun_prop
  have hden : q * (a 0 + q) ≠ 0 := by rw [ha0]; positivity
  have hc : ContinuousAt G 0 :=
    continuousAt_const.mul (continuousAt_const.sub
      ((continuousAt_const.mul (continuousAt_const.sub continuousAt_id)).div
        (continuousAt_const.mul (hca.continuousAt.add continuousAt_const)) hden))
  have hG0 : G 0 = -halfMean/(2*H) := by
    unfold G
    rw [ha0, sub_zero, mul_one]
    rw [show q * (q+q) = 2*H by nlinarith only [hq2]]
    field_simp [hH.ne']
    ring
  have hmem : ∀ᶠ w : ℝ in 𝓝[Ioo 0 1] 0, w ∈ Ioo (0 : ℝ) 1 := self_mem_nhdsWithin
  have hR : Tendsto (fun w ↦ R w / w) (𝓝[Ioo 0 1] 0) (𝓝 (-halfMean/(2*H))) := by
    rw [← hG0]
    have hG : Tendsto G (𝓝[Ioo 0 1] 0) (𝓝 (G 0)) := hc.continuousWithinAt
    exact hG.congr' (hmem.mono fun w hw ↦ (he w hw).symm)
  have hneg : -halfMean/(2*H) < 0 := div_neg_of_neg_of_pos (neg_lt_zero.mpr halfMean_pos) (by positivity)
  have hthin : ∀ᶠ w in 𝓝[Ioo 0 1] 0, R w < 0 := by
    filter_upwards [hmem, hR.eventually (gt_mem_nhds hneg)] with w hw hn
    exact (div_lt_iff₀ hw.1).mp hn |>.trans_eq (zero_mul _)
  have ht := (hR.pow 2).div_const 2
  have heval : (-halfMean/(2*H))^2 / 2 = halfMean^2 / (8 * H^2) := by
    rw [div_pow, neg_sq]
    ring
  rw [← heval]
  apply ht.congr'
  filter_upwards [hthin] with w hw
  change (R w / w)^2 / 2 = R w^2 / (2 * (if R w < 0 then w else 1-w)^2)
  rw [ite_eq_left hw, div_pow, div_div]
  ring

end

end UnivariateML.TwoPieceNormal

end
