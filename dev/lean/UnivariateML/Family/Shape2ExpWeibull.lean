module

public import UnivariateML.Family.ShapeFacesExpWeibull
import Mathlib.Analysis.Calculus.Deriv.Slope
import Mathlib.Analysis.Analytic.IsolatedZeros
import Mathlib.Analysis.Calculus.FDeriv.Analytic
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic

set_option linter.style.header false

/-!
Constant-sample concentration and the held-exponent attainment criterion
for exponentiated Weibull.

## Deviations

The free-scale constant path supplies the constant branches only. The nonconstant all-free
face comparison, the all-free tie, and held-shape uniqueness require additional results.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2ExpWeibull

open ShapeFacesExpWeibull

noncomputable section

variable {n : ℕ}

private theorem constant_value (n : ℕ) {x₀ b c : ℝ} (hx₀ : 0 < x₀) :
    logLik (fun _ : Fin n ↦ x₀) b x₀ c =
      (n : ℝ) * (log b + (-log x₀ - 1 + log c +
        (c - 1) * log (1 - exp (-1)))) := by
  simp [logLik, Weibull.logLik, Weibull.logDensity, hx₀.ne']
  ring

/-- At the tied observation as scale, increasing shape with any fixed
positive exponent gives unbounded log likelihood. -/
theorem constant_concentration (hn : 0 < n) {x₀ c : ℝ} (hx₀ : 0 < x₀)
    (_hc : 0 < c) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ x₀) b x₀ c) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  simp only [constant_value n hx₀]
  exact (tendsto_log_atTop.atTop_add tendsto_const_nhds).const_mul_atTop hn'

theorem held_exponent_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p : ℝ × ℝ, p ∈ Weibull.domain ∧
      IsMaxOn (fun q ↦ logLik x q.1 q.2 c) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, _, hp⟩
    by_contra h
    push Not at h
    let i₀ : Fin n := ⟨0, hn⟩
    have hconst : x = fun _ ↦ x i₀ := funext fun i ↦ h i i₀
    have ht := constant_concentration hn (hx i₀) hc
    have hbig := (tendsto_atTop.1 ht (logLik x p.1 p.2 c + 1)).and
      (eventually_gt_atTop (0 : ℝ))
    obtain ⟨b, hb, hbpos⟩ := hbig.exists
    have hle : logLik x b (x i₀) c ≤ logLik x p.1 p.2 c :=
      hp (show (b, x i₀) ∈ Weibull.domain from ⟨hbpos, hx i₀⟩)
    rw [hconst] at hle hb
    linarith
  · intro h
    exact (held_exponent_mle hn hx hc h).exists

private def R (u : ℝ) : ℝ := u / (exp u - 1)

private def R' (u : ℝ) : ℝ :=
  (exp u - 1 - u * exp u) / (exp u - 1) ^ 2

private theorem Q_pos {u : ℝ} (hu : 0 < u) : 0 < 1 - exp (-u) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hu))

private theorem R_deriv {u : ℝ} (hu : 0 < u) : HasDerivAt R (R' u) u := by
  have he : 0 < exp u - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hu)
  convert (hasDerivAt_id u).div ((hasDerivAt_exp u).sub_const 1) he.ne' using 1
  · rfl
  · simp [R', id_eq]

private theorem R'_bounds {u : ℝ} (hu : 0 < u) : -1 < R' u ∧ R' u < 0 := by
  have he : 0 < exp u - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hu)
  have hp : 0 < (exp u - 1) ^ 2 := sq_pos_of_pos he
  constructor
  · apply (lt_div_iff₀ hp).2
    nlinarith [mul_pos (exp_pos u) (sub_pos.mpr (add_one_lt_exp hu.ne'))]
  · apply div_neg_of_neg_of_pos _ hp
    have h := mul_lt_mul_of_pos_right (add_one_lt_exp (neg_ne_zero.mpr hu.ne')) (exp_pos u)
    rw [← exp_add, neg_add_cancel, exp_zero] at h
    nlinarith

private theorem logKernel_deriv (c z : ℝ) :
    HasDerivAt (logKernel c) (1 - exp z + (c - 1) * R (exp z)) z := by
  have hq := Q_pos (exp_pos z)
  have he : exp (-exp z) * exp (exp z) = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
  have hd : HasDerivAt (fun t ↦ log (1 - exp (-exp t))) (R (exp z)) z := by
    convert (((((hasDerivAt_exp z).neg).exp).const_sub 1).log hq.ne') using 1
    simp only [R, mul_neg, neg_neg, Pi.neg_apply]
    have hden : exp (exp z) - 1 ≠ 0 := (sub_pos.mpr (one_lt_exp_iff.mpr (exp_pos z))).ne'
    apply (div_eq_div_iff hden hq.ne').2
    nlinarith [congrArg (fun w ↦ w * exp z) he]
  exact ((hasDerivAt_id z).sub (hasDerivAt_exp z)).add (hd.const_mul (c - 1))

private theorem logKernel_strictConcave {c : ℝ} (hc : 0 < c) :
    StrictConcaveOn ℝ univ (logKernel c) := by
  apply strictConcaveOn_of_deriv2_neg' convex_univ
    (fun z _ ↦ (logKernel_deriv c z).continuousAt.continuousWithinAt)
  intro z _
  have he : deriv (logKernel c) = fun t ↦ 1 - exp t + (c - 1) * R (exp t) :=
    funext fun t ↦ (logKernel_deriv c t).deriv
  have hd := ((hasDerivAt_exp z).const_sub 1).add
    (((R_deriv (exp_pos z)).comp z (hasDerivAt_exp z)).const_mul (c - 1))
  change deriv (deriv (logKernel c)) z < 0
  rw [he]
  have hh : deriv (fun t ↦ 1 - exp t + (c - 1) * R (exp t)) z =
      -exp z + (c - 1) * (R' (exp z) * exp z) := hd.deriv
  rw [hh]
  obtain ⟨hlo, hhi⟩ := R'_bounds (exp_pos z)
  have h : (c - 1) * R' (exp z) < 1 := by
    by_cases h : 1 ≤ c
    · linarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hhi.le]
    · have h' := mul_lt_mul_of_neg_left hlo (sub_neg.mpr (lt_of_not_ge h))
      linarith
  change -exp z + (c - 1) * (R' (exp z) * exp z) < 0
  nlinarith [mul_lt_mul_of_pos_right h (exp_pos z)]

private theorem logKernel_bound {c : ℝ} (hc : 0 < c) (z : ℝ) :
    logKernel c z ≤ -(min 1 c * |z|) := by
  let u := exp z
  have hu : 0 < u := exp_pos z
  have hQ := Q_pos hu
  have hlow : z - u ≤ log (1 - exp (-u)) := by
    calc
      z - u = log (exp (z - u)) := (log_exp _).symm
      _ ≤ _ := by
        apply log_le_log (exp_pos _)
        rw [exp_sub]
        change u / exp u ≤ 1 - exp (-u)
        have h := add_one_le_exp u
        have hp : exp (-u) * exp u = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
        apply (div_le_iff₀ (exp_pos u)).2
        nlinarith
  have hupp : log (1 - exp (-u)) ≤ z := by
    rw [← log_exp z]
    apply log_le_log hQ
    exact (show 1 - exp (-u) ≤ u by linarith [add_one_le_exp (-u)])
  have hneg : log (1 - exp (-u)) ≤ 0 := log_nonpos hQ.le (by linarith [exp_pos (-u)])
  have h2 (hz : 0 ≤ z) : 2 * z ≤ exp z := by
    have he : exp 1 * exp (z - 1) = exp z := by
      rw [← exp_add, show 1 + (z - 1) = z by ring]
    have h := add_one_le_exp (z - 1)
    calc
      2 * z ≤ exp 1 * z := mul_le_mul_of_nonneg_right
        (show (2 : ℝ) ≤ exp 1 by linarith [add_one_le_exp (1 : ℝ)]) hz
      _ ≤ exp z := by rw [← he]; exact mul_le_mul_of_nonneg_left (by linarith) (exp_pos _).le
  change z - u + (c - 1) * log (1 - exp (-u)) ≤ -(min 1 c * |z|)
  by_cases h : 1 ≤ c
  · rw [min_eq_left h]
    rcases le_total 0 z with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [h2 hz, mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hneg]
    · rw [abs_of_nonpos hz]
      nlinarith [mul_le_mul_of_nonneg_left hupp (sub_nonneg.mpr h),
        mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr h) hz]
  · rw [min_eq_right (le_of_not_ge h)]
    have hbound : z - u + (c - 1) * log (1 - exp (-u)) ≤ c * (z - u) := by
      nlinarith [mul_le_mul_of_nonpos_left hlow (sub_nonpos.mpr (le_of_not_ge h))]
    apply hbound.trans
    rcases le_total 0 z with hz | hz
    · rw [abs_of_nonneg hz]
      nlinarith [mul_le_mul_of_nonneg_left (h2 hz) hc.le]
    · rw [abs_of_nonpos hz]
      nlinarith [mul_pos hc hu]


private theorem physical_chart {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) (b c : ℝ) :
    logLik x b a c =
      ((n : ℝ) * log b + ∑ i, logKernel c (b * log (x i / a))) +
        ((n : ℝ) * log c - ∑ i, log (x i)) := by
  have hr (i : Fin n) : (x i / a) ^ b = exp (b * log (x i / a)) := by
    rw [rpow_def_of_pos (div_pos (hx i) ha), mul_comm]
  have hl (i : Fin n) : log (x i / a) - log (x i) = -log a := by
    rw [log_div (hx i).ne' ha.ne']
    ring
  have hpoint (i : Fin n) : Weibull.logDensity b a (x i) =
      log b + b * log (x i / a) - exp (b * log (x i / a)) - log (x i) := by
    rw [Weibull.logDensity, hr]
    linarith only [hl i]
  simp only [logLik, Weibull.logLik, hpoint, hr, logKernel, Finset.sum_add_distrib,
    Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.mul_sum]
  ring

private def shapeObjective (d : Fin n → ℝ) (c b : ℝ) : ℝ :=
  (n : ℝ) * log b + ∑ i, logKernel c (b * d i)

private theorem shape_strictConcave (hn : 0 < n) {c : ℝ} (hc : 0 < c)
    (d : Fin n → ℝ) : StrictConcaveOn ℝ (Ioi 0) (shapeObjective d c) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  refine ⟨convex_Ioi 0, ?_⟩
  intro b hb e he hne u v hu hv huv
  have hs : ∑ i, (u * logKernel c (b * d i) + v * logKernel c (e * d i)) ≤
      ∑ i, logKernel c ((u * b + v * e) * d i) := by
    apply Finset.sum_le_sum
    intro i _
    rw [add_mul, mul_assoc, mul_assoc]
    exact (logKernel_strictConcave hc).concaveOn.2 (mem_univ _) (mem_univ _)
      hu.le hv.le huv
  simp only [Finset.sum_add_distrib, ← Finset.mul_sum] at hs
  have hl := strictConcaveOn_log_Ioi.2 hb he hne hu hv huv
  change u * log b + v * log e < log (u * b + v * e) at hl
  change u * shapeObjective d c b + v * shapeObjective d c e <
    shapeObjective d c (u * b + v * e)
  unfold shapeObjective
  linarith only [hs, mul_lt_mul_of_pos_left hl hn']

private theorem shape_attains (hn : 0 < n) {c : ℝ} (hc : 0 < c)
    {d : Fin n → ℝ} (hd : ∃ i, d i ≠ 0) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (shapeObjective d c) (Ioi 0) b := by
  obtain ⟨i, hi⟩ := hd
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let D := min 1 c * |d i|
  have hD : 0 < D := mul_pos (lt_min zero_lt_one hc) (abs_pos.mpr hi)
  have hp (b : ℝ) (hb : 0 < b) :
      shapeObjective d c b ≤ (n : ℝ) * log b - D * b := by
    have hnonneg (j : Fin n) : 0 ≤ -logKernel c (b * d j) := by
      have ha := mul_nonneg (lt_min zero_lt_one hc).le (abs_nonneg (b * d j))
      linarith only [logKernel_bound hc (b * d j), ha]
    have hs := Finset.single_le_sum (fun j (_ : j ∈ Finset.univ) ↦ hnonneg j)
      (Finset.mem_univ i)
    rw [Finset.sum_neg_distrib] at hs
    have h := logKernel_bound hc (b * d i)
    rw [abs_mul, abs_of_pos hb] at h
    unfold shapeObjective D
    linarith only [hs, h]
  let M := shapeObjective d c 1
  let l := min 1 (exp (M / n))
  let t := D / (2 * n)
  let r := max 1 ((-(n : ℝ) * (log t + 1) - M) / (D / 2))
  have ht : 0 < t := div_pos hD (by positivity)
  have hl : 0 < l := lt_min zero_lt_one (exp_pos _)
  have hsub : Icc l r ⊆ Ioi (0 : ℝ) := fun _ hb ↦ hl.trans_le hb.1
  have hcontains (b : ℝ) (hb : 0 < b) (hM : M ≤ shapeObjective d c b) :
      b ∈ Icc l r := by
    have hbnd := hp b hb
    constructor
    · apply (min_le_right _ _).trans
      rw [← exp_log hb, exp_le_exp]
      apply (div_le_iff₀ hn').2
      nlinarith only [hM, hbnd, mul_pos hD hb]
    · apply le_trans _ (le_max_right _ _)
      apply (le_div_iff₀ (by positivity : 0 < D / 2)).2
      have hg := log_le_sub_one_of_pos (mul_pos ht hb)
      rw [log_mul ht.ne' hb.ne'] at hg
      have htn : (n : ℝ) * t = D / 2 := by dsimp [t]; field_simp
      have hmul : (n : ℝ) * (t * b) = (D / 2) * b := by rw [← mul_assoc, htn]
      have hg' := mul_le_mul_of_nonneg_left hg hn'.le
      nlinarith only [hbnd, hM, hg', hmul]
  have h1 : (1 : ℝ) ∈ Icc l r := ⟨min_le_left _ _, le_max_left _ _⟩
  have hsc := shape_strictConcave hn hc d
  obtain ⟨b, hb, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩
    ((hsc.concaveOn.continuousOn isOpen_Ioi).mono hsub)
  have hm : IsMaxOn (shapeObjective d c) (Ioi 0) b := by
    intro s hs
    by_cases hM : M ≤ shapeObjective d c s
    · exact hmax (hcontains s hs hM)
    · exact (le_of_not_ge hM).trans (hmax h1)
  exact ⟨b, ⟨hsub hb, hm⟩, fun s hs ↦ hsc.eq_of_isMaxOn hs.2 hm hs.1 (hsub hb)⟩

/-- A held positive exponent and scale leave a single optimal shape exactly
when some observation differs from that scale. In the complementary case
the log likelihood diverges as shape increases. -/
theorem held_exponent_scale (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a c : ℝ} (ha : 0 < a) (hc : 0 < c) :
    ((∃ i, x i ≠ a) →
      ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun b ↦ logLik x b a c) (Ioi 0) b) ∧
    ((∀ i, x i = a) → Tendsto (fun b ↦ logLik x b a c) atTop atTop) := by
  constructor
  · rintro ⟨i, hi⟩
    have hd : log (x i / a) ≠ 0 := by
      intro h
      have h' := exp_log (div_pos (hx i) ha)
      rw [h, exp_zero] at h'
      exact hi ((div_eq_one_iff_eq ha.ne').1 h'.symm)
    exact (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ)))
      (fun b _ ↦ physical_chart hx ha b c)).2 (shape_attains hn hc ⟨i, hd⟩)
  · intro h
    have he : x = fun _ ↦ a := funext h
    simpa only [he] using constant_concentration hn ha hc

/-- At every pair of positive holds, the shape and exponent fix a unique
positive scale for any nonempty positive sample, including ties. -/
theorem held_exponent_shape_mle (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b c : ℝ} (hb : 0 < b) (hc : 0 < c) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun a ↦ logLik x b a c) (Ioi 0) a := by
  let L := fun m : ℝ ↦ ∑ j, logKernel c (b * (log (x j) - m))
  let i : Fin n := ⟨0, hn⟩
  have hsc : StrictConcaveOn ℝ univ L := by
    refine ⟨convex_univ, ?_⟩
    intro m _ q _ hmq u v hu hv huv
    have hpoint (j : Fin n) :
        u * logKernel c (b * (log (x j) - m)) +
          v * logKernel c (b * (log (x j) - q)) <
        logKernel c (b * (log (x j) - (u * m + v * q))) := by
      have he : u * (b * (log (x j) - m)) + v * (b * (log (x j) - q)) =
          b * (log (x j) - (u * m + v * q)) := by
        linear_combination (b * log (x j)) * huv
      have hne : b * (log (x j) - m) ≠ b * (log (x j) - q) := by
        intro h
        exact hmq (by linarith only [(mul_right_inj' hb.ne').1 h])
      rw [← he]
      exact (logKernel_strictConcave hc).2 (mem_univ _) (mem_univ _) hne hu hv huv
    have hs := Finset.sum_lt_sum_of_nonempty (s := Finset.univ)
      ⟨i, Finset.mem_univ i⟩ (fun j _ ↦ hpoint j)
    change u * L m + v * L q < L (u * m + v * q)
    simpa only [L, Finset.sum_add_distrib, ← Finset.mul_sum] using hs
  let D := min 1 c * b
  have hD : 0 < D := mul_pos (lt_min zero_lt_one hc) hb
  have hbound (m : ℝ) : L m ≤ -(D * |log (x i) - m|) := by
    have hp (j : Fin n) : 0 ≤ -logKernel c (b * (log (x j) - m)) := by
      have ha := mul_nonneg (lt_min zero_lt_one hc).le (abs_nonneg (b * (log (x j) - m)))
      linarith only [logKernel_bound hc (b * (log (x j) - m)), ha]
    have hs := Finset.single_le_sum (fun j (_ : j ∈ Finset.univ) ↦ hp j)
      (Finset.mem_univ i)
    rw [Finset.sum_neg_distrib] at hs
    have hi := logKernel_bound hc (b * (log (x i) - m))
    rw [abs_mul, abs_of_pos hb, ← mul_assoc] at hi
    linarith only [show logKernel c (b * (log (x i) - m)) ≤ -(D * |log (x i) - m|) from hi,
      hs, show L m = _ from rfl]
  let y := log (x i)
  let r := -L y / D
  have hr : 0 ≤ r := div_nonneg (by simpa [y] using hbound y) hD.le
  have hy : y ∈ Icc (y - r) (y + r) := ⟨by linarith, by linarith⟩
  obtain ⟨m, hm, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨y, hy⟩
    ((hsc.concaveOn.continuousOn isOpen_univ).mono (subset_univ _))
  have hmu : IsMaxOn L univ m := by
    intro q _
    by_cases h : L y ≤ L q
    · have h' : |y - q| ≤ r := (le_div_iff₀ hD).2 (by linarith only [h, hbound q])
      exact hmax ⟨by linarith [(abs_le.mp h').2], by linarith [(abs_le.mp h').1]⟩
    · exact (le_of_not_ge h).trans (hmax hy)
  have hbij : BijOn log (Ioi (0 : ℝ)) univ :=
    ⟨fun _ _ ↦ mem_univ _, log_injOn_pos, fun q _ ↦ ⟨exp q, exp_pos _, log_exp q⟩⟩
  apply (Transfer.existsUnique_max_iff (f := fun a ↦ logLik x b a c) (g := L)
    (c := (n : ℝ) * (log b + log c) - ∑ j, log (x j)) hbij ?_).2
  · exact ⟨m, ⟨mem_univ _, hmu⟩, fun t ht ↦
      hsc.eq_of_isMaxOn ht.2 hmu (mem_univ _) (mem_univ _)⟩
  · intro a ha
    rw [physical_chart hx ha b c]
    have he (j : Fin n) : log (x j / a) = log (x j) - log a := log_div (hx j).ne' ha.ne'
    simp only [he, L]
    ring

private theorem concentration_residual {t : ℝ} (ht : 0 < t) :
    -1 ≤ (exp t - 1) * log (1 - exp (-t)) := by
  have hq := Q_pos ht
  have hlog := log_le_sub_one_of_pos (inv_pos.mpr hq)
  rw [log_inv] at hlog
  have hE : exp t * exp (-t) = 1 := by rw [← exp_add, add_neg_cancel, exp_zero]
  have hFact : exp t - 1 = exp t * (1 - exp (-t)) := by linarith only [hE]
  have hprod := mul_le_mul_of_nonneg_left hlog
    (show 0 ≤ exp t - 1 from sub_nonneg.mpr (one_le_exp_iff.mpr ht.le))
  have hcancel : (exp t - 1) * (1 - exp (-t))⁻¹ = exp t := by
    rw [hFact]
    field_simp
  rw [mul_sub, mul_one, hcancel, mul_neg] at hprod
  linarith only [hprod]

/-- At held shape, the concentration path changes both scale and exponent. -/
theorem held_shape_constant_concentration (hn : 0 < n) {x₀ b : ℝ}
    (hx₀ : 0 < x₀) (hb : 0 < b) :
    Tendsto (fun t ↦ logLik (fun _ : Fin n ↦ x₀) b
      (x₀ * exp (-log t / b)) (exp t)) atTop atTop := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hlim : Tendsto (fun t : ℝ ↦ (n : ℝ) * (log t + (log b - log x₀ - 1)))
      atTop atTop :=
    (tendsto_log_atTop.atTop_add tendsto_const_nhds).const_mul_atTop hn'
  apply tendsto_atTop_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with t ht
  let a := x₀ * exp (-log t / b)
  have ha : 0 < a := mul_pos hx₀ (exp_pos _)
  have hz : b * log (x₀ / a) = log t := by
    rw [log_div hx₀.ne' ha.ne']
    dsimp [a]
    rw [log_mul hx₀.ne' (exp_ne_zero _), log_exp]
    field_simp
    ring
  rw [physical_chart (fun _ ↦ hx₀) ha, log_exp]
  simp only [hz, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
    logKernel, exp_log ht]
  have hterm := concentration_residual ht
  nlinarith only [mul_le_mul_of_nonneg_left hterm hn'.le]

theorem held_shape_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {b : ℝ} (hb : 0 < b) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x b q.1 q.2) Weibull.domain p) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, _, hp⟩
    by_contra h
    push Not at h
    let i₀ : Fin n := ⟨0, hn⟩
    have he : x = fun _ ↦ x i₀ := funext fun i ↦ h i i₀
    have ht := held_shape_constant_concentration hn (hx i₀) hb
    obtain ⟨t, ht⟩ := (tendsto_atTop.1 ht (logLik x b p.1 p.2 + 1)).exists
    have hc : (x i₀ * exp (-log t / b), exp t) ∈ Weibull.domain :=
      ⟨mul_pos (hx i₀) (exp_pos _), exp_pos _⟩
    have hle : logLik x b (x i₀ * exp (-log t / b)) (exp t) ≤ logLik x b p.1 p.2 :=
      @hp (x i₀ * exp (-log t / b), exp t) hc
    rw [he] at ht hle
    linarith
  · exact held_shape_attains hn hx hb

def logQ (u : ℝ) : ℝ := log (1 - exp (-u))

private theorem logQ_strict_bounds {u : ℝ} (hu : 0 < u) :
    log u - u < logQ u ∧ logQ u < log u ∧ logQ u < 0 := by
  have hq := Q_pos hu
  refine ⟨?_, ?_, log_neg hq (by linarith [exp_pos (-u)])⟩
  · rw [← log_exp (log u - u)]
    apply log_lt_log (exp_pos _)
    rw [exp_sub, exp_log hu]
    apply (div_lt_iff₀ (exp_pos u)).2
    have hp : exp (-u) * exp u = 1 := by rw [← exp_add, neg_add_cancel, exp_zero]
    nlinarith only [add_one_lt_exp hu.ne', hp]
  · apply log_lt_log hq
    linarith only [add_one_lt_exp (neg_ne_zero.mpr hu.ne')]

private theorem physical_sum {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) (b c : ℝ) :
    logLik x b a c =
      ∑ i, (log b + log c + logKernel c (b * log (x i / a)) - log (x i)) := by
  rw [physical_chart hx ha]
  simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

/-- Per-sample held-endpoint trace, with a penalty for every contact. -/
def heldPowerTrace (x : Fin n → ℝ) (a k : ℝ) : ℝ :=
  ∑ i, (log k + k * log (x i / a) - log (x i) -
    if x i = a then log (exp 1 - 1) else 0)

private theorem contact_penalty : log (exp 1 - 1) = logQ 1 + 1 := by
  have h : (1 - exp (-1)) * exp 1 = exp 1 - 1 := by
    rw [sub_mul, one_mul, ← exp_add, neg_add_cancel, exp_zero]
  rw [← h, log_mul (Q_pos one_pos).ne' (exp_ne_zero _), log_exp, logQ]

private theorem held_point_limit {d k : ℝ} {c : ℝ → ℝ} (hk : 0 < k)
    (hd : d ≤ 0) (hc : ∀ᶠ b in atTop, 0 < c b)
    (hbc : Tendsto (fun b ↦ b * c b) atTop (𝓝 k)) :
    Tendsto (fun b ↦ log b + log (c b) + logKernel (c b) (b * d)) atTop
      (𝓝 (log k + k * d - if d = 0 then log (exp 1 - 1) else 0)) := by
  have hc0 : Tendsto c atTop (𝓝 0) := by
    have h := hbc.mul (tendsto_inv_atTop_zero : Tendsto (fun b : ℝ ↦ b⁻¹) atTop (𝓝 0))
    rw [mul_zero] at h
    refine h.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    field_simp
  have hl : Tendsto (fun b ↦ log (b * c b)) atTop (𝓝 (log k)) :=
    (continuousAt_log hk.ne').tendsto.comp hbc
  rcases hd.eq_or_lt with rfl | hd
  · have h := (hl.add ((hc0.sub_const 1).mul_const (logQ 1))).sub_const 1
    simp only [mul_zero, add_zero, ite_true, contact_penalty]
    rw [show log k - (logQ 1 + 1) = log k + (0 - 1) * logQ 1 - 1 by ring]
    apply h.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ), hc] with b hb hcb
    simp only [log_mul hb.ne' hcb.ne', logKernel, exp_zero, zero_sub, logQ]
    ring
  · have hx : Tendsto (fun b : ℝ ↦ exp (b * d)) atTop (𝓝 0) :=
      tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg hd)
    let B := fun b : ℝ ↦ logQ (exp (b * d)) - b * d
    have hB : Tendsto B atTop (𝓝 0) := by
      have hneg : Tendsto (fun b : ℝ ↦ -exp (b * d)) atTop (𝓝 0) := by
        simpa only [neg_zero] using hx.neg
      apply tendsto_of_tendsto_of_tendsto_of_le_of_le hneg tendsto_const_nhds
      · intro b
        have h := (logQ_strict_bounds (exp_pos (b * d))).1
        rw [log_exp] at h
        unfold B
        linarith only [h]
      · intro b
        have h := (logQ_strict_bounds (exp_pos (b * d))).2.1
        rw [log_exp] at h
        exact sub_nonpos.mpr h.le
    have h := ((hl.add (hbc.mul_const d)).sub hx).add ((hc0.sub_const 1).mul hB)
    simp only [ite_eq_right hd.ne, sub_zero]
    simp only [sub_zero, mul_zero, add_zero] at h
    apply h.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ), hc] with b hb hcb
    dsimp only [B, logKernel, logQ]
    rw [log_mul hb.ne' hcb.ne']
    ring

/-- Any positive exponent path with shape times exponent tending to a
positive constant has the penalized power trace at a held scale. -/
theorem held_scale_power_path {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a k : ℝ} (ha : 0 < a) (hk : 0 < k) (hle : ∀ i, x i ≤ a)
    {c : ℝ → ℝ} (hc : ∀ᶠ b in atTop, 0 < c b)
    (hbc : Tendsto (fun b ↦ b * c b) atTop (𝓝 k)) :
    Tendsto (fun b ↦ logLik x b a (c b)) atTop (𝓝 (heldPowerTrace x a k)) := by
  have h (i : Fin n) : log (x i / a) ≤ 0 :=
    log_nonpos (div_pos (hx i) ha).le ((div_le_one ha).2 (hle i))
  have heq (i : Fin n) : log (x i / a) = 0 ↔ x i = a := by
    rw [← log_one, log_injOn_pos.eq_iff (div_pos (hx i) ha) zero_lt_one,
      div_eq_one_iff_eq ha.ne']
  have ht := tendsto_finsetSum Finset.univ
    (fun i _ ↦ (held_point_limit hk (h i) hc hbc).sub_const (log (x i)))
  have he (i : Fin n) : log k + k * log (x i / a) -
      (if log (x i / a) = 0 then log (exp 1 - 1) else 0) - log (x i) =
      log k + k * log (x i / a) - log (x i) -
      (if x i = a then log (exp 1 - 1) else 0) := by
    simp only [heq i]
    ring
  simp only [he] at ht
  unfold heldPowerTrace
  exact ht.congr fun b ↦ (physical_sum hx ha b (c b)).symm

/-- With at least one observation below the held scale and none above it,
the optimized power trace is the supremum and strictly beats every finite fit. -/
theorem held_scale_power_supremum (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) (hle : ∀ i, x i ≤ a)
    (hlt : ∃ i, x i < a) :
    let D := ∑ i, -log (x i / a)
    let P := heldPowerTrace x a ((n : ℝ) / D)
    IsLUB ((fun p : ℝ × ℝ ↦ logLik x p.1 a p.2) '' Weibull.domain) P ∧
      ∀ b c, 0 < b → 0 < c → logLik x b a c < P := by
  let D := ∑ i, -log (x i / a)
  have hD : 0 < D := by
    obtain ⟨i, hi⟩ := hlt
    exact Finset.sum_pos'
      (fun j _ ↦ neg_nonneg.mpr (log_nonpos (div_pos (hx j) ha).le ((div_le_one ha).2 (hle j))))
      ⟨i, Finset.mem_univ _, neg_pos.mpr (log_neg (div_pos (hx i) ha) ((div_lt_one ha).2 hi))⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let k := (n : ℝ) / D
  have hk : 0 < k := div_pos hn' hD
  have hmul : D * k = n := by dsimp [k]; field_simp
  have hstrict (b c : ℝ) (hb : 0 < b) (hc : 0 < c) :
      logLik x b a c < heldPowerTrace x a k := by
    have hpoint (i : Fin n) :
        log b + log c + logKernel c (b * log (x i / a)) - log (x i) <
          log (b * c) + (b * c) * log (x i / a) - log (x i) -
            (if x i = a then log (exp 1 - 1) else 0) := by
      rw [log_mul hb.ne' hc.ne']
      by_cases hi : x i = a
      · simp only [hi, div_self ha.ne', log_one, mul_zero, add_zero, ite_true,
          logKernel, exp_zero, zero_sub, contact_penalty]
        change _ + (-(1 : ℝ) + (c - 1) * logQ 1) - log a <
          _ - log a - (logQ 1 + 1)
        linarith only [mul_neg_of_pos_of_neg hc (logQ_strict_bounds one_pos).2.2]
      · simp only [ite_eq_right hi, sub_zero, logKernel]
        let z := b * log (x i / a)
        have hz := logQ_strict_bounds (exp_pos z)
        rw [log_exp] at hz
        change log b + log c + (z - exp z + (c - 1) * logQ (exp z)) - log (x i) <
          log b + log c + (b * c) * log (x i / a) - log (x i)
        nlinarith only [hz.1, mul_lt_mul_of_pos_left hz.2.1 hc,
          show c * z = (b * c) * log (x i / a) by unfold z; ring]
    have hs := Finset.sum_lt_sum_of_nonempty (s := Finset.univ)
      ⟨⟨0, hn⟩, Finset.mem_univ _⟩ (fun i _ ↦ hpoint i)
    rw [physical_sum hx ha]
    apply hs.trans_le
    have hlog := log_le_sub_one_of_pos (div_pos (mul_pos hb hc) hk)
    rw [log_div (mul_pos hb hc).ne' hk.ne'] at hlog
    have hr : (n : ℝ) * (b * c / k) = D * (b * c) := by
      rw [← hmul]
      field_simp
    have hg := mul_le_mul_of_nonneg_left hlog hn'.le
    unfold heldPowerTrace
    simp only [Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum]
    have hD' : D = -(∑ i, log (x i / a)) := Finset.sum_neg_distrib _
    nlinarith only [hg, hr, hmul, congrArg (fun v ↦ (b * c) * v) hD',
      congrArg (fun v ↦ k * v) hD']
  have hc : ∀ᶠ b : ℝ in atTop, 0 < k / b :=
    (eventually_gt_atTop (0 : ℝ)).mono fun _ hb ↦ div_pos hk hb
  have hbc : Tendsto (fun b : ℝ ↦ b * (k / b)) atTop (𝓝 k) := by
    apply tendsto_const_nhds.congr'
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    field_simp
  have hT := held_scale_power_path hx ha hk hle hc hbc
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨p, hp, rfl⟩
    exact (hstrict p.1 p.2 hp.1 hp.2).le
  · intro y hy
    apply le_of_tendsto hT
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
    exact hy ⟨(b, k / b), ⟨hb, div_pos hk hb⟩, rfl⟩

/-- A sample constant at or above the held scale concentrates by increasing
the shape and choosing the exponent from the powered observation. -/
theorem held_scale_constant_concentration (hn : 0 < n) {x₀ a : ℝ}
    (ha : 0 < a) (hxa : a ≤ x₀) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ x₀) b a
      (exp (exp (b * log (x₀ / a))))) atTop atTop := by
  have hx₀ := ha.trans_le hxa
  have hd : 0 ≤ log (x₀ / a) := log_nonneg ((le_div_iff₀ ha).2 (by simpa using hxa))
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hlim : Tendsto (fun b : ℝ ↦ (n : ℝ) * (log b + (-1 - log x₀)))
      atTop atTop :=
    (tendsto_log_atTop.atTop_add tendsto_const_nhds).const_mul_atTop hn'
  apply tendsto_atTop_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  let z := b * log (x₀ / a)
  rw [physical_sum (fun _ ↦ hx₀) ha]
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, log_exp]
  change _ ≤ (n : ℝ) * (log b + exp z + logKernel (exp (exp z)) z - log x₀)
  apply mul_le_mul_of_nonneg_left _ hn'.le
  unfold logKernel
  linarith only [concentration_residual (exp_pos z), mul_nonneg hb.le hd]


def heldScaleRate (d : Fin n → ℝ) (b : ℝ) : ℝ := ∑ i, -logQ (exp (b * d i))

def residue (d : Fin n → ℝ) (b : ℝ) : ℝ :=
  ∑ i, (b * d i - exp (b * d i) - logQ (exp (b * d i)))

def heldScaleProfile (d : Fin n → ℝ) (b : ℝ) : ℝ :=
  (n : ℝ) * log b + (n : ℝ) * log ((n : ℝ) / heldScaleRate d b) - n + residue d b

private theorem rate_pos (hn : 0 < n) (d : Fin n → ℝ) (b : ℝ) : 0 < heldScaleRate d b :=
  Finset.sum_pos (fun i _ ↦ neg_pos.mpr (logQ_strict_bounds (exp_pos (b * d i))).2.2)
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private def profileRemainder (d : Fin n → ℝ) (b : ℝ) : ℝ :=
  (n : ℝ) * log ((n : ℝ) / heldScaleRate d b) - n + residue d b

private theorem remainder_analytic (hn : 0 < n) (d : Fin n → ℝ) (b : ℝ) :
    AnalyticAt ℝ (profileRemainder d) b := by
  have hterm (i : Fin n) : AnalyticAt ℝ (fun t ↦ logQ (exp (t * d i))) b := by
    unfold logQ
    apply AnalyticAt.log (by fun_prop)
    exact Q_pos (exp_pos (b * d i))
  have hrate : AnalyticAt ℝ (heldScaleRate d) b := by
    unfold heldScaleRate
    fun_prop
  have hres : AnalyticAt ℝ (residue d) b := by
    unfold residue
    fun_prop
  have hFrac : AnalyticAt ℝ (fun t ↦ log ((n : ℝ) / heldScaleRate d t)) b := by
    apply AnalyticAt.log
    · exact analyticAt_const.div hrate (rate_pos hn d b).ne'
    · exact div_pos (by exact_mod_cast hn) (rate_pos hn d b)
  unfold profileRemainder
  fun_prop

private theorem profile_analytic (hn : 0 < n) (d : Fin n → ℝ) :
    AnalyticOnNhd ℝ (heldScaleProfile d) (Ioi 0) := by
  intro b hb
  have hl : AnalyticAt ℝ log b := analyticAt_log hb
  have hr := remainder_analytic hn d b
  have he : heldScaleProfile d = fun t ↦ (n : ℝ) * log t + profileRemainder d t := by
    funext t
    unfold heldScaleProfile profileRemainder
    ring
  rw [he]
  fun_prop

private theorem profile_continuous (hn : 0 < n) (d : Fin n → ℝ) :
    ContinuousOn (heldScaleProfile d) (Ioi 0) := fun b hb ↦
  ((profile_analytic hn d) b hb).continuousAt.continuousWithinAt

private theorem fixed_scale_chart {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) (b c : ℝ) :
    let d := fun i ↦ log (x i / a)
    logLik x b a c = (n : ℝ) * log b + (n : ℝ) * log c - c * heldScaleRate d b +
      residue d b - ∑ i, log (x i) := by
  intro d
  rw [physical_chart hx ha]
  change ((n : ℝ) * log b + ∑ i, logKernel c (b * d i)) + _ = _
  unfold residue heldScaleRate
  have he (i : Fin n) : logKernel c (b * d i) =
      (b * d i - exp (b * d i) - logQ (exp (b * d i))) -
        c * (-logQ (exp (b * d i))) := by
    simp only [logKernel, logQ]
    ring
  simp only [he, Finset.sum_sub_distrib, ← Finset.mul_sum]
  ring

private theorem profile_bounds (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) (b : ℝ) :
    let d := fun i ↦ log (x i / a)
    let c₀ := (n : ℝ) / heldScaleRate d b
    (0 < c₀) ∧ logLik x b a c₀ = heldScaleProfile d b - ∑ i, log (x i) ∧
      (∀ c > 0, logLik x b a c ≤ logLik x b a c₀) := by
  intro d c₀
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hA := rate_pos hn d b
  have hc₀ : 0 < c₀ := div_pos hN hA
  have he : c₀ * heldScaleRate d b = n := div_mul_cancel₀ _ hA.ne'
  refine ⟨hc₀, ?_, ?_⟩
  · rw [fixed_scale_chart hx ha, he]
    rfl
  · intro c hc
    rw [fixed_scale_chart hx ha, fixed_scale_chart hx ha]
    have hl := log_le_sub_one_of_pos (div_pos hc hc₀)
    rw [log_div hc.ne' hc₀.ne'] at hl
    have hprod : (n : ℝ) * (c / c₀) = c * heldScaleRate d b := by
      rw [← he]
      field_simp
    have hsub : (n : ℝ) * (c / c₀ - 1) = c * heldScaleRate d b - n := by
      rw [mul_sub, hprod, mul_one]
    have h' := mul_le_mul_of_nonneg_left hl hN.le
    rw [hsub] at h'
    nlinarith only [h', he]

private theorem profile_left (hn : 0 < n) (d : Fin n → ℝ) :
    Tendsto (heldScaleProfile d) (𝓝[>] 0) atBot := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have ht := (tendsto_log_nhdsGT_zero.const_mul_atBot hN).atBot_add
    ((remainder_analytic hn d 0).continuousAt.tendsto.mono_left nhdsWithin_le_nhds)
  exact ht.congr fun b ↦ by unfold heldScaleProfile profileRemainder; ring

private theorem profile_right (hn : 0 < n) {d : Fin n → ℝ}
    {i j : Fin n} (hdi : 0 < d i) (hgap : d j < d i) :
    Tendsto (heldScaleProfile d) atTop atBot := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let A := (n : ℝ) + d i
  have hA : 0 < A := add_pos hN hdi
  let C := (n : ℝ) * log n - n - logQ 1
  have hsmall : ∀ᶠ b : ℝ in atTop, exp (b * (d j - d i)) ≤ 1 / (2 * n) :=
    ((tendsto_exp_atBot.comp (tendsto_id.atTop_mul_const_of_neg (sub_neg.mpr hgap))).eventually
      (eventually_lt_nhds (by positivity : (0 : ℝ) < 1 / (2 * n)))).mono fun _ h ↦ h.le
  have hlarge : ∀ᶠ b : ℝ in atTop, 4 * A * b ≤ exp (b * d i) := by
    filter_upwards [eventually_ge_atTop (8 * A / d i ^ 2)] with b hb
    have hlo := div_nonneg (by positivity : 0 ≤ 8 * A) (sq_nonneg (d i))
    have hb0 := hlo.trans hb
    have hd := (div_le_iff₀ (sq_pos_of_pos hdi)).1 hb
    have hm := mul_le_mul_of_nonneg_right hd hb0
    have he := quadratic_le_exp_of_nonneg (mul_nonneg hb0 hdi.le)
    nlinarith only [he, hm, mul_nonneg hb0 hdi.le]
  have hlim := tendsto_atBot_add_const_right _ C
    (tendsto_id.const_mul_atTop_of_neg (neg_neg_of_pos hA))
  apply tendsto_atBot_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ), hsmall, hlarge] with b hb hsmall hlarge
  let ui := exp (b * d i)
  let uj := exp (b * d j)
  have hU : (n : ℝ) * uj ≤ ui / 2 := by
    dsimp only [ui, uj]
    have he : exp (b * (d j - d i)) = exp (b * d j) / exp (b * d i) := by
      rw [mul_sub, exp_sub]
    rw [he] at hsmall
    have h := (div_le_iff₀ (exp_pos (b * d i))).1 hsmall
    have h' := mul_le_mul_of_nonneg_left h (show 0 ≤ 2 * (n : ℝ) from by positivity)
    have hh : 2 * (n : ℝ) * (1 / (2 * n) * exp (b * d i)) = exp (b * d i) := by
      field_simp
    linarith only [h', hh]
  have hR : residue d b ≤ b * d i - ui - logQ 1 := by
    have hnonpos (k : Fin n) : 0 ≤
        -(b * d k - exp (b * d k) - logQ (exp (b * d k))) := by
      have h := (logQ_strict_bounds (exp_pos (b * d k))).1
      rw [log_exp] at h
      linarith only [h]
    have h := Finset.single_le_sum (fun k (_ : k ∈ Finset.univ) ↦ hnonpos k)
      (Finset.mem_univ i)
    rw [Finset.sum_neg_distrib] at h
    have hQ : logQ 1 ≤ logQ ui := by
      apply log_le_log (Q_pos one_pos)
      apply sub_le_sub_left
      apply exp_le_exp.2
      have hui : 1 ≤ ui := one_le_exp_iff.2 (mul_nonneg hb.le hdi.le)
      linarith only [hui]
    unfold residue
    linarith only [h, hQ]
  have hrate : exp (-uj) ≤ heldScaleRate d b := by
    have h := log_le_sub_one_of_pos (Q_pos (exp_pos (b * d j)))
    have ht := Finset.single_le_sum
      (s := Finset.univ) (f := fun k ↦ -logQ (exp (b * d k)))
      (fun k _ ↦ (neg_pos.mpr (logQ_strict_bounds (exp_pos (b * d k))).2.2).le)
      (Finset.mem_univ j)
    unfold logQ at ht
    dsimp [uj]
    exact (by linarith only [h] : exp (-exp (b * d j)) ≤ -log (1 - exp (-exp (b * d j)))).trans ht
  have hlograte := log_le_log (exp_pos (-uj)) hrate
  rw [log_exp] at hlograte
  have hlog := log_le_sub_one_of_pos hb
  unfold heldScaleProfile
  rw [log_div hN.ne' (rate_pos hn d b).ne']
  change _ ≤ -A * b + C
  dsimp [A, C]
  nlinarith only [hR, hU, hlarge, mul_le_mul_of_nonneg_left hlog hN.le,
    mul_le_mul_of_nonneg_left hlograte hN.le]


/-- At a positive held scale the two free coefficients attain exactly
when the sample is nonconstant and at least one observation exceeds scale. -/
theorem held_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a) :
    (∃ p ∈ Weibull.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 a q.2) Weibull.domain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, a < x i := by
  constructor
  · rintro ⟨p, hp, hm⟩
    have hconst (v : ℝ) (hv : a ≤ v) (he : x = fun _ ↦ v) : False := by
      have ht := held_scale_constant_concentration (n := n) hn ha hv
      obtain ⟨b, hb, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
        (ht.eventually (eventually_gt_atTop (logLik x p.1 a p.2)))).exists
      let c := exp (exp (b * log (v / a)))
      have hle := isMaxOn_iff.mp hm (b, c) ⟨hb, exp_pos _⟩
      change logLik x b a c ≤ logLik x p.1 a p.2 at hle
      rw [he] at hle hgt
      exact hgt.not_ge hle
    have habove : ∃ i, a < x i := by
      by_contra! h
      by_cases hlt : ∃ i, x i < a
      · obtain ⟨hB, hstrict⟩ := held_scale_power_supremum hn hx ha h hlt
        exact (hstrict p.1 p.2 hp.1 hp.2).not_ge (hB.2 (by
          rintro _ ⟨q, hq, rfl⟩
          exact hm hq))
      · push Not at hlt
        exact hconst a le_rfl (funext fun i ↦ le_antisymm (h i) (hlt i))
    refine ⟨?_, habove⟩
    by_contra! h
    obtain ⟨i, hi⟩ := habove
    exact hconst (x i) hi.le (funext fun j ↦ h j i)
  · rintro ⟨hne, i₀, hi₀⟩
    let d := fun i ↦ log (x i / a)
    obtain ⟨i, _, hix⟩ := Finset.exists_max_image Finset.univ x ⟨i₀, Finset.mem_univ _⟩
    have hmax (k : Fin n) : x k ≤ x i := hix k (Finset.mem_univ _)
    have hi : 0 < d i := log_pos ((one_lt_div ha).2 (hi₀.trans_le (hmax i₀)))
    obtain ⟨j, hj⟩ : ∃ j, x j < x i := by
      by_contra! h
      obtain ⟨j, k, hjk⟩ := hne
      exact hjk ((le_antisymm (hmax j) (h j)).trans (le_antisymm (hmax k) (h k)).symm)
    have hd : d j < d i := log_lt_log (div_pos (hx j) ha) (div_lt_div_of_pos_right hj ha)
    have h0 := profile_left hn d
    have hInf := profile_right hn hi hd
    obtain ⟨lo, hlo, hlow⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp
      (h0.eventually (eventually_lt_atBot (heldScaleProfile d 1)))
    obtain ⟨hi, hhigh⟩ := eventually_atTop.1
      (hInf.eventually (eventually_lt_atBot (heldScaleProfile d 1)))
    let K := Icc (min lo 1) (max hi 1)
    have hsub : K ⊆ Ioi (0 : ℝ) := fun _ h ↦ (lt_min hlo zero_lt_one).trans_le h.1
    have h1 : 1 ∈ K := ⟨min_le_right _ _, le_max_right _ _⟩
    obtain ⟨b, hb, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, h1⟩
      ((profile_continuous hn d).mono hsub)
    have hbm (t : ℝ) (ht : 0 < t) : heldScaleProfile d t ≤ heldScaleProfile d b := by
      by_cases h : t ∈ K
      · exact hm h
      · have h' : heldScaleProfile d t < heldScaleProfile d 1 := by
          change ¬ (min lo 1 ≤ t ∧ t ≤ max hi 1) at h
          rw [not_and_or] at h
          rcases h with h | h
          · exact hlow ⟨ht, (lt_of_not_ge h).trans_le (min_le_left _ _)⟩
          · exact hhigh t ((le_max_left _ _).trans (lt_of_not_ge h).le)
        exact h'.le.trans (hm h1)
    let c := (n : ℝ) / heldScaleRate d b
    obtain ⟨hc, hbval, _⟩ := profile_bounds hn hx ha b
    refine ⟨(b, c), ⟨hsub hb, hc⟩, ?_⟩
    apply isMaxOn_iff.mpr
    intro q hq
    obtain ⟨_, hv, hqval⟩ := profile_bounds hn hx ha q.1
    have hle := hqval q.2 hq.2
    change logLik x q.1 a q.2 ≤ logLik x b a c
    rw [hbval]
    rw [hv] at hle
    exact hle.trans (sub_le_sub_right (hbm q.1 hq.1) _)


private theorem stationary_finite (hn : 0 < n) (d : Fin n → ℝ) {K : Set ℝ}
    (hK : IsCompact K) (hsub : K ⊆ Ioi 0) :
    {b ∈ K | deriv (heldScaleProfile d) b = 0}.Finite := by
  let f := heldScaleProfile d
  let S := {b ∈ K | deriv f b = 0}
  have hA := profile_analytic hn d
  have hD : AnalyticOnNhd ℝ (deriv f) (Ioi 0) := fun b hb ↦ (hA b hb).deriv
  have hpre := (convex_Ioi (0 : ℝ)).isPreconnected
  have hnonzero : ¬ EqOn (deriv f) 0 (Ioi 0) := by
    intro hzero
    have hc : EqOn f (fun _ ↦ f 1) (Ioi 0) :=
      isOpen_Ioi.eqOn_of_deriv_eq hpre
        (fun b hb ↦ (hA b hb).differentiableAt.differentiableWithinAt)
        (differentiableOn_const (f 1)) (by
          intro b hb
          simpa only [deriv_const, Pi.zero_apply] using hzero hb) one_pos rfl
    have hev := (profile_left hn d).eventually (eventually_lt_atBot (f 1))
    obtain ⟨b, hb, hlt⟩ := ((show ∀ᶠ b : ℝ in 𝓝[>] 0, 0 < b from
      self_mem_nhdsWithin).and hev).exists
    exact hlt.ne (hc hb)
  have hcont : Continuous (fun b : K ↦ deriv f b) :=
    (show ContinuousOn (deriv f) K from fun b hb ↦
      (hD b (hsub hb)).continuousAt.continuousWithinAt).domRestrict
  let : CompactSpace K := isCompact_iff_compactSpace.mp hK
  have hc : IsClosed ((fun b : K ↦ deriv f b) ⁻¹' {0}) := isClosed_singleton.preimage hcont
  have hS : IsCompact S := by
    convert hc.isCompact.image continuous_subtype_val using 1
    ext b
    simp [S, and_comm]
  apply hS.finite
  apply isDiscrete_iff_nhdsNE.2
  intro b hb
  have hb' : b ∈ Ioi (0 : ℝ) := hsub hb.1
  rcases (hD b hb').eventually_eq_zero_or_eventually_ne_zero with he | he
  · exact (hnonzero (hD.eqOn_zero_of_preconnected_of_eventuallyEq_zero hpre hb' he)).elim
  · apply inf_eq_bot_iff.2
    refine ⟨{t | deriv f t ≠ 0}, he, S, mem_principal_self S, ?_⟩
    ext t
    constructor
    · rintro ⟨ht, hs⟩
      exact (ht hs.2).elim
    · simp

/-- All held-scale maximizers are precisely the best stationary shapes in a
compact witness superlevel, each paired with its unique profiled exponent. -/
theorem held_scale_maximizer_set (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {a : ℝ} (ha : 0 < a)
    (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, a < x i) :
    let d := fun i ↦ log (x i / a)
    let W := {b ∈ Ioi (0 : ℝ) | heldScaleProfile d 1 ≤ heldScaleProfile d b}
    let S := {b ∈ W | deriv (heldScaleProfile d) b = 0}
    IsCompact W ∧ S.Finite ∧ S.Nonempty ∧
      ∀ p ∈ Weibull.domain,
        IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 a q.2) Weibull.domain p ↔
        p.1 ∈ S ∧ p.2 = (n : ℝ) / heldScaleRate d p.1 ∧
          IsMaxOn (heldScaleProfile d) S p.1 := by
  intro d W S
  obtain ⟨i₀, hi₀⟩ := habove
  obtain ⟨i, _, hix⟩ := Finset.exists_max_image Finset.univ x ⟨i₀, Finset.mem_univ _⟩
  have hmax (k : Fin n) : x k ≤ x i := hix k (Finset.mem_univ _)
  have hi : 0 < d i := log_pos ((one_lt_div ha).2 (hi₀.trans_le (hmax i₀)))
  obtain ⟨j, hj⟩ : ∃ j, x j < x i := by
    by_contra! h
    obtain ⟨j, k, hjk⟩ := hne
    exact hjk ((le_antisymm (hmax j) (h j)).trans (le_antisymm (hmax k) (h k)).symm)
  have hd : d j < d i := log_lt_log (div_pos (hx j) ha) (div_lt_div_of_pos_right hj ha)
  obtain ⟨lo, hlo, hlow⟩ := (nhdsGT_basis (0 : ℝ)).mem_iff.mp
    ((profile_left hn d).eventually (eventually_lt_atBot (heldScaleProfile d 1)))
  obtain ⟨hi, hhigh⟩ := eventually_atTop.1
    ((profile_right hn hi hd).eventually (eventually_lt_atBot (heldScaleProfile d 1)))
  let K := Icc lo hi
  have hsub : K ⊆ Ioi (0 : ℝ) := fun _ hb ↦ hlo.trans_le hb.1
  have hWsub : W ⊆ K := by
    intro b hb
    exact ⟨le_of_not_gt (fun h ↦ (hlow ⟨hb.1, h⟩).not_ge hb.2),
      le_of_not_gt (fun h ↦ (hhigh b h.le).not_ge hb.2)⟩
  have hW : IsCompact W := by
    have he : W = K ∩ heldScaleProfile d ⁻¹' Ici (heldScaleProfile d 1) :=
      le_antisymm (fun b hb ↦ ⟨hWsub hb, hb.2⟩) (fun b hb ↦ ⟨hsub hb.1, hb.2⟩)
    rw [he]
    exact ((profile_continuous hn d).mono hsub).upperSemicontinuousOn
      |>.isCompact_inter_preimage_Ici isCompact_Icc _
  have hS : S.Finite := stationary_finite hn d hW (fun _ h ↦ h.1)
  let L := fun q : ℝ × ℝ ↦ logLik x q.1 a q.2
  let op := fun b : ℝ ↦ (n : ℝ) / heldScaleRate d b
  have he (b : ℝ) := profile_bounds hn hx ha b
  have hm (p : ℝ × ℝ) (hp : p ∈ Weibull.domain) (hmp : IsMaxOn L Weibull.domain p) :
      p.1 ∈ S ∧ p.2 = op p.1 ∧ IsMaxOn (heldScaleProfile d) S p.1 := by
    have h₁ : IsMaxOn (fun c ↦ logLik x p.1 a c) (Ioi 0) p.2 :=
      fun c hc ↦ isMaxOn_iff.mp hmp (p.1, c) ⟨hp.1, hc⟩
    obtain ⟨_, _, hu⟩ := held_shape_scale_mle hn hx (b := p.1) ha
    have hp₂ := (hu p.2 ⟨hp.2, h₁⟩).trans
      (hu (op p.1) ⟨(he p.1).1, fun c hc ↦ (he p.1).2.2 c hc⟩).symm
    have hpm : IsMaxOn (heldScaleProfile d) (Ioi 0) p.1 := by
      intro b hb
      have h := isMaxOn_iff.mp hmp (b, op b) ⟨hb, (he b).1⟩
      change logLik x b a (op b) ≤ logLik x p.1 a p.2 at h
      rw [hp₂, (he b).2.1, (he p.1).2.1] at h
      exact (sub_le_sub_iff_right _).1 h
    have hs : p.1 ∈ S := ⟨⟨hp.1, hpm (show (1 : ℝ) ∈ Ioi 0 from one_pos)⟩,
      (hpm.isLocalMax (Ioi_mem_nhds hp.1)).deriv_eq_zero⟩
    exact ⟨hs, hp₂, fun _ ht ↦ hpm ht.1.1⟩
  obtain ⟨q, hq, hqmax⟩ := (held_scale_attains_iff hn hx ha).2 ⟨hne, i₀, hi₀⟩
  have hqm := hm q hq hqmax
  refine ⟨hW, hS, ⟨q.1, hqm.1⟩, fun p hp ↦ ⟨hm p hp, ?_⟩⟩
  rintro ⟨hpS, hp₂, hpmax⟩
  intro r hr
  have hcomp := isMaxOn_iff.mp hqmax r hr
  change L r ≤ L p
  apply hcomp.trans
  unfold L
  rw [hqm.2.1, hp₂, (he q.1).2.1, (he p.1).2.1]
  exact sub_le_sub_right (hpmax hqm.1) _

end

end UnivariateML.Shape2ExpWeibull

end
