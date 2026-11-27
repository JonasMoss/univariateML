module

public import UnivariateML.Family.Ged
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.Convex.SpecificFunctions.Pow
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv
public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.NumberTheory.Harmonic.GammaDeriv
public import Mathlib.Analysis.SpecialFunctions.Stirling
public import Mathlib.NumberTheory.Real.Irrational
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized error fits at held positive shape

The existing GED log likelihood uses public standard deviation. For shape
below one the finitely many distinct sample knots suffice even when the
sample is constant.

## Deviations

The knot ordering is a real-arithmetic result. The binary64 contact
classification and comparisons across free shapes require separate results.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPieceGED

noncomputable section

variable {n : ℕ}

private theorem gap_loss {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    {x : Fin n → ℝ} {l m r : ℝ} (hl : l < m) (hr : m < r)
    (hgap : ∀ i, x i ≤ l ∨ r ≤ x i) :
    min (Ged.residualSum ν x l) (Ged.residualSum ν x r) < Ged.residualSum ν x m := by
  let a := (r - m) / (r - l)
  let b := (m - l) / (r - l)
  have hd := sub_pos.mpr (hl.trans hr)
  have ha : 0 < a := div_pos (sub_pos.mpr hr) hd
  have hb : 0 < b := div_pos (sub_pos.mpr hl) hd
  have hab : a + b = 1 := by dsimp [a,b]; field_simp; ring
  have hm : a*l+b*r=m := by dsimp [a,b]; field_simp; ring
  have hterm (i : Fin n) :
      a * |x i-l|^ν + b * |x i-r|^ν < |x i-m|^ν := by
    have hne : |x i-l| ≠ |x i-r| := by
      rcases hgap i with h | h
      · rw [abs_of_nonpos (sub_nonpos.mpr h),
          abs_of_nonpos (sub_nonpos.mpr (h.trans (hl.trans hr).le))]
        intro he
        linarith
      · rw [abs_of_nonneg (sub_nonneg.mpr ((hl.trans hr).le.trans h)),
          abs_of_nonneg (sub_nonneg.mpr h)]
        intro he
        linarith
    have hblend : a * |x i-l| + b * |x i-r| = |x i-m| := by
      rcases hgap i with h | h
      · rw [abs_of_nonpos (sub_nonpos.mpr h),
          abs_of_nonpos (sub_nonpos.mpr (h.trans (hl.trans hr).le)),
          abs_of_nonpos (sub_nonpos.mpr (h.trans hl.le))]
        linear_combination hm - x i * hab
      · rw [abs_of_nonneg (sub_nonneg.mpr ((hl.trans hr).le.trans h)),
          abs_of_nonneg (sub_nonneg.mpr h),
          abs_of_nonneg (sub_nonneg.mpr (hr.le.trans h))]
        linear_combination x i * hab - hm
    have hc := (strictConcaveOn_rpow hν hν').2
      (show |x i-l| ∈ Ici 0 from abs_nonneg _) (show |x i-r| ∈ Ici 0 from abs_nonneg _)
      hne ha hb hab
    simp only [smul_eq_mul, hblend] at hc
    exact hc
  have hsum : a * Ged.residualSum ν x l + b * Ged.residualSum ν x r <
      Ged.residualSum ν x m := by
    unfold Ged.residualSum
    rw [Finset.mul_sum, Finset.mul_sum, ← Finset.sum_add_distrib]
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩ fun i _ ↦ hterm i
  have he := mul_add (min (Ged.residualSum ν x l) (Ged.residualSum ν x r)) a b
  nlinarith only [hsum, he, hab,
    mul_le_mul_of_nonneg_left (min_le_left (Ged.residualSum ν x l) (Ged.residualSum ν x r)) ha.le,
    mul_le_mul_of_nonneg_left (min_le_right (Ged.residualSum ν x l) (Ged.residualSum ν x r)) hb.le,
    congrArg (fun t : ℝ ↦ min (Ged.residualSum ν x l) (Ged.residualSum ν x r) * t) hab]

private theorem improving_knot {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    {x : Fin n → ℝ} {m : ℝ} (hm : ∀ i, x i ≠ m) :
    ∃ i, Ged.residualSum ν x (x i) < Ged.residualSum ν x m := by
  classical
  let k : Fin n := ⟨0, hn⟩
  have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨k, Finset.mem_univ _⟩
  by_cases hL : ∃ i, x i < m
  · by_cases hR : ∃ i, m < x i
    · obtain ⟨i, hi, him⟩ := Finset.exists_max_image (Finset.univ.filter fun i ↦ x i < m) x (by
        obtain ⟨i, hi⟩ := hL
        exact ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hi⟩⟩)
      obtain ⟨j, hj, hjm⟩ := Finset.exists_min_image (Finset.univ.filter fun j ↦ m < x j) x (by
        obtain ⟨j, hj⟩ := hR
        exact ⟨j, Finset.mem_filter.mpr ⟨Finset.mem_univ _, hj⟩⟩)
      have hgap (k : Fin n) : x k ≤ x i ∨ x j ≤ x k := by
        rcases lt_or_gt_of_ne (hm k) with hk | hk
        · exact Or.inl (him k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
        · exact Or.inr (hjm k (Finset.mem_filter.mpr ⟨Finset.mem_univ _, hk⟩))
      have hh := gap_loss hν hν' hn (Finset.mem_filter.mp hi).2 (Finset.mem_filter.mp hj).2 hgap
      rcases le_total (Ged.residualSum ν x (x i)) (Ged.residualSum ν x (x j)) with he | he
      · exact ⟨i, by rwa [min_eq_left he] at hh⟩
      · exact ⟨j, by rwa [min_eq_right he] at hh⟩
    · obtain ⟨i, _, hi⟩ := Finset.exists_max_image Finset.univ x hne
      have hik : x i < m := lt_of_le_of_ne (le_of_not_gt (fun h ↦ hR ⟨i,h⟩)) (hm i)
      refine ⟨i, Finset.sum_lt_sum_of_nonempty hne (fun j _ ↦ ?_)⟩
      have hj := hi j (Finset.mem_univ j)
      rw [abs_of_nonpos (sub_nonpos.mpr hj), abs_of_nonpos (sub_nonpos.mpr (hj.trans hik.le))]
      exact rpow_lt_rpow (by linarith) (by linarith) hν
  · obtain ⟨i, _, hi⟩ := Finset.exists_min_image Finset.univ x hne
    have hik : m < x i := lt_of_le_of_ne (le_of_not_gt (fun h ↦ hL ⟨i,h⟩)) (Ne.symm (hm i))
    refine ⟨i, Finset.sum_lt_sum_of_nonempty hne (fun j _ ↦ ?_)⟩
    have hj := hi j (Finset.mem_univ j)
    rw [abs_of_nonneg (sub_nonneg.mpr hj), abs_of_nonneg (sub_nonneg.mpr (hik.le.trans hj))]
    exact rpow_lt_rpow (sub_nonneg.mpr hj) (by linarith) hν

/-- At a positive shape below one, the location minimizers are exactly
the knots whose loss is smallest among the knots. -/
theorem minimizingKnots {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    (x : Fin n → ℝ) (m : ℝ) :
    IsMinOn (Ged.residualSum ν x) univ m ↔
      ∃ i, m = x i ∧ ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j) := by
  constructor
  · intro h
    by_cases he : ∃ i, m = x i
    · obtain ⟨i, rfl⟩ := he
      exact ⟨i, rfl, fun j ↦ h (mem_univ (x j))⟩
    · obtain ⟨i, hi⟩ := improving_knot hν hν' hn (fun i h ↦ he ⟨i, h.symm⟩)
      exact (hi.not_ge (h (mem_univ (x i)))).elim
  · rintro ⟨i, rfl, hi⟩ m _
    by_cases he : ∃ j, m = x j
    · obtain ⟨j,rfl⟩ := he
      exact hi j
    · obtain ⟨j,hj⟩ := improving_knot hν hν' hn (fun j h ↦ he ⟨j,h.symm⟩)
      exact (hi j).trans hj.le

/-- At positive fixed sd and shape below one, the knots of least power
loss are precisely all the location maximizers. -/
theorem heldSd_belowOne {ν σ : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hσ : 0 < σ)
    (hn : 0 < n) (x : Fin n → ℝ) (m : ℝ) :
    IsMaxOn (fun t ↦ Ged.logLik ν x (t, σ)) univ m ↔
      ∃ i, m = x i ∧ ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j) :=
  (Ged.heldScale ν x hν hσ).trans (minimizingKnots hν hν' hn x m)

/-- With shape below one and free scale, every maximizing knot shares
the same profiled scale. The sample is nonconstant so that scale is positive. -/
theorem heldShape_belowOne {ν : ℝ} (hν : 0 < ν) (hν' : ν < 1) (hn : 0 < n)
    {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    Ged.jointMaximizers ν x =
      {p | (∃ i, p.1 = x i ∧
        ∀ j, Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x (x j)) ∧
        p.2 = Ged.bestScale ν x p.1} := by
  rw [Ged.joint_graph hν hn hx]
  ext p
  exact and_congr_left' (minimizingKnots hν hν' hn x p.1)

/-- With positive sd and shape held, a maximizing location exists for every
finite nonempty sample. This includes constant data and all subunit shapes. -/
theorem heldSd_attains {ν σ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) (hn : 0 < n)
    (x : Fin n → ℝ) : ∃ m, IsMaxOn (fun t ↦ Ged.logLik ν x (t, σ)) univ m := by
  let i : Fin n := ⟨0, hn⟩
  by_cases hν' : ν < 1
  · obtain ⟨k, _, hk⟩ := Finset.exists_min_image Finset.univ
      (fun j ↦ Ged.residualSum ν x (x j)) ⟨i, Finset.mem_univ _⟩
    exact ⟨x k, (heldSd_belowOne hν hν' hσ hn x _).mpr
      ⟨k, rfl, fun j ↦ hk j (Finset.mem_univ _)⟩⟩
  · by_cases hx : LocationScale.Nonconstant x
    · obtain ⟨⟨m, s⟩, hs, hm⟩ := Ged.joint_exists (le_of_not_gt hν') hx
      apply Exists.intro m
      apply (Ged.heldScale ν x hν hσ).mpr
      apply (Ged.heldScale ν x hν (show 0 < s from hs.2)).mp
      exact fun t _ ↦ hm (show (t, s) ∈ LocationScale.domain from ⟨mem_univ _, hs.2⟩)
    · have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j,i,h⟩)
      refine ⟨x i, (Ged.heldScale ν x hν hσ).mpr (fun t _ ↦ ?_)⟩
      change Ged.residualSum ν x (x i) ≤ Ged.residualSum ν x t
      rw [show Ged.residualSum ν x (x i) = 0 by simp [Ged.residualSum, he, zero_rpow hν.ne']]
      exact Finset.sum_nonneg (fun _ _ ↦ rpow_nonneg (abs_nonneg _) _)

private theorem sdFactor_pos {ν : ℝ} (hν : 0 < ν) : 0 < Ged.sdFactor ν :=
  sqrt_pos.2 (div_pos (Gamma_pos_of_pos (div_pos (by norm_num) hν))
    (Gamma_pos_of_pos (div_pos one_pos hν)))

/-- At positive fixed sd and shape the log likelihood vanishes to
minus infinity at either location end. -/
theorem heldSd_tail {ν σ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) (hn : 0 < n)
    (x : Fin n → ℝ) :
    Tendsto (fun μ ↦ Ged.logLik ν x (μ, σ)) (cocompact ℝ) atBot := by
  let i : Fin n := ⟨0, hn⟩
  let C := (n : ℝ) * (-log σ + Ged.logNormalizer ν)
  let a := (Ged.sdFactor ν / σ)^ν
  have ha : 0 < a := rpow_pos_of_pos (div_pos (sdFactor_pos hν) hσ) _
  have hval (μ : ℝ) : Ged.logLik ν x (μ, σ) = C - a * Ged.residualSum ν x μ := by
    unfold Ged.logLik LocationScale.logLik Ged.loss C a Ged.residualSum
    simp only [abs_div, abs_of_pos hσ, div_rpow (abs_nonneg _) hσ.le,
      div_rpow (sdFactor_pos hν).le hσ.le, Finset.sum_sub_distrib, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, Finset.mul_sum]
    rw [sub_add_eq_add_sub, mul_add]
    congr 1
    exact Finset.sum_congr rfl (fun j _ ↦ by ring)
  have hbound (μ : ℝ) : Ged.logLik ν x (μ, σ) ≤ C - a * |x i-μ|^ν := by
    rw [hval]
    apply sub_le_sub_left
    apply mul_le_mul_of_nonneg_left _ ha.le
    exact Finset.single_le_sum (f := fun j ↦ |x j-μ|^ν)
      (fun _ _ ↦ rpow_nonneg (abs_nonneg _) _) (Finset.mem_univ i)
  have hpos : Tendsto (fun μ : ℝ ↦ x i-μ) atTop atBot := by
    simpa only [sub_eq_add_neg, add_comm (x i)] using
      tendsto_atBot_add_const_right atTop (x i) tendsto_neg_atTop_atBot
  have hneg : Tendsto (fun μ : ℝ ↦ x i-μ) atBot atTop := by
    simpa only [sub_eq_add_neg, add_comm (x i)] using
      tendsto_atTop_add_const_right atBot (x i) tendsto_neg_atBot_atTop
  rw [cocompact_eq_atBot_atTop, tendsto_sup]
  have finish {F : Filter ℝ} (h : Tendsto (fun μ : ℝ ↦ |x i-μ|) F atTop) :
      Tendsto (fun μ ↦ Ged.logLik ν x (μ, σ)) F atBot := by
    have ht := ((tendsto_rpow_atTop hν).comp h).const_mul_atTop ha
    apply tendsto_atBot_mono hbound
    simpa only [sub_eq_add_neg, add_comm C, Function.comp_def] using
      tendsto_atBot_add_const_right F C (tendsto_neg_atTop_atBot.comp ht)
  exact ⟨finish (tendsto_abs_atTop_atTop.comp hneg), finish (tendsto_abs_atBot_atTop.comp hpos)⟩

/-- A rational radius and a positive rational sd cannot meet the exact
real uniform contact condition. Binary64 inputs are a special case. -/
theorem rational_noContact (r σ : ℚ) (hσ : σ ≠ 0) :
    (r : ℝ) ≠ sqrt 3 * (σ : ℝ) := by
  intro h
  have hi : Irrational (sqrt (3 : ℝ)) := by
    simpa using (show Nat.Prime 3 by norm_num).irrational_sqrt
  have hs : (σ : ℝ) ≠ 0 := by exact_mod_cast hσ
  apply hi
  refine ⟨r/σ, ?_⟩
  push_cast
  exact (div_eq_iff hs).mpr h

/-- GED log likelihood in kernel scale, before conversion to sd. -/
def kernelLogLik (ν : ℝ) (x : Fin n → ℝ) (m b : ℝ) : ℝ :=
  -(n : ℝ) * log (2 * b * Gamma (1 + 1/ν)) - ∑ i, (|x i-m| / b)^ν

/-- The positive kernel-scale parametrization is exactly the GED model. -/
theorem kernelLogLik_eq {ν b : ℝ} (hν : 0 < ν) (hb : 0 < b)
    (x : Fin n → ℝ) (m : ℝ) :
    kernelLogLik ν x m b = Ged.logLik ν x (m, b * Ged.sdFactor ν) := by
  let g := Ged.sdFactor ν
  let G := Gamma (1 + 1/ν)
  have hg : 0 < g := sdFactor_pos hν
  have hG : 0 < G := Gamma_pos_of_pos (by have := div_pos one_pos hν; linarith)
  have hs := mul_pos hb hg
  have hterm (i : Fin n) :
      Ged.loss ν ((x i-m) / (b*g)) = (|x i-m| / b)^ν := by
    unfold Ged.loss
    rw [abs_div, abs_of_pos hs, ← mul_rpow hg.le (div_nonneg (abs_nonneg _) hs.le)]
    congr 1
    field_simp [hb.ne', hg.ne']
  change -(n : ℝ) * log (2 * b * G) - ∑ i, (|x i-m| / b)^ν =
    (∑ i, (-log (b*g) - Ged.loss ν ((x i-m)/(b*g)))) +
      n * (-log (2*G/g))
  simp only [hterm, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  rw [log_mul (mul_pos (by norm_num) hb).ne' hG.ne',
    log_mul (by norm_num : (2 : ℝ) ≠ 0) hb.ne', log_mul hb.ne' hg.ne',
    log_div (mul_pos (by norm_num) hG).ne' hg.ne',
    log_mul (by norm_num : (2 : ℝ) ≠ 0) hG.ne']
  ring

private theorem gamma_one_add_div_limit (k : ℝ) :
    Tendsto (fun p : ℝ ↦ Gamma (1 + k/p)) atTop (𝓝 1) := by
  have hi : Tendsto (fun p : ℝ ↦ k/p) atTop (𝓝 (0 : ℝ)) := by
    simpa only [div_eq_mul_inv, mul_zero] using tendsto_inv_atTop_zero.const_mul k
  have hu : Tendsto (fun p : ℝ ↦ (1 : ℝ) + k/p) atTop (𝓝 1) := by
    simpa only [add_zero] using tendsto_const_nhds.add hi
  have hg : ContinuousAt Gamma (1 : ℝ) :=
    (differentiableAt_Gamma (s := 1) (by intro k; linarith [Nat.cast_nonneg (α := ℝ) k])).continuousAt
  simpa only [Function.comp_def, Gamma_one] using hg.tendsto.comp hu

private theorem sdFactor_limit : Tendsto Ged.sdFactor atTop (𝓝 (1 / sqrt 3)) := by
  have hg : Tendsto (fun p : ℝ ↦ Gamma (1 + 3/p) / (3 * Gamma (1 + 1/p))) atTop (𝓝 (1/3)) := by
    have ht : Tendsto (fun p : ℝ ↦ Gamma (1 + 3/p) / (3 * Gamma (1 + 1/p))) atTop
        (𝓝 (1/(3*1))) :=
      (gamma_one_add_div_limit 3).div ((gamma_one_add_div_limit 1).const_mul 3) (by norm_num)
    simpa only [mul_one] using ht
  have h := continuous_sqrt.continuousAt.tendsto.comp hg
  rw [sqrt_div (by norm_num : (0 : ℝ) ≤ 1), sqrt_one] at h
  apply h.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
  simp only [Function.comp_def, Ged.sdFactor]
  congr 1
  have h₁ : 0 < 1/p := div_pos one_pos hp
  have h₃ : 0 < 3/p := div_pos (by norm_num) hp
  rw [add_comm (1 : ℝ) (3/p), add_comm (1 : ℝ) (1/p),
    Gamma_add_one h₃.ne', Gamma_add_one h₁.ne']
  field_simp [hp.ne', (Gamma_pos_of_pos h₁).ne']

/-- A vanishing relative widening approaches the unpenalized uniform
value, even with observations exactly at either endpoint of the limiting
interval. The fitted sd tends to the sd of that interval. -/
theorem uniform_path_freeSd {x : Fin n → ℝ} {μ ρ : ℝ}
    (hρ : 0 < ρ) (h : ∀ i, |x i-μ| ≤ ρ) :
    let scale := fun p : ℝ ↦ (ρ * p^(1/p)) * Ged.sdFactor p
    Tendsto scale atTop (𝓝 (ρ / sqrt 3)) ∧
      Tendsto (fun p ↦ Ged.logLik p x (μ, scale p)) atTop (𝓝 (-(n : ℝ) * log (2*ρ))) := by
  intro scale
  let b (p : ℝ) := ρ * p^(1/p)
  have hb : Tendsto b atTop (𝓝 ρ) := by
    simpa only [b, mul_one] using tendsto_rpow_div.const_mul ρ
  refine ⟨by simpa only [scale, b, mul_one_div] using hb.mul sdFactor_limit, ?_⟩
  have hnorm : Tendsto (fun p ↦ 2 * b p * Gamma (1 + 1/p)) atTop (𝓝 (2*ρ)) := by
    simpa only [mul_one] using (hb.const_mul 2).mul (gamma_one_add_div_limit 1)
  have hlog := (continuousAt_log (mul_ne_zero (by norm_num) hρ.ne')).tendsto.comp hnorm
  have hs : Tendsto (fun p ↦ ∑ i, (|x i-μ| / b p)^p) atTop (𝓝 (0 : ℝ)) := by
    have hu : Tendsto (fun p : ℝ ↦ (n : ℝ) / p) atTop (𝓝 (0 : ℝ)) := by
      simpa only [div_eq_mul_inv, mul_zero] using tendsto_inv_atTop_zero.const_mul (n : ℝ)
    apply squeeze_zero' _ _ hu
    · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
      have hp' : 0 < b p := mul_pos hρ (rpow_pos_of_pos hp _)
      exact Finset.sum_nonneg (fun _ _ ↦ rpow_nonneg (div_nonneg (abs_nonneg _) hp'.le) _)
    · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
      have hp' : 0 < b p := mul_pos hρ (rpow_pos_of_pos hp _)
      have hc : (ρ/b p)^p = 1/p := by
        have he : ρ/b p = 1 / p^(1/p) := by
          dsimp [b]
          field_simp [hρ.ne', (rpow_pos_of_pos hp (1/p)).ne']
        rw [he, div_rpow zero_le_one (rpow_nonneg hp.le _), one_rpow,
          ← rpow_mul hp.le, one_div_mul_cancel hp.ne', rpow_one]
      calc
        _ ≤ ∑ _i : Fin n, (1 : ℝ)/p := by
          apply Finset.sum_le_sum
          intro i _
          rw [← hc]
          exact rpow_le_rpow (div_nonneg (abs_nonneg _) hp'.le)
            (div_le_div_of_nonneg_right (h i) hp'.le) hp.le
        _ = (n : ℝ)/p := by simp [div_eq_mul_inv]

  have ht := (hlog.const_mul (-(n : ℝ))).sub hs
  simp only [Function.comp_def, sub_zero] at ht
  apply ht.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
  exact kernelLogLik_eq hp (mul_pos hρ (rpow_pos_of_pos hp _)) x μ

/-- If shape and both location and sd are free, allowing positive shapes
below one makes the log likelihood unbounded on every nonempty sample.
The construction uses reciprocal integer shapes at a sample atom. -/
theorem unrestricted_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ¬ BddAbove {L : ℝ | ∃ ν μ σ : ℝ, 0 < ν ∧ 0 < σ ∧ L = Ged.logLik ν x (μ, σ)} := by
  rintro ⟨level,hlevel⟩
  let i : Fin n := ⟨0, hn⟩
  let m := x i
  let R := 1 + ∑ j, |x j-m|
  have hN : (1 : ℝ) ≤ n := by exact_mod_cast hn
  have hn' : (0 : ℝ) < n := by linarith
  have hR : 0 < R := add_pos_of_pos_of_nonneg one_pos
    (Finset.sum_nonneg fun _ _ ↦ abs_nonneg _)
  have hres (j : Fin n) : |x j-m| ≤ R := by
    have := Finset.single_le_sum (f := fun j ↦ |x j-m|)
      (fun _ _ ↦ abs_nonneg _) (Finset.mem_univ j)
    unfold R
    linarith
  let q := ((n : ℝ) - 1/2) / n
  have hq : 0 < q := div_pos (by linarith) hn'
  have hq' : q < 1 := (div_lt_iff₀ hn').2 (by linarith)
  have hqlog : log q < 0 := log_neg hq hq'
  let δ := -log q / 2
  have hδ : 0 < δ := by unfold δ; linarith
  let C := -(n : ℝ) * (log (2*R) + log (Stirling.stirlingSeq 1)) +
      (n : ℝ)/2 * (1 + log δ)
  let A := -(n : ℝ)/2 * log q
  have hA : 0 < A := by unfold A; nlinarith [mul_pos hn' (neg_pos.mpr hqlog)]
  obtain ⟨j,hj⟩ := exists_nat_gt ((level-C)/A)
  let k := j+1
  let t := (k : ℝ)
  let ν := t⁻¹
  let b := R * (q/t)^k
  have ht : 0 < t := by unfold t k; positivity
  have hν : 0 < ν := inv_pos.mpr ht
  have hqt := div_pos hq ht
  have hb : 0 < b := mul_pos hR (pow_pos hqt _)
  have hpow : ((q/t)^k)^ν = q/t := by
    rw [← rpow_natCast, ← rpow_mul hqt.le]
    change (q/t)^(t*t⁻¹) = q/t
    rw [mul_inv_cancel₀ ht.ne', rpow_one]
  have hpen (l : Fin n) : (|x l-m|/b)^ν ≤ t/q := by
    rw [show |x l-m|/b = (|x l-m|/R)/((q/t)^k) by rw [div_div],
      div_rpow (div_nonneg (abs_nonneg _) hR.le) (pow_nonneg hqt.le _), hpow]
    have hle : (|x l-m|/R)^ν ≤ 1 := by
      rw [← one_rpow ν]
      exact rpow_le_rpow (div_nonneg (abs_nonneg _) hR.le) ((div_le_one hR).2 (hres l)) hν.le
    exact (div_le_div_of_nonneg_right hle hqt.le).trans_eq (one_div_div _ _)
  have hzero : (|x i-m|/b)^ν = 0 := by simp [m, zero_rpow hν.ne']
  have hsum : ∑ l, (|x l-m|/b)^ν ≤ (n : ℝ)*t := by
    have h₁ : ∑ l, (|x l-m|/b)^ν ≤ ∑ _l ∈ Finset.univ.erase i, t/q := by
      rw [← Finset.sum_erase_add _ _ (Finset.mem_univ i), hzero, add_zero]
      exact Finset.sum_le_sum fun l _ ↦ hpen l
    have hcard : ((Finset.univ.erase i).card : ℝ) = (n : ℝ)-1 := by
      rw [Finset.card_erase_of_mem (Finset.mem_univ i), Finset.card_univ, Fintype.card_fin,
        Nat.cast_sub (by omega : 1 ≤ n), Nat.cast_one]
    simp only [Finset.sum_const, nsmul_eq_mul, hcard] at h₁
    apply h₁.trans
    have hqeq : (n : ℝ)*q = n - 1/2 := by unfold q; field_simp
    rw [← mul_div_assoc, div_le_iff₀ hq]
    nlinarith only [congrArg (fun s : ℝ ↦ s*t) hqeq, ht]
  have hΓ : Gamma (1+1/ν) = k.factorial := by
    rw [show 1/ν = t by simp [ν], add_comm]
    exact Gamma_nat_eq_factorial k
  have hS : log (Stirling.stirlingSeq k) ≤ log (Stirling.stirlingSeq 1) :=
    Stirling.log_stirlingSeq'_antitone (show (0 : ℕ) ≤ j from Nat.zero_le _)
  have hSt := Stirling.log_stirlingSeq_formula k
  have hnorm : log (2*b*Gamma (1+1/ν)) =
      log (2*R) + log (Stirling.stirlingSeq k) + (1/2)*log (2*t) +
        t*(log q - 1) := by
    rw [hΓ, log_mul (mul_pos (by norm_num) hb).ne' (by positivity),
      show 2*b = (2*R)*(q/t)^k by unfold b; ring,
      log_mul (mul_pos (by norm_num) hR).ne' (pow_pos hqt _).ne', log_pow,
      log_div hq.ne' ht.ne']
    change log (Stirling.stirlingSeq k) = log (k.factorial : ℝ) -
      1/2*log (2*t) - t*log (t/exp 1) at hSt
    rw [log_div ht.ne' (exp_pos _).ne', log_exp] at hSt
    change _ + t*(log q - log t) + _ = _
    nlinarith only [hSt]
  have hlog := log_le_sub_one_of_pos (mul_pos hδ (mul_pos (by norm_num : (0 : ℝ) < 2) ht))
  rw [log_mul hδ.ne' (mul_pos (by norm_num) ht).ne'] at hlog
  have htineq : log (2*t) ≤ (-log q)*t - (1+log δ) := by
    dsimp [δ] at *
    nlinarith only [hlog]
  have hscore : C + A*t ≤ kernelLogLik ν x m b := by
    unfold C A kernelLogLik
    rw [hnorm]
    have hlog' := mul_le_mul_of_nonneg_left htineq (show 0 ≤ (n : ℝ)/2 by positivity)
    have hSt' := mul_le_mul_of_nonneg_left hS hn'.le
    nlinarith only [hsum, hlog', hSt']
  have hlarge : level < C+A*t := by
    have hj' := (div_lt_iff₀ hA).mp hj
    have ht' : (j : ℝ) < t := by simp [t,k]
    linarith [mul_lt_mul_of_pos_left ht' hA]
  have hv := hlevel (show Ged.logLik ν x (m,b*Ged.sdFactor ν) ∈ _ from
    ⟨ν, m, b*Ged.sdFactor ν, hν, mul_pos hb (sdFactor_pos hν), rfl⟩)
  exact (hlarge.trans_le hscore).not_ge ((kernelLogLik_eq hν hb x m).symm ▸ hv)

/-- The shape cutoff from the uniform comparison with free sd. -/
def uniformCutoff (n : ℕ) : ℝ := max 1 (n * exp (eulerMascheroniConstant - 1))

private theorem logGamma_tangent {t : ℝ} (ht : 0 < t) :
    -eulerMascheroniConstant * t ≤ log (Gamma (1+t)) := by
  have hΓ : HasDerivAt (log ∘ Gamma) (-eulerMascheroniConstant) 1 := by
    simpa only [Gamma_one, div_one, Function.comp_def] using
      hasDerivAt_Gamma_one.log (by norm_num [Gamma_one])
  have hpos : (1+t) ∈ Ioi (0 : ℝ) := by change 0 < 1+t; linarith
  have hlt : (1 : ℝ) < 1+t := by linarith
  have h := convexOn_log_Gamma.le_slope_of_hasDerivAt one_pos hpos hlt hΓ
  simp only [slope_def_field, Function.comp_def, Gamma_one, log_one, sub_zero,
    add_sub_cancel_left] at h
  exact (le_div_iff₀ ht).mp h

/-- Beyond the compact cutoff, a positive unavoidable radius forces
every finite likelihood below the unpenalized uniform. This bound is
uniform over the positive kernel scale and the location. -/
theorem uniform_tail_excluded (hn : 0 < n) {x : Fin n → ℝ} {μ ρ p b : ℝ}
    (hρ : 0 < ρ) (hb : 0 < b) (hp : uniformCutoff n < p)
    (hx : ∃ i, ρ ≤ |x i-μ|) :
    kernelLogLik p x μ b < -(n : ℝ) * log (2*ρ) := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hP1 : 1 ≤ uniformCutoff n := le_max_left _ _
  have hp' := zero_lt_one.trans (hP1.trans_lt hp)
  let k := (n : ℝ)/p
  let y := (ρ/b)^p
  let G := Gamma (1+1/p)
  have hk : 0 < k := div_pos hN hp'
  have hy : 0 < y := rpow_pos_of_pos (div_pos hρ hb) _
  have hG : 0 < G := Gamma_pos_of_pos (by have := div_pos one_pos hp'; linarith)
  obtain ⟨i, hi⟩ := hx
  have hpen : y ≤ ∑ j, (|x j-μ|/b)^p := by
    apply le_trans _ (Finset.single_le_sum (f := fun j ↦ (|x j-μ|/b)^p)
      (fun _ _ ↦ rpow_nonneg (div_nonneg (abs_nonneg _) hb.le) _) (Finset.mem_univ i))
    exact rpow_le_rpow (div_nonneg hρ.le hb.le) (div_le_div_of_nonneg_right hi hb.le) hp'.le
  have hlogy : log y = p * (log ρ - log b) := by
    unfold y
    rw [log_rpow (div_pos hρ hb), log_div hρ.ne' hb.ne']
  have hlogk : log k = log n - log p := log_div hN.ne' hp'.ne'
  have hky := log_le_sub_one_of_pos (div_pos hy hk)
  rw [log_div hy.ne' hk.ne'] at hky
  have hmul := mul_le_mul_of_nonneg_left hky hk.le
  have hmul' : k * (y/k - 1) = y-k := by field_simp
  have hopt : k*log y - y ≤ k*(log k-1) := by nlinarith only [hmul, hmul']
  have hgamm := mul_le_mul_of_nonneg_left (logGamma_tangent (div_pos one_pos hp')) hN.le
  have hscale : k*p = n := by dsimp [k]; field_simp
  have hlog := log_lt_log (mul_pos hN (exp_pos (eulerMascheroniConstant - 1)))
    ((le_max_right (1 : ℝ) _).trans_lt hp)
  rw [log_mul hN.ne' (exp_pos _).ne', log_exp] at hlog
  have hsigned := mul_neg_of_pos_of_neg hk
    (show log k + eulerMascheroniConstant - 1 < 0 by linarith [hlogk])
  have hnorm : log (2*b*G) = log (2*ρ) + log G - (log ρ-log b) := by
    rw [log_mul (mul_pos (by norm_num) hb).ne' hG.ne',
      log_mul (by norm_num : (2 : ℝ) ≠ 0) hb.ne',
      log_mul (by norm_num : (2 : ℝ) ≠ 0) hρ.ne']
    ring
  change -(n : ℝ)*log (2*b*G) - ∑ j, (|x j-μ|/b)^p < _
  rw [hnorm]
  have h₁ := congrArg (fun s : ℝ ↦ k*s) hlogy
  have h₂ := congrArg (fun s : ℝ ↦ s*(log ρ-log b)) hscale
  have h₃ : (n : ℝ) * (-eulerMascheroniConstant * (1/p)) = -k*eulerMascheroniConstant := by
    dsimp [k]
    ring
  change (n : ℝ) * (-eulerMascheroniConstant * (1/p)) ≤ n * log G at hgamm
  rw [h₃] at hgamm
  nlinarith only [hpen, hopt, hsigned, hgamm, h₁, h₂]

private def bestKernelScale (x : Fin n → ℝ) (p μ : ℝ) : ℝ :=
  (p * Ged.residualSum p x μ / n) ^ p⁻¹

/-- Log likelihood profiled over kernel scale for each shape and location. -/
def freeSdProfile (x : Fin n → ℝ) (y : ℝ × ℝ) : ℝ :=
  Ged.logLik y.1 x (y.2, Ged.bestScale y.1 x y.2)

private theorem bestKernel_pos (hn : 0 < n) {x : Fin n → ℝ}
    {p μ : ℝ} (hp : 0 < p) (hx : ∃ i, x i ≠ μ) : 0 < bestKernelScale x p μ := by
  obtain ⟨i,hi⟩ := hx
  have hS : 0 < Ged.residualSum p x μ :=
    (rpow_pos_of_pos (abs_pos.mpr (sub_ne_zero.mpr hi)) p).trans_le
      (Finset.single_le_sum (f := fun j ↦ |x j-μ|^p)
        (fun _ _ ↦ rpow_nonneg (abs_nonneg _) _) (Finset.mem_univ i))
  exact rpow_pos_of_pos (div_pos (mul_pos hp hS) (by exact_mod_cast hn)) _

private theorem freeSdProfile_scales (hn : 0 < n) {x : Fin n → ℝ} {p μ : ℝ}
    (hp : 0 < p) (hx : ∃ i, x i ≠ μ) :
    let b := bestKernelScale x p μ
    0 < b ∧ freeSdProfile x (p,μ) = kernelLogLik p x μ b ∧
      ∀ t > 0, kernelLogLik p x μ t ≤ freeSdProfile x (p,μ) := by
  intro b
  have hb := bestKernel_pos hn hp hx
  have h := Ged.heldLocation hp hn hx
  have he : b * Ged.sdFactor p = Ged.bestScale p x μ := by
    unfold b bestKernelScale Ged.bestScale
    ring
  refine ⟨hb, ?_, ?_⟩
  · rw [kernelLogLik_eq hp hb, he]
    rfl
  · intro t ht
    rw [kernelLogLik_eq hp ht]
    exact ((h.2 _ h.1).mpr rfl) (show t * Ged.sdFactor p ∈ Ioi 0 from
      mul_pos ht (sdFactor_pos hp))

private theorem freeSdProfile_continuousAt (hn : 0 < n) {x : Fin n → ℝ}
    {y : ℝ × ℝ} (hp : 0 < y.1) (hx : ∃ i, x i ≠ y.2) :
    ContinuousAt (fun z : ℝ × ℝ ↦ kernelLogLik z.1 x z.2 (bestKernelScale x z.1 z.2)) y := by
  have hp' : ContinuousAt (fun z : ℝ × ℝ ↦ z.1⁻¹) y :=
    continuous_fst.continuousAt.inv₀ hp.ne'
  have hpower (i : Fin n) : ContinuousAt (fun z : ℝ × ℝ ↦ |x i-z.2|^z.1) y :=
    ((continuousAt_const.sub continuous_snd.continuousAt).abs.rpow
      continuous_fst.continuousAt (Or.inr hp))
  have hS (s : Finset (Fin n)) : ContinuousAt (fun z : ℝ × ℝ ↦ ∑ i ∈ s, |x i-z.2|^z.1) y := by
    classical
    induction s using Finset.induction_on with
    | empty => simpa only [Finset.sum_empty] using (continuousAt_const (y := (0 : ℝ)) (x := y))
    | insert i s hi hs =>
      simp_rw [Finset.sum_insert hi]
      exact (hpower i).add hs
  have hb : ContinuousAt (fun z : ℝ × ℝ ↦ bestKernelScale x z.1 z.2) y :=
    ((continuous_fst.continuousAt.mul (hS Finset.univ)).div_const _).rpow hp'
      (Or.inr (inv_pos.mpr hp))
  have hbpos := bestKernel_pos hn hp hx
  have hG (p : ℝ) (hp : 0 < p) :
      ContinuousAt Gamma (1+1/p) := (differentiableAt_Gamma (by
        intro k
        have hh := div_pos one_pos hp
        linarith [Nat.cast_nonneg (α := ℝ) k])).continuousAt
  have ht : ContinuousAt (fun z : ℝ × ℝ ↦ (1 : ℝ) + 1/z.1) y :=
    continuousAt_const.add (continuousAt_const.div continuous_fst.continuousAt hp.ne')
  have hGc : ContinuousAt (fun z : ℝ × ℝ ↦ Gamma (1+1/z.1)) y :=
    (hG y.1 hp).comp (f := fun z : ℝ × ℝ ↦ 1+1/z.1) ht
  have hGpos := Gamma_pos_of_pos (show 0 < 1+1/y.1 by have := div_pos one_pos hp; linarith)
  have hnorm := ((continuousAt_const.mul hb).mul hGc).log
    (mul_pos (mul_pos (by norm_num : (0 : ℝ) < 2) hbpos) hGpos).ne'
  have hpen (i : Fin n) :
      ContinuousAt (fun z : ℝ × ℝ ↦ (|x i-z.2| / bestKernelScale x z.1 z.2)^z.1) y := by
    exact (((continuousAt_const.sub continuous_snd.continuousAt).abs.div hb hbpos.ne').rpow
      continuous_fst.continuousAt (Or.inr hp))
  have hsum (s : Finset (Fin n)) :
      ContinuousAt (fun z : ℝ × ℝ ↦ ∑ i ∈ s, (|x i-z.2| / bestKernelScale x z.1 z.2)^z.1) y := by
    classical
    induction s using Finset.induction_on with
    | empty => simpa only [Finset.sum_empty] using (continuousAt_const (y := (0 : ℝ)) (x := y))
    | insert i s hi hs =>
      simp_rw [Finset.sum_insert hi]
      exact (hpen i).add hs
  unfold kernelLogLik
  exact (continuousAt_const.mul hnorm).sub (hsum Finset.univ)

/-- Log likelihood values for fitted shapes and positive sd with location
restricted to a specified set. -/
def freeSdValues (x : Fin n → ℝ) (S : Set ℝ) : Set ℝ :=
  {L | ∃ p μ b : ℝ, 1 ≤ p ∧ μ ∈ S ∧ 0 < b ∧ L = kernelLogLik p x μ b}

/-- The compact finite-shape profile decides attainment by comparison with
the range uniform. The hypotheses express that the positive radius is
unavoidable for the location set and encloses the data at some location. -/
theorem freeSd_compactCriterion (hn : 0 < n) {x : Fin n → ℝ} {S : Set ℝ}
    (hS : IsCompact S) {ρ : ℝ} (hρ : 0 < ρ)
    (hlower : ∀ μ ∈ S, ∃ i, ρ ≤ |x i-μ|)
    (hupper : ∃ μ ∈ S, ∀ i, |x i-μ| ≤ ρ) :
    let K := Icc 1 (uniformCutoff n) ×ˢ S
    let F := -(n : ℝ)*log (2*ρ)
    ∃ y ∈ K, IsMaxOn (freeSdProfile x) K y ∧
      ((∃ L, IsGreatest (freeSdValues x S) L) ↔ F ≤ freeSdProfile x y) ∧
      (freeSdProfile x y < F → IsLUB (freeSdValues x S) F) := by
  intro K F
  obtain ⟨μ₀,hμ₀,hcover⟩ := hupper
  have hK : IsCompact K := isCompact_Icc.prod hS
  have h1 : (1,μ₀) ∈ K := ⟨⟨le_rfl, le_max_left _ _⟩,hμ₀⟩
  have hne {μ : ℝ} (hμ : μ ∈ S) : ∃ i, x i ≠ μ := by
    obtain ⟨i,hi⟩ := hlower μ hμ
    exact ⟨i, sub_ne_zero.mp (abs_pos.mp (hρ.trans_le hi))⟩
  have hcont : ContinuousOn (freeSdProfile x) K := by
    intro z hz
    have hp : 0 < z.1 := zero_lt_one.trans_le hz.1.1
    refine (freeSdProfile_continuousAt hn hp (hne hz.2)).continuousWithinAt.congr ?_
      (freeSdProfile_scales hn hp (hne hz.2)).2.1
    intro w hw
    have hp' : 0 < w.1 := zero_lt_one.trans_le hw.1.1
    exact (freeSdProfile_scales hn hp' (hne hw.2)).2.1
  obtain ⟨y,hy,hm⟩ := hK.exists_isMaxOn ⟨(1,μ₀),h1⟩ hcont
  have hscale := freeSdProfile_scales hn (zero_lt_one.trans_le hy.1.1) (hne hy.2)
  have hyv : freeSdProfile x y ∈ freeSdValues x S :=
    ⟨y.1, y.2, _, hy.1.1, hy.2, hscale.1, hscale.2.1⟩
  have hF {L : ℝ} (hL : L ∈ upperBounds (freeSdValues x S)) : F ≤ L := by
    have hlim := (uniform_path_freeSd hρ hcover).2
    apply le_of_tendsto hlim
    filter_upwards [eventually_ge_atTop (1 : ℝ)] with p hp
    have hp' := zero_lt_one.trans_le hp
    let b := ρ * p^(1/p)
    have hb : 0 < b := mul_pos hρ (rpow_pos_of_pos hp' _)
    exact hL ⟨p, μ₀, b, hp, hμ₀, hb, (kernelLogLik_eq hp' hb x μ₀).symm⟩
  have hv {p μ b : ℝ} (hp : 1 ≤ p) (hμ : μ ∈ S) (hb : 0 < b) :
      kernelLogLik p x μ b ≤ freeSdProfile x y ∨ kernelLogLik p x μ b < F := by
    by_cases hP : p ≤ uniformCutoff n
    · have hm' := hm (show (p,μ) ∈ K from ⟨⟨hp,hP⟩,hμ⟩)
      exact Or.inl (((freeSdProfile_scales hn (zero_lt_one.trans_le hp) (hne hμ)).2.2 b hb).trans hm')
    · exact Or.inr (uniform_tail_excluded hn hρ hb (lt_of_not_ge hP) (hlower μ hμ))
  refine ⟨y, hy, hm, ⟨?_,?_⟩, ?_⟩
  · rintro ⟨L, ⟨p, μ, b, hp, hμ, hb, rfl⟩, hL⟩
    have hlow := hF hL
    rcases hv hp hμ hb with h | h
    · exact hlow.trans h
    · exact (h.not_ge hlow).elim
  · intro h
    refine ⟨freeSdProfile x y, hyv, ?_⟩
    rintro _ ⟨p, μ, b, hp, hμ, hb, rfl⟩
    exact (hv hp hμ hb).elim id (fun hh ↦ hh.le.trans h)
  · intro h
    refine ⟨?_, fun _ hL ↦ hF hL⟩
    rintro _ ⟨p, μ, b, hp, hμ, hb, rfl⟩
    exact (hv hp hμ hb).elim (fun hh ↦ hh.trans h.le) le_of_lt

/-- When the held location has a nonzero residual, the compact shape
profile decides the comparison against the centered uniform. -/
theorem freeSd_heldMean (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    ∃ i : Fin n, ∀ (ρ : ℝ), ρ = |x i-μ| →
      0 < ρ ∧ (∀ j, |x j-μ| ≤ ρ) ∧
      ∃ y ∈ Icc 1 (uniformCutoff n) ×ˢ ({μ} : Set ℝ),
        IsMaxOn (freeSdProfile x) (Icc 1 (uniformCutoff n) ×ˢ {μ}) y ∧
        ((∃ L, IsGreatest (freeSdValues x {μ}) L) ↔ -(n : ℝ)*log (2*ρ) ≤ freeSdProfile x y) ∧
        (freeSdProfile x y < -(n : ℝ)*log (2*ρ) →
          IsLUB (freeSdValues x {μ}) (-(n : ℝ)*log (2*ρ))) := by
  obtain ⟨j,hj⟩ := hx
  obtain ⟨i,_,hi⟩ := Finset.exists_max_image Finset.univ (fun j ↦ |x j-μ|)
    ⟨j,Finset.mem_univ _⟩
  refine ⟨i, fun ρ hρ ↦ ?_⟩
  subst ρ
  have hb : 0 < |x i-μ| :=
    (abs_pos.mpr (sub_ne_zero.mpr hj)).trans_le (hi j (Finset.mem_univ _))
  have hw (k : Fin n) := hi k (Finset.mem_univ k)
  refine ⟨hb, hw, freeSd_compactCriterion hn isCompact_singleton hb ?_ ⟨μ,rfl,hw⟩⟩
  intro m hm
  obtain rfl : m = μ := hm
  exact ⟨i,le_rfl⟩

private theorem kernelLogLik_of_residual_le {x : Fin n → ℝ} {p b c d : ℝ}
    (hp : 0 < p) (hb : 0 < b) (h : ∀ i, |x i-c| ≤ |x i-d|) :
    kernelLogLik p x d b ≤ kernelLogLik p x c b := by
  unfold kernelLogLik
  apply sub_le_sub_left
  apply Finset.sum_le_sum
  intro i _
  exact rpow_le_rpow (div_nonneg (abs_nonneg _) hb.le)
    (div_le_div_of_nonneg_right (h i) hb.le) hp.le

private theorem freeSd_clip {x : Fin n → ℝ} {l r : ℝ}
    (hl : ∀ i, l ≤ x i) (hr : ∀ i, x i ≤ r) (hlr : l ≤ r) :
    ∀ L ∈ freeSdValues x univ, ∃ v ∈ freeSdValues x (Icc l r), L ≤ v := by
  rintro L ⟨p,m,b,hp,_,hb,rfl⟩
  by_cases h : m < l
  · refine ⟨kernelLogLik p x l b, ⟨p,l,b,hp,⟨le_rfl,hlr⟩,hb,rfl⟩,
      kernelLogLik_of_residual_le (zero_lt_one.trans_le hp) hb ?_⟩
    intro i
    rw [abs_of_nonneg (sub_nonneg.mpr (hl i)), abs_of_nonneg (sub_nonneg.mpr (h.le.trans (hl i)))]
    linarith
  · by_cases h' : r < m
    · refine ⟨kernelLogLik p x r b, ⟨p,r,b,hp,⟨hlr,le_rfl⟩,hb,rfl⟩,
        kernelLogLik_of_residual_le (zero_lt_one.trans_le hp) hb ?_⟩
      intro i
      rw [abs_of_nonpos (sub_nonpos.mpr (hr i)), abs_of_nonpos (sub_nonpos.mpr ((hr i).trans h'.le))]
      linarith
    · exact ⟨_, ⟨p,m,b,hp,⟨le_of_not_gt h,le_of_not_gt h'⟩,hb,rfl⟩,le_rfl⟩

/-- Nonconstant data with all coefficients free are decided by the
profile on the sample range and the stated compact shape interval. -/
theorem freeSd_allFree (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    ∃ l r : Fin n, (∀ i, x l ≤ x i ∧ x i ≤ x r) ∧
      let ρ := (x r-x l)/2
      let K := Icc 1 (uniformCutoff n) ×ˢ Icc (x l) (x r)
      let F := -(n : ℝ)*log (2*ρ)
      ∃ y ∈ K, IsMaxOn (freeSdProfile x) K y ∧
        ((∃ L, IsGreatest (freeSdValues x univ) L) ↔ F ≤ freeSdProfile x y) ∧
        (freeSdProfile x y < F → IsLUB (freeSdValues x univ) F) := by
  let j : Fin n := ⟨0,hn⟩
  obtain ⟨l,_,hl⟩ := Finset.exists_min_image Finset.univ x ⟨j,Finset.mem_univ _⟩
  obtain ⟨r,_,hr⟩ := Finset.exists_max_image Finset.univ x ⟨j,Finset.mem_univ _⟩
  have hL (i : Fin n) := hl i (Finset.mem_univ i)
  have hR (i : Fin n) := hr i (Finset.mem_univ i)
  refine ⟨l,r,fun i ↦ ⟨hL i,hR i⟩,?_⟩
  let ρ := (x r-x l)/2
  let m := (x r+x l)/2
  let S := Icc (x l) (x r)
  let F := -(n : ℝ)*log (2*ρ)
  have hlr : x l < x r := by
    by_contra! h
    obtain ⟨a,b,hab⟩ := hx
    apply hab
    exact le_antisymm ((hR a).trans (h.trans (hL b))) ((hR b).trans (h.trans (hL a)))
  have hρ : 0 < ρ := div_pos (sub_pos.mpr hlr) (by norm_num)
  have hun (d : ℝ) (_ : d ∈ S) : ∃ i, ρ ≤ |x i-d| := by
    rcases le_total d m with h | h
    · exact ⟨r, (show ρ ≤ x r-d by dsimp [ρ,m] at *; linarith).trans (le_abs_self _)⟩
    · exact ⟨l, (show ρ ≤ -(x l-d) by dsimp [ρ,m] at *; linarith).trans (neg_le_abs _)⟩
  have hco : ∃ d ∈ S, ∀ i, |x i-d| ≤ ρ := by
    refine ⟨m,⟨by dsimp [m]; linarith, by dsimp [m]; linarith⟩,?_⟩
    intro i
    apply abs_le.mpr
    dsimp [m,ρ]
    constructor <;> linarith [hL i,hR i]
  have hclip := freeSd_clip hL hR hlr.le
  have hsub : freeSdValues x S ⊆ freeSdValues x univ := by
    rintro z ⟨p,d,b,hp,_,hb,he⟩
    exact ⟨p,d,b,hp,mem_univ _,hb,he⟩
  have heq : upperBounds (freeSdValues x S) = upperBounds (freeSdValues x univ) := by
    ext T
    exact ⟨fun ht z hz ↦ by
      obtain ⟨v,hv,hzv⟩ := hclip z hz
      exact hzv.trans (ht hv), fun ht _ hz ↦ ht (hsub hz)⟩
  obtain ⟨y,hy,hm,he,hb⟩ := freeSd_compactCriterion hn isCompact_Icc hρ hun hco
  refine ⟨y,hy,hm, ?_, ?_⟩
  · constructor
    · rintro ⟨T,hT,hmT⟩
      obtain ⟨v,hv,hTv⟩ := hclip T hT
      have hEq : v = T := le_antisymm (hmT (hsub hv)) hTv
      exact he.mp ⟨T, hEq ▸ hv, heq.symm ▸ hmT⟩
    · intro h
      obtain ⟨T,hT,hmT⟩ := he.mpr h
      exact ⟨T,hsub hT,heq ▸ hmT⟩
  · intro h
    change IsLeast (upperBounds (freeSdValues x univ)) F
    rw [← heq]
    exact hb h

/-- Constant data at an admissible location have unbounded fitted-domain
likelihood even if the shape restriction includes only shapes at least one. -/
theorem fitted_constant_collapse (hn : 0 < n) {x : Fin n → ℝ} {S : Set ℝ} {μ : ℝ}
    (hμ : μ ∈ S) (hx : ∀ i, x i = μ) :
    ¬ BddAbove (freeSdValues x S) := by
  rintro ⟨L,hL⟩
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let G := Gamma 2
  have hG : 0 < G := Gamma_pos_of_pos (by norm_num)
  let b := exp (-(L/n+1)) / (2*G)
  have hb : 0 < b := div_pos (exp_pos _) (mul_pos (by norm_num) hG)
  have he : 2*b*G = exp (-(L/n+1)) := by unfold b; field_simp
  have h := hL ⟨1,μ,b,le_rfl,hμ,hb,rfl⟩
  change kernelLogLik 1 x μ b ≤ L at h
  unfold kernelLogLik at h
  rw [show 1 + 1/(1 : ℝ) = 2 by norm_num] at h
  change -(n : ℝ)*log (2*b*G) - _ ≤ _ at h
  simp only [he, log_exp, hx, sub_self, abs_zero, zero_div, rpow_one, Finset.sum_const_zero,
    sub_zero] at h
  have hle : (n : ℝ)*(L/n) = L := by field_simp
  nlinarith

end

end UnivariateML.TwoPieceGED

end
