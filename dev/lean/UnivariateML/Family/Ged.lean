module

public import UnivariateML.Generic.LocationScale
public import UnivariateML.Family.Laplace
public import Mathlib.Analysis.SpecialFunctions.Gamma.Basic

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-shape generalized error likelihood

The scale parameter is the standard deviation. In inverse coordinates the
pair is ordered as `(b, u) = (1/σ, μ/σ)` to match `LocationScale`.

## Deviations

The fixed-shape results concern nonempty finite samples and exact real
arithmetic. Held-shape results below one and comparisons across shapes are
outside the scope. The conditional scale calculation applies at any positive
shape.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Ged

noncomputable section

variable {n : ℕ}

/-- Multiplier converting the kernel scale to standard deviation. -/
def sdFactor (ν : ℝ) : ℝ := sqrt (Gamma (3 / ν) / Gamma (1 / ν))

def loss (ν z : ℝ) : ℝ := sdFactor ν ^ ν * |z| ^ ν

def logNormalizer (ν : ℝ) : ℝ := -log (2 * Gamma (1 + 1 / ν) / sdFactor ν)

def logLik (ν : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  LocationScale.logLik (loss ν) x p + n * logNormalizer ν

def inverseLogLik (ν : ℝ) (x : Fin n → ℝ) (p : ℝ × ℝ) : ℝ :=
  LocationScale.inverseLogLik (loss ν) x p + n * logNormalizer ν

def residualSum (ν : ℝ) (x : Fin n → ℝ) (μ : ℝ) : ℝ := ∑ i, |x i - μ| ^ ν

def bestScale (ν : ℝ) (x : Fin n → ℝ) (μ : ℝ) : ℝ :=
  sdFactor ν * (ν * residualSum ν x μ / n) ^ ν⁻¹

def jointMaximizers (ν : ℝ) (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {p ∈ LocationScale.domain | IsMaxOn (logLik ν x) LocationScale.domain p}

private theorem factor_pos {ν : ℝ} (hν : 0 < ν) : 0 < sdFactor ν :=
  sqrt_pos_of_pos (div_pos (Gamma_pos_of_pos (by positivity)) (Gamma_pos_of_pos (by positivity)))

private theorem strictConvex_absPow {ν : ℝ} (hν : 1 < ν) :
    StrictConvexOn ℝ univ (fun z : ℝ ↦ |z| ^ ν) := by
  refine ⟨convex_univ, ?_⟩
  intro x _ y _ hxy a b ha hb hab
  simp only [smul_eq_mul]
  have ht : |a * x + b * y| ≤ a * |x| + b * |y| := by
    simpa only [abs_mul, abs_of_pos ha, abs_of_pos hb] using abs_add_le (a * x) (b * y)
  have hν' : 0 < ν := zero_lt_one.trans hν
  rcases eq_or_ne |x| |y| with he | he
  · have hy : y = -x := by
      rcases abs_eq_abs.mp he with h | h
      · exact (hxy h).elim
      · linarith
    have hx : 0 < |x| := abs_pos.mpr (by intro h; apply hxy; simp [h, hy])
    have hab' : |a - b| < 1 := abs_lt.mpr ⟨by linarith, by linarith⟩
    rw [← he, ← add_mul, hab, one_mul, hy,
      show a * x + b * -x = (a - b) * x by ring, abs_mul]
    exact rpow_lt_rpow (by positivity) (by nlinarith only [mul_lt_mul_of_pos_right hab' hx]) hν'
  · exact (rpow_le_rpow (abs_nonneg _) ht hν'.le).trans_lt
      ((strictConvexOn_rpow hν).2 (abs_nonneg x) (abs_nonneg y) he ha hb hab)

private theorem loss_strictConvex {ν : ℝ} (hν : 1 < ν) :
    StrictConvexOn ℝ univ (loss ν) := by
  refine ⟨convex_univ, ?_⟩
  intro x hx y hy hxy a b ha hb hab
  have h := (strictConvex_absPow hν).2 hx hy hxy ha hb hab
  simpa only [loss, smul_eq_mul, mul_add, mul_left_comm] using
    mul_lt_mul_of_pos_left h (rpow_pos_of_pos (factor_pos (zero_lt_one.trans hν)) ν)

private theorem loss_convex {ν : ℝ} (hν : 1 ≤ ν) : ConvexOn ℝ univ (loss ν) := by
  rcases hν.eq_or_lt with rfl | h
  · refine ⟨convex_univ, ?_⟩
    intro x _ y _ a b ha hb _
    have h : |a * x + b * y| ≤ a * |x| + b * |y| := by
      simpa only [abs_mul, abs_of_nonneg ha, abs_of_nonneg hb] using
        abs_add_le (a * x) (b * y)
    simpa only [loss, rpow_one, smul_eq_mul, mul_add, mul_left_comm] using
      mul_le_mul_of_nonneg_left h (factor_pos zero_lt_one).le
  · exact (loss_strictConvex h).convexOn

private theorem loss_linearLowerBound {ν : ℝ} (hν : 1 ≤ ν) :
    LocationScale.LinearLowerBound (loss ν) := by
  have hc := rpow_pos_of_pos (factor_pos (zero_lt_one.trans_le hν)) ν
  refine ⟨_, hc, sdFactor ν ^ ν, fun z ↦ ?_⟩
  by_cases hz : 1 ≤ |z|
  · have h := mul_le_mul_of_nonneg_left
      (by simpa only [rpow_one] using rpow_le_rpow_of_exponent_le hz hν) hc.le
    unfold loss
    linarith
  · have h := mul_le_mul_of_nonneg_left (le_of_not_ge hz) hc.le
    rw [mul_one] at h
    exact (sub_nonpos.mpr h).trans (mul_nonneg hc.le (rpow_nonneg (abs_nonneg _) _))

/-- Inverse-scale coordinates preserve the normalized likelihood. -/
theorem inverseLogLik_eq (ν : ℝ) (x : Fin n → ℝ) (p : LocationScale.domain) :
    inverseLogLik ν x (LocationScale.inverseEquiv p) = logLik ν x p := by
  unfold inverseLogLik logLik
  rw [LocationScale.inverseLogLik_inverseEquiv]

/-- The fixed-shape likelihood is concave in inverse-scale coordinates. -/
theorem concave_inverseLogLik {ν : ℝ} (hν : 1 ≤ ν) (x : Fin n → ℝ) :
    ConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ν x) :=
  (LocationScale.concave_inverseLogLik (loss_convex hν) x).add_const _

/-- For shape above one the inverse-scale likelihood is strictly concave. -/
theorem strictConcave_inverseLogLik {ν : ℝ} (hν : 1 < ν) (x : Fin n → ℝ) (hn : 0 < n) :
    StrictConcaveOn ℝ LocationScale.inverseDomain (inverseLogLik ν x) :=
  (LocationScale.strictConcave_inverseLogLik (loss_strictConvex hν) x hn).add_const _

private theorem max_iff {ν : ℝ} {x : Fin n → ℝ} {p : ℝ × ℝ} :
    IsMaxOn (logLik ν x) LocationScale.domain p ↔
      IsMaxOn (LocationScale.logLik (loss ν) x) LocationScale.domain p := by
  simp only [isMaxOn_iff, logLik, add_le_add_iff_right]

/-- Every nonconstant sample has a finite joint maximizer at fixed shape. -/
theorem joint_exists {ν : ℝ} (hν : 1 ≤ ν) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    (jointMaximizers ν x).Nonempty := by
  obtain ⟨p, hp, hm⟩ :=
    LocationScale.exists_mle (loss_convex hν) (loss_linearLowerBound hν) hx
  exact ⟨p, hp, max_iff.mpr hm⟩

/-- At fixed shape above one the joint maximizer is unique. -/
theorem joint_existsUnique {ν : ℝ} (hν : 1 < ν) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    ∃! p, p ∈ jointMaximizers ν x := by
  simpa only [jointMaximizers, mem_ofPred_eq, max_iff] using
    LocationScale.existsUnique_mle (loss_strictConvex hν) (loss_linearLowerBound hν.le) hx

private theorem logLik_eq {ν : ℝ} (x : Fin n → ℝ) (μ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik ν x (μ, σ) = n * logNormalizer ν - n * log σ -
      (sdFactor ν ^ ν / σ ^ ν) * residualSum ν x μ := by
  unfold logLik LocationScale.logLik loss residualSum
  simp only [abs_div, abs_of_pos hσ, div_rpow (abs_nonneg _) hσ.le,
    Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  rw [Finset.mul_sum]
  have he : (∑ i, sdFactor ν ^ ν * (|x i - μ| ^ ν / σ ^ ν)) =
      ∑ i, (sdFactor ν ^ ν / σ ^ ν) * |x i - μ| ^ ν := by
    apply Finset.sum_congr rfl
    intro i _
    ring
  rw [he]
  ring

/-- If all residuals vanish, the log likelihood diverges as scale decreases to zero. -/
theorem collapse {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (fun σ ↦ logLik ν x (μ, σ)) (𝓝[>] 0) atTop := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hz : residualSum ν x μ = 0 := by
    simp [residualSum, hx, zero_rpow hν.ne']
  have he : (fun σ ↦ logLik ν x (μ, σ)) =ᶠ[𝓝[>] (0 : ℝ)]
      (fun σ ↦ n * logNormalizer ν + (- (n : ℝ)) * log σ) := by
    filter_upwards [self_mem_nhdsWithin] with σ hσ
    rw [logLik_eq x μ (show 0 < σ from hσ), hz]
    ring
  rw [tendsto_congr' he]
  have ht := Tendsto.const_mul_atBot_of_neg (neg_neg_of_pos hN) tendsto_log_nhdsGT_zero
  apply tendsto_atTop.mpr
  intro M
  filter_upwards [tendsto_atTop.mp ht (M - n * logNormalizer ν)] with σ h
  change _ ≤ n * logNormalizer ν + -(n : ℝ) * log σ
  linarith

/-- The ordinary likelihood also diverges along the collapsing scale path. -/
theorem collapse_likelihood {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, x i = μ) :
    Tendsto (fun σ ↦ exp (logLik ν x (μ, σ))) (𝓝[>] 0) atTop :=
  tendsto_exp_atTop.comp (collapse hν hn hx)

private theorem sum_pos {ν : ℝ} {x : Fin n → ℝ} {μ : ℝ} (hx : ∃ i, x i ≠ μ) :
    0 < residualSum ν x μ := by
  obtain ⟨i, hi⟩ := hx
  exact lt_of_lt_of_le (rpow_pos_of_pos (abs_pos.mpr (sub_ne_zero.mpr hi)) ν)
    (Finset.single_le_sum (f := fun j ↦ |x j - μ| ^ ν)
      (fun j _ ↦ rpow_nonneg (abs_nonneg (x j - μ)) ν) (Finset.mem_univ i))

/-- With a nonzero residual the explicit conditional scale is positive and uniquely optimal. -/
theorem heldLocation {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∃ i, x i ≠ μ) :
    0 < bestScale ν x μ ∧
      ∀ σ > 0, IsMaxOn (fun s ↦ logLik ν x (μ, s)) (Ioi 0) σ ↔ σ = bestScale ν x μ := by
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  let S := residualSum ν x μ
  let A := ν * S / n
  let B := bestScale ν x μ
  have hA : 0 < A := div_pos (mul_pos hν (sum_pos hx)) hN
  have hF := factor_pos hν
  have hB : 0 < B := mul_pos hF (rpow_pos_of_pos hA _)
  have hBpow : B ^ ν = sdFactor ν ^ ν * A := by
    change (sdFactor ν * A ^ ν⁻¹) ^ ν = _
    rw [mul_rpow hF.le (rpow_nonneg hA.le _), rpow_inv_rpow hA.le hν.ne']
  have hpen (s : ℝ) (hs : 0 < s) :
      sdFactor ν ^ ν / s ^ ν * S = ((B / s) ^ ν) * (n / ν) := by
    rw [div_rpow hB.le hs.le, hBpow]
    dsimp [A]
    field_simp [hν.ne', hN.ne', (rpow_pos_of_pos hs ν).ne']
  have hstrict (s : ℝ) (hs : 0 < s) (hne : s ≠ B) :
      logLik ν x (μ, s) < logLik ν x (μ, B) := by
    have hr : (B / s) ^ ν ≠ 1 := by
      intro he
      have he' : (B / s) ^ ν = (1 : ℝ) ^ ν := by simpa only [one_rpow] using he
      have hd := (rpow_left_inj (div_nonneg hB.le hs.le) zero_le_one hν.ne').mp he'
      exact hne (by simpa using ((div_eq_iff hs.ne').mp hd).symm)
    have h := log_lt_sub_one_of_pos (rpow_pos_of_pos (div_pos hB hs) ν) hr
    rw [log_rpow (div_pos hB hs), log_div hB.ne' hs.ne'] at h
    have hh := mul_lt_mul_of_pos_right h (div_pos hN hν)
    have he : ν * (log B - log s) * (n / ν) = n * (log B - log s) := by
      field_simp
    rw [he, sub_mul, one_mul] at hh
    rw [logLik_eq x μ hs, logLik_eq x μ hB, hpen s hs, hpen B hB,
      div_self hB.ne', one_rpow, one_mul]
    linarith
  refine ⟨hB, fun s hs ↦ ⟨?_, ?_⟩⟩
  · intro hm
    by_contra hne
    exact (hstrict s hs hne).not_ge (hm hB)
  · rintro rfl t ht
    rcases eq_or_ne t B with rfl | hne
    · exact le_rfl
    · exact (hstrict t ht hne).le

/-- At fixed positive scale the likelihood maximizers are the residual-loss minimizers. -/
theorem heldScale (ν : ℝ) (x : Fin n → ℝ) {σ μ : ℝ} (hν : 0 < ν) (hσ : 0 < σ) :
    IsMaxOn (fun m ↦ logLik ν x (m, σ)) univ μ ↔ IsMinOn (residualSum ν x) univ μ := by
  have hC := div_pos (rpow_pos_of_pos (factor_pos hν) ν) (rpow_pos_of_pos hσ ν)
  have hcmp (m : ℝ) :
      logLik ν x (m, σ) ≤ logLik ν x (μ, σ) ↔ residualSum ν x μ ≤ residualSum ν x m := by
    rw [logLik_eq x m hσ, logLik_eq x μ hσ, sub_le_sub_iff_left, mul_le_mul_iff_right₀ hC]
  exact ⟨fun h m hm ↦ (hcmp m).mp (h hm), fun h m hm ↦ (hcmp m).mpr (h hm)⟩

private theorem strictConvex_residualSum {ν : ℝ} (hν : 1 < ν) (hn : 0 < n)
    (x : Fin n → ℝ) : StrictConvexOn ℝ univ (residualSum ν x) := by
  refine ⟨convex_univ, ?_⟩
  intro m _ t _ hmt a b ha hb hab
  simp only [residualSum, smul_eq_mul, Finset.mul_sum, ← Finset.sum_add_distrib]
  let : Nonempty (Fin n) := ⟨⟨0, hn⟩⟩
  apply Finset.sum_lt_sum_of_nonempty Finset.univ_nonempty
  intro i _
  have he : x i - (a * m + b * t) = a * (x i - m) + b * (x i - t) := by
    linear_combination -x i * hab
  rw [he]
  exact (strictConvex_absPow hν).2 (mem_univ _) (mem_univ _)
    (fun h ↦ hmt (by linarith)) ha hb hab

private theorem nonzero_residual {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x)
    (μ : ℝ) : ∃ i, x i ≠ μ := by
  by_contra! he
  obtain ⟨i, j, hij⟩ := hx
  exact hij ((he i).trans (he j).symm)

/-- At a positive held scale, a shape above one gives exactly one location even on constant data. -/
theorem heldScale_existsUnique {ν : ℝ} (hν : 1 < ν) (hn : 0 < n) (x : Fin n → ℝ)
    {σ : ℝ} (hσ : 0 < σ) :
    ∃! μ, IsMaxOn (fun m ↦ logLik ν x (m, σ)) univ μ := by
  have hν' := zero_lt_one.trans hν
  simp_rw [heldScale ν x hν' hσ]
  have hex : ∃ μ, IsMinOn (residualSum ν x) univ μ := by
    by_cases hx : LocationScale.Nonconstant x
    · obtain ⟨⟨μ, s⟩, hp, hm⟩ := joint_exists hν.le hx
      have hs : 0 < s := hp.2
      exact ⟨μ, (heldScale ν x hν' hs).mp
        (fun m _ ↦ hm (show (m, s) ∈ LocationScale.domain from ⟨mem_univ _, hs⟩))⟩
    · let i : Fin n := ⟨0, hn⟩
      have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
      refine ⟨x i, fun m _ ↦ ?_⟩
      change residualSum ν x (x i) ≤ residualSum ν x m
      rw [show residualSum ν x (x i) = 0 by simp [residualSum, he, zero_rpow hν'.ne']]
      exact Finset.sum_nonneg (fun _ _ ↦ rpow_nonneg (abs_nonneg _) _)
  obtain ⟨μ, hm⟩ := hex
  exact ⟨μ, hm, fun t ht ↦
    (strictConvex_residualSum hν hn x).eq_of_isMinOn ht hm (mem_univ _) (mem_univ _)⟩

private theorem median_iff_min (hn : 0 < n) (x : Fin n → ℝ) (μ : ℝ) :
    μ ∈ Laplace.medianSet x hn ↔ IsMinOn (residualSum 1 x) univ μ := by
  rw [← Laplace.heldScale hn x one_pos]
  have he (m : ℝ) : Laplace.logLik x m 1 =
      n * (-log 2) - residualSum 1 x m := by
    simp [Laplace.logLik, Laplace.logDensity, residualSum, Finset.sum_sub_distrib, nsmul_eq_mul]
  change IsMaxOn (fun m ↦ Laplace.logLik x m 1) univ μ ↔ _
  simp only [isMaxOn_iff, isMinOn_iff, he, sub_le_sub_iff_left]

/-- At shape one and positive held scale, the complete location set is the median interval. -/
theorem heldScale_one (hn : 0 < n) (x : Fin n → ℝ) {σ : ℝ} (hσ : 0 < σ) :
    {μ | IsMaxOn (fun m ↦ logLik 1 x (m, σ)) univ μ} = Laplace.medianSet x hn := by
  ext μ
  exact (heldScale 1 x one_pos hσ).trans (median_iff_min hn x μ).symm

/-- Every joint maximizer has a loss-minimizing location and its unique conditional scale. -/
theorem joint_graph {ν : ℝ} (hν : 0 < ν) (hn : 0 < n) {x : Fin n → ℝ}
    (hx : LocationScale.Nonconstant x) :
    jointMaximizers ν x =
      {p | IsMinOn (residualSum ν x) univ p.1 ∧ p.2 = bestScale ν x p.1} := by
  ext ⟨μ, σ⟩
  have hc := heldLocation hν hn (nonzero_residual hx μ)
  change ((μ, σ) ∈ LocationScale.domain ∧
    IsMaxOn (logLik ν x) LocationScale.domain (μ, σ)) ↔
      IsMinOn (residualSum ν x) univ μ ∧ σ = bestScale ν x μ
  constructor
  · rintro ⟨hp, hm⟩
    have hs : 0 < σ := hp.2
    exact ⟨(heldScale ν x hν hs).mp (fun m _ ↦ hm
      (show (m, σ) ∈ LocationScale.domain from ⟨mem_univ _, hs⟩)),
      (hc.2 σ hs).mp (fun s hs' ↦ hm
        (show (μ, s) ∈ LocationScale.domain from ⟨mem_univ _, hs'⟩))⟩
  · rintro ⟨hm, rfl⟩
    refine ⟨⟨mem_univ _, hc.1⟩, ?_⟩
    intro p hp
    have hs : 0 < p.2 := hp.2
    exact (((heldScale ν x hν hs).mpr hm) (mem_univ p.1)).trans
      (((hc.2 _ hc.1).mpr rfl) hs)

/-- Attainment at fixed shape is equivalent to nonconstancy for nonempty data. -/
theorem joint_exists_iff {ν : ℝ} (hν : 1 ≤ ν) (hn : 0 < n) (x : Fin n → ℝ) :
    (jointMaximizers ν x).Nonempty ↔ LocationScale.Nonconstant x := by
  refine ⟨?_, joint_exists hν⟩
  rintro ⟨p, _, hm⟩
  by_contra hx
  let i : Fin n := ⟨0, hn⟩
  have he (j : Fin n) : x j = x i := not_ne_iff.mp (fun h ↦ hx ⟨j, i, h⟩)
  have ht := collapse (zero_lt_one.trans_le hν) hn he
  have hpos : ∀ᶠ s in 𝓝[>] (0 : ℝ), s ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
  obtain ⟨s, hs, hgt⟩ :=
    (hpos.and (ht.eventually (eventually_gt_atTop (logLik ν x p)))).exists
  exact hgt.not_ge (hm (show (x i, s) ∈ LocationScale.domain from ⟨mem_univ _, hs⟩))

/-- The shape-one scale is the usual Laplace scale converted to standard deviation. -/
theorem bestScale_one (x : Fin n → ℝ) (μ : ℝ) :
    bestScale 1 x μ = sqrt 2 * Laplace.meanAbsDev x μ := by
  simp [bestScale, sdFactor, residualSum, Laplace.meanAbsDev]

/-- The joint shape-one maximizing set is the median interval at a common standard deviation. -/
theorem joint_one (hn : 0 < n) {x : Fin n → ℝ} (hx : LocationScale.Nonconstant x) :
    jointMaximizers 1 x =
      Laplace.medianSet x hn ×ˢ {sqrt 2 * Laplace.meanAbsDev x (Laplace.lowerMedian x hn)} := by
  have hlo : Laplace.lowerMedian x hn ∈ Laplace.medianSet x hn :=
    ⟨le_rfl, Tuple.monotone_sort x (show
      (⟨(n - 1) / 2, by omega⟩ : Fin n) ≤ ⟨n / 2, by omega⟩ from by
        change (n - 1) / 2 ≤ n / 2
        omega)⟩
  rw [joint_graph one_pos hn hx]
  ext p
  rw [mem_ofPred_eq, mem_prod, mem_singleton_iff, ← median_iff_min hn x p.1, bestScale_one]
  exact and_congr_right fun hm ↦ by
    rw [Laplace.meanAbsDev_eq_of_mem_medianSet hn hm hlo]

end

end UnivariateML.Ged
