module

public import Mathlib.Analysis.Complex.ExponentialBounds
public import Mathlib.Analysis.Convex.SpecificFunctions.Basic
public import Mathlib.Analysis.Calculus.Deriv.MeanValue
public import Mathlib.Analysis.SpecialFunctions.Log.Deriv
public import Mathlib.Analysis.SpecialFunctions.Pow.Real
public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Order.IntermediateValue
public import Mathlib.Topology.Semicontinuity.Basic

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Generalized Pareto likelihood

The sample is represented by excesses over the threshold. The scale domain
includes the right endpoint of the uniform member.

## Deviations

Free-threshold unboundedness is witnessed with a fixed positive shape, chosen
using the sample size, and scale tending to zero at the sample minimum.
For the joint tie, an exact comparison at k = 1/2 replaces the derivative
test at zero, and finite tail exclusions suffice to locate the maximum of a.
The global comparison uses the likelihood at fixed b directly.
General attainment uses compactness in the profile coordinate and comparison
with the attained uniform member, without a separate density envelope.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.GPD

noncomputable section

variable {n : ℕ}

/-- The divided logarithm with its exponential-member extension. -/
def logRatio (y b : ℝ) : ℝ :=
  if b = 0 then y else log (1 + b * y) / b

def logDensity (σ ξ y : ℝ) : ℝ :=
  -log σ - (1 + ξ) * logRatio (y / σ) ξ

def logLik (y : Fin n → ℝ) (σ ξ : ℝ) : ℝ :=
  ∑ i, logDensity σ ξ (y i)

/-- The finite log-likelihood domain, including the closed uniform support. -/
def scaleDomain (y : Fin n → ℝ) (ξ : ℝ) : Set ℝ :=
  {σ | 0 < σ ∧ ∀ i, 0 ≤ y i ∧
    if ξ = -1 then y i ≤ σ else 0 < σ + ξ * y i}

private theorem logDensity_of_ne_zero {σ ξ y : ℝ} (h : ξ ≠ 0) :
    logDensity σ ξ y = -log σ - (1 + 1 / ξ) * log (1 + ξ * (y / σ)) := by
  simp only [logDensity, logRatio, ite_eq_right h]
  field_simp
  ring

private theorem exists_max_index (hn : 0 < n) (x : Fin n → ℝ) :
    ∃ j, ∀ i, x i ≤ x j := by
  obtain ⟨j, _, hj⟩ := Finset.exists_max_image Finset.univ x
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  exact ⟨j, fun i ↦ hj i (Finset.mem_univ i)⟩

/-- Every nonempty sample has arbitrarily large finite log-likelihood with
the threshold at an observed minimum. -/
theorem freeThreshold_unbounded (hn : 0 < n) (x : Fin n → ℝ) :
    ∃ μ, (∃ j, μ = x j) ∧ ∀ C : ℝ, ∃ σ ξ,
      σ ∈ scaleDomain (fun i ↦ x i - μ) ξ ∧
        C < logLik (fun i ↦ x i - μ) σ ξ := by
  classical
  obtain ⟨j, hj⟩ := exists_max_index hn (fun i ↦ -x i)
  let y : Fin n → ℝ := fun i ↦ x i - x j
  have hy (i) : 0 ≤ y i := by
    dsimp [y]
    have := hj i
    linarith
  let ξ : ℝ := 2 * n
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hξ : 0 < ξ := by dsimp [ξ]; positivity
  have hξ₁ : ξ ≠ -1 := by linarith
  let L (i : Fin n) := (1 + 1 / ξ) * log (1 + ξ * y i)
  refine ⟨x j, ⟨j, rfl⟩, ?_⟩
  intro C
  let t := 2 * (max 0 (C + ∑ i ∈ Finset.univ.erase j, L i) + 1)
  have ht : 0 < t := by dsimp [t]; positivity
  have hex : 1 ≤ exp t := by simpa using (exp_le_exp.mpr ht.le)
  refine ⟨exp (-t), ξ, ⟨exp_pos _, fun i ↦ ⟨hy i, ?_⟩⟩, ?_⟩
  · simp only [ite_eq_right hξ₁]
    exact add_pos_of_pos_of_nonneg (exp_pos _) (mul_nonneg hξ.le (hy i))
  have hterm (i) : -(t / ξ) - L i ≤ logDensity (exp (-t)) ξ (y i) := by
    rw [logDensity_of_ne_zero hξ.ne', log_exp]
    have hbound : 1 + ξ * (y i / exp (-t)) ≤ exp t * (1 + ξ * y i) := by
      rw [exp_neg, div_inv_eq_mul]
      nlinarith only [hex]
    have hyi := hy i
    have hlog := strictMonoOn_log.monotoneOn
      (show 0 < 1 + ξ * (y i / exp (-t)) by positivity)
      (show 0 < exp t * (1 + ξ * y i) by positivity) hbound
    rw [log_mul (exp_ne_zero _) (by positivity), log_exp] at hlog
    have hp := mul_le_mul_of_nonneg_left hlog (show 0 ≤ 1 + 1 / ξ by positivity)
    dsimp [L]
    rw [div_eq_mul_inv, one_div] at *
    linarith only [hp]
  have hjterm : logDensity (exp (-t)) ξ (y j) = t := by
    simp [y, logDensity_of_ne_zero hξ.ne']
  change C < ∑ i, logDensity (exp (-t)) ξ (y i)
  rw [← Finset.sum_erase_add _ _ (Finset.mem_univ j), hjterm]
  have hs := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ.erase j) ↦ hterm i)
  rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul] at hs
  have hcard : ((Finset.univ.erase j).card : ℝ) ≤ n := by
    have h := Finset.card_le_card (Finset.erase_subset j Finset.univ)
    simp only [Finset.card_univ, Fintype.card_fin] at h
    exact_mod_cast h
  have htt : (n : ℝ) * (t / ξ) = t / 2 := by
    dsimp [ξ]
    field_simp
  have hmul := mul_le_mul_of_nonneg_right hcard (show 0 ≤ t / ξ by positivity)
  have hC := le_max_right 0 (C + ∑ i ∈ Finset.univ.erase j, L i)
  have htC : C + ∑ i ∈ Finset.univ.erase j, L i < t / 2 := by
    dsimp [t]
    linarith
  linarith

/-- A fixed shape below the uniform shape gives arbitrarily large finite
log-likelihood as the upper endpoint approaches the largest excess. -/
theorem shapeBelow_unbounded (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {ξ : ℝ} (hξ : ξ < -1) (C : ℝ) :
    ∃ σ ∈ scaleDomain y ξ, C < logLik y σ ξ := by
  classical
  obtain ⟨j, hj⟩ := exists_max_index hn y
  let c := -ξ * y j
  let p := 1 + 1 / ξ
  have hc : 0 < c := by dsimp [c]; exact mul_pos (by linarith) (hy j)
  have hp : 0 < p := by
    dsimp [p]
    have h : -1 < 1 / ξ := by
      rw [lt_div_iff_of_neg (show ξ < 0 by linarith)]
      linarith
    linarith
  have hξ₀ : ξ ≠ 0 := by linarith
  have hξ₁ : ξ ≠ -1 := by linarith
  let t := max 0 ((C + (n : ℝ) * log (c + 1)) / p - log c) + 1
  have ht : 0 < t := by dsimp [t]; positivity
  let σ := c + exp (-t)
  have hσ : 0 < σ := by dsimp [σ]; positivity
  have hs₁ : σ ≤ c + 1 := by
    dsimp [σ]
    have : exp (-t) ≤ 1 := exp_le_one_iff.mpr (by linarith)
    linarith
  have hlog := strictMonoOn_log.monotoneOn hσ (show 0 < c + 1 by positivity) hs₁
  have hsupport (i) : 0 < σ + ξ * y i := by
    have hm := mul_le_mul_of_nonpos_left (hj i) (show ξ ≤ 0 by linarith)
    dsimp [σ, c]
    linarith [exp_pos (-t)]
  have hbase (i) : 0 < 1 + ξ * (y i / σ) := by
    convert div_pos (hsupport i) hσ using 1
    field_simp
  have hle (i) : log (1 + ξ * (y i / σ)) ≤ 0 := by
    apply log_nonpos (hbase i).le
    have := mul_nonpos_of_nonpos_of_nonneg (show ξ ≤ 0 by linarith)
      (div_nonneg (hy i).le hσ.le)
    linarith
  have hjlog : log (1 + ξ * (y j / σ)) = -t - log σ := by
    have he : 1 + ξ * (y j / σ) = exp (-t) / σ := by
      field_simp
      simp [σ, c]
    rw [he, log_div (exp_ne_zero _) hσ.ne', log_exp]
  have hlow := strictMonoOn_log.monotoneOn hc hσ
    (le_add_of_nonneg_right (exp_pos (-t)).le)
  refine ⟨σ, ⟨hσ, fun i ↦ ⟨(hy i).le, by simpa [hξ₁] using hsupport i⟩⟩, ?_⟩
  have hLL : logLik y σ ξ =
      (n : ℝ) * (-log σ) + ∑ i, -(p * log (1 + ξ * (y i / σ))) := by
    simp only [logLik, logDensity_of_ne_zero hξ₀, sub_eq_add_neg,
      Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
      nsmul_eq_mul, p]
  rw [hLL, ← Finset.sum_erase_add _ _ (Finset.mem_univ j), hjlog]
  have hsum : 0 ≤ ∑ i ∈ Finset.univ.erase j, -(p * log (1 + ξ * (y i / σ))) :=
    Finset.sum_nonneg fun i _ ↦ neg_nonneg.mpr (mul_nonpos_of_nonneg_of_nonpos hp.le (hle i))
  have hnt : (0 : ℝ) ≤ n := by positivity
  have hC := le_max_right 0 ((C + (n : ℝ) * log (c + 1)) / p - log c)
  have hct : (C + (n : ℝ) * log (c + 1)) / p < t + log c := by
    dsimp [t]
    linarith
  have hct' := (div_lt_iff₀ hp).mp hct
  nlinarith only [hct', hsum, mul_le_mul_of_nonneg_left hlog hnt,
    mul_le_mul_of_nonneg_left hlow hp.le]

/-- For the closed uniform member the unique maximizing scale is the sample maximum. -/
theorem uniform_scale_unique (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i) :
    ∃ j, (∀ i, y i ≤ y j) ∧
      ∀ σ, (σ ∈ scaleDomain y (-1) ∧
        IsMaxOn (fun s ↦ logLik y s (-1)) (scaleDomain y (-1)) σ) ↔ σ = y j := by
  obtain ⟨j, hj⟩ := exists_max_index hn y
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hdom (σ : ℝ) : σ ∈ scaleDomain y (-1) ↔ y j ≤ σ := by
    constructor
    · intro h
      simpa using (h.2 j).2
    · intro h
      exact ⟨lt_of_lt_of_le (hy j) h, fun i ↦ ⟨(hy i).le, by simpa using (hj i).trans h⟩⟩
  have hll (σ : ℝ) : logLik y σ (-1) = (n : ℝ) * (-log σ) := by
    simp [logLik, logDensity]
  refine ⟨j, hj, fun σ ↦ ⟨?_, ?_⟩⟩
  · rintro ⟨hs, hm⟩
    have hs' := (hdom σ).mp hs
    by_contra! hne
    have hlt := log_lt_log (hy j) (lt_of_le_of_ne hs' (Ne.symm hne))
    have hm' := hm ((hdom _).mpr le_rfl)
    change logLik y (y j) (-1) ≤ logLik y σ (-1) at hm'
    rw [hll, hll] at hm'
    nlinarith
  · rintro rfl
    refine ⟨(hdom _).mpr le_rfl, fun s hs ↦ ?_⟩
    change logLik y s (-1) ≤ logLik y (y j) (-1)
    rw [hll, hll]
    have hs' := (hdom _).mp hs
    have := strictMonoOn_log.monotoneOn (hy j) (lt_of_lt_of_le (hy j) hs') hs'
    nlinarith

private def scaleScore (y : Fin n → ℝ) (ξ σ : ℝ) : ℝ :=
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

/-- With positive excesses every fixed shape above the uniform floor has a
unique maximizing scale. -/
theorem heldShape_scale_unique (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {ξ : ℝ} (hξ : -1 < ξ) :
    ∃! σ, σ ∈ scaleDomain y ξ ∧
      IsMaxOn (fun s ↦ logLik y s ξ) (scaleDomain y ξ) σ := by
  obtain ⟨j, hj⟩ := exists_max_index hn y
  obtain ⟨k, hk⟩ := exists_max_index hn (fun i ↦ -y i)
  have hm (i) : y k ≤ y i := neg_le_neg_iff.mp (hk i)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hξ' : 0 < 1 + ξ := by linarith
  have hne : ξ ≠ -1 := ne_of_gt hξ
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
      refine ⟨hs, fun i ↦ ⟨(hy i).le, ?_⟩⟩
      by_cases hx : 0 ≤ ξ
      · exact add_pos_of_pos_of_nonneg hs (mul_nonneg hx (hy i).le)
      · have := mul_le_mul_of_nonpos_left (hj i) (le_of_not_ge hx)
        linarith
  have hpos {s : ℝ} (hs : s ∈ Ioi c) (i : Fin n) : 0 < s + ξ * y i := by
    simpa [hne] using (((hdom s).mpr hs).2 i).2
  have hanti : StrictAntiOn f (Ioi c) := by
    intro a ha b hb hab
    unfold f scaleScore
    apply sub_lt_sub_right
    apply Finset.sum_lt_sum
    · intro i _
      exact (div_le_div_iff_of_pos_left (mul_pos hξ' (hy i)) (hpos hb i) (hpos ha i)).mpr
        (by linarith)
    · exact ⟨j, Finset.mem_univ _, (div_lt_div_iff_of_pos_left
        (mul_pos hξ' (hy j)) (hpos hb j) (hpos ha j)).mpr (by linarith)⟩
  have hcont : ContinuousOn f (Ioi c) := by
    apply ContinuousOn.sub _ continuousOn_const
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.div (continuous_id.add continuous_const).continuousOn
      (fun s hs ↦ (hpos hs i).ne')
  obtain ⟨a, ha, hfa⟩ : ∃ a ∈ Ioi c, 0 < f a := by
    by_cases hx : ξ < 0
    · have hc : c = -ξ * y j := max_eq_right (mul_nonneg (neg_nonneg.mpr hx.le) (hy j).le)
      let δ := (1 + ξ) * y j / (2 * n)
      have hδ : 0 < δ := div_pos (mul_pos hξ' (hy j)) (by positivity)
      let a := c + δ
      have ha : a ∈ Ioi c := lt_add_of_pos_right _ hδ
      refine ⟨a, ha, ?_⟩
      have hjval : (1 + ξ) * y j / (a + ξ * y j) = 2 * n := by
        rw [show a + ξ * y j = δ by dsimp [a]; rw [hc]; ring]
        dsimp [δ]
        field_simp [(hy j).ne']
      have hsum : (1 + ξ) * y j / (a + ξ * y j) ≤
          ∑ i, (1 + ξ) * y i / (a + ξ * y i) :=
        Finset.single_le_sum (fun i _ ↦ div_nonneg (mul_nonneg hξ'.le (hy i).le)
          (hpos ha i).le) (Finset.mem_univ j)
      dsimp [f, scaleScore]
      linarith
    · let a := y k / 2
      have hc : c = 0 := max_eq_left (mul_nonpos_of_nonpos_of_nonneg
        (neg_nonpos.mpr (le_of_not_gt hx)) (hy j).le)
      have ha : a ∈ Ioi c := by rw [hc]; exact half_pos (hy k)
      refine ⟨a, ha, ?_⟩
      have hsum := Finset.sum_lt_sum_of_nonempty
        (s := (Finset.univ : Finset (Fin n))) ⟨j, Finset.mem_univ _⟩
        (fun i _ ↦ (lt_div_iff₀ (hpos ha i)).mpr
          (show 1 * (a + ξ * y i) < (1 + ξ) * y i by
            dsimp [a]
            nlinarith [hm i, hy k]))
      simpa [f, scaleScore, sub_pos] using hsum
  obtain ⟨b, hb, hfb⟩ : ∃ b ∈ Ioi c, f b < 0 := by
    let b := c + y j + y j
    have hb : b ∈ Ioi c := by change c < b; dsimp [b]; linarith [hy j]
    refine ⟨b, hb, ?_⟩
    have hsum := Finset.sum_lt_sum_of_nonempty
      (s := (Finset.univ : Finset (Fin n))) ⟨j, Finset.mem_univ _⟩
      (fun i _ ↦ (div_lt_iff₀ (hpos hb i)).mpr
        (show (1 + ξ) * y i < 1 * (b + ξ * y i) by
          dsimp [b]
          nlinarith [hj i, hy j]))
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


/-- The profile coordinate of a candidate with mean log-likelihood zero. -/
def b (k : ℝ) : ℝ := k * exp (1 + k)

private def u (k : ℝ) : ℝ := exp ((3 * k - log (1 + b k)) / 2)

def a (k : ℝ) : ℝ :=
  if k = 0 then (3 / exp 1 - 1) / 2 else
    (exp ((3 * k - log (1 + b k)) / 2) - 1) / b k

private theorem a_of_ne {k : ℝ} (h : k ≠ 0) : a k = (u k - 1) / b k := by
  simp [a, u, h]

private theorem a_at_zero : a 0 = (3 / exp 1 - 1) / 2 := by
  simp [a]

private theorem b_bound {k : ℝ} (hk : -1 < k) : -1 < b k := by
  have hl : -k < exp (-(1 + k)) := by
    linarith [add_one_lt_exp (show -(1 + k) ≠ 0 by linarith)]
  have hm := mul_lt_mul_of_pos_right hl (exp_pos (1 + k))
  rw [← exp_add, neg_add_cancel, exp_zero] at hm
  unfold b
  linarith

private theorem b_ne {k : ℝ} (hk : k ≠ 0) : b k ≠ 0 :=
  mul_ne_zero hk (exp_ne_zero _)

private theorem b_cont : Continuous b :=
  continuous_id.mul (continuous_exp.comp (continuous_const.add continuous_id))

private theorem u_sq {k : ℝ} (hk : -1 < k) : u k ^ 2 = exp (3 * k) / (1 + b k) := by
  unfold u
  rw [pow_two, ← exp_add, show (3 * k - log (1 + b k)) / 2 +
    (3 * k - log (1 + b k)) / 2 = 3 * k - log (1 + b k) by ring,
    exp_sub, exp_log (by linarith [b_bound hk])]

private theorem u_pos (k : ℝ) : 0 < u k := exp_pos _

private theorem a_identity {k : ℝ} (hk : k ≠ 0) : 1 + b k * a k = u k := by
  rw [a_of_ne hk]
  field_simp [b_ne hk]
  ring

/-- The zero-shape value removes the singularity in the tie construction. -/
theorem a_continuous : ContinuousOn a (Ioi (-1 : ℝ)) := by
  intro k hk
  have hp : 0 < 1 + b k := by linarith [b_bound hk]
  have hd (k : ℝ) : HasDerivAt b (exp (1 + k) + k * exp (1 + k)) k := by
    have he : HasDerivAt (fun t : ℝ ↦ exp (1 + t)) (exp (1 + k)) k := by
      simpa [add_comm] using (((hasDerivAt_id k).const_add 1).exp)
    convert (hasDerivAt_id k).mul he using 1
    · rfl
    · simp
  have hu : ContinuousAt u k := by
    unfold u
    exact continuous_exp.continuousAt.comp (((continuousAt_id.const_mul 3).sub
      ((continuousAt_const.add b_cont.continuousAt).log hp.ne')).div_const 2)
  by_cases h : k = 0
  · subst k
    have hb0 : b 0 = 0 := by simp [b]
    have hu0 : u 0 = 1 := by simp [u, b]
    have hdu : HasDerivAt u ((3 - exp 1) / 2) 0 := by
      have hlog := ((hd 0).const_add 1).log (by simp [hb0])
      convert ((((hasDerivAt_id (0 : ℝ)).const_mul 3).sub hlog).div_const 2).exp using 1
      · rfl
      · simp [b]
    have ht : Tendsto (fun k : ℝ ↦ (u k - 1) / k) (𝓝[≠] 0)
        (𝓝 ((3 - exp 1) / 2)) := by
      simpa [hu0, smul_eq_mul, div_eq_mul_inv, mul_comm] using hdu.tendsto_slope_zero
    have he : Continuous (fun k : ℝ ↦ exp (1 + k)) :=
      continuous_exp.comp (continuous_const.add continuous_id)
    have ht' := ht.div (he.continuousAt.continuousWithinAt (s := {0}ᶜ)) (exp_ne_zero _)
    have hv : (3 - exp 1) / 2 / exp (1 + 0) = a 0 := by
      rw [a_at_zero, add_zero]
      field_simp
    have ha : Tendsto a (𝓝[≠] 0) (𝓝 (a 0)) := by
      rw [hv] at ht'
      apply ht'.congr'
      filter_upwards [self_mem_nhdsWithin] with k hk
      have hne : k ≠ 0 := hk
      simp [a_of_ne hne, b, div_div]
    exact (continuousAt_iff_punctured_nhds.mpr ha).continuousWithinAt
  · apply ContinuousAt.continuousWithinAt
    apply ((hu.sub (continuousAt_const (y := (1 : ℝ)))).div b_cont.continuousAt (b_ne h)).congr
    filter_upwards [isOpen_compl_singleton.mem_nhds (show k ∈ {0}ᶜ from h)] with t ht
    simp [a_of_ne (show t ≠ 0 from ht)]

private theorem exp_compare {k : ℝ} (hk : -1 < k) :
    (0 < k → exp k < 1 + b k) ∧ (k < 0 → 1 + b k < exp k) := by
  constructor
  · intro h
    have hm := mul_lt_mul_of_pos_right
      (add_one_lt_exp (neg_ne_zero.mpr h.ne')) (exp_pos k)
    rw [← exp_add, neg_add_cancel, exp_zero] at hm
    have hb : k * exp k < b k := by
      apply mul_lt_mul_of_pos_left _ h
      exact exp_lt_exp.mpr (by linarith)
    nlinarith
  · intro h
    have hb : b k < k * 1 := mul_lt_mul_of_neg_left
      (one_lt_exp_iff.mpr (by linarith)) h
    linarith [add_one_lt_exp h.ne]

private theorem a_lt_one {k : ℝ} (hk : -1 < k) : a k < 1 := by
  have hp : 0 < 1 + b k := by linarith [b_bound hk]
  rcases lt_trichotomy k 0 with h | rfl | h
  · have hl := log_lt_log hp ((exp_compare hk).2 h)
    rw [log_exp] at hl
    have hu : exp k < u k := exp_lt_exp.mpr (by linarith)
    have hb : b k < 0 := mul_neg_of_neg_of_pos h (exp_pos _)
    rw [a_of_ne h.ne, div_lt_iff_of_neg hb, one_mul]
    linarith [lt_trans ((exp_compare hk).2 h) hu]
  · have hd : 3 / exp 1 < 3 := (div_lt_iff₀ (exp_pos 1)).mpr (by linarith [exp_one_gt_two])
    rw [a_at_zero]
    linarith
  · have hl := log_lt_log (exp_pos k) ((exp_compare hk).1 h)
    rw [log_exp] at hl
    have hu : u k < exp k := exp_lt_exp.mpr (by linarith)
    have hb : 0 < b k := mul_pos h (exp_pos _)
    rw [a_of_ne h.ne', div_lt_iff₀ hb, one_mul]
    linarith [lt_trans hu ((exp_compare hk).1 h)]

private theorem a_zero_pos : 0 < a 0 := by
  have hz : 1 < 3 / exp 1 := (lt_div_iff₀ (exp_pos 1)).mpr (by linarith [exp_one_lt_three])
  rw [a_at_zero]
  linarith

private theorem a_half_gt : a 0 < a (1 / 2) := by
  let E := exp (3 / 2)
  have e₁ : (2.71 : ℝ) < exp 1 := by linarith [exp_one_gt_d9]
  have e₂ : exp 1 < (2.72 : ℝ) := by linarith [exp_one_lt_d9]
  have hE : E ^ 2 = exp 1 ^ 3 := by
    unfold E
    rw [← exp_nat_mul, ← exp_nat_mul]
    norm_num
  have hlow : (4.4 : ℝ) < E := by
    have hp : (2.71 : ℝ) ^ 3 < exp 1 ^ 3 := by gcongr
    by_contra! h
    have hs : E ^ 2 ≤ (4.4 : ℝ) ^ 2 := by gcongr
    rw [hE] at hs
    linarith
  have hhigh : E < (4.5 : ℝ) := by
    have hp : exp 1 ^ 3 < (2.72 : ℝ) ^ 3 := by gcongr
    by_contra! h
    have hs : (4.5 : ℝ) ^ 2 ≤ E ^ 2 := by gcongr
    rw [hE] at hs
    linarith
  have hB : b (1 / 2) = E / 2 := by unfold b E; norm_num; ring
  have h0 : a 0 < 7 / 100 := by
    have hq : 3 / exp 1 < (114 / 100 : ℝ) :=
      (div_lt_iff₀ (exp_pos 1)).mpr (by linarith)
    rw [a_at_zero]
    linarith
  apply h0.trans
  have hU := u_sq (k := 1 / 2) (by norm_num)
  rw [hB, show (3 : ℝ) * (1 / 2) = 3 / 2 by norm_num] at hU
  change u (1 / 2) ^ 2 = E / (1 + E / 2) at hU
  have hsq : (1 + (7 / 100 : ℝ) * (E / 2)) ^ 2 < u (1 / 2) ^ 2 := by
    rw [hU, lt_div_iff₀ (by linarith : 0 < 1 + E / 2)]
    calc
      _ ≤ (1 + (7 / 100 : ℝ) * (4.5 / 2)) ^ 2 * (1 + 4.5 / 2) := by gcongr
      _ < E := by norm_num; linarith
  have hlt : 1 + (7 / 100 : ℝ) * (E / 2) < u (1 / 2) := by
    by_contra! h
    have hh : u (1 / 2) ^ 2 ≤ (1 + (7 / 100 : ℝ) * (E / 2)) ^ 2 := by
      gcongr
      exact (u_pos _).le
    linarith
  rw [a_of_ne (by norm_num : (1 / 2 : ℝ) ≠ 0), hB,
    lt_div_iff₀ (by linarith : 0 < E / 2)]
  linarith

/-- A strictly positive maximum below one is attained away from zero. -/
theorem a_attains_max : ∃ k ∈ Ioi (-1 : ℝ),
    k ≠ 0 ∧ a k ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn a (Ioi (-1)) k := by
  let F := fun k : ℝ ↦ exp (3 * k) - (1 + b k)
  have hF : Continuous F := (continuous_exp.comp (continuous_id.const_mul 3)).sub
    (continuous_const.add b_cont)
  have hleft : 0 < F (-1) := by simpa [F, b] using exp_pos (-3)
  obtain ⟨ε, hε, he⟩ := Metric.mem_nhds_iff.mp
    (hF.continuousAt.preimage_mem_nhds (Ioi_mem_nhds hleft))
  let l := -1 + min ε (1 / 2) / 2
  have hl : -1 < l := by dsimp [l]; linarith [lt_min hε (by norm_num : (0 : ℝ) < 1 / 2)]
  have hl₀ : l < 0 := by dsimp [l]; linarith [min_le_right ε (1 / 2 : ℝ)]
  have hleft' (k : ℝ) (hk : -1 < k) (hkl : k < l) : a k < 0 := by
    have hk₀ : k < 0 := hkl.trans hl₀
    have hmem : k ∈ Metric.ball (-1 : ℝ) ε := by
      rw [Metric.mem_ball, Real.dist_eq, abs_of_pos (by linarith)]
      dsimp [l] at hkl
      linarith [min_le_left ε (1 / 2 : ℝ)]
    have hh : 0 < F k := he hmem
    have hs : 1 < u k ^ 2 := by
      rw [u_sq hk, lt_div_iff₀ (by linarith [b_bound hk] : 0 < 1 + b k)]
      dsimp [F] at hh
      linarith
    have hu : 1 < u k := by
      by_contra! h
      have : u k ^ 2 ≤ 1 ^ 2 := by gcongr; exact (u_pos _).le
      linarith
    rw [a_of_ne hk₀.ne]
    exact div_neg_of_pos_of_neg (sub_pos.mpr hu) (mul_neg_of_neg_of_pos hk₀ (exp_pos _))
  let r := max 1 (a 0)⁻¹
  have hr : 1 ≤ r := le_max_left _ _
  have hright (k : ℝ) (hk : r < k) : a k < a 0 := by
    have hp : 0 < k := by linarith
    have hB : 0 < b k := mul_pos hp (exp_pos _)
    have hlog := log_lt_log (exp_pos k) ((exp_compare (by linarith : -1 < k)).1 hp)
    rw [log_exp] at hlog
    have hu : u k < exp k := exp_lt_exp.mpr (by linarith)
    have hk' : (a 0)⁻¹ < k := (le_max_right _ _).trans_lt hk
    have hka : 1 < k * a 0 := (mul_inv_lt_iff₀ a_zero_pos).mp (by simpa using hk')
    have he : exp k < (a 0) * b k := by
      calc
        exp k = 1 * exp k := (one_mul _).symm
        _ < (k * a 0) * exp k := mul_lt_mul_of_pos_right hka (exp_pos k)
        _ ≤ ((k * a 0) * exp k) * exp 1 :=
          le_mul_of_one_le_right (mul_pos (mul_pos hp a_zero_pos) (exp_pos k)).le
            (by linarith [exp_one_gt_two])
        _ = a 0 * b k := by rw [b, exp_add]; ring
    rw [a_of_ne hp.ne', div_lt_iff₀ hB]
    linarith
  have hm : (1 / 2 : ℝ) ∈ Icc l r := ⟨by linarith, by linarith⟩
  obtain ⟨k, hk, hmax⟩ := isCompact_Icc.exists_isMaxOn ⟨_, hm⟩
    (a_continuous.mono (fun _ ht ↦ hl.trans_le ht.1))
  have hk' : -1 < k := hl.trans_le hk.1
  have hhalf : a (1 / 2) ≤ a k := hmax hm
  have hgt : a 0 < a k := a_half_gt.trans_le hhalf
  refine ⟨k, hk', fun he ↦ (ne_of_gt hgt) (congrArg a he),
    ⟨a_zero_pos.trans hgt, a_lt_one hk'⟩, ?_⟩
  intro t ht
  change a t ≤ a k
  by_cases htl : t < l
  · exact ((hleft' t ht htl).trans (a_zero_pos.trans hgt)).le
  by_cases htr : r < t
  · exact ((hright t htr).trans hgt).le
  exact hmax ⟨le_of_not_gt htl, le_of_not_gt htr⟩

private theorem b_surj {v : ℝ} (hv : -1 < v) : ∃ k ∈ Ioi (-1 : ℝ), b k = v := by
  let r := max 1 v
  have hr : 1 ≤ r := le_max_left _ _
  have he : v ≤ b r := by
    have h := mul_le_mul_of_nonneg_left (show 1 ≤ exp (1 + r) by
      rw [one_le_exp_iff]; linarith) (show 0 ≤ r by linarith)
    have hv' : v ≤ r := le_max_right _ _
    unfold b
    linarith
  have hb : b (-1) = -1 := by simp [b]
  obtain ⟨k, hk, hkv⟩ := intermediate_value_Icc (show (-1 : ℝ) ≤ r by linarith)
    b_cont.continuousOn (show v ∈ Icc (b (-1)) (b r) by rw [hb]; exact ⟨hv.le, he⟩)
  exact ⟨k, lt_of_le_of_ne hk.1 (fun h ↦ by rw [h, hkv] at hb; linarith), hkv⟩

private def D (w v : ℝ) : ℝ := (2 * logRatio w v + logRatio 1 v) / 3

private theorem ratio_mono {v x y : ℝ} (hxy : x ≤ y)
    (hx : 0 < 1 + v * x) (hy : 0 < 1 + v * y) :
    logRatio x v ≤ logRatio y v := by
  rcases lt_trichotomy v 0 with h | rfl | h
  · simp only [logRatio, ite_eq_right h.ne]
    apply (div_le_div_right_of_neg h).mpr
    apply strictMonoOn_log.monotoneOn hy hx
    nlinarith [mul_nonpos_of_nonpos_of_nonneg h.le (sub_nonneg.mpr hxy)]
  · simpa [logRatio] using hxy
  · simp only [logRatio, ite_eq_right h.ne']
    apply (div_le_div_iff_of_pos_right h).mpr
    apply strictMonoOn_log.monotoneOn hx hy
    nlinarith [mul_nonneg h.le (sub_nonneg.mpr hxy)]

private theorem D_identity {k : ℝ} (hk : -1 < k) : D (a k) (b k) = exp (-1 - k) := by
  by_cases h : k = 0
  · subst k
    simp [D, a, b, logRatio, exp_neg]
    field_simp
    ring
  · simp only [D, logRatio, ite_eq_right (b_ne h), a_identity h, mul_one, u, log_exp]
    field_simp [b_ne h]
    have he : b k * exp (-1 - k) = k := by
      rw [b, mul_assoc, ← exp_add, show (1 + k) + (-1 - k) = 0 by ring, exp_zero, mul_one]
    nlinarith only [he]

private theorem ratio_rescale (y : ℝ) {σ ξ : ℝ} (hσ : 0 < σ) :
    logRatio (y / σ) ξ = logRatio y (ξ / σ) / σ := by
  by_cases hξ : ξ = 0
  · simp [hξ, logRatio]
  · simp only [logRatio, ite_eq_right hξ, ite_eq_right (div_ne_zero hξ hσ.ne')]
    rw [show ξ * (y / σ) = ξ / σ * y by ring]
    field_simp

/-- The three excesses in the exact tie construction. -/
def tieSample (w : ℝ) : Fin 3 → ℝ := ![w, w, 1]

private theorem tieFormula (w ξ : ℝ) {σ : ℝ} (hσ : 0 < σ) :
    logLik (tieSample w) σ ξ = 3 * (-log σ - (1 + ξ) * (D w (ξ / σ) / σ)) := by
  have h0 : tieSample w 0 = w := rfl
  have h1 : tieSample w 1 = w := rfl
  have h2 : tieSample w 2 = 1 := rfl
  simp only [logLik, Fin.sum_univ_three, h0, h1, h2, logDensity, ratio_rescale _ hσ, D]
  ring

private theorem uniform_tie {w : ℝ} (hw : w ∈ Ioo (0 : ℝ) 1) :
    (1 : ℝ) ∈ scaleDomain (tieSample w) (-1) ∧ logLik (tieSample w) 1 (-1) = 0 := by
  refine ⟨⟨zero_lt_one, ?_⟩, ?_⟩
  · intro i
    fin_cases i <;> simp [tieSample, hw.1.le, hw.2.le]
  · rw [tieFormula _ _ zero_lt_one]
    norm_num

private theorem interior_tie {k : ℝ} (hk : -1 < k) (hne : k ≠ 0)
    (hw : a k ∈ Ioo (0 : ℝ) 1) :
    exp (-1 - k) ∈ scaleDomain (tieSample (a k)) k ∧
      logLik (tieSample (a k)) (exp (-1 - k)) k = 0 := by
  let σ := exp (-1 - k)
  have hσ : 0 < σ := exp_pos _
  have hb : k / σ = b k := by
    change k / exp (-1 - k) = b k
    rw [b, show -1 - k = -(1 + k) by ring, exp_neg, div_inv_eq_mul]
  have hp (i : Fin 3) : 0 < 1 + b k * tieSample (a k) i := by
    fin_cases i
    · simpa [tieSample, a_identity hne] using u_pos k
    · simpa [tieSample, a_identity hne] using u_pos k
    · simpa [tieSample] using (show 0 < 1 + b k by linarith [b_bound hk])
  refine ⟨⟨hσ, fun i ↦ ⟨?_, ?_⟩⟩, ?_⟩
  · fin_cases i <;> simp [tieSample, hw.1.le]
  · simp only [ite_eq_right (ne_of_gt hk)]
    have hh := mul_pos hσ (hp i)
    rw [← hb] at hh
    change 0 < σ + k * tieSample (a k) i
    convert hh using 1
    field_simp [hσ.ne']
  · rw [tieFormula _ _ hσ, hb, D_identity hk, div_self hσ.ne', log_exp]
    ring

private theorem tie_global_bound {w : ℝ}
    (hmax : ∀ k ∈ Ioi (-1 : ℝ), a k ≤ w) {ξ σ : ℝ} (hξ : -1 ≤ ξ)
    (hs : σ ∈ scaleDomain (tieSample w) ξ) :
    logLik (tieSample w) σ ξ ≤ 0 := by
  have hσ := hs.1
  by_cases he : ξ = -1
  · subst ξ
    have h1 : 1 ≤ σ := by simpa [tieSample] using (hs.2 (2 : Fin 3)).2
    rw [tieFormula _ _ hσ]
    have := log_nonneg h1
    linarith
  let v := ξ / σ
  have hsup (i : Fin 3) : 0 < 1 + v * tieSample w i := by
    have hi : 0 < σ + ξ * tieSample w i := by simpa [he] using (hs.2 i).2
    convert div_pos hi hσ using 1
    dsimp [v]
    field_simp
  have hv : -1 < v := by
    have h : 0 < 1 + v := by simpa [tieSample] using hsup 2
    linarith
  obtain ⟨k, hk, hkv⟩ := b_surj hv
  let τ := exp (-1 - k)
  have hτ : 0 < τ := exp_pos _
  have hcomp : τ ≤ D w v := by
    change exp (-1 - k) ≤ D w v
    rw [← hkv, ← D_identity hk]
    have ha : 0 < 1 + b k * a k := by
      by_cases hz : k = 0
      · simp [hz, b]
      · rw [a_identity hz]
        exact u_pos _
    have haw : 0 < 1 + b k * w := by
      rw [hkv]
      simpa [tieSample] using hsup 0
    have hm := ratio_mono (hmax k hk) ha haw
    unfold D
    linarith
  have hvτ : v * τ = k := by
    rw [← hkv, b]
    dsimp [τ]
    rw [mul_assoc, ← exp_add, show (1 + k) + (-1 - k) = 0 by ring, exp_zero, mul_one]
  have hcoef : 0 ≤ 1 + ξ := by linarith
  have hc := mul_le_mul_of_nonneg_left
    (div_le_div_of_nonneg_right hcomp hσ.le) hcoef
  have hl := log_le_sub_one_of_pos (div_pos hτ hσ)
  rw [log_div hτ.ne' hσ.ne', show log τ = -1 - k by exact log_exp _] at hl
  have heq : (1 + ξ) * (τ / σ) = τ / σ + k := by
    rw [← hvτ]
    dsimp [v]
    ring
  rw [tieFormula _ _ hσ]
  nlinarith only [hc, hl, heq]

/-- The dossier's sample admits both the interior and the uniform member
as global maximizers, with total log-likelihood zero. -/
theorem exact_joint_tie :
    ∃ k ∈ Ioi (-1 : ℝ), k ≠ 0 ∧ a k ∈ Ioo (0 : ℝ) 1 ∧
      IsMaxOn a (Ioi (-1)) k ∧
      exp (-1 - k) ∈ scaleDomain (tieSample (a k)) k ∧
      (1 : ℝ) ∈ scaleDomain (tieSample (a k)) (-1) ∧
      logLik (tieSample (a k)) (exp (-1 - k)) k = 0 ∧
      logLik (tieSample (a k)) 1 (-1) = 0 ∧
      ∀ ξ ≥ -1, ∀ σ ∈ scaleDomain (tieSample (a k)) ξ,
        logLik (tieSample (a k)) σ ξ ≤ 0 := by
  obtain ⟨k, hk, hn, ha, hm⟩ := a_attains_max
  obtain ⟨hi, hli⟩ := interior_tie hk hn ha
  obtain ⟨hu, hlu⟩ := uniform_tie ha
  exact ⟨k, hk, hn, ha, hm, hi, hu, hli, hlu,
    fun ξ hξ σ hs ↦ tie_global_bound (fun _ h ↦ hm h) hξ hs⟩

private theorem ratio_pos {y v : ℝ} (hy : 0 < y) (hv : 0 < 1 + v * y) :
    0 < logRatio y v := by
  rcases lt_trichotomy v 0 with h | rfl | h
  · rw [logRatio, ite_eq_right h.ne]
    exact div_pos_of_neg_of_neg (log_neg hv (by linarith [mul_neg_of_neg_of_pos h hy])) h
  · simpa [logRatio] using hy
  · rw [logRatio, ite_eq_right h.ne']
    exact div_pos (log_pos (by linarith [mul_pos h hy])) h

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

private def meanRatio (y : Fin n → ℝ) (v : ℝ) : ℝ := (∑ i, logRatio (y i) v) / n

private theorem likelihood_profile_formula (y : Fin n → ℝ) {σ ξ : ℝ}
    (hn : 0 < n) (hσ : 0 < σ) :
    logLik y σ ξ = (n : ℝ) * (-log σ - (1 + ξ) * (meanRatio y (ξ / σ) / σ)) := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp only [logLik, logDensity, ratio_rescale _ hσ, meanRatio,
    Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.mul_sum, ← Finset.sum_div]
  field_simp

private theorem scale_comparison {d σ : ℝ} (hd : 0 < d) (hσ : 0 < σ) :
    -log σ - d / σ ≤ -log d - 1 := by
  have hh := log_le_sub_one_of_pos (div_pos hd hσ)
  rw [log_div hd.ne' hσ.ne'] at hh
  linarith

/-- The GPD has an attained joint maximum over scale and shape for
strictly positive excesses under the closed shape floor. -/
theorem joint_exists (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i) :
    ∃ σ ξ, -1 ≤ ξ ∧ σ ∈ scaleDomain y ξ ∧
      ∀ κ ≥ -1, ∀ s ∈ scaleDomain y κ, logLik y s κ ≤ logLik y σ ξ := by
  obtain ⟨j, hj⟩ := exists_max_index hn y
  obtain ⟨k, hk⟩ := exists_max_index hn (fun i ↦ -y i)
  have hmin (i : Fin n) : y k ≤ y i := neg_le_neg_iff.mp (hk i)
  let M := y j
  let m := y k
  have hM : 0 < M := hy j
  have hm : 0 < m := hy k
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hne : (Finset.univ : Finset (Fin n)).Nonempty := ⟨j, Finset.mem_univ _⟩
  let V := Ioi (-1 / M)
  let d := meanRatio y
  let F := fun v : ℝ ↦ -log (d v) - v * d v - 1
  have hsupport {v : ℝ} (hv : v ∈ V) (i : Fin n) : 0 < 1 + v * y i := by
    by_cases h : 0 ≤ v
    · linarith [mul_nonneg h (hy i).le]
    · have hh := (div_lt_iff₀ hM).mp hv
      have haux := mul_le_mul_of_nonpos_left (hj i) (le_of_not_ge h)
      change v * y i ≥ v * M at haux
      linarith
  have hd {v : ℝ} (hv : v ∈ V) : 0 < d v := by
    apply div_pos _ hN
    exact Finset.sum_pos (fun i _ ↦ ratio_pos (hy i) (hsupport hv i)) hne
  have hdc : ContinuousOn d V := by
    apply ContinuousOn.div_const
    exact continuousOn_finsetSum _ fun i _ v hv ↦
      (ratio_continuousAt (y i) v (hsupport hv i)).continuousWithinAt
  have hFc : ContinuousOn F V :=
    (((hdc.log (fun _ hv ↦ (hd hv).ne')).neg).sub (continuousOn_id.mul hdc)).sub
      continuousOn_const
  let l := (exp (-(n : ℝ)) - 1) / M
  let r := (exp (M / m + 1) - 1) / m
  have hl : -1 / M < l := (div_lt_div_iff_of_pos_right hM).mpr (by linarith [exp_pos (-(n : ℝ))])
  have hl₀ : l ≤ 0 := div_nonpos_of_nonpos_of_nonneg
    (sub_nonpos.mpr (exp_le_one_iff.mpr (by linarith))) hM.le
  have hr₀ : 0 < r := div_pos (sub_pos.mpr (one_lt_exp_iff.mpr (by positivity))) hm
  let I := Icc l r
  let K := I ∩ (fun v : ℝ ↦ v * d v) ⁻¹' Ici (-1)
  have hIV : I ⊆ V := fun v hv ↦ hl.trans_le hv.1
  have hKV : K ⊆ V := fun v hv ↦ hIV hv.1
  have hK : IsCompact K :=
    (continuousOn_id.mul (hdc.mono hIV)).upperSemicontinuousOn.isCompact_inter_preimage_Ici
      isCompact_Icc (-1)
  have hK₀ : (0 : ℝ) ∈ K := by
    refine ⟨⟨hl₀, hr₀.le⟩, ?_⟩
    change -1 ≤ 0 * d 0
    simp
  obtain ⟨v₀, hv₀, hopt⟩ := hK.exists_isMaxOn ⟨0, hK₀⟩ (hFc.mono hKV)
  have hd₀ := hd (hKV hv₀)
  have hshape : -1 ≤ v₀ * d v₀ := hv₀.2
  have hscale : d v₀ ∈ scaleDomain y (v₀ * d v₀) := by
    refine ⟨hd₀, fun i ↦ ⟨(hy i).le, ?_⟩⟩
    have hp := mul_pos hd₀ (hsupport (hKV hv₀) i)
    rw [show d v₀ * (1 + v₀ * y i) = d v₀ + (v₀ * d v₀) * y i by ring] at hp
    by_cases he : v₀ * d v₀ = -1
    · simp [he] at hp ⊢
      linarith
    · simpa [he] using hp
  have hprof : logLik y (d v₀) (v₀ * d v₀) = (n : ℝ) * F v₀ := by
    rw [likelihood_profile_formula y hn hd₀, mul_div_cancel_right₀ _ hd₀.ne',
      div_self hd₀.ne']
    unfold F
    ring
  have huni : M ∈ scaleDomain y (-1) :=
    ⟨hM, fun i ↦ ⟨(hy i).le, by simpa [M] using hj i⟩⟩
  have huni_value : logLik y M (-1) = (n : ℝ) * (-log M) := by
    simp [logLik, logDensity]
  have hbound (ξ σ : ℝ) (hξ : -1 ≤ ξ) (hs : σ ∈ scaleDomain y ξ) :
      logLik y σ ξ ≤ max (logLik y M (-1)) (logLik y (d v₀) (v₀ * d v₀)) := by
    have hσ : 0 < σ := hs.1
    by_cases he : ξ = -1
    · subst ξ
      apply le_trans _ (le_max_left _ _)
      rw [huni_value, likelihood_profile_formula y hn hσ]
      have hle : M ≤ σ := by simpa [M] using (hs.2 j).2
      have hh := strictMonoOn_log.monotoneOn hM hσ hle
      norm_num
      nlinarith [mul_le_mul_of_nonneg_left hh hN.le]
    let v := ξ / σ
    have hv : v ∈ V := by
      have hsu : 0 < σ + ξ * M := by simpa [he, M] using (hs.2 j).2
      change -1 / M < ξ / σ
      rw [lt_div_iff₀ hσ]
      have haux : -σ / M < ξ := (div_lt_iff₀ hM).mpr (by linarith)
      convert haux using 1
      ring
    have hdv := hd hv
    have hformula := likelihood_profile_formula y hn hσ (ξ := ξ)
    have hvσ : v * σ = ξ := div_mul_cancel₀ _ hσ.ne'
    have hunrestricted : logLik y σ ξ ≤ (n : ℝ) * F v := by
      rw [hformula]
      apply mul_le_mul_of_nonneg_left _ hN.le
      dsimp only [F]
      have hh := scale_comparison hdv hσ
      have hterm : (1 + ξ) * (d v / σ) = d v / σ + v * d v := by
        rw [← hvσ]
        field_simp
      change -log σ - (1 + ξ) * (d v / σ) ≤ _
      linarith
    -- A conditional profile below the shape floor is dominated by the uniform.
    have hconstrained (hc : v * d v ≤ -1) :
        logLik y σ ξ ≤ logLik y M (-1) := by
      have hv' : v < 0 := by
        by_contra! hh
        linarith [mul_nonneg hh hdv.le]
      let t := -1 / v
      have ht : 0 < t := div_pos_of_neg_of_neg (by norm_num) hv'
      have ht' : t ≤ d v := (div_le_iff_of_neg hv').mpr (by nlinarith)
      have hMt : M ≤ t := (le_div_iff_of_neg hv').mpr (by
        have hh := hsupport hv j
        change 0 < 1 + v * M at hh
        linarith)
      have hcoef : 0 ≤ 1 + ξ := by linarith
      have hcomp := mul_le_mul_of_nonneg_left (div_le_div_of_nonneg_right ht' hσ.le) hcoef
      have hterm : (1 + ξ) * (t / σ) = t / σ - 1 := by
        rw [← hvσ]
        dsimp [t]
        field_simp [hv'.ne]
        ring
      have hlog := strictMonoOn_log.monotoneOn hM ht hMt
      have hsc := scale_comparison ht hσ
      rw [hformula, huni_value]
      apply mul_le_mul_of_nonneg_left _ hN.le
      change -log σ - (1 + ξ) * (d v / σ) ≤ _
      linarith
    -- The maximum excess excludes the left tail through the shape floor.
    by_cases hvl : v < l
    · apply le_trans (hconstrained _) (le_max_left _ _)
      have hvneg : v < 0 := hvl.trans_le hl₀
      have hsum : v * d v = (∑ i, log (1 + v * y i)) / n := by
        simp only [d, meanRatio, logRatio, ite_eq_right hvneg.ne, ← Finset.sum_div]
        field_simp [hvneg.ne]
      have hlogs (i : Fin n) : log (1 + v * y i) ≤ 0 :=
        log_nonpos (hsupport hv i).le (by linarith [mul_neg_of_neg_of_pos hvneg (hy i)])
      classical
      rw [hsum, div_le_iff₀ hN, ← Finset.sum_erase_add _ _ (Finset.mem_univ j)]
      have hs₀ := Finset.sum_nonneg (s := Finset.univ.erase j)
        (fun i _ ↦ neg_nonneg.mpr (hlogs i))
      rw [Finset.sum_neg_distrib] at hs₀
      have hjb : 1 + v * M ≤ exp (-(n : ℝ)) := by
        have hh := (lt_div_iff₀ hM).mp hvl
        linarith
      have hjl := strictMonoOn_log.monotoneOn (hsupport hv j) (exp_pos _) hjb
      rw [log_exp] at hjl
      linarith
    -- The minimum excess bounds the right tail of the unrestricted profile.
    by_cases hvr : r < v
    · apply le_trans hunrestricted
      rw [huni_value]
      apply le_trans _ (le_max_left _ _)
      apply mul_le_mul_of_nonneg_left _ hN.le
      have hvp : 0 < v := hr₀.trans hvr
      let L := log (1 + v * m)
      have hL : M / m + 1 ≤ L := by
        have hh := (div_lt_iff₀ hm).mp hvr
        have hle : exp (M / m + 1) ≤ 1 + v * m := by linarith
        simpa [L] using strictMonoOn_log.monotoneOn (exp_pos (M / m + 1)) (hsupport hv k) hle
      have hLp : 0 < L := by linarith [div_pos hM hm]
      have hratio : L / v ≤ d v := by
        apply (le_div_iff₀ hN).mpr
        have hh := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦
          ratio_mono (hmin i) (hsupport hv k) (hsupport hv i))
        simpa [logRatio, hvp.ne', L, m, mul_comm] using hh
      have hlog := strictMonoOn_log.monotoneOn (div_pos hLp hvp) hdv hratio
      rw [log_div hLp.ne' hvp.ne'] at hlog
      have hmul := (div_le_iff₀ hvp).mp hratio
      have hLL := strictMonoOn_log.monotoneOn (div_pos hM hm) hLp
        (show M / m ≤ L by linarith)
      rw [log_div hM.ne' hm.ne'] at hLL
      have hvm := strictMonoOn_log.monotoneOn (mul_pos hvp hm) (hsupport hv k)
        (show v * m ≤ 1 + v * m by linarith)
      rw [log_mul hvp.ne' hm.ne'] at hvm
      dsimp [F]
      linarith
    by_cases hc : v * d v ≤ -1
    · exact (hconstrained hc).trans (le_max_left _ _)
    · have hmem : v ∈ K := ⟨⟨le_of_not_gt hvl, le_of_not_gt hvr⟩, le_of_lt (lt_of_not_ge hc)⟩
      have hle : F v ≤ F v₀ := hopt hmem
      rw [hprof]
      exact hunrestricted.trans ((mul_le_mul_of_nonneg_left hle hN.le).trans (le_max_right _ _))
  by_cases hwin : logLik y (d v₀) (v₀ * d v₀) ≤ logLik y M (-1)
  · refine ⟨M, -1, le_rfl, huni, fun ξ hξ σ hσ ↦ ?_⟩
    simpa only [max_eq_left hwin] using hbound ξ σ hξ hσ
  · refine ⟨d v₀, v₀ * d v₀, hshape, hscale, fun ξ hξ σ hσ ↦ ?_⟩
    simpa only [max_eq_right (le_of_not_ge hwin)] using hbound ξ σ hξ hσ

end

end UnivariateML.GPD
