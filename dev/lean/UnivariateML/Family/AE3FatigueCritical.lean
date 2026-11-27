module

public import UnivariateML.Family.AE2Fatigue
import Mathlib.Analysis.Calculus.Deriv.MeanValue
import Mathlib.Analysis.Calculus.Deriv.Pow
import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Tactic

set_option linter.style.header false

/-! Exact Birnbaum–Saunders singleton on the critical sample (0, 1, 1). -/

@[expose] public section

open Set Real

namespace UnivariateML.AE3FatigueCritical

noncomputable section

private def profileDenominator (t : ℝ) : ℝ := 3 * t^3 - t^2 - t + 1
private def shapeSquared (t : ℝ) : ℝ := ((t + 1 / t - 2) + 2 * (t + 1 + 1 / (t + 1) - 2)) / 3

private theorem shapeSquared_pos {t : ℝ} (ht : 0 < t) : 0 < shapeSquared t := by
  have h1 : 0 ≤ t + 1 / t - 2 := by
    apply (nonneg_of_mul_nonneg_left _ ht)
    rw [sub_mul, add_mul, div_mul_cancel₀ _ ht.ne']
    nlinarith [sq_nonneg (t - 1)]
  have h2 : 0 < t + 1 + 1 / (t + 1) - 2 := by
    have hp : 0 < t+1 := by positivity
    apply (pos_of_mul_pos_left _ hp.le)
    rw [sub_mul, add_mul, div_mul_cancel₀ _ hp.ne']
    nlinarith [sq_pos_of_pos ht]
  unfold shapeSquared
  linarith

private theorem shapeSquared_eq {t : ℝ} (ht : 0 < t) :
    shapeSquared t = profileDenominator t / (3 * t * (t + 1)) := by
  unfold shapeSquared profileDenominator
  field_simp [ht.ne', (show 0 < t + 1 by positivity).ne']
  ring

private def stationaryNumerator (t : ℝ) : ℝ := 9 * t^4 + 36 * t^3 + 19 * t^2 - 11 * t - 4
private def thresholdProfile (t : ℝ) : ℝ :=
  2 * log (t+2) - log (t+1) / 2 - 3 * log (profileDenominator t) / 2

private theorem profileDenominator_pos {t : ℝ} (ht : 0 < t) : 0 < profileDenominator t := by
  have h : 0 < profileDenominator t / (3*t*(t+1)) := shapeSquared_eq ht ▸ shapeSquared_pos ht
  exact (div_pos_iff_of_pos_right (by positivity : 0 < 3*t*(t+1))).mp h

private theorem thresholdProfile_hasDeriv {t : ℝ} (ht : 0 < t) :
    HasDerivAt thresholdProfile
      (-stationaryNumerator t / ((t + 2) * (t + 1) * profileDenominator t)) t := by
  have h1 := (hasDerivAt_id t).add_const 1
  have h2 := (hasDerivAt_id t).add_const 2
  have hP := (((((hasDerivAt_id t).pow 3).const_mul 3).sub
    ((hasDerivAt_id t).pow 2)).sub (hasDerivAt_id t)).add_const 1
  have hp : HasDerivAt profileDenominator (9*t^2-2*t-1) t := by
    convert hP using 1
    · rfl
    · norm_num
      ring
  have hp1 : 0 < t+1 := by positivity
  have hp2 : 0 < t+2 := by positivity
  have hPLog := ((hp.log (profileDenominator_pos ht).ne').const_mul 3).div_const 2
  have hh := (((h2.log hp2.ne').const_mul 2).sub ((h1.log hp1.ne').div_const 2)).sub hPLog
  convert hh using 1
  · rfl
  · simp only [id_eq, stationaryNumerator]
    field_simp [hp1.ne',hp2.ne',(profileDenominator_pos ht).ne']
    unfold profileDenominator
    ring

private theorem stationaryRatio_strictMono :
    StrictMonoOn (fun t : ℝ ↦ stationaryNumerator t / t) (Ioi 0) := by
  intro a ha b hb hab
  have ha' : 0 < a := ha
  have hb' : 0 < b := hb
  have he (x : ℝ) (hx : 0 < x) :
      stationaryNumerator x / x = 9*x^3 + 36*x^2 + 19*x - 11 - 4/x := by
    unfold stationaryNumerator
    field_simp [hx.ne']
  change stationaryNumerator a / a < stationaryNumerator b / b
  rw [he a ha', he b hb']
  have h1 := pow_lt_pow_left₀ hab ha'.le (by norm_num : 3 ≠ 0)
  have h2 := pow_lt_pow_left₀ hab ha'.le (by norm_num : 2 ≠ 0)
  have h3 := div_lt_div_of_pos_left (by norm_num : 0 < (4 : ℝ)) ha' hab
  linarith

private theorem stationaryRoot_exists : ∃ t ∈ Ioi (0 : ℝ), stationaryNumerator t = 0 := by
  have hcont : Continuous stationaryNumerator := by unfold stationaryNumerator; fun_prop
  have h0 : stationaryNumerator 0 < 0 := by norm_num [stationaryNumerator]
  have h1 : 0 < stationaryNumerator 1 := by norm_num [stationaryNumerator]
  obtain ⟨t, ht, h⟩ := intermediate_value_Icc (by norm_num : (0 : ℝ) ≤ 1) hcont.continuousOn
    (show (0 : ℝ) ∈ Icc (stationaryNumerator 0) (stationaryNumerator 1) from ⟨h0.le, h1.le⟩)
  have hne : t ≠ 0 := by rintro rfl; linarith
  exact ⟨t, lt_of_le_of_ne ht.1 hne.symm, h⟩

private theorem thresholdProfile_unique :
    ∃! t, t ∈ Ioi (0 : ℝ) ∧ IsMaxOn thresholdProfile (Ioi 0) t := by
  obtain ⟨t, ht, hR⟩ := stationaryRoot_exists
  have ht' : 0 < t := ht
  have hleft : StrictMonoOn thresholdProfile (Ioc 0 t) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioc _ _)
    · exact fun x hx ↦ (thresholdProfile_hasDeriv hx.1).continuousAt.continuousWithinAt
    · intro x hx
      have hx' : x ∈ Ioo 0 t := by simpa using hx
      have he := stationaryRatio_strictMono hx'.1 ht hx'.2
      change stationaryNumerator x / x < stationaryNumerator t / t at he
      rw [hR, zero_div] at he
      rw [(thresholdProfile_hasDeriv hx'.1).deriv]
      have hn : stationaryNumerator x < 0 := by
        by_contra! hh
        exact he.not_ge (div_nonneg hh hx'.1.le)
      have hp := profileDenominator_pos hx'.1
      have hx₀ : 0 < x := hx'.1
      exact div_pos (neg_pos.mpr hn) (by positivity)
  have hright : StrictAntiOn thresholdProfile (Ici t) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici _)
    · exact fun x hx ↦ (thresholdProfile_hasDeriv (ht'.trans_le hx)).continuousAt.continuousWithinAt
    · intro x hx
      have htx : t < x := by simpa using hx
      have hx' := ht'.trans htx
      have he := stationaryRatio_strictMono ht hx' htx
      change stationaryNumerator t / t < stationaryNumerator x / x at he
      rw [hR, zero_div] at he
      rw [(thresholdProfile_hasDeriv hx').deriv]
      exact div_neg_of_neg_of_pos (neg_neg_of_pos ((div_pos_iff_of_pos_right hx').mp he))
        (by have := profileDenominator_pos hx'; positivity)
  have hlt (x : ℝ) (hx : 0 < x) (hne : x ≠ t) : thresholdProfile x < thresholdProfile t := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hleft ⟨hx,h.le⟩ ⟨ht',le_rfl⟩ h
    · exact hright (show t ∈ Ici t by simp) h.le h
  refine ⟨t, ⟨ht, ?_⟩, ?_⟩
  · intro x hx
    rcases eq_or_ne x t with rfl | hne
    · change thresholdProfile x ≤ thresholdProfile x
      exact le_rfl
    · exact (hlt x hx hne).le
  · intro x ⟨hx, h⟩
    by_contra hn
    exact (hlt x hx hn).not_ge (h ht)
private def parameterDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private def profileObjective (z : ℝ × ℝ) : ℝ :=
  2 * log (z.2+2) - 2 * log (z.2+1) - 3 * log z.2 / 2 -
    3 * log z.1 - 3 * shapeSquared z.2 / (2 * z.1^2)

private theorem profile_eq {t : ℝ} (ht : 0 < t) :
    profileObjective (sqrt (shapeSquared t), t) = thresholdProfile t + (3 * log 3 / 2 - 3 / 2) := by
  have hq := shapeSquared_pos ht
  have hP := profileDenominator_pos ht
  have ht1 : 0 < t+1 := by positivity
  have hthree : (0 : ℝ) < 3 := by norm_num
  unfold profileObjective thresholdProfile
  dsimp only
  rw [sq_sqrt hq.le, log_sqrt hq.le]
  have he : log (shapeSquared t) = log (profileDenominator t) - (log 3 + log t + log (t+1)) := by
    rw [shapeSquared_eq ht, log_div hP.ne' (by positivity), log_mul (by positivity) ht1.ne',
      log_mul hthree.ne' ht.ne']
  rw [he, show 3 * shapeSquared t / (2 * shapeSquared t) = 3 / 2 by field_simp [hq.ne']]
  ring

private theorem shape_at_threshold {a t : ℝ} (ha : 0 < a) (ht : 0 < t) :
    profileObjective (a,t) ≤ profileObjective (sqrt (shapeSquared t), t) ∧
      (profileObjective (a, t) = profileObjective (sqrt (shapeSquared t), t) →
        a = sqrt (shapeSquared t)) := by
  have hq := shapeSquared_pos ht
  have hA : 0 < a^2 := by positivity
  have hu : 0 < shapeSquared t / a^2 := div_pos hq hA
  have hlog : log (shapeSquared t / a^2) = log (shapeSquared t) - 2 * log a := by
    rw [log_div hq.ne' hA.ne', log_pow]
    norm_num
  have he : profileObjective (a,t) - profileObjective (sqrt (shapeSquared t),t) =
      (3/2) * (log (shapeSquared t / a^2) - shapeSquared t / a^2 + 1) := by
    unfold profileObjective
    dsimp only
    rw [sq_sqrt hq.le, log_sqrt hq.le, hlog,
      show 3 * shapeSquared t / (2 * shapeSquared t) = 3 / 2 by field_simp [hq.ne']]
    ring
  refine ⟨by linarith [log_le_sub_one_of_pos hu], ?_⟩
  intro hh
  by_contra hne
  have hne' : shapeSquared t / a^2 ≠ 1 := by
    intro heq
    have hh := (div_eq_one_iff_eq hA.ne').mp heq
    exact hne (by rw [hh, sqrt_sq ha.le])
  linarith [log_lt_sub_one_of_pos hu hne']

private theorem profileObjective_unique :
    ∃! z, z ∈ parameterDomain ∧ IsMaxOn profileObjective parameterDomain z := by
  obtain ⟨t, ⟨ht, hm⟩, hu⟩ := thresholdProfile_unique
  have ht' : 0 < t := ht
  let a := sqrt (shapeSquared t)
  have hz : (a,t) ∈ parameterDomain := ⟨sqrt_pos.mpr (shapeSquared_pos ht'), ht⟩
  have hc (x : ℝ) (hx : 0 < x) :
      profileObjective (sqrt (shapeSquared x), x) ≤ profileObjective (a, t) := by
    rw [profile_eq hx, profile_eq ht']
    have hh : thresholdProfile x ≤ thresholdProfile t := hm (show x ∈ Ioi 0 from hx)
    exact add_le_add_left hh _
  refine ⟨(a,t), ⟨hz, ?_⟩, ?_⟩
  · intro z h
    exact (shape_at_threshold h.1 h.2).1.trans (hc z.2 h.2)
  · intro z ⟨hz', hm'⟩
    have hMax : IsMaxOn thresholdProfile (Ioi 0) z.2 := by
      intro x hx
      have hsx : (sqrt (shapeSquared x), x) ∈ parameterDomain :=
        ⟨sqrt_pos.mpr (shapeSquared_pos hx), hx⟩
      have h := hm' hsx
      have he := h.trans (shape_at_threshold hz'.1 hz'.2).1
      rw [profile_eq hx, profile_eq hz'.2] at he
      exact le_of_add_le_add_right he
    have he := hu z.2 ⟨hz'.2, hMax⟩
    have hc' : (sqrt (shapeSquared z.2), z.2) ∈ parameterDomain :=
      ⟨sqrt_pos.mpr (shapeSquared_pos hz'.2), hz'.2⟩
    have ha := (shape_at_threshold hz'.1 hz'.2).2
      (le_antisymm (shape_at_threshold hz'.1 hz'.2).1 (hm' hc'))
    exact Prod.ext (ha.trans (congrArg (fun s ↦ sqrt (shapeSquared s)) he)) he

private theorem sampleLikelihood_eq (z : ℝ × ℝ) :
    AE2Fatigue.likelihood (fun p : ℝ × ℝ ↦ ⟨p.1, 1, -p.2⟩)
      (fun i : Fin 2 ↦ if i = 0 then 1 else 2) z (fun i ↦ if i = 0 then 0 else 1) =
        profileObjective z := by
  unfold AE2Fatigue.likelihood profileObjective shapeSquared
  rw [Fin.sum_univ_two]
  norm_num [add_comm]
  ring_nf

/-- With scale one, the sample consisting of zero once and one twice has
exactly one finite maximizing shape and threshold below zero. The second
coordinate is the positive distance from the threshold to zero. -/
theorem held_beta_critical_sample_unique :
    let S : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun z : ℝ × ℝ ↦ AE2Fatigue.likelihood (fun p : ℝ × ℝ ↦ ⟨p.1, 1, -p.2⟩)
      (fun i : Fin 2 ↦ if i = 0 then 1 else 2) z (fun i ↦ if i = 0 then 0 else 1)
    ∃! z, z ∈ S ∧ IsMaxOn L S z := by
  have he : (fun z : ℝ × ℝ ↦ AE2Fatigue.likelihood (fun p : ℝ × ℝ ↦ ⟨p.1, 1, -p.2⟩)
      (fun i : Fin 2 ↦ if i = 0 then 1 else 2) z (fun i ↦ if i = 0 then 0 else 1)) =
      profileObjective := funext sampleLikelihood_eq
  dsimp only
  rw [he]
  exact profileObjective_unique

end

end UnivariateML.AE3FatigueCritical

end
