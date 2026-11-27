module

public import UnivariateML.Family.HeldSweepGEV

import Mathlib.Analysis.SpecialFunctions.Trigonometric.DerivHyp

set_option linter.style.header false

/-!
Sharp held-minimum likelihood comparison on the fitted GEV shape interval.

## Deviations

The sharp comparison addresses the exact half-contact branch with location held. All-free
shape comparison and the other nonconstant held-location branches require additional
results.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.Shape2GEV

noncomputable section

private def logKernel (ξ z : ℝ) : ℝ :=
  if ξ = 0 then -z - exp (-z)
  else (-1 / ξ - 1) * log (1 + ξ * z) - (1 + ξ * z) ^ (-1 / ξ)

private theorem convex_sinh : ConvexOn ℝ (Ici 0) sinh := by
  apply convexOn_of_deriv2_nonneg' (convex_Ici 0) Real.differentiable_sinh.differentiableOn
    (by rw [Real.deriv_sinh]; exact Real.differentiable_cosh.differentiableOn)
  intro v hv
  change 0 ≤ deriv (deriv sinh) v
  rw [Real.deriv_sinh, Real.deriv_cosh]
  exact sinh_nonneg_iff.mpr hv

private theorem sinh_ratio_le {ξ u : ℝ} (hξ : |ξ| ≤ 1) (hξ₀ : ξ ≠ 0) (hu : 0 ≤ u) :
    sinh (ξ * u) / ξ ≤ sinh u := by
  have ha : 0 < |ξ| := abs_pos.mpr hξ₀
  have h := convex_sinh.2 (show u ∈ Ici (0 : ℝ) from hu)
    (show (0 : ℝ) ∈ Ici 0 from le_rfl) ha.le (sub_nonneg.mpr hξ)
      (show |ξ| + (1 - |ξ|) = 1 by ring)
  simp only [smul_eq_mul, mul_zero, add_zero, sinh_zero] at h
  have he : sinh (ξ * u) / ξ = sinh (|ξ| * u) / |ξ| := by
    rcases le_total 0 ξ with hξp | hξn
    · rw [abs_of_nonneg hξp]
    · rw [abs_of_nonpos hξn, neg_mul, sinh_neg, neg_div_neg_eq]
  rw [he]
  exact (div_le_iff₀ ha).2 (by linarith only [h])

private theorem sinh_exp (u : ℝ) :
    2 * sinh u * exp (-u) = 1 - exp (-(2 * u)) := by
  rw [sinh_eq, show -(2 * u) = -u + -u by ring, exp_add]
  have h : exp u * exp (-u) = 1 := by rw [← exp_add, add_neg_cancel, exp_zero]
  nlinarith only [h]

private theorem interior_bound {ξ z : ℝ} (hξ : |ξ| ≤ 1) (hz : 0 < z)
    (hA : 0 < 1 + ξ * z) : logKernel ξ z + 2 * log z < 0 := by
  by_cases hξ₀ : ξ = 0
  · subst ξ
    have h := (self_le_sinh_iff.mpr (half_pos hz).le)
    have he : z * exp (-(z / 2)) < 1 := by
      calc
        _ ≤ 2 * sinh (z / 2) * exp (-(z / 2)) := by
          apply mul_le_mul_of_nonneg_right _ (exp_pos _).le
          linarith only [h]
        _ = 1 - exp (-(2 * (z / 2))) := sinh_exp _
        _ < _ := sub_lt_self _ (exp_pos _)
    have hl := log_neg (mul_pos hz (exp_pos _)) he
    rw [log_mul hz.ne' (exp_ne_zero _), log_exp] at hl
    simp only [logKernel, ite_true]
    linarith only [hl, exp_pos (-z)]
  · let T := log (1 + ξ * z) / ξ
    have hT : 0 < T := by
      by_cases h : 0 < ξ
      · exact div_pos (log_pos (by linarith only [mul_pos h hz])) h
      · have hn := lt_of_le_of_ne (le_of_not_gt h) hξ₀
        exact div_pos_of_neg_of_neg (log_neg hA (by linarith only [mul_neg_of_neg_of_pos hn hz])) hn
    have hlog : ξ * T = log (1 + ξ * z) := by dsimp [T]; field_simp
    have hE : exp (ξ * T) = 1 + ξ * z := by rw [hlog, exp_log hA]
    have he : z * exp (-(ξ * T / 2)) = 2 * (sinh (ξ * (T / 2)) / ξ) := by
      rw [sinh_eq]
      have hp : exp (ξ * (T / 2)) = (1 + ξ * z) * exp (-(ξ * T / 2)) := by
        rw [← hE, ← exp_add]
        congr 1
        ring
      rw [hp, show -(ξ * (T / 2)) = -(ξ * T / 2) by ring]
      field_simp
      ring
    have hpos : z * exp (-((1 + ξ) * T / 2)) < 1 := by
      rw [show -((1 + ξ) * T / 2) = -(ξ * T / 2) + -(T / 2) by ring, exp_add,
        ← mul_assoc, he]
      calc
        _ ≤ 2 * sinh (T / 2) * exp (-(T / 2)) :=
          mul_le_mul_of_nonneg_right
            (mul_le_mul_of_nonneg_left (sinh_ratio_le hξ hξ₀ (half_pos hT).le) (by norm_num))
            (exp_pos _).le
        _ = 1 - exp (-(2 * (T / 2))) := sinh_exp _
        _ < _ := sub_lt_self _ (exp_pos _)
    have hl := log_neg (mul_pos hz (exp_pos _)) hpos
    rw [log_mul hz.ne' (exp_ne_zero _), log_exp] at hl
    have hK : logKernel ξ z = -(1 + ξ) * T - exp (-T) := by
      rw [logKernel, ite_eq_right hξ₀, rpow_def_of_pos hA, ← hlog]
      have heq : ξ * T * (-1 / ξ) = -T := by field_simp
      rw [heq]
      field_simp
      ring
    rw [hK]
    linarith only [hl, exp_pos (-T)]

private theorem residual_bound {ξ z : ℝ} (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hz : 0 < z)
    (hdom : if ξ = -1 then 0 ≤ 1 + ξ * z else 0 < 1 + ξ * z) :
    logKernel ξ z ≤ -2 * log z ∧
      (logKernel ξ z = -2 * log z ↔ ξ = -1 ∧ z = 1) := by
  have hA : 0 ≤ 1 + ξ * z := by
    by_cases he : ξ = -1
    · simpa only [ite_eq_left he] using hdom
    · exact (show 0 < 1 + ξ * z by simpa only [ite_eq_right he] using hdom).le
  rcases hA.eq_or_lt with he | hp
  · have hξ' : ξ = -1 := by
      by_contra h
      simp only [ite_eq_right h, ← he] at hdom
      exact (lt_irrefl 0) hdom
    have hz' : z = 1 := by rw [hξ'] at he; linarith
    subst ξ
    subst z
    norm_num [logKernel]
  · have h := interior_bound (abs_le.mpr hξ) hz hp
    have hlt : logKernel ξ z < -2 * log z := by linarith only [h]
    refine ⟨hlt.le, ⟨fun he ↦ (hlt.ne he).elim, ?_⟩⟩
    rintro ⟨rfl, rfl⟩
    norm_num at hp

variable {n : ℕ}

def halfValue (x : Fin n → ℝ) (m : ℝ) : ℝ :=
  ∑ i, if x i = m then (-1 : ℝ) else -2 * log (x i - m)

/-- The held-minimum half-tie bound and its exact equality condition.
The reverse-exponential endpoint is included in the fitted shape interval. -/
theorem held_minimum_half_bound (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card)
    {ξ s : ℝ} (hξ : ξ ∈ Icc (-1 : ℝ) 1) (hs : (m, s) ∈ GEV.domain x ξ) :
    GEV.logLik x ξ (m, s) ≤ halfValue x m ∧
      (GEV.logLik x ξ (m, s) = halfValue x m ↔
        ξ = -1 ∧ ∀ i, m < x i → x i - m = s) := by
  have hs0 : 0 < s := hs.1
  let B (i : Fin n) := if x i = m then -log s - 1 else log s - 2 * log (x i - m)
  let F (i : Fin n) := -log s + logKernel ξ ((x i - m) / s)
  have hF : GEV.logLik x ξ (m, s) = ∑ i, F i := by
    unfold GEV.logLik
    apply Finset.sum_congr rfl
    intro i _
    by_cases he : ξ = 0
    · simp [F, logKernel, he]
      ring
    · simp [F, logKernel, GEV.supportArg, he]
      ring
  have hB : (∑ i, B i) = halfValue x m := by
    have hab (i : Fin n) : B i =
        (if x i = m then (-1 : ℝ) else -2 * log (x i - m)) +
        (if x i = m then -log s else log s) := by
      by_cases h : x i = m <;> simp [B, h] <;> ring
    simp only [hab, Finset.sum_add_distrib, halfValue, add_eq_left]
    rw [Finset.sum_ite]
    simp only [Finset.sum_const, nsmul_eq_mul]
    have he : (Finset.univ.filter (fun i ↦ ¬ x i = m)).card =
        (Finset.univ.filter (fun i ↦ x i = m)).card := by
      have h := Finset.card_filter_add_card_filter_not (s := Finset.univ) (p := fun i ↦ x i = m)
      simp only [Finset.card_univ, Fintype.card_fin] at h
      omega
    rw [he]
    ring
  have hpos (i : Fin n) (hi : m < x i) :
      F i ≤ B i ∧ (F i = B i ↔ ξ = -1 ∧ x i - m = s) := by
    let z := (x i - m) / s
    have hz : 0 < z := div_pos (sub_pos.mpr hi) hs0
    have h := residual_bound hξ hz (hs.2 i)
    have hl : log z = log (x i - m) - log s :=
      log_div (sub_pos.mpr hi).ne' hs0.ne'
    have he : B i = -log s + (-2 * log z) := by
      simp only [B, ite_eq_right hi.ne']
      rw [hl]
      ring
    rw [he]
    change -log s + logKernel ξ z ≤ -log s + (-2 * log z) ∧
      (-log s + logKernel ξ z = -log s + (-2 * log z) ↔ _)
    simp only [add_le_add_iff_left, add_right_inj, h.2, z, div_eq_one_iff_eq hs0.ne']
    exact ⟨h.1, trivial⟩
  have heq (i : Fin n) (hi : x i = m) : F i = B i := by
    simp only [F, B, hi, ite_true, sub_self, zero_div, logKernel, neg_zero, exp_zero]
    split <;> simp [sub_eq_add_neg]
  have hle (i : Fin n) : F i ≤ B i := by
    rcases (hm i).eq_or_lt with he | hl
    · rw [heq i he.symm]
    · exact (hpos i hl).1
  rw [hF, ← hB]
  refine ⟨Finset.sum_le_sum (fun i _ ↦ hle i), ?_⟩
  constructor
  · intro h
    have he (i : Fin n) : F i = B i := by
      by_contra! he
      exact (Finset.sum_lt_sum (fun j _ ↦ hle j)
        (⟨i, Finset.mem_univ _, (hle i).lt_of_ne he⟩)).ne h
    have hex : ∃ i, m < x i := by
      by_contra! h'
      have hf : (Finset.univ.filter (fun i ↦ x i = m)) = Finset.univ :=
        Finset.filter_true_of_mem (fun i _ ↦ le_antisymm (h' i) (hm i))
      simp only [hf, Finset.card_univ, Fintype.card_fin] at hc
      omega
    obtain ⟨i, hi⟩ := hex
    exact ⟨((hpos i hi).2.mp (he i)).1, fun j hj ↦ ((hpos j hj).2.mp (he j)).2⟩
  · rintro ⟨hξ', hsample⟩
    apply Finset.sum_congr rfl
    intro i _
    rcases (hm i).eq_or_lt with he | hl
    · exact heq i he.symm
    · exact (hpos i hl).2.mpr ⟨hξ', hsample i hl⟩


/-- The half-tie trace is the supremum, and it is attained exactly when
the positive residuals share one value. -/
theorem held_minimum_half_classification (hn : 0 < n) {x : Fin n → ℝ} {m : ℝ}
    (hm : ∀ i, m ≤ x i)
    (hc : n = 2 * (Finset.univ.filter (fun i ↦ x i = m)).card) :
    let D : Set (ℝ × ℝ) := {q | q.1 ∈ Icc (-1 : ℝ) 1 ∧ (m, q.2) ∈ GEV.domain x q.1}
    let L := fun q : ℝ × ℝ ↦ GEV.logLik x q.1 (m, q.2)
    IsLUB (L '' D) (halfValue x m) ∧
      ((∃ q ∈ D, IsMaxOn L D q) ↔ ∃ s > 0, ∀ i, m < x i → x i - m = s) := by
  intro D L
  have hpath := HeldSweepGEV.held_minimum_half_path x hm hc
  have hfeasible (s : ℝ) (hs : 0 < s) : (1, s) ∈ D := by
    refine ⟨by norm_num, hs, fun i ↦ ?_⟩
    simp only [GEV.supportArg, one_mul, ite_eq_right (show (1 : ℝ) ≠ -1 by norm_num)]
    exact add_pos_of_pos_of_nonneg one_pos (div_nonneg (sub_nonneg.mpr (hm i)) hs.le)
  have hB : IsLUB (L '' D) (halfValue x m) := by
    constructor
    · rintro _ ⟨q, hq, rfl⟩
      exact (held_minimum_half_bound hn hm hc hq.1 hq.2).1
    · intro u hu
      apply le_of_tendsto hpath
      filter_upwards [self_mem_nhdsWithin] with s (hs : 0 < s)
      exact hu ⟨(1, s), hfeasible s hs, rfl⟩
  refine ⟨hB, ?_⟩
  constructor
  · rintro ⟨q, hq, hmax⟩
    have hub : L q ∈ upperBounds (L '' D) := by
      rintro _ ⟨p, hp, rfl⟩
      exact hmax hp
    have hEq := le_antisymm (hB.1 ⟨q, hq, rfl⟩) (hB.2 hub)
    exact ⟨q.2, hq.2.1, ((held_minimum_half_bound hn hm hc hq.1 hq.2).2.mp hEq).2⟩
  · rintro ⟨s, hs, he⟩
    have hξ : (-1 : ℝ) ∈ Icc (-1) 1 := by norm_num
    have hd : (m, s) ∈ GEV.domain x (-1) := by
      refine ⟨hs, fun i ↦ ?_⟩
      simp only [GEV.supportArg, ite_true]
      rcases (hm i).eq_or_lt with h | h
      · simp [h]
      · simp [he i h, hs.ne']
    have hEq := (held_minimum_half_bound hn hm hc hξ hd).2.mpr ⟨rfl, he⟩
    exact ⟨(-1, s), ⟨hξ, hd⟩, fun q hq ↦ (hB.1 ⟨q, hq, rfl⟩).trans_eq hEq.symm⟩

end

end UnivariateML.Shape2GEV

end
