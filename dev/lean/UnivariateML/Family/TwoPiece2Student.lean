module

public import UnivariateML.Family.TwoPieceStudent
import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-! # Fixed-mean collapse and stationary candidates for skew Student

Inverse total denominator width is the free scale coordinate. At fixed
shape and skew the reported standard deviation is proportional to total
denominator width, so the positive inverse widths exhaust the scale domain.

## Deviations

The singleton theorem covers outward residuals and unit skew. The generic
cardinality claim on the full stratum remains open.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TwoPiece2Student

open TwoPieceStudent

variable {n : ℕ}

/-- With mean, shape and skew held, the width ends and the multiplicity
exponent determine the escape envelope. The surviving score roots give
the complete set of maximizing scales after exact likelihood comparison. -/
theorem heldMeanShapeSkew (hn : 0 < n) (x : Fin n → ℝ) (μ ν w : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1) :
    let e := halfMoment ν * (1 - 2*w)
    let f := logAlongAtom x μ ν w e
    let E := TDist.exponent x μ ν
    let C := collapseLogHeight x μ ν w e
    let R := {u : ℝ | 0 < u ∧ scaleScore x μ ν w e u = 0}
    Tendsto f (𝓝[>] 0) atBot ∧
    (¬ BddAbove (f '' Ioi 0) ↔ E < 0) ∧
    (E < 0 → Tendsto f atTop atTop) ∧
    (0 < E → Tendsto f atTop atBot) ∧
    (E = 0 → Tendsto f atTop (𝓝 C) ∧
      Tendsto (fun u ↦ exp (f u)) atTop (𝓝 (exp C))) ∧
    ((∃ u ∈ Ioi (0 : ℝ), IsMaxOn f (Ioi 0) u) ↔
      ∃ u ∈ R, 0 < E ∨ E = 0 ∧ C ≤ f u) ∧
    R.Finite ∧
    {u ∈ Ioi (0 : ℝ) | IsMaxOn f (Ioi 0) u} =
      {u ∈ R | (0 < E ∨ E = 0 ∧ C ≤ f u) ∧ ∀ v ∈ R, f v ≤ f u} := by
  intro e f E C R
  obtain ⟨hn1, hp1, hz⟩ := collapse_cases x μ ν w e hw
  refine ⟨infiniteWidth_loses hn x μ ν w e hν,
    fixedOffset_unbounded_iff hn x μ ν w e hν hw, hn1, hp1, hz,
    ?_, scaleScore_roots_finite hn x μ ν w e hw, ?_⟩
  · rw [fixedOffset_criterion hn x μ ν w e hν hw]
    exact ⟨fun ⟨u, hu, hscore, h⟩ ↦ ⟨u, ⟨hu, hscore⟩, h⟩,
      fun ⟨u, ⟨hu, hscore⟩, h⟩ ↦ ⟨u, hu, hscore, h⟩⟩
  · ext u
    change (0 < u ∧ IsMaxOn f (Ioi 0) u) ↔
      ((0 < u ∧ scaleScore x μ ν w e u = 0) ∧
        (0 < E ∨ E = 0 ∧ C ≤ f u) ∧
        ∀ v, (0 < v ∧ scaleScore x μ ν w e v = 0) → f v ≤ f u)
    by_cases hu : 0 < u
    · rw [fixedOffset_maximizers hn x μ ν w e hν hw hu]
      exact ⟨fun ⟨_, hscore, h, hmax⟩ ↦ ⟨⟨hu, hscore⟩, h,
        fun v ⟨hv, hscore'⟩ ↦ hmax v hv hscore'⟩,
        fun ⟨⟨_, hscore⟩, h, hmax⟩ ↦ ⟨hu, hscore, h,
          fun v hv hscore' ↦ hmax v ⟨hv, hscore'⟩⟩⟩
    · simp [hu]

private noncomputable def scoreTerm (A b t : ℝ) : ℝ := t * (t+b) / (A+(t+b)^2)

private theorem scoreTerm_strictMono {A b : ℝ} (hA : 0 < A) (hb : 0 ≤ b) :
    StrictMonoOn (scoreTerm A b) (Ici 0) := by
  intro u hu v _ huv
  have hu' : 0 ≤ u := hu
  have hv' : 0 ≤ v := hu'.trans huv.le
  unfold scoreTerm
  apply (div_lt_div_iff₀ (by positivity) (by positivity)).mpr
  have he : v*(v+b)*(A+(u+b)^2) - u*(u+b)*(A+(v+b)^2) =
      (v-u)*(A*(u+v+b) + b*(u+b)*(v+b)) := by ring
  have h : 0 < (v-u)*(A*(u+v+b) + b*(u+b)*(v+b)) := by
    have : 0 < v := hu'.trans_lt huv
    exact mul_pos (sub_pos.mpr huv)
      (add_pos_of_pos_of_nonneg (mul_pos hA (by positivity)) (by positivity))
  linarith only [he, h]

private theorem scoreTerm_lt_one {A b t : ℝ} (hA : 0 < A) (hb : 0 ≤ b) (ht : 0 ≤ t) :
    scoreTerm A b t < 1 := by
  apply (div_lt_one (by positivity)).mpr
  nlinarith only [hA, mul_nonneg hb ht, sq_nonneg b]

private theorem outward_score (x : Fin n → ℝ) (μ ν w e : ℝ)
    (hout : ∀ i, (x i - μ) * e ≥ 0) {u : ℝ} (hu : 0 < u) :
    scaleScore x μ ν w e u * u =
      n - (ν+1) * ∑ i, scoreTerm ((side w (x i-μ))^2) |e| (|x i-μ| *u) := by
  unfold scaleScore
  rw [sub_mul, div_mul_cancel₀ _ hu.ne', mul_assoc, Finset.sum_mul]
  congr 1
  simp only [Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro i _
  let d := x i - μ
  change (ν+1)/2 * (2*(d*u+e)/((side w (d*u+e))^2+(d*u+e)^2)*d*u) =
    (ν+1)*scoreTerm ((side w d)^2) |e| (|d| *u)
  rcases lt_trichotomy d 0 with hd | hd | hd
  · have he : e ≤ 0 := nonpos_of_mul_nonneg_right (hout i) hd
    have hr : d*u+e < 0 := add_neg_of_neg_of_nonpos (mul_neg_of_neg_of_pos hd hu) he
    simp only [side, ite_eq_left hr, ite_eq_left hd, abs_of_nonpos he, abs_of_neg hd, scoreTerm]
    rw [show -d * u + -e = -(d*u+e) by ring, neg_sq]
    ring
  · simp [hd, scoreTerm]
  · have he : 0 ≤ e := nonneg_of_mul_nonneg_right (hout i) hd
    have hr : 0 ≤ d*u+e := add_nonneg (mul_nonneg hd.le hu.le) he
    simp only [side, ite_eq_right hr.not_gt, ite_eq_right hd.not_gt, abs_of_nonneg he,
      abs_of_pos hd, scoreTerm]
    ring

/-- With mean, shape and skew held, outward nonzero residuals give a
singleton precisely when the collapse exponent is positive. This also
covers every sample at unit skew. At the critical count the collapse
height is the unattained supremum. -/
theorem heldMeanShapeSkew_outward (hn : 0 < n) (x : Fin n → ℝ) (μ ν w : ℝ)
    (hν : 2 < ν) (hw : w ∈ Ioo (0 : ℝ) 1)
    (hout : ∀ i, (x i - μ) * (halfMoment ν * (1 - 2 * w)) ≥ 0) :
    let f := logAlongAtom x μ ν w (halfMoment ν * (1 - 2*w))
    let E := TDist.exponent x μ ν
    let C := collapseLogHeight x μ ν w (halfMoment ν * (1 - 2*w))
    ((∃! u, 0 < u ∧ IsMaxOn f (Ioi 0) u) ↔ 0 < E) ∧
    (E ≤ 0 → StrictMonoOn f (Ioi 0)) ∧
    (E = 0 → IsLUB (f '' Ioi 0) C ∧ ∀ u > 0, f u < C) := by
  classical
  intro f E C
  let e := halfMoment ν * (1 - 2*w)
  let off := Finset.univ.filter (fun i : Fin n ↦ x i ≠ μ)
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hp : 0 < ν+1 := by linarith
  have hcount : E = (ν+1)*(off.card : ℝ) - n := by
    have hc : ((TDist.multiplicity x μ : ℕ) : ℝ) + off.card = n := by
      exact_mod_cast (Finset.card_filter_add_card_filter_not (s := Finset.univ)
        (fun i ↦ x i = μ)).trans (by simp)
    unfold E TDist.exponent
    nlinarith only [hc, congrArg (fun z : ℝ ↦ ν*z) hc]
  let A (i : Fin n) := (side w (x i-μ))^2
  let F (u : ℝ) (i : Fin n) := scoreTerm (A i) |e| (|x i-μ| *u)
  have hA (i : Fin n) : 0 < A i := by
    unfold A side
    split
    · exact pow_pos hw.1 _
    · exact pow_pos (sub_pos.mpr hw.2) _
  have hzero {i : Fin n} (hi : x i = μ) (u : ℝ) : F u i = 0 := by
    simp [F, hi, scoreTerm]
  have hsum (u : ℝ) : ∑ i ∈ off, F u i = ∑ i, F u i :=
    Finset.sum_filter_of_ne (fun i _ h ↦ fun hi ↦ h (hzero hi u))
  have hscore {u : ℝ} (hu : 0 < u) :
      scaleScore x μ ν w e u * u = n - (ν+1)*∑ i ∈ off, F u i := by
    rw [hsum]
    exact outward_score x μ ν w e hout hu
  have hderiv {u : ℝ} (hu : 0 < u) :
      HasDerivAt f (scaleScore x μ ν w e u) u :=
    hasDerivAt_scale x μ ν w e hw hu
  have hpos (hE : E ≤ 0) {u : ℝ} (hu : 0 < u) : 0 < scaleScore x μ ν w e u := by
    apply pos_of_mul_pos_left _ hu.le
    rw [hscore hu]
    by_cases hne : off.Nonempty
    · have hlt : ∑ i ∈ off, F u i < (off.card : ℝ) := by
        simpa only [Finset.sum_const, nsmul_eq_mul, mul_one] using
          (Finset.sum_lt_sum_of_nonempty hne
            (fun i _ ↦ scoreTerm_lt_one (hA i) (abs_nonneg e)
              (mul_nonneg (abs_nonneg _) hu.le) : ∀ i ∈ off, F u i < 1))
      linarith only [hE, hcount, mul_lt_mul_of_pos_left hlt hp]
    · rw [Finset.not_nonempty_iff_eq_empty.mp hne, Finset.sum_empty, mul_zero, sub_zero]
      exact hN
  have hmono (hE : E ≤ 0) : StrictMonoOn f (Ioi 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    · intro u hu
      exact (hderiv hu).continuousAt.continuousWithinAt
    · intro u hu
      rw [interior_Ioi] at hu
      rw [(hderiv hu).deriv]
      exact hpos hE hu
  refine ⟨⟨?_, ?_⟩, hmono, ?_⟩
  · rintro ⟨u, ⟨hu, hm⟩, _⟩
    by_contra! h
    have ht : u < u+1 := by linarith
    exact ((hmono h) hu (hu.trans ht) ht).not_ge (hm (show u+1 ∈ Ioi 0 from hu.trans ht))
  · intro hE
    let g (t : ℝ) := f (exp t)
    have hcont : Continuous g :=
      continuous_iff_continuousAt.mpr fun t ↦ (hderiv (exp_pos t)).continuousAt.comp
        continuous_exp.continuousAt
    have ht : Tendsto g (cocompact ℝ) atBot := by
      rw [cocompact_eq_atBot_atTop, tendsto_sup]
      exact ⟨(infiniteWidth_loses hn x μ ν w e hν).comp tendsto_exp_atBot_nhdsGT,
        ((collapse_cases x μ ν w e hw).2.1 hE).comp tendsto_exp_atTop⟩
    obtain ⟨t, hmax⟩ := hcont.exists_forall_ge ht
    have hmax' : IsMaxOn f (Ioi 0) (exp t) := by
      intro u hu
      have hh := hmax (log u)
      change f (exp (log u)) ≤ f (exp t) at hh
      rwa [exp_log hu] at hh
    have hne : off.Nonempty := by
      by_contra he
      rw [Finset.not_nonempty_iff_eq_empty.mp he, Finset.card_empty, Nat.cast_zero, mul_zero,
        zero_sub] at hcount
      linarith
    have hanti : StrictAntiOn (fun u ↦ scaleScore x μ ν w e u * u) (Ioi 0) := by
      intro u hu v hv huv
      have hu' : 0 < u := hu
      have hv' : 0 < v := hv
      change scaleScore x μ ν w e v * v < scaleScore x μ ν w e u * u
      rw [hscore hu', hscore hv']
      apply sub_lt_sub_left
      apply mul_lt_mul_of_pos_left _ hp
      apply Finset.sum_lt_sum_of_nonempty hne
      intro i hi
      have hd : 0 < |x i-μ| := abs_pos.mpr
        (sub_ne_zero.mpr (Finset.mem_filter.mp hi).2)
      exact scoreTerm_strictMono (hA i) (abs_nonneg e)
        (mul_nonneg hd.le hu'.le) (mul_nonneg hd.le hv'.le)
        (mul_lt_mul_of_pos_left huv hd)
    have hz {u : ℝ} (hu : 0 < u) (hm : IsMaxOn f (Ioi 0) u) :
        scaleScore x μ ν w e u * u = 0 := by
      have he := (hm.isLocalMax (Ioi_mem_nhds hu)).deriv_eq_zero
      rw [(hderiv hu).deriv] at he
      rw [he, zero_mul]
    refine ⟨exp t, ⟨exp_pos t, hmax'⟩, ?_⟩
    rintro u ⟨hu, hm⟩
    exact hanti.injOn hu (exp_pos t) ((hz hu hm).trans (hz (exp_pos t) hmax').symm)
  · intro hE
    have hlim : Tendsto f atTop (𝓝 C) := ((collapse_cases x μ ν w e hw).2.2 hE).1
    have hub (u : ℝ) (hu : 0 < u) : f u ≤ C := by
      apply ge_of_tendsto hlim
      filter_upwards [eventually_ge_atTop u] with v hv
      exact (hmono hE.le).monotoneOn hu (hu.trans_le hv) hv
    refine ⟨⟨?_, ?_⟩, ?_⟩
    · rintro _ ⟨u, hu, rfl⟩
      exact hub u hu
    · intro L hL
      exact le_of_tendsto hlim ((eventually_gt_atTop (0 : ℝ)).mono
        (fun u hu ↦ hL (mem_image_of_mem f hu)))
    · intro u hu
      have h : u < u+1 := by linarith
      exact ((hmono hE.le) hu (hu.trans h) h).trans_le (hub _ (hu.trans h))

end UnivariateML.TwoPiece2Student

end
