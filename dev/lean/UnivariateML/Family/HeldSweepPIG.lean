module

public import UnivariateML.Family.Discrete3PIG
public import UnivariateML.Generic.Score

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Poisson–inverse Gaussian held fits

## Deviations

The zero-data results use the all-zero likelihood directly, including at shape
zero. The positive-data theorem solves the binary case, where the Bessel
polynomial factor is constant.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.HeldSweepPIG

noncomputable section

/-- At positive mean and nonnegative shape, the all-zero sample likelihood
is the zero-cell probability raised to the sample size. -/
def zeroLikelihood (n : ℕ) (μ lam : ℝ) : ℝ :=
  exp (-(n : ℝ) * (sqrt (2 * lam + (lam / μ)^2) - lam / μ))

private theorem gap_pos {μ lam : ℝ} (hμ : 0 < μ) (hlam : 0 < lam) :
    0 < sqrt (2 * lam + (lam / μ)^2) - lam / μ := by
  apply sub_pos.mpr
  nth_rw 1 [← sqrt_sq (div_pos hlam hμ).le]
  exact sqrt_lt_sqrt (sq_nonneg _) (by linarith)

private theorem gap_rationalize {μ lam : ℝ} (hμ : 0 < μ) (hlam : 0 < lam) :
    sqrt (2 * lam + (lam / μ)^2) - lam / μ =
      2 * lam / (sqrt (2 * lam + (lam / μ)^2) + lam / μ) := by
  apply (eq_div_iff (add_pos_of_nonneg_of_pos (sqrt_nonneg _) (div_pos hlam hμ)).ne').mpr
  have hs := sq_sqrt (show 0 ≤ 2 * lam + (lam / μ)^2 by positivity)
  nlinarith only [hs]

/-- The entire shape-zero ray, with arbitrary positive mean, is the set of
all-zero maximizers in the full feasible domain. -/
theorem all_zero_argmax {n : ℕ} (hn : 0 < n) {p : ℝ × ℝ}
    (hp : p ∈ Ioi (0 : ℝ) ×ˢ Ici 0) :
    IsMaxOn (fun q : ℝ × ℝ ↦ zeroLikelihood n q.1 q.2)
      (Ioi (0 : ℝ) ×ˢ Ici 0) p ↔ p.2 = 0 := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hstrict {μ lam : ℝ} (hμ : 0 < μ) (hlam : 0 < lam) :
      zeroLikelihood n μ lam < 1 :=
    exp_lt_one_iff.mpr (mul_neg_of_neg_of_pos (neg_neg_of_pos hn') (gap_pos hμ hlam))
  have hz (μ : ℝ) : zeroLikelihood n μ 0 = 1 := by simp [zeroLikelihood]
  constructor
  · intro h
    rcases eq_or_lt_of_le hp.2 with he | hlam
    · exact he.symm
    · have hh : zeroLikelihood n 1 0 ≤ zeroLikelihood n p.1 p.2 :=
        h (show ((1, 0) : ℝ × ℝ) ∈ Ioi 0 ×ˢ Ici 0 from ⟨one_pos, le_rfl⟩)
      rw [hz] at hh
      exact ((hstrict hp.1 hlam).not_ge hh).elim
  · intro he q hq
    change zeroLikelihood n q.1 q.2 ≤ zeroLikelihood n p.1 p.2
    rw [he, hz]
    rcases eq_or_lt_of_le hq.2 with he' | hlam
    · rw [← he', hz]
    · exact (hstrict hq.1 hlam).le

/-- For held positive shape on all-zero data, the excluded mean-zero limit
is the strictly unattained supremum. -/
theorem held_shape_all_zero {n : ℕ} (hn : 0 < n) {lam : ℝ} (hlam : 0 < lam) :
    Tendsto (fun μ ↦ zeroLikelihood n μ lam) (𝓝[>] (0 : ℝ)) (𝓝 1) ∧
      IsLUB ((fun μ ↦ zeroLikelihood n μ lam) '' Ioi (0 : ℝ)) 1 ∧
      ∀ μ > 0, zeroLikelihood n μ lam < 1 := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hs (μ : ℝ) (hμ : 0 < μ) : zeroLikelihood n μ lam < 1 :=
    exp_lt_one_iff.mpr (mul_neg_of_neg_of_pos (neg_neg_of_pos hn') (gap_pos hμ hlam))
  have hg : Tendsto (fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2) - lam / μ)
      (𝓝[>] (0 : ℝ)) (𝓝 0) := by
    refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
      (show Tendsto (fun μ : ℝ ↦ 2 * μ) (𝓝[>] 0) (𝓝 0) by
        have h : Tendsto id (𝓝[>] (0 : ℝ)) (𝓝 0) :=
          tendsto_nhdsWithin_of_tendsto_nhds tendsto_id
        simpa only [mul_zero, id_eq] using h.const_mul 2)
      ?_ ?_
    · filter_upwards [eventually_mem_nhdsWithin] with μ (hμ : 0 < μ)
      exact (gap_pos hμ hlam).le
    · filter_upwards [eventually_mem_nhdsWithin] with μ (hμ : 0 < μ)
      rw [gap_rationalize hμ hlam]
      have hd := add_pos_of_nonneg_of_pos (sqrt_nonneg (2 * lam + (lam / μ)^2)) (div_pos hlam hμ)
      apply (div_le_iff₀ hd).mpr
      have ht : 2 * μ * (lam / μ) = 2 * lam := by field_simp
      rw [mul_add, ht]
      exact le_add_of_nonneg_left (mul_nonneg (by positivity) (sqrt_nonneg _))
  have ht : Tendsto (fun μ ↦ zeroLikelihood n μ lam) (𝓝[>] (0 : ℝ)) (𝓝 1) := by
    simpa only [zeroLikelihood, mul_zero, exp_zero, Function.comp_def] using
      continuous_exp.continuousAt.tendsto.comp (hg.const_mul (-(n : ℝ)))
  refine ⟨ht, ⟨?_, ?_⟩, hs⟩
  · rintro _ ⟨μ, hμ, rfl⟩
    exact (hs μ hμ).le
  · intro b hb
    apply le_of_tendsto ht
    filter_upwards [eventually_mem_nhdsWithin] with μ (hμ : 0 < μ)
    exact hb ⟨μ, hμ, rfl⟩

/-- For each held positive shape the Bessel gap is a bijective
coordinate for the positive mean. -/
theorem held_shape_gap_bijOn {lam : ℝ} (hl : 0 < lam) :
    BijOn (fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2) - lam / μ)
      (Ioi 0) (Ioo 0 (sqrt (2 * lam))) := by
  let d := fun μ : ℝ ↦ sqrt (2 * lam + (lam / μ)^2) - lam / μ
  have hsq (μ : ℝ) : d μ ^ 2 + 2 * d μ * (lam / μ) = 2 * lam := by
    have h := sq_sqrt (show 0 ≤ 2 * lam + (lam / μ)^2 by positivity)
    unfold d
    nlinarith only [h]
  have hmem (μ : ℝ) (hμ : 0 < μ) : d μ ∈ Ioo 0 (sqrt (2 * lam)) := by
    have hd : 0 < d μ := gap_pos hμ hl
    have h := mul_pos (mul_pos (by norm_num : (0 : ℝ) < 2) hd) (div_pos hl hμ)
    refine ⟨hd, ?_⟩
    rw [← sqrt_sq hd.le]
    exact sqrt_lt_sqrt (sq_nonneg _) (by linarith only [hsq μ, h])
  refine ⟨hmem, ?_, ?_⟩
  · intro μ hμ ν hν he
    have hd := (hmem μ hμ).1
    have hh := hsq ν
    change d μ = d ν at he
    rw [← he] at hh
    have hinj : lam / μ = lam / ν := by
      apply (mul_right_inj' (show 2 * d μ ≠ 0 by positivity)).mp
      linarith only [hsq μ, hh]
    have heq := (div_eq_div_iff (show 0 < μ from hμ).ne' (show 0 < ν from hν).ne').mp hinj
    exact (mul_right_inj' hl.ne').mp heq.symm
  · intro u hu
    have hu' : 0 < u := hu.1
    have hu2 : u^2 < 2 * lam := by
      rw [← sq_sqrt (show 0 ≤ 2 * lam by positivity)]
      exact pow_lt_pow_left₀ hu.2 hu'.le (by decide)
    let t := (2 * lam - u^2) / (2 * u)
    have ht : 0 < t := div_pos (sub_pos.mpr hu2) (by positivity)
    let μ := lam / t
    have hμ : 0 < μ := div_pos hl ht
    refine ⟨μ, hμ, ?_⟩
    change d μ = u
    have hte : lam / μ = t := by
      dsimp [μ]
      field_simp
    have he : 2 * lam + t^2 = (u + t)^2 := by
      have hh : t * (2 * u) = 2 * lam - u^2 := by
        dsimp [t]
        field_simp
      nlinarith only [hh]
    dsimp [d]
    rw [hte, he, sqrt_sq (add_nonneg hu'.le ht.le), add_sub_cancel_right]

/-- Held-shape binary log likelihood in the gap coordinate (d=z-t),
up to a constant. Here (S) is the number of ones. -/
def binaryHeldShapeLog (n S lam d : ℝ) : ℝ :=
  -n * d + S * (log d - log (lam + d^2 / 2))

private def binaryShapeScore (n S lam d : ℝ) : ℝ :=
  -n + S * (1 / d - d / (lam + d^2 / 2))

private theorem binaryShape_deriv {n S lam d : ℝ} (hl : 0 < lam) (hd : 0 < d) :
    HasDerivAt (binaryHeldShapeLog n S lam) (binaryShapeScore n S lam d) d := by
  have hq : 0 < lam + d^2 / 2 := by positivity
  have hh : HasDerivAt (fun u : ℝ ↦ lam + u^2 / 2) d d := by
    convert ((((hasDerivAt_id d).pow 2).div_const 2).const_add lam) using 1 <;>
      simp [id_eq]
  convert (((hasDerivAt_id d).log hd.ne').sub (hh.log hq.ne')).const_mul S |>.add
    ((hasDerivAt_id d).const_mul (-n)) using 1
  · ext u
    simp only [binaryHeldShapeLog, Pi.add_apply, Pi.sub_apply, id_eq]
    ring
  · simp [binaryShapeScore]
    ring

/-- At positive held shape, a nonempty binary sample with a positive count
has a unique fit, with gap strictly between zero and the infinite-mean end. -/
theorem held_shape_binary_unique {n S lam : ℝ} (hn : 0 < n) (hS : 0 < S) (hl : 0 < lam) :
    ∃! d, d ∈ Ioo (0 : ℝ) (sqrt (2 * lam)) ∧
      IsMaxOn (binaryHeldShapeLog n S lam) (Ioo 0 (sqrt (2 * lam))) d := by
  let b := sqrt (2 * lam)
  let g := binaryShapeScore n S lam
  have hb : 0 < b := sqrt_pos.mpr (by positivity)
  have hb2 : b^2 = 2 * lam := sq_sqrt (by positivity)
  have hcont : ContinuousOn g (Ioc 0 b) := by
    unfold g binaryShapeScore
    apply continuousOn_const.add
    apply ContinuousOn.const_mul
    apply ContinuousOn.sub
    · exact continuousOn_const.div continuousOn_id (fun d hd ↦ (show 0 < d from hd.1).ne')
    · exact continuousOn_id.div (by fun_prop) (fun d _ ↦ by positivity)
  have hanti : StrictAntiOn g (Ioo 0 b) := by
    intro d hd e he hde
    have hd₀ : 0 < d := hd.1
    have he₀ : 0 < e := he.1
    have hde' : d * e ≤ 2 * lam := by
      have h := mul_le_mul hd.2.le he.2.le he₀.le hb.le
      nlinarith only [h, hb2]
    have h₁ : 1 / e < 1 / d :=
      div_lt_div_of_pos_left one_pos hd₀ hde
    have h₂ : d / (lam + d^2 / 2) ≤ e / (lam + e^2 / 2) := by
      apply (div_le_div_iff₀ (by positivity) (by positivity)).mpr
      nlinarith only [mul_nonneg (sub_nonneg.mpr hde.le)
        (show 0 ≤ lam - d * e / 2 by linarith)]
    unfold g binaryShapeScore
    have hdiff : 1 / e - e / (lam + e^2 / 2) < 1 / d - d / (lam + d^2 / 2) := by
      linarith only [h₁, h₂]
    linarith only [mul_lt_mul_of_pos_left hdiff hS]
  have hgb : g b = -n := by
    unfold g binaryShapeScore
    rw [hb2]
    have h : b / (lam + 2 * lam / 2) = 1 / b := by
      apply (div_eq_div_iff (by positivity) hb.ne').mpr
      nlinarith only [hb2]
    rw [h, sub_self, mul_zero, add_zero]
  have hneg : ∃ e ∈ Ioo (0 : ℝ) b, g e < 0 := by
    have hh := hcont b (show b ∈ Ioc 0 b from ⟨hb, le_rfl⟩)
    have ht : ∀ᶠ e in 𝓝[Ioc 0 b] b, g e < 0 :=
      hh.eventually (Iio_mem_nhds (by rw [hgb]; exact neg_neg_of_pos hn))
    have hi : Ioo (0 : ℝ) b ⊆ Ioc 0 b := fun _ h ↦ ⟨h.1, h.2.le⟩
    have hw := ht.filter_mono (nhdsWithin_mono _ hi)
    let := right_nhdsWithin_Ioo_neBot hb
    exact (eventually_mem_nhdsWithin.and hw).exists
  obtain ⟨e, he, hge⟩ := hneg
  let d := min (e / 2) (S / (2 * (n + S * (b / lam))))
  have hsum : 0 < n + S * (b / lam) := by positivity
  have hd : 0 < d := lt_min (half_pos he.1) (div_pos hS (by positivity))
  have hde : d < e := (min_le_left _ _).trans_lt (half_lt_self he.1)
  have hgd : 0 < g d := by
    have hsmall := min_le_right (e / 2) (S / (2 * (n + S * (b / lam))))
    have hh := (le_div_iff₀ (show 0 < 2 * (n + S * (b / lam)) by positivity)).mp hsmall
    have hhigh : n + S * (b / lam) < S / d := by
      apply (lt_div_iff₀ hd).mpr
      linarith only [hh, mul_pos hd hsum]
    have hlow : d / (lam + d^2 / 2) ≤ b / lam := by
      apply (div_le_div_iff₀ (by positivity) hl).mpr
      nlinarith only [mul_le_mul_of_nonneg_right (hde.trans he.2).le hl.le,
        mul_nonneg hb.le (sq_nonneg d)]
    unfold g binaryShapeScore
    rw [mul_sub, ← mul_div_assoc, mul_one]
    linarith only [hhigh, mul_le_mul_of_nonneg_left hlow hS.le]
  obtain ⟨⟨v, hv, hu⟩, hchar⟩ := Generic.existsUnique_score_zero_of_signs
    (convex_Ioo 0 b) (fun t ht ↦ binaryShape_deriv hl ht.1)
    (hcont.mono fun _ h ↦ ⟨h.1, h.2.le⟩) hanti
    (show d ∈ Ioo 0 b from ⟨hd, hde.trans he.2⟩) he hde.le hgd hge
  exact ⟨v, ⟨hv.1, (hchar v hv.1).mpr hv.2⟩, fun t ht ↦
    hu t ⟨ht.1, (hchar t ht.1).mp ht.2⟩⟩

end

end UnivariateML.HeldSweepPIG

end
