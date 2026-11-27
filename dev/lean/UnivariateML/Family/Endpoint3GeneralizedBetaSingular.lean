module

public import UnivariateML.Family.Endpoint3GeneralizedBeta

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized beta with first shape and power held

## Deviations

The exhaustion result concerns likelihood divergence on strictly containing
endpoints. The simultaneous contact-rate expansion and weak-law classification
remain separate.
-/

@[expose] public section

open Real Set Filter
open scoped BigOperators Topology

namespace UnivariateML.Endpoint3GeneralizedBetaSingular

open Endpoint2GeneralizedBeta

private theorem logBeta_formula {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    -Beta.logBeta (p, q) =
      log (Gamma (p + q)) - log (Gamma q) - log (Gamma p) := by
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
      (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']
  ring

private theorem bounded_shape_upper {p : ℝ} (hp : 0 < p) :
    ∃ C : ℝ, ∀ q, 0 < q → q ≤ 2 → -Beta.logBeta (p, q) ≤ C := by
  let F := fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma q) - log (Gamma p)
  have ht : Tendsto F (𝓝[>] 0) atBot := by
    apply (tendsto_log_nhdsGT_zero.atBot_add
      (Endpoint3GammaRatio.log_gamma_ratio_at_zero hp)).congr
    intro q
    dsimp [F]
    ring
  obtain ⟨ε, hε, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    (ht.eventually_lt_atBot (0 : ℝ))
  let K := Icc (min ε 2) 2
  have hc : ContinuousOn F K := by
    intro q hq
    have hq₀ : 0 < q := (lt_min hε (by norm_num)).trans_le hq.1
    have hG {z : ℝ} (hz : 0 < z) : ContinuousAt (fun y ↦ log (Gamma y)) z :=
      ((Real.differentiableAt_Gamma (fun m ↦ by
        have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
        linarith)).continuousAt).log (Gamma_pos_of_pos hz).ne'
    have hh : ContinuousAt (fun y : ℝ ↦ p + y) q :=
      continuousAt_const.add continuousAt_id
    have hG₁ := (hG (add_pos hp hq₀)).comp' (f := fun y : ℝ ↦ p + y) (x := q) hh
    exact ((hG₁.sub (hG hq₀)).sub continuousAt_const).continuousWithinAt
  obtain ⟨q, _, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨2, min_le_right _ _, le_rfl⟩ hc
  refine ⟨max 0 (F q), fun u hu hu₂ ↦ ?_⟩
  rw [logBeta_formula hp hu]
  by_cases he : u < min ε 2
  · exact (hsmall ⟨hu, he.trans_le (min_le_left _ _)⟩).le.trans (le_max_left _ _)
  · exact (hm ⟨le_of_not_gt he, hu₂⟩).trans (le_max_right _ _)

private theorem large_shape_upper {p q : ℝ} (hp : 0 < p) (hq : 1 ≤ q) :
    -Beta.logBeta (p, q) ≤
      p * log q + (p * log (1 + p) - log (Gamma p)) := by
  have hq₀ : 0 < q := zero_lt_one.trans_le hq
  have hqp := add_pos hq₀ hp
  have h := convexOn_log_Gamma.slope_mono_adjacent (x := q) (y := q + p) (z := q + p + 1)
    hq₀ (by linarith : 0 < q + p + 1) (by linarith) (by linarith)
  simp only [Function.comp_apply, add_sub_cancel_left, div_one] at h
  rw [Gamma_add_one hqp.ne', log_mul hqp.ne' (Gamma_pos_of_pos hqp).ne'] at h
  have hu : log (Gamma (q+p)) - log (Gamma q) ≤ p * log (q+p) := by
    have hh := (div_le_iff₀ hp).mp h
    linarith
  have hl := log_le_log hqp (show q + p ≤ q * (1+p) by
    nlinarith [mul_le_mul_of_nonneg_right hq hp.le])
  rw [log_mul hq₀.ne' (by positivity : 1+p ≠ 0)] at hl
  rw [logBeta_formula hp hq₀, add_comm p q]
  linarith [mul_le_mul_of_nonneg_left hl hp.le]

/-- At fixed positive first shape and power, any divergent interior
likelihood sequence eventually has subunit upper shape and approaches
the sample maximum. The bounds are uniform over upper shape and apply
also to constants and samples with ties at the maximum. -/
theorem divergent_sequences_require_subunit_contact {ι : Type*} {l : Filter ι}
    {n : ℕ} {x : Fin n → ℝ} {M p c : ℝ} (hx : ∀ i, 0 < x i)
    (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) {b q : ι → ℝ}
    (hb : ∀ᶠ j in l, M < b j) (hq : ∀ᶠ j in l, 0 < q j)
    (hdiv : Tendsto (fun j ↦ logLik x (b j) p (q j) c) l atTop) :
    (∀ᶠ j in l, q j < 1) ∧ Tendsto b l (𝓝 M) := by
  obtain ⟨i, hi⟩ := heq
  have hM : 0 < M := hi ▸ hx i
  obtain ⟨C, hC⟩ := bounded_shape_upper hp
  let U := p * log (1 + p) - log (Gamma p) + p * (log (2 * p) - 1)
  let B := ∑ i, (log c - log (x i) + max C U)
  have hunit {v : ℝ} (hv : M < v) (i : Fin n) :
      unitData x v c i ∈ Ioo (0 : ℝ) 1 := by
    have hv₀ := hM.trans hv
    exact ⟨rpow_pos_of_pos (div_pos (hx i) hv₀) _,
      by simpa only [unitData, one_rpow] using
        rpow_lt_rpow (div_nonneg (hx i).le hv₀.le)
          ((div_lt_one hv₀).mpr ((hle i).trans_lt hv)) hc⟩
  have hregular {v Q : ℝ} (hv : M < v) (hQ : 1 ≤ Q) :
      logLik x v p Q c ≤ B := by
    unfold logLik B
    apply Finset.sum_le_sum
    intro i _
    let z := unitData x v c i
    have hz : z ∈ Ioo (0 : ℝ) 1 := hunit hv i
    have hQ₀ := zero_lt_one.trans_le hQ
    by_cases hQ₂ : Q ≤ 2
    · have hβ := (hC Q hQ₀ hQ₂).trans (le_max_left C U)
      have hpz := mul_nonpos_of_nonneg_of_nonpos hp.le (log_nonpos hz.1.le hz.2.le)
      have hQz := mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hQ)
        (log_nonpos (sub_nonneg.mpr hz.2.le) (sub_le_self _ hz.1.le))
      change _ + p * log z + (Q - 1) * log (1 - z) ≤ _
      linarith
    · have hβ := large_shape_upper hp hQ
      have hl : log (1 - z) ≤ -z := by
        linarith [log_le_sub_one_of_pos (sub_pos.mpr hz.2)]
      have htail : (Q - 1) * log (1 - z) ≤ -(Q * z / 2) := by
        linarith [mul_le_mul_of_nonneg_left hl (sub_nonneg.mpr hQ),
          mul_nonneg (show 0 ≤ Q - 2 by linarith [lt_of_not_ge hQ₂]) hz.1.le]
      have harg : 0 < Q * z / (2 * p) := div_pos (mul_pos hQ₀ hz.1) (by positivity)
      have hbound := log_le_sub_one_of_pos harg
      rw [log_div (mul_pos hQ₀ hz.1).ne' (by positivity : 2 * p ≠ 0),
        log_mul hQ₀.ne' hz.1.ne'] at hbound
      have he : p * (Q * z / (2 * p)) = Q * z / 2 := by field_simp
      have hbound := mul_le_mul_of_nonneg_left hbound hp.le
      have hM := le_max_right C U
      dsimp [U] at hM
      change _ + p * log z + (Q - 1) * log (1 - z) ≤ _
      nlinarith
  have hsub : ∀ᶠ j in l, q j < 1 := by
    filter_upwards [hb, hdiv.eventually_gt_atTop B] with j hj hgt
    exact lt_of_not_ge fun h ↦ hgt.not_ge (hregular hj h)
  refine ⟨hsub, tendsto_order.mpr ⟨fun v hv ↦ hb.mono fun j hj ↦ hv.trans hj, ?_⟩⟩
  intro v hv
  let B' := ∑ i, (log c - log (x i) + C - log (1 - unitData x v c i))
  filter_upwards [hb, hq, hsub, hdiv.eventually_gt_atTop B'] with j hj hQ₀ hQ hgt
  by_contra! h
  apply hgt.not_ge
  unfold logLik B'
  apply Finset.sum_le_sum
  intro i _
  have hu := hunit hj i
  have hv' := hunit hv i
  have hz : unitData x (b j) c i ≤ unitData x v c i :=
    rpow_le_rpow (div_nonneg (hx i).le (hM.trans hj).le)
      (div_le_div_of_nonneg_left (hx i).le (hM.trans hv) h) hc.le
  have hl := log_le_log (sub_pos.mpr hv'.2) (sub_le_sub_left hz 1)
  have hpen := mul_le_mul_of_nonpos_left hl (sub_nonpos.mpr hQ.le)
  have hl₀ := log_nonpos (sub_nonneg.mpr hv'.2.le) (sub_le_self _ hv'.1.le)
  have hpz := mul_nonpos_of_nonneg_of_nonpos hp.le (log_nonpos hu.1.le hu.2.le)
  nlinarith [hC (q j) hQ₀ (by linarith), mul_nonpos_of_nonneg_of_nonpos hQ₀.le hl₀]

end UnivariateML.Endpoint3GeneralizedBetaSingular

end
