module

public import UnivariateML.Family.Endpoint2GeneralizedBeta
public import UnivariateML.Family.Endpoint2GeneralizedBetaJoint
public import UnivariateML.Generic.Endpoint3GammaRatio

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized beta endpoint and held-scale limits

## Deviations

The gamma face and constant concentration path are statements about sample
log likelihood. The corresponding weak convergence statements remain open.
-/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint3GeneralizedBeta

open Endpoint2GeneralizedBeta

variable {n : ℕ}

/-- At fixed shapes, every divergent strict-interior subunit-shape
sequence approaches the sample maximum. Away from that endpoint the
positive spike terms have a common finite upper bound. -/
theorem subunit_divergence_requires_contact {ι : Type*} {l : Filter ι}
    {x : Fin n → ℝ} {M p q c : ℝ} (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M)
    (hp : 0 < p) (hq : q < 1) (hc : 0 < c) {b : ι → ℝ}
    (hb : ∀ᶠ j in l, M < b j) (hdiv : Tendsto (fun j ↦ logLik x (b j) p q c) l atTop) :
    Tendsto b l (𝓝 M) := by
  apply tendsto_order.mpr
  refine ⟨fun v hv ↦ hb.mono fun j hj ↦ hv.trans hj, ?_⟩
  intro v hv
  let z := unitData x v c
  let B := ∑ i, (log c - log (x i) - Beta.logBeta (p, q) + (q - 1) * log (1 - z i))
  filter_upwards [hb, hdiv.eventually_gt_atTop B] with j hj hB
  by_contra! h
  apply hB.not_ge
  unfold logLik B
  apply Finset.sum_le_sum
  intro i _
  have hvpos : 0 < v := (hx i).trans_le ((hle i).trans hv.le)
  have hbpos : 0 < b j := hvpos.trans_le h
  have hratio : x i / b j ≤ x i / v :=
    div_le_div_of_nonneg_left (hx i).le hvpos h
  have hz : unitData x (b j) c i ≤ z i :=
    rpow_le_rpow (div_nonneg (hx i).le hbpos.le) hratio hc.le
  have hz1 : z i < 1 := by
    dsimp [z, unitData]
    simpa only [one_rpow] using
      rpow_lt_rpow (div_nonneg (hx i).le hvpos.le)
        ((div_lt_one hvpos).mpr ((hle i).trans_lt hv)) hc
  have hlog : log (1 - z i) ≤ log (1 - unitData x (b j) c i) :=
    log_le_log (sub_pos.mpr hz1) (sub_le_sub_left hz 1)
  have hpterm : p * log (unitData x (b j) c i) ≤ 0 :=
    mul_nonpos_of_nonneg_of_nonpos hp.le
      (log_nonpos (rpow_nonneg (div_nonneg (hx i).le hbpos.le) _) (hz.trans hz1.le))
  have hqterm := mul_le_mul_of_nonpos_left hlog (sub_nonpos.mpr hq.le)
  linarith

/-- The sole finite endpoint fit is sample contact at unit upper shape.
Above one, a unique interior endpoint solves the normalized score. -/
theorem fixed_shapes_endpoint_uniqueness (hn : 0 < n) {x : Fin n → ℝ} {M p q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) (hc : 0 < c) :
    let L := fun b ↦ logLik x b p q c
    (q = 1 → IsMaxOn L (Ici M) M ∧ ∀ b ∈ Ici M, IsMaxOn L (Ici M) b → b = M) ∧
    (1 < q → (∃! b, b ∈ Ioi M ∧ IsMaxOn L (Ioi M) b) ∧
      ∀ b ∈ Ioi M, IsMaxOn L (Ioi M) b ↔
        Endpoint2BetaWidth.score (unitData x M c) q p ((M / b) ^ c) = 0) := by
  intro L
  have hf := fixed_shapes_endpoint_fit (q := q) hn hx hle heq hp hc
  constructor
  · intro hq
    simpa only [L, hq, ite_true] using hf.2.1 hq
  · intro hq
    refine ⟨?_, fun b hb ↦ fixed_shapes_endpoint_score hn hx hle heq hp hq hc hb⟩
    simpa only [hq.ne', ite_false] using hf.2.2 hq

/-- A constant positive sample strictly below the held endpoint concentrates
along the written joint upper-shape and power ray with first shape fixed. -/
theorem held_scale_first_shape_constant_path (hn : 0 < n) {v b p : ℝ}
    (hv : 0 < v) (hb : v < b) (hp : 0 < p) :
    Tendsto (fun c ↦ logLik (fun _ : Fin n ↦ v) b p
      (p * exp (c * log (b / v))) c) atTop atTop := by
  let D := log (b / v)
  let q := fun c : ℝ ↦ p * exp (c * D)
  let z := fun c : ℝ ↦ exp (-(c * D))
  have hD : 0 < D := log_pos ((one_lt_div hv).mpr hb)
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have harg : Tendsto (fun c : ℝ ↦ c * D) atTop atTop := tendsto_id.atTop_mul_const hD
  have hq : Tendsto q atTop atTop := (tendsto_exp_atTop.comp harg).const_mul_atTop hp
  have hz : Tendsto z atTop (𝓝 0) := tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp harg)
  let E := log v + log (Gamma p) - p * log p + 2 * p + 1
  have hbase : Tendsto (fun c : ℝ ↦ (n : ℝ) * (log c - E)) atTop atTop :=
    (tendsto_atTop_add_const_right _ (-E) tendsto_log_atTop).const_mul_atTop hN
  apply tendsto_atTop_mono' _ ?_ hbase
  filter_upwards [hq.eventually_ge_atTop (1 : ℝ),
    hz.eventually (Iio_mem_nhds (by norm_num : (0 : ℝ) < 1 / 2)),
    ((Endpoint3GammaRatio.log_gamma_ratio p hp).comp hq).eventually
      (Ioi_mem_nhds (by norm_num : (-1 : ℝ) < 0))]
    with c hq₁ hz₁ hr
  change 1 ≤ q c at hq₁
  change z c < 1 / 2 at hz₁
  change -1 < log (Gamma (q c + p)) - log (Gamma (q c)) - p * log (q c) at hr
  have hb₀ : 0 < b := hv.trans hb
  have hz₀ : 0 < z c := exp_pos _
  have hw : 0 < 1 - z c := by linarith
  have hqz : q c * z c = p := by
    dsimp [q, z]
    rw [mul_assoc, ← exp_add, add_neg_cancel, exp_zero, mul_one]
  have hprod : log (q c) + log (z c) = log p := by
    rw [← log_mul (zero_lt_one.trans_le hq₁).ne' hz₀.ne', hqz]
  have hlog : -log (1 - z c) ≤ z c / (1 - z c) := by
    have h := log_le_sub_one_of_pos (inv_pos.mpr hw)
    rw [log_inv] at h
    have he : (1 - z c)⁻¹ - 1 = z c / (1 - z c) := by
      field_simp [hw.ne']
      ring
    exact he ▸ h
  have hpenalty : (q c - 1) * -log (1 - z c) ≤ 2 * p := by
    calc
      _ ≤ (q c - 1) * (z c / (1 - z c)) :=
        mul_le_mul_of_nonneg_left hlog (sub_nonneg.mpr hq₁)
      _ ≤ q c * (z c / (1 - z c)) :=
        mul_le_mul_of_nonneg_right (sub_le_self _ zero_le_one) (div_nonneg hz₀.le hw.le)
      _ = p / (1 - z c) := by rw [mul_div_assoc', hqz]
      _ ≤ 2 * p := by
        apply (div_le_iff₀ hw).mpr
        nlinarith [mul_nonneg hp.le (show 0 ≤ 1 - 2 * z c by linarith)]
  have hunit (i : Fin n) : unitData (fun _ : Fin n ↦ v) b c i = z c := by
    unfold unitData z D
    rw [rpow_def_of_pos (div_pos hv hb₀), log_div hv.ne' hb₀.ne', log_div hb₀.ne' hv.ne']
    congr 1
    ring
  change (n : ℝ) * (log c - E) ≤ logLik (fun _ : Fin n ↦ v) b p (q c) c
  simp only [logLik, hunit, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  apply mul_le_mul_of_nonneg_left _ hN.le
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos (zero_lt_one.trans_le hq₁))).ne'
      (Gamma_pos_of_pos (add_pos hp (zero_lt_one.trans_le hq₁))).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos (zero_lt_one.trans_le hq₁)).ne',
    add_comm p (q c)]
  dsimp [E]
  linear_combination (norm := ring_nf) le_of_lt hr + hpenalty - p * hprod
  exact le_rfl

private theorem exp_gap_limit {u : ℝ} (hu : 0 < u) :
    Tendsto (fun p : ℝ ↦ p * (1 - exp (-(u / p)))) atTop (𝓝 u) := by
  have hh : Tendsto (fun p : ℝ ↦ 1 + u / p) atTop (𝓝 1) := by
    simpa [div_eq_mul_inv] using
      (tendsto_inv_atTop_zero.const_mul u).const_add (1 : ℝ)
  have hl : Tendsto (fun p : ℝ ↦ u / (1 + u / p)) atTop (𝓝 u) := by
    simpa [div_eq_mul_inv] using (hh.inv₀ one_ne_zero).const_mul u
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hl tendsto_const_nhds
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    let t := u / p
    have ht : 0 < t := div_pos hu hp
    have he : exp (-t) ≤ (1 + t)⁻¹ := by
      rw [exp_neg]
      exact (inv_le_inv₀ (exp_pos _) (by positivity)).mpr (by linarith [add_one_le_exp t])
    have ht' : 1 - (1 + t)⁻¹ = t / (1 + t) := by
      field_simp [show 1 + t ≠ 0 by positivity]
      ring
    have hlow := mul_le_mul_of_nonneg_left (sub_le_sub_left he 1) hp.le
    rw [ht', ← mul_div_assoc, show p * t = u by dsimp [t]; field_simp [hp.ne']] at hlow
    exact hlow
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
    have hh' : 1 - exp (-(u / p)) ≤ u / p := by
      linarith [add_one_le_exp (-(u / p))]
    calc
      _ ≤ p * (u / p) := mul_le_mul_of_nonneg_left hh' hp.le
      _ = u := by field_simp [hp.ne']

/-- At a held endpoint, increasing the first shape while keeping its
product with power equal to the rate gives the gamma face on log
distances. The right side includes the Jacobian on the original data. -/
theorem held_endpoint_gamma_path {x : Fin n → ℝ} {b q r : ℝ}
    (hx : ∀ i, 0 < x i ∧ x i < b) (hq : 0 < q) (hr : 0 < r) :
    let d := fun i ↦ log (b / x i)
    Tendsto (fun p ↦ logLik x b p q (r / p)) atTop
      (𝓝 (∑ i, (q * log r + (q - 1) * log (d i) -
        r * d i - log (Gamma q) - log (x i)))) := by
  intro d
  unfold logLik
  apply tendsto_finsetSum
  intro i _
  have hb : 0 < b := (hx i).1.trans (hx i).2
  have hd : 0 < d i := log_pos ((one_lt_div (hx i).1).mpr (hx i).2)
  let u := r * d i
  have hu : 0 < u := mul_pos hr hd
  have hgap := (continuousAt_log hu.ne').tendsto.comp (exp_gap_limit hu)
  let A := log r - log (x i) - log (Gamma q) - u
  have hlim := ((tendsto_const_nhds (x := A)).add
    (Endpoint3GammaRatio.log_gamma_ratio q hq)).add (hgap.const_mul (q - 1))
  have he : A + 0 + (q - 1) * log u =
      q * log r + (q - 1) * log (d i) - r * d i - log (Gamma q) - log (x i) := by
    dsimp [A, u]
    rw [log_mul hr.ne' hd.ne']
    ring
  rw [he] at hlim
  apply hlim.congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with p hp
  have hunit : unitData x b (r / p) i = exp (-(u / p)) := by
    unfold unitData u d
    rw [rpow_def_of_pos (div_pos (hx i).1 hb), log_div (hx i).1.ne' hb.ne',
      log_div hb.ne' (hx i).1.ne']
    congr 1
    ring
  have hgap₀ : 0 < 1 - exp (-(u / p)) :=
    sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos (div_pos hu hp)))
  simp only [Function.comp_apply, hunit, log_exp, A]
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div hr.ne' hp.ne', log_mul hp.ne' hgap₀.ne',
    log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
      (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']
  field_simp [hp.ne']
  ring

/-- At fixed beta shapes every attained endpoint and power fit is
unique. Its endpoint is sample contact at unit upper shape and is
strictly above the sample at greater upper shape. -/
theorem fixed_beta_shapes_joint_uniqueness (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 p q v.1
    (∃ v ∈ D, IsMaxOn L D v) →
      ∃! v, v ∈ D ∧ IsMaxOn L D v ∧ (v.2 = M ↔ q = 1) := by
  intro D L h
  have hf := Endpoint2GeneralizedBetaJoint.fixed_shapes_joint_criterion
    (q := q) hn hx hle heq hp
  obtain ⟨v, ⟨hv, hm⟩, hu⟩ := hf.2 (hf.1.mp h)
  have hcontact : v.2 = M ↔ q = 1 := by
    constructor
    · intro he
      by_contra hq
      have hb : M < v.2 := by simpa [hq] using hv.2
      exact hb.ne' he
    · intro hq
      have fit := (fixed_shapes_endpoint_fit (q := q) hn hx hle heq hp hv.1).2.1 hq
      have hmem : v.2 ∈ Ici M := by simpa [hq] using hv.2
      apply fit.2 v.2 (by simpa [hq] using hmem)
      intro b hb
      have hfv : (v.1, b) ∈ D := ⟨hv.1, by simpa [hq] using hb⟩
      exact (show L (v.1, b) ≤ L v from hm hfv)
  exact ⟨v, ⟨hv, hm, hcontact⟩, fun w hw ↦ hu w ⟨hw.1, hw.2.1⟩⟩

end UnivariateML.Endpoint3GeneralizedBeta

end
