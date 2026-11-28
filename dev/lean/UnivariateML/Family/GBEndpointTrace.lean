module

public import UnivariateML.Family.Endpoint2GeneralizedBeta

import Mathlib.Analysis.SpecialFunctions.Integrals.Basic
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Uniform subunit endpoint contact error

The estimate retains the contribution of every tied maximum, even if
the power and either beta shape change along the endpoint approach.

## Deviations

The rate test keeps the exact log beta normalizer. It treats strict
support with the endpoint already tending to the sample maximum.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.GBEndpointTrace

noncomputable section

private theorem gap_pos {x : ℝ} (hx : 0 < x) : 0 < 1 - exp (-x) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hx))

private theorem log_gap_difference {a d s : ℝ} (ha : 0 < a) (hd : 0 < d) (hs : 0 ≤ s) :
    0 ≤ log (1 - exp (-(a * (d + s)))) - log (1 - exp (-(a * d))) ∧
    log (1 - exp (-(a * (d + s)))) - log (1 - exp (-(a * d))) ≤ log (1 + s / d) := by
  have hds : 0 < d + s := add_pos_of_pos_of_nonneg hd hs
  have hgap := gap_pos (mul_pos ha hd)
  have hgap' := gap_pos (mul_pos ha hds)
  constructor
  · apply sub_nonneg.mpr (log_le_log hgap _)
    apply sub_le_sub_left (exp_le_exp.mpr (neg_le_neg _))
    exact mul_le_mul_of_nonneg_left (le_add_of_nonneg_right hs) ha.le
  · rw [← log_div hgap'.ne' hgap.ne']
    apply log_le_log (div_pos hgap' hgap)
    have hsum : d / (d + s) + s / (d + s) = 1 := by
      rw [← add_div, div_self hds.ne']
    have h := convexOn_exp.2 (mem_univ (-(a * (d + s)))) (mem_univ 0)
      (div_nonneg hd.le hds.le) (div_nonneg hs hds.le) hsum
    simp only [smul_eq_mul, mul_zero, add_zero, exp_zero, mul_one] at h
    have he : d / (d + s) * -(a * (d + s)) = -(a * d) := by
      field_simp
    rw [he] at h
    have h' : 1 - exp (-(a * (d + s))) ≤ (1 + s / d) *
        (1 - exp (-(a * d))) := by
      have H := mul_le_mul_of_nonneg_right h hds.le
      rw [add_mul, mul_right_comm (d / (d + s)),
        div_mul_cancel₀ _ hds.ne', div_mul_cancel₀ _ hds.ne'] at H
      have hr : (1 + s / d) * d = d + s := by
        rw [add_mul, one_mul, div_mul_cancel₀ _ hd.ne']
      apply (mul_le_mul_iff_right₀ hd).mp
      rw [← mul_assoc, mul_comm d (1 + s / d), hr]
      nlinarith
    exact (div_le_iff₀ hgap).mpr h'

variable {n : ℕ}

/-- Log likelihood on the log-observation scale, retaining the exact
normalizer. The input distances are measured from the sample maximum. -/
def endpointObjective (d : Fin n → ℝ) (p q a s : ℝ) : ℝ :=
  ∑ i, (log a - Beta.logBeta (p, q) - p * a * (d i + s) +
    (q - 1) * log (1 - exp (-(a * (d i + s)))))

/-- Away from ties, freeze the endpoint in the log-gap term only. -/
def endpointTrace (d : Fin n → ℝ) (p q a s : ℝ) : ℝ :=
  ∑ i, (log a - Beta.logBeta (p, q) - p * a * (d i + s) +
    (q - 1) * log (1 - exp (-(a * (if d i = 0 then s else d i)))))

/-- For every subunit positive upper shape, the contact trace overestimates
the exact objective by an error uniform in all three shape parameters. -/
theorem endpoint_trace_error {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {p q a s : ℝ} (hq : 0 < q) (hq₁ : q ≤ 1) (ha : 0 < a) (hs : 0 < s) :
    0 ≤ endpointTrace d p q a s - endpointObjective d p q a s ∧
    endpointTrace d p q a s - endpointObjective d p q a s ≤
      ∑ i, (if d i = 0 then 0 else log (1 + s / d i)) := by
  classical
  unfold endpointTrace endpointObjective
  rw [← Finset.sum_sub_distrib]
  have h (i : Fin n) : let w := d i
    let E := log a - Beta.logBeta (p, q) - p * a * (w + s)
    let δ := (E + (q - 1) * log (1 - exp (-(a * (if w = 0 then s else w))))) -
      (E + (q - 1) * log (1 - exp (-(a * (w + s)))))
    0 ≤ δ ∧ δ ≤ (if w = 0 then 0 else log (1 + s / w)) := by
    dsimp
    by_cases hi : d i = 0
    · simp [hi]
    · simp only [ite_eq_right hi]
      have hw := lt_of_le_of_ne (hd i) (Ne.symm hi)
      obtain ⟨hlo, hhi⟩ := log_gap_difference ha hw hs.le
      have hlog := log_nonneg (le_add_of_nonneg_right (div_nonneg hs.le hw.le))
      constructor
      · nlinarith [mul_nonneg (sub_nonneg.mpr hq₁) hlo]
      · have hmul := mul_le_mul_of_nonneg_left hhi (sub_nonneg.mpr hq₁)
        nlinarith [mul_nonneg hq.le hlog]
  exact ⟨Finset.sum_nonneg (fun i _ ↦ (h i).1),
    Finset.sum_le_sum (fun i _ ↦ (h i).2)⟩

/-- Along any strict-support subunit approach to contact, the likelihood
diverges precisely when the contact trace diverges. -/
theorem endpoint_trace_divergence {ι : Type*} {l : Filter ι}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) {p q a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < q j ∧ q j ≤ 1 ∧ 0 < a j ∧ 0 < s j)
    (hs : Tendsto s l (𝓝 0)) :
    Tendsto (fun j ↦ endpointObjective d (p j) (q j) (a j) (s j)) l atTop ↔
      Tendsto (fun j ↦ endpointTrace d (p j) (q j) (a j) (s j)) l atTop := by
  classical
  let E := fun j ↦ endpointTrace d (p j) (q j) (a j) (s j) -
    endpointObjective d (p j) (q j) (a j) (s j)
  have htop : Tendsto (fun t : ℝ ↦ ∑ i, if d i = 0 then 0 else log (1 + t / d i))
      (𝓝 0) (𝓝 0) := by
    rw [← Finset.sum_const_zero (s := (Finset.univ : Finset (Fin n))) (M := ℝ)]
    apply tendsto_finsetSum
    intro i _
    by_cases hi : d i = 0
    · simp [hi]
    · have hh : ContinuousAt (fun t : ℝ ↦ 1 + t / d i) 0 := by fun_prop
      simpa [hi] using (hh.log (by norm_num)).tendsto
  have hb := hdom.mono fun j hj ↦ endpoint_trace_error (p := p j) hd hj.1 hj.2.1 hj.2.2.1 hj.2.2.2
  have hE : Tendsto E l (𝓝 0) := tendsto_of_tendsto_of_tendsto_of_le_of_le'
    tendsto_const_nhds (htop.comp hs) (hb.mono fun _ h ↦ h.1) (hb.mono fun _ h ↦ h.2)
  constructor
  · intro h
    exact (h.atTop_add hE).congr (fun j ↦ by dsimp [E]; ring)
  · intro h
    exact (h.atTop_add hE.neg).congr (fun j ↦ by dsimp [E]; ring)

open Endpoint2GeneralizedBeta

/-- The endpoint chart agrees with the physical log likelihood up to
the sample log Jacobian. -/
theorem endpoint_objective_eq {x : Fin n → ℝ} {M b p q a : ℝ}
    (hx : ∀ i, 0 < x i) (hM : 0 < M) (hb : 0 < b) :
    logLik x b p q a =
      endpointObjective (fun i ↦ log (M / x i)) p q a (log (b / M)) -
        ∑ i, log (x i) := by
  have hunit (i : Fin n) : unitData x b a i =
      exp (-(a * (log (M / x i) + log (b / M)))) := by
    rw [unitData, rpow_def_of_pos (div_pos (hx i) hb),
      log_div (hx i).ne' hb.ne', log_div hM.ne' (hx i).ne', log_div hb.ne' hM.ne']
    congr 1
    ring
  unfold logLik endpointObjective
  rw [← Finset.sum_sub_distrib]
  apply Finset.sum_congr rfl
  intro i _
  rw [hunit, log_exp]
  ring

private theorem log_linear_tail {c : ℝ} (hc : 0 < c) :
    Tendsto (fun x : ℝ ↦ log x - c * x) atTop atBot := by
  have h : Tendsto (fun x : ℝ ↦ log x / x) atTop (𝓝 0) := by
    simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
  have hlin : Tendsto (fun x : ℝ ↦ -(c / 2) * x) atTop atBot :=
    tendsto_id.const_mul_atTop_of_neg (show -(c / 2) < 0 by linarith)
  apply tendsto_atBot_mono' atTop _ hlin
  filter_upwards [h.eventually (gt_mem_nhds (half_pos hc)), eventually_gt_atTop (0 : ℝ)]
    with x hx hx₀
  linarith [(div_lt_iff₀ hx₀).mp hx]

/-- Holding both shapes at regular upper shape yields compact
power/endpoint superlevels on every nonconstant sample, including closed
endpoint contact when the upper shape is one. -/
theorem fixed_beta_shapes_compact_superlevels {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hzero : ∃ i, d i = 0) (hpos : ∃ i, 0 < d i)
    {p q : ℝ} (hp : 0 < p) (hq : 1 ≤ q) (L : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici 0 else Ioi 0)
    IsCompact {v ∈ D | L ≤ endpointObjective d p q v.1 v.2} := by
  intro D
  obtain ⟨j, hj⟩ := hzero
  obtain ⟨k, hk⟩ := hpos
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr (Nat.zero_lt_of_lt k.isLt)
  let c := p * (∑ i, d i) / n
  let β := Beta.logBeta (p, q)
  have hc : 0 < c := div_pos (mul_pos hp (Finset.sum_pos'
    (fun i _ ↦ hd i) ⟨k, Finset.mem_univ _, hk⟩)) hN
  have hβ (v : ℝ × ℝ) :
      endpointObjective d p q v.1 v.2 =
        (n : ℝ) * (log v.1 - c * v.1 - β) - (n : ℝ) * p * v.1 * v.2 +
          (q - 1) * ∑ i, log (1 - exp (-(v.1 * (d i + v.2)))) := by
    simp only [endpointObjective, Finset.sum_add_distrib, Finset.sum_sub_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum]
    dsimp [c, β]
    field_simp
    ring
  have hs (v : ℝ × ℝ) (hv : v ∈ D) : 0 ≤ v.2 := by
    have h := hv.2
    by_cases he : q = 1
    · simpa [he] using h
    · exact (show 0 < v.2 by simpa [he] using h).le
  have hgap (v : ℝ × ℝ) (hv : v ∈ D) (i : Fin n) :
      log (1 - exp (-(v.1 * (d i + v.2)))) ≤ 0 := by
    apply log_nonpos
    · exact sub_nonneg.mpr (exp_le_one_iff.mpr
        (neg_nonpos.mpr (mul_nonneg (show 0 ≤ v.1 from hv.1.le) (add_nonneg (hd i) (hs v hv)))))
    · linarith [exp_pos (-(v.1 * (d i + v.2)))]
  have hF (v : ℝ × ℝ) (hv : v ∈ D) :
      endpointObjective d p q v.1 v.2 ≤
        (n : ℝ) * (log v.1 - c * v.1 - β) := by
    rw [hβ]
    have H := Finset.sum_nonpos (s := Finset.univ) (fun i _ ↦ hgap v hv i)
    linarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq) H,
      mul_nonneg (mul_nonneg (mul_nonneg hN.le hp.le) (show 0 ≤ v.1 from hv.1.le)) (hs v hv)]
  let R := L / n + β
  have hlow : ∀ᶠ a in 𝓝[>] (0 : ℝ), log a < R := tendsto_log_nhdsGT_zero.eventually_lt_atBot _
  obtain ⟨A, hA, ha⟩ := (nhdsGT_basis (0 : ℝ)).eventually_iff.mp hlow
  obtain ⟨B, hb⟩ := eventually_atTop.mp ((log_linear_tail hc).eventually_lt_atBot R)
  let S := (log B - β - L / n) / (p * A)
  let δ := if q = 1 then 0 else exp ((L - (n : ℝ) * (log B - β)) / (q - 1)) / B
  let K := Icc A B ×ˢ Icc δ S
  have hbox (v : ℝ × ℝ) (hv : v ∈ D) (hL : L ≤ endpointObjective d p q v.1 v.2) :
      v ∈ K := by
    have hu : R ≤ log v.1 - c * v.1 := by
      dsimp [R]
      have h := (div_le_iff₀ hN).2 (show L ≤ (log v.1 - c * v.1 - β) * n by
        rw [mul_comm]; exact hL.trans (hF v hv))
      linarith
    have hv₁ : v.1 ∈ Icc A B := ⟨le_of_not_gt (fun h ↦ by
      have hh := ha ⟨hv.1, h⟩
      linarith [mul_nonneg hc.le (show 0 ≤ v.1 from hv.1.le)]),
      le_of_not_gt fun h ↦ (hb _ h.le).not_ge hu⟩
    have hB : 0 < B := hA.trans_le (hv₁.1.trans hv₁.2)
    have hlog := log_le_log (show 0 < v.1 from hv.1) hv₁.2
    have htail := Finset.sum_nonpos (s := Finset.univ) (fun i _ ↦ hgap v hv i)
    rw [hβ] at hL
    refine ⟨hv₁, ?_, ?_⟩
    · by_cases he : q = 1
      · simpa [δ, he] using hs v hv
      · have hq' := lt_of_le_of_ne hq (Ne.symm he)
        have hv₂ : 0 < v.2 := by simpa [D, he] using hv.2
        have hneg := Finset.single_le_sum (s := Finset.univ)
          (f := fun i ↦ -log (1 - exp (-(v.1 * (d i + v.2)))))
          (fun i _ ↦ neg_nonneg.mpr (hgap v hv i)) (Finset.mem_univ j)
        rw [Finset.sum_neg_distrib] at hneg
        have ht := neg_le_neg_iff.mp hneg
        rw [hj, zero_add] at ht
        have hg := log_le_log (gap_pos (mul_pos (show 0 < v.1 from hv.1) hv₂))
          (show 1 - exp (-(v.1 * v.2)) ≤ B * v.2 by
            linarith [add_one_le_exp (-(v.1 * v.2)),
              mul_le_mul_of_nonneg_right hv₁.2 hv₂.le])
        have hu' : (L - (n : ℝ) * (log B - β)) / (q - 1) ≤ log (B * v.2) := by
          apply (div_le_iff₀ (sub_pos.mpr hq')).mpr
          nlinarith [mul_le_mul_of_nonneg_left (ht.trans hg) (sub_nonneg.mpr hq),
            mul_nonneg (mul_nonneg (mul_nonneg hN.le hp.le)
              (show 0 ≤ v.1 from hv.1.le)) hv₂.le,
            mul_nonneg (mul_nonneg hN.le hc.le) (show 0 ≤ v.1 from hv.1.le)]
        dsimp [δ]
        rw [ite_eq_right he]
        apply (div_le_iff₀ hB).mpr
        simpa [exp_log (mul_pos hB hv₂), mul_comm] using exp_le_exp.mpr hu'
    · apply (le_div_iff₀ (mul_pos hp hA)).mpr
      have hu' := (div_le_iff₀ hN).2 (show L ≤
          (log B - β - p * A * v.2) * n by
        nlinarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq) htail,
          mul_nonneg (mul_nonneg hN.le hc.le) (show 0 ≤ v.1 from hv.1.le),
          mul_le_mul_of_nonneg_left
            (mul_le_mul_of_nonneg_right hv₁.1 (hs v hv)) (mul_nonneg hN.le hp.le)])
      linarith
  let F := fun v : ℝ × ℝ ↦ endpointObjective d p q v.1 v.2
  have hKD : K ⊆ D := by
    intro v hv
    refine ⟨hA.trans_le hv.1.1, ?_⟩
    by_cases he : q = 1
    · simpa [δ, he] using hv.2.1
    · have hB : 0 < B := hA.trans_le (hv.1.1.trans hv.1.2)
      have hδ : 0 < δ := by simp [δ, he, div_pos (exp_pos _) hB]
      simpa [he] using hδ.trans_le hv.2.1
  have hFcont : ContinuousOn F K := by
    intro v hv
    apply ContinuousAt.continuousWithinAt
    change ContinuousAt (fun v : ℝ × ℝ ↦ ∑ i,
      (log v.1 - Beta.logBeta (p, q) - p * v.1 * (d i + v.2) +
        (q - 1) * log (1 - exp (-(v.1 * (d i + v.2)))))) v
    apply tendsto_finsetSum
    intro i _
    have h₁ : ContinuousAt (fun w : ℝ × ℝ ↦
        log w.1 - Beta.logBeta (p, q) - p * w.1 * (d i + w.2)) v := by
      fun_prop (disch := exact (hA.trans_le hv.1.1).ne')
    by_cases he : q = 1
    · simpa [he] using h₁.tendsto
    · have hv₂ : 0 < v.2 := by simpa [D, he] using (hKD hv).2
      have harg : ContinuousAt (fun w : ℝ × ℝ ↦
          1 - exp (-(w.1 * (d i + w.2)))) v := by fun_prop
      have hgap := gap_pos (mul_pos (hA.trans_le hv.1.1) (add_pos_of_nonneg_of_pos (hd i) hv₂))
      exact (h₁.add (continuousAt_const.mul (harg.log hgap.ne'))).tendsto
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  let _ : CompactSpace K := isCompact_iff_compactSpace.mp hK
  have hclosed : IsClosed {v : K | L ≤ F v} := isClosed_Ici.preimage hFcont.domRestrict
  convert hclosed.isCompact.image continuous_subtype_val using 1
  ext v
  constructor
  · rintro ⟨hv, hL⟩
    exact ⟨⟨v, hbox v hv hL⟩, hL, rfl⟩
  · rintro ⟨w, hw, rfl⟩
    exact ⟨hKD w.property, hw⟩

private theorem unit_objective {d : Fin n → ℝ} {p a : ℝ} (hp : 0 < p) (ha : 0 < a)
    (s : ℝ) :
    endpointObjective d p 1 a s =
      (n : ℝ) * log (p * a) - p * a * ((∑ i, d i) + n * s) := by
  have hβ : Beta.logBeta (p, 1) = -log p := by
    unfold Beta.logBeta Beta.normalizer
    dsimp
    rw [Gamma_one, mul_one, Gamma_add_one hp.ne', log_div (Gamma_pos_of_pos hp).ne'
      (mul_ne_zero hp.ne' (Gamma_pos_of_pos hp).ne'),
      log_mul hp.ne' (Gamma_pos_of_pos hp).ne']
    ring
  simp only [endpointObjective, hβ, sub_self, zero_mul, add_zero, sub_neg_eq_add,
    Finset.sum_sub_distrib, Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, log_mul hp.ne' ha.ne']
  ring

/-- The unit upper shape has a closed-contact ridge on nonconstant
data. Its exact rate is the reciprocal of the mean log distance. -/
theorem unit_endpoint_ridge (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hpos : ∃ i, 0 < d i) :
    let D := Ioi (0 : ℝ) ×ˢ (Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ))
    let F := fun v : ℝ × ℝ × ℝ ↦ endpointObjective d v.1 1 v.2.1 v.2.2
    ∀ v ∈ D, IsMaxOn F D v ↔ v.2.2 = 0 ∧ v.1 * v.2.1 = (n : ℝ) / (∑ i, d i) := by
  intro D F
  let m := ∑ i, d i
  have hm : 0 < m := Finset.sum_pos' (fun i _ ↦ hd i)
    (hpos.imp fun i hi ↦ ⟨Finset.mem_univ i, hi⟩)
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  let r := (n : ℝ) / m
  have hr : 0 < r := div_pos hN hm
  let w : ℝ × ℝ × ℝ := (r, 1, 0)
  have hw : w ∈ D := ⟨hr, one_pos, le_rfl⟩
  have hcmp (v : ℝ × ℝ × ℝ) (hv : v ∈ D) :
      F v ≤ F w ∧ (F v = F w → v.2.2 = 0 ∧ v.1 * v.2.1 = r) := by
    let u := v.1 * v.2.1
    have hu : 0 < u := mul_pos hv.1 hv.2.1
    have hs : 0 ≤ v.2.2 := hv.2.2
    have hF : F v = (n : ℝ) * log u - u * (m + n * v.2.2) :=
      unit_objective hv.1 hv.2.1 _
    have hFw : F w = (n : ℝ) * log r - r * m := by
      dsimp [F, w]
      rw [unit_objective hr one_pos]
      simp [m]
    have hrate : r * m = n := div_mul_cancel₀ _ hm.ne'
    have hopt : ∀ {g : ℝ}, g ≤ u / r - 1 → (n : ℝ) * g ≤ u * m - r * m := by
      intro g hg
      have H := (le_div_iff₀ hr).mp (show g + 1 ≤ u / r by linarith)
      have HH := mul_le_mul_of_nonneg_right H hm.le
      rw [mul_comm u m, ← mul_assoc] at HH
      nlinarith [congrArg (fun z : ℝ ↦ g * z) hrate]
    have H := log_le_sub_one_of_pos (div_pos hu hr)
    rw [log_div hu.ne' hr.ne'] at H
    have hopt' := hopt H
    have hcoef : 0 < u * (n : ℝ) := mul_pos hu hN
    refine ⟨?_, ?_⟩
    · rw [hF, hFw]
      nlinarith [mul_nonneg hcoef.le hs]
    · intro he
      rw [hF, hFw] at he
      have hz : v.2.2 = 0 := by
        by_contra hne
        nlinarith [mul_pos hcoef (lt_of_le_of_ne hs (Ne.symm hne))]
      refine ⟨hz, ?_⟩
      by_contra hne
      have hne' : u / r ≠ 1 := fun h ↦ hne ((div_eq_iff hr.ne').mp h |>.trans (one_mul r))
      have H' := log_lt_sub_one_of_pos (div_pos hu hr) hne'
      rw [log_div hu.ne' hr.ne'] at H'
      have HH := (lt_div_iff₀ hr).mp
        (show log u - log r + 1 < u / r by linarith)
      have H'' := mul_lt_mul_of_pos_right HH hm
      have hmul : (log u - log r + 1) * r * m =
          (log u - log r + 1) * (n : ℝ) := by rw [mul_assoc, hrate]
      rw [hmul] at H''
      simp only [hz, mul_zero, add_zero] at he
      linarith
  intro v hv
  constructor
  · intro h
    exact (hcmp v hv).2 (le_antisymm (hcmp v hv).1 (h hw))
  · rintro ⟨hs, hu⟩ u huD
    have hFv : F v = F w := by
      change endpointObjective d v.1 1 v.2.1 v.2.2 = endpointObjective d r 1 1 0
      rw [unit_objective hv.1 hv.2.1, unit_objective hr one_pos, hs, hu, mul_one]
    exact hFv ▸ (hcmp u huD).1

/-- Fixing the power reduces the unit-shape ridge to a unique pair of
first shape and closed endpoint, even with tied maxima. -/
theorem held_power_unit_endpoint_unique (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hpos : ∃ i, 0 < d i) {a : ℝ} (ha : 0 < a) :
    let D := Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ)
    let F := fun v : ℝ × ℝ ↦ endpointObjective d v.1 1 a v.2
    ∀ v ∈ D, IsMaxOn F D v ↔ v = ((n : ℝ) / (a * ∑ i, d i), 0) := by
  intro D F
  have hm : 0 < (∑ i, d i) := Finset.sum_pos' (fun i _ ↦ hd i)
    (hpos.imp fun i hi ↦ ⟨Finset.mem_univ i, hi⟩)
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  let p := (n : ℝ) / (a * ∑ i, d i)
  have hp : 0 < p := div_pos hN (mul_pos ha hm)
  have hpa : p * a = (n : ℝ) / (∑ i, d i) := by
    dsimp [p]
    field_simp
  have hm' := (unit_endpoint_ridge hn hd hpos (p, a, 0) ⟨hp, ha, le_rfl⟩).mpr ⟨rfl, hpa⟩
  have hw : (p, (0 : ℝ)) ∈ D := ⟨hp, le_rfl⟩
  have hbest : IsMaxOn F D (p, 0) :=
    fun v hv ↦ hm' (show (v.1, a, v.2) ∈ Ioi (0 : ℝ) ×ˢ (Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ))
      from ⟨hv.1, ha, hv.2⟩)
  intro v hv
  constructor
  · intro hm
    have he : F v = F (p, 0) := le_antisymm (hbest hv) (hm hw)
    have hmAll : IsMaxOn (fun v : ℝ × ℝ × ℝ ↦ endpointObjective d v.1 1 v.2.1 v.2.2)
        (Ioi (0 : ℝ) ×ˢ (Ioi (0 : ℝ) ×ˢ Ici (0 : ℝ))) (v.1, a, v.2) := by
      intro w hw
      have h := hm' hw
      change endpointObjective d w.1 1 w.2.1 w.2.2 ≤ F (p, 0) at h
      exact h.trans_eq he.symm
    obtain ⟨hs, hr⟩ := (unit_endpoint_ridge hn hd hpos
      (v.1, a, v.2) ⟨hv.1, ha, hv.2⟩).mp hmAll
    exact Prod.ext (mul_right_cancel₀ ha.ne' (hr.trans hpa.symm)) hs
  · intro he
    change v = (p, 0) at he
    rwa [he]

/-- On a constant sample with both beta shapes fixed and upper shape at
least one, every divergent feasible path has infinite power and tends
to endpoint contact. Each interior beta quantile converges to the
constant observation in units of the sample maximum. -/
theorem fixed_beta_shapes_constant_collapse {ι : Type*} {l : Filter ι}
    (hn : 0 < n) {p q : ℝ} (hp : 0 < p) (hq : 1 ≤ q) {a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < a j ∧ s j ∈ (if q = 1 then Ici 0 else Ioi 0))
    (hdiv : Tendsto (fun j ↦ endpointObjective (fun _ : Fin n ↦ 0) p q (a j) (s j))
      l atTop) :
    Tendsto a l atTop ∧ Tendsto s l (𝓝 0) ∧
      ∀ u ∈ Ioo (0 : ℝ) 1, Tendsto (fun j ↦ exp (s j) * u ^ (a j)⁻¹) l (𝓝 1) := by
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  let β := Beta.logBeta (p, q)
  let F := fun j ↦ endpointObjective (fun _ : Fin n ↦ 0) p q (a j) (s j)
  have hmem : ∀ᶠ j in l, 0 < a j ∧ 0 ≤ s j ∧
      F j ≤ (n : ℝ) * (log (a j) - β - p * a j * s j) := by
    filter_upwards [hdom] with j hj
    have hs : 0 ≤ s j := by
      by_cases he : q = 1
      · simpa [he] using hj.2
      · exact (show 0 < s j by simpa [he] using hj.2).le
    have ht : log (1 - exp (-(a j * s j))) ≤ 0 := log_nonpos
      (sub_nonneg.mpr (exp_le_one_iff.mpr (neg_nonpos.mpr (mul_nonneg hj.1.le hs))))
      (by linarith [exp_pos (-(a j * s j))])
    refine ⟨hj.1, hs, ?_⟩
    simp only [F, endpointObjective, zero_add, Finset.sum_const, Finset.card_univ,
      Fintype.card_fin, nsmul_eq_mul]
    exact mul_le_mul_of_nonneg_left
      (by linarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq) ht]) hN.le
  have ha : Tendsto a l atTop := by
    apply tendsto_atTop.mpr
    intro A
    by_cases hA : 0 < A
    · filter_upwards [hmem, hdiv.eventually_gt_atTop ((n : ℝ) * (log A - β))]
        with j hj hF
      by_contra! h
      have H := log_le_log hj.1 h.le
      have hs := mul_nonneg (mul_nonneg hp.le hj.1.le) hj.2.1
      have H' := mul_le_mul_of_nonneg_left
        (show log (a j) - β - p * a j * s j ≤ log A - β by linarith) hN.le
      exact hF.not_ge (hj.2.2.trans H')
    · filter_upwards [hmem] with j hj
      exact (le_of_not_gt hA).trans hj.1.le
  have hs : Tendsto s l (𝓝 0) := by
    apply tendsto_order.mpr
    refine ⟨fun ε hε ↦ hmem.mono (fun _ h ↦ hε.trans_le h.2.1), fun ε hε ↦ ?_⟩
    let c := p * ε
    have hc : 0 < c := mul_pos hp hε
    filter_upwards [hmem, hdiv.eventually_gt_atTop ((n : ℝ) * (-1 - log c - β))]
      with j hj hF
    by_contra! h
    have hopt := log_le_sub_one_of_pos (mul_pos hc hj.1)
    rw [log_mul hc.ne' hj.1.ne'] at hopt
    have hh := mul_le_mul_of_nonneg_left h (mul_nonneg hp.le hj.1.le)
    have H := mul_le_mul_of_nonneg_left
      (show log (a j) - β - p * a j * s j ≤ -1 - log c - β by dsimp [c] at *; nlinarith)
      hN.le
    exact hF.not_ge (hj.2.2.trans H)
  refine ⟨ha, hs, fun u hu ↦ ?_⟩
  have hT := (tendsto_inv_atTop_zero.comp ha).const_mul (log u)
  have hpow : Tendsto (fun j ↦ u ^ (a j)⁻¹) l (𝓝 1) := by
    convert (continuous_exp.continuousAt.tendsto.comp hT) using 1
    · ext j
      rw [rpow_def_of_pos hu.1]
      rfl
    · simp
  simpa using (continuous_exp.continuousAt.tendsto.comp hs).mul hpow

open MeasureTheory

private theorem beta_integral {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let f := fun z : ℝ ↦ z ^ (p - 1) * (1 - z) ^ (q - 1)
    IntervalIntegrable f volume 0 1 ∧ (∫ z in (0 : ℝ)..1, f z) = Beta.normalizer (p, q) := by
  intro f
  let g := fun z : ℝ ↦ (z : ℂ) ^ ((p : ℂ) - 1) * (1 - (z : ℂ)) ^ ((q : ℂ) - 1)
  have he (z : ℝ) (hz : z ∈ Ioo (0 : ℝ) 1) : (f z : ℂ) = g z := by
    dsimp [f, g]
    rw [Complex.ofReal_mul, Complex.ofReal_cpow hz.1.le,
      Complex.ofReal_cpow (sub_nonneg.mpr hz.2.le)]
    push_cast
    rfl
  have hg := Complex.betaIntegral_convergent
    (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq)
  constructor
  · apply (intervalIntegrable_iff_integrableOn_Ioc_of_le zero_le_one).mpr
    rw [integrableOn_Ioc_iff_integrableOn_Ioo]
    exact (Integrable.iff_ofReal (𝕜 := ℂ)).mpr ((hg.1.mono_set Ioo_subset_Ioc_self).congr
      ((ae_restrict_mem measurableSet_Ioo).mono fun z hz ↦ (he z hz).symm))
  · apply Complex.ofReal_injective
    rw [← intervalIntegral.integral_ofReal]
    calc
      _ = Complex.betaIntegral p q :=
        intervalIntegral.integral_congr_Ioo_of_le zero_le_one he
      _ = _ := by
        rw [Complex.betaIntegral_eq_Gamma_mul_div _ _
          (show 0 < (p : ℂ).re from hp) (show 0 < (q : ℂ).re from hq)]
        rw [← Complex.ofReal_add]
        simp only [Beta.normalizer, Complex.Gamma_ofReal, Complex.ofReal_div, Complex.ofReal_mul]

private theorem beta_kernel_lower {p q u : ℝ} (hp : 0 < p) (hq : 0 < q)
    (hq₁ : q ≤ 1) (hu : u ∈ Ioo (0 : ℝ) 1) :
    u ^ p * (1 - u) ^ (q - 1) * (-log u) ≤ Beta.normalizer (p, q) := by
  let f := fun z : ℝ ↦ z ^ (p - 1) * (1 - z) ^ (q - 1)
  let c := u ^ p * (1 - u) ^ (q - 1)
  obtain ⟨hf, he⟩ := beta_integral hp hq
  have hsub : IntervalIntegrable f volume u 1 := hf.mono_set
    (by rw [uIcc_of_le hu.2.le, uIcc_of_le zero_le_one]; exact Icc_subset_Icc_left hu.1.le)
  have hi : IntervalIntegrable (fun z : ℝ ↦ c * z⁻¹) volume u 1 := by
    apply ContinuousOn.intervalIntegrable
    intro z hz
    rw [uIcc_of_le hu.2.le] at hz
    exact (continuousAt_const.mul (continuousAt_id.inv₀
      (hu.1.trans_le hz.1).ne')).continuousWithinAt
  have hle := intervalIntegral.integral_mono_on_of_le_Ioo hu.2.le hi hsub
    (fun z hz ↦ show c * z⁻¹ ≤ f z from by
      have hz₀ := hu.1.trans hz.1
      have h₁ := rpow_le_rpow hu.1.le hz.1.le hp.le
      have h₂ := rpow_le_rpow_of_nonpos (sub_pos.mpr hz.2)
        (sub_le_sub_left hz.1.le 1) (sub_nonpos.mpr hq₁)
      have h := mul_le_mul h₁ h₂ (rpow_nonneg (sub_nonneg.mpr hu.2.le) _)
        (rpow_nonneg hz₀.le _)
      apply (mul_le_mul_iff_right₀ hz₀).mp
      change z * (c * z⁻¹) ≤ z * f z
      rw [mul_left_comm z c, mul_inv_cancel₀ hz₀.ne', mul_one]
      have heq : z * f z = z ^ p * (1 - z) ^ (q - 1) := by
        dsimp [f]
        rw [← mul_assoc, mul_comm z, ← rpow_add_one hz₀.ne', sub_add_cancel]
      exact h.trans_eq heq.symm)
  have hm : (∫ z in u..1, f z) ≤ ∫ z in (0 : ℝ)..1, f z := by
    apply intervalIntegral.integral_mono_interval hu.1.le hu.2.le le_rfl _ hf
    refine (ae_restrict_mem measurableSet_Ioc).mono fun z hz ↦ ?_
    exact mul_nonneg (rpow_nonneg hz.1.le _) (rpow_nonneg (sub_nonneg.mpr hz.2) _)
  rw [intervalIntegral.integral_const_mul, integral_inv_of_pos hu.1 one_pos,
    one_div, log_inv] at hle
  exact hle.trans (hm.trans_eq he)

/-- A positive subunit upper shape makes each log-distance density at
most the reciprocal of the log distance. This bound is independent of
both free shape parameters and the power. -/
theorem subunit_endpoint_upper {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {p q a s : ℝ} (hp : 0 < p) (hq : 0 < q) (hq₁ : q ≤ 1)
    (ha : 0 < a) (hs : 0 < s) :
    endpointObjective d p q a s ≤ -(n : ℝ) * log s := by
  unfold endpointObjective
  calc
    _ ≤ ∑ _ : Fin n, -log s := by
      apply Finset.sum_le_sum
      intro i _
      let t := d i + s
      let u := exp (-(a * t))
      have ht : 0 < t := add_pos_of_nonneg_of_pos (hd i) hs
      have hu : u ∈ Ioo (0 : ℝ) 1 := ⟨exp_pos _,
        exp_lt_one_iff.mpr (neg_neg_of_pos (mul_pos ha ht))⟩
      have hK := beta_kernel_lower hp hq hq₁ hu
      have hU : -log u = a * t := by simp [u]
      rw [hU] at hK
      have hlog := log_le_log (mul_pos (mul_pos (rpow_pos_of_pos hu.1 _)
        (rpow_pos_of_pos (sub_pos.mpr hu.2) _)) (mul_pos ha ht)) hK
      rw [log_mul (mul_pos (rpow_pos_of_pos hu.1 _) (rpow_pos_of_pos (sub_pos.mpr hu.2) _)).ne'
        (mul_pos ha ht).ne', log_mul (rpow_pos_of_pos hu.1 _).ne'
        (rpow_pos_of_pos (sub_pos.mpr hu.2) _).ne',
        log_rpow hu.1, log_rpow (sub_pos.mpr hu.2), log_mul ha.ne' ht.ne'] at hlog
      rw [show log u = -(a * t) by simp [u]] at hlog
      have H := log_le_log hs (le_add_of_nonneg_left (hd i))
      change log a - Beta.logBeta (p, q) - p * a * t +
        (q - 1) * log (1 - u) ≤ -log s
      unfold Beta.logBeta
      linarith
    _ = _ := by simp [nsmul_eq_mul, neg_mul]

/-- The subunit branch requires approach to endpoint contact on any
divergent path, and the trace exactly tests all such divergences. -/
theorem subunit_endpoint_divergence_iff {ι : Type*} {l : Filter ι}
    {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i) {p q a s : ι → ℝ}
    (hdom : ∀ᶠ j in l, 0 < p j ∧ 0 < q j ∧ q j < 1 ∧ 0 < a j ∧ 0 < s j) :
    Tendsto (fun j ↦ endpointObjective d (p j) (q j) (a j) (s j)) l atTop ↔
      Tendsto s l (𝓝 0) ∧
      Tendsto (fun j ↦ endpointTrace d (p j) (q j) (a j) (s j)) l atTop := by
  have hD := hdom.mono fun j h ↦ show 0 < q j ∧ q j ≤ 1 ∧ 0 < a j ∧ 0 < s j from
    ⟨h.2.1, h.2.2.1.le, h.2.2.2⟩
  constructor
  · intro hf
    have hs : Tendsto s l (𝓝 0) := by
      apply tendsto_order.mpr
      refine ⟨fun ε hε ↦ hD.mono (fun _ h ↦ hε.trans h.2.2.2), fun ε hε ↦ ?_⟩
      filter_upwards [hdom, hf.eventually_gt_atTop (-(n : ℝ) * log ε)] with j hj hF
      by_contra! h
      have H := subunit_endpoint_upper hd hj.1 hj.2.1 hj.2.2.1.le hj.2.2.2.1 hj.2.2.2.2
      have hh := mul_le_mul_of_nonpos_left (log_le_log hε h)
        (neg_nonpos.mpr (Nat.cast_nonneg n))
      exact hF.not_ge (H.trans hh)
    exact ⟨hs, (endpoint_trace_divergence hd hD hs).mp hf⟩
  · exact fun ⟨hs, ht⟩ ↦ (endpoint_trace_divergence hd hD hs).mpr ht

private theorem spike_ray {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hzero : ∃ i, d i = 0) {p q a : ℝ} (hq : q < 1) (ha : 0 < a) :
    Tendsto (endpointObjective d p q a) (𝓝[>] 0) atTop := by
  let z := fun i ↦ exp (-(a * d i))
  let t := fun s : ℝ ↦ 1 - exp (-(a * s))
  let C := ∑ i, (log a - Beta.logBeta (p, q) - p * a * d i)
  have hz (i : Fin n) : z i ∈ Icc (0 : ℝ) 1 :=
    ⟨(exp_pos _).le, exp_le_one_iff.mpr (neg_nonpos.mpr (mul_nonneg ha.le (hd i)))⟩
  have hcontact : ∃ i, z i = 1 := hzero.imp fun i hi ↦ by simp [z, hi]
  have ht : Tendsto t (𝓝[>] 0) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    have hc : Continuous t := by dsimp [t]; fun_prop
    refine ⟨?_, ?_⟩
    · simpa [t] using (hc.continuousAt (x := 0)).tendsto.mono_left nhdsWithin_le_nhds
    · have hmem : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioi (0 : ℝ) := self_mem_nhdsWithin
      exact hmem.mono (fun s hs ↦ gap_pos (mul_pos ha hs))
  have H := (Endpoint2BetaWidth.spike hz hcontact hq p).comp ht
  apply (tendsto_atTop_add_const_right _ C H).congr
  intro s
  change Endpoint2BetaWidth.logLik z q p (1 - (1 - exp (-(a * s)))) + C =
    endpointObjective d p q a s
  rw [sub_sub_cancel]
  unfold Endpoint2BetaWidth.logLik endpointObjective
  dsimp [C, z]
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  rw [log_exp, ← exp_add, show -(a * s) + -(a * d i) = -(a * (d i + s)) by ring]
  ring

private theorem collapse_ray (hn : 0 < n) (p q : ℝ) :
    Tendsto (fun a : ℝ ↦ endpointObjective (fun _ : Fin n ↦ 0) p q a a⁻¹) atTop atTop := by
  let C := -Beta.logBeta (p, q) - p + (q - 1) * log (1 - exp (-1))
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  apply ((tendsto_atTop_add_const_right _ C tendsto_log_atTop).const_mul_atTop hN).congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
  have he : a * a⁻¹ = 1 := mul_inv_cancel₀ ha.ne'
  simp only [C, endpointObjective, zero_add, he, mul_assoc, mul_one, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

/-- Extended log likelihood in the endpoint chart. At contact the
upper shape determines whether the density is finite, infinite or zero. -/
def endpointExtended (d : Fin n → ℝ) (p q a s : ℝ) : EReal :=
  if 0 < a then
    if 0 < s ∨ (s = 0 ∧ q = 1) then endpointObjective d p q a s
    else if s = 0 ∧ q < 1 then ⊤ else ⊥
  else ⊥

/-- For fixed positive beta shapes, the subunit branch has an unbounded
spike with the joint rate test. The regular branch has compact
superlevels off constant data. On constants it has an unbounded ray,
and every divergence collapses pointwise in the common beta coupling. -/
theorem fixed_beta_shapes_failure (hn : 0 < n) {d : Fin n → ℝ}
    (hd : ∀ i, 0 ≤ d i) (hzero : ∃ i, d i = 0) {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici 0 else Ioi 0)
    (∀ a s, endpointExtended d p q a s = ⊤ ↔ 0 < a ∧ s = 0 ∧ q < 1) ∧
    (q < 1 → (∀ a > 0, Tendsto (endpointObjective d p q a) (𝓝[>] 0) atTop) ∧
      ∀ {l : Filter ℕ} {a s : ℕ → ℝ}, (∀ᶠ j in l, (a j, s j) ∈ D) →
        (Tendsto (fun j ↦ endpointObjective d p q (a j) (s j)) l atTop ↔
          Tendsto s l (𝓝 0) ∧
          Tendsto (fun j ↦ endpointTrace d p q (a j) (s j)) l atTop)) ∧
    (1 ≤ q → ((∃ i, 0 < d i) →
      ∀ L : ℝ, IsCompact {v ∈ D | L ≤ endpointObjective d p q v.1 v.2}) ∧
      ((∀ i, d i = 0) →
        Tendsto (fun a : ℝ ↦ endpointObjective d p q a a⁻¹) atTop atTop ∧
        ∀ {l : Filter ℕ} {a s : ℕ → ℝ}, (∀ᶠ j in l, (a j, s j) ∈ D) →
          Tendsto (fun j ↦ endpointObjective d p q (a j) (s j)) l atTop →
          Tendsto a l atTop ∧ Tendsto s l (𝓝 0) ∧
          ∀ u ∈ Ioo (0 : ℝ) 1, Tendsto (fun j ↦ exp (s j) * u ^ (a j)⁻¹) l (𝓝 1))) := by
  intro D
  refine ⟨?_, ?_, ?_⟩
  · intro a s
    constructor
    · unfold endpointExtended
      split_ifs with ha hs hc
      · simp
      · intro _
        exact ⟨ha, hc⟩
      · simp
      · simp
    · rintro ⟨ha, rfl, hq₁⟩
      simp [endpointExtended, ha, ne_of_lt hq₁, hq₁]
  · intro h
    refine ⟨fun _ ha ↦ spike_ray hd hzero h ha, fun hdom ↦ subunit_endpoint_divergence_iff hd ?_⟩
    exact hdom.mono fun j hj ↦ ⟨hp, hq, h, hj.1, by simpa [D, ne_of_lt h] using hj.2⟩
  · intro h
    refine ⟨fun hpos ↦ fixed_beta_shapes_compact_superlevels hd hzero hpos hp h, ?_⟩
    intro hconst
    have he : d = fun _ ↦ 0 := funext hconst
    rw [he]
    exact ⟨collapse_ray hn p q, fun hdom hdiv ↦ fixed_beta_shapes_constant_collapse hn hp h
      (hdom.mono fun _ hj ↦ ⟨hj.1, hj.2⟩) hdiv⟩

end

end UnivariateML.GBEndpointTrace

end
