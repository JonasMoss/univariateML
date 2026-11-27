module

public import UnivariateML.Family.Endpoint3GeneralizedBeta

import Mathlib.Tactic

set_option linter.style.header false

/-! # Generalized beta with upper shape and power held -/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint3GeneralizedBetaHeldPower

open Endpoint2GeneralizedBeta

variable {n : ℕ}

private theorem logBeta_formula {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    Beta.logBeta (p, q) =
      log (Gamma p) + log (Gamma q) - log (Gamma (p + q)) := by
  unfold Beta.logBeta Beta.normalizer
  dsimp only [Prod.fst, Prod.snd]
  rw [log_div (mul_pos (Gamma_pos_of_pos hp) (Gamma_pos_of_pos hq)).ne'
      (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']

private theorem logBeta_continuousAt {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ContinuousAt (fun y ↦ Beta.logBeta (y, q)) p := by
  have hG {z : ℝ} (hz : 0 < z) : ContinuousAt (fun y ↦ log (Gamma y)) z :=
    ((Real.differentiableAt_Gamma (fun m ↦ by
      have hm : 0 ≤ (m : ℝ) := Nat.cast_nonneg m
      linarith)).continuousAt).log (Gamma_pos_of_pos hz).ne'
  have hh : ContinuousAt (fun y : ℝ ↦ y + q) p :=
    continuousAt_id.add continuousAt_const
  have hc := ((hG hp).add (continuousAt_const (y := log (Gamma q)))).sub
    ((hG (add_pos hp hq)).comp' (f := fun y : ℝ ↦ y + q) (x := p) hh)
  apply ContinuousAt.congr_of_eventuallyEq hc
  filter_upwards [Ioi_mem_nhds hp] with y hy
  exact logBeta_formula hy hq

private theorem normalizer_tails {q A : ℝ} (hq : 0 < q) (hA : 0 < A) :
    let F := fun p : ℝ ↦ -Beta.logBeta (p, q) - A * p
    Tendsto F (𝓝[>] 0) atBot ∧ Tendsto F atTop atBot := by
  intro F
  constructor
  · have h0 : Tendsto (fun p : ℝ ↦ p) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
    apply (tendsto_log_nhdsGT_zero.atBot_add
      ((Endpoint3GammaRatio.log_gamma_ratio_at_zero hq).sub (h0.const_mul A))).congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with p hp
    change 0 < p at hp
    dsimp [F]
    rw [logBeta_formula hp hq, add_comm p q]
    ring
  · have hlog : Tendsto (fun p : ℝ ↦ log p / p) atTop (𝓝 0) := by
      simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
    have hsmall : ∀ᶠ p in atTop, q * log p ≤ (A / 2) * p := by
      filter_upwards [((hlog.const_mul q).eventually
        (Iio_mem_nhds (show q * 0 < A / 2 by simpa using half_pos hA))),
        eventually_gt_atTop (0 : ℝ)] with p hp hp₀
      change q * (log p / p) < A / 2 at hp
      rw [← mul_div_assoc] at hp
      exact ((div_lt_iff₀ hp₀).mp hp).le
    have hbound : ∀ᶠ p in atTop, F p ≤ -(A / 2) * p + (1 - log (Gamma q)) := by
      filter_upwards [hsmall, eventually_gt_atTop (0 : ℝ),
        (Endpoint3GammaRatio.log_gamma_ratio q hq).eventually
          (Iio_mem_nhds (by norm_num : (0 : ℝ) < 1))] with p hp hp₀ hb
      change log (Gamma (p + q)) - log (Gamma p) - q * log p < 1 at hb
      dsimp [F]
      rw [logBeta_formula hp₀ hq]
      linarith
    exact tendsto_atBot_mono' _ hbound
      (tendsto_atBot_add_const_right _ _ (tendsto_id.const_mul_atTop_of_neg
        (neg_neg_of_pos (half_pos hA))))

private theorem unitData_exp {x b c : ℝ} (hx : 0 < x) (hb : 0 < b) :
    (x / b) ^ c = exp (-(c * log (b / x))) := by
  rw [rpow_def_of_pos (div_pos hx hb), log_div hx.ne' hb.ne', log_div hb.ne' hx.ne']
  congr 1
  ring

private theorem logLik_continuousAt {x : Fin n → ℝ} {M q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (hc : 0 < c) (hq : 0 < q)
    {v : ℝ × ℝ} (hp : 0 < v.1) (hb : 0 < v.2) (h : q = 1 ∨ M < v.2) :
    ContinuousAt (fun w : ℝ × ℝ ↦ logLik x w.2 w.1 q c) v := by
  have hβ := (logBeta_continuousAt hp hq).comp'
    (f := Prod.fst) (x := v) continuous_fst.continuousAt
  unfold logLik
  apply tendsto_finsetSum Finset.univ
  intro i _
  have hr : ContinuousAt (fun w : ℝ × ℝ ↦ x i / w.2) v :=
    continuousAt_const.div continuous_snd.continuousAt hb.ne'
  have hu : ContinuousAt (fun w : ℝ × ℝ ↦ unitData x w.2 c i) v :=
    (continuousAt_rpow_const _ c (Or.inr hc.le)).comp' hr
  have hup := rpow_pos_of_pos (div_pos (hx i) hb) c
  have hbody := continuous_fst.continuousAt.mul (hu.log hup.ne')
  have htail : ContinuousAt (fun w : ℝ × ℝ ↦ (q - 1) * log (1 - unitData x w.2 c i)) v := by
    rcases h with he | hv
    · simp [he, continuousAt_const]
    · have ht : unitData x v.2 c i < 1 := by
        simpa only [unitData, one_rpow] using rpow_lt_rpow (div_nonneg (hx i).le hb.le)
          ((div_lt_one hb).mpr ((hle i).trans_lt hv)) hc
      exact continuousAt_const.mul ((continuousAt_const.sub hu).log (sub_pos.mpr ht).ne')
  exact (((continuousAt_const.sub hβ).add hbody).add htail).tendsto

/-- With held regular upper shape and held power, every finite
superlevel is compact in first-shape and physical endpoint coordinates
on nonconstant positive data. Unit upper shape includes closed contact. -/
theorem regular_superlevel_compact (hn : 0 < n) {x : Fin n → ℝ} {M q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hne : ∃ i, x i < M) (hq : 1 ≤ q) (hc : 0 < c) (C : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q c
    IsCompact {v ∈ D | C ≤ L v} := by
  intro D L
  obtain ⟨j, hj⟩ := heq
  obtain ⟨k, hk⟩ := hne
  let N : ℝ := n
  let d := fun i ↦ log (M / x i)
  let A := c * (∑ i, d i) / N
  let F := fun p : ℝ ↦ -Beta.logBeta (p, q) - A * p
  let J := N * log c - ∑ i, log (x i)
  let U := fun p ↦ N * F p + J
  let g := fun b : ℝ ↦ log (b / M)
  let R := fun z : ℝ ↦ log (1 - exp (-(c * z)))
  have hM : 0 < M := hj ▸ hx j
  have hN : 0 < N := Nat.cast_pos.mpr hn
  have hq₀ : 0 < q := zero_lt_one.trans_le hq
  have hd (i : Fin n) : 0 ≤ d i := log_nonneg ((one_le_div (hx i)).mpr (hle i))
  have hdj : d j = 0 := by simp [d, hj, hM.ne']
  have hA : 0 < A := div_pos (mul_pos hc (Finset.sum_pos' (fun i _ ↦ hd i)
    ⟨k, Finset.mem_univ _, log_pos ((one_lt_div (hx k)).mpr hk)⟩)) hN
  have hb (v : ℝ × ℝ) (hv : v ∈ D) : M ≤ v.2 := by
    have h := hv.2
    by_cases he : q = 1
    · simpa [he] using h
    · exact (show M < v.2 by simpa [he] using h).le
  have hstrict (v : ℝ × ℝ) (hv : v ∈ D) (he : q ≠ 1) : M < v.2 := by
    simpa [he] using hv.2
  have hg (b : ℝ) (hb : M ≤ b) : 0 ≤ g b := log_nonneg ((one_le_div hM).mpr hb)
  have hform (v : ℝ × ℝ) (hv : v ∈ D) :
      L v = U v.1 - N * v.1 * c * g v.2 + (q - 1) * ∑ i, R (g v.2 + d i) := by
    have hv₀ : 0 < v.2 := hM.trans_le (hb v hv)
    have hlog (i : Fin n) : log (v.2 / x i) = g v.2 + d i := by
      simp only [g, d, log_div hv₀.ne' (hx i).ne', log_div hv₀.ne' hM.ne',
        log_div hM.ne' (hx i).ne']
      ring
    have hunit (i : Fin n) :
        unitData x v.2 c i = exp (-(c * (g v.2 + d i))) := by
      rw [unitData, unitData_exp (hx i) hv₀, hlog]
    have he : A * N = c * ∑ i, d i := div_mul_cancel₀ _ hN.ne'
    simp only [L, logLik, hunit, log_exp, R, U, F, J, Finset.sum_add_distrib,
      Finset.sum_sub_distrib, Finset.sum_const, Fintype.card_fin, Finset.card_univ,
      nsmul_eq_mul, Finset.sum_neg_distrib, ← Finset.mul_sum]
    dsimp [N] at he ⊢
    linear_combination v.1 * he
  have hR (z : ℝ) (hz : 0 ≤ z) : R z ≤ 0 := by
    have h : 0 < exp (-(c * z)) := exp_pos _
    exact log_nonpos (sub_nonneg.mpr (exp_le_one_iff.mpr (neg_nonpos.mpr (mul_nonneg hc.le hz))))
      (sub_le_self _ h.le)
  have hL (v : ℝ × ℝ) (hv : v ∈ D) :
      L v ≤ U v.1 - N * v.1 * c * g v.2 := by
    rw [hform v hv]
    exact add_le_of_nonpos_right (mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq)
      (Finset.sum_nonpos fun i _ ↦ hR _ (add_nonneg (hg _ (hb v hv)) (hd i))))
  have hLU (v : ℝ × ℝ) (hv : v ∈ D) : L v ≤ U v.1 :=
    (hL v hv).trans (sub_le_self _ (mul_nonneg
      (mul_nonneg (mul_nonneg hN.le (show 0 < v.1 from hv.1).le) hc.le) (hg _ (hb v hv))))
  obtain ⟨ε, hε, hs⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp
    ((normalizer_tails hq₀ hA).1.eventually_lt_atBot ((C - J) / N))
  obtain ⟨P, hP⟩ := eventually_atTop.mp
    ((normalizer_tails hq₀ hA).2.eventually_lt_atBot ((C - J) / N))
  let I := Icc ε (max ε P)
  have hUF : ContinuousOn U I := by
    intro p hp
    have hp₀ := hε.trans_le hp.1
    apply ContinuousAt.continuousWithinAt
    exact (continuousAt_const.mul ((logBeta_continuousAt hp₀ hq₀).neg.sub
      (continuousAt_const.mul continuousAt_id))).add continuousAt_const
  obtain ⟨pstar, _, hstar⟩ := isCompact_Icc.exists_isMaxOn ⟨ε, le_rfl, le_max_left _ _⟩ hUF
  let B := U pstar
  let hi := M * exp ((B - C) / (N * ε * c))
  let lo := if q = 1 then M else M * exp (exp ((C - B) / (q - 1)) / c)
  let K : Set (ℝ × ℝ) := I ×ˢ Icc lo hi
  have hD : K ⊆ D := by
    intro v hv
    refine ⟨hε.trans_le hv.1.1, ?_⟩
    by_cases he : q = 1
    · simpa [lo, he] using hv.2.1
    · have hlt : M < lo := by
        simp only [lo, he, ite_false]
        have h := one_lt_exp_iff.mpr (div_pos (exp_pos ((C - B) / (q - 1))) hc)
        linarith [mul_lt_mul_of_pos_left h hM]
      simpa [he] using hlt.trans_le hv.2.1
  have hkeep (v : ℝ × ℝ) (hv : v ∈ D) (h : C ≤ L v) : v ∈ K := by
    have hf : (C - J) / N ≤ F v.1 := (div_le_iff₀ hN).mpr (by
      have hU := h.trans (hLU v hv)
      dsimp [U] at hU
      linarith)
    have hvI : v.1 ∈ I := ⟨le_of_not_gt (fun ht ↦ (hs ⟨hv.1, ht⟩).not_ge hf),
      le_of_not_gt (fun ht ↦ (hP _ ((le_max_right _ _).trans ht.le)).not_ge hf)⟩
    have hUx : U v.1 ≤ B := hstar hvI
    refine ⟨hvI, ?_, ?_⟩
    · by_cases he : q = 1
      · simpa [lo, he] using hb v hv
      · have hq₁ : 0 < q - 1 := sub_pos.mpr (lt_of_le_of_ne hq (Ne.symm he))
        have hg₀ : 0 < g v.2 := log_pos ((one_lt_div hM).mpr (hstrict v hv he))
        have hsum : (∑ i, R (g v.2 + d i)) ≤ R (g v.2) := by
          rw [← Finset.sum_erase_add _ _ (Finset.mem_univ j), hdj, add_zero]
          exact add_le_of_nonpos_left (Finset.sum_nonpos fun i _ ↦
            hR _ (add_nonneg hg₀.le (hd i)))
        have ht : R (g v.2) ≤ log (c * g v.2) := by
          have hpos : 0 < c * g v.2 := mul_pos hc hg₀
          apply log_le_log (sub_pos.mpr (exp_lt_one_iff.mpr (neg_lt_zero.mpr hpos)))
          linarith [add_one_le_exp (-(c * g v.2))]
        have hl : (C - B) / (q - 1) ≤ log (c * g v.2) := by
          apply (div_le_iff₀ hq₁).mpr
          have hz := mul_le_mul_of_nonneg_left (hsum.trans ht) hq₁.le
          rw [hform v hv] at h
          linarith [mul_nonneg (mul_nonneg (mul_nonneg hN.le
            (show 0 < v.1 from hv.1).le) hc.le) hg₀.le]
        have h₁ : exp ((C - B) / (q - 1)) / c ≤ g v.2 := by
          apply (div_le_iff₀ hc).mpr
          have hh := exp_le_exp.mpr hl
          rw [exp_log (mul_pos hc hg₀)] at hh
          simpa only [mul_comm c] using hh
        have h₂ : exp (exp ((C - B) / (q - 1)) / c) ≤ v.2 / M := by
          have hv₀ : 0 < v.2 := hM.trans_le (hb v hv)
          simpa only [g, exp_log (div_pos hv₀ hM)] using exp_le_exp.mpr h₁
        simpa only [lo, he, ite_false, mul_comm M] using (le_div_iff₀ hM).mp h₂
    · have hcoef := mul_le_mul_of_nonneg_right
        (mul_le_mul_of_nonneg_right (mul_le_mul_of_nonneg_left hvI.1 hN.le) hc.le)
        (hg _ (hb v hv))
      have hlog : g v.2 ≤ (B - C) / (N * ε * c) := by
        apply (le_div_iff₀ (mul_pos (mul_pos hN hε) hc)).mpr
        linarith [h.trans (hL v hv)]
      have hv₀ : 0 < v.2 := hM.trans_le (hb v hv)
      have ht : v.2 / M ≤ exp ((B - C) / (N * ε * c)) := by
        simpa only [g, exp_log (div_pos hv₀ hM)] using exp_le_exp.mpr hlog
      simpa only [hi, mul_comm M] using (div_le_iff₀ hM).mp ht
  have hcont : ContinuousOn L K := by
    intro v hv
    exact (logLik_continuousAt hx hle hc hq₀ (hD hv).1 (hM.trans_le (hb v (hD hv)))
      (by by_cases he : q = 1
          · exact Or.inl he
          · exact Or.inr (hstrict v (hD hv) he))).continuousWithinAt
  have hsame : {v ∈ D | C ≤ L v} = K ∩ L ⁻¹' Ici C := by
    ext v
    exact ⟨fun h ↦ ⟨hkeep v h.1 h.2, h.2⟩, fun h ↦ ⟨hD h.1, h.2⟩⟩
  rw [hsame]
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  let _ : CompactSpace K := isCompact_iff_compactSpace.mp hK
  have hclosed : IsClosed {v : K | C ≤ L v} :=
    isClosed_Ici.preimage hcont.domRestrict
  have hImage := hclosed.isCompact.image continuous_subtype_val
  convert hImage using 1
  ext v
  constructor
  · rintro ⟨hv, hC⟩
    exact ⟨⟨v, hv⟩, hC, rfl⟩
  · rintro ⟨u, hu, rfl⟩
    exact ⟨u.property, hu⟩

/-- On constant positive samples, a held upper shape and power allow
concentration as the first shape increases and the endpoint approaches
the common value from above. -/
theorem constant_first_shape_path (hn : 0 < n) {v q c : ℝ}
    (hv : 0 < v) (hq : 0 < q) (hc : 0 < c) :
    Tendsto (fun p ↦ logLik (fun _ : Fin n ↦ v) (v / (1 - p⁻¹) ^ c⁻¹) p q c)
      atTop atTop := by
  let A := log c - log v - log (Gamma q) - 3
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hbase : Tendsto (fun p : ℝ ↦ (n : ℝ) * (log p + A)) atTop atTop :=
    (tendsto_atTop_add_const_right _ A tendsto_log_atTop).const_mul_atTop hN
  apply tendsto_atTop_mono' _ ?_ hbase
  filter_upwards [eventually_gt_atTop (2 : ℝ),
    (Endpoint3GammaRatio.log_gamma_ratio q hq).eventually
      (Ioi_mem_nhds (show (-1 : ℝ) < 0 by norm_num))] with p hp hr
  have hp₀ : 0 < p := by linarith
  have hpi : 0 < p⁻¹ := inv_pos.mpr hp₀
  have hpi' : p⁻¹ < 1 / 2 := by
    have h := (inv_lt_inv₀ hp₀ (show (0 : ℝ) < 2 by norm_num)).mpr hp
    simpa only [one_div] using h
  have ht : 0 < 1 - p⁻¹ := by linarith
  have hunit (i : Fin n) : unitData (fun _ ↦ v) (v / (1 - p⁻¹) ^ c⁻¹) c i = 1 - p⁻¹ := by
    unfold unitData
    rw [div_div_cancel₀ hv.ne', ← rpow_mul ht.le, inv_mul_cancel₀ hc.ne', rpow_one]
  have hlog : -log (1 - p⁻¹) ≤ p⁻¹ / (1 - p⁻¹) := by
    have h := log_le_sub_one_of_pos (inv_pos.mpr ht)
    rw [log_inv] at h
    have he : (1 - p⁻¹)⁻¹ - 1 = p⁻¹ / (1 - p⁻¹) := by
      apply (eq_div_iff ht.ne').mpr
      rw [sub_mul, inv_mul_cancel₀ ht.ne', one_mul]
      ring
    exact he ▸ h
  have hpenalty : -(p * log (1 - p⁻¹)) ≤ 2 := by
    calc
      _ ≤ p * (p⁻¹ / (1 - p⁻¹)) := by
        simpa only [mul_neg] using (mul_le_mul_of_nonneg_left hlog hp₀.le)
      _ = 1 / (1 - p⁻¹) := by rw [← mul_div_assoc, mul_inv_cancel₀ hp₀.ne']
      _ ≤ 2 := (div_le_iff₀ ht).mpr (by linarith)
  simp only [logLik, hunit, sub_sub_cancel, log_inv, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  apply mul_le_mul_of_nonneg_left _ hN.le
  change -1 < log (Gamma (p + q)) - log (Gamma p) - q * log p at hr
  rw [logBeta_formula hp₀ hq]
  dsimp [A]
  linear_combination (norm := ring_nf) hpenalty + le_of_lt hr
  exact le_rfl

set_option maxHeartbeats 600000 in
-- The joined statement elaborates the spike, concentration and compact branches together.
/-- With fixed upper shape and power, a finite positive maximum exists
exactly for nonconstant positive data and upper shape at least one.
The regular nonconstant branch has compact finite superlevels. -/
theorem criterion {x : Fin n → ℝ} {M q c : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hq : 0 < q) (hc : 0 < c) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ logLik x v.2 v.1 q c
    ((∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ q ∧ ∃ i, x i < M) ∧
      ((q < 1 ∨ ∀ i, x i = M) → ¬ BddAbove (L '' D)) ∧
      ((1 ≤ q ∧ ∃ i, x i < M) → ∀ C, IsCompact {v ∈ D | C ≤ L v}) := by
  intro D L
  obtain ⟨j, hj⟩ := heq
  have hz : ∃ i, x i = M := ⟨j, hj⟩
  have hn : 0 < n := j.isLt.pos
  have hM : 0 < M := hj ▸ hx j
  have hdom {p b : ℝ} (hp : 0 < p) (hb : M < b) : (p, b) ∈ D := by
    refine ⟨hp, ?_⟩
    by_cases h : q = 1 <;> simp [h, hb, hb.le]
  have hray {u : ℝ} (hu : 0 < u) (hu' : u < 1) : M < M / u ^ c⁻¹ := by
    have he : u ^ c⁻¹ < 1 := by
      simpa only [one_rpow] using rpow_lt_rpow hu.le hu' (inv_pos.mpr hc)
    apply (lt_div_iff₀ (rpow_pos_of_pos hu _)).mpr
    simpa only [mul_one] using mul_lt_mul_of_pos_left he hM
  have hu (h : q < 1 ∨ ∀ i, x i = M) : ¬ BddAbove (L '' D) := by
    rintro ⟨B, hB⟩
    rcases h with h | h
    · have ht : ∀ᶠ t : ℝ in 𝓝[>] 0, t ∈ Ioo 0 1 :=
        (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
          (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
      have hlim := endpoint_spike (p := 1) hx hle hz h hc
      obtain ⟨t, hgt, ht₀, ht₁⟩ := ((hlim.eventually_gt_atTop B).and ht).exists
      have hb := hray (sub_pos.mpr ht₁) (sub_lt_self _ ht₀)
      have hh : L (1, M / (1 - t) ^ c⁻¹) ≤ B :=
        hB ⟨(1, M / (1 - t) ^ c⁻¹), hdom one_pos hb, rfl⟩
      exact hgt.not_ge hh
    · have he : x = fun _ ↦ M := funext h
      have hlim := constant_first_shape_path hn hM hq hc
      obtain ⟨p, hgt, hp⟩ := ((hlim.eventually_gt_atTop B).and
        (eventually_gt_atTop (1 : ℝ))).exists
      have hp₀ : 0 < p := zero_lt_one.trans hp
      have hb := hray (sub_pos.mpr ((inv_lt_one₀ hp₀).mpr hp))
        (sub_lt_self _ (inv_pos.mpr hp₀))
      have hle := hB (mem_image_of_mem L (hdom hp₀ hb))
      exact hgt.not_ge (by simpa only [L, he] using hle)
  have hcomp (h : 1 ≤ q ∧ ∃ i, x i < M) (C : ℝ) : IsCompact {v ∈ D | C ≤ L v} :=
    regular_superlevel_compact hn hx hle hz h.2 h.1 hc C
  refine ⟨⟨?_, ?_⟩, hu, hcomp⟩
  · rintro ⟨v, _, hm⟩
    by_contra! h
    have hbad : q < 1 ∨ ∀ i, x i = M := by
      by_cases hr : 1 ≤ q
      · exact Or.inr fun i ↦ le_antisymm (hle i) (h hr i)
      · exact Or.inl (lt_of_not_ge hr)
    exact hu hbad hm.bddAbove
  · intro h
    let w : ℝ × ℝ := (1, M + 1)
    have hw : w ∈ D := hdom one_pos (lt_add_of_pos_right M one_pos)
    let C := L w
    let K := {v ∈ D | C ≤ L v}
    have hcK : IsCompact K := hcomp h C
    have hcont : ContinuousOn L K := by
      intro v hv
      have hb : M ≤ v.2 := by
        by_cases he : q = 1
        · simpa [he] using hv.1.2
        · exact (show M < v.2 by simpa [he] using hv.1.2).le
      exact (logLik_continuousAt hx hle hc hq hv.1.1 (hM.trans_le hb)
        (by by_cases he : q = 1
            · exact Or.inl he
            · exact Or.inr (by simpa [he] using hv.1.2))).continuousWithinAt
    have hwK : w ∈ K := ⟨hw, le_rfl⟩
    obtain ⟨v, hv, hm⟩ := hcK.exists_isMaxOn ⟨w, hwK⟩ hcont
    refine ⟨v, hv.1, fun u huD ↦ ?_⟩
    by_cases huC : C ≤ L u
    · exact hm ⟨huD, huC⟩
    · exact (lt_of_not_ge huC).le.trans (hm hwK)

end UnivariateML.Endpoint3GeneralizedBetaHeldPower

end
