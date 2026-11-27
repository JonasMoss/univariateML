module

public import UnivariateML.Family.HeldSweepGEV
public import Mathlib.Topology.Semicontinuity.Basic
import Mathlib.Analysis.Calculus.DSlope
import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.JoinsGEV

noncomputable section

private def arg (p : ℝ × ℝ) : ℝ := 1 + p.1 * p.2

private def rate (p : ℝ × ℝ) : ℝ :=
  exp (-p.2 * dslope log 1 (arg p))

private def kernel (xi y : ℝ) : ℝ := y ^ (1 + xi) * exp (-y)

private def stdDensity (p : ℝ × ℝ) : ℝ :=
  if 0 < arg p then kernel p.1 (rate p)
  else if p.1 = -1 ∧ arg p = 0 then 1 else 0

private theorem rate_zero (u : ℝ) : rate (0, u) = exp (-u) := by
  simp [rate, arg, dslope_same, Real.deriv_log]

private theorem rate_eq_rpow {p : ℝ × ℝ} (ha : 0 < arg p) (hxi : p.1 ≠ 0) :
    rate p = arg p ^ (-1 / p.1) := by
  by_cases hu : p.2 = 0
  · simp [rate, arg, hu]
  have hne : arg p ≠ 1 := by
    dsimp [arg]
    intro he
    exact (mul_ne_zero hxi hu) (by linarith)
  rw [rate, dslope_of_ne _ hne, slope_def_field, log_one, sub_zero, rpow_def_of_pos ha]
  congr 1
  have hh : arg p - 1 = p.1 * p.2 := by unfold arg; ring
  rw [hh]
  field_simp

private theorem rate_continuousAt {p : ℝ × ℝ} (hp : 0 < arg p) :
    ContinuousAt rate p := by
  have ha : Continuous arg := by unfold arg; fun_prop
  have hd : ContinuousAt (dslope log 1) (arg p) := by
    by_cases h : arg p = 1
    · rw [h]
      exact continuousAt_dslope_same.mpr (differentiableAt_log one_ne_zero)
    · exact (continuousAt_dslope_of_ne h).mpr (continuousAt_log hp.ne')
  exact continuous_exp.continuousAt.comp
    (continuous_snd.continuousAt.neg.mul (hd.comp ha.continuousAt))

private theorem kernel_continuousAt {p : ℝ × ℝ} (hp : 0 < p.2) :
    ContinuousAt (fun p : ℝ × ℝ ↦ kernel p.1 p.2) p := by
  unfold kernel
  have hc : Continuous (fun p : ℝ × ℝ ↦ (p.2, 1 + p.1)) := by fun_prop
  have he : Continuous (fun p : ℝ × ℝ ↦ exp (-p.2)) := by fun_prop
  exact (ContinuousAt.comp' (f := fun p : ℝ × ℝ ↦ (p.2, 1 + p.1))
    (continuousAt_rpow_of_ne (p.2, 1 + p.1) hp.ne') hc.continuousAt).mul
    he.continuousAt

private theorem density_nonneg (p : ℝ × ℝ) : 0 ≤ stdDensity p := by
  unfold stdDensity kernel rate
  split_ifs <;> positivity

private theorem kernel_le_small {xi y : ℝ} (hx : -1 ≤ xi) (hy : 0 < y) (hy1 : y ≤ 1) :
    kernel xi y ≤ 1 := by
  have hp : y ^ (1 + xi) ≤ 1 := by
    simpa using rpow_le_rpow_of_exponent_ge hy hy1 (show 0 ≤ 1 + xi by linarith)
  exact (mul_le_of_le_one_right (rpow_nonneg hy.le _)
    (exp_le_one_iff.mpr (neg_nonpos.mpr hy.le))).trans hp

private theorem kernel_le_large {xi y : ℝ} (hx : xi ≤ 1) (hy : 1 ≤ y) :
    kernel xi y ≤ y^2 * exp (-y) := by
  unfold kernel
  apply mul_le_mul_of_nonneg_right _ (exp_pos _).le
  rw [← rpow_natCast]
  exact rpow_le_rpow_of_exponent_le hy (by push_cast; linarith)

private theorem kernel_le_half {xi y : ℝ} (hx : -(1 / 2 : ℝ) ≤ xi)
    (hy : 0 < y) (hy1 : y ≤ 1) : kernel xi y ≤ y ^ (1/2 : ℝ) := by
  unfold kernel
  have hp := rpow_le_rpow_of_exponent_ge hy hy1 (show (1/2 : ℝ) ≤ 1 + xi by linarith)
  exact (mul_le_mul_of_nonneg_right hp (exp_pos _).le).trans
    (mul_le_of_le_one_right (rpow_nonneg hy.le _) (exp_le_one_iff.mpr (neg_nonpos.mpr hy.le)))

private theorem rate_arg_eq {p : ℝ × ℝ} (ha : 0 < arg p) (hxi : p.1 ≠ 0) :
    (rate p) ^ (-p.1) = arg p := by
  rw [rate_eq_rpow ha hxi, ← rpow_mul ha.le, show (-1 / p.1) * -p.1 = 1 by field_simp, rpow_one]

private theorem density_upperSemicontinuous :
    UpperSemicontinuousOn stdDensity {p : ℝ × ℝ | p.1 ∈ Icc (-1 : ℝ) 1} := by
  let D := {p : ℝ × ℝ | p.1 ∈ Icc (-1 : ℝ) 1}
  have harg : Continuous arg := by unfold arg; fun_prop
  intro p hp
  rcases lt_trichotomy (arg p) 0 with hneg | hzero | hpos
  · have hn := harg.continuousAt.eventually (Iio_mem_nhds hneg)
    have he : stdDensity =ᶠ[𝓝 p] fun _ ↦ 0 := hn.mono fun q h ↦ by
      simp [stdDensity, h.not_gt, h.ne]
    exact (continuousAt_const.congr he.symm).upperSemicontinuousAt.upperSemicontinuousWithinAt D
  · intro B hB
    have hxi : p.1 ≠ 0 := by intro h; simp [arg, h] at hzero
    by_cases hclose : p.1 = -1
    · have hB1 : 1 < B := by simpa [stdDensity, hzero, hclose] using hB
      have hz : p.1 < 0 := by rw [hclose]; norm_num
      have harg1 : arg p < 1 := by rw [hzero]; norm_num
      have hshape := continuous_fst.continuousAt.eventually (Iio_mem_nhds hz)
      have hbound := harg.continuousAt.eventually (Iio_mem_nhds harg1)
      have hnear : ∀ᶠ q in 𝓝[D] p, q.1 < 0 ∧ arg q < 1 :=
        (hshape.and hbound).filter_mono nhdsWithin_le_nhds
      filter_upwards [hnear, (self_mem_nhdsWithin : ∀ᶠ q in 𝓝[D] p, q ∈ D)]
        with q hq hD
      by_cases ha : 0 < arg q
      · rw [stdDensity, ite_eq_left ha]
        have hr1 : rate q ≤ 1 := by
          rw [rate_eq_rpow ha hq.1.ne]
          exact rpow_le_one ha.le hq.2.le ((div_pos_of_neg_of_neg (by norm_num) hq.1).le)
        exact (kernel_le_small hD.1 (exp_pos _) hr1).trans_lt hB1
      · unfold stdDensity
        rw [ite_eq_right ha]
        split_ifs <;> linarith
    · have hB0 : 0 < B := by simpa [stdDensity, hzero, hclose] using hB
      rcases lt_or_gt_of_ne hxi with hn | hpos
      · have he : 0 < 1 + p.1 := by
          have h := lt_of_le_of_ne hp.1 (Ne.symm hclose)
          linarith
        have hexp : 0 < -1 / p.1 := div_pos_of_neg_of_neg (by norm_num) hn
        have hbase : ContinuousAt (fun q : ℝ × ℝ ↦ (arg q, -1 / q.1)) p :=
          harg.continuousAt.prodMk (continuousAt_const.div continuous_fst.continuousAt hxi)
        have hrate : ContinuousAt (fun q : ℝ × ℝ ↦ arg q ^ (-1 / q.1)) p :=
          ContinuousAt.comp' (f := fun q : ℝ × ℝ ↦ (arg q, -1 / q.1))
            (continuousAt_rpow_of_pos _ hexp) hbase
        have hzero' : arg p ^ (-1 / p.1) = 0 := by rw [hzero, zero_rpow hexp.ne']
        have hcontinu : ContinuousAt
            (fun q : ℝ × ℝ ↦ kernel q.1 (arg q ^ (-1 / q.1))) p := by
          have hf : ContinuousAt (fun q : ℝ × ℝ ↦ (arg q ^ (-1 / q.1), 1 + q.1)) p :=
            hrate.prodMk (by fun_prop)
          exact (ContinuousAt.comp' (f := fun q : ℝ × ℝ ↦ (arg q ^ (-1 / q.1), 1 + q.1))
            (continuousAt_rpow_of_pos _ he) hf).mul (continuous_exp.continuousAt.comp hrate.neg)
        have hnear := hcontinu.eventually (Iio_mem_nhds (show
            kernel p.1 (arg p ^ (-1 / p.1)) < B by
          rw [hzero', kernel, zero_rpow he.ne']
          simpa using hB0))
        have hsh := continuous_fst.continuousAt.eventually (Iio_mem_nhds hn)
        have hpc := continuous_fst.continuousAt.eventually
          (Ioi_mem_nhds (show -1 < p.1 from lt_of_le_of_ne hp.1 (Ne.symm hclose)))
        filter_upwards [((hnear.and hsh).and hpc).filter_mono nhdsWithin_le_nhds] with q hq
        by_cases ha : 0 < arg q
        · simpa only [stdDensity, ite_eq_left ha, rate_eq_rpow ha hq.1.2.ne] using hq.1.1
        · have hcl : ¬ (q.1 = -1 ∧ arg q = 0) := fun h ↦ hq.2.ne' h.1
          simpa only [stdDensity, ite_eq_right ha, ite_eq_right hcl] using hB0
      · have hexp : 0 < 1 / p.1 := one_div_pos.mpr hpos
        let g := fun q : ℝ × ℝ ↦ arg q ^ (1 / q.1)
        have hbase : ContinuousAt (fun q : ℝ × ℝ ↦ (arg q, 1 / q.1)) p :=
          harg.continuousAt.prodMk (continuousAt_const.div continuous_fst.continuousAt hxi)
        have hg : ContinuousAt g p :=
          ContinuousAt.comp' (f := fun q : ℝ × ℝ ↦ (arg q, 1 / q.1))
            (continuousAt_rpow_of_pos _ hexp) hbase
        have hg0 : g p = 0 := by simp only [g, hzero, zero_rpow hexp.ne']
        obtain ⟨R, hR⟩ := eventually_atTop.1
          ((tendsto_pow_mul_exp_neg_atTop_nhds_zero 2).eventually (Iio_mem_nhds hB0))
        let M := max R 1
        have hM : 0 < M := zero_lt_one.trans_le (le_max_right _ _)
        have hnear := hg.eventually (Iio_mem_nhds (show g p < M⁻¹ by rw [hg0]; positivity))
        have hsh := continuous_fst.continuousAt.eventually (Ioi_mem_nhds hpos)
        filter_upwards [(hnear.and hsh).filter_mono nhdsWithin_le_nhds,
          (self_mem_nhdsWithin : ∀ᶠ q in 𝓝[D] p, q ∈ D)] with q hq hD
        by_cases ha : 0 < arg q
        · rw [stdDensity, ite_eq_left ha]
          have hgpos : 0 < g q := rpow_pos_of_pos ha _
          have hr : rate q = (g q)⁻¹ := by
            rw [rate_eq_rpow ha hq.2.ne', show -1 / q.1 = -(1 / q.1) by ring, rpow_neg ha.le]
          have hlarge : M ≤ rate q := by
            rw [hr, ← inv_inv M]
            exact (inv_lt_inv₀ (inv_pos.mpr hM) hgpos).mpr hq.1 |>.le
          exact (kernel_le_large hD.2 ((le_max_right _ _).trans hlarge)).trans_lt
            (hR _ ((le_max_left _ _).trans hlarge))
        · have hcl : ¬ (q.1 = -1 ∧ arg q = 0) := fun h ↦ by linarith [hq.2, h.1]
          simpa only [stdDensity, ite_eq_right ha, ite_eq_right hcl] using hB0
  · have hc := kernel_continuousAt (p := (p.1, rate p)) (exp_pos _)
    have hc' : ContinuousAt (fun q ↦ kernel q.1 (rate q)) p :=
      ContinuousAt.comp' (f := fun q ↦ (q.1, rate q)) hc
        (continuous_fst.continuousAt.prodMk (rate_continuousAt hpos))
    have he : stdDensity =ᶠ[𝓝 p] fun q ↦ kernel q.1 (rate q) :=
      (harg.continuousAt.eventually (Ioi_mem_nhds hpos)).mono fun q h ↦ by
        simp [stdDensity, show 0 < arg q from h]
    exact (hc'.congr he.symm).upperSemicontinuousAt.upperSemicontinuousWithinAt D

private theorem density_le_two {p : ℝ × ℝ} (hp : p.1 ∈ Icc (-1 : ℝ) 1) :
    stdDensity p ≤ 2 := by
  unfold stdDensity
  split_ifs with ha _
  · let y := rate p
    have hy : 0 < y := exp_pos _
    rcases le_total y 1 with h | h
    · exact (kernel_le_small hp.1 hy h).trans (by norm_num)
    · apply (kernel_le_large hp.2 h).trans
      have he := Real.pow_div_factorial_le_exp y hy.le 2
      norm_num at he
      rw [exp_neg, mul_inv_le_iff₀ (exp_pos y)]
      linarith
  · norm_num
  · norm_num

private def stdQuantile (p : ℝ × ℝ) : ℝ :=
  -p.2 * dslope exp 0 (-p.1 * p.2)

private theorem stdQuantile_continuous : Continuous stdQuantile := by
  have hc : Continuous (dslope exp (0 : ℝ)) := continuous_iff_continuousAt.mpr fun z ↦ by
    rcases eq_or_ne z 0 with rfl | hz
    · exact continuousAt_dslope_same.mpr differentiable_exp.differentiableAt
    · exact (continuousAt_dslope_of_ne hz).mpr continuous_exp.continuousAt
  exact continuous_snd.neg.mul (hc.comp (continuous_fst.neg.mul continuous_snd))

private theorem stdQuantile_rate {p : ℝ × ℝ} (hp : 0 < arg p) :
    stdQuantile (p.1, log (rate p)) = p.2 := by
  rcases p with ⟨xi, u⟩
  by_cases hxi : xi = 0
  · subst xi
    simp [stdQuantile, rate_zero, dslope_same, Real.deriv_exp]
  have harg := rate_arg_eq hp hxi
  by_cases hlog : log (rate (xi, u)) = 0
  · have hp0 : 0 < rate (xi, u) := exp_pos _
    have he : rate (xi, u) = 1 := calc
      _ = exp (log (rate (xi, u))) := (exp_log hp0).symm
      _ = 1 := by rw [hlog, exp_zero]
    rw [he, one_rpow] at harg
    have hu : u = 0 := by
      change 1 = 1 + xi * u at harg
      exact (mul_eq_zero.mp (by linarith : xi * u = 0)).resolve_left hxi
    change -log (rate (xi,u)) * dslope exp 0 (-xi * log (rate (xi,u))) = u
    rw [hlog, hu]; ring
  · have hn : -xi * log (rate (xi, u)) ≠ 0 := mul_ne_zero (neg_ne_zero.mpr hxi) hlog
    have he : exp (-xi * log (rate (xi,u))) = arg (xi,u) := by
      have hp0 : 0 < rate (xi,u) := exp_pos _
      rw [mul_comm, ← rpow_def_of_pos hp0]
      exact harg
    change -log (rate (xi,u)) * dslope exp 0 (-xi * log (rate (xi,u))) = u
    rw [dslope_of_ne _ hn, slope_def_field, sub_zero, exp_zero, he]
    unfold arg
    dsimp only
    field_simp
    ring

private theorem density_superlevel_bounded {e : ℝ} (he : 0 < e) :
    ∃ M : ℝ, 1 ≤ M ∧ ∀ p : ℝ × ℝ, p.1 ∈ Icc (-1 : ℝ) 1 → e ≤ stdDensity p → |p.2| ≤ M := by
  obtain ⟨R, hR⟩ := eventually_atTop.1
    ((tendsto_pow_mul_exp_neg_atTop_nhds_zero 2).eventually (Iio_mem_nhds he))
  let U := max R 1
  let δ : ℝ := min 1 (e^2 / 4)
  have hδ : 0 < δ := lt_min one_pos (by positivity)
  have hδ1 : δ ≤ 1 := min_le_left _ _
  have hsmall : δ ^ (1/2 : ℝ) < e := by
    rw [← sqrt_eq_rpow]
    rw [← sqrt_sq he.le]
    exact sqrt_lt_sqrt hδ.le ((min_le_right _ _).trans_lt (by nlinarith [sq_pos_of_pos he]))
  let K := Icc (-1 : ℝ) 1 ×ˢ Icc (log δ) (log U)
  obtain ⟨M, hM⟩ := ((isCompact_Icc.prod isCompact_Icc :
    IsCompact K).image stdQuantile_continuous.abs).bddAbove
  refine ⟨max 2 M, (by norm_num : (1 : ℝ) ≤ 2).trans (le_max_left _ _), ?_⟩
  intro p hp hv
  by_cases ha : 0 < arg p
  · let y := rate p
    have hy : 0 < y := exp_pos _
    rw [stdDensity, ite_eq_left ha] at hv
    have hyU : y ≤ U := by
      by_contra! h
      exact (hv.trans (kernel_le_large hp.2 ((le_max_right _ _).trans h.le))).not_gt
        (hR y ((le_max_left _ _).trans h.le))
    by_cases hlow : δ ≤ y
    · have hm : (p.1, log y) ∈ K :=
        ⟨hp, log_le_log hδ hlow, log_le_log hy hyU⟩
      have hh := hM (mem_image_of_mem _ hm)
      change |stdQuantile (p.1, log y)| ≤ M at hh
      rw [stdQuantile_rate ha] at hh
      exact hh.trans (le_max_right _ _)
    · have hx : p.1 < -(1 / 2 : ℝ) := by
        by_contra! h
        have hle := kernel_le_half h hy ((le_of_not_ge hlow).trans hδ1)
        exact (hv.trans hle).not_gt
          ((rpow_le_rpow hy.le (le_of_not_ge hlow) (by norm_num)).trans_lt hsmall)
      have hxi : p.1 ≠ 0 := (hx.trans (by norm_num)).ne
      have hae : arg p ≤ 1 := by
        rw [← rate_arg_eq ha hxi]
        exact rpow_le_one hy.le ((le_of_not_ge hlow).trans hδ1) (by linarith)
      have hup : p.2 ≤ 2 := by
        by_contra! h
        have hh := mul_lt_mul_of_neg_left h (show p.1 < 0 by linarith)
        dsimp [arg] at ha
        linarith
      have hnonneg : 0 ≤ p.2 := by
        by_contra! h
        have hh := mul_pos_of_neg_of_neg (show p.1 < 0 by linarith) h
        dsimp [arg] at hae
        linarith
      rw [abs_of_nonneg hnonneg]
      exact hup.trans (le_max_left _ _)
  · have hc : p.1 = -1 ∧ arg p = 0 := by
      by_contra h
      simp only [stdDensity, ite_eq_right ha, ite_eq_right h] at hv
      exact he.not_ge hv
    have h : p.2 = 1 := by
      have hh := hc.2
      unfold arg at hh
      rw [hc.1] at hh
      linarith
    rw [h, abs_one]
    exact (by norm_num : (1 : ℝ) ≤ 2).trans (le_max_left _ _)

private theorem nonneg_usc_mul {α : Type*} [TopologicalSpace α] {D : Set α}
    {f g : α → ℝ} (hf : UpperSemicontinuousOn f D) (hg : UpperSemicontinuousOn g D)
    (hfn : ∀ x ∈ D, 0 ≤ f x) (hgn : ∀ x ∈ D, 0 ≤ g x) :
    UpperSemicontinuousOn (fun x ↦ f x * g x) D := by
  intro p hp b hb
  let h := fun e : ℝ ↦ (f p + e) * (g p + e)
  have hc : Continuous h := by unfold h; fun_prop
  have h0 : h 0 < b := by simpa [h] using hb
  have he := hc.continuousAt.eventually (Iio_mem_nhds h0)
  have hpos : ∀ᶠ e in 𝓝[>] (0 : ℝ), e ∈ Ioi 0 := self_mem_nhdsWithin
  obtain ⟨e, hp0, heb⟩ := (hpos.and (he.filter_mono nhdsWithin_le_nhds)).exists
  filter_upwards [hf p hp (f p + e) (lt_add_of_pos_right _ hp0),
    hg p hp (g p + e) (lt_add_of_pos_right _ hp0), self_mem_nhdsWithin]
    with q hqf hqg hq
  exact (mul_le_mul hqf.le hqg.le (hgn q hq)
    (add_nonneg (hfn p hp) hp0.le)).trans_lt heb

private theorem nonneg_usc_prod {α ι : Type*} [TopologicalSpace α] {D : Set α}
    {f : ι → α → ℝ} (s : Finset ι) (hf : ∀ i ∈ s, UpperSemicontinuousOn (f i) D)
    (hn : ∀ i ∈ s, ∀ x ∈ D, 0 ≤ f i x) :
    UpperSemicontinuousOn (fun x ↦ ∏ i ∈ s, f i x) D := by
  classical
  induction s using Finset.induction_on with
  | empty =>
    simp only [Finset.prod_empty]
    exact fun p _ ↦ continuousAt_const.upperSemicontinuousAt.upperSemicontinuousWithinAt D
  | @insert i s hi ih =>
    simp only [Finset.prod_insert hi]
    exact nonneg_usc_mul (hf i (Finset.mem_insert_self ..))
      (ih (fun j hj ↦ hf j (Finset.mem_insert_of_mem hj))
        (fun j hj ↦ hn j (Finset.mem_insert_of_mem hj)))
      (hn i (Finset.mem_insert_self ..))
      (fun x hx ↦ Finset.prod_nonneg (fun j hj ↦ hn j (Finset.mem_insert_of_mem hj) x hx))

/-- Held-scale likelihood, extended by zero off the support, on the
location and shape coordinates. -/
def heldScaleLikelihood {n : ℕ} (x : Fin n → ℝ) (s : ℝ) (p : ℝ × ℝ) : ℝ := by
  classical
  exact if (p.1,s) ∈ GEV.domain x p.2 then exp (GEV.logLik x p.2 (p.1,s)) else 0

private theorem heldScaleLikelihood_eq {n : ℕ} (x : Fin n → ℝ) {s : ℝ}
    (hs : 0 < s) (p : ℝ × ℝ) :
    heldScaleLikelihood x s p =
      (s⁻¹)^n * ∏ i, stdDensity (p.2, (x i - p.1) / s) := by
  let u := fun i ↦ (x i - p.1) / s
  let q := fun i ↦ (p.2, u i)
  have hsupp (i : Fin n) : GEV.supportArg p.2 (x i) (p.1,s) = arg (q i) := rfl
  have hcoef : exp (-log s) = s⁻¹ := by rw [exp_neg, exp_log hs]
  unfold heldScaleLikelihood
  split_ifs with h
  · have hcell (i : Fin n) :
        exp (if p.2 = 0 then -log s - u i - exp (-u i) else
          -log s + (-1 / p.2 - 1) * log (arg (q i)) - (arg (q i)) ^ (-1 / p.2)) =
          s⁻¹ * stdDensity (q i) := by
      by_cases hz : p.2 = 0
      · simp only [ite_eq_left hz, sub_eq_add_neg, exp_add, hcoef]
        have he : q i = (0,u i) := Prod.ext hz rfl
        rw [he]
        simp [stdDensity, arg, rate_zero, kernel, mul_assoc]
      by_cases ha : 0 < arg (q i)
      · rw [ite_eq_right hz, stdDensity, ite_eq_left ha]
        unfold kernel
        have hr := rate_eq_rpow ha hz
        rw [hr, sub_eq_add_neg, exp_add, exp_add, hcoef, mul_assoc]
        congr 2
        rw [rpow_def_of_pos (rpow_pos_of_pos ha _), log_rpow ha]
        congr 1
        change (-1 / p.2 - 1) * log (arg (q i)) =
          (-1 / p.2) * log (arg (q i)) * (1 + p.2)
        field_simp
        ring
      · have hh := h.2 i
        rw [hsupp] at hh
        by_cases he : p.2 = -1
        · have hq0 : arg (q i) = 0 :=
            le_antisymm (le_of_not_gt ha) (by simpa only [he, ite_true] using hh)
          rw [stdDensity, ite_eq_right ha]
          have hqshape : (q i).1 = -1 := he
          simp [he, hq0, hqshape, exp_neg, exp_log hs]
        · exact (ha (by simpa only [ite_eq_right he] using hh)).elim
    unfold GEV.logLik
    rw [exp_sum]
    calc
      _ = ∏ i : Fin n, s⁻¹ * stdDensity (q i) :=
        Finset.prod_congr rfl (fun i _ ↦ hcell i)
      _ = _ := by rw [Finset.prod_mul_distrib]; simp [q, u]
  · have hbad : ∃ i, ¬ (if p.2 = -1 then 0 ≤ arg (q i) else 0 < arg (q i)) := by
      by_contra! hAll
      exact h ⟨hs, fun i ↦ hsupp i ▸ hAll i⟩
    obtain ⟨i, hi⟩ := hbad
    have hc : stdDensity (q i) = 0 := by
      by_cases hh : p.2 = -1
      · have he : arg (q i) < 0 := lt_of_not_ge (by simpa only [hh, ite_true] using hi)
        simp [stdDensity, he.not_gt, he.ne]
      · have he : ¬ 0 < arg (q i) := by simpa only [ite_eq_right hh] using hi
        simp only [stdDensity, ite_eq_right he]
        exact ite_eq_right (fun hq ↦ hh hq.1)
    rw [Finset.prod_eq_zero (Finset.mem_univ i) hc, mul_zero]

/-- The support-extended likelihood is upper semicontinuous in
location and fitted shape, including shape minus one at contact. -/
theorem held_scale_upperSemicontinuous {n : ℕ} (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s) :
    UpperSemicontinuousOn (heldScaleLikelihood x s)
      ((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) := by
  let D := (univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1
  rw [show heldScaleLikelihood x s = fun p ↦
      (s⁻¹)^n * ∏ i, stdDensity (p.2, (x i - p.1) / s) from
    funext (heldScaleLikelihood_eq x hs)]
  apply nonneg_usc_mul (fun p _ ↦
    continuousAt_const.upperSemicontinuousAt.upperSemicontinuousWithinAt D)
  · apply nonneg_usc_prod
    · intro i _ p hp c hpc
      let f := fun q : ℝ × ℝ ↦ (q.2, (x i - q.1) / s)
      have hc : Continuous f := by unfold f; fun_prop
      have hm : MapsTo f D {q : ℝ × ℝ | q.1 ∈ Icc (-1 : ℝ) 1} := fun _ hq ↦ hq.2
      exact (density_upperSemicontinuous (f p) (hm hp) c hpc).filter_mono
        (hc.continuousWithinAt.tendsto_nhdsWithin hm)
    · exact fun _ _ _ _ ↦ density_nonneg _
  · exact fun _ _ ↦ by positivity
  · exact fun _ _ ↦ Finset.prod_nonneg (fun _ _ ↦ density_nonneg _)

/-- Every positive likelihood superlevel is compact at fixed positive
scale, uniformly over the fitted shape interval. -/
theorem held_scale_compact {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ} (hs : 0 < s)
    {e : ℝ} (he : 0 < e) :
    IsCompact (((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) ∩
      heldScaleLikelihood x s ⁻¹' Ici e) := by
  let i : Fin n := ⟨0, hn⟩
  let c := (s⁻¹)^n * (2 : ℝ)^n
  have hc : 0 < c := by dsimp [c]; positivity
  obtain ⟨M, hM, hbound⟩ := density_superlevel_bounded (div_pos he hc)
  let K := Icc (x i - s*M) (x i + s*M) ×ˢ Icc (-1 : ℝ) 1
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hsub : K ⊆ (univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1 := fun _ hp ↦ ⟨mem_univ _,hp.2⟩
  suffices h : (((univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1) ∩
        heldScaleLikelihood x s ⁻¹' Ici e) = K ∩ heldScaleLikelihood x s ⁻¹' Ici e by
    rw [h]
    exact ((held_scale_upperSemicontinuous x hs).mono hsub).isCompact_inter_preimage_Ici hK e
  refine Subset.antisymm ?_ (fun _ h ↦ ⟨hsub h.1, h.2⟩)
  rintro p ⟨hp, hval⟩
  let f := fun j ↦ stdDensity (p.2, (x j - p.1)/s)
  have hprod : ∏ j, (f j / 2) ≤ f i / 2 := by
    simpa only [Finset.prod_singleton] using Finset.prod_le_prod_of_subset_of_le_one₀
      (f := fun j ↦ f j / 2) (Finset.subset_univ {i})
      (fun j _ ↦ div_nonneg (density_nonneg _) (by norm_num))
      (fun j _ _ ↦ (div_le_one (by norm_num)).mpr (density_le_two hp.2))
  have hratio : (∏ j, f j) / (2 : ℝ)^n ≤ f i := by
    rw [Finset.prod_div_distrib, Finset.prod_const, Finset.card_univ, Fintype.card_fin] at hprod
    exact hprod.trans (div_le_self (density_nonneg _) (by norm_num))
  have hfc : e / c ≤ f i := by
    apply (div_le_iff₀ hc).mpr
    change e ≤ heldScaleLikelihood x s p at hval
    rw [heldScaleLikelihood_eq x hs] at hval
    have hh := mul_le_mul_of_nonneg_left
      ((div_le_iff₀ (show 0 < (2 : ℝ)^n by positivity)).mp hratio)
      (show 0 ≤ (s⁻¹)^n by positivity)
    dsimp only [c]
    convert hval.trans hh using 1
    ring
  have hd := hbound (p.2, (x i - p.1)/s) hp.2 hfc
  change |(x i - p.1)/s| ≤ M at hd
  obtain ⟨hl, hu⟩ := abs_le.mp hd
  have h₁ := (le_div_iff₀ hs).mp hl
  have h₂ := (div_le_iff₀ hs).mp hu
  exact ⟨⟨⟨by linarith, by linarith⟩, hp.2⟩, hval⟩

/-- With scale held positive, a nonempty sample attains a maximum on
the fitted shape interval with the closed reverse-exponential endpoint. -/
theorem held_scale_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ) {s : ℝ}
    (hs : 0 < s) :
    let D := {p : ℝ × ℝ | p.2 ∈ Icc (-1 : ℝ) 1 ∧ (p.1,s) ∈ GEV.domain x p.2}
    ∃ p ∈ D, IsMaxOn (fun q ↦ GEV.logLik x q.2 (q.1,s)) D p := by
  classical
  intro D
  let p₀ : ℝ × ℝ := (0,0)
  have hp₀ : (p₀.1,s) ∈ GEV.domain x p₀.2 := by
    simp [p₀, GEV.domain, GEV.supportArg, hs]
  let L := heldScaleLikelihood x s
  let e := L p₀
  have he : 0 < e := by
    dsimp only [e, L, heldScaleLikelihood]
    rw [ite_eq_left hp₀]
    exact exp_pos _
  let E := (univ : Set ℝ) ×ˢ Icc (-1 : ℝ) 1
  let K := E ∩ L ⁻¹' Ici e
  have hk : IsCompact K := held_scale_compact hn x hs he
  have hne : K.Nonempty := ⟨p₀, ⟨mem_univ _, by norm_num [p₀]⟩, le_rfl⟩
  obtain ⟨p, hp, hm⟩ := UpperSemicontinuousOn.exists_isMaxOn hne hk
    ((held_scale_upperSemicontinuous x hs).mono (show K ⊆ E from inter_subset_left))
  have hv : e ≤ L p := hp.2
  have hdom : (p.1,s) ∈ GEV.domain x p.2 := by
    by_contra h
    have hzero : L p = 0 := by simp only [L, heldScaleLikelihood, ite_eq_right h]
    exact he.not_ge (hzero ▸ hv)
  refine ⟨p, ⟨hp.1.2, hdom⟩, ?_⟩
  intro q hq
  have hcmp : L q ≤ L p := by
    by_cases h : e ≤ L q
    · exact hm ⟨⟨mem_univ _, hq.1⟩, h⟩
    · exact (le_of_not_ge h).trans hv
  change GEV.logLik x q.2 (q.1,s) ≤ GEV.logLik x p.2 (p.1,s)
  simpa only [L, heldScaleLikelihood, ite_eq_left hq.2, ite_eq_left hdom, exp_le_exp]
    using hcmp

end

end UnivariateML.JoinsGEV

end
