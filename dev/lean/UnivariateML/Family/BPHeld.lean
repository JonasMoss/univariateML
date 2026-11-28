module

public import UnivariateML.Family.Endpoint3BetaPrime
public import UnivariateML.Family.Shape2BetaPrime
public import Mathlib.Analysis.Calculus.LocalExtr.Basic
public import Mathlib.Analysis.Calculus.InverseFunctionTheorem.Analytic
public import Mathlib.Analysis.Complex.CauchyIntegral
public import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
public import Mathlib.Analysis.SpecialFunctions.Log.NegMulLog
public import Mathlib.Analysis.Analytic.IsolatedZeros

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Stationary scale-profile comparison for beta prime

## Deviations

The signed large-shape tail uses the digamma recurrence and the analytic
reciprocal-shape data term. This avoids the need for all-order digamma
asymptotics and gives the same finite-list conclusion for every held shape.
-/

@[expose] public section

open Real Set Filter Topology

namespace UnivariateML.BPHeld

noncomputable section

variable {n : ℕ}

/-- Conditional scale score, in the log-scale coordinate. -/
def scaleScore (x : Fin n → ℝ) (p q b : ℝ) : ℝ :=
  ∑ i, ((p + q) * (x i / (b + x i)) - p)

/-- Partial score in the second shape at a fixed scale, in total log units. -/
def shapeScore (x : Fin n → ℝ) (p q b : ℝ) : ℝ :=
  ∑ i, (Gamma.digamma (p + q) - Gamma.digamma q - log (1 + x i / b))

private theorem scale_derivative {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (p q : ℝ) {b : ℝ} (hb : 0 < b) :
    HasDerivAt (fun s ↦ BetaPrime.logLik x s (p, q)) (scaleScore x p q b / b) b := by
  rw [scaleScore, Finset.sum_div]
  unfold BetaPrime.logLik
  apply HasDerivAt.fun_sum
  intro i _
  have hr := (hasDerivAt_const b (x i)).div (hasDerivAt_id b) hb.ne'
  have h₁ := hr.log (div_pos (hx i) hb).ne'
  have h₂ := (hr.const_add 1).log ((add_pos one_pos (div_pos (hx i) hb)).ne')
  convert (((h₁.const_mul (p - 1)).sub (hasDerivAt_log hb.ne')).sub
    (h₂.const_mul (p + q))).sub_const (Beta.logBeta (p, q)) using 1
  · simp
  · simp only [id_eq, Pi.div_apply]
    have h : 1 + x i / b = (b + x i) / b := by field_simp
    rw [h]
    field_simp [hb.ne', (hx i).ne', (add_pos hb (hx i)).ne']
    ring

private theorem shape_derivative {x : Fin n → ℝ} {p q b : ℝ}
    (hp : 0 < p) (hq : 0 < q) :
    HasDerivAt (fun t ↦ BetaPrime.logLik x b (p, t)) (shapeScore x p q b) q := by
  have hlog {v : ℝ} (hv : 0 < v) :
      HasDerivAt (log ∘ Real.Gamma) (Gamma.digamma v) v :=
    ((hasDerivAt_Gamma_of_pos hv).log (Gamma_pos_of_pos hv).ne').differentiableAt.hasDerivAt
  have H : (fun t : ℝ ↦ Beta.logBeta (p, t)) =ᶠ[𝓝 q]
      fun t ↦ log (Real.Gamma p) + log (Real.Gamma t) - log (Real.Gamma (p + t)) := by
    filter_upwards [Ioi_mem_nhds hq] with t ht
    have ht' : 0 < t := ht
    simp only [Beta.logBeta, Beta.normalizer, log_div
      (mul_ne_zero (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos ht').ne')
      (Gamma_pos_of_pos (add_pos hp ht')).ne',
      log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos ht').ne']
  have hβ := (((hlog hq).const_add (log (Real.Gamma p))).sub
    ((hlog (add_pos hp hq)).comp q ((hasDerivAt_id q).const_add p))).congr_of_eventuallyEq H
  unfold BetaPrime.logLik shapeScore
  apply HasDerivAt.fun_sum
  intro i _
  convert ((hasDerivAt_const q ((p - 1) * log (x i / b) - log b)).sub
    (((hasDerivAt_id q).const_add p).mul_const (log (1 + x i / b)))).sub hβ using 1
  · rfl
  · ring

/-- The conditional scale is the unique positive zero of the scale score
and the unique conditional maximizer. -/
theorem scale_score_root (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃ b > 0, IsMaxOn (fun s ↦ BetaPrime.logLik x s (p, q)) (Ioi 0) b ∧
      ∀ s > 0, scaleScore x p q s = 0 ↔ s = b := by
  obtain ⟨b, ⟨hb, hm⟩, _⟩ := Shape2BetaPrime.held_shapes_scale_mle hn hx hp hq
  have root : scaleScore x p q b = 0 :=
    (div_eq_zero_iff.mp ((scale_derivative hx p q hb).deriv.symm.trans
      (hm.isLocalMax (Ioi_mem_nhds hb)).deriv_eq_zero)).resolve_right hb.ne'
  have anti : StrictAntiOn (scaleScore x p q) (Ioi 0) := by
    intro a ha s hs has
    have ha' : 0 < a := ha
    have hs' : 0 < s := hs
    exact Finset.sum_lt_sum_of_nonempty ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      (fun i _ ↦ sub_lt_sub_right (mul_lt_mul_of_pos_left
        (div_lt_div_of_pos_left (hx i) (add_pos ha' (hx i)) (add_lt_add_left has _))
        (add_pos hp hq)) _)
  exact ⟨b, hb, hm, fun s hs ↦ ⟨fun h ↦ anti.injOn hs hb (h.trans root.symm),
    fun h ↦ h ▸ root⟩⟩

/-! ## Analytic conditional scale -/

private def scaleSum (x : Fin n → ℝ) (b : ℝ) : ℝ := ∑ i, x i / (b + x i)

private def shapeAtScale (x : Fin n → ℝ) (p b : ℝ) : ℝ :=
  p * (n / scaleSum x b - 1)

private theorem scaleSum_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 ≤ b) : 0 < scaleSum x b :=
  Finset.sum_pos (fun i _ ↦ div_pos (hx i) (add_pos_of_nonneg_of_pos hb (hx i)))
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩

private theorem shapeAtScale_pos (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p b : ℝ} (hp : 0 < p) (hb : 0 < b) : 0 < shapeAtScale x p b := by
  have hs : scaleSum x b < n := by
    simpa [scaleSum] using (Finset.sum_lt_sum_of_nonempty
      (s := (Finset.univ : Finset (Fin n))) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
      (fun i _ ↦ (div_lt_one (add_pos hb (hx i))).2 (by linarith)))
  exact mul_pos hp (sub_pos.mpr ((lt_div_iff₀ (scaleSum_pos hn hx hb.le)).2 (by linarith)))

private theorem score_eq_iff_shape (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (p q : ℝ) {b : ℝ} (hb : 0 < b) :
    scaleScore x p q b = 0 ↔ shapeAtScale x p b = q := by
  have hS := (scaleSum_pos hn hx hb.le).ne'
  simp only [scaleScore, Finset.sum_sub_distrib, ← Finset.mul_sum,
    Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  change (p + q) * scaleSum x b - n * p = 0 ↔ _
  unfold shapeAtScale
  constructor <;> intro h
  · field_simp [hS]
    linarith only [h]
  · field_simp [hS] at h
    linarith only [h]

private theorem scaleSum_analytic {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {b : ℝ} (hb : 0 ≤ b) : AnalyticAt ℝ (scaleSum x) b := by
  apply Finset.analyticAt_fun_sum
  intro i _
  exact analyticAt_const.div (analyticAt_id.add analyticAt_const)
    (add_pos_of_nonneg_of_pos hb (hx i)).ne'

private theorem shapeAtScale_deriv_pos (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p b : ℝ} (hp : 0 < p) (hb : 0 ≤ b) :
    0 < deriv (shapeAtScale x p) b := by
  have hd : HasDerivAt (scaleSum x) (∑ i, -(x i) / (b + x i) ^ 2) b := by
    apply HasDerivAt.fun_sum
    intro i _
    have h := (hasDerivAt_const b (x i)).div ((hasDerivAt_id b).add_const (x i))
      (add_pos_of_nonneg_of_pos hb (hx i)).ne'
    change HasDerivAt (fun t ↦ x i / (t + x i)) _ b at h
    simpa using h
  have h := (((hasDerivAt_const b (n : ℝ)).div hd
    (scaleSum_pos hn hx hb).ne').sub_const 1).const_mul p
  change HasDerivAt (shapeAtScale x p) _ b at h
  rw [h.deriv]
  have hs : (∑ i, -(x i) / (b + x i) ^ 2) < 0 :=
    Finset.sum_neg (fun i _ ↦ div_neg_of_neg_of_pos (neg_neg_of_pos (hx i))
      (pow_pos (add_pos_of_nonneg_of_pos hb (hx i)) _)) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  simp only [zero_mul, zero_sub]
  exact mul_pos hp (div_pos (neg_pos.mpr (mul_neg_of_pos_of_neg hN hs))
    (pow_pos (scaleSum_pos hn hx hb) _))

/-- The log likelihood's conditional scale can be chosen analytic at
every positive second shape, and it satisfies the scale equation. -/
theorem analytic_scale_profile (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) :
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0 ∧
        IsMaxOn (fun s ↦ BetaPrime.logLik x s (p, q)) (Ioi 0) (b q) := by
  classical
  choose s hs hm hr using fun q (hq : 0 < q) ↦ scale_score_root hn hx hp hq
  let b := fun q ↦ if hq : 0 < q then s q hq else 0
  have hb (q : ℝ) (hq : 0 < q) :
      0 < b q ∧ scaleScore x p q (b q) = 0 ∧
        IsMaxOn (fun t ↦ BetaPrime.logLik x t (p, q)) (Ioi 0) (b q) := by
    simp only [b, dite_eq_left hq]
    exact ⟨hs q hq, (hr q hq _ (hs q hq)).mpr rfl, hm q hq⟩
  have hi {t : ℝ} (ht : 0 < t) : b (shapeAtScale x p t) = t := by
    have hq := shapeAtScale_pos hn hx hp ht
    have ht' := (score_eq_iff_shape hn hx p _ ht).2 rfl
    simpa [b, hq] using ((hr _ hq t ht).1 ht').symm
  refine ⟨b, ?_, hb⟩
  intro q hq
  obtain ⟨hpos, hzero, _⟩ := hb q hq
  have he := (score_eq_iff_shape hn hx p q hpos).mp hzero
  rw [← he]
  have hana : AnalyticAt ℝ (shapeAtScale x p) (b q) :=
    analyticAt_const.mul ((analyticAt_const.div (scaleSum_analytic hx hpos.le)
      (scaleSum_pos hn hx hpos.le).ne').sub analyticAt_const)
  apply (analyticAt_comp_iff_of_deriv_ne_zero hana
    (shapeAtScale_deriv_pos hn hx hp hpos.le).ne').mp
  apply analyticAt_id.congr
  filter_upwards [Ioi_mem_nhds hpos] with t ht
  exact (hi ht).symm

/-! ## Score near zero -/

private theorem logGamma_analytic {q : ℝ} (hq : 0 < q) :
    AnalyticAt ℝ (log ∘ Real.Gamma) q := by
  have h : AnalyticAt ℂ Complex.Gamma (q : ℂ) := by
    apply Complex.analyticAt_iff_eventually_differentiableAt.mpr
    filter_upwards [Complex.continuous_re.continuousAt (Ioi_mem_nhds
      (show 0 < (q : ℂ).re from hq))] with z hz
    apply Complex.differentiableAt_Gamma
    intro k hk
    have h := congrArg Complex.re hk
    simp only [Complex.neg_re, Complex.natCast_re] at h
    exact (not_lt_of_ge (show z.re ≤ 0 by rw [h]; exact neg_nonpos.mpr (Nat.cast_nonneg k))) hz
  exact h.re_ofReal.log (Gamma_pos_of_pos hq)

private theorem digamma_step {q : ℝ} (hq : 0 < q) :
    Gamma.digamma (q + 1) = q⁻¹ + Gamma.digamma q := by
  have h := (logGamma_analytic (show 0 < q + 1 by positivity)).differentiableAt.hasDerivAt.comp q
    ((hasDerivAt_id q).add_const 1)
  have h' := (hasDerivAt_log hq.ne').add (logGamma_analytic hq).differentiableAt.hasDerivAt
  have he : (fun t ↦ log t + (log ∘ Real.Gamma) t) =ᶠ[𝓝 q]
      (log ∘ Real.Gamma) ∘ (fun t ↦ t + 1) := by
    filter_upwards [Ioi_mem_nhds hq] with t ht
    have ht' : 0 < t := ht
    change _ = log (Real.Gamma (t + 1))
    rw [Real.Gamma_add_one ht'.ne', log_mul ht'.ne' (Gamma_pos_of_pos ht').ne']
    rfl
  simpa [Gamma.digamma] using h.unique (h'.congr_of_eventuallyEq he.symm)

private theorem scaleSum_zero {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    scaleSum x 0 = n := by simp [scaleSum, fun i ↦ (hx i).ne']

private theorem shapeAtScale_zero (x : Fin n → ℝ) (p : ℝ) (hn : 0 < n)
    (hx : ∀ i, 0 < x i) : shapeAtScale x p 0 = 0 := by
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp [shapeAtScale, scaleSum_zero hx, hN]

private theorem shapeAtScale_analytic (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (p : ℝ) {b : ℝ} (hb : 0 ≤ b) :
    AnalyticAt ℝ (shapeAtScale x p) b :=
  analyticAt_const.mul ((analyticAt_const.div (scaleSum_analytic hx hb)
    (scaleSum_pos hn hx hb).ne').sub analyticAt_const)

private theorem small_scale_score (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) :
    ∀ᶠ b in 𝓝[>] (0 : ℝ), 0 < shapeScore x p (shapeAtScale x p b) b := by
  let Q := shapeAtScale x p
  have hQ : AnalyticAt ℝ Q 0 := shapeAtScale_analytic hn hx p (b := 0) le_rfl
  have hzero : Q 0 = 0 := shapeAtScale_zero x p hn hx
  have ht : Tendsto Q (𝓝[>] 0) (𝓝 0) := by
    simpa only [hzero] using (hQ.continuousAt.tendsto.mono_left
      (nhdsWithin_le_nhds (s := Ioi (0 : ℝ))))
  have hratio : Tendsto (fun b ↦ Q b / b) (𝓝[>] 0) (𝓝 (deriv Q 0)) := by
    have h := hasDerivAt_iff_tendsto_slope.mp hQ.differentiableAt.hasDerivAt
    have h' := h.mono_left (nhdsWithin_mono _ (show Ioi (0 : ℝ) ⊆ {0}ᶜ from fun _ hb ↦ ne_of_gt hb))
    apply h'.congr
    intro t
    rw [slope_def_field, hzero, sub_zero, sub_zero]
  have hdata (i : Fin n) :
      Tendsto (fun b ↦ Q b * log (1 + x i / b)) (𝓝[>] 0) (𝓝 0) := by
    have hcont : Tendsto (fun b ↦ log (b + x i)) (𝓝[>] 0) (𝓝 (log (x i))) := by
      have h : Tendsto (fun b : ℝ ↦ b + x i) (𝓝[>] 0) (𝓝 (x i)) := by
        simpa using (show Tendsto (fun b : ℝ ↦ b) (𝓝[>] 0) (𝓝 0) from
          nhdsWithin_le_nhds).add_const (x i)
      exact (continuousAt_log (hx i).ne').tendsto.comp h
    have hbLog : Tendsto (fun b : ℝ ↦ b * log b) (𝓝[>] 0) (𝓝 0) := by
      simpa using (continuous_mul_log.continuousAt (x := (0 : ℝ))).tendsto.mono_left
        (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
    have h := (ht.mul hcont).sub (hratio.mul hbLog)
    simp only [zero_mul, mul_zero, sub_self] at h
    apply h.congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with b hb
    have hb' : 0 < b := hb
    rw [show 1 + x i / b = (b + x i) / b by field_simp,
      log_div (add_pos hb' (hx i)).ne' hb'.ne']
    field_simp
  have ha :
      Tendsto (fun b ↦ Gamma.digamma (p + Q b)) (𝓝[>] 0) (𝓝 (Gamma.digamma p)) := by
    exact (logGamma_analytic hp).deriv.continuousAt.tendsto.comp
      (by simpa using ht.const_add p)
  have h1 :
      Tendsto (fun b ↦ Gamma.digamma (Q b + 1)) (𝓝[>] 0) (𝓝 (Gamma.digamma 1)) := by
    exact (logGamma_analytic one_pos).deriv.continuousAt.tendsto.comp
      (by simpa using ht.add_const 1)
  have hscore (i : Fin n) : Tendsto (fun b ↦ Q b *
      (Gamma.digamma (p + Q b) - Gamma.digamma (Q b) - log (1 + x i / b)))
      (𝓝[>] 0) (𝓝 1) := by
    have h := (ht.mul (ha.sub h1)).add_const 1 |>.sub (hdata i)
    simp only [zero_mul, zero_add, sub_zero] at h
    apply h.congr'
    filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with b hb
    have hq := shapeAtScale_pos hn hx hp hb
    have h := digamma_step hq
    change Gamma.digamma (Q b + 1) = (Q b)⁻¹ + Gamma.digamma (Q b) at h
    rw [h]
    have hQb : Q b ≠ 0 := hq.ne'
    field_simp [hQb]
    ring
  have h := tendsto_finsetSum Finset.univ (fun i _ ↦ hscore i)
  simp only [Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, mul_one] at h
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  filter_upwards [h.eventually (Ioi_mem_nhds hN), self_mem_nhdsWithin (s := Ioi (0 : ℝ))]
    with b hs hb
  rw [← Finset.mul_sum] at hs
  exact (mul_pos_iff_of_pos_left (shapeAtScale_pos hn hx hp hb)).mp hs

private theorem positive_near_zero (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) {b : ℝ → ℝ}
    (hb : ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0) :
    ∀ᶠ q in 𝓝[>] (0 : ℝ), 0 < shapeScore x p q (b q) := by
  obtain ⟨δ, hδ, hsmall⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp (small_scale_score hn hx hp)
  have hmono : StrictMonoOn (shapeAtScale x p) (Ioi 0) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioi _)
    · exact fun t ht ↦ (shapeAtScale_analytic hn hx p (le_of_lt ht)).continuousAt.continuousWithinAt
    · intro t ht
      rw [isOpen_Ioi.interior_eq] at ht
      exact shapeAtScale_deriv_pos hn hx hp (le_of_lt ht)
  filter_upwards [Ioo_mem_nhdsGT (shapeAtScale_pos hn hx hp hδ)] with q hq
  obtain ⟨hpos, hzero⟩ := hb q hq.1
  have he := (score_eq_iff_shape hn hx p q hpos).mp hzero
  have hlt : b q < δ := by
    by_contra! h
    exact hq.2.not_ge (he ▸ hmono.monotoneOn hδ hpos h)
  have h : 0 < shapeScore x p (shapeAtScale x p (b q)) (b q) :=
    hsmall (show b q ∈ Ioo 0 δ from ⟨hpos, hlt⟩)
  rwa [he] at h

/-! ## Score at large shape -/

private theorem data_germ (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) {b : ℝ → ℝ}
    (hb : ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0) :
    ∃ A : ℝ → ℝ, AnalyticAt ℝ A 0 ∧ A 0 = 0 ∧
      ∀ᶠ q in atTop, A q⁻¹ = ∑ i, log (1 + x i / b q) := by
  let M := fun t : ℝ ↦ ∑ i, x i / (1 + t * x i)
  let U := fun t : ℝ ↦ t * M t
  let H := fun t : ℝ ↦ U t / (p * (n - U t))
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hM : AnalyticAt ℝ M 0 := by
    apply Finset.analyticAt_fun_sum
    intro i _
    refine analyticAt_const.div (analyticAt_const.add (analyticAt_id.mul analyticAt_const)) ?_
    simp
  have hU : AnalyticAt ℝ U 0 := analyticAt_id.mul hM
  have hU0 : U 0 = 0 := by simp [U]
  have hden : p * (n - U 0) ≠ 0 := by rw [hU0, sub_zero]; positivity
  have hH : AnalyticAt ℝ H 0 :=
    hU.div (analyticAt_const.mul (analyticAt_const.sub hU)) hden
  have hH0 : H 0 = 0 := by simp [H, hU0]
  have hH' : HasDerivAt H (M 0 / (p * n)) 0 := by
    have hu := (hasDerivAt_id (0 : ℝ)).mul hM.differentiableAt.hasDerivAt
    change HasDerivAt U _ 0 at hu
    have hd := ((hasDerivAt_const (0 : ℝ) (n : ℝ)).sub hu).const_mul p
    have h := hu.div hd hden
    change HasDerivAt H _ 0 at h
    convert h using 1
    simp only [Pi.sub_apply, hU0, id_eq, one_mul, zero_mul, add_zero, sub_zero, zero_sub]
    field_simp [hp.ne', hN.ne']
  have hM0 : 0 < M 0 := by
    unfold M
    simp only [zero_mul, add_zero, div_one]
    exact Finset.sum_pos (fun i _ ↦ hx i) ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hdpos : 0 < deriv H 0 := hH'.deriv ▸ div_pos hM0 (mul_pos hp hN)
  let T := hH.hasStrictDerivAt.localInverse H (deriv H 0) 0 hdpos.ne'
  have hT : AnalyticAt ℝ T 0 := by
    simpa only [hH0] using hH.analyticAt_localInverse hdpos.ne'
  have hEq : ∀ᶠ h in 𝓝 (0 : ℝ), H (T h) = h := by
    simpa only [hH0] using hH.hasStrictDerivAt.eventually_right_inverse hdpos.ne'
  have hT0 : T 0 = 0 := by
    have h := (hH.hasStrictDerivAt.eventually_left_inverse hdpos.ne').self_of_nhds
    simpa only [hH0] using h
  have hT' : HasDerivAt T (deriv H 0)⁻¹ 0 := by
    simpa only [hH0] using (hH.hasStrictDerivAt.to_localInverse hdpos.ne').hasDerivAt
  have hPos : ∀ᶠ h in 𝓝[>] (0 : ℝ), 0 < T h := by
    have hs := hasDerivAt_iff_tendsto_slope.mp hT'
    have hs' := hs.mono_left (nhdsWithin_mono _ (show Ioi (0 : ℝ) ⊆ {0}ᶜ
      from fun _ hh ↦ ne_of_gt hh))
    filter_upwards [hs'.eventually (Ioi_mem_nhds (inv_pos.mpr hdpos)),
      self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with h hS hh
    change 0 < slope T 0 h at hS
    rw [slope_def_field, hT0, sub_zero, sub_zero] at hS
    exact (div_pos_iff_of_pos_right hh).mp hS
  let A := fun h : ℝ ↦ ∑ i, log (1 + x i * T h)
  refine ⟨A, ?_, by simp [A, hT0], ?_⟩
  · apply Finset.analyticAt_fun_sum
    intro i _
    exact (analyticAt_const.add (analyticAt_const.mul hT)).log (by simp [hT0])
  filter_upwards [tendsto_inv_atTop_nhdsGT_zero.eventually hPos,
    tendsto_inv_atTop_zero.eventually hEq, eventually_gt_atTop (0 : ℝ)] with q ht he hq
  have hUeq : U (T q⁻¹) = scaleSum x (T q⁻¹)⁻¹ := by
    unfold U M scaleSum
    rw [Finset.mul_sum]
    apply Finset.sum_congr rfl
    intro i _
    have hd : 0 < 1 + T q⁻¹ * x i := add_pos one_pos (mul_pos ht (hx i))
    field_simp [ht.ne', hd.ne', (add_pos (inv_pos.mpr ht) (hx i)).ne']
  have hq' := shapeAtScale_pos hn hx hp (inv_pos.mpr ht)
  have hHinv : H (T q⁻¹) = (shapeAtScale x p (T q⁻¹)⁻¹)⁻¹ := by
    have hs := (scaleSum_pos hn hx (inv_pos.mpr ht).le).ne'
    change U (T q⁻¹) / (p * (n - U (T q⁻¹))) = _
    rw [hUeq, shapeAtScale]
    have hne : (n : ℝ) - scaleSum x (T q⁻¹)⁻¹ ≠ 0 := by
      intro h
      unfold shapeAtScale at hq'
      rw [sub_eq_zero.mp h, div_self hs, sub_self, mul_zero] at hq'
      exact lt_irrefl 0 hq'
    field_simp [hs, hp.ne', hne]
  have hroot := inv_injective (he.symm.trans hHinv)
  obtain ⟨v, _, _, hv⟩ := scale_score_root hn hx hp hq
  have hbq := (hv _ (hb q hq).1).mp (hb q hq).2
  have hvq := (hv _ (inv_pos.mpr ht)).mp
    ((score_eq_iff_shape hn hx p q (inv_pos.mpr ht)).mpr hroot.symm)
  have heq : b q = (T q⁻¹)⁻¹ := hbq.trans hvq.symm
  simp only [A, heq, div_eq_mul_inv, inv_inv]

private theorem digamma_increment_tendsto {p : ℝ} (hp : 0 < p) :
    Tendsto (fun q ↦ Gamma.digamma (p + q) - Gamma.digamma q) atTop (𝓝 0) := by
  have hmono : MonotoneOn Gamma.digamma (Ioi 0) :=
    convexOn_log_Gamma.monotoneOn_deriv (fun _ h ↦ (logGamma_analytic h).differentiableAt)
  obtain ⟨m, hm⟩ := exists_nat_gt p
  have hbound (q : ℝ) (hq : 0 < q) (j : ℕ) :
      Gamma.digamma (q + j) - Gamma.digamma q ≤ j / q := by
    induction j with
    | zero => simp
    | succ j ih =>
      rw [Nat.cast_succ, ← add_assoc, digamma_step (by positivity : 0 < q + (j : ℝ)),
        add_div]
      have h := inv_anti₀ hq (show q ≤ q + (j : ℝ) by linarith [Nat.cast_nonneg (α := ℝ) j])
      rw [one_div]
      linarith
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds
    (show Tendsto (fun q : ℝ ↦ (m : ℝ) / q) atTop (𝓝 0) from by
      simpa [div_eq_mul_inv] using tendsto_inv_atTop_zero.const_mul (m : ℝ))
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with q hq
    exact sub_nonneg.mpr (hmono hq (add_pos hp hq) (by linarith))
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with q hq
    exact (sub_le_sub_right (hmono (add_pos hp hq) (by positivity : 0 < q + (m : ℝ))
      (by linarith)) _).trans (hbound q hq m)

private theorem analytic_right_sign {f : ℝ → ℝ} (hf : AnalyticAt ℝ f 0) :
    (∀ᶠ t in 𝓝[>] (0 : ℝ), f t = 0) ∨
      (∀ᶠ t in 𝓝[>] (0 : ℝ), 0 < f t) ∨
        (∀ᶠ t in 𝓝[>] (0 : ℝ), f t < 0) := by
  obtain ⟨a, ha⟩ := hf
  by_cases h : a = 0
  · subst a
    exact Or.inl (ha.eventually_eq_zero.filter_mono nhdsWithin_le_nhds)
  let g := (Function.swap dslope (0 : ℝ))^[a.order] f
  have hg := (ha.has_fpower_series_iterate_dslope_fslope a.order).continuousAt
  have hne : g 0 ≠ 0 := ha.iterate_dslope_fslope_ne_zero h
  rcases lt_or_gt_of_ne hne with hs | hs
  · refine Or.inr (Or.inr ?_)
    have he : ∀ᶠ t in 𝓝 (0 : ℝ), g t < 0 := hg.tendsto.eventually (Iio_mem_nhds hs)
    filter_upwards [he.filter_mono nhdsWithin_le_nhds,
      self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht ht'
    rw [ha.eq_pow_order_mul_iterate_dslope t, smul_eq_mul, sub_zero]
    exact mul_neg_of_pos_of_neg (pow_pos ht' _) ht
  · refine Or.inr (Or.inl ?_)
    have he : ∀ᶠ t in 𝓝 (0 : ℝ), 0 < g t := hg.tendsto.eventually (Ioi_mem_nhds hs)
    filter_upwards [he.filter_mono nhdsWithin_le_nhds,
      self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht ht'
    rw [ha.eq_pow_order_mul_iterate_dslope t, smul_eq_mul, sub_zero]
    exact mul_pos (pow_pos ht' _) ht

private theorem increment_sign {f : ℝ → ℝ} (hf : Tendsto f atTop (𝓝 0)) {Q : ℝ}
    (h : ∀ q ≥ Q, f q < f (q + 1)) {q : ℝ} (hq : Q ≤ q) : f q < 0 := by
  have hm (j : ℕ) : f (q + 1) ≤ f (q + 1 + j) := by
    induction j with
    | zero => simp
    | succ j ih =>
      rw [Nat.cast_succ, ← add_assoc]
      exact ih.trans (h _ (by linarith [Nat.cast_nonneg (α := ℝ) j] : Q ≤ q + 1 + (j : ℝ))).le
  have ht : Tendsto (fun j : ℕ ↦ q + 1 + (j : ℝ)) atTop atTop :=
    tendsto_atTop_add_const_left _ (q + 1) tendsto_natCast_atTop_atTop
  exact (h q hq).trans_le (ge_of_tendsto (hf.comp ht) (.of_forall hm))

/-- The score on the conditional scale profile is analytic and has strict
signs on both tails, allowing either sign on the large-shape tail. -/
theorem profile_score_tails (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) {b : ℝ → ℝ}
    (hban : AnalyticOnNhd ℝ b (Ioi 0))
    (hb : ∀ q > 0, 0 < b q ∧ scaleScore x p q (b q) = 0) :
    let S := fun q ↦ shapeScore x p q (b q)
    AnalyticOnNhd ℝ S (Ioi 0) ∧
      (∀ᶠ q in 𝓝[>] 0, 0 < S q) ∧
      ((∀ᶠ q in atTop, 0 < S q) ∨ (∀ᶠ q in atTop, S q < 0)) := by
  intro S
  have hS : AnalyticOnNhd ℝ S (Ioi 0) := by
    intro q hq
    have hq' : 0 < q := hq
    apply Finset.analyticAt_fun_sum
    intro i _
    have H := (logGamma_analytic (add_pos hp hq')).deriv.comp (analyticAt_const.add analyticAt_id)
    have J := (logGamma_analytic hq').deriv
    exact (H.sub J).sub ((analyticAt_const.add (analyticAt_const.div (hban q hq)
      (hb q hq).1.ne')).log (add_pos one_pos (div_pos (hx i) (hb q hq).1)))
  have hsmall := positive_near_zero hn hx hp hb
  refine ⟨hS, hsmall, ?_⟩
  obtain ⟨A, hA, hA0, hAeq⟩ := data_germ hn hx hp hb
  have hsplit (q : ℝ) : S q = n * (Gamma.digamma (p + q) - Gamma.digamma q) -
      ∑ i, log (1 + x i / b q) := by
    simp [S, shapeScore, Finset.sum_sub_distrib, mul_sub]
  have hS0 : Tendsto S atTop (𝓝 0) := by
    have h := ((digamma_increment_tendsto hp).const_mul (n : ℝ)).sub
      (hA.continuousAt.tendsto.comp tendsto_inv_atTop_zero)
    rw [hA0, mul_zero, sub_zero] at h
    exact h.congr' (hAeq.mono fun q hq ↦ by rw [hsplit, ← hq]; rfl)
  let V := fun t : ℝ ↦ n * (t / (1 + p * t) - t) - (A (t / (1 + t)) - A t)
  have hfrac (c : ℝ) : AnalyticAt ℝ (fun t : ℝ ↦ t / (1 + c * t)) 0 :=
    analyticAt_id.div (analyticAt_const.add (analyticAt_const.mul analyticAt_id)) (by simp)
  have hVA : AnalyticAt ℝ (fun t : ℝ ↦ A (t / (1 + t))) 0 := by
    let w := fun t : ℝ ↦ t / (1 + 1 * t)
    have h : AnalyticAt ℝ w 0 := hfrac 1
    have h' : AnalyticAt ℝ A (w 0) := by simpa [w] using hA
    simpa only [w, one_mul, Function.comp_def] using h'.comp h
  have hV : AnalyticAt ℝ V 0 :=
    (analyticAt_const.mul ((hfrac p).sub analyticAt_id)).sub (hVA.sub hA)
  have hEq : ∀ᶠ q : ℝ in atTop, S (q + 1) - S q = V q⁻¹ := by
    have hshift : Tendsto (fun q : ℝ ↦ q + 1) atTop atTop :=
      tendsto_atTop_add_const_right _ 1 tendsto_id
    filter_upwards [hAeq, hshift.eventually hAeq, eventually_gt_atTop (0 : ℝ)] with q he he' hq
    change A (q + 1)⁻¹ = _ at he'
    rw [hsplit, hsplit, ← he, ← he', ← add_assoc, digamma_step (add_pos hp hq),
      digamma_step hq]
    have ha : q⁻¹ / (1 + p * q⁻¹) = (p + q)⁻¹ := by
      have hd : 0 < 1 + p * q⁻¹ := by positivity
      field_simp [hq.ne', hd.ne', (add_pos hp hq).ne']
      ring
    have hb' : q⁻¹ / (1 + q⁻¹) = (q + 1)⁻¹ := by
      have hd : 0 < 1 + q⁻¹ := by positivity
      field_simp [hq.ne', hd.ne', (show 0 < q + 1 by positivity).ne']
    dsimp [V]
    rw [ha, hb']
    ring
  rcases analytic_right_sign hV with hz | hpos | hneg
  · obtain ⟨Q, hQ⟩ := eventually_atTop.1 (hEq.and
      (tendsto_inv_atTop_nhdsGT_zero.eventually hz))
    have hzero (q : ℝ) (hq : Q ≤ q) : S q = 0 := by
      have hstep (j : ℕ) : S (q + j) = S q := by
        induction j with
        | zero => simp
        | succ j ih =>
          rw [Nat.cast_succ, ← add_assoc]
          obtain ⟨h₁, h₂⟩ := hQ (q + j) (by linarith [Nat.cast_nonneg (α := ℝ) j])
          exact (sub_eq_zero.mp (h₁.trans h₂)).trans ih
      have ht : Tendsto (fun j : ℕ ↦ q + (j : ℝ)) atTop atTop :=
        tendsto_atTop_add_const_left _ q tendsto_natCast_atTop_atTop
      have h := (hS0.comp ht).congr (fun j ↦ hstep j)
      exact tendsto_nhds_unique tendsto_const_nhds h
    have hcenter : (max Q 0 + 1) ∈ Ioi (0 : ℝ) := by
      exact add_pos_of_nonneg_of_pos (le_max_right _ _) zero_lt_one
    have hlocal : S =ᶠ[𝓝 (max Q 0 + 1)] 0 :=
      (show ∀ᶠ q in 𝓝 (max Q 0 + 1), Q < q from
        Ioi_mem_nhds (lt_add_of_le_of_pos (le_max_left Q 0) zero_lt_one)).mono
        fun q hq ↦ hzero q (le_of_lt hq)
    have hall := hS.eqOn_zero_of_preconnected_of_eventuallyEq_zero
      (convex_Ioi _).isPreconnected hcenter hlocal
    obtain ⟨q, hq, hpq⟩ := ((show ∀ᶠ q in 𝓝[>] (0 : ℝ), q ∈ Ioi 0
        from self_mem_nhdsWithin).and hsmall).exists
    exact (hpq.ne' (hall hq)).elim
  · right
    obtain ⟨Q, hQ⟩ := eventually_atTop.1 (hEq.and
      (tendsto_inv_atTop_nhdsGT_zero.eventually hpos))
    exact (eventually_ge_atTop Q).mono fun q hq ↦ increment_sign hS0
      (fun u hu ↦ sub_pos.mp ((hQ u hu).1.symm ▸ (hQ u hu).2)) hq
  · left
    obtain ⟨Q, hQ⟩ := eventually_atTop.1 (hEq.and
      (tendsto_inv_atTop_nhdsGT_zero.eventually hneg))
    exact (eventually_ge_atTop Q).mono fun q hq ↦ neg_neg_iff_pos.mp
      (increment_sign (by simpa using hS0.neg)
        (fun u hu ↦ neg_lt_neg (sub_neg.mp ((hQ u hu).1.symm ▸ (hQ u hu).2))) hq)

private theorem finite_roots {S : ℝ → ℝ} (hS : AnalyticOnNhd ℝ S (Ioi 0))
    (hsmall : ∀ᶠ q in 𝓝[>] (0 : ℝ), 0 < S q)
    (hlarge : (∀ᶠ q in atTop, 0 < S q) ∨ (∀ᶠ q in atTop, S q < 0)) :
    {q | 0 < q ∧ S q = 0}.Finite := by
  obtain ⟨e, he, hsmall'⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp hsmall
  have hlarge' : ∀ᶠ q in atTop, S q ≠ 0 :=
    hlarge.elim (fun h ↦ h.mono fun _ hq ↦ hq.ne') (fun h ↦ h.mono fun _ hq ↦ hq.ne)
  obtain ⟨M, hM⟩ := eventually_atTop.1 hlarge'
  have hdis : ∀ᶠ q in codiscreteWithin (Ioi (0 : ℝ)), S q ≠ 0 := by
    rcases hS.eqOn_zero_or_eventually_ne_zero_of_preconnected
      (convex_Ioi (0 : ℝ)).isPreconnected with h | h
    · obtain ⟨q, hq, hpq⟩ := ((show ∀ᶠ q in 𝓝[>] (0 : ℝ), q ∈ Ioi 0
        from self_mem_nhdsWithin).and hsmall).exists
      exact (hpq.ne' (h hq)).elim
    · exact h
  have hsub : Icc e M ⊆ Ioi 0 := fun _ h ↦ he.trans_le h.1
  have hfin := (isCompact_Icc (a := e) (b := M)).finite_sdiff_of_mem_codiscreteWithin
    (show {q | S q ≠ 0} ∈ codiscreteWithin (Icc e M) from codiscreteWithin_mono hsub hdis)
  exact hfin.subset (fun q ⟨hq, hz⟩ ↦
    ⟨⟨le_of_not_gt (fun h ↦ (hsmall' ⟨hq, h⟩).ne' hz),
      le_of_not_gt (fun h ↦ hM q h.le hz)⟩, fun h ↦ h hz⟩)

/-! ## Finite stationary comparison -/

/-- A stationary conditional scale-profile point tying or beating the
matched gamma is equivalent to finite attainment. -/
private theorem first_stationary_comparison (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {p : ℝ} (hp : 0 < p) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (p, v.1)
    let G := ∑ i, Gamma.logDensity p (Gamma.rate x p) (x i)
    (∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
      ∃ v ∈ Beta.domain, scaleScore x p v.1 v.2 = 0 ∧
        shapeScore x p v.1 v.2 = 0 ∧ G ≤ L v := by
  intro L G
  have h := Endpoint3BetaPrime.held_first_face_comparison hn hx hp
  constructor
  · rintro ⟨v, hv, hm⟩
    have hb : 0 < v.2 := hv.2
    have hq : 0 < v.1 := hv.1
    have hs : IsMaxOn (fun s ↦ BetaPrime.logLik x s (p, v.1)) (Ioi 0) v.2 :=
      fun s hs ↦ hm (show (v.1, s) ∈ Beta.domain from ⟨hv.1, hs⟩)
    have ht : IsMaxOn (fun t ↦ BetaPrime.logLik x v.2 (p, t)) (Ioi 0) v.1 :=
      fun t ht ↦ hm (show (t, v.2) ∈ Beta.domain from ⟨ht, hv.2⟩)
    obtain ⟨w, hw, hwG⟩ := h.mp ⟨v, hv, hm⟩
    refine ⟨v, hv, ?_, ?_, hwG.trans (hm hw)⟩
    · exact (div_eq_zero_iff.mp ((scale_derivative hx p v.1 hb).deriv.symm.trans
        (hs.isLocalMax (Ioi_mem_nhds hb)).deriv_eq_zero)).resolve_right hb.ne'
    · exact (shape_derivative (x := x) (b := v.2) hp hq).deriv.symm.trans
        (ht.isLocalMax (Ioi_mem_nhds hq)).deriv_eq_zero
  · rintro ⟨v, hv, _, _, hG⟩
    exact h.mpr ⟨v, hv, hG⟩

private theorem profile_derivative {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) {b : ℝ → ℝ}
    (hb : 0 < b q) (hbd : DifferentiableAt ℝ b q) (hz : scaleScore x p q (b q) = 0) :
    HasDerivAt (fun t ↦ BetaPrime.logLik x (b t) (p, t)) (shapeScore x p q (b q)) q := by
  let F := fun t ↦ BetaPrime.logLik x (b t) (p, q)
  let L := fun t ↦ BetaPrime.logLik x (b q) (p, t)
  let G := fun t ↦ ∑ i, log (1 + x i / b t)
  have hG : DifferentiableAt ℝ G q := by
    unfold G
    apply DifferentiableAt.fun_sum
    intro i _
    exact ((differentiableAt_const (x i)).div hbd hb.ne' |>.const_add 1).log
      (add_pos one_pos (div_pos (hx i) hb)).ne'
  have hF : HasDerivAt F 0 q := by
    simpa only [F, Function.comp_def, hz, zero_div, zero_mul] using
      (scale_derivative hx p q hb).comp q hbd.hasDerivAt
  have hL : HasDerivAt L (shapeScore x p q (b q)) q := shape_derivative hp hq
  have hD := ((hasDerivAt_id q).sub_const q).mul
    (hG.hasDerivAt.sub_const (G q))
  have h := (hF.add hL).sub_const (L q) |>.sub hD
  simp only [id_eq, sub_self, zero_mul, mul_zero, add_zero, zero_add, sub_zero] at h
  convert h using 1
  funext t
  change _ = F t + L t - L q - (t - q) * (G t - G q)
  simp only [F, L, G, BetaPrime.logLik, ← Finset.sum_sub_distrib, ← Finset.sum_add_distrib,
    Finset.mul_sum]
  apply Finset.sum_congr rfl
  intro i _
  ring

/-- For any fixed positive first shape, the conditional scale profile has
finitely many stationary shapes. A finite fit exists exactly when one
of those stationary values reaches the matched gamma, including equality. -/
theorem held_first_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {p : ℝ} (hp : 0 < p) :
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (p, v.1)
    let G := ∑ i, Gamma.logDensity p (Gamma.rate x p) (x i)
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      (∀ q > 0, 0 < b q ∧ IsMaxOn (fun s ↦ L (q, s)) (Ioi 0) (b q)) ∧
      let P := fun q ↦ L (q, b q)
      let C := {q | 0 < q ∧ deriv P q = 0}
      C.Finite ∧ ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
        ∃ q ∈ C, G ≤ P q) := by
  intro L G
  obtain ⟨b, hban, hb⟩ := analytic_scale_profile hn hx hp
  obtain ⟨hs, hsmall, hlarge⟩ := profile_score_tails hn hx hp hban
    (fun q hq ↦ ⟨(hb q hq).1, (hb q hq).2.1⟩)
  let P := fun q ↦ L (q, b q)
  let C := {q | 0 < q ∧ deriv P q = 0}
  have hd (q : ℝ) (hq : 0 < q) : deriv P q = shapeScore x p q (b q) :=
    (profile_derivative hx hp hq (hb q hq).1 (hban q hq).differentiableAt (hb q hq).2.1).deriv
  have hC : C = {q | 0 < q ∧ shapeScore x p q (b q) = 0} := by
    ext q
    change (0 < q ∧ deriv P q = 0) ↔ (0 < q ∧ shapeScore x p q (b q) = 0)
    exact and_congr_right (fun hq ↦ by rw [hd q hq])
  refine ⟨b, hban, fun q hq ↦ ⟨(hb q hq).1, (hb q hq).2.2⟩, ?_, ?_⟩
  · change C.Finite
    rw [hC]
    exact finite_roots hs hsmall hlarge
  have hcompare := first_stationary_comparison hn hx hp
  constructor
  · intro hm
    obtain ⟨v, hv, hz₁, hz₂, hG⟩ := hcompare.mp hm
    obtain ⟨s, _, _, hr⟩ := scale_score_root hn hx hp hv.1
    have he : v.2 = b v.1 :=
      ((hr _ hv.2).mp hz₁).trans (((hr _ (hb v.1 hv.1).1).mp (hb v.1 hv.1).2.1).symm)
    refine ⟨v.1, ⟨hv.1, (hd v.1 hv.1).trans (he ▸ hz₂)⟩, ?_⟩
    change G ≤ L (v.1, b v.1)
    rw [← he]
    exact hG
  · rintro ⟨q, ⟨hq, hz⟩, hG⟩
    exact hcompare.mpr ⟨(q, b q), ⟨hq, (hb q hq).1⟩, (hb q hq).2.1,
      (hd q hq).symm.trans hz, hG⟩

/-- With second shape fixed, the finite stationary list is that of the
reciprocal sample. A fit exists iff its transported likelihood reaches
the matched inverse gamma, including equality. -/
theorem held_second_criterion (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {q : ℝ} (hq : 0 < q) :
    let y := fun i ↦ (x i)⁻¹
    let J := ∑ i, 2 * log (x i)
    let L := fun v : ℝ × ℝ ↦ BetaPrime.logLik x v.2 (v.1, q)
    let G := (∑ i, Gamma.logDensity q (Gamma.rate y q) (y i)) - J
    ∃ b : ℝ → ℝ, AnalyticOnNhd ℝ b (Ioi 0) ∧
      (∀ p > 0, 0 < b p ∧ IsMaxOn (fun s ↦ L (p, s)) (Ioi 0) (b p)) ∧
      let P := fun p ↦ L (p, b p)
      let C := {p | 0 < p ∧ deriv P p = 0}
      C.Finite ∧ ((∃ v ∈ Beta.domain, IsMaxOn L Beta.domain v) ↔
        ∃ p ∈ C, G ≤ P p) := by
  intro y J L G
  obtain ⟨d, hdan, hd, hfin, hiff⟩ := held_first_criterion hn (x := y)
    (fun i ↦ inv_pos.mpr (hx i)) hq
  let b := fun p ↦ (d p)⁻¹
  let A := fun v : ℝ × ℝ ↦ BetaPrime.logLik y v.2 (q, v.1)
  let P := fun p ↦ A (p, d p)
  let R := fun p ↦ L (p, b p)
  let T : ℝ × ℝ → ℝ × ℝ := fun v ↦ (v.1, v.2⁻¹)
  have hT : BijOn T Beta.domain Beta.domain :=
    ⟨fun _ hv ↦ ⟨hv.1, inv_pos.mpr hv.2⟩,
      fun v _ w _ h ↦ by
        have ha := congrArg (fun a : ℝ × ℝ ↦ a.1) h
        have hb := congrArg (fun a : ℝ × ℝ ↦ a.2) h
        exact Prod.ext ha (inv_injective hb),
      fun v hv ↦ ⟨T v, ⟨hv.1, inv_pos.mpr hv.2⟩, Prod.ext rfl (inv_inv _)⟩⟩
  have hphys (v : ℝ × ℝ) (hv : v ∈ Beta.domain) : L v = A (T v) + (-J) :=
    AllFreeBetaPrime.reciprocal_logLik hx hv.2 v.1 q
  have hR {p : ℝ} (hp : 0 < p) : R p = P p - J := by
    have h := hphys (p, b p) ⟨hp, inv_pos.mpr (hd p hp).1⟩
    simpa [T, A, b, R, P, sub_eq_add_neg] using h
  have hDeriv {p : ℝ} (hp : 0 < p) : deriv R p = deriv P p := by
    have h : R =ᶠ[𝓝 p] fun t ↦ P t - J := by
      filter_upwards [Ioi_mem_nhds hp] with t ht
      exact hR ht
    rw [h.deriv_eq, deriv_sub_const]
  have hC : {p | 0 < p ∧ deriv R p = 0} = {p | 0 < p ∧ deriv P p = 0} := by
    ext p
    change (0 < p ∧ deriv R p = 0) ↔ (0 < p ∧ deriv P p = 0)
    exact and_congr_right (fun hp ↦ by rw [hDeriv hp])
  refine ⟨b, fun p hp ↦ (hdan p hp).inv (hd p hp).1.ne', ?_, ?_, ?_⟩
  · intro p hp
    refine ⟨inv_pos.mpr (hd p hp).1, fun s hs ↦ ?_⟩
    change L (p, s) ≤ R p
    rw [hphys (p, s) ⟨hp, hs⟩, hR hp]
    exact sub_le_sub_right ((hd p hp).2 (inv_pos.mpr hs)) _
  · exact hC ▸ hfin
  · refine (Transfer.exists_max_iff hT hphys).trans (hiff.trans ?_)
    rw [hC]
    constructor
    · rintro ⟨p, hp, h⟩
      refine ⟨p, hp, ?_⟩
      change G ≤ R p
      rw [hR hp.1]
      exact sub_le_sub_right h J
    · rintro ⟨p, hp, h⟩
      change G ≤ R p at h
      rw [hR hp.1] at h
      exact ⟨p, hp, (sub_le_sub_iff_right J).mp h⟩

end

end UnivariateML.BPHeld

end
