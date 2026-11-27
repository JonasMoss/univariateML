module

public import UnivariateML.Family.StudentHalf
public import UnivariateML.Family.FinishHalfT
public import UnivariateML.Family.Student2MovingCollapse

import Mathlib.Tactic
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.ImproperIntegrals
import Mathlib.MeasureTheory.Integral.Prod
import Mathlib.Analysis.Calculus.ParametricIntervalIntegral
public import Mathlib.Analysis.Analytic.IsolatedZeros
import Mathlib.Analysis.Complex.CauchyIntegral
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Analysis.SpecialFunctions.Integrals.Basic

set_option linter.style.header false

/-!
# Half-Student stationary comparisons

The profile theorem below identifies all winning candidates. Finiteness
of the general stationary root list additionally requires a strict
eventual derivative sign at the normal end.
-/

@[expose] public section

open Set Real Filter Topology

namespace UnivariateML.Student2Half

open SkewLSHalfT

private theorem noncontact_of_count {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (hc : 2*TDist.multiplicity x b ≤ n) : ∃ i, x i ≠ b := by
  by_contra! h
  have H : TDist.multiplicity x b = n := by simp [TDist.multiplicity,h]
  omega

private theorem optimum_scale_profile {n : ℕ} (x : Fin n → ℝ) (μ : ℝ) {q s : ℝ}
    (hs : 0 < s) (hm : IsMaxOn (likelihood x μ q) (Ioi 0) s) :
    scaleProfile x μ q = likelihood x μ q s :=
  (hm.isLUB hs).csSup_eq (Nonempty.image _ ⟨s,hs⟩)

private theorem scale_max_of_score {n : ℕ} (x : Fin n → ℝ) (b : ℝ) {q s : ℝ}
    (hq : 0 < q) (hx : ∃ i, x i ≠ b) (he : 0 < TDist.exponent x b q)
    (hs : 0 < s) (hS : TDist.scaleScore x b q s = 0) :
    IsMaxOn (likelihood x b q) (Ioi 0) s := by
  have hm := (((TDist.heldLocation_maximum x b hq hx).2 he).2 s hs).mpr hS
  intro a ha
  have hd : TDist.logLik x b q a ≤ TDist.logLik x b q s := hm ha
  exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr hd) (by positivity : 0 ≤ (2 : ℝ)^n)

/-- Feasible floor and stationary candidates, represented with the
minimum endpoint and the unique eligible conditional scale. -/
def Candidate {n : ℕ} (x : Fin n → ℝ) (b : ℝ) (p : ℝ × ℝ × ℝ) : Prop :=
  p.1 = b ∧ 1 ≤ p.2.1 ∧ 0 < p.2.2 ∧
    0 < TDist.exponent x b p.2.1 ∧ TDist.scaleScore x b p.2.1 p.2.2 = 0 ∧
    (p.2.1 = 1 ∨ deriv (scaleProfile x b) p.2.1 = 0)

set_option maxHeartbeats 1000000 in
-- Transporting the fitted extrema to the endpoint slice expands the product-chart predicates.
private theorem max_candidate {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (hc : 2*TDist.multiplicity x (x i) ≤ n)
    {p : ℝ × ℝ × ℝ} (hp : p ∈ FinishHalfT.domain x)
    (hm : IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ likelihood x q.1 q.2.1 q.2.2)
      (FinishHalfT.domain x) p) :
    Candidate x (x i) p := by
  have hν := zero_lt_one.trans_le hp.2.1
  have hs : 0 < p.2.2 := hp.2.2
  obtain ⟨j,hj,H⟩ := StudentHalf.held_shape_scale_endpoint x hn hν hs
  have hEndpoint : p.1 = x j := (H _).mp ⟨hp.1,fun m hm' ↦ by
    have hq : (m,p.2.1,p.2.2) ∈ FinishHalfT.domain x := ⟨hm',hp.2.1,hs⟩
    exact hm hq⟩
  have hloc : p.1 = x i := hEndpoint.trans (le_antisymm (hj i) (hi j))
  have hscale : IsMaxOn (likelihood x (x i) p.2.1) (Ioi 0) p.2.2 := by
    intro a ha
    have hh : likelihood x (x i) p.2.1 a ≤ likelihood x p.1 p.2.1 p.2.2 :=
      hm (show (x i,p.2.1,a) ∈ FinishHalfT.domain x from ⟨hi,hp.2.1,ha⟩)
    change likelihood x (x i) p.2.1 a ≤ likelihood x (x i) p.2.1 p.2.2
    simpa only [hloc] using hh
  have hL : IsMaxOn (TDist.logLik x (x i) p.2.1) (Ioi 0) p.2.2 := by
    intro a ha
    have H : likelihood x (x i) p.2.1 a ≤ likelihood x (x i) p.2.1 p.2.2 := hscale ha
    unfold likelihood TDist.likelihood at H
    exact exp_le_exp.mp (le_of_mul_le_mul_left H (by positivity : 0 < (2 : ℝ)^n))
  have hx := noncontact_of_count x hn (x i) hc
  have H := TDist.heldLocation_maximum x (x i) hν hx
  have he := H.1.mp ⟨p.2.2,hs,hL⟩
  have hS := ((H.2 he).2 _ hs).mp hL
  have hv := optimum_scale_profile x (x i) hs hscale
  refine ⟨hloc,hp.2.1,hs,he,hS,?_⟩
  rcases (show 1 ≤ p.2.1 from hp.2.1).eq_or_lt with hh | hh
  · exact Or.inl hh.symm
  · apply Or.inr
    apply IsLocalMax.deriv_eq_zero
    change ∀ᶠ u : ℝ in 𝓝 p.2.1, scaleProfile x (x i) u ≤ scaleProfile x (x i) p.2.1
    filter_upwards [Ici_mem_nhds hh] with u hu
    rw [hv]
    change sSup (likelihood x (x i) u '' Ioi (0 : ℝ)) ≤ _
    refine csSup_le (show (likelihood x (x i) u '' Ioi (0 : ℝ)).Nonempty from
      ⟨_,1,zero_lt_one,rfl⟩) ?_
    rintro _ ⟨a,ha,rfl⟩
    have hh : likelihood x (x i) u a ≤ likelihood x p.1 p.2.1 p.2.2 :=
      @hm (x i,u,a) (show (x i,u,a) ∈ FinishHalfT.domain x from ⟨hi,hu,ha⟩)
    change likelihood x (x i) u a ≤ likelihood x (x i) p.2.1 p.2.2
    simpa only [hloc] using hh

set_option maxHeartbeats 1000000 in
-- The comparison expands the product-chart and profile value predicates.
/-- The finite-parameter argmax of the all-free half-Student fit consists
of the eligible candidates at the largest value, provided they reach
both excluded faces. Each eligible shape has one conditional scale. -/
theorem all_free_maximizing_candidates {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) (p : ℝ × ℝ × ℝ) :
    let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.1 p.2.2
    (p ∈ FinishHalfT.domain x ∧ IsMaxOn L (FinishHalfT.domain x) p) ↔
      2*TDist.multiplicity x (x i) ≤ n ∧ Candidate x (x i) p ∧
        max (normalFace x (x i)) (collapseFace x (x i)) ≤ L p ∧
        ∀ q, Candidate x (x i) q → L q ≤ L p := by
  dsimp only
  let L := fun p : ℝ × ℝ × ℝ ↦ likelihood x p.1 p.2.1 p.2.2
  have feasible (q : ℝ × ℝ × ℝ) (h : Candidate x (x i) q) : q ∈ FinishHalfT.domain x :=
    ⟨by rw [h.1]; exact hi,h.2.1,h.2.2.1⟩
  have profile (q : ℝ × ℝ × ℝ) (hc : 2*TDist.multiplicity x (x i) ≤ n)
      (h : Candidate x (x i) q) : scaleProfile x (x i) q.2.1 = L q := by
    have hv := zero_lt_one.trans_le h.2.1
    have hm := scale_max_of_score x (x i) hv (noncontact_of_count x hn _ hc)
      h.2.2.2.1 h.2.2.1 h.2.2.2.2.1
    rw [optimum_scale_profile x (x i) h.2.2.1 hm]
    change _ = likelihood x q.1 q.2.1 q.2.2
    rw [h.1]
  constructor
  · rintro ⟨hp,hm⟩
    obtain ⟨hc,q,hq,he,hstat,hface⟩ := (FinishHalfT.all_free_criterion x hi).mp ⟨p,hp,hm⟩
    have hv := zero_lt_one.trans_le hq
    obtain ⟨a,⟨ha,ha'⟩,_⟩ := ((TDist.heldLocation_maximum x (x i) hv
      (noncontact_of_count x hn _ hc)).2 he).1
    have hC : Candidate x (x i) (x i,q,a) := ⟨rfl,hq,ha,he,ha',hstat.imp_right HasDerivAt.deriv⟩
    have hh : L (x i,q,a) ≤ L p := @hm (x i,q,a) (feasible _ hC)
    have hP : scaleProfile x (x i) q = L (x i,q,a) := profile (x i,q,a) hc hC
    rw [hP] at hface
    refine ⟨hc,max_candidate x hn hi hc (p := p) hp hm,?_ ,fun q hq ↦ @hm q (feasible q hq)⟩
    exact hface.trans hh
  · rintro ⟨hc,hC,hFace,hBest⟩
    have hF : max (normalFace x (x i)) (collapseFace x (x i)) ≤ scaleProfile x (x i) p.2.1 :=
      hFace.trans (le_of_eq (profile p hc hC).symm)
    obtain ⟨q,hq,hm⟩ := (joint_attainment_criterion x hi hc).mpr
      ⟨p.2.1,hC.2.1,hC.2.2.2.1,hF⟩
    have hqC := max_candidate x hn hi hc (p := q) hq hm
    exact ⟨feasible _ hC,fun u hu ↦ (@hm u hu).trans (hBest q hqC)⟩

open MeasureTheory

private noncomputable def gammaLog (v : ℝ) : ℝ := log (Gamma v)

private theorem gammaLog_diff {v : ℝ} (hv : 0 < v) : DifferentiableAt ℝ gammaLog v :=
  (Real.differentiableAt_Gamma (fun m ↦ by have : 0 ≤ (m : ℝ) := Nat.cast_nonneg m; linarith)).log
    (Gamma_pos_of_pos hv).ne'

private theorem gammaLog_deriv_recur {v : ℝ} (hv : 0 < v) :
    deriv gammaLog (v+1) = 1/v + deriv gammaLog v := by
  have hd := (hasDerivAt_id v).add_const 1
  have h1 := (gammaLog_diff (show 0 < v+1 by linarith)).hasDerivAt.comp v hd
  have h2 := (Real.hasDerivAt_log hv.ne').add (gammaLog_diff hv).hasDerivAt
  have he : (fun z ↦ gammaLog (z+1)) =ᶠ[𝓝 v] (fun z ↦ log z + gammaLog z) := by
    filter_upwards [Ioi_mem_nhds hv] with z hz
    have h : 0 < z := hz
    simp [gammaLog, Gamma_add_one h.ne', log_mul h.ne' (Gamma_pos_of_pos h).ne']
  have hh := (h1.congr_of_eventuallyEq he.symm).unique h2
  simpa using hh

private noncomputable def gammaIncrement (v : ℝ) : ℝ :=
  (deriv gammaLog ((v+1)/2) - deriv gammaLog (v/2))/2

private theorem gammaIncrement_recur {v : ℝ} (hv : 0 < v) :
    gammaIncrement v + gammaIncrement (v+1) = 1/v := by
  unfold gammaIncrement
  rw [show (v+1+1)/2 = v/2+1 by ring, gammaLog_deriv_recur (half_pos hv)]
  field_simp
  ring

private theorem gammaIncrement_pos {v : ℝ} (hv : 0 < v) : 0 ≤ gammaIncrement v := by
  have hm : MonotoneOn (deriv gammaLog) (Ioi 0) :=
    Real.convexOn_log_Gamma.monotoneOn_deriv (fun _ hu ↦ gammaLog_diff hu)
  unfold gammaIncrement
  exact div_nonneg (sub_nonneg.mpr (hm (by change 0 < v/2; positivity)
    (by change 0 < (v+1)/2; positivity) (by linarith))) (by norm_num)

private noncomputable def laplaceKernel (v t : ℝ) : ℝ := exp (-v*t) / (1+exp (-t))

private theorem laplaceKernel_integrable {v : ℝ} (hv : 0 < v) :
    IntegrableOn (laplaceKernel v) (Ioi 0) := by
  have h : Integrable (fun x : ℝ ↦ exp (-v*x)) (volume.restrict (Ioi 0)) :=
    integrableOn_exp_mul_Ioi (a := -v) (neg_neg_of_pos hv) (0 : ℝ)
  apply h.mono
  · have hc : Continuous (laplaceKernel v) := by
      unfold laplaceKernel
      exact (by fun_prop : Continuous fun t : ℝ ↦ exp (-v*t)).div
        (by fun_prop) (fun t ↦ by positivity)
    exact hc.aestronglyMeasurable
  · exact ae_of_all _ fun t ↦ by
      simp only [laplaceKernel, Real.norm_eq_abs, abs_of_pos (exp_pos _),
        abs_of_nonneg (show 0 ≤ exp (-v*t)/(1+exp (-t)) by positivity)]
      exact div_le_self (exp_pos _).le (by linarith [exp_pos (-t)])

private noncomputable def gammaLaplace (v : ℝ) : ℝ :=
  ∫ t in Ioi (0 : ℝ), laplaceKernel v t

private theorem gammaLaplace_recur {v : ℝ} (hv : 0 < v) :
    gammaLaplace v + gammaLaplace (v+1) = 1/v := by
  unfold gammaLaplace
  rw [← integral_add (laplaceKernel_integrable hv) (laplaceKernel_integrable (by linarith))]
  have hh : (fun t ↦ laplaceKernel v t + laplaceKernel (v+1) t) = fun t ↦ exp (-v*t) := by
    funext t
    rw [laplaceKernel, laplaceKernel, show -(v+1)*t = -v*t + -t by ring, exp_add]
    field_simp
  rw [hh, integral_exp_mul_Ioi (neg_neg_of_pos hv)]
  simp

private theorem gammaLaplace_pos (v : ℝ) : 0 ≤ gammaLaplace v :=
  integral_nonneg fun t ↦ by unfold laplaceKernel; positivity

/-- The derivative of the difference of log Gamma values at consecutive
half steps is the Laplace transform of the inverse logistic kernel. -/
theorem log_gamma_half_step_derivative {v : ℝ} (hv : 0 < v) :
    HasDerivAt (fun u ↦ log (Gamma ((u+1)/2)) - log (Gamma (u/2)))
      (∫ t in Ioi (0 : ℝ), exp (-v*t)/(1+exp (-t))) v := by
  have hD : ∀ {u : ℝ}, 0 < u → |gammaIncrement u-gammaLaplace u| ≤ 1/u := by
    intro u hu
    have h1 := gammaIncrement_recur hu
    have h2 := gammaLaplace_recur hu
    have h3 := gammaIncrement_pos hu
    have h4 := gammaIncrement_pos (show 0 < u+1 by linarith)
    have h5 := gammaLaplace_pos u
    have h6 := gammaLaplace_pos (u+1)
    exact abs_le.mpr ⟨by linarith,by linarith⟩
  have hInvariant (k : ℕ) :
      |gammaIncrement (v+k)-gammaLaplace (v+k)| = |gammaIncrement v-gammaLaplace v| := by
    induction k with
    | zero => simp
    | succ k ih =>
      rw [Nat.cast_succ, ← add_assoc]
      have hp : 0 < v+(k : ℝ) := by positivity
      have h1 := gammaIncrement_recur hp
      have h2 := gammaLaplace_recur hp
      rw [show gammaIncrement (v+(k : ℝ)+1)-gammaLaplace (v+(k : ℝ)+1) =
        -(gammaIncrement (v+k)-gammaLaplace (v+k)) by linarith, abs_neg, ih]
  have he : gammaIncrement v = gammaLaplace v := by
    by_contra hn
    have hd := abs_pos.mpr (sub_ne_zero.mpr hn)
    obtain ⟨k,hk⟩ := exists_nat_gt (1/|gammaIncrement v-gammaLaplace v|)
    have hp : 0 < v+(k : ℝ) := by positivity
    have hlt : 1/(v+(k : ℝ)) < |gammaIncrement v-gammaLaplace v| := by
      apply (div_lt_iff₀ hp).2
      have hh := (div_lt_iff₀ hd).1 hk
      nlinarith [mul_pos hv hd]
    exact hlt.not_ge ((hInvariant k) ▸ hD hp)
  have h1 := (gammaLog_diff (show 0 < (v+1)/2 by positivity)).hasDerivAt.comp v
    (((hasDerivAt_id v).add_const 1).div_const (2 : ℝ))
  have h2 := (gammaLog_diff (half_pos hv)).hasDerivAt.comp v ((hasDerivAt_id v).div_const (2 : ℝ))
  change HasDerivAt _ (gammaLaplace v) v
  rw [← he]
  have hE : gammaIncrement v =
      deriv gammaLog ((v+1)/2) * (1/2) - deriv gammaLog (v/2) * (1/2) := by
    unfold gammaIncrement
    ring
  rw [hE]
  convert h1.sub h2 using 1 <;> rfl


/-- A positive-to-negative crossing of the shape score follows from one
negative-to-positive crossing of its Laplace kernel. -/
theorem laplace_score_strict_crossing {K : ℝ → ℝ} {b v w : ℝ} (hb : 0 < b) (hv : v < w)
    (hneg : ∀ t ∈ Ioo 0 b, K t < 0) (hpos : ∀ t ∈ Ioi b, 0 ≤ K t)
    (iv : IntegrableOn (fun t ↦ exp (-v*t)*K t) (Ioi 0))
    (iw : IntegrableOn (fun t ↦ exp (-w*t)*K t) (Ioi 0))
    (hscore : (∫ t in Ioi (0 : ℝ), exp (-v*t)*K t) ≤ 0) :
    (∫ t in Ioi (0 : ℝ), exp (-w*t)*K t) < 0 := by
  let c := exp (-(w-v)*b)
  let G := fun t : ℝ ↦ c*(exp (-v*t)*K t)-exp (-w*t)*K t
  have iG : IntegrableOn G (Ioi (0 : ℝ)) := (iv.const_mul c).sub iw
  have hform (t : ℝ) : G t = (c-exp (-(w-v)*t))*(exp (-v*t)*K t) := by
    have he : exp (-(w-v)*t)*exp (-v*t) = exp (-w*t) := by rw [← exp_add]; congr 1; ring
    dsimp [G]
    linear_combination K t*he
  have hG1 (t : ℝ) (ht : t ∈ Ioo 0 b) : 0 < G t := by
    have hh : c < exp (-(w-v)*t) :=
      exp_lt_exp.mpr (mul_lt_mul_of_neg_left ht.2 (by linarith))
    rw [hform]
    exact mul_pos_of_neg_of_neg (sub_neg.mpr hh)
      (mul_neg_of_pos_of_neg (exp_pos _) (hneg _ ht))
  have hG (t : ℝ) (ht : t ∈ Ioi 0) : 0 ≤ G t := by
    rcases lt_trichotomy t b with h | rfl | h
    · exact (hG1 t ⟨ht,h⟩).le
    · rw [hform]; simp [c]
    · have hh : exp (-(w-v)*t) ≤ c :=
        exp_le_exp.mpr (mul_le_mul_of_nonpos_left h.le (by linarith))
      rw [hform]
      exact mul_nonneg (sub_nonneg.mpr hh) (mul_nonneg (exp_pos _).le (hpos _ h))
  have hlt : 0 < ∫ t in Ioi (0 : ℝ), G t := by
    rw [integral_pos_iff_support_of_nonneg_ae _ iG]
    · have hsub : Ioo 0 b ⊆ Function.support G := fun t ht ↦ (hG1 t ht).ne'
      apply lt_of_lt_of_le _ (measure_mono hsub)
      rw [Measure.restrict_apply measurableSet_Ioo, inter_eq_left.mpr (Ioo_subset_Ioi_self : Ioo 0 b ⊆ Ioi 0),
        Real.volume_Ioo, sub_zero]
      exact ENNReal.ofReal_pos.mpr hb
    · exact (ae_restrict_iff' measurableSet_Ioi).mpr (ae_of_all _ hG)
  have he : (∫ t in Ioi (0 : ℝ), G t) =
      c*(∫ t in Ioi (0 : ℝ), exp (-v*t)*K t) -
        (∫ t in Ioi (0 : ℝ), exp (-w*t)*K t) := by
    dsimp [G]
    rw [integral_sub (iv.const_mul c) iw, integral_const_mul]
  rw [he] at hlt
  linarith [mul_nonpos_of_nonneg_of_nonpos (exp_pos (-(w-v)*b)).le hscore]


private theorem log_laplace {v Q : ℝ} (hv : 0 < v) (hQ : 0 ≤ Q) :
    IntegrableOn (fun t ↦ exp (-v*t) * ∫ u in Icc 0 Q, exp (-u*t)) (Ioi 0) ∧
    (∫ t in Ioi (0 : ℝ), exp (-v*t) * ∫ u in Icc 0 Q, exp (-u*t)) =
      log (1+Q/v) := by
  let μ := volume.restrict (Ioi (0 : ℝ))
  let ν := volume.restrict (Icc (0 : ℝ) Q)
  let f := fun t u : ℝ ↦ exp (-(v+u)*t)
  have hbase : Integrable (fun p : ℝ × ℝ ↦ exp (-v*p.1) * (1 : ℝ)) (μ.prod ν) :=
    (integrableOn_exp_mul_Ioi (neg_neg_of_pos hv) 0).mul_prod
      (integrableOn_const (s := Icc (0 : ℝ) Q) (by simp [Real.volume_Icc]))
  have hi : Integrable (Function.uncurry f) (μ.prod ν) := by
    apply hbase.mono
    · unfold Function.uncurry f
      exact (by fun_prop : Continuous fun p : ℝ × ℝ ↦ exp (-(v+p.2)*p.1)).aestronglyMeasurable
    · have ht : ∀ᵐ t ∂μ, t ∈ Ioi (0 : ℝ) := ae_restrict_mem measurableSet_Ioi
      have hu : ∀ᵐ u ∂ν, u ∈ Icc (0 : ℝ) Q := ae_restrict_mem measurableSet_Icc
      have hae : ∀ᵐ p ∂μ.prod ν, p ∈ Ioi (0 : ℝ) ×ˢ Icc (0 : ℝ) Q :=
        (Measure.ae_prod_mem_iff_ae_ae_mem (measurableSet_Ioi.prod measurableSet_Icc)).mpr
          (ht.mono fun t ht ↦ hu.mono fun u hu ↦ ⟨ht,hu⟩)
      filter_upwards [hae] with p hp
      have ht' : 0 < p.1 := hp.1
      have hu' : 0 ≤ p.2 := hp.2.1
      simp only [Function.uncurry, f, mul_one, Real.norm_eq_abs, abs_of_pos (exp_pos _)]
      exact exp_le_exp.mpr (by nlinarith [mul_nonneg ht'.le hu'])
  have hleft (t : ℝ) : exp (-v*t) * (∫ u, exp (-u*t) ∂ν) = ∫ u, f t u ∂ν := by
    rw [← integral_const_mul]
    apply integral_congr_ae (ae_of_all _ fun u ↦ ?_)
    dsimp [f]
    rw [← exp_add]
    congr 1
    ring
  have hI : Integrable (fun t ↦ ∫ u, f t u ∂ν) μ := hi.integral_prod_left
  constructor
  · change Integrable (fun t ↦ exp (-v*t)*(∫ u, exp (-u*t) ∂ν)) μ
    simp_rw [hleft]
    exact hI
  change (∫ t, exp (-v*t) * (∫ u, exp (-u*t) ∂ν) ∂μ) = _
  simp_rw [hleft]
  rw [integral_integral_swap hi]
  have hright (u : ℝ) (hu : u ∈ Icc 0 Q) : (∫ t, f t u ∂μ) = 1/(v+u) := by
    have hu' : 0 ≤ u := hu.1
    unfold f μ
    rw [integral_exp_mul_Ioi (by linarith : -(v+u) < 0)]
    simp only [mul_zero, exp_zero, neg_div_neg_eq]
  calc
    (∫ u, ∫ t, f t u ∂μ ∂ν) = ∫ u in Icc 0 Q, 1/(v+u) := by
      exact setIntegral_congr_fun measurableSet_Icc hright
    _ = ∫ u in (0:ℝ)..Q, 1/(v+u) := by
      rw [intervalIntegral.integral_of_le hQ, integral_Icc_eq_integral_Ioc]
    _ = log (1+Q/v) := by
      rw [show (fun u : ℝ ↦ 1/(v+u)) = (fun u : ℝ ↦ 1/(u+v)) by
        ext u; rw [add_comm]]
      rw [intervalIntegral.integral_comp_add_right (f := fun u : ℝ ↦ 1/u)]
      rw [zero_add, integral_one_div_of_pos hv (by linarith)]
      congr 1
      field_simp <;> ring


private noncomputable def oneMagnitudeLog (Q w v : ℝ) : ℝ :=
  TDist.logNormalizer v-w*((v+1)/2)*log (1+Q/v)

private noncomputable def magnitudeKernel (Q w t : ℝ) : ℝ :=
  2/(1+exp (-t))-(1-w)+w*((Q-1)*exp (-Q*t)-∫ u in Icc 0 Q, exp (-u*t))

private theorem kernel_integral_eq_and_integrable {v Q : ℝ} (hv : 0 < v) (hQ : 0 ≤ Q) (w : ℝ) :
    IntegrableOn (fun t ↦ exp (-v*t)*magnitudeKernel Q w t) (Ioi 0) ∧
    (∫ t in Ioi (0 : ℝ), exp (-v*t)*magnitudeKernel Q w t) =
      2*gammaLaplace v-(1-w)/v+w*((Q-1)/(v+Q)-log (1+Q/v)) := by
  have i0 := laplaceKernel_integrable hv
  have i1 := integrableOn_exp_mul_Ioi (neg_neg_of_pos hv) (0 : ℝ)
  have hp : 0 < v+Q := by linarith
  have i2 := integrableOn_exp_mul_Ioi (neg_neg_of_pos hp) (0 : ℝ)
  have i3 := log_laplace hv hQ
  have hform (t : ℝ) : exp (-v*t)*magnitudeKernel Q w t =
      2*laplaceKernel v t-(1-w)*exp (-v*t)+
        w*((Q-1)*exp (-(v+Q)*t)-exp (-v*t)*(∫ u in Icc 0 Q, exp (-u*t))) := by
    have he : exp (-(v+Q)*t) = exp (-v*t)*exp (-Q*t) := by
      rw [← exp_add]; congr 1; ring
    rw [he]
    unfold magnitudeKernel laplaceKernel
    field_simp
  simp_rw [hform]
  have i4 := i0.const_mul (2:ℝ)
  have i5 := i1.const_mul (1-w)
  have i6 := (i2.const_mul (Q-1)).sub i3.1
  refine ⟨(i4.sub i5).add (i6.const_mul w),?_⟩
  have hAdd := integral_add (i4.sub i5) (i6.const_mul w)
  simp only [Pi.sub_apply] at hAdd
  rw [hAdd, integral_sub i4 i5, integral_const_mul,
    integral_const_mul, integral_const_mul, integral_sub (i2.const_mul (Q-1)) i3.1,
    integral_const_mul, i3.2, integral_exp_mul_Ioi (neg_neg_of_pos hv),
    integral_exp_mul_Ioi (neg_neg_of_pos hp)]
  simp only [mul_zero,exp_zero,neg_div_neg_eq,gammaLaplace]
  ring

private theorem oneMagnitudeLog_derivative {v Q : ℝ} (hv : 0 < v) (hQ : 0 ≤ Q) (w : ℝ) :
    HasDerivAt (oneMagnitudeLog Q w)
      ((∫ t in Ioi (0 : ℝ), exp (-v*t)*magnitudeKernel Q w t)/2) v := by
  have hp : 0 < v+Q := by linarith
  have hvlog := (Real.hasDerivAt_log hv.ne')
  have hqlog := (Real.hasDerivAt_log hp.ne').comp v ((hasDerivAt_id v).add_const Q)
  have hh : HasDerivAt (fun u : ℝ ↦ log (1+Q/u)) (1/(v+Q)-1/v) v := by
    have hb : (fun u : ℝ ↦ log (1+Q/u)) =ᶠ[𝓝 v] (fun u ↦ log (u+Q)-log u) := by
      filter_upwards [Ioi_mem_nhds hv] with u hu
      have hu' : 0 < u := hu
      rw [← log_div (by linarith : u+Q ≠ 0) hu'.ne']
      congr 1
      field_simp
    have h' : HasDerivAt (fun u : ℝ ↦ log (u+Q)-log u) (1/(v+Q)-1/v) v := by
      convert hqlog.sub hvlog using 1 <;> simp [Function.comp_def, one_div] <;> rfl
    exact h'.congr_of_eventuallyEq hb
  have hpi : HasDerivAt (fun u : ℝ ↦ log (u*π)/2) (1/v/2) v := by
    have hp' : 0 < v*π := mul_pos hv pi_pos
    have h := ((Real.hasDerivAt_log hp'.ne').comp v ((hasDerivAt_id v).mul_const π)).div_const 2
    convert h using 1 <;> simp [Function.comp_def] <;> field_simp [hv.ne',pi_ne_zero]
  have hG := (log_gamma_half_step_derivative hv).sub hpi
  have hT := (((hasDerivAt_id v).add_const 1).div_const 2).const_mul w
  have h := hG.sub (hT.mul hh)
  unfold oneMagnitudeLog TDist.logNormalizer
  rw [(kernel_integral_eq_and_integrable hv hQ w).2]
  convert h using 1
  · rfl
  · change _ = gammaLaplace v - 1/v/2 -
      (w*(1/2)*log (1+Q/v) + w*((v+1)/2)*(1/(v+Q)-1/v))
    field_simp [hv.ne',hp.ne']
    <;> ring


private theorem kernel_inner_derivative {Q : ℝ} (hQ : 0 ≤ Q) (t : ℝ) :
    HasDerivAt (fun z : ℝ ↦ ∫ u in Icc 0 Q, exp (-u*z))
      (∫ u in Icc 0 Q, -u*exp (-u*t)) t := by
  have he : ∀ f : ℝ → ℝ, (∫ u in Icc 0 Q, f u) = ∫ u in (0:ℝ)..Q, f u := fun f ↦ by
    rw [intervalIntegral.integral_of_le hQ, integral_Icc_eq_integral_Ioc]
  simp_rw [he]
  refine (intervalIntegral.hasDerivAt_integral_of_dominated_loc_of_deriv_le
    (F := fun z u : ℝ ↦ exp (-u*z))
    (F' := fun z u : ℝ ↦ -u*exp (-u*z))
    (bound := fun _ ↦ Q*exp (Q*(|t|+1)))
    (Metric.ball_mem_nhds t zero_lt_one) ?_ ?_ ?_ ?_ ?_ ?_).2
  · exact Filter.Eventually.of_forall fun z ↦ (by fun_prop : Continuous fun u : ℝ ↦
      exp (-u*z)).aestronglyMeasurable
  · exact (by fun_prop : Continuous fun u : ℝ ↦ exp (-u*t)).intervalIntegrable 0 Q
  · exact (by fun_prop : Continuous fun u : ℝ ↦ -u*exp (-u*t)).aestronglyMeasurable
  · filter_upwards with u hu z hz
    rw [uIoc_of_le hQ] at hu
    have hu0 : 0 ≤ u := hu.1.le
    have huQ : u ≤ Q := hu.2
    have hz' : |z-t| < 1 := by simpa [Real.dist_eq] using hz
    have hz'' : -z ≤ |t|+1 := by
      have hzlo := (abs_lt.mp hz').1
      have htlo := neg_abs_le t
      linarith
    have he : exp (-u*z) ≤ exp (Q*(|t|+1)) := by
      apply exp_le_exp.mpr
      calc
        -u*z = u*(-z) := by ring
        _ ≤ u*(|t|+1) := mul_le_mul_of_nonneg_left hz'' hu0
        _ ≤ Q*(|t|+1) := mul_le_mul_of_nonneg_right huQ (by positivity)
    have hnon : -u*exp (-u*z) ≤ 0 := mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr hu0) (exp_pos _).le
    rw [Real.norm_eq_abs, abs_of_nonpos hnon, neg_mul, neg_neg]
    exact mul_le_mul huQ he (exp_pos _).le hQ
  · exact continuous_const.intervalIntegrable 0 Q
  · filter_upwards with u _ z _
    have he := ((hasDerivAt_id z).const_mul (-u)).exp
    simpa only [id_eq, mul_one, mul_comm] using he

private noncomputable def kernelSlope (Q w t : ℝ) : ℝ :=
  2*exp (-t)/(1+exp (-t))^2 - w*Q*(Q-1)*exp (-Q*t) +
    w*(∫ u in Icc 0 Q, u*exp (-u*t))

private theorem magnitudeKernel_derivative {Q : ℝ} (hQ : 0 ≤ Q) (w t : ℝ) :
    HasDerivAt (magnitudeKernel Q w) (kernelSlope Q w t) t := by
  have he := ((hasDerivAt_id t).const_mul (-1)).exp
  have h₀ : HasDerivAt (fun z : ℝ ↦ 2/(1+exp (-z)))
      (2*exp (-t)/(1+exp (-t))^2) t := by
    have h := (hasDerivAt_const t (2 : ℝ)).div (he.const_add 1) (by positivity)
    convert h using 1 <;> simp [id_eq] <;> rfl
  have h₁ := (((hasDerivAt_id t).const_mul (-Q)).exp).const_mul (Q-1)
  have h₂ := kernel_inner_derivative hQ t
  have h := (h₀.sub_const (1-w)).add ((h₁.sub h₂).const_mul w)
  unfold magnitudeKernel kernelSlope
  convert h using 1
  · rfl
  · simp only [mul_one, id_eq, neg_mul, integral_neg]
    ring

private theorem magnitudeKernel_zero {Q : ℝ} (hQ : 0 ≤ Q) (w : ℝ) :
    magnitudeKernel Q w 0 = 0 := by
  unfold magnitudeKernel
  simp only [mul_zero, exp_zero, neg_zero]
  rw [setIntegral_const, Real.volume_real_Icc]
  simp only [sub_zero, max_eq_left hQ, smul_eq_mul, mul_one]
  ring_nf

private theorem weightedSlope_form {Q w t : ℝ} :
    exp (Q*t)*kernelSlope Q w t =
      2*exp ((Q-1)*t)/(1+exp (-t))^2-w*Q*(Q-1)+
        w*(∫ u in Icc 0 Q, u*exp ((Q-u)*t)) := by
  have he : exp (Q*t)*exp (-Q*t) = 1 := by rw [← exp_add]; ring_nf; simp
  have hh : exp (Q*t)*exp (-t) = exp ((Q-1)*t) := by rw [← exp_add]; congr 1; ring
  have hI : exp (Q*t)*(∫ u in Icc 0 Q, u*exp (-u*t)) =
      ∫ u in Icc 0 Q, u*exp ((Q-u)*t) := by
    rw [← integral_const_mul]
    apply integral_congr_ae (ae_of_all _ fun u ↦ ?_)
    calc
      exp (Q*t)*(u*exp (-u*t)) = u*(exp (Q*t)*exp (-u*t)) := by ring
      _ = u*exp ((Q-u)*t) := by rw [← exp_add]; congr 2; ring
  unfold kernelSlope
  rw [← hI, ← hh]
  linear_combination -w*Q*(Q-1)*(he-1)

private theorem weightedSlope_strictMono {Q w : ℝ} (hQ : 1 < Q) (hw : 0 ≤ w) :
    StrictMono (fun t ↦ exp (Q*t)*kernelSlope Q w t) := by
  intro s t hst
  change exp (Q*s)*kernelSlope Q w s < exp (Q*t)*kernelSlope Q w t
  rw [weightedSlope_form, weightedSlope_form]
  have hnum : 2*exp ((Q-1)*s) < 2*exp ((Q-1)*t) := by
    gcongr
  have hden : (1+exp (-t))^2 ≤ (1+exp (-s))^2 := by gcongr
  have hlt : 2*exp ((Q-1)*s)/(1+exp (-s))^2 < 2*exp ((Q-1)*t)/(1+exp (-t))^2 := by
    calc
      _ < 2*exp ((Q-1)*t)/(1+exp (-s))^2 := div_lt_div_of_pos_right hnum (by positivity)
      _ ≤ _ := div_le_div_of_nonneg_left (by positivity) (by positivity) hden
  have hle : (∫ u in Icc 0 Q, u*exp ((Q-u)*s)) ≤ ∫ u in Icc 0 Q, u*exp ((Q-u)*t) := by
    apply setIntegral_mono_on
    · exact (by fun_prop : Continuous fun u : ℝ ↦ u*exp ((Q-u)*s)).continuousOn.integrableOn_Icc
    · exact (by fun_prop : Continuous fun u : ℝ ↦ u*exp ((Q-u)*t)).continuousOn.integrableOn_Icc
    · exact measurableSet_Icc
    · intro u hu
      have h0 : 0 ≤ u := hu.1
      have h1 : 0 ≤ Q-u := sub_nonneg.mpr hu.2
      exact mul_le_mul_of_nonneg_left (exp_le_exp.mpr (mul_le_mul_of_nonneg_left hst.le h1)) h0
  linarith [mul_le_mul_of_nonneg_left hle hw]

private theorem magnitudeKernel_mvt {Q : ℝ} (hQ : 0 ≤ Q) (w : ℝ) {s t : ℝ} (hst : s < t) :
    ∃ u ∈ Ioo s t, kernelSlope Q w u = (magnitudeKernel Q w t-magnitudeKernel Q w s)/(t-s) := by
  have hd (u : ℝ) := magnitudeKernel_derivative hQ w u
  exact exists_hasDerivAt_eq_slope _ _ hst (fun u _ ↦ (hd u).continuousAt.continuousWithinAt)
    (fun u _ ↦ hd u)

private theorem small_magnitude_kernel_positive {Q w : ℝ}
    (hQ : 0 ≤ Q) (hQ' : Q ≤ 1) (hw : 0 ≤ w) (t : ℝ) (ht : 0 < t) :
    0 < magnitudeKernel Q w t := by
  have hJ : (∫ u in Icc 0 Q, exp (-u*t)) ≤ Q := by
    calc
      _ ≤ ∫ u in Icc 0 Q, (1 : ℝ) := by
        apply setIntegral_mono_on
        · exact (by fun_prop : Continuous fun u : ℝ ↦ exp (-u*t)).continuousOn.integrableOn_Icc
        · exact continuous_const.continuousOn.integrableOn_Icc
        · exact measurableSet_Icc
        · intro u hu
          exact exp_le_one_iff.mpr (by nlinarith [mul_nonneg hu.1 ht.le])
      _ = Q := by
        rw [setIntegral_const, Real.volume_real_Icc]
        simp [hQ]
  have hE : exp (-Q*t) ≤ 1 := exp_le_one_iff.mpr (by nlinarith [mul_nonneg hQ ht.le])
  have he : 1-1 < 2/(1+exp (-t))-1 := by
    have ht' := exp_lt_one_iff.mpr (neg_neg_of_pos ht)
    apply lt_of_le_of_lt (by norm_num : (1:ℝ)-1 ≤ 0)
    apply sub_pos.mpr
    exact (lt_div_iff₀ (by positivity)).2 (by linarith)
  have hh : 0 ≤ 1+(Q-1)*exp (-Q*t)-(∫ u in Icc 0 Q, exp (-u*t)) := by
    linarith [mul_le_mul_of_nonpos_left hE (sub_nonpos.mpr hQ')]
  unfold magnitudeKernel
  linarith [mul_nonneg hw hh]

/-- A one-magnitude half-t shape kernel has constant strict sign or
exactly one crossing from negative to positive off the origin. -/
private theorem magnitudeKernel_sign {Q w : ℝ} (hQ : 0 ≤ Q) (hw : 0 ≤ w) :
    (∀ t > 0, 0 < magnitudeKernel Q w t) ∨
      (∀ t > 0, magnitudeKernel Q w t < 0) ∨
      ∃ b > 0, (∀ t ∈ Ioo 0 b, magnitudeKernel Q w t < 0) ∧
        (∀ t ∈ Ioi b, 0 ≤ magnitudeKernel Q w t) := by
  by_cases hQ' : Q ≤ 1
  · exact Or.inl (small_magnitude_kernel_positive hQ hQ' hw)
  have hM := weightedSlope_strictMono (lt_of_not_ge hQ') hw
  have slopes {a b : ℝ} (hab : a < b) (ha : 0 ≤ kernelSlope Q w a) :
      0 < kernelSlope Q w b := by
    exact pos_of_mul_pos_right ((mul_nonneg (exp_pos _).le ha).trans_lt (hM hab)) (exp_pos _).le
  have h0 := magnitudeKernel_zero hQ w
  by_cases hz : ∃ b > 0, magnitudeKernel Q w b = 0
  · rcases hz with ⟨b,hb,hbz⟩
    obtain ⟨c,hc,hs⟩ := magnitudeKernel_mvt hQ w hb
    have hc0 : kernelSlope Q w c = 0 := by simp [hs,h0,hbz]
    refine Or.inr (Or.inr ⟨b,hb,?_,?_⟩)
    · intro t ht
      by_contra! h
      obtain ⟨d,hd,hm⟩ := magnitudeKernel_mvt hQ w ht.1
      have hd' : 0 ≤ kernelSlope Q w d := by rw [hm,h0,sub_zero]; exact div_nonneg h (sub_nonneg.mpr ht.1.le)
      obtain ⟨e,he,heq⟩ := magnitudeKernel_mvt hQ w ht.2
      have hh := slopes (hd.2.trans he.1) hd'
      rw [heq,hbz,zero_sub] at hh
      exact (not_lt_of_ge (div_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr h)
        (sub_nonneg.mpr ht.2.le))) hh
    · intro t ht
      obtain ⟨d,hd,hm⟩ := magnitudeKernel_mvt hQ w (show b < t from ht)
      have hp := slopes (hc.2.trans hd.1) hc0.symm.le
      rw [hm,hbz,sub_zero] at hp
      exact ((div_pos_iff_of_pos_right (sub_pos.mpr ht)).mp hp).le
  · have hc : Continuous (magnitudeKernel Q w) :=
      continuous_iff_continuousAt.mpr fun t ↦ (magnitudeKernel_derivative hQ w t).continuousAt
    have constant_sign {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) :
        ¬ (magnitudeKernel Q w a ≤ 0 ∧ 0 ≤ magnitudeKernel Q w b) ∧
        ¬ (0 ≤ magnitudeKernel Q w a ∧ magnitudeKernel Q w b ≤ 0) := by
      constructor
      · intro ⟨h₁,h₂⟩
        obtain ⟨z,hz',he⟩ := intermediate_value_Icc hab hc.continuousOn ⟨h₁,h₂⟩
        exact hz ⟨z,ha.trans_le hz'.1,he⟩
      · intro ⟨h₁,h₂⟩
        obtain ⟨z,hz',he⟩ := intermediate_value_Icc' hab hc.continuousOn ⟨h₂,h₁⟩
        exact hz ⟨z,ha.trans_le hz'.1,he⟩
    by_cases h : 0 < magnitudeKernel Q w 1
    · left
      intro t ht
      by_contra! hn
      rcases le_total t 1 with hle | hle
      · exact (constant_sign ht hle).1 ⟨hn,h.le⟩
      · exact (constant_sign zero_lt_one hle).2 ⟨h.le,hn⟩
    · right
      left
      intro t ht
      by_contra! hn
      have h := le_of_not_gt h
      rcases le_total t 1 with hle | hle
      · exact (constant_sign ht hle).2 ⟨hn,h⟩
      · exact (constant_sign zero_lt_one hle).1 ⟨h,hn⟩


private theorem signed_kernel_integral {v : ℝ} {K : ℝ → ℝ}
    (i : IntegrableOn (fun t ↦ exp (-v*t)*K t) (Ioi 0)) (h : ∀ t > 0, 0 < K t) :
    0 < ∫ t in Ioi (0 : ℝ), exp (-v*t)*K t := by
  let μ := volume.restrict (Ioi (0 : ℝ))
  have h₁ (t : ℝ) (ht : t ∈ Ioi (0 : ℝ)) : 0 < exp (-v*t)*K t :=
    mul_pos (exp_pos _) (h t ht)
  change 0 < ∫ t, exp (-v*t)*K t ∂μ
  rw [integral_pos_iff_support_of_nonneg_ae _ i]
  · have hs : Ioo 0 1 ⊆ Function.support (fun t ↦ exp (-v*t)*K t) :=
      fun t ht ↦ (h₁ t ht.1).ne'
    apply lt_of_lt_of_le _ (measure_mono hs)
    have he : μ (Ioo (0 : ℝ) 1) = 1 := by
      unfold μ
      rw [Measure.restrict_apply measurableSet_Ioo,
        inter_eq_left.mpr (Ioo_subset_Ioi_self : Ioo (0 : ℝ) 1 ⊆ Ioi 0),Real.volume_Ioo]
      simp
    rw [he]; exact zero_lt_one
  · exact (ae_restrict_iff' measurableSet_Ioi).mpr (ae_of_all _ fun t ht ↦ (h₁ t ht).le)

private theorem magnitudeScore_prior_positive {Q w v z : ℝ} (hQ : 0 ≤ Q) (hw : 0 ≤ w)
    (hv : 0 < v) (hvz : v < z)
    (hz : (∫ t in Ioi (0 : ℝ), exp (-z*t)*magnitudeKernel Q w t) = 0) :
    0 < ∫ t in Ioi (0 : ℝ), exp (-v*t)*magnitudeKernel Q w t := by
  have hz0 := hv.trans hvz
  have iv := (kernel_integral_eq_and_integrable hv hQ w).1
  have iz := (kernel_integral_eq_and_integrable hz0 hQ w).1
  rcases magnitudeKernel_sign hQ hw with h | h | ⟨b,hb,hn,hp⟩
  · exact signed_kernel_integral iv h
  · have hin : IntegrableOn (fun t ↦ exp (-z*t)* -magnitudeKernel Q w t) (Ioi 0) := by
      apply Integrable.congr iz.neg
      exact ae_of_all _ fun t ↦ by
        change -(exp (-z*t)*magnitudeKernel Q w t) = exp (-z*t)* -magnitudeKernel Q w t
        rw [mul_neg]
    have hs := signed_kernel_integral hin (fun t ht ↦ neg_pos.mpr (h t ht))
    simp only [mul_neg, integral_neg, hz, neg_zero, lt_irrefl] at hs
  · by_contra! hn'
    exact (laplace_score_strict_crossing hb hvz hn hp iv iz hn').ne hz

private theorem distinct_positive_magnitude_max {Q w f a b : ℝ} (hQ : 0 ≤ Q) (hw : 0 ≤ w)
    (ha : a ∈ Ioi (0 : ℝ) ∩ Ici f) (hab : a < b)
    (hB : IsMaxOn (oneMagnitudeLog Q w) (Ioi 0 ∩ Ici f) b) :
    oneMagnitudeLog Q w a < oneMagnitudeLog Q w b := by
  have hb : 0 < b := ha.1.trans hab
  have hfb : f < b := ha.2.trans_lt hab
  have hbMax : IsLocalMax (oneMagnitudeLog Q w) b :=
    hB.isLocalMax (Filter.inter_mem (Ioi_mem_nhds hb) (Ici_mem_nhds hfb))
  have hd := (oneMagnitudeLog_derivative hb hQ w).deriv
  have hz' := hbMax.deriv_eq_zero
  rw [hd] at hz'
  have hz : (∫ t in Ioi (0 : ℝ), exp (-b*t)*magnitudeKernel Q w t) = 0 := by linarith
  have hc (v : ℝ) (hv : v ∈ Icc a b) : ContinuousAt (oneMagnitudeLog Q w) v :=
    (oneMagnitudeLog_derivative (ha.1.trans_le hv.1) hQ w).continuousAt
  obtain ⟨v,hv,hvs⟩ := exists_deriv_eq_slope _ hab
    (fun v hv ↦ (hc v hv).continuousWithinAt)
    (fun v hv ↦ (oneMagnitudeLog_derivative (ha.1.trans hv.1) hQ w).differentiableAt.differentiableWithinAt)
  rw [(oneMagnitudeLog_derivative (ha.1.trans hv.1) hQ w).deriv] at hvs
  have hsp := magnitudeScore_prior_positive hQ hw (ha.1.trans hv.1) hv.2 hz
  rw [← sub_pos]
  apply (div_pos_iff_of_pos_right (sub_pos.mpr hab)).mp
  linarith

set_option maxHeartbeats 1000000 in
private theorem oneMagnitude_likelihood_eq {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b d s v : ℝ} (hs : 0 < s) (hv : 0 < v) (hx : ∀ i, x i = b ∨ x i = b+d) :
    let w := ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ)/n
    likelihood x b v s = (2 : ℝ)^n * exp ((n : ℝ)*(oneMagnitudeLog (d^2/s^2) w v - log s)) := by
  dsimp only
  unfold likelihood TDist.likelihood TDist.logLik oneMagnitudeLog
  congr 2
  have hterm (i : Fin n) :
      TDist.logNormalizer v - log s - (v+1)/2*log (1+(x i-b)^2/(v*s^2)) =
        (TDist.logNormalizer v-log s) - if x i = b then 0 else (v+1)/2*log (1+(d^2/s^2)/v) := by
    by_cases h : x i = b
    · simp [h]
    · have he := (hx i).resolve_left h
      rw [ite_eq_right h, he, add_sub_cancel_left]
      congr 3
      field_simp
  simp_rw [hterm]
  rw [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  have he (i : Fin n) : (if x i = b then (0 : ℝ) else (v+1)/2*log (1+(d^2/s^2)/v)) =
      if x i ≠ b then (v+1)/2*log (1+(d^2/s^2)/v) else 0 := by by_cases h : x i = b <;> simp [h]
  simp_rw [he]
  rw [Finset.sum_ite, Finset.sum_const, Finset.sum_const_zero, add_zero, nsmul_eq_mul]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  field_simp
  ring

/-- With fixed endpoint and scale and at most one positive residual
magnitude, the set of attained maximizing shapes is a subsingleton.
The shape floor may be zero or positive, and endpoint contacts are allowed. -/
theorem held_endpoint_scale_one_magnitude {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b d s f : ℝ} (hs : 0 < s)
    (hx : ∀ i, x i = b ∨ x i = b+d) :
    Set.Subsingleton {v : ℝ | v ∈ Ioi 0 ∩ Ici f ∧
      IsMaxOn (fun u ↦ likelihood x b u s) (Ioi 0 ∩ Ici f) v} := by
  let Q := d^2/s^2
  let w := ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ)/n
  have hQ : 0 ≤ Q := by unfold Q; positivity
  have hw : 0 ≤ w := by unfold w; positivity
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have htrans {v z : ℝ} (hv : v ∈ Ioi (0 : ℝ) ∩ Ici f) (hz : z ∈ Ioi (0 : ℝ) ∩ Ici f) :
      likelihood x b v s ≤ likelihood x b z s ↔
        oneMagnitudeLog Q w v ≤ oneMagnitudeLog Q w z := by
    rw [oneMagnitude_likelihood_eq x hn hs hv.1 hx,oneMagnitude_likelihood_eq x hn hs hz.1 hx,
      mul_le_mul_iff_right₀ (by positivity : 0 < (2 : ℝ)^n), exp_le_exp,
      mul_le_mul_iff_right₀ hn',sub_le_sub_iff_right]
  have H {a b' : ℝ} (ha : a ∈ Ioi (0 : ℝ) ∩ Ici f)
      (hb : b' ∈ Ioi (0 : ℝ) ∩ Ici f)
      (hA : IsMaxOn (fun u ↦ likelihood x b u s) (Ioi 0 ∩ Ici f) a)
      (hB : IsMaxOn (fun u ↦ likelihood x b u s) (Ioi 0 ∩ Ici f) b') : ¬ a < b' := by
    intro hab
    have hM : IsMaxOn (oneMagnitudeLog Q w) (Ioi 0 ∩ Ici f) b' :=
      fun z hz ↦ (htrans hz hb).mp (@hB z hz)
    exact (distinct_positive_magnitude_max hQ hw ha hab hM).not_ge
      ((htrans hb ha).mp (@hA b' hb))
  intro a ha b' hb
  exact le_antisymm (le_of_not_gt (H hb.1 ha.1 hb.2 ha.2)) (le_of_not_gt (H ha.1 hb.1 ha.2 hb.2))


/-- With scale held, the fitted endpoint is the sample minimum. If all
positive residuals have one magnitude, at most one pair attains. -/
theorem held_scale_one_magnitude {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) {d s : ℝ} (hs : 0 < s)
    (hx : ∀ j, x j = x i ∨ x j = x i+d) :
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
    Set.Subsingleton {p ∈ D | IsMaxOn L D p} := by
  dsimp only
  let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
  let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
  have H (p : ℝ × ℝ) (hp : p ∈ D) (hm : IsMaxOn L D p) :
      p.1 = x i ∧ p.2 ∈ {v : ℝ | v ∈ Ioi 0 ∩ Ici 1 ∧
        IsMaxOn (fun u ↦ likelihood x (x i) u s) (Ioi 0 ∩ Ici 1) v} := by
    have hν : 0 < p.2 := zero_lt_one.trans_le hp.2
    obtain ⟨j,hj,H⟩ := StudentHalf.held_shape_scale_endpoint x hn hν hs
    have hj' : p.1 = x j := (H _).mp ⟨hp.1,fun b hb ↦
      @hm (b,p.2) (show (b,p.2) ∈ D from ⟨hb,hp.2⟩)⟩
    have he : p.1 = x i := hj'.trans (le_antisymm (hj i) (hi j))
    refine ⟨he,⟨hν,hp.2⟩,?_⟩
    intro q hq
    have hh := @hm (x i,q) (show (x i,q) ∈ D from ⟨hi,hq.2⟩)
    change likelihood x (x i) q s ≤ likelihood x p.1 p.2 s at hh
    change likelihood x (x i) q s ≤ likelihood x (x i) p.2 s
    simpa only [he] using hh
  intro p hp q hq
  have hp' := H p hp.1 hp.2
  have hq' := H q hq.1 hq.2
  exact Prod.ext (hp'.1.trans hq'.1.symm)
    ((held_endpoint_scale_one_magnitude x hn hs hx) hp'.2 hq'.2)

private theorem small_magnitude_score_positive {v Q : ℝ} (hv : 0 < v)
    (hQ0 : 0 ≤ Q) (hQ1 : Q ≤ 1) {w : ℝ} (hw : 0 ≤ w) :
    0 < deriv (oneMagnitudeLog Q w) v := by
  rw [(oneMagnitudeLog_derivative hv hQ0 w).deriv]
  exact half_pos (signed_kernel_integral (kernel_integral_eq_and_integrable hv hQ0 w).1
    (small_magnitude_kernel_positive hQ0 hQ1 hw))

/-- For one positive magnitude no held-endpoint scale-and-shape fit
attains. This is strict comparison at every finite shape. -/
theorem held_endpoint_one_magnitude_escape {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b r f : ℝ} (hr : 0 < r) (hx : ∀ i, x i = b+r)
    {q s : ℝ} (hq : 0 < q) (hs : 0 < s) (hf : f ≤ q) :
    ∃ v > q, f ≤ v ∧ likelihood x b q s < likelihood x b v r := by
  have hxne : ∀ i, x i ≠ b := fun i ↦ by rw [hx]; linarith
  have hcount : TDist.multiplicity x b = 0 := by simp [TDist.multiplicity,hxne]
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hex : 0 < TDist.exponent x b q := by
    unfold TDist.exponent
    rw [hcount]
    simp only [Nat.cast_zero, sub_zero]
    positivity
  have hScore : TDist.scaleScore x b q r = 0 := by
    have he (i : Fin n) : x i-b = r := by rw [hx]; ring
    have hq1 : q+1 ≠ 0 := by positivity
    have hrea : (q+1)*r^2/(q*r^2+r^2) = 1 := by field_simp <;> ring
    simp [TDist.scaleScore,he,hrea]
  have hbest := scale_max_of_score x b hq
    ⟨⟨0,hn⟩,hxne ⟨0,hn⟩⟩ hex hr hScore
  let v := q+1
  have hv : q < v := by dsimp [v]; linarith
  refine ⟨v,hv,hf.trans hv.le,(@hbest s hs).trans_lt ?_⟩
  have hR : ∀ i, x i = b ∨ x i = b+r := fun i ↦ Or.inr (hx i)
  rw [oneMagnitude_likelihood_eq x hn hr hq hR,
    oneMagnitude_likelihood_eq x hn hr (hq.trans hv) hR]
  simp only [hxne, ne_eq, not_false_eq_true, Finset.filter_true, Finset.card_univ,
    Fintype.card_fin, div_self (show r^2 ≠ 0 by positivity)]
  apply mul_lt_mul_of_pos_left _ (by positivity)
  apply exp_lt_exp.mpr
  apply mul_lt_mul_of_pos_left _ hn'
  apply sub_lt_sub_right
  let w := (n : ℝ)/n
  have hw : 0 ≤ w := by dsimp [w]; positivity
  apply strictMonoOn_of_deriv_pos (convex_Ioi (0 : ℝ))
    (f := oneMagnitudeLog 1 w) _ _ hq (hq.trans hv) hv
  · intro t ht
    exact (oneMagnitudeLog_derivative ht zero_le_one w).continuousAt.continuousWithinAt
  · intro t ht
    rw [interior_Ioi] at ht
    exact small_magnitude_score_positive ht zero_le_one le_rfl hw


private theorem log_normalizer_derivative {v : ℝ} (hv : 0 < v) :
    HasDerivAt TDist.logNormalizer (gammaLaplace v-1/v/2) v := by
  have hpi : HasDerivAt (fun u : ℝ ↦ log (u*π)/2) (1/v/2) v := by
    have hp' : 0 < v*π := mul_pos hv pi_pos
    have h := ((Real.hasDerivAt_log hp'.ne').comp v ((hasDerivAt_id v).mul_const π)).div_const 2
    convert h using 1 <;> simp <;> field_simp [hv.ne',pi_ne_zero]
  change HasDerivAt TDist.logNormalizer
    ((∫ t in Ioi (0 : ℝ), exp (-v*t)/(1+exp (-t)))-1/v/2) v
  convert (log_gamma_half_step_derivative hv).sub hpi using 1 <;> rfl

private theorem sigmoid_gap_le {t : ℝ} (ht : 0 ≤ t) :
    1/(1+exp (-t))-1/2 ≤ t/4 := by
  let G := fun z : ℝ ↦ z/4-(1/(1+exp (-z))-1/2)
  have h0 : G 0 = 0 := by norm_num [G]
  have hd (z : ℝ) : HasDerivAt G (1/4-exp (-z)/(1+exp (-z))^2) z := by
    have h := (hasDerivAt_const z (1:ℝ)).div
      ((((hasDerivAt_id z).neg).exp).const_add 1) (by positivity)
    have hh := ((hasDerivAt_id z).div_const 4).sub (h.sub_const (1/2))
    convert hh using 1 <;> simp [G,Pi.div_apply] <;> rfl
  have hm : MonotoneOn G (Ici 0) := by
    apply monotoneOn_of_deriv_nonneg (convex_Ici 0)
    · exact (continuous_iff_continuousAt.mpr fun z ↦ (hd z).continuousAt).continuousOn
    · intro z _
      exact (hd z).differentiableAt.differentiableWithinAt
    · intro z _
      rw [(hd z).deriv]
      apply sub_nonneg.mpr
      apply (div_le_iff₀ (by positivity)).mpr
      nlinarith [sq_nonneg (1-exp (-z))]
  have h := hm (show (0:ℝ) ∈ Ici 0 from le_rfl) (show t ∈ Ici 0 from ht) ht
  rw [h0] at h
  exact sub_nonneg.mp h

/-- The Student log normalizer has a strictly positive shape derivative
and the universal upper bound one over four times squared shape. -/
theorem student_log_normalizer_score_bounds {v : ℝ} (hv : 0 < v) :
    0 < deriv TDist.logNormalizer v ∧ deriv TDist.logNormalizer v ≤ 1/(4*v^2) := by
  have hExp := integrableOn_exp_mul_Ioi (a := -v) (neg_neg_of_pos hv) (0 : ℝ)
  have form : deriv TDist.logNormalizer v =
      ∫ t in Ioi (0 : ℝ), exp (-v*t)*(1/(1+exp (-t))-1/2) := by
    rw [(log_normalizer_derivative hv).deriv]
    have h (t : ℝ) : exp (-v*t)*(1/(1+exp (-t))-1/2) =
        laplaceKernel v t-(1/2)*exp (-v*t) := by unfold laplaceKernel; ring
    simp_rw [h]
    rw [integral_sub (laplaceKernel_integrable hv) (hExp.const_mul (1/2)),
      integral_const_mul, integral_exp_mul_Ioi (neg_neg_of_pos hv)]
    simp only [mul_zero,exp_zero,neg_div_neg_eq,gammaLaplace]
    ring
  rw [form]
  have hi : IntegrableOn (fun t ↦ exp (-v*t)*(1/(1+exp (-t))-1/2)) (Ioi 0) := by
    refine ((laplaceKernel_integrable hv).sub (hExp.const_mul (1/2))).congr ?_
    exact ae_of_all _ fun t ↦ by
      change laplaceKernel v t-(1/2)*exp (-v*t) = exp (-v*t)*(1/(1+exp (-t))-1/2)
      unfold laplaceKernel
      ring
  refine ⟨signed_kernel_integral hi fun t ht ↦ ?_,?_⟩
  · apply sub_pos.mpr
    apply (lt_div_iff₀ (by positivity)).mpr
    linarith [exp_lt_one_iff.mpr (neg_neg_of_pos ht)]
  have hi' : IntegrableOn (fun t : ℝ ↦ (t/4)*exp (-v*t)) (Ioi 0) := by
    have h := integrableOn_rpow_mul_exp_neg_mul_rpow (s := (1:ℝ)) (p := 1)
      (by norm_num : -1 < (1:ℝ)) zero_lt_one hv
    refine (h.div_const (4 : ℝ)).congr (ae_of_all _ fun t ↦ ?_)
    change (t^(1:ℝ)*exp (-v*t^(1:ℝ)))/4 = (t/4)*exp (-v*t)
    rw [rpow_one]
    ring
  calc
    _ ≤ ∫ t in Ioi (0 : ℝ), (t/4)*exp (-v*t) := by
      apply setIntegral_mono_on hi hi' measurableSet_Ioi
      intro t ht
      rw [mul_comm]
      exact mul_le_mul_of_nonneg_right (sigmoid_gap_le ht.le) (exp_pos _).le
    _ = 1/(4*v^2) := by
      have h := integral_rpow_mul_exp_neg_mul_Ioi (a := (2:ℝ)) (r := v) (by norm_num) hv
      norm_num [Gamma_two, rpow_natCast] at h
      calc
        _ = (∫ t in Ioi (0 : ℝ), t*exp (-(v*t)))/4 := by
          rw [← integral_div]
          apply integral_congr_ae (ae_of_all _ fun t ↦ ?_)
          change (t/4)*exp (-v*t) = (t*exp (-(v*t)))/4
          rw [neg_mul]
          ring
        _ = _ := by rw [h]; field_simp

private theorem kernelSlope_at_zero {Q : ℝ} (hQ : 0 ≤ Q) (w : ℝ) :
    kernelSlope Q w 0 = (1-w*(Q^2-2*Q))/2 := by
  unfold kernelSlope
  simp only [mul_zero,neg_zero,exp_zero,mul_one]
  have h : (∫ u in Icc (0:ℝ) Q, u) = Q^2/2 := by
    rw [integral_Icc_eq_integral_Ioc,← intervalIntegral.integral_of_le hQ]
    rw [integral_id]
    ring
  rw [h]
  ring

private theorem magnitudeKernel_pos_of_moment {Q w : ℝ} (hQ : 0 ≤ Q) (hw : 0 ≤ w)
    (hmom : w*(Q^2-2*Q) ≤ 1) : ∀ t > 0, 0 < magnitudeKernel Q w t := by
  rcases le_or_gt Q 1 with h | h
  · exact small_magnitude_kernel_positive hQ h hw
  intro t ht
  obtain ⟨u,hu,hus⟩ := magnitudeKernel_mvt hQ w ht
  have hm := weightedSlope_strictMono h hw hu.1
  have hb : 0 ≤ kernelSlope Q w 0 := by rw [kernelSlope_at_zero hQ]; linarith
  have hmul : 0 < exp (Q*u)*kernelSlope Q w u :=
    (mul_nonneg (exp_pos _).le hb).trans_lt hm
  have hp := pos_of_mul_pos_right hmul (exp_pos _).le
  rw [hus,magnitudeKernel_zero hQ,sub_zero] at hp
  exact (div_pos_iff_of_pos_right (sub_pos.mpr ht)).mp hp

private theorem initially_negative_kernel {Q w : ℝ} (hQ : 0 ≤ Q)
    (hmom : 1 < w*(Q^2-2*Q)) :
    ∃ δ > 0, ∀ t ∈ Ioo 0 δ, magnitudeKernel Q w t < 0 := by
  have hD := magnitudeKernel_derivative hQ w 0
  have hb : kernelSlope Q w 0 < 0 := by rw [kernelSlope_at_zero hQ]; linarith
  have hS := hasDerivAt_iff_tendsto_slope.mp hD
  have he : ∀ᶠ t : ℝ in 𝓝[>] 0, (magnitudeKernel Q w t-magnitudeKernel Q w 0)/(t-0) < 0 := by
    have ht : 𝓝[>] (0:ℝ) ≤ 𝓝[≠] 0 :=
      nhdsWithin_mono _ (fun t ht ↦ show t ≠ 0 from (mem_Ioi.mp ht).ne')
    exact ((hS.mono_left ht).eventually (Iio_mem_nhds hb)).mono (fun t ht ↦ by
      simpa [slope_def_field] using ht)
  obtain ⟨δ,hδ,hε⟩ := Metric.mem_nhdsWithin_iff.mp he
  refine ⟨δ,hδ,fun t ht ↦ ?_⟩
  have hz := hε ⟨by
    rw [Metric.mem_ball,Real.dist_eq,sub_zero,abs_of_pos ht.1]
    exact ht.2,ht.1⟩
  change (magnitudeKernel Q w t-magnitudeKernel Q w 0)/(t-0) < 0 at hz
  rw [magnitudeKernel_zero hQ,sub_zero,sub_zero] at hz
  simpa using (div_lt_iff₀ ht.1).mp hz

private theorem eventually_negative_laplace {K : ℝ → ℝ} {δ : ℝ} (hδ : 0 < δ)
    (hneg : ∀ t ∈ Ioo (0:ℝ) δ, K t < 0)
    (hi : ∀ v > 0, IntegrableOn (fun t ↦ exp (-v*t)*K t) (Ioi 0)) :
    ∀ᶠ v : ℝ in Filter.atTop, (∫ t in Ioi (0:ℝ), exp (-v*t)*K t) < 0 := by
  let μ := volume.restrict (Ioi (0:ℝ))
  let g := fun t : ℝ ↦ exp (-t)*K t
  let U := Ioo (δ/4) (δ/2)
  let V := Ici δ
  let C := -(∫ t in U, g t ∂μ)
  let D := ∫ t in V, |g t| ∂μ
  have hU : MeasurableSet U := measurableSet_Ioo
  have hV : MeasurableSet V := measurableSet_Ici
  have iG : Integrable g μ := by
    apply Integrable.congr (hi 1 zero_lt_one)
    exact ae_of_all _ fun t ↦ by change exp (-(1:ℝ)*t)*K t = g t; simp [g]
  have hC : 0 < C := by
    unfold C
    rw [← integral_neg]
    have hInt : IntegrableOn (fun t ↦ -g t) U μ := iG.neg.integrableOn
    have hP (t : ℝ) (ht : t ∈ U) : 0 < -g t := by
      apply neg_pos.mpr
      exact mul_neg_of_pos_of_neg (exp_pos _) (hneg t ⟨by linarith [ht.1],by linarith [ht.2]⟩)
    rw [integral_pos_iff_support_of_nonneg_ae _ hInt]
    · have hs : U ⊆ Function.support (fun t ↦ -g t) := fun t ht ↦ (hP t ht).ne'
      apply lt_of_lt_of_le _ (measure_mono hs)
      rw [Measure.restrict_apply hU,inter_self]
      unfold μ U
      rw [Measure.restrict_apply measurableSet_Ioo,
        inter_eq_left.mpr (show Ioo (δ/4) (δ/2) ⊆ Ioi (0:ℝ) from
          fun t ht ↦ (by linarith : 0 < δ/4).trans ht.1),Real.volume_Ioo]
      exact ENNReal.ofReal_pos.mpr (by linarith)
    · exact (ae_restrict_iff' hU).mpr (ae_of_all _ fun t ht ↦ (hP t ht).le)
  have hb {v : ℝ} (hv : 1 ≤ v) :
      (∫ t in Ioi (0:ℝ), exp (-v*t)*K t) ≤
        -(exp (-(v-1)*(δ/2)))*C + exp (-(v-1)*δ)*D := by
    let a := exp (-(v-1)*(δ/2))
    let b := exp (-(v-1)*δ)
    let H := fun t ↦ a*U.indicator g t+b*V.indicator (fun t ↦ |g t|) t
    have iU : Integrable (U.indicator g) μ := iG.indicator hU
    have iV : Integrable (V.indicator (fun t ↦ |g t|)) μ := iG.abs.indicator hV
    have iH : Integrable H μ := (iU.const_mul a).add (iV.const_mul b)
    have hExp (t : ℝ) : exp (-v*t)*K t = exp (-(v-1)*t)*g t := by
      unfold g
      rw [← mul_assoc,← exp_add]
      congr 2
      ring
    have h1 : (∫ t, exp (-v*t)*K t ∂μ) ≤ ∫ t, H t ∂μ := by
      apply integral_mono_ae (hi v (zero_lt_one.trans_le hv)) iH
      apply (ae_restrict_iff' measurableSet_Ioi).mpr (ae_of_all _ _)
      intro t ht
      change exp (-v*t)*K t ≤ H t
      rw [hExp]
      unfold H
      by_cases hT : δ ≤ t
      · have hV' : t ∈ V := hT
        have hU' : t ∉ U := fun h ↦ by linarith [h.2]
        rw [indicator_of_notMem hU',indicator_of_mem hV',mul_zero,zero_add]
        have hb' : exp (-(v-1)*t) ≤ b := exp_le_exp.mpr
          (mul_le_mul_of_nonpos_left hT (by linarith))
        exact (mul_le_mul_of_nonneg_left (le_abs_self _) (exp_pos _).le).trans
          (mul_le_mul_of_nonneg_right hb' (abs_nonneg _))
      · have hV' : t ∉ V := hT
        have ht' : 0 < t := ht
        have hg : g t < 0 := mul_neg_of_pos_of_neg (exp_pos _) (hneg t ⟨ht',lt_of_not_ge hT⟩)
        rw [indicator_of_notMem hV',mul_zero,add_zero]
        by_cases hU' : t ∈ U
        · rw [indicator_of_mem hU']
          have ha' : a ≤ exp (-(v-1)*t) := exp_le_exp.mpr
            (mul_le_mul_of_nonpos_left hU'.2.le (by linarith))
          exact mul_le_mul_of_nonpos_right ha' hg.le
        · rw [indicator_of_notMem hU',mul_zero]
          exact (mul_neg_of_pos_of_neg (exp_pos _) hg).le
    change _ ≤ -a*C+b*D
    refine h1.trans (le_of_eq ?_)
    unfold H
    rw [integral_add (iU.const_mul a) (iV.const_mul b),integral_const_mul,integral_const_mul,
      integral_indicator hU,integral_indicator hV]
    unfold C D
    ring
  have hE : Tendsto (fun v : ℝ ↦ exp (-(v-1)*(δ/2))*D) atTop (𝓝 0) := by
    have hV : Tendsto (fun v : ℝ ↦ v-1) atTop atTop := by
      simpa [sub_eq_add_neg] using tendsto_atTop_add_const_right atTop (-1:ℝ) tendsto_id
    have hh : Tendsto (fun v : ℝ ↦ -(v-1)*(δ/2)) atTop atBot := by
      simpa only [neg_mul,mul_neg] using hV.atTop_mul_const_of_neg (show -(δ/2) < 0 by linarith)
    simpa only [Function.comp_apply,zero_mul] using (tendsto_exp_atBot.comp hh).mul_const D
  filter_upwards [eventually_ge_atTop (1:ℝ),hE.eventually (Iio_mem_nhds hC)] with v hv he
  apply (hb hv).trans_lt
  have hf : exp (-(v-1)*δ) = exp (-(v-1)*(δ/2))*exp (-(v-1)*(δ/2)) := by
    rw [← exp_add]
    congr 1
    ring
  rw [hf]
  rw [show -(exp (-(v-1)*(δ/2)))*C +
      rexp (-(v-1)*(δ/2))*rexp (-(v-1)*(δ/2))*D =
      exp (-(v-1)*(δ/2))*(exp (-(v-1)*(δ/2))*D-C) by ring]
  exact mul_neg_of_pos_of_neg (exp_pos _) (sub_neg.mpr he)


private theorem oneMagnitudeLog_closed_floor_exists {Q w f : ℝ} (hQ : 0 ≤ Q)
    (hw : 0 ≤ w) (hf : 0 < f) :
    (∃ v ∈ Ici f, IsMaxOn (oneMagnitudeLog Q w) (Ici f) v) ↔ 1 < w*(Q^2-2*Q) := by
  have hd (v : ℝ) (hv : 0 < v) := oneMagnitudeLog_derivative hv hQ w
  constructor
  · rintro ⟨v,hv,hm⟩
    by_contra! h
    have hM : StrictMonoOn (oneMagnitudeLog Q w) (Ioi 0) := by
      apply strictMonoOn_of_deriv_pos (convex_Ioi 0)
      · exact fun x hx ↦ (hd x hx).continuousAt.continuousWithinAt
      · intro x hx
        rw [interior_Ioi] at hx
        rw [(hd x hx).deriv]
        exact half_pos (signed_kernel_integral (kernel_integral_eq_and_integrable hx hQ w).1
          (magnitudeKernel_pos_of_moment hQ hw h))
    have hv0 : 0 < v := hf.trans_le hv
    have hh : v < v+1 := by linarith
    exact (hM hv0 (hv0.trans hh) hh).not_ge (@hm (v+1) (by exact hv.trans hh.le))
  · intro hm
    obtain ⟨δ,hδ,hneg⟩ := initially_negative_kernel hQ hm
    have hT := eventually_negative_laplace hδ hneg
      (fun _ hv ↦ (kernel_integral_eq_and_integrable hv hQ w).1)
    obtain ⟨B,hB⟩ := eventually_atTop.mp hT
    let C := max f B
    have hfc : f ≤ C := le_max_left _ _
    have hBC : B ≤ C := le_max_right _ _
    have hpos : 0 < C := hf.trans_le hfc
    have hm : StrictAntiOn (oneMagnitudeLog Q w) (Ici C) := by
      apply strictAntiOn_of_deriv_neg (convex_Ici C)
      · exact fun x hx ↦ (hd x (hpos.trans_le hx)).continuousAt.continuousWithinAt
      · intro x hx
        rw [interior_Ici] at hx
        rw [(hd x (hpos.trans hx)).deriv]
        exact div_neg_of_neg_of_pos (hB x (hBC.trans hx.le)) (by norm_num)
    have hcont : ContinuousOn (oneMagnitudeLog Q w) (Icc f C) := fun x hx ↦
      (hd x (hf.trans_le hx.1)).continuousAt.continuousWithinAt
    obtain ⟨p,hp,hpbest⟩ := isCompact_Icc.exists_isMaxOn ⟨f,le_rfl,hfc⟩ hcont
    refine ⟨p,hp.1,?_⟩
    intro t ht
    rcases le_total t C with h | h
    · exact @hpbest t ⟨ht,h⟩
    · exact (hm.antitoneOn (show C ∈ Ici C from le_rfl) (show t ∈ Ici C from h) h).trans
        (@hpbest C ⟨hfc,le_rfl⟩)

/-- On every positive shape floor, a held-endpoint held-scale half-t
with at most one positive magnitude attains exactly above the moment
threshold, and the maximizer is unique. Equality is an escape. -/
theorem held_endpoint_scale_moment_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {b d s f : ℝ} (hs : 0 < s) (hf : 0 < f) (hx : ∀ i, x i = b ∨ x i = b+d) :
    let Q := d^2/s^2
    let w := ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ)/n
    (∃! v, v ∈ Ici f ∧ IsMaxOn (fun u ↦ likelihood x b u s) (Ici f) v) ↔ 1 < w*(Q^2-2*Q) := by
  dsimp only
  let Q := d^2/s^2
  let w := ((Finset.univ.filter (fun i ↦ x i ≠ b)).card : ℝ)/n
  have hQ : 0 ≤ Q := by dsimp [Q]; positivity
  have hw : 0 ≤ w := by dsimp [w]; positivity
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have ht : Ioi (0 : ℝ) ∩ Ici f = Ici f := inter_eq_right.mpr fun _ hv ↦ hf.trans_le hv
  have H (v : ℝ) (hv : v ∈ Ici f) :
      IsMaxOn (fun u ↦ likelihood x b u s) (Ici f) v ↔ IsMaxOn (oneMagnitudeLog Q w) (Ici f) v := by
    rw [isMaxOn_iff,isMaxOn_iff]
    apply forall₂_congr
    intro t h
    rw [oneMagnitude_likelihood_eq x hn hs (hf.trans_le h) hx,
      oneMagnitude_likelihood_eq x hn hs (hf.trans_le hv) hx,
      mul_le_mul_iff_right₀ (by positivity : 0 < (2 : ℝ)^n), exp_le_exp,
      mul_le_mul_iff_right₀ hn', sub_le_sub_iff_right]
  constructor
  · rintro ⟨v,⟨hv,hm⟩,_⟩
    exact (oneMagnitudeLog_closed_floor_exists hQ hw hf).mp ⟨v,hv,(H v hv).mp hm⟩
  · intro h
    obtain ⟨v,hv,hm⟩ := (oneMagnitudeLog_closed_floor_exists hQ hw hf).mpr h
    refine ⟨v,⟨hv,(H v hv).mpr hm⟩,?_⟩
    intro t ht'
    have hs := held_endpoint_scale_one_magnitude x hn (f := f) hs hx
    rw [ht] at hs
    exact hs ht' ⟨hv,(H v hv).mpr hm⟩


/-- Sum of the squared-residual fractions on the scale-score locus. -/
noncomputable def residualWeight {n : ℕ} (x : Fin n → ℝ) (b u : ℝ) : ℝ :=
  ∑ i, (x i-b)^2/(u+(x i-b)^2)

/-- Shape expressed as a function of the squared Student kernel width
along the scale-score locus. -/
noncomputable def kernelShape {n : ℕ} (x : Fin n → ℝ) (b u : ℝ) : ℝ :=
  n/residualWeight x b u-1

/-- Half-t likelihood along the scale-score locus in squared width. -/
noncomputable def kernelProfile {n : ℕ} (x : Fin n → ℝ) (b u : ℝ) : ℝ :=
  likelihood x b (kernelShape x b u) (sqrt (u/kernelShape x b u))

private theorem residualWeight_bounds {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) {u : ℝ} (hu : 0 < u) :
    0 < residualWeight x b u ∧
      residualWeight x b u < (n : ℝ)-TDist.multiplicity x b := by
  classical
  obtain ⟨j,hj⟩ := hx
  have h1 : 0 < (x j-b)^2/(u+(x j-b)^2) := by positivity
  constructor
  · unfold residualWeight
    exact h1.trans_le (Finset.single_le_sum (f := fun i : Fin n ↦ (x i-b)^2/(u+(x i-b)^2))
      (fun i _ ↦ by positivity) (Finset.mem_univ j))
  have h2 (i : Fin n) : (x i-b)^2/(u+(x i-b)^2) ≤ if x i=b then (0:ℝ) else 1 := by
    by_cases hi : x i = b
    · simp [hi]
    · rw [ite_eq_right hi]
      exact (div_le_one (by positivity)).2 (by linarith)
  have h3 : (x j-b)^2/(u+(x j-b)^2) < if x j=b then (0:ℝ) else 1 := by
    rw [ite_eq_right hj]
    exact (div_lt_one (by positivity)).2 (by linarith)
  have hC : (∑ i : Fin n, if x i=b then (0:ℝ) else 1) =
      (n : ℝ)-TDist.multiplicity x b := by
    have h4 := Finset.card_filter_add_card_filter_not (s := (Finset.univ : Finset (Fin n)))
      (p := fun i ↦ x i = b)
    simp only [Finset.card_univ,Fintype.card_fin] at h4
    rw [Finset.sum_ite,Finset.sum_const_zero,Finset.sum_const,zero_add,nsmul_eq_mul,mul_one]
    have h4' := congrArg (fun k : ℕ ↦ (k : ℝ)) h4
    push_cast at h4'
    unfold TDist.multiplicity
    linarith
  rw [← hC]
  exact Finset.sum_lt_sum (fun i _ ↦ h2 i) ⟨j,Finset.mem_univ j,h3⟩

/-- The positive-width chart gives the unique optimal scale at its
shape and attains the exact profiled half-t value. -/
theorem kernelProfile_eq_scaleProfile {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) {u : ℝ} (hu : 0 < u) :
    let v := kernelShape x b u
    let s := sqrt (u/v)
    0 < v ∧ 0 < s ∧ TDist.scaleScore x b v s = 0 ∧ 0 < TDist.exponent x b v ∧
      kernelProfile x b u = scaleProfile x b v := by
  dsimp only
  let v := kernelShape x b u
  let s := sqrt (u/v)
  have H := residualWeight_bounds x b hx hu
  let W := residualWeight x b u
  have hn : 0 < (n : ℝ) := by obtain ⟨i,_⟩ := hx; exact_mod_cast i.isLt.pos
  have hv1 : v+1 = n/W := by unfold v kernelShape W; ring
  have hv : 0 < v := by
    unfold v kernelShape
    have hh : W < n := H.2.trans_le (sub_le_self _ (Nat.cast_nonneg _))
    exact sub_pos.mpr ((lt_div_iff₀ H.1).2 (by simpa using hh))
  have hs : 0 < s := by unfold s; positivity
  have hwidth : v*s^2 = u := by unfold s; rw [sq_sqrt (by positivity)]; field_simp
  have hstat : TDist.scaleScore x b v s = 0 := by
    unfold TDist.scaleScore
    rw [hwidth]
    simp_rw [mul_div_assoc,← Finset.mul_sum]
    change (v+1)*W-n = 0
    rw [hv1,div_mul_cancel₀ _ (show W ≠ 0 from H.1.ne'),sub_self]
  have hExp : 0 < TDist.exponent x b v := by
    have hh := mul_lt_mul_of_pos_left H.2 (div_pos hn H.1)
    have hC : ((n:ℝ)/W)*W = n := div_mul_cancel₀ _ H.1.ne'
    rw [hC,← hv1] at hh
    unfold TDist.exponent
    linarith
  have hopt := scale_max_of_score x b hv hx hExp hs hstat
  exact ⟨hv,hs,hstat,hExp,(optimum_scale_profile x b hs hopt).symm⟩

private theorem real_Gamma_analytic {q : ℝ} (hq : 0 < q) :
    AnalyticAt ℝ Gamma q := by
  have h : AnalyticAt ℂ Complex.Gamma (q : ℂ) := by
    apply Complex.analyticAt_iff_eventually_differentiableAt.mpr
    have hC : Continuous (fun z : ℂ ↦ z.re) := Complex.continuous_re
    filter_upwards [hC.continuousAt (Ioi_mem_nhds (show 0 < (q:ℂ).re by exact hq))] with z hz
    apply Complex.differentiableAt_Gamma
    intro k hh
    have hh' := congrArg Complex.re hh
    simp only [Complex.neg_re,Complex.natCast_re] at hh'
    exact (not_lt_of_ge (show z.re ≤ 0 by rw [hh']; exact neg_nonpos.mpr (Nat.cast_nonneg k))) hz
  exact h.re_ofReal

/-- The location-held half-t scale profile is real analytic in the
positive squared-width coordinate along its score locus. -/
theorem kernelProfile_analytic {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) :
    AnalyticOnNhd ℝ (kernelProfile x b) (Ioi 0) := by
  intro u hu
  have hu' : 0 < u := hu
  have hV := kernelProfile_eq_scaleProfile x b hx hu'
  let v := kernelShape x b
  let s := fun t ↦ sqrt (t/v t)
  have hv : 0 < v u := hV.1
  have hs : 0 < s u := hV.2.1
  have hA : AnalyticAt ℝ v u := by
    have hW : AnalyticAt ℝ (residualWeight x b) u := by
      unfold residualWeight
      fun_prop (disch := positivity)
    exact (analyticAt_const.div hW (residualWeight_bounds x b hx hu').1.ne').sub analyticAt_const
  have hLogQ : AnalyticAt ℝ (fun t ↦ log (t/v t)) u :=
    (analyticAt_id.div hA hv.ne').log (show 0 < u/v u from div_pos hu' hv)
  have hS : AnalyticAt ℝ s u := by
    have h : AnalyticAt ℝ (fun t ↦ exp (log (t/v t)*(1/2))) u :=
      (hLogQ.mul analyticAt_const).rexp'
    apply h.congr
    filter_upwards [Ioi_mem_nhds hu', hA.continuousAt (Ioi_mem_nhds hv)] with t ht hv'
    have hp : 0 < t/v t := div_pos ht hv'
    unfold s
    rw [sqrt_eq_rpow,rpow_def_of_pos hp]
  have hN : AnalyticAt ℝ (TDist.logNormalizer ∘ v) u := by
    change AnalyticAt ℝ (fun t ↦ log (Gamma ((v t+1)/2)) - log (Gamma (v t/2)) -
      log (v t*π)/2) u
    have h1 : AnalyticAt ℝ (fun t ↦ (v t+1)/2) u := (hA.add analyticAt_const).div_const
    have h2 : AnalyticAt ℝ (fun t ↦ v t/2) u := hA.div_const
    have hl1 := ((real_Gamma_analytic (show 0 < (v u+1)/2 by positivity)).comp (f := fun t ↦ (v t+1)/2) h1).log
      (Gamma_pos_of_pos (by positivity))
    have hl2 := ((real_Gamma_analytic (half_pos hv)).comp (f := fun t ↦ v t/2) h2).log
      (Gamma_pos_of_pos (half_pos hv))
    have hl3 := (hA.mul analyticAt_const).log (mul_pos hv pi_pos)
    exact (hl1.sub hl2).sub hl3.div_const
  unfold kernelProfile likelihood TDist.likelihood TDist.logLik
  change AnalyticAt ℝ (fun t ↦ (2:ℝ)^n*exp (∑ i,
    (TDist.logNormalizer (v t)-log (s t)-
      (v t+1)/2*log (1+(x i-b)^2/(v t*(s t)^2))))) u
  have hg (i : Fin n) :
      AnalyticAt ℝ (fun t ↦ log (1+(x i-b)^2/(v t*(s t)^2))) u := by
    have hPos : 0 < v u*(s u)^2 := mul_pos hv (sq_pos_of_pos hs)
    have he : AnalyticAt ℝ (fun t ↦ (x i-b)^2/(v t*(s t)^2)) u :=
      analyticAt_const.div (hA.mul (hS.pow 2)) hPos.ne'
    exact (analyticAt_const.add he).log (zero_lt_one.trans_le
      (le_add_of_nonneg_right (div_nonneg (sq_nonneg _) hPos.le)))
  have hh (i : Fin n) : AnalyticAt ℝ (fun t ↦ TDist.logNormalizer (v t)-log (s t)-
      (v t+1)/2*log (1+(x i-b)^2/(v t*(s t)^2))) u :=
    (hN.sub (hS.log hs)).sub (((hA.add (analyticAt_const : AnalyticAt ℝ (fun _ : ℝ ↦ (1:ℝ)) u)).div_const (c := 2)).mul (hg i))
  exact analyticAt_const.mul (Finset.analyticAt_fun_sum Finset.univ (fun i _ ↦ hh i)).rexp'


/-- On any compact positive-width interval the stationary locus is
finite unless the complete analytic profile is constant. -/
theorem kernelProfile_compact_stationary {n : ℕ} (x : Fin n → ℝ) (b : ℝ)
    (hx : ∃ i, x i ≠ b) {a c : ℝ} (ha : 0 < a) :
    Set.Finite {u ∈ Icc a c | deriv (kernelProfile x b) u = 0} ∨
      ∀ u > 0, ∀ v > 0, kernelProfile x b u = kernelProfile x b v := by
  have hp := kernelProfile_analytic x b hx
  have hd : AnalyticOnNhd ℝ (deriv (kernelProfile x b)) (Ioi 0) :=
    fun z hz ↦ (hp z hz).deriv
  rcases hd.eqOn_zero_or_eventually_ne_zero_of_preconnected (isPreconnected_Ioi) with hz | hz
  · right
    have hD (u : ℝ) (hu : 0 < u) : HasDerivAt (kernelProfile x b) 0 u :=
      ((hp u hu).differentiableAt.hasDerivAt).congr_deriv (hz hu)
    have he {u v : ℝ} (hu : 0 < u) (huv : u < v) :
        kernelProfile x b u = kernelProfile x b v := by
      obtain ⟨z,_,hh⟩ := exists_hasDerivAt_eq_slope _ (fun _ ↦ 0) huv
        (fun z hz ↦ (hD z (hu.trans_le hz.1)).continuousAt.continuousWithinAt)
        (fun z hz ↦ hD z (hu.trans hz.1))
      exact (sub_eq_zero.mp ((div_eq_zero_iff.mp hh.symm).resolve_right
        (sub_ne_zero.mpr huv.ne'))).symm
    intro u hu v hv
    rcases lt_trichotomy u v with hh | rfl | hh
    · exact he hu hh
    · rfl
    · exact (he hv hh).symm
  · left
    have hsub : Icc a c ⊆ Ioi (0:ℝ) := fun u hu ↦ ha.trans_le hu.1
    have h := hz.filter_mono (Filter.codiscreteWithin_mono hsub)
    have hf := isCompact_Icc.finite_sdiff_of_mem_codiscreteWithin h
    convert hf using 1
    ext u
    simp [Set.mem_diff]


/-- At held scale, one or two data levels give an exact uniqueness or
escape decision after profiling the endpoint to the sample minimum. -/
theorem held_scale_moment_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {i : Fin n} (hi : ∀ j, x i ≤ x j) {d s : ℝ} (hs : 0 < s)
    (hx : ∀ j, x j = x i ∨ x j = x i+d) :
    let Q := d^2/s^2
    let w := ((Finset.univ.filter (fun j ↦ x j ≠ x i)).card : ℝ)/n
    let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
    let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
    (∃! p, p ∈ D ∧ IsMaxOn L D p) ↔ 1 < w*(Q^2-2*Q) := by
  dsimp only
  let D := {p : ℝ × ℝ | (∀ j, p.1 ≤ x j) ∧ 1 ≤ p.2}
  let L := fun p : ℝ × ℝ ↦ likelihood x p.1 p.2 s
  have endOpt (u : ℝ) (hu : 1 ≤ u) :
      ∀ m, ((∀ j, m ≤ x j) ∧ IsMaxOn (fun b ↦ likelihood x b u s) {b | ∀ j, b ≤ x j} m)
        ↔ m = x i := by
    obtain ⟨j,hj,H⟩ := StudentHalf.held_shape_scale_endpoint x hn (zero_lt_one.trans_le hu) hs
    rwa [show x j = x i from le_antisymm (hj i) (hi j)] at H
  have H (p : ℝ × ℝ) : (p ∈ D ∧ IsMaxOn L D p) ↔
      p.1 = x i ∧ 1 ≤ p.2 ∧ IsMaxOn (fun u ↦ likelihood x (x i) u s) (Ici 1) p.2 := by
    constructor
    · rintro ⟨hp,hm⟩
      have he : p.1 = x i := (endOpt p.2 hp.2 _).mp ⟨hp.1,fun b hb ↦
        @hm (b,p.2) (show (b,p.2) ∈ D from ⟨hb,hp.2⟩)⟩
      refine ⟨he,hp.2,?_⟩
      intro q hq
      have hh : likelihood x (x i) q s ≤ likelihood x p.1 p.2 s :=
        @hm (x i,q) (show (x i,q) ∈ D from ⟨hi,hq⟩)
      change likelihood x (x i) q s ≤ likelihood x (x i) p.2 s
      rwa [he] at hh
    · rintro ⟨hp,hq,hm⟩
      refine ⟨⟨by rw [hp]; exact hi,hq⟩,?_⟩
      intro q hq'
      have he := ((endOpt q.2 hq'.2 (x i)).mpr rfl).2
      change likelihood x q.1 q.2 s ≤ likelihood x p.1 p.2 s
      rw [hp]
      exact (@he q.1 hq'.1).trans (@hm q.2 hq'.2)
  have H₁ := held_endpoint_scale_moment_decision x hn hs (f := 1) zero_lt_one hx
  constructor
  · rintro ⟨p,hp,hUni⟩
    have hp' := (H p).mp hp
    apply H₁.mp
    refine ⟨p.2,⟨hp'.2.1,hp'.2.2⟩,?_⟩
    intro u hu
    exact congrArg Prod.snd (hUni (x i,u) ((H _).mpr ⟨rfl,hu.1,hu.2⟩))
  · intro e
    obtain ⟨u,hu,hUni⟩ := H₁.mpr e
    refine ⟨(x i,u),(H _).mpr ⟨rfl,hu.1,hu.2⟩,?_⟩
    intro p hp
    have hp' := (H p).mp hp
    exact Prod.ext hp'.1 (hUni p.2 ⟨hp'.2.1,hp'.2.2⟩)


private theorem residualWeight_noncontact {n : ℕ} (x : Fin n → ℝ) (b u : ℝ) :
    residualWeight x b u =
      ∑ i ∈ Finset.univ.filter (fun i ↦ x i ≠ b), (x i-b)^2/(u+(x i-b)^2) := by
  classical
  unfold residualWeight
  rw [Finset.sum_filter]
  apply Finset.sum_congr rfl
  intro i _
  by_cases hi : x i = b
  · simp [hi]
  · simp [hi]

private theorem noncontact_card {n : ℕ} (x : Fin n → ℝ) (b : ℝ) :
    (Finset.univ.filter (fun i ↦ x i ≠ b)).card + TDist.multiplicity x b = n := by
  classical
  have h := Finset.card_filter_add_card_filter_not (s := (Finset.univ : Finset (Fin n))) (p := fun i ↦ x i=b)
  simpa [TDist.multiplicity,add_comm] using h

/-- At exactly half contacts, the fitted shape one is the endpoint of
the analytic squared-width parameterization and its right slope is positive. -/
theorem critical_kernelShape_at_zero {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ)
    (hc : 2*TDist.multiplicity x b = n) :
    kernelShape x b 0 = 1 ∧ AnalyticAt ℝ (kernelShape x b) 0 ∧
      0 < deriv (kernelShape x b) 0 := by
  classical
  let S := Finset.univ.filter (fun i ↦ x i ≠ b)
  have hS : S.card + TDist.multiplicity x b = n := noncontact_card x b
  have hsN : 0 < S.card := by omega
  have hN : (0:ℝ) < n := by exact_mod_cast hn
  have hsR : (0:ℝ) < S.card := by exact_mod_cast hsN
  have hsc : (S.card : ℝ)*2 = n := by exact_mod_cast (show S.card*2 = n by omega)
  have he : residualWeight x b 0 = S.card := by
    rw [residualWeight_noncontact]
    change (∑ i ∈ S, (x i-b)^2/(0+(x i-b)^2)) = _
    calc
      _ = ∑ i ∈ S, (1:ℝ) := by
        apply Finset.sum_congr rfl
        intro i hi
        have hne : x i ≠ b := (Finset.mem_filter.mp hi).2
        rw [zero_add,div_self (by positivity)]
      _ = _ := by simp
  have hT (i : Fin n) (hi : i ∈ S) : (x i-b)^2 ≠ 0 := by
    have hx : x i ≠ b := (Finset.mem_filter.mp hi).2
    positivity
  have ha : AnalyticAt ℝ (residualWeight x b) 0 := by
    rw [show residualWeight x b = (fun u ↦ ∑ i ∈ S, (x i-b)^2/(u+(x i-b)^2)) from
      funext (residualWeight_noncontact x b)]
    apply Finset.analyticAt_fun_sum
    intro i hi
    exact analyticAt_const.div (by fun_prop) (by simpa using hT i hi)
  have hD (i : Fin n) (hi : i ∈ S) :
      HasDerivAt (fun u : ℝ ↦ (x i-b)^2/(u+(x i-b)^2)) (-1/(x i-b)^2) 0 := by
    have hh := (hasDerivAt_const (0:ℝ) ((x i-b)^2)).div
      ((hasDerivAt_id (0:ℝ)).add_const ((x i-b)^2)) (by simpa using hT i hi)
    convert hh using 1
    · rfl
    · simp only [id_eq,zero_add,zero_mul,mul_one,zero_sub]
      field_simp [hT i hi]
      <;> ring
  have hd : HasDerivAt (residualWeight x b) (∑ i ∈ S, -1/(x i-b)^2) 0 := by
    rw [show residualWeight x b = (fun u ↦ ∑ i ∈ S, (x i-b)^2/(u+(x i-b)^2)) from
      funext (residualWeight_noncontact x b)]
    exact HasDerivAt.fun_sum hD
  have hneg : (∑ i ∈ S, -1/(x i-b)^2) < 0 := by
    apply Finset.sum_neg
    · exact fun i hi ↦ div_neg_of_neg_of_pos (by norm_num) ((sq_nonneg _).lt_of_ne (Ne.symm (hT i hi)))
    · exact Finset.card_pos.mp hsN
  have hp : 0 < residualWeight x b 0 := he ▸ hsR
  refine ⟨?_,(analyticAt_const.div ha hp.ne').sub analyticAt_const,?_⟩
  · unfold kernelShape
    rw [he]
    have hN : (n:ℝ)/S.card = 2 := (div_eq_iff hsR.ne').2 (by linarith)
    rw [hN]
    norm_num
  · unfold kernelShape
    have H := (((hasDerivAt_const (0:ℝ) (n:ℝ)).div hd hp.ne').sub_const 1).deriv
    change deriv (fun u ↦ (n:ℝ)/residualWeight x b u-1) 0 = _ at H
    rw [H]
    simp only [zero_mul,zero_sub]
    exact div_pos (neg_pos.mpr (mul_neg_of_pos_of_neg hN hneg)) (by positivity)



/-- At critical half contact the derivative of the log profile in the
squared kernel width tends to minus infinity at collapse. -/
theorem critical_kernelProfile_log_derivative {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) (hc : 2*TDist.multiplicity x b = n) :
    Tendsto (deriv (fun u ↦ log (kernelProfile x b u))) (𝓝[>] 0) atBot := by
  classical
  let S := Finset.univ.filter (fun i ↦ x i ≠ b)
  let v := kernelShape x b
  have hS : S.card + TDist.multiplicity x b = n := noncontact_card x b
  have hSR : (S.card:ℝ)*2 = n := by exact_mod_cast (show S.card*2=n by omega)
  have hx : ∃ i, x i ≠ b := by
    obtain ⟨i,hi⟩ := Finset.card_pos.mp (show 0 < S.card by omega)
    exact ⟨i,(Finset.mem_filter.mp hi).2⟩
  obtain ⟨hV,hVa,hVd⟩ := critical_kernelShape_at_zero x hn b hc
  have hV' : v 0 = 1 := hV
  have hv : 0 < v 0 := hV'.symm ▸ zero_lt_one
  let A := fun u : ℝ ↦ (n:ℝ)*log 2 + (n:ℝ)*(TDist.logNormalizer (v u) + log (v u)/2)
    - (v u+1)/2 * ∑ i ∈ S, log (u+(x i-b)^2)
  let B := fun u : ℝ ↦ (n:ℝ)/4*(v u-1)
  have hA : AnalyticAt ℝ A 0 := by
    have h1 : AnalyticAt ℝ (fun t ↦ (v t+1)/2) 0 := (hVa.add analyticAt_const).div_const
    have h2 : AnalyticAt ℝ (fun t ↦ v t/2) 0 := hVa.div_const
    have hl1 := ((real_Gamma_analytic (show 0 < (v 0+1)/2 by positivity)).comp
      (f := fun t ↦ (v t+1)/2) h1).log (Gamma_pos_of_pos (by positivity))
    have hl2 := ((real_Gamma_analytic (half_pos hv)).comp
      (f := fun t ↦ v t/2) h2).log (Gamma_pos_of_pos (half_pos hv))
    have hl3 := (hVa.mul analyticAt_const).log (mul_pos hv pi_pos)
    have hN : AnalyticAt ℝ (TDist.logNormalizer ∘ v) 0 :=
      (hl1.sub hl2).sub hl3.div_const
    have hSum : AnalyticAt ℝ (fun u ↦ ∑ i ∈ S, log (u+(x i-b)^2)) 0 := by
      apply Finset.analyticAt_fun_sum
      intro i hi
      have hne : x i-b ≠ 0 := sub_ne_zero.mpr (Finset.mem_filter.mp hi).2
      exact (analyticAt_id.add analyticAt_const).log (by simpa using sq_pos_of_ne_zero hne)
    exact (analyticAt_const.add (analyticAt_const.mul (hN.add (hVa.log hv).div_const))).sub
      (h1.mul hSum)
  have hBd : HasDerivAt B ((n:ℝ)/4*deriv v 0) 0 :=
    (hVa.differentiableAt.hasDerivAt.sub_const 1).const_mul _
  have hBa : AnalyticAt ℝ B 0 := analyticAt_const.mul (hVa.sub analyticAt_const)
  have hB0 : B 0 = 0 := by simp [B,hV']
  have hBp : 0 < deriv B 0 := by
    rw [hBd.deriv]
    exact mul_pos (by exact_mod_cast (show 0 < (n:ℝ)/4 by positivity)) hVd
  have hEq (u : ℝ) (hu : 0 < u) : log (kernelProfile x b u) = A u+B u*log u := by
    let s := sqrt (u/v u)
    have hv' : 0 < v u := (kernelProfile_eq_scaleProfile x b hx hu).1
    have hQ : v u*s^2 = u := by
      unfold s
      rw [sq_sqrt (by positivity)]
      field_simp
    have hs : s ≠ 0 := (kernelProfile_eq_scaleProfile x b hx hu).2.1.ne'
    have hlogS : log s = (log u-log (v u))/2 := by
      unfold s
      rw [log_sqrt (by positivity),log_div hu.ne' hv'.ne']
    have hSplit : ∑ i, log (1+(x i-b)^2/u) =
        (∑ i ∈ S, log (u+(x i-b)^2))-(S.card:ℝ)*log u := by
      rw [← nsmul_eq_mul,← Finset.sum_const,← Finset.sum_sub_distrib]
      unfold S
      rw [Finset.sum_filter]
      apply Finset.sum_congr rfl
      intro i _
      by_cases hi : x i = b
      · simp [hi]
      · simp only [hi,if_false,if_true,ne_eq,not_false_eq_true]
        rw [← log_div (by positivity) hu.ne']
        congr 1
        field_simp
        <;> ring
    change log ((2:ℝ)^n * exp (TDist.logLik x b (v u) s)) = _
    rw [log_mul (by positivity) (exp_pos _).ne',log_pow,log_exp]
    unfold TDist.logLik
    simp_rw [hQ]
    rw [Finset.sum_sub_distrib,← Finset.mul_sum,Finset.sum_const,Finset.card_univ,Fintype.card_fin,
      nsmul_eq_mul,hSplit,hlogS]
    unfold A B
    rw [show (S.card:ℝ)=(n:ℝ)/2 by linarith]
    ring
  have hDeq : (fun u ↦ deriv (fun t ↦ log (kernelProfile x b t)) u) =ᶠ[𝓝[>] 0]
      (fun u ↦ deriv B u * log u + (deriv A u+B u/u)) := by
    have hnA : ∀ᶠ u in 𝓝 (0:ℝ), AnalyticAt ℝ A u := hA.eventually_analyticAt
    have hnB : ∀ᶠ u in 𝓝 (0:ℝ), AnalyticAt ℝ B u := hBa.eventually_analyticAt
    filter_upwards [self_mem_nhdsWithin,hnA.filter_mono nhdsWithin_le_nhds,
      hnB.filter_mono nhdsWithin_le_nhds] with u hu ha hb
    have hu' : 0 < u := hu
    have he := (ha.differentiableAt.hasDerivAt.add
      (hb.differentiableAt.hasDerivAt.mul (hasDerivAt_log hu'.ne'))).congr_of_eventuallyEq
      (by filter_upwards [Ioi_mem_nhds hu'] with t ht; exact hEq t ht)
    rw [he.deriv]
    simp only [div_eq_mul_inv]
    ring
  have hDl : Tendsto (deriv B) (𝓝[>] 0) (𝓝 (deriv B 0)) :=
    hBa.deriv.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
  have hAl : Tendsto (deriv A) (𝓝[>] 0) (𝓝 (deriv A 0)) :=
    hA.deriv.continuousAt.tendsto.mono_left nhdsWithin_le_nhds
  have hBl : Tendsto (fun u ↦ B u/u) (𝓝[>] 0) (𝓝 (deriv B 0)) := by
    simpa [hB0,div_eq_mul_inv,mul_comm] using
      (hBa.differentiableAt.hasDerivAt.tendsto_slope_zero_right)
  exact (hDl.pos_mul_atBot hBp tendsto_log_nhdsGT_zero).atBot_add (hAl.add hBl) |>.congr' hDeq.symm


private theorem sigmoid_gap_cubic_lower {t : ℝ} (ht : 0 ≤ t) :
    t/4-t^3/48 ≤ 1/(1+exp (-t))-1/2 := by
  let g := fun z : ℝ ↦ 1/(1+exp (-z))-1/2
  let G := fun z : ℝ ↦ g z-z/4+z^3/48
  have hg (z : ℝ) : HasDerivAt g (1/4-(g z)^2) z := by
    have h := ((hasDerivAt_const z (1:ℝ)).fun_div
      ((((hasDerivAt_id z).neg).exp).const_add 1) (by positivity)).sub_const (1/2)
    convert h using 1
    · rfl
    · unfold g
      simp only [Pi.neg_apply,id_eq,zero_mul,zero_sub,mul_one,neg_neg]
      field_simp
      ring
  have hd (z : ℝ) : HasDerivAt G (z^2/16-(g z)^2) z := by
    have h := ((hg z).sub ((hasDerivAt_id z).div_const 4)).add
      (((hasDerivAt_id z).pow 3).div_const 48)
    convert h using 1
    · rfl
    · simp only [id_eq]
      ring
  have hM : MonotoneOn G (Ici 0) := by
    apply monotoneOn_of_deriv_nonneg (convex_Ici _)
    · exact fun z _ ↦ (hd z).continuousAt.continuousWithinAt
    · exact fun z _ ↦ (hd z).differentiableAt.differentiableWithinAt
    · intro z hz
      have hz' : 0 ≤ z := interior_subset hz
      rw [(hd z).deriv]
      have hq : 0 ≤ g z := by
        unfold g
        have h := exp_le_one_iff.mpr (by linarith : -z ≤ 0)
        have H := (le_div_iff₀ (by positivity : 0 < 1+exp (-z))).2
          (show (1/2:ℝ)*(1+exp (-z)) ≤ 1 by linarith)
        linarith
      have h := pow_le_pow_left₀ hq (sigmoid_gap_le hz') 2
      linarith
  have h0 : G 0 = 0 := by norm_num [G,g]
  have h := hM (show (0:ℝ) ∈ Ici 0 from le_rfl) ht ht
  rw [h0] at h
  unfold G g at h
  linarith

/-- The cubic bound on the Student log-normalizer score makes the
large-shape expansion quantitative through degree four. -/
theorem student_log_normalizer_score_cubic_lower {q : ℝ} (hq : 0 < q) :
    1/(4*q^2)-1/(8*q^4) ≤ deriv TDist.logNormalizer q := by
  let f := fun t ↦ exp (-q*t)*(1/(1+exp (-t))-1/2)
  have hExp := integrableOn_exp_mul_Ioi (a := -q) (neg_neg_of_pos hq) (0 : ℝ)
  have hf : IntegrableOn f (Ioi 0) := by
    refine ((laplaceKernel_integrable hq).sub (hExp.const_mul (1/2))).congr ?_
    exact ae_of_all _ fun t ↦ by
      unfold f laplaceKernel
      simp only [Pi.sub_apply,neg_mul]
      ring
  have hEq : deriv TDist.logNormalizer q = ∫ t in Ioi 0, f t := by
    rw [(log_normalizer_derivative hq).deriv]
    have h (t : ℝ) : f t = laplaceKernel q t-(1/2)*exp (-q*t) := by unfold f laplaceKernel; ring
    simp_rw [h]
    rw [integral_sub (laplaceKernel_integrable hq) (hExp.const_mul (1/2)),
      integral_const_mul, integral_exp_mul_Ioi (neg_neg_of_pos hq)]
    simp only [mul_zero,exp_zero,neg_div_neg_eq,gammaLaplace]
    ring
  have h1 : IntegrableOn (fun t : ℝ ↦ t*exp (-(q*t))) (Ioi 0) := by
    simpa only [rpow_one,neg_mul] using
      (integrableOn_rpow_mul_exp_neg_mul_rpow (s := (1:ℝ)) (p := 1) (by norm_num) zero_lt_one hq)
  have h3 : IntegrableOn (fun t : ℝ ↦ t^3*exp (-(q*t))) (Ioi 0) := by
    simpa only [rpow_one,neg_mul,show (3:ℝ)=(3:ℕ) by norm_num,rpow_natCast] using
      (integrableOn_rpow_mul_exp_neg_mul_rpow (s := (3:ℝ)) (p := 1) (by norm_num) zero_lt_one hq)
  have H1 : (∫ t : ℝ in Ioi 0, t*exp (-(q*t))) = q⁻¹^2 := by
    have h := integral_rpow_mul_exp_neg_mul_Ioi (a := (2:ℝ)) (r := q) (by norm_num) hq
    norm_num [rpow_natCast,Gamma_two,one_div] at h ⊢
    exact h
  have H3 : (∫ t : ℝ in Ioi 0, t^3*exp (-(q*t))) = q⁻¹^4*6 := by
    have h := integral_rpow_mul_exp_neg_mul_Ioi (a := (4:ℝ)) (r := q) (by norm_num) hq
    norm_num [rpow_natCast,Gamma_nat_eq_factorial] at h ⊢
    convert h using 1
  rw [hEq]
  calc
    _ = ∫ t : ℝ in Ioi 0, t*exp (-(q*t))/4-t^3*exp (-(q*t))/48 := by
      rw [integral_sub (h1.div_const _) (h3.div_const _),integral_div,integral_div,H1,H3]
      field_simp
      ring
    _ ≤ _ := by
      apply setIntegral_mono_on ((h1.div_const _).sub (h3.div_const _)) hf measurableSet_Ioi
      intro t ht
      have h := mul_le_mul_of_nonneg_right (sigmoid_gap_cubic_lower ht.le) (exp_pos (-q*t)).le
      unfold f
      change t*exp (-(q*t))/4-t^3*exp (-(q*t))/48 ≤
        exp (-q*t)*(1/(1+exp (-t))-1/2)
      simp only [neg_mul] at h ⊢
      convert h using 1 <;> ring


/-- Scale collapse gives divergence exactly in the three half-Student
regimes listed by the atlas detector. Each forward implication gives
an admissible fixed-shape path at the appropriate endpoint. -/
theorem scale_collapse_divergence_detector {n : ℕ} (x : Fin n → ℝ) {i : Fin n}
    (hi : ∀ j, x i ≤ x j) {v b : ℝ} (hv : 0 < v) (hb : ∀ j, b ≤ x j) :
    let H := SkewLSHalfT.likelihood x
    let k := TDist.multiplicity x (x i)
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ H p.1 v p.2) ''
        {p | (∀ j, p.1 ≤ x j) ∧ 0 < p.2})) ↔
      TDist.exponent x (x i) v < 0) ∧
    (TDist.exponent x (x i) v < 0 → Tendsto (H (x i) v) (𝓝[>] 0) atTop) ∧
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ H b p.1 p.2) '' (Ioi 0 ×ˢ Ioi 0))) ↔
      ∃ j, x j = b) ∧
    ((∃ j, x j = b) → ∃ q > 0, Tendsto (H b q) (𝓝[>] 0) atTop) ∧
    ((¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ H p.1 p.2.1 p.2.2) '' FinishHalfT.domain x)) ↔
      n < 2*k) ∧
    (n < 2*k → Tendsto (H (x i) 1) (𝓝[>] 0) atTop) := by
  dsimp only
  classical
  refine ⟨StudentHalf.held_shape_unbounded_iff x hi hv,
    (StudentHalf.held_shape_collapse x hi hv).1,
    (Student2MovingCollapse.half_held_endpoint_failure.{0,0,0,0,0} x (Nat.zero_lt_of_lt i.isLt) b hb).1,
    ?_,FinishHalfT.all_free_unbounded_iff x hi,SkewLSHalfT.majority_at_floor x⟩
  rintro ⟨j,hj⟩
  apply SkewLSHalfT.held_endpoint_free_shape_unbounded x b
  apply Finset.card_pos.mpr
  exact ⟨j,Finset.mem_filter.mpr ⟨Finset.mem_univ _,hj⟩⟩


/-- The collapse and finite-width parts of stationary-list finiteness.
The only additional input is exclusion of stationary points for all
sufficiently large squared widths on the normal end. -/
theorem all_free_stationary_finite_of_normal_collar {n : ℕ} (x : Fin n → ℝ)
    (hn : 0 < n) (b : ℝ) (hc : 2*TDist.multiplicity x b ≤ n)
    (hTail : ∀ᶠ u in atTop, deriv (kernelProfile x b) u ≠ 0) :
    Set.Finite {u : ℝ | 0 < u ∧ 1 ≤ kernelShape x b u ∧ deriv (kernelProfile x b) u = 0} := by
  classical
  let S := Finset.univ.filter (fun i ↦ x i ≠ b)
  have hS : S.card + TDist.multiplicity x b = n := noncontact_card x b
  have hSp : 0 < S.card := by omega
  obtain ⟨i,hi⟩ := Finset.card_pos.mp hSp
  have hX : ∃ i, x i ≠ b := ⟨i,(Finset.mem_filter.mp hi).2⟩
  have lower : ∃ ε > 0, ∀ u : ℝ, 0 < u → u < ε →
      kernelShape x b u < 1 ∨ deriv (kernelProfile x b) u ≠ 0 := by
    by_cases hEq : 2*TDist.multiplicity x b = n
    · have h := (critical_kernelProfile_log_derivative x hn b hEq).eventually_lt_atBot 0
      rw [eventually_nhdsWithin_iff] at h
      obtain ⟨ε,hε,hBall⟩ := Metric.mem_nhds_iff.mp h
      refine ⟨ε,hε,fun u hu huε ↦ Or.inr ?_⟩
      have ht : deriv (fun v ↦ log (kernelProfile x b v)) u < 0 :=
        hBall (by simpa [Metric.mem_ball,Real.dist_eq,abs_of_pos hu] using huε) hu
      have hU := (kernelProfile_analytic x b hX u (mem_Ioi.mpr hu)).differentiableAt.hasDerivAt
      have hp : 0 < kernelProfile x b u := by
        unfold kernelProfile likelihood TDist.likelihood
        positivity
      rw [(hU.log hp.ne').deriv] at ht
      intro he
      rw [he,zero_div] at ht
      exact (lt_irrefl _) ht
    · have hE : residualWeight x b = (fun u ↦ ∑ i ∈ S, (x i-b)^2/(u+(x i-b)^2)) :=
        funext (residualWeight_noncontact x b)
      have h0 : residualWeight x b 0 = S.card := by
        rw [hE]
        change (∑ i ∈ S, (x i-b)^2/(0+(x i-b)^2)) = _
        calc
          _ = ∑ i ∈ S, (1:ℝ) := by
            apply Finset.sum_congr rfl
            intro j hj
            have hj' : x j ≠ b := (Finset.mem_filter.mp hj).2
            rw [zero_add,div_self (by positivity)]
          _ = _ := by simp
      have hW : ContinuousAt (residualWeight x b) 0 := by
        rw [hE]
        apply AnalyticAt.continuousAt (𝕜 := ℝ)
        apply Finset.analyticAt_fun_sum
        intro j hj
        have hJ : x j ≠ b := (Finset.mem_filter.mp hj).2
        exact analyticAt_const.div (by fun_prop) (by simp; positivity)
      have hP : (n:ℝ)/2 < residualWeight x b 0 := by
        rw [h0]
        have hN : n < 2*S.card := by omega
        have H : (n:ℝ) < 2*S.card := by exact_mod_cast hN
        linarith
      have hE' := hW (Ioi_mem_nhds hP)
      obtain ⟨ε,hε,hBall⟩ := Metric.mem_nhds_iff.mp hE'
      refine ⟨ε,hε,fun u hu huε ↦ Or.inl ?_⟩
      have hp : (n:ℝ)/2 < residualWeight x b u :=
        hBall (by simpa [Metric.mem_ball,Real.dist_eq,abs_of_pos hu] using huε)
      unfold kernelShape
      have hP' := (residualWeight_bounds x b hX hu).1
      suffices h : (n:ℝ)/residualWeight x b u < 2 by linarith
      exact (div_lt_iff₀ hP').mpr (by linarith)
  obtain ⟨ε,hε,hLower⟩ := lower
  obtain ⟨A,hA⟩ := eventually_atTop.1 hTail
  let B := max A ε+1
  have hB : ε ≤ B := by unfold B; linarith [le_max_right A ε]
  have hBA : A ≤ B := by unfold B; linarith [le_max_left A ε]
  rcases kernelProfile_compact_stationary x b hX (c := B) hε with hFinite | hConst
  · apply hFinite.subset
    intro u hu
    change 0 < u ∧ 1 ≤ kernelShape x b u ∧ deriv (kernelProfile x b) u = 0 at hu
    have huε : ε ≤ u := by
      by_contra! h
      rcases hLower u hu.1 h with h | h
      · exact h.not_ge hu.2.1
      · exact h hu.2.2
    have huB : u ≤ B := by
      by_contra! h
      exact hA u (hBA.trans h.le) hu.2.2
    exact ⟨⟨huε,huB⟩,hu.2.2⟩
  · have hPos : 0 < B := hε.trans_le hB
    have hSame : (kernelProfile x b) =ᶠ[𝓝 B] fun _ ↦ kernelProfile x b B := by
      filter_upwards [Ioi_mem_nhds hPos] with u hu
      exact hConst u hu B hPos
    have he := (hasDerivAt_const B (kernelProfile x b B)).congr_of_eventuallyEq hSame
    exact (hA B hBA he.deriv).elim


/-- The critical fixed-standard-deviation collapse has a strict
punctured losing collar. Its log-score tends to minus infinity as
the shape approaches two from above. -/
theorem std_fixed_mean_sd_critical_log_score {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b : ℝ) {s : ℝ} (hs : 0 < s) (hc : 3*TDist.multiplicity x b = 2*n) :
    Tendsto (deriv (fun q ↦ TDist.stdLogLik x b q s)) (𝓝[>] 2) atBot := by
  classical
  let S := Finset.univ.filter (fun i ↦ x i ≠ b)
  let W := fun q : ℝ ↦ (q-2)*s^2
  have hS : S.card + TDist.multiplicity x b = n := noncontact_card x b
  have hM : (S.card:ℝ)*3 = n := by exact_mod_cast (show S.card*3=n by omega)
  have hSn : 0 < S.card := by omega
  have hSr : 0 < (S.card:ℝ) := by exact_mod_cast hSn
  let A := fun q : ℝ ↦ (n:ℝ)*(TDist.logNormalizer q+log q/2)-
    (q+1)/2*∑ i ∈ S, log (W q+(x i-b)^2)
  let C := (S.card:ℝ)/2
  have hC : 0 < C := by unfold C; positivity
  have hA : AnalyticAt ℝ A 2 := by
    have h1 : AnalyticAt ℝ (fun q : ℝ ↦ (q+1)/2) 2 :=
      (analyticAt_id.add analyticAt_const).div_const
    have h2 : AnalyticAt ℝ (fun q : ℝ ↦ q/2) 2 := analyticAt_id.div_const
    have hg1 := ((real_Gamma_analytic (by norm_num : (0:ℝ)<(2+1)/2)).comp
      (f := fun q : ℝ ↦ (q+1)/2) h1).log (Gamma_pos_of_pos (by norm_num))
    have hg2 := ((real_Gamma_analytic (by norm_num : (0:ℝ)<2/2)).comp
      (f := fun q : ℝ ↦ q/2) h2).log (Gamma_pos_of_pos (by norm_num))
    have hL : AnalyticAt ℝ (fun q : ℝ ↦ log (q*π)/2) 2 :=
      ((analyticAt_id.mul analyticAt_const).log (mul_pos (by norm_num : (0:ℝ)<2) pi_pos)).div_const
    have hN : AnalyticAt ℝ TDist.logNormalizer 2 := (hg1.sub hg2).sub hL
    have hG : AnalyticAt ℝ (fun q : ℝ ↦ ∑ i ∈ S, log (W q+(x i-b)^2)) 2 := by
      apply Finset.analyticAt_fun_sum
      intro i hi
      have hne : x i-b ≠ 0 := sub_ne_zero.mpr (Finset.mem_filter.mp hi).2
      exact ((by unfold W; fun_prop : AnalyticAt ℝ (fun q ↦ W q+(x i-b)^2) 2)).log
        (by simp [W]; positivity)
    exact (analyticAt_const.mul (hN.add (analyticAt_id.log (by norm_num)).div_const)).sub
      (h1.mul hG)
  have hEq (q : ℝ) (hq : 2 < q) :
      TDist.stdLogLik x b q s = A q+C*(q-2)*(log (q-2)+log (s^2)) := by
    have hW : 0 < W q := by unfold W; positivity
    have hNorm :
        log (Gamma ((q+1)/2))-log (Gamma (q/2))-log ((q-2)*π)/2-log s =
          TDist.logNormalizer q+log q/2-log (W q)/2 := by
      unfold TDist.logNormalizer W
      rw [log_mul (sub_pos.mpr hq).ne' pi_pos.ne',
        log_mul (by positivity : q ≠ 0) pi_pos.ne',
        log_mul (sub_pos.mpr hq).ne' (by positivity : s^2 ≠ 0),log_pow]
      ring
    have hSum : ∑ i, log (1+(x i-b)^2/W q) =
        (∑ i ∈ S, log (W q+(x i-b)^2))-(S.card:ℝ)*log (W q) := by
      rw [← nsmul_eq_mul,← Finset.sum_const,← Finset.sum_sub_distrib]
      unfold S
      rw [Finset.sum_filter]
      apply Finset.sum_congr rfl
      intro i _
      by_cases hi : x i = b
      · simp [hi]
      · simp only [hi,if_false,if_true,ne_eq,not_false_eq_true]
        rw [← log_div (by positivity) hW.ne']
        congr 1
        field_simp <;> ring
    unfold TDist.stdLogLik
    simp_rw [hNorm]
    rw [Finset.sum_sub_distrib,← Finset.mul_sum,Finset.sum_const,Finset.card_univ,Fintype.card_fin,
      nsmul_eq_mul,hSum]
    unfold A C
    have hQ : log (W q) = log (q-2)+log (s^2) := log_mul (sub_pos.mpr hq).ne' (by positivity)
    rw [hQ]
    linear_combination (log (q-2)+log (s^2))/2 * hM
  have ev : (fun q ↦ deriv (fun v ↦ TDist.stdLogLik x b v s) q) =ᶠ[𝓝[>] 2]
      (fun q ↦ C*log (q-2)+(deriv A q+C*(1+log (s^2)))) := by
    have hAn := hA.eventually_analyticAt.filter_mono (nhdsWithin_le_nhds (s := Ioi 2))
    filter_upwards [hAn,self_mem_nhdsWithin] with q hqA hq
    have hq' : 2 < q := hq
    have hF := ((hasDerivAt_id q).sub_const 2).const_mul C
    have hL := (((hasDerivAt_id q).sub_const 2).log (sub_pos.mpr hq').ne').add_const (log (s^2))
    have h := (hqA.differentiableAt.hasDerivAt.add (hF.mul hL)).congr_of_eventuallyEq
      (by filter_upwards [Ioi_mem_nhds hq'] with p hp; exact hEq p hp)
    rw [h.deriv]
    simp only [id_eq,mul_one]
    field_simp
    <;> ring
  have hShift : Tendsto (fun q : ℝ ↦ q-2) (𝓝[>] 2) (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    have hRight : ∀ᶠ q : ℝ in 𝓝[>] 2, q ∈ Ioi 2 := self_mem_nhdsWithin
    refine ⟨?_,hRight.mono (fun q hq ↦ sub_pos.mpr hq)⟩
    have H : Tendsto (fun q : ℝ ↦ q-2) (𝓝 2) (𝓝 (2-2)) :=
      ((continuous_id.continuousAt (x := (2:ℝ))).tendsto.sub_const (2:ℝ))
    rw [sub_self] at H
    exact H.mono_left nhdsWithin_le_nhds
  have hDeriv := (hA.deriv.continuousAt.tendsto.mono_left (nhdsWithin_le_nhds (s := Ioi 2))).add_const
    (C*(1+log (s^2)))
  exact (((tendsto_log_nhdsGT_zero.comp hShift).const_mul_atBot hC).atBot_add hDeriv).congr' ev.symm


end UnivariateML.Student2Half
end
