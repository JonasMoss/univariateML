module

public import Mathlib.Analysis.SpecialFunctions.Gamma.BohrMollerup
public import Mathlib.Analysis.SpecialFunctions.ImproperIntegrals
public import Mathlib.Analysis.Convex.Deriv
public import Mathlib.MeasureTheory.Integral.DominatedConvergence
public import Mathlib.MeasureTheory.Integral.Bochner.Set
public import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.Tactic

set_option linter.style.header false

/-! Beta-negative-binomial likelihood in the held regimes.
A digamma difference integral turns each scalar score into a Laplace
transform with one change of sign. When alpha alone is held, small- and
large-shape bounds give compact likelihood levels. -/

@[expose] public section

namespace UnivariateML.Discrete3BNBinom

open Set Real MeasureTheory Filter
open scoped Topology BigOperators

noncomputable section

/-- A score kernel with one negative-to-positive sign change integrates
strictly negatively after any positive exponential tilt, if its original
integral is zero. -/
theorem tilted_score_neg {g : ℝ → ℝ} {c δ : ℝ} (hc : 0 < c) (hδ : 0 < δ)
    (hleft : ∀ t ∈ Ioo 0 c, g t < 0) (hright : ∀ t > c, 0 < g t)
    (hg : IntegrableOn g (Ioi 0))
    (ht : IntegrableOn (fun t ↦ exp (-δ*t) * g t) (Ioi 0))
    (hzero : ∫ t in Ioi 0, g t = 0) :
    (∫ t in Ioi 0, exp (-δ*t) * g t) < 0 := by
  let f := fun t ↦ exp (-δ*c) * g t - exp (-δ*t) * g t
  have hf : IntegrableOn f (Ioi 0) := (hg.const_mul _).sub ht
  have hsign (t : ℝ) (ht0 : 0 < t) : 0 ≤ f t := by
    change 0 ≤ exp (-δ*c) * g t - exp (-δ*t) * g t
    rw [← sub_mul]
    rcases lt_trichotomy t c with h | rfl | h
    · have he : exp (-δ*c) < exp (-δ*t) :=
        exp_lt_exp.mpr (by nlinarith [mul_pos hδ (sub_pos.mpr h)])
      exact mul_nonneg_of_nonpos_of_nonpos (sub_nonpos.mpr he.le) (hleft t ⟨ht0, h⟩).le
    · simp
    · have he : exp (-δ*t) < exp (-δ*c) :=
        exp_lt_exp.mpr (by nlinarith [mul_pos hδ (sub_pos.mpr h)])
      exact mul_nonneg (sub_nonneg.mpr he.le) (hright t h).le
  have hs : Ioo c (c+1) ⊆ Function.support f ∩ Ioi 0 := by
    intro t ht'
    have he : exp (-δ*t) < exp (-δ*c) :=
      exp_lt_exp.mpr (by nlinarith [mul_pos hδ (sub_pos.mpr ht'.1)])
    refine ⟨?_, hc.trans ht'.1⟩
    change exp (-δ*c) * g t - exp (-δ*t) * g t ≠ 0
    rw [← sub_mul]
    exact (mul_pos (sub_pos.mpr he) (hright t ht'.1)).ne'
  have hpos : 0 < ∫ t in Ioi 0, f t := by
    apply (setIntegral_pos_iff_support_of_nonneg_ae
      ((ae_restrict_iff' measurableSet_Ioi).2 (Filter.Eventually.of_forall hsign)) hf).2
    exact lt_of_lt_of_le (by simp [Real.volume_Ioo] : 0 < volume (Ioo c (c+1))) (measure_mono hs)
  dsimp [f] at hpos
  rw [integral_sub (hg.const_mul _) ht, integral_const_mul, hzero, mul_zero, zero_sub] at hpos
  linarith

/-- On the positive shape domain, a scalar score of this Laplace form has
at most one root. -/
theorem score_root_unique {g : ℝ → ℝ} {c : ℝ} (hc : 0 < c)
    (hleft : ∀ t ∈ Ioo 0 c, g t < 0) (hright : ∀ t > c, 0 < g t)
    (hint : ∀ a > 0, IntegrableOn (fun t ↦ exp (-a*t) * g t) (Ioi 0))
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hza : ∫ t in Ioi 0, exp (-a*t) * g t = 0)
    (hzb : ∫ t in Ioi 0, exp (-b*t) * g t = 0) : a = b := by
  have no_pair (u v : ℝ) (hu : 0 < u) (hv : 0 < v) (huv : u < v)
      (hzu : ∫ t in Ioi 0, exp (-u*t) * g t = 0)
      (hzv : ∫ t in Ioi 0, exp (-v*t) * g t = 0) : False := by
    have he (t : ℝ) : exp (-(v-u)*t) * (exp (-u*t) * g t) = exp (-v*t) * g t := by
      rw [← mul_assoc, ← exp_add]
      congr 2
      ring
    have hn := tilted_score_neg hc (sub_pos.mpr huv)
      (fun t ht ↦ mul_neg_of_pos_of_neg (exp_pos _) (hleft t ht))
      (fun t ht ↦ mul_pos (exp_pos _) (hright t ht))
      (hint u hu) (by simpa only [he] using hint v hv) hzu
    simp only [he, hzv, lt_irrefl] at hn
  rcases lt_trichotomy a b with hab | hab | hab
  · exact (no_pair a b ha hb hab hza hzb).elim
  · exact hab
  · exact (no_pair b a hb ha hab hzb hza).elim

private def logGamma (x : ℝ) : ℝ := log (Gamma x)
private def psi : ℝ → ℝ := deriv logGamma

private theorem logGamma_deriv {x : ℝ} (hx : 0 < x) :
    HasDerivAt logGamma (psi x) x := by
  apply DifferentiableAt.hasDerivAt
  have hG : DifferentiableAt ℝ Gamma x :=
    Real.differentiableAt_Gamma (fun m h ↦ by linarith [Nat.cast_nonneg (α := ℝ) m])
  exact hG.log (Gamma_pos_of_pos hx).ne'

private theorem logGamma_succ {x : ℝ} (hx : 0 < x) :
    logGamma (x+1) = log x+logGamma x := by
  unfold logGamma
  rw [Gamma_add_one hx.ne', log_mul hx.ne' (Gamma_pos_of_pos hx).ne']

private theorem psi_succ {x : ℝ} (hx : 0 < x) :
    psi (x+1) = psi x+1/x := by
  have ha := (logGamma_deriv (show 0 < x+1 by linarith)).comp x
    ((hasDerivAt_id x).add_const 1)
  have hb := (Real.hasDerivAt_log hx.ne').add (logGamma_deriv hx)
  have he : (fun y ↦ logGamma (y+1)) =ᶠ[𝓝 x] (fun y ↦ log y+logGamma y) := by
    filter_upwards [Ioi_mem_nhds hx] with y hy
    exact logGamma_succ hy
  have hadd : HasDerivAt (fun y ↦ logGamma (y+1)) (psi (x+1)) x := by
    simpa only [Function.comp_def, id_eq, mul_one] using ha
  have heq := hadd.unique (hb.congr_of_eventuallyEq he)
  simpa only [one_div, add_comm] using heq

private theorem psi_monotone : MonotoneOn psi (Ioi 0) :=
  Real.convexOn_log_Gamma.monotoneOn_deriv
    (fun _ hx ↦ (logGamma_deriv hx).differentiableAt)

private theorem psi_le_log {x : ℝ} (hx : 0 < x) : psi x ≤ log x := by
  have h : psi x ≤ slope logGamma x (x+1) :=
    Real.convexOn_log_Gamma.deriv_le_slope hx (show 0 < x+1 by linarith)
      (by linarith) (logGamma_deriv hx).differentiableAt
  rw [slope_def_field, logGamma_succ hx] at h
  simpa only [add_sub_cancel_right, add_sub_cancel_left, div_one] using h

private theorem log_sub_one_le_psi {x : ℝ} (hx : 1 < x) : log (x-1) ≤ psi x := by
  have hx' : 0 < x-1 := sub_pos.mpr hx
  have h : slope logGamma (x-1) x ≤ psi x :=
    Real.convexOn_log_Gamma.slope_le_deriv hx' (zero_lt_one.trans hx)
      (by linarith) (logGamma_deriv (zero_lt_one.trans hx)).differentiableAt
  rw [slope_def_field] at h
  have he : logGamma x = log (x-1)+logGamma (x-1) := by
    simpa only [sub_add_cancel] using logGamma_succ hx'
  rw [he, add_sub_cancel_right, show x-(x-1) = 1 by ring, div_one] at h
  exact h

private def gammaDifferenceKernel (a b t : ℝ) : ℝ :=
  exp (-a*t)*(1-exp (-b*t))/(1-exp (-t))

private theorem gammaDifference_bound {a b t : ℝ} (hb : 0 ≤ b) (ht : 0 < t)
    {m : ℕ} (hm : b ≤ m) :
    0 ≤ gammaDifferenceKernel a b t ∧ gammaDifferenceKernel a b t ≤ (m : ℝ)*exp (-a*t) := by
  have hu : exp (-t) < 1 := exp_lt_one_iff.mpr (neg_neg_of_pos ht)
  have hd := sub_pos.mpr hu
  have hn : 0 ≤ 1-exp (-b*t) := sub_nonneg.mpr
    (exp_le_one_iff.mpr (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr hb) ht.le))
  have he : exp (-(m : ℝ)*t) = (exp (-t))^m := by
    rw [show -(m : ℝ)*t = (m : ℝ)*(-t) by ring, exp_nat_mul]
  have hratio : (1-(exp (-t))^m)/(1-exp (-t)) ≤ (m : ℝ) := by
    rw [← geom_sum_mul_neg, mul_div_cancel_right₀ _ hd.ne']
    calc
      (∑ i ∈ Finset.range m, (exp (-t))^i) ≤ ∑ _i ∈ Finset.range m, (1 : ℝ) :=
        Finset.sum_le_sum (fun i _ ↦ pow_le_one₀ (exp_pos _).le hu.le)
      _ = m := by simp
  have hn' : 1-exp (-b*t) ≤ 1-(exp (-t))^m := by
    rw [← he]
    exact sub_le_sub_left (exp_le_exp.mpr (mul_le_mul_of_nonneg_right (neg_le_neg hm) ht.le)) _
  unfold gammaDifferenceKernel
  refine ⟨div_nonneg (mul_nonneg (exp_pos _).le hn) hd.le, ?_⟩
  rw [mul_div_assoc, mul_comm (m : ℝ)]
  exact mul_le_mul_of_nonneg_left ((div_le_div_of_nonneg_right hn' hd.le).trans hratio)
    (exp_pos _).le

private theorem gammaDifference_integrable {a b : ℝ} (ha : 0 < a) (hb : 0 ≤ b) :
    IntegrableOn (gammaDifferenceKernel a b) (Ioi 0) := by
  obtain ⟨m, hm⟩ := exists_nat_ge b
  apply ((integrableOn_exp_mul_Ioi (a := -a) (neg_neg_of_pos ha) 0).const_mul (m : ℝ)).mono'
  · exact (show Measurable (gammaDifferenceKernel a b) from by
      unfold gammaDifferenceKernel
      fun_prop).aestronglyMeasurable
  · filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
    have h := gammaDifference_bound (a := a) hb ht hm
    rw [Real.norm_of_nonneg h.1]
    exact h.2

private def gammaDifference (a b : ℝ) : ℝ :=
  ∫ t in Ioi 0, gammaDifferenceKernel a b t

private theorem exp_tail_integral {a : ℝ} (ha : 0 < a) :
    (∫ t in Ioi 0, exp (-a*t)) = 1/a := by
  rw [integral_exp_mul_Ioi (neg_neg_of_pos ha), mul_zero, exp_zero, neg_div_neg_eq]

private theorem gammaDifference_nonneg {a b : ℝ} (hb : 0 ≤ b) :
    0 ≤ gammaDifference a b := by
  obtain ⟨m, hm⟩ := exists_nat_ge b
  apply integral_nonneg_of_ae
  filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
  exact (gammaDifference_bound (a := a) hb ht hm).1

private theorem gammaDifference_le {a b : ℝ} (ha : 0 < a) (hb : 0 ≤ b)
    {m : ℕ} (hm : b ≤ m) : gammaDifference a b ≤ (m : ℝ)/a := by
  have h := integral_mono_ae (gammaDifference_integrable ha hb)
    ((integrableOn_exp_mul_Ioi (a := -a) (neg_neg_of_pos ha) 0).const_mul (m : ℝ))
    (by filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
        exact (gammaDifference_bound (a := a) hb ht hm).2)
  rw [integral_const_mul, exp_tail_integral ha] at h
  unfold gammaDifference
  simpa only [mul_one_div] using h

private theorem gammaDifference_shift {a b : ℝ} (ha : 0 < a) (hb : 0 ≤ b) :
    gammaDifference a b - gammaDifference (a+1) b = 1/a-1/(a+b) := by
  have hint (u : ℝ) (hu : 0 < u) :
      IntegrableOn (fun t : ℝ ↦ exp (-u*t)) (Ioi 0) :=
    integrableOn_exp_mul_Ioi (neg_neg_of_pos hu) 0
  have h : (∫ t in Ioi 0, gammaDifferenceKernel a b t-gammaDifferenceKernel (a+1) b t) =
      ∫ t in Ioi 0, exp (-a*t)-exp (-(a+b)*t) := by
    apply setIntegral_congr_fun measurableSet_Ioi
    intro t ht
    have hd : 1-exp (-t) ≠ 0 := (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht))).ne'
    unfold gammaDifferenceKernel
    dsimp only
    rw [show -(a+1)*t = -a*t+(-t) by ring, show -(a+b)*t = -a*t+(-b*t) by ring,
      exp_add, exp_add]
    field_simp [hd]
  rw [integral_sub (gammaDifference_integrable ha hb) (gammaDifference_integrable
    (show 0 < a+1 by linarith) hb), integral_sub (hint a ha) (hint (a+b) (by linarith)),
    exp_tail_integral ha, exp_tail_integral (show 0 < a+b by linarith)] at h
  exact h

private theorem psi_difference_integral {a b : ℝ} (ha : 0 < a) (hb : 0 ≤ b) :
    psi (a+b)-psi a = gammaDifference a b := by
  let f := fun x : ℝ ↦ psi (x+b)-psi x-gammaDifference x b
  have hshift (x : ℝ) (hx : 0 < x) : f (x+1) = f x := by
    unfold f
    rw [show x+1+b = (x+b)+1 by ring, psi_succ (show 0 < x+b by linarith), psi_succ hx]
    linarith only [gammaDifference_shift hx hb]
  have hconst (n : ℕ) : f (a+n) = f a := by
    induction n with
    | zero => simp
    | succ n =>
      rw [Nat.cast_succ, ← add_assoc, hshift (a+n) (by positivity), ‹f (a+(n : ℝ)) = f a›]
  obtain ⟨m, hm⟩ := exists_nat_ge b
  have hlim : Tendsto f atTop (𝓝 0) := by
    let U := fun x : ℝ ↦ (b+1)/(x-1)+(m : ℝ)/x
    have hL : Tendsto (fun x : ℝ ↦ x-1) atTop atTop :=
      tendsto_atTop_add_const_right atTop (-1) tendsto_id
    have hu : Tendsto U atTop (𝓝 0) := by
      have h₁ := (tendsto_inv_atTop_zero.comp hL).const_mul (b+1)
      have h₂ := tendsto_inv_atTop_zero.const_mul (m : ℝ)
      simpa only [U, div_eq_mul_inv, Function.comp_def, mul_zero, add_zero] using h₁.add h₂
    have hneg : Tendsto (fun t ↦ -U t) atTop (𝓝 0) := by
      simpa only [neg_zero] using hu.neg
    apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hneg hu <;>
      filter_upwards [eventually_gt_atTop (1 : ℝ)] with x hx
    · have hx₀ : 0 < x := zero_lt_one.trans hx
      have hp : psi x ≤ psi (x+b) :=
        psi_monotone (show x ∈ Ioi (0 : ℝ) from hx₀)
          (show 0 < x+b by linarith) (le_add_of_nonneg_right hb)
      have hg := gammaDifference_le hx₀ hb hm
      have hz : 0 ≤ (b+1)/(x-1) := by positivity
      dsimp [f, U]
      linarith only [hp, hg, hz]
    · have hx₀ : 0 < x := zero_lt_one.trans hx
      have h := log_le_sub_one_of_pos (div_pos (show 0 < x+b by linarith) (sub_pos.mpr hx))
      rw [log_div (show x+b ≠ 0 by linarith) (sub_pos.mpr hx).ne'] at h
      have he : (x+b)/(x-1)-1 = (b+1)/(x-1) := by
        field_simp [(sub_pos.mpr hx).ne']
        ring
      rw [he] at h
      have hM : 0 ≤ (m : ℝ)/x := by positivity
      dsimp [f, U]
      linarith only [h, psi_le_log (show 0 < x+b by linarith), log_sub_one_le_psi hx,
        gammaDifference_nonneg (a := x) hb, hM]
  have haNat : Tendsto (fun n : ℕ ↦ a+(n : ℝ)) atTop atTop :=
    tendsto_atTop_add_const_left atTop a tendsto_natCast_atTop_atTop
  have h := (hlim.comp haNat).congr (fun n ↦ hconst n)
  have hf : f a = 0 := tendsto_nhds_unique tendsto_const_nhds h
  exact sub_eq_zero.mp hf

private def loss (a t : ℝ) : ℝ := 1-exp (-a*t)

private theorem loss_pos {a t : ℝ} (ha : 0 < a) (ht : 0 < t) : 0 < loss a t :=
  sub_pos.mpr (exp_lt_one_iff.mpr (mul_neg_of_neg_of_pos (neg_neg_of_pos ha) ht))

private theorem loss_nonneg {a t : ℝ} (ha : 0 ≤ a) (ht : 0 ≤ t) : 0 ≤ loss a t :=
  sub_nonneg.mpr (exp_le_one_iff.mpr (mul_nonpos_of_nonpos_of_nonneg (neg_nonpos.mpr ha) ht))

private theorem loss_deriv (a t : ℝ) :
    HasDerivAt (loss a) (a*exp (-a*t)) t := by
  have h := (hasDerivAt_const t (1 : ℝ)).sub (((hasDerivAt_id t).const_mul (-a)).exp)
  convert h using 1
  · rfl
  · simp only [id_eq, mul_one, zero_sub]
    ring

private theorem loss_zero (a : ℝ) : loss a 0 = 0 := by simp [loss]

private theorem loss_atTop {a : ℝ} (ha : 0 < a) : Tendsto (loss a) atTop (𝓝 1) := by
  have h := tendsto_exp_atBot.comp (tendsto_id.const_mul_atTop_of_neg (neg_neg_of_pos ha))
  change Tendsto (fun t : ℝ ↦ 1-exp (-a*t)) atTop (𝓝 1)
  simpa only [Function.comp_def, id_eq, sub_zero] using
    (tendsto_const_nhds (x := (1 : ℝ))).sub h

private theorem loss_secant_anti (a : ℝ) : AntitoneOn (fun t ↦ loss a t / t) (Ioi 0) := by
  intro t ht u hu htu
  let q := t/u
  have hq₀ : 0 ≤ q := div_nonneg ht.le hu.le
  have hq₁ : 0 ≤ 1-q := sub_nonneg.mpr ((div_le_one hu).mpr htu)
  have hq : (1-q)+q=1 := by ring
  have he := convexOn_exp.2 (mem_univ (0 : ℝ)) (mem_univ (-a*u)) hq₁ hq₀ hq
  simp only [smul_eq_mul, mul_zero, zero_add, exp_zero, mul_one] at he
  have hxt : q*(-a*u) = -a*t := by dsimp [q]; field_simp [show u ≠ 0 from (show 0 < u from hu).ne']
  rw [hxt] at he
  have hr : q*loss a u ≤ loss a t := by unfold loss; linarith only [he]
  apply (div_le_div_iff₀ hu ht).mpr
  dsimp [q] at hr
  have heq : t/u*loss a u = t*loss a u/u := by ring
  rw [heq] at hr
  have hp := (div_le_iff₀ hu).mp hr
  linarith only [hp]

private theorem rise_secant_strict {a : ℝ} (ha : 0 < a) :
    StrictMonoOn (fun t ↦ (exp (a*t)-1)/t) (Ioi 0) := by
  intro t ht u hu htu
  let q := t/u
  have hq₀ : 0 < q := div_pos ht hu
  have hq₁ : 0 < 1-q := sub_pos.mpr ((div_lt_one hu).mpr htu)
  have hq : (1-q)+q=1 := by ring
  have hne : (0 : ℝ) ≠ a*u := (mul_pos ha hu).ne
  have he := strictConvexOn_exp.2 (mem_univ (0 : ℝ)) (mem_univ (a*u)) hne hq₁ hq₀ hq
  simp only [smul_eq_mul, mul_zero, zero_add, exp_zero, mul_one] at he
  have hxt : q*(a*u) = a*t := by dsimp [q]; field_simp [show u ≠ 0 from (show 0 < u from hu).ne']
  rw [hxt] at he
  have hr : exp (a*t)-1 < q*(exp (a*u)-1) := by linarith only [he]
  apply (div_lt_div_iff₀ ht hu).mpr
  dsimp [q] at hr
  have heq : t/u*(exp (a*u)-1) = t*(exp (a*u)-1)/u := by ring
  rw [heq] at hr
  have hp := (lt_div_iff₀ hu).mp hr
  linarith only [hp]

private def avgLoss {n : ℕ} (x : Fin n → ℕ) (t : ℝ) : ℝ :=
  (∑ i, loss (x i) t)/n

private def countMean {n : ℕ} (x : Fin n → ℕ) : ℝ := (∑ i, (x i : ℝ))/n

private def positiveFraction {n : ℕ} (x : Fin n → ℕ) : ℝ :=
  (∑ i, if x i = 0 then (0 : ℝ) else 1)/n

private theorem avgLoss_zero {n : ℕ} (x : Fin n → ℕ) : avgLoss x 0 = 0 := by
  simp [avgLoss, loss_zero]

private theorem avgLoss_deriv_zero {n : ℕ} (x : Fin n → ℕ) :
    HasDerivAt (avgLoss x) (countMean x) 0 := by
  have h := HasDerivAt.fun_sum (u := (Finset.univ : Finset (Fin n)))
    (fun i _ ↦ loss_deriv (x i) 0)
  unfold avgLoss countMean
  simpa only [mul_zero, exp_zero, mul_one] using h.div_const (n : ℝ)

private theorem countMean_pos {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) : 0 < countMean x := by
  obtain ⟨i, hi⟩ := hx
  unfold countMean
  exact div_pos (Finset.sum_pos' (fun k _ ↦ Nat.cast_nonneg _)
    ⟨i, Finset.mem_univ _, by exact_mod_cast hi⟩) (by exact_mod_cast hn)

private theorem positiveFraction_pos {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) : 0 < positiveFraction x := by
  obtain ⟨i, hi⟩ := hx
  unfold positiveFraction
  apply div_pos _ (by exact_mod_cast hn)
  exact Finset.sum_pos' (fun k _ ↦ by split_ifs <;> norm_num)
    ⟨i, Finset.mem_univ _, by simp [hi.ne']⟩

private theorem avgLoss_atTop {n : ℕ} (x : Fin n → ℕ) :
    Tendsto (avgLoss x) atTop (𝓝 (positiveFraction x)) := by
  have h (i : Fin n) :
      Tendsto (loss (x i)) atTop (𝓝 (if x i = 0 then (0 : ℝ) else 1)) := by
    by_cases hi : x i = 0
    · rw [hi, Nat.cast_zero, ite_eq_left rfl]
      change Tendsto (fun t : ℝ ↦ loss 0 t) atTop (𝓝 0)
      simpa only [loss, neg_zero, zero_mul, exp_zero, sub_self] using
        (tendsto_const_nhds (x := (0 : ℝ)))
    · simpa only [ite_eq_right hi] using loss_atTop
        (show (0 : ℝ) < (x i : ℝ) by exact_mod_cast Nat.pos_of_ne_zero hi)
  unfold avgLoss positiveFraction
  exact (tendsto_finsetSum Finset.univ (fun i _ ↦ h i)).div_const (n : ℝ)

private theorem avgLoss_continuous {n : ℕ} (x : Fin n → ℕ) : Continuous (avgLoss x) := by
  unfold avgLoss
  exact (continuous_finsetSum _ fun i _ ↦ continuous_iff_continuousAt.mpr
    fun t ↦ (loss_deriv (x i) t).continuousAt).div_const (n : ℝ)

private theorem avgLoss_pos {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) {t : ℝ} (ht : 0 < t) : 0 < avgLoss x t := by
  obtain ⟨i, hi⟩ := hx
  unfold avgLoss
  exact div_pos (Finset.sum_pos' (fun _ _ ↦ loss_nonneg (Nat.cast_nonneg _) ht.le)
    ⟨i, Finset.mem_univ _, loss_pos (by exact_mod_cast hi) ht⟩) (by exact_mod_cast hn)

private theorem avgLoss_strictMono {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) : StrictMono (avgLoss x) := by
  intro t u htu
  unfold avgLoss
  apply div_lt_div_of_pos_right _ (by exact_mod_cast hn)
  obtain ⟨i, hi⟩ := hx
  apply Finset.sum_lt_sum _ ⟨i, Finset.mem_univ _, ?_⟩
  · intro j _
    unfold loss
    exact sub_le_sub_left (exp_le_exp.mpr
      (mul_le_mul_of_nonpos_left htu.le (neg_nonpos.mpr (Nat.cast_nonneg _)))) _
  · unfold loss
    exact sub_lt_sub_left (exp_lt_exp.mpr
      (mul_lt_mul_of_neg_left htu (neg_neg_of_pos (by exact_mod_cast hi : (0 : ℝ) < x i)))) _

private theorem avgLoss_secant_anti {n : ℕ} (x : Fin n → ℕ) :
    AntitoneOn (fun t ↦ avgLoss x t/t) (Ioi 0) := by
  intro t ht u hu htu
  have he (s : ℝ) : avgLoss x s/s = (∑ i, loss (x i) s/s)/n := by
    rw [avgLoss, ← Finset.sum_div]
    ring
  change avgLoss x u/u ≤ avgLoss x t/t
  rw [he t, he u]
  exact div_le_div_of_nonneg_right (Finset.sum_le_sum fun i _ ↦
    loss_secant_anti (x i) ht hu htu) (Nat.cast_nonneg _)

private theorem quotient_limit_at_zero {f : ℝ → ℝ} {d : ℝ}
    (hd : HasDerivAt f d 0) (hf : f 0 = 0) :
    Tendsto (fun t ↦ f t/t) (𝓝[>] (0 : ℝ)) (𝓝 d) := by
  have h := hd.tendsto_slope.mono_left (nhdsGT_le_nhdsNE 0)
  simpa only [slope_fun_def_field, hf, sub_zero] using h

private theorem loss_deriv_zero (a : ℝ) : HasDerivAt (loss a) a 0 := by
  simpa only [mul_zero, exp_zero, mul_one] using loss_deriv a 0

private theorem loss_continuous (a : ℝ) : Continuous (loss a) :=
  continuous_iff_continuousAt.mpr fun t ↦ (loss_deriv a t).continuousAt

private theorem expLinear_deriv (a t : ℝ) :
    HasDerivAt (fun u ↦ exp (a*u)) (exp (a*t)*a) t := by
  simpa only [id_eq, mul_one] using ((hasDerivAt_id t).const_mul a).exp

private theorem expLinear_tendsto {a : ℝ} (ha : a < 0) :
    Tendsto (fun t ↦ exp (a*t)) atTop (𝓝 0) := by
  simpa only [Function.comp_def, id_eq] using
    tendsto_exp_atBot.comp (tendsto_id.const_mul_atTop_of_neg ha)

private def alphaNumer {n : ℕ} (x : Fin n → ℕ) (r b t : ℝ) : ℝ :=
  loss r t*loss b t - exp (-(r+b)*t)*avgLoss x t

private def sizeNumer {n : ℕ} (x : Fin n → ℕ) (a b t : ℝ) : ℝ :=
  loss (a+b) t*avgLoss x t - exp (-a*t)*loss b t

private def alphaKernel {n : ℕ} (x : Fin n → ℕ) (r b t : ℝ) : ℝ :=
  alphaNumer x r b t/loss 1 t

private def sizeKernel {n : ℕ} (x : Fin n → ℕ) (a b t : ℝ) : ℝ :=
  sizeNumer x a b t/loss 1 t

private theorem alphaNumer_continuous {n : ℕ} (x : Fin n → ℕ) (r b : ℝ) :
    Continuous (alphaNumer x r b) :=
  ((loss_continuous r).mul (loss_continuous b)).sub
    ((by fun_prop : Continuous (fun t : ℝ ↦ exp (-(r+b)*t))).mul (avgLoss_continuous x))

private theorem sizeNumer_continuous {n : ℕ} (x : Fin n → ℕ) (a b : ℝ) :
    Continuous (sizeNumer x a b) :=
  ((loss_continuous (a+b)).mul (avgLoss_continuous x)).sub
    ((by fun_prop : Continuous (fun t : ℝ ↦ exp (-a*t))).mul (loss_continuous b))

private theorem kernel_nearZero {F : ℝ → ℝ} {u : ℝ}
    (hd : HasDerivAt F u 0) (hF : F 0 = 0) :
    Tendsto (fun t ↦ F t/loss 1 t) (𝓝[>] (0 : ℝ)) (𝓝 u) := by
  have h := (quotient_limit_at_zero hd hF).div
    (quotient_limit_at_zero (loss_deriv_zero 1) (loss_zero 1)) one_ne_zero
  rw [div_one] at h
  apply h.congr'
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
  have ht' : 0 < t := ht
  have hl := loss_pos one_pos ht'
  change (F t/t)/(loss 1 t/t) = F t/loss 1 t
  field_simp [ht'.ne', hl.ne']

private theorem alphaKernel_atZero {n : ℕ} (x : Fin n → ℕ) (r b : ℝ) :
    Tendsto (alphaKernel x r b) (𝓝[>] (0 : ℝ)) (𝓝 (-countMean x)) := by
  apply kernel_nearZero
  · have h := ((loss_deriv_zero r).mul (loss_deriv_zero b)).sub
      ((expLinear_deriv (-(r+b)) 0).mul (avgLoss_deriv_zero x))
    have he : HasDerivAt (alphaNumer x r b) (-countMean x) 0 := by
      convert h using 1
      · rfl
      · simp [loss_zero, avgLoss_zero]
    exact he
  · simp [alphaNumer, loss_zero, avgLoss_zero]

private theorem sizeKernel_atZero {n : ℕ} (x : Fin n → ℕ) (a b : ℝ) :
    Tendsto (sizeKernel x a b) (𝓝[>] (0 : ℝ)) (𝓝 (-b)) := by
  apply kernel_nearZero
  · have h := ((loss_deriv_zero (a+b)).mul (avgLoss_deriv_zero x)).sub
      ((expLinear_deriv (-a) 0).mul (loss_deriv_zero b))
    have he : HasDerivAt (sizeNumer x a b) (-b) 0 := by
      convert h using 1
      · rfl
      · simp [loss_zero, avgLoss_zero]
    exact he
  · simp [sizeNumer, loss_zero, avgLoss_zero]

private theorem alphaKernel_atTop {n : ℕ} (x : Fin n → ℕ) {r b : ℝ}
    (hr : 0 < r) (hb : 0 < b) : Tendsto (alphaKernel x r b) atTop (𝓝 1) := by
  have h := ((loss_atTop hr).mul (loss_atTop hb)).sub
    ((expLinear_tendsto (neg_neg_of_pos (add_pos hr hb))).mul (avgLoss_atTop x))
  have h' : Tendsto (alphaNumer x r b) atTop (𝓝 1) := by
    change Tendsto (fun t ↦ loss r t*loss b t - exp (-(r+b)*t)*avgLoss x t) atTop (𝓝 1)
    simpa only [mul_one, zero_mul, sub_zero] using h
  change Tendsto (fun t ↦ alphaNumer x r b t/loss 1 t) atTop (𝓝 1)
  have hq := h'.div (loss_atTop one_pos) one_ne_zero
  rw [div_one] at hq
  exact hq.congr (fun _ ↦ rfl)

private theorem sizeKernel_atTop {n : ℕ} (x : Fin n → ℕ) {a b : ℝ}
    (ha : 0 < a) (hb : 0 < b) : Tendsto (sizeKernel x a b) atTop (𝓝 (positiveFraction x)) := by
  have h := ((loss_atTop (add_pos ha hb)).mul (avgLoss_atTop x)).sub
    ((expLinear_tendsto (neg_neg_of_pos ha)).mul (loss_atTop hb))
  have h' : Tendsto (sizeNumer x a b) atTop (𝓝 (positiveFraction x)) := by
    change Tendsto (fun t ↦ loss (a+b) t*avgLoss x t-exp (-a*t)*loss b t) atTop
      (𝓝 (positiveFraction x))
    simpa only [one_mul, mul_one, sub_zero] using h
  change Tendsto (fun t ↦ sizeNumer x a b t/loss 1 t) atTop (𝓝 (positiveFraction x))
  have hq := h'.div (loss_atTop one_pos) one_ne_zero
  rw [div_one] at hq
  exact hq.congr (fun _ ↦ rfl)

private theorem alphaKernel_factor {n : ℕ} (x : Fin n → ℕ) (r b : ℝ) {t : ℝ} (ht : 0 < t) :
    alphaKernel x r b t =
      (exp (-(r+b)*t)*t/loss 1 t) *
        (((exp (r*t)-1)/t)*(exp (b*t)-1)-avgLoss x t/t) := by
  have hx (a : ℝ) : exp (-a*t)*(exp (a*t)-1) = loss a t := by
    unfold loss
    rw [mul_sub, mul_one, ← exp_add, show -a*t+a*t=0 by ring, exp_zero]
  have hf : loss r t*loss b t = exp (-(r+b)*t)*(exp (r*t)-1)*(exp (b*t)-1) := by
    rw [← hx r, ← hx b, show -(r+b)*t = -r*t+(-b*t) by ring, exp_add]
    ring
  unfold alphaKernel alphaNumer
  rw [hf]
  field_simp [ht.ne', (loss_pos one_pos ht).ne']

private theorem sizeKernel_factor {n : ℕ} (x : Fin n → ℕ) {a b t : ℝ}
    (hb : 0 < b) (ht : 0 < t) :
    sizeKernel x a b t =
      (exp (-a*t)*loss b t/loss 1 t) *
        ((1+((exp (a*t)-1)/t)/(loss b t/t))*avgLoss x t-1) := by
  have hl := (loss_pos hb ht).ne'
  have hh := (loss_pos one_pos ht).ne'
  have he : loss (a+b) t = exp (-a*t) * (loss b t+(exp (a*t)-1)) := by
    unfold loss
    rw [show -(a+b)*t = -a*t+(-b*t) by ring, exp_add]
    have h₁ : exp (-a*t)*exp (a*t) = 1 := by
      rw [← exp_add, show -a*t+a*t=0 by ring, exp_zero]
    linear_combination -h₁
  unfold sizeKernel sizeNumer
  rw [he]
  field_simp [ht.ne', hl, hh]

private theorem alphaTest_strictMono {n : ℕ} (x : Fin n → ℕ) {r b : ℝ}
    (hr : 0 < r) (hb : 0 < b) :
    StrictMonoOn (fun t ↦ ((exp (r*t)-1)/t)*(exp (b*t)-1)-avgLoss x t/t) (Ioi 0) := by
  intro t ht u hu htu
  have hb1 := sub_pos.mpr (one_lt_exp_iff.mpr (mul_pos hb ht))
  have hb2 : exp (b*t)-1 ≤ exp (b*u)-1 :=
    sub_le_sub_right (exp_le_exp.mpr (mul_le_mul_of_nonneg_left htu.le hb.le)) _
  have hr1 : (exp (r*t)-1)/t < (exp (r*u)-1)/u :=
    rise_secant_strict hr ht hu htu
  have hr2 : 0 ≤ (exp (r*u)-1)/u :=
    (div_pos (sub_pos.mpr (one_lt_exp_iff.mpr (mul_pos hr hu))) hu).le
  have hprod := (mul_lt_mul_of_pos_right hr1 hb1).trans_le
    (mul_le_mul_of_nonneg_left hb2 hr2)
  dsimp only
  linarith only [hprod, avgLoss_secant_anti x ht hu htu.le]

private theorem sizeTest_strictMono {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    StrictMonoOn (fun t ↦ (1+((exp (a*t)-1)/t)/(loss b t/t))*avgLoss x t-1) (Ioi 0) := by
  intro t ht u hu htu
  have hp : (exp (a*t)-1)/t < (exp (a*u)-1)/u := rise_secant_strict ha ht hu htu
  have hq : loss b u/u ≤ loss b t/t := loss_secant_anti b ht hu htu.le
  have ht' : 0 < loss b t/t := div_pos (loss_pos hb ht) ht
  have hu' : 0 < loss b u/u := div_pos (loss_pos hb hu) hu
  have hnum (s : ℝ) (hs : 0 < s) : 0 < (exp (a*s)-1)/s :=
    div_pos (sub_pos.mpr (one_lt_exp_iff.mpr (mul_pos ha hs))) hs
  have hdiv := (div_lt_div_of_pos_right hp ht').trans_le
    (div_le_div_of_nonneg_left (hnum u hu).le hu' hq)
  have hh : 0 ≤ 1+((exp (a*u)-1)/u)/(loss b u/u) :=
    (add_pos one_pos (div_pos (hnum u hu) hu')).le
  have hmul := (mul_lt_mul_of_pos_right (add_lt_add_right hdiv 1) (avgLoss_pos hn hx ht)).trans_le
    (mul_le_mul_of_nonneg_left (avgLoss_strictMono hn hx htu).le hh)
  exact sub_lt_sub_right hmul _

private theorem kernel_continuousOn {F : ℝ → ℝ} (hf : Continuous F) :
    ContinuousOn (fun t ↦ F t/loss 1 t) (Ioi 0) :=
  hf.continuousOn.div (loss_continuous 1).continuousOn fun _ ht ↦ (loss_pos one_pos ht).ne'

private theorem kernel_bounded {g : ℝ → ℝ} (hc : ContinuousOn g (Ioi 0)) {u v : ℝ}
    (hu : Tendsto g (𝓝[>] (0 : ℝ)) (𝓝 u)) (hv : Tendsto g atTop (𝓝 v)) :
    ∃ C : ℝ, ∀ t > 0, ‖g t‖ ≤ C := by
  have hu' := (continuous_norm.continuousAt.tendsto.comp hu).eventually
    (Iio_mem_nhds (lt_add_of_pos_right ‖u‖ zero_lt_one))
  obtain ⟨ε, hε, hnear⟩ := Metric.mem_nhdsWithin_iff.mp hu'
  have hv' := (continuous_norm.continuousAt.tendsto.comp hv).eventually
    (Iio_mem_nhds (lt_add_of_pos_right ‖v‖ zero_lt_one))
  obtain ⟨R, hR⟩ := eventually_atTop.mp hv'
  let S := max ε R
  have hs : ε ≤ S := le_max_left _ _
  have hc' : ContinuousOn (fun t ↦ ‖g t‖) (Icc ε S) :=
    continuous_norm.comp_continuousOn (hc.mono fun t ht ↦ hε.trans_le ht.1)
  obtain ⟨m, _, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨ε, le_rfl, hs⟩ hc'
  let C := max (‖u‖+1) (max (‖v‖+1) ‖g m‖)
  refine ⟨C, fun t ht ↦ ?_⟩
  rcases lt_or_ge t ε with ht' | ht'
  · have hmem : t ∈ Metric.ball (0 : ℝ) ε ∩ Ioi 0 :=
      ⟨by rwa [Metric.mem_ball, Real.dist_eq, sub_zero, abs_of_pos ht], ht⟩
    exact (show ‖g t‖ < ‖u‖+1 from hnear hmem).le.trans (le_max_left _ _)
  · rcases le_or_gt t S with hts | hts
    · exact (show ‖g t‖ ≤ ‖g m‖ from hm ⟨ht', hts⟩).trans
        ((le_max_right _ _).trans (le_max_right _ _))
    · exact (show ‖g t‖ < ‖v‖+1 from hR t ((le_max_right ε R).trans hts.le)).le.trans
        ((le_max_left _ _).trans (le_max_right _ _))

private theorem kernel_crossing {g F p : ℝ → ℝ} {u v : ℝ}
    (hc : ContinuousOn g (Ioi 0)) (hF : StrictMonoOn F (Ioi 0))
    (hp : ∀ t > 0, 0 < p t) (he : ∀ t > 0, g t = p t*F t)
    (hu' : u < 0) (hv' : 0 < v)
    (hu : Tendsto g (𝓝[>] (0 : ℝ)) (𝓝 u)) (hv : Tendsto g atTop (𝓝 v)) :
    ∃ c > 0, (∀ t ∈ Ioo 0 c, g t < 0) ∧ (∀ t > c, 0 < g t) := by
  obtain ⟨a, ha, hga⟩ := (Filter.Eventually.and (self_mem_nhdsWithin (s := Ioi (0 : ℝ)))
    (hu.eventually (Iio_mem_nhds hu'))).exists
  obtain ⟨b, hb, hgb⟩ := (Filter.Eventually.and (eventually_gt_atTop a)
    (hv.eventually (Ioi_mem_nhds hv'))).exists
  have ha' : 0 < a := ha
  have hc' : ContinuousOn g (Icc a b) := hc.mono (fun _ ht ↦ ha'.trans_le ht.1)
  obtain ⟨c, hcb, hcz⟩ := intermediate_value_Icc hb.le hc' ⟨hga.le, hgb.le⟩
  have hc₀ := ha'.trans_le hcb.1
  have hcF : F c = 0 := by
    rw [he c hc₀] at hcz
    exact (mul_eq_zero.mp hcz).resolve_left (hp c hc₀).ne'
  refine ⟨c, hc₀, ?_, ?_⟩
  · intro t ht
    have h := hF ht.1 hc₀ ht.2
    rw [hcF] at h
    rw [he t ht.1]
    exact mul_neg_of_pos_of_neg (hp t ht.1) h
  · intro t ht
    have ht₀ := hc₀.trans ht
    have h := hF hc₀ ht₀ ht
    rw [hcF] at h
    rw [he t ht₀]
    exact mul_pos (hp t ht₀) h

private def laplace (g : ℝ → ℝ) (a : ℝ) : ℝ :=
  ∫ t in Ioi 0, exp (-a*t)*g t

private theorem laplace_integrable {g : ℝ → ℝ} (hm : Measurable g)
    {C a : ℝ} (hC : ∀ t > 0, ‖g t‖ ≤ C) (ha : 0 < a) :
    IntegrableOn (fun t ↦ exp (-a*t)*g t) (Ioi 0) := by
  apply ((integrableOn_exp_mul_Ioi (neg_neg_of_pos ha) 0).mul_const C).mono'
  · exact ((by fun_prop : Measurable (fun t : ℝ ↦ exp (-a*t))).mul hm).aestronglyMeasurable
  · filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
    rw [norm_mul, Real.norm_of_nonneg (exp_pos _).le]
    exact mul_le_mul_of_nonneg_left (hC t ht) (exp_pos _).le

private theorem laplace_scaled_eq {g : ℝ → ℝ} {a : ℝ} (ha : 0 < a) :
    a*laplace g a = ∫ t in Ioi 0, exp (-t)*g (t/a) := by
  have h := integral_comp_mul_left_Ioi (fun t ↦ exp (-a*t)*g t) 0
    (b := a⁻¹) (inv_pos.mpr ha)
  simp only [inv_inv, mul_zero, smul_eq_mul] at h
  rw [laplace, ← h]
  apply setIntegral_congr_fun measurableSet_Ioi
  intro t _
  dsimp only
  have he : -a*(a⁻¹*t) = -t := by field_simp [ha.ne']
  rw [he, div_eq_mul_inv, mul_comm t]

private theorem laplace_scaled_limit {g : ℝ → ℝ} (hm : Measurable g) {C : ℝ}
    (hC : ∀ t > 0, ‖g t‖ ≤ C) {l : Filter ℝ} [l.IsCountablyGenerated] {u : ℝ}
    (hl : ∀ᶠ a in l, 0 < a)
    (hpoint : ∀ t > 0, Tendsto (fun a ↦ g (t/a)) l (𝓝 u)) :
    Tendsto (fun a ↦ a*laplace g a) l (𝓝 u) := by
  have ht : Tendsto (fun a ↦ ∫ t in Ioi 0, exp (-t)*g (t/a)) l
      (𝓝 (∫ t in Ioi 0, exp (-t)*u)) := by
    apply tendsto_integral_filter_of_dominated_convergence
      (μ := volume.restrict (Ioi (0 : ℝ))) (fun t ↦ exp (-t)*C)
    · exact Eventually.of_forall (fun a ↦ ((by fun_prop : Measurable (fun t : ℝ ↦ exp (-t))).mul
        (hm.comp (by fun_prop : Measurable (fun t : ℝ ↦ t/a)))).aestronglyMeasurable)
    · filter_upwards [hl] with a ha
      filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
      rw [norm_mul, Real.norm_of_nonneg (exp_pos _).le]
      exact mul_le_mul_of_nonneg_left (hC _ (div_pos ht ha)) (exp_pos _).le
    · exact (integrableOn_exp_neg_Ioi 0).mul_const C
    · filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
      exact (hpoint t ht).const_mul (exp (-t))
  rw [integral_mul_const, integral_exp_neg_Ioi_zero, one_mul] at ht
  exact ht.congr' (hl.mono fun _ ha ↦ (laplace_scaled_eq ha).symm)

private theorem laplace_continuousAt {g : ℝ → ℝ} (hm : Measurable g) {C : ℝ}
    (hC : ∀ t > 0, ‖g t‖ ≤ C) {a : ℝ} (ha : 0 < a) :
    ContinuousAt (laplace g) a := by
  unfold laplace
  apply tendsto_integral_filter_of_dominated_convergence
    (μ := volume.restrict (Ioi (0 : ℝ))) (fun t ↦ exp (-(a/2)*t)*C)
  · exact Eventually.of_forall (fun b ↦
      ((by fun_prop : Measurable (fun t : ℝ ↦ exp (-b*t))).mul hm).aestronglyMeasurable)
  · filter_upwards [Ioi_mem_nhds (show a/2 < a by linarith)] with b hb
    filter_upwards [ae_restrict_mem (μ := volume) measurableSet_Ioi] with t ht
    rw [norm_mul, Real.norm_of_nonneg (exp_pos _).le]
    exact mul_le_mul (exp_le_exp.mpr (mul_le_mul_of_nonneg_right (neg_le_neg hb.le) ht.le))
      (hC t ht) (norm_nonneg _) (exp_pos _).le
  · exact (integrableOn_exp_mul_Ioi (neg_neg_of_pos (show 0 < a/2 by positivity)) 0).mul_const C
  · exact ae_of_all _ fun t ↦ (show ContinuousAt (fun b : ℝ ↦ exp (-b*t)*g t) a by fun_prop).tendsto

private theorem laplace_unique_max {f g : ℝ → ℝ} {u v : ℝ}
    (hm : Measurable g) (hc : ContinuousOn g (Ioi 0))
    (h0 : Tendsto g (𝓝[>] (0 : ℝ)) (𝓝 u)) (hInf : Tendsto g atTop (𝓝 v))
    (hu : u < 0) (hv : 0 < v)
    (hcross : ∃ c > 0, (∀ t ∈ Ioo 0 c, g t < 0) ∧ (∀ t > c, 0 < g t))
    (hd : ∀ a > 0, HasDerivAt f (laplace g a) a) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) a := by
  obtain ⟨C,hC⟩ := kernel_bounded hc h0 hInf
  obtain ⟨c,hc₀,hleft,hright⟩ := hcross
  have huniq (a b : ℝ) (ha : 0 < a) (hb : 0 < b)
      (hza : laplace g a = 0) (hzb : laplace g b = 0) : a = b :=
    score_root_unique hc₀ hleft hright (fun _ h ↦ laplace_integrable hm hC h)
      ha hb hza hzb
  have hl : Tendsto (fun a ↦ a*laplace g a) (𝓝[>] (0 : ℝ)) (𝓝 v) := by
    apply laplace_scaled_limit hm hC self_mem_nhdsWithin
    intro t ht
    have h : Tendsto (fun a : ℝ ↦ t/a) (𝓝[>] (0 : ℝ)) atTop :=
      (tendsto_inv_nhdsGT_zero.const_mul_atTop ht).congr (fun a ↦ (div_eq_mul_inv t a).symm)
    exact hInf.comp h
  have hr : Tendsto (fun a ↦ a*laplace g a) atTop (𝓝 u) := by
    apply laplace_scaled_limit hm hC (eventually_gt_atTop (0 : ℝ))
    intro t ht
    have h : Tendsto (fun a : ℝ ↦ t/a) atTop (𝓝[>] (0 : ℝ)) := by
      apply tendsto_nhdsWithin_iff.mpr
      refine ⟨?_, ?_⟩
      · simpa only [div_eq_mul_inv, mul_zero] using
          (tendsto_inv_atTop_zero (𝕜 := ℝ)).const_mul t
      · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
        exact (div_pos ht ha : 0 < t/a)
    exact h0.comp h
  have hpos (b : ℝ) (hb : 0 < b) : ∃ a ∈ Ioo 0 b, 0 < laplace g a := by
    obtain ⟨a, ha, hA⟩ := (Filter.Eventually.and (Ioo_mem_nhdsGT hb)
      (hl.eventually (Ioi_mem_nhds hv))).exists
    exact ⟨a, ha, pos_of_mul_pos_right (show 0 < a*laplace g a from hA) ha.1.le⟩
  have hneg (a : ℝ) (ha : 0 < a) : ∃ b > a, laplace g b < 0 := by
    obtain ⟨b, hb, hB⟩ := (Filter.Eventually.and (eventually_gt_atTop a)
      (hr.eventually (Iio_mem_nhds hu))).exists
    exact ⟨b, hb, neg_of_mul_neg_right (show b*laplace g b < 0 from hB) (ha.trans hb).le⟩
  have hcont : ContinuousOn (laplace g) (Ioi 0) :=
    fun a ha ↦ (laplace_continuousAt hm hC ha).continuousWithinAt
  have hab (a b : ℝ) (ha : 0 < a) (hab : a ≤ b)
      (hA : 0 ≤ laplace g a) (hB : laplace g b ≤ 0) :
      ∃ t ∈ Icc a b, laplace g t = 0 :=
    intermediate_value_Icc' hab (hcont.mono (fun _ ht ↦ ha.trans_le ht.1)) ⟨hB, hA⟩
  obtain ⟨a, ha, hA⟩ := hpos 1 one_pos
  obtain ⟨b, hb, hB⟩ := hneg a ha.1
  obtain ⟨r, hr', hrz⟩ := hab a b ha.1 hb.le hA.le hB.le
  have hr₀ := ha.1.trans_le hr'.1
  have hlo (t : ℝ) (ht : 0 < t) (htr : t < r) : 0 < laplace g t := by
    by_contra! hn
    obtain ⟨s, hs, hS⟩ := hpos t ht
    obtain ⟨p, hp, hpz⟩ := hab s t hs.1 hs.2.le hS.le hn
    exact (hp.2.trans_lt htr).ne (huniq p r (hs.1.trans_le hp.1) hr₀ hpz hrz)
  have hhi (t : ℝ) (hrt : r < t) : laplace g t < 0 := by
    by_contra! hn
    have ht := hr₀.trans hrt
    obtain ⟨s, hs, hS⟩ := hneg t ht
    obtain ⟨p, hp, hpz⟩ := hab t s ht hs.le hn hS.le
    exact (hrt.trans_le hp.1).ne (huniq r p hr₀ (ht.trans_le hp.1) hrz hpz)
  have hinc : StrictMonoOn f (Ioc 0 r) := by
    apply strictMonoOn_of_deriv_pos (convex_Ioc _ _)
      (fun t ht ↦ (hd t ht.1).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ioc] at ht
    rw [(hd t ht.1).deriv]
    exact hlo t ht.1 ht.2
  have hdec : StrictAntiOn f (Ici r) := by
    apply strictAntiOn_of_deriv_neg (convex_Ici _)
      (fun t ht ↦ (hd t (hr₀.trans_le ht)).continuousAt.continuousWithinAt)
    intro t ht
    rw [interior_Ici] at ht
    rw [(hd t (hr₀.trans ht)).deriv]
    exact hhi t ht
  have hstrict (s : ℝ) (hs : 0 < s) (hne : s ≠ r) : f s < f r := by
    rcases lt_or_gt_of_ne hne with h | h
    · exact hinc ⟨hs, h.le⟩ ⟨hr₀, le_rfl⟩ h
    · exact hdec (show r ∈ Ici r by simp) (show s ∈ Ici r from h.le) h
  refine ⟨r, ⟨hr₀, ?_⟩, fun s hs ↦ ?_⟩
  · intro s hs
    rcases eq_or_ne s r with he | hne
    · simp [he]
    · exact (hstrict s hs hne).le
  · by_contra hn
    exact (not_le_of_gt (hstrict s hs.1 hn)) (hs.2 (show r ∈ Ioi (0 : ℝ) from hr₀))

/-- Beta-negative-binomial log mass in the shape order size, alpha, beta. -/
@[no_expose] def logMass (k : ℕ) (r a b : ℝ) : ℝ :=
  logGamma (a+r)+logGamma (a+b)-logGamma a-logGamma (a+r+b+(k : ℝ)) +
    (logGamma (r+(k : ℝ))-logGamma r+logGamma (b+(k : ℝ))-logGamma b-
      log (k.factorial : ℝ))

/-- Average beta-negative-binomial log likelihood. -/
def logLikelihood {n : ℕ} (x : Fin n → ℕ) (r a b : ℝ) : ℝ :=
  (∑ i, logMass (x i) r a b)/n

private theorem logGamma_shift_deriv {u v : ℝ} (h : 0 < u+v) :
    HasDerivAt (fun t ↦ logGamma (t+v)) (psi (u+v)) u := by
  simpa only [Function.comp_def, id_eq, mul_one] using
    (logGamma_deriv h).comp u ((hasDerivAt_id u).add_const v)

private theorem logMass_deriv_alpha (k : ℕ) {r a b : ℝ}
    (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ logMass k r t b)
      (gammaDifference a r-gammaDifference (a+b) (r+(k : ℝ))) a := by
  let K := logGamma (r+(k : ℝ))-logGamma r+logGamma (b+(k : ℝ))-logGamma b-log (k.factorial : ℝ)
  have h := ((((logGamma_shift_deriv (u := a) (v := r) (add_pos ha hr)).add
    (logGamma_shift_deriv (u := a) (v := b) (add_pos ha hb))).sub
      (logGamma_deriv ha)).sub
      (logGamma_shift_deriv (u := a) (v := r+b+(k : ℝ)) (by positivity))).add_const K
  convert h using 1
  · funext t
    have he : t+(r+b+(k : ℝ)) = t+r+b+(k : ℝ) := by ring
    change _ = logGamma (t+r)+logGamma (t+b)-logGamma t-logGamma (t+(r+b+(k : ℝ)))+K
    rw [he]
    rfl
  · rw [← psi_difference_integral ha hr.le,
      ← psi_difference_integral (add_pos ha hb) (show 0 ≤ r+(k : ℝ) by positivity)]
    rw [show a+b+(r+(k : ℝ)) = a+(r+b+(k : ℝ)) by ring]
    ring

private theorem logMass_deriv_size (k : ℕ) {r a b : ℝ}
    (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ logMass k t a b)
      (gammaDifference r (k : ℝ)-gammaDifference (r+a) (b+(k : ℝ))) r := by
  let K := logGamma (a+b)-logGamma a+logGamma (b+(k : ℝ))-logGamma b-log (k.factorial : ℝ)
  have h := ((((logGamma_shift_deriv (u := r) (v := a) (add_pos hr ha)).add
    (logGamma_shift_deriv (u := r) (v := (k : ℝ)) (by positivity))).sub
      (logGamma_deriv hr)).sub
      (logGamma_shift_deriv (u := r) (v := a+b+(k : ℝ)) (by positivity))).add_const K
  convert h using 1
  · funext t
    have he : t+(a+b+(k : ℝ)) = a+t+b+(k : ℝ) := by ring
    change logMass k t a b =
      logGamma (t+a)+logGamma (t+(k : ℝ))-logGamma t-logGamma (t+(a+b+(k : ℝ)))+K
    rw [he, add_comm t a]
    unfold logMass K
    ring
  · rw [← psi_difference_integral hr (Nat.cast_nonneg k),
      ← psi_difference_integral (add_pos hr ha) (show 0 ≤ b+(k : ℝ) by positivity)]
    rw [show r+a+(b+(k : ℝ)) = r+(a+b+(k : ℝ)) by ring]
    ring

private theorem integral_average_affine {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (G : Fin n → ℝ → ℝ) (P C D : ℝ → ℝ)
    (hi : ∀ i, IntegrableOn (G i) (Ioi 0))
    (he : ∀ i t, 0 < t → G i t = P t*(C t+D t*loss (x i) t)) :
    (∑ i, ∫ t in Ioi 0, G i t)/n =
      ∫ t in Ioi 0, P t*(C t+D t*avgLoss x t) := by
  rw [← integral_finsetSum Finset.univ (fun i _ ↦ hi i), ← integral_div]
  apply setIntegral_congr_fun measurableSet_Ioi
  intro t ht
  dsimp only
  simp_rw [he _ t ht]
  rw [← Finset.mul_sum, Finset.sum_add_distrib, ← Finset.mul_sum, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, avgLoss]
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  field_simp [hn']

private theorem loss_add (a b t : ℝ) :
    loss (a+b) t = loss a t+exp (-a*t)*loss b t := by
  unfold loss
  rw [show -(a+b)*t = -a*t+(-b*t) by ring, exp_add]
  ring

private theorem exp_neg_add (a b t : ℝ) :
    exp (-(a+b)*t) = exp (-a*t)*exp (-b*t) := by
  rw [← exp_add]
  congr 1
  ring

private theorem logLikelihood_deriv_alpha {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ logLikelihood x r t b)
      (laplace (alphaKernel x r b) a) a := by
  let G := fun i : Fin n ↦ fun t ↦ gammaDifferenceKernel a r t -
    gammaDifferenceKernel (a+b) (r+(x i : ℝ)) t
  have hg (i : Fin n) : IntegrableOn (G i) (Ioi 0) :=
    (gammaDifference_integrable ha hr.le).sub
      (gammaDifference_integrable (add_pos ha hb) (by positivity))
  have hraw := (HasDerivAt.fun_sum (u := (Finset.univ : Finset (Fin n)))
    (fun i _ ↦ logMass_deriv_alpha (x i) hr ha hb)).div_const (n : ℝ)
  have hsum (i : Fin n) : gammaDifference a r-gammaDifference (a+b) (r+(x i : ℝ)) =
      ∫ t in Ioi 0, G i t := by
    unfold G gammaDifference
    rw [integral_sub (gammaDifference_integrable ha hr.le)
      (gammaDifference_integrable (add_pos ha hb) (by positivity))]
  simp_rw [hsum] at hraw
  have he (i : Fin n) (t : ℝ) (_ht : 0 < t) :
      G i t = (exp (-a*t)/loss 1 t) *
        (loss r t*loss b t + (-exp (-(r+b)*t))*loss (x i) t) := by
    unfold G gammaDifferenceKernel
    rw [show 1-exp (-t) = loss 1 t by simp [loss]]
    change (exp (-a*t)*loss r t/loss 1 t -
      exp (-(a+b)*t)*loss (r+(x i : ℝ)) t/loss 1 t) = _
    rw [exp_neg_add a b, loss_add r (x i : ℝ), exp_neg_add r b]
    unfold loss
    ring
  rw [integral_average_affine hn x G _ _ _ hg he] at hraw
  convert hraw using 1
  · rfl
  · unfold laplace alphaKernel alphaNumer
    apply setIntegral_congr_fun measurableSet_Ioi
    intro t _
    dsimp only
    ring

private theorem logLikelihood_deriv_size {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ logLikelihood x t a b)
      (laplace (sizeKernel x a b) r) r := by
  let G := fun i : Fin n ↦ fun t ↦ gammaDifferenceKernel r (x i : ℝ) t -
    gammaDifferenceKernel (r+a) (b+(x i : ℝ)) t
  have hg (i : Fin n) : IntegrableOn (G i) (Ioi 0) :=
    (gammaDifference_integrable hr (Nat.cast_nonneg _)).sub
      (gammaDifference_integrable (add_pos hr ha) (by positivity))
  have hraw := (HasDerivAt.fun_sum (u := (Finset.univ : Finset (Fin n)))
    (fun i _ ↦ logMass_deriv_size (x i) hr ha hb)).div_const (n : ℝ)
  have hsum (i : Fin n) : gammaDifference r (x i : ℝ) -
      gammaDifference (r+a) (b+(x i : ℝ)) = ∫ t in Ioi 0, G i t := by
    unfold G gammaDifference
    rw [integral_sub (gammaDifference_integrable hr (Nat.cast_nonneg _))
      (gammaDifference_integrable (add_pos hr ha) (by positivity))]
  simp_rw [hsum] at hraw
  have he (i : Fin n) (t : ℝ) (_ht : 0 < t) :
      G i t = (exp (-r*t)/loss 1 t) *
        (-exp (-a*t)*loss b t + loss (a+b) t*loss (x i) t) := by
    unfold G gammaDifferenceKernel
    rw [show 1-exp (-t) = loss 1 t by simp [loss]]
    change (exp (-r*t)*loss (x i) t/loss 1 t -
      exp (-(r+a)*t)*loss (b+(x i : ℝ)) t/loss 1 t) = _
    rw [exp_neg_add r a, loss_add b (x i : ℝ)]
    unfold loss
    rw [exp_neg_add a b]
    ring
  rw [integral_average_affine hn x G _ _ _ hg he] at hraw
  convert hraw using 1
  · rfl
  · unfold laplace sizeKernel sizeNumer
    apply setIntegral_congr_fun measurableSet_Ioi
    intro t _
    dsimp only
    ring

/-- Holding size and beta positive, any sample with a positive count
has a unique finite alpha maximizer. -/
theorem held_beta_size {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    ∃! a, a ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r t b) (Ioi 0) a := by
  have hc := kernel_continuousOn (alphaNumer_continuous x r b)
  have hm : Measurable (alphaKernel x r b) :=
    (alphaNumer_continuous x r b).measurable.div (loss_continuous 1).measurable
  have h0 := alphaKernel_atZero x r b
  have hInf := alphaKernel_atTop x hr hb
  have hu := neg_neg_of_pos (countMean_pos hn hx)
  have hx' := kernel_crossing hc (alphaTest_strictMono x hr hb)
    (fun t ht ↦ div_pos (mul_pos (exp_pos _) ht) (loss_pos one_pos ht))
    (fun _ ht ↦ alphaKernel_factor x r b ht) hu one_pos h0 hInf
  exact laplace_unique_max hm hc h0 hInf hu one_pos hx'
    (fun _ ha ↦ logLikelihood_deriv_alpha hn x hr ha hb)

/-- Holding alpha and beta positive, any sample with a positive count
has a unique finite size maximizer. -/
theorem held_alpha_beta {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x t a b) (Ioi 0) r := by
  have hc := kernel_continuousOn (sizeNumer_continuous x a b)
  have hm : Measurable (sizeKernel x a b) :=
    (sizeNumer_continuous x a b).measurable.div (loss_continuous 1).measurable
  have h0 := sizeKernel_atZero x a b
  have hInf := sizeKernel_atTop x ha hb
  have hu := neg_neg_of_pos hb
  have hv := positiveFraction_pos hn hx
  have hx' := kernel_crossing hc (sizeTest_strictMono hn hx ha hb)
    (fun t ht ↦ div_pos (mul_pos (exp_pos _) (loss_pos hb ht)) (loss_pos one_pos ht))
    (fun _ ht ↦ sizeKernel_factor x hb ht) hu hv h0 hInf
  exact laplace_unique_max hm hc h0 hInf hu hv hx'
    (fun _ hr ↦ logLikelihood_deriv_size hn x hr ha hb)

/-- The beta-negative-binomial law is symmetric in size and beta. -/
theorem logLikelihood_swap {n : ℕ} (x : Fin n → ℕ) (r a b : ℝ) :
    logLikelihood x r a b = logLikelihood x b a r := by
  unfold logLikelihood
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  unfold logMass
  rw [show a+b+r+(x i : ℝ) = a+r+b+(x i : ℝ) by ring]
  ring

/-- Holding alpha and size positive, any sample with a positive count
has a unique finite beta maximizer. -/
theorem held_alpha_size {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a r : ℝ} (ha : 0 < a) (hr : 0 < r) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun t ↦ logLikelihood x r a t) (Ioi 0) b := by
  have he : (fun t ↦ logLikelihood x r a t) = (fun t ↦ logLikelihood x t a r) :=
    funext fun t ↦ logLikelihood_swap x r a t
  rw [he]
  exact held_alpha_beta hn hx ha hr

private theorem gammaDifference_pos {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    0 < gammaDifference a b := by
  have hpos (t : ℝ) (ht : 0 < t) : 0 < gammaDifferenceKernel a b t := by
    change 0 < exp (-a*t)*loss b t/(1-exp (-t))
    exact div_pos (mul_pos (exp_pos _) (loss_pos hb ht))
      (sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht)))
  unfold gammaDifference
  apply (setIntegral_pos_iff_support_of_nonneg_ae
    ((ae_restrict_iff' (measurableSet_Ioi (a := (0 : ℝ)))).mpr (Eventually.of_forall fun t ht ↦ (hpos t ht).le))
    (gammaDifference_integrable ha hb.le)).mpr
  have hS : Ioo (1 : ℝ) 2 ⊆ Function.support (gammaDifferenceKernel a b) ∩ Ioi 0 :=
    fun t ht ↦ ⟨(hpos t (zero_lt_one.trans ht.1)).ne', zero_lt_one.trans ht.1⟩
  exact lt_of_lt_of_le (by norm_num [Real.volume_Ioo] : 0 < volume (Ioo (1 : ℝ) 2)) (measure_mono hS)

private def zeroLogMass (r a b : ℝ) : ℝ :=
  logGamma (a+r)+logGamma (a+b)-logGamma a-logGamma (a+r+b)

private theorem logMass_zero (r a b : ℝ) : logMass 0 r a b = zeroLogMass r a b := by
  simp [logMass, zeroLogMass]

private theorem zeroLogMass_swap (r a b : ℝ) :
    zeroLogMass r a b = zeroLogMass b a r := by
  unfold zeroLogMass
  rw [show a+b+r = a+r+b by ring]
  ring

private theorem zeroLogMass_zero (a b : ℝ) : zeroLogMass 0 a b = 0 := by
  simp [zeroLogMass]

private theorem zeroLogMass_deriv_size {r a b : ℝ} (har : 0 < a+r) (hb : 0 < b) :
    HasDerivAt (fun t ↦ zeroLogMass t a b) (-gammaDifference (a+r) b) r := by
  let C := logGamma (a+b)-logGamma a
  have hd := ((logGamma_shift_deriv (u := r) (v := a) (by linarith)).sub
    (logGamma_shift_deriv (u := r) (v := a+b) (by linarith))).add_const C
  convert hd using 1
  · funext t
    change zeroLogMass t a b = logGamma (t+a)-logGamma (t+(a+b))+C
    rw [show t+a = a+t by ring, show t+(a+b) = a+t+b by ring]
    unfold zeroLogMass C
    ring
  · rw [← psi_difference_integral har hb.le]
    rw [show r+a = a+r by ring, show r+(a+b) = a+r+b by ring]
    ring

private theorem zeroLogMass_strictAnti {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    StrictAntiOn (fun r ↦ zeroLogMass r a b) (Ici 0) := by
  apply strictAntiOn_of_deriv_neg (convex_Ici _)
    (fun _ hr ↦ (zeroLogMass_deriv_size (add_pos_of_pos_of_nonneg ha hr) hb).continuousAt.continuousWithinAt)
  intro r hr
  rw [interior_Ici] at hr
  have h := add_pos ha (show 0 < r from hr)
  rw [(zeroLogMass_deriv_size h hb).deriv]
  exact neg_neg_of_pos (gammaDifference_pos h hb)

private theorem zeroLogMass_lt_zero {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    zeroLogMass r a b < 0 := by
  have h : zeroLogMass r a b < zeroLogMass 0 a b :=
    zeroLogMass_strictAnti ha hb (show (0 : ℝ) ∈ Ici 0 by simp)
      (show r ∈ Ici 0 from hr.le) hr
  rwa [zeroLogMass_zero] at h

private theorem zeroLogMass_size_atZero {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun r ↦ zeroLogMass r a b) (𝓝[>] (0 : ℝ)) (𝓝 0) := by
  have hd := zeroLogMass_deriv_size (r := 0) (a := a) (b := b) (by simpa using ha) hb
  have h : Tendsto (fun r ↦ zeroLogMass r a b) (𝓝[>] (0 : ℝ)) (𝓝 (zeroLogMass 0 a b)) :=
    hd.continuousAt.continuousWithinAt
  rwa [zeroLogMass_zero] at h

private theorem zeroLogMass_alpha_atTop {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    Tendsto (fun a ↦ zeroLogMass r a b) atTop (𝓝 0) := by
  obtain ⟨m, hm⟩ := exists_nat_ge r
  have hbound (a : ℝ) (ha : 0 < a) :
      -(b*(m : ℝ))/a ≤ zeroLogMass r a b := by
    let H := fun x ↦ logGamma (x+r)-logGamma x
    have hd (x : ℝ) (hx : 0 < x) : HasDerivAt H (gammaDifference x r) x := by
      rw [← psi_difference_integral hx hr.le]
      exact (logGamma_shift_deriv (add_pos hx hr)).sub (logGamma_deriv hx)
    have hc : ContinuousOn H (Icc a (a+b)) := fun x hx ↦
      (hd x (ha.trans_le hx.1)).continuousAt.continuousWithinAt
    have hd' : DifferentiableOn ℝ H (Ioo a (a+b)) := fun x hx ↦
      (hd x (ha.trans hx.1)).differentiableAt.differentiableWithinAt
    obtain ⟨x,hx,hx'⟩ := exists_deriv_eq_slope H (lt_add_of_pos_right a hb) hc hd'
    rw [(hd x (ha.trans hx.1)).deriv] at hx'
    have h := (gammaDifference_le (ha.trans hx.1) hr.le hm).trans
      (div_le_div_of_nonneg_left (Nat.cast_nonneg m) ha hx.1.le)
    rw [hx', show a+b-a = b by ring] at h
    have hh := (div_le_iff₀ hb).mp h
    unfold H at hh
    have he : a+b+r = a+r+b := by ring
    rw [he] at hh
    unfold zeroLogMass
    have he' : -(b*(m : ℝ))/a = -((m : ℝ)/a*b) := by ring
    rw [he']
    linarith only [hh]
  have hlo : Tendsto (fun a : ℝ ↦ -(b*(m : ℝ))/a) atTop (𝓝 0) := by
    simpa only [div_eq_mul_inv, mul_zero] using tendsto_inv_atTop_zero.const_mul (-(b*(m : ℝ)))
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hlo tendsto_const_nhds
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    exact hbound a ha
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    exact (zeroLogMass_lt_zero hr ha hb).le

private theorem all_zero_logLikelihood_eq {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} (hx : ∀ i, x i = 0) (r a b : ℝ) :
    logLikelihood x r a b = zeroLogMass r a b := by
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  simp only [logLikelihood, hx, logMass_zero, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, mul_div_cancel_left₀ _ hn']

/-- A nonempty all-zero sample has strictly negative average log likelihood
at every finite positive parameter vector. The three scalar boundary
paths have limiting log likelihood zero. -/
theorem all_zero_held_faces {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∀ i, x i = 0)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    logLikelihood x r a b < 0 ∧
      Tendsto (fun t ↦ logLikelihood x t a b) (𝓝[>] (0 : ℝ)) (𝓝 0) ∧
      Tendsto (fun t ↦ logLikelihood x r a t) (𝓝[>] (0 : ℝ)) (𝓝 0) ∧
      Tendsto (fun t ↦ logLikelihood x r t b) atTop (𝓝 0) := by
  simp_rw [all_zero_logLikelihood_eq hn hx]
  exact ⟨zeroLogMass_lt_zero hr ha hb, zeroLogMass_size_atZero ha hb,
    (zeroLogMass_size_atZero ha hr).congr (fun t ↦ (zeroLogMass_swap r a t).symm),
    zeroLogMass_alpha_atTop hr hb⟩

private def logRise (s : ℝ) (k : ℕ) : ℝ :=
  ∑ j ∈ Finset.range k, log (s+(j : ℝ))

private theorem logGamma_nat_add {s : ℝ} (hs : 0 < s) (k : ℕ) :
    logGamma (s+(k : ℝ)) = logGamma s+logRise s k := by
  induction k with
  | zero => simp [logRise]
  | succ k ih =>
    rw [Nat.cast_succ, ← add_assoc, logGamma_succ (by positivity), ih]
    unfold logRise
    rw [Finset.sum_range_succ]
    ring

private theorem logRise_one (k : ℕ) : logRise 1 k = log (k.factorial : ℝ) := by
  have h := logGamma_nat_add one_pos k
  rw [show (1 : ℝ)+(k : ℝ) = (k : ℝ)+1 by ring] at h
  unfold logGamma at h
  simpa only [Gamma_nat_eq_factorial, Gamma_one, log_one, zero_add] using h.symm

private theorem logMass_size_bound (k : ℕ) {r a b : ℝ}
    (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    logMass k r a b ≤ zeroLogMass r a b + logRise r k-log (k.factorial : ℝ) := by
  have hs : 0 < a+r+b := by positivity
  unfold logMass zeroLogMass
  rw [logGamma_nat_add hr, logGamma_nat_add hb, logGamma_nat_add hs]
  have h : logRise b k ≤ logRise (a+r+b) k := by
    unfold logRise
    apply Finset.sum_le_sum
    intro j _
    apply log_le_log (by positivity)
    linarith
  linarith only [h]

private theorem logMass_swap (k : ℕ) (r a b : ℝ) : logMass k r a b = logMass k b a r := by
  unfold logMass
  rw [show a+b+r+(k : ℝ) = a+r+b+(k : ℝ) by ring]
  ring

private theorem logMass_min_bound (k : ℕ) {r a b : ℝ}
    (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    logMass k r a b ≤ zeroLogMass r a b+logRise (min r b) k-log (k.factorial : ℝ) := by
  rcases le_total r b with h | h
  · rw [min_eq_left h]
    exact logMass_size_bound k hr ha hb
  · rw [min_eq_right h, logMass_swap k r a b, zeroLogMass_swap r a b]
    exact logMass_size_bound k hb ha hr

private theorem logRise_le_factorial {s : ℝ} (hs : 0 < s) (hs' : s ≤ 1) (k : ℕ) :
    logRise s k ≤ log (k.factorial : ℝ) := by
  rw [← logRise_one]
  apply Finset.sum_le_sum
  intro j _
  exact log_le_log (by positivity) (add_le_add_left hs' _)

private theorem logRise_small {s : ℝ} (hs : 0 < s) (hs' : s ≤ 1)
    {k : ℕ} (hk : 0 < k) : logRise s k-log (k.factorial : ℝ) ≤ log s := by
  let E := fun j : ℕ ↦ log (s+(j : ℝ))-log (1+(j : ℝ))
  have hE (j : ℕ) : E j ≤ 0 :=
    sub_nonpos.mpr (log_le_log (by positivity) (add_le_add_left hs' _))
  have hsum : (∑ j ∈ Finset.range k, E j) ≤ E 0 := by
    have h0 : (0 : ℕ) ∈ Finset.range k := Finset.mem_range.mpr hk
    have h := Finset.sum_le_sum_of_subset_of_nonpos (f := E)
      (Finset.singleton_subset_iff.mpr h0) (fun j _ _ ↦ hE j)
    simpa only [Finset.sum_singleton] using h
  rw [← logRise_one]
  change logRise s k-logRise 1 k ≤ log s
  unfold logRise
  rw [← Finset.sum_sub_distrib]
  simpa only [E, Nat.cast_zero, add_zero, log_one, sub_zero] using hsum

private theorem sample_bound_small {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    (hx : ∃ i, 0 < x i) {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b)
    (hs : min r b ≤ 1) :
    logLikelihood x r a b ≤ log (min r b)/(n : ℝ) := by
  let s := min r b
  have hs₀ : 0 < s := lt_min hr hb
  obtain ⟨i, hi⟩ := hx
  have h0 (j : Fin n) : logMass (x j) r a b ≤ 0 := by
    have h := logMass_min_bound (x j) hr ha hb
    have h' := logRise_le_factorial hs₀ hs (x j)
    linarith only [h, h', zeroLogMass_lt_zero hr ha hb]
  have hi' : logMass (x i) r a b ≤ log s := by
    have h := logMass_min_bound (x i) hr ha hb
    have h' := logRise_small hs₀ hs hi
    linarith only [h, h', zeroLogMass_lt_zero hr ha hb]
  unfold logLikelihood
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  apply div_le_div_of_nonneg_right _ hn'.le
  refine le_trans ?_ hi'
  simpa only [Finset.sum_singleton] using Finset.sum_le_sum_of_subset_of_nonpos
    (f := fun j : Fin n ↦ logMass (x j) r a b) (Finset.subset_univ {i})
    (fun j _ _ ↦ h0 j)

private theorem sample_bound_large {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) {K : ℝ}
    (_hK : 0 < K) (hx : ∀ i, (x i : ℝ) ≤ K) :
    logLikelihood x r a b ≤ zeroLogMass r a b + countMean x*log (min r b+K) := by
  let s := min r b
  have hs : 0 < s := lt_min hr hb
  have hi (i : Fin n) : logMass (x i) r a b ≤ zeroLogMass r a b + (x i : ℝ)*log (s+K) := by
    have hRise : logRise s (x i) ≤ (x i : ℝ)*log (s+K) := by
      unfold logRise
      calc
        (∑ j ∈ Finset.range (x i), log (s+(j : ℝ))) ≤ ∑ _j ∈ Finset.range (x i), log (s+K) := by
          apply Finset.sum_le_sum
          intro j hj
          have hj' : (j : ℝ) ≤ (x i : ℝ) := by exact_mod_cast (Finset.mem_range.mp hj).le
          exact log_le_log (by positivity) (add_le_add_right (hj'.trans (hx i)) _)
        _ = _ := by simp
    have hfact : 0 ≤ log ((x i).factorial : ℝ) :=
      log_nonneg (by exact_mod_cast Nat.factorial_pos (x i))
    linarith only [logMass_min_bound (x i) hr ha hb, hRise, hfact]
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  unfold logLikelihood countMean
  apply (div_le_iff₀ hn').mpr
  have h := Finset.sum_le_sum (s := (Finset.univ : Finset (Fin n))) (fun i _ ↦ hi i)
  simp only [Finset.sum_add_distrib, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul, ← Finset.sum_mul] at h
  field_simp [hn'.ne']
  linear_combination h

private theorem zeroLogMass_order {a r b u v : ℝ} (ha : 0 < a) (hu : 0 < u) (hv : 0 < v)
    (hr : u ≤ r) (hb : v ≤ b) :
    zeroLogMass r a b ≤ zeroLogMass u a v := by
  have hb' : 0 < b := hv.trans_le hb
  calc
    _ ≤ zeroLogMass u a b := (zeroLogMass_strictAnti ha hb').antitoneOn
      (show u ∈ Ici 0 from hu.le) (show r ∈ Ici 0 from hu.le.trans hr) hr
    _ ≤ zeroLogMass u a v := by
      rw [zeroLogMass_swap u a b, zeroLogMass_swap u a v]
      exact (zeroLogMass_strictAnti ha hu).antitoneOn
        (show v ∈ Ici 0 from hv.le) (show b ∈ Ici 0 from hv.le.trans hb) hb

private theorem zeroLogMass_size_atTop {a b : ℝ} (_ha : 0 < a) (hb : 0 < b) :
    Tendsto (fun r ↦ zeroLogMass r a b) atTop atBot := by
  let C := logGamma (a+b)-logGamma a
  have hbound (r : ℝ) (hr : 1 < a+r) :
      zeroLogMass r a b ≤ C-b*log (a+r-1) := by
    have hr' := zero_lt_one.trans hr
    have hb' := add_pos hr' hb
    have he : psi (a+r) ≤ slope logGamma (a+r) (a+r+b) :=
      Real.convexOn_log_Gamma.deriv_le_slope hr' hb' (lt_add_of_pos_right _ hb)
        (logGamma_deriv hr').differentiableAt
    rw [slope_def_field, show a+r+b-(a+r) = b by ring] at he
    have h' := (le_div_iff₀ hb).mp ((log_sub_one_le_psi hr).trans he)
    unfold zeroLogMass C
    linarith only [h']
  have ht : Tendsto (fun r : ℝ ↦ log (a+r-1)) atTop atTop := by
    apply tendsto_log_atTop.comp
    have h := tendsto_atTop_add_const_left atTop (a-1) (tendsto_id (x := atTop))
    exact h.congr (fun r ↦ by change a-1+r=a+r-1; ring)
  have hT : Tendsto (fun r : ℝ ↦ C-b*log (a+r-1)) atTop atBot := by
    simpa only [← neg_mul, sub_eq_add_neg] using tendsto_atBot_add_const_left atTop C
      (ht.const_mul_atTop_of_neg (neg_neg_of_pos hb))
  apply tendsto_atBot.mpr
  intro M
  filter_upwards [eventually_gt_atTop (1-a), hT.eventually (eventually_le_atBot M)] with r hr hh
  exact (hbound r (by linarith)).trans hh

private def tailSlope : ℝ := log (2/3 : ℝ)

private theorem tailSlope_neg : tailSlope < 0 := log_neg (by norm_num) (by norm_num)

private theorem zeroLogMass_diag_nat {a : ℝ} (ha : 0 < a) (m : ℕ) (hm : a ≤ m) :
    zeroLogMass (m : ℝ) a (m : ℝ) ≤ (m : ℝ)*tailSlope := by
  have h₁ := logGamma_nat_add ha m
  have h₂ := logGamma_nat_add (show 0 < a+(m : ℝ) by positivity) m
  have he : zeroLogMass (m : ℝ) a (m : ℝ) =
      logRise a m - logRise (a+(m : ℝ)) m := by
    unfold zeroLogMass
    linarith only [h₁,h₂]
  rw [he]
  unfold logRise
  rw [← Finset.sum_sub_distrib]
  calc
    (∑ j ∈ Finset.range m, (log (a+(j : ℝ))-log (a+(m : ℝ)+(j : ℝ)))) ≤
        ∑ _j ∈ Finset.range m, tailSlope := by
      apply Finset.sum_le_sum
      intro j hj
      have hj' : (j : ℝ) < m := by exact_mod_cast Finset.mem_range.mp hj
      have hp : 0 < a+(j : ℝ) := by positivity
      have hq : 0 < a+(m : ℝ)+(j : ℝ) := by positivity
      rw [← log_div hp.ne' hq.ne']
      apply log_le_log (div_pos hp hq)
      exact (div_le_iff₀ hq).mpr (by linarith only [hm,hj'])
    _ = _ := by simp

private theorem zeroLogMass_diag_tail {a s : ℝ} (ha : 0 < a) (hs : a+1 ≤ s) :
    zeroLogMass s a s ≤ tailSlope*(s-1) := by
  let m := ⌊s⌋₊
  have hs₀ : 0 ≤ s := by linarith
  have hm : (m : ℝ) ≤ s := Nat.floor_le hs₀
  have hm' : s < (m : ℝ)+1 := Nat.lt_floor_add_one s
  have ham : a ≤ (m : ℝ) := by linarith
  have hm₀ := ha.trans_le ham
  have h₁ := zeroLogMass_order ha hm₀ hm₀ hm hm
  have h₂ := zeroLogMass_diag_nat ha m ham
  have h₃ : (m : ℝ)*tailSlope ≤ tailSlope*(s-1) := by
    rw [mul_comm (m : ℝ)]
    exact mul_le_mul_of_nonpos_left (by linarith only [hm']) tailSlope_neg.le
  exact h₁.trans (h₂.trans h₃)

private theorem diagonal_log_bound_atTop (K M : ℝ) :
    Tendsto (fun s ↦ tailSlope*(s-1)+M*log (s+K)) atTop atBot := by
  have hratio : Tendsto (fun s ↦ log (s+K)/s) atTop (𝓝 0) := by
    have h := (tendsto_pow_log_div_mul_add_atTop 1 (-K) 1 one_ne_zero).comp
      (tendsto_atTop_add_const_right atTop K (tendsto_id (x := atTop)))
    exact h.congr fun s ↦ by
      change (log (s+K))^1/(1*(s+K)+(-K)) = log (s+K)/s
      rw [pow_one, show 1*(s+K)+(-K)=s by ring]
  have h : Tendsto (fun s ↦ tailSlope+M*(log (s+K)/s)) atTop (𝓝 tailSlope) := by
    simpa only [mul_zero, add_zero] using (hratio.const_mul M).const_add tailSlope
  have he := h.eventually (Iio_mem_nhds (show tailSlope < tailSlope/2 by linarith [tailSlope_neg]))
  have hlin : Tendsto (fun s : ℝ ↦ (tailSlope/2)*s-tailSlope) atTop atBot :=
    tendsto_atBot_add_const_right atTop (-tailSlope) (tendsto_id.const_mul_atTop_of_neg
      (show tailSlope/2 < 0 by linarith [tailSlope_neg]))
  apply tendsto_atBot.mpr
  intro B
  filter_upwards [he, hlin.eventually (eventually_le_atBot B), eventually_gt_atTop (0 : ℝ)]
    with s hs hB hs₀
  have hmul := mul_le_mul_of_nonneg_right (show tailSlope+M*(log (s+K)/s) ≤ tailSlope/2 from hs.le)
    hs₀.le
  have hcancel : log (s+K)/s*s = log (s+K) := div_mul_cancel₀ _ hs₀.ne'
  rw [add_mul, mul_assoc M, hcancel] at hmul
  change (tailSlope/2)*s-tailSlope ≤ B at hB
  linarith only [hmul,hB]

private theorem logLikelihood_pair_continuous {n : ℕ} (x : Fin n → ℕ) {a : ℝ} (ha : 0 < a) :
    ContinuousOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) := by
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
  have hG : ContinuousOn logGamma (Ioi 0) :=
    fun _ h ↦ (logGamma_deriv h).continuousAt.continuousWithinAt
  have hg (u : ℝ × ℝ → ℝ) (hc : Continuous u) (hu : ∀ p ∈ D, 0 < u p) :
      ContinuousOn (fun p ↦ logGamma (u p)) D :=
    hG.comp hc.continuousOn hu
  have hcell (k : ℕ) : ContinuousOn (fun p : ℝ × ℝ ↦ logMass k p.1 a p.2) D := by
    have h₁ := hg (fun p ↦ a+p.1) (by fun_prop) (fun p hp ↦ add_pos ha hp.1)
    have h₂ := hg (fun p ↦ a+p.2) (by fun_prop) (fun p hp ↦ add_pos ha hp.2)
    have h₃ := hg (fun p ↦ a+p.1+p.2+(k : ℝ)) (by fun_prop)
      (fun p hp ↦ by
        have h₁ : 0 < p.1 := hp.1
        have h₂ : 0 < p.2 := hp.2
        positivity)
    have h₄ := hg (fun p ↦ p.1+(k : ℝ)) (by fun_prop)
      (fun p hp ↦ add_pos_of_pos_of_nonneg hp.1 (Nat.cast_nonneg k))
    have h₅ := hg (fun p ↦ p.2+(k : ℝ)) (by fun_prop)
      (fun p hp ↦ add_pos_of_pos_of_nonneg hp.2 (Nat.cast_nonneg k))
    have h₆ := hg Prod.fst continuous_fst (fun _ hp ↦ hp.1)
    have h₇ := hg Prod.snd continuous_snd (fun _ hp ↦ hp.2)
    exact (((h₁.add h₂).sub continuousOn_const).sub h₃).add
      (((((h₄.sub h₆).add h₅).sub h₇).sub continuousOn_const))
  exact (continuousOn_finsetSum (Finset.univ : Finset (Fin n))
    (fun i _ ↦ hcell (x i))).div_const (n : ℝ)

/-- With alpha held positive, any sample with a positive count has an
attained finite joint fit in size and beta. -/
theorem held_alpha_attained {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a : ℝ} (ha : 0 < a) :
    ∃ p ∈ Ioi (0 : ℝ) ×ˢ Ioi 0,
      IsMaxOn (fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2) (Ioi 0 ×ˢ Ioi 0) p := by
  let f := fun p : ℝ × ℝ ↦ logLikelihood x p.1 a p.2
  let c : ℝ × ℝ := (1,1)
  let B := f c
  let A := min B 0-1
  have hA : A < B ∧ A < 0 := by
    dsimp [A]
    exact ⟨sub_lt_self _ zero_lt_one |>.trans_le (min_le_left _ _),
      sub_lt_self _ zero_lt_one |>.trans_le (min_le_right _ _)⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let δ := exp ((n : ℝ)*A)
  have hδ : 0 < δ := exp_pos _
  have hδ₁ : δ < 1 := exp_lt_one_iff.mpr (mul_neg_of_pos_of_neg hn' hA.2)
  have hδB : log δ/(n : ℝ) < B := by
    dsimp [δ]
    rw [log_exp, mul_div_cancel_left₀ _ hn'.ne']
    exact hA.1
  let K : ℝ := 1+∑ i, (x i : ℝ)
  have hK : 0 < K := add_pos_of_pos_of_nonneg one_pos
    (Finset.sum_nonneg fun _ _ ↦ Nat.cast_nonneg _)
  have hxK (i : Fin n) : (x i : ℝ) ≤ K :=
    (Finset.single_le_sum (f := fun i : Fin n ↦ (x i : ℝ))
      (fun _ _ ↦ Nat.cast_nonneg _) (Finset.mem_univ i)).trans
      (by dsimp [K]; linarith)
  have hmean : 0 < countMean x := countMean_pos hn hx
  obtain ⟨M₀, hM₀⟩ := eventually_atTop.mp
    ((diagonal_log_bound_atTop K (countMean x)).eventually (eventually_lt_atBot B))
  let M := max M₀ (a+1)
  have hM : 0 < M := (add_pos ha one_pos).trans_le (le_max_right _ _)
  obtain ⟨R₀, hR₀⟩ := eventually_atTop.mp
    ((tendsto_atBot_add_const_right atTop (countMean x*log (M+K))
      (zeroLogMass_size_atTop ha hδ)).eventually (eventually_lt_atBot B))
  let R := max (max R₀ M) 1
  let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
  let Q : Set (ℝ × ℝ) := Icc δ R ×ˢ Icc δ R
  have hQ : IsCompact Q := isCompact_Icc.prod isCompact_Icc
  have hsub : Q ⊆ D := fun p hp ↦ ⟨hδ.trans_le hp.1.1, hδ.trans_le hp.2.1⟩
  have hcQ : c ∈ Q := ⟨⟨hδ₁.le, le_max_right _ _⟩, hδ₁.le, le_max_right _ _⟩
  have hcompact (p : ℝ × ℝ) (hp : p ∈ D) (hB : B ≤ f p) : p ∈ Q := by
    let s := min p.1 p.2
    let t := max p.1 p.2
    have ha₁ : 0 < p.1 := hp.1
    have ha₂ : 0 < p.2 := hp.2
    have hs : 0 < s := lt_min ha₁ ha₂
    have hsδ : δ ≤ s := by
      by_contra! h
      have hh := sample_bound_small hn hx ha₁ ha ha₂ (h.le.trans hδ₁.le)
      have hlog := div_lt_div_of_pos_right (log_lt_log hs h) hn'
      exact (not_lt_of_ge hB) (hh.trans_lt (hlog.trans hδB))
    have hM' : s < M := by
      by_contra! h
      have hZ := zeroLogMass_order ha hs hs (min_le_left _ _) (min_le_right _ _)
      have hZ' := zeroLogMass_diag_tail ha ((le_max_right _ _).trans h)
      have hL := sample_bound_large hn x ha₁ ha ha₂ hK hxK
      have htail := hM₀ s ((le_max_left _ _).trans h)
      change tailSlope*(s-1)+countMean x*log (s+K) < B at htail
      exact (not_lt_of_ge hB) ((hL.trans (add_le_add_left (hZ.trans hZ') _)).trans_lt htail)
    have htR : t ≤ R := by
      by_contra! ht
      have hz : zeroLogMass p.1 a p.2 = zeroLogMass t a s := by
        rcases le_total p.1 p.2 with he | he
        · simpa only [t, s, max_eq_right he, min_eq_left he] using zeroLogMass_swap p.1 a p.2
        · simp only [t, s, max_eq_left he, min_eq_right he]
      have hm : 0 < t := ha₁.trans_le (le_max_left _ _)
      have hZ := hz.trans_le (zeroLogMass_order ha hm hδ le_rfl hsδ)
      have hL := sample_bound_large hn x ha₁ ha ha₂ hK hxK
      have hlog := log_le_log (add_pos hs hK) (add_le_add_left hM'.le K)
      have hupper := add_le_add hZ (mul_le_mul_of_nonneg_left hlog hmean.le)
      have hh := hR₀ t (((le_max_left R₀ M).trans (le_max_left _ _)).trans ht.le)
      change zeroLogMass t a δ+countMean x*log (M+K) < B at hh
      exact (not_lt_of_ge hB) ((hL.trans hupper).trans_lt hh)
    exact ⟨⟨hsδ.trans (min_le_left _ _), (le_max_left _ _).trans htR⟩,
      hsδ.trans (min_le_right _ _), (le_max_right _ _).trans htR⟩
  obtain ⟨p,hp,hm⟩ := hQ.exists_isMaxOn ⟨c,hcQ⟩ ((logLikelihood_pair_continuous x ha).mono hsub)
  refine ⟨p, hsub hp, fun q hq ↦ ?_⟩
  by_cases h : B ≤ f q
  · exact hm (hcompact q hq h)
  · exact (lt_of_not_ge h).le.trans (hm hcQ)

/-- Fixing all three coefficients gives a singleton maximizer. -/
theorem fully_held {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ × ℝ) :
    ∃! q, q ∈ ({p} : Set (ℝ × ℝ × ℝ)) ∧
      IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ logLikelihood x q.1 q.2.1 q.2.2) {p} q := by
  refine ⟨p, ⟨mem_singleton _, ?_⟩, fun q h ↦ mem_singleton_iff.mp h.1⟩
  rintro q hq
  have h := mem_singleton_iff.mp hq
  subst q
  change logLikelihood x p.1 p.2.1 p.2.2 ≤ logLikelihood x p.1 p.2.1 p.2.2
  exact le_rfl

end

end UnivariateML.Discrete3BNBinom

end
