module

public import UnivariateML.Family.Discrete3BNBinom
import Mathlib.Tactic

set_option linter.style.header false

/-!
# One-free-shape beta-negative-binomial score rules

The earlier module proves the maximum but keeps the actual score representation
and sign proofs private. This module derives the score rule for the explicit
normalized log mass. The analytic steps from `Discrete3BNBinom` are restated
locally because its log mass body and score lemmas are hidden across modules.

The result is for the positive-count branch of each one-free-shape mask.
The excluded all-zero suprema are already covered by
`Discrete3BNBinom.all_zero_held_faces`.
-/

@[expose] public section

open Set Real MeasureTheory Filter
open scoped Topology BigOperators
open UnivariateML.Discrete3BNBinom

namespace UnivariateML.FinishBNBinom

noncomputable section

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
    (hp : ∀ t > 0, 0 < p t) (he : ∀ t > 0, g t = p t * F t)
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
    (hpoint : ∀ t > 0, Tendsto (fun a ↦ g (t / a)) l (𝓝 u)) :
    Tendsto (fun a ↦ a*laplace g a) l (𝓝 u) := by
  have ht : Tendsto (fun a ↦ ∫ t in Ioi 0, exp (-t)*g (t / a)) l
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

private theorem laplace_score_peak {f g : ℝ → ℝ} {u v : ℝ}
    (hm : Measurable g) (hc : ContinuousOn g (Ioi 0))
    (h0 : Tendsto g (𝓝[>] (0 : ℝ)) (𝓝 u)) (hInf : Tendsto g atTop (𝓝 v))
    (hu : u < 0) (hv : 0 < v)
    (hcross : ∃ c > 0, (∀ t ∈ Ioo 0 c, g t < 0) ∧ (∀ t > c, 0 < g t))
    (hd : ∀ a > 0, HasDerivAt f (laplace g a) a) :
    ∃ r > 0, HasDerivAt f 0 r ∧
      (∀ a ∈ Ioo 0 r, 0 < deriv f a) ∧
      (∀ a > r, deriv f a < 0) ∧
      (∀ a > 0, IsMaxOn f (Ioi 0) a ↔ a = r) := by
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
  refine ⟨r, hr₀, hrz ▸ hd r hr₀, ?_, ?_, ?_⟩
  · intro a ha
    rw [(hd a ha.1).deriv]
    exact hlo a ha.1 ha.2
  · intro a ha
    rw [(hd a (hr₀.trans ha)).deriv]
    exact hhi a ha
  · intro a ha
    constructor
    · intro hm
      by_contra hne
      exact (hstrict a ha hne).not_ge (hm (show r ∈ Ioi (0 : ℝ) from hr₀))
    · intro he
      subst a
      intro t ht
      rcases eq_or_ne t r with rfl | hne
      · exact le_rfl
      · exact (hstrict t ht hne).le

/-- Beta-negative-binomial log mass in the shape order size, alpha, beta. -/
private def explicitLogMass (k : ℕ) (r a b : ℝ) : ℝ :=
  logGamma (a+r)+logGamma (a+b)-logGamma a-logGamma (a+r+b+(k : ℝ)) +
    (logGamma (r+(k : ℝ))-logGamma r+logGamma (b+(k : ℝ))-logGamma b-
      log (k.factorial : ℝ))

/-- The sample mean of the normalized beta-negative-binomial log mass. -/
def averageLogMass {n : ℕ} (x : Fin n → ℕ) (r a b : ℝ) : ℝ :=
  (∑ i, (log (Gamma (a+r)) + log (Gamma (a+b)) - log (Gamma a) -
    log (Gamma (a+r+b+(x i : ℝ))) +
      (log (Gamma (r+(x i : ℝ))) - log (Gamma r) + log (Gamma (b+(x i : ℝ))) -
        log (Gamma b) - log ((x i).factorial : ℝ)))) / n

private theorem logGamma_shift_deriv {u v : ℝ} (h : 0 < u + v) :
    HasDerivAt (fun t ↦ logGamma (t+v)) (psi (u+v)) u := by
  simpa only [Function.comp_def, id_eq, mul_one] using
    (logGamma_deriv h).comp u ((hasDerivAt_id u).add_const v)

private theorem logMass_deriv_alpha (k : ℕ) {r a b : ℝ}
    (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ explicitLogMass k r t b)
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
    HasDerivAt (fun t ↦ explicitLogMass k t a b)
      (gammaDifference r (k : ℝ)-gammaDifference (r+a) (b+(k : ℝ))) r := by
  let K := logGamma (a+b)-logGamma a+logGamma (b+(k : ℝ))-logGamma b-log (k.factorial : ℝ)
  have h := ((((logGamma_shift_deriv (u := r) (v := a) (add_pos hr ha)).add
    (logGamma_shift_deriv (u := r) (v := (k : ℝ)) (by positivity))).sub
      (logGamma_deriv hr)).sub
      (logGamma_shift_deriv (u := r) (v := a+b+(k : ℝ)) (by positivity))).add_const K
  convert h using 1
  · funext t
    have he : t+(a+b+(k : ℝ)) = a+t+b+(k : ℝ) := by ring
    change explicitLogMass k t a b =
      logGamma (t+a)+logGamma (t+(k : ℝ))-logGamma t-logGamma (t+(a+b+(k : ℝ)))+K
    rw [he, add_comm t a]
    unfold explicitLogMass K
    ring
  · rw [← psi_difference_integral hr (Nat.cast_nonneg k),
      ← psi_difference_integral (add_pos hr ha) (show 0 ≤ b+(k : ℝ) by positivity)]
    rw [show r+a+(b+(k : ℝ)) = r+(a+b+(k : ℝ)) by ring]
    ring

private theorem integral_average_affine {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (G : Fin n → ℝ → ℝ) (P C D : ℝ → ℝ)
    (hi : ∀ i, IntegrableOn (G i) (Ioi 0))
    (he : ∀ i t, 0 < t → G i t = P t * (C t + D t * loss (x i) t)) :
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

private theorem averageLogMass_deriv_alpha {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ averageLogMass x r t b)
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

private theorem averageLogMass_deriv_size {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {r a b : ℝ} (hr : 0 < r) (ha : 0 < a) (hb : 0 < b) :
    HasDerivAt (fun t ↦ averageLogMass x t a b)
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
has exactly one positive alpha score root. The score is positive before
it and negative after it, and the root is the unique maximizer. -/
theorem alpha_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r b : ℝ} (hr : 0 < r) (hb : 0 < b) :
    let f := fun t ↦ averageLogMass x r t b
    ∃ a > 0, HasDerivAt f 0 a ∧
      (∀ t ∈ Ioo 0 a, 0 < deriv f t) ∧ (∀ t > a, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = a) := by
  have hc := kernel_continuousOn (alphaNumer_continuous x r b)
  have hm : Measurable (alphaKernel x r b) :=
    (alphaNumer_continuous x r b).measurable.div (loss_continuous 1).measurable
  have h0 := alphaKernel_atZero x r b
  have hInf := alphaKernel_atTop x hr hb
  have hu := neg_neg_of_pos (countMean_pos hn hx)
  have hx' := kernel_crossing hc (alphaTest_strictMono x hr hb)
    (fun t ht ↦ div_pos (mul_pos (exp_pos _) ht) (loss_pos one_pos ht))
    (fun _ ht ↦ alphaKernel_factor x r b ht) hu one_pos h0 hInf
  exact laplace_score_peak hm hc h0 hInf hu one_pos hx'
    (fun _ ha ↦ averageLogMass_deriv_alpha hn x hr ha hb)

/-- Holding alpha and beta positive, any sample with a positive count
has exactly one positive size score root. The score changes from
positive to negative there, and the root is the unique maximizer. -/
theorem size_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) :
    let f := fun t ↦ averageLogMass x t a b
    ∃ r > 0, HasDerivAt f 0 r ∧
      (∀ t ∈ Ioo 0 r, 0 < deriv f t) ∧ (∀ t > r, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = r) := by
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
  exact laplace_score_peak hm hc h0 hInf hu hv hx'
    (fun _ hr ↦ averageLogMass_deriv_size hn x hr ha hb)

private theorem averageLogMass_swap {n : ℕ} (x : Fin n → ℕ) (r a b : ℝ) :
    averageLogMass x r a b = averageLogMass x b a r := by
  change (∑ i, explicitLogMass (x i) r a b) / n =
    (∑ i, explicitLogMass (x i) b a r) / n
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  unfold explicitLogMass
  rw [show a+b+r+(x i : ℝ) = a+r+b+(x i : ℝ) by ring]
  ring

/-- The beta score has the same single-root and maximizing properties
by exchanging size and beta in the normalized mass. -/
theorem beta_score {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} (hx : ∃ i, 0 < x i)
    {r a : ℝ} (hr : 0 < r) (ha : 0 < a) :
    let f := fun t ↦ averageLogMass x r a t
    ∃ b > 0, HasDerivAt f 0 b ∧
      (∀ t ∈ Ioo 0 b, 0 < deriv f t) ∧ (∀ t > b, deriv f t < 0) ∧
      (∀ t > 0, IsMaxOn f (Ioi 0) t ↔ t = b) := by
  dsimp only
  rw [show (fun t ↦ averageLogMass x r a t) = (fun t ↦ averageLogMass x t a r) from
    funext fun t ↦ averageLogMass_swap x r a t]
  exact size_score hn hx ha hr

end

end UnivariateML.FinishBNBinom
