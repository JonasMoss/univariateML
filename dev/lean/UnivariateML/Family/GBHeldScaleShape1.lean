module

public import UnivariateML.Family.Base9GenBeta
public import UnivariateML.Generic.Compactification

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Joint shape-power bounds at fixed endpoint

At fixed first shape, compact superlevels give the complete existence
criterion. The constant-data rate test classifies likelihood divergence
without a statement about convergence of the fitted laws. A separate
comparison at fixed upper shape above one rules out the power face as
a global winner, leaving the gamma comparison open.
-/

@[expose] public section

open Real Set Filter
open scoped Topology

namespace UnivariateML.GBHeldScaleShape1

noncomputable section

variable {n : ℕ}

private def penalty (y : Fin n → ℝ) (a : ℝ) : ℝ :=
  -(∑ i, log (1 - exp (-(a * y i)))) / n

private def avg (y : Fin n → ℝ) : ℝ := (∑ i, y i) / n

private def objective (y : Fin n → ℝ) (p : ℝ) (v : ℝ × ℝ) :=
  log v.2 - p * v.2 * avg y + (1 - v.1) * penalty y v.2 -
    Beta.logBeta (p, v.1)

private theorem logGamma_cont {x : ℝ} (hx : 0 < x) :
    ContinuousAt (fun x ↦ log (Gamma x)) x := by
  apply ContinuousAt.log _ (Gamma_pos_of_pos hx).ne'
  exact (differentiableAt_Gamma (fun m ↦ by linarith [Nat.cast_nonneg (α := ℝ) m])).continuousAt

private theorem logBeta_formula {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    -Beta.logBeta (p, q) = log (Gamma (p + q)) - log (Gamma q) - log (Gamma p) := by
  unfold Beta.logBeta Beta.normalizer
  dsimp
  rw [log_div (mul_ne_zero (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne')
    (Gamma_pos_of_pos (add_pos hp hq)).ne',
    log_mul (Gamma_pos_of_pos hp).ne' (Gamma_pos_of_pos hq).ne']
  ring

private theorem logBeta_bounds {p : ℝ} (hp : 0 < p) : ∃ C : ℝ,
    (∀ q ∈ Ioc (0 : ℝ) 1, -Beta.logBeta (p, q) ≤ log q + C) ∧
    (∀ q ∈ Ici (1 : ℝ), -Beta.logBeta (p, q) ≤ p * log q + C) := by
  let f := fun q : ℝ ↦ log (Gamma (p + q)) - log (Gamma (q + 1)) - log (Gamma p)
  let g := fun q : ℝ ↦ log (Gamma (q + p)) - log (Gamma q) - p * log q
  have hf : ContinuousOn f (Icc 0 1) := by
    intro q hq
    have h₁ := (logGamma_cont (add_pos_of_pos_of_nonneg hp hq.1)).comp
      (f := fun q : ℝ ↦ p + q) (continuousAt_const.add continuousAt_id)
    have h₂ := (logGamma_cont (add_pos_of_nonneg_of_pos hq.1 one_pos)).comp
      (f := fun q : ℝ ↦ q + 1) (continuousAt_id.add continuousAt_const)
    exact ((h₁.sub h₂).sub continuousAt_const).continuousWithinAt
  have hg : ContinuousOn g (Ici 1) := by
    intro q hq
    have hq₀ : 0 < q := zero_lt_one.trans_le hq
    have h₁ := (logGamma_cont (add_pos hq₀ hp)).comp
      (f := fun q : ℝ ↦ q + p) (continuousAt_id.add continuousAt_const)
    exact ((h₁.sub (logGamma_cont hq₀)).sub
      (continuousAt_const.mul (continuousAt_log hq₀.ne'))).continuousWithinAt
  obtain ⟨u, _, hu⟩ := isCompact_Icc.exists_isMaxOn
    (nonempty_Icc.mpr (zero_le_one : (0 : ℝ) ≤ 1)) hf
  obtain ⟨Q, hQ⟩ := eventually_atTop.mp
    ((Endpoint3GammaRatio.log_gamma_ratio p hp).eventually
      (gt_mem_nhds (zero_lt_one : (0 : ℝ) < 1)))
  obtain ⟨v, _, hv⟩ := isCompact_Icc.exists_isMaxOn
    (nonempty_Icc.mpr (le_max_left (1 : ℝ) Q)) (hg.mono (fun _ h ↦ h.1))
  refine ⟨max (f u) (max (1 - log (Gamma p)) (g v - log (Gamma p))), ?_, ?_⟩
  · intro q hq
    rw [logBeta_formula hp hq.1]
    have he : f q = log (Gamma (p + q)) - log (Gamma q) - log q - log (Gamma p) := by
      dsimp [f]
      rw [Gamma_add_one hq.1.ne', log_mul hq.1.ne' (Gamma_pos_of_pos hq.1).ne']
      ring
    linarith [show f q ≤ max (f u) (max (1 - log (Gamma p)) (g v - log (Gamma p))) from
      (hu (show q ∈ Icc 0 1 from ⟨hq.1.le, hq.2⟩)).trans (le_max_left _ _)]
  · intro q hq
    rw [logBeta_formula hp (zero_lt_one.trans_le hq)]
    have he : log (Gamma (p + q)) - log (Gamma q) - log (Gamma p) =
        p * log q + (g q - log (Gamma p)) := by dsimp [g]; rw [add_comm p q]; ring
    rw [he]
    apply add_le_add_right
    by_cases hq' : q ≤ max 1 Q
    · exact (sub_le_sub_right (hv (show q ∈ Icc 1 (max 1 Q) from ⟨hq, hq'⟩)) _).trans
        ((le_max_right _ _).trans (le_max_right _ _))
    · have hlt : g q < 1 := hQ q ((le_max_right _ _).trans (le_of_not_ge hq'))
      exact (sub_le_sub_right hlt.le _).trans ((le_max_left _ _).trans (le_max_right _ _))

private theorem gap_pos {t : ℝ} (ht : 0 < t) : 0 < 1 - exp (-t) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos ht))

private theorem point_penalty {t : ℝ} (ht : 0 < t) :
    exp (-t) ≤ -log (1 - exp (-t)) ∧
    -log t ≤ -log (1 - exp (-t)) ∧
    -log (1 - exp (-t)) ≤ t - log t := by
  refine ⟨?_, ?_, ?_⟩
  · linarith [log_le_sub_one_of_pos (gap_pos ht)]
  · exact neg_le_neg (log_le_log (gap_pos ht) (by linarith [add_one_le_exp (-t)]))
  · have h : t * exp (-t) ≤ 1 - exp (-t) := by
      have h := mul_le_mul_of_nonneg_right (add_one_le_exp t) (exp_pos (-t)).le
      rw [← exp_add, add_neg_cancel, exp_zero] at h
      linarith
    have hh := log_le_log (mul_pos ht (exp_pos _)) h
    rw [log_mul ht.ne' (exp_pos _).ne', log_exp] at hh
    linarith

private theorem penalty_bounds (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 < y i) {a : ℝ} (ha : 0 < a) :
    (∀ j, exp (-(a * y j)) / n ≤ penalty y a) ∧
    -log a - avg (fun i ↦ log (y i)) ≤ penalty y a ∧
    penalty y a ≤ a * avg y - log a - avg (fun i ↦ log (y i)) := by
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have h (i : Fin n) := point_penalty (mul_pos ha (hy i))
  refine ⟨?_, ?_, ?_⟩
  · intro j
    unfold penalty
    rw [← Finset.sum_neg_distrib]
    apply div_le_div_of_nonneg_right _ hN.le
    exact (h j).1.trans (Finset.single_le_sum
      (fun i _ ↦ (exp_pos _).le.trans (h i).1) (Finset.mem_univ j))
  · unfold penalty avg
    apply (le_div_iff₀ hN).mpr
    rw [← Finset.sum_neg_distrib]
    have H := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ (h i).2.1)
    simp only [log_mul ha.ne' (hy _).ne', neg_add, Finset.sum_add_distrib, Finset.sum_neg_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at H
    convert H using 1
    · field_simp
      ring
    · rw [Finset.sum_neg_distrib]
  · unfold penalty avg
    apply (div_le_iff₀ hN).mpr
    rw [← Finset.sum_neg_distrib]
    have H := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ (h i).2.2)
    simp only [log_mul ha.ne' (hy _).ne', sub_add_eq_sub_sub, Finset.sum_sub_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, ← Finset.mul_sum] at H
    field_simp
    linarith

private theorem penalty_antitone {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) : penalty y b ≤ penalty y a := by
  unfold penalty
  apply div_le_div_of_nonneg_right _ (Nat.cast_nonneg n)
  apply neg_le_neg
  apply Finset.sum_le_sum
  intro i _
  apply log_le_log (gap_pos (mul_pos ha (hy i)))
  exact sub_le_sub_left (exp_le_exp.mpr (neg_le_neg
    (mul_le_mul_of_nonneg_right hab (hy i).le))) _

private theorem avg_pos (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 < y i) : 0 < avg y := by
  exact div_pos (Finset.sum_pos (fun i _ ↦ hy i) ⟨⟨0, hn⟩, Finset.mem_univ _⟩)
    (Nat.cast_pos.mpr hn)

private theorem log_minus_linear {d : ℝ} (hd : 0 < d) (C : ℝ) :
    Tendsto (fun a : ℝ ↦ log a - d * a + C) atTop atBot := by
  have hlog : Tendsto (fun a : ℝ ↦ log a / a) atTop (𝓝 0) := by
    simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
  have h := (hlog.eventually (gt_mem_nhds (half_pos hd))).and
    (eventually_gt_atTop (0 : ℝ))
  have hlin : Tendsto (fun a : ℝ ↦ -(d / 2) * a + C) atTop atBot :=
    tendsto_atBot_add_const_right _ C
      (tendsto_id.const_mul_atTop_of_neg (show -(d / 2) < 0 by linarith))
  apply tendsto_atBot_mono' atTop _ hlin
  filter_upwards [h] with a ha
  have := (div_lt_iff₀ ha.2).mp ha.1
  linarith

private theorem small_power_tail (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 < y i) {p : ℝ} (hp : 0 < p) (L : ℝ) :
    ∀ᶠ a : ℝ in 𝓝[>] 0, ∀ q > 0, objective y p (q, a) < L := by
  obtain ⟨C, hsmall, hlarge⟩ := logBeta_bounds hp
  let m := avg y
  let g := avg (fun i ↦ log (y i))
  have hm : 0 < m := avg_pos hn hy
  let T := fun a : ℝ ↦ -log a - g
  have hT : Tendsto T (𝓝[>] 0) atTop :=
    tendsto_atTop_add_const_right _ (-g) (tendsto_neg_atBot_atTop.comp tendsto_log_nhdsGT_zero)
  have hfirst : Tendsto (fun a : ℝ ↦ m - g + C - 1 - log (T a))
      (𝓝[>] 0) atBot :=
    tendsto_atBot_add_const_left _ _ (tendsto_neg_atTop_atBot.comp (tendsto_log_atTop.comp hT))
  have hsecond : Tendsto (fun a : ℝ ↦ log a + C) (𝓝[>] 0) atBot :=
    tendsto_atBot_add_const_right _ C tendsto_log_nhdsGT_zero
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ)),
    (nhdsGT_basis (0 : ℝ)).mem_of_mem zero_lt_one,
    hT.eventually_gt_atTop p, hfirst.eventually_lt_atBot L,
    hsecond.eventually_lt_atBot L] with a ha ha₁ hT₀ hF hS q hq
  have hR := penalty_bounds hn hy ha
  have hpos : 0 < penalty y a := hp.trans (hT₀.trans_le hR.2.1)
  have hneg := mul_nonneg (mul_nonneg hp.le ha.le) hm.le
  by_cases hq₁ : q ≤ 1
  · have hopt := log_le_sub_one_of_pos (mul_pos hq hpos)
    rw [log_mul hq.ne' hpos.ne'] at hopt
    have hlog := log_le_log (hp.trans hT₀) hR.2.1
    have ham : a * m ≤ m := by simpa using
      mul_le_mul_of_nonneg_right (show a ≤ 1 from ha₁.2.le) hm.le
    have hb := hsmall q ⟨hq, hq₁⟩
    change m - g + C - 1 - log (T a) < L at hF
    unfold objective
    dsimp only
    dsimp [T] at hF hlog
    dsimp [m, g] at ham hneg
    nlinarith [hR.2.2]
  · have hql : 1 ≤ q := le_of_not_ge hq₁
    have hlog := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hq) hp.le
    have hmul := mul_le_mul_of_nonneg_left (le_of_lt (hT₀.trans_le hR.2.1))
      (sub_nonneg.mpr hql)
    have hb := hlarge q hql
    unfold objective
    dsimp only
    nlinarith

private theorem power_box (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {p : ℝ} (hp : 0 < p) (j : Fin n) (hj : y j < avg y) (L : ℝ) :
    ∃ A B : ℝ, 0 < A ∧ ∀ q a : ℝ, 0 < q → 0 < a →
      L ≤ objective y p (q, a) → a ∈ Icc A B := by
  obtain ⟨C, hsmall, hlarge⟩ := logBeta_bounds hp
  let m := avg y
  have hm : 0 < m := avg_pos hn hy
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  obtain ⟨A, hA, hbelow⟩ := (nhdsGT_basis (0 : ℝ)).eventually_iff.mp (small_power_tail hn hy hp L)
  have hlargeEnv : ∀ᶠ a : ℝ in atTop, ∀ q > 0, objective y p (q, a) < L := by
    let d := p * (m - y j)
    have hd : 0 < d := mul_pos hp (sub_pos.mpr hj)
    have hD : Tendsto (fun a : ℝ ↦ log a - d * a +
        (C + penalty y 1 + p * (log p + log n - 1))) atTop atBot := log_minus_linear hd _
    have hD' : Tendsto (fun a : ℝ ↦ log a - (p * m) * a +
        (C + penalty y 1)) atTop atBot := log_minus_linear (mul_pos hp hm) _
    filter_upwards [eventually_ge_atTop (1 : ℝ), hD.eventually_lt_atBot L,
      hD'.eventually_lt_atBot L] with a ha hD hD' q hq
    have ha₀ : 0 < a := zero_lt_one.trans_le ha
    have hr := (penalty_bounds hn hy ha₀).1 j
    have hpos := (div_pos (exp_pos _) hN).trans_le hr
    have hupper := penalty_antitone hy one_pos ha
    by_cases hq₁ : q ≤ 1
    · have hlog := log_nonpos hq.le hq₁
      have hB := hsmall q ⟨hq, hq₁⟩
      unfold objective
      dsimp only
      nlinarith [mul_nonneg hq.le hpos.le]
    · have hql : 1 ≤ q := le_of_not_ge hq₁
      have hu := log_le_sub_one_of_pos (div_pos (mul_pos hq hpos) hp)
      rw [log_div (mul_pos hq hpos).ne' hp.ne', log_mul hq.ne' hpos.ne'] at hu
      have hu' := mul_le_mul_of_nonneg_left hu hp.le
      rw [mul_sub p (q * penalty y a / p) 1, mul_div_cancel₀ _ hp.ne', mul_one] at hu'
      have hl := log_le_log (div_pos (exp_pos _) hN) hr
      rw [log_div (exp_pos _).ne' hN.ne', log_exp] at hl
      have hpl := mul_le_mul_of_nonneg_left hl hp.le
      have hB := hlarge q hql
      unfold objective
      dsimp only
      dsimp [d, m] at hD
      nlinarith
  obtain ⟨B, hB⟩ := eventually_atTop.mp hlargeEnv
  refine ⟨A, B, hA, fun q a hq ha hv ↦ ⟨?_, ?_⟩⟩
  · exact le_of_not_gt (fun ha' ↦ (hbelow ⟨ha, ha'⟩ q hq).not_ge hv)
  · exact le_of_not_gt (fun ha' ↦ (hB a ha'.le q hq).not_ge hv)

private theorem shape_box (hn : 0 < n) {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {p A B : ℝ} (hp : 0 < p) (hA : 0 < A) (hAB : A ≤ B) (L : ℝ) :
    ∃ U V : ℝ, 0 < U ∧ ∀ q a : ℝ, 0 < q → a ∈ Icc A B →
      L ≤ objective y p (q, a) → q ∈ Icc U V := by
  obtain ⟨C, hsmall, hlarge⟩ := logBeta_bounds hp
  have hB := hA.trans_le hAB
  have hR : 0 < penalty y B := (div_pos (exp_pos _) (Nat.cast_pos.mpr hn)).trans_le
    ((penalty_bounds hn hy hB).1 ⟨0, hn⟩)
  let K := log B + penalty y A + C
  have hsmallEnv : ∀ᶠ q : ℝ in 𝓝[>] 0,
      ∀ a ∈ Icc A B, objective y p (q, a) < L := by
    have hlim := tendsto_atBot_add_const_right _ K tendsto_log_nhdsGT_zero
    filter_upwards [(nhdsGT_basis (0 : ℝ)).mem_of_mem zero_lt_one,
      hlim.eventually_lt_atBot L] with q hq hlim a ha
    have ha₀ := hA.trans_le ha.1
    have hlog := log_le_log ha₀ ha.2
    have hRa := penalty_antitone hy hA ha.1
    have hRb := hR.trans_le (penalty_antitone hy ha₀ ha.2)
    have hb := hsmall q ⟨hq.1, hq.2.le⟩
    unfold objective
    dsimp [K] at hlim ⊢
    nlinarith [mul_nonneg (mul_nonneg hp.le ha₀.le) (avg_pos hn hy).le,
      mul_nonneg hq.1.le hRb.le]
  obtain ⟨U, hU, hlow⟩ := (nhdsGT_basis (0 : ℝ)).eventually_iff.mp hsmallEnv
  have hlargeEnv : ∀ᶠ q : ℝ in atTop,
      ∀ a ∈ Icc A B, objective y p (q, a) < L := by
    have hbase : Tendsto (fun q : ℝ ↦ log q - (penalty y B / p) * q) atTop atBot :=
      (log_minus_linear (div_pos hR hp) 0).congr (fun q ↦ add_zero _)
    have hlim : Tendsto (fun q : ℝ ↦ p * (log q - (penalty y B / p) * q) + K)
        atTop atBot := tendsto_atBot_add_const_right _ K (hbase.const_mul_atBot hp)
    filter_upwards [eventually_ge_atTop (1 : ℝ), hlim.eventually_lt_atBot L]
      with q hq hlim a ha
    have ha₀ := hA.trans_le ha.1
    have hlog := log_le_log ha₀ ha.2
    have hRa := penalty_antitone hy hA ha.1
    have hRb := mul_le_mul_of_nonneg_left (penalty_antitone hy ha₀ ha.2)
      (zero_le_one.trans hq)
    have hb := hlarge q hq
    have he : p * (penalty y B / p) = penalty y B := mul_div_cancel₀ _ hp.ne'
    dsimp [K] at hlim
    rw [mul_sub, ← mul_assoc, he] at hlim
    unfold objective
    dsimp only
    nlinarith [mul_nonneg (mul_nonneg hp.le ha₀.le) (avg_pos hn hy).le]
  obtain ⟨V, hV⟩ := eventually_atTop.mp hlargeEnv
  exact ⟨U, V, hU, fun q a hq ha hv ↦
    ⟨le_of_not_gt fun h ↦ (hlow ⟨hq, h⟩ a ha).not_ge hv,
      le_of_not_gt fun h ↦ (hV q h.le a ha).not_ge hv⟩⟩

open Endpoint2GeneralizedBeta

private theorem chart_identity (hn : 0 < n) {x : Fin n → ℝ} {b p : ℝ}
    (hb : 0 < b) (hx : ∀ i, 0 < x i) (v : ℝ × ℝ) :
    logLik x b p v.1 v.2 =
      (n : ℝ) * objective (fun i ↦ log (b / x i)) p v - ∑ i, log (x i) := by
  have hN : (n : ℝ) ≠ 0 := (Nat.cast_pos.mpr hn).ne'
  have hunit (i : Fin n) :
      unitData x b v.2 i = exp (-(v.2 * log (b / x i))) := by
    rw [unitData, rpow_def_of_pos (div_pos (hx i) hb),
      log_div (hx i).ne' hb.ne', log_div hb.ne' (hx i).ne']
    congr 1
    ring
  simp only [logLik, hunit, log_exp, Finset.sum_add_distrib, Finset.sum_sub_distrib,
    ← Finset.mul_sum, Finset.sum_neg_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, objective, avg, penalty]
  field_simp
  ring

private theorem objective_continuous {y : Fin n → ℝ} (hy : ∀ i, 0 < y i)
    {p : ℝ} (hp : 0 < p) :
    ContinuousOn (objective y p) (Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)) := by
  intro v hv
  have hq : 0 < v.1 := hv.1
  have ha : 0 < v.2 := hv.2
  have hr : ContinuousAt (fun w : ℝ × ℝ ↦ penalty y w.2) v := by
    unfold penalty
    apply ContinuousAt.div_const
    apply ContinuousAt.neg
    apply tendsto_finsetSum Finset.univ
    intro i _
    have hgap : ContinuousAt (fun w : ℝ × ℝ ↦ 1 - exp (-(w.2 * y i))) v := by
      fun_prop
    exact (hgap.log (gap_pos (mul_pos ha (hy i))).ne').tendsto
  have hG := ((logGamma_cont (add_pos hp hq)).comp
    (f := fun w : ℝ × ℝ ↦ p + w.1) (continuousAt_const.add continuous_fst.continuousAt)).sub
    ((logGamma_cont hq).comp (f := Prod.fst) continuous_fst.continuousAt)
  have hmain : ContinuousAt (fun w : ℝ × ℝ ↦
      log w.2 - p * w.2 * avg y + (1 - w.1) * penalty y w.2 +
        (log (Gamma (p + w.1)) - log (Gamma w.1) - log (Gamma p))) v := by
    have hlog := continuous_snd.continuousAt.log (a := v) ha.ne'
    exact ((hlog.sub ((continuousAt_const.mul continuous_snd.continuousAt).mul
      continuousAt_const)).add ((continuousAt_const.sub continuous_fst.continuousAt).mul hr)).add
      (hG.sub continuousAt_const)
  apply hmain.continuousWithinAt.congr
  · intro w hw
    have h := logBeta_formula hp (show 0 < w.1 from hw.1)
    unfold objective
    linarith
  · have h := logBeta_formula hp hq
    unfold objective
    linarith

/-- Every joint finite superlevel lies in a compact subset of the
positive shape-power quadrant on nonconstant strictly interior samples. -/
theorem held_scale_shape1_compact_superlevels (hn : 0 < n)
    {x : Fin n → ℝ} {b p : ℝ} (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b)
    (hp : 0 < p) (hne : ∃ i j, x i ≠ x j) (L : ℝ) :
    IsCompact {v ∈ Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ) | L ≤ logLik x b p v.1 v.2} := by
  let y := fun i ↦ log (b / x i)
  let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
  let C := (L + ∑ i, log (x i)) / n
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hb : 0 < b := (hx ⟨0, hn⟩).1.trans (hx ⟨0, hn⟩).2
  have hy (i : Fin n) : 0 < y i := log_pos ((one_lt_div (hx i).1).mpr (hx i).2)
  obtain ⟨j, -, hj⟩ := Finset.exists_min_image Finset.univ y
    ⟨⟨0, hn⟩, Finset.mem_univ _⟩
  have hlt : y j < avg y := by
    have he (i : Fin n) := hj i (Finset.mem_univ i)
    have hex : ∃ i, y j < y i := by
      by_contra! H
      have hall (i : Fin n) : y i = y j := le_antisymm (H i) (he i)
      obtain ⟨u, v, huv⟩ := hne
      have h := (hall u).trans (hall v).symm
      dsimp [y] at h
      rw [log_div hb.ne' (hx u).1.ne', log_div hb.ne' (hx v).1.ne'] at h
      exact huv (log_injOn_pos (hx u).1 (hx v).1 (by linarith))
    obtain ⟨i, hi⟩ := hex
    apply (lt_div_iff₀ hN).mpr
    simpa [nsmul_eq_mul, mul_comm] using
      Finset.sum_lt_sum (s := Finset.univ) (fun k _ ↦ he k) ⟨i, Finset.mem_univ _, hi⟩
  have hiff (v : ℝ × ℝ) : L ≤ logLik x b p v.1 v.2 ↔ C ≤ objective y p v := by
    rw [chart_identity hn hb (fun i ↦ (hx i).1)]
    exact ⟨fun h ↦ (div_le_iff₀ hN).mpr (by linarith),
      fun h ↦ by have := (div_le_iff₀ hN).mp h; linarith⟩
  simp_rw [hiff]
  obtain ⟨A, B, hA, hab⟩ := power_box hn hy hp j hlt C
  by_cases hAB : A ≤ B
  · obtain ⟨U, V, hU, huv⟩ := shape_box hn hy hp hA hAB C
    let K := Icc U V ×ˢ Icc A B
    have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
    have hKD : K ⊆ D := fun _ hv ↦ ⟨hU.trans_le hv.1.1, hA.trans_le hv.2.1⟩
    have hcon := (objective_continuous hy hp).mono hKD
    let _ : CompactSpace K := isCompact_iff_compactSpace.mp hK
    have hclosed : IsClosed {v : K | C ≤ objective y p v} :=
      isClosed_Ici.preimage hcon.domRestrict
    have hc := hclosed.isCompact.image continuous_subtype_val
    convert hc using 1
    ext v
    constructor
    · rintro ⟨hv, hL⟩
      have hpow := hab v.1 v.2 hv.1 hv.2 hL
      exact ⟨⟨v, huv v.1 v.2 hv.1 hpow hL, hpow⟩, hL, rfl⟩
    · rintro ⟨w, hw, rfl⟩
      exact ⟨hKD w.property, hw⟩
  · convert isCompact_empty (X := ℝ × ℝ) using 1
    apply eq_empty_iff_forall_notMem.mpr
    rintro v ⟨hv, hL⟩
    have h := hab v.1 v.2 hv.1 hv.2 hL
    exact hAB (h.1.trans h.2)

/-- With a strictly containing endpoint and positive first shape held,
a finite maximum exists exactly when the sample is nonconstant. -/
theorem held_scale_shape1_criterion (hn : 0 < n) {x : Fin n → ℝ} {b p : ℝ}
    (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun v : ℝ × ℝ ↦ logLik x b p v.1 v.2
    (∃ v ∈ D, IsMaxOn L D v) ↔ ∃ i j, x i ≠ x j := by
  intro D L
  constructor
  · intro h
    by_contra! he
    let j : Fin n := ⟨0, hn⟩
    have hconst : x = fun _ ↦ x j := funext (fun i ↦ he i j)
    rw [show L = fun v : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ x j) b p v.1 v.2
      from by simp only [L, ← hconst]] at h
    exact (Base9GenBeta.held_scale_shape1_constant_no_fit hn (hx j).1 (hx j).2 hp).2 h
  · intro hne
    have hb := (hx ⟨0, hn⟩).1.trans (hx ⟨0, hn⟩).2
    let y := fun i ↦ log (b / x i)
    have hy : ∀ i, 0 < y i := fun i ↦ log_pos ((one_lt_div (hx i).1).mpr (hx i).2)
    have hchart (v : ℝ × ℝ) :
        L v = (n : ℝ) * objective y p v - ∑ i, log (x i) :=
      chart_identity hn hb (fun i ↦ (hx i).1) v
    have hcont : ContinuousOn L D := by
      rw [show L = (fun v ↦ (n : ℝ) * objective y p v - ∑ i, log (x i)) from
        funext hchart]
      exact (continuousOn_const.mul (objective_continuous hy hp)).sub continuousOn_const
    apply Generic.exists_isMaxOn_of_tendsto_cocompact
      (show D.Nonempty from ⟨(1, 1), one_pos, one_pos⟩) hcont
    apply tendsto_atBot.mpr
    intro C
    let K : Set (ℝ × ℝ) := {v ∈ D | C ≤ L v}
    have hK : IsCompact K := held_scale_shape1_compact_superlevels hn hx hp hne _
    have hKD : K ⊆ D := fun _ h ↦ h.1
    have hs : IsCompact ((Subtype.val : D → ℝ × ℝ) ⁻¹' K) := by
      rw [Subtype.isCompact_iff, Set.image_preimage_eq_of_subset]
      · exact hK
      · simpa using hKD
    filter_upwards [hs.compl_mem_cocompact] with v hv
    have he : ¬ C ≤ L v := fun h ↦ hv ⟨v.property, h⟩
    exact (lt_of_not_ge he).le

private theorem objective_upper_of_bounded_shape (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 < y i) {p : ℝ} (hp : 0 < p) (Q : ℝ) :
    ∃ C : ℝ, ∀ q a : ℝ, 0 < q → q ≤ Q → 0 < a → objective y p (q, a) ≤ C := by
  obtain ⟨K, hk₁, hk₂⟩ := logBeta_bounds hp
  have hB (q : ℝ) (hq : 0 < q) (hqQ : q ≤ Q) :
      -Beta.logBeta (p, q) ≤ K + p * log (max 1 Q) := by
    by_cases hq₁ : q ≤ 1
    · linarith [hk₁ q ⟨hq, hq₁⟩, log_nonpos hq.le hq₁,
        mul_nonneg hp.le (log_nonneg (le_max_left 1 Q))]
    · linarith [hk₂ q (le_of_not_ge hq₁), mul_le_mul_of_nonneg_left
        (log_le_log hq (hqQ.trans (le_max_right 1 Q))) hp.le]
  have hm := avg_pos hn hy
  have hlin := log_minus_linear (mul_pos hp hm) (K + p * log (max 1 Q) + penalty y 1)
  obtain ⟨A, hA, hA'⟩ :=
    (nhdsGT_basis (0 : ℝ)).eventually_iff.mp (small_power_tail hn hy hp 0)
  obtain ⟨B, hB'⟩ := eventually_atTop.mp (hlin.eventually_lt_atBot 0)
  let l := min A 1
  let u := max B 1
  have hl : 0 < l := lt_min hA one_pos
  refine ⟨max 0 (log u + penalty y l + K + p * log (max 1 Q)), ?_⟩
  intro q a hq hqQ ha
  have hR := (div_pos (exp_pos _) (Nat.cast_pos.mpr hn)).trans_le
    ((penalty_bounds hn hy ha).1 ⟨0, hn⟩)
  by_cases hlo : a < l
  · exact (hA' ⟨ha, hlo.trans_le (min_le_left A 1)⟩ q hq).le.trans (le_max_left _ _)
  by_cases hhi : u < a
  · have hh := hB' a ((le_max_left B 1).trans hhi.le)
    have hr := penalty_antitone hy one_pos ((le_max_right B 1).trans hhi.le)
    apply le_trans _ (le_max_left 0 _)
    unfold objective
    dsimp only
    nlinarith [hB q hq hqQ, mul_nonneg hq.le hR.le]
  · have hh := log_le_log ha (le_of_not_gt hhi)
    have hr := penalty_antitone hy hl (le_of_not_gt hlo)
    apply le_trans _ (le_max_right 0 _)
    unfold objective
    dsimp only
    nlinarith [hB q hq hqQ, mul_nonneg hq.le hR.le,
      mul_nonneg (mul_nonneg hp.le ha.le) hm.le]

private theorem objective_upper_of_bounded_power (hn : 0 < n) {y : Fin n → ℝ}
    (hy : ∀ i, 0 < y i) {p : ℝ} (hp : 0 < p) (B : ℝ) :
    ∃ C : ℝ, ∀ q a : ℝ, 0 < q → 0 < a → a ≤ B → objective y p (q, a) ≤ C := by
  obtain ⟨A, hA, hA'⟩ :=
    (nhdsGT_basis (0 : ℝ)).eventually_iff.mp (small_power_tail hn hy hp 0)
  obtain ⟨U, V, _, hbox⟩ := shape_box hn hy hp hA (le_max_left A B) 0
  obtain ⟨C, hC⟩ := objective_upper_of_bounded_shape hn hy hp V
  refine ⟨max 0 C, fun q a hq ha hb ↦ ?_⟩
  by_cases hL : 0 ≤ objective y p (q, a)
  · have haA : A ≤ a := le_of_not_gt fun ht ↦ (hA' ⟨ha, ht⟩ q hq).not_ge hL
    have hqV := (hbox q a hq ⟨haA, hb.trans (le_max_right A B)⟩ hL).2
    exact (hC q a hq hqV ha).trans (le_max_right _ _)
  · exact (le_of_not_ge hL).trans (le_max_left _ _)

/-- At a held endpoint on constant data, divergence forces both free
parameters to infinity and is equivalent to the fixed-first-shape joint
rate test together with those two limits. -/
theorem held_scale_shape1_constant_rate {ι : Type*} {l : Filter ι} (hn : 0 < n)
    {x b p : ℝ} (hx : 0 < x) (hb : x < b) (hp : 0 < p)
    {q a : ι → ℝ} (hdom : ∀ᶠ i in l, 0 < q i ∧ 0 < a i) :
    let y := log (b / x)
    let rate := fun i ↦ log (a i) + p * log (q i * exp (-(a i * y))) -
      log (Gamma p) + (q i - 1) * log (1 - exp (-(a i * y)))
    Tendsto (fun i ↦ logLik (fun _ : Fin n ↦ x) b p (q i) (a i)) l atTop ↔
      Tendsto q l atTop ∧ Tendsto a l atTop ∧ Tendsto rate l atTop := by
  intro y rate
  let F := fun i ↦ objective (fun _ : Fin n ↦ y) p (q i, a i)
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hy : 0 < y := log_pos ((one_lt_div hx).mpr hb)
  have hchart (i : ι) :
      logLik (fun _ : Fin n ↦ x) b p (q i) (a i) =
        (n : ℝ) * F i - (n : ℝ) * log x := by
    simpa [F, y, nsmul_eq_mul] using
      chart_identity (x := fun _ : Fin n ↦ x) (p := p) hn (hx.trans hb)
        (fun _ ↦ hx) (q i, a i)
  have hchart' :
      Tendsto (fun i ↦ logLik (fun _ : Fin n ↦ x) b p (q i) (a i)) l atTop ↔
        Tendsto F l atTop := by
    constructor
    · intro h
      apply (tendsto_const_mul_atTop_of_pos hN).mp
      exact (tendsto_atTop_add_const_right _ ((n : ℝ) * log x) h).congr
        (fun i ↦ by rw [hchart, sub_add_cancel])
    · intro h
      exact (tendsto_atTop_add_const_right _ (-((n : ℝ) * log x))
        ((tendsto_const_mul_atTop_of_pos hN).mpr h)).congr
        (fun i ↦ by rw [hchart, sub_eq_add_neg])
  rw [hchart']
  have he : F =ᶠ[l] fun i ↦ rate i +
      (log (Gamma (q i + p)) - log (Gamma (q i)) - p * log (q i)) := by
    filter_upwards [hdom] with i hi
    have hA (u : ℝ) : avg (fun _ : Fin n ↦ u) = u := by
      simp [avg, nsmul_eq_mul, mul_div_cancel_left₀ _ hN.ne']
    have hR : penalty (fun _ : Fin n ↦ y) (a i) = -log (1 - exp (-(a i * y))) := by
      simp only [penalty, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
      field_simp
    dsimp [rate, F, objective]
    rw [hA, hR, sub_eq_add_neg _ (Beta.logBeta (p, q i)), logBeta_formula hp hi.1,
      log_mul hi.1.ne' (exp_pos _).ne', log_exp, add_comm p (q i)]
    ring
  have hlim (hq : Tendsto q l atTop) : Tendsto F l atTop ↔ Tendsto rate l atTop := by
    rw [tendsto_congr' he]
    have hg := (Endpoint3GammaRatio.log_gamma_ratio p hp).comp hq
    exact ⟨fun h ↦ (h.atTop_add hg.neg).congr (fun i ↦ add_neg_cancel_right _ _),
      fun h ↦ h.atTop_add hg⟩
  constructor
  · intro h
    have hq : Tendsto q l atTop := tendsto_atTop.mpr fun B ↦ by
      obtain ⟨C, hC⟩ := objective_upper_of_bounded_shape (y := fun _ : Fin n ↦ y)
        hn (fun _ ↦ hy) hp B
      filter_upwards [hdom, h.eventually_gt_atTop C] with i hi hL
      exact (lt_of_not_ge fun hqB ↦ hL.not_ge (hC _ _ hi.1 hqB hi.2)).le
    have ha : Tendsto a l atTop := tendsto_atTop.mpr fun B ↦ by
      obtain ⟨C, hC⟩ := objective_upper_of_bounded_power (y := fun _ : Fin n ↦ y)
        hn (fun _ ↦ hy) hp B
      filter_upwards [hdom, h.eventually_gt_atTop C] with i hi hL
      exact (lt_of_not_ge fun haB ↦ hL.not_ge (hC _ _ hi.1 hi.2 haB)).le
    exact ⟨hq, ha, (hlim hq).mp h⟩
  · exact fun ⟨hq, _, hr⟩ ↦ (hlim hq).mpr hr

private theorem gap_log_fast {y : ℝ} (hy : 0 < y) :
    Tendsto (fun a : ℝ ↦ a * log (1 - exp (-(a * y)))) atTop (𝓝 0) := by
  have hz : Tendsto (fun a : ℝ ↦ a * exp (-(a * y))) atTop (𝓝 0) :=
    (tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero 1 y hy).congr
      (fun a ↦ by simp [mul_comm y a])
  have hz' : Tendsto (fun a : ℝ ↦ exp (-(a * y))) atTop (𝓝 0) :=
    tendsto_exp_atBot.comp (tendsto_neg_atTop_atBot.comp (tendsto_id.atTop_mul_const hy))
  have hlow : Tendsto (fun a : ℝ ↦ -2 * (a * exp (-(a * y)))) atTop (𝓝 0) :=
    by simpa using hz.const_mul (-2 : ℝ)
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' hlow tendsto_const_nhds
  · filter_upwards [hz'.eventually (gt_mem_nhds (by norm_num : (0 : ℝ) < 1 / 2)),
      eventually_gt_atTop (0 : ℝ)] with a hhalf ha
    let z := exp (-(a * y))
    have hgap : 0 < 1 - z := by change z < 1 / 2 at hhalf; linarith
    have h := log_le_sub_one_of_pos (inv_pos.mpr hgap)
    rw [log_inv] at h
    have he : (1 - z)⁻¹ - 1 = z / (1 - z) := by field_simp; ring
    rw [he] at h
    have hle : z / (1 - z) ≤ 2 * z := by
      apply (div_le_iff₀ hgap).mpr
      change z < 1 / 2 at hhalf
      nlinarith [mul_nonneg (exp_pos (-(a * y))).le (show 0 ≤ 1 - 2 * z by linarith)]
    have hh := mul_le_mul_of_nonneg_left (h.trans hle) ha.le
    dsimp [z] at hh ⊢
    linarith
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with a ha
    exact mul_nonpos_of_nonneg_of_nonpos ha.le (log_nonpos
      (gap_pos (mul_pos ha hy)).le (by linarith [exp_pos (-(a * y))]))

private theorem logGamma_deriv {z : ℝ} (hz : 0 < z) :
    HasDerivAt (log ∘ Gamma) (UnivariateML.Gamma.digamma z) z :=
  (((hasDerivAt_Gamma_of_pos hz).log (Gamma_pos_of_pos hz).ne').differentiableAt).hasDerivAt

private theorem normalizer_gain {q : ℝ} (hq : 1 < q) :
    ∃ c > 0, ∀ᶠ p : ℝ in 𝓝[>] 0, c * p < -Beta.logBeta (p, q) - log p := by
  have hq₀ := zero_lt_one.trans hq
  let f := fun p : ℝ ↦ log (Gamma (p + q)) - log (Gamma (p + 1)) - log (Gamma q)
  let d := UnivariateML.Gamma.digamma q - UnivariateML.Gamma.digamma 1
  have hd : 0 < d := by
    have h := UnivariateML.Gamma.scoreGap_strictAnti (show (1 : ℝ) ∈ Ioi 0 from one_pos)
      (show q ∈ Ioi 0 from hq₀) hq
    dsimp [d, UnivariateML.Gamma.scoreGap] at *
    linarith [log_lt_log zero_lt_one hq]
  have hder : HasDerivAt f d 0 := by
    have h (r : ℝ) (hr : 0 < r) :
        HasDerivAt (fun p : ℝ ↦ log (Gamma (p + r))) (UnivariateML.Gamma.digamma r) 0 := by
      have hr' : 0 < 0 + r := by simpa using hr
      simpa [Function.comp_def] using (logGamma_deriv hr').comp 0
        ((hasDerivAt_id (0 : ℝ)).add_const r)
    exact ((h q hq₀).sub (h 1 one_pos)).sub_const _
  have hlim := hder.tendsto_slope_zero_right
  have hf : f 0 = 0 := by simp [f]
  refine ⟨d / 2, half_pos hd, ?_⟩
  filter_upwards [hlim.eventually (lt_mem_nhds (show d / 2 < d by linarith)),
    self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with p hp hp₀
  change d / 2 < p⁻¹ • (f (0 + p) - f 0) at hp
  simp only [sub_zero, smul_eq_mul, zero_add, hf, ← div_eq_inv_mul] at hp
  have hh := (lt_div_iff₀ hp₀).mp hp
  have he : f p = -Beta.logBeta (p, q) - log p := by
    rw [logBeta_formula hp₀ hq₀]
    unfold f
    rw [Gamma_add_one (ne_of_gt hp₀), log_mul (ne_of_gt hp₀) (Gamma_pos_of_pos hp₀).ne']
    ring
  exact he ▸ hh

/-- At held endpoint and upper shape above one, a finite point at the
same rate strictly improves on the fixed-endpoint power face. -/
theorem held_scale_power_face_beaten (hn : 0 < n) {x : Fin n → ℝ}
    {b q r : ℝ} (hx : ∀ i, x i ∈ Ioo (0 : ℝ) b) (hq : 1 < q) (hr : 0 < r) :
    ∀ᶠ a : ℝ in atTop,
      (n : ℝ) * log r - r * (∑ i, log (b / x i)) - ∑ i, log (x i) <
        logLik x b (r / a) q a := by
  let y := fun i ↦ log (b / x i)
  have hy (i : Fin n) : 0 < y i := log_pos ((one_lt_div (hx i).1).mpr (hx i).2)
  have hb := (hx ⟨0, hn⟩).1.trans (hx ⟨0, hn⟩).2
  have hN : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  obtain ⟨c, hc, hgain⟩ := normalizer_gain hq
  have hp₀ : Tendsto (fun a : ℝ ↦ r / a) atTop (𝓝[>] 0) := by
    apply tendsto_nhdsWithin_iff.mpr
    exact ⟨by simpa [div_eq_mul_inv] using tendsto_inv_atTop_zero.const_mul r,
      (eventually_gt_atTop (0 : ℝ)).mono fun a ha ↦ div_pos hr ha⟩
  have hgap : Tendsto (fun a : ℝ ↦ (q - 1) *
      (∑ i, a * log (1 - exp (-(a * y i))))) atTop (𝓝 0) := by
    have H := tendsto_finsetSum (Finset.univ : Finset (Fin n)) (fun i _ ↦ gap_log_fast (hy i))
    simpa using H.const_mul (q - 1)
  filter_upwards [hp₀.eventually hgain, eventually_gt_atTop (0 : ℝ),
    hgap.eventually (lt_mem_nhds (neg_neg_of_pos (mul_pos hN (mul_pos hc hr))))] with a hG ha hS
  change -((n : ℝ) * (c * r)) < (q - 1) * (∑ i, a * log (1 - exp (-(a * y i)))) at hS
  rw [← Finset.mul_sum, mul_left_comm] at hS
  have h' : -((n : ℝ) * (c * (r / a))) <
      (q - 1) * ∑ i, log (1 - exp (-(a * y i))) := by
    apply (mul_lt_mul_iff_left₀ ha).mp
    convert hS using 1 <;> field_simp
  have hchart := chart_identity (p := r / a) hn hb (fun i ↦ (hx i).1) (q, a)
  rw [hchart]
  have he : (r / a) * a = r := div_mul_cancel₀ _ ha.ne'
  have hlog : log a + log (r / a) = log r := by
    rw [log_div hr.ne' ha.ne']
    ring
  change c * (r / a) < -Beta.logBeta (r / a, q) - log (r / a) at hG
  have hh := mul_lt_mul_of_pos_left hG hN
  dsimp only [objective, Prod.fst, Prod.snd]
  change (n : ℝ) * log r - r * (∑ i, y i) - ∑ i, log (x i) <
    (n : ℝ) * (log a - (r / a) * a * avg y + (1 - q) * penalty y a -
      Beta.logBeta (r / a, q)) - ∑ i, log (x i)
  rw [he]
  unfold avg penalty
  calc
    _ = (n : ℝ) * log r - r * (∑ i, y i) - ∑ i, log (x i) := rfl
    _ < _ := by
      field_simp
      nlinarith [mul_pos hN (show 0 < (n : ℝ) * (-Beta.logBeta (r / a, q) -
        log (r / a)) + (q - 1) * (∑ i, log (1 - exp (-(a * y i)))) by linarith)]

end

end UnivariateML.GBHeldScaleShape1

end
