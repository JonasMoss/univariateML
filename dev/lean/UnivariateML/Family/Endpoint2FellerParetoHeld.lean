module

public import UnivariateML.Family.Endpoint2FellerPareto
public import UnivariateML.Family.ShapeFacesFellerPareto
public import UnivariateML.Family.BurrHeld

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Held-threshold Feller–Pareto fits

For fixed beta shapes the log-excess likelihood is strictly concave in
power and power times log scale. The sample range controls large power,
and the likelihood bound at one observation controls the intercept.
-/

@[expose] public section

open Set Real Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2FellerParetoHeld

noncomputable section

variable {n : ℕ}

private def loss (p q v : ℝ) : ℝ :=
  p * BurrHeld.softplus (-v) + q * BurrHeld.softplus v

private theorem softplus_nonneg (v : ℝ) : 0 ≤ BurrHeld.softplus v :=
  log_nonneg (by linarith [exp_pos v])

private theorem le_softplus (v : ℝ) : v ≤ BurrHeld.softplus v := by
  have h := log_le_log (exp_pos v) (show exp v ≤ 1 + exp v by linarith)
  simpa [BurrHeld.softplus] using h

private theorem softplus_strictConvex : StrictConvexOn ℝ univ BurrHeld.softplus := by
  have hd (v : ℝ) : HasDerivAt BurrHeld.softplus (exp v / (1 + exp v)) v := by
    convert ((hasDerivAt_exp v).const_add 1).log
      (show 1 + exp v ≠ 0 by positivity) using 1
    rfl
  apply StrictMono.strictConvexOn_univ_of_deriv
    (continuous_iff_continuousAt.mpr (fun v ↦ (hd v).continuousAt))
  intro x y hxy
  rw [(hd x).deriv, (hd y).deriv]
  apply (div_lt_div_iff₀ (by positivity) (by positivity)).mpr
  nlinarith [exp_lt_exp.mpr hxy]

private theorem loss_abs_le {p q : ℝ} (hp : 0 < p) (hq : 0 < q) (v : ℝ) :
    min p q * |v| ≤ loss p q v := by
  unfold loss
  rcases le_or_gt 0 v with hv | hv
  · rw [abs_of_nonneg hv]
    have h₁ := mul_le_mul_of_nonneg_right (min_le_right p q) hv
    have h₂ := mul_le_mul_of_nonneg_left (le_softplus v) hq.le
    linarith [mul_nonneg hp.le (softplus_nonneg (-v))]
  · rw [abs_of_neg hv]
    have h₁ := mul_le_mul_of_nonneg_right (min_le_left p q) (neg_nonneg.mpr hv.le)
    have h₂ := mul_le_mul_of_nonneg_left (le_softplus (-v)) hp.le
    linarith [mul_nonneg hq.le (softplus_nonneg v)]

private def chartLik (y : Fin n → ℝ) (p q : ℝ) (v : ℝ × ℝ) : ℝ :=
  (n : ℝ) * log v.1 - ∑ i, loss p q (v.1 * y i - v.2)

private def chartDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ univ

private theorem chart_strictConcave (hn : 0 < n) (y : Fin n → ℝ) {p q : ℝ}
    (hp : 0 < p) (hq : 0 < q) : StrictConcaveOn ℝ chartDomain (chartLik y p q) := by
  have hnR : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  refine ⟨(convex_Ioi 0).prod convex_univ, ?_⟩
  intro u hu v hv huv a b ha hb hab
  have hl (w z : ℝ) :
      a * loss p q w + b * loss p q z ≥ loss p q (a * w + b * z) ∧
        (w ≠ z → loss p q (a * w + b * z) < a * loss p q w + b * loss p q z) := by
    have H₁ := softplus_strictConvex.convexOn.2 (mem_univ (-w)) (mem_univ (-z)) ha.le hb.le hab
    have H₂ := softplus_strictConvex.convexOn.2 (mem_univ w) (mem_univ z) ha.le hb.le hab
    simp only [smul_eq_mul] at H₁ H₂
    rw [show a * -w + b * -z = -(a * w + b * z) by ring] at H₁
    constructor
    · unfold loss
      linarith [mul_le_mul_of_nonneg_left H₁ hp.le, mul_le_mul_of_nonneg_left H₂ hq.le]
    · intro h
      have H₃ := softplus_strictConvex.2 (mem_univ w) (mem_univ z) h ha hb hab
      simp only [smul_eq_mul] at H₃
      unfold loss
      linarith [mul_le_mul_of_nonneg_left H₁ hp.le, mul_lt_mul_of_pos_left H₃ hq]
  have he (i : Fin n) : (a • u + b • v).1 * y i - (a • u + b • v).2 =
      a * (u.1 * y i - u.2) + b * (v.1 * y i - v.2) := by
    change (a * u.1 + b * v.1) * y i - (a * u.2 + b * v.2) = _
    ring
  have hterm (i : Fin n) : loss p q ((a • u + b • v).1 * y i - (a • u + b • v).2) ≤
      a * loss p q (u.1 * y i - u.2) + b * loss p q (v.1 * y i - v.2) := by
    rw [he]
    exact (hl _ _).1
  have h₁ := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hterm i
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum] at h₁
  have hlog := strictConcaveOn_log_Ioi.concaveOn.2 hu.1 hv.1 ha.le hb.le hab
  change a * log u.1 + b * log v.1 ≤ log (a * u.1 + b * v.1) at hlog
  change a * chartLik y p q u + b * chartLik y p q v < chartLik y p q (a • u + b • v)
  by_cases h : u.1 = v.1
  · have hs : u.2 ≠ v.2 := fun hh ↦ huv (Prod.ext h hh)
    let i : Fin n := ⟨0, hn⟩
    have ht : u.1 * y i - u.2 ≠ v.1 * y i - v.2 := by
      rw [h]
      exact sub_right_inj.not.mpr hs
    have hlt := Finset.sum_lt_sum (s := Finset.univ) (fun j _ ↦ hterm j)
      ⟨i, Finset.mem_univ _, by rw [he]; exact (hl _ _).2 ht⟩
    rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum] at hlt
    unfold chartLik
    have he' : (a • u + b • v).1 = a * u.1 + b * v.1 := rfl
    rw [he'] at hlt ⊢
    linarith [mul_le_mul_of_nonneg_left hlog hnR.le]
  · have hlog' := strictConcaveOn_log_Ioi.2 hu.1 hv.1 h ha hb hab
    change a * log u.1 + b * log v.1 < log (a * u.1 + b * v.1) at hlog'
    unfold chartLik
    have he' : (a • u + b • v).1 = a * u.1 + b * v.1 := rfl
    rw [he'] at h₁ ⊢
    linarith [mul_lt_mul_of_pos_left hlog' hnR]

private theorem chart_exists (hn : 0 < n) {y : Fin n → ℝ} (hne : ∃ i j, y i ≠ y j)
    {p q : ℝ} (hp : 0 < p) (hq : 0 < q) :
    ∃ v ∈ chartDomain, IsMaxOn (chartLik y p q) chartDomain v := by
  obtain ⟨i, j, hij⟩ := hne
  let m := min p q
  let F := chartLik y p q
  let C := F (1, 0)
  let D := m * |y i - y j| / 2
  have hm : 0 < m := lt_min hp hq
  have hD : 0 < D := div_pos (mul_pos hm (abs_pos.mpr (sub_ne_zero.mpr hij))) two_pos
  have hN : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  have hterm (v : ℝ × ℝ) (k : Fin n) :
      m * |v.1 * y k - v.2| ≤ ∑ i, loss p q (v.1 * y i - v.2) := by
    apply (loss_abs_le hp hq _).trans
    exact Finset.single_le_sum (s := Finset.univ) (f := fun j ↦ loss p q (v.1 * y j - v.2))
      (fun j _ ↦ (mul_nonneg hm.le (abs_nonneg _)).trans (loss_abs_le hp hq _)) (Finset.mem_univ k)
  have Hrange (v : ℝ × ℝ) (hv : v ∈ chartDomain) : F v ≤ (n : ℝ) * log v.1 - D * v.1 := by
    have habs := abs_sub (v.1 * y i - v.2) (v.1 * y j - v.2)
    rw [show v.1 * y i - v.2 - (v.1 * y j - v.2) = v.1 * (y i - y j) by ring,
      abs_mul, abs_of_pos hv.1] at habs
    have hh := mul_le_mul_of_nonneg_left habs hm.le
    have hsum := add_le_add (hterm v i) (hterm v j)
    dsimp [D, F, chartLik]
    linarith
  have Hsmall (v : ℝ × ℝ) : F v ≤ (n : ℝ) * log v.1 := by
    dsimp [F, chartLik]
    linarith [(mul_nonneg hm.le (abs_nonneg _)).trans (hterm v i)]
  let A := exp ((C - 1) / n)
  have hA : 0 < A := exp_pos _
  have Hlow (v : ℝ × ℝ) (hv : v ∈ chartDomain) (h : C ≤ F v) : A ≤ v.1 := by
    by_contra! hlt
    have hg := log_lt_log hv.1 hlt
    change log v.1 < log (exp ((C - 1) / n)) at hg
    rw [log_exp, lt_div_iff₀ hN] at hg
    linarith [Hsmall v]
  have hlim : Tendsto (fun c : ℝ ↦ (n : ℝ) * log c - D * c) atTop atBot := by
    have hlog : Tendsto (fun c : ℝ ↦ log c / c) atTop (𝓝 0) := by
      simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
    have he : ∀ᶠ c : ℝ in atTop, (n : ℝ) * (log c / c) < D / 2 :=
      (hlog.const_mul (n : ℝ)).eventually (Iio_mem_nhds (by simpa using half_pos hD))
    apply tendsto_atBot_mono' _ _
      (tendsto_id.const_mul_atTop_of_neg (show -(D / 2) < 0 by linarith))
    filter_upwards [he, eventually_gt_atTop (0 : ℝ)] with c hc hpos
    have hcross := mul_lt_mul_of_pos_right hc hpos
    rw [mul_assoc, div_mul_cancel₀ _ hpos.ne'] at hcross
    change (n : ℝ) * log c - D * c ≤ -(D / 2) * c
    linarith
  obtain ⟨B₀, hB₀⟩ := eventually_atTop.mp (hlim.eventually_lt_atBot C)
  let B := max 1 B₀
  have hB : 0 < B := zero_lt_one.trans_le (le_max_left _ _)
  let R := B * |y i| + max 0 ((n : ℝ) * log B - C) / m
  let K := Icc A B ×ˢ Icc (-R) R
  have HK (v : ℝ × ℝ) (hv : v ∈ chartDomain) (h : C ≤ F v) : v ∈ K := by
    have hu : v.1 ≤ B := by
      by_contra! hh
      exact (hB₀ _ ((le_max_right _ _).trans hh.le)).not_ge (h.trans (Hrange v hv))
    have H : (n : ℝ) * log v.1 ≤ (n : ℝ) * log B :=
      mul_le_mul_of_nonneg_left (log_le_log hv.1 hu) hN.le
    have hs : m * |v.1 * y i - v.2| ≤ max 0 ((n : ℝ) * log B - C) := by
      have hh := hterm v i
      dsimp [F, chartLik] at h
      linarith [le_max_right 0 ((n : ℝ) * log B - C)]
    have habs : |v.1 * y i - v.2| ≤ max 0 ((n : ℝ) * log B - C) / m := by
      apply (le_div_iff₀ hm).mpr
      linarith
    have htriangle := abs_sub (v.1 * y i) (v.1 * y i - v.2)
    rw [show v.1 * y i - (v.1 * y i - v.2) = v.2 by ring, abs_mul, abs_of_pos hv.1] at htriangle
    have hR : |v.2| ≤ R := by
      dsimp [R]
      linarith [mul_le_mul_of_nonneg_right hu (abs_nonneg (y i))]
    exact ⟨⟨Hlow v hv h, hu⟩, abs_le.mp hR⟩
  have hk0 : (1, (0 : ℝ)) ∈ K := HK _ ⟨one_pos, mem_univ _⟩ le_rfl
  have hKD : K ⊆ chartDomain := fun v hv ↦ ⟨hA.trans_le hv.1.1, mem_univ _⟩
  have hcont : ContinuousOn F K := by
    intro v hv
    apply ContinuousAt.continuousWithinAt
    have hfirst : ContinuousAt (fun v : ℝ × ℝ ↦ (n : ℝ) * log v.1) v :=
      continuousAt_const.mul (continuous_fst.continuousAt.log (hKD hv).1.ne')
    have hsecond : Continuous (fun v : ℝ × ℝ ↦ ∑ i, loss p q (v.1 * y i - v.2)) := by
      apply continuous_finsetSum
      intro i _
      have hsoft : Continuous BurrHeld.softplus := by
        change Continuous (fun w : ℝ ↦ log (1 + exp w))
        exact ((continuous_const (y := (1 : ℝ))).add continuous_exp).log
          (fun w ↦ (add_pos one_pos (exp_pos w)).ne')
      have hv' : Continuous (fun v : ℝ × ℝ ↦ v.1 * y i - v.2) := by fun_prop
      exact (continuous_const.mul (hsoft.comp hv'.neg)).add
        (continuous_const.mul (hsoft.comp hv'))
    exact hfirst.sub hsecond.continuousAt
  obtain ⟨v, hv, hm'⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨_, hk0⟩ hcont
  refine ⟨v, hKD hv, ?_⟩
  intro u hu
  by_cases huK : u ∈ K
  · exact hm' huK
  · exact (lt_of_not_ge (fun h ↦ huK (HK u hu h))).le.trans (hm' hk0)

private theorem chart_bij :
    BijOn (fun z : ℝ × ℝ ↦ (z.1, z.1 * log z.2))
      (Ioi 0 ×ˢ Ioi 0) chartDomain := by
  refine ⟨fun z hz ↦ ⟨hz.1, mem_univ _⟩, ?_, ?_⟩
  · rintro ⟨c,s⟩ hcs ⟨d,t⟩ hdt he
    have he₁ : c = d := congrArg Prod.fst he
    subst d
    have he₂ : c * log s = c * log t := congrArg Prod.snd he
    exact Prod.ext rfl (log_injOn_pos hcs.2 hdt.2 (mul_left_cancel₀ hcs.1.ne' he₂))
  · rintro ⟨c,u⟩ h
    have hc : 0 < c := h.1
    exact ⟨(c,exp (u/c)), ⟨hc, exp_pos _⟩, Prod.ext rfl (by
      change c * log (exp (u/c)) = u
      rw [log_exp, mul_div_cancel₀ _ hc.ne'])⟩

private theorem chart_transfer {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    (p q c : ℝ) {s : ℝ} (hs : 0 < s) :
    ShapeFacesFellerPareto.logLikelihood x μ c s (p,q) =
      chartLik (fun i ↦ log (x i - μ)) p q (c,c * log s) +
        ∑ i, (-log (x i - μ) - Beta.logBeta (p,q)) := by
  have hsoft (v : ℝ) : BurrHeld.softplus v = v + BurrHeld.softplus (-v) := by
    unfold BurrHeld.softplus
    have he : 1 + exp v = exp v * (1 + exp (-v)) := by
      rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
    rw [he, log_mul (exp_pos _).ne' (by positivity), log_exp]
  have he (i : Fin n) :
      log (1 + ((x i - μ) / s) ^ c) =
        BurrHeld.softplus (c * log (x i - μ) - c * log s) := by
    have hr := div_pos (sub_pos.mpr (hx i)) hs
    unfold BurrHeld.softplus
    rw [← exp_log (rpow_pos_of_pos hr c), log_rpow hr, log_div (sub_pos.mpr (hx i)).ne' hs.ne']
    congr 2
    ring_nf
  unfold ShapeFacesFellerPareto.logLikelihood chartLik
  have hterm (i : Fin n) :
      log c - log (x i - μ) - Beta.logBeta (p,q) + p * c * log ((x i - μ) / s) -
          (p + q) * log (1 + ((x i - μ) / s) ^ c) =
        log c - loss p q (c * log (x i - μ) - c * log s) +
          (-log (x i - μ) - Beta.logBeta (p,q)) := by
    rw [he, log_div (sub_pos.mpr (hx i)).ne' hs.ne']
    unfold loss
    linarith [congrArg (fun z ↦ p * z) (hsoft (c * log (x i - μ) - c * log s))]
  simp only [hterm, Finset.sum_add_distrib, Finset.sum_sub_distrib, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]

/-- At fixed beta shapes, every nonconstant positive-excess sample has a
unique fitted power and scale. This applies under either separate shape hold
when conditioning on the remaining beta shape. -/
theorem conditional_power_scale (hn : 0 < n) {x : Fin n → ℝ} {μ p q : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hq : 0 < q) (hne : ∃ i j, x i ≠ x j) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun v : ℝ × ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ v.1 v.2 (p,q)
    ∃! v, v ∈ D ∧ IsMaxOn L D v := by
  intro D L
  let y := fun i ↦ log (x i - μ)
  have hy : ∃ i j, y i ≠ y j := by
    obtain ⟨i,j,hij⟩ := hne
    refine ⟨i,j,fun h ↦ hij ?_⟩
    have hh := log_injOn_pos (sub_pos.mpr (hx i)) (sub_pos.mpr (hx j)) h
    linarith
  obtain ⟨v, hv, hm⟩ := chart_exists hn hy hp hq
  have hu := chart_strictConcave hn y hp hq
  apply (Transfer.existsUnique_max_iff chart_bij (fun z hz ↦ chart_transfer hx p q z.1 hz.2)).mpr
  exact ⟨v, ⟨hv, hm⟩, fun w hw ↦ hu.eq_of_isMaxOn hw.2 hm hw.1 hv⟩

/-- Holding threshold, power and scale leaves a two-shape beta-prime fit.
The constant branch has unbounded likelihood. -/
theorem held_power_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ c s : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hs : 0 < s) :
    let L := ShapeFacesFellerPareto.logLikelihood x μ c s
    ((∃ p ∈ Beta.domain, IsMaxOn L Beta.domain p) ↔ ∃ i j, x i ≠ x j) ∧
    ((∀ i j, x i = x j) → ¬ BddAbove (L '' Beta.domain)) := by
  intro L
  have hconst (he : ∀ i j, x i = x j) : ¬ BddAbove (L '' Beta.domain) := by
    rw [Transfer.bddAbove_iff (bijOn_id Beta.domain)
      (fun p _ ↦ ShapeFacesFellerPareto.logLik_powered_excess hx c hs p)]
    apply BetaPrime.joint_unbounded hn (rpow_pos_of_pos hs c)
      (fun i ↦ rpow_pos_of_pos (sub_pos.mpr (hx i)) c)
    rintro ⟨i,j,hij⟩
    exact hij (by simp [he i j])
  refine ⟨⟨?_, ?_⟩, hconst⟩
  · rintro ⟨p, _, hm⟩
    by_contra! he
    exact hconst he hm.bddAbove
  · intro h
    obtain ⟨p,hp,-⟩ := (ShapeFacesFellerPareto.held_power_scale_iff hn hx hc hs).mpr h
    exact ⟨p,hp⟩

/-- Fixed beta shapes concentrate at any positive common excess by
increasing power with scale set to that excess. -/
theorem constant_sample_path (hn : 0 < n) {v μ : ℝ} (hv : μ < v) (p q : ℝ) :
    Tendsto (fun c ↦ ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ v)
      μ c (v - μ) (p,q)) atTop atTop := by
  have hnR : (0 : ℝ) < n := by exact_mod_cast hn
  have he (c : ℝ) :
      ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ v) μ c (v - μ) (p,q) =
        (n : ℝ) * log c + (n : ℝ) * (-log (v - μ) - Beta.logBeta (p,q) - (p + q) * log 2) := by
    simp [ShapeFacesFellerPareto.logLikelihood, div_self (sub_pos.mpr hv).ne', nsmul_eq_mul]
    ring_nf
  simpa only [he] using (tendsto_log_atTop.const_mul_atTop hnR).atTop_add
    (tendsto_const_nhds (x := (n : ℝ) *
      (-log (v - μ) - Beta.logBeta (p,q) - (p + q) * log 2)))

/-- Holding threshold and both beta shapes gives a finite fit exactly
on nonconstant positive excesses. Every attained fit is unique. -/
theorem held_shapes_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ p q : ℝ}
    (hx : ∀ i, μ < x i) (hp : 0 < p) (hq : 0 < q) :
    let D : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0
    let L := fun v : ℝ × ℝ ↦ ShapeFacesFellerPareto.logLikelihood x μ v.1 v.2 (p,q)
    ((∃ v ∈ D, IsMaxOn L D v) ↔ ∃ i j, x i ≠ x j) ∧
      ((∃ i j, x i ≠ x j) → ∃! v, v ∈ D ∧ IsMaxOn L D v) := by
  intro D L
  have hu (h : ∃ i j, x i ≠ x j) : ∃! v, v ∈ D ∧ IsMaxOn L D v :=
    conditional_power_scale hn hx hp hq h
  refine ⟨⟨?_, fun h ↦ (hu h).exists.elim fun v hv ↦ ⟨v,hv⟩⟩, hu⟩
  rintro ⟨v,_,hm⟩
  by_contra! h
  let j : Fin n := ⟨0, hn⟩
  have he := funext (fun i ↦ h i j)
  have hlarge := (constant_sample_path hn (hx j) p q).eventually_gt_atTop (L v)
  obtain ⟨c,hgt,hc⟩ := (hlarge.and (eventually_gt_atTop (0 : ℝ))).exists
  have hmem : (c, x j - μ) ∈ D := ⟨hc, sub_pos.mpr (hx j)⟩
  have heq : L (c, x j - μ) =
      ShapeFacesFellerPareto.logLikelihood (fun _ : Fin n ↦ x j) μ c (x j - μ) (p,q) :=
    congrArg (fun y : Fin n → ℝ ↦ ShapeFacesFellerPareto.logLikelihood y μ c (x j - μ) (p,q)) he
  exact hgt.not_ge (heq ▸ hm hmem)

end

end UnivariateML.Endpoint2FellerParetoHeld

end
