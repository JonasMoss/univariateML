module

public import UnivariateML.Family.Endpoint2ParetoII
public import UnivariateML.Family.ShapeFacesParetoIV
public import UnivariateML.Family.JoinsBurr

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Pareto IV threshold and conditional fits

The free-threshold kernel fixes the lower beta shape to one. Held-threshold
results below use strictly positive excesses. They include constant data
and the equality between the held scale and an observation.
-/

@[expose] public section

open Real Filter Set
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2ParetoIV

noncomputable section

variable {n : ℕ}

private theorem kernel {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    {t k c s : ℝ} (ht : 0 < t) (hk : 0 < k) (hc : 0 < c) (hs : 0 < s) :
    ShapeFacesParetoIV.logLik d (-t) k c s =
      Endpoint2FellerPareto.interiorLogLik d 1 k c s t := by
  have hΓ : Gamma (1 + k) = k * Gamma k := by
    simpa only [add_comm] using Gamma_add_one hk.ne'
  have hc' : (c + c * k) / c = k + 1 := by field_simp [hc.ne']; ring_nf
  unfold ShapeFacesParetoIV.logLik Endpoint2FellerPareto.interiorLogLik
    Endpoint2FellerPareto.logLik
  apply Finset.sum_congr rfl
  intro i _
  have hy : 0 < t + d i := add_pos_of_pos_of_nonneg ht (hd i)
  simp only [sub_neg_eq_add, add_comm (d i) t, mul_one, hc', hΓ, Gamma_one, log_one]
  rw [log_mul hk.ne' (Gamma_pos_of_pos hk).ne', log_div hs.ne' hy.ne', log_div hy.ne' hs.ne']
  ring

/-- The moving-threshold spike holds with arbitrary fixed positive scale
and upper shape whenever the power is below one. -/
theorem threshold_spike {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {k c s : ℝ} (hk : 0 < k) (hc : 0 < c) (hsub : c < 1)
    (hs : 0 < s) :
    Tendsto (fun t ↦ ShapeFacesParetoIV.logLik d (-t) k c s) (𝓝[>] 0) atTop := by
  have h : Tendsto (Endpoint2FellerPareto.interiorLogLik d 1 k c s) (𝓝[>] 0) atTop := by
    unfold Endpoint2FellerPareto.interiorLogLik
    rw [mul_one]
    exact Endpoint2FellerPareto.threshold_spike hd hz hsub hc hs
  apply h.congr'
  filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with t ht
  exact (kernel hd ht hk hc hs).symm

/-- With scale and upper shape free, the Pareto IV likelihood diverges
along the simultaneous small-scale, small-shape path at any fixed power. -/
theorem simultaneous_collapse {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hz : ∃ i, d i = 0) {c : ℝ} (hc : 0 < c) :
    Tendsto (fun T ↦ ShapeFacesParetoIV.logLik d (-exp (-T)) T⁻¹ c (exp (-T)))
      atTop atTop := by
  apply (Endpoint2FellerPareto.vanishing_scale_upper_shape hd hz one_pos hc).congr'
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with T hT
  exact (kernel hd (exp_pos (-T)) (inv_pos.mpr hT) hc (exp_pos (-T))).symm

/-- With threshold and scale fixed, attainment is exactly the branch
with nonconstant positive excesses and an observation below scale. -/
theorem held_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) :
    let D := BurrHeld.shapeDomain
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.2 p.1 s
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∃ i j, x i ≠ x j) ∧ (∃ i, x i - μ < s) := by
  intro D L
  constructor
  · rintro ⟨p, hp, hm⟩
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    have hnone (h : ∀ i, s ≤ x i - μ) (h' : ∃ i, s < x i - μ) : False := by
      have ⟨hLUB, hlt⟩ := ShapeFacesParetoIV.held_scale_face hn hx hs h h'
      apply (hlt p hp).not_ge
      apply hLUB.2
      rintro _ ⟨q, hq, rfl⟩
      exact (div_le_div_iff_of_pos_right hN).mpr (hm hq)
    have hconst (v : ℝ) (hv : μ < v) (he : x = fun _ ↦ v) (hle : v - μ ≤ s) : False := by
      have hu := ShapeFacesParetoIV.held_scale_constant_unbounded (n := n) hn hv hle
      have hb := hm.bddAbove
      exact hu (by simpa [D, L, he] using hb)
    have hex : ∃ i, x i - μ < s := by
      by_contra! h
      by_cases h' : ∃ i, s < x i - μ
      · exact hnone h h'
      · push Not at h'
        have he : x = fun _ ↦ μ + s := by
          funext i
          linarith [h i, h' i]
        exact hconst (μ + s) (by linarith) he (by linarith)
    refine ⟨?_, hex⟩
    obtain ⟨i, hi⟩ := hex
    by_contra! h
    exact hconst (x i) (hx i) (funext fun j ↦ h j i) hi.le
  · rintro ⟨hne, habove⟩
    obtain ⟨p, hp, _⟩ := ShapeFacesParetoIV.held_scale_unique hn hx hs hne habove
    exact ⟨p, hp⟩

/-- Held threshold and scale give the exact Pareto contact trace when
all excesses reach scale and some exceed it. A constant excess at or
below scale instead allows an unbounded fit. -/
theorem held_scale_escape (hn : 0 < n) {x : Fin n → ℝ} {μ s : ℝ}
    (hx : ∀ i, μ < x i) (hs : 0 < s) :
    let D := BurrHeld.shapeDomain
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.2 p.1 s / n
    let z := BurrHeld.logRatios (ShapeFacesParetoIV.excess x μ) s
    let B := BurrHeld.paretoValue z - BurrHeld.avg (fun i ↦ log (x i - μ))
    (((∀ i, s ≤ x i - μ) ∧ (∃ i, s < x i - μ)) →
      IsLUB (L '' D) B ∧ ∀ p ∈ D, L p < B) ∧
    ((∃ v, (∀ i, x i = v) ∧ v - μ ≤ s) → ¬ BddAbove (L '' D)) := by
  intro D L z B
  constructor
  · rintro ⟨hle,hlt⟩
    simpa only [D, L, z, B, ShapeFacesParetoIV.excess, sub_eq_add_neg] using
      ShapeFacesParetoIV.held_scale_face hn hx hs hle hlt
  · rintro ⟨v,he,hv⟩ ⟨C,hC⟩
    have heq : x = fun _ ↦ v := funext he
    have hv' : μ < v := (he ⟨0,hn⟩) ▸ hx ⟨0,hn⟩
    have hN : (0 : ℝ) < n := Nat.cast_pos.mpr hn
    apply ShapeFacesParetoIV.held_scale_constant_unbounded (n := n) hn hv' hv
    refine ⟨(n : ℝ) * C, ?_⟩
    rintro _ ⟨p,hp,rfl⟩
    have h : L p ≤ C := hC (mem_image_of_mem L hp)
    change ShapeFacesParetoIV.logLik x μ p.2 p.1 s / n ≤ C at h
    rw [heq] at h
    exact ((div_le_iff₀ hN).mp h).trans_eq (mul_comm C n)

/-- With threshold and first shape fixed, the existence criterion is
nonconstant positive excesses. The constant branch has a divergent path. -/
theorem held_first_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ k : ℝ}
    (hx : ∀ i, μ < x i) (hk : 0 < k) :
    let L := fun p : ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ k p.1 p.2
    (∃ p ∈ BurrHeld.shapeDomain, IsMaxOn L BurrHeld.shapeDomain p) ↔
      ∃ i j, x i ≠ x j := by
  intro L
  constructor
  · rintro ⟨p, hp, hm⟩
    by_contra! h
    let j : Fin n := ⟨0, hn⟩
    have he : x = fun _ ↦ x j := funext fun i ↦ h i j
    have hpath := (ShapeFacesParetoIV.held_first_constant_path hn (hx j) hk).eventually_gt_atTop
      (L p)
    obtain ⟨c, hlarge, hc⟩ := (hpath.and (eventually_gt_atTop (0 : ℝ))).exists
    have hle := hm (show (c, x j - μ) ∈ BurrHeld.shapeDomain from
      ⟨hc, sub_pos.mpr (hx j)⟩)
    have heq : L (c, x j - μ) =
        ShapeFacesParetoIV.logLik (fun _ : Fin n ↦ x j) μ k c (x j - μ) :=
      congrArg (fun y : Fin n → ℝ ↦ ShapeFacesParetoIV.logLik y μ k c (x j - μ)) he
    exact hlarge.not_ge (heq ▸ hle)
  · intro h
    obtain ⟨p, hp, _⟩ := ShapeFacesParetoIV.held_first_unique hn hx hk h
    exact ⟨p, hp⟩

/-- With threshold, scale and first shape fixed, the remaining power has a
finite maximum precisely when some positive excess differs from scale. -/
theorem held_first_scale_criterion (hn : 0 < n) {x : Fin n → ℝ} {μ k s : ℝ}
    (hx : ∀ i, μ < x i) (hk : 0 < k) (hs : 0 < s) :
    let L := fun c ↦ ShapeFacesParetoIV.logLik x μ k c s
    (∃ c ∈ Ioi (0 : ℝ), IsMaxOn L (Ioi 0) c) ↔ ∃ i, x i - μ ≠ s := by
  intro L
  constructor
  · rintro ⟨c, _, hm⟩
    by_contra! h
    have hpath := (ShapeFacesParetoIV.held_first_scale_unbounded hn hk hs h).eventually_gt_atTop
      (L c)
    obtain ⟨c', hgt, hc'⟩ := (hpath.and (eventually_gt_atTop (0 : ℝ))).exists
    exact hgt.not_ge (hm hc')
  · intro h
    obtain ⟨p, hp, _⟩ := ShapeFacesParetoIV.held_first_scale_unique hn hx hk hs h
    exact ⟨p, hp⟩

/-- The entire Burr maximizing set on the excesses is the Pareto IV
maximizing set at a held threshold. Every finite tie transfers exactly. -/
theorem burr_maximizers_transfer (x : Fin n → ℝ) (μ : ℝ)
    (D : Set (ℝ × ℝ × ℝ)) :
    {v ∈ D | IsMaxOn (fun w : ℝ × ℝ × ℝ ↦
        ShapeFacesParetoIV.logLik x μ w.1 w.2.1 w.2.2) D v} =
      {v ∈ D | IsMaxOn (fun w : ℝ × ℝ × ℝ ↦
        Transfer.Burr.logLik (ShapeFacesParetoIV.excess x μ) w.2.1 w.1 w.2.2) D v} := by
  simp only [ShapeFacesParetoIV.logLik_excess]

/-- The held-power shifted Weibull path, expressed on the original
observations via their excesses. -/
theorem held_threshold_weibull_path {x : Fin n → ℝ} {μ c r : ℝ}
    (hx : ∀ i, μ < x i) (hc : 0 < c) (hr : 0 < r) :
    let e := ShapeFacesParetoIV.excess x μ
    Tendsto (fun k ↦ ShapeFacesParetoIV.logLik x μ k c ((k / r) ^ c⁻¹)) atTop
      (𝓝 (Lomax.expLogLik (Transfer.Burr.powered e c) r + Transfer.Burr.powerJac e c)) := by
  simpa only [ShapeFacesParetoIV.logLik_excess] using
    Transfer.Burr.face_path (x := ShapeFacesParetoIV.excess x μ) (fun i ↦ sub_pos.mpr (hx i)) hc hr

/-- The exact Pareto II crossing also gives exactly two Pareto IV
maximizers when the threshold is zero and the power is one. -/
theorem held_power_one_three_level_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p q : ℝ × ℝ, p ≠ q ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          ShapeFacesParetoIV.logLik x 0 b.1 1 b.2) Lomax.domain v} = {p, q} := by
  obtain ⟨a, ha, p, q, hpq, hm⟩ := Endpoint2ParetoII.held_threshold_three_level_tie
  let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  have ha' : 0 < a := (by positivity : 0 < ((2 : ℝ) ^ 512)⁻¹).trans ha.1
  have hx (i : Fin 21) : 0 < x i := by
    dsimp [x]
    split
    · exact ha'
    · split <;> norm_num
  have he (b : ℝ × ℝ) (hb : b ∈ Lomax.domain) :
      ShapeFacesParetoIV.logLik x 0 b.1 1 b.2 = Transfer.ParetoII.logLik x 0 b.1 b.2 := by
    unfold ShapeFacesParetoIV.logLik Transfer.ParetoII.logLik
    apply Finset.sum_congr rfl
    intro i _
    have hs : 0 < b.2 := hb.2
    simp only [Transfer.ParetoII.logDensity, sub_zero, log_one, add_zero, one_mul, rpow_one]
    rw [log_div hs.ne' (hx i).ne']
    ring
  refine ⟨a, ha, p, q, hpq, ?_⟩
  rw [← hm]
  ext v
  simp only [mem_ofPred_eq]
  exact and_congr_right fun hv ↦ Transfer.isMaxOn_iff (c := 0) (bijOn_id _)
    (fun b hb ↦ (he b hb).trans (add_zero _).symm) hv

/-- With a held threshold, constants diverge. On nonconstant excesses,
if the finite maximum fails to exist, the larger fitted Weibull and
Pareto values give the strict, unattained supremum. -/
theorem held_threshold_failure {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let D := BurrFree.physicalDomain
    let f := fun p : ℝ × ℝ × ℝ ↦ ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2 / n
    let e := ShapeFacesParetoIV.excess x μ
    let y := BurrFree.logExcess e j
    let J := BurrHeld.avg (fun i ↦ log (e i))
    ((∀ i, x i = x j) → ¬ BddAbove (f '' D)) ∧
      ((∃ i k, x i ≠ x k) → ∃ c > 0, ∃ u,
        (∀ v > 0, ∀ w, BurrFree.weibullLik y v w ≤ BurrFree.weibullLik y c u) ∧
        ((¬ ∃ p ∈ D, IsMaxOn f D p) →
          IsLUB (f '' D) (max (BurrFree.paretoValue y) (BurrFree.weibullLik y c u) - J) ∧
          ∀ p ∈ D, f p < max (BurrFree.paretoValue y) (BurrFree.weibullLik y c u) - J)) := by
  intro D f e y J
  let g := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik e p.1 p.2.1 p.2.2
  have he (i : Fin n) : 0 < e i := sub_pos.mpr (hx i)
  have hmin (i : Fin n) : e j ≤ e i := sub_le_sub_right (hj i) μ
  have heq (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : f p = g p + 0 := by
    rw [add_zero]
    change ShapeFacesParetoIV.logLik x μ p.1 p.2.1 p.2.2 / n =
      BurrHeld.logLik e p.1 p.2.1 p.2.2
    rw [ShapeFacesParetoIV.logLik_excess]
    unfold Transfer.Burr.logLik BurrHeld.logLik BurrHeld.avg
    congr 1
    apply Finset.sum_congr rfl
    intro i _
    have hs : 0 < p.2.2 := hp.2.2
    change Transfer.Burr.logDensity p.2.1 p.1 p.2.2 (e i) =
      BurrHeld.logDensity p.1 p.2.1 p.2.2 (e i)
    have hpow : log (1 + (e i / p.2.2) ^ p.2.1) =
        BurrHeld.softplus (p.2.1 * log (e i / p.2.2)) := by
      rw [BurrHeld.softplus, rpow_def_of_pos (div_pos (he i) hs), mul_comm p.2.1]
    unfold Transfer.Burr.logDensity BurrHeld.logDensity
    rw [hpow, log_div hs.ne' (he i).ne']
    simp_rw [log_div (he i).ne' hs.ne']
    ring
  have H := JoinsBurr.all_free_failure he j hmin
  refine ⟨?_, ?_⟩
  · intro h
    have hconst (i : Fin n) : e i = e j := congrArg (fun a : ℝ ↦ a - μ) (h i)
    exact fun hb ↦ H.1 hconst ((Transfer.bddAbove_iff (bijOn_id D) heq).mp hb)
  · rintro ⟨i,k,hik⟩
    have hne : e i ≠ e k := fun hh ↦ hik (sub_left_inj.mp hh)
    obtain ⟨c,hc,u,hW,hface⟩ := H.2 ⟨i,k,hne⟩
    refine ⟨c,hc,u,hW,fun hf ↦ ?_⟩
    have hn : ¬ ∃ p ∈ D, IsMaxOn g D p :=
      fun h ↦ hf ((Transfer.exists_max_iff (bijOn_id D) heq).mpr h)
    obtain ⟨hL, hlt⟩ := hface hn
    let B := max (BurrFree.paretoValue y) (BurrFree.weibullLik y c u) - J
    have hL' : IsLUB (f '' D) (B + 0) :=
      (Transfer.isLUB_iff (bijOn_id D) heq B).mpr hL
    refine ⟨by simpa only [add_zero] using hL', fun v hv ↦ ?_⟩
    rw [heq v hv, add_zero]
    exact hlt v hv


end

end UnivariateML.Endpoint2ParetoIV

end
