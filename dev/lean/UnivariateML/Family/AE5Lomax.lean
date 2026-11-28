module

public import UnivariateML.Generic.AE5Weighted
public import UnivariateML.Family.Base7Lomax
public import UnivariateML.Family.Base7InversePareto
public import UnivariateML.Family.Base7ParetoIIMin

import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-! The fixed-count Lomax, inverse Pareto and held-minimum Pareto II fits.
The existence test is the original Lomax sign test on the replicated,
transformed observations. The exceptional exact tie results are separate.
A finite winner is retained at equality with the limiting face. -/

@[expose] public section

open Set MeasureTheory Real

namespace UnivariateML.AE5Lomax

noncomputable section

/-- Expand the distinct levels into a sample with the given multiplicities. -/
def expanded {n : ℕ} (w : Fin n → ℕ) (x : Fin n → ℝ) :
    Fin (Fintype.card (Σ i, Fin (w i))) → ℝ :=
  fun j ↦ x ((Fintype.equivFin (Σ i, Fin (w i))).symm j).1

private theorem sum_expanded {n : ℕ} (w : Fin n → ℕ) (x : Fin n → ℝ) (f : ℝ → ℝ) :
    ∑ j, f (expanded w x j) = ∑ i, (w i : ℝ) * f (x i) := by
  classical
  calc
    _ = ∑ j : Σ i, Fin (w i), f (x j.1) :=
      (Fintype.sum_equiv (Fintype.equivFin _) _ _ (fun j ↦ by simp [expanded])).symm
    _ = _ := by rw [Fintype.sum_sigma]; simp

private def chart (V : Set ℝ) : Set ((ℝ × ℝ) × ℝ) := Lomax.domain ×ˢ V

private def kernel (T : ℝ → ℝ) (z : (ℝ × ℝ) × ℝ) : ℝ :=
  Lomax.logDensity z.1.1 z.1.2 (T z.2)

private theorem analytic_kernel {V : Set ℝ} {T : ℝ → ℝ}
    (hT : AnalyticOnNhd ℝ T V) (hpos : ∀ t ∈ V, 0 < T t) :
    AnalyticOnNhd ℝ (kernel T) (chart V) := by
  intro z hz
  have hk : 0 < z.1.1 := hz.1.1
  have hr : 0 < z.1.2 := hz.1.2
  have ht := hpos z.2 hz.2
  have ha : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ y.1.1) z :=
    analyticAt_fst.comp analyticAt_fst
  have hb : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ y.1.2) z :=
    analyticAt_snd.comp analyticAt_fst
  have hc : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ T y.2) z :=
    (hT _ hz.2).comp analyticAt_snd
  have hp : 0 < 1 + z.1.2 * T z.2 := by positivity
  have hsum : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ 1 + y.1.2 * T y.2) z :=
    analyticAt_const.add (hb.mul hc)
  have haLog : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ log y.1.1) z :=
    (analyticAt_log hk).comp (f := fun y : (ℝ × ℝ) × ℝ ↦ y.1.1) (x := z) ha
  have hbLog : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ log y.1.2) z :=
    (analyticAt_log hr).comp (f := fun y : (ℝ × ℝ) × ℝ ↦ y.1.2) (x := z) hb
  have hsumLog : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ log (1 + y.1.2 * T y.2)) z :=
    (analyticAt_log hp).comp (f := fun y : (ℝ × ℝ) × ℝ ↦ 1 + y.1.2 * T y.2) (x := z) hsum
  have hshape : AnalyticAt ℝ (fun y : (ℝ × ℝ) × ℝ ↦ y.1.1 + 1) z :=
    ha.add analyticAt_const
  exact (haLog.add hbLog).sub (hshape.mul hsumLog)

private theorem kernel_score {V : Set ℝ} {T : ℝ → ℝ}
    (hT : AnalyticOnNhd ℝ T V) (hpos : ∀ t ∈ V, 0 < T t)
    {p : ℝ × ℝ} {t : ℝ} (hz : (p, t) ∈ chart V) :
    AE5Weighted.score (kernel T) p t =
      -((p.1 + 1) * p.2 / (1 + p.2 * T t)) * deriv T t := by
  have hp : 0 < p.2 := hz.1.2
  have ht := hpos t hz.2
  have hb : 0 < 1 + p.2 * T t := by positivity
  have hbase := ((hT t hz.2).differentiableAt.hasDerivAt.const_mul p.2).const_add 1
  have h := ((hbase.log hb.ne').const_mul (p.1 + 1)).const_sub (log p.1 + log p.2)
  have h' : HasDerivAt (fun s ↦ kernel T (p, s))
      (AE5Weighted.score (kernel T) p t) t := by
    exact ((analytic_kernel hT hpos (p, t) hz).differentiableAt.hasFDerivAt).comp_hasDerivAt t
      ((hasDerivAt_const t p).prodMk (hasDerivAt_id t))
  have hh : HasDerivAt (fun s ↦ kernel T (p, s)) _ t := h
  rw [← h'.deriv, hh.deriv]
  ring

private theorem separates {V : Set ℝ} {T : ℝ → ℝ}
    (hT : AnalyticOnNhd ℝ T V) (hpos : ∀ t ∈ V, 0 < T t)
    (hinj : InjOn T V) (hne : ∀ t ∈ V, deriv T t ≠ 0)
    (p q : ℝ × ℝ) (s : Finset ℝ) (hs : 2 ≤ s.card)
    (he : ∀ t ∈ s, (p, t) ∈ chart V ∧ (q, t) ∈ chart V ∧
      AE5Weighted.score (kernel T) p t = AE5Weighted.score (kernel T) q t) :
    p = q := by
  obtain ⟨a, ha, b, hb, hab⟩ := Finset.one_lt_card.mp (show 1 < s.card by omega)
  have hp : 0 < p.2 := (he a ha).1.1.2
  have hq : 0 < q.2 := (he a ha).2.1.1.2
  have htne : T a ≠ T b := fun h ↦ hab (hinj (he a ha).1.2 (he b hb).1.2 h)
  have cross (t : ℝ) (ht : t ∈ s) :
      (p.1 + 1) * p.2 * (1 + q.2 * T t) = (q.1 + 1) * q.2 * (1 + p.2 * T t) := by
    obtain ⟨hpt, hqt, heq⟩ := he t ht
    have htpos := hpos t hpt.2
    rw [kernel_score hT hpos hpt, kernel_score hT hpos hqt] at heq
    have h := mul_right_cancel₀ (hne t hpt.2) heq
    have h₁ : 1 + p.2 * T t ≠ 0 := by positivity
    have h₂ : 1 + q.2 * T t ≠ 0 := by positivity
    exact (div_eq_div_iff h₁ h₂).mp (neg_inj.mp h)
  have hz : (p.1 - q.1) * p.2 * q.2 * (T a - T b) = 0 := by
    linear_combination cross a ha - cross b hb
  have hshape : p.1 = q.1 := by
    have h := (mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr htne)
    have h' := (mul_eq_zero.mp h).resolve_right hq.ne'
    exact sub_eq_zero.mp ((mul_eq_zero.mp h').resolve_right hp.ne')
  have hκ : 0 < p.1 := (he a ha).1.1.1
  have hr : (p.1 + 1) * (p.2 - q.2) = 0 := by
    linear_combination cross a ha - (q.2 * (1 + p.2 * T a)) * hshape
  exact Prod.ext hshape (sub_eq_zero.mp ((mul_eq_zero.mp hr).resolve_left (by positivity)))

private theorem expanded_likelihood {n : ℕ} (w : Fin n → ℕ) (x : Fin n → ℝ)
    (T : ℝ → ℝ) (p : ℝ × ℝ) :
    Lomax.logLik (expanded w (T ∘ x)) p.1 p.2 =
      AE3WeightedChart.likelihood (kernel T) (fun _ ↦ 0) w p x := by
  rw [Lomax.logLik, sum_expanded]
  simp [AE3WeightedChart.likelihood, kernel]

private theorem transformed {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    {V : Set ℝ} (hV : IsOpen V) {T : ℝ → ℝ}
    (hT : AnalyticOnNhd ℝ T V) (hpos : ∀ t ∈ V, 0 < T t)
    (hinj : InjOn T V) (hne : ∀ t ∈ V, deriv T t ≠ 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, x i ∈ V) → (∃ i j, x i ≠ x j) →
      let y := expanded w (T ∘ x)
      let L := fun p : ℝ × ℝ ↦ Lomax.logLik y p.1 p.2
      let test := ∃ u ∈ Ioo 0 (Lomax.uBar y), Lomax.psi y u ≤ 0
      ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
      ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test) := by
  classical
  have ho : IsOpen (chart V) := (isOpen_Ioi.prod isOpen_Ioi).prod hV
  filter_upwards [AE5Weighted.ae_unique_of_distinct_scores ho (analytic_kernel hT hpos)
    2 (separates hT hpos hinj hne) w hw] with x hx hmem hlevels
  obtain ⟨i, j, hij⟩ := hlevels
  have hcard : 2 ≤ (Finset.univ.image x).card :=
    Finset.one_lt_card.mpr ⟨x i, Finset.mem_image_of_mem _ (Finset.mem_univ i),
      x j, Finset.mem_image_of_mem _ (Finset.mem_univ j), hij⟩
  let y := expanded w (T ∘ x)
  have hn : 0 < Fintype.card (Σ i, Fin (w i)) :=
    Fintype.card_pos_iff.mpr ⟨⟨i, ⟨0, hw i⟩⟩⟩
  have hy (j) : 0 < y j := hpos _ (hmem _)
  have winner (p : ℝ × ℝ) (hp : p ∈ Lomax.domain)
      (hm : IsMaxOn (fun q : ℝ × ℝ ↦ Lomax.logLik y q.1 q.2) Lomax.domain p) :
      UniqueAE.Winner (AE3WeightedChart.domain (chart V))
        (AE3WeightedChart.likelihood (kernel T) (fun _ ↦ 0) w) x p := by
    refine ⟨fun l ↦ ⟨hp, hmem l⟩, fun q hq ↦ ?_⟩
    rw [← expanded_likelihood, ← expanded_likelihood]
    exact hm (hq i).1
  refine ⟨?_, Lomax.exists_max_iff hn hy⟩
  change (∃! p, p ∈ Lomax.domain ∧
    IsMaxOn (fun q : ℝ × ℝ ↦ Lomax.logLik y q.1 q.2) Lomax.domain p) ↔ _
  rw [← Lomax.exists_max_iff hn hy]
  constructor
  · rintro ⟨p, ⟨hp, hm⟩, _⟩
    exact ⟨p, hp, hm⟩
  · rintro ⟨p, hp, hm⟩
    exact ⟨p, ⟨hp, hm⟩, fun q ⟨hq, hmq⟩ ↦ hx hcard q p (winner q hq hmq) (winner p hp hm)⟩

/-- On every positive fixed-count stratum with at least two levels, the
Lomax fit is almost everywhere a singleton exactly when the sign test succeeds. -/
private theorem lomax_joined {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, 0 < x i) → (∃ i j, x i ≠ x j) →
      let y := expanded w x
      let L := fun p : ℝ × ℝ ↦ Lomax.logLik y p.1 p.2
      let test := ∃ u ∈ Ioo 0 (Lomax.uBar y), Lomax.psi y u ≤ 0
      ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
      ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test) := by
  exact transformed w hw (V := Ioi 0) isOpen_Ioi (T := id)
    (fun _ _ ↦ analyticAt_id) (fun _ ht ↦ ht) (fun _ _ _ _ h ↦ h)
    (fun t _ ↦ by simp)

/-- On every positive fixed-count stratum with at least two levels, the
inverse Pareto fit is almost everywhere a singleton exactly when its
reciprocal-data sign test succeeds. -/
private theorem invpareto_joined {n : ℕ} (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, 0 < x i) → (∃ i j, x i ≠ x j) →
      let y := expanded w x
      let z := Transfer.reciprocal y
      let L := fun p : ℝ × ℝ ↦ Transfer.InvPareto.logLik y p.1 p.2
      let test := ∃ u ∈ Ioo 0 (Lomax.uBar z), Lomax.psi z u ≤ 0
      ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
      ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test) := by
  have ha : AnalyticOnNhd ℝ (fun t : ℝ ↦ t⁻¹) (Ioi 0) :=
    fun t ht ↦ analyticAt_id.inv (show 0 < t from ht).ne'
  have hn (t : ℝ) (ht : t ∈ Ioi (0 : ℝ)) : deriv (fun t : ℝ ↦ t⁻¹) t ≠ 0 := by
    rw [(hasDerivAt_inv (show 0 < t from ht).ne').deriv]
    exact neg_ne_zero.mpr (inv_ne_zero (pow_ne_zero _ (show 0 < t from ht).ne'))
  filter_upwards [transformed w hw isOpen_Ioi ha (fun _ ht ↦ inv_pos.mpr ht)
    (fun _ _ _ _ h ↦ inv_injective h) hn] with x hx hpos hlevels
  have ht := (hx hpos hlevels).1
  have hn' : 0 < Fintype.card (Σ i, Fin (w i)) := by
    obtain ⟨i, _, _⟩ := hlevels
    exact Fintype.card_pos_iff.mpr ⟨⟨i, ⟨0, hw i⟩⟩⟩
  refine ⟨?_, Transfer.InvPareto.exists_max_iff hn' (fun _ ↦ hpos _)⟩
  exact (Transfer.existsUnique_max_iff (bijOn_id Lomax.domain)
    (fun p _ ↦ Transfer.InvPareto.logLik_transfer (expanded w x) p.1 p.2)).trans ht

/-- On every positive-excess fixed-count stratum with at least two levels,
the held-minimum Pareto II fit is almost everywhere a singleton exactly
when the excess-data sign test succeeds. -/
private theorem pareto2_joined {n : ℕ} (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, m < x i) → (∃ i j, x i ≠ x j) →
      let y := expanded w x
      let z := Transfer.ParetoII.excess y m
      let L := fun p : ℝ × ℝ ↦ Transfer.ParetoII.logLik y m p.1 p.2
      let test := ∃ u ∈ Ioo 0 (Lomax.uBar z), Lomax.psi z u ≤ 0
      ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
      ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test) := by
  have ha : AnalyticOnNhd ℝ (fun t : ℝ ↦ t - m) (Ioi m) :=
    fun _ _ ↦ analyticAt_id.sub analyticAt_const
  have hn (t : ℝ) : deriv (fun t : ℝ ↦ t - m) t ≠ 0 := by
    have hd : HasDerivAt (fun t : ℝ ↦ t - m) 1 t := (hasDerivAt_id t).sub_const m
    rw [hd.deriv]
    norm_num
  filter_upwards [transformed w hw isOpen_Ioi ha (fun _ ht ↦ sub_pos.mpr ht)
    (fun _ _ _ _ h ↦ sub_left_inj.mp h) (fun t _ ↦ hn t)] with x hx hpos hlevels
  have ht := (hx hpos hlevels).1
  have hn' : 0 < Fintype.card (Σ i, Fin (w i)) := by
    obtain ⟨i, _, _⟩ := hlevels
    exact Fintype.card_pos_iff.mpr ⟨⟨i, ⟨0, hw i⟩⟩⟩
  refine ⟨?_, Transfer.ParetoII.exists_max_iff hn' (fun _ ↦ hpos _)⟩
  let φ := fun p : ℝ × ℝ ↦ (p.1, p.2⁻¹)
  have hmap (p : ℝ × ℝ) (hp : p ∈ Lomax.domain) : φ p ∈ Lomax.domain :=
    ⟨hp.1, inv_pos.mpr hp.2⟩
  have hinv (p : ℝ × ℝ) : φ (φ p) = p := by simp [φ]
  have hbij : BijOn φ Lomax.domain Lomax.domain :=
    ⟨fun _ ↦ hmap _, fun p _ q _ h ↦ by simpa only [hinv] using congrArg φ h,
      fun p hp ↦ ⟨φ p, hmap p hp, hinv p⟩⟩
  exact (Transfer.existsUnique_max_iff hbij (c := 0)
    (fun p _ ↦ (Transfer.ParetoII.logLik_transfer (expanded w x) m p.1 p.2).trans
      (add_zero _).symm)).trans ht

/-- The physical fit has exactly one winner almost everywhere on the
attaining fixed-count strata, with the exact sign criterion, the pointwise
two-level result and the exceptional replicated-data tie. -/
theorem lomax :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, 0 < x i) → (∃ i j, x i ≠ x j) →
        let y := expanded w x
        let L := fun p : ℝ × ℝ ↦ Lomax.logLik y p.1 p.2
        let test := ∃ u ∈ Ioo 0 (Lomax.uBar y), Lomax.psi y u ≤ 0
        ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
        ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test)) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃! a, a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹ ∧
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      ∃ p ∈ Lomax.domain, ∃ q ∈ Lomax.domain,
        p.2 < 1024 ∧ 1024 < q.2 ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain p ∧
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain q ∧
        (∀ b ∈ Lomax.domain,
          IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik x v.1 v.2) Lomax.domain b → b = p ∨ b = q) ∧
        Lomax.bestExp x < Lomax.logLik x p.1 p.2) :=
  ⟨fun w hw ↦ lomax_joined w hw, (Base7Lomax.uniqueness).2⟩

/-- The physical fit has exactly one winner almost everywhere on the
attaining fixed-count strata, with the exact sign criterion, the pointwise
two-level result and the exceptional replicated-data tie. -/
theorem invpareto :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, 0 < x i) → (∃ i j, x i ≠ x j) →
        let y := expanded w x
        let z := Transfer.reciprocal y
        let L := fun p : ℝ × ℝ ↦ Transfer.InvPareto.logLik y p.1 p.2
        let test := ∃ u ∈ Ioo 0 (Lomax.uBar z), Lomax.psi z u ≤ 0
        ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
        ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test)) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, 0 < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.InvPareto.logLik x v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then 1 else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.InvPareto.logLik x b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < Transfer.InvPareto.logLik x p.1 p.2) :=
  ⟨fun w hw ↦ invpareto_joined w hw, (Base7InversePareto.uniqueness).2⟩

/-- The physical fit has exactly one winner almost everywhere on the
attaining fixed-count strata, with the exact sign criterion, the pointwise
two-level result and the exceptional replicated-data tie. -/
theorem pareto2 (m : ℝ) :
    (∀ {n : ℕ} (w : Fin n → ℕ), (∀ i, 0 < w i) →
      ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, m < x i) → (∃ i j, x i ≠ x j) →
        let y := expanded w x
        let z := Transfer.ParetoII.excess y m
        let L := fun p : ℝ × ℝ ↦ Transfer.ParetoII.logLik y m p.1 p.2
        let test := ∃ u ∈ Ioo 0 (Lomax.uBar z), Lomax.psi z u ≤ 0
        ((∃! p, p ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain p) ↔ test) ∧
        ((∃ p ∈ Lomax.domain, IsMaxOn L Lomax.domain p) ↔ test)) ∧
    (∀ {n : ℕ}, 0 < n → ∀ x : Fin n → ℝ, (∀ i, m < x i) →
      (∃ a b : ℝ, ∀ i, x i = a ∨ x i = b) →
      let L := fun v : ℝ × ℝ ↦ Transfer.ParetoII.logLik x m v.1 v.2
      (∃ v ∈ Lomax.domain, IsMaxOn L Lomax.domain v) →
        ∃! v, v ∈ Lomax.domain ∧ IsMaxOn L Lomax.domain v) ∧
    (
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun b : ℝ × ℝ ↦
          Transfer.ParetoII.logLik x 0 b.1 b.2) Lomax.domain v} = {p, q} ∧
        Transfer.ParetoII.bestShiftedExp x 0 < Transfer.ParetoII.logLik x 0 q.1 q.2) :=
  ⟨fun w hw ↦ pareto2_joined m w hw, (Base7ParetoIIMin.uniqueness m).2⟩

end

end UnivariateML.AE5Lomax

end
