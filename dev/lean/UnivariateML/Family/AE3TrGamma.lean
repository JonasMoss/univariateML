module

public import UnivariateML.Generic.AE3WeightedChart
public import UnivariateML.Family.Shape3TrGamma
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-!
Three-level finite winner separation for the transformed-gamma pair.

## Deviations

The all-free statements explicitly require three distinct levels. Excluding
attainment for smaller strata requires a separate likelihood comparison.
-/

@[expose] public section

open Set MeasureTheory Real
open UnivariateML.AE3WeightedChart (domain likelihood)

namespace UnivariateML.AE3TrGamma

noncomputable section

variable {n : ℕ}

/-- Gamma shape, positive power magnitude, and scale. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Positive parameters and strictly positive observations. -/
def support {P : Type*} (param : P → Parameters) : Set (P × ℝ) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧ 0 < (param z.1).2.2 ∧ 0 < z.2}

/-- Scalar kernel for the given orientation, using the public scale in both cases. -/
def kernel {P : Type*} (ε : ℝ) (param : P → Parameters) (z : P × ℝ) : ℝ :=
  let (k, p, a) := param z.1
  let t := (ε * p) * (log z.2 - log a)
  k * t - exp t - log z.2

/-- Normalizer in either orientation. -/
def normalizer {P : Type*} (param : P → Parameters) (v : P) : ℝ :=
  log (param v).2.1 - log (Gamma (param v).1)

private theorem score_eq {ε k p a x : ℝ} (hx : 0 < x) :
    deriv (fun t ↦ kernel ε id ((k, p, a), t)) x =
      (k * (ε * p) - (exp (-(ε * p) * log a)) * (ε * p) * exp ((ε * p) * log x) - 1) / x := by
  have hl := (hasDerivAt_id x).log hx.ne'
  have h := ((hl.sub_const (log a)).const_mul (ε * p))
  have hh := ((h.const_mul k).sub h.exp).sub hl
  rw [show deriv (fun t ↦ kernel ε id ((k, p, a), t)) x = _ from hh.deriv]
  simp only [id_eq, mul_sub, exp_sub, div_eq_mul_inv, ← exp_neg, neg_mul]
  ring

private theorem chart_unique {P : Type*} [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P] (ε : ℝ) (hε : ε ≠ 0)
    (param : P → Parameters) (hp : Function.Injective param)
    (ha : AnalyticOnNhd ℝ param univ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (kernel ε param) (normalizer param) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (kernel ε param) (normalizer param) w) x q → p = q := by
  have hc := continuousOn_univ.mp ha.continuousOn
  let v : P × ℝ → Parameters := param ∘ Prod.fst
  have cv : Continuous v := hc.comp continuous_fst
  have ho : IsOpen (support param) := by
    have he : support param = ({z | 0 < (v z).1} ∩ {z | 0 < (v z).2.1} ∩
        {z | 0 < (v z).2.2} ∩ {z | 0 < z.2} : Set (P × ℝ)) := by
      ext z
      simp only [support, mem_ofPred_eq, mem_inter_iff, and_assoc]
      rfl
    rw [he]
    exact (((isOpen_lt continuous_const cv.fst).inter
      (isOpen_lt continuous_const cv.snd.fst)).inter
      (isOpen_lt continuous_const cv.snd.snd)).inter
      (isOpen_lt continuous_const continuous_snd)
  have hf : AnalyticOnNhd ℝ (kernel ε param) (support param) := by
    intro z hz
    have av : AnalyticAt ℝ v z := (ha z.1 (mem_univ _)).comp
      ((ContinuousLinearMap.fst ℝ P ℝ).analyticAt z)
    have hx : 0 < z.2 := hz.2.2.2
    have hs : 0 < (v z).2.2 := hz.2.2.1
    have ax : AnalyticAt ℝ (fun y : P × ℝ ↦ log y.2) z :=
      (analyticAt_log hx).comp ((ContinuousLinearMap.snd ℝ P ℝ).analyticAt z)
    have ak : AnalyticAt ℝ (fun y ↦ (v y).1) z :=
      ((ContinuousLinearMap.fst ℝ ℝ (ℝ × ℝ)).analyticAt (x := v z)).comp (x := z) av
    have ar : AnalyticAt ℝ (fun y ↦ (v y).2) z :=
      ((ContinuousLinearMap.snd ℝ ℝ (ℝ × ℝ)).analyticAt (x := v z)).comp (x := z) av
    have ap : AnalyticAt ℝ (fun y ↦ (v y).2.1) z :=
      ((ContinuousLinearMap.fst ℝ ℝ ℝ).analyticAt (x := (v z).2)).comp (x := z) ar
    have aval : AnalyticAt ℝ (fun y ↦ (v y).2.2) z :=
      ((ContinuousLinearMap.snd ℝ ℝ ℝ).analyticAt (x := (v z).2)).comp (x := z) ar
    have as' : AnalyticAt ℝ (fun y ↦ log (v y).2.2) z :=
      (analyticAt_log hs).comp (g := log) (x := z) aval
    have at' := ((analyticAt_const (v := ε)).mul ap).mul (ax.sub as')
    exact ((ak.mul at').sub at'.rexp').sub ax
  have hn : ContinuousOn (fun z : P × ℝ ↦ normalizer param z.1) (support param) := by
    intro z hz
    have hk : 0 < (v z).1 := hz.1
    have hG : ContinuousAt (fun y : P × ℝ ↦ Gamma (v y).1) z :=
      (hasDerivAt_Gamma_of_pos hk).continuousAt.comp (g := Gamma) (x := z)
        (cv.fst.continuousAt (x := z))
    exact ((cv.snd.fst.continuousAt.log (show (v z).2.1 ≠ 0 from hz.2.1.ne')).sub
      (hG.log (Gamma_pos_of_pos hk).ne')).continuousWithinAt
  refine AE3WeightedChart.ae_winning_class_analytic ho hf hn w hw id _ ?_
  apply Filter.Eventually.of_forall
  intro x ⟨i, j, m, hij, hjm⟩ p q hp' hq he
  have hpos (i : Fin n) : 0 < x i := (hp'.1 i).2.2.2
  have hlog (i j : Fin n) (hij : x i < x j) : log (x i) < log (x j) :=
    strictMonoOn_log (hpos i) (hpos j) hij
  apply hp
  let a := param p
  let b := param q
  have hs (i : Fin n) :
      a.1 * (ε * a.2.1) - exp (-(ε * a.2.1) * log a.2.2) * (ε * a.2.1) *
        exp ((ε * a.2.1) * log (x i)) =
      b.1 * (ε * b.2.1) - exp (-(ε * b.2.1) * log b.2.2) * (ε * b.2.1) *
        exp ((ε * b.2.1) * log (x i)) := by
    have hh := he i
    change deriv (fun t ↦ kernel ε id (a, t)) (x i) =
      deriv (fun t ↦ kernel ε id (b, t)) (x i) at hh
    rw [score_eq (hpos i), score_eq (hpos i)] at hh
    exact sub_left_inj.mp ((div_left_inj₀ (hpos i).ne').mp hh)
  have ha' : 0 < a.2.1 := (hp'.1 i).2.1
  have hb' : 0 < b.2.1 := (hq.1 i).2.1
  obtain ⟨hpow, hk, hrate⟩ := Shape3GenGamma.signed_score_separation
    (mul_ne_zero hε ha'.ne') (mul_ne_zero hε hb'.ne') (exp_pos _) (exp_pos _)
    (hlog i j hij) (hlog j m hjm) (fun t ht ↦ by
      simp only [mem_insert_iff, mem_singleton_iff] at ht
      rcases ht with rfl | rfl | rfl
      · exact hs i
      · exact hs j
      · exact hs m)
  have hpow' := mul_left_cancel₀ hε hpow
  have hl : log a.2.2 = log b.2.2 := by
    refine mul_left_cancel₀ (neg_ne_zero.mpr (mul_ne_zero hε ha'.ne')) ?_
    simpa only [← hpow] using exp_injective hrate
  exact Prod.ext hk (Prod.ext hpow' (strictMonoOn_log.injOn
    (hp'.1 i).2.2.1 (hq.1 i).2.2.1 hl))

/-- Finite trgamma winners agree almost everywhere on strata with three or more levels. -/
theorem trgamma_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support id))
          (likelihood (kernel 1 id) (normalizer id) w) x p →
        UniqueAE.Winner (domain (support id))
          (likelihood (kernel 1 id) (normalizer id) w) x q → p = q :=
  chart_unique 1 (by norm_num) id Function.injective_id
    (fun _ _ ↦ analyticAt_id) w hw

/-- Finite trgamma winners agree almost everywhere on strata with three or more levels. -/
theorem trgamma_held_scale_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, z.2, a)
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (kernel 1 param) (normalizer param) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (kernel 1 param) (normalizer param) w) x q → p = q :=
  chart_unique 1 (by norm_num) (fun z : ℝ × ℝ ↦ (z.1, z.2, a))
    (by intro p q h; simpa [Prod.ext_iff] using h)
    (fun z _ ↦ ((ContinuousLinearMap.fst ℝ ℝ ℝ).analyticAt z).prod
      (((ContinuousLinearMap.snd ℝ ℝ ℝ).analyticAt z).prod analyticAt_const)) w hw

/-- Finite invtrgamma winners agree almost everywhere on strata with three or more levels. -/
theorem invtrgamma_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support id))
          (likelihood (kernel (-1) id) (normalizer id) w) x p →
        UniqueAE.Winner (domain (support id))
          (likelihood (kernel (-1) id) (normalizer id) w) x q → p = q :=
  chart_unique (-1) (by norm_num) id Function.injective_id
    (fun _ _ ↦ analyticAt_id) w hw

/-- Finite invtrgamma winners agree almost everywhere on strata with three or more levels. -/
theorem invtrgamma_held_scale_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param : ℝ × ℝ → Parameters := fun z ↦ (z.1, z.2, a)
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (kernel (-1) param) (normalizer param) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (kernel (-1) param) (normalizer param) w) x q → p = q :=
  chart_unique (-1) (by norm_num) (fun z : ℝ × ℝ ↦ (z.1, z.2, a))
    (by intro p q h; simpa [Prod.ext_iff] using h)
    (fun z _ ↦ ((ContinuousLinearMap.fst ℝ ℝ ℝ).analyticAt z).prod
      (((ContinuousLinearMap.snd ℝ ℝ ℝ).analyticAt z).prod analyticAt_const)) w hw

end

end UnivariateML.AE3TrGamma

end
