module

public import UnivariateML.Generic.AE3WeightedChart
public import UnivariateML.Family.Shape3GenGamma
import Mathlib.Analysis.Calculus.DSlope
import Mathlib.Tactic

set_option linter.style.header false

/-!
Prentice generalized gamma on positive observations, with the normal seam
in the same likelihood chart as both signs of the shape.
-/

@[expose] public section

open Set Filter MeasureTheory Real
open UnivariateML.AE3WeightedChart (domain likelihood)

namespace UnivariateML.AE3GenGamma

noncomputable section

variable {n : ℕ}

/-- Log location, scale and signed Prentice shape. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- Positive scale and strictly positive observations. -/
def support {P : Type*} (param : P → Parameters) : Set (P × ℝ) :=
  {z | 0 < (param z.1).2.1 ∧ 0 < z.2}

/-- Physical log density, including the observation Jacobian. -/
def density {P : Type*} (param : P → Parameters) (z : P × ℝ) : ℝ :=
  let p := param z.1
  GenGammaTwoPoint.logDensity p.1 p.2.1 p.2.2 (log z.2) - log z.2

private def score (p : Parameters) (x : ℝ) : ℝ :=
  let z := (log x - p.1) / p.2.1
  (-z / p.2.1 * dslope exp 0 (p.2.2 * z) - 1) / x

private theorem dslope_exp_continuous : Continuous (dslope exp 0) := by
  apply continuous_iff_continuousAt.mpr
  intro t
  by_cases ht : t = 0
  · subst t
    exact continuousAt_dslope_same.mpr (differentiable_exp.differentiableAt)
  · exact (continuousAt_dslope_of_ne ht).mpr continuous_exp.continuousAt

private theorem normal_score (μ σ x : ℝ) :
    score (μ, σ, 0) x = (μ / σ ^ 2 + (-1 / σ ^ 2) * log x - 1) / x := by
  simp [score, dslope, Real.deriv_exp]
  ring

private theorem signed_score {μ σ Q x : ℝ} (hσ : 0 < σ) (hQ : Q ≠ 0) :
    score (μ, σ, Q) x = ((1 / Q ^ 2) * (Q / σ) -
      (1 / Q ^ 2) * exp (-(Q / σ) * μ) * (Q / σ) * exp ((Q / σ) * log x) - 1) / x := by
  have he : Q / σ * log x + -(Q / σ) * μ = Q * ((log x - μ) / σ) := by ring
  have hh : exp (-(Q / σ) * μ) * exp ((Q / σ) * log x) =
      exp (Q * ((log x - μ) / σ)) := by rw [← exp_add, add_comm, he]
  dsimp only [score]
  conv_rhs => rw [show (1 / Q ^ 2) * exp (-(Q / σ) * μ) * (Q / σ) * exp ((Q / σ) * log x) =
      (1 / Q ^ 2) * (Q / σ) * (exp (-(Q / σ) * μ) * exp ((Q / σ) * log x)) by ring, hh]
  by_cases hz : log x - μ = 0
  · simp [hz]
  · have hn : Q * ((log x - μ) / σ) ≠ 0 := mul_ne_zero hQ (div_ne_zero hz hσ.ne')
    rw [dslope_of_ne _ hn, slope_def_field]
    simp only [exp_zero, sub_zero]
    field_simp
    ring

private theorem density_hasDeriv {μ σ Q x : ℝ} (hσ : 0 < σ) (hx : 0 < x) :
    HasDerivAt (fun t ↦ density id ((μ, σ, Q), t)) (score (μ, σ, Q) x) x := by
  have hlog := (hasDerivAt_id x).log hx.ne'
  have hz := (hlog.sub_const μ).div_const σ
  by_cases hQ : Q = 0
  · subst Q
    have hh := ((((hz.pow 2).div_const 2).const_sub (-log σ - log (2 * π) / 2))).sub hlog
    convert hh using 1
    · funext t
      simp [density, GenGammaTwoPoint.logDensity]
    · rw [normal_score]
      simp only [id_eq]
      ring
  · let k := 1 / Q ^ 2
    let p := Q / σ
    have hp := (hlog.sub_const μ).const_mul p
    have hh := (((((hp.exp.sub_const 1).sub hp).const_mul k).const_sub
      (log (|Q| / σ) + (k * log k - k - log (Gamma k))))).sub hlog
    convert hh using 1
    · funext t
      simp [density, GenGammaTwoPoint.logDensity, hQ, p, k]
    · rw [signed_score hσ hQ]
      simp only [id_eq, p, k, mul_sub, exp_sub, div_eq_mul_inv, ← exp_neg, neg_mul]
      ring

private theorem log_density_at_zero_continuous :
    ContinuousOn (fun p : Parameters ↦ GenGammaTwoPoint.logDensity p.1 p.2.1 p.2.2 0)
      GenGammaTwoPoint.domain := by
  have h := Shape3GenGamma.likelihood_continuous (fun _ : Fin 1 ↦ (1 : ℝ))
  unfold GenGammaTwoPoint.sampleLogLik at h
  simpa using h

private theorem density_shift (p : Parameters) (x : ℝ) :
    density id (p, x) = GenGammaTwoPoint.logDensity (p.1 - log x) p.2.1 p.2.2 0 - log x := by
  simp only [density, id_eq, GenGammaTwoPoint.logDensity, zero_sub, neg_sub]

private theorem chart_unique {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (param : P → Parameters)
    (hinj : Function.Injective param) (hc : Continuous param)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q := by
  let v : P × ℝ → Parameters := param ∘ Prod.fst
  have cv : Continuous v := hc.comp continuous_fst
  have ho : IsOpen (support param) := (isOpen_lt continuous_const cv.snd.fst).inter
    (isOpen_lt continuous_const continuous_snd)
  have cl : ContinuousOn (fun z : P × ℝ ↦ log z.2) (support param) :=
    continuous_snd.continuousOn.log (fun z hz ↦ hz.2.ne')
  have hσ (z : P × ℝ) (hz : z ∈ support param) : (v z).2.1 ≠ 0 := hz.1.ne'
  have hf : ContinuousOn (density param) (support param) := by
    have hshift : ContinuousOn (fun z : P × ℝ ↦ ((v z).1 - log z.2, (v z).2))
        (support param) := (cv.fst.continuousOn.sub cl).prodMk cv.snd.continuousOn
    have hh := (log_density_at_zero_continuous.comp hshift (fun z hz ↦ hz.1)).sub cl
    exact hh.congr (fun z _ ↦ density_shift (v z) z.2)
  have hd : ContinuousOn (fun z : P × ℝ ↦ score (param z.1) z.2) (support param) := by
    have cz := (cl.sub cv.fst.continuousOn).div cv.snd.fst.continuousOn hσ
    exact (((cz.neg.div cv.snd.fst.continuousOn hσ).mul
      (dslope_exp_continuous.comp_continuousOn (cv.snd.snd.continuousOn.mul cz))).sub
      continuousOn_const).div continuous_snd.continuousOn (fun z hz ↦ hz.2.ne')
  refine AE3WeightedChart.ae_winning_class ho hf hd continuousOn_const
    (fun p x hx ↦ density_hasDeriv (μ := (param p).1) (Q := (param p).2.2) hx.1 hx.2) w hw id _ ?_
  apply Eventually.of_forall
  intro x ⟨i, j, k, hij, hjk⟩ p q hp hq he
  apply hinj
  let a := param p
  let b := param q
  have ha : 0 < a.2.1 := (hp.1 i).1
  have hb : 0 < b.2.1 := (hq.1 i).1
  have hx (i : Fin n) : 0 < x i := (hp.1 i).2
  have h₁ := strictMonoOn_log (hx i) (hx j) hij
  have h₂ := strictMonoOn_log (hx j) (hx k) hjk
  have hnormal {a : Parameters} (ha : a.2.2 = 0) (i : Fin n) :
      score a (x i) = (a.1 / a.2.1 ^ 2 + (-1 / a.2.1 ^ 2) * log (x i) - 1) / x i := by
    rcases a with ⟨μ, σ, Q⟩
    change Q = 0 at ha
    subst Q
    exact normal_score μ σ _
  let normal (a : Parameters) (t : ℝ) := a.1 / a.2.1 ^ 2 + (-1 / a.2.1 ^ 2) * t
  let signed (a : Parameters) (t : ℝ) := (1 / a.2.2 ^ 2) * (a.2.2 / a.2.1) -
    ((1 / a.2.2 ^ 2) * exp (-(a.2.2 / a.2.1) * a.1)) * (a.2.2 / a.2.1) * exp ((a.2.2 / a.2.1) * t)
  have hsigned {a : Parameters} (ha : 0 < a.2.1) (hQ : a.2.2 ≠ 0) (i : Fin n) :
      score a (x i) = ((1 / a.2.2 ^ 2) * (a.2.2 / a.2.1) -
        (1 / a.2.2 ^ 2) * exp (-(a.2.2 / a.2.1) * a.1) * (a.2.2 / a.2.1) *
          exp ((a.2.2 / a.2.1) * log (x i)) - 1) / x i :=
    signed_score ha hQ
  have hcross (f g : ℝ → ℝ) (hh : ∀ m, (f (log (x m)) - 1) / x m =
      (g (log (x m)) - 1) / x m) :
      ∀ t ∈ ({log (x i), log (x j), log (x k)} : Set ℝ), f t = g t := by
    have hh' (m : Fin n) := sub_left_inj.mp ((div_left_inj₀ (hx m).ne').mp (hh m))
    intro t ht
    simp only [mem_insert_iff, mem_singleton_iff] at ht
    rcases ht with rfl | rfl | rfl
    · exact hh' i
    · exact hh' j
    · exact hh' k
  have hsignedNormal {a b : Parameters} (ha : 0 < a.2.1)
      (haQ : a.2.2 ≠ 0) (hbQ : b.2.2 = 0) (he : ∀ m, score a (x m) = score b (x m)) : False := by
    refine Shape3GenGamma.signed_normal_score_separation (div_ne_zero haQ ha.ne')
      (mul_pos (one_div_pos.mpr (sq_pos_of_ne_zero haQ)) (exp_pos _)) h₁ h₂
      (hcross (signed a) (normal b) fun m ↦ ?_)
    dsimp only [signed, normal]
    rw [← hsigned ha haQ, ← hnormal hbQ]
    exact he m
  by_cases haQ : a.2.2 = 0
  · by_cases hbQ : b.2.2 = 0
    · have hh := hcross (normal a) (normal b) (fun m ↦ by
        dsimp only [normal]
        rw [← hnormal haQ, ← hnormal hbQ]
        exact he m)
      have hi := hh (log (x i)) (by simp)
      have hj := hh (log (x j)) (by simp)
      dsimp only [normal] at hi hj
      have hm : -1 / a.2.1 ^ 2 = -1 / b.2.1 ^ 2 := by
        apply mul_right_cancel₀ (sub_ne_zero.mpr h₁.ne)
        linear_combination hi - hj
      have hsq : a.2.1 ^ 2 = b.2.1 ^ 2 := by
        field_simp [ha.ne', hb.ne'] at hm
        linarith
      have hσ : a.2.1 = b.2.1 := by nlinarith [sq_nonneg (a.2.1 - b.2.1)]
      have hμ : a.1 = b.1 := by
        rw [← hσ] at hi
        exact (div_left_inj₀ (pow_ne_zero 2 ha.ne')).mp (add_right_cancel hi)
      exact Prod.ext hμ (Prod.ext hσ (haQ.trans hbQ.symm))
    · exact (hsignedNormal hb hbQ haQ (fun m ↦ (he m).symm)).elim
  · by_cases hbQ : b.2.2 = 0
    · exact (hsignedNormal ha haQ hbQ he).elim
    · obtain ⟨hpow, hk, hA⟩ := Shape3GenGamma.signed_score_separation
        (div_ne_zero haQ ha.ne') (div_ne_zero hbQ hb.ne')
        (mul_pos (one_div_pos.mpr (sq_pos_of_ne_zero haQ)) (exp_pos _))
        (mul_pos (one_div_pos.mpr (sq_pos_of_ne_zero hbQ)) (exp_pos _))
        h₁ h₂ (hcross (signed a) (signed b) (fun m ↦ by
          dsimp only [signed]
          rw [← hsigned ha haQ, ← hsigned hb hbQ]
          exact he m))
      have hsq : a.2.2 ^ 2 = b.2.2 ^ 2 :=
        inv_injective (by simpa only [one_div] using hk)
      have hQ : a.2.2 = b.2.2 := by
        rcases sq_eq_sq_iff_eq_or_eq_neg.mp hsq with h | h
        · exact h
        · rw [h, neg_div] at hpow
          field_simp [ha.ne', hb.ne', hbQ] at hpow
          linarith
      have hσ : a.2.1 = b.2.1 := by
        rw [← hQ] at hpow
        exact (mul_left_cancel₀ haQ ((div_eq_div_iff ha.ne' hb.ne').mp hpow)).symm
      have hμ : a.1 = b.1 := by
        apply mul_left_cancel₀ (neg_ne_zero.mpr (div_ne_zero haQ ha.ne'))
        rw [← hk] at hA
        have he := mul_left_cancel₀ (one_div_ne_zero (pow_ne_zero 2 haQ)) hA
        simpa only [← hpow] using exp_injective he
      exact Prod.ext hμ (Prod.ext hσ hQ)

/-- Finite Prentice winners agree almost everywhere on three or more levels. -/
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := id
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q :=
  chart_unique id Function.injective_id continuous_id w hw

/-- Finite Prentice winners agree almost everywhere on three or more levels. -/
theorem held_mu_ae_unique (μ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun z : ℝ × ℝ ↦ (μ, z.1, z.2))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q :=
  chart_unique (fun z : ℝ × ℝ ↦ (μ, z.1, z.2)) (by intro p q h; simpa [Prod.ext_iff] using h)
    (by fun_prop) w hw

/-- Finite Prentice winners agree almost everywhere on three or more levels. -/
theorem held_mu_sigma_ae_unique (μ σ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun Q : ℝ ↦ (μ, σ, Q))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q :=
  chart_unique (fun Q : ℝ ↦ (μ, σ, Q)) (by intro p q h; simpa using h) (by fun_prop) w hw

/-- Finite Prentice winners agree almost everywhere on three or more levels. -/
theorem held_sigma_ae_unique (σ : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    let param := (fun z : ℝ × ℝ ↦ (z.1, σ, z.2))
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x p →
        UniqueAE.Winner (domain (support param))
          (likelihood (density param) (fun _ ↦ 0) w) x q → p = q :=
  chart_unique (fun z : ℝ × ℝ ↦ (z.1, σ, z.2)) (by intro p q h; simpa [Prod.ext_iff] using h)
    (by fun_prop) w hw

end

end UnivariateML.AE3GenGamma

end
