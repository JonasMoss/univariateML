module

public import UnivariateML.Generic.UniqueAE
import Mathlib.Analysis.SpecialFunctions.Log.Deriv
import Mathlib.Analysis.SpecialFunctions.ExpDeriv
import Mathlib.Tactic

set_option linter.style.header false

/-!
## Fixed-count Lomax likelihood with a held threshold and orientation

The sample coordinate transform is the held nonzero power of the positive
excess. The likelihood omits its parameter-independent Jacobian. The
parameter coordinates are the Lomax shape and positive inverse scale in
the transformed units.
-/

@[expose] public section

open Set MeasureTheory ContinuousLinearMap

namespace UnivariateML.AE2Lomax

noncomputable section

variable {n : ℕ}

private def power (c m t : ℝ) : ℝ := Real.exp (c * Real.log (t - m))

/-- Positive shape and rate, and strictly positive observed excesses. -/
def domain (m : ℝ) : Set ((ℝ × ℝ) × (Fin n → ℝ)) :=
  {z | 0 < z.1.1 ∧ 0 < z.1.2 ∧ ∀ i, m < z.2 i}

/-- Fixed-count log likelihood in powered excess units. -/
def likelihood (c m : ℝ) (w : Fin n → ℕ) (p : ℝ × ℝ) (x : Fin n → ℝ) :=
  ∑ i, (w i : ℝ) * (Real.log p.1 + Real.log p.2 -
    (p.1 + 1) * Real.log (1 + p.2 * Real.exp (c * Real.log (x i - m))))

private def slope (c m : ℝ) (p : ℝ × ℝ) (t : ℝ) : ℝ :=
  -(p.1 + 1) * p.2 / (1 + p.2 * power c m t) * (c * power c m t / (t - m))

private def sampleDeriv (c m : ℝ) (w : Fin n → ℕ) (p : ℝ × ℝ) (x : Fin n → ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ := ∑ i, ((w i : ℝ) * slope c m p (x i)) • proj i

private theorem power_pos (c m t : ℝ) : 0 < power c m t := Real.exp_pos _

private theorem hasSampleDeriv (c m : ℝ) (w : Fin n → ℕ) (p : ℝ × ℝ) (x : Fin n → ℝ)
    (hz : (p, x) ∈ domain m) :
    HasFDerivAt (likelihood c m w p) (sampleDeriv c m w p x) x := by
  unfold likelihood sampleDeriv
  apply HasFDerivAt.fun_sum
  intro i _
  have ht : 0 < x i - m := sub_pos.mpr (hz.2.2 i)
  have hr : 0 < p.2 := hz.2.1
  have hd := (((hasDerivAt_id (x i)).sub_const m).log ht.ne' |>.const_mul c).exp
  have hbase : 0 < 1 + p.2 * power c m (x i) := by have := power_pos c m (x i); positivity
  have hl := ((hd.const_mul p.2).const_add 1).log hbase.ne'
  have hh := ((hl.const_mul (p.1 + 1)).const_sub (Real.log p.1 + Real.log p.2)).const_mul (w i : ℝ)
  have hterm : HasDerivAt (fun t : ℝ ↦ (w i : ℝ) * (Real.log p.1 + Real.log p.2 -
      (p.1 + 1) * Real.log (1 + p.2 * power c m t))) ((w i : ℝ) * slope c m p (x i)) (x i) := by
    convert hh using 1
    · rfl
    · simp only [slope, power, id_eq]
      ring
  let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
  have hcomp := hterm.comp_hasFDerivAt x f.hasFDerivAt
  exact hcomp

private theorem chart (c m : ℝ) (w : Fin n → ℕ) :
    IsOpen (domain (n := n) m) ∧
      ContinuousOn (fun z : (ℝ × ℝ) × (Fin n → ℝ) ↦ likelihood c m w z.1 z.2) (domain m) ∧
      ContinuousOn (fun z : (ℝ × ℝ) × (Fin n → ℝ) ↦ sampleDeriv c m w z.1 z.2) (domain m) := by
  let P := (ℝ × ℝ) × (Fin n → ℝ)
  let a (z : P) := z.1.1
  let r (z : P) := z.1.2
  let t (i : Fin n) (z : P) := z.2 i - m
  have ca : Continuous a := continuous_fst.comp continuous_fst
  have cr : Continuous r := continuous_snd.comp continuous_fst
  have ct (i : Fin n) : Continuous (t i) :=
    ((continuous_apply i).comp continuous_snd).sub continuous_const
  have na (z : P) (hz : z ∈ domain m) : a z ≠ 0 := (show 0 < a z from hz.1).ne'
  have nr (z : P) (hz : z ∈ domain m) : r z ≠ 0 := (show 0 < r z from hz.2.1).ne'
  have nt (i : Fin n) (z : P) (hz : z ∈ domain m) : t i z ≠ 0 :=
    (sub_pos.mpr (hz.2.2 i)).ne'
  have cg (i : Fin n) : ContinuousOn (fun z : P ↦ power c m (z.2 i)) (domain m) :=
    (continuousOn_const.mul ((ct i).continuousOn.log (nt i))).rexp
  have nb (i : Fin n) (z : P) (hz : z ∈ domain m) : 1 + r z * power c m (z.2 i) ≠ 0 := by
    have hp := power_pos c m (z.2 i)
    have hr : 0 < r z := hz.2.1
    positivity
  refine ⟨?_, ?_, ?_⟩
  · have he : domain (n := n) m =
        {z : P | 0 < a z} ∩ {z | 0 < r z} ∩ ⋂ i, {z | 0 < t i z} := by
      ext z
      simp [domain, a, r, t, sub_pos, and_assoc]; rfl
    rw [he]
    exact ((isOpen_lt continuous_const ca).inter (isOpen_lt continuous_const cr)).inter
      (isOpen_iInter_of_finite fun i ↦ isOpen_lt continuous_const (ct i))
  · unfold likelihood
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.mul
      (((ca.continuousOn.log na).add (cr.continuousOn.log nr)).sub
        ((ca.continuousOn.add continuousOn_const).mul
          ((continuousOn_const.add (cr.continuousOn.mul (cg i))).log (nb i))))
  · unfold sampleDeriv
    apply continuousOn_finsetSum
    intro i _
    have cs : ContinuousOn (fun z : P ↦ slope c m z.1 (z.2 i)) (domain m) :=
      ((((ca.continuousOn.add continuousOn_const).neg.mul cr.continuousOn).div
        (continuousOn_const.add (cr.continuousOn.mul (cg i))) (nb i)).mul
        ((continuousOn_const.mul (cg i)).div (ct i).continuousOn (nt i)))
    exact (continuousOn_const.mul cs).smul continuousOn_const

private theorem evalSampleDeriv (c m : ℝ) (w : Fin n → ℕ) (p : ℝ × ℝ) (x : Fin n → ℝ)
    (i : Fin n) : sampleDeriv c m w p x (Pi.single i 1) = (w i : ℝ) * slope c m p (x i) := by
  simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji
    simp [Pi.single_eq_of_ne hji]
  · simp

private theorem separates {c m : ℝ} (hc : c ≠ 0) {p q : ℝ × ℝ} {x : Fin n → ℝ}
    (hp : (p, x) ∈ domain m) (hq : (q, x) ∈ domain m)
    {i j : Fin n} (hij : x i ≠ x j)
    (hs : ∀ l, slope c m p (x l) = slope c m q (x l)) : p = q := by
  have pp : 0 < p.2 := hp.2.1
  have pq : 0 < q.2 := hq.2.1
  have he (l : Fin n) : (p.1 + 1) * p.2 * (1 + q.2 * power c m (x l)) =
      (q.1 + 1) * q.2 * (1 + p.2 * power c m (x l)) := by
    have hg := power_pos c m (x l)
    have ht : 0 < x l - m := sub_pos.mpr (hp.2.2 l)
    have n₁ : 1 + p.2 * power c m (x l) ≠ 0 := by positivity
    have n₂ : 1 + q.2 * power c m (x l) ≠ 0 := by positivity
    have hz := hs l
    unfold slope at hz
    have hz' := mul_right_cancel₀ (div_ne_zero (mul_ne_zero hc hg.ne') ht.ne') hz
    have hh := (div_eq_div_iff n₁ n₂).mp hz'
    linear_combination -hh
  have hne : power c m (x i) ≠ power c m (x j) := by
    intro h
    unfold power at h
    apply hij
    have hh := mul_left_cancel₀ hc (Real.exp_injective h)
    have ht := Real.strictMonoOn_log.injOn (sub_pos.mpr (hp.2.2 i))
      (sub_pos.mpr (hp.2.2 j)) hh
    exact sub_left_inj.mp ht
  have hz : (p.1 - q.1) * p.2 * q.2 * (power c m (x i) - power c m (x j)) = 0 := by
    linear_combination he i - he j
  have ha : p.1 = q.1 := by
    have h₁ := (mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hne)
    have h₂ := (mul_eq_zero.mp h₁).resolve_right pq.ne'
    exact sub_eq_zero.mp ((mul_eq_zero.mp h₂).resolve_right pp.ne')
  have hr : p.2 = q.2 := by
    have hn : p.1 + 1 ≠ 0 := by have : 0 < p.1 := hp.1; positivity
    apply sub_eq_zero.mp
    apply (mul_eq_zero.mp (show (p.1 + 1) * (p.2 - q.2) = 0 from ?_)).resolve_left hn
    linear_combination he i - (q.2 * (1 + p.2 * power c m (x i))) * ha
  exact Prod.ext ha hr

/-- The held nonzero power of excesses transports a Lomax likelihood, with
a unique attained fit almost everywhere on each stratum of two or more levels. -/
private theorem powered_ae_unique {c : ℝ} (hc : c ≠ 0) (m : ℝ)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain m) (likelihood c m w) x p →
        UniqueAE.Winner (domain m) (likelihood c m w) x q → p = q := by
  obtain ⟨ho, hl, hd⟩ := chart c m w
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hd
    (hasSampleDeriv c m w)] with x he hlevels p q hp hq
  obtain ⟨i, j, hij⟩ := hlevels
  apply separates hc hp.1 hq.1 hij
  intro l
  have hh := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single l 1)) (he p q hp hq)
  simp only [evalSampleDeriv] at hh
  exact mul_left_cancel₀ (by exact_mod_cast (hw l).ne') hh

/-- The all-free Lomax fit is almost everywhere unique when attained. -/
theorem lomax_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (1) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (1) 0 w) x q → p = q :=
  powered_ae_unique (by norm_num) 0 w hw

/-- The Pareto II fit with held minimum is almost everywhere unique when attained. -/
theorem pareto2_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain m) (likelihood (1) m w) x p →
        UniqueAE.Winner (domain m) (likelihood (1) m w) x q → p = q :=
  powered_ae_unique (by norm_num) m w hw

/-- The inverse Pareto fit is almost everywhere unique when attained. -/
theorem invpareto_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (-1) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (-1) 0 w) x q → p = q :=
  powered_ae_unique (by norm_num) 0 w hw

/-- The Burr XII fit with held positive power is almost everywhere unique when attained. -/
theorem burr_held_shape2_ae_unique {c : ℝ} (hc : 0 < c) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood c 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood c 0 w) x q → p = q :=
  powered_ae_unique (hc.ne') 0 w hw

/-- The inverse Burr fit with held positive power is almost everywhere unique when attained. -/
theorem invburr_held_shape2_ae_unique {c : ℝ} (hc : 0 < c) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain 0) (likelihood (-c) 0 w) x p →
        UniqueAE.Winner (domain 0) (likelihood (-c) 0 w) x q → p = q :=
  powered_ae_unique (neg_ne_zero.mpr hc.ne') 0 w hw

/-- With the threshold and positive power held, the Pareto IV fit is almost
everywhere unique when attained. -/
theorem pareto4_held_min_shape2_ae_unique (m : ℝ) {c : ℝ} (hc : 0 < c)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j, x i ≠ x j) →
      ∀ p q, UniqueAE.Winner (domain m) (likelihood c m w) x p →
        UniqueAE.Winner (domain m) (likelihood c m w) x q → p = q :=
  powered_ae_unique (hc.ne') m w hw

end
end UnivariateML.AE2Lomax
end
