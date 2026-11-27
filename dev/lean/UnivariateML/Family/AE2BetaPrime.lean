module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Family.Beta
import Mathlib.Tactic

set_option linter.style.header false

/-! Almost-everywhere uniqueness for the finite beta-prime likelihood. -/

@[expose] public section

open Set MeasureTheory ContinuousLinearMap

namespace UnivariateML.AE2BetaPrime

noncomputable section

variable {n : ℕ}

/-- The coordinates are the two beta shapes and the positive scale. -/
abbrev Parameters := ℝ × ℝ × ℝ

/-- The positive candidate and sample domain in any of the three charts. -/
def domain {P : Type*} (param : P → Parameters) : Set (P × (Fin n → ℝ)) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧ 0 < (param z.1).2.2 ∧ ∀ i, 0 < z.2 i}

/-- The weighted beta-prime log likelihood. -/
def likelihood {P : Type*} (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : ℝ :=
  let (a, b, s) := param v
  ∑ i, (w i : ℝ) * (b * Real.log s - Beta.logBeta (a, b) +
    (a - 1) * Real.log (x i) - (a + b) * Real.log (x i + s))

private def score (p : Parameters) (t : ℝ) : ℝ :=
  (p.1 - 1) / t - (p.1 + p.2.1) / (t + p.2.2)

private def sampleDeriv {P : Type*} (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score (param v) (x i)) • proj i

private theorem hasSampleDeriv {P : Type*} (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) (hz : (v, x) ∈ domain param) :
    HasFDerivAt (likelihood param w v) (sampleDeriv param w v x) x := by
  unfold likelihood sampleDeriv
  apply HasFDerivAt.fun_sum
  intro i _
  let a := (param v).1
  let b := (param v).2.1
  let s := (param v).2.2
  have ht : 0 < x i := hz.2.2.2 i
  have hs : 0 < s := hz.2.2.1
  have h := hasDerivAt_id (x i)
  have ht' := (h.log ht.ne').const_mul (a - 1)
  have hs' := ((h.add_const s).log (add_pos ht hs).ne').const_mul (a + b)
  have hh := ((ht'.const_add (b * Real.log s - Beta.logBeta (a, b))).sub hs').const_mul (w i : ℝ)
  have hterm : HasDerivAt (fun t : ℝ ↦ (w i : ℝ) *
      (b * Real.log s - Beta.logBeta (a, b) + (a - 1) * Real.log t -
        (a + b) * Real.log (t + s))) ((w i : ℝ) * score (param v) (x i)) (x i) := by
    convert hh using 1
    · rfl
    · simp [score, a, b, s, div_eq_mul_inv]
  let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
  have hcomp := hterm.comp_hasFDerivAt x f.hasFDerivAt
  exact hcomp

private theorem chart {P : Type*} [TopologicalSpace P] (param : P → Parameters)
    (cp : Continuous param) (w : Fin n → ℕ) :
    IsOpen (domain (n := n) param) ∧
      ContinuousOn (fun z : P × (Fin n → ℝ) ↦ likelihood param w z.1 z.2) (domain param) ∧
      ContinuousOn (fun z : P × (Fin n → ℝ) ↦ sampleDeriv param w z.1 z.2) (domain param) := by
  let T := P × (Fin n → ℝ)
  let a (z : T) := (param z.1).1
  let b (z : T) := (param z.1).2.1
  let s (z : T) := (param z.1).2.2
  let t (i : Fin n) (z : T) := z.2 i
  have ca : Continuous a := cp.fst.comp continuous_fst
  have cb : Continuous b := cp.snd.fst.comp continuous_fst
  have cs : Continuous s := cp.snd.snd.comp continuous_fst
  have ct (i : Fin n) : Continuous (t i) := (continuous_apply i).comp continuous_snd
  have pa (z : T) (hz : z ∈ domain param) : 0 < a z := hz.1
  have pb (z : T) (hz : z ∈ domain param) : 0 < b z := hz.2.1
  have ps (z : T) (hz : z ∈ domain param) : 0 < s z := hz.2.2.1
  have pt (i : Fin n) (z : T) (hz : z ∈ domain param) : 0 < t i z := hz.2.2.2 i
  have ns z hz := (ps z hz).ne'
  have nt i z hz := (pt i z hz).ne'
  have nts i z hz := (add_pos (pt i z hz) (ps z hz)).ne'
  have cg {f : T → ℝ} (cf : Continuous f) (hf : ∀ z ∈ domain param, 0 < f z) :
      ContinuousOn (fun z ↦ Real.Gamma (f z)) (domain param) :=
    fun z hz ↦ (Real.hasDerivAt_Gamma_of_pos (hf z hz)).continuousAt.comp
      cf.continuousAt |>.continuousWithinAt
  have pg z hz := Real.Gamma_pos_of_pos (add_pos (pa z hz) (pb z hz))
  have cn : ContinuousOn (fun z ↦ Beta.logBeta (a z, b z)) (domain param) := by
    unfold Beta.logBeta Beta.normalizer
    exact (((cg ca pa).mul (cg cb pb)).div
      (cg (ca.add cb) (fun z hz ↦ add_pos (pa z hz) (pb z hz))) (fun z hz ↦ (pg z hz).ne')).log
      (fun z hz ↦ (div_pos (mul_pos (Real.Gamma_pos_of_pos (pa z hz))
        (Real.Gamma_pos_of_pos (pb z hz))) (pg z hz)).ne')
  refine ⟨?_, ?_, ?_⟩
  · have he : domain (n := n) param =
        {z : T | 0 < a z} ∩ {z | 0 < b z} ∩ {z | 0 < s z} ∩ ⋂ i, {z | 0 < t i z} := by
      ext z
      simp [domain, a, b, s, t, and_assoc]; rfl
    rw [he]
    exact (((isOpen_lt continuous_const ca).inter (isOpen_lt continuous_const cb)).inter
      (isOpen_lt continuous_const cs)).inter
      (isOpen_iInter_of_finite fun i ↦ isOpen_lt continuous_const (ct i))
  · unfold likelihood
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.mul
      ((((cb.continuousOn.mul (cs.continuousOn.log ns)).sub cn).add
        ((ca.continuousOn.sub continuousOn_const).mul ((ct i).continuousOn.log (nt i)))).sub
        ((ca.continuousOn.add cb.continuousOn).mul (((ct i).add cs).continuousOn.log (nts i))))
  · unfold sampleDeriv
    apply continuousOn_finsetSum
    intro i _
    have h : ContinuousOn (fun z : T ↦ score (param z.1) (z.2 i)) (domain param) :=
      (((ca.continuousOn.sub continuousOn_const).div (ct i).continuousOn (nt i)).sub
        ((ca.continuousOn.add cb.continuousOn).div ((ct i).add cs).continuousOn (nts i)))
    exact (continuousOn_const.mul h).smul continuousOn_const

private theorem sampleDeriv_single {P : Type*} (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) (i : Fin n) :
    sampleDeriv param w v x (Pi.single i 1) = (w i : ℝ) * score (param v) (x i) := by
  simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji
    simp [Pi.single_eq_of_ne hji]
  · simp

private theorem separates {p q : Parameters} {x y z : ℝ} (hx : 0 < x)
    (hxy : x < y) (hyz : y < z)
    (hp : 0 < p.1 ∧ 0 < p.2.1 ∧ 0 < p.2.2)
    (hq : 0 < q.1 ∧ 0 < q.2.1 ∧ 0 < q.2.2)
    (he : ∀ t ∈ ({x, y, z} : Set ℝ), score p t = score q t) : p = q := by
  obtain ⟨a, b, s⟩ := p
  obtain ⟨d, e, r⟩ := q
  obtain ⟨ha, hb, hs⟩ := hp
  obtain ⟨hd, he', hr⟩ := hq
  let A := e - b
  let B := (a - d) * (s + r) - (a + b) * r + (d + e) * s
  let C := (a - d) * s * r
  have hroot (t : ℝ) (ht : 0 < t) (ht' : t ∈ ({x, y, z} : Set ℝ)) :
      A * t ^ 2 + B * t + C = 0 := by
    have h := he t ht'
    change (a - 1) / t - (a + b) / (t + s) = (d - 1) / t - (d + e) / (t + r) at h
    field_simp [ht.ne', (add_pos ht hs).ne', (add_pos ht hr).ne'] at h
    dsimp [A, B, C]
    linear_combination h
  have h₁ := hroot x hx (by simp)
  have h₂ := hroot y (hx.trans hxy) (by simp)
  have h₃ := hroot z (hx.trans (hxy.trans hyz)) (by simp)
  have hfactor {u v : ℝ} (hu : A * u ^ 2 + B * u + C = 0)
      (hv : A * v ^ 2 + B * v + C = 0) (hne : u ≠ v) : A * (u + v) + B = 0 := by
    apply (mul_eq_zero.mp (show (u - v) * (A * (u + v) + B) = 0 from ?_)).resolve_left
      (sub_ne_zero.mpr hne)
    linear_combination hu - hv
  have hb₁ := hfactor h₁ h₂ hxy.ne
  have hb₂ := hfactor h₁ h₃ (hxy.trans hyz).ne
  have hA : A = 0 := by
    apply (mul_eq_zero.mp (show A * (z - y) = 0 from ?_)).resolve_right
      (sub_pos.mpr hyz).ne'
    linear_combination hb₂ - hb₁
  have hB : B = 0 := by simpa only [hA, zero_mul, zero_add] using hb₁
  have hC : C = 0 := by simpa only [hA, hB, zero_mul, zero_add] using h₁
  have had : a = d := sub_eq_zero.mp
    (((mul_eq_zero.mp hC).resolve_right hr.ne' |> mul_eq_zero.mp).resolve_right hs.ne')
  have hbe : b = e := (sub_eq_zero.mp hA).symm
  have hsr : s = r := by
    apply sub_eq_zero.mp
    apply (mul_eq_zero.mp (show (a + b) * (s - r) = 0 from ?_)).resolve_left (add_pos ha hb).ne'
    dsimp [B] at hB
    rw [← had, ← hbe] at hB
    linear_combination hB
  rw [had, hbe, hsr]

private theorem chart_ae_unique {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (param : P → Parameters) (cp : Continuous param)
    (hp : Function.Injective param) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain param) (likelihood param w) x p →
        UniqueAE.Winner (domain param) (likelihood param w) x q → p = q := by
  obtain ⟨ho, hl, hd⟩ := chart param cp w
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hd
    (hasSampleDeriv param w)] with x he hlevels p q hv hq
  obtain ⟨i, j, k, hij, hjk⟩ := hlevels
  have h (l : Fin n) : score (param p) (x l) = score (param q) (x l) := by
    have hh := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single l 1)) (he p q hv hq)
    simp only [sampleDeriv_single] at hh
    exact mul_left_cancel₀ (by exact_mod_cast (hw l).ne') hh
  apply hp
  apply separates (hv.1.2.2.2 i) hij hjk
    ⟨hv.1.1, hv.1.2.1, hv.1.2.2.1⟩ ⟨hq.1.1, hq.1.2.1, hq.1.2.2.1⟩
  intro t ht
  simp only [mem_insert_iff, mem_singleton_iff] at ht
  rcases ht with rfl | rfl | rfl
  · exact h i
  · exact h j
  · exact h k

/-- The finite all-free beta-prime fit is unique almost everywhere when attained. -/
theorem all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain id) (likelihood id w) x p →
        UniqueAE.Winner (domain id) (likelihood id w) x q → p = q :=
  chart_ae_unique id continuous_id Function.injective_id w hw

/-- With shape1 held, the finite beta-prime fit is unique almost everywhere when attained. -/
theorem held_shape1_ae_unique (a : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain fun z ↦ (a, z)) (likelihood (fun z ↦ (a, z)) w) x p →
        UniqueAE.Winner (domain fun z ↦ (a, z)) (likelihood (fun z ↦ (a, z)) w) x q → p = q :=
  chart_ae_unique (fun z ↦ (a, z)) (continuous_const.prodMk continuous_id)
    (fun _ _ h ↦ congrArg Prod.snd h) w hw

/-- With shape2 held, the finite beta-prime fit is unique almost everywhere when attained. -/
theorem held_shape2_ae_unique (b : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∃ i j k, x i < x j ∧ x j < x k) →
      ∀ p q, UniqueAE.Winner (domain fun z : ℝ × ℝ ↦ (z.1, b, z.2))
        (likelihood (fun z : ℝ × ℝ ↦ (z.1, b, z.2)) w) x p →
        UniqueAE.Winner (domain fun z : ℝ × ℝ ↦ (z.1, b, z.2))
          (likelihood (fun z : ℝ × ℝ ↦ (z.1, b, z.2)) w) x q → p = q :=
  chart_ae_unique (fun z : ℝ × ℝ ↦ (z.1, b, z.2))
    (continuous_fst.prodMk (continuous_const.prodMk continuous_snd))
    (fun _ _ h ↦ Prod.ext (congrArg (fun z : Parameters ↦ z.1) h) (congrArg (fun z ↦ z.2.2) h)) w hw

end
end UnivariateML.AE2BetaPrime
end
