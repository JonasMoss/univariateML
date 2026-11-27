module

public import UnivariateML.Generic.AE3WeightedChart
public import UnivariateML.Family.ExpSumSeparation
public import UnivariateML.Family.Beta
import Mathlib.Analysis.SpecialFunctions.Complex.Analytic
import Mathlib.Tactic

set_option linter.style.header false

/-! Four-level uniqueness for the GB2 likelihood and its fixed-threshold submodels. -/

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap
open scoped Topology
open UnivariateML.ExpSumSeparation

namespace UnivariateML.AE3GB2

noncomputable section

variable {n : ℕ}

/-- Upper shape, power, lower shape and scale, in that order. -/
abbrev Parameters := ℝ × ℝ × ℝ × ℝ

/-- The strict-support finite likelihood chart. -/
def domain {P : Type*} (m : ℝ) (param : P → Parameters) :
    Set (P × (Fin n → ℝ)) :=
  {z | 0 < (param z.1).1 ∧ 0 < (param z.1).2.1 ∧
    0 < (param z.1).2.2.1 ∧ 0 < (param z.1).2.2.2 ∧ ∀ i, m < z.2 i}

/-- Weighted likelihood on physical levels, with the beta normalizer. -/
def likelihood {P : Type*} (m : ℝ) (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : ℝ :=
  let (u, p, l, s) := param v
  ∑ i, (w i : ℝ) * (Real.log p - Beta.logBeta (l, u) - l * p * Real.log s +
    gb2Kernel m u p l s (x i))

private def score (m : ℝ) (z : Parameters) (x : ℝ) : ℝ :=
  let (u, p, l, s) := z
  let e := Real.exp ((-p) * Real.log s) * Real.exp (p * Real.log (x - m))
  (l * p - 1 - (l + u) * p * e / (1 + e)) / (x - m)

private theorem kernel_hasDeriv {m u p l s x : ℝ} (hx : m < x) :
    HasDerivAt (gb2Kernel m u p l s) (score m (u, p, l, s) x) x := by
  have ht := ((hasDerivAt_id x).sub_const m).log (sub_ne_zero.mpr hx.ne')
  have he := ((ht.const_mul p).exp.const_mul (Real.exp ((-p) * Real.log s)))
  have hpositive : 0 < 1 + Real.exp ((-p) * Real.log s) *
      Real.exp (p * Real.log (x - m)) := by positivity
  convert ((ht.const_mul (l * p - 1)).sub
    (((he.const_add 1).log hpositive.ne').const_mul (l + u))) using 1
  · rfl
  · simp only [score, id_eq, div_eq_mul_inv, one_mul]
    ring

private def sampleDeriv {P : Type*} (m : ℝ) (param : P → Parameters) (w : Fin n → ℕ)
    (v : P) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score m (param v) (x i)) • proj i

private theorem hasSampleDeriv {P : Type*} (m : ℝ) (param : P → Parameters)
    (w : Fin n → ℕ) (v : P) (x : Fin n → ℝ) (hz : (v, x) ∈ domain m param) :
    HasFDerivAt (likelihood m param w v) (sampleDeriv m param w v x) x := by
  unfold likelihood sampleDeriv
  rcases h : param v with ⟨u, p, l, s⟩
  apply HasFDerivAt.fun_sum
  intro i _
  have hh := (((kernel_hasDeriv (u := u) (p := p) (l := l) (s := s)
    (hz.2.2.2.2 i)).const_add
    (Real.log p - Beta.logBeta (l, u) - l * p * Real.log s)).const_mul
    (w i : ℝ))
  have hc := hh.comp_hasFDerivAt x (proj i : (Fin n → ℝ) →L[ℝ] ℝ).hasFDerivAt
  exact hc

private theorem chart_ae_unique {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (m : ℝ) (param : P → Parameters)
    (cp : Continuous param) (hp : Function.Injective param)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m param) (likelihood m param w) x p →
        UniqueAE.Winner (domain m param) (likelihood m param w) x q → p = q := by
  let S := domain (n := n) m param
  let T := P × (Fin n → ℝ)
  let u (z : T) := (param z.1).1
  let p (z : T) := (param z.1).2.1
  let l (z : T) := (param z.1).2.2.1
  let s (z : T) := (param z.1).2.2.2
  let t (i : Fin n) (z : T) := z.2 i - m
  have cu : Continuous u := cp.fst.comp continuous_fst
  have cp' : Continuous p := cp.snd.fst.comp continuous_fst
  have cl : Continuous l := cp.snd.snd.fst.comp continuous_fst
  have cs : Continuous s := cp.snd.snd.snd.comp continuous_fst
  have cx (i : Fin n) : Continuous (fun z : T ↦ z.2 i) :=
    (continuous_apply i).comp continuous_snd
  have ct (i : Fin n) : Continuous (t i) := (cx i).sub continuous_const
  have pu (z : T) (hz : z ∈ S) : 0 < u z := hz.1
  have pp (z : T) (hz : z ∈ S) : 0 < p z := hz.2.1
  have pl (z : T) (hz : z ∈ S) : 0 < l z := hz.2.2.1
  have ps (z : T) (hz : z ∈ S) : 0 < s z := hz.2.2.2.1
  have pt (i : Fin n) (z : T) (hz : z ∈ S) : 0 < t i z := sub_pos.mpr (hz.2.2.2.2 i)
  have ho : IsOpen S := by
    have he : S = {z | 0 < u z} ∩ {z | 0 < p z} ∩ {z | 0 < l z} ∩
        {z | 0 < s z} ∩ ⋂ i, {z | m < z.2 i} := by
      ext z
      simp [S, domain, u, p, l, s, and_assoc]
      rfl
    rw [he]
    exact ((((isOpen_lt continuous_const cu).inter (isOpen_lt continuous_const cp')).inter
      (isOpen_lt continuous_const cl)).inter (isOpen_lt continuous_const cs)).inter
      (isOpen_iInter_of_finite fun i ↦ isOpen_lt continuous_const (cx i))
  have cg {f : T → ℝ} (cf : Continuous f) (hf : ∀ z ∈ S, 0 < f z) :
      ContinuousOn (fun z ↦ Real.Gamma (f z)) S :=
    fun z hz ↦ ((Real.hasDerivAt_Gamma_of_pos (hf z hz)).continuousAt.comp
      cf.continuousAt).continuousWithinAt
  have cn : ContinuousOn (fun z ↦ Beta.logBeta (l z, u z)) S := by
    unfold Beta.logBeta Beta.normalizer
    have pg z hz := Real.Gamma_pos_of_pos (add_pos (pl z hz) (pu z hz))
    exact (((cg cl pl).mul (cg cu pu)).div
      (cg (cl.add cu) (fun z hz ↦ add_pos (pl z hz) (pu z hz))) (fun z hz ↦ (pg z hz).ne')).log
      (fun z hz ↦ (div_pos (mul_pos (Real.Gamma_pos_of_pos (pl z hz))
        (Real.Gamma_pos_of_pos (pu z hz))) (pg z hz)).ne')
  let e (i : Fin n) (z : T) :=
    Real.exp ((-p z) * Real.log (s z)) * Real.exp (p z * Real.log (t i z))
  have cls := cs.continuousOn.log (fun z hz ↦ (ps z hz).ne')
  have clt (i : Fin n) := (ct i).continuousOn.log (fun z hz ↦ (pt i z hz).ne')
  have ce (i : Fin n) : ContinuousOn (e i) S :=
    (Real.continuous_exp.comp_continuousOn (cp'.continuousOn.neg.mul cls)).mul
      (Real.continuous_exp.comp_continuousOn (cp'.continuousOn.mul (clt i)))
  have ne (i : Fin n) (z : T) : 1 + e i z ≠ 0 := by dsimp [e]; positivity
  have hl : ContinuousOn (fun z : T ↦ likelihood m param w z.1 z.2) S := by
    unfold likelihood
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.mul
      ((((cp'.continuousOn.log (fun z hz ↦ (pp z hz).ne')).sub cn).sub
          ((cl.continuousOn.mul cp'.continuousOn).mul cls)).add
        ((((cl.continuousOn.mul cp'.continuousOn).sub continuousOn_const).mul (clt i)).sub
          ((cl.continuousOn.add cu.continuousOn).mul
            ((continuousOn_const.add (ce i)).log (fun z _ ↦ ne i z)))))
  have hd : ContinuousOn (fun z : T ↦ sampleDeriv m param w z.1 z.2) S := by
    unfold sampleDeriv
    apply continuousOn_finsetSum
    intro i _
    have hh : ContinuousOn (fun z : T ↦ score m (param z.1) (z.2 i)) S :=
      ((((cl.continuousOn.mul cp'.continuousOn).sub continuousOn_const).sub
        ((((cl.continuousOn.add cu.continuousOn).mul cp'.continuousOn).mul (ce i)).div
          (continuousOn_const.add (ce i)) (fun z _ ↦ ne i z))).div
        (ct i).continuousOn (fun z hz ↦ (pt i z hz).ne'))
    exact (continuousOn_const.mul hh).smul continuousOn_const
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hd
    (hasSampleDeriv m param w)] with x he hlevels v v' hv hv'
  have single (a : P) (i : Fin n) :
      sampleDeriv m param w a x (Pi.single i 1) = (w i : ℝ) * score m (param a) (x i) := by
    simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single i]
    · simp
    · intro j _ hji
      simp [Pi.single_eq_of_ne hji]
    · simp
  have hscore (i : Fin n) : score m (param v) (x i) = score m (param v') (x i) := by
    have hh := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) (he v v' hv hv')
    rw [single, single] at hh
    exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') hh
  apply hp
  obtain ⟨hu, hpow, hlo, hs⟩ := fpareto_held_min_separation
    hv.1.1 hv.1.2.1 hv.1.2.2.1 hv.1.2.2.2.1
    hv'.1.1 hv'.1.2.1 hv'.1.2.2.1 hv'.1.2.2.2.1
    (Finset.univ.image x) hlevels
    (fun a ha ↦ by
      obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ha
      exact hv.1.2.2.2.2 i)
    (fun a ha ↦ by
      obtain ⟨i, _, rfl⟩ := Finset.mem_image.mp ha
      rw [(kernel_hasDeriv (hv.1.2.2.2.2 i)).deriv,
        (kernel_hasDeriv (hv'.1.2.2.2.2 i)).deriv]
      exact hscore i)
  exact Prod.ext hu (Prod.ext hpow (Prod.ext hlo hs))

/-- All-free GB2 winners agree almost everywhere on four or more levels. -/
theorem gb2_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 id) (likelihood 0 id w) x p →
        UniqueAE.Winner (domain 0 id) (likelihood 0 id w) x q → p = q :=
  chart_ae_unique 0 id continuous_id Function.injective_id w hw

/-- GB2 at held upper shape has at most one attained winner almost everywhere on four or more
levels. -/
theorem gb2_held_shape1_ae_unique (u : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique 0 (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- GB2 at held lower shape has at most one attained winner almost everywhere on four or more
levels. -/
theorem gb2_held_shape3_ae_unique (l : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Feller–Pareto at held minimum has at most one attained winner almost everywhere on four
or more levels. -/
theorem fpareto_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m id)
        (likelihood m id w) x p →
        UniqueAE.Winner (domain m id)
          (likelihood m id w) x q → p = q :=
  chart_ae_unique m id continuous_id Function.injective_id w hw

/-- Feller–Pareto at held minimum and upper shape has at most one attained winner almost
everywhere on four or more levels. -/
theorem fpareto_held_min_shape1_ae_unique (m u : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique m (fun z : ℝ × ℝ × ℝ ↦ (u, z.1, z.2.1, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Feller–Pareto at held minimum and lower shape has at most one attained winner almost
everywhere on four or more levels. -/
theorem fpareto_held_min_shape3_ae_unique (m l : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, l, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Burr XII has at most one attained winner almost everywhere on four or more levels. -/
theorem burr_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique 0 (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Pareto IV at held minimum has at most one attained winner almost everywhere on four or
more levels. -/
theorem pareto4_held_min_ae_unique (m : ℝ) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
        (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x p →
        UniqueAE.Winner (domain m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)))
          (likelihood m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique m (fun z : ℝ × ℝ × ℝ ↦ (z.1, z.2.1, 1, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Inverse Burr in its positive-power GB2 chart has at most one attained winner almost
everywhere on four or more levels. -/
theorem invburr_all_free_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, 4 ≤ (Finset.univ.image x).card →
      ∀ p q, UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)))
        (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)) w) x p →
        UniqueAE.Winner (domain 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)))
          (likelihood 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)) w) x q → p = q := by
  apply chart_ae_unique 0 (fun z : ℝ × ℝ × ℝ ↦ (1, z.2.1, z.1, z.2.2)) (by fun_prop) ?_ w hw
  intro a b h
  simpa [Prod.ext_iff, and_left_comm] using h

/-- Shape and threshold are the two coordinates in the moving-minimum chart. -/
def movingSupport (p l s : ℝ) : Set ((Fin 2 → ℝ) × ℝ) :=
  {z | 0 < z.1 0 ∧ z.1 1 < z.2 ∧ 0 < p ∧ 0 < l ∧ 0 < s ∧ 1 < l * p}

/-- Observation-dependent term with power, lower shape, and scale held. -/
def movingKernel (p l s : ℝ) (z : (Fin 2 → ℝ) × ℝ) : ℝ :=
  gb2Kernel (z.1 1) (z.1 0) p l s z.2

/-- Parameter-dependent normalization with fixed power, lower shape, and scale. -/
def movingNormalizer (p l s : ℝ) (z : Fin 2 → ℝ) : ℝ :=
  Real.log p - Beta.logBeta (l, z 0) - l * p * Real.log s

private theorem scaled_score_limit {p l s u m : ℝ} (hp : 0 < p) :
    Tendsto (fun t : ℝ ↦ t * score m (u, p, l, s) (m + t)) (𝓝[>] 0) (𝓝 (l * p - 1)) := by
  have hlog := Real.tendsto_log_nhdsGT_zero
  have hexp := Real.tendsto_exp_atBot.comp (hlog.const_mul_atBot hp)
  have he := hexp.const_mul (Real.exp (-p * Real.log s))
  have hh := ((he.const_mul ((l + u) * p)).div
    ((tendsto_const_nhds (x := (1 : ℝ))).add he) (by simp)).const_sub (l * p - 1)
  simp only [mul_zero, add_zero, div_one, sub_zero] at hh
  refine hh.congr' ?_
  filter_upwards [self_mem_nhdsWithin] with t ht
  have hpos : 0 < t := ht
  dsimp [score, Function.comp_def]
  rw [add_sub_cancel_left]
  field_simp

private theorem moving_identify {p l s u v m k : ℝ}
    (hp : 0 < p) (hbound : 1 < l * p)
    (he : ∀ x > max m k, score m (u, p, l, s) x = score k (v, p, l, s) x) :
    u = v ∧ m = k := by
  have no_cross {u v m k : ℝ} (hgt : k < m)
      (he : ∀ x > m, score m (u, p, l, s) x = score k (v, p, l, s) x) : False := by
    have ht : ContinuousAt (fun t : ℝ ↦ m + t - k) 0 := by fun_prop
    have hn : (fun t : ℝ ↦ m + t - k) 0 ≠ 0 := by simpa using (sub_pos.mpr hgt).ne'
    have cE := (Real.continuous_exp.continuousAt.comp
        ((continuousAt_const (y := p)).mul (ht.log hn)))
    have cE' := (continuousAt_const (y := Real.exp (-p * Real.log s))).mul cE
    have hE : 1 + Real.exp (-p * Real.log s) *
        Real.exp (p * Real.log (m + 0 - k)) ≠ 0 := by positivity
    have h := ((continuousAt_const (y := l * p - 1)).sub
      (((continuousAt_const (y := (l + v) * p)).mul cE').div
        (continuousAt_const.add cE') hE)).div ht hn
    have hb : Tendsto (fun t : ℝ ↦ t * score k (v, p, l, s) (m + t)) (𝓝[>] 0) (𝓝 0) := by
      have h' : ContinuousAt (fun t : ℝ ↦ score k (v, p, l, s) (m + t)) 0 := h
      have hh : ContinuousAt (fun t : ℝ ↦ t * score k (v, p, l, s) (m + t)) 0 :=
        continuous_id.continuousAt.mul h'
      simpa only [zero_mul] using hh.continuousWithinAt (s := Ioi (0 : ℝ)) |>.tendsto
    have hh := tendsto_nhds_unique_of_eventuallyEq (scaled_score_limit (l := l) (s := s)
      (u := u) (m := m) hp) hb (by
        filter_upwards [self_mem_nhdsWithin] with t ht
        rw [he (m + t) (lt_add_of_pos_right _ (show 0 < t from ht))])
    linarith
  have hmk : m = k := by
    rcases lt_trichotomy m k with h | h | h
    · exact (no_cross h (fun x hx ↦ (he x (max_lt_iff.mpr ⟨h.trans hx, hx⟩)).symm)).elim
    · exact h
    · exact (no_cross h (fun x hx ↦ he x (max_lt_iff.mpr ⟨hx, h.trans hx⟩))).elim
  subst k
  have h := he (m + 1) (by simp)
  have hepos : 0 < Real.exp (-p * Real.log s) := Real.exp_pos _
  simp only [score, add_sub_cancel_left, Real.log_one, mul_zero, Real.exp_zero,
    mul_one, div_one] at h
  have hh : (u - v) * (p * Real.exp (-p * Real.log s)) = 0 := by
    field_simp [show 1 + Real.exp (-p * Real.log s) ≠ 0 by positivity] at h
    simp only [neg_mul] at *
    linear_combination -h
  exact ⟨sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_right (mul_pos hp hepos).ne'), rfl⟩

set_option maxHeartbeats 650000 in
-- Elaboration combines the analytic chart, its scalar score, and the incidence application.
/-- With power, lower shape and scale held and lower-end exponent above one,
finite Feller–Pareto winners agree almost everywhere on at least five levels. -/
theorem fpareto_held_scale_shape2_shape3_ae_unique (p l s : ℝ)
    (hn : 5 ≤ n) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p l s))
        (AE3WeightedChart.likelihood (movingKernel p l s) (movingNormalizer p l s) w) x a →
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p l s))
        (AE3WeightedChart.likelihood (movingKernel p l s) (movingNormalizer p l s) w) x b →
      a = b := by
  let T := (Fin 2 → ℝ) × ℝ
  let u : T → ℝ := fun z ↦ z.1 0
  let m : T → ℝ := fun z ↦ z.1 1
  let U := movingSupport p l s
  let d (z : T) := score (m z) (u z, p, l, s) z.2
  let P := Fin 2 → ℝ
  let ul : T →L[ℝ] ℝ := (proj (0 : Fin 2)).comp (ContinuousLinearMap.fst ℝ P ℝ)
  let ml : T →L[ℝ] ℝ := (proj (1 : Fin 2)).comp (ContinuousLinearMap.fst ℝ P ℝ)
  have hlog (z : T) (hz : z ∈ U) : AnalyticAt ℝ (fun y : T ↦ Real.log (y.2 - m y)) z :=
    (analyticAt_log (sub_pos.mpr hz.2.1)).comp (g := Real.log) (x := z)
      (((ContinuousLinearMap.snd ℝ P ℝ).analyticAt z).sub (ml.analyticAt z))
  let e (z : T) := Real.exp (-p * Real.log s) * Real.exp (p * Real.log (z.2 - m z))
  have he (z : T) (hz : z ∈ U) : AnalyticAt ℝ e z :=
    analyticAt_const.mul ((analyticAt_const.mul (hlog z hz)).rexp')
  have hopen : IsOpen U := by
    have h : U = {z : T | 0 < u z} ∩ {z | m z < z.2} ∩
        {z : T | 0 < p ∧ 0 < l ∧ 0 < s ∧ 1 < l * p} := by
      ext z
      simp [U, movingSupport, u, m, and_assoc]
      rfl
    rw [h]
    exact ((isOpen_lt continuous_const ul.continuous).inter
      (isOpen_lt ml.continuous continuous_snd)).inter isOpen_const
  have hf : AnalyticOnNhd ℝ (movingKernel p l s) U := by
    intro z hz
    have hepos : 0 < 1 + e z := by dsimp [e]; positivity
    have h := (analyticAt_log hepos).comp (g := Real.log) (x := z)
      (analyticAt_const.add (he z hz))
    exact (analyticAt_const.mul (hlog z hz)).sub
      ((analyticAt_const.add (ul.analyticAt z)).mul h)
  have hd : AnalyticOnNhd ℝ d U := by
    intro z hz
    have hne : 1 + e z ≠ 0 := by dsimp [e]; positivity
    have hCoeff := ((analyticAt_const (v := l)).add (ul.analyticAt z)).mul
      (analyticAt_const (v := p))
    have h := (hCoeff.mul (he z hz)).div (analyticAt_const.add (he z hz)) hne
    exact (analyticAt_const.sub h).div
      (((ContinuousLinearMap.snd ℝ P ℝ).analyticAt z).sub (ml.analyticAt z))
      (sub_pos.mpr hz.2.1).ne'
  have hc : ContinuousOn (fun z : T ↦ movingNormalizer p l s z.1) U := by
    intro z hz
    have hu : 0 < u z := hz.1
    have hl : 0 < l := hz.2.2.2.1
    have cg {g : T → ℝ} (hg : Continuous g) (hpos : 0 < g z) :
        ContinuousAt (fun y ↦ Real.Gamma (g y)) z :=
      (Real.hasDerivAt_Gamma_of_pos hpos).continuousAt.comp (g := Real.Gamma)
        (x := z) hg.continuousAt
    have hB : ContinuousAt (fun y : T ↦ Beta.logBeta (l, u y)) z := by
      unfold Beta.logBeta Beta.normalizer
      exact ((continuousAt_const.mul (cg ul.continuous hu)).div
        (cg (continuous_const.add ul.continuous) (add_pos hl hu))
          (Real.Gamma_pos_of_pos (add_pos hl hu)).ne').log
          (div_pos (mul_pos (Real.Gamma_pos_of_pos hl) (Real.Gamma_pos_of_pos hu))
            (Real.Gamma_pos_of_pos (add_pos hl hu))).ne'
    exact ((continuousAt_const.sub hB).sub continuousAt_const).continuousWithinAt
  refine AE3WeightedChart.ae_unique_of_analytic_score (by omega : 2 + 2 < n)
    hopen hf.continuousOn hd hc
    (fun a x hx ↦ kernel_hasDeriv (p := p) (l := l) (s := s) (u := a 0) hx.2.1) ?_ ?_ w hw
  · intro a b
    by_cases h : 0 < a 0 ∧ 0 < b 0 ∧ 0 < p ∧ 0 < l ∧ 0 < s ∧ 1 < l * p
    · convert (convex_Ioi (max (a 1) (b 1))).isPreconnected using 1
      ext t
      simp [U, movingSupport, h.1, h.2.1, h.2.2.1, h.2.2.2.1, h.2.2.2.2.1,
        h.2.2.2.2.2, max_lt_iff]
    · convert isPreconnected_empty using 1
      ext t
      simp only [mem_ofPred_eq, mem_empty_iff_false, iff_false]
      intro ht
      exact h ⟨ht.1.1, ht.2.1, ht.1.2.2⟩
  · rintro a b hne ⟨t, ha, hb⟩
    by_contra! h
    obtain ⟨hu, hm⟩ := moving_identify (m := a 1) (k := b 1) (l := l) (s := s)
      ha.2.2.1 ha.2.2.2.2.2 (fun t ht ↦
        h t ⟨ha.1, (le_max_left _ _).trans_lt ht, ha.2.2⟩
          ⟨hb.1, (le_max_right _ _).trans_lt ht, hb.2.2⟩)
    apply hne
    funext i
    fin_cases i
    · exact hu
    · exact hm

/-- Pareto IV with power and scale held is the lower-shape-one restriction
of the moving-minimum Feller–Pareto chart. -/
theorem pareto4_held_scale_shape2_ae_unique (p s : ℝ) (hn : 5 ≤ n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ a b,
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p 1 s))
        (AE3WeightedChart.likelihood (movingKernel p 1 s) (movingNormalizer p 1 s) w) x a →
      UniqueAE.Winner (AE3WeightedChart.domain (movingSupport p 1 s))
        (AE3WeightedChart.likelihood (movingKernel p 1 s) (movingNormalizer p 1 s) w) x b →
      a = b :=
  fpareto_held_scale_shape2_shape3_ae_unique p 1 s hn w hw


/-- At unit lower-end exponent the support includes contact. -/
def unitSupport (x : Fin n → ℝ) : Set (ℝ × ℝ) :=
  {q | 0 < q.1 ∧ ∀ i, q.2 ≤ x i}

/-- Weighted log likelihood at unit lower-end exponent, including contact. -/
def unitLikelihood (p l s : ℝ) (w : Fin n → ℕ) (x : Fin n → ℝ) (q : ℝ × ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (Real.log p - Real.log s - Beta.logBeta (l, q.1) -
    (l + q.1) * Real.log (1 + ((x i - q.2) / s) ^ p))

set_option maxHeartbeats 450000 in
-- The expanded weighted objectives are needed for the strict convexity comparison.
/-- With the lower-end exponent fixed at one, two attained Feller–Pareto
winners agree pointwise and both have threshold equal to the sample minimum. -/
theorem fpareto_unit_lower_end_unique {p l s : ℝ} (hp : 0 < p) (hl : 0 < l)
    (hs : 0 < s) (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmin : ∀ j, x i ≤ x j)
    {a b : ℝ × ℝ} (ha : a ∈ unitSupport x) (hb : b ∈ unitSupport x)
    (hA : IsMaxOn (unitLikelihood p l s w x) (unitSupport x) a)
    (hB : IsMaxOn (unitLikelihood p l s w x) (unitSupport x) b) :
    a = b ∧ a.2 = x i := by
  have contact (q : ℝ × ℝ) (hq : q ∈ unitSupport x)
      (hmax : IsMaxOn (unitLikelihood p l s w x) (unitSupport x) q) : q.2 = x i := by
    apply le_antisymm (hq.2 i)
    by_contra! hc
    have hterm (j : Fin n) : Real.log (1 + ((x j - x i) / s) ^ p) <
        Real.log (1 + ((x j - q.2) / s) ^ p) := by
      have hg : 0 ≤ (x j - x i) / s := div_nonneg (sub_nonneg.mpr (hmin j)) hs.le
      exact Real.log_lt_log (by have := Real.rpow_nonneg hg p; linarith)
        (add_lt_add_right (Real.rpow_lt_rpow hg
          (div_lt_div_of_pos_right (sub_lt_sub_left hc _) hs) hp) _)
    have hlt : unitLikelihood p l s w x q < unitLikelihood p l s w x (q.1, x i) := by
      unfold unitLikelihood
      apply Finset.sum_lt_sum
      · intro j _
        apply mul_le_mul_of_nonneg_left _ (by positivity)
        exact sub_le_sub_left (mul_le_mul_of_nonneg_left (hterm j).le (add_pos hl hq.1).le) _
      · refine ⟨i, Finset.mem_univ _, mul_lt_mul_of_pos_left ?_ (by exact_mod_cast hw i)⟩
        exact sub_lt_sub_left (mul_lt_mul_of_pos_left (hterm i) (add_pos hl hq.1)) _
    exact hlt.not_ge (hmax (show (q.1, x i) ∈ unitSupport x from ⟨hq.1, hmin⟩))
  have hat := contact a ha hA
  have hbt := contact b hb hB
  suffices he : a.1 = b.1 by exact ⟨Prod.ext he (hat.trans hbt.symm), hat⟩
  by_contra hne
  let u := (1 / 2 : ℝ) * a.1 + (1 / 2 : ℝ) * b.1
  have ha' : (l, a.1) ∈ Beta.domain := ⟨hl, ha.1⟩
  have hb' : (l, b.1) ∈ Beta.domain := ⟨hl, hb.1⟩
  have hpair : (l, a.1) ≠ (l, b.1) := fun h ↦ hne (congrArg Prod.snd h)
  have hc := Beta.logBeta_strictConvex.2 ha' hb' hpair
    (by norm_num : 0 < (1 / 2 : ℝ)) (by norm_num : 0 < (1 / 2 : ℝ))
    (by norm_num : (1 / 2 : ℝ) + 1 / 2 = 1)
  have hc' : Beta.logBeta (l, u) <
      (1 / 2 : ℝ) * Beta.logBeta (l, a.1) + (1 / 2 : ℝ) * Beta.logBeta (l, b.1) := by
    have h : (1 / 2 : ℝ) • (l, a.1) + (1 / 2 : ℝ) • (l, b.1) = (l, u) := by
      ext <;> simp [u, smul_eq_mul]; ring
    rwa [h] at hc
  let W : ℝ := ∑ i, (w i : ℝ)
  let T : ℝ := ∑ j, (w j : ℝ) * Real.log (1 + ((x j - x i) / s) ^ p)
  have hW : 0 < W :=
    Finset.sum_pos' (fun _ _ ↦ by positivity) ⟨i, Finset.mem_univ _, by exact_mod_cast hw i⟩
  have hobj (v : ℝ) : unitLikelihood p l s w x (v, x i) =
      W * (Real.log p - Real.log s - Beta.logBeta (l, v)) - (l + v) * T := by
    unfold unitLikelihood
    let A := Real.log p - Real.log s - Beta.logBeta (l, v)
    let B (j : Fin n) := Real.log (1 + ((x j - x i) / s) ^ p)
    change ∑ j, (w j : ℝ) * (A - (l + v) * B j) = W * A - (l + v) * T
    calc
      _ = ∑ j, ((w j : ℝ) * A - (l + v) * ((w j : ℝ) * B j)) := by
        apply Finset.sum_congr rfl
        intro j _
        ring
      _ = _ := by rw [Finset.sum_sub_distrib, ← Finset.sum_mul, ← Finset.mul_sum]
  have hua : 0 < a.1 := ha.1
  have hub : 0 < b.1 := hb.1
  have hmem : (u, x i) ∈ unitSupport x := ⟨by dsimp [u]; positivity, hmin⟩
  have h₁ := hA hmem
  have h₂ := hB hmem
  rcases a with ⟨a, m⟩
  rcases b with ⟨b, k⟩
  change m = x i at hat
  change k = x i at hbt
  subst m
  subst k
  change unitLikelihood p l s w x (u, x i) ≤ unitLikelihood p l s w x (a, x i) at h₁
  change unitLikelihood p l s w x (u, x i) ≤ unitLikelihood p l s w x (b, x i) at h₂
  rw [hobj, hobj] at h₁ h₂
  have ht := mul_lt_mul_of_pos_left hc' hW
  dsimp [u] at h₁ h₂ ht
  nlinarith only [h₁, h₂, ht]

/-- At unit held power the Pareto IV threshold is closed, and its attained
winner is pointwise unique. -/
theorem pareto4_unit_power_unique {s : ℝ} (hs : 0 < s)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (x : Fin n → ℝ) (i : Fin n) (hmin : ∀ j, x i ≤ x j)
    {a b : ℝ × ℝ} (ha : a ∈ unitSupport x) (hb : b ∈ unitSupport x)
    (hA : IsMaxOn (unitLikelihood 1 1 s w x) (unitSupport x) a)
    (hB : IsMaxOn (unitLikelihood 1 1 s w x) (unitSupport x) b) :
    a = b ∧ a.2 = x i :=
  fpareto_unit_lower_end_unique zero_lt_one zero_lt_one hs w hw x i hmin ha hb hA hB

end

end UnivariateML.AE3GB2

end
