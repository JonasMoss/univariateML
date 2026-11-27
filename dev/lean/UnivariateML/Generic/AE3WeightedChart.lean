module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Generic.AE3AnalyticIncidence
public import Mathlib.Analysis.Calculus.FDeriv.Analytic
public import Mathlib.Analysis.Calculus.Deriv.Comp
import Mathlib.Tactic

set_option linter.style.header false

/-! Application of the winning-class theorem to weighted scalar log densities. -/

@[expose] public section

open Set MeasureTheory ContinuousLinearMap

namespace UnivariateML.AE3WeightedChart

noncomputable section

variable {n : ℕ} {P : Type*}

/-- Open scalar support required at every observed level. -/
def domain (U : Set (P × ℝ)) : Set (P × (Fin n → ℝ)) :=
  {z | ∀ i, (z.1, z.2 i) ∈ U}

/-- Weighted log likelihood, including the candidate-dependent normalizer. -/
def likelihood (f : P × ℝ → ℝ) (c : P → ℝ) (w : Fin n → ℕ)
    (p : P) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (c p + f (p, x i))

private def sampleDeriv (d : P × ℝ → ℝ) (w : Fin n → ℕ)
    (p : P) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * d (p, x i)) • proj i

private theorem sampleDeriv_single (d : P × ℝ → ℝ) (w : Fin n → ℕ)
    (p : P) (x : Fin n → ℝ) (i : Fin n) :
    sampleDeriv d w p x (Pi.single i 1) = (w i : ℝ) * d (p, x i) := by
  simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
  rw [Finset.sum_eq_single i]
  · simp
  · intro j _ hji
    simp [Pi.single_eq_of_ne hji]
  · simp

/-- On a fixed-count chart, separation of the scalar scores of winners
almost everywhere implies uniqueness of the winning class almost everywhere. -/
theorem ae_winning_class [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] {A : Type*} {U : Set (P × ℝ)} (hU : IsOpen U)
    {f d : P × ℝ → ℝ} {c : P → ℝ} (hf : ContinuousOn f U) (hd : ContinuousOn d U)
    (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (cls : P → A) (good : (Fin n → ℝ) → Prop)
    (hsep : ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q →
      (∀ i, d (p, x i) = d (q, x i)) → cls p = cls q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q := by
  let ev (i : Fin n) (z : P × (Fin n → ℝ)) : P × ℝ := (z.1, z.2 i)
  have hev (i : Fin n) : Continuous (ev i) := by fun_prop
  have hm (i : Fin n) : MapsTo (ev i) (domain U) U := fun _ hz ↦ hz i
  have ho : IsOpen (domain (n := n) U) := by
    have hdom : domain (n := n) U = ⋂ i, ev i ⁻¹' U := by
      ext z
      simp [domain, ev]
    rw [hdom]
    exact
      isOpen_iInter_of_finite (fun i ↦ hU.preimage (hev i))
  have hl : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ likelihood f c w z.1 z.2)
      (domain U) := by
    unfold likelihood
    apply continuousOn_finsetSum
    intro i _
    exact continuousOn_const.mul ((hc.comp (hev i).continuousOn (hm i)).add
      (hf.comp (hev i).continuousOn (hm i)))
  have hD : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ sampleDeriv d w z.1 z.2)
      (domain U) := by
    unfold sampleDeriv
    apply continuousOn_finsetSum
    intro i _
    exact (continuousOn_const.mul (hd.comp (hev i).continuousOn (hm i))).smul continuousOn_const
  have hdiff (p : P) (x : Fin n → ℝ) (hx : (p, x) ∈ domain U) :
      HasFDerivAt (likelihood f c w p) (sampleDeriv d w p x) x := by
    unfold likelihood sampleDeriv
    apply HasFDerivAt.fun_sum
    intro i _
    have h := ((hfd p (x i) (hx i)).const_add (c p)).const_mul (w i : ℝ)
    have hcomp := h.comp_hasFDerivAt x (proj i : (Fin n → ℝ) →L[ℝ] ℝ).hasFDerivAt
    exact hcomp
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) ho hl hD hdiff, hsep]
    with x he hx hg p q hp hq
  apply hx hg p q hp hq
  intro i
  have hh := congrArg (fun g : (Fin n → ℝ) →L[ℝ] ℝ ↦ g (Pi.single i 1)) (he p q hp hq)
  rw [sampleDeriv_single, sampleDeriv_single] at hh
  exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') hh

/-- Joint analyticity provides continuity of both the scalar kernel and its sample score. -/
theorem ae_winning_class_analytic [NormedAddCommGroup P] [NormedSpace ℝ P]
    [LocallyCompactSpace P] [SecondCountableTopology P] {A : Type*}
    {U : Set (P × ℝ)} (hU : IsOpen U) {f : P × ℝ → ℝ} {c : P → ℝ}
    (hf : AnalyticOnNhd ℝ f U) (hc : ContinuousOn (fun z : P × ℝ ↦ c z.1) U)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (cls : P → A) (good : (Fin n → ℝ) → Prop)
    (hsep : ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q →
      (∀ i, deriv (fun t ↦ f (p, t)) (x i) = deriv (fun t ↦ f (q, t)) (x i)) →
      cls p = cls q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, good x → ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q := by
  have hder (p : P) (t : ℝ) (hz : (p, t) ∈ U) :
      HasDerivAt (fun s ↦ f (p, s)) (fderiv ℝ f (p, t) (0, 1)) t := by
    have hh : HasDerivAt (fun s : ℝ ↦ (p, s)) (0, 1) t :=
      (hasDerivAt_const t p).prodMk (hasDerivAt_id t)
    exact (hf (p, t) hz).differentiableAt.hasFDerivAt.comp_hasDerivAt t hh
  refine ae_winning_class hU hf.continuousOn
    (d := fun z ↦ fderiv ℝ f z (0, 1)) ?_ hc hder w hw cls good ?_
  · exact (ContinuousLinearMap.apply ℝ ℝ ((0, 1) : P × ℝ)).continuous.comp_continuousOn
      (hf.fderiv_of_isOpen hU).continuousOn
  · filter_upwards [hsep] with x hx hg p q hp hq he
    apply hx hg p q hp hq
    intro i
    rw [(hder p (x i) (hp.1 i)).deriv, (hder q (x i) (hq.1 i)).deriv]
    exact he i

/-- An analytic score identifying winning classes on the common support yields
almost-everywhere class uniqueness when the number of levels exceeds twice the free parameter
dimension. -/
theorem ae_unique_class_of_analytic_score {m : ℕ} {A : Type*} (hn : m + m < n)
    {U : Set ((Fin m → ℝ) × ℝ)} (hU : IsOpen U)
    {f d : (Fin m → ℝ) × ℝ → ℝ} {c : (Fin m → ℝ) → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : (Fin m → ℝ) × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (hconn : ∀ p q, IsPreconnected {t | (p, t) ∈ U ∧ (q, t) ∈ U})
    (cls : (Fin m → ℝ) → A)
    (hident : ∀ p q, cls p ≠ cls q → (∃ t, (p, t) ∈ U ∧ (q, t) ∈ U) →
      ∃ t, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) ≠ d (q, t))
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → cls p = cls q := by
  let left : (Fin (m + m) → ℝ) →L[ℝ] (Fin m → ℝ) :=
    ContinuousLinearMap.pi (fun i ↦ proj (Fin.castAdd m i))
  let right : (Fin (m + m) → ℝ) →L[ℝ] (Fin m → ℝ) :=
    ContinuousLinearMap.pi (fun i ↦ proj (Fin.natAdd m i))
  let V (r : Fin (m + m) → ℝ) : Set ℝ := {t | (left r, t) ∈ U ∧ (right r, t) ∈ U}
  let S : Set (Fin (m + m) → ℝ) := {r | cls (left r) ≠ cls (right r) ∧ (V r).Nonempty}
  let g (z : (Fin (m + m) → ℝ) × ℝ) := d (left z.1, z.2) - d (right z.1, z.2)
  have hga (r) (_ : r ∈ S) (t) (ht : t ∈ V r) : AnalyticAt ℝ g (r, t) := by
    let L := left.prodMap (ContinuousLinearMap.id ℝ ℝ)
    let R := right.prodMap (ContinuousLinearMap.id ℝ ℝ)
    exact ((hd (left r, t) ht.1).comp (x := (r, t)) (L.analyticAt (r, t))).sub
      ((hd (right r, t) ht.2).comp (x := (r, t)) (R.analyticAt (r, t)))
  have hne (r) (hr : r ∈ S) : ∃ t ∈ V r, g (r, t) ≠ 0 := by
    obtain ⟨t, h₁, h₂, he⟩ := hident (left r) (right r) hr.1 hr.2
    exact ⟨t, ⟨h₁, h₂⟩, sub_ne_zero.mpr he⟩
  have he := AE3AnalyticIncidence.ae_no_identified_score_ties hn g S V
    (fun r _ ↦ hconn (left r) (right r)) hga hne
  have huniq := ae_winning_class hU hf hd.continuousOn hc hfd w hw cls (fun _ ↦ True) ?_
  · filter_upwards [huniq] with x hx
    exact hx trivial
  · filter_upwards [he] with x hx _ p q hp hq hscore
    by_contra hn
    let r := Fin.append p q
    have hL : left r = p := funext (Fin.append_left p q)
    have hR : right r = q := funext (Fin.append_right p q)
    have hi (i : Fin n) : x i ∈ V r := by
      change (left r, x i) ∈ U ∧ (right r, x i) ∈ U
      rw [hL, hR]
      exact ⟨hp.1 i, hq.1 i⟩
    have hr : r ∈ S := by
      change cls (left r) ≠ cls (right r) ∧ (V r).Nonempty
      exact ⟨by simpa [hL, hR] using hn, x ⟨0, by omega⟩, hi ⟨0, by omega⟩⟩
    apply hx r hr hi
    intro i
    dsimp only [g]
    rw [hL, hR, sub_eq_zero]
    exact hscore i

/-- An identifiable analytic score on the common support yields almost-everywhere
uniqueness when the number of levels exceeds twice the free parameter dimension. -/
theorem ae_unique_of_analytic_score {m : ℕ} (hn : m + m < n)
    {U : Set ((Fin m → ℝ) × ℝ)} (hU : IsOpen U)
    {f d : (Fin m → ℝ) × ℝ → ℝ} {c : (Fin m → ℝ) → ℝ}
    (hf : ContinuousOn f U) (hd : AnalyticOnNhd ℝ d U)
    (hc : ContinuousOn (fun z : (Fin m → ℝ) × ℝ ↦ c z.1) U)
    (hfd : ∀ p t, (p, t) ∈ U → HasDerivAt (fun s ↦ f (p, s)) (d (p, t)) t)
    (hconn : ∀ p q, IsPreconnected {t | (p, t) ∈ U ∧ (q, t) ∈ U})
    (hident : ∀ p q, p ≠ q → (∃ t, (p, t) ∈ U ∧ (q, t) ∈ U) →
      ∃ t, (p, t) ∈ U ∧ (q, t) ∈ U ∧ d (p, t) ≠ d (q, t))
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q,
      UniqueAE.Winner (domain U) (likelihood f c w) x p →
      UniqueAE.Winner (domain U) (likelihood f c w) x q → p = q :=
  ae_unique_class_of_analytic_score hn hU hf hd hc hfd hconn id hident w hw

end

end UnivariateML.AE3WeightedChart

end
