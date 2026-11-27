module

public import Mathlib.Analysis.Convex.Topology
public import Mathlib.Analysis.Normed.Module.Convex
public import Mathlib.Analysis.Normed.Module.FiniteDimension
public import Mathlib.Analysis.Calculus.Deriv.Basic
import Mathlib.Tactic

set_option linter.style.header false

/-! # Stability of a strictly exposed optimizer for concave objectives -/

@[expose] public section

open Set Filter Topology Metric

namespace UnivariateML.FinishProfile

/-- Concavity localizes any beating point inside a sphere on which the
objective is strictly below its value at the center. -/
private theorem inside_of_boundary {E : Type*} [NormedAddCommGroup E]
    [NormedSpace ℝ E] {D : Set E} {f : E → ℝ} {p q : E} {r : ℝ}
    (hf : ConcaveOn ℝ D f) (hp : p ∈ D) (hq : q ∈ D) (hr : 0 < r)
    (hb : ∀ y ∈ sphere p r, f y < f p) (hle : f p ≤ f q) :
    q ∈ ball p r := by
  by_contra he
  have hd : r ≤ ‖q-p‖ := by simpa [mem_ball, dist_eq_norm] using he
  let a := r / ‖q-p‖
  have ha : 0 < a := div_pos hr (hr.trans_le hd)
  have ha' : a ≤ 1 := (div_le_one (hr.trans_le hd)).mpr hd
  let y := (1-a) • p + a • q
  have hdiff : y-p = a • (q-p) := by dsimp [y]; module
  have hy : y ∈ sphere p r := by
    rw [mem_sphere, dist_eq_norm, hdiff, norm_smul, Real.norm_of_nonneg ha.le]
    exact div_mul_cancel₀ _ (hr.trans_le hd).ne'
  have hc := hf.2 hp hq (sub_nonneg.mpr ha') ha.le (by ring : 1-a+a=1)
  have hh := hb y hy
  change f y < f p at hh
  change (1-a)*f p+a*f q ≤ f y at hc
  nlinarith only [hh, hc, mul_nonneg ha.le (sub_nonneg.mpr hle)]

/-- Joint convergence of concave objectives near a strict interior maximizer
forces convergence of every choice of competing nuisance parameters. -/
theorem tendsto_optimizer {A E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    [FiniteDimensional ℝ E] {l : Filter A} {D : Set E} (hD : IsOpen D)
    {f : A → E → ℝ} {g : E → ℝ} {p : E} {q : A → E} (hp : p ∈ D)
    (hjoint : ∀ y ∈ D, Tendsto (fun av : A × E ↦ f av.1 av.2)
      (l ×ˢ 𝓝 y) (𝓝 (g y)))
    (hstrict : ∀ y ∈ D, y ≠ p → g y < g p)
    (hconc : ∀ᶠ a in l, ConcaveOn ℝ D (f a))
    (hq : ∀ᶠ a in l, q a ∈ D ∧ f a p ≤ f a (q a)) :
    Tendsto q l (𝓝 p) := by
  obtain ⟨R, hR, hsub⟩ := Metric.isOpen_iff.mp hD p hp
  refine Metric.tendsto_nhds.mpr fun ε hε ↦ ?_
  let r := min (ε/2) (R/2)
  have hr : 0 < r := lt_min (by positivity) (by positivity)
  have hR' : r < R := (min_le_right _ _).trans_lt (by linarith)
  have hsub' : sphere p r ⊆ D := fun y hy ↦
    hsub (by rw [mem_ball, mem_sphere.mp hy]; exact hR')
  have hfp : Tendsto (fun a ↦ f a p) l (𝓝 (g p)) :=
    (hjoint p hp).comp (tendsto_id.prodMk tendsto_const_nhds)
  have hall : ∀ᶠ a in l, ∀ y ∈ sphere p r, f a y < f a p := by
    have hmem : {av : A × E | f av.1 av.2 < f av.1 p} ∈ l ×ˢ 𝓝ˢ (sphere p r) :=
      (isCompact_sphere p r).mem_prod_nhdsSet_of_forall fun y hy ↦
        ((hjoint y (hsub' hy)).eventually_lt (hfp.comp tendsto_fst)
          (hstrict y (hsub' hy) (by
            intro h
            subst y
            exact hr.ne' (by simpa using (mem_sphere.mp hy).symm))))
    have he : ∀ᶠ av : A × E in l ×ˢ 𝓝ˢ (sphere p r), f av.1 av.2 < f av.1 p := hmem
    exact he.curry.mono fun _ h ↦ h.self_of_nhdsSet
  filter_upwards [hall, hconc, hq] with a ha hc hqa
  exact (mem_ball.mp (inside_of_boundary hc hp hqa.1 hr ha hqa.2)).trans_le
    ((min_le_left _ _).trans (by linarith))

/-- An envelope derivative for a converging nuisance optimizer. The two
secants at the old and new nuisance choices bound the profile increment. -/
theorem hasDerivAt_profile {E : Type*} [NormedAddCommGroup E] [NormedSpace ℝ E]
    {f : ℝ → E → ℝ} {P : ℝ → ℝ} {t : ℝ} {p : E} {q : ℝ → E}
    {D : (ℝ × E) →L[ℝ] ℝ}
    (hd : HasStrictFDerivAt (fun ap : ℝ × E ↦ f ap.1 ap.2) D (t,p))
    (hq : Tendsto q (𝓝 t) (𝓝 p)) (hP : P t = f t p)
    (hmax : ∀ᶠ a in 𝓝 t, f a p ≤ P a ∧ P a = f a (q a) ∧ f t (q a) ≤ P t) :
    HasDerivAt P (D (1,0)) t := by
  rw [hasDerivAt_iff_isLittleO, Asymptotics.isLittleO_iff]
  intro ε hε
  have hb := Asymptotics.isLittleO_iff.mp hd.isLittleO hε
  have bound (v : ℝ → E) (hv : Tendsto v (𝓝 t) (𝓝 p)) :
      ∀ᶠ a in 𝓝 t, |f a (v a) - f t (v a) - (a-t) * D (1,0)| ≤ ε * |a-t| := by
    have h : Tendsto (fun a : ℝ ↦ ((a,v a),(t,v a))) (𝓝 t) (𝓝 ((t,p),(t,p))) :=
      (tendsto_id.prodMk_nhds hv).prodMk_nhds (tendsto_const_nhds.prodMk_nhds hv)
    filter_upwards [h.eventually hb] with a ha
    have he : ((a,v a) : ℝ × E)-(t,v a) = (a-t,0) := by simp
    have hD : D (a-t,0) = (a-t)*D (1,0) := by
      have he' : (a-t) • ((1,0) : ℝ × E) = (a-t,0) := by simp
      simpa only [he', smul_eq_mul] using D.map_smul (a-t) ((1,0) : ℝ × E)
    simpa only [mem_ofPred_eq, he, hD, Prod.norm_def, norm_zero, max_eq_left (norm_nonneg (a-t)),
      Real.norm_eq_abs, max_eq_left (abs_nonneg (a-t))] using ha
  filter_upwards [bound (fun _ ↦ p) tendsto_const_nhds, bound q hq, hmax] with a h₁ h₂ hm
  rw [abs_le] at h₁ h₂
  rw [Real.norm_eq_abs, Real.norm_eq_abs, smul_eq_mul, abs_le]
  rw [hP] at hm ⊢
  constructor <;> linarith only [h₁.1, h₂.2, hm.1, hm.2.1, hm.2.2]

end UnivariateML.FinishProfile
