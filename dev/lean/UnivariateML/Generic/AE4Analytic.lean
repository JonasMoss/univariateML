module

public import UnivariateML.Generic.AE1Incidence
public import UnivariateML.Generic.UniqueAE
public import Mathlib.Analysis.Analytic.Order
public import Mathlib.Analysis.Calculus.FDeriv.Analytic
public import Mathlib.Analysis.Normed.Module.FiniteDimension

set_option linter.style.header false

@[expose] public section

open Set Filter MeasureTheory ContinuousLinearMap
open scoped Topology ContDiff

namespace UnivariateML.AE4Analytic

variable {d n : ℕ}

private theorem iterated_analytic {f : (Fin d → ℝ) × ℝ → ℝ}
    {z : (Fin d → ℝ) × ℝ} (hf : AnalyticAt ℝ f z) (k : ℕ) :
    AnalyticAt ℝ (fun y ↦ iteratedDeriv k (fun t ↦ f (y.1, t)) y.2) z := by
  induction k with
  | zero => simpa using hf
  | succ k ih =>
    let g : (Fin d → ℝ) × ℝ → ℝ := fun y ↦ iteratedDeriv k (fun t ↦ f (y.1, t)) y.2
    have ha : AnalyticAt ℝ g z := ih
    let ev := ContinuousLinearMap.apply ℝ ℝ ((0, 1) : (Fin d → ℝ) × ℝ)
    have h := AnalyticAt.comp (ev.analyticAt _) ha.fderiv
    apply h.congr
    filter_upwards [ha.eventually_analyticAt] with y hy
    have hd := hy.differentiableAt.hasFDerivAt
    have hin : HasDerivAt (fun t : ℝ ↦ (y.1, t)) (0, 1) y.2 := by
      exact (hasDerivAt_const y.2 y.1).prodMk (hasDerivAt_id y.2)
    simpa [ev, g, iteratedDeriv_succ, Function.comp_def] using
      (hd.comp_hasDerivAt y.2 hin).deriv.symm

private theorem multiplicity {f : ℝ → ℝ} {x : ℝ} (ha : AnalyticAt ℝ f x)
    (hn : ¬ ∀ᶠ t in 𝓝 x, f t = 0) (hz : f x = 0) :
    ∃ k : ℕ, iteratedDeriv k f x = 0 ∧ iteratedDeriv (k + 1) f x ≠ 0 := by
  have ht : analyticOrderAt f x ≠ ⊤ := mt analyticOrderAt_eq_top.mp hn
  have ho := ENat.natCast_toNat_eq_self.mpr ht
  have h := (analyticOrderAt_eq_nat_iff_iteratedDeriv_eq_zero ha).mp ho.symm
  cases hk : (analyticOrderAt f x).toNat with
  | zero => exact False.elim (h.2 (by simpa [hk] using hz))
  | succ k =>
    exact ⟨k, h.1 k (by omega), by simpa [hk] using h.2⟩

/-- A family of nonzero analytic germs cannot have simultaneous roots at
more freely varying levels than there are real parameter coordinates.
The admissibility set can depend on both the parameters and the levels. -/
theorem ae_no_analytic_incidence (hdn : d < n)
    (S : Set ((Fin d → ℝ) × (Fin n → ℝ))) (f : (Fin d → ℝ) × ℝ → ℝ)
    (ha : ∀ z ∈ S, ∀ i, AnalyticAt ℝ f (z.1, z.2 i))
    (hn : ∀ z ∈ S, ∀ i, ¬ ∀ᶠ t in 𝓝 (z.2 i), f (z.1, t) = 0) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p, (p, x) ∈ S → ∃ i, f (p, x i) ≠ 0 := by
  classical
  let F (k : Fin n → ℕ) (z : (Fin d → ℝ) × (Fin n → ℝ)) (i : Fin n) :=
    iteratedDeriv (k i) (fun t ↦ f (z.1, t)) (z.2 i)
  let T (k : Fin n → ℕ) := {z ∈ S | F k z = 0 ∧ ∀ i,
    iteratedDeriv (k i + 1) (fun t ↦ f (z.1, t)) (z.2 i) ≠ 0}
  have smooth (k : Fin n → ℕ) (z) (hz : z ∈ T k) : ContDiffAt ℝ 1 (F k) z := by
    apply contDiffAt_pi.mpr
    intro i
    let g : ((Fin d → ℝ) × (Fin n → ℝ)) → ((Fin d → ℝ) × ℝ) :=
      fun y ↦ (y.1, y.2 i)
    have hg : AnalyticAt ℝ g z := by
      apply analyticAt_fst.prod
      exact AnalyticAt.comp ((proj i : (Fin n → ℝ) →L[ℝ] ℝ).analyticAt _) analyticAt_snd
    have hc : AnalyticAt ℝ (fun y ↦
        iteratedDeriv (k i) (fun t ↦ f ((g y).1, t)) (g y).2) z :=
      AnalyticAt.comp (f := g) (x := z) (iterated_analytic (ha z hz.1 i) (k i)) hg
    exact hc.contDiffAt
  have h := UnivariateML.AE1Incidence.ae_no_implicit_incidence hdn T F
    (fun _ _ hz ↦ hz.2.1) smooth ?_
  · filter_upwards [h] with x hx p hp
    by_contra! hzero
    have hmult (i : Fin n) : ∃ k : ℕ,
        iteratedDeriv k (fun t ↦ f (p, t)) (x i) = 0 ∧
        iteratedDeriv (k + 1) (fun t ↦ f (p, t)) (x i) ≠ 0 :=
      multiplicity ((ha (p, x) hp i).comp (analyticAt_const.prod analyticAt_id))
        (hn (p, x) hp i) (hzero i)
    choose k hk using hmult
    exact hx k p ⟨hp, funext fun i ↦ (hk i).1, fun i ↦ (hk i).2⟩
  · intro k z hz
    let a (i : Fin n) := iteratedDeriv (k i + 1) (fun t ↦ f (z.1, t)) (z.2 i)
    let e : (Fin n → ℝ) ≃L[ℝ] (Fin n → ℝ) :=
      ContinuousLinearEquiv.piCongrRight (fun i ↦
        (LinearEquiv.smulOfNeZero ℝ ℝ (a i) (hz.2.2 i)).toContinuousLinearEquiv)
    have hs := smooth k z hz
    have hd : HasFDerivAt (fun x ↦ F k (z.1, x))
        (fderiv ℝ (F k) z ∘L inr ℝ (Fin d → ℝ) (Fin n → ℝ)) z.2 := by
      have hin : HasFDerivAt (fun x : Fin n → ℝ ↦ (z.1, x))
          (inr ℝ (Fin d → ℝ) (Fin n → ℝ)) z.2 := by
        simpa using ((inr ℝ (Fin d → ℝ) (Fin n → ℝ)).hasFDerivAt
          (x := z.2)).const_add (z.1, 0)
      exact hs.differentiableAt_one.hasFDerivAt.comp z.2 hin
    have he : HasFDerivAt (fun x ↦ F k (z.1, x)) (e : _ →L[ℝ] _) z.2 := by
      apply hasFDerivAt_pi''
      intro i
      have hi : AnalyticAt ℝ (iteratedDeriv (k i) (fun t ↦ f (z.1, t))) (z.2 i) :=
        (iterated_analytic (ha z hz.1 i) (k i)).comp (analyticAt_const.prod analyticAt_id)
      have hder := hi.differentiableAt.hasDerivAt
      rw [← iteratedDeriv_succ] at hder
      exact hder.comp_hasFDerivAt z.2 (proj i : (Fin n → ℝ) →L[ℝ] ℝ).hasFDerivAt
    rw [hd.unique he]
    exact isInvertible_equiv


/-- If distinct parameters have distinct analytic spatial scores, an
attained fit on a fixed-count chart is almost everywhere unique once
the number of freely varying levels exceeds twice the parameter dimension. -/
theorem ae_unique_analytic_scores (hdn : d + d < n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set (Fin d → ℝ)) (hA : IsOpen A)
    (L g : (Fin d → ℝ) → ℝ → ℝ)
    (hL : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ L z.1 z.2) (A ×ˢ univ))
    (hg : ∀ p ∈ A, ∀ t, AnalyticAt ℝ (fun z : (Fin d → ℝ) × ℝ ↦ g z.1 z.2) (p, t))
    (hDeriv : ∀ p ∈ A, ∀ t, HasDerivAt (L p) (g p t) t)
    (hsep : ∀ p ∈ A, ∀ q ∈ A, (∀ t, g p t = g q t) → p = q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ A, ∀ q ∈ A,
      IsMaxOn (fun r ↦ ∑ i, (w i : ℝ) * L r (x i)) A p →
      IsMaxOn (fun r ↦ ∑ i, (w i : ℝ) * L r (x i)) A q → p = q := by
  classical
  let left : (Fin (d+d) → ℝ) →L[ℝ] (Fin d → ℝ) := pi (fun i ↦ proj (Fin.castAdd d i))
  let right : (Fin (d+d) → ℝ) →L[ℝ] (Fin d → ℝ) := pi (fun i ↦ proj (Fin.natAdd d i))
  let S : Set ((Fin (d+d) → ℝ) × (Fin n → ℝ)) :=
    {z | left z.1 ∈ A ∧ right z.1 ∈ A ∧ left z.1 ≠ right z.1}
  let F (z : (Fin (d+d) → ℝ) × ℝ) := g (left z.1) z.2 - g (right z.1) z.2
  have hmap (u : (Fin (d+d) → ℝ) →L[ℝ] (Fin d → ℝ)) (z : (Fin (d+d) → ℝ) × ℝ) :
      AnalyticAt ℝ (fun y ↦ (u y.1, y.2)) z :=
    (AnalyticAt.comp (u.analyticAt z.1) analyticAt_fst).prod analyticAt_snd
  have ha (z : (Fin (d+d) → ℝ) × (Fin n → ℝ)) (hz : z ∈ S) (i : Fin n) :
      AnalyticAt ℝ F (z.1, z.2 i) := by
    change AnalyticAt ℝ (fun y : (Fin (d+d) → ℝ) × ℝ ↦
      g (left y.1) y.2 - g (right y.1) y.2) (z.1, z.2 i)
    exact (AnalyticAt.comp (x := (z.1, z.2 i)) (f := fun y : (Fin (d+d) → ℝ) × ℝ ↦ (left y.1, y.2))
      (hg _ hz.1 _) (hmap left _)).sub
      (AnalyticAt.comp (x := (z.1, z.2 i)) (f := fun y : (Fin (d+d) → ℝ) × ℝ ↦ (right y.1, y.2))
        (hg _ hz.2.1 _) (hmap right _))
  have hinc := ae_no_analytic_incidence hdn S F ha ?_
  · let T : Set ((Fin d → ℝ) × (Fin n → ℝ)) := A ×ˢ univ
    let Lt (p : Fin d → ℝ) (x : Fin n → ℝ) := ∑ i, (w i : ℝ) * L p (x i)
    let D (p : Fin d → ℝ) (x : Fin n → ℝ) : (Fin n → ℝ) →L[ℝ] ℝ :=
      ∑ i, ((w i : ℝ) * g p (x i)) • proj i
    have hLt : ContinuousOn (fun z : (Fin d → ℝ) × (Fin n → ℝ) ↦ Lt z.1 z.2) T := by
      unfold Lt
      apply continuousOn_finsetSum Finset.univ
      intro i _
      have hc : Continuous (fun z : (Fin d → ℝ) × (Fin n → ℝ) ↦ (z.1, z.2 i)) := by
        fun_prop
      exact (hL.comp hc.continuousOn (fun z (hz : z ∈ T) ↦ ⟨hz.1, mem_univ _⟩)).const_mul
        (w i : ℝ)
    have hDt : ContinuousOn (fun z : (Fin d → ℝ) × (Fin n → ℝ) ↦ D z.1 z.2) T := by
      unfold D
      apply continuousOn_finsetSum Finset.univ
      intro i _
      have hc : ContinuousOn (fun z : (Fin d → ℝ) × (Fin n → ℝ) ↦ g z.1 (z.2 i)) T := by
        have hf : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ g z.1 z.2) (A ×ˢ univ) :=
          fun z hz ↦ (hg z.1 hz.1 z.2).continuousAt.continuousWithinAt
        have he : Continuous (fun z : (Fin d → ℝ) × (Fin n → ℝ) ↦ (z.1, z.2 i)) := by
          fun_prop
        exact hf.comp he.continuousOn (fun z (hz : z ∈ T) ↦ ⟨hz.1, mem_univ _⟩)
      exact (hc.const_mul (w i : ℝ)).smul continuousOn_const
    have hd (p x) (hp : (p, x) ∈ T) : HasFDerivAt (Lt p) (D p x) x := by
      unfold Lt D
      apply HasFDerivAt.fun_sum
      intro i _
      let ev : (Fin n → ℝ) →L[ℝ] ℝ := proj i
      have hh := ((hDeriv p hp.1 (x i)).const_mul (w i : ℝ)).comp_hasFDerivAt x
        (ev.hasFDerivAt (x := x))
      exact hh
    have eval (p : Fin d → ℝ) (x : Fin n → ℝ) (i : Fin n) :
        D p x (Pi.single i 1) = (w i : ℝ) * g p (x i) := by
      simp only [D, sum_apply, smul_apply, proj_apply, smul_eq_mul]
      rw [Finset.sum_eq_single i]
      · simp
      · intro j _ hji
        simp [Pi.single_eq_of_ne hji]
      · simp
    have hclass := UniqueAE.ae_winning_class (μ := volume) (hA.prod isOpen_univ)
      hLt hDt hd id ?_
    · filter_upwards [hclass] with x hx p hp q hq hmaxp hmaxq
      exact hx p q ⟨⟨hp, mem_univ _⟩, fun r hr ↦ hmaxp hr.1⟩
        ⟨⟨hq, mem_univ _⟩, fun r hr ↦ hmaxq hr.1⟩
    · filter_upwards [hinc] with x hx p q hp hq he
      by_contra! hneq
      have hl : left (Fin.append p q) = p := funext (Fin.append_left p q)
      have hr : right (Fin.append p q) = q := funext (Fin.append_right p q)
      obtain ⟨i, hi⟩ := hx (Fin.append p q) (show (Fin.append p q, x) ∈ S by
        change left (Fin.append p q) ∈ A ∧ right (Fin.append p q) ∈ A ∧ _
        simpa [hl, hr] using And.intro hp.1.1 (And.intro hq.1.1 hneq))
      apply hi
      change g (left (Fin.append p q)) (x i) - g (right (Fin.append p q)) (x i) = 0
      rw [hl, hr]
      apply sub_eq_zero.mpr
      have h := congrArg (fun f : (Fin n → ℝ) →L[ℝ] ℝ ↦ f (Pi.single i 1)) he
      rw [eval, eval] at h
      exact mul_left_cancel₀ (by exact_mod_cast (hw i).ne') h
  · intro z hz i hzero
    let f (t : ℝ) := g (left z.1) t - g (right z.1) t
    have ha (t : ℝ) : AnalyticAt ℝ f t :=
      (AnalyticAt.comp (f := Prod.mk (left z.1)) (hg _ hz.1 t)
        (analyticAt_const.prod analyticAt_id)).sub
      (AnalyticAt.comp (f := Prod.mk (right z.1)) (hg _ hz.2.1 t)
        (analyticAt_const.prod analyticAt_id))
    have h : f = 0 := (AnalyticOnNhd.analyticOrderAt_eq_top_iff_eq_zero _ ha).mp
      (analyticOrderAt_eq_top.mpr hzero)
    exact hz.2.2 (hsep _ hz.1 _ hz.2.1 (fun t ↦ sub_eq_zero.mp (congrFun h t)))

/-- Fixed observations can contribute a continuous parameter-dependent
offset to the log likelihood without changing the free-level score count. -/
theorem ae_unique_analytic_scores_with_offset (hdn : d + d < n)
    (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    (A : Set (Fin d → ℝ)) (hA : IsOpen A)
    (L g : (Fin d → ℝ) → ℝ → ℝ) (C : (Fin d → ℝ) → ℝ)
    (hL : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ L z.1 z.2) (A ×ˢ univ))
    (hC : ContinuousOn C A)
    (hg : ∀ p ∈ A, ∀ t, AnalyticAt ℝ (fun z : (Fin d → ℝ) × ℝ ↦ g z.1 z.2) (p, t))
    (hDeriv : ∀ p ∈ A, ∀ t, HasDerivAt (L p) (g p t) t)
    (hsep : ∀ p ∈ A, ∀ q ∈ A, (∀ t, g p t = g q t) → p = q) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p ∈ A, ∀ q ∈ A,
      IsMaxOn (fun r ↦ C r + ∑ i, (w i : ℝ) * L r (x i)) A p →
      IsMaxOn (fun r ↦ C r + ∑ i, (w i : ℝ) * L r (x i)) A q → p = q := by
  let M : ℝ := ∑ i, (w i : ℝ)
  let _ : Nonempty (Fin n) := ⟨⟨0, by omega⟩⟩
  have hM : 0 < M := Finset.sum_pos (fun i _ ↦ by exact_mod_cast hw i) Finset.univ_nonempty
  let L' (p : Fin d → ℝ) (t : ℝ) := L p t + C p/M
  have hL' : ContinuousOn (fun z : (Fin d → ℝ) × ℝ ↦ L' z.1 z.2) (A ×ˢ univ) :=
    hL.add ((hC.comp continuous_fst.continuousOn
      (fun _ (h : _ ∈ A ×ˢ (univ : Set ℝ)) ↦ h.1)).div_const M)
  have hDeriv' (p : Fin d → ℝ) (hp : p ∈ A) (t : ℝ) :
      HasDerivAt (L' p) (g p t) t := (hDeriv p hp t).add_const _
  have hsum (p : Fin d → ℝ) (x : Fin n → ℝ) :
      (∑ i, (w i : ℝ) * L' p (x i)) = C p + ∑ i, (w i : ℝ) * L p (x i) := by
    unfold L'
    rw [Finset.sum_congr rfl (fun i _ ↦ mul_add (w i : ℝ) _ _),
      Finset.sum_add_distrib, ← Finset.sum_mul]
    change _ + M*(C p/M) = _
    rw [← mul_div_assoc, mul_div_cancel_left₀ _ hM.ne', add_comm]
  filter_upwards [ae_unique_analytic_scores hdn w hw A hA L' g hL' hg hDeriv' hsep]
    with x hx p hp q hq hmaxp hmaxq
  simp_rw [← hsum] at hmaxp hmaxq
  exact hx p hp q hq hmaxp hmaxq

end UnivariateML.AE4Analytic

end
