module

public import UnivariateML.Generic.UniqueAE
public import UnivariateML.Family.Ged
public import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv

import Mathlib.Analysis.Calculus.Deriv.Abs
import Mathlib.MeasureTheory.Measure.Lebesgue.EqHaar
import Mathlib.Tactic

set_option linter.style.header false

/-!
# Fixed-centre GED uniqueness

The free scale is the kernel scale. The held-standard-deviation chart
restricts that scale via `Ged.sdFactor`.

## Deviations

The results compare attained finite
winners, including shape one. Comparisons with the uniform limit and the
exceptional-sample crossing constructions are outside these theorems.
-/

@[expose] public section

open Set Filter MeasureTheory Real ContinuousLinearMap

namespace UnivariateML.AE1GED

noncomputable section

variable {n : ℕ}

/-- Weighted GED log likelihood in kernel scale coordinates (shape, scale). -/
def logLik (w : Fin n → ℕ) (m : ℝ) (p : ℝ × ℝ) (x : Fin n → ℝ) : ℝ :=
  ∑ i, (w i : ℝ) * (-log (2 * p.2 * Gamma (1 + 1 / p.1)) -
    (|x i - m| / p.2) ^ p.1)

private def score (m : ℝ) (p : ℝ × ℝ) (t : ℝ) : ℝ :=
  -(p.1 * (|t - m| / p.2) ^ p.1) / (t - m)

private def sampleDeriv (w : Fin n → ℕ) (m : ℝ) (p : ℝ × ℝ) (x : Fin n → ℝ) :
    (Fin n → ℝ) →L[ℝ] ℝ :=
  ∑ i, ((w i : ℝ) * score m p (x i)) • proj i

private theorem score_at {m t : ℝ} {p : ℝ × ℝ} (hb : 0 < p.2) (ht : t ≠ m) :
    HasDerivAt (fun u ↦ -log (2 * p.2 * Gamma (1 + 1 / p.1)) -
      (|u - m| / p.2) ^ p.1) (score m p t) t := by
  have hr : t - m ≠ 0 := sub_ne_zero.mpr ht
  have ha : 0 < |t - m| / p.2 := div_pos (abs_pos.mpr hr) hb
  have hd := (((hasDerivAt_abs hr).comp t ((hasDerivAt_id t).sub_const m)).div_const p.2)
  have hd' := (hd.rpow_const (p := p.1) (Or.inl ha.ne')).const_sub
    (-log (2 * p.2 * Gamma (1 + 1 / p.1)))
  convert hd' using 1 <;> try rfl
  unfold score
  simp only [id, mul_one, Function.comp_apply, Real.rpow_sub_one ha.ne']
  by_cases h : 0 < t - m
  · simp [abs_of_pos h, h]
    field_simp
  · have hneg : t - m < 0 := lt_of_le_of_ne (le_of_not_gt h) hr
    simp [abs_of_neg hneg, hneg]
    field_simp
    ring

private theorem chart {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (w : Fin n → ℕ) (m : ℝ) (k : P → ℝ × ℝ)
    (A : Set P) (hA : IsOpen A) (hk : ContinuousOn k A)
    (ha : ∀ p ∈ A, 0 < (k p).1) (hb : ∀ p ∈ A, 0 < (k p).2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, (∀ i, x i ≠ m) →
      ∀ p q, IsMaxOn (fun r ↦ logLik w m (k r) x) A p →
        IsMaxOn (fun r ↦ logLik w m (k r) x) A q → p ∈ A → q ∈ A →
        sampleDeriv w m (k p) x = sampleDeriv w m (k q) x := by
  let X : Set (Fin n → ℝ) := ⋂ i, {x | x i ≠ m}
  let S := A ×ˢ X
  have hX : IsOpen X := isOpen_iInter_of_finite fun i ↦
    isOpen_ne_fun (continuous_apply i) continuous_const
  have hS : IsOpen S := hA.prod hX
  have hK : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ k z.1) S :=
    hk.comp continuous_fst.continuousOn (fun _ hz ↦ hz.1)
  have hr (i : Fin n) : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ z.2 i - m) S :=
    (continuous_apply i |>.comp continuous_snd |>.sub continuous_const).continuousOn
  have hn (z : P × (Fin n → ℝ)) (hz : z ∈ S) (i : Fin n) : z.2 i - m ≠ 0 :=
    sub_ne_zero.mpr (mem_iInter.mp hz.2 i)
  have hp (i : Fin n) : ContinuousOn
      (fun z : P × (Fin n → ℝ) ↦ (|z.2 i - m| / (k z.1).2) ^ (k z.1).1) S := by
    have hd := (continuous_abs.comp_continuousOn (hr i)).div hK.snd
      (fun z hz ↦ (hb z.1 hz.1).ne')
    exact hd.rpow hK.fst (fun z hz ↦ Or.inl
      (div_pos (abs_pos.mpr (hn z hz i)) (hb z.1 hz.1)).ne')
  have hg : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ 1 + 1 / (k z.1).1) S :=
    continuousOn_const.add (continuousOn_const.div hK.fst (fun z hz ↦ (ha z.1 hz.1).ne'))
  have hgp (z : P × (Fin n → ℝ)) (hz : z ∈ S) : 0 < 1 + 1 / (k z.1).1 :=
    add_pos zero_lt_one (one_div_pos.mpr (ha z.1 hz.1))
  have hG : ContinuousOn Gamma (Ioi 0) := differentiableOn_Gamma_Ioi.continuousOn
  have hnorm := ((continuousOn_const.mul hK.snd).mul
      (hG.comp hg (fun z hz ↦ hgp z hz))).log
      (fun z hz ↦ (mul_pos (mul_pos two_pos (hb z.1 hz.1)) (Gamma_pos_of_pos (hgp z hz))).ne')
  have hL : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ logLik w m (k z.1) z.2) S := by
    unfold logLik
    apply continuousOn_finsetSum Finset.univ
    intro i _
    exact continuousOn_const.mul (hnorm.neg.sub (hp i))
  have hD : ContinuousOn (fun z : P × (Fin n → ℝ) ↦ sampleDeriv w m (k z.1) z.2) S := by
    unfold sampleDeriv score
    apply continuousOn_finsetSum Finset.univ
    intro i _
    exact (continuousOn_const.mul ((hK.fst.mul (hp i)).neg.div (hr i)
      (fun z hz ↦ hn z hz i))).smul continuousOn_const
  have hd (p : P) (x : Fin n → ℝ) (hx : (p, x) ∈ S) :
      HasFDerivAt (logLik w m (k p)) (sampleDeriv w m (k p) x) x := by
    unfold logLik sampleDeriv
    apply HasFDerivAt.fun_sum
    intro i _
    let f : (Fin n → ℝ) →L[ℝ] ℝ := proj i
    have ht : x i ≠ m := mem_iInter.mp hx.2 i
    have hd := (score_at (hb p hx.1) ht).const_mul (w i : ℝ)
    have h := hd.comp_hasFDerivAt x (f.hasFDerivAt (x := x))
    exact h
  filter_upwards [UniqueAE.ae_winners_derivative (μ := volume) hS hL hD hd]
    with x hx hxm p q hp hq hpa hqa
  have hxX : x ∈ X := mem_iInter.mpr hxm
  exact hx p q ⟨⟨hpa, hxX⟩, fun r hr ↦ hp hr.1⟩ ⟨⟨hqa, hxX⟩, fun r hr ↦ hq hr.1⟩

private theorem separates (w : Fin n → ℕ) (hw : ∀ i, 0 < w i)
    {m : ℝ} {p q : ℝ × ℝ} (hp : p ∈ Ioi 0 ×ˢ Ioi 0) (hq : q ∈ Ioi 0 ×ˢ Ioi 0)
    {x : Fin n → ℝ} (hx : ∀ i, x i ≠ m)
    {i j : Fin n} (hij : |x i - m| ≠ |x j - m|)
    (he : sampleDeriv w m p x = sampleDeriv w m q x) : p = q := by
  have eval (r : ℝ × ℝ) (l : Fin n) :
      sampleDeriv w m r x (Pi.single l 1) = (w l : ℝ) * score m r (x l) := by
    simp only [sampleDeriv, sum_apply, smul_apply, proj_apply, smul_eq_mul]
    rw [Finset.sum_eq_single l]
    · simp
    · intro a _ ha
      simp [Pi.single_eq_of_ne ha]
    · simp
  have eq (l : Fin n) := congrArg (fun d : (Fin n → ℝ) →L[ℝ] ℝ ↦ d (Pi.single l 1)) he
  have hlog (l : Fin n) :
      log p.1 + (log |x l - m| - log p.2) * p.1 =
      log q.1 + (log |x l - m| - log q.2) * q.1 := by
    have hl := eq l
    rw [eval, eval] at hl
    have hw' : (w l : ℝ) ≠ 0 := by exact_mod_cast (hw l).ne'
    have hs := mul_left_cancel₀ hw' hl
    have hr := sub_ne_zero.mpr (hx l)
    have ha := abs_pos.mpr hr
    simp only [score, neg_div] at hs
    have hpow := (div_left_inj₀ hr).mp (neg_inj.mp hs)
    have hh := congrArg Real.log hpow
    rw [log_mul hp.1.ne' (rpow_pos_of_pos (div_pos ha hp.2) _).ne',
      log_mul hq.1.ne' (rpow_pos_of_pos (div_pos ha hq.2) _).ne',
      log_rpow (div_pos ha hp.2), log_rpow (div_pos ha hq.2),
      log_div ha.ne' hp.2.ne', log_div ha.ne' hq.2.ne'] at hh
    linear_combination hh
  have hne : log |x i - m| - log |x j - m| ≠ 0 :=
    sub_ne_zero.mpr fun hh ↦ hij (strictMonoOn_log.injOn (abs_pos.mpr (sub_ne_zero.mpr (hx i)))
      (abs_pos.mpr (sub_ne_zero.mpr (hx j))) hh)
  have hh : (log |x i - m| - log |x j - m|) * (p.1 - q.1) = 0 := by
    linear_combination hlog i - hlog j
  have hshape := sub_eq_zero.mp ((mul_eq_zero.mp hh).resolve_left hne)
  have hs := hlog i
  rw [← hshape] at hs
  have hhscale : p.1 * (log p.2 - log q.2) = 0 := by linear_combination -hs
  have hscale := sub_eq_zero.mp ((mul_eq_zero.mp hhscale).resolve_left hp.1.ne')
  exact Prod.ext hshape (strictMonoOn_log.injOn hp.2 hq.2 hscale)

private theorem ae_nonzero_distinct_radii (m : ℝ) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume,
      (∀ i, x i ≠ m) ∧ ∃ i j, |x i - m| ≠ |x j - m| := by
  let i : Fin n := ⟨0, by omega⟩
  let j : Fin n := ⟨1, by omega⟩
  have hij : i ≠ j := by simp [i, j]
  have hnul (f : (Fin n → ℝ) →L[ℝ] ℝ) (hf : ∃ x, f x ≠ 0) (a : ℝ) :
      ∀ᵐ x : Fin n → ℝ ∂volume, f x ≠ a := by
    rw [ae_iff]
    simp only [not_not]
    obtain ⟨v, hv⟩ := hf
    let u := (a / f v) • v
    have hu : f u = a := by simp [u, div_mul_cancel₀ _ hv]
    have hk : LinearMap.ker f.toLinearMap ≠ ⊤ := by
      intro h
      exact hv (LinearMap.mem_ker.mp (show v ∈ LinearMap.ker f.toLinearMap by simp [h]))
    have hh : {x | f x = a} =
        (fun x : Fin n → ℝ ↦ x + -u) ⁻¹'
          (LinearMap.ker f.toLinearMap : Set (Fin n → ℝ)) := by
      ext x
      simp [LinearMap.mem_ker, hu, sub_eq_zero, ← sub_eq_add_neg]
    rw [hh, measure_preimage_add_right]
    exact Measure.addHaar_submodule volume _ hk
  have hall : ∀ᵐ x : Fin n → ℝ ∂volume, ∀ l, x l ≠ m := ae_all_iff.mpr fun l ↦
    hnul (proj l) ⟨Pi.single l 1, by simp⟩ m
  let f : (Fin n → ℝ) →L[ℝ] ℝ := (proj i : (Fin n → ℝ) →L[ℝ] ℝ) - proj j
  let g : (Fin n → ℝ) →L[ℝ] ℝ := (proj i : (Fin n → ℝ) →L[ℝ] ℝ) + proj j
  have hm := hnul f ⟨Pi.single i 1, by
    simp [f, _root_.sub_apply, proj_apply, Pi.single_eq_of_ne hij.symm]⟩ 0
  have hp := hnul g ⟨Pi.single i 1, by
    simp [g, _root_.add_apply, proj_apply, Pi.single_eq_of_ne hij.symm]⟩ (2 * m)
  filter_upwards [hall, hm, hp] with x hx hm hp
  refine ⟨hx, i, j, fun h ↦ ?_⟩
  rcases abs_eq_abs.mp h with h | h
  · apply hm
    simp only [f, _root_.sub_apply, proj_apply]
    linarith
  · apply hp
    simp only [g, _root_.add_apply, proj_apply]
    linarith

private theorem uniqueIn {P : Type*} [TopologicalSpace P] [LocallyCompactSpace P]
    [SecondCountableTopology P] (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m : ℝ)
    (hn : 2 ≤ n) (k : P → ℝ × ℝ) (A : Set P) (hA : IsOpen A)
    (hk : ContinuousOn k A) (hki : Set.InjOn k A)
    (ha : ∀ p ∈ A, 0 < (k p).1) (hb : ∀ p ∈ A, 0 < (k p).2) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : P, p ∈ A → q ∈ A →
      IsMaxOn (fun r ↦ logLik w m (k r) x) A p →
      IsMaxOn (fun r ↦ logLik w m (k r) x) A q → p = q := by
  filter_upwards [ae_nonzero_distinct_radii m hn, chart w m k A hA hk ha hb]
    with x ⟨hx, i, j, hij⟩ he p q hp hq hmp hmq
  exact hki hp hq (separates w hw ⟨ha p hp, hb p hp⟩ ⟨ha q hq, hb q hq⟩
    hx hij (he hx p q hmp hmq hp hq))

/-- With the mean held, an attained finite GED fit is almost everywhere unique
on each positive-count stratum with at least two levels. -/
theorem held_mean_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m : ℝ)
    (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ × ℝ,
      p ∈ Ici 1 ×ˢ Ioi 0 → q ∈ Ici 1 ×ˢ Ioi 0 →
      IsMaxOn (fun r ↦ logLik w m r x) (Ici 1 ×ˢ Ioi 0) p →
      IsMaxOn (fun r ↦ logLik w m r x) (Ici 1 ×ˢ Ioi 0) q → p = q := by
  let P := Ici (1 : ℝ)
  let _ : LocallyCompactSpace P := isClosed_Ici.locallyCompactSpace
  let k : P × ℝ → ℝ × ℝ := fun p ↦ (p.1.val, p.2)
  let A : Set (P × ℝ) := univ ×ˢ Ioi 0
  have hk : Continuous k := by unfold k; fun_prop
  have hi : Set.InjOn k A := fun p q _ _ h ↦
    Prod.ext (Subtype.ext (congrArg Prod.fst h)) (congrArg (fun r : ℝ × ℝ ↦ r.2) h)
  have h := uniqueIn w hw m hn k A (isOpen_univ.prod isOpen_Ioi) hk.continuousOn hi
    (fun p _ ↦ zero_lt_one.trans_le p.1.property) (fun _ hp ↦ hp.2)
  filter_upwards [h] with x hx p q hp hq hmp hmq
  let p' : P × ℝ := (⟨p.1, hp.1⟩, p.2)
  let q' : P × ℝ := (⟨q.1, hq.1⟩, q.2)
  have pull (r : P × ℝ) (hr : r ∈ A) : k r ∈ Ici 1 ×ˢ Ioi 0 :=
    ⟨r.1.property, hr.2⟩
  exact congrArg k (hx p' q' ⟨mem_univ _, hp.2⟩ ⟨mem_univ _, hq.2⟩
    (fun r hr ↦ hmp (pull r hr)) (fun r hr ↦ hmq (pull r hr)))

/-- GED likelihood at held mean and standard deviation. -/
def heldSdLogLik (w : Fin n → ℕ) (m sd a : ℝ) (x : Fin n → ℝ) : ℝ :=
  logLik w m (a, sd / Ged.sdFactor a) x

/-- With mean and standard deviation held, an attained finite GED fit is
almost everywhere unique on each positive-count stratum with at least two levels. -/
theorem held_mean_sd_ae_unique (w : Fin n → ℕ) (hw : ∀ i, 0 < w i) (m sd : ℝ)
    (hsd : 0 < sd) (hn : 2 ≤ n) :
    ∀ᵐ x : Fin n → ℝ ∂volume, ∀ p q : ℝ, p ∈ Ici 1 → q ∈ Ici 1 →
      IsMaxOn (fun a ↦ heldSdLogLik w m sd a x) (Ici 1) p →
      IsMaxOn (fun a ↦ heldSdLogLik w m sd a x) (Ici 1) q → p = q := by
  let P := Ici (1 : ℝ)
  let _ : LocallyCompactSpace P := isClosed_Ici.locallyCompactSpace
  let k : P → ℝ × ℝ := fun p ↦ (p.val, sd / Ged.sdFactor p.val)
  have ha (p : P) : 0 < p.val := zero_lt_one.trans_le p.property
  have hf (p : P) : 0 < Ged.sdFactor p.val := by
    unfold Ged.sdFactor
    exact sqrt_pos_of_pos (div_pos (Gamma_pos_of_pos (div_pos (by norm_num) (ha p)))
      (Gamma_pos_of_pos (one_div_pos.mpr (ha p))))
  have hg (a : ℝ) (ha : 0 < a) : Continuous (fun p : P ↦ Gamma (a / p.val)) :=
    differentiableOn_Gamma_Ioi.continuousOn.comp_continuous
      (continuous_const.div continuous_subtype_val (fun p ↦ (zero_lt_one.trans_le p.property).ne'))
      (fun p ↦ div_pos ha (zero_lt_one.trans_le p.property))
  have hfactor : Continuous (fun p : P ↦ Ged.sdFactor p.val) := by
    unfold Ged.sdFactor
    exact continuous_sqrt.comp ((hg 3 (by norm_num)).div (hg 1 zero_lt_one)
      (fun p ↦ (Gamma_pos_of_pos (one_div_pos.mpr (ha p))).ne'))
  have hk : Continuous k := continuous_subtype_val.prodMk
    (continuous_const.div hfactor (fun p ↦ (hf p).ne'))
  have hi : InjOn k univ := fun _ _ _ _ h ↦ Subtype.ext (congrArg Prod.fst h)
  have h := uniqueIn w hw m hn k univ isOpen_univ hk.continuousOn hi
    (fun p _ ↦ ha p) (fun p _ ↦ div_pos hsd (hf p))
  filter_upwards [h] with x hx p q hp hq hmp hmq
  exact congrArg Subtype.val (hx ⟨p, hp⟩ ⟨q, hq⟩ (mem_univ _) (mem_univ _)
    (fun r _ ↦ hmp r.property) (fun r _ ↦ hmq r.property))

end
end UnivariateML.AE1GED

end
