module

public import UnivariateML.Family.JoinsPIG

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Poisson–inverse Gaussian held masks

At held mean the likelihood is divided by its positive Poisson value.
The compact shape coordinate takes values in `(0,1]`, with zero shape
at one and the excluded Poisson face at zero.
-/

@[expose] public section

open Set Filter Topology Real
open scoped BigOperators Classical

namespace UnivariateML.Discrete4PIG

open Discrete3PIG

/-- The held-mean likelihood in the compact shape coordinate, normalized
by the Poisson likelihood at the held mean. -/
noncomputable def normalizedProfile {n : ℕ} (x : Fin n → ℕ) (μ w : ℝ) : ℝ :=
  if w = 1 then if ∀ i, x i = 0 then exp ((n : ℝ)*μ) else 0
  else exp (sampleGain x μ w)

/-- Positive shape expressed in the compact coordinate. The point at one
has shape zero. -/
noncomputable def shape (μ w : ℝ) : ℝ := μ^2*(1-w)^2/(2*w)

private theorem shape_strictAnti {μ : ℝ} (hμ : 0 < μ) :
    StrictAntiOn (shape μ) (Ioc 0 1) := by
  intro a ha b hb hab
  have ha' : 0 < 1-a := sub_pos.mpr (hab.trans_le hb.2)
  have hb' : 0 ≤ 1-b := sub_nonneg.mpr hb.2
  have hn : μ^2*(1-b)^2 < μ^2*(1-a)^2 := by
    exact mul_lt_mul_of_pos_left
      ((sq_lt_sq₀ hb' ha'.le).mpr (by linarith)) (sq_pos_of_pos hμ)
  have ha₀ := ha.1
  have hb₀ := hb.1
  unfold shape
  exact (div_lt_div_of_pos_right hn (by positivity : 0 < 2*b)).trans_le
    (div_le_div_of_nonneg_left (by positivity) (by positivity) (by linarith))

/-- The compact chart covers exactly all nonnegative physical shapes. -/
theorem shape_bijOn {μ : ℝ} (hμ : 0 < μ) : BijOn (shape μ) (Ioc 0 1) (Ici 0) := by
  refine ⟨fun w hw ↦ ?_,(shape_strictAnti hμ).injOn,?_⟩
  · change 0 ≤ shape μ w
    have := hw.1
    unfold shape
    positivity
  intro lam hlam
  change 0 ≤ lam at hlam
  rcases eq_or_lt_of_le hlam with h | h
  · subst lam
    exact ⟨1,⟨zero_lt_one,le_rfl⟩,by simp [shape]⟩
  · let t := lam/μ
    let z := sqrt (2*lam+t^2)
    let d := z-t
    have ht : 0 < t := div_pos h hμ
    have hz₀ : 0 ≤ z := sqrt_nonneg _
    have hz : z^2 = 2*lam+t^2 := sq_sqrt (by positivity)
    have htm : t*μ = lam := by dsimp [t]; field_simp
    have hd : 0 < d := by
      have : t < z := by
        dsimp [z]
        nth_rw 1 [← sqrt_sq ht.le]
        exact sqrt_lt_sqrt (sq_nonneg _) (by linarith)
      exact sub_pos.mpr this
    have hdμ : d < μ := by
      have hh : z < t+μ := by
        apply lt_of_not_ge
        intro H
        have hsq := mul_nonneg (sub_nonneg.mpr H) (show 0 ≤ z+(t+μ) by positivity)
        nlinarith [sq_pos_of_pos hμ]
      dsimp [d]
      linarith
    let w := 1-d/μ
    have hw : w ∈ Ioc 0 1 := ⟨sub_pos.mpr ((div_lt_one hμ).mpr hdμ),
      sub_le_self _ (div_nonneg hd.le hμ.le)⟩
    refine ⟨w,hw,?_⟩
    have he : d^2+2*t*d = 2*lam := by dsimp [d]; nlinarith only [hz]
    have hw' : 2*w ≠ 0 := mul_ne_zero two_ne_zero hw.1.ne'
    unfold shape
    apply (div_eq_iff hw').mpr
    have hD : μ*(1-w) = d := by dsimp [w]; field_simp [hμ.ne']; ring
    calc
      μ^2*(1-w)^2 = d^2 := by rw [← mul_pow,hD]
      _ = lam*(2*w) := by
        dsimp [w]
        field_simp [hμ.ne']
        linear_combination μ * he - 2*d * htm

private theorem inward_eq {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) (μ : ℝ) (hμ : 0 < μ) :
    let m := (∑ i, (x i : ℝ))/(n : ℝ)
    let v := (∑ i, ((x i : ℝ)-m)^2)/(n : ℝ)
    μ * inwardScore x μ = (n : ℝ) * (v+(m-μ)^2-m) := by
  dsimp
  let S := ∑ i, (x i : ℝ)
  let m := S/(n : ℝ)
  have hn' : (n : ℝ) ≠ 0 := Nat.cast_ne_zero.mpr hn.ne'
  have hS : (n : ℝ)*m = S := by unfold m; field_simp
  change μ * inwardScore x μ =
    (n : ℝ)*((∑ i, ((x i : ℝ)-m)^2)/(n : ℝ)+(m-μ)^2-m)
  unfold inwardScore
  have he (i : Fin n) : μ * (μ-2*(x i : ℝ)+(x i : ℝ)*((x i : ℝ)-1)/μ) =
      μ^2-2*μ*(x i : ℝ)+(x i : ℝ)^2-(x i : ℝ) := by
    field_simp [hμ.ne']
    ring
  have hv (i : Fin n) : ((x i : ℝ)-m)^2 =
      (x i : ℝ)^2-2*m*(x i : ℝ)+m^2 := by ring
  rw [Finset.mul_sum]
  simp only [he,hv,Finset.sum_add_distrib,Finset.sum_sub_distrib,
    ← Finset.mul_sum,Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
  change (n : ℝ)*μ^2 - 2*μ*S + (∑ i, (x i : ℝ)^2) - S = _
  rw [mul_sub, mul_add, mul_div_cancel₀ _ hn']
  linear_combination (1+2*μ-2*m) * hS

/-- On positive data, the held-mean fit exists exactly at positive
variance excess. Whenever it exists it is unique. Otherwise the
normalized Poisson height strictly exceeds all finite profiles. -/
theorem held_mean_positive {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∃ i, 0 < x i) {μ : ℝ} (hμ : 0 < μ) :
    let m := (∑ i, (x i : ℝ))/(n : ℝ)
    let v := (∑ i, ((x i : ℝ)-m)^2)/(n : ℝ)
    if m < v+(m-μ)^2 then
      ∃! w, w ∈ Ioc (0 : ℝ) 1 ∧ IsMaxOn (normalizedProfile x μ) (Ioc 0 1) w
    else
      IsLUB (normalizedProfile x μ '' Ioc 0 1) 1 ∧
        ∀ w ∈ Ioc (0 : ℝ) 1, normalizedProfile x μ w < 1 := by
  intro m v
  have hx' : ¬ ∀ i, x i = 0 := by obtain ⟨i,hi⟩ := hx; exact fun h ↦ hi.ne' (h i)
  have hsign : 0 < inwardScore x μ ↔ m < v+(m-μ)^2 := by
    have he := inward_eq hn x μ hμ
    change μ * inwardScore x μ = (n : ℝ)*(v+(m-μ)^2-m) at he
    calc
      0 < inwardScore x μ ↔ 0 < μ * inwardScore x μ := (mul_pos_iff_of_pos_left hμ).symm
      _ ↔ 0 < (n : ℝ) * (v+(m-μ)^2-m) := by rw [he]
      _ ↔ m < v+(m-μ)^2 := (mul_pos_iff_of_pos_left (Nat.cast_pos.mpr hn)).trans sub_pos
  have hf (w : ℝ) (hw : w ∈ Ioo 0 1) :
      normalizedProfile x μ w = exp (sampleGain x μ w) := by
    simp [normalizedProfile,hw.2.ne]
  have hend : normalizedProfile x μ 1 = 0 := by simp [normalizedProfile,hx']
  have H := held_mean_profile hμ hx
  by_cases h : m < v+(m-μ)^2
  · have hs : 0 < inwardScore x μ := hsign.mpr h
    rw [ite_eq_left hs] at H
    rw [ite_eq_left h]
    obtain ⟨w,⟨hw,hM⟩,hU⟩ := H
    have hm : IsMaxOn (normalizedProfile x μ) (Ioc 0 1) w := by
      intro q hq
      change normalizedProfile x μ q ≤ normalizedProfile x μ w
      rw [hf w hw]
      rcases eq_or_lt_of_le hq.2 with he | he
      · rw [he,hend]
        exact (exp_pos _).le
      · rw [hf q ⟨hq.1,he⟩]
        exact exp_le_exp.mpr (hM ⟨hq.1,he⟩)
    refine ⟨w,⟨⟨hw.1,hw.2.le⟩,hm⟩,?_⟩
    rintro q ⟨hq,hQ⟩
    have hq' : q < 1 := by
      apply lt_of_le_of_ne hq.2
      intro he
      have hh := hQ (show w ∈ Ioc 0 1 from ⟨hw.1,hw.2.le⟩)
      change normalizedProfile x μ w ≤ normalizedProfile x μ q at hh
      rw [hf w hw,he,hend] at hh
      exact (exp_pos _).not_ge hh
    refine hU q ⟨⟨hq.1,hq'⟩,fun u hu ↦ ?_⟩
    have hh := hQ (show u ∈ Ioc 0 1 from ⟨hu.1,hu.2.le⟩)
    change normalizedProfile x μ u ≤ normalizedProfile x μ q at hh
    rw [hf u hu,hf q ⟨hq.1,hq'⟩] at hh
    exact exp_le_exp.mp hh
  · have h' : ¬ 0 < inwardScore x μ := fun hs ↦ h (hsign.mp hs)
    rw [ite_eq_right h'] at H
    rw [ite_eq_right h]
    have hstrict (q : ℝ) (hq : q ∈ Ioc 0 1) : normalizedProfile x μ q < 1 := by
      rcases eq_or_lt_of_le hq.2 with he | he
      · simp [he,hend]
      · rw [hf q ⟨hq.1,he⟩]
        exact exp_lt_one_iff.mpr (H.2 q ⟨hq.1,he⟩)
    refine ⟨⟨?_,?_⟩,hstrict⟩
    · rintro _ ⟨q,hq,rfl⟩
      exact (hstrict q hq).le
    · intro C hC
      have hpos : 0 < C := by
        have hhalf : (1/2 : ℝ) ∈ Ioo 0 1 := by norm_num
        have hh := hC (mem_image_of_mem (normalizedProfile x μ)
          (show (1/2 : ℝ) ∈ Ioc 0 1 from ⟨hhalf.1,hhalf.2.le⟩))
        rw [hf _ hhalf] at hh
        exact (exp_pos _).trans_le hh
      have he : 0 ≤ log C := H.1.2 (by
        rintro _ ⟨q,hq,rfl⟩
        have hh := hC (mem_image_of_mem (normalizedProfile x μ)
          (show q ∈ Ioc 0 1 from ⟨hq.1,hq.2.le⟩))
        rw [hf q hq, ← exp_log hpos] at hh
        exact exp_le_exp.mp hh)
      simpa only [exp_zero] using (le_log_iff_exp_le hpos).mp he

/-- With positive mean held on zero data, the shape-zero member is the
unique fit on the nonnegative shape domain. -/
theorem held_mean_all_zero {n : ℕ} (hn : 0 < n) {μ : ℝ} (hμ : 0 < μ)
    {lam : ℝ} (hlam : 0 ≤ lam) :
    IsMaxOn (HeldSweepPIG.zeroLikelihood n μ) (Ici 0) lam ↔ lam = 0 := by
  have H := HeldSweepPIG.all_zero_argmax hn
    (show (μ,lam) ∈ Ioi (0 : ℝ) ×ˢ Ici 0 from ⟨hμ,hlam⟩)
  constructor
  · intro h
    by_contra hne
    have hstrict : HeldSweepPIG.zeroLikelihood n μ lam < 1 := by
      have hl : 0 < lam := lt_of_le_of_ne hlam (Ne.symm hne)
      have G := HeldSweepPIG.held_shape_all_zero hn hl
      exact G.2.2 μ hμ
    have h₁ := h (show (0 : ℝ) ∈ Ici 0 from le_rfl)
    change HeldSweepPIG.zeroLikelihood n μ 0 ≤ HeldSweepPIG.zeroLikelihood n μ lam at h₁
    have h₀ : HeldSweepPIG.zeroLikelihood n μ 0 = 1 := by simp [HeldSweepPIG.zeroLikelihood]
    rw [h₀] at h₁
    exact hstrict.not_ge h₁
  · intro hz q hq
    exact (H.mpr hz) (show (μ,q) ∈ Ioi (0 : ℝ) ×ˢ Ici 0 from ⟨hμ,hq⟩)

/-- Positive held shape has a unique mean on positive data. All-zero data
approach the excluded zero-mean atom. -/
theorem held_shape {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ) {lam : ℝ} (hlam : 0 < lam) :
    let f := JoinsPIG.heldShapeLikelihood x lam
    ((∃ i, 0 < x i) → ∃! μ, μ ∈ Ioi (0 : ℝ) ∧ IsMaxOn f (Ioi 0) μ) ∧
      ((∀ i, x i = 0) → Tendsto f (𝓝[>] 0) (𝓝 1) ∧
        IsLUB (f '' Ioi 0) 1 ∧ ∀ μ > 0, f μ < 1) :=
  JoinsPIG.held_shape_classification hn x hlam

end UnivariateML.Discrete4PIG

end
