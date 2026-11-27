module

public import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
public import Mathlib.Topology.Order.Compact
public import Mathlib.Topology.Order.IntermediateValue

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Tukey lambda at positive held shape

## Deviations

The positive-shape likelihood is represented on the quantile graph. The
graph includes closed support contacts, with zero weight below shape one.
The nonpositive shape cases and the varying-shape comparisons require
separate tail and compactness proofs.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.LocScaleTukeyLambda

/-- Standardized quantile at positive shape. -/
noncomputable def quantile (lam p : ℝ) : ℝ := (p^lam - (1-p)^lam)/lam

/-- Reciprocal quantile derivative with the R7a endpoint convention.
For shape below one the expression is regularized at zero and one. -/
noncomputable def weight (lam p : ℝ) : ℝ :=
  if lam < 1 then
    p^(1-lam) * (1-p)^(1-lam) / (p^(1-lam) + (1-p)^(1-lam))
  else if lam = 1 then 1/2 else
    1 / (p^(lam-1) + (1-p)^(lam-1))

private theorem quantile_continuous {lam : ℝ} (hlam : 0 < lam) : Continuous (quantile lam) := by
  unfold quantile
  exact ((continuous_rpow_const hlam.le).sub
    ((continuous_rpow_const hlam.le).comp (continuous_const.sub continuous_id))).div_const _

private theorem quantile_strictMonoOn {lam : ℝ} (hlam : 0 < lam) :
    StrictMonoOn (quantile lam) (Icc 0 1) := by
  intro a ha b hb hab
  unfold quantile
  apply (div_lt_div_iff_of_pos_right hlam).mpr
  have h₁ := strictMonoOn_rpow_Ici_of_exponent_pos hlam ha.1 hb.1 hab
  have h₂ := strictMonoOn_rpow_Ici_of_exponent_pos hlam
    (sub_nonneg.mpr hb.2) (sub_nonneg.mpr ha.2) (sub_lt_sub_left hab 1)
  linarith

private theorem quantile_zero {lam : ℝ} (hlam : 0 < lam) : quantile lam 0 = -1/lam := by
  simp [quantile, zero_rpow hlam.ne']

private theorem quantile_one {lam : ℝ} (hlam : 0 < lam) : quantile lam 1 = 1/lam := by
  simp [quantile, zero_rpow hlam.ne']

private theorem quantile_bounds {lam p : ℝ} (hlam : 0 < lam) (hp : p ∈ Icc 0 1) :
    quantile lam p ∈ Icc (-1/lam) (1/lam) := by
  have hm := (quantile_strictMonoOn hlam).monotoneOn
  exact ⟨(quantile_zero hlam) ▸ hm (by norm_num) hp hp.1,
    (quantile_one hlam) ▸ hm hp (by norm_num) hp.2⟩

private theorem quantile_interior_bounds {lam p : ℝ} (hlam : 0 < lam) (hp : p ∈ Ioo 0 1) :
    quantile lam p ∈ Ioo (-1/lam) (1/lam) := by
  have hm := quantile_strictMonoOn hlam
  exact ⟨(quantile_zero hlam) ▸ hm (by norm_num) (Ioo_subset_Icc_self hp) hp.1,
    (quantile_one hlam) ▸ hm (Ioo_subset_Icc_self hp) (by norm_num) hp.2⟩

private theorem quantile_covers {lam z : ℝ} (hlam : 0 < lam) (hz : z ∈ Icc (-1/lam) (1/lam)) :
    ∃ p ∈ Icc (0 : ℝ) 1, quantile lam p = z := by
  have he := intermediate_value_Icc (by norm_num : (0 : ℝ) ≤ 1)
    (quantile_continuous hlam).continuousOn
  rw [quantile_zero hlam, quantile_one hlam] at he
  exact he hz

private theorem rpow_sum_pos {p r : ℝ} (hp : p ∈ Icc 0 1) :
    0 < p^r + (1-p)^r := by
  have hq : 0 ≤ 1-p := sub_nonneg.mpr hp.2
  rcases hp.1.eq_or_lt with h | h
  · rw [← h]
    have h := zero_rpow_nonneg r
    simpa using add_pos_of_nonneg_of_pos h (show (0 : ℝ) < 1 by norm_num)
  · exact add_pos_of_pos_of_nonneg (rpow_pos_of_pos h _) (rpow_nonneg hq _)

private theorem weight_continuousOn (lam : ℝ) :
    ContinuousOn (weight lam) (Icc (0 : ℝ) 1) := by
  unfold weight
  by_cases h : lam < 1
  · simp only [h,ite_true]
    have hp : Continuous (fun p : ℝ ↦ p^(1-lam)) := continuous_rpow_const (by linarith)
    have hq : Continuous (fun p : ℝ ↦ (1-p)^(1-lam)) :=
      hp.comp (continuous_const.sub continuous_id)
    exact (hp.mul hq).continuousOn.div (hp.add hq).continuousOn
      (fun _ h ↦ (rpow_sum_pos h).ne')
  · by_cases h' : lam = 1
    · simp [h',continuousOn_const]
    · simp only [h,h',ite_false]
      have hp : Continuous (fun p : ℝ ↦ p^(lam-1)) := continuous_rpow_const (by linarith)
      have hq : Continuous (fun p : ℝ ↦ (1-p)^(lam-1)) :=
        hp.comp (continuous_const.sub continuous_id)
      exact continuousOn_const.div (hp.add hq).continuousOn (fun _ h ↦ (rpow_sum_pos h).ne')

private theorem weight_pos {lam p : ℝ} (hp : p ∈ Icc 0 1) :
    0 < weight lam p ↔ ¬ lam < 1 ∨ p ∈ Ioo 0 1 := by
  unfold weight
  by_cases h : lam < 1
  · simp only [h,ite_true,not_true_eq_false,false_or]
    constructor
    · intro hw
      have hp₀ : p ≠ 0 := by
        rintro rfl
        simp [zero_rpow (sub_ne_zero.mpr h.ne') ] at hw
      have hp₁ : p ≠ 1 := by
        rintro rfl
        simp [zero_rpow (sub_ne_zero.mpr h.ne') ] at hw
      exact ⟨hp.1.lt_of_ne hp₀.symm,hp.2.lt_of_ne hp₁⟩
    · intro hq
      exact div_pos (mul_pos (rpow_pos_of_pos hq.1 _) (rpow_pos_of_pos (sub_pos.mpr hq.2) _))
        (rpow_sum_pos hp)
  · simp only [h,ite_false,not_false_eq_true,true_or,iff_true]
    split_ifs
    · norm_num
    · exact one_div_pos.mpr (rpow_sum_pos hp)

variable {n : ℕ}

/-- The held-scale feasible quantile graph. The first component is the
location and the other components are the observation quantiles. -/
def heldScaleGraph (x : Fin n → ℝ) (lam s : ℝ) : Set (ℝ × (Fin n → ℝ)) :=
  {q | ∀ j, q.2 j ∈ Icc 0 1 ∧ q.1 + s * quantile lam (q.2 j) = x j}

/-- Product likelihood of a positive-shape graph point. -/
noncomputable def graphLikelihood (lam s : ℝ) (q : ℝ × (Fin n → ℝ)) : ℝ :=
  ∏ j, weight lam (q.2 j)/s

private theorem graph_unique_quantiles {x : Fin n → ℝ} {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) {p q : ℝ × (Fin n → ℝ)}
    (hp : p ∈ heldScaleGraph x lam s) (hq : q ∈ heldScaleGraph x lam s) (he : p.1 = q.1) :
    p = q := by
  refine Prod.ext he (funext fun j ↦ ?_)
  apply (quantile_strictMonoOn hlam).injOn (hp j).1 (hq j).1
  have h₁ := (hp j).2
  have h₂ := (hq j).2
  rw [he] at h₁
  exact mul_left_cancel₀ hs.ne' (by linarith)

private theorem heldScaleGraph_compact {x : Fin n → ℝ} {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) (i : Fin n) : IsCompact (heldScaleGraph x lam s) := by
  let Q : Set (Fin n → ℝ) := Set.pi univ (fun _ ↦ Icc (0 : ℝ) 1)
  let B := Icc (x i-s/lam) (x i+s/lam) ×ˢ Q
  let A : Set (ℝ × (Fin n → ℝ)) :=
    ⋂ j, {q | q.1 + s * quantile lam (q.2 j) = x j}
  have hc : IsCompact B := isCompact_Icc.prod (isCompact_univ_pi fun _ ↦ isCompact_Icc)
  have ha : IsClosed A := isClosed_iInter fun j ↦ isClosed_eq
    (continuous_fst.add ((quantile_continuous hlam).comp
      ((continuous_apply j).comp continuous_snd) |>.const_mul s))
    continuous_const
  have he : heldScaleGraph x lam s = B ∩ A := by
    ext p
    constructor
    · intro hp
      have h := quantile_bounds hlam (hp i).1
      have h₁ := mul_le_mul_of_nonneg_left h.1 hs.le
      have h₂ := mul_le_mul_of_nonneg_left h.2 hs.le
      have h₃ := (hp i).2
      refine ⟨⟨⟨?_,?_⟩,fun j _ ↦ (hp j).1⟩, mem_iInter.mpr (fun j ↦ (hp j).2)⟩
      · linarith only [h₂,h₃, show s*(1/lam) = s/lam by ring]
      · linarith only [h₁,h₃, show s*(-1/lam) = -(s/lam) by ring]
    · rintro ⟨hp,hA⟩ j
      exact ⟨hp.2 j (mem_univ j), mem_iInter.mp hA j⟩
  rw [he]
  exact hc.inter_right ha

/-- With positive held shape and scale, positive-likelihood attainment
requires strict support compatibility below shape one and closed support
compatibility at and above shape one. -/
theorem held_positive_shape_scale_criterion (x : Fin n → ℝ) {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) (i j : Fin n)
    (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ q ∈ heldScaleGraph x lam s,
      0 < graphLikelihood lam s q ∧ IsMaxOn (graphLikelihood lam s) (heldScaleGraph x lam s) q) ↔
      if lam < 1 then x j - x i < 2*(s/lam) else x j - x i ≤ 2*(s/lam) := by
  let L : ℝ × (Fin n → ℝ) → ℝ := graphLikelihood lam s
  let S := heldScaleGraph x lam s
  have hc : ContinuousOn L S := by
    unfold L graphLikelihood
    apply continuousOn_finsetProd Finset.univ
    intro k _
    apply ContinuousOn.div_const
    exact (weight_continuousOn lam).comp ((continuous_apply k).comp continuous_snd).continuousOn
      (fun q hq ↦ (hq k).1)
  have hfit (h : ∃ q ∈ S, 0 < L q) :
      ∃ q ∈ S, 0 < L q ∧ IsMaxOn L S q := by
    obtain ⟨p,hp,hpL⟩ := h
    obtain ⟨q,hq,hm⟩ := (heldScaleGraph_compact hlam hs i).exists_isMaxOn ⟨p,hp⟩ hc
    exact ⟨q,hq,hpL.trans_le (hm hp),hm⟩
  constructor
  · rintro ⟨q,hq,hqL,_⟩
    have hbound (k : Fin n) := quantile_bounds hlam (hq k).1
    have hx : x j-x i = s*(quantile lam (q.2 j) - quantile lam (q.2 i)) := by
      rw [← (hq j).2, ← (hq i).2]
      ring
    by_cases h : lam < 1
    · rw [ite_eq_left h]
      have hp (k : Fin n) : q.2 k ∈ Ioo 0 1 := by
        have hn : weight lam (q.2 k) ≠ 0 := by
          intro hz
          exact hqL.ne' (Finset.prod_eq_zero (Finset.mem_univ k) (by simp [hz]))
        have hw : 0 < weight lam (q.2 k) := by
          apply lt_of_le_of_ne _ hn.symm
          unfold weight
          simp only [h,ite_true]
          exact div_nonneg (mul_nonneg (rpow_nonneg (hq k).1.1 _)
            (rpow_nonneg (sub_nonneg.mpr (hq k).1.2) _)) (rpow_sum_pos (hq k).1).le
        exact ((weight_pos (hq k).1).mp hw).resolve_left (not_not_intro h)
      have h₁ := mul_lt_mul_of_pos_left (quantile_interior_bounds hlam (hp j)).2 hs
      have h₂ := mul_lt_mul_of_pos_left (quantile_interior_bounds hlam (hp i)).1 hs
      linarith only [hx,h₁,h₂,show s*(1/lam) = s/lam by ring,show s*(-1/lam) = -(s/lam) by ring]
    · rw [ite_eq_right h]
      have h₁ := mul_le_mul_of_nonneg_left (hbound j).2 hs.le
      have h₂ := mul_le_mul_of_nonneg_left (hbound i).1 hs.le
      linarith only [hx,h₁,h₂,show s*(1/lam) = s/lam by ring,show s*(-1/lam) = -(s/lam) by ring]
  · intro hspan
    let m := (x i+x j)/2
    have e₁ : (-1/lam)*s = -(s/lam) := by ring
    have e₂ : (1/lam)*s = s/lam := by ring
    have hbound (k : Fin n) : (x k-m)/s ∈ Icc (-1/lam) (1/lam) := by
      have h' : x j - x i ≤ 2*(s/lam) := by split_ifs at hspan with h <;> linarith
      exact ⟨(le_div_iff₀ hs).mpr (by dsimp [m]; linarith [hi k,e₁]),
        (div_le_iff₀ hs).mpr (by dsimp [m]; linarith [hj k,e₂])⟩
    choose p hp he using fun k ↦ quantile_covers hlam (hbound k)
    have hq : (m,p) ∈ S := by
      intro k
      refine ⟨hp k, ?_⟩
      change m + s * quantile lam (p k) = x k
      rw [he k, mul_comm s, div_mul_cancel₀ _ hs.ne']
      ring
    apply hfit
    refine ⟨(m,p),hq,Finset.prod_pos (fun k _ ↦ div_pos ((weight_pos (hp k)).mpr ?_) hs)⟩
    by_cases h : lam < 1
    · right
      rw [ite_eq_left h] at hspan
      have hin : (x k-m)/s ∈ Ioo (-1/lam) (1/lam) := by
        exact ⟨(lt_div_iff₀ hs).mpr (by dsimp [m]; linarith [hi k,e₁]),
          (div_lt_iff₀ hs).mpr (by dsimp [m]; linarith [hj k,e₂])⟩
      constructor
      · apply (hp k).1.lt_of_ne
        intro hz
        rw [← he k, ← hz, quantile_zero hlam] at hin
        exact (lt_irrefl _ hin.1)
      · apply (hp k).2.lt_of_ne
        intro hz
        rw [← he k,hz,quantile_one hlam] at hin
        exact (lt_irrefl _ hin.2)
    · exact Or.inl h

/-- At positive shape, an admissible held-scale location has exactly
one quantile vector precisely when every residual lies in the closed support. -/
theorem held_positive_shape_scale_location_graph (x : Fin n → ℝ) {lam s : ℝ}
    (hlam : 0 < lam) (hs : 0 < s) (m : ℝ) :
    (∃! p : Fin n → ℝ, (m,p) ∈ heldScaleGraph x lam s) ↔
      ∀ j, |x j-m| ≤ s/lam := by
  constructor
  · rintro ⟨p,hp,_⟩ j
    have h := quantile_bounds hlam (hp j).1
    have h₁ := mul_le_mul_of_nonneg_left h.1 hs.le
    have h₂ := mul_le_mul_of_nonneg_left h.2 hs.le
    have hb := (hp j).2
    exact abs_le.mpr ⟨by linarith only [h₁,hb,show s*(-1/lam) = -(s/lam) by ring],
      by linarith only [h₂,hb,show s*(1/lam) = s/lam by ring]⟩
  · intro h
    have hb (j : Fin n) : (x j-m)/s ∈ Icc (-1/lam) (1/lam) := by
      obtain ⟨h₁,h₂⟩ := abs_le.mp (h j)
      exact ⟨(le_div_iff₀ hs).mpr ((show (-1/lam)*s = -(s/lam) by ring).trans_le h₁),
        (div_le_iff₀ hs).mpr (h₂.trans_eq (show (1/lam)*s = s/lam by ring).symm)⟩
    choose p hp he using fun j ↦ quantile_covers hlam (hb j)
    have hp' : (m,p) ∈ heldScaleGraph x lam s := by
      intro j
      refine ⟨hp j,?_⟩
      change m+s*quantile lam (p j) = x j
      rw [he j, mul_comm s, div_mul_cancel₀ _ hs.ne']
      ring
    exact ⟨p,hp',fun q hq ↦ congrArg Prod.snd (graph_unique_quantiles hlam hs hq hp' rfl)⟩

private theorem quantile_half (lam : ℝ) : quantile lam (1/2) = 0 := by
  unfold quantile
  rw [show (1 : ℝ)-1/2 = 1/2 by norm_num,sub_self,zero_div]

/-- Constant data at the held location collapse at every held positive
shape along a vanishing scale. -/
theorem constant_positive_shape_collapse {n : ℕ} (hn : 0 < n) (m lam : ℝ) (_hlam : 0 < lam) :
    let p : ℝ × (Fin n → ℝ) := (m,fun _ ↦ 1/2)
    (∀ s > 0, p ∈ heldScaleGraph (fun _ ↦ m) lam s) ∧
      Tendsto (fun s ↦ graphLikelihood lam s p) (𝓝[>] 0) atTop := by
  let p : ℝ × (Fin n → ℝ) := (m,fun _ ↦ 1/2)
  have hW : 0 < weight lam (1/2) :=
    (weight_pos (by norm_num : (1/2 : ℝ) ∈ Icc 0 1)).mpr
      (Or.inr (by norm_num : (1/2 : ℝ) ∈ Ioo 0 1))
  refine ⟨fun s _ j ↦ ?_,?_⟩
  · refine ⟨by norm_num [p], ?_⟩
    change m + s * quantile lam (1/2) = m
    rw [quantile_half, mul_zero, add_zero]
  · have ht : Tendsto (fun s : ℝ ↦ weight lam (1/2)/s) (𝓝[>] 0) atTop := by
      simpa only [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop hW
    simpa only [graphLikelihood,p,Finset.prod_const,Finset.card_univ,Fintype.card_fin] using
      ht.atTop_pow₀ hn.ne'

/-- At positive held shape and held location, a positive-likelihood
scale maximizer exists exactly when some sample residual is nonzero. -/
theorem held_positive_shape_location_criterion {n : ℕ} (hn : 0 < n) (x : Fin n → ℝ)
    {lam : ℝ} (hlam : 0 < lam) (m : ℝ) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ heldScaleGraph x lam s ∧
      0 < graphLikelihood lam s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ heldScaleGraph x lam t →
        graphLikelihood lam t (m,q) ≤ graphLikelihood lam s (m,p)) ↔
    ∃ i, x i ≠ m := by
  classical
  constructor
  · rintro ⟨s,_,p,_,_,hm⟩
    by_contra! h
    have he : x = fun _ ↦ m := funext h
    obtain ⟨hvalid,hpath⟩ := constant_positive_shape_collapse hn m lam hlam
    rw [he] at hm
    obtain ⟨t,ht,hgt⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
      (hpath.eventually_gt_atTop (graphLikelihood lam s (m,p)))).exists
    exact hgt.not_ge (hm t ht _ (hvalid t ht))
  · rintro ⟨i,hi⟩
    let B := 1/(lam*|x i-m|)
    let C := Icc 0 B ×ˢ (Set.pi univ fun _ : Fin n ↦ Icc (0 : ℝ) 1)
    let K := C ∩ ⋂ j : Fin n, {p : ℝ × (Fin n → ℝ) |
      quantile lam (p.2 j) = p.1*(x j-m)}
    let V (p : ℝ × (Fin n → ℝ)) := ∏ j, p.1 * weight lam (p.2 j)
    have hK : IsCompact K := by
      apply (isCompact_Icc.prod (isCompact_univ_pi fun _ ↦ isCompact_Icc)).inter_right
      exact isClosed_iInter fun j ↦ isClosed_eq
        ((quantile_continuous hlam).comp ((continuous_apply _).comp continuous_snd)) (by fun_prop)
    have hc : ContinuousOn V K := by
      unfold V
      apply continuousOn_finsetProd Finset.univ
      intro j _
      exact continuous_fst.continuousOn.mul
        ((weight_continuousOn lam).comp ((continuous_apply j).comp continuous_snd).continuousOn
          (fun p hp ↦ hp.1.2 j (mem_univ j)))
    have hfrom (s : ℝ) (hs : 0 < s) {p : Fin n → ℝ} (hp : (m,p) ∈ heldScaleGraph x lam s) :
        (s⁻¹,p) ∈ K := by
      have hα (j : Fin n) : quantile lam (p j) = s⁻¹*(x j-m) := by
        have hh := (hp j).2
        change m + s*quantile lam (p j) = x j at hh
        rw [show x j-m = s*quantile lam (p j) by linarith,← mul_assoc,inv_mul_cancel₀ hs.ne',one_mul]
      have hb := quantile_bounds hlam (hp i).1
      have hh : |quantile lam (p i)| ≤ 1/lam := abs_le.mpr (by simpa only [neg_div] using And.intro hb.1 hb.2)
      rw [hα i,abs_mul,abs_of_pos (inv_pos.mpr hs)] at hh
      have he := (le_div_iff₀ hlam).mp hh
      refine ⟨⟨⟨(inv_pos.mpr hs).le,(le_div_iff₀ (mul_pos hlam
        (abs_pos.mpr (sub_ne_zero.mpr hi)))).mpr ?_⟩,fun j _ ↦ (hp j).1⟩,
        mem_iInter.mpr hα⟩
      nlinarith only [he]
    have hto {a : ℝ} (ha : 0 < a) {p : Fin n → ℝ} (hp : (a,p) ∈ K) :
        (m,p) ∈ heldScaleGraph x lam a⁻¹ := by
      intro j
      refine ⟨hp.1.2 j (mem_univ _),?_⟩
      have he : quantile lam (p j) = a*(x j-m) := mem_iInter.mp hp.2 j
      change m + a⁻¹ * quantile lam (p j) = x j
      rw [he, ← mul_assoc,inv_mul_cancel₀ ha.ne',one_mul]
      ring
    have hva (a : ℝ) (p : Fin n → ℝ) :
        V (a,p) = graphLikelihood lam a⁻¹ (m,p) := by
      simp only [V,graphLikelihood,div_inv_eq_mul,mul_comm]
    let s := lam * (∑ j, |x j-m|) + 1
    have hs : 0 < s := by dsimp [s]; positivity
    have hsmall (j : Fin n) : |(x j-m)/s| < 1/lam := by
      rw [abs_div,abs_of_pos hs,div_lt_div_iff₀ hs hlam]
      have hh := Finset.single_le_sum (f := fun k ↦ |x k-m|) (fun k _ ↦ abs_nonneg _)
        (Finset.mem_univ j)
      dsimp [s]
      nlinarith only [mul_le_mul_of_nonneg_left hh hlam.le]
    have hz (j : Fin n) : (x j-m)/s ∈ Icc (-1/lam) (1/lam) := by
      exact ⟨by simpa only [neg_div] using (abs_le.mp (hsmall j).le).1,
        (abs_le.mp (hsmall j).le).2⟩
    choose p hp he using fun j ↦ quantile_covers hlam (hz j)
    have hfit : (m,p) ∈ heldScaleGraph x lam s := by
      intro j
      refine ⟨hp j,?_⟩
      change m + s*quantile lam (p j) = x j
      rw [he j,mul_comm,div_mul_cancel₀ _ hs.ne']
      ring
    have hV : 0 < V (s⁻¹,p) := by
      apply Finset.prod_pos
      intro j _
      apply mul_pos (inv_pos.mpr hs)
      apply (weight_pos (hp j)).mpr
      right
      have hh := abs_lt.mp (hsmall j)
      rw [← he j] at hh
      refine ⟨(hp j).1.lt_of_ne ?_,(hp j).2.lt_of_ne ?_⟩
      · intro h
        rw [← h,quantile_zero hlam] at hh
        exact (lt_irrefl _ (by simpa only [neg_div] using hh.1))
      · intro h
        rw [h,quantile_one hlam] at hh
        exact (lt_irrefl _ hh.2)
    have hmem := hfrom s hs hfit
    obtain ⟨r,hr,hm⟩ := hK.exists_isMaxOn ⟨(s⁻¹,p),hmem⟩ hc
    have hrV := hV.trans_le (hm hmem)
    have hr0 : 0 < r.1 := hr.1.1.1.lt_of_ne (fun h ↦ by
      have hzero : V r = 0 := Finset.prod_eq_zero (Finset.mem_univ (⟨0,hn⟩ : Fin n))
        (by change r.1 * _ = 0; rw [← h,zero_mul])
      exact hrV.ne' hzero)
    refine ⟨r.1⁻¹,inv_pos.mpr hr0,r.2,hto hr0 hr,?_,fun t ht q hq ↦ ?_⟩
    · exact (hva r.1 r.2) ▸ hrV
    · have h := hm (hfrom t ht hq)
      change V (t⁻¹,q) ≤ V (r.1,r.2) at h
      simpa only [hva,inv_inv] using h

end UnivariateML.LocScaleTukeyLambda

end
