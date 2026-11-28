module

public import UnivariateML.Family.TLCollapse
public import UnivariateML.Generic.Escape
public import Mathlib.Topology.Order.MonotoneContinuity

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Tukey lambda with shape and location held

The scale criterion and the escape classification join the negative,
logistic and positive branches. Positive fits use the R7a conventions.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TLHeldLocation

open LocScaleTukeyLambda TLHeldScale TLCollapse

private theorem q_mono {l : ℝ} (hl : l ≤ 0) :
    StrictMonoOn (Q l) (Ioo 0 1) := by
  intro u hu v hv huv
  have hq : 0 < 1-v := sub_pos.mpr hv.2
  have hp : 0 < 1-u := sub_pos.mpr hu.2
  have h' : 1-v < 1-u := sub_lt_sub_left huv 1
  by_cases h : l = 0
  · simp only [Q,h,ite_true]
    linarith [log_lt_log hu.1 huv,log_lt_log hq h']
  · have hn : l < 0 := lt_of_le_of_ne hl h
    simp only [Q,h,ite_false,quantile]
    rw [← neg_div_neg_eq (u^l-(1-u)^l) l,← neg_div_neg_eq (v^l-(1-v)^l) l]
    apply (div_lt_div_iff_of_pos_right (neg_pos.mpr hn)).mpr
    linarith [rpow_lt_rpow_of_neg hu.1 huv hn,rpow_lt_rpow_of_neg hq h' hn]

private noncomputable def pIso {l : ℝ} (hl : l ≤ 0) : Ioo (0 : ℝ) 1 ≃o ℝ :=
  StrictMono.orderIsoOfSurjective (fun p : Ioo (0 : ℝ) 1 ↦ Q l p) (q_mono hl).strictMono
    (by intro z; obtain ⟨p,hp,hq⟩ := nonpositive_quantile_surjective hl z; exact ⟨⟨p,hp⟩,hq⟩)

private noncomputable def prob {l : ℝ} (hl : l ≤ 0) (z : ℝ) : ℝ := (pIso hl).symm z

private theorem prob_mem {l : ℝ} (hl : l ≤ 0) (z : ℝ) : prob hl z ∈ Ioo 0 1 :=
  ((pIso hl).symm z).2

private theorem q_prob {l : ℝ} (hl : l ≤ 0) (z : ℝ) : Q l (prob hl z) = z :=
  (pIso hl).apply_symm_apply z

private theorem prob_cont {l : ℝ} (hl : l ≤ 0) : Continuous (prob hl) :=
  continuous_subtype_val.comp (pIso hl).symm.continuous

private theorem w_pos_bound {l p : ℝ} (hl : l ≤ 0) (hp : p ∈ Ioo 0 1) :
    0 < weight l p ∧ weight l p ≤ 1 := by
  have h : l < 1 := hl.trans_lt zero_lt_one
  rw [weight,ite_eq_left h]
  have hq := sub_pos.mpr hp.2
  let A := p^(1-l)
  let B := (1-p)^(1-l)
  have hA : 0 < A := rpow_pos_of_pos hp.1 _
  have hB : 0 < B := rpow_pos_of_pos hq _
  have hAle : A ≤ 1 := rpow_le_one hp.1.le hp.2.le (sub_nonneg.mpr h.le)
  change 0 < A*B/(A+B) ∧ A*B/(A+B) ≤ 1
  exact ⟨div_pos (mul_pos hA hB) (add_pos hA hB),
    (div_le_iff₀ (add_pos hA hB)).mpr (by nlinarith [mul_le_mul_of_nonneg_right hAle hB.le])⟩

private theorem w_cont {l p : ℝ} (hl : l ≤ 0) (hp : p ∈ Ioo 0 1) :
    ContinuousAt (weight l) p := by
  have h : l < 1 := hl.trans_lt zero_lt_one
  have ha : Continuous (fun p : ℝ ↦ p^(1-l)) := continuous_rpow_const (sub_nonneg.mpr h.le)
  have hb : Continuous (fun p : ℝ ↦ (1-p)^(1-l)) := ha.comp (continuous_const.sub continuous_id)
  change ContinuousAt (fun p ↦ weight l p) p
  simp only [weight,h,ite_true]
  exact (ha.mul hb).continuousAt.div (ha.add hb).continuousAt
    (add_pos (rpow_pos_of_pos hp.1 _) (rpow_pos_of_pos (sub_pos.mpr hp.2) _)).ne'

private noncomputable def profile {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {l : ℝ} (hl : l ≤ 0)
    (s : ℝ) : ℝ := graphLikelihood l s (m,fun j ↦ prob hl ((x j-m)/s))

private theorem profile_graph {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {l : ℝ}
    (hl : l ≤ 0) {s : ℝ} (hs : 0 < s) :
    (m,fun j ↦ prob hl ((x j-m)/s)) ∈ graph x l s := by
  rw [graph,ite_eq_left hl]
  intro j
  refine ⟨prob_mem hl _,?_⟩
  change m+s*Q l (prob hl ((x j-m)/s))=x j
  rw [q_prob,mul_comm s,div_mul_cancel₀ _ hs.ne']
  ring

private theorem profile_eq {n : ℕ} {x : Fin n → ℝ} {m l s : ℝ} (hl : l ≤ 0)
    (hs : 0 < s) {p : Fin n → ℝ} (hp : (m,p) ∈ graph x l s) :
    graphLikelihood l s (m,p) = profile x m hl s := by
  rw [graph,ite_eq_left hl] at hp
  unfold profile
  congr 2
  funext j
  have he : Q l (p j) = (x j-m)/s := by
    apply (eq_div_iff hs.ne').mpr
    have h := (hp j).2
    change m+s*Q l (p j)=x j at h
    linarith only [h,show Q l (p j)*s=s*Q l (p j) by ring]
  exact (q_mono hl).injOn (hp j).1 (prob_mem hl _) (he.trans (q_prob hl _).symm)

private theorem profile_pos {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {l : ℝ}
    (hl : l ≤ 0) {s : ℝ} (hs : 0 < s) : 0 < profile x m hl s := by
  unfold profile graphLikelihood
  exact Finset.prod_pos fun j _ ↦ div_pos (w_pos_bound hl (prob_mem hl ((x j-m)/s))).1 hs

private theorem profile_cont {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {l : ℝ} (hl : l ≤ 0) :
    ContinuousOn (profile x m hl) (Ioi 0) := by
  unfold profile graphLikelihood
  apply continuousOn_finsetProd
  intro j _
  have hprob : ContinuousOn (fun s : ℝ ↦ prob hl ((x j-m)/s)) (Ioi 0) :=
    (prob_cont hl).comp_continuousOn
      (continuousOn_const.div continuousOn_id fun _ hs ↦ (show _ < _ from hs).ne')
  apply ContinuousOn.div _ continuousOn_id (fun _ hs ↦ (show _ < _ from hs).ne')
  intro s hs
  exact (w_cont hl (prob_mem hl ((x j-m)/s))).comp_continuousWithinAt
    (f := fun t : ℝ ↦ prob hl ((x j-m)/t)) (hprob s hs)

private theorem profile_large {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {l : ℝ} (hl : l ≤ 0) : Tendsto (profile x m hl) atTop (𝓝 0) := by
  have ht : Tendsto (fun s : ℝ ↦ s⁻¹ ^ n) atTop (𝓝 0) := by
    simpa only [zero_pow hn.ne'] using (tendsto_inv_atTop_zero (𝕜 := ℝ)).pow n
  refine tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht ?_ ?_
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with s hs
    exact (profile_pos x m hl hs).le
  · filter_upwards [eventually_gt_atTop (0 : ℝ)] with s hs
    unfold profile graphLikelihood
    calc
      _ ≤ ∏ _ : Fin n, s⁻¹ := by
        apply Finset.prod_le_prod₀
        · exact fun _ _ ↦ div_nonneg (w_pos_bound hl (prob_mem hl _)).1.le hs.le
        · intro j _
          simpa only [one_div] using div_le_div_of_nonneg_right
            (w_pos_bound hl (prob_mem hl ((x j-m)/s))).2 hs.le
      _ = _ := by simp

private theorem profile_negative_small {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a : ℝ}
    (ha : 0 < a) (he : 0 < ((n : ℝ)-multiplicity x m)/a - multiplicity x m) :
    Tendsto (profile x m (neg_nonpos.mpr ha.le)) (𝓝[>] 0) (𝓝 0) := by
  let hl := neg_nonpos.mpr ha.le
  let p (s : ℝ) (j : Fin n) := prob hl ((x j-m)/s)
  have ht := (fixed_location_negative_trace x m ha p (fun _ hs ↦ profile_graph x m hl hs)).2
  have he' : Tendsto (fun s : ℝ ↦ s^(((n : ℝ)-multiplicity x m)/a - multiplicity x m))
      (𝓝[>] 0) (𝓝 0) := by
    simpa only [zero_rpow he.ne'] using
      ((continuous_rpow_const he.le).continuousAt (x := (0 : ℝ))).tendsto.mono_left
        nhdsWithin_le_nhds
  have h := ht.mul he'
  simp only [mul_zero] at h
  apply h.congr'
  filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
  exact div_mul_cancel₀ _ (rpow_pos_of_pos (show 0 < s from hs) _).ne'

private theorem profile_max {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {l : ℝ} (hl : l ≤ 0) (hsmall : Tendsto (profile x m hl) (𝓝[>] 0) (𝓝 0)) :
    ∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ graph x l s ∧ 0 < graphLikelihood l s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ graph x l t →
        graphLikelihood l t (m,q) ≤ graphLikelihood l s (m,p) := by
  let D := Ioi (0 : ℝ)
  let L := profile x m hl
  let F (s : ℝ) := log (L s)
  have hF : ContinuousOn F D :=
    (profile_cont x m hl).log fun _ hs ↦ (profile_pos x m hl hs).ne'
  have hescape : Tendsto (fun s : D ↦ F s) (cocompact D) atBot := by
    refine tendsto_atBot.mpr fun b ↦ ?_
    have hb : 0 < exp b := exp_pos b
    obtain ⟨a,ha,hsmall'⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp (hsmall (Iio_mem_nhds hb))
    obtain ⟨z,hz⟩ := eventually_atTop.mp ((profile_large x m hn hl).eventually (Iio_mem_nhds hb))
    let K : Set D := Subtype.val ⁻¹' Icc a z
    have hK : IsCompact K := by
      apply Topology.IsEmbedding.subtypeVal.isCompact_iff.mpr
      have he : (Subtype.val : D → ℝ) '' K = Icc a z := by
        ext t
        constructor
        · rintro ⟨s,hs,rfl⟩
          exact hs
        · intro ht
          exact ⟨⟨t,ha.trans_le ht.1⟩,ht,rfl⟩
      exact he ▸ isCompact_Icc
    filter_upwards [hK.compl_mem_cocompact] with s hs
    have hh : (s : ℝ) < a ∨ z < (s : ℝ) := by
      simpa only [K,mem_compl_iff,mem_preimage,mem_Icc,not_and_or,not_le] using hs
    have hs₀ : 0 < (s : ℝ) := s.2
    have hLt : L s < exp b := hh.elim
      (fun hs' ↦ hsmall' ⟨hs₀,hs'⟩) (fun hs' ↦ hz _ hs'.le)
    exact (log_lt_iff_lt_exp (profile_pos x m hl hs₀)).mpr hLt |>.le
  obtain ⟨s,hs,hm⟩ := Generic.exists_isMaxOn_of_tendsto_cocompact
    (show D.Nonempty from ⟨1,zero_lt_one⟩) hF hescape
  have hs₀ : 0 < s := hs
  refine ⟨s,hs₀,fun j ↦ prob hl ((x j-m)/s),profile_graph x m hl hs₀,
    profile_pos x m hl hs₀,fun t ht q hq ↦ ?_⟩
  rw [profile_eq hl ht hq]
  change L t ≤ L s
  exact (log_le_log_iff (profile_pos x m hl ht) (profile_pos x m hl hs₀)).mp (hm ht)

/-- The held-location scale fit at a negative held shape exists exactly
when the collapse exponent is positive, including constant samples. -/
theorem negative_held_location_criterion {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {a : ℝ} (ha : 0 < a) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ graph x (-a) s ∧ 0 < graphLikelihood (-a) s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ graph x (-a) t →
        graphLikelihood (-a) t (m,q) ≤ graphLikelihood (-a) s (m,p)) ↔
    0 < ((n : ℝ)-multiplicity x m)/a - multiplicity x m := by
  constructor
  · rintro ⟨s,hs,p,hp,_,hm⟩
    by_contra! h
    rcases h.eq_or_lt with he | he
    · have H := critical_held_location_supremum x m hn ha he
      let D : Set (ℝ × (Fin n → ℝ)) := {q | 0 < q.1 ∧ (m,q.2) ∈ graph x (-a) q.1}
      let L (q : ℝ × (Fin n → ℝ)) := graphLikelihood (-a) q.1 (m,q.2)
      have hu : sSup (L '' D) ≤ L (s,p) := by
        apply csSup_le ⟨L (s,p),mem_image_of_mem _ (by exact ⟨hs,hp⟩)⟩
        rintro _ ⟨q,hq,rfl⟩
        exact hm q.1 hq.1 q.2 hq.2
      exact (H.2 (s,p) ⟨hs,hp⟩).not_ge (H.1 ▸ hu)
    · obtain ⟨t,ht,q,hq,hh⟩ := negative_exponent_unbounded x m ha he _ 1 zero_lt_one
      exact hh.not_ge (hm t ht.1 q hq)
  · intro he
    exact profile_max x m hn (neg_nonpos.mpr ha.le) (profile_negative_small x m ha he)

private theorem logistic_tail {p : ℝ} (hp : p ∈ Ioo 0 1) :
    weight 0 p ≤ exp (-|Q 0 p|) := by
  have hq := sub_pos.mpr hp.2
  have hW : weight 0 p = p*(1-p) := by
    rw [weight,ite_eq_left (show (0 : ℝ) < 1 from zero_lt_one),sub_zero,rpow_one,rpow_one,
      show p+(1-p)=1 by ring,div_one]
  rw [hW,Q,ite_eq_left rfl]
  by_cases h : 0 ≤ log p-log (1-p)
  · rw [abs_of_nonneg h,neg_sub,exp_sub,exp_log hq,exp_log hp.1]
    apply (le_div_iff₀ hp.1).mpr
    calc
      p*(1-p)*p = p^2*(1-p) := by ring
      _ ≤ 1-p := by
        simpa only [one_mul] using
          mul_le_mul_of_nonneg_right (pow_le_one₀ hp.1.le hp.2.le) hq.le
  · rw [abs_of_neg (lt_of_not_ge h),neg_neg,exp_sub,exp_log hp.1,exp_log hq]
    apply (le_div_iff₀ hq).mpr
    calc
      p*(1-p)*(1-p) = (1-p)^2*p := by ring
      _ ≤ p := by
        simpa only [one_mul] using
          mul_le_mul_of_nonneg_right (pow_le_one₀ hq.le (by linarith [hp.1])) hp.1.le

private theorem profile_logistic_small {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (i : Fin n)
    (hi : x i ≠ m) : Tendsto (profile x m (le_refl (0 : ℝ))) (𝓝[>] 0) (𝓝 0) := by
  let d := |x i-m|
  have hd : 0 < d := abs_pos.mpr (sub_ne_zero.mpr hi)
  have ht : Tendsto (fun s : ℝ ↦ s⁻¹ ^ n * exp (-d*s⁻¹)) (𝓝[>] 0) (𝓝 0) := by
    have h := (tendsto_rpow_mul_exp_neg_mul_atTop_nhds_zero n d hd).comp
      tendsto_inv_nhdsGT_zero
    refine h.congr fun s ↦ ?_
    change (s⁻¹)^(n : ℝ)*exp (-d*s⁻¹) = (s⁻¹)^n*exp (-d*s⁻¹)
    rw [rpow_natCast]
  apply tendsto_of_tendsto_of_tendsto_of_le_of_le' tendsto_const_nhds ht
  · filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    exact (profile_pos x m (le_refl (0 : ℝ)) hs).le
  · filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    have hs₀ : 0 < s := hs
    classical
    let p (j : Fin n) := prob (le_refl (0 : ℝ)) ((x j-m)/s)
    calc
      profile x m (le_refl (0 : ℝ)) s =
          s⁻¹ ^ n * (∏ j, weight 0 (p j)) := by
        change (∏ j, weight 0 (p j)/s) = _
        simp only [div_eq_mul_inv,Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,
          Fintype.card_fin,mul_comm]
      _ ≤ s⁻¹ ^ n * (∏ j, if j=i then exp (-d*s⁻¹) else 1) := by
        apply mul_le_mul_of_nonneg_left _ (by positivity)
        apply Finset.prod_le_prod₀ (fun j _ ↦ (w_pos_bound le_rfl (prob_mem le_rfl _)).1.le)
        intro j _
        by_cases h : j=i
        · subst j
          rw [ite_eq_left rfl]
          have hh := logistic_tail (prob_mem le_rfl ((x i-m)/s))
          rw [q_prob,abs_div,abs_of_pos hs₀] at hh
          simpa only [p,d,div_eq_mul_inv,neg_mul] using hh
        · rw [ite_eq_right h]
          exact (w_pos_bound le_rfl (prob_mem le_rfl _)).2
      _ = s⁻¹ ^ n * exp (-d*s⁻¹) := by simp

private theorem profile_logistic_constant {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    (h : ∀ j, x j = m) :
    Tendsto (profile x m (le_refl (0 : ℝ))) (𝓝[>] 0) atTop := by
  have h₁ : prob (le_refl (0 : ℝ)) 0 = 1/2 := by
    apply (q_mono le_rfl).injOn (prob_mem le_rfl _) (by norm_num)
    rw [q_prob,Q,ite_eq_left rfl,show (1 : ℝ)-1/2=1/2 by norm_num,sub_self]
  have hW : 0 < weight 0 (1/2) := (w_pos_bound le_rfl (by norm_num)).1
  have ht : Tendsto (fun s : ℝ ↦ weight 0 (1/2)/s) (𝓝[>] 0) atTop := by
    simpa only [div_eq_mul_inv] using tendsto_inv_nhdsGT_zero.const_mul_atTop hW
  have he (s : ℝ) : profile x m (le_refl (0 : ℝ)) s = (weight 0 (1/2)/s)^n := by
    simp only [profile,graphLikelihood,h,sub_self,zero_div,h₁,Finset.prod_const,
      Finset.card_univ,Fintype.card_fin]
  exact (ht.atTop_pow₀ hn.ne').congr (fun s ↦ (he s).symm)

/-- With shape and location held, a scale MLE exists exactly when a residual
is nonzero at nonnegative shape, or the collapse exponent is positive at negative shape. -/
theorem held_shape_location_criterion {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) (hn : 0 < n) :
    (∃ s > 0, ∃ p : Fin n → ℝ, (m,p) ∈ graph x l s ∧ 0 < graphLikelihood l s (m,p) ∧
      ∀ t > 0, ∀ q : Fin n → ℝ, (m,q) ∈ graph x l t →
        graphLikelihood l t (m,q) ≤ graphLikelihood l s (m,p)) ↔
    if l < 0 then 0 < ((n : ℝ)-multiplicity x m)/(-l) - multiplicity x m else
      ∃ i, x i ≠ m := by
  rcases lt_trichotomy l 0 with h | rfl | h
  · rw [ite_eq_left h]
    convert negative_held_location_criterion x m hn (neg_pos.mpr h) using 1;
      simp only [neg_neg]
  · rw [ite_eq_right (lt_irrefl _)]
    constructor
    · rintro ⟨s,hs,p,hp,_,hm⟩
      by_contra! h
      have hpath := profile_logistic_constant x m hn h
      obtain ⟨t,ht,hgt⟩ := ((eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))).and
        (hpath.eventually_gt_atTop (graphLikelihood 0 s (m,p)))).exists
      exact hgt.not_ge (hm t ht _ (profile_graph x m le_rfl ht))
    · rintro ⟨i,hi⟩
      exact profile_max x m hn le_rfl (profile_logistic_small x m i hi)
  · simp only [ite_eq_right (not_lt.mpr h.le)]
    simpa only [graph,ite_eq_right h.not_ge] using
      held_positive_shape_location_criterion hn x h m

private theorem scale_large_all {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) (hn : 0 < n) :
    ∀ c > 0, ∃ R : ℝ, ∀ s > R, ∀ p : Fin n → ℝ, (m,p) ∈ graph x l s →
      graphLikelihood l s (m,p) < c := by
  intro c hc
  let P : Set (Fin n → ℝ) := Set.pi univ fun _ ↦ Icc (0 : ℝ) 1
  let V (p : Fin n → ℝ) := ∏ j, weight l (p j)
  have hP : IsCompact P := isCompact_univ_pi fun _ ↦ isCompact_Icc
  have hV : ContinuousOn V P := by
    unfold V
    apply continuousOn_finsetProd
    intro j _
    exact (weight_continuousOn l).comp
      (show ContinuousOn (fun p : Fin n → ℝ ↦ p j) P from (continuous_apply j).continuousOn)
      (fun p hp ↦ hp j (mem_univ j))
  obtain ⟨C,hC⟩ := (hP.image_of_continuousOn hV).bddAbove
  have ht : Tendsto (fun s : ℝ ↦ C*(s⁻¹)^n) atTop (𝓝 0) := by
    simpa only [zero_pow hn.ne',mul_zero] using
      ((tendsto_inv_atTop_zero (𝕜 := ℝ)).pow n).const_mul C
  obtain ⟨R,hR⟩ := eventually_atTop.mp ((ht.eventually (Iio_mem_nhds hc)).and
    (eventually_gt_atTop (0 : ℝ)))
  refine ⟨R,fun s hs p hp ↦ ?_⟩
  have h := hR s hs.le
  have hp' : p ∈ P := by
    intro j _
    by_cases hl : l ≤ 0
    · rw [graph,ite_eq_left hl] at hp
      exact Ioo_subset_Icc_self (hp j).1
    · rw [graph,ite_eq_right hl] at hp
      exact (hp j).1
  have hb : V p ≤ C := hC (mem_image_of_mem V hp')
  calc
    graphLikelihood l s (m,p) = V p*(s⁻¹)^n := by
      simp only [graphLikelihood,V,div_eq_mul_inv,Finset.prod_mul_distrib,Finset.prod_const,
        Finset.card_univ,Fintype.card_fin]
    _ ≤ C*(s⁻¹)^n := mul_le_mul_of_nonneg_right hb (pow_nonneg (inv_nonneg.mpr (show 0 < s from h.2).le) _)
    _ < c := h.1

/-- Zero envelope at vanishing positive scales in the held-location graph. -/
def Vanishes {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) : Prop :=
  ∀ c > 0, ∃ δ > (0 : ℝ), ∀ s ∈ Ioo (0 : ℝ) δ, ∀ p : Fin n → ℝ,
    (m,p) ∈ graph x l s → graphLikelihood l s (m,p) < c

/-- Infinite envelope at vanishing positive scales in the held-location graph. -/
def Diverges {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) : Prop :=
  ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
    (m,p) ∈ graph x l s ∧ B < graphLikelihood l s (m,p)

private theorem profile_vanishes {n : ℕ} {x : Fin n → ℝ} {m l : ℝ} (hl : l ≤ 0)
    (h : Tendsto (profile x m hl) (𝓝[>] 0) (𝓝 0)) : Vanishes x m l := by
  intro c hc
  obtain ⟨δ,hδ,hlt⟩ := mem_nhdsGT_iff_exists_Ioo_subset.mp (h (Iio_mem_nhds hc))
  exact ⟨δ,hδ,fun s hs p hp ↦ (profile_eq hl hs.1 hp) ▸ hlt hs⟩

private theorem positive_vanishes {n : ℕ} {x : Fin n → ℝ} {m l : ℝ} (hl : 0 < l)
    (i : Fin n) (hi : x i ≠ m) : Vanishes x m l := by
  intro c hc
  refine ⟨l*|x i-m|,mul_pos hl (abs_pos.mpr (sub_ne_zero.mpr hi)),fun s hs p hp ↦ ?_⟩
  rw [graph,ite_eq_right hl.not_ge] at hp
  have hq := (hp i).1
  have h₀ := rpow_le_one hq.1 hq.2 hl.le
  have h₁ := rpow_le_one (sub_nonneg.mpr hq.2) (by linarith [hq.1]) hl.le
  have h₂ := rpow_nonneg hq.1 l
  have h₃ := rpow_nonneg (sub_nonneg.mpr hq.2) l
  have hq' : |(p i)^l-(1-p i)^l| ≤ 1 := abs_le.mpr ⟨by linarith,by linarith⟩
  have he := (hp i).2
  change m+s*(((p i)^l-(1-p i)^l)/l) = x i at he
  have h : l*(x i-m) = s*((p i)^l-(1-p i)^l) := by
    field_simp [hl.ne'] at he
    linarith
  have h' := congrArg abs h
  rw [abs_mul,abs_mul,abs_of_pos hl,abs_of_pos hs.1] at h'
  have hn := mul_le_mul_of_nonneg_left hq' hs.1.le
  exact (show s < l*|x i-m| from hs.2).not_ge (by linarith) |>.elim

private theorem nonnegative_constant_diverges {n : ℕ} {x : Fin n → ℝ} {m l : ℝ}
    (hn : 0 < n) (hl : 0 ≤ l) (h : ∀ j, x j = m) : Diverges x m l := by
  intro B δ hδ
  have hnear : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioo 0 δ := Ioo_mem_nhdsGT hδ
  rcases hl.eq_or_lt with he | he
  · subst l
    have ht := profile_logistic_constant x m hn h
    obtain ⟨s,hs,hB⟩ := (hnear.and (ht.eventually_gt_atTop B)).exists
    exact ⟨s,hs,_,profile_graph x m le_rfl hs.1,hB⟩
  · obtain ⟨hvalid,ht⟩ := constant_positive_shape_collapse hn m l he
    obtain ⟨s,hs,hB⟩ := (hnear.and (ht.eventually_gt_atTop B)).exists
    have hx : x = fun _ ↦ m := funext h
    exact ⟨s,hs,_,by rw [hx,graph,ite_eq_right he.not_ge]; exact hvalid s hs.1,hB⟩

/-- The scale escape classification at held shape and location. Infinite
scale always vanishes. At zero scale the nonnegative branches give zero or
infinity; the negative branches are classified by the collapse exponent. -/
theorem held_shape_location_failure {n : ℕ} (x : Fin n → ℝ) (m l : ℝ) (hn : 0 < n) :
    (∀ c > 0, ∃ R : ℝ, ∀ s > R, ∀ p : Fin n → ℝ, (m,p) ∈ graph x l s →
      graphLikelihood l s (m,p) < c) ∧
    (if l < 0 then
      let e : ℝ := ((n : ℝ)-multiplicity x m)/(-l) - multiplicity x m
      (0 < e → Vanishes x m l) ∧ (e < 0 → Diverges x m l) ∧
      (e = 0 →
        let B := ∏ j, if x j = m then weight l (1/2) else
          1/((-l)*|x j-m|)^(((-l)+1)/(-l))
        let D : Set (ℝ × (Fin n → ℝ)) := {q | 0 < q.1 ∧ (m,q.2) ∈ graph x l q.1}
        let L : ℝ × (Fin n → ℝ) → ℝ := fun q ↦ graphLikelihood l q.1 (m,q.2)
        sSup (L '' D) = B ∧ (∀ q ∈ D, L q < B) ∧
          ∃ p : ℝ → Fin n → ℝ, (∀ s > 0, (m,p s) ∈ graph x l s) ∧
            Tendsto (fun s ↦ graphLikelihood l s (m,p s)) (𝓝[>] 0) (𝓝 B))
    else
      ((∃ i, x i ≠ m) → Vanishes x m l) ∧ ((∀ i, x i = m) → Diverges x m l)) := by
  refine ⟨scale_large_all x m l hn,?_⟩
  by_cases hl : l < 0
  · rw [ite_eq_left hl]
    let a := -l
    have ha : 0 < a := neg_pos.mpr hl
    have hl' : -a = l := neg_neg l
    refine ⟨?_,?_,?_⟩
    · intro he
      simpa only [hl'] using profile_vanishes (neg_nonpos.mpr ha.le)
        (profile_negative_small x m ha he)
    · intro he
      simpa only [Diverges,hl'] using negative_exponent_unbounded x m ha he
    · intro he
      have H := critical_held_location_supremum x m hn ha he
      refine ⟨by simpa only [hl'] using H.1,by simpa only [hl'] using H.2,?_⟩
      let hp (s : ℝ) (j : Fin n) := prob (neg_nonpos.mpr ha.le) ((x j-m)/s)
      have hh (s : ℝ) (hs : 0 < s) : (m,hp s) ∈ graph x (-a) s :=
        profile_graph x m (neg_nonpos.mpr ha.le) hs
      refine ⟨hp,by simpa only [hl'] using hh,?_⟩
      have he' : ((n : ℝ)-multiplicity x m)/a - multiplicity x m = 0 := he
      simpa only [hl',he',rpow_zero,div_one] using
        (fixed_location_negative_trace x m ha hp hh).2
  · rw [ite_eq_right hl]
    have h := le_of_not_gt hl
    refine ⟨?_,nonnegative_constant_diverges hn h⟩
    rintro ⟨i,hi⟩
    rcases h.eq_or_lt with rfl | h
    · exact profile_vanishes le_rfl (profile_logistic_small x m i hi)
    · exact positive_vanishes h i hi

end UnivariateML.TLHeldLocation
end
