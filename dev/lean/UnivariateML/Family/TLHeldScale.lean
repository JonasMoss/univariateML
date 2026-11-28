module

public import UnivariateML.Family.LocScaleTukeyLambda
public import UnivariateML.Family.AE4TukeyLambda
public import UnivariateML.Generic.Compactification
public import Mathlib.Analysis.SpecialFunctions.Pow.Asymptotics
public import Mathlib.Topology.MetricSpace.ProperSpace.Real

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Tukey lambda with held scale

Positive superlevels in the probability graph are compact with shape fixed
or free on the fitted interval. The lifted free-shape graph tracks density
weights to retain simultaneous uniform support contacts.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TLHeldScale

open LocScaleTukeyLambda

/-- The quantile, including the logistic branch. -/
noncomputable def Q (l p : ℝ) : ℝ :=
  if l = 0 then log p - log (1 - p) else quantile l p

private theorem q_cont (l : ℝ) : ContinuousOn (Q l) (Ioo 0 1) := by
  change ContinuousOn (fun p ↦ Q l p) (Ioo 0 1)
  by_cases h : l = 0
  · simp only [Q, h, ite_true]
    exact ((continuousOn_id : ContinuousOn id (Ioo (0 : ℝ) 1)).log fun _ hp ↦ hp.1.ne').sub
      ((show ContinuousOn (fun p : ℝ ↦ 1-p) (Ioo 0 1) by fun_prop).log
        fun _ hp ↦ (sub_pos.mpr hp.2).ne')
  · simp only [Q, h, ite_false, quantile]
    exact (((continuousOn_id : ContinuousOn id (Ioo (0 : ℝ) 1)).rpow_const fun _ hp ↦ Or.inl hp.1.ne').sub
      ((show ContinuousOn (fun p : ℝ ↦ 1-p) (Ioo 0 1) by fun_prop).rpow_const
        fun _ hp ↦ Or.inl (sub_pos.mpr hp.2).ne')).div_const _

private theorem q_left {l : ℝ} (hl : l ≤ 0) : Tendsto (Q l) (𝓝[>] 0) atBot := by
  have hc : Tendsto (fun p : ℝ ↦ 1-p) (𝓝[>] 0) (𝓝 1) := by
    have h : ContinuousAt (fun p : ℝ ↦ 1-p) 0 := by fun_prop
    simpa using h.tendsto.mono_left
      (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))
  change Tendsto (fun p ↦ Q l p) (𝓝[>] 0) atBot
  by_cases h : l = 0
  · simp only [Q,h,ite_true]
    have ht := (continuousAt_log one_ne_zero).tendsto.comp hc
    refine tendsto_atBot.mpr fun b ↦ ?_
    filter_upwards [tendsto_log_nhdsGT_zero.eventually_le_atBot (b-1),
      ht.eventually (Ioi_mem_nhds (show -1 < log 1 by simp))] with p hp hp'
    change -1 < log (1-p) at hp'
    exact by linarith
  · simp only [Q,h,ite_false,quantile]
    rw [show (fun p : ℝ ↦ (p ^ l - (1-p)^l)/l) =
      fun p : ℝ ↦ ((1-p)^l - p^l)/(-l) by ext p; ring]
    have ht : Tendsto (fun p : ℝ ↦ (1-p)^l) (𝓝[>] 0) (𝓝 (1^l)) :=
      (continuousAt_rpow_const 1 l (Or.inl one_ne_zero)).tendsto.comp hc
    have hd : Tendsto (fun p : ℝ ↦ (1-p)^l - p^l) (𝓝[>] 0) atBot := by
      refine tendsto_atBot.mpr fun b ↦ ?_
      filter_upwards [ht.eventually (Iio_mem_nhds (show (1 : ℝ)^l < 2 by simp)),
        (tendsto_rpow_neg_nhdsGT_zero (lt_of_le_of_ne hl h)).eventually_ge_atTop (2-b)]
        with p hp hp'
      exact by linarith
    exact hd.atBot_div_const (neg_pos.mpr (lt_of_le_of_ne hl h))

private theorem q_reflect (l p : ℝ) : Q l (1-p) = - Q l p := by
  by_cases h : l = 0 <;> simp only [Q,h,ite_true,ite_false,quantile,show 1-(1-p)=p by ring] <;> ring

/-- Nonpositive shapes have a full real quantile range. -/
theorem nonpositive_quantile_surjective {l : ℝ} (hl : l ≤ 0) (z : ℝ) : ∃ p ∈ Ioo 0 1, Q l p = z := by
  let a := min z (-z)
  have hnear : ∀ᶠ p : ℝ in 𝓝[>] 0, p ∈ Ioo (0 : ℝ) (1/2) :=
    Ioo_mem_nhdsGT (by norm_num)
  obtain ⟨p, hp, hq⟩ := (hnear.and ((q_left hl).eventually_lt_atBot a)).exists
  have hsub : Icc p (1-p) ⊆ Ioo (0 : ℝ) 1 := fun t ht ↦
    ⟨hp.1.trans_le ht.1, ht.2.trans_lt (by linarith [hp.1])⟩
  have hz : z ∈ Icc (Q l p) (Q l (1-p)) := by
    rw [q_reflect]
    exact ⟨(hq.le.trans (min_le_left _ _)), by linarith [hq.le.trans (min_le_right z (-z))]⟩
  obtain ⟨t, ht, he⟩ := intermediate_value_Icc (by linarith [hp.2] : p ≤ 1-p)
    ((q_cont l).mono hsub) hz
  exact ⟨t,hsub ht,he⟩

/-- At each fixed shape the support weight is continuous on the closed probability interval. -/
theorem weight_continuousOn (l : ℝ) : ContinuousOn (weight l) (Icc 0 1) := by
  have powers (r : ℝ) (hr : 0 < r) :
      Continuous (fun p : ℝ ↦ p^r) ∧ Continuous (fun p : ℝ ↦ (1-p)^r) ∧
      ∀ p ∈ Icc (0 : ℝ) 1, p^r + (1-p)^r ≠ 0 := by
    refine ⟨continuous_rpow_const hr.le,
      (continuous_rpow_const hr.le).comp (continuous_const.sub continuous_id),?_⟩
    intro p h
    rcases h.2.eq_or_lt with he | he
    · simp [he, zero_rpow hr.ne']
    · exact (add_pos_of_nonneg_of_pos (rpow_nonneg h.1 _)
        (rpow_pos_of_pos (sub_pos.mpr he) _)).ne'
  change ContinuousOn (fun p ↦ weight l p) (Icc 0 1)
  by_cases h : l < 1
  · simp only [weight,h,ite_true]
    obtain ⟨hp,hq,hn⟩ := powers (1-l) (by linarith)
    exact (hp.mul hq).continuousOn.div (hp.add hq).continuousOn hn
  · by_cases he : l = 1
    · simp [weight,he,continuousOn_const]
    · simp only [weight,h,he,ite_false]
      obtain ⟨hp,hq,hn⟩ := powers (l-1) (by grind)
      exact continuousOn_const.div (hp.add hq).continuousOn hn

private theorem w_end {l : ℝ} (hl : l ≤ 0) :
    weight l 0 = 0 ∧ weight l 1 = 0 := by
  simp [weight, show l < 1 by linarith, zero_rpow (show 1-l ≠ 0 by linarith)]

private theorem w_pos {l p : ℝ} (hl : l ≤ 0) (hp : p ∈ Ioo 0 1) : 0 < weight l p := by
  have h₀ := rpow_pos_of_pos hp.1 (1-l)
  have h₁ := rpow_pos_of_pos (sub_pos.mpr hp.2) (1-l)
  simpa only [weight, show l < 1 by linarith, ite_true] using
    div_pos (mul_pos h₀ h₁) (add_pos h₀ h₁)

/-- Held-scale graph for all real held shapes. -/
noncomputable def graph {n : ℕ} (x : Fin n → ℝ) (l s : ℝ) : Set (ℝ × (Fin n → ℝ)) :=
  if l ≤ 0 then {q | ∀ j, q.2 j ∈ Ioo 0 1 ∧ q.1 + s * Q l (q.2 j) = x j}
  else heldScaleGraph x l s

private theorem graph_nonpos {n : ℕ} {x : Fin n → ℝ} {l s : ℝ} (hl : l ≤ 0)
    {q : ℝ × (Fin n → ℝ)} :
    q ∈ graph x l s ↔ ∀ j, q.2 j ∈ Ioo 0 1 ∧ q.1 + s * Q l (q.2 j) = x j := by
  simp [graph, hl]

private theorem graph_pos {n : ℕ} {x : Fin n → ℝ} {l s : ℝ} (hl : 0 < l) :
    graph x l s = heldScaleGraph x l s := by simp [graph, hl.not_ge]

private theorem graph_probs {n : ℕ} {x : Fin n → ℝ} {l s : ℝ} {q : ℝ × (Fin n → ℝ)}
    (hq : q ∈ graph x l s) (j : Fin n) : q.2 j ∈ Icc 0 1 := by
  by_cases h : l ≤ 0
  · exact Ioo_subset_Icc_self ((graph_nonpos h |>.mp hq) j).1
  · exact ((graph_pos (lt_of_not_ge h) ▸ hq) j).1

private theorem likelihood_cont {n : ℕ} (x : Fin n → ℝ) (l s : ℝ) :
    ContinuousOn (graphLikelihood l s) (graph x l s) := by
  unfold graphLikelihood
  apply continuousOn_finsetProd
  intro j _
  exact ((weight_continuousOn l).comp ((continuous_apply j).comp continuous_snd).continuousOn
    (fun _ hq ↦ graph_probs hq j)).div_const s

private theorem nonpositive_location_superlevel {n : ℕ} (i : Fin n) (x : Fin n → ℝ) {l s c : ℝ}
    (hl : l ≤ 0) (_hs : 0 < s) (hc : 0 < c) :
    IsCompact (graph x l s ∩ {q | c ≤ graphLikelihood l s q}) := by
  let P : Set (Fin n → ℝ) := Set.pi univ fun _ ↦ Icc (0 : ℝ) 1
  let V (p : Fin n → ℝ) : ℝ := ∏ j, weight l (p j) / s
  let K := P ∩ {p | c ≤ V p}
  have hv : ContinuousOn V P := by
    unfold V
    apply continuousOn_finsetProd
    intro j _
    exact ((weight_continuousOn l).comp ((continuous_apply j).continuousOn :
      ContinuousOn (fun p : Fin n → ℝ ↦ p j) P)
      (fun p hp ↦ hp j (mem_univ j))).div_const s
  have hP : IsCompact P := isCompact_univ_pi fun _ ↦ isCompact_Icc
  have hk : IsCompact K := hP.of_isClosed_subset
    (hv.preimage_isClosed_of_isClosed hP.isClosed (isClosed_Ici (a := c))) inter_subset_left
  have hin (p : Fin n → ℝ) (hp : p ∈ K) (j : Fin n) : p j ∈ Ioo 0 1 := by
    have h := hp.1 j (mem_univ j)
    have hne : weight l (p j) ≠ 0 := by
      intro he
      have hz : V p = 0 := Finset.prod_eq_zero (Finset.mem_univ j) (by simp [he])
      exact hc.not_ge (hz ▸ hp.2)
    exact ⟨h.1.lt_of_ne (fun e ↦ hne (by rw [← e,(w_end hl).1])),
      h.2.lt_of_ne (fun e ↦ hne (by rw [e,(w_end hl).2]))⟩
  let m (p : Fin n → ℝ) := x i - s * Q l (p i)
  have hq (j : Fin n) : ContinuousOn (fun p : Fin n → ℝ ↦ Q l (p j)) K :=
    (q_cont l).comp (continuous_apply j).continuousOn fun p hp ↦ hin p hp j
  have hm : ContinuousOn m K := continuousOn_const.sub ((hq i).const_mul s)
  let A := K ∩ ⋂ j, {p | m p + s * Q l (p j) = x j}
  have ha : IsCompact A := by
    apply hk.of_isClosed_subset _ inter_subset_left
    have : Nonempty (Fin n) := ⟨i⟩
    change IsClosed (K ∩ ⋂ j, {p | m p + s * Q l (p j) = x j})
    rw [inter_iInter]
    exact isClosed_iInter fun j ↦
      ((hm.add ((hq j).const_mul s))).preimage_isClosed_of_isClosed hk.isClosed
        (isClosed_singleton (x := x j))
  have he : graph x l s ∩ {q | c ≤ graphLikelihood l s q} =
      (fun p ↦ (m p,p)) '' A := by
    ext q
    constructor
    · rintro ⟨hG,hL⟩
      have h := graph_nonpos hl |>.mp hG
      have hh : m q.2 = q.1 := by dsimp [m]; linarith [(h i).2]
      refine ⟨q.2, ⟨⟨fun j _ ↦ Ioo_subset_Icc_self (h j).1,hL⟩,
        mem_iInter.mpr fun j ↦ ?_⟩,Prod.ext hh rfl⟩
      change m q.2 + s * Q l (q.2 j) = x j
      rw [hh]
      exact (h j).2
    · rintro ⟨p,hp,rfl⟩
      exact ⟨(graph_nonpos hl).mpr fun j ↦ ⟨hin p hp.1 j,mem_iInter.mp hp.2 j⟩,hp.1.2⟩
  rw [he]
  exact ha.image_of_continuousOn ((hm.prodMk continuousOn_id).mono inter_subset_left)

private theorem positive_location_superlevel {n : ℕ} (x : Fin n → ℝ) {l s c : ℝ}
    (hl : 0 < l) (hs : 0 < s) (i : Fin n) :
    IsCompact (graph x l s ∩ {q | c ≤ graphLikelihood l s q}) := by
  let B := Icc (x i-s/l) (x i+s/l) ×ˢ (Set.pi univ fun _ : Fin n ↦ Icc (0 : ℝ) 1)
  let A : Set (ℝ × (Fin n → ℝ)) :=
    ⋂ j, {q | q.1 + s * quantile l (q.2 j) = x j}
  have hb : IsCompact B := isCompact_Icc.prod (isCompact_univ_pi fun _ ↦ isCompact_Icc)
  have hq : Continuous (fun p ↦ quantile l p) := by
    unfold quantile
    exact ((continuous_rpow_const hl.le).sub
      ((continuous_rpow_const hl.le).comp (continuous_const.sub continuous_id))).div_const _
  have ha : IsClosed A := isClosed_iInter fun j ↦ isClosed_eq
    (continuous_fst.add ((hq.comp ((continuous_apply j).comp continuous_snd)).const_mul s))
    continuous_const
  have he : graph x l s = B ∩ A := by
    ext q
    rw [graph_pos hl]
    constructor
    · intro h
      have hh := (h i).2
      change q.1 + s * (((q.2 i)^l-(1-q.2 i)^l)/l) = x i at hh
      have h₀ := rpow_le_one (h i).1.1 (h i).1.2 hl.le
      have h₁ := rpow_le_one (sub_nonneg.mpr (h i).1.2)
        (by linarith [(h i).1.1]) hl.le
      have h₂ := rpow_nonneg (h i).1.1 l
      have h₃ := rpow_nonneg (sub_nonneg.mpr (h i).1.2) l
      have he : (x i-q.1)*l = s*((q.2 i)^l-(1-q.2 i)^l) := by
        field_simp [hl.ne'] at hh
        linarith
      have hfit : |x i-q.1| ≤ s/l := by
        apply abs_le.mpr
        constructor
        · rw [← neg_div]
          apply (div_le_iff₀ hl).mpr
          rw [he]
          nlinarith [mul_nonneg hs.le h₂, mul_nonneg hs.le (sub_nonneg.mpr h₁)]
        · apply (le_div_iff₀ hl).mpr
          rw [he]
          nlinarith [mul_nonneg hs.le h₃, mul_nonneg hs.le (sub_nonneg.mpr h₀)]
      refine ⟨⟨⟨?_,?_⟩,fun j _ ↦ (h j).1⟩,mem_iInter.mpr fun j ↦ (h j).2⟩
      · linarith [(abs_le.mp hfit).2]
      · linarith [(abs_le.mp hfit).1]
    · rintro ⟨hb,ha⟩ j
      exact ⟨hb.2 j (mem_univ j),mem_iInter.mp ha j⟩
  have hg : IsCompact (graph x l s) := he ▸ hb.inter_right ha
  exact hg.of_isClosed_subset
    ((likelihood_cont x l s).preimage_isClosed_of_isClosed hg.isClosed
      (isClosed_Ici (a := c))) inter_subset_left

/-- Every positive likelihood superlevel at fixed shape and scale is compact. -/
theorem compact_location_superlevel {n : ℕ} (i : Fin n) (x : Fin n → ℝ) {l s c : ℝ}
    (hs : 0 < s) (hc : 0 < c) :
    IsCompact (graph x l s ∩ {q | c ≤ graphLikelihood l s q}) := by
  by_cases h : l ≤ 0
  · exact nonpositive_location_superlevel i x h hs hc
  · exact positive_location_superlevel x (lt_of_not_ge h) hs i

private theorem maximize_from_witness {n : ℕ} (i : Fin n) {x : Fin n → ℝ} {l s : ℝ}
    (hs : 0 < s) {q : ℝ × (Fin n → ℝ)} (hq : q ∈ graph x l s)
    (hp : 0 < graphLikelihood l s q) :
    ∃ r ∈ graph x l s, 0 < graphLikelihood l s r ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) r := by
  let V := @graphLikelihood n l s
  let c := V q / 2
  let K := graph x l s ∩ {r | c ≤ V r}
  let D := graph x l s ∩ {r | c < V r}
  have hK : IsCompact K := compact_location_superlevel i x hs (half_pos hp)
  have hDK : D ⊆ K := by
    intro t ht
    exact ⟨ht.1, (show c < V t from ht.2).le⟩
  have hq' : q ∈ D := ⟨hq,half_lt_self hp⟩
  obtain ⟨r, hr, hm⟩ := Generic.exists_isMaxOn_of_boundary_le hDK hK
    ((likelihood_cont x l s).mono inter_subset_left) hq' (by
      intro t ht
      exact (le_of_not_gt fun ht' ↦ ht.2 ⟨ht.1.1,ht'⟩).trans (half_lt_self hp).le)
  refine ⟨r,hr.1,(half_pos hp).trans (show c < V r from hr.2),fun t ht ↦ ?_⟩
  by_cases h : t ∈ D
  · exact hm h
  · exact (le_of_not_gt (fun he ↦ h ⟨ht,he⟩)).trans (show c < V r from hr.2).le

private theorem nonpos_witness {n : ℕ} (x : Fin n → ℝ) {l s : ℝ}
    (hl : l ≤ 0) (hs : 0 < s) (m : ℝ) :
    ∃ p : Fin n → ℝ, (m,p) ∈ graph x l s ∧ 0 < graphLikelihood l s (m,p) := by
  choose p hp he using fun j ↦ nonpositive_quantile_surjective hl ((x j-m)/s)
  refine ⟨p, (graph_nonpos hl).mpr fun j ↦ ⟨hp j,?_⟩,
    Finset.prod_pos fun j _ ↦ div_pos (w_pos hl (hp j)) hs⟩
  change m + s * Q l (p j) = x j
  rw [he j, mul_comm s, div_mul_cancel₀ _ hs.ne']
  ring

/-- For a nonempty sample at held shape and scale, positive-likelihood attainment is
automatic at nonpositive shapes. At positive shapes it is exactly support compatibility,
strict below shape one and closed from shape one onward. -/
theorem held_shape_scale_criterion {n : ℕ} (x : Fin n → ℝ) {l s : ℝ}
    (hs : 0 < s) (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∃ q ∈ graph x l s, 0 < graphLikelihood l s q ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) q) ↔
    if l ≤ 0 then True else if l < 1 then x j-x i < 2*(s/l) else x j-x i ≤ 2*(s/l) := by
  by_cases h : l ≤ 0
  · rw [ite_eq_left h]
    exact iff_true_intro (by
      obtain ⟨p,hp,hv⟩ := nonpos_witness x h hs 0
      exact maximize_from_witness i hs hp hv)
  · rw [ite_eq_right h,graph_pos (lt_of_not_ge h)]
    exact held_positive_shape_scale_criterion x (lt_of_not_ge h) hs i j hi hj

/-- Infinite location has zero positive envelope at any held shape and scale.
The only failure of positive-likelihood attainment is support incompatibility. -/
theorem held_shape_scale_failure {n : ℕ} (x : Fin n → ℝ) {l s : ℝ}
    (hs : 0 < s) (i j : Fin n) (hi : ∀ k, x i ≤ x k) (hj : ∀ k, x k ≤ x j) :
    (∀ c > 0, ∃ M : ℝ, ∀ q ∈ graph x l s, M < |q.1| → graphLikelihood l s q < c) ∧
    ((¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) q) ↔
      (¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q)) ∧
    ((¬ ∃ q ∈ graph x l s, 0 < graphLikelihood l s q) ↔
      0 < l ∧ (if l < 1 then 2*(s/l) ≤ x j-x i else 2*(s/l) < x j-x i)) := by
  have he : (∃ q ∈ graph x l s, 0 < graphLikelihood l s q ∧
      IsMaxOn (graphLikelihood l s) (graph x l s) q) ↔
      (∃ q ∈ graph x l s, 0 < graphLikelihood l s q) :=
    ⟨fun ⟨q,hq,hp,_⟩ ↦ ⟨q,hq,hp⟩,
      fun ⟨_,hq,hp⟩ ↦ maximize_from_witness i hs hq hp⟩
  refine ⟨?_, not_congr he,?_⟩
  · intro c hc
    have hk := compact_location_superlevel i x (l := l) hs hc
    have ha := hk.image (continuous_abs.comp continuous_fst)
    obtain ⟨M,hM⟩ := ha.bddAbove
    exact ⟨M,fun q hq hMq ↦ lt_of_not_ge fun h ↦
      hMq.not_ge (hM (mem_image_of_mem _ ⟨hq,h⟩))⟩
  · rw [← he, held_shape_scale_criterion x hs i j hi hj]
    by_cases h : l ≤ 0
    · simp [h,not_lt.mpr h]
    · have hp : 0 < l := lt_of_not_ge h
      by_cases h' : l < 1 <;> simp [h,hp,h']
      constructor <;> intro hh <;> linarith

private theorem q_joint_interior (l p : ℝ) (hp : p ∈ Ioo 0 1) :
    ContinuousAt (fun t : ℝ × ℝ ↦ Q t.1 t.2) (l,p) :=
  (AE4TukeyLambda.quantileExtended_analytic l p hp).continuousAt

private theorem q_joint_positive (l p : ℝ) (hl : 0 < l) :
    ContinuousAt (fun t : ℝ × ℝ ↦ Q t.1 t.2) (l,p) := by
  have ht : ContinuousAt (fun t : ℝ × ℝ ↦ quantile t.1 t.2) (l,p) := by
    unfold quantile
    have hf : Tendsto Prod.fst (𝓝 (l,p)) (𝓝 l) := continuousAt_fst
    have hg : Tendsto Prod.snd (𝓝 (l,p)) (𝓝 p) := continuousAt_snd
    have hq : Tendsto (fun t : ℝ × ℝ ↦ 1-t.2) (𝓝 (l,p)) (𝓝 (1-p)) :=
      tendsto_const_nhds.sub hg
    exact ((hg.rpow hf (Or.inr hl)).sub (hq.rpow hf (Or.inr hl))).div hf hl.ne'
  apply ht.congr
  filter_upwards [continuousAt_fst.eventually (Ioi_mem_nhds hl)] with t ht
  exact (ite_eq_right (show 0 < t.1 from ht).ne').symm

private theorem w_joint (l p : ℝ) (hl : l < 1) (hp : p ∈ Icc 0 1) :
    ContinuousAt (fun t : ℝ × ℝ ↦ weight t.1 t.2) (l,p) := by
  have hf : Tendsto (fun t : ℝ × ℝ ↦ 1-t.1) (𝓝 (l,p)) (𝓝 (1-l)) :=
    tendsto_const_nhds.sub continuousAt_fst
  have hg : Tendsto Prod.snd (𝓝 (l,p)) (𝓝 p) := continuousAt_snd
  have hq : Tendsto (fun t : ℝ × ℝ ↦ 1-t.2) (𝓝 (l,p)) (𝓝 (1-p)) :=
    tendsto_const_nhds.sub hg
  have h₀ := hg.rpow hf (Or.inr (sub_pos.mpr hl))
  have h₁ := hq.rpow hf (Or.inr (sub_pos.mpr hl))
  have hs : p^(1-l)+(1-p)^(1-l) ≠ 0 := by
    rcases hp.2.eq_or_lt with he | he
    · simp [he, zero_rpow (sub_ne_zero.mpr hl.ne')]
    · exact (add_pos_of_nonneg_of_pos (rpow_nonneg hp.1 _)
        (rpow_pos_of_pos (sub_pos.mpr he) _)).ne'
  change Tendsto (fun t : ℝ × ℝ ↦ weight t.1 t.2) (𝓝 (l,p)) (𝓝 (weight l p))
  rw [weight,ite_eq_left hl]
  apply ((h₀.mul h₁).div (h₀.add h₁) hs).congr'
  filter_upwards [continuousAt_fst.eventually (Iio_mem_nhds hl)] with t ht
  exact (ite_eq_left (show t.1 < 1 from ht)).symm

private theorem fitted_weight_bounds {l p : ℝ} (hl : l ≤ 1) (hp : p ∈ Icc 0 1) :
    weight l p ∈ Icc 0 (1/2) := by
  rcases hl.eq_or_lt with rfl | h
  · norm_num [weight]
  · rw [weight,ite_eq_left h]
    let a := p ^ (1-l)
    let b := (1-p) ^ (1-l)
    have ha : a ∈ Icc (0 : ℝ) 1 :=
      ⟨rpow_nonneg hp.1 _,rpow_le_one hp.1 hp.2 (sub_nonneg.mpr h.le)⟩
    have hb : b ∈ Icc (0 : ℝ) 1 := by
      have hn := sub_nonneg.mpr hp.2
      exact ⟨rpow_nonneg hn _,rpow_le_one hn (by linarith [hp.1]) (sub_nonneg.mpr h.le)⟩
    change a*b/(a+b) ∈ Icc (0 : ℝ) (1/2)
    refine ⟨div_nonneg (mul_nonneg ha.1 hb.1) (add_nonneg ha.1 hb.1),?_⟩
    by_cases hz : a+b = 0
    · simp [hz]
    · apply (div_le_iff₀ ((add_nonneg ha.1 hb.1).lt_of_ne (Ne.symm hz))).mpr
      nlinarith [mul_le_mul_of_nonneg_left ha.2 hb.1,
        mul_le_mul_of_nonneg_left hb.2 ha.1]


/-- The fitted held-scale graph retains both shape endpoints. -/
def fittedGraph {n : ℕ} (x : Fin n → ℝ) (s : ℝ) : Set (ℝ × (ℝ × (Fin n → ℝ))) :=
  {q | q.1 ∈ Icc (-1) 1 ∧ q.2 ∈ graph x q.1 s}

/-- Ordinary product likelihood on the fitted held-scale graph. -/
noncomputable def fittedLikelihood {n : ℕ} (s : ℝ)
    (q : ℝ × (ℝ × (Fin n → ℝ))) : ℝ := graphLikelihood q.1 s q.2

/-- Shape, probabilities and density weights form the coordinates of the compactification.
Location is reconstructed from the quantile equation at one observation. -/
abbrev Lift (n : ℕ) := ℝ × (Fin n → ℝ) × (Fin n → ℝ)

private noncomputable def lift {n : ℕ} (q : ℝ × (ℝ × (Fin n → ℝ))) : Lift n :=
  (q.1,q.2.2,fun j ↦ weight q.1 (q.2.2 j))

private noncomputable def score {n : ℕ} (s : ℝ) (r : Lift n) : ℝ :=
  ∏ j, r.2.2 j / s

private noncomputable def unlift {n : ℕ} (i : Fin n) (x : Fin n → ℝ) (s : ℝ)
    (r : Lift n) : ℝ × (ℝ × (Fin n → ℝ)) :=
  (r.1, x i - s * Q r.1 (r.2.1 i), r.2.1)

private theorem score_cont {n : ℕ} (s : ℝ) : Continuous (@score n s) := by
  unfold score
  exact continuous_finsetProd _ fun j _ ↦
    (show Continuous (fun r : Lift n ↦ r.2.2 j / s) by fun_prop)

private theorem graph_eq_Q {n : ℕ} {x : Fin n → ℝ} {l s : ℝ} {q : ℝ × (Fin n → ℝ)}
    (h : q ∈ graph x l s) (j : Fin n) : q.1 + s * Q l (q.2 j) = x j := by
  by_cases hl : l ≤ 0
  · exact ((graph_nonpos hl).mp h j).2
  · have hp : 0 < l := lt_of_not_ge hl
    simpa only [Q,hp.ne',ite_false] using ((graph_pos hp ▸ h) j).2

private theorem unlift_lift {n : ℕ} (i : Fin n) {x : Fin n → ℝ} {s : ℝ}
    {q : ℝ × (ℝ × (Fin n → ℝ))} (hq : q ∈ fittedGraph x s) :
    unlift i x s (lift q) = q := by
  refine Prod.ext rfl (Prod.ext ?_ rfl)
  change x i - s * Q q.1 (q.2.2 i) = q.2.1
  linarith [graph_eq_Q hq.2 i]

private noncomputable def trace {n : ℕ} (x : Fin n → ℝ) (s c : ℝ) : Set (Lift n) :=
  lift '' (fittedGraph x s ∩ {q | c ≤ fittedLikelihood s q})

private theorem trace_closedBox {n : ℕ} (x : Fin n → ℝ) (s c : ℝ) :
    IsCompact (closure (trace x s c)) := by
  let P (b : ℝ) : Set (Fin n → ℝ) := Set.pi univ fun _ ↦ Icc 0 b
  let B : Set (Lift n) := Icc (-1) 1 ×ˢ (P 1 ×ˢ P (1/2))
  have hB : IsCompact B := isCompact_Icc.prod
    ((isCompact_univ_pi fun _ ↦ isCompact_Icc).prod (isCompact_univ_pi fun _ ↦ isCompact_Icc))
  apply hB.of_isClosed_subset isClosed_closure
  apply closure_minimal _ hB.isClosed
  rintro _ ⟨q, hq, rfl⟩
  exact ⟨hq.1.1,fun j _ ↦ graph_probs hq.1.2 j,
    fun j _ ↦ fitted_weight_bounds hq.1.1.2 (graph_probs hq.1.2 j)⟩

private theorem trace_limit {n : ℕ} {x : Fin n → ℝ} {s c : ℝ}
    (i : Fin n) (hs : 0 < s) (hc : 0 < c) {r : Lift n}
    (hr : r ∈ closure (trace x s c)) :
    unlift i x s r ∈ fittedGraph x s ∧ score s r ≤ fittedLikelihood s (unlift i x s r) ∧
    ContinuousAt (unlift i x s) r := by
  obtain ⟨u, hu, ht⟩ := mem_closure_iff_seq_limit.mp hr
  choose v hv he using hu
  have hl (k : ℕ) : (u k).1 ∈ Icc (-1) 1 := by rw [← he k]; exact (hv k).1.1
  have hp (k : ℕ) (j : Fin n) : (u k).2.1 j ∈ Icc 0 1 := by
    rw [← he k]; exact graph_probs (hv k).1.2 j
  have hw (k : ℕ) (j : Fin n) : (u k).2.2 j = weight (u k).1 ((u k).2.1 j) := by
    rw [← he k]; rfl
  have h₀ : Tendsto (fun k ↦ (u k).1) atTop (𝓝 r.1) := continuous_fst.continuousAt.tendsto.comp ht
  have h₁ (j : Fin n) : Tendsto (fun k ↦ (u k).2.1 j) atTop (𝓝 (r.2.1 j)) :=
    ((continuous_apply j).comp (continuous_fst.comp continuous_snd)).continuousAt.tendsto.comp ht
  have h₂ (j : Fin n) : Tendsto (fun k ↦ (u k).2.2 j) atTop (𝓝 (r.2.2 j)) :=
    ((continuous_apply j).comp (continuous_snd.comp continuous_snd)).continuousAt.tendsto.comp ht
  have hrl : r.1 ∈ Icc (-1) 1 := isClosed_Icc.mem_of_tendsto h₀ (Eventually.of_forall hl)
  have hrp (j : Fin n) : r.2.1 j ∈ Icc 0 1 :=
    isClosed_Icc.mem_of_tendsto (h₁ j) (Eventually.of_forall fun k ↦ hp k j)
  have hrw (j : Fin n) : r.2.2 j ∈ Icc 0 (1/2) :=
    isClosed_Icc.mem_of_tendsto (h₂ j) (Eventually.of_forall fun k ↦
      (hw k j).symm ▸ fitted_weight_bounds (hl k).2 (hp k j))
  have hrc : c ≤ score s r :=
    ge_of_tendsto ((score_cont s).continuousAt.tendsto.comp ht)
      (Eventually.of_forall fun k ↦ by
        change c ≤ score s (u k)
        rw [← he k]
        exact (hv k).2)
  have hstrict (j : Fin n) : 0 < r.2.2 j := by
    refine (hrw j).1.lt_of_ne fun h ↦ ?_
    have hz : score s r = 0 := Finset.prod_eq_zero (Finset.mem_univ j)
      (by rw [← h,zero_div])
    exact hc.not_ge (hz ▸ hrc)
  have hwlim (h : r.1 < 1) (j : Fin n) : r.2.2 j = weight r.1 (r.2.1 j) := by
    apply tendsto_nhds_unique (h₂ j)
    exact ((w_joint r.1 (r.2.1 j) h (hrp j)).tendsto.comp ((h₀).prodMk_nhds (h₁ j))).congr
      (fun k ↦ (hw k j).symm)
  have hinterior (h : r.1 < 1) (j : Fin n) : r.2.1 j ∈ Ioo 0 1 := by
    have hn : 1-r.1 ≠ 0 := sub_ne_zero.mpr h.ne'
    have hne : weight r.1 (r.2.1 j) ≠ 0 := (hwlim h j) ▸ (hstrict j).ne'
    refine ⟨(hrp j).1.lt_of_ne ?_,(hrp j).2.lt_of_ne ?_⟩
    · intro he
      apply hne
      simp [← he,weight,h,zero_rpow hn]
    · intro he
      apply hne
      simp [he,weight,h,zero_rpow hn]
  have hq (j : Fin n) : ContinuousAt (fun t : ℝ × ℝ ↦ Q t.1 t.2) (r.1,r.2.1 j) := by
    by_cases h : 0 < r.1
    · exact q_joint_positive _ _ h
    · exact q_joint_interior _ _ (hinterior (by linarith) j)
  have heq (j : Fin n) :
      x i - s * Q r.1 (r.2.1 i) + s * Q r.1 (r.2.1 j) = x j := by
    have H (j : Fin n) := (hq j).tendsto.comp (h₀.prodMk_nhds (h₁ j))
    apply tendsto_nhds_unique (f := fun k : ℕ ↦
      x i - s * Q (u k).1 ((u k).2.1 i) + s * Q (u k).1 ((u k).2.1 j)) (l := atTop)
    · exact (tendsto_const_nhds.sub ((H i).const_mul s)).add ((H j).const_mul s)
    · refine tendsto_const_nhds.congr fun k ↦ ?_
      rw [← he k]
      change x j = x i - s * Q (v k).1 ((v k).2.2 i) + s * Q (v k).1 ((v k).2.2 j)
      linarith [graph_eq_Q (hv k).1.2 i,graph_eq_Q (hv k).1.2 j]
  refine ⟨⟨hrl,?_⟩,?_,?_⟩
  · by_cases h : r.1 ≤ 0
    · exact (graph_nonpos h).mpr fun j ↦ ⟨hinterior (by linarith) j,heq j⟩
    · have hh : 0 < r.1 := lt_of_not_ge h
      change (x i - s * Q r.1 (r.2.1 i),r.2.1) ∈ graph x r.1 s
      rw [graph_pos hh]
      exact fun j ↦ ⟨hrp j,by simpa only [Q,hh.ne',ite_false] using heq j⟩
  · change (∏ j, r.2.2 j / s) ≤ ∏ j, weight r.1 (r.2.1 j) / s
    apply Finset.prod_le_prod₀ (fun j _ ↦ div_nonneg (hrw j).1 hs.le)
    intro j _
    apply div_le_div_of_nonneg_right _ hs.le
    rcases hrl.2.eq_or_lt with h | h
    · simpa [h,weight] using (hrw j).2
    · exact (hwlim h j).le
  · unfold unlift
    apply continuousAt_fst.prodMk
    refine (continuousAt_const.sub ?_).prodMk (continuous_fst.comp continuous_snd).continuousAt
    apply ContinuousAt.const_mul
    exact (hq i).comp (f := fun t : Lift n ↦ (t.1,t.2.1 i)) (by fun_prop)

/-- Every positive likelihood superlevel on the fitted held-scale chart is compact,
including at moving support contacts and at both shape endpoints. -/
theorem fitted_scale_superlevel {n : ℕ} (x : Fin n → ℝ) (i : Fin n) {s c : ℝ}
    (hs : 0 < s) (hc : 0 < c) :
    IsCompact (fittedGraph x s ∩ {q | c ≤ fittedLikelihood s q}) := by
  have hK := trace_closedBox x s c
  have he : fittedGraph x s ∩ {q | c ≤ fittedLikelihood s q} =
      unlift i x s '' closure (trace x s c) := by
    ext q
    constructor
    · intro hq
      exact ⟨lift q,subset_closure ⟨q,hq,rfl⟩,unlift_lift i hq.1⟩
    · rintro ⟨r,hr,rfl⟩
      obtain ⟨hg,hw,_⟩ := trace_limit i hs hc hr
      have hclosed : IsClosed {r : Lift n | c ≤ score s r} :=
        isClosed_le continuous_const (score_cont s)
      have hr' := closure_minimal (s := trace x s c) (t := {r | c ≤ score s r})
        (by rintro _ ⟨v,hv,rfl⟩; exact hv.2) hclosed hr
      exact ⟨hg,(show c ≤ score s r from hr').trans hw⟩
  rw [he]
  exact hK.image_of_continuousOn (fun _ hr ↦ (trace_limit i hs hc hr).2.2.continuousWithinAt)

/-- Every finite nonempty sample has a positive-likelihood maximizer over the
fitted shape interval when scale is held positive. -/
theorem fitted_scale_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ}
    (hs : 0 < s) :
    ∃ q ∈ fittedGraph x s, 0 < fittedLikelihood s q ∧
      IsMaxOn (fittedLikelihood s) (fittedGraph x s) q := by
  let i : Fin n := ⟨0,hn⟩
  obtain ⟨p,hp,hv⟩ := nonpos_witness x (show (0 : ℝ) ≤ 0 by rfl) hs 0
  let q : ℝ × (ℝ × (Fin n → ℝ)) := (0,0,p)
  have hq : q ∈ fittedGraph x s := ⟨by norm_num, hp⟩
  let c := fittedLikelihood s q
  have hc : 0 < c := hv
  have hmem : lift q ∈ closure (trace x s c) :=
    subset_closure ⟨q,⟨hq,show c ≤ c from le_rfl⟩,rfl⟩
  obtain ⟨r,hr,hm⟩ := (trace_closedBox x s c).exists_isMaxOn ⟨lift q,hmem⟩
    (score_cont s).continuousOn
  obtain ⟨hfg,hle,_⟩ := trace_limit i hs hc hr
  have hb : c ≤ fittedLikelihood s (unlift i x s r) := (hm hmem).trans hle
  refine ⟨unlift i x s r,hfg,hc.trans_le hb,fun t ht ↦ ?_⟩
  by_cases he : c ≤ fittedLikelihood s t
  · exact (hm (subset_closure ⟨t,⟨ht,he⟩,rfl⟩)).trans hle
  · exact (le_of_not_ge he).trans hb

/-- Infinite location has zero positive envelope. The negative and uniform shape
endpoints remain in the graph, and at every positive shape the closed support
contacts are represented with the prescribed density weights. -/
theorem fitted_scale_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ}
    (hs : 0 < s) :
    (∀ c > 0, ∃ M : ℝ, ∀ q ∈ fittedGraph x s, M < |q.2.1| → fittedLikelihood s q < c) ∧
    (∀ m : ℝ, ∃ p : Fin n → ℝ, (-1,m,p) ∈ fittedGraph x s ∧
      0 < fittedLikelihood s (-1,m,p)) ∧
    (∀ l ∈ Ioc (0 : ℝ) 1, ∀ m : ℝ,
      (∃! p : Fin n → ℝ, (l,m,p) ∈ fittedGraph x s) ↔ ∀ j, |x j-m| ≤ s/l) := by
  refine ⟨?_,?_,?_⟩
  · intro c hc
    have hk := fitted_scale_superlevel x (⟨0,hn⟩ : Fin n) hs hc
    have ha := hk.image (continuous_abs.comp (continuous_fst.comp continuous_snd))
    obtain ⟨M,hM⟩ := ha.bddAbove
    exact ⟨M,fun q hq hMq ↦ lt_of_not_ge fun h ↦
      hMq.not_ge (hM (mem_image_of_mem _ ⟨hq,h⟩))⟩
  · intro m
    obtain ⟨p,hp,hv⟩ := nonpos_witness x (show (-1 : ℝ) ≤ 0 by norm_num) hs m
    exact ⟨p,⟨by norm_num,hp⟩,hv⟩
  · intro l hl m
    have hI : l ∈ Icc (-1 : ℝ) 1 := ⟨by linarith [hl.1],hl.2⟩
    change (∃! p : Fin n → ℝ, l ∈ Icc (-1 : ℝ) 1 ∧ (m,p) ∈ graph x l s) ↔ _
    simp only [hI,true_and,graph_pos hl.1]
    exact held_positive_shape_scale_location_graph x hl.1 hs m

end UnivariateML.TLHeldScale
end
