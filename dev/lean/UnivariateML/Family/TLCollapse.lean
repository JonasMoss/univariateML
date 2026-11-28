module

public import UnivariateML.Family.TLHeldScale

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Tukey lambda negative-shape collapse

Normalize the likelihood along fixed-location traces and separate the
divergent cases from the critical finite value.

## Deviations

The fixed-location formulas hold on a fixed-shape slice. The majority and
free-shape corner theorems give explicit divergence paths. Joint upper
envelopes for drifting location or shape and the all-free critical box
require further estimates.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.TLCollapse

open LocScaleTukeyLambda TLHeldScale

/-- Multiplicity of the chosen collapse atom. -/
noncomputable def multiplicity {n : ℕ} (x : Fin n → ℝ) (m : ℝ) : ℕ := by
  classical
  exact (Finset.univ.filter fun j ↦ x j = m).card

private theorem q_center {a : ℝ} (ha : 0 < a) :
    Q (-a) (1/2) = 0 := by
  rw [Q,ite_eq_right (neg_ne_zero.mpr ha.ne'),quantile,
    show (1 : ℝ)-1/2=1/2 by norm_num,sub_self,zero_div]

private theorem w_center {a : ℝ} (ha : 0 < a) : 0 < weight (-a) (1/2) := by
  rw [weight,ite_eq_left (show -a < 1 by linarith),
    show (1 : ℝ)-1/2=1/2 by norm_num]
  positivity

private theorem neg_tail_identity {a p z : ℝ} (ha : 0 < a) (hp : p ∈ Ioo 0 1)
    (hz : Q (-a) p = z) :
    let d := max p (1-p)
    weight (-a) p = 1 / ((a*|z|+d^(-a))^((a+1)/a) + d^(-a-1)) := by
  let b := min p (1-p)
  let d := max p (1-p)
  have hq : 0 < 1-p := sub_pos.mpr hp.2
  have hb : 0 < b := lt_min hp.1 hq
  have hdiff : b^(-a) = a*|z| + d^(-a) := by
    have hdiff' : a*|z| = |p^(-a)-(1-p)^(-a)| := by
      rw [← hz,Q,ite_eq_right (neg_ne_zero.mpr ha.ne'),quantile,abs_div,abs_neg,abs_of_pos ha,
        mul_comm a,div_mul_cancel₀ _ ha.ne']
    rw [hdiff']
    rcases le_total p (1-p) with h | h
    · simp only [b,d,min_eq_left h,max_eq_right h]
      rw [abs_of_nonneg (sub_nonneg.mpr (rpow_le_rpow_of_nonpos hp.1 h (by linarith)))]
      ring
    · simp only [b,d,min_eq_right h,max_eq_left h]
      rw [abs_of_nonpos (sub_nonpos.mpr (rpow_le_rpow_of_nonpos hq h (by linarith)))]
      ring
  have hpow : b^(-a-1) = (a*|z|+d^(-a))^((a+1)/a) := by
    rw [show -a-1 = (-a)*((a+1)/a) by field_simp; ring, rpow_mul hb.le,hdiff]
  have hD : p^(-a-1)+(1-p)^(-a-1) = b^(-a-1)+d^(-a-1) := by
    rcases le_total p (1-p) with h | h
    · simp only [b,d,min_eq_left h,max_eq_right h]
    · simp only [b,d,min_eq_right h,max_eq_left h,add_comm]
  have hw : weight (-a) p = 1/(p^(-a-1)+(1-p)^(-a-1)) := by
    rw [weight,ite_eq_left (show -a < 1 by linarith),show 1-(-a) = -(-a-1) by ring,
      rpow_neg hp.1.le,rpow_neg hq.le]
    set P := p ^ (-a-1)
    set V := (1-p) ^ (-a-1)
    have hP : 0 < P := rpow_pos_of_pos hp.1 _
    have hV : 0 < V := rpow_pos_of_pos hq _
    have hsum := (add_pos hP hV).ne'
    field_simp [hP.ne',hV.ne',hsum]
    ring
  rw [hw,hD,hpow]

private theorem max_half (p : ℝ) : (1/2 : ℝ) ≤ max p (1-p) := by
  by_cases h : (1/2 : ℝ) ≤ p
  · exact h.trans (le_max_left _ _)
  · exact (show (1/2 : ℝ) ≤ 1-p by linarith).trans (le_max_right _ _)

private theorem neg_tail_lower {a p z : ℝ} (ha : 0 < a) (hp : p ∈ Ioo 0 1)
    (hz : Q (-a) p = z) :
    1 / (2*(a*|z|+(1/2 : ℝ)^(-a))^((a+1)/a)) ≤ weight (-a) p := by
  rw [neg_tail_identity ha hp hz]
  let d := max p (1-p)
  have hhalf : (1/2 : ℝ) ≤ d := max_half p
  have hd : 0 < d := (by norm_num : (0 : ℝ) < 1/2).trans_le hhalf
  have hu : d^(-a) ≤ (1/2 : ℝ)^(-a) :=
    rpow_le_rpow_of_nonpos (by norm_num) hhalf (neg_nonpos.mpr ha.le)
  have hpow : (a*|z|+d^(-a))^((a+1)/a) ≤ (a*|z|+(1/2 : ℝ)^(-a))^((a+1)/a) :=
    rpow_le_rpow (by positivity) (add_le_add_right hu _) (by positivity)
  have hdpow : d^(-a-1) ≤ (a*|z|+d^(-a))^((a+1)/a) := by
    rw [show -a-1 = (-a)*((a+1)/a) by field_simp; ring,rpow_mul hd.le]
    exact rpow_le_rpow (by positivity) (le_add_of_nonneg_left (by positivity)) (by positivity)
  apply one_div_le_one_div_of_le
  · change 0 < (a*|z|+d^(-a))^((a+1)/a) + d^(-a-1)
    positivity
  · change (a*|z|+d^(-a))^((a+1)/a) + d^(-a-1) ≤ _
    linarith

/-- The negative-shape normalized tail has a strict finite-scale upper bound
and converges to that bound for any admissible probability trace. -/
theorem negative_tail_normalized {a d : ℝ} (ha : 0 < a) (hd : d ≠ 0)
    (p : ℝ → ℝ) (hp : ∀ s > 0, p s ∈ Ioo 0 1 ∧ Q (-a) (p s) = d/s) :
    (∀ s > 0, weight (-a) (p s) / s^((a+1)/a) < 1/(a*|d|)^((a+1)/a)) ∧
    Tendsto (fun s ↦ weight (-a) (p s) / s^((a+1)/a)) (𝓝[>] 0)
      (𝓝 (1/(a*|d|)^((a+1)/a))) := by
  let r := (a+1)/a
  let b := a*|d|
  let v (s : ℝ) := max (p s) (1-p s)
  have hb : 0 < b := mul_pos ha (abs_pos.mpr hd)
  have hr : 0 < r := by dsimp [r]; positivity
  have hv (s : ℝ) : 0 < v s := (by norm_num : (0 : ℝ) < 1/2).trans_le (max_half _)
  have hiden (s : ℝ) (hs : 0 < s) :
      weight (-a) (p s) / s^r = 1/((b+s*(v s)^(-a))^r+s^r*(v s)^(-a-1)) := by
    rw [neg_tail_identity ha (hp s hs).1 (hp s hs).2,abs_div,abs_of_pos hs]
    change (1 / ((a*(|d|/s)+(v s)^(-a))^r+(v s)^(-a-1)))/s^r = _
    have hbase : 0 ≤ a*(|d|/s)+(v s)^(-a) :=
      add_nonneg (mul_nonneg ha.le (div_nonneg (abs_nonneg _) hs.le))
        (rpow_nonneg (hv s).le _)
    rw [div_div, mul_comm _ (s^r),mul_add,← mul_rpow hs.le hbase]
    rw [show s*(a*(|d|/s)+(v s)^(-a)) = b+s*(v s)^(-a) by
      dsimp [b]
      field_simp]
  constructor
  · intro s hs
    rw [hiden s hs]
    apply one_div_lt_one_div_of_lt (rpow_pos_of_pos hb _)
    have hpow := rpow_lt_rpow hb.le
      (lt_add_of_pos_right b (mul_pos hs (rpow_pos_of_pos (hv s) (-a)))) hr
    exact hpow.trans_le (le_add_of_nonneg_right
      (mul_nonneg (rpow_nonneg hs.le _) (rpow_nonneg (hv s).le _)))
  · have hvanish (u : ℝ) (hu : 0 < u) (t : ℝ) (ht : t ≤ 0) :
        Tendsto (fun s : ℝ ↦ s^u*(v s)^t) (𝓝[>] 0) (𝓝 0) := by
      have hpow : Tendsto (fun s : ℝ ↦ s^u) (𝓝[>] 0) (𝓝 0) := by
        simpa only [zero_rpow hu.ne'] using
          ((continuous_rpow_const hu.le).continuousAt (x := (0 : ℝ))).tendsto.mono_left
            nhdsWithin_le_nhds
      have hbig (s : ℝ) : (v s)^t ≤ (1/2 : ℝ)^t :=
        rpow_le_rpow_of_nonpos (by norm_num) (max_half _) ht
      refine tendsto_of_tendsto_of_tendsto_of_le_of_le'
        tendsto_const_nhds (by simpa only [zero_mul] using hpow.mul_const ((1/2 : ℝ)^t))
        ?_ ?_
      · filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
        exact mul_nonneg (rpow_nonneg (show 0 < s from hs).le _) (rpow_nonneg (hv s).le _)
      · filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
        exact mul_le_mul_of_nonneg_left (hbig s) (rpow_nonneg (show 0 < s from hs).le _)
    have h₁ : Tendsto (fun s : ℝ ↦ s*(v s)^(-a)) (𝓝[>] 0) (𝓝 0) := by
      simpa only [rpow_one] using hvanish 1 zero_lt_one (-a) (by linarith)
    have h₂ := hvanish r hr (-a-1) (by linarith)
    have h₃ : Tendsto (fun s : ℝ ↦ b+s*(v s)^(-a)) (𝓝[>] 0) (𝓝 b) := by
      simpa only [add_zero] using tendsto_const_nhds.add h₁
    have h₄ : Tendsto (fun s : ℝ ↦ (b+s*(v s)^(-a))^r+s^r*(v s)^(-a-1))
        (𝓝[>] 0) (𝓝 (b^r)) := by
      simpa only [add_zero] using (h₃.rpow_const (Or.inl hb.ne')).add h₂
    apply (tendsto_const_nhds.div h₄ (rpow_pos_of_pos hb _).ne').congr'
    filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    exact (hiden s hs).symm

private theorem q_zero_iff {a p : ℝ} (ha : 0 < a) (hp : p ∈ Ioo 0 1) :
    Q (-a) p = 0 ↔ p = 1/2 := by
  constructor
  · intro h
    rw [Q,ite_eq_right (neg_ne_zero.mpr ha.ne'),quantile,
      div_eq_zero_iff] at h
    have he := sub_eq_zero.mp (h.resolve_right (neg_ne_zero.mpr ha.ne'))
    have h' := (rpow_left_inj hp.1.le (sub_nonneg.mpr hp.2.le)
      (neg_ne_zero.mpr ha.ne')).mp he
    linarith
  · rintro rfl
    exact q_center ha

/-- The fixed-location negative-shape collapse has the exact power-law
coefficient. The scaled likelihood lies strictly below that coefficient
at every finite scale if at least one residual is nonzero. -/
theorem fixed_location_negative_trace {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a : ℝ}
    (ha : 0 < a) (p : ℝ → Fin n → ℝ)
    (hp : ∀ s > 0, (m,p s) ∈ graph x (-a) s) :
    let e : ℝ := ((n : ℝ)-multiplicity x m)/a - multiplicity x m
    let B := ∏ j, if x j = m then weight (-a) (1/2) else
      1/(a*|x j-m|)^((a+1)/a)
    (∀ s > 0, (∃ j, x j ≠ m) → graphLikelihood (-a) s (m,p s)/s^e < B) ∧
    Tendsto (fun s ↦ graphLikelihood (-a) s (m,p s)/s^e) (𝓝[>] 0) (𝓝 B) := by
  classical
  intro e B
  let S := Finset.univ.filter fun j : Fin n ↦ x j = m
  let T := Finset.univ.filter fun j : Fin n ↦ ¬ x j = m
  let r := (a+1)/a
  have hk : S.card + T.card = n := by rw [Finset.card_filter_add_card_filter_not]; simp
  have he : e = (T.card : ℝ)/a - S.card := by
    have hc : (S.card : ℝ) + T.card = n := by exact_mod_cast hk
    change ((n : ℝ) - S.card)/a - S.card = (T.card : ℝ)/a - S.card
    rw [← hc,add_sub_cancel_left]
  let W (s : ℝ) (j : Fin n) := if x j = m then weight (-a) (1/2) else
    weight (-a) (p s j)/s^r
  let C (j : Fin n) := if x j = m then weight (-a) (1/2) else 1/(a*|x j-m|)^r
  have hobs (s : ℝ) (hs : 0 < s) (j : Fin n) :
      p s j ∈ Ioo 0 1 ∧ Q (-a) (p s j) = (x j-m)/s := by
    have h := hp s hs
    rw [graph,ite_eq_left (neg_nonpos.mpr ha.le)] at h
    refine ⟨(h j).1,(eq_div_iff hs.ne').mpr ?_⟩
    have h' : m + s * Q (-a) (p s j) = x j := (h j).2
    linarith only [h',show Q (-a) (p s j) * s = s * Q (-a) (p s j) by ring]
  have hW (j : Fin n) :
      (∀ s > 0, W s j ≤ C j ∧ (x j ≠ m → W s j < C j)) ∧
      Tendsto (fun s ↦ W s j) (𝓝[>] 0) (𝓝 (C j)) := by
    by_cases hj : x j = m
    · have hw (s : ℝ) : W s j = weight (-a) (1/2) := by simp [W,hj]
      have hc : C j = weight (-a) (1/2) := by simp [C,hj]
      refine ⟨fun s _ ↦ ⟨by rw [hw,hc],fun hn ↦ (hn hj).elim⟩,?_⟩
      simpa only [hw,hc] using (tendsto_const_nhds (x := weight (-a) (1/2)))
    · obtain ⟨h₁,h₂⟩ := negative_tail_normalized ha (sub_ne_zero.mpr hj) (fun s ↦ p s j)
        (fun s hs ↦ hobs s hs j)
      refine ⟨fun s hs ↦ ?_,?_⟩
      · have h : W s j < C j := by simpa only [W,C,hj,ite_false] using h₁ s hs
        exact ⟨h.le,fun _ ↦ h⟩
      · simpa only [W,C,hj,ite_false] using h₂
  have hfactor (s : ℝ) (hs : 0 < s) :
      graphLikelihood (-a) s (m,p s)/s^e = ∏ j, W s j := by
    have hcent (j : Fin n) (hj : j ∈ S) : weight (-a) (p s j)/s = W s j / s := by
      have hz : x j = m := (Finset.mem_filter.mp hj).2
      have hq := (hobs s hs j).2
      rw [hz,sub_self,zero_div] at hq
      rw [(q_zero_iff ha (hobs s hs j).1).mp hq]
      simp [W,hz]
    have htail (j : Fin n) (hj : j ∈ T) :
        weight (-a) (p s j)/s = W s j * s^(1/a : ℝ) := by
      have hz : x j ≠ m := (Finset.mem_filter.mp hj).2
      have hr : r = 1/a+1 := by dsimp [r]; field_simp; ring
      dsimp only [W]
      rw [ite_eq_right hz,hr,rpow_add hs,rpow_one]
      field_simp
    change (∏ j, weight (-a) (p s j)/s)/s^e = _
    rw [← Finset.prod_filter_mul_prod_filter_not Finset.univ (fun j ↦ x j = m)
      (fun j ↦ weight (-a) (p s j)/s),
      ← Finset.prod_filter_mul_prod_filter_not Finset.univ (fun j ↦ x j = m) (W s)]
    change ((∏ j ∈ S, weight (-a) (p s j)/s) *
      (∏ j ∈ T, weight (-a) (p s j)/s))/s^e = (∏ j ∈ S,W s j)*(∏ j ∈ T,W s j)
    rw [Finset.prod_congr rfl hcent,Finset.prod_congr rfl htail,Finset.prod_div_distrib,
      Finset.prod_mul_distrib,Finset.prod_const,Finset.prod_const,he,rpow_sub hs,rpow_natCast,
      ← rpow_mul_natCast hs.le,show (1/a : ℝ)*(T.card : ℝ) = T.card/a by ring]
    field_simp
  constructor
  · intro s hs ⟨j,hj⟩
    rw [hfactor s hs]
    apply Finset.prod_lt_prod₀
    · intro k _
      by_cases hk : x k = m
      · simpa only [W,hk,ite_true] using w_center ha
      · dsimp only [W]
        rw [ite_eq_right hk]
        have hp' := (hobs s hs k).1
        rw [weight,ite_eq_left (show -a < 1 by linarith)]
        have hq : 0 < 1-p s k := sub_pos.mpr hp'.2
        have hh : 0 < p s k := hp'.1
        positivity
    · exact fun k _ ↦ ((hW k).1 s hs).1
    · exact ⟨j,Finset.mem_univ j,((hW j).1 s hs).2 hj⟩
  · have ht : Tendsto (fun s ↦ ∏ j, W s j) (𝓝[>] 0) (𝓝 B) :=
      tendsto_finsetProd _ fun j _ ↦ (hW j).2
    apply ht.congr'
    filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
    exact (hfactor s hs).symm

/-- On the critical held-location and held-negative-shape slice, the
collapse value is the supremum and every positive scale is strictly below it. -/
theorem critical_held_location_supremum {n : ℕ} (x : Fin n → ℝ) (m : ℝ) (hn : 0 < n)
    {a : ℝ} (ha : 0 < a)
    (he : ((n : ℝ)-multiplicity x m)/a - multiplicity x m = 0) :
    let B := ∏ j, if x j = m then weight (-a) (1/2) else
      1/(a*|x j-m|)^((a+1)/a)
    let D : Set (ℝ × (Fin n → ℝ)) := {q | 0 < q.1 ∧ (m,q.2) ∈ graph x (-a) q.1}
    let L : ℝ × (Fin n → ℝ) → ℝ := fun q ↦ graphLikelihood (-a) q.1 (m,q.2)
    sSup (L '' D) = B ∧ ∀ q ∈ D, L q < B := by
  classical
  intro B D L
  have hx : ∃ j, x j ≠ m := by
    by_contra! h
    have hk : multiplicity x m = n := by simp [multiplicity,h]
    rw [hk,sub_self,zero_div,zero_sub,neg_eq_zero] at he
    exact (Nat.cast_pos.mpr hn : (0 : ℝ) < n).ne' he
  have hwit (s : ℝ) (hs : 0 < s) : ∃ p : Fin n → ℝ, (m,p) ∈ graph x (-a) s := by
    choose p hp heq using fun j ↦
      nonpositive_quantile_surjective (neg_nonpos.mpr ha.le) ((x j-m)/s)
    refine ⟨p,?_⟩
    rw [graph,ite_eq_left (neg_nonpos.mpr ha.le)]
    intro j
    refine ⟨hp j,?_⟩
    change m + s * Q (-a) (p j) = x j
    rw [heq j,mul_comm s,div_mul_cancel₀ _ hs.ne']
    ring
  let p (s : ℝ) := if hs : 0 < s then (hwit s hs).choose else fun _ ↦ 1/2
  have hp (s : ℝ) (hs : 0 < s) : (m,p s) ∈ graph x (-a) s := by
    simpa only [p,dite_eq_left hs] using (hwit s hs).choose_spec
  have hstrict : ∀ q ∈ D, L q < B := by
    intro q hq
    have hq₀ : 0 < q.1 := hq.1
    let u (s : ℝ) := if s = q.1 then q.2 else p s
    have hu (s : ℝ) (hs : 0 < s) : (m,u s) ∈ graph x (-a) s := by
      by_cases h : s = q.1
      · subst s
        simpa only [u,ite_true] using hq.2
      · simpa only [u,h,ite_false] using hp s hs
    have ht := (fixed_location_negative_trace x m ha u hu).1 q.1 hq₀ hx
    rwa [he,rpow_zero,div_one,show u q.1 = q.2 by simp [u]] at ht
  have hlim : Tendsto (fun s ↦ L (s,p s)) (𝓝[>] 0) (𝓝 B) := by
    have ht := (fixed_location_negative_trace x m ha p hp).2
    simpa only [he,rpow_zero,div_one] using ht
  have hnon : (L '' D).Nonempty :=
    ⟨L (1,p 1),mem_image_of_mem _ (show (1,p 1) ∈ D from ⟨zero_lt_one,hp 1 zero_lt_one⟩)⟩
  have hb : B ∈ upperBounds (L '' D) := by
    rintro _ ⟨q,hq,rfl⟩
    exact (hstrict q hq).le
  refine ⟨le_antisymm (csSup_le hnon hb) ?_,hstrict⟩
  apply le_of_tendsto hlim
  filter_upwards [eventually_mem_nhdsWithin (s := Ioi (0 : ℝ))] with s hs
  exact le_csSup ⟨B,hb⟩ (mem_image_of_mem _ (show (s,p s) ∈ D from ⟨hs,hp s hs⟩))

/-- At negative shape, the centered collapse path has at least the asserted
power-law growth. Every strictly negative exponent causes divergence. -/
theorem negative_exponent_unbounded {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a : ℝ}
    (ha : 0 < a) (he : ((n : ℝ) - multiplicity x m)/a - multiplicity x m < 0) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
      (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p) := by
  classical
  let S := Finset.univ.filter fun j : Fin n ↦ x j = m
  let T := Finset.univ.filter fun j : Fin n ↦ ¬ x j = m
  have hk : S.card + T.card = n := by
    rw [Finset.card_filter_add_card_filter_not]
    simp
  have hT : (T.card : ℝ) = (n : ℝ) - multiplicity x m := by
    change (T.card : ℝ) = (n : ℝ) - (S.card : ℝ)
    exact eq_sub_of_add_eq (by exact_mod_cast (add_comm T.card S.card).trans hk)
  let A := a*(∑ j, |x j-m|)+(1/2 : ℝ)^(-a)
  have hA : 0 < A := by dsimp [A]; positivity
  let H := 1 / (2 * A^((a+1)/a))
  have hH : 0 < H := by dsimp [H]; positivity
  let M := weight (-a) (1/2)
  have hM : 0 < M := w_center ha
  let C := M^S.card * H^T.card
  have hC : 0 < C := by dsimp [C]; positivity
  let e : ℝ := T.card/a - S.card
  have hneg : e < 0 := by simp only [e,hT]; exact he
  have ht : Tendsto (fun s : ℝ ↦ C*s^e) (𝓝[>] 0) atTop :=
    (tendsto_rpow_neg_nhdsGT_zero hneg).const_mul_atTop hC
  intro B δ hδ
  have hδ' : 0 < min δ 1 := lt_min hδ zero_lt_one
  have hnear : ∀ᶠ s : ℝ in 𝓝[>] 0, s ∈ Ioo 0 (min δ 1) := Ioo_mem_nhdsGT hδ'
  obtain ⟨s,hs,hB⟩ := (hnear.and (ht.eventually_gt_atTop B)).exists
  have hs₁ := hs.2.le.trans (min_le_right δ 1)
  have hsa := hs.1
  choose u hu hv using fun j ↦ nonpositive_quantile_surjective (neg_nonpos.mpr ha.le) ((x j-m)/s)
  let p (j : Fin n) := if x j = m then 1/2 else u j
  have hp (j : Fin n) : p j ∈ Ioo 0 1 ∧ Q (-a) (p j) = (x j-m)/s := by
    by_cases hj : x j = m
    · have h : p j = 1/2 := by simp [p,hj]
      rw [h,hj,sub_self,zero_div]
      exact ⟨by norm_num,q_center ha⟩
    · simpa only [p,hj,ite_false] using And.intro (hu j) (hv j)
  have hg : (m,p) ∈ graph x (-a) s := by
    rw [graph,ite_eq_left (neg_nonpos.mpr ha.le)]
    intro j
    refine ⟨(hp j).1,?_⟩
    change m + s * Q (-a) (p j) = x j
    rw [(hp j).2, mul_comm s,div_mul_cancel₀ _ hsa.ne']
    ring
  have hbound (j : Fin n) : H*s^(1/a : ℝ) ≤ weight (-a) (p j)/s := by
    have h := neg_tail_lower ha (hp j).1 (hp j).2
    have hd := Finset.single_le_sum (f := fun k ↦ |x k-m|)
      (fun k _ ↦ abs_nonneg _) (Finset.mem_univ j)
    have hm : a*|(x j-m)/s|+(1/2 : ℝ)^(-a) ≤ A/s := by
      rw [abs_div,abs_of_pos hsa]
      apply (le_div_iff₀ hsa).mpr
      rw [add_mul,mul_assoc,div_mul_cancel₀ _ hsa.ne']
      dsimp [A]
      linarith [mul_le_mul_of_nonneg_left hd ha.le,
        mul_le_mul_of_nonneg_left hs₁ (rpow_nonneg (by norm_num : (0 : ℝ) ≤ 1/2) (-a))]
    have hpow := rpow_le_rpow (by positivity : 0 ≤ a*|(x j-m)/s|+(1/2 : ℝ)^(-a))
      hm (by positivity : 0 ≤ (a+1)/a)
    calc
      H*s^(1/a : ℝ) = (1 / (2*(A/s)^((a+1)/a)))/s := by
        rw [div_rpow hA.le hsa.le, show (a+1)/a = 1/a+1 by field_simp; ring,
          rpow_add hsa,rpow_one]
        unfold H
        rw [show 1/a+1 = (a+1)/a by field_simp; ring]
        field_simp
      _ ≤ (1 / (2*(a*|(x j-m)/s|+(1/2 : ℝ)^(-a))^((a+1)/a)))/s :=
        div_le_div_of_nonneg_right
          (one_div_le_one_div_of_le (by positivity) (by linarith)) hsa.le
      _ ≤ _ := div_le_div_of_nonneg_right h hsa.le
  have hcent : (∏ j ∈ S, weight (-a) (p j)/s) = (M/s)^S.card := by
    rw [← Finset.prod_const]
    apply Finset.prod_congr rfl
    intro j hj
    have h : x j = m := (Finset.mem_filter.mp hj).2
    simp [M,p,h]
  have hother : (H*s^(1/a : ℝ))^T.card ≤ ∏ j ∈ T, weight (-a) (p j)/s := by
    rw [← Finset.prod_const]
    exact Finset.prod_le_prod₀ (fun _ _ ↦ by positivity) (fun j _ ↦ hbound j)
  have hall : C*s^e ≤ graphLikelihood (-a) s (m,p) := by
    calc
      C*s^e = (M/s)^S.card * (H*s^(1/a : ℝ))^T.card := by
        simp only [C,e,div_pow,mul_pow,rpow_sub hsa,rpow_natCast,← rpow_mul_natCast hsa.le]
        rw [show (1/a : ℝ)*(T.card : ℝ) = T.card/a by ring]
        ring
      _ ≤ (∏ j ∈ S, weight (-a) (p j)/s) * (∏ j ∈ T, weight (-a) (p j)/s) := by
        rw [hcent]
        exact mul_le_mul_of_nonneg_left hother (by positivity)
      _ = graphLikelihood (-a) s (m,p) := Finset.prod_filter_mul_prod_filter_not _ _ _
  exact ⟨s,⟨hsa,hs.2.trans_le (min_le_left _ _)⟩,p,hg,hB.trans_le hall⟩

/-- A strict majority atom gives arbitrary likelihood at arbitrarily small
scale on the fitted all-free branch, already at shape minus one. -/
theorem majority_unbounded {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (h : (n : ℝ) < 2 * multiplicity x m) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
      (m,p) ∈ graph x (-1) s ∧ B < graphLikelihood (-1) s (m,p) :=
  negative_exponent_unbounded x m (show (0 : ℝ) < 1 by norm_num) (by
    rw [div_one]
    linarith)

/-- A tie at the held location allows joint zero-scale, negative-infinite-shape
collapse with arbitrarily large likelihood. -/
theorem held_location_tie_corner {n : ℕ} (x : Fin n → ℝ) (m : ℝ)
    (i : Fin n) (hi : x i = m) :
    ∀ A B : ℝ, ∀ δ > (0 : ℝ), ∃ a > A, 0 < a ∧
      ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
        (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p) := by
  classical
  have hk : 1 ≤ multiplicity x m := Finset.one_le_card.mpr
    ⟨i, Finset.mem_filter.mpr ⟨Finset.mem_univ i,hi⟩⟩
  have hkr : (1 : ℝ) ≤ multiplicity x m := by exact_mod_cast hk
  intro A B δ hδ
  let a := max A (n : ℝ) + 1
  have ha : 0 < a := by dsimp [a]; linarith [le_max_right A (n : ℝ)]
  have hAn : (n : ℝ) < a := by dsimp [a]; linarith [le_max_right A (n : ℝ)]
  have he : ((n : ℝ)-multiplicity x m)/a - multiplicity x m < 0 := by
    apply sub_neg.mpr
    apply (div_lt_iff₀ ha).mpr
    nlinarith [mul_le_mul_of_nonneg_right hkr ha.le]
  exact ⟨a,by dsimp [a]; linarith [le_max_left A (n : ℝ)],
    ha, negative_exponent_unbounded x m ha he B δ hδ⟩

/-- A critical negative held-location exponent has infinite joint envelope
when shape is allowed to tend to the critical value from more negative shapes. -/
theorem held_location_critical_joint_collapse {n : ℕ} (x : Fin n → ℝ) (m : ℝ) {a₀ : ℝ}
    (h₀ : 0 < a₀) (hk : 0 < multiplicity x m)
    (he : ((n : ℝ)-multiplicity x m)/a₀ - multiplicity x m = 0) :
    ∀ B : ℝ, ∀ δ > (0 : ℝ), ∃ a ∈ Ioo a₀ (a₀+δ),
      ∃ s ∈ Ioo (0 : ℝ) δ, ∃ p : Fin n → ℝ,
        (m,p) ∈ graph x (-a) s ∧ B < graphLikelihood (-a) s (m,p) := by
  intro B δ hδ
  let a := a₀+δ/2
  have ha₀ : a₀ < a := by dsimp [a]; linarith
  have ha : 0 < a := h₀.trans ha₀
  have hkr : (0 : ℝ) < multiplicity x m := by exact_mod_cast hk
  have hh : (n : ℝ)-multiplicity x m = multiplicity x m*a₀ :=
    (div_eq_iff h₀.ne').mp (sub_eq_zero.mp he)
  have hneg : ((n : ℝ)-multiplicity x m)/a - multiplicity x m < 0 := by
    apply sub_neg.mpr
    apply (div_lt_iff₀ ha).mpr
    rw [hh]
    exact mul_lt_mul_of_pos_left ha₀ hkr
  exact ⟨a,⟨ha₀,by dsimp [a]; linarith⟩,
    negative_exponent_unbounded x m ha hneg B δ hδ⟩

end UnivariateML.TLCollapse
end
