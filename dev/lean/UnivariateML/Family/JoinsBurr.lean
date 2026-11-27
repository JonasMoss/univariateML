module

public import UnivariateML.Family.BurrFree
import Mathlib.Tactic

set_option linter.style.header false

/-!
## Deviations

The optimized Pareto path approaches the limiting endpoint from below.
A scale kept equal to the endpoint has a density penalty at every contact
and does not recover the optimized Pareto likelihood.
-/

@[expose] public section

open Set Filter Topology Real
open UnivariateML.BurrHeld UnivariateML.BurrFree

namespace UnivariateML.JoinsBurr

/-- With any fixed positive first shape, taking the power to infinity
and keeping scale at the common observation gives divergence. -/
theorem constant_path {n : ℕ} (hn : 0 < n) {v k : ℝ} (hv : 0 < v) (hk : 0 < k) :
    (∀ᶠ t : ℝ in atTop, (k, t, v) ∈ physicalDomain) ∧
    Tendsto (fun t : ℝ ↦ logLik (fun _ : Fin n ↦ v) k t v) atTop atTop := by
  refine ⟨(eventually_gt_atTop 0).mono (fun _ ht ↦ ⟨hk, ht, hv⟩), ?_⟩
  have he (t : ℝ) : logLik (fun _ : Fin n ↦ v) k t v =
      log t + (log k - log v - (k + 1) * softplus 0) := by
    unfold logLik logDensity avg
    simp only [div_self hv.ne', log_one, mul_zero, Finset.sum_const,
      Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
    rw [mul_div_cancel_left₀ _ (Nat.cast_ne_zero.mpr hn.ne')]
    ring
  simp_rw [he]
  refine tendsto_atTop.mpr fun b ↦ ?_
  filter_upwards [tendsto_atTop.mp tendsto_log_atTop (b - (log k - log v - (k + 1) * softplus 0))]
    with t ht
  linarith

/-- In physical coordinates the optimized Pareto path has diverging
power, constant positive product of shapes, and scale approaching the
sample minimum strictly from below. -/
theorem pareto_path {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (hne : ∃ i j, x i ≠ x j) (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let y := logExcess x j
    let a := (avg y)⁻¹
    let s := fun t : ℝ ↦ exp (log (x j) - t⁻¹)
    0 < a ∧
    (∀ᶠ t : ℝ in atTop, (a / t^2, t^2, s t) ∈ physicalDomain ∧
      (a / t^2) * t^2 = a) ∧
    Tendsto (fun t : ℝ ↦ t^2) atTop atTop ∧
    Tendsto s atTop (𝓝[<] x j) ∧
    Tendsto (fun t : ℝ ↦ logLik x (a / t^2) (t^2) (s t)) atTop
      (𝓝 (BurrFree.paretoValue y - avg (fun i ↦ log (x i)))) := by
  intro y a s
  have hy (i : Fin n) : 0 ≤ y i := sub_nonneg.mpr (log_le_log (hx j) (hj i))
  have hypos : ∃ i, 0 < y i := by
    by_contra h
    push Not at h
    have he (i : Fin n) : x i = x j := by
      have e : y i = 0 := le_antisymm (h i) (hy i)
      change log (x i) - log (x j) = 0 at e
      rw [← exp_log (hx i), ← exp_log (hx j), sub_eq_zero.mp e]
    obtain ⟨i, k, hk⟩ := hne
    exact hk ((he i).trans (he k).symm)
  obtain ⟨i, hi⟩ := hypos
  have hn : 0 < n := j.isLt.pos
  have hm : 0 < avg y := by
    unfold avg
    exact div_pos (Finset.sum_pos' (fun k _ ↦ hy k) ⟨i, Finset.mem_univ _, hi⟩)
      (Nat.cast_pos.mpr hn)
  have ha : 0 < a := inv_pos.mpr hm
  have ht := eventually_gt_atTop (0 : ℝ)
  refine ⟨ha, ht.mono (fun t h ↦ ⟨⟨div_pos ha (sq_pos_of_pos h), sq_pos_of_pos h, exp_pos _⟩,
    div_mul_cancel₀ _ (sq_pos_of_pos h).ne'⟩), tendsto_pow_atTop (by decide), ?_, ?_⟩
  · apply tendsto_nhdsWithin_iff.mpr
    constructor
    · have h := (tendsto_const_nhds (x := log (x j))).sub tendsto_inv_atTop_zero
      simpa [s, Function.comp_def, exp_log (hx j)] using (continuous_exp.tendsto _).comp h
    · filter_upwards [ht] with t h
      change exp (log (x j) - t⁻¹) < x j
      conv_rhs => rw [← exp_log (hx j)]
      exact exp_lt_exp.mpr (sub_lt_self _ (inv_pos.mpr h))
  · refine ((BurrFree.pareto_path hn hy hm).sub_const _).congr' ?_
    filter_upwards [ht] with t h
    rw [logLik_eq_first hn hx (show 0 < s t from exp_pos _)]
    congr 1
    unfold BurrFree.likelihood firstLik
    dsimp only
    have he (i : Fin n) :
        t ^ 2 * y i + t = t ^ 2 * log (x i) + -(t ^ 2 * log (s t)) := by
      dsimp [s, y, logExcess]
      rw [log_exp]
      field_simp
      ring
    simp_rw [he]
    rfl

/-- Keeping the scale equal to the sample minimum imposes a strict
gap below the optimized, moving-endpoint Pareto face. -/
theorem fixed_endpoint_penalty {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) (hpos : ∃ i, x j < x i) :
    ∃ B < BurrFree.paretoValue (logExcess x j) - avg (fun i ↦ log (x i)),
      ∀ k > 0, ∀ c > 0, logLik x k c (x j) < B := by
  let z := logRatios x (x j)
  have hz (i : Fin n) : z i = logExcess x j i :=
    log_div (hx i).ne' (hx j).ne'
  have h0 (i : Fin n) : 0 ≤ z i := by
    rw [hz]
    exact sub_nonneg.mpr (log_le_log (hx j) (hj i))
  have hp : ∃ i, 0 < z i := by
    obtain ⟨i, hi⟩ := hpos
    exact ⟨i, by rw [hz]; exact sub_pos.mpr (log_lt_log (hx j) hi)⟩
  have hn := j.isLt.pos
  have hzc : 0 < zeroCount z := Finset.card_pos.mpr ⟨j, by simp [z, logRatios, (hx j).ne']⟩
  have hpen : 0 < ((zeroCount z : ℝ) / n) * log 2 :=
    mul_pos (div_pos (Nat.cast_pos.mpr hzc) (Nat.cast_pos.mpr hn)) (log_pos (by norm_num))
  refine ⟨BurrHeld.paretoValue z - avg (fun i ↦ log (x i)), ?_, ?_⟩
  · rw [← funext hz]
    unfold BurrHeld.paretoValue BurrFree.paretoValue
    linarith
  · intro k hk c hc
    rw [logLik_eq_scale hn]
    exact sub_lt_sub_right ((scale_pareto_supremum hn h0 hp).2 (c,k) ⟨hc,hk⟩) _

private theorem softplus_shift (x : ℝ) :
    softplus x = x + softplus (-x) := by
  unfold softplus
  have he : 1 + exp x = exp x * (1 + exp (-x)) := by
    rw [mul_add, mul_one, ← exp_add, add_neg_cancel, exp_zero, add_comm]
  rw [he, log_mul (exp_pos _).ne' (by positivity), log_exp]

private theorem G_div_tendsto {n : ℕ} {z : Fin n → ℝ} (hz : ∀ i, 0 ≤ z i) :
    Tendsto (fun c ↦ G z c / c) atTop (𝓝 (avg z)) := by
  have hy (i : Fin n) : Tendsto (fun c : ℝ ↦ softplus (-(c * z i))) atTop
      (𝓝 (if z i = 0 then log 2 else 0)) := by
    by_cases hi : z i = 0
    · simp [hi, softplus]
      norm_num
    · have hi0 := lt_of_le_of_ne (hz i) (Ne.symm hi)
      have ht : Tendsto (fun c : ℝ ↦ -(c * z i)) atTop atBot :=
        tendsto_neg_atTop_atBot.comp ((tendsto_id.const_mul_atTop hi0).congr
          fun c ↦ mul_comm (z i) c)
      have he : Tendsto (fun c : ℝ ↦ 1 + exp (-(c * z i))) atTop (𝓝 1) := by
        simpa only [add_zero, Function.comp_def] using
          (tendsto_const_nhds (x := (1 : ℝ))).add (tendsto_exp_atBot.comp ht)
      simpa [softplus, hi, Function.comp_def] using (continuousAt_log one_ne_zero).tendsto.comp he
  have hr : Tendsto (R z) atTop (𝓝 (avg (fun i ↦ if z i = 0 then log 2 else 0))) :=
    (tendsto_finsetSum _ fun i _ ↦ hy i).div_const _
  have ht : Tendsto (fun c : ℝ ↦ avg z + R z c / c) atTop (𝓝 (avg z)) := by
    simpa only [add_zero, id_eq] using
      (tendsto_const_nhds (x := avg z)).add (hr.div_atTop tendsto_id)
  refine ht.congr' ?_
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
  have he : G z c = c * avg z + R z c := by
    unfold G R avg
    simp_rw [softplus_shift (c * z _), Finset.sum_add_distrib, ← Finset.mul_sum]
    ring
  rw [he, add_div, mul_div_cancel_left₀ _ hc.ne']

/-- The literal fixed-endpoint Burr path has diverging power and positive
limiting product of shapes. Its likelihood tends to the Pareto trace
with the contact penalty included. -/
theorem pareto_fixed_endpoint_path {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) (hpos : ∃ i, x j < x i) :
    let z := logRatios x (x j)
    let k := bestK z
    (∀ᶠ c : ℝ in atTop, (k c, c, x j) ∈ physicalDomain) ∧
    0 < (avg z)⁻¹ ∧
    Tendsto (fun c : ℝ ↦ c) atTop atTop ∧
    Tendsto (fun c : ℝ ↦ k c * c) atTop (𝓝 ((avg z)⁻¹)) ∧
    Tendsto (fun c : ℝ ↦ logLik x (k c) c (x j)) atTop
      (𝓝 (BurrHeld.paretoValue z - avg (fun i ↦ log (x i)))) := by
  intro z k
  have hn := j.isLt.pos
  have hz (i : Fin n) : 0 ≤ z i :=
    log_nonneg ((le_div_iff₀ (hx j)).mpr (by simpa using hj i))
  have hpos' : ∃ i, 0 < z i := by
    obtain ⟨i, hi⟩ := hpos
    exact ⟨i, log_pos ((lt_div_iff₀ (hx j)).mpr (by simpa using hi))⟩
  have hm : 0 < avg z := by
    obtain ⟨i, hi⟩ := hpos'
    exact div_pos (Finset.sum_pos' (fun l _ ↦ hz l) ⟨i, Finset.mem_univ _, hi⟩)
      (Nat.cast_pos.mpr hn)
  have hK {c : ℝ} (hc : 0 < c) : 0 < k c := (scale_bestK hn z hc).1
  refine ⟨(eventually_gt_atTop 0).mono (fun c hc ↦ ⟨hK hc, hc, hx j⟩),
    inv_pos.mpr hm, tendsto_id, ?_, ?_⟩
  · refine ((G_div_tendsto hz).inv₀ hm.ne').congr fun c ↦ ?_
    change (G z c / c)⁻¹ = (G z c)⁻¹ * c
    rw [inv_div, div_eq_mul_inv, mul_comm]
  · refine ((scale_pareto_limit hn hz hpos').sub_const _).congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with c hc
    rw [logLik_eq_scale hn]
    have hg : G z c ≠ 0 := by intro h; exact (hK hc).ne' (by simp [k, bestK, h])
    unfold scaleProfile scaleLik k bestK
    dsimp only
    rw [log_inv, log_div hg hc.ne', inv_mul_cancel₀ hg]
    ring

/-- Constant positive data have no finite maximum. For nonconstant data
attainment is equivalent to reaching the better optimized face value. -/
theorem all_free_criterion {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2
    let y := logExcess x j
    (∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) ↔
      (∃ i k, x i ≠ x k) ∧
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ∃ p ∈ physicalDomain,
          max (BurrFree.paretoValue y) (weibullLik y c b) - avg (fun i ↦ log (x i)) ≤ L p := by
  intro L y
  have hn := j.isLt.pos
  by_cases hne : ∃ i k, x i ≠ x k
  · rw [and_iff_right hne]
    obtain ⟨c, hc, b, hb, he, _⟩ := physical_criterion_and_escape hn hx hne hj
    constructor
    · intro h
      exact ⟨c, hc, b, hb, he.mp h⟩
    · rintro ⟨d, hd, t, ht, p, hp, hL⟩
      rw [le_antisymm (hb d hd t) (ht c hc b)] at hL
      exact he.mpr ⟨p, hp, hL⟩
  · refine iff_of_false ?_ (fun h ↦ hne h.1)
    rintro ⟨p, _, hm⟩
    have h : x = fun _ ↦ x j := funext fun i ↦
      by_contradiction fun hi ↦ hne ⟨i,j,hi⟩
    have hb : BddAbove ((fun q : ℝ × ℝ × ℝ ↦ logLik x q.1 q.2.1 q.2.2) '' physicalDomain) :=
      hm.bddAbove
    rw [h] at hb
    exact constant_unbounded hn (hx j) hb

private theorem weibull_physical_limit {n : ℕ} {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) (j : Fin n) {c : ℝ} (hc : 0 < c) (b : ℝ) :
    Tendsto (fun k ↦ logLik x k c (exp (log (x j) - (b - log k) / c)))
      atTop (𝓝 (weibullLik (logExcess x j) c b - avg (fun i ↦ log (x i)))) := by
  have hn := j.isLt.pos
  refine ((weibull_path hn (logExcess x j) c b).sub_const _).congr ?_
  intro k
  rw [logLik_eq_first hn hx (exp_pos _)]
  congr 1
  unfold BurrFree.likelihood firstLik
  dsimp only
  have he (i : Fin n) : c * logExcess x j i + (b - log k) =
      c * log (x i) + -(c * log (exp (log (x j) - (b - log k) / c))) := by
    unfold logExcess
    rw [log_exp]
    field_simp
    ring
  simp_rw [he]

/-- Constants have unbounded Burr likelihood. For nonconstant samples
without finite attainment, the larger optimized face value is the
unattained supremum, even when both faces give the same value. -/
theorem all_free_failure {n : ℕ} {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x j ≤ x i) :
    let L := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2
    let y := logExcess x j
    ((∀ i, x i = x j) → ¬ BddAbove (L '' physicalDomain)) ∧
    ((∃ i k, x i ≠ x k) →
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, weibullLik y d t ≤ weibullLik y c b) ∧
        ((¬ ∃ p ∈ physicalDomain, IsMaxOn L physicalDomain p) →
          IsLUB (L '' physicalDomain)
            (max (BurrFree.paretoValue y) (weibullLik y c b) -
              avg (fun i ↦ log (x i))) ∧
          ∀ p ∈ physicalDomain, L p <
            max (BurrFree.paretoValue y) (weibullLik y c b) - avg (fun i ↦ log (x i)))) := by
  intro L y
  refine ⟨?_, ?_⟩
  · intro h
    have he : x = fun _ ↦ x j := funext h
    dsimp only [L]
    rw [he]
    exact constant_unbounded j.isLt.pos (hx j)
  · intro hne
    obtain ⟨c, hc, b, hW, _, _⟩ := physical_criterion_and_escape j.isLt.pos hx hne hj
    refine ⟨c,hc,b,hW,fun h ↦ ?_⟩
    let J := avg (fun i ↦ log (x i))
    let B := max (BurrFree.paretoValue y) (weibullLik y c b) - J
    have hlt (p : ℝ × ℝ × ℝ) (hp : p ∈ physicalDomain) : L p < B := by
      by_contra! hpB
      exact h ((all_free_criterion hx j hj).mpr ⟨hne, c, hc, b, hW, p, hp, hpB⟩)
    refine ⟨⟨?_, ?_⟩, hlt⟩
    · rintro _ ⟨p,hp,rfl⟩
      exact (hlt p hp).le
    · intro M hM
      rw [← max_sub_sub_right]
      apply max_le
      · obtain ⟨_, hparam, _, _, hlim⟩ := pareto_path hx hne j hj
        apply le_of_tendsto hlim
        filter_upwards [hparam] with t ht
        let q : ℝ × ℝ × ℝ := ((avg (logExcess x j))⁻¹ / t^2, t^2,
          exp (log (x j) - t⁻¹))
        change L q ≤ M
        have hq : q ∈ physicalDomain := ht.1
        exact hM (mem_image_of_mem L hq)
      · apply le_of_tendsto (weibull_physical_limit hx j hc b)
        filter_upwards [eventually_gt_atTop (0 : ℝ)] with k hk
        let q : ℝ × ℝ × ℝ := (k,c,exp (log (x j) - (b - log k) / c))
        change L q ≤ M
        have hq : q ∈ physicalDomain := ⟨hk,hc,exp_pos _⟩
        exact hM (mem_image_of_mem L hq)

end UnivariateML.JoinsBurr

end
