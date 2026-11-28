module

public import UnivariateML.Family.Discrete4CMB

set_option linter.style.header false

@[expose] public section

open Set Real Filter Topology
open scoped BigOperators

namespace UnivariateML.Base8CMBHeldProbability

open Discrete3CMB

private theorem scalar_minimizers {K : Type*} [Fintype K] [Nonempty K]
    (A s : K → ℝ) (hA : ∀ k, 0 < A k) (hs : ∃ k, s k ≠ 0) :
    let Z := fun t : ℝ ↦ ∑ k, A k * exp (t * s k)
    ((∃ t, IsMinOn Z univ t) ↔ (∃ k, s k < 0) ∧ (∃ k, 0 < s k)) ∧
    (∀ t u, IsMinOn Z univ t → IsMinOn Z univ u → t = u) := by
  classical
  intro Z
  constructor
  · constructor
    · rintro ⟨t,hT⟩
      obtain ⟨k,hk⟩ := hs
      have hm (u : ℝ) : Z t ≤ Z u := hT (mem_univ u)
      constructor
      · by_contra! hneg
        have hlt : Z (t-1) < Z t := by
          apply Finset.sum_lt_sum
          · intro j _
            apply mul_le_mul_of_nonneg_left _ (hA j).le
            exact exp_le_exp.mpr (mul_le_mul_of_nonneg_right (by linarith) (hneg j))
          · refine ⟨k,Finset.mem_univ _,mul_lt_mul_of_pos_left (exp_lt_exp.mpr ?_) (hA k)⟩
            exact mul_lt_mul_of_pos_right (by linarith) (lt_of_le_of_ne (hneg k) hk.symm)
        exact hlt.not_ge (hm _)
      · by_contra! hpos
        have hlt : Z (t+1) < Z t := by
          apply Finset.sum_lt_sum
          · intro j _
            apply mul_le_mul_of_nonneg_left _ (hA j).le
            exact exp_le_exp.mpr (mul_le_mul_of_nonpos_right (by linarith) (hpos j))
          · refine ⟨k,Finset.mem_univ _,mul_lt_mul_of_pos_left (exp_lt_exp.mpr ?_) (hA k)⟩
            exact mul_lt_mul_of_neg_right (by linarith) (lt_of_le_of_ne (hpos k) hk)
        exact hlt.not_ge (hm _)
    · rintro ⟨⟨l,hl⟩,⟨r,hr⟩⟩
      have hcont : Continuous Z := by dsimp [Z]; fun_prop
      let D := {t : ℝ | Z t ≤ Z 0}
      have hD : IsClosed D := isClosed_le hcont continuous_const
      have hp (k : K) (t : ℝ) : A k * exp (t*s k) ≤ Z t :=
        Finset.single_le_sum (f := fun j ↦ A j * exp (t*s j))
          (fun j _ ↦ mul_nonneg (hA j).le (exp_pos _).le) (Finset.mem_univ k)
      have hZ₀ : 0 < Z 0 := (mul_pos (hA l) (exp_pos _)).trans_le (hp l 0)
      have hdom {t : ℝ} (ht : t ∈ D) (k : K) : t*s k ≤ log (Z 0/A k) := by
        rw [← log_exp (t*s k)]
        apply (log_le_log_iff (exp_pos _) (div_pos hZ₀ (hA k))).mpr
        exact (le_div_iff₀ (hA k)).mpr (by rw [mul_comm]; exact (hp k t).trans ht)
      have hcomp : IsCompact D := isCompact_Icc.of_isClosed_subset hD
        (fun t ht ↦ show t ∈ Icc (log (Z 0/A l)/s l) (log (Z 0/A r)/s r) from
          ⟨(div_le_iff_of_neg hl).2 (hdom ht l), (le_div_iff₀ hr).2 (hdom ht r)⟩)
      obtain ⟨t, ht, hT⟩ := hcomp.exists_isMinOn ⟨0,show Z 0 ≤ Z 0 from le_rfl⟩ hcont.continuousOn
      refine ⟨t, fun u _ ↦ ?_⟩
      change Z t ≤ Z u
      by_cases hu : Z u ≤ Z 0
      · exact hT (show u ∈ D from hu)
      · exact (hT (show Z 0 ≤ Z 0 from le_rfl)).trans (le_of_not_ge hu)
  · intro t u ht hu
    by_contra hne
    obtain ⟨k,hk⟩ := hs
    have mid (j : K) : exp ((t+u)/2*s j) ≤ (exp (t*s j)+exp (u*s j))/2 := by
      have H := strictConvexOn_exp.convexOn.2 (mem_univ (t*s j)) (mem_univ (u*s j))
        (show 0 ≤ (1/2 : ℝ) by norm_num) (show 0 ≤ (1/2 : ℝ) by norm_num)
        (show (1/2 : ℝ)+(1/2)=1 by norm_num)
      convert H using 1 <;> simp only [smul_eq_mul] <;> ring_nf
    have mid' : exp ((t+u)/2*s k) < (exp (t*s k)+exp (u*s k))/2 := by
      have H := strictConvexOn_exp.2 (mem_univ (t*s k)) (mem_univ (u*s k))
        (fun he ↦ hne (mul_right_cancel₀ hk he))
        (show 0 < (1/2 : ℝ) by norm_num) (show 0 < (1/2 : ℝ) by norm_num)
        (show (1/2 : ℝ)+(1/2)=1 by norm_num)
      convert H using 1 <;> simp only [smul_eq_mul] <;> ring_nf
    have hlt : Z ((t+u)/2) < (Z t + Z u)/2 := by
      dsimp [Z]
      rw [← Finset.sum_add_distrib, Finset.sum_div]
      apply Finset.sum_lt_sum
      · intro j _
        rw [← mul_add, mul_div_assoc]
        exact mul_le_mul_of_nonneg_left (mid j) (hA j).le
      · refine ⟨k,Finset.mem_univ _,?_⟩
        rw [← mul_add, mul_div_assoc]
        exact mul_lt_mul_of_pos_left mid' (hA k)
    have he : Z t = Z u := le_antisymm (ht (mem_univ _)) (hu (mem_univ _))
    have hle : Z t ≤ Z ((t+u)/2) := ht (mem_univ _)
    linarith

/-- With probability and size fixed, the full dispersion maximizing set is
either empty, a singleton when the empirical height lies strictly between
the cell extrema, or all dispersions in a flat case. -/
theorem held_probability_size_classification {N n : ℕ} (hn : 0 < n)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Icc 0 1) :
    let f := closedLikelihood x p
    let m := (∑ i, height N (x i))/(n : ℝ)
    let Inner := (∃ k, height N k < m) ∧ (∃ k, m < height N k)
    ((p = 0 ∨ p = 1 ∨ N ≤ 1) → ∀ t, IsMaxOn f univ t) ∧
    ((p ∈ Ioo 0 1 ∧ 2 ≤ N) →
      ((∃ t, IsMaxOn f univ t) ↔ Inner) ∧
      (Inner → ∃! t, IsMaxOn f univ t)) := by
  classical
  intro f m Inner
  constructor
  · rintro (rfl | rfl | h) t u _
    · simp [f, closedLikelihood]
    · simp [f, closedLikelihood]
    change closedLikelihood x p u ≤ closedLikelihood x p t
    interval_cases N
    · rw [size_zero_likelihood, size_zero_likelihood]
    · rw [bernoulli_likelihood x hp, bernoulli_likelihood x hp]
  · rintro ⟨hp',hN⟩
    let η := log (p/(1-p))
    let A (k : Fin (N+1)) := exp (η*k.val)
    let s (k : Fin (N+1)) := height N k - m
    let Z (t : ℝ) := ∑ k, A k * exp (t*s k)
    let C := ∏ i, A (x i)
    have hA (k : Fin (N+1)) : 0 < A k := exp_pos _
    have hC : 0 < C := Finset.prod_pos (fun _ _ ↦ hA _)
    have hZ (t : ℝ) : 0 < Z t :=
      Finset.sum_pos (fun k _ ↦ mul_pos (hA k) (exp_pos _)) Finset.univ_nonempty
    have hs : ∃ k, s k ≠ 0 := by
      by_contra! he
      have h0 := he 0
      have h1 := he (⟨1,by omega⟩ : Fin (N+1))
      simp [s, height] at h0 h1
      have hlog : 0 < log (N : ℝ) := log_pos (by exact_mod_cast (show 1 < N by omega))
      linarith
    have hf (t : ℝ) : f t = C / Z t ^ n := by
      have he (k : Fin (N+1)) : weight N η t k =
          exp (t*m) * (A k * exp (t*s k)) := by
        unfold weight
        dsimp [A,s]
        rw [← exp_add, ← exp_add]
        congr 1
        ring
      have hm : (n : ℝ)*m = ∑ i, height N (x i) := by
        dsimp [m]
        field_simp [show (n : ℝ) ≠ 0 from (Nat.cast_pos.mpr hn).ne']
      simp only [f, closedLikelihood, hp'.1.ne', hp'.2.ne, ite_false]
      change (∏ i, weight N η t (x i)/(∑ k : Fin (N+1), weight N η t k)) = _
      simp_rw [he, ← Finset.mul_sum, mul_div_mul_left _ _ (exp_pos _).ne',
        Finset.prod_div_distrib, Finset.prod_mul_distrib, ← exp_sum, ← Finset.mul_sum]
      have hsum : (∑ i, s (x i)) = 0 := by
        simp only [s, Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
          Fintype.card_fin, nsmul_eq_mul, hm, sub_self]
      simp [hsum, C, Z]
    have hiff (t : ℝ) : IsMaxOn f univ t ↔ IsMinOn Z univ t := by
      have he (u : ℝ) : f u ≤ f t ↔ Z t ≤ Z u := by
        rw [hf, hf, div_le_div_iff₀ (pow_pos (hZ u) _) (pow_pos (hZ t) _)]
        constructor
        · intro h
          by_contra! hh
          exact ((mul_lt_mul_iff_of_pos_left hC).2
            (pow_lt_pow_left₀ hh (hZ u).le hn.ne')).not_ge h
        · intro h
          exact mul_le_mul_of_nonneg_left (pow_le_pow_left₀ (hZ t).le h _) hC.le
      exact ⟨fun h u hu ↦ (he u).1 (h hu), fun h u hu ↦ (he u).2 (h hu)⟩
    obtain ⟨hE,hU⟩ := scalar_minimizers A s hA hs
    have hI : ((∃ k, s k < 0) ∧ (∃ k, 0 < s k)) ↔ Inner := by
      simp only [s, Inner, sub_neg, sub_pos]
    refine ⟨⟨?_,?_⟩,?_⟩
    · rintro ⟨t,ht⟩
      exact hI.mp (hE.mp ⟨t,(hiff t).mp ht⟩)
    · intro h
      obtain ⟨t,ht⟩ := hE.mpr (hI.mpr h)
      exact ⟨t,(hiff t).mpr ht⟩
    · intro h
      obtain ⟨t,ht⟩ := hE.mpr (hI.mpr h)
      exact ⟨t,(hiff t).mpr ht, fun u hu ↦ hU u t ((hiff u).mp hu) ht⟩

private theorem height_min {N : ℕ} (k : Fin (N+1)) :
    0 ≤ height N k ∧ (height N k = 0 ↔ k.val = 0 ∨ k.val = N) := by
  have hpos : 0 < N.choose k.val := Nat.choose_pos (by have := k.isLt; omega)
  have hr : 0 < (N.choose k.val : ℝ) := Nat.cast_pos.mpr hpos
  refine ⟨log_nonneg (by exact_mod_cast hpos), ?_⟩
  unfold height
  have he : log (N.choose k.val : ℝ) = log 1 ↔ (N.choose k.val : ℝ) = 1 :=
    ⟨log_injOn_pos (show (N.choose k.val : ℝ) ∈ Ioi 0 from hr) zero_lt_one,
      congrArg log⟩
  rw [log_one] at he
  rw [he, Nat.cast_eq_one, Nat.choose_eq_one_iff]
  exact or_congr Iff.rfl eq_comm

private theorem height_max {N : ℕ} (k : Fin (N+1)) :
    let c := N/2
    let H := log (N.choose c : ℝ)
    height N k ≤ H ∧ (height N k = H ↔ k.val = c ∨ k.val = N-c) := by
  intro c H
  let g (a : ℕ) := log (N.choose a : ℝ)
  have hM : StrictMonoOn g (Iic c) := by
    apply strictMonoOn_of_lt_succ ordConnected_Iic
    intro a _ ha ha'
    have hs : a+1 ≤ c := ha'
    have hs' : a+1 ≤ N := by dsimp [c] at hs; omega
    have hlt : a+1 < N-a := by dsimp [c] at hs; omega
    have hc₀ : 0 < (N.choose a : ℝ) := Nat.cast_pos.mpr (Nat.choose_pos (by omega))
    have he : (N.choose (a+1) : ℝ)*((a : ℝ)+1) =
        (N.choose a : ℝ)*(N-a : ℕ) := by exact_mod_cast Nat.choose_succ_right_eq N a
    change log (N.choose a : ℝ) < log (N.choose (a+1) : ℝ)
    apply log_lt_log hc₀
    apply (mul_lt_mul_iff_of_pos_right (show 0 < (a : ℝ)+1 by positivity)).mp
    rw [he]
    exact mul_lt_mul_of_pos_left (by exact_mod_cast hlt) hc₀
  let j := min k.val (N-k.val)
  have hk : k.val ≤ N := by have := k.isLt; omega
  have hj : j ≤ c := by dsimp [j,c]; omega
  have hg : g j = height N k := by
    by_cases h : k.val ≤ N-k.val
    · simp [g,j,min_eq_left h,height]
    · simp [g,j,min_eq_right (le_of_not_ge h),Nat.choose_symm hk,height]
  have hiff : g j = H ↔ j = c := by
    constructor
    · intro h
      exact hM.injOn (show j ∈ Iic c from hj) (mem_Iic.mpr le_rfl) h
    · intro h; rw [h]
  rw [← hg]
  exact ⟨hM.monotoneOn (show j ∈ Iic c from hj) (mem_Iic.mpr le_rfl) hj,
    hiff.trans (by dsimp [j,c]; omega)⟩

/-- At either extremal empirical height, the signed dispersion limit is
the law with the held odds restricted to cells at that height. The resulting
likelihood is a strict, unattained supremum. -/
theorem held_probability_size_failure {N n : ℕ} (hn : 0 < n) (hN : 2 ≤ N)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Ioo 0 1) :
    let m := (∑ i, height N (x i))/(n : ℝ)
    let F := Finset.univ.filter (fun k : Fin (N+1) ↦ height N k = m)
    let η := log (p/(1-p))
    let v := ∏ i, exp (η*(x i).val)/(∑ j ∈ F, exp (η*j.val))
    let f := closedLikelihood x p
    ((∀ k, height N k ≤ m) →
      (∀ k : Fin (N+1), k ∈ F ↔ k.val = N/2 ∨ k.val = N-N/2) ∧
      Tendsto f atTop (𝓝 v) ∧ IsLUB (range f) v ∧ ∀ t, f t < v) ∧
    ((∀ k, m ≤ height N k) →
      (∀ k : Fin (N+1), k ∈ F ↔ k.val = 0 ∨ k.val = N) ∧
      Tendsto (fun t ↦ f (-t)) atTop (𝓝 v) ∧ IsLUB (range f) v ∧ ∀ t, f t < v) := by
  classical
  intro m F η v f
  have hnonflat : ∃ k : Fin (N+1), height N k ≠ m := by
    by_contra! he
    have h0 := he 0
    have h1 := he (⟨1, by omega⟩ : Fin (N+1))
    simp [height] at h0 h1
    have hlog : 0 < log (N : ℝ) := log_pos
      (by exact_mod_cast (show 1 < N by omega))
    linarith
  have hface (d : ℝ) (hd : d ≠ 0) (hside : ∀ k, d*(height N k-m) ≤ 0) :
      Tendsto (fun t ↦ f (d*t)) atTop (𝓝 v) ∧
        IsLUB (range f) v ∧ ∀ t, f t < v := by
    let base (k : Fin (N+1)) := η*k.val
    let slope (k : Fin (N+1)) := d*(height N k-m)
    have hsum : (∑ i, slope (x i)) = 0 := by
      have he : (n : ℝ)*m = ∑ i, height N (x i) := by
        dsimp [m]
        field_simp [show (n : ℝ) ≠ 0 from (Nat.cast_pos.mpr hn).ne']
      simp only [slope, ← Finset.mul_sum, Finset.sum_sub_distrib, Finset.sum_const,
        Finset.card_univ, Fintype.card_fin, nsmul_eq_mul, he, sub_self, mul_zero]
    have hx (i : Fin n) : x i ∈ F := by
      have H : ∀ i : Fin n, -(slope (x i)) = 0 := by
        intro i
        apply (Finset.sum_eq_zero_iff_of_nonneg (fun i _ ↦ neg_nonneg.mpr (hside (x i)))).mp
          (by rw [Finset.sum_neg_distrib, hsum, neg_zero]) i (Finset.mem_univ i)
      simpa [F, slope, hd, sub_eq_zero] using H i
    have hzero (k : Fin (N+1)) (hk : k ∈ F) : slope k = 0 := by
      have he : height N k = m := by simpa [F] using hk
      simp [slope, he]
    have hneg (k : Fin (N+1)) (hk : k ∉ F) : slope k < 0 := by
      apply lt_of_le_of_ne (hside k)
      simpa [F, slope, hd, sub_eq_zero] using hk
    have hout : ∃ k, k ∉ F := by simpa [F] using hnonflat
    have hEq (t : ℝ) : (∏ i, rayMass base slope t (x i)) = f (d*t) := by
      have hW (k : Fin (N+1)) : exp (base k+t*slope k) =
          exp (-(t*d*m))*weight N η (d*t) k := by
        rw [weight, ← exp_add]
        congr 1
        dsimp [base,slope]
        ring
      simp only [f, closedLikelihood, hp.1.ne', hp.2.ne, ite_false]
      change _ = likelihood x η (d*t)
      simp only [rayMass, hW, ← Finset.mul_sum,
        mul_div_mul_left _ _ (exp_pos _).ne', likelihood]
    have hlim : Tendsto (fun t ↦ ∏ i, rayMass base slope t (x i)) atTop (𝓝 v) :=
      tendsto_finsetProd _ (fun i _ ↦ exposed_mass base slope F (x i) (hx i) hzero hneg)
    have hstrict (t : ℝ) : f t < v := by
      have H := (exposed_supremum hn base slope F x hx hzero hneg hout).2 (t/d)
      rw [hEq, mul_div_cancel₀ _ hd] at H
      exact H
    refine ⟨hlim.congr hEq, ⟨?_,?_⟩, hstrict⟩
    · rintro _ ⟨t,rfl⟩
      exact (hstrict t).le
    · intro w hw
      exact le_of_tendsto (hlim.congr hEq)
        (Eventually.of_forall fun t ↦ hw ⟨d*t,rfl⟩)
  constructor
  · intro h
    have hc : N/2 < N+1 := by omega
    let c : Fin (N+1) := ⟨N/2,hc⟩
    have hm : m ≤ height N c := by
      apply (div_le_iff₀ (Nat.cast_pos.mpr hn : 0 < (n : ℝ))).2
      have hh := Finset.sum_le_sum (fun i (_ : i ∈ Finset.univ) ↦ (height_max (x i)).1)
      simpa [c, height, mul_comm] using hh
    have he := le_antisymm hm (h c)
    refine ⟨fun k ↦ ?_, ?_⟩
    simpa [F, he, c, height] using (height_max k).2
    simpa only [one_mul] using hface 1 one_ne_zero (fun k ↦ by linarith [h k])
  · intro h
    have hm : 0 ≤ m := div_nonneg (Finset.sum_nonneg (fun i _ ↦ (height_min (x i)).1))
      (Nat.cast_nonneg n)
    have he : m = 0 := le_antisymm (by simpa [height] using h (0 : Fin (N+1))) hm
    refine ⟨fun k ↦ ?_, ?_⟩
    simpa [F, he] using (height_min k).2
    simpa only [neg_one_mul] using hface (-1) (by norm_num) (fun k ↦ by linarith [h k])

private theorem fiber_has_height {N n : ℕ} (hn : 0 < n)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ v, IsLUB (range (closedLikelihood x p)) v := by
  let f := closedLikelihood x p
  let m := (∑ i, height N (x i))/(n : ℝ)
  have h := held_probability_size_classification hn x hp
  have hmax (t : ℝ) (ht : IsMaxOn f univ t) : IsLUB (range f) (f t) := by
    refine ⟨?_, ?_⟩
    · rintro _ ⟨q,rfl⟩
      exact ht (mem_univ q)
    · intro v hv
      exact hv (mem_range_self t)
  by_cases hflat : p = 0 ∨ p = 1 ∨ N ≤ 1
  · exact ⟨f 0, hmax 0 (h.1 hflat 0)⟩
  have hp' : p ∈ Ioo 0 1 := ⟨lt_of_le_of_ne hp.1 (fun h ↦ hflat (Or.inl h.symm)),
    lt_of_le_of_ne hp.2 (fun h ↦ hflat (Or.inr (Or.inl h)))⟩
  have hN : 2 ≤ N := by omega
  by_cases hinner : (∃ k, height N k < m) ∧ (∃ k, m < height N k)
  · obtain ⟨t,ht⟩ := (h.2 ⟨hp',hN⟩).1.mpr hinner
    exact ⟨f t, hmax t ht⟩
  have H := held_probability_size_failure hn hN x hp'
  by_cases hlow : ∃ k, height N k < m
  · have hhigh : ∀ k, height N k ≤ m := by
      intro k
      exact le_of_not_gt (fun h' ↦ hinner ⟨hlow,⟨k,h'⟩⟩)
    exact ⟨_, (H.1 hhigh).2.2.1⟩
  · have hlow' : ∀ k, m ≤ height N k := fun k ↦ le_of_not_gt (fun h' ↦ hlow ⟨k,h'⟩)
    exact ⟨_, (H.2 hlow').2.2.1⟩

private theorem fiber_excluded_limit {N n : ℕ} (hn : 0 < n)
    (x : Fin n → Fin (N+1)) {p : ℝ} (hp : p ∈ Icc 0 1) {v : ℝ}
    (hv : IsLUB (range (closedLikelihood x p)) v)
    (hno : ¬ ∃ t, IsMaxOn (closedLikelihood x p) univ t) :
    Tendsto (closedLikelihood x p) atTop (𝓝 v) ∨
      Tendsto (fun t ↦ closedLikelihood x p (-t)) atTop (𝓝 v) := by
  let m := (∑ i, height N (x i))/(n : ℝ)
  have h := held_probability_size_classification hn x hp
  have hflat : ¬ (p = 0 ∨ p = 1 ∨ N ≤ 1) := fun h' ↦ hno ⟨0, h.1 h' 0⟩
  have hp' : p ∈ Ioo 0 1 := ⟨lt_of_le_of_ne hp.1 (fun h ↦ hflat (Or.inl h.symm)),
    lt_of_le_of_ne hp.2 (fun h ↦ hflat (Or.inr (Or.inl h)))⟩
  have hN : 2 ≤ N := by omega
  have hinner : ¬ ((∃ k, height N k < m) ∧ (∃ k, m < height N k)) :=
    fun h' ↦ hno ((h.2 ⟨hp', hN⟩).1.mpr h')
  have H := held_probability_size_failure hn hN x hp'
  by_cases hlow : ∃ k, height N k < m
  · have hhigh : ∀ k, height N k ≤ m := fun k ↦ le_of_not_gt
      (fun h' ↦ hinner ⟨hlow, ⟨k,h'⟩⟩)
    obtain ⟨_,hT,hS,_⟩ := H.1 hhigh
    exact Or.inl ((hS.unique hv) ▸ hT)
  · have hlow' : ∀ k, m ≤ height N k := fun k ↦ le_of_not_gt (fun h' ↦ hlow ⟨k,h'⟩)
    obtain ⟨_,hT,hS,_⟩ := H.2 hlow'
    exact Or.inr ((hS.unique hv) ▸ hT)

/-- The capped probability-hold comparison retains every attained
profile at the largest height, including equality with an excluded
profile and all dispersion values in a flat winning fiber. -/
theorem held_probability_capped {n B : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) {p : ℝ} (hp : p ∈ Icc 0 1) :
    let D : Set ℕ := {N | N ≤ B ∧ Discrete4CMB.Compatible x N p}
    let L := fun q : ℕ × ℝ ↦ Discrete4CMB.sizeLikelihood x q.1 p q.2
    let Att := fun N : ℕ ↦ ∃ t, IsMaxOn (fun u ↦ L (N,u)) univ t
    D.Nonempty →
    ∃ H : ℕ → ℝ, ∃ V : ℝ,
      (∀ N ∈ D, IsLUB (range (fun t ↦ L (N,t))) (H N)) ∧
      IsLUB (L '' (D ×ˢ univ)) V ∧
      (∀ q ∈ D ×ˢ univ, IsMaxOn L (D ×ˢ univ) q ↔ L q = H q.1 ∧ H q.1 = V) ∧
      ((∃ q ∈ D ×ˢ univ, IsMaxOn L (D ×ˢ univ) q) ↔
        ∃ N ∈ D, Att N ∧ (∀ J ∈ D, ¬ Att J → H J ≤ H N)) ∧
      ((∀ N ∈ D, Att N → H N < V) →
        ∃ N ∈ D, H N = V ∧ ¬ Att N ∧
          (Tendsto (fun t ↦ L (N,t)) atTop (𝓝 V) ∨
            Tendsto (fun t ↦ L (N,-t)) atTop (𝓝 V))) := by
  classical
  intro D L Att hD
  have hH (N : ℕ) : ∃ v, N ∈ D → IsLUB (range (fun t ↦ L (N,t))) v := by
    by_cases hN : N ∈ D
    · let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hN.2.1 i)⟩
      obtain ⟨v,hv⟩ := fiber_has_height hn y hp
      refine ⟨v, fun _ ↦ ?_⟩
      have he : (fun t ↦ L (N,t)) = closedLikelihood y p := by
        funext t
        simp [L, Discrete4CMB.sizeLikelihood, hN.2.1, y]
      rwa [he]
    · exact ⟨0, fun h ↦ (hN h).elim⟩
  choose H hH using hH
  let grid := (Finset.range (B+1)).filter fun N ↦ Discrete4CMB.Compatible x N p
  have he (N : ℕ) : N ∈ grid ↔ N ∈ D := by simp [grid,D]
  have hgrid : grid.Nonempty := by obtain ⟨N,hN⟩ := hD; exact ⟨N,(he N).mpr hN⟩
  obtain ⟨N,hN,hM⟩ := grid.exists_max_image H hgrid
  have hND := (he N).mp hN
  let V := H N
  have hV : IsLUB (L '' (D ×ˢ univ)) V := by
    refine ⟨?_,?_⟩
    · rintro _ ⟨⟨J,t⟩,hJ,rfl⟩
      exact ((hH J hJ.1).1 (mem_range_self t)).trans (hM J ((he J).mpr hJ.1))
    · intro v hv
      apply (hH N hND).2
      rintro _ ⟨t,rfl⟩
      exact hv ⟨(N,t),⟨hND,mem_univ t⟩,rfl⟩
  have hiff (q : ℕ × ℝ) (hq : q ∈ D ×ˢ univ) :
      IsMaxOn L (D ×ˢ univ) q ↔ L q = H q.1 ∧ H q.1 = V := by
    have h₁ := (hH q.1 hq.1).1 (mem_range_self q.2)
    have h₂ := hM q.1 ((he _).mpr hq.1)
    constructor
    · intro hm
      have hv : V ≤ L q := hV.2 (by rintro _ ⟨r,hr,rfl⟩; exact hm hr)
      exact ⟨le_antisymm h₁ (h₂.trans hv), le_antisymm h₂ (hv.trans h₁)⟩
    · intro ⟨h,h'⟩ r hr
      have hv := hV.1 (mem_image_of_mem L hr)
      exact hv.trans_eq (h.trans h').symm
  refine ⟨H,V,hH,hV,hiff,?_,?_⟩
  swap
  · intro hstrict
    have hno : ¬ Att N := fun hA ↦ (hstrict N hND hA).ne rfl
    refine ⟨N,hND,rfl,hno,?_⟩
    let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hND.2.1 i)⟩
    have heq : (fun t ↦ L (N,t)) = closedLikelihood y p := by
      funext t
      simp [L, Discrete4CMB.sizeLikelihood, hND.2.1, y]
    have hno' : ¬ ∃ t, IsMaxOn (closedLikelihood y p) univ t := by
      simpa only [Att, heq] using hno
    have hV' : IsLUB (range (closedLikelihood y p)) V := by
      simpa only [heq] using hH N hND
    have hlim := fiber_excluded_limit hn y hp hV' hno'
    rw [heq]
    exact hlim.imp id (fun h ↦ h.congr fun t ↦ (congrFun heq (-t)).symm)
  constructor
  · rintro ⟨q,hq,hm⟩
    obtain ⟨hq₁,hq₂⟩ := (hiff q hq).mp hm
    refine ⟨q.1,hq.1,⟨q.2,fun t _ ↦ ?_⟩,fun J hJ _ ↦ ?_⟩
    · exact ((hH q.1 hq.1).1 (mem_range_self t)).trans_eq hq₁.symm
    · exact (hM J ((he J).mpr hJ)).trans_eq hq₂.symm
  · rintro ⟨J,hJ,hA,hdom⟩
    have Hatt {K : ℕ} (hK : K ∈ D) (hA : Att K) (hv : H K = V) :
        ∃ q ∈ D ×ˢ univ, IsMaxOn L (D ×ˢ univ) q := by
      obtain ⟨t,ht⟩ := hA
      have heq : L (K,t) = H K := le_antisymm ((hH K hK).1 (mem_range_self t))
        ((hH K hK).2 (by rintro _ ⟨u,rfl⟩; exact ht (mem_univ u)))
      exact ⟨(K,t),⟨hK,mem_univ t⟩,(hiff (K,t) ⟨hK,mem_univ t⟩).2 ⟨heq,hv⟩⟩
    by_cases hAN : Att N
    · exact Hatt hND hAN rfl
    · exact Hatt hJ hA (le_antisymm (hM J ((he J).mpr hJ)) (hdom N hND hAN))

end UnivariateML.Base8CMBHeldProbability

end
