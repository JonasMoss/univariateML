module

public import UnivariateML.Family.StudentFaces
public import Mathlib.Topology.Order.Compact

import Mathlib.Tactic

set_option linter.style.header false

/-! # Joint Student attainment at held shape -/

@[expose] public section

open Set Real

namespace UnivariateML.StudentHeldShape

noncomputable section

variable {n : ℕ}

private theorem uniform_gap (x : Fin n → ℝ) (hn : 0 < n) :
    ∃ d : ℝ, 0 < d ∧ ∀ i j, x i ≠ x j → 2*d ≤ |x i-x j| := by
  let g (p : Fin n × Fin n) :=
    if x p.1 = x p.2 then 1 else |x p.1-x p.2|/2
  let i : Fin n := ⟨0, hn⟩
  obtain ⟨p, _, hp⟩ := Finset.exists_min_image Finset.univ g
    ⟨(i,i), Finset.mem_univ _⟩
  refine ⟨g p, ?_, ?_⟩
  · dsimp [g]
    split_ifs with he
    · norm_num
    · exact half_pos (abs_pos.mpr (sub_ne_zero.mpr he))
  · intro j k hjk
    have h := hp (j,k) (Finset.mem_univ _)
    simp only [g, ite_eq_right hjk] at h
    linarith

private theorem log_kernel_nonneg {ν a : ℝ} (hν : 0 < ν) (ha : 0 < a) (r : ℝ) :
    0 ≤ log (1+r^2/(ν*a^2)) :=
  log_nonneg (le_add_of_nonneg_right (by positivity))

private theorem logLik_expand (x : Fin n → ℝ) (μ ν a : ℝ) :
    TDist.logLik x μ ν a =
      n*(TDist.logNormalizer ν - log a) -
      (ν+1)/2 * ∑ i, log (1+(x i-μ)^2/(ν*a^2)) := by
  simp [TDist.logLik, Finset.sum_sub_distrib, Finset.mul_sum, mul_sub]

private theorem small_scale_bound (x : Fin n → ℝ) {ν a d m b e : ℝ}
    (hν : 0 < ν) (ha : 0 < a) (ha1 : a ≤ 1) (hd : 0 < d)
    (hE : e ≤ TDist.exponent x b ν)
    (hfar : ∀ i, x i ≠ b → d ≤ |x i - m|) :
    TDist.logLik x m ν a ≤
      n*TDist.logNormalizer ν + (ν+1)/2*n*|log (d^2/ν)| + e*log a := by
  let U := Finset.univ.filter (fun i ↦ x i ≠ b)
  let j (i : Fin n) := log (1+(x i-m)^2/(ν*a^2))
  have hcount : (U.card : ℝ) = (n : ℝ) - TDist.multiplicity x b := by
    have h : TDist.multiplicity x b + U.card = n := by
      unfold TDist.multiplicity U
      rw [Finset.card_filter_add_card_filter_not, Finset.card_univ, Fintype.card_fin]
    have h' : (TDist.multiplicity x b : ℝ) + U.card = n := by exact_mod_cast h
    linarith
  have hterm (i : Fin n) (hi : i ∈ U) : log (d^2/ν) - 2*log a ≤ j i := by
    have hd2 := (sq_le_sq₀ hd.le (abs_nonneg _)).2 (hfar i (Finset.mem_filter.mp hi).2)
    rw [sq_abs] at hd2
    have hlog : log (a^2) = 2 * log a := by rw [log_pow]; norm_num
    rw [← hlog, ← log_div (by positivity : d^2/ν ≠ 0) (by positivity : a^2 ≠ 0)]
    unfold j
    apply log_le_log (by positivity)
    rw [div_div]
    exact ((div_le_div_iff_of_pos_right (show 0 < ν*a^2 by positivity)).2 hd2).trans
      (by linarith)
  have hs : (U.card : ℝ) * (log (d^2/ν) - 2*log a) ≤ ∑ i, j i := by
    have h := Finset.sum_le_sum hterm
    rw [Finset.sum_const, nsmul_eq_mul] at h
    exact h.trans (Finset.sum_le_sum_of_subset_of_nonneg
      (Finset.filter_subset _ _) (fun i _ _ ↦ log_kernel_nonneg hν ha _))
  let w := (ν+1)/2
  have hw : 0 ≤ w := by dsimp [w]; positivity
  have h1 := mul_le_mul_of_nonneg_left hs hw
  have h2 := mul_le_mul_of_nonpos_right hE (log_nonpos ha.le ha1)
  have hu : (U.card : ℝ) ≤ n := by
    rw [hcount]
    exact sub_le_self _ (by positivity)
  have h3 := mul_le_mul_of_nonneg_right hu (abs_nonneg (log (d^2/ν)))
  have h4 := mul_le_mul_of_nonneg_left (le_abs_self (-log (d^2/ν)))
    (show 0 ≤ (U.card : ℝ) by positivity)
  rw [abs_neg] at h4
  have h5 := mul_le_mul_of_nonneg_left (h4.trans h3) hw
  have h6 := congrArg (fun y : ℝ ↦ (ν+1)*y*log a) hcount
  rw [logLik_expand]
  change (n : ℝ)*(TDist.logNormalizer ν - log a) - w*(∑ i, j i) ≤ _
  unfold TDist.exponent at h2
  dsimp [w] at h1 h5 ⊢
  nlinarith only [h1,h2,h5,h6]

private theorem logLik_continuousOn (x : Fin n → ℝ) {ν : ℝ} (hν : 0 < ν) :
    ContinuousOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 ν p.2) (univ ×ˢ Ioi 0) := by
  apply continuousOn_finsetSum
  intro i _
  refine (continuousOn_const.sub (continuous_snd.continuousOn.log ?_)).sub
    (continuousOn_const.mul (ContinuousOn.log ?_ ?_))
  · exact fun p hp ↦ (show 0 < p.2 from hp.2).ne'
  · apply continuousOn_const.add
    exact (by fun_prop : Continuous (fun p : ℝ × ℝ ↦ (x i-p.1)^2)).continuousOn.div
      (by fun_prop) (fun p hp ↦ mul_ne_zero hν.ne' (pow_ne_zero 2 (show 0 < p.2 from hp.2).ne'))
  · intro p hp
    have hs : 0 < p.2 := hp.2
    positivity

private theorem clip_location (x : Fin n → ℝ) {ν a M : ℝ} (hν : 0 < ν) (ha : 0 < a)
    (hM : 0 < M) (hX : ∀ i, |x i| ≤ M) (m : ℝ) :
    let u := max (-M) (min M m)
    u ∈ Icc (-M) M ∧ TDist.logLik x m ν a ≤ TDist.logLik x u ν a := by
  dsimp
  refine ⟨⟨le_max_left _ _, max_le (by linarith) (min_le_left _ _)⟩, ?_⟩
  have hd (i : Fin n) : (x i - max (-M) (min M m))^2 ≤ (x i-m)^2 := by
    obtain ⟨hl,hr⟩ := abs_le.mp (hX i)
    by_cases h : m < -M
    · rw [min_eq_right (by linarith), max_eq_left h.le]
      apply (sq_le_sq₀ (by linarith : 0 ≤ x i - -M) (by linarith : 0 ≤ x i-m)).mpr
      linarith
    · by_cases h' : M < m
      · rw [min_eq_left h'.le, max_eq_right (by linarith)]
        nlinarith [mul_nonneg (sub_nonneg.mpr h'.le) (show 0 ≤ m+M-2*x i by linarith)]
      · rw [min_eq_right (le_of_not_gt h'), max_eq_right (le_of_not_gt h)]
  rw [logLik_expand, logLik_expand]
  apply sub_le_sub_left
  apply mul_le_mul_of_nonneg_left _ (by positivity)
  apply Finset.sum_le_sum
  intro i _
  apply log_le_log (by positivity)
  exact add_le_add_right ((div_le_div_iff_of_pos_right (show 0 < ν*a^2 by positivity)).2 (hd i)) 1

set_option maxHeartbeats 800000 in
-- The scale estimates and compact rectangle elaborate together.
/-- At any positive held df, strictly positive exponents at all sample
atoms give an attained joint location-scale maximum. -/
theorem lst_strict_atoms_attain (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν)
    (hE : ∀ i, 0 < TDist.exponent x (x i) ν) :
    ∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun z : ℝ × ℝ ↦ TDist.logLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p := by
  let i : Fin n := ⟨0,hn⟩
  obtain ⟨j, _, hj⟩ := Finset.exists_min_image Finset.univ (fun j ↦ TDist.exponent x (x j) ν)
    ⟨i, Finset.mem_univ _⟩
  let e := TDist.exponent x (x j) ν
  have he : 0 < e := hE j
  obtain ⟨d, hd, hgap⟩ := uniform_gap x hn
  let C := n*TDist.logNormalizer ν + (ν+1)/2*n*|log (d^2/ν)|
  let T := TDist.logLik x 0 ν 1
  have small (m : ℝ) {a : ℝ} (ha : 0 < a) (ha1 : a ≤ 1) :
      TDist.logLik x m ν a ≤ C + e*log a := by
    by_cases hnear : ∃ k, |x k-m| < d
    · obtain ⟨k,hk⟩ := hnear
      apply small_scale_bound x hν ha ha1 hd (hj k (Finset.mem_univ _))
      intro j' hj'
      have h := abs_add_le (x j'-m) (m-x k)
      rw [sub_add_sub_cancel, abs_sub_comm m (x k)] at h
      linarith [hgap j' k hj']
    · push Not at hnear
      exact small_scale_bound x hν ha ha1 hd (hj i (Finset.mem_univ _))
        (fun j _ ↦ hnear j)
  let ε := exp (min 0 ((T-C-1)/e))
  let B := exp (max 0 (((n : ℝ)*TDist.logNormalizer ν-T+1)/n))
  let M : ℝ := 1 + ∑ i, |x i|
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hM : 0 < M := add_pos_of_pos_of_nonneg one_pos (Finset.sum_nonneg fun _ _ ↦ abs_nonneg _)
  have hX (i : Fin n) : |x i| ≤ M := by
    have h := Finset.single_le_sum (fun j _ ↦ abs_nonneg (x j)) (Finset.mem_univ i)
    dsimp [M]
    linarith
  have hε : 0 < ε := exp_pos _
  have hε1 : ε ≤ 1 := by rw [← exp_zero]; exact exp_le_exp.mpr (min_le_left _ _)
  have hB : 1 ≤ B := by rw [← exp_zero]; exact exp_le_exp.mpr (le_max_left _ _)
  let K := Icc (-M) M ×ˢ Icc ε B
  have hK : IsCompact K := isCompact_Icc.prod isCompact_Icc
  have hKsub : K ⊆ univ ×ˢ Ioi 0 := fun z hz ↦ ⟨mem_univ _, hε.trans_le hz.2.1⟩
  have hRef : (0, (1 : ℝ)) ∈ K := ⟨⟨by linarith, hM.le⟩, ⟨hε1,hB⟩⟩
  obtain ⟨p,hp,hmax⟩ := hK.exists_isMaxOn ⟨_,hRef⟩ ((logLik_continuousOn x hν).mono hKsub)
  have hT : T ≤ TDist.logLik x p.1 ν p.2 := hmax hRef
  refine ⟨p, hKsub hp, ?_⟩
  intro z hz
  change TDist.logLik x z.1 ν z.2 ≤ TDist.logLik x p.1 ν p.2
  have ha : 0 < z.2 := hz.2
  by_cases hlow : z.2 < ε
  · apply le_trans _ hT
    have hs := small z.1 ha (hlow.le.trans hε1)
    have hl := log_lt_log ha hlow
    rw [show log ε = min 0 ((T-C-1)/e) from log_exp _] at hl
    have h := (lt_div_iff₀ he).mp (hl.trans_le (min_le_right _ _))
    nlinarith only [h, hs]
  by_cases hhigh : B < z.2
  · apply le_trans _ hT
    have h := log_lt_log (lt_of_lt_of_le one_pos hB) hhigh
    rw [show log B = max 0 (((n : ℝ)*TDist.logNormalizer ν-T+1)/n) from log_exp _] at h
    have h' := (div_lt_iff₀ hn').mp ((le_max_right _ _).trans_lt h)
    rw [logLik_expand]
    have hS : 0 ≤ ∑ i, log (1+(x i-z.1)^2/(ν*z.2^2)) :=
      Finset.sum_nonneg fun _ _ ↦ log_kernel_nonneg hν ha _
    nlinarith only [h', mul_nonneg (show 0 ≤ (ν+1)/2 by positivity) hS]
  obtain ⟨hum, hle⟩ := clip_location x hν ha hM hX z.1
  exact hle.trans (hmax (show (max (-M) (min M z.1), z.2) ∈ K from
    ⟨hum, ⟨le_of_not_gt hlow, le_of_not_gt hhigh⟩⟩))

/-- At held df above two, the standard-deviation Student family has a
joint maximum exactly when the exponent at every sample atom is positive. -/
theorem std_held_df_attains_iff (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 2 < ν) :
    (∃ p ∈ (univ ×ˢ Ioi (0 : ℝ)),
      IsMaxOn (fun z : ℝ × ℝ ↦ TDist.stdLogLik x z.1 ν z.2) (univ ×ˢ Ioi 0) p) ↔
      ∀ i, 0 < TDist.exponent x (x i) ν := by
  have hv : 0 < ν := by linarith
  let c := sqrt ((ν-2)/ν)
  have hc : 0 < c := sqrt_pos.mpr (div_pos (sub_pos.mpr hν) hv)
  have hL (m s : ℝ) (hs : 0 < s) :
      TDist.stdLogLik x m ν s = TDist.logLik x m ν (s*c) := TDist.stdLogLik_eq x m hν hs
  have hE (m a : ℝ) (ha : 0 < a) :
      TDist.stdLogLik x m ν (a/c) = TDist.logLik x m ν a := by
    rw [hL m _ (div_pos ha hc), div_mul_cancel₀ _ hc.ne']
  change _ ↔ _
  constructor
  · rintro ⟨p,hp,hm⟩
    have hs : 0 < p.2 := hp.2
    have ha := mul_pos hs hc
    have H (m a : ℝ) (ha' : 0 < a) :
        TDist.likelihood x m ν a ≤ TDist.likelihood x p.1 ν (p.2*c) := by
      have h := hm (show (m,a/c) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _, div_pos ha' hc⟩)
      change TDist.stdLogLik x m ν (a/c) ≤ TDist.stdLogLik x p.1 ν p.2 at h
      rw [hE m a ha', hL p.1 p.2 hs] at h
      exact exp_le_exp.mpr h
    intro i
    by_contra! he
    rcases lt_or_eq_of_le he with hneg | hzero
    · have h := TDist.joint_unbounded x hv ⟨i,hneg⟩
      refine h ⟨TDist.likelihood x p.1 ν (p.2*c), ?_⟩
      rintro _ ⟨q,hq,rfl⟩
      exact H q.1 q.2 hq.2
    · obtain ⟨hSup,hstrict⟩ := StudentFaces.lst_critical_joint x hn (by linarith : 1 < ν) hzero
      have h := hSup.2 (show TDist.likelihood x p.1 ν (p.2*c) ∈ _ from by
        rintro _ ⟨q,hq,rfl⟩
        exact H q.1 q.2 hq.2)
      exact (hstrict p.1 (p.2*c) ha).not_ge h
  · intro hexp
    obtain ⟨p,hp,hm⟩ := lst_strict_atoms_attain x hn hv hexp
    have ha : 0 < p.2 := hp.2
    refine ⟨(p.1,p.2/c), ⟨mem_univ _,div_pos ha hc⟩, ?_⟩
    intro q hq
    have hs : 0 < q.2 := hq.2
    change TDist.stdLogLik x q.1 ν q.2 ≤ TDist.stdLogLik x p.1 ν (p.2/c)
    rw [hL q.1 q.2 hs, hE p.1 p.2 ha]
    exact hm (show (q.1,q.2*c) ∈ univ ×ˢ Ioi 0 from ⟨mem_univ _,mul_pos hs hc⟩)

/-- At held df above two and a critical atom, the joint supremum in the
standard-deviation coordinate is finite and every positive finite fit is worse. -/
theorem std_critical_joint (x : Fin n → ℝ) (hn : 0 < n) {b ν : ℝ}
    (hν : 2 < ν) (he : TDist.exponent x b ν = 0) :
    IsLUB ((fun z : ℝ × ℝ ↦ exp (TDist.stdLogLik x z.1 ν z.2)) '' (univ ×ˢ Ioi 0))
      (exp (TDist.collapseLogValue x b ν)) ∧
    ∀ m σ : ℝ, 0 < σ → exp (TDist.stdLogLik x m ν σ) < exp (TDist.collapseLogValue x b ν) := by
  obtain ⟨h,hstrict⟩ := StudentFaces.lst_critical_joint x hn (by linarith : 1 < ν) he
  let c := sqrt ((ν-2)/ν)
  have hc : 0 < c := sqrt_pos.mpr (div_pos (by linarith) (by linarith))
  have hs (m σ : ℝ) (hσ : 0 < σ) := hstrict m (σ*c) (mul_pos hσ hc)
  refine ⟨?_, fun m σ hσ ↦ ?_⟩
  · convert h using 1
    ext z
    constructor
    · rintro ⟨p,hp,rfl⟩
      exact ⟨(p.1,p.2*c), ⟨mem_univ _, mul_pos hp.2 hc⟩, by
        change TDist.likelihood x p.1 ν (p.2*c) = exp (TDist.stdLogLik x p.1 ν p.2)
        rw [TDist.stdLogLik_eq x p.1 hν hp.2]; rfl⟩
    · rintro ⟨p,hp,rfl⟩
      have ha : 0 < p.2/c := div_pos hp.2 hc
      refine ⟨(p.1,p.2/c), ⟨mem_univ _, ha⟩, ?_⟩
      change exp (TDist.stdLogLik x p.1 ν (p.2/c)) = TDist.likelihood x p.1 ν p.2
      rw [TDist.stdLogLik_eq x p.1 hν ha]
      change exp (TDist.logLik x p.1 ν (p.2/c*c)) = _
      rw [div_mul_cancel₀ _ hc.ne']
      rfl
  · rw [TDist.stdLogLik_eq x m hν hσ]
    exact hs m σ hσ

end
end UnivariateML.StudentHeldShape
end
