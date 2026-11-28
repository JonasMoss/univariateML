module

public import UnivariateML.Family.Discrete5CMP
public import UnivariateML.Family.Trunc3CMP

/-! Sharp heights of point and adjacent faces on the positive CMP ray. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc5CMPFaces

noncomputable section

/-- Zero-truncated CMP probability at positive count `k` in the log-rate chart. -/
def positiveMass (p : ℝ × ℝ) (k : ℕ) : ℝ :=
  CMP.weight p.1 p.2 k / (∑' j : ℕ, CMP.weight p.1 p.2 (j+1))

/-- The positive sample likelihood in the log-rate chart. -/
def positiveLikelihood {n : ℕ} (x : Fin n → ℕ) (p : ℝ × ℝ) : ℝ :=
  ∏ i, positiveMass p (x i)

private theorem parent_transfer {p : ℝ × ℝ} (hp : p ∈ CMP.domain) (k : ℕ) :
    let g := fun j ↦ CMP.likelihood (fun _ : Fin 1 ↦ j) (exp p.1) p.2
    positiveMass p k = g k / (1-g 0) := by
  intro g
  have hs : Summable (CMP.weight p.1 p.2) := CMP.summable_weight hp
  have hpos : 0 < ∑' j : ℕ, CMP.weight p.1 p.2 (j+1) :=
    (exp_pos _).trans_le (((summable_nat_add_iff 1).mpr hs).le_tsum 0
      (fun _ _ ↦ (exp_pos _).le))
  have hZ := hs.sum_add_tsum_nat_add 1
  simp only [Finset.sum_range_one, CMP.weight, Nat.cast_zero, CMP.logFactorial,
    Nat.factorial_zero, Nat.cast_one, log_one, mul_zero, sub_zero, exp_zero] at hZ
  have htot : 0 < ∑' j : ℕ, CMP.weight p.1 p.2 j := by
    change 1 + (∑' j : ℕ, CMP.weight p.1 p.2 (j+1)) =
      (∑' j : ℕ, CMP.weight p.1 p.2 j) at hZ
    rw [← hZ]
    positivity
  dsimp [positiveMass, g]
  simp only [CMP.likelihood, ite_eq_right (exp_pos _).ne', log_exp, Fin.prod_univ_one]
  have hz : CMP.weight p.1 p.2 0 = 1 := by simp [CMP.weight, CMP.logFactorial]
  rw [hz]
  rw [show (1 - 1 / (∑' j : ℕ, CMP.weight p.1 p.2 j)) =
    (∑' j : ℕ, CMP.weight p.1 p.2 (j+1)) / (∑' j : ℕ, CMP.weight p.1 p.2 j) by
      rw [sub_div' htot.ne']
      change 1 + (∑' j : ℕ, CMP.weight p.1 p.2 (j+1)) =
        (∑' j : ℕ, CMP.weight p.1 p.2 j) at hZ
      rw [← hZ, one_mul]
      congr 1
      ring]
  exact (div_div_div_cancel_right₀ htot.ne' _ _).symm

private theorem transfer_path (a : ℝ → ℝ × ℝ) (ha : ∀ᶠ t in atTop, a t ∈ CMP.domain)
    {k : ℕ} {v : ℝ}
    (h0 : Tendsto (fun t ↦ CMP.likelihood (fun _ : Fin 1 ↦ 0)
      (exp (a t).1) (a t).2) atTop (𝓝 0))
    (hk : Tendsto (fun t ↦ CMP.likelihood (fun _ : Fin 1 ↦ k)
      (exp (a t).1) (a t).2) atTop (𝓝 v)) :
    Tendsto (fun t ↦ positiveMass (a t) k) atTop (𝓝 v) := by
  have H := hk.div (tendsto_const_nhds.sub h0) (show (1:ℝ)-0 ≠ 0 by norm_num)
  have H' := H.congr' (ha.mono fun t ht ↦ (parent_transfer ht k).symm)
  simpa only [sub_zero, div_one] using H'

private theorem ray_params (η s : ℝ) :
    ∀ᶠ t : ℝ in atTop, (η+t*s,1+t) ∈ CMP.domain :=
  (eventually_ge_atTop (0 : ℝ)).mono fun t ht ↦ Or.inl (by linarith)

private theorem adjacent_recovery {c n : ℕ} (hc : 0 < c) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) {q : ℝ} (hq : q ∈ Ioo 0 1) :
    let s := log ((c : ℝ)+1)
    let η := log (q/(1-q))+s
    Tendsto (fun t : ℝ ↦ positiveLikelihood x (η+t*s,1+t)) atTop
      (𝓝 (Discrete5CMP.adjacentHeight x c q)) := by
  intro s η
  have hs : CMP.logFactorial (c+1) = CMP.logFactorial c + s := by
    simp only [CMP.logFactorial, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ]
    rw [log_mul (by positivity) (by positivity), add_comm]
  have hrat : CMP.weight η 1 (c+1) = CMP.weight η 1 c * (q/(1-q)) := by
    have hp : 0 < q/(1-q) := div_pos hq.1 (sub_pos.mpr hq.2)
    rw [CMP.weight, CMP.weight, hs, ← exp_log hp, ← exp_add]
    congr 1
    push_cast
    dsimp [η]
    ring
  have hw : CMP.weight η 1 c ≠ 0 := (exp_pos _).ne'
  have hq' : 1-q ≠ 0 := (sub_pos.mpr hq.2).ne'
  have hf (i : Fin n) :
      (if x i ∈ ({c,c+1} : Finset ℕ) then CMP.weight η 1 (x i) else 0) /
        (CMP.weight η 1 c + CMP.weight η 1 (c+1)) = if x i = c then 1-q else q := by
    rcases hx i with he | he
    · simp only [he, Finset.mem_insert, true_or, ite_true, hrat]
      field_simp
      ring
    · simp only [he, Finset.mem_insert, Finset.mem_singleton, or_true, ite_true,
        show c+1 ≠ c by omega, ite_false, hrat]
      field_simp
      ring
  have h0 := Discrete4CMP.adjacent_path c η 0
  have hz : (0 : ℕ) ∉ ({c,c+1} : Finset ℕ) := by
    simp [hc.ne]
  simp only [hz, ite_false, zero_div] at h0
  apply tendsto_finsetProd Finset.univ
  intro i _
  exact transfer_path (fun t ↦ (η+t*s,1+t)) (ray_params η s) h0
    (by simpa only [hf i] using Discrete4CMP.adjacent_path c η (x i))

/-- CMP, all free, `zero:truncated`, failure on constant and adjacent
positive samples. Their exact supremum is the best two-cell likelihood,
and every finite fit is strictly below that height. -/
theorem positive_adjacent_supremum {n c : ℕ} (hn : 0 < n) (hc : 0 < c)
    (x : Fin n → ℕ) (hx : ∀ i, x i = c ∨ x i = c+1) :
    ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) q ∧
      IsLUB (positiveLikelihood x '' CMP.domain) (Discrete5CMP.adjacentHeight x c q) ∧
      ∀ p ∈ CMP.domain, positiveLikelihood x p < Discrete5CMP.adjacentHeight x c q := by
  let H := Discrete5CMP.adjacentHeight x c
  let L := positiveLikelihood x
  have hcont : Continuous H := by
    unfold H Discrete5CMP.adjacentHeight
    apply continuous_finsetProd
    intro i _
    by_cases he : x i = c
    · simp only [he, ite_true]
      fun_prop
    · simp only [ite_eq_right he]
      fun_prop
  obtain ⟨q,hq,hM⟩ := isCompact_Icc.exists_isMaxOn
    (nonempty_Icc.mpr (zero_le_one : (0 : ℝ) ≤ 1)) hcont.continuousOn
  have hupper (p : ℝ × ℝ) (hp : p ∈ CMP.domain) : L p < H q := by
    let w := CMP.weight p.1 p.2
    let a := w c
    let b := w (c+1)
    let Z := ∑' j : ℕ, w (j+1)
    have ha : 0 < a := exp_pos _
    have hb : 0 < b := exp_pos _
    have hab : 0 < a+b := add_pos ha hb
    let v := b/(a+b)
    have hv : v ∈ Icc (0 : ℝ) 1 :=
      ⟨div_nonneg hb.le hab.le,(div_le_one hab).mpr (by linarith)⟩
    have hs : a+b < Z := by
      let A : Finset ℕ := {c-1,c,c+1}
      have hsum := ((summable_nat_add_iff 1).mpr (CMP.summable_weight hp)).sum_le_tsum
        A (fun j _ ↦ show 0 ≤ w (j+1) from (exp_pos _).le)
      have hsA : (∑ j ∈ A, w (j+1)) = a+b+w (c+2) := by
        simp [A,Finset.sum_insert, show c-1 ≠ c by omega, show c-1 ≠ c+1 by omega,
          Nat.sub_add_cancel hc, a, b, add_assoc]
      rw [hsA] at hsum
      exact (lt_add_of_pos_right _ (exp_pos _)).trans_le hsum
    have hcell (i : Fin n) : w (x i)/(a+b) = if x i = c then 1-v else v := by
      rcases hx i with hi | hi
      · simp only [hi, ite_true]
        change a/(a+b) = 1-b/(a+b)
        field_simp
        ring
      · simp only [hi, ite_eq_right (show c+1 ≠ c by omega)]
        rfl
    suffices L p < H v from this.trans_le (hM hv)
    unfold L H positiveLikelihood Discrete5CMP.adjacentHeight
    simp_rw [← hcell]
    apply Finset.prod_lt_prod_of_nonempty₀
    · intro i _
      exact div_pos (exp_pos _) (hab.trans hs)
    · intro i _
      exact div_lt_div_of_pos_left (exp_pos _) hab hs
    · exact ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  refine ⟨q,hq,hM,⟨?_,?_⟩,hupper⟩
  · rintro _ ⟨p,hp,rfl⟩
    exact (hupper p hp).le
  · intro Y hY
    have hclosed : IsClosed {v | H v ≤ Y} := isClosed_le hcont continuous_const
    have hsub : Ioo (0 : ℝ) 1 ⊆ {v | H v ≤ Y} := by
      intro v hv
      let s := log ((c : ℝ)+1)
      let η := log (v/(1-v))+s
      apply le_of_tendsto (adjacent_recovery hc x hx hv)
      filter_upwards [ray_params η s] with t ht
      exact hY ⟨(η+t*s,1+t),ht,rfl⟩
    exact hclosed.closure_subset_iff.mpr hsub (by rwa [closure_Ioo zero_ne_one])

/-- CMP, all free, `zero:truncated`, failure. Every positive point mass
is reached by an all-free path, including each point winning at height one
on its constant sample. -/
theorem positive_point_path (c : ℕ) (hc : 0 < c) (k : ℕ) :
    let s := (log (c : ℝ)+log ((c : ℝ)+1))/2
    Tendsto (fun t : ℝ ↦ positiveMass (t*s,1+t) k) atTop
      (𝓝 (if k = c then 1 else 0)) := by
  intro s
  have h0 := Discrete4CMP.positive_point_path c hc 0
  simp only [hc.ne, ite_false] at h0
  apply transfer_path (fun t ↦ (t*s,1+t)) (by simpa only [zero_add] using ray_params 0 s)
    h0 (Discrete4CMP.positive_point_path c hc k)

private def partition (p : ℝ × ℝ) : ℝ :=
  ∑' j : ℕ, CMP.weight p.1 p.2 (j+1)

private theorem pos_sum {p : ℝ × ℝ} (hp : p ∈ CMP.domain) :
    Summable (fun j : ℕ ↦ CMP.weight p.1 p.2 (j+1)) :=
  (summable_nat_add_iff 1).mpr (CMP.summable_weight hp)

private theorem weight_le_partition {p : ℝ × ℝ} (hp : p ∈ CMP.domain) {k : ℕ}
    (hk : 0 < k) : CMP.weight p.1 p.2 k ≤ partition p := by
  simpa only [partition, Nat.sub_add_cancel hk] using
    (pos_sum hp).le_tsum (k-1) (fun _ _ ↦ (exp_pos _).le)

private theorem partition_pos {p : ℝ × ℝ} (hp : p ∈ CMP.domain) : 0 < partition p :=
  (exp_pos _).trans_le (weight_le_partition hp Nat.one_pos)

private theorem mass_pos {p : ℝ × ℝ} (hp : p ∈ CMP.domain) (k : ℕ) :
    0 < positiveMass p k :=
  div_pos (exp_pos _) (partition_pos hp)

private theorem mass_le_one {p : ℝ × ℝ} (hp : p ∈ CMP.domain) {k : ℕ}
    (hk : 0 < k) : positiveMass p k ≤ 1 :=
  (div_le_one (partition_pos hp)).mpr (weight_le_partition hp hk)

private theorem logFact_succ (k : ℕ) :
    CMP.logFactorial (k+1) = CMP.logFactorial k + log ((k : ℝ)+1) := by
  simp only [CMP.logFactorial, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ]
  rw [log_mul (by positivity) (by positivity), add_comm]

private theorem logFact_nonneg (k : ℕ) : 0 ≤ CMP.logFactorial k :=
  log_nonneg (by exact_mod_cast Nat.factorial_pos k)

private theorem weight_succ (p : ℝ × ℝ) (k : ℕ) :
    CMP.weight p.1 p.2 (k+1) =
      CMP.weight p.1 p.2 k * exp (p.1 - p.2 * log ((k : ℝ)+1)) := by
  simp only [CMP.weight, logFact_succ, Nat.cast_succ, ← exp_add]
  congr 1
  ring

private theorem parameters_bounded {c d : ℕ} (hc : 0 < c) (hcd : c+1 < d)
    {ε : ℝ} (hε : 0 < ε) :
    ∃ B : ℝ, ∀ p ∈ CMP.domain, ε ≤ positiveMass p c → ε ≤ positiveMass p d →
      p ∈ Icc (-B) B ×ˢ Icc 0 B := by
  let a := log ((c : ℝ)+1)
  let b := log (d : ℝ)
  have ha : 0 ≤ a := log_nonneg (by linarith [Nat.cast_nonneg (α := ℝ) c])
  have hab : a < b := log_lt_log (by positivity) (by exact_mod_cast hcd)
  let V := (-2*log ε)/(b-a)
  let B := |log ε| + |V| *(1+a)
  refine ⟨B,fun p hp hcε hdε ↦ ?_⟩
  have hν : 0 ≤ p.2 := hp.elim le_of_lt (fun h ↦ h.1.ge)
  have hf (k : ℕ) (hk : ε ≤ positiveMass p k) :
      ε * partition p ≤ CMP.weight p.1 p.2 k :=
    (le_div_iff₀ (partition_pos hp)).mp hk
  have hUpper : p.1-p.2*a ≤ -log ε := by
    have H := (mul_le_mul_of_nonneg_left (weight_le_partition hp (Nat.succ_pos c)) hε.le).trans
      (hf c hcε)
    rw [weight_succ, ← mul_left_comm] at H
    have H' : ε * exp (p.1-p.2*a) ≤ 1 :=
      le_of_mul_le_mul_left (by simpa only [mul_one] using H)
        (show 0 < CMP.weight p.1 p.2 c from exp_pos _)
    rw [mul_comm ε, ← le_div_iff₀ hε, one_div, ← exp_log hε, ← exp_neg] at H'
    exact exp_le_exp.mp H'
  have hd : 0 < d-1 := by omega
  have hd' : d-1+1 = d := Nat.sub_add_cancel (by omega)
  have hLower : log ε ≤ p.1-p.2*b := by
    have H := (mul_le_mul_of_nonneg_left (weight_le_partition hp hd) hε.le).trans
      (hf d hdε)
    have hstep := weight_succ p (d-1)
    rw [hd'] at hstep
    rw [hstep] at H
    have H' := le_of_mul_le_mul_left (by simpa only [mul_comm ε] using H)
      (show 0 < CMP.weight p.1 p.2 (d-1) from exp_pos _)
    have hcast : ((d-1 : ℕ) : ℝ)+1 = d := by exact_mod_cast hd'
    rw [hcast, ← exp_log hε] at H'
    exact exp_le_exp.mp H'
  have hV : p.2 ≤ V := (le_div_iff₀ (sub_pos.mpr hab)).mpr (by linarith)
  have hV' := hV.trans (le_abs_self V)
  have hB₂ : p.2 ≤ B := by
    dsimp [B]
    nlinarith [abs_nonneg (log ε), mul_nonneg (abs_nonneg V) ha]
  have hB₁ : -B ≤ p.1 := by
    dsimp [B]
    have hmul := mul_nonneg hν (ha.trans hab.le)
    nlinarith [neg_abs_le (log ε), abs_nonneg V, mul_nonneg (abs_nonneg V) ha]
  have hB₁' : p.1 ≤ B := by
    dsimp [B]
    nlinarith [neg_abs_le (log ε), abs_nonneg V, mul_le_mul_of_nonneg_right hV' ha]
  exact ⟨⟨hB₁,hB₁'⟩,⟨hν,hB₂⟩⟩

private theorem continuous_weight (k : ℕ) :
    Continuous (fun p : ℝ × ℝ ↦ CMP.weight p.1 p.2 k) := by
  unfold CMP.weight
  fun_prop

private theorem partition_continuous :
    ContinuousOn partition CMP.domain := by
  intro p hp
  have hterm (k : ℕ) : Tendsto (fun q : ℝ × ℝ ↦ CMP.weight q.1 q.2 (k+1))
      (𝓝[CMP.domain] p) (𝓝 (CMP.weight p.1 p.2 (k+1))) :=
    (continuous_weight _).continuousAt.continuousWithinAt
  have hdom {M v : ℝ} (hMv : (M,v) ∈ CMP.domain)
      (h₁ : ∀ᶠ q : ℝ × ℝ in 𝓝[CMP.domain] p, q.1 ≤ M)
      (h₂ : ∀ᶠ q : ℝ × ℝ in 𝓝[CMP.domain] p, v ≤ q.2) :
      ContinuousWithinAt partition CMP.domain p := by
    apply tendsto_tsum_of_dominated_convergence (pos_sum hMv) hterm
    filter_upwards [h₁,h₂] with q hq₁ hq₂ k
    rw [Real.norm_of_nonneg (show 0 ≤ CMP.weight q.1 q.2 (k+1) from (exp_pos _).le)]
    unfold CMP.weight
    exact exp_le_exp.mpr (sub_le_sub (mul_le_mul_of_nonneg_right hq₁ (by positivity))
      (mul_le_mul_of_nonneg_right hq₂ (logFact_nonneg _)))
  rcases hp with hp | ⟨_,hp⟩
  · apply hdom (M := p.1+1) (v := p.2/2) (Or.inl (by linarith))
    · exact (continuous_fst.continuousAt.continuousWithinAt.eventually
        (gt_mem_nhds (by linarith : p.1 < p.1+1))).mono fun _ h ↦ h.le
    · exact (continuous_snd.continuousAt.continuousWithinAt.eventually
        (lt_mem_nhds (by linarith : p.2/2 < p.2))).mono fun _ h ↦ h.le
  · apply hdom (M := p.1/2) (v := 0) (Or.inr ⟨rfl,by linarith⟩)
    · exact (continuous_fst.continuousAt.continuousWithinAt.eventually
        (gt_mem_nhds (by linarith : p.1 < p.1/2))).mono fun _ h ↦ h.le
    · exact (show ∀ᶠ q in 𝓝[CMP.domain] p, q ∈ CMP.domain from self_mem_nhdsWithin).mono
        fun _ h ↦ h.elim le_of_lt (fun H ↦ H.1.ge)

private theorem likelihood_continuous {n : ℕ} (x : Fin n → ℕ) :
    ContinuousOn (positiveLikelihood x) CMP.domain := by
  unfold positiveLikelihood
  apply continuousOn_finsetProd
  intro i _
  exact (continuous_weight _).continuousOn.div partition_continuous
    (fun _ h ↦ (partition_pos h).ne')

private theorem partition_excluded {ι : Type*} {F : Filter ι}
    {f : ι → ℝ × ℝ} {p : ℝ × ℝ} (hf : Tendsto f F (𝓝 p))
    (hD : ∀ᶠ i in F, f i ∈ CMP.domain) (hv : p.2 = 0) (hη : 0 ≤ p.1) :
    Tendsto (partition ∘ f) F atTop := by
  apply tendsto_atTop.mpr
  intro B
  obtain ⟨m,hm⟩ := exists_nat_gt B
  have H : B < ∑ k ∈ Finset.range m, CMP.weight p.1 p.2 (k+1) := by
    apply hm.trans_le
    simpa using Finset.sum_le_sum (s := Finset.range m) (fun k _ ↦
      (show (1 : ℝ) ≤ CMP.weight p.1 p.2 (k+1) by
        unfold CMP.weight
        rw [hv,zero_mul,sub_zero]
        exact one_le_exp (by positivity)))
  have hcont : Continuous (fun q : ℝ × ℝ ↦ ∑ k ∈ Finset.range m,
      CMP.weight q.1 q.2 (k+1)) :=
    continuous_finsetSum _ fun k _ ↦ continuous_weight (k+1)
  filter_upwards [hD,(hcont.continuousAt.tendsto.comp hf).eventually (Ioi_mem_nhds H)]
    with i hi hiB
  exact hiB.le.trans ((pos_sum hi).sum_le_tsum _ fun _ _ ↦ (exp_pos _).le)

private theorem likelihood_le_cell {n : ℕ} {x : Fin n → ℕ} (hx : ∀ i, 0 < x i)
    {p : ℝ × ℝ} (hp : p ∈ CMP.domain) (i : Fin n) :
    positiveLikelihood x p ≤ positiveMass p (x i) := by
  unfold positiveLikelihood
  calc
    _ ≤ ∏ j : Fin n, if j = i then positiveMass p (x i) else 1 := by
      apply Finset.prod_le_prod₀ (fun j _ ↦ (mass_pos hp (x j)).le)
      intro j _
      split_ifs with h
      · subst j
        rfl
      · exact mass_le_one hp (hx j)
    _ = _ := by simp

/-- CMP, all free, `zero:truncated`, criterion and failure. A positive
sample containing observations at least two cells apart has an attained
fit. Every positive likelihood superlevel set is compact. Thus its
maximizing sequences cannot escape through an excluded geometric limit,
diverging coefficients, or drift to infinity. -/
theorem positive_nonadjacent_attainment {n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, 0 < x i)
    (i j : Fin n) (hij : x i+1 < x j) :
    (∀ ε > 0, IsCompact {p ∈ CMP.domain | ε ≤ positiveLikelihood x p}) ∧
    ∃ p ∈ CMP.domain, IsMaxOn (positiveLikelihood x) CMP.domain p := by
  have hlevel (ε : ℝ) (hε : 0 < ε) :
      IsCompact {p ∈ CMP.domain | ε ≤ positiveLikelihood x p} := by
    let S := {p ∈ CMP.domain | ε ≤ positiveLikelihood x p}
    obtain ⟨B,hB⟩ := parameters_bounded (hx i) hij hε
    have hsub : S ⊆ Icc (-B) B ×ˢ Icc 0 B := fun p hp ↦ hB p hp.1
      (hp.2.trans (likelihood_le_cell hx hp.1 i)) (hp.2.trans (likelihood_le_cell hx hp.1 j))
    refine (isCompact_Icc.prod isCompact_Icc).of_isClosed_subset ?_ hsub
    apply isSeqClosed_iff_isClosed.mp
    intro f p hS hlim
    have hD : ∀ᶠ k in atTop, f k ∈ CMP.domain := Eventually.of_forall fun k ↦ (hS k).1
    have hv : 0 ≤ p.2 :=
      ge_of_tendsto (continuous_snd.continuousAt.tendsto.comp hlim)
        (hD.mono fun _ hp ↦ hp.elim (fun h ↦ h.le) (fun h ↦ h.1.ge))
    have hp : p ∈ CMP.domain := by
      by_contra hbad
      have hz : p.2 = 0 := hv.eq_of_not_lt (fun hv' ↦ hbad (Or.inl hv')) |>.symm
      have hη : 0 ≤ p.1 := le_of_not_gt (fun h ↦ hbad (Or.inr ⟨hz,h⟩))
      have hden := partition_excluded hlim hD hz hη
      have hc : Tendsto (fun k ↦ positiveMass (f k) (x i)) atTop (𝓝 0) :=
        ((continuous_weight _).continuousAt.tendsto.comp hlim).div_atTop hden
      exact hε.not_ge (ge_of_tendsto hc (Eventually.of_forall fun k ↦
        ((hS k).2.trans (likelihood_le_cell hx (hS k).1 i))))
    have hrel : Tendsto f atTop (𝓝[CMP.domain] p) := tendsto_nhdsWithin_iff.mpr ⟨hlim,hD⟩
    exact ⟨hp,ge_of_tendsto ((likelihood_continuous x p hp).tendsto.comp hrel)
      (Eventually.of_forall fun k ↦ (hS k).2)⟩
  refine ⟨hlevel,?_⟩
  let p₀ : ℝ × ℝ := (0,1)
  have hp₀ : p₀ ∈ CMP.domain := Or.inl one_pos
  let ε := positiveLikelihood x p₀
  have hε : 0 < ε := Finset.prod_pos fun k _ ↦ mass_pos hp₀ (x k)
  obtain ⟨p,hp,H⟩ := (hlevel ε hε).exists_isMaxOn
    ⟨p₀,hp₀,le_rfl⟩ ((likelihood_continuous x).mono fun _ h ↦ h.1)
  refine ⟨p,hp.1,fun q hq ↦ ?_⟩
  by_cases h : ε ≤ positiveLikelihood x q
  · exact H (a := q) ⟨hq,h⟩
  · exact (le_of_not_ge h).trans hp.2

private theorem domain_convex : Convex ℝ CMP.domain := by
  intro p hp q hq a b ha hb hab
  change 0 < a*p.2+b*q.2 ∨ a*p.2+b*q.2 = 0 ∧ a*p.1+b*q.1 < 0
  have hp' : 0 ≤ p.2 := hp.elim le_of_lt (fun h ↦ h.1.ge)
  have hq' : 0 ≤ q.2 := hq.elim le_of_lt (fun h ↦ h.1.ge)
  rcases ha.eq_or_lt with h | ha
  · have he : a = 0 := h.symm
    have hb : b = 1 := by linarith
    simpa only [he,hb,zero_mul,one_mul,zero_add] using (show 0 < q.2 ∨ q.2 = 0 ∧ q.1 < 0 from hq)
  rcases hb.eq_or_lt with h | hb
  · have he : b = 0 := h.symm
    have ha : a = 1 := by linarith
    simpa only [he,ha,zero_mul,one_mul,add_zero] using (show 0 < p.2 ∨ p.2 = 0 ∧ p.1 < 0 from hp)
  rcases hp with hp | ⟨hp,hpη⟩
  · exact Or.inl (add_pos_of_pos_of_nonneg (mul_pos ha hp) (mul_nonneg hb.le hq'))
  · rcases hq with hq | ⟨hq,hqη⟩
    · exact Or.inl (add_pos_of_nonneg_of_pos (mul_nonneg ha.le hp') (mul_pos hb hq))
    · exact Or.inr ⟨by simp [hp,hq],add_neg (mul_neg_of_pos_of_neg ha hpη)
        (mul_neg_of_pos_of_neg hb hqη)⟩

private def scoreExponent (m f : ℝ) (p : ℝ × ℝ) (k : ℕ) : ℝ :=
  p.1*((k : ℝ)-m)-p.2*(CMP.logFactorial k-f)

private def centeredPartition (m f : ℝ) (p : ℝ × ℝ) : ℝ :=
  ∑' j : ℕ, exp (scoreExponent m f p (j+1))

private theorem centered_eq (m f : ℝ) (p : ℝ × ℝ) (j : ℕ) :
    exp (scoreExponent m f p (j+1)) =
      exp (-p.1*m+p.2*f) * CMP.weight p.1 p.2 (j+1) := by
  unfold scoreExponent CMP.weight
  rw [← exp_add]
  congr 1
  ring

private theorem centered_strict_convex (m f : ℝ) :
    StrictConvexOn ℝ CMP.domain (centeredPartition m f) := by
  refine ⟨domain_convex,fun p hp q hq hpq a b ha hb hab ↦ ?_⟩
  let g := fun (p : ℝ × ℝ) (j : ℕ) ↦ exp (scoreExponent m f p (j+1))
  have hs (p : ℝ × ℝ) (hp : p ∈ CMP.domain) : Summable (g p) := by
    simp only [g,centered_eq]
    exact (pos_sum hp).mul_left _
  have hl (k : ℕ) : scoreExponent m f (a • p + b • q) k =
      a*scoreExponent m f p k+b*scoreExponent m f q k := by
    simp [scoreExponent]
    ring
  have hlt : ∃ j : ℕ, scoreExponent m f p (j+1) ≠ scoreExponent m f q (j+1) := by
    by_contra! H
    have H₁ := H 0
    have H₂ := H 1
    have H₃ := H 2
    change scoreExponent m f p 1 = scoreExponent m f q 1 at H₁
    change scoreExponent m f p 2 = scoreExponent m f q 2 at H₂
    change scoreExponent m f p 3 = scoreExponent m f q 3 at H₃
    simp only [scoreExponent, Nat.cast_one, Nat.cast_ofNat] at H₁ H₂ H₃
    have h₁ : CMP.logFactorial 2 = CMP.logFactorial 1+log 2 := by
      simpa only [Nat.cast_one, show (1:ℝ)+1 = 2 by norm_num] using logFact_succ 1
    have h₂ : CMP.logFactorial 3 = CMP.logFactorial 2+log 3 := by
      simpa only [Nat.cast_ofNat, show (2:ℝ)+1 = 3 by norm_num] using logFact_succ 2
    have hz : (p.2-q.2)*(log 3-log 2) = 0 := by
      rw [h₂,h₁] at H₃
      rw [h₁] at H₂
      linear_combination -H₃+2*H₂-H₁
    have hlog : log (2 : ℝ) < log 3 := log_lt_log (by norm_num) (by norm_num)
    have hν : p.2 = q.2 :=
      sub_eq_zero.mp ((mul_eq_zero.mp hz).resolve_right (sub_ne_zero.mpr hlog.ne'))
    have hη : p.1 = q.1 := by rw [hν] at H₁ H₂; linarith only [H₁,H₂]
    exact hpq (Prod.ext hη hν)
  obtain ⟨j,hj⟩ := hlt
  have hle (k : ℕ) : g (a • p+b • q) k ≤ a*g p k+b*g q k := by
    dsimp [g]
    rw [hl]
    exact convexOn_exp.2 (mem_univ _) (mem_univ _) ha.le hb.le hab
  have hj' : g (a • p+b • q) j < a*g p j+b*g q j := by
    dsimp [g]
    rw [hl]
    exact strictConvexOn_exp.2 (mem_univ _) (mem_univ _) hj ha hb hab
  have hA := (hs p hp).mul_left a
  have hB := (hs q hq).mul_left b
  have H := Summable.tsum_lt_tsum hle hj' (hs _ (domain_convex hp hq ha.le hb.le hab))
    (hA.add hB)
  rw [hA.tsum_add hB,tsum_mul_left,tsum_mul_left] at H
  exact H

private theorem likelihood_centered {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (p : ℝ × ℝ) :
    let m := (∑ i, (x i : ℝ))/n
    let f := (∑ i, CMP.logFactorial (x i))/n
    positiveLikelihood x p = (centeredPartition m f p)⁻¹ ^ n := by
  intro m f
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  have hZ : centeredPartition m f p = exp (-p.1*m+p.2*f)*partition p := by
    simp only [centeredPartition,centered_eq,tsum_mul_left,partition]
  have hexp : (∑ i, (p.1*x i-p.2*CMP.logFactorial (x i))) = n*(p.1*m-p.2*f) := by
    simp only [Finset.sum_sub_distrib, ← Finset.mul_sum]
    dsimp [m,f]
    field_simp
  change (∏ i, CMP.weight p.1 p.2 (x i) / partition p) = _
  rw [Finset.prod_div_distrib]
  simp only [CMP.weight, ← exp_sum, Finset.prod_const,Finset.card_univ,
    Fintype.card_fin,hexp,exp_nat_mul,hZ,mul_inv_rev,mul_pow,div_eq_mul_inv]
  rw [← exp_neg,show -( -p.1*m+p.2*f) = p.1*m-p.2*f by ring,mul_comm,inv_pow]

/-- CMP, all free, `zero:truncated`, uniqueness. Any two finite
maximizers coincide, even if the geometric boundary is admitted and wins. -/
theorem positive_window_unique {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {p q : ℝ × ℝ} (hp : p ∈ CMP.domain) (hq : q ∈ CMP.domain)
    (hP : IsMaxOn (positiveLikelihood x) CMP.domain p)
    (hQ : IsMaxOn (positiveLikelihood x) CMP.domain q) : p = q := by
  let m := (∑ i, (x i : ℝ))/n
  let f := (∑ i, CMP.logFactorial (x i))/n
  let Z := centeredPartition m f
  have hpos {p : ℝ × ℝ} (hp : p ∈ CMP.domain) : 0 < Z p := by
    simp only [Z,centeredPartition,centered_eq,tsum_mul_left]
    exact mul_pos (exp_pos _) (partition_pos hp)
  have hor (p q : ℝ × ℝ) (hp : p ∈ CMP.domain) (hq : q ∈ CMP.domain)
      (H : positiveLikelihood x q ≤ positiveLikelihood x p) : Z p ≤ Z q := by
    rw [likelihood_centered hn x q,likelihood_centered hn x p] at H
    change (Z q)⁻¹ ^ n ≤ (Z p)⁻¹ ^ n at H
    by_contra! h
    exact (pow_lt_pow_left₀ ((inv_lt_inv₀ (hpos hp) (hpos hq)).mpr h)
      (inv_nonneg.mpr (hpos hp).le) hn.ne').not_ge H
  exact (centered_strict_convex m f).eq_of_isMinOn
    (fun r hr ↦ hor p r hp hr (hP hr)) (fun r hr ↦ hor q r hq hr (hQ hr)) hp hq

/-- CMP, all free, `zero:truncated`, criterion, failure and uniqueness.
An attained fit exists exactly on positive samples whose observed
range is at least two. The attained optimum is unique. All other
positive samples have the point or adjacent-face height, with strict
inequality at every admissible coefficient pair. -/
theorem positive_window_attainment {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, 0 < x i) :
    ((∃ p ∈ CMP.domain, IsMaxOn (positiveLikelihood x) CMP.domain p) ↔
      ∃ i j, x i+1 < x j) ∧
    ((∃! p, p ∈ CMP.domain ∧ IsMaxOn (positiveLikelihood x) CMP.domain p) ↔
      ∃ i j, x i+1 < x j) ∧
    (∀ c > 0, (∀ i, x i = c ∨ x i = c+1) →
      ∃ q ∈ Icc (0 : ℝ) 1, IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) q ∧
        IsLUB (positiveLikelihood x '' CMP.domain) (Discrete5CMP.adjacentHeight x c q) ∧
        ∀ p ∈ CMP.domain, positiveLikelihood x p < Discrete5CMP.adjacentHeight x c q) := by
  have hf (c : ℕ) (hc : 0 < c) (h : ∀ i, x i = c ∨ x i = c+1) :=
    positive_adjacent_supremum hn hc x h
  have H :
      (∃ p ∈ CMP.domain, IsMaxOn (positiveLikelihood x) CMP.domain p) ↔
        ∃ i j, x i+1 < x j := by
    constructor
    · rintro ⟨p,hp,H⟩
      by_contra! h
      obtain ⟨i,_,hi⟩ := Finset.univ.exists_min_image x
        (⟨⟨0,hn⟩,Finset.mem_univ _⟩ : (Finset.univ : Finset (Fin n)).Nonempty)
      have hpair (j : Fin n) : x j = x i ∨ x j = x i+1 := by
        have Hlo := hi j (Finset.mem_univ _)
        have Hhi := h i j
        omega
      obtain ⟨q,_,_,hL,hlt⟩ := hf (x i) (hx i) hpair
      have hc := hL.2 (show positiveLikelihood x p ∈ upperBounds
          (positiveLikelihood x '' CMP.domain) from by
        rintro _ ⟨q,hq,rfl⟩
        exact H hq)
      exact (hlt p hp).not_ge hc
    · rintro ⟨i,j,hij⟩
      exact (positive_nonadjacent_attainment x hx i j hij).2
  refine ⟨H,?_ ,hf⟩
  constructor
  · rintro ⟨p,⟨hp,hM⟩,_⟩
    exact H.mp ⟨p,hp,hM⟩
  · intro h
    obtain ⟨p,hp,hM⟩ := H.mpr h
    exact ⟨p,⟨hp,hM⟩,fun q hq ↦ positive_window_unique hn x hq.1 hp hq.2 hM⟩

private theorem sum_mass_le_one {p : ℝ × ℝ} (hp : p ∈ CMP.domain)
    (A : Finset ℕ) (hA : ∀ k ∈ A, 0 < k) :
    (∑ k ∈ A, positiveMass p k) ≤ 1 := by
  classical
  let w := fun k : ℕ ↦ CMP.weight p.1 p.2 (k+1)
  let B := A.image (fun k ↦ k-1)
  have he : (∑ k ∈ B, w k) = ∑ k ∈ A, CMP.weight p.1 p.2 k := by
    rw [Finset.sum_image]
    · exact Finset.sum_congr rfl (fun k hk ↦ by simp [w,Nat.sub_add_cancel (hA k hk)])
    · intro a ha b hb H
      have h₁ := hA a ha
      have h₂ := hA b hb
      change a-1 = b-1 at H
      omega
  have hsum := (pos_sum hp).sum_le_tsum B (fun _ _ ↦ (exp_pos _).le)
  change (∑ k ∈ B, w k) ≤ partition p at hsum
  rw [he] at hsum
  simpa only [positiveMass,partition,← Finset.sum_div] using (div_le_one (partition_pos hp)).mpr hsum

/-- CMP, all free, `zero:truncated`, failure. Every pointwise limit at the
winning height on an adjacent-supported positive sample is supported on
those two cells. Its split maximizes the two-cell likelihood, including
the endpoint split when the sample is constant. -/
theorem positive_adjacent_winning_limit {ι : Type*} {F : Filter ι} [NeBot F]
    {n c : ℕ} (hn : 0 < n) (hc : 0 < c) (x : Fin n → ℕ)
    (hx : ∀ i, x i = c ∨ x i = c+1) {p : ι → ℝ × ℝ}
    (hp : ∀ᶠ t in F, p t ∈ CMP.domain) {g : ℕ → ℝ}
    (hg : ∀ k > 0, Tendsto (fun t ↦ positiveMass (p t) k) F (𝓝 (g k)))
    (hwin : Tendsto (fun t ↦ positiveLikelihood x (p t)) F
      (𝓝 (sSup (positiveLikelihood x '' CMP.domain)))) :
    g c = 1-g (c+1) ∧ g (c+1) ∈ Icc (0 : ℝ) 1 ∧
    IsMaxOn (Discrete5CMP.adjacentHeight x c) (Icc 0 1) (g (c+1)) ∧
    ∀ k > 0, k ≠ c → k ≠ c+1 → g k = 0 := by
  let H := Discrete5CMP.adjacentHeight x c
  obtain ⟨q,hq,hqmax,hL,_⟩ := positive_adjacent_supremum hn hc x hx
  have hnorm (A : Finset ℕ) (hA : ∀ k ∈ A, 0 < k) : (∑ k ∈ A, g k) ≤ 1 :=
    le_of_tendsto (tendsto_finsetSum A (fun k hk ↦ hg k (hA k hk)))
      (hp.mono fun _ hp ↦ sum_mass_le_one hp A hA)
  have hnon (k : ℕ) (hk : 0 < k) : 0 ≤ g k :=
    ge_of_tendsto (hg k hk) (hp.mono fun _ hp ↦ (mass_pos hp k).le)
  let a := g c
  let b := g (c+1)
  have hab : a+b ≤ 1 := by
    have H := hnorm {c,c+1} (by
      intro k hk
      simp only [Finset.mem_insert,Finset.mem_singleton] at hk
      rcases hk with rfl | rfl <;> omega)
    simpa only [Finset.sum_pair (show c ≠ c+1 by omega)] using H
  have hheight : (∏ i, g (x i)) = H q := by
    have hprod := tendsto_finsetProd Finset.univ (fun i _ ↦ hg (x i)
      (by rcases hx i with he | he <;> omega))
    have hs : sSup (positiveLikelihood x '' CMP.domain) = H q := hL.csSup_eq ⟨positiveLikelihood x (0,1), (0,1), Or.inl one_pos, rfl⟩
    rw [hs] at hwin
    exact tendsto_nhds_unique hprod hwin
  have hH : 0 < H q := (Finset.prod_pos (fun i _ ↦ mass_pos (Or.inl one_pos) (x i))).trans_le
    (hL.1 ⟨(0,1),Or.inl one_pos,rfl⟩)
  have hsum : 0 < a+b := by
    by_contra! hab'
    have ha' : a = 0 := by linarith [hnon c hc,hnon (c+1) (Nat.succ_pos c)]
    have hb' : b = 0 := by linarith [hnon c hc,hnon (c+1) (Nat.succ_pos c)]
    have hz (i : Fin n) : g (x i) = 0 := by
      rcases hx i with he | he
      · exact he.symm ▸ ha'
      · exact he.symm ▸ hb'
    simp only [hz,Finset.prod_const,Finset.card_univ,Fintype.card_fin,zero_pow hn.ne'] at hheight
    exact hH.ne' hheight.symm
  let v := b/(a+b)
  have hv : v ∈ Icc (0 : ℝ) 1 :=
    ⟨div_nonneg (hnon _ (Nat.succ_pos c)) hsum.le,
      (div_le_one hsum).mpr (by linarith [hnon c hc])⟩
  have hcell (i : Fin n) : g (x i) = (a+b) * (if x i = c then 1-v else v) := by
    rcases hx i with he | he
    · simp only [he,ite_true]
      change a = (a+b)*(1-b/(a+b))
      field_simp
      ring
    · simp only [he,ite_eq_right (show c+1 ≠ c by omega)]
      change b = (a+b)*(b/(a+b))
      field_simp
  have heq : H q = (a+b)^n * H v := by
    rw [← hheight]
    simp only [hcell,Finset.prod_mul_distrib,Finset.prod_const,Finset.card_univ,Fintype.card_fin]
    rfl
  have hsum' : a+b = 1 := by
    by_contra hne
    have hlt : (a+b)^n < 1 := pow_lt_one₀ hsum.le (lt_of_le_of_ne hab hne) hn.ne'
    have hh := mul_le_mul_of_nonneg_left (hqmax hv) (pow_nonneg hsum.le n)
    have hh' : H q ≤ (a+b)^n * H q := heq.trans_le hh
    exact (lt_irrefl (H q)) (hh'.trans_lt (by simpa only [one_mul] using mul_lt_mul_of_pos_right hlt hH))
  have hv' : v = b := by simp [v,hsum']
  have hvEq : H v = H q := by simpa only [hsum',one_pow,one_mul] using heq.symm
  change g c = 1-b ∧ b ∈ Icc (0 : ℝ) 1 ∧ IsMaxOn H (Icc 0 1) b ∧ _
  refine ⟨by linarith, hv' ▸ hv, ?_, ?_⟩
  · rw [← hv']
    exact fun t ht ↦ (hqmax ht).trans_eq hvEq.symm
  · intro k hk hkc hkd
    have H := hnorm {c,c+1,k} (by
      intro j hj
      simp only [Finset.mem_insert,Finset.mem_singleton] at hj
      rcases hj with rfl | rfl | rfl <;> omega)
    simp only [Finset.sum_insert (by simp [hkc.symm] : c ∉ ({c+1,k} : Finset ℕ)),
      Finset.sum_pair hkd.symm] at H
    linarith [hnon k hk]

end

end UnivariateML.Trunc5CMPFaces

end
