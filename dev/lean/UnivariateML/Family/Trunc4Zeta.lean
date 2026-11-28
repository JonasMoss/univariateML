module

public import UnivariateML.Family.Trunc3Zeta

/-! The signed zeta sample likelihood on the declared finite window. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc4Zeta

open Trunc3Zeta

/-- The signed-zeta known-cut criterion, failure and cardinality claims.
A singleton admits every exponent. Otherwise an interior empirical log mean
has one maximizer, and the two endpoint-only samples have unattained
likelihood supremum one approached by the corresponding endpoint atom. -/
theorem signed_zeta_known {N n : ℕ} (hN : 1 ≤ N) (hn : 0 < n)
    (x : Fin n → Finset.Icc 1 N) :
    let m := (∑ i, log ((x i).val : ℝ)) / (n : ℝ)
    let L := fun s ↦ ∑ i, log (mass N (x i).val s)
    let M := {s | IsMaxOn L univ s}
    let atom := fun (e : ℕ) (F : Filter ℝ) ↦
      (∀ j ∈ Finset.Icc 1 N, Tendsto (mass N j) F (𝓝 (if j = e then 1 else 0))) ∧
      IsLUB (range (fun s ↦ exp (L s))) 1 ∧ Tendsto (fun s ↦ exp (L s)) F (𝓝 1)
    ((N = 1 ∧ M = univ ∧ (∀ s, L s = 0) ∨
      1 < N ∧
        (m ∈ Ioo 0 (log (N : ℝ)) ∧ (∃! s, s ∈ M) ∨
         (∀ i, (x i).val = 1) ∧ m = 0 ∧ M = ∅ ∧ atom 1 atTop ∨
         (∀ i, (x i).val = N) ∧ m = log (N : ℝ) ∧ M = ∅ ∧ atom N atBot)) ∧
    (M.Nonempty ↔ N = 1 ∨ m ∈ Ioo 0 (log (N : ℝ)))) ∧
    (m = (∑ k ∈ Finset.Icc 1 N, log (k : ℝ)) / ((Finset.Icc 1 N).card : ℝ) → 0 ∈ M) := by
  dsimp only
  let m := (∑ i, log ((x i).val : ℝ)) / (n : ℝ)
  let L := fun s ↦ ∑ i, log (mass N (x i).val s)
  let M := {s | IsMaxOn L univ s}
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hc (s : ℝ) : s ∈ M ↔ IsMaxOn (logLik N m) univ s := by
    change IsMaxOn L univ s ↔ _
    constructor <;> intro H t ht
    · have H' := H ht
      change L t ≤ L s at H'
      simp only [L, sample_logLik hN hn x] at H'
      exact le_of_mul_le_mul_left H' hn'
    · change L t ≤ L s
      simp only [L, sample_logLik hN hn x]
      exact mul_le_mul_of_nonneg_left (H ht) hn'.le
  refine ⟨?_, ?_⟩
  swap
  · intro h
    apply (hc 0).mpr
    change m = _ at h
    rw [h]
    exact uniform_fit hN
  by_cases he : N = 1
  · subst N
    have hx (i : Fin n) : (x i).val = 1 := by
      have H := Finset.mem_Icc.mp (x i).property
      omega
    have hL (s : ℝ) : L s = 0 := by simp [L, hx, (Trunc3Zeta.singleton s).1]
    have hM : M = univ := by
      ext s
      simp only [mem_univ, iff_true]
      intro t _
      simp [hL]
    exact ⟨Or.inl ⟨rfl,hM,hL⟩, by
      change M.Nonempty ↔ _
      simp [hM]⟩
  have hlt : 1 < N := lt_of_le_of_ne hN (Ne.symm he)
  have hlog (i : Fin n) :
      0 ≤ log ((x i).val : ℝ) ∧ log ((x i).val : ℝ) ≤ log (N : ℝ) := by
    obtain ⟨ha,hb⟩ := Finset.mem_Icc.mp (x i).property
    have ha' : (1 : ℝ) ≤ (x i).val := by exact_mod_cast ha
    exact ⟨log_nonneg ha', log_le_log (by linarith) (by exact_mod_cast hb)⟩
  have hend (e : ℕ) (he' : e = 1 ∨ e = N) (hx : ∀ i, (x i).val = e) :
      m = log (e : ℝ) ∧ M = ∅ ∧ IsLUB (range (fun s ↦ exp (L s))) 1 ∧
      Tendsto (fun s ↦ exp (L s)) (if e = 1 then atTop else atBot) (𝓝 1) := by
    have hm : m = log (e : ℝ) := by
      simp [m,hx,nsmul_eq_mul,mul_div_cancel_left₀ _ hn'.ne']
    have hEq (s : ℝ) : exp (L s) = mass N e s ^ n := by
      have hpos : 0 < mass N e s := by
        apply div_pos (exp_pos _)
        exact Finset.sum_pos (fun _ _ ↦ exp_pos _)
          ⟨1,Finset.mem_Icc.mpr ⟨le_rfl,hN⟩⟩
      simp [L,hx,nsmul_eq_mul,exp_nat_mul,exp_log hpos]
    have ⟨hSup,hStrict⟩ := endpoint_supremum hlt hn he'
    have hS : IsLUB (range (fun s ↦ exp (L s))) 1 := by
      simpa only [hEq] using hSup
    have hL : Tendsto (fun s ↦ exp (L s)) (if e = 1 then atTop else atBot) (𝓝 1) := by
      rcases he' with rfl | rfl
      · have h := lower_atom (Finset.mem_Icc.mpr ⟨le_rfl,hN⟩)
        simpa only [hEq, ite_true, one_pow] using h.pow n
      · have h := upper_atom (Finset.mem_Icc.mpr ⟨hN,le_rfl⟩)
        simpa only [hEq, ite_eq_right he, ite_true, one_pow] using h.pow n
    refine ⟨hm, eq_empty_iff_forall_notMem.mpr (fun s hs ↦ ?_),hS,hL⟩
    have hlt := hStrict s
    rw [← hEq s] at hlt
    exact hlt.not_ge (hS.2 (by
      rintro _ ⟨t,rfl⟩
      exact exp_le_exp.mpr (hs (mem_univ t))))
  by_cases ha : ∀ i, (x i).val = 1
  · obtain ⟨hm,hM,hS,hL⟩ := hend 1 (Or.inl rfl) ha
    have hm' : m = 0 := by simpa using hm
    exact ⟨Or.inr ⟨hlt,Or.inr (Or.inl ⟨ha,hm',hM,fun _ hj ↦ lower_atom hj,hS,by simpa using hL⟩)⟩,
      by
        change M.Nonempty ↔ N = 1 ∨ m ∈ Ioo 0 (log (N : ℝ))
        simp [hM, he, hm', mem_Ioo]⟩
  by_cases hb : ∀ i, (x i).val = N
  · obtain ⟨hm,hM,hS,hL⟩ := hend N (Or.inr rfl) hb
    exact ⟨Or.inr ⟨hlt,Or.inr (Or.inr ⟨hb,hm,hM,fun _ hj ↦ upper_atom hj,hS,by simpa [he] using hL⟩)⟩,
      by
        change M.Nonempty ↔ N = 1 ∨ m ∈ Ioo 0 (log (N : ℝ))
        simp [hM, he, hm, mem_Ioo]⟩
  push Not at ha hb
  obtain ⟨a,ha⟩ := ha
  obtain ⟨b,hb⟩ := hb
  have hmean : m ∈ Ioo 0 (log (N : ℝ)) := by
    have ha' : (1 : ℝ) < (x a).val := by
      exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp (x a).property).1 ha.symm
    have hb' : ((x b).val : ℝ) < N := by
      exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp (x b).property).2 hb
    constructor
    · exact div_pos (Finset.sum_pos' (fun i _ ↦ (hlog i).1)
        ⟨a, Finset.mem_univ _, log_pos ha'⟩) hn'
    · apply (div_lt_iff₀ hn').mpr
      calc
        _ < ∑ _ : Fin n, log (N : ℝ) :=
          Finset.sum_lt_sum (fun i _ ↦ (hlog i).2)
            ⟨b,Finset.mem_univ _, log_lt_log (by
              have h := (Finset.mem_Icc.mp (x b).property).1
              exact_mod_cast (show 0 < (x b).val by omega)) hb'⟩
        _ = _ := by simp [nsmul_eq_mul, mul_comm]
  obtain ⟨⟨s,hs⟩,hu⟩ := (signed_fit hlt m).mpr hmean
  exact ⟨Or.inr ⟨hlt,Or.inl ⟨hmean,s,(hc s).mpr hs,
      fun t ht ↦ hu t s ((hc t).mp ht) hs⟩⟩,
    iff_of_true ⟨s,(hc s).mpr hs⟩ (Or.inr hmean)⟩

end UnivariateML.Trunc4Zeta

end
