module

public import UnivariateML.Family.TruncCount

/-! Finite zeta windows in the signed exponent chart. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc3Zeta

noncomputable section

def logLik (N : ℕ) (m s : ℝ) : ℝ :=
  Truncation.logLik (Finset.Icc 1 N) (fun k ↦ log (k : ℝ)) (fun _ ↦ 1) m (-s)

def mass (N k : ℕ) (s : ℝ) : ℝ :=
  exp (-s * log (k : ℝ)) / ∑ j ∈ Finset.Icc 1 N, exp (-s * log (j : ℝ))

/-- On a nonempty sample the sum of log masses depends on the data
through the empirical log count. -/
theorem sample_logLik {N n : ℕ} (hN : 1 ≤ N) (hn : 0 < n) (x : Fin n → Finset.Icc 1 N) (s : ℝ) :
    (∑ i, log (mass N (x i).val s)) =
      (n : ℝ) * logLik N ((∑ i, log ((x i).val : ℝ))/(n : ℝ)) s := by
  let Z := ∑ j ∈ Finset.Icc 1 N, exp (-s * log (j : ℝ))
  have hZ : 0 < Z := Finset.sum_pos (fun _ _ ↦ exp_pos _)
    ⟨1,Finset.mem_Icc.mpr ⟨le_rfl,hN⟩⟩
  have hsum (i : Fin n) : log (mass N (x i).val s) = -s*log ((x i).val : ℝ) - log Z := by
    change log (exp (-s * log ((x i).val : ℝ)) / Z) = _
    rw [log_div (exp_pos _).ne' hZ.ne', log_exp]
  simp only [hsum, Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const,
    Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  change _ = (n : ℝ) * ((∑ i, log ((x i).val : ℝ))/(n : ℝ) * (-s) -
    log (∑ j ∈ Finset.Icc 1 N, 1 * exp (log (j : ℝ)*(-s))))
  have he : (∑ j ∈ Finset.Icc 1 N, 1 * exp (log (j : ℝ)*(-s))) = Z := by
    simp only [one_mul, Z]
    exact Finset.sum_congr rfl (fun j _ ↦ by rw [mul_comm])
  rw [he]
  field_simp

/-- A nontrivial window fits exactly at interior empirical log means,
with a unique signed exponent. -/
theorem signed_fit {N : ℕ} (hN : 1 < N) (m : ℝ) :
    (∃ s, IsMaxOn (logLik N m) univ s) ∧
      (∀ s t, IsMaxOn (logLik N m) univ s → IsMaxOn (logLik N m) univ t → s = t)
      ↔ m ∈ Ioo 0 (log (N : ℝ)) := by
  let A := Finset.Icc 1 N
  let T := fun k : ℕ ↦ log (k : ℝ)
  let F := Truncation.logLik A T (fun _ ↦ 1) m
  have hl : 1 ∈ A := Finset.mem_Icc.mpr ⟨le_rfl,hN.le⟩
  have hu : N ∈ A := Finset.mem_Icc.mpr ⟨hN.le,le_rfl⟩
  have hlt : T 1 < T N := by
    apply log_lt_log (by norm_num)
    exact_mod_cast hN
  have hb : ∀ k ∈ A, T k ∈ Icc (T 1) (T N) := by
    intro k hk
    obtain ⟨ha,hb⟩ := Finset.mem_Icc.mp hk
    exact ⟨log_le_log (by norm_num) (by exact_mod_cast ha),
      log_le_log (by exact_mod_cast (show 0 < k by omega)) (by exact_mod_cast hb)⟩
  have hf : ∀ s, IsMaxOn (logLik N m) univ s ↔ IsMaxOn F univ (-s) := by
    intro s
    constructor
    · intro H t _
      simpa [logLik, F] using H (a := -t) (mem_univ _)
    · intro H t _
      exact H (a := -t) (mem_univ _)
  have huniq {s t : ℝ} (hs : IsMaxOn F univ s) (ht : IsMaxOn F univ t) : s = t := by
    have H := Truncation.maximizers_constant_direction A T (fun _ ↦ 1)
      ⟨1,hl⟩ (fun _ _ ↦ one_pos) hs ht 1 hl N hu
    change T 1 * (s-t) = T N * (s-t) at H
    exact sub_eq_zero.mp ((mul_eq_zero.mp (show (T N-T 1)*(s-t) = 0 by linarith)).resolve_left
      (sub_pos.mpr hlt).ne')
  have he := Truncation.scalar_existsUnique_iff (h := fun _ ↦ 1)
    (fun _ _ ↦ one_pos) hl hu hlt hb m
  constructor
  · rintro ⟨⟨s,hs⟩,_⟩
    have H := (hf s).mp hs
    simpa [T] using he.mp ⟨-s,H,fun t ht ↦ huniq ht H⟩
  · intro hm
    have hh : m ∈ Ioo (T 1) (T N) := by simpa [T] using hm
    obtain ⟨s,hs,_⟩ := he.mpr hh
    refine ⟨⟨-s,(hf _).mpr (by simpa using hs)⟩, fun a b ha hb ↦ ?_⟩
    exact neg_injective (huniq ((hf a).mp ha) ((hf b).mp hb))

/-- The finite exponent zero fits at the uniform mean log count. -/
theorem uniform_fit {N : ℕ} (hN : 1 ≤ N) :
    let A := Finset.Icc 1 N
    let m := (∑ k ∈ A, log (k : ℝ)) / (A.card : ℝ)
    IsMaxOn (logLik N m) univ 0 := by
  dsimp only
  let A := Finset.Icc 1 N
  let C := (A.card : ℝ)
  let m := (∑ k ∈ A, log (k : ℝ)) / C
  have hA : A.Nonempty := ⟨1,Finset.mem_Icc.mpr ⟨le_rfl,hN⟩⟩
  have hC : 0 < C := by
    change (0 : ℝ) < (A.card : ℝ)
    exact_mod_cast Finset.card_pos.mpr hA
  have hs : (∑ k ∈ A, (log (k : ℝ)-m)) = 0 := by
    rw [Finset.sum_sub_distrib, Finset.sum_const, nsmul_eq_mul]
    change (∑ k ∈ A, log (k : ℝ)) - C*((∑ k ∈ A, log (k : ℝ))/C) = 0
    field_simp
    ring
  intro t _
  change logLik N m t ≤ logLik N m 0
  have hp : C ≤ ∑ k ∈ A, exp (-t*(log (k : ℝ)-m)) := by
    have H := Finset.sum_le_sum (s := A) (fun k _ ↦ add_one_le_exp (-t*(log (k : ℝ)-m)))
    rw [Finset.sum_add_distrib, ← Finset.mul_sum, hs, mul_zero, zero_add,
      Finset.sum_const, nsmul_eq_mul, mul_one] at H
    exact H
  have hf : (∑ k ∈ A, exp (-t*(log (k : ℝ)-m))) =
      (∑ k ∈ A, exp (log (k : ℝ)*(-t))) * exp (m*t) := by
    rw [Finset.sum_mul]
    apply Finset.sum_congr rfl
    intro k hk
    rw [← exp_add]
    congr 1
    ring
  have hpos : 0 < ∑ k ∈ A, exp (log (k : ℝ)*(-t)) :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) hA
  have H := log_le_log hC hp
  rw [hf, log_mul hpos.ne' (exp_pos _).ne', log_exp] at H
  change m * (-t) - log (∑ k ∈ A, 1 * exp (log (k : ℝ)*(-t))) ≤
    m * (-(0 : ℝ)) - log (∑ k ∈ A, 1 * exp (log (k : ℝ)*(-(0 : ℝ))))
  simp only [one_mul, neg_zero, mul_zero, exp_zero, Finset.sum_const, nsmul_eq_mul, mul_one,
    zero_sub]
  linarith

/-- A compatible singleton puts unit mass at one for every exponent. -/
theorem singleton (s : ℝ) : mass 1 1 s = 1 ∧ IsMaxOn (logLik 1 0) univ s := by
  constructor
  · simp [mass]
  · intro t _
    change logLik 1 0 t ≤ logLik 1 0 s
    simp [logLik, Truncation.logLik, Truncation.partition]

private theorem atom {N j e : ℕ} {F : Filter ℝ}
    (he : e ∈ Finset.Icc 1 N) (hj : j ∈ Finset.Icc 1 N)
    (hlim : ∀ k ∈ Finset.Icc 1 N, k ≠ e →
      Tendsto (fun s : ℝ ↦ exp (-s * (log (k : ℝ) - log (e : ℝ)))) F (𝓝 0)) :
    Tendsto (mass N j) F (𝓝 (if j = e then 1 else 0)) := by
  let w (k : ℕ) (s : ℝ) := exp (-s * (log (k : ℝ) - log (e : ℝ)))
  have hw (k : ℕ) (hk : k ∈ Finset.Icc 1 N) :
      Tendsto (w k) F (𝓝 (if k = e then 1 else 0)) := by
    by_cases h : k = e
    · subst k
      simpa [w] using (tendsto_const_nhds (x := (1 : ℝ)) (f := F))
    · simpa [h, w] using hlim k hk h
  have hs := tendsto_finsetSum (Finset.Icc 1 N) hw
  rw [Finset.sum_ite_eq', ite_eq_left he] at hs
  have H := (hw j hj).div hs one_ne_zero
  simp only [div_one] at H
  apply H.congr
  intro s
  have hfactor (k : ℕ) :
      w k s = exp (-s * log (k : ℝ)) * exp (s * log (e : ℝ)) := by
    dsimp [w]
    rw [show -s * (log (k : ℝ) - log (e : ℝ)) =
      -s * log (k : ℝ) + s * log (e : ℝ) by ring, exp_add]
  change w j s / (∑ k ∈ Finset.Icc 1 N, w k s) = mass N j s
  simp only [hfactor, ← Finset.sum_mul, mass]
  exact mul_div_mul_right _ _ (exp_pos _).ne'

/-- At positive infinite exponent the window concentrates at one. -/
theorem lower_atom {N j : ℕ} (hj : j ∈ Finset.Icc 1 N) :
    Tendsto (mass N j) atTop (𝓝 (if j = 1 then 1 else 0)) := by
  refine atom (Finset.mem_Icc.mpr ⟨le_rfl, (Finset.mem_Icc.mp hj).1.trans
    (Finset.mem_Icc.mp hj).2⟩) hj ?_
  intro k hk hne
  have h : (1 : ℝ) < k := by
    exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp hk).1 hne.symm
  have H := tendsto_exp_atBot.comp
    (tendsto_id.atTop_mul_const_of_neg (neg_neg_of_pos (log_pos h)))
  exact H.congr fun s ↦ by simp

/-- At negative infinite exponent the window concentrates at its upper cell. -/
theorem upper_atom {N j : ℕ} (hj : j ∈ Finset.Icc 1 N) :
    Tendsto (mass N j) atBot (𝓝 (if j = N then 1 else 0)) := by
  refine atom (Finset.mem_Icc.mpr ⟨(Finset.mem_Icc.mp hj).1.trans
    (Finset.mem_Icc.mp hj).2, le_rfl⟩) hj ?_
  intro k hk hne
  have h : (k : ℝ) < N := by exact_mod_cast lt_of_le_of_ne (Finset.mem_Icc.mp hk).2 hne
  have hk0 : (0 : ℝ) < k := by exact_mod_cast (show 0 < k from (Finset.mem_Icc.mp hk).1)
  have H := tendsto_exp_atBot.comp
    (tendsto_id.atBot_mul_const (sub_pos.mpr (log_lt_log hk0 h)))
  exact H.congr fun s ↦ by simp only [Function.comp_def, id_eq]; congr 1; ring

/-- Endpoint-only samples on a nontrivial window have supremum one,
approached by the appropriate signed exponent and unattained at finite exponents. -/
theorem endpoint_supremum {N n : ℕ} (hN : 1 < N) (hn : 0 < n)
    {j : ℕ} (hj : j = 1 ∨ j = N) :
    IsLUB (range (fun s ↦ mass N j s ^ n)) 1 ∧ ∀ s, mass N j s ^ n < 1 := by
  have h₁ : 1 ∈ Finset.Icc 1 N := Finset.mem_Icc.mpr ⟨le_rfl,hN.le⟩
  have hN' : N ∈ Finset.Icc 1 N := Finset.mem_Icc.mpr ⟨hN.le,le_rfl⟩
  have hj' : j ∈ Finset.Icc 1 N := hj.elim (· ▸ h₁) (· ▸ hN')
  have hh : ∃ k ∈ Finset.Icc 1 N, k ≠ j := by
    rcases hj with rfl | rfl
    · exact ⟨N,hN',hN.ne'⟩
    · exact ⟨1,h₁,hN.ne⟩
  obtain ⟨k,hk,hkj⟩ := hh
  have hlt (s : ℝ) : mass N j s ^ n < 1 := by
    unfold mass
    let w := fun i : ℕ ↦ exp (-s * log (i : ℝ))
    have hp : w j < ∑ i ∈ Finset.Icc 1 N, w i := by
      rw [← Finset.add_sum_erase _ _ hj']
      exact lt_add_of_pos_right _ (Finset.sum_pos (fun _ _ ↦ exp_pos _)
        ⟨k,Finset.mem_erase.mpr ⟨hkj,hk⟩⟩)
    exact pow_lt_one₀ (div_nonneg (exp_pos _).le ((exp_pos _).trans hp).le)
      ((div_lt_one ((exp_pos _).trans hp)).mpr hp) hn.ne'
  refine ⟨⟨?_,?_⟩,hlt⟩
  · rintro _ ⟨s,rfl⟩
    exact (hlt s).le
  · intro C hC
    have he : ∀ s, mass N j s ^ n ≤ C := fun s ↦ hC ⟨s,rfl⟩
    rcases hj with hj | hj
    · subst j
      have h : Tendsto (fun s ↦ mass N 1 s ^ n) atTop (𝓝 1) := by
        simpa using (lower_atom h₁).pow n
      exact le_of_tendsto h (Eventually.of_forall he)
    · subst j
      have h : Tendsto (fun s ↦ mass N N s ^ n) atBot (𝓝 1) := by
        simpa using (upper_atom hN').pow n
      exact le_of_tendsto h (Eventually.of_forall he)

end

end UnivariateML.Trunc3Zeta

end
