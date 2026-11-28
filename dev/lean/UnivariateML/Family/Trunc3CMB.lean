module

public import UnivariateML.Generic.Truncation
public import UnivariateML.Family.Discrete3CMB
public import UnivariateML.Family.TruncCount

/-! Fixed-size CMB on a known window. Positive truncation uses the same
statistic on the cells from one through the fixed size.

Deviations: the two-coordinate existence statement is for finite
log-odds. Admission and comparison of the deterministic endpoint laws
are stated separately. The face-limit theorem takes the exposed cell set as input. -/

@[expose] public section

open Set Real Filter
open scoped RealInnerProductSpace Topology

namespace UnivariateML.Trunc3CMB

noncomputable section

def statistic {N : ℕ} (k : Fin (N+1)) : CMP.Plane :=
  WithLp.toLp 2 ((k.val : ℝ), Discrete3CMB.height N k)

/-- The finite canonical fit criterion on a retained fixed-size carrier. -/
theorem canonical_fit {N : ℕ} {A : Finset (Fin (N+1))} (hA : A.Nonempty)
    (m : CMP.Plane) :
    (∃ θ, IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (statistic '' (A : Set (Fin (N+1))))) :=
  Truncation.finite_exists_iff A statistic (fun _ ↦ 1) hA (fun _ _ ↦ one_pos) m

/-- On a positive window the same canonical criterion uses the hull with
zero removed. -/
theorem positive_fit {N : ℕ} (hN : 0 < N) (m : CMP.Plane) :
    let A := Finset.univ.filter (fun k : Fin (N+1) ↦ 0 < k.val)
    (∃ θ, IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ) ↔
      m ∈ intrinsicInterior ℝ (convexHull ℝ (statistic '' (A : Set (Fin (N+1))))) := by
  apply canonical_fit
  exact ⟨⟨1, by omega⟩, Finset.mem_filter.mpr ⟨Finset.mem_univ _, Nat.zero_lt_one⟩⟩

/-- For an empirical statistic on the segment between two retained cells,
the whole maximizing fibre is the line of constant log weight ratio. -/
theorem pair_fibre {N : ℕ} (l u : Fin (N+1)) {a b : ℝ} (hab : a+b = 1)
    {θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ θ) :
    IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ φ ↔
      ⟪φ, statistic u⟫ - ⟪φ, statistic l⟫ = ⟪θ, statistic u⟫ - ⟪θ, statistic l⟫ := by
  have hA : ({l,u} : Finset (Fin (N+1))).Nonempty := ⟨l, by simp⟩
  constructor
  · intro hφ
    have H := Truncation.maximizers_constant_direction _ statistic (fun _ ↦ 1)
      hA (fun _ _ ↦ one_pos) hφ hθ l (by simp) u (by simp)
    simp only [inner_sub_left] at H
    linarith
  · intro H
    have he : ∀ k ∈ ({l,u} : Finset (Fin (N+1))),
        ⟪φ-θ, statistic k⟫ = ⟪φ-θ, a • statistic l + b • statistic u⟫ := by
      intro k hk
      simp only [Finset.mem_insert, Finset.mem_singleton] at hk
      have hc : ⟪φ-θ, statistic u⟫ = ⟪φ-θ, statistic l⟫ := by
        simp only [inner_sub_left]
        linarith
      rw [inner_add_right, real_inner_smul_right, real_inner_smul_right, hc, ← add_mul, hab, one_mul]
      rcases hk with rfl | rfl
      · rfl
      · exact hc
    have hs := Truncation.logLik_shift _ statistic (fun _ ↦ 1) (φ-θ) θ _
      hA (fun _ _ ↦ one_pos) he
    rw [add_sub_cancel] at hs
    intro t ht
    exact (hθ ht).trans_eq hs.symm

/-- For two different retained cells with positive empirical proportions
the optimum consists of every coefficient on the empirical-odds line. -/
theorem pair_empirical_odds {N : ℕ} {l u : Fin (N+1)} (hne : l ≠ u)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a+b = 1) (φ : CMP.Plane) :
    IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ φ ↔
      ⟪φ, statistic u⟫ - ⟪φ, statistic l⟫ = log (b/a) := by
  let t := log (b/a)
  let θ : CMP.Plane := WithLp.toLp 2 (t / ((u.val : ℝ)-l.val), 0)
  have hne' : (u.val : ℝ) - l.val ≠ 0 := by
    apply sub_ne_zero.mpr
    exact_mod_cast (fun h ↦ hne (Fin.ext h.symm) : u.val ≠ l.val)
  have θratio : ⟪θ, statistic u⟫ - ⟪θ, statistic l⟫ = t := by
    simp [θ, statistic, WithLp.prod_inner_apply, RCLike.inner_apply]
    field_simp
  let H := fun r : ℝ ↦ log (1 + exp r)
  have reduce (v : CMP.Plane) :
      Truncation.logLik {l,u} statistic (fun _ ↦ 1) (a • statistic l + b • statistic u) v =
        b * (⟪v, statistic u⟫ - ⟪v, statistic l⟫) -
          H (⟪v, statistic u⟫ - ⟪v, statistic l⟫) := by
    unfold Truncation.logLik Truncation.partition
    simp only [Finset.sum_insert (by simpa using hne : l ∉ ({u} : Finset _)),
      Finset.sum_singleton, one_mul, inner_add_right, real_inner_smul_right]
    have he : exp ⟪v, statistic l⟫ + exp ⟪v, statistic u⟫ =
        exp ⟪v, statistic l⟫ * (1 + exp (⟪v, statistic u⟫ - ⟪v, statistic l⟫)) := by
      rw [mul_add, mul_one, ← exp_add]
      ring_nf
    rw [he, log_mul (exp_pos _).ne' (by positivity), log_exp]
    dsimp [H]
    have ha' : a = 1-b := by linarith
    rw [ha']
    ring
  have hθ : IsMaxOn (Truncation.logLik {l,u} statistic (fun _ ↦ 1)
      (a • statistic l + b • statistic u)) univ θ := by
    intro v _
    let r := ⟪v, statistic u⟫ - ⟪v, statistic l⟫
    change Truncation.logLik _ _ _ _ v ≤ Truncation.logLik _ _ _ _ θ
    rw [reduce, reduce, θratio]
    change b*r - H r ≤ b*t - H t
    let q := (1 + exp r)⁻¹
    let p := exp r * q
    have hq : 0 < q := by dsimp [q]; positivity
    have hp : 0 < p := mul_pos (exp_pos _) hq
    have hpq : p+q = 1 := by dsimp [p,q]; field_simp; ring
    have hqlog : log q = -H r := log_inv _
    have hplog : log p = r-H r := by
      dsimp [p]
      rw [log_mul (exp_pos _).ne' hq.ne', log_exp, hqlog]
      ring
    have L₁ := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (div_pos hq ha)) ha.le
    have L₂ := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos (div_pos hp hb)) hb.le
    rw [log_div hq.ne' ha.ne', hqlog] at L₁
    rw [log_div hp.ne' hb.ne', hplog] at L₂
    have h₁ : a*(q/a-1) = q-a := by field_simp
    have h₂ : b*(p/b-1) = p-b := by field_simp
    rw [h₁] at L₁
    rw [h₂] at L₂
    have ht : t = log b-log a := log_div hb.ne' ha.ne'
    have hHt : H t = -log a := by
      dsimp [H,t]
      rw [exp_log (div_pos hb ha), show 1+b/a = a⁻¹ by field_simp; linarith, log_inv]
    nlinarith [congrArg (fun x : ℝ ↦ x * (H r + log a)) hab]
  rw [pair_fibre l u hab hθ, θratio]

/-- A singleton carrier has a flat finite canonical likelihood. -/
theorem singleton {N : ℕ} (k : Fin (N+1)) (θ : CMP.Plane) :
    Truncation.logLik {k} statistic (fun _ ↦ 1) (statistic k) θ = 0 := by
  simpa using Truncation.singleton_logLik statistic (fun _ ↦ 1) k one_pos θ

/-- Fixed dispersion on a finite retained window. The endpoint atom is
included only when it is an actual parent endpoint. -/
theorem held_dispersion_fit {N l u : ℕ} (hlu : l < u) (_hu : u ≤ N)
    (ν : ℝ) {m : ℝ} (hm : m ∈ Icc (l : ℝ) (u : ℝ)) :
    let h := fun k : ℕ ↦ exp (ν * log (N.choose k : ℝ))
    let D := TruncCount.finiteCountDomain (decide (l = 0)) (decide (u = N))
    (∃! p, p ∈ D ∧ IsMaxOn (TruncCount.finiteCountLikelihood l u h m) D p) ↔
      m ∈ Ioo (l : ℝ) (u : ℝ) ∨ (m = l ∧ l = 0) ∨ (m = u ∧ u = N) := by
  simpa using TruncCount.finite_count_fit hlu
    (h := fun k ↦ exp (ν * log (N.choose k : ℝ))) (fun _ _ ↦ exp_pos _) hm
    (decide (l = 0)) (decide (u = N))

/-- A deterministic parent endpoint contributes a conditional atom
exactly when its retained mass is nonzero. -/
theorem endpoint_admission {N : ℕ} (A : Finset (Fin (N+1))) (upper : Bool)
    (k : Fin (N+1)) :
    let e := if upper then Fin.last N else (0 : Fin (N+1))
    let w := fun j : Fin (N+1) ↦ if j = e then (1 : ℝ) else 0
    (0 < ∑ j ∈ A, w j ↔ e ∈ A) ∧
      (e ∈ A → w k / (∑ j ∈ A, w j) = if k = e then 1 else 0) := by
  classical
  dsimp only
  rw [Finset.sum_ite_eq']
  split_ifs <;> simp_all

/-- On a nonsingleton retained carrier an endpoint-only sample has
strictly smaller likelihood at every finite canonical point than at its
compatible deterministic endpoint. -/
theorem endpoint_strictly_wins {N n : ℕ} (hn : 0 < n)
    {A : Finset (Fin (N+1))} (upper : Bool)
    (h : ∃ j ∈ A, j ≠ if upper then Fin.last N else (0 : Fin (N+1)))
    (he : (if upper then Fin.last N else (0 : Fin (N+1))) ∈ A) (θ : CMP.Plane) :
    let e := if upper then Fin.last N else (0 : Fin (N+1))
    let w := fun j : Fin (N+1) ↦ exp ⟪θ, statistic j⟫
    (w e / (∑ j ∈ A, w j))^n < 1 := by
  dsimp only
  let e := if upper then Fin.last N else (0 : Fin (N+1))
  let w := fun j : Fin (N+1) ↦ exp ⟪θ, statistic j⟫
  obtain ⟨j,hj,hjne⟩ := h
  have hp : w e < ∑ k ∈ A, w k := by
    rw [← Finset.add_sum_erase _ _ he]
    exact lt_add_of_pos_right _ (Finset.sum_pos (fun _ _ ↦ exp_pos _)
      ⟨j,Finset.mem_erase.mpr ⟨hjne,hj⟩⟩)
  have hs := (exp_pos ⟪θ, statistic e⟫).trans hp
  exact pow_lt_one₀ (div_nonneg (exp_pos _).le hs.le) ((div_lt_one hs).mpr hp) hn.ne'

/-- At fixed interior odds, the scalar statistic is log choose.
When it varies, the interior empirical interval is exactly the criterion. -/
theorem held_odds_fit {N : ℕ} {A : Finset (Fin (N+1))}
    (η m : ℝ) {l u : Fin (N+1)} (hl : l ∈ A) (hu : u ∈ A)
    (hlt : Discrete3CMB.height N l < Discrete3CMB.height N u)
    (hb : ∀ k ∈ A, Discrete3CMB.height N k ∈
      Icc (Discrete3CMB.height N l) (Discrete3CMB.height N u)) :
    (∃! ν, IsMaxOn (Truncation.logLik A (Discrete3CMB.height N)
      (fun k ↦ exp (η * k.val)) m) univ ν) ↔
      m ∈ Ioo (Discrete3CMB.height N l) (Discrete3CMB.height N u) :=
  Truncation.scalar_existsUnique_iff (fun _ _ ↦ exp_pos _) hl hu hlt hb m

/-- At fixed odds a constant log-binomial statistic makes dispersion flat,
including the equal-height pair at the centre of an odd design. -/
theorem held_odds_flat {N : ℕ} {A : Finset (Fin (N+1))}
    (hA : A.Nonempty) (η m ν : ℝ) (hm : ∀ k ∈ A, Discrete3CMB.height N k = m) :
    IsMaxOn (Truncation.logLik A (Discrete3CMB.height N)
      (fun k ↦ exp (η * k.val)) m) univ ν := by
  intro t _
  have H := Truncation.logLik_shift A (Discrete3CMB.height N) (fun k ↦ exp (η*k.val))
    (t-ν) ν m hA (fun _ _ ↦ exp_pos _) (fun k hk ↦ by rw [hm k hk])
  rw [add_sub_cancel] at H
  exact H.le

/-- At fixed dispersion either odds endpoint concentrates on the
corresponding retained extreme, even if the parent endpoint is excluded. -/
theorem held_dispersion_limits {N l u j : ℕ} (hlu : l ≤ u) (ν : ℝ)
    (hj : j ∈ Finset.Icc l u) :
    let h := fun k : ℕ ↦ exp (ν * log (N.choose k : ℝ))
    Tendsto (TruncCount.finiteCountMass l u h j) atBot (𝓝 (if j = l then 1 else 0)) ∧
    Tendsto (TruncCount.finiteCountMass l u h j) atTop (𝓝 (if j = u then 1 else 0)) :=
  ⟨TruncCount.finite_lower_atom hlu hj (exp_pos _),
    TruncCount.finite_upper_atom hlu hj (exp_pos _)⟩

private theorem log_choose_form {N k : ℕ} (hk : k ≤ N) :
    log (N.choose k : ℝ) =
      CMP.logFactorial N - CMP.logFactorial k - CMP.logFactorial (N-k) := by
  have h := congrArg (fun z : ℕ ↦ log (z : ℝ)) (Nat.choose_mul_factorial_mul_factorial hk)
  push_cast at h
  rw [log_mul (by have := Nat.choose_pos hk; positivity) (by positivity),
    log_mul (by have := Nat.choose_pos hk; positivity) (by positivity)] at h
  unfold CMP.logFactorial
  linarith only [h]

private theorem log_factorial_succ (k : ℕ) :
    CMP.logFactorial (k+1) = CMP.logFactorial k + log ((k : ℝ)+1) := by
  unfold CMP.logFactorial
  rw [Nat.factorial_succ, Nat.cast_mul, Nat.cast_add, Nat.cast_one,
    log_mul (by positivity) (by positivity)]
  ring

private theorem adjacent_slope {N k : ℕ} (hk : k < N) :
    log (N.choose (k+1) : ℝ) - log (N.choose k : ℝ) =
      log ((N-k : ℕ) : ℝ) - log ((k : ℝ)+1) := by
  have hh : N-k = (N-(k+1))+1 := by omega
  rw [log_choose_form (show k+1 ≤ N by omega), log_choose_form hk.le,
    log_factorial_succ k]
  rw [hh, log_factorial_succ (N-(k+1)), Nat.cast_add, Nat.cast_one]
  ring

/-- The adjacent log-binomial slopes decrease strictly over each
three-cell block of a fixed-size carrier. -/
theorem adjacent_strict {N k : ℕ} (hk : k+2 ≤ N) :
    log (N.choose (k+2) : ℝ) - log (N.choose (k+1) : ℝ) <
      log (N.choose (k+1) : ℝ) - log (N.choose k : ℝ) := by
  rw [adjacent_slope (show k+1 < N by omega), adjacent_slope (show k < N by omega)]
  have h₁ : log ((N-(k+1) : ℕ) : ℝ) < log ((N-k : ℕ) : ℝ) :=
    log_lt_log (by exact_mod_cast (show 0 < N-(k+1) by omega))
      (by exact_mod_cast (show N-(k+1) < N-k by omega))
  have h₂ : log ((k : ℝ)+1) < log (((k+1 : ℕ) : ℝ)+1) :=
    log_lt_log (by positivity) (by push_cast; linarith)
  linarith

/-- Each consecutive pair is exposed from above by the line through
its two log-binomial statistics. This persists after restricting the carrier. -/
theorem adjacent_exposure {N c : ℕ} (hc : c < N) (k : Fin (N+1)) :
    let slope := log ((N-c : ℕ) : ℝ) - log ((c : ℝ)+1)
    let gap := Discrete3CMB.height N k - log (N.choose c : ℝ) - slope*((k.val : ℝ)-c)
    gap ≤ 0 ∧ (gap = 0 ↔ k.val = c ∨ k.val = c+1) := by
  dsimp only
  let d := N-(c+1)
  have hk : k.val ≤ N := Nat.le_of_lt_succ k.isLt
  have hd : d+1 = N-c := by dsimp [d]; omega
  have hd' : (d : ℝ)+1 = ((N-c : ℕ) : ℝ) := by exact_mod_cast hd
  have hkdiff : ((N-k.val : ℕ) : ℝ)-(d : ℝ) = 1-((k.val : ℝ)-c) := by
    dsimp [d]
    rw [Nat.cast_sub hk, Nat.cast_sub (by omega : c+1 ≤ N), Nat.cast_add, Nat.cast_one]
    ring
  let L := fun j : ℕ ↦ CMP.logFactorial c+log ((c : ℝ)+1)*((j : ℝ)-c)
  let R := fun j : ℕ ↦ CMP.logFactorial d+log ((d : ℝ)+1)*((j : ℝ)-d)
  have hL : L k.val ≤ CMP.logFactorial k.val := (CMP.adjacent_supporting_line c k.val).1
  have hR : R (N-k.val) ≤ CMP.logFactorial (N-k.val) :=
    (CMP.adjacent_supporting_line d (N-k.val)).1
  have hRc : R (N-c) = CMP.logFactorial (N-c) :=
    (CMP.adjacent_supporting_line d (N-c)).2.mpr (Or.inr hd.symm)
  have hsum : Discrete3CMB.height N k - log (N.choose c : ℝ) -
        (log ((N-c : ℕ) : ℝ)-log ((c : ℝ)+1))*((k.val : ℝ)-c) =
      (L k.val-CMP.logFactorial k.val)+(R (N-k.val)-CMP.logFactorial (N-k.val)) := by
    unfold Discrete3CMB.height
    rw [log_choose_form hk, log_choose_form hc.le, ← hRc]
    dsimp [L,R]
    rw [hkdiff, hd']
    have hs : (((N-c : ℕ) : ℝ)-(d : ℝ)) = 1 := by linarith only [hd']
    rw [hs]
    ring
  rw [hsum]
  refine ⟨by linarith, ?_⟩
  constructor
  · intro E
    apply (CMP.adjacent_supporting_line c k.val).2.mp
    change L k.val = CMP.logFactorial k.val
    linarith
  · intro h
    have heL : L k.val = CMP.logFactorial k.val :=
      (CMP.adjacent_supporting_line c k.val).2.mpr h
    have heR : R (N-k.val) = CMP.logFactorial (N-k.val) := by
      apply (CMP.adjacent_supporting_line d (N-k.val)).2.mpr
      rcases h with h | h
      · exact Or.inr (by omega)
      · exact Or.inl (by dsimp [d]; omega)
    rw [heL,heR,sub_self,sub_self,zero_add]

/-- Every retained interior cell is strictly above the chord through
two retained extremes. The chord therefore exposes only its endpoints. -/
theorem lower_chord {N : ℕ} {l c u : Fin (N+1)}
    (hl : l.val < c.val) (hu : c.val < u.val) :
    ((u.val : ℝ)-l.val) * Discrete3CMB.height N c >
      ((u.val : ℝ)-c.val) * Discrete3CMB.height N l +
      ((c.val : ℝ)-l.val) * Discrete3CMB.height N u := by
  have hcN : c.val < N := hu.trans_le (Nat.le_of_lt_succ u.isLt)
  let S := log ((N-c.val : ℕ) : ℝ) - log ((c.val : ℝ)+1)
  let H := Discrete3CMB.height N c
  have hcl : (l.val : ℝ) < c.val := by exact_mod_cast hl
  have hcu : (c.val : ℝ) < u.val := by exact_mod_cast hu
  have E₁ := adjacent_exposure hcN l
  have E₂ := adjacent_exposure hcN u
  change Discrete3CMB.height N l - H - S*((l.val : ℝ)-c.val) ≤ 0 ∧
    (Discrete3CMB.height N l - H - S*((l.val : ℝ)-c.val) = 0 ↔
      l.val = c.val ∨ l.val = c.val+1) at E₁
  change Discrete3CMB.height N u - H - S*((u.val : ℝ)-c.val) ≤ 0 ∧ _ at E₂
  have he : Discrete3CMB.height N l - H - S*((l.val : ℝ)-c.val) < 0 :=
    lt_of_le_of_ne E₁.1 (fun h ↦ by rcases E₁.2.mp h with h | h <;> omega)
  have H₁ := mul_neg_of_pos_of_neg (sub_pos.mpr hcu) he
  have H₂ := mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hcl.le) E₂.1
  change _ * H > _
  linarith only [H₁,H₂]

/-- On three cells with distinct successive secant slopes, two canonical
maximizers must be equal. -/
theorem identified {N : ℕ} {A : Finset (Fin (N+1))}
    {l c u : Fin (N+1)} (hl : l ∈ A) (hc : c ∈ A) (hu : u ∈ A)
    (hdet : ((c.val : ℝ)-l.val) * (Discrete3CMB.height N u - Discrete3CMB.height N c) ≠
      ((u.val : ℝ)-c.val) * (Discrete3CMB.height N c - Discrete3CMB.height N l))
    {m θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ)
    (hφ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ φ) : θ = φ := by
  let d := θ - φ
  let x : ℝ := (WithLp.ofLp d).1
  let y : ℝ := (WithLp.ofLp d).2
  have he (j : Fin (N+1)) : ⟪d, statistic j⟫ =
      (j.val : ℝ)*x + Discrete3CMB.height N j*y := by
    simp [statistic, x, y, WithLp.prod_inner_apply, RCLike.inner_apply]
  have H := Truncation.maximizers_constant_direction A statistic (fun _ ↦ 1)
    ⟨l,hl⟩ (fun _ _ ↦ one_pos) hθ hφ
  have H₁ := H l hl c hc
  have H₂ := H c hc u hu
  change ⟪d, statistic l⟫ = ⟪d, statistic c⟫ at H₁
  change ⟪d, statistic c⟫ = ⟪d, statistic u⟫ at H₂
  rw [he, he] at H₁ H₂
  let a := (c.val : ℝ)-l.val
  let b := Discrete3CMB.height N c - Discrete3CMB.height N l
  let e := (u.val : ℝ)-c.val
  let f := Discrete3CMB.height N u - Discrete3CMB.height N c
  have he₁ : a*x+b*y = 0 := by dsimp [a,b]; linarith
  have he₂ : e*x+f*y = 0 := by dsimp [e,f]; linarith
  have h : a*f-e*b ≠ 0 := sub_ne_zero.mpr hdet
  have hx : x = 0 := (mul_eq_zero.mp (show (a*f-e*b)*x = 0 by
    linear_combination f * he₁ - b * he₂)).resolve_left h
  have hy : y = 0 := (mul_eq_zero.mp (show (a*f-e*b)*y = 0 by
    linear_combination a * he₂ - e * he₁)).resolve_left h
  apply sub_eq_zero.mp
  apply WithLp.ofLp_injective 2
  exact Prod.ext hx hy

/-- Three different retained cells identify both finite canonical
coordinates, since their log-binomial statistics are noncollinear. -/
theorem three_cell_unique {N : ℕ} {A : Finset (Fin (N+1))}
    {l c u : Fin (N+1)} (hl : l ∈ A) (hc : c ∈ A) (hu : u ∈ A)
    (hlc : l.val < c.val) (hcu : c.val < u.val)
    {m θ φ : CMP.Plane}
    (hθ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ θ)
    (hφ : IsMaxOn (Truncation.logLik A statistic (fun _ ↦ 1) m) univ φ) : θ = φ := by
  refine identified hl hc hu (fun H ↦ ?_) hθ hφ
  have E := lower_chord hlc hcu
  linarith only [H,E]

/-- An exposed set of the conditional count/log-binomial hull carries
the limiting conditional tilt on that set. -/
theorem exposed_window_mass {N : ℕ} {A : Finset (Fin (N+1))}
    (θ d : CMP.Plane) (c : ℝ) (F : Finset A) (k : A) (hk : k ∈ F)
    (he : ∀ j ∈ F, ⟪d, statistic j.val⟫ = c)
    (hn : ∀ j ∉ F, ⟪d, statistic j.val⟫ < c) :
    Tendsto (fun t ↦ Discrete3CMB.rayMass
      (fun j : A ↦ ⟪θ, statistic j.val⟫) (fun j : A ↦ ⟪d, statistic j.val⟫-c) t k)
      atTop (𝓝 (exp ⟪θ, statistic k.val⟫ / ∑ j ∈ F, exp ⟪θ, statistic j.val⟫)) :=
  Discrete3CMB.exposed_mass _ _ F k hk
    (fun j hj ↦ sub_eq_zero.mpr (he j hj)) (fun j hj ↦ sub_neg.mpr (hn j hj))

/-- For observations retained by an exposed conditional face, its
likelihood is the strict limiting height along the exposing ray. -/
theorem exposed_window_supremum {N n : ℕ} {A : Finset (Fin (N+1))}
    (hN : 0 < n) (θ d : CMP.Plane) (c : ℝ) (F : Finset A) (x : Fin n → A)
    (hx : ∀ i, x i ∈ F) (he : ∀ j ∈ F, ⟪d, statistic j.val⟫ = c)
    (hn : ∀ j ∉ F, ⟪d, statistic j.val⟫ < c) (hout : ∃ j, j ∉ F) :
    let base := fun j : A ↦ ⟪θ, statistic j.val⟫
    let slope := fun j : A ↦ ⟪d, statistic j.val⟫-c
    let height := ∏ i, exp (base (x i)) / ∑ j ∈ F, exp (base j)
    let L := fun t ↦ ∏ i, Discrete3CMB.rayMass base slope t (x i)
    IsLUB (range L) height ∧ ∀ t, L t < height :=
  Discrete3CMB.exposed_supremum hN _ _ F x hx
    (fun j hj ↦ sub_eq_zero.mpr (he j hj)) (fun j hj ↦ sub_neg.mpr (hn j hj)) hout

/-- Comparing each slice's least upper bound retains all tied size
and parameter combinations that actually reach the greatest height. -/
theorem size_slice_winners {P : Type*} {S : Set ℕ} {D : ℕ → Set P}
    {L : ℕ → P → ℝ} {height : ℕ → ℝ}
    (hsup : ∀ N ∈ S, IsLUB (L N '' D N) (height N))
    {N : ℕ} {p : P} (hN : N ∈ S) (hp : p ∈ D N) :
    IsMaxOn (fun t : ℕ × P ↦ L t.1 t.2) {t | t.1 ∈ S ∧ t.2 ∈ D t.1} (N,p) ↔
      L N p = height N ∧ IsMaxOn height S N := by
  constructor
  · intro H
    have hub (j : ℕ) (hj : j ∈ S) : height j ≤ L N p :=
      (hsup j hj).2 (by rintro _ ⟨q,hq,rfl⟩; exact H (a := (j,q)) ⟨hj,hq⟩)
    have he := le_antisymm ((hsup N hN).1 ⟨p,hp,rfl⟩) (hub N hN)
    exact ⟨he, fun j hj ↦ (hub j hj).trans_eq he⟩
  · rintro ⟨he,H⟩ ⟨j,q⟩ ⟨hj,hq⟩
    exact ((hsup j hj).1 ⟨q,hq,rfl⟩).trans ((H hj).trans_eq he.symm)

end

end UnivariateML.Trunc3CMB

end
