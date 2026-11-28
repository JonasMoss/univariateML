module

public import UnivariateML.Family.Trunc3CMB

/-! Tagged canonical CMB chart, including compatible deterministic endpoints.
The retained carrier is the effective window at the specified size. -/

@[expose] public section

open Set Real Filter
open scoped Topology RealInnerProductSpace

namespace UnivariateML.Trunc5CMBChart

noncomputable section

/-- The parent endpoint represented by an atom tag. -/
def endpoint (N : ℕ) (b : Bool) : Fin (N+1) :=
  if b then Fin.last N else 0

/-- The fixed-size chart with unrestricted finite coordinates and
deterministic endpoint tags when the nominal window retains them. -/
def chart {N : ℕ} (A : Finset (Fin (N+1))) : Set (Sum CMP.Plane Bool) :=
  {p | match p with
    | .inl _ => True
    | .inr b => endpoint N b ∈ A}

open Classical in
/-- Sample likelihood in the full tagged fixed-size chart. -/
def sampleLikelihood {N n : ℕ} {A : Finset (Fin (N+1))}
    (x : Fin n → A) : Sum CMP.Plane Bool → ℝ
  | .inl θ => ∏ i, exp ⟪θ, Trunc3CMB.statistic (x i).val⟫ /
      ∑ k ∈ A, exp ⟪θ, Trunc3CMB.statistic k⟫
  | .inr b => if ∀ i, (x i).val = endpoint N b then 1 else 0

/-- Sample mean of the canonical count and log-binomial statistics. -/
def statisticMean {N n : ℕ} {A : Finset (Fin (N+1))} (x : Fin n → A) : CMP.Plane :=
  (n : ℝ)⁻¹ • ∑ i, Trunc3CMB.statistic (x i).val

private theorem canonical_eq {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (θ : CMP.Plane) :
    sampleLikelihood x (.inl θ) =
      exp (n * Truncation.logLik A Trunc3CMB.statistic (fun _ ↦ 1) (statisticMean x) θ) := by
  let Z := ∑ k ∈ A, exp ⟪θ, Trunc3CMB.statistic k⟫
  have hZ : 0 < Z := Finset.sum_pos (fun _ _ ↦ exp_pos _)
    ⟨(x ⟨0,hn⟩).val,(x ⟨0,hn⟩).property⟩
  have hn' : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  change (∏ i, exp ⟪θ, Trunc3CMB.statistic (x i).val⟫ / Z) = _
  simp only [Finset.prod_div_distrib, ← exp_sum, Finset.prod_const, Finset.card_univ,
    Fintype.card_fin]
  rw [← exp_log hZ, ← exp_nat_mul, ← exp_sub]
  congr 1
  unfold statisticMean Truncation.logLik Truncation.partition
  simp only [one_mul, real_inner_smul_right, inner_sum]
  change _ = (n : ℝ)*((n : ℝ)⁻¹ * (∑ i, ⟪θ, Trunc3CMB.statistic (x i).val⟫) - log Z)
  field_simp

private theorem atom_score {N n : ℕ} {A : Finset (Fin (N+1))}
    (x : Fin n → A) (b : Bool) (hx : ∀ i, (x i).val = endpoint N b) :
    sampleLikelihood x (.inr b) = 1 := by simp [sampleLikelihood,hx]

private theorem canonical_le_one {N n : ℕ} {A : Finset (Fin (N+1))}
    (x : Fin n → A) (θ : CMP.Plane) :
    sampleLikelihood x (.inl θ) ≤ 1 := by
  change (∏ i, exp ⟪θ, Trunc3CMB.statistic (x i).val⟫ /
      ∑ k ∈ A, exp ⟪θ, Trunc3CMB.statistic k⟫) ≤ 1
  apply Finset.prod_le_one₀
  · intro i _
    exact div_nonneg (exp_pos _).le (Finset.sum_nonneg fun _ _ ↦ (exp_pos _).le)
  · intro i _
    have H := Finset.single_le_sum (f := fun k ↦ exp ⟪θ, Trunc3CMB.statistic k⟫)
      (fun _ _ ↦ (exp_pos _).le) (x i).property
    exact (div_le_one ((exp_pos _).trans_le H)).mpr H

/-- CMB, size held, `truncate:known` and `zero:truncated`, criterion
and uniqueness. This gives the maximizing set with endpoint tags and
the finite hull test in the same sample chart. A retained singleton
admits its entire compatible chart. In the absence of a compatible
endpoint, finite maximizers are exactly those in the canonical chart. -/
theorem fixed_size_maxima {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) :
    let L := sampleLikelihood x
    let D := chart A
    let f := Truncation.logLik A Trunc3CMB.statistic (fun _ ↦ 1) (statisticMean x)
    let constantEndpoint := fun b ↦ endpoint N b ∈ A ∧ ∀ i, (x i).val = endpoint N b
    (∀ b, (Sum.inr b ∈ D ∧ IsMaxOn L D (.inr b)) ↔ constantEndpoint b) ∧
    (∀ θ, IsMaxOn L D (.inl θ) ↔ IsMaxOn f univ θ ∧
      (∀ b, constantEndpoint b → ∀ k ∈ A, k = endpoint N b)) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∃ b, constantEndpoint b) ∨
      statisticMean x ∈ intrinsicInterior ℝ
        (convexHull ℝ (Trunc3CMB.statistic '' (A : Set (Fin (N+1)))))) := by
  classical
  intro L D f E
  have hA : A.Nonempty := ⟨(x ⟨0,hn⟩).val,(x ⟨0,hn⟩).property⟩
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have hatom (b : Bool) : (Sum.inr b ∈ D ∧ IsMaxOn L D (.inr b)) ↔ E b := by
    constructor
    · rintro ⟨hb,H⟩
      refine ⟨hb,?_⟩
      by_contra h
      have hpos : 0 < L (.inl 0) := by
        change 0 < sampleLikelihood x (.inl 0)
        rw [canonical_eq hn]
        exact exp_pos _
      have he : L (.inr b) = 0 := by simp [L,sampleLikelihood,h]
      exact hpos.not_ge (he ▸ H (a := .inl 0) trivial)
    · rintro ⟨hb,h⟩
      refine ⟨hb,?_⟩
      intro p _
      change L p ≤ L (.inr b)
      rw [show L (.inr b) = 1 from atom_score x b h]
      cases p with
      | inl θ => exact canonical_le_one x θ
      | inr v =>
        change (if ∀ i, (x i).val = endpoint N v then (1:ℝ) else 0) ≤ 1
        split <;> norm_num
  have hfinite (θ : CMP.Plane) : IsMaxOn L D (.inl θ) ↔ IsMaxOn f univ θ ∧
      (∀ b, E b → ∀ k ∈ A, k = endpoint N b) := by
    constructor
    · intro H
      refine ⟨fun φ _ ↦ ?_,?_⟩
      · have hh := H (a := .inl φ) trivial
        change sampleLikelihood x (.inl φ) ≤ sampleLikelihood x (.inl θ) at hh
        rwa [canonical_eq hn,canonical_eq hn,exp_le_exp,mul_le_mul_iff_right₀ hn'] at hh
      · intro b hb k hk
        by_contra hne
        have hstrict := Trunc3CMB.endpoint_strictly_wins hn b ⟨k,hk,hne⟩ hb.1 θ
        have hEq : L (.inl θ) =
            (exp ⟪θ, Trunc3CMB.statistic (endpoint N b)⟫ /
              ∑ j ∈ A, exp ⟪θ, Trunc3CMB.statistic j⟫)^n := by
          change sampleLikelihood x (.inl θ) = _
          simp [sampleLikelihood,hb.2,div_pow]
        have hbad := H (a := .inr b) hb.1
        change L (.inr b) ≤ L (.inl θ) at hbad
        rw [show L (.inr b) = 1 from atom_score x b hb.2,hEq] at hbad
        exact hstrict.not_ge hbad
    · rintro ⟨H,hflat⟩ (φ | b) hp
      · change sampleLikelihood x (.inl φ) ≤ sampleLikelihood x (.inl θ)
        rw [canonical_eq hn,canonical_eq hn,exp_le_exp,mul_le_mul_iff_right₀ hn']
        exact H (mem_univ φ)
      · by_cases hb : ∀ i, (x i).val = endpoint N b
        · have he := atom_score x b hb
          change sampleLikelihood x (.inr b) ≤ sampleLikelihood x (.inl θ)
          rw [he]
          have hA' : A = {endpoint N b} := by
            ext k
            exact ⟨fun hk ↦ Finset.mem_singleton.mpr (hflat b ⟨hp,hb⟩ k hk),
              fun hk ↦ Finset.mem_singleton.mp hk ▸ hp⟩
          simp [sampleLikelihood,hA',hb,(exp_pos _).ne']
        · change sampleLikelihood x (.inr b) ≤ sampleLikelihood x (.inl θ)
          rw [show sampleLikelihood x (.inr b) = 0 by simp [sampleLikelihood,hb],
            canonical_eq hn]
          exact (exp_pos _).le
  refine ⟨hatom,hfinite,?_⟩
  constructor
  · rintro ⟨θ | b,hp,H⟩
    · exact Or.inr ((Trunc3CMB.canonical_fit hA _).mp ⟨θ,((hfinite θ).mp H).1⟩)
    · exact Or.inl ⟨b,(hatom b).mp ⟨hp,H⟩⟩
  · rintro (⟨b,hb⟩ | H)
    · exact ⟨.inr b,(hatom b).mpr hb⟩
    · by_cases hb : ∃ b, E b
      · obtain ⟨b,hb⟩ := hb
        exact ⟨.inr b,(hatom b).mpr hb⟩
      · obtain ⟨θ,hθ⟩ := (Trunc3CMB.canonical_fit hA _).mpr H
        exact ⟨.inl θ,trivial,(hfinite θ).mpr ⟨hθ,fun b h ↦ (hb ⟨b,h⟩).elim⟩⟩

/-- Conditional likelihood on a specified face of the retained carrier. -/
def faceLikelihood {N n : ℕ} {A : Finset (Fin (N+1))}
    (F : Finset A) (x : Fin n → A) (θ : CMP.Plane) : ℝ :=
  ∏ i, exp ⟪θ, Trunc3CMB.statistic (x i).val⟫ /
    ∑ k ∈ F, exp ⟪θ, Trunc3CMB.statistic k.val⟫

/-- CMB, size held, `truncate:known` and `zero:truncated`, failure
and criterion on a proper exposed face. A maximizing tilt of the
face gives the global slice height and no finite canonical point
reaches it. A deterministic tag reaches it exactly when the tag's
parent endpoint is retained and agrees with every observation. -/
theorem fixed_size_face_height {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (F : Finset A) (hx : ∀ i, x i ∈ F)
    (θ d : CMP.Plane) (c : ℝ)
    (he : ∀ j ∈ F, ⟪d, Trunc3CMB.statistic j.val⟫ = c)
    (hlt : ∀ j ∉ F, ⟪d, Trunc3CMB.statistic j.val⟫ < c) (hout : ∃ j, j ∉ F)
    (hθ : IsMaxOn (faceLikelihood F x) univ θ) :
    let H := faceLikelihood F x θ
    IsLUB (sampleLikelihood x '' chart A) H ∧
    (∀ φ, sampleLikelihood x (.inl φ) < H) ∧
    (∀ b, Sum.inr b ∈ chart A →
      (sampleLikelihood x (.inr b) = H ↔ ∀ i, (x i).val = endpoint N b)) := by
  classical
  intro H
  let B (φ : CMP.Plane) (k : A) := ⟪φ, Trunc3CMB.statistic k.val⟫
  have hp : 0 < H := by
    unfold H faceLikelihood
    exact Finset.prod_pos fun _ _ ↦ div_pos (exp_pos _)
      (Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨x ⟨0,hn⟩,hx ⟨0,hn⟩⟩)
  have hcanon (φ : CMP.Plane) :
      sampleLikelihood x (.inl φ) = faceLikelihood Finset.univ x φ := by
    simp only [sampleLikelihood, faceLikelihood]
    rw [← Finset.sum_attach]
    rfl
  have hs (φ : CMP.Plane) : sampleLikelihood x (.inl φ) < H := by
    rw [hcanon]
    refine lt_of_lt_of_le ?_ (hθ (mem_univ φ))
    unfold faceLikelihood
    have hZ : 0 < ∑ k ∈ F, exp (B φ k) :=
      Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨x ⟨0,hn⟩,hx ⟨0,hn⟩⟩
    obtain ⟨j,hj⟩ := hout
    have hlt' : (∑ k ∈ F, exp (B φ k)) < ∑ k : A, exp (B φ k) :=
      Finset.sum_lt_sum_of_subset (Finset.subset_univ _) (Finset.mem_univ j) hj
        (exp_pos _) (fun _ _ _ ↦ (exp_pos _).le)
    apply Finset.prod_lt_prod_of_nonempty₀
    · exact fun _ _ ↦ div_pos (exp_pos _) (hZ.trans hlt')
    · exact fun _ _ ↦ div_lt_div_of_pos_left (exp_pos _) hZ hlt'
    · exact ⟨⟨0,hn⟩,Finset.mem_univ _⟩
  have hAtom (b : Bool) (hb : ∀ i, (x i).val = endpoint N b) : H = 1 := by
    have hF : F = {x ⟨0,hn⟩} := by
      by_contra hne
      have hex : ∃ j ∈ F, j ≠ x ⟨0,hn⟩ := by
        by_contra! h
        apply hne
        ext j
        constructor
        · exact fun hj ↦ Finset.mem_singleton.mpr (h j hj)
        · rintro hj
          rw [Finset.mem_singleton.mp hj]
          exact hx _
      obtain ⟨j,hj,hjne⟩ := hex
      have hX (i : Fin n) : x i = x ⟨0,hn⟩ := Subtype.ext ((hb i).trans (hb _).symm)
      -- If the face had another cell, increasing a count tilt towards the
      -- parent endpoint would strictly improve even the face likelihood.
      let e : CMP.Plane := WithLp.toLp 2 ((if b then 1 else -1 : ℝ),0)
      let w (k : A) := exp (B θ k-B θ (x ⟨0,hn⟩))
      have heq (φ : CMP.Plane) :
          faceLikelihood F x φ = (1 / (∑ k ∈ F, exp (B φ k-B φ (x ⟨0,hn⟩))))^n := by
        have he (k : A) : exp (B φ k-B φ (x ⟨0,hn⟩)) =
            exp (-(B φ (x ⟨0,hn⟩))) * exp (B φ k) := by rw [← exp_add]; congr 1; ring
        simp only [faceLikelihood,hX,Finset.prod_const,Finset.card_univ,Fintype.card_fin]
        rw [show (∑ k ∈ F, exp (B φ k-B φ (x ⟨0,hn⟩))) =
            exp (-(B φ (x ⟨0,hn⟩))) * ∑ k ∈ F, exp (B φ k) by simp only [he,Finset.mul_sum]]
        rw [one_div, mul_inv_rev, ← exp_neg, neg_neg, div_eq_mul_inv, mul_comm]
      have hbval (k : A) : B e k = (if b then 1 else -1 : ℝ) * k.val.val := by
        simp [B, e, Trunc3CMB.statistic, WithLp.prod_inner_apply, RCLike.inner_apply, mul_comm]
      have hE (k : A) : B e k ≤ B e (x ⟨0,hn⟩) := by
        rw [hbval, hbval, hb ⟨0,hn⟩]
        cases b
        · simp [endpoint]
        · simpa [endpoint] using
            (show (k.val.val : ℝ) ≤ N by exact_mod_cast Nat.le_of_lt_succ k.val.isLt)
      have hE' : B e j < B e (x ⟨0,hn⟩) := by
        rw [hbval, hbval, hb ⟨0,hn⟩]
        have hjv : j.val.val ≠ (endpoint N b).val :=
          fun h ↦ hjne (Subtype.ext ((Fin.ext h).trans (hb ⟨0,hn⟩).symm))
        cases b
        · simpa [endpoint] using
            (show (0 : ℝ) < j.val.val by exact_mod_cast Nat.pos_of_ne_zero hjv)
        · simpa [endpoint] using
            (show (j.val.val : ℝ) < N by
              exact_mod_cast lt_of_le_of_ne (Nat.le_of_lt_succ j.val.isLt) hjv)
      have hshift (k : A) :
          B (θ+e) k-B (θ+e) (x ⟨0,hn⟩) =
          B θ k-B θ (x ⟨0,hn⟩)+(B e k-B e (x ⟨0,hn⟩)) := by
        simp only [B,inner_add_left]
        ring
      have hz : (∑ k ∈ F, exp (B (θ+e) k-B (θ+e) (x ⟨0,hn⟩))) <
          ∑ k ∈ F, w k := by
        apply Finset.sum_lt_sum
        · intro k _
          rw [hshift,exp_le_exp]
          linarith [hE k]
        · refine ⟨j,hj,?_⟩
          rw [hshift,exp_lt_exp]
          linarith
      have hpos : 0 < ∑ k ∈ F, exp (B (θ+e) k-B (θ+e) (x ⟨0,hn⟩)) :=
        Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨x ⟨0,hn⟩,hx _⟩
      have hbad := hθ (mem_univ (θ+e))
      change faceLikelihood F x (θ+e) ≤ faceLikelihood F x θ at hbad
      rw [heq,heq] at hbad
      exact (pow_lt_pow_left₀ (div_lt_div_of_pos_left zero_lt_one hpos hz)
        (div_nonneg zero_le_one (hpos.trans hz).le) hn.ne').not_ge hbad
    have hX (i : Fin n) : x i = x ⟨0,hn⟩ := Subtype.ext ((hb i).trans (hb _).symm)
    simp [H,faceLikelihood,hF,hX,(exp_pos _).ne']
  have htag (b : Bool) :
      sampleLikelihood x (.inr b) ≤ H ∧
        (sampleLikelihood x (.inr b) = H ↔ ∀ i, (x i).val = endpoint N b) := by
    by_cases hb : ∀ i, (x i).val = endpoint N b
    · rw [atom_score x b hb,hAtom b hb]
      simp [hb]
    · rw [show sampleLikelihood x (.inr b) = 0 by simp [sampleLikelihood,hb]]
      simp [hp.le,hb,ne_of_lt hp]
  refine ⟨⟨?_,?_⟩,hs,fun b _ ↦ (htag b).2⟩
  · rintro _ ⟨φ | b,_,rfl⟩
    · exact (hs φ).le
    · exact (htag b).1
  · intro Y hY
    have hlim := (Trunc3CMB.exposed_window_supremum hn θ d c F x hx he hlt hout).1
    apply hlim.2
    rintro _ ⟨t,rfl⟩
    have hEq (k : A) :
        Discrete3CMB.rayMass (B θ) (fun j ↦ B d j-c) t k =
          exp (B (θ+t • d) k) / ∑ j : A, exp (B (θ+t • d) j) := by
      have he (j : A) : exp (B θ j+t*(B d j-c)) =
          exp (-t*c) * exp (B (θ+t • d) j) := by
        rw [← exp_add]
        congr 1
        simp only [B,inner_add_left,real_inner_smul_left]
        ring
      simp only [Discrete3CMB.rayMass,he, ← Finset.mul_sum]
      rw [mul_div_mul_left _ _ (exp_pos _).ne']
    change (∏ i, Discrete3CMB.rayMass (B θ) (fun j ↦ B d j-c) t (x i)) ≤ Y
    simp only [hEq]
    exact hY ⟨.inl (θ+t • d),trivial,(hcanon _).trans rfl⟩

/-- CMB, size held, `truncate:known`, criterion and uniqueness on a
mixed two-cell window. All maximizers are finite and form the entire
empirical-odds affine fiber. -/
theorem fixed_size_mixed_pair {N n : ℕ} (hn : 0 < n)
    {l u : Fin (N+1)} (hne : l ≠ u) (x : Fin n → ({l,u} : Finset (Fin (N+1))))
    (hl : ∃ i, (x i).val = l) (hu : ∃ i, (x i).val = u)
    {a b : ℝ} (ha : 0 < a) (hb : 0 < b) (hab : a+b = 1)
    (hm : statisticMean x = a • Trunc3CMB.statistic l + b • Trunc3CMB.statistic u) :
    ∀ p ∈ chart ({l,u} : Finset (Fin (N+1))), IsMaxOn (sampleLikelihood x) (chart {l,u}) p ↔
      ∃ θ, p = .inl θ ∧
        ⟪θ, Trunc3CMB.statistic u⟫ - ⟪θ, Trunc3CMB.statistic l⟫ = log (b/a) := by
  obtain ⟨i,hi⟩ := hl
  obtain ⟨j,hj⟩ := hu
  have hnatom (p : Bool) : ¬ (∀ k, (x k).val = endpoint N p) := by
    intro H
    exact hne (hi.symm.trans ((H i).trans ((H j).symm.trans hj)))
  intro p hp
  obtain ⟨hatom,hfin,_⟩ := fixed_size_maxima hn x
  cases p with
  | inl θ =>
    have H := hfin θ
    dsimp only at H
    rw [hm, Trunc3CMB.pair_empirical_odds hne ha hb hab] at H
    exact H.trans ⟨fun h ↦ ⟨θ,rfl,h.1⟩,
      fun ⟨φ,hφ,hd⟩ ↦ ⟨Sum.inl.inj hφ ▸ hd,fun p hp ↦ (hnatom p hp.2).elim⟩⟩
  | inr p =>
    constructor
    · intro H
      exact (hnatom p ((hatom p).mp ⟨hp,H⟩).2).elim
    · rintro ⟨θ,h,_⟩
      cases h

end

end UnivariateML.Trunc5CMBChart

end
