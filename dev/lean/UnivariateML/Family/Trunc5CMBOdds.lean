module

public import UnivariateML.Family.Trunc3CMB

/-! Exact fixed-odds finite-carrier heights, with the saturated and flat
branches included in the same comparison. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc5CMBOdds

noncomputable section

/-- Conditional likelihood on the retained fixed-size carrier, at log odds
`η` and dispersion `ν`. -/
def oddsLikelihood {N n : ℕ} {A : Finset (Fin (N+1))}
    (x : Fin n → A) (η ν : ℝ) : ℝ :=
  ∏ i, Discrete3CMB.rayMass (fun k : A ↦ η * k.val.val)
    (fun k ↦ Discrete3CMB.height N k.val) ν (x i)

private theorem shift_mass {K : Type*} [Fintype K] (b s : K → ℝ) (c t : ℝ) (k : K) :
    Discrete3CMB.rayMass b (fun j ↦ s j-c) t k = Discrete3CMB.rayMass b s t k := by
  have he (j : K) : exp (b j + t*(s j-c)) = exp (-t*c) * exp (b j+t*s j) := by
    rw [← exp_add]
    congr 1
    ring
  simp only [Discrete3CMB.rayMass, he, ← Finset.mul_sum]
  rw [mul_div_mul_left _ _ (exp_pos _).ne']

private theorem sample_compare {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (η s t : ℝ) :
    let T := fun k : A ↦ Discrete3CMB.height N k.val
    let m := (∑ i, T (x i)) / n
    let f := Truncation.logLik (Finset.univ : Finset A) T
      (fun k ↦ exp (η * k.val.val)) m
    oddsLikelihood x η s ≤ oddsLikelihood x η t ↔ f s ≤ f t := by
  intro T m f
  let Z (v : ℝ) := ∑ k : A, exp (η*k.val.val + v*T k)
  have hZ (v : ℝ) : 0 < Z v :=
    Finset.sum_pos (fun _ _ ↦ exp_pos _) ⟨x ⟨0,hn⟩,Finset.mem_univ _⟩
  have hn' : 0 < (n : ℝ) := by exact_mod_cast hn
  have heq (v : ℝ) : oddsLikelihood x η v =
      exp ((∑ i, η*(x i).val.val) + n*f v) := by
    simp only [oddsLikelihood, Discrete3CMB.rayMass]
    change (∏ i, exp (η*(x i).val.val + v*T (x i)) / Z v) = _
    simp only [Finset.prod_div_distrib, ← exp_sum, Finset.sum_add_distrib, ← Finset.mul_sum,
      Finset.prod_const, Finset.card_univ, Fintype.card_fin]
    rw [← exp_log (hZ v), ← exp_nat_mul, ← exp_sub]
    congr 1
    dsimp [f,Truncation.logLik,Truncation.partition]
    have h : (∑ k : A, exp (η*k.val.val) * exp (T k*v)) = Z v := by
      apply Finset.sum_congr rfl
      intro k _
      rw [← exp_add,mul_comm (T k)]
    simp only [conj_trivial]
    change _ = _ + n*(m*v - log (∑ k : A, exp (η*k.val.val) * exp (T k*v)))
    rw [h]
    dsimp [m]
    field_simp
    ring
  rw [heq, heq, exp_le_exp, add_le_add_iff_left, mul_le_mul_iff_right₀ hn']

/-- CMB, prob and size held, `truncate:known` and `zero:truncated`,
criterion, failure and uniqueness in the interior-probability branch.
An interior statistic gives a unique optimum. A common statistic gives
the full dispersion fiber. At either nonflat extreme the supremum is the
likelihood of the odds carrier restricted to the corresponding extreme set. -/
theorem held_interior_odds {N n : ℕ} (hn : 0 < n) {A : Finset (Fin (N+1))}
    (x : Fin n → A) (η : ℝ) (lo hi : A)
    (hb : ∀ k : A, Discrete3CMB.height N k.val ∈
      Icc (Discrete3CMB.height N lo.val) (Discrete3CMB.height N hi.val)) :
    let T := fun k : A ↦ Discrete3CMB.height N k.val
    let m := (∑ i, T (x i)) / n
    let L := oddsLikelihood x η
    (T lo = T hi → ∀ ν, IsMaxOn L univ ν) ∧
    (T lo < T hi → ((∃! ν, IsMaxOn L univ ν) ↔ m ∈ Ioo (T lo) (T hi))) ∧
    (T lo < T hi → ∀ sign : ℝ, sign = 1 ∨ sign = -1 →
      let c := if sign = 1 then T hi else T lo
      let F := Finset.univ.filter (fun k : A ↦ T k = c)
      (∀ i, T (x i) = c) →
      let H := ∏ i, exp (η*(x i).val.val) / ∑ k ∈ F, exp (η*k.val.val)
      IsLUB (range L) H ∧ ∀ ν, L ν < H) := by
  classical
  intro T m L
  let b := fun k : A ↦ η * k.val.val
  let A' : Finset A := Finset.univ
  let f := Truncation.logLik A' T (fun k ↦ exp (b k)) m
  have hmax (ν : ℝ) : IsMaxOn L univ ν ↔ IsMaxOn f univ ν := by
    constructor <;> intro H s hs
    · exact (sample_compare hn x η s ν).mp (H hs)
    · exact (sample_compare hn x η s ν).mpr (H hs)
  refine ⟨?_, ?_, ?_⟩
  · intro he ν s _
    have hconst (k : A) : T k = T lo := le_antisymm ((hb k).2.trans_eq he.symm) (hb k).1
    have H (v : ℝ) : L v = ∏ i, Discrete3CMB.rayMass b (fun _ ↦ 0) v (x i) := by
      unfold L oddsLikelihood
      apply Finset.prod_congr rfl
      intro i _
      rw [← shift_mass b T (T lo)]
      simp only [hconst, sub_self]
    change L s ≤ L ν
    rw [H s, H ν]
    simp [Discrete3CMB.rayMass]
  · intro hlt
    simp only [hmax]
    exact Truncation.scalar_existsUnique_iff (fun _ _ ↦ exp_pos _) (Finset.mem_univ lo)
      (Finset.mem_univ hi) hlt (fun k _ ↦ hb k) m
  · intro hlt sign hs c F hx H
    let slope := fun k : A ↦ sign*(T k-c)
    have hF (k : A) : k ∈ F ↔ T k = c := by simp [F]
    have hout : ∃ k : A, k ∉ F := by
      rcases hs with rfl | rfl
      · exact ⟨lo, fun h ↦ hlt.ne (by simpa [c, show (-1 : ℝ) ≠ 1 from by norm_num] using (hF lo).mp h)⟩
      · exact ⟨hi, fun h ↦ hlt.ne' (by simpa [c, show (-1 : ℝ) ≠ 1 from by norm_num] using (hF hi).mp h)⟩
    have hslope (k : A) (hk : k ∉ F) : slope k < 0 := by
      have hn := fun h ↦ hk ((hF k).mpr h)
      rcases hs with rfl | rfl
      · have H := lt_of_le_of_ne (hb k).2 (by simpa [c, show (-1 : ℝ) ≠ 1 from by norm_num] using hn)
        simpa [slope,c, show (-1 : ℝ) ≠ 1 from by norm_num] using sub_neg.mpr H
      · have H := lt_of_le_of_ne (hb k).1 (by simpa [c, show (-1 : ℝ) ≠ 1 from by norm_num] using Ne.symm hn)
        simpa [slope,c, show (-1 : ℝ) ≠ 1 from by norm_num] using sub_pos.mpr H
    have hs0 (k : A) (hk : k ∈ F) : slope k = 0 := by simp [slope,(hF k).mp hk]
    obtain ⟨hL,hstrict⟩ := Discrete3CMB.exposed_supremum hn b slope F x
      (fun i ↦ (hF _).mpr (hx i)) hs0 hslope hout
    have he (t : ℝ) : (∏ i, Discrete3CMB.rayMass b slope t (x i)) = L (sign*t) := by
      apply Finset.prod_congr rfl
      intro i _
      rw [← shift_mass b T c]
      simp only [Discrete3CMB.rayMass,slope,mul_left_comm t sign, ← mul_assoc]
    simp_rw [he] at hL hstrict
    have hsign : sign*sign = 1 := by rcases hs with rfl | rfl <;> norm_num
    have hpre (v : ℝ) : sign*(sign*v) = v := by rw [← mul_assoc,hsign,one_mul]
    have hRange : range (fun t ↦ L (sign*t)) = range L := by
      ext y
      constructor
      · rintro ⟨t,rfl⟩
        exact ⟨sign*t,rfl⟩
      · rintro ⟨v,rfl⟩
        exact ⟨sign*v,by change L (sign*(sign*v)) = L v; rw [hpre]⟩
    exact ⟨hRange ▸ hL, fun v ↦ by simpa only [hpre] using hstrict (sign*v)⟩

/-- CMB, nu and size held, `truncate:known` and `zero:truncated`,
criterion and failure at a retained count extreme. The height of a
constant sample is one, strictly above every finite-odds likelihood in
a nonsingleton window. The atom reaches it in the parent chart exactly
at a true parent endpoint. -/
theorem held_dispersion_extreme {N l u n : ℕ} (hlu : l < u) (_huN : u ≤ N) (hn : 0 < n)
    (ν : ℝ) (upper : Bool) :
    let k := if upper then u else l
    let h := fun j : ℕ ↦ exp (ν * log (N.choose j : ℝ))
    let f := TruncCount.finiteCountMass l u h k
    let L := fun p : Sum ℝ Bool ↦ match p with
      | .inl t => f t ^ n
      | .inr b => if b = upper then 1 else 0
    let D := TruncCount.finiteCountDomain (decide (l = 0)) (decide (u = N))
    Tendsto f (if upper then atTop else atBot) (𝓝 1) ∧
    IsLUB (L '' D) 1 ∧
    (∀ p ∈ D, IsMaxOn L D p ↔ p = .inr upper) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (if upper then u = N else l = 0)) := by
  intro k h f L D
  have hk : k ∈ Finset.Icc l u := by
    cases upper <;> simp [k,hlu.le]
  have ho : ∃ j ∈ Finset.Icc l u, j ≠ k := by
    cases upper
    · exact ⟨u,Finset.mem_Icc.mpr ⟨hlu.le,le_rfl⟩,hlu.ne'⟩
    · exact ⟨l,Finset.mem_Icc.mpr ⟨le_rfl,hlu.le⟩,hlu.ne⟩
  have hlim : Tendsto f (if upper then atTop else atBot) (𝓝 1) := by
    have H := Trunc3CMB.held_dispersion_limits (N := N) hlu.le ν hk
    cases upper
    · simpa [k,f,h] using H.1
    · simpa [k,f,h] using H.2
  have hlt (t : ℝ) : f t ^ n < 1 := by
    let w := fun j : ℕ ↦ h j * exp ((j:ℝ)*t)
    obtain ⟨j,hj,hjne⟩ := ho
    have hp (j : ℕ) : 0 < w j := mul_pos (exp_pos _) (exp_pos _)
    have hs : w k < ∑ i ∈ Finset.Icc l u, w i := by
      have H := Finset.sum_lt_sum_of_subset (f := w)
        (Finset.singleton_subset_iff.mpr hk) hj (by simpa using hjne) (hp j)
        (fun i _ _ ↦ (hp i).le)
      simpa only [Finset.sum_singleton] using H
    have hZ := (hp k).trans hs
    exact pow_lt_one₀ (div_nonneg (hp k).le hZ.le) ((div_lt_one hZ).mpr hs) hn.ne'
  have hs : IsLUB (L '' D) 1 := by
    constructor
    · rintro _ ⟨t | b,_,rfl⟩
      · exact (hlt t).le
      · change (if b = upper then (1:ℝ) else 0) ≤ 1
        split <;> norm_num
    · intro B hB
      have H (t : ℝ) : f t ^ n ≤ B := hB ⟨.inl t,trivial,rfl⟩
      cases upper
      · have hh : Tendsto f atBot (𝓝 1) := hlim
        simpa only [one_pow] using le_of_tendsto (hh.pow n) (Eventually.of_forall H)
      · have hh : Tendsto f atTop (𝓝 1) := hlim
        simpa only [one_pow] using le_of_tendsto (hh.pow n) (Eventually.of_forall H)
  have hwin : ∀ p ∈ D, IsMaxOn L D p ↔ p = .inr upper := by
    intro p hp
    have he : IsMaxOn L D p ↔ L p = 1 := by
      constructor
      · intro H
        exact le_antisymm (hs.1 ⟨p,hp,rfl⟩) (hs.2 (by rintro _ ⟨q,hq,rfl⟩; exact H hq))
      · intro H q hq
        change L q ≤ L p
        rw [H]
        exact hs.1 ⟨q,hq,rfl⟩
    rw [he]
    cases p with
    | inl t => exact iff_of_false (hlt t).ne (by simp)
    | inr b =>
      change (if b = upper then (1:ℝ) else 0) = 1 ↔ _
      by_cases h : b = upper <;> simp [h]
  refine ⟨hlim,hs,hwin,?_⟩
  have he : (Sum.inr upper : Sum ℝ Bool) ∈ D ↔ (if upper then u = N else l = 0) := by
    cases upper <;> simp [D,TruncCount.finiteCountDomain]
  constructor
  · rintro ⟨p,hp,H⟩
    exact he.mp (((hwin p hp).mp H) ▸ hp)
  · intro h
    have hh := he.mpr h
    exact ⟨.inr upper,hh,(hwin _ hh).mpr rfl⟩

end

end UnivariateML.Trunc5CMBOdds

end
