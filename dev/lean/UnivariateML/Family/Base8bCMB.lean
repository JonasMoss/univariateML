module

public import UnivariateML.Family.Base8CMB
public import UnivariateML.Family.JoinsCMB
public import UnivariateML.Family.Trunc3CMB
public import UnivariateML.Family.BinomialSize
public import UnivariateML.Family.BetaBinomial
public import UnivariateML.Generic.Truncation
public import Mathlib.FieldTheory.AlgebraicClosure
import Mathlib.Topology.Order.Lattice
import Mathlib.Analysis.LocallyConvex.Separation
import Mathlib.Analysis.Normed.Affine.AddTorsorBases

/-!
# Capped Conway–Maxwell–binomial profiles

The cap is a natural parameter. The implemented cap is its specialization to
(10^7).

## Deviations

The ordinary attained strata have a conditional maximizer graph. At rational
dispersion the odds root and its height are algebraic, and the capped height
comparison retains every co-winner. The exact tied real hold is specified by the first zero of the profile difference. The full-support
saturation tests and an executable algebraic isolation procedure remain open.
-/

set_option linter.style.header false

@[expose] public section

open Set Filter Topology
open scoped BigOperators

namespace UnivariateML.Base8bCMB

open Discrete3CMB Discrete4CMB

private theorem pair_bound {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N+1)) {l u : Fin (N+1)} (hne : l ≠ u)
    (hx : ∀ i, x i = l ∨ x i = u) (hl : 0 < Discrete3CMB.multiplicity x l) (hu : 0 < Discrete3CMB.multiplicity x u)
    (p ν : ℝ) :
    closedLikelihood x p ν < pairHeight (Discrete3CMB.multiplicity x l) (Discrete3CMB.multiplicity x u) := by
  classical
  let z := Discrete3CMB.multiplicity x l
  let o := Discrete3CMB.multiplicity x u
  let B := pairHeight z o
  have hB : 0 < B := by unfold B pairHeight; positivity
  have occurs {k : Fin (N+1)} (h : 0 < Discrete3CMB.multiplicity x k) :
      ∃ i, x i = k := by
    obtain ⟨i, hi⟩ := Finset.card_pos.mp h
    exact ⟨i, (Finset.mem_filter.mp hi).2⟩
  have hnc (k : Fin (N+1)) : ¬ ∀ i, x i = k := by
    obtain ⟨i, hi⟩ := occurs hl
    obtain ⟨j, hj⟩ := occurs hu
    intro hall
    exact hne (hi.symm.trans ((hall i).trans ((hall j).symm.trans hj)))
  by_cases hp0 : p = 0
  · simpa [closedLikelihood, hp0, hnc] using hB
  by_cases hp1 : p = 1
  · simpa [closedLikelihood, hp0, hp1, hnc] using hB
  let η := Real.log (p/(1-p))
  let base (k : Fin (N+1)) : ℝ := η*k.val + ν * height N k
  let F : Finset (Fin (N+1)) := {l,u}
  let slope (k : Fin (N+1)) : ℝ := if k ∈ F then 0 else -1
  have hcard : F.card = 2 := by simp [F, hne]
  have hout : ∃ k, k ∉ F := by
    by_contra! h
    have he : F = Finset.univ := Finset.eq_univ_of_forall h
    rw [he, Finset.card_univ, Fintype.card_fin] at hcard
    omega
  have hm (i : Fin n) : x i ∈ F := by simpa [F] using hx i
  have H := (exposed_supremum hn base slope F x hm
    (fun k hk ↦ by simp [slope, hk]) (fun k hk ↦ by simp [slope, hk]) hout).2 0
  simp only [rayMass, zero_mul, add_zero] at H
  rw [closedLikelihood, ite_eq_right hp0, ite_eq_right hp1]
  change (∏ i, Real.exp (base (x i)) / ∑ j, Real.exp (base j)) < B
  refine H.trans_le ?_
  let y (i : Fin n) : Fin 2 := if x i = l then 0 else 1
  have hy0 (i : Fin n) : y i = 0 ↔ x i = l := by by_cases h : x i = l <;> simp [y, h]
  have hy1 (i : Fin n) : y i = 1 ↔ x i = u := by
    rcases hx i with h | h <;> simp [y, h, hne, hne.symm]
  have hzo : Discrete3CMB.multiplicity y 0 = z ∧ Discrete3CMB.multiplicity y 1 = o := by
    unfold Discrete3CMB.multiplicity z o
    simp only [hy0, hy1]
    exact ⟨rfl, rfl⟩
  let den := Real.exp (base l) + Real.exp (base u)
  let q := Real.exp (base u)/den
  have hden : 0 < den := add_pos (Real.exp_pos _) (Real.exp_pos _)
  have hq : q ∈ Icc 0 1 :=
    ⟨div_nonneg (Real.exp_pos _).le hden.le,
      (div_le_one hden).mpr (le_add_of_nonneg_left (Real.exp_pos _).le)⟩
  have hsub : 1 - q = Real.exp (base l)/den := by
    dsimp [q]
    field_simp
    simp [den]
  have hF : (∑ j ∈ F, Real.exp (base j)) = den := by
    simp [F, Finset.sum_pair hne, den]
  have hzR : 0 < (z : ℝ) := Nat.cast_pos.mpr hl
  have hoR : 0 < (o : ℝ) := Nat.cast_pos.mpr hu
  let q₀ := (o : ℝ)/(o+z)
  have hq₀ : q₀ ∈ Icc 0 1 :=
    ⟨div_nonneg hoR.le (add_pos hoR hzR).le,
      (div_le_one (add_pos hoR hzR)).mpr (le_add_of_nonneg_right hzR.le)⟩
  have hsub₀ : 1 - q₀ = (z : ℝ)/(z+o) := by
    dsimp [q₀]
    rw [add_comm (z : ℝ) o]
    field_simp
    ring
  have hprod :
      (∏ i, Real.exp (base (x i)) / ∑ j ∈ F, Real.exp (base j)) =
        (1-q)^z * q^o := by
    have he (i : Fin n) : Real.exp (base (x i)) / (∑ j ∈ F, Real.exp (base j)) =
        if x i = l then 1-q else q := by
      rcases hx i with h | h <;> simp [h, hne.symm, hF, hsub, q]
    have hf : (Finset.univ.filter (fun i ↦ ¬x i = l)) =
        Finset.univ.filter (fun i ↦ x i = u) := by
      ext i
      rcases hx i with h | h <;> simp [h, hne, hne.symm]
    simp_rw [he]
    rw [Finset.prod_ite, hf, Finset.prod_const, Finset.prod_const]
    rfl
  rw [hprod, ← hzo.1, ← hzo.2, mul_comm, ← bernoulli_likelihood y hq (0 : ℝ)]
  have hmax := (bernoulli_fit hn y (0 : ℝ) hq₀).mpr
    (by rw [hzo.1, hzo.2])
  refine (hmax hq).trans_eq ?_
  change closedLikelihood y q₀ 0 = B
  rw [bernoulli_likelihood y hq₀, hzo.1, hzo.2, hsub₀]
  simp [B, pairHeight, q₀, add_comm (o : ℝ) z, mul_comm]

private theorem integer_pair_bound {n N l u : ℕ} (hn : 0 < n) (hN : 2 ≤ N)
    (x : Fin n → ℕ) (hx : ∀ i, x i = l ∨ x i = u) (hlu : l < u)
    (hl : ∃ i, x i = l) (hu : ∃ i, x i = u) (p ν : ℝ) :
    sizeLikelihood x N p ν <
      pairHeight (Finset.univ.filter fun i ↦ x i = l).card
        (Finset.univ.filter fun i ↦ x i = u).card := by
  let z := (Finset.univ.filter fun i ↦ x i = l).card
  let o := (Finset.univ.filter fun i ↦ x i = u).card
  have hz : 0 < z := by
    obtain ⟨i, hi⟩ := hl
    exact Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  have ho : 0 < o := by
    obtain ⟨i, hi⟩ := hu
    exact Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  change sizeLikelihood x N p ν < pairHeight z o
  by_cases hv : ∀ i, x i ≤ N
  · have huN : u ≤ N := by obtain ⟨i, hi⟩ := hu; exact hi ▸ hv i
    let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hv i)⟩
    let L : Fin (N+1) := ⟨l, by omega⟩
    let U : Fin (N+1) := ⟨u, by omega⟩
    have H : ∀ i, y i = L ∨ y i = U := fun i ↦ (hx i).imp Fin.ext Fin.ext
    have hL : Discrete3CMB.multiplicity y L = z := by simp only [Discrete3CMB.multiplicity, Fin.ext_iff]; rfl
    have hU : Discrete3CMB.multiplicity y U = o := by simp only [Discrete3CMB.multiplicity, Fin.ext_iff]; rfl
    have hb := pair_bound hN hn y (fun e ↦ hlu.ne (congrArg Fin.val e)) H
      (hL ▸ hz) (hU ▸ ho) p ν
    rwa [hL, hU, ← show sizeLikelihood x N p ν = closedLikelihood y p ν by
      simp [sizeLikelihood, hv, y]] at hb
  · rw [sizeLikelihood, dite_eq_right hv]
    unfold pairHeight
    positivity

/-- The two exceptional two-point samples reach the saturated pair
height only on the excluded boundary, even when size varies across
the cap. On mixed binary data the same adjacent face ties the finite
Bernoulli fit whenever a size of at least two is within the cap. -/
theorem all_free_pair_failure {n B l u : ℕ} (hn : 0 < n) (huB : u ≤ B)
    (x : Fin n → ℕ) (hx : ∀ i, x i = l ∨ x i = u) (hlu : l < u)
    (hl : ∃ i, x i = l) (hu : ∃ i, x i = u) (he : l = 0 ∨ u = l+1) :
    let z := (Finset.univ.filter fun i ↦ x i = l).card
    let o := (Finset.univ.filter fun i ↦ x i = u).card
    let H := pairHeight z o
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    IsLUB (L '' D) H ∧
      (2 ≤ u → ∀ q ∈ D, L q < H) ∧
      (u = 1 →
        (∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1 = (o : ℝ)/(o+z)) ∧
        ∀ N ≤ B, 2 ≤ N →
          IsLUB ((fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2) ''
            (Icc 0 1 ×ˢ univ)) H ∧
            ∀ p ∈ Icc 0 1, ∀ ν, sizeLikelihood x N p ν < H) := by
  intro z o H D L
  have hz : 0 < z := by obtain ⟨i, hi⟩ := hl; exact Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  have ho : 0 < o := by obtain ⟨i, hi⟩ := hu; exact Finset.card_pos.mpr ⟨i, by simp [hi]⟩
  have hH : 0 < H := by unfold H pairHeight; positivity
  have hstrict (q : ℕ × ℝ × ℝ) (hq : 2 ≤ q.1 ∨ q.1 < u) : L q < H := by
    rcases hq with h | h
    · exact integer_pair_bound hn h x hx hlu hl hu q.2.1 q.2.2
    · have hv : ¬ ∀ i, x i ≤ q.1 := by
        obtain ⟨i, hi⟩ := hu
        exact fun hall ↦ (not_le_of_gt h) (hi ▸ hall i)
      simpa [L, sizeLikelihood, hv] using hH
  have excluded {N : ℕ} (hN : 2 ≤ N) (huN : u ≤ N)
      (hface : u = l+1 ∨ (l = 0 ∧ u = N)) :
      IsLUB ((fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2) '' (Icc 0 1 ×ˢ univ)) H ∧
      ∀ p ∈ Icc 0 1, ∀ ν, sizeLikelihood x N p ν < H := by
    have hv (i : Fin n) : x i ≤ N := (hx i).elim (fun h ↦ h ▸ hlu.le.trans huN)
      (fun h ↦ h ▸ huN)
    let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hv i)⟩
    let lo : Fin (N+1) := ⟨l, by omega⟩
    let up : Fin (N+1) := ⟨u, by omega⟩
    have hy : ∀ i, y i = lo ∨ y i = up := fun i ↦ (hx i).imp Fin.ext Fin.ext
    have hL : Discrete3CMB.multiplicity y lo = z := by
      simp only [Discrete3CMB.multiplicity, Fin.ext_iff]; rfl
    have hU : Discrete3CMB.multiplicity y up = o := by
      simp only [Discrete3CMB.multiplicity, Fin.ext_iff]; rfl
    have H' := Base8CMB.held_size_failure hN hn y
    have E : IsLUB ((fun q : ℝ × ℝ ↦ closedLikelihood y q.1 q.2) ''
        (Icc 0 1 ×ˢ univ)) H := by
      rcases hface with h | ⟨h₀, h₁⟩
      · simpa [H, hL, hU] using
          (H'.2.1 lo up h hy (hL ▸ hz) (hU ▸ ho)).1
      · have e₀ : lo = 0 := Fin.ext h₀
        have e₁ : up = Fin.last N := Fin.ext h₁
        rw [e₀, e₁] at hy
        rw [e₀] at hL
        rw [e₁] at hU
        simpa [H, hL, hU] using (H'.2.2 hy (hL ▸ hz) (hU ▸ ho)).1
    have eq (p ν : ℝ) : sizeLikelihood x N p ν = closedLikelihood y p ν := by
      simp [sizeLikelihood, hv, y]
    exact ⟨by simpa only [eq] using E, fun p _ ν ↦ hstrict (N, p, ν) (Or.inl hN)⟩
  by_cases hU : u = 1
  · have hL : l = 0 := by omega
    have hB : 1 ≤ B := by omega
    have hxb (i : Fin n) : x i ≤ 1 := by rcases hx i with h | h <;> omega
    have hb := mixed_binary_maximizers hn x hxb
      (by simpa [hL] using hl) (by simpa [hU] using hu) hB
    have hb' : ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1 ∧ q.2.1 = (o : ℝ)/(o+z) := by
      simpa [D, L, z, o, hL, hU] using hb
    let p := (o : ℝ)/(o+z)
    have hd : 0 < (o : ℝ) + z := add_pos (Nat.cast_pos.mpr ho) (Nat.cast_pos.mpr hz)
    have hp : p ∈ Icc (0 : ℝ) 1 :=
      ⟨div_nonneg (Nat.cast_nonneg _) hd.le,
        (div_le_one hd).mpr (le_add_of_nonneg_right (Nat.cast_nonneg _))⟩
    have hf : (1, p, 0) ∈ D := ⟨hB, hp⟩
    have hval : L (1,p,0) = H := by
      let y : Fin n → Fin 2 := fun i ↦ ⟨x i, Nat.lt_succ_of_le (hxb i)⟩
      have hL' : Discrete3CMB.multiplicity y 0 = z := by
        simp only [Discrete3CMB.multiplicity, Fin.ext_iff, z, hL]; rfl
      have hU' : Discrete3CMB.multiplicity y 1 = o := by
        simp only [Discrete3CMB.multiplicity, Fin.ext_iff, o, hU]; rfl
      have hp' : 1-p = (z : ℝ)/(z+o) := by
        dsimp [p]
        rw [add_comm (z : ℝ) o]
        field_simp
        ring
      change sizeLikelihood x 1 p 0 = _
      rw [show sizeLikelihood x 1 p 0 = closedLikelihood y p 0 by simp [sizeLikelihood, hxb, y],
        bernoulli_likelihood y hp, hL', hU', hp']
      simp [H, pairHeight, p, add_comm (o : ℝ) z, mul_comm]
    refine ⟨⟨?_, ?_⟩, fun h ↦ by omega, fun _ ↦ ⟨hb', ?_⟩⟩
    · rintro _ ⟨q, hq, rfl⟩
      exact (((hb' _ hf).mpr ⟨rfl, rfl⟩) hq).trans_eq hval
    · intro a ha
      rw [← hval]
      exact ha ⟨(1,p,0), hf, rfl⟩
    · intro N _ hN
      exact excluded hN (by omega) (Or.inl (by omega))
  · have h : 2 ≤ u := by omega
    have E := (excluded h le_rfl (he.elim (fun e ↦ Or.inr ⟨e, rfl⟩) Or.inl)).1
    have hlt (q : ℕ × ℝ × ℝ) : L q < H := hstrict q (by omega)
    refine ⟨⟨?_, ?_⟩, fun _ q _ ↦ hlt q, fun e ↦ (hU e).elim⟩
    · rintro _ ⟨q, _, rfl⟩
      exact (hlt q).le
    · intro a ha
      apply E.2
      rintro _ ⟨⟨p, ν⟩, hq, rfl⟩
      exact ha ⟨(u,p,ν), ⟨huB, hq.1⟩, rfl⟩

private abbrev Plane := EuclideanSpace ℝ (Fin 2)

private noncomputable def e₀ : Plane := WithLp.toLp 2 ![1,0]
private noncomputable def e₁ : Plane := WithLp.toLp 2 ![0,1]

private theorem basis_expansion (q : Plane) : q = q 0 • e₀ + q 1 • e₁ := by
  ext j
  fin_cases j <;> simp [e₀, e₁]

private theorem height_nonnegative {N : ℕ} (k : Fin (N+1)) : 0 ≤ height N k :=
  Real.log_nonneg (by exact_mod_cast Nat.choose_pos (Nat.le_of_lt_succ k.isLt))

private theorem statistic_hull_interior {N : ℕ} (hN : 2 ≤ N) :
    (interior (convexHull ℝ (range (FinishCMB.statistic N)))).Nonempty := by
  rw [(convex_convexHull ℝ _).interior_nonempty_iff_affineSpan_eq_top,
    affineSpan_convexHull]
  let S := affineSpan ℝ (range (FinishCMB.statistic N))
  have h0 : (0 : Plane) ∈ S := by
    apply mem_affineSpan ℝ
    refine ⟨0, ?_⟩
    ext j
    fin_cases j <;> simp [FinishCMB.statistic, height]
  have hmem (k : Fin (N+1)) : FinishCMB.statistic N k ∈ S.direction := by
    have h := AffineSubspace.vsub_mem_direction
      (mem_affineSpan ℝ (mem_range_self k) : FinishCMB.statistic N k ∈ S) h0
    simpa using h
  have hpos : 0 < (N : ℝ) := Nat.cast_pos.mpr (by omega)
  have hlog : 0 < Real.log (N : ℝ) := Real.log_pos (by exact_mod_cast hN)
  have hlast : FinishCMB.statistic N (Fin.last N) = (N : ℝ) • e₀ := by
    ext j
    fin_cases j <;> simp [FinishCMB.statistic, e₀, height]
  have hone : FinishCMB.statistic N ⟨1, by omega⟩ = e₀ + Real.log (N : ℝ) • e₁ := by
    ext j
    fin_cases j <;> simp [FinishCMB.statistic, e₀, e₁, height, Nat.choose_one_right]
  have hE₀ : e₀ ∈ S.direction := by
    have h := hmem (Fin.last N)
    rw [hlast] at h
    simpa [smul_smul, inv_mul_cancel₀ hpos.ne'] using S.direction.smul_mem (N : ℝ)⁻¹ h
  have hE₁ : e₁ ∈ S.direction := by
    have h := hmem ⟨1, by omega⟩
    rw [hone] at h
    have h' := S.direction.sub_mem h hE₀
    rw [add_sub_cancel_left] at h'
    simpa [smul_smul, inv_mul_cancel₀ hlog.ne'] using
      S.direction.smul_mem (Real.log (N : ℝ))⁻¹ h'
  apply top_unique
  intro q _
  have hq : q ∈ S.direction := by
    rw [basis_expansion q]
    exact S.direction.add_mem (S.direction.smul_mem _ hE₀) (S.direction.smul_mem _ hE₁)
  simpa using AffineSubspace.vadd_mem_of_mem_direction hq h0

private theorem supported_pair_of_noninterior {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N+1)) (hnc : ∀ k, ¬ ∀ i, x i = k)
    (hi : FinishCMB.empirical x ∉ interior (convexHull ℝ (range (FinishCMB.statistic N)))) :
    ∃ l u : Fin (N+1), l.val < u.val ∧ (∀ i, x i = l ∨ x i = u) ∧
      (∃ i, x i = l) ∧ (∃ i, x i = u) ∧
      (u.val = l.val+1 ∨ (l.val = 0 ∧ u.val = N)) := by
  obtain ⟨f, hf, hmax⟩ := geometric_hahn_banach_of_nonempty_interior_point
    (convex_convexHull ℝ _) hi (statistic_hull_interior hN)
  let M := f (FinishCMB.empirical x)
  let a := f e₀
  let b := f e₁
  let g (k : Fin (N+1)) := a*k.val + b*height N k
  have he (k : Fin (N+1)) : f (FinishCMB.statistic N k) = g k := by
    rw [basis_expansion (FinishCMB.statistic N k), map_add, map_smul, map_smul]
    change (k.val : ℝ)*a + height N k*b = g k
    dsimp [g]
    ring
  have hg (k : Fin (N+1)) : g k ≤ M :=
    he k ▸ hmax _ (subset_convexHull ℝ _ (mem_range_self k))
  have hNpos : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hmean : (n : ℝ) * M = ∑ i, g (x i) := by
    dsimp [M, FinishCMB.empirical]
    simp only [map_smul, map_sum, he, smul_eq_mul, ← mul_assoc,
      mul_inv_cancel₀ hNpos.ne', one_mul]
  have hobs (i : Fin n) : g (x i) = M := by
    refine le_antisymm (hg _) (le_of_not_gt fun h ↦ ?_)
    have H : ∑ j, g (x j) < ∑ _j : Fin n, M :=
      Finset.sum_lt_sum (fun j _ ↦ hg _) ⟨i, Finset.mem_univ _, h⟩
    simp [← hmean] at H
  have endpoints : g 0 = 0 ∧ g (Fin.last N) = a*N := by simp [g, height]
  rcases lt_trichotomy b 0 with hb | hb | hb
  · have H (i : Fin n) : x i = 0 ∨ x i = Fin.last N := by
      by_contra! h
      have h₀ : 0 < (x i).val := by
        exact Nat.pos_of_ne_zero (fun e ↦ h.1 (Fin.ext e))
      have h₁ : (x i).val < N := by
        have hle := (x i).isLt
        by_contra! he
        exact h.2 (Fin.ext (show (x i).val = N by omega))
      have hH : 0 < height N (x i) := by
        apply Real.log_pos
        exact_mod_cast show 1 < N.choose (x i).val from
          lt_of_le_of_ne (Nat.choose_pos h₁.le) (Ne.symm (by
            intro e
            rcases Nat.choose_eq_one_iff.mp e with e | e <;> omega))
      have E := mul_neg_of_neg_of_pos hb hH
      have hA : a * (x i).val ≤ M := by
        by_cases ha : 0 ≤ a
        · refine (mul_le_mul_of_nonneg_left ?_ ha).trans ((endpoints.2 ▸ hg (Fin.last N)))
          exact_mod_cast h₁.le
        · have h := endpoints.1 ▸ hg 0
          exact (mul_nonpos_of_nonpos_of_nonneg (le_of_not_ge ha) (Nat.cast_nonneg _)).trans h
      have K := hobs i
      dsimp [g] at K
      linarith
    have h₀ : ∃ i, x i = 0 := by
      by_contra! h
      exact hnc (Fin.last N) (fun i ↦ (H i).resolve_left (h i))
    have h₁ : ∃ i, x i = Fin.last N := by
      by_contra! h
      exact hnc 0 (fun i ↦ (H i).resolve_right (h i))
    exact ⟨0, Fin.last N, by simp; omega, H, h₀, h₁, Or.inr ⟨rfl, rfl⟩⟩
  · have ha : a ≠ 0 := by
      intro ha
      apply hf
      ext q
      rw [basis_expansion q, map_add, map_smul, map_smul]
      change q 0 * a + q 1 * b = 0
      simp [ha, hb]
    rcases lt_or_gt_of_ne ha with ha' | ha'
    · exfalso
      apply hnc 0
      intro i
      apply Fin.ext
      change (x i).val = 0
      by_contra! h
      have h' : 0 < ((x i).val : ℝ) := Nat.cast_pos.mpr (by omega)
      have K := hobs i
      have H := endpoints.1 ▸ hg 0
      simp [g, hb] at K
      linarith [mul_neg_of_neg_of_pos ha' h']
    · exfalso
      apply hnc (Fin.last N)
      intro i
      apply Fin.ext
      change (x i).val = N
      by_contra! h
      have hv := (x i).isLt
      have h' : ((x i).val : ℝ) < N := by exact_mod_cast (show (x i).val < N by omega)
      have K := hobs i
      have H := endpoints.2 ▸ hg (Fin.last N)
      simp [g, hb] at K
      linarith [mul_lt_mul_of_pos_left h' ha']
  · obtain ⟨i, -, hmin⟩ := Finset.exists_min_image Finset.univ (fun i ↦ (x i).val)
      (show (Finset.univ : Finset (Fin n)).Nonempty from ⟨⟨0, hn⟩, Finset.mem_univ _⟩)
    let l := x i
    have H (j : Fin n) : (x j).val ≤ l.val+1 := by
      by_contra! hj
      let c : Fin (N+1) := ⟨l.val+1, by have := (x j).isLt; omega⟩
      have h₁ : l.val < c.val := by simp [c]
      have h₂ : c.val < (x j).val := hj
      have hch := Trunc3CMB.lower_chord h₁ h₂
      have hch' := mul_lt_mul_of_pos_left hch hb
      have h₁' : (l.val : ℝ) < c.val := by exact_mod_cast h₁
      have h₂' : (c.val : ℝ) < (x j).val := by exact_mod_cast h₂
      have K := hg c
      have Kl : g l = M := hobs i
      have Ku := hobs j
      have hh : (((x j).val : ℝ)-l.val) * g c ≤
          (((x j).val : ℝ)-c.val) * g l + ((c.val : ℝ)-l.val) * g (x j) := by
        rw [Kl, Ku, ← add_mul, sub_add_sub_cancel]
        exact mul_le_mul_of_nonneg_left K (sub_nonneg.mpr (h₁'.trans h₂').le)
      dsimp [g] at hh
      linarith only [hh, hch']
    obtain ⟨j, hj⟩ : ∃ j, x j ≠ l := by by_contra! h; exact hnc l h
    let u := x j
    have hu : u.val = l.val+1 := by
      have hm := hmin j (Finset.mem_univ _)
      have h := H j
      have hne : (x j).val ≠ l.val := fun e ↦ hj (Fin.ext e)
      omega
    refine ⟨l, u, by omega, ?_, ⟨i, rfl⟩, ⟨j, rfl⟩, Or.inl hu⟩
    intro k
    have hl := hmin k (Finset.mem_univ _)
    have hh := H k
    rcases (show (x k).val = l.val ∨ (x k).val = u.val by omega) with h | h
    · exact Or.inl (Fin.ext h)
    · exact Or.inr (Fin.ext h)

/-- The capped maximizer set for constant samples contains precisely
the compatible endpoint atoms and the size-zero fiber on zero data. -/
theorem all_free_constant_maximizers {n B c : ℕ} (hn : 0 < n) (hc : c ≤ B)
    (x : Fin n → ℕ) (hx : ∀ i, x i = c) :
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    IsLUB (L '' D) 1 ∧
    ∀ q ∈ D, IsMaxOn L D q ↔
      (q.1 = 0 ∧ c = 0) ∨ (q.2.1 = 0 ∧ c = 0) ∨ (q.2.1 = 1 ∧ q.1 = c) := by
  intro D L
  have H (q : ℕ × ℝ × ℝ) (hq : q ∈ D) : L q ≤ 1 ∧
      (L q = 1 ↔ (q.1 = 0 ∧ c = 0) ∨ (q.2.1 = 0 ∧ c = 0) ∨
        (q.2.1 = 1 ∧ q.1 = c)) := by
    by_cases h : c ≤ q.1
    · have hxN (i : Fin n) : x i ≤ q.1 := hx i ▸ h
      have he : L q = closedLikelihood (fun _ : Fin n ↦
          (⟨c, Nat.lt_succ_of_le h⟩ : Fin (q.1+1))) q.2.1 q.2.2 := by
        simp [L, sizeLikelihood, hx, h]
      rw [he]
      have H' := constant_saturated_iff hn
        (⟨c, Nat.lt_succ_of_le h⟩ : Fin (q.1+1)) hq.2 q.2.2
      exact ⟨H'.1, H'.2.trans (by
        simp only
        constructor
        · rintro (hz | hother | ⟨h₁,hc'⟩)
          · exact Or.inl ⟨hz, by omega⟩
          · exact Or.inr (Or.inl hother)
          · exact Or.inr (Or.inr ⟨h₁,hc'.symm⟩)
        · rintro (⟨hz, _⟩ | hother | ⟨h₁,hc'⟩)
          · exact Or.inl hz
          · exact Or.inr (Or.inl hother)
          · exact Or.inr (Or.inr ⟨h₁,hc'.symm⟩))⟩
    · have hbad : ¬ ∀ i, x i ≤ q.1 := fun hh ↦ h (hx ⟨0,hn⟩ ▸ hh ⟨0,hn⟩)
      have he : L q = 0 := by simp [L, sizeLikelihood, hbad]
      have hc₀ : c ≠ 0 := by omega
      have hc' : q.1 ≠ c := by omega
      simp [he, hc₀, hc']
  have hq : (c, 1, 0) ∈ D := ⟨hc, ⟨zero_le_one, le_rfl⟩⟩
  have he := (H _ hq).2.mpr (Or.inr (Or.inr ⟨rfl,rfl⟩))
  refine ⟨⟨?_, ?_⟩, ?_⟩
  · rintro _ ⟨q, hq, rfl⟩
    exact (H q hq).1
  · intro b hb
    rw [← he]
    exact hb ⟨(c, 1, 0), hq, rfl⟩
  · intro q hq
    rw [← (H q hq).2]
    exact ⟨fun hm ↦ le_antisymm (H q hq).1 ((show 1 ≤ L q from he ▸ hm (a := (c, 1, 0)) (by exact ⟨hc, ⟨zero_le_one, le_rfl⟩⟩))),
      fun hr r hr' ↦ hr ▸ (H r hr').1⟩

private theorem ordinary_slices {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ N) (hnc : ∀ c, ¬ ∀ i, x i = c)
    (hlarge : ∃ i, 2 ≤ x i)
    (hbad : ¬ ∃ l u : ℕ, 2 ≤ u ∧ l < u ∧ (l = 0 ∨ u = l+1) ∧
      (∀ i, x i = l ∨ x i = u) ∧ (∃ i, x i = l) ∧ (∃ i, x i = u)) :
    ∃ q ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ),
      IsMaxOn (fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2) (Icc 0 1 ×ˢ univ) q := by
  let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hx i)⟩
  have he : (fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2) =
      fun q ↦ closedLikelihood y q.1 q.2 := by
    funext q
    simp [sizeLikelihood, hx, y]
  rw [he, JoinsCMB.held_size_criterion hn y]
  by_cases h : N ≤ 1
  · exact Or.inl h
  · have hy (k : Fin (N+1)) : ¬ ∀ i, y i = k :=
      fun h ↦ hnc k.val (fun i ↦ congrArg Fin.val (h i))
    refine Or.inr (Or.inr (Or.inr ?_))
    by_contra hi
    obtain ⟨l, u, hlu, hy', hl, hu, he⟩ :=
      supported_pair_of_noninterior (by omega) hn y hy hi
    have hU : 2 ≤ u.val := by
      obtain ⟨i, hi⟩ := hlarge
      rcases hy' i with h | h
      · have he : x i = l.val := congrArg Fin.val h
        omega
      · have he : x i = u.val := congrArg Fin.val h
        omega
    apply hbad
    exact ⟨l.val,u.val,hU,hlu,he.elim Or.inr (fun h ↦ Or.inl h.1),
      fun i ↦ (hy' i).imp (congrArg Fin.val) (congrArg Fin.val),
      hl.imp (fun i ↦ congrArg Fin.val), hu.imp (fun i ↦ congrArg Fin.val)⟩

private theorem size_likelihood_nonneg {n N : ℕ} (x : Fin n → ℕ) (p ν : ℝ) :
    0 ≤ sizeLikelihood x N p ν := by
  by_cases hv : ∀ i, x i ≤ N
  · rw [sizeLikelihood, dite_eq_left hv]
    unfold closedLikelihood
    split
    · split <;> norm_num
    · split
      · split <;> norm_num
      · unfold likelihood weight
        positivity
  · simp [sizeLikelihood, hv]

/-- On the capped size domain, a finite maximizer exists for every feasible
histogram except a mixed positive adjacent pair or zero paired with a
count at least two. The probability chart includes both endpoint atoms. -/
theorem all_free_criterion {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hB : ∀ i, x i ≤ B) :
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ≤ B ∧ q.2.1 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    (∃ q ∈ D, IsMaxOn L D q) ↔
      ¬ ∃ l u : ℕ, 2 ≤ u ∧ l < u ∧ (l = 0 ∨ u = l+1) ∧
        (∀ i, x i = l ∨ x i = u) ∧ (∃ i, x i = l) ∧ (∃ i, x i = u) := by
  classical
  intro D L
  constructor
  · rintro ⟨q, hq, hmax⟩ ⟨l, u, h₂, hlu, he, hx, hl, hu⟩
    have hU : u ≤ B := by obtain ⟨i, hi⟩ := hu; exact hi ▸ hB i
    have H := all_free_pair_failure hn hU x hx hlu hl hu he
    have top := H.1.2 (by rintro _ ⟨r, hr, rfl⟩; exact hmax hr)
    exact (not_le_of_gt (H.2.1 h₂ q hq)) top
  · intro hbad
    by_cases hconst : ∃ c, ∀ i, x i = c
    · obtain ⟨c, hc⟩ := hconst
      have hcB : c ≤ B := hc ⟨0,hn⟩ ▸ hB ⟨0,hn⟩
      have hf : (c, 1, 0) ∈ D := ⟨hcB, ⟨zero_le_one, le_rfl⟩⟩
      exact ⟨(c,1,0), hf, ((all_free_constant_maximizers hn hcB x hc).2 _ hf).mpr
        (Or.inr (Or.inr ⟨rfl, rfl⟩))⟩
    have hnc : ∀ c, ¬ ∀ i, x i = c := fun c h ↦ hconst ⟨c,h⟩
    by_cases hlarge : ∃ i, 2 ≤ x i
    · let S := (Finset.range (B+1)).filter (fun N ↦ ∀ i, x i ≤ N)
      have hbS : B ∈ S := by simp [S, hB]
      have hf {N : ℕ} (hN : N ∈ S) : N ≤ B ∧ ∀ i, x i ≤ N := by
        simpa [S, Nat.lt_succ_iff] using hN
      have slice (N : ℕ) (hN : N ∈ S) :=
        ordinary_slices hn x (hf hN).2 hnc hlarge hbad
      let P (N : ℕ) : ℝ × ℝ := if h : N ∈ S then (slice N h).choose else (0,0)
      have p_spec {N : ℕ} (hN : N ∈ S) :
          P N ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ) ∧
            IsMaxOn (fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2)
              (Icc 0 1 ×ˢ univ) (P N) := by
        simpa only [P, dite_eq_left hN] using (slice N hN).choose_spec
      obtain ⟨N, hN, hmax⟩ := Finset.exists_max_image S
        (fun N ↦ sizeLikelihood x N (P N).1 (P N).2) ⟨B, hbS⟩
      refine ⟨(N, P N), ⟨(hf hN).1, (p_spec hN).1.1⟩, ?_⟩
      rintro ⟨J,p,ν⟩ hJ
      have hp : p ∈ Icc (0 : ℝ) 1 := hJ.2
      have hJB : J ≤ B := hJ.1
      change sizeLikelihood x J p ν ≤ sizeLikelihood x N (P N).1 (P N).2
      by_cases hJ' : ∀ i, x i ≤ J
      · have hJS : J ∈ S := by simp [S, hJB, hJ']
        exact ((p_spec hJS).2 (show (p,ν) ∈ Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
          from ⟨hp, mem_univ _⟩)).trans (hmax J hJS)
      · rw [sizeLikelihood, dite_eq_right hJ']
        exact size_likelihood_nonneg x _ _
    · have hx (i : Fin n) : x i ≤ 1 := by by_contra! hi; exact hlarge ⟨i, by omega⟩
      have hz : ∃ i, x i = 0 := by
        by_contra! hh
        exact hnc 1 (fun i ↦ by have := hx i; have := hh i; omega)
      have ho : ∃ i, x i = 1 := by
        by_contra! hh
        exact hnc 0 (fun i ↦ by have := hx i; have := hh i; omega)
      have h₁ : 1 ≤ B := by obtain ⟨i, hi⟩ := ho; exact hi ▸ hB i
      let z := (Finset.univ.filter fun i ↦ x i = 0).card
      let o := (Finset.univ.filter fun i ↦ x i = 1).card
      let p := (o : ℝ)/(o+z)
      have ho' : 0 < o := by obtain ⟨i, hi⟩ := ho; exact Finset.card_pos.mpr ⟨i, by simp [hi]⟩
      have hd : 0 < (o : ℝ)+z := add_pos_of_pos_of_nonneg (Nat.cast_pos.mpr ho') (Nat.cast_nonneg _)
      have hp : p ∈ Icc (0 : ℝ) 1 :=
        ⟨div_nonneg (Nat.cast_nonneg _) hd.le,
          (div_le_one hd).mpr (le_add_of_nonneg_right (Nat.cast_nonneg _))⟩
      have hq : (1, p, 0) ∈ D := ⟨h₁, hp⟩
      exact ⟨(1,p,0), hq, ((mixed_binary_maximizers hn x hx hz ho h₁) _ hq).mpr ⟨rfl,rfl⟩⟩

/-- For a nonconstant, nonbinary histogram outside the two exposed pair
patterns, every compatible size has a unique conditional winner and every
global winner is recovered by comparing these conditional heights. -/
theorem all_free_maximizer_profiles {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hnc : ∀ c, ¬ ∀ i, x i = c) (hlarge : ∃ i, 2 ≤ x i)
    (hbad : ¬ ∃ l u : ℕ, 2 ≤ u ∧ l < u ∧ (l = 0 ∨ u = l+1) ∧
      (∀ i, x i = l ∨ x i = u) ∧ (∃ i, x i = l) ∧ (∃ i, x i = u)) :
    let S : Set ℕ := {N | N ≤ B ∧ ∀ i, x i ≤ N}
    let P := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let D : Set (ℕ × ℝ × ℝ) := {q | q.1 ∈ S ∧ q.2 ∈ P}
    let L := fun q : ℕ × ℝ × ℝ ↦ sizeLikelihood x q.1 q.2.1 q.2.2
    ∃ best : ℕ → ℝ × ℝ,
      (∀ N ∈ S, ∀ q ∈ P,
        IsMaxOn (fun q : ℝ × ℝ ↦ L (N,q)) P q ↔ q = best N) ∧
      (∀ N ∈ S, best N ∈ P) ∧
      ∀ q ∈ D, IsMaxOn L D q ↔
        q.2 = best q.1 ∧ IsMaxOn (fun N ↦ L (N, best N)) S q.1 := by
  classical
  intro S P D L
  have prof (N : ℕ) (hN : N ∈ S) :
      ∃! q, q ∈ P ∧ IsMaxOn (fun q : ℝ × ℝ ↦ L (N,q)) P q := by
    have hxN : ∀ i, x i ≤ N := hN.2
    have hN' : 2 ≤ N := by obtain ⟨i, hi⟩ := hlarge; exact hi.trans (hxN i)
    let y (i : Fin n) : Fin (N+1) := ⟨x i,Nat.lt_succ_of_le (hxN i)⟩
    have he : (fun q : ℝ × ℝ ↦ L (N,q)) = fun q ↦ closedLikelihood y q.1 q.2 := by
      funext q
      simp [L, sizeLikelihood, hxN, y]
    have hy (k : Fin (N+1)) : ¬ ∀ i, y i = k :=
      fun h ↦ hnc k.val (fun i ↦ congrArg Fin.val (h i))
    have H := (Base8CMB.held_size_maximizers hn y).2.2 hN'
    rw [he]
    refine (H.2.2 (hy _) (hy _)).mpr ?_
    have H' := (FinishCMB.held_size_closed_criterion hN' hn y).mp
      (by
        obtain ⟨q,hq,hbest⟩ := ordinary_slices hn x hxN hnc hlarge hbad
        refine ⟨q,hq,?_⟩
        rw [← he]
        exact hbest)
    exact H'.resolve_left (hy _) |>.resolve_left (hy _)
  let best (N : ℕ) : ℝ × ℝ := if h : N ∈ S then (prof N h).exists.choose else (0,0)
  have spec {N : ℕ} (hN : N ∈ S) :
      best N ∈ P ∧ IsMaxOn (fun q : ℝ × ℝ ↦ L (N,q)) P (best N) := by
    simpa only [best, dite_eq_left hN] using (prof N hN).exists.choose_spec
  have uniq {N : ℕ} (hN : N ∈ S) {q : ℝ × ℝ} (hq : q ∈ P) :
      IsMaxOn (fun q : ℝ × ℝ ↦ L (N,q)) P q ↔ q = best N := by
    refine ⟨fun h ↦ ?_, fun h ↦ h ▸ (spec hN).2⟩
    exact (prof N hN).unique ⟨hq,h⟩ (spec hN)
  refine ⟨best, fun _ hN _ hq ↦ uniq hN hq, fun _ hN ↦ (spec hN).1, ?_⟩
  rintro ⟨N,q⟩ ⟨hN,hq⟩
  constructor
  · intro h
    have hqbest := (uniq hN hq).mp (fun p hp ↦ h (show (N,p) ∈ D from ⟨hN,hp⟩))
    refine ⟨hqbest, fun J hJ ↦ ?_⟩
    exact (h (show (J,best J) ∈ D from ⟨hJ,(spec hJ).1⟩)).trans_eq
      (congrArg (fun p ↦ L (N,p)) hqbest)
  · rintro ⟨he,hNmax⟩ ⟨J,p⟩ ⟨hJ,hp⟩
    change q = best N at he
    change L (J,p) ≤ L (N,q)
    rw [he]
    exact ((spec hJ).2 hp).trans (hNmax hJ)

/-- The weight of every count at rational dispersion is specified as the
unique positive root of a power equation with rational right-hand side. -/
theorem rational_dispersion_weight {N d : ℕ} (hd : 0 < d) (a : ℤ)
    (k : Fin (N+1)) :
    let w := Real.exp ((((a : ℝ)/d) * height N k))
    0 < w ∧ w^d = (N.choose k.val : ℝ)^a ∧
      ∀ u > 0, u^d = (N.choose k.val : ℝ)^a → u = w := by
  let c : ℝ := N.choose k.val
  have hc : 0 < c := Nat.cast_pos.mpr (Nat.choose_pos (Nat.le_of_lt_succ k.isLt))
  let w := c^((a : ℝ)/d)
  have hw' : w = Real.exp ((((a : ℝ)/d) * height N k)) := by
    rw [show w = c^((a : ℝ)/d) from rfl, Real.rpow_def_of_pos hc, mul_comm]
    rfl
  rw [← hw']
  have hw : 0 < w := Real.rpow_pos_of_pos hc _
  have hd' : (d : ℝ) ≠ 0 := (Nat.cast_pos.mpr hd).ne'
  have he : w^d = c^a := by
    dsimp [w]
    rw [← Real.rpow_mul_natCast hc.le, div_mul_cancel₀ _ hd', Real.rpow_intCast]
  refine ⟨hw, he, fun u hu h ↦ ?_⟩
  exact (pow_left_strictMonoOn₀ hd.ne').injOn hu.le hw.le (h.trans he.symm)

private theorem rational_dispersion_weight_algebraic {N d : ℕ} (hd : 0 < d)
    (a : ℤ) (k : Fin (N+1)) :
    IsAlgebraic ℚ (Real.exp (((a : ℝ)/d)*height N k)) := by
  apply IsAlgebraic.of_pow hd
  rw [(rational_dispersion_weight hd a k).2.1]
  simpa only [Rat.cast_zpow,Rat.cast_natCast] using
    (isAlgebraic_ratCast ℚ (A := ℝ) ((N.choose k.val : ℚ)^a))

private noncomputable def probabilityWeight (N : ℕ) (p ν : ℝ) (k : Fin (N+1)) :=
  Real.exp (ν * height N k) * p^k.val * (1-p)^(N-k.val)

private theorem weight_sum_positive {N : ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (ν : ℝ) :
    0 < ∑ k, probabilityWeight N p ν k := by
  apply Finset.sum_pos'
  · intro k _
    unfold probabilityWeight
    exact mul_nonneg (mul_nonneg (Real.exp_pos _).le (pow_nonneg hp.1 _))
      (pow_nonneg (sub_nonneg.mpr hp.2) _)
  · by_cases h : p = 1
    · refine ⟨Fin.last N, Finset.mem_univ _, ?_⟩
      simp [probabilityWeight, height, h]
    · refine ⟨0, Finset.mem_univ _, ?_⟩
      have hq : 0 < 1-p := sub_pos.mpr (lt_of_le_of_ne hp.2 h)
      simp [probabilityWeight, height, pow_pos hq]

private theorem closed_likelihood_probability_weights {N n : ℕ} (x : Fin n → Fin (N+1))
    {p : ℝ} (hp : p ∈ Icc 0 1) (ν : ℝ) :
    closedLikelihood x p ν =
    ∏ i, probabilityWeight N p ν (x i) / ∑ j, probabilityWeight N p ν j := by
  classical
  by_cases h0 : p = 0
  · subst p
    have H (k : Fin (N+1)) : probabilityWeight N 0 ν k = if k = 0 then 1 else 0 := by
      by_cases h : k = 0
      · subst k
        simp [probabilityWeight, height]
      · have h' : k.val ≠ 0 := fun e ↦ h (Fin.ext e)
        simp [probabilityWeight, h, zero_pow h']
    simp_rw [H]
    have hd : (∑ j : Fin (N+1), if j = 0 then (1 : ℝ) else 0) = 1 := by simp
    rw [hd]
    simp only [div_one, closedLikelihood]
    by_cases hx : ∀ i, x i = 0
    · simp [hx]
    · obtain ⟨i, hi⟩ := not_forall.mp hx
      rw [Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi]), ite_eq_right hx]
      simp
  by_cases h1 : p = 1
  · subst p
    have H (k : Fin (N+1)) : probabilityWeight N 1 ν k =
        if k = Fin.last N then 1 else 0 := by
      by_cases h : k = Fin.last N
      · subst k
        simp [probabilityWeight, height]
      · have h' : N-k.val ≠ 0 := by
          have := k.isLt
          exact fun z ↦ h (Fin.ext (show k.val = N by omega))
        simp [probabilityWeight, h, zero_pow h']
    simp_rw [H]
    have hd : (∑ j : Fin (N+1), if j = Fin.last N then (1 : ℝ) else 0) = 1 := by simp
    rw [hd]
    simp only [div_one, closedLikelihood, one_ne_zero, ite_false]
    by_cases hx : ∀ i, x i = Fin.last N
    · simp [hx]
    · obtain ⟨i, hi⟩ := not_forall.mp hx
      rw [Finset.prod_eq_zero (Finset.mem_univ i) (by simp [hi]), ite_eq_right hx]
      simp
  have hp0 := lt_of_le_of_ne hp.1 (Ne.symm h0)
  have hp1 := lt_of_le_of_ne hp.2 h1
  have hq := sub_pos.mpr hp1
  have he (j : Fin (N+1)) :
      probabilityWeight N p ν j =
        (1-p)^N * weight N (Real.log (p/(1-p))) ν j := by
    have hj : j.val ≤ N := Nat.le_of_lt_succ j.isLt
    unfold probabilityWeight weight
    rw [Real.exp_add, mul_comm (Real.log _) (j.val : ℝ), Real.exp_nat_mul,
      Real.exp_log (div_pos hp0 hq), div_pow,
      show (1-p)^N = (1-p)^j.val * (1-p)^(N-j.val) by rw [← pow_add, Nat.add_sub_of_le hj]]
    field_simp
  rw [closedLikelihood, ite_eq_right h0, ite_eq_right h1]
  unfold likelihood
  simp_rw [he, ← Finset.mul_sum]
  simp only [mul_div_mul_left _ _ (pow_pos hq _).ne']

private theorem size_likelihood_continuous {n N : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ N) :
    ContinuousOn (fun q : ℝ × ℝ ↦ sizeLikelihood x N q.1 q.2)
      (Icc 0 1 ×ˢ (univ : Set ℝ)) := by
  let y (i : Fin n) : Fin (N+1) := ⟨x i,Nat.lt_succ_of_le (hx i)⟩
  have hW (k : Fin (N+1)) :
      Continuous (fun q : ℝ × ℝ ↦ probabilityWeight N q.1 q.2 k) := by
    unfold probabilityWeight
    fun_prop
  refine (continuousOn_finsetProd (Finset.univ : Finset (Fin n))
    (f := fun i q ↦ probabilityWeight N q.1 q.2 (y i) /
      ∑ j, probabilityWeight N q.1 q.2 j) (fun i _ ↦ ?_)).congr ?_
  · apply (hW (y i)).continuousOn.div
    · exact continuousOn_finsetSum _ fun j _ ↦ (hW j).continuousOn
    · exact fun q hq ↦ (weight_sum_positive hq.1 q.2).ne'
  · intro q hq
    have he : sizeLikelihood x N q.1 q.2 = closedLikelihood y q.1 q.2 := by
      simp [sizeLikelihood,hx,y]
    exact he.trans (closed_likelihood_probability_weights y hq.1 q.2)

/-- Each feasible fixed-size profile height is continuous on every
compact interval of held dispersions, including endpoint probabilities. -/
theorem held_dispersion_height_continuous {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ N) (a b : ℝ) :
    ContinuousOn (fun ν ↦ sSup ((fun p ↦ sizeLikelihood x N p ν) '' Icc 0 1))
      (Icc a b) := by
  let F (q : ℝ × ℝ) := sizeLikelihood x N q.1 q.2
  let I := Icc (0 : ℝ) 1
  let J := Icc a b
  let H (ν : ℝ) := sSup ((fun p ↦ F (p,ν)) '' I)
  have hc : ContinuousOn F (I ×ˢ J) :=
    (size_likelihood_continuous x hx).mono (fun q hq ↦ ⟨hq.1,mem_univ _⟩)
  have hu := Metric.uniformContinuousOn_iff.mp
    ((isCompact_Icc.prod isCompact_Icc).uniformContinuousOn_of_continuous hc)
  have hmax (ν : ℝ) :
      ∃ p ∈ I, F (p,ν) = H ν ∧ ∀ q ∈ I, F (q,ν) ≤ H ν := by
    obtain ⟨p,hp,hm⟩ := (Base8CMB.held_dispersion_failure hn x ν hx).1 N le_rfl hx
    have hne : ((fun q ↦ F (q,ν)) '' I).Nonempty := ⟨_,p,hp,rfl⟩
    have hsup : IsLUB ((fun q ↦ F (q,ν)) '' I) (F (p,ν)) := by
      refine ⟨?_,fun v hv ↦ hv ⟨p,hp,rfl⟩⟩
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq
    refine ⟨p,hp,(hsup.csSup_eq hne).symm,fun q hq ↦ ?_⟩
    change F (q,ν) ≤ sSup ((fun q ↦ F (q,ν)) '' I)
    rw [hsup.csSup_eq hne]
    exact hm hq
  change ContinuousOn H J
  rw [Metric.continuousOn_iff]
  intro ν hν ε hε
  obtain ⟨δ,hδ,hclose⟩ := hu ε hε
  refine ⟨δ,hδ,fun φ hφ hd ↦ ?_⟩
  obtain ⟨p,hp,hPH,hP⟩ := hmax ν
  obtain ⟨q,hq,hQH,hQ⟩ := hmax φ
  have close (r : ℝ) (hr : r ∈ I) : |F (r,φ)-F (r,ν)| < ε := by
    have hD : dist ((r,φ) : ℝ × ℝ) (r,ν) < δ := by
      simpa [Prod.dist_eq, max_eq_right dist_nonneg] using hd
    simpa [Real.dist_eq] using hclose (r,φ) ⟨hr,hφ⟩ (r,ν) ⟨hr,hν⟩ hD
  rw [Real.dist_eq, abs_lt]
  have h₀ := (abs_lt.mp (close p hp)).1
  have h₁ := (abs_lt.mp (close q hq)).2
  exact ⟨by linarith [hQ p hp], by linarith [hP q hq]⟩

private theorem dispersion_one {n N : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ N)
    {p : ℝ} (hp : p ∈ Icc 0 1) :
    sizeLikelihood x N p 1 = Discrete.Binomial.sampleLikelihood x N p := by
  let y (i : Fin n) : Fin (N+1) := ⟨x i,Nat.lt_succ_of_le (hx i)⟩
  have hW (k : Fin (N+1)) :
      probabilityWeight N p 1 k = (N.choose k.val : ℝ) * p^k.val * (1-p)^(N-k.val) := by
    unfold probabilityWeight height
    rw [one_mul, Real.exp_log (Nat.cast_pos.mpr (Nat.choose_pos (Nat.le_of_lt_succ k.isLt)))]
  have hd : (∑ j, probabilityWeight N p 1 j) = 1 := by
    simp_rw [hW]
    have h := add_pow p (1-p) N
    rw [show p+(1-p) = 1 by ring, one_pow] at h
    refine Eq.trans ?_ h.symm
    rw [Finset.sum_fin_eq_sum_range]
    apply Finset.sum_congr rfl
    intro k hk
    simp only [dite_eq_left (Finset.mem_range.mp hk)]
    ring
  rw [show sizeLikelihood x N p 1 = closedLikelihood y p 1 by simp [sizeLikelihood,hx,y],
    closed_likelihood_probability_weights y hp 1, hd]
  simp [Discrete.Binomial.sampleLikelihood, hW, y]

private noncomputable def heightProfile {n : ℕ} (x : Fin n → ℕ) (N : ℕ) (ν : ℝ) :=
  sSup ((fun p ↦ sizeLikelihood x N p ν) '' Icc 0 1)

private theorem height_realized {n N : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (hx : ∀ i, x i ≤ N) (ν : ℝ) :
    ∃ p ∈ Icc 0 1, sizeLikelihood x N p ν = heightProfile x N ν ∧
      ∀ q ∈ Icc 0 1, sizeLikelihood x N q ν ≤ heightProfile x N ν := by
  obtain ⟨p,hp,hm⟩ := (Base8CMB.held_dispersion_failure hn x ν hx).1 N le_rfl hx
  have hl : IsLUB ((fun q ↦ sizeLikelihood x N q ν) '' Icc 0 1) (sizeLikelihood x N p ν) := by
    refine ⟨?_,fun v hv ↦ hv ⟨p,hp,rfl⟩⟩
    rintro _ ⟨q,hq,rfl⟩
    exact hm hq
  have he : heightProfile x N ν = sizeLikelihood x N p ν := hl.csSup_eq ⟨_,p,hp,rfl⟩
  exact ⟨p,hp,he.symm,fun q hq ↦ he ▸ hm hq⟩

/-- The exact sample (7,8,13) has a held-dispersion global tie at an
interior value of dispersion, between size 13 and a larger capped size. -/
theorem held_dispersion_exact_tie {B : ℕ} (hB : 38 ≤ B) :
    let x : Fin 3 → ℕ := ![7,8,13]
    let D : Set (ℕ × ℝ) := {q | q.1 ≤ B ∧ (∀ i, x i ≤ q.1) ∧ q.2 ∈ Icc 0 1}
    let H := fun N ν ↦ sSup ((fun p ↦ sizeLikelihood x N p ν) '' Icc 0 1)
    let S := Finset.Icc 14 B
    let hS : S.Nonempty := ⟨38, Finset.mem_Icc.mpr ⟨by decide,hB⟩⟩
    ∃ ν ∈ Ioo (0 : ℝ) 1,
      IsLeast {v ∈ Icc 0 1 | S.sup' hS (fun N ↦ H N v) = H 13 v} ν ∧
      ∃ p, ∃ q ∈ D, 13 < q.1 ∧ (13,p) ∈ D ∧
      IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D (13,p) ∧
      IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D q := by
  intro x D H S hS
  have hx : ∀ i, x i ≤ 13 := by intro i; fin_cases i <;> decide
  have hx13 : x 2 = 13 := rfl
  have h3 : 0 < (3 : ℕ) := by decide
  have hfeas {N : ℕ} : (∀ i, x i ≤ N) ↔ 13 ≤ N := by
    exact ⟨fun h ↦ hx13 ▸ h 2, fun h i ↦ (hx i).trans h⟩
  let G (ν : ℝ) := S.sup' hS (fun N ↦ H N ν) - H 13 ν
  have hc : ContinuousOn G (Icc 0 1) := by
    apply (ContinuousOn.finset_sup'_apply hS (fun N hN ↦
      held_dispersion_height_continuous h3 x (hfeas.mpr (by
        have := (Finset.mem_Icc.mp hN).1; omega)) 0 1)).sub
    exact held_dispersion_height_continuous h3 x hx 0 1
  have hzero : G 0 < 0 := by
    obtain ⟨N,hN,hmax⟩ := Finset.exists_mem_eq_sup' hS (fun N ↦ H N 0)
    have hN' := Finset.mem_Icc.mp hN
    have hNx : ∀ i, x i ≤ N := hfeas.mpr (by omega)
    obtain ⟨p,hp,hp' ,hP⟩ := height_realized h3 x hNx 0
    obtain ⟨⟨K,q⟩,hK,hM⟩ := held_dispersion_capped h3 x 0
      (fun i ↦ (hx i).trans (by omega : 13 ≤ B))
    have hsup : Finset.univ.sup x = 13 := by
      exact le_antisymm (Finset.sup_le fun i _ ↦ hx i) (hx13 ▸ Finset.le_sup (Finset.mem_univ (2 : Fin 3)))
    have hK' : K = 13 := by
      have he := held_zero_dispersion_size_unique (B := B) h3 x
        (show ∃ i, 0 < x i from ⟨2, by decide⟩) (K,q) hK hM
      exact he.trans hsup
    subst K
    have hNmem : (N,p) ∈ D := ⟨hN'.2,hNx,hp⟩
    have hl : H N 0 < H 13 0 := by
      by_contra! he
      obtain ⟨z,hz,hz',hZ⟩ := height_realized h3 x hx 0
      have hW : IsMaxOn (fun t : ℕ × ℝ ↦ sizeLikelihood x t.1 t.2 0) D (N,p) := by
        intro t ht
        exact (hM (a := t) ht).trans ((hZ q hK.2.2).trans (he.trans_eq hp'.symm))
      have eq := held_zero_dispersion_size_unique (B := B) h3 x
        (show ∃ i, 0 < x i from ⟨2, by decide⟩) (N,p) hNmem hW
      change N = Finset.univ.sup x at eq
      omega
    exact sub_neg.mpr (hmax.trans_lt hl)
  have hpos : 0 < G 1 := by
    have hN : ∀ i, x i ≤ 38 := fun i ↦ (hx i).trans (by decide)
    have hμ : BinomialSize.mean x = (28 : ℝ)/3 := by
      norm_num [BinomialSize.mean, x, Fin.sum_univ_succ]
    have hμ' : 0 < BinomialSize.mean x := by rw [hμ]; norm_num
    have hfit (N : ℕ) (hN : ∀ i, x i ≤ N) (hN' : 13 ≤ N) :
        H N 1 = Discrete.Binomial.sampleLikelihood x N (BinomialSize.mean x / N) := by
      let p := BinomialSize.mean x / N
      have hpp : p ∈ Ioo (0 : ℝ) 1 := by
        have hNr : (13 : ℝ) ≤ N := by exact_mod_cast hN'
        exact ⟨div_pos hμ' (by linarith), (div_lt_one (by linarith)).mpr (by rw [hμ]; linarith)⟩
      have hp : p ∈ Icc (0 : ℝ) 1 := ⟨hpp.1.le,hpp.2.le⟩
      obtain ⟨q,hq,hq',hQ⟩ := height_realized h3 x hN 1
      have he : Discrete.Binomial.sampleLikelihood x N q ≤ Discrete.Binomial.sampleLikelihood x N p := by
        have hpositive := (Discrete.Binomial.sample_likelihood_pos_iff x hp).mpr
          ⟨hN,fun h ↦ (hpp.1.ne' h).elim,fun h ↦ (hpp.2.ne h).elim⟩
        by_cases hq₀ : q = 0
        · subst q
          have hz : ¬ Discrete.Binomial.compatible x N 0 := by
            intro h; have := h.2.1 rfl 2; omega
          exact (le_of_not_gt ((Discrete.Binomial.sample_likelihood_pos_iff x hq).not.mpr hz)).trans hpositive.le
        by_cases hq₁ : q = 1
        · subst q
          have hz : ¬ Discrete.Binomial.compatible x N 1 := by
            intro h
            have h' := h.2.2 rfl (0 : Fin 3)
            have h7 : x 0 = 7 := rfl
            omega
          exact (le_of_not_gt ((Discrete.Binomial.sample_likelihood_pos_iff x hq).not.mpr hz)).trans hpositive.le
        have hqi : q ∈ Ioo (0 : ℝ) 1 := ⟨lt_of_le_of_ne hq.1 (Ne.symm hq₀),lt_of_le_of_ne hq.2 hq₁⟩
        have hpos := (Discrete.Binomial.sample_likelihood_pos_iff x hq).mpr
          ⟨hN,fun h ↦ (hqi.1.ne' h).elim,fun h ↦ (hqi.2.ne h).elim⟩
        have hn' : 0 < N := by omega
        have hh := BetaBinomial.binomial_isMaxOn hn' h3 hN hpp.1 hpp.2 hqi
        apply (Real.log_le_log_iff hpos hpositive).mp
        rw [← Discrete.Binomial.sizeLogLik_eq_log x hqi hN,
          ← Discrete.Binomial.sizeLogLik_eq_log x hpp hN]
        change BetaBinomial.binomialLogLik N x q ≤ BetaBinomial.binomialLogLik N x p at hh
        simpa [BetaBinomial.binomialLogLik, Discrete.Binomial.sizeLogLik, Nat.cast_sub (hN _)] using hh
      refine le_antisymm (hq'.symm.trans_le ?_) ?_
      · rwa [dispersion_one x hN hq]
      · rw [← dispersion_one x hN hp]
        exact hQ p hp
    have hlt : H 13 1 < H 38 1 := by
      rw [hfit 13 hx le_rfl,hfit 38 hN (by decide),hμ]
      simp only [Discrete.Binomial.sampleLikelihood, x, Fin.prod_univ_succ,
        Fin.prod_univ_zero, Matrix.cons_val_zero, Matrix.cons_val_succ, mul_one]
      norm_num [Nat.choose_eq_descFactorial_div_factorial, Nat.descFactorial, Nat.factorial]
    have h38 : 38 ∈ S := Finset.mem_Icc.mpr ⟨by decide,hB⟩
    exact sub_pos.mpr (hlt.trans_le (Finset.le_sup' (fun N ↦ H N 1) h38))
  obtain ⟨w,hw,hwG⟩ := intermediate_value_Icc (zero_le_one : (0 : ℝ) ≤ 1) hc
    (show 0 ∈ Icc (G 0) (G 1) from ⟨hzero.le,hpos.le⟩)
  have hcompact : IsCompact (G ⁻¹' ({0} : Set ℝ) ∩ Icc 0 1) := by
    obtain ⟨U,hU,hEq⟩ := continuousOn_iff_isClosed.mp hc {0} isClosed_singleton
    rw [hEq]
    exact isCompact_Icc.inter_left hU
  obtain ⟨ν,hνmin⟩ := hcompact.exists_isLeast ⟨w,hwG,hw⟩
  have hν := hνmin.1.2
  have hG : G ν = 0 := hνmin.1.1
  have hLeast : IsLeast {v ∈ Icc 0 1 | S.sup' hS (fun N ↦ H N v) = H 13 v} ν := by
    refine ⟨⟨hν, sub_eq_zero.mp hG⟩,?_⟩
    intro v hv
    exact hνmin.2 ⟨sub_eq_zero.mpr hv.2,hv.1⟩
  have hν' : ν ∈ Ioo (0 : ℝ) 1 := by
    refine ⟨lt_of_le_of_ne hν.1 ?_,lt_of_le_of_ne hν.2 ?_⟩
    · exact fun h ↦ hzero.ne (h ▸ hG)
    · exact fun h ↦ hpos.ne' (h ▸ hG)
  obtain ⟨N,hN,hEq⟩ := Finset.exists_mem_eq_sup' hS (fun N ↦ H N ν)
  have hNs := Finset.mem_Icc.mp hN
  have hNx := hfeas.mpr (show 13 ≤ N by omega)
  have he : H N ν = H 13 ν := by dsimp [G] at hG; linarith only [hG,hEq]
  obtain ⟨p,hp,hp',hP⟩ := height_realized h3 x hx ν
  obtain ⟨q,hq,hq',hQ⟩ := height_realized h3 x hNx ν
  have hmem : (13,p) ∈ D := ⟨by omega,hx,hp⟩
  have hmem' : (N,q) ∈ D := ⟨hNs.2,hNx,hq⟩
  have hw (j : ℕ) (hj : 13 ≤ j) (hjB : j ≤ B) : H j ν ≤ H 13 ν := by
    by_cases h : j = 13
    · simp [h]
    · have hJ : j ∈ S := Finset.mem_Icc.mpr ⟨by omega,hjB⟩
      exact (Finset.le_sup' _ hJ).trans_eq (hEq.trans he)
  have hm : IsMaxOn (fun q ↦ sizeLikelihood x q.1 q.2 ν) D (13,p) := by
    intro t ht
    obtain ⟨s,hs,hs',hS'⟩ := height_realized h3 x ht.2.1 ν
    exact (hS' t.2 ht.2.2).trans ((hw t.1 (hfeas.mp ht.2.1) ht.1).trans_eq hp'.symm)
  refine ⟨ν,hν',hLeast,p,(N,q),hmem',by omega,hmem,hm,?_⟩
  intro t ht
  exact (hm ht).trans_eq (hp'.trans (he.symm.trans hq'.symm))

/-- The held-dispersion odds equation has a unique positive root for
every interior empirical mean, including coefficients at a rational hold. -/
private theorem held_dispersion_odds_chart {N : ℕ} {μ : ℝ}
    (hμ : μ ∈ Ioo (0 : ℝ) N) (ν : ℝ) :
    let T : Fin (N+1) → ℝ := fun k ↦ k.val
    let w : Fin (N+1) → ℝ := fun k ↦ Real.exp (ν*height N k)
    let f := Truncation.logLik Finset.univ T w μ
    let Eqn := fun r : ℝ ↦ ∑ k : Fin (N+1), ((k.val : ℝ)-μ) * w k * r^k.val
    (∃! r : ℝ, 0 < r ∧ Eqn r = 0) ∧
      ∀ r > 0, Eqn r = 0 ↔ IsMaxOn f univ (Real.log r) := by
  intro T w f Eqn
  have hpos (k : Fin (N+1)) : 0 < w k := Real.exp_pos _
  have hx (k : Fin (N+1)) : T k ∈ Icc (T 0) (T (Fin.last N)) := by
    simpa [T] using (show (k.val : ℝ) ∈ Icc (0 : ℝ) (N : ℝ) from
      ⟨Nat.cast_nonneg _, Nat.cast_le.mpr (Nat.le_of_lt_succ k.isLt)⟩)
  have hlt : T 0 < T (Fin.last N) := by simpa [T] using hμ.1.trans hμ.2
  obtain ⟨t, hMax, hUniq⟩ :=
    (Truncation.scalar_existsUnique_iff (fun k _ ↦ hpos k)
      (Finset.mem_univ (0 : Fin (N+1))) (Finset.mem_univ (Fin.last N))
      hlt (fun k _ ↦ hx k) μ).mpr (by simpa [T] using hμ)
  let Z (u : ℝ) := ∑ k, w k * Real.exp (u*T k)
  let W (u : ℝ) := ∑ k, w k * (Real.exp (u*T k)*T k)
  have hZ (u : ℝ) : 0 < Z u := by
    apply Finset.sum_pos (fun k _ ↦ mul_pos (hpos k) (Real.exp_pos _))
    exact ⟨0,Finset.mem_univ _⟩
  have hD (u : ℝ) : HasDerivAt f (μ-W u/Z u) u := by
    have hS : HasDerivAt Z (W u) u :=
      HasDerivAt.fun_sum fun k _ ↦ by
        simpa only [id_eq,one_mul] using (((hasDerivAt_id u).mul_const (T k)).exp).const_mul (w k)
    change HasDerivAt (fun v ↦ μ*v - Real.log (∑ k, w k*Real.exp (T k*v))) _ u
    simpa only [Z, mul_comm (T _), id_eq, one_mul,mul_one] using
      ((hasDerivAt_id u).const_mul μ).fun_sub (hS.log (hZ u).ne')
  have equation (u : ℝ) :
      (∑ k : Fin (N+1), ((k.val : ℝ)-μ) * w k * (Real.exp u)^k.val) = 0 ↔
        μ-W u/Z u = 0 := by
    have he (k : Fin (N+1)) : (Real.exp u)^k.val = Real.exp (u*T k) := by
      rw [← Real.exp_nat_mul]
      simp [T,mul_comm]
    simp_rw [he]
    rw [sub_eq_zero, eq_comm (a := μ),div_eq_iff (hZ u).ne']
    have hs : (∑ k : Fin (N+1), ((k.val : ℝ)-μ) * w k * Real.exp (u*T k)) =
        W u - μ*Z u := by
      unfold W Z
      rw [Finset.mul_sum, ← Finset.sum_sub_distrib]
      apply Finset.sum_congr rfl
      intro i _
      dsimp [T]
      ring
    rw [hs,sub_eq_zero]
  have at_max (u : ℝ) (hu : IsMaxOn f univ u) : μ-W u/Z u = 0 :=
    hu.isLocalMaxOn.isLocalMax (by simp) |>.hasDerivAt_eq_zero (hD u)
  have at_root (u : ℝ) (hU : μ-W u/Z u = 0) : IsMaxOn f univ u := by
    have hDu : HasDerivAt (-f) 0 u := by
      simpa only [hU,neg_zero] using (hD u).neg
    have hc : ConvexOn ℝ univ (-f) := by
      have hC := (ExpFamily.concave_logLik T hpos μ).neg
      have he : f = ExpFamily.logLik T w μ := by
        funext v
        simp [f,Truncation.logLik,Truncation.partition,ExpFamily.logLik,ExpFamily.logPartition]
      rwa [he]
    intro v hv
    change f v ≤ f u
    rcases lt_trichotomy u v with h | rfl | h
    · have H := hc.le_slope_of_hasDerivAt (mem_univ u) hv h hDu
      rw [slope_def_field] at H
      have h' := (le_div_iff₀ (sub_pos.mpr h)).mp H
      change 0 * (v-u) ≤ -f v - -f u at h'
      linarith
    · rfl
    · have H := hc.slope_le_of_hasDerivAt hv (mem_univ u) h hDu
      rw [slope_def_field] at H
      have h' := (div_le_iff₀ (sub_pos.mpr h)).mp H
      change -f u - -f v ≤ 0 * (u-v) at h'
      linarith
  have chart (r : ℝ) (hr : 0 < r) : Eqn r = 0 ↔ IsMaxOn f univ (Real.log r) := by
    have H := equation (Real.log r)
    rw [Real.exp_log hr] at H
    exact H.trans ⟨at_root _,at_max _⟩
  refine ⟨⟨Real.exp t,⟨Real.exp_pos _,(equation t).mpr (at_max t hMax)⟩,?_⟩,chart⟩
  rintro r ⟨hr,he⟩
  have h := hUniq _ ((chart r hr).mp he)
  rw [← h,Real.exp_log hr]

/-- The held-dispersion odds equation has a unique positive root for
every interior empirical mean, including coefficients at a rational hold. -/
theorem held_dispersion_positive_odds_root {N : ℕ} {μ : ℝ}
    (hμ : μ ∈ Ioo (0 : ℝ) N) (ν : ℝ) :
    ∃! r : ℝ, 0 < r ∧
      (∑ k : Fin (N+1), ((k.val : ℝ)-μ) * Real.exp (ν*height N k) * r^k.val) = 0 :=
  (held_dispersion_odds_chart hμ ν).1

/-- At any rational dispersion, the unique positive odds solving the
mean equation is real algebraic when the empirical mean is rational. -/
theorem rational_dispersion_algebraic_odds {N d : ℕ} (hd : 0 < d) (a : ℤ)
    (μ : ℚ) (hμ : (μ : ℝ) ∈ Ioo (0 : ℝ) (N : ℝ)) :
    let w := fun k : Fin (N+1) ↦ Real.exp (((a : ℝ)/d)*height N k)
    ∃! r : ℝ, 0 < r ∧ IsAlgebraic ℚ r ∧
      (∑ k : Fin (N+1), ((k.val : ℝ)-(μ : ℝ))*w k*r^k.val) = 0 := by
  intro w
  obtain ⟨r,⟨hr,he⟩,huniq⟩ := held_dispersion_positive_odds_root hμ ((a : ℝ)/d)
  let K := algebraicClosure ℚ ℝ
  have hw (k : Fin (N+1)) : w k ∈ K :=
    mem_algebraicClosure_iff.mpr (rational_dispersion_weight_algebraic hd a k)
  let coeff (k : Fin (N+1)) : K :=
    ⟨((k.val : ℝ)-(μ : ℝ))*w k, K.mul_mem
      (K.sub_mem (K.natCast_mem k.val) (K.algebraMap_mem μ)) (hw k)⟩
  let F : Polynomial K := ∑ k : Fin (N+1), Polynomial.monomial k.val (coeff k)
  have hF : F ≠ 0 := by
    have h0 : F.coeff 0 = coeff 0 := by
      dsimp [F]
      rw [Polynomial.finsetSum_coeff, Finset.sum_eq_single (0 : Fin (N+1))]
      · simp
      · intro k _ hk
        have hne : k.val ≠ 0 := fun h ↦ hk (Fin.ext (by simpa using h))
        simp [Polynomial.coeff_monomial,hne.symm]
      · simp
    intro h
    have hz : coeff 0 = 0 := h0.symm.trans (by simp [h])
    have h' := congrArg (fun z : K ↦ (z : ℝ)) hz
    have hp : 0 < (μ : ℝ) := hμ.1
    change (((0 : Fin (N+1)).val : ℝ) - (μ : ℝ))*w 0 = 0 at h'
    have hn := mul_neg_of_neg_of_pos (show (((0 : Fin (N+1)).val : ℝ) - (μ : ℝ)) < 0 by simp [hp])
      (show 0 < w 0 from Real.exp_pos _)
    exact hn.ne h'
  have hRoot : Polynomial.aeval r F = 0 := by
    unfold F
    simp only [map_sum,Polynomial.aeval_monomial]
    exact he
  have hA : IsAlgebraic ℚ r := (show IsAlgebraic K r from ⟨F,hF,hRoot⟩).restrictScalars ℚ
  exact ⟨r,⟨hr,hA,he⟩,fun s hs ↦ huniq s ⟨hs.1,hs.2.2⟩⟩

/-- An interior empirical mean at a rational dispersion has an algebraic
maximizing odds and an algebraic profile height. The ordinary sample
likelihood agrees with the polynomial odds formula at this root. -/
theorem rational_dispersion_algebraic_profile {N n d : ℕ}
    (hn : 0 < n) (hd : 0 < d) (a : ℤ) (x : Fin n → ℕ) (hx : ∀ i, x i ≤ N)
    (h₀ : ∃ i, 0 < x i) (h₁ : ∃ i, x i < N) :
    let ν := (a : ℝ)/d
    let w := fun k : Fin (N+1) ↦ Real.exp (ν*height N k)
    let y : Fin n → Fin (N+1) := fun i ↦ ⟨x i,Nat.lt_succ_of_le (hx i)⟩
    let μ := (∑ i, (x i : ℝ)) / n
    ∃ r > 0, IsAlgebraic ℚ r ∧
      (∑ k, ((k.val : ℝ)-μ)*w k*r^k.val) = 0 ∧
      let H := ∏ i, (w (y i) * r^(x i)) / (∑ k, w k*r^k.val)
      IsAlgebraic ℚ H ∧
      sizeLikelihood x N (r/(1+r)) ν = H ∧
      IsMaxOn (fun p ↦ sizeLikelihood x N p ν) (Icc 0 1) (r/(1+r)) := by
  intro ν w y μ
  let μq : ℚ := (∑ i, (x i : ℚ)) / n
  have hq : (μq : ℝ) = μ := by simp [μq,μ]
  have hn' : 0 < (n : ℝ) := Nat.cast_pos.mpr hn
  have hμ : μ ∈ Ioo (0 : ℝ) (N : ℝ) := by
    obtain ⟨i,hi⟩ := h₀
    obtain ⟨j,hj⟩ := h₁
    refine ⟨div_pos ?_ hn', (div_lt_iff₀ hn').mpr ?_⟩
    · exact (Nat.cast_pos.mpr hi).trans_le
        (Finset.single_le_sum (fun k _ ↦ Nat.cast_nonneg (x k)) (Finset.mem_univ i))
    · calc
        ∑ k, (x k : ℝ) < ∑ _k : Fin n, (N : ℝ) :=
          Finset.sum_lt_sum (fun k _ ↦ Nat.cast_le.mpr (hx k))
            ⟨j,Finset.mem_univ _,Nat.cast_lt.mpr hj⟩
        _ = (N : ℝ)*n := by simp [mul_comm]
  obtain ⟨r,⟨hr,ha,he⟩,_⟩ := rational_dispersion_algebraic_odds hd a μq (hq ▸ hμ)
  rw [hq] at he
  change (∑ k : Fin (N+1), ((k.val : ℝ)-μ)*w k*r^k.val) = 0 at he
  let p := r/(1+r)
  have hp : p ∈ Ioo 0 1 := ⟨by dsimp [p]; positivity,
    (div_lt_one (by positivity)).mpr (by linarith)⟩
  have hp' : p/(1-p) = r := by
    have h : 1-p = 1/(1+r) := by dsimp [p]; field_simp; ring
    rw [h]
    dsimp [p]
    field_simp
  let f := Truncation.logLik Finset.univ (fun k : Fin (N+1) ↦ (k.val : ℝ)) w μ
  have hopt : IsMaxOn f univ (Real.log r) :=
    ((held_dispersion_odds_chart hμ ν).2 r hr).mp he
  have hweight (s : ℝ) (hs : 0 < s) (k : Fin (N+1)) :
      weight N (Real.log s) ν k = w k * s^k.val := by
    unfold weight
    rw [Real.exp_add, mul_comm (Real.log s) (k.val : ℝ), Real.exp_nat_mul, Real.exp_log hs]
    exact mul_comm _ _
  let K := algebraicClosure ℚ ℝ
  have hW (k : Fin (N+1)) : w k ∈ K :=
    mem_algebraicClosure_iff.mpr (rational_dispersion_weight_algebraic hd a k)
  have hrK : r ∈ K := mem_algebraicClosure_iff.mpr ha
  have Halg : IsAlgebraic ℚ (∏ i, (w (y i)*r^x i) / ∑ k, w k*r^k.val) := by
    apply mem_algebraicClosure_iff.mp
    apply K.prod_mem
    intro i _
    exact K.div_mem (K.mul_mem (hW (y i)) (K.pow_mem hrK (x i)))
      (K.sum_mem fun k _ ↦ K.mul_mem (hW k) (K.pow_mem hrK k.val))
  have hfit : sizeLikelihood x N p ν =
      ∏ i, (w (y i)*r^x i) / ∑ k, w k*r^k.val := by
    rw [show sizeLikelihood x N p ν = closedLikelihood y p ν from by simp [sizeLikelihood,hx,y],
      closedLikelihood,ite_eq_right hp.1.ne',ite_eq_right hp.2.ne,hp']
    unfold likelihood
    simp_rw [hweight r hr]
    rfl
  have hZ (t : ℝ) : 0 < ∑ k, weight N t ν k :=
    Finset.sum_pos (fun _ _ ↦ Real.exp_pos _) ⟨0,Finset.mem_univ _⟩
  have hpos (t : ℝ) : 0 < likelihood y t ν :=
    Finset.prod_pos fun i _ ↦ div_pos (Real.exp_pos _) (hZ t)
  let C := ∑ i, ν * height N (y i)
  have hLog (t : ℝ) : Real.log (likelihood y t ν) = C + (n : ℝ)*f t := by
    unfold likelihood
    rw [Real.log_prod (f := fun i ↦ weight N t ν (y i) / ∑ j, weight N t ν j)
      (fun i _ ↦ (div_pos (Real.exp_pos _) (hZ t)).ne')]
    have hlogs (i : Fin n) :
        Real.log (weight N t ν (y i) / ∑ j, weight N t ν j) =
          Real.log (weight N t ν (y i)) - Real.log (∑ j, weight N t ν j) :=
      Real.log_div (Real.exp_pos _).ne' (hZ t).ne'
    simp_rw [hlogs]
    simp only [weight, Real.log_exp, Finset.sum_sub_distrib, Finset.sum_add_distrib,
      Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul,
      ← Finset.mul_sum]
    have hDen : (∑ k : Fin (N+1), Real.exp (t*k.val + ν*height N k)) =
        ∑ k : Fin (N+1), w k*Real.exp ((k.val : ℝ)*t) := by
      apply Finset.sum_congr rfl
      intro k _
      rw [Real.exp_add]
      simp [w,mul_comm]
    rw [hDen]
    change _ = C + n*(μ*t - Real.log _)
    dsimp [C, μ, y]
    field_simp [hn'.ne']
    simp only [starRingEnd_apply, star_trivial, ← Finset.mul_sum, mul_comm t]
    ring
  refine ⟨r,hr,ha,he,Halg,hfit,?_⟩
  intro q hq
  change sizeLikelihood x N q ν ≤ sizeLikelihood x N p ν
  have hpL : sizeLikelihood x N p ν = likelihood y (Real.log r) ν := by
    simp only [sizeLikelihood,dite_eq_left hx,closedLikelihood,
      ite_eq_right hp.1.ne',ite_eq_right hp.2.ne,hp',y]
  rw [hpL, show sizeLikelihood x N q ν = closedLikelihood y q ν from by simp [sizeLikelihood,hx,y]]
  by_cases hq0 : q = 0
  · obtain ⟨i,hi⟩ := h₀
    have H : ¬ ∀ j, y j = 0 := fun h ↦ hi.ne' (by simpa [y] using congrArg Fin.val (h i))
    simpa [closedLikelihood,hq0,H] using (hpos (Real.log r)).le
  by_cases hq1 : q = 1
  · obtain ⟨i,hi⟩ := h₁
    have H : ¬ ∀ j, y j = Fin.last N := fun h ↦ hi.ne (by simpa [y] using congrArg Fin.val (h i))
    simpa [closedLikelihood,hq0,hq1,H] using (hpos (Real.log r)).le
  rw [closedLikelihood,ite_eq_right hq0,ite_eq_right hq1]
  apply (Real.log_le_log_iff (hpos _) (hpos _)).mp
  rw [hLog,hLog]
  exact add_le_add le_rfl (mul_le_mul_of_nonneg_left (hopt (mem_univ _)) hn'.le)

private theorem size_likelihood_le_one {N n : ℕ} (x : Fin n → ℕ) (hx : ∀ i, x i ≤ N)
    {p : ℝ} (hp : p ∈ Icc 0 1) (ν : ℝ) :
    sizeLikelihood x N p ν ≤ 1 := by
  let y : Fin n → Fin (N+1) := fun i ↦ ⟨x i,Nat.lt_succ_of_le (hx i)⟩
  rw [show sizeLikelihood x N p ν = closedLikelihood y p ν by simp [sizeLikelihood,hx,y],
    closed_likelihood_probability_weights y hp ν]
  have hZ := weight_sum_positive hp ν (N := N)
  have hW (k : Fin (N+1)) : 0 ≤ probabilityWeight N p ν k := by
    unfold probabilityWeight
    exact mul_nonneg (mul_nonneg (Real.exp_pos _).le (pow_nonneg hp.1 _))
      (pow_nonneg (sub_nonneg.mpr hp.2) _)
  exact Finset.prod_le_one₀
    (fun i _ ↦ div_nonneg (hW (y i)) hZ.le)
    (fun i _ ↦ (div_le_one hZ).mpr
      (Finset.single_le_sum (fun k _ ↦ hW k) (Finset.mem_univ (y i))))

/-- At a rational dispersion, every compatible conditional height is
algebraic. Comparing the finitely many exact heights recovers every
global co-winner, with the zero and full-size constant strata included. -/
theorem rational_dispersion_capped_argmax {B n d : ℕ}
    (hn : 0 < n) (hd : 0 < d) (a : ℤ) (x : Fin n → ℕ) :
    let S : Set ℕ := {N | N ≤ B ∧ ∀ i, x i ≤ N}
    let D : Set (ℕ × ℝ) := {q | q.1 ∈ S ∧ q.2 ∈ Icc 0 1}
    let L := fun q : ℕ × ℝ ↦ sizeLikelihood x q.1 q.2 ((a : ℝ)/d)
    ∃ H : ℕ → ℝ,
      (∀ N ∈ S, IsAlgebraic ℚ (H N) ∧
        (∃ p ∈ Icc 0 1, L (N,p) = H N) ∧ ∀ p ∈ Icc 0 1, L (N,p) ≤ H N) ∧
      ∀ q ∈ D, IsMaxOn L D q ↔ L q = H q.1 ∧ ∀ N ∈ S, H N ≤ H q.1 := by
  intro S D L
  let ν := (a : ℝ)/d
  have cert (N : ℕ) (hN : N ∈ S) :
      IsAlgebraic ℚ (heightProfile x N ν) := by
    have hx := hN.2
    let y : Fin n → Fin (N+1) := fun i ↦ ⟨x i,Nat.lt_succ_of_le (hx i)⟩
    have eq_one {p : ℝ} (hp : p ∈ Icc 0 1) (he : sizeLikelihood x N p ν = 1) :
        heightProfile x N ν = 1 := by
      obtain ⟨q,hq,hq',hMax⟩ := height_realized hn x hx ν
      exact le_antisymm (hq'.symm.trans_le (size_likelihood_le_one x hx hq ν))
        (he ▸ hMax p hp)
    by_cases hz : ∀ i, x i = 0
    · have he : sizeLikelihood x N 0 ν = 1 := by
        have hy (i : Fin n) : y i = 0 := Fin.ext (by simpa [y] using hz i)
        simp [sizeLikelihood,hx,closedLikelihood,show ∀ i, y i = 0 from hy,y]
      rw [eq_one ⟨le_rfl,zero_le_one⟩ he]
      exact isAlgebraic_one
    by_cases hfull : ∀ i, x i = N
    · have he : sizeLikelihood x N 1 ν = 1 := by
        have hy (i : Fin n) : y i = Fin.last N := Fin.ext (by simpa [y] using hfull i)
        simp [sizeLikelihood,hx,closedLikelihood,show ∀ i, y i = Fin.last N from hy,y]
      rw [eq_one ⟨zero_le_one,le_rfl⟩ he]
      exact isAlgebraic_one
    have h₀ : ∃ i, 0 < x i := by
      obtain ⟨i,hi⟩ := not_forall.mp hz
      exact ⟨i,Nat.pos_of_ne_zero hi⟩
    have h₁ : ∃ i, x i < N := by
      obtain ⟨i,hi⟩ := not_forall.mp hfull
      exact ⟨i,lt_of_le_of_ne (hx i) hi⟩
    obtain ⟨r,hr,hAlg,hroot,hAlgH,hEq,hM⟩ := rational_dispersion_algebraic_profile hn hd a x hx h₀ h₁
    have hp : r/(1+r) ∈ Icc (0 : ℝ) 1 :=
      ⟨by positivity,(div_le_one (by positivity)).mpr (by linarith)⟩
    obtain ⟨q,hq,hq',hMax⟩ := height_realized hn x hx ν
    have HH : sizeLikelihood x N (r/(1+r)) ν = heightProfile x N ν :=
      le_antisymm (hMax _ hp) (hq' ▸ hM hq)
    rwa [← HH,hEq]
  refine ⟨fun N ↦ heightProfile x N ν, fun N hN ↦ ⟨cert N hN,?_⟩,?_⟩
  · obtain ⟨p,hp,he,hM⟩ := height_realized hn x hN.2 ν
    exact ⟨⟨p,hp,he⟩,hM⟩
  · intro q hq
    obtain ⟨p,hp,he,hM⟩ := height_realized hn x hq.1.2 ν
    constructor
    · intro h
      have hq' : L q = heightProfile x q.1 ν :=
        le_antisymm (hM q.2 hq.2) (he ▸ h (a := (q.1,p)) ⟨hq.1,hp⟩)
      refine ⟨hq', fun N hN ↦ ?_⟩
      obtain ⟨r,hr,he',hM'⟩ := height_realized hn x hN.2 ν
      exact he'.symm.le.trans ((h (a := (N,r)) ⟨hN,hr⟩).trans_eq hq')
    · rintro ⟨hq',hH⟩ s hs
      obtain ⟨r,hr,he',hM'⟩ := height_realized hn x hs.1.2 ν
      exact (hM' s.2 hs.2).trans ((hH s.1 hs.1).trans_eq hq'.symm)

end UnivariateML.Base8bCMB

end
