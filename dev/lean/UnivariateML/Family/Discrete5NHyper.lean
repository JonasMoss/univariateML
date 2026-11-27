module

public import UnivariateML.Family.Discrete4NHyper

import Mathlib.Tactic

set_option linter.style.header false

@[expose] public section

open Set Filter
open scoped Topology

namespace UnivariateML.Discrete5NHyper

open Discrete4NHyper

/-- At fixed population and composition, compatible zeros force stopping at
zero. Otherwise the first crossing, with its exact tie rule, gives the whole
maximizing set. -/
theorem held_population_composition {h N m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ r, ∀ i, Compatible N m r (x i)) :
    let D := {r | ∀ i, Compatible N m r (x i)}
    ((∀ i, x i = 0) ∧ D = {0}) ∨
      ((∀ i, 0 < x i) ∧
        let l := max 1 (Finset.univ.sup x - (N-m))
        let u := min m (Finset.univ.inf' ⟨⟨0,hh⟩,Finset.mem_univ _⟩ x)
        D = Icc l u ∧
        ∃ b ∈ D, (b < u → stoppingRatio x N m b ≤ 1) ∧
          (∀ j ∈ Ico l u, j < b → 1 < stoppingRatio x N m j) ∧
          ∀ j ∈ D, IsMaxOn (fun r ↦ totalLikelihood x N m r) D j ↔
            j = b ∨ (j = b+1 ∧ b < u ∧ stoppingRatio x N m b = 1)) := by
  intro D
  obtain ⟨r,hr⟩ := hf
  have hm := (hr ⟨0,hh⟩).1
  by_cases hz : r = 0
  · have hx (i : Fin h) : x i = 0 := by simpa [hz] using (hr i).2.2
    refine Or.inl ⟨hx, ?_⟩
    ext j
    change (∀ i, Compatible N m j (x i)) ↔ j = 0
    constructor
    · intro hj
      by_contra hj₀
      have hi := (hj ⟨0,hh⟩).2.2
      simp [hj₀, hx] at hi
    · rintro rfl i
      simp [Compatible, hm, hx]
  · have hx (i : Fin h) : 0 < x i := by
      have hi := (hr i).2.2
      rw [ite_eq_right hz] at hi
      exact (Nat.pos_of_ne_zero hz).trans_le hi.1
    refine Or.inr ⟨hx, ?_⟩
    dsimp only
    have hD : D = Icc (max 1 (Finset.univ.sup x - (N-m)))
        (min m (Finset.univ.inf' ⟨⟨0,hh⟩,Finset.mem_univ _⟩ x)) := by
      ext j
      change (∀ i, Compatible N m j (x i)) ↔ _
      constructor
      · intro hj
        have hj₀ : j ≠ 0 := by
          intro he
          have hi := (hj ⟨0,hh⟩).2.2
          exact (hx ⟨0,hh⟩).ne' (by simpa [he] using hi)
        have hi (i : Fin h) : j ≤ x i ∧ x i-j ≤ N-m := by
          simpa only [ite_eq_right hj₀] using (hj i).2.2
        have hsup : Finset.univ.sup x ≤ j+(N-m) :=
          Finset.sup_le fun i _ ↦ by have := (hi i).2; omega
        exact ⟨max_le (Nat.pos_of_ne_zero hj₀) (by omega),
          le_min (hj ⟨0,hh⟩).2.1 (Finset.le_inf' _ x fun i _ ↦ (hi i).1)⟩
      · intro hj i
        have hj₁ := (le_max_left 1 (Finset.univ.sup x - (N-m))).trans hj.1
        have hj₂ := (le_max_right 1 (Finset.univ.sup x - (N-m))).trans hj.1
        have hj₃ := hj.2.trans (min_le_right _ _)
        have hi : x i ≤ Finset.univ.sup x := Finset.le_sup (Finset.mem_univ i)
        refine ⟨hm, hj.2.trans (min_le_left _ _), ?_⟩
        rw [ite_eq_right (by omega : j ≠ 0)]
        exact ⟨hj₃.trans (Finset.inf'_le x (Finset.mem_univ i)), by omega⟩
    exact ⟨hD, hD.symm ▸ fixed_population_mark_crossing hh x hx ⟨r,hr⟩⟩

/-- At fixed population and stopping count, the zero fiber is flat.
For positive stopping count the exact crossing describes all co-winners. -/
theorem held_population_stopping {h N r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ m, ∀ i, Compatible N m r (x i)) :
    let D := {m | ∀ i, Compatible N m r (x i)}
    (r = 0 ∧ (∀ i, x i = 0) ∧ D = Iic N ∧
      ∀ m ∈ D, totalLikelihood x N m r = 1) ∨
    (0 < r ∧ (∀ i, r ≤ x i) ∧
      let u := N - NegHypergeometric.maxBlackDraws (fun i ↦ x i-r)
      D = Icc r u ∧
      ∃ b ∈ D, (b < u → compositionRatio x N r b ≤ 1) ∧
        (∀ j ∈ Ico r u, j < b → 1 < compositionRatio x N r j) ∧
        ∀ j ∈ D, IsMaxOn (fun m ↦ totalLikelihood x N m r) D j ↔
          j = b ∨ (j = b+1 ∧ b < u ∧ compositionRatio x N r b = 1)) := by
  intro D
  obtain ⟨m,hm⟩ := hf
  by_cases hr : r = 0
  · subst r
    have hx (i : Fin h) : x i = 0 := by simpa using (hm i).2.2
    have hD : D = Iic N := by
      ext j
      change (∀ i, Compatible N j 0 (x i)) ↔ j ≤ N
      exact ⟨fun hj ↦ (hj ⟨0,hh⟩).1, fun hj i ↦ by simp [Compatible, hj, hx]⟩
    exact Or.inl ⟨rfl,hx,hD,fun j hj ↦ held_zero_stop_flat x (show j ∈ Iic N from hD ▸ hj) hx⟩
  · have hx (i : Fin h) : r ≤ x i := by
      have hi := (hm i).2.2
      rw [ite_eq_right hr] at hi
      exact hi.1
    refine Or.inr ⟨Nat.pos_of_ne_zero hr,hx,?_⟩
    dsimp only
    let d := NegHypergeometric.maxBlackDraws (fun i ↦ x i-r)
    have hd (b : ℕ) : d ≤ b ↔ ∀ i, x i-r ≤ b :=
      NegHypergeometric.white_feasible_iff _ _
    have hD : D = Icc r (N-d) := by
      ext j
      change (∀ i, Compatible N j r (x i)) ↔ r ≤ j ∧ j ≤ N-d
      constructor
      · intro hj
        have hjN := (hj ⟨0,hh⟩).1
        have hjd : d ≤ N-j := (hd _).mpr fun i ↦ by
          have hi := (hj i).2.2
          rw [ite_eq_right hr] at hi
          exact hi.2
        exact ⟨(hj ⟨0,hh⟩).2.1,by omega⟩
      · rintro ⟨hrj,hj⟩ i
        have hdN : d ≤ N := by
          apply (hd _).mpr
          intro k
          have hk := (hm k).2.2
          rw [ite_eq_right hr] at hk
          exact hk.2.trans (Nat.sub_le _ _)
        have hjN : j ≤ N := hj.trans (Nat.sub_le _ _)
        refine ⟨hjN,hrj,?_⟩
        rw [ite_eq_right hr]
        exact ⟨hx i,(hd _).mp (by omega) i⟩
    exact ⟨hD, hD.symm ▸ fixed_population_stop_crossing hh x (Nat.pos_of_ne_zero hr) hx ⟨m,hm⟩⟩

/-- At fixed population on positive data, retain the first crossing and
any tied successor in each compatible stopping slice, then compare
their full likelihoods. On zeros, all admissible compositions at stop zero win. -/
theorem held_population_maximizers {h N : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ m r, ∀ i, Compatible N m r (x i)) :
    let D := {p : ℕ × ℕ | ∀ i, Compatible N p.1 p.2 (x i)}
    let L := fun p : ℕ × ℕ ↦ totalLikelihood x N p.1 p.2
    let U := fun r ↦ N - NegHypergeometric.maxBlackDraws (fun i ↦ x i-r)
    ((∀ i, x i = 0) ∧ {p ∈ D | IsMaxOn L D p} = {p | p.1 ≤ N ∧ p.2 = 0}) ∨
    ((∀ i, 0 < x i) ∧ ∃ b : ℕ → ℕ,
      let feasibleStop := {r | ∃ m, (m,r) ∈ D}
      let candidates := {p ∈ D | p.1 = b p.2 ∨
        (p.1 = b p.2+1 ∧ b p.2 < U p.2 ∧ compositionRatio x N p.2 (b p.2) = 1)}
      (∀ r ∈ feasibleStop, (b r,r) ∈ D ∧
        (b r < U r → compositionRatio x N r (b r) ≤ 1) ∧
        (∀ j ∈ Ico r (U r), j < b r → 1 < compositionRatio x N r j)) ∧
      {p ∈ D | IsMaxOn L D p} = {p ∈ candidates | IsMaxOn L candidates p}) := by
  classical
  intro D L U
  obtain ⟨m,r,hr⟩ := hf
  by_cases hz : r = 0
  · have hx (i : Fin h) : x i = 0 := by simpa [hz] using (hr i).2.2
    refine Or.inl ⟨hx,?_⟩
    have hD (p : ℕ × ℕ) : p ∈ D ↔ p.1 ≤ N ∧ p.2 = 0 := by
      constructor
      · intro hp
        refine ⟨(hp ⟨0,hh⟩).1,?_⟩
        by_contra h
        have hi := (hp ⟨0,hh⟩).2.2
        simp [h,hx] at hi
      · rintro ⟨hm,hr⟩ i
        simp [Compatible,hm,hr,hx]
    have hL (p : ℕ × ℕ) (hp : p ∈ D) : L p = 1 := by
      obtain ⟨hm,hr⟩ := (hD p).1 hp
      change totalLikelihood x N p.1 p.2 = 1
      rw [hr]
      exact held_zero_stop_flat x hm hx
    ext p
    change (p ∈ D ∧ IsMaxOn L D p) ↔ _
    exact ⟨fun hp ↦ (hD p).1 hp.1, fun hp ↦ by
      have h := (hD p).2 hp
      exact ⟨h,fun q hq ↦ le_of_eq ((hL q hq).trans (hL p h).symm)⟩⟩
  · have hx (i : Fin h) : 0 < x i := by
      have hi := (hr i).2.2
      rw [ite_eq_right hz] at hi
      exact (Nat.pos_of_ne_zero hz).trans_le hi.1
    refine Or.inr ⟨hx,?_⟩
    have slice (r : ℕ) (hf : ∃ m, (m,r) ∈ D) :
        ∃ b, (b,r) ∈ D ∧ (b < U r → compositionRatio x N r b ≤ 1) ∧
          (∀ j ∈ Ico r (U r), j < b → 1 < compositionRatio x N r j) ∧
          ∀ j, (j,r) ∈ D →
            (IsMaxOn (fun m ↦ L (m,r)) {m | (m,r) ∈ D} j ↔
             j = b ∨ (j = b+1 ∧ b < U r ∧ compositionRatio x N r b = 1)) := by
      rcases held_population_stopping hh x hf with hzero | ⟨_,_,he,b,hb,hcross,hprior,hmax⟩
      · exact ((hx ⟨0,hh⟩).ne' (hzero.2.1 ⟨0,hh⟩)).elim
      exact ⟨b,hb,hcross,hprior,fun j hj ↦ hmax j hj⟩
    let b (r : ℕ) := if hf : ∃ m, (m,r) ∈ D then (slice r hf).choose else 0
    have hb (r : ℕ) (hf : ∃ m, (m,r) ∈ D) := (slice r hf).choose_spec
    have hb' (r : ℕ) (hf : ∃ m, (m,r) ∈ D) : b r = (slice r hf).choose :=
      dite_eq_left hf
    refine ⟨b, ?_, ?_⟩
    · intro r hf
      rw [hb' r hf]
      exact ⟨(hb r hf).1,(hb r hf).2.1,(hb r hf).2.2.1⟩
    let C := {p ∈ D | p.1 = b p.2 ∨
      (p.1 = b p.2+1 ∧ b p.2 < U p.2 ∧ compositionRatio x N p.2 (b p.2) = 1)}
    have memC (p : ℕ × ℕ) (hp : p ∈ D) :
        p ∈ C ↔ IsMaxOn (fun m ↦ L (m,p.2)) {m | (m,p.2) ∈ D} p.1 := by
      let hf : ∃ m, (m,p.2) ∈ D := ⟨p.1,hp⟩
      change (p ∈ D ∧ (p.1 = b p.2 ∨ _)) ↔ _
      rw [and_iff_right hp, hb' p.2 hf]
      exact ((hb p.2 hf).2.2.2 _ hp).symm
    have best (r : ℕ) (hf : ∃ m, (m,r) ∈ D) : (b r,r) ∈ C := by
      change (b r,r) ∈ D ∧ _
      exact ⟨by rw [hb' r hf]; exact (hb r hf).1, Or.inl rfl⟩
    change {p ∈ D | IsMaxOn L D p} = {p ∈ C | IsMaxOn L C p}
    ext p
    change (p ∈ D ∧ IsMaxOn L D p) ↔ p ∈ C ∧ IsMaxOn L C p
    constructor
    · rintro ⟨hp,hM⟩
      refine ⟨(memC p hp).2 (fun j hj ↦ hM hj), hM.of_subset (fun q hq ↦ hq.1)⟩
    · rintro ⟨hp,hM⟩
      refine ⟨hp.1,fun q hq ↦ ?_⟩
      have hbest := best q.2 ⟨q.1,hq⟩
      have hle : L q ≤ L (b q.2,q.2) := (memC _ hbest.1).1 hbest hq
      exact hle.trans (hM hbest)

/-- Holding composition and stopping count, compatible data either give a
flat zero-stop ray or have the stated one- or two-point maximizing set
and zero likelihood at population infinity. -/
theorem held_composition_stopping {h m r : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ n, ∀ i, Compatible (m+n) m r (x i)) :
    let D := {n | ∀ i, Compatible (m+n) m r (x i)}
    let L := fun n ↦ totalLikelihood x (m+n) m r
    (r = 0 ∧ (∀ i, x i = 0) ∧ D = univ ∧ ∀ n, L n = 1) ∨
    (0 < r ∧
      let y := fun i ↦ x i-r
      D = Ici (NegHypergeometric.maxBlackDraws y) ∧
      Tendsto L atTop (𝓝 0) ∧
      ∃ b ∈ D, NegHypergeometric.whiteRatio r m (m+b+1) y ≤ 1 ∧
        (∀ j ∈ D, j < b → 1 < NegHypergeometric.whiteRatio r m (m+j+1) y) ∧
        ∀ j, (IsMaxOn L D j ∧ j ∈ D) ↔ j = b ∨
          (j = b+1 ∧ NegHypergeometric.whiteRatio r m (m+b+1) y = 1)) := by
  intro D L
  obtain ⟨n,hn⟩ := hf
  by_cases hr : r = 0
  · subst r
    have hx (i : Fin h) : x i = 0 := by simpa using (hn i).2.2
    have he : D = univ := by
      ext j
      change (∀ i, Compatible (m+j) m 0 (x i)) ↔ j ∈ univ
      simp [Compatible,hx]
    exact Or.inl ⟨rfl,hx,he,fun j ↦ held_zero_stop_flat x (by omega) hx⟩
  · have hr' := Nat.pos_of_ne_zero hr
    have hm := (hn ⟨0,hh⟩).2.1
    have hx (i : Fin h) : r ≤ x i := by
      have hi := (hn i).2.2
      rw [ite_eq_right hr] at hi
      exact hi.1
    let y := fun i ↦ x i-r
    have he : D = Ici (NegHypergeometric.maxBlackDraws y) := by
      ext j
      change (∀ i, Compatible (m+j) m r (x i)) ↔ j ∈ Ici _
      rw [NegHypergeometric.white_feasible_iff]
      constructor
      · intro hj i
        have hi := (hj i).2.2
        rw [ite_eq_right hr] at hi
        simpa [y] using hi.2
      · intro hj i
        exact ⟨by omega,hm,by simpa [hr,y] using And.intro (hx i) (hj i)⟩
    refine Or.inr ⟨hr',he,held_mark_stop_limit hh x hr' hm,?_⟩
    exact he.symm ▸ held_mark_stop_positive hh x hr' hm hx

/-- With composition held, compare the one or two full-likelihood winners
per positive stopping slice. Every compatible zero-stop urn wins on all-zero data. -/
theorem held_composition_maximizers {h m : ℕ} (hh : 0 < h) (x : Fin h → ℕ)
    (hf : ∃ n r, ∀ i, Compatible (m+n) m r (x i)) :
    let D := {p : ℕ × ℕ | ∀ i, Compatible (m+p.1) m p.2 (x i)}
    let L := fun p : ℕ × ℕ ↦ totalLikelihood x (m+p.1) m p.2
    let R := fun (r j : ℕ) ↦ NegHypergeometric.whiteRatio r m (m+j+1) (fun i ↦ x i-r)
    ((∀ i, x i = 0) ∧ {p ∈ D | IsMaxOn L D p} = {p | p.2 = 0}) ∨
    ((∀ i, 0 < x i) ∧ ∃ b : ℕ → ℕ,
      let feasibleStop := {r | ∃ n, (n,r) ∈ D}
      let candidates := {p ∈ D | p.1 = b p.2 ∨
        (p.1 = b p.2+1 ∧ R p.2 (b p.2) = 1)}
      (∀ r ∈ feasibleStop, (b r,r) ∈ D ∧ R r (b r) ≤ 1 ∧
        (∀ j, (j,r) ∈ D → j < b r → 1 < R r j)) ∧
      candidates.Finite ∧
      {p ∈ D | IsMaxOn L D p} = {p ∈ candidates | IsMaxOn L candidates p}) := by
  classical
  intro D L R
  obtain ⟨n,r,hr⟩ := hf
  by_cases hz : r = 0
  · have hx (i : Fin h) : x i = 0 := by simpa [hz] using (hr i).2.2
    refine Or.inl ⟨hx,?_⟩
    have hD (p : ℕ × ℕ) : p ∈ D ↔ p.2 = 0 := by
      constructor
      · intro hp
        by_contra h
        have hi := (hp ⟨0,hh⟩).2.2
        simp [h,hx] at hi
      · intro hp i
        simp [Compatible,hp,hx]
    have hL (p : ℕ × ℕ) (hp : p ∈ D) : L p = 1 := by
      change totalLikelihood x (m+p.1) m p.2 = 1
      rw [(hD p).1 hp]
      exact held_zero_stop_flat x (by omega) hx
    ext p
    change (p ∈ D ∧ IsMaxOn L D p) ↔ p.2 = 0
    exact ⟨fun hp ↦ (hD p).1 hp.1, fun hp ↦ by
      have h := (hD p).2 hp
      exact ⟨h,fun q hq ↦ le_of_eq ((hL q hq).trans (hL p h).symm)⟩⟩
  · have hx (i : Fin h) : 0 < x i := by
      have hi := (hr i).2.2
      rw [ite_eq_right hz] at hi
      exact (Nat.pos_of_ne_zero hz).trans_le hi.1
    refine Or.inr ⟨hx,?_⟩
    have slice (r : ℕ) (hf : ∃ n, (n,r) ∈ D) :
        ∃ b, (b,r) ∈ D ∧ R r b ≤ 1 ∧
          (∀ j, (j,r) ∈ D → j < b → 1 < R r j) ∧
          ∀ j, (j,r) ∈ D →
            (IsMaxOn (fun n ↦ L (n,r)) {n | (n,r) ∈ D} j ↔
             j = b ∨ (j = b+1 ∧ R r b = 1)) := by
      rcases held_composition_stopping hh x hf with hzero | ⟨_,he,_,b,hb,hcross,hprior,hmax⟩
      · exact ((hx ⟨0,hh⟩).ne' (hzero.2.1 ⟨0,hh⟩)).elim
      refine ⟨b,hb,hcross,hprior,fun j hj ↦ ?_⟩
      exact ⟨fun h ↦ (hmax j).1 ⟨h,hj⟩,fun h ↦ ((hmax j).2 h).1⟩
    let b (r : ℕ) := if hf : ∃ n, (n,r) ∈ D then (slice r hf).choose else 0
    have hb (r : ℕ) (hf : ∃ n, (n,r) ∈ D) := (slice r hf).choose_spec
    have hb' (r : ℕ) (hf : ∃ n, (n,r) ∈ D) : b r = (slice r hf).choose :=
      dite_eq_left hf
    let C := {p ∈ D | p.1 = b p.2 ∨ (p.1 = b p.2+1 ∧ R p.2 (b p.2) = 1)}
    refine ⟨b, ?_, ?_, ?_⟩
    · intro r hf
      rw [hb' r hf]
      exact ⟨(hb r hf).1,(hb r hf).2.1,(hb r hf).2.2.1⟩
    · change C.Finite
      apply (((finite_Iic m).image (fun r ↦ (b r,r))).union
        ((finite_Iic m).image (fun r ↦ (b r+1,r)))).subset
      intro p hp
      have hm := (hp.1 ⟨0,hh⟩).2.1
      rcases hp.2 with he | ⟨he,_⟩
      · exact Or.inl ⟨p.2,hm,Prod.ext he.symm rfl⟩
      · exact Or.inr ⟨p.2,hm,Prod.ext he.symm rfl⟩
    have memC (p : ℕ × ℕ) (hp : p ∈ D) :
        p ∈ C ↔ IsMaxOn (fun n ↦ L (n,p.2)) {n | (n,p.2) ∈ D} p.1 := by
      let hf : ∃ n, (n,p.2) ∈ D := ⟨p.1,hp⟩
      change (p ∈ D ∧ (p.1 = b p.2 ∨ _)) ↔ _
      rw [and_iff_right hp, hb' p.2 hf]
      exact ((hb p.2 hf).2.2.2 _ hp).symm
    have best (r : ℕ) (hf : ∃ n, (n,r) ∈ D) : (b r,r) ∈ C := by
      change (b r,r) ∈ D ∧ _
      exact ⟨by rw [hb' r hf]; exact (hb r hf).1, Or.inl rfl⟩
    change {p ∈ D | IsMaxOn L D p} = {p ∈ C | IsMaxOn L C p}
    ext p
    change (p ∈ D ∧ IsMaxOn L D p) ↔ p ∈ C ∧ IsMaxOn L C p
    constructor
    · rintro ⟨hp,hM⟩
      refine ⟨(memC p hp).2 (fun j hj ↦ hM hj), hM.of_subset (fun q hq ↦ hq.1)⟩
    · rintro ⟨hp,hM⟩
      refine ⟨hp.1,fun q hq ↦ ?_⟩
      have hbest := best q.2 ⟨q.1,hq⟩
      have hle : L q ≤ L (b q.2,q.2) := (memC _ hbest.1).1 hbest hq
      exact hle.trans (hM hbest)

end UnivariateML.Discrete5NHyper

end
