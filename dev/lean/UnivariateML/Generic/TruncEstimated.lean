module

public import Mathlib.Algebra.Order.BigOperators.Ring.Finset
public import Mathlib.Algebra.Order.Group.Indicator
public import Mathlib.Topology.Algebra.InfiniteSum.ENNReal
public import Mathlib.Tactic.Linarith

/-! Profiling ordinary count windows under a common parent mass function.
A window-dependent extension of the parent law requires a separate comparison. -/

@[expose] public section

open Set
open scoped BigOperators

namespace UnivariateML.TruncEstimated

noncomputable section

/-- A nominal closed window. An infinite upper cut gives a ray. -/
abbrev Window := ℝ × WithTop ℝ

def cells (w : Window) : Set ℕ :=
  {k | w.1 ≤ (k : ℝ) ∧ ((k : ℝ) : WithTop ℝ) ≤ w.2}

inductive Design
  | lower | upper | both | known

def allows (d : Design) (fixed w : Window) : Prop :=
  match d with
  | .lower => w.2 = fixed.2
  | .upper => w.1 = fixed.1
  | .both => True
  | .known => w = fixed

def tight (d : Design) (fixed : Window) (m h : ℕ) : Window :=
  match d with
  | .lower => (m, fixed.2)
  | .upper => (fixed.1, (h : ℝ))
  | .both => (m, (h : ℝ))
  | .known => fixed

/-- The tight design is feasible and contained in every compatible nominal window.
The attained sample extrema are supplied explicitly to cover any finite index type. -/
theorem tight_cells {ι : Type*} {x : ι → ℕ} {m h : ℕ}
    (hm : ∃ i, x i = m) (hh : ∃ i, x i = h)
    (hb : ∀ i, m ≤ x i ∧ x i ≤ h) {d fixed w}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    allows d fixed (tight d fixed m h) ∧
      (∀ i, x i ∈ cells (tight d fixed m h)) ∧ cells (tight d fixed m h) ⊆ cells w := by
  obtain ⟨i, hi⟩ := hm
  obtain ⟨j, hj⟩ := hh
  have hl : w.1 ≤ (m : ℝ) := hi ▸ (hx i).1
  have hu : ((h : ℝ) : WithTop ℝ) ≤ w.2 := hj ▸ (hx j).2
  have hlow (k : ι) : (m : ℝ) ≤ x k := by exact_mod_cast (hb k).1
  have hhigh (k : ι) : ((x k : ℝ) : WithTop ℝ) ≤ (h : ℝ) := by
    exact WithTop.coe_le_coe.mpr (by exact_mod_cast (hb k).2)
  cases d
  · change w.2 = fixed.2 at hw
    exact ⟨rfl, fun k ↦ ⟨hlow k, hw ▸ (hx k).2⟩,
      fun _ hk ↦ ⟨hl.trans hk.1, hw.symm ▸ hk.2⟩⟩
  · change w.1 = fixed.1 at hw
    exact ⟨rfl, fun k ↦ ⟨hw ▸ (hx k).1, hhigh k⟩,
      fun _ hk ↦ ⟨hw.symm ▸ hk.1, hk.2.trans hu⟩⟩
  · exact ⟨trivial, fun k ↦ ⟨hlow k, hhigh k⟩,
      fun _ hk ↦ ⟨hl.trans hk.1, hk.2.trans hu⟩⟩
  · change w = fixed at hw
    subst w
    exact ⟨rfl, hx, Subset.rfl⟩

def mass (f : ℕ → ℝ) (A : Set ℕ) : ℝ := ∑' k, A.indicator f k

def likelihood {n : ℕ} (x : Fin n → ℕ) (f : ℕ → ℝ) (A : Set ℕ) : ℝ :=
  (∏ i, f (x i)) / mass f A ^ n

private theorem restricted_summable {f : ℕ → ℝ} (hf : ∀ k, 0 ≤ f k)
    (hs : Summable f) (A : Set ℕ) : Summable (A.indicator f) := by
  classical
  exact hs.of_nonneg_of_le (fun k ↦ by by_cases hk : k ∈ A <;> simp [hk, hf])
    (fun k ↦ by by_cases hk : k ∈ A <;> simp [hk, hf])

private theorem mass_pos {f : ℕ → ℝ} (hf : ∀ k, 0 ≤ f k)
    (hs : Summable f) {A : Set ℕ} {k : ℕ} (hk : k ∈ A) (hp : 0 < f k) :
    0 < mass f A := by
  have H := (restricted_summable hf hs A).le_tsum k
    (fun j _ ↦ indicator_nonneg (fun k _ ↦ hf k) _)
  exact hp.trans_le (by simpa [mass, hk] using H)

/-- Compatibility and likelihood comparison for nested windows. Equality means
every removed cell has zero parent mass. The same statement covers rays. -/
theorem likelihood_shrink {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {f : ℕ → ℝ}
    (hf : ∀ k, 0 ≤ f k) (hs : Summable f) (hx : ∀ i, 0 < f (x i))
    {A B : Set ℕ} (hab : A ⊆ B) (ha : ∀ i, x i ∈ A) :
    0 < mass f A ∧ likelihood x f B ≤ likelihood x f A ∧
      (likelihood x f B = likelihood x f A ↔ ∀ k ∈ B \ A, f k = 0) := by
  classical
  have he : mass f B = mass f A + mass f (B \ A) := by
    unfold mass
    rw [← (restricted_summable hf hs A).tsum_add (restricted_summable hf hs (B \ A))]
    congr 1
    funext k
    by_cases hA : k ∈ A
    · simp [hA, hab hA]
    · by_cases hB : k ∈ B <;> simp [hA, hB]
  have hz : 0 ≤ mass f (B \ A) :=
    tsum_nonneg (fun k ↦ indicator_nonneg (fun j _ ↦ hf j) k)
  have hzero : mass f (B \ A) = 0 ↔ ∀ k ∈ B \ A, f k = 0 := by
    constructor
    · intro heq k hk
      by_contra hne
      exact (mass_pos hf hs hk (lt_of_le_of_ne (hf k) (Ne.symm hne))).ne' heq
    · intro h
      have hi : (B \ A).indicator f = 0 := by
        funext k
        by_cases hk : k ∈ B \ A <;> simp [hk, h]
      simp [mass, hi]
  have hA := mass_pos hf hs (ha ⟨0, hn⟩) (hx ⟨0, hn⟩)
  have hAB : mass f A ≤ mass f B := by linarith
  have hC : 0 < ∏ i, f (x i) := Finset.prod_pos (fun i _ ↦ hx i)
  refine ⟨hA, div_le_div_of_nonneg_left hC.le (pow_pos hA _) (pow_le_pow_left₀ hA.le hAB _), ?_⟩
  rw [← hzero]
  constructor
  · intro h
    by_contra hne
    have hlt : mass f A < mass f B := by linarith [lt_of_le_of_ne hz (Ne.symm hne)]
    have hd := pow_lt_pow_left₀ hlt hA.le hn.ne'
    exact (div_lt_div_of_pos_left hC (pow_pos hA n) hd).ne h
  · intro h
    have : mass f B = mass f A := by linarith
    simp [likelihood, this]

/-- A compatible conditional sample likelihood is at most one. -/
theorem likelihood_le_one {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {f : ℕ → ℝ}
    (hf : ∀ k, 0 ≤ f k) (hs : Summable f) {A : Set ℕ}
    (hx : ∀ i, x i ∈ A ∧ 0 < f (x i)) :
    likelihood x f A ≤ 1 := by
  have hle (i : Fin n) : f (x i) ≤ mass f A := by
    have H := (restricted_summable hf hs A).le_tsum (x i)
      (fun j _ ↦ indicator_nonneg (fun k _ ↦ hf k) _)
    exact le_trans (le_of_eq (indicator_of_mem (hx i).1 f).symm) H
  refine (div_le_one (pow_pos (mass_pos hf hs (hx ⟨0,hn⟩).1 (hx ⟨0,hn⟩).2) n)).mpr ?_
  simpa using Finset.prod_le_prod₀ (s := Finset.univ) (fun i _ ↦ (hx i).2.le) (fun i _ ↦ hle i)

/-- At an admitted singleton every compatible parent coefficient gives likelihood one. -/
theorem likelihood_singleton {n : ℕ} {x : Fin n → ℕ} {f : ℕ → ℝ}
    {j : ℕ} (hx : ∀ i, x i = j) (hj : 0 < f j) :
    likelihood x f {j} = 1 := by
  classical
  have he : ({j} : Set ℕ).indicator f = Pi.single j (f j) := by
    ext k
    by_cases hk : k = j <;> simp [hk]
  simp [likelihood, mass, he, hx, hj.ne']

/-- Ordinary joint candidates have a compatible window and positive likelihood. -/
def candidates {P : Type*} {n : ℕ} (x : Fin n → ℕ) (f : P → ℕ → ℝ)
    (D : Set P) (d : Design) (fixed : Window) : Set (P × Window) :=
  {p | p.1 ∈ D ∧ allows d fixed p.2 ∧ ∀ i, x i ∈ cells p.2 ∧ 0 < f p.1 (x i)}

/-- Exact supremum and maximizer-set transfer, with all nominal cut ties. -/
theorem profile {P : Type*} {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {m h : ℕ}
    (hm : ∃ i, x i = m) (hh : ∃ i, x i = h) (hb : ∀ i, m ≤ x i ∧ x i ≤ h)
    {f : P → ℕ → ℝ} {D : Set P}
    (hf : ∀ p ∈ D, ∀ k, 0 ≤ f p k) (hs : ∀ p ∈ D, Summable (f p))
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    let T := tight d fixed m h
    let E := {p ∈ D | ∀ i, 0 < f p (x i)}
    let C := candidates x f D d fixed
    let L := fun p : P × Window ↦ likelihood x (f p.1) (cells p.2)
    sSup (L '' C) = sSup ((fun p ↦ L (p,T)) '' E) ∧
      (∀ q ∈ C, IsMaxOn L C q ↔
        IsMaxOn (fun p ↦ L (p,T)) E q.1 ∧
          ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0) ∧
      ((∃ q ∈ C, IsMaxOn L C q) ↔ ∃ p ∈ E, IsMaxOn (fun r ↦ L (r,T)) E p) := by
  dsimp only
  let T := tight d fixed m h
  let E := {p ∈ D | ∀ i, 0 < f p (x i)}
  let C := candidates x f D d fixed
  let L := fun p : P × Window ↦ likelihood x (f p.1) (cells p.2)
  have ht := tight_cells hm hh hb hw hx
  have lift (p : P) (hp : p ∈ E) : (p,T) ∈ C :=
    ⟨hp.1, ht.1, fun i ↦ ⟨ht.2.1 i, hp.2 i⟩⟩
  have down (q : P × Window) (hq : q ∈ C) : q.1 ∈ E :=
    ⟨hq.1, fun i ↦ (hq.2.2 i).2⟩
  have bound (q : P × Window) (hq : q ∈ C) :
      L q ≤ L (q.1,T) ∧ (L q = L (q.1,T) ↔ ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0) :=
    (likelihood_shrink hn (hf _ hq.1) (hs _ hq.1) (down q hq).2
      (tight_cells hm hh hb hq.2.1 (fun i ↦ (hq.2.2 i).1)).2.2 ht.2.1).2
  have hmax_iff (q : P × Window) (hq : q ∈ C) :
      IsMaxOn L C q ↔ IsMaxOn (fun p ↦ L (p,T)) E q.1 ∧
        ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0 := by
    constructor
    · intro hmax
      have he := le_antisymm (bound q hq).1 (hmax (lift q.1 (down q hq)))
      exact ⟨fun p hp ↦ (hmax (lift p hp)).trans he.le, (bound q hq).2.mp he⟩
    · rintro ⟨hmax, hz⟩ p hp
      exact (bound p hp).1.trans ((hmax (down p hp)).trans ((bound q hq).2.mpr hz).ge)
  refine ⟨?_, hmax_iff, ?_⟩
  · have bounded : BddAbove ((fun p ↦ L (p,T)) '' E) := by
      refine ⟨1, ?_⟩
      rintro _ ⟨p, hp, rfl⟩
      exact likelihood_le_one hn (hf p hp.1) (hs p hp.1) (fun i ↦ ⟨ht.2.1 i,hp.2 i⟩)
    by_cases hE : E.Nonempty
    · have hj : (L '' C).Nonempty := by
        obtain ⟨p,hp⟩ := hE
        exact ⟨L (p,T), (p,T), lift p hp, rfl⟩
      have hB : BddAbove (L '' C) := by
        obtain ⟨r,hr⟩ := bounded
        refine ⟨r, ?_⟩
        rintro _ ⟨q,hq,rfl⟩
        exact (bound q hq).1.trans (hr ⟨q.1, down q hq, rfl⟩)
      apply le_antisymm
      · refine csSup_le hj ?_
        rintro _ ⟨q,hq,rfl⟩
        exact (bound q hq).1.trans (le_csSup bounded ⟨q.1, down q hq, rfl⟩)
      · refine csSup_le (hE.image _) ?_
        rintro _ ⟨p,hp,rfl⟩
        exact le_csSup hB ⟨(p,T), lift p hp, rfl⟩
    · have he : E = ∅ := not_nonempty_iff_eq_empty.mp hE
      have hc : C = ∅ := not_nonempty_iff_eq_empty.mp (fun ⟨q,hq⟩ ↦ hE ⟨q.1,down q hq⟩)
      change sSup (L '' C) = sSup ((fun p ↦ L (p,T)) '' E)
      rw [he, hc, image_empty, image_empty]
  · constructor
    · rintro ⟨q,hq,hmax⟩
      exact ⟨q.1, down q hq, ((hmax_iff q hq).mp hmax).1⟩
    · rintro ⟨p,hp,hmax⟩
      exact ⟨(p,T), lift p hp, (hmax_iff (p,T) (lift p hp)).mpr
        ⟨hmax, fun k hk ↦ False.elim (hk.2 hk.1)⟩⟩

end

end UnivariateML.TruncEstimated

end
