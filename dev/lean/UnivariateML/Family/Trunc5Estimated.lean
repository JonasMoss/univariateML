module

public import UnivariateML.Family.Trunc4Count
public import UnivariateML.Family.CMP

/-! Nominal estimated CMB and CMP windows, with common parent weights.
The weights may be unnormalized. The domain can be intersected with
any public hold restrictions before profiling the cuts. -/

@[expose] public section

open Set Real
open UnivariateML.Trunc4Count (Window cells allows tight)
open UnivariateML.TruncEstimated (Design likelihood)

namespace UnivariateML.Trunc5Estimated

noncomputable section

/-- CMB parent weights at a size and either interior log odds and dispersion
or a deterministic endpoint. Dispersion is immaterial on an atom tag. -/
def cmbWeight (p : ℕ × (Sum ℝ Bool × ℝ)) (k : ℕ) : ℝ :=
  match p.2.1 with
  | .inl η => if k ≤ p.1 then exp (η*k + p.2.2*log (p.1.choose k : ℝ)) else 0
  | .inr b => if k = (if b then p.1 else 0) then 1 else 0

/-- CMP parameter domain in log-rate coordinates, including the zero-rate tag. -/
def cmpDomain : Set (Option ℝ × ℝ) :=
  {p | match p.1 with
    | none => 0 ≤ p.2
    | some η => (η,p.2) ∈ CMP.domain}

/-- CMP parent weights, including the deterministic zero-rate parent. -/
def cmpWeight (p : Option ℝ × ℝ) (k : ℕ) : ℝ :=
  match p.1 with
  | none => if k = 0 then 1 else 0
  | some η => CMP.weight η p.2 k

private theorem cmb_nonneg (p : ℕ × (Sum ℝ Bool × ℝ)) (k : ℕ) : 0 ≤ cmbWeight p k := by
  unfold cmbWeight
  split <;> split <;> positivity

private theorem cmb_sum (p : ℕ × (Sum ℝ Bool × ℝ)) : Summable (cmbWeight p) := by
  apply summable_of_ne_finset_zero (s := Finset.range (p.1+1))
  intro k hk
  have hn : p.1 < k := by simpa using Nat.le_of_not_lt (fun h ↦ hk (Finset.mem_range.mpr h))
  unfold cmbWeight
  split
  · exact ite_eq_right hn.not_ge
  · apply ite_eq_right
    split <;> omega

private theorem cmp_nonneg (p : Option ℝ × ℝ) (k : ℕ) : 0 ≤ cmpWeight p k := by
  unfold cmpWeight
  split
  · split <;> positivity
  · exact (exp_pos _).le

private theorem cmp_sum {p : Option ℝ × ℝ} (hp : p ∈ cmpDomain) : Summable (cmpWeight p) := by
  rcases p with ⟨η | _,ν⟩
  · exact summable_of_ne_finset_zero (s := {0}) (by simp [cmpWeight])
  · exact CMP.summable_weight hp

private theorem tight_cells {n : ℕ} {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    allows d fixed (tight d fixed lo hi) ∧
      (∀ i, x i ∈ cells (tight d fixed lo hi)) ∧ cells (tight d fixed lo hi) ⊆ cells w := by
  obtain ⟨i,hi'⟩ := hm
  obtain ⟨j,hj⟩ := hh
  have hl : w.1 ≤ ((lo : ℝ) : WithBot ℝ) := hi' ▸ (hx i).1
  have hu : ((hi : ℝ) : WithTop ℝ) ≤ w.2 := hj ▸ (hx j).2
  have hlow (k : Fin n) : ((lo : ℝ) : WithBot ℝ) ≤ (x k : ℝ) :=
    WithBot.coe_le_coe.mpr (by exact_mod_cast (hb k).1)
  have hhigh (k : Fin n) : ((x k : ℝ) : WithTop ℝ) ≤ (hi : ℝ) :=
    WithTop.coe_le_coe.mpr (by exact_mod_cast (hb k).2)
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
    exact ⟨rfl,hx,Subset.rfl⟩

private theorem lower_half (m k : ℕ) : (m : ℝ)-1/2 ≤ k ↔ (m : ℝ) ≤ k := by
  constructor
  · intro H
    by_contra! hk
    have hn : k+1 ≤ m := by exact_mod_cast hk
    have hc : (k : ℝ)+1 ≤ m := by exact_mod_cast hn
    linarith
  · intro H
    linarith

private theorem upper_half (m k : ℕ) : (k : ℝ) ≤ (m : ℝ)+1/2 ↔ (k : ℝ) ≤ m := by
  constructor
  · intro H
    by_contra! hk
    have hn : m+1 ≤ k := by exact_mod_cast hk
    have hc : (m : ℝ)+1 ≤ k := by exact_mod_cast hn
    linarith
  · intro H
    linarith

private theorem second_window {d : Design} (hd : d ≠ .known) (fixed : Window) (lo hi : ℕ) :
    ∃ v, allows d fixed v ∧ v ≠ tight d fixed lo hi ∧
      cells v = cells (tight d fixed lo hi) := by
  cases d
  · refine ⟨(((lo : ℝ)-1/2 : ℝ),fixed.2),rfl,?_,?_⟩
    · intro H
      have He := WithBot.coe_eq_coe.mp (congrArg Prod.fst H)
      linarith
    · ext k
      simp only [cells,mem_ofPred_eq,tight,WithBot.coe_le_coe,lower_half]
  · refine ⟨(fixed.1,((hi : ℝ)+1/2 : ℝ)),rfl,?_,?_⟩
    · intro H
      have He := WithTop.coe_eq_coe.mp (congrArg Prod.snd H)
      linarith
    · ext k
      simp only [cells,mem_ofPred_eq,tight,WithTop.coe_le_coe,upper_half]
  · refine ⟨(((lo : ℝ)-1/2 : ℝ),(hi : ℝ)),trivial,?_,?_⟩
    · intro H
      have He := WithBot.coe_eq_coe.mp (congrArg Prod.fst H)
      linarith
    · ext k
      simp only [cells,mem_ofPred_eq,tight,WithBot.coe_le_coe,lower_half]
  · exact (hd rfl).elim

/-- The estimated-window comparison, including all zero-padding ties and
nonuniqueness of nominal real cuts. The coefficient optimization on the
tight window can use any subset of the parent domain specified by holds. -/
def EstimatedClaim {P : Type*} {n : ℕ} (x : Fin n → ℕ) (lo hi : ℕ)
    (f : P → ℕ → ℝ) (D : Set P) (d : Design) (fixed : Window) : Prop :=
  let T := tight d fixed lo hi
  let E := {p ∈ D | ∀ i, 0 < f p (x i)}
  let C := {q : P × Window | q.1 ∈ E ∧ allows d fixed q.2 ∧ ∀ i, x i ∈ cells q.2}
  let L := fun q : P × Window ↦ likelihood x (f q.1) (cells q.2)
  let G := fun p ↦ L (p,T)
  (∀ p ∈ E, cells T = {lo} → IsMaxOn G E p) ∧
  (∀ q ∈ C, IsMaxOn L C q ↔ IsMaxOn G E q.1 ∧
    ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0) ∧
  ((∃ q ∈ C, IsMaxOn L C q) ↔ ∃ p ∈ E, IsMaxOn G E p) ∧
  (d ≠ .known → ∀ p ∈ E, IsMaxOn G E p →
    ∃ v, v ≠ T ∧ (p,T) ∈ C ∧ (p,v) ∈ C ∧ IsMaxOn L C (p,T) ∧ IsMaxOn L C (p,v))

private theorem estimated_claim {P : Type*} {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {f : P → ℕ → ℝ} {D : Set P}
    (hf : ∀ p ∈ D, ∀ k, 0 ≤ f p k) (hs : ∀ p ∈ D, Summable (f p))
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    EstimatedClaim x lo hi f D d fixed := by
  unfold EstimatedClaim
  intro T E C L G
  have ht := tight_cells hm hh hb hw hx
  have lift (p : P) (hp : p ∈ E) : (p,T) ∈ C := ⟨hp,ht.1,ht.2.1⟩
  have bound (q : P × Window) (hq : q ∈ C) :
      L q ≤ G q.1 ∧ (L q = G q.1 ↔ ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0) :=
    (TruncEstimated.likelihood_shrink hn (hf _ hq.1.1) (hs _ hq.1.1) hq.1.2
      (tight_cells hm hh hb hq.2.1 hq.2.2).2.2 ht.2.1).2
  have hwin (q : P × Window) (hq : q ∈ C) : IsMaxOn L C q ↔ IsMaxOn G E q.1 ∧
      ∀ k ∈ cells q.2 \ cells T, f q.1 k = 0 := by
    constructor
    · intro H
      have he := le_antisymm (bound q hq).1 (H (lift _ hq.1))
      exact ⟨fun p hp ↦ (H (lift p hp)).trans he.le, (bound q hq).2.mp he⟩
    · rintro ⟨H,hzero⟩ v hv
      exact (bound v hv).1.trans ((H hv.1).trans ((bound q hq).2.mpr hzero).ge)
  have tightWin (p : P) (hp : p ∈ E) (H : IsMaxOn G E p) : IsMaxOn L C (p,T) :=
    (hwin (p,T) (lift p hp)).mpr ⟨H,fun _ hk ↦ (hk.2 hk.1).elim⟩
  refine ⟨?_,hwin,?_,?_⟩
  · intro p hp hT v hv
    have hobs (i : Fin n) : x i = lo := by
      have hi := ht.2.1 i
      change x i ∈ cells T at hi
      simpa only [hT,mem_singleton_iff] using hi
    have he (q : P) (hq : q ∈ E) : G q = 1 := by
      change likelihood x (f q) (cells T) = 1
      rw [hT]
      exact TruncEstimated.likelihood_singleton hobs (by
        obtain ⟨i,hi⟩ := hm
        exact hi ▸ hq.2 i)
    change G v ≤ G p
    rw [he v hv,he p hp]
  · constructor
    · rintro ⟨q,hq,H⟩
      exact ⟨q.1,hq.1,((hwin q hq).mp H).1⟩
    · rintro ⟨p,hp,H⟩
      exact ⟨(p,T),lift p hp,tightWin p hp H⟩
  · intro hd p hp H
    obtain ⟨v,hv,hne,he⟩ := second_window hd fixed lo hi
    have hc : (p,v) ∈ C := ⟨hp,hv,fun i ↦ he ▸ ht.2.1 i⟩
    refine ⟨v,hne,lift p hp,hc,tightWin p hp H,(hwin (p,v) hc).mpr ⟨H,?_⟩⟩
    intro k hk
    exact (hk.2 (he ▸ hk.1)).elim

/-- CMB, any parent holds, `truncate:lower-estimated`,
`truncate:upper-estimated`, `truncate:both-estimated`, criterion and
uniqueness. The tight nominal cuts profile the likelihood, with exact
coefficient and cut ties and an attained compatible singleton. -/
theorem cmb_estimated_windows {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    (D : Set (ℕ × (Sum ℝ Bool × ℝ))) {d : Design} {fixed w : Window}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    EstimatedClaim x lo hi cmbWeight D d fixed :=
  estimated_claim hn hm hh hb (fun p _ ↦ cmb_nonneg p) (fun p _ ↦ cmb_sum p) hw hx

/-- CMP, any parent holds, `truncate:lower-estimated`,
`truncate:upper-estimated`, `truncate:both-estimated`, criterion and
uniqueness. The tight nominal cuts profile the likelihood, including
when the retained parent is the zero-rate atom. -/
theorem cmp_estimated_windows {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hm : ∃ i, x i = lo) (hh : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {D : Set (Option ℝ × ℝ)} (hD : D ⊆ cmpDomain) {d : Design} {fixed w : Window}
    (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w) :
    EstimatedClaim x lo hi cmpWeight D d fixed :=
  estimated_claim hn hm hh hb (fun p _ ↦ cmp_nonneg p) (fun _ hp ↦ cmp_sum (hD hp)) hw hx

end

end UnivariateML.Trunc5Estimated

end
