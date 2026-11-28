module

public import UnivariateML.Family.Trunc2Known

/-! Count windows with extended nominal cuts. Ordinary conditioning is
transported by retained cells, and the positive alias by exact nominal cuts. -/

@[expose] public section

open Set Real Filter
open scoped Topology

namespace UnivariateML.Trunc4Count

open Trunc2Known TruncCount
open TruncEstimated (Design likelihood)

noncomputable section

/-- Nominal cuts can independently omit the lower or upper bound. -/
abbrev Window := WithBot ℝ × WithTop ℝ

def cells (w : Window) : Set ℕ :=
  {k | (w.1 ≤ ((k : ℝ) : WithBot ℝ)) ∧ ((k : ℝ) : WithTop ℝ) ≤ w.2}

def allows (d : Design) (fixed w : Window) : Prop :=
  match d with
  | .lower => w.2 = fixed.2
  | .upper => w.1 = fixed.1
  | .both => True
  | .known => w = fixed

def tight (d : Design) (fixed : Window) (lo hi : ℕ) : Window :=
  match d with
  | .lower => ((lo : ℝ), fixed.2)
  | .upper => (fixed.1, (hi : ℝ))
  | .both => ((lo : ℝ), (hi : ℝ))
  | .known => fixed

def aliasWindow : Window := ((1 : ℝ), ⊤)

def reduceWindow (w : Window) : TruncEstimated.Window :=
  (w.1.recBotCoe 0 id, w.2)

def effectiveCut (s : RegularCount) (w : Window) : CountCut :=
  s.resolve (reduceWindow w)

private theorem reduce_cells (w : Window) : TruncEstimated.cells (reduceWindow w) = cells w := by
  rcases w with ⟨a,b⟩
  cases a using WithBot.recBotCoe <;>
    simp [reduceWindow, TruncEstimated.cells, cells, Nat.cast_nonneg]

private theorem reduce_tight (d : Design) (fixed : Window) (lo hi : ℕ) :
    reduceWindow (tight d fixed lo hi) =
      TruncEstimated.tight d (reduceWindow fixed) lo hi := by
  cases d <;> rfl

private theorem reduce_allows {d : Design} {fixed w : Window} (h : allows d fixed w) :
    TruncEstimated.allows d (reduceWindow fixed) (reduceWindow w) := by
  cases d
  · exact h
  · change (reduceWindow w).1 = (reduceWindow fixed).1
    exact congrArg (fun a : WithBot ℝ ↦ (reduceWindow (a, ⊤)).1) h
  · trivial
  · exact congrArg reduceWindow h

private theorem reduce_alias (w : Window) :
    reduceWindow w = positiveWindow ↔ w = aliasWindow := by
  rcases w with ⟨a,b⟩
  cases a using WithBot.recBotCoe
  · simp [reduceWindow, positiveWindow, aliasWindow]
  · simp [reduceWindow, positiveWindow, aliasWindow]

private theorem allows_alias (d : Design) (fixed : Window) :
    TruncEstimated.allows d (reduceWindow fixed) positiveWindow ↔ allows d fixed aliasWindow := by
  rcases fixed with ⟨a,b⟩
  cases d <;> cases a using WithBot.recBotCoe <;>
    simp [TruncEstimated.allows, allows, reduceWindow, positiveWindow, aliasWindow, Prod.mk.injEq, eq_comm]

private theorem lift_window {d : Design} {fixed : Window} {w : TruncEstimated.Window}
    (h : TruncEstimated.allows d (reduceWindow fixed) w) :
    ∃ v, allows d fixed v ∧ reduceWindow v = w := by
  cases d
  · exact ⟨((w.1 : WithBot ℝ),w.2),h,rfl⟩
  · exact ⟨(fixed.1,w.2),rfl,Prod.ext h.symm rfl⟩
  · exact ⟨((w.1 : WithBot ℝ),w.2),trivial,rfl⟩
  · exact ⟨fixed,rfl,h.symm⟩

-- Surjectivity is needed only on candidates. On the extended side the
-- reflection of candidate membership is needed only for allowed windows.
private theorem max_transport_on {P Q : Type*} {C : Set P} {D : Set Q}
    {F : P → ℝ} {G : Q → ℝ} {f : P → Q}
    (hC : MapsTo f C D) (hs : SurjOn f C D) (hF : ∀ p ∈ C, F p = G (f p))
    {p : P} (hp : p ∈ C) : IsMaxOn F C p ↔ IsMaxOn G D (f p) := by
  constructor
  · intro H q hq
    obtain ⟨v,hv,rfl⟩ := hs hq
    change G (f v) ≤ G (f p)
    rw [← hF v hv, ← hF p hp]
    exact H hv
  · intro H v hv
    change F v ≤ F p
    rw [hF v hv, hF p hp]
    exact H (hC hv)

private theorem valid_cut (s : RegularCount) (w : TruncEstimated.Window) {k : ℕ}
    (hk : k ∈ TruncEstimated.cells w)
    (hN : ∀ N, s = .binomial N → k ≤ N)
    (hr : ∀ r, s = .negBinomial r → 0 < r) : s.valid (s.resolve w) := by
  rcases w with ⟨a,b⟩
  have hL : ⌈a⌉₊ ≤ k := Nat.ceil_le.mpr hk.1
  cases b using WithTop.recTopCoe
  · cases s
    · trivial
    · trivial
    · exact ⟨hL.trans (hN _ rfl),le_rfl⟩
    · exact hr _ rfl
  · rename_i b
    have hu : (k : ℝ) ≤ b := WithTop.coe_le_coe.mp hk.2
    have hU : k ≤ ⌊b⌋₊ := Nat.le_floor hu
    cases s
    · exact hL.trans hU
    · exact hL.trans hU
    · exact ⟨hL.trans (le_min hU (hN _ rfl)), min_le_right ..⟩
    · exact ⟨hr _ rfl,hL.trans hU⟩

/-- The boundary cases, each with the law on the retained counts and
the limit of the public coefficient along the same tilt path. -/
def FaceLaws (s : RegularCount) (K : CountCut) (m : ℝ) : Prop :=
  let p := fun t : ℝ ↦ s.coefficient (.inl t)
  let lowerCoefficient := s.coefficient (.inr false)
  match K with
  | .ray l => m = l ∧ l ≠ 0 ∧ Tendsto p atBot (𝓝 lowerCoefficient) ∧
      ∀ j : ℕ,
        Tendsto (fun t : ℝ ↦ s.carrier (l+j) * exp t ^ j /
            (∑' k : ℕ, s.carrier (l+k) * exp t ^ k)) atBot
          (𝓝 (if j = 0 then 1 else 0))
  | .finite l u =>
    let c := s.carrier
    let M := fun j ↦ finiteCountMass l u c j
    l < u ∧
      ((m = l ∧ l ≠ 0 ∧ Tendsto p atBot (𝓝 lowerCoefficient) ∧
        ∀ j ∈ Finset.Icc l u, Tendsto (M j) atBot (𝓝 (if j = l then 1 else 0))) ∨
      match s with
      | .poisson => m = u ∧ Tendsto p atTop atTop ∧
          ∀ j ∈ Finset.Icc l u, Tendsto (M j) atTop (𝓝 (if j = u then 1 else 0))
      | .binomial N => m = u ∧ u ≠ N ∧ Tendsto p atTop (𝓝 1) ∧
          ∀ j ∈ Finset.Icc l u, Tendsto (M j) atTop (𝓝 (if j = u then 1 else 0))
      | _ => Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) c ≤ m ∧
          Tendsto p (𝓝[<] 0) (𝓝 0) ∧
          StrictMonoOn (finiteLogLik l u c m) (Iic 0) ∧
          (∀ j ∈ Finset.Icc l u,
            Tendsto (M j) (𝓝[<] 0) (𝓝 (c j / ∑ k ∈ Finset.Icc l u, c k))) ∧
          (s = .geometric ∨ s = .negBinomial 1 → ∀ j ∈ Finset.Icc l u,
            Tendsto (M j) (𝓝[<] 0) (𝓝 (((Finset.Icc l u).card : ℝ)⁻¹))))

private theorem lower_coefficient (s : RegularCount) :
    Tendsto (fun t ↦ s.coefficient (.inl t)) atBot (𝓝 (s.coefficient (.inr false))) := by
  have h := tendsto_const_nhds (x := (1 : ℝ)) (f := (atBot : Filter ℝ))
  cases s
  · exact tendsto_exp_atBot
  · simpa [RegularCount.coefficient,negativeProb] using h.sub tendsto_exp_atBot
  · have H := tendsto_exp_atBot.div (h.add tendsto_exp_atBot) (by norm_num)
    have H' : Tendsto (fun t : ℝ ↦ exp t / (1 + exp t)) atBot (𝓝 ((0 : ℝ) / (1 + 0))) :=
      H.congr (fun _ ↦ rfl)
    simpa [RegularCount.coefficient,binomProb] using H'
  · simpa [RegularCount.coefficient,negativeProb] using h.sub tendsto_exp_atBot

private theorem binomial_upper_coefficient :
    Tendsto (fun t ↦ binomProb (.inl t)) atTop (𝓝 1) := by
  have htop : Tendsto (fun t : ℝ ↦ 1 + exp t) atTop atTop := by
    refine tendsto_atTop.mpr (fun a ↦ ?_)
    filter_upwards [tendsto_atTop.mp tendsto_exp_atTop (a-1)] with t ht
    linarith
  have h := (tendsto_const_nhds (x := (1 : ℝ)) (f := (atTop : Filter ℝ))).sub
    (tendsto_inv_atTop_zero.comp htop)
  have H : Tendsto (fun t : ℝ ↦ 1 - (1 + exp t)⁻¹) atTop (𝓝 1) := by
    simpa using h
  apply H.congr
  intro t
  change 1 - (1 + exp t)⁻¹ = exp t / (1 + exp t)
  field_simp
  ring

private theorem negative_upper_coefficient :
    Tendsto (fun t ↦ negativeProb (.inl t)) (𝓝[<] 0) (𝓝 0) := by
  have h : Tendsto exp (𝓝[<] 0) (𝓝 1) := by
    simpa using (continuous_exp.continuousAt (x := (0 : ℝ))).tendsto.mono_left nhdsWithin_le_nhds
  simpa [negativeProb] using (tendsto_const_nhds (x := (1 : ℝ))).sub h

private theorem size_one_carrier (k : ℕ) : negBinomCarrier 1 k = 1 := by
  rw [negBinomCarrier, show (1 : ℝ) + k - 1 = k by ring, Ring.choose_natCast, Nat.choose_self]
  norm_num

private theorem face_laws (s : RegularCount) (K : CountCut) (m : ℝ)
    (hv : s.valid K) (h : s.escapes K m) : FaceLaws s K m := by
  cases K with
  | ray l =>
    exact ⟨h.1,h.2,lower_coefficient s,fun j ↦ regular_ray_face s l j hv⟩
  | finite l u =>
    have hlt : l < u := h.1
    have H (j : ℕ) (hj : j ∈ Finset.Icc l u) := regular_finite_faces s hlt hv hj m
    refine ⟨hlt,?_⟩
    rcases h.2 with ⟨hm,hne⟩ | hu
    · exact Or.inl ⟨hm,hne,lower_coefficient s,fun j hj ↦ (H j hj).1⟩
    · refine Or.inr ?_
      have hl := Finset.mem_Icc.mpr ⟨le_rfl,hlt.le⟩
      cases s with
      | poisson => exact ⟨hu,tendsto_exp_atTop,fun j hj ↦ (H j hj).2⟩
      | binomial N => exact ⟨hu.1,hu.2,binomial_upper_coefficient,fun j hj ↦ (H j hj).2⟩
      | geometric =>
        refine ⟨hu,negative_upper_coefficient,(H l hl).2.2 hu,fun j hj ↦ (H j hj).2.1,?_⟩
        intro _ j hj
        simpa [RegularCount.carrier, one_div] using (H j hj).2.1
      | negBinomial r =>
        refine ⟨hu,negative_upper_coefficient,(H l hl).2.2 hu,fun j hj ↦ (H j hj).2.1,?_⟩
        intro he j hj
        rcases he with h | h
        · cases h
        · have eq := RegularCount.negBinomial.inj h
          subst r
          simpa [RegularCount.carrier, size_one_carrier, one_div] using (H j hj).2.1

def ordinary {n : ℕ} (x : Fin n → ℕ) (s : RegularCount)
    (d : Design) (fixed : Window) : Set (Sum ℝ Bool × Window) :=
  {q | q.1 ∈ s.parent ∧ allows d fixed q.2 ∧
    ∀ i, x i ∈ cells q.2 ∧ 0 < s.weights q.1 (x i)}

def atomEligible {n : ℕ} (x : Fin n → ℕ) (d : Design) (fixed : Window) : Prop :=
  allows d fixed aliasWindow ∧ ∀ i, x i = 1

def atomPair : Sum ℝ Bool × Window := (.inr false, aliasWindow)

def atomCandidates {n : ℕ} (x : Fin n → ℕ) (d : Design) (fixed : Window) :
    Set (Sum ℝ Bool × Window) :=
  {q | atomEligible x d fixed ∧ q = atomPair}

def countObjective {n : ℕ} (x : Fin n → ℕ) (s : RegularCount)
    (q : Sum ℝ Bool × Window) : ℝ :=
  if q = atomPair then 1 else likelihood x (s.weights q.1) (cells q.2)

def publish (s : RegularCount) (q : Sum ℝ Bool × Window) : ℝ × Window :=
  (s.coefficient q.1, q.2)

def countMaxima {n : ℕ} (x : Fin n → ℕ) (s : RegularCount)
    (d : Design) (fixed : Window) : Set (ℝ × Window) :=
  let C := ordinary x s d fixed ∪ atomCandidates x d fixed
  publish s '' {q ∈ C | IsMaxOn (countObjective x s) C q}

private theorem atom_excludes_ordinary {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ}
    {s : RegularCount} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed : Window} (he : atomEligible x d fixed)
    (hN : ∀ N, s = .binomial N → hi ≤ N) :
    let K := effectiveCut s (tight d fixed lo hi)
    K.nonsingleton → ¬ s.attains K m := by
  obtain ⟨i,hi₀⟩ := hlo
  obtain ⟨j,hj⟩ := hhi
  have hlo : lo = 1 := hi₀.symm.trans (he.2 i)
  have hhi : hi = 1 := hj.symm.trans (he.2 j)
  simp only [hlo,hhi] at hN ⊢
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hm' : (n : ℝ) = n * m := by simp [he.2] at hm; exact hm
  have hm₁ : m = 1 := mul_left_cancel₀ hn'.ne' (by simpa only [mul_one] using hm'.symm)
  subst m
  have hc : tight d fixed 1 1 = aliasWindow ∨
      tight d fixed 1 1 = (((1 : ℝ) : WithBot ℝ), ((1 : ℝ) : WithTop ℝ)) := by
    have ha := he.1
    cases d
    · exact Or.inl (Prod.ext (by simp [tight, aliasWindow]) (ha.symm))
    · exact Or.inr (Prod.ext (ha.symm) (by simp [tight]))
    · exact Or.inr (by norm_num [tight])
    · exact Or.inl ha.symm
  rcases hc with hc | hc
  · rw [hc]
    unfold effectiveCut
    rw [show reduceWindow aliasWindow = ((1 : ℝ), ⊤) by rfl]
    cases s with
    | binomial N =>
      have hcut : (RegularCount.binomial N).resolve ((1 : ℝ), ⊤) = .finite 1 N := by
        simp [RegularCount.resolve]
      rw [hcut]
      intro h
      change 1 < N at h
      have hN' : (1 : ℝ) < N := by exact_mod_cast h
      simp [RegularCount.attains, mem_Ioo, h.ne, hN'.ne]
    | _ =>
      simp [RegularCount.resolve, RegularCount.attains, CountCut.nonsingleton]
  · rw [hc]
    unfold effectiveCut
    rw [show reduceWindow (((1 : ℝ) : WithBot ℝ), ((1 : ℝ) : WithTop ℝ)) =
      ((1 : ℝ), ((1 : ℝ) : WithTop ℝ)) by rfl]
    cases s with
    | binomial N =>
      intro h
      change ⌈(1 : ℝ)⌉₊ < min ⌊(1 : ℝ)⌋₊ N at h
      simp [min_eq_left (hN N rfl)] at h
    | _ =>
      intro h
      change ⌈(1 : ℝ)⌉₊ < ⌊(1 : ℝ)⌋₊ at h
      simp at h

/-- The exact public maximizing pairs, existence, profiled coefficient
cardinality and excluded laws for a regular count window. -/
def RegularClaim {n : ℕ} (x : Fin n → ℕ) (s : RegularCount) (lo hi : ℕ)
    (m : ℝ) (d : Design) (fixed : Window) : Prop :=
  let T := tight d fixed lo hi
  let K := effectiveCut s T
  let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
  let G := fun p ↦ likelihood x (s.weights p) K.kept
  let eligible := atomEligible x d fixed
  let M := countMaxima x s d fixed
  BijOn s.coefficient s.parent s.coefficientDomain ∧
    (M.Nonempty ↔ s.attains K m ∨ eligible) ∧
    (∀ q, q ∈ M ↔
      (eligible ∧ q = publish s atomPair) ∨
      ∃ p, q.1 = s.coefficient p ∧ (p,q.2) ∈ ordinary x s d fixed ∧
        IsMaxOn G E p ∧ (∀ k ∈ cells q.2 \ cells T, s.weights p k = 0) ∧
        (eligible → G p = 1)) ∧
    (K.nonsingleton → ((∃! p, p ∈ E ∧ IsMaxOn G E p) ↔ s.attains K m)) ∧
    (¬ K.nonsingleton → ∀ p ∈ E, G p = 1) ∧
    (M = ∅ ↔ s.escapes K m ∧ ¬ eligible) ∧
    (M = ∅ → FaceLaws s K m) ∧
    (s = .geometric → ∀ l u, K = .finite l u →
      Truncation.carrierMean (Finset.Icc l u) (fun k ↦ (k : ℝ)) s.carrier =
        ((l : ℝ) + u) / 2) ∧
    (K.nonsingleton → (Prod.fst '' M).Subsingleton)

private theorem regular_claim (s : RegularCount)
    (hr : ∀ r, s = .negBinomial r → 0 < r)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed w : Window} (hw : allows d fixed w) (hx : ∀ i, x i ∈ cells w)
    (hN : ∀ N, s = .binomial N → hi ≤ N) :
    RegularClaim x s lo hi m d fixed := by
  classical
  let T := tight d fixed lo hi
  let R := TruncEstimated.tight d (reduceWindow fixed) lo hi
  let K := effectiveCut s T
  have hTR : reduceWindow T = R := reduce_tight d fixed lo hi
  have hK : K = s.resolve R := congrArg s.resolve hTR
  let E := {p ∈ s.parent | ∀ i, 0 < s.weights p (x i)}
  let G := fun p ↦ likelihood x (s.weights p) K.kept
  let B := ordinary x s d fixed
  let A := atomCandidates x d fixed
  let C := B ∪ A
  let L := countObjective x s
  let f := fun q : Sum ℝ Bool × Window ↦ (q.1, reduceWindow q.2)
  let B' := TruncEstimated.candidates x s.weights s.parent d (reduceWindow fixed)
  let a' : Sum ℝ Bool × TruncEstimated.Window := (.inr false, positiveWindow)
  let A' := {q | (TruncEstimated.allows d (reduceWindow fixed) positiveWindow ∧ ∀ i, x i = 1) ∧ q = a'}
  let C' := B' ∪ A'
  let L' := fun q ↦ if q = a' then 1 else likelihood x (s.weights q.1) (TruncEstimated.cells q.2)
  have hAlias (q) : f q = a' ↔ q = atomPair := by
    rcases q with ⟨p,v⟩
    change (p,reduceWindow v) = (.inr false,positiveWindow) ↔
      (p,v) = (.inr false,aliasWindow)
    simp only [Prod.mk.injEq, reduce_alias]
  have hA (q) : f q ∈ A' ↔ q ∈ A := by
    change (_ ∧ f q = a') ↔ (_ ∧ q = atomPair)
    rw [hAlias, allows_alias]
    rfl
  have hB (q : Sum ℝ Bool × Window) (h : allows d fixed q.2) :
      f q ∈ B' ↔ q ∈ B := by
    change (_ ∧ _ ∧ ∀ i, x i ∈ TruncEstimated.cells (reduceWindow q.2) ∧ _) ↔
      (_ ∧ _ ∧ ∀ i, x i ∈ cells q.2 ∧ _)
    exact and_congr_right (fun _ ↦
      and_congr (iff_of_true (reduce_allows h) h) (by rw [reduce_cells]))
  have hmaps : MapsTo f C C' := by
    rintro q (h | h)
    · exact Or.inl ((hB q h.2.1).mpr h)
    · exact Or.inr ((hA q).mpr h)
  have hsurj : SurjOn f C C' := by
    rintro q (h | h)
    · obtain ⟨v,hv,he⟩ := lift_window h.2.1
      have hf : f (q.1,v) = q := Prod.ext rfl he
      exact ⟨(q.1,v),Or.inl ((hB _ hv).mp (hf ▸ h)),hf⟩
    · have hf : f atomPair = q := h.2.symm
      exact ⟨atomPair,Or.inr ((hA _).mp (hf ▸ h)),hf⟩
  have hL (q) : L q = L' (f q) := by
    change (if q = atomPair then 1 else likelihood x _ (cells q.2)) =
      (if f q = a' then 1 else likelihood x _ (TruncEstimated.cells (reduceWindow q.2)))
    simp only [hAlias, reduce_cells, f]
  let eligible' := TruncEstimated.allows d (reduceWindow fixed) positiveWindow ∧ ∀ i, x i = 1
  have hel : eligible' ↔ atomEligible x d fixed := and_congr (allows_alias d fixed) Iff.rfl
  have H := regular_window_fit s hr hn hlo hhi hb hm (reduce_allows hw)
    (fun i ↦ reduce_cells w ▸ hx i) hN
  change ((∃ q ∈ C', IsMaxOn L' C' q) ↔ s.attains (s.resolve R) m ∨ eligible') ∧
    (∀ q, (q ∈ C' ∧ IsMaxOn L' C' q) ↔
      q ∈ A' ∨ (q ∈ B' ∧ IsMaxOn (fun p ↦ likelihood x (s.weights p) (s.resolve R).kept) E q.1 ∧
        (∀ k ∈ TruncEstimated.cells q.2 \ TruncEstimated.cells R, s.weights q.1 k = 0) ∧
        (eligible' → (fun p ↦ likelihood x (s.weights p) (s.resolve R).kept) q.1 = 1))) ∧
    ((s.resolve R).nonsingleton → _) ∧ (¬ (s.resolve R).nonsingleton → _) ∧
    ((¬ ∃ q ∈ C', IsMaxOn L' C' q) ↔ s.escapes (s.resolve R) m ∧ ¬ eligible') at H
  rw [← hK, ← hTR, reduce_cells, hel] at H
  have hT := (TruncEstimated.tight_cells hlo hhi hb (reduce_allows hw)
    (fun i ↦ reduce_cells w ▸ hx i)).2.1
  have hv : s.valid K := hK ▸ valid_cut s R (hT ⟨0,hn⟩)
    (fun N hs ↦ (hb _).2.trans (hN N hs)) hr
  have hex : (countMaxima x s d fixed).Nonempty ↔ ∃ q ∈ C', IsMaxOn L' C' q := by
    change (publish s '' {q ∈ C | IsMaxOn L C q}).Nonempty ↔ _
    constructor
    · rintro ⟨_,q,⟨hq,hm⟩,rfl⟩
      exact ⟨f q, hmaps hq, (max_transport_on hmaps hsurj (fun p _ ↦ hL p) hq).mp hm⟩
    · rintro ⟨q,hq,hm⟩
      obtain ⟨p,hp,rfl⟩ := hsurj hq
      exact ⟨_,p,⟨hp,(max_transport_on hmaps hsurj (fun r _ ↦ hL r) hp).mpr hm⟩,rfl⟩
  have hfailure : countMaxima x s d fixed = ∅ ↔ s.escapes K m ∧ ¬ atomEligible x d fixed :=
    not_nonempty_iff_eq_empty.symm.trans ((not_congr hex).trans H.2.2.2.2)
  have hset : ∀ q, q ∈ countMaxima x s d fixed ↔
      (atomEligible x d fixed ∧ q = publish s atomPair) ∨
      ∃ p, q.1 = s.coefficient p ∧ (p,q.2) ∈ B ∧ IsMaxOn G E p ∧
        (∀ k ∈ cells q.2 \ cells T, s.weights p k = 0) ∧
        (atomEligible x d fixed → G p = 1) := by
    intro q
    change q ∈ publish s '' {p ∈ C | IsMaxOn L C p} ↔ _
    constructor
    · rintro ⟨p,⟨hp,hm⟩,rfl⟩
      have hp' := hmaps hp
      have hm' := (max_transport_on hmaps hsurj (fun r _ ↦ hL r) hp).mp hm
      rcases (H.2.1 (f p)).mp ⟨hp',hm'⟩ with ha | ⟨hb',hg,hz,he⟩
      · obtain ⟨h,eq⟩ := (hA p).mp ha
        exact Or.inl ⟨h,congrArg (publish s) eq⟩
      · have haw : allows d fixed p.2 := by
          rcases hp with hp | ⟨he,hpa⟩
          · exact hp.2.1
          · exact hpa.symm ▸ he.1
        exact Or.inr ⟨p.1,rfl,(hB _ haw).mp hb',hg,by simpa only [f,reduce_cells,publish,T] using hz,he⟩
    · rintro (⟨ha,rfl⟩ | ⟨p,hp,hBq,hg,hz,he⟩)
      · have hq : atomPair ∈ A := ⟨ha,rfl⟩
        have hm := (H.2.1 _).mpr (Or.inl ((hA _).mpr hq))
        exact ⟨atomPair,⟨Or.inr hq,
          (max_transport_on hmaps hsurj (fun r _ ↦ hL r) (Or.inr hq)).mpr hm.2⟩,rfl⟩
      · let v := (p,q.2)
        have hq : v ∈ C := Or.inl hBq
        have hB' := (hB v hBq.2.1).mpr hBq
        have hm := (H.2.1 (f v)).mpr (Or.inr ⟨hB',hg,
          by simpa only [f,v,reduce_cells] using hz,he⟩)
        exact ⟨v,⟨hq,(max_transport_on hmaps hsurj (fun r _ ↦ hL r) hq).mpr hm.2⟩,
          Prod.ext hp.symm rfl⟩
  refine ⟨regular_coefficient_chart s,hex.trans H.1,hset,H.2.2.1,H.2.2.2.1,hfailure,
    fun h ↦ face_laws s K m hv ((hfailure.mp h).1),?_,?_⟩
  · intro hs l u hk
    have hv' : s.valid (.finite l u) := hk ▸ hv
    subst s
    exact geom_carrier_midpoint hv'
  · intro hNS
    have Huniq := H.2.2.1 hNS
    have hOnly {p} (hp : p ∈ E) (hpm : IsMaxOn G E p) :
        s.attains K m := by
      have hsample (i : Fin n) : x i ∈ K.kept := by
        have hR := hT i
        change x i ∈ TruncEstimated.cells R at hR
        -- The effective interval clips only at the held binomial size.
        rw [hK]
        rcases R with ⟨a,b⟩
        have hl : ⌈a⌉₊ ≤ x i := Nat.ceil_le.mpr hR.1
        cases b using WithTop.recTopCoe
        · cases s
          · exact hl
          · exact hl
          · exact ⟨hl,(hb i).2.trans (hN _ rfl)⟩
          · exact hl
        · have hu := Nat.le_floor (WithTop.coe_le_coe.mp hR.2)
          cases s
          · exact ⟨hl,hu⟩
          · exact ⟨hl,hu⟩
          · exact ⟨hl,le_min hu ((hb i).2.trans (hN _ rfl))⟩
          · exact ⟨hl,hu⟩
      exact (regular_known s K hv hn hsample hm).1.mp ⟨p,hp,hpm⟩
    have hCoeff (q) (hq : q ∈ countMaxima x s d fixed) :
        (atomEligible x d fixed ∧ q.1 = s.coefficient atomPair.1) ∨
        ∃ p ∈ E, IsMaxOn G E p ∧ q.1 = s.coefficient p := by
      rcases (hset q).mp hq with ⟨he,rfl⟩ | ⟨p,hp,hb,hg,_,_⟩
      · exact Or.inl ⟨he,rfl⟩
      · exact Or.inr ⟨p,⟨hb.1,fun i ↦ (hb.2.2 i).2⟩,hg,hp⟩
    rintro _ ⟨q,hq,rfl⟩ _ ⟨v,hv',rfl⟩
    rcases hCoeff q hq with ⟨ha,hq⟩ | ⟨p,hp,hpm,hq⟩ <;>
      rcases hCoeff v hv' with ⟨hb,hv⟩ | ⟨t,ht,htm,hv⟩
    · exact hq.trans hv.symm
    · exact (atom_excludes_ordinary hn hlo hhi hm ha hN hNS (hOnly ht htm)).elim
    · exact (atom_excludes_ordinary hn hlo hhi hm hb hN hNS (hOnly hp hpm)).elim
    · obtain ⟨a,_,hu⟩ := Huniq.mpr (hOnly hp hpm)
      exact hq.trans ((congrArg s.coefficient ((hu p ⟨hp,hpm⟩).trans
        (hu t ⟨ht,htm⟩).symm)).trans hv.symm)

def zeroNBChart : Set (Option ℝ) := closedLowerDomain (Iio 0)

def zeroNBProb : Option ℝ → ℝ
  | none => 1
  | some t => 1 - exp t

private theorem zero_prob_chart : BijOn zeroNBProb zeroNBChart (Ioc 0 1) := by
  refine ⟨?_,?_,?_⟩
  · rintro (_ | t) ht
    · exact ⟨zero_lt_one,le_rfl⟩
    · have h : t < 0 := ht t rfl
      exact ⟨sub_pos.mpr (exp_lt_one_iff.mpr h), sub_le_self _ (exp_pos _).le⟩
  · rintro (_ | t) _ (_ | u) _ h
    · rfl
    · change (1 : ℝ) = 1 - exp u at h
      linarith [exp_pos u]
    · change (1 : ℝ) - exp t = 1 at h
      linarith [exp_pos t]
    · change 1 - exp t = 1 - exp u at h
      exact congrArg some (exp_injective (by linarith))
  · intro p hp
    by_cases he : p = 1
    · exact ⟨none, by simp [zeroNBChart,closedLowerDomain], he.symm⟩
    · have hq : 0 < 1 - p := sub_pos.mpr (lt_of_le_of_ne hp.2 he)
      refine ⟨some (log (1-p)), ?_, ?_⟩
      · change ∀ t, some (log (1-p)) = some t → t ∈ Iio 0
        intro t ht
        cases ht
        exact log_neg hq (by linarith [hp.1])
      · change 1 - exp (log (1-p)) = p
        rw [exp_log hq, sub_sub_cancel]

def zeroOrdinary {n : ℕ} (x : Fin n → ℕ) (d : Design) (fixed : Window) :
    Set (Option ℝ × Window) :=
  {q | q.1 ∈ zeroNBChart ∧ allows d fixed q.2 ∧ 0 ∈ cells q.2 ∧ ∀ i, x i = 0}

def zeroAlias {n : ℕ} (x : Fin n → ℕ) (d : Design) (fixed : Window) :
    Set (Option ℝ × Window) :=
  {q | q.1 ∈ zeroNBChart ∧ allows d fixed aliasWindow ∧ q.2 = aliasWindow ∧ ∀ i, 1 ≤ x i}

def zeroNBObjective (m : ℝ) (q : Option ℝ × Window) : ℝ :=
  if q.2 = aliasWindow then closedLowerLikelihood logSeriesCarrier (m-1) q.1 else 1

def zeroNBMaxima {n : ℕ} (x : Fin n → ℕ) (m : ℝ) (d : Design) (fixed : Window) :
    Set (ℝ × Window) :=
  let C := zeroOrdinary x d fixed ∪ zeroAlias x d fixed
  (fun q : Option ℝ × Window ↦ (zeroNBProb q.1,q.2)) '' {q ∈ C | IsMaxOn (zeroNBObjective m) C q}

/-- The mean of the positive size-zero NB alias in the public probability. -/
def logarithmicMean (p : ℝ) : ℝ := (1-p) / (p * (-log p))

private theorem logarithmic_mean_anti : StrictAntiOn logarithmicMean (Ioo 0 1) := by
  have hD (p : ℝ) (hp : p ∈ Ioo 0 1) :
      HasDerivAt logarithmicMean ((log p + 1 - p) / (p * (-log p)) ^ 2) p := by
    have hp0 := hp.1.ne'
    have hden : p * (-log p) ≠ 0 := (mul_pos hp.1 (neg_pos.mpr (log_neg hp.1 hp.2))).ne'
    have h := ((hasDerivAt_id p).const_sub 1).div
      ((hasDerivAt_id p).mul (hasDerivAt_log hp0).neg) hden
    convert h using 1
    · rfl
    change (log p + 1 - p) / (p * -log p) ^ 2 =
      (-(1 : ℝ) * (p * -log p) - (1-p) * (1 * -log p + p * -p⁻¹)) /
        (p * -log p) ^ 2
    apply congrArg (fun a : ℝ ↦ a / (p * -log p) ^ 2)
    field_simp
    ring
  apply strictAntiOn_of_deriv_neg (convex_Ioo 0 1)
  · exact fun p hp ↦ (hD p hp).continuousAt.continuousWithinAt
  · intro p hp
    rw [isOpen_Ioo.interior_eq] at hp
    rw [(hD p hp).deriv]
    exact div_neg_of_neg_of_pos (by linarith [log_lt_sub_one_of_pos hp.1 hp.2.ne])
      (sq_pos_of_ne_zero (mul_ne_zero hp.1.ne' (neg_ne_zero.mpr (log_neg hp.1 hp.2).ne)))

/-- At size zero the ordinary parent is a zero atom. Its whole probability
domain is flat. The only positive-data branch is the exact logarithmic alias. -/
def ZeroNBClaim {n : ℕ} (x : Fin n → ℕ) (m : ℝ) (d : Design) (fixed : Window) : Prop :=
  let M := zeroNBMaxima x m d fixed
  let Z := ∀ i, x i = 0
  let P := ∀ i, 1 ≤ x i
  let W := {w | allows d fixed w ∧ 0 ∈ cells w}
  ((M.Nonempty) ↔ Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow) ∧
  (Z → M = Ioc 0 1 ×ˢ W) ∧
  (P ∧ allows d fixed aliasWindow → ∃ p ∈ Ioc 0 1,
    M = {(p,aliasWindow)} ∧ (p = 1 ↔ m = 1) ∧
      (p < 1 → m = logarithmicMean p ∧ ∀ q ∈ Ioo 0 1, (m = logarithmicMean q ↔ q = p))) ∧
  (¬ (Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow) →
    zeroOrdinary x d fixed ∪ zeroAlias x d fixed = ∅)

private theorem zero_nb_claim {n : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) (d : Design) (fixed : Window) :
    ZeroNBClaim x m d fixed := by
  classical
  let M := zeroNBMaxima x m d fixed
  let Z := ∀ i, x i = 0
  let P := ∀ i, 1 ≤ x i
  let W := {w | allows d fixed w ∧ 0 ∈ cells w}
  let B := zeroOrdinary x d fixed
  let A := zeroAlias x d fixed
  let C := B ∪ A
  let f := fun q : Option ℝ × Window ↦ (zeroNBProb q.1,q.2)
  let F := closedLowerLikelihood logSeriesCarrier (m-1)
  change (M.Nonempty ↔ Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow) ∧
    (Z → M = Ioc 0 1 ×ˢ W) ∧ _ ∧ (_ → C = ∅)
  have hdisj (hz : Z) (hp : P) : False := by
    have H := hp ⟨0,hn⟩
    rw [hz ⟨0,hn⟩] at H
    omega
  have he (h : ¬ (Z ∧ W.Nonempty ∨ P ∧ allows d fixed aliasWindow)) : C = ∅ := by
    apply eq_empty_iff_forall_notMem.mpr
    rintro q (hq | hq)
    · exact h (Or.inl ⟨hq.2.2.2,q.2,hq.2.1,hq.2.2.1⟩)
    · exact h (Or.inr ⟨hq.2.2.2,hq.2.1⟩)
  have hOrd (hz : Z) : M = Ioc 0 1 ×ˢ W := by
    have hC : ∀ q, q ∈ C ↔ q.1 ∈ zeroNBChart ∧ q.2 ∈ W := by
      intro q
      constructor
      · rintro (hq | hq)
        · exact ⟨hq.1,hq.2.1,hq.2.2.1⟩
        · exact (hdisj hz hq.2.2.2).elim
      · rintro ⟨h₁,h₂,h₃⟩
        exact Or.inl ⟨h₁,h₂,h₃,hz⟩
    have hL (q) (hq : q ∈ C) : zeroNBObjective m q = 1 := by
      have hneq : q.2 ≠ aliasWindow := by
        intro hh
        have h0 := ((hC q).mp hq).2.2
        rw [hh] at h0
        have h : (1 : ℝ) ≤ (0 : ℕ) := WithBot.coe_le_coe.mp h0.1
        rw [Nat.cast_zero] at h
        exact (not_le_of_gt zero_lt_one) h
      simp [zeroNBObjective,hneq]
    ext q
    change q ∈ f '' {v ∈ C | IsMaxOn (zeroNBObjective m) C v} ↔ _
    constructor
    · rintro ⟨v,⟨hv,_⟩,rfl⟩
      obtain ⟨h₁,h₂⟩ := (hC v).mp hv
      exact ⟨zero_prob_chart.1 h₁,h₂⟩
    · rintro ⟨hp,hw⟩
      obtain ⟨a,ha,he⟩ := zero_prob_chart.2.2 hp
      have hq : (a,q.2) ∈ C := (hC _).mpr ⟨ha,hw⟩
      refine ⟨(a,q.2),⟨hq,?_⟩,Prod.ext he rfl⟩
      intro u hu
      change zeroNBObjective m u ≤ zeroNBObjective m (a,q.2)
      rw [hL u hu,hL _ hq]
  have hAlias (hp : P) (hd : allows d fixed aliasWindow) :
      ∃ p ∈ Ioc 0 1, M = {(p,aliasWindow)} ∧ (p = 1 ↔ m = 1) ∧
        (p < 1 → m = logarithmicMean p ∧ ∀ q ∈ Ioo 0 1, (m = logarithmicMean q ↔ q = p)) := by
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    have hmean : 1 ≤ m := by
      have H := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦
        show (1 : ℝ) ≤ x i by exact_mod_cast hp i)
      simp only [Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul,mul_one,hm] at H
      exact (le_mul_iff_one_le_right hn').mp H
    have hC : ∀ q, q ∈ C ↔ q.1 ∈ zeroNBChart ∧ q.2 = aliasWindow := by
      intro q
      constructor
      · rintro (hq | hq)
        · exact (hdisj hq.2.2.2 hp).elim
        · exact ⟨hq.1,hq.2.2.1⟩
      · rintro ⟨h₁,h₂⟩
        exact Or.inr ⟨h₁,hd,h₂,hp⟩
    have hL (q) (hq : q ∈ C) : zeroNBObjective m q = F q.1 := by
      simp [zeroNBObjective,((hC q).mp hq).2,F]
    have hmapped : MapsTo Prod.fst C zeroNBChart := fun _ h ↦ ((hC _).mp h).1
    have hsurj : SurjOn Prod.fst C zeroNBChart := fun a ha ↦
      ⟨(a,aliasWindow),(hC _).mpr ⟨ha,rfl⟩,rfl⟩
    obtain ⟨b,⟨hb,hbm⟩,hu⟩ := nbinom_zero_alias hmean
    refine ⟨zeroNBProb b,zero_prob_chart.1 hb,?_,?_,?_⟩
    · ext q
      change q ∈ f '' {v ∈ C | IsMaxOn (zeroNBObjective m) C v} ↔ q = (zeroNBProb b,aliasWindow)
      constructor
      · rintro ⟨v,⟨hv,hvm⟩,rfl⟩
        have h := (max_transport_on hmapped hsurj hL hv).mp hvm
        exact Prod.ext (congrArg zeroNBProb (hu v.1 ⟨hmapped hv,h⟩)) ((hC v).mp hv).2
      · rintro rfl
        have hq : (b,aliasWindow) ∈ C := (hC _).mpr ⟨hb,rfl⟩
        exact ⟨(b,aliasWindow),⟨hq,(max_transport_on hmapped hsurj hL hq).mpr hbm⟩,rfl⟩
    · constructor
      · intro h
        have hnone : (none : Option ℝ) ∈ zeroNBChart := by simp [zeroNBChart,closedLowerDomain]
        have H : b = none := zero_prob_chart.2.1 hb hnone h
        exact (nbinom_zero_alias_atom hmean).mp (H ▸ hbm)
      · intro h
        have hnone : (none : Option ℝ) ∈ zeroNBChart := by simp [zeroNBChart,closedLowerDomain]
        rw [← hu none ⟨hnone,(nbinom_zero_alias_atom hmean).mpr h⟩]
        rfl
    · intro h
      have hmatch : m = logarithmicMean (zeroNBProb b) := by
        cases b with
        | none => exact (lt_irrefl _ h).elim
        | some t =>
          change m = (1 - (1 - exp t)) / ((1 - exp t) * -log (1 - exp t))
          rw [sub_sub_cancel]
          exact nbinom_zero_alias_match (hb t rfl) hbm
      have hp₀ : 0 < zeroNBProb b := (zero_prob_chart.1 hb).1
      exact ⟨hmatch, fun q hq ↦
        ⟨fun e ↦ logarithmic_mean_anti.injOn hq ⟨hp₀,h⟩ (e.symm.trans hmatch),
          fun e ↦ e.symm ▸ hmatch⟩⟩
  refine ⟨?_,hOrd,fun h ↦ hAlias h.1 h.2,he⟩
  constructor
  · rintro ⟨_,q,⟨hq,_⟩,_⟩
    rcases hq with hq | hq
    · exact Or.inl ⟨hq.2.2.2,q.2,hq.2.1,hq.2.2.1⟩
    · exact Or.inr ⟨hq.2.2.2,hq.2.1⟩
  · rintro (⟨hz,w,hw⟩ | ⟨hp,hd⟩)
    · rw [hOrd hz]
      exact ⟨(1,w),⟨⟨zero_lt_one,le_rfl⟩,hw⟩⟩
    · obtain ⟨p,_,hM,_⟩ := hAlias hp hd
      rw [hM]
      exact ⟨_,rfl⟩

/-- The held-size NB claim dispatches size zero to its separate likelihood. -/
def NBClaim {n : ℕ} (x : Fin n → ℕ) (r : ℝ) (lo hi : ℕ) (m : ℝ)
    (d : Design) (fixed : Window) : Prop :=
  if r = 0 then ZeroNBClaim x m d fixed
  else RegularClaim x (.negBinomial r) lo hi m d fixed

private theorem nb_claim {r : ℝ} (hr : 0 ≤ r)
    {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    {d : Design} {fixed : Window}
    (hc : r ≠ 0 → ∃ w, allows d fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m d fixed := by
  by_cases h : r = 0
  · simp only [NBClaim,h,ite_true]
    exact zero_nb_claim hn x hm d fixed
  · simp only [NBClaim,h,ite_false]
    obtain ⟨w,hw,hx⟩ := hc h
    refine regular_claim (.negBinomial r) ?_ hn hlo hhi hb hm hw hx ?_
    · intro u hu
      have he : r = u := RegularCount.negBinomial.inj hu
      exact he ▸ lt_of_le_of_ne hr (Ne.symm h)
    · intro N hN
      cases hN

private theorem both_cut (s : RegularCount) (lo hi : ℕ)
    (hN : ∀ N, s = .binomial N → hi ≤ N) :
    effectiveCut s (tight .both (⊥, ⊤) lo hi) = .finite lo hi := by
  change s.resolve ((lo : ℝ), ((hi : ℝ) : WithTop ℝ)) = _
  cases s with
  | binomial N =>
    change CountCut.finite ⌈(lo : ℝ)⌉₊ (min ⌊(hi : ℝ)⌋₊ N) = _
    simp [min_eq_left (hN N rfl)]
  | _ =>
    change CountCut.finite ⌈(lo : ℝ)⌉₊ ⌊(hi : ℝ)⌋₊ = _
    simp

private theorem extrema_mean {n : ℕ} (hn : 0 < n) {x : Fin n → ℕ} {lo hi : ℕ}
    (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi) (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi)
    {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    lo = hi ∨ m ∈ Ioo (lo : ℝ) (hi : ℝ) := by
  have hle := (hb ⟨0,hn⟩).1.trans (hb ⟨0,hn⟩).2
  rcases hle.eq_or_lt with h | h
  · exact Or.inl h
  · obtain ⟨i,hi₀⟩ := hlo
    obtain ⟨j,hj⟩ := hhi
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    have hlt : (lo : ℝ) < hi := by exact_mod_cast h
    have hr : ∑ k : Fin n, (lo : ℝ) < ∑ k, (x k : ℝ) :=
      Finset.sum_lt_sum (fun k _ ↦ by exact_mod_cast (hb k).1)
        ⟨j,Finset.mem_univ _, by rwa [hj]⟩
    have hs : ∑ k : Fin n, (x k : ℝ) < ∑ k : Fin n, (hi : ℝ) :=
      Finset.sum_lt_sum (fun k _ ↦ by exact_mod_cast (hb k).2)
        ⟨i,Finset.mem_univ _, by rwa [hi₀]⟩
    simp only [hm, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul] at hr hs
    exact Or.inr ⟨lt_of_mul_lt_mul_left hr hn'.le,lt_of_mul_lt_mul_left hs hn'.le⟩

private theorem both_alias_one {n lo hi : ℕ} {x : Fin n → ℕ} (hlo : ∃ i, x i = lo)
    (hhi : ∃ i, x i = hi) {d : Design} {w : Window} :
    atomEligible x d w → lo = hi := by
  rintro ⟨_, hx⟩
  obtain ⟨i,hi₀⟩ := hlo
  obtain ⟨j,hj⟩ := hhi
  exact hi₀.symm.trans ((hx i).trans ((hx j).symm.trans hj))

/-- The Poisson known-cut criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem pois_known {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .known fixed := by
  exact regular_claim .poisson (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The Poisson lower-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem pois_lower_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .lower fixed := by
  exact regular_claim .poisson (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The Poisson upper-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem pois_upper_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .poisson lo hi m .upper fixed := by
  exact regular_claim .poisson (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The Poisson both-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem pois_both_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    RegularClaim x .poisson lo hi m .both (⊥, ⊤) ∧
    (countMaxima x (.poisson) .both (⊥, ⊤)).Nonempty := by
  have H := regular_claim .poisson (fun _ h ↦ by cases h) hn hlo hhi hb hm
    (w := (⊥, ⊤)) (d := .both) (fixed := (⊥, ⊤)) trivial (fun _ ↦ ⟨bot_le,le_top⟩) (fun _ h ↦ by cases h)
  refine ⟨H, H.2.1.mpr (Or.inl ?_)⟩
  rw [both_cut _ _ _ (fun _ h ↦ by cases h)]
  rcases extrema_mean hn hlo hhi hb hm with h | h
  · exact Or.inl h
  · exact Or.inr (Or.inl h)

/-- The geometric known-cut criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem geom_known {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .known fixed := by
  exact regular_claim .geometric (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The geometric lower-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem geom_lower_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .lower fixed := by
  exact regular_claim .geometric (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The geometric upper-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem geom_upper_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x .geometric lo hi m .upper fixed := by
  exact regular_claim .geometric (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h)

/-- The geometric both-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem geom_both_estimated {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    RegularClaim x .geometric lo hi m .both (⊥, ⊤) ∧
    ((countMaxima x .geometric .both (⊥, ⊤)).Nonempty ↔ lo = hi ∨ m < ((lo : ℝ) + hi) / 2) := by
  have H := regular_claim .geometric (fun _ h ↦ by cases h) hn hlo hhi hb hm
    (w := (⊥, ⊤)) (d := .both) (fixed := (⊥, ⊤)) trivial (fun _ ↦ ⟨bot_le,le_top⟩) (fun _ h ↦ by cases h)
  refine ⟨H, H.2.1.trans ?_⟩
  rw [both_cut _ _ _ (fun _ h ↦ by cases h)]
  rcases extrema_mean hn hlo hhi hb hm with h | h
  · exact iff_of_true (Or.inl (Or.inl h)) (Or.inl h)
  · have hne : lo ≠ hi := by
      intro h'
      have H' : (lo : ℝ) = hi := by exact_mod_cast h'
      linarith [h.1,h.2]
    have hle : lo ≤ hi := (hb ⟨0,hn⟩).1.trans (hb ⟨0,hn⟩).2
    have hna : ¬ atomEligible x .both (⊥, ⊤) := fun he ↦ hne (both_alias_one hlo hhi he)
    have hlt : (lo : ℝ) < m := h.1
    simp [RegularCount.attains, RegularCount.carrier, geom_carrier_midpoint hle, hne, hna,
      hlt, hlt.ne']

/-- The held-size binomial known-cut criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem binom_known (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .known fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .known fixed := by
  exact regular_claim (.binomial N) (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h; exact hN)

/-- The held-size binomial lower-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem binom_lower_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .lower fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .lower fixed := by
  exact regular_claim (.binomial N) (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h; exact hN)

/-- The held-size binomial upper-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem binom_upper_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N)
    (fixed : Window) {w : Window}
    (hw : allows .upper fixed w) (hx : ∀ i, x i ∈ cells w) :
    RegularClaim x (.binomial N) lo hi m .upper fixed := by
  exact regular_claim (.binomial N) (fun _ h ↦ by cases h) hn hlo hhi hb hm
    hw hx (fun _ h ↦ by cases h; exact hN)

/-- The held-size binomial both-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem binom_both_estimated (N : ℕ) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (hN : hi ≤ N) :
    RegularClaim x (.binomial N) lo hi m .both (⊥, ⊤) ∧
    (countMaxima x (.binomial N) .both (⊥, ⊤)).Nonempty := by
  have H := regular_claim (.binomial N) (fun _ h ↦ by cases h) hn hlo hhi hb hm
    (w := (⊥, ⊤)) (d := .both) (fixed := (⊥, ⊤)) trivial (fun _ ↦ ⟨bot_le,le_top⟩) (fun _ h ↦ by cases h; exact hN)
  refine ⟨H, H.2.1.mpr (Or.inl ?_)⟩
  rw [both_cut _ _ _ (fun _ h ↦ by cases h; exact hN)]
  rcases extrema_mean hn hlo hhi hb hm with h | h
  · exact Or.inl h
  · exact Or.inr (Or.inl h)

/-- The held-size negative-binomial known-cut criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem nbinom_known {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .known fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .known fixed := by
  exact nb_claim hr hn hlo hhi hb hm hc

/-- The held-size negative-binomial lower-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem nbinom_lower_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .lower fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .lower fixed := by
  exact nb_claim hr hn hlo hhi hb hm hc

/-- The held-size negative-binomial upper-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem nbinom_upper_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m)
    (fixed : Window) (hc : r ≠ 0 → ∃ w, allows .upper fixed w ∧ ∀ i, x i ∈ cells w) :
    NBClaim x r lo hi m .upper fixed := by
  exact nb_claim hr hn hlo hhi hb hm hc

/-- The held-size negative-binomial both-estimated criterion, failure-law and uniqueness claims
for the public coefficient and all admissible nominal cuts. -/
theorem nbinom_both_estimated {r : ℝ} (hr : 0 ≤ r) {n : ℕ} (hn : 0 < n)
    {x : Fin n → ℕ} {lo hi : ℕ} (hlo : ∃ i, x i = lo) (hhi : ∃ i, x i = hi)
    (hb : ∀ i, lo ≤ x i ∧ x i ≤ hi) {m : ℝ} (hm : ∑ i, (x i : ℝ) = n * m) :
    NBClaim x r lo hi m .both (⊥, ⊤) := by
  exact nb_claim hr hn hlo hhi hb hm (fun _ ↦ ⟨(⊥, ⊤), trivial, fun _ ↦ ⟨bot_le,le_top⟩⟩)

end

end UnivariateML.Trunc4Count

end
