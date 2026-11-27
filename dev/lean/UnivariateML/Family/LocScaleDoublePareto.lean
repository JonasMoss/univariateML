module

public import UnivariateML.Family.HeldSweepDoublePareto

import Mathlib.Tactic

-- The repository license applies to this downstream module.
set_option linter.style.header false

/-!
# Double Pareto with one held rate

The log-transformed sample is used throughout.
-/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.LocScaleDoublePareto

open DoublePareto HeldSweepDoublePareto

variable {n : ℕ}

private theorem at_knot (z : Fin n → ℝ) (hn : 0 < n) {a b c : ℝ}
    (ha : 0 < a) (hb : 0 < b)
    (hc : IsMaxOn (fun d ↦ logLik z d a b) univ c) :
    ∃ i, c ≤ z i ∧ IsMaxOn (fun d ↦ logLik z d a b) univ (z i) ∧
      (∀ j, z j < z i → z j ≤ c) := by
  classical
  have hc' := (held_rates_maximizers z hn ha hb c).mp hc
  have hw : (Finset.univ.filter (fun i ↦ c ≤ z i)).Nonempty := by
    by_contra h
    have hz : ∀ i, z i < c := fun i ↦ lt_of_not_ge (fun hi ↦ h ⟨i,by simp [hi]⟩)
    have he : countBelow z c = n := by simp [countBelow,hz]
    rw [he] at hc'
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    nlinarith [hc'.1, mul_pos ha hN]
  obtain ⟨i,hi,hm⟩ := Finset.exists_min_image _ z hw
  have hci := (Finset.mem_filter.mp hi).2
  have hgap j (hj : z j < z i) : z j ≤ c := by
    by_contra! h
    exact hj.not_ge (hm j (by simp [h.le]))
  refine ⟨i,hci,(held_rates_maximizers z hn ha hb _).mpr ⟨?_,?_⟩,hgap⟩
  · rcases hci.eq_or_lt with he | he
    · simpa [← he] using hc'.1
    have hh : countBelow z (z i) = countBelow z c := by
      unfold countBelow
      congr 1
      ext j
      simp only [Finset.mem_filter,Finset.mem_univ,true_and]
      exact ⟨fun hj ↦ (hgap j hj).lt_of_ne (fun hj' ↦ by
        have := hm j (by simp [hj'])
        linarith),fun hj ↦ hj.trans he⟩
    simpa [hh] using hc'.1
  · have hh : countAtOrBelow z c ≤ countAtOrBelow z (z i) :=
      Finset.card_le_card (by
        intro j hj
        exact Finset.mem_filter.mpr ⟨Finset.mem_univ _,(Finset.mem_filter.mp hj).2.trans hci⟩)
    exact hc'.2.trans (mul_le_mul_of_nonneg_left (by exact_mod_cast hh) (add_pos ha hb).le)

private theorem right_zero (z : Fin n → ℝ) {c : ℝ} (hc : ∀ i, z i ≤ c) :
    right z c = 0 := by
  simp [right,mean,fun i ↦ max_eq_right (sub_nonpos.mpr (hc i))]

private theorem right_strict_on_span (z : Fin n → ℝ) (hn : 0 < n) {c d : ℝ}
    (hcd : c < d) {i : Fin n} (hi : d ≤ z i) : right z d < right z c := by
  unfold right mean
  apply (div_lt_div_iff_of_pos_right (show (0 : ℝ) < n by exact_mod_cast hn)).2
  refine Finset.sum_lt_sum (fun j _ ↦ max_le_max (sub_le_sub_left hcd.le _) le_rfl)
    ⟨i,Finset.mem_univ i,?_⟩
  rw [max_eq_left (sub_nonneg.mpr hi),max_eq_left (sub_nonneg.mpr (hcd.le.trans hi))]
  exact sub_lt_sub_left hcd _

private theorem rate_score {L R a b : ℝ} (ha : 0 < a) (hb : 0 < b)
    (hm : IsMaxOn (objective L R a) (Ioi 0) b) : a/(b*(a+b)) = R := by
  have haux : objective L R a =ᶠ[𝓝 b]
      (fun t ↦ log a + log t - log (a + t) - a * L - t * R) := by
    filter_upwards [Ioi_mem_nhds hb] with t ht
    unfold objective
    rw [log_div (mul_pos ha ht).ne' (add_pos ha ht).ne', log_mul ha.ne' ht.ne']
  have hd : HasDerivAt (objective L R a) (a/(b*(a+b))-R) b := by
    apply HasDerivAt.congr_of_eventuallyEq _ haux
    have h := (((((hasDerivAt_id b).log hb.ne').const_add (log a)).sub
      (((hasDerivAt_id b).const_add a).log (add_pos ha hb).ne')).sub_const (a * L)).sub
      ((hasDerivAt_id b).mul_const R)
    convert h using 1
    · ext t
      simp
    · simp only [id_eq,one_mul]
      field_simp [hb.ne', (add_pos ha hb).ne']
      ring
  exact sub_eq_zero.mp ((hm.isLocalMax (Ioi_mem_nhds hb)).hasDerivAt_eq_zero hd)

/-- For a held left rate, every joint winner is an observation knot with
positive right deviation. The conditional right rate is uniquely determined. -/
theorem held_left_winner_is_eligible {z : Fin n → ℝ} (hn : 0 < n) {a b c : ℝ}
    (ha : 0 < a) (hb : 0 < b)
    (hm : ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) :
    c ∈ range z ∧ 0 < right z c ∧
      ∃! r, r ∈ Ioi (0 : ℝ) ∧ IsMaxOn (objective (left z c) (right z c) a) (Ioi 0) r := by
  obtain ⟨i,hci,hi,_⟩ := at_knot z hn ha hb (fun d _ ↦ hm d b hb)
  have he : logLik z (z i) a b = logLik z c a b := le_antisymm (hm _ _ hb) (hi (mem_univ c))
  have hcond (d : ℝ) (hd : logLik z d a b = logLik z c a b) :
      IsMaxOn (objective (left z d) (right z d) a) (Ioi 0) b :=
    fun r hr ↦ (hm d r hr).trans_eq hd.symm
  have hr := rate_score ha hb (hcond (z i) he)
  have hc := rate_score ha hb (hcond c rfl)
  have hp : 0 < right z c := hc ▸ div_pos ha (mul_pos hb (add_pos ha hb))
  have h' : c = z i := hci.eq_or_lt.resolve_right (fun h ↦
    (right_strict_on_span z hn h (i := i) le_rfl).ne (hr.symm.trans hc))
  exact ⟨⟨i,h'.symm⟩,hp,held_left_rate_unique ha hp⟩

private theorem left_mono (z : Fin n → ℝ) {c d : ℝ} (hcd : c ≤ d) : left z c ≤ left z d :=
  div_le_div_of_nonneg_right (Finset.sum_le_sum (fun _ _ ↦
    max_le_max (sub_le_sub_right hcd _) le_rfl)) (Nat.cast_nonneg _)

/-- With the left rate held, the exact global supremum is the maximum of
the one reachable power face and the finite conditional-knot values. -/
theorem held_left_supremum {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    let V := fun c b ↦ logLik z c a b
    let E := log a - a*left z (z u)
    let K := {v : ℝ | ∃ i r, 0 < right z (z i) ∧ 0 < r ∧
      IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) r ∧ v = V (z i) r}
    K.Finite ∧ IsLUB (insert E K) (sSup (insert E K)) ∧
    IsLUB {v | ∃ c b, 0 < b ∧ v = V c b} (sSup (insert E K)) := by
  dsimp only
  let V := fun c b ↦ logLik z c a b
  let E := log a - a*left z (z u)
  let K := {v : ℝ | ∃ i r, 0 < right z (z i) ∧ 0 < r ∧
    IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) r ∧ v = V (z i) r}
  have hK : K.Finite := by
    classical
    let f : Fin n → ℝ := fun i ↦ if hi : 0 < right z (z i) then
      V (z i) (held_left_rate_unique (L := left z (z i)) ha hi).exists.choose else E
    refine (finite_range f).subset ?_
    rintro v ⟨i,r,hi,hr,hm,rfl⟩
    refine ⟨i,?_⟩
    dsimp only [f]
    rw [dite_eq_left hi]
    congr 1
    exact ((held_left_rate_unique (L := left z (z i)) ha hi).unique
      (held_left_rate_unique (L := left z (z i)) ha hi).exists.choose_spec ⟨hr,hm⟩)
  have hfin := hK.insert E
  have hne : (insert E K).Nonempty := ⟨E,mem_insert _ _⟩
  have hL := isLUB_csSup hne hfin.bddAbove
  change K.Finite ∧ IsLUB (insert E K) (sSup (insert E K)) ∧ _
  refine ⟨hK,hL,?_,?_⟩
  · rintro v ⟨c,b,hb,rfl⟩
    obtain ⟨d,hd⟩ := held_rates_exists z hn ha hb
    obtain ⟨i,_,hi,_⟩ := at_knot z hn ha hb hd
    have hv : V c b ≤ V (z i) b := hi (mem_univ c)
    by_cases hp : 0 < right z (z i)
    · obtain ⟨r,⟨hr,hm⟩,_⟩ := held_left_rate_unique (L := left z (z i)) ha hp
      exact hv.trans ((hm hb).trans (hL.1 (mem_insert_of_mem _ ⟨i,r,hp,hr,hm,rfl⟩)))
    · have hzi : z i = z u := le_antisymm (hu i)
        (le_of_not_gt (fun h ↦ hp ((right_pos_iff z _).mpr ⟨u,h⟩)))
      rw [hzi] at hv
      have h0 : V (z u) b = objective (left z (z u)) 0 a b := by simp [V,logLik,right_zero z hu]
      exact hv.trans ((h0.trans_lt ((held_left_rate_zero_deviation ha).2.2 b hb)).le.trans
        (hL.1 (mem_insert _ _)))
  · intro w hw
    apply hL.2
    intro v hv
    rcases mem_insert_iff.mp hv with rfl | hv
    · apply (held_left_rate_zero_deviation (L := left z (z u)) ha).2.1.2
      rintro _ ⟨b,hb,rfl⟩
      simpa [V,logLik,right_zero z hu] using hw ⟨z u,b,hb,rfl⟩
    · obtain ⟨i,r,_,hr,_,rfl⟩ := hv
      exact hw ⟨z i,r,hr,rfl⟩

/-- The complete left-held winning set consists of exactly the eligible
conditional knot winners at or above the excluded power face and every
other eligible conditional knot value. -/
theorem held_left_maximizers {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) (c b : ℝ) :
    let eligible := fun d r ↦ d ∈ range z ∧ 0 < right z d ∧ 0 < r ∧
      IsMaxOn (objective (left z d) (right z d) a) (Ioi 0) r
    (0 < b ∧ ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) ↔
      eligible c b ∧ log a - a*left z (z u) ≤ logLik z c a b ∧
        ∀ d r, eligible d r → logLik z d a r ≤ logLik z c a b := by
  dsimp only
  obtain ⟨_,hfinite,hall⟩ := held_left_supremum hn ha hu
  constructor
  · rintro ⟨hb,hm⟩
    obtain ⟨hk,hR,_⟩ := held_left_winner_is_eligible hn ha hb hm
    refine ⟨⟨hk,hR,hb,fun r hr ↦ hm c r hr⟩,?_,fun d r he ↦ hm d r he.2.2.1⟩
    exact (hfinite.1 (mem_insert _ _)).trans (hall.2 (by
      rintro _ ⟨d,r,hr,rfl⟩
      exact hm d r hr))
  · rintro ⟨⟨_,_,hb,_⟩,hE,hK⟩
    refine ⟨hb,fun d r hr ↦ (hall.1 ⟨d,r,hr,rfl⟩).trans (hfinite.2 ?_)⟩
    intro v hv
    rcases mem_insert_iff.mp hv with rfl | hv
    · exact hE
    · obtain ⟨j,r,hR,hr,hm,rfl⟩ := hv
      exact hK _ _ ⟨⟨j,rfl⟩,hR,hr,hm⟩

/-- Below the power face at every eligible conditional knot, the
supremum is exactly that face and is unattained, including constants. -/
theorem held_left_power_face {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    let E := log a - a*left z (z u)
    ((∀ c b, 0 < b → logLik z c a b < E) ∧
      IsLUB {v | ∃ c b, 0 < b ∧ v = logLik z c a b} E) ↔
      ∀ i b, 0 < right z (z i) → 0 < b →
        IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) b →
        logLik z (z i) a b < E := by
  dsimp only
  refine ⟨fun h i b _ hb _ ↦ h.1 _ _ hb,fun h ↦ ?_⟩
  obtain ⟨_,hfinite,hall⟩ := held_left_supremum hn ha hu
  have hE := le_antisymm (hfinite.2 (by
    intro v hv
    rcases mem_insert_iff.mp hv with rfl | hv
    · rfl
    · obtain ⟨i,b,hR,hb,hm,rfl⟩ := hv
      exact (h i b hR hb hm).le)) (hfinite.1 (mem_insert _ _))
  rw [hE] at hall
  refine ⟨fun c b hb ↦ (hall.1 ⟨c,b,hb,rfl⟩).lt_of_ne ?_,hall⟩
  intro he
  have hm : ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b :=
    fun d r hr ↦ (hall.1 ⟨d,r,hr,rfl⟩).trans_eq he.symm
  obtain ⟨⟨i,hi⟩,hR,_⟩ := held_left_winner_is_eligible hn ha hb hm
  subst c
  exact (h i b hR hb (fun r hr ↦ hm _ r hr)).ne he

/-- At any kink at or above the sample maximum, sending the right
rate to infinity reaches the power face with the left rate unchanged. -/
theorem held_left_power_path {z : Fin n → ℝ} {a c : ℝ} (ha : 0 < a)
    (hc : ∀ i, z i ≤ c) :
    Tendsto (logLik z c a) atTop (𝓝 (log a - a*left z c)) := by
  change Tendsto (objective (left z c) (right z c) a) atTop _
  rw [right_zero z hc]
  exact (held_left_rate_zero_deviation ha).1

private theorem left_reflect (z : Fin n → ℝ) (c : ℝ) :
    left (fun i ↦ -z i) (-c) = right z c := by simp [left,right,sub_eq_add_neg,add_comm]

private theorem right_reflect (z : Fin n → ℝ) (c : ℝ) :
    right (fun i ↦ -z i) (-c) = left z c := by simp [left,right,sub_eq_add_neg,add_comm]

private theorem lik_reflect (z : Fin n → ℝ) (c a b : ℝ) :
    logLik (fun i ↦ -z i) (-c) b a = logLik z c a b := by
  rw [logLik,left_reflect,right_reflect,logLik]
  unfold objective
  rw [mul_comm b a, add_comm b a]
  ring

/-- Reflection gives the complete right-held winning set, with the
Pareto face at the sample minimum and a unique left rate at every knot. -/
theorem held_right_maximizers {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) (c a : ℝ) :
    let eligible := fun d r ↦ d ∈ range z ∧ 0 < left z d ∧ 0 < r ∧
      IsMaxOn (fun t ↦ objective (left z d) (right z d) t b) (Ioi 0) r
    (0 < a ∧ ∀ d r, 0 < r → logLik z d r b ≤ logLik z c a b) ↔
      eligible c a ∧ log b - b*right z (z l) ≤ logLik z c a b ∧
        ∀ d r, eligible d r → logLik z d r b ≤ logLik z c a b := by
  let w : Fin n → ℝ := fun i ↦ -z i
  have hw : ∀ i, w i ≤ w l := fun i ↦ neg_le_neg (hl i)
  have he (d : ℝ) : -d ∈ range (fun i ↦ -z i) ↔ d ∈ range z := by
    constructor
    · rintro ⟨i,hi⟩
      exact ⟨i,neg_injective hi⟩
    · rintro ⟨i,rfl⟩
      exact ⟨i,rfl⟩
  have h := held_left_maximizers hn hb hw (-c) a
  change _ ∧ (∀ d r, 0 < r → logLik w d b r ≤ logLik w (-c) b a) ↔ _ at h
  have hn' : Function.Surjective (fun d : ℝ ↦ -d) :=
    fun d ↦ ⟨-d,neg_neg _⟩
  rw [hn'.forall (p := fun d ↦ ∀ r, 0 < r → logLik w d b r ≤ logLik w (-c) b a)] at h
  rw [hn'.forall (p := fun d ↦ ∀ r, (d ∈ range w ∧ 0 < right w d ∧ 0 < r ∧
    IsMaxOn (objective (left w d) (right w d) b) (Ioi 0) r) →
    logLik w d b r ≤ logLik w (-c) b a)] at h
  simp only [w] at h
  simp only [lik_reflect, he, left_reflect, right_reflect] at h
  have hs (d : ℝ) : objective (right z d) (left z d) b =
      (fun a ↦ objective (left z d) (right z d) a b) := by
    funext a
    unfold objective
    rw [mul_comm b a,add_comm b a]
    ring
  simpa only [hs] using h

/-- Sending the left rate to infinity at any kink at or below the
sample minimum reaches the right-held Pareto face. -/
theorem held_right_pareto_path {z : Fin n → ℝ} {b c : ℝ} (hb : 0 < b)
    (hc : ∀ i, c ≤ z i) :
    Tendsto (fun a ↦ logLik z c a b) atTop (𝓝 (log b - b*right z c)) := by
  let w : Fin n → ℝ := fun i ↦ -z i
  have hw : ∀ i, w i ≤ -c := fun i ↦ neg_le_neg (hc i)
  simpa only [w,left_reflect,show logLik w (-c) b = (fun a ↦ logLik z c a b)
    from funext fun a ↦ lik_reflect z c a b] using held_left_power_path hb hw

/-- If every eligible conditional knot loses, the right-held Pareto
face is the exact unattained global supremum. -/
theorem held_right_pareto_face {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) :
    let E := log b - b*right z (z l)
    ((∀ c a, 0 < a → logLik z c a b < E) ∧
      IsLUB {v | ∃ c a, 0 < a ∧ v = logLik z c a b} E) ↔
      ∀ i a, 0 < left z (z i) → 0 < a →
        IsMaxOn (fun t ↦ objective (left z (z i)) (right z (z i)) t b) (Ioi 0) a →
        logLik z (z i) a b < E := by
  let w : Fin n → ℝ := fun i ↦ -z i
  have hw : ∀ i, w i ≤ w l := fun i ↦ neg_le_neg (hl i)
  have h := held_left_power_face hn hb hw
  have hs (d : ℝ) : objective (right z d) (left z d) b =
      (fun a ↦ objective (left z d) (right z d) a b) := by
    funext a
    unfold objective
    rw [mul_comm b a,add_comm b a]
    ring
  dsimp only at h ⊢
  have hE : left w (w l) = right z (z l) := left_reflect z (z l)
  have hsets : {v | ∃ c a, 0 < a ∧ v = logLik w c b a} =
      {v | ∃ c a, 0 < a ∧ v = logLik z c a b} := by
    ext v
    constructor
    · rintro ⟨c,a,ha,rfl⟩
      exact ⟨-c,a,ha,by simpa only [w,neg_neg] using lik_reflect z (-c) a b⟩
    · rintro ⟨c,a,ha,rfl⟩
      exact ⟨-c,a,ha,(lik_reflect z c a b).symm⟩
  rw [hE,hsets] at h
  have hneg : Function.Surjective (fun c : ℝ ↦ -c) :=
    fun c ↦ ⟨-c,neg_neg _⟩
  rw [hneg.forall (p := fun c ↦ ∀ a, 0 < a →
    logLik w c b a < log b - b*right z (z l))] at h
  simpa only [w,lik_reflect,right_reflect,left_reflect,hs] using h

/-- A left-held MLE exists exactly when some eligible knot at its
conditional right rate reaches the power face. Equality suffices. -/
theorem held_left_criterion {z : Fin n → ℝ} (hn : 0 < n) {a : ℝ} (ha : 0 < a)
    {u : Fin n} (hu : ∀ i, z i ≤ z u) :
    (∃ c b, 0 < b ∧ ∀ d r, 0 < r → logLik z d a r ≤ logLik z c a b) ↔
      ∃ i b, 0 < right z (z i) ∧ 0 < b ∧
        IsMaxOn (objective (left z (z i)) (right z (z i)) a) (Ioi 0) b ∧
        log a - a*left z (z u) ≤ logLik z (z i) a b := by
  constructor
  · rintro ⟨c,b,hb,hm⟩
    obtain ⟨⟨⟨i,hi⟩,hR,_,hcond⟩,hE,_⟩ := (held_left_maximizers hn ha hu c b).mp ⟨hb,hm⟩
    subst c
    exact ⟨i,b,hR,hb,hcond,hE⟩
  · rintro ⟨i,b,hR,hb,hm,hE⟩
    let K := {v : ℝ | ∃ j r, 0 < right z (z j) ∧ 0 < r ∧
      IsMaxOn (objective (left z (z j)) (right z (z j)) a) (Ioi 0) r ∧ v = logLik z (z j) a r}
    have hK : K.Finite := (held_left_supremum hn ha hu).1
    have hw : logLik z (z i) a b ∈ K := ⟨i,b,hR,hb,hm,rfl⟩
    obtain ⟨v,hv,hmax⟩ := hK.isCompact.exists_isMaxOn ⟨_,hw⟩ continuous_id.continuousOn
    obtain ⟨j,r,hR',hr,hm',rfl⟩ := hv
    refine ⟨z j,r,(held_left_maximizers hn ha hu _ _).mpr ?_⟩
    refine ⟨⟨⟨j,rfl⟩,hR',hr,hm'⟩,hE.trans (hmax hw),?_⟩
    rintro _ t ⟨⟨k,rfl⟩,hR'',ht,hcond⟩
    exact hmax (show logLik z (z k) a t ∈ K from ⟨k,t,hR'',ht,hcond,rfl⟩)

/-- A right-held MLE exists exactly when some eligible knot at its
conditional left rate reaches the Pareto face. Equality suffices. -/
theorem held_right_criterion {z : Fin n → ℝ} (hn : 0 < n) {b : ℝ} (hb : 0 < b)
    {l : Fin n} (hl : ∀ i, z l ≤ z i) :
    (∃ c a, 0 < a ∧ ∀ d r, 0 < r → logLik z d r b ≤ logLik z c a b) ↔
      ∃ i a, 0 < left z (z i) ∧ 0 < a ∧
        IsMaxOn (fun t ↦ objective (left z (z i)) (right z (z i)) t b) (Ioi 0) a ∧
        log b - b*right z (z l) ≤ logLik z (z i) a b := by
  let w : Fin n → ℝ := fun i ↦ -z i
  have hw : ∀ i, w i ≤ w l := fun i ↦ neg_le_neg (hl i)
  have hs (d : ℝ) : objective (right z d) (left z d) b =
      (fun a ↦ objective (left z d) (right z d) a b) := by
    funext a
    unfold objective
    rw [mul_comm b a,add_comm b a]
    ring
  have h := held_left_criterion hn hb hw
  have hneg : Function.Surjective (fun c : ℝ ↦ -c) :=
    fun c ↦ ⟨-c,neg_neg _⟩
  rw [hneg.exists (p := fun c ↦ ∃ a, 0 < a ∧ ∀ d r, 0 < r →
    logLik w d b r ≤ logLik w c b a)] at h
  have he c a : (∀ d r, 0 < r → logLik w d b r ≤ logLik w (-c) b a) ↔
      (∀ d r, 0 < r → logLik z d r b ≤ logLik z c a b) := by
    rw [hneg.forall (p := fun d ↦ ∀ r, 0 < r → logLik w d b r ≤ logLik w (-c) b a)]
    simp only [w,lik_reflect]
  simp only [he] at h
  simpa only [w,lik_reflect,right_reflect,left_reflect,hs] using h

end UnivariateML.LocScaleDoublePareto

end
