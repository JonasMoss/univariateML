module

public import UnivariateML.Family.StudentHeldShape
public import UnivariateML.Family.StudentCauchy

import Mathlib.Tactic
import Mathlib.Topology.Instances.EReal.Lemmas
import Mathlib.Topology.Sequences

set_option linter.style.header false

/-!
# Location–scale Student at held positive df

Critical subunit shape is handled by compact superlevels and a weak
finite comparison with the sharp critical atom height.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Student2LocationScaleHeldDf

private def domain : Set (ℝ × ℝ) := univ ×ˢ Ioi 0

private noncomputable def value {n : ℕ} (x : Fin n → ℝ) (ν : ℝ) (p : ℝ × ℝ) : ℝ :=
  TDist.likelihood x p.1 ν p.2

private theorem logLik_continuousOn {n : ℕ} (x : Fin n → ℝ) {ν : ℝ} (hν : 0 < ν) :
    ContinuousOn (fun p : ℝ × ℝ ↦ TDist.logLik x p.1 ν p.2) domain := by
  apply continuousOn_finsetSum
  intro i _
  refine (continuousOn_const.sub (continuous_snd.continuousOn.log ?_)).sub
    (continuousOn_const.mul (ContinuousOn.log ?_ ?_))
  · exact fun p hp ↦ (show 0 < p.2 from hp.2).ne'
  · apply continuousOn_const.add
    exact (by fun_prop : Continuous (fun p : ℝ × ℝ ↦ (x i-p.1)^2)).continuousOn.div
      (by fun_prop) (fun p hp ↦ mul_ne_zero hν.ne' (pow_ne_zero 2 (show 0 < p.2 from hp.2).ne'))
  · intro p hp
    have hs : 0 < p.2 := hp.2
    positivity

private theorem superlevel_compact {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν B : ℝ}
    (hν : 0 < ν) (hB : 0 < B) (he : ∀ b, 0 ≤ TDist.exponent x b ν)
    (hcrit : ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) < B) :
    IsCompact {p ∈ domain | B ≤ value x ν p} := by
  apply IsSeqCompact.isCompact
  intro p hp
  let u : ℕ → EReal × EReal := fun j ↦ ((p j).1, (p j).2)
  obtain ⟨z,φ,hφ,hu⟩ := CompactSpace.tendsto_subseq u
  let m := fun j ↦ (p (φ j)).1
  let a := fun j ↦ (p (φ j)).2
  have hpos : ∀ᶠ j in atTop, 0 < a j := Eventually.of_forall fun j ↦ (hp (φ j)).1.2
  have hval : ∀ᶠ j in atTop, B ≤ TDist.likelihood x (m j) ν (a j) :=
    Eventually.of_forall fun j ↦ (hp (φ j)).2
  have hz : ¬ Tendsto (fun j ↦ TDist.likelihood x (m j) ν (a j)) atTop (𝓝 0) :=
    fun h ↦ hB.not_ge (ge_of_tendsto h hval)
  have hremote (h : Tendsto a atTop atTop ∨ Tendsto m atTop (cocompact ℝ)) : False :=
    hz (StudentFaces.lst_fixed_df_remote_ends x hn hν hpos h)
  have hm : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) := (continuous_fst.tendsto z).comp hu
  have ha : Tendsto (fun j ↦ (a j : EReal)) atTop (𝓝 z.2) := (continuous_snd.tendsto z).comp hu
  generalize z.1 = zm at hm
  generalize z.2 = za at ha
  cases zm using EReal.rec with
  | bot =>
    exact (hremote (.inr ((EReal.tendsto_coe_nhds_bot_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left)))).elim
  | top =>
    exact (hremote (.inr ((EReal.tendsto_coe_nhds_top_iff.mp hm).mono_right
      (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right)))).elim
  | coe b =>
    have hb := EReal.tendsto_coe.mp hm
    cases za using EReal.rec with
    | bot =>
      obtain ⟨j,hj,hj'⟩ := (hpos.and
        ((EReal.tendsto_coe_nhds_bot_iff.mp ha).eventually_lt_atBot 0)).exists
      exact (hj.not_gt hj').elim
    | top => exact (hremote (.inl (EReal.tendsto_coe_nhds_top_iff.mp ha))).elim
    | coe s =>
      have hs := EReal.tendsto_coe.mp ha
      have h₀ := ge_of_tendsto hs (hpos.mono fun _ h ↦ h.le)
      rcases h₀.eq_or_lt with hEq | hs₀
      · subst s
        have h := StudentFaces.lst_fixed_df_local_collapse x b hν hb
          (tendsto_nhdsWithin_iff.mpr ⟨hs,hpos⟩)
        rcases (he b).eq_or_lt with hE | hE
        · obtain ⟨j,hlt,hle⟩ := (((h.2 hE.symm).2 B (hcrit b hE.symm)).and hval).exists
          exact (hlt.not_ge hle).elim
        · exact (hz (h.1 hE)).elim
      · have hd : (b,s) ∈ domain := ⟨mem_univ _, hs₀⟩
        have hlim : Tendsto (p ∘ φ) atTop (𝓝 (b,s)) := hb.prodMk_nhds hs
        have hl : ContinuousOn (value x ν) domain :=
          continuous_exp.comp_continuousOn (logLik_continuousOn x hν)
        have hd' := (tendsto_nhdsWithin_iff.mpr ⟨hlim,
          Eventually.of_forall fun j ↦ (hp (φ j)).1⟩)
        have hv := ((hl (b,s) hd).tendsto).comp hd'
        exact ⟨(b,s), ⟨hd, ge_of_tendsto hv hval⟩, φ, hφ, hlim⟩

private theorem max_of_witness {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν)
    (he : ∀ b, 0 ≤ TDist.exponent x b ν) {p : ℝ × ℝ} (hp : p ∈ domain)
    (h : ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) ≤ value x ν p) :
    ∃ q ∈ domain, IsMaxOn (value x ν) domain q := by
  by_cases hm : IsMaxOn (value x ν) domain p
  · exact ⟨p,hp,hm⟩
  · rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hpw⟩ := hm
    let K := {q ∈ domain | value x ν w ≤ value x ν q}
    have hK : IsCompact K := superlevel_compact x hn hν (exp_pos _) he
      (fun b hb ↦ (h b hb).trans_lt hpw)
    have hl : ContinuousOn (value x ν) domain :=
      continuous_exp.comp_continuousOn (logLik_continuousOn x hν)
    obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ (hl.mono fun _ h ↦ h.1)
    exact ⟨q,hq.1,fun z hz ↦ if h : value x ν w ≤ value x ν z then hqm ⟨hz,h⟩
      else (le_of_not_ge h).trans hq.2⟩

/-- At held df, a supercritical atom precludes a fit. With no such atom,
attainment is equivalent to a stationary point reaching all critical
collapse heights. This includes all strict masses and the equality
cases, including the balanced Cauchy pair. -/
theorem lst_held_df_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 ν p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 0 ≤ TDist.exponent x b ν) ∧
      ∃ p ∈ D, fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 ν q.2) p = 0 ∧
        ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) ≤ L p := by
  dsimp only
  change (∃ p ∈ domain, IsMaxOn (value x ν) domain p) ↔ _
  constructor
  · rintro ⟨p,hp,hm⟩
    have hm' (b a : ℝ) (ha : 0 < a) : TDist.likelihood x b ν a ≤ value x ν p := by
      have h : value x ν (b,a) ≤ value x ν p := hm (show (b,a) ∈ domain from ⟨mem_univ _,ha⟩)
      exact h
    have hp' : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    refine ⟨?_,p,hp,?_,?_⟩
    · intro b
      by_contra! hb
      obtain ⟨a,ha,hgt⟩ := (hp'.and
        ((TDist.collapse_unbounded x b hν hb).eventually_gt_atTop (value x ν p))).exists
      exact hgt.not_ge (hm' b a ha)
    · have hLog : IsMaxOn (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 ν q.2) domain p :=
        fun _ hq ↦ exp_le_exp.mp (hm hq)
      have ho : IsOpen domain := isOpen_univ.prod isOpen_Ioi
      exact (hLog.isLocalMax (ho.mem_nhds hp)).fderiv_eq_zero
    · intro b hb
      have h := StudentFaces.lst_fixed_df_local_collapse x b hν (l := 𝓝[>] (0 : ℝ))
        (m := fun _ ↦ b) (a := id) tendsto_const_nhds tendsto_id
      exact le_of_tendsto (h.2 hb).1 (hp'.mono fun a ha ↦ hm' b a ha)
  · rintro ⟨he,p,hp,_,h⟩
    exact max_of_witness x hn hν he hp h

/-- The full held-df decision distinguishes strict mass, the balanced
Cauchy pair, and a subunit stationary candidate reaching all critical
heights. The weak comparison includes finite ties with a collapse face. -/
theorem lst_held_df_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 ν p.2
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 0 < TDist.exponent x b ν) ∨
      (ν = 1 ∧ ∃ a b, a ≠ b ∧ 2 * TDist.multiplicity x a = n ∧
        ∀ i, x i = a ∨ x i = b) ∨
      (ν < 1 ∧ (∀ b, 0 ≤ TDist.exponent x b ν) ∧
        ∃ p ∈ D, fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 ν q.2) p = 0 ∧
          ∀ b, TDist.exponent x b ν = 0 → exp (TDist.collapseLogValue x b ν) ≤ L p) := by
  dsimp only
  change (∃ p ∈ domain, IsMaxOn (value x ν) domain p) ↔ _
  have hlog {v : ℝ} (p : ℝ × ℝ) : IsMaxOn (value x v) domain p ↔
      IsMaxOn (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 v q.2) domain p := by
    exact ⟨fun h _ hq ↦ exp_le_exp.mp (h hq), fun h _ hq ↦ exp_le_exp.mpr (h hq)⟩
  have hc : (∃ p ∈ domain, IsMaxOn (value x 1) domain p) ↔
      (∀ b, 2 * TDist.multiplicity x b < n) ∨
        ∃ a b, a ≠ b ∧ 2 * TDist.multiplicity x a = n ∧ ∀ i, x i = a ∨ x i = b := by
    rw [show (∃ p ∈ domain, IsMaxOn (value x 1) domain p) ↔
      (∃ p ∈ UnivariateML.Cauchy.domain,
        IsMaxOn (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 1 q.2) UnivariateML.Cauchy.domain p)
      from exists_congr fun p ↦ and_congr_right (fun _ ↦ hlog p)]
    exact StudentCauchy.lst_df_one_attains_iff x hn
  have hcrit := lst_held_df_criterion x hn hν
  constructor
  · rintro ⟨p,hp,hm⟩
    obtain ⟨he,q,hq,hq',h⟩ := hcrit.mp ⟨p,hp,hm⟩
    by_cases hpos : ∀ b, 0 < TDist.exponent x b ν
    · exact Or.inl hpos
    push Not at hpos
    obtain ⟨b,hb⟩ := hpos
    have hzero : TDist.exponent x b ν = 0 := hb.antisymm (he b)
    rcases lt_trichotomy ν 1 with hlt | rfl | hgt
    · exact Or.inr (Or.inr ⟨hlt,he,q,hq,hq',h⟩)
    · rcases hc.mp ⟨p,hp,hm⟩ with hs | hs
      · have H := hs b
        have H' : (2 : ℝ) * TDist.multiplicity x b < (n : ℝ) := by exact_mod_cast H
        unfold TDist.exponent at hzero
        linarith
      · exact Or.inr (Or.inl ⟨rfl,hs⟩)
    · exact (((StudentFaces.lst_critical_joint x hn hgt hzero).2 q.1 q.2 hq.2).not_ge
        (h b hzero)).elim
  · rintro (hpos | ⟨rfl,hpair⟩ | ⟨_,he,p,hp,hst,h⟩)
    · exact max_of_witness x hn hν (fun b ↦ (hpos b).le)
        (p := (0,1)) ⟨mem_univ _,zero_lt_one⟩
        (fun b hb ↦ ((hpos b).ne' hb).elim)
    · exact hc.mpr (Or.inr hpair)
    · exact hcrit.mpr ⟨he,p,hp,hst,h⟩

/-- At held positive shape only a zero-scale atom can give a positive
boundary envelope. Negative exponent is equivalent to unboundedness on
the chart; a critical atom has a sharp finite vertical trace. -/
theorem lst_held_df_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {ν : ℝ} (hν : 0 < ν) :
    let L := fun m a ↦ TDist.likelihood x m ν a
    ((¬ BddAbove ((fun p : ℝ × ℝ ↦ L p.1 p.2) '' (univ ×ˢ Ioi 0))) ↔
      ∃ b, TDist.exponent x b ν < 0) ∧
    (∀ b, TDist.exponent x b ν < 0 → Tendsto (L b) (𝓝[>] 0) atTop) ∧
    (∀ {ι : Type*} {l : Filter ι} {m a : ι → ℝ}, (∀ᶠ j in l, 0 < a j) →
      (Tendsto a l atTop ∨ Tendsto m l (cocompact ℝ)) →
      Tendsto (fun j ↦ L (m j) (a j)) l (𝓝 0)) ∧
    (∀ b {ι : Type*} {l : Filter ι} {m a : ι → ℝ},
      Tendsto m l (𝓝 b) → Tendsto a l (𝓝[>] 0) →
      (0 < TDist.exponent x b ν → Tendsto (fun j ↦ L (m j) (a j)) l (𝓝 0)) ∧
      (TDist.exponent x b ν = 0 →
        Tendsto (L b) (𝓝[>] 0) (𝓝 (exp (TDist.collapseLogValue x b ν))) ∧
        ∀ B > exp (TDist.collapseLogValue x b ν), ∀ᶠ j in l, L (m j) (a j) < B)) := by
  dsimp only
  refine ⟨?_, fun b ↦ TDist.collapse_unbounded x b hν, ?_, ?_⟩
  rotate_left
  · intro ι l m a ha ht
    exact StudentFaces.lst_fixed_df_remote_ends x hn hν ha ht
  · intro b ι l m a hm ha
    exact StudentFaces.lst_fixed_df_local_collapse x b hν hm ha
  change (¬ BddAbove (value x ν '' domain)) ↔ _
  constructor
  · intro hub
    by_contra! h
    by_cases hw : ∃ p ∈ domain, ∀ b, TDist.exponent x b ν = 0 →
        exp (TDist.collapseLogValue x b ν) ≤ value x ν p
    · obtain ⟨p,hp,hp'⟩ := hw
      obtain ⟨q,hq,hm⟩ := max_of_witness x hn hν h hp hp'
      exact hub ⟨value x ν q, by rintro _ ⟨z,hz,rfl⟩; exact hm hz⟩
    · push Not at hw
      let B := ∑ i, exp (TDist.collapseLogValue x (x i) ν)
      apply hub
      refine ⟨B, ?_⟩
      rintro _ ⟨p,hp,rfl⟩
      obtain ⟨b,hb,hlt⟩ := hw p hp
      have hpos : 0 < TDist.multiplicity x b := by
        by_contra! hzero
        have hz : TDist.multiplicity x b = 0 := Nat.eq_zero_of_le_zero hzero
        have hn' : (0 : ℝ) < n := by exact_mod_cast hn
        unfold TDist.exponent at hb
        rw [hz, Nat.cast_zero, sub_zero, sub_zero] at hb
        exact (mul_pos hν hn').ne' hb
      obtain ⟨i,hi⟩ := Finset.card_pos.mp hpos
      have hi' : x i = b := (Finset.mem_filter.mp hi).2
      apply hlt.le.trans
      rw [← hi']
      exact Finset.single_le_sum (f := fun j ↦ exp (TDist.collapseLogValue x (x j) ν))
        (fun _ _ ↦ (exp_pos _).le) (Finset.mem_univ i)
  · rintro ⟨b,hb⟩ ⟨B,hB⟩
    have hp : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha,hbad⟩ := (hp.and
      ((TDist.collapse_unbounded x b hν hb).eventually_gt_atTop B)).exists
    exact hbad.not_ge (show value x ν (b,a) ≤ B from hB ⟨(b,a),⟨mem_univ _,ha⟩,rfl⟩)


/-- The five-level sample for the subunit shape reflection witness. -/
def reflectedSubunitSample : Fin 5 → ℝ := ![-100,-99,0,99,100]

private theorem reflected_subunit_atom_bound (b : ℝ) :
    TDist.multiplicity reflectedSubunitSample b ≤ 1 := by
  classical
  unfold TDist.multiplicity
  rw [Finset.card_le_one_iff]
  intro i j hi hj
  have hind : Function.Injective reflectedSubunitSample := by
    intro a b hab
    fin_cases a <;> fin_cases b <;> norm_num [reflectedSubunitSample] at hab <;> rfl
  exact hind (((Finset.mem_filter.mp hi).2).trans ((Finset.mem_filter.mp hj).2).symm)

private theorem reflected_subunit_logLik (m s : ℝ) :
    TDist.logLik reflectedSubunitSample (-m) (1/2) s =
      TDist.logLik reflectedSubunitSample m (1/2) s := by
  have href (i : Fin 5) :
      reflectedSubunitSample (Fin.revPerm i) = -reflectedSubunitSample i := by
    fin_cases i <;> norm_num [reflectedSubunitSample,Fin.revPerm,Fin.rev]
  unfold TDist.logLik
  rw [← Equiv.sum_comp Fin.revPerm (fun i ↦ TDist.logNormalizer (1/2)-log s-
    (1/2+1)/2*log (1+(reflectedSubunitSample i- -m)^2/(1/2*s^2)))]
  apply Finset.sum_congr rfl
  intro i _
  rw [href,show -reflectedSubunitSample i- -m = -(reflectedSubunitSample i-m) by ring,neg_sq]

private noncomputable def subunitComparison (m s : ℝ) : ℝ :=
  (s^2/2)^5 / (∏ i, (s^2/2+(reflectedSubunitSample i-m)^2))^3

private theorem subunitComparison_pos (m : ℝ) {s : ℝ} (hs : 0 < s) :
    0 < subunitComparison m s := by unfold subunitComparison; positivity

private theorem subunitComparison_log (m : ℝ) {s : ℝ} (hs : 0 < s) :
    log (subunitComparison m s) =
      4*TDist.logLik reflectedSubunitSample m (1/2) s-
        20*TDist.logNormalizer (1/2)-10*log (1/2) := by
  let Q := s^2/2
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have hL (i : Fin 5) : log (1+(reflectedSubunitSample i-m)^2/(1/2*s^2)) =
      log (Q+(reflectedSubunitSample i-m)^2)-log Q := by
    have hW : (1:ℝ)/2*s^2 = Q := by unfold Q; ring
    rw [hW,show 1+(reflectedSubunitSample i-m)^2/Q = (Q+(reflectedSubunitSample i-m)^2)/Q by field_simp,
      log_div (by positivity) hQ.ne']
  have hqLog : log Q = 2*log s+log (1/2) := by
    unfold Q
    rw [show s^2/2 = s^2*(1/2) by ring,log_mul (by positivity) (by norm_num),log_pow]
    norm_num
  unfold subunitComparison TDist.logLik
  change log (Q^5 / (∏ i, (Q+(reflectedSubunitSample i-m)^2))^3) = _
  rw [log_div (by positivity) (by positivity),log_pow,log_pow,log_prod (fun i _ ↦ by positivity)]
  simp_rw [hL]
  rw [Finset.sum_sub_distrib,Finset.sum_sub_distrib, Finset.sum_const]
  simp only [← Finset.mul_sum,Finset.sum_const,Finset.card_univ,Fintype.card_fin,nsmul_eq_mul]
  norm_num
  linarith

private theorem centered_subunit_scale_bounds {s : ℝ} (hs : 0 < s)
    (hstat : TDist.scaleScore reflectedSubunitSample 0 (1/2) s = 0) :
    1979 < s^2/2 ∧ s^2/2 < 1981 := by
  let Q := s^2/2
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have hq : (1:ℝ)/2*s^2 = Q := by dsimp [Q]; ring
  unfold TDist.scaleScore at hstat
  rw [hq] at hstat
  norm_num [Fin.sum_univ_succ,reflectedSubunitSample] at hstat
  have h1 : Q+9801 ≠ 0 := by positivity
  have h2 : Q+10000 ≠ 0 := by positivity
  field_simp [h1,h2,hQ.ne'] at hstat
  have hp : 5*Q^2+39602*Q-98010000 = 0 := by linear_combination -hstat/2
  change 1979 < Q ∧ Q < 1981
  constructor
  · by_contra! h
    have hs' : Q^2 ≤ 1979^2 := pow_le_pow_left₀ hQ.le h 2
    linarith
  · by_contra! h
    have hs' : 1981^2 ≤ Q^2 := pow_le_pow_left₀ (by norm_num) h 2
    linarith

private theorem centered_subunit_value_loses {s : ℝ} (hs : 0 < s)
    (hstat : TDist.scaleScore reflectedSubunitSample 0 (1/2) s = 0) :
    TDist.logLik reflectedSubunitSample 0 (1/2) s <
      TDist.logLik reflectedSubunitSample (199/2) (1/2) (1/2) := by
  have ⟨hl,hu⟩ := centered_subunit_scale_bounds hs hstat
  let Q := s^2/2
  have hQ : 0 < Q := by dsimp [Q]; positivity
  have he : subunitComparison 0 s = Q^2/((Q+9801)^6*(Q+10000)^6) := by
    unfold subunitComparison
    change Q^5 / (∏ i, (Q+(reflectedSubunitSample i-0)^2))^3 = _
    norm_num [Fin.prod_univ_succ,reflectedSubunitSample]
    field_simp
  have hB : subunitComparison 0 s ≤ 1981^2/(11780^6*11979^6) := by
    rw [he]
    apply div_le_div₀ (by positivity)
    · exact pow_le_pow_left₀ hQ.le hu.le 2
    · norm_num
    · gcongr
      · dsimp [Q]; linarith
      · dsimp [Q]; linarith
  have hw : (1981:ℝ)^2/(11780^6*11979^6) < subunitComparison (199/2) (1/2) := by
    norm_num [subunitComparison,Fin.prod_univ_succ,reflectedSubunitSample]
  have hL := log_lt_log (subunitComparison_pos 0 hs) (hB.trans_lt hw)
  rw [subunitComparison_log 0 hs,subunitComparison_log (199/2) (by norm_num : (0:ℝ)<1/2)] at hL
  linarith

/-- At df one half, the prescribed five-level symmetric Student sample
has two distinct reflected global location–scale maximizers. -/
theorem reflected_subunit_global_maxima :
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood reflectedSubunitSample p.1 (1/2) p.2
    let D := (univ : Set ℝ) ×ˢ Ioi (0 : ℝ)
    ∃ m s : ℝ, m ≠ 0 ∧ 0 < s ∧ IsMaxOn L D (m,s) ∧ IsMaxOn L D (-m,s) := by
  dsimp only
  have ex (b : ℝ) : 0 < TDist.exponent reflectedSubunitSample b (1/2) := by
    have h := reflected_subunit_atom_bound b
    have hr : (TDist.multiplicity reflectedSubunitSample b : ℝ) ≤ 1 := by exact_mod_cast h
    unfold TDist.exponent
    norm_num
    linarith
  obtain ⟨⟨m,s⟩,hp,hm⟩ := (lst_held_df_decision reflectedSubunitSample (by decide)
      (by norm_num : (0:ℝ)<1/2)).mpr (Or.inl ex)
  have hs : 0 < s := hp.2
  have hm0 : m ≠ 0 := by
    intro he
    subst m
    have hM : IsMaxOn (TDist.logLik reflectedSubunitSample 0 (1/2)) (Ioi 0) s := by
      intro u hu
      have hh := @hm (0,u) ⟨mem_univ _,hu⟩
      change exp (TDist.logLik reflectedSubunitSample 0 (1/2) u) ≤
        exp (TDist.logLik reflectedSubunitSample 0 (1/2) s) at hh
      exact exp_le_exp.mp hh
    have hx : ∃ i, reflectedSubunitSample i ≠ 0 := ⟨0,by norm_num [reflectedSubunitSample]⟩
    have hscore := (((TDist.heldLocation_maximum reflectedSubunitSample 0
      (by norm_num : (0:ℝ)<1/2) hx).2 (ex 0)).2 s hs).mp hM
    have hh := @hm (199/2,1/2) ⟨mem_univ _,by norm_num⟩
    change exp (TDist.logLik reflectedSubunitSample (199/2) (1/2) (1/2)) ≤
      exp (TDist.logLik reflectedSubunitSample 0 (1/2) s) at hh
    exact (centered_subunit_value_loses hs hscore).not_ge (exp_le_exp.mp hh)
  refine ⟨m,s,hm0,hs,hm,?_⟩
  intro p hp
  change TDist.likelihood reflectedSubunitSample p.1 (1/2) p.2 ≤
    TDist.likelihood reflectedSubunitSample (-m) (1/2) s
  unfold TDist.likelihood
  rw [reflected_subunit_logLik]
  exact @hm p hp

end UnivariateML.Student2LocationScaleHeldDf
end
