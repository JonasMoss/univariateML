module

public import UnivariateML.Family.Discrete4CMB
public import UnivariateML.Family.FinishCMB

set_option linter.style.header false

@[expose] public section

open Set
namespace UnivariateML.Base8CMB

open Discrete3CMB Discrete4CMB

/-- Every feasible size has an attained probability profile, and the
finite capped size comparison attains its maximum. -/
theorem held_dispersion_failure {n B : ℕ} (hn : 0 < n) (x : Fin n → ℕ)
    (ν : ℝ) (hx : ∀ i, x i ≤ B) :
    (∀ N ≤ B, (∀ i, x i ≤ N) →
      ∃ p ∈ Icc (0 : ℝ) 1,
        IsMaxOn (fun q ↦ sizeLikelihood x N q ν) (Icc 0 1) p) ∧
    ∃ N ≤ B, (∀ i, x i ≤ N) ∧ ∃ p ∈ Icc (0 : ℝ) 1,
      ∀ J ≤ B, (∀ i, x i ≤ J) → ∀ q ∈ Icc (0 : ℝ) 1,
        sizeLikelihood x J q ν ≤ sizeLikelihood x N p ν := by
  constructor
  · intro N _ hN
    let y (i : Fin n) : Fin (N+1) := ⟨x i, Nat.lt_succ_of_le (hN i)⟩
    have H := held_dispersion_size hn y ν
    have he : (fun q ↦ sizeLikelihood x N q ν) =
        fun q ↦ closedLikelihood y q ν := by
      funext q
      simp [sizeLikelihood, hN, y]
    rw [he]
    by_cases h : N = 0
    · exact ⟨0, ⟨le_rfl, zero_le_one⟩, fun q hq ↦ by
        change closedLikelihood y q ν ≤ closedLikelihood y 0 ν
        rw [H.1 h q hq, H.1 h 0 ⟨le_rfl,zero_le_one⟩]⟩
    · obtain ⟨q, hq, _⟩ := H.2 (Nat.pos_of_ne_zero h)
      exact ⟨q, hq⟩
  · obtain ⟨⟨N,p⟩, ⟨hN,hxN,hp⟩, hM⟩ := held_dispersion_capped hn x ν hx
    exact ⟨N,hN,hxN,p,hp,fun J hJ hxJ q hq ↦ hM (a := (J,q)) ⟨hJ,hxJ,hq⟩⟩

/-- When both dispersion and probability are held, the maximizer set is
the set of compatible sizes tied at the winning height. At zero dispersion
and interior probability it is exactly the smallest feasible size. -/
theorem held_probability_dispersion_maximizers {n B : ℕ} (hn : 0 < n)
    (x : Fin n → ℕ) (p ν : ℝ) (hB : ∀ i, x i ≤ B) :
    let D : Set ℕ := {N | N ≤ B ∧ Compatible x N p}
    (D.Nonempty → ∃ v : ℝ, ∀ N ∈ D,
      IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N ↔ sizeLikelihood x N p ν = v) ∧
    (ν = 0 → p ∈ Ioo (0 : ℝ) 1 →
      ∀ N ∈ D, IsMaxOn (fun J ↦ sizeLikelihood x J p ν) D N ↔ N = Finset.univ.sup x) := by
  intro D
  refine ⟨?_, ?_⟩
  · intro hD
    obtain ⟨N, hN, hM⟩ := (held_probability_dispersion_capped (B := B) x p ν).2.mp hD
    refine ⟨sizeLikelihood x N p ν, fun J hJ ↦ ⟨fun h ↦ le_antisymm (hM hJ) (h hN), ?_⟩⟩
    intro he K hK
    exact (hM hK).trans_eq he.symm
  · rintro rfl hp
    exact held_probability_zero_dispersion hn x hp hB

private theorem closed_excluded {N n : ℕ} (x : Fin n → Fin (N+1))
    (hz : ¬ ∀ i, x i = 0) (hf : ¬ ∀ i, x i = Fin.last N) {v : ℝ} (hv : 0 < v)
    (h : IsLUB (range (fun a : ℝ × ℝ ↦ likelihood x a.1 a.2)) v ∧
      ∀ a : ℝ × ℝ, likelihood x a.1 a.2 < v) :
    let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let L := fun q : ℝ × ℝ ↦ closedLikelihood x q.1 q.2
    IsLUB (L '' D) v ∧ ∀ q ∈ D, L q < v := by
  intro D L
  have hstrict (q : ℝ × ℝ) (hq : q ∈ D) : L q < v := by
    by_cases h₀ : q.1 = 0
    · simpa [L,closedLikelihood,h₀,hz] using hv
    by_cases h₁ : q.1 = 1
    · simpa [L,closedLikelihood,h₀,h₁,hf] using hv
    simpa [L,closedLikelihood,h₀,h₁] using h.2 (Real.log (q.1/(1-q.1)),q.2)
  refine ⟨⟨?_, ?_⟩, hstrict⟩
  · rintro _ ⟨q,hq,rfl⟩
    exact (hstrict q hq).le
  · intro w hw
    apply h.1.2
    rintro _ ⟨⟨η,ν⟩,rfl⟩
    let p := Real.exp η/(1+Real.exp η)
    have hp : p ∈ Ioo (0 : ℝ) 1 :=
      ⟨by dsimp [p]; positivity,
        (div_lt_one (show 0 < 1+Real.exp η by positivity)).mpr (by linarith)⟩
    have hη : Real.log (p/(1-p)) = η := by
      have hD : 0 < 1+Real.exp η := by positivity
      rw [show p/(1-p) = Real.exp η by dsimp [p]; field_simp [hD.ne']; ring, Real.log_exp]
    have he : L (p,ν) = likelihood x η ν := by
      simp [L,closedLikelihood,hp.1.ne',hp.2.ne,hη]
    change likelihood x η ν ≤ w
    rw [← he]
    exact hw ⟨(p,ν),⟨Ioo_subset_Icc_self hp,mem_univ _⟩,rfl⟩

/-- The three excluded fixed-size support patterns have saturated,
strictly unattained suprema over the full closed probability chart. -/
theorem held_size_failure {N n : ℕ} (hN : 2 ≤ N) (hn : 0 < n)
    (x : Fin n → Fin (N+1)) :
    let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let L := fun q : ℝ × ℝ ↦ closedLikelihood x q.1 q.2
    let Excluded := fun v : ℝ ↦ IsLUB (L '' D) v ∧ ∀ p ∈ D, L p < v
    (∀ k : Fin (N+1), 0 < k.val → k.val < N → (∀ i, x i = k) → Excluded 1) ∧
    (∀ l u : Fin (N+1), u.val = l.val+1 → (∀ i, x i = l ∨ x i = u) →
      0 < Discrete3CMB.multiplicity x l → 0 < Discrete3CMB.multiplicity x u →
      Excluded (pairHeight (Discrete3CMB.multiplicity x l) (Discrete3CMB.multiplicity x u))) ∧
    ((∀ i, x i = 0 ∨ x i = Fin.last N) →
      0 < Discrete3CMB.multiplicity x 0 → 0 < Discrete3CMB.multiplicity x (Fin.last N) →
      Excluded (pairHeight (Discrete3CMB.multiplicity x 0)
        (Discrete3CMB.multiplicity x (Fin.last N)))) := by
  classical
  have occurs {k : Fin (N+1)} (h : 0 < Discrete3CMB.multiplicity x k) :
      ∃ i, x i = k := by
    obtain ⟨i,hi⟩ := Finset.card_pos.mp h
    exact ⟨i,(Finset.mem_filter.mp hi).2⟩
  have hpair {l u : Fin (N+1)} (hne : l ≠ u)
      (hl : 0 < Discrete3CMB.multiplicity x l) (hu : 0 < Discrete3CMB.multiplicity x u) :
      (¬ ∀ i, x i = 0) ∧ (¬ ∀ i, x i = Fin.last N) ∧
        0 < pairHeight (Discrete3CMB.multiplicity x l) (Discrete3CMB.multiplicity x u) := by
    obtain ⟨i,hi⟩ := occurs hl
    obtain ⟨j,hj⟩ := occurs hu
    have diff (k : Fin (N+1)) (H : ∀ i, x i = k) : False :=
      hne ((hi.symm.trans (H i)).trans ((H j).symm.trans hj))
    have hp : 0 < (Discrete3CMB.multiplicity x l : ℝ) := Nat.cast_pos.mpr hl
    have hq : 0 < (Discrete3CMB.multiplicity x u : ℝ) := Nat.cast_pos.mpr hu
    exact ⟨diff _,diff _,by unfold pairHeight; positivity⟩
  refine ⟨?_, ?_, ?_⟩
  · intro k hk hk' hx
    have i : Fin n := ⟨0,hn⟩
    have hz : ¬ ∀ i, x i = 0 := by
      intro he
      have H : k.val = 0 := congrArg Fin.val ((hx i).symm.trans (he i))
      omega
    have hf : ¬ ∀ i, x i = Fin.last N := by
      intro he
      have H : k.val = N := congrArg Fin.val ((hx i).symm.trans (he i))
      omega
    exact closed_excluded x hz hf zero_lt_one (constant_interior_supremum hn x hk hk' hx)
  · intro l u hlu hx hl hu
    have hne : l ≠ u := by intro h; subst u; omega
    obtain ⟨hz,hf,hp⟩ := hpair hne hl hu
    exact closed_excluded x hz hf hp (adjacent_supremum hN x hlu hx hl hu)
  · intro hx hl hu
    have hne : (0 : Fin (N+1)) ≠ Fin.last N := by
      intro h
      have H : 0 = N := congrArg Fin.val h
      omega
    obtain ⟨hz,hf,hp⟩ := hpair hne hl hu
    exact closed_excluded x hz hf hp (endpoints_free_supremum hN x hx hl hu)

/-- At size zero the whole chart maximizes. At size one probability is
Bernoulli-fitted and dispersion is free. At larger sizes, endpoint atoms
leave dispersion free and every other attained fit is identifiable. -/
theorem held_size_maximizers {N n : ℕ} (hn : 0 < n) (x : Fin n → Fin (N+1)) :
    let D := Icc (0 : ℝ) 1 ×ˢ (univ : Set ℝ)
    let L := fun q : ℝ × ℝ ↦ closedLikelihood x q.1 q.2
    (N = 0 → ∀ q ∈ D, IsMaxOn L D q) ∧
    (N = 1 → ∀ q ∈ D, IsMaxOn L D q ↔
      q.1 = (Discrete3CMB.multiplicity x (Fin.last N) : ℝ) /
        (Discrete3CMB.multiplicity x (Fin.last N) + Discrete3CMB.multiplicity x 0)) ∧
    (2 ≤ N →
      ((∀ i, x i = 0) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 0) ∧
      ((∀ i, x i = Fin.last N) → ∀ q ∈ D, IsMaxOn L D q ↔ q.1 = 1) ∧
      ((¬ ∀ i, x i = 0) → (¬ ∀ i, x i = Fin.last N) →
        ((∃! q, q ∈ D ∧ IsMaxOn L D q) ↔
          FinishCMB.empirical x ∈ interior (convexHull ℝ (range (FinishCMB.statistic N)))))) := by
  classical
  intro D L
  refine ⟨?_, ?_, fun hN ↦ ⟨?_,?_,?_⟩⟩
  · rintro rfl q _ t _
    change closedLikelihood x t.1 t.2 ≤ closedLikelihood x q.1 q.2
    rw [size_zero_likelihood, size_zero_likelihood]
  · rintro rfl q hq
    have H := bernoulli_fit hn x q.2 hq.1
    rw [show Fin.last 1 = (1 : Fin 2) from rfl, ← H]
    constructor
    · intro hm t ht
      exact hm (show (t,q.2) ∈ D from ⟨ht,mem_univ _⟩)
    · intro hm t ht
      change closedLikelihood x t.1 t.2 ≤ closedLikelihood x q.1 q.2
      rw [bernoulli_likelihood x ht.1, ← bernoulli_likelihood x ht.1 q.2]
      exact hm ht.1
  · intro hx q hq
    have he : x = fun _ ↦ 0 := funext hx
    have hQ := Discrete4CMB.constant_saturated_iff (N := N) hn 0 hq.1 q.2
    rw [← he] at hQ
    have hH (t : ℝ × ℝ) (ht : t ∈ D) : L t ≤ 1 := by
      rw [show L t = closedLikelihood x t.1 t.2 from rfl, he]
      exact (Discrete4CMB.constant_saturated_iff (N := N) hn 0 ht.1 t.2).1
    have hL : L (0,0) = 1 := by simp [L,closedLikelihood,hx]
    have hf : L q = 1 ↔ q.1 = 0 := by
      simpa [show N ≠ 0 by omega, show (0 : ℕ) ≠ N by omega] using hQ.2
    exact ⟨fun hm ↦ hf.mp (le_antisymm (hH q hq)
      (hL ▸ hm (show ((0,0) : ℝ × ℝ) ∈ D from ⟨by norm_num,mem_univ _⟩))),
      fun h t ht ↦ (hH t ht).trans_eq (hf.mpr h).symm⟩
  · intro hx q hq
    have he : x = fun _ ↦ Fin.last N := funext hx
    have hQ := Discrete4CMB.constant_saturated_iff (N := N) hn (Fin.last N) hq.1 q.2
    rw [← he] at hQ
    have hH (t : ℝ × ℝ) (ht : t ∈ D) : L t ≤ 1 := by
      rw [show L t = closedLikelihood x t.1 t.2 from rfl, he]
      exact (Discrete4CMB.constant_saturated_iff (N := N) hn (Fin.last N) ht.1 t.2).1
    have hL : L (1,0) = 1 := by simp [L,closedLikelihood,hx]
    have hf : L q = 1 ↔ q.1 = 1 := by
      simpa [show N ≠ 0 by omega] using hQ.2
    exact ⟨fun hm ↦ hf.mp (le_antisymm (hH q hq)
      (hL ▸ hm (show ((1,0) : ℝ × ℝ) ∈ D from ⟨by norm_num,mem_univ _⟩))),
      fun h t ht ↦ (hH t ht).trans_eq (hf.mpr h).symm⟩
  · intro hzero hlast
    have hb := FinishCMB.held_size_closed_criterion hN hn x
    simp only [hzero, hlast, false_or] at hb
    change (∃ q ∈ D, IsMaxOn L D q) ↔ _ at hb
    refine ⟨fun h ↦ hb.mp (by obtain ⟨q,hq,_⟩ := h; exact ⟨q,hq⟩), ?_⟩
    intro h
    obtain ⟨q,hq,hQ⟩ := hb.mpr h
    refine ⟨q,⟨hq,hQ⟩,?_⟩
    have hℓ (η ν : ℝ) : 0 < likelihood x η ν := by
      unfold likelihood
      apply Finset.prod_pos
      intro i _
      exact div_pos (Real.exp_pos _) (Finset.sum_pos (fun _ _ ↦ Real.exp_pos _) Finset.univ_nonempty)
    let σ (η : ℝ) := Real.exp η/(1+Real.exp η)
    have hσ (η : ℝ) : σ η ∈ Ioo (0:ℝ) 1 :=
      ⟨by dsimp [σ]; positivity,
        (div_lt_one (show 0 < 1+Real.exp η by positivity)).mpr (by linarith)⟩
    have hσ' (η : ℝ) : Real.log (σ η / (1-σ η)) = η := by
      have hp : 0 < 1+Real.exp η := by positivity
      rw [show σ η/(1-σ η) = Real.exp η by dsimp [σ]; field_simp [hp.ne']; ring,
        Real.log_exp]
    have hchart {p : ℝ} (hp : p ∈ Ioo 0 1) (ν : ℝ) :
        L (p,ν) = likelihood x (Real.log (p/(1-p))) ν := by
      simp [L, closedLikelihood, hp.1.ne', hp.2.ne]
    have hnat (p : ℝ × ℝ) (hp : p ∈ D) (hP : IsMaxOn L D p) :
        p.1 ∈ Ioo 0 1 ∧
          IsMaxOn (fun θ : EuclideanSpace ℝ (Fin 2) ↦ likelihood x (θ 0) (θ 1)) univ
            (WithLp.toLp 2 ![Real.log (p.1/(1-p.1)),p.2]) := by
      have htest : 0 < L (σ 0,0) := by rw [hchart (hσ 0), hσ']; exact hℓ 0 0
      have hpos : 0 < L p := htest.trans_le
        (hP (show (σ 0,0) ∈ D from ⟨Ioo_subset_Icc_self (hσ 0),mem_univ _⟩))
      have hp' : p.1 ∈ Ioo 0 1 := by
        refine ⟨lt_of_le_of_ne hp.1.1 ?_, lt_of_le_of_ne hp.1.2 ?_⟩
        · intro he
          simp [L,closedLikelihood,← he,hzero] at hpos
        · intro he
          simp [L,closedLikelihood,he,hlast] at hpos
      refine ⟨hp',fun θ _ ↦ ?_⟩
      have H := hP (show (σ (θ 0),θ 1) ∈ D from
        ⟨Ioo_subset_Icc_self (hσ (θ 0)),mem_univ _⟩)
      change L (σ (θ 0),θ 1) ≤ L (p.1,p.2) at H
      rw [hchart (hσ _), hσ', hchart hp'] at H
      exact H
    rintro p ⟨hp,hP⟩
    obtain ⟨hp',hpM⟩ := hnat p hp hP
    obtain ⟨hq',hqM⟩ := hnat q hq hQ
    obtain ⟨a,_,ha⟩ := (FinishCMB.held_size_open_hull hN hn x).mpr h
    have he := (ha _ hpM).trans (ha _ hqM).symm
    have he₀ := congrArg (fun t : EuclideanSpace ℝ (Fin 2) ↦ t 0) he
    have he₁ := congrArg (fun t : EuclideanSpace ℝ (Fin 2) ↦ t 1) he
    change Real.log (p.1/(1-p.1)) = Real.log (q.1/(1-q.1)) at he₀
    have H := Real.log_injOn_pos (div_pos hp'.1 (sub_pos.mpr hp'.2))
      (div_pos hq'.1 (sub_pos.mpr hq'.2)) he₀
    have he' := (div_eq_div_iff (sub_pos.mpr hp'.2).ne' (sub_pos.mpr hq'.2).ne').mp H
    exact Prod.ext (by linarith [show p.1*q.1 = q.1*p.1 from mul_comm ..]) he₁

end UnivariateML.Base8CMB

end
