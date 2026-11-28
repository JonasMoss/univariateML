module

public import UnivariateML.Family.Base7NHyper
public import UnivariateML.Family.Discrete4Hyper
import Mathlib.Topology.Sequences

set_option linter.style.header false

/-! Negative hypergeometric population limits.

## Deviations
The limit is along a fixed stopping count and a convergent marked proportion.
The rational residue comparison and the all-free maximizing escape
classification are separate steps.
-/

@[expose] public section

namespace UnivariateML.Base7bNHyper

open Filter Set Discrete4NHyper
open scoped Topology

private theorem stop_mass {N m r t : ℕ} (hr : 0 < r) :
    totalMass N m r t = ((r : ℝ) / t) * Hypergeometric.mass m N t r := by
  classical
  have H : Compatible N m r t ↔ Hypergeometric.Compatible m N t r := by
    simp only [Compatible, ite_eq_right hr.ne', Hypergeometric.Compatible]
    omega
  by_cases hc : Compatible N m r t
  · have hs := H.mp hc
    have hrt := hs.2.2.2.1
    have ht : r + (t - r) = t := Nat.add_sub_of_le hrt
    have hd (a b : ℕ) : (a.descFactorial b : ℝ) = b.factorial * (a.choose b : ℝ) := by
      rw [Nat.descFactorial_eq_factorial_mul_choose, Nat.cast_mul]
    have hfact : ((t - 1).choose (r - 1) : ℝ) * r.factorial * (t - r).factorial * t =
        r * t.factorial := by
      have H := Nat.choose_mul_factorial_mul_factorial (show r - 1 ≤ t - 1 by omega)
      have E : ((t - 1).choose (r - 1) : ℝ) * (r - 1).factorial * (t - r).factorial =
          (t - 1).factorial := by
        exact_mod_cast (show (t - 1).choose (r - 1) * (r - 1).factorial * (t - r).factorial =
          (t - 1).factorial by simpa only [show t - 1 - (r - 1) = t - r by omega] using H)
      have Hr : (r.factorial : ℝ) = r * (r - 1).factorial := by
        conv_lhs => rw [show r = (r - 1) + 1 by omega, Nat.factorial_succ, Nat.cast_mul]
        rw [Nat.sub_add_cancel hr]
      have Ht : (t.factorial : ℝ) = t * (t - 1).factorial := by
        conv_lhs => rw [show t = (t - 1) + 1 by omega, Nat.factorial_succ, Nat.cast_mul]
        rw [Nat.sub_add_cancel (show 1 ≤ t by omega)]
      rw [Hr, Ht]
      linear_combination ((r : ℝ) * t) * E
    rw [totalMass, ite_eq_left hc, NegHypergeometric.mass, NegHypergeometric.reducedMass,
      Nat.add_sub_of_le hc.1, ht, hd, hd, hd, Hypergeometric.mass_choose hs]
    have hne : (N.choose t : ℝ) ≠ 0 := (Nat.cast_pos.mpr (Nat.choose_pos hs.2.1)).ne'
    have htR : (t : ℝ) ≠ 0 := (Nat.cast_pos.mpr (hr.trans_le hrt)).ne'
    field_simp [hne, htR]
    linear_combination ((m.choose r : ℝ) * ((N - m).choose (t - r) : ℝ)) * hfact
  · simp [totalMass, hc, Hypergeometric.mass, mt H.mpr hc]

/-- At fixed positive stopping count, population divergence with convergent
marked proportion gives the shifted negative-binomial likelihood, including
both endpoint proportions. The binomial coefficient is written as
`(r / xᵢ) * choose xᵢ r` to keep the stopping probability explicit. -/
theorem held_stop_face_limit {h r : ℕ} (hr : 0 < r) (x : Fin h → ℕ)
    {m N : ℕ → ℕ} {p : ℝ} (hp : p ∈ Icc 0 1) (hN : Tendsto N atTop atTop)
    (hm : ∀ᶠ j in atTop, m j ≤ N j)
    (hprop : Tendsto (fun j ↦ (m j : ℝ) / N j) atTop (𝓝 p)) :
    Tendsto (fun j ↦ totalLikelihood x (N j) (m j) r) atTop
      (𝓝 (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p)) := by
  apply tendsto_finsetProd
  intro i _
  have H := Discrete4Hyper.held_draw_binomial_limit (fun _ : Fin 1 ↦ r) (x i) hp hN hm hprop
  have H' : Tendsto (fun j ↦ Hypergeometric.mass (m j) (N j) (x i) r) atTop
      (𝓝 (Discrete4Hyper.binomialFace (x i) r p)) := by
    simpa [Hypergeometric.likelihood] using H
  simpa only [stop_mass hr] using H'.const_mul ((r : ℝ) / x i)

private theorem marked_proportion_mem {N m : ℕ} (hm : m ≤ N) :
    (m : ℝ) / N ∈ Icc (0 : ℝ) 1 := by
  refine ⟨by positivity, ?_⟩
  by_cases hz : N = 0
  · simp [hz]
  · exact (div_le_one (Nat.cast_pos.mpr (Nat.pos_of_ne_zero hz))).mpr (by exact_mod_cast hm)

/-- Every shifted negative-binomial face at fixed positive stopping count
is reachable by compatible urns, including the two endpoint proportions. -/
theorem held_stop_face_reachable {h r : ℕ} (hr : 0 < r) (x : Fin h → ℕ)
    (hx : ∀ i, r ≤ x i) {p : ℝ} (hp : p ∈ Icc 0 1) :
    ∃ m N : ℕ → ℕ, Tendsto N atTop atTop ∧
      (∀ t i, Compatible (N t) (m t) r (x i)) ∧
      Tendsto (fun t ↦ (m t : ℝ) / N t) atTop (𝓝 p) ∧
      Tendsto (fun t ↦ totalLikelihood x (N t) (m t) r) atTop
        (𝓝 (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p)) := by
  let T := max r (Finset.univ.sup x)
  have hT (i : Fin h) : x i ≤ T := (Finset.le_sup (Finset.mem_univ i)).trans (le_max_right _ _)
  obtain ⟨m, N, hN, hm, hprop, -⟩ :=
    Discrete4Hyper.held_draw_reaches_binomial_face (fun _ : Fin 1 ↦ r) T
      (fun _ ↦ le_max_left _ _) hp
  have hcomp (t : ℕ) (i : Fin h) : Compatible (N t) (m t) r (x i) := by
    have H := hm t 0
    have H' := hT i
    have H'' := hx i
    simp only [Hypergeometric.Compatible] at H
    simp [Compatible, hr.ne']
    omega
  exact ⟨m, N, hN, hcomp, hprop, held_stop_face_limit hr x hp hN
    (Eventually.of_forall fun t ↦ (hm t 0).1) hprop⟩

/-- Above every fixed-stop face, all compositions of sufficiently large
populations are excluded. This is a uniform cutoff for a strict finite
witness. The proof uses compactness of marked proportions. -/
theorem held_stop_uniform_cutoff {h r : ℕ} (hh : 0 < h) (hr : 0 < r) (x : Fin h → ℕ)
    {B : ℝ} (hB : ∀ p ∈ Icc (0 : ℝ) 1,
      (∏ i, ((r : ℝ) / x i) * Discrete4Hyper.binomialFace (x i) r p) < B) :
    ∃ N₀ : ℕ, ∀ N ≥ N₀, ∀ m, (∀ i, Compatible N m r (x i)) →
      totalLikelihood x N m r < B := by
  by_contra! H
  choose N hN m hc hB' using H
  have ht : Tendsto N atTop atTop :=
    tendsto_atTop_mono (show ∀ j : ℕ, id j ≤ N j from hN) tendsto_id
  have hm (j : ℕ) : m j ≤ N j := (hc j ⟨0, hh⟩).1
  obtain ⟨p, hp, f, hf, hprop⟩ :=
    isCompact_Icc.tendsto_subseq (fun j ↦ marked_proportion_mem (hm j))
  have hlim := held_stop_face_limit hr x hp (ht.comp hf.tendsto_atTop)
    (Eventually.of_forall fun t ↦ hm (f t)) hprop
  exact (hB p hp).not_ge (ge_of_tendsto' hlim fun t ↦ hB' (f t))

private theorem fixed_stop_of_compatible {h : ℕ} {x : Fin h → ℕ}
    {i : Fin h} (hi : 0 < x i) {N m r : ℕ}
    (hc : Compatible N m r (x i)) : r ∈ Finset.Icc 1 (x i) := by
  have hr : r ≠ 0 := by
    intro e
    have := hc.2.2
    simp [e, hi.ne'] at this
  exact Finset.mem_Icc.mpr ⟨Nat.pos_of_ne_zero hr, (by
    have H := hc.2.2
    rw [ite_eq_right hr] at H
    exact H.1)⟩

/-- A maximizing population escape on positive nonconstant data has a
subsequence with fixed stopping count and an interior marked proportion.
Its shifted negative-binomial height equals the greatest slice supremum,
including when finite urns also reach that height. -/
theorem positive_nonconstant_maximizing_escape {h : ℕ} (x : Fin h → ℕ)
    {i : Fin h} (_hi : ∀ j, x i ≤ x j) (hx : 0 < x i)
    (hne : ∃ j, x i < x j)
    (N m r : ℕ → ℕ) (hc : ∀ t j, Compatible (N t) (m t) (r t) (x j))
    (hN : Tendsto N atTop atTop) {L : ℝ} (hL : 0 < L)
    (hl : Tendsto (fun t ↦ totalLikelihood x (N t) (m t) (r t)) atTop (𝓝 L))
    (hwin : ∀ A B C, (∀ j, Compatible A B C (x j)) → totalLikelihood x A B C ≤ L) :
    ∃ s ∈ Finset.Icc 1 (x i), ∃ p ∈ Ioo (0 : ℝ) 1, ∃ f : ℕ → ℕ,
      Tendsto f atTop atTop ∧ (∀ t, r (f t) = s) ∧
      Tendsto (fun t ↦ (m (f t) : ℝ) / N (f t)) atTop (𝓝 p) ∧
      L = (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) ∧
      IsLUB {v | ∃ A B, (∀ j, Compatible A B s (x j)) ∧
        v = totalLikelihood x A B s} L := by
  have he : ∀ᶠ t in atTop, ∃ s ∈ Finset.Icc 1 (x i), r t = s :=
    Eventually.of_forall fun t ↦ ⟨r t, fixed_stop_of_compatible hx (hc t i), rfl⟩
  obtain ⟨s, hs, hfreq⟩ := (Finset.frequently_exists _).mp he.frequently
  obtain ⟨g, hg, hgs⟩ := exists_seq_forall_of_frequently hfreq
  have hcomp (t : ℕ) : (m (g t) : ℝ) / N (g t) ∈ Icc (0 : ℝ) 1 :=
    marked_proportion_mem (hc (g t) i).1
  obtain ⟨p, hp, a, ha, hpa⟩ := isCompact_Icc.tendsto_subseq hcomp
  let f := g ∘ a
  have hf : Tendsto f atTop atTop := hg.comp ha.tendsto_atTop
  have hfs (t : ℕ) : r (f t) = s := hgs (a t)
  have hspos : 0 < s := (Finset.mem_Icc.mp hs).1
  have hface := held_stop_face_limit hspos x hp (hN.comp hf)
    (Eventually.of_forall fun t ↦ (hc (f t) i).1) hpa
  have hls : Tendsto (fun t ↦ totalLikelihood x (N (f t)) (m (f t)) s) atTop (𝓝 L) :=
    (hl.comp hf).congr fun t ↦ by simp [Function.comp_apply, hfs]
  have hEq := tendsto_nhds_unique hls hface
  have hpi : p ∈ Ioo (0 : ℝ) 1 := by
    refine ⟨lt_of_le_of_ne hp.1 ?_, lt_of_le_of_ne hp.2 ?_⟩
    · intro hz
      have hz' : p = 0 := hz.symm
      have H : (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) = 0 := by
        apply Finset.prod_eq_zero (Finset.mem_univ i)
        simp [hz', Discrete4Hyper.binomialFace, hspos.ne']
      linarith
    · intro hz
      obtain ⟨j, hj⟩ := hne
      have hsub : x j - s ≠ 0 := by
        have := (Finset.mem_Icc.mp hs).2
        omega
      have H : (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) = 0 := by
        apply Finset.prod_eq_zero (Finset.mem_univ j)
        simp [hz, Discrete4Hyper.binomialFace, hsub]
      linarith
  refine ⟨s, hs, p, hpi, f, hf, hfs, hpa, hEq, ?_, ?_⟩
  · rintro _ ⟨A, B, hab, rfl⟩
    exact hwin A B s hab
  · intro U hU
    apply le_of_tendsto' hls
    intro t
    exact hU ⟨N (f t), m (f t), fun j ↦ hfs t ▸ hc (f t) j, rfl⟩

private theorem positive_constant_escape {h c : ℕ} (hh : 0 < h) (hc : 0 < c)
    (N m r : ℕ → ℕ) (hvalid : ∀ t, Compatible (N t) (m t) (r t) c)
    (hN : Tendsto N atTop atTop)
    (hl : Tendsto (fun t ↦ totalLikelihood (fun _ : Fin h ↦ c) (N t) (m t) (r t)) atTop (𝓝 1)) :
    (∀ᶠ t in atTop, r t = c) ∧ Tendsto (fun t ↦ (m t : ℝ) / N t) atTop (𝓝 1) := by
  have hcr : 0 < (c : ℝ) := by exact_mod_cast hc
  let b := ((c : ℝ) - 1) / c
  have hb : 0 ≤ b := by
    exact div_nonneg (sub_nonneg.mpr (by exact_mod_cast hc)) hcr.le
  have hb' : b ^ h < 1 := pow_lt_one₀ hb ((div_lt_one hcr).mpr (by linarith)) hh.ne'
  have hr : ∀ᶠ t in atTop, r t = c := by
    filter_upwards [hl.eventually (Ioi_mem_nhds hb')] with t ht
    by_contra hne
    have H := fixed_stop_of_compatible (x := fun _ : Fin h ↦ c) (i := ⟨0, hh⟩) hc (hvalid t)
    have hrpos : 0 < r t := (Finset.mem_Icc.mp H).1
    have hrlt : r t < c := (Finset.mem_Icc.mp H).2.lt_of_ne hne
    have hnon : 0 ≤ totalMass (N t) (m t) (r t) c := by
      unfold totalMass NegHypergeometric.mass NegHypergeometric.reducedMass
      split <;> positivity
    have hle : totalMass (N t) (m t) (r t) c ≤ b := by
      have hup := (Discrete4Hyper.constant_attains_saturated (h := 1) (r t)).2 (m t) (N t) c
      rw [(Discrete4Hyper.constant_attains_saturated (h := 1) (r t)).1] at hup
      have hmass : Hypergeometric.mass (m t) (N t) c (r t) ≤ 1 := by
        simpa [Hypergeometric.likelihood] using hup
      have he : (r t : ℝ) ≤ (c : ℝ) - 1 := by
        have := Nat.succ_le_of_lt hrlt
        have H : (r t : ℝ) + 1 ≤ c := by exact_mod_cast this
        linarith
      rw [stop_mass hrpos]
      exact (mul_le_of_le_one_right (by positivity) hmass).trans
        (div_le_div_of_nonneg_right he hcr.le)
    have hpow := pow_le_pow_left₀ hnon hle h
    have H' : totalLikelihood (fun _ : Fin h ↦ c) (N t) (m t) (r t) ≤ b ^ h := by
      simpa [totalLikelihood] using hpow
    exact ht.not_ge H'
  refine ⟨hr, tendsto_order.2 ⟨?_, ?_⟩⟩
  · intro a ha
    by_contra hbad
    have H : ∃ᶠ t in atTop, (m t : ℝ) / N t ∈ Icc (0 : ℝ) a := by
      have hf := not_eventually.mp hbad
      exact hf.mono fun t ht ↦ ⟨by positivity, le_of_not_gt ht⟩
    obtain ⟨p, hp, f, hf, hp'⟩ := isCompact_Icc.tendsto_subseq' H
    have hpI : p ∈ Icc (0 : ℝ) 1 := ⟨hp.1, hp.2.trans ha.le⟩
    have hff := hf.tendsto_atTop
    have hc' := held_stop_face_limit hc (fun _ : Fin h ↦ c) hpI (hN.comp hff)
      (Eventually.of_forall fun t ↦ (hvalid (f t)).1) hp'
    have he : Tendsto (fun t ↦ totalLikelihood (fun _ : Fin h ↦ c) (N (f t)) (m (f t)) c)
        atTop (𝓝 1) := by
      apply (hl.comp hff).congr'
      filter_upwards [hff.eventually hr] with t ht
      simp only [Function.comp_apply, ht]
    have HE := tendsto_nhds_unique he hc'
    have hpow := pow_lt_one₀ hp.1 (hp.2.trans_lt ha) hc.ne'
    have hpow' := pow_lt_one₀ (pow_nonneg hp.1 _) hpow hh.ne'
    simp [Discrete4Hyper.binomialFace, div_self hcr.ne'] at HE
    exact hpow'.ne HE.symm
  · intro a ha
    exact Eventually.of_forall fun t ↦ (marked_proportion_mem (hvalid t).1).2.trans_lt ha

private theorem positive_likelihood_witness {h : ℕ} (x : Fin h → ℕ) (hx : ∀ i, 0 < x i) :
    ∃ N, (∀ i, Compatible N 1 1 (x i)) ∧ 0 < totalLikelihood x N 1 1 := by
  let N := max 1 (Finset.univ.sup x)
  have hN (i : Fin h) : x i ≤ N := (Finset.le_sup (Finset.mem_univ i)).trans (le_max_right _ _)
  have hN' : 1 ≤ N := le_max_left _ _
  refine ⟨N, fun i ↦ ⟨hN', le_rfl, ?_⟩, ?_⟩
  · exact ⟨hx i, Nat.sub_le_sub_right (hN i) 1⟩
  · apply Finset.prod_pos
    intro i _
    rw [stop_mass Nat.one_pos]
    have hcomp : Hypergeometric.Compatible 1 N (x i) 1 := by
      unfold Hypergeometric.Compatible
      have := hN i
      have := hx i
      omega
    rw [Hypergeometric.mass_choose hcomp]
    have hdiff : x i - 1 ≤ N - 1 := Nat.sub_le_sub_right (hN i) 1
    exact mul_pos (div_pos (by norm_num) (Nat.cast_pos.mpr (hx i)))
      (div_pos (mul_pos (Nat.cast_pos.mpr (Nat.choose_pos le_rfl))
        (Nat.cast_pos.mpr (Nat.choose_pos hdiff))) (Nat.cast_pos.mpr (Nat.choose_pos (hN i))))

/-- The full branch classification of a maximizing population escape.
Mixed zeros and positive counts are infeasible. A positive constant
requires its own stopping count eventually and marked fraction tending to
one. A positive nonconstant sample has an interior shifted negative-binomial
face at the highest slice height. All compatible zero paths are flat. -/
theorem all_free_maximizing_escape {h : ℕ} (hh : 0 < h) (x : Fin h → ℕ) :
    ((∃ i j, x i = 0 ∧ 0 < x j) → ¬ ∃ N m r, ∀ k, Compatible N m r (x k)) ∧
    (∀ (N m r : ℕ → ℕ), (∀ t i, Compatible (N t) (m t) (r t) (x i)) →
      Tendsto N atTop atTop → ∀ L : ℝ,
      Tendsto (fun t ↦ totalLikelihood x (N t) (m t) (r t)) atTop (𝓝 L) →
      (∀ A B C, (∀ j, Compatible A B C (x j)) → totalLikelihood x A B C ≤ L) →
      ((∀ i, x i = 0) ∧ (∀ t, r t = 0) ∧ L = 1) ∨
      (∃ c > 0, (∀ i, x i = c) ∧ L = 1 ∧ (∀ᶠ t in atTop, r t = c) ∧
        Tendsto (fun t ↦ (m t : ℝ) / N t) atTop (𝓝 1)) ∨
      (∃ i, (∀ j, x i ≤ x j) ∧ 0 < x i ∧ (∃ j, x i < x j) ∧
        ∃ s ∈ Finset.Icc 1 (x i), ∃ p ∈ Ioo (0 : ℝ) 1, ∃ f : ℕ → ℕ,
          Tendsto f atTop atTop ∧ (∀ t, r (f t) = s) ∧
          Tendsto (fun t ↦ (m (f t) : ℝ) / N (f t)) atTop (𝓝 p) ∧
          L = (∏ j, ((s : ℝ) / x j) * Discrete4Hyper.binomialFace (x j) s p) ∧
          IsLUB {v | ∃ A B, (∀ j, Compatible A B s (x j)) ∧
            v = totalLikelihood x A B s} L)) := by
  refine ⟨?_, ?_⟩
  · rintro ⟨i, j, hi, hj⟩
    exact mixed_zero_infeasible x hi hj
  · intro N m r hc hN L hl hwin
    obtain ⟨i, -, hi⟩ := Finset.exists_min_image Finset.univ x
      ⟨⟨0, hh⟩, Finset.mem_univ _⟩
    have hmin (j : Fin h) : x i ≤ x j := hi j (Finset.mem_univ _)
    by_cases hz : x i = 0
    · have hx (j : Fin h) : x j = 0 := by
        by_contra he
        exact mixed_zero_infeasible x hz (Nat.pos_of_ne_zero he) ⟨N 0, m 0, r 0, hc 0⟩
      have hr (t : ℕ) : r t = 0 := by
        by_contra he
        have H := (hc t i).2.2
        simp [he, hz] at H
      have hL : (fun t ↦ totalLikelihood x (N t) (m t) (r t)) = fun _ ↦ (1 : ℝ) := by
        funext t
        rw [hr]
        exact held_zero_stop_flat x (hc t i).1 hx
      exact Or.inl ⟨hx, hr, tendsto_nhds_unique (hL ▸ hl) tendsto_const_nhds⟩
    · have hpos := Nat.pos_of_ne_zero hz
      by_cases hconst : ∀ j, x j = x i
      · let c := x i
        have hxc : x = fun _ ↦ c := funext hconst
        have H := constant_attains_saturated (h := h) c
        have h1 : 1 ≤ L := by
          have Hc (j : Fin h) : Compatible c c c (x j) := by
            rw [hconst j]
            simp [Compatible, c, hz]
          have h := hwin c c c Hc
          rw [hxc, H.1] at h
          exact h
        have h2 : L ≤ 1 := by
          apply le_of_tendsto' hl
          intro t
          rw [hxc]
          exact H.2 _ _ _
        have he : L = 1 := h2.antisymm h1
        have hc' (t : ℕ) : Compatible (N t) (m t) (r t) c := hc t i
        have hl' : Tendsto (fun t ↦ totalLikelihood (fun _ : Fin h ↦ c) (N t) (m t) (r t))
            atTop (𝓝 1) := by
          rw [← hxc, ← he]
          exact hl
        exact Or.inr (Or.inl ⟨c, hpos, hconst, he, positive_constant_escape hh hpos N m r hc' hN hl'⟩)
      · have hne : ∃ j, x i < x j := by
          push Not at hconst
          obtain ⟨j, hj⟩ := hconst
          exact ⟨j, (hmin j).lt_of_ne hj.symm⟩
        obtain ⟨A, hA, HA⟩ := positive_likelihood_witness x (fun j ↦ hpos.trans_le (hmin j))
        have hL := HA.trans_le (hwin A 1 1 hA)
        exact Or.inr (Or.inr ⟨i, hmin, hpos, hne,
          positive_nonconstant_maximizing_escape x hmin hpos hne N m r hc hN hL hl hwin⟩)

end UnivariateML.Base7bNHyper

end
