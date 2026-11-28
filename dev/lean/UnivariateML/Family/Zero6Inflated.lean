module

public import UnivariateML.Family.Trunc3Zero
public import Mathlib.Analysis.Calculus.Deriv.Pow

/-! Exact inflation profiles and fixed-design attainment. The free profile
includes both sides of the seam. The fixed-design likelihoods suppress
positive, parameter-independent carrier factors where the parent does. -/

@[expose] public section

open Set Filter Real
open scoped Topology

namespace UnivariateML.Zero6Inflated

open Truncation TruncZero

noncomputable section

def effective (u rho : ℝ) : ℝ := rho + (1-rho)*u

private theorem effective_mem {u rho : ℝ} (hu : u ∈ Icc (0 : ℝ) 1) (hr : rho ∈ Icc 0 1) :
    effective u rho ∈ Icc 0 1 := by
  unfold effective
  exact ⟨add_nonneg hr.1 (mul_nonneg (sub_nonneg.mpr hr.2) hu.1), by
    nlinarith [mul_nonneg (sub_nonneg.mpr hr.2) (sub_nonneg.mpr hu.2)]⟩

/-- The likelihood is polynomial in the observed masses and the weight,
including at the degenerate parent zero atom. -/
def likelihood {P : Type*} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    (mass : P → ℕ → ℝ) (t : P × ℝ) : ℝ :=
  effective (mass t.1 0) t.2 ^ z * (∏ i, (1-t.2)*mass t.1 (x i))

theorem face_limit {P : Type*} {F : Filter P} {k : ℕ} (z : ℕ) (x : Fin k → ℕ)
    {mass : P → ℕ → ℝ} {limit : ℕ → ℝ} {rho : P → ℝ} {r : ℝ}
    (h : ∀ j, Tendsto (fun p ↦ mass p j) F (𝓝 (limit j)))
    (hr : Tendsto rho F (𝓝 r)) :
    Tendsto (fun p ↦ likelihood z x mass (p,rho p)) F
      (𝓝 (likelihood z x (fun _ : Unit ↦ limit) ((),r))) := by
  unfold likelihood effective
  exact ((hr.add ((tendsto_const_nhds.sub hr).mul (h 0))).pow z).mul
    (tendsto_finsetProd _ fun i _ ↦ (tendsto_const_nhds.sub hr).mul (h (x i)))

/-- Both branches of the free profile, compared on their own feasible
sets. When a positive witness exists, every maximizing pair realizes
the clipped weight and maximizes the joined profile. -/
theorem free_profile {P : Type*} {D : Set P} {z k : ℕ} (hn : 0 < z+k)
    {u L : P → ℝ} (hu : ∀ q ∈ D, u q ∈ Ico 0 1)
    (hL : ∀ q ∈ D, 0 < L q)
    {p : P} {r : ℝ} (hp : p ∈ D) (hr : r ∈ Icc 0 1) :
    let v := zeroFraction z k
    let F := fun t : P × ℝ ↦ bernoulli z k (effective (u t.1) t.2) * L t.1
    IsMaxOn F (D ×ˢ Icc 0 1) (p,r) ↔
    effective (u p) r = max (u p) v ∧
      (∀ q ∈ D, u q ≤ v → bernoulli z k v * L q ≤ F (p,r)) ∧
      (∀ q ∈ D, v ≤ u q → bernoulli z k (u q) * L q ≤ F (p,r)) := by
  dsimp only
  let v := zeroFraction z k
  let F := fun t : P × ℝ ↦ bernoulli z k (effective (u t.1) t.2) * L t.1
  have hv' : v ∈ Icc 0 1 := by
    unfold v zeroFraction
    have H : (0 : ℝ) < z+k := by exact_mod_cast hn
    exact ⟨div_nonneg (Nat.cast_nonneg _) H.le,
      (div_le_one H).mpr (by linarith [Nat.cast_nonneg (α := ℝ) k])⟩
  have hform (a b : ℝ) : effective a b = a + (1-a)*b := by unfold effective; ring
  have hopt (q : P) (hq : q ∈ D) :
      ∃ s ∈ Icc (0 : ℝ) 1, effective (u q) s = max (u q) v ∧
        ∀ t ∈ Icc (0 : ℝ) 1, F (q,t) ≤ F (q,s) := by
    have huq := hu q hq
    have H : max (u q) v ∈ Icc (u q) 1 :=
      ⟨le_max_left _ _, max_le huq.2.le hv'.2⟩
    obtain ⟨s, hs, he⟩ := (inflation_weight_iff huq.1 huq.2).mpr H
    refine ⟨s,hs,(hform _ _).trans he.symm,?_⟩
    have hm := (Trunc3Zero.inflated_clip hn huq.1 huq.2 hs (hL q hq)).mpr he.symm
    intro t ht
    have H := hm ht
    change bernoulli z k (u q + (1-u q)*t) * L q ≤
      bernoulli z k (u q + (1-u q)*s) * L q at H
    simpa only [F, hform] using H
  change IsMaxOn F _ _ ↔ _
  constructor
  · intro hm
    have hfix : IsMaxOn (fun s ↦ F (p,s)) (Icc 0 1) r :=
      fun _ hs ↦ hm ⟨hp,hs⟩
    have hclip := (Trunc3Zero.inflated_clip hn (hu p hp).1 (hu p hp).2 hr (hL p hp)).mp
      (by simpa only [F, hform] using hfix)
    refine ⟨(hform _ _).trans hclip, ?_, ?_⟩
    · intro q hq hle
      obtain ⟨s, hs, he, _⟩ := hopt q hq
      have H : F (q,s) ≤ F (p,r) := hm ⟨hq,hs⟩
      simpa only [F, he, max_eq_right hle, v] using H
    · intro q hq hle
      obtain ⟨s, hs, he, _⟩ := hopt q hq
      have H : F (q,s) ≤ F (p,r) := hm ⟨hq,hs⟩
      simpa only [F, he, max_eq_left hle, v] using H
  · rintro ⟨_, ha, hb⟩ ⟨q,t⟩ ⟨hq,ht⟩
    obtain ⟨s, _, he, hs⟩ := hopt q hq
    refine (hs t ht).trans ?_
    rcases le_total (u q) v with h | h
    · simpa only [F, he, max_eq_right h] using ha q hq h
    · simpa only [F, he, max_eq_left h] using hb q hq h

/-- The held-weight score and the derivative shared by the two branches
at an interior empirical seam. The log Bernoulli multiplier is supplied
with its derivative at the evaluation point. -/
theorem held_score {T u B : ℝ → ℝ} {p t' u' b' rho : ℝ}
    (hT : HasDerivAt T t' p) (hu : HasDerivAt u u' p)
    (hB : HasDerivAt B b' (effective (u p) rho)) :
    HasDerivAt (fun q ↦ T q + B (effective (u q) rho))
      (t' + b' * ((1-rho)*u')) p := by
  have h : HasDerivAt (fun q ↦ effective (u q) rho) ((1-rho)*u') p :=
    (hu.const_mul (1-rho)).const_add rho
  exact hT.add (hB.comp p h)

theorem seam_scores {T u B : ℝ → ℝ} {p t' u' v : ℝ} (he : u p = v)
    (hT : HasDerivAt T t' p) (hu : HasDerivAt u u' p)
    (hB : HasDerivAt B 0 v) :
    HasDerivAt (fun q ↦ T q + B v) t' p ∧
      HasDerivAt (fun q ↦ T q + B (u q)) t' p := by
  refine ⟨hT.add_const _, ?_⟩
  have hb : HasDerivAt B 0 (u p) := he ▸ hB
  have H := hT.add (hb.comp p hu)
  rw [zero_mul, add_zero] at H
  exact H

/-- Criterion and failure for held Poisson inflation on all zeros.
All positive rates tie at inflation one. Below one the excluded zero
atom gives height one, strictly above every finite rate. -/
theorem pois_held_zeros {z : ℕ} (hz : 0 < z) {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    let F := fun r : ℝ ↦ inflatedPoisson z 0 0 (r,rho)
    (∀ r ∈ Ioi 0, IsMaxOn F (Ioi 0) r ↔ rho = 1) ∧
    (rho < 1 → (∀ r ∈ Ioi 0, F r < 1) ∧ Tendsto F (𝓝[>] 0) (𝓝 1)) := by
  have he (r : ℝ) : inflatedPoisson z 0 0 (r,rho) = effective (exp (-r)) rho ^ z := by
    simp [inflatedPoisson, effective]
  have hstrict (r : ℝ) (h : r ∈ Ioi 0) (hr' : rho < 1) :
      inflatedPoisson z 0 0 (r,rho) < 1 := by
    rw [he]
    have hE := effective_mem ⟨(exp_pos _).le, (exp_lt_one_iff.mpr (neg_lt_zero.mpr h)).le⟩ hr
    apply pow_lt_one₀ hE.1 _ hz.ne'
    unfold effective
    nlinarith [mul_pos (sub_pos.mpr hr') (sub_pos.mpr (exp_lt_one_iff.mpr (neg_lt_zero.mpr h)))]
  refine ⟨?_, fun h ↦ ⟨fun r hr ↦ hstrict r hr h, ?_⟩⟩
  · intro r h
    constructor
    · intro hm
      by_contra hn
      have hr' := lt_of_le_of_ne hr.2 hn
      have hhalf : r/2 ∈ Ioi (0 : ℝ) := by exact half_pos h
      have hle := hm hhalf
      simp only [he] at hle
      have hpos : 0 < r := h
      exact (exp_lt_exp.mpr (by linarith : -r < -(r/2))).not_ge
        ((Trunc3Zero.inflated_zero_held hz (exp_pos _).le (exp_pos _).le ⟨hr.1,hr'⟩).mp hle)
    · rintro rfl q hq
      simp [he, effective]
  · have H : Continuous (fun r : ℝ ↦ effective (exp (-r)) rho ^ z) := by
      unfold effective
      fun_prop
    have H' := (H.continuousAt (x := (0 : ℝ))).tendsto
    change Tendsto _ _ (𝓝 (effective (exp (-(0 : ℝ))) rho ^ z)) at H'
    rw [show effective (exp (-(0 : ℝ))) rho ^ z = 1 by
      simp only [neg_zero, exp_zero, effective, mul_one, add_sub_cancel, one_pow]] at H'
    simpa only [he] using H'.mono_left (nhdsWithin_le_nhds (s := Ioi (0 : ℝ)))

/-- Pois, free or held pi, zero:inflated criterion. For positive data
the sum of positive counts is positive and the compatible inflation
slice contains a weight below one. With all zeros, attainment requires
that the inflation slice contain one. -/
theorem pois_inflated {z k S : ℕ} (hn : 0 < z+k)
    (hS : S = 0 ↔ k = 0) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) :
    let D := Ioi (0 : ℝ) ×ˢ W
    let F := inflatedPoisson z k S
    (∃ p ∈ D, IsMaxOn F D p ∧ 0 < F p) ↔
      (k = 0 ∧ (1 : ℝ) ∈ W) ∨ (0 < k ∧ ∃ r ∈ W, r < 1) := by
  dsimp only
  by_cases hk : k = 0
  · have hz : 0 < z := by omega
    have hzero := hS.mpr hk
    subst k
    subst S
    simp only [true_and, lt_self_iff_false, false_and, or_false]
    constructor
    · rintro ⟨⟨p,r⟩,hp,hm,_⟩
      have hr := (pois_held_zeros hz (hvalid hp.2)).1 p hp.1 |>.mp
        (fun q hq ↦ hm (a := (q,r)) ⟨hq,hp.2⟩)
      exact hr ▸ hp.2
    · intro h
      refine ⟨(1,1),⟨zero_lt_one,h⟩,?_,by simp [inflatedPoisson]⟩
      rintro ⟨p,r⟩ hp
      change inflatedPoisson z 0 0 (p,r) ≤ inflatedPoisson z 0 0 (1,1)
      simp only [inflatedPoisson, pow_zero, mul_one, Nat.cast_zero, zero_mul, neg_zero,
        exp_zero, sub_self, zero_mul, add_zero, one_pow]
      exact pow_le_one₀
        (effective_mem ⟨(exp_pos _).le, (exp_lt_one_iff.mpr (neg_lt_zero.mpr hp.1)).le⟩
          (hvalid hp.2)).1
        (effective_mem ⟨(exp_pos _).le, (exp_lt_one_iff.mpr (neg_lt_zero.mpr hp.1)).le⟩
          (hvalid hp.2)).2
  · have hk' : 0 < k := Nat.pos_of_ne_zero hk
    have hS' : 0 < S := Nat.pos_of_ne_zero (fun h ↦ hk (hS.mp h))
    simp only [hk, false_and, false_or, hk', true_and]
    constructor
    · rintro ⟨⟨p,r⟩, hp, _, hpos⟩
      refine ⟨r,hp.2,lt_of_le_of_ne (hvalid hp.2).2 ?_⟩
      rintro rfl
      simp [inflatedPoisson, zero_pow hk] at hpos
    · rintro ⟨r,hr,hr'⟩
      obtain ⟨p,hp,hm⟩ := poisson_zi_positive_attainment hk' hS' hW hvalid hr hr'
      refine ⟨p,hp,hm,?_⟩
      apply lt_of_lt_of_le _ (hm (a := (1,r)) ⟨zero_lt_one,hr⟩)
      unfold inflatedPoisson
      exact mul_pos
        (mul_pos (pow_pos (add_pos_of_nonneg_of_pos (hvalid hr).1
          (mul_pos (sub_pos.mpr hr') (exp_pos _))) _) (pow_pos (sub_pos.mpr hr') _))
        (mul_pos (pow_pos one_pos _) (exp_pos _))

/-- Binom, size and pi+size, zero:inflated criterion and failure.
Both allowed weight designs attain on the closed probability slice.
The all-zero winner set includes the size-zero, probability-zero and
inflation-one branches. Free inflation at size one gives the product ridge. -/
theorem binom_inflated {k : ℕ} (N z : ℕ) (x : Fin k → ℕ)
    (hn : 0 < z+k) {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    (∃ p ∈ Icc (0 : ℝ) 1, IsMaxOn (fun p ↦ inflatedBinom N z x (p,rho)) (Icc 0 1) p) ∧
    (∃ t ∈ (Icc (0 : ℝ) 1 ×ˢ Icc (0 : ℝ) 1),
      IsMaxOn (inflatedBinom N z x) (Icc 0 1 ×ˢ Icc 0 1) t) ∧
    (k = 0 → ∀ p ∈ Icc (0 : ℝ) 1,
      inflatedBinom N z x (p,rho) = 1 ↔ N = 0 ∨ p = 0 ∨ rho = 1) ∧
    ((∀ i, x i = 1) → ∀ p ∈ Icc (0 : ℝ) 1,
      IsMaxOn (inflatedBinom 1 z x) (Icc 0 1 ×ˢ Icc 0 1) (p,rho) ↔
        (1-rho)*p = 1 - zeroFraction z k) := by
  refine ⟨binom_zi_held_inflation_attainment N z x rho, binom_zi_attainment N z x,
    ?_, fun hx p hp ↦ binom_zi_size_one hn hx hp hr⟩
  intro hk p hp
  subst k
  have hz : 0 < z := by omega
  have hu : (1-p)^N ∈ Icc (0 : ℝ) 1 :=
    ⟨pow_nonneg (sub_nonneg.mpr hp.2) _, pow_le_one₀ (sub_nonneg.mpr hp.2) (by linarith [hp.1])⟩
  have hU : (1-p)^N = 1 ↔ N = 0 ∨ p = 0 := by
    constructor
    · intro h
      by_contra! H
      exact (pow_lt_one₀ (sub_nonneg.mpr hp.2)
        (sub_lt_self _ (lt_of_le_of_ne hp.1 (Ne.symm H.2))) H.1).ne h
    · rintro (rfl | rfl) <;> simp
  simpa only [inflatedBinom, pow_zero, mul_one, Finset.univ_eq_empty,
    Finset.prod_empty, hU, or_assoc] using inflated_all_zero hz hu hr

/-- Nbinom, size and pi+size, zero:inflated positive criterion. Positive
held size and an allowed weight below one give an interior probability
fit; the compatible probability endpoint at zero continuation loses. -/
theorem nbinom_inflated_positive {r : ℝ} (hr : 0 < r) {z k S : ℕ}
    (hk : 0 < k) (hS : 0 < S) {W : Set ℝ} (hW : IsCompact W)
    (hvalid : W ⊆ Icc 0 1) {rho : ℝ} (hi : rho ∈ W) (hi' : rho < 1) :
    let D := Ico (0 : ℝ) 1 ×ˢ W
    let F := inflatedNegBinom r z k S
    ∃ p ∈ D, p.1 ∈ Ioo (0 : ℝ) 1 ∧ IsMaxOn F D p ∧ 0 < F p := by
  dsimp only
  obtain ⟨p,hp,hm⟩ := nbinom_zi_positive_attainment hr hk hS hW hvalid hi hi'
  have hh : ((1/2 : ℝ),rho) ∈ Ioo (0 : ℝ) 1 ×ˢ W := ⟨by norm_num,hi⟩
  have hV : 0 < inflatedNegBinom r z k S (1/2,rho) := by
    unfold inflatedNegBinom
    have hP : 0 < (1-(1/2 : ℝ))^r := rpow_pos_of_pos (by norm_num) _
    exact mul_pos (mul_pos (mul_pos (pow_pos
      (add_pos_of_nonneg_of_pos (hvalid hi).1 (mul_pos (sub_pos.mpr hi') hP)) _)
      (pow_pos (sub_pos.mpr hi') _)) (pow_pos (by norm_num) _)) (pow_pos hP _)
  have hpos := hV.trans_le (hm hh)
  refine ⟨p,⟨⟨hp.1.1.le,hp.1.2⟩,hp.2⟩,hp.1,?_,hpos⟩
  rintro ⟨q,s⟩ hq
  rcases eq_or_lt_of_le hq.1.1 with he | he
  · change inflatedNegBinom r z k S (q,s) ≤ inflatedNegBinom r z k S p
    change 0 = q at he
    rw [← he]
    simpa [inflatedNegBinom, zero_pow hS.ne'] using hpos.le
  · exact hm ⟨⟨he,hq.1.2⟩,hq.2⟩

/-- Nbinom, size and pi+size, zero:inflated criterion, failure and
uniqueness on all zeros. The supremum one is attained at the included
zero atom; at size zero the entire slice ties. -/
theorem nbinom_inflated_zeros {r : ℝ} (hr : 0 ≤ r) {z : ℕ} (hz : 0 < z)
    {q rho : ℝ} (hq : q ∈ Ico 0 1) (hi : rho ∈ Icc 0 1) :
    inflatedNegBinom r z 0 0 (q,rho) = 1 ↔ r = 0 ∨ q = 0 ∨ rho = 1 := by
  have hbase0 : 0 ≤ 1-q := sub_nonneg.mpr hq.2.le
  have hbase1 : 1-q ≤ 1 := sub_le_self _ hq.1
  have hu : (1-q)^r ∈ Icc (0 : ℝ) 1 :=
    ⟨rpow_nonneg hbase0 _, rpow_le_one hbase0 hbase1 hr⟩
  have he : (1-q)^r = 1 ↔ r = 0 ∨ q = 0 := by
    constructor
    · intro he
      by_contra! H
      have hq0 : 0 < q := lt_of_le_of_ne hq.1 (Ne.symm H.2)
      have hr0 : 0 < r := lt_of_le_of_ne hr (Ne.symm H.1)
      exact (rpow_lt_one hbase0 (sub_lt_self _ hq0) hr0).ne he
    · rintro (rfl | rfl) <;> simp
  simpa only [inflatedNegBinom, pow_zero, mul_one, he, or_assoc] using
    inflated_all_zero hz hu hi


/-- Geom, all free, zero:inflated criterion and uniqueness on the
continuation/effective-zero chart including the parent zero atom.
Here d is the positive-count sum less k, so d is zero on all zeros. -/
theorem geom_inflated {z k d : ℕ} (hn : 0 < z+k) (hd : k = 0 → d = 0) :
    let D : Set (ℝ × ℝ) := {t | t.1 ∈ Ico 0 1 ∧ t.2 ∈ Icc (1-t.1) 1}
    let F := inflatedGeom z k d
    (k = 0 → ∀ p ∈ D, IsMaxOn F D p ↔ p.2 = 1) ∧
    (0 < k → ∃! p, p ∈ D ∧ IsMaxOn F D p) := by
  dsimp only
  let D : Set (ℝ × ℝ) := {t | t.1 ∈ Ico 0 1 ∧ t.2 ∈ Icc (1-t.1) 1}
  have hsub : inflatedGeomDomain ⊆ D :=
    fun _ hp ↦ ⟨⟨hp.1.1.le,hp.1.2⟩,hp.2⟩
  constructor
  · intro hk
    have H := hd hk
    subst k
    subst d
    intro p hp
    have hz : 0 < z := by omega
    have hW (q : ℝ × ℝ) (hq : q ∈ D) : q.2 ∈ Icc (0 : ℝ) 1 :=
      ⟨(sub_nonneg.mpr hq.1.2.le).trans hq.2.1,hq.2.2⟩
    have hf (q : ℝ × ℝ) : inflatedGeom z 0 0 q = q.2^z := by
      simp [inflatedGeom, bernoulli]
    have htop : ((0,1) : ℝ × ℝ) ∈ D := by
      exact ⟨⟨le_rfl,zero_lt_one⟩,⟨by norm_num,le_rfl⟩⟩
    constructor
    · intro H
      have hle : inflatedGeom z 0 0 (0,1) ≤ inflatedGeom z 0 0 p := H htop
      rw [hf,hf] at hle
      by_contra! hn
      have Hlt := pow_lt_one₀ (hW p hp).1 (lt_of_le_of_ne (hW p hp).2 hn) hz.ne'
      exact Hlt.not_ge (by simpa using hle)
    · intro H q hq
      change inflatedGeom z 0 0 q ≤ inflatedGeom z 0 0 p
      rw [hf,hf,H,one_pow]
      exact pow_le_one₀ (hW q hq).1 (hW q hq).2
  · intro hk
    obtain ⟨hp,hm,hu⟩ := geom_zi_fit (z := z) (d := d) hk
    let a := 1 - zeroFraction z k
    let q := if a ≤ zeroFraction d k then zeroFraction d k else zeroFraction (d+k) (z+k)
    let p := (q,max (1-q) (zeroFraction z k))
    change p ∈ inflatedGeomDomain at hp
    change IsMaxOn (inflatedGeom z k d) inflatedGeomDomain p at hm
    change ∀ t ∈ inflatedGeomDomain, IsMaxOn (inflatedGeom z k d) inflatedGeomDomain t → t = p at hu
    have hzero (t : ℝ × ℝ) (ht : t ∈ D) (h : t.1 = 0) :
        inflatedGeom z k d t = 0 := by
      have he : t.2 = 1 := le_antisymm ht.2.2 (by simpa [h] using ht.2.1)
      simp [inflatedGeom, bernoulli, he, zero_pow hk.ne']
    have hhalf : ((1/2,1/2) : ℝ × ℝ) ∈ inflatedGeomDomain :=
      ⟨⟨by norm_num,by norm_num⟩,⟨by norm_num,by norm_num⟩⟩
    have hhalfpos : 0 < inflatedGeom z k d (1/2,1/2) := by
      unfold inflatedGeom bernoulli
      dsimp only
      exact mul_pos (mul_pos (pow_pos (by norm_num) _) (pow_pos (by norm_num) _))
        (mul_pos (pow_pos (by norm_num) _) (pow_pos (by norm_num) _))
    have hpos := hhalfpos.trans_le (hm hhalf)
    refine ⟨p,⟨hsub hp,?_⟩,?_⟩
    · intro t ht
      by_cases he : t.1 = 0
      · change inflatedGeom z k d t ≤ inflatedGeom z k d p
        rw [hzero t ht he]
        exact hpos.le
      · exact hm ⟨⟨lt_of_le_of_ne ht.1.1 (Ne.symm he),ht.1.2⟩,ht.2⟩
    · rintro t ⟨ht,htM⟩
      have he : t.1 ≠ 0 := by
        intro H
        have hh : inflatedGeom z k d p ≤ inflatedGeom z k d t := htM (hsub hp)
        rw [hzero t ht H] at hh
        exact hpos.not_ge hh
      exact hu t ⟨⟨lt_of_le_of_ne ht.1.1 (Ne.symm he),ht.1.2⟩,ht.2⟩
        (fun _ h ↦ htM (hsub h))

/-- Geom, pi held, zero:inflated criterion and failure. In continuation
coordinates zero is admitted, and one has zero height on positive data.
All-zero fits are the zero atom or the full slice at inflation one. -/
theorem geom_held {z k d : ℕ} (hn : 0 < z+k) (hd : k = 0 → d = 0)
    {rho : ℝ} (hr : rho ∈ Icc 0 1) :
    let D := Ico (0 : ℝ) 1
    let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
    (k = 0 → ∀ q ∈ D, IsMaxOn F D q ↔ q = 0 ∨ rho = 1) ∧
    (0 < k → ((∃ q ∈ D, IsMaxOn F D q ∧ 0 < F q) ↔ rho < 1)) := by
  dsimp only
  constructor
  · intro hk
    have he := hd hk
    subst k
    subst d
    have hz : 0 < z := by omega
    intro q hq
    have hE (t : ℝ) (ht : t ∈ Ico (0 : ℝ) 1) :
        effective (1-t) rho ∈ Icc 0 1 :=
      effective_mem ⟨sub_nonneg.mpr ht.2.le,sub_le_self _ ht.1⟩ hr
    have hf (t : ℝ) : inflatedGeom z 0 0 (t,effective (1-t) rho) =
        effective (1-t) rho ^ z := by simp [inflatedGeom, bernoulli]
    have hz0 : inflatedGeom z 0 0 (0,effective (1-0) rho) = 1 := by
      rw [hf]
      simp [effective]
    have hiff : inflatedGeom z 0 0 (q,effective (1-q) rho) = 1 ↔ q = 0 ∨ rho = 1 := by
      rw [hf]
      have H := inflated_all_zero hz ⟨sub_nonneg.mpr hq.2.le,sub_le_self _ hq.1⟩ hr
      simpa only [effective, sub_eq_self] using H
    constructor
    · intro H
      apply hiff.mp
      have hle : inflatedGeom z 0 0 (0,effective (1-0) rho) ≤
          inflatedGeom z 0 0 (q,effective (1-q) rho) := H (a := 0) ⟨le_rfl,zero_lt_one⟩
      rw [hz0] at hle
      refine le_antisymm ?_ hle
      rw [hf]
      exact pow_le_one₀ (hE q hq).1 (hE q hq).2
    · intro H t ht
      change inflatedGeom z 0 0 (t,effective (1-t) rho) ≤
        inflatedGeom z 0 0 (q,effective (1-q) rho)
      rw [hiff.mpr H,hf]
      exact pow_le_one₀ (hE t ht).1 (hE t ht).2
  · intro hk
    let F := fun q ↦ inflatedGeom z k d (q,effective (1-q) rho)
    constructor
    · rintro ⟨q,hq,_,hpos⟩
      refine lt_of_le_of_ne hr.2 ?_
      rintro rfl
      simp [inflatedGeom, effective, bernoulli, zero_pow hk.ne'] at hpos
    · intro h
      have hc : Continuous F := by
        unfold F inflatedGeom effective bernoulli
        fun_prop
      have hh : (1/2 : ℝ) ∈ Icc 0 1 := by norm_num
      obtain ⟨q,hq,hm⟩ := isCompact_Icc.exists_isMaxOn ⟨_,hh⟩ hc.continuousOn
      have hmid : 0 < F (1/2) := by
        have h1 : 0 < effective (1-(1/2 : ℝ)) rho :=
          add_pos_of_nonneg_of_pos hr.1 (mul_pos (sub_pos.mpr h) (by norm_num))
        have h2 : 0 < 1-effective (1-(1/2 : ℝ)) rho := by
          unfold effective
          linarith
        exact mul_pos (mul_pos (pow_pos h1 _) (pow_pos h2 _))
          (mul_pos (pow_pos (by norm_num) _) (pow_pos (by norm_num) _))
      have hpos := hmid.trans_le (hm hh)
      have hq' : q < 1 := lt_of_le_of_ne hq.2 (by
        rintro rfl
        simp [F, inflatedGeom, bernoulli, zero_pow hk.ne'] at hpos)
      exact ⟨q,⟨hq.1,hq'⟩,fun _ ht ↦ hm ⟨ht.1,ht.2.le⟩,hpos⟩

end

end UnivariateML.Zero6Inflated

end
