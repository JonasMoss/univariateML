module

public import UnivariateML.Family.Student2MovingCollapse
public import UnivariateML.Family.Student2Normal
public import UnivariateML.Family.StudentHeldShape

import Mathlib.Tactic
import Mathlib.Topology.Instances.EReal.Lemmas
import Mathlib.Topology.Sequences

set_option linter.style.header false

/-!
# Joint standard Student with free coefficients

The ordinary scale is used throughout. At df above two this is a
positive bijective change of coordinates from standard deviation.
-/

@[expose] public section

open Set Filter Topology Real

namespace UnivariateML.Student2Free

private theorem continuousAt_logGamma {z : ℝ} (hz : 0 < z) :
    ContinuousAt (fun t ↦ log (Gamma t)) z :=
  ((differentiableAt_Gamma (fun k ↦ ne_of_gt
    ((neg_nonpos.mpr (Nat.cast_nonneg k)).trans_lt hz))).log
    (Gamma_pos_of_pos hz).ne').continuousAt

private theorem finite_shape_face {n : ℕ} (x : Fin n → ℝ)
    {ι : Type*} {l : Filter ι} {m a v : ι → ℝ} {b s q : ℝ}
    (hs : 0 < s) (hq : 0 < q)
    (hm : Tendsto m l (𝓝 b)) (ha : Tendsto a l (𝓝 s)) (hv : Tendsto v l (𝓝 q)) :
    Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) l
      (𝓝 (TDist.likelihood x b q s)) := by
  have hΓ₁ := (continuousAt_logGamma (show 0 < (q + 1) / 2 by positivity)).tendsto.comp
    ((hv.add_const 1).div_const 2)
  have hΓ₂ := (continuousAt_logGamma (show 0 < q / 2 by positivity)).tendsto.comp (hv.div_const 2)
  have hN := (hΓ₁.sub hΓ₂).sub (((hv.mul_const π).log (by positivity)).div_const 2)
  have hS (i : Fin n) :
      Tendsto (fun j ↦ TDist.logNormalizer (v j) - log (a j) -
        (v j + 1) / 2 * log (1 + (x i - m j)^2 / (v j * (a j)^2))) l
        (𝓝 (TDist.logNormalizer q - log s -
          (q + 1) / 2 * log (1 + (x i - b)^2 / (q * s^2)))) := by
    have hR := (((tendsto_const_nhds (x := x i)).sub hm).pow 2).div
      (hv.mul (ha.pow 2)) (by positivity)
    exact (hN.sub (ha.log hs.ne')).sub
      (((hv.add_const 1).div_const 2).mul ((hR.const_add 1).log (by positivity)))
  exact continuous_exp.continuousAt.tendsto.comp (tendsto_finsetSum _ fun i _ ↦ hS i)

private def chart : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 2 < p.2.2}

private noncomputable def value {n : ℕ} (x : Fin n → ℝ) (p : ℝ × ℝ × ℝ) : ℝ :=
  TDist.likelihood x p.1 p.2.2 p.2.1

private def faceLE {n : ℕ} (x : Fin n → ℝ) (A : ℝ) : Prop :=
  (∀ b s, 0 < s → (∏ i, TDist.normalDensity b s (x i)) ≤ A) ∧
  (∀ b s, 0 < s → TDist.likelihood x b 2 s ≤ A) ∧
  ∀ b, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ A

private def faceLEOn {n : ℕ} (x : Fin n → ℝ) (M : Set ℝ) (A : ℝ) : Prop :=
  (∀ b ∈ M, ∀ s, 0 < s → (∏ i, TDist.normalDensity b s (x i)) ≤ A) ∧
  (∀ b ∈ M, ∀ s, 0 < s → TDist.likelihood x b 2 s ≤ A) ∧
  ∀ b ∈ M, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ A

private theorem count_le_n {n : ℕ} (x : Fin n → ℝ) (b : ℝ) :
    (TDist.multiplicity x b : ℝ) ≤ (n : ℝ) := by
  exact_mod_cast (Finset.card_filter_le (s := Finset.univ) (fun i ↦ x i = b)).trans_eq
    (by simp : (Finset.univ : Finset (Fin n)).card = n)

private theorem exponent_pos_above {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b r q : ℝ) (hq : r < q) (he : 0 ≤ TDist.exponent x b r) :
    0 < TDist.exponent x b q := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hk := count_le_n x b
  have hlt : (TDist.multiplicity x b : ℝ) < n := by
    by_contra! h
    unfold TDist.exponent at he
    have heq : (TDist.multiplicity x b : ℝ) = n := le_antisymm hk h
    rw [heq, sub_self, mul_zero, zero_sub] at he
    linarith
  have h := mul_pos (sub_pos.mpr hq) (sub_pos.mpr hlt)
  unfold TDist.exponent at he ⊢
  nlinarith only [he, h]

private theorem superlevel_subseqAt {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (r : ℝ) (hr : 1 ≤ r) (included : Bool)
    {M : Set ℝ} (hM : IsClosed M) (hE : ∀ b ∈ M, 0 ≤ TDist.exponent x b r)
    {A B : ℝ} (hAB : B < A) (hA : 0 < A)
    (p : ℕ → ℝ × ℝ × ℝ) (hp : ∀ j, 0 < (p j).2.1 ∧ (if included then r ≤ (p j).2.2 else r < (p j).2.2)) (hmp : ∀ j, (p j).1 ∈ M)
    (hval : ∀ j, A ≤ value x (p j))
    (hN : ∀ (φ : ℕ → ℕ) b s, b ∈ M → 0 < s →
      Tendsto (fun j ↦ (p (φ j)).2.1) atTop (𝓝 s) →
      Tendsto (fun j ↦ (p (φ j)).2.2) atTop atTop →
      (∏ i, TDist.normalDensity b s (x i)) ≤ B)
    (hT : ∀ (φ : ℕ → ℕ) b s, b ∈ M → 0 < s →
      Tendsto (fun j ↦ (p (φ j)).2.1) atTop (𝓝 s) →
      Tendsto (fun j ↦ (p (φ j)).2.2) atTop (𝓝 r) →
      included = false → TDist.likelihood x b r s ≤ B)
    (hC : ∀ b ∈ M, TDist.exponent x b r = 0 → exp (TDist.collapseLogValue x b r) ≤ B) :
    ∃ z : ℝ × ℝ × ℝ, (0 < z.2.1 ∧ (if included then r ≤ z.2.2 else r < z.2.2)) ∧
      z.1 ∈ M ∧ ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (p ∘ φ) atTop (𝓝 z) ∧
      Tendsto (fun j ↦ value x (p (φ j))) atTop (𝓝 (value x z)) := by
  let u : ℕ → EReal × EReal × EReal := fun j ↦ ((p j).1, (p j).2.1, (p j).2.2)
  obtain ⟨z, φ, hφ, hu⟩ := CompactSpace.tendsto_subseq u
  let m : ℕ → ℝ := fun j ↦ (p (φ j)).1
  let a : ℕ → ℝ := fun j ↦ (p (φ j)).2.1
  let v : ℕ → ℝ := fun j ↦ (p (φ j)).2.2
  have ha : ∀ᶠ j in atTop, 0 < a j := Eventually.of_forall fun j ↦ (hp (φ j)).1
  have hv : ∀ᶠ j in atTop, r ≤ v j := Eventually.of_forall fun j ↦ by
    have h := (hp (φ j)).2
    cases included
    · exact (show r < v j from h).le
    · exact h
  have hv₁ : ∀ᶠ j in atTop, 1 ≤ v j := hv.mono fun _ hj ↦ hr.trans hj
  have hL : ∀ᶠ j in atTop, A ≤ TDist.likelihood x (m j) (v j) (a j) :=
    Eventually.of_forall fun j ↦ hval (φ j)
  have hz (t : ℝ) (ht : t < A) :
      ¬ Tendsto (fun j ↦ TDist.likelihood x (m j) (v j) (a j)) atTop (𝓝 t) :=
    fun h ↦ ht.not_ge (ge_of_tendsto h hL)
  have hma : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) := continuous_fst.tendsto z |>.comp hu
  have haa : Tendsto (fun j ↦ (a j : EReal)) atTop (𝓝 z.2.1) :=
    (continuous_fst.comp continuous_snd).tendsto z |>.comp hu
  have hva : Tendsto (fun j ↦ (v j : EReal)) atTop (𝓝 z.2.2) :=
    (continuous_snd.comp continuous_snd).tendsto z |>.comp hu
  have hfar (h : Tendsto m atTop (cocompact ℝ)) : False :=
    hz 0 hA (Student2Normal.moving_shape_remote_ends x hn ha hv₁ (.inr h))
  have hbig (h : Tendsto a atTop atTop) : False :=
    hz 0 hA (Student2Normal.moving_shape_remote_ends x hn ha hv₁ (.inl h))
  have hforbidden {f : ℕ → ℝ} (c : ℝ) (hc : ∀ᶠ j in atTop, c ≤ f j)
      (ht : Tendsto f atTop atBot) : False :=
    ((ht.eventually_lt_atBot c).and hc).exists.elim fun _ h ↦ h.1.not_ge h.2
  generalize hzm : z.1 = zm at hma
  generalize hza : z.2.1 = za at haa
  generalize hzv : z.2.2 = zv at hva
  cases zm using EReal.rec with
  | bot =>
      exact (hfar ((EReal.tendsto_coe_nhds_bot_iff.mp hma).mono_right
        (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left))).elim
  | top =>
      exact (hfar ((EReal.tendsto_coe_nhds_top_iff.mp hma).mono_right
        (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right))).elim
  | coe b =>
    have hm := EReal.tendsto_coe.mp hma
    have hbM : b ∈ M := hM.mem_of_tendsto hm (Eventually.of_forall fun j ↦ hmp (φ j))
    cases za using EReal.rec with
    | bot => exact (hforbidden 0 (ha.mono fun _ h ↦ h.le)
        (EReal.tendsto_coe_nhds_bot_iff.mp haa)).elim
    | top => exact (hbig (EReal.tendsto_coe_nhds_top_iff.mp haa)).elim
    | coe s =>
      have hs := EReal.tendsto_coe.mp haa
      have hs₀ : 0 ≤ s := ge_of_tendsto hs (ha.mono fun _ h ↦ h.le)
      cases zv using EReal.rec with
      | bot => exact (hforbidden r hv
          (EReal.tendsto_coe_nhds_bot_iff.mp hva)).elim
      | top =>
        have ht := EReal.tendsto_coe_nhds_top_iff.mp hva
        rcases hs₀.eq_or_lt with rfl | hs'
        · have hnon : ∃ i, x i ≠ b := by
            by_contra! h
            have he := hE b hbM
            rw [TDist.exponent_all_tied x b r h] at he
            exact (not_le_of_gt (show (0 : ℝ) < n by exact_mod_cast hn)) (by linarith)
          exact (hz 0 hA (Student2Normal.zero_scale_normal_corner x hm
            (tendsto_nhdsWithin_iff.mpr ⟨hs, ha⟩) ht hnon)).elim
        · exact (hz _ ((hN φ b s hbM hs' hs ht).trans_lt hAB)
            (Student2Normal.moving_normal_face x hs' hm hs ht)).elim
      | coe q =>
        have hq := EReal.tendsto_coe.mp hva
        have hq₂ : r ≤ q := ge_of_tendsto hq hv
        have hq₀ : 0 < q := by linarith
        rcases hs₀.eq_or_lt with rfl | hs'
        · obtain ⟨hpos, hcrit⟩ := Student2MovingCollapse.moving_shape_collapse x b hq₀ hm
            (tendsto_nhdsWithin_iff.mpr ⟨hs, ha⟩) hq
          rcases hq₂.eq_or_lt with rfl | hq'
          · rcases (hE b hbM).eq_or_lt with he | he
            · exact (((hcrit he.symm hv A ((hC b hbM he.symm).trans_lt hAB)).and hL).exists.elim
                fun _ h ↦ h.1.not_ge h.2).elim
            · exact (hz 0 hA (hpos he)).elim
          · exact (hz 0 hA (hpos (exponent_pos_above x hn b r q hq' (hE b hbM)))).elim
        · have hlim := finite_shape_face x hs' hq₀ hm hs hq
          rcases hq₂.eq_or_lt with rfl | hq'
          · cases hinc : included
            · exact (hz _ ((hT φ b s hbM hs' hs hq hinc).trans_lt hAB) hlim).elim
            · exact ⟨(b, s, r), ⟨hs', by simp⟩, hbM, φ, hφ,
                hm.prodMk_nhds (hs.prodMk_nhds hq), hlim⟩
          · refine ⟨(b, s, q), ⟨hs', ?_⟩, hbM, φ, hφ, hm.prodMk_nhds (hs.prodMk_nhds hq), hlim⟩
            cases included
            · exact hq'
            · exact hq'.le

private theorem superlevel_subseq {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {M : Set ℝ} (hM : IsClosed M) (hE : ∀ b ∈ M, 0 ≤ TDist.exponent x b 2)
    {A B : ℝ} (hAB : B < A) (hA : 0 < A)
    (p : ℕ → ℝ × ℝ × ℝ) (hp : ∀ j, p j ∈ chart) (hmp : ∀ j, (p j).1 ∈ M)
    (hval : ∀ j, A ≤ value x (p j))
    (hN : ∀ (φ : ℕ → ℕ) b s, b ∈ M → 0 < s →
      Tendsto (fun j ↦ (p (φ j)).2.1) atTop (𝓝 s) →
      Tendsto (fun j ↦ (p (φ j)).2.2) atTop atTop →
      (∏ i, TDist.normalDensity b s (x i)) ≤ B)
    (hT : ∀ (φ : ℕ → ℕ) b s, b ∈ M → 0 < s →
      Tendsto (fun j ↦ (p (φ j)).2.1) atTop (𝓝 s) →
      Tendsto (fun j ↦ (p (φ j)).2.2) atTop (𝓝 2) →
      TDist.likelihood x b 2 s ≤ B)
    (hC : ∀ b ∈ M, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ B) :
    ∃ z ∈ chart, z.1 ∈ M ∧ ∃ φ : ℕ → ℕ, StrictMono φ ∧
      Tendsto (p ∘ φ) atTop (𝓝 z) ∧
      Tendsto (fun j ↦ value x (p (φ j))) atTop (𝓝 (value x z)) := by
  exact superlevel_subseqAt x hn 2 (by norm_num) false hM hE hAB hA p hp hmp hval
    hN (fun φ b s hb hs ha hv _ ↦ hT φ b s hb hs ha hv) hC

private theorem value_continuousOn {n : ℕ} (x : Fin n → ℝ) :
    ContinuousOn (value x) chart := by
  intro z hz
  have h := nhdsWithin_le_nhds (s := chart) (a := z)
  exact finite_shape_face x hz.1 (show 0 < z.2.2 by have := hz.2; linarith)
    ((continuous_fst.tendsto z).mono_left h)
    (((continuous_fst.comp continuous_snd).tendsto z).mono_left h)
    (((continuous_snd.comp continuous_snd).tendsto z).mono_left h)

private theorem max_of_witnessOn {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {M : Set ℝ} (hM : IsClosed M) (he : ∀ b ∈ M, 0 ≤ TDist.exponent x b 2)
    {z : ℝ × ℝ × ℝ} (hz : z ∈ chart ∩ {p | p.1 ∈ M}) (hf : faceLEOn x M (value x z)) :
    ∃ p ∈ chart ∩ {p | p.1 ∈ M}, IsMaxOn (value x) (chart ∩ {p | p.1 ∈ M}) p := by
  let D := chart ∩ {p : ℝ × ℝ × ℝ | p.1 ∈ M}
  by_cases hmax : IsMaxOn (value x) D z
  · exact ⟨z, hz, hmax⟩
  · rw [IsMaxOn, IsMaxFilter, eventually_principal] at hmax
    push Not at hmax
    obtain ⟨w, hw, hzw⟩ := hmax
    let K := {p ∈ D | value x w ≤ value x p}
    have hK : IsCompact K := IsSeqCompact.isCompact (by
      intro p hp
      obtain ⟨q, hq, hqm, φ, hφ, hlim, hv⟩ :=
        superlevel_subseq x hn hM he hzw (exp_pos _) p (fun j ↦ (hp j).1.1)
          (fun j ↦ (hp j).1.2) (fun j ↦ (hp j).2)
          (fun _ b a hb ha _ _ ↦ hf.1 b hb a ha)
          (fun _ b a hb ha _ _ ↦ hf.2.1 b hb a ha) hf.2.2
      exact ⟨q, ⟨⟨hq, hqm⟩, ge_of_tendsto' hv (fun j ↦ (hp (φ j)).2)⟩, φ, hφ, hlim⟩)
    obtain ⟨q, hq, hqm⟩ := hK.exists_isMaxOn ⟨w, hw, le_rfl⟩
      ((value_continuousOn x).mono fun _ hp ↦ hp.1.1)
    refine ⟨q, hq.1, fun p hp ↦ ?_⟩
    exact if h : value x w ≤ value x p then hqm ⟨hp, h⟩ else (le_of_not_ge h).trans hq.2

private theorem max_of_witness {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (he : ∀ b, 0 ≤ TDist.exponent x b 2)
    {z : ℝ × ℝ × ℝ} (hz : z ∈ chart) (hf : faceLE x (value x z)) :
    ∃ p ∈ chart, IsMaxOn (value x) chart p := by
  obtain ⟨p, hp, hm⟩ := max_of_witnessOn x hn (M := univ) isClosed_univ
    (fun b _ ↦ he b) (z := z) ⟨hz, mem_univ _⟩
    ⟨fun b _ ↦ hf.1 b, fun b _ ↦ hf.2.1 b, fun b _ ↦ hf.2.2 b⟩
  exact ⟨p, hp.1, fun _ h ↦ hm ⟨h, mem_univ _⟩⟩

set_option maxHeartbeats 1000000 in
private theorem upper_reaches_facesOn {n : ℕ} (x : Fin n → ℝ) {M : Set ℝ} {B : ℝ}
    (hz : ∀ p ∈ chart, p.1 ∈ M → value x p ≤ B) : faceLEOn x M B := by
  have hbound (b s v : ℝ) (hb : b ∈ M) (hs : 0 < s) (hv : 2 < v) :
      TDist.likelihood x b v s ≤ B := by
    have H : value x (b,s,v) ≤ B := hz _ (show (b,s,v) ∈ chart from ⟨hs,hv⟩) hb
    exact H
  have rightPos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  have h0 : Tendsto (id : ℝ → ℝ) (𝓝[>] 0) (𝓝 0) := nhdsWithin_le_nhds
  have hν : Tendsto (fun t : ℝ ↦ 2 + t) (𝓝[>] 0) (𝓝 2) := by simpa using h0.const_add 2
  refine ⟨?_, ?_, ?_⟩
  · intro b hb s hs
    apply le_of_tendsto (Student2Normal.moving_normal_face x hs
      (l := atTop) (m := fun _ : ℝ ↦ b) (a := fun _ ↦ s) (v := id)
      tendsto_const_nhds tendsto_const_nhds tendsto_id)
    change ∀ᶠ v : ℝ in atTop, TDist.likelihood x b v s ≤ B
    filter_upwards [eventually_gt_atTop (2 : ℝ)] with v hv
    exact hbound b s v hb hs hv
  · intro b hb s hs
    apply le_of_tendsto (finite_shape_face x hs (by norm_num)
      (m := fun _ ↦ b) (a := fun _ ↦ s) tendsto_const_nhds tendsto_const_nhds hν)
    change ∀ᶠ t : ℝ in 𝓝[>] 0, TDist.likelihood x b (2+t) s ≤ B
    filter_upwards [rightPos] with t ht
    have hv : 2 < 2 + t := lt_add_of_pos_right _ ht
    exact hbound b s (2+t) hb hs hv
  · intro b hb he
    have hrate : Tendsto (fun t : ℝ ↦ (2+t-2) * log t) (𝓝[>] 0) (𝓝 0) := by
      simpa only [add_sub_cancel_left, zero_mul, Function.comp_id] using continuous_mul_log.continuousAt.tendsto.comp h0
    apply le_of_tendsto (Student2MovingCollapse.critical_vertical_rate x b (by norm_num)
      he (a := id) tendsto_id hν hrate)
    change ∀ᶠ t : ℝ in 𝓝[>] 0, TDist.likelihood x b (2+t) t ≤ B
    filter_upwards [rightPos] with t ht
    have hv : 2 < 2 + t := lt_add_of_pos_right _ ht
    exact hbound b t (2+t) hb ht hv

private theorem upper_reaches_faces {n : ℕ} (x : Fin n → ℝ) {B : ℝ}
    (hz : ∀ p ∈ chart, value x p ≤ B) : faceLE x B := by
  have hf := upper_reaches_facesOn x (M := univ) (fun p hp _ ↦ hz p hp)
  exact ⟨fun b ↦ hf.1 b (mem_univ _), fun b ↦ hf.2.1 b (mem_univ _),
    fun b ↦ hf.2.2 b (mem_univ _)⟩

set_option maxHeartbeats 1000000 in
private theorem max_mass {n : ℕ} (x : Fin n → ℝ) {z : ℝ × ℝ × ℝ}
    (hz : IsMaxOn (value x) chart z) (b : ℝ) : 0 ≤ TDist.exponent x b 2 := by
  by_contra! h
  have h' : (2 : ℝ) * n < 3 * (TDist.multiplicity x b : ℝ) := by
    unfold TDist.exponent at h
    linarith
  obtain ⟨v, hv, ht⟩ := StudentFaces.std_supercritical_atom x b (by exact_mod_cast h')
  have rightPos : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t := self_mem_nhdsWithin
  obtain ⟨t, ht', hval⟩ := (rightPos.and (ht.eventually_gt_atTop (value x z))).exists
  have hu : value x (b,t,v) ≤ value x z := hz (show (b,t,v) ∈ chart from ⟨ht',hv⟩)
  exact (not_lt_of_ge hu) (show value x z < value x (b,t,v) from hval)

/-- A finite standard Student fit exists exactly when no atom exceeds
two thirds and a feasible stationary candidate reaches every excluded
face height. The comparisons are weak, so a finite candidate tied with
a face may attain. Ordinary scale is used for the free scale parameter. -/
theorem std_all_free_criterion {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 2 < p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    (∃ p ∈ D, IsMaxOn L D p) ↔
      (∀ b, 3 * TDist.multiplicity x b ≤ 2 * n) ∧
      ∃ p ∈ D,
        fderiv ℝ (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) p = 0 ∧
        (∀ b s, 0 < s → (∏ i, TDist.normalDensity b s (x i)) ≤ L p) ∧
        (∀ b s, 0 < s → TDist.likelihood x b 2 s ≤ L p) ∧
        (∀ b, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ L p) := by
  dsimp only
  change (∃ p ∈ chart, IsMaxOn (value x) chart p) ↔ _
  have hmass (b : ℝ) : 0 ≤ TDist.exponent x b 2 ↔ 3 * TDist.multiplicity x b ≤ 2 * n := by
    have h : (3 * TDist.multiplicity x b ≤ 2 * n) ↔
        (3 * (TDist.multiplicity x b : ℝ) ≤ 2 * (n : ℝ)) := by exact_mod_cast Iff.rfl
    rw [h]
    unfold TDist.exponent
    constructor <;> intro h <;> linarith
  constructor
  · rintro ⟨p, hp, hm⟩
    refine ⟨fun b ↦ (hmass b).mp (max_mass x hm b), p, hp, ?_, upper_reaches_faces x (fun _ hw ↦ hm hw)⟩
    have hOpen : IsOpen chart :=
      (isOpen_lt continuous_const (continuous_fst.comp continuous_snd)).inter
        (isOpen_lt continuous_const (continuous_snd.comp continuous_snd))
    have hmax : IsMaxOn (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) chart p :=
      fun _ hz ↦ exp_le_exp.mp (hm hz)
    exact (hmax.isLocalMax (hOpen.mem_nhds hp)).fderiv_eq_zero
  · rintro ⟨h, p, hp, _, hf⟩
    exact max_of_witness x hn (fun b ↦ (hmass b).mpr (h b)) hp hf

private theorem gaussian_bound_at {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (b a : ℝ) (ha : 0 < a) (d : ℝ) (hd : 0 < d)
    (hS : d ≤ ∑ i, (x i - b)^2) :
    (∏ i, TDist.normalDensity b a (x i)) ≤
      exp ((n : ℝ) * (-(log (2*π)/2)) + n/2 * (log n - log d - 1)) := by
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  let u := d / ((n : ℝ) * a^2)
  have hu : 0 < u := by dsimp [u]; positivity
  have hlog : log u = log d - log n - 2*log a := by
    dsimp [u]
    rw [log_div hd.ne' (by positivity), log_mul hn'.ne' (by positivity), log_pow]
    norm_num
    ring
  have hbound := mul_le_mul_of_nonneg_left (log_le_sub_one_of_pos hu)
    (show 0 ≤ (n : ℝ)/2 by positivity)
  have he : (n : ℝ) / 2 * u = d / a^2 / 2 := by dsimp [u]; field_simp
  rw [mul_sub, he, hlog] at hbound
  simp_rw [TDist.normalDensity]
  rw [← exp_sum]
  apply exp_le_exp.mpr
  simp only [Finset.sum_sub_distrib, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul, ← Finset.sum_div]
  have hscaled := div_le_div_of_nonneg_right (div_le_div_of_nonneg_right hS
    (sq_nonneg a)) (show 0 ≤ (2 : ℝ) by norm_num)
  change _ ≤ _
  nlinarith only [hbound, hscaled]

private theorem gaussian_bound {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (h : ∃ i j, x i ≠ x j) :
    ∃ B, ∀ b a : ℝ, 0 < a → (∏ i, TDist.normalDensity b a (x i)) ≤ B := by
  obtain ⟨i,j,hij⟩ := h
  let d := (x i - x j)^2 / 4
  have hd : 0 < d := div_pos (sq_pos_of_ne_zero (sub_ne_zero.mpr hij)) (by norm_num)
  refine ⟨exp ((n : ℝ) * (-(log (2*π)/2)) + n/2 * (log n - log d - 1)),
    fun b a ha ↦ ?_⟩
  let S := ∑ k, (x k - b)^2
  have hi : (x i - b)^2 ≤ S :=
    Finset.single_le_sum (f := fun k ↦ (x k - b)^2) (fun k _ ↦ sq_nonneg (x k - b)) (Finset.mem_univ i)
  have hj : (x j - b)^2 ≤ S :=
    Finset.single_le_sum (f := fun k ↦ (x k - b)^2) (fun k _ ↦ sq_nonneg (x k - b)) (Finset.mem_univ j)
  have hS : d ≤ S := by dsimp [d]; nlinarith [sq_nonneg (x i + x j - 2*b)]
  exact gaussian_bound_at x hn b a ha d hd hS

private def faces {n : ℕ} (x : Fin n → ℝ) : Set ℝ :=
  {y | (∃ b a, 0 < a ∧ y = ∏ i, TDist.normalDensity b a (x i)) ∨
    (∃ b a, 0 < a ∧ y = TDist.likelihood x b 2 a) ∨
    (∃ b, TDist.exponent x b 2 = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}

private theorem face_le_iff {n : ℕ} (x : Fin n → ℝ) (B : ℝ) :
    faceLE x B ↔ B ∈ upperBounds (faces x) := by
  constructor
  · rintro ⟨hN,hT,hC⟩ _ (⟨b,a,ha,rfl⟩ | ⟨b,a,ha,rfl⟩ | ⟨b,hb,rfl⟩)
    · exact hN b a ha
    · exact hT b a ha
    · exact hC b hb
  · intro h
    exact ⟨fun b a ha ↦ h (Or.inl ⟨b,a,ha,rfl⟩),
      fun b a ha ↦ h (Or.inr (Or.inl ⟨b,a,ha,rfl⟩)),
      fun b hb ↦ h (Or.inr (Or.inr ⟨b,hb,rfl⟩))⟩

private theorem faces_bounded {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    (he : ∀ b, 0 ≤ TDist.exponent x b 2) : BddAbove (faces x) := by
  have hnon : ∃ i j, x i ≠ x j := by
    by_contra! h
    let i : Fin n := ⟨0, hn⟩
    have H := he (x i)
    rw [TDist.exponent_all_tied x (x i) 2 (fun j ↦ h j i)] at H
    have hn' : (0 : ℝ) < n := by exact_mod_cast hn
    linarith
  obtain ⟨B,hB⟩ := gaussian_bound x hn hnon
  have hLower : ∃ C, (∀ b s, 0 < s → TDist.likelihood x b 2 s ≤ C) ∧
      (∀ b, TDist.exponent x b 2 = 0 → exp (TDist.collapseLogValue x b 2) ≤ C) := by
    by_cases h : ∃ b, TDist.exponent x b 2 = 0
    · obtain ⟨b,hb⟩ := h
      obtain ⟨hL,hstrict⟩ := StudentFaces.lst_critical_joint x hn (by norm_num : (1 : ℝ) < 2) hb
      refine ⟨exp (TDist.collapseLogValue x b 2), fun m a ha ↦ (hstrict m a ha).le, ?_⟩
      intro m hm
      have H := (StudentFaces.lst_critical_joint x hn (by norm_num : (1 : ℝ) < 2) hm).1
      exact H.2 hL.1
    · have hpos (i : Fin n) : 0 < TDist.exponent x (x i) 2 :=
        lt_of_le_of_ne (he _) (fun hEq ↦ h ⟨x i,hEq.symm⟩)
      obtain ⟨p,hp,hm⟩ := StudentHeldShape.lst_strict_atoms_attain x hn (by norm_num : (0 : ℝ) < 2) hpos
      refine ⟨TDist.likelihood x p.1 2 p.2, ?_, fun b hb ↦ (h ⟨b,hb⟩).elim⟩
      intro b s hs
      have H : TDist.logLik x b 2 s ≤ TDist.logLik x p.1 2 p.2 :=
        hm (show (b,s) ∈ (univ : Set ℝ) ×ˢ Ioi 0 from ⟨mem_univ _,hs⟩)
      exact exp_le_exp.mpr H
  obtain ⟨C,hT,hC⟩ := hLower
  exact ⟨max B C, (face_le_iff x _).mp ⟨
    fun b a ha ↦ (hB b a ha).trans (le_max_left _ _),
    fun b a ha ↦ (hT b a ha).trans (le_max_right _ _),
    fun b hb ↦ (hC b hb).trans (le_max_right _ _)⟩⟩

/-- A supercritical atom gives an unbounded fitted likelihood. Otherwise
the excluded height is the least upper bound of the feasible Gaussian,
Student-two and critical-collapse faces, and is the unattained fitted
supremum exactly when all finite fits lose to it. -/
theorem std_all_free_failure {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 2 < p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    let F : Set ℝ := {y |
      (∃ b a, 0 < a ∧ y = ∏ i, TDist.normalDensity b a (x i)) ∨
      (∃ b a, 0 < a ∧ y = TDist.likelihood x b 2 a) ∨
      (∃ b, TDist.exponent x b 2 = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, TDist.exponent x b 2 < 0) ∧
    ((∀ b, 0 ≤ TDist.exponent x b 2) →
      IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F)) := by
  dsimp only
  change ((¬ BddAbove (value x '' chart)) ↔ _) ∧
    (_ → IsLUB (faces x) (sSup (faces x)) ∧ _)
  have hBound {B : ℝ} (hB : B ∈ upperBounds (value x '' chart)) :
      sSup (faces x) ≤ B := by
    apply csSup_le
    · exact ⟨_, Or.inr (Or.inl ⟨0,1,zero_lt_one,rfl⟩)⟩
    · exact (face_le_iff x B).mp (upper_reaches_faces x fun p hp ↦ hB ⟨p,hp,rfl⟩)
  have hFaces (h : ∀ b, 0 ≤ TDist.exponent x b 2) : IsLUB (faces x) (sSup (faces x)) :=
    isLUB_csSup ⟨_, Or.inr (Or.inl ⟨0,1,zero_lt_one,rfl⟩)⟩ (faces_bounded x hn h)
  constructor
  · constructor
    · intro hu
      by_contra! h
      have hf := (face_le_iff x _).mpr (hFaces h).1
      by_cases hex : ∃ p ∈ chart, sSup (faces x) ≤ value x p
      · obtain ⟨p,hp,hp'⟩ := hex
        obtain ⟨z,hz,hm⟩ := max_of_witness x hn h hp
          ⟨fun b a ha ↦ (hf.1 b a ha).trans hp', fun b a ha ↦ (hf.2.1 b a ha).trans hp',
            fun b hb ↦ (hf.2.2 b hb).trans hp'⟩
        exact hu ⟨value x z, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
      · push Not at hex
        exact hu ⟨sSup (faces x), by rintro _ ⟨q,hq,rfl⟩; exact (hex q hq).le⟩
    · rintro ⟨b,hb⟩ ⟨B,hB⟩
      have hb' : (2 : ℝ) * n < 3 * (TDist.multiplicity x b : ℝ) := by
        unfold TDist.exponent at hb
        linarith
      obtain ⟨v,hv,hv'⟩ := StudentFaces.std_supercritical_atom x b (by exact_mod_cast hb')
      have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
      obtain ⟨a,ha,hbad⟩ := (hpos.and (hv'.eventually_gt_atTop B)).exists
      exact hbad.not_ge (show value x (b,a,v) ≤ B from hB ⟨(b,a,v),⟨ha,hv⟩,rfl⟩)
  · intro h
    have hF := hFaces h
    refine ⟨hF, ?_⟩
    constructor
    · intro hnmax
      have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ chart) : value x p < sSup (faces x) := by
        by_contra! hge
        have h' := (face_le_iff x _).mpr hF.1
        exact hnmax (max_of_witness x hn h hp
          ⟨fun b a ha ↦ (h'.1 b a ha).trans hge, fun b a ha ↦ (h'.2.1 b a ha).trans hge,
            fun b hb ↦ (h'.2.2 b hb).trans hge⟩)
      exact ⟨⟨by rintro _ ⟨q,hq,rfl⟩; exact (hstrict q hq).le, fun _ hB ↦ hBound hB⟩, hstrict⟩
    · rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
      exact (hlt p hp).not_ge (hL.2 (show value x p ∈ upperBounds (value x '' chart) by
        rintro _ ⟨q,hq,rfl⟩
        exact hm hq))

/-- With mean held, the matched Gaussian and the same-mean Student-two
or critical atom determine the excluded height. The strict supercritical
branch is unbounded, while a stationary candidate tied with the excluded
height is sufficient for a finite fit. -/
theorem std_held_mean_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) (b : ℝ) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi (2 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x b p.2 p.1
    let E := TDist.exponent x b 2
    let F : Set ℝ := {y | (∃ a > 0, y = ∏ i, TDist.normalDensity b a (x i)) ∨
      (∃ a > 0, y = TDist.likelihood x b 2 a) ∨
      (E = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}
    ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 ≤ E ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x b q.2 q.1) p = 0 ∧ sSup F ≤ L p) ∧
    (0 ≤ E → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F)) := by
  dsimp only
  let D := Ioi (0 : ℝ) ×ˢ Ioi (2 : ℝ)
  let L := fun p : ℝ × ℝ ↦ TDist.likelihood x b p.2 p.1
  let E := TDist.exponent x b 2
  let F : Set ℝ := {y | (∃ a > 0, y = ∏ i, TDist.normalDensity b a (x i)) ∨
    (∃ a > 0, y = TDist.likelihood x b 2 a) ∨
    (E = 0 ∧ y = exp (TDist.collapseLogValue x b 2))}
  change ((¬ BddAbove (L '' D)) ↔ E < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ 0 ≤ E ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x b q.2 q.1) p = 0 ∧ sSup F ≤ L p) ∧
    (0 ≤ E → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
  have hne : F.Nonempty := ⟨_, Or.inr (Or.inl ⟨1,zero_lt_one,rfl⟩)⟩
  have hface {B : ℝ} (h : faceLEOn x ({b} : Set ℝ) B) :
      B ∈ upperBounds F := by
    rintro _ (⟨a,ha,rfl⟩ | ⟨a,ha,rfl⟩ | ⟨hb,rfl⟩)
    · exact h.1 b (mem_singleton b) a ha
    · exact h.2.1 b (mem_singleton b) a ha
    · exact h.2.2 b (mem_singleton b) hb
  have hfaces (he : 0 ≤ E) : IsLUB F (sSup F) := by
    have hx : ∃ i, x i ≠ b := by
      by_contra! h
      have H : 0 ≤ TDist.exponent x b 2 := he
      rw [TDist.exponent_all_tied x b 2 h] at H
      have hn' : (0 : ℝ) < n := by exact_mod_cast hn
      linarith
    obtain ⟨i,hi⟩ := hx
    let d := (x i - b)^2
    let B := exp ((n : ℝ) * (-(log (2*π)/2)) + n/2 * (log n - log d - 1))
    have hN (a : ℝ) (ha : 0 < a) : (∏ i, TDist.normalDensity b a (x i)) ≤ B := by
      apply gaussian_bound_at x hn b a ha d (sq_pos_of_ne_zero (sub_ne_zero.mpr hi))
      exact Finset.single_le_sum (f := fun k ↦ (x k-b)^2)
        (fun k _ ↦ sq_nonneg (x k-b)) (Finset.mem_univ i)
    have hT : ∃ C, (∀ a > 0, TDist.likelihood x b 2 a ≤ C) ∧
        (E = 0 → exp (TDist.collapseLogValue x b 2) ≤ C) := by
      rcases he.eq_or_lt with he | he
      · refine ⟨exp (TDist.collapseLogValue x b 2), ?_, fun _ ↦ le_rfl⟩
        exact fun a ha ↦ ((TDist.heldLocation_likelihood_equality x b
          (by norm_num : (0 : ℝ) < 2) ⟨i,hi⟩ he.symm).2.2 a ha).le
      · obtain ⟨s,hs,hm⟩ := (TDist.heldLocation_maximum x b
          (by norm_num : (0 : ℝ) < 2) ⟨i,hi⟩).1.mpr he
        exact ⟨TDist.likelihood x b 2 s, fun a ha ↦ exp_le_exp.mpr (hm ha),
          fun h ↦ (he.ne' h).elim⟩
    obtain ⟨C,hC,hE⟩ := hT
    refine isLUB_csSup hne ?_
    refine ⟨max B C, ?_⟩
    apply hface
    refine ⟨?_, ?_, ?_⟩
    · rintro c (rfl : c = b) a ha
      exact (hN a ha).trans (le_max_left _ _)
    · rintro c (rfl : c = b) a ha
      exact (hC a ha).trans (le_max_right _ _)
    · rintro c (rfl : c = b) h
      exact (hE h).trans (le_max_right _ _)
  have hreach {B : ℝ} (h : ∀ p ∈ D, L p ≤ B) : sSup F ≤ B := by
    apply csSup_le hne
    apply hface
    apply upper_reaches_facesOn x
    rintro ⟨m,a,v⟩ hp (rfl : m = b)
    exact h (a,v) hp
  have hmax (he : 0 ≤ E) (p : ℝ × ℝ) (hp : p ∈ D) (h : sSup F ≤ L p) :
      ∃ q ∈ D, IsMaxOn L D q := by
    have hB := (hfaces he).1
    have H : faceLEOn x ({b} : Set ℝ) (value x (b,p.1,p.2)) := by
      refine ⟨?_, ?_, ?_⟩
      · rintro m (rfl : m = b) a ha
        exact (hB (Or.inl ⟨a,ha,rfl⟩)).trans h
      · rintro m (rfl : m = b) a ha
        exact (hB (Or.inr (Or.inl ⟨a,ha,rfl⟩))).trans h
      · rintro m (rfl : m = b) hb
        exact (hB (Or.inr (Or.inr ⟨hb,rfl⟩))).trans h
    obtain ⟨⟨m,a,v⟩, hz, hm⟩ := max_of_witnessOn x hn (M := ({b} : Set ℝ))
      isClosed_singleton (by rintro c (rfl : c = b); exact he)
      (z := (b,p.1,p.2)) ⟨hp,mem_singleton b⟩ H
    have hb : m = b := hz.2
    subst m
    refine ⟨(a,v), hz.1, fun q hq ↦ ?_⟩
    have Hq : value x (b,q.1,q.2) ≤ value x (b,a,v) :=
      hm (show (b,q.1,q.2) ∈ chart ∩ {p | p.1 ∈ ({b} : Set ℝ)} from ⟨hq,mem_singleton b⟩)
    exact Hq
  have hu (he : E < 0) : ¬ BddAbove (L '' D) := by
    have he' : (2 : ℝ) * n < 3 * (TDist.multiplicity x b : ℝ) := by
      change TDist.exponent x b 2 < 0 at he
      unfold TDist.exponent at he
      linarith
    obtain ⟨v,hv,hv'⟩ := StudentFaces.std_supercritical_atom x b (by exact_mod_cast he')
    rintro ⟨B,hB⟩
    have hpos : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha,hbad⟩ := (hpos.and (hv'.eventually_gt_atTop B)).exists
    exact hbad.not_ge (show L (a,v) ≤ B from hB ⟨(a,v), ⟨ha,hv⟩, rfl⟩)
  have hSup (he : 0 ≤ E) (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F := by
    have hs (p : ℝ × ℝ) (hp : p ∈ D) : L p < sSup F :=
      lt_of_not_ge fun h ↦ hnm (hmax he p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hs p hp).le,
      fun B hB ↦ hreach fun p hp ↦ hB ⟨p,hp,rfl⟩⟩, hs⟩
  refine ⟨⟨?_,hu⟩, ⟨?_,?_⟩, ?_⟩
  · intro h
    by_contra! he
    by_cases hm : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hp'⟩ := hm
      exact h ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hp' hq⟩
    · exact h ⟨sSup F, (hSup he hm).1.1⟩
  · rintro ⟨p,hp,hm⟩
    have he : 0 ≤ E := le_of_not_gt fun h ↦ hu h
      ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    have hOpen : IsOpen D := isOpen_Ioi.prod isOpen_Ioi
    have hLog : IsMaxOn (fun q : ℝ × ℝ ↦ TDist.logLik x b q.2 q.1) D p :=
      fun _ hq ↦ exp_le_exp.mp (hm hq)
    exact ⟨he,p,hp,(hLog.isLocalMax (hOpen.mem_nhds hp)).fderiv_eq_zero,
      hreach fun _ hq ↦ hm hq⟩
  · rintro ⟨he,p,hp,_,h⟩
    exact hmax he p hp h
  · intro he
    refine ⟨hfaces he, ⟨hSup he, ?_⟩⟩
    rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq))

private theorem ordinaryScale_continuousAt (s : ℝ) {q : ℝ} (hq : 0 < q) :
    ContinuousAt (fun v ↦ TDist.ordinaryScale v s) q := by
  unfold TDist.ordinaryScale
  exact continuousAt_const.mul (((continuousAt_id.sub continuousAt_const).div
    continuousAt_id hq.ne').sqrt)

private theorem ordinaryScale_atTop (s : ℝ) :
    Tendsto (fun v ↦ TDist.ordinaryScale v s) atTop (𝓝 s) := by
  have h : Tendsto (fun v : ℝ ↦ (v - 2) / v) atTop (𝓝 1) := by
    have H := (tendsto_const_nhds (x := (1 : ℝ))).sub
      ((tendsto_const_nhds (x := (2 : ℝ))).div_atTop tendsto_id)
    simp only [sub_zero] at H
    refine H.congr' ?_
    filter_upwards [eventually_gt_atTop (0 : ℝ)] with v hv
    change 1 - 2 / v = (v - 2) / v
    field_simp
  simpa [TDist.ordinaryScale] using h.sqrt.const_mul s

/-- At held positive standard deviation, the only competitive excluded
heights are the same-sd Gaussian and critical collapse. A supercritical
atom gives unbounded likelihood. In the bounded regime a stationary
candidate reaching the excluded height, including equality, gives a fit. -/
theorem std_held_sd_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {σ : ℝ} (hσ : 0 < σ) :
    let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
    let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik x p.1 p.2 σ)
    let E := fun b ↦ TDist.exponent x b 2
    let F : Set ℝ := {y | (∃ b, y = ∏ i, TDist.normalDensity b σ (x i)) ∨
      (∃ b, E b = 0 ∧ y = exp (StudentFaces.fixedSdCollapseLog x b))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 0 ≤ E b) ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.stdLogLik x q.1 q.2 σ) p = 0 ∧ sSup F ≤ L p) ∧
    ((∀ b, 0 ≤ E b) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F)) := by
  dsimp only
  let D := (univ : Set ℝ) ×ˢ Ioi (2 : ℝ)
  let L := fun p : ℝ × ℝ ↦ exp (TDist.stdLogLik x p.1 p.2 σ)
  let E := fun b ↦ TDist.exponent x b 2
  let F : Set ℝ := {y | (∃ b, y = ∏ i, TDist.normalDensity b σ (x i)) ∨
    (∃ b, E b = 0 ∧ y = exp (StudentFaces.fixedSdCollapseLog x b))}
  change ((¬ BddAbove (L '' D)) ↔ ∃ b, E b < 0) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 0 ≤ E b) ∧ ∃ p ∈ D,
      fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.stdLogLik x q.1 q.2 σ) p = 0 ∧ sSup F ≤ L p) ∧
    ((∀ b, 0 ≤ E b) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔
        IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
  have heq (b v : ℝ) (hv : 2 < v) :
      L (b,v) = value x (b, TDist.ordinaryScale v σ, v) := by
    change exp (TDist.stdLogLik x b v σ) = _
    rw [TDist.stdLogLik_eq x b hv hσ]
    rfl
  have hscale (v : ℝ) (hv : 2 < v) : 0 < TDist.ordinaryScale v σ := by
    unfold TDist.ordinaryScale
    exact mul_pos hσ (sqrt_pos.mpr (div_pos (sub_pos.mpr hv) (by linarith)))
  have hcritical (b : ℝ) (hb : E b = 0) :
      exp (StudentFaces.fixedSdCollapseLog x b) = exp (TDist.collapseLogValue x b 2) := by
    change TDist.exponent x b 2 = 0 at hb
    unfold TDist.exponent at hb
    unfold StudentFaces.fixedSdCollapseLog TDist.collapseLogValue
    congr 1
    linear_combination -(log (2 : ℝ) / 2) * hb
  have hne : F.Nonempty := ⟨_, Or.inl ⟨0,rfl⟩⟩
  have hfaces (he : ∀ b, 0 ≤ E b) : IsLUB F (sSup F) := by
    refine isLUB_csSup hne ?_
    let N := exp (-(log (2*π)/2) - log σ)
    have hN (b : ℝ) : (∏ i, TDist.normalDensity b σ (x i)) ≤ ∏ _ : Fin n, N := by
      apply Finset.prod_le_prod₀ (fun _ _ ↦ (exp_pos _).le)
      intro i _
      apply exp_le_exp.mpr
      exact sub_le_self _ (by positivity)
    obtain ⟨B,hB⟩ := faces_bounded x hn he
    refine ⟨max (∏ _ : Fin n, N) B, ?_⟩
    rintro _ (⟨b,rfl⟩ | ⟨b,hb,rfl⟩)
    · exact (hN b).trans (le_max_left _ _)
    · rw [hcritical b hb]
      exact (hB (Or.inr (Or.inr ⟨b,hb,rfl⟩))).trans (le_max_right _ _)
  have hreach {B : ℝ} (h : ∀ p ∈ D, L p ≤ B) : sSup F ≤ B := by
    refine csSup_le hne ?_
    rintro _ (⟨b,rfl⟩ | ⟨b,hb,rfl⟩)
    · apply le_of_tendsto (StudentFaces.std_fixed_sd_normal x b hσ)
      filter_upwards [eventually_gt_atTop (2 : ℝ)] with v hv
      exact h (b,v) ⟨mem_univ _,hv⟩
    · apply le_of_tendsto ((StudentFaces.std_fixed_sd_collapse x b hσ).2.1 hb)
      filter_upwards [self_mem_nhdsWithin] with v hv
      exact h (b,v) ⟨mem_univ _,hv⟩
  have hmax (he : ∀ b, 0 ≤ E b) (p : ℝ × ℝ) (hp : p ∈ D) (h : sSup F ≤ L p) :
      ∃ q ∈ D, IsMaxOn L D q := by
    by_cases hm : IsMaxOn L D p
    · exact ⟨p,hp,hm⟩
    rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    have hB := (hfaces he).1
    let K := {q ∈ D | L w ≤ L q}
    have hK : IsCompact K := IsSeqCompact.isCompact (by
      intro y hy
      let u : ℕ → ℝ × ℝ × ℝ := fun j ↦ ((y j).1, TDist.ordinaryScale (y j).2 σ, (y j).2)
      have huv (j : ℕ) : 2 < (y j).2 := (hy j).1.2
      have hval (j : ℕ) : L w ≤ value x (u j) :=
        (hy j).2.trans (heq _ _ (huv j)).le
      obtain ⟨z,hz,_,φ,hφ,hlim,hv⟩ := superlevel_subseq x hn (M := univ) isClosed_univ
        (fun b _ ↦ he b) (h.trans_lt hw') (exp_pos _) u
        (fun j ↦ ⟨hscale _ (huv j), huv j⟩) (fun _ ↦ mem_univ _) hval
        (by
          intro ψ b s _ _ hs ht
          have hS := (ordinaryScale_atTop σ).comp ht
          have hs' : s = σ := tendsto_nhds_unique hs hS
          subst s
          exact hB (Or.inl ⟨b,rfl⟩))
        (by
          intro ψ b s _ hs hs' ht
          have hS := (ordinaryScale_continuousAt σ (show 0 < (2 : ℝ) by norm_num)).tendsto.comp ht
          have hzero : s = 0 := (tendsto_nhds_unique hs' hS).trans (by simp [TDist.ordinaryScale])
          exact (hs.ne' hzero).elim)
        (fun b _ hb ↦ (le_of_eq (hcritical b hb).symm).trans (hB (Or.inr ⟨b,hb,rfl⟩)))
      have hν := ((continuous_snd.comp continuous_snd).tendsto z).comp hlim
      have ha := ((continuous_fst.comp continuous_snd).tendsto z).comp hlim
      change Tendsto (fun j ↦ (y (φ j)).2) atTop (𝓝 z.2.2) at hν
      change Tendsto (fun j ↦ TDist.ordinaryScale (y (φ j)).2 σ) atTop (𝓝 z.2.1) at ha
      have he' : z.2.1 = TDist.ordinaryScale z.2.2 σ :=
        tendsto_nhds_unique ha ((ordinaryScale_continuousAt σ (show 0 < z.2.2 by
          have := hz.2; linarith)).tendsto.comp hν)
      have hz' : (z.1,z.2.2) ∈ D := ⟨mem_univ _,hz.2⟩
      have hL : value x z = L (z.1,z.2.2) := by
        rw [heq _ _ hz.2]
        unfold value
        rw [he']
      refine ⟨(z.1,z.2.2), ⟨hz', ?_⟩, φ, hφ, ?_⟩
      · rw [← hL]
        exact ge_of_tendsto' hv fun j ↦ hval (φ j)
      · exact ((continuous_fst.tendsto z).comp hlim).prodMk_nhds hν)
    have hc : ContinuousOn L D := by
      intro q hq
      have hpos : 2 < q.2 := hq.2
      have hlim := finite_shape_face x (hscale q.2 hpos) (show 0 < q.2 by linarith)
        (m := Prod.fst) (v := Prod.snd) (a := fun p : ℝ × ℝ ↦ TDist.ordinaryScale p.2 σ)
        (continuous_fst.tendsto q) (((ordinaryScale_continuousAt σ (show 0 < q.2 by
          linarith)).tendsto).comp (continuous_snd.tendsto q)) (continuous_snd.tendsto q)
      change Tendsto L (𝓝[D] q) (𝓝 (L q))
      rw [show L q = value x (q.1,TDist.ordinaryScale q.2 σ,q.2) from heq _ _ hpos]
      refine (hlim.mono_left nhdsWithin_le_nhds).congr' ?_
      filter_upwards [self_mem_nhdsWithin (s := D)] with z hz
      exact (heq _ _ hz.2).symm
    obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ (hc.mono fun _ h ↦ h.1)
    exact ⟨q,hq.1,fun z hz ↦ if h : L w ≤ L z then hqm ⟨hz,h⟩ else (le_of_not_ge h).trans hq.2⟩
  have hu (b : ℝ) (he : E b < 0) : ¬ BddAbove (L '' D) := by
    rintro ⟨B,hB⟩
    have hp : ∀ᶠ v : ℝ in 𝓝[>] 2, 2 < v := self_mem_nhdsWithin
    obtain ⟨v,hv,hgt⟩ := (hp.and
      (((StudentFaces.std_fixed_sd_collapse x b hσ).2.2 he).eventually_gt_atTop B)).exists
    exact hgt.not_ge (show L (b,v) ≤ B from hB ⟨(b,v),⟨mem_univ _,hv⟩,rfl⟩)
  have hSup (he : ∀ b, 0 ≤ E b) (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F := by
    have hs (p : ℝ × ℝ) (hp : p ∈ D) : L p < sSup F :=
      lt_of_not_ge fun h ↦ hnm (hmax he p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hs p hp).le,
      fun B hB ↦ hreach fun p hp ↦ hB ⟨p,hp,rfl⟩⟩, hs⟩
  refine ⟨⟨?_,?_⟩, ⟨?_,?_⟩, ?_⟩
  · intro h
    by_contra! he
    by_cases hm : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hp'⟩ := hm
      exact h ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hp' hq⟩
    · exact h ⟨sSup F, (hSup he hm).1.1⟩
  · rintro ⟨b,hb⟩
    exact hu b hb
  · rintro ⟨p,hp,hm⟩
    have he (b : ℝ) : 0 ≤ E b := le_of_not_gt fun h ↦ hu b h
      ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    have hOpen : IsOpen D := isOpen_univ.prod isOpen_Ioi
    have hLog : IsMaxOn (fun q : ℝ × ℝ ↦ TDist.stdLogLik x q.1 q.2 σ) D p :=
      fun _ hq ↦ exp_le_exp.mp (hm hq)
    exact ⟨he,p,hp,(hLog.isLocalMax (hOpen.mem_nhds hp)).fderiv_eq_zero,
      hreach fun _ hq ↦ hm hq⟩
  · rintro ⟨he,p,hp,_,h⟩
    exact hmax he p hp h
  · intro he
    refine ⟨hfaces he, ⟨hSup he, ?_⟩⟩
    rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq))

/-- The fitted ordinary location–scale Student chart includes the Cauchy
floor. A strict majority gives divergence. Otherwise a finite maximizer
exists exactly when an interior stationary or conditional Cauchy maximum
reaches the Gaussian and critical collapse face heights. -/
theorem lst_all_free_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) :
    let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2}
    let L := fun p : ℝ × ℝ × ℝ ↦ TDist.likelihood x p.1 p.2.2 p.2.1
    let F : Set ℝ := {y | (∃ b s, 0 < s ∧ y = ∏ i, TDist.normalDensity b s (x i)) ∨
      (∃ b, TDist.exponent x b 1 = 0 ∧ y = exp (TDist.collapseLogValue x b 1))}
    ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2 ∧
          fderiv ℝ (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) p = 0) ∨
        (p.2.2 = 1 ∧ ∀ b s, 0 < s → TDist.likelihood x b 1 s ≤ L p)) ∧ sSup F ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F)) := by
  dsimp only
  let D : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 ≤ p.2.2}
  let L := value x
  let F : Set ℝ := {y | (∃ b s, 0 < s ∧ y = ∏ i, TDist.normalDensity b s (x i)) ∨
    (∃ b, TDist.exponent x b 1 = 0 ∧ y = exp (TDist.collapseLogValue x b 1))}
  change ((¬ BddAbove (L '' D)) ↔ ∃ b, n < 2 * TDist.multiplicity x b) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ (∀ b, 2 * TDist.multiplicity x b ≤ n) ∧ ∃ p ∈ D,
      ((1 < p.2.2 ∧
          fderiv ℝ (fun z : ℝ × ℝ × ℝ ↦ TDist.logLik x z.1 z.2.2 z.2.1) p = 0) ∨
        (p.2.2 = 1 ∧ ∀ b s, 0 < s → TDist.likelihood x b 1 s ≤ L p)) ∧ sSup F ≤ L p) ∧
    ((∀ b, 2 * TDist.multiplicity x b ≤ n) → IsLUB F (sSup F) ∧
      ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F))
  have hmass (b : ℝ) : 0 ≤ TDist.exponent x b 1 ↔ 2 * TDist.multiplicity x b ≤ n := by
    have H : (2 * TDist.multiplicity x b ≤ n) ↔
        (2 * (TDist.multiplicity x b : ℝ) ≤ (n : ℝ)) := by exact_mod_cast Iff.rfl
    rw [H]
    unfold TDist.exponent
    constructor <;> intro h <;> linarith
  have hne : F.Nonempty := ⟨_, Or.inl ⟨0,1,zero_lt_one,rfl⟩⟩
  have hfaces (he : ∀ b, 0 ≤ TDist.exponent x b 1) : IsLUB F (sSup F) := by
    have hnon : ∃ i j, x i ≠ x j := by
      by_contra! h
      let i : Fin n := ⟨0,hn⟩
      have H := he (x i)
      rw [TDist.exponent_all_tied x (x i) 1 (fun j ↦ h j i)] at H
      have hn' : (0 : ℝ) < n := by exact_mod_cast hn
      linarith
    obtain ⟨B,hB⟩ := gaussian_bound x hn hnon
    let C := ∑ i, exp (TDist.collapseLogValue x (x i) 1)
    refine isLUB_csSup hne ⟨max B C, ?_⟩
    rintro _ (⟨b,a,ha,rfl⟩ | ⟨b,hb,rfl⟩)
    · exact (hB b a ha).trans (le_max_left _ _)
    · have hk : 0 < TDist.multiplicity x b := by
        by_contra! hz
        have hz' : TDist.multiplicity x b = 0 := Nat.eq_zero_of_le_zero hz
        have hn' : (0 : ℝ) < n := by exact_mod_cast hn
        unfold TDist.exponent at hb
        rw [hz', Nat.cast_zero, sub_zero, sub_zero, one_mul] at hb
        exact hn'.ne' hb
      obtain ⟨i,hi⟩ := Finset.card_pos.mp hk
      rw [← (Finset.mem_filter.mp hi).2]
      exact (Finset.single_le_sum (f := fun j ↦ exp (TDist.collapseLogValue x (x j) 1))
        (fun _ _ ↦ (exp_pos _).le) (Finset.mem_univ i)).trans (le_max_right _ _)
  have hreach {B : ℝ} (h : ∀ p ∈ D, L p ≤ B) : sSup F ≤ B := by
    refine csSup_le hne ?_
    have bound (b a v : ℝ) (ha : 0 < a) (hv : 1 ≤ v) : TDist.likelihood x b v a ≤ B :=
      show L (b,a,v) ≤ B from h _ ⟨ha,hv⟩
    rintro _ (⟨b,a,ha,rfl⟩ | ⟨b,hb,rfl⟩)
    · apply le_of_tendsto (Student2Normal.moving_normal_face x ha
        (l := atTop) (m := fun _ : ℝ ↦ b) (a := fun _ ↦ a) (v := id)
        tendsto_const_nhds tendsto_const_nhds tendsto_id)
      filter_upwards [eventually_ge_atTop (1 : ℝ)] with v hv
      exact bound b a v ha hv
    · have H := StudentFaces.lst_fixed_df_local_collapse x b
        (by norm_num : (0 : ℝ) < 1) (l := 𝓝[>] (0 : ℝ))
        (m := fun _ ↦ b) (a := id) tendsto_const_nhds tendsto_id
      apply le_of_tendsto (H.2 hb).1
      filter_upwards [self_mem_nhdsWithin (s := Ioi (0 : ℝ))] with a ha
      exact bound b a 1 ha le_rfl
  have hmax (he : ∀ b, 0 ≤ TDist.exponent x b 1) (p : ℝ × ℝ × ℝ) (hp : p ∈ D)
      (h : sSup F ≤ L p) : ∃ q ∈ D, IsMaxOn L D q := by
    by_cases hm : IsMaxOn L D p
    · exact ⟨p,hp,hm⟩
    rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    have hF := (hfaces he).1
    let K := {q ∈ D | L w ≤ L q}
    have hK : IsCompact K := IsSeqCompact.isCompact (by
      intro u hu
      obtain ⟨z,hz,_,φ,hφ,hlim,hv⟩ := superlevel_subseqAt x hn 1 le_rfl true
        (M := univ) isClosed_univ (fun b _ ↦ he b) (h.trans_lt hw') (exp_pos _) u
        (fun j ↦ (hu j).1) (fun _ ↦ mem_univ _) (fun j ↦ (hu j).2)
        (fun _ b s _ hs _ _ ↦ hF (Or.inl ⟨b,s,hs,rfl⟩))
        (fun _ _ _ _ _ _ _ hFalse ↦ by cases hFalse)
        (fun b _ hb ↦ hF (Or.inr ⟨b,hb,rfl⟩))
      exact ⟨z, ⟨hz,ge_of_tendsto' hv fun j ↦ (hu (φ j)).2⟩, φ,hφ,hlim⟩)
    have hc : ContinuousOn L K := by
      intro z hz
      obtain ⟨ha,hv⟩ := (show 0 < z.2.1 ∧ 1 ≤ z.2.2 from hz.1)
      have H := finite_shape_face x ha (zero_lt_one.trans_le hv)
        (continuous_fst.tendsto z) ((continuous_fst.comp continuous_snd).tendsto z)
        ((continuous_snd.comp continuous_snd).tendsto z)
      exact H.mono_left nhdsWithin_le_nhds
    obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ hc
    exact ⟨q,hq.1,fun z hz ↦ if h' : L w ≤ L z then hqm ⟨hz,h'⟩ else (le_of_not_ge h').trans hq.2⟩
  have hu (b : ℝ) (hb : n < 2 * TDist.multiplicity x b) : ¬ BddAbove (L '' D) := by
    have hb' : TDist.exponent x b 1 < 0 := lt_of_not_ge
      (fun h ↦ hb.not_ge ((hmass b).mp h))
    rintro ⟨B,hB⟩
    have ha : ∀ᶠ a : ℝ in 𝓝[>] 0, 0 < a := self_mem_nhdsWithin
    obtain ⟨a,ha',hbad⟩ := (ha.and ((TDist.collapse_unbounded x b zero_lt_one hb').eventually_gt_atTop B)).exists
    exact hbad.not_ge (show L (b,a,1) ≤ B from hB ⟨(b,a,1),⟨ha',le_rfl⟩,rfl⟩)
  have hSup (he : ∀ b, 0 ≤ TDist.exponent x b 1) (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) (sSup F) ∧ ∀ p ∈ D, L p < sSup F := by
    have hs (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : L p < sSup F :=
      lt_of_not_ge fun h ↦ hnm (hmax he p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hs p hp).le,
      fun B hB ↦ hreach fun p hp ↦ hB ⟨p,hp,rfl⟩⟩, hs⟩
  refine ⟨⟨?_,?_⟩, ⟨?_,?_⟩, ?_⟩
  · intro h
    by_contra! he
    by_cases hm : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hp'⟩ := hm
      exact h ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hp' hq⟩
    · exact h ⟨sSup F, (hSup (fun b ↦ (hmass b).mpr (he b)) hm).1.1⟩
  · rintro ⟨b,hb⟩
    exact hu b hb
  · rintro ⟨p,hp,hm⟩
    have he (b : ℝ) : 2 * TDist.multiplicity x b ≤ n := le_of_not_gt fun h ↦ hu b h
      ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    refine ⟨he,p,hp,?_,hreach fun _ hq ↦ hm hq⟩
    obtain ⟨ha,hv⟩ := (show 0 < p.2.1 ∧ 1 ≤ p.2.2 from hp)
    rcases hv.eq_or_lt with heq | hlt
    · refine Or.inr ⟨heq.symm, fun b s hs ↦ ?_⟩
      show L (b,s,1) ≤ L p
      exact hm (show (b,s,1) ∈ D from ⟨hs,le_rfl⟩)
    · refine Or.inl ⟨hlt, ?_⟩
      let U : Set (ℝ × ℝ × ℝ) := {p | 0 < p.2.1 ∧ 1 < p.2.2}
      have hOpen : IsOpen U :=
        (isOpen_lt continuous_const (continuous_fst.comp continuous_snd)).inter
          (isOpen_lt continuous_const (continuous_snd.comp continuous_snd))
      have hLog : IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ TDist.logLik x q.1 q.2.2 q.2.1) U p :=
        fun q (hq : 0 < q.2.1 ∧ 1 < q.2.2) ↦ exp_le_exp.mp (hm (show q ∈ D from ⟨hq.1,hq.2.le⟩))
      exact (hLog.isLocalMax (hOpen.mem_nhds ⟨ha,hlt⟩)).fderiv_eq_zero
  · rintro ⟨he,p,hp,_,h⟩
    exact hmax (fun b ↦ (hmass b).mpr (he b)) p hp h
  · intro he
    have he' (b : ℝ) : 0 ≤ TDist.exponent x b 1 := (hmass b).mpr (he b)
    refine ⟨hfaces he', ⟨hSup he', ?_⟩⟩
    rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq))

private theorem gaussian_at_held_scale {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n)
    {s : ℝ} (_hs : 0 < s) (m : ℝ) :
    (∏ i, TDist.normalDensity m s (x i)) ≤
      ∏ i, TDist.normalDensity ((∑ i, x i) / n) s (x i) := by
  let b := (∑ i, x i) / (n : ℝ)
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have hb : (∑ i, x i) = (n : ℝ) * b := by dsimp [b]; field_simp
  have hzero : ∑ i, (x i - b) = 0 := by
    simp [Finset.sum_sub_distrib, hb]
  have hsums : ∑ i, (x i - m)^2 = (∑ i, (x i - b)^2) + (n : ℝ) * (b - m)^2 := by
    calc
      ∑ i, (x i - m)^2 = ∑ i, ((x i - b)^2 + 2*(b-m)*(x i-b) + (b-m)^2) :=
        Finset.sum_congr rfl fun i _ ↦ by ring
      _ = _ := by simp [Finset.sum_add_distrib, ← Finset.mul_sum, hzero]
  change (∏ i, TDist.normalDensity m s (x i)) ≤ ∏ i, TDist.normalDensity b s (x i)
  simp_rw [TDist.normalDensity, ← exp_sum]
  apply exp_le_exp.mpr
  simp only [Finset.sum_sub_distrib, ← Finset.sum_div, Finset.sum_const, Finset.card_univ,
    Fintype.card_fin, nsmul_eq_mul]
  rw [hsums]
  exact sub_le_sub_left (div_le_div_of_nonneg_right (div_le_div_of_nonneg_right
    (le_add_of_nonneg_right (by positivity)) (sq_nonneg s)) (by norm_num : 0 ≤ (2 : ℝ))) _

/-- At held positive ordinary scale, the only competitive excluded face
is the normal at that scale, optimized over location. The included
Cauchy floor needs a conditional maximizing location. Finite equality
with the normal face counts as attainment. -/
theorem lst_held_scale_decision {n : ℕ} (x : Fin n → ℝ) (hn : 0 < n) {s : ℝ} (hs : 0 < s) :
    let D := (univ : Set ℝ) ×ˢ Ici (1 : ℝ)
    let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 p.2 s
    let N := ∏ i, TDist.normalDensity ((∑ j, x j) / n) s (x i)
    BddAbove (L '' D) ∧
    ((∃ p ∈ D, IsMaxOn L D p) ↔ ∃ p ∈ D,
      ((1 < p.2 ∧ fderiv ℝ (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 q.2 s) p = 0) ∨
        (p.2 = 1 ∧ ∀ b, TDist.likelihood x b 1 s ≤ L p)) ∧ N ≤ L p) ∧
    ((¬ ∃ p ∈ D, IsMaxOn L D p) ↔ IsLUB (L '' D) N ∧ ∀ p ∈ D, L p < N) := by
  dsimp only
  let D := (univ : Set ℝ) ×ˢ Ici (1 : ℝ)
  let L := fun p : ℝ × ℝ ↦ TDist.likelihood x p.1 p.2 s
  let b := (∑ j, x j) / (n : ℝ)
  let N := ∏ i, TDist.normalDensity b s (x i)
  have hreach {B : ℝ} (h : ∀ p ∈ D, L p ≤ B) : N ≤ B := by
    apply le_of_tendsto (Student2Normal.moving_normal_face x hs
      (l := atTop) (v := id) (m := fun _ : ℝ ↦ b) (a := fun _ ↦ s)
      tendsto_const_nhds tendsto_const_nhds tendsto_id)
    filter_upwards [eventually_ge_atTop (1 : ℝ)] with v hv
    exact h (b,v) ⟨mem_univ _,hv⟩
  have hmax (p : ℝ × ℝ) (hp : p ∈ D) (h : N ≤ L p) : ∃ q ∈ D, IsMaxOn L D q := by
    by_cases hm : IsMaxOn L D p
    · exact ⟨p,hp,hm⟩
    rw [IsMaxOn, IsMaxFilter, eventually_principal] at hm
    push Not at hm
    obtain ⟨w,hw,hw'⟩ := hm
    let K := {q ∈ D | L w ≤ L q}
    have hK : IsCompact K := IsSeqCompact.isCompact (by
      intro u hu
      let P : ℕ → EReal × EReal := fun j ↦ ((u j).1,(u j).2)
      obtain ⟨z,φ,hφ,hP⟩ := CompactSpace.tendsto_subseq P
      let m := fun j ↦ (u (φ j)).1
      let v := fun j ↦ (u (φ j)).2
      have hm : Tendsto (fun j ↦ (m j : EReal)) atTop (𝓝 z.1) :=
        (continuous_fst.tendsto z).comp hP
      have hv : Tendsto (fun j ↦ (v j : EReal)) atTop (𝓝 z.2) :=
        (continuous_snd.tendsto z).comp hP
      have hfloor : ∀ᶠ j in atTop, 1 ≤ v j := Eventually.of_forall fun j ↦ (hu (φ j)).1.2
      have hval : ∀ᶠ j in atTop, L w ≤ TDist.likelihood x (m j) (v j) s :=
        Eventually.of_forall fun j ↦ (hu (φ j)).2
      have hremote (h : Tendsto m atTop (cocompact ℝ)) : False := by
        have hz := Student2Normal.moving_shape_remote_ends x hn (Eventually.of_forall fun _ ↦ hs)
          hfloor (.inr h)
        exact (exp_pos _).not_ge (ge_of_tendsto hz hval)
      generalize z.1 = zm at hm
      generalize z.2 = zv at hv
      cases zm using EReal.rec with
      | bot =>
        exact (hremote ((EReal.tendsto_coe_nhds_bot_iff.mp hm).mono_right
          (by rw [cocompact_eq_atBot_atTop]; exact le_sup_left))).elim
      | top =>
        exact (hremote ((EReal.tendsto_coe_nhds_top_iff.mp hm).mono_right
          (by rw [cocompact_eq_atBot_atTop]; exact le_sup_right))).elim
      | coe c =>
        have hc := EReal.tendsto_coe.mp hm
        cases zv using EReal.rec with
        | bot =>
          obtain ⟨j,hj,hj'⟩ := (((EReal.tendsto_coe_nhds_bot_iff.mp hv).eventually_lt_atBot 1).and hfloor).exists
          exact (hj.not_ge hj').elim
        | top =>
          have hlim := Student2Normal.moving_normal_face x hs hc tendsto_const_nhds
            (EReal.tendsto_coe_nhds_top_iff.mp hv)
          exact ((h.trans_lt hw').not_ge ((ge_of_tendsto hlim hval).trans
            (gaussian_at_held_scale x hn hs c))).elim
        | coe q =>
          have hq := EReal.tendsto_coe.mp hv
          have hq' := ge_of_tendsto hq hfloor
          have hlim := finite_shape_face x hs (zero_lt_one.trans_le hq') hc tendsto_const_nhds hq
          exact ⟨(c,q), ⟨⟨mem_univ _,hq'⟩,ge_of_tendsto hlim hval⟩, φ, hφ,
            hc.prodMk_nhds hq⟩)
    have hc : ContinuousOn L K := by
      intro p hp
      exact (finite_shape_face x hs (zero_lt_one.trans_le hp.1.2)
        (continuous_fst.tendsto p) tendsto_const_nhds
        (continuous_snd.tendsto p)).mono_left nhdsWithin_le_nhds
    obtain ⟨q,hq,hqm⟩ := hK.exists_isMaxOn ⟨w,hw,le_rfl⟩ hc
    exact ⟨q,hq.1,fun z hz ↦ if hh : L w ≤ L z then hqm ⟨hz,hh⟩ else (le_of_not_ge hh).trans hq.2⟩
  have hSup (hnm : ¬ ∃ p ∈ D, IsMaxOn L D p) :
      IsLUB (L '' D) N ∧ ∀ p ∈ D, L p < N := by
    have hlt (p : ℝ × ℝ) (hp : p ∈ D) : L p < N :=
      lt_of_not_ge fun h ↦ hnm (hmax p hp h)
    exact ⟨⟨by rintro _ ⟨p,hp,rfl⟩; exact (hlt p hp).le,
      fun B hB ↦ hreach fun p hp ↦ hB ⟨p,hp,rfl⟩⟩, hlt⟩
  refine ⟨?_, ⟨?_,?_⟩, ⟨hSup,?_⟩⟩
  · by_cases h : ∃ p ∈ D, IsMaxOn L D p
    · obtain ⟨p,hp,hm⟩ := h
      exact ⟨L p, by rintro _ ⟨q,hq,rfl⟩; exact hm hq⟩
    · exact ⟨N,(hSup h).1.1⟩
  · rintro ⟨p,hp,hm⟩
    refine ⟨p,hp,?_,hreach fun _ hq ↦ hm hq⟩
    rcases (show 1 ≤ p.2 from hp.2).eq_or_lt with h | h
    · refine Or.inr ⟨h.symm,fun b' ↦ ?_⟩
      show L (b',1) ≤ L p
      exact hm (show (b',1) ∈ D from ⟨mem_univ _, le_rfl⟩)
    · refine Or.inl ⟨h,?_⟩
      let U := (univ : Set ℝ) ×ˢ Ioi (1 : ℝ)
      have hOpen : IsOpen U := isOpen_univ.prod isOpen_Ioi
      have hLog : IsMaxOn (fun q : ℝ × ℝ ↦ TDist.logLik x q.1 q.2 s) U p :=
        fun q hq ↦ exp_le_exp.mp (hm (show q ∈ D from ⟨mem_univ _,(show 1 < q.2 from hq.2).le⟩))
      exact (hLog.isLocalMax (hOpen.mem_nhds ⟨mem_univ _,h⟩)).fderiv_eq_zero
  · rintro ⟨p,hp,_,h⟩
    exact hmax p hp h
  · rintro ⟨hL,hlt⟩ ⟨p,hp,hm⟩
    exact (hlt p hp).not_ge (hL.2 (show L p ∈ upperBounds (L '' D) by
      rintro _ ⟨q,hq,rfl⟩
      exact hm hq))

end UnivariateML.Student2Free
end
