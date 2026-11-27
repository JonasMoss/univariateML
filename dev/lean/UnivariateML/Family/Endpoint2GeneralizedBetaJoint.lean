module

public import UnivariateML.Family.Endpoint2GeneralizedBeta
public import Mathlib.Analysis.Convex.Deriv

import Mathlib.Tactic

set_option linter.style.header false

/-!
# Generalized beta with both beta shapes held

In log-distance coordinates the intercept is power times the log ratio
of the endpoint to the sample maximum. At upper shape above one the
strictly concave objective has compact superlevels for nonconstant data.
-/

@[expose] public section

open Set Real Filter
open scoped Topology BigOperators

namespace UnivariateML.Endpoint2GeneralizedBetaJoint

noncomputable section

variable {n : ℕ}

private def penalty (v : ℝ) : ℝ := log (1 - exp (-v))

private theorem penalty_arg {v : ℝ} (hv : 0 < v) : 0 < 1 - exp (-v) :=
  sub_pos.mpr (exp_lt_one_iff.mpr (neg_neg_of_pos hv))

private theorem penalty_nonpos {v : ℝ} (hv : 0 < v) : penalty v ≤ 0 :=
  log_nonpos (penalty_arg hv).le (by linarith [exp_pos (-v)])

private theorem penalty_le_log {v : ℝ} (hv : 0 < v) : penalty v ≤ log v :=
  log_le_log (penalty_arg hv) (by linarith [add_one_le_exp (-v)])

private theorem penalty_deriv {v : ℝ} (hv : 0 < v) :
    HasDerivAt penalty (1 / (exp v - 1)) v := by
  have h := (((hasDerivAt_id v).neg.exp).const_sub 1).log (penalty_arg hv).ne'
  convert h using 1
  · rfl
  · simp only [Function.id_def, mul_neg, mul_one, neg_neg]
    change 1 / (exp v - 1) = exp (-v) / (1 - exp (-v))
    rw [exp_neg]
    have hpos : 0 < exp v - 1 := sub_pos.mpr (one_lt_exp_iff.mpr hv)
    field_simp [(exp_pos v).ne', hpos.ne']

private theorem penalty_strictConcave : StrictConcaveOn ℝ (Ioi 0) penalty := by
  apply StrictAntiOn.strictConcaveOn_of_deriv (convex_Ioi 0)
    (fun v hv ↦ (penalty_deriv hv).continuousAt.continuousWithinAt)
  intro v hv w _ hvw
  rw [interior_Ioi] at hv
  have hw : 0 < w := hv.trans hvw
  rw [(penalty_deriv hv).deriv, (penalty_deriv hw).deriv]
  exact one_div_lt_one_div_of_lt (sub_pos.mpr (one_lt_exp_iff.mpr hv))
    (by linarith [exp_lt_exp.mpr hvw])

private def chart (d : Fin n → ℝ) (p q : ℝ) (v : ℝ × ℝ) : ℝ :=
  (n : ℝ) * log v.1 - p * (v.1 * (∑ i, d i) + n * v.2) +
    (q - 1) * ∑ i, penalty (v.2 + v.1 * d i)

private def chartDomain : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi 0

private theorem chart_strictConcave (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (p : ℝ) {q : ℝ} (hq : 1 < q) :
    StrictConcaveOn ℝ chartDomain (chart d p q) := by
  have hN : (0 : ℝ) < n := Nat.cast_pos.mpr hn
  refine ⟨(convex_Ioi 0).prod (convex_Ioi 0), ?_⟩
  intro u hu v hv huv a b ha hb hab
  have harg (w : ℝ × ℝ) (hw : w ∈ chartDomain) (i : Fin n) : 0 < w.2 + w.1 * d i :=
    add_pos_of_pos_of_nonneg hw.2 (mul_nonneg hw.1.le (hd i))
  let w := a • u + b • v
  have he (i : Fin n) : w.2 + w.1 * d i =
      a * (u.2 + u.1 * d i) + b * (v.2 + v.1 * d i) := by
    change (a*u.2+b*v.2) + (a*u.1+b*v.1)*d i = _
    ring
  have hterm (i : Fin n) : a * penalty (u.2+u.1*d i) + b * penalty (v.2+v.1*d i) ≤
      penalty (w.2 + w.1 * d i) := by
    rw [he]
    exact penalty_strictConcave.concaveOn.2 (harg u hu i) (harg v hv i) ha.le hb.le hab
  have hsum := Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ hterm i
  rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum] at hsum
  have hlog := strictConcaveOn_log_Ioi.concaveOn.2 hu.1 hv.1 ha.le hb.le hab
  change a * log u.1 + b * log v.1 ≤ log w.1 at hlog
  change a * chart d p q u + b * chart d p q v < chart d p q w
  have hw₁ : w.1 = a * u.1 + b * v.1 := rfl
  have hw₂ : w.2 = a * u.2 + b * v.2 := rfl
  by_cases h : u.1 = v.1
  · let i : Fin n := ⟨0,hn⟩
    have hne : u.2 + u.1 * d i ≠ v.2 + v.1 * d i := by
      rw [h]
      intro hh
      exact huv (Prod.ext h (by linarith))
    have hsum' := Finset.sum_lt_sum (s := Finset.univ) (fun j _ ↦ hterm j)
      ⟨i, Finset.mem_univ _, by
        rw [he]
        exact penalty_strictConcave.2 (harg u hu i) (harg v hv i) hne ha hb hab⟩
    rw [Finset.sum_add_distrib, ← Finset.mul_sum, ← Finset.mul_sum, hw₁, hw₂] at hsum'
    unfold chart
    rw [hw₁, hw₂]
    rw [hw₁] at hlog
    linarith [mul_le_mul_of_nonneg_left hlog hN.le,
      mul_lt_mul_of_pos_left hsum' (sub_pos.mpr hq)]
  · have hlog' := strictConcaveOn_log_Ioi.2 hu.1 hv.1 h ha hb hab
    change a * log u.1 + b * log v.1 < log w.1 at hlog'
    rw [hw₁, hw₂] at hsum
    rw [hw₁] at hlog'
    unfold chart
    rw [hw₁, hw₂]
    linarith [mul_lt_mul_of_pos_left hlog' hN,
      mul_le_mul_of_nonneg_left hsum (sub_nonneg.mpr hq.le)]

private theorem chart_exists (hn : 0 < n) {d : Fin n → ℝ} (hd : ∀ i, 0 ≤ d i)
    (hpos : ∃ i, 0 < d i) (hzero : ∃ i, d i = 0) {p q : ℝ} (hp : 0 < p) (hq : 1 < q) :
    ∃ v ∈ chartDomain, IsMaxOn (chart d p q) chartDomain v := by
  let F := chart d p q
  let C := F (1,1)
  let N : ℝ := n
  let D := ∑ i, d i
  have hN : 0 < N := Nat.cast_pos.mpr hn
  have hD : 0 < D := by
    obtain ⟨i, hi⟩ := hpos
    exact Finset.sum_pos' (fun i _ ↦ hd i) ⟨i,Finset.mem_univ _,hi⟩
  obtain ⟨j,hj⟩ := hzero
  have hterm (v : ℝ × ℝ) (hv : v ∈ chartDomain) (i : Fin n) :
      penalty (v.2 + v.1 * d i) ≤ 0 :=
    penalty_nonpos (add_pos_of_pos_of_nonneg hv.2 (mul_nonneg hv.1.le (hd i)))
  have hf (v : ℝ × ℝ) (hv : v ∈ chartDomain) : F v ≤ N * log v.1 - p*D*v.1 - p*N*v.2 := by
    have hh := Finset.sum_nonpos (s := Finset.univ) fun i _ ↦ hterm v hv i
    unfold F chart
    change N * log v.1 - p * (v.1 * D + N * v.2) +
      (q - 1) * ∑ i, penalty (v.2 + v.1 * d i) ≤ _
    linarith [mul_nonpos_of_nonneg_of_nonpos (sub_nonneg.mpr hq.le) hh]
  have hh (v : ℝ × ℝ) (hv : v ∈ chartDomain) : F v ≤ N * log v.1 - p*D*v.1 :=
    (hf v hv).trans (sub_le_self _ (mul_nonneg (mul_pos hp hN).le hv.2.le))
  let A := exp ((C - 1) / N)
  have hA : 0 < A := exp_pos _
  have ht : Tendsto (fun c : ℝ ↦ N * log c - p*D*c) atTop atBot := by
    have hl : Tendsto (fun c : ℝ ↦ log c / c) atTop (𝓝 0) := by
      simpa using tendsto_pow_log_div_mul_add_atTop 1 0 1 one_ne_zero
    have he : ∀ᶠ c : ℝ in atTop, N * (log c / c) < p*D/2 :=
      (hl.const_mul N).eventually (Iio_mem_nhds (by simpa using half_pos (mul_pos hp hD)))
    apply tendsto_atBot_mono' _ _ (tendsto_id.const_mul_atTop_of_neg
      (neg_neg_of_pos (half_pos (mul_pos hp hD))))
    filter_upwards [he, eventually_gt_atTop (0 : ℝ)] with c hc hcp
    have H := mul_lt_mul_of_pos_right hc hcp
    rw [mul_assoc, div_mul_cancel₀ _ hcp.ne'] at H
    change N * log c - p*D*c ≤ -(p*D/2)*c
    linarith
  obtain ⟨B₀,hB₀⟩ := eventually_atTop.mp (ht.eventually_lt_atBot C)
  let B := max 1 B₀
  have hB : 0 < B := zero_lt_one.trans_le (le_max_left _ _)
  let R := max 1 ((N * log B - C) / (p*N))
  let δ := exp ((C - 1 - N * log B) / (q - 1))
  have hδ : 0 < δ := exp_pos _
  let K := Icc A B ×ˢ Icc δ R
  have HK (v : ℝ × ℝ) (hv : v ∈ chartDomain) (hg : C ≤ F v) : v ∈ K := by
    have hcB : v.1 ≤ B := by
      by_contra! hc
      exact (hB₀ _ ((le_max_right _ _).trans hc.le)).not_ge (hg.trans (hh v hv))
    have hcA : A ≤ v.1 := by
      by_contra! hc
      have H := log_lt_log hv.1 hc
      change log v.1 < log (exp ((C-1)/N)) at H
      rw [log_exp, lt_div_iff₀ hN] at H
      linarith [hh v hv, mul_nonneg (mul_pos hp hD).le hv.1.le]
    have hl := mul_le_mul_of_nonneg_left (log_le_log hv.1 hcB) hN.le
    have hvR : v.2 ≤ R := by
      apply le_trans _ (le_max_right _ _)
      apply (le_div_iff₀ (mul_pos hp hN)).mpr
      linarith [hf v hv, mul_nonneg (mul_pos hp hD).le hv.1.le]
    have hs : (∑ i, penalty (v.2 + v.1 * d i)) ≤ penalty v.2 := by
      classical
      have H (i : Fin n) : penalty (v.2 + v.1 * d i) ≤ if i = j then penalty v.2 else 0 := by
        by_cases hi : i = j
        · simp [hi,hj]
        · simpa [hi] using hterm v hv i
      simpa using Finset.sum_le_sum (s := Finset.univ) fun i _ ↦ H i
    have hδv : δ ≤ v.2 := by
      by_contra! hv'
      have H := log_lt_log hv.2 hv'
      change log v.2 < log (exp ((C-1-N*log B)/(q-1))) at H
      rw [log_exp, lt_div_iff₀ (sub_pos.mpr hq)] at H
      have hs' := mul_le_mul_of_nonneg_left (hs.trans (penalty_le_log hv.2)) (sub_nonneg.mpr hq.le)
      have hh' : F v ≤ N * log B + (q-1)*log v.2 := by
        unfold F chart
        change N * log v.1 - p*(v.1*D + N*v.2) + _ ≤ _
        linarith [mul_nonneg hp.le (add_nonneg (mul_nonneg hv.1.le hD.le)
          (mul_nonneg hN.le hv.2.le))]
      linarith
    exact ⟨⟨hcA,hcB⟩,⟨hδv,hvR⟩⟩
  have hk : (1, (1 : ℝ)) ∈ K := HK _ ⟨one_pos,one_pos⟩ le_rfl
  have hsub : K ⊆ chartDomain := fun _ h ↦ ⟨hA.trans_le h.1.1,hδ.trans_le h.2.1⟩
  have hcont : ContinuousOn F K := by
    intro v hv
    have hv' := hsub hv
    have hlin : Continuous (fun w : ℝ × ℝ ↦ p*(w.1*D+N*w.2)) := by fun_prop
    have hlog : ContinuousAt (fun w : ℝ × ℝ ↦ N*log w.1) v :=
      continuousAt_const.mul (continuous_fst.continuousAt.log hv'.1.ne')
    have hs : ContinuousAt (fun w : ℝ × ℝ ↦ ∑ i, penalty (w.2+w.1*d i)) v := by
      apply tendsto_finsetSum Finset.univ
      intro i _
      have Ha : ContinuousAt (fun w : ℝ × ℝ ↦ w.2+w.1*d i) v := by fun_prop
      have Hp : 0 < v.2+v.1*d i :=
        add_pos_of_pos_of_nonneg hv'.2 (mul_nonneg hv'.1.le (hd i))
      exact (((penalty_deriv Hp).continuousAt).comp' (x := v)
        (f := fun w : ℝ × ℝ ↦ w.2+w.1*d i) Ha).tendsto
    exact ((hlog.sub hlin.continuousAt).add (continuousAt_const.mul hs)).continuousWithinAt
  obtain ⟨v,hv,hm⟩ := (isCompact_Icc.prod isCompact_Icc).exists_isMaxOn ⟨_,hk⟩ hcont
  refine ⟨v,hsub hv,fun w hw ↦ ?_⟩
  by_cases he : w ∈ K
  · exact hm he
  · exact (lt_of_not_ge (fun hb ↦ he (HK w hw hb))).le.trans (hm hk)

private def physicalDomain (M : ℝ) : Set (ℝ × ℝ) := Ioi 0 ×ˢ Ioi M

private def toChart (M : ℝ) (v : ℝ × ℝ) : ℝ × ℝ := (v.1, v.1 * log (v.2 / M))

private theorem chart_bij {M : ℝ} (hM : 0 < M) :
    BijOn (toChart M) (physicalDomain M) chartDomain := by
  refine ⟨fun v hv ↦ ⟨hv.1, mul_pos hv.1 (log_pos ((one_lt_div hM).mpr hv.2))⟩, ?_, ?_⟩
  · rintro ⟨c,b⟩ hv ⟨a,B⟩ hw he
    have hca : c = a := congrArg Prod.fst he
    subst a
    have h := mul_left_cancel₀ hv.1.ne'
      (show c * log (b/M) = c * log (B/M) from congrArg Prod.snd he)
    exact Prod.ext rfl ((div_left_inj₀ hM.ne').mp
      (log_injOn_pos (div_pos (hM.trans hv.2) hM) (div_pos (hM.trans hw.2) hM) h))
  · rintro ⟨c,u⟩ h
    have hB : M < M * exp (u/c) := by
      have := one_lt_exp_iff.mpr (div_pos h.2 h.1)
      nlinarith [mul_lt_mul_of_pos_left this hM]
    refine ⟨(c,M*exp (u/c)), ⟨h.1,hB⟩, Prod.ext rfl ?_⟩
    change c*log ((M*exp (u/c))/M) = u
    rw [mul_comm M, mul_div_cancel_right₀ _ hM.ne', log_exp, mul_div_cancel₀ _ h.1.ne']

private theorem physical_likelihood {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hM : 0 < M) {v : ℝ × ℝ} (hv : v ∈ physicalDomain M) :
    Endpoint2GeneralizedBeta.logLik x v.2 p q v.1 =
      chart (fun i ↦ log (M / x i)) p q (toChart M v) +
        ∑ i, (-log (x i) - Beta.logBeta (p,q)) := by
  have hb := hM.trans hv.2
  have H (i : Fin n) : log (Endpoint2GeneralizedBeta.unitData x v.2 v.1 i) =
      -((toChart M v).2 + v.1 * log (M / x i)) := by
    rw [Endpoint2GeneralizedBeta.unitData, log_rpow (div_pos (hx i) hb), toChart,
      log_div (hx i).ne' hb.ne', log_div hb.ne' hM.ne', log_div hM.ne' (hx i).ne']
    ring
  have H' (i : Fin n) : Endpoint2GeneralizedBeta.unitData x v.2 v.1 i =
      exp (-((toChart M v).2 + v.1 * log (M / x i))) := by
    rw [← H i]
    exact (exp_log (rpow_pos_of_pos (div_pos (hx i) hb) _)).symm
  have hh (i : Fin n) :
      log v.1 - log (x i) - Beta.logBeta (p,q) +
        p * log (Endpoint2GeneralizedBeta.unitData x v.2 v.1 i) +
          (q - 1) * log (1 - Endpoint2GeneralizedBeta.unitData x v.2 v.1 i) =
      log v.1 - p * ((toChart M v).2 + v.1 * log (M / x i)) +
        (q - 1) * penalty ((toChart M v).2 + v.1 * log (M / x i)) +
          (-log (x i) - Beta.logBeta (p,q)) := by
    rw [H, H']
    unfold penalty
    ring
  simp only [Endpoint2GeneralizedBeta.logLik, chart, hh, Finset.sum_add_distrib,
    Finset.sum_sub_distrib, ← Finset.mul_sum, Finset.sum_const, Finset.card_univ, Fintype.card_fin,
    nsmul_eq_mul]
  simp only [toChart]
  ring

/-- For held positive first shape and held upper shape above one, every
nonconstant positive sample has exactly one maximizing power and
strictly containing endpoint. -/
theorem fixed_shapes_joint_regular (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M)
    (hne : ∃ i, x i < M) (hp : 0 < p) (hq : 1 < q) :
    let D := Ioi (0 : ℝ) ×ˢ Ioi M
    let L := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik x v.2 p q v.1
    ∃! v, v ∈ D ∧ IsMaxOn L D v := by
  intro D L
  let d := fun i ↦ log (M / x i)
  have hM : 0 < M := (hx ⟨0,hn⟩).trans_le (hle ⟨0,hn⟩)
  have hd (i : Fin n) : 0 ≤ d i := log_nonneg ((one_le_div (hx i)).mpr (hle i))
  have hzero : ∃ i, d i = 0 := heq.imp fun i hi ↦ by simp [d,hi,hM.ne']
  have hpos : ∃ i, 0 < d i := hne.imp fun i hi ↦ log_pos ((one_lt_div (hx i)).mpr hi)
  obtain ⟨v,hv,hm⟩ := chart_exists hn hd hpos hzero hp hq
  apply (Transfer.existsUnique_max_iff (chart_bij hM) (fun _ hw ↦ physical_likelihood hx hM hw)).mpr
  exact ⟨v,⟨hv,hm⟩,fun w hw ↦
    (chart_strictConcave hn hd p hq).eq_of_isMaxOn hw.2 hm hw.1 hv⟩

/-- Holding both beta shapes gives a finite endpoint and power fit
exactly for a nonconstant positive sample with upper shape at least one.
The solution is unique, with closed endpoint precisely at shape one. -/
theorem fixed_shapes_joint_criterion (hn : 0 < n) {x : Fin n → ℝ} {M p q : ℝ}
    (hx : ∀ i, 0 < x i) (hle : ∀ i, x i ≤ M) (heq : ∃ i, x i = M) (hp : 0 < p) :
    let D := Ioi (0 : ℝ) ×ˢ (if q = 1 then Ici M else Ioi M)
    let L := fun v : ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik x v.2 p q v.1
    ((∃ v ∈ D, IsMaxOn L D v) ↔ 1 ≤ q ∧ ∃ i, x i < M) ∧
      ((1 ≤ q ∧ ∃ i, x i < M) → ∃! v, v ∈ D ∧ IsMaxOn L D v) := by
  intro D L
  have hM : 0 < M := (hx ⟨0,hn⟩).trans_le (hle ⟨0,hn⟩)
  have hnot_const (v : ℝ × ℝ) (hm : IsMaxOn L D v) : ∃ i, x i < M := by
    by_contra! h
    have hex : x = fun _ ↦ M := funext fun i ↦ le_antisymm (hle i) (h i)
    have hpath := Endpoint2GeneralizedBeta.constant_sample_free_power_path hn hM p q
    have hh := hpath.eventually_gt_atTop (L v)
    obtain ⟨c,hgt,hc⟩ := (hh.and (eventually_gt_atTop (0 : ℝ))).exists
    have hb : M < M * exp c⁻¹ := by
      have hE := one_lt_exp_iff.mpr (inv_pos.mpr hc)
      linarith [mul_lt_mul_of_pos_left hE hM]
    have hm' : (c, M * exp c⁻¹) ∈ D := by
      refine ⟨hc, ?_⟩
      by_cases hq : q = 1 <;> simp [hq, hb, hb.le]
    have hle' : L (c, M * exp c⁻¹) ≤ L v := hm hm'
    have hrepl : L (c, M * exp c⁻¹) =
        Endpoint2GeneralizedBeta.logLik (fun _ : Fin n ↦ M) (M * exp c⁻¹) p q c :=
      congrArg (fun y : Fin n → ℝ ↦ Endpoint2GeneralizedBeta.logLik y (M * exp c⁻¹) p q c) hex
    exact hgt.not_ge (hrepl ▸ hle')
  have hnot_subunit (v : ℝ × ℝ) (hm : IsMaxOn L D v) : 1 ≤ q := by
    by_contra! hq
    have hspike := Endpoint2GeneralizedBeta.endpoint_spike (p := p) hx hle heq hq one_pos
    have hpath := hspike.eventually_gt_atTop (L v)
    have hnear : ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t ∧ t < 1 :=
      (show ∀ᶠ t : ℝ in 𝓝[>] 0, 0 < t from self_mem_nhdsWithin).and
        (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds zero_lt_one))
    obtain ⟨t,hgt,ht,ht'⟩ := (hpath.and hnear).exists
    have hb : M < M / (1 - t) := by
      apply (lt_div_iff₀ (sub_pos.mpr ht')).mpr
      nlinarith [mul_pos hM ht]
    have hmem : (1, M / (1 - t)) ∈ D := by
      refine ⟨one_pos, ?_⟩
      simpa [hq.ne] using hb
    have hle' : L (1, M / (1 - t)) ≤ L v := hm hmem
    exact (show L v < L (1, M / (1 - t)) by simpa using hgt).not_ge hle'
  have huniq (h : 1 ≤ q ∧ ∃ i, x i < M) : ∃! v, v ∈ D ∧ IsMaxOn L D v := by
    obtain ⟨hq,hne⟩ := h
    rcases eq_or_lt_of_le hq with rq | hq'
    · subst q
      let S := Endpoint2GeneralizedBeta.logDistanceSum x M
      have hS : 0 < S := by
        obtain ⟨i,hi⟩ := hne
        apply Finset.sum_pos'
        · exact fun j _ ↦ log_nonneg ((one_le_div (hx j)).mpr (hle j))
        · exact ⟨i,Finset.mem_univ _,log_pos ((one_lt_div (hx i)).mpr hi)⟩
      let c := (n : ℝ) / S / p
      have hc : 0 < c := div_pos (div_pos (Nat.cast_pos.mpr hn) hS) hp
      have hd : (c,M) ∈ D := ⟨hc, by simp⟩
      have hw : (M,p,c) ∈ Ici M ×ˢ Beta.domain := ⟨le_rfl,hp,hc⟩
      have hprod : p*c = (n : ℝ) / S := by
        dsimp [c]
        exact mul_div_cancel₀ _ hp.ne'
      have hm := (Endpoint2GeneralizedBeta.free_endpoint_ridge hn hx hle hne (M,p,c) hw).mpr
        ⟨rfl,hprod⟩
      have hmax : IsMaxOn L D (c,M) := by
        intro w hw'
        exact hm (show (w.2,p,w.1) ∈ Ici M ×ˢ Beta.domain from
          ⟨by simpa using hw'.2,hp,hw'.1⟩)
      refine ⟨(c,M),⟨hd,hmax⟩,?_⟩
      rintro v ⟨hv,hvmax⟩
      have htop : L v = L (c,M) := le_antisymm (hmax hv) (hvmax hd)
      have hmem : (v.2,p,v.1) ∈ Ici M ×ˢ Beta.domain := ⟨by simpa using hv.2,hp,hv.1⟩
      have hglobal : IsMaxOn
          (fun w : ℝ × ℝ × ℝ ↦ Endpoint2GeneralizedBeta.logLik x w.1 w.2.1 1 w.2.2)
          (Ici M ×ˢ Beta.domain) (v.2,p,v.1) := by
        intro w hw'
        exact (hm hw').trans htop.ge
      have H := (Endpoint2GeneralizedBeta.free_endpoint_ridge hn hx hle hne _ hmem).mp hglobal
      exact Prod.ext (mul_left_cancel₀ hp.ne' (H.2.trans hprod.symm)) H.1
    · simpa [D,L,hq'.ne'] using fixed_shapes_joint_regular hn hx hle heq hne hp hq'
  exact ⟨⟨fun ⟨v,_,hm⟩ ↦ ⟨hnot_subunit v hm,hnot_const v hm⟩,
    fun h ↦ (huniq h).exists.elim fun v hv ↦ ⟨v,hv⟩⟩, huniq⟩

end

end UnivariateML.Endpoint2GeneralizedBetaJoint

end
