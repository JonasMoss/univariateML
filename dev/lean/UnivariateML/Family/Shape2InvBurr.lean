module

public import UnivariateML.Family.ShapeFacesInverseBurr
public import UnivariateML.Family.TiesLomax
public import UnivariateML.Family.JoinsBurr

set_option linter.style.header false

/-!
Exact branch criteria and reciprocal finite ties for inverse Burr.

## Deviations

The all-free face comparison is on the mean log-likelihood scale. The held-power exact tie
is the reciprocal 21-point Lomax crossing at power one. The all-free exact tie requires a
parent Burr counterexample.
-/

@[expose] public section

open Real Set Filter
namespace UnivariateML.Shape2InvBurr

open ShapeFacesInverseBurr

noncomputable section

variable {n : ℕ}

theorem held_scale_first_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s) :
    (∃ b ∈ Ioi (0 : ℝ), IsMaxOn (fun b ↦ logLik x k b s) (Ioi 0) b) ↔
      ∃ i, x i ≠ s := by
  constructor
  · rintro ⟨b, _, hm⟩
    by_contra! h
    have ht := held_first_scale_unbounded hn hk hs h
    obtain ⟨d, hd, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop (logLik x k b s)))).exists
    exact hgt.not_ge (hm hd)
  · intro h
    exact (held_first_scale_unique hn hx hk hs h).exists

theorem held_first_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k : ℝ} (hk : 0 < k) :
    (∃ p ∈ BurrHeld.shapeDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x k q.1 q.2) BurrHeld.shapeDomain p) ↔
      ∃ i j, x i ≠ x j := by
  constructor
  · rintro ⟨p, _, hm⟩
    by_contra! h
    let i : Fin n := ⟨0, hn⟩
    have he : x = fun _ ↦ x i := funext fun j ↦ h j i
    have ht := held_first_constant_path hn (hx i) hk
    obtain ⟨b, hb, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and
      (ht.eventually (eventually_gt_atTop (logLik x k p.1 p.2)))).exists
    have hle : logLik x k b (x i) ≤ logLik x k p.1 p.2 :=
      isMaxOn_iff.mp hm (b, x i) ⟨hb, hx i⟩
    rw [he] at hle hgt
    exact hgt.not_ge hle
  · intro h
    exact (held_first_unique hn hx hk h).exists

theorem held_scale_attains_iff (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {s : ℝ} (hs : 0 < s) :
    (∃ p ∈ BurrHeld.shapeDomain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s) BurrHeld.shapeDomain p) ↔
      (∃ i j, x i ≠ x j) ∧ ∃ i, s < x i := by
  constructor
  · rintro ⟨p, hp, hm⟩
    have hN : (0 : ℝ) < n := by exact_mod_cast hn
    have habove : ∃ i, s < x i := by
      by_contra! hle
      by_cases hbelow : ∃ i, x i < s
      · obtain ⟨hlub, hlt⟩ := held_scale_face hn hx hs hle hbelow
        have hub : logLik x p.2 p.1 s / n ∈ upperBounds
            ((fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s / n) '' BurrHeld.shapeDomain) := by
          rintro _ ⟨q, hq, rfl⟩
          exact div_le_div_of_nonneg_right (isMaxOn_iff.mp hm q hq) hN.le
        exact (hlt p hp).not_ge (hlub.2 hub)
      · push Not at hbelow
        have he : x = fun _ ↦ s := funext fun i ↦ le_antisymm (hle i) (hbelow i)
        have hB := hm.bddAbove
        rw [he] at hB
        exact (held_scale_constant_unbounded (v := s) hn hs le_rfl) hB
    refine ⟨?_, habove⟩
    by_contra! h
    obtain ⟨i, hi⟩ := habove
    have he : x = fun _ ↦ x i := funext fun j ↦ h j i
    have hB := hm.bddAbove
    rw [he] at hB
    exact (held_scale_constant_unbounded hn hs hi.le) hB
  · rintro ⟨hne, habove⟩
    exact (held_scale_unique hn hx hs hne habove).exists

private theorem at_unit_power {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x k 1 s = Transfer.InvPareto.logLik x k s := by
  rw [logLik_reciprocal hx k 1 hs, Transfer.InvPareto.logLik_transfer,
    Transfer.Burr.logLik_transfer (x := Transfer.reciprocal x)
      (fun i ↦ inv_pos.mpr (hx i)) 1 k (inv_pos.mpr hs)]
  simp only [Transfer.Burr.powerJac, Transfer.Burr.rate, rpow_one, inv_inv,
    log_one, sub_self, zero_mul, add_zero, Finset.sum_const_zero]
  rw [show Transfer.Burr.powered (Transfer.reciprocal x) 1 = Transfer.reciprocal x from
    funext fun i ↦ rpow_one (Transfer.reciprocal x i)]

/-- The exact reciprocal Lomax crossing has precisely two inverse Burr
maximizers at power one, with both strictly above the inverse-Weibull face. -/
theorem held_power_exact_tie :
    ∃ a ∈ Ioo ((2 : ℝ) ^ 512)⁻¹ ((2 : ℝ) ^ 32)⁻¹,
      let x : Fin 21 → ℝ :=
        fun i ↦ (if i.val = 0 then a else if i.val ≤ 10 then (1 : ℝ) else 100)⁻¹
      (∀ i, 0 < x i) ∧ ∃ p q : ℝ × ℝ, p.2 < q.2 ∧
        {v ∈ Lomax.domain | IsMaxOn (fun r : ℝ × ℝ ↦ logLik x r.1 1 r.2)
          Lomax.domain v} = {p, q} ∧
        Transfer.InvPareto.bestInvExp x < logLik x p.1 1 p.2 := by
  obtain ⟨a, ⟨ha, ht⟩, _⟩ := TiesLomax.three_level_global_tie
  let u : Fin 21 → ℝ := fun i ↦ if i.val = 0 then a else if i.val ≤ 10 then 1 else 100
  let x := Transfer.reciprocal u
  obtain ⟨p, hp, q, hq, hps, hqs, hmp, hmq, hall, hface⟩ := ht
  have ha₀ : 0 < a := (by positivity : (0 : ℝ) < ((2 : ℝ) ^ (512 : ℕ))⁻¹).trans ha.1
  have hx (i : Fin 21) : 0 < x i := by
    apply inv_pos.mpr
    dsimp [u]
    split
    · exact ha₀
    · split <;> norm_num
  have he : Transfer.reciprocal x = u := by funext i; exact inv_inv (u i)
  have hf (r : ℝ × ℝ) (hr : r ∈ Lomax.domain) :
      logLik x r.1 1 r.2 = Lomax.logLik u r.1 r.2 + Transfer.reciprocalJac x := by
    rw [at_unit_power hx r.1 hr.2, Transfer.InvPareto.logLik_transfer, he]
  have hmap (r : ℝ × ℝ) (hr : r ∈ Lomax.domain) :
      IsMaxOn (fun v : ℝ × ℝ ↦ logLik x v.1 1 v.2) Lomax.domain r ↔
        IsMaxOn (fun v : ℝ × ℝ ↦ Lomax.logLik u v.1 v.2) Lomax.domain r :=
    Transfer.isMaxOn_iff (bijOn_id Lomax.domain) hf hr
  refine ⟨a, ha, hx, p, q, hps.trans hqs, ?_, ?_⟩
  · ext v
    simp only [mem_ofPred_eq, mem_insert_iff, mem_singleton_iff]
    constructor
    · rintro ⟨hv, hm⟩
      exact hall v hv ((hmap v hv).1 hm)
    · rintro (rfl | rfl)
      · exact ⟨hp, (hmap _ hp).2 hmp⟩
      · exact ⟨hq, (hmap _ hq).2 hmq⟩
  · change Transfer.InvPareto.bestInvExp x < logLik x p.1 1 p.2
    rw [Transfer.InvPareto.bestInvExp, he, hf p hp]
    exact add_lt_add_left hface _


private theorem burr_total_mean (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    Transfer.Burr.logLik x c k s = n * BurrHeld.logLik x k c s := by
  have h (i : Fin n) :
      Transfer.Burr.logDensity c k s (x i) = BurrHeld.logDensity k c s (x i) := by
    unfold Transfer.Burr.logDensity BurrHeld.logDensity BurrHeld.softplus
    rw [rpow_def_of_pos (div_pos (hx i) hs), log_div (hx i).ne' hs.ne',
      log_div hs.ne' (hx i).ne']
    ring_nf
  have hN : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
  unfold Transfer.Burr.logLik BurrHeld.logLik BurrHeld.avg
  simp_rw [h]
  field_simp

/-- Constants concentrate. On a nonconstant sample with no finite
maximizer, the larger optimized reciprocal face is the strict supremum. -/
theorem all_free_failure (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (j : Fin n) (hj : ∀ i, x i ≤ x j) :
    let F := fun p : ℝ × ℝ × ℝ ↦ logLik x p.1 p.2.1 p.2.2 / n
    let y := BurrFree.logExcess (Transfer.reciprocal x) j
    ((∀ i, x i = x j) → ¬ BddAbove (F '' BurrFree.physicalDomain)) ∧
    ((∃ i k, x i ≠ x k) →
      ∃ c > 0, ∃ b, (∀ d > 0, ∀ t, BurrFree.weibullLik y d t ≤ BurrFree.weibullLik y c b) ∧
        ((¬ ∃ p ∈ BurrFree.physicalDomain, IsMaxOn F BurrFree.physicalDomain p) →
          let B := max (BurrFree.paretoValue y) (BurrFree.weibullLik y c b) -
            BurrHeld.avg (fun i ↦ log (x i))
          IsLUB (F '' BurrFree.physicalDomain) B ∧
            ∀ p ∈ BurrFree.physicalDomain, F p < B)) := by
  intro F y
  let u := Transfer.reciprocal x
  let G := fun p : ℝ × ℝ × ℝ ↦ BurrHeld.logLik u p.1 p.2.1 p.2.2
  let φ := ShapeFacesInverseBurr.reciprocateScale
  let D := BurrFree.physicalDomain
  let J := Transfer.reciprocalJac x / n
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have hu (i : Fin n) : 0 < u i := inv_pos.mpr (hx i)
  have hmin (i : Fin n) : u j ≤ u i := inv_anti₀ (hx i) (hj i)
  have htwice (p : ℝ × ℝ × ℝ) : φ (φ p) = p := by
    rcases p with ⟨k, c, s⟩
    simp [φ, ShapeFacesInverseBurr.reciprocateScale]
  have hmaps : MapsTo φ D D := fun _ hp ↦ ⟨hp.1, hp.2.1, inv_pos.mpr hp.2.2⟩
  have hbij : BijOn φ D D :=
    ⟨hmaps, fun p _ q _ h ↦ by simpa only [htwice] using congrArg φ h,
      fun p hp ↦ ⟨φ p, hmaps hp, htwice p⟩⟩
  have hF (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : F p = G (φ p) + J := by
    dsimp [F, G, φ, ShapeFacesInverseBurr.reciprocateScale, J]
    rw [logLik_reciprocal hx _ _ hp.2.2, burr_total_mean hn hu _ _ (inv_pos.mpr hp.2.2),
      add_div, mul_div_cancel_left₀ _ hN.ne']
  have htransfer : (∃ p ∈ D, IsMaxOn F D p) ↔ ∃ p ∈ D, IsMaxOn G D p :=
    Transfer.exists_max_iff hbij hF
  have ⟨hconst, hvar⟩ := JoinsBurr.all_free_failure (x := u) hu j hmin
  constructor
  · intro h hB
    apply hconst (fun i ↦ congrArg Inv.inv (h i))
    obtain ⟨M, hM⟩ := hB
    refine ⟨M - J, ?_⟩
    rintro _ ⟨p, hp, rfl⟩
    have he := hM (mem_image_of_mem F (hmaps hp))
    change F (φ p) ≤ M at he
    rw [hF _ (hmaps hp), htwice] at he
    linarith only [he]
  · rintro ⟨i, k, hik⟩
    obtain ⟨c, hc, b, hW, hface⟩ := hvar ⟨i, k, fun h ↦ hik (inv_injective h)⟩
    refine ⟨c, hc, b, hW, fun hnmax ↦ ?_⟩
    obtain ⟨hlub, hlt⟩ := hface (fun h ↦ hnmax (htransfer.2 h))
    let B₀ := max (BurrFree.paretoValue y) (BurrFree.weibullLik y c b) -
      BurrHeld.avg (fun i ↦ log (u i))
    let B := max (BurrFree.paretoValue y) (BurrFree.weibullLik y c b) -
      BurrHeld.avg (fun i ↦ log (x i))
    have he : B = B₀ + J := by
      simp only [B, B₀, J, BurrHeld.avg, Transfer.reciprocalJac, u,
        Transfer.reciprocal, log_inv, Finset.sum_neg_distrib, ← Finset.mul_sum]
      ring
    change IsLUB (F '' D) B ∧ ∀ p ∈ D, F p < B
    have hstrict (p : ℝ × ℝ × ℝ) (hp : p ∈ D) : F p < B := by
      have h := hlt (φ p) (hmaps hp)
      change G (φ p) < B₀ at h
      rw [hF p hp, he]
      exact add_lt_add_left h _
    refine ⟨⟨?_, ?_⟩, hstrict⟩
    · rintro _ ⟨p, hp, rfl⟩
      exact (hstrict p hp).le
    · intro M hM
      have hUb : M - J ∈ upperBounds (G '' D) := by
        rintro _ ⟨p, hp, rfl⟩
        have hi := hM (mem_image_of_mem F (hmaps hp))
        change F (φ p) ≤ M at hi
        rw [hF _ (hmaps hp), htwice] at hi
        linarith only [hi]
      have h : B₀ ≤ M - J := hlub.2 hUb
      rw [he]
      linarith only [h]


/-- At a common observation at or above the held scale, increasing power
with the corresponding conditional shape gives unbounded likelihood. -/
theorem held_scale_constant_path (hn : 0 < n) {v s : ℝ} (hs : 0 < s) (hsv : s ≤ v) :
    let k := fun b : ℝ ↦ (log (1 + (s / v) ^ b))⁻¹
    (∀ b, 0 < k b) ∧ Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) (k b) b s) atTop atTop := by
  intro k
  have hv : 0 < v := hs.trans_le hsv
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  let U := fun b : ℝ ↦ (s / v) ^ b
  have hu (b : ℝ) : 0 < U b := rpow_pos_of_pos (div_pos hs hv) _
  have hH (b : ℝ) : 0 < log (1 + U b) := log_pos (by linarith [hu b])
  refine ⟨fun b ↦ inv_pos.mpr (hH b), ?_⟩
  have hlim := tendsto_atTop_add_const_right _ ((n : ℝ) * (-log v - 1 - log 2))
    (tendsto_log_atTop.const_mul_atTop hN)
  refine tendsto_atTop_mono' _ ?_ hlim
  filter_upwards [eventually_gt_atTop (0 : ℝ)] with b hb
  have hU1 : U b ≤ 1 := Real.rpow_le_one (div_pos hs hv).le ((div_le_one hv).2 hsv) hb.le
  have hlog := log_le_sub_one_of_pos (show 0 < 1 + U b by linarith [hu b])
  have hlog' : log (log (1 + U b)) ≤ log (U b) :=
    log_le_log (hH b) (by linarith only [hlog])
  have hlog2 : log (1 + U b) ≤ log 2 :=
    log_le_log (by linarith [hu b]) (by linarith only [hU1])
  have hident : log (U b) = b * log (s / v) := log_rpow (div_pos hs hv) b
  have he : (k b + 1) * log (1 + U b) = 1 + log (1 + U b) := by
    change ((log (1 + U b))⁻¹ + 1) * log (1 + U b) = _
    rw [add_mul, inv_mul_cancel₀ (hH b).ne', one_mul]
  have hk : log (k b) = -log (log (1 + U b)) := log_inv _
  simp only [logLik, logDensity, Finset.sum_const, Finset.card_univ, Fintype.card_fin, nsmul_eq_mul]
  change _ ≤ (n : ℝ) * (log (k b) + log b - log v + b * log (s / v) -
    (k b + 1) * log (1 + U b))
  rw [he, hk, ← hident, ← mul_add]
  exact mul_le_mul_of_nonneg_left (by linarith only [hlog', hlog2]) hN.le

end

end UnivariateML.Shape2InvBurr

end
