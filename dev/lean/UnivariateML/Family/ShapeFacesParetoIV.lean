import UnivariateML.Family.BurrHeld
import UnivariateML.Family.Transfers
import UnivariateML.Family.ShapeFacesInverseBurr

/-! Pareto IV at a held threshold.

## Deviations

The likelihood transfers use strictly positive excesses. Endpoint contact and
the free-threshold problem require separate arguments.
-/

open Real Set
namespace UnivariateML.ShapeFacesParetoIV
open Transfer
noncomputable section

variable {n : ℕ}

def excess (x : Fin n → ℝ) (μ : ℝ) : Fin n → ℝ := fun i ↦ x i - μ

def logLik (x : Fin n → ℝ) (μ k c s : ℝ) : ℝ :=
  ∑ i, (log k + log c - log (x i - μ) - c * log (s / (x i - μ)) -
    (k + 1) * log (1 + ((x i - μ) / s) ^ c))

theorem logLik_excess (x : Fin n → ℝ) (μ k c s : ℝ) :
    logLik x μ k c s = Burr.logLik (excess x μ) c k s := rfl

theorem held_power_iff (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.1 c q.2) Lomax.domain p) ↔
    ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (excess x μ) c)),
      Lomax.psi (Burr.powered (excess x μ) c) u ≤ 0 :=
  Burr.exists_max_iff hn (fun i ↦ sub_pos.mpr (hx i)) hc

theorem held_power_face (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (excess x μ) c)),
      Lomax.psi (Burr.powered (excess x μ) c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x μ p.1 c p.2) '' Lomax.domain) =
      Burr.bestWeibull (excess x μ) c ∧
    ∀ p ∈ Lomax.domain, logLik x μ p.1 c p.2 < Burr.bestWeibull (excess x μ) c :=
  Burr.exponential_sup_of_no_witness hn (fun i ↦ sub_pos.mpr (hx i)) hc hw

theorem held_shapes_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x μ k c) (Ioi 0) s :=
  Burr.heldShape_unique hn (fun i ↦ sub_pos.mpr (hx i)) hc hk

theorem held_scale_power_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x μ k c s) (Ioi 0) k :=
  Burr.heldScale_unique hn (fun i ↦ sub_pos.mpr (hx i)) hc hs

private theorem avg_eq {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x μ k c s / n = BurrHeld.logLik (excess x μ) k c s := by
  rw [logLik_excess]
  unfold Burr.logLik BurrHeld.logLik BurrHeld.avg
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  have hy : 0 < excess x μ i := sub_pos.mpr (hx i)
  have ht := hs
  dsimp only
  unfold Burr.logDensity BurrHeld.logDensity BurrHeld.softplus
  rw [log_div ht.ne' hy.ne', rpow_def_of_pos (div_pos hy ht),
    log_div hy.ne' ht.ne']
  rw [mul_comm c]
  ring_nf

theorem held_scale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, excess x μ i < s) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ q.2 q.1 s) BurrHeld.shapeDomain p := by
  let z := BurrHeld.logRatios (excess x μ) s
  have hy (i : Fin n) : 0 < excess x μ i := sub_pos.mpr (hx i)
  have hzvar : ∃ i j, z i ≠ z j := by
    obtain ⟨i, j, hij⟩ := hne
    refine ⟨i, j, fun h ↦ hij ?_⟩
    have hr := log_injOn_pos (div_pos (hy i) hs) (div_pos (hy j) hs) h
    have he : excess x μ i = excess x μ j := (div_left_inj₀ hs.ne').mp hr
    dsimp [excess] at he
    linarith
  have hzneg : ∃ i, z i < 0 := by
    obtain ⟨i, hi⟩ := habove
    exact ⟨i, log_neg (div_pos (hy i) hs) ((div_lt_one hs).2 hi)⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have heq (q : ℝ × ℝ) :
      logLik x μ q.2 q.1 s / n =
        BurrHeld.scaleLik z q +
          (-BurrHeld.avg (fun i ↦ log (excess x μ i))) := by
    rw [avg_eq hx _ _ hs, BurrHeld.logLik_eq_scale hn]
    dsimp [z]
    ring
  obtain ⟨p, hp, hu⟩ := (Transfer.existsUnique_max_iff
    (bijOn_id BurrHeld.shapeDomain) (fun q _ ↦ heq q)).2
      (BurrHeld.scale_existsUnique hn hzvar hzneg)
  exact ⟨p, ⟨hp.1, fun _ h ↦ (div_le_div_iff_of_pos_right hn').mp (hp.2 h)⟩,
    fun q h ↦ hu q ⟨h.1, fun _ ha ↦ (div_le_div_iff_of_pos_right hn').mpr (h.2 ha)⟩⟩

theorem held_scale_face (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {s : ℝ} (hs : 0 < s) (hle : ∀ i, s ≤ excess x μ i) (hlt : ∃ i, s < excess x μ i) :
    let z := BurrHeld.logRatios (excess x μ) s
    let value := BurrHeld.paretoValue z + (-BurrHeld.avg (fun i ↦ log (excess x μ i)))
    IsLUB ((fun q : ℝ × ℝ ↦ logLik x μ q.2 q.1 s / n) '' BurrHeld.shapeDomain) value ∧
      ∀ q ∈ BurrHeld.shapeDomain, logLik x μ q.2 q.1 s / n < value := by
  let z := BurrHeld.logRatios (excess x μ) s
  have hz (i : Fin n) : 0 ≤ z i := log_nonneg ((one_le_div hs).2 (hle i))
  have hpos : ∃ i, 0 < z i := by
    obtain ⟨i, hi⟩ := hlt
    exact ⟨i, log_pos ((one_lt_div hs).2 hi)⟩
  obtain ⟨hlub, hu⟩ := BurrHeld.scale_pareto_supremum hn hz hpos
  have heq (q : ℝ × ℝ) :
      logLik x μ q.2 q.1 s / n =
        BurrHeld.scaleLik z q + (-BurrHeld.avg (fun i ↦ log (excess x μ i))) := by
    rw [avg_eq hx _ _ hs, BurrHeld.logLik_eq_scale hn]
    dsimp [z]
    ring_nf
  refine ⟨?_, fun q hq ↦ ?_⟩
  · exact (Transfer.isLUB_iff (bijOn_id BurrHeld.shapeDomain) (fun q _ ↦ heq q) _).2 hlub
  · rw [heq]
    exact add_lt_add_left (hu q hq) _

private theorem first_bij :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, -(p.1 * log p.2)))
      BurrHeld.shapeDomain BurrHeld.firstDomain := by
  refine ⟨fun p hp ↦ ⟨hp.1, mem_univ _⟩, ?_, ?_⟩
  · rintro ⟨c,s⟩ hp ⟨d,t⟩ hq h
    have hc : 0 < c := hp.1
    have hs : 0 < s := hp.2
    have ht : 0 < t := hq.2
    have hcd : c = d := congrArg Prod.fst h
    subst d
    have hb : -(c * log s) = -(c * log t) := congrArg Prod.snd h
    have hEq := (BurrHeld.first_intercept_iff hc hs).mp hb
    have hEq' := (BurrHeld.first_intercept_iff hc ht).mp
      (rfl : -(c * log t) = -(c * log t))
    exact Prod.ext rfl (hEq.trans hEq'.symm)
  · rintro ⟨c,b⟩ hp
    have hc : 0 < c := hp.1
    have hs : 0 < exp (-b / c) := exp_pos _
    refine ⟨(c, exp (-b / c)), ⟨hc, hs⟩, Prod.ext rfl ?_⟩
    exact (BurrHeld.first_intercept_iff hc hs).mpr rfl

theorem held_first_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    {k : ℝ} (hk : 0 < k) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x μ k q.1 q.2) BurrHeld.shapeDomain p := by
  let y : Fin n → ℝ := fun i ↦ log (excess x μ i)
  have hy (i : Fin n) : 0 < excess x μ i := sub_pos.mpr (hx i)
  have hyvar : ∃ i j, y i ≠ y j := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij (by
      have he := log_injOn_pos (hy i) (hy j) h
      change x i - μ = x j - μ at he
      linarith)⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have heq (q : ℝ × ℝ) (hq : q ∈ BurrHeld.shapeDomain) :
      logLik x μ k q.1 q.2 / n =
        BurrHeld.firstLik y k (q.1, -(q.1 * log q.2)) +
          (-BurrHeld.avg y) := by
    rw [avg_eq hx _ _ hq.2, BurrHeld.logLik_eq_first hn hy hq.2]
    dsimp [y]
    ring_nf
  obtain ⟨p, hp, hu⟩ := (Transfer.existsUnique_max_iff first_bij heq).2
    (BurrHeld.first_existsUnique hn hk hyvar)
  exact ⟨p, ⟨hp.1, fun _ h ↦ (div_le_div_iff_of_pos_right hn').mp (hp.2 h)⟩,
    fun q h ↦ hu q ⟨h.1, fun _ ha ↦ (div_le_div_iff_of_pos_right hn').mpr (h.2 ha)⟩⟩


private theorem inverse_excess {x : Fin n → ℝ} {μ : ℝ} (hx : ∀ i, μ < x i)
    (k b : ℝ) {s : ℝ} (hs : 0 < s) :
    ShapeFacesInverseBurr.logLik (reciprocal (excess x μ)) k b s⁻¹ =
      logLik x μ k b s + reciprocalJac (reciprocal (excess x μ)) := by
  have hy (i : Fin n) : 0 < reciprocal (excess x μ) i := inv_pos.mpr (sub_pos.mpr (hx i))
  rw [ShapeFacesInverseBurr.logLik_reciprocal hy k b (inv_pos.mpr hs), inv_inv, logLik_excess]
  have he : reciprocal (reciprocal (excess x μ)) = excess x μ := by
    funext i
    simp only [reciprocal, inv_inv]
  rw [he]

theorem held_first_scale_unique (hn : 0 < n) {x : Fin n → ℝ} {μ : ℝ}
    (hx : ∀ i, μ < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s)
    (hne : ∃ i, excess x μ i ≠ s) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun c ↦ logLik x μ k c s) (Ioi 0) b := by
  let y := reciprocal (excess x μ)
  have hy (i : Fin n) : 0 < y i := inv_pos.mpr (sub_pos.mpr (hx i))
  have hvar : ∃ i, y i ≠ s⁻¹ := by
    obtain ⟨i, hi⟩ := hne
    exact ⟨i, fun h ↦ hi (inv_injective h)⟩
  exact (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ)))
    (fun b _ ↦ inverse_excess hx k b hs)).1
      (ShapeFacesInverseBurr.held_first_scale_unique hn hy hk (inv_pos.mpr hs) hvar)

open Filter

theorem held_first_constant_path (hn : 0 < n) {v μ k : ℝ}
    (hv : μ < v) (_hk : 0 < k) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) μ k b (v - μ)) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have h (b : ℝ) : logLik (fun _ : Fin n ↦ v) μ k b (v - μ) =
      (n : ℝ) * log b + (n : ℝ) * (log k - log (v - μ) - (k + 1) * log 2) := by
    simp [logLik, div_self (sub_pos.mpr hv).ne', nsmul_eq_mul]
    ring_nf
  simp only [h]
  exact tendsto_atTop_add_const_right atTop _ (tendsto_log_atTop.const_mul_atTop hN)

theorem held_threshold_constant_unbounded (hn : 0 < n) {v μ : ℝ} (hv : μ < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) μ p.1 p.2.1 p.2.2) ''
      (Ioi 0 ×ˢ BurrHeld.shapeDomain)) := by
  rintro ⟨M, hM⟩
  have h : ∀ᶠ b : ℝ in atTop, M < logLik (fun _ : Fin n ↦ v) μ 1 b (v - μ) :=
    (held_first_constant_path hn hv (by norm_num : (0 : ℝ) < 1)).eventually (eventually_gt_atTop M)
  obtain ⟨b, hb, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and h).exists
  have hmem : (1, b, v - μ) ∈ Ioi (0 : ℝ) ×ˢ BurrHeld.shapeDomain := by
    change (0 : ℝ) < 1 ∧ 0 < b ∧ 0 < v - μ
    exact ⟨by norm_num, hb, sub_pos.mpr hv⟩
  have hle : logLik (fun _ : Fin n ↦ v) μ 1 b (v - μ) ≤ M :=
    hM ⟨(1, b, v - μ), hmem, rfl⟩
  exact hgt.not_ge hle

theorem held_first_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {μ k s : ℝ}
    (hk : 0 < k) (hs : 0 < s) (he : ∀ i, excess x μ i = s) :
    Tendsto (fun b ↦ logLik x μ k b s) atTop atTop := by
  have hx : x = fun _ ↦ μ + s := by
    funext i
    have hi := he i
    unfold excess at hi
    linarith
  have hv : μ < μ + s := by linarith
  rw [hx]
  simpa only [add_sub_cancel_left] using held_first_constant_path hn hv hk

theorem held_scale_constant_unbounded (hn : 0 < n) {v μ s : ℝ}
    (hv : μ < v) (hvs : v - μ ≤ s) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) μ p.2 p.1 s) ''
      BurrHeld.shapeDomain) := by
  let x : Fin n → ℝ := fun _ ↦ v
  have hs : 0 < s := (sub_pos.mpr hv).trans_le hvs
  let z : Fin n → ℝ := fun _ ↦ log ((v - μ) / s)
  have hz : BurrHeld.logRatios (excess x μ) s = z := rfl
  have hnonpos : log ((v - μ) / s) ≤ 0 :=
    log_nonpos (div_pos (sub_pos.mpr hv) hs).le ((div_le_one hs).2 hvs)
  let C := -BurrHeld.avg (fun i ↦ log (excess x μ i))
  have he (p : ℝ × ℝ) :
      logLik x μ p.2 p.1 s / n = BurrHeld.scaleLik z p + C := by
    rw [avg_eq (fun _ ↦ hv) _ _ hs, BurrHeld.logLik_eq_scale hn, hz]
    dsimp [C]
    ring
  rintro ⟨M, hM⟩
  apply BurrHeld.scale_constant_unbounded hn hnonpos
  refine ⟨M / n - C, ?_⟩
  rintro _ ⟨p, hp, rfl⟩
  have h := hM (mem_image_of_mem _ hp)
  have hh := div_le_div_of_nonneg_right h (Nat.cast_nonneg n : (0 : ℝ) ≤ n)
  change logLik x μ p.2 p.1 s / n ≤ M / n at hh
  rw [he] at hh
  linarith

end
end UnivariateML.ShapeFacesParetoIV
