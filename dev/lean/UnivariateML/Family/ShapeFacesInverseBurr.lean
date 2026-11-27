import UnivariateML.Family.BurrHeld
import UnivariateML.Generic.Escape
import UnivariateML.Family.Transfers

/-! Inverse Burr on strictly positive data, with the exact reciprocal Jacobian. -/

open Real Set
namespace UnivariateML.ShapeFacesInverseBurr
open Transfer
noncomputable section

variable {n : ℕ}

def logDensity (k c s x : ℝ) : ℝ :=
  log k + log c - log x + c * log (s / x) - (k + 1) * log (1 + (s / x) ^ c)

def logLik (x : Fin n → ℝ) (k c s : ℝ) : ℝ := ∑ i, logDensity k c s (x i)

def domain : Set (ℝ × ℝ × ℝ) := Ioi 0 ×ˢ (Ioi 0 ×ˢ Ioi 0)

def reciprocateScale (p : ℝ × ℝ × ℝ) : ℝ × ℝ × ℝ :=
  (p.1, p.2.1, p.2.2⁻¹)

theorem logLik_reciprocal {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x k c s = Burr.logLik (reciprocal x) c k s⁻¹ + reciprocalJac x := by
  unfold logLik Burr.logLik reciprocalJac
  rw [← Finset.sum_add_distrib]
  apply Finset.sum_congr rfl
  intro i _
  simp only [logDensity, Burr.logDensity, reciprocal, inv_div_inv, log_inv]
  rw [log_div (hx i).ne' hs.ne', log_div hs.ne' (hx i).ne']
  ring_nf

private theorem inv_bij : BijOn (fun s : ℝ ↦ s⁻¹) (Ioi 0) (Ioi 0) := by
  refine ⟨fun s h ↦ (show 0 < s⁻¹ from inv_pos.mpr h), fun _ _ _ _ h ↦ inv_injective h, ?_⟩
  intro s hs
  exact ⟨s⁻¹, (show 0 < s⁻¹ from inv_pos.mpr hs), inv_inv s⟩

private theorem joint_bij : BijOn reciprocateScale domain domain := by
  refine ⟨?_, ?_, ?_⟩
  · rintro ⟨k,c,s⟩ h
    exact ⟨h.1, h.2.1, inv_bij.mapsTo h.2.2⟩
  · rintro ⟨k,c,s⟩ _ ⟨a,b,t⟩ _ h
    simpa [reciprocateScale] using h
  · rintro ⟨k,c,s⟩ h
    exact ⟨(k,c,s⁻¹), ⟨h.1, h.2.1, inv_bij.mapsTo h.2.2⟩, by simp [reciprocateScale]⟩

theorem joint_maximizers {x : Fin n → ℝ} (hx : ∀ i, 0 < x i) :
    BijOn reciprocateScale
      {p ∈ domain | IsMaxOn (fun q : ℝ × ℝ × ℝ ↦ logLik x q.1 q.2.1 q.2.2) domain p}
      {p ∈ domain | IsMaxOn
        (fun q : ℝ × ℝ × ℝ ↦ Burr.logLik (reciprocal x) q.2.1 q.1 q.2.2) domain p} :=
  Transfer.bijOn_maximizers joint_bij (fun _ hp ↦ logLik_reciprocal hx _ _ hp.2.2)

theorem held_shapes_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k c : ℝ} (hk : 0 < k) (hc : 0 < c) :
    ∃! s, s ∈ Ioi (0 : ℝ) ∧ IsMaxOn (logLik x k c) (Ioi 0) s :=
  (Transfer.existsUnique_max_iff inv_bij (fun _ hs ↦ logLik_reciprocal hx k c hs)).2
    (Burr.heldShape_unique hn (fun i ↦ inv_pos.mpr (hx i)) hc hk)

theorem held_scale_power_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c s : ℝ} (hc : 0 < c) (hs : 0 < s) :
    ∃! k, k ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun k ↦ logLik x k c s) (Ioi 0) k :=
  (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ)))
    (fun k _ ↦ logLik_reciprocal hx k c hs)).2
      (Burr.heldScale_unique hn (fun i ↦ inv_pos.mpr (hx i)) hc (inv_pos.mpr hs))

private def reciprocatePair (p : ℝ × ℝ) : ℝ × ℝ := (p.1, p.2⁻¹)

private theorem pair_bij : BijOn reciprocatePair Lomax.domain Lomax.domain := by
  refine ⟨fun p hp ↦ ⟨hp.1, inv_bij.mapsTo hp.2⟩, ?_, ?_⟩
  · intro p _ q _ h
    change (p.1, p.2⁻¹) = (q.1, q.2⁻¹) at h
    exact Prod.ext (Prod.mk.inj h).1 (inv_injective (Prod.mk.inj h).2)
  · intro p hp
    exact ⟨(p.1, p.2⁻¹), ⟨hp.1, inv_bij.mapsTo hp.2⟩, by simp [reciprocatePair]⟩

theorem held_power_iff (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c) :
    (∃ p ∈ Lomax.domain,
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.1 c q.2) Lomax.domain p) ↔
    ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (reciprocal x) c)),
      Lomax.psi (Burr.powered (reciprocal x) c) u ≤ 0 := by
  exact (Transfer.exists_max_iff pair_bij
    (fun p hp ↦ logLik_reciprocal hx p.1 c hp.2)).trans
      (Burr.exists_max_iff hn (fun i ↦ inv_pos.mpr (hx i)) hc)

theorem held_power_face (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {c : ℝ} (hc : 0 < c)
    (hw : ¬ ∃ u ∈ Ioo 0 (Lomax.uBar (Burr.powered (reciprocal x) c)),
      Lomax.psi (Burr.powered (reciprocal x) c) u ≤ 0) :
    sSup ((fun p : ℝ × ℝ ↦ logLik x p.1 c p.2) '' Lomax.domain) =
      Burr.bestWeibull (reciprocal x) c + reciprocalJac x ∧
    ∀ p ∈ Lomax.domain, logLik x p.1 c p.2 <
      Burr.bestWeibull (reciprocal x) c + reciprocalJac x := by
  have he (p : ℝ × ℝ) (hp : p ∈ Lomax.domain) :
      logLik x p.1 c p.2 =
        Burr.logLik (reciprocal x) c (reciprocatePair p).1 (reciprocatePair p).2 +
          reciprocalJac x := logLik_reciprocal hx p.1 c hp.2
  obtain ⟨hv, hlt⟩ := Burr.exponential_sup_of_no_witness hn
    (fun i ↦ inv_pos.mpr (hx i)) hc hw
  have hb : BddAbove ((fun p : ℝ × ℝ ↦ Burr.logLik (reciprocal x) c p.1 p.2) ''
      Lomax.domain) := ⟨Burr.bestWeibull (reciprocal x) c, by
        rintro _ ⟨p, hp, rfl⟩
        exact (hlt p hp).le⟩
  refine ⟨?_, fun p hp ↦ ?_⟩
  · rw [Transfer.sSup_eq pair_bij he ⟨(1, 1), by norm_num [Lomax.domain]⟩ hb]
    exact congrArg (fun v : ℝ ↦ v + reciprocalJac x) hv
  · rw [he p hp]
    exact add_lt_add_left (hlt _ (pair_bij.mapsTo hp)) _

private theorem avg_eq {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    (k c : ℝ) {s : ℝ} (hs : 0 < s) :
    logLik x k c s / n =
      BurrHeld.logLik (reciprocal x) k c s⁻¹ + reciprocalJac x / n := by
  rw [logLik_reciprocal hx k c hs, add_div]
  congr 1
  unfold Burr.logLik BurrHeld.logLik BurrHeld.avg
  congr 1
  apply Finset.sum_congr rfl
  intro i _
  have hy : 0 < reciprocal x i := inv_pos.mpr (hx i)
  have ht := inv_pos.mpr hs
  dsimp only
  unfold Burr.logDensity BurrHeld.logDensity BurrHeld.softplus
  rw [log_div ht.ne' hy.ne', rpow_def_of_pos (div_pos hy ht),
    log_div hy.ne' ht.ne']
  rw [mul_comm c]
  ring_nf

theorem held_scale_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (hne : ∃ i j, x i ≠ x j) (habove : ∃ i, s < x i) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s) BurrHeld.shapeDomain p := by
  let z := BurrHeld.logRatios (reciprocal x) s⁻¹
  have hz (i : Fin n) : z i = log (s / x i) := by
    simp [z, BurrHeld.logRatios, reciprocal, div_eq_mul_inv, mul_comm]
  have hzvar : ∃ i j, z i ≠ z j := by
    obtain ⟨i, j, hij⟩ := hne
    refine ⟨i, j, fun h ↦ hij ?_⟩
    rw [hz, hz] at h
    have h' := (log_injOn_pos (div_pos hs (hx i)) (div_pos hs (hx j))) h
    exact inv_injective (mul_left_cancel₀ hs.ne' (by rw [← div_eq_mul_inv, ← div_eq_mul_inv]; exact h'))
  have hzneg : ∃ i, z i < 0 := by
    obtain ⟨i, hi⟩ := habove
    exact ⟨i, by rw [hz]; exact log_neg (div_pos hs (hx i)) ((div_lt_one (hx i)).2 hi)⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have heq (q : ℝ × ℝ) :
      logLik x q.2 q.1 s / n =
        BurrHeld.scaleLik z q +
          (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i))) := by
    rw [avg_eq hx _ _ hs, BurrHeld.logLik_eq_scale hn]
    dsimp [z]
    ring_nf
  obtain ⟨p, hp, hu⟩ := (Transfer.existsUnique_max_iff
    (bijOn_id BurrHeld.shapeDomain) (fun q _ ↦ heq q)).2
      (BurrHeld.scale_existsUnique hn hzvar hzneg)
  exact ⟨p, ⟨hp.1, fun _ h ↦ (div_le_div_iff_of_pos_right hn').mp (hp.2 h)⟩,
    fun q h ↦ hu q ⟨h.1, fun _ ha ↦ (div_le_div_iff_of_pos_right hn').mpr (h.2 ha)⟩⟩

theorem held_scale_face (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {s : ℝ} (hs : 0 < s) (hle : ∀ i, x i ≤ s) (hlt : ∃ i, x i < s) :
    let z := BurrHeld.logRatios (reciprocal x) s⁻¹
    let value := BurrHeld.paretoValue z + (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i)))
    IsLUB ((fun q : ℝ × ℝ ↦ logLik x q.2 q.1 s / n) '' BurrHeld.shapeDomain) value ∧
      ∀ q ∈ BurrHeld.shapeDomain, logLik x q.2 q.1 s / n < value := by
  let z := BurrHeld.logRatios (reciprocal x) s⁻¹
  have hratio (i : Fin n) : z i = log (s / x i) := by
    simp [z, BurrHeld.logRatios, reciprocal, div_eq_mul_inv, mul_comm]
  have hz (i : Fin n) : 0 ≤ z i := by
    rw [hratio]
    exact log_nonneg ((one_le_div (hx i)).2 (hle i))
  have hpos : ∃ i, 0 < z i := by
    obtain ⟨i, hi⟩ := hlt
    exact ⟨i, by rw [hratio]; exact log_pos ((one_lt_div (hx i)).2 hi)⟩
  obtain ⟨hlub, hu⟩ := BurrHeld.scale_pareto_supremum hn hz hpos
  have heq (q : ℝ × ℝ) :
      logLik x q.2 q.1 s / n =
        BurrHeld.scaleLik z q + (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i))) := by
    rw [avg_eq hx _ _ hs, BurrHeld.logLik_eq_scale hn]
    dsimp [z]
    ring_nf
  refine ⟨?_, fun q hq ↦ ?_⟩
  · exact (Transfer.isLUB_iff (bijOn_id BurrHeld.shapeDomain) (fun q _ ↦ heq q) _).2 hlub
  · rw [heq]
    exact add_lt_add_left (hu q hq) _

private theorem first_bij :
    BijOn (fun p : ℝ × ℝ ↦ (p.1, -(p.1 * log p.2⁻¹)))
      BurrHeld.shapeDomain BurrHeld.firstDomain := by
  refine ⟨fun p hp ↦ ⟨hp.1, mem_univ _⟩, ?_, ?_⟩
  · rintro ⟨c,s⟩ hp ⟨d,t⟩ hq h
    have hc : 0 < c := hp.1
    have hs : 0 < s := hp.2
    have ht : 0 < t := hq.2
    have hcd : c = d := congrArg Prod.fst h
    subst d
    have hb : -(c * log s⁻¹) = -(c * log t⁻¹) := congrArg Prod.snd h
    have hEq := (BurrHeld.first_intercept_iff hc (inv_pos.mpr hs)).mp hb
    have hEq' := (BurrHeld.first_intercept_iff hc (inv_pos.mpr ht)).mp
      (rfl : -(c * log t⁻¹) = -(c * log t⁻¹))
    exact Prod.ext rfl (inv_injective (hEq.trans hEq'.symm))
  · rintro ⟨c,b⟩ hp
    have hc : 0 < c := hp.1
    have hs : 0 < exp (-b / c) := exp_pos _
    refine ⟨(c, (exp (-b / c))⁻¹), ⟨hc, inv_pos.mpr hs⟩, Prod.ext rfl ?_⟩
    change -(c * log ((exp (-b / c))⁻¹)⁻¹) = b
    rw [inv_inv]
    exact (BurrHeld.first_intercept_iff hc hs).mpr rfl

theorem held_first_unique (hn : 0 < n) {x : Fin n → ℝ} (hx : ∀ i, 0 < x i)
    {k : ℝ} (hk : 0 < k) (hne : ∃ i j, x i ≠ x j) :
    ∃! p, p ∈ BurrHeld.shapeDomain ∧
      IsMaxOn (fun q : ℝ × ℝ ↦ logLik x k q.1 q.2) BurrHeld.shapeDomain p := by
  let y : Fin n → ℝ := fun i ↦ log (reciprocal x i)
  have hy (i : Fin n) : 0 < reciprocal x i := inv_pos.mpr (hx i)
  have hyvar : ∃ i j, y i ≠ y j := by
    obtain ⟨i, j, hij⟩ := hne
    exact ⟨i, j, fun h ↦ hij (inv_injective (log_injOn_pos (hy i) (hy j) h))⟩
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  have heq (q : ℝ × ℝ) (hq : q ∈ BurrHeld.shapeDomain) :
      logLik x k q.1 q.2 / n =
        BurrHeld.firstLik y k (q.1, -(q.1 * log q.2⁻¹)) +
          (reciprocalJac x / n - BurrHeld.avg y) := by
    rw [avg_eq hx _ _ hq.2, BurrHeld.logLik_eq_first hn hy (inv_pos.mpr hq.2)]
    dsimp [y]
    ring_nf
  obtain ⟨p, hp, hu⟩ := (Transfer.existsUnique_max_iff first_bij heq).2
    (BurrHeld.first_existsUnique hn hk hyvar)
  exact ⟨p, ⟨hp.1, fun _ h ↦ (div_le_div_iff_of_pos_right hn').mp (hp.2 h)⟩,
    fun q h ↦ hu q ⟨h.1, fun _ ha ↦ (div_le_div_iff_of_pos_right hn').mpr (h.2 ha)⟩⟩


private theorem softplus_convex : ConvexOn ℝ univ BurrHeld.softplus := by
  have hd (t : ℝ) : HasDerivAt BurrHeld.softplus (exp t / (1 + exp t)) t :=
    ((hasDerivAt_exp t).const_add 1).log (by positivity)
  apply MonotoneOn.convexOn_of_deriv (convex_univ (𝕜 := ℝ))
    (fun t _ ↦ (hd t).continuousAt.continuousWithinAt)
    (fun t _ ↦ (hd t).differentiableAt.differentiableWithinAt)
  intro s _ t _ hst
  rw [(hd s).deriv, (hd t).deriv]
  apply (div_le_div_iff₀ (by positivity) (by positivity)).2
  nlinarith [exp_le_exp.mpr hst]

private theorem fixedScale_strictConcave (z : Fin n → ℝ) {k : ℝ} (hk : 0 < k) :
    StrictConcaveOn ℝ (Ioi 0) (fun b ↦ BurrHeld.scaleLik z (b, k)) := by
  refine ⟨convex_Ioi _, ?_⟩
  intro p hp q hq hpq a b ha hb hab
  let A (r : ℝ) (i : Fin n) := k * BurrHeld.softplus (r * z i) +
    BurrHeld.softplus (-(r * z i))
  have hA (i : Fin n) : A (a * p + b * q) i ≤ a * A p i + b * A q i := by
    have h₁ := softplus_convex.2 (mem_univ (p * z i)) (mem_univ (q * z i)) ha.le hb.le hab
    have h₂ := softplus_convex.2 (mem_univ (-(p * z i))) (mem_univ (-(q * z i))) ha.le hb.le hab
    simp only [smul_eq_mul] at h₁ h₂
    unfold A
    rw [show (a * p + b * q) * z i = a * (p * z i) + b * (q * z i) by ring]
    rw [show -(a * (p * z i) + b * (q * z i)) = a * -(p * z i) + b * -(q * z i) by ring]
    nlinarith only [h₂, mul_le_mul_of_nonneg_left h₁ hk.le]
  have hsum : BurrHeld.avg (A (a * p + b * q)) ≤
      a * BurrHeld.avg (A p) + b * BurrHeld.avg (A q) := by
    have h := Finset.sum_le_sum (s := Finset.univ) (fun i _ ↦ hA i)
    unfold BurrHeld.avg
    simp only [Finset.sum_add_distrib, ← Finset.mul_sum] at h
    rw [← mul_div_assoc, ← mul_div_assoc, ← add_div]
    exact div_le_div_of_nonneg_right h (Nat.cast_nonneg _)
  have he (r : ℝ) : BurrHeld.scaleLik z (r, k) = log k + log r - BurrHeld.avg (A r) := by
    simp only [BurrHeld.scaleLik, BurrHeld.G, BurrHeld.R, A, BurrHeld.avg,
      Finset.sum_add_distrib, ← Finset.mul_sum, add_div, mul_div_assoc]
    ring
  simp only [smul_eq_mul, he]
  have hlog := strictConcaveOn_log_Ioi.2 hp hq hpq ha hb hab
  simp only [smul_eq_mul] at hlog
  nlinarith only [hlog, hsum, congrArg (fun t : ℝ ↦ t * log k) hab]

private theorem scale_single_bound (z : Fin n → ℝ) {k b : ℝ}
    (hk : 0 < k) (hb : 0 < b) (i : Fin n) :
    (min 1 k * |z i| / n) * b ≤ log k + log b - BurrHeld.scaleLik z (b, k) := by
  let A (j : Fin n) := k * BurrHeld.softplus (b * z j) + BurrHeld.softplus (-(b * z j))
  have hpos (t : ℝ) : 0 ≤ BurrHeld.softplus t := log_nonneg (by linarith [exp_pos t])
  have hle (t : ℝ) : t ≤ BurrHeld.softplus t := by
    conv_lhs => rw [← log_exp t]
    exact log_le_log (exp_pos _) (by linarith)
  have hp : min 1 k * |b * z i| ≤ A i := by
    unfold A
    rcases le_total 0 (b * z i) with hz | hz
    · rw [abs_of_nonneg hz]
      linarith [mul_le_mul_of_nonneg_right (min_le_right 1 k) hz,
        mul_le_mul_of_nonneg_left (hle (b * z i)) hk.le, hpos (-(b * z i))]
    · rw [abs_of_nonpos hz]
      linarith [mul_le_mul_of_nonneg_right (min_le_left 1 k) (neg_nonneg.mpr hz),
        hle (-(b * z i)), mul_nonneg hk.le (hpos (b * z i))]
  have hsum : A i ≤ ∑ j, A j := Finset.single_le_sum (f := A)
    (fun j _ ↦ add_nonneg (mul_nonneg hk.le (hpos _)) (hpos _)) (Finset.mem_univ i)
  have h := div_le_div_of_nonneg_right (hp.trans hsum) (Nat.cast_nonneg n : (0 : ℝ) ≤ n)
  rw [abs_mul, abs_of_pos hb] at h
  have he : (∑ j, A j) / n = k * BurrHeld.G z b + BurrHeld.R z b := by
    simp only [A, BurrHeld.G, BurrHeld.R, BurrHeld.avg, Finset.sum_add_distrib,
      ← Finset.mul_sum, add_div, mul_div_assoc]
  rw [he] at h
  have hh : (min 1 k * (b * |z i|)) / n = (min 1 k * |z i| / n) * b := by ring
  rw [hh] at h
  unfold BurrHeld.scaleLik
  linarith

private theorem fixedScale_unique (hn : 0 < n) (z : Fin n → ℝ) {k : ℝ} (hk : 0 < k)
    (hne : ∃ i, z i ≠ 0) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun c ↦ BurrHeld.scaleLik z (c, k)) (Ioi 0) b := by
  obtain ⟨i, hi⟩ := hne
  let f : ℝ → ℝ := fun c ↦ BurrHeld.scaleLik z (c, k)
  let d := min 1 k * |z i| / n / 2
  have hN : 0 < (n : ℝ) := by exact_mod_cast hn
  have hd : 0 < d := div_pos (div_pos (mul_pos (lt_min zero_lt_one hk) (abs_pos.mpr hi)) hN)
    (by norm_num)
  let A := exp (f 1 - log k)
  let B := (log k - f 1 - log d - 1) / d
  have hlevel (b : ℝ) (hb : 0 < b) (hM : f 1 ≤ f b) : b ∈ Icc A B := by
    have hbound := scale_single_bound z hk hb i
    change (min 1 k * |z i| / n) * b ≤ log k + log b - f b at hbound
    rw [show min 1 k * |z i| / n = 2 * d by dsimp [d]; ring] at hbound
    have h₀ : A ≤ b := by
      rw [← exp_log hb]
      exact exp_le_exp.mpr (by nlinarith only [hM, hbound, mul_nonneg hd.le hb.le])
    have hlog := log_le_sub_one_of_pos (mul_pos hd hb)
    rw [log_mul hd.ne' hb.ne'] at hlog
    have h₁ : b ≤ B := (le_div_iff₀ hd).2 (by nlinarith only [hM, hbound, hlog])
    exact ⟨h₀, h₁⟩
  let hconc := fixedScale_strictConcave z hk
  have hw := hlevel 1 zero_lt_one le_rfl
  have hsub : Icc A B ⊆ Ioi (0 : ℝ) := fun _ h ↦ (exp_pos _).trans_le h.1
  obtain ⟨p, hp, hm⟩ := isCompact_Icc.exists_isMaxOn ⟨1, hw⟩
    ((hconc.concaveOn.continuousOn isOpen_Ioi).mono hsub)
  have hmax : IsMaxOn f (Ioi 0) p := by
    intro r hr
    by_cases h : f 1 ≤ f r
    · exact hm (hlevel r hr h)
    · exact (not_le.mp h).le.trans (hm hw)
  exact ⟨p, ⟨hsub hp, hmax⟩, fun r hr ↦ hconc.eq_of_isMaxOn hr.2 hmax hr.1 (hsub hp)⟩

theorem held_first_scale_unique (hn : 0 < n) {x : Fin n → ℝ}
    (hx : ∀ i, 0 < x i) {k s : ℝ} (hk : 0 < k) (hs : 0 < s)
    (hne : ∃ i, x i ≠ s) :
    ∃! b, b ∈ Ioi (0 : ℝ) ∧ IsMaxOn (fun c ↦ logLik x k c s) (Ioi 0) b := by
  let z := BurrHeld.logRatios (reciprocal x) s⁻¹
  have hz : ∃ i, z i ≠ 0 := by
    obtain ⟨i, hi⟩ := hne
    refine ⟨i, fun he ↦ hi ?_⟩
    have hh : log (s / x i) = 0 := by
      simpa [z, BurrHeld.logRatios, reciprocal, div_eq_mul_inv, mul_comm] using he
    rw [← log_one] at hh
    exact ((div_eq_one_iff_eq (hx i).ne').mp
      (log_injOn_pos (div_pos hs (hx i)) (by norm_num : (1 : ℝ) ∈ Ioi 0) hh)).symm
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have he (b : ℝ) :
      logLik x k b s / n = BurrHeld.scaleLik z (b, k) +
        (reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i))) := by
    rw [avg_eq hx _ _ hs, BurrHeld.logLik_eq_scale hn]
    dsimp [z]
    ring_nf
  obtain ⟨p, hp, hu⟩ := (Transfer.existsUnique_max_iff (bijOn_id (Ioi (0 : ℝ)))
    (fun b _ ↦ he b)).2 (fixedScale_unique hn z hk hz)
  exact ⟨p, ⟨hp.1, fun _ h ↦ (div_le_div_iff_of_pos_right hN).mp (hp.2 h)⟩,
    fun q h ↦ hu q ⟨h.1, fun _ ha ↦ (div_le_div_iff_of_pos_right hN).mpr (h.2 ha)⟩⟩


open Filter

theorem held_first_constant_path (hn : 0 < n) {v k : ℝ} (hv : 0 < v) (_hk : 0 < k) :
    Tendsto (fun b ↦ logLik (fun _ : Fin n ↦ v) k b v) atTop atTop := by
  have hN : (0 : ℝ) < n := by exact_mod_cast hn
  have h (b : ℝ) : logLik (fun _ : Fin n ↦ v) k b v =
      (n : ℝ) * log b + (n : ℝ) * (log k - log v - (k + 1) * log 2) := by
    simp [logLik, logDensity, div_self hv.ne', nsmul_eq_mul]
    ring_nf
  simp only [h]
  exact tendsto_atTop_add_const_right atTop _ (tendsto_log_atTop.const_mul_atTop hN)

theorem joint_constant_unbounded (hn : 0 < n) {v : ℝ} (hv : 0 < v) :
    ¬ BddAbove ((fun p : ℝ × ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) p.1 p.2.1 p.2.2) ''
      domain) := by
  rintro ⟨M, hM⟩
  have h : ∀ᶠ b : ℝ in atTop, M < logLik (fun _ : Fin n ↦ v) 1 b v :=
    (held_first_constant_path hn hv (by norm_num : (0 : ℝ) < 1)).eventually (eventually_gt_atTop M)
  obtain ⟨b, hb, hgt⟩ := ((eventually_gt_atTop (0 : ℝ)).and h).exists
  have hmem : (1, b, v) ∈ domain := by
    change (0 : ℝ) < 1 ∧ 0 < b ∧ 0 < v
    exact ⟨by norm_num, hb, hv⟩
  have hle : logLik (fun _ : Fin n ↦ v) 1 b v ≤ M := hM ⟨(1, b, v), hmem, rfl⟩
  exact hgt.not_ge hle

theorem held_first_scale_unbounded (hn : 0 < n) {x : Fin n → ℝ} {k s : ℝ}
    (hk : 0 < k) (hs : 0 < s) (he : ∀ i, x i = s) :
    Tendsto (fun b ↦ logLik x k b s) atTop atTop := by
  rw [show x = fun _ ↦ s from funext he]
  exact held_first_constant_path hn hs hk

theorem held_scale_constant_unbounded (hn : 0 < n) {v s : ℝ}
    (hs : 0 < s) (hsv : s ≤ v) :
    ¬ BddAbove ((fun p : ℝ × ℝ ↦ logLik (fun _ : Fin n ↦ v) p.2 p.1 s) ''
      BurrHeld.shapeDomain) := by
  let x : Fin n → ℝ := fun _ ↦ v
  have hv : 0 < v := hs.trans_le hsv
  let z : Fin n → ℝ := fun _ ↦ log (s / v)
  have hz : BurrHeld.logRatios (reciprocal x) s⁻¹ = z := by
    funext i
    simp [BurrHeld.logRatios, reciprocal, x, z, div_eq_mul_inv, mul_comm]
  have hnonpos : log (s / v) ≤ 0 := log_nonpos (div_pos hs hv).le ((div_le_one hv).2 hsv)
  let C := reciprocalJac x / n - BurrHeld.avg (fun i ↦ log (reciprocal x i))
  have he (p : ℝ × ℝ) :
      logLik x p.2 p.1 s / n = BurrHeld.scaleLik z p + C := by
    rw [avg_eq (fun _ ↦ hv) _ _ hs, BurrHeld.logLik_eq_scale hn, hz]
    dsimp [C]
    ring
  rintro ⟨M, hM⟩
  apply BurrHeld.scale_constant_unbounded hn hnonpos
  refine ⟨M / n - C, ?_⟩
  rintro _ ⟨p, hp, rfl⟩
  have h := hM (mem_image_of_mem _ hp)
  have hh := div_le_div_of_nonneg_right h (Nat.cast_nonneg n : (0 : ℝ) ≤ n)
  change logLik x p.2 p.1 s / n ≤ M / n at hh
  rw [he] at hh
  linarith

end
end UnivariateML.ShapeFacesInverseBurr
