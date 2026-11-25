# FKML half ties and secondary shape maxima on the fitted square

TASK-276 · 2026-11-25 · source revision `bb391a57789c0f23ea7501668d07ce4deac09955`

A strict, sample-dependent scale cutoff reduces the half-tie comparison to a
compact finite-parameter problem. A cover closed that comparison for
\((0,0,1,3)\) in 1,555 boxes, conditional on the computed bounds. An exact
separated sample proves nonattainment, and a transition in the same sample
family proves finite equality with the collapse ceiling. Shape-profile
unimodality fails already for a distinct pair. It also fails for four
distinct observations with a finite fit at a secondary shape maximum.

Use the notation and closed-support convention of TASK-241, with
\(K=[-1,1]^2\) for \((a,b)\), standardized quantile \(q\), and
\(D(u)=u^{a-1}+(1-u)^{b-1}\). For scale \(s>0\), the density satisfies
\(f(\mu+s q(u))=1/(sD(u))\). The branch in this worktree has no GLD dossier
or fitter. TASK-241 and the FKML roadmap specify the model. The Tukey
lambda cutoff and its refinement motivate the compact comparison below.
[FKML report](2026-11-25-gld-math-report.md) ·
[FKML roadmap](../../roadmap/2026-11-08-quantile-defined-families.md#generalized-lambda-fkml) ·
[Tukey lambda cutoff](2026-11-25-tl-math-report.md) ·
[Tukey lambda refinement](2026-11-25-tlroot-math-report.md#sample-dependent-cutoff)

## Spacing bounds

**Proved.** For a pair of positive-density observations \(t<x\), write
\(p=F(t)\), \(r=F(x)\), \(w=1-p\), \(v=1-r\), \(d=x-t\), and
\[
 G=d/s=\int_p^r D(u)\,du,\qquad
 C=\int_p^r u^{a-1}\,du,\qquad
 P=d^2 f(t) f(x),\qquad R=G/\sqrt{D(r)}.
\]
Then \(\sqrt P=R/\sqrt{D(p)}\). TASK-241's chord inequality gives
\(\sqrt P\le r-p\). For a left observation, use the same notation after
reflection, which interchanges the shapes. Write \(e=1-b\in[0,2]\).
For fixed \(v,w,C\) the following upper bound is convex as a function of \(e\),
since it is a sum and integral of exponentials affine in \(e\):
\[
 R\le\Phi_e(v,w,C)=v^{e/2}\left(C+\int_v^w y^{-e}\,dy\right).
\]
Write \(H=D(p)-2\ge0\). Using \(C\le(w-v)p^{a-1}\le(w-v)(1+H)\),
\[
 \Phi_1\le \sqrt v(\log(1/v)+C),\qquad
 \Phi_2=1-v/w+vC\le1-v^2+v(w-v)H.
 \tag{1}
\]
In particular \(\Phi_2\le1+vH\) and
\(\Phi_2\le1-v^2+H/4\). For \(0<\eta\le1/2\) and \(C\le M\),
\[
 \Phi_{2-\eta}\le 2v^{\eta/2}+M v^{1-\eta/2}.
 \tag{2}
\]
For \(e\le1\), use
\(y^{-e}\le y^{-3/4}D(r)^{1/4}\) on \([v,w]\).
If \(C\le M<G\), integration gives
\[
 G\le M+4D(r)^{1/4},\qquad R\le16G/(G-M)^2.
 \tag{3}
\]
The argument includes positive-density contacts by one-sided limits.
In particular, (3) covers a finite right contact with \(e=0\).

There is also a sharper common envelope on \(K\) than the one needed in
TASK-241. For \(z=q(u)>0\), drop the left, nonpositive term of \(q\)
and bound \(z/\sqrt{D(u)}\) by
\((1-u)^{e/2}\int_{1-u}^1 y^{-e}dy\). This is convex in \(e\), and
both its endpoint values at \(0,2\) equal \(u\). Reflect for \(z<0\).
Together with \(D\ge2\), this proves
\[
 f(x)\le\min\left(\frac1{2s},\frac{s}{(x-\mu)^2}\right).
 \tag{4}
\]

## Half-tie cutoff and compact comparison

**Proved.** Suppose \(n=2k\) and maximal exact multiplicity is \(k\).
Let \(B=\max_t F_t\) be the maximum of TASK-241's collapse values over
half-tied observations \(t\), with constant \(c_t=1/2\) at an extreme
and \(1/8\) otherwise. Let \(\delta\) be half the least distinct gap.
For a location within distance strictly less than \(\delta\) of \(m\)
observations, (4) gives
\[
 L\le 2^{-m}\delta^{-2(n-m)}s^{n-2m}.
\]
Thus, for
\(s\le\min_{0\le m<k}(B2^{m-1}\delta^{2(n-m)})^{1/(n-2m)}\), any
candidate at level \(B\) must be within \(\delta\) of a half-tied value
\(t\). Set \(d_i=|x_i-t|\) for the \(k\) other observations. Applying (4)
to those observations gives the necessary bound
\[
 D(p)\le M=\max\left(2,\left[B^{-1}\prod_i(d_i-\delta)^{-2}\right]^{1/k}\right).
\]
The standardized location of \(t\) satisfies \(|q(p)|\le\sqrt M\).
For each oriented pair, \(G_i=d_i/s\). When \(G_i>\sqrt M\), the
upper bound \(q(r_i)\le1/v_i-1\) gives
\(v_i\le1/(G_i-\sqrt M)\). For any proposed common bound \(v_0\)
on the tail probabilities, it therefore suffices to take
\(G_i>\sqrt M+1/v_0\). Also \(C\le M\). The comparison in this sector
reduces exactly to
\[
 (L/F_t)^{1/2}=(1/[c_t D(p)])^{k/2}\prod_i R_i.
 \tag{5}
\]

For an **extreme tie**, choose \(v_0\le e^{-2}\) small enough that
\(\sqrt v(\log(1/v)+M)<1\) for \(v\le v_0\), and
\(v_0<1/[4(1+\sqrt{M/2})]\). For \(e\le1\), (3) gives \(R<1\) once
\(G>\max(2M,64)\). For \(e\in[1,2]\), (1) and convexity compare
\(R\) with \(\max(\Phi_1,\Phi_2)\). Rationalizing
\(\sqrt{1+H/2}-1\) shows \(1+vH\le\sqrt{1+H/2}\), strictly if
\(H>0\). When \(H=0\), \(\Phi_2\le1-v^2<1\). Consequently every
\(R_i<\sqrt{D(p)/2}\), and (5) is strictly below one.

For an **interior tie**, set
\(\rho=2^{-2k-2}\) and \(\eta=\min(1/2,1/(100k\log M))\).
Choose \(v_0\le e^{-2}\) so that \(Mv_0\le1/100\), and both the
right-hand side of (2) and \(\sqrt v(\log(1/v)+M)\) are below \(\rho\)
for \(v\le v_0\). Take \(G>\max(2M,64/\rho)\) in addition to the
tail-probability requirement. Bounds (1)–(3) give \(R_i\le1+Mv_0\)
for all occupied tails and \(R_i<\rho\) whenever its exponent
\(e\le2-\eta\). If any such observation occurs, (5) is at most
\(2^k\rho(1.01)^{k-1}<1\).

Otherwise put \(\Delta_- = a+1\) and \(\Delta_+=b+1\).
Both exponents exceed one, so \(p,1-p\ge1/M\), and
\[
 D(p)\ge(p^{-2}+(1-p)^{-2})\exp[-(\Delta_-+\Delta_+)\log M].
\]
If \(p\notin[2/5,3/5]\), the parenthesized factor is at least
\(325/36\), giving \(D(p)>8.8\) and making (5) strictly less than one
using \(R_i\le1.01\). For \(p\in[2/5,3/5]\), after either orientation
\(C\le1/p-1\) and \(C-1/(1-p)\le-1/6\).
Consequently \(\Phi_2\le1-v/6<1\). Interpolation with
\(\Phi_{2-\eta}<\rho<1/2\) yields
\(R_i<1-\Delta_j/(2\eta)\) for the corresponding side \(j\).
If \(n_-,n_+\) count the observations on each side, the logarithm of
(5) is strictly below
\[
 \frac{k\log M}{2}(\Delta_-+\Delta_+)
 -\frac{n_-\Delta_-+n_+\Delta_+}{2\eta}\le0.
\]
Both counts are positive and \(p^{-2}+(1-p)^{-2}\ge8\).

Taking the smallest scale threshold over the finitely many sectors and
over the initial multiplicity bound gives an explicit \(\tau>0\) with
\(L<B\) for \(0<s\le\tau\). For instance, use the minimum \(d_i\)
in each sector divided by a strict upper choice of the required \(G\).
This cutoff is uniform over all locations and shapes. It is also uniform
on a compact sample family with a fixed multiplicity pattern if the gaps
and \(B\) have positive lower bounds and the constants \(M\) have a
finite upper bound.

By (4), \(s\le S=B^{-1/n}/2\) is necessary at level \(B\).
For \(s\le S\), a location outside the sample span enlarged at both
ends by \(\sqrt{2S B^{-1/n}}\) has \(L<B\). Let \(V\) be the maximum
on this compact nuisance box times \(K\), using the preceding cutoff,
or zero if the box is empty. TASK-241's upper semicontinuity gives the
maximum on the box. The three outcomes are
\[
 \begin{array}{c|l}
 V<B & \sup L=B\text{ with no finite maximizer}\\
 V=B & \text{finite maximizers attain the collapse ceiling}\\
 V>B & \text{finite maximizers strictly beat the collapse ceiling.}
 \end{array}
\]
This compares finite candidates with the escape value, including when a
finite search has missed an improvement.

**Proved cover property.** On a finite parameter box, bound each
\(z=(x-\mu)/s\) and bracket its inverse using the extreme quantiles.
The quantile is increasing in \(u,a\) and decreasing in \(b\). A
strict support violation at the extreme quantiles eliminates the box.
Given \(u\in[u_l,u_h]\), use
\(D(u)\ge u_h^{a_h-1}+(1-u_l)^{b_h-1}\), with the endpoint power
convention. Multiply the resulting density bounds at the lower scale.
As boxes shrink, the bound converges at interior inverses. A limiting
endpoint with shape below one forces a zero upper limit for its
density. At shape one the bound has upper limit at most the contact
density \(1/(2s)\). A strict support violation is eventually eliminated.
If a strictly subcritical compact problem had arbitrarily small boxes
remaining at level \(B\), compactness would give a limit with likelihood
at least \(B\). Hence sufficiently fine subdivision closes every
strict exclusion in exact arithmetic. For a strict improvement, a
feasible lower witness suffices. A positive-density support contact can
be approached from strict feasibility by expanding the support about
the sample midpoint at fixed shapes. Thus dense finite search can
also witness any strict improvement in exact arithmetic.

### Exact nonattainment and finite equality

**Proved nonattainment.** Take \(x=(0,0,\epsilon,1)\) with the exact
dyadic \(\epsilon=2^{-2^{26}}\) and \(B=1/(4\epsilon^2)\).
One observation in \((0,1)\) is at distance at least \(1/2\) from
\(\mu\), so (4) gives \(L\le1/(2s^2)\). A level-\(B\) candidate
would have \(s\le\sqrt2\epsilon\). If \(|\mu|\ge1/4\), the first
three observations have distances at least \(1/8\), so
\(L\le131072s^2<B\). In the remaining sector, pair each copy of zero
with either \(\epsilon\) or one. Both normalized pair products are
at most one. Their product would have to be at least \(1/4\),
so \(P_{(0,1)}\ge1/4\). By (4) and \(1-\mu\ge3/4\) this
implies \(D(p)\le64/9<8\). For the far pair \(G=1/s\) and
\(v<2s<4\epsilon\).

Set \(h=10^{-6}\). If \(e\le2-h\), (2), (3) and interpolation from
\(e=1\) imply \(R_{(0,1)}<1/2\), contradicting \(P_{(0,1)}\ge1/4\).
Indeed \(2(4\epsilon)^{h/2}<2^{-32}\), the other term in (2) is
smaller than \(2^{-32}\), and the bounds at \(e=1\) and in (3) are
each below \(1/2\). So \(e>2-h\). For the far pair, interpolation
between (1)'s endpoints gives \(R\le1+H/100\). For the near pair
with \(v\le1/100\), (1) gives \(\Phi_1<1+H/3\) using
\(C\le1+H\), and \(\Phi_2<1+H/3\). For the near pair with
\(v\ge1/100\), pointwise comparison with exponent two gives
\[
 R\le v^{-h/2}\Phi_2
 \le 100^{h/2}(1-10^{-4}+H/4)<1+H/3 .
\]
The last bound follows from \(100^{h/2}<1+3h\).
Since \(0\le H<6\), the product of the two \(R\)'s is strictly
less than \((1+H/100)(1+H/3)\le1+H/2=D(p)/2\).
This contradicts level \(B\) via (5). The collapse path from TASK-241
gives supremum \(B\).

**Proved finite equality.** Continue the sample \((0,0,\epsilon,1)\)
over the closed interval from the dyadic above to \(3/4\).
Use a common compact nuisance box for all candidates at level
\(B(\epsilon)=1/(4\epsilon^2)\). It exists by the uniform cutoff and
(4). The set of \(\epsilon\) with a finite candidate at or above
\(B(\epsilon)\) is closed by joint upper semicontinuity, also when
the observations vary. At \(3/4\) the uniform value 1 strictly
beats \(B=4/9\). Any strict improvement persists under a small change
of \(\epsilon\). To see this at a support contact, first expand the
support about \(1/2\) at fixed shapes by a sufficiently small dilation.
The likelihood tends to the old value because endpoint density is
the interior limit, and the new observations are strictly inside the
expanded support. The set is nonempty, excludes the lower endpoint,
and has a first element \(\epsilon_*\) in the interior of the interval.
At that exact, implicitly defined sample there is a finite value
\(B(\epsilon_*)\) and there can be no strict improvement. This proves
finite equality without assigning equality to a small numerical gap.

## Secondary shape maxima

**Proved for the distinct pair \((0,1)\).** Write \(V_2(a,b)\) for its
supremum over finite nuisances. At \((1,-1)\), \(C=w-v\), and (1)
gives \(R\le1-v^2\) with \(D(p)\ge2\). Every finite positive
likelihood is strictly below \(1/2\). At \(p=0\), letting \(v\)
decrease to zero gives the exact finite path values
\[
 P=\frac{(1-v^2)^2}{2(1+v^2)},\qquad s=\frac{v}{1-v^2},
 \qquad \mu=s.
\]
Thus \(V_2(1,-1)=1/2\). The uniform global maximum is 1 by TASK-241.

Here are uniform bounds in the relative neighborhood
\(\mathcal R=[1-h,1]\times[-1,-1+h]\), with \(h=10^{-6}\).
They apply to \(\sqrt P\) for any pair distance.

- If \(p\ge3/10\) or \(v\ge3/10\), the chord gives \(\sqrt P\le7/10\).
- For \(p<3/10\) and \(v\ge1/100\), compare at the same probabilities
  with the corner shapes. At both endpoints the actual \(D\) is at
  least \(100^{-h}\) times the corner \(D\). The spacing grows by at
  most \(\int_0^1 (u^{-h}-1)du<2h\) because changing the right exponent
  can only decrease the right primitive. As the corner \(\sqrt P\)
  is at most \((1-v^2)/\sqrt2\), the actual value is at most
  \(100^h(1-10^{-4}+2h)/\sqrt2 < (1-h/10)/\sqrt2\).
- For \(v\le1/100\), put \(c=1-a\). The left primitive is at most
  \(1/a\le1+2c\). Then \(\Phi_1<0.6\) and
  \(\Phi_2\le1+v\min(H,2c)\). Dividing the latter by
  \(\sqrt{2+H}\) gives at most \(1/\sqrt2\). For \(H\le2c\) this
  follows by rationalizing the square root, and for \(H>2c\) the
  numerator is capped. If \(a<1\) and \(p<3/10\),
  \(D(p)\ge1+(3/10)^{-c}\ge2+c\), so the stronger upper bound
  \((1+c/50)/\sqrt{2+c}<(1-c/10)/\sqrt2\) holds for \(\Phi_2/\sqrt{D(p)}\).
  If \(b>-1\), interpolation of \(\Phi_1,\Phi_2\) gives the
  upper bound \((1-0.4(b+1))/\sqrt2\).
  These inequalities use only \(0<c\le h\) or \(0<b+1\le h\),
  respectively.

Each fixed noncorner shape in \(\mathcal R\) therefore has profiled
value strictly below \(1/2\). On the two edges \(a=1-h\) or
\(b=-1+h\), the common profiled upper bound for a unit pair is
\(U=(1-h/10)^2/2\). The corner is a strict secondary profile maximum.
Its reflection is another. The finite likelihood supremum at each of
these secondary shape maxima is approached by collapse.

**Proved for a larger sample with strict ties.** Take the exact
four-point sample \((0,\varepsilon,1-\varepsilon,1)\) with
\(\varepsilon=10^{-14}\). Pair \((0,1)\) and
\((\varepsilon,1-\varepsilon)\). On either inner edge of
\(\mathcal R\), the full profiled upper bound is
\(U^2/(1-2\varepsilon)^2<(1-3\times10^{-7})/4\).
Throughout the rectangle it is at most
\(1/[4(1-2\varepsilon)^2]<1\).

At the corner choose \(v=10^{-5}\), \(s=v/(1-v^2)\), \(\mu=s\).
The right-hand density decreases on the support, so
\(f(1-\varepsilon)\ge f(1)\). For the inverse \(u\) of \(\varepsilon\),
\(u\le\varepsilon/(2s)\) since \(D\ge2\). Using
\((1-u)^{-2}\le1+4u\) for \(u\le1/4\) gives
\(f(\varepsilon)/f(0)\ge1-\varepsilon/s\).
The pair path above has \(P\ge(1-3v^2)/2\).
Consequently this finite four-point likelihood exceeds
\((1-3\times10^{-9})/4\), which beats the entire inner boundary.
The strict-tie compactness theorem of TASK-241 applies on the closed
rectangle as well, so its maximum is attained at a shape off the
two inner edges. This is a local maximum relative to \(K\).
The span uniform gives 1, so the local maximum is secondary.
Reflection gives another such region. The proof permits a maximizing
set within either region.

## Prototype on the TASK-241 samples

**Proved specialization.** On \((0,0,1,3)\), \(B=1/36\). For
\(s\le1/128\), a location outside \((-1/2,1/2)\) has at most one
observation within distance strictly less than \(1/2\). Its likelihood is
at most \(32s^2<B\). In the tied sector use \(M=24/5\),
\(G\ge128\), and \(v<0.008\). The extreme-tie proof applies with
\(\Phi_1<1\), (3) below 0.14, and the strict \(\Phi_2\) comparison.
This gives a practical lower scale of \(1/128\).

The cover uses \((p,\log s,a,b)\), which eliminates \(\mu\) at the
tied observation. At level \(B\), the product of the two normalized
pair square roots must be at least \(1/2\). The chord requires
\(p\le1-1/\sqrt2<0.3\). Moreover, pairing 1 and 3 and applying (4)
to the copies of zero gives \(|\mu|\le\sqrt{3s}\). The modal
bound gives \(D(p)\le3/s^2\). The normalized \((0,3)\) pair must
have \(P\ge1/4\) and thus gives \(D(p)\le36/(3-\mu)^2\) when
\(\mu<3\). Splitting at \(\mu=5/4\) yields the global cap
\(D(p)\le12\). A box can also use its scale faces in these bounds.
A resulting cap \(M_\text{box}\) and upper shape \(a_h<1\) imply
\(p\ge(M_\text{box}-1)^{-1/(1-a_h)}\).

For each right observation, bound \(r\) by inverting \(G=d/s\) at
box corners. The spacing decreases in \(p,a,b\) at fixed \(r\).
The test also checks the largest possible spacing to the support
endpoint. Upper bounds on \(\sqrt P\) use the chord, the direct
\(G/\sqrt{D(p)D(r)}\) enclosure, and the normalized primitives.
For the latter, bound \(w,C\) from above, take the two extreme
exponents using convexity, and maximize over the resulting interval of
\(v\). For \(0<e<2\) the primitive is a sum of two powers of \(v\)
(or a power times an affine function of \(\log v\) if \(e=1\)).
Endpoints and its explicit stationary point when present suffice.
For \(e\in[1,2]\), the separate bound (1) also preserves the
dependence on \(H\) by using
\((1-v_l^2+v_h\min(H,1/a_l-1))/\sqrt{2+H}\) when \(a_l>0\).
Here \(C\le1/a_l-v\), since the omitted integral over \([r,1]\)
is at least \(v\). The derivative of the bound in \(H\) has affine numerator until the cap, and it decreases
after the cap. The maximum uses the lower \(H\) bound or the cap.
The code evaluates all these bounds as upper comparisons.

**Numerical.** On the workstation the final binary64 cover used a
\(2\times10^{-8}\) margin on the product of normalized square roots.
It visited **1,555 boxes**, discarded 778, and had zero pending, in
**0.260 s** of script wall time. The compact upper comparison and
the analytic small-scale bound give nonattainment for \((0,0,1,3)\)
conditional on the computed bounds. An earlier 40,001-box run without
the level-specific cap on \(D(p)\) retained 14,818 boxes. The cap
removed regions near \(p=0\) that otherwise had loose inverse bounds.
The sufficient generic log-scale cutoff was \(-9.099\) versus
\(-4.852\) for the sample specialization.

| TASK-241 sample | Collapse ceiling | Finite comparison | Boxes |
|---|---:|---|---:|
| \((0,1)\) | \(1/2\) | Uniform gives 1 | 0 |
| \((0,1,2)\) | 0 | Strict ties | 0 |
| \((-2,0,0,3)\) | \(1/2304\) | Uniform gives \(1/625\) | 0 |
| Four sign combinations \(\pm(1\pm1/10000)\) | 0 | Strict ties | 0 |
| \((0,0,1,3)\) | \(1/36\) | Strict cover exclusion | 1,555 |
| \((1/8,1/4,1/2)\) | 0 | Strict ties | 0 |
| \((-1,0,2)\) | 0 | Strict ties | 0 |

The strict-tie rows use TASK-241's existence result. The generic
cutoff for the interior half tie \((-2,0,0,3)\) had log scale about
\(-8192.41\) on the prototype's dyadic grid for \(-\log v_0\).
The uniform comparison avoids that search.

The final Python 3.14.5 and mpmath 1.3.0 audit took **0.870 s**
and used **85 decimal digits**. It checked 4,116 upper bounds using
all 28 increasing pairs from eight probabilities between \(10^{-5}\)
and \(1-10^{-5}\), seven shapes per axis including both endpoints
and zero, and three half-widths \(0,0.002,0.07\). The smallest
binary64 upper bound minus the high-precision feasible value was
\(-5.33\times10^{-17}\), within the practical margin. The fixed
shape witnesses and cutoff constants were checked in the same run.
For the four-point counterexample the audited corner witness was
\(0.249999999725005\) and the inner-edge upper bound was
\(0.249999900000025\). The grid audits supplement the exact formulas.
The cover uses floating-point comparisons with a practical margin
and ordinary binary64 inversion. It has no outward-rounding guarantee.

Both final runs exited zero on the workstation under the one-slot lock.
[Cover code](2026-11-25-gld2-math-data/cover.py) ·
[Cover output](2026-11-25-gld2-math-data/cover.json) ·
[Audit code](2026-11-25-gld2-math-data/check.py) ·
[Audit output](2026-11-25-gld2-math-data/checks.json)

~~~sh
export TMPDIR="$PWD/build/tmp" PYTHONDONTWRITEBYTECODE=1
mkdir -p "$TMPDIR"
d=dev/research/evaluations/2026-11-25-gld2-math-data
/tmp/umvl-cpu/run python3 "$d/cover.py"
/tmp/umvl-cpu/run python3 "$d/check.py"
~~~

## What this changes for the fitter

- On the all-free fitted square, keep the strict majority, strict
  sub-half and distinct-pair routes of TASK-241. For other half ties,
  compute \(B\) and check the span uniform and any converged search
  candidates first. A proved feasible value at or above \(B\) gives
  finite attainment. Use the sample cutoff to exclude small scales
  and the compact cover for a global comparison. Strict exclusion
  at \(B\) proves a collapse limit with no finite MLE. A finite
  equality witness plus a global upper bound of \(B\) proves attained
  equality, with the equally good escape available for diagnostics.
- Keep secondary shape regions in the search. The distinct pair can
  still take the analytic uniform route. For larger samples the
  finite counterexample rules out a general one-peak assumption when
  pruning shape boxes or certifying a local search.
- Use explicit numerical margins and bounded work. If the cover cannot
  close a comparison, retain a feasible search result that passes the
  convergence or score gate, flagged uncertified, unless the
  certificate proves that result wrong. A failed search supplies no
  fallback estimate. A likelihood comparison alone gives neither
  coefficient inclusion at the requested tolerance nor completeness of
  a maximizing set.

## Open

- For an arbitrary fixed half-tied sample whose compact maximum equals
  \(B\), subdivision with strict margins may leave boxes around the
  equality locus. The transition proof establishes actual finite
  equality for an implicit sample. A general exact critical-value
  identity or a comparison procedure that can certify equality at
  isolated roots or on stationary sets is still needed.
- The generic interior-tie cutoff can be far below a useful numerical
  scale. A tighter estimate that retains the sample spacings jointly
  near the two occupied tails, together with a measured cover budget
  for interior comparisons that the uniform cannot resolve, is still
  needed.
