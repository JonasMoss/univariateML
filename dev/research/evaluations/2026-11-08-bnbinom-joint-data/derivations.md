# Uniform exterior bounds for beta negative binomial fitting

These are analytic bounds, conditional on resolved floating-point comparisons
when evaluated. They require nonnegative integer observations and positive shapes.
Write `a=alpha`, `b=beta`, and `r=size`.

## Size held

Let `s=a+b`, `p=a/s`, and `q=1-p`. The beta-mixture representation and
Jensen's inequality give, for `delta=min(r,1)` and `C_x=(r)_x/x!`,

\[
p_x\le\min(1,C_xp^\delta),\qquad
P(X>0)\le\min(1,\max(1,r)q),\qquad
p_x\le\min(1,(b)_x/x!).
\]

The first inequality uses `E Q^delta <= (E Q)^delta` for
`Q~Beta(a,b)`. The second uses `1-Q^r <= max(1,r)(1-Q)`.
These bounds exclude, uniformly in the other variables, `p<=pmin`,
`q<=qmin`, and `b<=l` from a specified finite likelihood superlevel.

The digamma integral comparison `log(z)-1/z <= psi(z) <= log(z)` implies

\[
|\log\Gamma(z+c)-\log\Gamma(z)-c\log z|
\le c(c+2)/(2z)\quad(z,c>0).
\]

Indeed integrate `psi(z+t)-log z` from zero to `c`, bounding its absolute
value by `(t+1)/z`. Consequently

\[
|\ell_{BNB}-\ell_{NB}(r,p)|\le C/s,\quad
C=\frac12\sum_xn_x\{r(r+2)/p_{min}+x(x+2)/q_{min}+(r+x)(r+x+2)\}.
\]

If a finite witness lower bound `L` exceeds the fixed-size NB face upper
bound `F`, take `S=2C/(L-F)`. Every global maximum is in
`a in [pmin*l,S]`, `b in [l,S]`. Outside the rectangle the likelihood is
below the witness: the only previously unexcluded case has `s>=S`.
This is uniform as alpha varies, including both ratio corners.

The mixed derivative is `psi1(a+b)-E psi1(a+b+r+X)>0`.
The scalar single-crossing theorem therefore orders coordinate-score signs
at opposite rectangle corners. The production cover combines these signs,
monotone mass bounds and isolated negative-definite root neighborhoods.
The beta-held pair is obtained by interchanging the two **actual** shape
values r and b; the held value becomes the NB size and stays fixed.

## Winning-face supporting polynomial

For a fixed-size NB face with fitted `p*=r/(r+mean)`, `q*=1-p*`, put

\[
D(t)=\frac{(1-t)^r}{(p^*)^r}\sum_x n_x(t/q^*)^x.
\]

For any mixing law G, concavity of log gives
`ell(G)-ell(NB*) <= integral D(1-p)dG(p)-n`.
The sign of the derivative of D is the sign of a polynomial whose
coefficient at degree k has the sign of

\[
(k+1)n_{k+1}-(k+r)q^*n_k.
\]

If the nonzero coefficients change sign just once from positive to negative,
Descartes' rule gives exactly one positive root. The NB likelihood equation
places it at `t=q*`, where `D=n`. Thus `D<=n` globally. The derivative is
nonzero off this root on `(0,1)`. The inequality for a nondegenerate beta
mixing law is strict; the face is a supremum and no finite BNB attains it.
Sparse coefficients are checked without scanning the count range.
Unresolved coefficient signs do not pass the test.

The same argument for a mixed Poisson law uses
`D(lambda)=exp(mean-lambda)*sum n_x*(lambda/mean)^x` and coefficient signs
`(k+1)n_{k+1}-mean*n_k`. BNB is mixed Poisson: conditionally draw the
beta success probability, then the gamma Poisson rate of its NB law.
This provides an all-free **Poisson-face** proof for every sample passing
the coefficient sign test. It is sufficient, not necessary.

## All-free exterior geometry

Set `m=rb/(a+r+b)` and `lambda=rb/a`. Since trigamma(z)>=1/z,

\[
\log p_0=-\int_0^r\int_0^b\psi_1(a+u+v)\,dv\,du\le-m.
\]

Also `(r+k)(b+k)/(a+r+b+k)<=m+k`. Hence for every count

\[
\log p_x\le-m+\log(m)_x-\log x!.
\]

The resulting likelihood upper bound `H(m)` is decreasing for
`m>mean`, since `H'(m)<=-n+n*mean/m`, and tends to minus infinity.
Doubling therefore finds an effective `M>mean` with `H(M)<L`, so every
point in a finite superlevel satisfies `m<M`.

The zero-mass bound and `log(1+k/r)<=k/r` give another uniform inequality:

\[
\ell_{BNB}\le\ell_{Pois}(\lambda)+
\left(n\lambda^2+\tfrac12\sum_x n_xx(x-1)\right)(1/r+1/b).
\]

This uses `lambda-m <= lambda^2*(1/r+1/b)` and discards the negative
count-denominator corrections. If `r,b>=U>=4M`, the identity
`1/m=1/lambda+1/r+1/b` implies `lambda<=2M`. With
`A=4nM^2+sum n_x*x*(x-1)/2`, the entire such neighborhood has
likelihood at most the fitted Poisson likelihood plus `2A/U`.
A witness beating Poisson therefore yields an effective upper cutoff on
`min(r,b)`, uniformly in alpha.

After ordering `r<=b`, a scalar small-shape bound gives `r>=l` and the
Poisson cutoff gives `r<=U`. On this compact interval replace r by U in
C_x and by min(l,1) in the Jensen exponent; use max(U,1) for the positive
mass bound. This supplies uniform ratio cutoffs. The NB remainder above,
with r replaced by U, then excludes `a+b>=S` whenever the witness beats
the **globally fitted** NB face. Thus a finite effective superlevel box
exists even in the all-free case, subject to those strict likelihood gaps.

The remaining all-free certificate requires a complete three-dimensional
stationary/root cover or a rigorously bounded two-dimensional profiled cover,
including the r=b seam. The inequalities alone do not certify an optimizer
endpoint or a winning NB face. The production all-free finite search remains
explicitly uncertified; its coefficients are preserved in this job.

A sharper ratio bound avoids the rising-factorial coefficient at U. If
`r>=l`, choose `delta=min(l,1)`. Stochastic monotonicity of NB in its size
and the finite generalized-binomial sum give

\[
p_x\le P_r(X\le x)\le\frac{(\delta+1)_x}{x!}E Q^\delta
\le\frac{(\delta+1)_x}{x!}p^\delta.
\]

The retained `profile-probe.cpp` uses this stronger bound, the Poisson
cutoff and the NB remainder to quantify a valid all-free superlevel box on
the frozen sample. It then tries to reuse the existing certified conditional
alpha solver at its profile endpoints. A numerical stop from these probes
is an arithmetic limitation of that proposed composition, not a proof that
no other cover can work. A practical cover needs coarser signed profile
brackets, stable differences in the face neighborhoods, or a different
covering strategy, followed by complete root isolation and comparisons.

The natural profiled cover would use the unique conditional alpha maximum.
Its score has positive mixed derivatives in both other shapes:

\[
\partial_r S_a=\psi_1(a+r)-E\psi_1(a+r+b+X)>0,\qquad
\partial_b S_a=\psi_1(a+b)-E\psi_1(a+r+b+X)>0.
\]

Scalar single crossing makes the conditional alpha mode increase in r and b.
Enclosing the two corner modes would therefore enclose the profile over a
whole r/b rectangle. The probe tests whether the current scalar certificate
can supply such endpoint enclosures on the effective all-free domain.
