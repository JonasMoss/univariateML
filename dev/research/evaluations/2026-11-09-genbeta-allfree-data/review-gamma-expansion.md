# The profiled first-order gamma coefficient vanishes identically

The literal first-order term vanishes. A signed term in squared power, with the uniform fourth-order bound below, supplies the implemented winning-gamma certificate. The asymptotic argument and explicit uniform bounds are mathematical evidence; their numerical checks are recorded separately.

Use normalized observations v with mean mu=1, h=a mean(Y), and lambda=p h. Per observation, apart from the fixed Jacobian, the log likelihood is

    F(h,q,lambda)=g(q,lambda)+h b(q,lambda)+O(h^2),
    g=q log(lambda)-log Gamma(q)+(q-1)mean(log(v))-lambda,
    b=(q-1)/2 * (q/lambda-1).

The terms follow from log Gamma(p+q)-log Gamma(p)=q log(p)+q(q-1)/(2p)+O(p^-2), and log(1-exp(-h v))=log(h v)-h v/2+O(h^2). They are uniform over compact positive q,lambda rectangles, including derivatives of any fixed finite order. The previously proved conditional compactness supplies such a rectangle, and p diverges uniformly. Thus taking the boundary maximum and applying the envelope theorem is justified for nonconstant data.

The gamma fitted rate is lambda_hat=q_hat. Consequently

    c1=b(q_hat,lambda_hat)=0

for **every** nonconstant sample. This is not confined to the quartic seam. With unnormalized data the same identity is (q-1)(q/lambda-mean(Y))/2=0. Equivalently b is minus (q-1)/2 times the gamma rate score. A nonzero first-order estimate from imperfect gamma convergence would therefore be a rate-fit error, not evidence for a signed neighborhood. No c1<0 branch can validly certify a genbeta gamma face.

There is also a direct uniform squeeze, avoiding reliance on the envelope theorem alone. The existing implemented bound is P(h)<=G+Cupper*h^2 on its explicit positive domain. For the lower bound choose q=q_hat, nu=q_hat and p=q_hat/h-(q_hat-1)/2. For 0<h<=1 this is a proper positive member. The integral representation

    psi(z+1/2)-log(z)
      = integral_0^infinity exp(-z*t)*(1/t-1/(2*sinh(t/2))) dt > 0

and the second-derivative bound for log on [t-q/2,t+q/2] imply

    Ac(t,q) >= -q^3/(24*(t-q/2)^2),     t=q/h.

Together with 0<=T(hv)<=h^2*v^2/24, the exact centered decomposition from proof.md gives

    P(h) >= F(h,q_hat,p_hat)
         >= G-h^2*(q_hat/6 + max(0,1-q_hat)*mean(v^2)/24),  0<h<=1.

Thus |P(h)-G|<=C*h^2 on the intersection of these explicit domains, for a finite sample-dependent C. In particular the right derivative c1 is exactly zero, uniformly over all conditional shape/power paths. The positive c1<0 test proposed in review has no instances in this family.

The information adjustment confirms the cancellation and gives the next term. At the normalized gamma maximum the information matrix and gradient are

    J = [[trigamma(q), -1/q],[-1/q,1/q]],
    grad(b) = [(q-1)/(2q), -(q-1)/(2q)].

The determinant is positive since trigamma(q)>1/q. Thus J^-1 grad(b)=(0,-(q-1)/2): the shape has no first-order motion; the uncentered rate shifts by -(q-1)h/2. The second raw coefficient is

    d=(q-1)*mean(v^2)/24 - q*(q-1)*(2*q-1)/(12*lambda^2).

Adding the profile correction grad(b)' J^-1 grad(b)/2 gives

    c2=(q-1)/24 * (mean(v^2)-(q+1)/q).

Centering nu=h*(p+(q-1)/2) makes the objective expansion even in h: the centered gamma ratio has only even inverse-rate powers and log(sinh(hv/2)/(hv/2)) is even. The local profiled expansion is therefore G+c2 h^2+O(h^4), with a sample-dependent finite-order remainder. The local expansion alone does not supply a computed uniform signed fourth-order bound for the full conditional profile. The following argument supplies that additional bound; changing the sign of the previous unsigned quadratic envelope would not suffice.

## An explicit signed fourth-order bound

The new `gamma_face_certificate.hpp` is called by the all-free gamma route. Its neighborhood bound uses squared power s=h^2, so the first coefficient in s is c2 above. The literal first coefficient in h remains zero.

Put nu=h*(p+(q-1)/2), t=nu/h, and use the exact decomposition from proof.md. For z>0,

    0 < psi(z+1/2)-log(z) <= 1/(24*z^2).

The upper bound follows from sinh(x)*(1-x^2/6)<=x: expansion in powers of x has zero cubic coefficient and strictly negative higher odd coefficients. Integrating its Laplace-transform form proves the digamma bound. Integrate the centered log term symmetrically, keeping its leading negative term, and integrate the reciprocal-square bound exactly. For t>q/2,

    Ac(t,q) <= q*(1-q^2)/(24*t^2)
               +q^3/(96*t^2*(t^2-q^2/4)).

Euler's product for sinh and u-u^2/2<=log(1+u)<=u give

    z^2/24-z^4/2880 <= T(z) <= z^2/24.

Therefore, throughout any positive compact q,nu rectangle with nu>q*h/2,

    F <= g(q,nu)+h^2*C(q,nu)+h^4*R,
    C = (q-1)*mean(v^2)/24+q*(1-q^2)/(24*nu^2),
    R <= qHi^3/[96*nuLo^2*(nuLo^2-qHi^2*H^2/4)]
          +max(0,1-qLo)*mean(v^4)/2880,      h<=H.

This bound is signed at second order and explicit at fourth order; it is not a sign change of the old unsigned quadratic envelope.

### Including motion of both fitted gamma coefficients

For mean(v)=mu, the exact gamma profile in q has second derivative 1/q-trigamma(q)<-1/(2*q^2). The trigamma inequality follows from the positive integral kernel u/(1-exp(-u))>1+u/2. Its rate gap is strongly concave along the segment to nu=q/mu. On a rectangle containing both points this gives

    G-g(q,nu) >= (q-k)^2/(4*Q^2)
                   +qLo*(nu-q/mu)^2/(2*nuHi^2).

The old unsigned quadratic upper and the feasible lower bound above give G-g<=D*h^2 at the conditional beta maximum, with D=Cupper+Clower. Consequently its q and nu coordinates belong to a smaller explicit rectangle:

    |q-k| <= 2*Q*sqrt(D)*H,
    |nu-q/mu| <= nuHi*sqrt(2*D/qLo)*H.

Intersect these bounds with the existing conditional compactness rectangle. They include all conditional maximizers for every 0<h<=H, not only a numerical stationary proposal. Independently padded signs enclose k through the monotone gamma shape score.

Write C0(q)=C(q,q/mu). Bounds on its derivative and on the rate derivative are

    Lq >= (abs(mean(v^2)-mu^2)+mu^2/qMin^2)/24,
    Lnu >= max_[qLo,qHi] abs(q-q^3)/(12*nuPathLo^3).

Before forming C4 the implementation also contracts the uniform rectangle using the centered correction scores. For every z>0 the same integral gives -1/(12*z^3)<=E'(z)<=0, where E(z)=psi(z+1/2)-log(z). If d=nuLo-qHi*H/2>0 and r=qHi^2*H^2/(4*nuLo^2)<1, then

    abs(partial_nu(F-g)) <= Bnu*h^2,
    Bnu=qHi/(12*d^3)+qHi^3/(12*nuLo^3*(1-r)),
    abs(partial_q(F-g)) <= Bq*h^2,
    Bq=qHi^2/(8*nuLo^2*(1-r))+1/(24*d^2)+mean(v^2)/24.

The first formula integrates E' and the positive centered reciprocal-log term. The second uses the exact endpoint-average derivative of Ac in q, plus T. At the conditional maximum, both centered scores vanish. Hence

    abs(nu-q/mu) <= nuHi*Bnu*H^2/mu,
    abs(q-k) <= 2*Qgamma^2*(Bq+nuHi*Bnu/(mu*nuPathLo))*H^2.

The second implication uses the gamma shape score and the logarithmic rate displacement. Intersect the rectangle with these bounds, with independently padded statistic endpoints, and repeat within the solver budget. Every intersection still encloses all conditional maxima throughout 0<h<=H. Unlike the initial objective-gap enclosure, these score bounds contract at order H^2. They do not assume a numerical proposal is a root. The compact cover starts at H itself, since the signed neighborhood already includes every smaller power.

The path rectangle includes the entire segment between nu and q/mu. Thus C(q,nu)<=C0(k)+Lq*abs(q-k)+Lnu*abs(nu-q/mu). Maximizing the two linear terms against the two negative quadratic gamma gaps yields the implemented uniform bound

    P(h) <= G+c2Upper*h^2+C4*h^4,
    C4 = Qpath^2*Lq^2+nuPathHi^2*Lnu^2/(2*qLo)+R.

C0 is convex, so a padded maximum at the two endpoints of the gamma shape enclosure bounds c2. All statistic endpoints and path constants receive practical floating-point allowances. If c2Upper+C4*H^2 is strictly negative, the entire neighborhood 0<h<=H is below the exact gamma maximum. A quadratic-zero seam cannot pass this test; it remains uncertified.

### Complete gamma-face comparison

The fitter also validates the gamma coefficient enclosure against the requested relative tolerance, excludes the entire large-power tail below a gamma lower witness, and covers every compact cell between the neighborhoods. It reuses the finite certificate's uniform shape/score bounds to integrate endpoint profile upper bounds over each cell, plus the power certificate's q-eliminated whole-cell envelope where shapes become enormous. A cell is removed only by a padded upper/lower likelihood comparison. The signed bound also prunes covered small-power cells. An empty queue and both neighborhoods set allfree_certified=true and provide finite global supremum bounds.

The certificate evaluation ledger includes the legacy proposal work, neighborhood trials, profile points, and compact cells; it is 128*min(maxit,100000). Inner solves and gamma shape bisection have min(maxit,100000) iterations. Statistic and gamma-score comparisons use 1024 or 4096 epsilon; signed remainder/path and cell comparisons use 8192 epsilon with the stated uncancelled operands. These are conditional exact-arithmetic certificates with practical margins, not outward-rounded intervals.

An unfinished gamma certificate retains the original proper UnitGammaBoundary payload and marks it uncertified. A validated finite witness above the gamma upper bound instead disproves that selection and stops with both witnesses. Finite and moving-power partial coverage follow the same successful-legacy fallback rule. The remaining positive-certification obligations concern quadratic-zero seams, unresolved arithmetic, and coverage that cannot close within the supplied work allowance; they do not justify refusing a successful legacy answer merely for lacking certification.

### Detecting an impossible fixed-margin comparison early

The compact verifier uses the fixed padded gamma lower target L and removes a
cell only if U+margin<L. If a validated feasible witness at a point has lower
bound B with B+margin>=L, no valid upper bound for a cell containing that point
can satisfy this test: U>=sup(cell)>=B. Subdivision cannot resolve that
particular comparison at the chosen margin. The verifier immediately retains
the legacy face uncertified instead of spending the ledger on that cell.
A witness above the gamma *upper* bound takes precedence and disproves the
legacy selection. This exit adds no work cap or acceptance shortcut and cannot
remove a certificate which this fixed-threshold cover could have completed.
