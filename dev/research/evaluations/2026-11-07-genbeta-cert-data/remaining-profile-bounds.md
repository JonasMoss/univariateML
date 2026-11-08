# Bounds for the remaining profiles with free p

These are exact-arithmetic results for the remaining free-p profiles. They
are not wired into the fitter. `check-remaining-bounds.py` independently
checks the formulas at 70 decimal digits. They reduce the remaining work;
they do not certify the retained mesh or its selected face statuses.

Use the normalization from `bounds.md`: mean(v)=1, h=a*mean(Y),
r(h)=-mean(log(1-exp(-h*v))), g=mean(log(v)), vmin=min(v), and
delta=1-vmin>0 for nonconstant data. Log likelihoods below are per observation,
with the common Jacobian and -log(mean(Y)) removed.

## Conditional beta-shape bounds

At the two-shape conditional MLE, write s=h and r=r(h). Both scores vanish:

    psi(p+q)-psi(p)=s,   psi(p+q)-psi(q)=r.

The statistic contrasts

    cp=s+log(1-exp(-r)),   cq=r+log(1-exp(-s))

are strictly positive for nonconstant transformed data. Let
alpha(t)=log(t)-psi(t). The standard integral inequalities give
0<alpha(t)<1/t, with alpha decreasing. Put dp=alpha(p)-alpha(p+q)>0
and dq=alpha(q)-alpha(p+q)>0. Then

    cq = dq + log(1+(p/q)*(1-exp(-dp))) < 2/q,

because dq<1/q and log(1+(p/q)*(1-exp(-dp))) <= (p/q)*dp < 1/q.
The symmetric argument gives cp<2/p. Thus

    p < 2/cp,    q < 2/cq.

Also trigamma(t)>1/t^2 yields s>=q/[p*(p+q)]. The other score and
trigamma(t)>1/t imply p/q<=exp(r)-1. Combining them, and symmetrically,

    p >= exp(-r)/s,    q >= exp(-s)/r.

These are explicit conditional shape rectangles. They are not enclosures of
a root to a requested coefficient tolerance.

## Uniform small-power upper bound, both shapes free

Let c=-g=log(mean(Y))-mean(log(Y))>0. The elementary bounds on r give

    cq = r+log(1-exp(-h)) >= c-h.

For 0<h<=c/2, every conditional beta-shape maximizer has q<=Q=4/c.
This excludes unbounded q in the small-power conditional maximum.

Compare the density with a gamma density in v of shape q and rate
lambda=p*h. Their per-observation log-likelihood difference is

    A(p,q) + (q-1)*mean(log((1-exp(-h*v))/(h*v))),
    A(p,q)=log Gamma(p+q)-log Gamma(p)-q*log(p).

Jensen's inequality gives psi(t)<=log(t), hence

    A(p,q)<=q*log(1+q/p)<=q*h.

The last inequality uses the conditional p score and trigamma(t)>=1/t.
The second difference is at most max(0,1-q)*h. Therefore, with G the
unrestricted gamma maximum on v,

    P(h) <= G + max(1,Q)*h,    0<h<=c/2.

The bound is uniform over both optimized beta shapes, including p tending
to infinity. With q held, the same argument gives

    P_q(h) <= G_q + max(1,q)*h,

where G_q is the gamma maximum retaining that held shape. These unsigned
bounds close a small-power tail when a finite or other-face lower witness
is strictly above the relevant gamma face. They cannot prove a winning
gamma face at equality.

## Uniform moving-endpoint upper bound, both shapes free

Assume h*vmin>=log(2), h*delta>=log(4*n), and
h*delta-log(n)-1>=2. Then r<=1 and

    cp>=h*delta-log(n)-r,
    p<=B(h)=2/(h*delta-log(n)-1)<=1.

Also log(1-exp(-h))>=-2*exp(-h). Since
r>=exp(-h*vmin)/n>=4*exp(-h), cq>=r/2, so

    q<=4/r<=4*n*exp(h*vmin),
    log(q+p)<=h*vmin+log(4*n+1).

Writing lambda=p*h, use log Gamma(q+p)-log Gamma(q)<=p*log(q+p)
and -log Gamma(1+p)<=EulerGamma*p. The factor with exponent q-1
contributes at most r, irrespective of the sign of q-1. Thus

    P(h) <= log(lambda)-lambda*delta
            +p*(log(4*n+1)+EulerGamma)+r
         <= W+B(h)*(log(4*n+1)+EulerGamma)+R(h*vmin),
    W=-log(delta)-1,
    R(z)=-log(1-exp(-z)).

The last two terms decrease to zero. W is the exponential fit in v-vmin,
which maps to the power face with endpoint max(x), not the held endpoint b.
This is a uniform, quantitative bound including the moving endpoint and
joint p-to-zero/q-to-infinity paths at the conditional maximum.

For held q, a simpler bound follows from
s=psi(p+q)-psi(p)<=q/p+1/p, so p<=(q+1)/h. Put
Aq=max(0,psi(q)-psi(1)). Monotonicity of the digamma difference gives

    log Gamma(p+q)-log Gamma(q)-log Gamma(1+p)<=p*Aq,

and therefore the fixed-endpoint power face E=-1 satisfies

    P_q(h)<=E+Aq*(q+1)/h+max(0,1-q)*R(h*vmin).

## A signed moving-endpoint neighborhood, both shapes free

A stronger inequality can certify that the whole sufficiently large-power
neighborhood is below W. Maximize the gamma-ratio upper bound over q:
for 0<r<1,

    sup_q [p*log(q+p)-q*r] = p*log(p/r)-p+p*r.

It follows that, with C=log(n)+EulerGamma,

    P(h)<=W+r+p*(log(p)+C).

The conditional shape bounds also give p>=exp(-r)/h>=exp(-1)/h.
When B(h)<exp(-C-1), the function p*(log(p)+C) is decreasing throughout
the retained p interval. Hence

    P(h)<=W+R(h*vmin)+(exp(-1)/h)*(C-1-log(h)).

Choose H satisfying the unsigned-bound thresholds above, H>=1/vmin,
B(H)<exp(-C-1), log(H)-C+1>0, and

    2*exp(1)*H*exp(-H*vmin) < log(H)-C+1.

Then P(h)<W for all h>=H. To see the last implication, R(h*vmin)<=
2*exp(-h*vmin), and the ratio
2*exp(1)*h*exp(-h*vmin)/(log(h)-C+1) decreases for h>=H:
its logarithmic derivative is at most 1/h-vmin<=0.
The remaining inequalities persist as h increases. Thus there is an
explicit finite cutoff for a winning moving-power neighborhood.

## A signed fixed-endpoint power neighborhood when q<1 is held

Let Bq=psi(1)-psi(q)>0 and bq=psi(2)-psi(1+q)>0. For h>=1,
recurrence and monotonicity give

    1/(h+Bq)<=p<=1/h,
    log Gamma(p+q)-log Gamma(q)-log Gamma(1+p)<=-bq*p.

Consequently

    P_q(h)<=-1-bq/(h+Bq)+(1-q)*R(h*vmin).

For H>=max(1,1/vmin), the sufficient condition
2*(1-q)*(H+Bq)*exp(-H*vmin)<bq makes the entire h>=H neighborhood
strictly lower than the fixed-endpoint power face. The ratio decreases
because (h+Bq)*exp(-h*vmin) decreases there.

## Remaining implementation and proof obligations

These bounds do not supply a certified conditional two-shape beta solver,
a complete compact power search for free p, or coefficient inclusion for
that search. A candidate inner likelihood is a lower bound, not an upper
bound on the beta conjugate. The compact triangle construction in
`bounds.md` needs certified conjugate upper values before it can be used.

A winning small-power gamma face still needs a signed uniform neighborhood
bound. The known profiled quadratic coefficient can be positive, negative,
or zero; the unsigned O(h) bound above does not settle its sign or the
zero-coefficient case. Nearly tied gamma and moving-power faces also require
explicit comparison margins or a correct tie/numerical stop. These are the
remaining gaps; merely increasing the legacy mesh cannot fill them.
