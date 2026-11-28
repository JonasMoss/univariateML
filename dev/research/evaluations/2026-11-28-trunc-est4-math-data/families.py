"""Family-specific support, contact and coordinate rules for the regime sections."""

POSITIVE = {
    "betapr": (r"z(x)=x/(S+x)", r"The fixed-scale beta test uses the images of the cuts in $z$. Zero-shape faces require cutting away the corresponding singular endpoint."),
    "burr": (r"z(x)=\log x", r"In the GB2 kernel $p=1$, $q=\mathrm{shape1}$ and $d=\mathrm{shape2}$. At fixed scale and power use the beta coordinate $x^d/(S^d+x^d)$ and the free upper beta tilt. Keep only the held-compatible Burr faces."),
    "gb2": (r"z(x)=\log x", r"In the beta-prime transform $p=\mathrm{shape3}$, $q=\mathrm{shape1}$ and $d=\mathrm{shape2}$. At fixed power and scale the window maps to beta cuts via $x^d/(S^d+x^d)$. Varying-power comparisons include reviewed nonzero log slopes at finite power and remote scale where reachable."),
    "gengamma": (r"z(x)=\log x", r"The Prentice density is strictly positive at every finite point of the log scale, including at $Q=0$. At fixed nonzero $Q,\sigma$ apply the gamma tilt in $u=e^{Qz/\sigma}$, reversing the cuts for $Q<0$. At $Q=0$ and held $\sigma$ the location tilt uses $z$ itself."),
    "trgamma": (r"z(x)=x^d", r"With $k=\mathrm{shape1}$ and $d=\mathrm{shape2}$ the gamma carrier at held power is $z^{k-1}e^{-S^{-d}z}$. The zero-shape face needs positive lower cut; the zero-rate face needs finite upper cut. With free power use the log-coordinate comparisons."),
    "invtrgamma": (r"z(x)=x^{-d}", r"With $d=\mathrm{shape2}$ held, the gamma coordinate decreases, with shape $\mathrm{shape1}$ and rate $S^d$. The zero-shape face needs $z$ at the physical upper cut positive, and the zero-rate face needs $z$ at the physical lower cut finite."),
    "invburr": (r"z(x)=1/x", r"Reciprocation gives Burr at scale $1/S$, held exactly when $S$ is held. Tail and onset exchange, and the Jacobian $1/x^2$ is independent of the coefficients."),
    "invparalogis": (r"z(x)=-\log x", r"Reciprocation gives paralogistic with $p=1$ and $q=d=\mathrm{shape}$. The log-uniform test and the large-shape exception at held scale use the reflected log interval."),
    "paralogis": (r"z(x)=\log x", r"The GB2 specialization $p=1,q=d=\mathrm{shape}$ changes both these coordinates together. At held scale a constant equal to it has large-shape divergence if the retained log window starts at the log scale, and its height tends to zero if the interval has a left neighborhood."),
    "invpareto": (r"z(x)=x/(S+x)", r"The forward GB2 kernel has $d=q=1$ and $p=\mathrm{shape}$. At held $S$ use the positive tilt on $\log z$ and carrier $1/z$, integrable exactly when the lower transformed cut is positive. The reciprocal proof calls $1-z$ its beta coordinate and writes the same statistic as its log-survival coordinate."),
    "naka": (r"z(x)=x^2", r"The squared law is gamma with shape $k$ and rate $k/\Omega$ for $\Omega=\mathrm{scale}$. Held $\Omega$ ties these coordinates and gives statistic $\log z-z/\Omega$ with peak at the projected $\Omega$; held shape gives statistic $-z$."),
}

LOWER_ONLY = {
    "fatigue": (r"\mu", r"The Birnbaum–Saunders coordinate $\sqrt{(x-\mu)/\beta}-\sqrt{\beta/(x-\mu)}$ has positive derivative for positive excess and sends the threshold to $-\infty$. Density at the threshold is zero for every $\alpha,\beta$. A positive fit requires $\mu<m$."),
    "fpareto": (r"\mathrm{min}", r"The lower power is $p d_0-1$, with $p=\mathrm{shape3}$ and $d_0=\mathrm{shape2}$, independent of $q=\mathrm{shape1}$. At contact the density is infinite, positive finite, or zero as $p d_0$ is below, equal to, or above one. For strictly positive excesses use the shifted GB2 kernel."),
    "pareto3": (r"\mathrm{min}", r"The excess kernel is $y^{p-1}(1+(y/S)^p)^{-2}$ with $p=\mathrm{shape}$. Its threshold factor is singular for $p<1$, positive finite at $p=1$, and zero for $p>1$. The fixed-pole reduction at $p=1$ preserves the kernel in physical coordinates as the threshold moves."),
    "pareto4": (r"\mathrm{min}", r"The excess survival is $(1+(y/S)^p)^{-h}$ with $p=\mathrm{shape2}$ and $h=\mathrm{shape1}$. The contact power is $p-1$. With only $h$ free use the decreasing tilt on $T=\log(1+(y/S)^p)$; finite upper transformed cut permits the excluded zero-tail-shape carrier $T'(x)$."),
    "expweibull": ("0", r"The inside density at zero is infinite, $1/S$, or zero as shape times exponent is below, equal to, or above one. The regular comparisons are for positive data. At held shape and scale, $T=\log(1-e^{-(x/S)^p})$ sends zero to $-\infty$, so a retained lower endpoint zero removes the finite zero-exponent mean threshold."),
    "pgw": ("0", r"The zero density value is infinite, $\mathrm{exponent}/S$, or zero as shape is below, equal to, or above one. For positive data the small-exponent kernel at held scale and shape is the derivative of $\log(1+(x/S)^p)$, nonintegrable on an upper ray."),
    "foldnorm": ("0", r"The sum of the two Gaussian densities is positive at zero. At held $\mu=0$ the statistic is $-x^2$; at held positive location the small-scale collapse projects $\mu$ onto the retained interval."),
    "sgomp": ("0", r"The density at zero is $b e^{-\eta}>0$. Its CDF $(1-e^{-bx})e^{-\eta e^{-bx}}$ increases strictly. The all-free onset face $(1+tx)e^{tx}$ includes uniform at $t=0$ and is nonintegrable on a right ray."),
}

MOVING = {
    "genbeta": ("0", r"S=\mathrm{scale}", r"Use the increasing beta coordinate $z=(x/S)^d$ with $d=\mathrm{shape3}$. The upper shape $q=\mathrm{shape2}$ has infinite, positive finite, or zero density at $S$ as $q$ is below, equal to, or above one. At $q=1$ the power exponent is $pd-1$ for $p=\mathrm{shape1}$, and only the product may be identified."),
    "gev": (r"-\infty\ \text{or}\ \mu-\sigma/\xi", r"\infty\ \text{or}\ \mu-\sigma/\xi", r"The finite end is upper for $\xi<0$ and lower for $\xi>0$. On the fitted branch only upper contact at $\xi=-1$ is positive finite. The lower contact has density zero. A held $\xi<-1$, if allowed, gives an upper singularity. The positive-shape minimum collapse at $\mu=m$ keeps the occupied minimum a distance $\sigma/\xi$ above the lower end."),
    "gpd": (r"\mu", r"\mu-\sigma/\xi\ \text{if }\xi<0,\ \infty\ \text{otherwise}", r"The threshold density is $1/\sigma$. The upper end has positive finite density at $\xi=-1$, zero for $-1<\xi<0$ and infinite at held $\xi<-1$ if allowed. At $\xi=-1$ the conditional likelihood is the reciprocal retained width to the power $n$."),
    "nsbeta": (r"\mathrm{min}", r"\mathrm{max}", r"The kernel is $(x-d)^{p-1}(e-x)^{q-1}$. Each occupied endpoint needs its own shape equal to one for positive finite density. A subunit shape gives a singular factor and a superunit shape gives zero. One held endpoint at contact leaves the other endpoint subject to its own hold."),
    "triang": ("a", "b", r"The affine height is positive in $(a,b)$ and at an endpoint only when $c$ equals that endpoint. Keep equality strata $a=c$ and $b=c$ in the height projection. A vanished piece contributes area zero, while an occupied endpoint at the mode keeps its inside value."),
}


def geometry(family, design, lower, upper):
    """Give the actual cut substitution and the source of strictness."""
    low = design == "lower-estimated"
    up = design == "upper-estimated"
    if family in POSITIVE:
        transform, mechanism = POSITIVE[family]
        lo = r"\max(0,A)" if up else "m"
        ordered = (upper, lo) if family in {"invtrgamma", "invburr", "invparalogis"} else (lo, upper)
        return (
            rf"The retained interior is $({lo},{upper})$, with positive observations at the cuts included. "
            rf"Compare in ${transform}$ with ordered cuts $z({ordered[0]})$ and $z({ordered[1]})$ when that transform is fixed by the holds. "
            rf"{mechanism} Every estimated cut removes positive mass from a strictly looser cut, because the observation at the extreme is strictly inside the positive parent support. Zero is outside the parent domain."
        )
    if family in LOWER_ONLY:
        d, mechanism = LOWER_ONLY[family]
        endpoints = {
            "lower-estimated": rf"At compatible coefficients retain $[m,B]$. Tightening removes the strip $(\max(\ell,{d}),m)$ from a proposed lower cut $\ell$.",
            "upper-estimated": rf"Retain $[\max(A,{d}),M]$. Any upward loosening adds positive mass because the parent is unbounded above.",
            "both-estimated": rf"For a nonconstant sample retain $[m,M]$. An upper loosening adds positive mass. A lower loosening adds positive mass exactly when ${d}<m$.",
        }[design]
        null = (
            rf"For a constant at $B=m$, left expansion needs ${d}<m$."
            if low else (
                rf"For a constant with $A<m$, contact at ${d}=m$ leaves a null tight event. Test the inside density and loosen upward."
                if up else rf"For a constant test expansion to the right with ${d}\le m$ and the stated inside value."
            )
        )
        return f"{endpoints} {mechanism} {null}"
    d, e, mechanism = MOVING[family]
    endpoints = {
        "lower-estimated": r"Retain $[m,\min(B,e)]$. Lower cuts below $m$ tie at $d=m$. At a constant with $e=m$, even $B>m$ leaves a null tight event and only left expansion can restore mass.",
        "upper-estimated": r"Retain $[\max(A,d),M]$. Upper cuts above $M$ tie at $e=M$. At a constant with $d=M$, even $A<M$ leaves a null tight event and only right expansion can restore mass.",
        "both-estimated": r"For nonconstants retain $[m,M]$. The removed left or right strip is empty precisely at $d=m$ or $e=M$ respectively. For constants test the allowed expansion on either side within the same support.",
    }[design]
    return rf"Use $(d,e)=({d},{e})$ for the support ends at the proposed coefficients. {endpoints} {mechanism}"
