---
id: snorm
name: Skew Normal
support: {type: real, lower: -inf, upper: inf, closed: [false, false]}
params:
  - {name: mean, role: location, domain: "(-inf, inf)", default: 0.0}
  - {name: sd, role: scale, domain: "(0, inf)", default: 1.0}
  - {name: xi, role: skew, domain: "(0, inf)", default: 3.0}
r:
  density: dsnorm
  sampler: rsnorm
  control: {}
log_density: log(sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)) - log(sd) + log(2/(xi+1/xi)) - log(2*pi)/2 - Piecewise(((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2/xi**2, (sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))>0), ((sqrt((1-2/pi)*(xi**2+xi**(-2))+4/pi-1)*(x-mean)/sd + sqrt(2/pi)*(xi-1/xi))**2*xi**2, True))/2
limits:
  - model: hnorm
    when: '$\xi \to \infty$'
    status: 'RightHalfNormalBoundary'
  - name: 'reflected translated half-normal'
    when: '$\xi \to 0$'
    status: 'LeftHalfNormalBoundary'
references:
  - "Fernandez, C. and Steel, M. F. J. (1998). On Bayesian modeling of fat tails and skewness. Journal of the American Statistical Association, 93(441), 359–371."
examples: |
  mlsnorm(c(-2, -1, 0, 1, 2))
---

The Fernandez--Steel skew-normal distribution introduces skewness by giving the two sides of a normal density different scales.

## Details

This is the two-piece normal family, rather than the Azzalini skew-normal family. The coefficients `mean` and `sd` are its mean and standard deviation; `xi = 1` gives the normal distribution. The translated half-normal boundary has its mode at the sample minimum or maximum.

Use `mlhnorm(x)` for the right boundary and `mlhnorm(-x)` for the left boundary, reflecting the fitted half-normal back at the sample maximum.
