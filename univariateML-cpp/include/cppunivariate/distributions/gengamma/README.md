---
id: gengamma
name: Prentice generalized gamma
stage: dpqr
support: {type: real, lower: 0, upper: inf, closed: [false, false]}
params:
- {name: mu, symbol: '\mu', role: location, domain: '(-inf, inf)', simulate: 0}
- {name: sigma, symbol: '\sigma', role: scale, domain: '(0, inf)', simulate: 1}
- {name: Q, symbol: 'Q', role: shape, domain: '(-inf, inf)', simulate: 0}
r:
  density: dgengamma
  sampler: rgengamma
  control: {}
log_density: 'Piecewise((-log(sigma)-log(x)-log(2*pi)/2-((log(x)-mu)/sigma)**2/2, Eq(Q,0)), (log(Abs(Q))-log(sigma)-log(x)+(log(Q**(-2))+Q*(log(x)-mu)/sigma-exp(Q*(log(x)-mu)/sigma))/Q**2-loggamma(Q**(-2)), True))'
relations: {}
---

The Prentice generalized gamma family includes the lognormal at $Q=0$.
