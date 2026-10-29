# Gumbel dgumbel parameter mapping

## Status: PARAMETERS MATCH

Both `extraDistr::dgumbel(x, mu, sigma)` and `actuar::dgumbel(x, alpha, scale)` use identical parameterizations with location and scale parameters.

## Mapping

- `extraDistr::dgumbel(x, mu, sigma)` is equivalent to `actuar::dgumbel(x, mu, sigma)`
- Location parameter: `mu` (extraDistr) = `alpha` (actuar)
- Scale parameter: `sigma` (extraDistr) = `scale` (actuar)

**No transformation needed.** The mlgumbel model in univariateML can use either package's dgumbel function without any parameter re-mapping.

## Verification

Tested on a grid of values:
- x: -5, -1, 0, 1, 5, 10
- mu/alpha: 0, 1, 5
- sigma/scale: 0.5, 1, 2
- Both scalar and log=TRUE evaluation

All tests passed with exact numerical agreement.

## Conclusion

The location/scale mapping between the two packages is direct: both use $d(x) = \frac{1}{\sigma} \exp\left(-\frac{x-\mu}{\sigma} - \exp\left(-\frac{x-\mu}{\sigma}\right)\right)$. No adjustment is required in the mlgumbel reference configuration.
