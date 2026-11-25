# Keep bootstrap finite differences inside marginal support

Hi Thomas,

The upcoming univariateML release adds GEV. In the `svine_bootstrap_models()` example, the default margin selection in `svine(..., family_set = "parametric")` now picks GEV for the first return series. The nested finite differences can move the fitted endpoint past the largest observation. That gives a non-finite Hessian and propagates into the bootstrap coefficients.

This patch scales the steps down for small coefficients and retries with smaller steps when a stencil gives non-finite values. It reports which margin failed if no finite stencil is found and checks the one-step update before building a bootstrap model. The regression explicitly selects a GEV margin so it does not depend on which family wins by AIC.

I ran the svines test suite and the documented bootstrap example against CRAN univariateML 1.5.0 and the upcoming build. Both passed, with the GEV regression skipped on 1.5.0 because that fitter is unavailable. Happy to adjust the step rule or the diagnostic.
