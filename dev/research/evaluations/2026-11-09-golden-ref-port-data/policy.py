"""Documented causes for measured default-fit accuracy on the frozen samples."""
causes={
 'betapr':'profile/Newton gradtol=1e-6; score stopping and likelihood rounding do not bound coefficient error',
 'bhatt':'simplex reltol=1e-8; coefficient error depends on curvature and floating-point comparisons',
 'bnbinom':'finite-difference BFGS gradtol=1e-8 and profile stopping',
 'burr':'finite-difference BFGS gradtol=1e-8 and objective stop',
 'invburr':'finite-difference BFGS gradtol=1e-8 and objective stop',
 'dgamma':'profile simplex width reltol=1e-8; likelihood rounding limits coefficient resolution',
 'emg':'certified profile with default reltol=1e-8; coefficient resolution and floating-point comparisons',
 'fatigue':'absolute parameter-step abstol=1.220703e-4 inherited from CRAN',
 'foldnorm':'scalar profile refinement reltol=1e-10; likelihood rounding limits coefficient resolution',
 'genbeta':'profile score stop gradtol=1e-7; shallow profile amplifies coefficient error',
 'gumbel':'Newton relative-step reltol=1.220703e-4 inherited from CRAN',
 'ht':'profile objective/scale stops; small likelihood error does not bound nu error',
 'kumar':'Newton relative-step reltol=1.220703e-4 inherited from CRAN',
 'lomax':'Newton relative-step reltol=1.220703e-4 inherited from CRAN',
 'pareto3':'delegated damped logistic Newton step reltol=1e-10; likelihood rounding can shrink the accepted step',
 'pig':'scalar profile reltol=1e-9; curvature amplifies shape error',
 'sgomp':'scalar profile reltol=1e-10; curvature amplifies coefficient error',
 'skellam':'scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution',
 'slash':'scalar profile width reltol=1e-8 and likelihood rounding; near-zero location amplifies relative error',
 'sn':'scalar profile refinement reltol=1e-9; likelihood rounding limits coefficient resolution',
 'st':'all-free BFGS score stop at reltol=1e-6; global/face certificate does not impose 1e-10 coefficient accuracy',
 'tlambda':'profile likelihood stop reltol=1e-7',
 'weibull':'Newton relative-step reltol=1.220703e-4 inherited from CRAN',
 'zinb':'negative-binomial scalar profile width reltol=1e-10 and likelihood rounding',
 'zmnbinom':'delegated positive negative-binomial profile reltol=1e-10',
 'ztnbinom':'positive negative-binomial profile reltol=1e-10',
 'cmb':'Newton moment-score stop at reltol=1e-10; inverse curvature amplifies coefficient error',
 'gb2':'all-free beta-shape profile score stop at gradtol=1e-7; profile curvature amplifies coefficient error',
 'invtrgamma':'delegated generalized-gamma profile reltol=1e-10',
}
for id in ('std','ged','sstd','sged','paralogis','invparalogis','lst'):
 causes[id]='finite-difference BFGS gradtol=1.220703e-4 inherited from CRAN; curvature amplifies coefficient error'

def explanation(id, tolerance):
    if tolerance <= 1e-10:
        return 'binary64 rounding and the default analytic/iterative solve'
    if id not in causes:
        raise ValueError(('unexplained default-fit error',id,tolerance))
    return causes[id]
