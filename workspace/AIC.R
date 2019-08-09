AIC(mlbetapr(airquality$Wind),
    mlexp(airquality$Wind),
    mlinvgamma(airquality$Wind),
    mlgamma(airquality$Wind),
    mllnorm(airquality$Wind),
    mlrayleigh(airquality$Wind),
    mlwald(airquality$Wind),
    mlweibull(airquality$Wind))