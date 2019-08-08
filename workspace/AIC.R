AIC(mlbetapr(airquality$Temp),
    mlexp(airquality$Temp),
    mlinvgamma(airquality$Temp),
    mlgamma(airquality$Temp),
    mllnorm(airquality$Temp),
    mlrayleigh(airquality$Temp),
    mlwald(airquality$Temp),
    mlweibull(airquality$Temp))