
#
#
# x = extraDistr::rlomax(10, 1, 7)
# x = extraDistr::rlomax(100, 10, 3)
# lambdas = seq(0, 100, by = 0.01)
#
#
# f1 = function(lambda, x) {
#
#   sapply(lambda, function(lambda) {
#     S = mean(log(1 + lambda*x))
#     -log(lambda) + log(S) + S + 1
#   })
#
# }
#
#
# plot(lambdas, f1(lambdas, tiny_data))
# plot(lambdas, f1(lambdas, small_data))
# plot(lambdas, f1(lambdas, medium_data))
# plot(lambdas, f1(lambdas, large_data))
#
#
# f = Vectorize(function(p, q) mean(extraDistr::dlomax(tiny_data, p, q, log = TRUE)))
# x = seq(0, 1, by = 0.1)
# y = x
# z = outer(x, y, FUN = f)
# contour(x, y, z)
#
# p <- plot_ly(
#   x = x,
#   y = y,
#   z = z,
#   type = "contour"
# )
#
# p



# f1 = function(lambda) {
#
#   sapply(lambda, function(lambda) {
#     S = mean(log(1 + lambda*x))
#     -log(lambda) + log(S) + S + 1
#   })
#
# }
#
# values = optimize(f = f1, interval = c(0, 1000))
# object = c(lambda = values$minimum,
#            kappa = 1/mean(log(1 + values$minimum*x)))
# class(object) = c("univariateML")
# attr(object, "logLik") = -n*values$objective
# attr(object, "model") = "Lomax"
# object
