i <- 1
res <- t(sapply(seq(10000), function(i) {
  set.seed(i)
  x <- actuar::rburr(100, runif(1, 0.5, 100), runif(1, 0.5, 100))
  e <- mean(log(x))
  s <- mean(log(x)^2)
  c(e, s, unname(c(mlburr(x)[2])))
  # print(i)
}))

plot(-res[, 1], log(res[, 3]))
plot(log(res[, 2]), log(res[, 3]))

model <- lm(I(log(res[, 3])) ~ I(log1p(res[, 1])) + I(log(res[, 2])))
model <- lm(I(log(res[, 3])) ~ I(log1p(res[, 1])))
model <- lm(I(log(res[, 3])) ~ I(log(res[, 2])))
summary(model)


model <- lm(I(res[, 3]) ~ I(res[, 1]) + I(res[, 2]) + I(res[, 1]^2) + I(res[, 2]^2))
summary(model)




model <- lm(I(res[, 3]) ~ I(res[, 1]) + I(res[, 2]) + I(res[, 1]^2) + I(res[, 2]^2))
summary(model)

model <- lm(I(res[, 3]) ~ I(1 / (res[, 2])^(-1 / 3)))
summary(model)

plot(res[, 2], res[, 3])
points(res[, 2], predict(model), col = "red")
