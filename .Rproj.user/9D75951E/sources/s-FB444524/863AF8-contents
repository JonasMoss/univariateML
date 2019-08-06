


m = median(x)
mad = median(abs(x - m))

values = nlm(f = function(p) -sum(dcauchy(x, p[1], exp(p[2]), log = TRUE)),
             p = c(median(x), log(mad)))$estimate
values = c(values[1], exp(values[2]))



k = 10
x = rcauchy(2*k + 1, 2, 1)
x = sort(x)

m = median(x)
mad = median(abs(x - m))

values = nlm(f = function(p) -sum(dcauchy(x, p[1], exp(p[2]), log = TRUE)),
             p = c(median(x), log(mad)))$estimate
values = c(values[1], exp(values[2]))



f = function(theta, sigma = 1){
  sapply(theta, function(theta) 2*sum((x - theta)/(sigma^2 + (x - theta)^2)))
}

g = function(theta){
  sapply(theta, function(theta) sum(2*(x - theta)/(1 + (x - theta)^2)))
}

y = seq(min(x), max(x) + 0.05, by = 0.05)
plot(y, f(y, sigma = 0.1), type = "l")
lines(y, f(y, sigma = 2), type = "l")
lines(y, f(y, sigma = 1), type = "l")
points(x, f(x, 0.1), pch = 20)
abline(h = 0)
abline(v = median(x))


j = 1
f(x[(k - j):(k + j)])

# Hyp: If the median is negative, there is always a positive to its right.
# If it is positive, there is always a negative to its left.

