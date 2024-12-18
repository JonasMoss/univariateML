x <- extraDistr::rgumbel(10000, 2, 5)
x_bar <- mean(x)
x <- x - x_bar

library(profvis)
profvis({
    x <- extraDistr::rgumbel(1000000, 2, 5)
    mlgumbel_(x)
})