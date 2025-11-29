
# create a grid of mu and sigma2
mu_vals <- seq(-2, 0, length.out = 100)
sigma2_vals <- seq(0.1, 2, length.out = 100)

# expand.grid makes all combos
grid <- expand.grid(mu = mu_vals, sigma2 = sigma2_vals)

# apply your function to the grid
grid$a_est <- mapply(kumar_lmom, grid$mu, grid$sigma2)

# load plotting library
library(ggplot2)

# plot that baby
ggplot(grid, aes(x = mu, y = sigma2, fill = a_est)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Estimated 'a' from kumar_lmom(mu, sigma2)",
       x = "log-mean (mu)", y = "log-variance (sigma2)",
       fill = "a estimate")



library(plotly)

# same grid setup
mu_vals <- seq(-10, 0.1, length.out = 50)
sigma2_vals <- seq(0.1, 10, length.out = 50)

# meshgrid like setup
mu_grid <- matrix(rep(mu_vals, each = length(sigma2_vals)), nrow = length(sigma2_vals))
sigma2_grid <- matrix(rep(sigma2_vals, times = length(mu_vals)), nrow = length(sigma2_vals))


a_est_grid <- (outer(
  mu_vals,
  sigma2_vals,
  Vectorize(function(mu, sigma2) kumar_lmom(mu, sigma2))
))

mu_vals <- log(-mu_vals)
sigma2_vals <- log(sigma2_vals)


# plotly surface plot
plot_ly(x = ~mu_vals, y = ~sigma2_vals, z = ~a_est_grid) %>%
  add_surface() %>%
  layout(
    title = "Surface plot of kumar_lmom(mu, sigma2)",
    scene = list(
      xaxis = list(title = "mu (log-mean)"),
      yaxis = list(title = "sigma2 (log-variance)"),
      zaxis = list(title = "a estimate")
    )
  )


mus <- seq(-30, -0.001, by = 0.01)
plot(-mus, sapply(mus, \(mu) kumar_lmom(mu, 9)), log = "x")
