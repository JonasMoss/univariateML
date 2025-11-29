abalone <- read.csv("abalone.csv", header = FALSE)
colnames(abalone) <- c(
    "sex", "length", "diameter", "height", "whole_weight",
    "shucked_weight", "viscera_weight", "shell_weight",
    "rings"
)
abalone <- tibble::as_tibble(abalone)
usethis::use_data(abalone)
