context("utility")

x <- list(
  a = 5,
  b = 0,
  c = "a",
  d = NULL
)

y <- list(
  a = 3,
  b = 7,
  f = NA
)

expect_equal(listmerge(x, y, type = "merge")$b, 7)
expect_null(listmerge(x, y, type = "template")$f, NULL)
expect_equal(listmerge(x, NULL, type = "merge"), x)
expect_equal(listmerge(x, list(h = "b"), type = "merge")$h, "b")
