# Extract current univariateML metadata to metadata/models.json
# Run via: Rscript scripts/extract_models_metadata.R

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to run this extractor.")
}

suppressMessages(devtools::load_all("."))

meta <- get("univariateML_metadata", envir = asNamespace("univariateML"))

to_sampler <- function(density) {
  parts <- strsplit(density, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    parts[2] <- sub("^d", "r", parts[2])
    paste0(parts[1], "::", parts[2])
  } else {
    sub("^d", "r", density)
  }
}

to_support <- function(supp) {
  lower <- supp@.Data[1]
  upper <- supp@.Data[2]
  list(
    type = supp@type,
    lower = if (is.finite(lower)) lower else NA,
    upper = if (is.finite(upper)) upper else NA,
    closed = as.logical(supp@closed)
  )
}

models <- lapply(names(meta), function(name) {
  m <- meta[[name]]
  id <- sub("^ml", "", name)
  defaults <- m$default
  if (is.null(defaults)) defaults <- m$defaults
  params <- lapply(seq_along(m$names), function(i) {
    list(
      name = m$names[[i]],
      role = NA,
      default = defaults[[i]],
      support = list(lower = NA, upper = NA, closed = c(FALSE, FALSE))
    )
  })

  list(
    id = id,
    name = m$model,
    density = m$density,
    support = to_support(m$support),
    params = params,
    sampler = to_sampler(m$density),
    start_hints = list(method = NA, notes = NA),
    validation_hooks = list(),
    legacy_impl = file.path("R", paste0(name, ".R"))
  )
})

jsonlite::write_json(list(models = models), "metadata/models.json", pretty = TRUE, auto_unbox = TRUE, na = "null")
