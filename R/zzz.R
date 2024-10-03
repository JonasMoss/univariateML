for (model in univariateML_models) {
  fun <- paste0("ml", model)
  assign(fun, decorator(fun))
}

rm(fun)