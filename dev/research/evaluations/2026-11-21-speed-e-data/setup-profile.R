# Copy into generated tests/testthat to record worker-side file timings and
# expectation events, including assertions outside test_that() blocks.
if (nzchar(Sys.getenv("UMVL_FILE_PROFILE_DIR"))) {
  trace("test_one_file", where = asNamespace("testthat"), print = FALSE,
        tracer = quote({
          umvl_file_reporter <- get_reporter()
          umvl_file_add_result <- umvl_file_reporter$add_result
          umvl_file_locked <- bindingIsLocked("add_result", umvl_file_reporter)
          if (umvl_file_locked) unlockBinding("add_result", umvl_file_reporter)
          umvl_file_counts <- new.env(parent = emptyenv())
          umvl_file_reporter$add_result <- local({
            original <- umvl_file_add_result
            counts <- umvl_file_counts
            function(context, test, result) {
              kind <- class(result)[1L]
              previous <- counts[[kind]]
              counts[[kind]] <- if (is.null(previous)) 1L else previous + 1L
              original(context, test, result)
            }
          })
          umvl_file_clock <- proc.time()
        }),
        exit = quote({
          elapsed <- proc.time() - umvl_file_clock
          umvl_file_reporter$add_result <- umvl_file_add_result
          if (umvl_file_locked) lockBinding("add_result", umvl_file_reporter)
          kinds <- c("expectation_success", "expectation_failure", "expectation_error",
                     "expectation_warning", "expectation_skip")
          events <- vapply(kinds, function(kind) {
            count <- umvl_file_counts[[kind]]
            if (is.null(count)) 0L else count
          }, integer(1L))
          dest <- file.path(Sys.getenv("UMVL_FILE_PROFILE_DIR"), paste0(basename(path), ".csv"))
          utils::write.csv(as.data.frame(as.list(c(cpu = sum(elapsed[c(1:2, 4:5)]),
                                                 wall = elapsed[[3L]], events))),
                           dest, row.names = FALSE)
        }))
}
