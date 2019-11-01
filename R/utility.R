
#' Merges two lists.
#'
#' @keywords internal
#' @param x A list of default arguments.
#' @param y A list of supplied arguments
#' @param type If \code{merge}, the list will be merge with y
#' having priority; if \code{template}, named the elements of
#' y not in x will be discarded after merging.
#' @return A merged list where conflicts are solved in favor
#' of y. Does not preserve ordering.

# examples \dontrun{
#     x = list(a = 5,
#              b = 0,
#              c = "a",
#              d = NULL)
#
#     y = list(a = 3,
#              b = 7,
#              f = NA)
#
#    listmerge(x, y, type = "merge")
#    listmerge(x, y, type = "template")}

listmerge = function(x, y, type = c("merge", "template")) {

  type = match.arg(type)

  if(length(y) == 0) {
    return(x)
  }

  ## Keep and not-keep are quite different.
  if(type == "merge") {
    matches = match(names(y), names(x))
    elements_to_discard = matches[!is.na(matches)]
    if(length(elements_to_discard) == 0)  {
      combined = c(y, x)
    } else{
      combined = c(y, x[-elements_to_discard])
    }
    return(combined)
  }

  if(type == "template") {
    matches = match(names(x), names(y))
    x[!is.na(matches)] = y[matches[!is.na(matches)]]
    return(x)
  }

}
