#' Abalone data
#'
#' Physical measurements of 4177 abalones, a species of sea snail.
#'
#' See the web page \url{https://archive.ics.uci.edu/ml/datasets/Abalone} for
#'    more information about the data set.
#'
#' @format A \link[tibble]{tibble} with 4,177 observations and 9 variables:
#'   \describe{
#'     \item{sex}{Sex of the abolone, \code{F} is female, \code{M} male, and \code{I} infant.}
#'     \item{length}{Longest shell measurement.}
#'     \item{diameter}{Diameter perpendicular to length.}
#'     \item{height}{Height with with meat in shell.}
#'     \item{whole_weight}{Grams	whole abalone.}
#'     \item{shucked_weight}{Grams weight of meat.}
#'     \item{viscera_weight}{Grams gut weight (after bleeding).}
#'     \item{shell_weight}{Grams after being dried.}
#'     \item{rings}{+1.5 gives the age in years.}
#'   }
#'
#' @source Dua, D. and Graff, C. (2019). UCI Machine Learning Repository \url{http://archive.ics.uci.edu/ml}. Irvine, CA: University of California, School of Information and Computer Science.
#' @references
#'   Ko, V., Hjort, N. L., & Hobaek Haff, I. (2019). Focused information criteria for copulas. Scandinavian Journal of Statistics.
#' @examples
#' abalone
"abalone"
