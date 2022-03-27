#' NBER working papers
#'
#' Data frame containing working paper attributes.
#'
#' @docType data
#'
#' @usage data(papers)
#'
#' @format Data frame with columns
#' \describe{
#' \item{paper}{Working paper number.}
#' \item{year}{Publication year.}
#' \item{month}{Publication month.}
#' \item{title}{Title.}
#' \item{outlet}{Publication outlet code:
#'   \enumerate{
#'     \item{"Top five" journal (i.e., \emph{American Economic Review}, \emph{Econometrica}, \emph{Journal of Political Economy}, \emph{Quarterly Journal of Economics}, or \emph{Review of Economic Studies}).}
#'     \item{Other journal.}
#'     \item{Book or book chapter.}
#'   }
#'   If published in multiple outlets then uses lowest code.
#'   Unpublished papers have NA values.
#'   Publication statuses last updated on March 26, 2022.}
#' }
#'
#' @examples
#' papers
#'
#' if (require('dplyr')) {
#' papers %>% count(year)
#' }
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"papers"
