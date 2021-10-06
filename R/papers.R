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
