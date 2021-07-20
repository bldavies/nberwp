#' NBER working paper authors
#'
#' Data frame containing author attributes.
#'
#' @docType data
#'
#' @usage data(authors)
#'
#' @format Data frame with columns
#' \describe{
#' \item{author}{Author ID}
#' \item{name}{Author name}
#' \item{user_nber}{Author username on NBER website}
#' \item{user_repec}{Author username on RePEc}
#' }
#'
#' @examples
#' authors
#'
#' if (require('dplyr')) {
#' paper_authors %>% count(author) %>% left_join(authors)
#' }
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"authors"
