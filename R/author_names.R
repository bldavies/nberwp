#' NBER working paper author names
#'
#' Data frame containing author names.
#'
#' @docType data
#'
#' @usage data(author_names)
#'
#' @format Data frame with columns
#' \describe{
#' \item{author}{Author ID.}
#' \item{name}{Author name.}
#' \item{user_nber}{Author username on NBER website.}
#' \item{user_repec}{Author username on RePEc.}
#' }
#'
#' @examples
#' author_names
#'
#' if (require('dplyr')) {
#' paper_authors %>% count(author) %>% left_join(author_names)
#' }
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"author_names"
