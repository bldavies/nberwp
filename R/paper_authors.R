#' NBER working paper authors
#'
#' Data frame containing paper-author correspondences.
#'
#' @docType data
#'
#' @usage data(paper_authors)
#'
#' @format Data frame with columns
#' \describe{
#' \item{paper}{Working paper number}
#' \item{author}{Author ID}
#' }
#'
#' @examples
#' paper_authors
#'
#' if (require('dplyr')) {
#' paper_authors %>% count(author) %>% left_join(authors)
#' }
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"paper_authors"
