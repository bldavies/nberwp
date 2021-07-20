#' NBER working paper programs
#'
#' Crosswalk between NBER working papers and programs.
#'
#' @docType data
#'
#' @usage data(paper_programs)
#'
#' @format Data frame with columns
#' \describe{
#' \item{paper}{Working paper number}
#' \item{program}{Program code}
#' }
#'
#' @examples
#' paper_programs
#'
#' if (require('dplyr')) {
#' paper_programs %>% count(program) %>% left_join(programs)
#' }
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"paper_programs"
