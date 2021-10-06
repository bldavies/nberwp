#' NBER working paper programs
#'
#' Crosswalk between NBER program codes, descriptions and categories.
#'
#' @docType data
#'
#' @usage data(programs)
#'
#' @format Data frame with columns
#' \describe{
#' \item{program}{Program code.}
#' \item{program_desc}{Program description.}
#' \item{program_category}{Program category based on Chari and Goldsmith-Pinkham (2017).}
#' }
#'
#' @references Chari, A. and P. Goldsmith-Pinkham (2017).
#' Gender Representation in Economics Across Topics and Time: Evidence from the NBER Summer Institute.
#' NBER Working Paper No. 23953,
#' National Bureau of Economic Research.
#'
#' @examples
#' programs
#'
#' if (require('dplyr')) {
#' paper_programs %>% count(program) %>% left_join(programs)
#' }
#'
#' @source \href{https://www.nber.org}{National Bureau of Economic Research}
"programs"
