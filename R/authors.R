#' NBER working paper authors
#'
#' Data frame containing author attributes.
#'
#' Most author genders come from matching authors' names against 1940--1995 US
#' Social Security Administration baby name data provided by the package
#' \emph{babynames}, and against Facebook name and self-reported gender data
#' collected by Tang et al. (2011).
#' Remaining genders come from manual identification, which involves searching for
#' gendered pronouns in online biographies, news articles, and professional and
#' social media profiles.
#' It also involves using online photos and video (e.g., interview and lecture)
#' footage.
#' See Davies (2022) for more details.
#'
#' @docType data
#'
#' @usage data(authors)
#'
#' @format Data frame with columns
#' \describe{
#' \item{author}{Author ID.}
#' \item{name}{Author name.}
#' \item{user_nber}{Author username on NBER website.}
#' \item{user_repec}{Author username on RePEc.}
#' \item{female}{Binary indicator for whether author is female.
#'   Values of -1 denote genders that could not be identified.}
#' \item{female_source}{Source of gender information.
#'   Values of 1 and 2 denote genders identified using SSA baby name and Facebook
#'   data, respectively.
#'   Negative values -x denote genders identified incorrectly by source x and
#'   overwritten manually.
#'   Values of 0 denote genders identified manually.}
#' }
#'
#' @examples
#' authors
#'
#' if (require('dplyr')) {
#' paper_authors %>% count(author) %>% left_join(authors)
#' }
#'
#' @references
#' Davies, B. (2022).
#' Sex-based sorting among economists: Evidence from the NBER.
#' SocArXiv.
#' \doi{10.31235/osf.io/zeb7a}
#'
#' Tang, C., Ross, K., Saxena, N., and Chen, R. (2011).
#' What’s in a Name: A Study of Names, Gender Inference, and Gender Behavior in Facebook.
#' In Xu, J., Yu, G., Zhou, S., and Unland, R., editors,
#' \emph{Database Systems for Advanced Applications}, volume 6637.
#' Springer, Berlin, Heidelberg.
#'
#' @source \href{https://data.nber.org/nber-wp-logs/}{National Bureau of Economic Research}
"authors"
