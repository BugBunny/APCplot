
#' Lexis data frame for plotting by APCplot
#' 
#' \code{Lexis_ATLANTIS} is a \code{Lexis} data frame for input to
#' \code{APCplot}. It mirrors the datasets created from _Human Mortality
#' Database_ data by \code{setupHMDdata} and is used by the examples in the
#' \code{APCplot} help file. It also illustrates the structure of Lexis data
#' frames for anyone preparing them from scratch.
#' 
#' @format{ A data frame with 10,200 rows and 9 columns:}
#' \itemize{
#'    \item{\code{Year}}:  {Calendar year - (unused, copied from the HMD files)}
#'    \item{\code{Age}}:  {Age in years - (unused, copied from the HMD files)}
#'    \item{\code{Cohort}}:  {Birth year - (unused, copied from the HMD files)}
#'    \item{\code{Women}}:  {Women's death rates calculated as Deaths/Exposures}
#'    \item{\code{Men}}:  {Men's death rates calculated as Deaths/Exposures}
#'    \item{\code{Rates}}:  {Total death rates calculated as Deaths/Exposures}
#'    \item{\code{age}}:  {Age index \code{<- Age}}
#'    \item{\code{coh}}:  {Cohort index \code{<- Cohort - Base_year}}
#'    \item{\code{per}}:  {Reversed period index \code{<- Base_year + 99 - Year}}
#' }
#' @docType data
#' @name Lexis_ATLANTIS
#' @usage data(Lexis_ATLANTIS)
#' @keywords datasets
#' @source For the avoidance of doubt, Atlantis is not a real place and these
#' are synthetic data.
#' @references Human Mortality Database: \url{mortality.org}
"Lexis_ATLANTIS"