#' Lexis data frame for plotting by APCplot
#' 
#' \code{Lexis} data frame for input to \code{APCplot}. This data frame mirrors
#' the datasets created from _Human Mortality Database_ data by
#' \code{setupHMDdata} and is used by the examples in the \code{APCplot} help
#' file. It also illustrative of the structure of Lexis data frames for anyone
#' preparing them from scratch. N.B. In this dataset time is measured forward
#' from a base date and the period axis has been reversed. If time were being
#' measured backward (e.g. as years before a date of fieldwork of 0), the cohort
#' axis would need to be reversed instead.
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
#'    \item{\code{per}}:  {Reversed period index \code{<- Base_year+99 - Year}}
#' }
#' @docType data
#' @name Lexis
#' @usage data(Lexis)
#' @keywords datasets
#' @source These are synthetic data
"Lexis"
#' Deaths data imported from the Human Mortality Database using HMDHFDplus
#' 
#' Data frame for input to \code{setupHMDdata}. This data frame mirrors the 
#' datasets that can be downloaded from _Human Mortality Database_ data by
#' registered users.
#' 
#' @format{ A data frame with 1000s of rows and 7 columns:}
#' \itemize{
#'    \item{\code{Year}}:  {Calendar year}
#'    \item{\code{Age}}:  {Age in years}
#'    \item{\code{Cohort}}:  {Birth year}
#'    \item{\code{Female}}:  {Count of deaths of females}
#'    \item{\code{Men}}:  {Count of deaths of males}
#'    \item{\code{Total}}:  {Count of deaths of both sexes}
#'    \item{\code{OpenInterval}}:  {TRUE for the open-ended age interval}
#' }
#' @docType data
#' @name Deaths
#' @usage data(Deaths)
#' @keywords datasets
#' @source These are synthetic data
#' @references Human Mortality Database: \url{mortality.org}
 "Deaths"
 #' Exposure data imported from the Human Mortality Database using HMDHFDplus
 #' 
 #' Data frame for input to \code{setupHMDdata}. This data frame mirrors the 
 #' datasets that can be downloaded from _Human Mortality Database_ data by
 #' registered users.
 #' 
 #' @format{ A data frame with 1000s of rows and 7 columns:}
 #' \itemize{
 #'    \item{\code{Year}}:  {Calendar year}
 #'    \item{\code{Age}}:  {Age in years}
 #'    \item{\code{Cohort}}:  {Birth year}
 #'    \item{\code{Female}}:  {Count of years of exposure of females}
 #'    \item{\code{Men}}:  {Count of years of exposure of males}
 #'    \item{\code{Total}}:  {Count of years of exposure of both sexes}
 #'    \item{\code{OpenInterval}}:  {TRUE for the open-ended age interval}
 #' }
 #' @docType data
 #' @name Exposures
 #' @usage data(Exposures)
 #' @keywords datasets
 #' @source These are synthetic data
 #' @references Human Mortality Database: \url{mortality.org}
 "Exposures"