#' Lexis data frame for plotting by APCplot
#' 
#' \code{Lexis} data frame for input to \code{APCplot}. This data frame contains
#' 100 years of data and mirrors the datasets created from _Human Mortality
#' Database_ data by \code{setupHMDdata}. It is used by the examples in the
#' \code{APCplot} help file. It also illustrative of the structure of Lexis data
#' frames for anyone preparing them from scratch. N.B. In this dataset time is
#' measured forward from a base date and the period axis has been reversed. If
#' time were being measured backward (e.g. as years before a date of fieldwork
#' of 0), the cohort axis would need to be reversed instead.
#' 
#' @format{ A data frame with 10,200 rows and 6 columns:}
#' \itemize{
#'    \item{\code{age}}:  {Age index} 
#'    \item{\code{coh}}:  {Cohort index \code{<- Year_of_birth - Base_year}}
#'    \item{\code{per}}:  {Reversed period index \code{<- Base_year+99 - Year}}#' 
#'    \item{\code{Women}}:  {Women's death rates calculated as Deaths/Exposures}
#'    \item{\code{Men}}:  {Men's death rates calculated as Deaths/Exposures}
#'    \item{\code{Rates}}:  {Total death rates calculated as Deaths/Exposures}
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
 #' Demographic and Health Survey individual recode data imported using haven
 #' 
 #' Data frame for input to \code{setupDHSdata}. These are a subset of the
 #' variables in a synthetic DHS data set (ZZIR61DT.DTA) that can be downloaded 
 #' from the _Demographic and Health Survey_ web site. These data are open  
 #' access but users are required to register and apply for access to real DHS 
 #' datasets.
 #' 
 #' @format{ A data frame with 8348 rows and 321 columns:}
 #' 
 #' @docType data
 #' @name DHSurvey
 #' @usage data(DHSurvey)
 #' @keywords datasets
 #' @source These are synthetic data
 #' @references Demographic and Health Survey: \url{dhsprogram.com}
 "DHSurvey"