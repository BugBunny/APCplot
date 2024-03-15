
#' Compute Lexis mortality surface from HMD data and plot using APCplot
#' 
#' @description
#' `APCmortHMD` is a function to download and reorganise mortality data from
#' the Human Mortality Database (HMD) for passing to APCplot.

#' 
#' @details
#' Users must first register on the Human Mortality Database
#' [web site](https://mortality.org) in order to download the data (N. B. If you 
#' registered before June 2022, you need to re-register).
#' 
#' Note that not all countries have a century-long run of data and the plotting
#' function has not yet been generalized for use with axes of differing length
#' (e.g. 50 years of data on ages 0 to 99).
#' 
#' @param user The email address used to create an HMD account.
#' @param password The password for that HMD account.
#' @param country_id Country code listed in the data section of the HMD website.
#' @param base_year Initial birth cohort for which rates are required.
#' @param length_yrs Number of APC categories for which rates are required.
#' @param ... Parameters to be passed to function `APCplot`
#'
#' @return Data frame with death rates by sex indexed by age, period and cohort. 
#' @export
#'
#' @examples
#' ## Load synthetic HMD data files as the real ones are password protected
#' data(Deaths)
#' data(Exposures)
#' 
#' ## For the avoidance of doubt, Atlantis isn't real and neither are these data
#' id <- "ATLANTIS"
#' Lexis <- APCmortHMD(user = yourID, password = yourPW, country_id = id, 
#' base_year = 1921)
APCmortHMD <- function(user,
                         password,
                         country_id,
                         base_year = 1922L,
                         length_yrs = 100L, 
                         ...) {
# Download the deaths and exposure counts for the Lexis triangles
if (!exists("Deaths")) {
   fname <- paste0("Deaths_", country_id, ".rda")
   if (!file.exists(fname)) {
      Deaths <- HMDHFDplus::readHMDweb(country_id, "Deaths_lexis",
         user, password)
      save(Deaths, file = fname)
   }  else {
      load(fname)
   }
}
if (!exists("Exposures")) {
   fname <- paste0("Exposures_", country_id, ".rda")
   if (!file.exists(fname)) {
      Exposures <- HMDHFDplus::readHMDweb(country_id, "Exposures_lexis",
         user, password)
      save(Exposures, file = fname)
   } else {
     load(fname)
   }
}   
# Calculate the APC death rates by Sex and for the two sexes combined
Lexis <- cbind(Deaths[ , 1:3],
   Deaths$Female / Exposures$Female,
   Deaths$Male / Exposures$Male,
   Deaths$Total / Exposures$Total)

# Compute APC indices that run from 0 up, reversing the period axis
Lexis[ , "Age"] <- as.integer(Lexis[, "Age"])
Lexis[ , "Cohort"] <- as.integer(Lexis[, "Cohort"]) - base_year
Lexis[ , "Year"] <- base_year + length_yrs-1 - as.integer(Lexis[, "Year"])

colnames(Lexis) <- c("age", "coh", "per", "Women", "Men", "Rates")
# Discard data on older ages and earlier dates (Data for the cohort born the 
# year prior to the base_year is kept to calculate decline into cohort 0)
Lexis <- with(Lexis, Lexis[age < length_yrs & coh >= -1 &  per <= length_yrs, ])
APCplot(Lexis, length_yrs = length_yrs, base_year = base_year, ...)
invisible(Lexis)
}  # End of function APCmortHMD
