
#' Set up HMD data for input to APCplot
#' 
#' @description
#' `setupHMDdata` is a function to download and reorganise mortality data from
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
#'
#' @return Data frame with death rates by sex indexed by age, period and cohort. 
#' @export
#'
#' @examples
#' yourID <- readline(prompt = "Input HMD user id (without quotes): ")
#' yourPW <- readline(prompt = "Input HMD password (without quotes): ")
#' id <- "GBRTENW"
#' Lexis <- setupHMDdata(user = yourID, password = yourPW, country_id = id, base_year = 1922)
#' save(Lexis, file = paste0("Lexis_", id, ".rda"))
setupHMDdata <- function(user,
                         password,
                         country_id = "GBRTENW",
                         base_year = 1922L,
                         length_yrs = 100L) {
   # Download the deaths and exposure counts for the Lexis triangles
   fn <- paste0("Deaths_", country_id, ".rda")
   if (file.exists(fn) == FALSE) {
      Deaths <- HMDHFDplus::readHMDweb(country_id, "Deaths_lexis",
         user, password)
      save(Deaths, file = fn)
   }  else {
      load(fn)
   }
   fn <- paste0("Exposures_", country_id, ".rda")
   if (file.exists(fn) == FALSE) {
      Exposures <- HMDHFDplus::readHMDweb(country_id, "Exposures_lexis",
         user, password)
      save(Exposures, file = fn)
   } else {
     load(fn)
   }
   # Calculate the APC death rates by Sex and for the two Sexes combined
   Lexis <- cbind(Deaths[ , 1:3],
      Deaths$Female / Exposures$Female,
      Deaths$Male / Exposures$Male,
      Deaths$Total / Exposures$Total)
   colnames(Lexis)[(ncol(Lexis)-2):ncol(Lexis)] <- c("Women", "Men", "Rates")

   # Compute APC indices that run from 0 up, reversing the period axis
   Lexis[, "age"] <- as.integer(Lexis[, "Age"])
   Lexis[, "coh"] <- as.integer(Lexis[, "Cohort"]) - base_year
   Lexis[, "per"] <- base_year + length_yrs-1 - as.integer(Lexis[, "Year"])
   # Discard data on older ages and earlier dates (Data for the cohort born the 
   #  year prior to the base_year is kept to calculate decline into cohort 0)
   subset(Lexis, age < length_yrs & coh >= -1 & per >= 0 & per <= length_yrs)
}  # End of function setupHMDdata
