
#' Compute Lexis fertility surface from a DHS survey and plot using APCplot
#'
#' @description
#' `APCfertDHS` calculates single-year age-period-cohort birth rates from the
#' birth histories reported in a Demographic and Health Survey. Women's birth 
#' cohort is defined by their age at interview and period is measured backward 
#' from the time of interview but an option exists to label the axes in calendar
#' years. 
#' 
#' @details
#' Users must first register on the Demographic and Health Surveys
#' [web site](https://dhsprogram.com) and request access to the survey(s) of 
#' interest. After downloading the relevant zipped flat (i.e. wide) Stata
#' individual recode data files (??IR??DT.ZIP), they should extract the Stata
#' .dta files to their working directory before running `APCfertDHS`. The 
#' function returns the Lexis data frame that it passes to APCplot, so users can
#' save it to use to produce further plots by calling `APCplot` directly. 
#'
#' @param survey Survey name (the .dta extension is not required)
#' @param fpath Path to the directory containing the survey data file
#' @param byVar Categorical variable defining sub-groups for separate plots 
#' @param subsetVar Variable identifying a sub-set of respondents to be plotted
#' @param subsetVal Code identifying the respondents to be kept
#' @param ... Parameters to be passed to function `APCplot`
#'
#' @return Data frame of birth rates indexed by age, period and cohort of mother
#' @export
#'
#' @examples
#' # Use a flat (i.e. wide) individual recode data file from a synthetic DHS 
#' # survey (ZZIR62FL). (To save space, the file only contains a few variables).
#'  
#' data(DHSurvey)
#' 
#' APCfertDHS(ZZIR62FL, byVar = "v190")
APCfertDHS <- function(survey,
                         fpath = "",
                         byVar = NULL,
                         subsetVar = NULL,
                         subsetVal = NA,
                         ...) {
## If the RDA file doesn't exist, import it from a flat IR Stata file
if (!exists("DHSurvey")) {
   fpath <- paste0(fpath, survey, ".DTA")
   svyVars <- c("v0", "v016", "v020", "v021", "v022", "v023", "awfactt")
   bhVars <- c("bidx", "b0", "b1", "b2", "b3", "b4", "b5", "b6")
   keepVars <- c("v010", "v011", "v012", svyVars, byVar, subsetVar, bhVars)
   DHSurvey <- haven::read_dta(fpath, col_select = starts_with(keepVars))
}
## Subset DHS survey if required
if ( ! is.null(subsetVar)) 
   DHSurvey <- DHSurvey[DHSurvey[, subsetVar] == subsetVal, ]
if ( ! is.null(byVar)) DHSurvey[[byVar]] <- haven::as_factor(DHSurvey[[byVar]])
apc_names <- c("age", "per", "coh")
base_age <- 15L
length_yrs <- 35L

## Split weighted exposure in the year before the survey by cohort and age
DHSurvey <- within(DHSurvey, {
   expos_adj <- ifelse(v020 == 1, awfactt / 100, 1L)
   v005 <- v005 / 1000000
   dayI <- (v016 + 0.5)/365.25
   dayB <- stats::runif(1) * ifelse(v006 == v009, dayI, 0.083333)
   e_upr <- ((v008 - v011)/12 - v012 + dayI - dayB) * v005 * expos_adj 
   e_lwr <- v005 * expos_adj - e_upr
})
## Aggregate exposure in period 0 by birth cohort for the two ages it is exposed
eu_temp <- e_upr_age <- stats::aggregate(DHSurvey$e_upr, 
   by = list(factor(DHSurvey[["v012"]])), sum)
el_temp <- e_lwr_age <- stats::aggregate(DHSurvey$e_lwr, 
   by = list(factor(DHSurvey[["v012"]])), sum)
unprefixed <- NULL
## Aggregate exposure in period 0 by birth cohort and sub-group for the two ages
if ( ! is.null(byVar)) {
   e_upr_age <- stats::aggregate(DHSurvey$e_upr, 
      by = list(factor(DHSurvey[["v012"]]), DHSurvey[[byVar]]), sum)
   e_lwr_age <- stats::aggregate(DHSurvey$e_lwr, 
      by = list(factor(DHSurvey[["v012"]]), DHSurvey[[byVar]]), sum)
   ## Reshape to wide with exposure for each sub-group as a separate variable
   e_upr_age <- stats::reshape(e_upr_age, direction = "wide", 
      idvar = "Group.1", timevar = "Group.2", v.names = "x")
   e_lwr_age <- stats::reshape(e_lwr_age, direction = "wide", 
      idvar = "Group.1", timevar = "Group.2", v.names = "x")
   unprefixed <- sapply(strsplit(names(e_upr_age[2:length(e_upr_age)]), "x."), 
             function(x) (x[2]))   
   ## Append variable for total exposure in period 0 by birth cohort
   e_upr_age <- cbind(e_upr_age,  eu_temp[ , 2])
   e_lwr_age <- cbind(e_lwr_age,  el_temp[ , 2])
}   
## Add period and age indices to the data frame
e_upr_age <- cbind(as.integer(e_upr_age[ , "Group.1"]) - 1L, 0L, e_upr_age)
colnames(e_upr_age) <- c(apc_names, unprefixed, "Rates")
e_lwr_age <- cbind(as.integer(e_lwr_age[ , "Group.1"]) - 2L, 0L, e_lwr_age)
colnames(e_lwr_age) <- c(apc_names, unprefixed, "Rates")
## Stack exposure in the two age groups and tidy up
Exposures <- rbind(e_upr_age, e_lwr_age)
Exposures[ , "coh"] <- length_yrs - as.integer(Exposures[ , "coh"])
## Add on exposure in earlier periods, rejuvenating age by a year each time
for (per in 1:(length_yrs-1)) {
   new_expos <- Exposures[1:(2L * length_yrs), ]
   new_expos[ , "per"] <- per
   new_expos[ , "age"] <- new_expos[ , "age"] - per
   Exposures <- rbind(Exposures, new_expos)
}
## Discard exposure before women's 15th birthday
Exposures <- Exposures[Exposures[ , "age"] >= 0L, ]

## Reshape the DHS file to a "long" births file and discard the empty records
DHS_Long <- stats::reshape(DHSurvey, direction = "long", varying = grep("^b", 
   colnames(DHSurvey)), sep = "_", idvar = c("v001", "v002", "v003"))
DHS_Long <- DHS_Long[!is.na(DHS_Long[, "bidx"]), ]
## Impute birth day of month and calculate single-year age and period of birth
DHS_Long <- within(DHS_Long, {
   dayC <- stats::runif(1) * ifelse(v008 == b3, dayI, 0.083333)
   perB <- floor((v008 - b3)/12 + dayI - dayC)
   ageB <- floor((b3 - v011)/12 + dayC - dayB)
})
## Discard births before 15th birthday 
DHS_Long <- DHS_Long[DHS_Long[ , "ageB"] >= base_age, ]
Births <- stats::aggregate(DHS_Long$v005, by = list(factor(DHS_Long$ageB), 
   factor(DHS_Long$perB), factor(DHS_Long$v012)), sum)
tempB <- Births
unprefixed <- NULL
## Aggregate births within each age, cohort and period cell and reshape to wide
if ( ! is.null(byVar)) {
   Births <-stats::aggregate(DHS_Long$v005, by = list(factor(DHS_Long$ageB),  
      factor(DHS_Long$perB), factor(DHS_Long$v012), DHS_Long[[byVar]]), sum)
   Births <- stats::reshape(Births, direction = "wide", timevar = "Group.4", 
      idvar = colnames(Births[1:3]), v.names = "x")
   unprefixed <- paste0("b-", sapply(strsplit(names(Births[4:length(Births)]),  
      "x."), function(x) (x[2])))
   Births <- with(Births, cbind(Births[order(Group.3, Group.2), ], tempB[ , 4]))
}
## Name birth variables differently from exposure ones for merge and re-index 
colnames(Births) <- c(apc_names, unprefixed, "b-Rates")
Births[ , "age"] <- as.integer(Births[ , "age"]) - 1L
Births[ , "per"] <- as.integer(Births[ , "per"]) - 1L
Births[ , "coh"] <- length_yrs - as.integer(Births[ , "coh"])

## Merge birth and exposure files, calculate the rates, and drop work variables
Lexis <- merge(Births, Exposures, all = TRUE)
for (f_rates in names(Exposures)[4:length(Exposures)]) {
   numratr <- paste0("b-", f_rates)
   Lexis[ , f_rates] <- 
      ifelse(is.na(Lexis[ , numratr]), 0, Lexis[ , numratr]) / Lexis[ , f_rates]
}
Lexis <- Lexis[, -c(4:length(Exposures))]
APCplot(Lexis, length_yrs = length_yrs, base_age = base_age, ...)
invisible(Lexis)
} ## End of function APCfertDHS
