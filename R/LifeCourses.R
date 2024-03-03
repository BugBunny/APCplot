#' Plots of life courses on a triangular Lexis diagram 
#' 
#' @description
#' `LifeCourses` draws the life courses of one or more individuals on a
#' Lexis plot represented as a grid of equilateral triangles. The vectors of 
#' years of birth and ages at death and lists of othe life events should all 
#' refer to the same number of individuals (i.e. be the same length).
#'
#' @param YrB Vector of years of birth (either calendar years or years ago).
#' @param AgeD Vector of ages at death or censoring in years
#' @param censored Vector indicating whether individuals died or were censored
#' @param Events List of vectors of ages at event (e.g. giving birth). NULL if none.
#' @param EvType Name describing the event for use in the key e.g. "Births"
#' @param Events2 List of vectors of ages at second type of event. NULL if none.
#' @param EvType2 Name describing the event for use in the key e.g."Marriage"
#' @param Events3 List of vectors of ages at third type of event. NULL if none. 
#' @param EvType3 Name describing the event for use in the key e.g."Divorce"
#' @param base_year Initial birth cohort
#' @param survey_year Year in which retrospective data were collected
#' @param base_age Youngest age supplying data 
#' @param length_yrs Number of cohorts required
#' @param exact_data Flags the use of exact ages/dates, rather than durations in 
#' completed years
#' @param plot_title Title
#'
#' @return R graphics plot that can be exported to multiple graphics formats.
#' @export
#'
#' @examples
#' ## Initialise life course histories for 4 individuals
#' YearB <- c(1925, 1938, 1962, 2001)
#' AgeD <- c(70, 80, 50, NA)
#' evType <- "Birth of child"
#' events <- list(NULL, c(17,26), c(32), NULL)
#' evType2 <- "Marriage"
#' events2 <- list(c(22, 40), c(16), c(25), NULL)
#' evType3 <- "Marital dissolution"
#' events3 <- list(c(28), NULL, NULL, NULL)
#' Censored <- c(F, F, T, F)
#' 
#' ## Draw lexis diagram with cohort and period measured in calendar years
#' LifeCourses(YearB, AgeD, Events = events, EvType = evType,  
#'                 Events2 = events2, EvType2 = evType2, Events3 = events3, 
#'                 EvType3 = evType3, censored = Censored, survey_year = 2020)
#' 
#' ## Redraw, indexing cohort and period as years before the end of observation
#' YearB <- c(95, 82, 58, 19)
#' LifeCourses(YearB, AgeD, Events = events, EvType = evType,  
#'                 Events2 = events2, EvType2 = evType2, Events3 = events3, 
#'                 EvType3 = evType3, censored = Censored)
LifeCourses <- function(YrB,
                        AgeD,
                        censored = FALSE,
                        Events = NULL,
                        EvType = "",
                        Events2 = NULL,
                        EvType2 = "",
                        Events3 = NULL,
                        EvType3 = "",
                        base_year = 0L,
                        survey_year = 0L,
                        base_age = 0L,
                        length_yrs = 100L,
                        exact_data = FALSE,
                        plot_title = "Example") {
library(Ternary)
# Set up axis labels   
if (survey_year > 0) base_year <- survey_year - length_yrs
intvl <- ifelse(length_yrs <= 50, 5, 10)

lexis_labels <- if (base_year == 0) {
   list(seq(base_age, base_age + length_yrs, by = intvl),
        seq(base_age + length_yrs, base_age, by = -intvl),
        seq(0, length_yrs, by = intvl))  
} else {
   list(seq(base_age, base_age + length_yrs, by = intvl),
        seq(base_year, base_year + length_yrs, by = intvl),
        seq(base_year + length_yrs, base_year, by = -intvl))
}
# Set up legend
evTypes <- c("Death", "Loss to follow-up")
symbs <- c(16, 1)
evcols <- c("red3", "red3")
if (EvType != "") {
   evTypes <- c(evTypes, EvType)
   evcols <- c(evcols, "blue4")
   symbs <- c(symbs, 16)
}
if (EvType2 != "") {
   evTypes <- c(evTypes, EvType2)
   evcols <- c(evcols, "orange")
   symbs <- c(symbs, 16)
}
if (EvType3 != "") {
   evTypes <- c(evTypes, EvType3)
   evcols <- c(evcols, "purple")
   symbs <- c(symbs, 16)   
}
# Draw the grid
rgn <- list(min = c(0, 0, length_yrs), max = c(length_yrs, length_yrs, 0))
TernaryPlot(alab = "Age", blab = "Cohort", clab = "Period", region = rgn,
   grid.lines = ceiling(length_yrs / intvl), axis.labels = lexis_labels, 
   main = plot_title)

# Recode dates of events reversing either the period or cohort axis
midyr <- ifelse(exact_data, 0, 0.5)
YrB <- YrB + midyr
YrB <- (if (base_year > 0) base_year + length_yrs - YrB else YrB) / 100
CohB <- 1 - YrB
# If censored by end of observation, not loss or withdrawal, set AgeD to time 0
for (i in 1:length(YrB)) {
   if (is.null(AgeD[i]) | is.na(AgeD[i])) {
      AgeD[i] <- YrB[i] 
   } else { 
      AgeD[i] <- (AgeD[i] + midyr) / 100
   } 
}
YrD <- YrB - AgeD
if (any(YrD < 0)) stop("age at death in the future")
N <- length(CohB)
# Loop over individuals plotting details of their life courses
for (i in 1:N) { 
   TernaryLines(list(c(0, CohB[i], YrB[i]), c(AgeD[i], CohB[i], YrD[i])),
                col = "red3", lwd = 1.75)
   typ_pch <- ifelse(censored[i] == TRUE, 1, 16)
   e_coords <- c(AgeD[i], CohB[i], YrD[i])
   # Do not plot censored marker if censored at by the end of data collection
   if (YrD[i] > 0) TernaryPoints(e_coords, col = "red3", pch = typ_pch)
   # Plot up to three sets of life course events
   for (j in 1:3) {
      if (j == 1) {events <- Events[[i]];  ecol <- "blue4"}
      if (j == 2) {events <- Events2[[i]]; ecol <- "orange"}
      if (j == 3) {events <- Events3[[i]]; ecol <- "purple"}
      e_coords <- list()
      # If no events of this type, length(events) == 0
      e_n <- length(events) 
      if (e_n > 0) {
         for (k in 1:e_n) {
            AgeE <- (events[[k]] + midyr) / 100
            YrE <- YrB[i] - AgeE
            if (length(YrE) > 0)
               if (AgeE > AgeD[i]) stop("event occurs after exit")
            e_coords[[length(e_coords) + 1]] <- c(AgeE, CohB[i], YrE)
         }
         TernaryPoints(e_coords, col = ecol, pch = 16)
      }
   }
}
legend("topright", legend = evTypes, pch = symbs, col = evcols,
       cex = 0.75, bty = "n", pt.cex = 1.25, y.intersp = 0.75,
       xpd = NA )     # Do not clip at edge of figure)
}
