#' Plots of life courses on a triangular Lexis diagram 
#' 
#' @description
#' `LifeCourses` draws the life courses of one or more individuals on a
#' Lexis plot represented as a grid of equilateral triangles. The vectors of 
#' years of birth and ages at death and lists of other life events should all 
#' refer to the same number of individuals (i.e. be the same length).
#'
#' @param YrB Vector of years of birth (either calendar years or years ago).
#' @param AgeD Vector of ages at death or censoring in years (Set to NA if
#' alive when observation ends by design)
#' @param censored Vector indicating whether individuals died or were censored
#' @param dLabel Description of the type of exit used in the key.
#' @param cLabel Description of the reason for censoring used in the key.
#' @param Events List of vectors of ages at first type of event. NULL if none.
#' @param eLabel Description of the event used in the key e.g. "Births"
#' @param Events2 List of vectors of ages at second type of event. NULL if none.
#' @param e2Label Description of the 2nd event used in the key e.g."Marriage"
#' @param Events3 List of vectors of ages at third type of event. NULL if none. 
#' @param e3Label Description of the 3rd event used in the key e.g."Divorce"
#' @param base_year Initial birth cohort
#' @param survey_year Year in which retrospective data were collected
#' @param base_age Youngest age supplying data 
#' @param length_yrs Number of cohorts required
#' @param exact_data Flags the use of exact ages/dates, rather than durations in 
#' completed years
#' @param plot_title Title for the plot
#'
#' @return R graphics plot that can be exported to multiple graphics formats.
#' @export
#'
#' @examples
#' ## Initialise life course histories for 4 individuals
#' YearB <- c(1925, 1938, 1962, 2001)
#' AgeD <- c(70, 80, 50, NA)
#' eLab <- "Birth of child"
#' events <- list(NULL, c(17,26), c(32), NULL)
#' e2Lab <- "Marriage"
#' events2 <- list(c(22, 40), c(16), c(25), NULL)
#' e3Lab <- "Marital dissolution"
#' events3 <- list(c(28), NULL, NULL, NULL)
#' cLab = "Emigration"
#' Censored <- c(FALSE, FALSE, TRUE, FALSE)
#' 
#' ## Draw lexis diagram with cohort and period measured in calendar years
#' LifeCourses(YearB, AgeD, Events = events, eLabel = eLab,  
#'                 Events2 = events2, e2Label = e2Lab, Events3 = events3, 
#'                 e3Label = e3Lab, censored = Censored, cLabel = cLab,
#'                survey_year = 2020)
#' 
#' ## Redraw, indexing cohort and period as years before the end of observation
#' YearB <- c(95, 82, 58, 19)
#' LifeCourses(YearB, AgeD, Events = events, eLabel = eLab,  
#'                 Events2 = events2, e2Label = e2Lab, Events3 = events3, 
#'                 e3Label = e3Lab, censored = Censored, cLabel = cLab)
LifeCourses <- function(YrB,
                        AgeD,
                        censored = FALSE,
                        dLabel = "Death",
                        cLabel = "Loss to follow up",
                        Events = NULL,
                        eLabel = "",
                        Events2 = NULL,
                        e2Label = "",
                        Events3 = NULL,
                        e3Label = "",
                        base_year = 0L,
                        survey_year = 0L,
                        base_age = 0L,
                        length_yrs = 100L,
                        exact_data = FALSE,
                        plot_title = "Individuals' Life Courses") {
# Colour definitions
colr1 <- "red3"
colr2 <- "blue4"
colr3 <- "orange3"
colr4 <- "purple"
# Set up axis labels   
if (survey_year > 0L) base_year <- survey_year - length_yrs
intvl <- ifelse(length_yrs <= 50L, 5L, 10L)

lexis_labels <- if (base_year == 0L) {
      list(seq(base_age, base_age + length_yrs, by = intvl),
         seq(base_age + length_yrs, base_age, by = -intvl),
         seq(0L, length_yrs, by = intvl))  
   } else {
      list(seq(base_age, base_age + length_yrs, by = intvl),
         seq(base_year, base_year + length_yrs, by = intvl),
         seq(base_year + length_yrs, base_year, by = -intvl))
   }
# Draw the grid
rgn <- list(min = c(0L, 0L, length_yrs), max = c(length_yrs, length_yrs, 0L))
Ternary::TernaryPlot(alab = "Age", blab = "Cohort", clab = "Period", 
   region = rgn, grid.lines = ceiling(length_yrs / intvl), 
   axis.labels = lexis_labels,main = plot_title)

# Recode dates of events, reversing either the period or cohort axis
midyr <- if (exact_data) 0 else 0.5
YrB <- YrB + midyr
YrB <- (if (base_year > 0L) base_year + length_yrs - YrB else YrB) / 100
CohB <- 1 - YrB
# If censored by end of observation, not loss or withdrawal, set AgeD to time 0
AgeD <- ifelse(is.na(AgeD), YrB, (AgeD + midyr)/100)
YrD <- YrB - AgeD
if (any(YrD < 0)) return("LifeCourses: age at exit in the future")

# Loop over individuals plotting details of their life courses
pch_type <- ifelse(censored, 1L, 16L)
for (i in 1:length(CohB)) {
   scoord <- c(0L, CohB[i], YrB[i])
   ecoord <- c(AgeD[i], CohB[i], YrD[i])
   if (YrD[i] < 0.0000001) {
      Ternary::TernaryArrows(scoord, ecoord, col = colr1, lwd = 1.75,
         length = 0.1, angle = 20) 
   } else {
      Ternary::TernaryLines(list(scoord, ecoord), col = colr1, lwd = 1.75)
      Ternary::TernaryPoints(ecoord, col = colr1, pch = pch_type[i])
   }
   # Plot up to three sets of life course events
   for (j in 1:3) {
      if (j == 1) {events <- Events[[i]];  ecolr <- colr2}
      if (j == 2) {events <- Events2[[i]]; ecolr <- colr3}
      if (j == 3) {events <- Events3[[i]]; ecolr <- colr4}
      # If no events of this type, length(events) == 0
      e_n <- length(events) 
      if (e_n > 0L) {
         AgeE <- (events + midyr) / 100
         if (any(AgeE > AgeD[i])) return("LifeCourses: event occurs after exit")
         YrE <- YrB[i] - AgeE
         e_coords <- list()         
         for (k in 1:e_n) {
            e_coords[[length(e_coords) + 1]] <- c(AgeE[k], CohB[i], YrE[k])
         }
         Ternary::TernaryPoints(e_coords, col = ecolr, pch = 16)
      }
   }
}
# Set up legend
lcLabels <- dLabel
symbs <- 16
lccolrs <- colr1
if (any(censored)) {
   lcLabels <- c(dLabel, cLabel)
   symbs <- c(16, 1)
   lccolrs <- c(colr1, colr1)
}      
if (eLabel != "") {
   lcLabels <- c(lcLabels, eLabel)
   lccolrs <- c(lccolrs, colr2)
   symbs <- c(symbs, 16)
}
if (e2Label != "") {
   lcLabels <- c(lcLabels, e2Label)
   lccolrs <- c(lccolrs, colr3)
   symbs <- c(symbs, 16)
}
if (e3Label != "") {
   lcLabels <- c(lcLabels, e3Label)
   lccolrs <- c(lccolrs, colr4)
   symbs <- c(symbs, 16)   
}
graphics::legend("topright", legend = lcLabels, pch = symbs, col = lccolrs,
       cex = 0.75, bty = "n", pt.cex = 1.25, y.intersp = 0.75, xpd = NA)
}
