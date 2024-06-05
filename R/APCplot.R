#' Plot age-period-cohort Lexis surfaces 
#' 
#' @description
#' `APCplot` produces age-period-cohort plots of Lexis surfaces (either rates or
#' rate ratios) on a grid of equilateral triangles.
#' @details
#' The Lexis surfaces can comprise rates or trends in rates (ratios of rates for 
#' successive periods) and either be logged or not. `APCplot` can produce plots 
#' for several sub-groups on a grid (e.g. men and women), a single plot of the 
#' full data, or plots of the ratio of the rates in a pair of sub-groups. Each 
#' of these types of plot can be drawn either for the individual APC triangles
#' or as a contour plot.
#' 
#' The function is implemented by torturing Martin Smith's [Ternary] package
#' into drawing APC surfaces.
#' 
#' @param Lexis Data frame with columns for age, per, coh and each set of rates. 
#' The first 3 columns index the coordinates of the rates contained in the other 
#' columns. Store the rates for the total population in a column named "Rates". 
#' [APCmortHMD] prepares and plots Lexis mortality surfaces using HMD data
#' [APCfertDHS] prepares and plots Lexis fertility surfaces from a DHS survey
#' @param base_year Initial birth cohort
#' @param survey_year Year when retrospective data were collected
#' @param base_age Youngest age supplying data 
#' @param length_yrs Number of cohorts required
#' @param contour_plot Draw a contour plot
#' @param group_specific Plot sub-groups (e.g. men and women) separately
#' @param total Add a plot for the total population to plots for sub-groups 
#' @param group_ratios Plot the ratio of the rates in a pair of sub-groups
#' @param period_ratios Plot the ratios of successive rates
#' @param log_rates Plot the logged rates or rate ratios
#' @param pct_trim Percentage of rates to trim from each extreme of the range 
#' @return R graphics plot that can be exported to multiple graphics formats.
#' @export
#' @examples
#' ## Load a Lexis data frame of death rates by age, period and cohort
#' data(Lexis)
#' ## Log rates for each sex
#' APCplot(Lexis, base_year = 1922)
#' ## Contour plot of the log rates for the two sexes combined
#' APCplot(Lexis, base_year = 1922, contour_plot = TRUE, group_specific = FALSE)
#' ## Ratios of the men's to the women's rates
#' APCplot(Lexis, base_year = 1922, group_ratios = TRUE, log_rates = FALSE)
#' ## Rates of change in the rates for each sex
#' APCplot(Lexis, base_year = 1922, period_ratios = TRUE)
APCplot <- function(Lexis,
                    base_year = 0L,
                    survey_year = 0L,
                    base_age = 0L,
                    length_yrs = 100L,
                    contour_plot = FALSE,
                    group_specific = TRUE,
                    total = NULL,
                    group_ratios = FALSE,
                    period_ratios = FALSE,
                    log_rates = TRUE,
                    pct_trim = 0.5) {
   
## Store the rates for a single plot in the format expected by Ternary
LookUpRates <- function(a, b, c) {
   if (contour_plot) {
      a <- round(a * (length_yrs - 1))
      b <- round(b * (length_yrs - 1) + 0.1)
      c <- round(c * (length_yrs - 1) - 0.1)
   } else {
      env <- environment()
      for (i in c("a", "b", "c")) {
         env[[i]] <- floor(env[[i]] * length_yrs)
      }   
   }
   n_triangles <- length(a)
   work <- integer(length = n_triangles)
   for (i in 1:n_triangles) if (c[i] >= 0L) {
      ## Look up rates for this group in Lexis
      work[i] <- with(Lexis, Lexis[age==a[i] & coh==b[i] & per==c[i], group])
      work[i] <- ifelse(log_rates, log(work[i]), work[i])
      ## For odd ages on contour plots, interpolate to offset by 0.5 of a year
      if (contour_plot & (a[i] %% 2 != 0) & (b[i] > 1L)) {
         work[i-1] <- (work[i-1] + work[i-2]) / 2
      }
   } else {
      ## Set the year after last to the last one to get the final contour points
      work[i] <- work[i-1]
   } 
   return(work)
}  ## End of function LookUpRates

## Calculate N of Lexis surfaces to be plotted
N <- length(Lexis) - 3L
if (N > 360L) stop("Unable to plot >360 sub-groups")   
if (N == 1) {
   group_specific <- FALSE
} 
if (group_ratios) {
   if (N != 3L) {
      stop("To plot sub-group ratios, supply data on exactly TWO sub-groups") 
   } else {   
      group_specific <- FALSE
      N <- 1   
      sub_groups <- names(Lexis)[4:5]
   }   
} else {
   if ( ! group_specific) {
      N <- 1      
      sub_groups <- "Rates"
   } else {
      ## Default is to plot overall rates except for a pair of sub-groups
      if (is.null(total)) total <- if (N > 3L) TRUE else FALSE
      N <- N - !total
      sub_groups <- names(Lexis)[4:(3+N)]
   }
}

## Set rates of zero to a small +ve value if logging or calculating ratios
if (log_rates | group_ratios | period_ratios) {
   avoid_zeros <- function(x) replace(x, x == 0, 0.000001)
   Lexis[sub_groups] <- sapply(Lexis[sub_groups], avoid_zeros)
}
## Sort by period, within the up and then the down triangles, within age
Lexis <- with(Lexis, Lexis[order(age, coh + per, per, 
   decreasing = c(FALSE, TRUE, TRUE)), ])
## Calculate ratios of successive rates if required
if (period_ratios) Lexis[sub_groups] <- sapply(Lexis[sub_groups], function(x) {      
   x[2:length(x)] <- x[2:length(x)] / x[1:length(x) - 1L]
   ## If no data on cohort -1, set change into cohort 0 to change into cohort 1
   if (min(Lexis$coh)==0) {
      cohort1 <- row(as.matrix(x))[Lexis["coh"]==0]
      cohort1 <- cohort1[-length(cohort1)]
      x[cohort1] <- x[cohort1 + 1]
      x[length(x)] <- x[length(x) - 1]
   }
   return(x)
})
Lexis <- Lexis[Lexis[ , "coh"] >= 0L, ]
## To plot inter-group differences, store the rate ratios in Rates
if (group_ratios) {
   Lexis[ , "Rates"] <- Lexis[ , 4] / Lexis[ , 5] 
   sub_groups <- "Rates"
}   
## Trim tails to stop a few outliers soaking up most of the spectrum of colours
if ( ! contour_plot) {
   pct_trim <- pct_trim/100
   Lexis[sub_groups] <- sapply(Lexis[sub_groups], function(gp) {
      left <- stats::quantile(gp, pct_trim, na.rm = TRUE)
      right <- stats::quantile(gp, 1 - pct_trim, na.rm = TRUE)      
      sapply(gp, function(x) 
         x <- replace(replace(x, x<left, left), x>right, right))
   })
} 
## Axis labels: Either the period or cohort axis is reversed in a ternary plot
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
graphics::par(mar = rep(0.8, 4))

gp_start <- integer(length = 0)
gp_end <- integer(length = 0)
gp_n <- integer(length = 0)
if (group_specific) {
   ## Calculate the dimensions of the grid of plots
   g_rows <- 12
   cut_points <- c(297, 250, 198, 160, 119, 90, 60, 40, 21, 12, 3)
   for (i in 1:11) if (cut_points[i] >= N) g_rows <- 12L - i
   g_cols <- ceiling(N / g_rows)
   on.exit(graphics::par(mfrow = c(1, 1)))
   graphics::par(mfrow = c(g_rows, g_cols))
   ## Establish a common colour spectrum for all the plots
   minz <- integer(length = 0)
   maxz <- integer(length = 0)
   mint <- 99
   maxt <- 0
   for (group in sub_groups) {
      minz[group] <- min(Lexis[ , group]) 
      maxz[group] <- max(Lexis[ , group])
      if (minz[group] < mint) mint <- minz[group]
      if (maxz[group] > maxt) maxt <- maxz[group]
      if (log_rates) {
         minz[group] <- log(minz[group])
         maxz[group] <- log(maxz[group])
      } 

   }
   if (log_rates) {mint <- log(mint); maxt <- log(maxt)}
   drange <- maxt - mint
   for (group in sub_groups) {
      gp_start[group] <- 1 - (maxz[group] - mint) / drange
      gp_end[group] <- 1 - (minz[group] - mint) / drange
      gp_n[group] <- ceiling(256 * (gp_end[group] - gp_start[group]))
   }
} else {
   sub_groups <- "Rates"      
   mint <- min(Lexis[, "Rates"]) 
   maxt <- max(Lexis[, "Rates"])
   if (log_rates) {mint <- log(mint); maxt <- log(maxt)}
   gp_start["Rates"] <- c(0L)
   gp_end["Rates"] <- c(1L)
   gp_n["Rates"] <- c(256L)
}

for(group in sub_groups) {
   ## Assemble heading for current group
   logged <- ifelse(log_rates, "Log", "")
   gp_ratios <- ifelse(group_ratios, " Ratios of the", "")
   deltas <- ifelse(period_ratios, " Changes in the", "")
   plot_title <- paste(logged, gp_ratios, deltas, group)
   if (group_specific) {
      if (total & group == "Rates") {
         plot_title <- paste(logged, gp_ratios, deltas, "total")  
      }   
   }
   ## Plot the grid for the current group
   rgn <- list(min = c(0, 0, length_yrs), max = c(length_yrs, length_yrs, 0))
   TernaryPlot(alab = "Age", blab = "Cohort", clab = "Period", region = rgn,
      grid.lines = ceiling(length_yrs / intvl), axis.labels = lexis_labels, 
      main = plot_title)
   ## Plot either the individual APC rates or a contour plot
   if ( ! contour_plot) {
      mySpectrum <- viridisLite::viridis(gp_n[group], alpha = 0.6,
            begin = gp_start[group], end = gp_end[group], direction = -1L)
      values <- TernaryPointValues(LookUpRates, resolution = length_yrs)
      ColourTernary(values, spectrum = mySpectrum)
   } else {   
      values <- TernaryContour(LookUpRates, resolution = length_yrs, 
         filled = TRUE, color.palette = function(n) viridisLite::viridis(n, 
         alpha = 0.6, direction = -1))
   }
}   
## Add a legend showing the common colour scale for all groups to the RHS plot 
PlotTools::SpectrumLegend(
   "topright",
   legend = signif(seq(from = maxt, to = mint, length.out = 5), 2),
   palette = viridisLite::viridis(256L, alpha = 0.6, direction = -1),
   lwd = 20L,
   bty = "n",    # No framing box
   inset = 0.00,
   xpd = NA      # Do not clip at edge of figure
)  
invisible(values)
}  ## End of function APCplot