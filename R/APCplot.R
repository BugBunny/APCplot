#' Plot age-period-cohort Lexis surfaces 
#' 
#' @description
#' `APCplot` produces age-period-cohort plots of Lexis surfaces (either rates or
#' rate ratios) on a grid of equilateral triangles.
#' @details
#' The Lexis surfaces can comprise rates or trends in rates (ratios of rates for 
#' successive periods) and either be logged or not. `APCplot` can produce plots 
#' for two sub-groups side-by-side (e.g. men and women), a single plot for the 
#' full data, or plots of the sub-group rate ratios. Each of these types of plot
#' can be drawn either for the individual APC triangles or as a contour plot.
#' 
#' The function is implemented by torturing Martin Smith's [Ternary] package
#' into drawing APC surfaces.
#' 
#' @param Lexis Data frame with columns for age, per, coh and each set of rates. 
#' The first 3 columns index the coordinates of the rates contained in the other 
#' columns. Store the rates for the total population in a column named "Rates". 
#' [setupHMDdata] prepares Lexis objects from the HMD data for a country.
#' @param base_year Initial birth cohort
#' @param length_yrs Number of cohorts required
#' @param label_interval Interval in years between axis labels
#' @param contour_plot Draw a contour plot
#' @param group_specific Plot sub-groups (e.g. men and women) separately
#' @param sub_groups Names of the sub-groups. These should match the column
#' names used for each sub-group in the data frame
#' @param group_ratios Plot the ratio of the rates in a pair of sub-groups
#' @param change_in_rates Plot the ratios of successive rates
#' @param log_rates Plot the logged rates or rate ratios
#' @return R graphics plot that can be exported to multiple graphics formats.
#' @export
#' @examples
#' # Check whether there are any APC data in the working directory to be plotted
#' errmsg <- "Create a Lexis object with setupHMDdata before running this code"
#' try(if (file.exists("Lexis_GBRTENW.rda")==FALSE) stop(errmsg, call. = FALSE))
#' load ("Lexis_GBRTENW.rda")
#' # Log rates for each sex
#' APCplot(Lexis, base_year = 1922)
#' # Contour plot of the log rates for the two sexes combined
#' APCplot(Lexis, base_year = 1922, contour_plot = TRUE, group_specific = FALSE)
#' # Ratios of the men's to the women's rates
#' APCplot(Lexis, base_year = 1922, group_ratios = TRUE, log_rates = FALSE)
#' # Rates of change in the rates for each sex
#' APCplot(Lexis, base_year = 1922, change_in_rates = TRUE)
APCplot <- function(Lexis,
                    base_year = 0L,
                    length_yrs = 100L,
                    label_interval = 10L,
                    contour_plot = FALSE,
                    group_specific = TRUE,
                    sub_groups = c("Men", "Women"),
                    group_ratios = FALSE,
                    change_in_rates = FALSE,
                    log_rates = TRUE)  {
   # Store the rates for a single plot in the format expected by Ternary
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
      for (i in 1:n_triangles) if (c[i] >= 0) {
         # Look up rates for this group in Lexis
         work[i] <- with(Lexis, Lexis[age==a[i] & coh==b[i] & per==c[i], group])
         work[i] <- ifelse(log_rates, log(work[i]), work[i])
         # For odd ages on contour plots, interpolate to offset by 0.5 of a year
         if (contour_plot & (a[i] %% 2 != 0) & (b[i] > 1)) {
            work[i-1] <- (work[i-1] + work[i-2]) / 2
         }
      } else {
         work[i] <- work[i-1]
      }      
      return(work)
   }  # End of function LookUpRates
   
   N <- length(sub_groups)
   if (N > 360) return("Error in APCplot() : unable to plot >360 sub-groups")
   # Set rates of zero to a small +ve value if logging or calculating ratios
   cols_rates <- if ("Rates" %in% colnames(Lexis)) 
      c(sub_groups, "Rates") else sub_groups
   if (log_rates | group_ratios | change_in_rates) {
      avoid_zeros <- function(x) replace(x, x == 0, 0.000001)
      Lexis[cols_rates] <- sapply(Lexis[cols_rates], avoid_zeros)
   }
   # Calculate ratios of successive rates if required
   if (change_in_rates) {
      # Sort by period, within up and then down triangles, within age
      Lexis <- with(Lexis, Lexis[order(age, coh + per, per, 
         decreasing = c(F, T, T)), ])
      # Calculate ratio of current to previous rate for all 3 columns of rates
      period_ratios <- function(x) {
         x <- x / c(-1, x[1:length(x) - 1])
         # Discard tails so there's more visible variation to look at
         if (!contour_plot) {
            left <- stats::quantile(x, 0.01, na.rm = TRUE)
            right <- stats::quantile(x, 0.99, na.rm = TRUE)
            x <- replace(replace(x, x<left, left), x>right, right)
         }
         return(x)
      }
      Lexis[cols_rates] <- sapply(Lexis[cols_rates], period_ratios)
   }
   Lexis <- Lexis[Lexis[ , "coh"] >= 0, ]
   
   # To plot inter-group differences, store the rate ratios in Rates
   if (group_ratios) {
      if (N > 2) return("Error in APCplot() : for group ratios, supply 2 names")
      Lexis[ , "Rates"] <- Lexis[ , sub_groups[1]] / Lexis[ , sub_groups[2]]
      # Discard tails so there's more visible variation to look at
      if (!contour_plot) {
         left <- stats::quantile(Lexis[ , "Rates"], 0.005, na.rm = TRUE)
         right <- stats::quantile(Lexis[ , "Rates"], 0.995, na.rm = TRUE)
         vtrim <- function(x) replace(replace(x, x<left, left), x>right, right)
         Lexis[ , "Rates"] <- vtrim(Lexis[ , "Rates"])
      }
      group_specific <- FALSE
   }
    
   # The period axis should run in reverse order to that on a true ternary plot
   lexis_labels <- list(seq(0, length_yrs, by = length_yrs/label_interval),
      seq(base_year, base_year + length_yrs, by = length_yrs/label_interval),
      seq(base_year + length_yrs, base_year, by = -length_yrs/label_interval))
   graphics::par(mar = rep(0.8, 4))
   

   if (group_specific) {
      # Calculate the dimensions of the grid of plots
      g_rows <- 12
      cut_points <- c(297, 250, 198, 160, 119, 90, 60, 40, 21, 12, 3)
      for (i in 11:1) if (cut_points[i] >= N) g_rows <- i
      g_cols <- ceiling(N / g_rows)
      on.exit(graphics::par(mfrow = c(1, 1)))
      graphics::par(mfrow = c(g_rows, g_cols))
      # Match spectrum of colours to the range of the rates for each sub-group
      # so that the same rates get coloured identically on both plots
      minz <- integer(length = 0)
      maxz <- integer(length = 0)
      mint <- 1
      maxt <- 0
      for (group in sub_groups) {
         minz[group] <- min(Lexis[ , group]) 
         maxz[group] <- max(Lexis[ , group])
         mint <- ifelse(minz[group] < mint, minz[group], mint)
         maxt <- ifelse(maxz[group] > maxt, maxz[group], maxt)
         if (log_rates) minz[group] <- log(minz[group])
         if (log_rates) maxz[group] <- log(maxz[group])
      }
      if (log_rates) mint <- log(mint)
      if (log_rates) maxt <- log(maxt) 
      drange <- maxt - mint
      gp_start <- integer(length = 0)
      gp_end <- integer(length = 0)
      gp_n <- integer(length = 0)
      for (group in sub_groups) {
         gp_start[group] <- 1 - (maxz[group] - mint) / drange
         gp_end[group] <- 1 - (minz[group] - mint) / drange
         gp_n[group] <- round(256L * (gp_end[group] - gp_start[group]))
      }
   } else {
      sub_groups <- "Rates"      
      mint <- min(Lexis[, "Rates"]) 
      maxt <- max(Lexis[, "Rates"])
      if (log_rates) {mint <- log(mint); maxt <- log(maxt)}
      gp_start["Rates"] <- c(0)
      gp_end["Rates"] <- c(1)
      gp_n["Rates"] <- c(256L)
   }
   for(group in sub_groups) {
      # Assemble plot heading
      logged <- ifelse(log_rates, "Log", "")
      gp_ratios <- ifelse(group_ratios, " Ratios of the", "")
      deltas <- ifelse(change_in_rates, " Changes in the", "")
      plot_title <- paste(logged, gp_ratios, deltas, group)
      # Plot the grid for the current group 
      TernaryPlot(alab = "Age", blab = "Cohort", clab = "Period",
                  axis.labels = lexis_labels, main = plot_title)
      
      # Plot either the individual APC rates or a contour plot
      if (!contour_plot) {
         mySpectrum <- viridisLite::viridis(gp_n[group], alpha = 0.6,
               begin = gp_start[group], end = gp_end[group], direction = -1)
         values <- TernaryPointValues(LookUpRates, resolution = length_yrs)
         ColourTernary(values, spectrum = mySpectrum)
      } else {   
         values <- TernaryContour(LookUpRates, resolution = length_yrs, 
            filled = TRUE, color.palette = function(n) viridisLite::viridis(n, 
            alpha = 0.6, direction = -1))
      }
   }   
   # Add legend to the RHS plot showing the range of the rates across all groups
   PlotTools::SpectrumLegend(
      "topright",
      legend = signif(seq(from = maxt, to = mint, length.out = 5), 2),
      palette = viridisLite::viridis(256L, alpha = 0.6, direction = -1),
      lwd = 20L,
      bty = "n",    # No framing box
      inset = 0.02,
      xpd = NA      # Do not clip at edge of figure
   )  
   invisible(values)
}  # End of function APCplot