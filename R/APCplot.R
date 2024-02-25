#' Plot age-period-cohort Lexis surfaces 
#' 
#' @description
#' `APCplot` produces age-period-cohort plots of Lexis surfaces (either rates or
#' rate ratios) on a grid of equilateral triangles.
#' @details
#' The Lexis surfaces can comprise rates or trends in rates (ratios of rates for 
#' successive periods) and either be logged or not. `APCplot` can produce plots 
#' for Men and Women side-by-side, a single plot for both sexes, or plots of the 
#' Men's:Women's rate ratios. Each of these types of plot can be drawn 
#' either for the individual APC triangles or as a contour plot.
#' 
#' The function is implemented by torturing Martin Smith's [Ternary] package
#' into drawing APC surfaces.
#' 
#' @param Lexis Data frame with the columns age, per, coh, Men, Women, Rates. 
#' The first 3 columns index the coordinates of the rates contained in the other 
#' 3 columns. [setupHMDdata] prepares Lexis objects from the HMD data for a 
#' country.
#' @param base_year Initial birth cohort
#' @param length_yrs Number of cohorts required
#' @param label_interval Interval in years between axis labels
#' @param contour_plot Draw a contour plot
#' @param sex_specific Plot men and women separately
#' @param sex_differences Plot the men:women rate ratios
#' @param change_in_rates Plot the ratios of successive rates
#' @param log_rates Plot the log rates or rate ratios
#' @return R graphics plot that can be exported to multiple graphics formats.
#' @export
#' @examples
#' # Check whether APC data exist to be plotted
#' fn <- "Lexis_GBRTENW.rda"
#' errmsg <- "Create a Lexis object with setupHMDdata before running this code"
#' try(if (file.exists(fn)==F) stop(errmsg, call. = F))
#' load (fn)
#' # Log rates for each sex
#' APCplot(Lexis, base_year = 1922)
#' # Contour plot of the log rates for the two sexes combined
#' APCplot(Lexis, base_year = 1922, contour_plot = TRUE, sex_specific = FALSE)
#' # Ratios of the men's to the women's rates
#' APCplot(Lexis, base_year = 1922, sex_differences = TRUE, log_rates = FALSE)
#' # Rates of change in the rates for each sex
#' APCplot(Lexis, base_year = 1922, change_in_rates = TRUE)
APCplot <- function(Lexis,
                    base_year = 0L,
                    length_yrs = 100L,
                    label_interval = 10L,
                    contour_plot = FALSE,
                    sex_specific = TRUE,
                    sex_differences = FALSE,
                    change_in_rates = FALSE,
                    log_rates = TRUE) {
   # Store the rates for a single plot in the format expected by Ternary
   LookUpRates <- function(a, b, c) {
     env <- environment()
     if (contour_plot) {
        a <- round(a * (length_yrs - 1))
        b <- round(b * (length_yrs - 1) + 0.1)
        c <- round(c * (length_yrs - 1) - 0.1)
      } else {
         for (i in c("a", "b", "c")) env[[i]] <- floor(env[[i]] * length_yrs)
      }
      n_triangles <- length(a)
      workvec <- integer(length = n_triangles)
      for (i in 1:n_triangles) {
         work <- with(Lexis, Lexis[age==a[i] & coh==b[i] & per==c[i], sex])
         # Log the rates if required, avoiding infinite values
         workvec[i] <- ifelse(log_rates, log(ifelse(work==0, 0.000001, work)),
            work)
         # For odd ages on contour plots, interpolate to offset by 0.5 of a year
         if (contour_plot & ((a[i] %% 2) != 0)) {
            if (c[i] < 0) 
               workvec[i] <- workvec[i-1]
            else
               if (b[i] > 0) workvec[i] <- (workvec[i-1] + workvec[i]) / 2
         }
      }      
      return(workvec)
   }  # End of function LookUpRates
   
   # Calculate ratios of successive rates if required
   if (change_in_rates) {
      # Sort by period, within up and then down triangles, within age
      Lexis <- with(Lexis, Lexis[order(age, coh + per, per, 
         decreasing = c(F, T, T)), ])
      # Calculate ratio of current to previous rate for all 3 columns of rates
      period_ratios <- function(x) {
         x <- x / c(-1, x[1:length(x) - 1])
         # Discard tails so there's more visible variation to look at
         left <- stats::quantile(x, 0.01)
         right <- stats::quantile(x, 0.99)
         sapply(x, function(x) ifelse(x<left, left, ifelse(x>right, right, x)))
      }
      columns_of_rates <- c("Men", "Women", "Rates")
      Lexis[columns_of_rates] <- lapply(Lexis[columns_of_rates], period_ratios)
   }
   Lexis <- Lexis[Lexis$coh >= 0, ]
   # To plot sex differences, store the male:female ratios of the rates in Rates
   if (sex_differences) {
      Lexis$Rates <- Lexis$Men / Lexis$Women
      # Discard tails so there's more visible variation to look at
      left <- stats::quantile(Lexis$Rates, 0.005)
      right <- stats::quantile(Lexis$Rates, 0.995)
      trunc_rates <- function(x) ifelse(x<left, left, ifelse(x>right, right, x))
      Lexis$Rates <- sapply(Lexis$Rates, trunc_rates) 
      sex_specific <- FALSE
   }
    
   # The period axis should run in reverse order to that on a true ternary plot
   lexis_labels <- list(seq(0, length_yrs, by = length_yrs/label_interval),
      seq(base_year, base_year + length_yrs, by = length_yrs/label_interval),
      seq(base_year + length_yrs, base_year, by = -length_yrs/label_interval))
   graphics::par(mar = rep(0.8, 4))
   
   env <- environment()
   if (sex_specific) {
      # Draw plots for the two Sexes side by side   
      graphics::par(mfrow = c(1, 2))
      # Match the spectrum of colours to the range of the rates for each Sex
      # so that the same rates get coloured identically on both plots
      mint <- min(Lexis$Men, Lexis$Women) + 0.000001
      maxt <- max(Lexis$Men, Lexis$Women)
      minm <- min(Lexis$Men) + 0.000001
      maxm <- max(Lexis$Men)
      minf <- min(Lexis$Women) + 0.000001
      maxf <- max(Lexis$Women)
      minimax <- c("mint", "maxt", "minm", "maxm", "minf", "maxf")
      if (log_rates) for (item in minimax) env[[item]] <- log(env[[item]])
      d_range <- (maxt - mint)
      m_start <- 1 - (maxm - mint) / d_range
      m_end <- 1 - (minm - mint) / d_range
      m_n <- round(256L * (m_end - m_start))
      f_start  <- 1 - (maxf - mint) / d_range
      f_end <- 1 - (minf - mint) / d_range
      f_n <- round(256L * (f_end - f_start))  
      columns_to_plot <- c("Men", "Women")
   } else {
      mint <- min(Lexis$Rates) + 0.000001
      maxt <- max(Lexis$Rates)
      if (log_rates) {mint <- log(mint); maxt <- log(maxt)}
      f_start <- 0
      f_end <- 1
      f_n <- 256L
      columns_to_plot <- "Rates"
   }
   
   for(sex in columns_to_plot) {
      # The two sexes use overlapping subsets of the full colour palette
      if (sex=="Men") {
         mySpectrum = viridisLite::viridis(m_n, alpha = 0.6,
            begin = m_start, end = m_end, direction = -1)
      } else {
         mySpectrum = viridisLite::viridis(f_n, alpha = 0.6, 
            begin = f_start, end = f_end, direction = -1)
      }
      # Assemble plot heading and plot the grid for the current Sex 
      logged <- ifelse(log_rates, "Log", "")
      s_diffs <- ifelse(sex_differences, " Sex Differences in the", "")
      deltas <- ifelse(change_in_rates, " Changes in the", "")
      plot_title <- paste(logged, s_diffs, deltas, sex)
      TernaryPlot(alab = "Age", blab = "Cohort", clab = "Period",
                  axis.labels = lexis_labels, main = plot_title)
      # Plot either the individual APC rates or a contour plot
      if (!contour_plot) {
         values <- TernaryPointValues(LookUpRates, resolution = length_yrs)
         ColourTernary(values, spectrum = mySpectrum)
      } else {   
         values <- TernaryContour(LookUpRates, resolution = length_yrs, 
            filled = TRUE, color.palette = function(n) viridisLite::viridis(n, 
            alpha = 0.6, direction = -1))
      }
   }   
   # Add legend to the RHS plot showing the full spectrum used for both Sexes
   if (sex != "Men") {
      PlotTools::SpectrumLegend(
         "topright",
         legend = signif(seq(from = maxt, to = mint, length.out = 5), 2),
         palette = viridisLite::viridis(256L, alpha = 0.6, direction = -1),
         lwd = 20L,
         bty = "n",    # No framing box
         inset = 0.02,
         xpd = NA      # Do not clip at edge of figure
      )  
   }
   graphics::par(mfrow = c(1, 1))
   invisible(values)
}  # End of function APCplot