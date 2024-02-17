# Plot APC rates properly, using equilateral rather than right-angled triangles.


# APCplot: Function that tortures Martin Smith's Ternary package 
# https://github.com/ms609/Ternary/ into drawing either an age-period-cohort 
# plot of a set of rates (i.e. surface) or a pair of APC plots for men and women
# respectively. It can either plot the individual APC rates for each triangle 
# or produce a contour plot.

# As input, APCplot expects a data frame that should include columns named age, 
# per and coh that cover the universe of possible combinations of values of the 
# integer coordinates (so ranging from 0 to 99 in the case of mortality data).  
# It should also include either columns named Male and Female, or a single
# column named Rates, containing the rates for each possible combination of age,
# per and coh. The data frame should contain N observations, where N is the
# square of the length of the age, period, and cohort vectors (so, for
# mortality, the dataset should have 10,000 observations).  

# Author: Ian Timaeus, 17 Feb 2024. https://github.com/BugBunny/APCplot

APCplot <- function(Rates_lexis,
                    base_year = 0L,
                    length_yrs = 100L,
                    contour_plot = FALSE,
                    sex_specific = TRUE) {
   require("Ternary")
   # Store the rates for a single plot in the format expected by Ternary
   DataFunction <- function(a, b, c) {
      a <- round(a * (length_yrs-1))
      b <- round(b *  (length_yrs-1) + 0.1)
      c <- round(c * (length_yrs-1) - 0.1)
      n_triangles <- length(a)
      work2 <- integer(length = n_triangles)
      for (i in 1:n_triangles) {
         work1 <- with(Rates_lexis, Rates_lexis[[Sex]][age==a[i] & coh==b[i] & 
                                                          per==c[i]])
         # Fill in the dummy data points at the end of rows that Ternary uses 
         # to avoid RHS white space after mapping on to Cartesian coordinates
         if (length(work1) != 0) {
            work2[i] <- work1
         } else {
               work2[i] <- work2[i-1]
         }
      }
      # Plot log rates
      return(log(work2))
   }  # End of function DataFunction
   
   # The period axis runs in reverse order to that on a true ternary plot
   alabels <- list(seq(0, length_yrs, by = length_yrs/10),
      seq(base_year, base_year + length_yrs, by = length_yrs/10),
      seq(base_year + length_yrs, base_year, by = -length_yrs/10))
   par(mar = rep(0.8, 4))
   
   if (sex_specific) {
      # Draw plots for the two Sexes side by side   
      par(mfrow = c(1, 2))
      # Match the spectrum of colours to the range of the rates for each Sex
      # so that the same rates get coloured identically on both plots
      minm <- min(Rates_lexis$Male)
      minf <- min(Rates_lexis$Female)
      maxm <- max(Rates_lexis$Male)
      maxf <- max(Rates_lexis$Female)
      m_end <- 1 - minm /(maxm-minf)
      m_n <- round(256L * m_end)
      f_start  <- 1 - maxf /(maxm-minf)
      f_n <- round(256L * (1 - f_start))     
      list_of_cols <- c("Male", "Female")
   } else {
      minf <- 0.0000454
      maxm <- 1
      f_start <- 0
      f_n <- 256L
      list_of_cols <- "Rates"
   }
   
   for(Sex in list_of_cols) {
      # The two sexes use overlapping subsets of the full colour palette
      if (Sex=="Male") {
         spec = viridisLite::viridis(m_n, alpha = 0.6,
                                     begin = 0, end = m_end, direction = -1)
      } else {
         spec = viridisLite::viridis(f_n, alpha = 0.6,
                                     begin = f_start, end = 1, direction = -1)
      }
      # Plot the grid for the current Sex   
      TernaryPlot(alab="Age", blab="Cohort", clab="Period",
                  axis.labels=alabels, main=Sex)
      # Plot either the individual APC rates or a contour plot
      if (!contour_plot) {
         values <- TernaryPointValues(DataFunction, resolution = length_yrs)
         ColourTernary(values, spectrum = spec)
      } else {   
         values <- TernaryContour(DataFunction, resolution = length_yrs, 
            filled = TRUE, color.palette = function(n) viridisLite::viridis(n, 
            alpha = 0.6, direction = -1)                     )
      }
   }   
   # Add legend to the RHS plot showing the full spectrum used for both Sexes
   if (Sex != "Male") {
      PlotTools::SpectrumLegend(
         "topright",
         legend = signif(seq(log(maxm), log(minf), length.out = 4), 2),
         palette = viridisLite::viridis(256L, alpha = 0.6, direction = -1),
         bty = "n",    # No framing box
         inset = 0.02,
         xpd = NA      # Do not clip at edge of figure
      )  
   }
   par(mfrow = c(1, 1))
   invisible(NULL)
}  # End of function APCplot

# setupHMDdata: Function to download and reorganise mortality data from the 
# Human Mortality Database (HMD) at https://mortality.org for passing to 
# APCplot. Note that users must first register on the site in order to
# download the data (Note also that, if you registered before June 2022, you
# need to re-register).
setupHMDdata <- function(user,
                         password,
                         country_id = "GBRTENW",
                         base_year = 1922L,
                         length_yrs = 100L) {
   # GBRTENW is the country id for the total population of England and Wales. 
   # Other country codes are listed in the data section of the HMD website. Note
   # that not all countries have a century-long run of data and the plotting
   # function has not yet been generalized for use with axes of differing length
   # (e.g. 50 years of data on ages 0 to 99).
   require(HMDHFDplus)
   
   # Download the deaths and exposure counts for the lexis triangles
   Deaths_lexis <- readHMDweb(country_id, "Deaths_lexis", user, password)
   Exposures_lexis <- readHMDweb(country_id, "Exposures_lexis", user, password)
   
   # Calculate the APC death rates by Sex and for the Sexes combined
   Rates_lexis <- Deaths_lexis[ , 1:3]
   Rates_lexis <- cbind(Rates_lexis, Deaths_lexis$Female / 
                           Exposures_lexis$Female)
   colnames(Rates_lexis)[ncol(Rates_lexis)] <- "Female"
   Rates_lexis <- cbind(Rates_lexis, Deaths_lexis$Male / Exposures_lexis$Male)
   colnames(Rates_lexis)[ncol(Rates_lexis)] <- "Male"
   Rates_lexis <- cbind(Rates_lexis, (Deaths_lexis$Male + Deaths_lexis$Female) /
                           (Exposures_lexis$Male + Exposures_lexis$Female))
   colnames(Rates_lexis)[ncol(Rates_lexis)] <- "Rates"
   
   # Compute APC indices that run from 0 upward, reversing the period axis
   Rates_lexis$age <- as.integer(Rates_lexis$Age)
   Rates_lexis$coh <- as.integer(Rates_lexis$Coh) - base_year
   Rates_lexis$per <- as.integer(base_year+length_yrs-1 - Rates_lexis$Year)
   Rates_lexis <- Rates_lexis[Rates_lexis$age>=0 & Rates_lexis$age<length_yrs, ]
   Rates_lexis <- Rates_lexis[Rates_lexis$coh>=0 & Rates_lexis$coh<length_yrs, ]
   Rates_lexis <- Rates_lexis[Rates_lexis$per>=0 & Rates_lexis$per<length_yrs, ]
   Rates_lexis <- na.omit(Rates_lexis)
   return(Rates_lexis)
}  # End of function setupHMDdata
   
# Function calls to illustrate the various types of plot
Rates_lexis <- setupHMDdata(user = "", password = "")
APCplot(Rates_lexis, base_year = 1922)
APCplot(Rates_lexis, base_year = 1922, contour_plot = T, sex_specific = F)
