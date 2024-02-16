# Plot APC rates properly onto equilateral rather than right-angled triangles.
#
library(HMDHFDplus)
# Download mortality data from the Human Mortality Database (HMD) at
# https://mortality.org Note that users are required to pre-register on the site
# in order to download the data (Note also that, if you registered before June
# 2022, you need to re-register).
user <- "ian.timaeus@lshtm.ac.uk"
pw <- "178Herne!"
# This is the country code for the total population of England and Wales. Other
# country codes are specified. Note that not all countries have a century long
# run of data and the plotting function has not yet been generalised for use on
# shorter or longer data series. 
country_id <- "GBRTENW"
Base_year <- 1922L
len <- 100L
# Import the deaths and exposure counts for the lexis triangles
Deaths_lexis <- readHMDweb(country_id, "Deaths_lexis", user, pw)
Exposures_lexis <- readHMDweb(country_id, "Exposures_lexis", user, pw)
# Calculate the APC death rates
Rates_lexis <- Deaths_lexis[ , 1:3]
Rates_lexis <- cbind(Rates_lexis, Deaths_lexis$Female / Exposures_lexis$Female)
colnames(Rates_lexis)[ncol(Rates_lexis)] <- "Female"
Rates_lexis <- cbind(Rates_lexis, Deaths_lexis$Male / Exposures_lexis$Male)
colnames(Rates_lexis)[ncol(Rates_lexis)] <- "Male"
# Convert APC measures to ranges 0:99, reversing the period axis
Rates_lexis$age <- as.integer(Rates_lexis$Age)
Rates_lexis$coh <- as.integer(Rates_lexis$Coh) - Base_year
Rates_lexis$per <- as.integer(Base_year+len-1 - Rates_lexis$Year)
Rates_lexis <- na.omit(Rates_lexis)
Rates_lexis <- Rates_lexis[Rates_lexis$age>=0 & Rates_lexis$age<len, ]
Rates_lexis <- Rates_lexis[Rates_lexis$coh>=0 & Rates_lexis$coh<len, ]
Rates_lexis <- Rates_lexis[Rates_lexis$per>=0 & Rates_lexis$per<len, ]
#
# APCplot: Function that tortures Martin Smith's Ternary package 
# https://github.com/ms609/Ternary/ into drawing a pair of age-period-cohort 
# plots of sets of rates for men and women respectively. It can plot either 
# the individual APC rates or a contour plot.
# 
# As input, APCplot expects a data frame with 10,000 observations. This should
# include columns named age, per and coh that cover the universe of possible 
# combinations of values of the coordinates, each ranging from 0 to 99. It  
# should also include columns named Male and Female that contain the rates for
# each possible combination of age, per and coh. 
#
# Author: Ian Timaeus, 15 Feb 2024. https://github.com/BugBunny/APCplot
#
APCplot <- function(Rates_lexis,
                    base_yr = 0L,
                    length_yrs = 100L,
                    contour_plot = FALSE) {
   require("Ternary")
   # Match the spectrum of colours to the range of the rates for each sex so
   # that the same rates get coloured identically on both plots
   minm <- min(Rates_lexis$Male)
   minf <- min(Rates_lexis$Female)
   maxm <- max(Rates_lexis$Male)
   maxf <- max(Rates_lexis$Female)
   m_end <- 1 - minm /(maxm-minf)
   m_n <- round(256 * m_end)
   f_start  <- 1 - maxf /(maxm-minf)
   f_n <- round(256 * (1 - f_start))
   # Store the rates for a specific sex in the format expected by Ternary
   DataFunction <- function(a, b, c) {
      a <- round(a * (length_yrs-1))
      b <- round(b *  (length_yrs-1) + 0.1)
      c <- round(c * (length_yrs-1) - 0.1)
      n_triangles <- length(a)
      work2 <- integer(length = n_triangles)
      for (i in 1:n_triangles) {
         work1 <- with(Rates_lexis, Rates_lexis[[Sex]][age==a[i] & coh==b[i] & 
                                                          per==c[i]])
         # Fill in dummy data points atthe  end of rows that Ternary uses to 
         # avoid RHS white space after mapping on to Cartesian coordinates
         if (length(work1) != 0) {
            work2[i] <- work1
         } else {
               work2[i] <- work2[i-1]
         }
      }
      # Plot log rates
      log(work2)
   } 
   # The period axis runs in reverse order to that on a true ternary plot
   alabels <- list(seq(0, length_yrs, by = length_yrs/10),
      seq(base_yr, base_yr+length_yrs, by = length_yrs/10),
      seq(base_yr+length_yrs, base_yr, by = -length_yrs/10))
   # Draw the plots for the two sexes side by side
   par(mfrow = c(1, 2), mar = rep(0.3, 4))
   for(Sex in c("Male", "Female")) {
      # The two sexes use overlapping subsets of the full colour palette
      if (Sex=="Male") {
         spec = viridisLite::viridis(m_n, alpha = 0.6,
                                     begin = 0, end = m_end, direction = -1)
      } else {
         spec = viridisLite::viridis(f_n, alpha = 0.6,
                                     begin = f_start, end = 1, direction = -1)
      }
      # Plot the grid for thie current sex   
      TernaryPlot(alab="Age", blab="Cohort", clab="Period",
                  axis.labels=alabels, main=Sex)
      # Plot either the individual APC rates or a smoothed contour plot
      if (!contour_plot) {
         values <- TernaryPointValues(DataFunction, resolution = len)
         ColourTernary(values, spectrum = spec)
      } else {   
         values <- TernaryContour(DataFunction, resolution = len, filled = TRUE,
            color.palette = function(n) viridisLite::viridis(n, alpha = 0.6,
                                                             direction = -1)                     )
      }
      # Add legend to the RHS plot showing the full spectrum used for both sexes
      if (Sex=="Female") {
         PlotTools::SpectrumLegend(
            "topright",
            legend = signif(seq(log(maxm), log(minf), length.out = 4), 2),
            palette = viridisLite::viridis(256L, alpha = 0.6, direction = -1),
            bty = "n",    # No framing box
            inset = 0.02,
            xpd = NA      # Do not clip at edge of figure
          )  
      }   
   }
}
# Function calls to illustrate the tw type of plot
APCplot(Rates_lexis, base_yr = Base_year)
APCplot(Rates_lexis, base_yr = Base_year, contour_plot = TRUE)