---
title: "APCplot"
author: "Ian Timæus"
date: "23/02/2024"
output: "R graphics plot (can be saved to various formats)"
version: "0.2.1"
---

## Plot age-period-cohort rates on a grid of equilateral triangles using R

### APCplot
Function that tortures Martin Smith's *Ternary* package <https://github.com/ms609/Ternary/> into drawing age-period-cohort (APC) plots of a set of rates (i.e. a Lexis surface) using a triangular grid. It can produce the following plots:
1. Plots of the rates or of the trend in them i.e. the ratios of the rates for successive periods (`change_in_rates = TRUE`).
2. Plot the log measures or the untransformed rates/ratios (`log_rates = FALSE`).

For each of the above options, one can produce:
1. Plots for men and women displayed side-by-side.
2. One plot for both sexes (`sex_specific = FALSE`).
3. One plot showing the ratio of the male:female measures (`sex_differences = TRUE`).

For any of the above options, one can either plot the individual APC rates for each triangle or produce a contour plot (`contour_plot = TRUE`).

As input, APCplot expects a data frame that should include:
1. columns named *age*, *per* and *coh* whose rows cover the universe of possible combinations of values of the integer coordinates (so ranging from 0 to 99 in the case of mortality data).
2. Either columns named *Men* and *Women*, or a single column named *Rates*, containing the rates for each possible combination of *age*, *per* and *coh*.

The data frame should ususally contain *N* observations, where *N* is the square of the length of the age, period, and cohort vectors (so, for mortality, the dataset should have 10,000 observations). However, if one is plotting changes in the rates, one also needs to supply data for the cohort born in the year before the `base_year` as they are required to calculate the rate of change into the base year.

### setupHMDdata
Ancillary function to download and reorganise mortality data from the Human Mortality Database (HMD) at <https://mortality.org> for passing to APCplot. Note that users must first register on the site in order to download the data. (Note also that, if you registered before June 2022, you need to re-register).

The country codes are listed in the data section of the HMD website. Note that not all countries have a century-long run of data and the plotting function has not yet been generalized for use with axes of differing length (e. g. 50 years of data on ages 0 to 99).

### Illustrative plots - Death rates in England and Wales
```
# Import death rates to a data frame from the Human Mortality Database 
Lexis <- setupHMDdata(your_user_id, your_password, country_id = "GBRTENW", base_year = 1922L, length_yrs = 100L)
# Log rates for each sex
APCplot(Lexis, base_year = 1922)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/f5a3b785-010e-4648-b759-5e1d86308de4)

```
# Contour plot of the log rates for the two sexes combined
APCplot(Lexis, base_year = 1922, contour_plot = TRUE, sex_specific = FALSE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/bdcdef6d-3d23-4fa0-a8c0-de039279a610)

```
# Ratios of the men's to the women's rates
APCplot(Lexis, base_year = 1922, sex_differences = TRUE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/f1318902-f2dc-49b2-9e48-7f05e5e96ccc)

```
# Rates of change in the rates for each sex
APCplot(Lexis, base_year = 1922, change_in_rates = TRUE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/69bc5076-553a-4dad-b277-c977894eed8f)


The R scripts were developed in R 4.3.2 under Windows 11.
