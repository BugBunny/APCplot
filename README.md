---
title: "APCplot"
author: "Ian Timæus"
date: "17/02/2024"
output: "R graphics plot (can be saved to various formats)"
---

## Plot age-period-cohort rates on a grid of equilateral triangles using R

### APCplot
Function that tortures Martin Smith's *Ternary* package <https://github.com/ms609/Ternary/> into drawing either an age-period-cohort plot of a set of rates (i.e. surface) or a pair of APC plots for men and women respectively. It can either plot the individual APC rates triangle by triangle or produce a contour plot.

As input, APCplot expects a data frame that should include columns named *age*, *per* and *coh* that cover the universe of possible combinations of values of the integer coordinates (so ranging from 0 to 99 in the case of mortality data). It should also include either columns named *Male* and *Female*, or a single column named *Rates*, containing the rates for each possible combination of *age*, *per* and *coh*. The data frame should contain *N* observations, where *N* is the square of the length of the age, period, and cohort vectors (so, for mortality, the dataset should have 10,000 observations).

### setupHMDdata
Ancillary function to download and reorganise the mortality data from the Human Mortality Database (HMD) at <https://mortality.org> for passing to APCplot. Note that users must first register on the site in order to download the data. (Note also that, if you registered before June 2022, you need to re-register).

The country codes are listed in the data section of the HMD website. Note that not all countries have a century-long run of data and the plotting function has not yet been generalized for use with axes of differing length (e. g. 50 years of data on ages 0 to 99).

### Vignette - Death rates in England and Wales
```
Rates_lexis <- setupHMDdata(your_user_id, your_password, country_id = "GBRTENW", base_year = 1922L, length_yrs = 100L)
APCplot(Rates_lexis, base_year = 1922)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/09f05fb7-baf6-478c-b76e-6605416a6b31)

```
 APCplot(Rates_lexis, base_year = 1922, contour_plot = TRUE, sex_specific = FALSE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/8a36c6dd-7549-40c1-b5a7-fb46c82364d1)

The R scripts were developed in R 4.3.2 under Windows 11.
