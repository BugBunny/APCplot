---
title: "APCplot"
author: "Ian Timæus"
date: "3/03/2024"
output: "R graphics plot (can be saved to various formats)"
version: "0.2.3"
---

## Plot age-period-cohort rates and life courses on a Lexis grid of equilateral triangles using R

### APCplot
Function that tortures Martin Smith's `Ternary` package <https://github.com/ms609/Ternary/> into drawing age-period-cohort (APC) plots of a set of rates (i.e. a Lexis surface) using a triangular grid. It can produce the following plots:
1. Plots of the rates or of the trend in them i.e. the ratios of the rates in successive periods (`change_in_rates = TRUE`).
2. Plots of the log measures or of the untransformed rates or ratios (`log_rates = FALSE`).

For each of the above options, `APCplot` can produce:
1. Plots for sub-groups (e.g. men and women) displayed on a grid.
2. A single plot for the entire population (`group_specific = FALSE`).
3. A single plot showing the ratio of the rates in a pair of sub-groups (e.g. men:women) (`group_ratios = TRUE`).

For any of the above options, it can either plot the individual APC rates for each triangle on the Lexis grid or produce a contour plot (`contour_plot = TRUE`).

As input, `APCplot` expects a data frame that should include:
1. Columns named `age`, `per` and `coh` whose rows cover the universe of possible combinations of values of the integer coordinates. The number of categories of `age`, `per` and `coh` can vary, but should be the same for all three axes (e.g. 0 to 99 in the case of mortality data, so that `length_yrs = 100`).
2. Either a series of named columns for each of the sub-groups (e.g. `Men` and `Women`), or a single column for the entire population named `Rates`. Each column should contain rates for every possible combination of values of `age`, `per` and `coh` (i.e. where `age + per + coh < length_yrs`).

The data frame should usually contain *N* observations, where *N* is the square of the length of the age, period, and cohort vectors (so, for mortality, the dataset should have 10,000 observations). However, if one is plotting changes in the rates, one also needs to supply data for the cohort born in the year before the `base_year` as they are required to calculate the rate of change into the base year.

### setupHMDdata
Ancillary function to download and reorganise mortality data from the Human Mortality Database (HMD) at <https://mortality.org> for passing to `APCplot`. Note that users must first register on the site in order to download the data. (Also note that, if you registered before June 2022, you need to re-register).

The country codes are listed in the data section of the HMD website. Note that not all countries have a century-long run of data and the plotting function has not yet been generalized for use with axes of differing length (e. g. 50 years of data on ages 0 to 99).

### Illustrative plots - Death rates in England and Wales
```
## Import death rates to a data frame from the Human Mortality Database 
Lexis <- setupHMDdata(your_user_id, your_password, country_id = "GBRTENW", base_year = 1922L, length_yrs = 100L)
## Log rates for each sex
APCplot(Lexis, base_year = 1922)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/f5a3b785-010e-4648-b759-5e1d86308de4)

```
## Contour plot of the log rates for the two sexes combined
APCplot(Lexis, base_year = 1922, contour_plot = TRUE, group_specific = FALSE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/bdcdef6d-3d23-4fa0-a8c0-de039279a610)

```
## Ratios of the men's to the women's rates
APCplot(Lexis, base_year = 1922, group_ratios = TRUE, log_rates = FALSE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/f1318902-f2dc-49b2-9e48-7f05e5e96ccc)

```
## Rates of change in the rates for each sex
APCplot(Lexis, base_year = 1922, change_in_rates = TRUE)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/69bc5076-553a-4dad-b277-c977894eed8f)


### LifeCourses
This function draws the life courses of one or more individuals on a Lexis plot represented as a grid of equilateral triangles. `LifeCourses` distinguishes exits due to death from those due to censoring. It can also plot the timing of up to three different types of life event along the life course (e.g. marrying, giving birth and marital dissolution). Each individual can experience each type of event once, multiple times, or not at all. The input vectors specifying the individuals' years of birth and ages at death and lists of vectors of other life events should all be the same length.
```
## Initialise life course histories for 4 individuals
YearB <- c(1925, 1938, 1962, 2001)
AgeD <- c(70, 80, 50, NA)
eLab <- "Birth of child"
events <- list(NULL, c(17,26), c(32), NULL)
e2Lab <- "Marriage"
events2 <- list(c(22, 40), c(16), c(25), NULL)
e3Lab <- "Marital dissolution"
events3 <- list(c(28), NULL, NULL, NULL)
cLab <- "Emigration"
Censored <- c(F, F, T, F)
## Draw lexis diagram with cohort and period measured in calendar years
PlotLifeCourses(YearB, AgeD, Events = events, eLabel = eLab,  
                Events2 = events2, e2Label = e2Lab, Events3 = events3, 
                e3Label = e3Lab, censored = Censored, cLabel = cLab
                survey_year = 2020)
```
![image](https://github.com/BugBunny/APCplot/assets/10499045/0edbb56f-69bb-4c89-a270-ff94d0ab77fa)



`APCplot` was developed in R 4.3.2 under Windows 11.
