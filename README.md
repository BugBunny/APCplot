## Plot age-period-cohort rates on a grid of equilateral triangles using R

### APCplot
Function that tortures Martin Smith's *Ternary* package https://github.com/ms609/Ternary/ into drawing a pair of age-period-cohort  plots of sets of rates for men and women respectively. It can either plot the individual APC rates triangle by triangle or a contour plot.
As input, APCplot expects a data frame that should include columns named *age*, *per* and *coh* that cover the universe of possible combinations of values of the coordinates (so ranging from 0 to 99 in the case of mortality data). It should also include either columns named *Male* and *Female*, or a column named *Rates*, containing the rates for each possible combination of *age*, *per* and *coh*. The data frame should contain *N* observations, where *N* is the square of the length of the age, period, cohort vectors (so, for mortality, the dataset  should have 10,000 observations).  

### setupHMDdata
Ancillary function to download and reorganise the mortality data from the Human Mortality Database (HMD) at https://mortality.org for passing to APCplot. Note that users must first register on the site in order to download the data (Note also that, if you registered before June 2022, you need to re-register).

### Examples

Rates_lexis <- setupHMDdata(your_user_id, your_password, country_id = "GBRTENW", base_year = 1922L, length_yrs = 100L)
APCplot(Rates_lexis, base_year = 1922)

![image](https://github.com/BugBunny/APCplot/assets/10499045/09f05fb7-baf6-478c-b76e-6605416a6b31)


 APCplot(Rates_lexis, base_year = 1922, contour_plot = T, sex_specific = F)

![image](https://github.com/BugBunny/APCplot/assets/10499045/8a36c6dd-7549-40c1-b5a7-fb46c82364d1)

#### Author: Ian Timaeus, 16 Feb 2024
