Plot age-period-cohort rates on a grid of equilateral triangles using R.

Author: Ian Timaeus, 16 Feb 2024. https://github.com/BugBunny/APCplot

APCplot: Function that tortures Martin Smith's Ternary package https://github.com/ms609/Ternary/ into drawing a pair of age-period-cohort  plots of sets of rates for men and women respectively. It can either plot the individual APC rates triangle by triangle or a contour plot.
As input, APCplot expects a data frame that should include columns named age, per and coh that cover the universe of possible combinations of values of the coordinates (so ranging from 0 to 99 in the case of mortality data). It should also include either columns named Male and Female, or a column namedRates, containing the rates for each possible combination of age, per and coh. The data frame should contain  N observations, where N is the square of the length of the age, period, cohort vectors (so, for mortality, the dataset  should have 10,000 observations).  

setupHMDdata: Ancillary function to download and reorganise the mortality data from the Human Mortality Database (HMD) at https://mortality.org. Note that users must first register on the sitein order to download the data (Note also that, if you registered before June 2022, you need to re-register).

Author: Ian Timaeus, 16 Feb 2024. https://github.com/BugBunny/APCplot
