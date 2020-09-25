
<!-- README.md is generated from README.Rmd. Please edit that file -->

# befproj

<!-- badges: start -->

<!-- badges: end -->

The goal of `befproj` is to make forecasts of a population. The method
used is a common projection method called cohort component method. In
this example data we will make a projection for Umea municipality .

## Installation

You can install the released version of befproj from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("befproj")
```

## Example

This is a simple example how to use the function `bef_components` and
extract results for the population components. The first argument is the
start population. The second argument is the assumptions about fertility
and death rates etc. The third argument is the year with our start
population. The first few rows and columns are shown below:

``` r
library(befproj)
bef.1 <- bef_components(startpop_data, assump_data, 2019)
bef.1[2:6,2:5]
#>      netmigration.men netmigration.tot birts.boys births.girls
#> 2020         541.0532         1115.083   784.4031     741.6729
#> 2021         480.7252         1086.154   798.4692     754.9728
#> 2022         515.1171         1089.646   810.5600     766.4050
#> 2023         461.5626         1061.646   819.9212     775.2562
#> 2024         486.9417         1067.007   826.8813     781.8372
```

This is a simple example how to use the function `bef_raw` and extract
results for the age specific numbers. The first six lines are shown:

``` r
bef.2 <- bef_raw(startpop_data, assump_data, 2019)
head(bef.2)
#>   year agegroup2 agegroup total_2 men_2 women_2 age.n
#> 1 2019     [0,1)    [0,1)    1487   784     703     0
#> 2 2019     [1,2)    [1,6)    1515   799     716     1
#> 3 2019     [2,3)    [1,6)    1546   802     744     2
#> 4 2019     [3,4)    [1,6)    1488   775     713     3
#> 5 2019     [4,5)    [1,6)    1484   767     717     4
#> 6 2019     [5,6)    [1,6)    1501   772     729     5
```

This is a simple example how to use the function `bef_proj` and extract
results for yearly growth:

``` r
bef.3 <- bef_proj(startpop_data, assump_data, 2019)
tail(bef.3)
#>        growth
#> 2025 1671.514
#> 2026 1647.338
#> 2027 1630.896
#> 2028 1603.411
#> 2029 1562.460
#> 2030 1490.897
```
