Analyzed data
================

Setup
=====

``` r
library(dplyr)
```

Load data
=========

Import data set 'FinPrisonMales'. This version of the data includes only males. Data is not publicly available.

``` r
rm(list = ls())
devtools::wd()
FinPrisonMales <- readRDS("not_public/FinPrisonMales.rds")
```

Variables
=========

Exclude irrelevant variables with missing values. Matchit does not allow missing values and they will not be needed anyway.

``` r
index_no_missing <- apply(FinPrisonMales, MARGIN = 2, FUN = anyNA) == FALSE

FinPrisonMales   <- FinPrisonMales[index_no_missing]
```

The package `matchit` uses dichotomous integer variables for outcomes when calculating propensity scores. Add these.

``` r
FinPrisonMales <-
  FinPrisonMales %>% 
  # Numeric versions of the factors have integers 1 and 2.
  # Subtracting 1 puts them on a logical 0, 1 scale.
  mutate(open01 = as.numeric(openPrison) - 1,
         cond01 = as.numeric(conditionalReleaseGranted) - 1,
  # We also want reversed codings with closed and no CR coded as 1
  # We achieve this by subtracting 2. (Results in -1, 0)
  # and taking the absolute value (results in 1, 0)
         open10 = abs(as.numeric(openPrison) - 2),
         cond10 = abs(as.numeric(conditionalReleaseGranted) - 2))
```

For the purpose of these analyses code "reoffenceThisTerm" as 0 = no reoffence and 1 reoffence

``` r
FinPrisonMales <- FinPrisonMales %>% 
  mutate(reoffenceThisTerm = 
           ifelse(reoffenceThisTerm == "new_prison_sentence",
                  1, 0))
```

Exclusion and inclusion
=======================

Exclude individuals who committed a crime during the sentence. This is the simplest way to handle the issue that we do not know when the crime was committed, when it was uncovered, or how it affected placement decisions.

These are 93 individuals

``` r
table(FinPrisonMales$crimeDuringSentence)
```

    ## 
    ## No records of crime     Record of crime 
    ##                1403                  93

``` r
analyzed_data_plac <-
  FinPrisonMales %>% 
  filter(crimeDuringSentence == "No records of crime")

rm(FinPrisonMales)
```

This is the distribution of individuals over status of placement at release and conditional release:

``` r
knitr::kable(
  table(analyzed_data_plac$openPrison,
        analyzed_data_plac$conditionalReleaseOutcome)
  )
```

|               |  No conditional release|  Successful conditional relase|  Cancelled conditional release|
|---------------|-----------------------:|------------------------------:|------------------------------:|
| Closed prison |                     668|                             38|                             48|
| Open prison   |                     411|                            220|                             18|

We exclude individuals with suspended conditional release. This is the easiest way to handle the confounding that individuals granted conditional release from open prison may have been placed in closed prison if the conditional release was revoked.

``` r
analyzed_data_plac <-
  analyzed_data_plac %>% 
  filter(conditionalReleaseOutcome != "Cancelled conditional release")

# Remove the excluded level for conditional release.
levels(analyzed_data_plac$conditionalReleaseOutcome) <- 
  c("No conditional release", "Successful conditional release", NA)
```

Further we separate those with their parole supervised and those without supervision.

``` r
knitr::kable(
  table(
    analyzed_data_plac$conditionalReleaseOutcome,
    analyzed_data_plac$openPrison,
    analyzed_data_plac$supervisedParole),
    col.names = c("Conditional release", "Placement", "Supervision", "Freq")
  )
```

| Conditional release            | Placement     | Supervision           |  Freq|
|:-------------------------------|:--------------|:----------------------|-----:|
| No conditional release         | Closed prison | No parole supervision |   241|
| Successful conditional release | Closed prison | No parole supervision |     9|
| No conditional release         | Open prison   | No parole supervision |   148|
| Successful conditional release | Open prison   | No parole supervision |    50|
| No conditional release         | Closed prison | Parole was supervised |   427|
| Successful conditional release | Closed prison | Parole was supervised |    29|
| No conditional release         | Open prison   | Parole was supervised |   263|
| Successful conditional release | Open prison   | Parole was supervised |   170|

The smallest groups (individuals placed in closed prison but granted conditional release, \[9 not supervised, 29 supervised\]) will not be examined further. That leaves us with the possibility to make the following comparisons:

1.  the effect of placement among those not granted conditional release
    -   when parole was not supervised (closed \[241\] vs. open prison \[148\])
    -   when parole was supervised (closed \[427\] vs. open \[263\])
2.  the effect of conditional release among those placed in open prison at the end of their sentence
    -   when parole was not supervised (no release\[148\] vs. successful release \[50\])
    -   when parole was supervised (no \[263\] vs. successful \[170\])
