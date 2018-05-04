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
FinPrisonMales <- 
  readRDS("not_public/FinPrisonMales.rds")
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
table(analyzed_data_plac$openPrison,
      analyzed_data_plac$conditionalReleaseOutcome)
```

    ##                
    ##                 No conditional release Successful conditional relase
    ##   closed_prison                    668                            38
    ##   open_prison                      411                           220
    ##                
    ##                 Cancelled conditional release
    ##   closed_prison                            48
    ##   open_prison                              18

Exclude individuals with suspended conditional release. This is the easiest way to handle the confounding that individuals granted conditional release from open prison may have been placed in closed prison if the conditional release was revoked.

``` r
analyzed_data_plac <-
  analyzed_data_plac %>% 
  filter(conditionalReleaseOutcome != "Cancelled conditional release")
```

This leaves us with the following breakdown of individuals.

``` r
table(analyzed_data_plac$openPrison,
      analyzed_data_plac$conditionalReleaseOutcome)
```

    ##                
    ##                 No conditional release Successful conditional relase
    ##   closed_prison                    668                            38
    ##   open_prison                      411                           220
    ##                
    ##                 Cancelled conditional release
    ##   closed_prison                             0
    ##   open_prison                               0

Further we separate those with their parole supervised and those without supervision.

``` r
 table(analyzed_data_plac$supervisedParole,
      analyzed_data_plac$conditionalReleaseOutcome,
      analyzed_data_plac$openPrison) 
```

    ## , ,  = closed_prison
    ## 
    ##                    
    ##                     No conditional release Successful conditional relase
    ##   no supervision                       241                             9
    ##   supervised parole                    427                            29
    ##                    
    ##                     Cancelled conditional release
    ##   no supervision                                0
    ##   supervised parole                             0
    ## 
    ## , ,  = open_prison
    ## 
    ##                    
    ##                     No conditional release Successful conditional relase
    ##   no supervision                       148                            50
    ##   supervised parole                    263                           170
    ##                    
    ##                     Cancelled conditional release
    ##   no supervision                                0
    ##   supervised parole                             0

The smallest groups (individuals placed in closed prison and not granted conditional release) will not be examined further. That leaves us with the possibility to examine - the effect of placement among those not granted conditional release - the effect of conditional release among those placed in open prison at the end of their sentence.
