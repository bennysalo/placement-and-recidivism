Analyzed data
================
Benny Salo
2018-06-14

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

Create new variable indicating both placement in open prison and in addition successful conditional release. This will be used for calculating 'overarching propensity score'.

``` r
FinPrisonMales <- FinPrisonMales %>% 
  mutate(open_and_cr = 
           ifelse(openPrison == "Open_prison" & 
                  conditionalReleaseOutcome == "Successful_conditional_release",
                  "yes", "no"))
```

Name levels of the key variables clearly

``` r
levels(FinPrisonMales$openPrison)
```

    ## [1] "closed_prison" "open_prison"

``` r
levels(FinPrisonMales$openPrison) <- 
  c("Closed_prison", "Open_prison")

levels(FinPrisonMales$supervisedParole)
```

    ## [1] "no supervision"    "supervised parole"

``` r
levels(FinPrisonMales$supervisedParole) <-
  c("No_parole_supervision", "Parole_was_supervised")

levels(FinPrisonMales$conditionalReleaseGranted) 
```

    ## [1] "cr_not_granted" "cr_granted"

``` r
levels(FinPrisonMales$conditionalReleaseGranted) <- 
  c("Not_granted", "Granted")
```

Exclude irrelevant variables with missing values. Matchit does not allow missing values and they will not be needed anyway.

``` r
index_no_missing <- apply(FinPrisonMales, MARGIN = 2, FUN = anyNA) == FALSE

FinPrisonMales   <- FinPrisonMales[index_no_missing]
```

The package `MatchIt` uses dichotomous integer variables for outcomes when calculating propensity scores. Add these.

``` r
FinPrisonMales <-
  FinPrisonMales %>% 
  # Numeric versions of the factors have integers 1 and 2.
  # Subtracting 1 puts them on a logical 0, 1 scale.
  mutate(open01      = as.numeric(openPrison) - 1,
         cond01      = as.numeric(conditionalReleaseGranted) - 1,
         reoffence01 = 
           ifelse(reoffenceThisTerm == "new_prison_sentence",
                  1, 0),
         open_and_cr01 = 
           ifelse(open_and_cr == "yes", 1, 0))
  # The following will probably be deleted.
  # We also want reversed codings with closed and no CR coded as 1
  # We achieve this by subtracting 2. (Results in -1, 0)
  # and taking the absolute value (results in 1, 0)
         # open10 = abs(as.numeric(openPrison) - 2),
         # cond10 = abs(as.numeric(conditionalReleaseGranted) - 2))
```

Define sets of predictors

The vector `all_predictors` will be used to write the formula for propensity scores.

``` r
offence_variables <- 
  stringr::str_subset(names(FinPrisonMales), "^o_")

static_preds <- 
   c("ageFirstSentence_mr", "ageFirst_missing", "ageAtRelease", 
     "ps_escapeHistory", "ps_prisonTerms_mr", "ps_comServiceTerms_mr", 
     "ps_remandTerms_mr", "ps_defaultTerms_mr", "ps_info_missing", 
     offence_variables)


rita_factors <- 
   c("economy_problems", "alcohol_problems", "resistance_change", 
     "drug_related_probl", "aggressiveness", "employment_probl") 


all_predictors <- c(static_preds, rita_factors)

devtools::use_data(all_predictors, overwrite = TRUE)
devtools::use_data(static_preds, overwrite = TRUE)
devtools::use_data(rita_factors, overwrite = TRUE)
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

|                |  No conditional release|  Successful conditional relase|  Cancelled conditional release|
|----------------|-----------------------:|------------------------------:|------------------------------:|
| Closed\_prison |                     668|                             38|                             48|
| Open\_prison   |                     411|                            220|                             18|

We exclude individuals with suspended conditional release. This is the easiest way to handle the confounding that individuals granted conditional release from open prison may have been placed in closed prison if the conditional release was revoked.

``` r
analyzed_data_plac <-
  analyzed_data_plac %>% 
  filter(conditionalReleaseOutcome != "Cancelled conditional release")

# Remove the excluded level for conditional release.
levels(analyzed_data_plac$conditionalReleaseOutcome) <- 
  c("No_conditional_release", "Successful_conditional_release", NA)
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

| Conditional release              | Placement      | Supervision             |  Freq|
|:---------------------------------|:---------------|:------------------------|-----:|
| No\_conditional\_release         | Closed\_prison | No\_parole\_supervision |   241|
| Successful\_conditional\_release | Closed\_prison | No\_parole\_supervision |     9|
| No\_conditional\_release         | Open\_prison   | No\_parole\_supervision |   148|
| Successful\_conditional\_release | Open\_prison   | No\_parole\_supervision |    50|
| No\_conditional\_release         | Closed\_prison | Parole\_was\_supervised |   427|
| Successful\_conditional\_release | Closed\_prison | Parole\_was\_supervised |    29|
| No\_conditional\_release         | Open\_prison   | Parole\_was\_supervised |   263|
| Successful\_conditional\_release | Open\_prison   | Parole\_was\_supervised |   170|

The smallest groups (individuals placed in closed prison but granted conditional release, \[9 not supervised, 29 supervised\]) will not be examined further. That leaves us with the possibility to make the following comparisons:

1.  the effect of open prison among those not granted conditional release
    1.  when parole was not supervised (closed \[241\] vs. open prison \[148\])
    2.  when parole was supervised (closed \[427\] vs. open \[263\])
2.  the effect of conditional release among those placed in open prison at the end of their sentence
    1.  when parole was not supervised (no release\[148\] vs. successful release \[50\])
    2.  when parole was supervised (no \[263\] vs. successful \[170\])

Create subsets
==============

We create a subset for each of the four (2 x 2) comparisons above.

1.  Effect of open prison (when conditional release is not granted).
    -   when parole is NOT supervised

    ``` r
    op_cr0_ps0<-
      analyzed_data_plac %>% 
      filter(conditionalReleaseOutcome == "No_conditional_release" & 
           supervisedParole == "No_parole_supervision")
    ```

    -   when parole IS supervised

    ``` r
    op_cr0_ps1 <-
      analyzed_data_plac %>% 
      filter(conditionalReleaseOutcome == "No_conditional_release" & 
           supervisedParole == "Parole_was_supervised")
    ```

2.  Effect of conditional release (for individuals placed in open prison)
    -   when parole is NOT supervised

    ``` r
    cr_op1_ps0  <-
      analyzed_data_plac %>% 
      filter(openPrison == "Open_prison" &
           supervisedParole == "No_parole_supervision")
    ```

    -   when parole IS supervised

    ``` r
    cr_op1_ps1 <- 
      analyzed_data_plac %>% 
      filter(openPrison == "Open_prison" & 
           supervisedParole == "Parole_was_supervised")
    ```

Assertions. Make sure the subsets have the expected number of observations.

``` r
stopifnot(nrow(op_cr0_ps0) == (241 + 148))
stopifnot(nrow(op_cr0_ps1) == (427 + 263))
stopifnot(nrow(cr_op1_ps0) == (148 + 50))
stopifnot(nrow(cr_op1_ps1) == (263 + 170))
```

Make a list for each subset that also contains the name of the effect variable and a description of inclusion criteria.

``` r
op_cr0_ps0 <- list(effect_variable = "openPrison",
                   inclusion_criteria = 
                     "No conditional release, parole NOT supervised",
                   data = op_cr0_ps0)

op_cr0_ps1 <- list(effect_variable = "openPrison",
                   inclusion_criteria = 
                     "No conditional release, parole WAS supervised",
                   data = op_cr0_ps1)

cr_op1_ps0  <- list(effect_variable = "conditionalReleaseOutcome",
                   inclusion_criteria = 
                     "Released from open prison, parole NOT supervised",
                   data = cr_op1_ps0)

cr_op1_ps1  <- list(effect_variable = "conditionalReleaseOutcome",
                   inclusion_criteria = 
                     "Released from open prison, parole WAS supervised",
                   data = cr_op1_ps1)
```

Assertions. Check that the script above has produced a list with three elements with correct names and of correct classes.

``` r
check_list_elements <- function(x) {
  assertthat::assert_that(
    class(cr_op1_ps1$effect_variable) == "character",
    class(cr_op1_ps1$inclusion_criteria) == "character",
    class(cr_op1_ps1$data) == "data.frame",
    msg = " The list does not contain the expected elements."
  )
}

check_list_elements(op_cr0_ps0)
```

    ## [1] TRUE

``` r
check_list_elements(op_cr0_ps1)
```

    ## [1] TRUE

``` r
check_list_elements(cr_op1_ps0)
```

    ## [1] TRUE

``` r
check_list_elements(cr_op1_ps1)
```

    ## [1] TRUE

Save subset list for later use.

``` r
devtools::wd()
saveRDS(op_cr0_ps0, "not_public/op_cr0_ps0.rds")
saveRDS(op_cr0_ps1, "not_public/op_cr0_ps1.rds")
saveRDS(cr_op1_ps0, "not_public/cr_op1_ps0.rds")
saveRDS(cr_op1_ps1, "not_public/cr_op1_ps1.rds")
saveRDS(analyzed_data_plac, "not_public/analyzed_data_plac.rds")
```
