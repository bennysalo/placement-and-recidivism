Lasso regression for selecting covariates
================
Benny Salo
2018-06-14

We try to select the covariates that have a unique effect on recidivism. Excluding covariates that are not needed should reduce variance of the analyses and make the presentation clearer.

To do this we use lasso regression.

Load data and setup.
--------------------

``` r
devtools::wd()
analyzed_data_plac <- readRDS("not_public/analyzed_data_plac.rds")

devtools::load_all(".")
library(tidyverse)
```

We define a formula for predicting reoffence. We consider all predictors previously defined. Supervision of parole is not included. We will do exact matching on this variable.

``` r
pred_reci_formula <- write_formula("reoffenceThisTerm", all_predictors)
```

Do lasso regression with ten fold cross-validation.

``` r
set.seed(120618)
# Create folds
ten_folds_reoffence <- 
  caret::createFolds(y = analyzed_data_plac$reoffenceThisTerm, k = 10)

# Define control function to be used with caret::train
ctrl_fun_reoffence <- caret::trainControl(
  method = "cv",
  number = 10,
  summaryFunction = caret::twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = "final",
  index = ten_folds_reoffence,
  returnData = FALSE
  )

# Train model
trained_lasso <- 
  caret::train(
      form      = pred_reci_formula,
      data      = analyzed_data_plac,
      method    = "glmnet",
      family    = "binomial", # passed to glmnet, define as logistic regression
      standardize = TRUE,     # passed to glmnet, explicitly standardize  
      metric    = "ROC",
      trControl = ctrl_fun_reoffence,
      tuneGrid  = expand.grid(alpha  = 1,
                              lambda = seq(0, 0.3, 0.001)))
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    ## + Fold01: alpha=1, lambda=0.3 
    ## - Fold01: alpha=1, lambda=0.3 
    ## + Fold02: alpha=1, lambda=0.3 
    ## - Fold02: alpha=1, lambda=0.3 
    ## + Fold03: alpha=1, lambda=0.3 
    ## - Fold03: alpha=1, lambda=0.3 
    ## + Fold04: alpha=1, lambda=0.3 
    ## - Fold04: alpha=1, lambda=0.3 
    ## + Fold05: alpha=1, lambda=0.3 
    ## - Fold05: alpha=1, lambda=0.3 
    ## + Fold06: alpha=1, lambda=0.3 
    ## - Fold06: alpha=1, lambda=0.3 
    ## + Fold07: alpha=1, lambda=0.3 
    ## - Fold07: alpha=1, lambda=0.3 
    ## + Fold08: alpha=1, lambda=0.3 
    ## - Fold08: alpha=1, lambda=0.3 
    ## + Fold09: alpha=1, lambda=0.3 
    ## - Fold09: alpha=1, lambda=0.3 
    ## + Fold10: alpha=1, lambda=0.3 
    ## - Fold10: alpha=1, lambda=0.3 
    ## Aggregating results
    ## Selecting tuning parameters
    ## Fitting alpha = 1, lambda = 0.04 on full training set

We can look at the relative importance of the predictors in the trained model with the chosen level of regularization.

``` r
caret::varImp(trained_lasso)
```

    ## glmnet variable importance
    ## 
    ##   only 20 most important variables shown (out of 30)
    ## 
    ##                                                  Overall
    ## drug_related_probl                               100.000
    ## o_traffic                                         99.538
    ## economy_problems                                  45.337
    ## o_sexual                                          36.443
    ## ps_remandTerms_mr                                 33.127
    ## o_assault                                         16.250
    ## o_homicide                                        13.879
    ## ps_prisonTerms_mr                                 11.882
    ## ageAtRelease                                       5.493
    ## ageFirstSentence_mr                                2.645
    ## ageFirst_missing                                   0.000
    ## alcohol_problems                                   0.000
    ## ps_escapeHistoryUnauthorized absence of any kind   0.000
    ## o_whitecollar                                      0.000
    ## employment_probl                                   0.000
    ## resistance_change                                  0.000
    ## o_againstOfficer                                   0.000
    ## o_damage                                           0.000
    ## o_otherProperty                                    0.000
    ## ps_defaultTerms_mr                                 0.000

Predictors with non-zero coefficients will be treated as potential confounders. We extract this character vector and save it for later use.

``` r
potential_confounders <- varImp(trained_lasso)$importance %>% 
  rownames_to_column(var = "predictor") %>% 
  filter(Overall > 0) %>% 
  select(predictor) %>% 
  simplify()

potential_confounders
```

    ##            predictor1            predictor2            predictor3 
    ## "ageFirstSentence_mr"        "ageAtRelease"   "ps_prisonTerms_mr" 
    ##            predictor4            predictor5            predictor6 
    ##   "ps_remandTerms_mr"           "o_traffic"           "o_assault" 
    ##            predictor7            predictor8            predictor9 
    ##          "o_homicide"            "o_sexual"    "economy_problems" 
    ##           predictor10 
    ##  "drug_related_probl"

Save for later use.

``` r
devtools::use_data(potential_confounders, overwrite = TRUE)
```
