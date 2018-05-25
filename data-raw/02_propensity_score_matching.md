Propensity score matching
================

The goal of this script is to add three columns to the data frames: - propensity score - subclass (high or low propensity) - matched (yes or no)

We do this in the following steps: 1. Calculate propensity scores (seperately for each subset) 2. Divide the subsets into two subclasses (above and below propensity 50%) 3. Do matching within these two subclasses. The high propensity subclass have theoretically more treated individuals and is therefore matched 2 treated to 1 control. Conversely the low propensity group is matched 1 treated to 2 controls.

In addition to this we add elements to the subset lists. The complete list will have the following elements: - effect\_variable (name of the variable used as independent variable) - inclusion\_criteria (description as a character string) - data (with the columns above added) - formulas - ordinary - reversed (the effect variable inverted to allow 2 treated to 1 control matching) - matchit (output from matchit) - low (the low propensity subclass) . high (the high propensity subclass)

Computationally we start with adding the formulas.

Import the subset lists
-----------------------

``` r
devtools::wd()
op_cr0_ps0 <- readRDS("not_public/op_cr0_ps0.rds")
op_cr0_ps1 <- readRDS("not_public/op_cr0_ps1.rds")
cr_op1_ps0 <- readRDS("not_public/cr_op1_ps0.rds")
cr_op1_ps1 <- readRDS("not_public/cr_op1_ps1.rds")
```

Write propensity score formulas
-------------------------------

### Define sets of predictors

The vector `all_predictors` will be used to write the formula for propensity scores.

``` r
offence_variables <- 
  stringr::str_subset(names(op_cr0_ps0), "^o_")

static_preds <- 
   c("ageFirstSentence_mr", "ageFirst_missing", "ageAtRelease", 
     "ps_escapeHistory", "ps_prisonTerms_mr", "ps_comServiceTerms_mr", 
     "ps_remandTerms_mr", "ps_defaultTerms_mr", "ps_info_missing", 
     offence_variables)


rita_factors <- 
   c("economy_problems", "alcohol_problems", "resistance_change", 
     "drug_related_probl", "aggressiveness", "employment_probl") 


all_predictors <- c(static_preds, rita_factors)
```

### Functions

A function to write the formula based on given outcome

``` r
  write_formula <- function(lhs, rhs) {
    f <- as.formula(paste(lhs, " ~ ",
                          paste(rhs, collapse = " + ")))
    return(f)
  } 
```

A function to append the list with formulas for propensity score matching. Takes a subset list and adds the 'formulas' element.

``` r
add_formulas <- function(subset_list, balancing_vars = all_predictors) {

  # This is the format of the output
  out <- subset_list
  out$formulas <- list(ordinary = NA, inverted = NA)

  # Write the formulas
  # Different formulas depending on the effect_variable
  # Matchit needs dichotomous outcomes so we use those
  if        (out$effect_variable == "openPrison") {
    out$formulas$ordinary  <- write_formula("open01", balancing_vars)
    out$formulas$inverted  <- write_formula("open10", balancing_vars)
    
  } else if (out$effect_variable == "conditionalReleaseOutcome") {
    out$formulas$ordinary <- write_formula("cond01", balancing_vars)
    out$formulas$inverted <- write_formula("cond10", balancing_vars)

  } else {
    warning("Could not determine effect")
  }
  
  return(out)
  
}
```

``` r
op_cr0_ps0 <- add_formulas(op_cr0_ps0)
```

Calculate propensity scores
---------------------------

``` r
add_propensity_scores <- function(subset_list) {
  
  # Initiate what we want
  out                       <- subset_list
  out$data$propensity_score <- NA
  
  # Use the ordinary formula that was written above 
  # to calculate propensity scores
  fit <- glm(subset_list$formulas$ordinary, 
             data = subset_list$data, family = binomial)
  
  out$data$propensity_score <- predict(fit)
  
  return(out)
}
```

``` r
op_cr0_ps0 <- add_propensity_scores(op_cr0_ps0)
```

Add subclasses
--------------

``` r
add_subclass <- function(subset_list) {
  # Initiate what we want
  out               <- subset_list
  out$data$subclass <- NA
  
  out$data$subclass <- ifelse(out$data$propensity_score < 0, yes = 1, no =  2)
  out$data$subclass <- factor(out$data$subclass, 
                              levels = c(1,2),
                              labels = c("low", "high"))
  
  return(out)
}
```

``` r
op_cr0_ps0 <- add_subclass(op_cr0_ps0)
```

``` r
split_data_into_subcl <- function(subset_list) {
  # Intitate what we want
  out <- subset_list
  out$subclass_vector <- NA
  
  #save the subclass_vector to allow unsplit later
  out$subclass_vector <- subset_list$data$subclass
  out$data <- split(out$data, out$subclass_vector)
  return(out)
}
```

``` r
op_cr0_ps0 <- split_data_into_subcl(op_cr0_ps0)
```

``` r
add_matching <- function(subset_list, my_caliper = 0.25) {
  require(MatchIt)
  # Initiate what we want
  out <- subset_list
  out$MatchIt$low
  out$MatchIt$high
  
  
  # Perform matching
  # In the low propensity subclass, there are more controls. 
  # We can use the ordinary formula and match treated individuals at a
  # ratio of 2 controls
  
  out$MatchIt$low  <- 
    MatchIt::matchit(formula = out$formulas$ordinary,
                     data    = out$data$low,
                     caliper = my_caliper, 
                     ratio   = 2)
  
  # In the high propensity subclass, there are more treated. 
  # We want 2 treated per one control and therefor need to reverse 
  # the propensity score formula to predict "non-treatment"
  # We match these controls at a ratio of 2 treated.
  
  out$MatchIt$high <-
    MatchIt::matchit(formula = out$formulas$inverted,
                     data    = out$data$high, 
                     caliper = my_caliper,
                     ratio   = 2)
  
  return(out)
}
```

``` r
op_cr0_ps0 <- add_matching(op_cr0_ps0)
```

    ## Loading required package: MatchIt

    ## Warning in matchit2nearest(structure(c(1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, :
    ## Not enough control units for 2 matches for each treated unit when matching
    ## without replacement. Not all treated units will receive 2 matches

Add columns for whether the observation was matched
---------------------------------------------------

Access the match\_matrix element of the matchit output to find the obeservations that were matched appropriately. Excluce cases that do not have two matches

We write a function for extracting indices for the mathced rows

``` r
  # Extract for each matching the row numbers that have been matched
    # Write a function to extract the row indeces for 
    # fully matched observations

   extract_matched_rows <- function(match.matrix) {
     
     # Find rows in match.matrix with a match missing
     not_fully_matched_i <- apply(match.matrix, MARGIN = 1, anyNA)
     # Create a matrix where these are excluded
     included_matches    <- match.matrix[!(not_fully_matched_i), ]
     # Write a vector with the matches and the observations theselves.
     included_obs        <- c(included_matches, row.names(included_matches))
     included_obs        <- as.numeric(included_obs)
    
     return(included_obs)
   }
```

Unsplit

``` r
op_cr0_ps0$data <- unsplit(op_cr0_ps0$data, op_cr0_ps0$subclass_vector)
```

A function to add information about matching

``` r
add_matched_col <- function(subset_list) {
  
 # Initiate what we want
 out <- subset_list
 out$data$matched <- NA
  
 match_index_low  <- extract_matched_rows(out$MatchIt$low$match.matrix)
 match_index_high <- extract_matched_rows(out$MatchIt$high$match.matrix)

 out$data$matched <- "not_matched"
 out$data$matched[c(match_index_low, match_index_high)] <- "matched"
 
 return(out)
}
```

``` r
op_cr0_ps0 <- add_matched_col(op_cr0_ps0)
```
