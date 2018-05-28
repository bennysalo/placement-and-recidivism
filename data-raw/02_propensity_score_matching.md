Propensity score matching
================

The goal of this script is to take the subset lists generated in *01\_analyzed\_data* and add three columns to the data element:

-   propensity score
-   subclass (high or low propensity)
-   matched (yes or no)

We do this in the following steps:

1.  Calculate propensity scores (separately for each subset)
2.  Divide the subsets into two subclasses (above and below propensity of 50%)
3.  Do matching within these two subclasses. The *high propensity subclass* have theoretically more treated individuals and is therefore matched **2 treated to 1 control**. Conversely the *low propensity subclass* is matched **1 treated to 2 controls**.

In addition to this we add elements to the subset lists. The complete list will have the following elements:

-   `effect_variable` (name of the variable used as independent variable)
-   `inclusion_criteria` (description as a character string)
-   `data` - Added columns are:
    -   `propensity_score`
    -   `subclass`
    -   `matched`
-   `formulas`
    -   `ordinary`
    -   `reversed` (the effect variable inverted to allow 2 treated to 1 control matching)
-   `matchit` (output from matchit)
    -   `low` (the low propensity subclass)
    -   `high` (the high propensity subclass)

Setup
-----

``` r
devtools::load_all(".")
```

    ## Loading placementAndRecidivism

Functions for each addition to the list
---------------------------------------

These functions are combined to a single function later.

### Write propensity score formulas

Computationally we start with adding the formulas. They are needed to calculate propensity scores and reused in with the `matchit` function.

We are going to write two formulas: one 'ordinary' and one 'inverted'. The inverted formula is simply the ordinary formula but with the levels in the outcome flipped. We used this to coerce matchit to allow matching two treated with one control. (The role of these groups are flipped when the levels of the outcome is flipped.)

#### Write formula

A function to write the formula with the arguments: - lhs, the left hand side of the formula - a character string naming the outcome - rhs, the right hand side of the formula - a character vector with all predictors

``` r
  write_formula <- function(lhs, rhs) {
    f <- as.formula(paste(lhs, " ~ ",
                          paste(rhs, collapse = " + ")))
    return(f)
  } 
```

#### Add a list of the two formulas to the subset list

A function to append the list with formulas for propensity score matching. Takes a subset list and adds the 'formulas' element. Writes formulas with different outcomes depending on what effect variable is defined in the subset list.

``` r
add_formulas <- function(x, balancing_vars = all_predictors) {
  
  # Initiate
  formulas <- x$formulas

  # Write the formulas
  # Different formulas depending on the effect_variable
  # Matchit needs dichotomous outcomes so we use those
  if        (x$effect_variable == "openPrison") {
    formulas$ordinary  <- write_formula("open01", balancing_vars)
    formulas$inverted  <- write_formula("open10", balancing_vars)
    
  } else if (x$effect_variable == "conditionalReleaseOutcome") {
    formulas$ordinary <- write_formula("cond01", balancing_vars)
    formulas$inverted <- write_formula("cond10", balancing_vars)

  } else {
    warning("The effect_variable needs to be either 'openPrison' or
            'conditionalReleaseOutcome'")
  }
  
  return(formulas)
  
}
```

### Calculate propensity scores

``` r
add_propensity_scores <- function(x) {
  
  # Use the ordinary formula that was written above 
  # to calculate propensity scores
  fit <- glm(x$formulas$ordinary, 
             data = x$data, family = binomial)
  
  propensity_score <- predict(fit)
  
  return(propensity_score)
}
```

### Do matching

This function takes the results list (with the formulas written) as input and outputs a list of matchit objects (lists of class matchit). Two matchit objects are produced - one for the low propensity group and one for the high propensity group. The caliper argument in matchit is here set to 0.25 for both matching runs. This can be changed via the `my_caliper` argument.

``` r
add_matching <- function(x, my_caliper = 0.25) {
  require(MatchIt)
  
  # Initiate
  MatchIt_output <- list()

  # Perform matching
  # In the low propensity subclass, there are more controls. 
  # We can use the ordinary formula and match treated individuals at a
  # ratio of 2 controls
  
  MatchIt_output$low  <- 
    MatchIt::matchit(formula = x$formulas$ordinary,
                     data    = x$data$low,
                     caliper = my_caliper, 
                     ratio   = 2)
  
  # In the high propensity subclass, there are more treated. 
  # We want 2 treated per one control and therefor need to reverse 
  # the propensity score formula to predict "non-treatment"
  # We match these controls at a ratio of 2 treated.
  
  MatchIt_output$high <-
    MatchIt::matchit(formula = x$formulas$inverted,
                     data    = x$data$high, 
                     caliper = my_caliper,
                     ratio   = 2)
  
  return(MatchIt_output)
}
```

### Add columns for whether the observation was matched

We will access the match.matrix element of the matchit output to find the observations that were matched appropriately. We exclude cases that do not have two matches.

We write a function for extracting indices in the `match.matrix` for observations that are matched. Takes the match.matrix as input and outputs a numeric vector of the rows of the matched observations (regardless if they are treated or control).

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

Add the matched column to data. Takes the subset list (with matchit objects added) and outputs the list including the pertinent column in data.

``` r
add_matched_col <- function(x) {
  # Intitate
   matched <-  factor(rep(NA, times = nrow(x$data)), 
                      levels = c("not_matched", "matched"))
   
   match_index_low  <- extract_matched_rows(x$MatchIt_output$low$match.matrix)
   match_index_high <- extract_matched_rows(x$MatchIt_output$high$match.matrix)
   
   matched[1:length(matched)] <- "not_matched"
   matched[c(match_index_low, match_index_high)] <- "matched"
 
 return(matched)
}
```

Putting it all together
-----------------------

Combine the previous functions to a single function that adds all the element we want.

``` r
perform_my_matching <- function(x) {
  
  # Formulas
  x$formulas               <- add_formulas(x)
  # Propensity scores
  x$data$propensity_score <- add_propensity_scores(x)
  # Subsclass
  x$data$subclass          <- factor(ifelse(x$data$propensity_score < 0,
                                            yes = "low", no =  "high"),
                                     levels = c("low", "high"))

  # Copy subclass vector for unsplitting
  x$subclass_vector        <- x$data$subclass

  # Split
  x$data                   <- split(x$data, x$subclass_vector)

  # MatchIt - output for both subclasses
  x$MatchIt_output         <- add_matching(x)

  # Unsplit
  x$data                   <- unsplit(x$data, x$subclass_vector)

  # Column indicating if observation was
  x$data$matched           <- add_matched_col(x)
  return(x)
}
```

Run analyses
------------

### Import the subset lists

``` r
devtools::wd()
op_cr0_ps0 <- readRDS("not_public/op_cr0_ps0.rds")
op_cr0_ps1 <- readRDS("not_public/op_cr0_ps1.rds")
cr_op1_ps0 <- readRDS("not_public/cr_op1_ps0.rds")
cr_op1_ps1 <- readRDS("not_public/cr_op1_ps1.rds")
```

### Add elements

``` r
op_cr0_ps0 <- perform_my_matching(op_cr0_ps0)
```

    ## Loading required package: MatchIt
