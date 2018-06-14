# A function to write the formula with the arguments:
#   - lhs, the left hand side of the formula - a character string naming the outcome
# - rhs, the right hand side of the formula - a character vector with all predictors

write_formula <- function(lhs, rhs) {
  f <- as.formula(paste(lhs, " ~ ",
                        paste(rhs, collapse = " + ")))
  return(f)
} 
