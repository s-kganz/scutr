# Helper functions for testing

no_dupe_rows <- function(df) {
  !any(duplicated(df))
}

loosely_balanced <- function(vector) {
  # all elements should be within 1 of the max
  # this is used for checking cluster sampling
  counts <- table(vector)
  all((max(counts) - counts) <= 1)
}

strictly_balanced <- function(vector) {
  # this is used for checking class distribution
  counts <- table(vector)
  all(max(counts) == counts)
}

# set the random seed for reproducibility
set.seed(1234)
