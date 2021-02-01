# Helper functions for testing

no.dupe.rows <- function(df){
    !any(duplicated(df))
}

loosely.balanced <- function(vector){
    # all elements should be within 1 of the max
    # this is used for checking cluster sampling
    counts <- table(vector)
    all((max(counts) - counts) <= 1)
}

strictly.balanced <- function(vector){
    # this is used for checking class distribution
    counts <- table(vector)
    all(max(counts) == counts)
}
