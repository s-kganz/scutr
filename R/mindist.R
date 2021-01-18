#' Undersample a dataset by iteratively removing the observation with the lowest total distance to its neighbors.
#'
#' @param subset Dataset to undersample. Aside from cls.col, must be numeric.
#' @param cls.col Column containing class information.
#' @param m Desired number of observations after undersampling.
#' @param cls Unused, but kept here for compatability with do.undersample().
#' @param dist.calc Method for distance calculation. Must be usable with dist().
#'
#' @return An undersampled dataframe.
#' @export
#'
#' @importFrom stats dist
#'
#' @examples
#' setosa <- iris[iris$Species == "setosa", ]
#' nrow(setosa)
#' undersamp <- mindist.undersample(setosa, "setosa", "Species", 50)
#' nrow(undersamp)
mindist.undersample <- function(subset, cls, cls.col, m, dist.calc="euclidean"){
    # ignore the class column in the distance matrix since it isn't numeric
    col.ind <- which(names(subset) == cls.col)
    dist.mtx <- as.matrix(dist(subset[, -col.ind], method=dist.calc))

    # number of times to iterate
    to.discard <- nrow(subset) - m
    discard.inds <- c()

    if (to.discard < 0){
        stop("Cannot undersample dataset that has fewer rows than desired amount.")
    }

    for (i in 1:to.discard){
        # calculate the index of the minimum rowSum
        min.ind <- which.min(rowSums(dist.mtx))
        # add the rowname at that index to the discard pile
        discard.inds <- c(discard.inds, rownames(dist.mtx)[min.ind])
        # remove the row/col from distance matrix
        dist.mtx <- dist.mtx[-min.ind, -min.ind]
    }
    d_prime <- subset[-as.integer(discard.inds), ]
    rownames(d_prime) <- 1:nrow(d_prime)
    return(d_prime)
}
