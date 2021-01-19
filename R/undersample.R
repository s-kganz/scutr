#' Undersample a dataset by iteratively removing the observation with the lowest total distance to its neighbors.
#'
#' @param data Dataset to undersample. Aside from cls.col, must be numeric.
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
#' undersamp <- undersample.mindist(setosa, "setosa", "Species", 50)
#' nrow(undersamp)
undersample.mindist <- function(data, cls, cls.col, m, dist.calc="euclidean"){
    # select the class to be undersampled
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]
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

#' Undersample a dataset by expectation-maximization algorithm.
#'
#' @param data Data to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls.col Class column.
#' @param m Desired number of observations.
#'
#' @return The undersampled dataframe.
#' @export
#'
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#'
#' @examples
#' setosa <- iris[iris$Species == "setosa", ]
#' nrow(setosa)
#' undersamp <- undersample.mclust(setosa, "setosa", "Species", 15)
#' nrow(undersamp)
undersample.mclust <- function(data, cls, cls.col, m){
    # select the class to be undersampled
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]
    # get cluster classification
    # class col is dropped bc it often results in only one cluster
    classif <- Mclust(subset[, -col.ind], verbose=F)$classification
    # sample equal number of obs per cluster
    inds <- 1:length(classif)
    samp.per.clust <- ceiling(m / length(unique(classif)))
    sample.ind <- sapply(unique(classif),
                         function(x) {
                             sample(inds[classif == x],
                                    samp.per.clust,
                                    replace=samp.per.clust > sum(classif == x))})[1:m]
    return(subset[sample.ind, ])
}
