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

#' Stratified index sample of different values in a vector.
#'
#' @param vec Vector of values to sample from.
#' @param tot.sample The desired total number of samples.
#'
#' @return A vector of indices that can be used to select a balanced population of values from vec.
#' @export
#'
#' @examples
#' vec <- sample(1:5, 30, replace=TRUE)
#' table(vec)
#' sample.ind <- sample.classes(vec, 15)
#' table(vec[sample.ind])
sample.classes <- function(vec, tot.sample){
    inds <- 1:length(vec)
    samp.per.class <- ceiling(tot.sample / length(unique(vec)))
    sample.ind <- sapply(unique(vec),
                         function(x) {
                             sample(inds[vec],
                                    samp.per.class,
                                    replace=samp.per.class > sum(vec == x))
                         })
    # transpose the resulting dataframe and trim.
    # the transpose ensures that at least samp.per.class-1 instances
    # from each cluster are included
    return(t(sample.ind)[1:tot.sample])
}

#' Undersample a dataset by expectation-maximization algorithm.
#'
#' @param data Data to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls.col Class column.
#' @param m Desired number of observations.
#'
#' @return The undersampled dataframe containing only instance of cls.
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
    sample.ind <- sample.classes(classif, m)
    subset[sample.ind, ]
}

#' Undersample a dataset by kmeans clustering.
#'
#' @param data Dataset to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls.col Column containing class information.
#' @param m Desired number of samples.
#' @param k Number of centers in clustering.
#'
#' @return The undersampled dataframe containing only instances of cls.
#' @export
#'
#' @importFrom stats kmeans
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample.kmeans(iris, "setosa", "Species", 15)
#' nrow(undersamp)
undersample.kmeans <- function(data, cls, cls.col, m, k=5){
    # select the class to be undersampled
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]
    # get cluster classification
    # class col is dropped bc it often results in only one cluster
    classif <- kmeans(subset[, -col.ind], centers=k)$cluster
    sample.ind <- sample.classes(classif, m)
    subset[sample.ind, ]
}

#' Undersample a dataset by hierarchical clustering.
#'
#' @param data Dataset to be undersampled.
#' @param cls Majority class that will be undersampled.
#' @param cls.col Column in data containing class memberships.
#' @param m Desired number of samples in undersampled dataset.
#' @param k Desired number of clusters to derive from clustering.
#' @param h Desired height at which to cut the clustering tree. k must be NA for this to be used.
#' @param dist.calc Distance calculation method.
#'
#' @return Undersampled dataframe containing only cls.
#' @export
#'
#' @importFrom stats cutree
#' @importFrom stats hclust
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample.hclust(iris, "setosa", "Species", 15)
#' nrow(undersamp)
undersample.hclust <- function(data, cls, cls.col, m, k=5, h=NA, dist.calc="euclidean"){
    # select the desired class
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]
    # perform hierarchical clustering
    d <- dist(subset, method=dist.calc)
    tree <- hclust(d)
    classif <- cutree(tree, k=k, h=h)
    sample.ind <- sample.classes(classif, m)
    subset[sample.ind, ]
}
