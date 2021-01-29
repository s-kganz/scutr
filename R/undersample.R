#' Undersample a dataset by iteratively removing the observation with the lowest total distance to its neighbors of the same class.
#'
#' @param data Dataset to undersample. Aside from `cls.col`, must be numeric.
#' @param cls.col Column containing class information.
#' @param m Desired number of observations after undersampling.
#' @param cls Unused, but kept here for compatability with `do.undersample()`.
#' @param dist.calc Method for distance calculation. See `dist()`.
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
        discard.inds <- c(discard.inds, min.ind)
        # eliminate that row/col from affecting further calculation
        dist.mtx[, min.ind] <- 0
        dist.mtx[min.ind, ] <- max(dist.mtx)
    }
    d_prime <- subset[-as.integer(discard.inds), ]
    return(d_prime)
}

#' Stratified index sample of different values in a vector.
#'
#' @param vec Vector of values to sample from.
#' @param tot.sample Total number of samples.
#'
#' @return A vector of indices that can be used to select a balanced population of values from `vec`.
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
                             this.class <- inds[vec == x]
                             if (length(this.class) == 1){
                                 rep(this.class, samp.per.class)
                             } else {
                                 sample(this.class,
                                        samp.per.class,
                                        replace=samp.per.class > sum(vec == x))
                             }
                         })
    # transpose the resulting dataframe and trim.
    # the transpose ensures that at least samp.per.class-1 instances
    # from each cluster are included
    return(t(sample.ind)[1:tot.sample])
}

#' Undersample a dataset by expectation-maximization clustering
#'
#' @param data Data to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls.col Class column.
#' @param m Number of samples in undersampled dataset.
#'
#' @return The undersampled dataframe containing only instance of `cls`.
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
#' @param m Number of samples in undersampled dataset.
#' @param k Number of centers in clustering.
#'
#' @return The undersampled dataframe containing only instances of `cls`.
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
#' @param m Number of samples in undersampled dataset.
#' @param k Number of clusters to derive from clustering.
#' @param h Height at which to cut the clustering tree. `k` must be `NA` for this to be used.
#' @param dist.calc Distance calculation method. See `dist`.
#'
#' @return Undersampled dataframe containing only `cls`.
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

#' Undersample a dataset by removing Tomek links.
#'
#' A Tomek link is a minority instance and majority instance that are each other's nearest neighbor. This function removes sufficient Tomek links that are an instance of cls to yield m instances of cls. If desired, samples are randomly discarded to yield m rows if insufficient Tomek links are in the data.
#'
#' @param data Dataset to be undersampled.
#' @param cls Majority class to be undersampled.
#' @param cls.col Column in data containing class memberships.
#' @param m Desired number of samples in undersampled dataset.
#' @param tomek Definition used to determine if a point is considered a minority in the Tomek link definition.
#'  - `minor`: Minor classes are all those with fewer than `m` instances.
#'  - `diff`: Minor classes are all those that aren't `cls`.
#' @param force.m If `TRUE`, uses random undersampling to discard samples if insufficient Tomek links are present to yield `m` rows of data.
#' @param dist.calc Distance calculation method. See `dist`.
#'
#' @return Undersampled dataframe containing only `cls`.
#' @export
#'
#' @importFrom stats dist
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample.tomek(iris, "setosa", "Species", 15, tomek="diff", force.m=TRUE)
#' nrow(undersamp)
#' undersamp2 <- undersample.tomek(iris, "setosa", "Species", 15, tomek="diff", force.m=FALSE)
#' nrow(undersamp2)
undersample.tomek <- function(data, cls, cls.col, m, tomek="minor",
                              force.m=T, dist.calc="euclidean"){
    cls.vec <- data[[cls.col]]
    if (tomek == "diff"){
        is.minor <- cls.vec != cls
    } else if (tomek == "minor"){
        minor.classes <- Filter(function(c) sum(cls.vec == c) < m, unique(cls.vec))
        is.minor <- cls.vec %in% minor.classes
    }
    dmtx <- as.matrix(dist(data[, -which(names(data) == cls.col)], method=dist.calc))
    diag(dmtx) <- max(dmtx)

    tomeks <- c()
    to.remove <- sum(cls.vec == cls) - m
    while (length(tomeks) < to.remove){
        # determine all the tomek links in the data
        nearest <- apply(dmtx, 1, which.min)
        new.tomeks <- Filter(function(x){
            neighbor <- nearest[x]
            nearest[neighbor] == x && cls.vec[x] == cls &&
                !(x %in% tomeks) && is.minor[neighbor]
            }, nearest)
        new.tomeks <- new.tomeks[!duplicated(new.tomeks)]
        if (length(new.tomeks) == 0) break
        tomeks <- c(tomeks, new.tomeks)
        dmtx[, new.tomeks] <- max(dmtx)
    }
    if ((length(tomeks) < to.remove) && force.m){
        # add more indices until we get enough
        maybe.drop <- Filter(
            function(x) !(x %in% tomeks) && cls.vec[x] == cls,
            c(1:nrow(data)))
        tomeks <- c(tomeks, sample(maybe.drop, to.remove - length(tomeks)))
    }
    # drop the Tomek links then filter to the class of interest
    if (length(tomeks) > 0) {d_prime <- data[-tomeks, ]}
    else {d_prime <- data}

    d_prime[d_prime[[cls.col]] == cls, ]
}

#' Randomly resample a dataset.
#'
#' This function is used to resample a dataset by randomly removing or duplicating rows. It is usable for both oversampling and undersampling.
#'
#' @param data Dataframe to be resampled.
#' @param cls Class that should be randomly resampled.
#' @param cls.col Column containing class information.
#' @param m Desired number of samples.
#'
#' @return Resampled dataframe containing only `cls`.
#' @export
#'
#' @examples
#' set.seed(1234)
#' only2 <- undersample.random(wine, 2, "type", 15)
resample.random <- function(data, cls, cls.col, m){
    subset <- data[data[[cls.col]] == cls, ]
    if (m > nrow(subset)) {inds <- 1:nrow(subset)}
    else {inds <- c()}
    inds <- c(inds, sample.int(nrow(subset),
                               size=m - length(inds),
                               replace=(m-length(inds)) > nrow(subset)))
    subset[inds, ]
}
