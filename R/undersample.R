#' Undersample a dataset by iteratively removing the observation with the lowest total distance to its neighbors of the same class.
#'
#' @param data Dataset to undersample. Aside from `cls_col`, must be numeric.
#' @param cls_col Column containing class information.
#' @param m Desired number of observations after undersampling.
#' @param cls Class to be undersampled.
#' @param ... Additional arguments passed to \code{\link[stats]{dist}()}.
#'
#' @return An undersampled dataframe.
#' @export
#'
#' @importFrom stats dist
#'
#' @examples
#' setosa <- iris[iris$Species == "setosa", ]
#' nrow(setosa)
#' undersamp <- undersample_mindist(setosa, "setosa", "Species", 50)
#' nrow(undersamp)
undersample_mindist <- function(data, cls, cls_col, m, ...) {
  # select the class to be undersampled
  col_ind <- which(names(data) == cls_col)
  subset <- data[data[[cls_col]] == cls, ]
  dist_mtx <- as.matrix(dist(subset[, -col_ind], ...))

  # number of times to iterate
  to_discard <- nrow(subset) - m
  discard_inds <- c()

  if (to_discard < 0) {
    stop("Cannot undersample dataset that has fewer rows than desired amount.")
  }

  for (i in 1:to_discard) {
    # calculate the index of the minimum rowSum
    min_ind <- which.min(rowSums(dist_mtx))
    # add the rowname at that index to the discard pile
    discard_inds <- c(discard_inds, min_ind)
    # eliminate that row/col from affecting further calculation
    dist_mtx[, min_ind] <- 0
    dist_mtx[min_ind, ] <- max(dist_mtx)
  }
  d_prime <- subset[-as.integer(discard_inds), ]
  return(d_prime)
}

#' Stratified index sample of different values in a vector.
#'
#' @param vec Vector of values to sample from.
#' @param tot_sample Total number of samples.
#'
#' @return A vector of indices that can be used to select a balanced population of values from `vec`.
#' @export
#'
#' @examples
#' vec <- sample(1:5, 30, replace = TRUE)
#' table(vec)
#' sample_ind <- sample_classes(vec, 15)
#' table(vec[sample_ind])
sample_classes <- function(vec, tot_sample) {
  inds <- 1:length(vec)
  samp_per_class <- ceiling(tot_sample / length(unique(vec)))
  sample_ind <- sapply(
    unique(vec),
    function(x) {
      this_class <- inds[vec == x]
      if (length(this_class) == 1) {
        rep(this_class, samp_per_class)
      } else {
        sample(this_class,
          samp_per_class,
          replace = samp_per_class > sum(vec == x)
        )
      }
    }
  )
  # transpose the resulting dataframe and trim.
  # the transpose ensures that at least samp.per.class-1 instances
  # from each cluster are included
  return(t(sample_ind)[1:tot_sample])
}

#' Undersample a dataset by expectation-maximization clustering
#'
#' @param data Data to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls_col Class column.
#' @param m Number of samples in undersampled dataset.
#' @param ... Additional arguments passed to \code{\link[mclust]{Mclust}()}
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
#' undersamp <- undersample_mclust(setosa, "setosa", "Species", 15)
#' nrow(undersamp)
undersample_mclust <- function(data, cls, cls_col, m, ...) {
  # select the class to be undersampled
  col_ind <- which(names(data) == cls_col)
  subset <- data[data[[cls_col]] == cls, ]
  # get cluster classification
  # class col is dropped bc it often results in only one cluster
  classif <- Mclust(subset[, -col_ind], verbose = FALSE, ...)$classification
  sample_ind <- sample_classes(classif, m)
  return(subset[sample_ind, ])
}

#' Undersample a dataset by kmeans clustering.
#'
#' @param data Dataset to be undersampled.
#' @param cls Class to be undersampled.
#' @param cls_col Column containing class information.
#' @param m Number of samples in undersampled dataset.
#' @param k Number of centers in clustering.
#' @param ... Additional arguments passed to \code{\link[stats]{kmeans}()}
#'
#' @return The undersampled dataframe containing only instances of `cls`.
#' @export
#'
#' @importFrom stats kmeans
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample_kmeans(iris, "setosa", "Species", 15)
#' nrow(undersamp)
undersample_kmeans <- function(data, cls, cls_col, m, k = 5, ...) {
  # select the class to be undersampled
  col_ind <- which(names(data) == cls_col)
  subset <- data[data[[cls_col]] == cls, ]
  # get cluster classification
  # class col is dropped bc it often results in only one cluster
  classif <- kmeans(subset[, -col_ind], centers = k, ...)$cluster
  sample_ind <- sample_classes(classif, m)
  return(subset[sample_ind, ])
}

#' Undersample a dataset by hierarchical clustering.
#'
#' @param data Dataset to be undersampled.
#' @param cls Majority class that will be undersampled.
#' @param cls_col Column in data containing class memberships.
#' @param m Number of samples in undersampled dataset.
#' @param k Number of clusters to derive from clustering.
#' @param h Height at which to cut the clustering tree. `k` must be `NA` for this to be used.
#' @param ... Additional arguments passed to \code{\link[stats]{dist}()}.
#'
#' @return Undersampled dataframe containing only `cls`.
#' @export
#'
#' @importFrom stats cutree
#' @importFrom stats hclust
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample_hclust(iris, "setosa", "Species", 15)
#' nrow(undersamp)
undersample_hclust <- function(data, cls, cls_col, m, k = 5, h = NA, ...) {
  # select the desired class
  col_ind <- which(names(data) == cls_col)
  subset <- data[data[[cls_col]] == cls, ]
  # perform hierarchical clustering
  d <- dist(subset, ...)
  tree <- hclust(d)
  classif <- cutree(tree, k = k, h = h)
  sample_ind <- sample_classes(classif, m)
  return(subset[sample_ind, ])
}

#' Undersample a dataset by removing Tomek links.
#'
#' A Tomek link is a minority instance and majority instance that are each other's nearest neighbor. This function removes sufficient Tomek links that are an instance of cls to yield m instances of cls. If desired, samples are randomly discarded to yield m rows if insufficient Tomek links are in the data.
#'
#' @param data Dataset to be undersampled.
#' @param cls Majority class to be undersampled.
#' @param cls_col Column in data containing class memberships.
#' @param m Desired number of samples in undersampled dataset.
#' @param tomek Definition used to determine if a point is considered a minority in the Tomek link definition.
#'  - `minor`: Minor classes are all those with fewer than `m` instances.
#'  - `diff`: Minor classes are all those that aren't `cls`.
#' @param force_m If `TRUE`, uses random undersampling to discard samples if insufficient Tomek links are present to yield `m` rows of data.
#' @param ... Additional arguments passed to \code{\link[stats]{dist}()}.
#'
#' @return Undersampled dataframe containing only `cls`.
#' @export
#'
#' @importFrom stats dist
#'
#' @examples
#' table(iris$Species)
#' undersamp <- undersample_tomek(iris, "setosa", "Species", 15, tomek = "diff", force_m = TRUE)
#' nrow(undersamp)
#' undersamp2 <- undersample_tomek(iris, "setosa", "Species", 15, tomek = "diff", force_m = FALSE)
#' nrow(undersamp2)
undersample_tomek <- function(data, cls, cls_col, m, tomek = "minor",
                              force_m = TRUE, ...) {
  cls_vec <- data[[cls_col]]
  if (tomek == "diff") {
    is_minor <- cls_vec != cls
  } else if (tomek == "minor") {
    minor_classes <- Filter(function(c) sum(cls_vec == c) < m, unique(cls_vec))
    is_minor <- cls_vec %in% minor_classes
  }
  dmtx <- as.matrix(dist(data[, -which(names(data) == cls_col)], ...))
  diag(dmtx) <- max(dmtx)

  tomeks <- c()
  to_remove <- sum(cls_vec == cls) - m
  while (length(tomeks) < to_remove) {
    # determine all the tomek links in the data
    nearest <- apply(dmtx, 1, which.min)
    new_tomeks <- Filter(function(x) {
      neighbor <- nearest[x]
      nearest[neighbor] == x && cls_vec[x] == cls &&
        !(x %in% tomeks) && is_minor[neighbor]
    }, nearest)
    new_tomeks <- new_tomeks[!duplicated(new_tomeks)]
    if (length(new_tomeks) == 0) break
    tomeks <- c(tomeks, new_tomeks)
    dmtx[, new_tomeks] <- max(dmtx)
  }
  if ((length(tomeks) < to_remove) && force_m) {
    # add more indices until we get enough
    maybe_drop <- Filter(
      function(x) !(x %in% tomeks) && cls_vec[x] == cls,
      c(1:nrow(data))
    )
    tomeks <- c(tomeks, sample(maybe_drop, to_remove - length(tomeks)))
  }
  # drop the Tomek links then filter to the class of interest
  if (length(tomeks) > 0) {
    d_prime <- data[-tomeks, ]
  }
  else {
    d_prime <- data
  }

  return(d_prime[d_prime[[cls_col]] == cls, ])
}

#' Randomly resample a dataset.
#'
#' This function is used to resample a dataset by randomly removing or duplicating rows. It is usable for both oversampling and undersampling.
#'
#' @param data Dataframe to be resampled.
#' @param cls Class that should be randomly resampled.
#' @param cls_col Column containing class information.
#' @param m Desired number of samples.
#'
#' @return Resampled dataframe containing only `cls`.
#' @export
#'
#' @examples
#' set.seed(1234)
#' only2 <- resample_random(wine, 2, "type", 15)
resample_random <- function(data, cls, cls_col, m) {
  subset <- data[data[[cls_col]] == cls, ]
  if (m > nrow(subset)) {
    inds <- 1:nrow(subset)
  }
  else {
    inds <- c()
  }
  inds <- c(inds, sample.int(nrow(subset),
    size = m - length(inds),
    replace = (m - length(inds)) > nrow(subset)
  ))
  return(subset[inds, ])
}
