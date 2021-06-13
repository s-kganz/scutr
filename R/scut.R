#' Validate a dataset for resampling.
#'
#' This functions checks that the given column is present in the data and that all columns besides the class column are numeric.
#'
#' @param data Dataframe to validate.
#' @param cls_col Column with class information.
#'
#' @return NA
validate_dataset <- function(data, cls_col) {
  if (!(cls_col %in% names(data))) {
    stop("Column not found in data: ", cls_col)
  }
  if (sum(!unlist(lapply(data, is.numeric))) == is.numeric(data[[cls_col]])) {
    stop("Data frame must be only numeric besides the class column.")
  }
  if (any(is.na(data))) {
    stop("Data frame cannot contain NAs.")
  }
}

#' SMOTE and cluster-based undersampling technique.
#'
#' This function balances multiclass training datasets. In a dataframe with `n` classes and `m` rows, the resulting dataframe will have `m / n` rows per class. \code{\link{SCUT_parallel}} distributes each over/undersampling task across multiple cores. Speedup usually occurs only if there are many classes using one of the slower resampling techniques (e.g. \code{\link{undersample_mclust}}).
#'
#' Custom functions can be used to perform under/oversampling (see the required signature below). Parameters represented by `...` should be passsed via `osamp_opts` or `usamp_opts` as a list.
#'
#' @param cls_col The column in `data` with class membership.
#' @param data Numeric data frame.
#' @param oversample Oversampling method. Must be a function with the signature `foo(data, cls, cls_col, m, ...)` that returns a data frame, one of the `oversample_*` functions, or \code{\link{resample_random}}.
#' @param osamp_opts List of options passed to the oversampling function.
#' @param usamp_opts List of options passed to the undersampling function.
#' @param undersample Undersampling method. Must be a function with the signature `foo(data, cls, cls_col, m, ...)` that returns a data frame, one of the `undersample_*` functions, or \code{\link{resample_random}}.
#' @param ncores Number of cores to use with \code{\link{SCUT_parallel}}.
#' @return A dataframe with equal class distribution.
#' @export
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{agrawal_scut_2015}{scutr}
#'
#' \insertRef{chawla_smote_2002}{scutr}
#'
#' @examples
#' ret <- SCUT(iris, "Species", undersample = undersample_hclust,
#'             usamp_opts = list(dist_calc="manhattan"))
#' ret2 <- SCUT(chickwts, "feed", undersample = undersample_kmeans)
#' table(ret$Species)
#' table(ret2$feed)
SCUT <- function(data, cls_col, oversample = oversample_smote,
                 undersample = undersample_mclust, osamp_opts = list(), usamp_opts = list()) {
  validate_dataset(data, cls_col)

  # target number of observations per class
  m <- round(nrow(data) / length(unique(data[[cls_col]])))
  # bulid skeleton of output
  ret <- as.data.frame(matrix(nrow = 0, ncol = ncol(data)), col.names = names(data))

  for (cls in unique(data[[cls_col]])) {
    n <- sum(data[[cls_col]] == cls)
    if (n < m) {
      d_prime <- do.call(
        oversample,
        c(
          list(
            data = data,
            cls_col = cls_col,
            cls = cls,
            m = m
          ),
          osamp_opts
        )
      )

      ret <- rbind(ret, d_prime)
    }
    else if (n > m) {
      d_prime <- do.call(
        undersample,
        c(
          list(
            data = data,
            cls_col = cls_col,
            cls = cls,
            m = m
          ),
          usamp_opts
        )
      )

      ret <- rbind(ret, d_prime)
    }
    else {
      # this class is already balanced
      ret <- rbind(ret, data[data[[cls_col]] == cls, ])
    }
  }

  rownames(ret) <- NULL
  return(ret)
}

#' @rdname SCUT
#'
#' @export
#'
#' @importFrom parallel detectCores
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach `%dopar%`
#'
#' @examples
#' \dontrun{
#' ret <- SCUT_parallel(wine, "type", ncores = 2, undersample = undersample_kmeans)
#' table(ret$type)
#' }
SCUT_parallel <- function(data, cls_col, ncores = detectCores() %/% 2,
                          oversample = oversample_smote, undersample = undersample_mclust,
                          osamp_opts = list(), usamp_opts = list()) {
  validate_dataset(data, cls_col)

  # vector of classes in the data
  classes <- unique(data[[cls_col]])
  # target number of observations per class
  m <- round(nrow(data) / length(classes))

  # register cores
  registerDoParallel(min(ncores, length(classes)))

  # define a local to make R CMD check happy
  cls <- NA

  d_prime <- foreach(cls = classes, .combine = rbind, .packages = c("scutr")) %dopar% {
    n <- sum(data[[cls_col]] == cls)
    if (n < m) {
      do.call(
        oversample,
        c(
          list(
            data = data,
            cls_col = cls_col,
            cls = cls,
            m = m
          ),
          osamp_opts
        )
      )
    } else if (n > m) {
      do.call(
        undersample,
        c(
          list(
            data = data,
            cls_col = cls_col,
            cls = cls,
            m = m
          ),
          usamp_opts
        )
      )
    } else {
      # this class is already balanced
      data[data[[cls_col]] == cls, ]
    }
  }
  rownames(d_prime) <- NULL
  stopImplicitCluster()
  return(d_prime)
}
