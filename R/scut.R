#' Oversampling driver function.
#'
#' @param data Data frame to be oversampled.
#' @param method Oversampling method to use. See `?SCUT` for more information.
#' @param cls.col Column in `data` containing class membership.
#' @param cls Class to be oversampled.
#' @param m Desired number of samples in oversampled data frame.
#' @param ... Other parameters passed to oversampling function.
#'
#' @return data.frame
#' @export
#'
#' @examples do.oversample(iris, "SMOTE", "Species", "setosa", 70)
do.oversample <- function(data, method, cls.col, cls, m, ...) {
    if (is.function(method))    { return(method(data, cls, cls.col, m, ...)) }
    else if (method == 'SMOTE') { return(scutr::oversample.smote(data, cls, cls.col, m, ...)) }
    else if (method == "random"){
        # simple upsampling of rows in the given class
        cls.ind <- c(1:nrow(data))[data[[cls.col]] == cls]
        samp.ind <- sample(cls.ind, m, replace=length(cls.ind) < m)
        return(data[samp.ind, ])
    } else {
        stop("Uncrecognized oversample method: ", method)
    }
}

#' Undersampling driver function.
#'
#' @param data Data frame to be undersampled.
#' @param method Undersampling method to use. See `?SCUT` for more information.
#' @param cls.col Column in `data` containing class membership.
#' @param cls Class to undersample.
#' @param m Desired number of rows in undersampled data frame.
#' @param ... Other parameters passed to undersampling function.
#'
#' @return Undersampled data frame.
#' @export
#'
#' @examples do.undersample(iris, "mclust", "Species", "setosa", 30)
do.undersample <- function(data, method, cls.col, cls, m, ...){
    # use the custom method to get the undersampled data
    if (is.function(method))     { return(method(data, cls, cls.col, m, ...)) }
    else if (method == "mclust") { return(scutr::undersample.mclust(data, cls, cls.col, m, ...)) }
    else if (method == "kmeans") { return(scutr::undersample.kmeans(data, cls, cls.col, m, ...)) }
    else if (method == "mindist"){ return(scutr::undersample.mindist(data, cls, cls.col, m, ...))}
    else if (method == "tomek")  { return(scutr::undersample.tomek(data, cls, cls.col, m, ...))  }
    else if (method == "hclust") { return(scutr::undersample.hclust(data, cls, cls.col, m, ...)) }
    else if (method == "random"){
        # simply pull m rows from the subset
        cls.ind <- c(1:nrow(data))[data[[cls.col]] == cls]
        sample.ind <- sample.int(cls.ind, size=m, replace=m>length(cls.ind))
        return(data[sample.ind, ])
    } else {
        stop("Unrecognized undersample method: ", method)
    }
}

#' SMOTE and cluster-based undersampling technique.
#'
#' This function balances multiclass training datasets. In a dataframe with `n` classes and `m` rows, the resulting dataframe will have `m / n` rows per class. `SCUT.parallel()` distributes each over/undersampling task across multiple cores. Speedup usually occurs only if there are many classes using one of the slower resampling techniques (e.g. `"mclust"`).
#'
#' @param form A formula describing the prediction task. This is generally of the form `cls.col ~ .`, where `cls.col` is the column with class membership in the input data. If specific variables are indicated in the right hand side, the input data will be subsetted to only those columns.
#' @param data Numeric data frame containing all variables given in `form`.
#' @param oversample Oversampling method. Must be one of:
#'  - SMOTE: Synthetic minority oversampling. Generates new data points between neighboring points.
#'  - random: Randomly duplicate rows in the data frame until the desired num
#' @param osamp.opts Custom options passed to the oversampling function.
#' @param usamp.opts Custom options passed to the undersampling function.
#' @param undersample Undersampling method. Must be a function with the signature `foo(data, cls, cls.col, m, ...)` that returns a data frame or one of:
#'  - `"mindist"`: The data point with the lowest average distance to all other datapoints in the data is removed until the desired number of instances is reached.
#'  - `"mclust"`: Representative samples are taken from the results of model-based clustering.
#'  - `"kmeans"`: Same as `"mclust"`, but uses k-means clustering.
#'  - `"hclust"`: Same as `"mclust"`, but uses hierarchical clustering.
#'  - `"tomek"`: Iteratively removes Tomek links from the data until either the desired number of data points remain or until no more Tomek links are present. If desired, random undersampling ensures the correct number of instances is returned.
#' @param ncores Number of cores to use with `SCUT.parallel()`.
#' @return A dataframe with equal class distribution.
#' @export
#'
#' @examples
#' SCUT(Species ~ ., iris, oversample="SMOTE", undersample="mclust")
#' SCUT(feed ~ ., chickwts, oversample="SMOTE", undersample="random")
SCUT <- function(form, data, oversample="SMOTE", undersample="mclust",
                 osamp.opts=list(), usamp.opts=list()) {
    cls.col <- as.character(form[[2]])
    if (!cls.col %in% names(data)){
        stop("Class column not found: ", cls.col)
    }
    # target number of observations per class
    m <- round(nrow(data) / length(unique(data[[cls.col]])))
    # bulid skeleton of output
    ret <- as.data.frame(matrix(nrow=0, ncol=ncol(data)), col.names=names(data))

    for (cls in unique(data[[cls.col]])) {
        n <- sum(data[[cls.col]] == cls)
        if (n < m){
            d_prime <- do.call(do.oversample,
                               c(list(data=data,
                                      method=oversample,
                                      cls.col=cls.col,
                                      cls=cls,
                                      m=m),
                                 osamp.opts))

            ret <- rbind(ret, d_prime)
        }
        else if (n > m){
            d_prime <- do.call(do.undersample,
                               c(list(data=data,
                                      method=undersample,
                                      cls.col=cls.col,
                                      cls=cls,
                                      m=m),
                                 usamp.opts))

            ret <- rbind(ret, d_prime)
        }
        else {
            # this class is already balanced
            ret <- rbind(ret, data[data[[cls.col]] == cls, ])
        }
    }

    rownames(ret) <- 1:nrow(ret)
    return(ret)
}

#' @rdname SCUT
#'
#' @export
#'
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach `%dopar%`
#'
#' @examples
#' SCUT.parallel(feed ~ ., chickwts, oversample="SMOTE", undersample="random")
SCUT.parallel <- function(form, data, ncores=2,
                          oversample="SMOTE", undersample="mclust",
                          osamp.opts=list(), usamp.opts=list()){
    cls.col <- as.character(form[[2]])
    if (!cls.col %in% names(data)){
        stop("Class column not found: ", cls.col)
    }
    # vector of classes in the data
    classes <- unique(data[[cls.col]])
    # target number of observations per class
    m <- round(nrow(data) / length(classes))

    # register cores
    doParallel::registerDoParallel(min(ncores, length(classes)))

    # define local operators to make R CMD check happy
    `%dopar%` <- foreach::`%dopar%`
    cls <- NA

    d_prime <- foreach::foreach(cls=classes, .combine=rbind,
                                .export = c("do.undersample", "do.oversample"),
                                .packages = c("scutr")) %dopar% {
        n <- sum(data[[cls.col]] == cls)
        if (n < m){
            do.call(do.oversample,
                    c(list(data=data,
                           method=oversample,
                           cls.col=cls.col,
                           cls=cls,
                           m=m),
                      osamp.opts))

        } else if (n > m){
            do.call(do.undersample,
                    c(list(data=data,
                           method=undersample,
                           cls.col=cls.col,
                           cls=cls,
                           m=m),
                      usamp.opts))

        } else {
            # this class is already balanced
            data[data[[cls.col]] == cls, ]
        }
    }
    rownames(d_prime) <- NULL
    doParallel::stopImplicitCluster()
    return(d_prime)
}
