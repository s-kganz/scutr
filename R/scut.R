#' Oversampling driver function.
#'
#' @param data data.frame
#' @param method character
#' @param cls.col character
#' @param cls character
#' @param m integer
#' @param ... Other options passed to custom function.
#'
#' @return data.frame
#' @export
#'
#' @examples do.oversample(iris, "SMOTE", "Species", "setosa", 70)
do.oversample <- function(data, method, cls.col, cls, m, ...) {
    if (is.function(method))    { return(method(data, cls, cls.col, m, ...)) }
    else if (method == 'SMOTE') { return(scutr::oversample.smote(data, cls, cls.col, m)) }
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
#' @param data data.frame
#' @param method character
#' @param cls.col character
#' @param cls character
#' @param m integer
#' @param ... Other options passed to custom function.
#'
#' @return data.frame
#' @export
#'
#' @examples do.undersample(iris, "mclust", "Species", "setosa", 30)
do.undersample <- function(data, method, cls.col, cls, m, ...){
    # use the custom method to get the undersampled data
    if (is.function(method))    { return(method(data, cls, cls.col, m, ...)) }
    else if (method == "mclust"){ return(scutr::undersample.mclust(data, cls, cls.col, m)) }
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
#' This function balances multiclass training datasets.
#'
#' @param form formula
#' @param data data.frame
#' @param oversample character
#' @param undersample character
#' @param ... Options passed to custom over/undersampling function.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' SCUT(Species ~ ., iris, oversample="SMOTE", undersample="mclust")
#' SCUT(feed ~ ., chickwts, oversample="SMOTE", undersample="random")
SCUT <- function(form, data, oversample="SMOTE", undersample="mclust", ...) {
    cls.col <- as.character(form[[2]])
    if (!cls.col %in% names(data)){
        stop("Class column not found: ", cls.col)
    }
    # target number of observations per class
    m <- round(nrow(data) / length(unique(data[[cls.col]])))
    # bulid skeleton of output
    ret <- as.data.frame(matrix(nrow=0, ncol=ncol(data)), col.names=names(data))
    # convert all the factors to numeric so we can convert it to a matrix
    #fact.cols     <- sapply(data, is.factor)
    #fact.levels   <- lapply(data[, fact.cols], levels)
    #data[, fact.cols] <- lapply(data[, fact.cols],
    #                        function (x) {as.factor(as.numeric(x))})

    for (cls in unique(data[[cls.col]])) {
        n <- sum(data[[cls.col]] == cls)
        if (n < m){
            d_prime <- do.oversample(data=data,
                                      method=oversample,
                                      cls.col=cls.col,
                                      cls=cls,
                                      m=m)

            ret <- rbind(ret, d_prime)
        }
        else if (n > m){
            d_prime <- do.undersample(data=data,
                                       method=undersample,
                                       cls.col=cls.col,
                                       cls=cls,
                                       m=m)

            ret <- rbind(ret, d_prime)
        }
        else {
            # this class is already balanced
            ret <- rbind(ret, data[data[[cls.col]] == cls, ])
        }
    }
    # convert to dataframe and reset the factors
    #ret <- as.data.frame(ret, col.names=names(data), row.names=seq(1:nrow(ret)))
    #for (name in names(fact.levels)){
    #    ret[[name]] <- as.factor(ret[[name]])
    #    levels(ret[[name]]) <- fact.levels[[name]]
    #}
    rownames(ret) <- 1:nrow(ret)
    return(ret)
}

#' Parallel implementation of SCUT.
#'
#' This function balances multiclass training datasets.
#'
#' @param form formula
#' @param data data.frame
#' @param oversample character
#' @param undersample character
#' @param ncores integer
#' @param ... Options passed to custom over/undersampling function.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # TIL that CRAN only allows 2 cores.
#' SCUT.parallel(feed ~ ., chickwts, oversample="SMOTE", undersample="random", ncores=2)
SCUT.parallel <- function(form, data, ncores=parallel::detectCores()-2,
                          oversample="SMOTE", undersample="mclust", ...){
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
            do.oversample(data=data,
                          method=oversample,
                          cls.col=cls.col,
                          cls=cls,
                          m=m)

        } else if (n > m){
            do.undersample(data=data,
                           method=undersample,
                           cls.col=cls.col,
                           cls=cls,
                           m=m)

        } else {
            # this class is already balanced
            data[data[[cls.col]] == cls, ]
        }
    }
    rownames(d_prime) <- 1:nrow(d_prime)
    doParallel::stopImplicitCluster()
    return(d_prime)
}
