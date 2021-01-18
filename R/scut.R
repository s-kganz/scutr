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
    if (is.function(method)){
        return(method(data, cls, cls.col, m, ...))
    }
    else if (method == 'SMOTE'){
        col.ind <- which(names(data) == cls.col)
        # set the class to whether it is equal to the minority class
        data[[cls.col]] <- as.factor(data[[cls.col]] == cls)
        # smotefamily::SMOTE breaks for one-dim datasets. This adds a dummy column
        # so SMOTE can execute in that case. This does not affect how data is synthesized
        if (ncol(data) == 2) {data$dummy__col__ <- 0}
        # perform SMOTE
        smoteret <- smotefamily::SMOTE(data[-col.ind], data[, col.ind])
        # rbind the original observations and sufficient samples of the synthetic ones
        orig <- smoteret$orig_P
        target.samp <- m - nrow(orig)
        synt <- smoteret$syn_data[sample.int(nrow(smoteret$syn_data),
                                             size=target.samp,
                                             replace=target.samp > nrow(smoteret$syn_data)), ]
        d_prime <- rbind(orig, synt)
        colnames(d_prime)[ncol(d_prime)] <- cls.col
        d_prime[[cls.col]] <- cls
        # remove the dummy column if necessary
        d_prime <- d_prime[, names(d_prime) != "dummy__col__"]
        return(d_prime)
    }
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
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#'
#' @examples do.undersample(iris, "mclust", "Species", "setosa", 30)
do.undersample <- function(data, method, cls.col, cls, m, ...){
    # subset to the data of interest
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]

    # use the custom method to get the undersampled data
    if (is.function(method)){
        # use a custom method
        return(method(subset, cls, cls.col, m, list(...)))
    }
    else if (method == "mclust"){
        # drop the class column since it often results in one cluster
        subset <- subset[, -col.ind]
        # get cluster classification
        classif <- mclust::Mclust(subset, verbose=F)$classification
        # sample equal number of obs per cluster
        inds <- 1:length(classif)
        samp.per.clust <- ceiling(m / length(unique(classif)))
        sample.ind <- sapply(unique(classif),
                             function(x) {
                                 sample(inds[classif == x],
                                        samp.per.clust,
                                        replace=samp.per.clust > sum(classif == x))})[1:m]
    } else if (method == "random"){
        # simply pull m rows from the subset
        sample.ind <- sample.int(nrow(subset), size=m, replace=m>nrow(subset))
    } else {
        stop("Unrecognized undersample method: ", method)
    }

    ret <- data[sample.ind, ]
    ret[[cls.col]] <- cls
    return(ret)
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
#' SCUT(Species ~ ., iris, oversample="SMOTE", undersample="mclust")
#' SCUT(feed ~ ., chickwts, oversample="SMOTE", undersample="random")
SCUT.parallel <- function(form, data, ncores=doParallel::detectCores()-2,
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
