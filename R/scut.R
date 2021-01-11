.do.oversample <- function(data, method, cls, cls.col, n, m, ...) {
    if (is.function(method)){
        return(method(data, cls, cls.col, m, ...))
    }
    else if (method == 'SMOTE'){
        col.ind <- which(names(data) == cls.col)
        # set the class to whether it is equal to the minority class
        data[[cls.col]] <- as.factor(data[[cls.col]] == cls)
        # smotefamily::SMOTE breaks for one-dim datasets. This adds a dummy column
        # so SMOTE can execute. This does not affect how SMOTE performs
        if (ncol(data) == 2) {data$dummy__col__ <- 0}
        # perform SMOTE, using the minority column as the formula
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

.sample.clusters <- function(classif, m){
    # sample m balanced indices from the classes in the classif vector
    inds <- 1:length(classif)
    clust.count <- length(unique(classif))
    sample.ind <- c()
    for (cluster in unique(classif)){
        sample.ind <- c(sample.ind,
                        sample(inds[classif == cluster],
                               round(m / clust.count),
                               replace=T))
    }
    return(sample.ind)
}

.do.undersample <- function(data, method, cls, cls.col, m, k, ...){
    # subset to the data of interest
    col.ind <- which(names(data) == cls.col)
    subset <- data[data[[cls.col]] == cls, ]

    # use the custom method to get a vector of cluster classification
    if (is.function(method)){
        # use a custom method
        sample.ind <- method(subset, cls, cls.col, m, list(...))
    }
    else if (method == "mclust"){
        # drop the class column since it often results in one cluste
        subset <- subset[, -col.ind]
        # get cluster classification
        classif <- mclust::Mclust(subset, verbose=F)$classification
        sample.ind <- .sample.clusters(classif, m)
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
#'
#' @return data.frame
#' @export
#'
#' @examples
#' SCUT(Species ~ ., iris, oversample="SMOTE", undersample="mclust")
#' SCUT(feed ~ ., chickwts, oversample="SMOTE", undersample="random")
SCUT <- function(form, data, oversample="SMOTE", undersample="mclust", k=5) {
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
            # do the oversampling
            d_prime <- scutr::.do.oversample(data=data,
                                      method=oversample,
                                      cls.col=cls.col,
                                      cls=cls,
                                      n=n, m=m)

            ret <- rbind(ret, d_prime)
        }
        else if (n > m){
            # do the undersampling
            d_prime <- scutr::.do.undersample(data=data,
                                       method=undersample,
                                       cls=cls,
                                       cls.col=cls.col,
                                       m=m, k=k)

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
