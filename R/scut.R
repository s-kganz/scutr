.do.oversample <- function(data, method, cls.col, minor, n, m) {
    if (method == 'SMOTE'){
        perc.over <- (m / n) * 100
        # attach a column indicating the minor class
        d_temp <- cbind(data, as.factor(data[[cls.col]] == minor))
        colnames(d_temp)[ncol(d_temp)] <- "is_minor__"
        # perform SMOTE, using the minority column as the formula
        d_prime <- DMwR::SMOTE(is_minor__ ~ ., d_temp, perc.over=perc.over)
        # filter to the minority class, drop minority column, resample to proper number
        # of observations
        d_prime <- d_prime[d_prime[[cls.col]] == minor, -ncol(d_prime)]
        samp.ind <- sample(1:nrow(d_prime), m, replace=nrow(d_prime) < m)
        return(d_prime[samp.ind, ])
    }
    else {
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
    subset <- data[data[[cls.col]] == cls, -col.ind]

    # use the clustering method to get a vector of cluster classification
    if (is.function(method)){
        # use a custom method
        classif <- method(subset, list(...))
        sample.ind <- .sample.clusters(classif, m)
    }
    else if (method == "mclust"){
        # get cluster classification
        classif <- mclust::Mclust(subset, verbose=F)$classification
        sample.ind <- .sample.clusters(classif, m)

    } else if (method == "kmeans"){
        # only numeric columns are allowed
        if (!all(sapply(subset[, -col.ind], is.numeric))){
            stop("Only numeric data allowed in kmeans undersampling.")
        }
        classif <- kmeans(subset[, -col.ind], centers=k)$cluster
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
#' SCUT(feed ~ ., chickwts, oversample="SMOTE", undersample="kmeans")
SCUT <- function(form, data, oversample="SMOTE", undersample="mclust", k=5) {
    cls.col <- as.character(form[[2]])
    if (!cls.col %in% names(data)){
        stop("Class column not found: ", cls.col)
    }
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
                                      minor=cls,
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
