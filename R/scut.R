.do.oversample <- function(data, method, cls.col, minor, n, m) {
    if (method == 'SMOTE'){
        perc.over <- (m / n) * 100
        # attach a column indicating the minor class
        d_temp <- cbind(data, as.factor(data[[cls.col]] == minor))
        colnames(d_temp)[ncol(d_temp)] <- "is_minor__"
        # perform SMOTE, using the minority column as the formula
        d_prime <- SMOTE(is_minor__ ~ ., d_temp, perc.over=perc.over)
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

.do.undersample <- function(data, method, cls, cls.col, m){
    if (method == "mclust"){
        # subset the data to the class of interest and cluster
        col.ind <- which(names(data) == cls.col)
        subset <- data[data[[cls.col]] == cls, -col.ind]
        classif <- Mclust(subset, verbose=F)$classification

        # randomly sample an equal number of observations from each cluster
        inds <- 1:length(classif)
        clust.count <- length(unique(classif))
        if (clust.count == 1){
            warning("Undersampling ", cls, " by Mclust resulted in only one cluster.")
        }
        sample.ind <- c()
        for (cluster in unique(classif)){
            sample.ind <- c(sample.ind, sample(inds[classif == cluster], round(m / clust.count)))
        }
        ret <- data[sample.ind, ]
        ret[[cls.col]] <- cls
        return(ret)

    } else {
        stop("Unrecognized undersample method: ", method)
    }
}

SCUT <- function(form, data, oversample="SMOTE", undersample="mclust") {
    cls.col <- as.character(form[[2]])
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
            d_prime <- .do.oversample(data=data,
                                      method=oversample,
                                      cls.col=cls.col,
                                      minor=cls,
                                      n=n, m=m)

            ret <- rbind(ret, d_prime)
        }
        else {
            # do the undersampling
            d_prime <- .do.undersample(data=data,
                                       method=undersample,
                                       cls=cls,
                                       cls.col=cls.col,
                                       m=m)

            ret <- rbind(ret, d_prime)
        }
    }
    # convert to dataframe and reset the factors
    #ret <- as.data.frame(ret, col.names=names(data), row.names=seq(1:nrow(ret)))
    #for (name in names(fact.levels)){
    #    ret[[name]] <- as.factor(ret[[name]])
    #    levels(ret[[name]]) <- fact.levels[[name]]
    #}
    return(ret)
}
