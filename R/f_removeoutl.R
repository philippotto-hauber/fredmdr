f_removeoutl <- function(x){
    ########################################################    
    # removes outliers from Nt x Nn matrix x, 
    # defined as any observation larger than 10 times 
    # the interquartile range (in absolute value)
    ########################################################    
    
    # back out dimensions of x    
    Nt <- nrow(x) ; Nn <- ncol(x)
    
    # initialize xoutl (carrying over row and column names)
    xoutl <- x
    
    # calculate median of columns of x
    medx <- matrix(apply(x, 2, median, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
    
    # calculate interquartile range of columns of x
    iqrx <- matrix(apply(x, 2, quantile, probs = c(0.75), na.rm = TRUE) - 
                       apply(x, 2, quantile, probs = c(0.25), na.rm = TRUE), 
                   nrow = Nt, ncol = Nn, byrow = TRUE)
    
    # determine outliers
    z <- abs(x-medx);
    outlier <- z > (10 * iqrx)
    
    # remove and count
    xoutl[outlier == TRUE] <- NA
    noutl <- apply(outlier, 2, sum, na.rm = TRUE)
    
    # return output
    return(list(xoutl = xoutl, noutl = noutl))
}