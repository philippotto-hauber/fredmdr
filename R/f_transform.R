#' f_transform
#' 
#' Transforms the FRED-MD series to achieve stationarity. The codes are (see McCracken and Ng 2016, Appendix):
#' 1 = no transformation
#' 2 = first difference
#' 3 = second difference
#' 4 = log
#' 5 = first difference of logs
#' 6 = second difference of logs
#' 7 = first difference of percentage change
#' 
#'@param x Nt x Nn matrix
#'@param trafos Vector or dataframe of transformation codes (of length Nn)
#'@return Matrix of transformed series 
#'
#'@examples
#'x_transformed <- f_transform(x, trafos)
#'
#'@export
#'
f_transform <- function(x, trafos){
    
    # back out dimensions
    Nt <- nrow(x) 
    Nn <- ncol(x)
    
    # check that number of columns equals length of trafos
    if (Nn != length(trafos)){
        print("dimensions of x and trafos do not match. Abort!")
        break
    }
    
    # output matrix (carrying over row and col names)
    xtrafo <- x
    xtrafo[] <- NA
    
    # loop over Nn
    for (i in 1:Nn) {
        if (trafos[i] == 1) # no transformation
            xtrafo[, i] <- x[, i]
        
        else if (trafos[i] == 2) # first difference
            xtrafo[ 2 : Nt, i] <- diff(x[, i])
        
        else if (trafos[i] == 3) # second difference
            xtrafo[ 3 : Nt, i] <- diff(x[, i], differences = 2)
        
        else if (trafos[i] == 4) # logs
            xtrafo[, i] <- log(x[, i])
        
        else if (trafos[i] == 5) # log first differences
            xtrafo[ 2 : Nt, i] <- diff(log(x[, i]))
        
        else if (trafos[i] == 6) # log second differenes
            xtrafo[ 3 : Nt, i] <- diff(log(x[, i]), differences = 2)
        
        else if (trafos[i] == 7) # difference of percentage change
            xtrafo[ 3 : Nt, i] <- diff(x[2 : Nt, i] / x[1 : (Nt-1), i] - 1)
    } 
    return(xtrafo)
}