#' f_pca
#' 
#' Performs principle components analysis on x.
#' 
#'@param x Nt x Nn matrix.
#'@return List containing the first Nr principle components (f), 
#' the associated loadings (lam), the common and idiosyncratic 
#' components (chi and e, respectively) as well as the 
#' eigenvalues (eigvals) of the covariance matrix of x
#'
#'@examples
#'tmp <- f_pca(x, Nr = 8)
#'f <- tmp$f # factors
#'lam <- tmp$lam # loadings
#'
#'@export
#'
f_pca <- function(x, Nr){
    
    # check input is a matrix. If not, convert!
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    
    # dim(x)
    Nn <- ncol(x)
    
    # singular value decomposition
    temp <- svd(t(x) %*% x)
    u <- temp$u
    eigvals <- temp$d
    
    # loadings
    lam <- u[, 1 : Nr] * sqrt(Nn)
    
    # factors
    f <- x %*% lam / Nn
    
    # common and idiosyncratic component
    chi <- f %*% t(lam)
    e <- x - chi
    
    # return as list
    return(list(lam = lam, f = f, chi = chi, e = e, eigvals = eigvals))
}