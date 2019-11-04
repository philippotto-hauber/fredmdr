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