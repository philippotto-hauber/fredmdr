# clear workspace except for functions
rm(list=setdiff(ls(), c("f_pca", "f_baing")))

load("fredmd201909.Rda")

xdf <- tidyr::spread(fredmd, var, value)

# pivot_wider too slow!!!
#xdf <- tidyr::pivot_wider(data = fredmd, id_cols = c(dates, var) , names_from = c(var), values_from = value) 

# convert xdf to matrix
x <- as.matrix(xdf[, 2 : ncol(xdf)])

Nr_max <- 10
Niter <- 10
ic <- "PC_p1"

#========================================================================#
#= function
#========================================================================#

f_emalg <- function(x, Nr_max = 10, Niter = 50, ic = "PC_p1", print_iter = FALSE){
    
    #========================================================================#
    #= initialisation
    #========================================================================#
    
    Nt <- nrow(x)
    Nn <- ncol(x)    
    
    # mean and sd of columns of xmat
    meanx <- matrix(apply(x, 2, mean, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
    sdx <- matrix(apply(x, 2, sd, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
    
    # pattern of missings in xmat
    isna_x <- is.na(x)
    
    # fill in missings with uncondtional mean
    x[isna_x] <- meanx[isna_x]
    
    # determine number of factors
    Nr <- f_baing((x - meanx) / sdx, Nr_max, ic)
    
    # initial estimate of common component
    temp <- f_pca((x - meanx) / sdx, Nr)
    chi_old <- temp$chi 
    
    #========================================================================#
    #= iterations
    #========================================================================#
    
    err <- 999 # initial error
    iter <- 0 # set iter counter to 0 
    
    while (err > 0.000001 & iter < Niter) {
        
        # update iter
        iter <- iter + 1 
        if (print_iter == TRUE) {
            cat(sprintf("iteration: %i, current error: %.7f, number of factors selected: %i\n", iter, err, Nr)) 
        }
        
        # update missing values
        x[isna_x] <- chi_old[isna_x] * sdx[isna_x] + meanx[isna_x]
        
        # recompute mean and sd
        meanx <- matrix(apply(x, 2, mean, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
        sdx <- matrix(apply(x, 2, sd, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
        
        # determine number of factors
        Nr <- f_baing((x - meanx) / sdx, Nr_max, ic)
        
        # estimate factor
        temp <- f_pca((x - meanx) / sdx, Nr)
        chi <- temp$chi
        f <- temp$f
        lam <- temp$lam
        eigvals <- temp$eigvals
        
        # compute err
        diff_chi <- chi - chi_old
        v1 <- sum(diff_chi ^ 2)
        v2 <- sum(chi_old ^ 2)
        err <- v1 / v2
        
        # overwrite chi_old
        chi_old <- chi
    }
    
    #========================================================================#
    #= output
    #========================================================================#
    return(list(f = f, lam = lam))
}

#========================================================================#
#= test function
#========================================================================#

tmp <- f_emalg(x)
f <- tmp$f
tmp2 <- f_emalg(x, print_iter = TRUE)
f2 <- tmp2$f
tmp3 <- f_emalg(x, print_iter = TRUE, ic = "PC_p2")
f3 <- tmp3$f
tmp4 <- f_emalg(x, print_iter = TRUE, Nr_max = 2, ic = "none")
f4 <- tmp4$f
