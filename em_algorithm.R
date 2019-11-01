########################################
## R implementation of FRED-MD codes
## EM algorithm
########################################

# clear workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Philipp/Documents/GitHub/fredmdr")
#setwd("C:/Users/Hauber/Desktop/fredmdr")

# load in data
data <- load("fredmd201909.Rda")

# spread 
library(tidyr)
#xdf <- spread(fredmd, var, value)
xdf <- pivot_wider(fredmd, names_from = c(var), values_from = value)

# convert xdf to matrix
xmat <- as.matrix(xdf[, 2 : dim(xdf)[2]])
rownames(xmat) <- xdf$dates
Nt <- dim(xmat)[1]
Nn <- dim(xmat)[2]

# mean and sd of columns of xmat
meanx <- matrix(apply(xmat, 2, mean, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
sdx <- matrix(apply(xmat, 2, sd, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)

# pattern of missings in xmat
isna_xmat <- is.na(xmat)

#======================================#
# EM algorithm
#======================================#

# functions
f_pca <- function(x, Nr){
    
    # check input is a matrix. If not, convert!
    if (!is.matrix(x)) {
        x <- as.matrix(x)
    }
    
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

# fill in missings with uncondtional mean
xmat[isna_xmat] <- meanx[isna_xmat]

# function to determine number of factors
f_baing <- function(x, Nr_max, ic){

    # check information criterion 
    if (ic == "none"){
        Nr <- Nr_max
        return(Nr)
        
    } else if (ic == "PC_p1"){
        crit <- log((Nt * Nn) / (Nt + Nn)) * (1 : Nr_max) * (Nt + Nn) / (Nt * Nn)
        
    } else if (ic == "PC_p2"){
        crit <- ((Nt + Nn) / (Nt * Nn)) * log(min(Nt,Nn)) * (1 : Nr_max)
        
    } else if (ic == "PC_p3"){
        crit <- log(min(Nt,Nn)) / min(Nt,Nn) * (1 : Nr_max)
        
    } else {
        print("No valid criterion selected. Nr = 0!")
        Nr <- 0
        return(Nr)
    }
    
    # loop over number of factors to compute idiosyncratic components
    sigma <- vector()
    for (r in 1 : Nr_max) {
        temp <- f_pca(x, r)
        e <- temp$e 
        sigma[r] <- sum(e ^ 2) / (Nt * Nn)
        crit[r] <- log(sigma[r]) + crit[r] 
    }
    
    # check model with no factors
    crit0 <- sum( x ^ 2 ) / (Nt * Nn)
    
    # find number of factors for which criterion is minimized
    Nr <- ifelse(crit0 < min(crit), 0, which.min(crit)) 
    return(Nr)  
}
# 
# # determine number of factors
# ic <- "p1"
# Nr_max <- 10
# rr <- 1 : Nr_max
# crit <- log((Nt * Nn) / (Nt + Nn)) * rr * (Nt + Nn) / (Nt * Nn)
# crit <- ((Nt + Nn) / (Nt * Nn)) * log(min(Nt,Nn)) * rr
# crit <- log(min(Nt,Nn)) / min(Nt,Nn) * rr
# 
# # PCA
# temp <- f_pca((xmat - meanx) / sdx, Nr_max)
# f0 <- temp$f
# lam0 <- temp$lam


# EM algorithm options
Niter <- 50 # maximum number of iterations
iter <- 0
err <- 999

# determine number of factors
Nr_max <- 10
ic <- "PC_p1"
Nr <- f_baing((xmat - meanx) / sdx, Nr_max, ic)

# initial estimate of common component
temp <- f_pca((xmat - meanx) / sdx, Nr)
chi_old <- temp$chi 

# iterate!
while (err > 0.000001 & iter < Niter) {
    
    # update iter
    iter <- iter + 1 
    print(iter)
    print(err)
    print(Nr)
    
    # update missing values
    xmat[isna_xmat] <- chi_old[isna_xmat] * sdx[isna_xmat] + meanx[isna_xmat]
    
    # recompute mean and sd
    meanx <- matrix(apply(xmat, 2, mean, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
    sdx <- matrix(apply(xmat, 2, sd, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
    
    # determine number of factors
    Nr <- f_baing((xmat - meanx) / sdx, Nr_max, ic)
    
    # estimate factor
    temp <- f_pca((xmat - meanx) / sdx, Nr)
    chi <- temp$chi
    f <- temp$f
    eigvals <- temp$eigvals
    
    # compute err
    diff_chi <- chi - chi_old
    v1 <- sum(diff_chi ^ 2)
    v2 <- sum(chi_old ^ 2)
    err <- v1 / v2

    # overwrite chi_old
    chi_old <- chi
}

# compare with MATLAB factors
f_MATLAB <- as.matrix(read.csv("original MATLAB code/FhatMATLAB.csv", header = FALSE))

par(mfrow = c(4,2))

plot(f[, 1], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 1")
lines(f_MATLAB[, 1], type = "l", col = "red")

plot(f[, 2], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 2")
lines(f_MATLAB[, 2], type = "l", col = "red")

plot(f[, 3], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 3")
lines(f_MATLAB[, 3], type = "l", col = "red")

plot(f[, 4], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 4")
lines(f_MATLAB[, 4], type = "l", col = "red")

plot(f[, 5], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 5")
lines(f_MATLAB[, 5], type = "l", col = "red")

plot(f[, 6], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 6")
lines(f_MATLAB[, 6], type = "l", col = "red")

plot(f[, 7], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 7")
lines(f_MATLAB[, 7], type = "l", col = "red")

plot(f[, 8], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 8")
lines(f_MATLAB[, 8], type = "l", col = "red")

par(mfrow=c(1,1))

# trace R2
Pf <- f_MATLAB %*% solve(t(f_MATLAB) %*% f_MATLAB) %*% t(f_MATLAB)
tr_num <- sum(diag(t(f) %*% Pf %*% f))
tr_denom <- sum(diag(t(f) %*% f))
traceR2 <- tr_num / tr_denom 

# other way around?
Pf <- f %*% solve(t(f) %*% f) %*% t(f)
tr_num <- sum(diag(t(f_MATLAB) %*% Pf %*% f_MATLAB))
tr_denom <- sum(diag(t(f_MATLAB) %*% f_MATLAB))
traceR2_b <- tr_num / tr_denom 

# tidy up factors
fdf <- data.frame(dates = xdf$dates, f = f)
