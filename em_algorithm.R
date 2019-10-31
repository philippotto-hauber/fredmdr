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

# spread and remove date column
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
    u <- temp[["u"]]
    eigvals <- temp[["d"]]
    
    # loadings
    lam <- u[, 1 : Nr] * sqrt(Nn)
    
    # factors
    f <- x %*% lam / Nn
    
    # common component
    chi <- f %*% t(lam)
    
    # return as list
    return(list(lam = lam, f = f, chi = chi, eigvals = eigvals))
}

Nr <- 4 # number of factors
Niter <- 50 # maximum number of iterations
iter <- 0
err <- 999

# fill in missings with uncondtional mean
xmat[isna_xmat] <- meanx[isna_xmat]

# initial estimate of common component
temp <- f_pca((xmat - meanx) / sdx, Nr)
chi_old <- temp$chi * sdx + meanx

# iterate!
while (err > 0.000001 & iter < Niter) {
    
    # update iter
    iter <- iter + 1 
    print(iter)
    
    # update missing values
    xmat[isna_xmat] <- chi_old[isna_xmat] 
    
    # recompute mean and sd
    meanx <- matrix(apply(xmat, 2, mean, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
    sdx <- matrix(apply(xmat, 2, sd, na.rm = FALSE), nrow = Nt, ncol = Nn, byrow = TRUE)
    
    # estimate factor
    temp <- f_pca((xmat - meanx) / sdx, Nr)
    chi <- temp$chi * sdx + meanx
    f <- temp$f
    eigvals <- temp$eigvals
    
    # compute err
    diff_chi <- chi - chi_old
    v1 <- sum(diff_chi ^ 2)
    v2 <- sum(chi_old ^ 2)
    err <- v1 / v2
    print(err)
    # overwrite chi_old
    chi_old <- chi
}

# plot factors
plot(f[, 1], type = "l", col = "blue")
lines(f[, 2], type = "l", col = "red")
lines(f[, 3], type = "l", col = "green")
lines(f[, 4], type = "l", col = "black")

# compare with MATLAB factors
f_MATLAB <- as.matrix(read.csv("original MATLAB code/FhatMATLAB.csv", header = FALSE))

par(mfrow = c(2,2))

plot(f[, 1], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 1")
lines(f_MATLAB[, 1], type = "l", col = "red")

plot(f[, 2], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 2")
lines(f_MATLAB[, 2], type = "l", col = "red")

plot(f[, 3], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 3")
lines(f_MATLAB[, 3], type = "l", col = "red")

plot(f[, 4], type = "l", col = "blue", xlab = "", ylab = "", main = "factor 4")
lines(f_MATLAB[, 4], type = "l", col = "red")

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
