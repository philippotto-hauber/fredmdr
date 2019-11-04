f_baing <- function(x, Nr_max, ic){
    
    # dim(x)
    Nt <- nrow(x)
    Nn <- ncol(x)
    
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