########################################
## R implementation of FRED-MD codes
########################################

# clear workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Philipp/Documents/GitHub/fredmdr")
setwd("C:/Users/Hauber/Desktop/fredmdr")

# load in data
data <- read.csv("2019-09.csv")

# extract trafos
trafos <- data[1, ]
data <- data[2 : dim(data)[1], ]

# convert first column to date format
data1 <- data
rownames(data1) <- seq(as.Date("1959-01-01"), by = "months", length.out = dim(data1)[1])
data1 <- data1[, -1]
trafos <- trafos[, -1]

# # convert entire matrix to ts format, removing first colum
# data2 <- ts(data, c(1959,1), frequency = 12)
# data2 <- data2[, -1]

# # gather data1
# library(tidyr)
# data1_gathered <- gather(data1, mnemonic, value)

# transform

# Nt <- dim(data)[1]
# Nn <- dim(data1)[2]
# data1_trafo <- matrix(NA_real_, Nt, Nn)
# for (i in 1:Nn) {
#   if (trafos[i] == 1) # no transformation
#     data1_trafo[, i] <- data1[, i]
#   else if (trafos[i] == 2) # first difference
#     data1_trafo[ 2 : Nt, i] <- diff(data1[, i])
#   else if (trafos[i] == 3) # second difference
#     data1_trafo[ 3 : Nt, i] <- diff(data1[, i], differences = 2)
#   else if (trafos[i] == 4) # logs
#     data1_trafo[, i] <- log(data[, i])
#   else if (trafos[i] == 5) # log first differences
#     data1_trafo[ 2 : Nt, i] <- diff(log(data1[, i]))
#   else if (trafos[i] == 6) # log second differenes
#     data1_trafo[ 3 : Nt, i] <- diff(log(data[, i]), differences = 2)
#   else if (trafos[i] == 7) # difference of percentage change
#     data1_trafo[ 3 : Nt, i] <- diff(data1[2 : Nt, i] / data1[1 : (Nt-1), i] - 1)
# }

# function to transform data
f_transform <- function(x, trafos){
  
  # back out dimensions
  Nt <- dim(x)[1] ;  Nn <- dim(x)[2]
  # check that number of columns equals length of trafos
  if (Nn != length(trafos)){
    print("dimensions of x and trafos does not match. Abort!")
    break
  }
  
  # output matrix (carrying over row and col names)
  xtrafo <- matrix(NA_real_, Nt, Nn)
  rownames(xtrafo) <- rownames(x) ;  names(xtrafo) <- names(x)
  
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

data1_trafo <- f_transform(data1[setdiff(names(data1), "dates")], trafos)

# convert to data frame and remove first two rows
data1_trafo <- as.data.frame(data1_trafo)
rownames(data1_trafo) <- rownames(data1)
names(data1_trafo) <- names(data1)
#data1_trafo <- data1_trafo[3 : Nt, ]

# # remove outliers
# data1_trafo_xoutl <- data1_trafo
# median_data <- matrix(apply(data1_trafo, 2, median, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
# iqr_data <- matrix(apply(data1_trafo, 2, quantile, probs = c(0.75), na.rm = TRUE) - apply(data1_trafo, 2, quantile, probs = c(0.25), na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
# z <- abs(data1_trafo-median_data);
# outlier <- z > (10 * iqr_data)
# data1_trafo_xoutl[outlier == TRUE] <- NA
# n_outl <- apply(outlier, 2, sum, na.rm = TRUE)

# function to remove outliers
f_removeoutl <- function(x){
  Nt <- dim(x)[1] ; Nn <- dim(x)[2]
  xoutl <- x
  medx <- matrix(apply(x, 2, median, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
  iqrx <- matrix(apply(x, 2, quantile, probs = c(0.75), na.rm = TRUE) - apply(data1_trafo, 2, quantile, probs = c(0.25), na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
  z <- abs(x-medx);
  outlier <- z > (10 * iqrx)
  xoutl[outlier == TRUE] <- NA
  n_outl <- apply(outlier, 2, sum, na.rm = TRUE)
  return(list(xoutl = xoutl, n_outl = n_outl))
}

data1_trafo_xoutl <- f_removeoutl(data1_trafo)

# convert transformed and outlier-free data set to ts format
data1_trafo_xoutl_ts <- ts(data1_trafo_xoutl, c(1959,1), frequency = 12)

# plot series to compare with MATLAB
ind_var <- 10
plot(data1_trafo_xoutl_ts[, ind_var], type = "l", main = names(data1_trafo_xoutl)[ind_var], ylab = "", xlab = "")


# standardize series
f_standardize <- function(x, demean = TRUE, unitvar = TRUE) {
  # standardizes Nt x Nn matrix x
  Nt <- dim(x)[1] ; Nn <- dim(x)[2] ; xstand <- x
  
  if (demean == TRUE & unitvar == TRUE) {
    xstand <- xstand - matrix(apply(xstand, 2, mean, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
    xstand <- xstand / matrix(apply(xstand, 2, sd, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
  } else if (demean == TRUE & unitvar == FALSE) {
    xstand <- xstand - matrix(apply(xstand, 2, mean, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
  } else if (unitvar == TRUE & demean == FALSE) {
    xstand <- xstand / matrix(apply(xstand, 2, sd, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
  }
  return(xstand)
}

data1_trafo_xoutl_standardized <- f_standardize(data1_trafo_xoutl)


# re-insert date column and gather 
data1_trafo_xoutl <- cbind(rownames(data1_trafo_xoutl_standardized), data.frame(data1_trafo_xoutl_standardized, row.names=NULL))
names(data1_trafo_xoutl)[1] <- "dates"
library(tidyr)
data_fredmd_trafo <- gather(data1_trafo_xoutl, key = var, value = trafo, -dates)

# do the same for the raw data
data1 <- cbind(rownames(data1), data.frame(data1, row.names=NULL))
names(data1)[1] <- "dates"
data_fredmd_raw <- gather(data1, key = var, value = raw, -dates) 

# merge both raw and transformed series into one dataset
data_fredmd <- merge(data_fredmd_raw, data_fredmd_trafo, by = c("dates", "var"))

# convert dates column to date format
data_fredmd$dates <- as.Date(data_fredmd$dates)

# plot both series
library(dplyr)
library(ggplot2)
p1 <- ggplot(filter(data_fredmd, var == "RPI"))+
  geom_line(aes(x = dates, y = raw, group = var))+
  labs(title = "raw",
       x ="", y = "level")
p2 <- ggplot(filter(data_fredmd, var == "RPI"))+
  geom_line(aes(x = dates, y = 100*trafo, group = var))+
  labs(title = "transformed",
       x ="", y = "%")

library(gridExtra)
grid.arrange(p1,p2,nrow = 2)

library(cowplot)
plot_grid(p1, p2, nrow = 2)

temp <- filter(data_fredmd, var == "RPI")
temp2 <- gather(temp, key = rawtrafo, value = value, -c(dates, var))

ggplot(temp2, aes(x = dates, y = value, group = var))+
  geom_line() + 
  facet_wrap(~rawtrafo, nrow = 2, scale = "free_y")+
  labs(title = "RPI",
       x ="", y = "")+
  theme_minimal()
