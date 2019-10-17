########################################
## R implementation of FRED-MD codes
########################################

# clear workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Philipp/Documents/GitHub/fredmdr")

# load in data
data <- read.csv("2019-09.csv")

# extract trafos
trafos <- data[1, ]
data <- data[2 : dim(data)[1], ]

# convert first column to date format
data1 <- data
rownames(data1) <- seq(as.Date("1959/01/01"), by = "months", length.out = dim(data1)[1])
data1 <- data1[, -1]
trafos <- trafos[, -1]

# # convert entire matrix to ts format, removing first colum
# data2 <- ts(data, c(1959,1), frequency = 12)
# data2 <- data2[, -1]

# # gather data1
# library(tidyr)
# data1_gathered <- gather(data1, mnemonic, value)

# transform
Nt <- dim(data)[1]
Nn <- dim(data1)[2]
data1_trafo <- matrix(NA_real_, Nt, Nn)
for (i in 1:Nn) {
  if (trafos[i] == 1) # no transformation
    data1_trafo[, i] <- data1[, i]
  else if (trafos[i] == 2) # first difference
    data1_trafo[ 2 : Nt, i] <- diff(data1[, i])
  else if (trafos[i] == 3) # second difference
    data1_trafo[ 3 : Nt, i] <- diff(data1[, i], differences = 2)
  else if (trafos[i] == 4) # logs
    data1_trafo[, i] <- log(data[, i])
  else if (trafos[i] == 5) # log first differences
    data1_trafo[ 2 : Nt, i] <- diff(log(data1[, i]))
  else if (trafos[i] == 6) # log second differenes
    data1_trafo[ 3 : Nt, i] <- diff(log(data[, i]), differences = 2)
  else if (trafos[i] == 7) # difference of percentage change
    data1_trafo[ 3 : Nt, i] <- diff(data1[2 : Nt, i] / data1[1 : (Nt-1), i] - 1)
}

# convert to data frame and remove first two rows
data1_trafo <- as.data.frame(data1_trafo)
rownames(data1_trafo) <- rownames(data1)
names(data1_trafo) <- names(data1)
data1_trafo <- data1_trafo[3 : Nt, ]

# remove outliers
data1_trafo_xoutl <- data1_trafo
median_data <- matrix(apply(data1_trafo, 2, median, na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
iqr_data <- matrix(apply(data1_trafo, 2, quantile, probs = c(0.75), na.rm = TRUE) - apply(data1_trafo, 2, quantile, probs = c(0.25), na.rm = TRUE), nrow = Nt, ncol = Nn, byrow = TRUE)
z <- abs(data1_trafo-median_data);
outlier <- z > (10 * iqr_data)
data1_trafo_xoutl[outlier == TRUE] <- NA
n_outl <- apply(outlier, 2, sum, na.rm = TRUE)

# convert transformed and outlier-free data set to ts format
data1_trafo_xoutl_ts <- ts(data1_trafo_xoutl, c(1959,1), frequency = 12)

# plot series to compare with MATLAB
ind_var <- 10
plot(data1_trafo_xoutl_ts[, ind_var], type = "l", main = names(data1_trafo_xoutl)[ind_var], ylab = "", xlab = "")
