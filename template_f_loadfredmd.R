# clear workspace except for functions
rm(list=setdiff(ls(), c("f_removeoutl", "f_transform")))

# f_load_fredmd <- function(vintage, dirname = NULL, alternative_trafos = NULL,
#                           sample_start = "1959-01-01", sample_end = NULL,
#                           keep_vars = NULL, remove_vars = NULL,
#                           drop_firstobs = TRUE)
   #========================================================================#
   #= function to read in FRED-MD data
   #========================================================================#

#========================================================================#
#= input arguments
#========================================================================#

# vintage <- "2019-09"
# dirname <- NULL
# alternative_trafos <- NULL
# alternative_trafos <- c(5,5)
# names(alternative_trafos) <- c("HOUST", "PERMIT")
# sample_start <- "1959-01-01"
# sample_end <-  NULL
# keep_vars <- NULL
# #keep_vars <- c("INDPRO", "PAYEMS", "CPIAUCSL")
# remove_vars <- NULL
# #remove_vars <- c("AAA", "BAA")
# drop_firstobs <- TRUE

f_loadfredmd <- function(vintage, dirname = NULL, alternative_trafos = NULL, 
                         sample_start = "1959-01-01", sample_end = NULL,
                         keep_vars = NULL, remove_vars = NULL, drop_firstobs = TRUE){

                #========================================================================#
                #= read in data
                #========================================================================#
                
                # csv.read
                temp <- read.csv(paste0(dirname, vintage, ".csv"))
                
                # extract trafos
                trafos <- temp[1, 2 : ncol(temp)]
                xmat <- temp[2 : nrow(temp), 2 : ncol(temp)]
                
                # convert first column to date format and move to rownames
                rownames(xmat) <- seq(as.Date("1959-01-01"), by = "months", length.out = dim(xmat)[1])
                
                # check that there are no complete rows of NA (this can happen at the end of the sample when reading in the csv)
                xmat <- xmat[apply(is.na(xmat), 1, sum) != ncol(xmat), ]
                
                # remove temp
                rm(temp)
                
                # subsample selection
                if (!is.null(sample_end)){
                    xmat <- xmat[rownames(xmat) >= sample_start & rownames(xmat) <= sample_end, ]
                } else {
                    xmat <- xmat[rownames(xmat) >= sample_start, ]
                }
                
                #========================================================================#
                #= transform data
                #========================================================================#
                
                # check for alternative trafos
                for (i in 1 : length(alternative_trafos)) {
                    trafos[grep(paste0("^",names(alternative_trafos)[i],"$"), names(trafos))] <- alternative_trafos[i]
                }
                
                
                # call f_transform
                xmat <- f_transform(xmat, trafos)
                
                
                # drop first observations? 
                if (drop_firstobs == TRUE){
                    xmat <- xmat[3 : nrow(xmat), ]  
                }
                
                #========================================================================#
                #= select variables
                #========================================================================#
                
                if (!is.null(keep_vars) & !is.null(remove_vars)) {
                    print("One of keep_vars or remove_vars needs to be NULL!")
                    return(0)
                } else if (!is.null(keep_vars)) {
                    xmat <- dplyr::select(xmat, keep_vars)
                } else if (!is.null(remove_vars)) {
                    xmat <- dplyr::select(xmat, -remove_vars)
                }
                
                #========================================================================#
                #= remove outliers
                #========================================================================#
                
                temp <- f_removeoutl(xmat)
                
                xmat <- temp[["xoutl"]]
                noutl <- temp[["noutl"]]
                rm(temp)
                
                #========================================================================#
                #= tidy up dataset and return
                #========================================================================#
                
                xmat <- cbind(as.Date(rownames(xmat)), data.frame(xmat, row.names=NULL))
                names(xmat)[1] <- "dates"
                #fredmd <- tidyr::gather(xmat, key = var, value = value, -dates)
                fredmd <- tidyr::pivot_longer(xmat, -dates, names_to = "var")
                return(fredmd)
}

#========================================================================#
#= test function
#========================================================================#

test_fredmd <- f_loadfredmd("2019-09")
test2_fredmd <- f_loadfredmd("2019-09", keep_vars = c("INDPRO", "PAYEMS", "CPIAUCSL"))
test3_fredmd <- f_loadfredmd("2019-09", 
                             keep_vars = c("INDPRO", "PAYEMS", "CPIAUCSL"),
                             drop_firstobs = FALSE)
test4_fredmd <- f_loadfredmd("2019-09", 
                             keep_vars = c("INDPRO", "PAYEMS", "CPIAUCSL"),
                             drop_firstobs = FALSE,
                             sample_start = "1985-01-01",
                             sample_end = "2010-12-01")

#========================================================================#
#= plots
#========================================================================#

library(ggplot2)
ggplot(test4_fredmd, aes(x = dates, y = value, group = var, col = var))+
    geom_line()+
    theme(legend.position="bottom")
     