# clear workspace except for functions
rm(list=setdiff(ls(), c("f_removeoutl", "f_transform"))

# f_load_fredmd <- function(vintage, dirname = NULL, alternative_trafos = NULL,
#                           sample_start = "1959-01-01", sample_end = NULL,
#                           keep_vars = NULL, remove_vars = NULL,
#                           drop_firstobs = TRUE)
   #========================================================================#
   #= function to read in FRED-MD data
   #========================================================================#

vintage <- "2019-09"
dirname <- NULL
alternative_trafos <- NULL
sample_start <- "1959-01-01"
sample_end <- NULL
keep_vars <- NULL
remove_vars <- NULL
drop_firstobs <- TRUE




   