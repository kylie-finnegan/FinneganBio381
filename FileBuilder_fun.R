##################################################
# function: file_builder
# purpose: create a set of random files for regression
# input: file_n = number of files to create
#        file_folder = name of folder for random files
#        file_size = c(min,max) number of rows in file
#        file_na = number on average of NA values per column
# output: set of random files
#------------------------------------------------- 
file_builder <- function(file_n=10,
                         file_folder="RandomFiles/",
                         file_size=c(15,100),
                         file_na=3){
  # check for folder - if present, destroy it
  if(dir.exists(file_folder)) 
    unlink(file_folder,
    recursive = TRUE)
  # then create fresh copy of folder
  dir.create(file_folder)
  
  for (i in seq_len(file_n)) {
    # get number of rows
    file_length <- sample(file_size[1]:file_size[2],size=1) 
    # create random x
    var_x <- runif(file_length)
    # create random y
    var_y <- runif(file_length)
    # bind into a data frame
    df <- data.frame(var_x,var_y) 
    # determine NA number
    bad_vals <- rpois(n=1,lambda=file_na) 
    # random NA in var_x
    df[sample(nrow(df),size=bad_vals),1] <- NA 
    # random NA in var_y
    df[sample(nrow(df),size=bad_vals),2] <- NA 
    
    # create label for file name with padded zeroes
    file_label <- paste(file_folder,
                        "ranFile",
                        formatC(i,
                                width=3,
                                format="d",
                                flag="0"),
                        ".csv",sep="")
    
    # set up data file and incorporate time stamp and minimal metadata
    write.table(cat("# Simulated random data file for batch processing","\n",
                    "# timestamp: ",as.character(Sys.time()),"\n",
                    "# KMF","\n",
                    "# ------------------------", "\n",
                    "\n",
                    file=file_label,
                    row.names="",
                    col.names="",
                    sep=""))
    
    # now add the data frame
    write.table(x=df,
                file=file_label,
                sep=",",
                row.names=FALSE,
                append=TRUE) # default = FALSE: will write over metadata
    
    
  }
}
#######################################################
