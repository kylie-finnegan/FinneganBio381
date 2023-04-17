# simple function to run regression model and get stats

##################################################
# function: reg_stats
# purpose: fits linear model, extracts statistics
# input: 2-column data frame (x and y)
# output: slope, p-value, and r2
#------------------------------------------------- 
reg_stats <- function(d=NULL) {
  if(is.null(d)) {
    x_var <- runif(10)
    y_var <- runif(10)
    d <- data.frame(x_var,y_var)
  }
  . <- lm(data=d,d[,2]~d[,1])
  . <- summary(.)
  stats_list <- list(slope=.$coefficients[2,1],
                     p_val=.$coefficients[2,4],
                     r2=.$r.squared)
  return(stats_list)
  
}
