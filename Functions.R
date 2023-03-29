# functions file - homework #9 (3/29/23)

##############################
# FUNCTION: sort_data
# packages: dplyr
# purpose: find mean and sd of treatment variables from initial (real) dataset
# input: initial (real) dataset
# output: mean and sd for both treatment variables
#-------------------- 
sort_data <- function(file_name) {
  df <- read.csv(file_name)
  ControlData <- filter(df, df$Treatment=="Control") 
  TreatmentData <- filter(df, df$Treatment=="Heat shock") 
  .GlobalEnv$df <- df
  .GlobalEnv$ControlData <- ControlData
  .GlobalEnv$TreatmentData <- TreatmentData
  
}
##############################################################

##############################
# FUNCTION: find_stats
# packages: MASS
# purpose: find stats of treatments
# input: vector of values
# output: mean, sd, fit distribution, n-value
#-------------------- 
find_stats <- function(control_vector=0, treatment_vector=0) {
  ControlMean <- mean(control_vector)
  ControlSd <- sd(control_vector)
  ControlSize <- nrow(ControlData)
  .GlobalEnv$ControlMean <- ControlMean
  .GlobalEnv$ControlSd <- ControlSd
  .GlobalEnv$ControlSize <- ControlSize

  TreatmentMean <- mean(treatment_vector)
  TreatmentSd <- sd(treatment_vector)
  TreatmentSize <- nrow(TreatmentData)
  .GlobalEnv$TreatmentMean <- TreatmentMean
  .GlobalEnv$TreatmentSd <- TreatmentSd
  .GlobalEnv$TreatmentSize <- TreatmentSize
}
######################################################

##############################
# FUNCTION: run_anova
# packages: none
# purpose: run an ANOVA on relationship between two variables
# input: y and x variable names
# output: summary of ANOVA
#-------------------- 
run_anova <- function(x_var, y_var) {
  ANOVA <- aov(y_var ~ x_var)
  ANOVA_sum <- summary(ANOVA)
  .GlobalEnv$ANOVA_sum <- ANOVA_sum
}
###############################################################

##############################
# FUNCTION: new_data
# packages: MASS
# purpose: generate a fake dataset based on parameters of an old dataset
# input: parameters (n, mean, sd)
# output: vector of generated values
#-------------------- 
new_data <- function(data) {
  NewControl <- rnorm(n = ControlSize, mean =  ControlMean,
                     sd = ControlSd)
  NewControl <- matrix(NewControl)
  Control <- rep("Control", 32)
  NewControl <- cbind(NewControl, Control)
  
  NewTreatment <- rnorm(n = TreatmentSize, mean = TreatmentMean,
                        sd = TreatmentSd)
  NewTreatment <- matrix(NewTreatment)
  Treatment <- rep("Heat shock", 17)
  NewTreatment <- cbind(NewTreatment, Treatment)
  
  NewData <- rbind(NewControl, NewTreatment)
  NewData <- data.frame(NewData)
  NewData$V1 <- as.numeric(NewData$V1)
  names(NewData) <- list("Intensity", "Treatment")
  .GlobalEnv$NewData <- NewData
}
###############################################################

##############################
# FUNCTION: plot_hist
# packages: ggplot2
# purpose: plot a histogram of new data
# input: dataset, x and y variables
# output: histogram
#-------------------- 
plot_hist <- function(data, x_var, y_var) {
  Histogram <- ggplot(data=NewData, aes(x=Intensity, 
                                        y=after_stat(density))) +
    geom_histogram(color="black", fill="lightblue", 
                   linewidth=0.2)
  return(Histogram)
  print(Histogram)
}
#############################################################
##############################
# FUNCTION: plot_box
# packages: ggplot2
# purpose: create boxplot of new data
# input: dataset, x and y variables
# output: boxplot
#-------------------- 
plot_box <- function(data, x_var, y_var) {
  Boxplot <- ggplot(data=data, aes(x=x_var, y=y_var)) +
    geom_boxplot(fill="lightblue") +
    labs(x="Treatment", y="Fluorescence Intensity")
  return(Boxplot)
  print(Boxplot)
}
##############################################################

##############################
# FUNCTION: sort_data
# packages: dplyr
# purpose: find mean and sd of treatment variables from initial (real) dataset
# input: initial (real) dataset
# output: mean and sd for both treatment variables
#-------------------- 
sort_data2 <- function(file_name) {
  df <- read.csv(file_name)
  Data <- dplyr::select(df, c(Intensity, LT50)) 
  .GlobalEnv$df <- df
  .GlobalEnv$Data <- Data
}
##############################################################

##############################
# FUNCTION: linmod
# packages: 
# purpose: run a linear regression
# input: x and y vars
# output: regression summary
#-------------------- 
linmod <- function(x_var, y_var) {
  lm <- lm(y_var ~ x_var)
  LMsummary <- summary(lm)
  .GlobalEnv$LMsummary <- LMsummary
  
  plot(lm)
}
