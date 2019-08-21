###############################################################
# Title: Data Processing Functions
# Date Start: 08/08/2019
# Author: Ketul Patel
# Description: Resuable function for data cleaning and 
#              preprocessing
# Version: 0.1 
###############################################################
###############################################################
# Required Libraries
###############################################################

###############################################################
# 0. Mathemitcal Calculation
###############################################################
#--------------------------------------------------------------
# 0.1 Column to the Power
#--------------------------------------------------------------
doPower <- function(df, cols, power = 2){
  
 new_col <- paste0(cols, "_", power)
 df[, new_col] <- df[, cols]^power
 return(df) 
}

#--------------------------------------------------------------
# 0.2 Natural Log of a col
#--------------------------------------------------------------

doNaturalLog <- function(df, cols){
  
  new_col <- paste0(cols, "_", "ln")
  df[, new_col] <- log(df[, cols]+1) # +1 to avoid 0 in source data
  return(df)
}



###############################################################
# 1. Missing Data
###############################################################
#--------------------------------------------------------------
# 1.1 Find NA and NaN and return with %
#--------------------------------------------------------------
getNAPercentage <- function(df=df){
  # Function for detecting NA observations: 
  na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}
  sapply(df, na_rate) %>% round(2) -> df_NA
  
  return(df_NA)
}

#--------------------------------------------------------------
# 1.2 Either Keep or Drop Columns from Dataframe
#--------------------------------------------------------------
doKeepOrDropCols = function(data=df,cols, drop=TRUE) {
  
  # Double Quote Output Dataset Name
  t = deparse(substitute(newdata))
  
  # Drop Columns
  if(drop == 1){
    newdata = data [ , !(names(data) %in% scan(textConnection(cols), what="", sep=" "))]
    }
  
  # Keep Columns
  else {
    newdata = data [ , names(data) %in% scan(textConnection(cols), what="", sep=" ")]
    }
  # assign(t, newdata, .GlobalEnv)
  
  return(newdata)
}


#--------------------------------------------------------------
# 1.3 Remove Columns with more than certain % NA
#--------------------------------------------------------------
removeColsWithPercNA <- function(df, perc){
  
  # Find Percentage of NA for each Predictors
  df_NA <- getNAPercentage(df)
  remove_cols <- names(df_NA[df_NA > perc])
  
  # Drop the columns
  newdf <- doKeepOrDropCols(df, remove_cols, drop = TRUE)
  
  # Return new dataframe
  return(newdf)
}

#--------------------------------------------------------------
# 1.4 Replace NA with 0 for numeric
#--------------------------------------------------------------
replaceNAWithZero <- function(df, cols){
  
  df[cols][is.na(df[cols])] <- 0
  
  # Return new dataframe
  return(df)
}



###############################################################
# 2. Correlation and Significance test
###############################################################
#--------------------------------------------------------------
# 2.1 Remove highly correlated predictors
#--------------------------------------------------------------
# Future work: Check for Cols with NaN

removeHighlyCorrCols <- function(df, cols, cutoff_perc= 0.9, plot = TRUE){
  
  # Find highly correlated
  df_corr <- df[, cols]
  
  # Check if any non numeric predictor assigned
  if(any(sapply(df_corr, class) != "numeric")){
    stop("Non Numeric predictor(s) selected")
  } 
  
  # Check if any predictor has NA and remove those observation
  else if (any(is.na(df_corr))) {
    df_corr <- na.omit(df_corr)
  }
  
    correlations <- cor(df_corr)
    pred_index <- findCorrelation(correlations, cutoff = cutoff_perc)
    corr_cols <- colnames(correlations)[pred_index]
    
    if(plot) {
      require(corrplot)
      corrplot::corrplot(correlations, method = "circle")
    }
    

  # Find among correlated which one is more significant
  
  # Remove highly correlated and non/less significant predictors
  newdf <- doKeepOrDropCols(df, corr_cols, drop = TRUE)

  # Return new dataframe
  return(newdf)
}

###############################################################
# 3. Model Planning
###############################################################

#--------------------------------------------------------------
# 1.4 Split Training and Test set
#--------------------------------------------------------------
splitTrain <- function(df, split_perc = 0.8){
  require(dtplyr)
  # Split data
  df_train <- df %>% 
    dtplyr::group_by(Defaulter) %>% 
    sample_frac(split_perc) %>% 
    ungroup()
  
  return(df_train)
  
}





