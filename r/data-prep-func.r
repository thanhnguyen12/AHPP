library(tidyverse)
library(caret)
setwd("C:/Users/Ben/Desktop/Kaggle/scripts")

# make sure stringsAsFactors = F

ames <- read.csv("../data/train.csv", stringsAsFactors = F)


cleanR <- function(test){
  
  test %>% as_data_frame()
  names(test) <- tolower(names(test))
  
  if ("saleprice" %in% names(test)){
    test$saleprice <- log(test$saleprice)
  }
  
  repl.ls <- list(bsmtqual = "nobsmt", bsmtcond = "nobsmt", bsmtexposure = "nobsmt",
                  bsmtfintype1 = "nobsmt", bsmtfintype2 = "nobsmt", 
                  fireplacequ = "nofire", alley = "noalley", saletype = "WD",
                  garagetype = "nogar", garagefinish = "nogar", bsmtunfsf = 0,
                  garagequal = "nogar", garagecond = "nogar", garagecars = 0,
                  garagearea = 0, poolqc = "nopool", fence = "nofence",
                  miscfeature = "none", mszoning = "RL", utilities = "AllPub",
                  bsmtfullbath = 0, bsmthalfbath = 0, functional = "Typ",
                  exterior1st = "VinylSd", exterior2nd = "VinylSd", bsmtfinsf1 = 0,
                  bsmtfinsf2 = 0, totalbsmtsf = 0, kitchenqual = "TA")
  
  test[ ,names(repl.ls)] <- test[ ,names(repl.ls)] %>% replace_na(repl.ls)
  
  # need to change this so known values are replaced via the above function
  # and unknown values are manually replaced below for easier editing
  
  test$lotfrontage[is.na(test$lotfrontage)] <- 0
  test$masvnrtype[is.na(test$masvnrtype)] <- "None"
  test$masvnrarea[is.na(test$masvnrarea)] <- 0
  
  brkyr <- c(1890, 1961, 1980, 2002, 2300)
  yrlabel <- c("pre1961","pre1980","pre2002","pre2010")
  
  test$garageyrblt <- as.character(cut(test$garageyrblt, breaks = brkyr, label = yrlabel))
  test$garageyrblt[is.na(test$garageyrblt)] <- "nogar"

  test$electrical[is.na(test$electrical)] <- "SBrkr"
 
   return(test)
}


