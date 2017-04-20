
#
# Data cleaning from Tanner Carbonati Data Cleaning
#

library(tidyverse)
require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot)

train = read.csv('./data/train.csv', stringsAsFactors = FALSE)
test = read.csv('./data/test.csv', stringsAsFactors = FALSE)

df.combined = rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(df.combined)


# -------------- fix pool var ------------
# fill in missing area with mean of corresponding group quality

df.combined[,c('PoolQC','PoolArea')] %>%
  group_by(PoolQC) %>%
  summarise(mean = mean(PoolArea), counts = n()
            
            
df.combined[2421,'PoolQC'] = 'Ex'
df.combined[2504,'PoolQC'] = 'Ex'
df.combined[2600,'PoolQC'] = 'Fa'
df.combined$PoolQC[is.na(df.combined$PoolQC)] = 'None'



# ----------------------- garage variables -------------------


# replace garageyrbuilt with date house was built, if they have  a garage

idx = which(is.na(df.combined$GarageYrBlt))
df.combined[idx, 'GarageYrBlt'] = df.combined[idx, 'YearBuilt']
            

garage.cols = c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')
            

# one house has missing entries for some garage variables, so replace with
# most frequent values for garages with similar area

df.combined[2127,'GarageQual'] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'
df.combined[2127, 'GarageCond'] = 'TA'


# only 1 missing value for kitchen and electrical = replaced with mode


df.combined$KitchenQual[is.na(df.combined$KitchenQual)] = 'TA'

df.combined$Electrical[is.na(df.combined$Electrical)] = 'SBrkr'



# ---------------------- basement variables --------------------------------

# fill in legit missing values with mode 'no'
df.combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No'



# for the rest no basement fill in with 0 if numeric or 'none' if categorical

bsmt.cols = names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]

for (col in bsmt.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na),col] = 0
  }
  else{
    df.combined[sapply(df.combined[col],is.na),col] = 'None'
  }
}


# only one house with both exterior features missing, so call it 'other'

df.combined$Exterior1st[is.na(df.combined$Exterior1st)] = 'Other'
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] = 'Other'


# our missing value for saletype is for a house with sale condition 'normal'
# so of the houses with sale condition normal, 'wd' is the mode sale type

df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'


# replace missing functional with mode

df.combined$Functional[is.na(df.combined$Functional)] = 'Typ'
        

# utilities is all one value except for one in our training set, so we can drop it


col.drops = c('Utilities')
df.combined = df.combined[,!names(df.combined) %in% c('Utilities')]

# for MSzoning missing values, we replace with the mode for the subclasses the houses
# belong to


df.combined$MSZoning[c(2217, 2905)] = 'RL'
df.combined$MSZoning[c(1916, 2251)] = 'RM'


# for masonry variables all but one house likely don't have any type of masonry

df.combined[2611, 'MasVnrType'] = 'BrkFace'

df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] = 0



#--------------- neighborhood ----------------------------



df.combined['Nbrh.factor'] = factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))

lot.by.nbrh = df.combined[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh


# replace missing lotfrontage with median for that neighborhood



idx = which(is.na(df.combined$LotFrontage))

for (i in idx){
  lot.median = lot.by.nbrh[lot.by.nbrh == df.combined$Neighborhood[i],'median']
  df.combined[i,'LotFrontage'] = lot.median[[1]]
}


# replace na for fence and misc feature with none

df.combined$Fence[is.na(df.combined$Fence)] = 'None'
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] = 'None'


# ---------------------- fireplace ------------------------------

# all houses with missing values for fireplace quality don't have a fireplace

df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'


# we also can assume no alley missing alley values

df.combined$Alley[is.na(df.combined$Alley)] = 'None'