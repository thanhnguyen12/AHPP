# More data refinement after Ben's preprocessing by Ben
#
# @author: Thanh Nguyen
# Apr, 2017

# Package import
library(caret)

############################### NOTE #############################
# Some more work needs to be done in this script
# 1/ Ordinal MSSubclass makes no sense, replace with categorical
# 2/ The same thing happens with MoSold, YearSold
# 3/ Numeric missing values should be replaced with mean/median rather than 0 (e.g. lotfrontage, 
# masvnrarea, ...)
# 4/ Decide whether test and train data should be process together or separately (avoid cheating)
#     4.1. Replacement of missing values (on both)
#     4.2. Normalization (centering and scaling)
################################################################## 

# Read raw training and testing data into data frames
raw_train_data <- read.csv('data/train.csv', stringsAsFactors = F)
raw_test_data <- read.csv('data/test.csv', stringsAsFactors = F)

# Remove useless 'id' columns from training and test data
raw_train_data$id <- NULL
raw_test_data$id <- NULL

# Combine datasets
combined_data <- rbind(within(raw_train_data, rm("Id", "SalePrice")), within(raw_test_data, rm("Id")))


# List of removed features due to their uselessness
removed_features <- NULL

# For ease
attach(combined_data)

# MSSubClass is numerical but the values in nature do not give information on which is better
combined_data$MSSubClass <- as.factor(combined_data$MSSubClass)

# MSZoning has 4 NAs (all fall in test set), refer to Carbonati kernel for how to do it
# it based on grouping with MSSubClass
which(is.na(combined_data$MSZoning))
combined_data$MSZoning[c(2217, 2905)] <- "RL"
combined_data$MSZoning[c(1916, 2251)] <- "RM"

# LotFrontage
LotFrontage.NAs <- which(is.na(combined_data$LotFrontage))
mean.nonNA.lotft <- mean(combined_data$LotFrontage[-LotFrontage.NAs])
median.nonNA.lotft <- median(combined_data$LotFrontage[-LotFrontage.NAs])
combined_data$LotFrontage[LotFrontage.NAs] <- mean.nonNA.lotft

# Street: type of road access to property. There may be outlier in this column

# Alley: alley access: 2721 NAs, mean no alley access
combined_data$Alley[is.na(combined_data$Alley)] <- 'None'

# LotShape (no NA): 4 possible values: Reg and 3 irregular. Replace with (Reg and IReg)
# (still keep it categorical)
irreg_lotshape <- which(LotShape != 'Reg')
combined_data$LotShape[irreg_lotshape] <- 'IReg'


# LandContour: Flatness of property. 4 levels but skewed about Lvl, so do same thing as LotShape
non_flat_contour <- which(LandContour != 'Lvl')
combined_data$LotShape[non_flat_contour] <- 'Slope'

# Utitilies: Type of utilities with 2 NAs. But all of them are AllPub except one, so remove it
removed_features <- c(removed_features, 'Utilities')


# LotConfig: fine

# LandSlope: reduce to 2 levels
slope_idx <- which(LandSlope != 'Gtl')
combined_data$LandSlope[slope_idx] <- "Sev"

# Neighborhood: fine

# Condition 1 & 2 have near-zero variance, remove it later

# BldgType, HouseStyle: Ok

# OverallQual and OverallCond

# YearBuilt: what about changing this to some categorical (may be not because standardization is good)

# YearRemodAdd: only some of the house are remodelled (YearBuilt#YearModel), change this to binary variable
remodelled <- which(YearBuilt < YearRemodAdd)
combined_data$YearRemodAdd[remodelled] <- 1
combined_data$YearRemodAdd[-remodelled] <- 0

# RoofStyle and RoofMat are fine

# Exterior1st and Exterior2nd have NAs that just mean having other coverage
combined_data$Exterior1st[is.na(Exterior1st)] <- "Other"
combined_data$Exterior2nd[is.na(Exterior2nd)] <- "Other"

# MasVnrType and MasVnrArea: with NAs coming from same houses (refer to Kaggle kernel)
combined_data$MasVnrType[2261] <- 'BrkFace'
combined_data$MasVnrType[is.na(MasVnrType)] <- 'None'
combined_data$MasVnrArea[is.na(MasVnrArea)] <- 0


# ExterQual and ExterCond: from excellent to poor. Should be converted to numeric
# None, Poor to Excellent  => 0, 1 -5

###### These following are useless due to code right after that

# No houses with Poor quality, because it is out of order, need to specify levels in correct order
# exterqual_lv <- c("Fa", "TA", "Gd", "Ex")
# combined_data$ExterQual <- as.numeric(factor(ExterQual, levels = exterqual_lv))

# Similarly for ExterCond. Check availability of all levels
# extercond_lv <- c("Po", "Fa", "TA", "Gd", "Ex")
# combined_data$ExterCond <- as.numeric(factor(ExterCond, levels = extercond_lv))

# Foundation is fine

# BsmtQual: having NA. First replace NA with None
combined_data$BsmtQual[is.na(BsmtQual)] <- "None"
combined_data$BsmtCond[is.na(BsmtCond)] <- "None"

# The same thing for GarageQual and GarageCond, FireplaceQua, KitchenQual, HeatingQC
combined_data$GarageQual[is.na(GarageQual)] <- "None"
combined_data$GarageCond[is.na(GarageCond)] <- "None"
combined_data$FireplaceQu[is.na(FireplaceQu)] <- "None"
combined_data$KitchenQual[is.na(KitchenQual)] <- "None"
combined_data$HeatingQC[is.na(HeatingQC)] <- "None"


qual_features <- c('ExterQual', 'ExterCond', 'BsmtQual', 
                   'BsmtCond', 'GarageQual', 
                               'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC')
map_list = c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

conv_category_numeric <- function(columns, map_list, dframe) {
  for(col in columns) {
    dframe[col] <- as.numeric(map_list[dframe[, col]])
  }
  return(dframe)
}

combined_data <- conv_category_numeric(qual_features, map_list, combined_data)

# BsmtExposure
which(is.na(BsmtExposure))
combined_data$BsmtExposure[c(949, 1488, 2349)] <- 'No'

# There 11 basement featuress with similar property, do same process (except BsmtQual 
# and BsmtCond already processed)
bsmt_features <- names(combined_data)[sapply(names(combined_data), FUN = function(x) str_detect(x, "Bsmt"))]
bsmt_features <- bsmt_features[-c(1, 2)] # Make sure their indices be 1 and 2

# If any of numeric Bsmt features is NA, replace with 0. Otherwise, replace with 'None'
for(fea in bsmt_features) {
  if(is.numeric(combined_data[fea])) {
    combined_data[sapply(combined_data[fea], is.na), fea] <- 0
  }
  else {
    combined_data[sapply(combined_data[fea], is.na), fea] <- 'None'
  }
}

# Convert BsmtExposure factor to level
bsmt_list = c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
combined_data <- conv_category_numeric(c('BsmtExposure'), bsmt_list, combined_data)

# BsmtFinType 1 and 2: the factors are ordered, convert them with correct levels
bsmt_fin_list <- c('None' = 1, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
combined_data <- conv_category_numeric(c('BsmtFinType1','BsmtFinType2'), bsmt_fin_list, combined_data)

# TotalBsmtSF already done in the above list

# Heating: type of heating: no NA so fine

# HeatingQC is already done with convert factor to levels

# CentralAir: Y/N. Convert to 0/1 because Y implies an advance over N. Or just leave it alone

# Electrical: same processing as LandContour, LandSlope above
combined_data$Electrical[is.na(combined_data$Electrical)] <- 'TA'
elec_sd_idx <- which(combined_data$Electrical != 'SBrkr')
combined_data$Electrical[elec_sd_idx] <- "Other"

# Columns from 1stFlrSF to GrLiv are fine (numeric, no NAs)

# BsmtFullBath and BsmtHalf were processed already. FullBath and HalfBath are ok

# The same for BedroomAbvGr, KitchenAbv

# KitchenQual already done

# TotRmsAbvGrd is fine

# Functional: 2NAs
combined_data$Functional[is.na(Functional)] <- "Typ"

# Fileplaces is fine. FireplaceQual is done previously

# GarageType and Garage-related columns
garage_features <- c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')

# Follow Carbonati kernel
combined_data[2127,'GarageQual'] <- 'TA'
combined_data[2127, 'GarageFinish'] <- 'Unf'
combined_data[2127, 'GarageCond'] <- 'TA'

# Replace NAs
for(fea in garage_features) {
  if(is.numeric(combined_data[fea])) {
    combined_data[sapply(combined_data[fea], is.na), fea] <- 0
  }
  else {
    combined_data[sapply(combined_data[fea], is.na), fea] <- 'None'
  }
}
garage_fin_list <- c('None' = 1,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)
combined_data <- conv_category_numeric(c('GarageFinish'), garage_fin_list, combined_data)

# GarageYrBlt: 141 NAs. 90% of data have same year built for garage and house
garage_year_blt_NAs <- which(is.na(GarageYrBlt))
combined_data$GarageYrBlt[garage_year_blt_NAs] <- YearBuilt[garage_year_blt_NAs]

# WoodDeckSF, OpenPorchSF. EnclosedPorch, Screen Porch are fine

# PoolArea and PoolQC
combined_data[2421,'PoolQC'] <- 'Ex'
combined_data[2504,'PoolQC'] <- 'Ex'
combined_data[2600,'PoolQC'] <- 'Fa'
combined_data$PoolQC[is.na(combined_data$PoolQC)] <- 'None'

# Fence quality
combined_data$Fence[is.na(Fence)] <- 'None'
fence_list = c('None' = 1, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)
combined_data <- conv_category_numeric(c('Fence'), fence_list, combined_data)


# MiscFeature
combined_data$MiscFeature[is.na(MiscFeature)] <- 'None'

# MiscVal is fine

# Change MoSold and YrSold to factors
combined_data$MoSold <- as.factor(MoSold)
combined_data$YrSold <- as.factor(YrSold)

# SaleType 1 NAs
combined_data$SaleType[is.na(SaleType)] <- 'WD'

# SaleCondition is fine

detach(combined_data)

# Everything done, now remove useles columns and check if any NAs remains
combined_data <- combined_data[, !names(combined_data) %in% removed_features]

NA_cols <- which(colSums(is.na(combined_data)) > 0)
length(NA_cols)
# If still have NAs, show which columns
sort(colSums(sapply(combined_data[NA_cols], is.na)), decreasing = TRUE)

dim(combined_data)

# Remove near-zero variance columns
near_zero_features <- nearZeroVar(combined_data)
cleaned_data <- combined_data[, -near_zero_features]
dim(cleaned_data)

# Write refined data to external directory
cleaned_train <- cleaned_data[1:1460, ]
cleaned_train$SalePrice <- raw_train_data$SalePrice

# remove outliers
outliers <- which(cleaned_train$GrLivArea > 4000)
outliers

# 4 outliers are removed from training set
cleaned_train <- cleaned_train[!1:nrow(cleaned_train) %in% outliers, ]
write.csv(x = cleaned_train, file = "data/cleaned_train.csv", row.names = F)

# 
cleaned_test <- cleaned_data[1461:nrow(cleaned_data), ]
write.csv(x = cleaned_test, file = "data/cleaned_test.csv", row.names = F)



############################# Prepare data for training deep net with Tensorflow #################
n_train <- nrow(raw_train_data)

# Create a dummy saleprice to create dummy variable for the whole data
sale_price <- rep(0, nrow(cleaned_data))
sale_price[1:n_train] <- raw_train_data$SalePrice

# 
dummy_data <- data.frame(cleaned_data, SalePrice = sale_price)

# Create dummy variables
dummy_vars <- dummyVars(SalePrice ~., data = dummy_data)

dummy_data_with_dum_vars <- as.data.frame(predict(dummy_vars, newdata = dummy_data))
dim(dummy_data_with_dum_vars)

# Again, remove any variable with near zero variance
near_zero_features <- nearZeroVar(dummy_data_with_dum_vars)
cleaned_dummy_data <- dummy_data_with_dum_vars[, -near_zero_features]
dim(cleaned_dummy_data)


cleaned_dummy_train <- cleaned_dummy_data[1:n_train, ]

# Centering and scaling
normal_model <- preProcess(cleaned_dummy_train, method = c("center", "scale"))

norm_train <- predict(normal_model, cleaned_dummy_train)

# 
cleaned_dummy_test <- cleaned_dummy_data[1461 :nrow(cleaned_dummy_data), ]

# Center and scaling test set with same information from training set
norm_test <- predict(normal_model, cleaned_dummy_test)

# Add output column
norm_train$SalePrice <- log(raw_train_data$SalePrice+1)

# Hold-out partition for training deep net
hold_in <- createDataPartition(y = norm_train$SalePrice, p = 0.9, list = FALSE)

ames_holdout <- norm_train[-hold_in, ]
ames_train <- norm_train[hold_in, ]

write.csv(x = ames_train, file = "../../tensorflow/study/data/ames_train.csv", row.names = F)
write.csv(x = ames_holdout, file = "../../tensorflow/study/data/ames_test.csv", row.names = F)
write.csv(x = norm_test, file = "../../tensorflow/study/data/ames_prediction.csv", row.names = F)

# Release memory