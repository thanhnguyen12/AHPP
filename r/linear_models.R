# Import required libraries
library(caret)

# Read training and testing data into data frames
train_data <- read.csv('data/train_no_na.csv')
test_data <- read.csv('data/test_no_na.csv')

## Data processing: produce dummy vars, remove zero-varance predictors
numeric_var <- names(train)[which(sapply(train, is.numeric))]

# for training data
dummy_vars <- dummyVars(saleprice ~., data = train_data[, -1])
tmp <- as.data.frame(predict(dummy_vars, newdata = train_data[, -1]))
train_data <- data.frame(saleprice = train_data[, "saleprice"], tmp)

# for test data
dummy_vars <- dummyVars(id ~., data = test_data)
test_data <- predict(dummy_vars, newdata = test_data)

## Fitting options
set.seed(1010)
train_ctrl <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 10)

near_zero_prds <- nearZeroVar(train_data)
pruned_train <- train_data[, -near_zero_prds]
pruned_train$saleprice <- log(pruned_train$saleprice)
dim(pruned_train)



# Various fitting options
pcr_fit <- train(saleprice ~., data = pruned_train,
                 method = 'pcr', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 tuneGrid = data.frame(.ncomp = seq(20, 50)),
                 trControl = train_ctrl)

pls_fit <- train(saleprice ~., data = pruned_train,
                 method = 'pls', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = train_ctrl)

spls_fit <- train(saleprice ~., data = pruned_train,
                 method = 'spls', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 tuneLength = 50,
                 trControl = train_ctrl)

train_num_var <- train[, numeric_var]
num_col_means <- colMeans(train_num_var, na.rm = TRUE)
train_num_var[sapply(train_num_var, is.na)] <- 0


numeric_var <- names(test)[which(sapply(test, is.numeric))]
test_num_var <- test[, numeric_var]
test_num_col_means <- colMeans(test_num_var, na.rm = TRUE)
test_num_var[sapply(test_num_var, is.na)] <- 0

pcr.tune.grid <- expand.grid(ncomp = seq(1, 50))
pcr_fit <- train(saleprice ~., data = pruned_train,
                 method = 'pcr', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 tuneGrid = pcr.tune.grid,
                 trControl = train_ctrl)


knn_fit <- train(saleprice ~., data = pruned_train,
                 method = 'knn', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 trControl = train_ctrl)

lasso_fit <- train(saleprice ~., data = pruned_train,
                 method = 'lasso', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 trControl = train_ctrl)

ridge_fit <- train(saleprice ~., data = pruned_train,
                   method = 'ridge', metric = 'RMSE',
                   preProcess = c("center", "scale"),
                   tuneLength = 10,
                   trControl = train_ctrl)

sparse.pcs.tune <- expand.grid(lambda = seq(0, 0.1, length.out = 2))

sparse_ridge_fit <- train(saleprice ~., data = new_train,
                   method = 'ridge', metric = 'RMSE',
                   preProcess = c("center", "scale"),
                   tuneGrid = sparse.pcs.tune,
                   trControl = train_ctrl)

elastic_fit <- train(saleprice ~., data = pruned_train,
                method = 'enet', metric = 'RMSE',
                preProcess = c("center", "scale"),
                trControl = train_ctrl)

svmKernel_fit <- train(saleprice ~., data = pruned_train,
                     method = 'svmLinear3', metric = 'RMSE',
                     preProcess = c("center", "scale"),
                     trControl = train_ctrl)

dnn_fit <- train(saleprice ~., data = pruned_train,
                 method = 'dnn', metric = 'RMSE',
                 preProcess = c("center", "scale"),
                 trControl = train_ctrl)

nn_fit <- train(saleprice ~., data = pruned_train,
                method = 'neuralnet', metric = 'RMSE',
                preProcess = c("center", "scale"),
                trControl = train_ctrl)

testfit <- predict(knn_fit, newdata = head(test_num_var))
testfit
