library(caret)
library(xgboost)


x_train <- read.csv("x_train.csv")
x_test <- read.csv("x_test.csv")
y_train <- read.csv("y_train.csv")

dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))

xgb.grid = expand.grid(nrounds = 10000,
                       eta = 0.005,
                       max_depth = 6,
                       colsample_bytree = 1,
                       min_child_weight = 2,
                       subsample = 0.2,
                       gamma=0.01)


pls.grid <- data.frame(ncomp=1:50)


model_list <- caretList(
  x_train, y_train,
  #trControl=my_control,
  tuneList = list(
    xg = caretModelSpec(method = "xgbTree",  tuneGrid = xgb.grid),
    pls = caretModelSpec(method = "pls",  tuneGrid = pls.grid)
    
  )
)

# best rmse was around .116
gbm_ensemble <- caretStack(
  model_list,
  method = "gbm",  # enter the model to use here
  metric = "RMSE",
  trControl = trainControl(
    method = "repeatedcv",
    repeats = 3,
    number = 10,
    savePredictions = "final"
  )
)





# code not using caretEnsemble


xgb_tune = train(as.matrix(x_train),
                 y_train,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="RMSE",
                 nthread =3)

xgb_params = list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE)

xgb.cv(xgb_params, dtrain, nrounds = 5000, nfold = 4, early_stopping_rounds = 500)


bst = xgb.train(xgb_params,dtrain,
                nrounds = 1000,
                early_stopping_rounds = 300,
                watchlist = list(train=dtrain))

y_pred.xgb = predict(bst, dtest)
y_pred.xgb = as.double(exp(y_pred.xgb) - 1)



pls <- caret::train(x_train, y_train, method = "pls", trControl = tc,
                    tuneGrid = pls.grid)

y_pls <- predict(pls, newdata = x_test)
y_pls <- exp(y_pls) - 1


pred <- (y_pred.xgb + y_pls)/2
