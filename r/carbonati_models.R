rm(list=ls())

library(tidyverse)
library(caret)

# Changes to tanners code
# He beens quantitative variables by 20 years - try other (wider) binwidths
# He "normalizes" the data, try not
# try straight averaging pcr, lasso, pls, and enet
# grouped lasso


# to start run carbonati kernel
# this gives you x_train and y_traing
runAllChunks <- function(rmd, envir=globalenv()){ tempR <- tempfile(tmpdir = ".",fileext = ".R")
                        knitr::purl(rmd, output=tempR)
                        sys.source(tempR, envir=envir) 
                        unlink(tempR)
}
runAllChunks("Carbonati_Untouched.rmd")


tc <- trainControl(method = "repeatedcv", number = 10, repeats = 3)






# Elastic Net

enet.grid <- expand.grid( lambda = seq(from = .01, to = .2, by = .03),
                          fraction = seq(from = .1, to = 1, by = .1))

enet <- caret::train(x_train, y_train,
                     method = "enet",
                     trControl = tc,
                     tuneGrid = enet.grid)



# PLS

pls.grid <- data.frame(ncomp=1:50)

pls <- caret::train(x_train, y_train, method = "pls", trControl = tc,
                    tuneGrid = pls.grid)

y_pls <- predict(pls, newdata = x_test)
y_pls <- exp(y_pls) - 1


# Lasso cv: .112

lasso.grid <- expand.grid(alpha = seq(from = .01, to = 1, by = .1),
                                       lambda =seq(from = .0001, to = .1, by = .005))
lasso <- caret::train(x_train, y_train,
                      method = "glmnet",
                      trControl = tc,
                      tuneGrid = lasso.grid)

y_lasso <- predict(lasso, newdata = x_test)
y_lasso <- exp(y_lasso) - 1




# XGBoost

dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))cv.ctrl = trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                                                             allowParallel=T)

xgb.grid = expand.grid(nrounds = 10000,
                       eta = 0.005,
                       max_depth = 6,
                       colsample_bytree = 1,
                       min_child_weight = 2,
                       subsample = 0.2,
                       gamma=0.01)


set.seed(45)


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








library(caretEnsemble)


my_control <- trainControl(
  method="repeatedcv",
  number= 10,
  repeats = 2,
  savePredictions="final",
  #index=createResample(ames$SalePrice, 140)
)


# best fit was with lasso, pls,  gbm, and earth

model_list <- caretList(
  x_train, y_train,
  #trControl=my_control,
  tuneList = list(
    lasso = caretModelSpec(method = "glmnet"),
    enet = caretModelSpec(method = "enet", tuneGrid = enet.grid),
    pls = caretModelSpec(method = "pls",  tuneGrid = pls.grid),
    pcr = caretModelSpec(method = "pls",  tuneGrid = pcr.grid)

  )
)



modelCor(resamples(model_list))

greedy <- caretEnsemble(
  model_list, 
  metric="RMSE",
  trControl=trainControl(number=5))

# best rmse was around .116
gbm_ensemble <- caretStack(
  model_list,
  method = "gbm",
  metric = "RMSE",
  trControl = trainControl(
    method = "repeatedcv",
    repeats = 3,
    number = 10,
    savePredictions = "final"
  )
)





y_ens <- (y_pred.xgb + y_pred.net)/2.0
y_ens <- (y_pred.xgb + y_pls)/2.0



testid <- read.csv("../data/test.csv")

pred.df <- data.frame(Id = testid$Id, SalePrice = y_pls)

write.csv(pred.df, "submission_002.csv", row.names = F)
