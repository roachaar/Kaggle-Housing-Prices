library(e1071)
library(tidyverse)
library(leaps)
library(glmnet)
library(pls)
library(FNN)

# Need predict.regsubsets function
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coef <- coef(object, id = id)
  xvars <- names(coef)
  return(mat[,xvars] %*% coef)
}

# set seed to get consistent results
set.seed(4)
# isolate training data to perform cv
cv_df <- data.frame(train_test_mm[1:1460,])
cv_df[,"SalePrice"] <- train_test_df$SalePrice[1:1460]
cv_df <- cv_df[-c(524,1299),]
# randomly split into 10 sets to perform cv
fold.index <- cut(sample(1:nrow(cv_df)), breaks=10, labels=FALSE)
# Form the error vectors we will be using later
test_error_lm <- rep(0, 10)
test_error_fwd_cp <- rep(0, 10)
test_error_fwd_bic <- rep(0, 10)
test_error_bwd_cp <- rep(0,10)
test_error_bwd_bic <- rep(0, 10)
test_error_ridge <- rep(0,10)
test_error_lasso <- rep(0,10)
test_error_pcr <- rep(0,10)
test_error_pls <- rep(0,10)
test_error_svm <- rep(0,10)
# used for ridge and lasso since we need model matrices
# same as before
X <- train_test_mm[1:1460,]
X <- X[-c(524,1299),]
y <- log(train_test_df$SalePrice[1:1460])
y <- y[-c(524,1299)]
# create a for loop as in the homework
# testing each fold.index and forming a vector of RMSE of the log of SalePrice
for (i in 1:10){
  # fit models
  fit_lm <- lm(log(SalePrice) ~ ., data = cv_df[!(fold.index == i),])
  fit_fwd <- regsubsets(log(SalePrice) ~ .,
                        data = cv_df[!(fold.index == i), ], nvmax = 150,
                        really.big = TRUE, method = "forward")
  fit_bwd <- regsubsets(log(SalePrice) ~ .,
                        data = cv_df[!(fold.index == i), ], nvmax = 150,
                        really.big = TRUE, method = "backward")
  fit_ridge <- glmnet(x = X[!(fold.index == i),], y = y[!(fold.index==i)], alpha = 0)
  cv_ridge <- cv.glmnet(X[!(fold.index == i),],y[!(fold.index == i)], alpha = 0, nfolds = 10)
  bestlam_ridge <- cv_ridge$lambda.min
  fit_lasso <- glmnet(x = X[!(fold.index == i),], y = y[!(fold.index==i)], alpha = 1)
  cv_lasso <- cv.glmnet(X[!(fold.index == i),],y[!(fold.index == i)], alpha = 1, nfolds = 10)
  bestlam_lasso <- cv_lasso$lambda.min
  fit_pcr <- pcr(log(SalePrice) ~ ., data = cv_df[!(fold.index == i),],
                 scale = FALSE, validation = "CV")
  cv_pcr <- RMSEP(fit_pcr)
  fit_pls <- plsr(log(SalePrice) ~ ., data = cv_df[!(fold.index == i),],
                  scale = FALSE, validation = "CV")
  cv_pls <- RMSEP(fit_pls)
  # make predictions
  svm_fit <- svm(log(SalePrice) ~ ., data = cv_df[!(fold.index == i),], cost = 3)
  pred_lm <- predict(fit_lm, cv_df[fold.index == i,])
  pred_fwd_cp <- predict.regsubsets(fit_fwd, cv_df[fold.index == i,], which.min(summary(fit_fwd)$cp))
  pred_fwd_bic <- predict.regsubsets(fit_fwd, cv_df[fold.index == i,], which.min(summary(fit_fwd)$bic))
  pred_bwd_cp<- predict.regsubsets(fit_bwd, cv_df[fold.index == i,], which.min(summary(fit_fwd)$cp))
  pred_bwd_bic <- predict.regsubsets(fit_bwd, cv_df[fold.index == i,],
                                     which.min(summary(fit_fwd)$bic))
  pred_ridge <- predict(fit_ridge, s = bestlam_ridge, newx = X[fold.index == i,])
  pred_lasso <- predict(fit_lasso, s = bestlam_lasso, newx = X[fold.index == i,])
  pred_pcr <- predict(fit_pcr, newdata = cv_df[fold.index == i,],
                      ncomp = which.min(cv_pcr$val[1,1,-1]))
  pred_pls <- predict(fit_pls, newdata = cv_df[fold.index == i,],
                      ncomp = which.min(cv_pls$val[1,1,-1]))
  pred_svm <- predict(object = svm_fit, newdata = cv_df[fold.index==i,])
  # the next line is the vector we are testing against
  cv_test <- log(cv_df[fold.index==i,]$SalePrice)
  # now we calculate errors for each prediction
  error_lm <- sqrt(mean((cv_test-pred_lm)^2))
  error_fwd_cp <- sqrt(mean((cv_test-pred_fwd_cp)^2))
  error_fwd_bic <- sqrt(mean((cv_test-pred_fwd_bic)^2))
  error_bwd_cp <- sqrt(mean((cv_test-pred_bwd_cp)^2))
  error_bwd_bic <- sqrt(mean((cv_test-pred_bwd_bic)^2))
  error_ridge <- sqrt(mean((cv_test-pred_ridge)^2))
  error_lasso <- sqrt(mean((cv_test-pred_lasso)^2))
  error_pcr <- sqrt(mean((cv_test-pred_pcr)^2))
  error_pls <- sqrt(mean((cv_test-pred_pls)^2))
  error_svm <- sqrt(mean((cv_test-pred_svm)^2))
  # then we store each of the errors as the ith coordinate of our error vector
  test_error_lm[i] <- error_lm
  test_error_fwd_cp[i] <- error_fwd_cp
  test_error_fwd_bic[i] <- error_fwd_bic
  test_error_bwd_cp[i] <- error_bwd_cp
  test_error_bwd_bic[i] <- error_bwd_bic
  test_error_ridge[i] <- error_ridge
  test_error_lasso[i] <- error_lasso
  test_error_pcr[i] <- error_pcr
  test_error_pls[i] <- error_pls
  test_error_svm[i] <- error_svm
}
# we create a nice pretty vector with names showing each mean test error
result_vector <- c("lm" = mean(test_error_lm), "fwd_cp" = mean(test_error_fwd_cp),
                   "fwd_bic" = mean(test_error_fwd_bic),
                   "bwd_cp" = mean(test_error_bwd_cp),
                   "bwd_bic" = mean(test_error_bwd_bic),
                   "ridge" = mean(test_error_ridge),
                   "lasso" = mean(test_error_lasso),
                   "pcr" = mean(test_error_pcr),
                   "pls" = mean(test_error_pls),
                   "svm" = mean(test_error_svm))

result_vector
which.min(result_vector)

# lasso is selected as the best
# by this CV code
# and SVM does quite bad for some reason, this is one of my many data cleanings
# so this one could be bad for SVM

# The True order is (in order of lowest RMSLE): svm, pls, pcr, lasso, ...
# See project (powerpoint slides) for actual predictions
# also kaggle website

#######
# Our best predictions came from taking the average of numerous models
# The models came from 3 different sets of cleaned data, all cleaned in different ways
# The models used to average were PLS and SVM since they were the best
# In the first avg_pred_df below, the most naive weights were chosen: all 1/3
# In the second, the weights were chosen based on individual model accuracy

# 0.11900
#avg_pred_df <- data.frame("Id" = 1461:2919, "SalePrice" = exp((pls_pred_final + svr_pred + pred.svm)/3))
#write.csv(x = avg_pred_df, file = "avg_pred2.csv", row.names = FALSE)

# 0.11778
#avg_pred_df2 <- data.frame("Id" = 1461:2919, "SalePrice" = exp((pls_pred_final + pred.svm + svm_pred + pred.svm + svm_pred + pred.svm)/6))
#write.csv(x = avg_pred_df3, file = "avg_pred2.csv", row.names = FALSE)
#######