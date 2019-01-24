##### Ridge & Lasso method performed here
clean_df <- clean_df[-c(935,1299),]
train_test_mm <- model.matrix(SalePrice ~ ., data = clean_df)
# Ridge coding
library(glmnet)
X <- train_test_mm[1:1458,]
y <- log(train_test_df$SalePrice[1:1458])
ridge_housing <- glmnet(x = X, y = y, alpha = 0)
cv_ridge_housing <- cv.glmnet(X,y, alpha  = 0, nfolds = 10)
bestlam <- cv_ridge_housing$lambda.min
coef(ridge_housing, s = bestlam)

test.X <- train_test_mm[1459:2917,]
ridge_pred_housing <- predict(ridge_housing, s = bestlam, newx = test.X)
ridge_pred_housing <- exp(ridge_pred_housing)

# Ridge Prediction
# 0.14434
ridge_fit_df <- data.frame(Id = 1461:2919, SalePrice = ridge_pred_housing[,1])
write.csv(x = ridge_fit_df, file = "prediction_ridge.csv", row.names = FALSE)

# Lasso coding
X <- train_test_mm[1:1458,]
y <- log(train_test_df$SalePrice[1:1458])
lasso_housing <- glmnet(x = X, y = y, alpha = 1)
cv_lasso_housing <- cv.glmnet(X,y, alpha  = 1, nfolds = 10)
bestlam <- cv_lasso_housing$lambda.min
coef(lasso_housing, s = bestlam)

test.X <- train_test_mm[1459:2917,]
lasso_pred_housing <- predict(lasso_housing, s = bestlam, newx = test.X)
lasso_pred_housing <- exp(lasso_pred_housing)


# Lasso Prediction
# 0.12912
lasso_fit_df <- data.frame(Id = 1461:2919, SalePrice = lasso_pred_housing[,1])
write.csv(x = lasso_fit_df, file = "prediction_lasso.csv", row.names = FALSE)
 