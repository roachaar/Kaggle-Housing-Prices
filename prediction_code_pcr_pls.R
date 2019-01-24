library(pls)
# start with pcr
#####
pcr_fit_final <- pcr(log(SalePrice) ~ .,
                     data = train_df[-c(935,1299),], scale = FALSE, validation = "CV")
# - LotFrontage - LotArea - X1stFlrSF - GrLivArea
# + log(LotFrontage) + log(LotArea) + log(X1stFlrSF) + log(GrLivArea)
pcr_cv_rmse <- RMSEP(pcr_fit_final)
pcr_cv_rmse$val[1,1,]
which.min(pcr_cv_rmse$val[1,1,-1])
validationplot(pcr_fit_final, val.type = "MSEP")
# PCR Prediction finally got it to work thanks to help from Dr. Sung (scale = FALSE)
# 0.12466 when I change LotFrontage to overall median
pcr_pred_final <- predict(pcr_fit_final, newdata = test_df, ncomp = which.min(pcr_cv_rmse$val[1,1,-1]))
pcr_pred_final_df <- data.frame(Id = 1461:2919, SalePrice = exp(pcr_pred_final))
colnames(pcr_pred_final_df) <- c("Id", "SalePrice")
write.csv(x = pcr_pred_final_df, file = "prediction_pcr.csv", row.names = FALSE)
##########
##########

# now trying pls
# Just change all instances of pcr above to pls and copy the code, using the plsr function
pls_fit_final <- plsr(log(SalePrice) ~ .,
                      data = train_df[-c(935,1299),], scale = FALSE, validation = "CV")
pls_cv_rmse <- RMSEP(pls_fit_final)
pls_cv_rmse$val[1,1,]
which.min(pls_cv_rmse$val[1,1,-1])
validationplot(pls_fit_final, val.type = "MSEP")
# pls Prediction finally got it to work thanks to help from Dr. Sung (scale = FALSE)
# 0.12385 when I change LotFrontage to median of respective neighborhood and remove outliers
# 0.12377 when I change LotFrontage to overall median and remove outliers
pls_pred_final <- predict(pls_fit_final, newdata = test_df, ncomp = which.min(pls_cv_rmse$val[1,1,-1]))
pls_pred_final_df <- data.frame(Id = 1461:2919, SalePrice = exp(pls_pred_final))
colnames(pls_pred_final_df) <- c("Id", "SalePrice")
write.csv(x = pls_pred_final_df, file = "prediction_pls.csv", row.names = FALSE)
