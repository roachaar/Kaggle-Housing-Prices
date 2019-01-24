setwd("C:/Users/Aaron/Desktop/STT 481/STT 481 Project")
library(e1071)
library(tidyverse)
library(leaps)
library(glmnet)
library(pls)
library(FNN)
#####
#####
#####
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test[,81] <- rep(0, nrow(test))
colnames(test)[81] <- "SalePrice"
#####
#####
#####
train_test_df <- rbind(train,test)
# Start with data cleaning
# Finding out where the NA's are
na.values1 <- colSums(is.na(train_test_df))
na.values1[na.values1 != 0]

# LotFrontage
median_LotFrontage <- median(train_test_df$LotFrontage, na.rm = TRUE)
train_test_df$LotFrontage <- as.character(train_test_df$LotFrontage)
train_test_df$LotFrontage[is.na(train_test_df$LotFrontage)] <- median_LotFrontage
train_test_df$LotFrontage <- as.double(train_test_df$LotFrontage)

# Fixing Alley, NA means "No Alley" in this case
train_test_df$Alley <- as.character(train_test_df$Alley)
train_test_df$Alley[is.na(train_test_df$Alley)] <- "None"
train_test_df$Alley <- as.factor(train_test_df$Alley)

# Fixing MasVnrType, there are only 8 NA's here
# the column is worth saving
train_test_df$MasVnrType <- as.character(train_test_df$MasVnrType)
train_test_df$MasVnrType[is.na(train_test_df$MasVnrType)] <- "None"
train_test_df$MasVnrType <- as.factor(train_test_df$MasVnrType)

# Fixing MasVnrArea, (again) there are only 8 NA's, store as mean
# None => 0
train_test_df$MasVnrArea <- as.character(train_test_df$MasVnrArea)
train_test_df$MasVnrArea[is.na(train_test_df$MasVnrArea)] <- 0
train_test_df$MasVnrArea <- as.double(train_test_df$MasVnrArea)

# Fixing BsmtQual, NA means no basement
train_test_df$BsmtQual <- as.character(train_test_df$BsmtQual)
train_test_df$BsmtQual[is.na(train_test_df$BsmtQual)] <- "None"
train_test_df$BsmtQual <- as.factor(train_test_df$BsmtQual)

# Fixing BsmtCond, NA means no basement
train_test_df$BsmtCond <- as.character(train_test_df$BsmtCond)
train_test_df$BsmtCond[is.na(train_test_df$BsmtCond)] <- "None"
train_test_df$BsmtCond <- as.factor(train_test_df$BsmtCond)

# Fixing BsmtExposure, NA means no basement
train_test_df$BsmtExposure <- as.character(train_test_df$BsmtExposure)
train_test_df$BsmtExposure[is.na(train_test_df$BsmtExposure)] <- "None"
train_test_df$BsmtExposure <- as.factor(train_test_df$BsmtExposure)

# Fixing BsmtFinType1, NA means no basement
train_test_df$BsmtFinType1 <- as.character(train_test_df$BsmtFinType1)
train_test_df$BsmtFinType1[is.na(train_test_df$BsmtFinType1)] <- "None"
train_test_df$BsmtFinType1 <- as.factor(train_test_df$BsmtFinType1)

# Fixing BsmtFinType2, NA means no basement
train_test_df$BsmtFinType2 <- as.character(train_test_df$BsmtFinType2)
train_test_df$BsmtFinType2[is.na(train_test_df$BsmtFinType2)] <- "None"
train_test_df$BsmtFinType2 <- as.factor(train_test_df$BsmtFinType2)

# Fixing Electrical, there is only 1 NA here
# the column is worth saving
train_test_df$Electrical <- as.character(train_test_df$Electrical)
train_test_df$Electrical[is.na(train_test_df$Electrical)] <- "SBrkr"
train_test_df$Electrical <- as.factor(train_test_df$Electrical)

# Fixing FireplaceQu, NA means no fireplace
train_test_df$FireplaceQu <- as.character(train_test_df$FireplaceQu)
train_test_df$FireplaceQu[is.na(train_test_df$FireplaceQu)] <- "None"
train_test_df$FireplaceQu <- as.factor(train_test_df$FireplaceQu)

# Fixing GarageType, NA means no garage
train_test_df$GarageType <- as.character(train_test_df$GarageType)
train_test_df$GarageType[is.na(train_test_df$GarageType)] <- "None"
train_test_df$GarageType <- as.factor(train_test_df$GarageType)

# Fixing GarageYrBlt, NA means no garage, store as mean
median_yr <- median(train_test_df$GarageYrBlt, na.rm = TRUE)
train_test_df$GarageYrBlt <- as.character(train_test_df$GarageYrBlt)
train_test_df$GarageYrBlt[is.na(train_test_df$GarageYrBlt)] <- median_yr
train_test_df$GarageYrBlt <- as.double(train_test_df$GarageYrBlt)

# Fixing GarageFinish, NA means no garage
train_test_df$GarageFinish <- as.character(train_test_df$GarageFinish)
train_test_df$GarageFinish[is.na(train_test_df$GarageFinish)] <- "None"
train_test_df$GarageFinish <- as.factor(train_test_df$GarageFinish)

# Fixing GarageQual, NA means no garage
train_test_df$GarageQual <- as.character(train_test_df$GarageQual)
train_test_df$GarageQual[is.na(train_test_df$GarageQual)] <- "None"
train_test_df$GarageQual <- as.factor(train_test_df$GarageQual)

# Fixing GarageCond, NA means no garage
train_test_df$GarageCond <- as.character(train_test_df$GarageCond)
train_test_df$GarageCond[is.na(train_test_df$GarageCond)] <- "None"
train_test_df$GarageCond <- as.factor(train_test_df$GarageCond)

# Fixing PoolQC, NA means no pool
train_test_df$PoolQC <- as.character(train_test_df$PoolQC)
train_test_df$PoolQC[is.na(train_test_df$PoolQC)] <- "None"
train_test_df$PoolQC <- as.factor(train_test_df$PoolQC)

# Fixing Fence, NA means no fence
train_test_df$Fence <- as.character(train_test_df$Fence)
train_test_df$Fence[is.na(train_test_df$Fence)] <- "None"
train_test_df$Fence <- as.factor(train_test_df$Fence)

# MiscFeature, NA means None
train_test_df$MiscFeature <- as.character(train_test_df$MiscFeature)
train_test_df$MiscFeature[is.na(train_test_df$MiscFeature)] <- "None"
train_test_df$MiscFeature <- as.factor(train_test_df$MiscFeature)
###################################

sum(is.na(train_test_df))

### Fixing the leftover NA's:
# KitchenQual, NA means Poor (Guess)
train_test_df$KitchenQual <- as.character(train_test_df$KitchenQual)
train_test_df$KitchenQual[is.na(train_test_df$KitchenQual)] <- "TA"
train_test_df$KitchenQual <- as.factor(train_test_df$KitchenQual)

# MSZoning, rows are 456, 757, 791, 1445
# I think it is reasonable to say Street = Gravel => MSZoning = A (456)
# Street = Pave => MSZoning = RM (757 & 791 & RM)
train_test_df$MSZoning <- as.character(train_test_df$MSZoning)
train_test_df$MSZoning[is.na(train_test_df$MSZoning)] <- "RM"
# I can't introduce A as a new factor, so I will just keep all as "RM"
train_test_df$MSZoning <- as.factor(train_test_df$MSZoning)

# Utilities, NA means no utilities (guess), we default to NoSeWa 
# because this seems closest to no utilities (minimal utilities)
# train_test_df$Utilities <- as.character(train_test_df$Utilities)
# train_test_df$Utilities[is.na(train_test_df$Utilities)] <- "NoSeWa"
# train_test_df$Utilities <- as.factor(train_test_df$Utilities)
# After further investigation, literally every row of train
# has Utilities = Pub, every row except the NA's have Utilities = Pub
# So we are just going to drop Utilities from the train & train_test_df
# But I am having trouble doing that, I tried:
# train_test_df <- train_test_df[,-8]
# train <- train[,-8]
# but this causes later issues with the regression
# So I will settle with this for now:
train_test_df$Utilities <- as.character(train_test_df$Utilities)
train_test_df$Utilities[is.na(train_test_df$Utilities)] <- "AllPub"
train_test_df$Utilities <- as.factor(train_test_df$Utilities)

# BsmtFinSF1, NA might mean 0 (guess)
train_test_df$BsmtFinSF1 <- as.character(train_test_df$BsmtFinSF1)
train_test_df$BsmtFinSF1[is.na(train_test_df$BsmtFinSF1)] <- "0"
train_test_df$BsmtFinSF1 <- as.double(train_test_df$BsmtFinSF1)

# BsmtFinSF2, NA might mean 0 (guess)
train_test_df$BsmtFinSF2 <- as.character(train_test_df$BsmtFinSF2)
train_test_df$BsmtFinSF2[is.na(train_test_df$BsmtFinSF2)] <- "0"
train_test_df$BsmtFinSF2 <- as.double(train_test_df$BsmtFinSF2)

# BsmtUnfSF, NA might mean 0 (guess)
train_test_df$BsmtUnfSF <- as.character(train_test_df$BsmtUnfSF)
train_test_df$BsmtUnfSF[is.na(train_test_df$BsmtUnfSF)] <- "0"
train_test_df$BsmtUnfSF <- as.double(train_test_df$BsmtUnfSF)

# TotalBsmtSF, NA might mean 0 (guess)
train_test_df$TotalBsmtSF <- as.character(train_test_df$TotalBsmtSF)
train_test_df$TotalBsmtSF[is.na(train_test_df$TotalBsmtSF)] <- "0"
train_test_df$TotalBsmtSF <- as.double(train_test_df$TotalBsmtSF)

# BsmtFullBath, NA might mean 0 (guess)
train_test_df$BsmtFullBath <- as.character(train_test_df$BsmtFullBath)
train_test_df$BsmtFullBath[is.na(train_test_df$BsmtFullBath)] <- "0"
train_test_df$BsmtFullBath <- as.double(train_test_df$BsmtFullBath)

# BsmtHalfBath, NA might mean 0 (guess)
train_test_df$BsmtHalfBath <- as.character(train_test_df$BsmtHalfBath)
train_test_df$BsmtHalfBath[is.na(train_test_df$BsmtHalfBath)] <- "0"
train_test_df$BsmtHalfBath <- as.double(train_test_df$BsmtHalfBath)

# Exterior1st, NA might mean VinylSd (guess)
# Motivation: which.max(summary(train$Exterior1st))
# VinylSd is #13
train_test_df$Exterior1st <- as.character(train_test_df$Exterior1st)
train_test_df$Exterior1st[is.na(train_test_df$Exterior1st)] <- "VinylSd"
train_test_df$Exterior1st <- as.factor(train_test_df$Exterior1st)

# Exterior2nd, NA might mean VinylSd (guess)
# Same motivation: which.max(summary(train$Exterior2nd))
train_test_df$Exterior2nd <- as.character(train_test_df$Exterior2nd)
train_test_df$Exterior2nd[is.na(train_test_df$Exterior2nd)] <- "VinylSd"
train_test_df$Exterior2nd <- as.factor(train_test_df$Exterior2nd)

# Functional, NA might mean Typ (guess, most common again)
train_test_df$Functional <- as.character(train_test_df$Functional)
train_test_df$Functional[is.na(train_test_df$Functional)] <- "Typ"
train_test_df$Functional <- as.factor(train_test_df$Functional)

# SaleType, NA might mean WD (guess)
train_test_df$SaleType <- as.character(train_test_df$SaleType)
train_test_df$SaleType[is.na(train_test_df$SaleType)] <- "WD"
train_test_df$SaleType <- as.factor(train_test_df$SaleType)

# GarageCars should be 0 since:
# train_test_df$GarageFinish[is.na(train_test_df$GarageCars)]
train_test_df$GarageCars <- as.character(train_test_df$GarageCars)
train_test_df$GarageCars[is.na(train_test_df$GarageCars)] <- "0"
train_test_df$GarageCars <- as.double(train_test_df$GarageCars)

# GarageArea
train_test_df$GarageArea <- as.character(train_test_df$GarageArea)
train_test_df$GarageArea[is.na(train_test_df$GarageArea)] <- "0"
train_test_df$GarageArea <- as.double(train_test_df$GarageArea)

# MSSubClass is a factor
train_test_df$MSSubClass <- as.factor(train_test_df$MSSubClass)

# GarageYrBlt nonsense year
train_test_df$GarageYrBlt[2593] <- 2007

#####
train_test_mm <- model.matrix(SalePrice ~ . ,  data = train_test_df)
useless_columns <- which(colSums(train_test_mm)<5)
train_test_mm <- train_test_mm[,-c(1,2,useless_columns)]