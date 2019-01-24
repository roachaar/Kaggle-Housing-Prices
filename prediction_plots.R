###
# Plots used in analysis/decision making process
# requires "full" housing dataframe from other code
# can alternatively use any dataframe of the training data

###
ggplot(data = full[1:1460,], aes( x = SalePrice)) + geom_histogram(binwidth = 20000)
# Use log of SalePrice
ggplot(data = full[1:1460,], aes( x = log(SalePrice))) + geom_histogram()

ggplot(data = full[1:1460,], aes( x = MSSubClass, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = MSZoning, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = LotFrontage, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = Neighborhood, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = log(LotArea), y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = OverallQual, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = OverallCond, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = YearBuilt, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = YearRemodAdd, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = MasVnrArea, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BsmtFinSF1, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BsmtFinSF2, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BsmtUnfSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = TotalBsmtSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = X1stFlrSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = X2ndFlrSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = LowQualFinSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = GrLivArea, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BsmtfullBath, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BsmtHalfBath, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = FullBath, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = HalfBath, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = BedroomAbvGr, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = KitchenAbvGr, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = TotRmsAbvGrd, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = GarageType, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = GarageYrBlt, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = GarageCars, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = GarageArea, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = WoodDeckSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = OpenPorchSF, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = EnclosedPorch, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = X3SsnPorch, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = ScreenPorch, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = PoolArea, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = MoSold, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = YrSold, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = SaleType, y = log(SalePrice))) + geom_point()

ggplot(data = full[1:1460,], aes( x = log(SalePrice), y = log(SalePrice))) + geom_point()