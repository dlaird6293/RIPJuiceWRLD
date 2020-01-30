library(dplyr)


ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
#subset of ameslist with int vals that can be intuited.
Ames <- select(ameslist,c(Id, LotFrontage, LotArea, OverallQual, OverallCond, YearBuilt, YearRemodAdd, 
                          BsmtFinSF1, BsmtUnfSF, TotalBsmtSF,X1stFlrSF, X2ndFlrSF, BsmtFullBath, BsmtHalfBath,
                          FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd,Fireplaces, GarageYrBlt, GarageCars,
                          GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, ScreenPorch, PoolArea, MoSold, YrSold,
                          SalePrice))

#Subset data for matrix
Pricecorr <- select(Ames,c(LotArea,OverallQual,BsmtFinSF1,GarageArea,WoodDeckSF,Fireplaces,
                           GarageCars,PoolArea, FullBath, OpenPorchSF, TotRmsAbvGrd, SalePrice))

#correlation matrix
pairs(Pricecorr)

#look at actual correlations
cor(Pricecorr)

#For these results, I would have expected pool area and wood dech square footage to be stronger predictors of 
#sale price, but as it turns out, some of the stgrongestr predictors are the Overall Quality
#measurte, and Garage area.  Total rooms baove ground also shows a decently srtrong correlation,
#but things like number of bathrooms and Finished basement square footage had less of an effect
#than expected on sale price.  It is surprising to see information relating to the garage 
#being some of the strongest predictors based on house characteristics.


plot(ameslist$SalePrice, ameslist$GrLivArea, main="Price vs GR Living Area",
     xlab="Sale Price ", ylab="Gr Living Area ", pch=19)
abline(abline(lm(GrLivArea~SalePrice, data=ameslist),col="red"))

#The largest outlier is a house with over 5000 GR Living area, but sold for less than 200,000 dollars.
#Upon further inspection, It is a 2 story house, with large square footage, an unfisished basememt,
#large garage, and a pool.  In all, it seems like a very nice house, but it was sold in 2008, 
#so there is a good chance that the recession is the reason this house sold for so little.


lm(SalePrice ~ GarageType, data=ameslist)
#Coefficients:
#  (Intercept)   GarageTypeAttchd  GarageTypeBasment  GarageTypeBuiltIn  GarageTypeCarPort  
#151283              51609               9287             103468             -41321  
#GarageTypeDetchd  
#-17192  
multreg <- lm(SalePrice ~ (LotFrontage+ LotArea+ OverallQual+OverallCond+ YearBuilt+ YearRemodAdd+
                             BsmtFinSF1+ BsmtUnfSF+ TotalBsmtSF+X1stFlrSF+ X2ndFlrSF+ BsmtFullBath+BsmtHalfBath+
                             FullBath+ HalfBath+ BedroomAbvGr+ KitchenAbvGr+ TotRmsAbvGrd+Fireplaces+ GarageYrBlt+ GarageCars+
                             GarageArea+ WoodDeckSF+ OpenPorchSF+ EnclosedPorch+ ScreenPorch+ PoolArea+ MoSold+ YrSold+
                             SalePrice), data=ameslist)


summary(multreg)
#It looks like the strongest relationships between sale price and these variables 
#exist among factors like Square footage, how many cars the garage holds, the lot area, and 
#the total number of rooms above ground.  The year variable suggests that for every year
#after being built, houses will depreciate in value by the smount of the coefficient 
#(in this model, at least.)

plot(multreg)
#the fit is mostly good until you start to reach x values of 400000 ad over.
#  These points would be outliers if the fit of the model was more accurate, but in this case,
#all of the values take a sharp upward turn un this area.  This model leaves out these higher valued 
#estimates.  There are some outliers with low residual values that could be skewing the fit in this 
#direction, so this may explain the problem.
multregint <- lm(SalePrice ~ (LotFrontage+ LotArea+ OverallQual+OverallCond+ YearBuilt+ YearRemodAdd+
                                BsmtFinSF1+ BsmtUnfSF+ TotalBsmtSF+X1stFlrSF* X2ndFlrSF+ BsmtFullBath+BsmtHalfBath+
                                FullBath* HalfBath+ BedroomAbvGr+ KitchenAbvGr+ TotRmsAbvGrd+Fireplaces+ GarageYrBlt+ GarageCars+
                                GarageArea+ WoodDeckSF+ OpenPorchSF+ EnclosedPorch+ ScreenPorch+ PoolArea+ MoSold+ YrSold+
                                SalePrice), data=ameslist)


plot(multregint)
#it may make sense to add interaction terms between variables like 1st floor square footage and 2nd floor square
#footage, given that second floor SF depends on the existance of a 1st floor.  Also, half-baths
#and full-baths may be able to be modeled with an interaction term involved, because very few houses 
#just have one or the other.  Overall, these terms don't help out model a lot, but it is worth
#exploring some of these factors.


# Taking ln() of sale price
multreg_2 <- lm(log(SalePrice) ~ (LotFrontage+ LotArea+ OverallQual+OverallCond+ YearBuilt+ YearRemodAdd+
                             BsmtFinSF1+ BsmtUnfSF+ TotalBsmtSF+X1stFlrSF+ X2ndFlrSF+ BsmtFullBath+BsmtHalfBath+
                             FullBath+ HalfBath+ BedroomAbvGr+ KitchenAbvGr+ TotRmsAbvGrd+Fireplaces+ GarageYrBlt+ GarageCars+
                             GarageArea+ WoodDeckSF+ OpenPorchSF+ EnclosedPorch+ ScreenPorch+ PoolArea+ MoSold+ YrSold+
                             SalePrice), data=ameslist)

plot(multreg_2)

# Taking the natural log of SalePrice gives a better fit to the residuals. This especially helps with residuals since 
#the data is skewed right.

