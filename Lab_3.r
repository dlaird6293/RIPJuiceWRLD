# Load in data exclude 2 variabes
library(tidyverse)


ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
df <- (ameslist)
ames = subset(df, select = -c(OverallCond,OverallQual))

#create series of models for prediction of RMSE

library(MASS)

model1 <- lm(SalePrice ~1,data = ames)

model2 <- lm(SalePrice ~ (LotArea), data = ames)

model3 <- lm(SalePrice ~ (LotArea+  YearBuilt), data = ames)

model4 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF), data = ames)

model5 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF), data = ames)

model6 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                            BsmtFullBath), data = ames)

model7 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                            BsmtFullBath+ BsmtHalfBath), data = ames)

model8 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                            BsmtFullBath+ BsmtHalfBath+BedroomAbvGr), data = ames)

model9 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                            BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr), data = ames)

model10 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd), data = ames)

model11 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars), data = ames)

model12 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars+WoodDeckSF), data = ames)

model13 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch), data = ames)

model14 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch+
                             PoolArea), data = ames)

model15 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch+
                             PoolArea+CentralAir), data = ames)

model16 <- lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                             BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                             TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch+
                             PoolArea+MSSubClass+CentralAir), data = ames)




test <- lm(SalePrice~GarageCars, data=ames)



get_complexity = function(model) {
  
  length(coef(model)) - 1
  
}

#given rmse

rmse = function(actual, predicted) {
  
  sqrt(mean((actual - predicted) ^ 2))
  
}

chart <- plot(c(get_complexity(model2),get_complexity(model3)
                ,get_complexity(model4),get_complexity(model5),get_complexity(model6),
                get_complexity(model7),get_complexity(model8),get_complexity(model9),get_complexity(model10)
                ,get_complexity(model11),get_complexity(model12),get_complexity(model13),get_complexity(model14)
                ,get_complexity(model15),get_complexity(model16)),c(rmse(ames$SalePrice,predict(model2)),rmse(ames$SalePrice,predict(model3)),
                                                                    rmse(ames$SalePrice,predict(model4)),rmse(ames$SalePrice,predict(model5)),
                                                                    rmse(ames$SalePrice,predict(model6)),rmse(ames$SalePrice,predict(model7)),
                                                                    rmse(ames$SalePrice,predict(model8)),rmse(ames$SalePrice,predict(model9)),
                                                                    rmse(ames$SalePrice,predict(model10)),rmse(ames$SalePrice,predict(model11)),
                                                                    rmse(ames$SalePrice,predict(model12)), rmse(ames$SalePrice,predict(model13)),
                                                                    rmse(ames$SalePrice,predict(model14)),rmse(ames$SalePrice,predict(model15)),
                                                                    rmse(ames$SalePrice,predict(model16))),main="rmse vs complexity", 
              xlab="Complexity", ylab="Rmse")


set.seed(9)
num_obs = nrow(ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = ames[train_index, ]
test_data = ames[-train_index, ]


fit_0 = lm(SalePrice ~ 1, data = train_data)
fit_1=lm(SalePrice ~ (LotArea), data = ames)
fit_2=lm(SalePrice ~ (LotArea+  YearBuilt), data = ames)
fit_3=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF), data = ames)
fit_4=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF), data = ames)
fit_5=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                        BsmtFullBath), data = ames)
fit_6=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                        BsmtFullBath+ BsmtHalfBath), data = ames)
fit_7=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                        BsmtFullBath+ BsmtHalfBath), data = ames)
fit_8=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                        BsmtFullBath+ BsmtHalfBath+BedroomAbvGr), data = ames)
fit_9=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                        BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr), data = ames)
fit_10=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd), data = ames)
fit_11=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd+ GarageCars), data = ames)
fit_12=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd+ GarageCars+WoodDeckSF), data = ames)
fit_13=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch), data = ames)
fit_14=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch+
                         PoolArea), data = ames)
fit_15=lm(SalePrice ~ (LotArea+  YearBuilt+X1stFlrSF+X2ndFlrSF+
                         BsmtFullBath+ BsmtHalfBath+BedroomAbvGr+KitchenAbvGr+
                         TotRmsAbvGrd+ GarageCars+WoodDeckSF+ScreenPorch+
                         PoolArea+MSSubClass), data = ames)
get_complexity(fit_1)



# train RMSE

print(paste0("Train: ", sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))))

# test RMSE
print(paste0("Test: ", sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))))

# train RMSE
print(paste0("Train: ", rmse(actual = train_data$SalePrice, predicted = predict(fit_0, train_data))))
# test RMSE
print(paste0("Test: ", rmse(actual = test_data$SalePrice, predicted = predict(fit_0, test_data))))




get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

print(paste0("Output: ", get_rmse(model = fit_0, data = train_data, response = "SalePrice"))) # train RMSE
print(paste0("Output: ", get_rmse(model = fit_0, data = test_data, response = "SalePrice"))) # test RMSE


model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5,fit_6,fit_7,fit_8,fit_9,fit_10,fit_11,fit_12
                  ,fit_13,fit_14,fit_15)


train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)


plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")

finalmodel <- lm(SalePrice ~ (LotArea+YearBuilt+X1stFlrSF+X2ndFlrSF+
        BedroomAbvGr+KitchenAbvGr+TotRmsAbvGrd+GarageCars+ScreenPorch), data = ames)

cat("Train RMSE: ",get_rmse(model = finalmodel, data = train_data, response = "SalePrice")) # train RMSE

cat("Test RMSE: ",get_rmse(model = finalmodel, data = test_data, response = "SalePrice")) # test RMSE

#Exercise 2 part 2

set.seed(9)
num_obs = nrow(ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = ames[train_index, ]
test_data = ames[-train_index, ]
get_complexity(fit_0)

# train RMSE
sqrt(mean((train_data$SalePrice - predict(finalmodel, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(finalmodel, test_data)) ^ 2))

# train RMSE
rmse(actual = train_data$SalePrice, predicted = predict(finalmodel, train_data))
# test RMSE
rmse(actual = test_data$SalePrice, predicted = predict(finalmodel, test_data))




get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}
