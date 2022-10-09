##install.packages("dplyr")
##install.packages("mlbench")

library(dplyr)
library(mlbench)

data("BostonHousing")

df <- BostonHousing
n <- nrow(df)

## review correlation 
df %>% 
    select(medv, crim, indus, tax) %>%
    cor()

## Train, Test, Split

## 1.) Split Data 
set.seed(42)
id <- sample(1:n, size = n*0.7) #train 70%, test 30%

train_data <- df[id, ]
test_data <- df[-id, ]

View(train_data)

## 2.) Train Model 
# build model
lm_model <- lm(medv ~ crim+indus+tax, data = train_data)
summary(lm_model)
coefs_train <- coef(lm_model)
# predict train model
predict_train <- predict(lm_model)
# find RMSE
rmse_train <- sqrt(mean((train_data$medv - predict_train)**2)) 


## 3.) Test Model 
# predict test model
predict_test <- predict(lm_model, newdata = test_data)
# find RMSE
rmse_test <- sqrt(mean((test_data$medv - predict_test)**2)) 

# print result train-test
cat("RMSE Train data : ", rmse_train,
    "\nRMSE Test data : ", rmse_test)
