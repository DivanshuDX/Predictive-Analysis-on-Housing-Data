# Importing Libraries
library(readxl)
library(tidyverse)
library(ISLR2)
library(stargazer)
library(caret)
library(leaps)
library(MASS)

# Importing Dataset
housing_data = read_excel("D:/Predictive Analytics/Multivariate Statistics/Project 1/King county, USA housing data.xlsx")
head(housing_data)

# Removing /Checking for Null Values
house_data = na.omit(housing_data)
head(house_data)

# Summary of the dataset
summary(house_data)

# Histogram for 
attach(house_data)
hist(price)
hist(bedrooms)
hist(bathrooms)
hist(sqft_living)
hist(sqft_lot)
hist(floors)
hist(view)
hist(condition)
hist(grade)
hist(sqft_above)


# Correlation
cor(price, bedrooms)
cor(price, bathrooms)
cor(price, sqft_living)
cor(price, sqft_lot)
cor(price, floors)
cor(price, view)
cor(price, condition)
cor(price, grade)
cor(price, sqft_above)

# GGplot
house_data %>% ggplot(aes(x = bedrooms, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = bathrooms, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = floors, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = sqft_above, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = sqft_living, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = sqft_lot, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = view, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = condition, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")
house_data %>% ggplot(aes(x = grade, y = price)) + geom_point() + geom_smooth(method="lm", formula = "y ~ x + I(x^2)")


# First Model with all the Square feet independent variables
first_model = lm(price ~ sqft_living + sqft_above + sqft_lot, data = house_data)

coef(first_model)
summary(first_model)

firstmodelResids <- first_model$residuals
firstmodelFitted <- first_model$fitted.values

hist(firstmodelResids)

plot(firstmodelFitted, firstmodelResids)

qqnorm(firstmodelResids)

# Training the first model
firstCVModel <- train(
  form = price ~ sqft_living + sqft_above + sqft_lot,
  data = house_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
firstCVModel


# Second model with all the interior variables
second_model <- lm(price ~ bedrooms + bathrooms + floors + view, data = house_data)
coef(second_model)

summary(second_model)

secondmodelResids <- second_model$residuals
secondmodelFitted <- second_model$fitted.values

hist(secondmodelResids)

plot(secondmodelFitted, secondmodelResids)

qqnorm(secondmodelResids)

# Training the second model
secondCVModel <- train(
  form = price ~ bedrooms + bathrooms + floors + view,
  data = house_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
secondCVModel


# Third Model
third_model <- lm(price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot, data = house_data)
coef(third_model)

summary(third_model)

thirdmodelResids <- third_model$residuals
thirdmodelFitted <- third_model$fitted.values

hist(thirdmodelResids)
plot(thirdmodelFitted, thirdmodelResids)

qqnorm(thirdmodelResids)

# Training the Third model
thirdCVModel <- train(
  form = price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot,
  data = house_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
thirdCVModel


# Using sub-setting
subsetmodel <- regsubsets(price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot, data = house_data)
plot(subsetmodel, scale = "adjr2")


# AIC best model
AIC <- lm(price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot, data = house_data)
step <- stepAIC(AIC, trace = FALSE)
step$anova

# Initial Model:
initial_model <- lm(price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot, data = house_data)
coef(initial_model)

summary(initial_model)

initialmodelResids <- initial_model$residuals
initialmodelFitted <- initial_model$fitted.values

hist(initialmodelResids)
plot(initialmodelFitted, initialmodelResids)

qqnorm(initialmodelResids)

# Training Initial model
InitialCVModel <- train(
  form = price ~ bedrooms + bathrooms + floors + view + condition + grade + sqft_living + sqft_above + sqft_lot,
  data = house_data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
InitialCVModel

# Prediction using Initial model
pricePrediction<- data.frame(bedrooms = c(8, 9, 11, 12), bathrooms = c(6,2,7,5), floors = c(1,2,3,4), view = c(5,6,7,8), condition = c(6, 7, 8, 9), 
                             grade = c(3, 4, 5, 8), sqft_living = c(9500, 10000, 10500, 11000), sqft_above = c(9000, 9050, 9500, 10000), 
                             sqft_lot = c(70000, 80000, 90000, 100000))
predict(initial_model, pricePrediction)

