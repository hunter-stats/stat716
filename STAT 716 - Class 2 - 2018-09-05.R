library(ISLR)
library(dplyr)
library(ggplot2)
library(splines)
library(purrr)

# Train/Test MSE ----
# Split data intro training and test data
set.seed(4)
train_sample <- sample(1:nrow(Wage), size = 1500)
wage_train <- Wage[train_sample,]
wage_test <- Wage[-train_sample,]

# Create a range of flexability
degrees_test <- 1:50

# Create a model for each value
models <- lapply(degrees_test, function(i){
  lm(wage ~ns(age, df = i), data = wage_train)
})

# Calculate the Train MSE
train_mse <- sapply(models, function(model){
  yhat_i <- predict(model, wage_train)
  square_error <- (yhat_i - wage_train$wage)^2
  mean(square_error)
})

# Calculate the Test MSE
test_mse <- sapply(models, function(model){
  yhat_0 <- predict(model, wage_test)
  square_error <- (yhat_0 - wage_test$wage)^2
  mean(square_error)
})

min_df <- which.min(test_mse)
# Graph results - this is simiar to figure 2.9 on page 31
data.frame(degrees_test, train_mse, test_mse) %>% 
  # the next line pivots the data into a 'tidy' form for easy graphing
  # the :: is used to access a specific function without calling 'library(tidyr)'
  tidyr::gather("error_type", "MSE", train_mse,test_mse) %>% 
  ggplot(aes(x = degrees_test, y = MSE, color = error_type)) + 
  geom_vline(xintercept = degrees_test[min_df], linetype = "longdash", alpha = 0.5) +
  geom_line()

data.frame(degrees_test, train_mse, test_mse) %>% 
  View()

# Some other practical considerations
# 1. We had to set the seed, a different seed will give different results - we will use bootstrapping to deal with this
# 2. How big do you make your train/test dataset - we will go over a suggestion for this in the future

# Classication Example ----

ISLR::Credit %>% ggplot(aes(x))

ISLR::Default %>% 
  ggplot(aes(x = balance, y = income, color = default)) +
  geom_point() +
  theme_minimal()

# R Introduction

d <- read.csv(file = "Book1.csv")

d <- read.csv(file = "Book1.csv")

# mutate, filter, group_by, summarise, select

Wage %>% 
  filter(wage >100) %>% glimpse


Wage %>% 
  group_by(education) %>% 
  summarise(mean_wage = mean(wage))

wage_2 <- Wage %>% 
  mutate(wage_sq = wage^2,
         wage_sq_l = log(wage_sq)) 


