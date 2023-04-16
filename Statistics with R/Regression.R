library(readxl)
setwd(choose.dir())

# --------- simple linear regression ---------
data = read_excel("regression-data.xls")
model = lm(data$price~data$meter)
summary(model)
# --------- multiple regression --------------
attach(mtcars)
model <- mpg~disp + hp + drat + wt
fit <- lm(model, mtcars)
summary(fit)
plot(fit)

# ----------- logistic regression -----------
library(gee)
data("warpbreaks")
contrasts(warpbreaks$wool)
m = glm(warpbreaks$wool~warpbreaks$breaks, family = "binomial", data = warpbreaks)
predict_reg = predict(m,warpbreaks, type = "response")
predict_reg <- ifelse(predict_reg >0.5, "A", "B")
warpbreaks$wool
summary(m)
# 
library(tidyverse)
library(caret)
theme_set(theme_bw())
# Load the data and remove NAs'

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)
# Split the data into training and test set
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Fit the model
model <- glm( diabetes ~., data = train.data, family = binomial)
# Summarize the model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == test.data$diabetes)
