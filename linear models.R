install.packages('MASS')
install.packages('pROC')
library(mlbench)
library(MASS)
library(pROC)
library(tidyverse)

s21 <- s21 %>% mutate(home_win = if_else(winner == home, "home", "away"))

summary(s21)

#remove unneeded variables
newdata <- s21 %>% select(-1, -2, -18, -19, -20, -21, -23, -24)

newdata2 <- newdata %>% select(-1, -2, -16, -36)

newdata2 <- newdata2 %>% mutate(h_win = if_else(home_win == "home", 1, 0))

newdata2 <- newdata2 %>% select(-34, -35)

newdata2 <- newdata2 %>% select(-1, -2, -3) #don't need result of game
newdata2 <- newdata2 %>% select(-25) #pred total duplicates

#select training and test set
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(newdata2), replace=TRUE, prob=c(0.7,0.3))
train  <- newdata2[sample, ]
test   <- newdata2[!sample, ]


logit_1 <- glm(h_win~., family = binomial,data = train)

summary(logit_1)

cor(newdata2)


logit_2 <- stepAIC(logit_1)

summary(logit_2)

summary(logit_2$fitted.values)

hist(logit_2$fitted.values,main = " Histogram ",
     xlab = "Probability of Home Win", 
     col = 'light green')

train$Predict <- ifelse(logit_2$fitted.values >0.5,"home","away")

logit_1$aic
logit_2$aic

ctab_train <- table(train$h_win,train$Predict)
ctab_train

accuracy_train <- sum(diag(ctab_train))/sum(ctab_train)*100
accuracy_train

Recall <- (ctab_train[2, 2]/sum(ctab_train[2, ]))*100
Recall

Precision <- (ctab_train[2, 2]/sum(ctab_train[, 2]))*100
Precision

TNR <- (ctab_train[1, 1]/sum(ctab_train[1, ]))*100
TNR

F_Score <- (2 * Precision * Recall / (Precision + Recall))/100
F_Score


roc(h_win~logit_2$fitted.values, data = train, plot = TRUE, main = "ROC CURVE", col= "blue")


# Predicting in the test dataset
pred_prob <- predict(logit_2, test, type = "response")

test$pred_prob <- pred_prob

test$Predict <- ifelse(test$pred_prob >0.5,"home","away")

ctab_test <- table(test$h_win, test$Predict)

ctab_test

accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)*100
accuracy_test
