df <- read.csv("C:/Users/Hp/Desktop/AnalyticsWithR/R/data/HR_Data.csv")
summary(df)
str(df)
View(df)
table(df$role)/nrow(df)
dataset <- df
dataset$role1 <- ifelse(dataset$role == 'sales', 1,0)
dataset$role2 <- ifelse(dataset$role == 'technical',1,0)
View(dataset)

table(dataset$salary)/nrow(dataset)
dataset$salary1 <- ifelse(dataset$salary == 'low', 1,0)
View(dataset)

dataset <- dataset[,-c(9,10)]

dataset$left <- factor(dataset$left, levels = c(0,1))
str(dataset)

library(caret)
trainDataIndex <- createDataPartition(dataset$left, p=0.7, list = F)
trainData <- dataset[trainDataIndex, ]
testData <- dataset[-trainDataIndex, ]
table(trainData$left)

logimod <- glm(left ~ satisfaction_level + last_evaluation 
               +number_project +
                 average_montly_hours + exp_in_company
               + Work_accident + promotion_last_5years
               #+ role1
               + role2 
               + salary1 
               , family = 'binomial', data = trainData)
summary(logimod)

pred <- predict(logimod, newdata = testData, type = "response")
pred

testData$pred_left <- ifelse(pred > 0.5, 1, 0)

table_mat<-table(testData$left,testData$left)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat) 
accuracy_Test

