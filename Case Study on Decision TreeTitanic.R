d = getwd()
d
setwd("C:/Users/Hp/Desktop/AnalyticsWithR/R")
Titanic = read.csv("data/titanic.csv")
View(Titanic)

Titanic[Titanic == "?"] <- NA
View(Titanic)
sum(is.na(Titanic))
summary(Titanic)

str(Titanic)
library("dplyr")
titanic_data <- Titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()
View(titanic_data)
str(titanic_data)
summary(titanic_data)

titanic_data$age <- as.numeric(as.character(titanic_data$age))
titanic_data$fare <- as.numeric(as.character(titanic_data$fare))
str(titanic_data)

library("rpart")
library("rpart.plot")


data = sample(1:nrow(titanic_data), 0.7*nrow(titanic_data), replace = F)
train_data = titanic_data[data,]
test_data = titanic_data[-data,]
View(train_data)
View(test_data)

ctree = rpart(survived ~., data = train_data, method = "class")
rpart.plot(ctree)

test_data$Prediction_survived = predict(ctree, test_data, type = 'class')
View(test_data)


table_mat<-table(test_data$survived,test_data$Prediction_survived)
table_mat

# Accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat) 
accuracy_Test