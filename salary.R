salary_train<-read.csv("F:/Excelr/Assignments/dataset/naive bayes/SalaryData_Train.csv")
View(salary_train)
str(salary_train)
summary(salary_train)
class(salary_train)
sum(is.na(salary_train))

salary_test<-read.csv("F:/Excelr/Assignments/dataset/naive bayes/SalaryData_Test.csv")
View(salary_test)
str(salary_test)
class(salary_test)
sum(is.na(salary_test))

library(e1071)
library(caret)
library(gmodels)

#exploratory data analysis

mean(salary_train$age)
mean(salary_train$hoursperweek)
mean(salary_train$capitalgain)
mean(salary_train$capitalloss)
mean(salary_train$educationno)


median(salary_train$age)
median(salary_train$hoursperweek)
median(salary_train$capitalgain)
median(salary_train$capitalloss)
median(salary_train$educationno)

var(salary_train$age)
var(salary_train$hoursperweek)
var(salary_train$capitalgain)
var(salary_train$capitalloss)
var(salary_train$educationno)

sd(salary_train$age)
sd(salary_train$hoursperweek)
sd(salary_train$capitalgain)
sd(salary_train$capitalloss)
sd(salary_train$educationno)

library(moments)
skewness(salary_train$age)
skewness(salary_train$hoursperweek)
skewness(salary_train$capitalgain)
skewness(salary_train$capitalloss)
skewness(salary_train$educationno)

kurtosis(salary_train$age)
kurtosis(salary_train$hoursperweek)
kurtosis(salary_train$capitalgain)
kurtosis(salary_train$capitalloss)
kurtosis(salary_train$educationno)


hist(salary_train$age)
hist(salary_train$hoursperweek)
hist(salary_train$capitalgain)
hist(salary_train$capitalloss)
hist(salary_train$educationno)

boxplot(salary_train$age)

boxplot(salary_train$hoursperweek)
boxplot(salary_train$capitalgain)
boxplot(salary_train$capitalloss)
boxplot(salary_train$educationno)




plot(salary_train$workclass,salary_train$Salary)
plot(salary_train$education,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$race,salary_train$Salary)
plot(salary_train$sex,salary_train$Salary)
plot(salary_train$native,salary_train$Salary)
plot(salary_train$age,salary_train$Salary)
plot(salary_train$capitalgain,salary_train$Salary)




#building model
salary_classifier<-naiveBayes(salary_train,salary_train$Salary)
salary_classifier$levels

salary_test_pred<-predict(salary_classifier,salary_test)
table(salary_test_pred,salary_test$Salary)

mean(salary_test_pred==salary_test$Salary)
confusionMatrix(salary_test_pred,salary_test$Salary)
