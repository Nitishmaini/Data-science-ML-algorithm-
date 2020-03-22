rm(list = ls()) # To clear the environment
install.packages("caret")#For confusion matrix
library(caret)
#setwd("C:\\Users\\BROTHERS\\Desktop\\Nptel\\Case study crash Test")
crash_train<-read.csv("C:\\Users\\BROTHERS\\Desktop\\Nptel\\Case study car crash\\crashTest_1.csv",row.names = 1)
crash_test<-read.csv("C:\\Users\\BROTHERS\\Desktop\\Nptel\\Case study car crash\\crashTest_1_TEST.csv",row.names = 1)
View(crash_test)
View(crash_train)# to view the data
str(crash_train) # to see the structure tof the data
summary(crash_train) # to see the summary of the data
logisticfit<-glm(formula = crash_train$CarType~., data = crash_train,family = 'binomial') # generalised linear model used for logestic regression
logisticfit
summary(logisticfit)
logistrain<-predict(logisticfit,type = 'response')
plot(logistrain)
tapply(logistrain,crash_train$CarType,mean)
logispred<-predict(logisticfit,newdata=crash_test,type ='response')
logispred
plot(logispred)
crash_test[logispred<=0.5,"logispred"]<-"Hatchback"
crash_test[logispred>0.5,"logispred"]<-"SUV"
install.packages("e1071")
confusionMatrix(table(crash_test[,7],crash_test[,6]),positive = "Hatchback")

