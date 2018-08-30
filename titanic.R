#read in train dataset
train = read.csv("train.csv")
#add column dataset to 'train'
train$dataset = "Train"

#read in test dataset 
test = read.csv("test.csv")
#add columns Survived and dataset to 'test'
test$Survived = NA
test$dataset = "Test"

#combine the 2 datasets
total.titanic = rbind(train, test)
View(total.titanic)
nrow(total.titanic)

#scatter plot for survived over sex
counts = table(train$Survived, train$Sex)
barplot(counts, xlab="sex ", ylab="s ", pch=19)

#create bar plot for survived over age
counts2 = table(train$Survived, train$Age)
barplot(counts2, xlab="age ", ylab="s ", pch=19)

#find the number of cells that have NA
table(is.na(total.titanic$Age))

#calculate the median of age
age2 = median(total.titanic$Age, na.rm = TRUE)
age2

#calculate the mean of age
age3 = mean(total.titanic$Age, na.rm = TRUE)
age3

#calculate mean of fare
fare2 = mean(total.titanic$Fare, na.rm = TRUE)

#make all the NA values under age equal the median
total.titanic[is.na(total.titanic$Age), "Age"] <- age2
View(total.titanic)

total.titanic[is.na(total.titanic$Fare), "Fare"] <- fare2

table(is.na(total.titanic$Age))
table(is.na(total.titanic$Fare))

#convert other columns in the dataset to categorical  
total.titanic$Pclass = as.factor(total.titanic$Pclass)
total.titanic$Sex = as.factor(total.titanic$Sex)
total.titanic$Embarked = as.factor(total.titanic$Embarked)

#split the dataset into 'training.data' and 'testing.data'
training.data = subset(total.titanic, total.titanic$dataset == "Train")
View(training.data)
testing.data = subset(total.titanic, total.titanic$dataset == "Test")
training.data$Survived = as.factor(training.data$Survived)

#install and load random forest
install.packages("randomForest")
library(randomForest)

#perform random forest prediction
survivedeqn = "Survived ~ Pclass + Age + Sex + SibSp + Parch + Embarked"
survivedform = as.formula(survivedeqn)

rf1 = randomForest(formula = survivedform, data = training.data, ntree = 500, mtry = 3)

survivedfeatures = "Pclass + Age + Sex + SibSp + Parch + Embarked"
predictions = predict(rf1, newdata = testing.data)
predictions

#create dataframe of results
datarframe1 = as.data.frame(testing.data$PassengerId)
datarframe1$Survived = predictions  
View(datarframe1)
names(datarframe1) = c("PassengerID", "Survived")
  
#write to csv
write.csv(datarframe1, file = "cr2stkaggle.csv", row.names = FALSE)




survivedeqn2 = "Survived ~ Pclass + Age + Sex"
survivedform2 = as.formula(survivedeqn2)

rf2 = randomForest(formula = survivedform2, data = training.data, ntree = 500, mtry = 3)

survivedfeatures = "Pclass + Age + Sex"
predictions2 = predict(rf2, newdata = testing.data)
predictions2

datarframe2 = as.data.frame(testing.data$PassengerId)
datarframe2$Survived = predictions2  
View(datarframe2)
names(datarframe2) = c("PassengerID", "Survived")


write.csv(datarframe2, file = "cr2stkaggle2.csv", row.names = FALSE)


total.titanic[is.na(total.titanic$Age), "Age"] <- age3
View(total.titanic)

training.data = subset(total.titanic, total.titanic$dataset == "Train")
View(training.data)
testing.data = subset(total.titanic, total.titanic$dataset == "Test")
training.data$Survived = as.factor(training.data$Survived)

survivedeqn3 = "Survived ~ Pclass + Age + Sex + SibSp + Parch + Embarked"
survivedform3 = as.formula(survivedeqn3)

rf3 = randomForest(formula = survivedform3, data = training.data, ntree = 500, mtry = 3)

survivedfeatures = "Pclass + Age + Sex + SibSp + Parch + Embarked"
predictions = predict(rf3, newdata = testing.data)
predictions

datarframe3 = as.data.frame(testing.data$PassengerId)
datarframe3$Survived = predictions  
View(datarframe3)
names(datarframe3) = c("PassengerID", "Survived")


write.csv(datarframe3, file = "cr2stkaggle3.csv", row.names = FALSE)

View(testing.data)



#perform random forest prediction
survivedeqn4 = "Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"
survivedform4 = as.formula(survivedeqn4)

rf4 = randomForest(formula = survivedform4, data = training.data, ntree = 500, mtry = 3)

survivedfeatures4 = "Pclass + Age + Sex + SibSp + Parch + Fare + Embarked"
predictions4 = predict(rf4, newdata = testing.data)
predictions4

#create dataframe of results
datarframe4 = as.data.frame(testing.data$PassengerId)
datarframe4$Survived = predictions4  
View(datarframe4)
names(datarframe4) = c("PassengerID", "Survived")

#write to csv
write.csv(datarframe4, file = "cr2stkaggle4.csv", row.names = FALSE)
