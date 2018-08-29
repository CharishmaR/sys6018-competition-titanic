train = read.csv("train.csv")
train$dataset = "Train"
test = read.csv("test.csv")
test$Survived = NA
test$dataset = "Test"

total.titanic = rbind(train, test)
View(total.titanic)
nrow(total.titanic)

counts = table(train$Survived, train$Sex)
barplot(counts, xlab="sex ", ylab="s ", pch=19)

counts2 = table(train$Survived, train$Age)
barplot(counts2, xlab="age ", ylab="s ", pch=19)

table(is.na(total.titanic$Age))

age2 = median(total.titanic$Age, na.rm = TRUE)
age2

total.titanic[is.na(total.titanic$Age), "Age"] <- age2
View(total.titanic)

table(is.na(total.titanic$Age))

total.titanic$Pclass = as.factor(total.titanic$Pclass)
total.titanic$Sex = as.factor(total.titanic$Sex)
total.titanic$Embarked = as.factor(total.titanic$Embarked)

training.data = subset(total.titanic, total.titanic$dataset == "Train")
View(training.data)
testing.data = subset(total.titanic, total.titanic$dataset == "Test")
training.data$Survived = as.factor(training.data$Survived)

install.packages("randomForest")
library(randomForest)

#survivedeqn = "Survived ~ Pclass, Age, Sex, Sibsp, Parch, Embarked"
#survivedform = as.formula(survivedeqn)



try1 = randomForest(formula = training.data$Survived ~ training.data$Pclass + training.data$Sex + training.data$Age + training.data$SibSp + training.data$Parch + training.data$Embarked)
try1

try2 = randomForest(formula = training.data$Survived ~ training.data$Pclass + training.data$Sex + training.data$Age)
try2
