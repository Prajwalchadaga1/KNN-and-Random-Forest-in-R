##KNN
wine <-read.table("wine.csv", header=T, sep=",")
View(wine)

# Conveting the "high_quality" feature into a factor (0 or 1) to create a Random Forest
wine$high_quality = factor(wine$high_quality)
X_wine= wine[, c("density", "sulphates", "residual_sugar","pH","fixed_acidity")]
y_wine = wine[, c("high_quality")]
library(caret)

# Splitting the data into training and testing sets with a splitting ratio of 80:20 respectively
inTrain = createDataPartition(y=y_wine, p=0.8, list=FALSE)
X_train = X_wine[inTrain, ]
X_test = X_wine[-inTrain, ]
y_train = y_wine[inTrain]
y_test = y_wine[-inTrain]
library(class)

#Creating the KNN model using the training and test sets created in the previous step
wine_prediction <- knn(train=X_train, test=X_test, cl=y_train)
wine_prediction

#Determining the accuracy of the KNN model created
accuracy <- mean(wine_prediction == y_test)
accuracy

##building cross table for evaluation
library(gmodels)
CrossTable(x=y_test, y=wine_prediction)

##Specifying the number of iterations which specifies how many times the KNN algorithm is going to be run for this model
num_iterations = 100
acc_history_1 <- list(num_iterations)

#Running the number of iterations specified for the KNN to determine the average accuracy
for (i in 1:num_iterations) {
  inTrain = createDataPartition(y=y_wine, p=0.8, list=FALSE)
  X_train = X_wine[inTrain, ]
  X_test = X_wine[-inTrain, ]
  y_train = y_wine[inTrain]
  y_test = y_wine[-inTrain]
  prediction1 <- knn(train=X_train, test=X_test, cl=y_train)
  accuracy1 <- mean(prediction1 == y_test)
  acc_history_1[[i]] <- accuracy1
}
#printing out the values of accuracy for the number of iterations ran and the average accuracy of the model
for (i in 1:num_iterations) {
  print(acc_history_1[[i]])
}
sum_acc = 0
for (i in 1:num_iterations) {
  sum_acc = sum_acc + acc_history_1[[i]]
}
#Printing out the average accuracy of the KNN model
ave_acc = sum_acc/num_iterations
print(ave_acc)



##RandomForest
wine <- read.csv("wine.csv", header=T, sep=",")
View(wine_RF)
wine_final = wine[, c("density", "sulphates", "residual_sugar","pH","fixed_acidity","high_quality")]

# Conveting the "high_quality" feature into a factor (0 or 1) to create a Random Forest
wine_final$high_quality = factor(wine_final$high_quality)

population <- sample(nrow(wine_reqd), 0.8*nrow(wine_reqd))
train <- wine_final[population, ]
test <- wine_final[-population, ]

View(population)
View(train)
View(test)

#Creating a random forest with "High_quality" as the dependent variable and the other variables in the wine_final as the independent variables
library(randomForest)
model <- randomForest(high_quality~., data=train)
model

#Predicting the values of the testrf dataset using the trained random forest model
prediction <- predict(model, newdata=test)
prediction

#Determining the average value of the accuracy of the predictions made in the previous step
accuracy <- mean(prediction == test$high_quality)
accuracy

##Specifying the number of iterations which specifies how many times the random forest algorithm is going to be run for this model
num_iterations <- 100
acc_history <- list(num_iterations)
acc_history_1 <- list(num_iterations)
library(caret)
library(randomForest)

#Running the number of iterations specified for the Random forest and KNN to determine which model is better
for (i in 1:num_iterations) {
  inTrain = createDataPartition(y=wine_final$high_quality, p=0.8, list=FALSE)
  X_train = wine_final[inTrain, ]
  X_test = wine_final[-inTrain, ]
  model <- randomForest(high_quality~., data=X_train)
  prediction <- predict(model, newdata=X_test)
  prediction1 <- knn(train=X_train, test=X_test, cl=y_train)
  accuracy <- mean(prediction == X_test$high_quality)
  accuracy1 <- mean(prediction1 == X_test$high_quality)
  acc_history[[i]] <- accuracy
  acc_history_1[[i]] <- accuracy1
}

#printing out the values of accuracy for the number of iterations ran and the average accuracy of the model
for (i in 1:num_iterations) {
  print(acc_history[[i]])
}
sum_acc = 0
for (i in 1:num_iterations) {
  sum_acc = sum_acc + acc_history[[i]]
}

#Printing out the average accuracy of the Random Forest model
ave_acc = sum_acc/num_iterations
print(ave_acc)

#Performing Statistical T-tests to find out which algorithm(Knn vs Random forest) is significant
#If p-value is less than 0.05 then I can conclude that Random forest performed better than KNN for this dataset
df1 <- data.frame(matrix(unlist(acc_history), nrow=length(acc_history), byrow=T))
df1$group = 1
df1$i <- seq.int(nrow(df1))
names(df1)[1] <- "accuracy"
df2 <- data.frame(matrix(unlist(acc_history_1), nrow=length(acc_history_1), byrow=T))
df2$group = 0
df2$i <- seq.int(nrow(df2))
names(df2)[1] <- "accuracy"

df3 <- rbind(df1, df2)

#Plot to highlight the accuracy scores of KNN(Group 0) vs Random Forest(Group 1)
ggplot(data = df3, aes(x=i, y=accuracy, color=factor(group))) + xlab("round of test") +
  ylab("accuracy score") + geom_point() + labs(color="group")
t.test(accuracy~group, data=df3)