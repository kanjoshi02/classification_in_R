library(datasets)
library(caret)
install.packages("e1071")
library(e1071)
# Importing data : IRIS

data("iris")

# checking missing values

sum(is.na(iris))

set.seed(100)

# splitting the data into training and testing data

trainingIndex <- createDataPartition(iris$Species, p =0.8, list = FALSE)
train_data<- iris[trainingIndex,] #training data
test_data<- iris[-trainingIndex,] #test data

# building a training model - SVM (Polynomial Kernal) #
#######################################################

model<-train(Species ~., data = train_data,
             method = "svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl = trainControl(method = "none"),
             tuneGrid = data.frame(degree=1,scale=1,C=1)
)             
# apply cross-validation model with SVM

model.cv<- train(Species ~., data = train_data,
                 method = "svmPoly",
                 na.action = na.omit,
                 preProcess=c("scale","center"),
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# apply model for prediction

model.train<- predict(model,train_data)
model.test<- predict(model, test_data)
model.cv <- predict(model.cv,train_data)

# creating confusin matrix for checking Model Performance

model.train.conf<-confusionMatrix(model.train,train_data$Species)
model.test.conf<-confusionMatrix(model.test,test_data$Species)
model.cv.conf<-confusionMatrix(model.cv,train_data$Species)


print(model.train.conf)
print(model.test.conf)
print(model.cv.conf)


# feature importance

Importance <- varImp(model)
plot(Importance,col = "red")
