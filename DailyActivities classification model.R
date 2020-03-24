---
title: "Classification technique using SVM"
author: "Kanchan Joshi - 18008486"
date: "March 23, 2020"
output: html_document
---

library(caret)

## Dataset : Daily and Sports Activities Data Set

Each of the 19 activities is performed by eight subjects (4 female, 4 male, between the ages 20 and 30) for 5 minutes.
Total signal duration is 5 minutes for each activity of each subject.
The subjects are asked to perform the activities in their own style and were not restricted on how the activities should be performed. For this reason, there are inter-subject variations in the speeds and amplitudes of some activities.
The activities are performed at the Bilkent University Sports Hall, in the Electrical and Electronics Engineering Building,
and in a flat outdoor area on campus. Sensor units are calibrated to acquire data at 25 Hz sampling frequency. The 5-min signals are divided into 5-sec segments so that 480(=60x8) signal segments are obtained for each activity.

The 19 activities are:
sitting (A1),
standing (A2),
lying on back and on right side (A3 and A4),
ascending and descending stairs (A5 and A6),
standing in an elevator still (A7)
and moving around in an elevator (A8),
walking in a parking lot (A9),
walking on a treadmill with a speed of 4 km/h (in flat and 15 deg inclined positions) (A1
0 and A11),
running on a treadmill with a speed of 8 km/h (A12),
exercising on a stepper (A13),
exercising on a cross trainer (A14),
cycling on an exercise bike in horizontal and vertical positions (A15 and A16),
rowing (A17),
jumping (A18),
and playing basketball (A19).


# Importing data from a flattened excel sheet  
data<-read.csv("https://raw.githubusercontent.com/kanjoshi02/classification_in_R/master/dailyActivitiesdata.csv", header = TRUE)

# checking for missing values
sum(is.na(data))

### Splitting data into training and testing datasets in the proportion of 80:20 respectively.

trainingIndex <- createDataPartition(data$activity, p =0.8, list = FALSE)
train_data<- data[trainingIndex,] #training data
test_data<- data[-trainingIndex,] #test data

## Classification Technique : Implimentation of SVM algorithm

The objective of this experiment is to train the model to identify the activity based on datapoints being recorded by varius sensors. 

Support Vector Machine is a supervised algorithm, commonly used for classification and regression. It uses a technique called the kernel trick to transform your data and then based on these transformations it finds an optimal boundary between the possible outputs.


# building a training model - SVM (Polynomial Kernal) #
#######################################################
model<-train(activity ~., data = train_data,
             method = "svmPoly",
             na.action = na.omit,
             preProcess=c("scale","center"),
             trControl = trainControl(method = "none"),
             tuneGrid = data.frame(degree=1,scale=1,C=1)
)             
# apply cross-validation model with SVM

model.cv<- train(activity ~., data = train_data,
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

model.train.conf<-confusionMatrix(model.train,train_data$activity)
model.test.conf<-confusionMatrix(model.test,test_data$activity)
model.cv.conf<-confusionMatrix(model.cv,train_data$activity)


print(model.train.conf)
print(model.test.conf)
print(model.cv.conf)


## Feature Engineering

To indentify the influensive variables among all, and parameter tuning accordingly to tune the model.


Importance <- varImp(model)
print(Importance)
plot(Importance,col = "red")



