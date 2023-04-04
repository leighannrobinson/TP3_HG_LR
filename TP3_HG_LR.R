library(tidyverse)
dataset=read_csv('Housing.csv')

#Explore the data
head(dataset)
glimpse(dataset)
length(dataset)
summary(dataset)

#Find missing values
colSums(is.na(dataset))

#No missing values in this dataset


#Splitting data
library(caTools)

split=sample.split(dataset$price, SplitRatio=.8) #80% training, 20% testing

#True for training set and false for test set
training_set=subset(dataset, split=TRUE)
test_set=subset(dataset, split=FALSE)

#Fitting multiple linear regression
MLR=lm(formula=price~ ., data= training_set)
summary(MLR)
#42771.69+244.14(area)+114787.56(bedrooms)+987668.11(bathrooms)+450848(stories)
#+421272.59(mainroadyes)+300525.86(guestroomyes)+350106.90(basementyes)
#+855447.15(hotwaterheatingyes)+864958.31(airconditioningyes)
#+277107.1(parking)+651543.8(prefareayes)-46344.62(furnishingstatus-semifurnished)
#-411234.39(furnishingstatusunfurnished)


#Mean square error
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste('Mean Square Error:', MSE)

#R-Squared
summary(MLR)

#Testing Set Prediction
y_pred=predict(MLR, newdata=test_set)
data=data.frame(test_set$price, y_pred)
head(data)


