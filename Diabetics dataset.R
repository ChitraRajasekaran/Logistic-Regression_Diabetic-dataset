library(caTools) #split function resides in this library
split<-sample.split(pima,SplitRatio = 0.8) # splitting the data set into 80:20 ratio

#split the data set into training and testing dataset
training<- subset(pima, split == "TRUE")
testing<- subset(pima, split == "FALSE")

#create the model using training datset
model <- glm(type~., training,family = "binomial")
summary(model)

#note in the output of the model summary we have *(astricks) at the end.
#Three(***) denotes that our model is 99.9% confident that our specific independent variable or value is significant.
#Two(**) denotes that our model is 99% confident that our specific independent variable or value is significant.
#one(*) denotes that our model is 95% confident that our specific independent variable or value is significant.
#. denotes that our model is 90% confindet that our specific independent variable or value is significant.

#Basically we are noticing this significance to optimize our model but beware that if a variable is not significant we cannot really remove the variable directly from the model.
#Null deviance : deviance that we get from the actual value of the dataset. It says that my model is 'x' units deviant when it is null.Null means when we are using only the intercept and not any other variable.
#Residual deviance: when we include our independant variables in our model we get the residual deviance.We are making the accuracy of our model more correct by including the independant variable.
#AIC: This value should be as minimum as possible. This is helpful when we remove unnecessary independant variable from the dataset.

#Try removing the less significant variables and check for the deviance and AIC and keep the variables accordingly.

model <- glm(type~.-skin, training,family = "binomial") #skin variable is insignificant
summary(model)

res<-predict(model,testing,type = "response")
res # probabilities of being diabetic

#create confusion matrix for training data set to check the accuracy of the model
(table(ActualVlaue = testing$type, PredictedValue = res>0.5))
#accuracy
(98+38)/(98+15+20+38)

#It is dangerous to have high false positive numbers. how can we be sure about threshold and what if we change our threshold and the accuracy increases?
#one method is trial and error but R has a method called ROC curve which is used to calcualte threshold in our model

#we will use training dataset and set the threshold inorder to test it in the testing dataset
res<-predict(model,training,type = "response")

#ROC Curve commands on training dataset to identify the threshold and that threshold can be used for testing dataset prediction
library(ROCR)
ROCRPred = prediction(res,training$type)
ROCRPref <- performance(ROCRPred,"tpr","fpr")

plot(ROCRPref, colorize = TRUE,print.cutoffs.at = seq(0.1,by = 0.1))

# We can test the same for our testing data set as well

res <- predict(model,testing,type = "response")
(table(ActualVlaue = testing$type, PredictedValue = res>0.2)) #I chose 0.2 based on the ROC curve

(64+54)/(64+54+49+4)
