
### CREATING MODEL FOR BOSTON ####

library(MASS)

library(dplyr)

library(ggplot2)

library(caTools)

library(car)

library(corrplot)

data("Boston")

library(psych)


pairs.panels(Boston[c(1:5)])

glimpse(Boston)

pairs(Boston)

###### target variable #################

###### Y = medv #######################

hist(sqrt(Boston$medv))

summary(sqrt(Boston$medv))### close to normal distribution.

###################################################################################################


#### add variable called medv_sqrt ########


Boston$medv_log = sqrt(Boston$medv)



#######################################################################################################


#### check correlation ####

cr = cor(Boston)

corrplot(cr,method = "number")



######################################################################################################


####################### split dataset in to training and test set #################################################

set.seed(2)

split = sample.split(Y = Boston$medv_log,SplitRatio = 0.7)

training_data = subset(Boston,split == "TRUE")

test_data = subset(Boston,split == "FALSE")



######################################################################################################


#### building model in train data set #####


MODEL_1 = lm(medv_log ~ .-medv,data = training_data)

summary(MODEL_1)



#### deleting indus & age ####


MODEL_2 = lm(medv_log ~ .-medv-indus-age,data = training_data)

summary(MODEL_2)

plot(MODEL_2)

#### hence rad and tax are collinear,we need to remove one ###


cor(training_data$rad,training_data$tax) ### 0.9062339 ###


###### model_3 ######


MODEL_3 = lm(medv_log ~ .-medv-age-indus-tax,data = training_data)

summary(MODEL_3)

plot(MODEL_3)


hist(MODEL_3$residuals)



#### prediction on training_test ####

preds = predict(MODEL_3,training_data)

training_data$prediction = preds**2




#### to compare prediction value and actual value ####

plot(training_data$medv_log,type = "l",lty=1.8,col="green")

lines(preds,type = "l",col="blue")



#####################################################################################################



plot(training_data$medv,type = "l",lty=1.8,col="green")

lines(training_data$prediction,type = "l",col="blue")


#####################################################################################################


predict_test = predict(MODEL_3,test_data)


test_data$predict_value = predict_test**2



###############################################################################################

###### comparision of actual value and predicted value #####


plot(test_data$medv,type = "l",lty=1.8,col="green")

lines(test_data$predict_value,type = "l",col="blue")


###################################################################################################

### RMSE ON TRAIN DATA ### 


RMSE_TRAIN  = sqrt(mean((training_data$medv - preds**2)^2))


####  RMSE ####

RMSE  = sqrt(mean((test_data$medv - predict_test**2)^2))


RMSE_TRAIN
##################################################################################################


diff =   select(test_data,medv,predict_value)



################################################################################################


diff$error = diff%>%mutate(medv - predict_value)


#####################################################################################################


View(diff)

library(lmtest)

bptest(MODEL_3)

### p value = 6.432

pf(22.59,2,12)

























































