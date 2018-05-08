
############################# DATA PRE-PROCESSING ############################################

#DATA PRE-PROCESSING IS THE MAJOR INITIAL CHALLENGE IN DATA SCIENCE PROJECT.
#DATA PRE-PRECESSING  CONSUMES AROUND 80% OF TIME IN A PROJECT.
#ONE OF THE INITIAL AND MAJOR SKILL NEEDED IN ANY DATA SCIENCE PROJECT IS DATA PRE-PROCESSING.
#DATA PRE-PROCESSING IS ALWAYS DEPENDS ON DATA SCIENTIST INTUTION.
#ALWAYS! REMEMBER THE GARBAGE IN AND GARBAGE OUT CONCEPT.
#LET'S GET STARTED.


############################# LOAD THE DATASET ##############################################

house_us = read.csv(file.choose())

dim(house_us)

# THE FIRST STEP FOR DATA PRE-PROCESSING IS TO UNDERSTAND THE DATA.
# YOU SHOULD HAVE A KNOWLEDGE ON CLIENT BUSINESS AND THEIR DATA.
# WE DO DATA PRE-PROCESSING FOR ALGORITHM IN ORDER TO GET ACCURACY OUT OF IT.
# SOME TIMES YOU DON'T SOME ATTRIBUTES,SO FEEL FREE TO ASK THE CLIENT ABOUT IT.
# ACCEPT THE FACT THAT,YOUR CLIENT KNOWS BETTER ABOUT THEIR BUSSINESS THAN YOU.
# ELSE YOU ENDED UP WITH EGOISTIC ISSUES.
# WELL! ENOUGH OF BUSSINESS EHITCS RULES.
# LET'S STICK TO THE DATA SCIENCE JOB.

# SOME OF THE IMPORTANT STEPS WE NEED TAKE CARE ABOUT IN SEQUENCE WISE OR YOU CAN SAY SOME
# OF THE THUMB RULE FOR DATA ANALYSING AND DATA PRE-PROCESSING ARE VERY IMP TO KNOW.
# LET'S GET TO KNOW THEM.

#1 ALWAYS SPEND TREMENDOUS TIME IN DATA UNDERSTANDING AND DATA ATTRIBUTES,
#  THAT WILL LEAD YOU IN HAPPY ENDING.

#2. TRY TO KNOW MORE ABOUT THEIR ATTRIBUTES.WHEN I SAY ATTRIBUTES, IT INCLUDES ALL KIND 
#   OF INFORMATION.

#3  CHECK DATA TYPES,DISTRIBUTION,MISSING VALUES,DUPLICATE ROWS,DUPLICATE COLMNS,
#   SCALE OF VARIABLE,SUMMARY,RANGE,OUTLIERS,DIMENSIONS,NOISE,INF,NEGATIVE VALUES.

#4. OUTLIERS NOT ALWAYS A BAD THING,IT MIGHT BE A GOOD THING,IF IT IS GENUINE DATA.

#5. VARIABLE'S DATA SHOULD BE ON SAME SCALE OR SAME UNIT.

#6. REMOVE THE NON-INFORMATIVE COLUMNS AND ALSO DON'T INCLUDE MULTICOLLINEAR ATTRIBUTES.

#7. SCALING(MIN-MAX),STANDARDIZING(Z-SCORE),BINNING(GROUPING IN DIFFERENT BUCKETS).

#8. REMOVING OUTLIERS IS NOT RECOMMENDED.

#9. IMPUTATION SHOULD BE APPROPRIATE AND ALWAYS BASED ON BUSINESS UNDERSTANDING.

#10 HANDLE MULTI-CLASS COLUMN.

#11 EXPLORATORY DATA ANALYSIS SHOULD BE DONE FOR A PURPOSE,NOT FOR A RANDOM PLOT.
#   VISUALIZATION IS A MAIN TECHNIQUE TO DRAW INTUTION AND CONCULUDE WITH SOME UNDERSTANDING.

#12 WE SHOULD KNOW HOW TO EXTRACT INFORMATION FROM DATA.
#   DATES AND ZIP CODES ARE NOT ALWAYS AVOIDABLE,WE SHOULD KNOW HOW TO EXTRACT,
#   INFORMATION FROM THOSE KIND OF VARIABLES.

#13 REAL TIME DATA ARE NOISY AND DIRTY,SOME TIMES YOU MAY ENCOUNTER SOME SITUATIONS LIKE,
#   A VARIABLE CONTAINS MULTI DATA TYPES. :-P

#14 SOMETIMES YOU MAY NOTICE THAT AFTER TRANSFORMATION,YOUR OUTLIERS DECREASES.
#   IF THEN ALSO YOU DATA HAVE LOTS OF OUTLIERS THEN YOU CAN USE CAPPING.(AVOID REMOVING OUTLIERS)

#15 LAST BUT NOT THE LEAST,INFERENCE IS THE KEY TO SOLVE PROBLEMS IN DATA SCIENCE.

############################### LET'S CHECK OUR DATA ######################################

library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)


summary(house_us)

str(house_us)

###### CHECK OUT "ID" COLUMN ####


# table(house_us$id) @ (not recommended)

sum(table(house_us$id))

# SINCE ALL ROWS IN "ID" COLUMNS CONTAINS UNIQUE VALUE,SO WE SHOULD'NT BE USING 
# THIS COLM,BECAUSE THIS COLUMN IS NOT GIVING ANY INFORMATION.


################################ DROPING ID COLM #########################################

house_us$id = NULL


############################### CHECK DATE COLUMN ########################################

## DATE COLUMN CONTAINS BIZZARE FORMAT LIKE "20151214T000000" WE NEED TO SUBSTRING LAST PART.

?substr

house_us$date = substr(house_us$date,1,4)


#### CHECK NA'S IN DATASET ###

any(is.na(house_us)) # FALSE

sum(is.na(house_us)) # 0

sum(!complete.cases(house_us))

### since there is no missing values,so no need to any kind of imputation ###

### i have noticed somethin,colm "Long" has negative values ###
### i don't know why this colm has negative values #### let's see



range(house_us$long)
summary(house_us$long)
sum(table(house_us$long))



### "long" colm has duplicate values ranging -122 to -122.5 ####
### dropping "long" colm ###

house_us$long=NULL


# Correlation plot of different features and price


house_us$date = as.numeric(house_us$date)

cr <- cor(house_us)


corrplot(cr, type="full", method = "circle", main="Correlation")

corrplot(cr, type="full", method = "number", main="Correlation")


# Print Correlation coefficients

print(round(cr, digits=2))


# Picking up some of the features that are highly correlated with price
# Correlation between price and sqft_living is (0.7)
# Correlation between price and grade is (0.67)
# Correlation between price and sqft_above is (0.61)
# Correlation between price and sqft_living15 is (0.59)
# Correlation between price and bathrooms is (0.53)

# Now, let us evaluate the correlation of the above features with each others for multicolinearity 
# Correlation between sqft_living and sqft_above is (0.88)
# Correlation between sqft_living and sqft_living15 is (0.76)
# Correlation between sqft_living and grade is (0.76)
# Correlation between sqft living and bathrooms is (0.75)

############################## EXPLORATORY DATA ANALYSIS ########################


par(mfrow=c(3,6))
for (i in 1:18) {
  hist(house_us[,i],main = names(house_us[i]),col="steelblue")
}


### most of the variables are right skewed,since we are performing linear regression on price
### linear regression assumption is,all variable should be normally distributed ###
### so we need to perform normalization ####


# Plotting price vs all other features.

par(mfrow=c(3,6))
for(i in 3:18){
  plot(house_us[,i], house_us$price, main=names(house_us[i]), ylab=names(house_us$price), xlab="", col='steelblue')
}

cor(house_us$date,house_us$price)


# Seems waterfront, floors, view, condition, grade, yr_built, yr_renovated, zipcode, lat,can also be given a thought from categorical perspective. 
# Running a quick boxplot on these to see the price quantiles

par(mfrow=c(1,1))
for(i in c(1,3,6,7,8,9,10,15)){
  boxplot(house_us[,2]~house_us[,i], xlab='comparision', main=names(house_us[i]), col=c("blue","red"))
}


# bedrooms:   median prices across bedrooms have less variation. Notice the extreme.
# bathrooms:  median pricee seems to be increasing but there are categorical lows also.
# floors:     shows categorical variation in median price
# waterfront: median price is higher with waterfront
# view:       little median price variation
# condition:  has some effect on median price
# grade:      median prices show linear trend
# zipcode:    some of the zipcodes command higher prices

# Now back to bedroom extremes that we noticed.
# bedrooms seem to have some extreme values. Checking a quick data distribution.



par(mfrow = c(1,2))
hist(house_us$bedrooms, breaks = 30, xlab = "", col = "lightsteelblue",  main = "Bedrooms")
plot(density(house_us$bedrooms), xlab="", col = "steelblue", main="Bedrooms")


# Checking bedroom extremes as distibution is very skewed
# there is two record of bedroom having 11 & 33 seems odd

print(subset(house_us, house_us$bedrooms > 10))


# There is one record each for bedroom 11 and 33. 
# 33 bedroom house has 1.75 bathrooms, sqft_living is also very less. Seems little odd.
# I suspect a data entry error here. Lets calculate the mean sqft_living of 3 bedroom house.

bed_room <- subset(house_us,house_us$bedrooms == 3)

print(tapply(bed_room$sqft_living, bed_room$bedrooms, mean)) #sqft_living

# Mean sqft_living of 3 bedroom house is approx 1805 whereas for 33 bedrooms it is 1620.
# since i will remove,those two records which i beleive data entry error.

new_data = house_us[house_us$bedrooms<=10,]
dim(new_data)


############################# checking distribution of bathrooms #########################
### density plot
### scatter plot between pice and bathroom



par(mfrow = c(1,3))
hist(new_data$bathrooms, breaks = 20, xlab = "", col = "lightsteelblue",  main = "Bathrooms")
plot(density(new_data$bedrooms), xlab="", col = "steelblue", main="Bathrooms")
scatter.smooth(new_data$bathrooms, new_data$price, col="steelblue", xlab="", ylab="Price",main="Bathrooms",lpars=list(col="red", lwd=3))



# Distribution looks skewed.
# Lets see what is the ratio of bedrooms >= 6

print(prop.table(table(new_data$bathrooms >= 6)))


# Only 0.0007% data has 6 bathrooms or more with few readings in each category. That too, with 
# spread out price range. For example, there are two reading with 8 bedrooms; one with very low price 
# and another with very high. This is the reason why we ware observing a higher median price with 
# increase in bedrooms.

# For numeric, will drop highly correlated data amongst features
# Will turn categorical data into factors. In R regression models, turnig them to factors has similar effect as dummy vars.



############################### converting data types ##################################

# converting to factors

new_data$zipcode <- as.factor(new_data$zipcode)
new_data$grade <- as.factor(new_data$grade)
new_data$waterfront <- as.factor(new_data$waterfront)
new_data$floors <- as.factor(new_data$floors)
new_data$bedrooms <- as.factor(new_data$bedrooms)
new_data$yr_built<- as.numeric(new_data$yr_built)


## i am thinking that, we should make feature engg.
### suppose we have an attribute called "yr_built"
### if we substract "date" - "yr_built" we can  find age of house ###

### add colmn

new_data$house_age = new_data$date - new_data$yr_built

### linear regression assumes that all colms are normally distributed ###
### so we need to perform normalization ####

hist(log(new_data$price)) ### close to normal distribution ###

new_data$price_log = log(new_data$price)

hist(log(new_data$sqft_above))## close to normal distribution ###

new_data$sqft_above_log = log(new_data$sqft_above)

new_data$sqft_above=NULL

hist(log(new_data$sqft_living))

new_data$sqft_living_log = log(new_data$sqft_living)

new_data$sqft_living=NULL

hist(log(new_data$sqft_living15)) #not bad#

new_data$sqft_living15_log = log(new_data$sqft_living15)

new_data$sqft_living15=NULL

hist(log(new_data$sqft_lot))

new_data$sqft_lot_log = log(new_data$sqft_lot)

new_data$sqft_lot = NULL

hist(log(new_data$sqft_basement))#not bad #
hist(new_data$sqft_basement)

new_data$sqft_basement_log = log(new_data$sqft_basement)
new_data$sqft_basement=NULL

hist(new_data$date)
hist(log(new_data$date)) ### no use ##

################################ dropping date colm #######################################

new_data$date=NULL

cor(new_data$yr_built,new_data$price)

################################ splitting data ##########################################



library(caTools)
set.seed(123)
split= sample.split(Y = new_data$price_log,SplitRatio = 0.7)
training_data = subset(new_data,split==TRUE)
tEST_data = subset(new_data,split==FALSE)


################################# MODELLING ############################################

model1 = lm(price_log~sqft_living_log + grade + waterfront + view + condition +zipcode+bathrooms+bedrooms+house_age,data = training_data)

summary(model1)

###################### since bedrooms are less significant,removing bedrooms ###################

model2 = lm(price_log~sqft_living_log + grade + waterfront + view + condition +zipcode+bathrooms+house_age,data = training_data)

summary(model2)

plot(model2)


############################# remove grade variable #####################################

model3 = lm(price_log~sqft_living_log + waterfront + view + condition +zipcode+bathrooms,data = training_data)

summary(model3)

### since adj.r2 is down by 3%, we should keep this,stick to model2 ####################



############################## PREDICTION ##############################################

PREDS = predict(model2,newdata = tEST_data)

head(PREDS,10)

PREDICTION_DATAFRAME = data.frame(RECORDED = tEST_data$price_log,PREDICTION = PREDS)



############################### plotting ################################################

plot(PREDICTION_DATAFRAME$RECORDED,type = "l",lty=1.8,col="green")
lines(PREDICTION_DATAFRAME$PREDICTION,type = "l",col="blue")

############################## PREDICTION ON ACTUAL PRICE #################################

PREDICTION_DATAFRAME$ACTUAL_PREDICTION = PREDICTION_DATAFRAME$PREDICTION*2.718

PREDICTION_DATAFRAME$ACTUAL_PRICE = tEST_data$price

PREDICTION_DATAFRAME$EXP_PREDICTION = exp(PREDICTION_DATAFRAME$PREDICTION)

############################# PLOT PREDICTION ###########################################



plot(PREDICTION_DATAFRAME$ACTUAL_PRICE,type = "l",lty=1.8,col="green")
lines(PREDICTION_DATAFRAME$ACTUAL_PREDICTION,type = "l",col="blue")



plot(PREDICTION_DATAFRAME$ACTUAL_PRICE,type = "l",lty=1.8,col="green")
lines(PREDICTION_DATAFRAME$EXP_PREDICTION,type = "l",col="blue")


################################## RMSE ################################################


RMSE1 = sqrt(mean(PREDICTION_DATAFRAME$RECORDED - PREDICTION_DATAFRAME$PREDICTION)^2)
RMSE1


RMSE2 = sqrt(mean(PREDICTION_DATAFRAME$ACTUAL_PRICE - PREDICTION_DATAFRAME$ACTUAL_PREDICTION)^2)
RMSE2


RMSE3 = sqrt(mean(PREDICTION_DATAFRAME$ACTUAL_PRICE - PREDICTION_DATAFRAME$EXP_PREDICTION)^2)
RMSE3


######################################## *** ###################################################



####################################  MODEL ON ACTUAL DATA #####################################
































