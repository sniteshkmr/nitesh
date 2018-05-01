
############################# DATA PRE-PROCESSING ############################################

#DATA PRE-PROCESSING IS THE MAJOR INITIAL CHALLENGE IN DATA SCIENCE PROJECT.
#DATA PRE-PRECESSING  CONSUMES AROUND 80% OF TIME IN A PROJECT.
#ONE OF THE INITIAL AND MAJOR SKILL NEEDED IN ANY DATA SCIENCE PROJECT IS DATA PRE-PROCESSING.
#DATA PRE-PROCESSING IS ALWAYS DEPENDS ON DATA SCIENTIST INTUTION.
#ALWAYS! REMEMBER THE GARBAGE IN AND GARBAGE OUT CONCEPT.
#LET'S GET STARTED.


############################# LOAD THE DATASET ##############################################

house_us = read.csv(file.choose())

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

#15 LAST BUT NOT THE LEAST,INFERENCE IS ALWAYS THE KEY SOLVE PROBLEMS IN DATA SCIENCE.


############################### LET'S CHECK OUR DATA ######################################

library(ggplot2)
library(dplyr)
library(caret)
library(corrplot)

dim(house_us)

summary(house_us)

str(house_us)

head(house_us,5)

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

house_us$date = substr(house_us$date,1,8)

############################# manupulating dates ###########################################

dates = house_us$date

year = substr(dates,1,4)
head(year)

month = substr(dates,5,6)
head(month)

day = substr(dates,7,8)
head(day)

############################# check attributes ###########################################

colnames(house_us)

############################ add month,day,year colm to data set ##########################

house_us$year = year
house_us$month = month
house_us$day = day

################################load data manupulation package ###########################

library(lubridate)
library(tidyr)

date1 = unite(house_us,date1,day,month,year,sep = "-")
head(date1)

dates_1 = date1$date1

head(dates_1)

house_us$date = dates_1

### since,date colm is not going to add information because those dates are dummy dates###
### we should remove this colm ###

house_us = subset(house_us,select = -c(date,year,month,day))

###################### now we have 19 colm and 21613 rows #######################################################

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
unique(house_us$long)

### "long" colm has duplicate values ranging -122 to -122.5 ####
### dropping "long" colm ###

house_us$long=NULL

# Correlation plot of different features and price

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

############################## CHECK TRANSFORMATION OF VARIABLES ##########################

par(mfrow=c(3,6))
for (i in 1:18) {
  hist(house_us[,i],main = names(house_us[i]),col="steelblue")
}

### most of the variables are right skewed,since we are performing linear regression on price
### linear regression assumption is,all variable should be normally distributed ###
### so we need to perform normalization ####

hist(log(house_us$price)) ### close to normal distribution ###

house_us$price_log = log(house_us$price)

hist(log(house_us$sqft_above))## close to normal distribution ###

house_us$sqft_above_log = log(house_us$sqft_above)

#house_us$sqft_above=NULL

hist(log(house_us$sqft_living))

house_us$sqft_living_log = log(house_us$sqft_living)

#house_us$sqft_living=NULL

hist(log(house_us$sqft_living15)) #not bad#

house_us$sqft_living15_log = log(house_us$sqft_living15)

#house_us$sqft_living15=NULL

hist(log(house_us$sqft_lot))

house_us$sqft_lot_log = log(house_us$sqft_lot)

#house_us$sqft_lot = NULL

hist(log(house_us$sqft_basement))#not bad #
hist(house_us$sqft_basement)

house_us$sqft_basement_log = log(house_us$sqft_basement)
#house_us$sqft_basement=NULL

# Plotting price vs all other features.

par(mfrow=c(3,6))
for(i in 2:18){
  plot(house_us[,i], house_us$price, main=names(house_us[i]), ylab=names(house_us$price), xlab="", col='steelblue')
}


# Seems waterfront, floors, view, condition, grade, yr_built, yr_renovated, zipcode, lat,can also be given a thought from categorical perspective. 
# Running a quick boxplot on these to see the price quantiles
par(mfrow=c(1,1))
for(i in c(2,3,6,7,8,9,10,15)){
  boxplot(house_us[,1]~house_us[,i], xlab='comparision', main=names(house_us[i]), col=c("blue","red"))
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

print(tapply(bed_room$sqft_living_log, bed_room$bedrooms, mean)) #sqft_living

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
#### check structure of data ######

glimpse(new_data)

## i am thinking that, we should make feature engg.
### suppose we have an attribute called "yr_built"
### if we substract 2018 - "yr_built" we can  find age of house ###

y = rep(x = 2018,21611)
head(y)
class(y)
length(y)

### add colmn

new_data$year_2018 = y

########################## substract yr_built - yr_2018 ##############################

library(Hmisc)

new_data1 = new_data[,c(9,19)]

head(new_data1)

glimpse(new_data1)

new_data1$house_age = new_data1$year_2018 - new_data1$yr_built

############################ add colmn to newdata #######################################


#new_data$house_aged = new_data1$house_age

#################################### splitting data ###############################################

library(caTools)
set.seed(123)
split= sample.split(Y = new_data$price_log,SplitRatio = 0.7)
training_data = subset(new_data,split==TRUE)
tEST_data = subset(new_data,split==FALSE)


################################# MODELLING ############################################

model1 = lm(price_log~sqft_living_log + grade + waterfront + view + condition +zipcode+bathrooms+bedrooms,data = training_data)
summary(model1)

#### it seems bedrooms are not significant, so remove those colm ####

model2 = lm(price_log~sqft_living_log + waterfront+grade+house_aged + view + condition +zipcode,data = training_data)
summary(model2)

############################## PREDICTION ##############################################

PREDS = predict(model2,newdata = tEST_data)

actual_predictn = data.frame(obs = tEST_data$price_log,prediction = PREDS)

actual_predictn


############################### plotting ################################################

plot(actual_predictn$obs,type = "l",lty=1.8,col="green")
lines(PREDS,type = "l",col="blue")

############################### prediction on actual price ##############################

actual_predictn$actual_price = tEST_data$price

actual_predictn$actual_pred = tEST_data$price_log*2.718

################################# check difference #####################################

actual_predictn$difference= actual_predictn$actual_price - actual_predictn$actual_pred

#################################### difference2 ###################################

actual_predictn$difference2= actual_predictn$obs - actual_predictn$prediction



################################## RMSE ###################################################

RMSE = sqrt(mean((actual_predictn$obs - actual_predictn$prediction**2)^2))

RMSE


################################# RMSE ON ACTUAL DATA ###################################

RMSE2 = sqrt(mean((actual_predictn$actual_price - actual_predictn$actual_pred**2)^2))

RMSE2


################################ MODEL ON ACTUAL PRICE ##################################


model3 = lm(price~sqft_living_log + waterfront+grade+house_aged + view + condition +zipcode,data = training_data)
summary(model3)

plot(model3)

predsss = predict(model3,newdata = tEST_data)

predsss


aaaa = data.frame(observation=tEST_data$price,predx=predsss)

aaaa$difference3 = aaaa$observation-aaaa$predx

plot(aaaa$observation,type = "l",lty=1.3,col="blue")

lines(aaaa$predx,lty=1.3,col="green")

################################# RMSE ###################################################


RMSE3 = sqrt(mean(aaaa$observation - aaaa$predx**2)^2)
RMSE3












