###################### DECISION TREE ON TITANIC DATASET ###########################

titanic = fread("titanic.csv")

##################### load necessary package ######################################

library(dplyr)
library(stringr)
library(rpart)
library(rattle)
library(party)
library(partykit)

########################## EDA #################################################

str(titanic)

summary(titanic)

dim(titanic)

############################# checking missing values ##########################


colSums(is.na(titanic))

#### IN "BODY" COLUMN,THERE ARE 1188 MISSING VALUES OUT OF 1309 OBS.
#### SO REMOVING "BODY" COLM.

titanic$body = NULL

##################### checking empty blocks #################################

colSums(titanic=="")


### imputing in embarked column & cabin

titanic$embarked[titanic$embarked==""]="c"



### imputing in fare column.

 which(is.na(titanic$fare))
 
 titanic$fare[is.na(titanic$fare)] = 33
 
 
#### imputing in age column 

titanic$age = sapply(titanic$age,FUN = function(x) {ifelse(is.na(x),
                                                           median(titanic$age,na.rm = T),x)})


### substring cabin

titanic$cabin = substr(titanic$cabin,1,1)


### assigning data types 

apply(titanic,2,function(x)length(unique(x)))


#### assigning factor colm

titanic$pclass = as.factor(titanic$pclass)
titanic$sex = as.factor(titanic$sex)
titanic$survived = as.factor(titanic$survived)
titanic$cabin = as.factor(titanic$cabin)

###deleting unwanted colm

titanic$boat = NULL
titanic$home_dest = NULL
titanic$name = NULL
titanic$ticket = NULL


### substring cabin

titanic$cabin = substr(titanic$cabin,1,1)

colSums(titanic=="")

cabin_fare = titanic[titanic$cabin==""]

fare_titanic = titanic%>%select(cabin,fare)%>%arrange(desc(fare))

fare = cabin_fare%>%select(cabin,fare)%>%arrange(desc(fare))

summary(fare$fare)

quantile(fare$fare,c(0.8,0.9,0.95))

mean(cabin_fare$fare)

### replacing with "C".


titanic$cabin[titanic$cabin==""]="c"

####################### splitting into 70% and 30% ###################################

library(caTools)
set.seed(357)
split = sample.split(Y = titanic$survived,SplitRatio = 0.7)
train_data = subset(titanic,split==T)
test_data = subset(titanic,split==F)


################## DECISION TREE #################

library(rpart)

tree = rpart(survived~.,data = train_data,method = "class")

fancyRpartPlot(tree)

train_pred = predict(tree,newdata = train_data,type = "class")

train_predcf = table(actual=train_data$survived,predicted = train_pred)

train_predcf

### validation on train_data

p = 247/(247+24)

r =247/(247+103)

f1 = 2*p*r/(p+r)

p
r
f1


####### prediction on test_data #####


test_pred = predict(tree,newdata = test_data,type = "class")

table(test_data$survived,test_pred)


#### validation on test_data

p2 = 97/(97+24)

r2 =97/(97+53)

f12 = 2*p*r/(p+r)

p2
r2
f12

########################## pruning ######################################


### CP matrix to check the cp and relative error 

printcp(tree)




### Pruning a tree 

prunedtree = prune(tree, cp = 0.011429)

fancyRpartPlot(prunedtree)



## performance of pruned tree 


test_data$pruned_pred = predict(prunedtree, newdata = test_data, type="class")


table(test_data$survived, test_data$pruned_pred)


### almost same ###


