
##################################### LOAD THE DATA #################################################

library(data.table)

telecom = fread("telecom_indus.csv")

class(telecom)

dim(telecom)

str(telecom)

colnames(telecom)

head(telecom)

summary(telecom)

##################################### LOAD NECESSARY PACKAGE ####################################

library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(corrplot)
library(psych)
library(caret)

################################### check if there is NA in dataset ##############################

sum(is.na(telecom)) # 11 NA'S PRESENT IN THE DATASET.

### ALL NA'S ARE PRESENT IN "TOTAL CHARGES COLM ###

############################# CHECK IF ID COLM HAS VARIANCE OR NOT ###############################

unique((telecom$customerID))

### since all observations are unique in "customer id colm" we should remove that colm ###

telecom$customerID = NULL

################################ CHECK SENIOR CITIZEN COLM ######################################

table(telecom$SeniorCitizen)

# since senior citizen is a factor in type,so change it's type #

telecom$SeniorCitizen = as.factor(telecom$SeniorCitizen)

ggplot(data = telecom,aes(SeniorCitizen,fill = Churn))+geom_bar()

# from plot we can see that out of 5900(approx) non-senior citizen persons around 2400 are
# churning out

# out of 1143(approx) senior citizen 500 are churning out i.e, nearly 50% of senior citizen are 
# churning out that is way too high.

# we should also think in life expectancy perspective of senior citizen.



###################### converting variable into their respective type..###################################

telecom$Dependents = as.factor(telecom$Dependents)

telecom$gender = as.factor(telecom$gender)

telecom$Partner = as.factor(telecom$Partner)

telecom$Dependents = as.factor(telecom$Dependents)

telecom$PhoneService = as.factor(telecom$PhoneService)

telecom$OnlineSecurity = as.factor(telecom$OnlineSecurity)

telecom$OnlineBackup = as.factor(telecom$OnlineBackup)

telecom$DeviceProtection = as.factor(telecom$DeviceProtection)

telecom$TechSupport = as.factor(telecom$TechSupport)

telecom$StreamingTV = as.factor(telecom$StreamingTV)

telecom$StreamingMovies = as.factor(telecom$StreamingMovies)

telecom$PaperlessBilling = as.factor(telecom$PaperlessBilling)

telecom$Churn = as.factor(telecom$Churn)

###################################### IMPUTATION ##############################################

sum(is.na(telecom$TotalCharges))

library(DMwR)

symnum(cor(telecom[,c(18,19)],use = "complete.obs"))

#telecom  =knnImputation(telecom,k = ,meth = "median")

telecom$TotalCharges[is.na(telecom$TotalCharges)] = mean(telecom$TotalCharges,na.rm = T)

sum(!complete.cases(telecom))


################################## EXPLORATORY DATA ANALYSIS ##################################

ggplot(data = telecom,aes(MonthlyCharges,TotalCharges,col ="steel blue")) + geom_point()

# MONTHLY CHARGES AND TOTAL CHARGES ARE HIGHLY CORRELATED,WHICH IS VERY OBVIOUS #



ggplot( telecom, aes(Churn, MonthlyCharges, color = Churn, fill = Churn)) + geom_boxplot()

# AROUND 50% OF PEOPLE CHURNING WHOSE MONTHLY CHARGES IS HIGH #



ggplot( telecom, aes(Churn, TotalCharges, color = Churn, fill = Churn)) + geom_boxplot()

# AROUND 35% OF PEOPLE CHURNING OUT,EVEN THEY ARE HAVING LESS CHARGES.
# I ASSUME THERE MAY BE SERVICE ISSUES.


ggplot(telecom, aes(Churn, fill = gender)) + geom_bar()

# BOTH MALE AND FEMALE ARE CHURNING OUT IN SAME PROPORTION.



ggplot(telecom, aes(Churn, fill = gender,col = SeniorCitizen)) + geom_bar()

ggplot(telecom, aes(Churn, fill =SeniorCitizen)) + geom_bar()

# CHURNING OF SENIOR CITIZEN IS VERY LESS AS COMPARED TO NON-SENIOR CITIZEN.



ggplot(telecom,aes(tenure,fill = Churn))+geom_bar()

A = ggplot(telecom, aes(PaperlessBilling,fill = Churn)) + geom_bar()
A
# THERE ARE MOST POST-PAID CUSTOMERS WHO ARE CHURNING OUT THAN PREPAID CUSTOMERS.


ggplot(telecom, aes(telecom$Partner,fill = telecom$Churn)) + geom_bar() 

ggplot(telecom, aes(telecom$PhoneService,fill = telecom$Churn)) + geom_bar() 

print(ggplot(telecom,aes(tenure,col = "blue"))+geom_density())

############################## DATA PRE-PROCESSING ###########################################

table(telecom$MultipleLines)

table(telecom$PhoneService)

#### BY SEEING BOTH TABLES OF MULTIPLELINES & PHONE SERVICE COLM,I AM ASSUMING THAT
#### BY IF A PERSON NOT HAVING PHONE,HE IS ALSO NOT HAVING MULTIPLELINES.
#### SO I BELEIVE, CHANGING "NO PHONE SERVICE" TO "NO",CAUSE NO HARM TO OUR DATASET.

telecom$MultipleLines = ifelse(telecom$MultipleLines== "Yes","Yes","No")





########################### CHECKING INTERNET SERVICE COLM ####################################



table(telecom$InternetService)


######################### CHECKING MONTHLY CHARGES W.R.T INTERNET SERVICE ######################

int_service = telecom%>%select(InternetService,MonthlyCharges,TotalCharges)%>%filter(InternetService=="DSL"|InternetService=="Fiberoptic")%>%arrange(desc(MonthlyCharges))

NO_INTERNET = telecom%>%select(InternetService,MonthlyCharges,TotalCharges)%>%filter(InternetService=="No")%>%arrange(desc(MonthlyCharges))


# THOSE WHO ARE OPTED FOR FIBER-OPTIC INTERNET SERVICE ARE PAYING MORE THAN THOSE WHO OPTED
# FOR DSL INTERNET SERVICE & WHO ARE NOT OPTED FOR INTERNET-SERVICES ARE PAYING LESS i.e OBVIOUS.

# THE THING IS,COMPANY SHOULD MORE CONCENTRATE IN THOSE CUSTOMERS WHO ARE PAYING MORE TO
# THE COMPANY BECAUSE THEY ARE THE REVENUE GENERATOR AND ALSO MAJORITY IN NUMBER.





# LET'S CHECK OUT,HOW MANY ARE CHURNING OUT BECAUSE OF INTERNET SERVICES.

CHURN_INTERNETSERVICE = telecom%>%select(InternetService,Churn)%>%arrange(Churn)

table(CHURN_INTERNETSERVICE)

# BY SEEING THE TABLE WE CAN FIGURE OUT THAT OUT OF 3096 FIBER OPTIC USER 1297 ARE CHURNING.
# MAY BE THERE IS A ISSUE IN THE INTERNET SERVICE OF FIBER-OPTIC

# DSL = 2421 CUSTOMERS & 459 ARE CHURNING OUT.

CHURN_INTERNETSERVICE$InternetService = as.factor(CHURN_INTERNETSERVICE$InternetService)

ggplot(CHURN_INTERNETSERVICE,aes(Churn,fill = InternetService))+geom_bar()


################## checking internet service  satisfaction w.r.t gender ############################

CHURN_INTERNETSERVICE_gender = telecom%>%select(InternetService,Churn,gender)%>%arrange(Churn)
table(CHURN_INTERNETSERVICE_gender)

ggplot(CHURN_INTERNETSERVICE_gender,aes(Churn,fill = gender,col = InternetService))+geom_bar()

# BOTH MALE AND FEMALE ARE CHURNING OUT IN SAME PROPORTION W.R.T INTERNET SERVICE



######################### checking partners,dependent,monthlycharge,churn colm's#################

part_churn = telecom%>%select(Partner,Churn)%>%arrange(Churn)
dep_churn = telecom%>%select(Dependents,Churn)%>%arrange(Churn)

table(part_churn)

table(dep_churn)

ggplot(part_churn,aes(Partner,fill = Churn)) +geom_bar()

ggplot(dep_churn,aes(Dependents,fill = Churn)) +geom_bar()





######################### checking  online security colm #########################################################

 table(telecom$OnlineSecurity)

 table(telecom$InternetService)

#### BY SEEING BOTH TABLES OF INTERNETSERVICE & ONLINESECURITY  COLM,I AM ASSUMING THAT
#### BY IF A PERSON NOT HAVING INTERNETSERVICE,HE IS ALSO NOT HAVING ONLINESERVICE.
#### SO I BELEIVE, CHANGING "NO INTERNET SERVICE" TO "NO",CAUSE NO HARM TO OUR DATASET.

telecom$OnlineSecurity = ifelse(telecom$OnlineSecurity== "Yes","Yes","No")

data_online = telecom%>%select(OnlineSecurity,Churn)%>%arrange(Churn)
table(data_online)

ggplot(data_online,aes(OnlineSecurity,fill = Churn))+geom_bar()


############################### CHECK COLM ONLINEBACKUP COLM ##################################


table(telecom$OnlineBackup)

table(telecom$InternetService)

#### similarly ####

telecom$OnlineBackup = ifelse(telecom$OnlineBackup=="Yes","Yes","No")

telecom$DeviceProtection = ifelse(telecom$DeviceProtection=="Yes","Yes","No")

telecom$TechSupport = ifelse(telecom$TechSupport=="Yes","Yes","No")

telecom$StreamingTV = ifelse(telecom$StreamingTV=="Yes","Yes","No")

telecom$StreamingMovies = ifelse(telecom$StreamingMovies=="Yes","Yes","No")


############################### CHECK DISTRIBUTION OF  #######################################

library(psych)

pairs.panels(telecom[,c(5,18,19)])

# total charges colm is right skewed #

symnum(cor(telecom[,c(5,18,19)]))

# strong correlation between tenure & total charges #


telecom$TotalCharges_sqrt = sqrt(telecom$TotalCharges)

############################# SPLITTING DATA INTO 70% & 30% #################################

library(caTools)
set.seed(123)
split = sample.split(Y = telecom$Churn,SplitRatio = 0.7)
trainig_data = subset(telecom,split==T)
test_data = subset(telecom,split==F)

################################# MODELLING #####################################################

model1 = glm(Churn~.-TotalCharges,data = trainig_data,family = binomial(link =logit))

summary(model1)


model2 = glm(Churn~.-TotalCharges-gender-Partner-Dependents-tenure-PhoneService-MonthlyCharges-DeviceProtection-OnlineBackup-OnlineSecurity,data = trainig_data,family = binomial(link =logit))

summary(model2)



model3 = glm(Churn~.-TotalCharges-Partner-tenure-DeviceProtection-OnlineBackup-gender,
             data = trainig_data,family = binomial)
summary(model3)



model4 = glm(Churn~.-TotalCharges-gender-tenure-Partner-OnlineSecurity-TechSupport,
             data = trainig_data,family = binomial(link =logit))

summary(model4)

res = predict(model4,newdata = test_data,type ="response")

head(res)

test_data$prediction = ifelse(pred>0.3,1,0)

####################### creating another data frame ######################################

data_validate = test_data%>%select(prediction,Churn)
data_validate
########################## confusion matrix #################################################

confusion.matrix = table(CHURN = test_data$Churn,predicted_value = test_data$prediction)
confusion.matrix



################################## VALIDATION METRICS #########################################


accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy



error = 1-accuracy
error


precision = table(test_data$Churn,test_data$prediction)[2,2]/sum(table(test_data$Churn,test_data$prediction)[,2])

precision


recall = table(test_data$Churn,test_data$prediction)[2,2]/sum(table(test_data$Churn,test_data$prediction)[2,])

recall


f1score =  2 * precision * recall/(precision+recall)
f1score


############################## rocr #########################################



library(ROCR)

rocrpred = prediction(res,test_data$Churn) 

rocrperf= performance(rocrpred, "tpr","fpr")

plot(rocrperf,colorize = T,print.cutoffs.at=seq(0.1,by=0.1))


############################# AUC ###########################

AUC_1 = performance(rocrpred, measure = 'auc')@y.values[[1]]

AUC_1


################################################################################################

