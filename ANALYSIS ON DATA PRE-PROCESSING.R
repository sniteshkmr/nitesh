
################################## DATA PRE-PROCESSING ############################################

#DATA PRE-PROCESSING IS THE MAJOR INITIAL CHALLENGE IN DATA SCIENCE PROJECT.
#DATA PRE-PRECESSING  CONSUMES AROUND 80% OF TIME IN A PROJECT.
#ONE OF THE INITIAL AND MAJOR SKILL NEEDED IN ANY DATA SCIENCE PROJECT IS DATA PRE-PROCESSING.
#LET'S GET START.

# DATA CAN BE IN DIFFERENT FORMAT
#1. CSV OR FLAT FILES .CSV
#2. TAB DELIMITED FILES .TXT
#3  EXCEL FILE .XLSX
#4. SAS FILES ETC.

# IMPORTING CSV FILE

house = read.csv("us_house.csv")

# IF YOU KNOW YOUR FILE PATH 

path = file.path("us_house.csv")

#IF YOU DONT KNOW YOUR FILE PATH AND WORKING DIRECTORY

NITESH = read.csv(file.choose())

# ANOTHER WAY OF IMPORTING FILES 

library(readr)

HOUSE_US = read_csv("us_house.csv")

# THE STRINGASFACTOR = F BY DEFAULT,HENCE WE HAVE TO MANUALLY MAKE IT TRUE. 

# THE MOST EFFICIENT AND CONVINIENT WAY OF IMPORTING LARGE DATA.

library(data.table)

house_df = fread("us_house.csv")

house_df2 = fread("us_house.csv",drop = 5:8)

house_df3 = fread("us_house.csv",select = c(1:15))

telecom = fread("telecom_indus.csv")

# IMPORTING TAB DELIMITED FILE

#read.delim("dataset.txt",stringsAsFactors = F)

# IMPORTING EXCEL FILE 

install.packages("readxl")

library(readxl)

read_excel("dataset.xlsx",sheet = 1,col_names = T,col_types = NULL,skip = 0)

#COL_NAMES = T BECAUSE TO PRINT THE COLM NAME OTHER WISE IT WILL TAKE SYSTEM DEFINED NAMES.
#SKIP = 0, READ_EXCEL()WOULD BY SKIP 5 ROWS BY DEFAULT.
# SHEET = 1 ,HOW MANY SHEET YOU WANT TO IMPORT.

# THE FIRST THING WE DO AFTER DATA IMPORT,IS TO CHECK THE CLASS OF THE DATASET.

class(telecom)
class(NITESH)

# CHECK THE DIMENSION,STRUCTURE,SUMMARY

str(telecom)
dim(NITESH)
summary(house_df)

library(dplyr)
glimpse(NITESH)

# CHECK THE LAST 5 AND FIRST 5 OBSERVATIONS OF DATASET.

head(NITESH,5)
tail(house_df,5)

#   CHANGING COLM NAMES

col_names = paste("col",1:15,sep = "")
names(house_df3)= col_names

m =paste("nitesh",1:15,sep="")
names(house_df3)= m

library(reshape)
house_df3=rename(house_df3,c(nitesh1 = "janani"))

## string manupulation##

library(stringr)

# MAKE ALL UPPER CASES 

telecom$gender= toupper(telecom$gender)

# make all lower cases 

telecom$gender= tolower(telecom$gender)

# trim all white spaces 

telecom$InternetService = str_trim(telecom$InternetService)

## replacing strings ##

telecom$MultipleLines = str_replace(telecom$MultipleLines,"No Phone Service","No")

# CHECKING FOR MISSING VALUES AND IMPUTATION 

any(is.na(telecom))

any(is.na(telecom$Dependents))

sum(is.na(telecom))

sum(!complete.cases(telecom))

sum(is.na(telecom$SeniorCitizen))

!complete.cases(telecom)

!is.na(telecom)

# to check how many rows contains missing values ###

library(DMwR)

nrow(telecom[!complete.cases(telecom),])

manyNAs(telecom,0.01) # WHICH ROWS HAS MISSING VALUES

#telecom = telecom[-manyNAs(telecom),]

# algae[48,"colname"] = mean(algae$colmname,na.rm =T)

symnum(cor(NITESH[,3:8],use = "complete.obs"))

summary(telecom)

# REGRESSION IMPUTATION

# lm(col~col2,data = algae)

# IMPUTATION 

telecom = knnImputation(algae,k=10,meth = "median")

# missing values in algae dataset

data("algae")
dim(algae)
sum(is.na(algae))

# how many rows has na values 

nrow(algae[!complete.cases(algae),])

### no. of which has more than 20% of missing values ###

manyNAs(algae,0.2)

algae = algae[-manyNAs(algae),]

nrow(algae)

#### IMPUTATION ####

algae[48,"mxPH"] = mean(algae$mxPH,na.rm = T)

### IMPUTATION BY USING CORRELATION AND REGRESSION ###

symnum(cor(algae[,4:18],use = "complete.obs"))

## po4 is strongly correlated with opo4 ##

data("algae")
dim(algae)
algae = algae[-manyNAs(algae),]
nrow(algae)

lm(algae$PO4~algae$oPO4)

# imputation by linear regression #

algae[28,"PO4"] = 42.897+1.293*algae[28,"oPO4"]

## knn imputation ##

data("algae")
dim(algae)
algae = algae[-manyNAs(algae),]
nrow(algae)
algae  =knnImputation(algae,k = 10,meth = "median")
sum(is.na(algae))

##### TIDYR PACKAGE ####

telecom_company = read.csv("telecom_indus.csv")
library(dplyr)
library(tidyr)
telecom_company =  unite(data = telecom_company,col = new_col,gender,Partner,sep = "-")

telecom_company = separate(data = telecom_company,new_col,c("col1","col2"))

?separate

# LOOK AT GATHER AND SPREAD FUCTION

i = which(telecom_company$Dependents==No)

# str_replace_all(att$state,"\\.","")


#### MISSING VALUES ####

# MISSING DATA IS A PROBLEM AFFECTING MOST DATABASES.BECAUSE MOST STATISTICAL MODEL OPERATES ONLY
# ON COMPLETE OBSERVATION OF EXPOSURE AND VARIABLE OUTCOME.


### WHY THERE IS MISSING VALUE ###

# BEFORE IMPUTING ANYTHING INPLACE OF NA'S,WE SHOULD KNOW WHY IT IS NA'S THERE IN THE FIRST PLACE.

#1. THE VALUE IS MISSING BECAUSE IT WAS FORGOTTEN OR LOST.
#2. THE VALUE IS MISSING BECAUSE IT WAS APPLICABLE TO THE INSTANCE.
#3. BECAUSE THE VALUES ARE NOT ELECTRONICALLY RECORDED.
#4. UNRELATED TO PATIENT CONDITION.(MEDICAL DATA)


# THE GENERAL STEPS THAT SHOULD BE FOLLOWED FOR HANDLING MISSING VALUES ARE-

# IDENTIFY PATTERNS AND REASON FOR MISSING DATA.

#ANALYSE THE PROPORTION OF MISSING DATA.

# CHOOSE THE BEST IMPUTATION METHOD.


############## HANDLING MISSING VALUES #############

#1. DELETE THE MISSING ROWS.(NOT RECOMMENDED)
#2. SINGLE IMPUTATION METHODS.(MEAN/MEDIAN/MODE/LINEAR INTERPOLATION/HOT DECK AND COLD DECK)
#3. MODEL BASED METHODS (REGRESSION,KNN,MULTIPLE IMPUTATION)

############################# data manupulation ##############################################

library(dplyr)

data("mtcars")
data("iris")

dim(mtcars)
dim(iris)

mynew_data = tbl_df(mtcars)
myiris_data = tbl_df(iris)

myiris_data

### renaming variable ###
library(reshape)

mtcars

rename(mtcars,hp = "mileper_gallon",cyl = "cylinder")

myiris_data = rename(myiris_data,c(Sepal.Length = "hpp"))



#### data.table package ###

mydata = data("airquality")
dim(airquality)
library(data.table)

ID = c(1,2,3,4,5)
NAMES = c("nitesh","gopi","raghu","joseph","piston")
DOB = c(1992,1995,1998,1997,1982)
SUBJECT = c("maths","biology","science","psycology","physics")

data = data.frame(ID,NAMES,DOB,SUBJECT)

data.table(data)

library(reshape2)
library(reshape)

mt = melt(data,id = (c("ID","NAMES")))

mt

################################# create a dummy set #############################################
library(tidyr)

names = c("A","B","C","D","E","A","B")
weight = c(55,66,44,85,24,53,47)
class = c("maths","science","social","accounts","physics","psycology","hindi")
age = c(21,22,36,45,23,28,26)

tdata = data.frame(names,age,weight,class)
 longt = tdata%>%gather(names,class)
longt


####################################### lubridate package ####################################


library(lubridate)

now()

time = now()
time
class(time)

### updating time ###

time = update(time,year = 1992,month = 07,day = 22)
time


################################### GATHER FUNCTION #############################################

id = c("X","Y","z")
name = c(1,4,5)
age = c(2,5,8)
class = c(3,6,7)
nitesh00 = data.frame(id,name,age,class)

gather = gather(data =nitesh00,key = gather_colm,value = gather_value,-id )
gather

#### wide to long format ###

##### reverse function is spread #####

nitesh77 = spread(gather,key = gather_colm,value = gather_value)
nitesh77


######## LUBRIDATE PACKAGE ############

library(lubridate)
a = ymd("2018-08-25")
mdy("08-01-2014")
mdy("august 25 2014")
ymd("2018 july 25")
class(a)

############################### STRING MANUPULATION ###########################################

library(stringr)

str_trim()
str_replace()
str_to_lower()





#Converting to Factors

for(i in 1:ncol(data_new)){
 data_new[,i] = as.factor(data_new[,i])
}

###############################################################

















