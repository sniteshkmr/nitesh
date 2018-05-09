
################################ EXPLORATORY DATA ANALYSIS ###############################

################################ SAGAR SIR: PART 1 #######################################


library(ggplot2)

data(mpg)

head(mpg)

################################ BASIC R PACKAGE PLOTTING ##############################


###### ONE DIMENSIONAL CONT. VARIABLE(INT,DOUBLE,NUMERIC), WE USE HIST AND BOX PLOT#####

hist(mpg$cty)


hist(mpg$cty,breaks = 10)

##### BOX-PLOT #####

boxplot(mpg$cty)


################################ FACTOR OR DISCRETE VARIABLE #########################

####### FOR DICRETE VARIABLE,WE USE BAR PLOT & PIE CHART #############################


### BAR CHART ##############

table(mpg$drv)

barplot(table(mpg$drv))


########## PIE CHART ########

pie(table(mpg$drv)) # GENERALLY WE MOSTLY USE BAR CHART INSTEAD OF PIE CHART #

### NOTE : DIFF BETWEEN BAR AND HIST. IS GAPS BETWEEN THEIR GRAPHS.


########################## TWO CONT. VARIABLE ########################################

### WE USE SCATTER PLOT OR POINT CHART & LINE CHART #################################


plot( mpg$cty, mpg$hwy)

### IF WE WANT TO HAVE A "LINE" TYPE PLOT ########


a = c( 10,20,30,40,50)
b = c(101,208, 308, 405, 506)

plot(a,b, type = "l")


## line chart #######################

line(a,b) ## WE USE LINE CHART VERY RARELY & IN SMALL SET OF DATA.



################### TWO DISCRETE VARIABLE ###############################

### WE USE BAR PLOT

barplot(table(mpg$class, mpg$drv))



################### ONE CONT. & ONE DISCRETE VARIABLE ###############################

########################### WE USE BOX PLOT & BAR CHARTS ################################################################

#### IF THERE IS "N" NO. OF FACTORS WE USE BOX PLOT.#########################

boxplot(mpg$cyl,mpg$cty)


barplot(mpg$cyl,mpg$cty)


###################################### GGPLOT ########################################

################## FOR BETTER AND MULTI USE #############################################

library(ggplot2)

################## one cont. variable #######################################

ggplot(mpg, aes(cty)) + geom_histogram()

ggplot(mpg, aes(cty)) + geom_histogram(binwidth = 3)

ggplot(mpg, aes(cty)) + geom_histogram(binwidth = 2)


########################################################################################


### IN GGPLOT WE USE ONE CONT. VARIABLE WITH A LITTLE TRICK.

### 1 = dummy value

ggplot(mpg, aes(1,cty)) + geom_boxplot()

############################################################################################



######### BAR CHART FOR ONE FACTOR VARIABLE ##########################################

ggplot(mpg, aes(class)) + geom_bar()


#######################################################################################

############################# FOR TWO CONT. VARIABLES #################################


ggplot( mpg, aes(cty, hwy)) + geom_point()

##########################################################################################


############## FOR ONE CONT. AND ONE DISCRETE VARIABLE ###############################


ggplot( mpg, aes(class, cty) ) + geom_boxplot()



ggplot(mpg, aes(drv, cty)) + geom_boxplot()


#######################################################################################

############################## TWO FACTOR VARIABLE ####################################


######### BAR GRAPH #########

ggplot(mpg, aes( manufacturer, fill=class)) + geom_bar()



ggplot(mpg, aes(class, fill = drv)) + geom_bar()



#######################################################################################



############################### MULTI DIMENSIONAL VARIABLES ##############################


ggplot(mpg, aes(cty, hwy, shape = drv, color = class)) + geom_point()


#########################################################################################


############################### FACETS ###################################################

a = ggplot(mpg, aes(cty, hwy, shape = drv, color = class, size = cyl)) + geom_point()

a ### SIZE MUST BE AN ORDINAL VARIABLE

## facets to add more variables to the plot 

### FACET MUST BE ALWAYS A FACTOR VARIABLE.

a + facet_grid( fl ~ .) ## horizontal facets 

a + facet_grid(. ~ fl) ### VERTICAL FACETS

?facet_wrap



############## FACET CAN TAKE TWO FACTOR VARIABLE ##################################


mpg$year = as.factor(mpg$year)

a + facet_grid(year ~ fl)


########################################################################################


############################## IMPROVING READIBILITY ######################################


b =  ggplot(mpg, aes(cty, hwy)) + geom_point()
b

############### if we want a smooth line which goes through variable #################

########## we need to add a func. as +geom_smooth(method = "lm) #################

b = ggplot(mpg, aes(cty, hwy)) + geom_point() + geom_smooth(method = "lm")

### "lm" is linear model

b

################### adding x-axis and y-axis naming #############################

c = b + ggtitle("City Vs Highway Mileage") + xlab("City Mileage") + ylab("Highway Mileage")


c

##########################################################################################

############### ADDING X- LIMIT AND Y-LIMIT ##########################################


d = c + coord_cartesian(xlim = c(10,30), ylim = c(10,35)) 

d

v = a + coord_cartesian(xlim = c(10,20), ylim = c(10,35))

v



########################################################################################



################################## ADDING THEMES #####################################


f = d + theme_classic()

f


ggplot(mpg, aes(class, fill = drv)) + geom_bar() + theme_classic()

ggplot(mpg, aes(class, fill = drv)) + geom_bar() + theme_bw()

############# best theme is theme_classic ##########################################



########################### ADDING POSITION ############################################



## adjusting positions 


ggplot(mpg, aes(class, fill = drv)) + geom_bar(position = "dodge") + theme_bw()

ggplot(mpg, aes(class, fill = drv)) + geom_bar(position = "fill") + theme_bw()

ggsave("plot.pdf") ####### SAVING IMAGE ##############################


############################################################################################

##############################################################################################

############################################################################################


############################ GGPLOT USING CHURN.CSV ####################################


df1 = read.csv("churn.csv")

library(ggplot2)

head(df1)

str(df1)

table(df1$CustServ.Calls)

df1$Churn = as.factor(df1$Churn)


class(df1$Churn)

##########################################################################################

### comparing one cont. and one factor variable

### acc.length vs churn

ggplot(df1, aes(Churn,Account.Length,color = Churn)) + geom_boxplot()

ggplot(df1, aes(Churn,Account.Length,fill = Churn)) + geom_boxplot()


### there is no impact on customer churn by account.lenght ##########################

########################################################################################


### comparing relation with day. mins


ggplot(df1, aes(Churn,Day.Mins,fill = Churn)) + geom_boxplot()



############################################################################################


### two  Factor variable 


df1$Intl.Plan = as.factor(df1$Intl.Plan)

ggplot(df1, aes(Churn, fill = Intl.Plan)) + geom_bar()


##########################################################################################




### Cust.Service calls 

ggplot( df1, aes(Churn, CustServ.Calls,fill=Churn))+geom_boxplot()



#### treat custserv.calls as a factor variable 

df1$cust.calls = as.factor(df1$CustServ.Calls)

ggplot(df1, aes(cust.calls, fill= Churn)) + geom_bar()





## two cont. variables 

## Using base plots 

plot(df1$Day.Mins, df1$Eve.Mins)

cor(df1$Day.Mins, df1$Eve.Mins)## 0.007042511 # hence there is no relation


plot(df1$Day.Mins, df1$Day.Charge)

cor(df1$Day.Mins,df1$Day.Charge)

#############################################################################################


######### matrix plot

plot(df1[,1:5])



##########################################################################################3
str(df1)

ggplot(df1, aes(Day.Mins, Eve.Mins, color = Churn, shape = Intl.Plan)) + geom_point()



###########################################################################################

### taking sample out of whole data set###############################################

set.seed(1,2,3,4)## for fixing of sample................

ids = sample(nrow(df1),nrow(df1)*0.25)

ra_sample = df1[ids,]

ra_sample

ra_omm = df1[-ids,]
ra_omm

a = ggplot(ra_sample, aes(Day.Mins, Eve.Mins, color = Churn, shape = Intl.Plan)) + geom_point()

a


a + facet_grid( VMail.Plan ~ .)


a + facet_grid( . ~ VMail.Plan )


##########################################################################################

##############################################################################################


########################## GG-PLOT USING titanic_train data #############################



# TITANIC_TRAIN DATA VISUALIZATION AND ANALYSIS########################################

# LOAD LIBRARY(GGPLOT) AND DATASET

library(titanic)
library(ggplot2)

data("titanic_train")
str(titanic_train)
### overview the data and build some strategy of analysis.

#### check the variables and class of data......................................

str(titanic_train)


###SETTING UP DATA#####

#CONVERT THE INTEGER VALUE IN TO FACTOR VARIABLE




titanic_train$Pclass = as.factor(titanic_train$Pclass)

titanic_train$Survived = as.factor(titanic_train$Survived)

titanic_train$Sex = as.factor(titanic_train$Sex)

titanic_train$Embarked = as.factor(titanic_train$Embarked)

str(titanic_train)


### we will visualize our data through questions.

### Q WHTA IS THE SURVIVAL RATE????


# AS "SURVIVED" VARIABLE IS A FACTOR VARIABLE.

# FOR ONE  FACTOR VARIABLE,"BAR" IS THE BEST CHART FOR VISUALIZATION.

ggplot(titanic_train, aes(Survived))+ geom_bar()

# if we want in % value.....................

prop.table(table(titanic_train$Survived))

### ADD SOME THEME AND MORE COLOR

ggplot(titanic_train, aes(Survived)) + theme_bw() + geom_bar() + labs(y = "PASSENGER COUNT",
                                                                      title = "SURVIVAL RATE IN TITANIC")


### adding color is nice

ggplot(titanic_train, aes(Survived, fill = Survived)) + theme_classic() + geom_bar() + labs(y = "PASSENGER COUNT",
                                                                                            title = "SURVIVAL RATE IN TITANIC")



### one factor variable "BOX PLOT"

ggplot(titanic_train, aes(1,Survived))+ geom_boxplot()


# Q WHAT WAS THE SURVIVAL RATE BY GENDER????


ggplot(titanic_train, aes(Sex, fill = Survived)) + theme_classic() + geom_bar() + labs(y = "PASSENGER COUNT",
                                                                                       title = "SURVIVAL RATE BY GENDER")

# Q WHAT WAS THE SURVIVAL RATE BY "PCLASS"?


ggplot(titanic_train, aes(Pclass,fill = Survived)) + theme_classic() + geom_bar() + labs(y = "PASSENGER COUNT",
                                                                                         title = "SURVIVAL RATE BY CLASS")


# Q WHAT WAS THE SURVIVAL RATE BY "PCLASS" AND "GENDER"?

# WE CAN LEVERAGE FACET HERE, IT DRILLS DOWN THE DATA...



ggplot(titanic_train, aes(Sex,fill = Survived)) + theme_classic() + geom_bar() + facet_wrap(~ Pclass)+ labs(y = "PASSENGER COUNT",
                                                                                                            title = "SURVIVAL RATE BY CLASS & GENDER")



# Q WHAT IS THE DISTRIBUTION OF PASSENGERS BY THEIR AGES???



ggplot(titanic_train, aes(Age)) + theme_classic() + geom_histogram(binwidth = 5) + labs(y = "PASSENGER COUNT",x = "DISTRIBUTION OF AGES(BINWIDTH = 5)",
                                                                                        
                                                                                        title = "DISTRIBUTION OF PASSENGER BY AGES")

#Q AGES VS SURVIVal?????




ggplot(titanic_train, aes(Age,fill= Survived)) + theme_classic() + geom_histogram(binwidth = 5) + labs(y = "PASSENGER COUNT",x = "DISTRIBUTION OF AGES(BINWIDTH = 5)",
                                                                                                       
                                                                                                       title = "AGES VS SURVIVAL")
## by using box plot


ggplot(titanic_train, aes(Survived,Age,fill = Survived)) + theme_classic() + geom_boxplot() + labs(y = "AGE",x = "DISTRIBUTION OF AGES(BINWIDTH = 5)",
                                                                                                   
                                                                                                   title = "DISTRIBUTION OF PASSENGER BY AGES")




ggplot(titanic_train, aes(Age,fill = Survived)) + theme_classic()+ facet_wrap(Pclass~Sex) + geom_density(alpha= 0.5) + labs(y = "AGE",x = "DISTRIBUTION OF AGES(BINWIDTH = 5)",
                                                                                                                            
                                                                                                                            title = "DISTRIBUTION OF PASSENGER BY AGES")


###########################################################################################

##############################################################################################

############################################################################################


############################## diamond ggplot #######################################

library(ggplot2)

data("diamonds")

head(diamonds)

ggplot(diamonds, aes(carat))+geom_histogram()

ggplot(diamonds, aes(carat))+geom_histogram(binwidth = .50)




#### fill above histogram by cut #############

### adding title ###############



ggplot(diamonds, aes(carat,fill = cut))+geom_histogram()+ ggtitle("diamonds cut by carat")



ggplot(diamonds, aes(carat,fill = cut))+ geom_freqpoly()

############# adding themes ############################3


ggplot(diamonds, aes(carat,fill = cut))+theme_classic()+geom_histogram()+ labs(x = "FREQ OF CARAT",y = "COUNT OF CARAT",title = "DIAMOND PARK")




#############################################################################################
############################################################################################
#######################################################################################






############################ TRY ANOTHER DATASET #######################################

try(data(package = "ggplot2"))

View(txhousing)

?txhousing


str(txhousing)


ggplot(txhousing, aes(year,sales))+ geom_bar(stat = "identity")


ggplot(txhousing, aes(year,sales,label = sales))+ geom_bar(stat = "identity")+geom_text()



### suppose we need to have avg sales by year #################


txhousing.agg = aggregate(sales ~ year,txhousing,mean)

View(txhousing.agg)

ggplot(txhousing.agg, aes(year,sales,label = sales))+ geom_bar(stat = "identity")+geom_text(size = 3)

txhousing.agg$sales = round(txhousing.agg$sales,1)


ggplot(txhousing.agg, aes(year,sales,label = sales))+theme_classic()+ geom_bar(stat = "identity")+geom_text(size = 5)



ggplot(txhousing.agg, aes(year,sales,label = sales))+theme_classic()+ geom_bar(stat = "identity")+geom_text(size = 5,vjust = 5)


ggplot(txhousing.agg, aes(year,sales,label = sales))+theme_classic()+ geom_bar(stat = "identity")+geom_text(size = 5,vjust = 5,color = "blue")


### create a new variable called "value" for specifying low value or high value###

summary(txhousing$median)

txhousing$value = ifelse(txhousing$median>123800,"high value","low value")

View(txhousing)



txhousing.agg = aggregate(sales ~ year+value,txhousing,mean)


View(txhousing.agg)








ggplot(txhousing.agg, aes(year,sales,label = sales,fill = value))+theme_classic()+ geom_bar(stat = "identity")+geom_text(vjust = 2)




ggplot(txhousing.agg, aes(year,sales,label = sales,fill = value))+theme_classic()+ geom_bar(stat = "identity")+geom_text(size = 5)





###########################################################################################
###########################################################################################
##########################################################################################

try(data(package = "ggplot2"))

View(economics)
str(economics)
ggplot(economics, aes(unemploy,uempmed)) + geom_point()

economics_scatter = economics

economics_scatter$year = as.numeric(format(economics$date,"%Y"))

View(economics_scatter)


economics_scatter$last3 = economics_scatter[which(economics_scatter$year== 2014 | economics_scatter$year == 2015 | economics_scatter$year == 2016),]


View(economics_scatter)
























































































































