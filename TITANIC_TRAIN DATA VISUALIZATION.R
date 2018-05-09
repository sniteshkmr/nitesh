
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




