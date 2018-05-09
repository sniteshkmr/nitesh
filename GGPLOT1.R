########################GGPLOT#######################################################
library(titanic)
data("titanic_train")
View(titanic_train)
str(titanic_train)

###setting up data#####

titanic_train$Pclass= as.factor(titanic_train$Pclass)
titanic_train$sex=as.factor(titanic_train$Sex)
titanic_train$Survived=as.factor(titanic_train$Survived)
titanic_train$Embarked= as.factor(titanic_train$Embarked)
str(titanic_train)


###what was the survival rate?????
##we use bar graph

ggplot(data = titanic_train,aes(x = Survived))+geom_bar()

##if you want %??

prop.table(table(titanic_train$Survived))

###ADD SOME CUSTOMIZATION



ggplot(data = titanic_train,aes(x = Survived)) +theme_bw() +geom_bar()+labs(y = "PASSENGER COUNT",
                                                                            title = "SURVIVAL RATE")

#Q WHAT WAS THE SURVIVAL RATE BY GENDER???

ggplot(data = titanic_train,aes(x = sex, fill =  Survived)) +theme_bw() +geom_bar()+labs(y = "PASSENGER COUNT",
                                                                            title = "SURVIVAL RATE by SEX")


# Q WHAT WAS THE SURVIVAL RATE OF PEOPLES BY CLASS???


ggplot(data = titanic_train,aes(x = Pclass, fill =  Survived)) +theme_bw() +geom_bar()+labs(y = "PASSENGER COUNT",
                                                                                         title = "SURVIVAL RATE by CLASS")

#Q WHAT WAS THE SURVIVAL RATE OF PEOPLE BY GENDER AND THE CLASS OF TICKET???


ggplot(data = titanic_train,aes(x = sex, fill =  Survived)) + theme_bw() + geom_bar() + facet_wrap(~ Pclass) +labs(y = "PASSENGER COUNT",
                                                                                         title = "SURVIVAL RATE by GENDER & CLASS")


# Q WHAT IS THE DISTRIUTION OF AGES?


ggplot(data = titanic_train,aes(x = Age,fill = Survived)) +theme_bw() + geom_histogram(binwidth = 5)+labs(y = "PASSENGER COUNT",x= "AGE(BANDWIDTH = 5)",
                                                                                         title = "DISTRIBUTION OF AGES")

#box plot visualization



ggplot(data = titanic_train,aes(y = Age,x = Survived)) +theme_bw() + geom_boxplot(binwidth = 5)+labs(y = "PASSENGER COUNT",x= "AGE(BANDWIDTH = 5)",
                                                                                                          title = "DISTRIBUTION OF AGES")

### by gender,age , survival


ggplot(data = titanic_train,aes(x = Age,fill = Survived)) + theme_bw() + facet_wrap(sex ~ Pclass) + geom_density(alpha = 0.5)+labs(y = "PASSENGER COUNT",x= "AGE(BANDWIDTH = 5)",
                                                                                                          title = "DISTRIBUTION OF AGES,SURVIVAL,GENDER")











