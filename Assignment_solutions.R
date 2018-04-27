
library(dplyr)
train=read.csv("train.csv")
test=read.csv("test.csv")
data=train

#ques1--------------------------------------------------------
data=read.csv("train.csv")

summary(data)

data$stay=as.character(data$Stay_In_Current_City_Years)

data$stay=ifelse(data$stay=="4+",4,data$stay)

data$stay=as.factor(data$stay)
summary(data)

#ques2----------------------------------------------------------

data$Marital_Status=as.factor(as.character(data$Marital_Status))
n.mar.0=nrow(data[data$Marital_Status==0,])
n.mar.0

#ques3----------------------------------------------------------

n.age.mar.0=nrow(data[data$Age=="26-35"&data$Marital_Status==0,])
n.age.mar.0

#ques4----------------------------------------------------------

data.dist.age=data[data$Age=="26-35"&data$Marital_Status==0,]

table(data.dist.age$Age)

#ques5----------------------------------------------------------

length(unique(data$Age))

#ques6----------------------------------------------------------

length(unique(data$User_ID))

#quest7--------------------------------------------------------
prod.id=table(data$Product_ID)
prod.id=as.data.frame(prod.id)
prod.id[prod.id$Freq==max(prod.id$Freq),]

#quest8--------------------------------------------------------

data.m=data[data$Gender=="M",]
data.f=data[data$Gender=="F",]

avg.m=sum(as.numeric(data.m$Purchase))/nrow(data.m)
avg.f=sum(as.numeric(data.f$Purchase))/nrow(data.f)
#or
aggregate(train$Purchase,list(train$Gender),mean)

#quest9--------------------------------------------------------

data.f.age=data.f[data.f$Age=="0-17",]
avg.f.age=sum(as.numeric(data.f.age$Purchase))/nrow(data.f.age)
#or
avd.f.age2=mean((data%>%filter(Gender=='F'&Age=='0-17'))$Purchase)

#quest10--------------------------------------------------------

even <- seq_len(nrow(data)) %% 2

data.odd=data[!even,]

data.odd.avg=sum(as.numeric(data.odd$Purchase))/nrow(data.odd)
#or
mean(train[seq(1,nrow(train),2),"Purchase"])
#quest11--------------------------------------------------------


sum(is.na(data))
train2=train[rowSums(is.na(data))<1,]
#or
train2=na.omit(data)
sum(is.na(train2))

#ques12--------------------------------------------------------
table(data$City_Category,data$Age=="0-17")

#ques13--------------------------------------------------------

sum(is.na(train$Product_Category_2))

#ques14--------------------------------------------------------

library(lazyeval)
pr_1=train%>%
  filter(is.na(Product_Category_2))%>%
  group_by(Product_Category_1)%>%
  summarise(count=n())%>%
  arrange(desc(count))%>%
  head(10)

#ques15,16--------------------------------------------------------

us.id=unique(train$User_ID) %in% unique(test$User_ID)
us.id2=unique(test$User_ID) %in% unique(train$User_ID)

#or
us.id=unique(merge(train,test,by='User_ID'))

#ques17---------------------------------------------------------

train=read.csv("train.csv")

train=train[1:100,]
test=read.csv("test.csv")
test=test[1:100,]
test$Purchase=0
library(sqldf)
require(sqldf)
# train.unique=sqldf("select distinct User_ID from train")
# test.unique=sqldf("select distinct User_ID from test")
# merge.unique=merge(train.unique,test.unique,by='User_ID')
# diff=setdiff(train.unique,merge.unique)
library(dplyr)

unique.train.diff=sqldf("SELECT User_ID FROM train EXCEPT SELECT User_ID FROM test")
sqldf("select * from train limit 10")
only.train=merge(train,unique.train.diff,by='User_ID')


only.train.purchase=sum(as.numeric(only.train$Purchase))/nrow(only.train)


#ques18---------------------------------------------------------

library(sqldf)
library(RODBC)
sql.df=sqldf("select User_ID,Product_ID from data group by User_ID")


#ques19---------------------------------------------------------
library(stringr)
dist.var.Purchase=function(df,colname){
  k=split(df,df[,colname])
  lis=vector()
  for(i in 1:length(k)){
    a=as.data.frame(k[i])
    for(cl in 1:length(colnames(a))){
      if(str_detect(colnames(a)[cl],"Purchase")==T){
        li=sum(a[,colnames(a)[cl]])/nrow(a)
        lis[i]=li
      }
    }
  }
  return(lis)
}

dist.var.Purchase(train,'City_Category')

cal.lift=function(vector){
  mi=min(vector)
  ma=max(vector)
  av=sum(vector)/length(vector)
  lift=(ma/av)-(mi/av)
  return(lift)
}

cal.lift(dist.var.Purchase(train,'City_Category'))

#ques20---------------------------------------------------------

freq=function(df,colname){
  return(table(df[,colname]))
}

x=freq(train,'Gender')
x

#ques21---------------------------------------------------------

createLevels=function(df,colname){
  
  temp=with(df,data.frame(model.matrix(~df[,colname]-1,df),df[,colname]))
  for (i in 2:length(names(temp))-1){
    p=paste(colname,strsplit(colnames(temp)[i],"df...colname.")[[1]][2],sep='_')
    colnames(temp)[i]<-p
  }
  names(temp)[length(names(temp))]=colname
  return(temp)
}

new.df=createLevels(train,'Gender')


