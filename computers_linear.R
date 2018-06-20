
#install.packages("Ecdat")

library(Ecdat)

data("Computers")

str(Computers)

sum(is.na(Computers))

boxplot(Computers$price)

hist(Computers$price,col = "BLUE")

library(corrplot)

cr = Computers[-c(1,6,7,8,9)]

cor(cr)


library(caTools)

split = sample.split(Y = Computers$price,SplitRatio = 0.8)

train = subset(Computers,split = TRUE)

test = subset(Computers,split = FALSE)



### model



pricemodel = lm(price~.-ram,data = train)

summary(pricemodel)

#model diagnostics 

library(MASS)

names(pricemodel)

res = stdres(pricemodel)

pred = pricemodel$fitted.values

plot(pred,res)

hist(res)# error should follow normal distribution

plot(pricemodel,which = 2) # checking normal distribution with qq plot


Computers$price = log(Computers$price)



library(caTools)

split = sample.split(Y = Computers$price,SplitRatio = 0.8)

train = subset(Computers,split = TRUE)

test = subset(Computers,split = FALSE)


logmodel = lm(price~.-ram,data = train)

summary(logmodel)

## model diagnostics

logres = stdres(logmodel)
logpred = logmodel$fitted.values

plot(logpred,logres)

plot(logmodel,which = 2) # checking normal distribution with qq plot

plot(logmodel,which = 1) # hetrosc. & autocorrelation

## outlier test 

library(car)

outlierTest(logmodel)

## check leverages

cd = cooks.distance(logmodel)

cutoff = 4/(nrow(train) -9-1)
summary(logmodel)

plot(logmodel,which = 4,cutoff=cutoff)


### remove outliers and influencer

train = train[-c(4478,3784,5961),]

model = lm(price~.-ram,data = train)

lgres = stdres(model)

lgpred = model$fitted.values

plot(lgpred,lgres)

## outlier leverage 

cutoff = 4/(nrow(train) -8-1)

plot(model,which = 4,cutoff=cutoff)

plot(model)

vif(model)

sqrt(vif(model))>2

## multicollinearity 

#vif(lm(hd~ram+screen,data = model))

### prediction




