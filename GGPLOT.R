#install.packages("ggplot2")
library(ggplot2)
data(mpg)
head(mpg)


###### base plotting


# on cont. variable --- histogram


hist(mpg$cty,breaks = 10)### break function is used to decrease bin size and increase bars.
###easy to visualize.... max - min / 10 = bin size

summary(mpg$cty)

###box plot for one continous variable
 
boxplot(mpg$cty)

#########discrete variable or factor variable######

table(mpg$drv)



barplot(table(mpg$drv))## by default it gives grey color shape


##piechart


pie(table(mpg$drv)) # not suggested always go for barplot



getwd()






a = c(10,20,30,40,50)
b = c(101,208,308,405,506)


plot(a,b)


plot(a,b,type = "l") # "l" represents line.

###two factor variables


barplot(table(mpg$class,mpg$drv))

##### ONE variable (cont.)

ggplot(mpg,aes(cty)) + geom_histogram(binwidth = 2)

###box plot

ggplot(mpg,aes(1,cty)) + geom_boxplot() #"1" is for single variable since it takes 2 variable


#bar chart for one factor variable

ggplot(mpg,aes(class)) + geom_bar()



#two cont. variable

ggplot(mpg,aes(cty,hwy)) + geom_point()


#one cont and other factor

ggplot(mpg,aes(class,cty))+geom_boxplot()


ggplot(mpg,aes(drv,cty))+geom_boxplot()


###two factor variables


ggplot(mpg,aes(manufacturer,fill = class))+geom_bar()



ggplot(mpg,aes(class,fill = drv))+geom_bar()


######### multi dimensional plots



ggplot(mpg,aes(cty ,hwy ,shape = drv , color = class)) + geom_point()


#adding size
#size factor should be ordinal value

 gg = ggplot(mpg,aes(cty ,hwy ,shape = drv , color = class,size = cyl)) + geom_point()



#facets to add more variable to the plot
#for facet we will always use factor variable else it wont take it.
 
gg + facet_grid(fl ~ .)


gg + facet_grid(.~fl)

###gg + facet_grid(year~fl)

  ### we can add max 7 variables to the same plot.




bgg = ggplot(mpg,aes(cty,hwy))+geom_point()+geom_smooth(method = "lm")

bcc= bgg + ggtitle("city vs highway mileage") + xlab("CITY MILEAGE") + ylab("HIGHWAY MILEAGE")


#bcc + coord_cartesian(xlim = c(1),30) + ylim = c(10,35))

#xlim and ylim function is  used to seperate overlapping of data for readability

#themes() is used to change the theme of plots.



### adjusting positions..........



ggplot(mpg,aes(cty ,hwy ,shape = drv , color = class)) + geom_point(position = "dodge")+theme_bw()




ggplot(mpg,aes(class,fill = drv))+geom_bar(position = "fill")# provides % value


ggsave("plot.pdf") # to save the plots......... in server......