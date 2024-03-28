sleep=sleep
help(t.test)
#Q1
model1= t.test(extra~group,sleep)
model1

#Q2
model2=t.test(extra~group,sleep,var.equal=T) #a true independent t test
model2

#Q3
model3=t.test(extra~group,sleep,var.equal=T,paired=T) #a true dependent t test；paired means before and after
model3

#Q4
model4=t.test(sleep$extra[1:10],mu=2) #1 sample two sided test; mu: if sleep different from 2 extra hours
model4

#Q5
model5=t.test(sleep$extra[1:10],mu=2,alternative="greater") #this is one-tail upper tail test
model5

#Q6
model6=t.test(sleep$extra[1:10],mu=2,alternative="less") #this is one-tail lower tail test
model6

#Regression
install.packages("readr")
#install.packages("ggplot2")
#install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("ggfortify")

library(readr)
library(ggplot2)
library(broom)
library(dplyr)
library(ggpubr)
library(ggfortify)

HollywoodMovies2011 <-read.csv("HollywoodMovies2011.csv")

#Q2
colnames(HollywoodMovies2011)
str(HollywoodMovies2011)
head(HollywoodMovies2011)
summary(HollywoodMovies2011)

#Q3
hist(HollywoodMovies2011$DomesticGross)
boxplot(HollywoodMovies2011$DomesticGross)

#Q4
cor(HollywoodMovies2011$ProductionBudget,HollywoodMovies2011$DomesticGross)
cor(HollywoodMovies2011$AudienceScore,HollywoodMovies2011$DomesticGross)
cor(HollywoodMovies2011$RottenTomatoesScore,HollywoodMovies2011$DomesticGross)

#Q5
plot1=ggplot(data=HollywoodMovies2011,aes(x=ProductionBudget,y=DomesticGross))+
  geom_point()+
plot1

#Q6
linearmodel=lm(DomesticGross~ProductionBudget,HollywoodMovies2011)
summary(linearmodel)

#Q7
coeff=coefficients(linearmodel)
slope<-coeff[2]
intercept<-coeff[1]
plot2=plot1+geom_abline(intercept=intercept,slope=slope,color="red")
plot2

#Q8
res_linearmodel <- residuals.lm(linearmodel)
fitted_linearmodel <-fitted(linearmodel)
autoplot(linearmodel)
#Normal QQ: monkeys are monkeys
#Residual vs fitted: it's a linear model not bending
#Scale location: Monkeys are same strength (equal variance)

#Q9
MRmodel1=lm(DomesticGross~RottenTomatoesScore+AudienceScore+TheatersOpeningWknd+ProductionBudget,HollywoodMovies2011)
summary(MRmodel1)
R_sq=round(summary(MRmodel1)$r.squared,2)

#Q10
fitted_MRmodel1 <-fitted(MRmodel1)
plot3=ggplot(data=HollywoodMovies2011,aes(x=DomesticGross,y=fitted_MRmodel1))+geom_point()
plot3

plot4=ggplot(data=HollywoodMovies2011,aes(x=DomesticGross,y=fitted_MRmodel1))+geom_point()+xlim(0,400)+ylim(0,300)+
  geom_abline(intercept=0,slope=1,color="red")
plot4

#Q11
res_MRmodel1 <- residuals.lm(MRmodel1)
fitted_MRmodel1 <-fitted(MRmodel1)
autoplot(MRmodel1)

#Q12
MRmodel2=lm(DomesticGross~AudienceScore+TheatersOpeningWknd+ProductionBudget,HollywoodMovies2011)
summary(MRmodel2)
R_sq=round(summary(MRmodel2)$r.squared,2)

summary(MRmodel2)

#Q13
plot6=ggplot(data=HollywoodMovies2011,aes(x=DomesticGross,y=ForeignGross))+geom_point()
plot6+geom_smooth()
linearmodel2=lm(ForeignGross~DomesticGross,HollywoodMovies2011)
summary(linearmodel2)
autoplot(linearmodel2)
coeff=coefficients(linearmodel2)
slope<-coeff[2]
intercept<-coeff[1]
ggplot(data=HollywoodMovies2011,aes(x=DomesticGross,y=ForeignGross))+geom_point()
plot7=plot6+geom_abline(intercept=intercept,slope=slope,color="red")
plot7

#Q17
HollywoodMovies2011$DomesticGrossSq <- HollywoodMovies2011$DomesticGross^2
colnames(HollywoodMovies2011)
QRmodel1=lm(ForeignGross~DomesticGrossSq+DomesticGross,data=HollywoodMovies2011)
summary(QRmodel1)

fitted_QRmodel1 <-fitted(QRmodel1)
plot8=ggplot(data=HollywoodMovies2011,aes(x=ForeignGross,y=fitted_QRmodel1))+geom_point()
plot8
plot9=ggplot(data=HollywoodMovies2011,aes(x=ForeignGross,y=fitted_QRmodel1))+geom_point()+
  xlim(0,800)+ylim(0,800)+geom_abline(intercept=0,slope=1,color="red")
plot9
#Q18
res_QRmodel1 <- residuals.lm(QRmodel1)
fitted_QRmodel1 <-fitted(QRmodel1)
autoplot(QRmodel1)
