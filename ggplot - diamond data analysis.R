#q1
library(ggplot2)
library(dplyr)
diamonds =diamonds

#1
diamonds%>%
  group_by(cut) %>%
  summarise(avg_price=mean(price))
#2
diamonds%>%
  group_by(cut) %>%
  summarise(avg_price=mean(price)) %>%
  ggplot(aes(x=reorder(cut,-avg_price),y=avg_price,fill=cut))+
  geom_col()+
  xlab("Quality of cut")+
  ylab("Average Price in USD")+
  ggtitle("Average Diamond price by Cut")
#3
class(diamonds$price)
head(diamonds$price)

diamonds$pricecategory=ifelse(diamonds$price<=1000,"Low",
                              ifelse(diamonds$price<=2400,"Medium","High"))
head(diamonds$pricecategory)
diamonds$pricecategory=as.factor(diamonds$pricecategory)
class(diamonds$pricecategory)
levels(diamonds$pricecategory)
#4
ggplot(diamonds,aes(pricecategory))+
  geom_bar()
#5
diamonds$pricecategory=factor(diamonds$pricecategory,levels=c("Low","Medium","High"))
class(diamonds$pricecategory)
levels(diamonds$pricecategory)
ggplot(diamonds,aes(pricecategory))+
  geom_bar()
#6
head(faithful)
volcanos<-faithful
library(ggplot2)
ggplot(volcanos,aes(x=waiting))+
  geom_histogram(bins=10,fill="lightblue",color="black")+
  geom_freqpoly()
#7
install.packages("MASS")
library(MASS)
babies<-birthwt
?birthwt
babies$race=as.factor(babies$race)
class(babies$race)
levels(babies$race)=c("White","Black","Other")

#mother's race(1=white,2=black,3=others)
ggplot(babies,aes(x=race,y=bwt,fill=race))+
  geom_boxplot()+
  ggtitle("Babies'weight by race'")
#or
ggplot(babies,aes(bwt))+
  geom_histogram(fill="white",color="black")+
  facet_wrap(~race)+
  ggtitle("Babies'weight by race'")+
  coord_flip()
#8
ggplot(volcanos,aes(x=waiting,y=after_stat(density)))+
  geom_histogram(fill="lightblue",color="white")+
  geom_density()
?geom_density()
#9
ggplot(babies,aes(x=race,y=bwt))+
  geom_boxplot()+
  stat_summary(fun.y="mean",geom="point",shape=21,size=3,fill="red")
#10
ggplot(babies,aes(bwt))+
  geom_boxplot()+scale_y_continuous(breaks=Null)+coord_flip()+ylab("")
#line graphs
#11
ggplot(BOD,aes(x=Time,y=demand))+
  geom_line()
#12
ggplot(BOD,aes(x=factor(Time),y=demand,group=1))+
  geom_line()
#13
ggplot(BOD,aes(x=factor(Time),y=demand,group=1))+
  geom_line()+
  geom_point()+scale_x_discrete(name="",breaks=1:7,labels=paste("Min",1:7))+
  scale_y_continuous(name="Demand",breaks=8:20,labels=8:20)+
  geom_vline(xintercept=3,color="red",linetype="dashed")+theme_bw()
#14 population graph
library(gcookbook)
uspopage=uspopage
ggplot(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))+
  geom_area()
#15 convert thousands to percentage
uspopage_prop<- uspopage%>%
  group_by(Year)%>%
  mutate(Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prop,aes(x=Year,y=Percent,fill=AgeGroup,order=AgeGroup))+
  geom_area(alpha=0.8)+
  geom_line(position="stack",linewidth=0.1)
#line chart
tg <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(length = mean(len))
ggplot(tg,aes(x=factor(dose),y=length,group=supp,color=supp))+
  geom_line()