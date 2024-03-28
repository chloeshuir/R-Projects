#Scatterplots
#1. Load the ggplot2 package into R.----
library(ggplot2)
#2. Use the mpg dataset that comes with the ggplot2 package, and create a scatterplot that shows the ----
#relationship between the engine displacement displ and the highway miles per gallon variable hwy.----
#3. Use color to distinguish between different types (class) of cars in the previous plot.----
mpg=mpg
str(mpg)
head(mpg)
ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point(aes(color=class))
#or
ggplot(data=mpg,aes(x=displ,y=hwy,color=class))+
  geom_point()

#4. In the previous plot, we assigned a categorical variable to the color aesthetics. In this exercise, try to
#add the color aesthetics to the geom_point() line of code. Explain what happened.----
ggplot(data=mpg,aes(x=displ,y=hwy,color=class))+
  geom_point(color="red")
#This will not work because R follows the latest commands

#5. Use the size aesthetic to show the different types of cars (class) instead of the color aesthetic.----
ggplot(data=mpg,aes(x=displ,y=hwy,size=class,color=class))+
  geom_point()
#Facets
#6. Use the function facet_wrap() to show the relationship between displ and hwy for each of the car types (class) in a separate graph.----
ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point()+
  facet_wrap(~class,nrow=2)
ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point()+
  facet_wrap(cyl~class)
  
#Bar Graphs
#7. Use the str() function to explore the diamonds dataset that comes with ggplot2 package.----
diamonds=diamonds
str(diamonds)
#8. Create a barchart that shows the count distribution for each of the cut levels of the diamonds.----
ggplot(data=diamonds,aes(x=cut))+
  geom_bar()
#9. Check what happens if you try to create a count bar chart for the variable carat.----
ggplot(data=diamonds,aes(x=carat))+
  geom_bar()
#extra
ggplot(data=diamonds,aes(x=carat))+
  geom_histogram(bins=10)+
  geom_freqpoly(color="red")

ggplot(data=diamonds,aes(x=carat))+
  geom_boxplot()
    
#10. Load the gcookbook package to R.
install.packages("gcookbook")
library(gcookbook)
#11. Investigate the contents of the BOD dataset that comes with gcookbook R package.
BOD=BOD
str(BOD)
#12. Create a bar chart that shows you the biochemical oxygen demand for each time in an evaluation of water quality.
ggplot(data=BOD,aes(x=Time,y=demand))+
  geom_col()
#when use geom_bar, we input only one variable, it autimatically gives you column,while in col(), we input two variables
#13. How can you improve the previous graph?
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
  geom_col()
#no blank values; when we do factor(),we categorize variables
#14. Change the width of each bar in the previous graph into 0.5.
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
  geom_col(width=0.5)
#bar chart has gaps in between, if it's a histogram, there's no gap unless there is gap in data
#any data beyond gaps in histogram is potential outliers
#15. Change the color of the bars in the previous graph into lightblue
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
geom_col(fill="lightblue",color="red")
#Adding Labels to a Bar Graph
#To do so, we will add a new layer to our plot using geom_text().
#16. For the BOD barchart created before, add the oxygen demand value on the bars for each of theTimes.
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
  geom_col(fill="lightblue",color="red")+
  geom_text(aes(label=demand))
#adjust label position
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
  geom_col(fill="lightblue",color="red")+
  geom_text(aes(label=demand),vjust=-1,color="red")
#Horizontal Bar Charts----
#17. Create a horizontal barchart version of the BOD barchart using the coord_flip() function.----
ggplot(data=BOD,aes(x=factor(Time),y=demand))+
  geom_col(width=0.5)+
  coord_flip()
#Grouping Bars Together
#18. Create a bargraph that shows how each cut level in the diamonds dataset is distributed among the different clarity levels for the diamonds.
#Using Colors in a Bar Graph
#use fill to add another dimension
ggplot(diamonds,aes(x=cut,fill=clarity))+
  geom_bar(position="dodge",color="black")
#other way
ggplot(diamonds,aes(x=cut,fill=clarity))+
  geom_bar()
#how each clarity level in the diamonds dataset is distributed among the different cut levels 
ggplot(diamonds,aes(x=clarity,fill=cut))+
  geom_bar(position="dodge",color="black")
#19. Exlpore the uspopchange dataset that comes with the gcookbook R package and create a barchart
#that shows the % change in population for the top 10 states. The bars should be colored based on
#the region of each state (default colors).
str(uspopchange)
sorted_uspopchange=uspopchange[order(-uspopchange$Change),]
top10=sorted_uspopchange[1:10,]
ggplot(top10,aes(x=Abb,y=Change,fill=Region))+
  geom_col()

#20. Use the scale_fill_manual() function to update the colors of the previous chart as follows: South
#(pink), and West (lightblue).
ggplot(top10,aes(x=Abb,y=Change,fill=Region))+
  geom_col()+
  scale_fill_manual(values=c("pink","lightblue"))+
  xlab("State")
#21. Reorder the bars in the previous barchart to decreasing order. Cleveland Dot Plot or Dot Plots
ggplot(top10,aes(x=reorder(Abb,-Change),y=Change,fill=Region))+
  geom_col()+
  scale_fill_manual(values=c("red","lightblue"))+
  xlab("State")
#22. Explore the tophitters2001 dataset that comes in the gcookbook R package.
str(tophitters2001)
#23. Create a dotplot that shows the name as well as the average batting score for the top 10 players.
sorted_tophitters=tophitters2001[order(-tophitters2001$avg),]
top10=sorted_tophitters[1:10,]
#or
#top10<-tophitters2001[1:10,c("name","avg")]
ggplot(top10,aes(x=avg,y=reorder(name,avg)))+
  geom_point(size=3)