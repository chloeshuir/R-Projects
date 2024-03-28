#The Five Verbs of dplyr
#1. The dplyr package contains five key data manipulation functions, also called verbs:
#What order of operations should we use to find the average value of the ArrDelay (arrival delay) variable
#for all American Airline flights in the hflights tbl; Manipulating Variables (Select and Mutate)
#The order is filter() and then summarise()
#2. Return a copy of hflights that contains the four columns related to delay (ActualElapsedTime, AirTime, ArrDelay, DepDelay)----.
install.packages("dplyr")
library(dplyr)
install.packages("hflights")
library(hflights)
df1=hflights
str(hflights)
df2=select(hflights,ActualElapsedTime, AirTime,ArrDelay,DepDelay)
head(df2)
#3. Return a copy of hflights containing the columns Origin up to Cancelled.
df3=select(hflights,Origin:Cancelled)
#4. Find the most concise way to select: columns Year up to and including DayOfWeek, columns ArrDelay up to and including Diverted.
df4=select(hflights,-(DepTime:AirTime))
colnames(df4)
#5. dplyr comes with a set of helper functions that can help you select variables. These functions find groups of variables to select, based on their names. dplyr provides 6 helper functions, each of which 
#only works when used inside select().
#• starts_with(“X”): every name that starts with “X”,
#• ends_with(“X”): every name that ends with “X”,
#• contains(“X”): every name that contains “X”,
#• matches(“X”): every name that matches “X”, which can be a regular expression,
#• num_range(“x”, 1:5): the variables named x01, x02, x03, x04 and x05,
#• one_of(x): every name that appears in x, which should be a character vector.
#Use select and a helper function to return a tbl copy of hflights that contains just ArrDelay and DepDelay.
df5=select(hflights,ends_with("Delay"))
head(df5)
#6. Use a combination of helper functions and variable names to return the UniqueCarrier, FlightNum,
#TailNum, Cancelled, and CancellationCode columns of hflights.
df6=select(hflights,UniqueCarrier,ends_with("Num"),starts_with("Cancel"))
colnames(df6)
#7. Which variables in hflight do you think count as a plane’s “ground time”? Use mutate() to add these
#variables together and save them as GroundTime.
df7=mutate(hflights,GroundTime=TaxiIn+TaxiOut)
head(df7)
str(df7)
#Manipulating Observations (Filter and Arrange)
#When manipulating observations, we should know the following operations:
#• x < y, TRUE if x is less than y
#• x <= y, TRUE if x is less than or equal to y
#• x == y, TRUE if x equals y
#• x != y, TRUE if x does not equal y
#• x >= y, TRUE if x is greater than or equal to y
#• x > y, TRUE if x is greater than y
#• x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)
#8. Return a copy of all flights that traveled 3000 miles or more.
df8=filter(hflights,Distance>=3000)
min(df8$Distance)
#9. Return a copy of all flights flown by one of American(AA), Alaska (AS), or JetBlue (B6) airlines.
df9=filter(hflights,UniqueCarrier %in% c("AA","AS","B6"))
#or:
df9=filter(hflights,UniqueCarrier=="AA" | UniqueCarrier=="AS" | UniqueCarrier=="B6")

#10. Return a copy of all flights where taxi-ing took longer than flying.
df10=filter(df7,GroundTime>AirTime)
#or
df10=filter(hflights,TaxiIn+TaxiOut>AirTime)
head(df10)
#11. Return a copy of all cancelled weekend flights
df11=filter(hflights,DayOfWeek %in% c(6,7) & Cancelled ==1)
head(df11)
unique(df11$DayOfWeek)
#12. Arrange according to carrier and decreasing departure delays.
df12=arrange(hflights,UniqueCarrier,desc(DepDelay))
head(df12)
#13. Arrange flights by total delay (normal order).
df13=arrange(hflights,(ArrDelay + DepDelay))
head(df13)
tail(df13)
#14. Filter out flights leaving to DFW before 8am and arrange according to decreasing AirTime
df14=arrange(filter(hflights,Dest=="DFW" & DepTime < 800),desc(AirTime))
head(df14)
df14a=select(df14,Dest,DepTime)
#Manipulating Groups of Observation (summarize and group_by)
#15. Determine the shortest and longest distance flown and save statistics to min_dist and max_dist resp.
df15 = hflights%>%
  summarize(min_distance=min(Distance),max_distance=max(Distance))
head(df15)
#16. Determine the longest distance for diverted flights, save statistic to max_div.
df16 = hflights%>%
  filter(Diverted==1)%>%
  summarize(max_div=max(Distance,na.rm=T))
head(df16)
#17. dplyr provides several helpful aggregate functions of its own, in addition to the ones that are already
#defined in R. These include:
#• first(x) - The first element of vector x.
#• last(x) - The last element of vector x.
#• nth(x, n) - The nth element of vector x.
#• n() - The number of rows in the data.frame or group of observations that summarise() describes.
#• n_distinct(x) - The number of unique values in vector x.
#Create a table with the following variables (and variable names): the total number of observations in hflights
#(n_obs), the total number of carriers that appear in hflights (n_carrier), the total number of destinations
#that appear in hflights (n_dest), and the destination of the flight that appears in the 100th row of hflights
#(dest100).
df17=hflights%>%
  summarise(n_obs=n(),n_carrier=n_distinct(UniqueCarrier),
            n_dest=n_distinct(Dest),
            dest100=nth(Dest,100))
head(df17)
#18. Use Piping: (1) Take the hflights data set and then, (2) Add a variable named diff that is the resultof
#subtracting TaxiIn from TaxiOut, and then (3) pick all of the rows whose diff value does not equal NA,
#and then (4) summarise the data set with a value named avg that is the mean diff value.
df18=hflights%>%
  mutate(diff=TaxiIn-TaxiOut)%>%
  filter(!is.na(diff))%>%
  summarise(avg=mean(diff))
head(df18)
#19. Use Piping: Define a data set named d that contains just the Dest, UniqueCarrier, Distance, and
#ActualElapsedTime columns of hflights as well an additional variable: RealTime which is equal the
#actual elapsed time plus 100 minute.
d=hflights%>%
  mutate(RealTime=ActualElapsedTime+100)%>%
  select(Dest,UniqueCarrier,Distance,ActualElapsedTime,RealTime)
head(d)
#20. For each destination, find the number of flights,the mean distance travelled, and the mean arrival delay
#and investigate visually if there is any relationship between distance travelled and arrival delays. You
#can get rid of the outliers if any.
library(ggplot2)
hflights%>%
  group_by(Dest)%>%
  summarize(count=n(),dist=mean(Distance,na.rm = T),delay=mean(ArrDelay,na.rm=T))%>%
  filter(dist<2000)%>%
  ggplot(aes(dist,delay))+geom_point()+geom_smooth(method=lm)



