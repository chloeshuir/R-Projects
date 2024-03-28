#install.packages("ggplot2")
#install.packages("dplyr")
install.packages("maps")
install.packages("mapproj")
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)

#1
states_map = map_data("state")
#2
#a. Try it with geom_points option
ggplot(states_map, aes(x = long, y = lat)) + geom_point()
#b. Try it with  geom_polygon option
states_map = map_data("state")
ggplot(states_map, aes(x = long, y = lat)) + geom_polygon()
#c. Try it with geom_polygon and use “group = group” option in ggplot
ggplot(states_map, aes(x = long, y = lat, group = group)) + geom_polygon()
#however, can't see the states
#d.
ggplot(states_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color ="black") + 
  coord_map("polyconic")+ 
  theme_void() #removing any background or grid lines from the plot
#try other types of map projection
ggplot(states_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color ="black") + 
  coord_map("gilbert")+ 
  theme_void()
#Q1. Which gives you the best US plot among the above options? Is anything missing?
  #Q1 Deliverable: State your answer in the comment.
##the polyconic one, because it' s not a flat map, it projects the map 3D not 2D, which is more accurate. But it's missing Hawaii and Alaska
#3. Create the complete USA map using ggplot2. 
install.packages("usmap")
library(usmap) #import the package
plot_usmap(regions = "states") +
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") +
  theme(panel.background=element_blank())
#Q2. Is this US STATES map better? What is the issue in terms of mapping?
  #Q2 Deliverable: State your answer in the comment.
#it's better as it adds the missing states,but missing states names, and it would be better if we can color code different states based on varies data

#4. Note: The maps data available in ggplot2: world, (outline of the USA), state (each state in the USA),
#and county (each county in the USA).
#Create the world map.
world_map= map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "dark green", color = "blue")+
  coord_map("polyconic")+ 
  theme_void()

world_map= map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "dark green", color = "blue")+
  coord_map("gilbert")+ 
  theme_void()

world_map= map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "light blue", color = "white")+
  coord_map("azequalarea")+ #projecting as it is in a 3D scale
  theme_void()
#5. Create a map of Spain.
spain = map_data("world", region = "Spain")
ggplot(spain, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") +
  coord_map("polyconic")

#6. Create a map of five Latin American countries (“Chile”, “Colombia”, “Argentina”, “Ecuador”, “Brazil”. 
LA_map = map_data("world", region = c("Chile", "Colombia", "Argentina", "Ecuador", "Brazil","India"))
ggplot(LA_map, aes(long, lat, group = group)) + geom_polygon(fill = "yellow", color = "salmon") +
  coord_map("polyconic")
#Creating a Choropleth Map
#7. Use the USArrests dataset to create a choropleth map that shows the number of arrests for assault in
#each state in the USA. 
# Check the rownames
Crime=USArrests
?USArrests
rownames(Crime)
# Check the States names
    #the lower and upper case in two dataset don't match
# fix the issue in names using “tolower” case names
tolower(rownames(Crime)) # for consistency in joins
# Create new_crime Dataframe using mutate Command
Crime=USArrests
new_crime = Crime %>%
  mutate(state = tolower(rownames(Crime)))
# Use “left_join” command to create New_crime_map_df
New_crime_map_df = left_join(new_crime, states_map, by = c("state" = "region"))
ggplot(New_crime_map_df, aes(long, lat, group = group, fill = Assault)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "red")

