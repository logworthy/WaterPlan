# main analysis file

install.packages('maps')
install.packages('alphahull') # had to update to 3.2.2 :/
#install.packages('spatstat')
library(maps)
library(data.table)
library(ggplot2)
library(plyr)
library(alphahull)

# load data into project environment
load.envir <- new.env()
loaded <- data(world.cities, envir=load.envir)
cities <- data.table(get(loaded, load.envir))

# our area of interest
india <- cities[country.etc == 'India']
summary(india)

# initial plot 
ggplot(data=india, aes(x=long, y=lat, label=name))+geom_point()

# exclude port blair
india <- india[name != 'Port Blair']

# find the hull
india.hull <- india[chull(india$long, india$lat)]
ggplot(data=india, aes(x=long, y=lat, label=name))+geom_point()+
  geom_path(data=rbind(india.hull, india.hull[1,]), color='red')

india.ahull <- ahull


# function to find new points within a hull
new_point <- function(x, y) {
  points <- data.table(x,y)
  null.hull <- chull(x, y)
  not.found <- T
  while(not.found) {
    x1 <- runif(1, min=min(x), max=max(x))
    y1 <- runif(1, min=min(y), max=max(y))
    new.hull <- chull(c(x,x1), c(y,y1))
    
    # point fails test if
    #  - length of new hull is not equal to length of old hull
    #  - any point in the new hull is not a point in the old hull
    not.found <- if(length(new.hull) != length(null.hull)) T else
      sum(new.hull != null.hull) 
  }
  return(c(x=x1,y=y1))
}

# randomly generate 10 candidate plant locations
plants <- ldply(
  .data=1:10
  , .fun=function(x) {
    point <- new_point(india.hull$long, india.hull$lat)
    data.frame(long=point["x"], lat=point["y"])
  }
)

# view the new map & our candidates
ggplot(data=india, aes(x=long, y=lat))+
  geom_point(aes(color="city"))+
  geom_point(data=plants, aes(color="candidate plant"))



