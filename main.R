# main analysis file

install.packages('maps')
install.packages('alphahull') # had to update to 3.2.2 :/
install.packages('Imap')
#install.packages('spatstat')
library(maps)
library(data.table)
library(ggplot2)
library(plyr)
library(alphahull)
library(Imap)

source('GeoDistanceMatrix.R')

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

# alpha=2 seems about right
india.ahull <- ahull(india$long, india$lat, alpha=2)
ggplot()+geom_point(data=india, aes(x=long, y=lat, label=name))+
  geom_segment(data=data.table(india.ahull$ashape.obj$edges), aes(x=x1,xend=x2, y=y1,yend=y2), color='red')

# sort the edges
edges <- data.table(india.ahull$ashape.obj$edges)
setkey(edges, ind1)
counter <- 1
edges[1, ord := counter]
edges[1, src := ind1]
edges[1, dst := ind2]
while(sum(is.na(edges$ord))) {
  last.row <- edges[,which(ord==counter)]
  src.id <- edges[last.row, src]
  dst.id <- edges[last.row, dst]
  counter <- counter + 1
  next.row <- edges[,which(is.na(ord) & (ind1 == dst.id | ind2 == dst.id))]
  if (edges[next.row, ind1] == dst.id) {
    edges[next.row, ord := counter]
    edges[next.row, src := ind1]
    edges[next.row, dst := ind2]
  } else {
    edges[next.row, ord := counter]
    edges[next.row, src := ind2]
    edges[next.row, dst := ind1]
  }
}

setkey(edges, ord)
edges
ggplot(edges, aes(x=x1,y=y1))+geom_path()

# should now be able to test some points
# point.in.polygon(80, 20, edges$x1, edges$y1) # T as expected
# point.in.polygon(80, 30, edges$x1, edges$y1) # F as expected
# point.in.polygon(85, 15, edges$x1, edges$y1) # F as expected
# point.in.polygon(77, 10, edges$x1, edges$y1) # T as expected

# function to find new points within a hull
new_point <- function(x, y) {
  points <- data.table(x,y)
  null.hull <- chull(x, y)
  not.found <- T
  while(not.found) {
    x1 <- runif(1, min=min(x), max=max(x))
    y1 <- runif(1, min=min(y), max=max(y))
    not.found <- !point.in.polygon(x1, y1, x, y)
  }
  return(c(x=x1,y=y1))
}

# randomly generate 10 candidate plant locations
plants <- ldply(
  .data=1:10
  , .fun=function(x) {
    point <- new_point(edges$x1, edges$y1)
    data.frame(long=point["x"], lat=point["y"])
  }
)

# view the new map & our candidates
ggplot(data=india, aes(x=long, y=lat))+
  geom_point(aes(color="city"))+
  geom_point(data=plants, aes(color="candidate plant"))+
  geom_path(data=edges, aes(x=x1,y=y1), color='red')

# okay have candidate plants
- generate build cost, fixed & marginal operating costs, capacity for each plant
- get distance matrix for each city to city and plant to city
- add demand parameters for each city (assume inelastic demand?)
- generate problem

obj:
--- maximize water_consumed:  sum(c in cities) consumption[c]

var:
--- build(p in plants) binary
--- plant2city(p in plants, c in cities) numeric/flow >= 0 
--- city2city(a in cities, b in cities) numeric/flow >= 0 # no reverse flow
--- consumption[c in cities] numeric >= 0
--- production[p in plants] numeric >= 0
--- price numeric >= 0 # how much to sell 1 unit of water for

constraint:
  
a plant can never deliver more than it produces
--- for every i in plants, sum(c in cities) plant2city[i,c] <= production[i]
a plant cannot produce if not built and can never produce more than it has capacity
--- for every i in plants, production[i]<= build[i] * capacity[i]

output of a city <= city input + plant input - consumption
--- for every city in cities
  sum(a in cities a = city, b in cities) city2city[a,b] <= 
  sum(i in plants, c in cities c=city) plant2city[i,c] + 
  sum(d in cities, e in cities e=city) city2city[d,e] -
  consumption[city]

consumption of a city cannot exceed demand
--- for every city in cities
  consumption[city] <= demand[city]

breakeven constraint
operating cost + delivery cost <= price * consumption
--- sum(p in plants) op_fixed_cost[p] + op_var_cost[p] * production[p] +
  sum(a in cities, b in cities) city2city[a,b] * city2city_cost[a,b] <= price * sum(c in cities) consumption[c]

param:
--- capacity(p in plants)  # how much capacity does each plant have
--- op_fixed_cost(p in plants)
--- op_var_cost(p in plants)
--- plant2city_cost(p in plants, c in cities) # 10 x 900 matrix
--- city2city_cost(a in cities, b in cities) # 900 x 900 matrix



  
  

# monthly running costs
# http://www.costwater.com/runningcostwater.htm
  prod <- 100000

  #variable (water, electricity, chemicals)
  0.07*prod*30
  0.17*0.1*prod*30
  (0.02*1.5+0.003*1)*prod*30

  ((0.07)+(0.17*0.1)+(0.02*1.5+0.003*1))

  # maintenance (1% of construction cost pa)
  400*0.01/12*prod

  # fixed
  24*2500
  24*2500*0.5
  

# construction costs
# http://www.costwater.com/watertreatment.htm
  #capacity   unit cost  total cost
  3785    572   572*3785
  7570	  432
  18925	  297
  37850	  224
  75700	  169
  189250	116
  378500	88

# lets approximate this
  build_costs <- data.table(
    unit_cost=c(
      572 
      , 432
      , 297
      , 224
      , 169
      , 116
      , 88
    )
    ,capacity=c(
      3785
      ,7570
      ,18925
      ,37850
      ,75700
      ,189250
      ,378500
    )
    , total_cost=c(
      3785 * 572
      ,7570 * 432
      ,18925 * 297
      ,37850 * 224
      ,75700 * 169
      ,189250 * 116
      ,378500 * 88
    )
  )

  ggplot(build_costs)+
  geom_point(aes(x=capacity, y=total_cost))+
  geom_segment(data=data.table(
    x=c(3785,18925,75700)
    , xend=c(18925,75700,378500)
    ,y=c(3785 * 572,18925 * 297,75700 * 169)
    , yend=c(18925 * 297,75700 * 169,378500 * 88)
    , color=c('tier1', 'tier2', 'tier3')
  ), aes(x=x,xend=xend,y=y,yend=yend,color=color))

# this will undercook the cost...  its actually just as feasible to make every segment a point
# let's also make 3785 the minimum capacity just to be safe
# and maybe 378500 the max

# find a skewed distribution with ~ 75750 as the mean
  ggplot(data=data.table(x=75000*rnorm(300)**2))+geom_density(aes(x=x))
  summary(75000*rnorm(30)**2)

  skew.dist <- function(min, max, mean) {
    out.of.bounds <- T
    while(out.of.bounds) {
      var <- mean*rnorm(1)**2
      out.of.bounds <- (var > max | var < min)
    }
    return(var)
  }

  plants[,'capacity'] <- round(sapply(FUN=function(x) skew.dist(3785, 378500, 75700), 1:10))
  

# costs will take the form:
  build_costs[,'name'] <- paste0('tier', 1:nrow(build_costs))
  build_costs[,'base'] <- c(0, build_costs[,'capacity'])[1:nrow(build_costs)]
  build_costs[,'capacity'] <- build_costs[,'capacity']-1
  tier1 : 2165020+572*(3785-capacity)
  tier2 : 3270240+432*(7570-capacity)


# transport costs...  hard
  #http://onlinelibrary.wiley.com/doi/10.1029/2004WR003749/full
  6.1 cents / m3 for 100km for 100million m3
  5.3 cents / m3 for 100km for 500million m3
  6c / 100km + 5c / 100m vertical
  
  # for every 1% extension of capacity, total costs increase by 0.92% and unit costs fall by 0.08%

  # make a distance matrix
  #http://eurekastatistics.com/calculating-a-distance-matrix-for-geographic-points-using-r
  plants[,'name'] <- paste('Water Plant', 1:10)
  india

  # bind plants & cities together (we dont need plant x plant connections but whatever)
  dist_matrix <- rbind(
    plants[,c('name', 'lat', 'long')]
    , india[,c('name', 'lat', 'long'),with=F]
  )

  # need a 'lon' column for the func to work
  setnames(dist_matrix, c('long'), c('lon'))

  # takes a couple of minutes
  system.time({
  geo_dist_matrix <- GeoDistanceInMetresMatrix(data.frame(dist_matrix))
  })

  # 6c per 100km
  # this needs to be multiplied by m3 volume
  geo_cost_matrix <- 0.06 * geo_dist_matrix / 100000
  head(geo_cost_matrix)

  # 1 litre = 0.001 cubic metres
  
  sample_cities <- sample(nrow(india), 10)
  sample_full <- india[sample_cities,]
  sample_full[,'demand'] <- sample_full[,'pop'] * 100 * 0.001 # demand in cubic m
  sample_names <- india[sample_cities,'name']
  sample_cost_matrix <- geo_cost_matrix[sample_names,sample_names]
  sample_cost_long <- expand.grid(row=rownames(sample_cost_matrix),col=colnames(sample_cost_matrix))
  sample_cost_vec <- as.vector(sample_cost_matrix)

  plant2city <- geo_cost_matrix[plants[,'name'], sample_names]
  plant2city_long <-  expand.grid(row=rownames(plant2city),col=colnames(plant2city))
  plant2city_vec <- as.vector(plant2city)

  # try writing data for first 10 cities
  problem_data <- sprintf("set CITIES := %s;", paste(sprintf('"%s"', sample_names), collapse=','))
  problem_data <- paste(problem_data, sprintf("set PLANTS := %s;", paste(sprintf('"%s"', plants[,'name']), collapse=',')), sep='\n')
  problem_data <- paste(problem_data, sprintf("set TIERS := %s;", paste(sprintf('"%s"', build_costs[,'name']), collapse=',')), sep='\n')
  problem_data <- paste(problem_data, sprintf("param build_max_capacity := %s;"
    , paste(sprintf('[%s] %s', build_costs[,'name'], build_costs[,'capacity']), collapse='\n')
    )
      , sep='\n'
  )
  problem_data <- paste(problem_data, sprintf("param build_base_capacity := %s;"
                                              , paste(sprintf('[%s] %s', build_costs[,'name'], build_costs[,'base']), collapse='\n')
  )
  , sep='\n'
  )
  problem_data <- paste(problem_data, sprintf("param capacity := %s;"
                                              , paste(sprintf('["%s"] %s', plants[,'name'], plants[,'capacity']), collapse='\n')
  )
  , sep='\n'
  )
  problem_data <- paste(problem_data, sprintf("param city2city_cost := %s;"
                                              , paste(sprintf('["%s","%s"] %s'
                                                              , sample_cost_long[,'row']
                                                              , sample_cost_long[,'col']
                                                              , sample_cost_vec), collapse='\n')
  )
  , sep='\n'
  )
  problem_data <- paste(problem_data, sprintf("param demand := %s;"
                                              , paste(sprintf('["%s"] %s', sample_full[,'name'], sample_full[,'demand']), collapse='\n')
  )
  , sep='\n'
  )
  problem_data <- paste(problem_data, sprintf("param plant2city_cost := %s;"
                                              , paste(sprintf('["%s","%s"] %s'
                                                              , plant2city_long[,'row']
                                                              , plant2city_long[,'col']
                                                              , plant2city_vec), collapse='\n')
  )
  , sep='\n'
  )
  problem_data <- paste(problem_data, 'end;', sep='\n')
  write(file="Problem.dat", problem_data)





as.vector(sample_cost_matrix)
rownames(sample_cost_matrix)
colnames(sample_cost_matrix)
expand.grid(rownames(sample_cost_matrix),colnames(sample_cost_matrix))