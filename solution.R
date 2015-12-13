library(stringr)

lp.read_solution <- function(solution, varnames, cols) {
  solution.lines <- readLines(solution)
  solution.list <- list()
  
  for(i in varnames) {
    solution.list[[i]] <- list()
  }
  
  #iterate over solution
  for(i in 1:length(solution.lines)) {
    
    #if the line doesn't contain a relevant variable, skip it
    test <- str_detect(solution.lines[[i]], varnames)
    if(sum(test) == 0) next

    vals <- str_match(solution.lines[[i]], '\\[(.*?)\\]')[[2]]
    tc1 <- textConnection(vals)
    valdata <- data.table(read.csv(tc1, header=F))
    close(tc1)
    setnames(valdata, cols[[which(test)]])
    activity <- str_match(solution.lines[[i+1]], '\\s*(-?\\d+\\.?d*)')[[2]]
    valdata[, value := activity]
    chosen_var <- varnames[[which(test)]]
    solution.list[[chosen_var]][[length(solution.list[[chosen_var]])+1]] <- valdata
  }
  
  for(i in varnames) {
    solution.list[[i]] <- Reduce(rbind, solution.list[[i]])
  }
  
  return(solution.list)
  
}


sol <- lp.read_solution(
  "Problem.sol"
  , varnames = c(
    'plant2city'
    , 'city2city'
  )
  , cols=list(
    c('plant_from', 'city_to')
    , c('city_from', 'city_to')
  )
)

sol[["city2city"]][value > 0]

routes <- sol[["plant2city"]][value > 0]
routes[, plant_from := gsub(x=plant_from, '\'', '')]
plants <- data.table(plants)
setkeyv(plants, 'name')
setkeyv(routes, 'plant_from')
routes <- plants[routes]
setkeyv(india, 'name')
setkeyv(routes, 'city_to')
routes <- india[routes]

ggplot(data=india[name %in% sample_names], aes(x=long, y=lat))+
  geom_point(aes(color="city"))+
  geom_point(data=plants, aes(color="candidate plant"))+
  geom_path(data=edges, aes(x=x1,y=y1), color='green')+
  geom_segment(data=routes, aes(y=i.lat, yend=lat, x=i.long, xend=long))

# clearly a problem with the pipe costs
# maybe volume weighting isn't the correct approach...
