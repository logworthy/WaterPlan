
country <- unique(cities$country.etc)[
  sample(length(unique(cities$country.etc)), 1)
  ]
country.cities <- cities[country.etc == country]
extent.x <- max(country.cities$long) - min(country.cities$long)
extent.y <- max(country.cities$lat) - min(country.cities$lat)
rm(country)
ggplot(data=country.cities, aes(x=long, y=lat, label=name, color=as.factor(capital)))+geom_point()+
  scale_x_continuous(labels=NULL, limits=
                       c(min(country.cities$long)-extent.x
                         , max(country.cities$long)+extent.x
                       ))+
  scale_y_continuous(labels=NULL, limits=
                       c(min(country.cities$lat)-extent.y
                         , max(country.cities$lat)+extent.y
                       ))
rm(extent.x)
rm(extent.y)

unique(country.cities$country.etc) == 'Kosovo'
sum(country.cities$pop)






country <- unique(cities$country.etc)[
  sample(length(unique(cities$country.etc)), 1)
  ]
country.cities <- cities[country.etc == country]
extent.x <- max(country.cities$long) - min(country.cities$long)
extent.y <- max(country.cities$lat) - min(country.cities$lat)
rm(country)
ggplot(data=country.cities, aes(x=long, y=lat, label=name, color=as.factor(capital)))+geom_point()+
  scale_x_continuous(labels=NULL, limits=
                       c(-180
                         , 180
                       ))+
  scale_y_continuous(labels=NULL, limits=
                       c(-90
                         , 90
                       ))
rm(extent.x)
rm(extent.y)

unique(country.cities$country.etc) == ''
sum(country.cities$pop)