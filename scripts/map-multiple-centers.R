# Map centers & radius
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-12-16

rm(list = ls())

################################################################################
# See [https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2](https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2)

library(ggplot2)
library(ggmap)

centers.file <- "data/centers.csv"
distance <- 50 # Max distance is 50 km

centers <- read.csv(file = centers.file)

map.boundaries <- c(left = -111.5, bottom = 31.0,
                    right = -108.5, top = 33.0)

google.map <- get_map(location = map.boundaries,
                      maptype = "roadmap")

base.map <- ggmap(google.map, extent = "panel", legend = "bottomright")
# print(base.map)

# create circles data frame from the centers data frame
make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$lat)
  # length per longitude changes with lattitude, so need correction
  radiusLng <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(center.name = rep(centers$center.name, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lng <- unlist(lapply(centers$lng, function(x) x + radiusLng * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

center.circles <- make_circles(centers = centers, radius = distance)

circles.map <- base.map + geom_point(data = centers, 
                                     mapping = aes(x = lng, y = lat), 
                                     color = "#FF0000") +
  geom_polygon(data = center.circles,
               mapping = aes(x = lng, y = lat, group = center.name),
               color = "#FF0000",
               alpha = 0)
# print(circles.map)
ggsave(filename = "output/centers-radius-map.pdf", plot = circles.map,
       width = 6, units = "in")
