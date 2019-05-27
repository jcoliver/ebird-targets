# Analyze multiple centers
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-12-10

# USAGE:
# source(file = "scripts/multiple-centers.R")

rm(list = ls())

################################################################################
# SETUP
# Load dependancies
# Load data & setups output path
# Set values for analysis

# Load dependancies
library("jsonlite")
library("dplyr")
source(file = "functions/eBird-API-2.0.R")

# Load data
current.list.file <- "data/ebird_north_america_year_list.csv"
centers.file <- "data/centers.csv"
output.dir <- "output/"

# Set values for analysis
key <- scan(file = "ebird-2.0-api-key.txt", what = "character")
distance <- 50 # Max distance is 50 km
days.back <- 4
max.targets <- 5 # Maximum number of sites to return per center

################################################################################
# ANALYZE
# Read in data & clean as necessary
# Iterate over each center and perform queries as appropriate
# Send results to file

# Read in data & clean
# Read current list, downloaded from eBird site
current.list <- read.csv(file = current.list.file)
# Deal with names
current.list <- SplitNames(data = current.list)

# Read centers with latitude & longitude for each
centers <- read.csv(file = centers.file)

# Iterate over each center
for (center.i in 1:nrow(centers)) {
  center.name <- centers$center.name[center.i]
  center.lat <- centers$lat[center.i]
  center.lng <- centers$lng[center.i]
  message(paste0("Running analysis on ", center.name))
  output.file <- paste0(output.dir, "targets-", center.name, ".txt")
  
  # Get recent nearby observations from eBird
  nearby.obs <- RecentNearby(key = key, 
                             lat = center.lat, 
                             lng = center.lng, 
                             dist = distance, 
                             back = days.back)
  
  # Drop some species, based on patterns in species names
  drop.patterns <- c("sp.", "/", "Domestic type")
  nearby.obs <- DropPatterns(data = nearby.obs, patterns = drop.patterns)
  
  # Perform set difference, getting list of nearby species *NOT* on current list
  nearby.missing <- nearby.obs[!(nearby.obs$comName %in% current.list$Common), ]
  
  nearby.list <- list()
  
  # Iterate over nearby.missing, extract speciesCode and get all observations
  # for (sp.code in nearby.missing$speciesCode[1:6]) {
  for (sp.code in nearby.missing$speciesCode) {
    nearby.obs.sp <- RecentNearbySpecies(key = key, 
                                         species.code = sp.code,
                                         lat = center.lat, 
                                         lng = center.lng, 
                                         dist = distance, 
                                         back = days.back)
    nearby.list[[sp.code]] <- nearby.obs.sp
    Sys.sleep(time = 1.0) # So we're not hammering on eBird's server
  }
  
  # Put all results together in single data frame
  targets.df <- bind_rows(nearby.list)
  
  # Identify unique values for site names
  sites <- unique(targets.df$locName)
  
  # Prepare container for site information
  target.list <- vector("list", length(sites))
  names(target.list) <- sites
  
  # Iterate over all sites, getting target list for each
  for (site in sites) {
    one.site <- targets.df[targets.df$locName == site, ]
    num.species <- nrow(one.site)
    lat <- one.site$lat[1] # Only first one, multiple species
    lng <- one.site$lng[1]
    coordinates <- c(lat, lng)
    names(coordinates) <- c("lat", "lng")
    species <- data.frame(Species = one.site$comName,
                          Num.obs = one.site$howMany,
                          Last.seen = as.Date(one.site$obsDt))
    target.list[[site]] <- list()
    target.list[[site]][["site.name"]] <- site
    target.list[[site]][["num.species"]] <- num.species
    target.list[[site]][["coordinates"]] <- coordinates
    target.list[[site]][["species"]] <- species
  }
  
  target.list <- target.list[order(sapply(target.list, '[[', "num.species"), decreasing = TRUE)]
  
  sink(file = output.file)
  for (site in names(target.list)[1:max.targets]) {
    print(site)
    print(target.list[[site]]$species)
  }
  sink()
  
  message(paste0("Analysis complete on ", center.name, ". Results written to ", output.file))
  
}
