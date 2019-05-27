# eBird targets
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-12-02

rm(list = ls())

################################################################################
library("jsonlite")
library("dplyr")
source(file = "functions/eBird-API-2.0.R")
current.list.file <- "data/ebird_north_america_year_list.csv"
output.file <- "output/targets-Tucson.txt"

key <- scan(file = "ebird-2.0-api-key.txt", what = "character")
lat <- 32.241
lng <- -110.938
distance <- 50 # Max distance is 50 km
days.back <- 4

obs.request <- RecentNearby(key = key, lat = lat, lng = lng, dist = distance, 
                            back = days.back)


# Read current list, downloaded from eBird site
current.list <- read.csv(file = current.list.file)

# Deal with names
current.list <- SplitNames(data = current.list)

# Comparing lists
nearby.obs <- obs.request

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
                                       lat = lat, 
                                       lng = lng, 
                                       dist = distance, 
                                       back = days.back)
  nearby.list[[sp.code]] <- nearby.obs.sp
  Sys.sleep(time = 1.0)
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
for (site in names(target.list)[1:5]) {
  print(site)
  print(target.list[[site]]$species)
}
sink()


################################################################################
## OLD BELOW HERE
################################################################################

centers <- c("tucson", "patagonia", "chiracahuas")
lats <- c(32.241, 31.543879, 31.759155)
lngs <- c(-110.938, -110.748517, -109.357098)
df <- data.frame(center.name = centers,
                 lat = lats,
                 lng = lngs)
write.csv(x = df, file = "data/centers.csv", quote = FALSE, row.names = FALSE)


# API 1.1 below here
# For API 2.0, see https://documenter.getpostman.com/view/664302/ebird-api-20/2HTbHW

# Change these values for different output / geographic center
centers <- list()

centers[["tucson"]]$lat <- 32.241
centers[["tucson"]]$lng <- -110.938

centers[["patagonia"]]$lat <- 31.543879
centers[["patagonia"]]$lng <- -110.748517

centers[["chiracahuas"]]$lat <- 31.759155
centers[["chiracahuas"]]$lng <- -109.357098

distance <- 50
days.back <- 7
top.cutoff <- 5

infile <- "data/ebird_north_america_year_list.csv"

# Read current list, downloaded from eBird site
current.list <- read.csv(file = infile)

# Names in current.list needs to be split:
# "Snow Goose - Anser caerulescens"
name.pairs <- strsplit(x = as.character(current.list$Species), split = " - ")
common <- sapply(name.pairs, "[[", 1)
scientific <- sapply(name.pairs, "[[", 2)
current.list$Common <- common
current.list$Scientific <- scientific

for (center in names(centers)) {
  outfile <- paste0("output/targets-", center, ".txt")
  lat <- centers[[center]]$lat
  lng <- centers[[center]]$lng

  # Build request URL
  request <- paste0("http://ebird.org/ws1.1/data/obs/geo/recent?locale=en_US&fmt=json",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", distance,
                    "&back=", days.back)
  
  # Get observations from eBird
  obs.request <- fromJSON(txt = request)
  
  message(paste0(nrow(obs.request), " observations for center = ", center))
  nearby.obs <- obs.request
  
  # Drop private observations
  nearby.obs <- nearby.obs[!nearby.obs$locationPrivate, ]
  
  # Convert to date format, dropping the time of day
  nearby.obs$obsDt <- as.Date(x = nearby.obs$obsDt)
  
  # Drop some columns that might cause problems with unique later on
  nearby.obs <- nearby.obs[, c("lat", "lng", "locID", "locName", "howMany", 
                               "sciName", "comName", "obsDt")]
  
  # For debugging purposes
  write.csv(x = nearby.obs, file = paste0("output/nearby-", center, ".csv"))
  
  # Drop any "sp." observations
  sp.in.names <- grep(x = nearby.obs$comName, pattern = "sp.")
  if (length(sp.in.names) > 0) {
    nearby.obs <- nearby.obs[-sp.in.names, ]
  }
  # Drop uncertain observations (e.g. Eastern/Western Meadowlark)
  uncertain <- grep(x = nearby.obs$comName, pattern = "/")
  if (length(uncertain) > 0) {
    nearby.obs <- nearby.obs[-uncertain, ]
  }
  # Drop domestics
  domestic <- grep(x = nearby.obs$comName, pattern = "Domestic type")
  if (length(domestic) > 0) {
    nearby.obs <- nearby.obs[-domestic, ]
  }
  message(paste0(nrow(nearby.obs), " unambiguous observations for center = ", center))
  
  # Reduce nearby observations to only those *not* on current.list
  nearby.missing <- nearby.obs[!(nearby.obs$comName %in% current.list$Common), ]
  message(paste0(nrow(nearby.missing), " missing observations for center = ", center))
  
  if (nrow(nearby.missing) > 0) {
    # Pull out unique rows only
    nearby.missing <- unique(x = nearby.missing)
    
    # Summarize to find hotspots to target
    targets <- nearby.missing %>%
      group_by(locName) %>%
      summarise(num.targets = length(unique(comName)))
    targets <- data.frame(targets)
    
    # Sort so top are listed first
    targets <- targets[order(targets$num.targets, decreasing = TRUE), ]
    
    # Get species lists for each of top X targets
    species.lists <- list()

    cutoff <- top.cutoff
    if (nrow(targets) < top.cutoff) {
      cutoff <- nrow(targets)
    }
    
    if (cutoff > 0) {
      for (i in 1:cutoff) {
        loc <- targets$locName[i]
        one.loc <- nearby.missing[nearby.missing$locName == loc, ]
        # Note: current API only returns MOST RECENT record of species/locality
        # observation, so times.obs will always be 1
        species.lists[[loc]] <- as.data.frame(table(one.loc$comName))
        colnames(species.lists[[loc]]) <- c("species", "times.obs")
      }
      
      # Write the list of top X spots to file
      sink(file = outfile)
      print(species.lists)
      sink()
    } else { # Hrm...this should never happen
      message(paste0("Zero targets found for center ", center))
    }
  } else {
    message(paste0("Zero unobserved species found for center ", center))
  }
}


