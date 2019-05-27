# Wrappers eBird API 2.0
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-12-09

################################################################################
#' Retrieve recent nearby observations using eBird API 2.0
#' 
#' See: https://documenter.getpostman.com/view/664302/ebird-api-20/2HTbHW#b785f3da-1802-d4e0-c447-85cb54abd0bb
RecentNearby <- function(key, lat = 32.241, lng = -110.938, dist = 80, back = 4, 
                         hotspot = "true", max.tries = 5, timeout.sec = 30) {
  request <- paste0("https://ebird.org/ws2.0/data/obs/geo/recent?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", distance,
                    "&back=", days.back,
                    "&hotspot=true",
                    "&key=", key)
  obs.request <- character(0)
  tries <- 0
  message(paste0("Requesting ", request, "..."))
  while(class(obs.request) != "data.frame" && tries < max.tries) {
    if (tries > 0) {
      message(paste0("...attempt ", tries, " failed; requesting again"))
    }
    
    # Old way, susceptible to timeout
    # obs.request <- try(expr = fromJSON(txt = request), silent = TRUE)

    # New way, explicitly allowing for longer host resolution times via the 
    # handle/CONNECTTIMEOUT options of curl::curl
    ebird.connection <- curl::curl(request,
                                   handle = curl::new_handle(CONNECTTIMEOUT = timeout.sec))
    # ebirdJSON <- try(expr = readLines(ebird.connection, warn = FALSE), silent = TRUE)
    # obs.request <- try(expr = jsonlite::fromJSON(txt = ebirdJSON), silent = TRUE)
    obs.request <- try(expr = {
      ebirdJSON <- readLines(ebird.connection, warn = FALSE)
      jsonlite::fromJSON(txt = ebirdJSON)
    }, silent = TRUE)
    
    close(ebird.connection)
    tries <- tries + 1
  }
  if (class(obs.request) != "data.frame"){
    message(paste0("Failed request for ", request, " after ", tries, " tries."))
  }
  return(obs.request)
}

################################################################################
#' Retrieve recent nearby observations of a species using eBird API 2.0
#' 
#' See: https://documenter.getpostman.com/view/664302/ebird-api-20/2HTbHW#32dc549f-74fb-4f6f-2b06-30b7647b2bca
RecentNearbySpecies <- function(key, species.code, lat = 32.241, lng = -110.938, 
                                dist = 80, back = 4, hotspot = "true", 
                                max.tries = 5, timeout.sec = 30) {
  request <- paste0("https://ebird.org/ws2.0/data/obs/geo/recent/",
                    species.code, "?",
                    "&lat=", lat,
                    "&lng=", lng,
                    "&dist=", distance,
                    "&back=", days.back,
                    "&hotspot=true",
                    "&key=", key)

  obs.request <- character(0)
  tries <- 0
  message(paste0("Requesting ", species.code, "..."))
  while(class(obs.request) != "data.frame" && tries < max.tries) {
    if (tries > 0) {
      message(paste0("...attempt ", tries, " failed for ", species.code, ". Requesting again"))
    }
    # Old way, susceptible to timeouts
    # obs.request <- try(expr = fromJSON(txt = request), silent = TRUE)
    
    # New way, explicitly allowing for longer host resolution times via the 
    # handle/CONNECTTIMEOUT options of curl::curl
    ebird.connection <- curl::curl(request,
                                   handle = curl::new_handle(CONNECTTIMEOUT = timeout.sec))
    # ebirdJSON <- try(expr = readLines(ebird.connection, warn = FALSE), silent = TRUE)
    # obs.request <- try(expr = jsonlite::fromJSON(txt = ebirdJSON), silent = TRUE)
    obs.request <- try(expr = {
      ebirdJSON <- readLines(ebird.connection, warn = FALSE)
      jsonlite::fromJSON(txt = ebirdJSON)
    }, silent = TRUE)
    close(ebird.connection)
    
    tries <- tries + 1
  }
  if (class(obs.request) != "data.frame"){
    message(paste0("Failed request for ", species.code, " after ", tries, " tries."))
  }
  return(obs.request)
}

################################################################################
#' Split names column into scientific and common
#' 
#' @param data data.frame that at has at least a character column of names
#' @param delim character separator
#' @param colname name of column with species names
#' @details Names from eBird come in a single column as: "Snow Goose - Anser 
#' caerulescens". This function provides a means of separating the common (Snow
#' Goose) from scientific (Anser caerulescens) into two separate columns.
#' @return data.frame with two additional columns, \code{Common} and 
#' \code{Scientific}
SplitNames <- function(data, delim = " - ", colname = "Species") {
  name.pairs <- strsplit(x = as.character(data[, colname]), split = delim)
  common <- sapply(name.pairs, "[[", 1)
  scientific <- sapply(name.pairs, "[[", 2)
  data$Common <- common
  data$Scientific <- scientific
  return(data)
}

################################################################################
#' Drops any species with given patterns in name
DropPatterns <- function(data, patterns = c(), colname = "comName"){
  for (p in patterns){
    pattern.found <- grep(x = data[, colname], pattern = p)
    if (length(pattern.found) > 0) {
      data <- data[-pattern.found, ]
    }
  }
  return(data)
}

################################################################################
#' Performs search for one pair of latitude & longitude coordinates
TargetList <- function(key, center.lat, center.lng, distance = 50, back = 4) {
  # Get recent nearby observations from eBird
  nearby.obs <- RecentNearby(key = key, 
                             lat = center.lat, 
                             lng = center.lng, 
                             dist = distance, 
                             back = back)
  
  # Drop some species, based on patterns in species names
  drop.patterns <- c("sp.", "/", "Domestic type")
  nearby.obs <- DropPatterns(data = nearby.obs, patterns = drop.patterns)
  
  # Perform set difference, getting list of nearby species *NOT* on current list
  nearby.missing <- nearby.obs[!(nearby.obs$comName %in% current.list$Common), ]
  
  # List to hold nearby observations
  nearby.list <- list()
  # Iterate over nearby.missing, extract speciesCode and get all observations
  # for (sp.code in nearby.missing$speciesCode[1:6]) {
  for (sp.code in nearby.missing$speciesCode) {
    nearby.obs.sp <- RecentNearbySpecies(key = key, 
                                         species.code = sp.code,
                                         lat = center.lat, 
                                         lng = center.lng, 
                                         dist = distance, 
                                         back = back)
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
  # Arrange the elements in the list by decreasing number of species
  target.list <- target.list[order(sapply(target.list, '[[', "num.species"), decreasing = TRUE)]
 
  return(target.list) 
}

################################################################################
#' Create markdown-format information for a site
CreateSiteMarkdown <- function(target, site.number) {
  # Add headers for this site
  cat(paste0("\n## ", site.number, ". ", target$site.name, "\n"))
  coordinates <- target$coordinates
  # http://maps.google.com/maps?q=24.197611,120.780512
  google.maps.url <- paste0("http://maps.google.com/maps?q=",
                            coordinates["lat"],
                            ",",
                            coordinates["lng"])
  cat(paste0("### ", coordinates["lat"], ", ", 
             coordinates["lng"], " (",
             "[Google Maps](", google.maps.url,
             "))\n"))
  cat(paste0("### ", target$num.species, " species\n"))
  species <- target$species
  
  cat("\n| Species | Count | Last seen |\n")
  cat("|:--------|:-----:|:---------:|\n")
  for (species.i in 1:nrow(species)){
    cat(paste0("| ", species$Species[species.i],
               " | ", species$Num.obs[species.i],
               " | ", species$Last.seen[species.i],
               " |\n"))
  }
  cat("\n")
}