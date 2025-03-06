# Create report for three centers around Tucson
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2021-02-11

################################################################################

# remotes::install_github(repo = "jcoliver/lifeR", build_vignettes = TRUE, force = TRUE)
# install.packages("lifeR")
library(lifeR)

# Settings for this report
max_sites <- 5
dist <- 20 # 50
back <- 7 # 4

# File with lat, long, and names of centers
center_file <- "../ebird-targets/data/centers.csv"

# File with eBird API key
key_file <- "../ebird-targets/ebird-key.txt"

# File with year list
list_file <- "../ebird-targets/data/year-lists/ebird_world_year_2025_list.csv"

# Destination information for report
report_filename <- paste0("Sites-report-", Sys.Date())
report_directory <- "~/Desktop"

# Load in center file; subset as necessary
centers <- read.csv(file = center_file)
# Pull out rows of centers of interest
# Tucson area
centers <- centers[1:3, ]
# White Mountains
# centers <- centers[4:5, ]
# Corvallis
# centers <- centers[c(6, 8), ]
# Carson City
# centers <- centers[10, ]
# Extract relevant information
locs <- as.matrix(centers[, c(2,3)])
loc_names <- centers$center.name

# Read in key from file
key <- scan(file = key_file, what = "character")
# Read in file with species that have been seen
seen <- read.csv(file = list_file)
# Pull out the common name
# my_species <- lifeR::SplitNames(x = seen$Scientific.Name)$Common
my_species <- seen$Common.Name

# Generate the report
report <- lifeR::SitesReport(centers = locs,
                             center_names = loc_names, 
                             ebird_key = key,
                             report_filename = report_filename,
                             report_dir = report_directory,
                             report_format = "html",
                             species_seen = my_species,
                             messages = "verbose",
                             max_sites = max_sites,
                             dist = dist,
                             back = back)
