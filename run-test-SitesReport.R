# A small test of SitesReport (2 centers, 2 days back, 25 km, max 2 sites)
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2024-05-20

################################################################################

# remotes::install_github(repo = "jcoliver/lifeR", build_vignettes = TRUE, force = TRUE)
# install.packages("lifeR")
library(lifeR)

# File with lat, long, and names of centers
center_file <- "../ebird-targets/data/centers.csv"

# File with eBird API key
key_file <- "../ebird-targets/ebird-key.txt"

# File with year list
list_file <- "../ebird-targets/data/year-lists/ebird_world_year_2025_list.csv"

# Destination information for report
report_filename <- paste0("Sites-report-TEST-", Sys.Date())
report_directory <- "~/Desktop"

# Not likely any edits necessary below here
centers <- read.csv(file = center_file)
# Extract relevant information
locs <- as.matrix(centers[1:2, c(2,3)])
loc_names <- centers$center.name[1:2]

# Read in key from file
key <- scan(file = key_file, what = "character")
# Read in file with species that have been seen
seen <- read.csv(file = list_file)
# Pull out the common name
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
                             max_sites = 2,
                             dist = 25,
                             back = 2)
