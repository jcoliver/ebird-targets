# Sites report for Pinetop
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-09-13

library(lifeR)

# Lat/lon for a Pinetop & Sheep Crossing centers
locs <- matrix(data = c(34.11729335843701, -109.93865012922537,
                        33.96084695988325, -109.50321014527596),
               nrow = 2, ncol = 2, byrow = TRUE)
loc_names <- c("Pinetop", "Sheep Crossing")

num_sites <- 10
year_list <- 2024

# File with eBird API key
key_file <- "ebird-key.txt"

# File with year list
list_file <- paste0("data/year-lists/ebird_world_year_",
                    year_list,
                    "_list.csv")

# Read in key from file
key <- scan(file = key_file, what = "character")
# Read in file with species that have been seen
seen <- read.csv(file = list_file)

my_species <- seen$Common.Name

# Destination information for report
report_filename <- paste0("Pinetop-sites-report-", Sys.Date())
report_directory <- "~/Desktop"

# Generate the report
report <- lifeR::SitesReport(centers = locs,
                             center_names = loc_names, 
                             ebird_key = key,
                             report_filename = report_filename,
                             report_dir = report_directory,
                             report_format = "html",
                             species_seen = my_species,
                             max_sites = num_sites,
                             messages = "verbose")
