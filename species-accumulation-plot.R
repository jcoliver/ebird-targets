# Species accumulation plot
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-12-31

rm(list = ls())

################################################################################
library(readr)     # read in files
library(dplyr)     # general data wrangling
library(lubridate) # easier retrieval of year for Jan 1 and Dec 31
library(ggplot2)   # plot, obvs

# Find list files and iterate over each of them
year_lists <- list.files(path = "data/year-lists",
                         pattern = "ebird_world_year*",
                         full.names = TRUE)

# The data frame that will hold information for all years
mult_years <- NULL

for (year_list in year_lists) {
  # Read in the data and convert Date column to Date format
  first_obs <- read_csv(file = year_list,
                        col_types = cols()) # To suppress messages
  first_obs <- first_obs %>%
    mutate(Date = as.Date(Date, format = "%d %b %Y"))
  
  # Summarize number of new species per day
  per_date <- first_obs %>%
    group_by(Date) %>%
    summarize(Num_new = n())
  
  # Arrange (order) by date
  per_date <- per_date %>%
    arrange(Date)
  
  # Grab the year for this list, which we may need
  list_year <- year(per_date$Date[1])
  
  # Check to see if January 1 is there, if not, add it with 0 species as first row
  if (month(per_date$Date[1]) == 1 & day(per_date$Date[1]) == 1) {
    message(paste0("List already includes observations on 01 January ", list_year))
  } else {
    per_date <- bind_rows(list(Date = as.Date(paste0(list_year, "-01-01")),
                               Num_new = 0),
                          per_date)
    message(paste0("Added row for 01 January ", list_year))
  }
  
  # Check to see if December 31 is there, if not, add it as last row with same 
  # number of species as reported in last date
  # Only do this for years other than the current year
  if (list_year != year(Sys.Date())) {
    if (month(per_date$Date[nrow(per_date)]) == 12 & day(per_date$Date[nrow(per_date)]) == 31) {
      message(paste0("List already includes observations on 31 December ", list_year))
    } else {
      per_date <- bind_rows(per_date,
                            list(Date = as.Date(paste0(list_year, "-12-31")),
                                 Num_new = 0))
      message(paste0("Added row for 31 December ", list_year, ", with 0 observations."))
    }
  } else {
    # For current year, add row for current date
    per_date <- bind_rows(per_date,
                          list(Date = Sys.Date(),
                               Num_new = 0))
  }
  # Calculate cumulative sum of number of new species
  per_date$Cum_sum <- cumsum(per_date$Num_new)

  # Create or add to multiple year data frame as appropriate  
  if (is.null(mult_years)) {
    mult_years <- per_date
  } else {
    mult_years <- bind_rows(mult_years, per_date)
  }
}

# Pull out year as separate column to use for different lines and
# add day of year, which will be used for plot
mult_years <- mult_years %>%
  mutate(Year_fac = as.factor(year(Date))) %>%
  mutate(DOY = yday(mult_years$Date))

# Create a little tibble so we can add months as x-axis labels, rather than 
# day of year
# Primarily drawn from
# https://scottishsnow.wordpress.com/2020/04/24/lubridate-ggplot-date-helpers/

doy <- date(paste(rep("2020", times = 12),
                  seq(1:12),
                  rep("1", times = 12),
                  sep = "-"))
doy <- tibble(Month = month(doy, label = TRUE),
              DOY = yday(doy))

# Character string we'll use for plot title
min_max_year <- paste0(min(year(mult_years$Date)), "-",
                       max(year(mult_years$Date)))

# Plot with date on Y, species on Y
sa_plot <- ggplot(data = mult_years, mapping = aes(x = DOY, 
                                                 y = Cum_sum,
                                                 color = Year_fac,
                                                 group = Year_fac)) +
  # geom_line() +
  geom_step() +
  scale_x_continuous(breaks = doy$DOY, labels = doy$Month) +
  labs(title = paste0("Species accumulation plot, ", min_max_year),
       x = "Date", 
       y = "# Species",
       color = "Year") +
  theme_minimal() + 
  theme(text = element_text(size = 16))
print(sa_plot)
ggsave(filename = "output/Sp_accumulation.pdf", plot = sa_plot)
