# Plot times of observations
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-10-13

rm(list = ls())

################################################################################
library(tidyverse)
library(lubridate)
library(hms)

eaqu <- readr::read_csv(file = "data/eaqu.csv")

# Some don't have times of actual observation, only start/end; will need to get
# midpoints as estimates
# Start with half the duration of the observation perioe
timespan <- difftime(time1 = eaqu$end, time2 = eaqu$start)/2

# Midpoint datetime, because just adding minutes isn't easy (possible?)
midpoint <- as.POSIXct(x = paste0(eaqu$date, " ", eaqu$start)) + as.difftime(timespan, units = "mins")

# start_datetime <- as_datetime(paste0(eaqu$date, " ", eaqu$start),
#                               tz = "MST")
# end_datetime <- as_datetime(paste0(eaqu$date, " ", eaqu$end),
#                               tz = "MST")
# period(end_datetime - start_datetime)

# Extract just the time of day from that midpoint
midpoint_times <- midpoint %>%
  str_split(" ") %>%
  map_chr(2) %>%
  as_hms()

eaqu$midpoint_times <- midpoint_times

eaqu <- eaqu %>%
  mutate(time_estimate = if_else(is.na(time_seen), "Inferred", "Actual")) %>%
  mutate(time_estimate = factor(time_estimate, levels = c("Actual", "Inferred"))) %>%
  mutate(time_seen_est = if_else(is.na(time_seen), midpoint_times, time_seen))

# Getting first and last observation times for each day for envelope
obs_times <- eaqu %>%
  group_by(date) %>%
  summarize(earliest = as_hms(min(start)),
            latest = as_hms(max(end))) %>%
  pivot_longer(cols = -date,
               names_to = "period",
               values_to = "time")

# Plot points
# Also want min/max to show observation window of each day
ggplot(data = eaqu, mapping = aes(x = date,
                                  y = time_seen_est)) +
  geom_line(data = obs_times,
            mapping = aes(x = date,
                          y = time,
                          group = period)) +
  geom_jitter(width = 0.2, mapping = aes(group = seen,
                                          color = seen,
                                          shape = time_estimate),
              size = 3) +
  scale_shape_manual(name = "Actual or inferred",
                     values = c(19, 3)) +
  scale_color_manual(name = "EAQU seen?",
                       values = c("#FF00FF", "#000000")) +
  xlab(label = "Date") +
  ylab(label = "Time Seen") +
  theme_classic()

# Plotting just start/end times as vertical lines
ggplot(data = eaqu, mapping = aes(x = date,
                                  y = time_seen_est)) +
  geom_errorbar(mapping = aes(ymin = start,
                              ymax = end,
                              color = seen),
                width = 0.0, # gets rid of horizonal lines
                alpha = 0.5,
                size = 1,
                position = position_dodge(width = 0.25)) +
  scale_color_manual(name = "EAQU seen?",
                     values = c("#FF00FF", "#000000"))

# Density plot of times seen
ggplot(data = eaqu, mapping = aes(x = time_seen_est,
                                  fill = seen)) +
  geom_density(alpha = 0.5)
