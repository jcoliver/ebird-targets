# Plot times of observations for ELTR data
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2020-10-13

################################################################################
library(tidyverse)
# library(lubridate)
# library(hms)

eltr <- readr::read_csv(file = "data/eltr.csv")

# Calculate end point for segment
eltr <- eltr %>%
  mutate(end = start + duration)

# Add a unique id for individual segments
eltr <- eltr %>%
  mutate(id = 1:nrow(eltr))

# Plot each checklist duration as a vertical line, coloring lines by whether or 
# not ELTR was seen
ggplot(data = eltr, mapping = aes(x = date, color = seen, group = id)) +
  geom_segment(mapping = aes(y = start, yend = end),
               position = position_dodge(0.25),
               linewidth = 2) +
  scale_color_manual(values = c("#CC6611", "#11CC66")) +
  theme_bw() +
  facet_wrap(~site)
