# Create data for multiple center analysis
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-12-10

rm(list = ls())

################################################################################
centers <- c("tucson", "patagonia", "chiracahuas")
lats <- c(32.241, 31.543879, 31.759155)
lngs <- c(-110.938, -110.748517, -109.757098)
df <- data.frame(center.name = centers,
                 lat = lats,
                 lng = lngs)
write.csv(x = df, file = "data/centers.csv", quote = FALSE, row.names = FALSE)
