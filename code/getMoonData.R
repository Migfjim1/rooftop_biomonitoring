# load/install packages
library(suncalc)
library(moonlit)
library(dplyr)
library(ggplot2)

# set dates and timezone 
date <- seq(ISOdate(2023,9,15), ISOdate(2024,10,20), "hour")
date_mst <- date
attr(date_mst, "tzone") <- "MST"

# extract illumination data
res_mst <- getMoonIllumination(date = date_mst)

# get illumination from moonlit 
# specify location and date range
lat <- 40.5853
lon <- -105.0844
date <- seq(ISOdate(2023,9,15), ISOdate(2024,10,20), "hour")
moonlit_df <- calculateMoonlightIntensity(lat, lon, date, 0.16)

# save data - optional
saveRDS(moonlit_df, "data/moon_illum.rds")

# make sure these line up
# ensure the date fields are of Date class
moonlit_df <- moonlit_df %>%
  mutate(date = as.Date(date))

res_mst <- res_mst %>%
  mutate(date = as.Date(date))

# merge the data frames by date
merged_df <- inner_join(moonlit_df, res_mst, by = "date")

# check the first few rows of the merged data
head(merged_df)

# visualize the relationship
ggplot(merged_df, aes(x = moonPhase, y = fraction)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Scatter Plot of Illumination vs Fraction (Aligned by Date)",
    x = "Illumination",
    y = "Fraction"
  ) +
  theme_minimal()
