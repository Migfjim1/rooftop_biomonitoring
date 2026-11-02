# load/install packages
library(suncalc)
library(moonlit)
library(dplyr)
library(ggplot2)

# set dates and timezone 
datetime <- seq(as.POSIXct("20261012 184500", format="%Y%m%d %H%M%S", tz="America/Chicago"),
                as.POSIXct("20261013 070000", format="%Y%m%d %H%M%S", tz="America/Chicago"),
                "15 mins")

# extract illumination data
res <- getMoonIllumination(date = datetime)

# import site reference file 
site_summary <- read.csv("Site_Summary_Moon.csv", header=TRUE)
site_list <- split(site_summary, site_summary$Site)

# get illumination from moonlit 
# specify location and date range
lat <- site_list[["LOOP-CAC"]]$Lat
lon <- site_list[["LOOP-CAC"]]$Lon
date <- datetime
ec <- elevExtCoeff(site_list[["LOOP-CAC"]]$Elev)
moonlit_df <- calculateMoonlightIntensity(lat, lon, date, ec)
illum <- formatC(moonlit_df$illumination, format="e", digits=2)

# save data - optional
saveRDS(moonlit_df, "data/moon_illum.rds")

# calculate time from sunset 
moon_stats <- calculateMoonlightStatistics(lat, lon, date[1], ec, "day", "America/Chicago")
sunset <- moon_stats$sunset 
datetime_first <- date[1]
difftime(datetime_first, sunset, tz="America/Chicago", units="mins")

# make sure these line up
# ensure the date fields are of Date class
moonlit_df <- moonlit_df %>%
  mutate(date = as.Date(date))

res <- res %>%
  mutate(date = as.Date(date))

# merge the data frames by date
merged_df <- inner_join(moonlit_df, res, by = "date")

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
