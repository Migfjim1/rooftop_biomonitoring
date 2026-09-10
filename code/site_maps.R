#### SITE MAPS FOR ROOFTOP BIODIVERISTY MONITORING PROJECT ####
# Iris Cazares + Mikko Jimenez
# last updated: 9/3/26

# load packages
library(dplyr)
library(sf)
library(rnaturalearth)
library(tigris)
library(ggplot2)

# set data file folder path
data_fold_path <- "data/rooftop_biodiversity_map" # IRIS, youll need to change this to the filepath to the folder I sent you
out_dir <- "output"

## read in spatial files
# files saved locally
cities <- st_read(paste0(data_fold_path, "/cities_for_analysis.gpkg")) # cities data
################ subset to create the chicago boundary ###################
chicago_boundary <- cities %>% filter(GEOID20 == "16264")  
buffer_dist <- 1000
bbox_chicago <- st_bbox(st_buffer(chicago_boundary, buffer_dist))
##########################################################################
states <- st_read(paste0(data_fold_path, "/states_nolakes_equalarea.shp")) # state outlines
site_points <- read_sf(paste0(data_fold_path, "/GONet Sites.kml")) # site points from google maps
# downoading basemaps using packages
lakes <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf") %>%
  st_make_valid() %>%
  st_transform(st_crs(chicago_boundary)) %>%
  st_crop(bbox_chicago)
il_roads <- primary_roads() %>%
  st_transform(st_crs(chicago_boundary)) %>%
  st_crop(bbox_chicago)

## create a shared coord range + theme to create a bunch of comparable maps
# filter cities df to just chicago + create buffer
xlim <- c(bbox_chicago["xmin"], bbox_chicago["xmax"])
ylim <- c(bbox_chicago["ymin"], bbox_chicago["ymax"])
# creating shared coords + theme
shared_coord <- coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
shared_theme <- theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none",   # turn on per-plot if you want a legend
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

## get spatial data in same crs/projection
# transform data to match chicago boundary crs
states <- st_transform(states, st_crs(chicago_boundary))
site_points <- st_transform(site_points, st_crs(chicago_boundary))
# add the site selection field to the site df
selected_idx <- c(5, 15, 24, 29, 30, 32, 38, 42, 43, 44)
site_points$selected <- ifelse(seq_len(nrow(site_points)) %in% selected_idx, "yes", "no")

# plot map in ggplot
map <- ggplot() +
  geom_sf(data = states, fill = "#F2EFE9", color = "grey60", linewidth = 0.3) +
  geom_sf(data = lakes, fill = "#A9C6D8", color = NA) +
  geom_sf(data = il_roads, color = "#B8AFA0", linewidth = 0.1, alpha = 0.7) +
  geom_sf(data = chicago_boundary, fill = "grey80", alpha = 0.5) +
  geom_sf(data = filter(site_points, selected == "no"), color = "black", size = 1) +
  geom_sf(data = filter(site_points, selected == "yes"), color = "black", fill = "red", size = 2, shape = 21) +
  shared_coord +
  shared_theme

ggsave(file.path(out_dir,"sample_map.png"), width = 6, height = 4, dpi = 300)
