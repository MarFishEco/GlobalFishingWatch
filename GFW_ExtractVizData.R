setwd("/Users/gverutes/Desktop/GFW/")

# import libraries
library(tidyverse) # for general data wrangling and plotting
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(ggplot2)

######

#install.packages("rnaturalearth")
library(rnaturalearth)
library(sp)

#world countries
#sp::plot(ne_countries())

# World polygons from the maps package
world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
#plot(world_shp)

# Load EEZ polygons
eezs <- read_sf('World_EEZ_v11_20191118/', layer = 'eez_v11') %>% 
  filter(POL_TYPE == '200NM') # select the 200 nautical mile polygon layer

######

# Specify location of data directory
data_dir <- '/Users/gverutes/Desktop/GFW/fishing_effort/'

# Create dataframe of filenames dates and filter to date range of interest
effort_files <- tibble(
  file = list.files(paste0(data_dir, 'daily_csvs'), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

# Generate a vector of dates of interest using ymd from lubridate
effort_dates <- seq(ymd('2013-01-01'), ymd('2013-12-31'), by='days')  # IMPORTANT!!!

# Filter to files within our date range of interest
effort_files <- filter(effort_files, date %in% effort_dates)

# Read in data (uncomment to read in parallel)
plan(multiprocess) # Windows users should change this to plan(multisession)
effort_df <- furrr::future_map_dfr(effort_files$file, .f = read_csv)

# Add date information
effort_df <- effort_df %>% 
  mutate(year  = year(date),
         month = month(date))

# Adjusting data resolution
# Specify new (lower) resolution in degrees for aggregating data
res <- 0.25

# Transform data across all fleets and geartypes
effort_df <- effort_df %>% 
  mutate(
    # convert from hundreths of a degree to degrees
    lat_bin = lat_bin / 100, 
    lon_bin = lon_bin / 100,
    # calculate new lat lon bins with desired resolution
    lat_bin = floor(lat_bin/res) * res + 0.5 * res, 
    lon_bin = floor(lon_bin/res) * res + 0.5 * res)

# Re-aggregate the data to 0.25 degrees
effort_df <- effort_df %>% 
  group_by(date, year, month, lon_bin, lat_bin, flag, geartype) %>% 
  summarize(vessel_hours = sum(vessel_hours, na.rm = T),
            fishing_hours = sum(fishing_hours, na.rm = T),
            mmsi_present  = sum(mmsi_present, na.rm = T))

# Mapping fishing effort
# Total global fishing effort in 2012 (or any date...change line 43)
# Aggregate data across all fleets and geartypes
effort_all <- effort_df %>% 
  group_by(lon_bin,lat_bin) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>% 
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>% 
  filter(fishing_hours >= 24)

# Linear green color palette function
# effort_pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'), 
#                                interpolate = 'linear')

# Map fishing effort
# p1 <- effort_flag %>%
#   ggplot() +
#   geom_sf(data = world_shp,
#           fill = '#374a6d',
#           color = '#0A1738',
#           size = 0.1) +
#   geom_sf(data = eezs,
#           color = '#374a6d',
#           alpha = 0.2,
#           fill = NA,
#           size = 0.1) +
#   geom_raster(aes(x = lon_bin, y = lat_bin, fill = log_fishing_hours)) +
#   scale_fill_gradientn(
#     "Fishing Hours",
#     na.value = NA,
#     limits = c(1, 5),
#     colours = effort_pal(5), # Linear Green
#     labels = c("10", "100", "1,000", "10,000", "100,000+"),
#     values = scales::rescale(c(0, 1))) +
#   labs(fill  = 'Fishing hours (log scale)',
#        title = 'Spanish fishing effort in 2012')

# Fishing effort by gear type
# Aggregate data by geartype across all fleets 
# effort_gear <- effort_df %>%
#   group_by(lon_bin,lat_bin, geartype) %>%
#   summarize(fishing_hours = sum(fishing_hours, na.rm = T),
#             log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>%
#   ungroup() %>%
#   mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
#          log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>%
#   filter(fishing_hours >= 24)
# 
# table(effort_gear$geartype)

# Fishing effort by flag state
# Aggregate data by geartype across all fleets 
# effort_flag <- effort_df %>%
#   filter(flag %in% c('ESP')) %>% 
#   group_by(lon_bin, lat_bin, flag) %>% 
#   summarize(fishing_hours = sum(fishing_hours, na.rm = T),
#             log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
#   ungroup() %>% 
#   mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
#          log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>% 
#   filter(fishing_hours >= 24)

#table(effort_df$flag)

# Fishing effort within a region
# Select Spain EEZ
# per <- eezs %>%
#   filter(ISO_TER1 == 'ESP' & POL_TYPE == '200NM') %>%
#   dplyr::select(ISO_TER1, geometry)
# # Get bounding box for Spain EEZ
# per_bbox <- sf::st_bbox(per)

# Convert effort data frame to sf object
# effort_all_sf <- st_as_sf(effort_all,
#                           coords = c("lon_bin", "lat_bin"),
#                           crs = st_crs(per))

# # Filter effort data to within the Spain EEZ
# per_effort <- effort_all_sf %>%
#   sf::st_join(per, join = st_intersects) %>% # use a spatial join
#   filter(ISO_TER1 == 'ESP') %>% # filter only datapoints within Peruvian EEZ
#   bind_cols(st_coordinates(.) %>% as.data.frame()) %>%
#   rename(lat_bin = Y,
#          lon_bin = X) %>%
#   st_set_geometry(NULL)

######

# Convert to raster and save as GeoTIFF
# Select the coordinates and the variable we want to rasterize
effort_trawl <- per_effort %>% 
  dplyr::select(lon_bin, lat_bin, log_fishing_hours)  #, geartype
  #filter(geartype == "trawlers")

effort_trawl <- rasterFromXYZ(effort_trawl, 
                                   crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# Save GeoTiff
writeRaster(effort_trawl, filename = 'spain_eez_2013_raster.tif')

# add CRS
#bbox@proj4string
#data2@proj4string
#coordinates(data2) <- c("LON","LAT")
#proj4string(data2) <- CRS("+proj=longlat +datum=WGS84")

# create final shapefile
#writeOGR(obj=total, dsn="/Users/gverutes/Desktop/", layer="xxx", driver="ESRI Shapefile")