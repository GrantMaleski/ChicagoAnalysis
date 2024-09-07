# Load required packages
install.packages(c("ggmap", "geosphere", "sf", "tigris", "lubridate"))
library(ggmap)
library(geosphere)
library(sf)
library(ggplot2)
library(tigris)
library(lubridate)
library(dplyr)
library(tidyverse)

# Set up Google Maps API
register_google(key = "AIzaSyBFtOlgqh_2oOpGILbJJEHVn-VzqJEP1rI")

# Load data
crashes <- read.csv('/Users/grantmaleski/Downloads/Traffic_Crashes_-_Crashes_20240408.csv')
cameras <- read.csv('/Users/grantmaleski/Downloads/Speed_Camera_Locations_20240408.csv')
chicago_safety_zones <- read.csv('/Users/grantmaleski/Downloads/chicago_safety_areas.csv')
chicago_map <- read_sf("/Users/grantmaleski/Downloads/WARDS_2015")
people <- read.csv('/Users/grantmaleski/Downloads/Traffic_Crashes_-_People.csv')

# Clean and preprocess data
## Some of the addresses aren't formatted properly for API call - Fix addresses
chicago_safety_zones[367,3] <- '3000 S Martin Luther King Dr'
chicago_safety_zones[1402,3] <- '7001 West 59th St'

## Geocode addresses via Google Maps API
addresses <- chicago_safety_zones$Address
chicago_addresses <- paste(addresses, ", Chicago, IL", sep = "")
geocodes <- geocode(chicago_addresses)
geocoded_df <- data.frame(Zone_Name = chicago_safety_zones$Name,
                          Address = chicago_addresses, 
                          Latitude = geocodes$lat, 
                          Longitude = geocodes$lon)


# Process dates
cameras$go.live.date <- as.Date(cameras$GO.LIVE.DATE, format = "%m/%d/%Y")
cameras$year <- format(cameras$go.live.date, "%Y")
cameras$month <- format(cameras$go.live.date, "%m")
cameras$day <- format(cameras$go.live.date, "%d")

crashes$CRASH_DATE <- as.Date(crashes$CRASH_DATE, format = "%m/%d/%Y")
crashes$crash_year <- format(crashes$CRASH_DATE, "%Y")
crashes$crash_month <- format(crashes$CRASH_DATE, "%m")
crashes$crash_day <- format(crashes$CRASH_DATE, "%d")




## Summarize persons dataset. Since we'll need this information to analyze how Chicago...
## calculates where they place safety cameras

crash_person_summary <- people %>%
  group_by(CRASH_RECORD_ID) %>%
  summarise(
    fatalities = sum(INJURY_CLASSIFICATION == "FATAL", na.rm = TRUE),
    bikers_pedestrians = sum(PERSON_TYPE %in% c("BICYCLE", "PEDESTRIAN"), na.rm = TRUE),
    under_18 = sum(AGE < 18, na.rm = TRUE)
  )


#filter out any crashes that don't have a lat or long
crashes_clean <- crashes %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))

# Join crashes df with crash_person_summary so we know who is involved in each crash
crashes_clean <- crashes_clean %>%
  left_join(crash_person_summary, by = "CRASH_RECORD_ID", suffix = c("_sf", "_summary"))

# Convert to sf object to preppare for geospatial calculations
crashes_sf <- st_as_sf(crashes_clean, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#collect coordinates from crashes geospatial df
crash_coordinates <- st_coordinates(crashes_sf)


# Create crash_location_list
crash_location_list <- data.frame(
  CRASH_RECORD_ID = crashes_clean$CRASH_RECORD_ID,
  Longitude = crash_coordinates[,1],
  Latitude = crash_coordinates[,2]
)


chicago_safety_zones_list <- data.frame(Latitude = chicago_safety_zones$Latitude, 
                                        Longitude = chicago_safety_zones$Longitude)

camera_location_list <- data.frame(Latitude = cameras$LATITUDE, 
                                   Longitude = cameras$LONGITUDE)

# Add latitude and longitude to chicago_safety_zones
chicago_safety_zones$Latitude <- geocodes$lat
chicago_safety_zones$Longitude <- geocodes$lon




# Set up our safety zone and crash locations lists to be geospatially analyzed
## Set up CRS
crs <- st_crs("+proj=longlat +datum=WGS84")

## Filter and convert to sf objects
crash_location_list_filtered <- crash_location_list %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

crash_location_sf <- st_as_sf(crash_location_list_filtered, 
                              coords = c("Longitude", "Latitude"), 
                              crs = crs)

camera_location_list_sf <- st_as_sf(camera_location_list, 
                                    coords = c("Longitude", "Latitude"), 
                                    crs = crs)


chicago_safety_zones_list_filtered <- chicago_safety_zones_list %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

chicago_safety_zones_list_sf <- st_as_sf(chicago_safety_zones_list_filtered, 
                                         coords = c("Longitude", "Latitude"), 
                                         crs = crs)



#start calculating distances of each crash to nearest sf zone and camera

## Calculate nearest neighbors and distances
### For cameras
nearest_indices_camera <- st_nearest_feature(crash_location_sf, camera_location_list_sf)
nearest_neighbors_camera <- camera_location_list_sf[nearest_indices_camera, ]
crash_distance_to_camera <- data.frame(
  CRASH_RECORD_ID = crash_location_list_filtered$CRASH_RECORD_ID,
  Latitude_df1 = st_coordinates(crash_location_sf)[, 2],
  Longitude_df1 = st_coordinates(crash_location_sf)[, 1],
  Latitude_closest = st_coordinates(nearest_neighbors_camera)[, 2],
  Longitude_closest = st_coordinates(nearest_neighbors_camera)[, 1],
  distance_to_crash = as.numeric(st_distance(crash_location_sf, nearest_neighbors_camera, by_element = TRUE))
)

### For safety zones
nearest_indices_zone <- st_nearest_feature(crash_location_sf, chicago_safety_zones_list_sf)
nearest_neighbors_zone <- chicago_safety_zones_list_sf[nearest_indices_zone, ]
safety_closest_points <- data.frame(
  CRASH_RECORD_ID = crash_location_list_filtered$CRASH_RECORD_ID,
  Latitude_df1 = st_coordinates(crash_location_sf)[, 2],
  Longitude_df1 = st_coordinates(crash_location_sf)[, 1],
  Latitude_closest = st_coordinates(nearest_neighbors_zone)[, 2],
  Longitude_closest = st_coordinates(nearest_neighbors_zone)[, 1],
  distance_to_zone = as.numeric(st_distance(crash_location_sf, nearest_neighbors_zone, by_element = TRUE))
)

# Join distance data to crashes dataframe
crashes_clean <- crashes_clean %>%
  left_join(crash_distance_to_camera %>% 
              select(CRASH_RECORD_ID, distance_to_crash) %>% 
              rename(distance_to_camera = distance_to_crash), 
            by = "CRASH_RECORD_ID")

crashes_clean <- crashes_clean %>%
  left_join(safety_closest_points %>% 
              select(CRASH_RECORD_ID, distance_to_zone) %>% 
              rename(distance_to_sfzone = distance_to_zone), 
            by = "CRASH_RECORD_ID")







## Count cameras by year to help us determine a before and after date for Avg Affect of Treatment
cameras_year_counts <- table(cameras$year)
print(cameras_year_counts)

## Count crashes by year
crashes_year_counts <- table(crashes_clean$year)
print(crashes_year_counts)



## Count crashes near cameras and safety zones
treated_crashes <- sum(crashes_clean$distance_to_camera.x < 201, na.rm = TRUE)
print(treated_crashes)
sfzone_crashes <- sum(crashes_clean$distance_to_sfzone.y < 201, na.rm = TRUE)
print(sfzone_crashes)


# Convert safety zones and cameras to sf objects
safety_zones_sf <- st_as_sf(chicago_safety_zones, coords = c("Longitude", "Latitude"), crs = 4326)
cameras_sf <- st_as_sf(cameras, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Find the nearest safety zone for each camera
nearest_safety_zone <- st_nearest_feature(cameras_sf, safety_zones_sf)

# Create a dataframe of camera assignments
camera_assignments <- data.frame(
  camera_id = cameras_sf$ID,
  safety_zone_name = safety_zones_sf$Name[nearest_safety_zone]
)

# Join camera information to safety zones
safety_zones_with_cameras <- safety_zones_sf %>%
  left_join(camera_assignments, by = c("Name" = "safety_zone_name")) %>%
  left_join(
    cameras_sf %>% 
      st_drop_geometry() %>% 
      select(ID, GO.LIVE.DATE, LOCATION.ID),
    by = c("camera_id" = "ID")
  )

# Replace NA with "N/A" for camera-related columns
safety_zones_with_cameras <- safety_zones_with_cameras %>%
  mutate(across(c(camera_id, GO.LIVE.DATE, LOCATION.ID), ~ifelse(is.na(.), "N/A", as.character(.))))

# View the result
print(head(safety_zones_with_cameras))



# Create spatial objects for analysis
safety_zones_sf <- st_as_sf(safety_zones_with_cameras, coords = c("Longitude", "Latitude"), crs = 4326)
safety_zones_sf$orig_lon <- st_coordinates(safety_zones_sf)[,1]
safety_zones_sf$orig_lat <- st_coordinates(safety_zones_sf)[,2]
safety_zones_buffer <- st_buffer(safety_zones_sf, dist = 201)



# Perform spatial analysis
crashes_in_buffer <- st_join(crashes_sf, safety_zones_buffer)

# Filter out crashes with missing or blank zone names
crashes_in_buffer_filtered <- crashes_in_buffer %>%
  filter(!is.na(Name) & Name != "")

crash_counts_by_zone <- crashes_in_buffer_filtered %>%
  group_by(Name) %>%
  summarise(crash_count = n()) %>%
  st_drop_geometry() 



camera_data <- cameras %>%
  select(Name = LOCATION.ID, go_live_date = GO.LIVE.DATE) %>%
  mutate(go_live_date = as.character(go_live_date))  # Convert date to character if needed

# Join crash counts and camera data to the filtered crashes
crashes_with_counts <- crashes_in_buffer_filtered %>%
  left_join(crash_counts_by_zone, by = "Name") %>%
  mutate(crash_count = ifelse(is.na(crash_count), 0, crash_count)) %>%  # Handle NA values for crash count
  left_join(camera_data, by = "Name") %>%  # Join camera data
  mutate(go_live_date = ifelse(is.na(go_live_date), "N/A", go_live_date))  # Handle NA values for go_live_date

# Check the result
print(head(crashes_with_counts))


#transform dates to right format
crashes_in_buffer$CRASH_DATE <- as.POSIXct(crashes_in_buffer$CRASH_DATE, format = "%m/%d/%Y %I:%M:%S %p")


# Monthly analysis
crashes_in_buffer <- crashes_in_buffer_filtered %>%
  mutate(
    crash_year = lubridate::year(CRASH_DATE),
    crash_month = lubridate::month(CRASH_DATE),
  )

crash_counts_monthly <- crashes_in_buffer_filtered %>%
  group_by(Name, crash_year, crash_month) %>%
  summarise(
    total_crashes = n(),
    serious_fatal_crashes = sum(fatalities, na.rm = TRUE),
    bike_ped_crashes = sum(bikers_pedestrians, na.rm = TRUE),
    speed_related_crashes = sum(PRIM_CONTRIBUTORY_CAUSE=='FAILING TO REDUCE SPEED TO AVOID CRASH', na.rm = TRUE),
    youth_related_crashes = sum(under_18, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  st_drop_geometry()

crashes_sf$CRASH_DATE <- as.Date(crashes_sf$CRASH_DATE, format = "%Y-%m-%d")


crash_counts_monthly <- crash_counts_monthly %>%
  mutate(crash_year = as.integer(crash_year))

crash_counts_monthly <- crash_counts_monthly %>%
  mutate(crash_month = as.integer(crash_month))


# Create a complete sequence of all year-month combinations
start_year <- min(crash_counts_monthly$crash_year, na.rm = TRUE)
end_year <- max(crash_counts_monthly$crash_year, na.rm = TRUE)

all_zones <- unique(crashes_in_buffer_filtered$Name)

all_months <- expand_grid(
  crash_year = start_year:end_year,
  crash_month = 1:12
) %>%
  arrange(crash_year, crash_month)

# Create a dataframe with all combinations of zones and dates
complete_zones_monthly <- expand_grid(
  Name = all_zones,
  crash_year = start_year:end_year,
  crash_month = 1:12
)

# Join the complete list with the crash counts
complete_crash_counts <- complete_zones_monthly %>%
  left_join(crash_counts_monthly, by = c("Name", "crash_year", "crash_month")) %>%
  mutate(total_crashes = replace_na(total_crashes, 0),
         serious_fatal_crashes = replace_na(serious_fatal_crashes, 0),
         bike_ped_crashes = replace_na(bike_ped_crashes, 0),
         speed_related_crashes = replace_na(speed_related_crashes, 0),
         youth_related_crashes = replace_na(youth_related_crashes, 0))


# Perform the join allowing the many-to-many relationship
complete_crash_counts <- complete_crash_counts %>%
  left_join(safety_zones_with_cameras %>% 
              select(Name, GO.LIVE.DATE), 
            by = "Name", 
            relationship = "many-to-many")





# Assuming the column High_Youth_Census_Tract is present in the dataframe
cameras_monthly_crashes$High_Youth_Census_Tract <- 0  # Sample value for High-Youth Census Tract



# Calculate the Safety Zone Score
crash_counts_monthly$Safety_Zone_Score <- cameras_monthly_crashes$total_crashes +
  crash_counts_monthly$serious_fatal_crashes +
  crash_counts_monthly$bike_ped_crashes +
  2 * crash_counts_monthly$speed_related_crashes +
  2 * crash_counts_monthly$youth_related_crashes +
  cameras_monthly_crashes$High_Youth_Census_Tract

cols <- c(
  names(cameras_monthly_crashes)[1:9],
  "High_Youth_Census_Tract",
  "Safety_Zone_Score",
  names(cameras_monthly_crashes)[10:(ncol(cameras_monthly_crashes)-2)]
)
cameras_monthly_crashes <- cameras_monthly_crashes[, cols]


# Print the updated data frame
print(df)


