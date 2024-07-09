install.packages("ggmap")
library(ggmap)

install.packages("geosphere")
library(geosphere)

install.packages('sf')
library(sf)

library(dplyr)

install.packages(c('rgdal'))

library(ggplot2)

install.packages('tigris')
library(tigris)

install.packages('rgdal')
library(rgdal)

install.packages('lubridate')
library(lubridate)




crashes <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_Crashes_20240408.csv')

cameras <- read.csv('/Users/grant.maleski/Downloads/Speed_Camera_Locations_20240408.csv')

chicago_safety_zones <- read.csv('/Users/grant.maleski/Downloads/chicago_safety_areas.csv')

chicago_map <- read_sf("/Users/grant.maleski/Downloads/WARDS_2015")

people <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_People.csv')

register_google(key = "AIzaSyBFtOlgqh_2oOpGILbJJEHVn-VzqJEP1rI")


# some of the addresses don't feed into the Google Maps API properly so I'm manually changing so that we can get their Lat/longitude when feeding it into the API later on

chicago_safety_zones[367,3] <- '3000 S Martin Luther King Dr'
chicago_safety_zones[1402,3] <- '7001 West 59th St'



# Read the addresses from the CSV file
addresses <- chicago_safety_zones$Address

#add in chicago information
chicago_addresses <- paste(addresses, ", Chicago, IL", sep = "")





# Geocode addresses
geocodes <- geocode(chicago_addresses)

geocoded_df <- data.frame(Zone_Name = chicago_safety_zones$Name ,Address = chicago_addresses, Latitude = geocodes$lat, Longitude = geocodes$lon)


#------clean up and organize data


crash_location_list <- data.frame(CRASH_RECORD_ID = crashes$CRASH_RECORD_ID ,Latitude = crashes$LATITUDE, Longitude = crashes$LONGITUDE)

chicago_safety_zones_list <- data.frame(Latitude = chicago_safety_zones$Latitude, Longitude = chicago_safety_zones$Longitude)

camera_location_list <- data.frame(Latitude = cameras$LATITUDE, Longitude = cameras$LONGITUDE)

# Add latitude and longitude to the original data frame


chicago_safety_zones$Latitude <- geocodes$lat
chicago_safety_zones$Longitude <- geocodes$lon


#---------calculate distance from each crash to the nearest camera


# Define the Coordinate Reference System (CRS) - using WGS84 as an example
crs <- st_crs("+proj=longlat +datum=WGS84")

# Option 1: Remove rows with missing values in crash_location_list
crash_location_list_filtered <- crash_location_list %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Convert filtered data to sf object
crash_location_sf <- st_as_sf(crash_location_list_filtered, coords = c("Longitude", "Latitude"), crs = crs)

# Convert camera_location_list to sf object
camera_location_list_sf <- st_as_sf(camera_location_list, coords = c("Longitude", "Latitude"), crs = crs)

# Calculate nearest neighbors
nearest_indices_camera <- st_nearest_feature(crash_location_sf, camera_location_list_sf)
nearest_neighbors_camera <- camera_location_list_sf[nearest_indices_camera, ]

# Extract information from nearest neighbors object
crash_distance_to_camera <- data.frame(
  CRASH_RECORD_ID = crash_location_list_filtered$CRASH_RECORD_ID,
  Latitude_df1 = st_coordinates(crash_location_sf)[, 2],  # Latitude
  Longitude_df1 = st_coordinates(crash_location_sf)[, 1], # Longitude
  Latitude_closest = st_coordinates(nearest_neighbors_camera)[, 2],  # Closest zone - Latitude
  Longitude_closest = st_coordinates(nearest_neighbors_camera)[, 1], # Closest zone - Longitude
  distance_to_crash = as.numeric(st_distance(crash_location_sf, nearest_neighbors_camera, by_element = TRUE))  # Distance
)

# Optional: Warning about removed rows if using option 1
if (nrow(crash_location_list) != nrow(crash_location_list_filtered)) {
  warning(paste("Removed", nrow(crash_location_list) - nrow(crash_location_list_filtered), 
                "rows from crash_location_list due to missing latitude or longitude values."))
}


#---------calculate distance from each crash to the nearest safety point


# Define the Coordinate Reference System (CRS) - using WGS84 as an example
crs <- st_crs("+proj=longlat +datum=WGS84")

# Option 1: Remove rows with missing values in crash_location_list
crash_location_list_filtered <- crash_location_list %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Convert filtered data to sf object
crash_location_sf <- st_as_sf(crash_location_list_filtered, coords = c("Longitude", "Latitude"), crs = crs)

chicago_safety_zones_list_filtered <- chicago_safety_zones_list %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Create the spatial object using the filtered data
chicago_safety_zones_list_sf <- st_as_sf(chicago_safety_zones_list_filtered, coords = c("Longitude", "Latitude"), crs = crs)

# Calculate nearest neighbors
nearest_indices_zone <- st_nearest_feature(crash_location_sf, chicago_safety_zones_list_sf)
nearest_neighbors_zone <- chicago_safety_zones_list_sf[nearest_indices_zone, ]

# Extract information from nearest neighbors object
safety_closest_points <- data.frame(
  CRASH_RECORD_ID = crash_location_list_filtered$CRASH_RECORD_ID,
  Latitude_df1 = st_coordinates(crash_location_sf)[, 2],  # Latitude
  Longitude_df1 = st_coordinates(crash_location_sf)[, 1], # Longitude
  Latitude_closest = st_coordinates(nearest_neighbors_zone)[, 2],  # Closest zone - Latitude
  Longitude_closest = st_coordinates(nearest_neighbors_zone)[, 1], # Closest zone - Longitude
  distance_to_zone = as.numeric(st_distance(crash_location_sf, nearest_neighbors_zone, by_element = TRUE))  # Distance
)

# Optional: Warning about removed rows if using option 1
if (nrow(crash_location_list) != nrow(crash_location_list_filtered)) {
  warning(paste("Removed", nrow(crash_location_list) - nrow(crash_location_list_filtered), 
                "rows from crash_location_list due to missing latitude or longitude values."))
}


#---append distance to camera and safety zone to crashes df

crashes <- crashes %>%
  left_join(crash_distance_to_camera %>% select(CRASH_RECORD_ID, distance_to_crash) %>% rename(distance_to_camera = distance_to_crash), by = "CRASH_RECORD_ID")


crashes <- crashes %>%
  left_join(safety_closest_points %>% select(CRASH_RECORD_ID, distance_to_zone) %>% rename(distance_to_sfzone = distance_to_zone), by = "CRASH_RECORD_ID")





####visualize dif in columns
# Calculate the absolute difference between the two columns
crashes$absolute_difference <- abs(crashes$distance_to_camera - crashes$distance_to_sfzone)

# Sort the dataframe by the absolute difference in descending order
crashes_sorted <- crashes[order(-crashes$absolute_difference), ]

# Print the dataframe to see the biggest differences
print(crashes_sorted)



#------


cameras$go.live.date <- as.Date(cameras$GO.LIVE.DATE, format = "%m/%d/%Y")

# Order the dataset by the 'go.live.date' column
ordered_cameras <- cameras[order(cameras$GO.LIVE.DATE), ]


cameras$year <- format(cameras$go.live.date, "%Y")
cameras$month <- format(cameras$go.live.date, "%m")
cameras$day <- format(cameras$go.live.date, "%d")

cameras_year_counts <- table(cameras$year)

# Print the counts
print(cameras_year_counts)

#find out how many crash entries have a lat/long

non_null_count <- sum(is.na(crashes$LATITUDE))


crashes$CRASH_DATE <- as.Date(crashes$CRASH_DATE, format = "%m/%d/%Y")


crashes$year <- format(crashes$CRASH_DATE, "%Y")
crashes$month <- format(crashes$CRASH_DATE, "%m")
crashes$day <- format(crashes$CRASH_DATE, "%d")


crashes_year_counts <- table(crashes$year)

# Print the counts
print(crashes_year_counts)


#---calculate crashes near sf/camera


treated_crashes <- sum(crashes$distance_to_camera < 201, na.rm = TRUE)

# Print the result
print(treated_crashes)


sfzone_crashes <- sum(crashes$distance_to_sfzone < 201, na.rm = TRUE)


print(sfzone_crashes)

#more crashes happen near a camera than they do near a safety zone




# Convert camera locations to sf object
cameras_sf <- st_as_sf(cameras, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Store original coordinates
cameras_sf$orig_lon <- st_coordinates(cameras_sf)[,1]
cameras_sf$orig_lat <- st_coordinates(cameras_sf)[,2]

# Create 1/8th mile (approx. 201 meters) buffer around each camera
cameras_buffer <- st_buffer(cameras_sf, dist = 201)

# Filter out crashes with missing coordinates
crashes_clean <- crashes %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))

# Convert filtered crash locations to sf object
crashes_sf <- st_as_sf(crashes_clean, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Perform spatial join to count crashes within each buffer
crashes_in_buffer <- st_join(crashes_sf, cameras_buffer)

# Count crashes for each camera
crash_counts <- crashes_in_buffer %>%
  group_by(ID) %>%  # Use the correct camera ID column name
  summarise(crash_count = n()) %>%
  st_drop_geometry()  # Remove geometry from crash_counts

# Join the counts back to the original camera data using a regular join
cameras_with_counts <- cameras_buffer %>%
  left_join(crash_counts, by = "ID")

# Replace NA with 0 for cameras with no crashes in their buffer
cameras_with_counts$crash_count[is.na(cameras_with_counts$crash_count)] <- 0

# Restore original coordinates
cameras_with_counts <- cameras_with_counts %>%
  mutate(LONGITUDE = orig_lon,
         LATITUDE = orig_lat) %>%
  select(-orig_lon, -orig_lat)

# View the results
print(cameras_with_counts)

# If you need it as a non-spatial dataframe:
cameras_with_counts_df <- st_drop_geometry(cameras_with_counts)


####----------------- do it on a monthly basis

# Ensure CRASH_DATE is in Date format
crashes_sf$CRASH_DATE <- as.Date(crashes_sf$CRASH_DATE)

# Perform spatial join to identify crashes within each camera buffer
crashes_in_buffer <- st_join(crashes_sf, cameras_buffer)

# Extract year and month from crash date
crashes_in_buffer <- crashes_in_buffer %>%
  mutate(
    crash_year = lubridate::year(CRASH_DATE),
    crash_month = lubridate::month(CRASH_DATE)
  )

# Count crashes for each camera by year and month
crash_counts_monthly <- crashes_in_buffer %>%
  group_by(ID, crash_year, crash_month) %>%
  summarise(crash_count = n()) %>%
  ungroup() %>%
  st_drop_geometry()

# Create a complete dataset with all camera-month combinations
all_cameras <- unique(cameras_sf$ID)
date_range <- seq(floor_date(min(crashes_sf$CRASH_DATE), "month"),
                  ceiling_date(max(crashes_sf$CRASH_DATE), "month") - days(1),
                  by = "month")
all_combinations <- expand.grid(
  ID = all_cameras,
  date = date_range
)

# Add year and month columns to all_combinations
all_combinations <- all_combinations %>%
  mutate(
    crash_year = lubridate::year(date),
    crash_month = lubridate::month(date)
  )

# Join the crash counts to all combinations
cameras_monthly_crashes <- all_combinations %>%
  left_join(crash_counts_monthly, by = c("ID", "crash_year", "crash_month"))

# Replace NA with 0 for months with no crashes
cameras_monthly_crashes <- cameras_monthly_crashes %>%
  mutate(crash_count = ifelse(is.na(crash_count), 0, crash_count))

# Add camera details if needed
cameras_monthly_crashes <- cameras_monthly_crashes %>%
  left_join(st_drop_geometry(cameras_sf), by = "ID")

# Ensure the data is sorted by camera ID and date
cameras_monthly_crashes <- cameras_monthly_crashes %>%
  arrange(ID, date)

# View the results
print(head(cameras_monthly_crashes, 20))
