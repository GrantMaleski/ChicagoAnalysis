# Load required packages
install.packages(c("tidygeocoder", "geosphere", "sf", "tigris","tidygeocoder", "lubridate","tidyverse","MatchIt"))
library(tidygeocoder)
library(geosphere)
library(sf)
library(ggplot2)
library(tigris)
library(lubridate)
library(dplyr)
library(tidygeocoder)
library(tidyverse)
library(MatchIt)



# Load data
crashes <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_Crashes_20240927.csv')
cameras <- read.csv('/Users/grant.maleski/Downloads/Speed_Camera_Locations_20240408.csv')
chicago_safety_zones <- read.csv('/Users/grant.maleski/Downloads/chicago_safety_areas.csv')
chicago_map <- read_sf("/Users/grant.maleski/Downloads/WARDS_2015")
people <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_People_20240927.csv')
census_information <- read.csv("/Users/grant.maleski/Downloads/Chicago_Population_Counts_20240927.csv", check.names = FALSE)

duplicate_address <- chicago_safety_zones$Address[duplicated(chicago_safety_zones$Address)]
length(unique(duplicate_address))





# Keep only one occurrence of duplicated addresses
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Antonia Pantoja HS" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Edgebrook" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Hale" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Orozco" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Pasteur" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Rogers" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Skinner" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "White" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Wildwood" & duplicated(chicago_safety_zones$Name)), ]
chicago_safety_zones <- chicago_safety_zones[!(chicago_safety_zones$Name == "Williams" & duplicated(chicago_safety_zones$Name)), ]



#Rename zones with the same name
chicago_safety_zones[chicago_safety_zones$Address == "7535 S Dobson Ave" & chicago_safety_zones$Name == "Adams", "Name"] <- "Grand Crossing Park"
chicago_safety_zones[chicago_safety_zones$Address == "10810 S Ave H" & chicago_safety_zones$Name == "Addams", "Name"] <-"Jane Addams Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "1340 W 71St St" & chicago_safety_zones$Name == "Altgeld", "Name"] <-"Wentworth Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "950 W 33rd Pl" & chicago_safety_zones$Name == "Armour", "Name"] <-"Armour Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "2945 N Sawyer Ave" & chicago_safety_zones$Name == "Avondale", "Name"] <-"Avondale Public School"
chicago_safety_zones[chicago_safety_zones$Address == "10354 S Charles St" & chicago_safety_zones$Name == "Barnard", "Name"] <-"Alice L . Barnard Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "3730 N Oakley Ave" & chicago_safety_zones$Name == "Bell", "Name"] <-"Alexander Graham Bell School"
chicago_safety_zones[chicago_safety_zones$Address == "4542 S Greenwood Ave" & chicago_safety_zones$Name == "Brooks", "Name"] <-"Brooks (Gwendolyn) Park"
chicago_safety_zones[chicago_safety_zones$Address == "5522 N Milwaukee Ave" & chicago_safety_zones$Name == "Care A Lot" , "Name"] <-"Care-A-Lot Early Learning Center"
chicago_safety_zones[chicago_safety_zones$Address == "1250 W Erie St" & chicago_safety_zones$Name == "Carpenter" , "Name"] <-"Ogden International High School"
chicago_safety_zones[chicago_safety_zones$Address == "2021 N Point St" & chicago_safety_zones$Name == "Chase" , "Name"] <-"Salmon P Chase Public School"
chicago_safety_zones[chicago_safety_zones$Address == "212 S Francisco Ave" & chicago_safety_zones$Name == "Chicago Jesuit Acad" , "Name"] <-"Marillac St. Vincent (East Garfield Park)"
chicago_safety_zones[chicago_safety_zones$Address == "2450 W Rice St" & chicago_safety_zones$Name == "Chopin" , "Name"] <-"Frederic Chopin Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "3400 N Rockwell Ave" & chicago_safety_zones$Name == "Clark" , "Name"] <-"Clark (Richard) Park"
chicago_safety_zones[chicago_safety_zones$Address == "1045 S Monitor Ave" & chicago_safety_zones$Name == "Clark" , "Name"] <-"G R Clark Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "1003 N Leavitt St" & chicago_safety_zones$Name == "Columbus" , "Name"] <-"Christopher Columbus School"
chicago_safety_zones[chicago_safety_zones$Address == "1624 W 19th St" & chicago_safety_zones$Name == "Cooper" , "Name"] <-"Peter Cooper Public School"
chicago_safety_zones[chicago_safety_zones$Address == "1809 W 50th St" & chicago_safety_zones$Name == "Cornell" , "Name"] <-"Cornell (Paul) Square Park"
chicago_safety_zones[chicago_safety_zones$Address == "5427 W Division St" & chicago_safety_zones$Name == "Davis" , "Name"] <-"Davis (Margaret) Park"
chicago_safety_zones[chicago_safety_zones$Address == "3810 W 81St Pl" & chicago_safety_zones$Name == "Dawes" , "Name"] <-"Dawes Elementary Scool"
chicago_safety_zones[chicago_safety_zones$Address == "1550 S State St" & chicago_safety_zones$Name == "Daystar School" , "Name"] <-"Daystar Academy"
chicago_safety_zones[chicago_safety_zones$Address == "8306 S St Lawrence Ave" & chicago_safety_zones$Name == "Dixon" , "Name"] <-"Dixon Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "6755 N Northwest Hwy" & chicago_safety_zones$Name == "Edison" , "Name"] <-"Edison (Thomas Alva) Park"
chicago_safety_zones[chicago_safety_zones$Address == "3537 S Paulina St" & chicago_safety_zones$Name == "Evergreen" , "Name"] <-"Evergreen Academy Middle School"
chicago_safety_zones[chicago_safety_zones$Address == "10041 S Union Ave" & chicago_safety_zones$Name == "Fernwood" , "Name"] <-"Fernwood Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "7019 N Ashland Bl" & chicago_safety_zones$Name == "Field" , "Name"] <-"Eugene Field Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "331 W 45th St" & chicago_safety_zones$Name == "Fuller" , "Name"] <-"Fuller (Melville) Park"
chicago_safety_zones[chicago_safety_zones$Address == "5421 N Menard Ave" & chicago_safety_zones$Name == "Gladstone" , "Name"] <-"Gladstone (William) Park"
chicago_safety_zones[chicago_safety_zones$Address == "4222 W Foster Ave" & chicago_safety_zones$Name == "Gompers" , "Name"] <-"Gompers (Samuel) Park"
chicago_safety_zones[chicago_safety_zones$Address == "5120 N Winthrop Ave" & chicago_safety_zones$Name == "Goudy" , "Name"] <-"William C Goudy Public School"
chicago_safety_zones[chicago_safety_zones$Address == "1650 W Cornelia Ave" & chicago_safety_zones$Name == "Hamilton" , "Name"] <-"Hamilton Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "6200 S Drexel Ave" & chicago_safety_zones$Name == "Harris" , "Name"] <-"Harris (Harriet) Park"
chicago_safety_zones[chicago_safety_zones$Address == "3417 S Hamilton Ave" & chicago_safety_zones$Name == "Hoyne" , "Name"] <-"Hoyne (Thomas) Park"
chicago_safety_zones[chicago_safety_zones$Address == "3849 W 69th Pl" & chicago_safety_zones$Name == "Hurley" , "Name"] <-"Hurley Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "1431 N Park Ave" & chicago_safety_zones$Name == "Immaculate Conception" , "Name"] <-"Immaculate Conception Catholic Church (ICSJ Parish)"
chicago_safety_zones[chicago_safety_zones$Address == "8385 S Birkhoff Ave" & chicago_safety_zones$Name == "Jackson" , "Name"] <-"Jackson (Mahalia) Park"
chicago_safety_zones[chicago_safety_zones$Address == "4822 N Long Ave" & chicago_safety_zones$Name == "Jefferson" , "Name"] <-"Jefferson (Thomas) Memorial Park"
chicago_safety_zones[chicago_safety_zones$Address == "1640 S Jefferson St" & chicago_safety_zones$Name == "Jefferson" , "Name"] <-"Jefferson Park Playground"
chicago_safety_zones[chicago_safety_zones$Address == "5036 S Blackstone Ave" & chicago_safety_zones$Name == "Just For Kids" , "Name"] <-"Kenwood Academy High School"
chicago_safety_zones[chicago_safety_zones$Address == "2725 W 41St St" & chicago_safety_zones$Name == "Kelly" , "Name"] <-"Kelly (Edward) Park"
chicago_safety_zones[chicago_safety_zones$Address == "1212 W 77th St" & chicago_safety_zones$Name == "King" , "Name"] <-"Dr. Martin Luther King, Jr. Park"
chicago_safety_zones[chicago_safety_zones$Address == "6448 S Tripp Ave" & chicago_safety_zones$Name == "Lee" , "Name"] <-"Richard Henry Lee Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "8050 S Chappel Ave" & chicago_safety_zones$Name == "Mann" , "Name"] <-"Horace Mann School"
chicago_safety_zones[chicago_safety_zones$Address == "4100 W West End Ave" & chicago_safety_zones$Name == "Mason" , "Name"] <-"Mason (Elizabeth) Park"
chicago_safety_zones[chicago_safety_zones$Address == "11710 S Morgan St" & chicago_safety_zones$Name == "Morgan" , "Name"] <-"Morgan Field Park"
chicago_safety_zones[chicago_safety_zones$Address == "2036 N Avers Ave" & chicago_safety_zones$Name == "Mozart" , "Name"] <-"Mozart (Amadeus) Park"
chicago_safety_zones[chicago_safety_zones$Address == "4837 W Erie St" & chicago_safety_zones$Name == "Nash" , "Name"] <-"Nash Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "429 N Columbus Dr" & chicago_safety_zones$Name == "Ogden" , "Name"] <-"Ogden Plaza Park"
chicago_safety_zones[chicago_safety_zones$Address == "200 E 111th St" & chicago_safety_zones$Name == "Palmer" , "Name"] <-"Palmer Park"
chicago_safety_zones[chicago_safety_zones$Address == "5510 N Christiana Ave" & chicago_safety_zones$Name == "Peterson" , "Name"] <-"Peterson Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "11311 S Forrestville Ave" & chicago_safety_zones$Name == "Pullman" , "Name"] <-"Pullman Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "2509 W Irving Park Rd" & chicago_safety_zones$Name == "Revere" , "Name"] <-"Revere (Paul) Park"
chicago_safety_zones[chicago_safety_zones$Address == "4225 S Lake Park Ave" & chicago_safety_zones$Name == "Robinson" , "Name"] <-"Jackie Robinson Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "6250 N Sheridan Rd" & chicago_safety_zones$Name == "Sacred Heart School" , "Name"] <-"Sacred Heart School- Sheridan"
chicago_safety_zones[chicago_safety_zones$Address == "6040 N Kilpatrick Ave" & chicago_safety_zones$Name == "Sauganash" , "Name"] <-"Sauganash Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "4600 S Hermitage Ave" & chicago_safety_zones$Name == "Seward" , "Name"] <-"William H. Seward Communication Arts Academy Elementary School"
chicago_safety_zones[chicago_safety_zones$Address == "1307 W 52nd St" & chicago_safety_zones$Name == "Sherman" , "Name"] <-"Sherman Park"
chicago_safety_zones[chicago_safety_zones$Address == "245 W 57th St" & chicago_safety_zones$Name == "Sherwood" , "Name"] <-"Sherwood (Jesse) Park"
chicago_safety_zones[chicago_safety_zones$Address == "1443 N. Ogden Ave" & chicago_safety_zones$Name == "Skinner" , "Name"] <-"Skinner (Mark) Park"
chicago_safety_zones[chicago_safety_zones$Address == "744 E 103rd St" & chicago_safety_zones$Name == "Smith" , "Name"] <-"Wendell Smith School"
chicago_safety_zones[chicago_safety_zones$Address == "9912 S Princeton Ave" & chicago_safety_zones$Name == "Smith" , "Name"] <-"Smith (Wendell) Park"
chicago_safety_zones[chicago_safety_zones$Address == "2526 W Grand Ave" & chicago_safety_zones$Name == "Smith" , "Name"] <-"Smith (Joseph Higgins) Park"
chicago_safety_zones[chicago_safety_zones$Address == "100 W 47th St" & chicago_safety_zones$Name == "Taylor" , "Name"] <-"Robert Taylor Park"
chicago_safety_zones[chicago_safety_zones$Address == "223 N Keeler Ave" & chicago_safety_zones$Name == "Tilton" , "Name"] <-"George W Tilton Public School"
chicago_safety_zones[chicago_safety_zones$Address == "2400 E 105th St" & chicago_safety_zones$Name == "Trumbull" , "Name"] <-"Trumbull (Lyman) Park"
chicago_safety_zones[chicago_safety_zones$Address == "2031 S Peoria St" & chicago_safety_zones$Name == "Walsh" , "Name"] <-"John A Walsh Public School"
chicago_safety_zones[chicago_safety_zones$Address == "6621 N Western Ave" & chicago_safety_zones$Name == "Warren" , "Name"] <-"Warren (Laurence) Park"
chicago_safety_zones[chicago_safety_zones$Address == "5531 S MLK Jr. Dr" & chicago_safety_zones$Name == "Washington" , "Name"] <-"Washington (George) Park"
chicago_safety_zones[chicago_safety_zones$Address == "5625 S Mobile Ave" & chicago_safety_zones$Name == "Wentworth" , "Name"] <-"Wentworth (John) Park"

chicago_safety_zones[chicago_safety_zones$Address == "8215 S Euclid Ave" & chicago_safety_zones$Name == "Washington" , "Name"] <-"Washington (Dinah) Park"
chicago_safety_zones[chicago_safety_zones$Address == "3770 S Wentworth Ave" & chicago_safety_zones$Name == "Wentworth" , "Name"] <-"Wentworth Gardens (John) Park"
chicago_safety_zones[chicago_safety_zones$Address == "400 W 123rd St" & chicago_safety_zones$Name == "West Pullman" , "Name"] <-"West Pullman Park"
chicago_safety_zones[chicago_safety_zones$Address == "4630 N Milwaukee Ave" & chicago_safety_zones$Name == "Wilson" , "Name"] <-"Wilson (Frank J.) Park"
chicago_safety_zones[chicago_safety_zones$Address == "1122 W 34th Pl" & chicago_safety_zones$Name == "Wilson" , "Name"] <-"Wilson (John P.) Park"




## Geocode addresses via Google Maps API
addresses <- chicago_safety_zones$Address
chicago_addresses <- paste(addresses, ", Chicago, IL", sep = "")
geocoded_df <- tibble(address = chicago_addresses) %>%
  geocode(address, method = 'osm', full_results = TRUE)

coordinates_df <- data.frame(lat = geocoded_df$lat, lon = geocoded_df$long)

# Perform reverse geocoding to get zip codes
geocoded_with_zip <- reverse_geocode(
  .tbl = coordinates_df,  # Pass the data frame with lat/lon
  lat = "lat",            # Specify the latitude column
  long = "lon",           # Specify the longitude column
  method = "osm",         # Method for reverse geocoding
  full_results = TRUE
)


chicago_safety_zones_list <- data.frame(
  Name = chicago_safety_zones$Name,
  Address = chicago_addresses, 
  Latitude = geocoded_with_zip$lat, 
  Longitude = geocoded_with_zip$lon,
  Zip_Code = geocoded_with_zip$postcode
)





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








camera_location_list <- data.frame(Latitude = cameras$LATITUDE, 
                                   Longitude = cameras$LONGITUDE)



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







##Count cameras by year to help us determine a before and after date for Avg Affect of Treatment
cameras_year_counts <- table(cameras$year)
print(cameras_year_counts)

## Count crashes by year
crashes_year_counts <- table(crashes_clean$crash_year)
print(crashes_year_counts)



## Count crashes near cameras and safety zones
treated_crashes <- sum(crashes_clean$distance_to_camera.x < 201, na.rm = TRUE)
print(treated_crashes)
sfzone_crashes <- sum(crashes_clean$distance_to_sfzone.y < 201, na.rm = TRUE)
print(sfzone_crashes)


# Convert safety zones and cameras to sf objects
cameras_sf <- st_as_sf(cameras, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Find the nearest safety zone for each camera
nearest_safety_zone <- st_nearest_feature(cameras_sf, chicago_safety_zones_list_sf)

# Create a dataframe of camera assignments
camera_assignments <- data.frame(
  camera_id = cameras_sf$ID,
  safety_zone_name = chicago_safety_zones_list_sf$Name[nearest_safety_zone]
)

# Join camera information to safety zones
safety_zones_with_cameras <- chicago_safety_zones_list_sf %>%
  left_join(camera_assignments, by = c("Name" = "safety_zone_name")) %>%
  left_join(
    cameras_sf %>% 
      st_drop_geometry() %>% 
      select(ID, GO.LIVE.DATE, LOCATION.ID),
    by = c("camera_id" = "ID")
  )

# Replace NA with "N/A" for camera-related columns
safety_zones_with_cameras <- safety_zones_with_cameras %>%
  mutate(across(c(camera_id, GO.LIVE.DATE, LOCATION.ID), ~ifelse(is.na(.), NA, as.character(.))))





# Create spatial objects for analysis
safety_zones_sf <- st_as_sf(safety_zones_with_cameras, coords = c("Longitude", "Latitude"), crs = 4326)
safety_zones_sf$orig_lon <- st_coordinates(safety_zones_sf)[,1]
safety_zones_sf$orig_lat <- st_coordinates(safety_zones_sf)[,2]
safety_zones_buffer <- st_buffer(safety_zones_sf, dist = 201)



# Perform spatial analysis to see how many crasehs in safety zone
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
  mutate(go_live_date = ifelse(is.na(go_live_date), NA, go_live_date))  # Handle NA values for go_live_date



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

start_year <- min(crashes$crash_year)
end_year <- max(crashes$crash_year)


safety_zones_with_cameras <- safety_zones_with_cameras %>%
  mutate(GO.LIVE.DATE = as.Date(GO.LIVE.DATE))  # Ensure GO.LIVE.DATE is in Date format

# Now filter for treated zones with GO.LIVE.DATE after 2018 or NA
unique_zones <- unique(safety_zones_with_cameras %>%
                         filter(is.na(GO.LIVE.DATE) | GO.LIVE.DATE >= as.Date("2018-01-01")) %>%
                         .$Name)  # Extract the 'Name' column after filtering

# Create a dataframe with all combinations of zones and dates, including crash_date
complete_zones_monthly <- expand_grid(
  Name = unique_zones,
  crash_year = start_year:end_year,
  crash_month = 1:12
) %>%
  # Create a crash_date column by combining crash_year and crash_month
  mutate(crash_date = as.Date(paste(crash_year, crash_month, "01", sep = "-"))) %>%
  arrange(crash_date)

# Join the complete list with the crash counts, including both Name and Zip_Code
complete_crash_counts <- complete_zones_monthly %>%
  left_join(crash_counts_monthly, by = c("Name", "crash_year", "crash_month")) %>%
  mutate(total_crashes = replace_na(total_crashes, 0),
         serious_fatal_crashes = replace_na(serious_fatal_crashes, 0),
         bike_ped_crashes = replace_na(bike_ped_crashes, 0),
         speed_related_crashes = replace_na(speed_related_crashes, 0),
         youth_related_crashes = replace_na(youth_related_crashes, 0))





safety_zones_with_cameras_unique <- safety_zones_with_cameras %>%
  group_by(Name) %>%
  slice(1) %>%  # Keep the first occurrence of each Name
  ungroup()


complete_crash_counts <- complete_crash_counts %>%
  left_join(safety_zones_with_cameras_unique %>% 
              select(Name, Zip_Code, GO.LIVE.DATE), 
            by = c("Name"))


#see if safety zones are high census or not

census_information <- census_information %>%
  filter(`Geography` == "ZIP Code") %>%
  mutate(
    youth_population = `Population - Age 0-17`,
    total_population = `Population - Total`,
    youth_percentage = (youth_population / total_population) * 100
  )


#determined by the city
high_youth_threshold <- 30

# Flag ZIP codes with high youth percentage
census_information <- census_information %>%
  mutate(high_youth_census = ifelse(youth_percentage >= 30, 20, 0))


# Join the high youth flag to complete_crash_counts
complete_crash_counts <- complete_crash_counts %>%
  left_join(census_information %>%
              select(Geography, high_youth_census), 
            by = c("Zip_Code" = "Geography")) %>%
  mutate(high_youth_census = replace_na(high_youth_census, 0))






# Calculate the Safety Zone Score
complete_crash_counts$Safety_Zone_Score <- complete_crash_counts$total_crashes +
  complete_crash_counts$serious_fatal_crashes +
  complete_crash_counts$bike_ped_crashes +
  2 * complete_crash_counts$speed_related_crashes +
  2 * complete_crash_counts$youth_related_crashes +
  complete_crash_counts$high_youth_census

# Reorder columns
complete_crash_counts <- complete_crash_counts %>%
  select(Name, Zip_Code, crash_year, crash_month, total_crashes, serious_fatal_crashes, bike_ped_crashes, speed_related_crashes, youth_related_crashes, high_youth_census,Safety_Zone_Score, everything())





#-------------- perform ATT calculation
# Ensure GO.LIVE.DATE is properly converted
complete_crash_counts$GO.LIVE.DATE <- as.Date(complete_crash_counts$GO.LIVE.DATE, format = "%m/%d/%Y")


# Create treatment and control groups based on GO.LIVE.DATE
complete_crash_counts <- complete_crash_counts %>%
  mutate(
    # Assign treatment group based on the availability of GO.LIVE.DATE and its comparison to crash_date
    treatment_group = case_when(
      is.na(GO.LIVE.DATE) ~ 0,  # Control: No camera installed (NA in GO.LIVE.DATE)
      !is.na(GO.LIVE.DATE) & crash_date < GO.LIVE.DATE ~ 0,  # Control: Crash occurred before camera installation
      !is.na(GO.LIVE.DATE) & crash_date >= GO.LIVE.DATE ~ 1  # Treatment: Crash occurred after camera installation
    ),
    # Define the period based on crash_date relative to GO.LIVE.DATE
    period = case_when(
      is.na(GO.LIVE.DATE) ~ "control",              # Control: No camera installed
      crash_date < GO.LIVE.DATE ~ "pre",            # Pre-treatment if crash occurred before camera installation
      crash_date >= GO.LIVE.DATE ~ "post",          # Post-treatment if crash occurred on or after camera installation
      TRUE ~ NA_character_                          # Exclude cases not fitting the criteria
    )
  ) %>%
  filter(!is.na(period))  # Exclude any rows that don't fit into pre or post categories


# Aggregate crashes (sum pre and post crash counts by zone and treatment group)
aggregated_crashes_wide <- complete_crash_counts %>%
  group_by(Name, treatment_group) %>%
  summarise(
    total_crashes_post = sum(total_crashes[period == "post"], na.rm = TRUE),
    total_crashes_pre = sum(total_crashes[period == "pre"], na.rm = TRUE),
    crash_difference = total_crashes_post - total_crashes_pre,
    .groups = 'drop'
  )

# Estimate the counterfactual crashes for treated zones
# Aggregate crashes for control units (without cameras)
control_crashes <- aggregated_crashes_wide %>%
  filter(treatment_group == 0) %>%
  summarise(
    control_total_crashes_post = mean(total_crashes_post, na.rm = TRUE),
    control_total_crashes_pre = mean(total_crashes_pre, na.rm = TRUE)
  )

# Apply control crash rates to treated zones to estimate counterfactual
treated_crashes_with_counterfactual <- aggregated_crashes_wide %>%
  filter(treatment_group == 1) %>%
  mutate(
    counterfactual_crashes_post = control_crashes$control_total_crashes_post,
    counterfactual_crashes_pre = control_crashes$control_total_crashes_pre
  ) %>%
  mutate(
    counterfactual_difference = counterfactual_crashes_post - counterfactual_crashes_pre
  )

# Calculate ATT
att_results <- treated_crashes_with_counterfactual %>%
  summarise(
    observed_crash_difference = mean(crash_difference, na.rm = TRUE),
    average_counterfactual_difference = mean(counterfactual_difference, na.rm = TRUE)
  ) %>%
  mutate(
    ATT = observed_crash_difference - average_counterfactual_difference
  )

# Print ATT result
print("ATT Results:")
print(att_results)

# Count the number of unique zones in the treated group
number_of_treated_zones <- complete_crash_counts %>%
  filter(treatment_group == 1) %>%
  summarise(number_of_zones = n_distinct(Name))

print(number_of_treated_zones)


#----------ATT controlled for SFzone score by matching


# Perform nearest neighbor matching
match_model <- matchit(treatment_group ~ Safety_Zone_Score, data = complete_crash_counts, method = "nearest")

# Get matched data
matched_data <- match.data(match_model)

# Now you can analyze the matched data
matched_results <- matched_data %>%
  group_by(treatment_group) %>%
  summarise(
    average_crashes = mean(total_crashes, na.rm = TRUE),
    .groups = 'drop'
  )

print(matched_results)

treatment_effect <- matched_results$average_crashes[2] - matched_results$average_crashes[1]
print(paste("Treatment Effect:", treatment_effect))


#---------------ATT controlled by stratisfication

# Define quantiles to create strata based on Safety Zone Score
complete_crash_counts <- complete_crash_counts %>%
  mutate(score_strata = ntile(Safety_Zone_Score, 3))  # Creates 3 strata (low, medium, high)

# Now you can run your analysis within each stratum
stratified_results <- complete_crash_counts %>%
  group_by(score_strata) %>%
  summarise(
    observed_crash_difference = mean(total_crashes[treatment_group == 1], na.rm = TRUE) - 
      mean(total_crashes[treatment_group == 0], na.rm = TRUE),
    .groups = 'drop'
  )

print(stratified_results)


#----------- Estimate ATE: Hypothetical models
# Use the ATT value from att_results
ATT_value <- att_results$ATT  # Extract the ATT value

# Create a hypothetical world where all zones have cameras
hypothetical_all_cameras <- complete_crash_counts %>%
  mutate(
    total_crashes = total_crashes + ATT_value,  # Adjust crashes by ATT value for treated group
    treatment_group = 1
  )

# Create a hypothetical world where no zones have cameras
hypothetical_no_cameras <- complete_crash_counts %>%
  mutate(
    total_crashes = total_crashes,  # No adjustment for control group
    treatment_group = 0
  )

# Aggregate crashes for hypothetical worlds
agg_all_cameras <- hypothetical_all_cameras %>%
  group_by(Name) %>%  # Aggregate across all zones
  summarise(
    total_crashes_post = sum(total_crashes[period == "post"], na.rm = TRUE),
    total_crashes_pre = sum(total_crashes[period == "pre"], na.rm = TRUE),
    crash_difference = total_crashes_post - total_crashes_pre,
    .groups = 'drop'
  ) %>%
  summarise(
    average_crash_difference = mean(crash_difference, na.rm = TRUE)
  )

agg_no_cameras <- hypothetical_no_cameras %>%
  group_by(Name) %>%  # Aggregate across all zones
  summarise(
    total_crashes_post = sum(total_crashes[period == "post"], na.rm = TRUE),
    total_crashes_pre = sum(total_crashes[period == "pre"], na.rm = TRUE),
    crash_difference = total_crashes_post - total_crashes_pre,
    .groups = 'drop'
  ) %>%
  summarise(
    average_crash_difference = mean(crash_difference, na.rm = TRUE)
  )

# Calculate and print ATE
ATE <- agg_all_cameras$average_crash_difference - agg_no_cameras$average_crash_difference
print(paste("Estimated ATE:", ATE))











#-----------------See crash totals pre and post treatment for each zone

# Create new columns to define the 3-year pre- and post-treatment windows
segmented_complete_crash_counts <- complete_crash_counts %>%
  mutate(
    pre_treatment_start = GO.LIVE.DATE - years(3),  # Start date for the pre-treatment period (3 years before GO.LIVE.DATE)
    post_treatment_end = GO.LIVE.DATE + years(3),   # End date for the post-treatment period (3 years after GO.LIVE.DATE)
    within_pre_treatment = ifelse(crash_date >= pre_treatment_start & crash_date < GO.LIVE.DATE, 1, 0),  # 1 if crash is within 3 years before treatment
    within_post_treatment = ifelse(crash_date > GO.LIVE.DATE & crash_date <= post_treatment_end, 1, 0)   # 1 if crash is within 3 years after treatment
  )

# Aggregate crashes by zone for the pre- and post-treatment periods
zone_crash_summary <- segmented_complete_crash_counts %>%
  group_by(Name) %>%
  summarise(
    crashes_pre_treatment = sum(within_pre_treatment, na.rm = TRUE),  # Total crashes in the 3 years before GO.LIVE.DATE
    crashes_post_treatment = sum(within_post_treatment, na.rm = TRUE) # Total crashes in the 3 years after GO.LIVE.DATE
  )

# View the summary for each zone
print(zone_crash_summary)











#----see how many total crashes happened in zones compared to outside zones
# Calculate the proportion of unique crashes in buffer compared to total unique crashes
proportion_of_crashes <- n_distinct(crashes_in_buffer_filtered$CRASH_RECORD_ID) / n_distinct(crashes$CRASH_RECORD_ID)

# View the result
print(proportion_of_crashes)

#

#----------Compare treated zones with not treated zones for similar sample size and sfzone score


# Step 1: Get the treated and untreated zones
# Get the unique untreated zones (zones with no cameras, i.e., NA in GO.LIVE.DATE)
untreated_zones <- unique(complete_crash_counts$Name[is.na(complete_crash_counts$GO.LIVE.DATE)])

# Filter the untreated zones and select the top 106 based on Safety_Zone_Score
top_untreated_zones <- complete_crash_counts %>%
  filter(Name %in% untreated_zones) %>%
  group_by(Name) %>%
  summarize(
    mean_safety_zone_score = mean(Safety_Zone_Score, na.rm = TRUE),
    GO_LIVE_DATE = first(GO.LIVE.DATE)  # This will be NA for untreated zones
  ) %>%
  arrange(desc(mean_safety_zone_score)) %>%
  top_n(106, mean_safety_zone_score)

# Get the treated zones (zones with cameras, i.e., non-null GO.LIVE.DATE)
top_treated_zones <- complete_crash_counts %>%
  filter(!is.na(GO.LIVE.DATE)) %>%
  group_by(Name) %>%
  summarize(
    mean_safety_zone_score = mean(Safety_Zone_Score, na.rm = TRUE),
    GO_LIVE_DATE = first(GO.LIVE.DATE)  # This will contain the actual GO.LIVE.DATE for treated zones
  )

# Step 2: Combine treated and untreated zones into one dataframe
# Add treatment group for matching purposes (1 for treated, 0 for untreated)
combined_zones <- bind_rows(
  mutate(top_untreated_zones, treatment_group = 0),  # Untreated zones
  mutate(top_treated_zones, treatment_group = 1)    # Treated zones
)

# Step 3: Perform nearest neighbor matching based on safety zone score
# We use 'treatment_group ~ mean_safety_zone_score' for matching
matched_data <- matchit(treatment_group ~ mean_safety_zone_score, 
                        data = combined_zones, 
                        method = "nearest", 
                        caliper = 0.1)

# Step 4: Get the matched results
matched_results <- match.data(matched_data)

# Step 5: Inspect matched results
str(matched_results)

# You can now proceed with analyzing the matched zones
