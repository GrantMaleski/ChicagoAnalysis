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
library(knitr)


# Load data
crashes <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_Crashes_20240927.csv')
cameras <- read.csv('/Users/grant.maleski/Downloads/Speed_Camera_Locations_20240408.csv')
chicago_safety_zones <- read.csv('/Users/grant.maleski/Downloads/chicago_safety_areas.csv')
chicago_map <- read_sf("/Users/grant.maleski/Downloads/WARDS_2015")
people <- read.csv('/Users/grant.maleski/Downloads/Traffic_Crashes_-_People_20240927.csv')
census_information <- read.csv("/Users/grant.maleski/Downloads/Chicago_Population_Counts_20240927.csv", check.names = FALSE)



#see how many duplicated safety zones there are in Chicago
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



### Geocode addresses via Google Maps API
addresses <- chicago_safety_zones$Address
chicago_addresses <- paste(addresses, ", Chicago, IL", sep = "")
geocoded_df <- tibble(address = chicago_addresses) %>%
  geocode(address, method = 'osm', full_results = TRUE)

coordinates_df <- data.frame(lat = geocoded_df$lat, lon = geocoded_df$long)

#Perform reverse geocoding to get zip codes
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




##--- Process dates
cameras$go.live.date <- as.Date(cameras$GO.LIVE.DATE, format = "%m/%d/%Y")
cameras$year <- format(cameras$go.live.date, "%Y")
cameras$month <- format(cameras$go.live.date, "%m")
cameras$day <- format(cameras$go.live.date, "%d")

crashes$CRASH_DATE <- as.Date(crashes$CRASH_DATE, format = "%m/%d/%Y")
crashes$crash_year <- format(crashes$CRASH_DATE, "%Y")
crashes$crash_month <- format(crashes$CRASH_DATE, "%m")
crashes$crash_day <- format(crashes$CRASH_DATE, "%d")

#count how many unique days cameras have been impelemented on 
length(unique(cameras$go.live.date))

#count how many unique days cameras have been impelemented on 
length(unique(cameras$ADDRESS))





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



###-- Join crashes df with crash_person_summary so we know who is involved in each crash
crashes_clean <- crashes_clean %>%
  left_join(crash_person_summary, by = "CRASH_RECORD_ID", suffix = c("_sf", "_summary"))

# count how many crashes we filtered out

count(crashes) - count(crashes_clean) 
# 6244

# ---Convert to sf object to preppare for geospatial calculations
crashes_sf <- st_as_sf(crashes_clean, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#collect coordinates from crashes geospatial df
crash_coordinates <- st_coordinates(crashes_sf)


#-- Create crash_location_list
crash_location_list <- data.frame(
  CRASH_RECORD_ID = crashes_clean$CRASH_RECORD_ID,
  Longitude = crash_coordinates[,1],
  Latitude = crash_coordinates[,2]
)



#structure location of all cameras
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







## Count crashes near cameras and safety zones
treated_crashes <- sum(crashes_clean$distance_to_camera.x < 201, na.rm = TRUE)
print(treated_crashes)


sfzone_crashes <- sum(crashes_clean$distance_to_sfzone.y < 201, na.rm = TRUE)
print(sfzone_crashes)



yearly_crashes <- crashes_clean %>%
  filter(crash_year >= 2018 & crash_year <= 2023) %>%
  group_by(crash_year) %>%
  summarize(
    all_crashes = n(),
    near_zone = sum(distance_to_sfzone.y < 201, na.rm = TRUE),
    near_camera = sum(distance_to_camera.x < 201, na.rm = TRUE)
  )

# Convert to long format for plotting  
yearly_crashes_long <- yearly_crashes %>%
  pivot_longer(cols = c(all_crashes, near_zone, near_camera),
               names_to = "category",
               values_to = "count") %>%
  mutate(category = factor(category, 
                           levels = c("all_crashes", "near_zone", "near_camera")))  # Set order here

# Create plot
ggplot(yearly_crashes_long, aes(x=crash_year, y=count, fill=category)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(x="Year", y="Number of Crashes", title="Crash Comparison by Year (2018-2023)") +
  scale_fill_manual(values=c("all_crashes"="#8884d8", 
                             "near_zone"="#ffc658",
                             "near_camera"="#82ca9d"),
                    labels=c("All Crashes", "Near Safety Zones", "Near Cameras")) +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Added this line to center title



zone_camera_summary <- yearly_crashes %>%
  summarize(
    avg_all = mean(all_crashes),
    avg_zone = mean(near_zone),
    avg_camera = mean(near_camera)
  ) %>%
  mutate(
    zone_percentage = (avg_zone / avg_all) * 100,
    camera_percentage = (avg_camera / avg_all) * 100
  )







###-- Convert safety zones and cameras to sf objects
cameras_sf <- st_as_sf(cameras, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Find the nearest safety zone for each camera
nearest_safety_zone <- st_nearest_feature(cameras_sf, chicago_safety_zones_list_sf)


# Calculate the distance for each nearest neighbor
distances_to_safety_zones <- st_distance(
  cameras_sf, 
  chicago_safety_zones_list_sf[nearest_safety_zone, ], 
  by_element = TRUE
)

# Add the nearest safety zone index and distance as columns in the cameras_sf dataset
cameras_sf <- cameras_sf %>%
  mutate(
    nearest_safety_zone = nearest_safety_zone,
    distance_to_safety_zone = as.numeric(distances_to_safety_zones) # Convert distance to numeric if needed
  )

# View the result
mean(cameras_sf$distance_to_safety_zone)





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

#--Replace NA with "N/A" for camera-related columns
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



crash_counts_monthly <- crash_counts_monthly %>%
  mutate(crash_year = as.integer(crash_year))

crash_counts_monthly <- crash_counts_monthly %>%
  mutate(crash_month = as.integer(crash_month))


# Create a complete sequence of all year-month combinations

start_year <- min(crashes$crash_year)
end_year <- max(crashes$crash_year)



# Convert the GO.LIVE.DATE column to Date format and filter for dates after 2020 or NA
safety_zones_with_cameras <- safety_zones_with_cameras %>%
  mutate(GO.LIVE.DATE = as.Date(GO.LIVE.DATE, format = "%m/%d/%Y")) %>%
  filter(is.na(GO.LIVE.DATE) | GO.LIVE.DATE >= as.Date("2020-01-01"))

# Ensure the filtering worked by checking the unique values
unique_zones <- unique(safety_zones_with_cameras$Name)



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







#----see how many total crashes happened in zones compared to outside zones
# Calculate the proportion of unique crashes in buffer compared to total unique crashes
proportion_of_crashes <- n_distinct(crashes_in_buffer_filtered$CRASH_RECORD_ID) / n_distinct(crashes$CRASH_RECORD_ID)

# View the result
print(proportion_of_crashes)


#----------Compare treated zones with not treated zones for similar sample size and sfzone score

# Step 1: Get the treated and untreated zones
# Get the unique untreated zones (zones with no cameras, i.e., NA in GO.LIVE.DATE)
untreated_zones <- complete_crash_counts %>%
  filter(is.na(GO.LIVE.DATE)) %>%
  group_by(Name) %>%
  summarize(mean_safety_zone_score = mean(Safety_Zone_Score, na.rm = TRUE)) %>%
  arrange(desc(mean_safety_zone_score))  # Sort by descending score

# Get the treated zones (zones with cameras, i.e., non-null GO.LIVE.DATE)
treated_zones <- complete_crash_counts %>%
  filter(!is.na(GO.LIVE.DATE)) %>%
  group_by(Name) %>%
  summarize(mean_safety_zone_score = mean(Safety_Zone_Score, na.rm = TRUE), 
            GO_LIVE_DATE = first(GO.LIVE.DATE)) %>%  # Include GO_LIVE_DATE
  arrange(desc(mean_safety_zone_score))  # Sort by descending score

# Step 2: Select top 22 untreated and top 22 treated zones based on the sorted score
top_untreated_zones <- untreated_zones %>%
  top_n(22, mean_safety_zone_score)



# Step 3: Combine treated and untreated zones into one dataframe with treatment group
combined_zones <- bind_rows(
  mutate(top_untreated_zones, treatment_group = 0),  # Untreated zones
  mutate(treated_zones, treatment_group = 1)    # Treated zones
)

# Step 4: Perform nearest neighbor matching based on safety zone score
# This step will match the highest safety zone scores in untreated with the highest in treated
match_model <- matchit(treatment_group ~ mean_safety_zone_score, 
                       data = combined_zones, 
                       method = "nearest",
                       ratio = 1)  # 1:1 matching

# Step 5: Get the matched results
matched_data <- match.data(match_model)

# Step 6: Extract the subclass column to ensure proper matching
treated_data <- matched_data %>% filter(treatment_group == 1)
control_data <- matched_data %>% filter(treatment_group == 0)

# Ensure GO_LIVE_DATE is treated as a date in the treated data
treated_data <- treated_data %>%
  mutate(GO_LIVE_DATE = as.Date(GO_LIVE_DATE, origin = "1970-01-01"))

# Step 7: Merge the treated group's GO_LIVE_DATE to the untreated group by subclass
control_data_with_dates <- control_data %>%
  left_join(treated_data %>% select(subclass, GO_LIVE_DATE), by = "subclass", suffix = c("", "_treated"))

# Step 8: Replace the GO_LIVE_DATE in the control (untreated) group with the treated GO_LIVE_DATE
control_data_with_dates <- control_data_with_dates %>%
  mutate(GO_LIVE_DATE = as.Date(GO_LIVE_DATE_treated, origin = "1970-01-01")) %>%
  select(-GO_LIVE_DATE_treated)  # Remove the extra column

# Step 9: Combine treated and updated control data back together
merged_data <- bind_rows(treated_data, control_data_with_dates)

# Step 10: Inspect the final merged data
print(merged_data)



#  Calculate crashes for each zone 2 years before and 2 years after GO_LIVE_DATE
# First, create a function to classify crashes into before/after periods
crash_counts <- merged_data %>%
  select(Name, GO_LIVE_DATE) %>%
  distinct() %>%
  # Cross join with complete_crash_counts to ensure we get all crashes for each zone
  left_join(complete_crash_counts, by = "Name") %>%
  # Add diagnostic columns
  mutate(
    is_before = crash_date >= (GO_LIVE_DATE - lubridate::years(2)) &
      crash_date <= GO_LIVE_DATE,
    is_after = crash_date > GO_LIVE_DATE &
      crash_date <= (GO_LIVE_DATE + lubridate::years(2))
  ) %>%
  # Only keep records within our 2-year window
  filter(!is.na(crash_date)) %>%
  group_by(Name) %>%
  summarize(
    crashes_2yr_before = sum(total_crashes[is_before], na.rm = TRUE),
    crashes_2yr_after = sum(total_crashes[is_after], na.rm = TRUE),
    avg_monthly_crashes_before = crashes_2yr_before / sum(is_before, na.rm = TRUE),
    avg_monthly_crashes_after = crashes_2yr_after / sum(is_after, na.rm = TRUE)
  ) %>%
  ungroup()




# Merge with original data
merged_data_with_crashes <- merged_data %>%
  left_join(crash_counts, by = "Name")

write.csv(merged_data_with_crashes, "crash_data.csv", row.names = F)


# Calculate ATT using the crash data
att_analysis <- merged_data_with_crashes %>%
  # First ensure we only look at treated units (D=1)
  filter(treatment_group == 1) %>%
  # Calculate the observed difference for each treated unit
  mutate(
    # Y(D=1) is the observed outcome after treatment
    Y_D1 = crashes_2yr_after,
    # Y(D=0) is the observed outcome before treatment
    Y_D0 = crashes_2yr_before,
    # Individual treatment effect
    individual_effect = Y_D1 - Y_D0
  ) %>%
  # Calculate summary statistics
  summarize(
    n_treated = n(),
    # ATT = E[Y(D=1)|D=1] - E[Y(D=0)|D=1]
    mean_Y_D1 = mean(Y_D1),
    mean_Y_D0 = mean(Y_D0),
    att = mean(individual_effect),
    # Calculate standard error
    se_att = sd(individual_effect) / sqrt(n()),
    # Calculate 95% confidence interval
    ci_lower = att - 1.96 * se_att,
    ci_upper = att + 1.96 * se_att,
    # Calculate percentage change
    percent_change = (mean_Y_D1 - mean_Y_D0) / mean_Y_D0 * 100
  )

# Print detailed results
print("Average Treatment Effect on the Treated (ATT) Analysis:")
print(att_analysis)

# Create visualization of individual treatment effects
treatment_effects <- merged_data_with_crashes %>%
  mutate(
    effect = crashes_2yr_after - crashes_2yr_before,
    percent_change = (crashes_2yr_after - crashes_2yr_before) / crashes_2yr_before * 100
  ) %>%
  arrange(effect)

write.csv(treatment_effects, "treatment_effects.csv", row.names = F)

# Print distribution of effects
print("\nDistribution of Individual Treatment Effects:")
print(summary(treatment_effects$effect))

# Print locations with largest effects (both positive and negative)
print("\nTop 5 Locations with Largest Crash Reductions:")
treatment_effects %>%
  arrange(effect) %>%
  select(Name, crashes_2yr_before, crashes_2yr_after, effect, percent_change) %>%
  head(5) %>%
  print()

print("\nTop 5 Locations with Largest Crash Increases (if any):")
treatment_effects %>%
  arrange(desc(effect)) %>%
  select(Name, crashes_2yr_before, crashes_2yr_after, effect, percent_change) %>%
  head(5) %>%
  print()

# Additional robustness check - paired t-test
ttest_result <- t.test(
  treatment_effects$crashes_2yr_after,
  treatment_effects$crashes_2yr_before,
  paired = TRUE
)

print("\nPaired t-test Results:")
print(ttest_result)





# Calculate treatment effects with comparison to control group
did_analysis <- merged_data_with_crashes %>%
  group_by(treatment_group) %>%
  summarize(
    n_units = n(),
    mean_before = mean(crashes_2yr_before),
    mean_after = mean(crashes_2yr_after),
    diff = mean_after - mean_before,
    se_diff = sd(crashes_2yr_after - crashes_2yr_before) / sqrt(n()),
    ci_lower = diff - 1.96 * se_diff,
    ci_upper = diff + 1.96 * se_diff,
    percent_change = (mean_after - mean_before) / mean_before * 100
  ) %>%
  ungroup()

# Calculate the difference-in-differences estimate
did_estimate <- did_analysis %>%
  summarize(
    diff_in_diff = diff[treatment_group == 1] - diff[treatment_group == 0],
    se_did = sqrt(se_diff[treatment_group == 1]^2 + se_diff[treatment_group == 0]^2),
    ci_lower = diff_in_diff - 1.96 * se_did,
    ci_upper = diff_in_diff + 1.96 * se_did,
    relative_effect = (diff[treatment_group == 1] / mean_before[treatment_group == 1]) - 
      (diff[treatment_group == 0] / mean_before[treatment_group == 0])
  )

# Formal statistical test using regression
did_regression <- lm(
  crashes_2yr_after - crashes_2yr_before ~ treatment_group, 
  data = merged_data_with_crashes
)

# Print results
print("Summary by Treatment Group:")
print(did_analysis)

print("\nDifference-in-Differences Estimate:")
print(did_estimate)

print("\nRegression Results:")
print(summary(did_regression))

# Calculate individual unit changes
unit_changes <- merged_data_with_crashes %>%
  mutate(
    change = crashes_2yr_after - crashes_2yr_before,
    percent_change = (crashes_2yr_after - crashes_2yr_before) / crashes_2yr_before * 100
  ) %>%
  arrange(treatment_group, desc(abs(change)))

# Print summary statistics by group
print("\nDetailed Summary Statistics:")
unit_changes %>%
  group_by(treatment_group) %>%
  summarize(
    n = n(),
    mean_change = mean(change),
    median_change = median(change),
    sd_change = sd(change),
    mean_percent_change = mean(percent_change),
    min_change = min(change),
    max_change = max(change)
  ) %>%
  print()

# Print top changes for each group
print("\nLargest Changes in Treated Group:")
unit_changes %>%
  filter(treatment_group == 1) %>%
  select(Name, crashes_2yr_before, crashes_2yr_after, change, percent_change) %>%
  arrange(desc(abs(change))) %>%
  head(5) %>%
  print()

print("\nLargest Changes in Control Group:")
unit_changes %>%
  filter(treatment_group == 0) %>%
  select(Name, crashes_2yr_before, crashes_2yr_after, change, percent_change) %>%
  arrange(desc(abs(change))) %>%
  head(5) %>%
  print()

# Additional balance check
print("\nBalance Check - Pre-treatment Crashes:")
t.test(crashes_2yr_before ~ treatment_group, data = merged_data_with_crashes)






#----------analysis

# Count cameras by year
cameras_year_counts <- as.data.frame(table(cameras$year))
colnames(cameras_year_counts) <- c("Year", "Count")



# Reshape data to wide format, with years as column names
cameras_wide_format <- cameras_year_counts %>%
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0)

# Display the table with each year as a column
kable(
  cameras_wide_format,
  caption = "Number of Cameras Implemented by Year",
  align = "c"
)

# Count crashes by year
crashes_year_counts <- as.data.frame(table(crashes_clean$crash_year))
colnames(crashes_year_counts) <- c("Year", "Count")



# Convert Year to numeric for better plotting control
crashes_year_counts$Year <- as.numeric(as.character(crashes_year_counts$Year))
crashes_year_counts <- crashes_year_counts[crashes_year_counts$Year >= 2018 & crashes_year_counts$Year <= 2023, ]

ggplot(crashes_year_counts, aes(x = Year, y = Count)) +
  geom_col(fill = "blue", alpha = 0.7) +
  geom_text(aes(label = scales::comma(Count)), vjust = -0.5) +
  labs(title = "Crashes by Year (2018-2023)",
       x = "Year", 
       y = "Crashes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 2018:2023) +
  scale_y_continuous(labels = scales::comma)



#---count crashes between 2018 and 2023
crashes_year_counts <- as.data.frame(table(crashes_clean$crash_year))
colnames(crashes_year_counts) <- c("Year", "Count")

# Step 2: Convert Year column to numeric for filtering purposes
crashes_year_counts$Year <- as.numeric(as.character(crashes_year_counts$Year))

# Step 3: Filter for years between 2018 and 2023
crashes_2018_2023 <- crashes_year_counts %>%
  filter(Year >= 2018 & Year <= 2023)

# Step 4: Calculate the average crash count for these years
average_crashes_2018_2023 <- mean(crashes_2018_2023$Count)

# Display the result
print(average_crashes_2018_2023)




# Reshape the data with ordered periods
did_data <- merged_data_with_crashes %>%
  select(Name, treatment_group, crashes_2yr_before, crashes_2yr_after) %>%
  gather(key = "period", value = "crashes", 
         crashes_2yr_before, crashes_2yr_after) %>%
  mutate(
    period = factor(ifelse(period == "crashes_2yr_before", "Before", "After"), 
                    levels = c("Before", "After")),  # Explicitly set order
    group = ifelse(treatment_group == 1, "Treatment", "Control")
  )

# Calculate means for each group and period
did_means <- did_data %>%
  group_by(group, period) %>%
  summarize(
    mean_crashes = mean(crashes),
    se = sd(crashes)/sqrt(n())
  )

# Create the DiD plot
ggplot(did_means, aes(x = period, y = mean_crashes, group = group, linetype = group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_crashes - 1.96*se, 
                    ymax = mean_crashes + 1.96*se), 
                width = 0.1) +
  scale_linetype_manual(values = c("Treatment" = "solid", "Control" = "dashed")) +
  theme_minimal() +
  labs(
    title = "Impact of Speed Cameras on Crash Frequency",
    x = "Period Relative to Camera Installation",
    y = "Average Number of Crashes",
    linetype = "Group"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black")
  )


# Treatment Effect Distribution Plot
ggplot(treatment_effects, 
       aes(x = reorder(Name, effect), 
           y = effect,
           fill = factor(treatment_group))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "black", "1" = "gray50"),
                    labels = c("Untreated", "Treated")) +
  labs(
    title = "Distribution of Treatment Effects Across Locations",
    x = "Location",
    y = "Change in Number of Crashes",
    fill = "Group"
  ) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  # Add borders to bars
  geom_bar(stat = "identity", fill = NA, color = "black", alpha = 0.7)




# Create simple crash analysis for matched zones
crash_analysis <- complete_crash_counts %>%
  filter(Name %in% merged_data_with_crashes$Name) %>%
  mutate(
    date = as.Date(paste(crash_year, crash_month, "01", sep = "-")),
    treated = if_else(!is.na(GO.LIVE.DATE), "Camera Zones", "Control Zones")
  ) %>%
  filter(crash_year >= 2018 & crash_year < 2024) %>%
  group_by(treated, date) %>%
  summarise(total_crashes = sum(total_crashes, na.rm = TRUE))

# Black and white plot with centered title
ggplot(crash_analysis, aes(x = date, y = total_crashes, 
                           linetype = treated)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Monthly Crashes in Matched Zones (2018-2023)",
       x = "", y = "Crashes") +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  scale_linetype_manual(values = c("Camera Zones" = "solid", "Control Zones" = "twodash")) +
  guides(linetype = guide_legend(title = ""))








