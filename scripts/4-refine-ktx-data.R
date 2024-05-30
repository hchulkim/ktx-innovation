
######################################################################################################

# start date: 2024.1.4.
# revised data (most recent): 2024.5.10.
# main coder: Hyoungchul Kim
# main purpose: this code refines the map for korea and location of the hsr KTX stations.

######################################################################################################

library(tidyverse)
library(sf)
library(lubridate)

# read in the ktx data
data <- read_rds("data/ktx_data.rds")

# write it in dta for readability
data %>% haven::write_dta("data/ktx_data.dta")
# dis <- read_rds("data/ktx_distance.rds")

# read in region crosswalk code and shape data
crosswalk <- haven::read_dta("data/region_label_concordance.dta")
geo <- st_read("data/region/bnd_sigungu_2021.shp") %>% 
  select(region_code=SIGUNGU_CD, sigungu=SIGUNGU_NM, geometry) %>% 
  st_simplify(dTolerance = 100)

# merge shape data with crosswalk to change the region code
geo <- geo %>% left_join(crosswalk, by=c("region_code", "sigungu"))


# manually take care of some NA matched cases
geo_prob <- geo %>% filter(is.na(sido))
geo <- geo %>% filter(!is.na(sido))

geo_prob <- geo_prob %>% mutate(sigungu = str_extract(sigungu, "^(\\S+)."))

geo_prob <- geo_prob %>% mutate(sigungu=ifelse(sigungu=="세종시", "특별자치시", sigungu))

geo_prob <- geo_prob %>% group_by(sigungu) %>% 
  summarise(geometry=st_union(geometry))

# merge again by sigungu
geo_prob <- geo_prob %>% 
  select(sigungu, geometry) %>% 
  left_join(crosswalk, by="sigungu")

# manually take care of NA cases
geo_fix <- geo_prob %>% filter(!is.na(sido))
geo_prob <- geo_prob %>% filter(is.na(sido))

geo <- geo %>% bind_rows(geo_fix)

geo_prob <- geo_prob %>% select(sigungu, geometry) %>% 
  bind_cols(tibble(sido=c("경기도", "경기도", "경기도", "경기도", "경기도", "경기도", "전라북도", "경상남도", "충청남도", "충청북도", "경상북도"), 
                   region_code = c("31100", "31020", "31010", "31090", "31040", "31190", "35010", "38110", "34010", "33040", "37010"))) %>% 
  mutate(iso=paste(sido, sigungu))

geo <- geo %>% bind_rows(geo_prob)


# merge by region_code
geo <- geo %>% group_by(region_code) %>% 
  summarise(geometry=st_union(geometry))
# 227 region

# check the map
geo %>% ggplot() + 
  geom_sf() +
  theme_void()

# add region label to geo again
geo <- geo %>% left_join(crosswalk %>% select(region_code, iso) %>% distinct(region_code, .keep_all = T), by="region_code")



# download the refined map
# geo %>% st_write("data/map_korea.geojson", driver = "GeoJSON")

# geo %>% haven::write_dta("data/map_korea.dta")

# just filter HSR station
data <- data %>% select(address, lon, lat, year, expand1, expand2) %>% 
  filter(str_detect(address, "서울|광명|천안|평택|동탄|수서|횡성|둔내|평창|진부|강릉|오송|공주|익산|정읍|광주|김천|대전|동대구|포항|신경주|울산|부산"))

data <- data %>% filter(!str_detect(address, "서대전"))

# manually fix 동대구
data <- data %>% mutate(expand2=if_else(str_detect(address, "동대구"), NA, expand2))


# fix the year
data <- data %>% mutate(year=if_else(!is.na(expand2), expand2, expand1))

data <- data %>% select(address, lon, lat, year)

# data <- data %>% mutate(hsr=ifelse(str_detect(address, "밀양|구포"), 0, 1))


# make lon lat into Korea CRS EPSG:5179
data <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326) %>% st_transform(data, crs = 5179)


# check the map with KTX
ggplot() +
  geom_sf(data = geo) +
  geom_sf(data = data, color = "red", size = 2) +
  ggtitle("KTX stations (HSR station only)") +
  theme_void()


#calculating the year of establishment for stations
# make date into year

data <- data %>% mutate(address, year=round_date(year, "year"), geometry)

data <- data %>%  mutate(year=year(year))

# only use year before 2018 강릉선 이전
data <- data %>% filter(year<2018)

# make the grid

final_data <- expand_grid(region_code=geo$region_code, year=1999:2017)

# Convert distances to meters (15 km)
distance_in_meters <- 15000 

# for year 2004
data04 <- data %>% filter(year<2005)

# Create buffers around stations
stations_buffer <- data04 %>%
  st_buffer(dist = distance_in_meters)

# Calculate centroids of administrative units
admin_units_centroids <- geo %>%
  st_centroid()


# Perform spatial join to find which centroids fall within the station buffers
overlap <- st_join(admin_units_centroids, stations_buffer, join = st_within)

overlap <- overlap %>% mutate(within_buffer04 = ifelse(is.na(address), 0, 1))

overlap <- overlap %>% st_drop_geometry() %>% select(region_code, within_buffer04)

overlap <- overlap %>% distinct(region_code, within_buffer04, .keep_all = T)

overlap <- overlap %>% rename(ktx_dummy=within_buffer04)

data04 <- expand_grid(region_code=overlap$region_code, year=2004:2017) %>% 
  left_join(overlap, by="region_code")


# Create the dummy variable
final_data <- final_data %>%
  left_join(data04, by=c("region_code", "year"))

# for year 2011
data11 <- data %>% filter(year<2012)

# Create buffers around stations
stations_buffer <- data11 %>%
  st_buffer(dist = distance_in_meters)

# Calculate centroids of administrative units
admin_units_centroids <- geo %>%
  st_centroid()


# Perform spatial join to find which centroids fall within the station buffers
overlap <- st_join(admin_units_centroids, stations_buffer, join = st_within)

overlap <- overlap %>% mutate(within_buffer11 = ifelse(is.na(address), 0, 1))

overlap <- overlap %>% st_drop_geometry() %>% select(region_code, within_buffer11)

overlap <- overlap %>% distinct(region_code, within_buffer11, .keep_all = T)

overlap <- overlap %>% rename(ktx_dummy=within_buffer11)

data11 <- expand_grid(region_code=overlap$region_code, year=2011:2017) %>% 
  left_join(overlap, by="region_code")


# Create the dummy variable
final_data <- final_data %>%
  left_join(data11, by=c("region_code", "year"))


# for year 2015
data15 <- data %>% filter(year<2016)

# Create buffers around stations
stations_buffer <- data15 %>%
  st_buffer(dist = distance_in_meters)

# Calculate centroids of administrative units
admin_units_centroids <- geo %>%
  st_centroid()


# Perform spatial join to find which centroids fall within the station buffers
overlap <- st_join(admin_units_centroids, stations_buffer, join = st_within)

overlap <- overlap %>% mutate(within_buffer15 = ifelse(is.na(address), 0, 1))

overlap <- overlap %>% st_drop_geometry() %>% select(region_code, within_buffer15)

overlap <- overlap %>% distinct(region_code, within_buffer15, .keep_all = T)

overlap <- overlap %>% rename(ktx_dummy=within_buffer15)

data15 <- expand_grid(region_code=overlap$region_code, year=2015:2017) %>% 
  left_join(overlap, by="region_code")


# Create the dummy variable
final_data <- final_data %>%
  left_join(data15, by=c("region_code", "year"))



# for year 2017
data17 <- data %>% filter(year<2018)

# Create buffers around stations
stations_buffer <- data17 %>%
  st_buffer(dist = distance_in_meters)

# Calculate centroids of administrative units
admin_units_centroids <- geo %>%
  st_centroid()


# Perform spatial join to find which centroids fall within the station buffers
overlap <- st_join(admin_units_centroids, stations_buffer, join = st_within)

overlap <- overlap %>% mutate(within_buffer17 = ifelse(is.na(address), 0, 1))

overlap <- overlap %>% st_drop_geometry() %>% select(region_code, within_buffer17)

overlap <- overlap %>% distinct(region_code, within_buffer17, .keep_all = T)

overlap <- overlap %>% rename(ktx_dummy=within_buffer17)

data17 <- expand_grid(region_code=overlap$region_code, year=2017) %>% 
  left_join(overlap, by="region_code")


# Create the dummy variable
final_data <- final_data %>%
  left_join(data17, by=c("region_code", "year"))


# make it into one ktx_dummy

final_data <- final_data %>% 
  transmute(region_code, year, 
            ktx_dummy = as.integer(if_any(starts_with("ktx"), ~ .x == 1)))

final_data <- final_data %>% mutate(ktx_dummy = ifelse(is.na(ktx_dummy), 0, ktx_dummy))


# add label
final_data <- final_data %>% 
  left_join(geo %>% st_drop_geometry() %>% distinct(region_code, .keep_all = T), by="region_code")

# total 227 regions x 19 years = 4313 variable. from 1999 - 2017.
# save it
final_data %>% haven::write_dta("data/ktx_dummy_rough.dta")




