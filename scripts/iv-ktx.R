


# this code basically makes data for IV: since the lon lat data can change everytime we run this code, we will refrain that from happening. You can take out the if (false) paranthesis if you want to rerun the results. This is just for stable analysis. 

library(ggmap) 
library(tidyverse)

if (false) {

register_google("AIzaSyBuCvo7LG7bHLJL2Mq03e9M9CsAlYoNOKs")

ktx <- tibble(address=c("서울시 중구 한강대로 405", "서울특별시 영등포구 경인로 846", "경기도 안양시 만안구 만안로 244", "경기도 수원시 팔달구 덕영대로 924", "경기도 오산시 오산동 역광장로 59", "경기도 평택시 탄현로 51", "경기도 평택시 평택로 51", "충청남도 천안시 서북구 성환읍 성환1로 237-5", "충청남도 천안시 동남구 대흥로 239", "세종특별자치시 전의면 만세길 10", "세종특별자치시 조치원읍 으뜸길 215", "세종특별자치시 부강면 청연로 90", "대전광역시 대덕구 신탄진로 807", "대전광역시 동구 중앙로 215", "충청북도 옥천군 옥천읍 옥천로 1624", "충청북도 옥천군 이원면 이원역길 51", "충청북도 옥천군 이원면 지탄1길 28", "충청북도 영동군 심천면 심천로5길 5", "충청북도 영동군 영동읍 계산로 87", "충청북도 영동군 황간면 하옥포2길 14", "충청북도 영동군 추풍령면 추풍령로 444", "경상북도 김천시 김천로 111", "경상북도 구미시 구미중앙로 76", "경상북도 칠곡군 약목면 칠곡대로 635", "경상북도 칠곡군 왜관읍 중앙로8길 10", "경상북도 칠곡군 지천면 신동로7길 37", "대구광역시 중구 태평로 161", "대구광역시 동구 동대구로 550", "경상북도 경산시 중앙로 1", "경상북도 청도군 화양읍 다로길 39", "경상북도 청도군 청도읍 청화로 214", "경상남도 밀양시 상동면 금산7길 4", "경상남도 밀양시 중앙로 62", "경상남도 밀양시 삼랑진읍 천태로 72", "경상남도 양산시 원동면 원동마을길 13", "경상남도 양산시 물금읍 황산로 347", "부산광역시 북구 학사로 135", "부산광역시 북구 구포만세길 82", "부산광역시 사상구 광장로 108", "부산광역시 부산진구 부전로 181"))

ktx <- ktx %>% mutate_geocode(address)


ktx %>% write_rds("data/ktx_iv_station.rds")

}
# now make the same buffer style for iv ktx
ktx <- read_rds("data/ktx_iv_station.rds")
library(sf)
library(lubridate)

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






# codes to refer to



# make lon lat into Korea CRS EPSG:5179
ktx <- st_as_sf(ktx, coords = c("lon", "lat"), crs = 4326) %>% st_transform(data, crs = 5179)


# check the map with KTX
ggplot() +
  geom_sf(data = geo) +
  geom_sf(data = ktx, color = "red", size = 2) +
  ggtitle("KTX IV stations)") +
  theme_void()
ggsave("results/figures/ktx_iv_map.png", width=10, height=7, dpi=400)




# make the grid

final_data <- expand_grid(region_code=geo$region_code)

# Convert distances to meters (15 km)
distance_in_meters <- 15000 

# Create buffers around stations
stations_buffer <- ktx %>%
  st_buffer(dist = distance_in_meters)

# check in map
ggplot() +
  geom_sf(data = geo) +
  geom_sf(data = stations_buffer, color = "red", size = 2) +
  ggtitle("KTX IV stations)") +
  theme_void()


# Calculate centroids of administrative units
admin_units_centroids <- geo %>%
  st_centroid()

admin_units_centroids <-  st_transform(admin_units_centroids, crs = 5179)


# Perform spatial join to find which centroids fall within the station buffers
overlap <- st_join(admin_units_centroids, stations_buffer, join = st_within)

#take out case where not unique
overlap <- overlap %>% distinct(region_code, .keep_all = T)

overlap <- overlap %>% mutate(within_buffer = ifelse(is.na(address), 0, 1))

overlap <- overlap %>% st_drop_geometry() %>% select(region_code, within_buffer)

overlap <- overlap %>% distinct(region_code, within_buffer, .keep_all = T)

overlap <- overlap %>% rename(ktx_iv_dummy=within_buffer)

ktx <- expand_grid(region_code=overlap$region_code) %>% 
  left_join(overlap, by="region_code")


# Create the dummy variable
final_data <- ktx


# save it
final_data %>% haven::write_dta("data/ktx_iv_dummy_rough.dta")





