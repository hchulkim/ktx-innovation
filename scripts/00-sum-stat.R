

# description: this source code file basically creates all the sum stats we use for our project.
# date: 2024.5.10
# creator: Hyoungchul Kim

library(tidyverse)
library(sf)



# 1. map of the korea and ktx hsr stations. ################################################################################

# read in the ktx data
data <- read_rds("data/ktx_data.rds")


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
ggsave("results/figures/map_korea.png", width=10, height=7, dpi=400)

# add region label to geo again
geo <- geo %>% left_join(crosswalk %>% select(region_code, iso) %>% distinct(region_code, .keep_all = T), by="region_code")



# make the KTX stations on the korea map

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
ggsave("results/figures/ktx_map.png", dpi=400)


# only map with gyeongbu line ktx

ggplot() +
  geom_sf(data = geo) +
  geom_sf(data = data %>% filter(year(year)<2015), color = "red", size= 2) +
  ggtitle("KTX stations (HSR stations for gyeongbu line only)") +
  theme_void()
ggsave("results/figures/ktx_map_gyeongbu.png", dpi=400)


# 2. Number of total patent numbers from 2000 to 2019
invt <- haven::read_dta("data/invt_refine_matched.dta")

# first make year out of appnum
invt <- invt %>% mutate(year = floor(appnum / 10^7) - 100000)

# first check total number of petant by year: make it unique patent.
invt_patent <- invt %>% distinct(appnum, .keep_all = T) %>% group_by(year) %>% summarise(n=n()) %>% 
  filter(year>1999, year<2020)

ggplot() +
  geom_point(data = invt_patent, aes(x = year, y = n)) +
  geom_line(data = invt_patent, aes(x = year, y = n)) +
  scale_x_continuous(name = "Year", breaks = seq(2000, 2019, by=1)) +
  scale_y_continuous(name = "Total Number of Patents", labels=scales::label_comma(scale=0.001, suffix="K")) +
  theme_bw()
ggsave("results/figures/aggregate_patent_by_year.png", dpi=400)


# 3. difference between sudogwon patent numbers with other places from 2000 to 2019. 

invt_patent <- invt %>% mutate(sudo = ifelse(str_detect(sido, "서울|인천|경기"), "sudo", "non-sudo")) %>% group_by(year, sudo) %>% summarise(n=n()) %>% 
  filter(year>1999, year<2020) %>% ungroup()

ggplot(data = invt_patent, aes(x = year, y = n, color=sudo, group=sudo)) + 
         geom_point() +
         geom_line() +
  scale_x_continuous(name = "Year", breaks = seq(2000, 2019, by=1)) +
  scale_y_continuous(name = "Total Number of Patents by regions (multiple regions for patent)", labels=scales::label_comma(scale=0.001, suffix="K")) +
  theme_bw()
ggsave("results/figures/patent_sudo_nonsudo_by_year.png", dpi=400)

# 4. difference between ktx stations plaes and non places.
ktx <- haven::read_dta("data/ktx_dummy_rough.dta")

# only leave regions that has ktx

ktx_yes <- ktx %>% filter(ktx_dummy==1) %>% 
  distinct(region_code, .keep_all = T) %>%
  select(region_code, ktx_dummy) %>% 
  mutate(ktx=ifelse(ktx_dummy==1, "ktx-station", "no-ktx-station"))

# merge it to the invt

invt_patent <- invt %>% 
  left_join(ktx_yes, by="region_code") %>% 
  group_by(year, ktx) %>% 
  summarise(n=n()) %>% 
  filter(year>1999, year<2020) %>% ungroup()

invt_patent <- invt_patent %>% mutate(ktx=ifelse(is.na(ktx), "no-ktx-station", ktx))

ggplot(data = invt_patent, aes(x = year, y = n, color=ktx, group=ktx)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "Year", breaks = seq(2000, 2019, by=1)) +
  scale_y_continuous(name = "Total Number of Patents by regions (multiple regions for patent)", labels=scales::label_comma(scale=0.001, suffix="K")) +
  theme_bw()
ggsave("results/figures/patent_ktx_noktx_by_year.png", dpi=400)


# 5 compare places between ktx bilateral and no ktx

citation <- haven::read_dta("data/citation_city_label.dta")

citation <- citation %>% group_by(cited_iso, citing_iso, citing_year) %>% 
  summarise(flow=sum(flow)) %>% ungroup()

citation <- citation %>% 
  left_join(ktx_yes, by=c("cited_iso"="region_code")) %>% 
  rename(ktx_dummy_cited=ktx_dummy, ktx_cited=ktx)

citation <- citation %>% 
  left_join(ktx_yes, by=c("citing_iso"="region_code")) %>% 
  rename(ktx_dummy_citing=ktx_dummy, ktx_citing=ktx)

citation <- citation %>% mutate(ktx_dummy_cited = ifelse(is.na(ktx_dummy_cited), 0, ktx_dummy_cited), ktx_dummy_citing = ifelse(is.na(ktx_dummy_citing), 0, ktx_dummy_citing))

citation <- citation %>% mutate(ktx= ktx_dummy_cited * ktx_dummy_citing)

citation <- citation %>% mutate(ktx_label = ifelse(ktx==1, "ktx", "no-ktx"))


citation <- citation %>% group_by(citing_year, ktx_label) %>% 
  summarise(flow=sum(flow)) %>% ungroup()

citation %>% filter(citing_year>1999, citing_year<2020) %>% 
  mutate(flow=log(flow+1)) %>% 
  ggplot(aes(x = citing_year, y = flow, color=ktx_label, group=ktx_label)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=seq(2000, 2019, by=1)) +
  scale_y_continuous(name="Number of patents (in multiple)") +
  theme_bw()
ggsave("results/figures/patent_bilateral_ktx_noktx_by_year.png", dpi=400)

# 6. map with patents in sigungu

invt_patent <- invt %>% distinct(appnum, .keep_all = T) %>% 
  group_by(region_code) %>% 
  summarise(n=n()) %>% ungroup()

invt_patent <- invt_patent %>%
  left_join(geo, by="region_code")


ggplot() +
  geom_sf(data= geo) +
  geom_sf(data = )
  


ggplot() +
  geom_sf(data = invt_patent, aes(geometry = geometry, fill=n)) 



