library(tidyverse)

######################################################################################################

# start date: 2023.12.27
# revised data (most recent): 2024.5.10
# main coder: Hyoungchul Kim
# main purpose: this code finalizes the data refinement process and also matches region numerical code to the refined data for innovation.
# notice: for replication purpose, we start from untouched raw data downloaded from the source. However, the code for matching region code and refined data is already given from other project. If you want more information about how we made the crosswalk for districts and region, contact us.

######################################################################################################

# read in the matching data
crosswalk <- haven::read_dta("data/region_label_concordance.dta")

# last fix
invt <- read_rds("data/invt_refine_main.rds") 

invt <- invt %>% mutate(sido=ifelse(str_detect(address, "수원시"), "경기도", sido), 
                        sigungu=ifelse(str_detect(address, "수원시"), "수원시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "원주시"), "강원도", sido), 
                        sigungu=ifelse(str_detect(address, "원주시"), "원주시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "구미시"), "경상북도", sido), 
                        sigungu=ifelse(str_detect(address, "구미시"), "구미시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "군포시"), "경기도", sido), 
                        sigungu=ifelse(str_detect(address, "군포시"), "군포시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "진해시"), "경상남도", sido), 
                        sigungu=ifelse(str_detect(address, "진해시"), "진해시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "여주"), "경기도", sido), 
                        sigungu=ifelse(str_detect(address, "여주"), "여주시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "광진구"), "서울특별시", sido), 
                        sigungu=ifelse(str_detect(address, "광진구"), "광진구", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "시흥시"), "경기도", sido), 
                        sigungu=ifelse(str_detect(address, "시흥시"), "시흥시", sigungu))
invt <- invt %>% mutate(sido=ifelse(str_detect(address, "일산"), "경기도", sido), 
                        sigungu=ifelse(str_detect(address, "일산"), "고양시", sigungu))
invt <- invt %>% mutate(sigungu=ifelse(str_detect(address, "북구") & str_detect(address, "인천"), "부평구", sigungu))

# problem of 서울특별시 강서구 and 부산광역시 강서구
invt <- invt %>% filter(address!="강서구")

invt <- invt %>% mutate(sido=ifelse(str_detect(sigungu, "강서구") & !str_detect(address, "서울|서을|특별시"), "부산광역시", sido))

invt <- invt %>% mutate(sido=ifelse(str_detect(sigungu, "강서구") & str_detect(address, "서울|서을|특별시"), "서울특별시", sido))

invt <- invt %>% mutate(sido=ifelse(str_detect(sigungu, "강서구") & !str_detect(address, "서울|서을|특별시|부산|광역시"), NA, sido))


invt <- invt %>% mutate(sido=ifelse(str_detect(address, "세종시|세종특별|세종자치|세종 조치원"), "세종", sido))
invt <- invt %>% mutate(sigungu=ifelse(str_detect(address, "세종시|세종특별|세종자치|세종 조치원"), "특별자치시", sigungu))

invt <- invt %>% mutate(sido=ifelse(sido=="세종특별자치시", "세종", sido))
invt <- invt %>% mutate(sigungu=ifelse(sido=="세종", "특별자치시", sigungu))


# merge them
invt <- invt %>% left_join(crosswalk, by=c("sido", "sigungu"))


# check any NA
check <- invt %>% filter(is.na(region_code)) %>% distinct(address, .keep_all = T)


# filter NA
check <- invt %>% filter(is.na(region_code))
invt <- invt %>% filter(!is.na(region_code))

# save the data
invt %>% write_rds("data/invt_refine_matched.rds", compression=9L, "xz") # 5949377 obs.
invt %>% haven::write_dta("data/invt_refine_matched.dta")


# check if the flow of the data is not that problematic
invt <- invt %>% mutate(year=floor(appnum / 10^7) - 100000)


# group by year and region_code
invt_year <- invt %>% group_by(year, region_code) %>% 
  count()

# first, just do it by year
invt_year <- invt_year %>% group_by(year) %>% 
  summarise(patent=sum(n, na.rm=T))


invt_year %>% ggplot(aes(year, patent)) +
  labs(title="Number of total inventors by year")+
  scale_x_continuous(name="Year", breaks = seq(1940, 2020, by=10))+
  scale_y_continuous(name="Number of total inventors", labels = scales::label_comma(scale=0.001, suffix = "K"))+
  geom_point() +
  geom_line() +
  theme_bw()
ggsave("results/figures/num_total_inventors.png", dpi=400)



# make appnum unique. group by year and region_code
invt_appnum <- invt %>% distinct(appnum, .keep_all = T)
  
invt_appnum <- invt_appnum %>% group_by(year, region_code) %>% 
  count()

# first, just do it by year
invt_appnum <- invt_appnum %>% group_by(year) %>% 
  summarise(patent=sum(n, na.rm=T))


invt_appnum %>% ggplot(aes(year, patent)) +
  labs(title="Number of total patents by year")+
  scale_x_continuous(name="Year", breaks = seq(1940, 2020, by=10))+
  scale_y_continuous(name="Number of total patents", labels = scales::label_comma(scale=0.001, suffix = "K"))+
  geom_point() +
  geom_line() +
  theme_bw()
ggsave("results/figures/num_total_patents.png", dpi=400)





# do it by year and region?
invt_year <- invt %>% group_by(year, region_code) %>% 
  count()

invt_year <- invt_year %>% group_by(year, region_code) %>% 
  summarise(patent=sum(n, na.rm=T)) %>% ungroup()

invt_year %>% ggplot(aes(year, patent, color=region_code)) +
  labs(title="Number of total inventors by year")+
  scale_x_continuous(name="Year", breaks = seq(1940, 2020, by=10))+
  scale_y_continuous(name="Number of total inventors", labels = scales::label_comma(scale=0.001, suffix = "K"))+
  geom_point() +
  geom_line() +
  theme_bw() +
  guides(color = FALSE)


# Assuming invt_year is your dataset and you're looking for the top N regions based on patent counts
N <- 5  # Set N to the number of top values you want to show

top_regions <- invt_year %>%
  group_by(region_code) %>%
  summarise(total_patents = sum(patent)) %>%
  top_n(N, total_patents) %>%
  pull(region_code)

filtered_data <- invt_year %>%
  filter(region_code %in% top_regions)

# Now plot with the filtered data
ggplot(filtered_data, aes(year, patent, color=region_code)) +
  labs(title="Number of total inventors by year") +
  scale_x_continuous(name="Year", breaks = seq(1940, 2020, by=10)) +
  scale_y_continuous(name="Number of total inventors", labels = scales::label_comma(scale=0.001, suffix = "K")) +
  geom_point() +
  geom_line() +
  theme_bw()





# in domestic data obs: about 5949377 / out of 6025491: 0.9874

# compare this with Manho Kang's paper "Knowledge Spillovers: Evidence from Innovation Cities in South Korea" for which used data for 2003-2019 : Correcting errors, typos, and misreporting, the municipality-level location of 2,283,085 out of 2,283,268 patents filed by domestic applicants are identified between 2003 and 2019.

invt <- read_rds("data/invt_refine_matched.rds")

invt <- invt %>% mutate(year=floor(appnum / 10^7) - 100000)

invt <- invt %>% filter(year>=2003 & year<2020) # 4,760,547

# make it unique by appnum
invt <- invt %>% distinct(appnum, .keep_all = T) #2,021,789 out of 2,283,085 : about 0.8856




# draw map
invt_2003 <- invt %>% filter(year==2003) %>% group_by(region_code) %>% 
  count()

invt_2015 <- invt %>% filter(year==2015) %>% group_by(region_code) %>% 
  count()

invt_2019 <- invt %>% filter(year==2019) %>% group_by(region_code) %>% 
  count()

library(sf)

shp <- read_rds("data/shp.rds")

invt_2003 <- shp %>% left_join(invt_2003, by="region_code")
invt_2003 <- invt_2003 %>% mutate(n=ifelse(is.na(n), 0, n))

invt_2015 <- shp %>% left_join(invt_2015, by="region_code")
invt_2015 <- invt_2015 %>% mutate(n=ifelse(is.na(n), 0, n))

invt_2019 <- shp %>% left_join(invt_2019, by="region_code")
invt_2019 <- invt_2019 %>% mutate(n=ifelse(is.na(n), 0, n))


invt_2003 %>% st_as_sf() %>% mutate(value_q=n %>% sanghoon::classify(4, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=value_q))+
  scale_fill_brewer(name=NULL, label=c("0-25%", "25-50", "50-75", "75-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_patent2003.png", width=10, height=7, dpi=400)

invt_2015 %>% st_as_sf() %>% mutate(value_q=n %>% sanghoon::classify(4, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=value_q))+
  scale_fill_brewer(name=NULL, label=c("0-25%", "25-50", "50-75", "75-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_patent2015.png", width=10, height=7, dpi=400)


invt_2019 %>% st_as_sf() %>% mutate(value_q=n %>% sanghoon::classify(4, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=value_q))+
  scale_fill_brewer(name=NULL, label=c("0-25%", "25-50", "50-75", "75-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_patent2019.png", width=10, height=7, dpi=400)

