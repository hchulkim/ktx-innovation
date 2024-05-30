
# basic regression test code file: temporary
library(tidyverse)
library(fixest)
library(texreg)

# read in the patent data and ktx dummy data
patent <-  haven::read_dta("data/citation_city_label.dta")

ktx <- haven::read_dta("data/ktx_dummy_rough.dta")

ktx_iv <- haven::read_dta("data/ktx_iv_dummy_rough.dta")



# make data into bilateral flow with just citing year.
patent <- patent %>% group_by(cited_iso, citing_iso, citing_year) %>% 
  summarise(flow=sum(flow, na.rm=T)) %>% ungroup()




# set up expand grid

region <- pull(ktx %>% distinct(region_code))

data <- expand.grid(cited_iso=region, citing_iso=region, citing_year=2000:2014)


data <- data %>% left_join(patent, by=c("cited_iso", "citing_iso", "citing_year"))

data <- data %>% mutate(flow=ifelse(is.na(flow), 0, flow)) # 772935-538134 is zero.



# ktx add

data <- data %>% left_join(ktx %>% select(region_code, year, ktx_dummy_cited=ktx_dummy), by=c("cited_iso"="region_code", "citing_year"="year"))

data <- data %>% left_join(ktx %>% select(region_code, year, ktx_dummy_citing=ktx_dummy), by=c("citing_iso"="region_code", "citing_year"="year"))


data <- data %>% mutate(ktx=ifelse(ktx_dummy_cited==1 & ktx_dummy_citing==1, 1, 0))



# regression



# read in the population
pop <- readxl::read_excel("data/pop.xlsx")

pop <- pop %>% mutate(sigungu=str_extract(sigungu, "\\S+"))

region <- haven::read_dta("data/region_label_concordance.dta")

pop <- pop %>% mutate(across(matches("^1|^2"), ~ ifelse(str_detect(.x, "-"), 0, .x)))


pop <- pop %>% left_join(region, by=c("sido", "sigungu"))

pop <- pop %>% pivot_longer(cols=-c("sido", "sigungu", "region_code", "iso"), names_to = "year", values_to = "pop")

pop <- pop %>% mutate(year=as.numeric(year), pop=as.numeric(pop))

pop <- pop %>% group_by(sido, sigungu, region_code, iso, year) %>% 
  summarise(pop=sum(pop, na.rm=T)) %>% ungroup()

pop <- pop %>% filter(!is.na(iso))

pop <- pop %>% group_by(region_code, year) %>% 
  summarise(pop=sum(pop, na.rm=T)) %>% ungroup()


data <- data %>% left_join(pop %>% rename(pop_cited=pop), by=c("cited_iso"="region_code", "citing_year"="year"))

data <- data %>% left_join(pop %>% rename(pop_citing=pop), by=c("citing_iso"="region_code", "citing_year"="year"))





pop2 <- pop %>% filter(year==1999) %>% select(region_code, pop1999=pop)


data <- data %>% left_join(pop2 %>% rename(pop1999_cited=pop1999), by=c("cited_iso"="region_code"))

data <- data %>% left_join(pop2 %>% rename(pop1999_citing=pop1999), by=c("citing_iso"="region_code"))



data <- data %>%
  mutate(iso_pair = interaction(cited_iso, citing_iso),
         cited_time_pair = interaction(cited_iso, citing_year),
         citing_time_pair = interaction(citing_iso, citing_year)
  )
 



data <- data %>% filter(cited_iso != citing_iso)



# put in commuting zone
cz <- read_rds("data/commuting_zone.rds") %>% select(iso, cz_id, cz_label) %>% 
  bind_rows(tibble(iso="33040", cz_id=11, cz_label="청원군"))



data <- data %>% left_join(cz %>% select(iso, cz_id_cited=cz_id, cz_label_cited=cz_label), by=c("cited_iso"="iso"))

data <- data %>% left_join(cz %>% select(iso, cz_id_citing=cz_id, cz_label_citing=cz_label), by=c("citing_iso"="iso"))

data <- data %>% 
  mutate(cz_pair=interaction(cz_id_cited, cz_id_citing))


# add in the ktx iv

data <- data %>% left_join(ktx_iv, by=c("cited_iso"="region_code")) %>% 
  rename(cited_ktx_iv=ktx_iv_dummy)

data <- data %>% left_join(ktx_iv, by=c("citing_iso"="region_code")) %>% 
  rename(citing_ktx_iv=ktx_iv_dummy)

data <- data %>% mutate(ktx_iv=if_else(cited_ktx_iv==1 & citing_ktx_iv==1, 1, 0))



data <- data %>% mutate(ktx_iv2 = paste0(ktx_iv, "-", as.character(citing_year)))

data <- data %>% mutate(year=as.character(citing_year))








# log, no weight, plain
result1 <- feols(log(flow+1) ~ 1 | iso_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

# size -> distance is already used through iso_pair

# gravity-like?

result2 <- feols(log(flow+1) ~ 1 +  log(pop_cited) + log(pop_citing) | iso_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result3 <- feols(log(flow+1) ~ 1 | iso_pair + cited_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result4 <- feols(log(flow+1) ~ 1 | iso_pair + citing_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result5 <- feols(log(flow+1) ~ 1 | iso_pair + cited_time_pair + citing_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)



screenreg(list(result1, result2, result3, result4, result5), stars = c(0.01, 0.05, 0.1), digits=3, custom.model.names=c("log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)"), custom.coef.names=c("fitted KTX", "log(population_cited)", "log(population_citing)"), custom.gof.rows=list("year FE"=c("yes", "yes", "yes", "yes", "yes"), "sigungu pair FE"=c("yes", "yes", "yes", "yes", "yes"), "i-year FE"=c("no", "no", "yes", "no", "yes"), "j-year FE"=c("no", "no", "no", "yes", "yes"),  "Clustered"=c("cz pair", "cz pair", "cz pair", "cz pair", "cz pair"), "weighted"=c("no", "no", "no", "no", "no")))


texreg(list(result1, result2, result3, result4, result5), stars = c(0.01, 0.05, 0.1), digits=3, custom.model.names=c("log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)"), custom.coef.names=c("fitted KTX Dummy", "log(population_cited)", "log(population_citing)"), custom.gof.rows=list("year FE"=c("yes", "yes", "yes", "yes", "yes"), "sigungu pair FE"=c("yes", "yes", "yes", "yes", "yes"), "i-year FE"=c("no", "no", "yes", "no", "yes"), "j-year FE"=c("no", "no", "no", "yes", "yes"),  "Clustered"=c("cz pair", "cz pair", "cz pair", "cz pair", "cz pair"), "weighted"=c("no", "no", "no", "no", "no")), "results/tables/bilateral_iv_regression.tex")


# first stage

# Extract the first-stage results
screenreg(list(summary(result1, stage=1), summary(result2, stage=1), summary(result3, stage=1), summary(result4, stage=1), summary(result5, stage=1)))



fitstat(result1, "ivwald")
fitstat(result2, "ivwald")
fitstat(result3, "ivwald")
fitstat(result4, "ivwald")
fitstat(result5, "ivwald")

texreg(list(summary(result1, stage=1), summary(result2, stage=1), summary(result3, stage=1), summary(result4, stage=1), summary(result5, stage=1)), "results/tables/first_stage.tex")




#########

# how about 1-10 years

# read in the patent data and ktx dummy data
patent <-  haven::read_dta("data/citation_city_label.dta")

ktx <- haven::read_dta("data/ktx_dummy_rough.dta")


ktx_iv <- haven::read_dta("data/ktx_iv_dummy_rough.dta")


# less than 5years for cited
patent <- patent %>% filter(citing_year-cited_year<11)


# make data into bilateral flow with just citing year.
patent <- patent %>% group_by(cited_iso, citing_iso, citing_year) %>% 
  summarise(flow=sum(flow, na.rm=T)) %>% ungroup()




# set up expand grid

region <- pull(ktx %>% distinct(region_code))

data <- expand.grid(cited_iso=region, citing_iso=region, citing_year=2000:2014)


data <- data %>% left_join(patent, by=c("cited_iso", "citing_iso", "citing_year"))

data <- data %>% mutate(flow=ifelse(is.na(flow), 0, flow)) # 772935-538134 is zero.



# ktx add

data <- data %>% left_join(ktx %>% select(region_code, year, ktx_dummy_cited=ktx_dummy), by=c("cited_iso"="region_code", "citing_year"="year"))

data <- data %>% left_join(ktx %>% select(region_code, year, ktx_dummy_citing=ktx_dummy), by=c("citing_iso"="region_code", "citing_year"="year"))


data <- data %>% mutate(ktx=ifelse(ktx_dummy_cited==1 & ktx_dummy_citing==1, 1, 0))



# regression



# read in the population
pop <- readxl::read_excel("data/pop.xlsx")

pop <- pop %>% mutate(sigungu=str_extract(sigungu, "\\S+"))

region <- haven::read_dta("data/region_label_concordance.dta")

pop <- pop %>% mutate(across(matches("^1|^2"), ~ ifelse(str_detect(.x, "-"), 0, .x)))


pop <- pop %>% left_join(region, by=c("sido", "sigungu"))

pop <- pop %>% pivot_longer(cols=-c("sido", "sigungu", "region_code", "iso"), names_to = "year", values_to = "pop")

pop <- pop %>% mutate(year=as.numeric(year), pop=as.numeric(pop))

pop <- pop %>% group_by(sido, sigungu, region_code, iso, year) %>% 
  summarise(pop=sum(pop, na.rm=T)) %>% ungroup()

pop <- pop %>% filter(!is.na(iso))

pop <- pop %>% group_by(region_code, year) %>% 
  summarise(pop=sum(pop, na.rm=T)) %>% ungroup()


data <- data %>% left_join(pop %>% rename(pop_cited=pop), by=c("cited_iso"="region_code", "citing_year"="year"))

data <- data %>% left_join(pop %>% rename(pop_citing=pop), by=c("citing_iso"="region_code", "citing_year"="year"))





pop2 <- pop %>% filter(year==1999) %>% select(region_code, pop1999=pop)


data <- data %>% left_join(pop2 %>% rename(pop1999_cited=pop1999), by=c("cited_iso"="region_code"))

data <- data %>% left_join(pop2 %>% rename(pop1999_citing=pop1999), by=c("citing_iso"="region_code"))



data <- data %>%
  mutate(iso_pair = interaction(cited_iso, citing_iso),
         cited_time_pair = interaction(cited_iso, citing_year),
         citing_time_pair = interaction(citing_iso, citing_year)
  )
# 
# 
# # get distance
# # read in the geo data to get the distance between sould and other places
# library(sf)
# 
# # read in region crosswalk code and shape data
# geo <- st_read("data/map_korea.geojson")
# 
# memory.limit(size = 32000)
# # Step 1: Calculate centroids for each region
# geo_centroids <- geo %>% 
#   st_simplify(preserveTopology = TRUE, dTolerance = 100) %>%
#   mutate(centroid = st_centroid(geometry))
# 
# # Step 2: Create all pairs of regions
# # To avoid duplication and self-pairing, you might filter out pairs with the same region_code or identical pairs later
# region_pairs <- crossing(region_code1 = geo_centroids$region_code, region_code2 = geo_centroids$region_code) %>%
#   # Removing identical pairs (optional, based on your requirement)
#   filter(region_code1 != region_code2)
# 
# # Step 3: Calculate distances for each pair
# # Join the centroids back to get the geometry for each region_code in the pairs
# region_pairs_with_geometry <- region_pairs %>%
#   left_join(geo_centroids %>% select(region_code, centroid), by = c("region_code1" = "region_code")) %>%
#   rename(centroid1 = centroid) %>%
#   left_join(geo_centroids %>% select(region_code, centroid), by = c("region_code2" = "region_code")) %>%
#   rename(centroid2 = centroid)
# 
# region_pairs_with_geometry <- region_pairs_with_geometry %>% select(region_code1, region_code2, centroid1, centroid2)
# 
# # Calculate distance
# region_pairs_with_geometry <- region_pairs_with_geometry %>%
#   mutate(dist = st_distance(centroid1, centroid2))
# 
# # Select only the required columns
# geo <- region_pairs_with_geometry %>%
#   st_drop_geometry() %>% 
#   select(region_code1, region_code2, dist)
# 
# 
# data <- data %>% 
#   left_join(geo, by="region_code")
# 
# 
# 



data <- data %>% filter(cited_iso != citing_iso)



# put in commuting zone
cz <- read_rds("data/commuting_zone.rds") %>% select(iso, cz_id, cz_label) %>% 
  bind_rows(tibble(iso="33040", cz_id=11, cz_label="청원군"))



data <- data %>% left_join(cz %>% select(iso, cz_id_cited=cz_id, cz_label_cited=cz_label), by=c("cited_iso"="iso"))

data <- data %>% left_join(cz %>% select(iso, cz_id_citing=cz_id, cz_label_citing=cz_label), by=c("citing_iso"="iso"))

data <- data %>% 
  mutate(cz_pair=interaction(cz_id_cited, cz_id_citing))


# add in the ktx iv

data <- data %>% left_join(ktx_iv, by=c("cited_iso"="region_code")) %>% 
  rename(cited_ktx_iv=ktx_iv_dummy)

data <- data %>% left_join(ktx_iv, by=c("citing_iso"="region_code")) %>% 
  rename(citing_ktx_iv=ktx_iv_dummy)

data <- data %>% mutate(ktx_iv=if_else(cited_ktx_iv==1 & citing_ktx_iv==1, 1, 0))



data <- data %>% mutate(ktx_iv2 = paste0(ktx_iv, "-", as.character(citing_year)))

data <- data %>% mutate(year=as.character(citing_year))








# log, no weight, plain
result1 <- feols(log(flow+1) ~ 1 | iso_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

# size -> distance is already used through iso_pair

# gravity-like?

result2 <- feols(log(flow+1) ~ 1 +  log(pop_cited) + log(pop_citing) | iso_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result3 <- feols(log(flow+1) ~ 1 | iso_pair + cited_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result4 <- feols(log(flow+1) ~ 1 | iso_pair + citing_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)

result5 <- feols(log(flow+1) ~ 1 | iso_pair + cited_time_pair + citing_time_pair + citing_year | ktx ~ ktx_iv:citing_year, data=data, cluster = ~cz_pair)



screenreg(list(result1, result2, result3, result4, result5), stars = c(0.01, 0.05, 0.1), digits=3, custom.model.names=c("log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)"), custom.coef.names=c("fitted KTX", "log(population_cited)", "log(population_citing)"), custom.gof.rows=list("year FE"=c("yes", "yes", "yes", "yes", "yes"), "sigungu pair FE"=c("yes", "yes", "yes", "yes", "yes"), "i-year FE"=c("no", "no", "yes", "no", "yes"), "j-year FE"=c("no", "no", "no", "yes", "yes"),  "Clustered"=c("cz pair", "cz pair", "cz pair", "cz pair", "cz pair"), "weighted"=c("no", "no", "no", "no", "no")))


texreg(list(result1, result2, result3, result4, result5), stars = c(0.01, 0.05, 0.1), digits=3, custom.model.names=c("log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)", "log(patents+1)"), custom.coef.names=c("fitted KTX Dummy", "log(population_cited)", "log(population_citing)"), custom.gof.rows=list("year FE"=c("yes", "yes", "yes", "yes", "yes"), "sigungu pair FE"=c("yes", "yes", "yes", "yes", "yes"), "i-year FE"=c("no", "no", "yes", "no", "yes"), "j-year FE"=c("no", "no", "no", "yes", "yes"),  "Clustered"=c("cz pair", "cz pair", "cz pair", "cz pair", "cz pair"), "weighted"=c("no", "no", "no", "no", "no")), "results/tables/bilateral_iv_regression_less10.tex")


# first stage

# Extract the first-stage results
screenreg(list(summary(result1, stage=1), summary(result2, stage=1), summary(result3, stage=1), summary(result4, stage=1), summary(result5, stage=1)))



fitstat(result1, "ivwald")
fitstat(result2, "ivwald")
fitstat(result3, "ivwald")
fitstat(result4, "ivwald")
fitstat(result5, "ivwald")

texreg(list(summary(result1, stage=1), summary(result2, stage=1), summary(result3, stage=1), summary(result4, stage=1), summary(result5, stage=1)), "results/tables/first_stage_less10.tex")







