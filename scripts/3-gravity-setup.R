

library(tidyverse)
######################################################################################################

# start date: 2024.1.1.
# revised data (most recent): 2024.5.10.
# main coder: Hyoungchul Kim
# main purpose: this code used refined cited and invt_loc data to convert it into gravity model data with i, j, s, t indexes.

######################################################################################################

# read in the data
invt <- haven::read_dta("data/invt_refine_matched.dta")

# read in the cited data
citation <- haven::read_dta("data/citation_all.dta") %>% 
  filter(cited_typecode=="E0802") %>% 
  select(cited, citing)

#gen city share for each appnum
invt <- invt %>% 
  group_by(appnum) %>% 
  mutate(share=1/n()) %>% 
  group_by(appnum, region_code) %>% summarise(share=sum(share)) %>% ungroup()


#gravity data
citation_across <- citation %>%  
  left_join(invt, by=c("citing"="appnum"),
            relationship="many-to-many") %>% 
  rename(citing_iso=region_code, citing_share=share) %>% 
  filter(!is.na(citing_iso)) %>%  
  left_join(invt, by=c("cited"="appnum"),
            relationship="many-to-many") %>% 
  rename(cited_iso=region_code, cited_share=share) %>% 
  filter(!is.na(cited_iso)) %>% 
  mutate(cited_year=floor(cited / 10^7) - 100000,
         citing_year=floor(citing / 10^7) - 100000) %>% 
  select(cited, citing, cited_iso, citing_iso, cited_share, citing_share, cited_year, citing_year)


# make gravity like equation where there is only one value out of citing and cited flow.
citation_across <- citation_across %>% mutate(flow=cited_share*citing_share)

citation_ijst <- citation_across %>% group_by(cited_iso, citing_iso, cited_year, citing_year) %>% 
  summarise(flow=sum(flow)) %>% ungroup()

citation_ijst %>% haven::write_dta("data/citation_city.dta")


# use region label data to match it again.
label <- haven::read_dta("data/region_label_concordance.dta")

# make it unique
label <- label %>% distinct(region_code, .keep_all = T)

label <- label %>% select(region_code, region_label=iso )

# merge them
citation_ijst <- citation_ijst %>% left_join(label, by=c("cited_iso"="region_code")) %>% 
  rename(cited_region_label=region_label)

citation_ijst <- citation_ijst %>% left_join(label, by=c("citing_iso"="region_code")) %>% 
  rename(citing_region_label=region_label)



citation_ijst %>% haven::write_dta("data/citation_city_label.dta")


citation_ijst <- citation_ijst %>% distinct(citing_iso)

check <- label %>% anti_join(citation_ijst, by=c("region_code"="citing_iso"))

