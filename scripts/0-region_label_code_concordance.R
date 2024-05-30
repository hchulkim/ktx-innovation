
######################################################################################################

# start date: 2023.12.19
# revised data (most recent): 2024.5.10.1.
# main coder: Hyoungchul Kim
# main purpose: this file makes concordance for region code and its labels. This is necessary as we have some multiple regions with same region code due to the merge and renaming of the regions. What this code does is (1) makes a concordance data file region_label_concordance.dta that links region names to the region code, (2) Also matches multiple region names to one unique region code. 

# we need to consider three cases for the matching multiple region names to one unique region code.
# For regions that merge, we will also merge them for the past years. 
# For regions that separate, we will merge them back into one again. 
# For regions that changed its name, we need to have its labels as well.

# I already manually adjusted such cases into the following excel file.
# you can see how I did this in a file "region_code_change_region.xlsx" in a ref folder.
######################################################################################################


if (!require(pacman)) (install.packages(pacman))
pacman::p_load(texreg, sf, gt, fixest, tidyverse, here)


data <- readxl::read_excel("data/region_label_code.xlsx")


#only leave distinct five number regions
data <- data %>% filter(str_length(region_code)>=5) %>% 
  mutate(region_code = str_sub(region_code, 1, 5)) %>% 
  
  #take out cases where 1th digit is not zero. This is because they are not the units we will be using.
  filter(str_sub(region_code, 5, 5)=="0") %>%
  # make iso name for regions merging sido and sigungu
  mutate(iso=paste0(sido, " ", sigungu)) %>% 
  # only get distinct values for iso
  distinct(iso, .keep_all = T)

# add 증평군 into 괴산군: this is because 증평군 came our from 괴산군.
data <- data %>% mutate(region_code = ifelse(sigungu=="증평군", "33360", region_code))

check <- data %>% distinct(region_code, .keep_all = T)

check %>% nrow()
# we are left with total 227 unique sigungu level. The reasons data has 245 rows is because there are cases where we have to match multiple region names to one unique code.


# save the data in dta file
data %>% haven::write_dta("data/region_label_concordance.dta")




