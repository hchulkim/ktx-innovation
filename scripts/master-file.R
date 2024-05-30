
# project name: ktx-innovation
# creator: Hyoungchul Kim
# description: this is the master source code_file file. by running this file, you will be able to reproduce all of our refined data, analysis, results and papers from the raw data.
# updated date: 2024-05-10

# this source code file makes all the sum stats we do for this project
source("code_file/00-sum-stat.R")


# this block of code_file makes concordance table to matching region names to region code_files. This also ensures unique region code_file for any multiple region names due to merge or renaming.
source("code_file/0-region_label_code_file_concordance.R")

# this block of code_file refines the raw data for patent innovation to refined data for analysis. Using this source code_file, we retrieve this much obs from the raw obs: 
# in domestic data obs: about 5949377 / out of 6025491: 0.9874
source("code_file/1-refine-invt_loc-data-add-region-code_file")

source("code_file/2-match-refine-data-to-region-code_file")


# this code_file makes merges the patent data with the citation data to make ijst style gravity data

source("code_file/3-gravity-setup.R")


# this code_file makes the ktx dummy variable where region that is close to the ktx station (the distance is in buffer and depends on our research objective) which is used for our OLS analysis

source("code_file/4-refine-ktx-data.R")


