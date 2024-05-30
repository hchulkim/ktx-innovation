
######################################################################################################

# start date: 2023.12.19
# revised data (most recent): 2024.5.10.
# main coder: Hyoungchul Kim
# main purpose: this code refines the address part of invt_loc.dta file and then merges region code into the data. 
# notice: for replication purpose, we start from untouched raw data downloaded from the source. However, the name of the file is changed into english as Korean letters are not well recognized in R.

# In terms of data: using this source code file, we can identify about 98.8% of the total raw data for analysis.

######################################################################################################

## 0. load basic libraries
library(tidyverse)
library(fuzzyjoin) # used to fuzzy join the sigungu labels.

  
  ## 1. load the main data
  
  # read in the data linking patent number and address
  invt <- haven::read_dta("data/invt_loc.dta") %>% 
    select(appnum, address)
  
# this code refines the region data used for refining
  
  # read in the raw region data used for refining
  region_data <- readxl::read_excel("data/district_data200101.xls", sheet=5, skip=1)
  
  # select only necessary columns
  region_data <- region_data %>% select(sido=`시도`, sigungu=`시군구`, dong1=`행정동(행정기관명)`, dong2=`법정동`, dong1_eng=`행정동 영문명칭`)
  
  # take out any white space in the back and front.
  region_data <- region_data %>% mutate(sido=str_replace_all(sido, "\\s+$", ""),
                                        sigungu=str_replace_all(sigungu, "\\s+$", ""),
                                        dong1=str_replace_all(dong1, "\\s+$", ""),
                                        dong2=str_replace_all(dong2, "\\s+$", ""))
  
  region_data <- region_data %>% mutate(sido=str_replace_all(sido, "^\\s+", ""),
                                        sigungu=str_replace_all(sigungu, "^\\s+", ""),
                                        dong1=str_replace_all(dong1, "^\\s+", ""),
                                        dong2=str_replace_all(dong2, "^\\s+", ""))
  
  # make sure this is unique by column values
  region_data <- region_data %>% distinct(sido, sigungu, dong1, dong2, .keep_all = T)
  
  region_data <- region_data %>% filter(dong1!=dong2)
  
  region_data <- region_data %>% filter(sigungu!=dong1)
  
  region_data <- region_data %>% filter(sido!=sigungu)
  
  # save the data
  region_data %>% write_rds("data/district_data_refined.rds")
  
  # read in the refined region data used for refining
  region_data <- read_rds("data/district_data_refined.rds")
  
  # check if there is not unique sigungu or dong
  check <- region_data %>% distinct(sido, sigungu, .keep_all = T)
  
  check <- check %>% group_by(sigungu) %>% 
    count() %>% filter(n>1) %>% view() 
  
  check <- region_data %>% distinct(sido, sigungu, dong1, .keep_all = T)
  
  check <- check %>% group_by(dong1) %>% 
    count() %>% filter(n>1) %>% view() 
  
  check <- region_data %>% distinct(sido, sigungu, dong2, .keep_all = T)
  
  check <- check %>% group_by(dong2) %>% 
    count() %>% filter(n>1) %>% view() 
  # there are many not unique names. Need to make sure this do not cause problem.
  
  # also read in the region label data with region code for sido and sigungu matching
  # sido_sigungu <- readxl::read_excel("data/region_label_code.xlsx")
  # 
  # sido_sigungu <- sido_sigungu %>% filter(str_length(as.character(region_code))==5 )
  # sido_sigungu <- sido_sigungu %>% filter(str_sub(as.character(region_code), 5, 5)=="0")
  # sido_sigungu <- sido_sigungu %>% distinct(sido, sigungu, .keep_all = T)
  # 
  # # save it
  # sido_sigungu %>% write_rds("data/sido_sigungu_matching_label.rds")
  # 
  ## 2. basic refining for invt data
  
  # If address do not have any alphabets or korean, make it into NA
  invt <- invt %>% mutate(address = ifelse(str_detect(address, "[[:alpha:]가-힣]"), address, NA))
  
  # first take out data that has NA for the address or appnum as we cannot use them.
  invt <- invt %>% filter(!is.na(address))
  invt <- invt %>% filter(!is.na(appnum))
  
  # take out case where address only has length <2: impossible to fix the address.
  invt <- invt %>% filter(str_length(address)>1)
  
  # also take out special characters. Just leave alphabet and korean letters.
  # find characters that are neither letters, Korean characters, nor whitespace.
  invt <- invt %>% mutate(address = str_replace_all(address, "[^[:alpha:]가-힣[:space:]]", ""))
  
  # Remove leading and ending white space
  invt <- invt %>% mutate(address = str_replace(address, "^\\s+", ""))
  
  invt <- invt %>% mutate(address = str_replace(address, "\\s+$", ""))
  
  ## 3. first, find the sido variable
  
  # if there is sido name in the address, use it as the address. otherwise, leave if NA
  # do some manual cases for very typical typos.
  invt <- invt %>% mutate(sido = case_when(
    str_detect(address, "서울") ~ "서울특별시",
    str_detect(address, "울특별시") ~ "서울특별시",
    str_detect(address, "부산") ~ "부산광역시",
    str_detect(address, "대구") ~ "대구광역시",
    str_detect(address, "인천") ~ "인천광역시",
    str_detect(address, "광주") ~ "광주광역시",
    str_detect(address, "대전") ~ "대전광역시",
    str_detect(address, "울산") ~ "울산광역시",
    str_detect(address, "산광역시") ~ "부산광역시",
    str_detect(address, "구광역시") ~ "대구광역시",
    str_detect(address, "세종") ~ "세종특별자치시",
    str_detect(address, "강원") ~ "강원도",
    str_detect(address, "경기") ~ "경기도",
    str_detect(address, "충북|충청북도|충청북|충청북동|충청북둥|충청부도") ~ "충청북도",
    str_detect(address, "충남|충청남도|충청남|충청남동|충청남둥") ~ "충청남도",
    str_detect(address, "경북|경상북도|경상북|경상북동|경상북둥|경상복도") ~ "경상북도",
    str_detect(address, "경남|경상남도|경상남|경상남둥|경상남동|경상나도|걍상남도") ~ "경상남도",
    str_detect(address, "전북|전라북도|전라북|전라북둥|전라북동") ~ "전라북도",
    str_detect(address, "전남|전라남도|전라남|전라남동|전라남둥|전라암도") ~ "전라남도",
    str_detect(address, "제주|주특별자치도") ~ "제주특별자치도",
    TRUE ~ NA
  ))
  
  # If there is some overlap in the var (e.g. seoul and gwangju), it gives NA to sido value.
  
  # # Custom function to check for overlaps
  # check_overlaps <- function(address) {
  #   # Define the patterns and corresponding names
  #   patterns <- c("서울", "울특별시", "부산", "대구", "인천", "광주", "대전", "울산", "산광역시", "구광역시", "세종", "강원", "경기", "충북|충청북도|충청북|충청북동|충청북둥|충청부도", "충남|충청남도|충청남|충청남동|충청남둥", "경북|경상북도|경상북|경상북동|경상북둥|경상복도", "경남|경상남도|경상남|경상남둥|경상남동|경상나도|걍상남도", "전북|전라북도|전라북|전라북둥|전라북동", "전남|전라남도|전라남|전라남동|전라남둥|전라암도", "제주|주특별자치도")
  #   names <- c("서울특별시", "서울특별시", "부산광역시", "대구광역시", "인천광역시", "광주광역시", "대전광역시", "울산광역시", "부산광역시", "대구광역시", "세종특별자치시", "강원도", "경기도", "충청북도", "충청남도", "경상북도", "경상남도", "전라북도", "전라남도", "제주특별자치도")
  #   
  #   # Check for each pattern and count matches
  #   matches <- sapply(patterns, function(p) str_detect(address, p))
  #   if (sum(matches) != 1) {
  #     return(NA)
  #   } else {
  #     return(names[which(matches)])
  #   }
  # }
  # 
  # # Apply the custom function
  # invt <- invt %>% mutate(sido = sapply(address, check_overlaps))
  
  
  # if above takes too long, use the commented code below
  
  # library(parallel)
  # 
  # # Detect the number of cores
  # no_cores <- detectCores() - 1  # leave one core free for system processes
  # 
  # # Split your data frame into a list of smaller data frames
  # df_list <- split(invt, cut(1:nrow(invt), no_cores))
  # 
  # # Use parallel lapply to process each part of the list
  # results_list <- mclapply(df_list, function(df) {
  #   df %>% mutate(sido = sapply(address, check_overlaps))
  # }, mc.cores = no_cores)
  # 
  # # Combine the results back into one data frame
  # invt <- do.call(rbind, results_list)
  
  
  # check how many NA values are there for sido
  check <- invt %>% filter(is.na(sido)) # about 16.6%
  
  # take care of 경기도 광주시 case
  invt_gwang <- invt %>% filter(str_detect(address, "광주"))
  invt <- invt %>% filter(!str_detect(address, "광주") | is.na(sido))
  
  # if address have "경기" in it, change sido to "경기". also with 광주시
  invt_gwang <- invt_gwang %>% mutate(sido=ifelse(str_detect(address, "경기") & !str_detect(address, "광역"), "경기도", sido))
  invt_gwang <- invt_gwang %>% mutate(sigungu=ifelse(str_detect(address, "경기") & !str_detect(address, "광역"), "광주시", NA))
  
  invt_gwang <- invt_gwang %>% mutate(sido=ifelse(str_detect(address, "광주시") & !str_detect(address, "동구|서구|남구|북구|광산구"), "경기도", sido))
  
  invt_gwang <- invt_gwang %>% mutate(sigungu=ifelse(str_detect(address, "광주시") & !str_detect(address, "동구|서구|남구|북구|광산구"), "광주시", sigungu))
  
  invt_gwang1 <- invt_gwang %>% filter(sido=="경기도")
  invt_gwang2 <- invt_gwang %>% filter(sido=="광주광역시") # some values are omitted as it has both seoul and gwangju. total 4 obs are dropped from the invt_gwang data.
  
  invt <- invt %>% bind_rows(invt_gwang2 %>% select(-sigungu))
  
  # take out 광주군 into 경기도 광주시
  invt_gwang3 <- invt %>% filter(address=="광주군") # total 3 obs
  invt <- invt %>% filter(address!="광주군")
  
  invt_gwang3 <- invt_gwang3 %>% mutate(sido="경기도", sigungu="광주시")
  invt_gwang1 <- invt_gwang1 %>% bind_rows(invt_gwang3)
  
  
  
  
  # manually take out many foreign variables as possible
  # only manually check foreign country name for values that have NA in sido value.

  invt_for <- invt %>% filter(str_detect(address, "대만|일본|중국|스위스|워신톤|미국|동경|파리|호동|타이페이|대만|이베르동|동독|중화인민공화국|중화민국|그리스|독일|타이완|베트남|인도|인디아|삿포로|파키스탄|필리핀|홍콩|방글라데시|싱가포르|러시아|요코하마|후쿠오카|멕시코|맥시코|오사카|나고야|캐나다|프랑스|대영|말레이시아|이탈리아|나가노|쿄토|스페인|요꼬하마|오스트리아|영국|벨기에|영국|덴마크|브라질|덴마크|네덜란드|쿠웨이트|이라크|우크라이나|이스라엘|스웨덴|폴란드|오스트레일리아|룩셈부르크|핀란드|리투아니아|포르투갈|헝가리|아일랜드|체코|핀랜드|핀란드|헝가리|태국|불가리아|캄보디아|슬로바키아|도쿄|오사카|노르웨이|우즈베키|일리노이|하와이|벨라루스|모스코바|베네수엘라|남아프리카|캘리포니아|아르헨티나|뉴질랜드|뉴욕|이집트|싱가폴|텍사스") & is.na(sido))

  invt <- invt %>% filter(!str_detect(address, "대만|일본|중국|스위스|워신톤|미국|동경|파리|호동|타이페이|대만|이베르동|동독|중화인민공화국|중화민국|그리스|독일|타이완|베트남|인도|인디아|삿포로|파키스탄|필리핀|홍콩|방글라데시|싱가포르|러시아|요코하마|후쿠오카|멕시코|맥시코|오사카|나고야|캐나다|프랑스|대영|말레이시아|이탈리아|나가노|쿄토|스페인|요꼬하마|오스트리아|영국|벨기에|영국|덴마크|브라질|덴마크|네덜란드|쿠웨이트|이라크|우크라이나|이스라엘|스웨덴|폴란드|오스트레일리아|룩셈부르크|핀란드|리투아니아|포르투갈|헝가리|아일랜드|체코|핀랜드|핀란드|헝가리|태국|불가리아|캄보디아|슬로바키아|도쿄|오사카|노르웨이|우즈베키|일리노이|하와이|벨라루스|모스코바|베네수엘라|남아프리카|캘리포니아|아르헨티나|뉴질랜드|뉴욕|이집트|싱가폴|텍사스") | !is.na(sido))

# data decomposition: invt_for, invt, invt_gwang1
  
# check again missing sido
  check <- invt %>% filter(is.na(sido)) 
  # 42634 missing sido obs.
  
  # if the variable only has english, take it out
  # Tibble with only English letters and white spaces
  english_only <- invt %>%
    filter(str_detect(address, "^[A-Za-z\\s]+$"))
  
  # add this into invt_for
  invt_for <- invt_for %>% bind_rows(english_only)
  
  # Tibble with other characters
  invt <- invt %>%
    filter(!str_detect(address, "^[A-Za-z\\s]+$"))
  
  
  # before doing fuzzyjoin, we need to manually take care of cases that make multiple matching
  
  # 강원도: 
  # 강원도 속초시 고성군 -> 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "강원도 속초시 고성군")
  
  # 경기도 :
  # 경기도 고양시 양평군 => 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "경기도 고양시 양평군")
  # 경기도 성남시 안양시 만안 => 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "경기도 성남시 안양시 만안")
  # 경기도 수원시 평택시 => 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "경기도 수원시 평택시")
  # 경기도 용인시 처인구 명지로 전자공학과겸임교수 => 용인시로 change. possible to identify
  invt <- invt %>% 
    mutate(address = ifelse(str_detect(address, "경기도 용인시"), "경기도 용인시", address))
  # 군포시 => 경기도 군포시로 정정
  invt <- invt %>% 
    mutate(address = ifelse(str_detect(address, "군포시"), "경기도 군포시", address))
  
  # 경기도수원시의왕시 => 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "경기도수원시의왕시")
  
  # 경상북도:
  # 경상북도 구미시 칠곡군=> 삭제 - not possible to identify
  invt <- invt %>% 
    filter(address != "경상북도 구미시 칠곡군")
  invt <- invt %>% 
    filter(address != "경상북도구미시칠곡군")
  
  # 울산광역시
  # => 울산광역시 남구 문수로  범서면 굴화 강변그린빌 동 호 옥동 울주군 => 울주군으로 변경.
  invt <- invt %>% 
    mutate(address = ifelse(str_detect(address, "범서면 굴화 강변그린빌"), "울산광역시 울주군", address))
  
  # 전라북도:
  # 전라북도 전주시 완주군 => 삭제.
  invt <- invt %>% 
    filter(address != "전라북도 전주시 완주군")
  # 전라북도 전주시 임실군 => 삭제
  invt <- invt %>% 
    filter(address != "전라북도 전주시 임실군")
  # 전북 전주시 완주군 => 삭제.
  invt <- invt %>% 
    filter(address != "전북 전주시 완주군")
  
  # 제주특별자치도: 
  # 제주시서귀포시 => 삭제.
  invt <- invt %>% 
    filter(address != "제주시서귀포시")
  
  
  # we also need to take care of case where there can be multiple matching due to similarity of 동구, 서구, 남구, 북구 with other district like 달서구, 강동구.
  
  # Here is the list of districts for such possible mistake:
  # 동,서,남,북구
  # 성동구
  # 성북구
  # 강북구
  # 강서구
  # 강남구
  # 강동구
  # 달서구
  # 남동구
  # steps to solve this is as follows
  # 1. first filter out case where sido is NA because this is the case where it can lead into problem: e.g. the address might only have information of gu such as 동구.
  invt_prob <- invt %>% filter(is.na(sido) & str_detect(address, "동구|서구|남구|북구"))
  invt <- invt %>% filter(!is.na(sido) | !str_detect(address, "동구|서구|남구|북구"))
  
  # 2. there is only 850-ish obs. So it can be checked with naked eye, I will manually fix the problem. At least one that can be identified will be fixed and added back to invt. Other obs that cannot be identified will be filtered here.
  
  # filter out 천진시 진남구 and add it into invt_for
  invt_for1 <- invt_prob %>% filter(address=="천진시 진남구")
  invt_prob <- invt_prob %>% filter(address!="천진시 진남구")
  invt_for <- invt_for %>% bind_rows(invt_for1)
  
  # as most of the values are districts in seoul, if there is such name in address, fix it to seoul address manually
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "성동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "성동구"), "성동구", NA))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "성북구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "성북구"), "성북구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강남구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강남구"), "강남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "간남구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "간남구"), "강남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강동구"), "강동구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강서구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강서구"), "강서구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강북구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강북구"), "강북구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "서초동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "서초동구"), "서초구", sigungu))
  
  
# do similar thing for other metro
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "천안"), "충청남도", sido), sigungu = ifelse(str_detect(address, "천안"), "천안시", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "고양"), "경기도", sido), sigungu = ifelse(str_detect(address, "고양"), "고양시", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천시"), "경기도", sido), sigungu = ifelse(str_detect(address, "이천시"), "이천시", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "부천시"), "경기도", sido), sigungu = ifelse(str_detect(address, "부천시"), "부천시", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "포항"), "경상북도", sido), sigungu = ifelse(str_detect(address, "포항"), "포항시", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "공주광역시서구"), "광주광역시", sido), sigungu = ifelse(str_detect(address, "공주광역시서구"), "서구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "주광역시 북구"), "광주광역시", sido), sigungu = ifelse(str_detect(address, "주광역시 북구"), "북구", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대주직할시남구"), "대구광역시", sido), sigungu = ifelse(str_detect(address, "대주직할시남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대국광영시 동구"), "대구광역시", sido), sigungu = ifelse(str_detect(address, "대국광영시 동구"), "동구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "울상광역시 남구"), "울산광역시", sido), sigungu = ifelse(str_detect(address, "울상광역시 남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "울신광역시 남구"), "울산광역시", sido), sigungu = ifelse(str_detect(address, "울신광역시 남구"), "남구", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 남동구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 남동구"), "남동구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인찬광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인찬광역시 남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천광역시북구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천광역시북구"), "북구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인차광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인차광역시 남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천광역시 남동구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천광역시 남동구"), "남동구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 서구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 서구"), "서구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "천광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "천광역시 남구"), "남구", sigungu))
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천직할시북구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천직할시북구"), "북구", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대정광역시 서구"), "대전광역시", sido), sigungu = ifelse(str_detect(address, "대정광역시 서구"), "서구", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대젼광역시 서구"), "대전광역시", sido), sigungu = ifelse(str_detect(address, "대젼광역시 서구"), "서구", sigungu))
  
  invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "부신광역시 북구"), "부산광역시", sido), sigungu = ifelse(str_detect(address, "부신광역시 북구"), "북구", sigungu))
  
  check <- invt_prob %>% filter(is.na(sido)) #we cannot use this 35 obs as it is impossible to identify them.
  
  # add invt_prob with no NA back into the invt: 35 obs dropped.
  invt <- invt %>% bind_rows(invt_prob %>% filter(!is.na(sido)))
  
  
  
  # as invt obs with non NA sigungu are already refined values. So leave this out
invt_final <- invt %>% filter(!is.na(sigungu))
invt <- invt %>% filter(is.na(sigungu))
  
  # decomposition of the data: invt_final, invt, invt_gwang1, invt_for
  
# 2. similarly, also do this for the case where sido is not NA as well.
invt_prob <- invt %>% filter(!is.na(sido) & str_detect(address, "동구|서구|남구|북구"))
invt <- invt %>% filter(is.na(sido) | !str_detect(address, "동구|서구|남구|북구"))
  
  
# as most of the values are districts in seoul, if there is such name in address, fix it to seoul address manually

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "동구"), "동구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "서구"), "서구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "북구"), "북구", sigungu))


invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "성동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "성동구"), "성동구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "성북구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "성북구"), "성북구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강남구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강남구"), "강남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "간남구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "간남구"), "강남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강동구"), "강동구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강서구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강서구"), "강서구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "강북구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "강북구"), "강북구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "서초동구"), "서울특별시", sido), sigungu = ifelse(str_detect(address, "서초동구"), "서초구", sigungu))


# do similar thing for other metro
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "천안"), "충청남도", sido), sigungu = ifelse(str_detect(address, "천안"), "천안시", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "고양"), "경기도", sido), sigungu = ifelse(str_detect(address, "고양"), "고양시", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천시"), "경기도", sido), sigungu = ifelse(str_detect(address, "이천시"), "이천시", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "부천시"), "경기도", sido), sigungu = ifelse(str_detect(address, "부천시"), "부천시", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "포항"), "경상북도", sido), sigungu = ifelse(str_detect(address, "포항"), "포항시", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "공주광역시서구"), "광주광역시", sido), sigungu = ifelse(str_detect(address, "공주광역시서구"), "서구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "주광역시 북구"), "광주광역시", sido), sigungu = ifelse(str_detect(address, "주광역시 북구"), "북구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대주직할시남구"), "대구광역시", sido), sigungu = ifelse(str_detect(address, "대주직할시남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대국광영시 동구"), "대구광역시", sido), sigungu = ifelse(str_detect(address, "대국광영시 동구"), "동구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "울상광역시 남구"), "울산광역시", sido), sigungu = ifelse(str_detect(address, "울상광역시 남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "울신광역시 남구"), "울산광역시", sido), sigungu = ifelse(str_detect(address, "울신광역시 남구"), "남구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 남동구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 남동구"), "남동구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인찬광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인찬광역시 남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천광역시북구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천광역시북구"), "북구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인차광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인차광역시 남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천광역시 남동구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천광역시 남동구"), "남동구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청광역시 서구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "인청광역시 서구"), "서구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "천광역시 남구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "천광역시 남구"), "남구", sigungu))
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "이천직할시북구"), "인천광역시", sido), sigungu = ifelse(str_detect(address, "이천직할시북구"), "북구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대정광역시 서구"), "대전광역시", sido), sigungu = ifelse(str_detect(address, "대정광역시 서구"), "서구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "대젼광역시 서구"), "대전광역시", sido), sigungu = ifelse(str_detect(address, "대젼광역시 서구"), "서구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "부신광역시 북구"), "부산광역시", sido), sigungu = ifelse(str_detect(address, "부신광역시 북구"), "북구", sigungu))

  
# just manually take care of multiple case
# 성동구
# 성북구
# 강북구
# 강서구
# 강남구
# 강동구
# 달서구
# 남동구
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "남동구"), "남동구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "달서구"), "달서구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "강동구"), "강동구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "강남구"), "강남구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "강서구"), "강서구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "강북구"), "강북구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "성북구"), "성북구", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "성동구"), "성동구", sigungu))


check <- invt_prob %>% filter(is.na(sigungu)) # no missing obs

# add this also into invt_final: then the case of multiple matching will be solved.
invt_final <- invt_final %>% bind_rows(invt_prob)

# decomposition of the data: invt_final, invt, invt_gwang1, invt_for


# note: now, invt data only has variables that will not be matched n:n.
# so we can now do fuzzyjoin.


# use sido_sigungu to do sigungu matching
# take out 경기도 광주시 and 
# 동서남북구
# 성동구
# 성북구
# 강북구
# 강서구
# 강남구
# 강동구
# 달서구
# 남동구
# as we have already done it

region_data <- region_data %>% filter(sigungu!="광주시")
region_data <- region_data %>% filter(sigungu!="동구")
region_data <- region_data %>% filter(sigungu!="서구")
region_data <- region_data %>% filter(sigungu!="남구")
region_data <- region_data %>% filter(sigungu!="북구")
region_data <- region_data %>% filter(sigungu!="성동구")
region_data <- region_data %>% filter(sigungu!="성북구")
region_data <- region_data %>% filter(sigungu!="강북구")
region_data <- region_data %>% filter(sigungu!="강서구")
region_data <- region_data %>% filter(sigungu!="강남구")
region_data <- region_data %>% filter(sigungu!="강동구")
region_data <- region_data %>% filter(sigungu!="달서구")
region_data <- region_data %>% filter(sigungu!="남동구")



region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "포항시"), "포항시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "수원시"), "수원시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "성남시"), "성남시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "창원"), "창원시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "용인시"), "용인시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "전주시"), "전주시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "고양시"), "고양시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "청주시"), "청주시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "천안시"), "천안시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "안산시"), "안산시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "안양시"), "안양시", sigungu))
region_data <- region_data %>% mutate(
  sigungu = ifelse(str_detect(sigungu, "수원시"), "수원시", sigungu))

region_data <- region_data %>% 
  select(sido, sigungu) %>% distinct(sido, sigungu, .keep_all = T)

region_data <- region_data %>% filter(sido!=sigungu)


# we also take care of 경기도 남양주시, and 경기도 양주시 separately as this is complicated.
region_data <- region_data %>% filter(sigungu!="양주시")
region_data <- region_data %>% filter(sigungu!="남양주시")

invt_prob <- invt %>% filter(str_detect(address, "양주"))
invt <- invt %>% filter(!str_detect(address, "양주"))


invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "양주시"), "양주시", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "남양주시"), "남양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "양주군"), "양주시", sigungu))
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "남양주군"), "남양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "남양주"), "남양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "경상남도|경남|남도") & str_detect(address, "양산"), "양산시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "경기도 양주"), "양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "경기 양주"), "양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "경기도  양주산성로"), "양주시", sigungu))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "강원도양주군"), "양구군", sigungu))

# add this also into invt_final

invt_final <- invt_final %>% bind_rows(invt_prob)


# decomposition of the data: invt_final, invt, invt_gwang1, invt_for

# now any possible problem in fuzzyjoin is gone. So we do fuzzyjoin now.

# invt is now 4931242 obs

# library(fuzzyjoin)

# Fuzzy join using regex
invt <- invt %>% fuzzy_left_join(region_data, by=c("address"="sigungu", "sido"="sido"), match_fun = stringr::str_detect)

# 4931242: no multiple matching happening.

invt <- invt %>% select(appnum, address, sido=sido.x, sigungu=sigungu.y)



# also take out sejong

invt_prob <- invt %>% filter(str_detect(address, "세종"))
invt <- invt %>% filter(!str_detect(address, "세종"))

invt_prob <- invt_prob %>% mutate(sido="세종특별자치시", sigungu=NA)

# add it to invt_final
invt_final <- invt_final %>% bind_rows(invt_prob)




# now see how many NA exists in invt
invt_prob <- invt %>% filter(is.na(sigungu))
invt <- invt %>% filter(!is.na(sigungu))

# add invt to the final as it is done.
invt_final <- invt_final %>% bind_rows(invt)

# also add invt_gwang1 to the final.
invt_final <- invt_final %>% bind_rows(invt_gwang1)

# so now we only need to take care of invt_prob. all the rest refined data is in invt_final.
# now we need to see what is wrong with the invt_prob data

# firstly, one with not NA sido.
check <- invt_prob %>% filter(!is.na(sido)) %>% distinct(sido, .keep_all = T) %>% 
  select(-c("sido", "sigungu"))

# manually fix some variable. We don't necessarily need to do this, but it's for precision. trying to leave as many obs as possible.
check <- check %>% mutate(sido=ifelse(str_detect(address, "경기도"), "경기도", NA))
check <- check %>% mutate(sido=ifelse(str_detect(address, "강원도"), "강원도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "전남"), "전라남도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "경상북도"), "경상북도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "경상남도"), "경상남도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "충청남도"), "충청남도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "충북"), "충청북도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "대구"), "대구광역시", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "인천"), "인천광역시", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "대전"), "대전광역시", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "제주"), "제주특별자치도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "전라북도"), "전라북도", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "울산"), "울산광역시", sido))
check <- check %>% mutate(sido=ifelse(str_detect(address, "광주광역시"), "광주광역시", sido))

check <- check %>% bind_cols(sigungu = c("화성시", 
                                         "시흥시",
                                         "나주시",
                                         "속초시",
                                         NA,
                                         "달서구",
                                         "제주시",
                                         "익산시",
                                         "아산시",
                                         "양산시",
                                         "유성구",
                                         "청주시",
                                         "강릉시",
                                         NA,
                                         "군포시",
                                         NA,
                                         NA))

invt_prob <- invt_prob %>% left_join(check %>% select(address, sido, sigungu), by="address")
invt_prob <- invt_prob %>% mutate(sido = ifelse(is.na(sido.x), sido.y, sido.x), sigungu = ifelse(is.na(sigungu.x), sigungu.y, sigungu.x)) %>% 
  select(appnum, address, sido, sigungu)

invt_fix <- invt_prob %>% filter(!is.na(sido) & !is.na(sigungu))
invt_prob <- invt_prob %>% filter(is.na(sido) | is.na(sigungu))

# add fixed values into invt_final
invt_final <- invt_final %>% bind_rows(invt_fix)





# now do fuzzy join one more time. but we take out district unit term: si, gun, gu to match more obs.
region_data <- region_data %>% mutate(sigungu2 = str_sub(sigungu, 1, -2))

# at least have two letters as only one can be problematic
region_data <- region_data %>% mutate(sigungu2 = ifelse(str_length(sigungu2)==1, sigungu, sigungu2))

# take care for case where multiple matching might happen.
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "인청"), "인천광역시", sido) )

# also take out case for 북경
invt_for2 <- invt_prob %>% filter(str_detect(address, "북경시"))
invt_prob <- invt_prob %>% filter(!str_detect(address, "북경시"))

invt_for <- invt_for %>% bind_rows(invt_for2)

invt_for3 <- invt_prob %>% filter(str_detect(address, "북경조양구"))
invt_prob <- invt_prob %>% filter(!str_detect(address, "북경조양구"))

invt_for <- invt_for %>% bind_rows(invt_for3)


# problem also for invt_final: 화부산 in 강원도 강릉
invt_final <- invt_final %>% mutate(sido=ifelse(str_detect(address, "화부산"), "강원도", sido))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "화부산"), "강원도", sido))


# 인천광역시 계양구, 경기도 고양시 덕양구, 강원도 양구군(but no case for 강원도 양구군)
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "계양"), "인천광역시", sido), sigungu=ifelse(str_detect(address, "계양"), "계양구", sigungu))

invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "덕양"), "경기도", sido), sigungu=ifelse(str_detect(address, "덕양"), "고양시", sigungu))

# add this into invt_final
invt_check <- invt_prob %>% filter(sigungu=="계양구")
invt_check2 <- invt_prob %>% filter(sigungu=="고양시")

invt_final <- invt_final %>% bind_rows(invt_check)
invt_final <- invt_final %>% bind_rows(invt_check2)

invt_prob <- invt_prob %>% filter(is.na(sigungu) )


# some miscellaneous possible mistake due to no white space
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "청주시흥덕구"), "충청북도", sido), sigungu = ifelse(str_detect(address, "청주시흥덕구"), "청주시", sigungu))

invt_check3 <- invt_prob %>% filter(!is.na(sigungu))
invt_final <- invt_final %>% bind_rows(invt_check3)

invt_prob <- invt_prob %>% filter(is.na(sigungu))

# 내유동 -> 고양시
invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "내유동"), "경기도", sido), sigungu=ifelse(str_detect(address, "내유동"), "고양시", sigungu))

invt_check4 <- invt_prob %>% filter(!is.na(sigungu))
invt_final <- invt_final %>% bind_rows(invt_check4)

invt_prob <- invt_prob %>% filter(is.na(sigungu))

# do some manual filtering for 나주 due to states in US
invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "이나주|리나주|지나주|아나주|애나주|조나주|리나주|존나주|오나주|내나주|니나주"), 1, sigungu))

invt_check5 <- invt_prob %>% filter(!is.na(sigungu))
invt_for <- invt_for %>% bind_rows(invt_check5 %>% mutate(sigungu=NA))

invt_prob <- invt_prob %>% filter(is.na(sigungu))


# finally, filter for case: 중구
# take it out if it only has 중구
invt_prob <- invt_prob %>% filter(address!="중구")

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "미합중구"), 1, sigungu))

invt_check6 <- invt_prob %>% filter(!is.na(sigungu))
invt_for <- invt_for %>% bind_rows(invt_check6 %>% mutate(sigungu=NA))

invt_prob <- invt_prob %>% filter(is.na(sigungu))

# take out all case with 중구
invt_check7 <- invt_prob %>% filter(str_detect(address, "중구"))

invt_prob <- invt_prob %>% mutate(sigungu = ifelse(str_detect(address, "중구"), 1, sigungu))
invt_prob <- invt_prob %>% filter(is.na(sigungu))

invt_check7 <- invt_check7 %>% mutate(sido=c("인천광역시", "대전광역시", "인천광역시", "대구광역시", "경기도", "서울특별시", "울산광역시", "서울특별시"),
                                      sigungu=c("중구", "중구", "중구", "중구", "부천시", "중구", "중구", "중구"))

invt_final <- invt_final %>% bind_rows(invt_check7)


# finally, take care of some problem that make multiple matching in fuzzy join

check1 <- invt_prob %>% group_by(address) %>% count()

# invt_prob now has 159442 obs and it does not change after fuzzy join.
invt_prob1 <- invt_prob %>% fuzzy_left_join(region_data, by=c("address"="sigungu2"), match_fun = stringr::str_detect)

check2 <- invt_prob1 %>% group_by(address) %>% count()


check <- check1 %>% left_join(check2, by="address")

check <- check %>% filter(n.x!=n.y)

# we use this check to see some data with problems and fix them manually.

invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="강원도 원주 단구로", "강원도", sido),
                                  sigungu=ifelse(address=="강원도 원주 단구로", "원주시", sigungu))
invt_prob <- invt_prob %>% filter(address!="거제천로 번지")
invt_prob <- invt_prob %>% filter(address!="고성군")
invt_prob <- invt_prob %>% filter(address!="보은군산회")
invt_prob <- invt_prob %>% filter(address!="산청군산청")
invt_prob <- invt_prob %>% filter(address!="영등포구구로")
invt_prob <- invt_prob %>% filter(address!="전주 완주군")

invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="경기도 군포군 김포읍 감", "경기도", sido),
                                  sigungu=ifelse(address=="경기도 군포군 김포읍 감", "군포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="경기도 안양구", "경기도", sido),
                                  sigungu=ifelse(address=="경기도 안양구", "안양시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기도시흥군군포읍"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기도시흥군군포읍"), "군포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기도용인군"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기도용인군"), "용인시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기용인기흥서천"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기용인기흥서천"), "용인시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기도평택군"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기도평택군"), "평택시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기도포천군"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기도포천군"), "포천시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경기포천군"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "경기포천군"), "포천시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="경산남도 창원시 성산구", "경상남도", sido),
                                  sigungu=ifelse(address=="경산남도 창원시 성산구", "창원시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="경산창원시", "경상남도", sido),
                                  sigungu=ifelse(address=="경산창원시", "창원시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "경상남도 김해"), "경상남도", sido),
                                  sigungu=ifelse(str_detect(address, "경상남도 김해"), "김해시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="부산연제거제", "부산광역시", sido),
                                  sigungu=ifelse(address=="부산연제거제", "연제구", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "오산시 양산로"), "경기도", sido),
                                  sigungu=ifelse(str_detect(address, "오산시 양산로"), "오산시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="전남해남", "전라남도", sido),
                                  sigungu=ifelse(address=="전남해남", "해남군", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="전라남도 여수시 장군산길  경남아파트", "전라남도", sido),
                                  sigungu=ifelse(address=="전라남도 여수시 장군산길  경남아파트", "여수시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="전북정읍군산", "전라북도", sido),
                                  sigungu=ifelse(address=="전북정읍군산", "정읍시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="제주 서귀포", "제주특별자치도", sido),
                                  sigungu=ifelse(address=="제주 서귀포", "서귀포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="제주 특별자치도 서귀포", "제주특별자치도", sido),
                                  sigungu=ifelse(address=="제주 특별자치도 서귀포", "서귀포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="제주도 서귀포", "제주특별자치도", sido),
                                  sigungu=ifelse(address=="제주도 서귀포", "서귀포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="제주특별자치도 서귀포", "제주특별자치도", sido),
                                  sigungu=ifelse(address=="제주특별자치도 서귀포", "서귀포시", sigungu))
invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="제주특별자치도 서귀포서 대청로", "제주특별자치도", sido),
                                  sigungu=ifelse(address=="제주특별자치도 서귀포서 대청로", "서귀포시", sigungu))

invt_prob <- invt_prob %>% mutate(sido=ifelse(address=="충청도 청주시 흥덕구", "충청북도", sido),
                                  sigungu=ifelse(address=="충청도 청주시 흥덕구", "청주시", sigungu))

invt_check <- invt_prob %>% filter(!is.na(sigungu))
invt_final <- invt_final %>% bind_rows(invt_check)

invt_prob <- invt_prob %>% filter(is.na(sigungu))

###############

#155460
invt_prob <- invt_prob %>% fuzzy_left_join(region_data, by=c("address"="sigungu2"), match_fun = stringr::str_detect)


# first check some weird case where sido.x is NA
check <- invt_prob %>% filter(is.na(sido.x))


# there are some mistakes due to similar names

# # firstly, take of case in the address where 인천 is written as 인청
# invt_prob <- invt_prob %>% mutate(sido.x = ifelse(str_detect(address, "인청"), "인천광역시", sido.x) )

# # also take out case for 북경
# invt_for2 <- invt_prob %>% filter(str_detect(address, "북경시"))
# invt_prob <- invt_prob %>% filter(!str_detect(address, "북경시"))
# 
# invt_for <- invt_for %>% bind_rows(invt_for2)
# 
# invt_for3 <- invt_prob %>% filter(str_detect(address, "북경조양구"))
# invt_prob <- invt_prob %>% filter(!str_detect(address, "북경조양구"))
# 
# invt_for <- invt_for %>% bind_rows(invt_for3)


# also check case where sido.x sido.y does not match.
check <- invt_prob %>% filter(sido.x!=sido.y) %>% distinct(address, .keep_all = T)


# # problem also for invt_final: 화부산 in 강원도 강릉
# invt_final <- invt_final %>% mutate(sido=ifelse(str_detect(address, "화부산"), "강원도", sido))
# invt_prob <- invt_prob %>% mutate(sido.x=ifelse(str_detect(address, "화부산"), "강원도", sido.x), sido.y=ifelse(str_detect(address, "화부산"), "강원도", sido.y))
# 
# # 인천광역시 계양구, 경기도 고양시 덕양구, 강원도 양구군(but no case for 강원도 양구군)
# invt_prob <- invt_prob %>% mutate(sido.x = ifelse(str_detect(address, "계양"), "인천광역시", sido.x), sigungu.x=ifelse(str_detect(address, "계양"), "계양구", sigungu.x))
# 
# invt_prob <- invt_prob %>% mutate(sido.x = ifelse(str_detect(address, "덕양"), "경기도", sido.x), sigungu.x=ifelse(str_detect(address, "덕양"), "고양시", sigungu.x))


# sido check
invt_prob <- invt_prob %>% mutate(sido.x = case_when(
  str_detect(address, "서울특별시") ~ "서울특별시",
  str_detect(address, "부산광역시") ~ "부산광역시",
  str_detect(address, "대구광역시") ~ "대구광역시",
  str_detect(address, "인천광역시") ~ "인천광역시",
  str_detect(address, "광주광역시") ~ "광주광역시",
  str_detect(address, "대전광역시") ~ "대전광역시",
  str_detect(address, "울산광역시") ~ "울산광역시",
  str_detect(address, "강원도") ~ "강원도",
  str_detect(address, "경기도") ~ "경기도",
  str_detect(address, "충북|충청북도|충청북|충청북동|충청북둥|충청부도") ~ "충청북도",
  str_detect(address, "충남|충청남도|충청남|충청남동|충청남둥") ~ "충청남도",
  str_detect(address, "경북|경상북도|경상북|경상북동|경상북둥|경상복도") ~ "경상북도",
  str_detect(address, "경남|경상남도|경상남|경상남둥|경상남동|경상나도|걍상남도") ~ "경상남도",
  str_detect(address, "전북|전라북도|전라북|전라북둥|전라북동") ~ "전라북도",
  str_detect(address, "전남|전라남도|전라남|전라남동|전라남둥|전라암도") ~ "전라남도",
  str_detect(address, "제주|주특별자치도") ~ "제주특별자치도",
  TRUE ~ sido.x
))

invt_prob <- invt_prob %>% mutate(sido.y = case_when(
  str_detect(address, "서울특별시") ~ "서울특별시",
  str_detect(address, "부산광역시") ~ "부산광역시",
  str_detect(address, "대구광역시") ~ "대구광역시",
  str_detect(address, "인천광역시") ~ "인천광역시",
  str_detect(address, "광주광역시") ~ "광주광역시",
  str_detect(address, "대전광역시") ~ "대전광역시",
  str_detect(address, "울산광역시") ~ "울산광역시",
  str_detect(address, "강원도") ~ "강원도",
  str_detect(address, "경기도") ~ "경기도",
  str_detect(address, "충북|충청북도|충청북|충청북동|충청북둥|충청부도") ~ "충청북도",
  str_detect(address, "충남|충청남도|충청남|충청남동|충청남둥") ~ "충청남도",
  str_detect(address, "경북|경상북도|경상북|경상북동|경상북둥|경상복도") ~ "경상북도",
  str_detect(address, "경남|경상남도|경상남|경상남둥|경상남동|경상나도|걍상남도") ~ "경상남도",
  str_detect(address, "전북|전라북도|전라북|전라북둥|전라북동") ~ "전라북도",
  str_detect(address, "전남|전라남도|전라남|전라남동|전라남둥|전라암도") ~ "전라남도",
  str_detect(address, "제주|주특별자치도") ~ "제주특별자치도",
  TRUE ~ sido.y
))

# safe version of invt main data
# invt_final %>% write_rds("data/invt_refine_safe.rds") # 5889875 obs.
# invt_final %>% haven::write_dta("data/invt_refine_safe.dta")

# only keep data that has sido.x==sido.y
invt_prob <- invt_prob %>% filter(sido.x==sido.y)
# this gives 109489 obs.

# switch y columns to x
invt_prob <- invt_prob %>% mutate(sido=sido.x) %>% select(-sido.y)
invt_prob <- invt_prob %>% select(-sido.x)


invt_prob <- invt_prob %>% rename(sigungu=sigungu.x)
invt_prob <- invt_prob %>% mutate(sigungu=ifelse(is.na(sigungu), sigungu.y, sigungu))


# check if there is sth wrong
check <- invt_prob %>% filter(sigungu!=sigungu.y) %>% 
  filter(!is.na(sigungu))
# there not problem here.

# # fix some mistake.
# invt_prob <- invt_prob %>% mutate(sido = ifelse(str_detect(address, "계양"), "인천광역시", sido))
# 
# 159962 obs left.
# only leave the data we wil use
invt_prob <- invt_prob %>% filter(!is.na(sido) & !is.na(sigungu)) %>% 
  select(appnum, address, sido, sigungu)
# 61100 obs left.

# # some solve miscellaneous case: 서귀포시, 청주시흥덕구
# 
# invt_prob <- invt_prob %>% mutate(sido=ifelse(str_detect(address, "청주시흥덕구"), "충청북도", sido), sigungu = ifelse(str_detect(address, "청주시흥덕구"), "청주시", sigungu))



# add it into invt_final
invt_final <- invt_final %>% bind_rows(invt_prob)

# TOTAL DATA OBS: 5950975 (main) + about 1155717 (foreign) = 7106692 / out of 7180017 (this denominator excludes data obs that are impossible to identify due to NA) : 0.9898

# in domestic data obs: about 5950975 / out of 6025491: 0.9876

# take care of 경상북도 군위군 this was not solved in the before data set and was removed.
invt_new <- haven::read_dta("data/invt_loc.dta") %>% 
  select(appnum, address)

# values that can be identified as 군위군
invt_fix1 <- invt_new %>% filter(str_detect(address, "경상북도 군"))
invt_new <- invt_new %>% filter(!str_detect(address, "경상북도 군"))

# fix it
invt_fix1 <- invt_fix1 %>% mutate(sido="경상북도", sigungu="군위군")
# 352 obs

# it is not possible to get any more data for 군위군

invt_final <- invt_final %>% bind_rows(invt_fix1)


# save the data
invt_final %>% write_rds("data/invt_refine_main.rds", compression=9L, "xz") # 5951327 obs.
invt_final %>% haven::write_dta("data/invt_refine_main.dta")

invt_for <- invt_for %>% select(appnum, address)

invt_for %>% write_rds("data/invt_for_refine_main.rds", compression=9L, "xz") # 1155717 obs.
invt_for %>% haven::write_dta("data/invt_for_refine_main.dta")



