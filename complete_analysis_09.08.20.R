
library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(tigris)
library(tidyverse)
library(tidycensus)
library(stringr)
library(readxl)
library(viridis)
library(scales)
library(lubridate)
library(reshape2)
library(ggrepel)
library(patchwork)
library(ggforce)

# Set file path of parent directory
path_wd <- '/Users/nm/Desktop/projects/work/mansueto/cop analysis/data/'

# Obtain Census API Key here: https://api.census.gov/data/key_signup.html
#census_api_key('API_KEY', install = TRUE) 

readRenviron("~/.Renviron")

# Spatial GIS Files --------------------------------------------------------------

# Community Areas GeoJSON
community_areas <- 'https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON'
tmp_filepath <- paste0(tempdir(), '/', basename(community_areas))
download.file(url = paste0(community_areas), destfile = tmp_filepath)
community_geo <- sf::st_read(tmp_filepath) %>% st_as_sf() %>% select(community)

# State and Tract shapefiles
state_xwalk <- as.data.frame(fips_codes) %>%
  rename(state_fips = state_code,
         state_codes = state,
         county_name = county) %>%
  mutate(county_fips = paste0(state_fips,county_code))

state_fips <- unique(state_xwalk$state_fips)[1:51]
state_codes <- unique(state_xwalk$state_codes)[1:51]

# Chicago Tracts
chicago_tracts_url <- 'https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON'
tmp_filepath <- paste0(tempdir(), '/', basename(chicago_tracts_url))
download.file(url = paste0(chicago_tracts_url), destfile = tmp_filepath)
chicago_tracts <- sf::st_read(tmp_filepath) %>% st_as_sf() %>%
  mutate_at(vars(geoid10),list(as.character)) %>%
  mutate(geoid10 = str_pad(geoid10, width=11, side="left", pad="0")) %>%
  select(geoid10) %>%
  rename(geoid = geoid10)
  
# Data Sources ------------------------------------------------------------

# U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.
# DataMade. Analysis of 2019 Chicago Municipal Elections.
# City of Chicago, Office of Budget and Management. Analysis of 2020 Current Employee Names, Salaries, and Position Titles database and third party sources.       
# City of Chicago, Chicago Police Department. Analysis of 2019 crime and arrest database.
# Opportunity Insights. Analysis of data from Household Income and Incarceration for Children from Low-Income Households by Census Tract, Race, and Gender.
# Invisible Institute, Citizens Police Data Project. Analysis of complaints database 

# Chicago Data Portal Matched CPD Data --------------------------------------------------------

df_cops <- read_csv(paste0(path_wd,'cop_residential/cop_analysis_data.csv'))

df_cops_processed <- df_cops %>%
  select(community,race_coalesced,race_subethnicity,age_coalesced,cpd_role,
         number_of_cops,total_annual_salary,predictwise_racial_resentment_score,predictwise_guns_score) %>%
  mutate(total = 'Total')%>%
  mutate(race_subethnicity = case_when(is.na(race_subethnicity) & race_coalesced == 'White' ~ 'White', 
                                       is.na(race_subethnicity) & race_coalesced == 'Black' ~ 'African American', 
                                       is.na(race_subethnicity) & race_coalesced == 'Asian' ~ 'Other Asian', 
                                       is.na(race_subethnicity) & race_coalesced == 'Latino' ~ 'Other Latino', 
                                       is.na(race_subethnicity) & race_coalesced == 'Other' ~ 'Other', 
                                       TRUE ~ as.character(str_to_title(gsub("_", " ",race_subethnicity)))),
         age_coalesced = case_when(is.na(age_coalesced) ~ '40-49',
                                   age_coalesced <= 29 ~ '18-29',
                                   age_coalesced >= 30 & age_coalesced <= 39  ~ '30-39',
                                   age_coalesced >= 40 & age_coalesced <= 49  ~ '40-49',
                                   age_coalesced >= 50 & age_coalesced <= 59  ~ '50-59',
                                   age_coalesced >= 60 ~ '60+',
                                   TRUE ~ as.character(mean(age_coalesced, na.rm=TRUE)))) %>%
  melt(id.vars = c('community','number_of_cops','total_annual_salary','predictwise_racial_resentment_score','predictwise_guns_score'))  %>%
  group_by(community, variable, value) %>% 
  summarize_at(vars(number_of_cops,total_annual_salary,predictwise_racial_resentment_score,predictwise_guns_score), list(sum), na.rm=TRUE)  %>%
  ungroup() %>%
  mutate(predictwise_racial_resentment_score = predictwise_racial_resentment_score /number_of_cops,
         predictwise_guns_score =  predictwise_guns_score/number_of_cops) %>%
  group_by(community, variable) %>%
  mutate(number_of_cops_agg = sum(number_of_cops),
         total_annual_salary_agg =  sum(total_annual_salary)) %>%
  ungroup() %>%
  filter(variable %in% c('total','race_coalesced', 'age_coalesced')) %>%
  select(community, variable, value, number_of_cops, total_annual_salary) %>%
  pivot_wider(id_cols = community,
              names_from = c(variable,value), 
              values_from = c(number_of_cops, total_annual_salary), 
              values_fill=list(number_of_cops=0,total_annual_salary=0)) %>%
  rename_all(tolower) %>%
  mutate(community_label = str_to_title(community))

rm(df_cops)

# Invisible Institute Police Complaints Data -----------------------------------------------------

filedir <- paste0(path_wd,'complaints')
file_list <- list.files(path=filedir, pattern="*.xlsx", full.names = T) 
xlsx_files <- (fs::dir_ls(filedir, regexp = "\\.xlsx$")) %>% as.vector()
xlsx_files[1] %>% excel_sheets() 

# Complaints data
df_allegations = map_dfr(xlsx_files, read_excel, sheet = "Allegations", col_names =TRUE) %>% 
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% rename_all(list(tolower)) %>% 
  mutate(incidentdate = ymd_hms(incidentdate),
         startdate = ymd(startdate),
         enddate = ymd(enddate))
df_police_witnesses = map_dfr(xlsx_files, read_excel, sheet = "Police Witnesses", col_names =TRUE)  %>% select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% rename_all(list(tolower))
df_complaining_witnesses = map_dfr(xlsx_files, read_excel, sheet = "Complaining Witnesses", col_names =TRUE)  %>% select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% rename_all(list(tolower))
df_officer_profile = map_dfr(xlsx_files, read_excel, sheet = "Officer Profile", col_names =TRUE)  %>% select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% rename_all(list(tolower))
df_officer_profile <- df_officer_profile %>% 
  group_by(officerid,officerfirst,officerlast,gender,race,age) %>%
  mutate(complaints = 1) %>%
  summarize_at(vars(complaints), list(sum), na.rm=TRUE) %>%
  ungroup() %>%
  group_by(officerid) %>%
  mutate(recent = row_number(age))%>%
  ungroup()%>%
  filter(recent == 1) 

# Complaint Categories
personal_complaints <- c('Conduct Unbecoming (Off-Duty)','Drug / Alcohol Abuse','Domestic','Medical')
work_complaints <- c('False Arrest','Illegal Search','Lockup Procedures','Operation/Personnel Violations',
                     'Supervisory Responsibilities','Criminal Misconduct','Bribery / Official Corruption',
                     'First Amendment','Racial Profiling','Money / Property','Excessive Force','Unknown',
                     'Traffic','Use Of Force','Verbal Abuse')
domestic_abuse_misconduct <- c('Domestic','Conduct Unbecoming (Off-Duty)','Drug / Alcohol Abuse')
illegal_search_lockup_violations <- c('First Amendment','Illegal Search','Lockup Procedures')
general_violations <- c('Operation/Personnel Violations','Supervisory Responsibilities','Traffic',
                        'Unknown','Medical')
false_arrest_corruption_and_abuse <- c('Criminal Misconduct','False Arrest','Verbal Abuse',
                                       'Racial Profiling','Bribery / Official Corruption','Money / Property')
use_of_force <- c('Use Of Force','Excessive Force')

# Process Complaints Data
df_complaints <- st_as_sf(df_allegations %>% filter(!is.na(longitude)), 
                    coords = c("longitude", "latitude"), 
                    crs = 4326, agr = "constant") %>%
  select(crid,officerid,allegationcode,category,
         allegation,recommendedfinding,
         recommendedoutcome,finalfinding,
         finaloutcome,finding,
         outcome,beat,incidentdate) %>%
  left_join(x=.,y=df_officer_profile, by=c('officerid'='officerid')) %>%
  mutate(bigcategory = case_when(category %in% personal_complaints ~ 'Off Duty',
                                 category %in% work_complaints ~ 'Complaints',
                                 TRUE ~ as.character('Complaints')),
         midcategory = case_when(category %in% domestic_abuse_misconduct ~ 'Domestic abuse & conduct unbecoming',
                                 category %in% illegal_search_lockup_violations ~ 'Illegal search & lockup violations',
                                 category %in% general_violations ~ 'Operation violations',
                                 category %in% false_arrest_corruption_and_abuse ~ 'False arrest, corruption, & abuse',
                                 category %in% use_of_force ~ 'Use of force',
                                 TRUE ~ as.character('Operation violations')),
         race = case_when(race == 'Asian/Pacific' ~ 'Asian',
                          race == 'Hispanic' ~ 'Latino',
                          race == 'Native American/Alaskan Native' ~ 'Other',
                          race == 'Unknown' ~ 'Other',
                          TRUE ~ as.character(race)),
         complaint_count = 1,
         year = year(incidentdate),
         time_range = ifelse(year >= 2008,'2008-2018','1971-2007')) 

df_complaints_processed <- df_complaints %>%
  st_join(x = .,y = community_geo) %>% #
  st_as_sf() %>%
  st_transform(crs = st_crs(4326)) %>%
  filter(!is.na(community), !is.na(year)) 

# Check for dupes
df_complaints_processed  %>% 
  group_by(crid, officerid) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

# Complaints by Community-Race Groupings
df_complaints_processed <- df_complaints_processed %>% st_drop_geometry() %>%  
  select(community,time_range,bigcategory,midcategory,category,race,complaint_count) %>%
  melt(id.vars = c('community','time_range','race','complaint_count'))  %>%
  group_by(community, time_range, race, variable, value) %>% 
  summarize_at(vars(complaint_count), list(sum), na.rm=TRUE)  %>%
  ungroup() %>%
  pivot_wider(id_cols = c(community,time_range, variable, value),
              names_from = c(race), 
              values_from = c(complaint_count), 
              values_fill=list(complaint_count=0)) %>%
  mutate(total_cop_complaints = White + Black + Asian + Latino + Other,
         share_white_cop_complaints = White / total_cop_complaints) %>% 
  rename_all(list(tolower)) %>% 
  rename(white_cop_complaints = white,
         black_cop_complaints = black,
         latino_cop_complaints = latino,
         asian_cop_complaints = asian,
         other_cop_complaints = other)

df_total_complaints_processed <- df_complaints_processed %>% filter(time_range == '2008-2018', value == 'Complaints')

# Remove intermediary data
rm(df_allegations, df_complaining_witnesses, df_officer_profile, df_police_witnesses, df_complaints, df_complaints_processed)

# ACS Data ----------------------------------------------------------------

# Census Planning Database at Tract level
data_url <- 'https://www2.census.gov/adrm/PDB/2020/pdb2020trv2_us.zip'
download.file(url = paste0(data_url), destfile = paste(tempdir(), basename(data_url), sep = "/")) 
df_acs_raw <- read.csv(unzip(zipfile = paste(tempdir(), basename(data_url), sep = "/")))

df_acs_raw <- df_acs_raw %>% 
  rename_all(tolower) %>%
  mutate_at(vars(gidtr, state),list(as.character)) %>%
  mutate(gidtr = str_pad(gidtr, width=11, side="left", pad="0"),
         state = str_pad(state, width=2, side="left", pad="0"),
         county = str_pad(county, width=3, side="left", pad="0"),
         county_fips = str_sub(gidtr, 1, 5)) %>%
  filter(state %in% state_fips) %>% 
  filter(!is.na(tot_population_acs_14_18))

df_acs_raw %>% 
  group_by(gidtr) %>% 
  mutate(dupes = n()) %>%
  ungroup() %>%
  filter(dupes >= 2)

df_acs_raw <- inner_join(df_acs_raw, chicago_tracts, by = c('gidtr'='geoid') ) %>% 
  st_as_sf() %>% 
  st_join(., community_geo, left= TRUE, largest = TRUE) %>%
  st_drop_geometry() %>% 
  as.data.frame() 

vars_sum <- c('nh_white_alone_acs_14_18','nh_blk_alone_acs_14_18','nh_aian_alone_acs_14_18','nh_asian_alone_acs_14_18','hispanic_acs_14_18',
              'college_acs_14_18','prs_blw_pov_lev_acs_14_18','children_in_pov_acs_14_18','female_no_hb_acs_14_18','tot_vacant_units_acs_14_18',
              'owner_occp_hu_acs_14_18','single_unit_acs_14_18','recent_built_hu_acs_14_18','aggr_house_value_acs_14_18','aggregate_hh_inc_acs_14_18',
              'tot_population_acs_14_18','pop_25yrs_over_acs_14_18','pov_univ_acs_14_18',
              'children_povdet_acs_14_18','tot_occp_units_acs_14_18','tot_housing_units_acs_14_18' )

df_acs_raw_sum <- df_acs_raw %>%
  mutate_at(vars('aggr_house_value_acs_14_18','aggregate_hh_inc_acs_14_18'),funs(as.numeric(gsub('\\$|,', '', .)))  ) %>%
  mutate_at(vars(vars_sum),list(as.numeric), na.rm = TRUE) %>%
  group_by(community) %>%
  summarise_at(vars(vars_sum), list(sum)) %>%
  ungroup() 

df_acs_processed <- df_acs_raw_sum %>%
  mutate(pct_white = nh_white_alone_acs_14_18/tot_population_acs_14_18,
         pct_black = nh_blk_alone_acs_14_18/tot_population_acs_14_18,
         pct_aian = nh_aian_alone_acs_14_18/tot_population_acs_14_18,
         pct_asian = nh_asian_alone_acs_14_18/tot_population_acs_14_18,
         pct_latino = hispanic_acs_14_18/tot_population_acs_14_18,
         pct_college = college_acs_14_18/pop_25yrs_over_acs_14_18,
         pct_blw_pov = prs_blw_pov_lev_acs_14_18 /pov_univ_acs_14_18,
         pct_children_blw_pov = children_in_pov_acs_14_18/children_povdet_acs_14_18,
         pct_female_no_hb = female_no_hb_acs_14_18/tot_occp_units_acs_14_18,
         pct_vacant = tot_vacant_units_acs_14_18/tot_housing_units_acs_14_18,
         pct_owner_occ = owner_occp_hu_acs_14_18/tot_occp_units_acs_14_18,
         pct_single_detached = single_unit_acs_14_18/tot_housing_units_acs_14_18,
         pct_recent_built = recent_built_hu_acs_14_18/tot_housing_units_acs_14_18,
         avg_hh_inc_acs_14_18 = aggregate_hh_inc_acs_14_18/tot_occp_units_acs_14_18, 
         avg_house_value_acs_14_18 = aggr_house_value_acs_14_18/tot_housing_units_acs_14_18) %>%
  select(community, tot_population_acs_14_18, tot_occp_units_acs_14_18, tot_housing_units_acs_14_18,
         pct_white,pct_black,pct_aian,pct_asian,pct_latino,
         pct_college,pct_blw_pov,pct_children_blw_pov,pct_female_no_hb,pct_vacant,
         pct_owner_occ,pct_single_detached,pct_recent_built,
         avg_hh_inc_acs_14_18, avg_house_value_acs_14_18 )

vars_median <- c('med_hhd_inc_acs_14_18','med_house_value_acs_14_18')

df_acs_raw_median <- df_acs_raw %>%
  mutate_at(vars(vars_median),funs(as.numeric(gsub('\\$|,', '', .)))  ) %>%
  mutate(med_house_value_acs_14_18_weighted = tot_occp_units_acs_14_18*med_house_value_acs_14_18,
         med_hhd_inc_acs_14_18_weighted = tot_housing_units_acs_14_18*med_hhd_inc_acs_14_18) %>%
  #select(community,med_house_value_acs_14_18, med_hhd_inc_acs_14_18)
  group_by(community) %>%
  summarise_at(vars(med_house_value_acs_14_18,med_hhd_inc_acs_14_18,
                    med_house_value_acs_14_18_weighted,med_hhd_inc_acs_14_18_weighted,
                    tot_occp_units_acs_14_18,tot_housing_units_acs_14_18), list(sum), na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(med_house_value_acs_14_18 = (med_house_value_acs_14_18_weighted/tot_occp_units_acs_14_18),
         med_hhd_inc_acs_14_18 = (med_hhd_inc_acs_14_18_weighted/tot_housing_units_acs_14_18)) %>%
  select(community, med_house_value_acs_14_18,med_hhd_inc_acs_14_18)

df_acs_processed = left_join(df_acs_processed, df_acs_raw_median, by = c('community'='community'))

rm(df_acs_raw_median, df_acs_raw_sum)

# Mayoral Vote ------------------------------------------------------------

url <- 'https://raw.githubusercontent.com/datamade/chicago-municipal-elections/master/data/municipal_general_2019.geojson'
tmp_filepath <- paste0(tempdir(), '/', basename(url))
download.file(url = paste0(url), destfile = tmp_filepath)
precincts <- sf::st_read(tmp_filepath) %>% st_as_sf()
precincts <- precincts %>%
  select(precinct, ward)  

df_mayor_vote_processed <- precincts %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  rename_all(list(tolower)) %>%
  mutate_at(vars(jerry_joyce:sophia_king), ~replace_na(., 0)) %>%
  select(ward, precinct,lori_lightfoot,jerry_joyce,ballots_cast)  %>%
  st_join(x = .,y = community_geo , largest = TRUE) %>% 
  st_drop_geometry() %>%
  mutate_at(vars(precinct, ward),list(as.integer)) %>%
  mutate_if(is.factor, as.character)  %>%
  group_by(community) %>% 
  summarize_at(vars(lori_lightfoot,jerry_joyce,ballots_cast), list(sum), na.rm=TRUE) %>%
  ungroup() %>%
  mutate(lightfoot_share= lori_lightfoot/ballots_cast,
         jerry_joyce_share = jerry_joyce/ballots_cast) 

rm(precincts)

# Crime and Incarceration -------------------------------------------------

url <- 'https://opportunityinsights.org/wp-content/uploads/2018/10/tract_outcomes_simple.csv'
tmp_filepath <- paste0(tempdir(), '/', basename(url))
download.file(url = paste0(url), destfile = tmp_filepath)
incarceration_raw <- read_csv(tmp_filepath)

df_incarceration <- incarceration_raw %>%
  mutate_at(vars(state, county, tract),list(as.character)) %>%
  mutate(tract = str_pad(tract, width=6, side="left", pad="0"),
         state = str_pad(state, width=2, side="left", pad="0"),
         county = str_pad(county, width=3, side="left", pad="0"),
         tract_full = paste0(state,county,tract)) %>%
  select(state,county,tract ,tract_full,
         kfr_pooled_pooled_p25,jail_pooled_pooled_p25,
         pooled_pooled_count,black_male_count,
         kfr_black_pooled_p25,jail_black_pooled_p25,
         kfr_black_male_p25,jail_black_male_p25) %>%
  filter(state == '17', county == '031') 

df_incarceration_processed <- left_join(df_acs_raw,
                                        df_incarceration, by = c('gidtr'='tract_full') ) %>%
  mutate(kfr_pooled_pooled_p25_weighted = kfr_pooled_pooled_p25*pooled_pooled_count,
         jail_pooled_pooled_p25_weighted = jail_pooled_pooled_p25*pooled_pooled_count) %>%
  group_by(community) %>%
  summarize_at(vars(kfr_pooled_pooled_p25_weighted,jail_pooled_pooled_p25_weighted,pooled_pooled_count), list(sum), na.rm=TRUE) %>%
  ungroup() %>%
  mutate(kfr_pooled_pooled_p25 = kfr_pooled_pooled_p25_weighted/pooled_pooled_count,
         jail_pooled_pooled_p25 = jail_pooled_pooled_p25_weighted/pooled_pooled_count) 

rm(df_incarceration, incarceration_raw)

# Crimes and Arrests ------------------------------------------------------

url <- 'https://data.cityofchicago.org/api/views/w98m-zvie/rows.csv?accessType=DOWNLOAD'
tmp_filepath <- paste0(tempdir(), '/', basename(url))
download.file(url = paste0(url), destfile = tmp_filepath)
crime_raw <- read_csv(tmp_filepath)

df_crime_processed <- crime_raw %>% 
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% 
  rename_all(list(tolower)) %>% 
  filter(!is.na(longitude)) %>%
  mutate(crime_count = 1,
         arrest_count = ifelse(arrest == TRUE,1,0)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_join(x = .,y = community_geo) %>% #
  st_drop_geometry() %>%
  filter(!is.na(community)) %>%
  group_by(community) %>%
  summarize_at(vars(crime_count,arrest_count), list(sum), na.rm=TRUE) %>%
  ungroup() %>%
  mutate(arrest_rate = arrest_count/crime_count)

rm(crime_raw)

# Consolidated analysis dataframe -----------------------------------------

df_analysis <- left_join(community_geo, df_cops_processed, by = c('community'='community')) %>%
  left_join(., df_acs_processed, by = c('community'='community')) %>%
  left_join(., df_total_complaints_processed, by = c('community'='community')) %>%
  left_join(., df_mayor_vote_processed, by = c('community'='community')) %>%
  left_join(., df_crime_processed, by = c('community'='community')) %>%
  left_join(., df_incarceration_processed, by = c('community'='community')) %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf()

df_analysis <- df_analysis %>%
  mutate(cops_per_household = number_of_cops_total_total / (tot_occp_units_acs_14_18) ,
         cops_income_per_household = total_annual_salary_total_total / (tot_occp_units_acs_14_18),
         average_cop_income = total_annual_salary_total_total/number_of_cops_total_total) %>%
  mutate(share_of_cops = number_of_cops_total_total / 13851,
         share_of_pop = tot_population_acs_14_18 / sum(tot_population_acs_14_18),
         ratio_cop_share_to_pop_share = share_of_cops /  share_of_pop) %>%
  mutate(pct_white_cops = number_of_cops_race_coalesced_white/number_of_cops_total_total,
         pct_black_cops = number_of_cops_race_coalesced_black/number_of_cops_total_total,
         pct_latino_cops = number_of_cops_race_coalesced_latino/number_of_cops_total_total,
         pct_asian_cops = number_of_cops_race_coalesced_asian/number_of_cops_total_total,
         pct_poc_cops = (number_of_cops_race_coalesced_black+number_of_cops_race_coalesced_latino+number_of_cops_race_coalesced_asian)/number_of_cops_total_total ) 

# Crop out O'Hare
chi_bbox <- st_bbox(df_analysis) 
chi_bbox_crop <- st_bbox(c(xmin = -87.862226, 
                           xmax = chi_bbox[[3]], 
                           ymax = chi_bbox[[4]], 
                           ymin = chi_bbox[[2]]), crs = st_crs(4326))
df_analysis <- st_crop(df_analysis, y = chi_bbox_crop) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 

df_analysis <- df_analysis %>%
  mutate(race_label = case_when((pct_white <= .6 & pct_white >= .2) & (pct_latino <= .6 & pct_latino >= .2) & (pct_black <= .6 & pct_black >= .2) ~ 'White, Black & Latino',
                                (pct_white <= .6 & pct_white >= .2) & (pct_asian <= .6 & pct_asian >= .2) ~ 'Asian & white',
                                (pct_white <= .6 & pct_white >= .2) & (pct_black <= .6 & pct_black >= .2) ~ 'Black & white',
                                (pct_white <= .6 & pct_white >= .2) & (pct_latino <= .6 & pct_latino >= .2) ~ 'Latino & white',
                                (pct_black <= .6 & pct_black >= .2) & (pct_latino <= .6 & pct_latino >= .2) ~ 'Black & Latino',
                                #(pct_asian <= .6 & pct_asian >= .2) & (pct_latino <= .6 & pct_latino >= .2) ~ 'Asian & Latino',
                                #(pct_asian <= .6 & pct_asian >= .2) & (pct_black <= .6 & pct_black >= .2) ~ 'Asian & Black',
                                pct_black >= .5 ~ 'Black',
                                pct_latino >= .5 ~ 'Latino',
                                pct_asian >= .5 ~ 'Asian',
                                pct_white >= .5 ~ 'White',
                                TRUE ~ as.character('Other')),
         income_label = case_when(med_hhd_inc_acs_14_18 <= 50000 ~ '$20-50K',
                                  med_hhd_inc_acs_14_18 > 50000 & med_hhd_inc_acs_14_18 <= 75000 ~ '$50-75K',
                                  med_hhd_inc_acs_14_18 > 75000 ~ '$75-120K',
                                  TRUE ~ as.character('Other'))) %>%
  mutate(race_income_label = paste0(race_label,' (',income_label,')'),
         cop_neighborhood = case_when(ratio_cop_share_to_pop_share >= 2 & number_of_cops_total_total >= 200 ~ 'Cop neighborhood',
                                      TRUE ~ as.character('')))

race_income_levels <- c("White, Black & Latino ($20-50K)","White ($50-75K)","White ($75-120K)","Black & white ($50-75K)","Black & white ($75-120K)","Black ($20-50K)","Black ($50-75K)","Black & Latino ($20-50K)","Black & Latino ($50-75K)","Latino ($20-50K)","Latino ($50-75K)","Latino & white ($50-75K)","Latino & white ($75-120K)","Asian ($20-50K)","Asian & white ($50-75K)","Asian & white ($75-120K)")

df_analysis$race_income_label<- factor(df_analysis$race_income_label, levels = race_income_levels)

# Final Plots -------------------------------------------------------------

map_ggplot <- function(data, fill_var, scale_label = percent,
                       lab_t = '', lab_s = '', lab_c = '') {
  plot <- ggplot(data) +
    geom_sf( aes_string(fill = fill_var), color = 'white', size = .55) +
    scale_fill_viridis("", option = "magma", direction = -1, labels = scale_label) +
    labs(title = lab_t,subtitle = lab_s,caption = lab_c) +
    coord_sf(clip = "on") + theme_minimal() +  
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing.x=unit(0, "lines"),
          panel.spacing.y=unit(0, "lines"),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA),
          #plot.title = element_text(face = 'bold',size=10),
          plot.title = element_blank(),
          legend.text = element_text(size= 9),
          plot.subtitle=element_text(size=9, hjust = .5, face = 'bold'),
          plot.caption=element_text(size=9, hjust = 0, face = 'italic'),
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.margin=unit(c(t=3,r=0,b=-10,l=0), "mm"),
          panel.border = element_blank())  
  print(plot)
  return(plot)
}

names(df_analysis)

# White demographics
(p_pct_white <- map_ggplot(data = df_analysis, fill_var = "pct_white", 
                           scale_label =  label_percent(accuracy = 1),
                           lab_t = '', lab_s = '% population\nwho are white', lab_c = ''))

(p_college_ba <- map_ggplot(data = df_analysis, fill_var = "pct_college", 
                            scale_label =  label_percent(accuracy = 1),
                            lab_t = '', lab_s = "% of population aged 25+\nwith a Bachelor's", lab_c = ''))

(p_recent_built <- map_ggplot(data = df_analysis, fill_var = "pct_recent_built", 
                              scale_label =  label_percent(accuracy = 1),
                              lab_t = '', lab_s = '% of housing units built\nbetween 2010-2018', lab_c = ''))

(p_med_house_value <- map_ggplot(data = df_analysis, fill_var = "med_house_value_acs_14_18/1000", 
                                 scale_label = dollar_format(suffix = 'K'),
                                 lab_t = '', lab_s = 'Median housing value\n(thousands)', lab_c = ''))

(p_med_hhd_inc <- map_ggplot(data = df_analysis, fill_var = "med_hhd_inc_acs_14_18/1000", 
                             scale_label =  dollar_format(suffix='K'),
                             lab_t = '', lab_s = 'Median household income\n(thousands)', lab_c = ''))

(p_lightfoot_general <- map_ggplot(data = df_analysis, fill_var = "lightfoot_share", 
                              scale_label =  label_percent(accuracy = 1),
                              lab_t = '', lab_s = '% of 2019 general election\nvote for Lori Lightfoot', lab_c = ''))

(p_joyce_general <- map_ggplot(data = df_analysis, fill_var = "jerry_joyce_share", 
                                   scale_label =  label_percent(accuracy = 1),
                                   lab_t = '', lab_s = '% of 2019 general election\nvote for Jerry Joyce', lab_c = ''))

# Cop Neighborhood Demos

(p_owner_occupied <- map_ggplot(data = df_analysis, fill_var = "pct_owner_occ", 
                                scale_label =  label_percent(accuracy = 1),
                                lab_t = '', lab_s = '% of housing units that\nare owner occupied', lab_c = ''))

(p_single_detached <- map_ggplot(data = df_analysis, fill_var = "pct_single_detached ", 
                                 scale_label =  label_percent(accuracy = 1),
                                 lab_t = '', lab_s = '% of housing units that\nare single-detached', lab_c = ''))

# Where cops live
(p_number_of_cops <- map_ggplot(data = df_analysis, fill_var = "number_of_cops_total_total",
                                scale_label = comma,
                                lab_t = '', lab_s = 'Number of residents\nemployed by the CPD', lab_c = ''))

(p_total_budget <- map_ggplot(data = df_analysis, fill_var = "total_annual_salary_total_total/1000000", 
                              scale_label = dollar_format(suffix='M'), lab_t = '', lab_s = 'Total income from the CPD\n(millions), 2020', lab_c = ''))

(p_cops_income_per_hh <- map_ggplot(data = df_analysis, fill_var = "cops_income_per_household/1000",                       
                                    scale_label = dollar_format(suffix='K'),
                                    lab_t = '', lab_s = 'CPD income per household\n(thousands)', lab_c = ''))

(p_average_cop_income <- map_ggplot(data = df_analysis, fill_var = "average_cop_income/1000",                       
                                    scale_label = dollar_format(suffix='K'),
                                    lab_t = '', lab_s = 'Average cop income\n(thousands)', lab_c = ''))

(p_cop_income_indexed_to_median <- map_ggplot(data = df_analysis, fill_var = "average_cop_income/med_hhd_inc_acs_14_18", 
                                              scale_label =  label_comma(accuracy = 1, suffix = 'x'),
                                              lab_t = '', lab_s = 'Average cop income relative to\nmedian household income', lab_c = ''))

(p_cop_lq <- map_ggplot(data = df_analysis, fill_var = "ratio_cop_share_to_pop_share",
                        scale_label = label_comma(accuracy = 1, suffix = 'x'), lab_t = '', lab_s = 'Concentration of residents\nemployed by the CPD\n(in multiples of population share)', lab_c = ''))

(p_cop_white <- map_ggplot(data = df_analysis, fill_var = "pct_white_cops" ,
                        scale_label = label_percent(accuracy = 1), lab_t = '', lab_s = '% of cop residents\nwho are white', lab_c = ''))

(p_cop_black <- map_ggplot(data = df_analysis, fill_var = "pct_black_cops" ,
                           scale_label = label_percent(accuracy = 1), lab_t = '', lab_s = '% of cop residents\nwho are Black', lab_c = ''))


(p_cop_latino <- map_ggplot(data = df_analysis, fill_var = "pct_latino_cops" ,
                           scale_label = label_percent(accuracy = 1), lab_t = '', lab_s = '% of cop residents\nwho are Latino', lab_c = ''))

(p_cop_poc <- map_ggplot(data = df_analysis, fill_var = "pct_poc_cops" ,
                            scale_label = label_percent(accuracy = 1), lab_t = '', lab_s = '% of cop residents\nwho are people of color', lab_c = ''))

# Complaints
(p_cop_complaints <- map_ggplot(data = df_analysis, fill_var = "total_cop_complaints",                       
                                scale_label = label_comma(accuracy =1),
                                lab_t = '', lab_s = 'Number of complaints\n against the CPD (2008-2018)', lab_c = ''))

(p_white_cop_complaints <- map_ggplot(data = df_analysis, fill_var = "share_white_cop_complaints", 
                                      scale_label = label_percent(accuracy = 1), lab_t = '', lab_s = '', lab_c = ''))

(p_pct_black<- map_ggplot(data = df_analysis, fill_var = "pct_black",                       
                          scale_label = label_percent(accuracy = 1),
                          lab_t = '', lab_s = '% population\nwho are Black', lab_c = ''))

(p_pct_latino<- map_ggplot(data = df_analysis, fill_var = "pct_latino",                       
                          scale_label = label_percent(accuracy = 1),
                          lab_t = '', lab_s = '% population\nwho are Latino', lab_c = ''))

(p_pct_blw_pov <- map_ggplot(data = df_analysis, fill_var = "pct_blw_pov ",                       
                             scale_label = label_percent(accuracy = 1),
                             lab_t = '', lab_s = "% of population below\nthe poverty line", lab_c = ''))

(p_pct_children_blw_pov <- map_ggplot(data = df_analysis, fill_var = "pct_children_blw_pov ",                       
                                      scale_label = label_percent(accuracy = 1),
                                      lab_t = '', lab_s = '', lab_c = ''))

(p_pct_female_no_hb <- map_ggplot(data = df_analysis, fill_var = "pct_female_no_hb ",                       
                                  scale_label = label_percent(accuracy = 1),
                                  lab_t = '', lab_s = '% of single female households', lab_c = ''))

(p_pct_vacant <- map_ggplot(data = df_analysis, fill_var = "pct_vacant ",                       
                            scale_label = label_percent(accuracy = 1),
                            lab_t = '', lab_s = '% of housing units\nthat are vacant', lab_c = ''))

(p_arrest_rate <- map_ggplot(data = df_analysis, fill_var = "arrest_rate",                       
                            scale_label = label_percent(accuracy = 1),
                            lab_t = '', lab_s = '% of crimes that lead to arrests', lab_c = ''))

(p_incarceration <- map_ggplot(data = df_analysis, fill_var = "jail_pooled_pooled_p25",                       
                             scale_label = label_percent(accuracy = 1),
                             lab_t = '', lab_s = '% of children in the bottom 25th percentile\nof income born in 1978-83\nwho were incarcerated in 2010', lab_c = ''))

## Findings

# Chicago's political and economic elite concentrate on the North Side who are higher income, own more wealth
# in terms of housing, recieve more education, live in zones with more new construction, and represent the Mayor's primary base of support   

# Cops disproportionately favor living in white suburb-like enclaves which are distinct from white North Side neighborhoods 
# They are more middle-class, generally have lower education and income, but high home ownership rates.
# These neighborhoods also have a different political orientation and a more suburban residential profile. 

# Black cops live in Black communities, white cops live in white communities, Latino cops live in Latino communities.
# The CPD provides some of the best paying jobs to Black Chicagoans 
# although the vast majority of CPD income flows to predominantly white communities and white cops generally make more on average    

# In spite of the relatively higher pay of CPD jobs, the Black community faces the most significant economic challenges
# and incur more abuse from the CPD, experience higher incarceration rates, and crimes are more likely to lead to arrests

panel1 <- p_number_of_cops +  p_total_budget + p_cop_lq  + p_joyce_general  + p_owner_occupied + p_single_detached +
  plot_annotation(tag_levels = c('1'),
                  tag_prefix = '  A',
                  tag_suffix = ' ',
                  title = '',
                  subtitle = '',
                  caption = "A1,A2,A3: City of Chicago, Office of Budget and Management. Analysis of 2020 Current Employee Names, Salaries, and Position Titles database and\nthird party sources.*\nA4: DataMade. Analysis of 2019 Chicago Municipal Elections.\nA5,A6: U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.\n\n*CPD residential statistics reflect 11,876 of the total 13,851 CPD police force, accounting for $1,062,154,410 of the $1,228,222,428 CPD budget.") & 
  theme(plot.tag = element_text(size = 8, vjust=13),
        plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.tag.position = "bottom") 

panel2 <- p_cop_white + p_cop_poc + p_average_cop_income + p_cop_income_indexed_to_median +  
  plot_annotation(tag_levels = c('1'),
                  tag_prefix = '  B',
                  tag_suffix = ' ',
                  title = '',
                  subtitle = '',
                  caption = "B1,B2,B3,B4: City of Chicago, Office of Budget and Management. Analysis of 2020\nCurrent Employee Names, Salaries, and Position Titles database and third party sources.*\n\n*CPD residential statistics reflect 11,876 of the total 13,851 CPD police force, accounting for\n$1,062,154,410 of the $1,228,222,428 CPD budget.") & 
  theme(plot.tag = element_text(size = 8, vjust=13),
        plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.tag.position = "bottom") 

panel3 <- p_pct_white + p_pct_black + p_pct_latino + p_cop_white + p_cop_black + p_cop_latino +  
  plot_annotation(tag_levels = c('1'),
                  tag_prefix = '  C',
                  tag_suffix = ' ',
                  title = '',
                  subtitle = '',
                  caption = "C1,C2,C3: U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.\nC4,C5,C6: City of Chicago, Office of Budget and Management. Analysis of 2020 Current Employee Names, Salaries, and Position Titles database and\nthird party sources.*\n\n*CPD residential statistics reflect 11,876 of the total 13,851 CPD police force, accounting for $1,062,154,410 of the $1,228,222,428 CPD budget.") & 
  theme(plot.tag = element_text(size = 8, vjust=13),
        plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.tag.position = "bottom") 

panel4 <- p_pct_black + p_pct_blw_pov + p_pct_vacant +  p_cop_complaints + p_arrest_rate + p_incarceration +
  plot_annotation(tag_levels = c('1'),
                  tag_prefix = '  D',
                  tag_suffix = ' ',
                  title = '',
                  subtitle = '',
                  caption = 'D1,D2,D3: U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.\nD4: Invisible Institute. Analysis of Citizens Police Data Project complaintsdatabase.*\nD5: City of Chicago, Chicago Police Department. Analysis of 2019 crime and arrest database.\nD6: Opportunity Insights. Analysis of data from Household Income and Incarceration for Children from Low-Income Households.\n\n*Excludes complaints in the following categories: conduct unbecoming, drug / alcohol abuse, domestic, medical.') & 
  theme(plot.tag = element_text(size = 8, vjust=13),
        plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.tag.position = "bottom") 

panel5 <- p_pct_white + p_college_ba + p_lightfoot_general + p_med_house_value + p_med_hhd_inc + p_recent_built +
  plot_annotation(tag_levels = c('1'),
                  tag_prefix = '  E',
                  tag_suffix = ' ',
                  title = '',
                  subtitle = '',
                  caption = 'E1,E2,E4,E5,E6: U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.\nA3: DataMade. Analysis of 2019 Chicago Municipal Elections.') & 
  theme(plot.tag = element_text(size = 8, vjust=13),
        plot.caption = element_text(hjust = 0, face= "italic"), 
        plot.tag.position = "bottom") 

ggsave(paste0(path_wd,'panel1.png'), panel1, dpi = 400, height =8.5, width=11)
ggsave(paste0(path_wd,'panel2.png'), panel2, dpi = 400, height =8, width=7)        
ggsave(paste0(path_wd,'panel3.png'), panel3, dpi = 400, height =8.5, width=11)                
ggsave(paste0(path_wd,'panel4.png'), panel4, dpi = 400, height =8.5, width=11)
ggsave(paste0(path_wd,'panel5.png'), panel5, dpi = 400, height =8.5, width=11)

write_csv(df_analysis %>% st_drop_geometry(), paste0(path_wd,'police_analysis.csv'))

names(df_analysis)

df_analysis <- df_analysis %>%
  mutate(community_label = gsub("North ","N. ", community_label ),
         community_label = gsub("South ", "S. ",community_label ),
         community_label = gsub("West ", "W. ", community_label ),
         community_label = gsub("East ", "E. ", community_label ))

df_outlines_all <- df_analysis %>%
  select(race_income_label) %>%
  group_by(race_income_label) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 

df_outlines_cops <- df_analysis %>%
  filter(cop_neighborhood != '') %>%
  select(cop_neighborhood) %>%
  group_by(cop_neighborhood) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

df_analysis <- df_analysis %>%
  mutate(lat_mod = case_when(community == 'WEST ENGLEWOOD' ~ .$lat - .01, # South
                             community == 'GREATER GRAND CROSSING' ~ .$lat - .0055,
                             community == 'JEFFERSON PARK' ~ .$lat - .007,
                             community == 'ALBANY PARK' ~ .$lat - .002,
                             community == 'BEVERLY' ~ .$lat - .01,
                             community == 'BRIGHTON PARK' ~ .$lat - .007,
                             community == 'BRIDGEPORT' ~ .$lat - .009,
                             community == 'WASHINGTON PARK' ~ .$lat - .004,
                             community == 'AVALON PARK' ~ .$lat - .004,
                             community == 'MCKINLEY PARK' ~ .$lat - .002,
                             community == 'WEST ELSDON' ~ .$lat - .004,
                             community == 'GARFIELD RIDGE'~ .$lat - .01,
                             community == 'ROSELAND' ~ .$lat - .015,
                             community == 'HEGEWISCH' ~ .$lat - .015,
                             community == 'RIVERDALE' ~ .$lat - .01,
                             community == 'FULLER PARK'~ .$lat - .008,
                             community == 'KENWOOD' ~ .$lat - .004,
                             community == 'ENGLEWOOD' ~ .$lat + .003, # North
                             community == 'NORTH LAWNDALE' ~ .$lat + .004,
                             community == 'WOODLAWN' ~ .$lat + .003,
                             community == 'GRAND BOULEVARD' ~ .$lat + .003,
                             community == 'ARMOUR SQUARE' ~ .$lat + .002,
                             community == 'WEST PULLMAN' ~ .$lat + .003,
                             community == 'SOUTH CHICAGO' ~ .$lat + .003,
                             community == 'CHATHAM' ~ .$lat + .003,
                             TRUE ~ as.numeric(.$lat)),
         lon_mod = case_when(community == 'LOWER WEST SIDE'~ .$lon - .003, # West
                             community == 'MCKINLEY PARK' ~ .$lon - .003,
                             community == 'GARFIELD RIDGE'~ .$lon - .02,
                             community == 'WASHINGTON PARK' ~ .$lon - .004,
                             community == 'GRAND BOULEVARD' ~ .$lon - .003,
                             community == 'ENGLEWOOD' ~ .$lon - .004,
                             community == 'ROSELAND' ~ .$lon - .007,
                             community == 'HEGEWISCH' ~ .$lon - .009,
                             community == 'ARMOUR SQUARE' ~ .$lon - .007,
                             community == 'EAST GARFIELD PARK' ~ .$lon + .003, # East
                             community == 'OAKLAND' ~ .$lon + .01,
                             community == 'NEAR NORTH SIDE' ~ .$lon + .002,
                             community == 'WEST PULLMAN' ~ .$lon + .007,
                             community == 'WEST ENGLEWOOD' ~ .$lon + .006,
                             community == 'KENWOOD' ~ .$lon + .005,
                             community == 'MCKINLEY PARK' ~ .$lon + .005,
                             community == 'EDGEWATER' ~ .$lon + .006,
                             community == 'IRVING PARK' ~ .$lon + .005,
                             community == 'BRIDGEPORT' ~ .$lon + .0065,
                             community == 'CALUMET HEIGHTS' ~ .$lon + .003,
                             community == 'DOUGLAS' ~ .$lon + .003,
                             TRUE ~ as.numeric(.$lon)))      

#colorhexes <-  colorRampPalette(c('#e41a1c','#f781bf','#ff7f00','#ffff33','#4daf4a','#377eb8','#984ea3','#999999'))(16)
#colorhexes <-  colorRampPalette(c('#543005','#8c510a','#bf812d','#35978f','#01665e','#003c30','#40004b','#762a83','#9970ab','#5aae61','#1b7837','#00441b'))(16)
#,'#67001f','#b2182b','#d6604d','#4393c3','#2166ac','#053061'))(16)

colorhexes <-  colorRampPalette(c('#f44336','#E91E63','#9C27B0','#673AB7','#3F51B5','#2196F3','#00BCD4','#009688','#4CAF50','#CDDC39','#FFEB3B','#FFC107','#FF9800','#795548','#9E9E9E','#607D8B'))(16)

(panel0 <- ggplot(df_analysis) +
    geom_sf(fill = 'white', color = '#242629', size = .45, alpha= .6) +
    geom_sf(data = df_outlines_all, aes(fill =  race_income_label), color = 'white', size = .45, alpha= .6) +
    #geom_sf(data = df_outlines_cops, color = '#242629', size = .3, alpha= 0) +
    geom_text(mapping = aes(x = lon_mod,y =lat_mod, label = str_wrap(community_label, width = 4)), 
              fontface = 'bold', size = 2.85) +
    scale_fill_manual(values=colorhexes)+
    labs(title = "",
         subtitle = "Race and Income in Chicago Community Areas",
         caption = '  Source: U.S. Census Bureau. Analysis of 2014-2018 American Community Survey 5-year estimates.') +
    coord_sf(clip = "on") +
    theme_minimal() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing.x=unit(0, "lines"),
          panel.spacing.y=unit(0, "lines"),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.title = element_text(face = 'bold',size=10),
          #plot.title = element_blank(),
          legend.text = element_text(size= 13),
          legend.title = element_blank(),
          #legend.position= "bottom",
          plot.subtitle=element_text(size=15, hjust = 1, vjust= -3, face = 'bold'),
          plot.caption=element_text(size=13, vjust =5,  hjust = 0, face = 'italic'),
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.margin=unit(c(t=-5,r=0,b=0,l=0), "mm"),
          panel.border = element_blank()) )

ggsave(paste0(path_wd,'panel0.png'), panel0, dpi = 400, height =11, width=10)




