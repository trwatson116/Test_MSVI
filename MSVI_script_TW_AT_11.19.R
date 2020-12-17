library(tidyverse)
library(janitor)

# Import and cleaning -----------------------------------------------------
# setwd("C:/Users/trwatson/Documents/R code")
fips_census <- read_csv("fips_codes.csv")
fips_census$county_state = tolower(fips_census$name)
fips_census$county_state <- gsub("," , "" , fips_census$county_state)
fips_census <- fips_census %>% rename(fips_county=fips)
fips_census <- fips_census %>% select(-name)

# removed "county", "parish", "municipio", "borough" and "census area" from county names
fips_census_cleaned <- fips_census
fips_census_cleaned$county_state <- gsub("county", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("parish", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("municipio", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("and borough", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("census area", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub(" borough", "", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("  ", " ", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("lasalle", "la salle", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("laporte", "la porte", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("dekalb", "de kalb", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("debaca", "de baca", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- gsub("anchorage municipality", "anchorage", fips_census_cleaned$county_state)
fips_census_cleaned$county_state <- iconv(fips_census_cleaned$county_state, from = 'UTF-8', to = 'ASCII//TRANSLIT')

fips_additional <- read.csv("abbv_additional.csv")
fips_additional <- fips_additional %>% clean_names()
fips_additional <- fips_additional %>% rename(fips_county=i_fips_county)
fips_additional <- fips_additional %>% mutate(fips_county = as.numeric(fips_county))
fips_census_cleaned <- bind_rows(fips_census_cleaned, fips_additional)

#####cms

library(MSVI)
cms_raw <- MSVI::cms
cms_raw$county_state <- paste(tolower(cms$county),tolower(cms$state))
cms_cleaned <- cms_raw %>% clean_names()
cms_cleaned$county_state <- gsub("county", "", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("  ", " ", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("parish", "", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("  ", " ", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("municipio", "", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("  ", " ", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("and borough", "", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("  ", " ", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("census area", "", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("  ", " ", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub(" borough", "", cms_cleaned$county_state)

cms_cleaned$county_state <- gsub("lasalle", "la salle", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("laporte", "la porte", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("dekalb", "de kalb", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("debaca", "de baca", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("anchorage municipality", "anchorage", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("wade hampton alaska", "kusilvak alaska", cms_cleaned$county_state)
cms_cleaned$county_state <- gsub("bedford city virginia", "bedford virginia", cms_cleaned$county_state)

cms_cleaned$fips[cms_cleaned$fips == 46113] <- 46102
cms_cleaned$fips[cms_cleaned$fips == 51515] <- 51019
cms_cleaned$fips[cms_cleaned$fips == 2270] <- 2158


cms_fips <- merge(x=cms_cleaned,y=fips_census_cleaned,by="county_state",all.x = TRUE)
cms_fips <- cms_fips %>% mutate(fips_county=as.numeric(fips_county))

cms_fips$check <- cms_fips$fips - cms_fips$fips_county

cms_check <- filter(cms_fips, check !=0 | is.na(check))

cms_fips_final <- cms_fips %>% select(c(fips,year,geography,measure,adjustment,analysis,domain,condition,primary_sex,primary_age,primary_dual,county,state,urban,primary_race,primary_eligibility,primary_denominator,analysis_value))

# Result is cms_fips_final

# TODO: remove extraneous columns, and make the fips one consistent throughout all data frames as well being the first column

#Changes:
#Bedford City, VA - now incorporated into the county of Bedford, VA
#Oglala Lakota County, SD - CMS data has outdated FIPS code; correct code is 46102 FIPS code in CMS raw data
#Wade Hampton, AK - recently renamed to Kusilvak, AK (reflected in CSV file), correct FIPS = 02158
#56 rows had "NA" in County column and had FIPS codes that do not correspond to any county

#####ahrf

ahrf_clean <- ahrf %>% clean_names()
ahrf_clean$county_state = paste(tolower(ahrf_clean$county),tolower(ahrf_clean$state))
ahrf_clean$county_state <- gsub("county", "", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("  ", " ", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("parish", "", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("  ", " ", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("\\(.*?\\)", "", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("  ", " ", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("obrien", "o'brien", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("lasalle", "la salle", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("laporte", "la porte", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("dekalb", "de kalb", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("debaca", "de baca", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("debaca", "de baca", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("pr of wales-outrketch", "prince of wales-hyder", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("the district", "district of columbia", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("juneau alaska", "juneau city alaska", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("wrangell alaska", "wrangell city alaska", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("wrangell-petersburg alaska", "petersburg alaska", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("kodiak island", "kodiak isl", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("sitka", "sitka city", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("skagway-hoonah-angoon", "skagway municipality", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("yakutat", "yakutat city", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("clifton forge city virginia", "alleghany virginia", ahrf_clean$county_state)
ahrf_clean$county_state <- gsub("bedford city virginia", "bedford virginia", ahrf_clean$county_state)

ahrf_fips <- merge(x=ahrf_clean,y=fips_census_cleaned,by="county_state",all.x = TRUE)
ahrf_check <- filter(ahrf_fips, is.na(fips_county))

ahrf_fips_final <- ahrf_fips %>% select(c(fips_county,category,variable,state_abbreviation,state,county,clinician_count,year_represented_by_variable,county_population,clinicians_per_100k_county_population,state_population,clinicians_per_100k_state_population,ahrf_release,ahrf_as_of_date))
ahrf_fips_final <- ahrf_fips_final %>% rename(fips=fips_county)
# Result ahrf_fips_final



#####definitive_hc

definitive_hc_clean <- definitive_hc %>% clean_names()
definitive_hc_clean$county_state = paste(tolower(definitive_hc_clean$county_name),tolower(definitive_hc_clean$state_name))
definitive_hc_clean$county_state <- gsub("county", "", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("  ", " ", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("obrien", "o'brien", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("lasalle", "la salle", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("laporte", "la porte", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("dekalb", "de kalb", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("debaca", "de baca", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("juneau alaska", "juneau city alaska", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("kodiak island alaska", "kodiak isl alaska", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("sitka alaska", "sitka city alaska", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("wrangell alaska", "wrangell city alaska", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("alexandria virginia", "alexandria city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("bristol virginia", "bristol city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("charlottesville virginia", "charlottesville city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("chesapeake virginia", "chesapeake city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("danville virginia", "danville city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("emporia virginia", "emporia city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("fredericksburg virginia", "fredericksburg city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("galax virginia", "galax city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("hampton virginia", "hampton city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("hopewell virginia", "hopewell city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("lexington virginia", "lexington city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("lynchburg virginia", "lynchburg city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("manassas virginia", "manassas city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("newport news virginia", "newport news city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("norfolk virginia", "norfolk city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("norton virginia", "norton city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("petersburg virginia", "petersburg city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("portsmouth virginia", "portsmouth city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("salem virginia", "salem city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("staunton virginia", "staunton city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("suffolk virginia", "suffolk city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("virginia beach virginia", "virginia beach city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("williamsburg virginia", "williamsburg city virginia", definitive_hc_clean$county_state)
definitive_hc_clean$county_state <- gsub("winchester virginia", "winchester city virginia", definitive_hc_clean$county_state)


definitive_hc_fips <- merge(x=definitive_hc_clean,y=fips_census_cleaned,by="county_state",all.x = TRUE)

definitive_hc_fips <- definitive_hc_fips %>% mutate(fips_county = as.numeric(fips_county))
definitive_hc_fips$check <- definitive_hc_fips$fips - definitive_hc_fips$fips_county

#Checked 24 counties without a match and FIPS codes are accurate; these did not join with a FIPS code from the CSV file because they lack the word "City" in the definitive_hc data
definitive_hc_check <- filter(definitive_hc_fips, check !=0 | is.na(check))

definitive_hc_fips_final <- definitive_hc_fips %>% select(c(fips,x,y,objectid,hospital_name,hospital_type,hq_address,hq_address1,hq_city,hq_state,hq_zip_code,county_name,state_name,state_fips,cnty_fips,num_licensed_beds,num_staffed_beds,num_icu_beds,adult_icu_beds,pedi_icu_beds,bed_utilization,potential_increase_in_bed_capac,avg_ventilator_usage))


# TODO: add city back to the virgina ones
# Result is definitive_hc_fips_final

#####county_health_rankings


county_health_rankings_clean <- county_health_rankings %>% clean_names()
county_health_rankings_clean <- county_health_rankings_clean[-c(1),]
county_health_rankings_clean <- county_health_rankings_clean %>% rename(county=name)
county_health_rankings_clean$county <- tolower(county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("county", "", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("  ", " ", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("parish", "", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("  ", " ", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("municipio", "", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("  ", " ", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub(" borough", "", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("  ", " ", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("census area", "", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("dekalb", "de kalb", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("debaca", "de baca", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("juneau", "juneau city", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("kodiak island", "kodiak isl", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("mc kean", "mckean", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("sitka", "sitka city", county_health_rankings_clean$county)
county_health_rankings_clean$county <- gsub("yakutat", "yakutat city", county_health_rankings_clean$county)


state_abbv <- read.csv("state_abbv.csv")
state_abbv <- state_abbv %>% clean_names()
state_abbv <- state_abbv %>% rename(state=i_state)
state_abbv$state = tolower(state_abbv$state)
county_health_rankings_clean_2 <- merge(x=county_health_rankings_clean,y=state_abbv,by="state_abbreviation",all.x=TRUE)
county_health_rankings_clean_2$county_state = paste(county_health_rankings_clean_2$county, county_health_rankings_clean_2$state)
county_health_rankings_clean_2$county_state <- gsub("  ", " ", county_health_rankings_clean_2$county_state)
county_health_rankings_clean_2$county_state <- gsub("juneau city wisconsin", "juneau wisconsin", county_health_rankings_clean_2$county_state)
county_health_rankings_fips <- merge(x=county_health_rankings_clean_2,y=fips_census_cleaned,by="county_state",all.x=TRUE)

#the below lists those rows that did not have a county name
county_check <- filter(county_health_rankings_fips, is.na(fips_county))
county_health_rankings_fips_final <- county_health_rankings_fips %>% filter(!is.na(fips_county))
county_health_rankings_fips_final$x5_digit_fips_code <- as.numeric(county_health_rankings_fips_final$x5_digit_fips_code)
county_health_rankings_fips_final$check <- county_health_rankings_fips_final$fips - county_health_rankings_fips_final$x5_digit_fips_code
county_check_2 <- filter(county_health_rankings_fips_final, is.na(check) | check != 0)
county_health_rankings_fips_final <- filter(county_health_rankings_fips_final, check==0)
county_health_rankings_fips_final_2 <- county_health_rankings_fips_final %>% select(c(5,3,4,2,seq(6,787,1)))
county_health_rankings_fips_final_2 <- county_health_rankings_fips_final_2 %>% rename(fips=x5_digit_fips_code)

# final - county_health_rankings_fips_final_2



#####svi_ranking

# focus on the counties, not the metro areas. you can filter the metro ones out
# when ur done fixing the county ones, you can bind row it to the metro ones

svi_ranking_clean <- svi_ranking %>% clean_names()

svi_ranking_clean$name <- tolower(svi_ranking_clean$name)
svi_ranking_metro_areas <- filter(svi_ranking_clean, grepl('metro area', name))
svi_ranking_metro_areas$fips <- as.numeric(svi_ranking_metro_areas$fips)
svi_ranking_counties <- filter(svi_ranking_clean, !grepl('metro area', name))
svi_ranking_counties$name <- gsub(",", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("county", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub(",", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("  ", " ", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("parish", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("  ", " ", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("and borough", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub(" borough", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("  ", " ", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("census area", "", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("  ", " ", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("dekalb", "de kalb", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("laporte", "la porte", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("lasalle", "la salle", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("kodiak island alaska", "kodiak isl alaska", svi_ranking_counties$name)
svi_ranking_counties$name <- gsub("anchorage municipality", "anchorage", svi_ranking_counties$name)
svi_ranking_counties$name <- iconv(svi_ranking_counties$name, from = 'UTF-8', to = 'ASCII//TRANSLIT')

svi_ranking_counties$fips <- as.numeric(svi_ranking_counties$fips)
svi_ranking_counties <- svi_ranking_counties %>% rename(county_state=name)

svi_ranking_fips <- merge(x=svi_ranking_counties,y=fips_census_cleaned,by="county_state",all.x=TRUE)

svi_ranking_fips <- svi_ranking_fips %>% mutate(fips_county = as.numeric(fips_county))
svi_ranking_fips$check <- svi_ranking_fips$fips - svi_ranking_fips$fips_county
svi_check <- filter(svi_ranking_fips, check !=0 | is.na(check))


svi_ranking_fips_semifinal <- bind_rows(svi_ranking_fips, svi_ranking_metro_areas)
svi_ranking_fips_final <- svi_ranking_fips_semifinal %>% select(c(fips,county_state,socioeconomic,household_disability,minority_language,housing_transportation,overall_ranking))
svi_ranking_fips_final <- svi_ranking_fips_final %>% rename(name=county_state)
# SVI is officially ready and the result is called svi_ranking_fips_final


