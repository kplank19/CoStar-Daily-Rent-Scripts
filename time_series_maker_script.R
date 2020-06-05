rm(list = ls())

pacman::p_load(tidyverse, magrittr, janitor, readxl,
               lubridate, xlsx, formattable, sf, mapview,
               qdap, ggplot2, scales, DT, stargazer, ggthemes,
               stringdist, fuzzyjoin, stringr, bdscale, 
               leaflet, reshape2, dplyr, reactable, sp,
               ggrepel, ggalt, tidyr, plotly, data.table)



# Read in data
# 
df <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/current_storage.csv")









# 
# nor <- c("Seattle/Puget Sound")
# soc <- c("Inland Empire (California)", "Los Angeles", "Orange County (California)",
#          "San Diego", "Santa Barbara/Sta Maria/Goleta")
# 
# 
# #summary for Paul
# df <- subset(df, df$date == "2020-04-29")
# df_cal <- df %>% filter(market_name %in% soc)
# 
# 
# 
# 
# df_cal %<>% mutate(total_studio = sum(number_of_studios_units, na.rm = TRUE),
#               total_one_bed = sum(number_of_1_bedrooms_units, na.rm = TRUE),
#               total_two_bed = sum(number_of_2_bedrooms_units, na.rm = TRUE))
# 
# # 1365
# #2730
# 
# df_low_studio <- subset(df_cal, df_cal$studio_asking_rent_unit < 1365)
# df_low_studio %<>% mutate(filt_studio = sum(number_of_studios_units, na.rm = TRUE)) %>% 
#                    mutate(pct_studio = filt_studio/total_studio)
# df_low_studio$pct_studio[1]
# 
# 
# df_low_one <- subset(df_cal, df_cal$one_bedroom_asking_rent_unit < 1365)
# df_low_one %<>% mutate(filt_one = sum(number_of_1_bedrooms_units, na.rm = TRUE)) %>% 
#   mutate(pct_one = filt_one/total_one_bed)
# df_low_one$pct_one[1]
# 
# 
# df_low_two <- subset(df_cal, df_cal$two_bedroom_asking_rent_unit < 1365)
# df_low_two %<>% mutate(filt_two = sum(number_of_2_bedrooms_units, na.rm = TRUE)) %>% 
#   mutate(pct_two = filt_two/total_two_bed)
# df_low_two$pct_two[1]
# 











df_init <- subset(df, df$date == "2020-02-28")

names(df_init) <- paste0(names(df_init), "_init")

df_init %<>% select(property_id_init, date_init, 
                    avg_asking_unit_init, one_bedroom_asking_rent_unit_init, two_bedroom_asking_rent_unit_init, studio_asking_rent_unit_init, #asking
                    avg_asking_sf_init, one_bedroom_asking_rent_sf_init, two_bedroom_asking_rent_sf_init, studio_asking_rent_sf_init, #asking sf
                    avg_effective_unit_init, one_bedroom_effective_rent_unit_init, two_bedroom_effective_rent_unit_init, studio_effective_rent_unit_init, #effective
                    avg_effective_sf_init, one_bedroom_effective_rent_sf_init, two_bedroom_effective_rent_sf_init, studio_effective_rent_sf_init) # effective sf


# merge
df <- merge(df, df_init, by.x = "property_id", by.y = "property_id_init")

# % change mutations
df %<>% mutate(avg_asking_unit_pct = (avg_asking_unit - avg_asking_unit_init.y)/avg_asking_unit_init.y,
               one_bedroom_asking_rent_unit_pct = (one_bedroom_asking_rent_unit - one_bedroom_asking_rent_unit_init)/one_bedroom_asking_rent_unit_init,
               two_bedroom_asking_rent_unit_pct = (two_bedroom_asking_rent_unit - two_bedroom_asking_rent_unit_init)/two_bedroom_asking_rent_unit_init,
               studio_asking_rent_unit_pct = (studio_asking_rent_unit - studio_asking_rent_unit_init)/studio_asking_rent_unit_init,
               avg_asking_sf_pct = (avg_asking_sf-avg_asking_sf_init)/avg_asking_sf_init,
               one_bedroom_asking_rent_sf_pct = (one_bedroom_asking_rent_sf- one_bedroom_asking_rent_sf_init)/one_bedroom_asking_rent_sf_init,
               two_bedroom_asking_rent_sf_pct = (two_bedroom_asking_rent_sf- two_bedroom_asking_rent_sf_init)/two_bedroom_asking_rent_sf_init,
               studio_asking_rent_sf_pct = (studio_asking_rent_sf- studio_asking_rent_sf_init)/studio_asking_rent_sf_init,
               avg_effective_unit_pct = (avg_effective_unit-avg_effective_unit_init.y)/avg_effective_unit_init.y,
               one_bedroom_effective_rent_unit_pct = (one_bedroom_effective_rent_unit-one_bedroom_effective_rent_unit_init.y)/one_bedroom_effective_rent_unit_init.y,
               two_bedroom_effective_rent_unit_pct = (two_bedroom_effective_rent_unit-two_bedroom_effective_rent_unit_init.y)/two_bedroom_effective_rent_unit_init.y,
               studio_effective_rent_unit_pct = (studio_effective_rent_unit-studio_effective_rent_unit_init.y)/studio_effective_rent_unit_init.y,
               avg_effective_sf_pct = (avg_effective_sf-avg_effective_sf_init)/avg_effective_sf_init,
               one_bedroom_effective_rent_sf_pct = (one_bedroom_effective_rent_sf - one_bedroom_effective_rent_sf_init)/one_bedroom_effective_rent_sf_init,
               two_bedroom_effective_rent_sf_pct = (two_bedroom_effective_rent_sf - two_bedroom_effective_rent_sf_init)/two_bedroom_effective_rent_sf_init,
               studio_effective_rent_sf_pct = (studio_effective_rent_sf - studio_effective_rent_sf_init)/studio_effective_rent_sf_init) %>% 
        mutate(avg_asking_unit_pct_wt = avg_asking_unit_pct*number_of_units,
               one_bedroom_asking_rent_unit_pct_wt = one_bedroom_asking_rent_unit_pct*number_of_1_bedrooms_units,
               two_bedroom_asking_rent_unit_pct_wt = two_bedroom_asking_rent_unit_pct*number_of_2_bedrooms_units,
               studio_asking_rent_unit_pct_wt = studio_asking_rent_unit_pct*number_of_studios_units,
               avg_asking_sf_pct_wt = avg_asking_sf_pct*number_of_units,
               one_bedroom_avg_sf_pct_wt = one_bedroom_asking_rent_sf_pct*number_of_1_bedrooms_units,
               two_bedroom_avg_sf_pct_wt = two_bedroom_asking_rent_sf_pct*number_of_2_bedrooms_units,
               studio_avg_sf_pct_wt = studio_asking_rent_sf_pct*number_of_studios_units,
               avg_effective_unit_pct_wt = avg_effective_unit_pct*number_of_units,
               one_bedroom_effective_rent_unit_pct_wt = one_bedroom_effective_rent_unit_pct*number_of_1_bedrooms_units,
               two_bedroom_effective_rent_unit_pct_wt = two_bedroom_effective_rent_unit_pct*number_of_2_bedrooms_units,
               studio_effective_rent_unit_pct_wt = studio_effective_rent_unit_pct*number_of_studios_units,
               avg_effective_sf_pct_wt = avg_effective_sf_pct*number_of_units,
               one_bedroom_effective_rent_sf_pct_wt = one_bedroom_effective_rent_sf_pct*number_of_1_bedrooms_units,
               two_bedroom_effective_rent_sf_pct_wt = two_bedroom_effective_rent_sf_pct*number_of_2_bedrooms_units,
               studio_effective_rent_sf_pct_wt = studio_effective_rent_sf_pct*number_of_studios_units,
               avg_asking_unit_wt = avg_asking_unit*number_of_units,
               one_bedroom_asking_rent_unit_wt = one_bedroom_asking_rent_unit*number_of_1_bedrooms_units,
               two_bedroom_asking_rent_unit_wt = two_bedroom_asking_rent_unit*number_of_2_bedrooms_units,
               studio_asking_rent_unit_wt = studio_asking_rent_unit*number_of_studios_units,
               avg_unit_sf_wt = avg_asking_sf*number_of_units,
               one_bedroom_avg_sf_wt = one_bedroom_asking_rent_sf*number_of_1_bedrooms_units,
               two_bedroom_avg_sf_wt = two_bedroom_asking_rent_sf*number_of_2_bedrooms_units,
               studio_avg_sf_wt = studio_asking_rent_sf*number_of_studios_units,
               avg_effective_unit_wt = avg_effective_unit*number_of_units,
               one_bedroom_effective_rent_unit_wt = one_bedroom_effective_rent_unit*number_of_1_bedrooms_units,
               two_bedroom_effective_rent_unit_wt = two_bedroom_effective_rent_unit*number_of_2_bedrooms_units,
               studio_effective_rent_unit_wt = studio_effective_rent_unit*number_of_studios_units,
               avg_effective_sf_wt = avg_effective_sf*number_of_units,
               one_bedroom_effective_rent_sf_wt = one_bedroom_effective_rent_sf*number_of_1_bedrooms_units,
               two_bedroom_effective_rent_sf_wt = two_bedroom_effective_rent_sf*number_of_2_bedrooms_units,
               studio_effective_rent_sf_wt = studio_effective_rent_sf*number_of_studios_units)



# Props & comps Information
props_comps <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/submarket_mapping/completed_comps_v2.csv")
# remove bad matches (such as ones in wrong markets)
props_comps <- subset(props_comps, props_comps$costar_id_comp != 4113693)
props_comps <- subset(props_comps, props_comps$costar_id_comp != 1575683)

props_comps <- merge(props_comps, market_id_storage_name, by.x = "costar_id_prop", by.y = "property_id")
props_comps %<>% select(costar_id_prop, costar_id=costar_id_comp, Name=property_name, market=essex_markets, team) %>% select(-X)

props_comps$uniq <- paste0(props_comps$costar_id_prop, "|", props_comps$costar_id_comp)
props_comps <- props_comps[!duplicated(props_comps$uniq), ]
checker <- props_comps %>% group_by(costar_id_prop) %>% tally()
#


# create uniq names prior to merge
df_props <- df
df_comps <- df
names(df_props) <- paste0(names(df_props), "_props")
names(df_comps) <- paste0(names(df_comps), "_comps")


#
comps_ts <- merge(props_comps, df_props, by.x ="costar_id_prop", by.y = "property_id_props")
# unique comp & date variable
comps_ts$uniq_date <- paste0(comps_ts$costar_id_comp, "|", comps_ts$date_props)
df_comps$uniq_date <- paste0(df_comps$property_id_comps, "|", df_comps$date_comps)

#merge by uniq
comps_ts <- merge(comps_ts, df_comps, by = "uniq_date")






market_name <- levels(as.factor(comps_ts$market_name_props))
essex_markets <- c("Oakland", "Orange", "Los Angeles", "San Francisco", "Orange", "San Diego", "San Francisco",
                   "Ventura", "San Jose", "Seattle", "San Jose")
marx <- data.frame(market_name, essex_markets)
comps_ts <- merge(comps_ts, marx, by.x = "market_name_props", by.y = "market_name")
comps_ts$essex_markets <- as.character(comps_ts$essex_markets)
comps_ts$essex_markets <- ifelse(comps_ts$county_name_props == "Ventura", "Ventura", comps_ts$essex_markets)
comps_ts$essex_markets <- as.factor(comps_ts$essex_markets)
rm(marx)





market_name <- levels(as.factor(df$market_name))
essex_markets <- c("Oakland", "Orange", "Los Angeles", "San Francisco", "Orange", "San Diego", "San Francisco",
                   "Ventura", "San Jose", "Seattle", "San Jose")
marx <- data.frame(market_name, essex_markets)
df <- merge(df, marx, by = "market_name")
df$essex_markets <- as.character(df$essex_markets)
df$essex_markets <- ifelse(df$county_name == "Ventura", "Ventura", df$essex_markets)
df$essex_markets <- as.factor(df$essex_markets)
rm(marx)


# 
mapper <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/submarket_mapping/mapper.csv")
mapper %<>% select(-X)
rpm <- read.csv("C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/submarket_mapping/rpm_mapper.csv")
rpm %<>% select(-X)


#attach submarket
df <- merge(df, mapper, by.x = "zip_short",by.y = "zip")

comps_ts <- merge(comps_ts, mapper, by.x = "zip_short_props",by.y = "zip")

comps_ts <- merge(comps_ts, rpm, by.x = "costar_id_prop", by.y = "property_id")



# Group by
# Need to create 3 additional variables to be attached to property Market, Submarket, Comps
comps_ts
market_group <- df
submarket_group <- df


# submarket----

submarket_group %<>% group_by(date, ess_submarket.y) %>%
  mutate(units_tot = sum(number_of_units, na.rm = TRUE),
         one_bed_tot = sum(number_of_1_bedrooms_units, na.rm = TRUE),
         two_bed_tot = sum(number_of_2_bedrooms_units, na.rm = TRUE),
         studio_tot = sum(number_of_studios_units, na.rm = TRUE)) %>%
  mutate(avg_asking_unit_pct_wt = avg_asking_unit_pct_wt/units_tot,
         one_bedroom_asking_rent_unit_pct_wt = one_bedroom_asking_rent_unit_pct_wt/one_bed_tot,
         two_bedroom_asking_rent_unit_pct_wt = two_bedroom_asking_rent_unit_pct_wt/two_bed_tot,
         studio_asking_rent_unit_pct_wt = studio_asking_rent_unit_pct_wt/studio_tot,
         avg_unit_sf_pct_wt = avg_asking_sf_pct_wt/units_tot,
         one_bedroom_avg_sf_pct_wt = one_bedroom_avg_sf_pct_wt/one_bed_tot,
         two_bedroom_avg_sf_pct_wt = two_bedroom_avg_sf_pct_wt/two_bed_tot,
         studio_avg_sf_pct_wt = studio_avg_sf_pct_wt/studio_tot,
         avg_effective_unit_pct_wt = avg_effective_unit_pct_wt/units_tot,
         one_bedroom_effective_rent_unit_pct_wt = one_bedroom_effective_rent_unit_pct_wt/one_bed_tot,
         two_bedroom_effective_rent_unit_pct_wt = two_bedroom_effective_rent_unit_pct_wt/two_bed_tot,
         studio_effective_rent_unit_pct_wt = studio_effective_rent_unit_pct_wt/studio_tot,
         avg_effective_sf_pct_wt = avg_effective_sf_pct_wt/units_tot,
         one_bedroom_effective_rent_sf_pct_wt = one_bedroom_effective_rent_sf_pct_wt/one_bed_tot,
         two_bedroom_effective_rent_sf_pct_wt = two_bedroom_effective_rent_sf_pct_wt/two_bed_tot,
         studio_effective_rent_sf_pct_wt = studio_effective_rent_sf_pct_wt/studio_tot,
         avg_asking_unit_wt = avg_asking_unit_wt/units_tot,
         one_bedroom_asking_rent_unit_wt = one_bedroom_asking_rent_unit_wt/one_bed_tot,
         two_bedroom_asking_rent_unit_wt = two_bedroom_asking_rent_unit_wt/two_bed_tot,
         studio_asking_rent_unit_wt = studio_asking_rent_unit_wt/studio_tot,
         avg_unit_sf_wt = avg_asking_unit_wt/units_tot,
         one_bedroom_avg_sf_wt = one_bedroom_avg_sf_wt/one_bed_tot,
         two_bedroom_avg_sf_wt = two_bedroom_avg_sf_wt/two_bed_tot,
         studio_avg_sf_wt = studio_avg_sf_wt/studio_tot,
         avg_effective_unit_wt = avg_effective_unit_wt/units_tot,
         one_bedroom_effective_rent_unit_wt = one_bedroom_effective_rent_unit_wt/one_bed_tot,
         two_bedroom_effective_rent_unit_wt = two_bedroom_effective_rent_unit_wt/two_bed_tot,
         studio_effective_rent_unit_wt = studio_effective_rent_unit_wt/studio_tot,
         avg_effective_sf_wt = avg_effective_sf_wt/units_tot,
         one_bedroom_effective_rent_sf_wt = one_bedroom_effective_rent_sf_wt/one_bed_tot,
         two_bedroom_effective_rent_sf_wt = two_bedroom_effective_rent_sf_wt/two_bed_tot,
         studio_effective_rent_sf_wt = studio_effective_rent_sf_wt/studio_tot) %>%
  summarise(avg_asking_unit_pct_wt = sum(avg_asking_unit_pct_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_pct_wt = sum(one_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            two_bedroom_asking_rent_unit_pct_wt = sum(two_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            studio_asking_rent_unit_pct_wt = sum(studio_asking_rent_unit_pct_wt, na.rm = TRUE),
            avg_unit_sf_pct_wt = sum(avg_unit_sf_pct_wt, na.rm = TRUE),
            one_bedroom_avg_sf_pct_wt = sum(one_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            two_bedroom_avg_sf_pct_wt = sum(two_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            studio_avg_sf_pct_wt = sum(studio_avg_sf_pct_wt, na.rm=TRUE),
            avg_effective_unit_pct_wt = sum(avg_effective_unit_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_unit_pct_wt = sum(one_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_unit_pct_wt = sum(two_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            studio_effective_rent_unit_pct_wt = sum(studio_effective_rent_unit_pct_wt, na.rm=TRUE),
            avg_effective_sf_pct_wt = sum(avg_effective_sf_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_sf_pct_wt = sum(one_bedroom_effective_rent_sf_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_sf_pct_wt = sum(two_bedroom_effective_rent_sf_pct_wt, na.rm =TRUE),
            studio_effective_rent_sf_pct_wt = sum(studio_effective_rent_sf_pct_wt, na.rm=TRUE),
            avg_asking_unit_wt = sum(avg_asking_unit_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_wt = sum(one_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            two_bedroom_asking_rent_unit_wt = sum(two_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            studio_asking_rent_unit_wt = sum(studio_asking_rent_unit_wt, na.rm = TRUE),
            avg_unit_sf_wt = sum(avg_unit_sf_wt, na.rm = TRUE),
            one_bedroom_avg_sf_wt = sum(one_bedroom_avg_sf_wt, na.rm = TRUE),
            two_bedroom_avg_sf_wt = sum(two_bedroom_avg_sf_wt, na.rm = TRUE),
            studio_avg_sf_wt = sum(studio_avg_sf_wt, na.rm = TRUE),
            avg_effective_unit_wt = sum(avg_effective_unit_wt, na.rm = TRUE),
            one_bedroom_effective_rent_unit_wt = sum(one_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            two_bedroom_effective_rent_unit_wt = sum(two_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            studio_effective_rent_unit_wt = sum(studio_effective_rent_unit_wt, na.rm = TRUE),
            avg_effective_sf_wt = sum(avg_effective_sf_wt, na.rm = TRUE),
            one_bedroom_effective_rent_sf_wt = sum(one_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            two_bedroom_effective_rent_sf_wt = sum(two_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            studio_effective_rent_sf_wt = sum(studio_effective_rent_sf_wt, na.rm = TRUE)) %>% ungroup()
  


names(submarket_group) <- paste0(names(submarket_group), "_submarket")

#----

# COMPS----

comp_group <- comps_ts %>% group_by(date_props, costar_id_prop) %>%
  mutate(units_tot = sum(number_of_units_comps, na.rm = TRUE),
         one_bed_tot = sum(number_of_1_bedrooms_units_comps, na.rm = TRUE),
         two_bed_tot = sum(number_of_2_bedrooms_units_comps, na.rm = TRUE),
         studio_tot = sum(number_of_studios_units_comps, na.rm = TRUE)) %>%
  mutate(avg_asking_unit_pct_wt = avg_asking_unit_pct_wt_comps/units_tot,
         one_bedroom_asking_rent_unit_pct_wt = one_bedroom_asking_rent_unit_pct_wt_comps/one_bed_tot,
         two_bedroom_asking_rent_unit_pct_wt = two_bedroom_asking_rent_unit_pct_wt_comps/two_bed_tot,
         studio_asking_rent_unit_pct_wt = studio_asking_rent_unit_pct_wt_comps/studio_tot,
         avg_unit_sf_pct_wt = avg_asking_sf_pct_wt_comps/units_tot,
         one_bedroom_avg_sf_pct_wt = one_bedroom_avg_sf_pct_wt_comps/one_bed_tot,
         two_bedroom_avg_sf_pct_wt = two_bedroom_avg_sf_pct_wt_comps/two_bed_tot,
         studio_avg_sf_pct_wt = studio_avg_sf_pct_wt_comps/studio_tot,
         avg_effective_unit_pct_wt = avg_effective_unit_pct_wt_comps/units_tot,
         one_bedroom_effective_rent_unit_pct_wt = one_bedroom_effective_rent_unit_pct_wt_comps/one_bed_tot,
         two_bedroom_effective_rent_unit_pct_wt = two_bedroom_effective_rent_unit_pct_wt_comps/two_bed_tot,
         studio_effective_rent_unit_pct_wt = studio_effective_rent_unit_pct_wt_comps/studio_tot,
         avg_effective_sf_pct_wt = avg_effective_sf_pct_wt_comps/units_tot,
         one_bedroom_effective_rent_sf_pct_wt = one_bedroom_effective_rent_sf_pct_wt_comps/one_bed_tot,
         two_bedroom_effective_rent_sf_pct_wt = two_bedroom_effective_rent_sf_pct_wt_comps/two_bed_tot,
         studio_effective_rent_sf_pct_wt = studio_effective_rent_sf_pct_wt_comps/studio_tot,
         avg_asking_unit_wt = avg_asking_unit_wt_comps/units_tot,
         one_bedroom_asking_rent_unit_wt = one_bedroom_asking_rent_unit_wt_comps/one_bed_tot,
         two_bedroom_asking_rent_unit_wt = two_bedroom_asking_rent_unit_wt_comps/two_bed_tot,
         studio_asking_rent_unit_wt = studio_asking_rent_unit_wt_comps/studio_tot,
         avg_unit_sf_wt = avg_unit_sf_wt_comps/units_tot,
         one_bedroom_avg_sf_wt = one_bedroom_avg_sf_wt_comps/one_bed_tot,
         two_bedroom_avg_sf_wt = two_bedroom_avg_sf_wt_comps/two_bed_tot,
         studio_avg_sf_wt = studio_avg_sf_wt_comps/studio_tot,
         avg_effective_unit_wt = avg_effective_unit_wt_comps/units_tot,
         one_bedroom_effective_rent_unit_wt = one_bedroom_effective_rent_unit_wt_comps/one_bed_tot,
         two_bedroom_effective_rent_unit_wt = two_bedroom_effective_rent_unit_wt_comps/two_bed_tot,
         studio_effective_rent_unit_wt = studio_effective_rent_unit_wt_comps/studio_tot,
         avg_effective_sf_wt = avg_effective_sf_wt_comps/units_tot,
         one_bedroom_effective_rent_sf_wt = one_bedroom_effective_rent_sf_wt_comps/one_bed_tot,
         two_bedroom_effective_rent_sf_wt = two_bedroom_effective_rent_sf_wt_comps/two_bed_tot,
         studio_effective_rent_sf_wt = studio_effective_rent_sf_wt_comps/studio_tot) %>%
  summarise(avg_asking_unit_pct_wt = sum(avg_asking_unit_pct_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_pct_wt = sum(one_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            two_bedroom_asking_rent_unit_pct_wt = sum(two_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            studio_asking_rent_unit_pct_wt = sum(studio_asking_rent_unit_pct_wt, na.rm = TRUE),
            avg_unit_sf_pct_wt = sum(avg_unit_sf_pct_wt, na.rm = TRUE),
            one_bedroom_avg_sf_pct_wt = sum(one_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            two_bedroom_avg_sf_pct_wt = sum(two_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            studio_avg_sf_pct_wt = sum(studio_avg_sf_pct_wt, na.rm=TRUE),
            avg_effective_unit_pct_wt = sum(avg_effective_unit_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_unit_pct_wt = sum(one_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_unit_pct_wt = sum(two_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            studio_effective_rent_unit_pct_wt = sum(studio_effective_rent_unit_pct_wt, na.rm=TRUE),
            avg_effective_sf_pct_wt = sum(avg_effective_sf_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_sf_pct_wt = sum(one_bedroom_effective_rent_sf_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_sf_pct_wt = sum(two_bedroom_effective_rent_sf_pct_wt, na.rm =TRUE),
            studio_effective_rent_sf_pct_wt = sum(studio_effective_rent_sf_pct_wt, na.rm=TRUE),
            avg_asking_unit_wt = sum(avg_asking_unit_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_wt = sum(one_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            two_bedroom_asking_rent_unit_wt = sum(two_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            studio_asking_rent_unit_wt = sum(studio_asking_rent_unit_wt, na.rm = TRUE),
            avg_unit_sf_wt = sum(avg_unit_sf_wt, na.rm = TRUE),
            one_bedroom_avg_sf_wt = sum(one_bedroom_avg_sf_wt, na.rm = TRUE),
            two_bedroom_avg_sf_wt = sum(two_bedroom_avg_sf_wt, na.rm = TRUE),
            studio_avg_sf_wt = sum(studio_avg_sf_wt, na.rm = TRUE),
            avg_effective_unit_wt = sum(avg_effective_unit_wt, na.rm = TRUE),
            one_bedroom_effective_rent_unit_wt = sum(one_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            two_bedroom_effective_rent_unit_wt = sum(two_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            studio_effective_rent_unit_wt = sum(studio_effective_rent_unit_wt, na.rm = TRUE),
            avg_effective_sf_wt = sum(avg_effective_sf_wt, na.rm = TRUE),
            one_bedroom_effective_rent_sf_wt = sum(one_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            two_bedroom_effective_rent_sf_wt = sum(two_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            studio_effective_rent_sf_wt = sum(studio_effective_rent_sf_wt, na.rm = TRUE)) %>% ungroup()


names(comp_group) <- paste0(names(comp_group), "_cgroup")

#----

# MARKET ----


market_group %<>% group_by(date, essex_markets) %>%
  mutate(units_tot = sum(number_of_units, na.rm = TRUE),
         one_bed_tot = sum(number_of_1_bedrooms_units, na.rm = TRUE),
         two_bed_tot = sum(number_of_2_bedrooms_units, na.rm = TRUE),
         studio_tot = sum(number_of_studios_units, na.rm = TRUE)) %>%
  mutate(avg_asking_unit_pct_wt = avg_asking_unit_pct_wt/units_tot,
         one_bedroom_asking_rent_unit_pct_wt = one_bedroom_asking_rent_unit_pct_wt/one_bed_tot,
         two_bedroom_asking_rent_unit_pct_wt = two_bedroom_asking_rent_unit_pct_wt/two_bed_tot,
         studio_asking_rent_unit_pct_wt = studio_asking_rent_unit_pct_wt/studio_tot,
         avg_unit_sf_pct_wt = avg_asking_sf_pct_wt/units_tot,
         one_bedroom_avg_sf_pct_wt = one_bedroom_avg_sf_pct_wt/one_bed_tot,
         two_bedroom_avg_sf_pct_wt = two_bedroom_avg_sf_pct_wt/two_bed_tot,
         studio_avg_sf_pct_wt = studio_avg_sf_pct_wt/studio_tot,
         avg_effective_unit_pct_wt = avg_effective_unit_pct_wt/units_tot,
         one_bedroom_effective_rent_unit_pct_wt = one_bedroom_effective_rent_unit_pct_wt/one_bed_tot,
         two_bedroom_effective_rent_unit_pct_wt = two_bedroom_effective_rent_unit_pct_wt/two_bed_tot,
         studio_effective_rent_unit_pct_wt = studio_effective_rent_unit_pct_wt/studio_tot,
         avg_effective_sf_pct_wt = avg_effective_sf_pct_wt/units_tot,
         one_bedroom_effective_rent_sf_pct_wt = one_bedroom_effective_rent_sf_pct_wt/one_bed_tot,
         two_bedroom_effective_rent_sf_pct_wt = two_bedroom_effective_rent_sf_pct_wt/two_bed_tot,
         studio_effective_rent_sf_pct_wt = studio_effective_rent_sf_pct_wt/studio_tot,
         avg_asking_unit_wt = avg_asking_unit_wt/units_tot,
         one_bedroom_asking_rent_unit_wt = one_bedroom_asking_rent_unit_wt/one_bed_tot,
         two_bedroom_asking_rent_unit_wt = two_bedroom_asking_rent_unit_wt/two_bed_tot,
         studio_asking_rent_unit_wt = studio_asking_rent_unit_wt/studio_tot,
         avg_unit_sf_wt = avg_unit_sf_wt/units_tot,
         one_bedroom_avg_sf_wt = one_bedroom_avg_sf_wt/one_bed_tot,
         two_bedroom_avg_sf_wt = two_bedroom_avg_sf_wt/two_bed_tot,
         studio_avg_sf_wt = studio_avg_sf_wt/studio_tot,
         avg_effective_unit_wt = avg_effective_unit_wt/units_tot,
         one_bedroom_effective_rent_unit_wt = one_bedroom_effective_rent_unit_wt/one_bed_tot,
         two_bedroom_effective_rent_unit_wt = two_bedroom_effective_rent_unit_wt/two_bed_tot,
         studio_effective_rent_unit_wt = studio_effective_rent_unit_wt/studio_tot,
         avg_effective_sf_wt = avg_effective_sf_wt/units_tot,
         one_bedroom_effective_rent_sf_wt = one_bedroom_effective_rent_sf_wt/one_bed_tot,
         two_bedroom_effective_rent_sf_wt = two_bedroom_effective_rent_sf_wt/two_bed_tot,
         studio_effective_rent_sf_wt = studio_effective_rent_sf_wt/studio_tot) %>%
  summarise(avg_asking_unit_pct_wt = sum(avg_asking_unit_pct_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_pct_wt = sum(one_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            two_bedroom_asking_rent_unit_pct_wt = sum(two_bedroom_asking_rent_unit_pct_wt, na.rm = TRUE),
            studio_asking_rent_unit_pct_wt = sum(studio_asking_rent_unit_pct_wt, na.rm = TRUE),
            avg_unit_sf_pct_wt = sum(avg_unit_sf_pct_wt, na.rm = TRUE),
            one_bedroom_avg_sf_pct_wt = sum(one_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            two_bedroom_avg_sf_pct_wt = sum(two_bedroom_avg_sf_pct_wt, na.rm = TRUE),
            studio_avg_sf_pct_wt = sum(studio_avg_sf_pct_wt, na.rm=TRUE),
            avg_effective_unit_pct_wt = sum(avg_effective_unit_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_unit_pct_wt = sum(one_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_unit_pct_wt = sum(two_bedroom_effective_rent_unit_pct_wt, na.rm=TRUE),
            studio_effective_rent_unit_pct_wt = sum(studio_effective_rent_unit_pct_wt, na.rm=TRUE),
            avg_effective_sf_pct_wt = sum(avg_effective_sf_pct_wt, na.rm=TRUE),
            one_bedroom_effective_rent_sf_pct_wt = sum(one_bedroom_effective_rent_sf_pct_wt, na.rm=TRUE),
            two_bedroom_effective_rent_sf_pct_wt = sum(two_bedroom_effective_rent_sf_pct_wt, na.rm =TRUE),
            studio_effective_rent_sf_pct_wt = sum(studio_effective_rent_sf_pct_wt, na.rm=TRUE),
            avg_asking_unit_wt = sum(avg_asking_unit_wt, na.rm = TRUE),
            one_bedroom_asking_rent_unit_wt = sum(one_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            two_bedroom_asking_rent_unit_wt = sum(two_bedroom_asking_rent_unit_wt, na.rm=TRUE),
            studio_asking_rent_unit_wt = sum(studio_asking_rent_unit_wt, na.rm = TRUE),
            avg_unit_sf_wt = sum(avg_unit_sf_wt, na.rm = TRUE),
            one_bedroom_avg_sf_wt = sum(one_bedroom_avg_sf_wt, na.rm = TRUE),
            two_bedroom_avg_sf_wt = sum(two_bedroom_avg_sf_wt, na.rm = TRUE),
            studio_avg_sf_wt = sum(studio_avg_sf_wt, na.rm = TRUE),
            avg_effective_unit_wt = sum(avg_effective_unit_wt, na.rm = TRUE),
            one_bedroom_effective_rent_unit_wt = sum(one_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            two_bedroom_effective_rent_unit_wt = sum(two_bedroom_effective_rent_unit_wt, na.rm = TRUE),
            studio_effective_rent_unit_wt = sum(studio_effective_rent_unit_wt, na.rm = TRUE),
            avg_effective_sf_wt = sum(avg_effective_sf_wt, na.rm = TRUE),
            one_bedroom_effective_rent_sf_wt = sum(one_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            two_bedroom_effective_rent_sf_wt = sum(two_bedroom_effective_rent_sf_wt, na.rm = TRUE),
            studio_effective_rent_sf_wt = sum(studio_effective_rent_sf_wt, na.rm = TRUE)) %>% ungroup()


names(market_group) <- paste0(names(market_group), "_market")



#----



# Merge and clean
# First select necessary variables from comps_ts, this will be data, ids, submarket, market, rpm, owner, team
comps_ts %<>% select(costar_id_prop, costar_id_comp, team, date_props, avg_asking_sf_props, avg_asking_unit_props, avg_effective_sf_props, avg_effective_unit_props,
                     building_class_props, market_name_props, number_of_1_bedrooms_units_props, number_of_2_bedrooms_units_props, 
                     number_of_3_bedrooms_units_props, number_of_4_bedrooms_units_props, number_of_studios_units_props, number_of_units_props,
                     one_bedroom_asking_rent_sf_props, one_bedroom_asking_rent_unit_props, one_bedroom_asking_rent_sf_props, one_bedroom_effective_rent_sf_props,
                     one_bedroom_effective_rent_unit_props, owner_name_props, property_name_props, studio_asking_rent_sf_props, studio_asking_rent_unit_props,
                     studio_avg_sf_props, studio_effective_rent_sf_props, studio_effective_rent_unit_props, submarket_name_props, three_bedroom_asking_rent_sf_props,
                     three_bedroom_asking_rent_unit_props, three_bedroom_asking_rent_sf_props, three_bedroom_effective_rent_sf_props, three_bedroom_effective_rent_unit_props,
                     two_bedroom_asking_rent_sf_props, two_bedroom_asking_rent_unit_props, two_bedroom_asking_rent_sf_props, two_bedroom_effective_rent_sf_props, 
                     two_bedroom_effective_rent_unit_props, 
                     avg_asking_unit_pct_props, one_bedroom_asking_rent_unit_pct_props, two_bedroom_asking_rent_unit_pct_props, 
                     studio_asking_rent_unit_pct_props, avg_asking_sf_pct_props, one_bedroom_asking_rent_sf_pct_props, two_bedroom_asking_rent_sf_pct_props, 
                     studio_asking_rent_sf_pct_props, avg_effective_unit_pct_props, one_bedroom_effective_rent_unit_pct_props, 
                     two_bedroom_effective_rent_unit_pct_props, studio_effective_rent_unit_pct_props, avg_effective_sf_pct_props, 
                     one_bedroom_effective_rent_sf_pct_props, two_bedroom_effective_rent_sf_pct_props, studio_effective_rent_sf_pct_props, 
                     year_built_props, latitude_props, longitude_props, avg_asking_sf_comps, avg_asking_unit_comps,
                     avg_effective_sf_comps, avg_effective_unit_comps, avg_unit_sf_comps, building_class_comps, 
                     number_of_1_bedrooms_units_comps, number_of_2_bedrooms_units_comps, number_of_3_bedrooms_units_comps, 
                     number_of_studios_units_comps, number_of_units_comps, one_bedroom_asking_rent_sf_comps,
                     one_bedroom_asking_rent_unit_comps, one_bedroom_asking_rent_sf_comps, 
                     one_bedroom_effective_rent_sf_comps, one_bedroom_effective_rent_unit_comps,
                     owner_name_comps, property_name_comps, studio_asking_rent_sf_comps, studio_asking_rent_unit_comps, studio_avg_sf_comps, 
                     studio_concessions_percent_comps, studio_effective_rent_sf_comps, studio_effective_rent_unit_comps, submarket_name_comps, 
                     three_bedroom_asking_rent_sf_comps, three_bedroom_asking_rent_unit_comps, three_bedroom_avg_sf_comps, three_bedroom_effective_rent_sf_comps, 
                     three_bedroom_effective_rent_unit_comps, two_bedroom_asking_rent_sf_comps, two_bedroom_asking_rent_unit_comps, two_bedroom_avg_sf_comps,
                     two_bedroom_concessions_percent_comps, two_bedroom_effective_rent_sf_comps, two_bedroom_effective_rent_unit_comps, year_built_comps, year_renovated_comps,
                     latitude_comps, longitude_comps, avg_asking_unit_pct_comps, one_bedroom_asking_rent_unit_pct_comps, two_bedroom_asking_rent_unit_pct_comps, 
                     studio_asking_rent_unit_pct_comps, avg_asking_sf_pct_comps, one_bedroom_asking_rent_sf_pct_comps, two_bedroom_asking_rent_sf_pct_comps, studio_asking_rent_sf_pct_comps, 
                     avg_effective_unit_pct_comps, one_bedroom_effective_rent_unit_pct_comps, two_bedroom_effective_rent_unit_pct_comps, studio_effective_rent_unit_pct_comps, 
                     two_bedroom_effective_rent_sf_pct_comps, studio_effective_rent_sf_pct_comps, avg_asking_unit_pct_wt_comps, 
                     rpm, essex_markets, ess_submarket)


#Merge with market

comps_ts$mark_date <- paste0(comps_ts$date_props, "|", comps_ts$essex_markets)
market_group$mark_date <- paste0(market_group$date_market, "|", market_group$essex_markets_market)
market_group %<>% select(-date_market, -essex_markets_market)
comps_ts <- merge(comps_ts, market_group, by = "mark_date")



#Merge with submarket

comps_ts$submark_date <- paste0(comps_ts$date_props, "|", comps_ts$ess_submarket)
submarket_group$submark_date <- paste0(submarket_group$date_submarket, "|", submarket_group$ess_submarket.y_submarket)
submarket_group %<>% select(-date_submarket, -ess_submarket.y_submarket)
comps_ts <- merge(comps_ts, submarket_group, by = "submark_date")



#Merge with Prop

comps_ts$cgroup_date <- paste0(comps_ts$date_props, "|", comps_ts$costar_id_prop)
comp_group$cgroup_date <- paste0(comp_group$date_props_cgroup, "|", comp_group$costar_id_prop_cgroup)
comp_group %<>% select(-date_props_cgroup, -costar_id_prop_cgroup)
comps_ts <- merge(comps_ts, comp_group, by = "cgroup_date")




comps_melt <- comps_ts %>% melt(id=c("submark_date","essex_markets", "mark_date", "costar_id_prop", 
                                     "costar_id_comp", "team", "submarket_name_props","submarket_name_comps",
                                     "building_class_props", "market_name_props",
                                     "date_props", "cgroup_date", "owner_name_props",
                                     "building_class_comps", "number_of_1_bedrooms_units_comps",
                                     "number_of_2_bedrooms_units_comps", "number_of_3_bedrooms_units_comps",
                                     "number_of_studios_units_comps",
                                     "owner_name_comps", "property_name_props", "property_name_comps",
                                     "year_built_props",
                                     "latitude_props", "longitude_props",  "number_of_units_comps", 
                                     "year_built_comps", "year_renovated_comps", "latitude_comps",
                                     "longitude_comps",
                                     "rpm",
                                     "essex_markets", "ess_submarket",
                                     "number_of_1_bedrooms_units_props", 
                                     "number_of_2_bedrooms_units_props", 
                                     "number_of_3_bedrooms_units_props",
                                     "number_of_4_bedrooms_units_props", 
                                     "number_of_studios_units_props",                   
                                     "number_of_units_props"))



# break inot different columns
comps_melt_pct <- subset(comps_melt, grepl("pct", comps_melt$variable))

comps_melt_raw <- subset(comps_melt, !grepl("pct", comps_melt$variable))

# comps_melt_props_pct <- subset(comps_melt_pct, grepl("props", comps_melt_pct$variable))
# 
# comps_melt_other_pct <- subset(comps_melt_pct, !grepl("props", comps_melt_pct$variable))
# 
# 
# comps_melt_props_raw <- subset(comps_melt_raw, grepl("props", comps_melt_raw$variable))
# 
# comps_melt_other_raw <- subset(comps_melt_raw, !grepl("props", comps_melt_raw$variable))


#create unique id
# Needs date | prop_id | comp_id | variable

#variable clean
comps_melt_pct$var_clean <- gsub("_pct", "", comps_melt_pct$variable)

comps_melt_pct$merge_var <- paste0(comps_melt_pct$date_props, "|", comps_melt_pct$costar_id_prop, "|", comps_melt_pct$costar_id_comp, "|", comps_melt_pct$var_clean)


comps_melt_raw$merge_var <- paste0(comps_melt_raw$date_props, "|", comps_melt_raw$costar_id_prop, "|", comps_melt_raw$costar_id_comp, "|", comps_melt_raw$variable)
# limit to only necessary vars
comps_melt_pct %<>% select(merge_var, value_pct=value)





#comps_merge <- merge(comps_melt_raw, comps_melt_pct, by = "merge_var", all.x = TRUE) # old way

comps_merge <-left_join(comps_melt_raw, comps_melt_pct, by=c("merge_var"))


comps_merge$Group <- ifelse(grepl("cgroup", comps_merge$variable), "Comps (Show Avg.)", 
                            ifelse(grepl("props", comps_merge$variable), "ESS. Property",
                            ifelse(grepl("_market", comps_merge$variable), "Market",
                            ifelse(grepl("_submark", comps_merge$variable), "Submarket", "Comps (Show Each)"))))


comps_merge$group_chart <- ifelse(comps_merge$Group=="Comps (Show Each)", as.character(comps_merge$property_name_comps), comps_merge$Group)

comps_merge$SF <- ifelse(grepl("_sf", comps_merge$variable), "Yes", "No")



comps_merge$Bedroom <- ifelse(grepl("studio", comps_merge$variable), "Studio", 
                            ifelse(grepl("one_", comps_merge$variable), "1 BR",
                                   ifelse(grepl("two_", comps_merge$variable), "2 BR",
                                   ifelse(grepl("three_", comps_merge$variable), "3 BR", "All"))))


comps_merge$`Rent Type` <- ifelse(grepl("effective", comps_merge$variable), "Effective", "Asking")


comps_merge %<>% select(date_props, essex_markets, costar_id_prop, costar_id_comp, team, submarket_name_props,
                        market_name_props, owner_name_props, building_class_comps, number_of_1_bedrooms_units_comps,
                        number_of_2_bedrooms_units_comps, number_of_3_bedrooms_units_comps, number_of_studios_units_comps,
                        owner_name_comps, property_name_props, property_name_comps, year_built_props, latitude_props,
                        longitude_props, number_of_units_comps, year_built_comps, year_renovated_comps, latitude_comps, longitude_comps,
                        rpm, essex_market = essex_markets.1, ess_submarket, number_of_1_bedrooms_units_props, 
                        number_of_2_bedrooms_units_props, number_of_3_bedrooms_units_props, number_of_4_bedrooms_units_props,
                        number_of_studios_units_props, number_of_units_props, variable, value, value_pct,
                        Group, SF, Bedroom, `Rent Type`, group_chart)



fwrite(comps_merge, "C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/power_bi_rent_data/melted_comp_ts.csv", na = "")


# Latitude and longitude data

time <- as.character(Sys.time())
time <- substr(time, 1, 10)
time_x <- time

map <- comps_merge %>% select(costar_id_prop, costar_id_comp, team, latitude_props, longitude_props, latitude_comps, longitude_comps, date_props, property_name_comps, property_name_props) %>% # nececssary variable
  filter(date_props %in% time_x) # filter for current date

map$uniq <- paste0(map$costar_id_prop, "|", map$costar_id_comp) # create unique id to remove duplicates
  
map <- map[!duplicated(map$uniq), ] #remove duplicate


# Now we need to select just props the goal is to add them to one unified lat_long column
props <- map %>% select(costar_id_prop, latitude = latitude_props, longitude = longitude_props, property_name = property_name_props)# nececssary variable
  
props <- props[!duplicated(props$costar_id_prop), ] #remove duplicates

props$team = "Subject"

props$costar_id_comp = props$costar_id_prop


map %<>% select(costar_id_prop, costar_id_comp, team, latitude = latitude_comps, longitude = longitude_comps, property_name = property_name_comps)

map <- rbind(map, props)
  
  
write.csv(map, "C:/Users/kplank/Essex Property Trust, Inc/Costar Data - Documents/power_bi_rent_data/property_comp_map_pbi.csv", na = "" )  

  
  
  


