########################################################################### LIBRARY ###########################################################################
library(terra)
library(tidyverse)
library(ggplot2)
library(tmap)
library(rgdal)
library(landscapemetrics)
library(landscapetools)
library(sf)
library(tigris)
library(FedData)
library(wesanderson)
library(patchwork)
library(lubridate)


rm(list=ls())


########################################################################### NOTES #############################################################################

# key questions:
# 1.) how to convert driver data to NLCD_epoch_year data?
# 2.) what to do about n? closely related to timeframe
#     a.) somehow use the NLCD data with an n = 7 or n = 8
#     b.) use the Dubertret et al. 2022 dataset?
#       i.) does the total urban area match the NLCD total urban area? we know that the subtypes conversions within total urban are drastically different.
#       ii.) 
# 3.) structure of the SEM
#     a.) should housing demand be included in the model? 
#       i.) there is theoretical grounds for including a term like this
#     b.) solutions: read the SEM model development page, start generating subunits of the model with n = 21 data.
# 4.) scope of the SEM
#     a.) should I just include construction GDP --> Residential Construction --> urban LUCC? Is that simple enough? Or do we need more detailed drivers of residential construction?


# TODO
# graphs connecting residential completions to NLCD LUCC data
# data for developmental loans, mortgage prices for buyers, migrations to phoenix
# data for housing demand?



########################################################################### FUNCTIONS #########################################################################

Clean_Labor_Data <- function(dataframe) {
  
  # gets the year from the dataframe title
  title_string <- deparse(substitute(dataframe)) 
  title_string <- str_split(title_string, "_", n = 2)
  year_string <- title_string[[1]][[2]]
  
  df <- dataframe %>%
    rename_with(str_to_lower) %>%
    mutate(occ_title = str_to_lower(occ_title)) %>%
    select(area_title, occ_code, occ_title, tot_emp, h_mean, a_mean) %>%
    filter(str_detect(area_title, "Phoenix|Tucson")) %>%
    filter(str_detect(occ_title, "construction")) %>% 
    mutate(area_title = ifelse(str_detect(area_title, "Phoenix"), "Phoenix MSA", "Tucson MSA")) %>%  
    add_column(year = year_string)
  
  df <- as.data.frame(df)
  
  return(df)
}

########################################################################### DATA PREP #########################################################################

pal2 <- c("#FD6467", "#5B1A18")

########################################################################### EDA #########################################################################


#### . #####
#### labor #####
#### @total construction laborers (OEWS dataset) ####

# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/labor'

# OEWS DATASET from Bureau of Labor Statistics
# The only way to retrieve all of the data is looking here: https://www.bls.gov/oes/tables.htm
# data retrieved from: https://www.bls.gov/eag/eag.az.htm
# about the data: https://www.bls.gov/oes/oes_ques.htm#overview
OEWS_2001 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2001.csv')
OEWS_2002 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2002.csv')
OEWS_2003 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2003.csv')
OEWS_2004 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2004.csv')
OEWS_2005 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2005.csv')
OEWS_2006 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2006.csv')
OEWS_2007 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2007.csv')
OEWS_2008 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2008.csv')
OEWS_2009 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2009.csv')
OEWS_2010 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2010.csv')
OEWS_2011 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2011.csv')
OEWS_2012 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2012.csv')
OEWS_2013 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2013.csv')
OEWS_2014 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2014.csv')
OEWS_2015 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2015.csv')
OEWS_2016 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2016.csv')
OEWS_2017 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2017.csv')
OEWS_2018 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2018.csv')
OEWS_2019 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2019.csv')
OEWS_2020 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2020.csv')
OEWS_2021 <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/OEWS_data/OEWS_2021.csv')

OEWS_2001 <- Clean_Labor_Data(OEWS_2001)
OEWS_2002 <- Clean_Labor_Data(OEWS_2002)
OEWS_2003 <- Clean_Labor_Data(OEWS_2003)
OEWS_2004 <- Clean_Labor_Data(OEWS_2004)
OEWS_2005 <- Clean_Labor_Data(OEWS_2005)
OEWS_2006 <- Clean_Labor_Data(OEWS_2006)
OEWS_2007 <- Clean_Labor_Data(OEWS_2007)
OEWS_2008 <- Clean_Labor_Data(OEWS_2008)
OEWS_2009 <- Clean_Labor_Data(OEWS_2009)
OEWS_2010 <- Clean_Labor_Data(OEWS_2010)
OEWS_2011 <- Clean_Labor_Data(OEWS_2011)
OEWS_2012 <- Clean_Labor_Data(OEWS_2012)
OEWS_2013 <- Clean_Labor_Data(OEWS_2013)
OEWS_2014 <- Clean_Labor_Data(OEWS_2014)
OEWS_2015 <- Clean_Labor_Data(OEWS_2015)
OEWS_2016 <- Clean_Labor_Data(OEWS_2016)
OEWS_2017 <- Clean_Labor_Data(OEWS_2017)
OEWS_2018 <- Clean_Labor_Data(OEWS_2018)
OEWS_2019 <- Clean_Labor_Data(OEWS_2019)
OEWS_2020 <- Clean_Labor_Data(OEWS_2020)
OEWS_2021 <- Clean_Labor_Data(OEWS_2021)

OEWS_combined <- rbind(OEWS_2001, OEWS_2002, OEWS_2003, OEWS_2004, OEWS_2005, OEWS_2006, OEWS_2007, OEWS_2008, OEWS_2009, OEWS_2010, OEWS_2011, OEWS_2012, OEWS_2013, OEWS_2014, OEWS_2015, OEWS_2016, OEWS_2017, OEWS_2018, OEWS_2019, OEWS_2020, OEWS_2021) %>% 
  mutate(tot_emp = gsub(",","", tot_emp)) %>% 
  mutate(tot_emp = as.numeric(tot_emp)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(area_title = as.factor(area_title))

# total construction workers OEWS
construction_laborers_OEWS <- OEWS_combined %>% 
  filter(occ_title == "construction laborers") %>% 
  ggplot(aes(x = year, y = tot_emp, group = area_title, color = area_title)) + geom_line() + geom_point() + 
  ylab("Total Construction Laborers") + scale_color_manual(values = pal2) + scale_x_discrete(breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

construction_laborers_OEWS
ggsave(path = landscape_driver_path, filename = "construction_laborers_OEWS.png", width = 6, height = 3.5, scale = 1.5)
# these data do not match the 'economy at a glance' data from bureau of labor statistics

#### @total construction wages (OEWS dataset) ####
a_mean_OEWS <- OEWS_combined %>% 
  filter(occ_title == "construction laborers") %>% 
  ggplot(aes(x = year, y = a_mean, group = area_title, color = area_title)) + geom_line() + geom_point() + 
  ylab("Annual Mean Earnings") + scale_color_manual(values = pal2) + scale_x_discrete(breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)) +
  xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

a_mean_OEWS
ggsave(path = landscape_driver_path, filename = "a_mean_OEWS.png", width = 6, height = 3.5, scale = 1.5)



# Notes
# are these data adjusted for inflation? probably not.
# as construction wages go up, so should housing prices, which negatively affects residential construction
# not sure why the data are different than the BLS data

#### @total construction laborers (EAG dataset) #### 
# economy at a glance: https://www.bls.gov/regions/west/az_phoenix_msa.htm
# notes about the data: https://www.bls.gov/eag/abouteag.htm#Anomalies
EAG_Phoenix_MSA <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/total_construction_workers_PMA_BLS.csv', skip = 12)

EAG_Phoneix_MSA_cleaned <- EAG_Phoenix_MSA %>% 
  rename_with(str_to_lower) %>%
  select(may, year) %>%
  rename(construction_jobs_in_may = may) %>% 
  add_column(area_title = "Phoenix MSA")

EAG_Tucson_MSA <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/labor/total_construction_workers_TMA_BLS.csv', skip = 12)

EAG_combined <- EAG_Tucson_MSA %>% 
  rename_with(str_to_lower) %>%
  select(may, year) %>%
  rename(construction_jobs_in_may = may) %>% 
  add_column(area_title = "Tucson MSA") %>% 
  rbind(., EAG_Phoneix_MSA_cleaned) %>% 
  mutate(area_title = as.factor(area_title)) %>% 
  mutate(year = as.numeric(year))
           
construction_laborers_EAG <- EAG_combined %>% 
  filter(year > 2000 & year < 2022) %>% 
  ggplot(aes(x = year, y = construction_jobs_in_may, group = area_title, color = area_title)) + geom_line() + geom_point() + scale_y_continuous(limits = c(0,200), breaks = c(50, 100, 150, 200)) + 
  ylab("Total Construction Laborers") + scale_color_manual(values = pal2) + scale_x_continuous(breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

construction_laborers_EAG
ggsave(path = landscape_driver_path, filename = "construction_laborers_EAG.png", width = 6, height = 3.5, scale = 1.5)

construction_laborers_EAG_NLCD <- EAG_combined %>%
  filter(year > 2000 & year < 2020) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years, area_title) %>% 
  summarise(mean = mean(construction_jobs_in_may)) %>% 
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = mean, group = area_title, color = area_title)) + geom_line() + geom_point() + scale_y_continuous(limits = c(0,200), breaks = c(50, 100, 150, 200)) + 
  ylab("Total Construction Laborers") + scale_color_manual(values = pal2) + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) +
  xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
  
construction_laborers_EAG_NLCD
ggsave(path = landscape_driver_path, filename = "construction_laborers_EAG_NLCD.png", width = 6, height = 3.5, scale = 1.5)

####  @combined figures ####
(a_mean_OEWS) + (construction_laborers_EAG) + plot_layout(ncol = 1, nrow = 2) & plot_annotation(title = "Labor Node") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "labor_combined.png", width = 12, height = 12, scale = 1.5)

#### . #####
#### residential construction #####

# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/construction'

#### @building permits (OEWS dataset) ####

# Data gathered from the building permits survey from the census bureau
# source (historic data): https://www.census.gov/construction/bps/historical_data/index.html
# source: https://www.census.gov/construction/bps/msaannual.html
building_permits_CB <- read_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/residential_construction/building_permits.csv')

building_permits <- building_permits_CB %>%
  mutate(area_title = as.factor(area_title)) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year, y = annual_permits, group = area_title, color = area_title)) + geom_line() + geom_point() + 
  ylab("Building Permits") + scale_color_manual(values = pal2, name = "Statistical Area") + scale_y_continuous(limits = c(0,70000)) +
  scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

building_permits


building_permits_NLCD <- building_permits_CB %>%
  filter(year > 2000 & year < 2020) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years, area_title) %>% 
  summarise(mean = mean(annual_permits)) %>% 
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = mean, group = area_title, color = area_title)) + geom_line() + geom_point() + scale_y_continuous(limits = c(0,70000)) +
  ylab("Building permits") + scale_color_manual(values = pal2) + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) +
  xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

building_permits_NLCD

#### @residential completions ####
# data retrieved here: https://azmag.gov/Programs/Maps-and-Data/Land-Use-and-Real-Estate/Housing-Data-Explorer
# no metadata info
residential_completions_MAG_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/MAG_residential_completions.csv'
residential_completions_MAG <- read_csv(residential_completions_MAG_path)
# these data are just estimates from the MAG website.Not the actual data!!
# what counts as a residential unit?' ''

residential_completions_fig <- residential_completions_MAG %>% 
  filter(year > 2000) %>% 
  mutate(year = as.numeric(year)) %>%  
  ggplot(aes(x = year, y = total_residential_completions, group = 1)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467") + 
  ylab("Total Residential Unit Completions Maricopa County") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,50000), breaks = c(10000,20000,30000,40000,50000)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

residential_completions_fig

residential_completions_NLCD <- residential_completions_MAG %>% 
  filter(year > 2000 & year < 2020) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years) %>% 
  summarise(mean = mean(total_residential_completions)) %>% 
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = mean, group = 1)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467") + scale_y_continuous(limits = c(0,50000), breaks = c(10000,20000,30000,40000,50000)) +
  ylab("Total Residential Completions") + scale_color_manual(values = pal2) + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) +
  xlab("NLCD Epoch Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
  

residential_completions_NLCD

#### @construction GDP ####
# GDP Bureau of Economic Analysis path
# source: https://apps.bea.gov/itable/?ReqID=70&step=1&acrdn=5
real_GDP_BEA_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/residential_construction/BEA_real_GDP.csv'
real_GDP_BEA <- read_csv(real_GDP_BEA_path)

real_total_GDP <- real_GDP_BEA %>% 
  filter(county != "Arizona" & county != "Pinal") %>% 
  filter(line_code == 11) %>% 
  gather(year, value, '2001':'2020') %>% 
  mutate(county = factor(county, levels = c("Maricopa", "Pima"))) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-line_code) %>% 
  ggplot(aes(x = year, y = value, color = county, group = county)) + geom_line() + geom_point() + 
  ylab("Construction Real GDP (thousand USD)") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,25000000)) + xlab("Year") + scale_color_manual(values = pal2, name = "County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

real_total_GDP

real_total_GDP_NLCD <- real_GDP_BEA %>% 
  filter(county != "Arizona" & county != "Pinal") %>% 
  filter(line_code == 11) %>% 
  gather(year, value, '2001':'2020') %>% 
  mutate(county = factor(county, levels = c("Maricopa", "Pima"))) %>% 
  select(-line_code) %>%
  filter(year > 2000 & year < 2020) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  mutate(value = as.numeric(value)) %>% 
  group_by(NLCD_epoch_years, county) %>%
  summarise(mean = mean(value)) %>%
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>%
  mutate(county = as.factor(county)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = mean, color = county, group = county)) + geom_line() + geom_point() + 
  ylab("Construction Real GDP (thousand USD)") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_y_continuous(limits = c(0,25000000)) + xlab("Year") + scale_color_manual(values = pal2, name = "County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

real_total_GDP_NLCD

####  @residential fixed investment ####
# FRED residential fixed investment 
# source: https://fred.stlouisfed.org/series/PRFIC1
# this is for the entire United States? Not specific to Arizona
residential_fixed_investment_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/residential_construction/residential_fixed_investment_FRED.csv'
residential_fixed_investment_FRED <- read_csv(residential_fixed_investment_path)

str(residential_fixed_investment_FRED)

rfi <- residential_fixed_investment_FRED %>% 
  mutate(date = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(residential_fixed_investment = mean(rfi)) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = residential_fixed_investment, group = 1)) + geom_line(color = "#D67236") + geom_point(color = "#D67236") + 
  ylab("Real Private Residential Fixed Investment") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,1000)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

rfi

rfi_NLCD <- residential_fixed_investment_FRED %>% 
  mutate(date = as.Date(DATE, "%m/%d/%y")) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(rfi = mean(rfi)) %>%
  filter(year > 2000 & year < 2020) %>%
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  mutate(rfi = as.numeric(rfi)) %>%
  group_by(NLCD_epoch_years) %>%
  summarise(mean = mean(rfi)) %>%
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>%
  ggplot(aes(x = NLCD_epoch_years, y = mean, group = 1)) + geom_line(color = "#D67236") + geom_point(color = "#D67236") + 
  ylab("Real Private Residential Fixed Investment") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_y_continuous(limits = c(0,1000)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

rfi_NLCD

#### @combined figures ####

# annual
(building_permits) + (residential_completions_fig) + (real_total_GDP) + (rfi) + plot_layout(ncol = 1, nrow = 4) & plot_annotation(title = "Construction Node") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "residential_combined.png", width = 12, height = 12, scale = 1.5)

# NLCD epoch years
(building_permits_NLCD) + (residential_completions_NLCD) + (real_total_GDP_NLCD) + (rfi_NLCD) + plot_layout(ncol = 1, nrow = 4) & plot_annotation(title = "Construction Node") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "residential_combined_NLCD.png", width = 6, height = 12, scale = 1.5)

##### . ####
#### loan standards ####

# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/loan_standards'

#### Federal reserve ####
# source: https://www.federalreserve.gov/econres/notes/feds-notes/an-aggregate-view-of-bank-lending-standards-and-demand-20200504.html
fed_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/loan_standards/federal_reserve.csv'
fed <- read_csv(fed_path)

# notes
# these data are based on the Senior Loan Officer Opinion Survey on Bank lending Practices (SLOOS), which provides information about the supply of, and demand for, bank credit in the United States
# this is C&I (commercial and industrial)
# description of the data on the source.
# the data provides unweighted lending standards for all banks and unweighted lending standards for large banks. The weighted data may be available somewhere, but I do not know where.
# negative is "loosening" of lending standards, positive is "tightening standards"

lending_standards_large_banks <- fed %>% 
  filter(year > 2000) %>% 
  group_by(year) %>% 
  summarise(unweighted_all_banks = mean(unweighted_large_banks)) %>% 
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = unweighted_all_banks)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467") + 
  ylab("Lending Standards Large Banks") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(-60,60), breaks = c(-60,-30,0,30,60)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
  
lending_standards_large_banks

lending_standards_large_banks_NLCD <- fed %>% 
  filter(year > 2000 & year < 2020) %>% 
  group_by(year) %>% 
  summarise(large_bank_standards = mean(unweighted_large_banks)) %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years) %>% 
  summarise(large_bank_standards = mean(large_bank_standards)) %>% 
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = large_bank_standards, group =1)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467")+ xlab("NLCD Epoch Year") + 
  scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019))+ 
  ylab("Lending Standards Large Banks") + 
  scale_y_continuous(limits = c(-60,60)) + scale_color_manual(values = pal2, name = "MSA") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

lending_standards_large_banks_NLCD

# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/loan_standards'

#### FDIC quarterly banking profile ####
# source: https://fred.stlouisfed.org/series/QBPBSTASLNREALCONDEV
# original source: FDIC quarterly banking profile: https://www.fdic.gov/analysis/quarterly-banking-profile/qbp/index.html
# loans secured by real estate: construction and development
FDIC_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/loan_standards/real_estate_loans.csv'
FDIC <- read_csv(FDIC_path)
# notes
# these data are national data; not specific to Arizona

real_estate_loans_fig <- FDIC %>% 
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  mutate(year = year(date)) %>%
  filter(year > 2000 & year < 2022) %>%
  group_by(year) %>% 
  summarise(average_annual_loans = mean(real_estate_loans)) %>% 
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = average_annual_loans)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467") + 
  ylab("Loans secured by real estate nationally (USD)") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(100000,700000), breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
  
real_estate_loans_fig

real_estate_loans_NLCD <- FDIC %>% 
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  mutate(year = year(date)) %>%
  filter(year > 2000 & year < 2020) %>%
  group_by(year) %>% 
  summarise(average_annual_loans = mean(real_estate_loans)) %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years) %>% 
  summarise(average_annual_loans = mean(average_annual_loans)) %>% 
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>% 
  ggplot(aes(x = NLCD_epoch_years, y = average_annual_loans, group = 1)) + geom_line(color = "#FD6467") + geom_point(color = "#FD6467") + 
  ylab("Loans secured by real estate nationally (USD)") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_y_continuous(limits = c(100000,700000), breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000)) + xlab("NLCD Epoch Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

real_estate_loans_NLCD

##### . ####
#### housing prices ####
# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/housing_prices'

#### @FHFA HPI ####
# HPI statistical areas FHFA
# source: https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx#qpo
HPI_SA_FHFA_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/housing_prices/FHFA_HPI_PO_metro.csv'
HPI_SA_FHFA <- read_csv(HPI_SA_FHFA_path)
# these HPI are estimated using sales price data
# there is little difference between seasonally- and non-seasonally adjusted data.

# HPI not seasonally adjusted statistical areas FHFA
HPI_NSA_SA_FHFA <- HPI_SA_FHFA %>% 
  filter(metro_name == "Phoenix-Mesa-Chandler, AZ" | metro_name == "Tucson, AZ") %>%
  filter(year > 2000 & year < 2022) %>%
  group_by(year, metro_name) %>% 
  summarise(yearly_nsa_average = mean(index_nsa)) %>% 
  mutate(metro_name = factor(metro_name, levels = c("Phoenix-Mesa-Chandler, AZ", "Tucson, AZ"))) %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(yearly_nsa_average = as.numeric(yearly_nsa_average)) %>% 
  ggplot(aes(x = year, y = yearly_nsa_average, color = metro_name, group = metro_name)) + geom_line() + geom_point() + 
  ylab("HPI not seasonally adjusted") + xlab("Year") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,1000)) + scale_color_manual(values = pal2, name = "MSA") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

HPI_NSA_SA_FHFA
ggsave(path = landscape_driver_path, filename = "HPI_NSA_SA_FHFA.png", width = 6, height = 3.5, scale = 1.5)

# HPI seasonally adjusted statistical areas FHFA
HPI_SA_SA_FHFA <- HPI_SA_FHFA %>% 
  filter(metro_name == "Phoenix-Mesa-Chandler, AZ" | metro_name == "Tucson, AZ") %>%
  filter(year > 2000 & year < 2022) %>%
  group_by(year, metro_name) %>% 
  summarise(yearly_sa_average = mean(index_sa)) %>% 
  mutate(metro_name = factor(metro_name, levels = c("Phoenix-Mesa-Chandler, AZ", "Tucson, AZ"))) %>% 
  mutate(year = as.numeric(year)) %>%
  mutate(yearly_sa_average = as.numeric(yearly_sa_average)) %>% 
  ggplot(aes(x = year, y = yearly_sa_average, color = metro_name, group = metro_name)) + geom_line() + geom_point() + 
  ylab("Housing Price Index (HPI) seasonally adjusted") + xlab("Year") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,1000)) + scale_color_manual(values = pal2, name = "MSA") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

HPI_SA_SA_FHFA
ggsave(path = landscape_driver_path, filename = "HPI_SA_SA_FHFA.png", width = 6, height = 3.5, scale = 1.5)

# HPI seasonally adjusted statistical areas FHFA
HPI_SA_SA_FHFA_NLCD <- HPI_SA_FHFA %>% 
  filter(metro_name == "Phoenix-Mesa-Chandler, AZ" | metro_name == "Tucson, AZ") %>%
  filter(year > 2000 & year < 2020) %>%
  group_by(year, metro_name) %>% 
  summarise(yearly_sa_average = mean(index_sa)) %>% 
  mutate(metro_name = factor(metro_name, levels = c("Phoenix-Mesa-Chandler, AZ", "Tucson, AZ"))) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  mutate(yearly_sa_average = as.numeric(yearly_sa_average)) %>%
  group_by(NLCD_epoch_years, metro_name) %>%
  summarise(mean = mean(yearly_sa_average)) %>%
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>%
  ggplot(aes(x = NLCD_epoch_years, y = mean, color = metro_name, group = metro_name)) + geom_line() + geom_point() + 
  ylab("Housing Price Index (HPI) seasonally adjusted") + xlab("NLCD Epoch Year") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_y_continuous(limits = c(0,1000)) + scale_color_manual(values = pal2, name = "MSA") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

HPI_SA_SA_FHFA_NLCD
ggsave(path = landscape_driver_path, filename = "HPI_SA_SA_FHFA.png", width = 6, height = 3.5, scale = 1.5)
  

##### . ####
#### mortgage rates ####
# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/interest_rates'

#### @Freddie Mac ####
# Freddie Mac mortgage rates
# source: https://www.freddiemac.com/pmms
interest_rates_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/interest_rates/mortgage_rates_FM.csv'
interest_rates_FM <- read_csv(interest_rates_path)

interst_rates_FM <- interest_rates_FM %>% 
  mutate(date = as.Date(DATE, "%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  filter(year > 2000 & year < 2022) %>% 
  group_by(year) %>%
  summarise(average_annual_mortgage = mean(thirty_yr_FRM)) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = average_annual_mortgage, group = 1)) + geom_line(color = "#D67236") + geom_point(color = "#D67236") + 
  ylab("30 year Freddie Mac FRM") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_y_continuous(limits = c(0,10)) + xlab("Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

interst_rates_FM

interst_rates_FM_NLCD <- interest_rates_FM %>% 
  mutate(date = as.Date(DATE, "%m/%d/%Y")) %>%
  mutate(year = year(date)) %>%
  filter(year > 2000 & year < 2020) %>% 
  group_by(year) %>%
  summarise(average_annual_mortgage = mean(thirty_yr_FRM)) %>%
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  mutate(average_annual_mortgage = as.numeric(average_annual_mortgage)) %>%
  group_by(NLCD_epoch_years) %>%
  summarise(mean = mean(average_annual_mortgage)) %>%
  mutate(NLCD_epoch_years = as.factor(NLCD_epoch_years)) %>%
  ggplot(aes(x = NLCD_epoch_years, y = mean, group = 1)) + geom_line(color = "#D67236") + geom_point(color = "#D67236") + 
  ylab("30 year Freddie Mac FRM") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_y_continuous(limits = c(0,10)) + xlab("NLCD Epoch Year") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

interst_rates_FM_NLCD
ggsave(path = landscape_driver_path, filename = "HPI_SA_SA_FHFA_NLCD.png", width = 6, height = 3.5, scale = 1.5)
# mortgage rates may not be that important. We are not capturing the rapid increase in mortgage rates in 2022. That is what many articles were referring to. Not sure how relevant mortgage rates are to housing demand for the rest of the period. Theoretically, they are important. 

##### . ####
#### population ####
# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/population'

#### @population ####
# population
# source: https://www.freddiemac.com/pmms
# Population estimates Bureau of Economic Analysis path
pop_est_BEA_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/landscape_drivers_data/population/BEA_population_income.csv'
pop_est_BEA <- read_csv(pop_est_BEA_path)
# these population estimate represent net migration correct? Inputs and Outputs are both part of these estimates.

population <- pop_est_BEA %>% 
  filter(description == "population") %>% 
  gather(year, value, '1969':'2020') %>% 
  filter(county != "Pinal") %>%
  filter(year > 2000) %>% 
  mutate(value = as.numeric(value)) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = value, color = county, group = county)) + geom_line() + geom_point() + 
  scale_y_continuous(limits = c(0,5000000), breaks = c(0,1000000,2000000,3000000,4000000,5000000), labels = c("0", "1", "2", "3","4","5")) +
  ylab("Total Population (million people)") + xlab("Year") + scale_x_continuous(breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  scale_color_manual(values = pal2)+ theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

population

population_difference_fig <- pop_est_BEA %>% 
  filter(description == "population") %>% 
  filter(county == "Maricopa") %>% 
  gather(year, value, '1969':'2020') %>% 
  filter(county != "Pinal") %>%
  filter(year > 1999) %>% 
  mutate(value = as.numeric(value)) %>%
  mutate(diff = value - lag(value)) %>%
  filter(year != 2000) %>% 
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = diff, group = 1)) + geom_line(color = "#46ACC8") + geom_point(color = "#46ACC8") +
  scale_y_continuous(limits = c(0,200000), breaks = c(0, 50000,100000,150000,200000)) +
  ylab("Net Migration/year (million people)") + xlab("Year") + scale_x_continuous(limits = c(2001,2021), breaks = c(2001,2002,2003, 2004, 2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
  
population_difference_fig

population_NLCD <- pop_est_BEA %>% 
  filter(description == "population") %>% 
  gather(year, value, '1969':'2020') %>% 
  filter(county != "Pinal") %>%
  filter(year == 2001 | year == 2004 | year == 2006 | year == 2008 | year == 2011 | year == 2013 | year == 2016 | year == 2019) %>%
  group_by(county) %>% 
  mutate(diff = value - lag(value)) %>%
  filter(year != 2001) %>% 
  ggplot(aes(x = year, y = diff, color = county, group = county)) + geom_line() + geom_point() + 
  scale_y_continuous(limits = c(0,500000), breaks = c(0,100000, 200000, 300000, 400000, 500000), labels = c("0", "1", "2", "3", "4", "5")) +
  ylab("Total Population (hundred thousand people)/ Epoch Year") + xlab("NLCD Epoch Year") + scale_x_discrete(breaks = c(2004, 2006, 2008,2011,2008,2013,2016,2019)) + 
  scale_color_manual(values = pal2) + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10, angle = 90),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))
 
population_NLCD


#### . ####
#### absorption rate ####

# housing sales vs. housing availability


#### . ####
#### LUCC visualizations ####

# open RDS files
m_LUCC<- readRDS("/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/RDS_files/maricopa_county_LUCC.rds")
p_LUCC<- readRDS("/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/RDS_files/pima_county_LUCC.rds")
urban_colors <- c("Developed, Open Space" = "#E6A0C4", "Developed, Low Intensity" = "#C6CDF7", "Developed, Medium Intensity" = "#D8A499", "Developed, High Intensity" = "#7294D4")

#### @maricopa county ####
maricopa_county_urban_sub_type_cover_change <- m_LUCC %>%
  filter(metric != "pland") %>% 
  group_by(class_id) %>% 
  mutate(diff = value - lag(value)) %>% 
  filter(year != 2001) %>% 
  ggplot(aes(x = year, y = diff, color = class_id, group = class_id)) + geom_line() + geom_point() + 
  scale_y_continuous(limits = c(-1800,14000), breaks = c(-1800,0,14000)) + scale_x_discrete() +
  scale_color_manual(values = urban_colors, name = "Urban Land Use") + ylab("Urban land use/land cover (ha)") + 
  xlab("NLCD Epoch Year") + ggtitle("Maricopa County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

maricopa_county_urban_sub_type_cover_change

# urban total cover change
maricopa_county_urban_cover_change <- m_LUCC %>%
  filter(metric != "pland") %>% 
  group_by(year) %>% 
  summarise(total_urban_cover = sum(value)) %>%
  mutate(diff = total_urban_cover - lag(total_urban_cover)) %>% 
  filter(year != 2001) %>% 
  ggplot(aes(x = year, y = diff, group = 1)) + geom_line(color = "#46ACC8") + geom_point(color = "#46ACC8") + 
  scale_y_continuous(limits = c(0,30000), breaks = c(0,30000)) + scale_x_discrete() +
  scale_color_manual(values = urban_colors, name = "Urban Land Use") + ylab("Urban land use/land cover (ha)") + 
  xlab("NLCD Epoch Year") + ggtitle("Maricopa County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

maricopa_county_urban_cover_change

####  @pima county ####
pima_county_urban_sub_type_cover_change <- p_LUCC %>%
  filter(metric != "pland") %>% 
  group_by(class_id) %>% 
  mutate(diff = value - lag(value)) %>% 
  filter(year != 2001) %>% 
  ggplot(aes(x = year, y = diff, color = class_id, group = class_id)) + geom_line() + geom_point() + 
  scale_y_continuous(limits = c(-1800,14000), breaks = c(-1800,0,14000)) + scale_x_discrete() +
  scale_color_manual(values = urban_colors, name = "Urban Land Use") + ylab("Urban land use/land cover (ha)") + 
  xlab("NLCD Epoch Year") + ggtitle("Pima County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

pima_county_urban_sub_type_cover_change

# urban total cover change
pima_county_urban_cover_change <- p_LUCC %>%
  filter(metric != "pland") %>% 
  group_by(year) %>% 
  summarise(total_urban_cover = sum(value)) %>%
  mutate(diff = total_urban_cover - lag(total_urban_cover)) %>% 
  filter(year != 2001) %>% 
  ggplot(aes(x = year, y = diff, group = 1)) + geom_line(color = "#46ACC8") + geom_point(color = "#46ACC8") + 
  scale_y_continuous(limits = c(0,30000), breaks = c(0,30000)) + scale_x_discrete() +
  scale_color_manual(values = urban_colors, name = "Urban Land Use") + ylab("Urban land use/land cover (ha)") + 
  xlab("NLCD Epoch Year") + ggtitle("Pima County") + theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 12),
    axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0), size = 10),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 10, l = 20), size = 10),
    strip.text.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(colour = NA),
    legend.key = element_rect(colour = "white", fill = NA),
    axis.line = element_line(colour = "black"))

pima_county_urban_cover_change

#### . #####
#### Prep SEM dataset ####

laborers <- EAG_Phoneix_MSA_cleaned %>% 
  select(-area_title) %>% 
  filter(year > 2000 & year < 2022) 

real_estate_loans <- FDIC %>% 
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  mutate(year = year(date)) %>%
  filter(year > 2000 & year < 2022) %>%
  group_by(year) %>% 
  summarise(loans_secured_by_real_estate = mean(real_estate_loans)) 

residential_completions <- residential_completions_MAG %>% 
  filter(year > 2000 & year < 2022)

# annual dataset
driver_dataset_annual <- laborers %>% 
  merge(., real_estate_loans, by = "year") %>%
  merge(., residential_completions, by = "year") %>% 
  rename(laborers = construction_jobs_in_may) %>%
  rename(residential_completions = total_residential_completions) %>%
  rename(real_estate_loans = loans_secured_by_real_estate)

saveRDS(driver_dataset_annual, '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/RDS_files/driver_dataset_annual.rds')

lucc<- m_LUCC %>% 
  select(-layer, -class, -level, -metric, -resolution, -study_extent) %>% 
  group_by(year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(class_id = "developed, total") %>% 
  mutate(layer = 1) %>%
  mutate(level = "class") %>%
  mutate(class = "2") %>%
  mutate(metric = "ca") %>%
  mutate(resolution = "30m") %>%
  mutate(study_extent  = "maricopa_county") %>%
  rbind(m_LUCC) %>%
  filter(metric == "ca") %>% 
  select(-layer, -class, -level, -metric, -resolution, -study_extent) %>%
  pivot_wider(names_from = c(class_id), values_from = value, year) %>% 
  filter(year != 2001) # gotten rid of the 2001 NLCD map completely. see key questions in notes above

# NLCD epoch year dataset (matches the NLCD data). see key questions in notes above
driver_dataset_NLCD <- driver_dataset_annual %>% 
  filter(year > 2000 & year < 2020) %>% 
  mutate(NLCD_epoch_years = case_when(year >= 2001 & year <= 2004 ~ 2004,
                                      year >= 2005 & year <= 2006 ~ 2006,
                                      year >= 2005 & year <= 2008 ~ 2008,
                                      year >= 2009 & year <= 2011 ~ 2011,
                                      year >= 2012 & year <= 2013 ~ 2013,
                                      year >= 2014 & year <= 2016 ~ 2016,
                                      year >= 2017 & year <= 2019 ~ 2019)) %>%
  group_by(NLCD_epoch_years) %>%
  summarise_at(c("construction_jobs_in_may", "loans_secured_by_real_estate", "total_residential_completions"), mean) %>%
  cbind(lucc) 

saveRDS(driver_dataset_NLCD, '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/data/RDS_files/driver_dataset_NLCD.rds')

##### . ####
#### linkages #####
# photos
landscape_driver_path <- '/Users/joshuagilman/Documents/code/PhD/dissertation_chap_2/stats/photos/static_photos/landscape_drivers_analysis/drivers/linkages'

#### @labor --> residential construction ####
(construction_laborers_EAG) + (residential_completions_fig) + plot_layout(ncol = 1, nrow = 2) & plot_annotation(title = "labor --> construction") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "labor_construction_linkage.png", width = 10, height = 12, scale = 1)

# epoch years
(construction_laborers_EAG_NLCD) + (residential_completions_NLCD) + (maricopa_county_urban_sub_type_cover_change) + plot_layout(ncol = 1, nrow = 3) & plot_annotation(title = "labor --> construction") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "labor_construction_linkage_epoch_years.png", width = 6, height = 12, scale = 1.5)

####  @residential loans secured --> residential construction ####
(lending_standards_large_banks) + (real_estate_loans_fig) + (residential_completions_fig) + plot_layout(ncol = 1, nrow = 3) & plot_annotation(title = "real estate loans --> construction") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "real_estate_loans_construction_linkage.png", width = 10, height = 12, scale = 1)

# epoch years
(lending_standards_large_banks_NLCD) + (real_estate_loans_NLCD) + (residential_completions_NLCD) + (maricopa_county_urban_sub_type_cover_change) + plot_layout(ncol = 1, nrow = 4) & plot_annotation(title = "real estate loans --> construction") & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "real_estate_loans_construction_linkage_epoch_years.png", width = 6, height = 12, scale = 1.5)

#### @ housing prices --> residential construction ####
(HPI_SA_SA_FHFA) + (residential_completions_fig) + plot_layout(ncol = 1, nrow = 2) & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "housing_prices_construction_linkage.png", width = 10, height = 12, scale = 1)

#### @ housing prices --> residential construction ####
(population_difference_fig) + (residential_completions_fig) + plot_layout(ncol = 1, nrow = 2) & theme(
  plot.title = element_text(size = 32),
  plot.tag = element_text(size = 22),
  plot.subtitle = element_text(size = 10))

ggsave(path = landscape_driver_path, filename = "population_construction_linkage.png", width = 10, height = 12, scale = 1)



