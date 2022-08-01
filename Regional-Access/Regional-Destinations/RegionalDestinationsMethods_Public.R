## MBTA Public Regional Destinations Methodology
## Updated August 2022

#############################
## PART 0: IMPORT PACKAGES ##

# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("sqldf")
library(rlang)
library(tidyverse)
library(magrittr)
library(sqldf)

# Add your computer username
USERN <- ""

# Add your working directory file path.
pathway <- ""

# Add your Census API key (https://api.census.gov/data/key_signup.html)
apikey <- ""

# Set working directory with your username.
setwd(pathway)


########################################
## PART 1: PULL BLOCK GROUP DENSITIES ##

# Import master frankenstein BAZ-BG crosswalk
masterBG <- read_csv("Data/master_baz_ref.csv")


## Pull census data ##
library(tidycensus)

# Load census key from https://api.census.gov/data/key_signup.html
census_api_key(apikey) 

# Pull population
# Change year value as data from the Census becomes available.
BGpop <- get_acs(geography = "block group", 
                 variables = "B03002_001", # population
                 output = "wide",
                 state = "MA",
                 year = 2019)

##########################################
## PART 2: MUTATE BLOCK GROUP DENSITIES ##

# Aggregate BG census variables
BGpop %<>% transmute(GEOID, pop = B03002_001E)

# Convert BG field type to character
masterBG %<>% mutate(blkgrp = as.character(blkgrp))

# Join to Frankenstein block groups
masterBG %<>% left_join(BGpop, by = c("blkgrp" = "GEOID"), all.x = TRUE)


## Process LEHD data from https://lehd.ces.census.gov/data/ ##

# LEHD Origin-Destination Employment Statistics (LODES) Workplace Area Characteristics (WAC)
jobsblock <- read_csv("Data/Fall 2019/ma_wac_S000_JT00_2018.csv")
geoxwalk <- read_csv("Data/Fall 2019/ma_xwalk.csv")

# Convert block groups from scientific notation to characters
geoxwalk <- transform(geoxwalk, tabblk2010 = as.character(tabblk2010))
jobsblock <- transform(jobsblock, w_geocode = as.character(w_geocode))

# Pull needed crosswalk columns
geos <- geoxwalk %>% select(c("bgrp", as.character("tabblk2010")))

# LODES column C000 is total number of jobs - http://celebratingcities.github.io/docs.html
jobs <- jobsblock %>% select(c(as.character("w_geocode"), "C000"))

# Merge geos and jobs
geojobs <- merge(geos, jobs, by.x = "tabblk2010", by.y = "w_geocode")

# Aggregate to block group
jobsBG <- aggregate(C000 ~ bgrp, geojobs, sum, na.rm = TRUE)

# Rename jobs column
jobsBG %<>% rename(jobs = "C000")

# Replace NA values with 0s 
jobsBG[is.na(jobsBG)] <- 0

# Convert BG field type to character
jobsBG %<>% mutate(bgrp = as.character(bgrp))

# Join jobs to master BG table
masterBG %<>% left_join(jobsBG, by = c("blkgrp" = "bgrp"), all.x = TRUE)


# Aggregate to BAZ level
masterBAZ <- masterBG %>%
  group_by(baz, baz_name, municipality, neighborhood, neighborhood_desc) %>%
  select(c(blkgrp_sqmi, pop, jobs)) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)

# Calculate densities at BAZ level
masterBAZ %<>% mutate(popjobs = pop + jobs,
                      popdensity = pop / blkgrp_sqmi,
                      jobdensity = jobs / blkgrp_sqmi,
                      popjobdensity = popjobs / blkgrp_sqmi)


#################################
## PART 3: PROCESS DEMAND DATA ##

# Change this path as the data for OD volumes changes!
setwd(paste0("C:/Users/",USERN,"/Downloads/"))
OD <- read_csv("CorrectedODVolumesFall2019_021122.csv")

# Revert working directory
setwd(pathway)

## Clean Demand Data ##

# Filter to all days and all times
OD %<>% filter(daytype == "0: All Days (M-Su)" & dayhr == "00: All Day (12am-12am)")

# Filter to OD pairs where O != D (inter-BAZ trips)
OD %<>% filter(obaz != dbaz)

# Calculate distance %s and apply to all volumes
OD %<>% mutate(pctlong = longvol / bpvvol)
OD %<>% rename(allbpvvol = "bpvvol",
               allminoVol = "minoVol",
               allliVol = "liVol")
OD %<>% mutate(bpvvol = longvol,
               livol = pctlong * allliVol,
               minovol = pctlong * allminoVol)

## Process all trips table ##
dailyTrips <- 10

# Filter to origins with >10 average daily trips
# OD_all <- OD %>% filter
OD_all <- OD %>% filter(bpvvol > dailyTrips)

# Aggregate trips and number of origins to end_baz
vol_all <- aggregate(bpvvol ~ dbaz, OD_all, sum)
orig_all <- aggregate(obaz ~ dbaz, OD_all, length)
RDs_all_full <- merge(orig_all, vol_all, by = 'dbaz')

# Join to BAZ-level densities table
RDs_all_full <- masterBAZ %>% left_join(RDs_all_full, by = c("baz" = "dbaz"), all.x = TRUE)

# Drop and rename columns
RDs_all_full %<>% select(c(baz, baz_name, obaz, bpvvol, popdensity, jobdensity, popjobdensity))
RDs_all_full %<>% rename(origins = "obaz",
                         trips = "bpvvol")

# Remove unneeded tables
rm(vol_all, orig_all)

# Create variables to judge destinations
jDensity <- 0
originCount <- 0
tripCount = 0

# Create the table of all (as of 2019) 827 BAZs in order to create percentiles
OD_li <- OD %>% filter(livol >= dailyTrips)

## Process LI trips table ##

# Aggregate trips and number of origins to destination BAZ
vol_li <- aggregate(livol ~ dbaz, OD_li, sum)
orig_li <- aggregate(obaz ~ dbaz, OD_li, length)
RDs_li_full <- merge(orig_li, vol_li, by = 'dbaz')

# Merge with densities table
RDs_li_full <- masterBAZ %>% left_join(RDs_li_full, by = c("baz" = "dbaz"), all.x = TRUE)

# Drop and rename columns
RDs_li_full %<>% select(c(baz, baz_name, obaz, livol, popdensity, jobdensity, popjobdensity))
RDs_li_full %<>% rename(origins = "obaz",
                        trips = "livol")

# Remove unneeded tables
rm(vol_li, orig_li)

## Process POC trips table ##

# Filter to origins with >10 average daily trips
OD_poc <- OD %>% filter(minovol >= dailyTrips)

# Aggregate trips and number of origins to destination BAZ
vol_poc <- aggregate(minovol ~ dbaz, OD_poc, sum)
orig_poc <- aggregate(obaz ~ dbaz, OD_poc, length)
RDs_poc_full <- merge(orig_poc, vol_poc, by = 'dbaz')

# Merge with densities table
RDs_poc_full <- masterBAZ %>% left_join(RDs_poc_full, by = c("baz" = "dbaz"), all.x = TRUE)

# Drop and rename columns
RDs_poc_full %<>% select(c(baz, baz_name, obaz, minovol, popdensity, jobdensity, popjobdensity))
RDs_poc_full %<>% rename(origins = "obaz",
                         trips = "minovol")

# Remove unneeded tables
rm(vol_poc, orig_poc)

#########################################
## PART 4: CENTRALIZE DESTINATION DATA ##

# Initial base filtering for trips tables
# Requiring 6,000 jobs per sq mi, 90 origins, and 8,000 trips
# With an exception (at the first stage) for Logan Airport
RDs_all <- RDs_all_full %>% 
  filter((jobdensity >= jDensity & origins >= originCount & trips >= tripCount) | baz == 436) %>% 
  arrange(desc(origins)) %>% 
  distinct(baz, .keep_all = TRUE)
RDs_li <- RDs_li_full %>% 
  filter((jobdensity >= jDensity & origins >= originCount & trips >= tripCount) | baz == 436) %>% 
  arrange(desc(origins)) %>% 
  distinct(baz, .keep_all = TRUE)
RDs_poc <- RDs_poc_full %>% 
  filter((jobdensity >= jDensity & origins >= originCount & trips >= tripCount) | baz == 436) %>% 
  arrange(desc(origins)) %>% 
  distinct(baz, .keep_all = TRUE)

## Create master RD table with all 3 groups ##

# Modify columns for joining
RDs_a <- RDs_all %>% select(c("baz", "baz_name", "origins", "trips", "popdensity", "jobdensity", "popjobdensity"))
RDs_l <- RDs_li %>% select(c("baz", "origins", "trips"))
RDs_p <- RDs_poc %>% select(c("baz", "origins", "trips"))

# Join first two tables
RDs_base <- RDs_a %>% full_join(RDs_l, by = "baz", all = TRUE)

# Rename overlap columns for clarity
RDs_base %<>% rename(originsALL = "origins.x",
                     tripsALL = "trips.x",
                     originsLI = "origins.y",
                     tripsLI = "trips.y")

# Join third table
RDs_base %<>% full_join(RDs_p, by = "baz", all = TRUE)

# Rename overlap columns for clarity
RDs_base %<>% rename(originsPOC = "origins",
                     tripsPOC = "trips")

# Drop unneeded columns
RDs_base %<>% select(c("baz", "baz_name", 
                       "originsALL", "originsLI", "originsPOC", 
                       "tripsALL", "tripsLI", "tripsPOC", 
                       "popdensity", "jobdensity", "popjobdensity"))

# Create equity list of destinations important to all three groups
RDs_equity <- RDs_base %>% filter(!is.na(originsALL) & !is.na(originsLI) & !is.na(originsPOC))

maxBAZ <- 831

RDs_equity_big <- RDs_equity %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(originsPOC))

##########################################
## PART 5: SELECT REGIONAL DESTINATIONS ##

inclusion <- 0

minLength <- 19

rv1 <- c()

# while (length(rv1) < minLength){
jdensity2 <- 40 + inclusion
tripsALL2 <- 262 + inclusion
tripsLI2 <- 49 + inclusion
tripsPOC2 <- 53 + inclusion
originsALL2 <- 508 + inclusion
originsLI2 <- 46 + inclusion
originsPOC2 <- 44 + inclusion 

RDs_equity_j <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(jobdensity))

RDs_equity_oA <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(originsALL))

RDs_equity_tA <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(tripsALL))

RDs_equity_tL <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(tripsLI))

RDs_equity_tP <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(tripsPOC))

RDs_equity_oL <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(originsLI))

RDs_equity_oP <- RDs_equity_big %>% 
  filter(baz <= maxBAZ) %>%
  arrange(desc(originsPOC))

rv1 <- c()

prop_j <- jdensity2 / nrow(RDs_equity_big)
prop_oA <- tripsALL2 / nrow(RDs_equity_big)
prop_tA <- tripsALL2 / nrow(RDs_equity_big)
prop_tL <- tripsLI2 / nrow(RDs_equity_big)
prop_tP <- tripsPOC2 / nrow(RDs_equity_big)
prop_oL <- originsLI2 / nrow(RDs_equity_big)
prop_oP <- originsPOC2 / nrow(RDs_equity_big)

head_j <- floor(nrow(RDs_equity_big) * prop_j)
head_oA <- floor(nrow(RDs_equity_big) * prop_oA)
head_tA <- floor(nrow(RDs_equity_big) * prop_tA)
head_tL <- floor(nrow(RDs_equity_big) * prop_tL)
head_tP <- floor(nrow(RDs_equity_big) * prop_tP)
head_oL <- floor(nrow(RDs_equity_big) * prop_oL)
head_oP <- floor(nrow(RDs_equity_big) * prop_oP)

# Logan Airport fails the job density check, but we do want it to be added.
# This exception is built in to specifically allow Logan Airport under the
# rules of the 'job density' filter.

# If you don't like how this is handled methodologically, you can always
# remove it (likely removing Logan Airport from the list), or substitute in
# line 356 to automatically add it no matter which filters it fails.
jhead = head(RDs_equity_j, head_j)$baz_name
jhead = c(jhead, "East Boston Logan Airport")

for (val in jhead){
  if (val %in% head(RDs_equity_oA, head_oA)$baz_name){
    if (val %in% head(RDs_equity_tA, head_tA)$baz_name){
      if (val %in% head(RDs_equity_tL, head_tL)$baz_name){
        if (val %in% head(RDs_equity_tP, head_tP)$baz_name){
          if (val %in% head(RDs_equity_oL, head_oL)$baz_name){
            if (val %in% head(RDs_equity_oP, head_oP)$baz_name){
              rv1 <- c(rv1, val)
            }
          }
        }
      }
    }
  }
}


inclusion = inclusion + 1
# rv1 = c(rv1, "East Boston Logan Airport") 
# }
