rm(list = ls())

library(RSelenium)
library(XML)
library(dataCompareR)
library(tidyverse)

source('OTTER Scraping.R')

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445L)

all_counties_df <- readRDS("All Tank Locations.RData")

Facility_IDs <- all_counties_df %>% pull(Facility_ID) %>% unique()

i <- readRDS(paste0("tank_data/", "current_index", ".RData"))
k <- 3930

system.time(tank_Sites1 <- lapply(Facility_IDs[i:(i+ k -1)], get_facilities_safe))

i <- i + k

saveRDS(tank_Sites1, paste0("tank_data/", "tank_detail_", gsub("\\:", "_", Sys.time()), ".RData"))
saveRDS(i, paste0("tank_data/", "current_index", ".RData"))

