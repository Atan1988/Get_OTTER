rm(list = ls())

library(RSelenium)
library(XML)
library(dataCompareR)
library(tidyverse)

source('OTTER Scraping.R')

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445L)

all_counties_df <- readRDS("All Tank Locations.RData")
colnames(all_counties_df) <- gsub("\\t", "", colnames(all_counties_df))

Releases_df <- all_counties_df %>% 
  filter(ReleaseNumber != "")

Facility_IDs <- Releases_df %>% pull(Facility_ID) %>% unique()
to_scrub <- Facility_IDs
# i <- readRDS(paste0("release_data/", "current_index", ".RData"))
P <- 0
# k <- 21869

log_file <- file(paste("Update Logs/Releases/Nursing Home DB Update Log ", 
                       gsub("\\:", "_", Sys.time()), ".Rout", sep=""), open = "wt")
sink(log_file)
sink(log_file, type = "message")
start <- Sys.time()

if (!is.null(rel_dfs1)) to_scrub <- to_scrub[!to_scrub %in% rel_dfs1$Facility_ID]

to_scrub <-Releases_df$Facility_ID[!Releases_df$Facility_ID %in% release_list$Facility_ID] %>% unique
system.time(rel_dfs1 <- bind_rows(lapply(to_scrub, get_releases_safe)))

total_time <- Sys.time() - start
cat(total_time)
sink(type = "message")
sink()
close(log_file)

# i <- i + k

saveRDS(rel_dfs1, paste0("release_data2/", "release_detail_", gsub("\\:", "_", Sys.time()), ".RData"))


