library(RSelenium)
library(XML)
library(dataCompareR)
library(tidyverse)

source('OTTER scraping.R')

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",
                      port = 4445L)

remDr$open(silent = TRUE)

remDr$navigate("https://apps.com.ohio.gov/fire/otter/#")

###get number of counties
doc <- htmlParse(remDr$getPageSource()[[1]])
counties <- readHTMLTable(doc)[[1]]
counties <- seq(1, nrow(counties) * ncol(counties), 1)

remDr$closeall()

system.time(all_counties <- lapply(counties, function(x) suppressMessages(get_county_tanks(x))))

all_counties_df <- bind_rows(all_counties)

colnames(all_counties_df) <- gsub("\\ |\\\n", "", colnames(all_counties_df))

all_counties_df <- all_counties_df %>% mutate(Facility_ID = substr(Facility, 1, 8))

saveRDS(all_counties_df, "All Tank Locations.RData")