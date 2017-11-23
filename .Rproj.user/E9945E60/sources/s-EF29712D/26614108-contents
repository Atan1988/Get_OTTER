library(tidyverse)
folder <- "release_data2"
release_files <- list.files(file.path(folder), pattern = "release_detail*")

get_releases_fl <- function(fl) {
  tmp <- bind_rows(readRDS(file.path(folder, fl)))# %>%
    #filter(!is.na(`Tank Number`))
  return(tmp)
}

release_list <- bind_rows(lapply(release_files, get_releases_fl)) %>% unique()

release_list %>% group_by(Facility_ID, ReleaseNumber) %>% mutate(ct = n()) %>% filter(ct > 1)

saveRDS(release_list, "All release details more.RData")
