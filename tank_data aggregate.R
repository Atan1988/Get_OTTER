

library(tidyverse)
folder <- "tank_data"
tanks_files <- list.files(file.path(folder), pattern = "tank_detail*")


get_tanks <- function(fl) {
  tmp <- bind_rows(readRDS(file.path(folder, fl))) %>%
         filter(!is.na(`Tank Number`))
  return(tmp)
}

tank_list <- bind_rows(lapply(tanks_files, get_tanks))

saveRDS(tank_list, "All Tanks.RData")

