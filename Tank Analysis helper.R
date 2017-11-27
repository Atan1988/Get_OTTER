get_feature_distr <- function(df, variables) {
  summ_df <- df%>% group_by_(.dots = c('Facility_ID', variables) ) %>% 
    summarise(ct = n()) %>% ungroup() %>%
    group_by(Facility_ID) %>%
    mutate(pct = ct / sum(ct, na.rm = T)) %>%
    dplyr::select(-ct)
  
  summ_df <-  summ_df %>% 
              spread_(variables, "pct")
  colnames(summ_df) <- c("Facility_ID", paste0(variables, "_", 
                                               colnames(summ_df)[colnames(summ_df) != "Facility_ID"]))
  
  colnames(summ_df) <- gsub("\\.", "", make.names(colnames(summ_df)))
  summ_df %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
}

set_model_df  <- function(fy_start, fy_end, Tanks_df, Releases_df) {
  tank_2000 <- Tanks_df_known %>% 
    filter(pmin(DateRemoved, TCLClosedDate, DateLastUsed, na.rm = T) > fy_start | 
             (is.na(DateRemoved) & is.na(TCLClosedDate) & is.na(DateLastUsed)) ) %>%
    filter(DateofInstallation < fy_start) %>% 
    mutate(Age = as.numeric(fy_start - DateofInstallation) / 365.25  )
  
  rels_2000 <- Releases_df %>% filter(DateReported >= fy_start, DateReported < fy_end)
  
  feature_list <- lapply(c('IsSensitiveArea', 'Contents', 
                           'USTPrimaryReleaseDetection', 'USTConstruction', 
                           'PipingReleaseDetection', 'PipingStyle', 'PipingConstruction'), 
                         function(x) get_feature_distr(tank_2000, x))
  
  feature_df <- feature_list %>% reduce(left_join, by = 'Facility_ID')
  
  facility_2000 <- tank_2000 %>%  group_by(Facility_ID) %>%
    summarise(ct = n(), 
              AvgAge = mean(Age), 
              MinAge = min(Age), 
              MaxAge = max(Age), 
              AvgCapacity = mean(as.numeric(Capacity), na.rm = T)) %>%
    mutate(Age30plus = ifelse(AvgAge > 30, 1, 0)) %>%
    mutate(AgeGroup = cut(AvgAge, c(0, 15, 20, 25, 30, Inf)))%>%
    mutate(PY = fy_start) %>%
    left_join(rels_2000 %>% group_by(Facility_ID) %>% summarise(Relct = n())) %>% 
    ungroup() %>% mutate(Relct = ifelse(is.na(Relct), 0, Relct), 
                         NRelct = ct - Relct)%>%
    filter(NRelct > 0) %>%
    left_join(feature_df)
  
  return(facility_2000)
}
