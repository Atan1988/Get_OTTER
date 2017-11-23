library(tidyverse)
library(lubridate)
library(car)
library(mgcv)

Locations_df <- readRDS("All Tank Locations.RData")
colnames(Locations_df) <- gsub("\\t", "", colnames(Locations_df))

Tanks_df <- readRDS("All Tanks.RData") 
rel_dtls_df <- readRDS("All release details more.RData")

Releases_df <- Locations_df %>% 
  filter(ReleaseNumber != "") %>% 
  select(-V1, -V2, -V3, -V4) %>% 
  left_join(rel_dtls_df, by = c('Facility_ID' = 'Facility_ID', "ReleaseNumber" = "ReleaseNumber"))

Releases_df <- Releases_df %>%
  mutate(Cause = case_when(
         cause1 == 'true' ~ "Corrosion"
         , cause2 == 'true' ~ "Install Problem"
         , cause3 == 'true' ~ "Other Cause"
         , cause4 == 'true' ~ "Overfill"
         , cause5 == 'true' ~ "Phys/Mech Damage"
         , cause6 == 'true' ~ "Spill"
         , cause7 == 'true' ~ "Unknown Cause"
         , TRUE ~ 'No Cause Selection'
         )
  )

Releases_df <- Releases_df %>% mutate(DateReported = as.Date(DateReported, "%m/%d/%Y")
                                      , RptYear = year(DateReported)
                                      , FiscYQ = quarter(DateReported, with_year = T, fiscal_start = 10)
                                      , FiscYear = floor(as.numeric(as.character((FiscYQ))))
                                      ) %>% 
  #filter(  !is.na(Releasestatus) & !is.na(LTFstatuss)) %>%
  filter(!is.na(RptYear))

# Releases_df %>% filter(DateReported <= as_date("2014-09-30"), 
#                        DateReported >= as_date("2013-10-01")) %>%
#   filter(Cause != "No Cause Selection") %>% 
#   # group_by(Cause) %>%
#   # summarise(ct = n()) %>%
#   View()
# 
# Releases_df %>% filter(Cause != "No Cause Selection") %>% 
#     group_by(FiscYear) %>%
#     summarise(ct = n()) %>% View()

colnames(Tanks_df) <- make.names(colnames(Tanks_df))
colnames(Tanks_df) <-gsub("\\.", "", colnames(Tanks_df))
Tanks_df <- Tanks_df %>% mutate(DateofInstallation = as.Date(DateofInstallation, "%m/%d/%Y"), 
                                DateLastUsed = as.Date(DateLastUsed, "%m/%d/%Y"), 
                                DateRemoved = as.Date(DateLastUsed, "%m/%d/%Y"), 
                                TCLClosedDate = as.Date(TCLClosedDate, "%m/%d/%Y")
                                ) %>%
                         filter(!is.na(TankNumber))
ppp <- Tanks_df %>% filter(is.na(Tanks_df$DateofInstallation))
Tanks_df_known <- Tanks_df %>% filter(!is.na(Tanks_df$DateofInstallation))

fiscal_bg <- "10-01"

fiscal_dates <- as_date(paste(seq(2000, 2016, 1), fiscal_bg, sep = "-"))

tank_2000 <- Tanks_df_known %>% 
    filter(pmin(DateRemoved, TCLClosedDate, DateLastUsed, na.rm = T) > fiscal_dates[1] | 
             (is.na(DateRemoved) & is.na(TCLClosedDate) & is.na(DateLastUsed)) ) %>%
    filter(DateofInstallation < fiscal_dates[1]) %>% 
    mutate(Age = as.numeric(fiscal_dates[1] - DateofInstallation) / 365.25  )

rels_2000 <- Releases_df %>% filter(DateReported >= fiscal_dates[1], DateReported < fiscal_dates[2])

get_feature_distr <- function(df, variables) {
  summ_df <- df%>% group_by_(.dots = c('Facility_ID', variables) ) %>% 
    summarise(ct = n()) %>% spread_(variables, "ct")
  colnames(summ_df) <- c("Facility_ID", paste0(variables, "_", 
                        colnames(summ_df)[colnames(summ_df) != "Facility_ID"]))
  summ_df
}

isSensitive <- tank_2000 %>% group_by(Facility_ID, IsSensitiveArea) %>% 
                            summarise(ct = n()) %>% spread(IsSensitiveArea, ct)
colnames(isSensitive) <- c("Facility_ID", paste0("isSensitive_", 
                          colnames(isSensitive)[colnames(isSensitive) != "Facility_ID"]))

facility_2000 <- tank_2000 %>%  group_by(Facility_ID) %>%
  summarise(ct = n(), 
            AvgAge = mean(Age), 
            MinAge = min(Age), 
            MaxAge = max(Age)) %>%
  mutate(Age30plus = ifelse(AvgAge > 30, 1, 0)) %>%
  mutate(AgeGroup = cut(AvgAge, c(0, 15, 20, 25, 30, Inf)))%>%
  mutate(PY = fiscal_dates[1]) %>%
  left_join(rels_2000 %>% group_by(Facility_ID) %>% summarise(Relct = n())) %>% 
  ungroup() %>% mutate(Relct = ifelse(is.na(Relct), 0, Relct), 
                       NRelct = ct - Relct)%>%
  filter(NRelct > 0) 
  
summary(fit1 <-glm(cbind(Relct, NRelct) ~ AvgAge:AgeGroup, 
                  data = facility_2000, family = binomial(link = "logit")))

summary(fit2 <-glm(cbind(Relct, NRelct) ~ AgeGroup, 
                  data = facility_2000, family = binomial(link = "logit")))

summary(fit3 <- gam(cbind(Relct, NRelct) ~ AvgAge:AgeGroup, 
                   data = facility_2000, family = binomial(link = "logit")))


table(Tanks_df$IsSensitiveArea)
table(Tanks_df$PipingReleaseDetection)
table(Tanks_df$USTSecondaryReleaseDetection)
table(Tanks_df$USTPrimaryReleaseDetection)
table(Tanks_df$USTConstruction)


NoTanks <- Releases_df %>% filter(TankStatus == "No Tanks Available")
Tanks_REM <- Tanks_df %>% filter(Facility_ID %in% NoTanks$Facility_ID)
Tanks_active <- Tanks_df %>% filter(!Facility_ID %in% NoTanks$Facility_ID)

Tanks_nouse <- Tanks_df %>% 



                                          




Releases_SUMM <- Releases_df %>% group_by(RptYear) %>% summarise(ct = n())

Releases_df %>% group_by(RptYear, LTFstatuss) %>% summarise(ct = n()) %>% spread(LTFstatuss, ct)

Releases_df %>% group_by(Releasestatus) %>% summarise(ct = n()) %>% spread(LTFstatuss, ct)

county_summ  <- function(county, 
                         to_excl =  c("DIS: a release is disproved", 'NFA: No Further Action',
                       'NFC: No Further Action Data Cleaning', 'SCK: Site Check')) {
 df <- Releases_df %>% 
          filter(LTFstatuss == '1 SUS/CON from regulated UST') %>%
          filter(!Releasestatus %in% to_excl)
 df %>% filter(County == county)%>% summarise(ct = n())%>% pull(ct)
}

sort(sapply(unique(Releases_df$County), county_summ
          #  , to_excl = c("DIS: a release is disproved", 'SCK: Site Check')
       ))

allen_df <- Releases_df %>% filter(County == 'Summit') %>%
  group_by(Releasestatus, LTFstatuss) %>% summarise(ct = n()) %>% arrange(LTFstatuss)

kkk <- Releases_df %>% filter(RptYear == 2016)%>% 
  group_by(Releasestatus, LTFstatuss) %>% summarise(ct = n()) %>% arrange(LTFstatuss)

status_to_incl <- Releases_df %>% select(Releasestatus, LTFstatuss) %>% unique() %>%
                  arrange(LTFstatuss)

ooo <- Releases_df %>% 
                filter(RptYear  > 2000) %>%
                filter(!Releasestatus %in% c('NFA: No Further Action' , 'DIS: a release is disproved')) %>%
                # filter(!(Releasestatus == 'NFA: No Further Action' & 
                #            LTFstatuss == '1 SUS/CON from regulated UST')) %>%
                filter(!is.na(RptYear)) %>% 
                group_by(RptYear) %>% summarise(ct = n())