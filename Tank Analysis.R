library(tidyverse)
library(lubridate)
library(car)
library(mgcv)
library(dataCompareR)

source('Tank Analysis helper.R')
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

facility_2000 <- set_model_df(fy_start = fiscal_dates[1], 
                              fy_end = fiscal_dates[2], 
                              Tanks_df = Tanks_df, Releases_df = Releases_df)
system.time(
  facility_all <- bind_rows(
                mapply(set_model_df,  fy_start = fiscal_dates[-length(fiscal_dates)],
                        fy_end = fiscal_dates[-1],
                        MoreArgs = list(Tanks_df = Tanks_df, Releases_df = Releases_df), 
                        SIMPLIFY = F
                       )
  )
)

facility_all <- facility_all %>% mutate(FY = year(PY) - 2000)

facility_all <- facility_all %>% filter(MaxAge < 50)
  
summary(fit1 <-glm(cbind(Relct, NRelct) ~ AvgAge:AgeGroup, 
                  data = facility_2000, family = binomial(link = "logit")))

summary(fit2 <-glm(cbind(Relct, NRelct) ~ AgeGroup, 
                  data = facility_2000, family = binomial(link = "logit")))

summary(fit3 <- gam(cbind(Relct, NRelct) ~ AvgAge:AgeGroup + IsSensitiveArea_Yes +
                      USTPrimaryReleaseDetection_ATGAutomaticTankGauging + 
                      USTConstruction_BMBareMetal + USTConstruction_CCPSCoatedCathodicallyProtectedSteel +
                      USTConstruction_FRPFiberglassReinforcedPlastic +
                      #USTConstruction_DWFRPFiberglass + 
                      #USTConstruction_DWCladSteel +
                      PipingReleaseDetection_ELLDElectronicLineLeakDetector+ 
                      PipingReleaseDetection_MLLDMechanicalLineLeakDetector + 
                      PipingReleaseDetection_SSSafeSuction, 
                   data = facility_2000, family = binomial(link = "logit")))

summary(fit4 <- gam(cbind(Relct, NRelct) ~ s(AvgAge) + IsSensitiveArea_Yes +
                      USTPrimaryReleaseDetection_ATGAutomaticTankGauging + 
                      USTPrimaryReleaseDetection_AMOAlternativeMethodOtherexplain +
                      USTConstruction_BMBareMetal + USTConstruction_CCPSCoatedCathodicallyProtectedSteel +
                      USTConstruction_FRPFiberglassReinforcedPlastic +
                      #USTConstruction_DWFRPFiberglass + 
                      #USTConstruction_DWCladSteel +
                      PipingReleaseDetection_ELLDElectronicLineLeakDetector + 
                      PipingReleaseDetection_MLLDMechanicalLineLeakDetector + 
                      PipingReleaseDetection_SSSafeSuction, 
                    data = facility_2000, family = binomial(link = "logit")))

summary(fit5 <- gam(cbind(Relct, NRelct) ~ s(AvgAge) + IsSensitiveArea_Yes +
                      USTPrimaryReleaseDetection_ATGAutomaticTankGauging + 
                      USTPrimaryReleaseDetection_AMOAlternativeMethodOtherexplain +
                      USTConstruction_BMBareMetal + USTConstruction_CCPSCoatedCathodicallyProtectedSteel +
                      USTConstruction_FRPFiberglassReinforcedPlastic +
                      #USTConstruction_DWFRPFiberglass + 
                      #USTConstruction_DWCladSteel +
                      PipingReleaseDetection_ELLDElectronicLineLeakDetector + 
                      PipingReleaseDetection_MLLDMechanicalLineLeakDetector + 
                      PipingReleaseDetection_SSSafeSuction, 
                    data = facility_all, family = binomial(link = "logit")))

system.time(fit6 <- gam(cbind(Relct, NRelct) ~ s(AvgAge) + FY + AvgCapacity + IsSensitiveArea_Yes +
                      Contents_Gasoline + Contents_Diesel +
                      PipingStyle_PPressure + PipingStyle_SSuction +
                      PipingConstruction_BMBareMetal +
                      PipingConstruction_FRPFiberglassReinforcedPlastic +
                      PipingConstruction_FPTPFlexiblePlasticTechnologyPiping +  
                      USTPrimaryReleaseDetection_ATGAutomaticTankGauging + 
                      USTPrimaryReleaseDetection_AMOAlternativeMethodOtherexplain +
                      USTPrimaryReleaseDetection_AMSIRAlternativeMethodSIR +
                      # USTPrimaryReleaseDetection_IMOIntMonitoringOther + 
                      # USTPrimaryReleaseDetection_IMTIntMonitoringDWTank +
                      # USTPrimaryReleaseDetection_MTGHManualTankGauging5512000 + 
                      # USTPrimaryReleaseDetection_MTGLManualTankGauging550 +
                      # USTPrimaryReleaseDetection_NPNonePresent +
                      USTPrimaryReleaseDetection_NRNoneRequiredbyRule +
                      USTConstruction_BMBareMetal + 
                      USTConstruction_CCPSCoatedCathodicallyProtectedSteel +
                      USTConstruction_FRPFiberglassReinforcedPlastic +
                      # USTConstruction_DWFRPFiberglass + 
                      # USTConstruction_CSCladSteel + 
                      # USTConstruction_DWCladSteel +
                      # USTConstruction_JSJacketedSteel + 
                      # USTConstruction_SWCladSteel + 
                      # USTConstruction_SWFRPFiberglass +
                      PipingReleaseDetection_ELLDElectronicLineLeakDetector + 
                      PipingReleaseDetection_MLLDMechanicalLineLeakDetector + 
                      PipingReleaseDetection_SSSafeSuction +
                      #PipingReleaseDetection_OTHOtherexplain, 
                    data = facility_all, family = binomial(link = "logit")))




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