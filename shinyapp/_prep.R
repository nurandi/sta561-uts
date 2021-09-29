library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

## data preparation
## tidak diperlukan untuk menjalankan shiny app

ipmData <- read.csv("../data/ipm_kab.csv") %>%
  as_tibble()

ipmLonger <- ipmData %>% pivot_longer(-c(Kode, Kab), 
                         names_to = c(".value", "Tahun"), 
                         names_pattern = "([^_]+)_(.*)") %>%
  mutate(Tahun = as.factor(Tahun),
         PPP = as.numeric(gsub(" ", "", PPP)),
         Kode2 = as.integer(paste0(substr(Kode,1,2),'00'))) %>%
  mutate(Kode2 = if_else(Kode == Kode2, 0L, Kode2),
         Kode2 = if_else(Kode == 0L, 9L, Kode2)) 

ipm <- ipmLonger %>%
  # filter(Kode != 0L) %>%
  rename(Reg = Kab) %>%
  left_join(
    ipmLonger %>% 
      filter(Kode2 == 0 | Kode2 == 9) %>%
      select(-Kode2) %>%
      select(Kode2 = Kode, 
             Reg2 = Kab,
             IPM2 = IPM,
             AHH2 = AHH,
             EYS2 = EYS,
             MYS2 = MYS,
             PPP2 = PPP,
             everything()),
    by = c("Kode2", "Tahun") ) %>%
  mutate(Reg  = if_else(Kode  == 0L, "NASIONAL", Reg),
         Reg2 = if_else(Kode2 == 0L, "NASIONAL", Reg2),
         IPMd = IPM - IPM2,
         AHHd = AHH - AHH2,
         EYSd = EYS - EYS2,
         MYSd = MYS - MYS2,
         PPPd = PPP - PPP2,
         IPMs = as.factor(if_else(IPMd > 0, 1,0)),
         AHHs = as.factor(if_else(AHHd > 0, 1,0)),
         EYSs = as.factor(if_else(EYSd > 0, 1,0)),
         MYSs = as.factor(if_else(MYSd > 0, 1,0)),
         PPPs = as.factor(if_else(PPPd > 0, 1,0)) ) %>%
  select(Kode, Kode2, Reg, Reg2, Tahun,
         everything())

saveRDS(ipm, "data/ipm.Rds")
# write.csv(ipm, "data/ipm.csv", row.names = FALSE)

# extract geometry dari shp

library(sf)
library(rmapshaper)

mapProv <- st_read("../data/shp/idn_admbnda_adm1_bps_20200401.shp", 
                        quiet = TRUE) %>%
  mutate(Kode = as.integer(paste0(substr(ADM1_PCODE,3,4),'00'))) %>%
  select(Kode, geometry) %>% 
  ms_simplify() # simplify geometry
  
mapKab <- st_read("../data/shp/idn_admbnda_adm2_bps_20200401.shp", 
                   quiet = TRUE) %>%
  mutate(Kode = as.integer(substr(ADM2_PCODE,3,7))) %>%
  select(Kode, geometry) %>% 
  ms_simplify() 

saveRDS(mapProv, "data/mapProv.Rds")
saveRDS(mapKab, "data/mapKab.Rds")





