library(readxl)
library(dplyr)
library(tidyr)


# read in mapping sheets
sector_mapping_GCAM <- read_xlsx("GCAM emissions matching/sector_mapping.xlsx", sheet = 1)
sector_mapping_EDGAR_GHGs <- read_xlsx("GCAM emissions matching/sector_mapping.xlsx", sheet = 2)
sector_mapping_EDGAR_emissions <- read_xlsx("GCAM emissions matching/sector_mapping.xlsx", sheet = 3)

region_mapping <- read_xlsx("GCAM emissions matching/region_mapping.xlsx")

# to proportion function for emissions
# calculates the proportions of a [country, major sector] combination in a [GCAM region, all sectors] combination
to_propn <- function(EDGAR) {
  EDGAR <- EDGAR[!is.na(EDGAR$ISO_A3), ]
  EDGAR[is.na(EDGAR$`2012`), '2012'] <- 0
  EDGAR[EDGAR$ISO_A3 %in% c("HKG", "MAC"), "ISO_A3"] <- "CHN"
  EDGAR[EDGAR$ISO_A3 %in% c("PRI"), "ISO_A3"] <- "USA"
  EDGAR_expanded <- EDGAR %>% left_join(region_mapping, by = "ISO_A3") %>% 
    left_join(sector_mapping_EDGAR_emissions, by = c("IPCC_description" = "EDGAR_emissions")) %>% select(-`IPCC_description`)
  # EDGAR_total <- EDGAR_expanded %>% group_by(major_sector) %>% mutate(`2012_total` = sum(`2012`)) %>% select(-`2012`)
  # EDGAR_summarised <- EDGAR_total %>% unique() %>% drop_na()
  EDGAR_summarised <- EDGAR_expanded %>% group_by(ISO_A3, GCAM, EDGAR, GCAM_30re_code, major_sector) %>% 
    summarise(`2012_total` = sum(`2012`), .groups = "drop")
  EDGAR_subtotal <- EDGAR_summarised %>% ungroup() %>% 
    group_by(GCAM) %>% mutate(propn = `2012_total` / sum(`2012_total`))
  return(EDGAR_subtotal)
}

# to proportion function for GHGs (CH4 and CO2. Eventually, CO2 and CH4 are not needed)
to_propn_GHG <- function(EDGAR) {
  EDGAR <- EDGAR[!is.na(EDGAR$ISO_A3), ]
  EDGAR$"2015"[is.na(EDGAR$`2015`)] <- 0
  EDGAR <- EDGAR %>% filter(!grepl("NULL", `2015`))
  EDGAR$`2015` <- as.numeric(EDGAR$`2015`)
  EDGAR[EDGAR$ISO_A3 %in% c("HKG", "MAC"), "ISO_A3"] <- "CHN"
  EDGAR[EDGAR$ISO_A3 %in% c("PRI"), "ISO_A3"] <- "USA"
  EDGAR_expanded <- EDGAR %>% left_join(region_mapping, by = "ISO_A3") %>% 
    left_join(sector_mapping_EDGAR_GHGs, by = c("IPCC_description" = "EDGAR_GHGs")) %>% select(-`IPCC_description`)
  # EDGAR_total <- EDGAR_expanded %>% group_by(major_sector) %>% mutate(`2015_total` = sum(`2015`)) %>% select(-`2015`)
  # EDGAR_summarised <- EDGAR_total %>% unique() %>% drop_na()
  EDGAR_summarised <- EDGAR_expanded %>% group_by(ISO_A3, GCAM, EDGAR, GCAM_30re_code, major_sector) %>% 
    summarise(`2015_total` = sum(`2015`), .groups = "drop")
  EDGAR_subtotal <- EDGAR_summarised %>% ungroup() %>% 
    group_by(GCAM) %>% mutate(propn = `2015_total` / sum(`2015_total`))
  return(EDGAR_subtotal)
}

# EDGAR data read in
BC_EDGAR <- read_xls("GCAM emissions matching/v432_BC_1970_2012.xls", sheet = 1, skip = 7) %>% 
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
CO_EDGAR <- read_xls("GCAM emissions matching/v432_CO_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
NH3_EDGAR <- read_xls("GCAM emissions matching/v432_NH3_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
NMVOC_EDGAR <- read_xls("GCAM emissions matching/v432_NMVOC_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
NOx_EDGAR <- read_xls("GCAM emissions matching/v432_NOx_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
OC_EDGAR <- read_xls("GCAM emissions matching/v432_OC_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
SO2_EDGAR <- read_xls("GCAM emissions matching/v432_SO2_1970_2012.xls", sheet = 1, skip = 7) %>%
  select(c(`ISO_A3`, `IPCC_description`, `2012`))

PM2.5bio_EDGAR <- read_xls("GCAM emissions matching/v432_PM2.5_bio_1970_2012.xls", sheet = 1, skip = 7) %>% 
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
PM2.5fossil_EDGAR <- read_xls("GCAM emissions matching/v432_PM2.5_fossil_1970_2012.xls", sheet = 1, skip = 7) %>% 
  select(c(`ISO_A3`, `IPCC_description`, `2012`))
PM2.5_EDGAR <- rbind(PM2.5bio_EDGAR, PM2.5fossil_EDGAR)

CH4_EDGAR <- read_xls("GCAM emissions matching/v50_CH4_1970_2015.xls", sheet = 1, skip = 9) %>% 
  select(c(`ISO_A3`, `IPCC_description`, `2015`))
CO2_EDGAR <- read_xls("GCAM emissions matching/v50_CO2_excl_short-cycle_org_C_1970_2018.xls", sheet = 1, skip = 9) %>% 
  select(c(`ISO_A3`, `IPCC_description`, `2015`))

# to proportions
BC_EDGAR_propn <- to_propn(BC_EDGAR)
CO_EDGAR_propn <- to_propn(CO_EDGAR)
NH3_EDGAR_propn <- to_propn(NH3_EDGAR)
NMVOC_EDGAR_propn <- to_propn(NMVOC_EDGAR)
NOx_EDGAR_propn <- to_propn(NOx_EDGAR)
OC_EDGAR_propn <- to_propn(OC_EDGAR)
PM2.5_EDGAR_propn <- to_propn(PM2.5_EDGAR)
SO2_EDGAR_propn <- to_propn(SO2_EDGAR)

CH4_EDGAR_propn <- to_propn_GHG(CH4_EDGAR)
CO2_EDGAR_propn <- to_propn_GHG(CO2_EDGAR)

## mapping GCAM data to major sectors * countries

BC <- c("BC", "BC_AWB")
CH4 <- c("CH4", "CH4_AGR", "CH4_AWB")
CO <- c("CO", "CO_AWB")
NH3 <- c("NH3", "NH3_AGR", "NH3_AWB")
NMVOC <- c("NMVOC", "NMVOC_AWB")
NOx <- c("NOx", "NOx_AGR", "NOx_AWB")
OC <- c("OC", "OC_AWB")
SO2 <- c("SO2", "SO2_1","SO2_1_AWB", "SO2_2","SO2_2_AWB", "SO2_3","SO2_3_AWB", "SO2_4","SO2_4_AWB")

to_mapped = function(gcam, EDGAR_propn, material, year) {
  material_GCAM <- gcam %>% select(region, sector, GHG, all_of(year)) %>% ungroup() %>% filter(GHG %in% material) %>%
    left_join(sector_mapping_GCAM, by = c("sector" = "GCAM")) %>% select(-c(GHG, sector))
  material_GCAM[, 2] <- as.numeric(unlist(material_GCAM[, 2]))
  colnames(material_GCAM)[2] <- "amount"
  material_GCAM <- material_GCAM %>% group_by(region) %>% summarise(amount = sum(amount))
  material_mapped <- EDGAR_propn %>% left_join(material_GCAM, by = c("GCAM" = "region"))
  material_mapped[is.na(material_mapped[, "amount"]), "amount"] <- 0
  material_mapped <- material_mapped %>% mutate(amount = amount * propn)
  material_country <- material_mapped %>% ungroup() %>%
    group_by(ISO_A3) %>% summarise(amount = sum(amount))
  colnames(material_country)[2] <- year
  return(material_country)
}

