source("downscaling.R")
source("TM5_SRMs/TM5_FASST.R")

library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(readxl)

# downscaling.R -> emission_to_pm.R -> HIA.R
# If care about local vs. imported effects, use local_vs_imported.R and pm2p5_details.R

# input files:
# GCAM scenarios
# Age structure
# Relative Risks
# Base mortality rates
# Base life expectancy

# input should be the emissions and populations (including age structure) for each scenario

# input read-in before the loop: 
RUE_ratio <- read_xlsx(path = "TM5_SRMs/COUNTRY-FASST-TABLE_GENERATOR_v2.xlsx", sheet = 3, range = "B1:I2")

FASST_mapping <- read_xlsx(path = "TM5_SRMs/COUNTRY-FASST-TABLE_GENERATOR_v2.xlsx", sheet = 1, range = "A1:C231") %>% 
  rename("ISO_A3" = "ISO 3")

diseases <- c("COPD", "DB", "IHD", "LC", "LRI", "Stroke")

RR_COPD <- read.csv("HIA/GBD_2019_RR_COPD.csv", stringsAsFactors = F)
RR_DB <- read.csv("HIA/GBD_2019_RR_DB.csv", stringsAsFactors = F)
RR_IHD <- read.csv("HIA/GBD_2019_RR_IHD.csv", stringsAsFactors = F)
RR_LC <- read.csv("HIA/GBD_2019_RR_LC.csv", stringsAsFactors = F)
RR_LRI <- read.csv("HIA/GBD_2019_RR_LRI.csv", stringsAsFactors = F)
RR_Stroke <- read.csv("HIA/GBD_2019_RR_Stroke.csv", stringsAsFactors = F)

for (d in diseases) {
  for (i in 1:5) {
    d_SSP_mort_0 <- read.csv(paste0("HIA/", d, "_SSP", i, "_mort_0.csv"), stringsAsFactors = F)
    d_SSP_mort_0 <- d_SSP_mort_0 %>% mutate(year = ifelse(year == 2099, 2100, year)) # because IFs 2100 data are flaw
    assign(paste0(d, "_SSP", i, "_mort_0"), d_SSP_mort_0)
  }
}

for (i in 1:5) {
  assign(paste0("pop_SSP", i), read.csv(paste0("HIA/pop_SSP", i, ".csv"), stringsAsFactors = F))
}

exposure_level <- function(x) {
  levels <- RR_COPD$X
  find_level <- function(x, levels) for (i in 1 : length(levels)) {if(x >= levels[i]) return(i)}
  return(sapply(x, function(x) levels[find_level(x, levels)]))
}

match_RR <- function(pm2p5, RR, d) {
  options(warn = -1)
  levels <- RR_COPD$X
  if(d %in% c("COPD", "DB", "LC", "LRI")) {
    match_RR_each <- function(PM_TOT) { for (i in 1 : length(levels)) { if(PM_TOT >= levels[i]) return(
          RR[i, 2] + (RR[i-1, 2] - RR[i, 2]) / (RR[i-1, 1] - RR[i, 1]) * (PM_TOT - RR[i, 1])
    )}}
    pm2p5$RR <- sapply(pm2p5$PM_TOT, match_RR_each)
    return(pm2p5)
  } else { # d %in% c("IHD", "stroke")
    for (i_age in 2 : ncol(RR)) {
      RR_age <- RR[, c(1, i_age)]
      match_RR_each <- function(PM_TOT) { for (i in 1 : length(levels)) { if(PM_TOT >= levels[i]) return(
        RR_age[i, 2] + (RR_age[i-1, 2] - RR_age[i, 2]) / (RR_age[i-1, 1] - RR_age[i, 1]) * (PM_TOT - RR_age[i, 1])
      )}}
      pm2p5[, paste0("RR", i_age)] <- sapply(pm2p5$PM_TOT, match_RR_each)
    }
    return(pm2p5)
  }
  options(warn = 0)
}

# scenario path should be modified in cluster
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(path = scenarios_path, pattern = "^query_[0-1]-.*\\.csv$")

while (length(scenarios) > 0) {
    
  exist_path <- "~/wei/xinyuanh/HEALED_XH_1/cp_results/pm2p5_results"
  existing_scenarios <- list.files(path = exist_path)
  existing_scenarios <- paste0(substr(existing_scenarios, 1, 21), substr(existing_scenarios, 28, 31))
  scenarios <- setdiff(scenarios, existing_scenarios)
  
  numCores <- detectCores()
  registerDoParallel(numCores)
  
  print(paste0(length(scenarios), " scenarios to go!"))
  
  puppet_results <- foreach (s = scenarios) %dopar% {
    scenario_input <- strsplit(substr(s, 7, 21), split = "-")[[1]]
    carbon_tax <- scenario_input[1]
    water_runoff <- scenario_input[2]
    socioecon <- scenario_input[3]
    energy_demand <- scenario_input[4]
    aglu <- scenario_input[5]
    fossil_fuel <- scenario_input[6]
    low_energy <- scenario_input[7]
    ccs <- scenario_input[8]
    
    # 1. read in the scenario
    scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(29)), fill = TRUE, 
                           nrow = 25000, stringsAsFactors = F)  
    row_start <- which(scenario$V1 == "nonCO2 emissions by sector") + 0
    row_end <- which(scenario$V1 == "nonCO2 emissions by tech") - 1
    nonCO2 <- scenario[row_start:row_end, ]
    colnames(nonCO2) <- nonCO2[2, ]
    nonCO2 <- nonCO2[-c(1, 2), ]
    nonCO2 <- nonCO2[, !colnames(nonCO2) %in% c("", "NA")]
    
    scenario_EFTA <- read.table(paste0(scenarios_path, substr(s, 1, 6), "EFTA", substr(s, 6, nchar(s))), 
                                header = FALSE, sep = ",", col.names = paste0("V",seq_len(29)), fill = TRUE, stringsAsFactors = F)  
    row_start_EFTA <- which(scenario_EFTA$V1 == "nonCO2 emissions by sector") + 0
    row_end_EFTA <- which(scenario_EFTA$V1 == "nonCO2 emissions by tech") - 1
    nonCO2_EFTA <- scenario_EFTA[row_start_EFTA:row_end_EFTA, ]
    colnames(nonCO2_EFTA) <- nonCO2_EFTA[2, ]
    nonCO2_EFTA <- nonCO2_EFTA[-c(1, 2), ]
    nonCO2_EFTA <- nonCO2_EFTA[, !colnames(nonCO2_EFTA) %in% c("", "NA")]
    nonCO2 <- rbind(nonCO2, nonCO2_EFTA)
    
    # 2. downscale all the emissions to country level
    BC_country <- to_mapped(nonCO2, BC_EDGAR_propn, BC, "2010")
    CO_country <- to_mapped(nonCO2, CO_EDGAR_propn, CO, "2010")
    NH3_country <- to_mapped(nonCO2, NH3_EDGAR_propn, NH3, "2010")
    NMVOC_country <- to_mapped(nonCO2, NMVOC_EDGAR_propn, NMVOC, "2010")
    NOx_country <- to_mapped(nonCO2, NOx_EDGAR_propn, NOx, "2010")
    OC_country <- to_mapped(nonCO2, OC_EDGAR_propn, OC, "2010")
    PM2.5_country <- to_mapped(nonCO2, PM2.5_EDGAR_propn, "PM2.5", "2010")
    SO2_country <- to_mapped(nonCO2, SO2_EDGAR_propn, SO2, "2010")
  
    CH4_country <- to_mapped(nonCO2, CH4_EDGAR_propn, CH4, "2010")
    CO2_country <- to_mapped(nonCO2, CO2_EDGAR_propn, "CO2", "2010")
  
    for (year in as.character(2010 + seq(1, 18) * 5)) {
      BC_country[, year] <- to_mapped(nonCO2, BC_EDGAR_propn, BC, year)[, 2]
      CO_country[, year] <- to_mapped(nonCO2, CO_EDGAR_propn, CO, year)[, 2]
      NH3_country[, year] <- to_mapped(nonCO2, NH3_EDGAR_propn, NH3, year)[, 2]
      NMVOC_country[, year] <- to_mapped(nonCO2, NMVOC_EDGAR_propn, NMVOC, year)[, 2]
      NOx_country[, year] <- to_mapped(nonCO2, NOx_EDGAR_propn, NOx, year)[, 2]
      OC_country[, year] <- to_mapped(nonCO2, OC_EDGAR_propn, OC, year)[, 2]
      PM2.5_country[, year] <- to_mapped(nonCO2, PM2.5_EDGAR_propn, "PM2.5", year)[, 2]
      SO2_country[, year] <- to_mapped(nonCO2, SO2_EDGAR_propn, SO2, year)[, 2]
  
      CH4_country[, year] <- to_mapped(nonCO2, CH4_EDGAR_propn, CH4, year)[, 2]
      CO2_country[, year] <- to_mapped(nonCO2, CO2_EDGAR_propn, "CO2", year)[, 2]
    }
    
    # 3. emissions to pm
    output <- list()
    for (year in as.character(2010 + seq(0, 18) * 5)) {
      input_init <- FASST_mapping %>% left_join(BC_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("BC" = year) %>%
        left_join(CH4_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("CH4" = year) %>%
        left_join(NH3_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("NH3" = year) %>%
        left_join(NOx_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("NOx" = year) %>%
        left_join(OC_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("OC" = year) %>%
        left_join(SO2_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("SO2" = year) %>%
        left_join(NMVOC_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("NMVOC" = year) %>%
        left_join(PM2.5_country %>% select(ISO_A3, year), by = c("ISO_A3")) %>% rename("PM2.5" = year)
      
      input_init[is.na(input_init)] <- 0
      
      input <- input_init %>% select(-c(`Country`, `ISO_A3`)) %>% group_by(`FASST REGION`) %>% summarise_all(funs(sum)) %>%
        mutate(OC = OC * 1.3) %>% rename("POM" = "OC")
      input <- input %>% tibble::column_to_rownames("FASST REGION")
      input["RUE", ] <- input["RUS", ] * RUE_ratio[1, ]
      input["RUS", ] <- input["RUS", ] * (1 - RUE_ratio[1, ])
      colnames(input) <- tolower(colnames(input))
      input <- input %>% rename("PM2.5" = "pm2.5")
      input <- input * 1000   # Super important!!! 1 Tg = 1000 kTonne
      
      if(nrow(input) == 56) {
        # use AR5_EM for air and ship, same as base year
        air_ship <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 7, range = "B1:I3")
        input["AIR", ] <- air_ship[1, ]
        input["SHIP", ] <- air_ship[2, ]
      }
      
      input$PM2.5 <- input$PM2.5 + input$bc + input$pom
      
      output[[year]] <- TM5_FASST(input)
  
      
    }
    output_csv <- cbind(rownames(output$`2010`), 
                        data.frame(lapply(output, function(x) x$`dPM+PM_AR5 DRY ANTHR` + x$`PM_NAT`), check.names = F))
    colnames(output_csv)[1] <- "FASST REGION"
    write.csv(output_csv, paste0("~/wei/xinyuanh/HEALED_XH_1/cp_results/pm2p5_results/", substr(s, 1, 21), "_pm2p5.csv"), row.names = F)
    
    # 4. Health Impact Assessment
    
    MORT_summary <- list()
    # output <- lapply(output, tibble::rownames_to_column, var = "FASST REGION")
    
    for (year in as.character(seq(2015, 2100, by = 5))) {
      
      # pm2p5 <- output[[year]] %>% mutate(PM_TOT = `dPM+PM_AR5 DRY ANTHR` + PM_NAT) %>% select(`FASST REGION`, PM_TOT)
  
      pm2p5 <- output_csv %>% select(`FASST REGION`, year)
      pm2p5$`FASST REGION` <- as.character(pm2p5$`FASST REGION`)
      colnames(pm2p5)[2] <- "PM_TOT"
      
      pm2p5 <- pm2p5 %>% left_join(FASST_mapping, by = "FASST REGION")
      
      age.str <- get(paste0("pop_SSP", as.numeric(socioecon)+1)) %>% filter(!grepl(".*Education$", VARIABLE)) %>% filter(grepl(".*Aged.*", VARIABLE))
      # age.str.num <- unlist(strsplit(as.character(age.str$VARIABLE), split = "Aged"))[c(FALSE, TRUE)]
      # age.str$VARIABLE <- age.str.num
      age.str <- age.str %>% mutate(VARIABLE = sub(".*Aged", "", VARIABLE))
      age.str <- age.str %>% select(REGION, VARIABLE, X2010, X2015, X2020, X2025, X2030, X2035, X2040, X2045, 
                                    X2050, X2055, X2060, X2065, X2070, X2075, X2080, X2085, X2090, X2095, X2100)
      age.str.year <- age.str %>% select(REGION, VARIABLE, paste0("X", year))
      age.str.year <- age.str.year %>% group_by(REGION, VARIABLE) %>% summarise_all(funs(sum)) %>% ungroup() # sum up male and female
      age.str.year <- age.str.year %>% spread(VARIABLE, paste0("X", year)) # get population by age, each country
      
      pm2p5$expo_level <- exposure_level(pm2p5$PM_TOT)
      pm2p5 <- pm2p5 %>% left_join(age.str.year, by = c("ISO_A3" = "REGION")) %>% drop_na()
      pm2p5 <- pm2p5 %>% mutate(`95-99` = `95-99` + `100+`)
      pm2p5 <- pm2p5 %>% rename("95+" = "95-99")
      pm2p5 <- pm2p5 %>% select(`FASST REGION`, PM_TOT, Country, ISO_A3, expo_level, 
                                `0-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`, `30-34`, `35-39`, 
                                `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75-79`, 
                                `80-84`, `85-89`, `90-94`, `95+`)
      
      common.region <- intersect(unique(pm2p5$ISO_A3), unique(COPD_SSP1_mort_0$ISO_A3))
      pm2p5 <- pm2p5 %>% filter(ISO_A3 %in% common.region) %>% arrange(ISO_A3) 
      
      for (d in diseases) {
        if(d %in% c("COPD", "DB", "LC", "LRI")) {
          RR <- get(paste0("RR_", d))
          RR <- RR[, 1:2]
          pm2p5_d <- match_RR(pm2p5, RR, d)
          colnames(pm2p5_d)[ncol(pm2p5_d)] <- "RR"
          pm2p5_d <- pm2p5_d %>% mutate(AF = (RR - 1) / RR)
          base.mort.d <- get(paste0(d, "_SSP", as.numeric(socioecon)+1, "_mort_0"))
          base.mort.d <- base.mort.d %>% filter(ISO_A3 %in% common.region, year == !!year)
          pm2p5_d_mort <- cbind(pm2p5_d[, 1:5], pm2p5_d[, 6:25] * base.mort.d[, 3:22] / 1e3 * pm2p5_d$AF * 1e6)
          # pm2p5_d_mort$DMORT <- apply(pm2p5_d_mort[, 6:25], 1, sum, na.rm =T)
          pm2p5_d_mort$YEAR <- year
          pm2p5_d_mort$metric <- "Deaths"
        } else { # d in IHD, Stroke
          RR <- get(paste0("RR_", d))
          RR <- RR[, c(1, 2 + 0:14 * 3)] # ignore the lower and upper bounds
          RR <- cbind(RR[, 1], as.data.frame(matrix(1, nrow(RR), 5)), RR[, -1])
          pm2p5_d <- match_RR(pm2p5, RR, d)
          pm2p5_d[, 26:45] <- (pm2p5_d[, 26:45] - 1) / pm2p5_d[, 26:45] # calculate AF
          base.mort.d <- get(paste0(d, "_SSP", as.numeric(socioecon)+1, "_mort_0"))
          base.mort.d <- base.mort.d %>% filter(ISO_A3 %in% common.region, year == !!year)
          pm2p5_d_mort <- cbind(pm2p5_d[, 1:5], pm2p5_d[, 6:25] * base.mort.d[, 3:22] / 1e3 * pm2p5_d[, 26:45] * 1e6)
          # pm2p5_d_mort$DMORT <- apply(pm2p5_d_mort[, 6:25], 1, sum, na.rm = T)
          pm2p5_d_mort$YEAR <- year
          pm2p5_d_mort$metric <- "Deaths"
        }
        assign(paste0("pm2p5_", d, "_mort"), pm2p5_d_mort[, c(3:4, 26:27, 11:25)])
      }
      MORT_summary[[year]] <- cbind(pm2p5_COPD_mort[, 1:4], 
                                    pm2p5_COPD_mort[, 5:19] + pm2p5_DB_mort[, 5:19] + pm2p5_IHD_mort[, 5:19] +
                                      pm2p5_LC_mort[, 5:19] + pm2p5_LRI_mort[, 5:19] + pm2p5_Stroke_mort[, 5:19]) %>% data.frame(stringsAsFactors = F)
    }
    
    MORT_summary_all <- data.table::rbindlist(MORT_summary)
    
    write.csv(MORT_summary_all, paste0("~/wei/xinyuanh/HEALED_XH_1/cp_results/results_05102022/", substr(s, 1, 21), "_all.csv"), row.names = F)
    
    if (which(scenarios == s) %% 100 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    
    return()
  }
  
  
  stopImplicitCluster()
}
