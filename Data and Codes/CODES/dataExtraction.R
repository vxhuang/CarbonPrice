library(dplyr)
library(tidyr)
library(readxl)
library(doParallel)


# 1. Temperature
library(dplyr)
library(doParallel)
setwd("~/work/HEALED_XH_1/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-1]-[0-3].*\\.csv$")
numCores <- detectCores()
registerDoParallel(numCores)

temp_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    if(substr(s, 7, 7) == 0) {
      scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(30)), fill = TRUE, 
                             skip = 4000, nrow = 1000, stringsAsFactors = F)  
    } else {
      scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(30)), fill = TRUE, 
                             skip = 5250, nrow = 1000, stringsAsFactors = F)  
    }
    row_start <- which(scenario$V1 == "global mean temperature") + 0
    row_end <- which(scenario$V1 == "total climate forcing") - 1
    temp <- scenario[row_start:row_end, ]
    colnames(temp) <- temp[2, ]
    temp <- temp[-c(1, 2), ]
    temp <- temp[, !colnames(temp) %in% c("", "NA")]
    temp <- temp %>% select(-`1980`, -`1985`, -`1990`, -`1995`, -`2000`, -`2005`, -`2010`, -`Units`)
    temp[, 4:ncol(temp)] <- apply(temp[, 4:ncol(temp)], 2, as.numeric)
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    temp}
    , error = function(c) "error"
  )
}
stopImplicitCluster()

temp_df <- data.table::rbindlist(temp_All)

write.csv(temp_df, "temp_1020.csv", row.names = F)

# 2. SO2
library(dplyr)
library(doParallel)
setwd("~/work/HEALED_XH_1/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

SO2_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
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
    
    # OC <- c("OC", "OC_AGR", "OC_AWB")
    # BC <- c("BC", "BC_AGR", "BC_AWB")
    SO2 <- c("SO2", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB", "SO2_4", "SO2_4_AWB")
    SO2_result <- nonCO2 %>% filter(GHG %in% SO2)
    
    SO2_result$`2050` <- as.numeric(SO2_result$`2050`)
    SO2_result <- SO2_result %>% select(region, `2050`) %>% group_by(region) %>% summarise(SO2 = sum(`2050`))
    # SO2_result
    
    
    SO2_result$scenario_input <- s
    
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    
    SO2_result
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()
SO2_All <- data.table::rbindlist(SO2_All)
write.csv(SO2_All, "~/wei/xinyuanh/HEALED_XH_1/cp_results/SO2_All.csv", row.names = F)


# 3. Primary energy consumption
library(dplyr)
library(doParallel)
library(readxl)
setwd("~/work/HEALED_1_CODES/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_08232021_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

primary_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(26)), fill = TRUE, stringsAsFactors = F)
    row_start <- which(scenario$V1 == "primary energy consumption by region (direct equivalent)") + 0
    row_end <- which(scenario$V1 == "aggregated land allocation") - 1
    primary <- scenario[row_start:row_end, ]
    colnames(primary) <- primary[2, ]
    primary <- primary[-c(1, 2), ]
    primary <- primary[, !colnames(primary) %in% c("", "NA")]
    primary <- primary %>% select(-`1990`, -`2005`, -`2010`, -`Units`)
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    primary}
    , error = function(c) "error"
  )
}
stopImplicitCluster()
primary_All <- data.table::rbindlist(primary_All)
write.csv(primary_All, "~/wei/xinyuanh/HEALED_XH_1/cp_results/primary_2050.csv", row.names = F)


# 4. PM2.5 by country
scenarios_path <- "~/wei/xinyuanh/HEALED_XH_1/cp_results/pm2p5_results/"
scenarios <- list.files(scenarios_path)
numCores <- detectCores()
registerDoParallel(numCores)

pm_country_data <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    carbon_tax <- substr(s, 7, 7)
    socioecon <- substr(s, 11, 11)
    scenario <- scenario %>% select(`FASST REGION`, `2050`)
    colnames(scenario)[2] <- "PM_TOT"
    scenario <- scenario %>% left_join(FASST_mapping, by = "FASST REGION")
    scenario <- scenario %>% left_join(region_mapping %>% select(ISO_A3, GCAM), by = "ISO_A3") %>% drop_na()
    scenario <- scenario %>% filter(ISO_A3 %in% common.region)
    
    scenario$scenario_input <- s
    scenario$carbon_tax <- carbon_tax
    scenario$socioecon <- socioecon
    
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    if (dim(scenario)[1] != 177) print(paste0(which(scenarios == s), ": ", s))
    if (dim(scenario)[2] != 8) print(paste0(which(scenarios == s), ": ", s))
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()
pm_country_data <- data.table::rbindlist(pm_country_data)
write.csv(pm_country_data, "~/wei/xinyuanh/HEALED_XH_1/cp_results/pm2p5_country_2050.csv", row.names = F)


# 5. OC by aggregate sectors

library(dplyr)
library(doParallel)
library(readxl)
setwd("~/work/HEALED_XH_1/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")
sector_mapping_GCAM <- read_xlsx("GCAM emissions matching/sector_mapping.xlsx", sheet = 1)

numCores <- detectCores()
registerDoParallel(numCores)

OC_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario_input <- strsplit(substr(s, 7, 21), split = "-")[[1]]
    carbon_tax <- scenario_input[1]
    water_runoff <- scenario_input[2]
    socioecon <- scenario_input[3]
    energy_demand <- scenario_input[4]
    aglu <- scenario_input[5]
    fossil_fuel <- scenario_input[6]
    low_energy <- scenario_input[7]
    ccs <- scenario_input[8]
    print(s)
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
    
    OC <- c("OC", "OC_AGR", "OC_AWB")
    # SO2 <- c("SO2", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB", "SO2_4", "SO2_4_AWB")
    OC_result <- nonCO2 %>% filter(GHG %in% OC)
    OC_result <- OC_result %>% left_join(sector_mapping_GCAM, by = c("sector" = "GCAM"))
    
    OC_result$`2050` <- as.numeric(OC_result$`2050`)
    OC_result <- OC_result %>% select(region, major_sector, `2050`) %>% group_by(region, major_sector) %>%
      summarise(OC = sum(`2050`), .groups = "drop")
    
    OC_result$scenario_input <- s
    
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    
    OC_result
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()
OC_All <- data.table::rbindlist(OC_All)
write.csv(OC_All, "OC_sectors_All.csv", row.names = F)


# 6. OC emissions from deforestation
library(dplyr)
library(doParallel)
library(readxl)
setwd("~/work/HEALED_XH_1")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")
sector_mapping_GCAM <- read_xlsx("GCAM emissions matching/sector_mapping.xlsx", sheet = 1)

numCores <- detectCores()
registerDoParallel(numCores)


OC_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario_input <- strsplit(substr(s, 7, 21), split = "-")[[1]]
    carbon_tax <- scenario_input[1]
    water_runoff <- scenario_input[2]
    socioecon <- scenario_input[3]
    energy_demand <- scenario_input[4]
    aglu <- scenario_input[5]
    fossil_fuel <- scenario_input[6]
    low_energy <- scenario_input[7]
    ccs <- scenario_input[8]
    print(s)
    # 1. read in the scenario
    if(substr(s, 7, 7) == 0) {
      scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(29)), fill = TRUE,
                             skip = 14000, nrow = 175000, stringsAsFactors = F)
    } else {
      scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(29)), fill = TRUE,
                             skip = 15000, nrow = 180000, stringsAsFactors = F)
    }
    row_start <- which(scenario$V1 == "nonCO2 emissions by tech") + 0
    row_end <- which(scenario$V1 == "population by region") - 1
    nonCO2 <- scenario[row_start:row_end, ]
    colnames(nonCO2) <- nonCO2[2, ]
    nonCO2 <- nonCO2[-c(1, 2), ]
    nonCO2 <- nonCO2[, !colnames(nonCO2) %in% c("", "NA")]
    
    scenario_EFTA <- read.table(paste0(scenarios_path, substr(s, 1, 6), "EFTA", substr(s, 6, nchar(s))), 
                                header = FALSE, sep = ",", col.names = paste0("V",seq_len(29)), fill = TRUE, stringsAsFactors = F)  
    row_start_EFTA <- which(scenario_EFTA$V1 == "nonCO2 emissions by tech") + 0
    row_end_EFTA <- which(scenario_EFTA$V1 == "population by region") - 1
    nonCO2_EFTA <- scenario_EFTA[row_start_EFTA:row_end_EFTA, ]
    colnames(nonCO2_EFTA) <- nonCO2_EFTA[2, ]
    nonCO2_EFTA <- nonCO2_EFTA[-c(1, 2), ]
    nonCO2_EFTA <- nonCO2_EFTA[, !colnames(nonCO2_EFTA) %in% c("", "NA")]
    nonCO2 <- rbind(nonCO2, nonCO2_EFTA)
    
    OC <- c("OC", "OC_AGR", "OC_AWB")
    # SO2 <- c("SO2", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB", "SO2_4", "SO2_4_AWB")
    # NOx <- c("NOx", "NOx_AGR", "NOx_AWB")
    OC_result <- nonCO2 %>% filter(GHG %in% OC)
    # OC_result <- OC_result %>% left_join(sector_mapping_GCAM, by = c("sector" = "GCAM"))
    
    OC_result$`2050` <- as.numeric(OC_result$`2050`)
    # OC_result <- OC_result %>% select(region, major_sector, `2050`) %>% group_by(region, major_sector) %>%
    #   summarise(OC = sum(`2050`), .groups = "drop")
    OC_result <- OC_result %>% filter(sector %in% c("UnmanagedLand")) %>%
      select(region, sector, subsector, `2050`) %>% group_by(region, sector, subsector) %>% 
      summarise(OC_2050 = sum(`2050`), .groups = "drop")
    # OC_result
    OC_result <- OC_result %>% mutate(subsector = sub("_.*", "", subsector))
    OC_result <- OC_result %>% filter(subsector == "Deforest")
    OC_result <- OC_result %>% group_by(region, sector, subsector) %>% 
      summarise(OC = sum(OC_2050), .groups = "drop") %>% 
      select(-sector)
    OC_result$scenario_input <- s
    
    if (which(scenarios == s) %% 100 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    
    OC_result
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()
OC_All <- data.table::rbindlist(OC_All)
write.csv(OC_All, "~/wei/xinyuanh/HEALED_XH_1/cp_results/OC_deforest.csv", row.names = F)

# 7. OC emissions (by region)
library(dplyr)
library(doParallel)
setwd("~/work/HEALED_XH_1/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

OC_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
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
    
    OC <- c("OC", "OC_AGR", "OC_AWB")
    # BC <- c("BC", "BC_AGR", "BC_AWB")
    # SO2 <- c("SO2", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB", "SO2_4", "SO2_4_AWB")
    OC_result <- nonCO2 %>% filter(GHG %in% OC)
    
    
    
    OC_result$`2050` <- as.numeric(OC_result$`2050`)
    OC_result <- OC_result %>% select(region, `2050`) %>% group_by(region) %>% summarise(OC = sum(`2050`))
    # OC_result
    
    
    OC_result$scenario_input <- s
    
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    
    OC_result
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()
OC_All <- data.table::rbindlist(OC_All)
write.csv(OC_All, "~/wei/xinyuanh/HEALED_XH_1/cp_results/OC_All.csv", row.names = F)


# 8. Aggregate land use in 2050
library(dplyr)
library(doParallel)
library(readxl)
setwd("~/work/HEALED_XH_1/HEALED_1_CODES/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_08232021_[0-9]-0.*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

land_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(26)), fill = TRUE, stringsAsFactors = F)
    row_start <- which(scenario$V1 == "aggregated land allocation") + 0
    row_end <- nrow(scenario)
    land <- scenario[row_start:row_end, ]
    colnames(land) <- land[2, ]
    land <- land[-c(1, 2), ]
    land <- land[, !colnames(land) %in% c("", "NA")]
    land <- land %>% select(-`1990`, -`2005`, -`2010`, -`Units`)
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    land}
    , error = function(c) "error"
  )
}
stopImplicitCluster()
land_All <- data.table::rbindlist(land_All)
write.csv(land_All, "land.csv", row.names = F)

#### land use in 2050
library(dplyr)
library(doParallel)
library(readxl)
setwd("~/work/HEALED_XH_1/")
scenarios_path <- "/gpfs/group/wvp5117/default/GCAM_xh/query_out/"
scenarios <- list.files(scenarios_path, pattern = "^query_08232021_[0-1]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

land_All <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.table(paste0(scenarios_path, s), header = FALSE, sep = ",", col.names = paste0("V",seq_len(26)), fill = TRUE, stringsAsFactors = F)
    row_start <- which(scenario$V1 == "aggregated land allocation") + 0
    row_end <- nrow(scenario)
    land <- scenario[row_start:row_end, ]
    colnames(land) <- land[2, ]
    land <- land[-c(1, 2), ]
    land <- land[, !colnames(land) %in% c("", "NA")]
    land <- land %>% select(region, LandLeaf, `2050`)
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    land$scenario <- s
    land}
    , error = function(c) "error"
  )
}
stopImplicitCluster()
land_All <- data.table::rbindlist(land_All)
write.csv(land_All, "land_2050.csv", row.names = F)


# 9. death rates (by region) in 2050
scenarios_path = "~/wei/xinyuanh/HEALED_XH_1/cp_results/results_05102022/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

fig72_data <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    socioecon <- substr(s, 11, 11)
    scenario$deaths_total <- rowSums(scenario[, 5:19])
    scenario <- scenario %>% select(1:4, deaths_total)
    
    pop_all_yr <- pop_all %>% filter(SCENARIO == socioecon) %>% arrange(year, REGION)
    pop_all_yr$pop_total <- rowSums(pop_all_yr[, 4:18])
    pop_all_yr <- pop_all_yr %>% select(1:3, pop_total)
    
    scenario <- scenario %>% cbind(pop_all_yr %>% select(pop_total))
    scenario <- scenario %>% left_join(region_mapping %>% select(ISO_A3, GCAM), by = "ISO_A3")
    scenario <- scenario %>% mutate(GCAM = if_else(ISO_A3 == "PRI", "USA", GCAM))
    
    scenario <- scenario %>% filter(YEAR == "2050") %>% select(GCAM, metric, deaths_total, pop_total)
    scenario <- scenario %>% group_by(GCAM, metric) %>% summarise_at(c("deaths_total", "pop_total"), sum, na.rm = T)
    scenario$death_rate <- scenario$deaths_total / scenario$pop_total
    ## can be filtered for 4 regions only
    scenario$scenario_input <- s
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()

fig72_data <- data.table::rbindlist(fig72_data)
write.csv(fig72_data, "~/wei/xinyuanh/HEALED_XH_1/cp_results/fig72_data.csv", row.names = F)



# 10. death rate time series, global death rates in 2050 and 2100

scenarios_path = "~/wei/xinyuanh/HEALED_XH_1/cp_results/results_05102022/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

dr_ts <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    socioecon <- substr(s, 11, 11)
    scenario$deaths_total <- rowSums(scenario[, 5:19])
    scenario <- scenario %>% filter(metric == "Deaths") %>% select(YEAR, deaths_total) %>% 
      group_by(YEAR) %>% summarise(deaths_total = sum(deaths_total, na.rm = T))
    
    pop_all_global$total_pop <- rowSums(pop_all_global[, 4:18])
    scenario$total_pop <- (pop_all_global %>% filter(SCENARIO == socioecon))$total_pop
    scenario <- scenario %>% mutate(death_rate = deaths_total / total_pop)
    scenario$scenario_input <- s
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()

dr_ts <- data.table::rbindlist(dr_ts)
write.csv(dr_ts, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_ts.csv", row.names = F)

dr_ts$carbon_tax <- substr(dr_ts$scenario_input, 7, 7)
substr(dr_ts$scenario_input, 7, 7) <- "X"
dr_2050_global <- dr_ts %>% filter(YEAR == "2050")
dr_2100_global <- dr_ts %>% filter(YEAR == "2100")
write.csv(dr_2050_global, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_2050_global.csv", row.names = F)
write.csv(dr_2100_global, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_2100_global.csv", row.names = F)
dr_ts <- dr_ts %>% select(YEAR, carbon_tax, death_rate) %>% group_by(YEAR, carbon_tax) %>%
  summarise(min = min(death_rate), median = median(death_rate), max = max(death_rate), .groups = "drop")
write.csv(dr_ts, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_ts_summary.csv", row.names = F)



# 12. death rates in 2050 (by country)
scenarios_path = "~/wei/xinyuanh/HEALED_XH_1/cp_results/results_05102022/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-[0-3].*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

dr_2050 <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    socioecon <- substr(s, 11, 11)
    scenario$deaths_total <- rowSums(scenario[, 5:19])
    scenario <- scenario %>% filter(YEAR == "2050", metric == "Deaths") %>% select(1:4, deaths_total)
    
    pop_all_yr <- pop_all %>% filter(year == "2050", SCENARIO == socioecon) %>% arrange(REGION)
    pop_all_yr$total_pop <- rowSums(pop_all_yr[, 4:18])
    pop_all_yr <- pop_all_yr %>% select(1:3, total_pop)
    
    scenario$total_pop <- pop_all_yr$total_pop
    scenario <- scenario %>% replace(is.na(.), 0)
    scenario <- scenario %>% mutate(death_rate = deaths_total / total_pop)
    scenario$scenario_input <- s
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()

dr_2050 <- data.table::rbindlist(dr_2050)
write.csv(dr_2050, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_2050.csv", row.names = F)



# 13. death rates assuming all clear-cutting
scenarios_path = "~/wei/xinyuanh/HEALED_XH_1/cp_results/results_10252022_lowEF/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-0.*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

dr_2050_lowEF <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    socioecon <- substr(s, 11, 11)
    scenario$deaths_total <- rowSums(scenario[, 5:19])
    scenario <- scenario %>% filter(YEAR == "2050", metric == "Deaths") %>% select(1:4, deaths_total)
    
    pop_all_yr <- pop_all %>% filter(year == "2050", SCENARIO == socioecon) %>% arrange(REGION)
    pop_all_yr$total_pop <- rowSums(pop_all_yr[, 4:18])
    pop_all_yr <- pop_all_yr %>% select(1:3, total_pop)
    
    scenario$total_pop <- pop_all_yr$total_pop
    scenario <- scenario %>% replace(is.na(.), 0)
    scenario <- scenario %>% mutate(death_rate = deaths_total / total_pop)
    scenario$scenario_input <- s
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    # if (dim(scenario)[1] != 62) print(paste0(which(scenarios == s), ": ", s))
    # if (dim(scenario)[2] != 4) print(paste0(which(scenarios == s), ": ", s))
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()

dr_2050_lowEF <- data.table::rbindlist(dr_2050_lowEF)
write.csv(dr_2050_lowEF, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_2050_lowEF.csv", row.names = F)




# 15. death rates in 2015
scenarios_path = "~/wei/xinyuanh/HEALED_XH_1/cp_results/results_05102022/"
scenarios <- list.files(scenarios_path, pattern = "^query_[0-9]-0.*\\.csv$")

numCores <- detectCores()
registerDoParallel(numCores)

dr_2015 <- foreach(s = scenarios) %dopar% {
  tryCatch({
    scenario <- read.csv(paste0(scenarios_path, s), header = T, stringsAsFactors = F, check.names = F)
    socioecon <- substr(s, 11, 11)
    scenario$deaths_total <- rowSums(scenario[, 5:19])
    scenario <- scenario %>% filter(YEAR == "2015", metric == "Deaths") %>% select(1:4, deaths_total)
    
    pop_all_yr <- pop_all %>% filter(year == "2015", SCENARIO == socioecon) %>% arrange(REGION)
    pop_all_yr$total_pop <- rowSums(pop_all_yr[, 4:18])
    pop_all_yr <- pop_all_yr %>% select(1:3, total_pop)
    
    scenario$total_pop <- pop_all_yr$total_pop
    scenario <- scenario %>% replace(is.na(.), 0)
    scenario <- scenario %>% mutate(death_rate = deaths_total / total_pop)
    scenario$scenario_input <- s
    if (which(scenarios == s) %% 1000 == 1) {
      print(which(scenarios == s))
      print(Sys.time())
    }
    # if (dim(scenario)[1] != 62) print(paste0(which(scenarios == s), ": ", s))
    # if (dim(scenario)[2] != 4) print(paste0(which(scenarios == s), ": ", s))
    scenario
  }, error = function(e) function(e) {cat("Index:", which(scenarios == s), "Query:", s, conditionMessage(e), "\n")}
  )
}
stopImplicitCluster()

dr_2015 <- data.table::rbindlist(dr_2015)
write.csv(dr_2015, "~/wei/xinyuanh/HEALED_XH_1/cp_results/dr_2015.csv", row.names = F)







