library(readxl)
library(dplyr)

AR5_EM <-read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 7) %>% tibble::column_to_rownames("SOURCE REGION")
AR5_CONC <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 8) %>% tibble::column_to_rownames("RECEPTOR REGION")
CH4_TO_SDMA8_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 9) %>% tibble::column_to_rownames("COUNTRY")
NOx_TO_SDMA8_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 10) %>% tibble::column_to_rownames("COUNTRY")
NMVOC_TO_SDMA8_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 11) %>% tibble::column_to_rownames("COUNTRY")
SO2_TO_SDMA8_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 12) %>% tibble::column_to_rownames("COUNTRY")
NH3_TO_NO3_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 13) %>% tibble::column_to_rownames("COUNTRY")
NH3_TO_NH4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 14) %>% tibble::column_to_rownames("COUNTRY")
NH3_TO_SO4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 15) %>% tibble::column_to_rownames("COUNTRY")
NOx_TO_NH4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 16) %>% tibble::column_to_rownames("COUNTRY")
NOx_TO_NO3_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 17) %>% tibble::column_to_rownames("COUNTRY")
NOx_TO_SO4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 18) %>% tibble::column_to_rownames("COUNTRY")
SO2_TO_SO4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 19) %>% tibble::column_to_rownames("COUNTRY")
SO2_TO_NH4_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 20) %>% tibble::column_to_rownames("COUNTRY")
SO2_TO_NO3_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 21) %>% tibble::column_to_rownames("COUNTRY")
PRPM25_TO_PRPM25_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 22) %>% tibble::column_to_rownames("COUNTRY")
BC_TO_BC_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 23) %>% tibble::column_to_rownames("COUNTRY")
POM_TO_POM_POPW_perkT <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 24) %>% tibble::column_to_rownames("COUNTRY")

# This is a tentative input - it should be changed to output from GCAM
# input <- read_xlsx(path = "NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 4) %>% tibble::column_to_rownames("kTonne/year")

TM5_FASST <- function(input) {
  colnames(input) <- toupper(colnames(input))
  
  # Calculate delta for each emission
  delta_EM_AR5 <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 5) %>% tibble::column_to_rownames("SOURCE REGION")
  delta_EM_AR5 <- input[match(rownames(delta_EM_AR5), rownames(input)), ] - AR5_EM[match(rownames(delta_EM_AR5), rownames(AR5_EM)), ]
  delta_EM_AR5$PM2.5 <- delta_EM_AR5$PM2.5 - delta_EM_AR5$BC - delta_EM_AR5$POM
  
  output <- read_xlsx(path = "TM5_SRMs/NORMALIZED_SR_PM_O3_POPW.xlsx", sheet = 2, skip = 3) %>% tibble::column_to_rownames("RECEPTOR")
  output$`SO4(SO2)` <- apply(SO2_TO_SO4_POPW_perkT[, match(rownames(output), colnames(SO2_TO_SO4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$SO2))
  output$`NO3(SO2)` <- apply(SO2_TO_NO3_POPW_perkT[, match(rownames(output), colnames(SO2_TO_NO3_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$SO2))
  output$`NH4(SO2)` <- apply(SO2_TO_NH4_POPW_perkT[, match(rownames(output), colnames(SO2_TO_NH4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$SO2))
  
  output$`SO4(NOx)` <- apply(NOx_TO_SO4_POPW_perkT[, match(rownames(output), colnames(NOx_TO_SO4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NOX))
  output$`NO3(NOx)` <- apply(NOx_TO_NO3_POPW_perkT[, match(rownames(output), colnames(NOx_TO_NO3_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NOX))
  output$`NH4(NOx)` <- apply(NOx_TO_NH4_POPW_perkT[, match(rownames(output), colnames(NOx_TO_NH4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NOX))
  
  output$`SO4(NH3)` <- apply(NH3_TO_SO4_POPW_perkT[, match(rownames(output), colnames(NH3_TO_SO4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NH3))
  output$`NO3(NH3)` <- apply(NH3_TO_NO3_POPW_perkT[, match(rownames(output), colnames(NH3_TO_NO3_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NH3))
  output$`NH4(NH3)` <- apply(NH3_TO_NH4_POPW_perkT[, match(rownames(output), colnames(NH3_TO_NH4_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$NH3))
  
  output$`BC(BC)` <- apply(BC_TO_BC_POPW_perkT[, match(rownames(output), colnames(BC_TO_BC_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$BC))
  output$`POM(POM)` <- apply(POM_TO_POM_POPW_perkT[, match(rownames(output), colnames(POM_TO_POM_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$POM))
  output$`other PrPM2.5(prPM)` <- apply(PRPM25_TO_PRPM25_POPW_perkT[, match(rownames(output), colnames(PRPM25_TO_PRPM25_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$PM2.5))
  
  output$`TOTAL ANTHR dPM DRY` <- output$`SO4(SO2)` + output$`NO3(SO2)` + output$`NH4(SO2)` +
    output$`SO4(NOx)` + output$`NO3(NOx)` + output$`NH4(NOx)` + output$`SO4(NH3)` + output$`NO3(NH3)` + 
    output$`NH4(NH3)` + output$`BC(BC)` + output$`POM(POM)` + output$`other PrPM2.5(prPM)`
  
  output$`dPM+PM_AR5 DRY ANTHR` <- pmax(AR5_CONC$SO4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`SO4(SO2)` + output$`SO4(NOx)` + output$`SO4(NH3)`, 0) +
    pmax(AR5_CONC$NO3_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NO3(SO2)` + output$`NO3(NOx)` + output$`NO3(NH3)`, 0) + 
    pmax(AR5_CONC$NH4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NH4(SO2)` + output$`NH4(NOx)` + output$`NH4(NH3)`, 0) + 
    pmax(AR5_CONC$BC_POP[match(rownames(output), rownames(AR5_CONC))] + output$`BC(BC)`, 0) + 
    pmax(AR5_CONC$POM_POP[match(rownames(output), rownames(AR5_CONC))] + output$`POM(POM)`, 0) + 
    output$`other PrPM2.5(prPM)`
  
  output$`residual H2O(35%) ANTHR` <- (pmax(AR5_CONC$SO4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`SO4(SO2)` + output$`SO4(NOx)` + output$`SO4(NH3)`, 0) +
                                         pmax(AR5_CONC$NO3_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NO3(SO2)` + output$`NO3(NOx)` + output$`NO3(NH3)`, 0) + 
                                         pmax(AR5_CONC$NH4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NH4(SO2)` + output$`NH4(NOx)` + output$`NH4(NH3)`, 0)) * 0.27
  
  output$`residual H2O(50%) ANTHR` <- (pmax(AR5_CONC$SO4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`SO4(SO2)` + output$`SO4(NOx)` + output$`SO4(NH3)`, 0) +
                                         pmax(AR5_CONC$NO3_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NO3(SO2)` + output$`NO3(NOx)` + output$`NO3(NH3)`, 0) + 
                                         pmax(AR5_CONC$NH4_POP[match(rownames(output), rownames(AR5_CONC))] + output$`NH4(SO2)` + output$`NH4(NOx)` + output$`NH4(NH3)`, 0)) * 0.43
  
  output$PM_NAT <- AR5_CONC$DUST_POP[match(rownames(output), rownames(AR5_CONC))] + AR5_CONC$SS_POP[match(rownames(output), rownames(AR5_CONC))]
  
  output$`residual H2O(35%) SS` <- AR5_CONC$SS_POP[match(rownames(output), rownames(AR5_CONC))] * 0.15
  
  output$`residual H2O(50%) SS` <- AR5_CONC$SS_POP[match(rownames(output), rownames(AR5_CONC))] * 0.27
  
  output$`SDMA8(NOx)` <- apply(NOx_TO_SDMA8_POPW_perkT[, match(rownames(output), colnames(NOx_TO_SDMA8_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$nox))
  output$`SDMA8(NMVOC)` <- apply(NMVOC_TO_SDMA8_POPW_perkT[, match(rownames(output), colnames(NMVOC_TO_SDMA8_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$nmvoc))
  output$`SDMA8(SO2)` <- apply(SO2_TO_SDMA8_POPW_perkT[, match(rownames(output), colnames(SO2_TO_SDMA8_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$so2))
  output$`SDMA8(CH4)` <- apply(CH4_TO_SDMA8_POPW_perkT[, match(rownames(output), colnames(CH4_TO_SDMA8_POPW_perkT))], 2, function(x) sum(x * delta_EM_AR5$ch4))
  
  output$`TOTAL dSDMA8 (ppb)` <- output$`SDMA8(NOx)` + output$`SDMA8(NMVOC)` + output$`SDMA8(SO2)` + output$`SDMA8(CH4)`
  output$`dSDMA8 + SDMA8_AR5` <- output$`TOTAL dSDMA8 (ppb)` + AR5_CONC$SDMA8[match(rownames(output), rownames(AR5_CONC))]
  return(output)
}

