library(maps)
diseases <- c("COPD", "DB", "IHD", "LC", "LRI", "Stroke")

# read 8h baseline mortality rates for all the 6 deseases in year 2015, from GBD 2017, 
all_mort_0 <- read.csv("~/Documents/PSU/2020 Summer/Research/HEALED_1_CODES/HIA/Diseases Allocations.csv", stringsAsFactors = F)

disease_map <- function(d) {
  if (d == "COPD") return("Chronic obstructive pulmonary disease")
  if (d == "DB") return("Diabetes mellitus type 2")
  if (d == "IHD") return("Ischemic heart disease")
  if (d == "LC") return("Tracheal, bronchus, and lung cancer")
  if (d == "LRI") return("Lower respiratory infections")
  if (d == "Stroke") return("Stroke")
  return(NA)
}

disease_map_IF <- function(d) {
  if (d == "COPD") return("Resp")
  if (d == "DB") return("Diabetes")
  if (d == "IHD") return("CardioVas")
  if (d == "LC") return("MaligNeo")
  if (d == "LRI") return("RespInfec")
  if (d == "Stroke") return("CardioVas")
  return(NA)
}

disease_map_GBD_cat <- function(d) {
  if (d == "COPD") return("Chronic respiratory diseases")
  if (d == "DB") return("Diabetes mellitus")
  if (d == "IHD") return("Cardiovascular diseases")
  if (d == "LC") return("Neoplasms")
  if (d == "LRI") return("Respiratory infections and tuberculosis")
  if (d == "Stroke") return("Cardiovascular diseases")
  return(NA)
}

# Calculate the time-varying base mortality rate (mort_0) for each disease, by country, by age group   
# mort_0_d = d_SSP (from IF) * d_mort_0 (from GBD) / d_cat_mort_0 (from GBD)

for (d in diseases) {
  for (i in 1:5) {
    # data for 2015, 2025, ..., 2095, 2100
    d_SSP <- read.csv(paste0("~/Documents/PSU/2020 Summer/Research/HEALED_1_CODES/International Futures (data)/", 
                             disease_map_IF(d), "_SSP", i, ".CSV"), skip = 2, header = F, stringsAsFactors = F)
    d_SSP <- t(d_SSP)
    colnames(d_SSP) <- c("country", "d_cat", "gender", "year", "ssp", d_SSP[1, -(1:5)])
    d_SSP <- d_SSP[-1, -c(6, 27)]
    d_SSP <- as.data.frame(d_SSP, stringsAsFactors = F)
    # modify names for some countries: DO
    d_SSP$country <- c(d_SSP$country[c(-seq(361, 380, 2), -seq(381, 400, 2), -seq(1102, 1121, 2))], rep(NA, 30))
    d_SSP <- d_SSP %>% drop_na()
    # d_SSP <- d_SSP[rowSums(is.na(d_SSP)) != ncol(d_SSP), ]
    d_SSP[, 6:25] <- apply(d_SSP[, 6:25], 2, as.numeric)

    
    # data for 2020, 2030, ..., 2090
    d_SSP_a <- read.csv(paste0("~/Documents/PSU/2020 Summer/Research/HEALED_1_CODES/International Futures (data)/", 
                               disease_map_IF(d), "_SSP", i, "_20_90.CSV"), skip = 2, header = F, stringsAsFactors = F)
    d_SSP_a <- t(d_SSP_a)
    colnames(d_SSP_a) <- c("country", "d_cat", "gender", "year", "ssp", d_SSP_a[1, -(1:5)])
    d_SSP_a <- d_SSP_a[-1, -c(6, 27)]
    d_SSP_a <- as.data.frame(d_SSP_a, stringsAsFactors = F)
    # modify names for some countries
    d_SSP_a$country <- c(d_SSP_a$country[c(-seq(289, 304, 2), -seq(305, 320, 2), -seq(882, 896, 2))], rep(NA, 24))
    d_SSP_a <- d_SSP_a %>% drop_na()
    # d_SSP_a <- d_SSP_a[rowSums(is.na(d_SSP_a)) != ncol(d_SSP_a), ]
    d_SSP_a[, 6:25] <- apply(d_SSP_a[, 6:25], 2, as.numeric)
    
    d_SSP <- rbind(d_SSP, d_SSP_a)
    d_SSP <- d_SSP %>% arrange(country, year)
    d_SSP$ISO_A3 <- iso.alpha(d_SSP$country, 3)
    
    d_SSP <- d_SSP %>% mutate(ISO_A3 = replace(ISO_A3, country == " Democratic Republic of", "COD")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == " Republic of", "COG")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Kosovo", "RKS")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Cote d'Ivoire", "CIV")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Equa Guinea", "GNQ")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Hong Kong", "HKG")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Korea North", "PRK")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Korea South", "KOR")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Papua NG", "PNG")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, country == "SierraLeo", "SLE")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, country == "Slovak Rep", "SVK")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, country == "St. Lucia", "LCA")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "St. Vincent and the Grenadines", "VCT")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "UAE", "ARE")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Unitd Kingdm", "GBR")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "GuineaBiss", "GNB")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Nigeria", "NGA")) %>%
      mutate(ISO_A3 = replace(ISO_A3, country == "Sudan South", "SSD"))
      
    
    d_mort_0 <- all_mort_0 %>% filter(cause == disease_map(d))
    d_mort_0 <- d_mort_0 %>% arrange(location, age) %>% select(-upper, -lower)
    d_mort_0 <- d_mort_0 %>% spread(age, val)
    if(d %in% c("COPD", "LRI", "Stroke")) {
      d_mort_0 <- d_mort_0[, c(1:6, 26, 15, 7:14, 16:25)]
    } else {
      d_mort_0$`Under 5` <- 0
      d_mort_0$`5 to 9` <- 0
      d_mort_0$`10 to 14` <- 0
      d_mort_0 <- d_mort_0[, c(1:6, 24:26, 7:23)]
    }
    d_mort_0$ISO_A3 <- iso.alpha(d_mort_0$location, 3)
    d_mort_0 <- d_mort_0 %>% mutate(ISO_A3 = replace(ISO_A3, location == "Federated States of Micronesia", "FSM")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "United Kingdom", "GBR")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "United States", "USA")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "The Bahamas", "BHS")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "Congo", "COG")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "Cote d'Ivoire", "CIV")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "The Gambia", "GMB")) %>%
      mutate(ISO_A3 = replace(ISO_A3, location == "Virgin Islands, U.S.", "VIR")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, location == "Nigeria", "NGA")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, location == "Guinea-Bissau", "GNB")) %>% 
      mutate(ISO_A3 = replace(ISO_A3, location == "Dominican Republic", "DOM"))
    
    d_cat_mort_0 <- all_mort_0 %>% filter(cause == disease_map_GBD_cat(d))
    d_cat_mort_0 <- d_cat_mort_0 %>% arrange(location, age) %>% select(-upper, -lower)
    d_cat_mort_0 <- d_cat_mort_0 %>% spread(age, val)
    d_cat_mort_0 <- d_cat_mort_0[, c(1:6, 26, 15, 7:14, 16:25)]
    d_cat_mort_0$ISO_A3 <- d_mort_0$ISO_A3
    
    countries_avail <- intersect(d_mort_0$ISO_A3, unique(d_SSP$ISO_A3))
    d_SSP <- d_SSP %>% filter(ISO_A3 %in% countries_avail) %>% arrange(year, ISO_A3)
    d_mort_0 <- d_mort_0 %>% filter(ISO_A3 %in% countries_avail) %>% arrange(ISO_A3)
    d_cat_mort_0 <- d_cat_mort_0 %>% filter(ISO_A3 %in% countries_avail) %>% arrange(ISO_A3)
    
    # key calculation
    d_SSP_mort_0 <- d_SSP[, 6:25] * 
      d_mort_0[rep(1:nrow(d_mort_0), nrow(d_SSP)/nrow(d_mort_0)), 7:26] / 
      d_cat_mort_0[rep(1:nrow(d_cat_mort_0), nrow(d_SSP)/nrow(d_cat_mort_0)), 7:26]
    d_SSP_mort_0 <- cbind(d_SSP$ISO_A3, d_SSP$year, d_SSP_mort_0)
    colnames(d_SSP_mort_0) <- c("ISO_A3", "year", colnames(d_mort_0)[7:26])
    write.csv(d_SSP_mort_0, paste0("~/Documents/PSU/2020 Summer/Research/HEALED_1_CODES/HIA/", d, "_SSP", i, "_mort_0.csv"), row.names = F)
  }
}
