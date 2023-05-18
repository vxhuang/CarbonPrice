library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(maps)
library(mapdata)
library(readxl)
library(PupillometryR)
library(scales)
library(RColorBrewer)
library(ggpattern)
library(ggtext)

setwd("Data and Codes/CODES")

FASST_mapping <- read_xlsx(path = "TM5_SRMs/COUNTRY-FASST-TABLE_GENERATOR_v2.xlsx", sheet = 1, range = "A1:C231") %>% 
  rename("ISO_A3" = "ISO 3")
COPD_SSP1_mort_0 <- read.csv("HIA/COPD_SSP1_mort_0.csv", stringsAsFactors = F)

# population by age group, each year
for (i in 1:5) {
  assign(paste0("pop_SSP", i), read.csv(paste0("HIA/pop_SSP", i, ".csv"), stringsAsFactors = F))
}

common.region <- setdiff(intersect(unique(FASST_mapping$ISO_A3), unique(COPD_SSP1_mort_0$ISO_A3)), c("SYC", "DMA", "TWN"))

pop_all <- do.call(rbind, list(pop_SSP1, pop_SSP2, pop_SSP3, pop_SSP4, pop_SSP5))
pop_all <- pop_all %>% filter(REGION %in% common.region)
pop_all <- pop_all %>% filter(!grepl(".*Education$", VARIABLE)) %>% filter(grepl(".*Aged.*", VARIABLE))
pop_all <- pop_all %>% mutate(VARIABLE = sub(".*Aged", "", VARIABLE))
pop_all <- pop_all %>% select(SCENARIO, REGION, VARIABLE, X2015, X2020, X2025, X2030, X2035, X2040, X2045, 
                              X2050, X2055, X2060, X2065, X2070, X2075, X2080, X2085, X2090, X2095, X2100) %>%
  group_by(SCENARIO, REGION, VARIABLE) %>% summarise_all(list(sum)) %>% ungroup()
pop_all <- pop_all %>% mutate(SCENARIO = ifelse(SCENARIO == "SSP1_v9_130115", "0", SCENARIO)) %>%
  mutate(SCENARIO = ifelse(SCENARIO == "SSP2_v9_130115", "1", SCENARIO)) %>%
  mutate(SCENARIO = ifelse(SCENARIO == "SSP3_v9_130115", "2", SCENARIO)) %>%
  mutate(SCENARIO = ifelse(SCENARIO == "SSP4d_v9_130115", "3", SCENARIO)) %>%
  mutate(SCENARIO = ifelse(SCENARIO == "SSP5_v9_130115", "4", SCENARIO))
pop_all <- pop_all %>% mutate(VARIABLE = paste0("X", VARIABLE))
pop_all <- pop_all %>% gather(key = "year", value = "val", -SCENARIO, -REGION, -VARIABLE) %>% spread(key = VARIABLE, value = val)
pop_all <- pop_all %>% mutate(year = sub("X", "", year)) %>% mutate(`X95+` = `X95-99` + `X100+`) %>%
  select(`SCENARIO`, REGION, year, `X25-29`, `X30-34`, `X35-39`, 
         `X40-44`, `X45-49`, `X50-54`, `X55-59`, `X60-64`, `X65-69`, `X70-74`, `X75-79`, 
         `X80-84`, `X85-89`, `X90-94`, `X95+`)

#### Figure 1: Influence diagram ####

#### Figure 2: Carbon price, CO2, and health risks, as time series; Map of health impacts ####

# Carbon prices are preset. 
carbon_price <- data.frame(year = as.character(seq(2015, 2100, by = 5)), 
                           price = c(0, 21, 38.1667, 
                                     55.3333, 72.5, 94.2, 115.9, 137.6, 
                                     148.067, 158.533, 169, 170.467, 171.933, 
                                     173.4, 190.167, 206.933, 223.7, 233.8))
carbon_price$price <- carbon_price$price * 1.83 * (12.01 / 44.01) # 1.83: CPI data from Bureau of Labor Statistics, 1990$ to 2015$; 

fig_carbon_price <- ggplot(data = carbon_price) + 
  geom_path(mapping = aes(x = year, y = price, group = 1)) + 
  geom_point(mapping = aes(x = carbon_price[8, 1], y = carbon_price[8, 2])) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size = 8), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(), plot.margin = unit(c(0.5, .2, 0.5, .15), "cm"), 
        axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0)), plot.title = element_markdown(size = 10)) + 
  labs(x = "Year", y = expression("2015 USD /"~tCO[2]), title = "<b>a) Carbon price <br>level</b>") + 
  scale_x_discrete(breaks = seq(2025, 2100, by = 25), 
                   expand = expansion(mult = c(0, 0.05))) + 
  scale_y_continuous(limits = c(0, 120), expand = expansion(mult = c(0, 0)))

temp_ts <- read.csv("../DATA/temp_1020.csv", stringsAsFactors = F) %>% 
  mutate(scenario = substr(scenario, 1, 15))
temp_ts$carbon_tax <- substr(temp_ts$scenario, 1, 1)
temp_2050 <- temp_ts %>% select(carbon_tax, X2050)
temp_2100 <- temp_ts %>% select(carbon_tax, X2100)
temp_ts <- temp_ts %>% select(carbon_tax, paste0("X", seq(2015, 2100, by = 5)))
temp_ts <- temp_ts %>% gather(key = "YEAR", value = Temp, -carbon_tax)
temp_ts$YEAR <- substr(temp_ts$YEAR, 2, 5)
temp_ts <- temp_ts %>% group_by(YEAR, carbon_tax) %>% summarise(min = min(Temp), median = median(Temp), max = max(Temp), .groups = "drop")

fig_temp_ts <- ggplot() + 
  geom_path(data = temp_ts, mapping = aes(x = YEAR, y = median , color = carbon_tax, group = carbon_tax), size = 1) + 
  geom_ribbon(data = temp_ts, mapping = aes(x = YEAR, ymin = min, ymax = max, 
                                            fill = carbon_tax, group = carbon_tax), 
              size = 0, alpha = 0.3) + 
  geom_boxplot(data = temp_2050, mapping = aes(x = 18.8, y = X2050, color = carbon_tax), 
               width = 0.7, outlier.size = 0.05, position = position_dodge(width = 1.2)) + 
  geom_boxplot(data = temp_2100, mapping = aes(x = 20.4, y = X2100, color = carbon_tax), 
               width = 0.7, outlier.size = 0.05, position = position_dodge(width = 1.2)) + 
  geom_segment(aes(x = 19.6, xend = 19.6, y = 1.15, yend = 5.35), size = 0.3, linetype = "dashed") + 
  geom_text(aes(x = 18.7, y = 5.275, label = "2050"), size = 2) + 
  geom_text(aes(x = 20.5, y = 5.275, label = "2100"), size = 2) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size = 8), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_markdown(size = 10),
        legend.position = c(0.3, 0.75), legend.title = element_blank(), legend.spacing.y = unit(0, "cm"), 
        legend.box.background = element_rect(), plot.margin = unit(c(0.5, .15, 0.5, .15), "cm"), 
        axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0))) + 
  labs(x = "Year", y = expression("Temperature change (°C)"), title = expression("<b>b) Global average <br>temperature change</b>")) + 
  scale_x_discrete(breaks = seq(2025, 2100, by = 25), 
                   expand = expansion(mult = c(0, 0.2))) + 
  scale_y_continuous(limits = c(1, 5.5), expand = expansion(mult = c(0, 0))) + 
  scale_color_manual(values = c("darkgrey", "deepskyblue"), labels = c("No carbon price", "With carbon price")) + 
  scale_fill_manual(values = c("darkgrey", "deepskyblue")) +
  guides(fill = "none", alpha = "none")


dr_ts <- read.csv("../DATA/dr_ts_summary.csv", stringsAsFactors = F)
dr_2050_global <- read.csv("../DATA/dr_2050_global.csv", stringsAsFactors = F)
dr_ts$YEAR <- as.character(dr_ts$YEAR)
dr_2050_global$YEAR <- as.character(dr_2050_global$YEAR)
dr_ts$carbon_tax <- as.character(dr_ts$carbon_tax)
dr_2050_global$carbon_tax <- as.character(dr_2050_global$carbon_tax)
dr_2100_global <- read.csv("../DATA/dr_2100_global.csv", stringsAsFactors = F)
dr_ts$YEAR <- as.character(dr_ts$YEAR)
dr_2100_global$YEAR <- as.character(dr_2100_global$YEAR)
dr_ts$carbon_tax <- as.character(dr_ts$carbon_tax)
dr_2100_global$carbon_tax <- as.character(dr_2100_global$carbon_tax)

fig_dr_ts <- ggplot() + 
  geom_path(data = dr_ts, mapping = aes(x = YEAR, y = median , color = carbon_tax, group = carbon_tax), size = 1) + 
  geom_ribbon(data = dr_ts, mapping = aes(x = YEAR, ymin = min, ymax = max, 
                                          fill = carbon_tax, group = carbon_tax), 
              size = 0, alpha = 0.3) + 
  geom_boxplot(data = dr_2050_global, mapping = aes(x = 18.8, y = death_rate, color = carbon_tax), 
               width = 0.7, outlier.size = 0.05, position = position_dodge(width = 1.2)) + 
  geom_boxplot(data = dr_2100_global, mapping = aes(x = 20.4, y = death_rate, color = carbon_tax), 
               width = 0.7, outlier.size = 0.05, position = position_dodge(width = 1.2)) + 
  geom_segment(aes(x = 19.6, xend = 19.6, y = 630, yend = 2070), size = 0.3, linetype = "dashed") + 
  geom_text(aes(x = 18.7, y = 2035, label = "2050"), size = 2) + 
  geom_text(aes(x = 20.5, y = 2035, label = "2100"), size = 2) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size = 8), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(0.3, 0.75), legend.title = element_blank(), legend.spacing.y = unit(0, "cm"),
        plot.title = element_markdown(size = 10),
        legend.box.background = element_rect(), plot.margin = unit(c(0.5, .15, 0.5, .15), "cm"), 
        axis.title.y = element_text(margin = margin(t = 0, r = -3, b = 0, l = 0))) + 
  labs(x = "Year", y = expression("Deaths per million per year"), title = "<b>c) Global average PM<sub>2.5</sub> <br>attributable death rates</b>") + 
  scale_x_discrete(breaks = seq(2025, 2100, by = 25), 
                   expand = expansion(mult = c(0, 0.2))) + 
  scale_y_continuous(limits = c(600, 2100), expand = expansion(mult = c(0, 0))) + 
  scale_color_manual(values = c("darkgrey", "deepskyblue"), labels = c("No carbon price", "With carbon price")) + 
  scale_fill_manual(values = c("darkgrey", "deepskyblue")) +
  guides(fill = "none", alpha = "none")

#### Now let's plot the map

region_mapping <- read_xlsx("GCAM emissions matching/region_mapping.xlsx", sheet = 1)
region_mapping2 <- read_xlsx("GCAM emissions matching/region_mapping.xlsx", sheet = 2)

region_mapping2[7, 2] <- "Brazil"

dt_fig2 <- read.csv("../DATA/dr_2050.csv", stringsAsFactors = F)
dt_fig2 <- dt_fig2 %>% mutate(scenario_input = paste0(substr(scenario_input, 1, 21), "-", "0"))
dt_fig2 <- dt_fig2 %>% select(Country, ISO_A3, deaths_total, death_rate, scenario_input)
names(dt_fig2)[3] <- "total_deaths"

dt_fig2$carbon_tax <- substr(dt_fig2$scenario_input, 7, 7)
substr(dt_fig2$scenario_input, 7, 7) <- "X"

dt_fig2_diff <- inner_join(
  x = dt_fig2 %>% filter(carbon_tax == "0"), 
  y = dt_fig2 %>% filter(carbon_tax == "1") %>% select(-Country), 
  by = c("ISO_A3", "scenario_input")
)
dt_fig2_diff <- dt_fig2_diff %>% mutate(dr_diff = (death_rate.y - death_rate.x) / death_rate.x,
                                        deaths_diff = (total_deaths.y - total_deaths.x) / total_deaths.x)
dt_fig2_diff <- dt_fig2_diff %>% mutate(dr_diff_abs = (death_rate.y - death_rate.x),
                                        deaths_diff_abs = (total_deaths.y - total_deaths.x))

dt_fig2_diff <- dt_fig2_diff %>% group_by(ISO_A3) %>%
  mutate(mark = if_else(quantile(dr_diff_abs, probs = 0.99) > 0, 0, 1)) # 1 for all co-benefits, 0 for potential co-harms
dt_fig2_diff <- dt_fig2_diff %>% select(-scenario_input, -carbon_tax.x, -carbon_tax.y) %>% 
  group_by(Country, ISO_A3) %>% summarise_all(list(median))
dt_fig2_diff$mark <- factor(dt_fig2_diff$mark, levels = c(1, 0), labels = c("benefit", "harm"))

world_map <- map_data("world")
world_map <- world_map %>% select(-subregion)

world_map <- world_map %>% mutate(ISO_A3 = iso.alpha(region, n = 3)) %>% 
  mutate(ISO_A3 = ifelse(region == "Kosovo", "XKX", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "Nigeria", "NGA", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "Guinea-Bissau", "GNB", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "Dominican Republic", "DOM", ISO_A3))

# Making up for Greenland, Taiwan, South Sudan, French Guiana

world_map <- world_map %>%  
  mutate(ISO_A3 = ifelse(region == "Greenland", "CAN", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "Taiwan", "CHN", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "South Sudan", "SDN", ISO_A3)) %>% 
  mutate(ISO_A3 = ifelse(region == "French Guiana", "GUY", ISO_A3))

dt_fig2_diff <- world_map %>% inner_join(dt_fig2_diff %>% select(-death_rate.x, -death_rate.y), by = "ISO_A3")

#### test of .shp
library(sf)
library(sp)
sf_use_s2(use_s2 = F)
gcam_sf <- st_read("GCAM 32 (with Taiwan)/GCAM_32_w_Taiwan.shp")

gcam_sf_32 <- gcam_sf %>% select(GCAM_30_re, geometry)
gcam_sf_32 <- gcam_sf_32 %>% group_by(GCAM_30_re) %>% summarise(geometry = st_union(geometry))
gcam_sf_32 <- gcam_sf_32[-1, ] %>% as_Spatial() %>% broom::tidy()

library(ggallin)
dt_fig2_diff_plot_only <- dt_fig2_diff %>% 
  mutate(dr_diff_abs = ifelse(region == "Greenland", 0, dr_diff_abs)) %>%
  mutate(mark = ifelse(region == "Greenland", "1", mark))

fig4_map <-
  ggplot() +
  geom_polygon_pattern(data = dt_fig2_diff_plot_only,
                       # geom_polygon_pattern(data = dt_fig2_diff_plot_only %>% filter(region %in% c("China", "India", "Russia", "Canada")),
                       mapping = aes(x = long, y = lat, group = group, fill = dr_diff_abs, pattern = mark),
                       color = "black", size = 0.1, 
                       pattern_color = "grey80", pattern_fill = "grey80", pattern_density = 0.1, pattern_spacing = 0.02) +
  geom_polygon(data = dt_fig2_diff_plot_only,
               mapping = aes(x = long, y = lat, group = group), fill = NA,
               color = "black", size = 0.1) +
  geom_polygon(data = gcam_sf_32,
               mapping = aes(x = long, y = lat, group = group), size = .4, color = "grey20", fill = NA) +
  coord_sf(xlim = c(-191, 191), ylim = c(-90.5, 90.5)) +
  coord_map(projection = "mollweide") +
  scale_x_continuous(breaks = seq(-180, 180, by = 60),
                     # labels = c("180°", "120°W", "60°W", "0°", "60°E", "120°E", "180°"),
                     limits = c(-191, 191), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 30),
                     # labels = c("90°S", "60°S", "30°S", "0°", "30°N", "60°N", "90°N"),
                     limits = c(-60, 90), expand = c(0, 0)) +
  scale_fill_gradientn(colors = c("deepskyblue4", "deepskyblue3", "white", "brown"), values = c(0, 4/8, 7/8, 1), # for death rate abs
                       limits = c(-210, 30),
                       breaks = seq(-210, 30, by = 30)) +
  scale_pattern_manual(values = c("none", "stripe"), labels = c("Consistent co-benefits", "Potential co-harms"), 
                       guide = "none") + 
  labs(x = "", y = "", 
       title = expression("<b>d) Changes in PM<sub>2.5</sub> attributable death rates due to a global carbon price</b>"),
       fill = "Ensemble median \nchange in death rate\n[deaths per million per year]") + 
  theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_line(colour = "gray85"), text = element_text(size = 8), 
        axis.text = element_blank(), axis.ticks =  element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), legend.spacing.y = unit(0.1, units = "in"), 
        plot.title = element_markdown(hjust = 0.25, size = 10), legend.key = element_rect(color = "grey70"),
        legend.position = c(0.12, 0.25), legend.box.background = element_rect())


fig_basic <- ggarrange(fig_carbon_price, fig_temp_ts, fig_dr_ts, ncol = 3, nrow = 1, align = "hv", 
                       widths = c(1.42, 2.83, 2.83), heights = 2.83)
fig_basic_1 <- ggarrange(fig_basic, fig4_map, ncol = 1, nrow = 2, widths = 7.08, heights = c(2.83, 3.5))
ggsave("Figures/Figure2.pdf", fig_basic_1, width = 7.08, height = 6.33, dpi = 600)

#### Figure 3 Reduction of Health Inequity ####

income_ssp <- read.csv("../DATA/gdp_GCAM32.csv", stringsAsFactors = F) # from GCAM!!!
pop_gcam <- read.csv("../DATA/pop_gcam.csv", stringsAsFactors = F)
dt_fig7 <- read.csv("../DATA/fig72_data.csv", stringsAsFactors = F)
dt_fig7 <- dt_fig7 %>% mutate(scenario_input = paste0(substr(scenario_input, 1, 21), "-", "0"))
dt_fig7$carbon_tax <- substr(dt_fig7$scenario_input, 7, 7)

substr(dt_fig7$scenario_input, 7, 7) <- "X"
dt_fig7 <- dt_fig7 %>% select(-deaths_total, -pop_total)
dt_fig7 <- inner_join(
  x = dt_fig7 %>% filter(carbon_tax == "0"), 
  y = dt_fig7 %>% filter(carbon_tax == "1"), 
  by = c("GCAM", "metric", "scenario_input")
)


dt_fig7 <- dt_fig7 %>% select(-carbon_tax.x, -carbon_tax.y) %>%
  mutate(death_rate.y = death_rate.y - death_rate.x)

dt_fig7 <- dt_fig7 %>% left_join(income_ssp, by = c("GCAM" = "region")) %>% inner_join(pop_gcam, by = "GCAM")
dt_fig7 <- dt_fig7 %>% mutate(income = GDP2015 / X2015) %>% select(-GDP2015, -X2015)
dt_fig7_median <- dt_fig7 %>% select(-scenario_input) %>% group_by(GCAM, metric) %>% summarise_all(list(median))

dr_2015 <- read.csv("../DATA/dr_2015.csv", stringsAsFactors = F)

dr_2015_bar <- dt_fig7 %>% left_join(dr_2015 %>% select(GCAM, death_rate), by = "GCAM") %>% 
  select(GCAM, death_rate, death_rate.x, death_rate.y) %>% group_by(GCAM) %>% 
  summarise(death_rate_2015 = median(death_rate), 
            median.x = median(death_rate.x), upper.x = quantile(death_rate.x, .995), lower.x = quantile(death_rate.x, .005), 
            median.y = median(death_rate.y), upper.y = quantile(death_rate.y, .995), lower.y = quantile(death_rate.y, .005))

dr_2015_bar <- dr_2015_bar %>% mutate(mark = if_else(upper.y <= 0, "benefit", "harm"))
dr_2015_bar$size.x <- "normal"
dr_2015_bar$size.y <- "normal"
dr_2015_bar[c(6, 11, 15, 18), "size.x"] <- "thick"
dr_2015_bar[c(8, 11, 18, 24, 31), "size.y"] <- "thick"

fig_3a <- ggplot() + 
  geom_pointrange(data = dr_2015_bar,
                  mapping = aes(x = death_rate_2015, y = median.x, ymin = lower.x, ymax = upper.x, size = size.x), 
                  shape = 21, fill = "white", stroke = 1, fatten = 2, 
                  position = position_dodge(width = 0)) + 
  geom_smooth(data = dr_2015_bar, 
              mapping = aes(x = death_rate_2015, y = median.x, alpha = "Regression line"), linetype = "twodash", 
              method = "lm", formula = y ~ x, se = F, color = "grey50") +
  labs(
    title = "<b>a) PM<sub>2.5</sub>-attributable death rate <br>without a carbon price</b>",
    x = "Deaths per million per year in 2015", 
    y = "Deaths per million per year in 2050") +
  theme_bw() +
  theme(plot.title = element_markdown(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(.3, .7), legend.text = element_text(size = 11), legend.key.width = unit(2.5, "line"), 
        legend.key = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"), 
        legend.margin = margin(0, -10, 0, -10),
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), plot.margin = unit(c(.3, .5, .3, .5), "cm")) +
  scale_x_continuous(limits = c(0, 1500), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0, 2600), expand = expansion(mult = c(.0, .0))) + 
  scale_size_manual(values = c(0.7, 1), name = NULL) + 
  scale_alpha_manual(values = 1, name = NULL) + 
  guides(alpha = guide_legend(order = 1), size = "none") +
  annotate(geom = "label", label = "Australia", x = 150, y = 80, color = "black") +
  annotate(geom = "label", label = "Eastern Europe", x = 1000, y = 100, color = "black") +
  annotate(geom = "label", label = "China", x = 1400, y = 800, color = "black") +
  annotate(geom = "label", label = "India", x = 1400, y = 2480, color = "black") 

fig_3b <- ggplot() + 
  geom_rect(mapping = aes(xmin = 0, xmax = 1500, ymin = 0, ymax = 60), fill = "grey70", alpha = .3) +
  geom_rect(mapping = aes(xmin = 0, xmax = 1500, ymin = -320, ymax = 0), fill = "dodgerblue", alpha = .4) +
  geom_pointrange(data = dr_2015_bar,
                  mapping = aes(x = death_rate_2015, y = median.y, ymin = lower.y, ymax = upper.y, 
                                color = mark, size = size.y), 
                  shape = 21, fill = "white", stroke = 1, fatten = 2, 
                  position = position_dodge(width = 0)) + 
  geom_smooth(data = dr_2015_bar, 
              mapping = aes(x = death_rate_2015, y = median.y, alpha = "Regression line"), linetype = "twodash", 
              method = "lm", formula = y ~ x, se = F, color = "grey50") +
  labs(
    title = "<b>b) Changes in PM<sub>2.5</sub>-attributable death rate <br>due to a carbon price</b>",
    x = "Deaths per million per year in 2015", 
    y = "Deaths reduction per million per year in 2050") +
  theme_bw() +
  theme(plot.title = element_markdown(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.position = c(.4, .2), legend.text = element_text(size = 11), legend.key.width = unit(2.5, "line"), 
        legend.key = element_rect(fill = "transparent"), legend.background = element_rect(fill = "transparent"), 
        legend.margin = margin(0, -10, 0, -10),
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), plot.margin = unit(c(.3, .5, .3, .5), "cm")) +
  scale_x_continuous(limits = c(0, 1500), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(-320, 60), expand = expansion(mult = c(.0, .0))) + 
  scale_color_manual(values = c("black", "brown"), name = "Death rate (median and range)", 
                     labels = c("Consistent co-benefits", "Potential co-benefits and co-harms")) + 
  scale_size_manual(values = c(0.7, 1), name = NULL) + 
  scale_alpha_manual(values = 1, name = NULL) +
  guides(color = guide_legend(order = 1, override.aes = list(size = .5)), linetype = guide_legend(order = 1),
         alpha = guide_legend(order = 2), size = "none") +
  annotate(geom = "label", label = "Health co-benefits", x = 550, y = -70, fill = "dodgerblue", color = "grey90") +
  annotate(geom = "label", label = "Health co-harms", x = 550, y = 48, fill = "brown", color = "grey90") +
  annotate(geom = "label", label = "China", x = 1400, y = -30, color = "black") +
  annotate(geom = "label", label = "India", x = 1270, y = -280, color = "black") +
  annotate(geom = "label", label = "Canada", x = 200, y = 30, color = "brown") +
  annotate(geom = "label", label = "Russia", x = 900, y = 30, color = "brown") + 
  annotate(geom = "label", label = "USA", x = 530, y = 20, color = "brown")

fig_3 <- gridExtra::grid.arrange(fig_3a, fig_3b, ncol = 2, nrow = 1)
ggsave("Figures/Figure3.pdf", plot = fig_3, width = 10, height = 5, dpi = 600)


#### Figure 4 ####

#### Mechanism of health effects 

SO2_2050 <- read.csv("../DATA/SO2_All.csv", stringsAsFactors = F)

region_mapping <- read_xlsx("GCAM emissions matching/region_mapping.xlsx", sheet = 1)
region_mapping2 <- read_xlsx("GCAM emissions matching/region_mapping.xlsx", sheet = 2)

region_mapping2[7, 2] <- "Brazil"

OC_2050 <- read.csv("../DATA/OC_All.csv", stringsAsFactors = F)
OC_2050$EF <- "High"

OC_2050 <- OC_2050 %>% left_join(region_mapping2, by = c("region" = "GCAM"))
OC_2050 <- OC_2050 %>% select(-region) %>% group_by(Map_19re, scenario_input, EF) %>% 
  summarise(OC = sum(OC), .groups = "drop")
OC_2050$carbon_tax <- substr(OC_2050$scenario_input, 7, 7)
substr(OC_2050$scenario_input, 7, 7) <- "X"
OC_2050 <- inner_join(
  x = OC_2050 %>% filter(carbon_tax == "0"), 
  y = OC_2050 %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "scenario_input", "EF")
)
OC_2050 = OC_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>% 
  mutate(dOC = OC.y - OC.x) 

SO2_2050 <- SO2_2050 %>% left_join(region_mapping2, by = c("region" = "GCAM"))
SO2_2050 <- SO2_2050 %>% select(-region) %>% group_by(Map_19re, scenario_input) %>% summarise(SO2 = sum(SO2)) %>% ungroup()
SO2_2050$carbon_tax <- substr(SO2_2050$scenario_input, 7, 7)
substr(SO2_2050$scenario_input, 7, 7) <- "X"
SO2_2050 <- inner_join(
  x = SO2_2050 %>% filter(carbon_tax == "0"), 
  y = SO2_2050 %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "scenario_input")
)
SO2_2050 = SO2_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>% 
  mutate(dSO2 = SO2.y - SO2.x) 


dr_2050 <- read.csv("../DATA/dr_2050.csv", stringsAsFactors = F)
dr_2050$EF <- "High"
dr_2050 <- dr_2050 %>% mutate(scenario_input = sub("_all", "", scenario_input))

dr_2050 <- dr_2050 %>% inner_join(region_mapping %>% select(ISO_A3, GCAM), by = "ISO_A3") %>% inner_join(region_mapping2, by = "GCAM")
dr_2050 <- dr_2050 %>% select(Map_19re, scenario_input, deaths_total, total_pop, EF) %>% 
  group_by(Map_19re, scenario_input, EF) %>% summarise_all(list(sum))
dr_2050 <- dr_2050 %>% mutate(death_rate = deaths_total / total_pop) 
dr_2050 <- dr_2050 %>% select(-deaths_total)

dr_2050$carbon_tax <- substr(dr_2050$scenario_input, 7, 7)
substr(dr_2050$scenario_input, 7, 7) <- "X"
dr_2050 <- inner_join(
  x = dr_2050 %>% filter(carbon_tax == "0"), 
  y = dr_2050 %>% filter(carbon_tax == "1") %>% select(-total_pop), 
  by = c("Map_19re", "scenario_input", "EF")
)
dr_2050 = dr_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>%
  mutate(ddr = death_rate.y - death_rate.x)

OC_vs_SO2 <- OC_2050 %>% inner_join(SO2_2050, by = c("Map_19re", "scenario_input")) %>%
  inner_join(dr_2050, by = c("Map_19re", "scenario_input", "EF"))
OC_vs_SO2 <- OC_vs_SO2 %>% mutate(ddr_TF = ifelse(ddr <= 0, 1, 0))


# Coal and biomass, SO2 and OC, health co-benefits as background color?

primary_2050 <- read.csv("../DATA/primary_2050.csv", stringsAsFactors = F)
names(primary_2050)[4] <- "pri"
primary_2050 <- primary_2050 %>% mutate(fuel = str_sub(fuel, 3, -1)) 
primary_2050$scenario <- substr(primary_2050$scenario, 1, 15)

primary_2050 <- primary_2050 %>% mutate(fuel_cat = case_when(
  fuel %in% c("biomass", "traditional biomass") ~ "biomass", 
  # fuel %in% c("oil", "coal", "natural gas") ~ "fossil", 
  fuel %in% c("hydro", "wind", "solar", "geothermal") ~ "renewables", 
  fuel == "nuclear" ~ "nuclear", 
  TRUE ~ fuel
))

primary_2050 <- primary_2050 %>% left_join(region_mapping2, by = c("region" = "GCAM")) %>%
  group_by(scenario, Map_19re, fuel_cat) %>% select(-region) %>% summarise(pri = sum(pri), .groups = "drop") %>%
  group_by(scenario, Map_19re) %>% mutate(pri_ratio = pri / sum(pri)) %>% 
  filter(fuel_cat %in% c("coal", "biomass", "renewables"))

primary_2050$carbon_tax <- substr(primary_2050$scenario, 1, 1)
substr(primary_2050$scenario, 1, 1) <- "X"

primary_2050 <- inner_join(
  x = primary_2050 %>% filter(carbon_tax == "0"), 
  y = primary_2050 %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "fuel_cat", "scenario")
)
primary_2050 <- primary_2050 %>% ungroup() %>% mutate(dpri_ratio = pri_ratio.y - pri_ratio.x) %>% 
  select(Map_19re, fuel_cat, dpri_ratio)

dt_fig7 <- read.csv("../DATA/fig72_data.csv", stringsAsFactors = F)
dt_fig7 <- dt_fig7 %>% mutate(scenario_input = paste0(substr(scenario_input, 1, 21), "-", "0"))
dt_fig4_bg <- dt_fig7

pop_2050 <- read.csv("../DATA/pop_GCAM32_2050.csv", stringsAsFactors = F)
pop_2050$socioecon <- as.character(pop_2050$socioecon)
dt_fig4_bg <- dt_fig4_bg %>% mutate(socioecon = substr(scenario_input, 11, 11)) %>%
  left_join(region_mapping2, by = c("GCAM")) %>% left_join(pop_2050, by = c("GCAM" = "region", "socioecon")) %>% 
  filter(metric == "Deaths") %>% select(scenario_input, Map_19re, deaths_total, X2050) %>%
  group_by(scenario_input, Map_19re) %>% summarise(deaths_total = weighted.mean(deaths_total, w = X2050), X2050 = sum(X2050), .groups = "drop")

dt_fig4_bg$carbon_tax <- substr(dt_fig4_bg$scenario_input, 7, 7)

substr(dt_fig4_bg$scenario_input, 7, 7) <- "X"

dt_fig4_bg <- inner_join(
  x = dt_fig4_bg %>% filter(carbon_tax == "0"), 
  y = dt_fig4_bg %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "scenario_input")
)

dt_fig4_bg <- dt_fig4_bg %>% ungroup() %>% mutate(ddr = deaths_total.y - deaths_total.x) %>% 
  select(Map_19re, ddr) %>% group_by(Map_19re) %>%
  summarise(ddr = median(ddr))

dt_fig4_em <- OC_vs_SO2 %>% mutate(dOC_pc = dOC / total_pop, dSO2_pc = dSO2 / total_pop) %>% 
  select(Map_19re, scenario_input, dOC_pc, dSO2_pc) %>% 
  gather(key = "em", value = "kT", -Map_19re, -scenario_input) %>% select(-scenario_input)

mech_regions <- c("India", "China", "Southeast Asia", "Russia","USA", "Sub-Saharan Africa", "Brazil",  "Canada")

dt_fig4_pri <- primary_2050 %>% inner_join(dt_fig4_bg, by = c("Map_19re")) %>% filter(Map_19re %in% mech_regions)
dt_fig4_pri$Map_19re <- factor(dt_fig4_pri$Map_19re, levels = mech_regions)


fig4_pri <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -20, ymax = 15), fill = "grey90") +
  geom_boxplot(data = dt_fig4_pri, mapping = aes(x = Map_19re, y = dpri_ratio * 1e2, color = fuel_cat), 
               width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "Percentage\npoints", title = "<b>a) Changes in primary energy mix</b>", 
       fill = "Ensemble median \nchanges in health risks\n(deaths per million\nper year)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.box.background = element_blank(), legend.text = element_text(size = 10), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(1.2, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.05, .5, 0.15, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-20, 15), expand = expansion(mult = 0)) + 
  scale_color_manual(values = c("darkgreen", "brown4", "blue"), guide = "none") + 
  annotate(geom = "text", x = 2.8, y = 11, label = "Share of biomass", color = "darkgreen") + 
  annotate(geom = "text", x = 6.8, y = 11, label = "Share of renewables", color = "blue") + 
  annotate(geom = "text", x = 4.8, y = -15, label = "Share of coal", color = "brown4")

dt_fig4_em <- dt_fig4_em %>% inner_join(dt_fig4_bg, by = c("Map_19re")) %>% filter(Map_19re %in% mech_regions)
dt_fig4_em$Map_19re <- factor(dt_fig4_em$Map_19re, levels = mech_regions)

fig4_em <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -10, ymax = 10), fill = "grey90") +
  geom_boxplot(data = dt_fig4_em, mapping = aes(x = Map_19re, y = kT * 1e3, color = em),
               width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "Per capita emissions\n(kT/year)", title = "<b>b) Changes in precursor air pollutant emissions</b>", 
       fill = "Ensemble median \nchanges in health risks\n(deaths per million\nper year)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.box.background = element_blank(), legend.text = element_text(size = 10), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(0.5, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.15, .5, 0.15, .5), "cm"), 
        axis.ticks.y.right = element_line(color = "royalblue1"),
        axis.text.y.right = element_text(color = "royalblue1")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-10, 10), expand = expansion(mult = 0), 
                     breaks = seq(-10, 10, by = 5), labels = seq(-10, 10, by = 5)) + 
  scale_color_manual(values = c("cyan4", "brown3"), guide = "none") +
  annotate(geom = "text", x = 2, y = 6, label = "OC emissions", color = "cyan4") +
  annotate(geom = "text", x = 5.8, y = -7.5, parse = T, label = "SO[2]~'emissions'", color = "brown3") 

pm2p5_2050 <- read.csv("../DATA/pm_country_2050.csv", stringsAsFactors = F)
pm2p5_2050$EF <- "High"

substr(pm2p5_2050$scenario_input, 7, 7) <- "X"

pm2p5_2050 <- inner_join(
  x = pm2p5_2050 %>% filter(carbon_tax == "0"), 
  y = pm2p5_2050 %>% filter(carbon_tax == "1"), 
  by = c("FASST.REGION", "Country", "ISO_A3", "GCAM", "scenario_input", "socioecon", "EF")
)
pm2p5_2050$dPM <- pm2p5_2050$PM_TOT.y - pm2p5_2050$PM_TOT.x
pm2p5_2050$socioecon <- as.character(pm2p5_2050$socioecon)

pop_2050_country <- cbind(pop_all %>% select(1:3), 
                          X2050 = apply(pop_all %>% filter(year == 2050) %>% select(-1, -2, -3), 1, sum)) %>% filter(year == 2050)
dt_fig4_pm <- pm2p5_2050 %>% inner_join(pop_2050_country, by = c("ISO_A3" = "REGION", "socioecon" = "SCENARIO"))
dt_fig4_pm <- dt_fig4_pm %>% left_join(region_mapping2, by = ("GCAM"))
dt_fig4_pm <- dt_fig4_pm %>% select(scenario_input, Map_19re, EF, dPM, X2050) %>% group_by(scenario_input, Map_19re, EF) %>% 
  summarise(dPM = weighted.mean(dPM, w = X2050), .groups = "drop")
dt_fig4_pm <- dt_fig4_pm %>% select(-scenario_input, -EF)

dt_fig4_pm <- dt_fig4_pm %>% filter(Map_19re %in% mech_regions)
dt_fig4_pm$Map_19re <- factor(dt_fig4_pm$Map_19re, levels = mech_regions)

fig4_pm <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -13, ymax = 1), fill = "grey90") +
  geom_boxplot(data = dt_fig4_pm, mapping = aes(x = Map_19re, y = dPM), 
               width = 0.15, outlier.size = 0.05) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = expression(atop("Concentrations", paste("("*mu*g/m^3*")"))), title = "<b>c) Changes in ambient PM<sub>2.5</sub> concentrations<b>", 
       fill = "Ensemble median \nchanges in health risks\n(deaths per million\nper year)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.box.background = element_blank(), legend.text = element_text(size = 10), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(0.5, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.15, .5, 0.15, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-13, 1), expand = expansion(mult = 0))

dt_fig4_dr <- dr_2050 %>% select(Map_19re, ddr)
dt_fig4_dr <- dt_fig4_dr %>% filter(Map_19re %in% mech_regions)
dt_fig4_dr$Map_19re <- factor(dt_fig4_dr$Map_19re, levels = mech_regions)

fig4_dr <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -350, ymax = 30), fill = "grey90") +
  geom_boxplot(data = dt_fig4_dr, mapping = aes(x = Map_19re, y = ddr), 
               width = 0.15, outlier.size = 0.05) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "Annual deaths\nper million", title = "<b>d) Changes in PM<sub>2.5</sub> attributable death rates<b>", 
       fill = "Ensemble median \nchanges in health risks\n(deaths per million\nper year)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.box.background = element_blank(), legend.text = element_text(size = 10), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(0.5, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.15, .5, 0.05, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) +   
  scale_y_continuous(limits = c(-350, 30), expand = expansion(mult = 0)) 

ggarrange(fig4_pri, fig4_em, fig4_pm, fig4_dr, nrow = 4, ncol = 1, align = "v", common.legend = T, legend = "right")
ggsave("Figures/Figure4.pdf", width = 7.08, height = 6.69, dpi = 600)


#### Figure 5 ####

# Panel a) 
land_2050 <- read.csv("../DATA/land_2050.csv", stringsAsFactors = F)
names(land_2050)[4] <- "km2"
land_2050$scenario <- substr(land_2050$scenario, 1, 15)

land_2050 <- land_2050 %>% mutate(LandLeaf = case_when(
  LandLeaf %in% c("crops", "pasture (grazed)", "pasture (other)") ~ "food-purpose", 
  TRUE ~ as.character(LandLeaf)
))

land_2050 <- land_2050 %>% left_join(region_mapping2, by = c("region" = "GCAM")) %>%
  group_by(scenario, Map_19re, LandLeaf) %>% select(-region) %>% summarise(km2 = sum(km2), .groups = "drop") 
land_2050 <- land_2050 %>% group_by(scenario, Map_19re) %>% mutate(km2_ratio = km2 / sum(km2))

land_2050$carbon_tax <- substr(land_2050$scenario, 1, 1)
substr(land_2050$scenario, 1, 1) <- "X"

land_2050 <- inner_join(
  x = land_2050 %>% filter(carbon_tax == "0"), 
  y = land_2050 %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "LandLeaf", "scenario")
)
land_2050 <- land_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>% 
  mutate(dkm2 = km2.y - km2.x, dkm2_ratio = km2_ratio.y - km2_ratio.x)
land_2050$aglu <- substr(land_2050$scenario, 9, 9)


dt_fig2 <- land_2050 %>% filter(
  # aglu %in% c("0", "2"), 
  LandLeaf %in% c("biomass", "forest (unmanaged)", "food-purpose"),
  Map_19re %in% mech_regions)

dt_fig2$Map_19re <- factor(dt_fig2$Map_19re, levels = mech_regions)

## panel a)
fig_harm_a <- ggplot() +
  # geom_rect(mapping = aes(xmin = .5, xmax = 8.5, ymin = -2.5, ymax = 5), fill = "white") +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -2.5, ymax = 5), fill = "grey90") +
  # geom_boxplot(data = dt_fig2, mapping = aes(x = Map_19re, y = dkm2, color = LandLeaf), 
  #              width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) + 
  geom_boxplot(data = dt_fig2, mapping = aes(x = Map_19re, y = dkm2_ratio * 1e2, color = LandLeaf), 
               width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "Share of land area<br>(percentage points)", title = "<b>a) Changes in land use</b>", ### It should be share of land use
       color = "Land type") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "transparent"), 
        legend.box.background = element_blank(), legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(1.2, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 10), axis.text.x = element_text(size = 8, color = "black"),
        axis.title.y = element_markdown(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.05, .5, 0.15, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-2.5, 5), expand = expansion(mult = 0)) + 
  scale_color_manual(values = c("darkgreen", "brown2", "purple2"), 
                     labels = c("biomass", "food-purpose", "unmanaged forest                 "))

OC_sectors_All <- read.csv("../DATA/OC_sectors_All.csv", stringsAsFactors = F)

OC_deforest_2050 <- read.csv("../DATA/OC_deforest_2050.csv", stringsAsFactors = F)

OC_aglu <- OC_sectors_All %>% filter(major_sector == "agriculture")
OC_aglu_lowEF <- OC_aglu %>% left_join(OC_deforest_2050 %>% filter(subsector == "Deforest"), by = c("region", "scenario_input"))
OC_aglu_lowEF <- OC_aglu_lowEF %>% select(-subsector) %>% replace(is.na(.), 0)
OC_aglu_lowEF <- OC_aglu_lowEF %>% mutate(OC.x = OC.x - OC.y) %>% select(-OC.y) %>% mutate(major_sector = "agriculture_lowEF")
names(OC_aglu_lowEF)[3] <- "OC"

OC_sectors_All <- OC_sectors_All %>% filter(major_sector != "agriculture") %>% 
  mutate(major_sector = ifelse(major_sector != "residential commercial", "others", major_sector))
OC_2050 <- bind_rows(OC_sectors_All, OC_aglu, OC_aglu_lowEF)

OC_2050 <- OC_2050 %>% left_join(region_mapping2, by = c("region" = "GCAM"))
OC_2050 <- OC_2050 %>% select(-region) %>% group_by(Map_19re, scenario_input, major_sector) %>% 
  summarise(OC = sum(OC), .groups = "drop")
OC_2050$carbon_tax <- substr(OC_2050$scenario_input, 7, 7)
substr(OC_2050$scenario_input, 7, 7) <- "X"
OC_2050 <- inner_join(
  x = OC_2050 %>% filter(carbon_tax == "0"), 
  y = OC_2050 %>% filter(carbon_tax == "1"), 
  by = c("Map_19re", "scenario_input", "major_sector")
)
OC_2050 = OC_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>% 
  mutate(dOC = OC.y - OC.x) 
dt_fig3 <- OC_2050 %>% filter(Map_19re %in% mech_regions)
dt_fig3$Map_19re <- factor(dt_fig3$Map_19re, levels = mech_regions)
data = dt_fig3 %>% filter(major_sector != "other")
dt_fig3$major_sector <- factor(dt_fig3$major_sector, levels = c("residential commercial", "agriculture", "agriculture_lowEF"))

# panel b)
fig_harm_b <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -.3, ymax = .5), fill = "grey90") +
  geom_boxplot(data = dt_fig3 %>% filter(major_sector != "others"), 
               mapping = aes(x = Map_19re, y = dOC, color = major_sector), 
               width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "OC emissions\n(Tg)", title = "<b>b) Changes in organic carbon emissions</b>", 
       color = "Sector and deforestation approach", linetype = "Sector and deforestation approach") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.title = element_text(size = 8), legend.key = element_blank(), 
        legend.box.background = element_blank(), legend.text = element_text(size = 8), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(1.2, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 8, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.05, .5, 0.15, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-.3, .5), expand = expansion(mult = 0)) + 
  scale_color_manual(values = c("orange", "blue2", "lightblue4"), 
                     labels = c("Residential", "Agriculture and land use\n(deforestation: open burning)", 
                                "Agriculture and land use \n(deforestation: clearcutting)")) 


dr_2050 <- read.csv("../DATA/dr_2050.csv", stringsAsFactors = F)
dr_lowEF <- read.csv("../DATA/dr_2050_lowEF.csv", stringsAsFactors = F)
dr_2050$EF <- "High"
dr_lowEF$EF <- "Low"
dr_2050 <- rbind(dr_2050, dr_lowEF)

dr_2050 <- dr_2050 %>% inner_join(region_mapping %>% select(ISO_A3, GCAM), by = "ISO_A3") %>% inner_join(region_mapping2, by = "GCAM")
dr_2050 <- dr_2050 %>% select(Map_19re, scenario_input, deaths_total, total_pop, EF) %>% 
  group_by(Map_19re, scenario_input, EF) %>% summarise_all(list(sum))
dr_2050 <- dr_2050 %>% mutate(death_rate = deaths_total / total_pop) 
dr_2050 <- dr_2050 %>% select(-deaths_total)

dr_2050$carbon_tax <- substr(dr_2050$scenario_input, 7, 7)
substr(dr_2050$scenario_input, 7, 7) <- "X"
dr_2050 <- inner_join(
  x = dr_2050 %>% filter(carbon_tax == "0"), 
  y = dr_2050 %>% filter(carbon_tax == "1") %>% select(-total_pop), 
  by = c("Map_19re", "scenario_input", "EF")
)
dr_2050 = dr_2050 %>% select(-carbon_tax.x, -carbon_tax.y) %>%
  mutate(ddr = death_rate.y - death_rate.x)

dt_fig5 <- dr_2050
dt_fig5 <- dt_fig5 %>% filter(Map_19re %in% mech_regions)
dt_fig5$Map_19re <- factor(dt_fig5$Map_19re, levels = mech_regions)

# panel c)
fig_harm_c <- ggplot() +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 1.6, xmax = 2.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 2.6, xmax = 3.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 3.6, xmax = 4.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 4.6, xmax = 5.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 5.6, xmax = 6.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 6.6, xmax = 7.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_rect(mapping = aes(xmin = 7.6, xmax = 8.4, ymin = -330, ymax = 30), fill = "grey90") +
  geom_boxplot(data = dt_fig5, mapping = aes(x = Map_19re, y = ddr, color = EF), 
               width = 0.3, outlier.size = 0.05, position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") + 
  labs(x = "",
       y = "Deaths per million", title = "<b>c) Changes in PM<sub>2.5</sub>-attributable death rate</b>", 
       color = "Deforestation approach", linetype = "Deforestation approach") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x.bottom = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key = element_blank(), legend.title = element_text(size = 8), 
        legend.box.background = element_blank(), legend.text = element_text(size = 8), 
        legend.spacing.y = unit(0.5, "cm"), legend.background = element_rect(fill = "transparent"), 
        legend.key.height = unit(1.2, "cm"), plot.title = element_markdown(size = 10), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 8, color = "black"),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 10), 
        plot.margin = unit(c(0.05, .5, 0.15, .5), "cm")
  ) + 
  scale_x_discrete(expand = expansion(mult = 0), 
                   labels = c("India", "China", "Southeast\nAsia", "Russia", 
                              "USA", "Sub-Saharan\nAfrica", "Brazil", "Canada")) + 
  scale_y_continuous(limits = c(-330, 30), expand = expansion(mult = 0)) +
  scale_color_manual(values = c("blue2", "lightblue4"), labels = c("Agriculture and land use\n(deforestation: open burning)", 
                                                                   "Agriculture and land use \n(deforestation: clearcutting)"))  


tmpfig5 <- cowplot::plot_grid(fig_harm_a, fig_harm_b, fig_harm_c, nrow = 3, ncol = 1, align = "v")

ggsave("Figures/Figure5.jpeg", tmpfig5, width = 7.08, height = 6.69, dpi = 600)

