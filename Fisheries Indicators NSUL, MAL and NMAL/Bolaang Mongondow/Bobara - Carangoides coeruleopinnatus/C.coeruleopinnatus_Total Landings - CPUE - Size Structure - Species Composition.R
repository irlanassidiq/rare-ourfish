##### ns Data #####

# Set working directory
setwd("C:/Users/IrlanAssidiq/OneDrive - Rare/ourfish/Fisheries Indicators NSUL, MAL and NMAL/Bolaang Mongondow")
getwd()

# Load Package
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(mgcv)
library(openxlsx)
library(grDevices)
library(TropFishR)

library(lme4)
library(fishmethods)
library(TropFishR)
library(Hmisc)


# Load OurFish raw data from Data World
ourfish.raw <- fread("https://query.data.world/s/ksnqjl2pvxfcfqtmlikqd2g53m3ggq?dws=00000", header=TRUE, stringsAsFactors=FALSE)

# Reformat buying unit into correct scientific naming format
ourfish.raw$buying_unit <- as.character(ourfish.raw$buying_unit)

substring(ourfish.raw$buying_unit, 2) <- tolower(substring(ourfish.raw$buying_unit, 2))

ourfish.raw$buying_unit <- as.factor(ourfish.raw$buying_unit)

# General Filters
start.date <- "2019-01-01"
end.date <- "2025-12-31"
sultra <- c("South East Sulawesi")
sulut <- c("North Sulawesi")
malut <- c("North Maluku")
maluku <- c("Maluku")
exclude.lgu <- "Wakatobi"
exclude.ma <- "Labengki"
ourfish.user.paap.2022 <- c("martawati", "wardo", "sukowati", "egerman",
                            "titilestari", "sahiudin", "hasan", "lampotaro",
                            "rusmankaumbu", "laabi", "samriah", "waona",
                            "wahalija", "wanurisa")
ourfish.user.paap.2024 <- c("zamria", "amu", "husain", "wahaasi")
exclude.buying.unit <- c("Mix grouper 1", "Mix grouper 2", "Mix family",
                         "Mixed fresh grouper", "Mixed live grouper", "Mixed family", 
                         "Epinephelus spp.", "Thunnus spp.")


#### Theme Format Start ####
theme.default.1 <- theme_classic() +
  theme(title = element_text(size = 18, color = "black", face = "bold"),
        plot.caption = element_text(size = 9),
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 14, color = "black"))

theme.default.2 <- theme_classic() +
  theme(title = element_text(size = 16, color = "black", face = "bold"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = "top")

theme.default.3 <- theme_classic() +
  theme(title = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))

theme.default.4 <- theme_classic() +
  theme(title = element_text(size = 16, color = "black", face = "bold"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 14, color = "black"))

theme.default.5 <- theme_classic() +
  theme(title = element_text(size = 18, color = "black", face = "bold"),
        plot.caption = element_text(size = 9, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.title.y.right = element_text(size = 16, color = "#008542"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.y.right = element_text(size = 14, color = "#008542"),
        strip.text = element_text(size = 14, color = "black"))

theme.default.6 <- theme_classic() +
  theme(title = element_text(size = 18, color = "black", face = "bold"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.title.y.right = element_text(size = 16, color = "#008542"),
        axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.text.y.right = element_text(size = 16, color = "#008542"),
        strip.text = element_text(size = 16, color = "black"))

theme.default.increase <- theme_classic() +
  theme(title = element_text(size = 18, color = "black", face = "bold"),
        plot.caption = element_text(size = 9),
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 14, color = "black"))

#### Theme Format End ####

# Subset Data
# Use general filters
# Remove observation with 0 in weight_kg
ourfish.data <- ourfish.raw[date >= start.date & date <= end.date &
                              #!lgu_name %in% exclude.lgu &
                              #!ma_name %in% exclude.ma &
                              !buying_unit %in% exclude.buying.unit &
                              !weight_kg == 0] [order(date)]
ourfish.raw.ns <- ourfish.raw[date >= start.date & date <= end.date &
                                 snu_name %in% sultra &
                                 #!family %in% "Istiophoridae" &
                                 #!lgu_name %in% exclude.lgu &
                                 #!ma_name %in% exclude.ma &
                                 #!buying_unit %in% exclude.buying.unit &
                                 !weight_kg == 0][order(date)]

ourfish.raw.ns <- ourfish.raw[date >= start.date & date <= end.date &
                                snu_name %in% sulut &
                                #!family %in% "Istiophoridae" &
                                #!buying_unit %in% exclude.buying.unit &
                                !weight_kg == 0][order(date)]

ourfish.raw.mal <- ourfish.raw[date >= start.date & date <= end.date &
                                 snu_name %in% maluku &
                                 #!family %in% "Istiophoridae" &
                                 #!buying_unit %in% exclude.buying.unit &
                                 !weight_kg == 0][order(date)]

ourfish.raw.nmal <- ourfish.raw[date >= start.date & date <= end.date &
                                  snu_name %in% malut &
                                  #!family %in% "Istiophoridae" &
                                  #!buying_unit %in% exclude.buying.unit &
                                  !weight_kg == 0][order(date)]

# Load fish meta data
fish.meta <- fread("C:/Users/IrlanAssidiq/Dropbox/OURFISH - FASTFIELD/meta_data/Reef Fish Meta Data_2022.07.05.csv",
                   header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("buying_unit", "family"))

# Join OurFish raw ns data with fish meta data
ourfish.all <- ourfish.data [fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.ns <- ourfish.raw.ns[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.ns <- ourfish.raw.ns[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.nmal <- ourfish.raw.nmal[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.mal <- ourfish.raw.mal[fish.meta, on = .(buying_unit), nomatch = NULL]

#===============================================================================
#etekis coruscans
etelis.ns <- ourfish.all[species %in% "Etelis coruscans"]

variola.ns <- ourfish.all[label %in% "Mancis"]

# Total Landings
ourfish.Bolaang_Mongondow<- ourfish.raw[#month >= 2 & month <= 7 &
  lgu_name %in% "Bolaang Mongondow"]

ourfish.ns.landings.Bolaang_Mongondow <- ourfish.Bolaang_Mongondow[lgu_name %in% "Bolaang Mongondow",
                                                  .(Berat = sum(weight_kg)),
                                                  by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.Bolaang_Mongondow, "lgu_name", "location")
ourfish.ns.landings.1 <- ourfish.Bolaang_Mongondow[location %in% "Bolaang Mongondow",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, location)][order(date)]

ourfish.ns.landings.1.glm <- ourfish.ns.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ns.landings.1.glm2 <- ourfish.ns.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ns.landings.1.glm2)

# Label Landings trends
ourfish.ns.landings.1.glm3 <- ourfish.ns.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.ns.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ns.landings.1.glm3)

ourfish.ns.landings.1.plot <- ggplot(ourfish.ns.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#00833e", fill = "#00833e") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "3 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#00833e",#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Bolaang Mongondow",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Bolaang_Mongondow <- ourfish.ns.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("landings Bolaang_Mongondow.svg", plot = landings.Bolaang_Mongondow, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ns.cpue.1 <- ourfish.Bolaang_Mongondow[location %in% "Bolaang Mongondow",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, location, fisher_id)][order(date)]

ourfish.ns.cpue.1.glm <- ourfish.ns.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ns.cpue.1.glm2 <- ourfish.ns.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ns.cpue.1.glm2)

# Label Landings trends
ourfish.ns.cpue.1.glm3 <- ourfish.ns.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.ns.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ns.cpue.1.glm3)

ourfish.ns.cpue.1.plot <- ggplot(ourfish.ns.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#00833e", fill = "#00833e") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "3 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-06-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "#00833e",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Bolaang Mongondow",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Bolaang_Mongondow <- ourfish.ns.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue Bolaang_Mongondow.svg", plot = cpue.Bolaang_Mongondow, bg = "transparent", width = 13.33, height = 7.3)

# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.Bolaang_Mongondow$weight_kg, 0.01)
quantile(ourfish.Bolaang_Mongondow$weight_kg, 0.99)

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.Bolaang_Mongondow[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.Bolaang_Mongondow[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.Bolaang_Mongondow$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.Bolaang_Mongondow.outlier <- ourfish.Bolaang_Mongondow[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.buton.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_buton_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.buton$weight_kg, 0.025)
# q3 <- quantile(ourfish.buton$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.buton[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.buton[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.Bolaang_Mongondow.clean <- ourfish.Bolaang_Mongondow[-landings.data.outliers$Obs.Num, ]

## Box Plot for annual comparison

# Make data for two years period
ourfish.Bolaang_Mongondow.clean.boxplot.1 <- ourfish.Bolaang_Mongondow.clean[,
                                                     year_period := as.factor("2 Years Period")]

# Make data for Year 1
ourfish.Bolaang_Mongondow.clean.boxplot.2 <- ourfish.Bolaang_Mongondow.clean[date >= start.date & date <= "2022-10-01"][,
                                                                                                year_period := as.factor("Year 1")]

# Make data for Year 2
ourfish.Bolaang_Mongondow.clean.boxplot.3 <- ourfish.Bolaang_Mongondow.clean[date >= "2022-08-01" & date <= end.date][,
                                                                                              year_period := as.factor("Year 2")]

# Combine data of two years period with year 1 & 2 data
ourfish.Bolaang_Mongondow.clean.boxplot.4 <- rbind(ourfish.Bolaang_Mongondow.clean.boxplot.1, ourfish.Bolaang_Mongondow.clean.boxplot.2, ourfish.Bolaang_Mongondow.clean.boxplot.3)

# Annual CPUE & Catch Value using box plot
ourfish.Bolaang_Mongondow.clean.boxplot.5 <- ourfish.Bolaang_Mongondow.clean.boxplot.4[,
                                                               .(cpue_kg_trip = sum(weight_kg),
                                                                 vpue_idr_trip = sum(total_price_local)),
                                                               by = .(year_period, date, fisher_id)][order(year_period)]

# Quartile 1,2,3 of annual CPUE & Catch Value
ourfish.Bolaang_Mongondow.clean.boxplot.5[,
                              .(IQR_cpue = quantile(cpue_kg_trip, probs = c(0.25, 0.5, 0.75)),
                                IQR_vpue = quantile(vpue_idr_trip, probs = c(0.25, 0.5, 0.75))),
                              by = .(year_period)]

# Independent Sample T-Test Between CPUE Year 1 and 2
t.test(data = ourfish.Bolaang_Mongondow.clean.boxplot.5[!year_period == "2 Years Period"],
       cpue_kg_trip ~ year_period,
       var.equal = TRUE)

# Independent Sample T-Test Between Catch Value Year 1 and 2
t.test(data = ourfish.Bolaang_Mongondow.clean.boxplot.5[!year_period == "2 Years Period"],
       vpue_idr_trip ~ year_period,
       var.equal = TRUE)

# Box Plot for Annual CPUE
boxplot.annual.cpue <- 
  ggplot(data = ourfish.Bolaang_Mongondow.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = cpue_kg_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), oob = squish) +
  labs(x = "Year Period",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Box Plot for Annual Catch Value
boxplot.annual.catch.value <- 
  ggplot(data = ourfish.Bolaang_Mongondow.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = vpue_idr_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,2000000), oob = squish,
                     labels = comma_format(accuracy = 1)) +
  labs(x = "Year Period",
       y = "Catch Value (IDR/trip)") +
  theme.default.1

# Combine box plot of CPUE & Catch Value
boxplot.annual.cpue.vpue <-
  ggarrange(boxplot.annual.cpue, boxplot.annual.catch.value,
            ncol = 2, nrow = 1)

annotate_figure(boxplot.annual.cpue.vpue,
                top = text_grob("Annual CPUE and Catch Value in Southeast Sulawesi", color = "black", face = "bold", size = 16))

# Average Annual CPUE & Catch Value
ourfish.Bolaang_Mongondow.clean.boxplot.6 <- ourfish.Bolaang_Mongondow.clean.boxplot.5[,
                                                               .(n_trip = .N,
                                                                 mean_cpue = mean(cpue_kg_trip),
                                                                 se_cpue = sd(cpue_kg_trip) / sqrt(.N),
                                                                 mean_vpue = mean(vpue_idr_trip),
                                                                 se_vpue = sd(vpue_idr_trip) / sqrt(.N)),
                                                               by = .(year_period)][order(year_period)]

fwrite(ourfish.Bolaang_Mongondow.clean.boxplot.6,
       "C:/Users/IrlanAssidiq/Documents/Rare/Rare OurFish/Trial WFF/Table 2 - Annual CPUE & VPUE.csv")

#need to calculate length on the table per row!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Rename level1_name and ma_name into the same column name to be combined
ourfish.Bolaang_Mongondow.combine.1 <- ourfish.Bolaang_Mongondow.clean[ , ]
ourfish.Bolaang_Mongondow.combine.2 <- ourfish.Bolaang_Mongondow.clean[ , ]

# Combine data from whole Bolaang_Mongondow with data by MA+R
ourfish.Bolaang_Mongondow.prov.ma <- rbind(ourfish.Bolaang_Mongondow.combine.1, ourfish.Bolaang_Mongondow.combine.2)

# Subset weight data by date, location, fisher_id, family
ourfish.Bolaang_Mongondow.length.fam.1 <- ourfish.Bolaang_Mongondow.prov.ma[,
                                                          .(mean_length_fam = mean(length)),
                                                          by = .(date, location, family)][order(date)]

ourfish.Bolaang_Mongondow.length.sp.1 <- ourfish.Bolaang_Mongondow.prov.ma[,
                                                         .(mean_length_sp = mean(length)),
                                                         by = .(date, location,family, species)][order(date)]

ourfish.Bolaang_Mongondow.length.sp.1 <- filter_if(ourfish.Bolaang_Mongondow.length.sp.1, is.numeric, all_vars((.) != 0))
table.sp.length <- table(c(ourfish.Bolaang_Mongondow.length.sp.1$species,
                           ourfish.Bolaang_Mongondow.length.sp.1$mean_length_sp))

write.xlsx(table.sp.length, "C:/Users/IrlanAssidiq/OneDrive - Rare/species_length.xlsx")

table.fam <- table(ourfish.Bolaang_Mongondow.prov.ma$family)
write.xlsx(table.fam, "C:/Users/IrlanAssidiq/OneDrive - Rare/Bolaang_Mongondow/plot/from 2019/famili_occ.xlsx")

# Change data into wide format
ourfish.Bolaang_Mongondow.length.fam.1.wide <- dcast(ourfish.Bolaang_Mongondow.length.fam.1,
                                         formula = date + location ~ family,
                                         value.var = "mean_length_fam")

ourfish.Bolaang_Mongondow.length.sp.1.wide <- dcast(ourfish.Bolaang_Mongondow.length.sp.1,
                                        formula = date + location + family ~ species,
                                        value.var = "mean_length_sp")

# Replace NA value with 0
ourfish.Bolaang_Mongondow.length.fam.1.wide[is.na(ourfish.Bolaang_Mongondow.length.fam.1.wide)] <- 0

ourfish.Bolaang_Mongondow.length.sp.1.wide[is.na(ourfish.Bolaang_Mongondow.length.sp.1.wide)] <- 0

# Change data back into long format
ourfish.Bolaang_Mongondow.length.fam.1.long <- melt(ourfish.Bolaang_Mongondow.length.fam.1.wide,
                                        measure.vars = c(3:19), # Please check the number of column first
                                        variable.name = "family",
                                        value.name = "mean_length_fam")
ourfish.Bolaang_Mongondow.length.fam.1.long.clean <- filter_if(ourfish.Bolaang_Mongondow.length.fam.1.long, is.numeric, all_vars((.) !=0))
ourfish.Bolaang_Mongondow.length.fam.1.long.clean <- filter_if(ourfish.Bolaang_Mongondow.length.fam.1.long.clean, is.numeric, all_vars((.) !=Inf))
ourfish.Bolaang_Mongondow.length.sp.1.long <- melt(ourfish.Bolaang_Mongondow.length.sp.1.wide,
                                       measure.vars = c(4:28), # Please check the number of column first
                                       variable.name = "species",
                                       value.name = "mean_length_sp")

ourfish.Bolaang_Mongondow.length.sp.1.long.clean <- filter_if(ourfish.Bolaang_Mongondow.length.sp.1.long, is.numeric, all_vars((.) != 0))
ourfish.Bolaang_Mongondow.length.sp.1.long.clean <- filter_if(ourfish.Bolaang_Mongondow.length.sp.1.long.clean, is.numeric, all_vars((.) !=Inf)) 

table.sp.clean <- table(ourfish.Bolaang_Mongondow.length.sp.1.long.clean$species)
write.xlsx(table.sp.clean, "C:/Users/IrlanAssidiq/OneDrive - Rare/species_occ_clean.xlsx")

#Size Structure
#creating new column for Year_month
library(zoo)
ourfish.Bolaang_Mongondow.clean$year_month <- ourfish.Bolaang_Mongondow.clean$date
ourfish.Bolaang_Mongondow.clean$year_month <- as.yearmon(ourfish.Bolaang_Mongondow.clean$year_month, format = "%Y_%m")

#creating midlength, using round up function
ourfish.Bolaang_Mongondow.clean$round_length <- round(ourfish.Bolaang_Mongondow.clean$length)

#Bobara - Carangoides coeruleopinnatus
ourfish.Bolaang_Mongondow.c_coeruleopinnatus <- ourfish.Bolaang_Mongondow.clean[species%in% "Carangoides coeruleopinnatus",
                                                         .(count = sum(count)),
                                                         by = .(year_month, round_length)] [order(year_month)]
ourfish.Bolaang_Mongondow.c_coeruleopinnatus$year_month <- as.Date(ourfish.Bolaang_Mongondow.c_coeruleopinnatus$year_month)
ourfish.Bolaang_Mongondow.c_coeruleopinnatus.wide <- ourfish.Bolaang_Mongondow.c_coeruleopinnatus %>%
  pivot_wider(names_from = year_month,
              values_from = count,
              values_fill = 0)

ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted <- ourfish.Bolaang_Mongondow.c_coeruleopinnatus.wide %>% arrange(round_length)

#===============================================================================
#Data arrangement: agar bisa dibaca sama R
dates <- colnames(ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted)[-1]

#Mari definisikan format si tanggal, nsuaikan dengan format data mentah
dates <- as.Date(dates, "%Y-%m-%d")
#New data frame from length.series data frame to be read by TropFishR
length.series.data <- list(dates = dates,
                           midLengths = ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted$round_length,
                           catch = as.matrix(ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted[,-1]))

class(length.series.data) <- "lfq"

#Change NA data into 0
length.series.data$catch[is.na(length.series.data$catch)] <- 0

#Use code below to reset plot results after using FishLife script
# dev.off()

#Plot to check size distribution
plot(length.series.data, Fname = "catch", ylab = "Panjang Ikan (cm)")


#RUNNING ELEFAN (three versions) harus running ketiganya biar dapet yang paling fit datanya
#Pastikan range nilai Linf dan K nsuai dengan species yang di analisis
#Linf min dan max bisa dicari di literatur seperti fishbase atau froese and binohlan
max.length = as.numeric(max(ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted$round_length))
Linf.froese.binohlan = 10^(0.044 + (0.9841 * log10(max.length)))
Linf.min <- as.numeric(0.9 * Linf.froese.binohlan)
Linf.max <- as.numeric(1.1 * Linf.froese.binohlan)

#Manual input for Linf min and Linf max
# Linf.min <- as.numeric(readline(prompt="Enter Linf-min: "))
# Linf.max <- as.numeric(readline(prompt="Enter Linf-max: "))

#Nilai K bisa pake fishlife juga untuk Kmin dan Kmax, biasanya range 0-1, makin slow-growth makin rendah angkanya, cek masuk akal ga nsuai biologi spesies
K.fishlife = 0.24 #Input K value from Fish Life script or other literatures/sources
K.min <- as.numeric(0.9 * K.fishlife)
K.max <- as.numeric(1.1 * K.fishlife)

#Display early life history parameters value
Linf.min
Linf.max
K.min
K.max

#Manual input for K min and K max
# K.min <- as.numeric(readline(prompt="Enter K-min: "))
# K.max <- as.numeric(readline(prompt="Enter K-max: "))

#A. ELEFAN
#ELEFAN Fitting
set.seed(1)
length.series.ELEFAN <- ELEFAN(length.series.data,
                               Linf_range = seq(Linf.min, Linf.max, 1),
                               K_range = seq(K.min, K.max, 0.05))

#Check the Rn_max value, compare it with other ELEFAN methods
length.series.ELEFAN$Rn_max

#Read the plot! Cari kotak yang warnanya paling terang, itu yang terduga paling baik
#Jumlah garis putus menunjukkan jumlah cohort di dalam data kita
plot(length.series.ELEFAN, Fname = "catch", draw = FALSE)

#Fit curve value
lfqFitCurves(length.series.ELEFAN, col = 4, par = length.series.ELEFAN$par, draw = TRUE)$Rn_max


#B. ELEFAN GA
#ELEFAN.GA Fitting
set.seed(1)
length.series.ELEFAN.GA <- ELEFAN_GA(length.series.data,
                                     low_par = list(Linf = Linf.min, K = K.min, t_anchor = -5, C = 0, ts = 0),
                                     up_par = list(Linf = Linf.max, K = K.max, t_anchor = 5, C = 10, ts = -10))

#Check the Rn_max value, compare it with other ELEFAN methods
length.series.ELEFAN.GA$Rn_max

#Read the plot! Cari kotak yang warnanya paling terang, itu yang terduga paling baik
#Model ELEFAN GA punya cohort lebih banyak karena diduga terjadi perubahan nilai K
plot(length.series.ELEFAN.GA, Fname = "catch", draw = FALSE)

#Fit curve value
lfqFitCurves(length.series.ELEFAN.GA, col = 4, par = length.series.ELEFAN.GA$par, draw = TRUE)$Rn_max


#C. ELEFAN SA
# ELEFAN.SA fitting
set.seed(1)
length.series.ELEFAN.SA <- ELEFAN_SA(length.series.data, init_par = list(Linf = ((Linf.max + Linf.min)/2), K= ((K.max + K.min)/2), t_anchor = 0, C = 0, ts = 0),
                                     low_par = list(Linf = Linf.min, K = K.min, t_anchor = -5, C = 0, ts = 10),
                                     up_par = list(Linf = Linf.max, K = K.max, t_anchor = 5, C = 10, ts = 10))

#Check the Rn_max value, compare it with other ELEFAN methods
length.series.ELEFAN.SA$Rn_max

#Read the plot! Lihat jumlah iterasi(pengulangan) running Linf dan K, semakin kecil cost value maka semakin besar Rn
plot(length.series.ELEFAN.SA, Fname = "catch", draw = FALSE)

#Fit curve value
lfqFitCurves(length.series.ELEFAN.SA, col = 4, par = length.series.ELEFAN.SA$par, draw = TRUE)$Rn_max

#sekarang punya 3 parameter life history

#Check all 3 Life History Paramaters Results one by one

#Check this values from all ELEFAN models above: agemax, Linf, & K, were they reasonable? Check to choose the most fit model
#Kalau kurang baik bisa diganti Linfmin pake yang Lmax misalnya, atau step iterasinya dibuat lebih kecil
#Nilai bisa beda, karena setiap running bisa berbeda
str(length.series.ELEFAN)

str(length.series.ELEFAN.GA)

str(length.series.ELEFAN.SA)

#Choose model with the HIGHEST Rn_max value
if (length.series.ELEFAN.GA$Rn_max > length.series.ELEFAN$Rn_max & length.series.ELEFAN.GA$Rn_max > length.series.ELEFAN.SA$Rn_max){Grafik = length.series.ELEFAN.GA}
if (length.series.ELEFAN.SA$Rn_max > length.series.ELEFAN.GA$Rn_max & length.series.ELEFAN.SA$Rn_max > length.series.ELEFAN$Rn_max){Grafik = length.series.ELEFAN.SA}
if (length.series.ELEFAN$Rn_max >= length.series.ELEFAN.GA$Rn_max & length.series.ELEFAN$Rn_max >= length.series.ELEFAN.SA$Rn_max){Grafik = length.series.ELEFAN}

#Manual input option to select ELEFAN Model with the highest Rn_max
#Selalu cek!!! walaupun Rn paling besar, tapi atribut Linf, K, dan agemax kurang bagus, harus ganti dengan yang baik
# Grafik = length.series.ELEFAN.GA

#Pauly Equation to obtain t0 value
Grafik$par$t0 = -10^(-0.3922 - (0.2752 * log10(Grafik$par$Linf)) - (1.038 * log10(Grafik$par$K)))

#Plot von Bertalanfy from best fit model choosen above
plot(Grafik, Fname = "catch", draw = FALSE)

#Fit curve value for choosen ELEFAN model
lfqFitCurves(Grafik, col = 4, par = Grafik$par, draw = TRUE)$Rn_max


#2. Plot Parameter Pertumbuhan

#Without Season Consideration
t <- seq(0, ((Grafik$agemax) + 20), 0.1)

Lt <- VBGF(list(Linf = Grafik$par$Linf, K = Grafik$par$K, t0 = Grafik$par$t0), t = t)

plot(t, Lt, type = "b", main = "vonBertalanffy Growth Curve without Season")

#With Season Consideration, kalau hujan biasanya K berkurang karena makan untuk survive bukan untuk growth
t <- seq(0, ((Grafik$agemax) + 20), 0.1)

Lt <- VBGF(list(Linf = Grafik$par$Linf, K = Grafik$par$K, t0 = Grafik$par$t0, ts=0.5, C=0.75), t = t)

plot(t, Lt, t = "b", main = "vonBertalanffy Growth Curve with Season")

#If want to see all parameters from choosen Model
# str(Grafik)

#3. Menghitung Natural Mortality
#Parameter M bisa dicek di barefoot ecologist, masukkan data parameter Grafik ke sana
# Use scripts below if data is limited, using parameters such as: Linf, K, t0, & agemax

#Methods: Pauly Linf, Hoenig, Then Growth, Alverson Carney, Then tmax
M = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                           method = c("Pauly_Linf", "Hoenig", "Then_growth", "AlversonCarney", "Then_tmax")))
M

#Method: Pauly Linf only
M.Pauly = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                                 method = "Pauly_Linf"))
M.Pauly

#Method: Hoenig only
M.Hoenig = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                                  method = "Hoenig"))
M.Hoenig

#Method: Then Growth only
M.Then.Growth = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                                       method = "Then_growth"))
M.Then.Growth

#Method: Alverson Carney only
M.Alverson.Carney = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                                           method = "AlversonCarney"))
M.Alverson.Carney

#Method: Then tmax only
M.Then.tmax = as.numeric(M_empirical(Linf = Grafik$par$Linf, K_l = Grafik$par$K, temp = 28.8, tmax = Grafik$agemax,
                                     method = "Then_tmax"))
M.Then.tmax

#Choose the most prevalent M value (Modes of M value)
length.series.data$M = 0.35

#preparing raw data for length series
ourfish.Bolaang_Mongondow.c_coeruleopinnatus.long <- ourfish.Bolaang_Mongondow.c_coeruleopinnatus.sorted %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "dates",
    values_to = "value")

ourfish.Bolaang_Mongondow.c_coeruleopinnatus.long.2 <- ourfish.Bolaang_Mongondow.c_coeruleopinnatus.long %>%
  uncount(weights = value)

#Raw Data
length.raw <- tibble(Length = ourfish.Bolaang_Mongondow.c_coeruleopinnatus.long.2$round_length)

Lmin = min(length.raw$Length)
Lmax = max(length.raw$Length)

#untuk bin pake yang selang kelas tadi, value in "by = " should be the same between bins and mid
bins = seq(Lmin, Lmax + 1, by = 2)
mid = seq(Lmin + 1, Lmax, by = 2)

#Make length frequency data frame
FR = cut(length.raw$Length, bins, include.lowest = TRUE, right = TRUE)
db.FR = transform(table(FR))

#Add mid length column
db.FR$mid = mid

#Cek lagi, sekarang data yang db.FR uda ada kolom midlength
db.FR

#4a. Calculating fishing mortality and exploitation rate using length catch curve 
#synLFQ3 terdiri dari data midlength, Linf, K, t0, dan catch(freq)
#Ini nanti keluar data bawaan packages, bukan data kita, harus diubah!! See scripts below
length.raw.data = synLFQ3

#Step 1. Run script below then view length.raw.data
#Step 2. Run "midLengths" list by pressing table symbol on the right, then press enter on console
length.raw.data$midLengths = db.FR$mid

#Step 1. Run script below then view length.raw.data
#Step 2. Run "catch" list by pressing table symbol on the right, then press enter on console
length.raw.data$catch = as.matrix(db.FR$Freq)

#Step 1. Run script below then view length.raw.data
#Step 2. Run "catch" list by pressing table symbol on the right, then press enter on console
length.raw.data$catch = as.numeric(length.raw.data$catch)

#Step 1. Run script below then view length.raw.data
#Step 2. Run "Linf" list by pressing table symbol on the right, then press enter on console
length.raw.data$Linf = Grafik$par$Linf

#Step 1. Run script below then view length.raw.data
#Step 2. Run "K" list by pressing table symbol on the right, then press enter on console
length.raw.data$K = Grafik$par$K

#Step 1. Run script below then view length.raw.data
#Step 2. Run "catch" list by pressing table symbol on the right, then press enter on console
length.raw.data$t0 = Grafik$par$t0

#ALWAYS DOUBLE CHECK ALL THE PARAMETERS VALUE ON length.raw.data, make sure the value are from previous analysis results

#Plot catch curve
#Choose two numbers with minimum interval 3 value between those numbers, make sure the two numbers choosen do not contain outlier
res.catch.curve <- catchCurve(length.raw.data, calc_ogive = TRUE, reg_int = NULL)

#Check for Catch Curve results: Z (F + M), t50, t75, t95, L50, L75, L95
#L50 = panjang ikan tertangkap pertama kali sebanyak 50%
#L75 = panjang ikan tertangkap 75% pertama kali sepanjang...
str(res.catch.curve)

#Combine previous analysis results with parameters from length.raw.data list
length.raw.data$Z = res.catch.curve$Z
length.raw.data$M = length.series.data$M

#Calculate Fishing Mortality (FM) from Z and M
length.raw.data$FM = as.numeric(length.raw.data$Z - length.series.data$M)

#Calculate Exploitation Rate (E)
length.raw.data$E = length.raw.data$FM / length.raw.data$Z

#Display result: Combine results from Catch Curve with previous analysis
hasil = res.catch.curve
hasil$M = length.series.data$M
hasil$FM = length.raw.data$FM
hasil$E = length.raw.data$E
hasil$FperM = hasil$FM / hasil$M

#Remove non-usable list from hasil
hasil$midLengths = NULL; hasil$catch = NULL; hasil$t_midL = NULL; hasil$lnC_dt = NULL;
hasil$reg_int = NULL; hasil$linear_mod = NULL;hasil$confidenceInt = NULL; hasil$intercept = NULL;
hasil$linear_mod_sel = NULL; hasil$Sobs = NULL; hasil$ln_1_S_1 = NULL; hasil$nst = NULL

#Double check E value! apakah mungkin E dari spesies kita itu di bawah 0.5, cek ke kondisi sebenarnya
#Kalau dirasa tidak cocok, balik lagi ke M yang dicari dengan berbagai metode itu, pakai angka yang lebih pas
unlist(hasil)


#4b. Calculating fishing mortality and exploitation rate using Beverton and Holt 
#Beverton-Holt Equation Scripts below use trawl selectivity equation
db.FR1 = db.FR

#Find f value -> frekuensi relatif = freq-i dibagi sum freq
db.FR1$f = db.FR1$Freq / sum(db.FR1$Freq)

#Find fk value -> itu frekuensi kumulatif, endingnya angka 1
db.FR1$fk = cumsum(db.FR1$f)
db.FR1$fk100 = 100 * (cumsum(db.FR1$f))
db.FR1$perfk = 1 / db.FR1$fk

#perfk dibagi fk = 1
db.FR1$perfk1 = db.FR1$perfk - 1
db.FR1$lnperfk = log(db.FR1$perfk1) #Equal to LN function in Microsoft Excel

#Remove Inf value in lnperfk column
db.FR1 = db.FR1[!db.FR1$lnperfk == "-Inf", ]

#nah, regresi nih, pake data frame baru, namanya boleh diganti
mod = lm(db.FR1$lnperfk ~ db.FR1$mid)
mod

#Find Selectivity Length (SL50) on this fishing gear (in this case, we use trawl), yang [1] itu buat intercept/b0 dan yang [2] itu buat slope/b1
SL50 = as.numeric(-(mod$coefficients[1] / mod$coefficients[2]))

#Cek dulu, kalau trawl itu kan logistik modelnya, nah data yang ini pake spear, dia bukan logistik jadi ga cocok
#Therefore, for the value of L50, please use L50 from "hasil"

#Below is Beverton Holt Equilibrium
#Since we do not want to use the SL50 results, therefore, use parameters from "hasil"
#Parameters used from "hasil" are Linf, K, L50 for Lc, and Lmax for La
#Default nboot value = 100
#In case of warning message pop up, please check data frame for its type and change into , cek typenya, ganti kalau ada error/warning
hasil2 = bheq(length.raw$Length, type = 1, Linf = hasil$Linf, K = hasil$K, Lc = hasil$L50, La = Lmax, nboot = 666)

#Please check whether Z value differ a alot between "hasil" and "hasil2"
#If there is a lot of difference, then use this script below for second option fo Beverton-Holt Equation
#Script below use SL50 as Lc
hasil2.b = bheq(length.raw$Length, type = 1, Linf = hasil$Linf, K = hasil$K, Lc = SL50, La = Lmax, nboot = 666)

#If use Z value from Beverton-Holt Equation above, then run this script below
hasil2$Linf = length.raw.data$Linf
hasil2$K = length.raw.data$K
hasil2$t0 = length.raw.data$t0
hasil2$M = length.series.data$M
hasil2$FM = hasil2$z - hasil2$M
hasil2$E = hasil2$FM / hasil2$z
hasil2$FperM = hasil2$FM / hasil2$M
hasil2$Lc = SL50 #or use hasil$L50
unlist(hasil2)

#Calculate Lmat and Lopt using and combine it with Catch Curve Analysis Results
hasil$Lmat = 10^((0.8979 * log10(hasil$Linf)) - 0.0782) #Source: Froese & Binohlan 2000
hasil$Lopt.Froese.Binohlan = 10^((1.0421 * log10(hasil$Linf)) - 0.2742) #Source: Froese & Binohlan 2000
hasil$Lopt.Cope.Punt1 = 1.33 * hasil$L50 #Source: Cope & Punt
hasil$Lopt.Cope.Punt2 = 1.33 * hasil$Lmat #Source: Cope & Punt
hasil$Amax = Grafik$agemax
hasil$MperK = hasil$M / hasil$K

#Calculate Lmat and Lopt using and combine it with Beverton-Holt Analysis Results
hasil2$Lmat = 10^((0.8979 * log10(hasil2$Linf)) - 0.0782) #Source: Froese & Binohlan 2000
hasil2$Lopt.Froese.Binohlan = 10^((1.0421 * log10(hasil2$Linf)) - 0.2742) #Source: Froese & Binohlan 2000
hasil2$Lopt.Cope.Punt1 = 1.33 * hasil2$Lc #Source: Cope & Punt 2009
hasil2$Lopt.Cope.Punt2 = 1.33 * hasil2$Lmat #Source: Cope & Punt 2009
hasil2$Amax = Grafik$agemax
hasil2$MperK = hasil2$M / hasil2$K


#Analysis Summary from Catch Curve and Beverton-Holt Equation
unlist(hasil) #From Catch Curve
unlist(hasil2) #From Beverton-Holt

#Froese Indicators: Percentage of Fish above maturity length
percentMature = round(length(which(length.raw$Length > hasil$Lmat)) / length(length.raw$Length) * 100, digits = 1)

#Froese Indicators: Percentage of Fish between optimum length threshold - Using Cope & Punt Lopt (Could be changed to other Option)
Lopt.lower = 0.9 * hasil$Lopt.Cope.Punt2 #Lopt can be changed
Lopt.upper = 1.1 * hasil$Lopt.Cope.Punt2 #Lopt can be changed
percentOpt = round(length(which(length.raw$Length > Lopt.lower & length.raw$Length < Lopt.upper)) / length(length.raw$Length) * 100, digits = 1)

#Froese Indicators: Percentage of Megaspawner Fish
Lmega = round(1.1 * hasil$Lopt.Cope.Punt2, digits = 1) #Use the same Lopt value with above Popt scripts
percentMega = round(length(which(length.raw$Length > Lmega)) / length(length.raw$Length) * 100, digits = 1)

Lmega
Lmat = hasil$Lmat
percentMature
percentOpt
percentMega

#plotting size structure
#data prep
library(summarytools)

length.freq <- table(length.raw)
size.structure <- as.data.frame(length.freq)
size.structure$Length <- as.numeric(as.character(size.structure$Length))

#plotting
size.structure.plot <- ggplot(size.structure, aes(x=Length, y = Freq)) +
  geom_bar(stat = "Identity") +
  geom_vline(xintercept = Lmat,
             linetype = "dashed",
             size = 2,
             color = "#B50A37"
             ) +
  geom_label(x = Lmat, y = 0, 
           label = "PMat = 32.3%",
           fill = "white"
           )+
  geom_vline(xintercept = c(Lopt.lower, Lopt.upper),
             linetype = "dashed",
             size = 2,
             color = "#F58232")+
  geom_label(x = Lopt.lower, y = 0, 
             label = "POpt = 9.2%",
             fill = "white"
  )+
  geom_vline(xintercept = Lmega,
             linetype = "dashed",
             size = 2,
             color = "#00B6DE"
  ) +
  geom_label(x = Lmega, y = 0, 
             label = "PMega = 0.7%",
             fill = "white"
  )+
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
ggsave("c_coeruleopinnatus size.svg", plot = size.structure.plot, bg = "transparent", width = 13.33, height = 7.3)

#total landings & CPUE L-coeruleopinnatus================================================
ourfish.c_coeruleopinnatus.landings.1 <- ourfish.Bolaang_Mongondow.clean[species %in% "Carangoides coeruleopinnatus",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, species)][order(date)]

ourfish.c_coeruleopinnatus.landings.1.glm <- ourfish.c_coeruleopinnatus.landings.1 %>%
  nest(-'species') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.c_coeruleopinnatus.landings.1.glm2 <- ourfish.c_coeruleopinnatus.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(species, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.c_coeruleopinnatus.landings.1.glm2)

# Label Landings trends
ourfish.c_coeruleopinnatus.landings.1.glm3 <- ourfish.c_coeruleopinnatus.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.c_coeruleopinnatus.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.c_coeruleopinnatus.landings.1.glm3)

ourfish.c_coeruleopinnatus.landings.1.plot <- ggplot(ourfish.c_coeruleopinnatus.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-06-01", format("%Y-%m-%d")),
                y = 5,
                label = trend),
            hjust = 0.1,
            color = "#e78828", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings L.coeruleopinnatus in MAR Bolaang_Mongondow",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Bolaang_Mongondow.indo <- ourfish.c_coeruleopinnatus.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("landings c_coeruleopinnatus Bolaang_Mongondow.svg", plot = landings.Bolaang_Mongondow.indo, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, species for total landings
ourfish.c_coeruleopinnatus.cpue.1 <- ourfish.Bolaang_Mongondow.clean[species %in% "Carangoides coeruleopinnatus",
                                                    .(sum_landings = sum(weight_kg)),
                                                    by = .(date, species, fisher_id)][order(date)]

ourfish.c_coeruleopinnatus.cpue.1.glm <- ourfish.c_coeruleopinnatus.cpue.1 %>%
  nest(-species) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.c_coeruleopinnatus.cpue.1.glm2 <- ourfish.c_coeruleopinnatus.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(species, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.c_coeruleopinnatus.cpue.1.glm2)

# Label Landings trends
ourfish.c_coeruleopinnatus.cpue.1.glm3 <- ourfish.c_coeruleopinnatus.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.c_coeruleopinnatus.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.c_coeruleopinnatus.cpue.1.glm3)

ourfish.c_coeruleopinnatus.cpue.1.plot <- ggplot(ourfish.c_coeruleopinnatus.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#e78828",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort L.coeruleopinnatus in MAR Bolaang_Mongondow",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Bolaang_Mongondow.indo <- ourfish.c_coeruleopinnatus.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue c_coeruleopinnatus Bolaang_Mongondow.svg", plot = cpue.Bolaang_Mongondow.indo, bg = "transparent", width = 13.33, height = 7.3)
