
##### ses Data #####

# Set working directory
setwd("C:/Users/IrlanAssidiq/OneDrive - Rare/ourfish/Data sharing")
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
ourfish.raw.ses <- ourfish.raw[date >= start.date & date <= end.date &
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

# Join OurFish raw ses data with fish meta data
ourfish.all <- ourfish.data [fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.ses <- ourfish.raw.ses[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.ns <- ourfish.raw.ns[fish.meta, on = .(buying_unit), nomatch = NULL]

write.xlsx(ourfish.ns, "data ourfish sulut.xlsx" )
ourfish.nmal <- ourfish.raw.nmal[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.mal <- ourfish.raw.mal[fish.meta, on = .(buying_unit), nomatch = NULL]

#cleaning weight=================================================================
# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.ses$weight_kg, 0.01)
quantile(ourfish.ses$weight_kg, 0.99)

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.ses[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.ses[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.ses$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.ses.outlier <- ourfish.ses[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.ses.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_ses_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.ses$weight_kg, 0.025)
# q3 <- quantile(ourfish.ses$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.ses[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.ses[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.ses.clean <- ourfish.ses[-landings.data.outliers$Obs.Num, ]
#clean count====================================================================
quantile(ourfish.ses.clean$count, 0.01)
quantile(ourfish.ses.clean$count, 0.99)

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.ses.clean[count >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.ses.clean[count <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.ses.clean$count, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.ses.outlier <- ourfish.ses.clean[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.ses.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_ses_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.ses$weight_kg, 0.025)
# q3 <- quantile(ourfish.ses$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.ses[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.ses[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.ses.clean.2 <- ourfish.ses.clean[-landings.data.outliers$Obs.Num, ]
ourfish.ses.clean.2

#CCVA Parameter=================================================================
#Fish susceptibility============================================================
#choosing the top 3 species, based on their production, from Ourfish data
#Talaga Raya

fish.suscept.talaga_raya <- ourfish.ses.clean.2[ma_name %in% c("Talaga Raya"),
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.kapuntori <- ourfish.ses.clean.2[ma_name %in% c("Kapuntori"),
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.mawasangka <- ourfish.ses.clean.2[ma_name %in% c("Mawasangka") &
                                               !buyer_name %in% "egerman",
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.zona_III <- ourfish.ses.clean.2[buyer_name %in% c("egerman"),
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.mawasangka_timur <- ourfish.ses.clean.2[ma_name %in% c("Mawasangka Timur"),
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.pasi_kolaga <- ourfish.ses.clean.2[ma_name %in% c("Pasi Kolaga"),
                                                     .(landing = sum(weight_kg)),
                                                     by = .(ma_name, species)]

fish.suscept.kadatua <- ourfish.ses.clean.2[ma_name %in% c("Kadatua"),
                                                .(landing = sum(weight_kg)),
                                                by = .(ma_name, species)]

fish.suscept.siompu <- ourfish.ses.clean.2[ma_name %in% c("Siompu"),
                                            .(landing = sum(weight_kg)),
                                            by = .(ma_name, species)]

#Fish catch change============================================================
#Trends in fishery production over the last 2 years (5 point likert scale)
start.date <- "2023-01-01"
end.date <- "2025-10-31"

#Talaga Raya
landings.talaga_raya <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                            ma_name %in% "Talaga Raya",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, ma_name)][order(date)]

landings.talaga_raya.glm <- landings.talaga_raya %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.talaga_raya.glm2 <- landings.talaga_raya.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.talaga_raya.glm2)

# Label Landings trends
landings.talaga_raya.glm3 <- landings.talaga_raya.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- landings.talaga_raya.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.talaga_raya.glm3)

landings.talaga_raya.plot <- ggplot(landings.talaga_raya, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Talaga Raya",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.talaga_raya <- landings.talaga_raya.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.talaga_raya <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                          ma_name %in% "Talaga Raya",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, ma_name, fisher_id)][order(date)]

cpue.talaga_raya.glm <- cpue.talaga_raya %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.talaga_raya.glm2 <- cpue.talaga_raya.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.talaga_raya.glm2)

# Label Landings trends
cpue.talaga_raya.glm3 <- cpue.talaga_raya.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- cpue.talaga_raya.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.talaga_raya.glm3)

cpue.talaga_raya.plot <- ggplot(cpue.talaga_raya, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Talaga Raya",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.talaga_raya <- cpue.talaga_raya.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Kapuntori
landings.Kapuntori <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                              ma_name %in% "Kapuntori",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, ma_name)][order(date)]

landings.Kapuntori.glm <- landings.Kapuntori %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Kapuntori.glm2 <- landings.Kapuntori.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Kapuntori.glm2)

# Label Landings trends
landings.Kapuntori.glm3 <- landings.Kapuntori.glm2[,
                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                          stat_test == "not significant" ~ "Stable",
                                                                          TRUE ~ "Decreased")]

trend.label <- landings.Kapuntori.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Kapuntori.glm3)

landings.Kapuntori.plot <- ggplot(landings.Kapuntori, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Kapuntori",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
  landings.Kapuntori <- landings.Kapuntori.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Kapuntori <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                          ma_name %in% "Kapuntori",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, ma_name, fisher_id)][order(date)]

cpue.Kapuntori.glm <- cpue.Kapuntori %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Kapuntori.glm2 <- cpue.Kapuntori.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Kapuntori.glm2)

# Label Landings trends
cpue.Kapuntori.glm3 <- cpue.Kapuntori.glm2[,
                                               trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                  stat_test == "not significant" ~ "Stable",
                                                                  TRUE ~ "Decreased")]

trend.label <- cpue.Kapuntori.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Kapuntori.glm3)

cpue.Kapuntori.plot <- ggplot(cpue.Kapuntori, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Kapuntori",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Kapuntori <- cpue.Kapuntori.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#Mawasangka
landings.mawasangka <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                              ma_name %in% "Mawasangka",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, ma_name)][order(date)]

landings.mawasangka.glm <- landings.mawasangka %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.mawasangka.glm2 <- landings.mawasangka.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.mawasangka.glm2)

# Label Landings trends
landings.mawasangka.glm3 <- landings.mawasangka.glm2[,
                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                          stat_test == "not significant" ~ "Stable",
                                                                          TRUE ~ "Decreased")]

trend.label <- landings.mawasangka.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.mawasangka.glm3)

landings.mawasangka.plot <- ggplot(landings.mawasangka, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR mawasangka",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.mawasangka <- landings.mawasangka.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.mawasangka <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                          ma_name %in% "Mawasangka",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, ma_name, fisher_id)][order(date)]

cpue.mawasangka.glm <- cpue.mawasangka %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.mawasangka.glm2 <- cpue.mawasangka.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.mawasangka.glm2)

# Label Landings trends
cpue.mawasangka.glm3 <- cpue.mawasangka.glm2[,
                                               trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                  stat_test == "not significant" ~ "Stable",
                                                                  TRUE ~ "Decreased")]

trend.label <- cpue.mawasangka.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.mawasangka.glm3)

cpue.mawasangka.plot <- ggplot(cpue.mawasangka, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR mawasangka",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.mawasangka <- cpue.mawasangka.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Zona III
landings.Zona_III <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                             buyer_name %in% "egerman",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, buyer_name)][order(date)]

landings.Zona_III.glm <- landings.Zona_III %>%
  nest(-'buyer_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Zona_III.glm2 <- landings.Zona_III.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(buyer_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Zona_III.glm2)

# Label Landings trends
landings.Zona_III.glm3 <- landings.Zona_III.glm2[,
                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                        stat_test == "not significant" ~ "Stable",
                                                                        TRUE ~ "Decreased")]

trend.label <- landings.Zona_III.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Zona_III.glm3)

landings.Zona_III.plot <- ggplot(landings.Zona_III, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "3 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Zona_III",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Zona_III <- landings.Zona_III.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Zona_III <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                         buyer_name %in% "egerman",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, buyer_name, fisher_id)][order(date)]

cpue.Zona_III.glm <- cpue.Zona_III %>%
  nest(-buyer_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Zona_III.glm2 <- cpue.Zona_III.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(buyer_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Zona_III.glm2)

# Label Landings trends
cpue.Zona_III.glm3 <- cpue.Zona_III.glm2[,
                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                stat_test == "not significant" ~ "Stable",
                                                                TRUE ~ "Decreased")]

trend.label <- cpue.Zona_III.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Zona_III.glm3)

cpue.Zona_III.plot <- ggplot(cpue.Zona_III, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Zona_III",
       x = " ",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Zona_III <- cpue.Zona_III.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Mawasangka_Timur
landings.Mawasangka_Timur <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                             ma_name %in% "Mawasangka Timur",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, ma_name)][order(date)]

landings.Mawasangka_Timur.glm <- landings.Mawasangka_Timur %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Mawasangka_Timur.glm2 <- landings.Mawasangka_Timur.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Mawasangka_Timur.glm2)

# Label Landings trends
landings.Mawasangka_Timur.glm3 <- landings.Mawasangka_Timur.glm2[,
                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                        stat_test == "not significant" ~ "Stable",
                                                                        TRUE ~ "Decreased")]

trend.label <- landings.Mawasangka_Timur.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Mawasangka_Timur.glm3)

landings.Mawasangka_Timur.plot <- ggplot(landings.Mawasangka_Timur, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Mawasangka_Timur",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Mawasangka_Timur <- landings.Mawasangka_Timur.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Mawasangka_Timur <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                         ma_name %in% "Mawasangka Timur",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, ma_name, fisher_id)][order(date)]

cpue.Mawasangka_Timur.glm <- cpue.Mawasangka_Timur %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Mawasangka_Timur.glm2 <- cpue.Mawasangka_Timur.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Mawasangka_Timur.glm2)

# Label Landings trends
cpue.Mawasangka_Timur.glm3 <- cpue.Mawasangka_Timur.glm2[,
                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                stat_test == "not significant" ~ "Stable",
                                                                TRUE ~ "Decreased")]

trend.label <- cpue.Mawasangka_Timur.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Mawasangka_Timur.glm3)

cpue.Mawasangka_Timur.plot <- ggplot(cpue.Mawasangka_Timur, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.3, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Mawasangka_Timur",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Mawasangka_Timur <- cpue.Mawasangka_Timur.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Pasi Kolaga
landings.Pasi_Kolaga <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                             ma_name %in% "Pasi Kolaga",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, ma_name)][order(date)]

landings.Pasi_Kolaga.glm <- landings.Pasi_Kolaga %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Pasi_Kolaga.glm2 <- landings.Pasi_Kolaga.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Pasi_Kolaga.glm2)

# Label Landings trends
landings.Pasi_Kolaga.glm3 <- landings.Pasi_Kolaga.glm2[,
                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                        stat_test == "not significant" ~ "Stable",
                                                                        TRUE ~ "Decreased")]

trend.label <- landings.Pasi_Kolaga.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Pasi_Kolaga.glm3)

landings.Pasi_Kolaga.plot <- ggplot(landings.Pasi_Kolaga, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Pasi_Kolaga",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Pasi_Kolaga <- landings.Pasi_Kolaga.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Pasi_Kolaga <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                         ma_name %in% "Pasi Kolaga",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, ma_name, fisher_id)][order(date)]

cpue.Pasi_Kolaga.glm <- cpue.Pasi_Kolaga %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Pasi_Kolaga.glm2 <- cpue.Pasi_Kolaga.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Pasi_Kolaga.glm2)

# Label Landings trends
cpue.Pasi_Kolaga.glm3 <- cpue.Pasi_Kolaga.glm2[,
                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                stat_test == "not significant" ~ "Stable",
                                                                TRUE ~ "Decreased")]

trend.label <- cpue.Pasi_Kolaga.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Pasi_Kolaga.glm3)

cpue.Pasi_Kolaga.plot <- ggplot(cpue.Pasi_Kolaga, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Pasi_Kolaga",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Pasi_Kolaga <- cpue.Pasi_Kolaga.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Kadatua
landings.Kadatua <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                              ma_name %in% "Kadatua",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, ma_name)][order(date)]

landings.Kadatua.glm <- landings.Kadatua %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Kadatua.glm2 <- landings.Kadatua.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Kadatua.glm2)

# Label Landings trends
landings.Kadatua.glm3 <- landings.Kadatua.glm2[,
                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                          stat_test == "not significant" ~ "Stable",
                                                                          TRUE ~ "Decreased")]

trend.label <- landings.Kadatua.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Kadatua.glm3)

landings.Kadatua.plot <- ggplot(landings.Kadatua, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Kadatua",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Kadatua <- landings.Kadatua.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Kadatua <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                          ma_name %in% "Kadatua",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, ma_name, fisher_id)][order(date)]

cpue.Kadatua.glm <- cpue.Kadatua %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Kadatua.glm2 <- cpue.Kadatua.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Kadatua.glm2)

# Label Landings trends
cpue.Kadatua.glm3 <- cpue.Kadatua.glm2[,
                                               trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                  stat_test == "not significant" ~ "Stable",
                                                                  TRUE ~ "Decreased")]

trend.label <- cpue.Kadatua.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Kadatua.glm3)

cpue.Kadatua.plot <- ggplot(cpue.Kadatua, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Kadatua",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Kadatua <- cpue.Kadatua.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)

#Siompu
landings.Siompu <- ourfish.ses.clean.2[date >= start.date & date <= end.date & 
                                          ma_name %in% "Siompu",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, ma_name)][order(date)]

landings.Siompu.glm <- landings.Siompu %>%
  nest(-'ma_name') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

landings.Siompu.glm2 <- landings.Siompu.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(landings.Siompu.glm2)

# Label Landings trends
landings.Siompu.glm3 <- landings.Siompu.glm2[,
                                               trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                  stat_test == "not significant" ~ "Stable",
                                                                  TRUE ~ "Decreased")]

trend.label <- landings.Siompu.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(landings.Siompu.glm3)

landings.Siompu.plot <- ggplot(landings.Siompu, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4", #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Siompu",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.Siompu <- landings.Siompu.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("landings Kapuntori.svg", plot = landings.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
cpue.Siompu <- ourfish.ses.clean.2[date >= start.date & date <= end.date &
                                      ma_name %in% "Siompu",
                                    .(sum_landings = sum(weight_kg)),
                                    by = .(date, ma_name, fisher_id)][order(date)]

cpue.Siompu.glm <- cpue.Siompu %>%
  nest(-ma_name) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

cpue.Siompu.glm2 <- cpue.Siompu.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(ma_name, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(cpue.Siompu.glm2)

# Label Landings trends
cpue.Siompu.glm3 <- cpue.Siompu.glm2[,
                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                          stat_test == "not significant" ~ "Stable",
                                                          TRUE ~ "Decreased")]

trend.label <- cpue.Siompu.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(cpue.Siompu.glm3)

cpue.Siompu.plot <- ggplot(cpue.Siompu, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-06-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",  #Orange #e78828; Hijau #00833e; Tosca #06a7b4
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Siompu",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.Siompu <- cpue.Siompu.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
#ggsave("cpue Kapuntori.svg", plot = cpue.Kapuntori, bg = "transparent", width = 13.33, height = 7.3)



#NSUL - cleaning weight=================================================================
# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.ns$weight_kg, 0.01)
quantile(ourfish.ns$weight_kg, 0.99)

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.ns[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.ns[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.ns$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.ns.outlier <- ourfish.ns[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.ns.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_ns_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.ns$weight_kg, 0.025)
# q3 <- quantile(ourfish.ns$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.ns[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.ns[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.ns.clean <- ourfish.ns[-landings.data.outliers$Obs.Num, ]
#clean count====================================================================
quantile(ourfish.ns.clean$count, 0.01)
quantile(ourfish.ns.clean$count, 0.99)

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.ns.clean[count >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.ns.clean[count <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.ns.clean$count, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.ns.outlier <- ourfish.ns.clean[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.ns.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_ns_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.ns$weight_kg, 0.025)
# q3 <- quantile(ourfish.ns$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.ns[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.ns[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.ns.clean.2 <- ourfish.ns.clean[-landings.data.outliers$Obs.Num, ]

write.xlsx(ourfish.ns.clean.2, "data ourfish sulut-clean.xlsx" )
#start filtering================================================================

ourfish.nsul<- ourfish.raw[#month >= 2 & month <= 7 &
                                buyer_name %in% "ramla"]
ourfish.rohaya <- ourfish.rohaya[date >= "2025-04-01" & date <= "2025-05-31" &
                                   label %in% "Teri"]

ourfish.ses.landings.Teri.rohaya <- ourfish.rohaya[buyer_name %in% "rohaya",
                                                  .(Berat = sum(weight_kg)),
                                                  by = .(date, label)][order(date)]

write.xlsx(ourfish.ses.landings.Teri.rohaya, "C:/Users/IrlanAssidiq/OneDrive - Rare/Ourfish Teri.xlsx")

#===============================================================================
ourfish.yulvice<- ourfish.raw[#month >= 2 & month <= 7 &
  buyer_name %in% "yulvice"]

write.xlsx(ourfish.yulvice, "C:/Users/IrlanAssidiq/OneDrive - Rare/Ourfish yulvice.xlsx")

ourfish.nmal<- ourfish.raw[#month >= 2 & month <= 7 &
  snu_name %in% "North Maluku"]

write.xlsx(ourfish.nmal, "C:/Users/IrlanAssidiq/OneDrive - Rare/Ourfish Malut.xlsx")

#===============================================================================
ourfish.egerman<- ourfish.raw[#month >= 2 & month <= 7 &
  buyer_name %in% "egerman"]

ourfish.ses.landings.egerman <- ourfish.egerman[buyer_name %in% "egerman",
                                                   .(Berat = sum(weight_kg)),
                                                   by = .(date, buying_unit, label)][order(date)]

write.xlsx(ourfish.ses.landings.egerman, "C:/Users/IrlanAssidiq/OneDrive - Rare/Ourfish egerman.xlsx")

library('officer')
library('svglite')

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
# Zona III Masteng =============================================================
setnames(ourfish.egerman, "buyer_name", "location")
ourfish.ses.landings.1 <- ourfish.egerman[location %in% "egerman",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#00833e", fill = "#00833e") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-07-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "#00833e",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Zona III",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.zona3 <- ourfish.ses.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
ggsave("total landings zona III.svg", plot = landings.zona3, bg = "transparent", width = 13.33, height = 7.3)
#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.egerman[location == "egerman",
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#00833e", fill = "#00833e") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-07-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "#00833e",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Zona III",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.zona3 <- ourfish.ses.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue zona III.svg", plot = cpue.zona3, bg = "transparent", width = 13.33, height = 7.3)

#IB Liana Banggai, Ramla dan Hasan==============================================
ourfish.liana.banggai<- ourfish.raw[#month >= 2 & month <= 7 &
  buyer_name %in% c("hasanlakorua", "ramla")]

ourfish.ses.landings.liana.banggai <- ourfish.liana.banggai[buyer_name %in% c("hasanlakorua", "ramla"),
                                                .(Berat = sum(weight_kg)),
                                                by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.liana.banggai, "buyer_install", "location")
ourfish.ses.landings.1 <- ourfish.liana.banggai[location %in% "Buton Tengah",
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "#e78828",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Liana Banggai",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.lianabanggai <- ourfish.ses.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("landings liana banggai.svg", plot = landings.lianabanggai, bg = "transparent", width = 13.33, height = 7.3)

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.liana.banggai[location %in% "Buton Tengah",
                                      .(sum_landings = sum(weight_kg)),
                                      by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 5,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Liana Banggai",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.lianabanggai <- ourfish.ses.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue liana banggai.svg", plot = cpue.lianabanggai, bg = "transparent", width = 13.33, height = 7.3)


#IB Lasalimu====================================================================
ourfish.lasalimu<- ourfish.raw[#month >= 2 & month <= 7 &
  ma_name %in% "Lasalimu"]

ourfish.ses.landings.lasalimu <- ourfish.lasalimu[ma_name %in% "Lasalimu",
                                                            .(Berat = sum(weight_kg)),
                                                            by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.lasalimu, "ma_name", "location")
ourfish.ses.landings.1 <- ourfish.lasalimu[location %in% "Lasalimu",
                                                .(sum_landings = sum(weight_kg)),
                                                by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-02-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "#e78828",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Lasalimu",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.lasalimu <- ourfish.ses.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("landings lasalimu.svg", plot = landings.lasalimu, bg = "transparent", width = 13.33, height = 7.3)


#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.lasalimu[location %in% "Lasalimu",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-02-01", format("%Y-%m-%d")),
                y = 4,
                label = trend),
            hjust = 0.1,
            color = "#e78828",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Lasalimu",
       x = "Month",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.lasalimu <- ourfish.ses.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue lasalimu.svg", plot = cpue.lasalimu, bg = "transparent", width = 13.33, height = 7.3)

#Pasarwajo 1, Ngapatowa=========================================================
ourfish.ngapatowa<- ourfish.raw[#month >= 2 & month <= 7 &
  ma_name %in% "Teluk Pasarwajo 1"]

ourfish.ses.landings.ngapatowa <- ourfish.ngapatowa[ma_name %in% "Teluk Pasarwajo 1",
                                                  .(Berat = sum(weight_kg)),
                                                  by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.ngapatowa, "ma_name", "location")
ourfish.ses.landings.1 <- ourfish.ngapatowa[location %in% "Teluk Pasarwajo 1",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decreased")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#06a7b4", fill = "#06a7b4") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-05-01", format("%Y-%m-%d")),
                y = 100,
                label = trend),
            hjust = 0.1,
            color = "#06a7b4",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in MAR Ngapatowa",
       x = "",
       y = "Total landings(kg)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
landings.ngapatowa <- ourfish.ses.landings.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("landings ngapatowa.svg", plot = landings.ngapatowa, bg = "transparent", width = 13.33, height = 7.3)
#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.ngapatowa[location %in% "Teluk Pasarwajo 1",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increased", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decreased")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "#e78828", fill = "#e78828") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-11-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "#e78828",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch Per Unit Effort in MAR Ngapatowa",
       x = "",
       y = "CPUE (kg/fisher/trip)") +
  theme.default.1 +
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
    panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
  )
cpue.ngapatowa <- ourfish.ses.cpue.1.plot + theme(
  plot.background = element_rect(fill = "transparent", colour = NA), # Makes the entire plot background transparent
  panel.background = element_rect(fill = "transparent", colour = NA) # Makes the panel background transparent
)
ggsave("cpue ngapatowa.svg", plot = cpue.ngapatowa, bg = "transparent", width = 13.33, height = 7.3)

#Indonesia======================================================================
# Zona III Masteng =============================================================
setnames(ourfish.egerman, "buyer_name", "location")
ourfish.ses.landings.1 <- ourfish.egerman[location %in% "egerman",
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-07-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Kawasan PAAP Zona III",
       x = "Bulan",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.egerman[location == "egerman",
                                      .(sum_landings = sum(weight_kg)),
                                      by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-07-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut di Kawasan PAAP Zona III",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1

#IB Liana Banggai, Ramla dan Hasan==============================================
ourfish.liana.banggai<- ourfish.raw[#month >= 2 & month <= 7 &
  buyer_name %in% c("hasanlakorua", "ramla")]

ourfish.ses.landings.liana.banggai <- ourfish.liana.banggai[buyer_name %in% c("hasanlakorua", "ramla"),
                                                            .(Berat = sum(weight_kg)),
                                                            by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.liana.banggai, "buyer_install", "location")
ourfish.ses.landings.1 <- ourfish.liana.banggai[location %in% "Buton Tengah",
                                                .(sum_landings = sum(weight_kg)),
                                                by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Kawasan PAAP Liana Banggai",
       x = "Bulan",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.liana.banggai[location %in% "Buton Tengah",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut di Kawasan PAAP Liana Banggai",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1

#IB Lasalimu====================================================================
ourfish.lasalimu<- ourfish.raw[#month >= 2 & month <= 7 &
  ma_name %in% "Lasalimu"]

ourfish.ses.landings.lasalimu <- ourfish.lasalimu[ma_name %in% "Lasalimu",
                                                  .(Berat = sum(weight_kg)),
                                                  by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.lasalimu, "ma_name", "location")
ourfish.ses.landings.1 <- ourfish.lasalimu[location %in% "Lasalimu",
                                           .(sum_landings = sum(weight_kg)),
                                           by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 30,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Kawasan PAAP Lasalimu",
       x = "Bulan",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.lasalimu[location %in% "Lasalimu",
                                       .(sum_landings = sum(weight_kg)),
                                       by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2025-03-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut di Kawasan PAAP Lasalimu",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1

#Pasarwajo 1, Ngapatowa=========================================================
ourfish.ngapatowa<- ourfish.raw[#month >= 2 & month <= 7 &
  ma_name %in% "Teluk Pasarwajo 1"]

ourfish.ses.landings.ngapatowa <- ourfish.ngapatowa[ma_name %in% "Teluk Pasarwajo 1",
                                                    .(Berat = sum(weight_kg)),
                                                    by = .(date, buying_unit, label)][order(date)]

setnames(ourfish.ngapatowa, "ma_name", "location")
ourfish.ses.landings.1 <- ourfish.ngapatowa[location %in% "Teluk Pasarwajo 1",
                                            .(sum_landings = sum(weight_kg)),
                                            by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-'location') %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-05-01", format("%Y-%m-%d")),
                y = 100,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Kawasan PAAP Ngapatowa",
       x = "Bulan",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.ngapatowa[location %in% "Teluk Pasarwajo 1",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2023-05-01", format("%Y-%m-%d")),
                y = 15,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut di Kawasan PAAP Ngapatowa",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1

#target Muna====================================================================

# Combine data from whole ses with data by MA+R
ourfish.ses.prov.ma <- ourfish.ses.clean
setnames(ourfish.ses.prov.ma, "lgu_name", "location")

start.muna <- "2022-01-01"
end.muna <- "2025-12-31"

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.landings.1 <- ourfish.ses.prov.ma[location == "Muna",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "12 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y =30,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Kabupaten Muna",
       x = "Tahun",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.ses.prov.ma[location == "Muna",
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "12 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip) di Kabupaten Muna",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1

#### Total Landings Trends in Mataoleo ####
ourfish.ses.prov.ma <- ourfish.all.clean
setnames(ourfish.ses.prov.ma, "ma_name", "location")
# Subset weight data by date, location for total landings
ourfish.mataoleo <- ourfish.ses.prov.ma[location == "Mataoleo"]
weight.mataoleo <- ourfish.ses.prov.ma[location == "Mataoleo",
                                       .(ton = sum(weight_kg)/1000),
                                       by = .(location)]
revenue.mataoleo <- ourfish.ses.prov.ma[location == "Mataoleo",
                                       .(rev = sum(total_price_local)),
                                       by = .(location)]
trip.mataoleo <- ourfish.ses.prov.ma[location == "Mataoleo",
                                       .(trip = uniqueN(id)),
                                       by = .(location)]


ourfish.ses.landings.1 <- ourfish.ses.prov.ma[location == "Mataoleo",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.ses.landings.1.glm <- ourfish.ses.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2 <- ourfish.ses.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2)

# Label Landings trends
ourfish.ses.landings.1.glm3 <- ourfish.ses.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.ses.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3)

ourfish.ses.landings.1.plot <- ggplot(ourfish.ses.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "12 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-04-01", format("%Y-%m-%d")),
                y =180,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di PAAP Mataoleo",
       x = "Tahun",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.ses.prov.ma[location == "Mataoleo",
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.ses.cpue.1.glm <- ourfish.ses.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.cpue.1.glm2 <- ourfish.ses.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.cpue.1.glm2)

# Label Landings trends
ourfish.ses.cpue.1.glm3 <- ourfish.ses.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stabil",
                                                                      TRUE ~ "Menurun")]

trend.label <- ourfish.ses.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.cpue.1.glm3)

ourfish.ses.cpue.1.plot <- ggplot(ourfish.ses.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "12 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-04-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip) di PAAP Mataoleo",
       x = "Tahun",
       y = "Tangkapan ikan per satu kali melaut (kg/nelayan/trip)") +
  theme.default.1



#===============================================================================
ourfish.ses.Muna <- ourfish.ses[lgu_name %in% "Muna"]
write.xlsx(ourfish.ses.Muna,"C:/Users/IrlanAssidiq/OneDrive - Rare/Ourfish Muna 2024.xlsx")

Muna.kakap_merah <- ourfish.ses.Muna[label %in% "Kakap Merah"]
sum(Muna.kakap_merah$weight_kg) #196.3

Muna.kerapu <- ourfish.ses.Muna[label %in% "Kerapu"]
sum(Muna.kerapu$weight_kg) #51.5

Muna.kerapu_merah <- ourfish.ses.Muna[label %in% "Kerapu Merah"]
sum(Muna.kerapu_merah$weight_kg) #8.6

Muna.kerapu_muso <- ourfish.ses.Muna[label %in% "Kerapu Muso"]
sum(Muna.kerapu_muso$weight_kg) #74.9

Muna.kerapu_mix <- ourfish.ses.Muna[label %in% "Kerapu Mix"]
sum(Muna.kerapu_mix$weight_kg) #19

Muna.kerapu_macan <- ourfish.ses.Muna[label %in% "Kerapu Macan"]
sum(Muna.kerapu_macan$weight_kg) #3

Muna.sunu_merah <- ourfish.ses.Muna[label %in% "Sunu Merah"]
sum(Muna.sunu_merah$weight_kg) #13.6

Muna.bobara <- ourfish.ses.Muna[label %in% "Bobara"]
sum(Muna.bobara$weight_kg) #10

Muna.limboko <- ourfish.ses.Muna[label %in% "Limboko"]
sum(Muna.limboko$weight_kg) #217
                                         
setnames(ourfish.egerman, "buyer_name", "location")
ourfish.egerman$length <- ((ourfish.egerman$weight_kg/ourfish.egerman$count)/ourfish.egerman$a)^1/ourfish.egerman$b 

write.xlsx(ourfish.egerman, "C:/Users/IrlanAssidiq/Desktop/data egerman zona iii.xlsx")

ourfish.ses.landings.1.egerman <- ourfish.egerman[location == "egerman",
                                                  buying_unit == "",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]
write.xlsx(ourfish.ses.landings.1.egerman, "C:/Users/IrlanAssidiq/Desktop/total Kg Ikan egerman not filter.xlsx")

ourfish.ses.landings.1.glm.egerman <- ourfish.ses.landings.1.egerman %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2.egerman <- ourfish.ses.landings.1.glm.egerman %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2.egerman)

# Label Landings trends
ourfish.ses.landings.1.glm3.egerman <- ourfish.ses.landings.1.glm2.egerman[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decrease")]

trend.label <- ourfish.ses.landings.1.glm3.egerman[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3.egerman)

ourfish.ses.landings.1.plot.egerman <- ggplot(ourfish.ses.landings.1.egerman, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y = 28,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
    labs(title = "Total landings (kg/month)",
         x = "Year",
              y = "Total catch (kg/month)") +
  theme.default.1

ourfish.ses.landings.1.glm2.egerman.indo <- ourfish.ses.landings.1.glm.egerman %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2.egerman.indo)
ourfish.ses.landings.1.glm3.egerman.indo <- ourfish.ses.landings.1.glm2.egerman.indo[,
                                                                                trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                   stat_test == "not significant" ~ "Stabil",
                                                                                                   TRUE ~ "Menurun")]

trend.label.indo <- ourfish.ses.landings.1.glm3.egerman.indo[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3.egerman.indo)
ourfish.ses.landings.1.plot.egerman.indo <- ggplot(ourfish.ses.landings.1.egerman, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label.indo,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y = 28,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Jumlah tangkapan ikan (dalam kg per bulan)",
       x = "Bulan",
       y = "Jumlah tangkapan (dalam kg per bulan)") +
  theme.default.1

#### CPUE Trends in Zona III ####
# Subset weight data by date, location for total landings
ourfish.egerman.cpue.1 <- ourfish.egerman[location=="egerman",
                                          .(sum_weight = sum(weight_kg)),
                                    by = .(date, location, fisher_id)][order(date)]

ourfish.egerman.cpue.1.glm <- ourfish.egerman.cpue.1 %>%
  nest(data = -location) %>%
  mutate(model = map(data, ~ glm(sum_weight ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.egerman.cpue.1.glm2 <- ourfish.egerman.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.egerman.cpue.1.glm2)

# Label Landings trends
ourfish.egerman.cpue.1.glm2.indo <- ourfish.egerman.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.egerman.cpue.1.glm2.indo)
ourfish.egerman.cpue.1.glm3.indo <- ourfish.egerman.cpue.1.glm2.indo[,
                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                        stat_test == "not significant" ~ "Stabil",
                                                                        TRUE ~ "Menurun")]

trend.label.indo <- ourfish.egerman.cpue.1.glm3.indo[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.egerman.cpue.1.glm3.indo)

ourfish.egerman.cpue.1.plot.indo <- ggplot(ourfish.egerman.cpue.1, aes(x = date, y = sum_weight)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label.indo,
            aes(x = as.Date("2022-12-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan sekali melaut (dalam kg per kapal per bulan)",
       x = "Bulan",
       y = "Tangkapan sekali melaut (kg per kapal per bulan)") +
  theme.default.1

#Indolabelling

ourfish.egerman.cpue.1.glm3.indo <- ourfish.egerman.cpue.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decrease")]

trend.label <- ourfish.egerman.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.egerman.cpue.1.glm3)

ourfish.egerman.cpue.1.plot <- ggplot(ourfish.egerman.cpue.1, aes(x = date, y = sum_weight)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-12-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "CPUE (catch in kg/boat/month)",
       x = "Year",
       y = "CPUE (kg/boat/month)") +
  theme.default.1

#===============================================================================
ourfish.ses.landings.1.muna <- ourfish.ses.Muna[lgu_name %in% "Muna",
                                                  .(landing= sum(weight_kg)),
                                                  by = .(date, location)][order(date)]
write.xlsx(ourfish.ses.landings.1.egerman, "C:/Users/IrlanAssidiq/Desktop/total Kg Ikan egerman not filter.xlsx")

ourfish.ses.landings.1.glm.egerman <- ourfish.ses.landings.1.egerman %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.1.glm2.egerman <- ourfish.ses.landings.1.glm.egerman %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2.egerman)

# Label Landings trends
ourfish.ses.landings.1.glm3.egerman <- ourfish.ses.landings.1.glm2.egerman[,
                                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                              stat_test == "not significant" ~ "Stable",
                                                                                              TRUE ~ "Decrease")]

trend.label <- ourfish.ses.landings.1.glm3.egerman[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3.egerman)

ourfish.ses.landings.1.plot.egerman <- ggplot(ourfish.ses.landings.1.egerman, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y = 28,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total landings (kg/month)",
       x = "Year",
       y = "Total catch (kg/month)") +
  theme.default.1

ourfish.ses.landings.1.glm2.egerman.indo <- ourfish.ses.landings.1.glm.egerman %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.1.glm2.egerman.indo)
ourfish.ses.landings.1.glm3.egerman.indo <- ourfish.ses.landings.1.glm2.egerman.indo[,
                                                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                        stat_test == "not significant" ~ "Stabil",
                                                                                                        TRUE ~ "Menurun")]

trend.label.indo <- ourfish.ses.landings.1.glm3.egerman.indo[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.1.glm3.egerman.indo)
ourfish.ses.landings.1.plot.egerman.indo <- ggplot(ourfish.ses.landings.1.egerman, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label.indo,
            aes(x = as.Date("2022-04-01", format("%Y-%m-%d")),
                y = 28,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Jumlah tangkapan ikan (dalam kg per bulan)",
       x = "Bulan",
       y = "Jumlah tangkapan (dalam kg per bulan)") +
  theme.default.1

#### CPUE Trends in Zona III ####
# Subset weight data by date, location for total landings
ourfish.egerman.cpue.1 <- ourfish.egerman[location=="egerman",
                                          .(sum_weight = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.egerman.cpue.1.glm <- ourfish.egerman.cpue.1 %>%
  nest(data = -location) %>%
  mutate(model = map(data, ~ glm(sum_weight ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.egerman.cpue.1.glm2 <- ourfish.egerman.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.egerman.cpue.1.glm2)

# Label Landings trends
ourfish.egerman.cpue.1.glm2.indo <- ourfish.egerman.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.egerman.cpue.1.glm2.indo)
ourfish.egerman.cpue.1.glm3.indo <- ourfish.egerman.cpue.1.glm2.indo[,
                                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                        stat_test == "not significant" ~ "Stabil",
                                                                                        TRUE ~ "Menurun")]

trend.label.indo <- ourfish.egerman.cpue.1.glm3.indo[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.egerman.cpue.1.glm3.indo)

ourfish.egerman.cpue.1.plot.indo <- ggplot(ourfish.egerman.cpue.1, aes(x = date, y = sum_weight)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label.indo,
            aes(x = as.Date("2022-12-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tangkapan sekali melaut (dalam kg per kapal per bulan)",
       x = "Bulan",
       y = "Tangkapan sekali melaut (kg per kapal per bulan)") +
  theme.default.1

#Indolabelling

ourfish.egerman.cpue.1.glm3.indo <- ourfish.egerman.cpue.1.glm2[,
                                                                trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                   stat_test == "not significant" ~ "Stable",
                                                                                   TRUE ~ "Decrease")]

trend.label <- ourfish.egerman.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.egerman.cpue.1.glm3)

ourfish.egerman.cpue.1.plot <- ggplot(ourfish.egerman.cpue.1, aes(x = date, y = sum_weight)) +
  geom_smooth(method = "loess", span = 0.5, colour = "#008542", fill = "#008542") +
  scale_fill_manual(values = c("#008542")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-12-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            color = "#008542",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "CPUE (catch in kg/boat/month)",
       x = "Year",
       y = "CPUE (kg/boat/month)") +
  theme.default.1

#Size structure=================================================================
library(lme4)
library(fishmethods)
library(TropFishR)
library(Hmisc)
library(dplyr)
library(tidyr)
ourfish.egerman$date2 <- format(as.Date(ourfish.egerman$date), "%Y-%m")
ourfish.egerman$length2 <- round(ourfish.egerman$length)
#sunu merah - Cephalopholis miniata #10 count===================================
egerman.sunumerah <- ourfish.egerman[species == "Cephalopholis miniata"]
sum(egerman.sunumerah$count)
#baronang - Siganus guttatus #209 count=========================================
#data selection
egerman.baronang <- ourfish.egerman[species=="Siganus guttatus",
                                    .(total = sum(count)),
                                    by = .(length2, date2)][order(length2)]
#pivoting
egerman.baronang.2 <- egerman.baronang %>% 
  pivot_wider(
    names_from = date2,
    values_from = total,
    values_fill = 0
  )
#create raw data -> uncount
egerman.baronang.3 <- egerman.baronang %>% 
  dplyr::select(c(length2, total)) %>% 
  uncount(total)

#size structure
#data arrangement
dates <- colnames(egerman.baronang.2)[-1]
dates <- as.Date(dates, "%Y_%d_%m")

length.series.data <- list(dates = dates,
                           midLengths = egerman.baronang.2$length2,
                           catch = as.matrix(egerman.baronang.2[,-1]))

class(egerman.baronang.2) <- "lfq"
# Plot to check size distribution
plot(egerman.baronang.2, Fname = "total", ylab = "Panjang Ikan (cm)")

#kerapu lumpur - Epinephelus malabaricus #0 count===============================
egerman.kerapulumpur <- ourfish.egerman[species=="Epinephelus malabaricus"]
