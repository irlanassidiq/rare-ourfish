##### ses Data #####

# Set working directory
setwd("C:/Users/IrlanAssidiq/OneDrive - Rare/Newsletter")
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


# Load OurFish raw data from Data World
ourfish.raw <- fread("https://query.data.world/s/ksnqjl2pvxfcfqtmlikqd2g53m3ggq?dws=00000", header=TRUE, stringsAsFactors=FALSE)

# Reformat buying unit into correct scientific naming format
ourfish.raw$buying_unit <- as.character(ourfish.raw$buying_unit)

substring(ourfish.raw$buying_unit, 2) <- tolower(substring(ourfish.raw$buying_unit, 2))

ourfish.raw$buying_unit <- as.factor(ourfish.raw$buying_unit)

write.xlsx(ourfish.raw, "C:/Users/IrlanAssidiq/OneDrive - Rare/Newsletter/ourfish_raw_18oct.xlsx")

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
##set colors
rare_primaries <- c("#005BBB", 
                    "#008542", 
                    "#5E6A71")

rare_secondaries <- c("#AA1948", 
                      "#00AFD8", 
                      "#F55713", 
                      "#7AB800",
                      "#EEAF00")

chart_colors <- c("#005BBB", 
                  "#F55713", 
                  "#5E6A71", 
                  "#00AFD8", 
                  "#EEAF00",
                  "#7AB800",
                  "#AA1948",
                  "#008542",
                  "#00AFA0",
                  "#AB7200")

#===============================================================================
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

ourfish.nmal <- ourfish.raw.nmal[fish.meta, on = .(buying_unit), nomatch = NULL]

ourfish.mal <- ourfish.raw.mal[fish.meta, on = .(buying_unit), nomatch = NULL]

write.xlsx(ourfish.ses,"C:/Users/IrlanAssidiq/OneDrive - Rare/MSMSY/ourfish catch SES-real.xlsx")

#target Kapuntori
ourfish.ses.buton <- ourfish.raw.ses[lgu_name %in% "Buton" &
                                       !buying_unit %in% exclude.buying.unit &
                                       !weight_kg == 0,
                                     .(production = sum(weight_kg)),
                                     by = .(year, month, lgu_name, ma_name)]

write.xlsx(ourfish.ses.buton,"C:/Users/IrlanAssidiq/OneDrive - Rare/ourfish catch SES Buton3.xlsx")

#### General Statistics Start ####

# Number of fish family recorded
uniqueN(ourfish.ses$lgu_name) #11
uniqueN(ourfish.ses$ma_name) #30
uniqueN(ourfish.ns$lgu_name) #7

uniqueN(ourfish.ses$family) #59
uniqueN(ourfish.ns$family) #29

uniqueN(ourfish.ses$fisher_id) #2462
uniqueN(ourfish.ses$buyer_id) #135
sum(ourfish.ses.clean$weight_kg) #805482.3 kg = 805.5 Ton
sum(ourfish.ses.clean$total_price_local) #Rp. 22.342.935.154 = Rp. 22 Miliar

uniqueN(ourfish.ses$id) #82473
uniqueN(ourfish.ns$id) #594



# Number of fish family recorded
uniqueN(ourfish.ses$buying_unit) #115
uniqueN(ourfish.ns$buying_unit) #42

sum(ourfish.ses$weight_kg)/1000 #59.6 #56.3
sum(ourfish.ns$weight_kg)/1000 #416.3729 #410.7
sum(ourfish.mal$weight_kg)/1000 #0.8848
sum(ourfish.nmal$weight_kg)/1000 #6.632075


# Number of transaction recorded
ourfish.mal.num.transaction <- ourfish.mal[,
                                           .N,
                                           by = .(date, fisher_id, buying_unit)][order(date)]

ourfish.mal.num.transaction.2 <- ourfish.mal.num.transaction[,
                                                             .(num_daily_transaction = .N),
                                                             by = .(date)][order(date)]
sum(ourfish.mal.num.transaction.2$num_daily_transaction)
#139

# Daily Number Transaction Trend
ourfish.mal.num.transaction.2.glm <- glm(ourfish.mal.num.transaction.2$num_daily_transaction ~ as.numeric(ourfish.mal.num.transaction.2$date))

summary(ourfish.mal.num.transaction.2.glm)


ourfish.mal.num.transaction.2.plot <- ggplot(ourfish.mal.num.transaction.2, aes(x = date, y = num_daily_transaction)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  #annotate("text",
   #        x = as.Date("2024-02-01", format("%Y-%m-%d")),
    #       y = 10,
     #      label = "Decrease",
      #     color = "black",
       #    size = 6,
        #   hjust = 0.1,
           #vjust = 5
  #) +
  labs(title = "Transaksi tercatat di Maluku",
       x = "",
       y = "Jumlah transaksi") +
  theme.default.1

# Number of trips
ourfish.ses.num.trips <- ourfish.ses[,
                                     .N,
                                     by = .(date, fisher_id)][order(date)]
ourfish.ses.num.ma.trips <- ourfish.ses[,
                                        .(daily_trip = .N),
                                        by = .(date, month, year, family, ma_name, fisher_id, weight_kg)][order(date)]

ourfish.ses.num.ma.trips.2 <- ourfish.ses.num.ma.trips[,
                                                       .(total_trip = sum(daily_trip),
                                                         sum_kg = sum(weight_kg)),
                                                       by = .(year, family, ma_name)][order(ma_name)]

write.xlsx(ourfish.ses.num.ma.trips.2, "C:/Users/IrlanAssidiq/Documents/Rare/ses_msmsy_march.xlsx")

ourfish.ses.num.trips.2 <- ourfish.ses.num.trips[,
                                                 .(num_daily_trips = .N),
                                                 by = .(date)][order(date)]
sum(ourfish.ses.num.trips.2$num_daily_trips) #18693
# Daily Number Trips Trend
ourfish.ses.num.trips.2.glm <- glm(ourfish.ses.num.trips.2$num_daily_trips ~ as.numeric(ourfish.ses.num.trips.2$date))

summary(ourfish.ses.num.trips.2.glm)

ourfish.ses.num.trips.2.plot <- ggplot(ourfish.ses.num.trips.2, aes(x = date, y = num_daily_trips)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  annotate("text",
           x = as.Date("2021-09-01", format("%Y-%m-%d")),
           y = 5,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Fishing Trips in South East Sulawesi",
       x = "Month",
       y = "Number of Fishing Trips") +
  theme.default.1

# Number of communities recorded
ourfish.ses.num.community <- uniqueN(ourfish.ses$community_name) #107

# Number of MA+R recorded
ourfish.mal.num.mar <- uniqueN(ourfish.mal$ma_name) #2
uniqueN(ourfish.mal$buyer_id) #8
uniqueN(ourfish.mal$fisher_id) #38

# Daily number of Reporting Fishers
ourfish.mal.num.fisher.monthly <- ourfish.mal[order(date)][,
                                                           mm := format(date, "%b")][,
                                                                                     yyyy := format(date, "%Y")][,
                                                                                                                 .(num_reporting_fisher_monthly = uniqueN(fisher_id)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.mal.num.fisher.2 <- ourfish.mal[,
                                        .(num_fisher = uniqueN(fisher_id)),
                                        by = .(date)][order(date)]

ourfish.mal.cumsum.fisher.1 <- ourfish.mal[,
                                           .(min_date = min(date)),
                                           by = .(fisher_id)][,
                                                              .(num_first_time_fisher = uniqueN(fisher_id)),
                                                              by = .(min_date)][order(min_date)][,
                                                                                                 cumsum_fisher := cumsum(num_first_time_fisher)]

# Daily number of reporting fishers trend
ourfish.mal.num.fisher.2.glm <- glm(num_fisher ~ as.numeric(date),
                                    data = ourfish.mal.num.fisher.2)

summary(ourfish.mal.num.fisher.2.glm)

# Plot - Number of Fisher Reporting Their Catch
coeff.cumsum.fisher <- max(ourfish.mal.cumsum.fisher.1$cumsum_fisher)/max(ourfish.mal.num.fisher.2$num_fisher)

ourfish.mal.num.fisher.plot <-
  ggplot() +
  geom_smooth(data = ourfish.mal.num.fisher.2, aes(x = date, y = num_fisher),
              method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.mal.cumsum.fisher.1,
            aes(x = min_date, y = cumsum_fisher/coeff.cumsum.fisher),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.fisher,
                                         name = "Pelaporan oleh nelayan secara kumulatif")) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2025-01-01")),
             color = "#5E6A71") +
 # annotate("text",
  #         x = as.Date("2024-09-10", format("%Y-%m-%d")),
   #        y = 20,
    #       label = "Stabil",
     #      color = "black",
      #     size = 6,
       #    hjust = 0.1,
        #   vjust = 5) +
  labs(title = "Pelaporan hasil tangkapan oleh Nelayan",
       x = "Bulan",
       y = "Jumlah nelayan yang melapor hasil tangkapan per hari"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Daily number of buyer recorded
ourfish.mal.num.buyer <- ourfish.mal[,
                                     length(unique(buyer_name)),
                                     by = .(date, buyer_name)][order(date)]

ourfish.mal.num.buyer.monthly <- ourfish.mal.num.buyer[, mm := format(date, "%b")][, yyyy := format(date, "%Y")][,
                                                                                                                 .(num_recording_buyer_monthly = uniqueN(buyer_name)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.mal.num.buyer.2 <- ourfish.mal[, .(num_buyer = uniqueN(buyer_name)),
                                       by = .(date)][order(date)]

ourfish.mal.cumsum.buyer.1 <- ourfish.mal[,
                                          .(min_date = min(date)),
                                          by = .(buyer_name)][order(min_date)][,
                                                                               .(num_first_time_buyer = uniqueN(buyer_name)),
                                                                               by = .(min_date)][,
                                                                                                 cumsum_buyer := cumsum(num_first_time_buyer)]

# Daily number of recording fish buyers trend
ourfish.mal.num.buyer.2.glm <- glm(ourfish.mal.num.buyer.2$num_buyer ~ as.numeric(ourfish.mal.num.buyer.2$date))

summary(ourfish.mal.num.buyer.2.glm)

# Plot number of recording buyer
coeff.cumsum.buyer <- max(ourfish.mal.cumsum.buyer.1$cumsum_buyer)/max(ourfish.mal.num.buyer.2$num_buyer)

ourfish.mal.num.buyer.plot <-
  ggplot(ourfish.mal.num.buyer.2, aes(x = date, y = num_buyer)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.mal.cumsum.buyer.1,
            aes(x = min_date, y = cumsum_buyer/coeff.cumsum.buyer),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.buyer,
                                         name = "Pencatatan oleh pembeli ikan secara kumulatif"
                     )) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2025-01-01")),
             color = "#5E6A71") +
 # annotate("text",
  #         x = as.Date("2024-09-10", format("%Y-%m-%d")),
   #        y = 5,
    #       label = "Stabil",
     #      color = "black",
      #     size = 6,
       #    hjust = 0.1,
        #   vjust = 5) +
  labs(title = "Pencatatan transaksi oleh pembeli ikan",
       x = "Bulan",
       y = "Jumlah pembeli ikan yang mencatat per hari"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Combine plot: fishers reporting and buyers recording
ourfish.mal.num.fishers.buyers.plot <-
  ggarrange(ourfish.mal.num.fisher.plot, ourfish.mal.num.buyer.plot,
            ncol = 2, nrow = 1)

#Indo data======================================================================
### Clean Data for whole nation (4 provinces)
library(EnvStats)
quantile(ourfish.all$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.all$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.all[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.all[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.all$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.all.outlier <- ourfish.all[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.all.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_all_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.all$weight_kg, 0.025)
# q3 <- quantile(ourfish.all$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.all[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.all[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.all.clean <- ourfish.all[-landings.data.outliers$Obs.Num, ]

sum(ourfish.all.clean$weight_kg)/1000 #575.459 Ton
sum(ourfish.all.clean$total_price_local) #15,789,007,869
sum(ourfish.all.clean$total_price_usd) #1,015,712

# Combine data from whole all with data by MA+R
ourfish.indo <- ourfish.all.clean
#lgu_id 15 = Buton
#lgu_id 3  = Buton Tengah
#lgu_id 16 = Buton Selatan
#lgu_id 4  = Buton Utara
butonraya <- c("Buton", 
               "Buton Tengah",
               "Buton Selatan",
               "Buton Utara")
setnames(ourfish.indo, "lgu_name", "location")

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.indo.landings.1 <- ourfish.indo[location %in% c("Buton") &
                                          date>= "2020-01-01",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]
sum(ourfish.indo.landings.1$sum_landings)

ourfish.indo.landings.1.glm <- ourfish.indo.landings.1 %>%
  nest(data= -location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.landings.1.glm2 <- ourfish.indo.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.landings.1.glm2)

# Label Landings trends
ourfish.indo.landings.1.glm3 <- ourfish.indo.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.indo.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.landings.1.glm3)

ourfish.indo.landings.1.plot <- ggplot(ourfish.indo.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 50,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tren Total volume tangkapan Ikan di Buton",
       caption = "tercatat di OurFish",
       x = " ",
       y = "Volume tangkapan ikan dalam Kg") +
  theme.default.1


#Buton Tengah===================================================================
ourfish.indo.landings.1 <- ourfish.indo[location %in% c("Buton Tengah") &
                                          date>= "2020-01-01",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, location)][order(date)]

ourfish.indo.landings.1.glm <- ourfish.indo.landings.1 %>%
  nest(data= -location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.landings.1.glm2 <- ourfish.indo.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.landings.1.glm2)

# Label Landings trends
ourfish.indo.landings.1.glm3 <- ourfish.indo.landings.1.glm2[,
                                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                stat_test == "not significant" ~ "Stabil",
                                                                                TRUE ~ "Menurun")]

trend.label <- ourfish.indo.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.landings.1.glm3)

ourfish.indo.landings.1.plot <- ggplot(ourfish.indo.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 80,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tren Total volume tangkapan Ikan di Buton Tengah",
       caption = "tercatat di OurFish",
       x = " ",
       y = "Volume tangkapan ikan dalam Kg") +
  theme.default.1

#Buton Selatan==================================================================
ourfish.indo.landings.1 <- ourfish.indo[location %in% c("Buton Selatan") &
                                          date>= "2020-01-01",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, location)][order(date)]

ourfish.indo.landings.1.glm <- ourfish.indo.landings.1 %>%
  nest(data= -location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.landings.1.glm2 <- ourfish.indo.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.landings.1.glm2)

# Label Landings trends
ourfish.indo.landings.1.glm3 <- ourfish.indo.landings.1.glm2[,
                                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                stat_test == "not significant" ~ "Stabil",
                                                                                TRUE ~ "Menurun")]

trend.label <- ourfish.indo.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.landings.1.glm3)

ourfish.indo.landings.1.plot <- ggplot(ourfish.indo.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 80,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tren Total volume tangkapan Ikan di Buton Selatan",
       caption = "tercatat di OurFish",
       x = " ",
       y = "Volume tangkapan ikan dalam Kg") +
  theme.default.1

#Buton Utara====================================================================
ourfish.indo.landings.1 <- ourfish.indo[location %in% c("Buton Utara") &
                                          date>= "2020-01-01",
                                        .(sum_landings = sum(weight_kg)),
                                        by = .(date, location)][order(date)]

ourfish.indo.landings.1.glm <- ourfish.indo.landings.1 %>%
  nest(data= -location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.landings.1.glm2 <- ourfish.indo.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.landings.1.glm2)

# Label Landings trends
ourfish.indo.landings.1.glm3 <- ourfish.indo.landings.1.glm2[,
                                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                stat_test == "not significant" ~ "Stabil",
                                                                                TRUE ~ "Menurun")]

trend.label <- ourfish.indo.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.landings.1.glm3)

ourfish.indo.landings.1.plot <- ggplot(ourfish.indo.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 80,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Tren Total volume tangkapan Ikan di Buton Utara",
       caption = "tercatat di OurFish",
       x = " ",
       y = "Volume tangkapan ikan dalam Kg") +
  theme.default.1
#===============================================================================
#### CPUE Trends in Indonesia ####
# Subset weight data by date, location for total landings
ourfish.indo.cpue.1 <- ourfish.indo[,
                                          .(sum_weight = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.indo.cpue.1.glm <- ourfish.indo.cpue.1 %>%
  nest(data = -location) %>%
  mutate(model = map(data, ~ glm(sum_weight ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.cpue.1.glm2 <- ourfish.indo.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.cpue.1.glm2)

# Label Landings trends
ourfish.indo.cpue.1.glm3 <- ourfish.indo.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decrease")]

trend.label <- ourfish.indo.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.cpue.1.glm3)

ourfish.indo.cpue.1.plot <- ggplot(ourfish.indo.cpue.1, aes(x = date, y = sum_weight)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 18,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Catch per unit effort (CPUE) trend in Indonesia",
       caption = "recorded in OurFish",
       x = "Year",
       y = "Catch per fishing trip (kg/trip)") +
  theme.default.1

# Subset weight data by date, location for total landings value
ourfish.indo.catch.value.1 <- ourfish.indo[,
                                                    .(sum_catch_value = sum(total_price_local/1000)),
                                                    by = .(date, location, fisher_id)][order(date)]

ourfish.indo.catch.value.1.glm <- ourfish.indo.catch.value.1 %>%
  nest(data=-location) %>%
  mutate(model = map(data, ~ glm(sum_catch_value ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.indo.catch.value.1.glm2 <- ourfish.indo.catch.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.indo.catch.value.1.glm2)

# Label Landings trends
ourfish.indo.catch.value.1.glm3 <- ourfish.indo.catch.value.1.glm2[,
                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                          stat_test == "not significant" ~ "Stable",
                                                                                          TRUE ~ "Decrease")]

trend.label.catch.value <- ourfish.indo.catch.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.indo.catch.value.1.glm3)

ourfish.indo.catch.value.1.plot <- ggplot(ourfish.indo.catch.value.1, aes(x = date, y = sum_catch_value)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "1 year") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2022-01-01", format("%Y-%m-%d")),
                y = 400,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Value per unit effort trend in Indonesia (in IDR)",
       caption = "recorded in OurFish",
       x = "Year",
       y = "Value generated per fishing trip (IDR/trip) x1,000") +
  theme.default.1

#COmbinde landings and landing value (USD)

trans.coeff.value <- max(ourfish.indo.catch.value.1$sum_catch_value)/max(ourfish.indo.cpue.1$sum_weight)

# Use the plot below to get a sense of varying catch value between managed access
ggplot() +
  geom_smooth(data = ourfish.indo.cpue.1,
              aes(x = date, y = sum_weight),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.indo.catch.value.1,
              aes(x = date, y = sum_catch_value/trans.coeff.value),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff.value,
                                         name = "IDR generated per fishing trip (IDR/trip) x1,000")) +
  scale_x_date(labels = date_format("%Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "1 year") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2025-01-01")),
             color = "#5E6A71") +
  #facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2021-06-30", format("%Y-%m-%d")),
                y = 18,
                label = trend),
            hjust = 0.1,
            vjust = 0.5,
            color = "black",
            size = 12,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.catch.value,
            aes(x = as.Date("2024-06-30", format("%Y-%m-%d")),
                y = 9,
                label = trend),
            hjust = 0.7,
            vjust = 0.5,
            color = "#008542",
            size = 12,
            inherit.aes = FALSE) +
  labs(title = "Catch and value generated per unit effort in Indonesia",
       caption = "recorded in OurFish",
       x = "Year",
       y = "Catch per fishing trip (kg/trip)") +
  theme.default.5

#==============SES==============================================================
# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.ses$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.ses$weight_kg, 0.99) #Upper outlier limit

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
write.xlsx(ourfish.ses.clean,"C:/Users/IrlanAssidiq/OneDrive - Rare/Newsletter/ourfish_raw_mar.xlsx")


sum(ourfish.ses.clean$weight_kg)/1000 #809,0656
sum(ourfish.ses.clean$total_price_local) #22.438.594.689

ses.pivot <- ourfish.ses.clean[snu_name %in% "South East Sulawesi",
                               .(landing_kg = sum(weight_kg)),
                               by = .(family)] [order(desc(landing_kg))]
ses.pivot.fisher <- ourfish.ses.clean[location %in% "South East Sulawesi",
                                      .(trip = uniqueN(id)),
                                      by = .(fisher_id)] [order(trip)]

ses.pivot <- mutate(ses.pivot, percent = round(ses.pivot$landing_kg/sum(ses.pivot$landing_kg)*100, 1))


ses.pivot2 <- ses.pivot[ses.pivot$landing_kg %in% tail(sort(ses.pivot$landing_kg),5),]
pie(ses.pivot2$landing_kg, ses.pivot2$family)

ses.pivot3 <- ses.pivot2
ses.pivot3$family <- as.character(ses.pivot3$family)

ses.pivot3 <- rbind(ses.pivot3, data.frame(landing_kg = sum(ses.pivot[!ses.pivot$landing_kg %in% 
                                                                  tail(sort(ses.pivot$landing_kg),5),"landing_kg"]), family = "Others"), fill=T)
ses.pivot3$percent <- round(ses.pivot3$landing_kg/sum(ses.pivot3$landing_kg)*100,1)

ses.pivot3$family <- factor(ses.pivot3$family, levels = ses.pivot3$family)
ggplot(ses.pivot3, aes(x=1, y=percent, fill = family)) +
  geom_col()+
  coord_polar(theta = "y")+
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)+
  ggtitle("Percentage of fish landings (kg) in South East Sulawesi")+
  theme_void(base_size = 20)+
  scale_fill_brewer(name = NULL, palette = "Set1")

buyer.ses <- uniqueN(ourfish.ses$buyer_id) #35
buyer.ses <- ourfish.ses[,
                         .(sum_gender = uniqueN(buyer_id)),
                         by = .(buyer_gender)] #1: male, 2: female; #male = 93, female = 42
fisher.ses <- uniqueN(ourfish.ses$fisher_id) #252
landing.ses <- sum(ourfish.ses$weight_kg) #38955kg, 38.9 Ton 
cpue.ses1 <- ourfish.ses[,
                        .(cpue = sum(weight_kg)),
                        by = .(date, month, fisher_id )]
cpue.ses2 <- cpue.ses1[,
                        .(cpue.monthly = mean(cpue)),
                        by = .(month)]
cpue.ses3 <- ourfish.ses[,
                         .(kg = sum(weight_kg),
                           n = uniqueN(id)),
                         by = .(fisher_id)]
cpue.ses4 <- cpue.ses3[,
                       .(cpue = kg/n),
                       by = .(fisher_id)]
mean(cpue.ses4$cpue)

buyer.ns <- uniqueN(ourfish.ns$buyer_id) #17
buyer.ns <- ourfish.ns[,
                       .(sum_gender = uniqueN(buyer_id)),
                       by = .(buyer_gender)] #male = 6, female = 11
fisher.ns <- uniqueN(ourfish.ns$fisher_id) #131
landing.ns <- sum(ourfish.ns$weight_kg) #7668kg, 7.6 Ton
cpue.ns1 <- ourfish.ns[,
                       .(cpue = sum(weight_kg)),
                       by = .(date, month, fisher_id )]
cpue.ns2 <- cpue.ns1[,
                     .(cpue.monthly = mean(cpue)),
                     by = .(month)]
cpue.ns3 <- ourfish.ns[,
                       .(kg = sum(weight_kg),
                         n = uniqueN(id)),
                       by = .(fisher_id)]
cpue.ns4 <- cpue.ns3[,
                     .(cpue = kg/n),
                     by = .(fisher_id)]
mean(cpue.ns4$cpue)


buyer.nmal <- uniqueN(ourfish.nmal$buyer_id) #4
buyer.nmal <- ourfish.nmal[,
                         .(sum_gender = uniqueN(buyer_id)),
                         by = .(buyer_gender)] #male = 4
fisher.nmal <- uniqueN(ourfish.nmal$fisher_id) #67
landing.nmal <- sum(ourfish.nmal$weight_kg) #3365kg, 3.3 Ton
cpue.nmal1 <- ourfish.nmal[,
                           .(cpue = sum(weight_kg)),
                           by = .(date, month, fisher_id )]
cpue.nmal2 <- cpue.nmal1[,
                         .(cpue.monthly = mean(cpue)),
                         by = .(month)]
cpue.nmal3 <- ourfish.nmal[,
                           .(kg = sum(weight_kg),
                             n = uniqueN(id)),
                           by = .(fisher_id)]
cpue.nmal4 <- cpue.nmal3[,
                         .(cpue = kg/n),
                         by = .(fisher_id)]
mean(cpue.nmal4$cpue)


buyer.mal <- uniqueN(ourfish.mal$buyer_id) #0

# Detecting outliers in weight data Kapuntori
library(EnvStats)
quantile(ourfish.ses.kapuntori$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.ses.kapuntori$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.ses.kapuntori[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.ses.kapuntori[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.ses.kapuntori$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.ses.kapuntori.outlier <- ourfish.ses.kapuntori[landings.data.outliers$Obs.Num, ]

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
ourfish.ses.kapuntori.clean <- ourfish.ses.kapuntori[-landings.data.outliers$Obs.Num, ]

sum(ourfish.ses.kapuntori.clean$weight_kg)/1000 #10,543
sum(ourfish.ses.kapuntori.clean$total_price_local) #382.564.477

kapuntori.pivot <- ourfish.ses.kapuntori.clean[snu_name %in% "South East Sulawesi",
                               .(transaction = .N),
                               by = .(buying_unit, label)] [order(desc(transaction))]

kapuntori.pivot <- mutate(kapuntori.pivot, percent = round(kapuntori.pivot$transaction/sum(kapuntori.pivot$transaction)*100, 1))


kapuntori.pivot2 <- kapuntori.pivot[kapuntori.pivot$transaction %in% tail(sort(kapuntori.pivot$transaction),5),]
pie(kapuntori.pivot2$transaction, kapuntori.pivot2$label)

kapuntori.pivot3 <- kapuntori.pivot2
kapuntori.pivot3$buying_unit <- as.character(kapuntori.pivot3$buying_unit)
kapuntori.pivot3$label <- as.character(kapuntori.pivot3$label)

kapuntori.pivot3 <- rbind(kapuntori.pivot3, data.frame(transaction = sum(kapuntori.pivot[!kapuntori.pivot$transaction %in% 
                                                                        tail(sort(kapuntori.pivot$transaction),5),"transaction"]), buying_unit = "Ikan lain", label = "Ikan lain"), fill=T)
kapuntori.pivot3$percent <- round(kapuntori.pivot3$transaction/sum(kapuntori.pivot3$transaction)*100,1)

kapuntori.pivot3$buying_unit <- factor(kapuntori.pivot3$buying_unit, levels = kapuntori.pivot3$buying_unit)
kapuntori.pivot3$label <- factor(kapuntori.pivot3$label, levels = kapuntori.pivot3$label)

ggplot(kapuntori.pivot3, aes(x=1, y=percent, fill = label)) +
  geom_col()+
  coord_polar(theta = "y")+
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)+
  ggtitle("Persentase jenis ikan tertangkap di Kawasan PAAP Kapontori")+
  theme_void(base_size = 20)+
  scale_fill_brewer(palette = "Blues")

  
# Detecting outliers in weight data
quantile(ourfish.ns$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.ns$weight_kg, 0.99) #Upper outlier limit

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
ourfish.ns.clean <- ourfish.ns[-landings.data.outliers$Obs.Num, ]

sum(ourfish.ns.clean$weight_kg)/1000 #12.85184 #7.57
sum(ourfish.ns.clean$total_price_local) #346.380.104 #253.941.806

ns.pivot <- ourfish.ns.clean[snu_name %in% "North Sulawesi",
                               .(landing_kg = sum(weight_kg)),
                               by = .(family)] [order(desc(landing_kg))]

ns.pivot <- mutate(ns.pivot, percent = round(ns.pivot$landing_kg/sum(ns.pivot$landing_kg)*100, 1))


ns.pivot2 <- ns.pivot[ns.pivot$landing_kg %in% tail(sort(ns.pivot$landing_kg),5),]
pie(ns.pivot2$landing_kg, ns.pivot2$family)

ns.pivot3 <- ns.pivot2
ns.pivot3$family <- as.character(ns.pivot3$family)
ns.pivot3 <- rbind(ns.pivot3, data.frame(landing_kg = sum(ns.pivot[!ns.pivot$landing_kg %in% 
                                                                        tail(sort(ns.pivot$landing_kg),5),"landing_kg"]), family = "Others"), fill=T)
ns.pivot3$percent <- round(ns.pivot3$landing_kg/sum(ns.pivot3$landing_kg)*100,1)

ns.pivot3$family <- factor(ns.pivot3$family, levels = ns.pivot3$family)
ggplot(ns.pivot3, aes(x=1, y=percent, fill = family)) +
  geom_col()+
  coord_polar(theta = "y")+
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)+
  ggtitle("Percentage of fish landings (kg) in North Sulawesi")+
  theme_void(base_size = 20)+
  scale_fill_brewer(name = NULL, palette = "Set1")

# Detecting outliers in weight data
quantile(ourfish.mal$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.mal$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.mal[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.mal[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.mal$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.mal.outlier <- ourfish.mal[landings.data.outliers$Obs.Num, ]

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
ourfish.mal.clean <- ourfish.mal[-landings.data.outliers$Obs.Num, ]

sum(ourfish.mal.clean$weight_kg)/1000 #0.7791
sum(ourfish.mal.clean$total_price_local) #19.143.148

mal.pivot <- ourfish.mal.clean[snu_name %in% "Maluku",
                             .(landing_kg = sum(weight_kg)),
                             by = .(family)] [order(desc(landing_kg))]

mal.pivot <- mutate(mal.pivot, percent = round(mal.pivot$landing_kg/sum(mal.pivot$landing_kg)*100, 1))


mal.pivot2 <- mal.pivot[mal.pivot$landing_kg %in% tail(sort(mal.pivot$landing_kg),5),]
pie(mal.pivot2$landing_kg, mal.pivot2$family)

mal.pivot3 <- mal.pivot2
mal.pivot3$family <- as.character(mal.pivot3$family)
mal.pivot3 <- rbind(mal.pivot3, data.frame(landing_kg = sum(mal.pivot[!mal.pivot$landing_kg %in% 
                                                                     tail(sort(mal.pivot$landing_kg),5),"landing_kg"]), family = "Others"), fill=T)
mal.pivot3$percent <- round(mal.pivot3$landing_kg/sum(mal.pivot3$landing_kg)*100,1)

mal.pivot3$family <- factor(mal.pivot3$family, levels = mal.pivot3$family)
ggplot(mal.pivot3, aes(x=1, y=percent, fill = family)) +
  geom_col()+
  coord_polar(theta = "y")+
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)+
  ggtitle("Percentage of fish landings (kg) in Maluku")+
  theme_void(base_size = 20)+
  scale_fill_brewer(name = NULL, palette = "Set1")

# Detecting outliers in weight data
quantile(ourfish.nmal$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.nmal$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.nmal[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.nmal[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.nmal$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.nmal.outlier <- ourfish.nmal[landings.data.outliers$Obs.Num, ]

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
ourfish.nmal.clean <- ourfish.nmal[-landings.data.outliers$Obs.Num, ]

sum(ourfish.nmal.clean$weight_kg)/1000 #5.493775
sum(ourfish.nmal.clean$total_price_local) #141.578.737

nmal.pivot <- ourfish.nmal.clean[snu_name %in% "North Maluku",
                               .(landing_kg = sum(weight_kg)),
                               by = .(family)] [order(desc(landing_kg))]

nmal.pivot <- mutate(nmal.pivot, percent = round(nmal.pivot$landing_kg/sum(nmal.pivot$landing_kg)*100, 1))


nmal.pivot2 <- nmal.pivot[nmal.pivot$landing_kg %in% tail(sort(nmal.pivot$landing_kg),5),]
pie(nmal.pivot2$landing_kg, nmal.pivot2$family)

nmal.pivot3 <- nmal.pivot2
nmal.pivot3$family <- as.character(nmal.pivot3$family)
nmal.pivot3 <- rbind(nmal.pivot3, data.frame(landing_kg = sum(nmal.pivot[!nmal.pivot$landing_kg %in% 
                                                                        tail(sort(nmal.pivot$landing_kg),5),"landing_kg"]), family = "Others"), fill=T)
nmal.pivot3$percent <- round(nmal.pivot3$landing_kg/sum(nmal.pivot3$landing_kg)*100,1)

nmal.pivot3$family <- factor(nmal.pivot3$family, levels = nmal.pivot3$family)
nmal.pivot2$family <- factor(nmal.pivot2$family, levels = nmal.pivot2$family)
ggplot(nmal.pivot2, aes(x=1, y=percent, fill = family)) +
  geom_col()+
  coord_polar(theta = "y")+
  geom_text(aes(label = paste(percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)+
  ggtitle("Percentage of fish landings (kg) in North Maluku")+
  theme_void(base_size = 20)+
  scale_fill_brewer(name = NULL, palette = "Set1")


# Number of transaction recorded
ourfish.ses.num.transaction <- ourfish.ses[,
                                           .N,
                                           by = .(date, fisher_id, buying_unit)][order(date)]

ourfish.ses.num.transaction.2 <- ourfish.ses.num.transaction[,
                                                             .(num_daily_transaction = .N),
                                                             by = .(date)][order(date)]
sum(ourfish.ses.num.transaction.2$num_daily_transaction)

# Daily Number Transaction Trend
ourfish.ses.num.transaction.2.glm <- glm(ourfish.ses.num.transaction.2$num_daily_transaction ~ as.numeric(ourfish.ses.num.transaction.2$date))

summary(ourfish.ses.num.transaction.2.glm)


ourfish.ses.num.transaction.2.plot <- ggplot(ourfish.ses.num.transaction.2, aes(x = date, y = num_daily_transaction)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 10,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Transactions in South East Sulawesi",
       x = "Month",
       y = "Number of Transaction") +
  theme.default.1

# Number of trips
ourfish.ses.num.trips <- ourfish.ses[,
                                     .N,
                                     by = .(date, fisher_id)][order(date)]

ourfish.ses.num.trips.2 <- ourfish.ses.num.trips[,
                                                 .(num_daily_trips = .N),
                                                 by = .(date)][order(date)]
sum(ourfish.ses.num.trips.2$num_daily_trips) #1425
# Daily Number Trips Trend
ourfish.ses.num.trips.2.glm <- glm(ourfish.ses.num.trips.2$num_daily_trips ~ as.numeric(ourfish.ses.num.trips.2$date))

summary(ourfish.ses.num.trips.2.glm)

ourfish.ses.num.trips.2.plot <- ggplot(ourfish.ses.num.trips.2, aes(x = date, y = num_daily_trips)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Fishing Trips in South East Sulawesi",
       x = "Month",
       y = "Number of Fishing Trips") +
  theme.default.1

# Number of communities recorded
ourfish.ses.num.community <- uniqueN(ourfish.ses$community_name) #68

# Number of MA+R recorded
ourfish.ses.num.mar <- uniqueN(ourfish.ses$ma_name) #18
uniqueN(ourfish.ses$buyer_id) #49
uniqueN(ourfish.ses$fisher_id) #460

# Daily number of Reporting Fishers
ourfish.ses.num.fisher.monthly <- ourfish.ses[order(date)][,
                                                           mm := format(date, "%b")][,
                                                                                     yyyy := format(date, "%Y")][,
                                                                                                                 .(num_reporting_fisher_monthly = uniqueN(fisher_id)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.ses.num.fisher.2 <- ourfish.ses[,
                                        .(num_fisher = uniqueN(fisher_id)),
                                        by = .(date)][order(date)]

ourfish.ses.cumsum.fisher.1 <- ourfish.ses[,
                                           .(min_date = min(date)),
                                           by = .(fisher_id)][,
                                                              .(num_first_time_fisher = uniqueN(fisher_id)),
                                                              by = .(min_date)][order(min_date)][,
                                                                                                 cumsum_fisher := cumsum(num_first_time_fisher)]

# Daily number of reporting fishers trend
ourfish.ses.num.fisher.2.glm <- glm(num_fisher ~ as.numeric(date),
                                    data = ourfish.ses.num.fisher.2)

summary(ourfish.ses.num.fisher.2.glm)

# Plot - Number of Fisher Reporting Their Catch
coeff.cumsum.fisher <- max(ourfish.ses.cumsum.fisher.1$cumsum_fisher)/max(ourfish.ses.num.fisher.2$num_fisher)

ourfish.ses.num.fisher.plot <-
  ggplot() +
  geom_smooth(data = ourfish.ses.num.fisher.2, aes(x = date, y = num_fisher),
              method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.ses.cumsum.fisher.1,
            aes(x = min_date, y = cumsum_fisher/coeff.cumsum.fisher),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.fisher,
                                         name = "Cumulative number of fishers reporting")) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 20,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Fishers Reporting",
       x = "Month",
       y = "Total number of fishers reporting per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Load registered fishers data
#registered.fishers <- fread("https://query.data.world/s/gdje6vzfgu3mpvblwuihs6komiqojo",
#header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("provinsi", "kabupaten", "ma_name", "fisher_name", "fisher_id", "gender"))

#ourfish.ses.fisher.gender <- ourfish.ses.num.fishers[registered.fishers,
#on = .(fisher_id), nomatch = NULL]

#ourfish.ses.num.reg.fisher.mar <- registered.fishers[provinsi == "South East Sulawesi" & !ma_name %in% c("Pasi Kolaga", "Sagori", "Wabula", "Maginti", "Wabula", "Tiworo Utara"),
#  .(num_reg_fisher = length(unique(fisher_id))),
# by = .(ma_name)]

# Number of female fishers
#ourfish.ses.num.female.fishers <- ourfish.ses.fisher.gender[gender == "f"]

# Daily number of buyer recorded
ourfish.ses.num.buyer <- ourfish.ses[,
                                     length(unique(buyer_name)),
                                     by = .(date, buyer_name)][order(date)]

ourfish.ses.num.buyer.monthly <- ourfish.ses.num.buyer[, mm := format(date, "%b")][, yyyy := format(date, "%Y")][,
                                                                                                                 .(num_recording_buyer_monthly = uniqueN(buyer_name)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.ses.num.buyer.2 <- ourfish.ses[, .(num_buyer = uniqueN(buyer_name)),
                                       by = .(date)][order(date)]

ourfish.ses.cumsum.buyer.1 <- ourfish.ses[,
                                          .(min_date = min(date)),
                                          by = .(buyer_name)][order(min_date)][,
                                                                               .(num_first_time_buyer = uniqueN(buyer_name)),
                                                                               by = .(min_date)][,
                                                                                                 cumsum_buyer := cumsum(num_first_time_buyer)]

# Daily number of recording fish buyers trend
ourfish.ses.num.buyer.2.glm <- glm(ourfish.ses.num.buyer.2$num_buyer ~ as.numeric(ourfish.ses.num.buyer.2$date))

summary(ourfish.ses.num.buyer.2.glm)

# Plot number of recording buyer
coeff.cumsum.buyer <- max(ourfish.ses.cumsum.buyer.1$cumsum_buyer)/max(ourfish.ses.num.buyer.2$num_buyer)

ourfish.ses.num.buyer.plot <-
  ggplot(ourfish.ses.num.buyer.2, aes(x = date, y = num_buyer)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.ses.cumsum.buyer.1,
            aes(x = min_date, y = cumsum_buyer/coeff.cumsum.buyer),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.buyer,
                                         name = "Cumulative number of buyers recording"
                     )) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Buyers Recording",
       x = "Month",
       y = "Total number of fish buyers recording per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Combine plot: fishers reporting and buyers recording
ourfish.ses.num.fishers.buyers.plot <-
  ggarrange(ourfish.ses.num.fisher.plot, ourfish.ses.num.buyer.plot,
            ncol = 2, nrow = 1)

# Table for number of reporting fishers & recording buyers by MA
# Data from 01 Aug 2021 to 30 Jul 2023
ourfish.ses.fisher.buyer.1 <- ourfish.ses[,
                                          .(num_fisher = length(unique(fisher_id)),
                                            num_buyer = length(unique(buyer_id))),
                                          by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Data from 01 Jun 2019 to 30 Jul 2023
ourfish.ses.fisher.buyer.2 <- ourfish.ses.historical[,
                                                     .(num_fisher_hist = length(unique(fisher_id)),
                                                       num_buyer_hist = length(unique(buyer_id))),
                                                     by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Combine both filtered and historical data
ourfish.ses.fisher.buyer.3 <- ourfish.ses.fisher.buyer.1[ourfish.ses.fisher.buyer.2, on = .(ma_name_2), nomatch = NULL]

fwrite(ourfish.ses.fisher.buyer.3, "C:/Users/IrlanAssidiq/OneDrive - Rare/Walton 2021-2024/plot/Table 1 - Number of fishers & fish buyers by MA.csv")

#### Trends in catch and catch value by location Start ####

# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.ses$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.ses$weight_kg, 0.99) #Upper outlier limit

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

ourfish.ses.clean <- ourfish.ses.clean[which(ourfish.ses.clean$count > 0 & ourfish.ses.clean$count < 10000)]
sum(ourfish.ses.clean$weight_kg)#36979.8
sum(ourfish.ses.clean$total_price_local) #10.955.316.898
sum(ourfish.ses.clean$total_price_usd) #55782.17
uniqueN(ourfish.ses.clean$buyer_name)#81
uniqueN(ourfish.ses.clean$community_name) #107
uniqueN(ourfish.ses.clean$fisher_id) #436
## Box Plot for annual comparison

# Make data for two years period
ourfish.ses.clean.boxplot.1 <- ourfish.ses.clean[,
                                                 year_period := as.factor("4 Years Period")]

# Make data for Year 1
ourfish.ses.clean.boxplot.2 <- ourfish.ses.clean[date >= start.date & date <= "2022-10-01"][,
                                                                                            year_period := as.factor("Year 1")]

# Make data for Year 2
ourfish.ses.clean.boxplot.3 <- ourfish.ses.clean[date >= "2022-08-01" & date <= end.date][,
                                                                                          year_period := as.factor("Year 2")]

# Combine data of two years period with year 1 & 2 data
ourfish.ses.clean.boxplot.4 <- rbind(ourfish.ses.clean.boxplot.1, ourfish.ses.clean.boxplot.2, ourfish.ses.clean.boxplot.3)

# Annual CPUE & Catch Value using box plot
ourfish.ses.clean.boxplot.5 <- ourfish.ses.clean.boxplot.4[,
                                                           .(cpue_kg_trip = sum(weight_kg),
                                                             vpue_idr_trip = sum(total_price_local)),
                                                           by = .(year_period, date, fisher_id)][order(year_period)]

# Quartile 1,2,3 of annual CPUE & Catch Value
ourfish.ses.clean.boxplot.5[,
                            .(IQR_cpue = quantile(cpue_kg_trip, probs = c(0.25, 0.5, 0.75)),
                              IQR_vpue = quantile(vpue_idr_trip, probs = c(0.25, 0.5, 0.75))),
                            by = .(year_period)]

# Independent Sample T-Test Between CPUE Year 1 and 2
t.test(data = ourfish.ses.clean.boxplot.5[!year_period == "2 Years Period"],
       cpue_kg_trip ~ year_period,
       var.equal = TRUE)

# Independent Sample T-Test Between Catch Value Year 1 and 2
t.test(data = ourfish.ses.clean.boxplot.5[!year_period == "2 Years Period"],
       vpue_idr_trip ~ year_period,
       var.equal = TRUE)

# Box Plot for Annual CPUE
boxplot.annual.cpue <- 
  ggplot(data = ourfish.ses.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = cpue_kg_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), oob = squish) +
  labs(x = "Year Period",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Box Plot for Annual Catch Value
boxplot.annual.catch.value <- 
  ggplot(data = ourfish.ses.clean.boxplot.5,
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
ourfish.ses.clean.boxplot.6 <- ourfish.ses.clean.boxplot.5[,
                                                           .(n_trip = .N,
                                                             mean_cpue = mean(cpue_kg_trip),
                                                             se_cpue = sd(cpue_kg_trip) / sqrt(.N),
                                                             mean_vpue = mean(vpue_idr_trip),
                                                             se_vpue = sd(vpue_idr_trip) / sqrt(.N)),
                                                           by = .(year_period)][order(year_period)]

fwrite(ourfish.ses.clean.boxplot.6,
       "C:/Users/IrlanAssidiq/Documents/Rare/Rare OurFish/Trial WFF/Table 2 - Annual CPUE & VPUE.csv")

## CPUE and Catch Value Trends

# Rename level1_name and ma_name into the same column name to be combined
ourfish.ses.combine.1 <- ourfish.ses.clean[ , !c("ma_name_2")]
setnames(ourfish.ses.combine.1, "snu_name", "location")

ourfish.ses.combine.2 <- ourfish.ses.clean[ , !c("snu_name")]
setnames(ourfish.ses.combine.2, "ma_name_2", "location")

# Combine data from whole ses with data by MA+R
ourfish.ses.prov.ma <- ourfish.ses.clean
setnames(ourfish.ses.prov.ma, "snu_name", "location")
ourfish.ses.prov.ma <- rbind(ourfish.ses.combine.1, ourfish.ses.combine.2)

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.landings.1 <- ourfish.ses.prov.ma[location == "South East Sulawesi",
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
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 200,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total pendaratan ikan di Sulawesi Tenggara",
       x = "Tahun",
       y = "Total berat ikan (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.ses.cpue.1 <- ourfish.ses.prov.ma[location == "South East Sulawesi",
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
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "CPUE (kg/trip) di Sulawesi Tenggara",
       x = "Tahun",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Subset weight data by date, location for total landings value
ourfish.ses.landings.value.1 <- ourfish.ses.prov.ma[location == "South East Sulawesi",
                                              .(sum_landings_value = sum(total_price_local/100000)),
                                              by = .(date, location, fisher_id)][order(date)]

ourfish.ses.landings.value.1.glm <- ourfish.ses.landings.value.1 %>%
  nest(data=-location) %>%
  mutate(model = map(data, ~ glm(sum_landings_value ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.ses.landings.value.1.glm2 <- ourfish.ses.landings.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.ses.landings.value.1.glm2)

# Label Landings trends
ourfish.ses.landings.value.1.glm3 <- ourfish.ses.landings.value.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Meningkat")]

trend.label.landings.value <- ourfish.ses.landings.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.ses.landings.value.1.glm3)

ourfish.ses.landings.value.1.plot <- ggplot(ourfish.ses.landings.value.1, aes(x = date, y = sum_landings_value)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = "12 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 200,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings (IDR) in South East Sulawesi",
       x = "Month",
       y = "Total Landings (IDR)") +
  theme.default.1

#COmbinde landings and landing value (USD)

trans.coeff.value <- max(ourfish.ses.landings.value.1$sum_landings_value)/max(ourfish.ses.cpue.1$sum_landings)

# Use the plot below to get a sense of varying catch value between managed access
ggplot() +
  geom_smooth(data = ourfish.ses.cpue.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.ses.landings.value.1,
              aes(x = date, y = sum_landings_value/trans.coeff.value),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff.value,
                                         name = "Nilai tangkapan (Rp. x100.000)")) +
  scale_x_date(labels = date_format("%Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "12 month") +
  #geom_vline(xintercept = as.Date(c("2022-01-01")),
   #          color = "#5E6A71") +
  #geom_vline(xintercept = as.Date(c("2023-01-01")),
   #          color = "#5E6A71") +
  #geom_vline(xintercept = as.Date(c("2024-01-01")),
   #          color = "#5E6A71") +
  #facet_wrap(~ location, ncol = 5, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-04-01", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.1,
            vjust = 0.5,
            color = "black",
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.landings.value,
            aes(x = as.Date("2023-06-30", format("%Y-%m-%d")),
                y = 10,
                label = trend),
            hjust = 0.7,
            vjust = 0.5,
            color = "#008542",
            size = 4,
            inherit.aes = FALSE) +
  labs(title = "CPUE (kg/trip) and nilai tangkapan (Rp)",
       x = "Tahun",
       y = "CPUE (kg/trip)") +
  theme.default.5

ourfish.ses.fishing.pressure.plot <- ggarrange(ourfish.ses.num.trips.2.plot, ourfish.ses.num.transaction.2.plot, ourfish.ses.landings.1.plot,
                                               align = "v",
                                               ncol = 1, nrow = 3)

#### Trends in catch and catch value at all location Start ####

# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.all$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.all$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.all[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.all[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.all$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.all.outlier <- ourfish.all[landings.data.outliers$Obs.Num, ]

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
ourfish.all.clean <- ourfish.all[-landings.data.outliers$Obs.Num, ]

ourfish.all.clean.2 <- ourfish.all.clean[which(ourfish.all.clean$count > 0 & ourfish.all.clean$count < 10000)]
sum(ourfish.all.clean$weight_kg)#154.669,3
sum(ourfish.all.clean$total_price_local) #3.899.598.442
sum(ourfish.all.clean$total_price_usd) #250.962,4
uniqueN(ourfish.all.clean$buyer_name)#88
uniqueN(ourfish.all.clean$community_name) #124
uniqueN(ourfish.all$ma_name) #34
uniqueN(ourfish.all$buying_unit)
uniqueN(ourfish.all.clean$fisher_id) #1135
## Box Plot for annual comparison

# Make data for two years period
ourfish.all.clean.boxplot.1 <- ourfish.all.clean[,
                                                 year_period := as.factor("4 Years Period")]

# Make data for Year 1
ourfish.all.clean.boxplot.2 <- ourfish.all.clean[date >= start.date & date <= "2022-10-01"][,
                                                                                            year_period := as.factor("Year 1")]

# Make data for Year 2
ourfish.all.clean.boxplot.3 <- ourfish.all.clean[date >= "2022-08-01" & date <= end.date][,
                                                                                          year_period := as.factor("Year 2")]

# Combine data of two years period with year 1 & 2 data
ourfish.all.clean.boxplot.4 <- rbind(ourfish.all.clean.boxplot.1, ourfish.all.clean.boxplot.2, ourfish.all.clean.boxplot.3)

# Annual CPUE & Catch Value using box plot
ourfish.all.clean.boxplot.5 <- ourfish.all.clean.boxplot.4[,
                                                           .(cpue_kg_trip = sum(weight_kg),
                                                             vpue_idr_trip = sum(total_price_local)),
                                                           by = .(year_period, date, fisher_id)][order(year_period)]

# Quartile 1,2,3 of annual CPUE & Catch Value
ourfish.all.clean.boxplot.5[,
                            .(IQR_cpue = quantile(cpue_kg_trip, probs = c(0.25, 0.5, 0.75)),
                              IQR_vpue = quantile(vpue_idr_trip, probs = c(0.25, 0.5, 0.75))),
                            by = .(year_period)]

# Independent Sample T-Test Between CPUE Year 1 and 2
t.test(data = ourfish.all.clean.boxplot.5[!year_period == "2 Years Period"],
       cpue_kg_trip ~ year_period,
       var.equal = TRUE)

# Independent Sample T-Test Between Catch Value Year 1 and 2
t.test(data = ourfish.all.clean.boxplot.5[!year_period == "2 Years Period"],
       vpue_idr_trip ~ year_period,
       var.equal = TRUE)

# Box Plot for Annual CPUE
boxplot.annual.cpue <- 
  ggplot(data = ourfish.all.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = cpue_kg_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), oob = squish) +
  labs(x = "Year Period",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Box Plot for Annual Catch Value
boxplot.annual.catch.value <- 
  ggplot(data = ourfish.all.clean.boxplot.5,
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
ourfish.all.clean.boxplot.6 <- ourfish.all.clean.boxplot.5[,
                                                           .(n_trip = .N,
                                                             mean_cpue = mean(cpue_kg_trip),
                                                             se_cpue = sd(cpue_kg_trip) / sqrt(.N),
                                                             mean_vpue = mean(vpue_idr_trip),
                                                             se_vpue = sd(vpue_idr_trip) / sqrt(.N)),
                                                           by = .(year_period)][order(year_period)]

fwrite(ourfish.all.clean.boxplot.6,
       "C:/Users/IrlanAssidiq/Documents/Rare/Rare OurFish/Trial WFF/Table 2 - Annual CPUE & VPUE.csv")

## CPUE and Catch Value Trends

# Rename level1_name and ma_name into the same column name to be combined
ourfish.all.combine.1 <- ourfish.all.clean[ , !c("ma_name_2")]
setnames(ourfish.all.combine.1, "snu_name", "location")

ourfish.all.combine.2 <- ourfish.all.clean[ , !c("snu_name")]
setnames(ourfish.all.combine.2, "ma_name_2", "location")

# Combine data from whole all with data by MA+R
ourfish.all.prov.ma <- ourfish.all.clean
setnames(ourfish.all.prov.ma, "snu_name", "location")
ourfish.all.prov.ma <- rbind(ourfish.all.combine.1, ourfish.all.combine.2)

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.all.landings.1 <- ourfish.all.prov.ma[,
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.all.landings.1.glm <- ourfish.all.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.all.landings.1.glm2 <- ourfish.all.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.all.landings.1.glm2)

# Label Landings trends
ourfish.all.landings.1.glm3 <- ourfish.all.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decrease")]

trend.label <- ourfish.all.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.all.landings.1.glm3)

ourfish.all.landings.1.plot <- ggplot(ourfish.all.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 100,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings trend in 4 Provinces",
       x = "Month",
       y = "Total Landings (kg)") +
  theme.default.1

#### CPUE Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.all.cpue.1 <- ourfish.all.prov.ma[,
                                          .(sum_landings = sum(weight_kg)),
                                          by = .(date, location, fisher_id)][order(date)]

ourfish.all.cpue.1.glm <- ourfish.all.cpue.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.all.cpue.1.glm2 <- ourfish.all.cpue.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.all.cpue.1.glm2)

# Label Landings trends
ourfish.all.cpue.1.glm3 <- ourfish.all.cpue.1.glm2[,
                                                   trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                      stat_test == "not significant" ~ "Stable",
                                                                      TRUE ~ "Decrease")]

trend.label <- ourfish.all.cpue.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.all.cpue.1.glm3)

ourfish.all.cpue.1.plot <- ggplot(ourfish.all.cpue.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "CPUE (kg/trip) in South East Sulawesi",
       x = "Month",
       y = "Catch per trip (kg/trip)") +
  theme.default.1

# Subset weight data by date, location for total landings value
ourfish.all.landings.value.1 <- ourfish.all.prov.ma[,
                                                    .(sum_landings_value = sum(total_price_local/1000)),
                                                    by = .(date, location, fisher_id)][order(date)]

ourfish.all.landings.value.1.glm <- ourfish.all.landings.value.1 %>%
  nest(data=-location) %>%
  mutate(model = map(data, ~ glm(sum_landings_value ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.all.landings.value.1.glm2 <- ourfish.all.landings.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.all.landings.value.1.glm2)

# Label Landings trends
ourfish.all.landings.value.1.glm3 <- ourfish.all.landings.value.1.glm2[,
                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                          stat_test == "not significant" ~ "Stable",
                                                                                          TRUE ~ "Decrease")]

trend.label.landings.value <- ourfish.all.landings.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.all.landings.value.1.glm3)

ourfish.all.landings.value.1.plot <- ggplot(ourfish.all.landings.value.1, aes(x = date, y = sum_landings_value)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 200,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings (IDR) in South East Sulawesi",
       x = "Month",
       y = "Total Landings (IDR)") +
  theme.default.1

#COmbinde landings and landing value (USD)

trans.coeff.value <- max(ourfish.all.landings.value.1$sum_landings_value)/max(ourfish.all.landings.1$sum_landings)

# Use the plot below to get a sense of varying catch value between managed access
ggplot() +
  geom_smooth(data = ourfish.all.cpue.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.all.landings.value.1,
              aes(x = date, y = sum_landings_value/trans.coeff.value),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff.value,
                                         name = "Value generated (IDR)")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  facet_wrap(~ location, ncol = 1, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            vjust = 0.5,
            color = "black",
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.landings.value,
            aes(x = as.Date("2024-06-30", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.7,
            vjust = 0.5,
            color = "#008542",
            size = 4,
            inherit.aes = FALSE) +
  labs(title = "Catch per trip (kg) and value generated (IDR x 1000)",
       x = "Month",
       y = "Catch (kg) per trip") +
  theme.default.5



###Kapontori--------------------------------------------------------------------
####Number of transaction recorded----------------------------------------------
ourfish.kapuntori.num.transaction <- ourfish.ses[ma_name %in% "Kapuntori",
                                           .N,
                                           by = .(date, fisher_id, buying_unit)][order(date)]

ourfish.kapuntori.num.transaction.2 <- ourfish.kapuntori.num.transaction[,
                                                             .(num_daily_transaction = .N),
                                                             by = .(date)][order(date)]

sum(ourfish.kapuntori.num.transaction.2$num_daily_transaction)#3102

# Daily Number Transaction Trend
ourfish.kapuntori.num.transaction.2.glm <- glm(ourfish.kapuntori.num.transaction.2$num_daily_transaction ~ as.numeric(ourfish.kapuntori.num.transaction.2$date))

summary(ourfish.kapuntori.num.transaction.2.glm)


ourfish.kapuntori.num.transaction.2.plot <- ggplot(ourfish.kapuntori.num.transaction.2, aes(x = date, y = num_daily_transaction)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  annotate("text",
           x = as.Date("2020-10-10", format("%Y-%m-%d")),
           y = 6,
           label = "Menurun",
           color = "black",
           size = 6,
           hjust = 0.1
  ) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total transaksi = 3102",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Jumlah transaksi ikan, harian di kawasan PAAP Kapuntori",
       x = "Bulan",
       y = "Jumlah transaksi harian") +
  theme.default.1

# Number of trips
ourfish.kapuntori.num.trips <- ourfish.ses[ma_name %in% "Kapuntori",
                                     .N,
                                     by = .(date, fisher_id)][order(date)]

ourfish.kapuntori.num.trips.2 <- ourfish.kapuntori.num.trips[,
                                                 .(num_daily_trips = .N),
                                                 by = .(date)][order(date)]
sum(ourfish.kapuntori.num.trips.2$num_daily_trips) #2171
# Daily Number Trips Trend
ourfish.kapuntori.num.trips.2.glm <- glm(ourfish.kapuntori.num.trips.2$num_daily_trips ~ as.numeric(ourfish.kapuntori.num.trips.2$date))

summary(ourfish.kapuntori.num.trips.2.glm)

ourfish.kapuntori.num.trips.2.plot <- ggplot(ourfish.kapuntori.num.trips.2, aes(x = date, y = num_daily_trips)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  annotate("text",
           x = as.Date("2020-10-10", format("%Y-%m-%d")),
           y = 4,
           label = "Menurun",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total hari melaut = 2171",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Jumlah nelayan melaut, harian di kawasan PAAP Kapuntori",
       x = "Bulan",
       y = "Jumlah nelayan melaut") +
  theme.default.1

# Number of communities recorded
ourfish.kapuntori.num.community <- uniqueN(ourfish.ses.kapuntori$community_name) #6

# Number of MA+R recorded
ourfish.kapuntori.num.mar <- uniqueN(ourfish.ses.kapuntori$ma_name) #18
uniqueN(ourfish.ses.kapuntori$buyer_id) #7
uniqueN(ourfish.ses.kapuntori$fisher_id) #178
sum(ourfish.ses.kapuntori.clean$weight_kg/1000) #10.5343

## CPUE and Catch Value Trends

# Rename level1_name and ma_name into the same column name to be combined
setnames(ourfish.ses.kapuntori.clean, "ma_name", "location")

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.kapuntori.landings.1 <- ourfish.ses.kapuntori.clean[location == "Kapuntori",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.kapuntori.landings.1.glm <- ourfish.kapuntori.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.landings.1.glm2 <- ourfish.kapuntori.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.landings.1.glm2)

# Label Landings trends
ourfish.kapuntori.landings.1.glm3 <- ourfish.kapuntori.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stabil",
                                                                              TRUE ~ "Menurun")]

trend.label <- ourfish.kapuntori.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.landings.1.glm3)

ourfish.kapuntori.landings.1.plot <- ggplot(ourfish.kapuntori.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 20,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total berat = 10,5 Ton",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total berat ikan, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

#Katamba------------------------------------------------------------------------
#Total landings-----------------------------------------------------------------
ourfish.kapuntori.katamba.landings.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Lethrinus lentjan",
                                                                    .(sum_landings = sum(weight_kg)),
                                                                    by = .(date, location)][order(date)]
sum(ourfish.kapuntori.katamba.landings.1$sum_landings) #1193.7
ourfish.kapuntori.katamba.landings.1.glm <- ourfish.kapuntori.katamba.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.katamba.landings.1.glm2 <- ourfish.kapuntori.katamba.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.katamba.landings.1.glm2)

# Label Landings trends
ourfish.kapuntori.katamba.landings.1.glm3 <- ourfish.kapuntori.katamba.landings.1.glm2[,
                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                          stat_test == "not significant" ~ "Stabil",
                                                                                          TRUE ~ "Menurun")]

trend.label <- ourfish.kapuntori.katamba.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.katamba.landings.1.glm3)

ourfish.kapuntori.katamba.landings.1.plot <- ggplot(ourfish.kapuntori.katamba.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 5,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total berat = 1,2 Ton",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total berat Ikan Katamba, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

#Total landings value-----------------------------------------------------------
ourfish.kapuntori.katamba.value.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Lethrinus lentjan",
                                                                    .(sum_price = sum(total_price_local)),
                                                                    by = .(date, location)][order(date)]
sum(ourfish.kapuntori.katamba.value.1$sum_price) #37.184.511
ourfish.kapuntori.katamba.value.1.glm <- ourfish.kapuntori.katamba.value.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_price ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.katamba.value.1.glm2 <- ourfish.kapuntori.katamba.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.katamba.value.1.glm2)

# Label Landings trends
ourfish.kapuntori.katamba.value.1.glm3 <- ourfish.kapuntori.katamba.value.1.glm2[,
                                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                          stat_test == "not significant" ~ "Stabil",
                                                                                                          TRUE ~ "Menurun")]

trend.label.value <- ourfish.kapuntori.katamba.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.katamba.value.1.glm3)

ourfish.kapuntori.katamba.value.1.plot <- ggplot(ourfish.kapuntori.katamba.value.1, aes(x = date, y = sum_price)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 50000,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total nilai = Rp. 37.184.511",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total nilai Ikan Katamba, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

# Combine landings with Catch Value into a 2 y-axes plot------------------------

# Calculate coefficients for second y axis
ourfish.kapuntori.katamba.value.1 <- ourfish.kapuntori.katamba.value.1[,
                                                                   sum_price_100000 := sum_price/100000]

trans.coeff <- max(ourfish.kapuntori.katamba.value.1$sum_price_100000)/max(ourfish.kapuntori.katamba.landings.1$sum_landings)

# CPUE and Catch Value Trends Plot in Southeast Sulawesi with free y axis
# Use the plot below to get a sense of varying catch value between managed access
kapuntori.katamba.catch.value <- ggplot() +
  geom_smooth(data = ourfish.kapuntori.katamba.landings.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.kapuntori.katamba.value.1,
              aes(x = date, y = sum_price_100000/trans.coeff),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff,
                                         name = "Nilai tangkapan (Rp) x100.000")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "6 month"
  ) +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 1,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.value,
            aes(x = as.Date("2024-04-10", format("%Y-%m-%d")),
                y = 1,
                label = trend),
            hjust = 0.7,
            color = "#008542",
            size = 6,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2020-04-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total 1,2 Ton ",
           color = "black",
           size = 4,
           hjust = 0.1
  ) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total Rp. 37.184.511 ",
           color = "#008542",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Perubahan berat tangkapan Ikan Katamba dan nilai Rupiahnya ",
       x = "Bulan",
       y = "Berat tangkapan (kg)") +
  theme.default.5

#Mata Besar---------------------------------------------------------------------
#Total landings-----------------------------------------------------------------
ourfish.kapuntori.matabesar.landings.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Priacanthus macracanthus",
                                                                    .(sum_landings = sum(weight_kg)),
                                                                    by = .(date, location)][order(date)]
sum(ourfish.kapuntori.matabesar.landings.1$sum_landings) #621.2
ourfish.kapuntori.matabesar.landings.1.glm <- ourfish.kapuntori.matabesar.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.matabesar.landings.1.glm2 <- ourfish.kapuntori.matabesar.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.matabesar.landings.1.glm2)

# Label Landings trends
ourfish.kapuntori.matabesar.landings.1.glm3 <- ourfish.kapuntori.matabesar.landings.1.glm2[,
                                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                          stat_test == "not significant" ~ "Stabil",
                                                                                                          TRUE ~ "Menurun")]

trend.label <- ourfish.kapuntori.matabesar.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.matabesar.landings.1.glm3)

ourfish.kapuntori.matabesar.landings.1.plot <- ggplot(ourfish.kapuntori.matabesar.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 5,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total berat = 621 Kg",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total berat Ikan Mata Besar, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

#Total landings value-----------------------------------------------------------
ourfish.kapuntori.matabesar.value.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Priacanthus macracanthus",
                                                                 .(sum_price = sum(total_price_local)),
                                                                 by = .(date, location)][order(date)]
sum(ourfish.kapuntori.matabesar.value.1$sum_price) #12.526.508
ourfish.kapuntori.matabesar.value.1.glm <- ourfish.kapuntori.matabesar.value.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_price ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.matabesar.value.1.glm2 <- ourfish.kapuntori.matabesar.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.matabesar.value.1.glm2)

# Label Landings trends
ourfish.kapuntori.matabesar.value.1.glm3 <- ourfish.kapuntori.matabesar.value.1.glm2[,
                                                                                 trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                    stat_test == "not significant" ~ "Stabil",
                                                                                                    TRUE ~ "Menurun")]

trend.label.value <- ourfish.kapuntori.matabesar.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.matabesar.value.1.glm3)

ourfish.kapuntori.matabesar.value.1.plot <- ggplot(ourfish.kapuntori.matabesar.value.1, aes(x = date, y = sum_price)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 50000,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total nilai = Rp. 12.526.508",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total nilai Ikan Mata Besar, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

# Combine landings with Catch Value into a 2 y-axes plot------------------------

# Calculate coefficients for second y axis
ourfish.kapuntori.matabesar.value.1 <- ourfish.kapuntori.matabesar.value.1[,
                                                                       sum_price_100000 := sum_price/100000]

trans.coeff <- max(ourfish.kapuntori.matabesar.value.1$sum_price_100000)/max(ourfish.kapuntori.matabesar.landings.1$sum_landings)

# CPUE and Catch Value Trends Plot in Southeast Sulawesi with free y axis
# Use the plot below to get a sense of varying catch value between managed access
kapuntori.matabesar.catch.value <- ggplot() +
  geom_smooth(data = ourfish.kapuntori.matabesar.landings.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.kapuntori.matabesar.value.1,
              aes(x = date, y = sum_price_100000/trans.coeff),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff,
                                         name = "Nilai tangkapan (Rp) x100.000")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "6 month"
  ) +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 1,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.value,
            aes(x = as.Date("2024-04-10", format("%Y-%m-%d")),
                y = 19,
                label = trend),
            hjust = 0.7,
            color = "#008542",
            size = 6,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2020-04-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total 621 Kg ",
           color = "black",
           size = 4,
           hjust = 0.1
  ) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 18,
           label = "Total Rp. 12.526.508",
           color = "#008542",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Perubahan berat tangkapan Ikan Mata Besar dan nilai Rupiahnya ",
       x = "Bulan",
       y = "Berat tangkapan (kg)") +
  theme.default.5

#Mada---------------------------------------------------------------------------
#Total landings-----------------------------------------------------------------
ourfish.kapuntori.mada.landings.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Lutjanus vitta",
                                                                    .(sum_landings = sum(weight_kg)),
                                                                    by = .(date, location)][order(date)]
sum(ourfish.kapuntori.mada.landings.1$sum_landings) #145.3
ourfish.kapuntori.mada.landings.1.glm <- ourfish.kapuntori.mada.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.mada.landings.1.glm2 <- ourfish.kapuntori.mada.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.mada.landings.1.glm2)

# Label Landings trends
ourfish.kapuntori.mada.landings.1.glm3 <- ourfish.kapuntori.mada.landings.1.glm2[,
                                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                          stat_test == "not significant" ~ "Stabil",
                                                                                                          TRUE ~ "Menurun")]

trend.label <- ourfish.kapuntori.mada.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.mada.landings.1.glm3)

ourfish.kapuntori.mada.landings.1.plot <- ggplot(ourfish.kapuntori.mada.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 5,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total berat = 145 kg",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total berat Ikan Mada, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

#Total landings value-----------------------------------------------------------
ourfish.kapuntori.mada.value.1 <- ourfish.ses.kapuntori.clean[buying_unit %in% "Lutjanus vitta",
                                                                 .(sum_price = sum(total_price_local)),
                                                                 by = .(date, location)][order(date)]
sum(ourfish.kapuntori.mada.value.1$sum_price) #4.541.997
ourfish.kapuntori.mada.value.1.glm <- ourfish.kapuntori.mada.value.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_price ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.mada.value.1.glm2 <- ourfish.kapuntori.mada.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.mada.value.1.glm2)

# Label Landings trends
ourfish.kapuntori.mada.value.1.glm3 <- ourfish.kapuntori.mada.value.1.glm2[,
                                                                                 trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                                    stat_test == "not significant" ~ "Stabil",
                                                                                                    TRUE ~ "Menurun")]

trend.label.value <- ourfish.kapuntori.mada.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.kapuntori.mada.value.1.glm3)

ourfish.kapuntori.mada.value.1.plot <- ggplot(ourfish.kapuntori.mada.value.1, aes(x = date, y = sum_price)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 50000,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.5,
           label = "Total nilai = Rp. 4.541.997",
           color = "red4",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Total nilai Ikan Mada, harian yang didaratkan di kawasan PAAP Kapontori",
       x = "Bulan",
       y = "Berat (Kg)") +
  theme.default.1

# Combine landings with Catch Value into a 2 y-axes plot------------------------

# Calculate coefficients for second y axis
ourfish.kapuntori.mada.value.1 <- ourfish.kapuntori.mada.value.1[,
                                                                       sum_price_100000 := sum_price/100000]

trans.coeff <- max(ourfish.kapuntori.mada.value.1$sum_price_100000)/max(ourfish.kapuntori.mada.landings.1$sum_landings)

# CPUE and Catch Value Trends Plot in Southeast Sulawesi with free y axis
# Use the plot below to get a sense of varying catch value between managed access
kapuntori.mada.catch.value <- ggplot() +
  geom_smooth(data = ourfish.kapuntori.mada.landings.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.kapuntori.mada.value.1,
              aes(x = date, y = sum_price_100000/trans.coeff),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff,
                                         name = "Nilai tangkapan (Rp) x100.000")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "6 month"
  ) +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 0.5,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.value,
            aes(x = as.Date("2024-04-10", format("%Y-%m-%d")),
                y = 0.5,
                label = trend),
            hjust = 0.7,
            color = "#008542",
            size = 6,
            inherit.aes = FALSE) +
  annotate("text",
           x = as.Date("2020-04-10", format("%Y-%m-%d")),
           y = 0.1,
           label = "Total 145 kg ",
           color = "black",
           size = 4,
           hjust = 0.1
  ) +
  annotate("text",
           x = as.Date("2024-03-10", format("%Y-%m-%d")),
           y = 0.1,
           label = "Total Rp. 4.541.997 ",
           color = "#008542",
           size = 4,
           hjust = 0.1
  ) +
  labs(title = "Perubahan berat tangkapan Ikan Mada dan nilai Rupiahnya ",
       x = "Bulan",
       y = "Berat tangkapan (kg)") +
  theme.default.5


##### CPUE Trends in Southeast Sulawesi by MA ####
# Subset weight data by date, location, fisher_id
ourfish.kapuntori.weight.1 <- ourfish.ses.kapuntori.clean[location == "Kapuntori",
                                            .(sum_weight_kg = sum(weight_kg)),
                                            by = .(date, location, fisher_id)][order(date)]

# GLM (linear regression) of date ~ weight by location
ourfish.kapuntori.weight.glm <- ourfish.kapuntori.weight.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_weight_kg ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.weight.glm.2 <- ourfish.kapuntori.weight.glm %>%
  filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>%
  select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.weight.glm.2)

# Label CPUE trends
ourfish.kapuntori.weight.glm.3 <- ourfish.kapuntori.weight.glm.2[,
                                                     trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat",
                                                                        stat_test == "not significant" ~ "Stabil",
                                                                        TRUE ~ "Menurun")]

trend.label <- ourfish.kapuntori.weight.glm.3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

#### Alternative in labeling trends ####
# Put trend label as facet label
# trend.label.2 <- trend.label[, location_trend := as.factor(paste0(location, " ", "(",trend,")"))]
# 
# trend.label.2$location_trend <- plyr::revalue(trend.label.2$location_trend, c("Maginti (Stable)" = "Maginti",
#                                                                               "Tiworo Utara (Decrease)" = "Tiworo Utara"))
# 
# ourfish.ses.weight.1 <- ourfish.ses.weight.1[trend.label.2, on = .(location), nomatch = NULL]

#### CPUE Trends Plot in Southeast Sulawesi with free y axis ####
# Use the plot below to get a sense of varying CPUE between managed access
kapuntori.cpue <-
  ggplot(ourfish.kapuntori.weight.1,
         aes(x = date, y = sum_weight_kg)) +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 3,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            inherit.aes = FALSE) +
  labs(title = "Perubahan hasil tangkapan tiap melaut di kawasan PAAP Kapuntori",
       x = "Bulan",
       y = "Berat tangkapan (kg/melaut)") +
  theme.default.2

#### Catch Value Trends in Southeast Sulawesi by MA ####
# Subset total price data by date, location, fisher_id
ourfish.kapuntori.catch.value.1 <- ourfish.ses.kapuntori.clean[location == "Kapuntori",
                                                 .(sum_total_price = sum(total_price_local)),
                                                 by = .(date, location, fisher_id)][order(date)]

# Linear regression of date ~ total price by location
ourfish.kapuntori.catch.value.glm <- ourfish.kapuntori.catch.value.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_total_price ~ as.numeric(date), data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.kapuntori.catch.value.glm.2 <- ourfish.kapuntori.catch.value.glm %>%
  filter(term == "as.numeric(date)") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>%
  select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.kapuntori.catch.value.glm.2)

ourfish.kapuntori.catch.value.glm.3 <- ourfish.kapuntori.catch.value.glm.2[,
                                                               trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Meningkat",
                                                                                  stat_test == "not significant" ~ "Stabil",
                                                                                  TRUE ~ "Menurun")]

# Label Catch Value trends
trend.label.catch.value <- ourfish.kapuntori.catch.value.glm.3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

#### Catch Value Trends Plot in Southeast Sulawesi with free y axis ####
# Use the plot below to get a sense of varying catch value between managed access
kapuntori.catch.value <-
  ggplot(ourfish.kapuntori.catch.value.1,
         aes(x = date, y = sum_total_price)) +
  geom_smooth(method = "loess", span = 0.5, se = TRUE, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "6 month") +
  geom_text(data = trend.label.catch.value,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 50000,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 4,
            inherit.aes = FALSE) +
  labs(title = "Catch Value Trends in Southeast Sulawesi",
       x = "Month",
       y = "Catch Value (IDR/trip)") +
  theme.default.2

#### CPUE and Catch Value Trends Plot by MA in Southeast Sulawesi ####
# Combine CPUE with Catch Value into a 2 y-axes plot

# Calculate coefficients for second y axis
ourfish.kapuntori.catch.value.1 <- ourfish.kapuntori.catch.value.1[,
                                                       sum_total_price_100000 := sum_total_price/100000]

trans.coeff <- max(ourfish.kapuntori.catch.value.1$sum_total_price_100000)/max(ourfish.kapuntori.weight.1$sum_weight_kg)

# CPUE and Catch Value Trends Plot in Southeast Sulawesi with free y axis
# Use the plot below to get a sense of varying catch value between managed access
kapuntori.cpue.catch.value <- ggplot() +
  geom_smooth(data = ourfish.kapuntori.weight.1,
              aes(x = date, y = sum_weight_kg),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.kapuntori.catch.value.1,
              aes(x = date, y = sum_total_price_100000/trans.coeff),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(2,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff,
                                         name = "Nilai hasil tangkapan (Rp/melaut) x100.000")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "6 month"
               ) +
  geom_text(data = trend.label,
            aes(x = as.Date("2020-10-10", format("%Y-%m-%d")),
                y = 3,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.catch.value,
            aes(x = as.Date("2024-04-10", format("%Y-%m-%d")),
                y = 3,
                label = trend),
            hjust = 0.7,
            color = "#008542",
            size = 6,
            inherit.aes = FALSE) +
  labs(title = "Perubahan hasil tangkapan ikan dasar per melaut dan nilai rupiahnya di PAAP Kapontori",
       x = "Bulan",
       y = "Berat tangkapan (kg/melaut)") +
  theme.default.5

#Maluku
# Subset Data
# Use general filters
# Remove observation with 0 in weight_kg
ourfish.raw.mal <- ourfish.raw[#date >= start.date & date <= end.date &
                                 snu_name %in% "Maluku" &
                                 !buying_unit %in% exclude.buying.unit &
                                 !weight_kg == 0][order(date)]

#NMAL
#Subset
start.date.nmal <- "2024-05-25"
ourfish.raw.nmal <- ourfish.raw[date >= start.date.nmal & date <= end.date &
  snu_name %in% "North Sulawesi" &
    !buying_unit %in% exclude.buying.unit &
    !weight_kg == 0][order(date)]



# Subset data
# Use general filters except date filters, i.e. historical data
ourfish.raw.nmal.2 <- ourfish.raw[date >= "2019-06-01" & date <= end.date &
                                    snu_name %in% "North Sulawesi" &
                                    !buying_unit %in% exclude.buying.unit &
                                    !weight_kg == 0][order(date)]

# Load fish meta data
fish.meta <- fread("C:/Users/IrlanAssidiq/Dropbox/OURFISH - FASTFIELD/meta_data/Reef Fish Meta Data_2022.07.05.csv",
                   header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("buying_unit", "family"))

# Join OurFish raw nmal data with fish meta data
ourfish.nmal <- ourfish.raw.nmal[fish.meta, on = .(buying_unit), nomatch = NULL]

# Join OurFish Raw nmal 2 data with fish meta data
ourfish.nmal.historical <- ourfish.raw.nmal.2[fish.meta, on = .(buying_unit), nomatch = NULL]

#### General Statistics Start ####

# Number of fish family recorded
uniqueN(ourfish.nmal$lgu_name) #2
uniqueN(ourfish.nmal$family) #7
uniqueN(ourfish.nmal$id) #217


# Number of fish family recorded
uniqueN(ourfish.nmal$buying_unit) #19

# Number of transaction recorded
ourfish.nmal.num.transaction <- ourfish.nmal[,
                                           .N,
                                           by = .(date, fisher_id, buying_unit)][order(date)]

ourfish.nmal.num.transaction.2 <- ourfish.nmal.num.transaction[,
                                                             .(num_daily_transaction = .N),
                                                             by = .(date)][order(date)]
sum(ourfish.nmal.num.transaction.2$num_daily_transaction)

# Daily Number Transaction Trend
ourfish.nmal.num.transaction.2.glm <- glm(ourfish.nmal.num.transaction.2$num_daily_transaction ~ as.numeric(ourfish.nmal.num.transaction.2$date))

summary(ourfish.nmal.num.transaction.2.glm)


ourfish.nmal.num.transaction.2.plot <- ggplot(ourfish.nmal.num.transaction.2, aes(x = date, y = num_daily_transaction)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 10,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Transactions in South East Sulawesi",
       x = "Month",
       y = "Number of Transaction") +
  theme.default.1

# Number of trips
ourfish.nmal.num.trips <- ourfish.nmal[,
                                     .N,
                                     by = .(date, fisher_id)][order(date)]

ourfish.nmal.num.trips.2 <- ourfish.nmal.num.trips[,
                                                 .(num_daily_trips = .N),
                                                 by = .(date)][order(date)]
sum(ourfish.nmal.num.trips.2$num_daily_trips) #136
# Daily Number Trips Trend
ourfish.nmal.num.trips.2.glm <- glm(ourfish.nmal.num.trips.2$num_daily_trips ~ as.numeric(ourfish.nmal.num.trips.2$date))

summary(ourfish.nmal.num.trips.2.glm)

ourfish.nmal.num.trips.2.plot <- ggplot(ourfish.nmal.num.trips.2, aes(x = date, y = num_daily_trips)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Fishing Trips in South East Sulawesi",
       x = "Month",
       y = "Number of Fishing Trips") +
  theme.default.1

# Number of communities recorded
ourfish.nmal.num.community <- uniqueN(ourfish.nmal$community_name) #2

# Number of MA+R recorded
ourfish.nmal.num.mar <- uniqueN(ourfish.nmal$ma_name) #1
uniqueN(ourfish.nmal$buyer_id) #3
uniqueN(ourfish.nmal$fisher_id) #58

# Daily number of Reporting Fishers
ourfish.nmal.num.fisher.monthly <- ourfish.nmal[order(date)][,
                                                           mm := format(date, "%b")][,
                                                                                     yyyy := format(date, "%Y")][,
                                                                                                                 .(num_reporting_fisher_monthly = uniqueN(fisher_id)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.nmal.num.fisher.2 <- ourfish.nmal[,
                                        .(num_fisher = uniqueN(fisher_id)),
                                        by = .(date)][order(date)]

ourfish.nmal.cumsum.fisher.1 <- ourfish.nmal[,
                                           .(min_date = min(date)),
                                           by = .(fisher_id)][,
                                                              .(num_first_time_fisher = uniqueN(fisher_id)),
                                                              by = .(min_date)][order(min_date)][,
                                                                                                 cumsum_fisher := cumsum(num_first_time_fisher)]

# Daily number of reporting fishers trend
ourfish.nmal.num.fisher.2.glm <- glm(num_fisher ~ as.numeric(date),
                                    data = ourfish.nmal.num.fisher.2)

summary(ourfish.nmal.num.fisher.2.glm)

# Plot - Number of Fisher Reporting Their Catch
coeff.cumsum.fisher <- max(ourfish.nmal.cumsum.fisher.1$cumsum_fisher)/max(ourfish.nmal.num.fisher.2$num_fisher)

ourfish.nmal.num.fisher.plot <-
  ggplot() +
  geom_smooth(data = ourfish.nmal.num.fisher.2, aes(x = date, y = num_fisher),
              method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.nmal.cumsum.fisher.1,
            aes(x = min_date, y = cumsum_fisher/coeff.cumsum.fisher),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.fisher,
                                         name = "Cumulative number of fishers reporting")) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 20,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Fishers Reporting",
       x = "Month",
       y = "Total number of fishers reporting per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Load registered fishers data
#registered.fishers <- fread("https://query.data.world/s/gdje6vzfgu3mpvblwuihs6komiqojo",
#header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("provinsi", "kabupaten", "ma_name", "fisher_name", "fisher_id", "gender"))

#ourfish.nmal.fisher.gender <- ourfish.nmal.num.fishers[registered.fishers,
#on = .(fisher_id), nomatch = NULL]

#ourfish.nmal.num.reg.fisher.mar <- registered.fishers[provinsi == "South East Sulawesi" & !ma_name %in% c("Pasi Kolaga", "Sagori", "Wabula", "Maginti", "Wabula", "Tiworo Utara"),
#  .(num_reg_fisher = length(unique(fisher_id))),
# by = .(ma_name)]

# Number of female fishers
#ourfish.nmal.num.female.fishers <- ourfish.nmal.fisher.gender[gender == "f"]

# Daily number of buyer recorded
ourfish.nmal.num.buyer <- ourfish.nmal[,
                                     length(unique(buyer_name)),
                                     by = .(date, buyer_name)][order(date)]

ourfish.nmal.num.buyer.monthly <- ourfish.nmal.num.buyer[, mm := format(date, "%b")][, yyyy := format(date, "%Y")][,
                                                                                                                 .(num_recording_buyer_monthly = uniqueN(buyer_name)),
                                                                                                                 by = .(yyyy, mm)]

ourfish.nmal.num.buyer.2 <- ourfish.nmal[, .(num_buyer = uniqueN(buyer_name)),
                                       by = .(date)][order(date)]

ourfish.nmal.cumsum.buyer.1 <- ourfish.nmal[,
                                          .(min_date = min(date)),
                                          by = .(buyer_name)][order(min_date)][,
                                                                               .(num_first_time_buyer = uniqueN(buyer_name)),
                                                                               by = .(min_date)][,
                                                                                                 cumsum_buyer := cumsum(num_first_time_buyer)]

# Daily number of recording fish buyers trend
ourfish.nmal.num.buyer.2.glm <- glm(ourfish.nmal.num.buyer.2$num_buyer ~ as.numeric(ourfish.nmal.num.buyer.2$date))

summary(ourfish.nmal.num.buyer.2.glm)

# Plot number of recording buyer
coeff.cumsum.buyer <- max(ourfish.nmal.cumsum.buyer.1$cumsum_buyer)/max(ourfish.nmal.num.buyer.2$num_buyer)

ourfish.nmal.num.buyer.plot <-
  ggplot(ourfish.nmal.num.buyer.2, aes(x = date, y = num_buyer)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.nmal.cumsum.buyer.1,
            aes(x = min_date, y = cumsum_buyer/coeff.cumsum.buyer),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.buyer,
                                         name = "Cumulative number of buyers recording"
                     )) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Buyers Recording",
       x = "Month",
       y = "Total number of fish buyers recording per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Combine plot: fishers reporting and buyers recording
ourfish.nmal.num.fishers.buyers.plot <-
  ggarrange(ourfish.nmal.num.fisher.plot, ourfish.nmal.num.buyer.plot,
            ncol = 2, nrow = 1)

# Table for number of reporting fishers & recording buyers by MA
# Data from 01 Aug 2021 to 30 Jul 2023
ourfish.nmal.fisher.buyer.1 <- ourfish.nmal[,
                                          .(num_fisher = length(unique(fisher_id)),
                                            num_buyer = length(unique(buyer_id))),
                                          by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Data from 01 Jun 2019 to 30 Jul 2023
ourfish.nmal.fisher.buyer.2 <- ourfish.nmal.historical[,
                                                     .(num_fisher_hist = length(unique(fisher_id)),
                                                       num_buyer_hist = length(unique(buyer_id))),
                                                     by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Combine both filtered and historical data
ourfish.nmal.fisher.buyer.3 <- ourfish.nmal.fisher.buyer.1[ourfish.nmal.fisher.buyer.2, on = .(ma_name_2), nomatch = NULL]

fwrite(ourfish.nmal.fisher.buyer.3, "C:/Users/IrlanAssidiq/OneDrive - Rare/Walton 2021-2024/plot/Table 1 - Number of fishers & fish buyers by MA.csv")

#### Trends in catch and catch value by location Start ####

# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.nmal$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.nmal$weight_kg, 0.99) #Upper outlier limit

# Number of k value for Rosner Test
num.k.upper.weight <- ourfish.nmal[weight_kg >= quantile(weight_kg, 0.99), .N]
num.k.lower.weight <- ourfish.nmal[weight_kg <= quantile(weight_kg, 0.01), .N]
k.value.rosnertest.weight <- num.k.upper.weight + num.k.lower.weight

# Statistical test for outliers using Rosner Test
rosner.test.landings.data <- rosnerTest(ourfish.nmal$weight_kg, k = k.value.rosnertest.weight)

# Observation/row detected as outliers
landings.data.outliers <- rosner.test.landings.data$all.stats
factor(landings.data.outliers$Outlier)

# Outlier data that will be removed
ourfish.nmal.outlier <- ourfish.nmal[landings.data.outliers$Obs.Num, ]

# fwrite(ourfish.nmal.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_nmal_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.nmal$weight_kg, 0.025)
# q3 <- quantile(ourfish.nmal$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.nmal[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.nmal[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.nmal.clean <- ourfish.nmal[-landings.data.outliers$Obs.Num, ]

ourfish.nmal.clean <- ourfish.nmal.clean[which(ourfish.nmal.clean$count > 0 & ourfish.nmal.clean$count < 10000)]
sum(ourfish.nmal.clean$weight_kg)#1383.6
sum(ourfish.nmal.clean$total_price_local) #10.955.316.898
sum(ourfish.nmal.clean$total_price_usd) #55782.17
uniqueN(ourfish.nmal.clean$buyer_name)#81
uniqueN(ourfish.nmal.clean$community_name) #107
uniqueN(ourfish.nmal.clean$fisher_id) #436
## Box Plot for annual comparison

# Make data for two years period
ourfish.nmal.clean.boxplot.1 <- ourfish.nmal.clean[,
                                                 year_period := as.factor("4 Years Period")]

# Make data for Year 1
ourfish.nmal.clean.boxplot.2 <- ourfish.nmal.clean[date >= start.date & date <= "2022-10-01"][,
                                                                                            year_period := as.factor("Year 1")]

# Make data for Year 2
ourfish.nmal.clean.boxplot.3 <- ourfish.nmal.clean[date >= "2022-08-01" & date <= end.date][,
                                                                                          year_period := as.factor("Year 2")]

# Combine data of two years period with year 1 & 2 data
ourfish.nmal.clean.boxplot.4 <- rbind(ourfish.nmal.clean.boxplot.1, ourfish.nmal.clean.boxplot.2, ourfish.nmal.clean.boxplot.3)

# Annual CPUE & Catch Value using box plot
ourfish.nmal.clean.boxplot.5 <- ourfish.nmal.clean.boxplot.4[,
                                                           .(cpue_kg_trip = sum(weight_kg),
                                                             vpue_idr_trip = sum(total_price_local)),
                                                           by = .(year_period, date, fisher_id)][order(year_period)]

# Quartile 1,2,3 of annual CPUE & Catch Value
ourfish.nmal.clean.boxplot.5[,
                            .(IQR_cpue = quantile(cpue_kg_trip, probs = c(0.25, 0.5, 0.75)),
                              IQR_vpue = quantile(vpue_idr_trip, probs = c(0.25, 0.5, 0.75))),
                            by = .(year_period)]

# Independent Sample T-Test Between CPUE Year 1 and 2
t.test(data = ourfish.nmal.clean.boxplot.5[!year_period == "2 Years Period"],
       cpue_kg_trip ~ year_period,
       var.equal = TRUE)

# Independent Sample T-Test Between Catch Value Year 1 and 2
t.test(data = ourfish.nmal.clean.boxplot.5[!year_period == "2 Years Period"],
       vpue_idr_trip ~ year_period,
       var.equal = TRUE)

# Box Plot for Annual CPUE
boxplot.annual.cpue <- 
  ggplot(data = ourfish.nmal.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = cpue_kg_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), oob = squish) +
  labs(x = "Year Period",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Box Plot for Annual Catch Value
boxplot.annual.catch.value <- 
  ggplot(data = ourfish.nmal.clean.boxplot.5,
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
ourfish.nmal.clean.boxplot.6 <- ourfish.nmal.clean.boxplot.5[,
                                                           .(n_trip = .N,
                                                             mean_cpue = mean(cpue_kg_trip),
                                                             se_cpue = sd(cpue_kg_trip) / sqrt(.N),
                                                             mean_vpue = mean(vpue_idr_trip),
                                                             se_vpue = sd(vpue_idr_trip) / sqrt(.N)),
                                                           by = .(year_period)][order(year_period)]

fwrite(ourfish.nmal.clean.boxplot.6,
       "C:/Users/IrlanAssidiq/Documents/Rare/Rare OurFish/Trial WFF/Table 2 - Annual CPUE & VPUE.csv")

## CPUE and Catch Value Trends

# Rename level1_name and ma_name into the same column name to be combined
ourfish.nmal.combine.1 <- ourfish.nmal.clean[ , !c("ma_name_2")]
setnames(ourfish.nmal.combine.1, "snu_name", "location")

ourfish.nmal.combine.2 <- ourfish.nmal.clean[ , !c("snu_name")]
setnames(ourfish.nmal.combine.2, "ma_name_2", "location")

# Combine data from whole nmal with data by MA+R
ourfish.nmal.prov.ma <- rbind(ourfish.nmal.combine.1, ourfish.nmal.combine.2)

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.nmal.landings.1 <- ourfish.nmal.prov.ma[location == "North Sulawesi",
                                              .(sum_landings = sum(weight_kg)),
                                              by = .(date, location)][order(date)]

ourfish.nmal.landings.1.glm <- ourfish.nmal.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.nmal.landings.1.glm2 <- ourfish.nmal.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.nmal.landings.1.glm2)

# Label Landings trends
ourfish.nmal.landings.1.glm3 <- ourfish.nmal.landings.1.glm2[,
                                                           trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                              stat_test == "not significant" ~ "Stable",
                                                                              TRUE ~ "Decrease")]

trend.label <- ourfish.nmal.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.nmal.landings.1.glm3)

ourfish.nmal.landings.1.plot <- ggplot(ourfish.nmal.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 50,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in North Sulawesi",
       x = "Month",
       y = "Total Landings (kg)") +
  theme.default.1

# Subset weight data by date, location for total landings value
ourfish.nmal.landings.value.1 <- ourfish.nmal.prov.ma[location == "North Sulawesi",
                                                    .(sum_landings_value = sum(total_price_usd)),
                                                    by = .(date, location)][order(date)]

ourfish.nmal.landings.value.1.glm <- ourfish.nmal.landings.value.1 %>%
  nest(data=-location) %>%
  mutate(model = map(data, ~ glm(sum_landings_value ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.nmal.landings.value.1.glm2 <- ourfish.nmal.landings.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.nmal.landings.value.1.glm2)

# Label Landings trends
ourfish.nmal.landings.value.1.glm3 <- ourfish.nmal.landings.value.1.glm2[,
                                                                       trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                          stat_test == "not significant" ~ "Stable",
                                                                                          TRUE ~ "Decrease")]

trend.label.landings.value <- ourfish.nmal.landings.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.nmal.landings.value.1.glm3)

ourfish.nmal.landings.value.1.plot <- ggplot(ourfish.nmal.landings.value.1, aes(x = date, y = sum_landings_value)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 100,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings (USD) in North Sulawesi",
       x = "Month",
       y = "Total Landings (USD)") +
  theme.default.1

#COmbinde landings and landing value (USD)

trans.coeff.value <- max(ourfish.nmal.landings.value.1$sum_landings_value)/max(ourfish.nmal.landings.1$sum_landings)

# Use the plot below to get a sense of varying catch value between managed access
ggplot() +
  geom_smooth(data = ourfish.nmal.landings.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.nmal.landings.value.1,
              aes(x = date, y = sum_landings_value/trans.coeff.value),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff.value,
                                         name = "Value generated (USD)")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  facet_wrap(~ location, ncol = 5, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-05-25", format("%Y-%m-%d")),
                y = 50,
                label = trend),
            hjust = 0.1,
            vjust = 0.5,
            color = "black",
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.landings.value,
            aes(x = as.Date("2024-06-30", format("%Y-%m-%d")),
                y = 50,
                label = trend),
            hjust = 0.7,
            vjust = 0.5,
            color = "#008542",
            size = 4,
            inherit.aes = FALSE) +
  labs(title = "Total landing (kg) and value generated (USD)",
       x = "Month",
       y = "Total landings (kg)") +
  theme.default.5
ourfish.nmal.fishing.pressure.plot <- ggarrange(ourfish.nmal.num.trips.2.plot, ourfish.nmal.num.transaction.2.plot, ourfish.nmal.landings.1.plot,
                                               align = "v",
                                               ncol = 1, nrow = 3)


#nsul
#Subset
start.date.nsul <- "2024-05-25"
ourfish.raw.nsul <- ourfish.raw[date >= start.date & date <= end.date &
                                  snu_name %in% "North Sulawesi" &
                                  !buying_unit %in% exclude.buying.unit &
                                  !weight_kg == 0][order(date)]



# Subset data
# Use general filters except date filters, i.e. historical data
ourfish.raw.nsul.2 <- ourfish.raw[date >= "2019-06-01" & date <= end.date &
                                    snu_name %in% "North Sulawesi" &
                                    !buying_unit %in% exclude.buying.unit &
                                    !weight_kg == 0][order(date)]

# Load fish meta data
fish.meta <- fread("C:/Users/IrlanAssidiq/Dropbox/OURFISH - FASTFIELD/meta_data/Reef Fish Meta Data_2022.07.05.csv",
                   header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("buying_unit", "family"))

# Join OurFish raw nsul data with fish meta data
ourfish.nsul <- ourfish.raw.nsul[fish.meta, on = .(buying_unit), nomatch = NULL]

# Join OurFish Raw nsul 2 data with fish meta data
ourfish.nsul.historical <- ourfish.raw.nsul.2[fish.meta, on = .(buying_unit), nomatch = NULL]

#### General Statistics Start ####

# Number of fish family recorded
uniqueN(ourfish.nsul$lgu_name) #6
uniqueN(ourfish.nsul$family) #7
uniqueN(ourfish.nsul$id) #183


# Number of fish family recorded
uniqueN(ourfish.nsul$buying_unit) #28

# Number of transaction recorded
ourfish.nsul.num.transaction <- ourfish.nsul[,
                                             .N,
                                             by = .(date, fisher_id, buying_unit)][order(date)]

ourfish.nsul.num.transaction.2 <- ourfish.nsul.num.transaction[,
                                                               .(num_daily_transaction = .N),
                                                               by = .(date)][order(date)]
sum(ourfish.nsul.num.transaction.2$num_daily_transaction)

# Daily Number Transaction Trend
ourfish.nsul.num.transaction.2.glm <- glm(ourfish.nsul.num.transaction.2$num_daily_transaction ~ as.numeric(ourfish.nsul.num.transaction.2$date))

summary(ourfish.nsul.num.transaction.2.glm)


ourfish.nsul.num.transaction.2.plot <- ggplot(ourfish.nsul.num.transaction.2, aes(x = date, y = num_daily_transaction)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 10,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Transactions in South East Sulawesi",
       x = "Month",
       y = "Number of Transaction") +
  theme.default.1

# Number of trips
ourfish.nsul.num.trips <- ourfish.nsul[,
                                       .N,
                                       by = .(date, fisher_id)][order(date)]

ourfish.nsul.num.trips.2 <- ourfish.nsul.num.trips[,
                                                   .(num_daily_trips = .N),
                                                   by = .(date)][order(date)]
sum(ourfish.nsul.num.trips.2$num_daily_trips) #136
# Daily Number Trips Trend
ourfish.nsul.num.trips.2.glm <- glm(ourfish.nsul.num.trips.2$num_daily_trips ~ as.numeric(ourfish.nsul.num.trips.2$date))

summary(ourfish.nsul.num.trips.2.glm)

ourfish.nsul.num.trips.2.plot <- ggplot(ourfish.nsul.num.trips.2, aes(x = date, y = num_daily_trips)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,25), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Increase",
           color = "black",
           size = 6,
           hjust = 0.1,
           #vjust = 5
  ) +
  labs(title = "Number of Fishing Trips in South East Sulawesi",
       x = "Month",
       y = "Number of Fishing Trips") +
  theme.default.1

# Number of communities recorded
ourfish.nsul.num.community <- uniqueN(ourfish.nsul$community_name) #2

# Number of MA+R recorded
ourfish.nsul.num.mar <- uniqueN(ourfish.nsul$ma_name) #8
uniqueN(ourfish.nsul$buyer_id) #16
uniqueN(ourfish.nsul$fisher_id) #65

# Daily number of Reporting Fishers
ourfish.nsul.num.fisher.monthly <- ourfish.nsul[order(date)][,
                                                             mm := format(date, "%b")][,
                                                                                       yyyy := format(date, "%Y")][,
                                                                                                                   .(num_reporting_fisher_monthly = uniqueN(fisher_id)),
                                                                                                                   by = .(yyyy, mm)]

ourfish.nsul.num.fisher.2 <- ourfish.nsul[,
                                          .(num_fisher = uniqueN(fisher_id)),
                                          by = .(date)][order(date)]

ourfish.nsul.cumsum.fisher.1 <- ourfish.nsul[,
                                             .(min_date = min(date)),
                                             by = .(fisher_id)][,
                                                                .(num_first_time_fisher = uniqueN(fisher_id)),
                                                                by = .(min_date)][order(min_date)][,
                                                                                                   cumsum_fisher := cumsum(num_first_time_fisher)]

# Daily number of reporting fishers trend
ourfish.nsul.num.fisher.2.glm <- glm(num_fisher ~ as.numeric(date),
                                     data = ourfish.nsul.num.fisher.2)

summary(ourfish.nsul.num.fisher.2.glm)

# Plot - Number of Fisher Reporting Their Catch
coeff.cumsum.fisher <- max(ourfish.nsul.cumsum.fisher.1$cumsum_fisher)/max(ourfish.nsul.num.fisher.2$num_fisher)

ourfish.nsul.num.fisher.plot <-
  ggplot() +
  geom_smooth(data = ourfish.nsul.num.fisher.2, aes(x = date, y = num_fisher),
              method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.nsul.cumsum.fisher.1,
            aes(x = min_date, y = cumsum_fisher/coeff.cumsum.fisher),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.fisher,
                                         name = "Cumulative number of fishers reporting")) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 20,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Fishers Reporting",
       x = "Month",
       y = "Total number of fishers reporting per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Load registered fishers data
#registered.fishers <- fread("https://query.data.world/s/gdje6vzfgu3mpvblwuihs6komiqojo",
#header = TRUE, stringsAsFactors = TRUE, na.strings = "NA", select = c("provinsi", "kabupaten", "ma_name", "fisher_name", "fisher_id", "gender"))

#ourfish.nsul.fisher.gender <- ourfish.nsul.num.fishers[registered.fishers,
#on = .(fisher_id), nomatch = NULL]

#ourfish.nsul.num.reg.fisher.mar <- registered.fishers[provinsi == "South East Sulawesi" & !ma_name %in% c("Pasi Kolaga", "Sagori", "Wabula", "Maginti", "Wabula", "Tiworo Utara"),
#  .(num_reg_fisher = length(unique(fisher_id))),
# by = .(ma_name)]

# Number of female fishers
#ourfish.nsul.num.female.fishers <- ourfish.nsul.fisher.gender[gender == "f"]

# Daily number of buyer recorded
ourfish.nsul.num.buyer <- ourfish.nsul[,
                                       length(unique(buyer_name)),
                                       by = .(date, buyer_name)][order(date)]

ourfish.nsul.num.buyer.monthly <- ourfish.nsul.num.buyer[, mm := format(date, "%b")][, yyyy := format(date, "%Y")][,
                                                                                                                   .(num_recording_buyer_monthly = uniqueN(buyer_name)),
                                                                                                                   by = .(yyyy, mm)]

ourfish.nsul.num.buyer.2 <- ourfish.nsul[, .(num_buyer = uniqueN(buyer_name)),
                                         by = .(date)][order(date)]

ourfish.nsul.cumsum.buyer.1 <- ourfish.nsul[,
                                            .(min_date = min(date)),
                                            by = .(buyer_name)][order(min_date)][,
                                                                                 .(num_first_time_buyer = uniqueN(buyer_name)),
                                                                                 by = .(min_date)][,
                                                                                                   cumsum_buyer := cumsum(num_first_time_buyer)]

# Daily number of recording fish buyers trend
ourfish.nsul.num.buyer.2.glm <- glm(ourfish.nsul.num.buyer.2$num_buyer ~ as.numeric(ourfish.nsul.num.buyer.2$date))

summary(ourfish.nsul.num.buyer.2.glm)

# Plot number of recording buyer
coeff.cumsum.buyer <- max(ourfish.nsul.cumsum.buyer.1$cumsum_buyer)/max(ourfish.nsul.num.buyer.2$num_buyer)

ourfish.nsul.num.buyer.plot <-
  ggplot(ourfish.nsul.num.buyer.2, aes(x = date, y = num_buyer)) +
  geom_smooth(method = "loess", span = 0.3, color = "black") +
  geom_line(data = ourfish.nsul.cumsum.buyer.1,
            aes(x = min_date, y = cumsum_buyer/coeff.cumsum.buyer),
            color = "#005BBB", size = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish,
                     sec.axis = sec_axis(trans = ~ . * coeff.cumsum.buyer,
                                         name = "Cumulative number of buyers recording"
                     )) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2020-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2021-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  annotate("text",
           x = as.Date("2024-04-01", format("%Y-%m-%d")),
           y = 5,
           label = "Stable",
           color = "black",
           size = 6,
           hjust = 0.1,
           vjust = 5) +
  labs(title = "Buyers Recording",
       x = "Month",
       y = "Total number of fish buyers recording per day"
  ) +
  theme.default.1 +
  theme(axis.title.y.right = element_text(size = 12, color = "#005BBB"),
        axis.text.y.right = element_text(size = 12, color = "#005BBB"),
        axis.title.y.left = element_text(size = 12, color = "black"),
        axis.text.y.left = element_text(size = 12, color = "black"))

# Combine plot: fishers reporting and buyers recording
ourfish.nsul.num.fishers.buyers.plot <-
  ggarrange(ourfish.nsul.num.fisher.plot, ourfish.nsul.num.buyer.plot,
            ncol = 2, nrow = 1)

# Table for number of reporting fishers & recording buyers by MA
# Data from 01 Aug 2021 to 30 Jul 2023
ourfish.nsul.fisher.buyer.1 <- ourfish.nsul[,
                                            .(num_fisher = length(unique(fisher_id)),
                                              num_buyer = length(unique(buyer_id))),
                                            by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Data from 01 Jun 2019 to 30 Jul 2023
ourfish.nsul.fisher.buyer.2 <- ourfish.nsul.historical[,
                                                       .(num_fisher_hist = length(unique(fisher_id)),
                                                         num_buyer_hist = length(unique(buyer_id))),
                                                       by = .(lgu_name, ma_name_2)][order(lgu_name, ma_name_2)]

# Combine both filtered and historical data
ourfish.nsul.fisher.buyer.3 <- ourfish.nsul.fisher.buyer.1[ourfish.nsul.fisher.buyer.2, on = .(ma_name_2), nomatch = NULL]

fwrite(ourfish.nsul.fisher.buyer.3, "C:/Users/IrlanAssidiq/OneDrive - Rare/Walton 2021-2024/plot/Table 1 - Number of fishers & fish buyers by MA.csv")

#### Trends in catch and catch value by location Start ####

# Detecting outliers in weight data
library(EnvStats)
quantile(ourfish.ns$weight_kg, 0.01) #Lower outlier limit
quantile(ourfish.ns$weight_kg, 0.99) #Upper outlier limit

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

# fwrite(ourfish.nsul.outlier, "Z:/Private/edelarosa/Rare Indonesia/Report - Walton Family Foundation/Results for Walton Report 2021_Ver.2021.07.19/ourfish_nsul_outlier_Ver.2021.07.20.csv")

## Alternative way of removing outlier
# q1 <- quantile(ourfish.nsul$weight_kg, 0.025)
# q3 <- quantile(ourfish.nsul$weight_kg, 0.975)
# iqr <- q3 - q1
# lower.outlier.threshold <- q1 - (1.5 * iqr)
# upper.outlier.threshold <- q3 + (1.5 * iqr)
# 
# tes.outlier <- ourfish.nsul[weight_kg < lower.outlier.threshold | weight_kg > upper.outlier.threshold]
# 
# tes.clean <- ourfish.nsul[weight_kg >= lower.outlier.threshold & weight_kg <= upper.outlier.threshold]

# Removing outlier from landing data
ourfish.ns.clean <- ourfish.ns[-landings.data.outliers$Obs.Num, ]

ourfish.nsul.clean <- ourfish.nsul.clean[which(ourfish.nsul.clean$count > 0 & ourfish.nsul.clean$count < 10000)]
sum(ourfish.nsul.clean$weight_kg)#1740.9
sum(ourfish.nsul.clean$total_price_local) #66983295
sum(ourfish.nsul.clean$total_price_usd) #4308.413
uniqueN(ourfish.nsul.clean$buyer_name)#16
uniqueN(ourfish.nsul.clean$community_name) #16
uniqueN(ourfish.nsul.clean$fisher_id) #60
## Box Plot for annual comparison

# Make data for two years period
ourfish.nsul.clean.boxplot.1 <- ourfish.nsul.clean[,
                                                   year_period := as.factor("4 Years Period")]

# Make data for Year 1
ourfish.nsul.clean.boxplot.2 <- ourfish.nsul.clean[date >= start.date & date <= "2022-10-01"][,
                                                                                              year_period := as.factor("Year 1")]

# Make data for Year 2
ourfish.nsul.clean.boxplot.3 <- ourfish.nsul.clean[date >= "2022-08-01" & date <= end.date][,
                                                                                            year_period := as.factor("Year 2")]

# Combine data of two years period with year 1 & 2 data
ourfish.nsul.clean.boxplot.4 <- rbind(ourfish.nsul.clean.boxplot.1, ourfish.nsul.clean.boxplot.2, ourfish.nsul.clean.boxplot.3)

# Annual CPUE & Catch Value using box plot
ourfish.nsul.clean.boxplot.5 <- ourfish.nsul.clean.boxplot.4[,
                                                             .(cpue_kg_trip = sum(weight_kg),
                                                               vpue_idr_trip = sum(total_price_local)),
                                                             by = .(year_period, date, fisher_id)][order(year_period)]

# Quartile 1,2,3 of annual CPUE & Catch Value
ourfish.nsul.clean.boxplot.5[,
                             .(IQR_cpue = quantile(cpue_kg_trip, probs = c(0.25, 0.5, 0.75)),
                               IQR_vpue = quantile(vpue_idr_trip, probs = c(0.25, 0.5, 0.75))),
                             by = .(year_period)]

# Independent Sample T-Test Between CPUE Year 1 and 2
t.test(data = ourfish.nsul.clean.boxplot.5[!year_period == "2 Years Period"],
       cpue_kg_trip ~ year_period,
       var.equal = TRUE)

# Independent Sample T-Test Between Catch Value Year 1 and 2
t.test(data = ourfish.nsul.clean.boxplot.5[!year_period == "2 Years Period"],
       vpue_idr_trip ~ year_period,
       var.equal = TRUE)

# Box Plot for Annual CPUE
boxplot.annual.cpue <- 
  ggplot(data = ourfish.nsul.clean.boxplot.5,
         aes(x = factor(year_period, levels = c("2 Years Period", "Year 1", "Year 2")),
             y = cpue_kg_trip)) +
  geom_boxplot(outlier.colour = "#AA1948") +
  scale_y_continuous(expand = c(0,0), limits = c(0,75), oob = squish) +
  labs(x = "Year Period",
       y = "CPUE (kg/trip)") +
  theme.default.1

# Box Plot for Annual Catch Value
boxplot.annual.catch.value <- 
  ggplot(data = ourfish.nsul.clean.boxplot.5,
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
ourfish.nsul.clean.boxplot.6 <- ourfish.nsul.clean.boxplot.5[,
                                                             .(n_trip = .N,
                                                               mean_cpue = mean(cpue_kg_trip),
                                                               se_cpue = sd(cpue_kg_trip) / sqrt(.N),
                                                               mean_vpue = mean(vpue_idr_trip),
                                                               se_vpue = sd(vpue_idr_trip) / sqrt(.N)),
                                                             by = .(year_period)][order(year_period)]

fwrite(ourfish.nsul.clean.boxplot.6,
       "C:/Users/IrlanAssidiq/Documents/Rare/Rare OurFish/Trial WFF/Table 2 - Annual CPUE & VPUE.csv")

## CPUE and Catch Value Trends

# Rename level1_name and ma_name into the same column name to be combined
ourfish.nsul.combine.1 <- ourfish.nsul.clean[ , !c("ma_name_2")]
setnames(ourfish.nsul.combine.1, "snu_name", "location")

ourfish.nsul.combine.2 <- ourfish.nsul.clean[ , !c("snu_name")]
setnames(ourfish.nsul.combine.2, "ma_name_2", "location")

# Combine data from whole nsul with data by MA+R
ourfish.nsul.prov.ma <- rbind(ourfish.nsul.combine.1, ourfish.nsul.combine.2)

#### Total Landings Trends in Southeast Sulawesi ####
# Subset weight data by date, location for total landings
ourfish.nsul.landings.1 <- ourfish.nsul.prov.ma[location == "North Sulawesi",
                                                .(sum_landings = sum(weight_kg)),
                                                by = .(date, location)][order(date)]

ourfish.nsul.landings.1.glm <- ourfish.nsul.landings.1 %>%
  nest(-location) %>%
  mutate(model = map(data, ~ glm(sum_landings ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.nsul.landings.1.glm2 <- ourfish.nsul.landings.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.nsul.landings.1.glm2)

# Label Landings trends
ourfish.nsul.landings.1.glm3 <- ourfish.nsul.landings.1.glm2[,
                                                             trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                stat_test == "not significant" ~ "Stable",
                                                                                TRUE ~ "Decrease")]

trend.label <- ourfish.nsul.landings.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.nsul.landings.1.glm3)

ourfish.nsul.landings.1.plot <- ggplot(ourfish.nsul.landings.1, aes(x = date, y = sum_landings)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 50,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings in North Sulawesi",
       x = "Month",
       y = "Total Landings (kg)") +
  theme.default.1

# Subset weight data by date, location for total landings value
ourfish.nsul.landings.value.1 <- ourfish.nsul.prov.ma[location == "North Sulawesi",
                                                      .(sum_landings_value = sum(total_price_usd)),
                                                      by = .(date, location)][order(date)]

ourfish.nsul.landings.value.1.glm <- ourfish.nsul.landings.value.1 %>%
  nest(data=-location) %>%
  mutate(model = map(data, ~ glm(sum_landings_value ~ date, data = .))) %>%
  mutate(tidied = map(model, tidy)) %>%
  unnest(tidied)

ourfish.nsul.landings.value.1.glm2 <- ourfish.nsul.landings.value.1.glm %>%
  dplyr::filter(term == "date") %>%
  mutate(p_value_adjusted = p.adjust(p.value)) %>%
  arrange(p_value_adjusted) %>%
  mutate(stat_test = ifelse(p_value_adjusted < 0.05, "significant", "not significant")) %>% #Pvalue <0.05 indicate ada perbedaan antara data dalam kelompok (signifikan)
  dplyr::select(location, term, estimate, std.error, statistic, p.value, p_value_adjusted, stat_test)

setDT(ourfish.nsul.landings.value.1.glm2)

# Label Landings trends
ourfish.nsul.landings.value.1.glm3 <- ourfish.nsul.landings.value.1.glm2[,
                                                                         trend := case_when(estimate >= 0 & stat_test == "significant" ~ "Increase", #estimate is to measure the alternative hypothesis, >=0 means there is positive linear relationship between predictor (date) and response (CPUE or weight)
                                                                                            stat_test == "not significant" ~ "Stable",
                                                                                            TRUE ~ "Decrease")]

trend.label.landings.value <- ourfish.nsul.landings.value.1.glm3[, `:=`(term = NULL, estimate = NULL, std.error = NULL, statistic = NULL, p.value = NULL, p_value_adjusted = NULL, stat_test = NULL)]

summary(ourfish.nsul.landings.value.1.glm3)

ourfish.nsul.landings.value.1.plot <- ggplot(ourfish.nsul.landings.value.1, aes(x = date, y = sum_landings_value)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish) +
  scale_x_date(labels = date_format("%b %Y"),
               date_breaks = "1 month") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 100,
                label = trend),
            hjust = 0.1,
            color = "black",
            size = 6,
            #vjust = 5,
            inherit.aes = FALSE) +
  labs(title = "Total Landings (USD) in North Sulawesi",
       x = "Month",
       y = "Total Landings (USD)") +
  theme.default.1

#COmbinde landings and landing value (USD)

trans.coeff.value <- max(ourfish.nsul.landings.value.1$sum_landings_value)/max(ourfish.nsul.landings.1$sum_landings)

# Use the plot below to get a sense of varying catch value between managed access
ggplot() +
  geom_smooth(data = ourfish.nsul.landings.1,
              aes(x = date, y = sum_landings),
              method = "loess", span = 0.5, se = TRUE, color = "black") +
  geom_smooth(data = ourfish.nsul.landings.value.1,
              aes(x = date, y = sum_landings_value/trans.coeff.value),
              method = "loess", span = 0.5, se = TRUE, color = "#008542", fill = "#008542", alpha = 0.3) +
  scale_y_continuous(expand = c(0,0), limits = c(0,NA), oob = squish, labels = comma,
                     sec.axis = sec_axis(trans = ~. * trans.coeff.value,
                                         name = "Value generated (USD)")) +
  scale_x_date(labels = date_format("%b %Y"),
               # breaks = as.Date(c("2021-08-01", "2021-12-01",
               #                    "2022-06-01", "2022-12-01",
               #                    "2023-06-01")),
               date_breaks = "1 month") +
  geom_vline(xintercept = as.Date(c("2022-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2023-01-01")),
             color = "#5E6A71") +
  geom_vline(xintercept = as.Date(c("2024-01-01")),
             color = "#5E6A71") +
  facet_wrap(~ location, ncol = 5, scales = "free_y") +
  geom_text(data = trend.label,
            aes(x = as.Date("2024-04-01", format("%Y-%m-%d")),
                y = 75,
                label = trend),
            hjust = 0.1,
            vjust = 0.5,
            color = "black",
            size = 4,
            inherit.aes = FALSE) +
  geom_text(data = trend.label.landings.value,
            aes(x = as.Date("2024-06-30", format("%Y-%m-%d")),
                y = 75,
                label = trend),
            hjust = 0.7,
            vjust = 0.5,
            color = "#008542",
            size = 4,
            inherit.aes = FALSE) +
  labs(title = "Total landing (kg) and value generated (USD)",
       x = "Month",
       y = "Total landings (kg)") +
  theme.default.5
ourfish.nsul.fishing.pressure.plot <- ggarrange(ourfish.nsul.num.trips.2.plot, ourfish.nsul.num.transaction.2.plot, ourfish.nsul.landings.1.plot,
                                                align = "v",
                                                ncol = 1, nrow = 3)