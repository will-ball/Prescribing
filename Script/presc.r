# Load in Packages

library(tidyverse)
library(readr)
library(lubridate)
library(ggthemes)

options(scipen = 999) # Remove scientific notation

# 1. BNF Codes ----
# Data from https://applications.nhsbsa.nhs.uk/infosystems/data/showDataSelector.do?reportId=126

prescribing <- read_csv("Data/20210201_1612173880487_BNF_Code_Information.csv")
# Up to date BNF codes

CNS <- prescribing %>%
  dplyr::filter(BNF_Chapter == "Central Nervous System") # Highlight only CNS drugs

BNFanx <- prescribing %>%
  dplyr::filter(BNF_Para == "Anxiolytics") # Only Anxiolytics

anx_chems <- BNFanx %>%
  count(BNF_Chem_Sub, BNF_Chem_Sub_Code) # Chemical names for distinct anxiolytics

# 2. OpenPrescribing data ----
# From: https://openprescribing.net/analyse
# Hypnotics & Anxiolytics Dec 2015 - Nov 2020 for England
# Source: 'OpenPrescribing.net, EBM DataLab, University of Oxford, 2020'

# > Anxiolytics & Hypnotics ----
hyp_anx <- read_csv("Data/items for hypnotics and anxiolytics per.csv")
hyp_anx$date <- dmy(hyp_anx$date)

hyp_anx_totals <- hyp_anx %>%
  group_by(date) %>%
  summarize(
    total_item = sum(y_items),
    total_cost = sum(y_actual_cost)
  ) %>%
  mutate(cost_per = total_cost / total_item)

# > Anxiolytics Alone ----

anx <- read_csv("Data/items for anxiolytics per.csv")
anxtotal$date <- ymd(anxtotal$date)

anx_totals <- anx %>%
  group_by(date) %>%
  summarize(
    total_item = sum(y_items),
    total_cost = sum(y_actual_cost)
  ) %>%
  mutate(cost_per = total_cost / total_item)

# > Antidepressants ----

antidep <- read_csv("Data/items for antidepressant drugs per.csv")
antidep$date <- dmy(antidep$date)

antidep_totals <- antidep %>%
  group_by(date) %>%
  summarize(
    total_item = sum(y_items),
    total_cost = sum(y_actual_cost)
  ) %>%
  mutate(cost_per = total_cost / total_item)

# Plot of total items ----
# change name of dataset, y = in geom_text for alignment & titles

(p <- antidep_totals %>%
  ggplot(aes(x = date, y = total_item)) +
  geom_point() +
  geom_line() +
  #  stat_smooth(method = "loess") +       # For optional trend line
  geom_vline(
    xintercept = as.Date("2020-03-16"), # Date of 1st UK Lockdown
    linetype = "dashed", colour = "dark red"
  ) +
  geom_text(aes(x = as.Date("2020-03-16"), label = "\nUK Lockdown", y = 5500000), # y coord sets aligment
    colour = "dark red", angle = 90, size = 3
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %y") +
  scale_y_continuous(labels = function(l) {
    paste0(round(l / 1e6, 1), "m")
  }) + # allows for display in thousands (k)
  theme_classic() +
  labs(
    x = "Date", y = "Frequency (Thousands)",
    title = "Monthly Total Items Prescribed for Antidepressants (BNF 4.3)",
    subtitle = "December 2015 to November 2020 in England",
    caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2020"
  )
)

ggsave(filename = "Plots/antidep.png", plot = p, width = 6, height = 4, units = "in", dpi = 1000)

# Plot of total cost ----

(p <- antidep_totals %>%
  ggplot(aes(x = date, y = total_cost)) +
  geom_point() +
  geom_line() +
  geom_vline(
    xintercept = as.Date("2020-03-16"),
    linetype = "dashed", colour = "dark red"
  ) +
  geom_text(aes(x = as.Date("2020-03-16"), label = "\nUK Lockdown", y = 20000000),
    colour = "dark red", angle = 90, size = 3
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
  scale_y_continuous(labels = function(l) {
    paste0(round(l / 1e6, 1), "m") }) +
  theme_classic() +
  labs(
    x = "Date", y = "Cost (?)",
    title = "Monthly Total Cost of Prescriptions for Antidepressants (BNF 4.3)",
    subtitle = "December 2015 to November 2020 in England",
    caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2020"
  )
)

# 3. Overall Prescriptions Scotland by month ----
# From opendata.nhs.scot

# Download Data
pis_2020 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/59ca69a6-5c4b-45f9-9d9c-8e52b3244b36?bom=True")
pis_2019 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/ad219b41-8131-4789-b46b-7aac8e4952ec?bom=True")
pis_2018 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/be254c0d-4e0e-491f-9cb9-e6764e53cc96?bom=True")
pis_2017 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/a807a11c-5cae-4090-9d2c-5f4a08765421?bom=True")
pis_2016 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/87f868b0-906e-4ce8-9a29-d21709476c96?bom=True")

# > Total by month & combine for 2016-2019 ----
tmp <- select(pis_2019, c(PaidDateMonth, NumberOfPaidItems)) %>%
  full_join(select(pis_2018,
                   c(PaidDateMonth, NumberOfPaidItems)),
            by = c("PaidDateMonth", "NumberOfPaidItems")) %>%
  mutate(PaidDateMonth = ym(PaidDateMonth)) %>%
  group_by(PaidDateMonth) %>%
  summarize(total_item = sum(NumberOfPaidItems)) # Number of obs too big when raw to do in one go

tmp1 <- select(pis_2017, c(PaidDateMonth, NumberOfPaidItems)) %>%
  full_join(select(pis_2016,
                   c(PaidDateMonth, NumberOfPaidItems)),
            by = c("PaidDateMonth", "NumberOfPaidItems")) %>%
  mutate(PaidDateMonth = ym(PaidDateMonth)) %>%
  group_by(PaidDateMonth) %>%
  summarize(total_item = sum(NumberOfPaidItems))

# Dataset for 2016-19
pis_old <- full_join(tmp, tmp1, by = c("PaidDateMonth", "total_item")) %>%
  mutate(year = case_when(
                year(PaidDateMonth) == 2019 ~ "2019",
                year(PaidDateMonth) == 2018 ~ "2018",
                year(PaidDateMonth) == 2017 ~ "2017",
                year(PaidDateMonth) == 2016 ~ "2016")) %>%
  group_by(month(PaidDateMonth)) %>%
  summarize(max = max(total_item),
            min = min(total_item),
            mean = mean(total_item)) %>%
  rename(PaidDateMonth = 'month(PaidDateMonth)')

# > Create dataset for 2020 ----
pis_2020 <- select(pis_2020, c(PaidDateMonth, NumberOfPaidItems)) %>%
  mutate(PaidDateMonth = ym(PaidDateMonth)) %>%
  group_by(PaidDateMonth) %>%
  summarize(total_item = sum(NumberOfPaidItems)) %>%
  mutate(year = year(PaidDateMonth))

# Oct & Nov (Not included in 2020 annual but available per month)

oct_2020 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/83bf4892-d246-41f8-a6ca-d44d18e2a2ca?bom=True")
nov_2020 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/ab8aa642-2562-4abb-a360-5c5f9e7e349c?bom=True")

oct_2020 <- oct_2020 %>%
  mutate(PaidDateMonth = ym(PaidDateMonth)) %>%
  group_by(PaidDateMonth) %>%
  summarize(total_item = sum(NumberOfPaidItems)) %>%
  mutate(year = year(PaidDateMonth))

nov_2020 <- nov_2020 %>%
  mutate(PaidDateMonth = ym(PaidDateMonth)) %>%
  group_by(PaidDateMonth) %>%
  summarize(total_item = sum(NumberOfPaidItems)) %>%
  mutate(year = year(PaidDateMonth))

pis_2020 <- pis_2020 %>%
  full_join(oct_2020) %>%
  full_join(nov_2020) %>%
  mutate(old_mean = pis_old$mean[1:11])

# Plot monthly totals ----

(p <- ggplot() +
  geom_ribbon(data=pis_old, aes(x = month(PaidDateMonth),
                                ymin=min, ymax=max), fill="Skyblue2") +
  geom_line(data=pis_old, aes(x=month(PaidDateMonth),
                              y=mean), colour="Grey50", linetype=2) +
  geom_line(data=pis_2020, aes(x=month(PaidDateMonth),
                               y=total_item), colour="Red", size = 1) +
  theme_classic() +
  scale_x_continuous(name = "Month Number",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
   scale_y_continuous(labels = function(l) {
     paste0(round(l / 1e6, 1), "m") }) +
  labs(title = "All items prescribed & dispensed in Scotland",
       subtitle = "2020 (red) against min/max (blue) & mean (dashed) for 2016-19",
       y = "Total Items (Millions)", caption = "Source: opendata.nhs.scot on Open Govt License")
)

ggsave(filename = "Plots/pre-pandemic.png", plot = p, height = 4, width = 6, units = "in", dpi = 800)

# Cumulative Sum 2020 vs. Mean 2016-19

(p <- ggplot() +
  geom_line(data = pis_2020, aes(x = month(PaidDateMonth), y = cumsum(total_item)),
            colour = "Red", size = 1) +
  geom_ribbon(data = pis_2020,
              aes(x = month(PaidDateMonth), ymin = cumsum(old_mean), ymax = cumsum(total_item)), 
              fill="Red", alpha=0.2)+
  geom_line(data = pis_old, aes(x = month(PaidDateMonth), y = cumsum(mean)),
            colour = "Grey50", linetype = 2) +
  theme_classic() +
  scale_x_continuous(name = "Month Number",
                     breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(labels = function(l) {
    paste0(round(l / 1e6, 1), "m") }) +
  labs(title = "Cumulative sum of items prescribed & dispensed in Scotland",
       subtitle = "2020 (red) against mean (dashed) for 2016-19",
       y = "Total Items (Millions)", caption = "Source: opendata.nhs.scot on Open Govt License")
)

ggsave(filename = "Plots/cumsum.png", plot = p, height = 6, width = 6, units = "in", dpi = 800)
