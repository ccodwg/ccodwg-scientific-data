### A sub-national real-time epidemiological and vaccination database for the COVID-19 pandemic in Canada ###
# Isha Berry, Meghan O’Neill, Shelby L. Sturrock, James E. Wright, Kamal Acharya, Gabrielle Brankston, Vinyas Harish, Kathy Kornas, Nika Maani, Thivya Naganathan, Lindsay Obress, Tanya Rossi, Alison E. Simmons, Matthew Van Camp, Xiao Xie, Ashleigh R. Tuite, Amy L. Greer, David N. Fisman and Jean-Paul R. Soucy

# load packages
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(cowplot)
library(sf)

# create directories for outputs
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

### CREATE FIGURES ###

# figure 2 (per-capita rates)

## load data
hr <- st_read("geo/RegionalHealthBoundaries.shp") %>%
  mutate(HR_UID = as.integer(HR_UID))
hr_map <- read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/other/hr_map.csv")

cases <- read_csv("data/cases_timeseries_hr.csv") %>%
  rename(date = date_report) %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

mortality <- read_csv("data/mortality_timeseries_hr.csv") %>%
  rename(date = date_death_report) %>%
  mutate(date = as.Date(date, "%d-%m-%Y"))

## define max date
date_max <- as.Date("2020-12-21")
date_max_lab <- format(date_max, "%B %d, %Y")

## summarize cases
cases <- cases %>%
  group_by(province, health_region) %>%
  filter(date %in% c(as.Date("2020-03-11"), as.Date("2020-09-07"), date_max))

## summarize deaths
mortality <- mortality %>%
  group_by(province, health_region) %>%
  filter(date %in% c(as.Date("2020-03-11"), as.Date("2020-09-07"), date_max))

## combine data
dat <- full_join(
  cases,
  mortality,
  by = c("date", "province", "health_region")
) %>%
  left_join(
    hr_map,
    by = c("province", "health_region")
  )

## join data to map
hr <- hr %>%
  left_join(
    dat,
    by = "HR_UID"
  )

## calculate per-capita rates per 100,000
hr$cumulative_cases_per_100k <- hr$cumulative_cases / hr$pop * 100000
hr$cumulative_deaths_per_100k <- hr$cumulative_deaths / hr$pop * 100000

## set 0 to NA for plotting
hr$cumulative_cases_per_100k <- ifelse(hr$cumulative_cases_per_100k == 0, NA, hr$cumulative_cases_per_100k)
hr$cumulative_deaths_per_100k <- ifelse(hr$cumulative_deaths_per_100k == 0, NA, hr$cumulative_deaths_per_100k)

## cases map
p_c <- ggplot(hr) +
  geom_sf(aes(fill = cumulative_cases_per_100k), size = 0.2) +
  scale_fill_gradient(low = "#EFF3FF", high = "#2171B5", na.value = "#E3E3E3", labels = function(x) format(x, big.mark = ",")) +
  facet_wrap(~ date) +
  labs(fill = "Cumulative cases per 100,000") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.key.width = unit(0.05, "npc"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )
p_c

## mortality map
p_m <- ggplot(hr) +
  geom_sf(aes(fill = cumulative_deaths_per_100k), size = 0.2) +
  scale_fill_gradient(low = "#FEE0D2", high = "#DE2D26", na.value = "#E3E3E3", labels = function(x) format(x, big.mark = ",")) +
  facet_wrap(~ date) +
  labs(fill = "Cumulative deaths per 100,000") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.key.width = unit(0.05, "npc"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )
p_m

## arrange plots
fig_2 <- plot_grid(p_c, p_m, nrow = 2, labels = "auto")

## plot
fig_2

## save as PDF
ggsave("figures/fig_2.pdf")

# figure S1 (absolute counts)

## set 0 to NA for plotting
hr$cumulative_cases <- ifelse(hr$cumulative_cases == 0, NA, hr$cumulative_cases)
hr$cumulative_deaths <- ifelse(hr$cumulative_deaths == 0, NA, hr$cumulative_deaths)

## cases map
p_c_s <- ggplot(hr) +
  geom_sf(aes(fill = cumulative_cases), size = 0.2) +
  scale_fill_gradient(low = "#EFF3FF", high = "#2171B5", na.value = "#E3E3E3", labels = function(x) format(x, big.mark = ",")) +
  facet_wrap(~ date) +
  labs(fill = "Cumulative cases") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.key.width = unit(0.05, "npc"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )
p_c_s

## mortality map
p_m_s <- ggplot(hr) +
  geom_sf(aes(fill = cumulative_deaths), size = 0.2) +
  scale_fill_gradient(low = "#FEE0D2", high = "#DE2D26", na.value = "#E3E3E3", labels = function(x) format(x, big.mark = ",")) +
  facet_wrap(~ date) +
  labs(fill = "Cumulative deaths") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    strip.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.key.width = unit(0.05, "npc"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )
p_m_s

## arrange plots
fig_s1 <- plot_grid(p_c_s, p_m_s, nrow = 2, labels = "auto")

## plot
fig_s1

## save as PDF
ggsave("figures/fig_s1.pdf")

# figure 3

## load data
cases <- read_csv("data/cases_timeseries_canada.csv") %>%
  rename(date = date_report) %>%
  mutate(date = as.Date(date, "%d-%m-%Y"),
         cases_roll = rollapply(cases, width = 7, FUN = mean, align = "right", partial = TRUE)
         ) %>%
  filter(date <= date_max)

mortality <- read_csv("data/mortality_timeseries_canada.csv") %>%
  rename(date = date_death_report) %>%
  mutate(date = as.Date(date, "%d-%m-%Y"),
         deaths_roll = rollapply(deaths, width = 7, FUN = mean, align = "right", partial = TRUE)
         ) %>%
  filter(date <= date_max)

## combine data
dat <- full_join(cases, mortality, by = c("province", "date"))

## figure theme
theme_fig <- theme_minimal() +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    )))

## plot - cases
p_c <- ggplot(dat, aes(x = date)) +
  geom_density(aes(y = cases_roll), stat = "identity", colour = "#2171B5") +
  geom_area(aes(y = cases), fill = "#2171B5", alpha = 0.4) +
  labs(x = "Date", y = "Daily COVID-19 Cases") +
  scale_x_date(breaks = c(as.Date("2020-02-01"), as.Date("2020-05-01"), as.Date("2020-09-01"), as.Date("2020-12-01")), date_labels = "%b %Y") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  theme_fig +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )

## plot - mortality
p_m <- ggplot(dat, aes(x = date)) +
  geom_density(aes(y = deaths_roll), stat = "identity", colour = "#DE2D26") +
  geom_area(aes(y = deaths), fill = "#DE2D26", alpha = 0.4) +
  labs(x = "Date", y = "Daily COVID-19 Deaths") +
  scale_x_date(breaks = c(as.Date("2020-02-01"), as.Date("2020-05-01"), as.Date("2020-09-01"), as.Date("2020-12-01")), date_labels = "%b %Y") +
  scale_y_continuous(label = comma, expand = c(0, 0)) +
  theme_fig +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = grid::unit(c(1, 1, 1, 1), "mm") # minimize whitespace
  )

## arrange plots
fig_3 <- plot_grid(p_c, p_m, ncol = 2, labels = "auto")
fig_3 <- ggdraw(add_sub(fig_3, "Report Date", size = 11, vpadding = grid::unit(1, "lines"), y = 1, x = 0.5, vjust = 1))

## plot
fig_3

## save as PDF
ggsave("figures/fig_3.pdf")

### CREATE TABLES ###

## load data
cases <- read_csv("data/cases_city.zip") # large file
deaths <- read_csv("data/mortality.csv")

## define time points of interest
time1 <- as.Date("2020-03-11")
time2 <- as.Date("2020-09-07")
time3 <- as.Date("2020-12-21")

# supplementary table 1 (cases)

## Jan 25 - Mar 11
miss_cases_time1 <- cases %>% 
  filter(date_report <= time1) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex, "Not Reported"), health_region = na_if(health_region, "Not Reported"), city = na_if(city, "Not Reported"), travel_yn = na_if(travel_yn, "Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), city = sum(is.na(city)), source = sum(is.na(travel_yn)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr=health_region / n, prop_city = city / n, prop_source = source / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2), prop_city = round(prop_city * 100, 2), prop_source = round(prop_source * 100, 2))

## Jan 25 - Sep 7
miss_cases_time2 <- cases %>% 
  filter(date_report <= time2) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex,"Not Reported"), health_region = na_if(health_region, "Not Reported"), city = na_if(city, "Not Reported"), travel_yn = na_if(travel_yn,"Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), city = sum(is.na(city)), source = sum(is.na(travel_yn)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr = health_region / n, prop_city = city / n, prop_source = source / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2), prop_city = round(prop_city * 100, 2), prop_source = round(prop_source * 100, 2))

## Jan 25 - Dec 21
miss_cases_time3 <- cases %>% 
  filter(date_report <= time3) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex, "Not Reported"), health_region = na_if(health_region, "Not Reported"), city = na_if(city, "Not Reported"), travel_yn = na_if(travel_yn, "Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), city = sum(is.na(city)), source = sum(is.na(travel_yn)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr = health_region / n, prop_city = city / n, prop_source = source / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2), prop_city = round(prop_city * 100, 2), prop_source = round(prop_source * 100, 2))

## merge data and keep relevant columns
tab_s1 <- rbind(miss_cases_time1, miss_cases_time2, miss_cases_time3) %>%
  select(province, prop_age, prop_sex, prop_hr, prop_city, prop_source)

## save table (unformatted)
write.csv(tab_s1, "tables/tab_s1.csv", row.names = FALSE, fileEncoding = "UTF-8")

# supplementary table 2 (deaths)

## Jan 25 - Mar 11
miss_deaths_time1 <- deaths %>% 
  mutate(date_death_report = as.Date(as.character(date_death_report), "%d-%m-%Y"))%>%
  filter(date_death_report <= time1) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex,"Not Reported"), health_region = na_if(health_region, "Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr = health_region / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2))

## Jan 25 - Sep 7
miss_deaths_time2 <- deaths %>% 
  mutate(date_death_report = as.Date(as.character(date_death_report), "%d-%m-%Y")) %>%
  filter(date_death_report <= time2) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex, "Not Reported"), health_region = na_if(health_region, "Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr = health_region / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2))

## Jan 25 - Dec 21
miss_deaths_time3 <- deaths %>% 
  mutate(date_death_report = as.Date(as.character(date_death_report), "%d-%m-%Y")) %>%
  filter(date_death_report <= time3) %>%
  mutate(age = na_if(age, "Not Reported"), sex = na_if(sex, "Not Reported"), health_region = na_if(health_region, "Not Reported")) %>%
  group_by(province) %>% 
  summarise(age = sum(is.na(age)), sex = sum(is.na(sex)), health_region = sum(is.na(health_region)), n = n()) %>%
  mutate(prop_age = age / n, prop_sex = sex / n, prop_hr = health_region / n) %>%
  mutate(prop_age = round(prop_age * 100, 2), prop_sex = round(prop_sex * 100, 2), prop_hr = round(prop_hr * 100, 2))

## merge data and keep relevant columns
tab_s2 <- rbind(miss_deaths_time1, miss_deaths_time2, miss_deaths_time3) %>%
  select(province, prop_age, prop_sex, prop_hr)

## save table (unformatted)
write.csv(tab_s2, "tables/tab_s2.csv", row.names = FALSE, fileEncoding = "UTF-8")