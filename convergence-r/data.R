# install.packages("tidyverse")
library(dplyr)
library(tidyr)

# Read GDP per capita csv files
gdp_usd_data <- read.csv("data/RAW-gdp-usd.csv")
# gdp_ppp_data <- read.csv("data/RAW-gdp-ppp.csv")

# Remove unneccessary columns and years
gdp_usd_data <- subset(gdp_usd_data, select = -c(Country.Code, Indicator.Name, Indicator.Code))
gdp_usd_data <- gdp_usd_data[, !(names(gdp_usd_data) %in% c(paste0("X", 1960:2012), paste0("X", 2020:2024)))]

# gdp_ppp_data <- subset(gdp_ppp_data, select = -c(Country.Code, Indicator.Name, Indicator.Code))
# gdp_ppp_data <- gdp_ppp_data[, !(names(gdp_ppp_data) %in% c(paste0("X", 1960:2013), paste0("X", 2020:2024)))]

# Omit rows with na values
gdp_usd_data <- na.omit(gdp_usd_data)
# gdp_ppp_data <- na.omit(gdp_ppp_data)

# Log-transform GDP per capita values
gdp_usd_data[,-1] <- log(gdp_usd_data[,-1])
# gdp_ppp_data[,-1] <- log(gdp_ppp_data[,-1])

# Pivot to long format and clean year values
gdp_pc_usd <- gdp_usd_data %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    names_prefix = "X",
    values_to = "gdp"
  ) %>%
  filter(grepl("^\\d+$", year)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(Country.Name, year)

# gdp_pc_ppp <- gdp_ppp_data %>%
#   pivot_longer(
#     cols = -Country.Name,
#     names_to = "year",
#     names_prefix = "X",
#     values_to = "gdp"
#   ) %>%
#   filter(grepl("^\\d+$", year)) %>%
#   mutate(year = as.numeric(year)) %>%
#   arrange(Country.Name, year)

# Create leading (next time step) column
gdp_pc_usd <- gdp_pc_usd %>%
  group_by(Country.Name) %>%
  mutate(
    lead_gdp = lead(gdp),
  ) %>%
  ungroup()

# gdp_pc_ppp <- gdp_pc_ppp %>%
#   group_by(Country.Name) %>%
#   mutate(
#     lag_gdp = lag(gdp),
#   ) %>%
#   ungroup()

# Take first order differencing to get GDP per capita growth by year
log_gdp_pc_growth <- na.omit(gdp_pc_usd)
log_gdp_pc_growth <- log_gdp_pc_growth %>%
  group_by(Country.Name) %>%
  mutate(
    percent_growth = lead_gdp - gdp,
  ) %>%
  ungroup()


gdp_convergence_data <- log_gdp_pc_growth[ , c("Country.Name", "year", "gdp", "percent_growth")]
names(gdp_convergence_data)[names(gdp_convergence_data) == 'Country.Name'] <- 'country'
names(gdp_convergence_data)[names(gdp_convergence_data) == 'gdp'] <- 'log_gdp'

# Save as csv
write.csv(gdp_convergence_data, "data/gdp-convergence-data.csv", row.names = FALSE)
# write.csv(gdp_pc_usd, "data/log-gdp-pc.csv", row.names = FALSE)
# write.csv(gdp_pc_ppp, "data/log-gdp-pc-ppp.csv", row.names = FALSE)