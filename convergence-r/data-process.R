library(dplyr)
library(tidyr)

# Read variables csv files
gdp_usd <- read.csv("data/RAW-gdp-usd.csv")
pop_growth_rate <- read.csv("control-data/RAW-pop-growth-rate.csv")
savings_rate <- read.csv("control-data/RAW-savings-rate.csv")
gross_capital_formation <- read.csv("control-data/RAW-gross-capital-formation.csv")

# Remove unneccessary columns and years
gdp_usd <- subset(gdp_usd, select = -c(Country.Code, Indicator.Name, Indicator.Code))
gdp_usd <- gdp_usd[, !(names(gdp_usd) %in% c(paste0("X", 1960:2013), paste0("X", 2020:2024)))]

pop_growth_rate <- subset(pop_growth_rate, select = -c(Country.Code, Indicator.Name, Indicator.Code))
pop_growth_rate <- pop_growth_rate[, !(names(pop_growth_rate) %in% c(paste0("X", 1960:2013), paste0("X", 2019:2024)))]

savings_rate <- subset(savings_rate, select = -c(Country.Code, Indicator.Name, Indicator.Code))
savings_rate <- savings_rate[, !(names(savings_rate) %in% c(paste0("X", 1960:2013), paste0("X", 2019:2024)))]

gross_capital_formation <- subset(gross_capital_formation, select = -c(Country.Code, Indicator.Name, Indicator.Code))
gross_capital_formation <- gross_capital_formation[, !(names(gross_capital_formation) %in% c(paste0("X", 1960:2013), paste0("X", 2019:2024)))]

# Omit rows with na values
gdp_usd <- na.omit(gdp_usd)
pop_growth_rate <- na.omit(pop_growth_rate)
savings_rate <- na.omit(savings_rate)
gross_capital_formation <- na.omit(gross_capital_formation)

# Keep only countries under intersection
countries_gdp <- gdp_usd$Country.Name
countries_gross_cap <- gross_capital_formation$Country.Name
countries_pop <- pop_growth_rate$Country.Name
countries_savings <- savings_rate$Country.Name

common_countries <- Reduce(intersect, list(
  countries_gdp,
  countries_gross_cap,
  countries_pop,
  countries_savings
))

gdp_usd <- subset(gdp_usd, Country.Name %in% common_countries)
gross_capital_formation <- subset(gross_capital_formation, Country.Name %in% common_countries)
pop_growth_rate <- subset(pop_growth_rate, Country.Name %in% common_countries)
savings_rate <- subset(savings_rate, Country.Name %in% common_countries)

# Log-transform GDP per capita values
gdp_usd[,-1] <- log(gdp_usd[,-1])

# Pivot to long format and clean year values
gdp_usd <- gdp_usd %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    names_prefix = "X",
    values_to = "gdp"
  ) %>%
  filter(grepl("^\\d+$", year)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(Country.Name, year)

pop_growth_rate <- pop_growth_rate %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    names_prefix = "X",
    values_to = "growth rate"
  ) %>%
  filter(grepl("^\\d+$", year)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(Country.Name, year)

savings_rate <- savings_rate %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    names_prefix = "X",
    values_to = "savings rate"
  ) %>%
  filter(grepl("^\\d+$", year)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(Country.Name, year)

gross_capital_formation <- gross_capital_formation %>%
  pivot_longer(
    cols = -Country.Name,
    names_to = "year",
    names_prefix = "X",
    values_to = "gross capital formation"
  ) %>%
  filter(grepl("^\\d+$", year)) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(Country.Name, year)

# Create leading (next time step) column
gdp_usd <- gdp_usd %>%
  group_by(Country.Name) %>%
  mutate(
    lead_gdp = lead(gdp),
  ) %>%
  ungroup()

# Take first order differencing to get GDP per capita growth by year
gdp_usd <- na.omit(gdp_usd)
gdp_usd <- gdp_usd %>%
  group_by(Country.Name) %>%
  mutate(
    percent_growth = lead_gdp - gdp,
  ) %>%
  ungroup()

gdp_usd <- gdp_usd[ , c("Country.Name", "year", "gdp", "percent_growth")]
names(gdp_usd)[names(gdp_usd) == 'Country.Name'] <- 'country'
names(gdp_usd)[names(gdp_usd) == 'gdp'] <- 'log_gdp'

names(pop_growth_rate)[names(pop_growth_rate) == 'Country.Name'] <- 'country'
names(savings_rate)[names(savings_rate) == 'Country.Name'] <- 'country'
names(gross_capital_formation)[names(gross_capital_formation) == 'Country.Name'] <- 'country'

# Remove non-countries
non_countries <- c(
  "Africa Eastern and Southern",
  "Arab World",
  "Central Europe and the Baltics",
  "Early-demographic dividend",
  "East Asia & Pacific",
  "East Asia & Pacific (IDA & IBRD countries)",
  "East Asia & Pacific (excluding high income)",
  "Euro area",
  "Europe & Central Asia",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Europe & Central Asia (excluding high income)",
  "European Union",
  "Heavily indebted poor countries (HIPC)",
  "High income",
  "IBRD only",
  "IDA & IBRD total",
  "IDA only",
  "IDA total",
  "Late-demographic dividend",
  "Latin America & Caribbean",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Least developed countries: UN classification",
  "Low & middle income",
  "Lower middle income",
  "Middle East & North Africa",
  "Middle income",
  "North America",
  "OECD members",
  "Other small states",
  "Pacific island small states",
  "Post-demographic dividend",
  "Small states",
  "South Asia",
  "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Sub-Saharan Africa (excluding high income)",
  "Upper middle income",
  "World"
)

gdp_usd <- gdp_usd[ !gdp_usd$country %in% non_countries, ]
pop_growth_rate <- pop_growth_rate[ !pop_growth_rate$country %in% non_countries, ]
savings_rate <- savings_rate[ !savings_rate$country %in% non_countries, ]
gross_capital_formation <- gross_capital_formation[ !gross_capital_formation$country %in% non_countries, ]

# Save as csv
write.csv(gdp_usd, "data/gdp-usd.csv", row.names = FALSE)
write.csv(pop_growth_rate, "control-data/pop-gr.csv", row.names = FALSE)
write.csv(savings_rate, "control-data/sav-rate.csv", row.names = FALSE)
write.csv(gross_capital_formation, "control-data/capital-form.csv", row.names = FALSE)
