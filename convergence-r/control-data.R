library(dplyr)
library(tidyr)

# Read control variables csv files
pop_growth_rate <- read.csv("control-data/RAW-pop-growth-rate.csv")

# Remove unneccessary columns and years
pop_growth_rate <- subset(pop_growth_rate, select = -c(Country.Code, Indicator.Name, Indicator.Code))
pop_growth_rate <- pop_growth_rate[, !(names(pop_growth_rate) %in% c(paste0("X", 1960:2012), paste0("X", 2020:2024)))]

# Omit rows with na values
pop_growth_rate <- na.omit(pop_growth_rate)

# Pivot to long format and clean year values
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

# Save as csv
write.csv(pop_growth_rate, "control-data/pop-gr.csv", row.names = FALSE)
