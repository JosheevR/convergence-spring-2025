# Read GDP per capita by USD data
gdp_usd_data <- read.csv("data/gdp-usd.csv")

gdp_usd_data <- subset(gdp_usd_data, select = -c(Country.Code, Indicator.Name, Indicator.Code))
gdp_usd_data <- gdp_usd_data[, !(names(gdp_usd_data) %in% c(paste0("X", 1960:2010), paste0("X", 2020:2024)))]


