library(dplyr)
library(tidyr)

# Read GDP per capita data
gdp_pc_usd <- read.csv("data/gdp-pc-usd.csv")
gdp_pc_usd <- read.csv("data/gdp-pc-ppp.csv")

# Regress on data
regression_usd <- lm((lag_gdp - gdp)~ gdp, data = gdp_pc_usd)
regression_ppp <- lm((lag_gdp - gdp)~ gdp, data = gdp_pc_ppp)

# Print summary statistics of regression
summary(regression_usd)
summary(regression_ppp)