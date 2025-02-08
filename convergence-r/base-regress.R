library(dplyr)
library(tidyr)

gdp_pc_usd <- read.csv("data/gdp-pc-usd.csv")

regression <- lm((lag_gdp - gdp)~ gdp, data = gdp_pc_usd)
summary(regression)