# Read data
log_gdp <- read.csv("data/gdp-usd.csv")
pop_gr <- read.csv("control-data/pop-gr.csv")
sav_rate <- read.csv("control-data/sav-rate.csv")
cap_form <- read.csv("control-data/capital-form.csv")

# Scatter Plots
plot(log_gdp$log_gdp, log_gdp$percent_growth, main="Log_GDP_PC vs. % change GDP_PC",
     xlab="Log_GDP_PC ", ylab="% change GDP_PC", pch=19)

plot(pop_gr$growth.rate, log_gdp$percent_growth, main="Log_GDP_PC vs. Pop_Growth_Rate",
     xlab="Pop_Growth_Rate ", ylab="% change GDP_PC", pch=19)

plot(sav_rate$savings.rate, log_gdp$percent_growth, main="Log_GDP_PC vs. Savings_Rate",
     xlab="Savings_Rate ", ylab="% change GDP_PC", pch=19)

plot(cap_form$gross.capital.formation, log_gdp$percent_growth, main="Log_GDP_PC vs. Gross_Capital_Form",
     xlab="Gross_Capital_Form ", ylab="% change GDP_PC", pch=19)