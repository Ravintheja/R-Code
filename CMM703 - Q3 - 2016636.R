library(dplyr)
setwd("E:\\IIT\\Data Analysis\\workspace\\Cw")
# dir()

covid_data = data.frame(read.csv("owid-covid-data2.csv"))

#Asia Cases Per Million
asia_cpm = covid_data %>% select(continent, date, total_cases_per_million)
asia_cpm = asia_cpm %>% filter(continent == 'Asia', !is.na(total_cases_per_million))
asia_cpm = asia_cpm %>% group_by(date) %>% summarise(cpm = sum(total_cases_per_million))

#India Cases Per Million
ind_cpm = covid_data %>% select(iso_code, date, total_cases_per_million)
ind_cpm = ind_cpm %>% filter(iso_code == 'IND', !is.na(total_cases_per_million))
ind_cpm = ind_cpm %>% group_by(date) %>% summarise(cpm = sum(total_cases_per_million))

#Pakistan Cases Per Million
pk_cpm = covid_data %>% select(iso_code, date, total_cases_per_million)
pk_cpm = pk_cpm %>% filter(iso_code == 'PAK', !is.na(total_cases_per_million))
pk_cpm = pk_cpm %>% group_by(date) %>% summarise(cpm = sum(total_cases_per_million))

#Sri Lanka Cases Per Million
sl_cpm = covid_data %>% select(iso_code, date, total_cases_per_million)  
sl_cpm = sl_cpm %>% filter(iso_code == 'LKA', !is.na(total_cases_per_million))
sl_cpm = sl_cpm %>% group_by(date) %>% summarise(cpm = sum(total_cases_per_million))
str(sl_cpm)

#Asia Dataframe
asia_date = c(asia_cpm[1])
asia_date <- as.Date(asia_date$date)
asia = data.frame(x = asia_date, y = asia_cpm['cpm'])

#India Dataframe
ind_date = c(ind_cpm[1])
ind_date <- as.Date(ind_date$date)
ind = data.frame(x = ind_date, y = ind_cpm['cpm'])

#Pakistan Dataframe
pak_date = c(pk_cpm[1])
pak_date <- as.Date(pak_date$date)
pak = data.frame(x = pak_date, y = pk_cpm['cpm'])

#SL Dataframe
sl_date = c(sl_cpm[1])
sl_date <- as.Date(sl_date$date)
sl = data.frame(x = sl_date, y = sl_cpm['cpm'])


plot(sl, main="Covid 19 - SL vs Asia",
                xlab="Date", ylab="Cases Per Million", col = "#264653", pch = 1)
points(ind, col = "#e9c46a", pch = 1)
points(pak, col = "#2a9d8f", pch = 1)
points(asia, col = "#f4a261", pch = 1)

legend("bottomright",
  legend = c("Sri Lanka", "India", "Pakistan", "Asia"),
  col = c("#264653", "#e9c46a", "#2a9d8f", "#f4a261"),
  pch = c(2,2,2,2)
)
