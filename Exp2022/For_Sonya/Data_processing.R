# Обработка данных учета птиц

library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)


dat <- read_excel("Data/Учет морских птиц_Лувеньга (ноч 01.07 ; день 04.07).xls", sheet = "Лист1", na = "NA")
str(dat)

dat$Date2 <- as.POSIXct(paste(dat$Date_2, " ", dat$Hour,":00", sep = ""), format = "%Y-%m-%d %H:%M" )


tide <- read_excel("Data/tides.xlsx", na = "NA")
tide$H <- as.numeric(tide$H)

  
tide$Date2 <- as.POSIXct(tide$Date, format = "%d.%m.%Y %H:%M")

tide$H2 <- tide$H * tide$Factor

str(tide)




dat_tide <- merge(dat, tide)


dat_tide %>% filter(Pattern == "Feeding") %>% 
  ggplot(., aes(x = H2, y = N)) + geom_point() + facet_wrap(~Species) 


dat_tide %>% filter(Pattern == "Rest") %>% 
  ggplot(., aes(x = H2, y = N)) + geom_point() + facet_wrap(~Species) 


dat_tide %>% filter(Pattern == "Moving") %>% 
  ggplot(., aes(x = H2, y = N)) + geom_point() + facet_wrap(~Species, scales = "free_y") 
