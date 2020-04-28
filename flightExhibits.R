
library(highcharter)
library(dplyr)
library(fst)

system.time({
  allFlights <- read.fst("cache/allFlight.fst")  
})

#----------#
# Exhibits #
#----------#

allFlights %>% 
  group_by(day, country) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  filter(country %in% c("US", "IT", "CN", "GB", "JP", "DE")) %>% 
  mutate(day = as.numeric(day) * 1000 * 60 * 60 * 24) %>% 
  hchart("line", hcaes(x = day, y = count, group = country)) %>% 
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(week = '%d-%m-%Y'))


allFlights %>% 
  group_by(day) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(day = as.numeric(day) * 1000 * 60 * 60 * 24) %>% 
  hchart("line", hcaes(x = day, y = count)) %>% 
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(week = '%d-%m-%Y'))


