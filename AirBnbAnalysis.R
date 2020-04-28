library(dplyr)
library(purrr)
library(magrittr)
library(highcharter)
library(lubridate)
library(readr)
library(stringr)
library(forecast)
library(xts)
library(glue)

nightsPer  <- 3L
reviewRate <- 0.5
occupCap   <- 0.7

dataDirs <- list.dirs("cache/AirBnb", recursive = FALSE)

x <- dataDirs[15]
cityName <- str_remove(x, "cache/AirBnb/")

listings <- 
  list.files(x, full.names = TRUE) %>% 
  list.files(full.names = TRUE, pattern = "listings")

reviews <- 
  list.files(x, full.names = TRUE) %>% 
  list.files(full.names = TRUE, pattern = "reviews")

calendars <- 
  list.files(x, full.names = TRUE) %>% 
  list.files(full.names = TRUE, pattern = "calendar")

#-------------#
# Review Data #
#-------------#

reviewData <-
  map_df(
    reviews[length(reviews)], # Take the last file from review files
    function(x){
      read_csv(x) %>% mutate(file = x) 
    }
  ) %>% 
  mutate(
    city = str_sub(file, 7L, 14L),
    year_mon = zoo::as.yearmon(date),
    week = lubridate::week(date),
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) 

reviewSum <-
  reviewData %>% 
  filter(date >= "2015-01-01") %>% 
  group_by(year_mon, city) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(type = "reviews") 

listSum <-
  reviewData %>% 
  mutate(city = str_sub(file, 7L, 14L),
         year_mon = zoo::as.yearmon(date)) %>% 
  group_by(year_mon, listing_id) %>% 
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(type = "listing")


splitDate <- zoo::as.yearmon("2019-12-31")

reviewActual <- reviewSum %>% filter(year_mon > splitDate) %$%
  ts(count, end = c(lubridate::year(max(year_mon)), lubridate::month(max(year_mon))),
     frequency = 12)

trainData <- reviewSum %>% filter(year_mon <= splitDate) %$% 
  ts(count, end = c(lubridate::year(max(year_mon)), lubridate::month(max(year_mon))), frequency = 12)

forecast <- forecast(trainData, h = 2, level = 95)

hchart(forecast) %>% 
  hc_title(text = glue("Reviews for {cityName}")) %>% 
  hc_add_series(reviewActual, type = "line", name = "Actual Data") %>% 
  hc_xAxis(type = "datetime") %>% 
  hc_tooltip(shared = TRUE, split = FALSE) %>% 
  hc_add_theme(hc_theme_smpl())

#-----------------#
# Occupancy Model #
#-----------------#

availData <-
  map_df(
    listings,
    function(y){
      suppressMessages({
        read_csv(
          y,
          col_types          = cols_only(
            id               = col_integer(),
            last_scraped     = col_date(),
            city             = col_character(),
            country          = col_character(),
            price            = col_character(),
            # availability_365  = col_double(),
            number_of_reviews = col_integer(),
            # number_of_reviews_ltm = col_integer(),
            reviews_per_month = col_double(),
            minimum_nights = col_integer()
          )
        )
      })
    }
  ) 

testData <- 
  availData %>% mutate(date = zoo::as.yearmon(last_scraped)) %>% 
  left_join(listSum, by = c("id" = "listing_id", "date" = "year_mon")) 

testData %>% 
  filter(!is.na(count)) %>% 
  mutate(
    year = lubridate::year(last_scraped),
    month = lubridate::month(last_scraped),
    price = price %>% str_replace("\\$", "") %>% str_replace(",", "") %>% as.numeric(),
    avg_nights = case_when(minimum_nights > nightsPer ~ minimum_nights,
                          TRUE ~ nightsPer),
    avg_nights = nightsPer,
    occupancy_days = (count / reviewRate) * avg_nights,
    occupancy_rate = occupancy_days / 30,
    occupancy_rate = case_when(occupancy_rate > occupCap ~ occupCap, 
                               TRUE ~ occupancy_rate)
  ) %>% 
  group_by(date) %>% 
  summarize(value = mean(occupancy_rate, na.rm=T)) %>% 
  hchart("line", hcaes(x = date, y = value)) %>% 
  hc_yAxis(min = 0)

availData %>% 
  mutate(
    mon_year = zoo::as.yearmon(last_scraped),
    year = lubridate::year(last_scraped),
    month = lubridate::month(last_scraped)
  ) %>% 
  group_by(mon_year, country) %>% 
  summarize(count = n()) %>% 
  hchart("line", hcaes(x = mon_year, count, group = country))

#-------------------#
# Availability Code #
#-------------------#

cityData <-
  availData %>% 
  mutate(month = lubridate::month(last_scraped),
         year = lubridate::year(last_scraped)) %>% 
  group_by(month, year) %>% 
  summarize(
    avg_month = 1 - mean(availability_30, na.rm = TRUE) / 30,
    avg_two_month = 1 - mean(availability_60, na.rm = TRUE) / 60,
    avg_quarter = 1 - mean(availability_90, na.rm = TRUE) / 90,
    avg_year = 1 - mean(availability_365, na.rm = TRUE) / 365,
    count = n()
  ) %>% 
  ungroup() %>% 
  filter(count > 500) %>% 
  select(-count) %>% 
  tidyr::gather(measure, value, -month, -year) %>% 
  group_by(month, year) %>% 
  mutate(value = mean(value))

hchart(cityData, "column", hcaes(x = month, y = value, group = year))  

