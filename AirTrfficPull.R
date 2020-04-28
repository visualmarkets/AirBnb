library(httr)
library(rvest)
library(glue)
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

user    <- "PaddyBeefCakes"
pass    <- "zakbyd-7sahvo-cuPjon"
baseUrl <- glue("https://{user}:{pass}@opensky-network.org/api")

today      <- Sys.Date()
todayYear  <- lubridate::year(today)
todayMonth <- lubridate::month(today)
yesterday  <- lubridate::day(today) - 1

hours <- seq.POSIXt(ISOdate(2020,04, 01, 0), ISOdate(todayYear, todayMonth, yesterday, 0), by = "2 hour")

#---------------------#
# Set and Create dirs #
#---------------------#

cacheDir <- "cacheFlights"
dir.create(cacheDir, showWarnings = FALSE)
dir.create(glue("{cacheDir}/flights"), showWarnings = FALSE)

#--------------------#
# Gather icao24 Data #
#--------------------#

# rawIcao24Data <- jsonlite::read_json("https://opensky-network.org/api/metadata/aircraft/list?n=507115&p=1&sc=&sd=")

# icao24Data <- rawIcao24Data[[1]] %>% map(safely({function(x){tidyr::replace_na(x) %>% unlist() %>% t()}})) %>% map_df(~as_tibble(.x$result))
  
#-----------------#
# Pull Covid Page #
#-----------------#

# rawHtml <- read_html("https://opensky-network.org/datasets/covid-19/")
# 
# htmlLinks <- 
#   rawHtml %>% html_nodes(css = "table > tbody > tr") %>% 
#   html_node(css = "a") %>% 
#   html_attr("href") %>% 
#   str_remove("./") %>% 
#   .[str_detect(., ".csv")]
# 
# walk(
#   htmlLinks,
#   function(x){
#     url <- glue("https://opensky-network.org/datasets/covid-19/{x}")
#     curl::curl_download(url, glue("{cacheDir}/flights/{x}"))    
#   }
# )

#---------------#
# Pull API Data #
#---------------#

hoursTbl <-
  tibble(
    from = lag(hours, 1) %>% as.numeric(),
    to   = hours %>% as.numeric()
  ) %>% 
  na.omit()

rawFlights <-
  pmap(
    hoursTbl %$%
    list(
      from = from %>% as.numeric(),
      to   = to %>% as.numeric()
    ), 
    safely({
      function(from, to){
        
        Sys.sleep(15L)
        
        resp <- GET(glue("{baseUrl}/flights/all"),
                    query = list(begin = from, end = to))
      
        print(glue("Response: {status_code(resp)}"))
        print(glue("Number of flights {length(resp %>% content())}"))
        
        if(status_code(resp) == 502){
          resp <- GET(glue("{baseUrl}/flights/all"),
                      query = list(begin = from, end = to))
        }
        
        assertthat::assert_that(
          status_code(resp) == 200,
          msg =glue("{status_code(resp)}")
        )

        resp %>% content()
        
      }
    })
  )

# Tabularize nested json data
newFlights <- 
  rawFlights %>% map(~.x$result) %>% 
    map_df(
      function(x){
        map_df(
          x, 
          function(y){
            tidyr::replace_na(y) %>% unlist() %>% map_df(~.x)
          }
        )
      }
    ) %>% 
  mutate(
    callsign     = str_trim(callsign, side = "both"),
    firstSeen    = as.POSIXct(as.numeric(firstSeen), origin = "1970-01-01", tz = "UTC"),
    lastSeen     = as.POSIXct(as.numeric(lastSeen), origin = "1970-01-01", tz = "UTC"),
    day          = as.Date(lastSeen),
    airline_code = str_sub(callsign, 1L, 3L)
  ) %>% 
  rename(origin = estDepartureAirport)

# Set names for export file
fromDate <- format(as.Date(min(newFlights$firstSeen), origin = "1970-01-01"), format = "%Y%m%d")
toDate   <- format(as.Date(max(newFlights$lastSeen), origin = "1970-01-01"), format = "%Y%m%d")

# Write new dataset
write_csv(newFlights, glue("cacheFlights/flights/flightlist_{fromDate}_{toDate}.csv"))
