library(dplyr)
library(purrr)
library(glue)
library(httr)
library(rvest)
library(tibble)

apiKey <- "X1-ZWz1eqi6jtdo97_9zlkj"

zillowApi <- "http://www.zillow.com/webservice/GetSearchResults.htm"

GetZips <-
  function(){
    ## Download source file, unzip and extract into table
    ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
    temp <- tempfile()
    download.file(ZipCodeSourceFile , temp)
    ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t", colClasses = "character")
    unlink(temp)
    names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
                        "AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
                        "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy") 
    ZipCodes$zip
  }

# zipCodes <- GetZips()

states <- state.abb

zipPrices <- 
  map(
    states, 
    function(state){
      
      print(glue("Gathering data for {state}"))
      
      Sys.sleep(15)
      
      rawHtml <- 
        GET("http://www.zillow.com/webservice/GetRegionChildren.htm", 
            query = list(
              `zws-id` = apiKey,
              state = state,
              childtype = "zipcode")
        ) %>% 
        content() %>% 
        html_nodes(css = "response")
      
      rawHtml %>% 
        html_nodes(css = "region") %>%
        map(
          function(x){
            
            print(glue("Gathering data for zip: {x}"))
            
            name <- x %>% html_nodes(css = "name") %>% html_text()
            
            zindex <- x %>% html_nodes(css = "zindex") %>% html_text()
            if(length(zindex) == 0){zindex <- NA}
            
            currency <- x %>% html_nodes(css = "zindex") %>% html_attr("currency")
            if(length(currency) == 0){currency <- NA}
            
            tibble(
              state = state,
              id = x %>% html_nodes(css = "id") %>% html_text(),
              name = name,
              zindex = zindex,
              currency = currency
            )
          }
        )
    }
  ) %>% 
  reduce(function(x, y){bind_rows(x, y)})

zipPrices %>% filter(name == "03801")


priceData <-
  map(
    zipCodes,
    safely({
      function(x){
        
        print(x)
        
        # Sys.sleep(5)
        
        queryParams <- list(
          `zws-id` = apiKey,
          address = "1",
          citystatezip = x
        )

        r <- GET(zillowApi, query = queryParams)

        localInfo <- content(r) %>% html_node(css = "result") %>% html_nodes(css = "localRealEstate")
        
        metaInfo <- localInfo %>% html_children() %>% html_attrs() %>% .[[1]]
        
        localPrice <- localInfo %>% html_node(css = "zindexValue") %>% html_text()
        priceChange <- localInfo %>% html_node(css = "zindexOneYearChange") %>% html_text()
        
        overviewLinks <- localInfo %>% html_nodes(css = "overview") %>% html_text()
        
        # Code to limit api calls
        limitWarning <- 
          content(r) %>% html_node(css = "message") %>% html_node(css = "limit-warning") %>%
          length()
        
        if(limitWarning == 1L){
          print("Limit hit sleeping one hour")
          Sys.sleep(60 * 60)
        }
        
        tibble(
          zip_code = x,
          date     = Sys.Date(),
          name     = metaInfo[["name"]],
          id       = metaInfo[["id"]],
          type     = metaInfo[["type"]],
          price    = localPrice,
          change   = priceChange,
          links    = overviewLinks
        )    
      }    
    })
  ) %>%
  map_df(~.x$result)

priceData[2,] %>% pull(links)




rawHtml <-
  priceData[2,] %>% 
  pull(links) %>% 
  read_html()

rawHtml %>% 
  html_nodes(css = "body") %>% 
  html_nodes(css = ".data-download-links.sub-module-actions") %>% 
  html_nodes(css = "a.time-series-link") %>% 
  html_attr("href")

cookies <- 
  html_session("https://www.zillow.com") %>% 
  httr::cookies() %$%
  list(
    x = name,
    y = value
  ) %>%
  pmap(~{.y %>% set_names(.x)}) %>% 
  reduce(function(x, y){c(x, y)})


GET("https://www.zillow.com/ajax/homevalues/data/timeseries.json?r=21534&m=zhvi_plus_forecast&dt=6",
    user_agent("rickypickering@mac.com"),
    set_cookies(cookies)) %>% 
  content()
