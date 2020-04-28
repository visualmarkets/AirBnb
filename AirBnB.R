library(httr)
library(rvest)
library(glue)
library(dplyr)
library(purrr)
library(magrittr)
library(tibble)
library(data.table)
library(curl)
library(fst)

# Base url for insideairbnb
baseUrl <- "http://insideairbnb.com/get-the-data.html"

# Specify cache directory
cacheDir <- "cache/AirBnb"

# Create cache directory
dir.create(cacheDir, showWarnings = FALSE)

# List current cities
cacheDirs <- list.dirs(cacheDir, recursive = FALSE, full.names = FALSE)

# Get lost of table nodes
tables <- read_html(baseUrl) %>% html_nodes("table")

# Create table of links cities and dates
airBnbLinks <-
  tables %>% 
    map_df(
      function(x){

        htmlTable <- x %>% html_table()
        
        # Code to be deleted if process runs correctly
        # city <- htmlTable %>% distinct(city = `Country/City`) %>% pull(city)
        # print(glue("Gathering data for {city}"))
        
        hrefs <- x %>% 
          html_nodes("tbody") %>% 
          html_children() %>% 
          html_nodes("td > a") %>% 
          html_attr("href")
        
        tibble(
          city      = htmlTable[["Country/City"]],
          date      = htmlTable[["Date Compiled"]],
          file_name = htmlTable[["File Name"]],
          links     = hrefs
        ) %>% 
        filter(file_name %like% ".gz") %>% 
        mutate(date = as.Date(date, format = "%d %B, %Y"))
      
      }
    )

# List all files
allDirs <- list.files(cacheDir, recursive = TRUE)

# Vector of new files to be downloaded
newFiles <- airBnbLinks %>% filter(!glue_data(.,"{city}/{date}/{file_name}") %in% allDirs)

# Walk through and download new files
pwalk(
  list(
    city     = newFiles$city,
    fileName = newFiles$file_name,
    date     = newFiles$date,
    link     = newFiles$links
  ),
  safely({
    function(city, fileName, date, link){
      
      Sys.sleep(5L)
      
      print(glue("Downloading {city} {date} {fileName}"))
    
      dir.create(glue("{cacheDir}/{city}"), showWarnings = FALSE)  
      
      fileDir <- glue("{cacheDir}/{city}/{date}")
      dir.create(fileDir, showWarnings = FALSE)
      curl_download(link, destfile = glue("{fileDir}/{fileName}"))
    }            
  })
)
