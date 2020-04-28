library(future)
library(promises)
library(storr)

plan(multisession, workers = 3L)

path <- tempfile("storr_")

st <- driver_rds(path) %>% storr()

inputs <- "testing"

hashName <- digest::digest(inputs)

(function(){
  if(hashName %in% st$list()){
    return(st$get(hashName))
  }
  
  future({mean(rnorm(50000000))}) %...>% 
    {
      st$set(hashName, .)
      .
    } %...>%
    {
      test <<- .
    } %...>%
    {print(st$list())}
})()

st$get(hashName)
