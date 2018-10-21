suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(quantmod)
  library(PerformanceAnalytics)
  library(highcharter)
  library(tbl2xts)
  library(future)
  library(promises)
  library(shiny)
})

plan(list(tweak(multiprocess, workers = 4L), tweak(multiprocess, workers = 4L)))

source("newFunctions.R")

promiseValue <-
  future({walk(1:20000,function(x){rnorm(x)})
          gatherPrices()$returns[1]})

promiseValue %...>% (function(x){walk(1:20000,function(x){rnorm(x)});print(mean(x))})

promiseValue %...>% (function(x){walk(1:20000,function(x){rnorm(x)});print(mean(x))})


promiseValue <-
  future({walk(1:20000,function(x){rnorm(x)})
    Sys.time()})

promise_all(
  list(mean, mean),
  function(x){
    promiseValue %...>%
      x
}) %...>%
  {print(.)}
