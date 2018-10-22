#-------------------#
# Utility Functions #
#-------------------#

ToJsDate <-
  function(date = as.Date('2017-12-31')){
    date %>%
      as.Date() %>%
      as.numeric() %>%
      multiply_by(86400000)
  }

GatherPrices <-
  function(..tickers = c("SPY", "AGG", "EFA", "HYG", "EEM", "PGX", 'CWB', "PSP"),
           ..periodicity = "52"){
    ..periodConvert <-
      switch(..periodicity, 
             "255" = to.daily,
             "52"  = to.weekly,
             "12"  = to.monthly,
             "1"   = to.yearly)
    
    ..prices <-
      map(..tickers,
          safely({
            function(x){
              getSymbols(x, auto.assign = FALSE)[,6] %>%
                ..periodConvert(OHLC = FALSE) %>%
                `colnames<-` (x)
            }
          })
      ) %>%
      map(function(x){x$result}) %>%
      reduce(function(x, y){
        merge(x, y, all = TRUE)
      })
    ..returns <-
      ..prices %$%
      map(names(.),
          function(x){
            .[,x] %>%
              Return.calculate() %>%
              na.omit()
          }) %>%
      reduce(function(x, y){
        merge(x, y, all = TRUE)
      }) %>% 
      na.omit()
    return(list(prices  = ..prices,
                returns = ..returns))
  }

XtsDataFltr <- 
  function(..priceRetData = GatherPrices(), start = NULL, end = NULL){
    ..start <- 
      if(is.null(start)){..priceRetData %$% prices %>% index() %>% min()} else {start}
    ..end   <- if(is.null(end)){..priceRetData %$% prices %>% index() %>% max()} else {end}
    ..priceRetData %>%
      map(function(x){
        map(1:ncol(x),
            function(y){
              x[glue("{..start}/{..end}"), y]
            }
        ) %>% reduce(function(x,y){
          merge(x, y, all = TRUE)
        })
      })
  }
