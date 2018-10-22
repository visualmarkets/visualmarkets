#----------------#
# Source Helpers #
#----------------#

source("VisualMarketsTheme.R")
source("UtilityHelpers.R")

#----------#
# Laod SIT #
#----------#

# library(curl)
# con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
# source(con)
# close(con)

#----------------------#
# Set Options & Future #
#----------------------#

options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.auto.assign"   = FALSE)
options("getSymbols.warning4.0"    = FALSE)

#-------------------------------#
# Portfolio Optimization Inputs #
#-------------------------------#

MeanReturns <- 
  function(..privateData = XtsDataFltr()){
    ..privateData %$% 
      map(1:ncol(returns), 
          function(x){
            name <- names(returns[,x])
            c(mean(returns[,x])) %>% set_names(name)
          }) %>% 
      reduce(function(x,y){c(x,y)})
  }

RiskStats <-
  function(..privateData = XtsDataFltr()){
    ..covData <- ..privateData %$% returns %>% cov()
    ..corData <- ..privateData %$% returns %>% cor()
    return(list(covData = ..covData,
                corData = ..corData))
  }

OptStats <- function(..returnData = 
                       XtsDataFltr() %$% 
                       returns,
                     ..nportfolios = 5,
                     ..lowerBound = 0, 
                     ..upperBound = 1,
                     ..periodicity = 52){
  
  ..secStats <- create.historical.ia(
    hist.returns = ..returnData,
    annual.factor = ..periodicity
  )
  
  ..constraints <- new.constraints(..secStats$n, 
                                   lb = ..lowerBound, 
                                   ub = ..upperBound)
  ..constraints <- add.constraints(rep(1, ..secStats$n), 1, type = '=', ..constraints)
  ..optStats <- portopt(..secStats, ..constraints, nportfolios = ..nportfolios)
  
  return(list(secStats = ..secStats, optStats = ..optStats))
  
}

FrontierOptHC <-
  function(..privateData = OptStats()[[2]], 
           ..secStats = OptStats()[[1]]){
    require(highcharter)
    ..frontierTbl <- 
      data.frame(return = ..privateData$return, 
                 risk = ..privateData$risk, 
                 category = 'frontier', 
                 type = 'line', 
                 stringsAsFactors = FALSE) %>%
      bind_rows(
        data.frame(return   = ..secStats$expected.return, 
                   risk     = ..secStats$risk, 
                   category = ..secStats$symbols, 
                   type     = 'scatter', 
                   stringsAsFactors = FALSE)
      )
    
     ..symbols <- c('frontier', ..secStats[['symbols']])
     
     ..hcColors <- 
       tibble(series = ..symbols,
              color = hc_theme_vm()[['colors']][1:length(..symbols)]
       )
    
    ..frontierOptHcData <- 
      ..frontierTbl %$%
      map(
        unique(category),
        function(x){
          ..mapValues <- filter(., category == x)
          list(
            name  = x,
            id    = glue("id_{x}"),
            color = ..hcColors %>% filter(series == x) %>% pull(color),
            type  = ..mapValues$type %>% unique(),
            yAxis = 0,
            data  = data.frame(x = ..mapValues$risk,
                               y = ..mapValues$return,
                               round = round(..mapValues$return*100, 1)
                    ) %>% 
                      list_parse()
          )  
        }  
      )    
    
    ..portOptHcData <- 
      ..privateData %$%
      data.frame(.[['weight']], risk = .[['risk']]) %$%
      map(
        names(.)[-which(names(.) == 'risk')],
        function(x){
          list(name = x,
               type = 'area',
               linkedTo = glue("id_{x}"),
               color = ..hcColors %>% filter(series == x) %>% pull(color),
               stacking = 'normal',
               yAxis = 1,
               data = data.frame(x = .[['risk']], 
                                 y = .[[x]],
                                 round = round(.[[x]] * 100, 1)) %>% list_parse() 
          )
        }
      )
    
    ..frontierOptHcData <- 
      append(..frontierOptHcData,
             ..portOptHcData)
    
    highchart() %>% 
      hc_add_theme(hc_theme_vm()) %>%
      hc_yAxis_multiples(
        create_yaxis(2, turnopposite = TRUE)
        #list(name = list(text = 'Asset Return')),
        #list(name = list(text = "Allocation"), opposite = FALSE)
      ) %>%
      hc_plotOptions(
        line = 
          list(marker = list(enabled = FALSE),
               dataLabels = list(enabled = TRUE,
                                 formatter = JS("function(){return Math.round(this.y*100) + '%'}"))),
        scatter = 
          list(dataLabels = list(enabled = TRUE,
                                 formatter = JS("function(){return this.series.name}"))),
        area = 
          list(marker = list(enabled = FALSE)),
        stickyTracking = FALSE,
        findNearestPointBy = 'xy') %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.round}%</b><br/>') %>%
      hc_add_series_list(..frontierOptHcData) 

  }

#--------------------#
# Under Construction #
#--------------------#

PortCumRet <- 
  function(weights = OptStats()[[2]]$weight,
           rtn     = XtsDataFltr() %$% returns){
    map(
      1:nrow(weights),
      function(x){
        Return.portfolio(rtn, weights = weights[x,], rebalance_on = 'years')
      }
    ) %>% 
      reduce(function(x, y){
        merge(x , y, all = TRUE)
      })  
  }

PortCumRetExh <-
  function(portCumRet = PortCumRet()){
    
    ..cumPlotData <- 
      PortCumRet() %>%
      to.weekly(OHLC = FALSE) %$%
      map(
        names(.),
        function(x){
          list(
            name = x,
            compare = 'percent',
            data = data.frame(x = index(.) %>% ToJsDate(), 
                              y = .[,x] %>% as.numeric() %>% `+` (1) %>% cumprod()
            ) %>% 
              mutate(ttValue = round(y * 100, 1)) %>% 
              list_parse())
        }
      )
    
    highchart() %>% 
      hc_add_theme(hc_theme_vm()) %>%
      hc_rangeSelector(enabled = TRUE) %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.ttValue}</b><br/>') %>%
      hc_plotOptions(line = 
                       list(marker = list(enabled = FALSE))) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_add_series_list(..cumPlotData)
    
  }

PortDrawdownExh <- 
  function(portCumRet = PortCumRet()){
    
    ..drawPlotData <- 
      PortCumRet() %>%
        to.weekly(OHLC = FALSE) %>%
        Drawdowns() %$%
        map(
          names(.),
          function(x){
            list(
              name = x,
              data = data.frame(x = index(.) %>% ToJsDate(), 
                                y = .[,x] %>% as.numeric()
              ) %>% 
                mutate(ttValue = glue("{round(y*100, 1)}")) %>% 
                list_parse())
          }
        )  
      
    highchart() %>% 
      hc_add_theme(hc_theme_vm()) %>%
      hc_rangeSelector(enabled = TRUE) %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.ttValue}</b><br/>') %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_add_series_list(..drawPlotData)
    
  }

PortBoxPlots <- 
  function(..portReturns = PortCumRet() %>% to.monthly(OHLC = FALSE)){
    ..boxPlotData <-  
      ..portReturns %>% 
        xts_tbl() %>% 
        gather(Variable, Value, -date)
    
    hcboxplot(x     = ..boxPlotData$Value, 
              var   = ..boxPlotData$Variable,
              name  = "Length", 
              color = "#2980b9") %>%
      hc_tooltip(shared = FALSE,
                 pointFormat = '<span style="color:{point.color}">●</span> {point.pointDate}: <b>{point.y}</b><br/>')
  }

PortRetrunDist <- 
  function(..portCumRet = PortCumRet() %>% 
                            to.monthly(OHLC = FALSE), 
           ..portfolio = 1){
    
    ..retSd      <- sd(..portCumRet[,..portfolio])
    ..retDen     <- density(..portCumRet[,..portfolio]) # Return Density
    ..portCumRet <- ..portCumRet[,..portfolio]
    ..portName   <- names(..portCumRet)
    
    highchart() %>%
      hc_add_theme(hc_theme_vm()) %>%
      hc_xAxis(list(name = 'test')) %>%
      hc_yAxis_multiples(
        list(name = 'yAxis0'), 
        list(name ='yAxis1', 
             type = 'linear',
             visible = FALSE,
             #type = 'datetime',
             opposite = TRUE)) %>%
      hc_add_series(
        name = glue("{..portName} Area"),
        type = 'area',
        xAxis = 0,
        yAxis = 0,
        fillOpacity = 0.1,
        zoneAxis = 'x',
        # zones =
        #   list(
        #     list(value = ..retSd * -1, color = 'red', fillColor = 'red'),
        #     list(value = 0, color = 'yellow', fillColor = 'yellow'),
        #     list(value = ..retSd * 1, color = 'lightgreen', fillColor = 'lightgreen'),
        #     list(value = max(..retDen[[2]]), color = 'green', fillColor = 'green')
        #   ),
        data = data.frame(
          x = ..retDen[[1]],
          y = ..retDen[[2]] / sum(..retDen[[2]])
        ) %>% list_parse()
      ) %>% 
      hc_add_series(
        name = glue("{..portName} Scatter"),
        type = 'scatter',
        marker = list(radius = 2),
        xAxis = 0,
        yAxis = 1,
        data = data.frame(x = ..portCumRet %>% as.numeric(),
                          y = jitter(rep(1, length(..portCumRet)), 
                                     amount = 0, 
                                     factor = ..retDen[[2]])
                          ) %>% list_parse()
      )
  }

