#-------------------------#
# Load required libraries #
#-------------------------#

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
  library(furrr)
  library(glue)
  library(SIT)
})

#----------------#
# Source Helpers #
#----------------#

source("helpers/PortOptHelpers.R")
source("helpers/UtilityHelpers.R")
source("helpers/VisualMarketsTheme.R")

#-----------------------#
# Create Execution Plan #
#-----------------------#

plan(multicore, workers = 6)

#--------------------#
# Start Shiny Server #
#--------------------#

shinyServer(
  function(input, output) {

    #--------------------#
    # Reactive Functions #
    #--------------------#
        
    GetReturns <- 
      reactive({
        GatherPrices(..tickers = input$inputTickers,
                     ..periodicity = input$timePeriod)
      }) %>%
        debounce(500)
       
    FtrDates <- 
      reactive({
        ..inputDates <- input$portOptDates
        GetReturns() %>% 
          XtsDataFltr(start = ..inputDates[1],
                      end   = ..inputDates[2])
      }) 
    
    PortOpt <- 
      reactive({
        # Required Reactive Values
        req(input$optLowerBound)
        req(input$optUpperBound)
        
        FtrDates() %$%
          returns %>% 
          OptStats(
            ..lowerBound  = input$optLowerBound / 100,
            ..upperBound  = input$optUpperBound / 100,
            ..periodicity = input$timePeriod %>% as.numeric()
          )
      })
    
    PortCumRet_reactive <- 
      reactive({
        PortCumRet(
          weights = PortOpt()[['optStats']]$weight,
          rtn     = FtrDates() %$% returns
        )
      })
    
    #-------------------#
    # Render UI Outputs #
    #-------------------#
    
    output$upperBound <-
      renderUI({
        ..tickerLen <- GetReturns() %$% returns %>% ncol()
        sliderInput(
          "optUpperBound",
          "Set Upper Bound",
          min   = (100 / ..tickerLen) %>% ceiling(),
          max   = 100,
          value = 100,
          step  = 5)
      })
    
    output$lowerBound <-
      renderUI({
        ..tickerLen <- GetReturns() %$% returns %>% ncol()
        sliderInput(
          "optLowerBound",
          "Set Lower Bound",
          min   = 0 ,
          max   = (100 / ..tickerLen) %>% floor(),
          value = 0,
          step  = 5)
      })
    
    #-------------------------#
    # Specify Output Bindings #
    #-------------------------#
        
    output$highchart <-
      renderHighchart({
        PortOpt() %$%
          FrontierOptHC(optStats, 
                        secStats)
      })
    
    output$cumReturn <- 
      renderHighchart({
        PortCumRet_reactive() %>%
          PortCumRetExh()
      })
    
    output$portDrawdowns <-
      renderHighchart({
        PortCumRet_reactive() %>%
          PortDrawdownExh()
      })
    
    output$portBoxPlots <- 
      renderHighchart({
        PortCumRet_reactive() %>%
          PortBoxPlots()
      })
    
    output$portDistScatter <- 
      renderHighchart({
        PortCumRet_reactive() %>%
          PortRetrunDist(..portfolio = input$distScatterPort %>% as.numeric())
      })
    
    #---------------------#
    # Reactive Priorities #
    #---------------------#
    
  }
)
