#-------------------------#
# Load required libraries #
#-------------------------#

suppressPackageStartupMessages({
  library(shiny)
  library(highcharter)
  library(htmlwidgets)
  library(shinycssloaders)
  library(gentelellaShiny)
  library(shinyWidgets)  
  library(rintrojs)
})

#----------------#
# Set UI Options #
#----------------#

options(spinner.color = "#1ABB9C")

#-------------#
# Gen UI HTML #
#-------------#

gentelellaPage(
  title = "VisualMarkets.io",
  navbar = gentelellaNavbar(
    navbarItems = notif(
      id = "menunotif",
      icon = "envelope-o",
      status = "primary",
      expanded = FALSE,
      lapply(X = 1:5, FUN = function(i) {
        notifItem(
          title = "John Doe",
          date = "3 min ago",
          img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
          "Film festivals used to be do-or-die moments
          for movie makers. They were where..."
        )
      })
      )
    ),
  sidebar = gentelellaSidebar(
    site_title = 
      shiny::HTML(paste(shiny::icon("line-chart"),
                        "VisualMarkets")),
    uiOutput("profile"),
    sidebarDate(),
    sidebarMenu(
      sidebarItem(
        "Portfolio Optimization",
        tabName = "portOptTab", 
        icon = "pie-chart"
      )
    )
  ),
  body = gentelellaBody(
    tabItems(
      tabItem(
        tabName = "portOptTab",
        fluidRow(
          column(
            width = 4,
            align = "center",
            selectizeInput(
              "inputTickers", 
              "Select ETFs",
              choices = c("SPY", "EFA", "AGG", "HYG", "EEM", "IWO"), 
              selected = c("SPY", "AGG"), 
              multiple = TRUE),
            dateRangeInput(
              "portOptDates",
              "Select Dates",
              start = '2009-01-01',
              end = Sys.Date() - 1
            ),
            uiOutput("upperBound"),
            uiOutput("lowerBound"),
            selectInput("timePeriod",
                        "Periodicity",
                        choices = c("Daily"   = 255,
                                    "Weekly"  = 52,
                                    "Monthly" = 12,
                                    "Yearly" = 1),
                        selected = 52)
          ),
          column(
            width = 8,
            align = "center",
            box(title = "Optimization",
                subtitle = "Mean Vairance",
                width = 12,
                highchartOutput("highchart") %>% withSpinner()
            ),
            tags$div(
              id = 'portReturns',
              box(title = 'Portfolio Returns',
                  width = 12,
                  highchartOutput("cumReturn") %>% withSpinner())              
            ),

            box(title = "Portfolio Drawdown",
                width = 12, 
                highchartOutput("portDrawdowns") %>% withSpinner()),
            box(title = "Return Box Plot",
                width = 12, 
                highchartOutput("portBoxPlots") %>% withSpinner()),
            box(title = "Return Box Plot",
                width = 12, 
                dropdownMenu = 
                  dropdownButton(
                  size = 'sm',
                  circle = TRUE, 
                  status = "primary", 
                  icon = icon("gear"),
                  tags$h3("List of Inputs"),
                  selectInput("distScatterPort", 
                              "Select Portfolio", 
                              width = '250px',
                              selected = 1, 
                              choices = c(1,2,3,4,5))),
                highchartOutput("portDistScatter") %>% withSpinner())
          )  
        )
      )
    )
  ),
  footer = gentelellaFooter()
)