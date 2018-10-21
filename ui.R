suppressPackageStartupMessages({
  library(shiny)
  library(highcharter)
  library(htmlwidgets)
  library(shinycssloaders)
  library(gentelellaShiny)
  library(shinyWidgets)  
})

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
        "Tab 1",
        tabName = "tab1", 
        icon = "bar-chart"
      )
    )
  ),
  body = gentelellaBody(
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
          column(
            width = 4,
            align = "center",
            selectizeInput(
              "inputTickers", 
              "Select ETFs",
              choices = c("SPY", "EFA", "AGG", "HYG", "EEM"), 
              selected = c("SPY", "AGG"), 
              multiple = TRUE),
            dateRangeInput(
              "portOptDates",
              "Select Dates",
              start = '2009-01-01',
              end = Sys.Date() - 1
            ),
            uiOutput("upperBound"),
            uiOutput("lowerBound")
          ),
          column(
            width = 8,
            align = "center",
            box(title = "Optimization",
                subtitle = "Mean Vairance",
                width = 12,
                highchartOutput("highchart") %>% withSpinner()
            ),
            box(title = 'Portfolio Returns',
                width = 12,
                highchartOutput("cumReturn") %>% withSpinner()),
            box(title = "Portfolio Drawdown",
                width = 12, 
                highchartOutput("portDrawdowns") %>% withSpinner()),
            box(title = "Return Box Plot",
                width = 12, 
                highchartOutput("portBoxPlots") %>% withSpinner()),
            box(title = "Return Box Plot",
                width = 12, 
                selectInput("distScatterPort", "Select Portfolio", selected = 1, choices = c(1,2,3,4,5)),
                highchartOutput("portDistScatter") %>% withSpinner())
          )  
        )
      )
    )
  ),
  footer = gentelellaFooter()
)