library(shiny)
library(shinyjs)
library(shinyBS)
library(bizdays)

#analysis packages
library(dplyr)
library(tidyr)
library(quantmod)
library(fTrading)
library(TTR)

#plotting packages
library(rbokeh)

#variable for adjusting dateinput size
cssSelect <- '~ .selectize-control .selectize-input {font-size: 11px !important; min-height: 24px !important;height: 24px;padding-top:0px; !important;} .selectize-dropdown {font-size: 11px !important}'

### start input UI ####
fluidPage(

  fluidRow(
    
    column(3, align='center', style="width:225px",
           useShinyjs(),
           shinyBS:::shinyBSDep,
           
           tags$style(HTML('.form-group, .selectize-control {margin-bottom: 1px; margin-top: 1px;}')),

           h4(HTML('<div style="text-align:center"><font size="2">Strategy Backtesting</font></div>')),

           wellPanel(
             tags$style(HTML('.form-group, .selectize-control {margin-bottom: 1px; margin-top: 1px; padding-left: 1px; padding-right: 1px;}')),
             HTML('<font size="1">'),
             tags$style(HTML("input[type='text'] { font-size: 12px; text-align: center; height: 24px;}")),
             dateInput("date1", "Start Date:", value=as.character(format(Sys.Date()-3-365, format="%m/%d/%Y")), format='mm/dd/yyyy', width="100px"),
             dateInput("date2", "End Date:", value=as.character(format(Sys.Date()-3, format="%m/%d/%Y")), format="mm/dd/yyyy", width="100px"),
             tags$style(HTML(".datepicker {z-index:99999 !important;}")),
             tags$style(paste0("#stockInput", cssSelect)),
             selectInput("stockInput", "Select a Stock:",
                         c("SPY", "AAPL", "MSFT", "AMZN", "FB", "BRK-B",
                           "JNJ", "JPM", "XOM", "GOOG", "GOOGL", "BAC", "WFC",
                           "CVX", "HD", "UNH", "PG", "T", "V",
                           "PFE", "VZ", "INTC", "C", "CSCO", "CMCSA", "BA",
                           "KO", "DWDP", "PEP", "ABBV", "PM", "MRK", "DIS",
                           "ORCL", "WMT", "MA", "MMM", "NVDA", "IBM", "AMGN",
                           "MCD", "GE", "MO", "HON", "NFLX", "MDT", "TXN", "GILD",
                           "ABT", "SLB", "UNP", "BMY", "UTX", "AVGO", "QCOM", "ACN",
                           "CAT", "ADBE", "PYPL", "PCLN", "GS", "UBS", "UPS", "LOW",
                           "NKE", "TMO", "COST", "LMT", "LLY", "CVS", "CELG",
                           "SBUX", "MS", "CRM", "BIIB", "PNC", "NEE", "TWX", "CHTR",
                           "BLK", "CB", "COP", "AXP", "WBA", "EOG", "MDLZ", "FDX", "ANTM",
                           "BDX", "SCHW", "CL", "DHR", "AET", "GD", "AMT", "AGN", "AMAT",
                           "RTN", "BK", "OXY", "NOC",
                           "SPXL", "SPXS", "TMF", "TMV", "MCHI", "INDA", "EPHE"), width="100px"
                         ),
             div(style="display:inline-block;text-align: center; margin-top: 5px;",
                 actionButton("button1", label = HTML('<font size="1">Graph the Stock</font>'), icon = icon("spoon"))),
             style = "padding: 5px; margin: 10px;"
             ),
             
           tags$style(HTML('.form-group, .selectize-control {margin-bottom: 1px; margin-top: 1px;}')),
             
           wellPanel(
             style = "padding: 5px; margin: 10px;",
             tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
             tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
             tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
             
             ###ENTRY STRATEGY SETTINGS ### BEGIN
             p(strong("Entry Strategy Settings")),
             tags$style(paste0("#entryStrat", cssSelect)),
             selectInput("entryStrat", "Entry indicator:",
                         c("SMA", "RSI", "Stoch"), width="100px"
                         ),
             tags$style(paste0("#buyAtStart", cssSelect)),
             selectInput("buyAtStart", "Long at day one?",
                         c("Yes", "No"), width="100px"
             ),
             conditionalPanel(
               condition = "input.entryStrat == 'SMA'",
               sliderInput("enterSMAset", "SMA day setting to buy", 10, 200, 100, 10, width='125px')
               ),
             conditionalPanel(
               condition = "input.entryStrat == 'RSI'",
               sliderInput("enterRSIset", "Level of RSI to buy", 10, 90, 30, 5, width='125px')
               ),
             conditionalPanel(
               condition = "input.entryStrat == 'Stoch'",
               sliderInput("enterStochset", "Level of Stoch to buy", 0.10, 0.90, 0.20, 0.05, width='125px')
               )
             ),
           ###ENTRY STRATEGY SETTINGS ### END
           
           wellPanel(
             style = "padding: 5px; margin: 10px;",
             tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: darkred}")),
             tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: darkred}")),
             tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: darkred}")),
             
             ###EXIT STRATEGY SETTINGS ### BEGIN
             p(strong("Exit Strategy Settings")),
             tags$style(paste0("#exitStrat", cssSelect)),
             selectInput("exitStrat", "Exit indicator:",
                         c("SMA", "RSI", "Stoch"), width="100px"
                         ),
             conditionalPanel(
               condition = "input.exitStrat == 'SMA'",
               sliderInput("exitSMAset", "SMA day setting to sell", 10, 200, 100, 10, width='125px')
               ),
             conditionalPanel(
               condition = "input.exitStrat == 'RSI'",
               sliderInput("exitRSIset", "Level of RSI to sell", 10, 90, 70, 5, width='125px')
               ),
             conditionalPanel(
               condition = "input.exitStrat == 'Stoch'",
               sliderInput("exitStochset", "Level of RSI to sell", 0.10, 0.90, 0.80, 0.05, width='125px')
               )
             ),
             ###EXIT STRATEGY SETTINGS ### END
           wellPanel(fluidRow(
             align='center', 
             div(style="display:inline-block;text-align: center;",
                 disabled(actionButton("button2", label = HTML('<font size="1">Run Strategy</font>'), icon = icon("play"))))
            ), 
            style = "padding: 5px; margin: 10px;"
           )
           
    ),
    ### plot area
    
    column(9,
           
           br(),
           div(style="width:600px;", htmlOutput("text_1")),
           rbokehOutput("plot_1"),
           div(style="width:600px;", htmlOutput("text_2")),
           rbokehOutput("plot_2")
           
           )
    
  )
)

##### end of input UI#####