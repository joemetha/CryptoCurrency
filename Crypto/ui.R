library(shiny)
library(markdown)
library(shinythemes)

navbarPage("CryptoCurrency",theme = shinytheme("flatly"),
           tabPanel("Home",
                    fluidRow(
                      column(12
              
                             
                           ,
                             
                             fluidRow(
                               column(4,  mainPanel(width = 16,
                                 wellPanel(h4("This is a Cryptocurrency analysis website"), align = "center",h5("by scraping the data from https://coinmarketcap.com/", align = "center"),br()),
                                 wellPanel(
                                  radioButtons("plotType", "Selecting the CryptoCurrency graph",
                                              c("Bitcoin (BTC)"="BTC"
                                                , "Dash (DASH)"="DASH"
                                                , "Ethereum Classic (ETC)"="ETC"
                                                , "Ethereum (ETH)"="ETH"
                                                , "Litecoin (LTC)"="LTC"
                                                , "NEM (XEM)"="XEM"
                                                , "Ripple (XRP)"="XRP"
                                                , "Zcash (ZEC)"="ZEC"
                                                )
                                  )
                                 ),
                                 
                                 plotOutput("piePlot")
                               )
                                 
                               ),
                               column(width = 8,
                                      wellPanel( plotOutput("plot")),
                                      wellPanel(dataTableOutput('mytable'))
                                      
                                    )
                             )
                      )
                    )
           ),
           
           tabPanel("Forecast",
                    fluidRow(
                      column(12
                             
                             
                             ,
                             
                             fluidRow(
                               column(4,  mainPanel(width = 16,
                                              
                                                    wellPanel(
                                                      radioButtons("forecastType", "Selecting the CryptoCurrency that you want to forecast",
                                                                   c("Bitcoin (BTC)"="BTC"
                                                                     , "Dash (DASH)"="DASH"
                                                                     , "Ethereum Classic (ETC)"="ETC"
                                                                     , "Ethereum (ETH)"="ETH"
                                                                     , "Litecoin (LTC)"="LTC"
                                                                     , "NEM (XEM)"="XEM"
                                                                     , "Ripple (XRP)"="XRP"
                                                                     , "Zcash (ZEC)"="ZEC"
                                                                   )
                                                      )     , sliderInput("day", "Day :",
                                                                         min=12, max=30, value=30),
                                                      sliderInput("forcastDay", "Forecast Day:",
                                                                  min=2, max=15, value=5)
                                                    ),
                                                    wellPanel(verbatimTextOutput('mySum'))
                               )
                               
                               ),
                               column(width = 8,
                                      wellPanel( plotOutput("forecastPlot")),
                                      wellPanel(dataTableOutput('forecastTable'))
                                      
                               )
                             )
                      )
                    )
           )
           
)








