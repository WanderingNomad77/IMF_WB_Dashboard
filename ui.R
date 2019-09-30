#inst
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(googleVis)
source("autoplotforecast.R")
source("WEO.R")
library(DT)
library(ggcorrplot)


shinyUI(dashboardPage(skin = "blue",
                      
                      dashboardHeader(title = "IMF Data"),
                      
                      dashboardSidebar(
                        
                        sidebarMenu(
                          
                          menuItem("Map", tabName = "map1", icon = icon("globe"), badgeLabel = "+ Country Rankings", badgeColor = 'red'),
                          
                          menuItem("Graphs", tabName = "ts", icon= icon("chart-bar"), badgeLabel = 'Exploratory Analysis', badgeColor = 'orange'),
                          
                          menuItem("Forecasting", tabName = "fc", icon = icon("chart-line"), badgeLabel = 'ARIMA Model', badgeColor = "yellow"),
                          
                          menuItem("Correlation", tabName = "cor", icon = icon("plus"), badgeLabel = "Corr Matrix", badgeColor = "purple"),
                          
                          menuItem("Downloads", tabName = "dl", icon = icon("download"), badgeLabel = "Raw & Tidy Data", badgeColor = "blue")
                          
                        )
                      ),
                      
                      #----------------------------------------------------------------------------------------# 
                      
                      dashboardBody(               #    MAP -- MAP -- MAP -- MAP       
                        tabItems(
                          tabItem(tabName = "map1",
                                  fluidRow(
                                    box(
                                      h4("International Monetary Fund (IMF) Economic Data Mapper", align = 'center'),background = "blue", width = 12)),
                                  fluidRow(infoBoxOutput("maxBox"),
                                           infoBoxOutput("medBox"),
                                           infoBoxOutput("minBox")),
                                  fluidRow(
                                    box(
                                      selectInput(inputId = "D",
                                                  label = "Select an Indicator:",
                                                  choices = unique(dat$Indicator),
                                                  selected = "Gross domestic product, current prices"), background = "blue", height = '100px'),
                                    box(sliderInput(inputId = "E",
                                                    label = "Year",min = min(dat$Year), max = max(dat$Year), ticks = F, 
                                                    value = 1980, animate = animationOptions(loop = T, interval = 2000)
                                    ), height = '100px')),
                                  
                                    tabsetPanel(tabPanel( "Geo Map",
                                                          fluidRow(
                                                          column(width = 12,
                                           wellPanel(htmlOutput("MGDP", align ='center', width = '100%'),style = 'padding: 10px; width: 100%; background: #add8e6', align = 'center'
                                              )))
                                  ),
                                  tabPanel("Rankings Table", 
                                           fluidRow(
                                             box(DT::dataTableOutput("rankingstable"), width = 12, title = "Rankings by Country",
                                                 status = "primary"))),
                                  tabPanel("Ordered Bar Chart",
                                           fluidRow(
                                               column(width = 12,
                                                      box(
                                                        plotlyOutput("BarChart", height = '1800px'), width = 12, height = '1900px',
                                                        title = "Ordered Bar Chart", status = "primary", background = "light-blue"
                                                      )
                                             )))
                                           ))
                          ,
                          
                          #----------------------------------------------------------------------------------------#  
                          
                          
                          tabItem(tabName = "ts",
                                  fluidRow(
                                 
                                  tabBox(tabPanel("Time Series & Distribution",fluidRow(
                                    column(width = 12,
                                    box(
                                      h5('Time Series - Bar, Box, and Density Plots', align = 'center'),
                                      width = 12, background = 'blue'
                                      
                                    ))),
                                  fluidRow(
                                    column(width = 12,
                                           box(selectInput(inputId = 'in6', label = 'Select Country(ies)', choices = unique(dat$Country), 
                                                           multiple=TRUE, selectize=TRUE, selected = c("United States", "China")), 
                                               verbatimTextOutput('out6'), background = "navy"),
                                           box(
                                             selectInput(inputId = "subject",
                                                         label = "Select an Indicator:",
                                                         choices = unique(dat$Indicator),
                                                         selected = "Gross domestic product, current prices"),background = "navy"))),
                                  fluidRow(
                                    column(width = 12,
                                           box(plotlyOutput("plt"), background = "blue", title = "Time Series", status = "primary"),
                                           box(plotlyOutput("bgdp"), background = "blue", title = "Bar Plot", status = "primary"),
                                           box(plotlyOutput("boxgdp"), background = "blue", title = "Box Plot", status = "primary"),
                                           box(plotlyOutput("dgdp"), background = "blue", title = "Density Plot", status = "primary")))),
                          

                          tabPanel("Correlation Plots",
                                   fluidRow(column(width = 12,
                                     box(
                                       h5('Interactive Dot Plots', align = 'center'),
                                       width = 12, background = "blue", status = "primary"
                                       
                                     ))),
                                  
                                    fluidRow(
                                      column(
                                        width = 12,
                                      
                                         box(
                                             selectInput(inputId = "indicator",
                                                         label = "Select an Indicator (x-axis) :",
                                                         choices = colnames(testplot[4:13]),
                                                         multiple = F,
                                                         selectize = F,
                                                         selected = "Gross domestic product, current prices, Billions of U.S. dollars"), background = "blue"),
                                      
                                             box(
                                               selectInput(inputId = "indicator2",
                                                         label = "Select a second Indicator (y-axis):",
                                                         choices = names(testplot[4:13]),
                                                         multiple = F,
                                                         selectize = F, 
                                                         selected = "Gross domestic product per capita, current prices, U.S. dollars"), background = "blue"))),
                                    
                                    fluidRow(column(width = 12,
                                           box(sliderInput(inputId = "y",
                                                    label = "Year",min = min(testplot$Year), max = max(testplot$Year), ticks = F, 
                                                    value = 1980, animate = animationOptions(loop = T, interval = 2000)), 
                                                    status = "primary")
                                    )),
                                    
                                          fluidRow(column(
                                             width = 12,
                                                  box(
                                                       plotOutput(
                                                          "interact1", 
                                                                   brush =  brushOpts(id = "plot_brush", resetOnNew = T)),
                                                       title = "Brush Plot (Controls right plot)", status = "primary", background = "blue", 
                                                       footer = span("Brush points to zoom -- Double click to reset.", style = "color: blue")),
                                                   box(plotOutput("interact2", click = "plot_click"), title = "Brush Points Zoom", status = "primary", background = "blue",
                                                       footer = span("Click on point of interest to display information in second table below.", style = "color: blue"))
                                                        ), 
                                                    column(
                                                      width = 12, 
                                                      (box(DT::dataTableOutput("plot_brushed_points"), width = 12, title = "Brushed Points (left plot)",
                                                           collapsible = T, status = "primary"))), 
                                                       column(width = 12,
                                                              box(DT::dataTableOutput("info"), width = 12, collapsible = T, title = "Point near click (right-plot)",
                                                                  status = "primary")))
                                                    
                                                           ), width = 12
                                   )
                                    )),
                          
                          
                          #----------------------------------------------------------------------------------------# 
                          
                          tabItem(tabName = "fc",
                                  fluidRow(column(width = 12),
                                           box(h4("6-Year ARIMA Forecast [2019 - 2024]"), width = 12, background = "black",
                                               align= 'center')),
                                  fluidRow(
                                    column(width = 12,
                                    infoBoxOutput("forecastval1", width = 4),
                                    infoBoxOutput("forecastval2", width = 4),
                                    infoBoxOutput("forecastval3", width = 4),
                                    infoBoxOutput("forecastval4", width = 4),
                                    infoBoxOutput("forecastval5", width = 4),
                                    infoBoxOutput("forecastval6", width = 4)
                                    
                                  )),
                                  fluidRow(
                                    column(
                                    width = 6,
                                    box(
                                    selectInput(
                                      inputId = "nation",
                                      label = "Select a Country:",
                                      choices = unique(dat$Country),
                                      selected = "United States"), background = "black", width = 16)),
                                  column(width = 6,
                                         box(selectInput(inputId = "indic",
                                                  label = "Select an Indicator:",
                                                  choices = unique(dat$Indicator),
                                                  selected = "Gross domestic product, current prices, U.S. dollars"), background = "black", width = 16
                                  ))),
                                  fluidRow(column(width = 6, 
                                                  tabBox(type = "tabs",
                                                   tabPanel("Backtest",plotlyOutput("forecast"), width = 6, height = '300px'), 
                                                              tabPanel("Forecast Plot", plotlyOutput("realforecast")),
                                                   tabPanel("Forecast Values (data table)", DT::dataTableOutput("forecastvalues")
                                                   ), width = 16)),
                                           column(width = 6,
                                                    box(plotOutput("residuals"), background = "blue", title = "Residuals Plot",
                                                      status = "primary", width = 16))),
                                  fluidRow(column(width = 12, 
                                                  box(h4("Select Coefficients for the Autoregressive Integrated Moving Average (ARIMA) Model", 
                                                         align = "center"), width = 12, background = "black"),
                                                  box(
                                                    sliderInput(
                                                      inputId = "ar",
                                                      label = "Autoregressive",
                                                      min = 0,
                                                      max = 10,
                                                      value = 3), width = 4),
                                                  box(
                                                    sliderInput(
                                                      inputId = "df",
                                                      label = "Differencing",
                                                      value = 1,
                                                      min = 0,
                                                      max = 10), width = 4),
                                                  box(
                                                    sliderInput(
                                                      inputId = "ma",
                                                      label = "Moving-Average",
                                                      min = 0,
                                                      max = 10,
                                                      value = 1), width = 4)),
                                           
                                           column(width = 12,
                                                  box(verbatimTextOutput("accuracy"), background = "navy", width = 12,
                                                      title = "Model Accuracy", status = "primary", align = "center"))
                                  )),
                          
                          #----------------------------------------------------------------------------------------# 
                          
                          tabItem(tabName = "cor",
                                  fluidRow(
                                    column(width = 12,
                                           box(
                                             selectInput("show_vars", "Select Columns to display",
                                                         choices = colnames(dat4), 
                                                         selected = c("Year","United States_Gross domestic product, current prices, U.S. dollars",
                                                                      "China_Gross domestic product, current prices, U.S. dollars"), 
                                                         multiple = T, selectize = T),
                                             width = 16, background = "navy")
                                           )
                                    ),
                                  fluidPage(
                                    fluidRow(
                                      box(
                                        id = "dataset", DT::dataTableOutput("mytable"), width = 16),
                                      box(plotOutput("corr3"), width = 16, collapsible = T, collapsed = F, background = "blue",
                                          title = "Scatterplot Matrix", status = 'primary'),
                                      box(
                                        plotOutput("ggcorr"), width = 16, collapsible = T, collapsed = F, background = "blue", 
                                        title = "Correlation Matrix 2", status = "primary")
                                      )
                                    )
                                  ),
                          tabItem(tabName = "dl",
                                  fluidRow(
                                  column(width = 12,
                                         tabBox(type = "tabs",
                                                     tabPanel(downloadButton("downloadData2", "Download Raw Data"),
                                                              
                                                              DT::dataTableOutput("tbl2"), title = "Raw Data"),
                                                     tabPanel("Tidy Data", downloadButton("downloadData", "Download Tidy Data"),
                                                              DT::dataTableOutput("tbl")), width = 16, title = "Download Dataset(s)",side = "left"
                                        
                                                     )
                                  
                                ))
                                  )))

                                  )
                                  
                                  )
                          
                        
                    
