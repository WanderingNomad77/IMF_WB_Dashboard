library(shiny)
library(shinydashboard)
library(googleVis)
source("autoplotforecast.R")
source("WEO.R")
library(DT)
library(ggcorrplot)


# Define UI for application that draws a histogram

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "World Bank Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Map", tabName = "map1", icon = icon("globe"), badgeLabel = "Economic Data", badgeColor = 'red'),
      menuItem("Exploratory Plots", tabName = "ts", badgeLabel = 'Time Series', badgeColor = 'orange'),
      menuItem("Forecasting", tabName = "fc", badgeLabel = 'ARIMA', badgeColor = "yellow"),
      menuItem("Correlation", tabName = "cor")
      
    )
  ),
  
  #----------------------------------------------------------------------------------------# 
  
  dashboardBody(               #    MAP -- MAP -- MAP -- MAP       
    tabItems(
      tabItem(tabName = "map1",
              fluidRow(
                box(
                  htmlOutput("title1"), width = 12, background = "black")),
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("medBox"),
                       infoBoxOutput("minBox")),
              fluidRow(
                box(
                  selectInput(inputId = "D",
                              label = "Indicator",
                              choices = unique(dat$Indicator),
                              selected = "Gross domestic product, current prices"), background = "blue", height = '100px'),
                  box(sliderInput(inputId = "E",
                              label = "Year",min = min(dat$Year), max = max(dat$Year), ticks = F, 
                              value = 1980, animate = animationOptions(loop = T, interval = 3000)
                                ), height = '100px'),
                column(width = 12,
                box(htmlOutput("MGDP", align ='center'), width = 16, background = "red"))
              ))
      ,
      
        #----------------------------------------------------------------------------------------#    
      
      
      tabItem(tabName = "realgdp",   #  EXPLORATORY PLOTS -- EXPLORATORY PLOTS -- EXPLORATORY PLOTS
              fluidRow(
                box(
                  selectInput(inputId = "B",
                              label = "Country:",
                              choices = unique(dat$Country),
                              selected = "United States"),
                  selectInput(inputId = "C",
                              label = "Indicator:",
                              choices = unique(dat$Indicator),
                              selected = "Gross domestic product, current prices"),
                  width = 10, background = "olive"),
                box(plotlyOutput("RGDP"), width = 10),
                box(plotlyOutput("FGDP"), width = 10)
              )
      ),
      
      #----------------------------------------------------------------------------------------# 
      
      
      tabItem(tabName = "ts",
              fluidRow(
                box(
                  h4('World Time Series Visualization', align = 'center'),
                  width = 12, background = 'black'
                
              )),
              fluidRow(
                column(width = 12,
                box(selectInput(inputId = 'in6', label = 'Options', choices = unique(dat$Country), 
                                multiple=TRUE, selectize=TRUE, selected = c("United States", "China")), 
                    verbatimTextOutput('out6'), background = "navy"),
                box(
                  selectInput(inputId = "subject",
                              label = "Indicator:",
                              choices = unique(dat$Indicator),
                              selected = "Gross domestic product, current prices"),background = "navy"))),
              fluidRow(
              column(width = 12,
                     box(plotlyOutput("plt"), background = "blue"),
                            box(plotlyOutput("bgdp"), background = "blue"),
                     box(plotlyOutput("boxgdp"), background = "blue"),
                         box(plotlyOutput("dgdp"), background = "blue")))),
      
      #----------------------------------------------------------------------------------------# 
           

      tabItem(tabName = "fc",
              box(
                  selectInput(
                    inputId = "nation",
                        label = "Country:",
                        choices = unique(dat$Country),
                        selected = "United States"), background = "red"),
          box(selectInput(inputId = "indic",
                          label = "Indicator:",
                          choices = unique(dat$Indicator),
                          selected = "Gross domestic product, current prices, U.S. dollars"), background = "red"
          ),
          fluidRow(column(width = 12, 
                   box(plotlyOutput("forecast"), background = "light-blue"),
                   box(plotOutput("residuals"), background = "light-blue"))),
            fluidRow(column(width = 12,
              box(
              sliderInput(
                inputId = "ar",
                label = "Autoregressive",
                min = 0,
                max = 10,
                value = 2), width = 4),
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
                value = 0), width = 4)),
              
                column(width = 12,
                box(verbatimTextOutput("accuracy"), background = "navy", width = 12))
           )),
      
      #----------------------------------------------------------------------------------------# 
  
  tabItem(tabName = "cor",
          fluidRow(
            column(width = 12,
            box(
              selectInput("show_vars", "Select Columns to display",
                                 choices = colnames(dat4), 
                          selected = c("Year","United States_Gross domestic product, current prices, U.S. dollars",
                                       "China_Gross domestic product, current prices, U.S. dollars"), multiple = T, selectize = T)))),
          fluidPage(
            fluidRow(
            box(
              id = "dataset", DT::dataTableOutput("mytable"), width = 16),
            box(plotOutput("corr3"), width = 16),
            box(
              plotOutput("ggcorr")
            )))
           
          ))))
          



# ---------------------------------------------------------------------------------------------------------#               
# ---------------------------------------------------------------------------------------------------------# 

#                                         SERVER - SERVER - SERVER

# ---------------------------------------------------------------------------------------------------------# 
# ---------------------------------------------------------------------------------------------------------# 
# ---------------------------------------------------------------------------------------------------------# 


server <- (function(input, output) {
  
  output$GDP <- renderPlotly({
    ggplot(na.omit(real_gdp2[,c("LOCATION", input$B)]), aes(reorder(LOCATION, na.omit(real_gdp2[[input$B]])), na.omit(real_gdp2[[input$B]]))) +
      geom_bar(stat = 'identity', fill = "blue") +
      labs( y = "GDP per Capita",
            x = "Country") + 
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })


# ---------------------------------------------------------------------------------------------------------# 
#                                                  EXPLORATORY PLOTS
# ---------------------------------------------------------------------------------------------------------#   

  

  output$TSGDP <- renderPlotly({
    ggplot(na.omit(filter(dat, Country == input$B, Indicator == input$C)), 
           aes(Year, na.omit(Value))) +
      geom_line() + geom_smooth(method = "loess")
  })
  
  output$linePlot <- renderPlotly({
    a <- ggplot() +
      geom_line(data = na.omit(filter(dat, Country == input$Country, Indicator == input$subject)), aes(x = Year, y = na.omit(Value), color = Country)) +
      geom_line(data = na.omit(filter(dat, Country == input$country2, Indicator == input$subject)), aes(x = Year, y = na.omit(Value), color = Country)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = seq(1980, 2018)) +
      ylab("fbke")
    b <- ggplotly(a)
    x <- list(
      title = "Year")
    ifelse(input$subject == "Gross domestic product, current prices, U.S. dollars", 
           y <- list(
             title = "Billion"),
           ifelse(input$subject == "Population, Persons",
                  y <- list(
                    title = "Million"),
                  y <- list(
                    title = "Units"
                  )
                  
           ))
    b %>% layout(xaxis = x, yaxis = y)
    

  })
  
  output$plt <- renderPlotly({
    a <- ggplot() +
      geom_line(data = na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(x = Year, y = na.omit(Value), color = Country)) +
      theme_solarized() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = seq(1980, 2018), expand = c(0,0)) +
      ggtitle(input$subject)
    b <- ggplotly(a)
    x <- list(
      title = "Year")
    ifelse(input$subject == "Gross domestic product, current prices, U.S. dollars", 
           y <- list(
             title = dat[dat$Indicator == input$subject,]$Scale),
           ifelse(input$subject == "Population, Persons",
                  y <- list(
                    title = "Million"),
                  y <- list(
                    title = dat[dat$Indicator == input$subject,]$Scale
                  )
                  
           ))
    b %>% layout(xaxis = x, yaxis = y)
    
  })
  
  output$bgdp <- renderPlotly({
    ggplot(data = na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Year, na.omit(Value), fill = Country)) +
      geom_bar(stat = 'identity', position = "stack") + ylab(dat[dat$Indicator == input$subject,]$Scale) +
       theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = seq(1980, 2018))
  })
  
  output$boxgdp <- renderPlotly({
    ggplot(na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Country, na.omit(Value), label = Year)) +
      geom_boxplot(aes(fill = Country)) + geom_point(aes(text = Year)) + theme_economist() +
      ylab(dat[dat$Indicator == input$subject,]$Scale)
  })
  
  output$dgdp <- renderPlotly({
    avg <- dat %>%
      select(-Scale) %>%
      filter(Country %in% input$in6, Indicator == input$subject) %>%
      group_by(Country) %>%
      summarise(grp.mean = mean(Value)) 
      
    ggplot(na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Value, fill = Country)) +
      geom_density(alpha = 0.3, position = "dodge", aes(text = Year)) + geom_vline(data = avg, aes(xintercept = grp.mean, col = Country), linetype = "dotted") +
      theme_bw() + xlab(dat[dat$Indicator == input$subject,]$Scale)
  })
  
  output$out6 <- renderPrint(input$in6)

# ---------------------------------------------------------------------------------------------------------#   
#                                             FORECAST BACKTESTING
# ---------------------------------------------------------------------------------------------------------# 
  
  
  output$forecast <- renderPlotly({
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, frequency = 1)
    train <- window(gdp_ts, start = 1980, end = 2012)
    
    gdp_training <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                       start = 1980, end = 2012, frequency = 1)
    ifelse(input$indic == "Volume of imports of goods and services, Percent change" ||
          input$indic == "Volume of exports of goods and services, Percent change" || 
          input$indic == "Inflation, average consumer prices,
           Percent Change",
           ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift = T, lambda = "auto", seasonal = T, method = "ML"),
           ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift =  T, seasonal = T, lambda = "auto", method = "ML"))
    testing_data <- filter(dat, Country == input$nation, Indicator == input$indic, Year > 2012)
    gdp_testing <- ts(testing_data[,5], start = 2013, frequency = 1)
    
    ft %>%
      forecast(h = 6) %>%
      autoplot(holdout = gdp_testing, forc_name = "ARIMA", ts_object_name = input$indic) + theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = seq(1980,2018)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
    
   
      
    
  })
    output$residuals <- renderPlot({
      gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                         start = 1980, frequency = 1)
      train <- window(gdp_ts, end = 2012)
      gdp_training <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                         start = 1980, end = 2012, frequency = 1)
      ifelse(input$indic == "Volume of imports of goods and services, Percent change" ||
               
               input$indic == "Volume of exports of goods and services, Percent change" || input$indic == "Inflation, average consumer prices,
             Percent change",
             ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift = T, lambda = "auto", seasonal = T),
             ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift =  T, seasonal = T, lambda = "auto"))
    
     checkresiduals(ft)
    
     
  })
    output$accuracy <- renderPrint({
      gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                   start = 1980, frequency = 1)
      train <- window(gdp_ts, end = 2012, frequency = 1)
      testing_data <- filter(dat, Country == input$nation, Indicator == input$indic, Year > 2012)
      gdp_testing <- ts(testing_data[,5], start = 2013, frequency = 1)
      gdp_training <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                         start = 1980, end = 2012, frequency = 1)
      ifelse(input$indic == "Volume of imports of goods and services, Percent change" ||
               
               input$indic == "Volume of exports of goods and services, Percent change" || input$indic == "Inflation, average consumer prices,
             Percent change",
             ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift = T, lambda = "auto", seasonal = T, method = "ML"),
             ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift =  T, seasonal = T, lambda = "auto", method = "ML"))
  
     
      fit_arima <- forecast(ft, h = 6)
      accuracy(fit_arima, gdp_ts)
    })
  
# --------------------------------------------------------------------------------------------------------#   
#                                               FORECASTING 
# --------------------------------------------------------------------------------------------------------# 
    
  
  output$FGDP <- renderPlotly({
    gdp_ts <- ts(filter(dat, Country == input$B, Indicator == input$C)[,5], start = 1980,frequency = 1)
    fit <- auto.arima(gdp_ts, allowdrift = T, lambda = 0)
    fit %>%
      forecast(h = 12) %>% 
      autoplot(holdout = 0, forc_name = 
                 'blabla', ts_object_name = "") +
      scale_x_continuous(breaks = seq(1980,2022)) +
      labs( y = "GDP per Capita",
            x = "Country") +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })

# ---------------------------------------------------------------------------------------------------------#   
#                                                 MAP 
# ---------------------------------------------------------------------------------------------------------# 
  
  output$MGDP <- renderGvis({
    x <- filter(dat, Year == input$E, Indicator == input$D)
    gvisGeoChart(na.omit(x), locationvar = "Country",
                 colorvar = "Value", options = list(width = '1400px', height = '700px', colorAxis="{colors: ['lightyellow', 'yellow', 'orange','red', 'brown','black']}",
                                                    backgroundColor="lightblue"))
   
  })
  
  output$title1 <- renderText(WORLD_GDP_VISUAL)

# ---------------------------------------------------------------------------------------------------------#  
#                                               INFO BOXES  
# ---------------------------------------------------------------------------------------------------------# 
  
output$maxBox <- renderInfoBox({
    y <- filter(dat,Year == input$E, Indicator == input$D)
    infoBox( y[which.max(y[,5]), 1], 
            paste(max(na.omit(filter(dat, Year == input$E, Indicator == input$D))$Value), 
                  unique(dat[dat$Indicator == input$D,]$Scale)),
            icon = icon("angle-double-up"), color = "green", fill = T)
  })
  
  output$minBox <- renderInfoBox({
    y <- filter(dat,Year == input$E, Indicator == input$D)
    infoBox(y[which.min(y[,5]), 1], 
            paste(min(na.omit(filter(dat, Year == input$E, Indicator == input$D))$Value),
                  unique(dat[dat$Indicator == input$D,]$Scale)), 
            icon = icon("angle-double-down"), color = "red", fill = T)
  })
  
  output$medBox <- renderInfoBox({
    infoBox(paste("MEDIAN", input$D), 
            paste(median(na.omit(filter(dat, Year == input$E, Indicator == input$D)$Value)),unique(dat[dat$Indicator == input$D,]$Scale)),
            icon = icon("calculator"), fill = T)
  })
  


dat6 = dat4
output$mytable <- DT::renderDataTable({
 
  DT::datatable(dat6[, input$show_vars, drop = F], class = "cell-border stripe", options = list(scrollX = T))
})

output$corr3 <- renderPlot({

  ggpairs(dat6[,input$show_vars, drop = F][,-1])
 
 

  
})

output$ggcorr <- renderPlot({
  correl <- round(cor(dat6[,input$show_vars, drop = F][,-1], use = "pairwise.complete.obs"), 2)
  ggcorrplot(correl,hc.order = T,type = "lower", ggtheme = ggplot2::theme_bw, lab = T, digits = 3)
})
  
})

# Run the application 
shinyApp(ui = ui, server = server)