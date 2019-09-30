#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
library(dplyr)



shinyServer(function(input, output) {
  
  
  
  

  # ---------------------------------------------------------------------------------------------------------#   
  #                                                 MAP 
  # ---------------------------------------------------------------------------------------------------------# 
  
  
  output$MGDP <- renderGvis({
    x <- filter(dat, Year == input$E, Indicator == input$D)
    
    gvisGeoChart(na.omit(x), locationvar = "Country",
                 colorvar = "Value", options = list(width = '1400px', height = '700px', 
                                                    colorAxis="{colors: ['red' ,'indianred', 'salmon',
                                                    'papayawhip', 'moccasin', '#FFFF33','greenyellow','yellowgreen','limegreen', 'green','darkgreen']}",
                                                    backgroundColor="lightblue"))
    
    
  })
  
  
  ## Rankings Data Table. 
  
  output$rankingstable <- DT::renderDataTable({
    x <- filter(dat, Year == input$E, Indicator == input$D) %>%
      arrange(desc(Value, Country))
    
    DT::datatable(x)
    
  })
  
  
  
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
  
  
  
  #=================================================================#
                       # 6-YEAR FORECAST BOXES #
  #=================================================================#
  
  
  
  output$forecastval1 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame()%>%
      round(2)
    
    infoBox("2019",
            x[1,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "red", fill = T)
    
  })
  
  output$forecastval2 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame()%>%
      round(2)
    
    infoBox("2020",
            x[2,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "orange", fill = T)
    
  })
  
  output$forecastval3 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame()%>%
      round(2)
    
    infoBox("2021",
            x[3,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "yellow", fill = T)
    
  })
  
  output$forecastval4 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame()%>%
      round(2)
    
    infoBox("2022",
            x[4,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "navy", fill = T)
    
  })
  
  output$forecastval5 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame()%>%
      round(2)
    
    infoBox("2023",
            x[5,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "blue", fill = T)
    
  })
  
  output$forecastval6 <- renderInfoBox({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame() %>%
      round(2)
    
    infoBox("2024",
            x[6,1], filter(dat, Indicator == input$indic)$Scale,
            icon = icon("chart-line"), color = "light-blue", fill = T)
    
  })
  
  
  
  
  # ---------------------------------------------------------------------------------------------------------# 
  #                                                  EXPLORATORY PLOTS
  # ---------------------------------------------------------------------------------------------------------#   
  
  
  # Line plots
  
  output$plt <- renderPlotly({
    x <- filter(dat, Country %in% input$in6, Indicator == input$subject)
    a <- ggplot() +
      geom_line(data = na.omit(x), aes(x = Year, y = na.omit(Value), color = Country)) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = seq(1980, 2018, 2), expand = c(0,0)) +
      theme(legend.position = "top") + theme(legend.title = element_text(size = 10))
    
    b <- ggplotly(a) %>% layout(legend = list(orientation  = 'h', x = 0.5, y = 1.2 ))
    
    x <- list(title = "Year")
    
    y <- list(title = dat[dat$Indicator == input$subject,]$Scale)
                 
    b %>% layout(xaxis = x, yaxis = y)
    
  })
  
  # Bar Plots
  
  
  output$bgdp <- renderPlotly({
  
      a <- ggplot(data = na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Year, na.omit(Value), fill = Country)) +
        geom_bar(stat = 'identity', position = "stack") + ylab(dat[dat$Indicator == input$subject,]$Scale) +
        theme_economist() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_continuous(breaks = seq(1980, 2018)) + theme(legend.position = "top")
    
  
    ggplotly(a) %>% layout(legend = list(orientation  = 'h', x = 0.5, y = 1.2 ))
   
  
    
  })
  
  # Box plots.
  
  output$boxgdp <- renderPlotly({
    a <- ggplot(na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Country, na.omit(Value), label = Year)) +
      geom_boxplot(aes(fill = Country)) + geom_point(aes(text = Year)) + theme_economist() +
      ylab(dat[dat$Indicator == input$subject,]$Scale)
    
    ggplotly(a) %>% layout(legend = list(orientation  = 'h', x = 0.5, y = 1.2 ))
  })
  
  # Density plots.
  
  output$dgdp <- renderPlotly({
    
    avg <- dat %>%
      select(-Scale) %>%
      filter(Country %in% input$in6, Indicator == input$subject) %>%
      group_by(Country) %>%
      summarise(grp.mean = mean(Value, na.rm = T))
    
    a <- ggplot(na.omit(filter(dat, Country %in% input$in6, Indicator == input$subject)), aes(Value, fill = Country)) +
      geom_density(alpha = 0.3, position = "dodge", aes(text = Year)) + geom_vline(data = avg, aes(xintercept = grp.mean, col = Country), linetype = "dotted") +
      theme_bw() + xlab(dat[dat$Indicator == input$subject,]$Scale)
    
    ggplotly(a) %>% layout(legend = list(orientation  = 'h', x = 0.5, y = 1.2 ))
    
  })
  
  # ---------------------------------------------------------------------------------------------------------# 
  
  #TAB #2
  
  ## Interactive dot plots. 
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  
  output$interact1 <- renderPlot({
    
    x <- ggplot(filter(testplot, Year == input$y), aes_string(x = as.name(input$indicator), y = as.name(input$indicator2)))
    
    
     x + geom_point(aes_string( col = "Continent", size = "Population")) +
      scale_size_continuous(breaks = c(0,5,50,100,200,1000)) + theme(legend.position = "bottom") + 
      scale_y_log10() + scale_x_log10() +
      geom_smooth(aes_string(as.name(input$indicator), as.name(input$indicator2)), method = "loess", col = "black") +
      geom_smooth(method = "lm", col = "red", show.legend = F) + 
      ylab(gsub("_"," ",input$indicator2)) + xlab(gsub("_", " ", input$indicator)) +
      theme_wsj() + theme(axis.title=element_text(size= 12, hjust = 0.5, family = "Times")) +
      labs(title = paste(gsub("Gross domestic product","GDP",input$indicator2), "vs.", gsub("Gross domestic product", "GDP", input$indicator))) +
       theme(plot.title = element_text(size = 14, hjust = 0.5, family = "Times"), legend.position = "top") +
       theme(legend.title = element_text(size = 12, hjust = 0))
  })
  
  output$interact2 <- renderPlot({
  
    x <- ggplot(filter(testplot, Year == input$y), aes_string(x = as.name(input$indicator), y = as.name(input$indicator2)), aes(label = Country))
    
      x + 
      geom_point(aes_string( col = "Continent", size = "Population")) +
      scale_size_continuous(breaks = c(0,5,50,100,200,1000)) + 
      theme(legend.position = "bottom") +
      geom_smooth(aes_string(as.name(input$indicator), as.name(input$indicator2)), method = "loess", col = "black") + 
      scale_y_log10() + scale_x_log10() +
      geom_smooth(method = "lm", col = "red", show.legend = F) + 
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      theme_fivethirtyeight() + theme(axis.title = element_text(size = 10, vjust = 0.25, hjust = 0.25)) +
      labs(title = paste(gsub("Gross domestic product","GDP",input$indicator2), "vs.", gsub("Gross domestic product", "GDP", input$indicator))) +
      theme(plot.title = element_text(size = 12, hjust = 0.5), legend.position = "top", legend.text = element_text(size = 10)) +
      theme(legend.title = element_text(size = 12, hjust = 0)) +
        theme(axis.title = element_text(size = 10, hjust = 0.5)) + geom_text(check_overlap = T, nudge_x = 0.03, aes(label= Country))
    
    
  })
    
 
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Data Table Outputs
  
  output$plot_brushed_points <- DT::renderDataTable({
    
    x <- na.omit(filter(testplot[c("Country", "Year","Population", input$indicator, input$indicator2)], Year == input$y))
      
    
    brushedPoints(x, input$plot_brush, xvar = input$indicator
                  , yvar = input$indicator2) 
  })
  
  
  output$info <- DT::renderDataTable({
    nearPoints(filter(testplot[c("Country", "Year", "Population", input$indicator, input$indicator2)], Year == input$y),input$plot_click, threshold = 10, maxpoints = 1)
    
    
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
   
    
           ft <- Arima(gdp_training, order = c(input$ar,input$df,input$ma), include.drift =  F, seasonal = T, lambda = "auto", method = "ML")
           
    testing_data <- filter(dat, Country == input$nation, Indicator == input$indic, Year > 2012)
    
    gdp_testing <- ts(testing_data[,5], start = 2013, frequency = 1)
    
  
    ft %>%
      forecast(h = 6) %>%
      autoplot(holdout = gdp_testing, forc_name = "ARIMA", ts_object_name = input$indic) + theme(plot.title = element_text(hjust = 0.5)) +
      theme_economist() +
      scale_x_continuous(breaks = seq(1980,2018, 2)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    
  })
  

# RESIDUALS PLOT #
  
  output$residuals <- renderPlot({
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, frequency = 1)
    
    train <- window(gdp_ts, end = 2012)
    
    gdp_training <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                       start = 1980, end = 2012, frequency = 1)
  
           ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift =  F, seasonal = T, lambda = "auto", method = "ML")
    
    checkresiduals(ft)
    
    
  })
  
# FORECAST ACCURACY  #
  
  output$accuracy <- renderPrint({
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    train <- window(gdp_ts, end = 2012, frequency = 1)
    testing_data <- filter(dat, Country == input$nation, Indicator == input$indic, Year > 2012)
    gdp_testing <- ts(testing_data[,5], start = 2013, frequency = 1)
    gdp_training <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                       start = 1980, end = 2012, frequency = 1)
  
    
           ft <- Arima(train, order = c(input$ar,input$df,input$ma), include.drift =  F, seasonal = F, lambda = "auto", method = "ML")
    
    
    fit_arima <- forecast(ft, h = 6)
    
    accuracy(fit_arima, gdp_ts)
  })
  
  # LJUNG BOX TEST
  
  
  
  # --------------------------------------------------------------------------------------------------------#   
  #                                               FORECASTING 
  # --------------------------------------------------------------------------------------------------------# 
  
  output$realforecast <- renderPlotly({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
           ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    ft %>%
      forecast(h = 6) %>%
      autoplot(holdout = 0, forc_name = "ARIMA", ts_object_name = input$indic) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_continuous(breaks = seq(1980,2024, 2))
    
  })
  
  output$forecastvalues <- DT::renderDataTable({
    
    gdp_ts <- ts(filter(dat, Country == input$nation, Indicator == input$indic)[,5],
                 start = 1980, end = 2018, frequency = 1)
    
    ft <- Arima(gdp_ts, order = c(input$ar,input$df,input$ma), include.drift = F, lambda = "auto", seasonal = T, method = "ML")
    
    x <- ft %>%
      forecast(h = 6) %>%
      as.data.frame() %>%
      round(2)
    
    x
      
  })
  

  
  
  
  dat6 = dat4
  output$mytable <- DT::renderDataTable({
    
    DT::datatable(dat6[, input$show_vars, drop = F], class = "cell-border stripe", options = list(scrollX = T))
  })
  
  output$corr3 <- renderPlot({
    
    my_fn <- function(data, mapping, ...){
    p <- ggplot(data = dat6[,input$show_vars, drop = F][,-1], mapping = mapping) + geom_point() + geom_smooth(method = "loess") +
      geom_smooth(method = "lm", color = "red")}
    
    ggpairs(dat6[,input$show_vars, drop = F][,-1], lower = list(continuous = my_fn))
    
    
    
    
  })
  
  output$ggcorr <- renderPlot({
    correl <- round(cor(dat6[,input$show_vars, drop = F][,-1], use = "pairwise.complete.obs"), 2)
    ggcorrplot(correl,hc.order = T,type = "lower", ggtheme = ggplot2::theme_bw, lab = T, digits = 2)
  })

  
  output$tbl <- DT::renderDataTable({
    dat
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = paste("IMFeconomicDataset", ".csv", sep = ""),
  content = function(file) {
    write.csv(dat, file, row.names = F)
  }
  )
  
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(data, options = list(scrollX = T))
    
  })
  output$downloadData2 <- downloadHandler(
    filename = paste("IMFrawData", ".csv", sep = ""),
    content = function(file) {
      write.csv(data, file, row.names = F)
    }
  )
  
  output$BarChart <- renderPlotly({
    y <- dat %>%
      filter(Year == input$E, Indicator == input$D) %>%
      arrange(Value, Continent)
    
    ggplot(na.omit(y), aes(reorder(Country, Value), Value, fill = Continent, width = 1)) + geom_col(stat = 'identity') + coord_flip() +
      xlab("Country\n\n") + theme(axis.text.x = element_text(vjust = 1)) 
     
  })
  

})
