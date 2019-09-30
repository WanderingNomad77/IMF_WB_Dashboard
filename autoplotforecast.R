
autoplot.forecast <- function(forecast, forc_name, ts_object_name, 
                              ..., holdout=NaN){
  #' Plots Forecasted values for Time Series Models
  #'
  #' Borrowed from Drew Schmidt, but modified to fit our aesthetic appeal
  #' we create a dataframe with all the appropriate sections (i.e. upper and
  #' lower 95% CI bands, forecasted, actual values, the training time series
  #' object, and upper and lower 80% CI), the we create a \code{ggplot2} object that
  #' reflects the forecasted plot
  #'
  #' @param forecast forecasted values created using \code{forecast} function
  #' @param forc_name name of forecasted method included in title
  #' @param ts_object_name time series name included in title
  #' @param holdout time series object that contains actual values that can be
  #' compared to the forecasted values
  
  # data wrangling
  time <- attr(forecast$x, "tsp")
  time <- seq(time[1], attr(forecast$mean, "tsp")[2], by=1/time[3])
  lenx <- length(forecast$x)
  lenmn <- length(forecast$mean)
  
  df <- data.frame(time=time,
                   x=c(forecast$x, forecast$mean),
                   x2=c(forecast$x, rep(NA, lenmn-length(holdout)), holdout),
                   forecast=c(rep(NA, lenx), forecast$mean),
                   low1=c(rep(NA, lenx), forecast$lower[, 1]),
                   upp1=c(rep(NA, lenx), forecast$upper[, 1]),
                   low2=c(rep(NA, lenx), forecast$lower[, 2]),
                   upp2=c(rep(NA, lenx), forecast$upper[, 2]),
                   holdout=c(rep(NA, lenx+lenmn-length(holdout)), holdout)
  )
  
  ggplot(df, aes(time, x)) +
    geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow", na.rm=TRUE) +
    geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange", na.rm=TRUE) +
    geom_line(data=df, aes(time, x2), color="turquoise4")+
    geom_line(colour = "black", size = 1) +
    geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", na.rm=TRUE) +
    geom_line(data=df[!is.na(df$holdout), ], aes(time, holdout), color="red", na.rm=TRUE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
    scale_y_continuous("")  +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.background = element_rect(fill = "gray98"),
          axis.line.y   = element_line(colour="gray"),
          axis.line.x = element_line(colour="gray")) +
    labs(x = "Year", y = "Closing Values") +
    ggtitle(sprintf('%s Forecast Plot of \n %s', forc_name, ts_object_name)) + theme(plot.title = element_text(hjust = 0.5))+
    theme_fivethirtyeight()
}