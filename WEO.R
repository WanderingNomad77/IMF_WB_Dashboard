
library(data.table)
library(tidyverse)
library(plotly)
library(ggthemes)
library(ggfortify)
library(forecast)
library(tseries)
library(gridExtra)
library(Mcomp)
library(GGally)


data <- read.csv("WEO_Data-2.csv")
  
dat <- read.csv("WEO_Data-2.csv", na.strings = "n/a", stringsAsFactors = F) %>%
  select(-`Country.Series.specific.Notes`, - Estimates.Start.After, Indicator =Subject.Descriptor) %>%
  gather(Year, Value, X1980:X2018)


dat$Indicator <- paste(dat$Indicator, dat$Units, sep = ", ") 

dat <- dat %>%
  select(-Units) %>%
  filter(Country != "International Monetary Fund, World Economic Outlook Database, October 2018", Country != "", Country != "Kiribati")



dat$Year <- gsub("X","",dat$Year) %>%
  as.numeric()

dat$Value <- gsub(",","", dat$Value) %>%
  as.numeric()



dat2 <- spread(dat, Country, Value)
dat3 <- spread(dat, Indicator, Value)

dat4 <- dat %>%
  unite(Subject, c(Country, Indicator)) %>%
  select(-Scale) %>%
  spread(Subject, Value)


colnames(dat4[,-1]) <- gsub("_", " ", colnames(dat4[,-1]))

dat$Scale[dat$Scale== ""] <- "%"
dat[dat$Indicator == "Inflation, average consumer prices, Index",]$Scale <- "Index"
dat$Scale[dat$Scale== "Billions"] <- "Billion Dollars"
dat$Scale[dat$Scale== "Units"] <- "Dollars"
dat$Scale[dat$Scale == "Millions"] <- "Million"
dat$Country[dat$Country == "Lao P.D.R."] <- "Laos"
dat$Country[dat$Country == "Congo, Democratic Republic of"] <- "Democratic Republic of the Congo"
dat$Indicator[dat$Indicator == "Population, Persons"] <- "Population"
dat$Country[dat$Country == "Islamic Republic of Iran"] <- "Iran"
dat$Country[dat$Country == "Taiwan Province of China"] <- "Taiwan"
dat$Country[dat$Country == "Korea"] <- "South Korea"
dat$Country[dat$Country == "Republic of Congo"] <- "Republic of the Congo"
dat$Country[dat$Country == "Myanmar"] <- "Myanmar (Burma)"


countries <- read.csv("Countries-Continents.csv", stringsAsFactors = F)

countries$Country[countries$Country == "Russian Federation"] <- "Russia"
countries$Country[countries$Country == "Korea, South"] <- "South Korea"
countries$Country[countries$Country == "Congo, Democratic Republic of"] <- "Democratic Republic of the Congo"
countries$Country[countries$Country == "Congo"] <- "Republic of the Congo"
countries$Country[countries$Country == "Gambia"] <- "The Gambia"
countries$Country[countries$Country == "Burma (Myanmar)"] <- "Myanmar (Burma)"

lifeexpectancy <- read.csv("life.csv", stringsAsFactors = F, header = F)
lifeexpectancy <- lifeexpectancy[-(1:2),]
colnames(lifeexpectancy) = lifeexpectancy[1,]
lifeexpectancy <- lifeexpectancy[-1,] %>%
  select(-`Country Code`, -`Indicator Code`, -`NA`, `Country` = `Country Name`, Indicator = `Indicator Name`) %>%
  gather(Year, Value, "1960":"2018") %>%
  mutate(Scale = "years") %>%
  select(Country, Indicator, Scale, Year, Value)

lifeexpectancy$Year <- as.numeric(lifeexpectancy$Year)

lifeexpectancy <- left_join(lifeexpectancy, countries, "Country")  %>%
  filter(Year >1979)
  

dat <- left_join(dat, countries, "Country")

dat[dat$Country == "Taiwan",]$Continent <- "Asia"

dat <- bind_rows(dat, lifeexpectancy)


                                                                                                                                                                                  

testplot <- dat %>%
  select(-Scale) %>%
  spread(Indicator, Value, drop = T)

colnames(testplot)[which(names(testplot) == "Gross domestic product, current prices, U.S. dollars")] <- "Gross domestic product, current prices, Billions of U.S. dollars"



a <- ggplot(testplot, aes(`Gross domestic product, current prices, U.S. dollars`, `Gross domestic product per capita, current prices, U.S. dollars`,
                          col = Continent, size = Population)) +
  geom_point() + scale_size_continuous(breaks = c(0,5,20,50,100,200,300,1000)) +
  labs(title = 'Year: {frame_time}')
  


WORLD_GDP_VISUAL <- 'WORLD GDP DATA VISUALIZATION'


# Create time series object

gdp_ts <- ts(filter(dat, Country == "France", Indicator == "Gross domestic product, current prices, U.S. dollars")[,5],
             start = c(1980), end = c(2017), frequency = 1)

gdp_ts2 <- ts(filter(lifeexpectancy, Country == "France")[,5], start = 1980, end = 2016, frequency = 1)
gdp_train2 <- ts(filter(lifeexpectancy, Country == "France")[,5], start = 1980, end = 2014, frequency = 1)
fitlife <- auto.arima(gdp_train2)
# TESTING FORECAST

# Remove 2011 - 2017 to use as our test set (CREATE TRAINING SET)


  
train <- window(gdp_ts,end = 2012, frequency = 1)
gdp_train <- ts(filter(dat, Country == "France", Indicator == "Gross domestic product, current prices, U.S. dollars")[,5],
             start = 1980, end = 2012, frequency = 1)


gdp_diff <- diff(gdp_train)

fit <- Arima(gdp_train, include.drift = F, seasonal = T, lambda = "auto", order = c(1,1,0))
fit2 <- auto.arima(train, seasonal = T, lambda = 0)
fit_arima <- forecast(fit, h = 5)
fit_arima2 <- forecast(fit2, h=5)

test_data <- filter(dat, Country == "France", Indicator == "Gross domestic product, current prices, U.S. dollars", Year > 2012)

gdp_test <- ts(test_data[,5], start = 2013,  frequency = 1)

tt <- forecast(auto.arima(gdp_train), h = 2)

listofcountries <- unique(lifeexpectancy$Country)
predict2years <- function(listofcountries) {
  i = 1
  for(i in 1:length(listofcountries)) {
   gdp_ts <- ts(filter(lifeexpectancy, Country == listofcountries[i])[,5], start = 1980, end = 2016, frequency = 1)
   gdp_train <- ts(filter(lifeexpectancy, Country == listofcountries[i])[,5], start = 1980, end = 2014, frequency = 1)
   fit <- auto.arima(gdp_train)
   
  }
  
}

testfunction <- function (x, y) {
  x + y
} 




