library(DBI)
library(odbc)
library(lubridate)
library(dplyr)
library(labeling)
library(timeDate)
library(forecast)


TIMESERIES <- NYC_HISTORICAL[,c(2,3,7,9,10,11,12)] %>%
  left_join (NEIGHBORHOOD[,c(1,2)], by = 'NEIGHBORHOOD_ID') %>%
  left_join(BUILDING_CLASS[,c(1,3)], by = c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID')) %>%
  filter(SALE_PRICE>100, TYPE == 'RESIDENTIAL', GROSS_SQUARE_FEET>100) %>%
  mutate(Year = year(SALE_DATE), Quarter = quarter(SALE_DATE)) %>%
  group_by (Quarter, Year)  %>%
  select(TYPE, SALE_PRICE, Quarter, Year, NEIGHBORHOOD_NAME)

fore_Bedford_Stuyvesant <- TIMESERIES %>%
  filter(NEIGHBORHOOD_NAME =='BEDFORD STUYVESANT', Year>2008) %>%
  mutate(t=Year*4 + Quarter - 2009*4) %>%
  group_by(t)  %>%
  summarise(TotalSales = sum(SALE_PRICE))


timeseries_Bedford_Stuyvesant <- ts(fore_Bedford_Stuyvesant$TotalSales, start=c(2009,1),frequency= 4)

ets_Bedford_Stuyvesant <- HoltWinters(timeseries_Bedford_Stuyvesant, alpha = 0.6, beta = 0.2,gamma=0.6,seasonal ="additive")

#ets_Bedford_Stuyvesant <- ets(timeseries_Bedford_Stuyvesant, model='AAA',gamma=0.6)
Forecas_Bedford_Stuyvesant <- forecast(ets_Bedford_Stuyvesant, 8)

plot(ets_Bedford_Stuyvesant)
plot(Forecas_Bedford_Stuyvesant)
summary(Forecas_Bedford_Stuyvesant)
checkresiduals(Forecas_Bedford_Stuyvesant)


# Part2  Use a multiple regression model to come up with another forecast for the next 8 quarters of sales
# Include time and seasonality
# Use sales beginning in the year 2009 to develop your code

fore_Bedford_Stuyvesant <- cbind(fore_Bedford_Stuyvesant, c("Q1","Q2","Q3","Q4"))
names(fore_Bedford_Stuyvesant)[3] <- 'Quarter'


# Regression including time and seasonality --> TotalSales ~ t : Total sales as a function of t(predictor: whatever is denoted by x)
regression_time <- lm(data=fore_Bedford_Stuyvesant, formula=TotalSales~t+Quarter)
summary(regression_time)
x <- data.frame(t=c(45,46,47,48,49,50,51,52),TotalSales = c(0,0,0,0,0,0,0,0), Quarter=c('Q1','Q2','Q3','Q4'))
predict.lm(regression_time, x, interval='confidence')



# Part3 :Use a multiple regression model to determine the sale of a given residential property in your neighborhood
# Include:
# Sale Date
# Year built
# Building type (categorical)
# Gross Square Feet
# Number of Units

NYC_df1 <- NYC_HISTORICAL %>%
  left_join(NEIGHBORHOOD, by='NEIGHBORHOOD_ID') %>%
  left_join(BUILDING_CLASS, by = c('BUILDING_CLASS_FINAL_ROLL' = 'BUILDING_CODE_ID')) %>%
  select(NEIGHBORHOOD_NAME, SALE_DATE,DESCRIPTION,YEAR_BUILT,ADDRESS,SALE_PRICE,GROSS_SQUARE_FEET,TYPE,RESIDENTIAL_UNITS,COMMERCIAL_UNITS,BUILDING_CLASS_FINAL_ROLL) %>%
  filter(SALE_PRICE>100, GROSS_SQUARE_FEET>100) %>%
  mutate(Year = year(SALE_DATE),Quarter=quarter(SALE_DATE)) %>%
  select(BUILDING_CLASS_FINAL_ROLL,NEIGHBORHOOD_NAME,TYPE,SALE_DATE,DESCRIPTION,ADDRESS,Year,YEAR_BUILT,SALE_PRICE,RESIDENTIAL_UNITS,COMMERCIAL_UNITS,GROSS_SQUARE_FEET) 

NYC_df1$sale_data <- ymd(NYC_df1$SALE_DATE)



Bedford_Stuyvesant_reg <- NYC_df1 %>%
  filter(Year>2008, NEIGHBORHOOD_NAME =='BEDFORD STUYVESANT',TYPE=='RESIDENTIAL') %>%
  select(BUILDING_CLASS_FINAL_ROLL,ADDRESS,SALE_PRICE,SALE_DATE,YEAR_BUILT,GROSS_SQUARE_FEET,RESIDENTIAL_UNITS,COMMERCIAL_UNITS)

Bedford_Stuyvesant_model <- lm(SALE_PRICE~SALE_DATE*YEAR_BUILT*GROSS_SQUARE_FEET*RESIDENTIAL_UNITS*COMMERCIAL_UNITS, data=Bedford_Stuyvesant_reg)
summary(Bedford_Stuyvesant_model)

Bedford_Stuyvesant_reg['Residuals'] <- Bedford_Stuyvesant_model$residuals
view(Bedford_Stuyvesant_reg)
