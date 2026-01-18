# Step -1 Load Libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)


#Load Clean Data
df <- read.csv("cleaned_rainfall_data.csv")
head(df)

# Convert Date Column
df$DATE <- as.Date(df$DATE)
View(df)
names(df)
str(df)
unique(df$MONTH)

#Mean ,Median ,SD ,VARiable 
mean(df$RAINFALL)
median(df$RAINFALL)
sd(df$RAINFALL)
var(df$RAINFALL)

#This data is highly skewed :- Positive skewness-> extreme rainfall events
library(psych)
skew(df$RAINFALL) 

#Monthly Statistical Summary
monthly_stats <- df %>%
  group_by(MONTH) %>%
  summarise(
    mean_rain = mean(RAINFALL),
    sd_rain = sd(RAINFALL),
    max_rain = max(RAINFALL),
    min_rain = min(RAINFALL)
  )
monthly_stats


#Coeficient of Variation 
#Higher coefficient of variation indicates unstable rainfall during certain months.
monthly_stats <-monthly_stats %>%
  mutate(cv=(sd_rain/mean_rain)*100)
monthly_stats

#Threshold
# Extreme rainfall events are increasing in recent decades
threshold <- quantile(df$RAINFALL, 0.95)
extreme_events <- df %>%
  filter(RAINFALL > threshold)
nrow(extreme_events)


#All india Average Rainfall Trend
india_trend <- df %>% 
  group_by(YEAR) %>% 
  summarise(avg_rainfall=mean(RAINFALL))

ggplot(india_trend ,aes(YEAR ,avg_rainfall))+
  geom_line(color="pink")+geom_point("black")
  labs(title = "All-India Average Rainfall Trend",
       x = "Year",
       y = "Rainfall (mm)")

#Seasonal Analysis
  seasonal <- df %>%
    group_by(MONTH) %>%
    summarise(avg_rainfall=mean(RAINFALL))
  
  ggplot(seasonal,aes(MONTH,avg_rainfall))+
    geom_col(fill="steelblue") +
    labs(title = "seasonal Rainfall Pattern in India")
  
# State _wise Comparison
state_trend <- df %>%
    filter(SUBDIVISION %in% c("EAST MADHYA PRADESH", "ARUNACHAL PRADESH", "KERALA"))
  

  ggplot(state_trend, aes(YEAR, RAINFALL, color=SUBDIVISION)) +
    geom_line() +geom_point()
    labs(title="State-wise Rainfall Comparison")
    
    
#Time-Series Forcasting 
    ts_data <- ts(india_trend$avg_rainfall, start=min(india_trend$YEAR), frequency=1)
    
    model <- auto.arima(ts_data)
    forecast_data <- forecast(model, h=10)
    
    plot(forecast_data)

    
