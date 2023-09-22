# Load packages
library(itsmr)
library(tidyverse)
library(tsibble)
library(feasts)
library(dplyr)
library(ggplot2)
library(zoo)

#load data 
Jones = read.table("http://users.stat.umn.edu/~kb/classes/5932/data/dowj.txt", header = FALSE)


#create abreviations for months

#mutate months and year to determine better where are we
Jones = Jones %>% 
  mutate(days = seq(0,77, 1)) %>% 
  as_tsibble(index = days)


#Plot the data
Jones %>%  autoplot(V1) +
  labs(y="Stocks prizes of DowJ market",
       title="Evolution of DowJ stocks prize in 78 days")

#strong trend spotted


#Try to plot ACF function with a large lag_max
Jones %>% ACF(V1, lag_max = 70) %>% autoplot()

#Smoothing moving average plot
Jones %>%
  mutate(ma.trend3 = smooth.ma(V1, 3),
         ma.trend5 = smooth.ma(V1, 5),
         ma.trend7 = smooth.ma(V1, 7), # q=3 means averaging 2q+1 values
         ma.trend9 = smooth.ma(V1, 9)) %>% 
  pivot_longer(cols =-days,
               names_to = "series",
               values_to = "values") %>%
  group_by(series) %>%
  autoplot(values)



#Exponential Smooting
Jones %>%
  mutate(exp.trend1 = smooth.exp(V1, 0.1),# alpha=0.1 mis the smootheness parameter
         exp.trend2 = smooth.exp(V1, 0.5),
         exp.trend3 = smooth.exp(V1, 0.7),
         exp.trend4 = smooth.exp(V1, 0.9)) %>% 
  pivot_longer(cols =-days,
               names_to = "series",
               values_to = "values") %>%
  group_by(series) %>%
  autoplot(values)

#FFT 

Jones %>%
  mutate(fft.trend1 = smooth.fft(V1, 0.1),# 0.1 is cutoff frequency - 10% of the lowest spectrum passes
         fft.trend2 = smooth.fft(V1, 0.25)) %>% 
  pivot_longer(cols =-days,
               names_to = "series",
               values_to = "values") %>%
  group_by(series) %>%
  autoplot(values)

#Regression

Jones %>%
  mutate(reg.trend1 = trend(V1, 1),# 1 means polynmial of deg 1
         reg.trend2 = trend(V1, 2),
         reg.trend3 = trend(V1, 3)) %>% 
  pivot_longer(cols =-days,
               names_to = "series",
               values_to = "values") %>%
  group_by(series) %>%
  autoplot(values)

#Let's go to seasonality with another dataset

plot.ts(deaths)

acf(deaths,lag.max=40) %>% autoplot()

estimate.season = season(deaths, d=20)
plotc(deaths,estimate.season)

r = deaths-estimate.season
test(r) #test for IID data

#If d is higher, Normal Q-Q plot seams more linear, but residuals more disarrange

