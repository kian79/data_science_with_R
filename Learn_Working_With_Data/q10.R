library(tidyr)
library(dplyr)
library(engsoccerdata)
View(spain)
spain_weeks <-
  spain%>%
  mutate(
  day_of_the_week = as.POSIXlt(Date)$wday
  )%>%
  separate(Date,c('year','month','day'), sep='-')%>%
  filter(day_of_the_week==5 & day == 13)
View(spain_weeks) # بازی هایی که در جمعه ی سیزدهم ماه انجام شده
unlucky_homes_rate <-
  spain_weeks%>%
  mutate(
    num = n()
  )%>%
  filter((hgoal - vgoal) < 0)%>%
  summarise(
    Misfortune_rate=  n()/num *100
  )%>%
  unique()
View(unlucky_homes_rate)  # درصد تیم‌هایی که میزبان بوده اند و باخته اند ( یک از چهار که می شود بیست و پنج درصد)
