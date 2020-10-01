library(ggplot2)
library(dplyr)
library(lubridate)
iran_eq = read.csv('iran.csv')
iran_eq$date = as.Date(iran_eq$time)
iran_eq <-
  iran_eq%>%
  mutate(month = month(date),day = day(date))
iran_eq<-
  iran_eq%>%
  mutate(season = case_when((month(date)<3| (month(date)==3&day(date)<=15)
                            |(month(date)==12&day(date)>15) ~'winter'),
         month(date)<6 |(month==6&day<=15)~ 'spring',
         month(date)<9| (month==9&day<=15)~'summer',TRUE~'fall'))
View(iran_eq)
ggplot(iran_eq,aes(x=season))+
  geom_bar()