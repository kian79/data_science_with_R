library(ggplot2)
library(dplyr)
library(lubridate)
iran_eq = read.csv('iran.csv')
View(iran_eq)
sangin <-
  which(iran_eq$mag>6.5)
rows = c()
for (i in 1:length(sangin)){
  print('kian')
  a=0
  eq_day = day(iran_eq[sangin[i],]$time)
  while(eq_day == day(iran_eq[sangin[i]-a,]$time) & a<5){
    a = a+1
  }
  print(a)
  print(sangin[i]-a)
  print(sangin[i])
  rows = append(rows,(sangin[i]-a):sangin[i])
}
View(rows)
sangin_eq <- 
  iran_eq[rows,]%>%
  select(time,mag)%>%
  mutate(index = 1:n(),month_Yr = format(as.Date(time), "%Y-%m-%d"), type = case_when(mag>6.5~'main eq', TRUE~'pre_eq'))
ggplot(sangin_eq,aes(x=-index,y=mag, fill=type))+
  geom_bar(stat='identity')+coord_flip()+
  geom_label(data = subset(sangin_eq,type=='main eq'), aes(label=month_Yr)
             ,position = position_dodge(width=0.9),  size=3)
