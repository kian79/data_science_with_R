library(dplyr)
library(devtools)
library(engsoccerdata)
data(package="engsoccerdata")
is.tbl(spain)
spain = as.tbl(spain)
spain_temp <- spain
spain_temp$winner <- case_when(spain_temp$hgoal > spain_temp$vgoal ~ 'H',
                               spain_temp$hgoal < spain_temp$vgoal ~ 'V', TRUE ~ 'D')

t <-
  spain_temp %>%
  group_by(Season, home) %>%
  mutate(
    hgoals = sum(hgoal),
    h_vgoals = sum(vgoal)
  )%>%
  filter(winner == 'H') %>%
  summarise(
    pointsh = n()*3,
    hgoals = hgoals,
    h_vgoals = h_vgoals
  )
View(t)
t1 <-
  spain_temp%>%
  group_by(Season, visitor)%>%
  mutate(
    vgoals = sum(vgoal),
    v_hgoals = sum(hgoal)
  )%>%
  filter(winner == 'V')%>%
  summarise(
    pointsv = n()*3,
    vgoals = vgoals,
    v_hgoals = v_hgoals
  )
t2 <-
  spain_temp%>%
  group_by(Season, visitor)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdv = n()
  )
t3 <-
  spain_temp%>%
  group_by(Season, home)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdh = n()
  )
View(t)
tt1 =  merge(x=t,y=t1,by.x = c("home",'Season'),by.y  = c("visitor",'Season'),all = T)
tt = merge(x=t2,y=t3,by.y = c("home",'Season'),by.x  = c("visitor",'Season'),all = T)
tt = merge(x=tt,y=tt1,by.y = c("home",'Season'),by.x  = c("visitor",'Season'),all = T)
tt[is.na(tt)] = 0
View(tt)
tt=unique(tt)
final_table <- 
  tt%>%
  mutate(
    points = case_when(Season >= 1995 ~  pointsdh+pointsdv+pointsh+pointsv, TRUE~ pointsdh+pointsdv+((pointsh+pointsv)*2/3)),
    goals = hgoals + vgoals - h_vgoals - v_hgoals
  )
View(final_table)
final_table<- final_table%>%
  rename('team'='visitor')
View(final_table)
save(final_table,file = 'final_table.RData')
final_table<-
  final_table%>%
  select(Season,team,goals,points)
champs <-
  final_table%>%
  group_by(Season)%>%
  filter(points==max(points))%>%
  filter(goals == max(goals))%>%
  distinct(team, .keep_all = T)

View(champs)
full_champs <-
  champs%>%
  select(team,Season,points)
save(full_champs, file='full_champs.RData')
View(full_champs)
final_champs <- full_champs%>%
  group_by(team)%>%
  count()%>%
  arrange(-n)
View(final_champs)
barplot(final_champs$n,names.arg = final_champs$team, cex.names=0.4) #جدول قهرمانان لیگ اسپانیا (به دلیل در نظر گرفتن تفاضل گل به جای بازی رودررو با واقعیت تطابق کامل)
