library(dplyr)
library(devtools)
library(engsoccerdata)
hf_table<-
  spain%>%
  arrange(Date)%>%
  group_by(Season)%>%
  filter(row_number()<=last(row_number())/2)
View(hf_table)
half_champs <- hf_table
half_champs$winner <- case_when(half_champs$hgoal > half_champs$vgoal ~ 'H',
                               half_champs$hgoal < half_champs$vgoal ~ 'V', TRUE ~ 'D')

ht <-
  half_champs %>%
  group_by(Season, home) %>%
  filter(winner == 'H') %>%
  summarise(
    pointsh = n()*3,
    hgoals = sum(hgoal)
  )

t1 <-
  half_champs%>%
  group_by(Season, visitor)%>%
  filter(winner == 'V')%>%
  summarise(
    pointsv = n()*3,
    vgoals = sum(vgoal)
  )
t2 <-
  half_champs%>%
  group_by(Season, visitor)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdv = n()
  )
t3 <-
  half_champs%>%
  group_by(Season, home)%>%
  #group_by(home)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdh = n()
  )
tt1 =  merge(x=ht,y=t1,by.x = c("home",'Season'),by.y  = c("visitor",'Season'),all = T)
tt = merge(x=t2,y=t3,by.y = c("home",'Season'),by.x  = c("visitor",'Season'),all = T)
tt = merge(x=tt,y=tt1,by.y = c("home",'Season'),by.x  = c("visitor",'Season'),all = T)
tt[is.na(tt)] = 0
final_half_table <- tt%>%
  mutate(
    points = pointsdh+pointsdv+pointsh+pointsv,
    goals = vgoals+hgoals
  )%>%
  arrange(Season,points) -> stat
final_half_table<- final_half_table%>%
  rename('team'='visitor')%>%
  select(Season,team,goals,points)
View(final_half_table)
half_champs <-
  final_half_table%>%
  group_by(Season)%>%
  filter(points==max(points))%>%
  filter(goals==max(goals))
View(half_champs)
half_champs <- 
  half_champs%>%
  select(team,Season,points)
View(half_champs)
load('full_champs.RData')
View(full_champs)
full_champs = full_champs
fh_champs = merge(full_champs, half_champs, by='Season')
View(fh_champs)
fh1_champs <-
  fh_champs%>%
  summarise(
    all_seasons = n())
View(fh_champs)

fh1_champs
fh2_champs <-
  fh_champs%>%
  filter(team.x == team.y)%>%
  summarise(
    half_seasons = n())
fh2_champs
fh2_champs / fh1_champs
fh_rate <- fh2_champs / fh1_champs *100
View(fh_rate) # درصد تیم‌هایی که در نیم فصل و پایان فصل قهرمان شده‌اند
