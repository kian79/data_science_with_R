library("lubridate")
library(engsoccerdata)
library(dplyr)

h_points <- spain%>%
  mutate(W=case_when(hgoal>vgoal~1,TRUE ~ 0),D=case_when(hgoal==vgoal~1,TRUE ~ 0),score=3*W+D)%>%
  rename(team=home)%>%
  select(Date,team,Season,score)
v_points <-spain%>%
  mutate(W=case_when(hgoal<vgoal~1,TRUE ~ 0),D=case_when(hgoal==vgoal~1,TRUE ~ 0),score=3*W+D)%>%
  rename(team=visitor)%>%
  select(Date,team,Season,score)
records<-bind_rows(v_points,h_points)%>%arrange(Date)


edited<-records%>%group_by(Season,team)%>%
  mutate(remaining_games=last(row_number())-row_number(),
         stt=cumsum(score),
         most_possible_score=stt+3*remaining_games)

fastest_fall<-modified%>%group_by(Season,remaining_games)%>%
  mutate(thirdworst =sort(stt)[3])%>%
  filter(most_possible_score<thirdworst)%>%ungroup%>%filter(remaining_games==max(remaining_games))
View(edited)
View(fastest_fall) 
