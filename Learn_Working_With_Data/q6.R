library(engsoccerdata)
library(dplyr)
library(stringr)


home_scores <- spain%>%
  mutate(info=str_c(Season," ",home),W=case_when(hgoal>vgoal ~ info, TRUE~ as.character(0)),
         L=case_when(hgoal<vgoal ~ info, TRUE ~ as.character(0)),D=case_when(hgoal==vgoal ~ info, TRUE~as.character(0)))%>%
  rename(team=home)%>%
  select(Date,Season,team,W,D,L)

away_scores <-spain%>%
  mutate(info=str_c(Season," ",visitor),
         W=case_when(hgoal<vgoal ~ info, TRUE~ as.character(0)),
         L=case_when(hgoal>vgoal ~ info, TRUE ~ as.character(0)),
         D=case_when(hgoal==vgoal ~ info, TRUE~as.character(0)))%>%
  rename(team=visitor)%>%
  select(Date,Season,team,W,D,L)



score_after_game<-bind_rows(away_scores,home_scores)%>%arrange(Season,team,Date)

win_line<-rle(score_after_game$W)
max_win<-max(win_line$lengths[win_line$value != as.character(0)]) #بیشترین تعداد برد متوالی
win_index<-which.max(win_line$lengths[win_line$value != as.character(0)])
win_info<-win_line$value[win_line$value != as.character(0)][win_index] # فصل و نام تیم


loose_line<-rle(score_after_game$L)
max_loose<-max(loose_line$lengths[loose_line$value != as.character(0)]) # بیشترین تعداد باخت متوالی
loose_index<-which.max(loose_line$lengths[loose_line$value != as.character(0)])
loose_info<-loose_line$values[loose_line$value != as.character(0)][loose_index] # فصل و نام تیم

draw_line<-rle(score_after_game$D)
max_draw<-max(draw_line$lengths[draw_line$value != as.character(0)]) #بیشترین تعداد مساوی پشت سر هم
draw_index<-which.max(draw_line$lengths[draw_line$value != as.character(0)])
draw_info<-draw_line$values[draw_line$value != as.character(0)][draw_index] # فصل و نام تیم

