library(dplyr)
library(devtools)
library(engsoccerdata)
# تعریف از کسل کننده بودن و جذاب بودن میانگین گل زده در هر بازی فصل است که بر این اساس تیم‌ها را دسته بندی میکنیم.
is.tbl(spain)
by_season <-
  spain%>%
  group_by(Season)%>%
  summarise(
    goals = (sum(hgoal)+sum(vgoal))/n() 
  )%>%
  arrange(goals)
View(by_season)
best_season = by_season%>%
  filter(goals == max(goals))
View(best_season) # هیجان انگیزترین تیم
worst_seasons = head(by_season,10) 
barplot(worst_seasons$goals,names.arg = worst_seasons$Season, cex.names=0.5) # نمودار ۱۰ فصلی که کمترین تعداد میانگین گل زده در هر بازی را داشتند
worst_teams <-
  spain%>%
  group_by(home)%>%
  mutate(
    goals=sum(hgoal)/n()
  )
View(worst_teams)
worst_teams = distinct(worst_teams, home, .keep_all = TRUE)
worst1 <-
  spain%>%
  group_by(visitor)%>%
  mutate(
    goals=sum(vgoal)/n()
  )
worst1 =  distinct(worst1, visitor, .keep_all = TRUE)
View(worst1)
worst_teams = worst_teams%>%
  select(home, goals)
View(worst_teams)
worst1 = worst1%>%
  select(visitor, goals)
View(worst1)
worst = merge(x=worst_teams,y=worst1,by.x = c("home"),by.y  = c("visitor"),all = T)
View(worst)
worst <-
  worst%>%
  rename('team'='home')%>%
  mutate(goals_per_game= goals.x+goals.y)%>%
  arrange(goals_per_game)
View(worst)
best_team <-
  worst%>%
  tail(1)
View(best_team) #بیشترین میانگین گل زده در هر بازی یعنی هیجان‌انگیزترین تیم
top10_worst_teams <- # ده تیم کسل کننده
  worst%>%
  head(10)
View(top10_worst_teams)
barplot(top10_worst_teams$goals_per_game,names.arg = top10_worst_teams$team, cex.names=0.5) # نمودار ۱۰ تیم کمترین تعداد میانگین گل زده در هر بازی را داشتند
