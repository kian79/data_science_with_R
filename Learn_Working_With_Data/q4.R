library(dplyr)
load('final_table.RData')
View(final_table)
final_table = unique(final_table)
top5 <-
  final_table%>%
  filter(2000 < Season & Season < 2011)%>%
  group_by(team)%>%
  mutate(
    total_points = sum(points),
    total_goals = sum(goals)
  )%>%
  distinct(team, .keep_all = T)%>%
  arrange(total_points, total_goals)

View(top5)
top5 = tail(top5,5)
top5 = select(top5, c(team,total_goals,total_points))
View(top5)
top5_games <-
  spain%>%
  filter((home %in% top5$team | visitor %in% top5$team) & Season>2000 & Season<2011)%>%
  mutate(
    winner = case_when(visitor %in% top5$team & vgoal > hgoal ~ 'W',
                                   home %in% top5$team & vgoal < hgoal ~ 'L',
                                   TRUE ~ 'D')
  )

View(top5_games)
black_games <-
  top5_games%>%
  filter(winner == 'L')
View(black_games)
black_teams <-
  black_games%>%
  filter(home %in% top5$team)%>%
  group_by(visitor)%>%
  mutate(
    blackness = sum(tier)
  )%>%
  distinct(visitor, .keep_all = T)
View(black_teams)
black_teams1 <-
  black_games%>%
  filter(visitor %in% top5$team)%>%
  group_by(home)%>%
  mutate(
    blackness = sum(tier)
  )%>%
  distinct(home, .keep_all = T)
View(black_teams1)
black = merge(black_teams, black_teams1, by.x = 'visitor', by.y= 'home', all=T)
View(black)
black = select(black, c(visitor, blackness.x, blackness.y))
View(black)
black[is.na(black)] = 0
black_teams <-
  black%>%
  mutate(
    blackness = blackness.x + blackness.y
    )%>%
  rename('team' = 'visitor')%>%
  select(c(team,blackness))%>%
  arrange(blackness)%>%
  filter(!(team %in% top5$team))

View(black_teams) # تیم‌هایی که از ۵تیم برتر این ۱۰ سال برده‌اند و تعداد بردشان در این سال‌ها
