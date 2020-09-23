library(dplyr)
load('final_table.RData')
top2 <-
  final_table%>%
  group_by(Season)%>%
  arrange(desc(points))%>%
  slice(1:2)%>%
  mutate(
    difference = first(points) - last(points)  
  )
View(top2)
best_champ <-
  top2%>%
  arrange(desc(difference))%>%
  select(c(team,Season,points,difference))%>%
  head(1)
View(best_champ) # مقتدرترین قهرمان
 